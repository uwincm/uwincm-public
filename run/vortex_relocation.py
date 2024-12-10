## This requires a Python distribution with numpy, scipy, and NetCDF4.
## It also uses standard Python modules: sys and shutil

import numpy as np
from netCDF4 import Dataset
from scipy.interpolate import NearestNDInterpolator
import datetime as dt
import shutil
import sys


def get_field(cg_file_name, field_name):
    DS = Dataset(cg_file_name, 'r')
    data = DS[field_name][:]
    DS.close()
    return data


def calculate_radius_weight(r1, r2, radius, method='cosine'):
    weight = None
    if radius < r1:
        weight = 1.0
    elif radius > r2:
        weight = 0.0
    else:
        if method == 'quadratic':
            weight = 1.0 - (radius - r1)**2 / (r2 - r1)**2
        elif method == 'cosine':
            weight = 0.5 + 0.5 * np.cos(np.pi * (radius - r1) / (r2 - r1))
        else:
            # Revert to linear weighting.
            weight = 1.0 - (radius - r1) / (r2 - r1)

    return weight


def get_storm_relative_distance(x, y, x_center, y_center):
    ## Note: if x and y are provided as lat/lon degrees, this will be in degrees.
    return np.sqrt(np.power(x - x_center, 2) + np.power(y - y_center, 2))


def vortex_relocation(wrfinput_file_name='wrfinput_d01', cg_file_name='cg.nc', vortex_insert_center_lon=None, vortex_insert_center_lat=None, inner_blending_radius=100.0, outer_blending_radius=400.0, add_const_u = 0.0, add_const_v = 0.0, output_file_name='wrfinput_d01.relo'):

    """
    The main vortex relocation function.
    1) Read in data.
    2) Get weights based on inner and outer blending radii.
    3) Interpolate CG data to the wrfinput grid (nearest neighbor)
    4) Apply the weights to each field.
    5) Write the new wrfinput file.

    Specifying the outer radius larger than the size of the cg file not recommended!
    """
    
    print(wrfinput_file_name)
    print('  + ' + cg_file_name)
    print('--> ' + output_file_name)
    print('', flush=True)

    
    ##
    ## This dictionary maps wrfinput variables with cg file variables.
    ##
    parameter_list = {}
    ######       wrfinput    CG
    parameter_list["MU"] = "mu"
    parameter_list["PSFC"] = "slp"
    parameter_list["U10"] = "un10"
    parameter_list["V10"] = "vn10"
    parameter_list["T2"] = "t2"
    parameter_list["TH2"] = "th2"    
    parameter_list["Q2"] = "q2"    
    parameter_list["QVAPOR"] = "qv"
    parameter_list["T"] = "th"
    parameter_list["P"] = "perturbation_p"
    parameter_list["PH"] = "perturbation_ph"
    parameter_list["U"] = "un"
    parameter_list["V"] = "vn"

    ##
    ## Longitude and latitude
    ##

    ## From wrfinput
    ## U and V use different latitude and longitude.
    lon_wrf = get_field(wrfinput_file_name, 'XLONG')[0]
    lon_wrf_u = get_field(wrfinput_file_name, 'XLONG_U')[0]
    lon_wrf_v = get_field(wrfinput_file_name, 'XLONG_V')[0]

    lat_wrf = get_field(wrfinput_file_name, 'XLAT')[0]
    lat_wrf_u = get_field(wrfinput_file_name, 'XLAT_U')[0]
    lat_wrf_v = get_field(wrfinput_file_name, 'XLAT_V')[0]

    ## from CG file
    lon_cg = get_field(cg_file_name, 'xlong')
    x_cg = get_field(cg_file_name, 'cg_x')
    y_cg = get_field(cg_file_name, 'cg_y')
    nz,ny,nx = lon_cg.shape
    lon_cg_relative = x_cg / 111.0
    lat_cg_relative = y_cg / 111.0

    ## For interpolator, points array must be of shape (npoints, ndims).
    ##  This uses the CG file points.
    xpoints = np.reshape(lon_cg_relative, (1, nx*ny))
    ypoints = np.reshape(lat_cg_relative, (1, nx*ny))
    points = np.array([xpoints[0], ypoints[0]]).T

    
    ##
    ## Storm relative distance and weighting for wrfinput.
    ##
    relative_distance = 111.0 * get_storm_relative_distance(lon_wrf, lat_wrf, vortex_insert_center_lon, vortex_insert_center_lat)
    relative_distance_u = 111.0 * get_storm_relative_distance(lon_wrf_u, lat_wrf_u, vortex_insert_center_lon, vortex_insert_center_lat)
    relative_distance_v = 111.0 * get_storm_relative_distance(lon_wrf_v, lat_wrf_v, vortex_insert_center_lon, vortex_insert_center_lat)

    NJ, NI = relative_distance.shape
    NJ_u, NI_u = relative_distance_u.shape
    NJ_v, NI_v = relative_distance_v.shape

    weights = np.array([[calculate_radius_weight(inner_blending_radius, outer_blending_radius, relative_distance[jj,ii]) for ii in range(NI)] for jj in range(NJ)])
    weights_u = np.array([[calculate_radius_weight(inner_blending_radius, outer_blending_radius, relative_distance_u[jj,ii]) for ii in range(NI_u)] for jj in range(NJ_u)])
    weights_v = np.array([[calculate_radius_weight(inner_blending_radius, outer_blending_radius, relative_distance_v[jj,ii]) for ii in range(NI_v)] for jj in range(NJ_v)])

    
    ##
    ## Fill in the "new" data dictionary one field at a time.
    ##
    data_new = {}
    
    for this_parameter in parameter_list:

        print('Doing ' + this_parameter + '.', flush=True)
        data_wrfout = get_field(wrfinput_file_name, this_parameter)
        data_cg = get_field(cg_file_name, parameter_list[this_parameter])
        if this_parameter == "T":
            data_cg -= 300.0  # For potential temperature, need to get back to perturbation theta.
        if this_parameter == "U":
            data_cg += add_const_u
        if this_parameter == "U10":
            data_cg += add_const_u
        if this_parameter == "V":
            data_cg += add_const_v
        if this_parameter == "V10":
            data_cg += add_const_v

        data_new[this_parameter] = 0.0*data_wrfout

        if len(data_wrfout.shape) == 3: # 2-D data (plus time)

            interpolator = NearestNDInterpolator(points, np.reshape(data_cg[:,:], (1, nx*ny))[0])

            data_cg_interp = interpolator(lon_wrf - vortex_insert_center_lon, lat_wrf - vortex_insert_center_lat)
            data_cg_interp2 = (weights * data_cg_interp) + ((1.0 - weights) * data_wrfout[0,:,:])

            data_new[this_parameter][0,:,:] = data_cg_interp2

        else:  # 3-D data (plus time)

            if this_parameter == "PH":
                nz_this_field = nz + 1  # Staggered field has extra vertical level.
            else:
                nz_this_field = nz
            
            for kk in range(nz_this_field):
                interpolator = NearestNDInterpolator(points, np.reshape(data_cg[kk,:,:], (1, nx*ny))[0])

                if this_parameter == "U":
                    data_cg_interp = interpolator(lon_wrf_u - vortex_insert_center_lon, lat_wrf_u - vortex_insert_center_lat)
                    data_cg_interp2 = (weights_u * data_cg_interp) + ((1.0 - weights_u) * data_wrfout[0,kk,:,:])

                elif this_parameter == "V":
                    data_cg_interp = interpolator(lon_wrf_v - vortex_insert_center_lon, lat_wrf_v - vortex_insert_center_lat)
                    data_cg_interp2 = (weights_v * data_cg_interp) + ((1.0 - weights_v) * data_wrfout[0,kk,:,:])

                else:
                    data_cg_interp = interpolator(lon_wrf - vortex_insert_center_lon, lat_wrf - vortex_insert_center_lat)
                    data_cg_interp2 = (weights * data_cg_interp) + ((1.0 - weights) * data_wrfout[0,kk,:,:])

                data_new[this_parameter][0,kk,:,:] = data_cg_interp2

    ##
    ## Output
    ##

    print('Saving new wrfinput file.')
    shutil.copy(wrfinput_file_name, output_file_name)
    with Dataset(output_file_name, 'a') as DS2:
        for this_parameter in parameter_list:
            DS2[this_parameter][:] = data_new[this_parameter]
        ## Add Attributes.
        if 'vortex_relocation_history' in DS2.ncattrs():
            DS2.vortex_relocation_history += "; " + str(dt.datetime.now()) + ": python " + " ".join(sys.argv)
        else:
            DS2.vortex_relocation_history = str(dt.datetime.now()) + ": python " + " ".join(sys.argv)
    
if __name__== "__main__":

    import sys

    if len(sys.argv) < 7:
        print('usage ex: python vortex_relocation.py wrfinput_d01 cg_2017-06-22_00:00:00 -90 25 100 400', flush=True)
        print('or with adding constant u=1, v=2 m/s: python vortex_relocation.py wrfinput_d01 cg_2017-06-22_00:00:00 -90 25 100 400 1 2', flush=True)
    else:
        
        ##
        ## Handle command line input
        ##
        wrfinput_file_name = sys.argv[1]
        cg_file_name = sys.argv[2]
        output_file_name = (wrfinput_file_name + ".relo")
        vortex_insert_center_lon = np.double(sys.argv[3])
        vortex_insert_center_lat = np.double(sys.argv[4])
        inner_blending_radius = np.double(sys.argv[5])
        outer_blending_radius = np.double(sys.argv[6])

        if len(sys.argv) >= 8:
            add_const_u = np.double(sys.argv[7])
        else:
            add_const_u = np.double(0.0)

        if len(sys.argv) >= 9:
            add_const_v = np.double(sys.argv[8])
        else:
            add_const_v = np.double(0.0)
        
            
        ##
        ## Call main program.
        ##
        vortex_relocation(wrfinput_file_name, cg_file_name, vortex_insert_center_lon
                          , vortex_insert_center_lat, inner_blending_radius
                          , outer_blending_radius, add_const_u, add_const_v
                          , output_file_name=output_file_name)

        print('Done.', flush=True)
