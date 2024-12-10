import numpy as np
from netCDF4 import Dataset
import sys



if len(sys.argv) < 2:
    print('Usage: python geo_em2umwmgrid.py geo_em.d01.nc')
else:
    fn = sys.argv[1]
    print(fn)

    ## Read lon, lat, landmask
    DS = Dataset(fn)
    lon = DS['XLONG_M'][:][0,:,:]
    lat = DS['XLAT_M'][:][0,:,:]
    lm = DS['LANDMASK'][:][0,:,:]
    DS.close()

    S = lon.shape

    ## Write umwm.grid file.
    fn_out = 'umwm.grid'
    print('--> '+fn_out)
    DS = Dataset(fn_out, 'w')
    DS.createDimension('x',S[1])
    DS.createDimension('y',S[0])
    DS.createVariable('lon','d',('y','x',))
    DS.createVariable('lat','d',('y','x',))
    DS.createVariable('seamask','d',('y','x',))
    DS['lon'][:] = lon
    DS['lat'][:] = lat
    DS['seamask'][:] = 1 - lm
    DS.close()
    print('Done.')
