# This example demonstrates how to regrid between a Grid and a LocStream.
# The data files can be retrieved from the ESMF data repository by uncommenting the
# following block of code:
#
# import os
# DD = os.path.join(os.getcwd(), "examples/data")
# if not os.path.isdir(DD):
#     os.makedirs(DD)
# from ESMF.util.cache_data import cache_data_file
# cache_data_file(os.path.join(DD, "ll1deg_grid.nc"))

try:
    from unittest import SkipTest
except ImportError:
    from nose import SkipTest

import ESMF
import numpy

import ESMF.util.helpers as helpers
import ESMF.api.constants as constants

# This call enables debug logging
ESMF.Manager(debug=True)

from ESMF.util.locstream_utilities import create_locstream_spherical_16, create_locstream_spherical_16_parallel
coord_sys=ESMF.CoordSys.SPH_DEG
domask=True
if ESMF.pet_count() == 1:
    locstream = create_locstream_spherical_16(coord_sys=coord_sys, domask=domask)
else:
    if constants._ESMF_MPIRUN_NP != 4:
        raise SkipTest('processor count must be 4 or 1 for this example')
    else:
        locstream = create_locstream_spherical_16_parallel(coord_sys=coord_sys, domask=domask)

grid1 = "examples/data/ll1deg_grid.nc"
grid = ESMF.Grid(filename=grid1, filetype=ESMF.FileFormat.SCRIP)

# create a field
srcfield = ESMF.Field(grid, name='srcfield')

dstfield = ESMF.Field(locstream, name='dstfield')
xctfield = ESMF.Field(locstream, name='xctfield')

# initialize the fields
[x, y] = [0, 1]
deg2rad = 3.14159/180

gridXCoord = srcfield.grid.get_coords(x)
gridYCoord = srcfield.grid.get_coords(y)
srcfield.data[...] = 10.0 + numpy.cos(gridXCoord * deg2rad) ** 2 + numpy.cos(2 * gridYCoord * deg2rad)

gridXCoord = locstream["ESMF:Lon"]
gridYCoord = locstream["ESMF:Lat"]
if coord_sys == ESMF.CoordSys.SPH_DEG:
    xctfield.data[...] = 10.0 + numpy.cos(gridXCoord * deg2rad) ** 2 + numpy.cos(2 * gridYCoord * deg2rad)
elif coord_sys == ESMF.CoordSys.SPH_RAD:
    xctfield.data[...] = 10.0 + numpy.cos(gridXCoord) ** 2 + numpy.cos(2 * gridYCoord)
else:
    raise ValueError("coordsys value does not work in this example")

dstfield.data[...] = 1e20

# create an object to regrid data from the source to the destination field
dst_mask_values=None
if domask:
    dst_mask_values=numpy.array([0])

regrid = ESMF.Regrid(srcfield, dstfield,
                     regrid_method=ESMF.RegridMethod.BILINEAR,
                     unmapped_action=ESMF.UnmappedAction.ERROR,
                     dst_mask_values=dst_mask_values)

# do the regridding from source to destination field
dstfield = regrid(srcfield, dstfield, zero_region=ESMF.Region.SELECT)

# compute the mean relative error
num_nodes = numpy.prod(xctfield.data.shape[:])
relerr = 0
meanrelerr = 0

dstfield = numpy.ravel(dstfield.data)
xctfield = numpy.ravel(xctfield.data)

if num_nodes != 0:
    ind = numpy.where((dstfield != 1e20) & (xctfield != 0))[0]
    relerr = numpy.sum(numpy.abs(dstfield[ind] - xctfield[ind]) / numpy.abs(xctfield[ind]))
    meanrelerr = relerr / num_nodes

# handle the parallel case
if ESMF.pet_count() > 1:
    relerr = helpers.reduce_val(relerr, op=constants.Reduce.SUM)
    num_nodes = helpers.reduce_val(num_nodes, op=constants.Reduce.SUM)

# output the results from one processor only
if ESMF.local_pet() == 0:
    meanrelerr = relerr / num_nodes
    print ("ESMPy Grid LocStream Regridding Example")
    print ("  interpolation mean relative error = {0}".format(meanrelerr))

    assert (meanrelerr < 2e-2)
