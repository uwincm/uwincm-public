import numpy as np
from netCDF4 import Dataset

# +++ Depths are positive upwards, negative below the sea surface. +++

# Set global max depth (based on ETOPO) to keep as sea.
#  (something large like 9999 to skip this step.)
MAX_DEPTH = 0.0

# Specify which boxes to use, and the max depths to set to land within each box.
BOXES = []
BOXES += [[-65.5,-64.0,31.6,33.2]] # Bermuda

DEPTHS = []
DEPTHS += [-3000.0]  # Bermuda



in_file = 'umwm.gridtopo.orig'
out_file = 'umwm.gridtopo'
#out_grid_file = 'umwm.grid'


##################################################

DSin = Dataset(in_file,'r')
mask = DSin['seamask'][:]   ## Land = 0, Sea = 1
depth = DSin['z'][:]
lon = DSin['lon'][:]
lat = DSin['lat'][:]
DSin.close()


## Do "global" max depth.
mask[depth >= MAX_DEPTH] = 0


## Do boxes.
for ii in range(len(BOXES)):
    this_box = BOXES[ii]
    this_depth = DEPTHS[ii]

    where_this_box_x = np.logical_and(lon > this_box[0], lon < this_box[1])
    where_this_box_y = np.logical_and(lat > this_box[2], lat < this_box[3])
    where_this_box = np.logical_and(where_this_box_x, where_this_box_y)
    where_make_land = np.logical_and(where_this_box, depth >= this_depth)
    mask[where_make_land] = 0
    depth[where_make_land] = 10.0


DSout = Dataset(out_file,'a')
DSout['seamask'][:] = mask
DSout['z'][:] = depth

DSout.close()

#DSout = Dataset(out_grid_file,'a')
#DSout['seamask'][:] = mask
#DSout.close()




print('Done.')
