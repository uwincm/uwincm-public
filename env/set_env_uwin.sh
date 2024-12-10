#!/bin/bash
#
# RSMAS uwin config file

PATH=/usr/local/intel/composer_xe_2013.3.163/bin/intel64:$PATH
PATH=/usr/local/intel/openmpi-1.7.5/bin:$PATH
LD_LIBRARY_PATH=/usr/local/intel/openmpi-1.7.5/lib
LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/intel/composer_xe_2013.3.163/compiler/lib/intel64/

module load intel/13.1.1 hdf5/1.8.14 netcdf/4.3.3 openmpi/1.7.5
export HDF5=/usr/local/modules/hdf5/1.8.14/intel/13.1.1
LD_LIBRARY_PATH=$HDF5/lib:$NETCDF/lib:$LD_LIBRARY_PATH

export PATH
export LD_LIBRARY_PATH

# WRF large NetCDF files
export WRFIO_NCD_LARGE_FILE_SUPPORT=1

# hi-res terrain and landuse
export TERRAIN_AND_LANDUSE=1

# WRF configure option
export WRF_CONFIGURE_OPTION=15 # intel dmpar

# ESMF configuration
export ESMF_DIR=$(pwd)/src/esmf
export ESMF_COMPILER=intel
export ESMF_ABI=64
export ESMF_BOPT=O
export ESMF_COMM=openmpi
export ESMF_PTHREADS=OFF
export ESMF_OPENMP=OFF
