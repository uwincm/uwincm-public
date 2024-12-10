#!/bin/bash
#
# CCS Pegasus config file

module load intel openmpi
module load netcdf/4.2.1.1

export NETCDF=/share/apps/netcdf/4.2.1.1/intel/13.0.2.146

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

