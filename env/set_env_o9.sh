#!/bin/bash
#
# orca.atmos.washington.edu uwin config file for running on the new Orca nodes (o9, o10, o11, o12).
# NOTE: The code needs to be compiled on the node o9!

# unlimit stack
ulimit -s unlimited

# local builds of HDF5 and NetCDF
export HDF5=/home/disk/orca2/mcurcic/opt/hdf5-1.8.19_intel-17.0.4
export NETCDF=/home/disk/orca2/mcurcic/opt/netcdf-4.4_intel-17.0.4

# set up Intel Fortran, C, and C++ compilers
source /opt/intel/compilers_and_libraries_2019/linux/bin/compilervars.sh intel64

# OpenMPI build
OPENMPI=/home/orca/bkerns/opt/install/rels/openmpi-4.1.1-ucx_intel-19.0.1.144

export LD_LIBRARY_PATH=$HDF5/lib:$NETCDF/lib:$OPENMPI/lib:$LD_LIBRARY_PATH
export PATH=$OPENMPI/bin:$PATH

# The following are needed after Feb. 2020 compiler upgrades on orca.
export LD_LIBRARY_PATH=/usr/lib/gcc/x86_64-linux-gnu/8:$LD_LIBRARY_PATH
export CPLUS_INCLUDE_PATH=$CPLUS_INCLUDE_PATH:/usr/include/x86_64-linux-gnu/c++/8

# WRF large NetCDF files
export WRFIO_NCD_LARGE_FILE_SUPPORT=1

# hi-res terrain and landuse
export TERRAIN_AND_LANDUSE=1

# WRF configure option
export WRF_CONFIGURE_OPTION=15 # intel dmpar

# UMWM build options
export FC=mpif90
export FCFLAGS="-O3 -w -heap-arrays -zero"
export CPPFLAGS="-DMPI -DESMF"

# ESMF configuration
export ESMF_OS=Linux
export ESMF_DIR=$(pwd)/src/esmf-ESMF_8_1_1
export ESMF_COMPILER=intel
export ESMF_ABI=64
export ESMF_BOPT=O
export ESMF_COMM=openmpi
export ESMF_PTHREADS=OFF
export ESMF_OPENMP=OFF
