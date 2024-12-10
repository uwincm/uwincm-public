# UWIN-CM tutorial

## Table of Contents

* [Getting the code](#getting-the-code)
* [Building UWIN-CM](#building-uwin-cm)
  - [Setting up the environment](#setting-up-the-environment)
  - [Selecting model version](#selecting-model-version)
  - [Building ESMF](#building-esmf)
  - [Building WRF](#building-wrf)
  - [Building UMWM](#building-umwm)
  - [Building HYCOM](#building-hycom)
  - [Building UWIN-CM](#building-uwin-cm)
* [Setting up a UMWM domain](#setting-up-a-umwm-domain)
  - [Building UMWM preprocessing programs](#building-umwm-preprocessing-programs)
  - [Generating UMWM input files](#generating-umwm-input-files)
* [Setting up a HYCOM domain](#setting-up-a-hycom-domain)
  - [Building HYCOM preprocessing programs](#building-hycom-preprocessing-programs)
* [Running the model](#running-the-model)
  - [Run directory structure](#run-directory-structure)
* [Resources](#resources)

## Getting the code

The UWIN-CM code is maintained in the group Gitlab repository
at https://gitlab.com/uwincm/uwincm.

To get the code from command line, type:

```
git clone https://gitlab.com/uwincm/uwincm.git
```

You will be prompted for your Gitlab username and password. 
When the clone is complete, you will have the `uwincm`

## Building UWIN-CM

To build UWIN-CM, you need to build ESMF, WRF, UMWM, HYCOM, and UWIN.
WRF, UMWM, and HYCOM can be built in any order, but ESMF has to be build first,
and UWIN has to be built last. Building UWIN results in new executable `uwincm`.
Edit the `Makefile` for the desired versions for each model component:

### Setting up the environment

The top level `uwincm` directory looks like this:

```
ls uwincm/
env  LICENSE  Makefile  README.md  src
```

The `env` directory contains a shell script that is specific to a machine (manta, uwin, orca...).
To set up the environment on orca, type:

```
cd uwincm
source env/set_env_orca.sh
```

For example, this is what the environment script for orca looks like:

```sh
cat env/set_env_orca.sh 
#!/bin/bash
#
# orca.atmos.washington.edu uwin config file
#

export HDF5=/home/disk/orca2/mcurcic/opt/hdf5-1.8.19_intel-17.0.4
export NETCDF=/home/disk/orca2/mcurcic/opt/netcdf-4.4_intel-17.0.4

source /opt/intel/compilers_and_libraries_2017.4.196/linux/bin/compilervars.sh intel64

OPENMPI=/usr/rels/openmpi-3.0.0

LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HDF5/lib:$NETCDF/lib:$OPENMPI/lib
export LD_LIBRARY_PATH

PATH=$OPENMPI/bin:$PATH
export PATH

# WRF large NetCDF files
export WRFIO_NCD_LARGE_FILE_SUPPORT=1

# ESMF configuration
export ESMF_OS=Linux
export ESMF_DIR=/home/disk/orca2/mcurcic/uwincm/src/esmf-7.0.1_intel17
export ESMF_COMPILER=intel
export ESMF_ABI=64
export ESMF_BOPT=O
export ESMF_COMM=openmpi
export ESMF_PTHREADS=OFF
export ESMF_OPENMP=OFF
```

**Warning**: It is important that you source `env/set_env_orca.sh` from `uwincm/`
and not from any other directory!

### Selecting model version

The version for each model component (WRF, UMWM, HYCOM) is selected in the top-level `Makefile`.
Edit this file for the model versions that you need.
Example:

```
# set desired model component version
wrf_version   = 3.9
umwm_version  = 2.0.0
hycom_version = 2.2.34
```

The following versions are available:

* WRF:
  - 3.6.1
  - 3.7.1
  - 3.8.1
  - 3.9
* UMWM:
  - 2.0.0
* HYCOM:
  - 2.2.34 (recommended)
  - 2.2.98


### Building ESMF

From the top-level directory, type:

```
make esmf
```

### Building WRF

Enter the desired WRF source directory, for example `src/components/WRFV3.9`, and type:

```
./configure
```

When prompted, select the following options:

* Intel dmpar (option 15)
* Moving nests (option 3)

After the configure is done, there will be a file called `configure.wrf`. 

From the top-level directory (`uwincm`), type:

```
make wrf
```

### Building UMWM

From the top-level directory, type:

```
make umwm
```

### Building HYCOM

If working with version 2.2.34, you have to edit `src/components/hycom/src_2.2.34/dimensions.h`
and set the appropriate domain decomposition parameters to match your `patch.input` (see section on setting up HYCOM below).

From the top-level directory, type:

```
make hycom
```

### Building UWIN-CM

From the top-level directory, type:

```
make uwincm
```

If the model builds succesfully, the executable `uwincm` will appear in the current directory.

## Setting up a UMWM domain

To set up a UMWM domain, we will use two programs:

* `wrf2umwmgrid` - create a UMWM grid based on a WRF file.
* `umwm_topogen` - creates a UMWM bathymetry file based on input UMWM grid

### Building UMWM preprocessing programs

To build UMWM preprocessing tools, type from the root `uwincm` directory:

```
cd src/components/umwm-2.0.0
make tools
```

### Generating UMWM input files

#### Generating the grid

We will first generate the grid based on an input WRF file using `wrf2umwmgrid`.
This program can ready any of the following WRF files:

* `wrfinput*`
* `wrfout*`
* `wrfrst*`

Go into the `tools` directory:

```
cd tools
ls
```

To generate the grid, simply pass a WRF file whose grid you want to use to `wrf2umwmgrid`:

```
./wrf2umwmgrid /home/orca/mcurcic/tutorials/sample_wrfout_file/wrfout_d02_2017-04-04_00\:00\:00
```

This will create a file `umwm.grid`. Use ncview to look inside!

`umwm.grid` is a required input to generate the batrhymetry file -- see next step.

#### Generating the bathymetry

To generate the bathymetry, we will use the `umwm_topogen` program.
This program reads the namelist file in `namelists/topogen.nml`:

```
&topogen
  umwmInputFile = 'umwm.grid'
  topoInputFile = '/home/orca/mcurcic/data/etopo01/ETOPO1_Ice_c_gmt4.grd'
  useSeamask    = .T.
/
```

`topoInputFile` must point to a base topography input data.
`useSeamask` controls whether we will use the mask from `umwm.grid`
to determine which cells are land or sea.
Otherwise, the seamask is determined based on the sign of elevation from topography
data -- positive is land, and negative is water. Obviously, this doesn't work for 
elevated lakes!

Once `namelists/topogen.nml` is set, run `umwm_topogen`:

```
./umwm_topogen
```

This will create the combined grid and topography file, `umwm.gridtopo`.

## Setting up a HYCOM domain

The best place to start is our own hycom-domain package. Get it from the group Gitlab:

```
git clone https://gitlab.com/uwincm/hycom-domain.git
```

hycom-domain contains the scripts to create a HYCOM grid, bathymetry, climatology,
relaxation weights, and forcing files.

There are two ways to run it:

* **Scripted**: By editing the master configuration script and running the scripts in sequence
* **Manually**: By following the step-by-step directions in the README.md file, and editting input scripts by hand for your domain. [Ajda's slides](https://gitlab.com/uwincm/hycom-domain/blob/master/docs/setting-up-hycom.pptx) are a great resource for the manual steps.

In either case, the detailed instructions are in the hycom-domain README.md file.

hycom-domain depends on a number of HYCOM preprocessing programs.
These can be build easily using our own hycom-processor package (next Section).

### Building HYCOM preprocessing programs

We have streamlined the building process of the HYCOM preprocessing programs.
These are located in our hycom-processor package. Get it from the group Gitlab:

```
git clone https://gitlab.com/uwincm/hycom-processor.git
```

Building the tools is straightforward:

```
cd hycom-processor
mkdir build
cd build
FC=ifort cmake ..
make
```

This will build all the required programs in `hycom-processor/build/bin`.
To make them available from the command line globally, add this directory
to your `PATH` environment variable.

## Running the model

To run the model, we need a compiled UWIN-CM executable,
and a dedicated run directory that contains all the required input
files for each of the model components.

### Run directory structure

Required WRF files are:

* Input namelist: `namelist.input`
* Run-specific input data files:
  - `wrfinput_d01`
  - `wrfbdy_d01`
  - `wrfrst_d0*` (if running from restart)
* Static files:
  - `GENPARM.TBL`
  - `LANDUSE.TBL`
  - `MPTABLE.TBL`
  - `SOILPARM.TBL`
  - `VEGPARM.TBL`
  - `RRTM_DATA` (if using RRTMG, you will need `RRTMG_DATA`)
  - `RRTM_DATA_DBL` (if using RRTMG, you will need `RRTMG_DATA_DBL`)

Required UMWM files are:

* Input namelists: 
  - `namelists/main.nml`
  - `namelists/exclude.nml`
  - `namelists/spectrum.nml`
* Run-specific input data file:
  - `input/umwm.gridtopo` - grid and bathymetry file
* Domain-specific input data files:
  - `restart/umwmrst_*.nc` (only if running from restart)
  - `input/umwmin_*.nc` (only if running in uncoupled and forced mode)

UMWM also requires that `output/` and `restart/` directories are present because that is where
the model will write `umwmout*.nc` and `umwmrst*.nc` files. 

Required HYCOM files are:

* Input namelist: `blkdat.input`
* Run-specific input data files:
  - `restart_in.[ab]` - initial conditions / restart files
  - `nest/archv.*.[ab]` - boundary condition files
* Domain-specific input data files:
  - `regional.grid.[ab]` - grid files
  - `regional.depth.[ab]` - bathymetry files
  - `nest/rmu.[ab]` - boundary relaxation weights
  - `patch.input` - domain decomposition file
  - `forcing.kpar.[ab]` - ocean turbidity data
  - `forcing.rivers.[ab]` - optional, if you have rivers input data
  - `ports.input` - a dummy file that has to be there but we don't use it.

Required UWIN files are:
* Input namelist: `uwin.nml`

These are all the files that you need to make a coupled model run.

You can get a sample run directory for Hurricane Harvey on orca from
`/home/orca/mcurcic/harvey/run_harvey_tutorial.tar.gz`.
Copy the tarball to a local directory and unpack it:

```
tar xzvf run_harvey_tutorial.tar.gz
cd run_harvey_tutorial
```

This sample run is set up to run on 24 cores. To run it, type:

```
mpiexec -n 24 ./uwincm
```

## Resources

* [UWIN-CM Gitbook](https://www.gitbook.com/book/milancurcic/uwincm-manual) - a bit outdated but thorough.
* [Ajda's slides](https://gitlab.com/uwincm/hycom-domain/blob/master/docs/setting-up-hycom.pptx) on generating a HYCOM domain
* [UMWM documentation](https://gitlab.com/uwincm/umwm/tree/master/docs)
* [WRF Preprocessing documentation](http://www2.mmm.ucar.edu/wrf/users/docs/user_guide_V3/users_guide_chap3.htm)
