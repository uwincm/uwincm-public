# Unified Wave INterface - Coupled Model (UWIN-CM)

This is the source code repository for the Unified Wave INterface - Coupled Model (UWIN-CM)
developed by Prof. Shuyi S. Chen and Milan Curcic and maintained by Brandon Kerns since 2017.

Several others have contributed improvements to the model code, including Dalton Kai Sasaki, Benjamin Barr, and Ajda Savarin.

**This code is being transferred to the UWIN-CM group on GitHub.**


## Getting started

### Getting the source code

The *master* branch is the most stable and tested.
The *dev* branch has more updates and features, but may have bugs.

In order to ensure that you're getting the full content of any submodules, use `-b dev` when cloning the dev branch.

#### -- Checking out the master branch --
If you're using an SSH token to authenticate with GitLab, 
clone the repo like this:

```
git clone --recursive git@gitlab.com:uwincm/uwincm.git
```

Otherwise, if you're using a username/password method to authenticate,
clone the repo like this:

```
git clone --recursive https://gitlab.com/uwincm/uwincm.git
```

#### -- Checking out the dev branch --
If you're using an SSH token to authenticate with GitLab, 
clone the repo like this:

```
git clone --recursive -b dev git@gitlab.com:uwincm/uwincm.git
```

Otherwise, if you're using a username/password method to authenticate,
clone the repo like this:

```
git clone --recursive -b dev https://gitlab.com/uwincm/uwincm.git
```


### Setting up the environment

The `env` directory contains scripts for setting up the environment
on a specific compute system. If the script for the desired system
is not available, you can create one and contribute it to the repo,
or request it by [opening an issue](https://gitlab.com/uwincm/uwincm/issues/new).

Set up your environment by typing:
```
source env/set_env_<platform>.sh
```
* For running on the orca nodes o1 - o8, <platform> is "orca". Compile on orca server.
* For running on the orca nodes o9 - o11, <platform> is "o9". Compile on o9.

The environment scripts must be sourced from the top-level directory.
Currently only bash shell is supported.


### Building the model
First copy Makefile.MASTER to Makefile. Select the versions of WRF, HYCOM and UMWM that you will be using.


To build UWIN-CM, you need to build ESMF, WRF, UMWM, HYCOM, and UWIN. 
WRF, UMWM, and HYCOM can be built in any order, but ESMF has to be built first,
and UWIN has to be built last. I usually do ESMF first, then immediately WRF since it take the longest. While WRF is compiling, I set things up for HYCOM and UMWM and compile them. WPS is now included, and it also requires WRF to be compiled first. Building UWIN results in new executable `uwincm`.


#### Compile ESMF

From the top-level directory, type:
```
make esmf
```
or `make esmf_info` to display compile-time options for building ESMF.


#### Compile WRF

Choose appropriate verion (3.6.1, 3.7.1, 3.8.1, 3.9, or one of the modified 3.9 versions) and run the configure script in the respective WRF directory. On orca, the compiling option selection that works is 15. In general, for the nesting option we select 3 for vortex following. These are the options that we are presented with on orca:
```
Please select from among the following Linux x86_64 options:

  1. (serial)   2. (smpar)   3. (dmpar)   4. (dm+sm)   PGI (pgf90/gcc)
  5. (serial)   6. (smpar)   7. (dmpar)   8. (dm+sm)   PGI (pgf90/pgcc): SGI MPT
  9. (serial)  10. (smpar)  11. (dmpar)  12. (dm+sm)   PGI (pgf90/gcc): PGI accelerator
 13. (serial)  14. (smpar)  15. (dmpar)  16. (dm+sm)   INTEL (ifort/icc)
                                         17. (dm+sm)   INTEL (ifort/icc): Xeon Phi (MIC architecture)
 18. (serial)  19. (smpar)  20. (dmpar)  21. (dm+sm)   INTEL (ifort/icc): Xeon (SNB with AVX mods)
 22. (serial)  23. (smpar)  24. (dmpar)  25. (dm+sm)   INTEL (ifort/icc): SGI MPT
 26. (serial)  27. (smpar)  28. (dmpar)  29. (dm+sm)   INTEL (ifort/icc): IBM POE
 30. (serial)               31. (dmpar)                PATHSCALE (pathf90/pathcc)
 32. (serial)  33. (smpar)  34. (dmpar)  35. (dm+sm)   GNU (gfortran/gcc)
 36. (serial)  37. (smpar)  38. (dmpar)  39. (dm+sm)   IBM (xlf90_r/cc_r)
 40. (serial)  41. (smpar)  42. (dmpar)  43. (dm+sm)   PGI (ftn/gcc): Cray XC CLE
 44. (serial)  45. (smpar)  46. (dmpar)  47. (dm+sm)   CRAY CCE (ftn $(NOOMP)/cc): Cray XE and XC
 48. (serial)  49. (smpar)  50. (dmpar)  51. (dm+sm)   INTEL (ftn/icc): Cray XC
 52. (serial)  53. (smpar)  54. (dmpar)  55. (dm+sm)   PGI (pgf90/pgcc)
 56. (serial)  57. (smpar)  58. (dmpar)  59. (dm+sm)   PGI (pgf90/gcc): -f90=pgf90
 60. (serial)  61. (smpar)  62. (dmpar)  63. (dm+sm)   PGI (pgf90/pgcc): -f90=pgf90
 64. (serial)  65. (smpar)  66. (dmpar)  67. (dm+sm)   INTEL (ifort/icc): HSW/BDW
 68. (serial)  69. (smpar)  70. (dmpar)  71. (dm+sm)   INTEL (ifort/icc): KNL MIC
 72. (serial)  73. (smpar)  74. (dmpar)  75. (dm+sm)   FUJITSU (frtpx/fccpx): FX10/FX100 SPARC64 IXfx/Xlfx

Enter selection [1-75] : 15
------------------------------------------------------------------------
Compile for nesting? (1=basic, 2=preset moves, 3=vortex following) [default 1]: 3
```

Check that the configure command was successful. It should produce output ending like this:
```
Settings listed above are written to configure.wrf.
If you wish to change settings, please edit that file.
If you wish to change the default options, edit the file:
     arch/configure_new.defaults
NetCDF users note:
 This installation of NetCDF supports large file support.  To DISABLE large file
 support in NetCDF, set the environment variable WRFIO_NCD_NO_LARGE_FILE_SUPPORT
 to 1 and run configure again. Set to any other value to avoid this message.


Testing for NetCDF, C and Fortran compiler

This installation of NetCDF is 64-bit
                 C compiler is 64-bit
           Fortran compiler is 64-bit
              It will build in 64-bit
```
Then, compile using `compile em_real`.

For example:
```
cd src/components/WRFV3.9
./configure
nohup compile em_real >& log.compile &
```
When the compilation is successful, the end of the output should look like this:
```
==========================================================================
build started:   Mon 06 Feb 2023 04:27:46 PM PST
build completed: Mon 06 Feb 2023 05:07:34 PM PST

--->                  Executables successfully built                  <---

-rwxr-xr-x 1 bkerns atgstaff 44771184 Feb  6 17:07 main/ndown.exe
-rwxr-xr-x 1 bkerns atgstaff 44839552 Feb  6 17:07 main/real.exe
-rwxr-xr-x 1 bkerns atgstaff 44159040 Feb  6 17:07 main/tc.exe
-rwxr-xr-x 1 bkerns atgstaff 55542792 Feb  6 17:06 main/wrf.exe

==========================================================================
```


Note: Running "make wrf" from the main uwincm directory will also work for the `compile em_real` step. 

BUT DO NOT RUN "make wrf" IN THE WRF DIRECTORY!!!


#### Compile WPS.

For generating WRF input files, you may need to compile a fresh version of the WRF Preprocessing System (WPS). The repository provides WPSV3.9.1. For other versions, e.g., 4.2 which is needed for GFS/CFS and ECMWF realtime forecast data, feel free to download from [https://github.com/wrf-model/WPS](https://github.com/wrf-model/WPS) and extract another version of WPS under the src/components directory.

Note: Compiling WPS depends on WRF having been compiled in the above step. WPS uses some libraries created by WRF.

Here is how to compile a fresh version of WPS:

1. Get into the WPS directory. e.g., `cd src/components/WPS-3.9.1`.
2. Run the ./configure step. The serial options are probably fine. I chose
```  17.  Linux x86_64, Intel compiler    (serial)```
But the gfortran one would probably work (but run slower):
```   1.  Linux x86_64, gfortran    (serial)```
3. Edit the WRF directory in the configure.wps file. e.g.,`WRF_DIR			=	../WRFV3.9`.
4. Run `./compile` to compile it. If successful, you should see WPS executables with links like this:
```
$ ls -l *.exe
lrwxrwxrwx 1 bkerns atgstaff 23 Feb  6 17:44 geogrid.exe -> geogrid/src/geogrid.exe
lrwxrwxrwx 1 bkerns atgstaff 23 Feb  6 17:46 metgrid.exe -> metgrid/src/metgrid.exe
lrwxrwxrwx 1 bkerns atgstaff 21 Feb  6 17:45 ungrib.exe -> ungrib/src/ungrib.exe

```
You can also check whether the WPS utility executables were created:
```
$ ls -l util/*.exe
lrwxrwxrwx 1 bkerns atgstaff 16 Feb  6 17:46 util/avg_tsfc.exe -> src/avg_tsfc.exe
lrwxrwxrwx 1 bkerns atgstaff 20 Feb  6 17:46 util/calc_ecmwf_p.exe -> src/calc_ecmwf_p.exe
lrwxrwxrwx 1 bkerns atgstaff 25 Feb  6 17:46 util/g1print.exe -> ../ungrib/src/g1print.exe
lrwxrwxrwx 1 bkerns atgstaff 25 Feb  6 17:46 util/g2print.exe -> ../ungrib/src/g2print.exe
lrwxrwxrwx 1 bkerns atgstaff 19 Feb  6 17:46 util/height_ukmo.exe -> src/height_ukmo.exe
lrwxrwxrwx 1 bkerns atgstaff 14 Feb  6 17:46 util/int2nc.exe -> src/int2nc.exe
lrwxrwxrwx 1 bkerns atgstaff 16 Feb  6 17:46 util/mod_levs.exe -> src/mod_levs.exe
lrwxrwxrwx 1 bkerns atgstaff 23 Feb  6 17:46 util/rd_intermediate.exe -> src/rd_intermediate.exe
```

NOTE: The script util/plotgrids_new_png.ncl has been added for this repo. It is not originally provided with WPS.


#### Compile UMWM

From the top-level directory, type:
```
make umwm
```


#### Compile HYCOM

This applies to version 2.2.34, 2.2.34_coldstart, and 2.2.99 in non-RELO mode.
In the directory like `src/components/hycom/src_2.2.34/`, copy `dimensions.h.MASTER to `dimensions.h`
and edit `dimensions.h` with the appropriate domain and tile dimensions.
Then from the top-level directory, type:
```
make hycom
```

#### Compile UWIN

When all the model components are built, compile the coupling interface by typing from
the top-level directory:
```
make uwincm
```

## Further reading

* Additional README files in this repository:
     + [UMWM3](https://gitlab.com/uwincm/umwm3/-/blob/dev/README.md)
     + [wrf_input](wrf_input/README.md) directory
     + [run](run/README.md) directory

* This repository has a [Wiki](https://gitlab.com/uwincm/uwincm/-/wikis/home) associated with it.
* [UWIN-CM gitbook documentation](https://www.gitbook.com/book/milancurcic/uwincm-manual/details)

## Publications

* Curcic, M., S. S. Chen, and T. M. Özgökmen, 2016: Hurricane-induced ocean waves and Stokes drift and their impacts on surface transport and dispersion in the Gulf of Mexico, *Geophys. Res. Lett.*, **43**, 2773–2781, doi:10.1002/2015GL067619. [PDF](https://github.com/milancurcic/publications/blob/master/Curcic_etal_GRL2016.pdf)

* Chen, S. S. and M. Curcic, 2016: Ocean surface waves in Hurricane Ike (2008) and Superstorm Sandy (2012): Coupled modeling and observations, *Oce. Mod.*, **103**, 161-176. doi:10.1016/j.ocemod.2015.08.005. [PDF](https://github.com/milancurcic/publications/blob/master/Chen_and_Curcic_OM2016.pdf)

* Judt, F., S. S. Chen, and M. Curcic, 2016: Atmospheric forcing of the upper ocean transport in the Gulf of Mexico: From seasonal to diurnal scales, *J. Geophys. Res. Oceans*, **121**, 4416–4433, doi:10.1002/2015JC011555. [PDF](https://github.com/milancurcic/publications/blob/master/Judt_etal_JGR2016.pdf)

* Zhu, P., Y. Wang, S. S. Chen, M. Curcic, and C. Gao, 2016: Impact of storm-induced cooling of sea surface temperature on large turbulent eddies and vertical turbulent transport in the atmospheric boundary layer of Hurricane Isaac, *J. Geophys. Res. Oceans*, **121**, 861–876, doi:10.1002/2015JC011320. [PDF](https://github.com/milancurcic/publications/blob/master/Zhu_etal_JGR2016.pdf)

* Kim, E., M. Lance, M. Curcic, S. S. Chen, C. Phillips, and P. Veers, 2016: On the use of coupled wind, wave, and current fields in the simulation of loads on bottom-supported offshore wind turbines during hurricanes, *Technical Report NREL/TP--5000-65283*, National Renewable Energy Lab. (NREL), Golden, CO, United States, doi:10.2172/1266702. [Link](http://www.osti.gov/scitech/biblio/1266702)

* Curcic, M., 2015: Explicit air-sea momentum exchange in coupled atmosphere-wave-ocean modeling of tropical cyclones, *Ph.D. Thesis*, University of Miami. [Link](http://scholarlyrepository.miami.edu/oa_dissertations/1512)

* Curcic M., E. Kim, L. Manuel, S. S. Chen, M. A. Donelan, J. Michalakes, 2013: Coupled atmosphere-wave-ocean modeling to characterize hurricane load cases for offshore wind turbines, *51st AIAA Aerospace Sciences Meeting*, January 2013, Grapevine TX, doi:10.2514/6.2013-198. [PDF](https://github.com/milancurcic/publications/blob/master/Curcic_etal_AIAA2013.pdf)

* Donelan, M. A., M. Curcic, S. S. Chen, and A. K. Magnusson, 2012: Modeling waves and wind stress, *J. Geophys. Res. Oceans*, **117**, C00J23, doi:10.1029/2011JC007787. [PDF](https://github.com/milancurcic/publications/blob/master/Donelan_etal_JGR2012.pdf)
