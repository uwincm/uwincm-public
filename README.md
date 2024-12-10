# Unified Wave INterface - Coupled Model (UWIN-CM)

This is the source code repository for the Unified Wave INterface - Coupled Model (UWIN-CM)
developed by Prof. Shuyi S. Chen and Milan Curcic and maintained by Brandon Kerns since 2017.

Several others have contributed improvements to the model code, including Dalton Kai Sasaki, Benjamin Barr, and Ajda Savarin.

**This code was originally on Gitlab. This version here on Github is the latest.**


## Getting started

### Getting the source code

+ Version tags are "official" releases. The most recent is v1.0.
+ The *master* branch is the most updated, stable and tested.
+ The *dev* branch has more updates and features, but may have bugs.

#### -- Checking out the master branch --
The master branch is the latest "official" version of UWIN-CM. It is the safest and most stable branch to use.

If you have added an SSH key from your local system to your GitHub account, you can clone the repo like this:
```
git clone --recursive git@github.com:uwincm/uwincm.git
```

Alternatively, if you have set up HTTPS access, you can clone the repo like this:
```
git clone --recursive https://github.com/uwincm/uwincm.git
```
NOTE: This method is more involved than before, since username/password authentication is no longer allowed. See 
[Caching your GitHub credentials in Git](https://docs.github.com/en/get-started/getting-started-with-git/caching-your-github-credentials-in-git).


#### -- Checking out the dev branch --
The dev branch may have recent updates not yet included in master.

In order to ensure that you're getting the full content of any submodules, use `-b dev` when cloning the dev branch
rather than cloning master and manually switching to dev.

For the SSH key access, clone the repo like this:
```
git clone --recursive -b dev git@github.com:uwincm/uwincm.git
```

And for HTTPS:
```
git clone --recursive -b dev https://github.com/uwincm/uwincm.git
```

### Setting up the environment

The `env` directory contains scripts for setting up the environment
on a specific compute system. If the script for the desired system
is not available, you can create one and contribute it to the repo,
or request it by [opening an issue](https://gitlab.com/uwincm/uwincm/issues/new).
In general, these scripts set the environment that you would use with uncoupled WRF
in addition to the ESMF environment variables.

Set up your environment sourcing the relevant script in the env/ directory:
* For running on the orca nodes o1 - o8, use `source env/set_env_orca.sh`.
* For running on the orca nodes o9 - o11, use `source env/set_env_o9.sh`.

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

Choose appropriate verion (3.6.1, 3.7.1, 3.8.1, 3.9, or one of the modified 3.9 versions) and run the configure script in the respective WRF directory. For example, for WRFV3.9, you would get into the directory **src/components/WRFV3.9**, run the `./configure` script, then run `./compile em_real`. On orca, the compiling option selection that works is 15. This is the same for the nodes o1 - o8 and for o9 - o12. In general, for the nesting option we select 3 for vortex following. These are the options that we are presented with on orca:
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
(Note that this will compile UMWM3, which is set in the Makefile.)


#### Compile HYCOM

This applies to version 2.2.34, 2.2.34_coldstart, and 2.2.99 in non-RELO mode.
Currently, we are using 2.2.99 for running the model and 2.2.34_coldstart for the 
cold start run used in [hycom-domain](https://github.com/uwincm/hycom-domain).
In the directory like **src/components/hycom/src_2.2.99**, copy `dimensions.h.MASTER to `dimensions.h`
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


### Setting up and managing run directories.

A template run directory is provided in the run/ directory. See the [run directory README.md](run/README.md) for more details. In summary, the namelist files are copied from MASTER files, and the static inputs and initial/boundary conditions for each model component are copied or linked into the run directory using the run/Makefile and the script run/cpfile.sh.

You can have more than one run directory beneath this parent uwincm directory! A common and useful work flow is to make entire copies of a run directory in order to change something in the namelists, turn on/off coupling, change initialization time, vortex relocation, and so on. To make copies of run directories without having to copy over any output that has been produced, the script copy_run_directory_excluding_output.sh is included. Use it like `copy_run_directory_excluding_output.sh run copy_of_run`. 


### Helpful scripts for managing run directories and model output.

#### Copying run directories

A common work flow is to set up a run directory, then make copies of it to run the model with small changes. This is useful for testing purposes and sensitivity tests. For example, you may set up an initial run directory to get something to run in uncoupled mode, then make a copy to turn on the ocean, then another copy to turn on ocean and waves. Or perhaps you are testing different vortex relocations for a hurricane case.

To avoid making unnecessary copies of many large output files, the script [copy](copy_run_directory_excluding_output.sh) can be used. For example, to copy **run/** to **run2**, you would invoke `copy_run_directory_excluding_output.sh run run2`.


#### Compressing model output

High resolution models can produce very large volumes of output data, on the order of terabytes per run!

There are several approaches you can take to minimize your "disk space footprint":
- Delete old run directories that were for testing purposes and won't be used for production.
- Delete the wrfrst and/or umwmrst files from run directories in which restarting will not be necessary. Each wrfrst file takes up around TEN wrfout files worth of disk space!
  * Tip: You can keep the last restart files in case you might need to extend the run!
- gzip the HYCOM archv a files. Many analysis softwares like Python can addess .gz files directly, at the cost of some performance.
  * I like to use [pigz](https://zlib.net/pigz/) to gzip the files in parallel, e.g., `pigz -n 10 archv*.a` to do all of the archv a files in parallel using 10 cores.
- Compress the other outputs, especially wrfout (and wrfrst if you want to keep them). 


#### Compressing model output

UWIN-CM output by default is NOT compressed. Using NetCDF compression can reduce the file sizes significantly. There is a script [compress_output.sh](run/compress_output.sh) that can accomplish this. The easiest way is to specify "all" NetCDF output files, e.g., `./compress_output.sh all`. Alternately, you can specify which sets of files you want to do. Specify one of more of: wrfout, wrfrst, cplout, umwmout, umwmrst. For example, to do wrfout only, call it like `./compress_output.sh wrfout`. Don't worry about double compressing things, as the script will check whether the files are already compressed.


## Restarting model runs

For various reasons, you may need to restart a UWIN-CM run.
* The run needs to be extended beyond the time period you originally specified.
* Re-run a portion of the run with an additional inner nest.
* A portion of the run needs to be re-run with different physics, nudging options, etc.
* The run crashed or got hung up before completing and you need to finish the run.

### Determining the Restart Time

Unfortunately, we cannot restart the model from any time. There needs to be a set of restart files **valid at the same time** for each model component, e.g., WRF, HYCOM, and UMWM3. The restart files are named like this (relative to run/ directory):
* For WRF: **wrfrst_d01_2017-09-20_06:00:00**
  - There needs to be a wrfrst file for each domain you wish to restart.
* For UMWM3: **restart/umwmrst_2017-09-20_06:00:00.nc**
* for HYCOM: **restart_out.[ab]**, **restart_out1.[ab]** (You can use either file, they will have different valid times).

The valid times of these files are controlled by the namelists. *These should be set to the same time intervals so there is a set of restart files at each time.*
* For WRF: in **namelist.input**: `restart_interval                    = 360,` (in minutes)
* For UMWM3: in **namelists/main.nml**: `outrst        = 6` (in hours)
* For HYCOM: in **blkdat.input**: `   0.25   'rstrfq' = number of days between model restart output` (in days)
  - *For HYCOM, only the last two restart files are retained.* They will get over-written as the run progresses.
  - HYCOM will usually be the limiting factor for choosing the restart time.

To get the valid times:
* The valid times of wrfrst and umwmrst are indicated in the file names.
* The valid times of the HYCOM files are trickier. See the top of the .b file. The valid time is on the second line. But it is a HYCOM decimal time. Easiest way to figure out what time it represents is to compare it with your restart_in.b file.
```$ head restart_out1.b                                                                                              [13:11:15]
RESTART2: iexpt,iversn,yrflag,sigver =    201    22     3     6
RESTART2: nstep,dtime,thbase =     61390440   42632.2500000000        34.0000000000000
u       : layer,tlevel,range =   1  1    -1.9490675E+00   1.7865663E+00
u       : layer,tlevel,range =   2  1    -1.9309760E+00   1.7643423E+00
```
In this case, the restart_in.b file has a valid time of 42630.0, which is the model start time of 2017-09-18 00Z.
```$ head restart_in.b                                                                                                [12:57:34]
RESTART2: iexpt,iversn,yrflag,sigver =    930    22     3     2
RESTART2: nstep,dtime,thbase =     15346800   42630.000000000000        34.0000000
u       : layer,tlevel,range =   1  1    -1.5812533E+00   2.1143599E+00
u       : layer,tlevel,range =   2  1    -1.5835017E+00   2.1519227E+00
Therefore, the valid time of this file is 2.25 days later, 2017-09-20 06Z.

In this example, we have valid restart files for all three components for 2017-09-20 06Z, so let's use that as the restart time.

(This example was for a run that got hung up at the valid time 2017-09-20 09Z, so it will be repeating 3 h of output.)


### Setting Up the Restart Run

#### Edit namelists
- The model start time needs to be set to the restart time in: **uwin.nml**, **namelist.input**, **namelists/main.nml**.
- In **namelist.input**, set `restart                             = .t.,`
- In **namelists/main.nml**, set `restart      = .true.`
- In **blkdat.input**, update the tides reference time to match the restart time: `42632.25  'tid_t0' = TIDES: origin for ramp time (model day)`

#### Remove or rename overlapping HYCOM output files
+++ *HYCOM will CRASH* if it tries to write to an output file that already exist +++

Therefore, we need to get rid of files that already exist and that HYCOM will try to write to in the restart run.
- Delete archv a, b, and txt files that are after the restart time. In this case,
```
rm archv.2017_263_07.*
rm archv.2017_263_08.*
rm archv.2017_263_09.*
```

I also usually `rm summary.out` and `rm ovrtn.out` if they exist, just to be safe.

#### Deal with HYCOM restart files.

HYCOM always runs as a restart run, using restart_in.[ab] as "initial conditions."

It is recommended to rename the original restart_in files in case you re-run from the beginning:
```
mv restart_in.a restart_in.a.orig
mv restart_in.b restart_in.b.orig
```
Next, re-name the restart_out files to restart_in. In this case:
```
mv restart_out1.a restart_in.a
mv restart_out1.b restart_in.b
```
Finally, it is recommended to rename the other restart_out files in case you need them:
```
mv restart_out.a restart_out.a.bak
mv restart_out.b restart_out.b.bak
```

Finally, you should be able to submit the run as you would a regular run: `qsub job.sh`.


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
