# UWIN-CM Run Directory

## Setting it up.

Copy Makefile.MASTER to Makefile, edit the paths at the top, then run make.

The Makefile is organized in sections. The most useful Makefile commands are:
- `make help` Displays the Makefile options.
- `make link_wrf` links the WRF executables and static data files. Use **WRF_VERSION** and **WRF_PATH** to tell it which WRF to use.
- `make link_wps` Links the WPS input files met_em and geo_em, also copies namelist.wps. Set **WPS_PATH** at the top of the Makefile.
- `make link_hycom_domain` Links HYCOM inputs from the **HYCOM_DOMAIN_PATH**.
- `make link_hycom_tides` Links HYCOM tide files from the **HYCOM_DOMAIN_PATH**.
- `make link_umwm` Creates and sets up directories for UMWM inputs and outputs.
- `make link_uwin` Makes a copy of the uwincm executable. Makes copies of the uwin.nml and job.sh script.
- `make clean_run` Clears the model output files (wrfout, archv, PET, ect).


## Boundary conditions for HYCOM.

Use the script cpfile.sh to link or copy the HYCOM boundary conditions to the nest/ directory.

usage: cpfile.sh [-c] prefix_to_archv_files
- Will use the files prefix_to_archv_files*.[ab].
- Specify -c to copy instead of ln -sf.


## UWIN-CM namelist options.

The **uwin.nml** file controls the couples system start and end times, exchange grid settings, and which coupling pathways are active. Here is an example from Hurricane Irene:
```
&MAIN
  atmosphere = .T.
  waves      = .T.
  ocean      = .T.
/

&DIMENSIONS
  atmdims =  539,  389 ! idm,jdm of the atmosphere component
  wavdims =  900,  720 ! idm,jdm of the wave component
  ocndims = 1380, 1100 ! idm,jdm of the ocean component
/

&REFINEMENT
  xgRefinementRatio = 9  ! Exchange grid refinement ratio (1 for 12 km, 3 for 4 km, 9 for 1.3 km,...)
/

&TIME
  startDate = 2011, 08, 26, 00, 00, 00
  endDate   = 2011, 08, 29, 00, 00, 00
  timeSteps =  60,  60, 60  ! atmosphere, waves, ocean time steps [s]
  uwinTimeSteps = 60  ! Set to greatest common divisor time steps [s] among the components, e.g., 30 for 30, 60, 60.
/

&COUPLER
  sstFromOcean              = .T. ! at least AO = .T.
  currentsFromOcean         = .T. ! at least AO = .T.
  windStressFromWavesScalar = .T. ! at least AW = .T.
  windStressFromWavesVector = .T. ! at least AW = .T.
  oceanStressFromWaves      = .T. ! at least WO = .T.
  oceanAdvectsWaves         = .T. ! at least WO = .T.
  waveCurrentInteraction    = .F. ! at least WO = .F.
/
```

The settings in each section are described below.

### MAIN

.T. turns on the component. .F. turns it off.

### DIMENSIONS

Specify the grid sizes (x, y) for WRF (atmdims), UMWM (wavdims), and HYCOM (ocndims).  _**These need to match the domain sizes in the individual model namelists!**_

Note that for atmdims, you need to use the _unstaggered_ grid (mass point) dimensions, so _**subtract 1 from the values in namelist.input**_!

### REFINEMENT

The setting **xgRefinementRatio** controls the resolution of the exchange grid. When running WRF with nested domains, it is desirable to have the full resolution of the inner nest reflected on the exchange grid. Generally WRF nest resolution ratio is 3, e.g., 12 km parent nest, 4 km first nest, and 1.3 km second nest. Thus, the refinement level would be a power of 3. The following options are usually used:
- 1 to use the WRF parent domain as the exchange grid. e.g., 12 km resolution.
- 3 to use the first nest resolution (e.g. 4 km)
- 9 to use the second nest resolution (e.g. 1.3 km)

The spin-up time of the model is much greater for higher refinement levels. Thus, for testing and debugging, it is OK to have it set to 1. However, for production runs, be sure to set it back to be appropriate to your highest resolution nest, 3 or 9.


### TIME

- Set the beginning and ending time of the coupled model system. startDate and endDate are in YYYY, MM, DD, HH, MI, SS format.

- timeSteps: The time steps in second of each model component. Atmosphere, then waves, then ocean. Some of them are ignored if one or more model component is turned off.

- uwinTimeSteps: The time step used by the interface layer. Originally it was hardcoded to 60 sec, but now it can be specified, for example, if something other than 12 km outer nest is used, or if a component needs to be run with faster time steps to avoid a CFL condition crash. Set it to the greatest common divisor of each of the model components. For example, if the component time steps are 30, 60, 60, you would set it to 30.


## Submitting a run

The **job.sh** script is provided to submit jobs to the queue on orca. Orca uses the SGE job scheduler system. The settings for Hurricane Irene are below. Consult the documentation for more details.

```
#!/bin/sh
#
# Specify job name
#$ -N my_run
#
# Pass all current environment variables to the job
#$ -V
#
# Specify which nodes to use.
#$ -q all.q@o1,all.q@o2,all.q@o3,all.q@o4
#
# Specify parallel environment (mpi or smp) and number of parallel processes
#$ -pe mpi 96
#
# Verify options and abort if there is an error
#$ -w e
#
# Run in current directory
#$ -cwd
#
# Specify output and error log files
#$ -o job.out
#$ -e job.err

ulimit -s unlimited
# run the model
/usr/bin/time -v mpiexec  -n 96 ./uwincm
```

Submitting a job generally involves two steps:
1. Load the MPI environment. `source env/set_env_orca.sh` or similar. (Only has to be done once. Use the `env` command to verify that MPI stuff is set.)
2. Submit the job script: `qsub job.sh`.

To monitor the progress of the job, use the `qstat` command. `qhost` is also helpful. It tells you the current load on each of the nodes.

Output from the modeling system is written to **job.out**, **job.err**, and a PET file for each node used.


## Using the Vortex Tracker

"WRF-2-Cylindrical" w2c code is now included as a submodule under this directory.
It can be used to track the vortex center, and optionally save 3-D fields in storm relative cylindrical coordinates.

See the README under the w2c/ repository.


## Vortex Relocation

If you have a cg.*.nc file from w2c, you can use it to insert the vortex into wrfinput.
Use the Python script vortex_relocation.py for this.

usage ex: python vortex_relocation.py wrfinput_d01 cg_2017-06-22_00:00:00 -90 25 100 400
or with adding constant u=1, v=2 m/s: python vortex_relocation.py wrfinput_d01 cg_2017-06-22_00:00:00 -90 25 100 400 1 2


