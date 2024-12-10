## Unified Wave INterface - Coupled Model (UWIN-CM)

This is the source code repository for the Unified Wave INterface - Coupled Model (UWIN-CM)
developed by Prof. Shuyi S. Chen and Milan Curcic.

## Getting started

### Getting the source code

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
First copy Makefile.MASTER to Makefile.

To build UWIN-CM, you need to build ESMF, WRF, UMWM, HYCOM, and UWIN. 
WRF, UMWM, and HYCOM can be built in any order, but ESMF has to be build first,
and UWIN has to be build last. Building UWIN results in new executable `uwincm`.
Edit the `Makefile` for the desired versions of each model component.

#### Compile ESMF

From the top-level directory, type:
```
make esmf
```
or `make esmf_info` to display compile-time options for building ESMF.

#### Compile WRF

Choose appropriate verions (3.6.1, 3.7.1, or 3.8.1) and run the configure
script in the respective WRF directory (e.g. `src/components/WRFV3.8.1`).
Once configured, build WRF by typing from the top-level directory:
```
make wrf
```

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
