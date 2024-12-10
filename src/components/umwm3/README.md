## University of Miami Wave Model (UMWM) v2.1

A third-generation spectral ocean wave model.

### Getting started

#### Getting the code

```
git clone --recursive git@gitlab.com:uwincm/umwm.git
```

#### Compiling

Set the following environment variables:

* `FC`: Fortran compiler (`mpif90` for parallel builds)
* `FCFLAGS`: Flags to pass to the Fortran compiler
* `CPPFLAGS`: Pre-processor flags
  - `-DMPI`: for parallel builds with MPI
  - `-DESMF`: for builds with UWIN-CM + ESMF
* `NETCDF`: Path to the NetCDF library, assuming 
library files are in `$NETCDF/lib` and module files are in `$NETCDF/include`.

Type `make`. Executable file called `umwm` will be built in the 
base `umwm/` directory. Auxilliary tools executables will be 
built in the `tools/` directory. Documentation will be
built in the `docs/` directory.

Version 2.1 documentation addendum is located at docs/umwm_addendum_v2.1.

### Publications

Publications about or using UMWM.

#### 2018

* Dietrich, J. C., A. Muhammad, M. Curcic, A. Fathi, C. N. Dawson, S. S. Chen, and R. A. Luettich Jr., 2018: Sensitivity of storm surge predictions to atmospheric forcing during Hurricane Isaac, *J. Waterway, Port, Coastal, Ocean Eng.*, **144**(1): 04017035. [PDF](https://github.com/milancurcic/publications/blob/master/Dietrich_etal_WWENG2018.pdf)

#### 2016

* Kim, E., M. Lance, M. Curcic, S. S. Chen, C. Phillips, and P. Veers, 2016: On the use of coupled wind, wave, and current fields in the simulation of loads on bottom-supported offshore wind turbines during hurricanes, *Technical Report NREL/TP--5000-65283*, National Renewable Energy Lab. (NREL), Golden, CO, United States, doi:10.2172/1266702. [Link](http://www.osti.gov/scitech/biblio/1266702)

* Judt, F., S. S. Chen, and M. Curcic, 2016: Atmospheric forcing of the upper ocean transport in the Gulf of Mexico: From seasonal to diurnal scales, *J. Geophys. Res. Oceans*, **121**, 4416-4433, doi:10.1002/2015JC011555. [PDF](https://github.com/milancurcic/publications/blob/master/Judt_etal_JGR2016.pdf)

* Curcic, M., S. S. Chen, and T. M. Ozgökmen, 2016: Hurricane-induced ocean waves and Stokes drift and their impacts on surface transport and dispersion in the Gulf of Mexico, *Geophys. Res. Lett.*, **43**, 2773–2781, doi:10.1002/2015GL067619. [PDF](https://github.com/milancurcic/publications/blob/master/Curcic_etal_GRL2016.pdf)

* Zhu, P., Y. Wang, S. S. Chen, M. Curcic, and C. Gao, 2016: Impact of storm-induced cooling of sea surface temperature on large turbulent eddies and vertical turbulent transport in the atmospheric boundary layer of Hurricane Isaac, *J. Geophys. Res. Oceans*, **121**, 861–876, doi:10.1002/2015JC011320. [PDF](https://github.com/milancurcic/publications/blob/master/Zhu_etal_JGR2016.pdf)

* Chen, S. S. and M. Curcic, 2016: Ocean surface waves in Hurricane Ike (2008) and Superstorm Sandy (2012): Coupled modeling and observations, *Oce. Mod.*, **103**, 161-176. doi:10.1016/j.ocemod.2015.08.005. [PDF](https://github.com/milancurcic/publications/blob/master/Chen_and_Curcic_OM2016.pdf)

#### 2015

* Curcic, M., 2015: Explicit air-sea momentum exchange in coupled atmosphere-wave-ocean modeling of tropical cyclones, *Ph.D. Thesis*, University of Miami. [Link](http://scholarlyrepository.miami.edu/oa_dissertations/1512)

* Banfield, D., M. A. Donelan, and L. Cavaleri, 2015: Winds, waves and shorelines from ancient martian seas, *Icarus*, **250**, 368-383, doi:10.1016/j.icarus.2014.12.001. [Link](http://www.sciencedirect.com/science/article/pii/S0019103514006794)

#### 2014

* Reichl, B. G., T. Hara, and I. Ginis, 2014: Sea state dependence of the wind stress over the ocean under hurricane winds, *J. Geophys. Res. Oceans*, **119**, 30-51, doi:10.1002/2013JC009289. [Link](http://onlinelibrary.wiley.com/doi/10.1002/2013JC009289/full)

#### 2013

* Curcic M., E. Kim, L. Manuel, S. S. Chen, M. A. Donelan, J. Michalakes, 2013: Coupled atmosphere-wave-ocean modeling to characterize hurricane load cases for offshore wind turbines, *51st AIAA Aerospace Sciences Meeting*, January 2013, Grapevine TX, doi:10.2514/6.2013-198. [PDF](https://github.com/milancurcic/publications/blob/master/Curcic_etal_AIAA2013.pdf)

#### 2012

* Donelan, M. A., M. Curcic, S. S. Chen, and A. K. Magnusson, 2012: Modeling waves and wind stress, *J. Geophys. Res. Oceans*, **117**, C00J23, doi:10.1029/2011JC007787. [PDF](https://github.com/milancurcic/publications/blob/master/Donelan_etal_JGR2012.pdf)
