#-------------------------------------------------------------------------------

all: cg.exe vortex.exe vorrel.exe

clean:  
	rm *.o *.mod *.exe

#-------------------------------------------------------------------------------
OBJS3 = module_definition.o module_constants.o \
	module_netcdf.o module_netcdf_cylindrical.o module_map_utils.o module_wrf_tools.o \
        module_vortex.o module_vortex_cylindrical.o module_get_variables.o module_wrf_io.o \
	vortex_relocation.o
OBJS2 =  module_definition.o module_constants.o \
        module_netcdf.o module_netcdf_cylindrical.o module_map_utils.o module_wrf_tools.o \
        module_vortex.o module_vortex_cylindrical.o module_get_variables.o module_wrf_io.o \
        module_cg_tools.o \
         cg.o
OBJS =  module_definition.o module_constants.o \
        module_netcdf.o module_netcdf_cylindrical.o module_map_utils.o module_wrf_tools.o \
        module_vortex.o module_vortex_cylindrical.o module_get_variables.o module_wrf_io.o \
        module_wind_radii_tail.o \
         VORTEX.o

vortex.exe: $(OBJS)
	$(FC) -o vortex.exe $(OBJS) -L$(NETCDF)/lib -lnetcdff

cg.exe: $(OBJS2)
	$(FC) -o cg.exe  $(OBJS2)   -L$(NETCDF)/lib -lnetcdff

vorrel.exe: $(OBJS3)
	$(FC) -o vorrel.exe  $(OBJS3)   -L$(NETCDF)/lib -lnetcdff
#-------------------------------------------------------------------------------

module_definition.o:
	$(FC) -c $(FCFLAGS) module_definition.f90

module_constants.o:
	$(FC) -c $(FCFLAGS) module_constants.f90

module_netcdf.o:
	$(FC) -c $(FCFLAGS) -I$(NETCDF)/include  module_netcdf.f

module_netcdf_cylindrical.o:
	$(FC) -c $(FCFLAGS) -I$(NETCDF)/include module_netcdf_cylindrical.f

module_map_utils.o:
	$(FC) -c $(FCFLAGS) module_map_utils.f

module_wrf_tools.o:
	$(FC) -c $(FCFLAGS) module_wrf_tools.f

module_vortex.o:
	$(FC) -c $(FCFLAGS) module_vortex.f90

module_vortex_cylindrical.o:
	$(FC) -c $(FCFLAGS) module_vortex_cylindrical.f90

module_get_variables.o:
	$(FC) -c $(FCFLAGS) module_get_variables.f90

module_wrf_io.o:
	$(FC) -c $(FCFLAGS) module_wrf_io.f90

module_cg_tools.o:
	$(FC) -c $(FCFLAGS) module_cg_tools.f90

module_wind_radii_tail.o:
	$(FC) -c $(FCFLAGS) module_wind_radii_tail.f90

vortex_relocation.o:
	$(FC) -c $(FCFLAGS) vortex_relocation.f90

cg.o:
	$(FC) -c $(FCFLAGS) cg.f90 

VORTEX.o:
	$(FC) -c $(FCFLAGS) VORTEX.f90

#-------------------------------------------------------------------------------
