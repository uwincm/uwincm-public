
ESMF_MK=${ESMF_DIR}/lib/lib${ESMF_BOPT}/Linux.${ESMF_COMPILER}.${ESMF_ABI}.${ESMF_COMM}.default/esmf.mk
include $(ESMF_MK)

# WRF
WRFDIR  = ../components/WRFV${wrf_version}
WRFPATH = -I$(WRFDIR)/main \
          -I$(WRFDIR)/external/esmf_time_f90 \
          -I$(WRFDIR)/frame \
          -I$(WRFDIR)/share \
          -I$(WRFDIR)/dyn_em \
          -I$(WRFDIR)/phys
WRFLIB  = $(WRFDIR)/main/libwrflib.a \
          $(WRFDIR)/external/esmf_time_f90/libesmf_time.a \
          $(WRFDIR)/external/fftpack/fftpack5/libfftpack.a \
          $(WRFDIR)/external/io_grib1/libio_grib1.a \
          $(WRFDIR)/external/io_grib_share/libio_grib_share.a \
          $(WRFDIR)/external/io_int/libwrfio_int.a \
          $(WRFDIR)/external/RSL_LITE/librsl_lite.a \
          $(WRFDIR)/frame/module_internal_header_util.o \
          $(WRFDIR)/frame/pack_utils.o \
          $(WRFDIR)/external/io_netcdf/libwrfio_nf.a

# HYCOM
HYCOMDIR  = ../components/hycom/src_$(hycom_version)
HYCOMPATH = -I$(HYCOMDIR)
HYCOMLIB  = $(HYCOMDIR)/libhycom.a

# UMWM
UMWMDIR  = ../components/${wave_component}/src
UMWMPATH = -I$(UMWMDIR)
UMWMLIB = $(UMWMDIR)/libumwm.a $(UMWMDIR)/external/datetime-fortran/build/lib/libdatetime.a

LIBS = $(WRFLIB) $(HYCOMLIB) $(UMWMLIB)

# NETCDF -- must be set in environment.
NETCDFPATH = -I$(NETCDF)/include -L$(NETCDF)/lib -lnetcdff

F90COMPILEPATHS = $(ESMF_F90COMPILEPATHS) $(HYCOMPATH) $(NETCDFPATH) $(WRFPATH) $(UMWMPATH)
F90LINKPATHS    = $(ESMF_F90LINKPATHS) $(HYCOMPATH) $(NETCDFPATH) $(WRFPATH) $(UMWMPATH)

OBJ = UWIN_constants.o \
      UWIN_coupler.o \
      UWIN_global.o \
      UWIN_utility.o \
      UWIN_interface_WRF.o \
      UWIN_interface_HYCOM.o \
      UWIN_interface_UMWM.o \
      UWIN_ExchangeGrid.o \
      UWIN_GriddedComponent.o \
      UWIN_physics.o \
      COAMPS_Gsrumd.o \
      COAMPS_Util.o \
      UWIN_spray_interface.o \
      UWIN_spray_physics.o

#FCFLAGS = -O0 -g -check bounds -traceback -heap-arrays -nowarn -zero -fp-model source
FCFLAGS = -O3 -heap-arrays -nowarn -zero -fp-model source
ESMF_F90COMPILEOPTS := $(ESMF_F90COMPILEOPTS) $(FCFLAGS) -DSINGLE

.SUFFIXES: .F90 .o

all:   uwincm

uwincm: uwin.F90 $(OBJ) $(LIBS)
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP) $<
	ar ruv $(WRFDIR)/main/libwrflib.a $(WRFDIR)/main/module_wrf_top.o
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ uwin.o $(OBJ) $(ESMF_F90ESMFLINKLIBS) $(HYCOMLIB) $(WRFLIB) $(UMWMLIB)

# Rule
.F90.o:
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP) $<

# Targets
UWIN_constants.o:        UWIN_constants.F90
UWIN_coupler.o:          UWIN_coupler.F90 UWIN_global.o UWIN_utility.o UWIN_ExchangeGrid.o COAMPS_Util.o UWIN_physics.o UWIN_spray_interface.o
UWIN_interface_WRF.o:    UWIN_interface_WRF.F90 UWIN_global.o UWIN_utility.o UWIN_physics.o
UWIN_interface_UMWM.o:   UWIN_interface_UMWM.F90 UWIN_global.o UWIN_utility.o
UWIN_interface_HYCOM.o:  UWIN_interface_HYCOM.F90 UWIN_global.o
UWIN_ExchangeGrid.o:     UWIN_ExchangeGrid.F90 UWIN_global.o UWIN_utility.o UWIN_interface_WRF.o
UWIN_GriddedComponent.o: UWIN_GriddedComponent.F90
UWIN_global.o:           UWIN_global.F90 UWIN_GriddedComponent.o
UWIN_physics.o:          UWIN_physics.F90 UWIN_global.o UWIN_utility.o COAMPS_Util.o 
UWIN_utility.o:          UWIN_utility.F90 UWIN_global.o UWIN_constants.o
COAMPS_Util.o:           COAMPS_Util.F90 COAMPS_Gsrumd.o
COAMPS_Gsrumd.o:         COAMPS_Gsrumd.F90
UWIN_spray_interface.o:  UWIN_spray_interface.F90 UWIN_spray_physics.o UWIN_global.o COAMPS_Util.o UWIN_ExchangeGrid.o
UWIN_spray_physics.o:    UWIN_spray_physics.F90 UWIN_global.o 

.PHONY:
clean:
	rm -f uwincm *.o *.mod
