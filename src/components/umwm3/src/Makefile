# University of Miami Wave Model Makefile
#
# FC, FCFLAGS, CPPFLAGS, and NETCDF 
# must be set in the environment, e.g.:
#
# FC       = mpif90
# FCFLAGS  = -O3 -w
# CPPFLAGS = -DMPI

NETCDFINC = -I$(NETCDF)/include
NETCDFLIB = -L$(NETCDF)/lib -lnetcdf -lnetcdff
EXTINC = -Iexternal/datetime-fortran/build/include
EXTLIB = -Lexternal/datetime-fortran/build/lib -ldatetime

# Rules, object list and dependencies start here.

.SUFFIXES: .F90 .o

# Rule
.F90.o:
	$(FC) -c $(CPPFLAGS) $(FCFLAGS) $(NETCDFINC) $(EXTINC) $<

# Object list
OBJS = umwm_top.o \
       umwm_module.o \
       umwm_constants.o \
       umwm_mpi.o \
       umwm_init.o \
       umwm_physics.o \
       umwm_advection.o \
       umwm_io.o \
       umwm_forcing.o \
       umwm_util.o \
       umwm_restart.o \
       umwm_source_functions.o \
       umwm_stokes.o

all: umwm

#datetime.o:
external/datetime-fortran/build/lib/libdatetime.a:
	mkdir external/datetime-fortran/build
	cd external/datetime-fortran/build && FC=$(FC) cmake .. && make

# Main target
#umwm: umwm.F90 datetime.o $(OBJS)
umwm: umwm.F90 external/datetime-fortran/build/lib/libdatetime.a $(OBJS)
	$(FC) -c $(CPPFLAGS) $(FCFLAGS) $(NETCDFINC) $(EXTINC) $<
	$(FC) $(FCFLAGS) -o $@ umwm.o $(OBJS) $(NETCDFLIB) $(EXTLIB)
	cp $@ ../.
	ar ru libumwm.a $(OBJS)

# Object          Source             Dependencies
umwm_top.o:       umwm_top.F90       umwm_constants.o umwm_module.o umwm_mpi.o umwm_forcing.o umwm_physics.o umwm_advection.o umwm_io.o umwm_init.o umwm_restart.o umwm_source_functions.o
umwm_module.o:    umwm_module.F90    
umwm_constants.o: umwm_constants.F90
umwm_mpi.o:       umwm_mpi.F90
umwm_init.o:      umwm_init.F90      umwm_module.o umwm_mpi.o umwm_util.o umwm_io.o umwm_constants.o
umwm_physics.o:   umwm_physics.F90   umwm_module.o umwm_io.o  umwm_advection.o
umwm_advection.o: umwm_advection.F90 umwm_module.o umwm_io.o  umwm_constants.o
umwm_io.o:        umwm_io.F90        umwm_module.o umwm_mpi.o umwm_util.o umwm_stokes.o
umwm_forcing.o:   umwm_forcing.F90   umwm_module.o umwm_io.o  umwm_util.o
umwm_util.o:      umwm_util.F90      umwm_module.o
umwm_restart.o:   umwm_restart.F90   umwm_module.o umwm_io.o
umwm_stokes.o:    umwm_stokes.F90    umwm_module.o
umwm_source_functions.o: umwm_source_functions.F90 umwm_constants.o umwm_io.o

.PHONY:
clean:
	rm -rf external/datetime-fortran/build
	rm -f umwm *.o *.mod libumwm.a 
