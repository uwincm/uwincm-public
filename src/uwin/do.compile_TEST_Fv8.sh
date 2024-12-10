gfortran-8 -c UWIN_physics_spray.F90
gfortran-8 -c -I"." TEST_physics_spray.F90
gfortran-8 -o TEST_physics_spray.exe UWIN_physics_spray.o TEST_physics_spray.o
