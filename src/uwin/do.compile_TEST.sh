gfortran -c UWIN_physics_spray.F90
gfortran -c -I"." TEST_physics_spray.F90
gfortran -o TEST_physics_spray.exe UWIN_physics_spray.o TEST_physics_spray.o