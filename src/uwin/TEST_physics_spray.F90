PROGRAM TEST_physics_spray
!-----------------------------
! Program to test spray heat flux calculations.
!-----------------------------

USE UWIN_physics_spray

INTERFACE
    SUBROUTINE print_array(x)
	    REAL,DIMENSION(:,:),INTENT(IN) :: x
	ENDSUBROUTINE print_array
ENDINTERFACE

REAL :: sourcestrength_test
REAL,DIMENSION(5,3) :: eps_test,wspd10_test,ustar_test,swh_test,&
                       dcp_test,mss_test,t2_test,P_test,q2_test,tsk_test
CHARACTER(10) :: SSGFname_test
REAL,DIMENSION(5,3) :: SHF_spray_nom_test,LHF_spray_nom_test

! Create test data
sourcestrength_test = 1.0
eps_test = reshape((/ 20., 25., 30., 35., 40., 45., 50., 55., 60., 65., 70., 75., 80., 85., 90. /),shape(eps_test))
wspd10_test = reshape((/ 5., 10., 15., 20., 25., 30., 35., 40., 45., 50., 55., 65., 70., 75., 80. /),shape(wspd10_test))
ustar_test = reshape((/ 1., 1.5, 2., 2.5, 3., 3.5, 4., 4.5, 5., 5.5, 6., 6.5, 7., 7.5, 8. /),shape(ustar_test))
swh_test = reshape((/ 0.5, 1., 1.5, 2., 2.5, 3., 3.5, 4., 4.5, 5., 5.5, 6., 6.5, 7., 8. /),shape(swh_test))
dcp_test = reshape((/ 15., 15., 15., 15., 15., 15., 15., 15., 15., 15., 15., 15., 15., 15., 15. /),shape(dcp_test))
mss_test = reshape((/ 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1 /),shape(mss_test))
t2_test = 273.15 + reshape((/ 27., 27., 27., 27., 27., 27., 27., 27., 27., 27., 27., 27., 27., 27., 27. /),shape(t2_test))
P_test = reshape((/ 1000., 1000., 1000., 1000., 1000., 1000., 1000., 1000., 1000., 1000., 1000., 1000., 1000., 1000., 1000. /),&
                 shape(P_test))
q2_test = reshape((/ 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01 /),shape(q2_test))
tsk_test = 273.15 + reshape((/ 29., 29.5, 30., 30.5, 31., 31.5, 32., 32.5, 33., 33.5, 34., 34.5, 35., 35.5, 36. /),shape(tsk_test))
SSGFname_test = 'F09v13'

! Call function to calculate spray heat fluxes
CALL spray_heatflux_nom_PSDv13(sourcestrength_test,eps_test,wspd10_test,ustar_test,swh_test,&
                               dcp_test,mss_test,SSGFname_test,t2_test,&
                               P_test,q2_test,tsk_test,SHF_spray_nom_test,LHF_spray_nom_test)

! Write spray heat flux output to screen
print *, 'Nominal Spray Sensible Heat Flux [W m-2]:'
CALL print_array(SHF_spray_nom_test)
print *,''
print *, 'Nominal Spray Latent Heat Flux [W m-2]:'
CALL print_array(LHF_spray_nom_test)

ENDPROGRAM TEST_physics_spray
!=========================================================================

SUBROUTINE print_array(x)
!--------------------------------
! Prints array x in matrix format.
!--------------------------------

REAL,DIMENSION(:,:),INTENT(IN) :: x    ! Input array
INTEGER :: i,j
INTEGER,DIMENSION(2) :: dims

dims = shape(x)
do i = 1,dims(1)
    write(*,*) (x(i,j),j=1,dims(2))
enddo

ENDSUBROUTINE print_array
