MODULE UMWM_forcing
!======================================================================!
USE umwm_module
USE UMWM_io,ONLY:input_nc,readFile,winds,currents,air_density,&
                 water_density

IMPLICIT NONE
!======================================================================!
CONTAINS



SUBROUTINE forcingInput(timeStr)
!======================================================================!
!                                                                      !
! DESCRIPTION: Reads atmospheric and oceanic input forcing fields      !
!                                                                      !
! LAST UPDATE: 2010-09-03 ~mc                                          !
!                                                                      !
!======================================================================!

! ARGUMENTS:
CHARACTER(LEN=19),INTENT(IN) :: timeStr

!======================================================================!

! Save forcing fields at time level n:
IF(winds)THEN
  uwb = uwf
  vwb = vwf
ENDIF

IF(currents)THEN
  ucb = ucf
  vcb = vcf
ENDIF

IF(air_density)THEN
  rhoab = rhoaf
ENDIF

IF(water_density)THEN
  rhowb = rhowf
ENDIF

! Load input fields at time level n+1:
IF(readFile)THEN
  CALL input_nc(timeStr)
ENDIF

ENDSUBROUTINE forcingInput
!======================================================================!



SUBROUTINE forcingInterpolate
!======================================================================!
!                                                                      !
! DESCRIPTION: Interpolates in time atmospheric and oceanic input      ! 
!              forcing fields                                          !
!                                                                      !
! LAST UPDATE: 2012-02-05 ~mc                                          !
!                                                                      !
!======================================================================!
USE UMWM_util,ONLY:remap_mn2i

REAL :: invdtg
!======================================================================!

invdtg = 1./dtg

IF(winds)THEN

  uw = uwb*(1-sumt*invdtg)+uwf*sumt*invdtg
  vw = vwb*(1-sumt*invdtg)+vwf*sumt*invdtg

  ! Add wind gustiness if desired:
  IF(gustiness > 0)THEN

    ! Get random numbers [0,1]:
    CALL RANDOM_NUMBER(gustu)
    CALL RANDOM_NUMBER(gustv)

    ! Make gustu and gustv random numbers in the range of [-0.1,0.1]:
    gustu = gustiness*(2*gustu-1)
    gustv = gustiness*(2*gustv-1)

    ! Add gustiness to the wind fields:
    uw = uw*(1+gustu)
    vw = vw*(1+gustv)

  ENDIF 

  wspd_2d = SQRT(uw*uw+vw*vw)
  wdir_2d = ATAN2(vw,uw)

  ! Remap to 1-D arrays:
  wspd = remap_mn2i(wspd_2d)
  wdir = remap_mn2i(wdir_2d)

ENDIF ! winds

IF(currents)THEN

  uc_2d = ucb*(1-sumt*invdtg)+ucf*sumt*invdtg
  vc_2d = vcb*(1-sumt*invdtg)+vcf*sumt*invdtg

  ! Remap to 1-D arrays:
  uc = remap_mn2i(uc_2d)
  vc = remap_mn2i(vc_2d)

ENDIF
  
IF(air_density)THEN
  rhoa = rhoab*(1-sumt*invdtg)+rhoaf*sumt*invdtg
ENDIF

IF(water_density)THEN
  rhow = rhowb*(1-sumt*invdtg)+rhowf*sumt*invdtg
ENDIF

rhorat = rhoa/rhow

ENDSUBROUTINE forcingInterpolate
!======================================================================!
ENDMODULE UMWM_forcing
