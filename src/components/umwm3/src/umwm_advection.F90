MODULE UMWM_advection
!======================================================================!
!                                                                      !
! DESCRIPTION: Provides advection routines (propagation and            !
!              refraction) for the wave model                          !
!                                                                      !
! CONTAINS: propagation                                                !
!           refraction                                                 !
!                                                                      !
!======================================================================!
USE UMWM_io,ONLY:currents
!======================================================================!
IMPLICIT NONE

LOGICAL :: zeroCurrents

!======================================================================!
CONTAINS



SUBROUTINE propagation
!======================================================================!
!                                                                      !
! DESCRIPTION: Computes the wave energy advection flux (wave propaga-  !
!              tion) by first-order upstream finite differencing.      !
!              This version is for regular or curvilinear rectangular  !
!              grids.                                                  !
!                                                                      !
!======================================================================!
USE umwm_module
USE umwm_constants,ONLY:eightinv
!======================================================================!
IMPLICIT NONE

INTEGER :: o,p,i

REAL,DIMENSION(om) :: cge,cgw,cgn,cgs

REAL,DIMENSION(istart:iend) :: feup,fedn,fwup,fwdn
REAL,DIMENSION(istart:iend) :: fnup,fndn,fsup,fsdn

!======================================================================!

dummy = 0. 

DO i=istart,iend
  DO p=1,pm
    DO o=1,oc(i)

      ! East, up:
      dummy(o,p,i) = dummy(o,p,i)+(cg0(o,i)*cth_curv(p,i)+cg0(o,ie(i))*cth_curv(p,i)  &
                              +ABS(cg0(o,i)*cth_curv(p,i)+cg0(o,ie(i))*cth_curv(p,i)))&
                                 *dye(i)*e(o,p,i)

      ! East, down:
      dummy(o,p,i) = dummy(o,p,i)+(cg0(o,i)*cth_curv(p,i)+cg0(o,ie(i))*cth_curv(p,i)  &
                              -ABS(cg0(o,i)*cth_curv(p,i)+cg0(o,ie(i))*cth_curv(p,i)))&
                                 *dye(i)*e(o,p,ie(i))

      ! West, up:
      dummy(o,p,i) = dummy(o,p,i)-(cg0(o,i)*cth_curv(p,i)+cg0(o,iw(i))*cth_curv(p,i)  &
                              +ABS(cg0(o,i)*cth_curv(p,i)+cg0(o,iw(i))*cth_curv(p,i)))&
                                 *dyw(i)*e(o,p,iw(i))

      ! West, down:
      dummy(o,p,i) = dummy(o,p,i)-(cg0(o,i)*cth_curv(p,i)+cg0(o,iw(i))*cth_curv(p,i)  &
                              -ABS(cg0(o,i)*cth_curv(p,i)+cg0(o,iw(i))*cth_curv(p,i)))&
                                 *dyw(i)*e(o,p,i)

      ! North, up:
      dummy(o,p,i) = dummy(o,p,i)+(cg0(o,i)*sth_curv(p,i)+cg0(o,in(i))*sth_curv(p,i)  &
                              +ABS(cg0(o,i)*sth_curv(p,i)+cg0(o,in(i))*sth_curv(p,i)))&
                                 *dxn(i)*e(o,p,i)

      ! North, down:
      dummy(o,p,i) = dummy(o,p,i)+(cg0(o,i)*sth_curv(p,i)+cg0(o,in(i))*sth_curv(p,i)  &
                              -ABS(cg0(o,i)*sth_curv(p,i)+cg0(o,in(i))*sth_curv(p,i)))&
                                 *dxn(i)*e(o,p,in(i))

      ! South, up:
      dummy(o,p,i) = dummy(o,p,i)-(cg0(o,i)*sth_curv(p,i)+cg0(o,is(i))*sth_curv(p,i)  &
                              +ABS(cg0(o,i)*sth_curv(p,i)+cg0(o,is(i))*sth_curv(p,i)))&
                                 *dxs(i)*e(o,p,is(i))

      ! South, down:
      dummy(o,p,i) = dummy(o,p,i)-(cg0(o,i)*sth_curv(p,i)+cg0(o,is(i))*sth_curv(p,i)  &
                              -ABS(cg0(o,i)*sth_curv(p,i)+cg0(o,is(i))*sth_curv(p,i)))&
                                 *dxs(i)*e(o,p,i)

    ENDDO
  ENDDO
ENDDO

! Check if currents are non-zero:
IF(.NOT.isGlobal)THEN
  zeroCurrents = .NOT. (ANY(uc(iistart:iiend) /= 0)&
                   .OR. ANY(vc(iistart:iiend) /= 0))
ELSE
  zeroCurrents = .NOT. (ANY(uc /= 0) .OR. ANY(vc /= 0))
ENDIF

IF(.NOT.zeroCurrents)THEN ! Advect wave energy by currents

  DO i=istart,iend

    ! x-direction:
    feup(i) = (uc(i)+uc(iie(i))+ABS(uc(i)+uc(iie(i))))*dye(i)
    fedn(i) = (uc(i)+uc(iie(i))-ABS(uc(i)+uc(iie(i))))*dye(i)
    fwup(i) = (uc(i)+uc(iiw(i))+ABS(uc(i)+uc(iiw(i))))*dyw(i)
    fwdn(i) = (uc(i)+uc(iiw(i))-ABS(uc(i)+uc(iiw(i))))*dyw(i)

    ! y-direction:
    fnup(i) = (vc(i)+vc(iin(i))+ABS(vc(i)+vc(iin(i))))*dxn(i)
    fndn(i) = (vc(i)+vc(iin(i))-ABS(vc(i)+vc(iin(i))))*dxn(i)
    fsup(i) = (vc(i)+vc(iis(i))+ABS(vc(i)+vc(iis(i))))*dxs(i)
    fsdn(i) = (vc(i)+vc(iis(i))-ABS(vc(i)+vc(iis(i))))*dxs(i)

  ENDDO

  DO i=istart,iend
    DO p=1,pm
      DO o=1,oc(i)
        dummy(o,p,i) = dummy(o,p,i)                         &
                     +(feup(i)*e(o,p,i)+fedn(i)*e(o,p,ie(i))&
                      -fwup(i)*e(o,p,iw(i))-fwdn(i)*e(o,p,i)&
                      +fnup(i)*e(o,p,i)+fndn(i)*e(o,p,in(i))&
                      -fsup(i)*e(o,p,is(i))-fsdn(i)*e(o,p,i))
      ENDDO
    ENDDO
  ENDDO

ENDIF

! Integrate in time:
DO i=istart,iend
  DO p=1,pm
    DO o=1,oc(i)
      ef(o,p,i) = ef(o,p,i)-0.25*dta*dummy(o,p,i)*oneovar(i)
    ENDDO
  ENDDO
ENDDO

ENDSUBROUTINE propagation
!======================================================================!



SUBROUTINE refraction
!======================================================================!
!                                                                      !
! DESCRIPTION: Computes advection in directional space (refraction)    !
!              using first order upstream differencing                 !
!                                                                      !
!======================================================================!
#ifdef MPI
  USE mpi
#endif
USE umwm_module
USE umwm_constants,ONLY:half
!======================================================================!
IMPLICIT NONE

INTEGER :: i,o,p

LOGICAL :: computeRotTend

REAL      :: sendBuffer
REAL,SAVE :: dtr_temp

!======================================================================!

#ifdef ESMF
! Always compute in coupled mode:
computeRotTend = .TRUE.
#else
! Compute if varrying currents or first step:
computeRotTend = currents.OR.first
#endif

IF(computeRotTend)THEN

  ! Compute rotation:
  dummy = 0.
  DO i=istart,iend
    DO p=1,pm
      DO o=1,oc(i)
        dummy(o,p,i) = half*(((cp0(o,ie(i))-cp0(o,iw(i)))*sth(p)             &
                                           +vc(iie(i))-vc(iiw(i)))*oneovdx(i)&
                            -((cp0(o,in(i))-cp0(o,is(i)))*cth(p)             & 
                                           +uc(iin(i))-uc(iis(i)))*oneovdy(i))
      ENDDO
    ENDDO
  ENDDO

  ! Evaluate rotation at cell edges:
  DO i=istart,iend
    DO p=1,pm
      DO o=1,oc(i)
        rotl(o,p,i) = half*(dummy(o,p,i)+dummy(o,pl(p),i))
        rotr(o,p,i) = half*(dummy(o,p,i)+dummy(o,pr(p),i))
      ENDDO
    ENDDO
  ENDDO

  IF(MAXVAL(dummy) <= TINY(sendBuffer))THEN
    dtr_temp = dts
  ELSE
#ifdef MPI
    sendBuffer = dth/MAXVAL(dummy)
    CALL MPI_Allreduce(sendBuffer,dtr_temp,1,MPI_REAL,MPI_MIN,mpi_comm_world,ierr)
#else
    dtr_temp = dth/MAXVAL(dummy)
#endif
  ENDIF

ENDIF

dtr = MIN(dtr_temp,dts)

dummy = 0. 
DO i=istart,iend
  DO p=1,pm
    DO o=1,oc(i)

      ! Compute tendencies by upstream differencing:
      dummy(o,p,i) = half*((rotl(o,p,i)+ABS(rotl(o,p,i)))*e(o,p,i)    &
                          +(rotl(o,p,i)-ABS(rotl(o,p,i)))*e(o,pl(p),i)&
                          -(rotr(o,p,i)+ABS(rotr(o,p,i)))*e(o,pr(p),i)&
                          -(rotr(o,p,i)-ABS(rotr(o,p,i)))*e(o,p,i))   &
                         *oneovdth
      ! Integrate: 
      ef(o,p,i) = ef(o,p,i)-dtr*dummy(o,p,i)

    ENDDO
  ENDDO
ENDDO

ENDSUBROUTINE refraction
!======================================================================!
ENDMODULE UMWM_advection
