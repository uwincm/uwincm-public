MODULE UMWM_stokes
!======================================================================!
IMPLICIT NONE

INTEGER :: lm

REAL,DIMENSION(:),    ALLOCATABLE :: depth,ds
REAL,DIMENSION(:,:),  ALLOCATABLE :: us,vs,usmag
REAL,DIMENSION(:,:,:),ALLOCATABLE :: util,arg

REAL,DIMENSION(100) :: depths = -1.

!======================================================================!
CONTAINS



SUBROUTINE stokes_drift(option)
!======================================================================!
!                                                                      !
! DESCRIPTION: Computes wave-induced Stokes drift                      !
!                                                                      !
!======================================================================!

USE UMWM_constants,ONLY:eulerinv
USE UMWM_module,   ONLY:twopi,d,e,f,k,dwn,dth,istart,iend,om,pm,cth,&
                        sth,nproc

!======================================================================!

CHARACTER(LEN=4),INTENT(IN),OPTIONAL :: option

INTEGER :: i,l,o,p
INTEGER,DIMENSION(2) :: coords

REAL :: ust_efolding
REAL,DIMENSION(:,:),ALLOCATABLE :: kd

NAMELIST /STOKES/ depths

!======================================================================!

IF(PRESENT(option))THEN
  IF(option=='INIT')THEN

    ! Read depth levels from namelist:
    OPEN(UNIT=21,FILE='namelists/main.nml',STATUS='OLD',&
         FORM='FORMATTED',ACCESS='SEQUENTIAL',ACTION='READ')
    READ(UNIT=21,NML=STOKES)
    CLOSE(UNIT=21)

    ! Get size of depth array:
    lm = COUNT(depths >= 0.)
    ALLOCATE(depth(lm))
    depth = -depths(1:lm)

    ! Allocate Stokes velocities and utility array:
    ALLOCATE(us(istart:iend,lm))
    ALLOCATE(vs(istart:iend,lm))
    ALLOCATE(usmag(istart:iend,lm))
    ALLOCATE(ds(istart:iend))
    ALLOCATE(util(om,istart:iend,lm))
    ALLOCATE(arg(om,istart:iend,lm))
    ALLOCATE(kd(om,istart:iend))

    FORALL(o=1:om,i=istart:iend)kd(o,i) = k(o,i)*d(i)

    ! Compute exponent:
    DO l=1,lm
      DO i=istart,iend
        DO o=1,om

          arg(o,i,l) = 2*k(o,i)*(depth(l)+d(i))

          IF(ABS(arg(o,i,l))>50.OR.kd(o,i)>50)THEN
            ! Hyperbolic trig. functions would overflow;
            ! Use deep water approximation instead:
            util(o,i,l) = twopi*f(o)*2*k(o,i)**2&
                         *EXP(2*k(o,i)*depth(l))*dwn(o,i)*dth
          ELSE
            ! First order approximation for arbitrary depth:
            util(o,i,l) = twopi*f(o)*k(o,i)**2&
                         *COSH(2*k(o,i)*(depth(l)+d(i)))&
                         /SINH(kd(o,i))**2*dwn(o,i)*dth
          ENDIF

        ENDDO
          
        IF(ABS(depth(l))>d(i))util(:,i,l) = 0.

      ENDDO
    ENDDO

    DEALLOCATE(arg,kd)

    IF(nproc==0)WRITE(*,FMT=101)'umwm: stokes_drift: Initialized'

  ENDIF
ENDIF
    
us = 0
vs = 0
ds = 0

! Stokes velocities
DO l = 1,lm
  DO i = istart,iend
    DO p = 1,pm
      DO o = 1,om
        us(i,l) = us(i,l)+util(o,i,l)*e(o,p,i)*cth(p)
        vs(i,l) = vs(i,l)+util(o,i,l)*e(o,p,i)*sth(p)
      ENDDO
    ENDDO
  ENDDO
ENDDO

! Stokes drift magnitude
usmag = SQRT(us**2+vs**2)

! Stokes e-folding depth
DO i = istart,iend

  IF(usmag(i,1) == 0)THEN
    ds(i) = 0
    CONTINUE
  ENDIF

  ust_efolding = usmag(i,1)*eulerinv

  DEPTH_LOOP: DO l = 2,lm
    IF(usmag(i,l) < ust_efolding)THEN
      ds(i) = (ABS(usmag(i,l-1)-ust_efolding)*depth(l)   &
              +ABS(usmag(i,l  )-ust_efolding)*depth(l-1))&
              /(usmag(i,l-1)-usmag(i,l))

      EXIT DEPTH_LOOP
    ENDIF
  ENDDO DEPTH_LOOP

ENDDO

ds = -ds

101 FORMAT(A)

ENDSUBROUTINE stokes_drift
!======================================================================!
ENDMODULE UMWM_stokes
