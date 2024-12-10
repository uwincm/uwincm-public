MODULE UMWM_util
!======================================================================!
!                                                                      !
! DESCRIPTION: A module with utility functions.                        !
!                                                                      !
! CONTAINS: remap_i2mn     - Remaps an (i) indexed array to (m,n)      !
!           remap_mn2i     - Remaps an (m,n) indexed array to (i)      !
!           sigWaveHeight  - Calculates sig. wave height               !
!           meanWavePeriod - Calculates mean wave period               !
!           raiseException - Raises an exception and prints a message  !
!           dealloc        - Array deallocation routine                !
!                                                                      !
!======================================================================!
IMPLICIT NONE
!======================================================================!
CONTAINS



PURE FUNCTION remap_i2mn(field_i) RESULT(field_mn)
!======================================================================+
!                                                                      !
! Remaps an (i) indexed array to (m,n)                                 !
!                                                                      !
!======================================================================+
USE UMWM_module,ONLY:imm,mm,nm,ii

! ARGUMENTS
REAL,DIMENSION(imm),INTENT(IN) :: field_i
REAL,DIMENSION(mm,nm)          :: field_mn

INTEGER :: m,n
!=======================================================================

DO n=1,nm
  DO m=1,mm
    field_mn(m,n) = field_i(ii(m,n))
  ENDDO
ENDDO

ENDFUNCTION remap_i2mn
!======================================================================>



PURE FUNCTION remap_mn2i(field_mn) RESULT(field_i)
!======================================================================+
!                                                                      !
! Remaps an (m,n) indexed array to (i)                                 !
!                                                                      !
!======================================================================+
USE UMWM_module,ONLY:imm,mm,nm,ii

REAL,DIMENSION(mm,nm),INTENT(IN) :: field_mn
REAL,DIMENSION(imm)              :: field_i
INTEGER :: m,n

DO n=1,nm
  DO m=1,mm
    field_i(ii(m,n)) = field_mn(m,n)
  ENDDO
ENDDO

ENDFUNCTION remap_mn2i
!======================================================================>



PURE FUNCTION sigWaveHeight(i) RESULT(swh)
!======================================================================+
!                                                                      !
! Given a spatial grid index i, returns significant wave height        !
! at that location.                                                    !
!                                                                      !
!======================================================================+
USE UMWM_module,ONLY:e,kdk,dth,om,pm

! ARGUMENTS:
INTEGER,INTENT(IN) :: i

INTEGER :: o,p
REAL    :: swh

!=======================================================================

swh = 0
DO p=1,pm
  DO o=1,om
    swh = swh+e(o,p,i)*kdk(o,i)
  ENDDO
ENDDO
swh = 4.*SQRT(swh*dth)

ENDFUNCTION sigWaveHeight
!======================================================================+



PURE FUNCTION meanWavePeriod(i) RESULT(mwp)
!======================================================================+
!                                                                      !
! Given a spatial grid index i, returns mean wave period at that       !
! location.                                                            !
!                                                                      !
!======================================================================+
USE UMWM_module,ONLY:e,f,kdk,om,pm

! ARGUMENTS:
INTEGER,INTENT(IN) :: i

INTEGER :: o,p
REAL    :: m0,m2,mwp

m0 = 0
m2 = 0
DO p=1,pm
  DO o=1,om
    m0 = m0+e(o,p,i)*kdk(o,i)
    m2 = m2+f(o)**2*e(o,p,i)*kdk(o,i)
  ENDDO
ENDDO
mwp = SQRT(m0/m2)

ENDFUNCTION meanWavePeriod
!======================================================================+



SUBROUTINE raiseException(exceptionType,routineName,message,flag)
!======================================================================+
!                                                                      !
! Raises an exception, prints a message to stdout and sets the flag    !
! to .FALSE. if present in argument list                               !
!                                                                      !
!======================================================================+

! ARGUMENTS:
CHARACTER(LEN=*),INTENT(IN)    :: exceptionType
CHARACTER(LEN=*),INTENT(IN)    :: routineName
CHARACTER(LEN=*),INTENT(IN)    :: message
LOGICAL,INTENT(INOUT),OPTIONAL :: flag

!======================================================================+

IF(PRESENT(flag))THEN
  flag = .FALSE.
ENDIF

WRITE(UNIT=*,FMT='(A)')'umwm: '//TRIM(routineName)//': '  &
                               //TRIM(exceptionType)//': '&
                               //TRIM(message)

ENDSUBROUTINE raiseException
!======================================================================+



SUBROUTINE dealloc
!======================================================================+
!                                                                      !
! Deallocates UMWM arrays                                              !
!                                                                      !
!======================================================================>
USE UMWM_module
!======================================================================>

DEALLOCATE(ar_2d,d_2d,dlon,dlat,dx_2d,dy_2d)
DEALLOCATE(curv)
DEALLOCATE(gustu,gustv)
DEALLOCATE(lat,lon)
DEALLOCATE(x,y)
DEALLOCATE(rhoa_2d,rhow_2d)
DEALLOCATE(wspd_2d,wdir_2d)
DEALLOCATE(uwb,vwb,uw,vw,uwf,vwf,ucb,uc_2d,ucf,vcb,vc_2d,vcf)
DEALLOCATE(dom,f,cth,cth2,sth,th)
DEALLOCATE(cth_curv,sth_curv)
DEALLOCATE(ar,cd,d,dx,dy,dwd,dwl,dwp,fcutoff)
DEALLOCATE(dxn,dxs,dye,dyw)
DEALLOCATE(dcp0,dcg0,dcp,dcg)
DEALLOCATE(ht,mss,mwd,mwl,mwp)
DEALLOCATE(momx,momy)
DEALLOCATE(cgmxx,cgmxy,cgmyy)
DEALLOCATE(oneovar,oneovdx,oneovdy)
DEALLOCATE(psim,psiml2)
DEALLOCATE(rhoab,rhoa,rhoaf,rhowb,rhow,rhowf,rhorat)
DEALLOCATE(taux,tauy,taux_form,tauy_form,taux_skin,tauy_skin)
DEALLOCATE(taux_ocntop,tauy_ocntop,taux_ocnbot,tauy_ocnbot)
DEALLOCATE(taux_diag,tauy_diag)
DEALLOCATE(taux_snl,tauy_snl)
DEALLOCATE(taux1,tauy1,taux2,tauy2,taux3,tauy3)
DEALLOCATE(tailatmx,tailatmy)
DEALLOCATE(tailocnx,tailocny)
DEALLOCATE(epsx_atm, epsy_atm, epsx_ocn, epsy_ocn)
DEALLOCATE(uc,vc,ustar)
DEALLOCATE(wspd,wdir)
DEALLOCATE(shelt)
DEALLOCATE(bf1_renorm,bf2_renorm)
DEALLOCATE(cg0,cp0,cothkd)
DEALLOCATE(dwn,invcp0)
DEALLOCATE(fkovg)
DEALLOCATE(k,k4,kdk,k3dk,l2,logl2overz,oneoverk4)
DEALLOCATE(sbf,sdv,sdt,snl_arg,dummy,e,ef,rotl,rotr,sds,snl,ssin)

ENDSUBROUTINE dealloc
!======================================================================!
ENDMODULE UMWM_util
