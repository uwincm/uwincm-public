MODULE UMWM_physics
!======================================================================!
!                                                                      !
! CONTAINS: s_in                                                       !
!           s_ds                                                       !
!           s_nl                                                       !
!           stress                                                     !
!           source                                                     !
!           diag                                                       !
!                                                                      !
!======================================================================!
USE umwm_module
USE umwm_constants,ONLY:one,onethird,two,tiny_real
USE umwm_util, ONLY:maxHeight,sigWaveHeight

IMPLICIT NONE
!======================================================================!
CONTAINS



SUBROUTINE s_in
!======================================================================!
!                                                                      !
! DESCRIPTION: Computes the wind input function Sin                    !
!                                                                      !
!======================================================================!
USE UMWM_io,ONLY:currents
!======================================================================!

INTEGER :: i,m,n,o,p

REAL :: a,b,c,s1,slope1,slope2

! wspd-dependent sheltering coefficient (sin_fac) parameters
REAL,PARAMETER :: x1 = 15   ! linear -> quadratic transition
REAL,PARAMETER :: x2 = 33   ! quadratic -> exponential decay transition
REAL,PARAMETER :: x3 = 60   ! end reference point
REAL,PARAMETER :: y1 = 0.10 ! A1 value where linear relationship ends
REAL,PARAMETER :: y2 = 0.09
REAL,PARAMETER :: y3 = 0.06 ! A1 value at wspd = x3
REAL,PARAMETER :: intercept = 0.04
REAL,PARAMETER :: curvature = 0.65
REAL,PARAMETER :: decay_rate = 1.6  ! lower is faster decay

! Variable sheltering coefficient switch
LOGICAL,PARAMETER :: variable_sin_fac = .TRUE.

!======================================================================!

! Protection against low wind speed values:
wspd = MAX(wspd,1E-2)

! Cut-off frequency (4*Pierson-Moskowitz peak frequency):
fcutoff(istart:iend) = 0.53*g/wspd(istart:iend)

WHERE(fcutoff>fprog)fcutoff = fprog

! Search for the cut-off frequency bin:
DO i=istart,iend
  DO o=om-2,1,-1
    oc(i) = o
    IF(fcutoff(i)>f(o))EXIT
  ENDDO
ENDDO

! Compute wind input at height of half wavelenght:
FORALL(o=1:om,p=1:pm,i=istart:iend)&
#ifdef ESMF
ssin(o,p,i) = (wspd(i)+2.5*ustar(i)*(logl2overz(o,i)+psim(i)-psiml2(o,i)))&
#else
ssin(o,p,i) = (wspd(i)+2.5*ustar(i)*logl2overz(o,i))&
#endif
                     *COS(wdir(i)-th(p))-cp0(o,i)&
             -uc(i)*cth(p)-vc(i)*sth(p)

ssin = sin_fac*ABS(ssin)*ssin

!======================================================================
! Compute variable sheltering coefficient

! Low range linear growth
slope1 = (y1-intercept)/x1
slope2 = (y3-y2)/(x3-x2)

! Medium range fitting (quadratic)
c = curvature*(slope2-slope1)/(x2-x1)
b = slope1-2*c*x1
a = y1-b*x1-c*x1**2

! High end decay
s1 = a+b*x2+c*x2**2

DO i = istart,iend
  IF(wspd(i) <= x1)THEN
    shelt(i) = intercept+slope1*wspd(i)
  ELSEIF(wspd(i) > x1 .AND. wspd(i) <= x2)THEN
    shelt(i) = a+b*wspd(i)+c*wspd(i)**2
  ELSE
    shelt(i) = s1*EXP(-(wspd(i)-x2)/(decay_rate*wspd(i)))
  ENDIF
ENDDO

IF(variable_sin_fac)THEN
  FORALL(o=1:om,p=1:pm,i=istart:iend,ssin(o,p,i) > 0)&
  ssin(o,p,i) = ssin(o,p,i)*shelt(i)/sin_fac
ENDIF

!=======================================================================

! Adjust input for opposing winds:
FORALL(o=1:om,p=1:pm,i=istart:iend,ssin(o,p,i) < 0)&
ssin(o,p,i) = ssin(o,p,i)*fieldscale1

! Further reduce for swell that overruns the wind:
FORALL(o=1:om,p=1:pm,i=istart:iend,ssin(o,p,i) < 0 .AND. COS(wdir(i)-th(p)) > 0)&
ssin(o,p,i) = ssin(o,p,i)*fieldscale2

FORALL(o=1:om,p=1:pm,i=istart:iend)&
ssin(o,p,i) = twopi*rhorat(i)*ssin(o,p,i)*fkovg(o,i)

! Prevent negative Sin for diagnostic tail:
DO i=istart,iend
  DO p=1,pm
    DO o=oc(i)+1,om
      IF(ssin(o,p,i) < 0)ssin(o,p,i) = 0
    ENDDO
  ENDDO
ENDDO

ENDSUBROUTINE s_in
!======================================================================!



SUBROUTINE s_ds
!======================================================================!
!                                                                      !
! DESCRIPTION: Computes the wave dissipation source term               !
!                                                                      !
!======================================================================!

INTEGER :: i,o,p

!======================================================================!

if(hmax_limiter)then
!apply wave breaking limiter
!TODO: find another way to stablish the conditions in the first if
!      and further test the coefficient value (0.125)
  do i=istart,iend
!    if(slim(i)<60.and.slim(i)>tiny(1.0).and.d(i)<30)then
!      e(:,:,i) = e(:,:,i) * 0.125
!    endif 
    if(ht(i)>maxHeight(i).and.d(i)<30)then
      e(:,:,i) = e(:,:,i) * maxHeight(i)/ht(i)
    end if
  end do
end if


dummy = 0

IF(mss_fac > 0)THEN

  DO i=istart,iend
    DO p=1,pm
      DO o=2,om
        dummy(o,p,i) = dummy(o-1,p,i)+SUM(e(o-1,:,i)*cth2pp(:,p))*k3dk(o-1,i)
      ENDDO
    ENDDO
  ENDDO

ENDIF

dummy = (1+mss_fac*dummy)**2

DO i=istart,iend
  DO p=1,pm
    DO o=1,om
      sds(o,p,i) = twopisds_fac*f(o)&
                 *dummy(o,p,i)*abs(e(o,p,i)*k4(o,i))**sds_power
    ENDDO
  ENDDO
ENDDO

ENDSUBROUTINE s_ds
!======================================================================!



SUBROUTINE s_nl
!======================================================================!
!                                                                      !
! DESCRIPTION: Computes non-linear wave-wave interaction source term   !
!                                                                      !
!======================================================================!

INTEGER :: o,p,i

!======================================================================!

snl = 0. ! Initialize

! Spread wave energy to 2 next longer wavenumbers exponentially decaying
! as distance from donating wavenumber, and remove the energy from
! donating wavenumbers:
DO i=istart,iend
  DO p=1,pm
    DO o=1,oc(i)
      snl(o,p,i) = bf1_renorm(o,i)*sds(o+1,p,i)*e(o+1,p,i)&
                  +bf2_renorm(o,i)*sds(o+2,p,i)*e(o+2,p,i)&
                  -snl_fac*sds(o,p,i)*e(o,p,i)
    ENDDO
  ENDDO
ENDDO

! Account for plunging breakers:
FORALL(o=1:om,p=1:pm,i=istart:iend)&
sds(o,p,i) = sds(o,p,i)*cothkd(o,i)

! Compute dissipation due to turbulence:
FORALL(o=1:om,i=istart:iend)&
sdt(o,i) = sdt_fac*SQRT(rhorat(i))*ustar(i)*k(o,i)

ENDSUBROUTINE s_nl
!======================================================================!



SUBROUTINE stress(option)
!======================================================================!
!                                                                      !
! DESCRIPTION: Computes wind stress and drag coefficient as well as    !
!              momentum fluxes into ocean top and bottom               !
!                                                                      !
!======================================================================!
USE UMWM_advection,ONLY:zeroCurrents
USE UMWM_stokes,   ONLY:util
!======================================================================!

CHARACTER(LEN=3),INTENT(IN) :: option

INTEGER :: i,m,n,o,p,l

REAL,DIMENSION(om,istart:iend) :: taux_util,tauy_util
REAL,DIMENSION(istart:iend)    :: kmax_pow_tail,tail

! x- and y-components of surface Stokes drift velocities
REAL,DIMENSION(istart:iend) :: usurf,vsurf

! Wind components, speed and direction relative to surface velocity
REAL,DIMENSION(istart:iend) :: uRel,vRel
REAL,DIMENSION(istart:iend) :: wspdRel,wdirRel

REAL :: kend = 1E3 ! ~6.28 mm wavelength

! Roughness length
REAL :: z0 = 1E-3

! Small number
REAL :: small = TINY(ustar)

!======================================================================!

! First evaluate wind speed dependent tail:
tail = 0.000112*wspd(istart:iend)**2.-0.01451*wspd(istart:iend)-1.0186
kmax_pow_tail = k(om,istart:iend)**tail

tail = (kend**(tail+1)-k(om,istart:iend)*kmax_pow_tail)&
      /(kmax_pow_tail*(tail+1))

IF(option=='OCN')THEN

! Compute stress into ocean top: (positive into ocean)
taux_util = 0.
tauy_util = 0.

DO i=istart,iend
  DO p=1,pm

    ! Dissipation into currents:
    DO o=1,om

      taux_util(o,i) = taux_util(o,i)                         &
                      +e(o,p,i)*(sds(o,p,i)+sdt(o,i)+sdv(o,i))&
                               *cth(p)*invcp0(o,i)

      tauy_util(o,i) = tauy_util(o,i)                         &
                      +e(o,p,i)*(sds(o,p,i)+sdt(o,i)+sdv(o,i))&
                               *sth(p)*invcp0(o,i)

    ENDDO

    ! Correction for the Snl in diagnostic part:
    DO o=3,om

      taux_util(o,i) = taux_util(o,i)-snl(o,p,i)   &
                      *(bf1*cp0(o,i)*invcp0(o-1,i) &
                       +bf2*cp0(o,i)*invcp0(o-2,i))&
                      *cth(p)*invcp0(o,i)

      tauy_util(o,i) = tauy_util(o,i)-snl(o,p,i)   &
                      *(bf1*cp0(o,i)*invcp0(o-1,i) &
                       +bf2*cp0(o,i)*invcp0(o-2,i))&
                      *sth(p)*invcp0(o,i)

    ENDDO

  ENDDO
ENDDO

! Compute the tail:
DO i=istart,iend
  tailocnx(i) = taux_util(om,i)*k(om,i)*tail(i)*rhow(i)*dthg
  tailocny(i) = tauy_util(om,i)*k(om,i)*tail(i)*rhow(i)*dthg
ENDDO

DO i=istart,iend

  taux_ocntop(i) = SUM(taux_util(:,i)*kdk(:,i),DIM=1)*rhow(i)*dthg&
                  +tailocnx(i)+taux_skin(i)

  tauy_ocntop(i) = SUM(tauy_util(:,i)*kdk(:,i),DIM=1)*rhow(i)*dthg&
                  +tailocny(i)+tauy_skin(i)

ENDDO

! Compute stress into ocean bottom: (positive into ocean)
taux_util = 0.
tauy_util = 0.

DO i=istart,iend
  DO p=1,pm
    DO o=1,om
      taux_util(o,i) = taux_util(o,i)+e(o,p,i)*sbf(o,i)*cth(p)*invcp0(o,i)
      tauy_util(o,i) = tauy_util(o,i)+e(o,p,i)*sbf(o,i)*sth(p)*invcp0(o,i)
    ENDDO
  ENDDO
ENDDO

DO i=istart,iend
  taux_ocnbot(i) = SUM(taux_util(:,i)*kdk(:,i),DIM=1)*rhow(i)*dthg
  tauy_ocnbot(i) = SUM(tauy_util(:,i)*kdk(:,i),DIM=1)*rhow(i)*dthg
ENDDO

! Snl conserves energy, but not momentum. The following term is
! the momentum loss due to non-linear downshifting of energy.
taux_util = 0.
tauy_util = 0.

DO i=istart,iend
  DO p=1,pm
    DO o=3,oc(i)

      taux_util(o,i) = taux_util(o,i)+snl(o,p,i)       &
                      *(bf1*(1-cp0(o,i)*invcp0(o-1,i)) &
                       +bf2*(1-cp0(o,i)*invcp0(o-2,i)))&
                      *cth(p)*invcp0(o,i)

      tauy_util(o,i) = tauy_util(o,i)+snl(o,p,i)       &
                      *(bf1*(1-cp0(o,i)*invcp0(o-1,i)) &
                       +bf2*(1-cp0(o,i)*invcp0(o-2,i)))&
                      *sth(p)*invcp0(o,i)

    ENDDO
  ENDDO
ENDDO

DO i=istart,iend
  taux_snl(i) = SUM(taux_util(:,i)*kdk(:,i),DIM=1)*rhow(i)*dthg
  tauy_snl(i) = SUM(tauy_util(:,i)*kdk(:,i),DIM=1)*rhow(i)*dthg
ENDDO

! Compute energy flux into ocean:
taux_util = 0.
tauy_util = 0.

DO i = istart, iend
  DO p = 1, pm

    DO o = 1, om
      taux_util(o,i) = taux_util(o,i) + e(o,p,i)*sds(o,p,i)*cth(p)
      tauy_util(o,i) = tauy_util(o,i) + e(o,p,i)*sds(o,p,i)*sth(p)
    ENDDO

    DO o =3, om

      taux_util(o,i) = taux_util(o,i)-snl(o,p,i)   &
                      *(bf1*cp0(o,i)*invcp0(o-1,i) &
                       +bf2*cp0(o,i)*invcp0(o-2,i))&
                      *cth(p)

      tauy_util(o,i) = tauy_util(o,i)-snl(o,p,i)   &
                      *(bf1*cp0(o,i)*invcp0(o-1,i) &
                       +bf2*cp0(o,i)*invcp0(o-2,i))&
                      *sth(p)

    ENDDO

  ENDDO
ENDDO

DO i = istart,iend
  epsx_ocn(i) = SUM(taux_util(:,i)*kdk(:,i),DIM=1)*rhow(i)*dthg
  epsy_ocn(i) = SUM(tauy_util(:,i)*kdk(:,i),DIM=1)*rhow(i)*dthg
ENDDO

! Compute energy flux from air:
taux_util = 0.
tauy_util = 0.

DO i = istart, iend
  DO p = 1, pm
    DO o = 1, om
      taux_util(o,i) = taux_util(o,i) + e(o,p,i)*ssin(o,p,i)*cth(p)
      tauy_util(o,i) = tauy_util(o,i) + e(o,p,i)*ssin(o,p,i)*sth(p)
    ENDDO
  ENDDO
ENDDO

DO i = istart,iend
  epsx_atm(i) = SUM(taux_util(:,i)*kdk(:,i),DIM=1)*rhow(i)*dthg
  epsy_atm(i) = SUM(tauy_util(:,i)*kdk(:,i),DIM=1)*rhow(i)*dthg
ENDDO

! This part calculates the components of form drag.
! It has nothing to do with momentum fluxes into ocean,
! but we do it here because it is needed only for output.

taux1 = 0
tauy1 = 0
taux2 = 0
tauy2 = 0
taux3 = 0
tauy3 = 0

DO i = istart,iend
  DO p = 1,pm
    DO o = 1,om

      dummy(o,p,i) = e(o,p,i)*ssin(o,p,i)*invcp0(o,i)*kdk(o,i)

      IF(ssin(o,p,i) > 0)THEN ! Positive stress

        ! Wind pushing waves
        taux1(i) = taux1(i)+dummy(o,p,i)*cth(p)
        tauy1(i) = tauy1(i)+dummy(o,p,i)*sth(p)

      ELSE ! Negative stress, two cases

        IF(COS(wdir(i)-th(p)) < 0)THEN

          ! Waves against wind
          taux2(i) = taux2(i)+dummy(o,p,i)*cth(p)
          tauy2(i) = tauy2(i)+dummy(o,p,i)*sth(p)

        ELSE

          ! Waves overrunning wind
          taux3(i) = taux3(i)+dummy(o,p,i)*cth(p)
          tauy3(i) = tauy3(i)+dummy(o,p,i)*sth(p)

        ENDIF

      ENDIF

    ENDDO
  ENDDO
ENDDO

taux1 = taux1*rhow*dthg
tauy1 = tauy1*rhow*dthg
taux2 = taux2*rhow*dthg
tauy2 = tauy2*rhow*dthg
taux3 = taux3*rhow*dthg
tauy3 = tauy3*rhow*dthg

ENDIF ! IF(option=='OCN')

!======================================================================!

IF(option=='ATM')THEN

! Compute form drag from atmosphere: (positive into waves)
taux_util = 0.
tauy_util = 0.

DO i=istart,iend
  DO p=1,pm
    DO o=1,om
      taux_util(o,i) = taux_util(o,i)+e(o,p,i)*ssin(o,p,i)*cth(p)*invcp0(o,i)
      tauy_util(o,i) = tauy_util(o,i)+e(o,p,i)*ssin(o,p,i)*sth(p)*invcp0(o,i)
    ENDDO
  ENDDO
ENDDO

! Compute the tail:
DO i=istart,iend
  tailatmx(i) = taux_util(om,i)*k(om,i)*tail(i)*rhow(i)*dthg
  tailatmy(i) = tauy_util(om,i)*k(om,i)*tail(i)*rhow(i)*dthg
ENDDO

DO i=istart,iend

  taux_form(i) = SUM(taux_util(:,i)*kdk(:,i),DIM=1)*rhow(i)*dthg&
                +tailatmx(i)

  tauy_form(i) = SUM(tauy_util(:,i)*kdk(:,i),DIM=1)*rhow(i)*dthg&
                +tailatmy(i)

  taux_diag(i) = SUM(taux_util(oc(i):om,i)*kdk(oc(i):om,i),DIM=1)*rhow(i)*dthg
  tauy_diag(i) = SUM(tauy_util(oc(i):om,i)*kdk(oc(i):om,i),DIM=1)*rhow(i)*dthg

ENDDO

!======================================================================!

! Compute surface Stokes drift velocities:
usurf = 0.
vsurf = 0.

DO i = istart,iend
  DO p = 1,pm
    DO o = 1,om
      usurf(i) = usurf(i)+util(o,i,1)*e(o,p,i)*cth(p)
      vsurf(i) = vsurf(i)+util(o,i,1)*e(o,p,i)*sth(p)
    ENDDO
  ENDDO
ENDDO

DO i = istart,iend
  urel(i) = wspd(i)*COS(wdir(i))-uc(i)-usurf(i)
  vrel(i) = wspd(i)*SIN(wdir(i))-vc(i)-vsurf(i)
ENDDO

! Wind speed and direction relative to surface velocity
wspdrel = SQRT(urel**2+vrel**2)
wdirrel = ATAN2(vrel,urel)

! Form-induced drag coefficient
DO i = istart,iend
  cd(i) = SQRT(taux_form(i)**2+tauy_form(i)**2)&
         /(rhoa(i)*wspd(i)**2)
ENDDO

! Friction velocity over a smooth surface
! Use wind speed relative to water surface
DO i = istart,iend
  DO l = 1,6
    ustar(i) = kappa*wspdrel(i)/LOG(z/z0)
    z0 = 0.132*nu_air/ustar(i)
  ENDDO
ENDDO

! Modulate skin drag due to form drag and add
DO i = istart,iend

  ustar(i) = (ustar(i)/wspd(i))**2
  ustar(i) = onethird*wspd(i)**2*ustar(i)&
            *(one+two*ustar(i)/(ustar(i)+cd(i)+small))

  taux_skin(i) = rhoa(i)*ustar(i)*COS(wdirrel(i))
  tauy_skin(i) = rhoa(i)*ustar(i)*SIN(wdirrel(i))

  taux(i) = taux_form(i)+taux_skin(i)
  tauy(i) = tauy_form(i)+tauy_skin(i)

  cd(i) = SQRT(taux(i)**2+tauy(i)**2)/(rhoa(i)*wspd(i)**2)

ENDDO

! Limit Cd
WHERE(cd > 5E-3)cd = 5E-3

! Update friction velocity (form+skin)
DO i = istart,iend
  ustar(i) = SQRT(cd(i))*wspd(i)
ENDDO

ENDIF ! IF(option=='ATM')

ENDSUBROUTINE stress
!======================================================================!



SUBROUTINE source
!======================================================================!
!                                                                      !
! DESCRIPTION: Integrates source functions Sin and Sds as an exact     !
!              exponential; source time step dts is limited to limit   !
!              growth of wave spectrum by a factor of 2; Snl is        !
!              integrated using forward Euler differencing             !
!                                                                      !
!======================================================================!
#ifdef MPI
USE mpi
#endif
!======================================================================!

INTEGER :: i,o,p
REAL    :: maxarg,send_buff,aux1

!REAL,SAVE :: explim_ramp

!======================================================================!

! Calculate the exponential argument:
ef = 0
DO i = istart,iend
  DO p = 1,pm
    DO o = 1,oc(i)
      ef(o,p,i) = ssin(o,p,i)-sds(o,p,i)*snl_arg(o,i)&
                 -sbf(o,i)-sdt(o,i)-sdv(o,i)
    ENDDO
  ENDDO
  slim(i) = explim/MAXVAL(ABS(ef(:,:,i)))
ENDDO

!explim_ramp = explim

! If first global step, apply ramp to explim
!IF(firstdtg)THEN
!  explim_ramp = (0.01+0.99*sumt/dtg)*explim
!ENDIF

! Compute the dynamic time step and update in-step model time:
maxarg = MAXVAL(ABS(ef))
!dts = MIN(explim_ramp/maxarg,dtamin,dtg-sumt)
dts = MIN(explim/maxarg,dtamin,dtg-sumt)

! Dynamic time step may get very small if sds >> ssin
! due to the wavebreaking associated with shoaling
! (coth in Donelan 2012) - if the imbalance is too
! big, dts gets tiny (and may tend to 0)
! Only a few points in the realistic case show this
! kind of behavior and we can ignore them by relaxing
! the time steps associated with maxarg
if(dt_relax)THEN
  aux1 = explim/maxarg
  if(aux1<60.and.dts<60.and.dts>=52)then
      dts = min(1.2/maxarg,dtamin,dtg-sumt)
  elseif(aux1<60.and.dts<52.and.dts>=42)then
      dts = min(1.5/maxarg,dtamin,dtg-sumt)
  elseif(aux1<60.and.dts<42.and.dts>=32.and..not.firstdtg)then
      dts = min(2/maxarg,dtamin,dtg-sumt)
  elseif(aux1<60.and.dts<32.and..not.firstdtg)then
      dts = min(3/maxarg,dtamin,dtg-sumt)
  endif
endif


#ifdef MPI
send_buff = dts
CALL MPI_Allreduce(send_buff,dts,1,MPI_REAL,MPI_MIN,mpi_comm_world,ierr)
#endif

! If first time step, set time step to zero and
! integrate only the diagnostic part
IF(.NOT. restart .AND. first)THEN
  dts = 0
ENDIF

dta = dts

! Increment time:
sumt = sumt+dts

! Integrate source terms for the prognostic range (o <= ol)
DO i = istart,iend
  DO p = 1,pm
    DO o = 1,oc(i)
      ef(o,p,i) = e(o,p,i)*EXP(dts*(ssin(o,p,i)-sds(o,p,i)      &
                                   -sbf(o,i)-sdt(o,i)-sdv(o,i)))&
                 +dts*snl(o,p,i)
    ENDDO
  ENDDO
ENDDO

! Integrate source terms for the diagnostic range (o > ol)
DO i = istart,iend
  DO p = 1,pm
    DO o = oc(i)+1,om
      IF(ssin(o,p,i)-sdt(o,i)-sdv(o,i) >= 0)THEN
        ef(o,p,i) = oneoverk4(o,i)*((ssin(o,p,i)-sdt(o,i)-sdv(o,i))   &
                  / (twopisds_fac*f(o)*dummy(o,p,i)*cothkd(o,i)))**inv_sds_power
      ENDIF
    ENDDO
  ENDDO
ENDDO


e(:,:,istart:iend) = 0.5*(e(:,:,istart:iend)+ef(:,:,istart:iend))



ENDSUBROUTINE source
!======================================================================!



SUBROUTINE diag
!======================================================================!
!                                                                      !
! DESCRIPTION: Compute diagnostic mean quantities                      !
!                                                                      !
!======================================================================!

INTEGER              :: o,p,i
INTEGER              :: opeak,ppeak
INTEGER,DIMENSION(2) :: spectrum_peak_loc

REAL                        :: mag,xcomp,ycomp
REAL,DIMENSION(istart:iend) :: m0,m2
REAL,DIMENSION(om,pm)       :: spectrumBin

REAL :: ekdkovcp

!======================================================================!

m0 = 0
m2 = 0

DO i=istart,iend
  DO p=1,pm
    DO o=1,om
      m0(i) = m0(i)+e(o,p,i)*kdk(o,i)
      m2(i) = m2(i)+f(o)**2*e(o,p,i)*kdk(o,i)
    ENDDO
  ENDDO
ENDDO

mwp = SQRT(m0/(m2+tiny_real)) ! Mean wave period

momx = 0
momy = 0
cgmxx = 0
cgmxy = 0
cgmyy = 0

DO i=istart,iend

  ! Total wave momentum:
  DO p=1,pm
    DO o=1,oc(i)

      ekdkovcp = e(o,p,i)*kdk(o,i)*invcp0(o,i)

      momx(i) = momx(i)+ekdkovcp*cth(p)
      momy(i) = momy(i)+ekdkovcp*sth(p)

      cgmxx(i) = cgmxx(i)+cg0(o,i)*ekdkovcp*cth(p)**2
      cgmxy(i) = cgmxy(i)+cg0(o,i)*ekdkovcp*cth(p)*sth(p)
      cgmyy(i) = cgmyy(i)+cg0(o,i)*ekdkovcp*sth(p)**2

    ENDDO
  ENDDO

  momx(i)  = momx(i)*rhow(i)*dthg
  momy(i)  = momy(i)*rhow(i)*dthg
  cgmxx(i) = cgmxx(i)*rhow(i)*dthg
  cgmxy(i) = cgmxy(i)*rhow(i)*dthg
  cgmyy(i) = cgmyy(i)*rhow(i)*dthg

  ! Significant wave height:
  ht(i) = 0.
  DO p=1,pm
    DO o=1,om
      ht(i) = ht(i)+e(o,p,i)*kdk(o,i)
    ENDDO
  ENDDO
  ht(i) = 4*SQRT(ht(i)*dth)

  ! Mean wave direction:
  xcomp = 0.
  ycomp = 0.
  DO p=1,pm
    mag   = SUM(e(:,p,i)*kdk(:,i),DIM=1)
    xcomp = xcomp+mag*cth(p)
    ycomp = ycomp+mag*sth(p)
  ENDDO
  mwd(i) = ATAN2(ycomp,xcomp)

  ! Wavenumber spectrum moments:
  m0(i) = 0.
  m2(i) = 0.
  DO p=1,pm
    DO o=1,om
      m0(i) = m0(i)+e(o,p,i)*kdk(o,i)
      m2(i) = m2(i)+e(o,p,i)*k3dk(o,i)
    ENDDO
  ENDDO

  ! Mean-squared slope:
  mss(i) = m2(i)*dth

  mwl(i) = twopi*SQRT(m0(i)/(m2(i)+tiny_real)) ! Mean wavelenght

  DO p=1,pm
    DO o=1,om
      spectrumBin(o,p) = e(o,p,i)*kdk(o,i)
    ENDDO
  ENDDO

  spectrum_peak_loc = MAXLOC(spectrumBin)  ! Indices of spectrum peak

  opeak = spectrum_peak_loc(1)             ! Frequency/wavenumber peak
  ppeak = spectrum_peak_loc(2)             ! Direction peak

  dwd(i) = th(ppeak)                       ! Dominant wave direction
  dwl(i) = twopi/k(opeak,i)                ! Dominant wave length
  dwp(i) = 1./f(opeak)                     ! Dominant wave period

  dcp0(i) = cp0(opeak,i)                   ! Dominant phase speed, intrinsic
  dcg0(i) = cg0(opeak,i)                   ! Dominant group speed, intrinsic

  ! Dominant phase and group speed:
  dcp(i) = dcp0(i)+uc(i)*cth(ppeak)+vc(i)*sth(ppeak)
  dcg(i) = dcg0(i)+uc(i)*cth(ppeak)+vc(i)*sth(ppeak)

ENDDO

ENDSUBROUTINE diag
!======================================================================!
ENDMODULE UMWM_physics
