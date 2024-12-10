module umwm_source_functions

use iso_fortran_env, only: rk => real32

use umwm_module
use umwm_constants, only: one, onethird, two, tiny_real

implicit none

private

public :: sin_dccm12

contains


  pure elemental real(kind=rk) function sheltering_coefficient(&
    wspd, x1, x2, x3, y1, y2, y3, intercept, curvature, decay) result(res)

    !! Returns the sheltering coefficient given input wind speed,
    !! and a number of parameters that determine the shape of the
    !! function. The function is linear for wspd < x1,
    !! inverse quadratic for x1 < wspd < x2, and inverse
    !! exponential for wspd > x2.

    real(kind=rk), intent(in) :: wspd
      !! wind speed [m/s]
    real(kind=rk), intent(in) :: x1
      !! linear -> quadratic transition
    real(kind=rk), intent(in) :: x2
      !! quadratic -> exponential decay transition
    real(kind=rk), intent(in) :: x3
      !! end reference point
    real(kind=rk), intent(in) :: y1
      !! sheltering value at x1
    real(kind=rk), intent(in) :: y2
      !! sheltering value at x2
    real(kind=rk), intent(in) :: y3
      !! sheltering value at x3
    real(kind=rk), intent(in) :: intercept
      !! sheltering value at zero wind speed
    real(kind=rk), intent(in) :: curvature
      !! how fast is the quadratic decay
    real(kind=rk), intent(in) :: decay
      !! inverse rate of exponential decay (lower is faster decay)
    
    real(kind=rk) :: a, b, c, s1, slope1, slope2

    ! low range linear growth
    slope1 = (y1 - intercept) / x1 
    slope2 = (y3 - y2) / (x3 - x2)

    ! medium range fitting (quadratic)
    c = curvature * (slope2 - slope1) / (x2 - x1)
    b = slope1 - 2 * c * x1
    a = y1 - b * x1 - c * x1**2

    ! high end decay
    s1 = a + b * x2 + c * x2**2

    if (wspd <= x1) then
      res = intercept + slope1 * wspd
    else if (wspd > x1 .and. wspd <= x2) then
      res = a + b * wspd + c * wspd**2
    else
      res = s1 * exp(- (wspd - x2) / (decay * wspd))
    end if

  end function sheltering_coefficient


  subroutine sin_dccm12 

    use umwm_io, only: currents

    integer :: i, m, n, o, p

    real(kind=rk) :: a, b, c, s1, slope1, slope2

    ! wspd-dependent sheltering coefficient (sin_fac) parameters
    real, parameter :: x1 = 15   ! linear -> quadratic transition
    real, parameter :: x2 = 33   ! quadratic -> exponential decay transition
    real, parameter :: x3 = 60   ! end reference point
    real, parameter :: y1 = 0.10 ! A1 value where linear relationship ends
    real, parameter :: y2 = 0.09
    real, parameter :: y3 = 0.06 ! A1 value at wspd = x3
    real, parameter :: intercept = 0.04  
    real, parameter :: curvature = 0.65
    real, parameter :: decay = 1.6  ! lower is faster decay

    ! Protection against low wind speed values:
    wspd = max(wspd, 1e-2)

    ! Cut-off frequency (4*Pierson-Moskowitz peak frequency):
    fcutoff(istart:iend) = 0.53 * g / wspd(istart:iend)

    where(fcutoff > fprog) fcutoff = fprog

    ! Search for the cut-off frequency bin:    
    do i = istart, iend
      do o = om-2, 1, -1
        oc(i) = o
        if (fcutoff(i) > f(o)) exit
      end do
    end do

    ! Compute wind input at height of half wavelenght:
    forall (o = 1:om,p = 1:pm, i = istart:iend)& 
#ifdef ESMF
    ssin(o,p,i) = (wspd(i) + 2.5 * ustar(i) * (logl2overz(o,i) + psim(i) - psiml2(o,i)))&
#else
    ssin(o,p,i) = (wspd(i) + 2.5 * ustar(i) * logl2overz(o,i))&
#endif
                         *cos(wdir(i) - th(p)) - cp0(o,i)&
                 - uc(i) * cth(p) - vc(i) * sth(p)
  
    ssin = sin_fac * abs(ssin) * ssin

    ! Compute variable sheltering coefficient
    shelt = sheltering_coefficient(wspd(istart:iend),&
      x1, x2, x3, y1, y2, y3, intercept, curvature, decay)

    forall(o = 1:om, p = 1:pm, i = istart:iend, ssin(o,p,i) > 0)&
    ssin(o,p,i) = ssin(o,p,i) * shelt(i) / sin_fac

    ! Adjust input for opposing winds:
    forall(o = 1:om, p = 1:pm, i = istart:iend, ssin(o,p,i) < 0)&
    ssin(o,p,i) = ssin(o,p,i)*fieldscale1

    ! Further reduce for swell that overruns the wind:
    forall(o = 1:om, p = 1:pm, i = istart:iend, ssin(o,p,i) < 0 .and. cos(wdir(i)-th(p)) > 0)&
    ssin(o,p,i) = ssin(o,p,i) * fieldscale2

    forall(o = 1:om, p = 1:pm, i = istart:iend)&
    ssin(o,p,i) = twopi * rhorat(i) * ssin(o,p,i) * fkovg(o,i)

    ! Prevent negative Sin for diagnostic tail:
    do i = istart, iend
      do p = 1,pm
        do o = oc(i)+1, om
          if (ssin(o,p,i) < 0) ssin(o,p,i) = 0
        enddo
      enddo
    enddo

  end subroutine sin_dccm12

end module umwm_source_functions
