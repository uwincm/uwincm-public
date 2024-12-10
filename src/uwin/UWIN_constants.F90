#define FILENAME "UWIN_constants.F90"
MODULE UWIN_constants
USE ESMF

IMPLICIT NONE

INTEGER,PARAMETER :: sp = KIND(1E0)
INTEGER,PARAMETER :: dp = KIND(1D0)

! Universal constants:

! Stefan-Boltzmann constant
REAL(KIND=ESMF_KIND_R8),PARAMETER :: sigma = 5.67037321d-8

! Pi-related constants
REAL(KIND=ESMF_KIND_R8),PARAMETER :: pi      = 3.141592653589793_dp
REAL(KIND=ESMF_KIND_R8),PARAMETER :: twopi   = 2d0*pi
REAL(KIND=ESMF_KIND_R8),PARAMETER :: d2r     = pi/180d0
REAL(KIND=ESMF_KIND_R8),PARAMETER :: grav    = 9.8d0
REAL(KIND=ESMF_KIND_R8),PARAMETER :: invgrav = 1/grav
REAL(KIND=ESMF_KIND_R8),PARAMETER :: Rd      = 287_dp
REAL(KIND=ESMF_KIND_R8),PARAMETER :: Rv      = 461_dp
REAL(KIND=ESMF_KIND_R8),PARAMETER :: ep1     = Rv/Rd-1. ! constant for virtual temp.

! Radius of Earth:
REAL(KIND=ESMF_KIND_R8),PARAMETER :: R_earth = 6.371d6

ENDMODULE UWIN_constants
