#define FILENAME "UMWM_constants.F90"
MODULE UMWM_constants

USE iso_fortran_env,ONLY:REAL32,REAL64

IMPLICIT NONE

INTEGER,PARAMETER :: kind_r4 = KIND(1e0)
INTEGER,PARAMETER :: kind_r8 = KIND(1d0)

#ifdef REAL64
INTEGER,PARAMETER :: KIND_RX = kind_r8
#else
INTEGER,PARAMETER :: KIND_RX = kind_r4
#endif

REAL(KIND=KIND_RX),PARAMETER :: one   = 1d0
REAL(KIND=KIND_RX),PARAMETER :: two   = 2d0
REAL(KIND=KIND_RX),PARAMETER :: three = 3d0

REAL(KIND=KIND_RX),PARAMETER :: half     = 0.5d0
REAL(KIND=KIND_RX),PARAMETER :: quart    = 0.25d0
REAL(KIND=KIND_RX),PARAMETER :: eightinv = 0.125d0
REAL(KIND=KIND_RX),PARAMETER :: onethird = one/three

REAL(KIND=KIND_RX),PARAMETER :: pi       = 3.141592653589793 ! pi
REAL(KIND=KIND_RX),PARAMETER :: euler    = EXP(1d0)          ! e 
REAL(KIND=KIND_RX),PARAMETER :: eulerinv = 1/euler           ! 1/e

REAL(KIND=KIND_RX),PARAMETER :: invpi  = one/pi   ! 1/pi
REAL(KIND=KIND_RX),PARAMETER :: twopi  = two*pi   ! 2*pi
REAL(KIND=KIND_RX),PARAMETER :: twopi2 = twopi**2 ! 4*pi^2
REAL(KIND=KIND_RX),PARAMETER :: dr     = pi/180   ! deg -> rad

REAL(KIND=KIND_RX),PARAMETER :: R_Earth = 6.371009E6 ! Earth radius

REAL(KIND=KIND_RX),PARAMETER :: tiny_real = TINY(one)
REAL(KIND=KIND_RX),PARAMETER :: huge_real = HUGE(one)

INTEGER,PARAMETER :: stdout = 6
INTEGER,PARAMETER :: stderr = 0

ENDMODULE UMWM_constants
