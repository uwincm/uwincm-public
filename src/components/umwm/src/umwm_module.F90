MODULE UMWM_module
!=======================================================================
!
! DESCRIPTION: A global definitions module and main memory pool.
!
!=======================================================================
USE datetime_module,ONLY:datetime

IMPLICIT NONE

CHARACTER(LEN=5),PARAMETER :: version = '2.0.0'

! Use blocking MPI routines?
LOGICAL,PARAMETER :: mpiIsBlocking = .FALSE.

! Time objects
TYPE(datetime) :: startTime,stopTime,currentTime

CHARACTER(LEN=19) :: startTimeStr_nml,stopTimeStr_nml

! Domain dimensions
INTEGER :: mm  ! Domain size in x
INTEGER :: nm  ! Domain size in y
INTEGER :: om  ! Number of frequency/wavenumber bins
INTEGER :: pm  ! Number of direction bins

INTEGER :: im  ! Unrolled domain length (sea-points only)
INTEGER :: imm ! Unrolled domain length (all, equals mm*nm)

INTEGER :: istart,iend   ! Tile exclusive range (no halo)
INTEGER :: iistart,iiend ! Tile computational range (exclusive+halo)

INTEGER :: nproc ! Process rank
INTEGER :: size  ! MPI pool size
INTEGER :: ierr  ! MPI error return status

! Lengths of leftmost and rightmost columns
INTEGER :: first_col_len,last_col_len 

! stdout related variables
INTEGER :: iip,nproc_plot,xpl,ypl

! Time stepping
INTEGER :: step,timeStep

! .TRUE. during first time step
LOGICAL :: first,firstdtg

! Main control switches
LOGICAL :: isGlobal,restart

! Grid and bathymetry related switches
LOGICAL :: gridFromFile,topoFromFile,fillLakes,fillEstuaries

! Output related switches
INTEGER :: outgrid,outspec,outrst
LOGICAL :: stokes

! Time steps:
REAL :: dta,dtr,dtamin 

! Miscellaneous variables
REAL :: bf1,bf1a,bf2
REAL :: cgmax,cfllim
REAL :: delx,dely
REAL :: dpt,dlnf,dmin,dtg,dts,dth,dthg
REAL :: explim
REAL :: fmin,fmax,fprog
REAL :: fieldscale1,fieldscale2
REAL :: g,gustiness
REAL :: inv_sds_power
REAL :: kappa
REAL :: log10overz
REAL :: mindelx,mss_fac
REAL :: nu_air,nu_water
REAL :: oneovdth
REAL :: rhoa0,rhow0
REAL :: sbf_fac,sbp_fac,sds_fac,sds_power,sdt_fac,sfct,sin_diss1
REAL :: sin_diss2,sin_fac,snl_fac,sumt
REAL :: temp0,twopisds_fac,twonu
REAL :: wspd0,wdir0,uc0,vc0,z

!=======================================================================

! Global constants
REAL,PARAMETER :: pi     = 3.141592653589793 ! pi
REAL,PARAMETER :: invpi  = 1/pi              ! 1/pi
REAL,PARAMETER :: twopi  = 2*pi              ! 2*pi
REAL,PARAMETER :: twopi2 = twopi**2          ! 4*pi^2
REAL,PARAMETER :: dr     = pi/180.           ! deg -> rad

INTEGER,DIMENSION(10),PARAMETER :: &
allowedOutputTimes = [-1,0,1,2,3,4,6,8,12,24]

!=======================================================================

! Allocatable arrays

! 1-dimensional arrays:

! Neighbor grid indices, aliased 
INTEGER,DIMENSION(:),ALLOCATABLE :: iw,ie,is,in 

! Neighbor grid indices, true
INTEGER,DIMENSION(:),ALLOCATABLE :: iiw,iie,iis,iin 

! Exchange indices for periodic BC
INTEGER,DIMENSION(:),ALLOCATABLE :: i_exchange_indices

! Indices m and n as functions of i
INTEGER,DIMENSION(:),ALLOCATABLE :: mi,ni

! Cut-off frequency index (maximum prognostic)
INTEGER,DIMENSION(:),ALLOCATABLE :: oc

! Directional indices, anti-clockwise and clockwise
INTEGER,DIMENSION(:),ALLOCATABLE :: pl,pr

REAL,DIMENSION(:),ALLOCATABLE :: th,cth,sth
REAL,DIMENSION(:),ALLOCATABLE :: cth2
REAL,DIMENSION(:),ALLOCATABLE :: dom
REAL,DIMENSION(:),ALLOCATABLE :: f

! Wave ray directions with grid curvature correction:
REAL,DIMENSION(:,:),ALLOCATABLE :: cth_curv,sth_curv

INTEGER,DIMENSION(:,:),ALLOCATABLE :: ii
INTEGER,DIMENSION(:,:),ALLOCATABLE :: mask
INTEGER,DIMENSION(:,:),ALLOCATABLE :: nproc_out

! 2-dimensional, unrolled arrays:
REAL,DIMENSION(:,:),ALLOCATABLE :: ar_2d
REAL,DIMENSION(:,:),ALLOCATABLE :: curv
REAL,DIMENSION(:,:),ALLOCATABLE :: d_2d,dlon,dlat,dx_2d,dy_2d
REAL,DIMENSION(:,:),ALLOCATABLE :: gustu,gustv
REAL,DIMENSION(:,:),ALLOCATABLE :: lat,lon
REAL,DIMENSION(:,:),ALLOCATABLE :: x,y
REAL,DIMENSION(:,:),ALLOCATABLE :: rhoa_2d,rhow_2d
REAL,DIMENSION(:,:),ALLOCATABLE :: wspd_2d,wdir_2d
REAL,DIMENSION(:,:),ALLOCATABLE :: uwb,vwb,uw,vw,uwf,vwf
REAL,DIMENSION(:,:),ALLOCATABLE :: ucb,uc_2d,ucf,vcb,vc_2d,vcf

REAL,DIMENSION(:),ALLOCATABLE :: ar,cd,d,dx,dy,dwd,dwl,dwp,fcutoff,mwf,pwf
REAL,DIMENSION(:),ALLOCATABLE :: dxn,dxs,dyw,dye
REAL,DIMENSION(:),ALLOCATABLE :: dcp0,dcp,dcg0,dcg
REAL,DIMENSION(:),ALLOCATABLE :: ht,mss,mwd,mwl,mwp,shelt
REAL,DIMENSION(:),ALLOCATABLE :: oneovar,oneovdx,oneovdy

REAL,DIMENSION(:),ALLOCATABLE :: momx,momy ! Momentum in x- and y-direction
REAL,DIMENSION(:),ALLOCATABLE :: cgmxx,cgmxy,cgmyy ! Horizontal momentum fluxes 

! Air and water density:
REAL,DIMENSION(:),ALLOCATABLE :: rhoab,rhoa,rhoaf
REAL,DIMENSION(:),ALLOCATABLE :: rhowb,rhow,rhowf
REAL,DIMENSION(:),ALLOCATABLE :: rhorat

! Stability function
REAL,DIMENSION(:),ALLOCATABLE :: psim

! Stress (momentum flux) arrays [N/m^2]
REAL,DIMENSION(:),ALLOCATABLE :: taux,tauy
REAL,DIMENSION(:),ALLOCATABLE :: taux_form,tauy_form
REAL,DIMENSION(:),ALLOCATABLE :: taux_skin,tauy_skin
REAL,DIMENSION(:),ALLOCATABLE :: taux_diag,tauy_diag
REAL,DIMENSION(:),ALLOCATABLE :: taux_ocntop,tauy_ocntop
REAL,DIMENSION(:),ALLOCATABLE :: taux_ocnbot,tauy_ocnbot
REAL,DIMENSION(:),ALLOCATABLE :: taux_snl,tauy_snl

! Wave energy growth flux [kg/s^3]
REAL,DIMENSION(:),ALLOCATABLE :: epsx_atm, epsy_atm

! Wave energy dissipation flux [kg/s^3]
REAL,DIMENSION(:),ALLOCATABLE :: epsx_ocn, epsy_ocn

! Form drag components:
REAL,DIMENSION(:),ALLOCATABLE :: taux1,tauy1
REAL,DIMENSION(:),ALLOCATABLE :: taux2,tauy2
REAL,DIMENSION(:),ALLOCATABLE :: taux3,tauy3

! Tail stress components:
REAL,DIMENSION(:),ALLOCATABLE :: tailatmx,tailatmy
REAL,DIMENSION(:),ALLOCATABLE :: tailocnx,tailocny

REAL,DIMENSION(:),ALLOCATABLE :: uc,vc,ustar
REAL,DIMENSION(:),ALLOCATABLE :: wspd,wdir

! Snl downshifting weights, used in snl routine:
REAL,DIMENSION(:,:),ALLOCATABLE :: bf1_renorm,bf2_renorm

! Utility array used for MSS in sds routine:
REAL,DIMENSION(:,:),ALLOCATABLE :: cth2pp

! Group and phase velocities:
REAL,DIMENSION(:,:),ALLOCATABLE :: cg0,cp0

REAL,DIMENSION(:,:),ALLOCATABLE :: cothkd
REAL,DIMENSION(:,:),ALLOCATABLE :: dwn,invcp0
REAL,DIMENSION(:,:),ALLOCATABLE :: fkovg 
REAL,DIMENSION(:,:),ALLOCATABLE :: k,k4,kdk,k3dk
REAL,DIMENSION(:,:),ALLOCATABLE :: l2,logl2overz,oneoverk4,psiml2
REAL,DIMENSION(:,:),ALLOCATABLE :: sbf,sdv,sdt,snl_arg

REAL,DIMENSION(:,:,:),ALLOCATABLE :: dummy,e,ef,rotl,rotr,sds,snl,ssin

!=======================================================================
ENDMODULE UMWM_module
