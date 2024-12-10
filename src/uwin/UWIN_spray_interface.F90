MODULE UWIN_spray_interface
!==============================================================================
USE ESMF
USE UWIN_global
USE COAMPS_Util,ONLY:FieldRemapStore,FieldRemap
!==============================================================================
IMPLICIT NONE
PRIVATE
PUBLIC :: SprayMedHeatFluxes
!==============================================================================
CONTAINS

SUBROUTINE SprayMedHeatFluxes(rc)
!-------------------------------------
! Calculate spray-mediated heat fluxes.  This subroutine extracts fields from the
! XG and then calls spraymedHF to perform spray flux calculations on the arrays.
! The results passed to WRF are the modifications to heat fluxes (dHS1spr, dHL1spr), 
! which are added to bulk heat fluxes calculated by WRF in module_first_rk_step_part1,
! and other fields that are not used by WRF but are useful diagnostics.
!-------------------------------------

USE UWIN_spray_physics,ONLY:spraymedHF
USE UWIN_ExchangeGrid,ONLY:xg

INTEGER,INTENT(OUT),OPTIONAL :: rc
INTEGER :: i,j,nSpray
REAL(KIND=ESMF_KIND_R4),DIMENSION(isx:iex,jsx:jex) :: z_1_XG,U_1_XG,th_1_XG,&
        q_1_XG,p_0_XG,eps_XG,dcp_XG,swh_XG,mss_XG,ustar_XG,t_0_XG
REAL(KIND=ESMF_KIND_R4),DIMENSION(isx:iex,jsx:jex) :: dHS1spr_XG,dHL1spr_XG,&
        HTspr_XG,HSspr_XG,HRspr_XG,HLspr_XG,alpha_S_XG,beta_S_XG,beta_L_XG,&
        gamma_H_XG,delt_spr_XG,delq_spr_XG,Mspr_XG
REAL(KIND=ESMF_KIND_R4),DIMENSION(isx:iex,jsx:jex) :: u10_XG,v10_XG,wspd10_XG
REAL(KIND=ESMF_KIND_R4),DIMENSION(:),ALLOCATABLE :: z_1,U_1,th_1,&
        q_1,p_0,eps,dcp,swh,mss,ustar,t_0
REAL(KIND=ESMF_KIND_R4),DIMENSION(:),ALLOCATABLE :: dHS1spr,dHL1spr,&
        HTspr,HSspr,HRspr,HLspr,alpha_S,beta_S,beta_L,&
        gamma_H,delt_spr,delq_spr,Mspr
LOGICAL,DIMENSION(isx:iex,jsx:jex) :: calcSpray

TYPE(ESMF_RouteHandle),SAVE :: routeHandle
LOGICAL,SAVE :: firstTimeSpray = .TRUE.

IF(firstTimeSpray)THEN

    ! Get route handle for XG -> ATM
    CALL FieldRemapStore(srcField   = xg % auxField2D(1),    &
                         dstField   = gc(1) % auxField2D(1), &
                         remapRH    = routeHandle,           &
                         vm         = vm,                    &
                         remapType  = 'bilinr',              &
                         rc         = rc)
    IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

    firstTimeSpray = .FALSE.

ENDIF

! Import fields: xg % expFieldPtr(n,tgt,src)
z_1_XG(isx:iex,jsx:jex)   = xg % expFieldPtr( 6,2,1) % Ptr(isx:iex,jsx:jex)
U_1_XG(isx:iex,jsx:jex)   = xg % expFieldPtr( 7,2,1) % Ptr(isx:iex,jsx:jex)
th_1_XG(isx:iex,jsx:jex)  = xg % expFieldPtr( 8,2,1) % Ptr(isx:iex,jsx:jex)
q_1_XG(isx:iex,jsx:jex)   = xg % expFieldPtr( 9,2,1) % Ptr(isx:iex,jsx:jex)
p_0_XG(isx:iex,jsx:jex)   = xg % expFieldPtr(10,2,1) % Ptr(isx:iex,jsx:jex)
IF((modelIsEnabled(3)) .AND. (sstFromOcean))THEN    ! If ocean and SST feedback are enabled, get SST from ocean
    t_0_XG(isx:iex,jsx:jex) = xg % expFieldPtr( 1,1,3) % Ptr(isx:iex,jsx:jex) + 300.15    ! [C -> K] + 27
ELSE    ! If ocean or SST feedback are not enabled, get SST from atmosphere
    t_0_XG(isx:iex,jsx:jex) = xg % expFieldPtr(11,2,1) % Ptr(isx:iex,jsx:jex)
ENDIF
eps_XG(isx:iex,jsx:jex)   = xg % expFieldPtr( 5,1,2) % Ptr(isx:iex,jsx:jex)
ustar_XG(isx:iex,jsx:jex) = xg % expFieldPtr( 6,1,2) % Ptr(isx:iex,jsx:jex)
swh_XG(isx:iex,jsx:jex)   = xg % expFieldPtr( 7,1,2) % Ptr(isx:iex,jsx:jex)
dcp_XG(isx:iex,jsx:jex)   = xg % expFieldPtr( 8,1,2) % Ptr(isx:iex,jsx:jex)
mss_XG(isx:iex,jsx:jex)   = xg % expFieldPtr( 9,1,2) % Ptr(isx:iex,jsx:jex)
u10_XG(isx:iex,jsx:jex)   = xg % expFieldPtr( 1,2,1) % Ptr(isx:iex,jsx:jex)
v10_XG(isx:iex,jsx:jex)   = xg % expFieldPtr( 2,2,1) % Ptr(isx:iex,jsx:jex)

! Initialize result arrays
dHS1spr_XG = 0.0
dHL1spr_XG = 0.0
HTspr_XG = 0.0/0.0
HSspr_XG = 0.0/0.0
HRspr_XG = 0.0/0.0
HLspr_XG = 0.0/0.0
alpha_S_XG = 0.0/0.0
beta_S_XG = 0.0/0.0
beta_L_XG = 0.0/0.0
gamma_H_XG = 0.0/0.0
delt_spr_XG = 0.0/0.0
delq_spr_XG = 0.0/0.0
Mspr_XG = 0.0/0.0

! Pack points for spray calculations into smaller arrays
wspd10_XG = SQRT(u10_XG**2 + v10_XG**2)    ! 10-m windspeed [m s-1]
calcSpray = (ISNAN(eps_XG) == .FALSE.) .AND. (wspd10_XG > 10.)    ! Only do sea points with wspd10 > 10 m s-1
nSpray = count(calcSpray)
ALLOCATE(z_1(nSpray),U_1(nSpray),th_1(nSpray),q_1(nSpray),p_0(nSpray),&
         eps(nSpray),dcp(nSpray),swh(nSpray),mss(nSpray),ustar(nSpray),t_0(nSpray))
ALLOCATE(dHS1spr(nSpray),dHL1spr(nSpray),HTspr(nSpray),HSspr(nSpray),HRspr(nSpray),&
         HLspr(nSpray),alpha_S(nSpray),beta_S(nSpray),beta_L(nSpray),&
         gamma_H(nSpray),delt_spr(nSpray),delq_spr(nSpray),Mspr(nSpray))
z_1      = PACK(z_1_XG,calcSpray)
U_1      = PACK(U_1_XG,calcSpray)
th_1     = PACK(th_1_XG,calcSpray)
q_1      = PACK(q_1_XG,calcSpray)
p_0      = PACK(p_0_XG,calcSpray)
eps      = PACK(eps_XG,calcSpray)
dcp      = PACK(dcp_XG,calcSpray)
swh      = PACK(swh_XG,calcSpray)
mss      = PACK(mss_XG,calcSpray)
ustar    = PACK(ustar_XG,calcSpray)
t_0      = PACK(t_0_XG,calcSpray)

! Calculate spray modifications to heat fluxes and other relevant quantities
CALL spraymedHF(z_1,U_1,th_1,q_1,p_0,eps,dcp,swh,mss,ustar,t_0,dHS1spr,dHL1spr,&
        HTspr,HSspr,HRspr,HLspr,alpha_S,beta_S,beta_L,gamma_H,delt_spr,delq_spr,Mspr,rc=rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! If spray calculations produced any NaNs in dHS1spr or dHL1spr, eliminate these
where ((ISNAN(dHS1spr)) .OR. (ISNAN(dHL1spr)))
        dHS1spr = 0.
        dHL1spr = 0.
end where

! Unpack result arrays
dHS1spr_XG  = UNPACK(dHS1spr,calcSpray,dHS1spr_XG)
dHL1spr_XG  = UNPACK(dHL1spr,calcSpray,dHL1spr_XG)
HTspr_XG    = UNPACK(HTspr,calcSpray,HTspr_XG)
HSspr_XG    = UNPACK(HSspr,calcSpray,HSspr_XG)
HRspr_XG    = UNPACK(HRspr,calcSpray,HRspr_XG)
HLspr_XG    = UNPACK(HLspr,calcSpray,HLspr_XG)
alpha_S_XG  = UNPACK(alpha_S,calcSpray,alpha_S_XG)
beta_S_XG   = UNPACK(beta_S,calcSpray,beta_S_XG)
beta_L_XG   = UNPACK(beta_L,calcSpray,beta_L_XG)
gamma_H_XG  = UNPACK(gamma_H,calcSpray,gamma_H_XG)
delt_spr_XG = UNPACK(delt_spr,calcSpray,delt_spr_XG)
delq_spr_XG = UNPACK(delq_spr,calcSpray,delq_spr_XG)
Mspr_XG     = UNPACK(Mspr,calcSpray,Mspr_XG)

! Write results to exchange grid auxiliary fields
xg % auxField2DPtr(1) % Ptr(isx:iex,jsx:jex) =  dHS1spr_XG(isx:iex,jsx:jex)
xg % auxField2DPtr(2) % Ptr(isx:iex,jsx:jex) =  dHL1spr_XG(isx:iex,jsx:jex)
xg % auxField2DPtr(3) % Ptr(isx:iex,jsx:jex) =    HTspr_XG(isx:iex,jsx:jex)
xg % auxField2DPtr(4) % Ptr(isx:iex,jsx:jex) =    HSspr_XG(isx:iex,jsx:jex)
xg % auxField2DPtr(5) % Ptr(isx:iex,jsx:jex) =    HRspr_XG(isx:iex,jsx:jex)
xg % auxField2DPtr(6) % Ptr(isx:iex,jsx:jex) =    HLspr_XG(isx:iex,jsx:jex)
xg % auxField2DPtr(7) % Ptr(isx:iex,jsx:jex) =  alpha_S_XG(isx:iex,jsx:jex)
xg % auxField2DPtr(8) % Ptr(isx:iex,jsx:jex) =   beta_S_XG(isx:iex,jsx:jex)
xg % auxField2DPtr(9) % Ptr(isx:iex,jsx:jex) =   beta_L_XG(isx:iex,jsx:jex)
xg % auxField2DPtr(10)% Ptr(isx:iex,jsx:jex) =  gamma_H_XG(isx:iex,jsx:jex)
xg % auxField2DPtr(11)% Ptr(isx:iex,jsx:jex) = delt_spr_XG(isx:iex,jsx:jex)
xg % auxField2DPtr(12)% Ptr(isx:iex,jsx:jex) = delq_spr_XG(isx:iex,jsx:jex)
xg % auxField2DPtr(13)% Ptr(isx:iex,jsx:jex) =     Mspr_XG(isx:iex,jsx:jex)

! Remap from exchange grid auxiliary fields to gc(1) auxiliary fields
do i = 1,numWRFAuxFields2D
    CALL FieldRemap(srcField    = xg % auxField2D(indxAuxField2D_XG2WRF(i)),  &
                    dstField    = gc(1) % auxField2D(i),  &
                    remapRH     = routeHandle,            &
                    zeroregion  = ESMF_REGION_TOTAL,      &
                    checkflag   = .FALSE.,                &
                    rc          = rc)
    IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)
enddo

rc = ESMF_SUCCESS

ENDSUBROUTINE SprayMedHeatFluxes
!===============================================================================

ENDMODULE UWIN_spray_interface
