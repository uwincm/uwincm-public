MODULE UWIN_spray_physics

USE ESMF
USE UWIN_global

IMPLICIT NONE
PRIVATE
PUBLIC :: spraymedHF
PUBLIC :: update_sprayHF
PUBLIC :: sourceM_F09
PUBLIC :: sourceM_F94
PUBLIC :: fall_velocity_PK97
PUBLIC :: qsat0
PUBLIC :: satratio
!==============================================================================
CONTAINS

SUBROUTINE spraymedHF(z_1,U_1,th_1,q_1,p_0,eps,dcp,swh,mss,ustar,t_0,dHS1spr,dHL1spr,&
                H_Tspr,H_Sspr,H_Rspr,H_Lspr,alpha_S,beta_S,beta_L,gamma_H,delt_spr,delq_spr,Mspr,rc)
!----------------------------------------------
! Model for spray modification to heat fluxes.  We calculate modifications (dHS1spr, dHL1spr) 
! which are added to WRF bulk heat fluxes in WRF's module_first_rk_step_part1.  We calculate 
! our own bulk heat fluxes (without stability or convective velocity adjustments) to 
! estimate the vertical theta and q profiles used as ambient conditions for spray heat 
! fluxes, but these are not used by WRF.  Neutral stability is assumed throughout.
! 
! Input Arguments:
!     z_1 - height of lowest model level [m]
!     U_1 - windspeed at lowest model level [m s-1]
!     th_1 - air potential temperature at lowest model level [K]
!     q_1 - specific humidity at lowest model level [kg kg-1]
!     p_0 - pressure [Pa], uses surface pressure which is assumed to not change vertically
!     eps - wave energy dissipation flux [kg s-3]
!     dcp - dominant phase velocity [m s-1]
!     swh - significant wave height [m]
!     mss - mean squared waveslope [-]
!     ustar - friction velocity [m s-1]
!     t_0 - sea surface temperature [K]
! Output Arguments:
!     dHS1spr - modification to SHF due to spray [W m-2]
!     dHL1spr - modification to LHF due to spray [W m-2]
!     H_Tspr - spray heat flux due to temperature change [W m-2]
!     H_Sspr - spray SHF [W m-2]
!     H_Rspr - spray heat flux due to droplet size change [W m-2]
!     H_Lspr - spray LHF [W m-2]
!     alpha_S - spray feedback coefficient alpha_S [-]
!     beta_S - spray feedback coefficient beta_S [-]
!     beta_L - spray feedback coefficient beta_L [-]
!     gamma_H - spray feedback coefficient gamma_H for enthalpy [-], 
!                   set equal to gamma_L since gamma_L ~ gamma_S
!     delt_spr - warming at mid-spray layer due to feedback [K]
!     delq_spr - moistening at mid-spray layer due to feedback [kg kg-1]
!     Mspr - spray mass flux [kg m-2 s-1]
!----------------------------------------------

REAL(KIND=ESMF_KIND_R4),DIMENSION(:),INTENT(IN) :: z_1,U_1,th_1,q_1,p_0,eps,dcp,swh,mss,ustar,t_0
REAL(KIND=ESMF_KIND_R4),DIMENSION(:),INTENT(OUT) :: dHS1spr,dHL1spr,H_Tspr,H_Sspr,H_Rspr,H_Lspr,alpha_S,&
                                                      beta_S,beta_L,gamma_H,delt_spr,delq_spr,Mspr
INTEGER,INTENT(OUT),OPTIONAL :: rc
INTEGER(KIND=ESMF_KIND_I4) :: Ng,i,n
REAL(KIND=ESMF_KIND_R4),PARAMETER :: kappa = 0.41    ! von Karman constant [-]
REAL(KIND=ESMF_KIND_R4),PARAMETER :: g = 9.81    ! Acceleration due to gravity [m s-2]
REAL(KIND=ESMF_KIND_R4),PARAMETER :: Rdry = 287.    ! Dry air gas constant [J kg-1 K-1]
REAL(KIND=ESMF_KIND_R4),PARAMETER :: rho_sw = 1030.    ! Density of seawater [kg m-3]
REAL(KIND=ESMF_KIND_R4),PARAMETER :: rho_dry = 2160.    ! Density of chrystalline salt [kg m-3]
REAL(KIND=ESMF_KIND_R4),PARAMETER :: cp_sw = 4200.    ! Specific heat capacity of seawater [J kg-1 K-1]
REAL(KIND=ESMF_KIND_R4),PARAMETER :: cp_a = 1004.67    ! Specific heat capacity of air [J kg-1 K-1]
REAL(KIND=ESMF_KIND_R4),PARAMETER :: Lv = 2.43e6    ! Latent heat of vap for water at 30C [J kg-1]
REAL(KIND=ESMF_KIND_R4),PARAMETER :: Pr_a = 0.71    ! Prandtl number for air [-]
REAL(KIND=ESMF_KIND_R4),PARAMETER :: Sc_a = 0.60    ! Schmidt number for air [-]
REAL(KIND=ESMF_KIND_R4),PARAMETER :: nu = 2    ! Number of ions into which NaCl dissociates [-]
REAL(KIND=ESMF_KIND_R4),PARAMETER :: Phi_s = 0.924    ! Practical osmotic coefficient at molality of 0.6 [-]
REAL(KIND=ESMF_KIND_R4),PARAMETER :: Mw = 18.02    ! Molecular weight of water [g mol-1]
REAL(KIND=ESMF_KIND_R4),PARAMETER :: Ms = 58.44    ! Molecular weight of salt [g mol-1]
REAL(KIND=ESMF_KIND_R4),PARAMETER :: xs = 0.035    ! Mass fraction of salt in seawater [-]
REAL(KIND=ESMF_KIND_R4) :: y0
INTEGER(KIND=ESMF_KIND_I4),PARAMETER :: Nr = 25
REAL(KIND=ESMF_KIND_R4),DIMENSION(25) :: r0       = (/ 10.,20.,30.,40.,50.,60.,70.,80.,90.,102.5,122.5,157.5,&
        215.,300.,400.,500.,600.,700.,800.,900.,1037.5,1250.,1500.,1750.,2000. /)*1e-6    ! Drop radius vector [m]
REAL(KIND=ESMF_KIND_R4),DIMENSION(25) :: delta_r0 = (/ 10.,10.,10.,10.,10.,10.,10.,10.,10.,  15.,  25.,  45.,&
         70.,100.,100.,100.,100.,100.,100.,100.,  175., 250., 250., 250., 250. /)*1e-6    ! Drop bin width vect [m]
REAL(KIND=ESMF_KIND_R4),DIMENSION(25) :: v_g
REAL(KIND=ESMF_KIND_R4),DIMENSION(:),ALLOCATABLE :: th2t,q_0,t_1,t_mean,tC_mean,q_mean,rho_a,k_a,&
        nu_a,Dv_a,gammaWB,G_S,G_L,delspr,th_0,z0,Restar,z0t,z0q,H_S0pr,H_L0pr,t_delsprD2pr,q_delsprD2pr,&
        U_10,h_gust,U_h,zR,H_Tsprpr,H_Ssprpr,H_Rsprpr,H_Lsprpr,H_S1,H_L1,H_S0,H_L0,t_delsprD2,q_delsprD2,&
        eps_cutoff,gamma_S,gamma_L,s_delsprD2pr,etaT,Cs_pr,C_HIG,H_IG,Psi,Chi,Lambda,A,B,C,B24AC,&
        s_hatPOS,H_Rspr_IG
REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),ALLOCATABLE :: dmdr0,tauf,Fp,tauT,zT

ALLOCATE(th2t,q_0,t_1,t_mean,tC_mean,q_mean,rho_a,k_a,nu_a,Dv_a,gammaWB,G_S,G_L,delspr,th_0,&
       z0,Restar,z0t,z0q,H_S0pr,H_L0pr,t_delsprD2pr,q_delsprD2pr,U_10,h_gust,U_h,zR,H_Tsprpr,H_Ssprpr,&
       H_Rsprpr,H_Lsprpr,H_S1,H_L1,H_S0,H_L0,t_delsprD2,q_delsprD2,eps_cutoff,gamma_S,gamma_L,&
       s_delsprD2pr,etaT,Cs_pr,C_HIG,H_IG,Psi,Chi,Lambda,A,B,C,B24AC,s_hatPOS,H_Rspr_IG, mold=z_1)

! 1. Non-varying parameters --------------------------------------------------------------------
Ng = size(z_1)    ! Number of horizontal gridpoints
th2t = (p_0/1e5)**0.286    ! Factor converting potential temperature to temperature [-]
y0 = -nu*Phi_s*Mw/Ms*xs/(1 - xs)    ! y for surface seawater [-]
q_0 = qsat0(t_0,p_0)*(1 + y0)    ! Specific humidity at surface (accounting for salt) [kg kg-1]
t_1 = th_1*th2t    ! Temperature at z_1 [K]
t_mean = 0.5*(t_1 + t_0)    ! Approx mean air temperature [K]
tC_mean = t_mean - 273.15    ! Approx mean air temperature [C]
q_mean = 0.5*(q_1 + q_0)    ! Approx mean air specific humidity [kg kg-1]
rho_a = p_0/(Rdry*t_mean*(1.+0.61*q_mean))    ! Air density [kg m-3]
k_a = 2.411e-2*(1.+3.309e-3*tC_mean-1.441e-6*tC_mean**2)    ! Air thermal conductivity [W m-1 K-1]
nu_a = 1.326e-5*(1.+6.542e-3*tC_mean+8.301e-6*tC_mean**2-4.84e-9*tC_mean**3)    ! Kin visc of air [m2 s-1]
Dv_a = 2.11e-5*((tC_mean+273.)/273.)**1.94    ! Water vapor diffusivity for air [m2 s-1]
gammaWB = 240.97*17.502/(tC_mean+240.97)**2    ! gamma = (dqsat0/dT)/qsat0 [K-1], per Buck (1981) correlation
G_S = rho_a*cp_a*kappa*ustar    ! Dimensional group for SHF [W m-2 K-1]
G_L = rho_a*Lv*kappa*ustar    ! Dimensional group for LHF [W m-2]
delspr = MIN(swh,z_1)    ! Spray layer thickness [m]
th_0 = t_0/th2t    ! Surface potential temperature [K]

! 2. Interfacial heat fluxes and related parameters, assuming neutral stability ---------------
z0 = z_1/EXP(kappa*U_1/ustar)    ! Momentum roughness length [m]
Restar = ustar*z0/nu_a    ! Roughness Reynolds number [-]
z0t = z0/EXP(kappa*(7.3*Restar**0.25*Pr_a**0.5 - 5.))    ! Thermal roughness length [m], Garratt 1992 Eq 4.14
z0q = z0/EXP(kappa*(7.3*Restar**0.25*Sc_a**0.5 - 5.))    ! Moisture roughness length [m], Garratt 1992 Eq 4.15
H_S0pr = G_S*(th_0 - th_1)/LOG(z_1/z0t)    ! Interfacial sensible heat flux without spray [W m-2]
H_L0pr = G_L*( q_0 -  q_1)/LOG(z_1/z0q)    ! Interfacial latent heat flux without spray [W m-2]
t_delsprD2pr = (th_0 - H_S0pr/G_S*LOG(delspr/2/z0t))*th2t    ! t at mid-spray layer without feedback [K]
q_delsprD2pr =   q_0 - H_L0pr/G_L*LOG(delspr/2/z0q)    ! q at mid-spray layer without feedback [kg kg-1]
s_delsprD2pr = satratio(t_delsprD2pr,p_0,q_delsprD2pr)    ! s at mid-spray layer without feedback [-]
if (spraySubgridFeedback) then
    gamma_S = (LOG(delspr/z0t) - 1.)/LOG(z_1/z0t)    ! Spray feedback coefficient gamma_S [-]
    gamma_L = (LOG(delspr/z0q) - 1.)/LOG(z_1/z0q)    ! Spray feedback coefficient gamma_L [-]
else
    gamma_S = 1.
    gamma_L = 1.
end if

! 3. Background calculations for spray heat fluxes --------------------------------------------
U_10 = ustar/kappa*LOG(10./z0)    ! 10m windspeed [m s-1]
! Hack: Use eps_cutoff to cap eps if unphysically large.  Not currently used (set extremely high).
eps_cutoff = eps
where(eps_cutoff > 1000.) eps_cutoff = 1000.
ALLOCATE(dmdr0(Ng,Nr),tauf(Ng,Nr),Fp(Ng,Nr),tauT(Ng,Nr),zT(Ng,Nr))
v_g = fall_velocity_PK97(r0)    ! Gravitational settling velocity [m s-1]
if (SSGF_name == 'SSBased') then
    h_gust = 200.*z0    ! Gust height [m]
    U_h = ustar/kappa*LOG(h_gust/z0)    ! Windspeed at gust height [m s-1]
    CALL sourceM_F09(r0,delta_r0,v_g,eps_cutoff,swh,dcp,mss,ustar,U_10,U_h,Mspr,dmdr0)    ! [kg m-2 s-1],[kg m-2 s-1 m-1]
else if (SSGF_name == 'WindBased') then
    CALL sourceM_F94(r0,delta_r0,U_10,Mspr,dmdr0)    ! [kg m-2 s-1],[kg m-2 s-1 m-1]
end if
do i = 1,Nr
    tauf(:,i) = delspr/v_g(i)    ! Characteristic droplet settling time [s]
    Fp(:,i) = 1.+0.25*(2.*v_g(i)*r0(i)/nu_a)**0.5    ! Slip factor (Pruppacher and Klett) [-]
    tauT(:,i) = rho_sw*cp_sw*r0(i)**2/3./k_a/Fp(:,i)    ! Characteristic droplet cooling time [s]
    zT(:,i) = MIN(0.5*delspr,0.5*v_g(i)*tauT(:,i))    ! Height for spray HF due to temp change [m]
end do
zR = delspr/2.    ! Height for spray HF due to size change [m], varying zR does not change results significantly

! 4. Spray heat fluxes without and with feedback -----------------------------------------------
! Calculate spray heat fluxes without feedback
H_Sspr = 0.
H_Rspr = 0.
H_Lspr = 0.
CALL update_sprayHF(H_Sspr,H_Rspr,H_Lspr,r0,delta_r0,H_S0pr,H_L0pr,gamma_S,gamma_L,th_0,q_0,&
        G_S,G_L,z0t,z0q,delspr,th2t,p_0,gammaWB,t_0,rho_a,Dv_a,zT,tauf,tauT,Fp,dmdr0,&
        Nr,Lv,y0,cp_a,rho_sw,xs,nu,Phi_s,Mw,Ms,cp_sw,H_Tspr)
H_Tsprpr = H_Tspr    ! Spray HF due to temp change, without feedback [W m-2]
H_Ssprpr = H_Sspr    ! Spray SHF, without feedback [W m-2]
H_Rsprpr = H_Rspr    ! Spray HF due to size change, without feedback [W m-2]
H_Lsprpr = H_Lspr    ! Spray LHF, without feedback [W m-2]
! If selected, solve for spray heat fluxes with feedback using simple model for initial guess
if (spraySubgridFeedback) then
    ! Calculate IG
    etaT = 17.502*240.97/(t_delsprD2pr-273.15+240.97)**2    ! [K-1]
    Cs_pr = (1.+y0-s_delsprD2pr)**2/(1.-s_delsprD2pr)    ! [-]
    C_HIG = -0.05*delspr + 1.2    ! Tuneable constant for equivalent height
    where(delspr < 4.) C_HIG = 1.0
    where(delspr > 10.) C_HIG = 0.7
    H_IG = MIN(C_HIG*delspr,z_1)    ! Equivalent height for heating in simple model [m]
    Psi = etaT*LOG(z_1/H_IG)/G_S*H_Tsprpr    ! [-]
    Chi = etaT*LOG(z_1/H_IG)/G_S + LOG(z_1/H_IG)/G_L/q_delsprD2pr    ! [m2 W-1]
    Lambda = (Psi - 1. + (1.+y0)/s_delsprD2pr)/Chi    ! [W m-2]
    A = H_Rsprpr/Cs_pr + 1./s_delsprD2pr/Chi    ! [W m-2]
    B = -Lambda - y0/s_delsprD2pr/Chi    ! [W m-2]
    C = y0*Lambda    ! [W m-2]
    B24AC = B**2 - 4.*A*C    ! Argument of radical in quadratic formula [W2 m-4]
    s_hatPOS = 0.    ! Initialize to zeros [-]
    H_Rspr_IG = 0.    ! Initialize to zeros, we assume values of zero where B24AC < 0 [W m-2]
    where(B24AC >= 0.) s_hatPOS = (-B + SQRT(B24AC))/2./A    ! Solve for positive real roots
    where(B24AC >= 0.) H_Rspr_IG = s_hatPOS**2/(s_hatPOS-y0)*H_Rsprpr/Cs_pr    ! Calculate IG [W m-2]
    ! Update heat fluxes using IG
    H_Rspr = H_Rspr_IG    ! Replace H_Rspr with IG values
    H_Lspr = H_Rspr_IG + (H_Tspr - H_Sspr)    ! Replace H_Lspr with IG values
    CALL update_sprayHF(H_Sspr,H_Rspr,H_Lspr,r0,delta_r0,H_S0pr,H_L0pr,gamma_S,gamma_L,th_0,q_0,&
            G_S,G_L,z0t,z0q,delspr,th2t,p_0,gammaWB,t_0,rho_a,Dv_a,zT,tauf,tauT,Fp,dmdr0,&
            Nr,Lv,y0,cp_a,rho_sw,xs,nu,Phi_s,Mw,Ms,cp_sw,H_Tspr)
end if

! 5. Spray-mediated heat fluxes and diagnostics ------------------------------------------------
H_S1 = H_S0pr + gamma_S*(H_Sspr - H_Rspr)    ! SHF with spray [W m-2]
H_L1 = H_L0pr + gamma_L*H_Lspr    ! LHF with spray [W m-2]
H_S0 = H_S1 - (H_Sspr - H_Rspr)    ! Interfacial SHF in the presence of spray [W m-2]
H_L0 = H_L1 - H_Lspr    ! Interfacial LHF in the presence of spray [W m-2]
dHS1spr = H_S1 - H_S0pr    ! Modification to SHF due to spray [W m-2]
dHL1spr = H_L1 - H_L0pr    ! Modification to LHF due to spray [W m-2]
alpha_S = H_Sspr/H_Ssprpr    ! Spray feedback coefficient alpha_S [-]
beta_S = H_Rspr/H_Rsprpr    ! Spray feedback coefficient beta_S [-]
beta_L = H_Lspr/H_Lsprpr    ! Spray feedback coefficient beta_L [-]
if (spraySubgridFeedback .EQ. .FALSE.) then
    t_delsprD2 = t_delsprD2pr
    q_delsprD2 = q_delsprD2pr
else if (spraySubgridFeedback .EQ. .TRUE.) then
    t_delsprD2 = (th_0 - 1./G_S*(H_S0*LOG(delspr/2./z0t) + 0.5*(H_Sspr - H_Rspr)))*th2t    ! Temp at delspr/2 [K]
    q_delsprD2 =   q_0 - 1./G_L*(H_L0*LOG(delspr/2./z0q) + 0.5*H_Lspr)    ! Specific humidity at delspr/2 [kg kg-1]
end if
delt_spr = t_delsprD2 - t_delsprD2pr    ! Temp difference at mid-spray layer due to feedback [K]
delq_spr = q_delsprD2 - q_delsprD2pr    ! q difference at mid-spray layer due to feedback [kg kg-1]
gamma_H = gamma_L    ! Just return one value (gamma_L) since gamma_L ~ gamma_S

rc = ESMF_SUCCESS

ENDSUBROUTINE spraymedHF
!==========================================================================================

SUBROUTINE update_sprayHF(H_Sspr,H_Rspr,H_Lspr,r0,delta_r0,H_S0pr,H_L0pr,gamma_S,gamma_L,th_0,q_0,&
        G_S,G_L,z0t,z0q,delspr,th2t,p_0,gammaWB,t_0,rho_a,Dv_a,zT,tauf,tauT,Fp,dmdr0,&
        Nr,Lv,y0,cp_a,rho_sw,xs,nu,Phi_s,Mw,Ms,cp_sw,H_Tspr)
!------------------------------------------------------
! Subroutine to update spray heat fluxes based on previous values.
! Updated (INOUT) Arguments:
!     H_Sspr [W m-2], H_Rspr [W m-2], H_Lspr [W m-2]
! Input Arguments:
!     r0 [m], delta_r0 [m], H_S0pr [W m-2], H_L0pr [W m-2], gamma_S [-], gamma_L [-], th_0 [K],
!     q_0 [kg kg-1], G_S [W m-2 K-1], G_L [W m-2], z0t [m], z0q [m], delspr [m], th2t [-],
!     p_0 [Pa], gammaWB [K-1], t_0 [K], rho_a [kg m-3], Dv_a [m2 s-1], zT [m],
!     tauf [s], tauT [s], Fp [-], dmdr0 [kg m-2 s-1 m-1], Nr [-], Lv [J kg-1],
!     y0 [-], cp_a [J kg-1 K-1], rho_sw [kg m-3], xs [-], nu [-], Phi_s [-],
!     Mw [g mol-1], Ms [g mol-1], cp_sw [J kg-1 K-1]
! Output Arguments:
!     H_Tspr [W m-2]
!------------------------------------------------------

REAL(KIND=ESMF_KIND_R4),DIMENSION(:),INTENT(INOUT) :: H_Sspr,H_Rspr,H_Lspr
REAL(KIND=ESMF_KIND_R4),DIMENSION(:),INTENT(IN) :: r0,delta_r0
REAL(KIND=ESMF_KIND_R4),DIMENSION(:),INTENT(IN) :: H_S0pr,H_L0pr,gamma_S,gamma_L,th_0,q_0,G_S,G_L,&
        z0t,z0q,delspr,th2t,p_0,gammaWB,t_0,rho_a,Dv_a
REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),INTENT(IN) :: zT,tauf,tauT,Fp,dmdr0
INTEGER(KIND=ESMF_KIND_I4),INTENT(IN) :: Nr
REAL(KIND=ESMF_KIND_R4),INTENT(IN) :: Lv,y0,cp_a,rho_sw,xs,nu,Phi_s,Mw,Ms,cp_sw
REAL(KIND=ESMF_KIND_R4),DIMENSION(:),INTENT(OUT) :: H_Tspr
REAL(KIND=ESMF_KIND_R4),DIMENSION(:),ALLOCATABLE :: H_S0,H_L0,t_zR,q_zR,qsat0_zR,betaWB_zR,s_zR
REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),ALLOCATABLE :: t_zT,q_zT,qsat0_zT,betaWB_zT,s_zT,wetdep_zT,tWB_zT,&
        tdropf,tauR,req,rf
INTEGER(KIND=ESMF_KIND_I4) :: i,j

ALLOCATE(H_S0,H_L0,t_zR,q_zR,qsat0_zR,betaWB_zR,s_zR, mold=H_Sspr)
ALLOCATE(t_zT,q_zT,qsat0_zT,betaWB_zT,s_zT,wetdep_zT,tWB_zT,tdropf,tauR,req,rf, mold=zT)

! Surface heat fluxes using previous values of spray heat fluxes
H_S0 = H_S0pr - (1. - gamma_S)*(H_Sspr - H_Rspr)    ! Surface SHF [W m-2]
H_L0 = H_L0pr - (1. - gamma_L)*H_Lspr    ! Surface LHF [W m-2]

! Thermodynamic parameters for H_Tspr
do i = 1,Nr
    t_zT(:,i) = (th_0 - 1./G_S*(H_S0*LOG((z0t+zT(:,i))/z0t) + zT(:,i)/delspr*(H_Sspr - H_Rspr)))*th2t    ! t at zT [K]
    q_zT(:,i) =   q_0 - 1./G_L*(H_L0*LOG((z0q+zT(:,i))/z0q) + zT(:,i)/delspr*H_Lspr)    ! q at zT [kg kg-1]
    qsat0_zT(:,i) = qsat0(t_zT(:,i),p_0)    ! Saturation specific humidity at zT [kg kg-1]
    betaWB_zT(:,i) = 1./(1. + Lv*gammaWB*(1. + y0)/cp_a*qsat0_zT(:,i))    ! Wetbulb coefficient at zT [-]
    s_zT(:,i) = satratio(t_zT(:,i),p_0,q_zT(:,i))    ! Saturation ratio at zT [-]
    wetdep_zT(:,i) = (1. - s_zT(:,i)/(1. + y0))*(1. - betaWB_zT(:,i))/gammaWB    ! Wetbulb depression at zT [K]
    tWB_zT(:,i) = t_zT(:,i) - wetdep_zT(:,i)    ! Wetbulb temperature at zT [K]
    tdropf(:,i) = tWB_zT(:,i) + (t_0 - tWB_zT(:,i))*EXP(-tauf(:,i)/tauT(:,i))    ! Final droplet temperature [K]
end do

! Thermodynamic parameters for H_Rspr; height for all calculations is 0.5delspr
t_zR = (th_0 - 1./G_S*(H_S0*LOG((z0t+delspr/2.)/z0t) + 0.5*(H_Sspr - H_Rspr)))*th2t    ! t at zR [K]
q_zR =   q_0 - 1./G_L*(H_L0*LOG((z0q+delspr/2.)/z0q) + 0.5*H_Lspr)    ! q at zR [kg kg-1]
qsat0_zR = qsat0(t_zR,p_0)    ! Saturation specific humidity at zR [kg kg-1]
betaWB_zR = 1./(1. + Lv*gammaWB*(1. + y0)/cp_a*qsat0_zR)    ! Wetbulb coefficient at zR [-]
s_zR = satratio(t_zR,p_0,q_zR)    ! Saturation ratio at zR [-]
do i = 1,Nr
    tauR(:,i) = rho_sw*r0(i)**2/(rho_a*Dv_a*Fp(:,i)*qsat0_zR*betaWB_zR*ABS(1. + y0 - s_zR))    ! Char evap time [s]
    req(:,i) = r0(i)*(xs*(1. + nu*Phi_s*Mw/Ms/(1. - s_zR)))**(1./3.)    ! Equilibrium radius at zR [m]
    rf(:,i) = req(:,i) + (r0(i) - req(:,i))*EXP(-tauf(:,i)/tauR(:,i))    ! Final droblet radius [m]
    where(ABS(1. + y0 - s_zR) .LT. 1.e-3) rf(:,i) = r0(i)    ! Assume no change if s_zR ~ 1 + y0
end do

! Updated spray heat fluxes
do j = 1,SIZE(H_Sspr)
    H_Tspr(j) = DOT_PRODUCT(cp_sw*(t_0(j) - tdropf(j,:))*dmdr0(j,:),delta_r0)    ! Spray HF due to t change [W m-2]
    H_Sspr(j) = DOT_PRODUCT(cp_sw*SIGN(MIN(ABS(t_0(j) - tdropf(j,:)),ABS(t_0(j) - t_zT(j,:))),&
            t_0(j) - tWB_zT(j,:))*dmdr0(j,:),delta_r0)    ! Spray SHF [W m-2]
    H_Rspr(j) = DOT_PRODUCT(Lv*(1. - (rf(j,:)/r0)**3)*dmdr0(j,:),delta_r0)    ! Spray HF due to size change [W m-2]
end do
H_Lspr = H_Rspr + H_Tspr - H_Sspr    ! Spray LHF [W m-2]

ENDSUBROUTINE update_sprayHF
!==================================================================================

SUBROUTINE sourceM_F09(r0,delta_r0,v_g,eps,swh,dcp,mss,ustar,U_10,U_h,Mspr,dmdr0)
!------------------------------------------------------
! SSGF based on Fairall et al. 2009, calibrated using Ortiz-Suslow et al. 2016 data.
! Input Arguments:
!     r0 - droplet radius [m]
!     delta_r0 - increment in droplet radius vector [m]
!     v_g - gravitational settling velocity [m s-1]
!     eps - wave energy dissipation flux [kg s-3]
!     swh - significant wave height [m]
!     dcp - dominant phase velocity [m s-1]
!     mss - mean squared waveslope [-]
!     ustar - friction velocity [m s-1]
!     U_10 - 10-m windspeed [m s-1]
!     U_h - windspeed at gust height [m s-1]
! Output Arguments:
!     Mspr - total spray mass flux [kg m-2 s-1]
!     dmdr0 - mass spectrum of ejected droplets [kg m-2 s-1 m-1]
!-------------------------------------------------------

REAL(KIND=ESMF_KIND_R4),DIMENSION(:),INTENT(IN) :: r0,delta_r0,v_g
REAL(KIND=ESMF_KIND_R4),DIMENSION(:),INTENT(IN) :: eps,swh,dcp,mss,ustar,U_10,U_h
REAL(KIND=ESMF_KIND_R4),DIMENSION(:),INTENT(OUT) :: Mspr
REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),INTENT(OUT) :: dmdr0
REAL(KIND=ESMF_KIND_R4),PARAMETER :: C1 = 1.35
REAL(KIND=ESMF_KIND_R4),PARAMETER :: C2 = 0.1116
REAL(KIND=ESMF_KIND_R4),PARAMETER :: C3 = 0.719
REAL(KIND=ESMF_KIND_R4),PARAMETER :: C4 = 2.17
REAL(KIND=ESMF_KIND_R4),PARAMETER :: C5 = 0.852
REAL(KIND=ESMF_KIND_R4),PARAMETER :: g = 9.81    ! Acceleration due to gravity [m s-2]
REAL(KIND=ESMF_KIND_R4),PARAMETER :: rho_sw = 1030.    ! Density of seawater [kg m-3]
REAL(KIND=ESMF_KIND_R4),PARAMETER :: nu_w = 0.90e-6    ! Kinematic viscosity of water [m2 s-1]
REAL(KIND=ESMF_KIND_R4),PARAMETER :: sigma_surf = 7.4e-5    ! Ratio of surface tension to water density [m3 s-2]
REAL(KIND=ESMF_KIND_R4),PARAMETER :: alpha_k = 1.5    ! Kolmogorov constant [-]
REAL(KIND=ESMF_KIND_R4),PARAMETER :: PI = 3.14159265
INTEGER(KIND=ESMF_KIND_I4) :: i,j
REAL(KIND=ESMF_KIND_R4),DIMENSION(:),ALLOCATABLE :: Wa,eps_KV,eta_k,U_crest,dmdr0_form,ejecprob

ALLOCATE(Wa,eps_KV,eta_k,U_crest,dmdr0_form,ejecprob, mold=eps)

Wa = MIN(0.018*dcp*ustar**2/g/swh,1.0)    ! Active breaking whitecap fraction, per Deike et al. 2017
eps_KV = 100.*eps/swh/rho_sw/Wa    ! Surface vol kin dissipation under whitecaps [m2 s-3], 100 per S&M2015
eta_k = (nu_w**3/eps_KV)**0.25    ! Kolmogorov microscale [m]
U_crest = 0.8*dcp    ! Speed of breaking wave crests [m s-1], 0.8 per Banner et al. 2014
do i = 1, SIZE(r0)
    dmdr0_form = (SSGF_sourcestrength*C1*rho_sw*eps_KV*r0(i)*Wa)/(3.*sigma_surf)&
            *EXP(-1.5*alpha_k*C2*(PI*eta_k/r0(i))**(4./3.))    ! Spectrum of droplets formed [kg m-2 s-1 m-1]
    ejecprob = (1. + ERF((U_h - U_crest - v_g(i)/C3/mss)/C4/U_10 - C5))/2.    ! Ejection probability [-]
    dmdr0(:,i) = dmdr0_form*ejecprob    ! Ejected droplet spectrum (SSGF) [kg m-2 s-1 m-1]
end do
do j = 1, SIZE(eps)
    Mspr(j) = DOT_PRODUCT(dmdr0(j,:),delta_r0)    ! [kg m-2 s-1]
end do

ENDSUBROUTINE sourceM_F09
!===============================================================================

SUBROUTINE sourceM_F94(r0,delta_r0,wspd10,Mspr,dmdr0)
!------------------------------------------------------
! SSGF per Fairall et al. 1994.  Constructed per Mueller and Veron (2014 Part II).
! Whitecap fraction per Chris Fairall's email on May 4, 2021.
! Input Arguments:
!     r0 - droplet radius [m]
!     delta_r0 - increment in droplet radius vector [m]
!     wspd10 - 10-m windspeed [m s-1]
! Output Arguments:
!     Mspr - total spray mass flux [kg m-2 s-1]
!     dmdr0 - mass spectrum of ejected droplets [kg m-2 s-1 m-1]
!-------------------------------------------------------

REAL(KIND=ESMF_KIND_R4),DIMENSION(:),INTENT(IN) :: r0,delta_r0
REAL(KIND=ESMF_KIND_R4),DIMENSION(:),INTENT(IN) :: wspd10
REAL(KIND=ESMF_KIND_R4),DIMENSION(:),INTENT(OUT) :: Mspr
REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),INTENT(OUT) :: dmdr0
INTEGER :: i,j
REAL(KIND=ESMF_KIND_R4),PARAMETER :: rho_sw = 1030.    ! Density of seawater [kg m-3]
REAL(KIND=ESMF_KIND_R4),PARAMETER :: B0 = 4.405
REAL(KIND=ESMF_KIND_R4),PARAMETER :: B1 = -2.646
REAL(KIND=ESMF_KIND_R4),PARAMETER :: B2 = -3.156
REAL(KIND=ESMF_KIND_R4),PARAMETER :: B3 = 8.902
REAL(KIND=ESMF_KIND_R4),PARAMETER :: B4 = -4.482
REAL(KIND=ESMF_KIND_R4),PARAMETER :: C1 = 1.02e4
REAL(KIND=ESMF_KIND_R4),PARAMETER :: C2 = 6.95e6
REAL(KIND=ESMF_KIND_R4),PARAMETER :: C3 = 1.75e17
REAL(KIND=ESMF_KIND_R4),PARAMETER :: PI = 3.14159265
REAL(KIND=ESMF_KIND_R4),PARAMETER :: WC_A92_11ms = 6.5e-4*9**1.5    ! Whitecap fraction at 11 m/s [-]
REAL(KIND=ESMF_KIND_R4),DIMENSION(:),ALLOCATABLE :: r0_micrometers,r80,dFdr80,dFdr0_11ms,dFdr0_perWC,dVdr0_perWC
REAL(KIND=ESMF_KIND_R4),DIMENSION(:),ALLOCATABLE :: WC

ALLOCATE(r0_micrometers,r80,dFdr80,dFdr0_11ms,dFdr0_perWC,dVdr0_perWC, mold=r0)
ALLOCATE(WC, mold=wspd10)

r0_micrometers = r0*1e6    ! [um]
r80 = 0.518*r0_micrometers**0.976    ! Equilibrium radius at 80% RH [um]
do i = 1, SIZE(r0)
    if (r80(i) .lt. 0.8) then
        dFdr80(i) = 0.0
    else if ((r80(i) .ge. 0.8) .and. (r80(i) .lt. 15.)) then
        dFdr80(i) = 10.0**(B0 + B1*log10(r80(i))&
                              + B2*log10(r80(i))**2&
                              + B3*log10(r80(i))**3&
                              + B4*log10(r80(i))**4)
    else if ((r80(i) .ge.  15.) .and. (r80(i) .lt. 37.5)) then
        dFdr80(i) = C1*r80(i)**-1
    else if ((r80(i) .ge. 37.5) .and. (r80(i) .lt. 100.)) then
        dFdr80(i) = C2*r80(i)**-2.8
    else if ((r80(i) .ge. 100.) .and. (r80(i) .lt. 250.)) then
        dFdr80(i) = C3*r80(i)**-8
    else if  (r80(i) .ge. 250.) then
        dFdr80(i) = 0.0
    end if
end do
dFdr0_11ms = dFdr80*0.506*r0**-0.024    ! A92 SSGF at 11 m/s, based on r0 [m-2 s-1 um-1]
dFdr0_perWC = dFdr0_11ms/WC_A92_11ms*1e6    ! F94 number SSGF per unit whitecap area [m-2 s-1 m-1]
dVdr0_perWC = dFdr0_perWC*4./3.*PI*r0**3    ! F94 volume SSGF per unit whitecap area [m3 m-2 s-1 m-1]
WC = MIN(6.5e-4*MAX(wspd10 - 2.,0.)**1.5,1.)    ! Whitecap fraction per Chris Fairall 210504 [-]
do i = 1, SIZE(r0)
    dmdr0(:,i) = SSGF_sourcestrength*rho_sw*WC*dVdr0_perWC(i)    ! SSGF [kg m-2 s-1 m-1]
end do
do j = 1, SIZE(wspd10)
    Mspr(j) = DOT_PRODUCT(dmdr0(j,:),delta_r0)    ! Spray mass flux [kg m-2 s-1]
end do

ENDSUBROUTINE sourceM_F94
!===============================================================================

FUNCTION fall_velocity_PK97(r) RESULT(v_g)
!----------------------------------------------
! Fall velocity of spherical droplets, based on Pruppacher and Klett (1997) section 10.3.6.
! Input Arguments:
!     r - droplet radius [m]
! Return:
!     v_g - settling velocity [m s-1]
!----------------------------------------------

REAL(KIND=ESMF_KIND_R4),DIMENSION(:),INTENT(IN) :: r

REAL(KIND=ESMF_KIND_R4),PARAMETER :: nu_a = 1.5e-5    ! Kinematic viscosity of air [m2 s-1]
REAL(KIND=ESMF_KIND_R4),PARAMETER :: rho_a = 1.25    ! Density of air [kg m-3]
REAL(KIND=ESMF_KIND_R4),PARAMETER :: rho_sw = 1030.    ! Density of seawater [kg m-3]
REAL(KIND=ESMF_KIND_R4),PARAMETER :: g = 9.81    ! Acceleration due to gravity [m s-2]
REAL(KIND=ESMF_KIND_R4),PARAMETER :: sigma_aw = 7.4e-2    ! Surface tension of air-water interface [N m-1]
REAL(KIND=ESMF_KIND_R4),PARAMETER :: lamda_a0 = 6.6e-8    ! Mean free path at 1013.25 mb, 293.15 K [m]
REAL(KIND=ESMF_KIND_R4),DIMENSION(7) :: B1 = (/ -0.318657e1,0.992696,-0.153193e-2,-0.987059e-3,&
        -0.578878e-3,0.855176e-4,-0.327815e-5 /)    ! Polynomial coeffs for 10um < r < 535um
REAL(KIND=ESMF_KIND_R4),DIMENSION(6) :: B2 = (/ -0.500015e1,0.523778e1,-0.204914e1,0.475294,&
        -0.542819e-1,0.238449e-2 /)    ! Polynomial coeffs for r > 535um
INTEGER :: i
REAL(KIND=ESMF_KIND_R4) :: v_stokes,f_slip,CdNRe2,X1,Y1,NRe1,NBo,NP,NBoNP16,X2,Y2,NRe2
REAL(KIND=ESMF_KIND_R4),DIMENSION(:),ALLOCATABLE :: v_g

ALLOCATE(v_g, mold=r)

do i = 1, size(r)
    if (r(i) .lt. 10.e-6) then
        v_stokes = 2.*r(i)**2*g*(rho_sw - rho_a)/9./(rho_a*nu_a)    ! Stokes velocity [m s-1]
        f_slip = 1. + 1.26*lamda_a0/r(i)    ! Slip flow Cunningham correction factor [-]
        v_g(i) = f_slip*v_stokes    ! Settling velocity for r < 10um [m s-1]
    else if ((r(i) .ge. 10.e-6) .and. (r(i) .le. 535.e-6)) then
        CdNRe2 = 32.*r(i)**3*(rho_sw - rho_a)/rho_a/nu_a**2*g/3.    ! Product of Cd and Re**2 [-]
        X1 = LOG(CdNRe2)    ! 'X' in polynomial curve fit [-]
        Y1 = B1(1) + B1(2)*X1 + B1(3)*X1**2 + B1(4)*X1**3 + &
             B1(5)*X1**4 + B1(6)*X1**5 + B1(7)*X1**6    ! 'Y' in polynomial curve fit [-]
        NRe1 = EXP(Y1)    ! Reynolds number [-]
        v_g(i) = nu_a*NRe1/2./r(i)    ! Settling velocity for 10 <= r <= 535 um [m s-1]
    else if (r(i) .gt. 535.e-6) then
        NBo = g*(rho_sw - rho_a)*r(i)**2/sigma_aw    ! Bond number [-]
        NP = sigma_aw**3/rho_a**2/nu_a**4/g/(rho_sw - rho_a)    ! Physical property number [-]
        NBoNP16 = NBo*NP**(1./6.)    ! Product of NBo and NP**(1/6) [-]
        X2 = LOG(16./3.*NBoNP16)    ! 'X' in polynomial curve fit [-]
        Y2 = B2(1) + B2(2)*X2 + B2(3)*X2**2 + B2(4)*X2**3 + &
             B2(5)*X2**4 + B2(6)*X2**5    ! 'Y' in polynomial curve fit [-]
        NRe2 = NP**(1./6.)*EXP(Y2)    ! Reynolds number [-]
        v_g(i) = nu_a*NRe2/2./r(i)    ! Settling velocity for r > 535 um [m s-1]
    end if
end do
    
END FUNCTION
!==============================================================================

FUNCTION qsat0(T_K,P) RESULT(q_sat0)
!-------------------------------------
! Saturation specific humidity over plane surface of pure water, with e_sat0 per Buck (1981).
! Input Arguments:
!     T_K - temperature [K]
!     P - pressure [Pa]
! Return:
!     q_sat0 - saturation specific humidity [kg kg-1]
!--------------------------------------

REAL(KIND=ESMF_KIND_R4),DIMENSION(:),INTENT(IN) :: T_K,P
REAL(KIND=ESMF_KIND_R4),DIMENSION(:),ALLOCATABLE :: e_sat0,q_sat0

ALLOCATE(e_sat0,q_sat0, mold=T_K)

e_sat0 = 6.1121*exp(17.502*(T_K - 273.15)/(T_K - 273.15 + 240.97))*(1.0007 + 3.46e-8*P)*1e2    ! SVP [Pa]
q_sat0 = e_sat0*0.622/(P - 0.378*e_sat0)    ! Saturation specific humidity [kg kg-1]

END FUNCTION
!===============================================================================

FUNCTION satratio(T_K,P,q) RESULT(s)
!-------------------------------------
! Saturation ratio, with e_sat0 per Buck (1981).  We assume that s = q/q_sat0, rather than
! w/w_sat0, so that air with q = q_sat0 will be saturated.
! Input Arguments:
!     T_K - temperature [K]
!     P - pressure [Pa]
!     q - specific humidity [kg kg-1]
! Return:
!     s - saturation ratio [-]
!--------------------------------------

REAL(KIND=ESMF_KIND_R4),DIMENSION(:),INTENT(IN) :: T_K,P,q
REAL(KIND=ESMF_KIND_R4),DIMENSION(:),ALLOCATABLE :: q_sat0,s

ALLOCATE(q_sat0,s, mold=T_K)

q_sat0 = qsat0(T_K,P)    ! Saturation specific humidity [kg kg-1]
s = MIN(q/q_sat0,0.99999)    ! Saturation ratio [-]

END FUNCTION
!===============================================================================

ENDMODULE UWIN_spray_physics
