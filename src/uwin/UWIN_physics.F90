MODULE UWIN_physics
!===============================================================================
USE ESMF
USE NUOPC
USE UWIN_global
USE COAMPS_Util,ONLY:FieldRemapStore,FieldRemap
!===============================================================================
IMPLICIT NONE

PRIVATE

PUBLIC :: stability
PUBLIC :: meanSeaLevelPressure
PUBLIC :: VortexForce
PUBLIC :: WindStressKara

!===============================================================================
CONTAINS



SUBROUTINE stability(psim10,rmol,rc)
!===============================================================================
!
! Calculates the universal stability function for momentum at heights z.
! If stable or neutral conditions, uses input psim10. If unstable, uses 
! WRF's pre-computed PSIM tables.
!
! This subroutine must be called with the psim10 and rmol field at the UMWM space.
!
!===============================================================================

  USE UMWM_module,     ONLY:mi,ni,istart,iend,om,l2,psiml2
  USE module_sf_sfclay,ONLY:psimtb
  USE module_domain,   ONLY:head_grid

  TYPE(ESMF_Field),INTENT(IN), OPTIONAL :: psim10
  TYPE(ESMF_Field),INTENT(IN), OPTIONAL :: rmol
  INTEGER,         INTENT(OUT),OPTIONAL :: rc

  REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),POINTER,SAVE :: psim10_Ptr => NULL()
  REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),POINTER,SAVE :: rmol_Ptr   => NULL()

  REAL(KIND=ESMF_KIND_R4) :: z
  REAL(KIND=ESMF_KIND_R4) :: zol
  REAL(KIND=ESMF_KIND_R4) :: rzol
  INTEGER                 :: nzol

  INTEGER :: i,m,n,o

  LOGICAL,SAVE :: firstCall = .TRUE.

  !=============================================================================

  rc = ESMF_FAILURE
 
  !----------------------------------------------------------------------------- 
  IF(firstCall)THEN

    CALL ESMF_FieldGet(field     = psim10,    &
                       localDE   = 0,         & 
                       farrayPtr = psim10_Ptr,&
                       rc        = rc)
    IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

    CALL ESMF_FieldGet(field               = rmol,    &
                       localDE             = 0,       & 
                       farrayPtr           = rmol_Ptr,&
                       rc                  = rc)
    IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

    firstCall = .FALSE.

  ENDIF
  !----------------------------------------------------------------------------- 

  ! Loop over UMWM space
  ILOOP: DO i = istart,iend

    m = mi(i)
    n = ni(i)

    !---------------------------------------------------------------------------
    ! If stable/neutral conditions, Psim is linear (Classes 1, 2 and 3)
    IF(psim10_Ptr(m,n) <= 0)THEN

      ! Compute Psim(l2/L) from the surface up:
      FLOOP1: DO o = om,1,-1

        ! Height of half-wavelength [m]:
        z = l2(o,i) 

        ! If we have reached the height above 10-m,
        ! set all remaining psiml2 to psim10 and exit loop
        IF(z > 10.)THEN
          psiml2(1:o,i) = psim10_Ptr(m,n)
          EXIT FLOOP1
        ENDIF

        ! Linear transformation:
        psiml2(o,i) = l2(o,i)/10.*psim10_Ptr(m,n)

      ENDDO FLOOP1

    !---------------------------------------------------------------------------
    ! Unstable conditions (Class 4, free convection):
    ELSE 

      ! Compute Psim(l2/L) from the surface up:
      FLOOP2: DO o = om,1,-1

        ! Height of half-wavelength [m]:
        z = l2(o,i) 

        ! If we have reached the height above 10-m,
        ! set all remaining psiml2 to psim10 and exit loop
        IF(z > 10.)THEN
          psiml2(1:o,i) = psim10_Ptr(m,n)
          EXIT FLOOP2
        ENDIF

        ! z/L
        zol = z*rmol_Ptr(m,n)
        zol = AMIN1(zol,0.)
        zol = AMAX1(zol,-9.9999)
        
        nzol = INT(-zol*100.)
        rzol = -zol*100.-nzol

        ! Unstable calculation (look-up table)
        psiml2(o,i) = PSIMTB(nzol)+rzol*(PSIMTB(nzol+1)-PSIMTB(nzol))

      ENDDO FLOOP2

    ENDIF
    !---------------------------------------------------------------------------

  ENDDO ILOOP

  rc = ESMF_SUCCESS

ENDSUBROUTINE stability
!===============================================================================



SUBROUTINE VortexForce(uc,vc,us_src,vs_src,rc)
!===============================================================================
!
! Computes the Coriolis-Stokes and Vortex Force tendencies and updates
! the u, v, T and S fields in the ocean circulation model.
!
!===============================================================================

USE UWIN_utility,ONLY:FieldCreateFromField

USE mod_hycom,ONLY:dp,dpu,dpv,p,pu,pv,u,v,ubavg,vbavg,temp,saln,&
                   scqx,scqy,scvx,scvy,scux,scuy,scuxi,scvyi,scp2i,scq2i,&
                   corio,plon,plat,nstep
USE mod_xc,        ONLY:xctilr,halo_pv,halo_ps,halo_us,halo_uv,halo_vs,halo_vv
USE mod_dimensions,ONLY:ii_dims => ii,&
                        jj_dims => jj,&
                        kk,ip,nbdy

USE UMWM_stokes,ONLY:z_src => depth !! 1-dimensional depth array

USE netcdf

!===============================================================================

TYPE(ESMF_Field),INTENT(INOUT),OPTIONAL :: uc,vc           !! HYCOM current ESMF fields
TYPE(ESMF_Field),INTENT(IN),   OPTIONAL :: us_src,vs_src   !! UMWM Stokes drift ESMF fields
INTEGER,         INTENT(OUT),  OPTIONAL :: rc

TYPE(ESMF_Field),      SAVE :: us,vs                       !! UMWM Stokes drift on HYCOM grid
TYPE(ESMF_RouteHandle),SAVE :: routeHandle,haloRouteHandle !! Route handles

LOGICAL,SAVE :: firstTime = .TRUE.

REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:,:),POINTER,SAVE :: uc_Ptr => NULL()
REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:,:),POINTER,SAVE :: vc_Ptr => NULL()
REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:,:),POINTER,SAVE :: us_Ptr => NULL()
REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:,:),POINTER,SAVE :: vs_Ptr => NULL()

INTEGER(KIND=ESMF_KIND_I4),SAVE :: its,ite,itc
INTEGER(KIND=ESMF_KIND_I4),SAVE :: jts,jte,jtc
INTEGER(KIND=ESMF_KIND_I4),SAVE :: kts,kte,ktc

INTEGER :: i,j,k,m,n
INTEGER :: ii,jj

INTEGER,DIMENSION(3) :: lb,ub

INTEGER(KIND=ESMF_KIND_I4),DIMENSION(:,:,:),ALLOCATABLE,SAVE :: seamask 

! work arrays
REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:,:),ALLOCATABLE,SAVE :: us_dst
REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:,:),ALLOCATABLE,SAVE :: vs_dst
REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:,:),ALLOCATABLE,SAVE :: ws_dst
REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:,:),ALLOCATABLE,SAVE :: ut
REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:,:),ALLOCATABLE,SAVE :: vt
REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:,:),ALLOCATABLE,SAVE :: utend
REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:,:),ALLOCATABLE,SAVE :: vtend
REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:,:),ALLOCATABLE,SAVE :: Ttend
REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:,:),ALLOCATABLE,SAVE :: Stend
REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:,:),ALLOCATABLE,SAVE :: dudz
REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:,:),ALLOCATABLE,SAVE :: dvdz
REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:,:),ALLOCATABLE,SAVE :: vort

! Temporary advective fluxes
REAL(KIND=ESMF_KIND_R4) :: feup,fedn,fwup,fwdn,fsup,fsdn,fnup,fndn

REAL(KIND=ESMF_KIND_R4),DIMENSION(:),ALLOCATABLE,SAVE :: p_src,dp_src

REAL(KIND=ESMF_KIND_R4),PARAMETER :: thicknessFactor = 1./9806.

TYPE(ESMF_TimeInterval)         :: hycomTimeInterval
INTEGER(KIND=ESMF_KIND_I4),SAVE :: hycomTimeStep

TYPE(ESMF_Time)   :: currentTime
CHARACTER(LEN=19) :: timeStr

INTEGER,PARAMETER :: haloWidth = 1

INTEGER :: stat,ncid,xdimid,ydimid,zdimid
INTEGER :: usid,vsid,wsid,ucid,vcid,utendid,vtendid,vortid,lonid,latid
INTEGER :: dudzid,dvdzid,stendid,ttendid

REAL(KIND=ESMF_KIND_R4),PARAMETER :: eps = 1E-3

!===============================================================================

! STEPS:

! 1) Input fields are ESMF_Fields. This will allow for a generic physics routine
! that operates on fields coming from different grids. The routine should allow
! the user to choose whether to do calculations on grid1, grid2 or exchange grid.

! 2) Determine whether all input fields are on the same grid. If yes, there is
! no need for regridding, and the calculations can be done right away.
! If the input fields are from different grid, the user should provide a grid
! object on which the physics term will be output.

! 3) If initial regridding is necessary, regrid all non-matching fields to 
! the desired grid. Somehow we need to keep track of dx,dy,dz (probably ESMF_GridGetCoord)
! Maybe this step should be implemented as a generic routine, something like
! UMCM_ReconcileFields.

! 4) Once all input fields are on the same grid, get fortran arrays or pointers.

! 5) If halo operations are needed, perform halo first.

! 6) Perform calculations.  

! 7) Store result in output field.
!
!==============================================================================>

! Assume we are operating on OCN grid
rc = ESMF_FAILURE

IF(.NOT.ALLOCATED(z_src))THEN
  rc = ESMF_SUCCESS
  RETURN
ENDIF

IF(firstTime)THEN

  !---------------------------------------------------------------------------->
  ! Get Fortran array pointers to OCN current fields:
  CALL ESMF_FieldGet(field     = uc,    &
                     localDE   = 0,     &
                     farrayPtr = uc_Ptr,&
                     rc        = rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

  CALL ESMF_FieldGet(field               = vc,    &
                     localDE             = 0,     &
                     farrayPtr           = vc_Ptr,&
                     computationalLBound = lb,    &
                     computationalUBound = ub,    &
                     rc                  = rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

  its = lb(1); ite = ub(1)
  jts = lb(2); jte = ub(2)
  kts = lb(3); kte = ub(3)

  itc = ite-its+1
  jtc = jte-jts+1
  ktc = kte-kts+1

  ! Allocate utility arrays:
  ALLOCATE(us_dst(its-haloWidth:ite+haloWidth,jts-haloWidth:jte+haloWidth,kts:kte))
  ALLOCATE(vs_dst(its-haloWidth:ite+haloWidth,jts-haloWidth:jte+haloWidth,kts:kte))
  ALLOCATE(ws_dst(its:ite,jts:jte,kts:kte))

  ALLOCATE(seamask(its-haloWidth:ite+haloWidth,jts-haloWidth:jte+haloWidth,kts:kte))

  ! Need halo width of 2 for currents:
  ALLOCATE(ut(its-haloWidth-1:ite+haloWidth+1,jts-haloWidth-1:jte+haloWidth+1,kts:kte))
  ALLOCATE(vt(its-haloWidth-1:ite+haloWidth+1,jts-haloWidth-1:jte+haloWidth+1,kts:kte))

  ! Need halo width of 1 for vorticity
  ALLOCATE(vort(its-haloWidth:ite+haloWidth,jts-haloWidth:jte+haloWidth,kts:kte))

  ALLOCATE(dudz(its:ite,jts:jte,kts:kte))
  ALLOCATE(dvdz(its:ite,jts:jte,kts:kte))

  ALLOCATE(utend(its:ite,jts:jte,kts:kte))
  ALLOCATE(vtend(its:ite,jts:jte,kts:kte))
  ALLOCATE(Ttend(its:ite,jts:jte,kts:kte))
  ALLOCATE(Stend(its:ite,jts:jte,kts:kte))

  !--- UMWM depths and depth thicknesses in pressure units ---!
  ALLOCATE(p_src(kts:kte+1))
  ALLOCATE(dp_src(kts:kte))

  p_src(kts:kte) = -z_src*9806.
  p_src(kte+1) = 200*9806. ! Extend the lowest level to 200 m

  dp_src = 0
  DO k = kts,kte
    dp_src(k) = p_src(k+1)-p_src(k)
  ENDDO
  !-----------------------------------------------------------!

  !--- Generate mask on target grid --------------------------!
  
  DO j = jts-haloWidth,jte+haloWidth
    DO i = its-haloWidth,ite+haloWidth

      ii = i-its+1 ! HYCOM local index in x
      jj = j-jts+1 ! HYCOM local index in y

      seamask(i,j,:) = ip(ii,jj)

    ENDDO
  ENDDO

  ! Create Stokes drift fields on OCN grid and get Fortran array pointers

  !---------------------------------------------------------------------------->
  ! u-Stokes component 

  us = FieldCreateFromField(fieldIn = uc,       &
                            name    = 'ustokes',&
                            rc      = rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

  ! Initialize
  CALL ESMF_FieldGet(field     = us,    &
                     localDE   = 0,     &
                     farrayPtr = us_Ptr,&
                     rc        = rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

  us_Ptr = 0
  !---------------------------------------------------------------------------->

  !---------------------------------------------------------------------------->
  ! v-Stokes component 

  vs = FieldCreateFromField(fieldIn = vc,       &
                            name    = 'vstokes',&
                            rc      = rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

  CALL ESMF_FieldGet(field     = vs,    &
                     localDE   = 0,     &
                     farrayPtr = vs_Ptr,&
                     rc        = rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

  vs_Ptr = 0
  !---------------------------------------------------------------------------->

  !---------------------------------------------------------------------------->
  ! Calculate remapping route handle for WAV -> OCN
  !
  ! (Note: There is a restriction for remapping of 3-d fields with ungridded 3rd 
  ! dimension - the 2 fields have to have the same 3-rd dimension length)

  CALL FieldRemapStore(srcField      = us_src,     &
                       dstField      = us,         &
                       remapRH       = routeHandle,&
                       vm            = vm,         &
                       remapType     = 'bilinr',   &
                       rc            = rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

  ! Store halo route handle
  CALL ESMF_FieldHaloStore(field       = us,             &
                           routehandle = haloRouteHandle,&
                           haloLDepth  = [1,1],          &
                           haloUDepth  = [1,1],          &
                           rc          = rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

  ! Get the ocean model time step:
  CALL ESMF_ClockGet(clock    = gc(2)%clock,      & !!! UMWM TIME INTERVAL
                     timeStep = hycomTimeInterval,&
                     rc       = rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

  CALL ESMF_TimeIntervalGet(timeinterval = hycomTimeInterval,&
                            s            = hycomTimeStep,    &
                            rc           = rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

  firstTime = .FALSE.

ENDIF
  
!=======================================================================

! remaps u-Stokes drift from UMWM to HYCOM grid
CALL FieldRemap(srcField   = us_src,           &
                dstField   = us,               &
                remapRH    = routeHandle,      &
                zeroregion = ESMF_REGION_TOTAL,&
                checkflag  = .FALSE.,          &
                rc         = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! updates halo points for u-Stokes drift on HYCOM grid
CALL ESMF_FieldHalo(field         = us,                     &
                    routehandle   = haloRouteHandle,        &
                    routesyncflag = ESMF_ROUTESYNC_BLOCKING,&
                    checkflag     = .FALSE.,                &
                    rc            = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! remaps v-Stokes drift from UMWM to HYCOM grid
CALL FieldRemap(srcField   = vs_src,           &
                dstField   = vs,               &
                remapRH    = routeHandle,      &
                zeroregion = ESMF_REGION_TOTAL,&
                checkflag  = .FALSE.,          &
                rc         = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

CALL ESMF_FieldHalo(field         = vs,                     &
                    routehandle   = haloRouteHandle,        &
                    routesyncflag = ESMF_ROUTESYNC_BLOCKING,&
                    checkflag     = .FALSE.,                &
                    rc            = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

!=======================================================================
!
! Remap Stokes drift in the vertical; use HYCOM's internal
! piecewise constant method. 

DO j = jts-haloWidth,jte+haloWidth
  DO i = its-haloWidth,ite+haloWidth

    ii = i-its+1 ! HYCOM local index in x
    jj = j-jts+1 ! HYCOM local index in y

    ! make sure the last input level matches last output level
    p_src(kte+1) = pu(ii,jj,kte+1) 
    dp_src(kte)  = p_src(kte+1)-p_src(kte)

    call hybgen_pcm(us_Ptr(i,j,:),    & ! si
                    p_src(:),         & ! pi
                    dp_src(:),        & ! dpi
                    us_dst(i,j,:),    & ! so
                    pu(ii,jj,1:kte+1),& ! po
                    kte,kte,1,0.)       ! ki,ko,ks,thin

    ! make sure the last input level matches last output level
    p_src(kte+1) = pv(ii,jj,kte+1) 
    dp_src(kte)  = p_src(kte+1)-p_src(kte)

    call hybgen_pcm(vs_Ptr(i,j,:),    & ! si
                    p_src(:),         & ! pi
                    dp_src(:),        & ! dpi
                    vs_dst(i,j,:),    & ! so
                    pv(ii,jj,1:kte+1),& ! po
                    kte,kte,1,0.)       ! ki,ko,ks,thin

  ENDDO
ENDDO

! Mask Stokes drift over land
us_dst = us_dst*seamask
vs_dst = vs_dst*seamask

!=======================================================================

! Update halo regions for HYCOM arrays
!call xctilr(dp(   1-nbdy,1-nbdy,1,1),1,kk, nbdy,nbdy, halo_ps)
!call xctilr(dp(   1-nbdy,1-nbdy,1,2),1,kk, nbdy,nbdy, halo_ps)
!call xctilr(dpu(  1-nbdy,1-nbdy,1,1),1,kk, nbdy,nbdy, halo_us)
!call xctilr(dpu(  1-nbdy,1-nbdy,1,2),1,kk, nbdy,nbdy, halo_us)
!call xctilr(dpv(  1-nbdy,1-nbdy,1,1),1,kk, nbdy,nbdy, halo_vs)
!call xctilr(dpv(  1-nbdy,1-nbdy,1,2),1,kk, nbdy,nbdy, halo_vs)
!call xctilr(u(    1-nbdy,1-nbdy,1,1),1,kk, nbdy,nbdy, halo_uv)
!call xctilr(u(    1-nbdy,1-nbdy,1,2),1,kk, nbdy,nbdy, halo_uv)
!call xctilr(v(    1-nbdy,1-nbdy,1,1),1,kk, nbdy,nbdy, halo_vv)
!call xctilr(v(    1-nbdy,1-nbdy,1,2),1,kk, nbdy,nbdy, halo_vv)
!call xctilr(ubavg(1-nbdy,1-nbdy,  3),1, 1, nbdy,nbdy, halo_uv)
!call xctilr(vbavg(1-nbdy,1-nbdy,  3),1, 1, nbdy,nbdy, halo_vv)
!call xctilr(saln( 1-nbdy,1-nbdy,1,1),1,kk, nbdy,nbdy, halo_ps)
!call xctilr(saln( 1-nbdy,1-nbdy,1,2),1,kk, nbdy,nbdy, halo_ps)
!call xctilr(temp( 1-nbdy,1-nbdy,1,1),1,kk, nbdy,nbdy, halo_ps)
!call xctilr(temp( 1-nbdy,1-nbdy,1,2),1,kk, nbdy,nbdy, halo_ps)

m = mod(nstep  ,2)+1
n = mod(nstep+1,2)+1

! u and v on global indexing (baroclinic + barotropic)
DO k = kts,kte
  DO j = jts-haloWidth-1,jte+haloWidth+1
    DO i = its-haloWidth-1,ite+haloWidth+1

      ut(i,j,k) = u(i-its+1,j-jts+1,k,m)+ubavg(i-its+1,j-jts+1,m)
      vt(i,j,k) = v(i-its+1,j-jts+1,k,m)+vbavg(i-its+1,j-jts+1,m)

    ENDDO
  ENDDO
ENDDO

!=======================================================================
!
! Absolute vorticity at vorticity points.
! Need to have 1-cell wide halo because of staggering relative
! to U and V points.
!
!     +-------+-------+-------+
!     | Q   V | Q   V | Q   V |
! j+1 |       |       |       |
!     | U   P | U   P | U   P |
!     +-------+-------+-------+
!     | Q   V | Q   V | Q   V |
!  j  |       |       |       |
!     | U   P | U   P | U   P |
!     +-------+-------+-------+
!     | Q   V | Q   V | Q   V |
! j-1 |       |       |       |
!     | U   P | U   P | U   P |
!     +-------+-------+-------+
!        i-1      i      i+1
!

DO k = kts,kte
  DO j = jts-haloWidth,jte+haloWidth
    DO i = its-haloWidth,ite+haloWidth

      ii = i-its+1 ! HYCOM local index in x
      jj = j-jts+1 ! HYCOM local index in y

      ! Absolute vorticity
      vort(i,j,k) = (vt(i,j,k)*scvy(ii,jj)-vt(i-1,j,k)*scvy(ii-1,jj) &
                    -ut(i,j,k)*scux(ii,jj)+ut(i,j-1,k)*scux(ii,jj-1))&
                   *scq2i(ii,jj)                                     &
                   +corio(ii,jj)

    ENDDO
  ENDDO
ENDDO

!=======================================================================
!
! u- and v- tendencies

DO k = kts,kte
  DO j = jts,jte
    DO i = its,ite

      ii = i-its+1 ! HYCOM local index in x
      jj = j-jts+1 ! HYCOM local index in y

      ! u and v tendencies in their respective points:
      utend(i,j,k) =  vs_dst(i,j,k)*0.5*(vort(i,j,k)+vort(i,j-1,k))
      vtend(i,j,k) = -us_dst(i,j,k)*0.5*(vort(i,j,k)+vort(i+1,j,k))
      !utend(i,j,k) =  vs_dst(i,j,k)*vort(i,j,k)
      !vtend(i,j,k) = -us_dst(i,j,k)*vort(i,j,k)

      ! Update hycom velocities
      u(ii,jj,k,m) = u(ii,jj,k,m) + hycomTimeStep*utend(i,j,k)
      v(ii,jj,k,m) = v(ii,jj,k,m) + hycomTimeStep*vtend(i,j,k)

    ENDDO
  ENDDO
ENDDO

!=======================================================================
!
! T and S advection by Stokes drift
!
! dT/dt = - us dT/dx - vs dT/dy
!
! dS/dt = - us dS/dx - vs dS/dy
!
!     +-------+-------+-------+
!     | Q   V | Q   V | Q   V |
! j+1 |       |       |       |
!     | U   P | U   P | U   P |
!     +-------+-------+-------+
!     | Q   V | Q   V | Q   V |
!  j  |       |       |       |
!     | U   P | U   P | U   P |
!     +-------+-------+-------+
!     | Q   V | Q   V | Q   V |
! j-1 |       |       |       |
!     | U   P | U   P | U   P |
!     +-------+-------+-------+
!        i-1      i      i+1
!

! Calculate advective fluxes first:
! 1st order upstream, positive-definite 

DO k = kts,kte
  DO j = jts,jte
    DO i = its,ite

      ii = i-its+1 ! HYCOM local index in x
      jj = j-jts+1 ! HYCOM local index in y

      ! x-direction
      feup = (us_dst(i+1,j,k)+ABS(us_dst(i+1,j,k)))*scuy(ii+1,jj)!*dpu(ii+1,jj,k,2)
      fedn = (us_dst(i+1,j,k)-ABS(us_dst(i+1,j,k)))*scuy(ii+1,jj)!*dpu(ii+1,jj,k,2)

      fwup = (us_dst(i,j,k)+ABS(us_dst(i,j,k)))*scuy(ii,jj)!*dpu(ii,jj,k,2)
      fwdn = (us_dst(i,j,k)-ABS(us_dst(i,j,k)))*scuy(ii,jj)!*dpu(ii,jj,k,2)

      ! y-direction
      fnup = (vs_dst(i,j,k)+ABS(vs_dst(i,j,k)))*scvx(ii,jj)!*dpv(ii,jj,k,2)
      fndn = (vs_dst(i,j,k)-ABS(vs_dst(i,j,k)))*scvx(ii,jj)!*dpv(ii,jj,k,2)

      fsup = (vs_dst(i,j-1,k)+ABS(vs_dst(i,j-1,k)))*scvx(ii,jj-1)!*dpv(ii,jj-1,k,2)
      fsdn = (vs_dst(i,j-1,k)-ABS(vs_dst(i,j-1,k)))*scvx(ii,jj-1)!*dpv(ii,jj-1,k,2)

      ! T and S tendencies

      Ttend(i,j,k) = (feup*temp(ii  ,jj  ,k,m)*seamask(i,j,k)  &
                     +fedn*temp(ii+1,jj  ,k,m)*seamask(i+1,j,k)&
                     -fwup*temp(ii-1,jj  ,k,m)*seamask(i-1,j,k)&
                     -fwdn*temp(ii  ,jj  ,k,m)*seamask(i,j,k)  &
                     +fnup*temp(ii  ,jj  ,k,m)*seamask(i,j,k)  &
                     +fndn*temp(ii  ,jj+1,k,m)*seamask(i,j+1,k)&
                     -fsup*temp(ii  ,jj-1,k,m)*seamask(i,j-1,k)&
                     -fsdn*temp(ii  ,jj  ,k,m)*seamask(i,j,k)) &
                     *0.25*scp2i(ii,jj)!/MAX(dp(ii,jj,k,m),1.)
    
      Stend(i,j,k) = (feup*saln(ii  ,jj  ,k,m)*seamask(i,j,k)  &
                     +fedn*saln(ii+1,jj  ,k,m)*seamask(i+1,j,k)&
                     -fwup*saln(ii-1,jj  ,k,m)*seamask(i-1,j,k)&
                     -fwdn*saln(ii  ,jj  ,k,m)*seamask(i,j,k)  &
                     +fnup*saln(ii  ,jj  ,k,m)*seamask(i,j,k)  &
                     +fndn*saln(ii  ,jj+1,k,m)*seamask(i,j+1,k)&
                     -fsup*saln(ii  ,jj-1,k,m)*seamask(i,j-1,k)&
                     -fsdn*saln(ii  ,jj  ,k,m)*seamask(i,j,k)) &
                     *0.25*scp2i(ii,jj)!/MAX(dp(ii,jj,k,m),1.)
    
    ENDDO
  ENDDO
ENDDO

DO k = kts,kte
  DO j = jts,jte
    DO i = its,ite

      ii = i-its+1 ! HYCOM local index in x
      jj = j-jts+1 ! HYCOM local index in y

      ! Update hycom T & S
      !temp(ii,jj,k,2) = temp(ii,jj,k,2)-hycomTimeStep*Ttend(i,j,k)
      !saln(ii,jj,k,2) = saln(ii,jj,k,2)-hycomTimeStep*Stend(i,j,k)

    ENDDO
  ENDDO
ENDDO

! Update halo regions for HYCOM arrays
!call xctilr(dp(   1-nbdy,1-nbdy,1,1),1,kk, nbdy,nbdy, halo_ps)
!call xctilr(dp(   1-nbdy,1-nbdy,1,2),1,kk, nbdy,nbdy, halo_ps)
!call xctilr(dpu(  1-nbdy,1-nbdy,1,1),1,kk, nbdy,nbdy, halo_us)
!call xctilr(dpu(  1-nbdy,1-nbdy,1,2),1,kk, nbdy,nbdy, halo_us)
!call xctilr(dpv(  1-nbdy,1-nbdy,1,1),1,kk, nbdy,nbdy, halo_vs)
!call xctilr(dpv(  1-nbdy,1-nbdy,1,2),1,kk, nbdy,nbdy, halo_vs)
call xctilr(u(    1-nbdy,1-nbdy,1,1),1,kk, nbdy,nbdy, halo_uv)
call xctilr(u(    1-nbdy,1-nbdy,1,2),1,kk, nbdy,nbdy, halo_uv)
call xctilr(v(    1-nbdy,1-nbdy,1,1),1,kk, nbdy,nbdy, halo_vv)
call xctilr(v(    1-nbdy,1-nbdy,1,2),1,kk, nbdy,nbdy, halo_vv)
!call xctilr(ubavg(1-nbdy,1-nbdy,  3),1, 1, nbdy,nbdy, halo_uv)
!call xctilr(vbavg(1-nbdy,1-nbdy,  3),1, 1, nbdy,nbdy, halo_vv)
!call xctilr(saln( 1-nbdy,1-nbdy,1,1),1,kk, nbdy,nbdy, halo_ps)
!call xctilr(saln( 1-nbdy,1-nbdy,1,2),1,kk, nbdy,nbdy, halo_ps)
!call xctilr(temp( 1-nbdy,1-nbdy,1,1),1,kk, nbdy,nbdy, halo_ps)
!call xctilr(temp( 1-nbdy,1-nbdy,1,2),1,kk, nbdy,nbdy, halo_ps)

!=======================================================================

! What is current time?
CALL ESMF_ClockGet(clock    = gc(2)%clock,&
                   currTime = currentTime,&
                   rc       = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! Gets time in ISO 8601 format: YYYY-MM-DDThh:mm:ss
CALL ESMF_TimeGet(time              = currentTime,&
                  timeStringISOFrac = timeStr,    &
                  rc                = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

timeStr(11:11) = '_'

!IF(timeStr(15:19) == '00:00')THEN
IF(.FALSE.)THEN

  IF(localPET == 0)THEN

    stat = nf90_create('vfout_'//timeStr//'.nc',nf90_write,ncid)
    stat = nf90_def_dim(ncid,'x',gc(3)%idm,xdimid)
    stat = nf90_def_dim(ncid,'y',gc(3)%jdm,ydimid)
    stat = nf90_def_dim(ncid,'z',gc(3)%kdm,zdimid)
    stat = nf90_def_var(ncid,'lon',nf90_float,[xdimid,ydimid],lonid)
    stat = nf90_def_var(ncid,'lat',nf90_float,[xdimid,ydimid],latid)
!   stat = nf90_def_var(ncid,'z',nf90_float,[xdimid,ydimid],zid)
    stat = nf90_def_var(ncid,'us',nf90_float,[xdimid,ydimid,zdimid],usid)
    stat = nf90_def_var(ncid,'vs',nf90_float,[xdimid,ydimid,zdimid],vsid)
!    stat = nf90_def_var(ncid,'ws',nf90_float,[xdimid,ydimid,zdimid],wsid)
    stat = nf90_def_var(ncid,'uc',nf90_float,[xdimid,ydimid,zdimid],ucid)
    stat = nf90_def_var(ncid,'vc',nf90_float,[xdimid,ydimid,zdimid],vcid)
!    stat = nf90_def_var(ncid,'dudz',nf90_float,[xdimid,ydimid,zdimid],dudzid)
!    stat = nf90_def_var(ncid,'dvdz',nf90_float,[xdimid,ydimid,zdimid],dvdzid)
    stat = nf90_def_var(ncid,'vorticity',nf90_float,[xdimid,ydimid,zdimid],vortid)
    stat = nf90_def_var(ncid,'utend',nf90_float,[xdimid,ydimid,zdimid],utendid)
    stat = nf90_def_var(ncid,'vtend',nf90_float,[xdimid,ydimid,zdimid],vtendid)
    stat = nf90_def_var(ncid,'Ttend',nf90_float,[xdimid,ydimid,zdimid],ttendid)
    stat = nf90_def_var(ncid,'Stend',nf90_float,[xdimid,ydimid,zdimid],stendid)
    stat = nf90_enddef(ncid)
    stat = nf90_close(ncid)

  ENDIF

  DO n = 0,petCount-1

    IF(localPET == n)THEN

      stat = nf90_open('vfout_'//timeStr//'.nc',nf90_write,ncid)
      stat = nf90_inq_dimid(ncid,'x',xdimid)
      stat = nf90_inq_dimid(ncid,'y',ydimid)
      stat = nf90_inq_dimid(ncid,'z',zdimid)
      stat = nf90_inq_varid(ncid,'lon',lonid)
      stat = nf90_inq_varid(ncid,'lat',latid)
      stat = nf90_inq_varid(ncid,'us',usid)
      stat = nf90_inq_varid(ncid,'vs',vsid)
!      stat = nf90_inq_varid(ncid,'ws',wsid)
      stat = nf90_inq_varid(ncid,'uc',ucid)
      stat = nf90_inq_varid(ncid,'vc',vcid)
!      stat = nf90_inq_varid(ncid,'dudz',dudzid)
!      stat = nf90_inq_varid(ncid,'dvdz',dvdzid)
      stat = nf90_inq_varid(ncid,'vorticity',vortid)
      stat = nf90_inq_varid(ncid,'utend',utendid)
      stat = nf90_inq_varid(ncid,'vtend',vtendid)
      stat = nf90_inq_varid(ncid,'Ttend',ttendid)
      stat = nf90_inq_varid(ncid,'Stend',stendid)
      stat = nf90_put_var(ncid,lonid,plon(1:itc,1:jtc),start=[its,jts],count=[itc,jtc])
      stat = nf90_put_var(ncid,latid,plat(1:itc,1:jtc),start=[its,jts],count=[itc,jtc])
      stat = nf90_put_var(ncid,usid,us_dst(its:ite,jts:jte,kts:kte),start=[its,jts,kts],count=[itc,jtc,ktc])
      stat = nf90_put_var(ncid,vsid,vs_dst(its:ite,jts:jte,kts:kte),start=[its,jts,kts],count=[itc,jtc,ktc])
!      stat = nf90_put_var(ncid,wsid,ws_dst(its:ite,jts:jte,kts:kte),start=[its,jts,kts],count=[itc,jtc,ktc])
      stat = nf90_put_var(ncid,ucid,ut(its:ite,jts:jte,kts:kte),start=[its,jts,kts],count=[itc,jtc,ktc])
      stat = nf90_put_var(ncid,vcid,vt(its:ite,jts:jte,kts:kte),start=[its,jts,kts],count=[itc,jtc,ktc])
      stat = nf90_put_var(ncid,vortid,vort(its:ite,jts:jte,kts:kte),start=[its,jts,kts],count=[itc,jtc,ktc])
!      stat = nf90_put_var(ncid,dudzid,dudz,start=[its,jts,kts],count=[itc,jtc,ktc])
!      stat = nf90_put_var(ncid,dvdzid,dvdz,start=[its,jts,kts],count=[itc,jtc,ktc])
      stat = nf90_put_var(ncid,utendid,utend,start=[its,jts,kts],count=[itc,jtc,ktc])
      stat = nf90_put_var(ncid,vtendid,vtend,start=[its,jts,kts],count=[itc,jtc,ktc])
      stat = nf90_put_var(ncid,ttendid,Ttend,start=[its,jts,kts],count=[itc,jtc,ktc])
      stat = nf90_put_var(ncid,stendid,Stend,start=[its,jts,kts],count=[itc,jtc,ktc])
      stat = nf90_close(ncid)

    ENDIF

    CALL ESMF_VMBarrier(vm)

  ENDDO

ENDIF

rc = ESMF_SUCCESS

ENDSUBROUTINE VortexForce
!===============================================================================



SUBROUTINE meanSeaLevelPressure(psfc,alt,mslp)
!===============================================================================
!
! Calculates mean sea-level pressure using barometric formula
!
!===============================================================================

  REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),INTENT(IN)  :: psfc ! surface pressure [Pa]
  REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),INTENT(IN)  :: alt  ! altitude [m]
  REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),INTENT(OUT) :: mslp ! sea-level pressure [Pa] 

  REAL(KIND=ESMF_KIND_R4),PARAMETER :: L = 0.0065    ! lapse rate [K/m]
  REAL(KIND=ESMF_KIND_R4),PARAMETER :: T0 = 288.15   ! sea-level standard temperature [K]
  REAL(KIND=ESMF_KIND_R4),PARAMETER :: g = 9.80665   ! gravitational acceleration [m/s^2]
  REAL(KIND=ESMF_KIND_R4),PARAMETER :: M = 0.0289644 ! molar mass of dry air [kg/mol]
  REAL(KIND=ESMF_KIND_R4),PARAMETER :: R = 8.31447   ! universal gas constant [J/mol/K]
  
  mslp = psfc/(1-(L*alt/T0))**(g*M/(R*L))

ENDSUBROUTINE meanSeaLevelPressure
!===============================================================================



SUBROUTINE windStressKara(u10,v10,wspd,psfc,ta,ts,taux,tauy)
!===============================================================================
!
! Calculates wind stress vector using Cd formulation by Birol Kara
!
!----------------------------------------------------
! Input arguments:
! 
! u10,v10 :: x and y wind components         [m/s]
! wspd    :: wind speed                      [m/s]
! psfc    :: atmospheric surface pressure    [Pa]
! ta,ts   :: Air and sea surface temperature [K]
!
! Output arguments:
!
! taux,tauy :: x and y wind stress components [N/m^2]
!----------------------------------------------------
!
! Atmospheric stability based on air and sea surface temperature.
! Drag coefficient based on stability and the magnitude of the winds.
! Air density is calculated based on constant atmospheric pressure 
! and air temperature.
!
! NOTE: This is the second formulation for variable Cd devised by B. Kara
!       17 March 2000
!
! Cd = Cd0 + Cd1*(Ts - Ta)
!
! Cd0 = 10E-3*(0.6920 + 0.07100|V| - 0.0007000(|V|)**2)
! Cd1 = 10E-3*(0.0830 - 0.00540|V| + 0.0000930(|V|)**2)
!
! rhoair = p/RT
!
!===============================================================================

! Dummy arguments:
REAL,DIMENSION(:,:),INTENT(IN)  :: u10,v10,wspd,psfc,ta,ts
REAL,DIMENSION(:,:),INTENT(OUT) :: taux,tauy

INTEGER :: i,j
INTEGER :: idm,jdm
INTEGER,DIMENSION(2) :: dims

REAL :: cd,cd0,cd1,rhoair,vmaghat

! Wind speed limits:
REAL,PARAMETER :: wmin = 2.5
REAL,PARAMETER :: wmax = 32.5

! Polynomial coefficients:
REAL,PARAMETER :: a0 =  0.692
REAL,PARAMETER :: a1 =  0.0710
REAL,PARAMETER :: a2 = -0.0007
REAL,PARAMETER :: b0 =  0.083
REAL,PARAMETER :: b1 = -0.0054
REAL,PARAMETER :: b2 =  0.000093

! Gas constant dry air:
REAL,PARAMETER :: gasConst = 287.1

!===============================================================================

dims = SHAPE(u10)
idm = dims(1)
jdm = dims(2)

DO j=1,jdm
  DO i=1,idm

     vmaghat = MAX(wmin,MIN(wmax,wspd(i,j)))
     cd0     = a0+a1*vmaghat+a2*vmaghat**2
     cd1     = b0+b1*vmaghat+b2*vmaghat**2
     cd      = 1E-3*(cd0+cd1*(ts(i,j)-ta(i,j)))
     rhoair  = psfc(i,j)/(gasConst*(ta(i,j)))

     taux(i,j) = rhoair*cd*wspd(i,j)*u10(i,j)
     tauy(i,j) = rhoair*cd*wspd(i,j)*v10(i,j)

  ENDDO
ENDDO

ENDSUBROUTINE windStressKara
!===============================================================================



      subroutine hybgen_pcm(si,pi,dpi,so,po,ki,ko,ks,thin)
!
      integer,intent(in) :: ki,ko,ks
      real,intent(in)    :: si(ki,ks),pi(ki+1),dpi(ki),thin
      real(kind=8),intent(in)    :: po(ko+1)
      real,intent(out)   :: so(ko,ks)
!
!-----------------------------------------------------------------------
!  1) remap from one set of vertical cells to another.
!     method: piecewise constant across each input cell
!             the output is the average of the interpolation
!             profile across each output cell.
!
!     PCM (donor cell) is the standard 1st order upwind method.
!
!  2) input arguments:
!       si    - initial scalar fields in pi-layer space
!       pi    - initial layer interface depths (non-negative)
!                  pi(   1) is the surface
!                  pi(ki+1) is the bathymetry
!                  pi(k+1) >= pi(k)
!       dpi   - initial layer thicknesses (dpi(k)=pi(k+1)-pi(k))
!       ki    - number of  input layers
!       ko    - number of output layers
!       ks    - number of fields
!       po    - target interface depths (non-negative)
!                  po(   1) is the surface
!                  po(ko+1) is the bathymetry (== pi(ki+1))
!                  po(k+1) >= po(k)
!       thin  - layer thickness (>0) that can be ignored
!
!  3) output arguments:
!       so    - scalar fields in po-layer space
!
!  4) Tim Campbell, Mississippi State University, October 2002.
!     Alan J. Wallcraft,  Naval Research Laboratory,  Aug. 2007.
!-----------------------------------------------------------------------
!
      integer :: i,k,l,lb,lt
      real    :: dpb,dpt,xb,xt,zb,zt,zx,o
      real    :: sz
!
      zx=pi(ki+1) !maximum depth
      zb=max(po(1),pi(1))
      lb=1
      do while (pi(lb+1).lt.zb .and. lb.lt.ki)
        lb=lb+1
      enddo
      do k= 1,ko  !output layers
        zt = zb
        zb = min(po(k+1),zx)
        lt=lb !top will always correspond to bottom of previous
              !find input layer containing bottom output interface
        do while (pi(lb+1).lt.zb .and. lb.lt.ki)
          lb=lb+1
        enddo
        if     (zb-zt.le.thin .or. zt.ge.zx) then
          if     (k.ne.1) then
!
! ---       thin or bottomed layer, values taken from layer above
!
            do i= 1,ks
              so(k,i) = so(k-1,i)
            enddo !i
          else !thin surface layer
            do i= 1,ks
              so(k,i) = si(k,i)
            enddo !i
          endif
        else
!
!         form layer averages.
!         use PPM-like logic (may not have minimum operation count)
!
          if     (lt.ne.lb) then  !multiple layers
            xt=(zt-pi(lt))/max(dpi(lt),thin)
            xb=(zb-pi(lb))/max(dpi(lb),thin)
            dpt=pi(lt+1)-zt
            dpb=zb-pi(lb)
            do i= 1,ks
              o  = si((lt+lb)/2,i)  !offset to reduce round-off
              sz = dpt*(si(lt,i)-o)
              do l=lt+1,lb-1
                sz = sz+dpi(l)*(si(l,i)-o)
              enddo !l
              sz = sz+dpb*(si(lb,i)-o)
              so(k,i) = o + sz/(zb-zt)  !zb-zt>=thin
            enddo !i
          else  !single layer
            do i= 1,ks
              so(k,i) = si(lt,i)
            enddo !i
          endif
        endif !thin:std layer
      enddo !k
      end subroutine hybgen_pcm
!===============================================================================
ENDMODULE UWIN_physics
