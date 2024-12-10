MODULE UWIN_interface_WRF
!===============================================================================

USE ESMF
USE UWIN_GriddedComponent
USE UWIN_constants
USE UWIN_global
USE UWIN_utility,      ONLY:GridRotation,nc_check
USE module_wrf_top,    ONLY:wrf_init,wrf_run,wrf_finalize
USE module_domain,     ONLY:get_ijk_from_grid,head_grid
USE module_domain_type,ONLY:domain

!===============================================================================
IMPLICIT NONE

LOGICAL,DIMENSION(10) :: wrfDomExists
LOGICAL,DIMENSION(10) :: wrfDomHasStopped

TYPE domain_ptr
  TYPE(domain),POINTER :: Ptr
ENDTYPE domain_ptr

TYPE(domain_ptr),DIMENSION(3) :: dom

CONTAINS
!===============================================================================



SUBROUTINE wrf_component_init(gcomp,importState,exportState,clock,rc)
!===============================================================================
USE mpi
USE netcdf
!===============================================================================

! ARGUMENTS
TYPE(ESMF_GridComp) :: gcomp
TYPE(ESMF_State)    :: importState
TYPE(ESMF_State)    :: exportState
TYPE(ESMF_Clock)    :: clock
INTEGER,INTENT(OUT) :: rc

INTEGER :: i,j,n
INTEGER :: pet

INTEGER :: ierr
INTEGER :: status,ncid,varid

! Time
TYPE(ESMF_Time) :: startTime
TYPE(ESMF_Time) :: stopTime
TYPE(ESMF_TimeInterval) :: couplingInterval

TYPE(ESMF_Grid)     :: wrf_grid
TYPE(ESMF_DistGrid) :: wrf_distgrid

! Decomposition
INTEGER :: ids,ide,jds,jde,kds,kde ! Total domain space
INTEGER :: ims,ime,jms,jme,kms,kme ! Tile domain space w/ halo points
INTEGER :: ips,ipe,jps,jpe,kps,kpe ! Tile domain space w/o halo points

REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),ALLOCATABLE :: alpha

REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),ALLOCATABLE :: wrf_lon,wrf_lat,wrf_tsk
INTEGER(KIND=ESMF_KIND_I4),DIMENSION(:,:),ALLOCATABLE :: wrf_mask

REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),POINTER :: Xcoord,Ycoord

INTEGER(KIND=ESMF_KIND_I4),DIMENSION(:,:),POINTER :: mask_ptr

INTEGER,DIMENSION(2) :: lb,ub ! Lower and upper bound

INTEGER(KIND=ESMF_KIND_I4),DIMENSION(:,:),ALLOCATABLE   :: tile_dims
INTEGER(KIND=ESMF_KIND_I4),DIMENSION(:,:,:),ALLOCATABLE :: deBlockList

INTEGER :: src,tgt

CHARACTER(LEN=256) :: couplingIntervalString

LOGICAL :: lonIsContinuous = .TRUE.

!===============================================================================

! Initialize domain switches
wrfDomExists     = .FALSE.
wrfDomExists(1)  = .TRUE.
wrfDomHasStopped = .FALSE.

CALL wrf_set_dm_communicator(mpicomm)
CALL wrf_init

!===============================================================================

dom(1)%Ptr => head_grid ! Convenience pointer

! Get domain size and tile dimensions in order to generate ESMF deBlockList:
CALL get_ijk_from_grid(head_grid,              &
                       ids,ide,jds,jde,kds,kde,&
                       ims,ime,jms,jme,kms,kme,&
                       ips,ipe,jps,jpe,kps,kpe)

! WRF returns staggered dimensions here; we want non-staggered, 
! so adjust accordingly:
ide = ide-1
jde = jde-1
ipe = MIN(ide,ipe)
jpe = MIN(jde,jpe)

ALLOCATE(tile_dims(4,petCount))

! First gather to root processor:
CALL ESMF_VMGather(vm       = vm,                     &
                   sendData = [ips,ipe,jps,jpe],      &
                   recvData = tile_dims(:,localPet+1),&
                   count    = 4,                      &
                   rootPET  = 0,                      &
                   rc       = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! ESMF_VM communication routines seem to be broken, 
! use standard MPI instead:
CALL MPI_Bcast(tile_dims,SIZE(tile_dims),MPI_INTEGER,0,mpicomm,ierr)

! Everybody report information that we shared:
WRITE(*,*)'WRF tiling (ips,ipe,jps,jpe)'
DO pet=1,petCount
  WRITE(*,*)pet,tile_dims(:,pet)
ENDDO

! Now that we have domain decomposition information, we can 
! easily create a deLayout and deBlockList:
ALLOCATE(deBlockList(2,2,petCount)) ! (dimCount,2,deCount)
DO pet=1,petCount !             I                J
  deBlockList(:,1,pet)=[tile_dims(1,pet),tile_dims(3,pet)] ! START
  deBlockList(:,2,pet)=[tile_dims(2,pet),tile_dims(4,pet)] ! END
ENDDO

!===============================================================================

! Allocate some arrays to read from file:
ALLOCATE(wrf_lon(ids:ide,jds:jde))
ALLOCATE(wrf_lat(ids:ide,jds:jde))
ALLOCATE(wrf_mask(ids:ide,jds:jde))
ALLOCATE(wrf_tsk(ids:ide,jds:jde))

! Read WRF input file to get grid information:
CALL nc_check(nf90_open('wrfinput_d01',nf90_nowrite,ncid))
CALL nc_check(nf90_inq_varid(ncid,'XLONG',varid))
CALL nc_check(nf90_get_var(ncid,varid,wrf_lon))
CALL nc_check(nf90_inq_varid(ncid,'XLAT',varid))
CALL nc_check(nf90_get_var(ncid,varid,wrf_lat))
CALL nc_check(nf90_inq_varid(ncid,'LANDMASK',varid))
CALL nc_check(nf90_get_var(ncid,varid,wrf_mask))
CALL nc_check(nf90_inq_varid(ncid,'TSK',varid))
CALL nc_check(nf90_get_var(ncid,varid,wrf_tsk))
CALL nc_check(nf90_close(ncid))

! Figure out if longitude field is continuous:
IF(MINVAL(wrf_lon) < -175 .AND. MAXVAL(wrf_lon) > 175)THEN
  ! Safe to assume:
  lonIsContinuous = .FALSE.
ENDIF

! Take care of discontinuity acros longitude:
IF(.NOT. lonIsContinuous)THEN
  WHERE(wrf_lon < 0)wrf_lon = wrf_lon + 360
ENDIF

ALLOCATE(alpha(idma,jdma))

alpha = GridRotation(wrf_lon,wrf_lat,rc=rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

!===============================================================================

! Create atm distgrid:
wrf_distgrid = ESMF_DistGridCreate(minIndex    = [ids,jds],        &
                                   maxIndex    = [ide,jde],        &
                                   deBlockList = deBlockList,      &
                                   indexflag   = ESMF_INDEX_GLOBAL,&
                                   vm          = vm,               & 
                                   rc          = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! Use distgrid to create grid:
wrf_grid = ESMF_GridCreate(name          = 'WRF grid',           &
                           coordsys      = ESMF_COORDSYS_SPH_DEG,&
                           distGrid      = wrf_distgrid,         &
                           coordTypeKind = ESMF_TYPEKIND_R4,     &
                           coordDimCount = [2,2],                &
                           indexflag     = ESMF_INDEX_GLOBAL,    &
                           rc            = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! Add coordinates:
CALL ESMF_GridAddCoord(grid       = wrf_grid,              &
                       staggerloc = ESMF_STAGGERLOC_CENTER,&
                       rc         = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! Set up coordinates:
CALL ESMF_GridGetCoord(grid            = wrf_grid,              &
                       localDE         = 0,                     &
                       CoordDim        = 1,                     &
                       staggerloc      = ESMF_STAGGERLOC_CENTER,&
                       exclusiveLBound = lb,                    &
                       exclusiveUBound = ub,                    &
                       farrayPtr       = Xcoord,                &
                       rc              = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)
FORALL(i=lb(1):ub(1),j=lb(2):ub(2))Xcoord(i,j) = wrf_lon(i,j)

CALL ESMF_GridGetCoord(grid            = wrf_grid,              &
                       localDE         = 0,                     &
                       CoordDim        = 2,                     &
                       staggerloc      = ESMF_STAGGERLOC_CENTER,&
                       exclusiveLBound = lb,                    &
                       exclusiveUBound = ub,                    &
                       farrayPtr       = Ycoord,                &
                       rc              = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)
FORALL(i=lb(1):ub(1),j=lb(2):ub(2))Ycoord(i,j) = wrf_lat(i,j)

! Add mask:
CALL ESMF_GridAddItem(grid       = wrf_grid,              &
                      staggerloc = ESMF_STAGGERLOC_CENTER,&
                      itemflag   = ESMF_GRIDITEM_MASK,    &
                      rc         = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

CALL ESMF_GridGetItem(grid       = wrf_grid,              &
                      localDE    = 0,                     &
                      staggerloc = ESMF_STAGGERLOC_CENTER,&
                      itemflag   = ESMF_GRIDITEM_MASK,    &
                      farrayPtr  = maskfarrayPtr(1)%Ptr,  &
                      rc         = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)
FORALL(i=lb(1):ub(1),j=lb(2):ub(2))maskfarrayPtr(1)%Ptr(i,j) = wrf_mask(i,j)

!==============================================================================

! TEMPORARY HACK:

gc(1) % distgrid = wrf_distgrid
gc(1) % grid     = wrf_grid
gc(1) % lon      = wrf_lon 
gc(1) % lat      = wrf_lat 
gc(1) % mask     = wrf_mask
gc(1) % alpha    = alpha

CALL gc(1) % createFields(arraySpec=arrspec2DR4,rc=rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

isa = gc(1) % its
iea = gc(1) % ite
jsa = gc(1) % jts
jea = gc(1) % jte

! Create 2D auxiliary fields:
DO i = 1,numWRFAuxFields2D,1
    
    gc(1)%auxField2D(i) = ESMF_FieldCreate(grid        = gc(1) % grid,           &
                                           arrayspec   = arrspec2DR4,            &
                                           indexflag   = ESMF_INDEX_GLOBAL,      &
                                           staggerLoc  = ESMF_STAGGERLOC_CENTER, &
                                           totalLwidth = [1,1],                  &
                                           totalUwidth = [1,1],                  &
                                           name        = WRFauxField2DName(i),   &
                                           rc          = rc)
    IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

    CALL ESMF_FieldGet(field     = gc(1)%auxField2D(i),        &
                       localDE   = 0,                          &
                       farrayPtr = gc(1)%auxField2DPtr(i)%Ptr, &
                       rc        = rc)
    IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

    gc(1)%auxField2DPtr(i)%Ptr = 0

END DO

CALL exportFields(rc=rc)

ENDSUBROUTINE wrf_component_init
!===============================================================================



SUBROUTINE wrf_component_run(gcomp,importState,exportState,clock,rc)
!===============================================================================
USE module_utility,ONLY:WRFU_Time

! ARGUMENTS
TYPE(ESMF_GridComp) :: gcomp
TYPE(ESMF_State)    :: importState
TYPE(ESMF_State)    :: exportState
TYPE(ESMF_Clock)    :: clock
INTEGER,INTENT(OUT) :: rc

INTEGER :: i,j

! timing
TYPE(ESMF_Time)         :: currentTime,wrfRunStartTime,wrfRunStopTime
TYPE(ESMF_TimeInterval) :: timeStep

TYPE(WRFU_Time) :: WRFU_StartTime,WRFU_StopTime

CHARACTER(LEN=256) :: wrfTimeString_start
CHARACTER(LEN=256) :: wrfTimeString_stop

!===============================================================================

! Get current time and time step to generate WRF start and stop time:
CALL ESMF_ClockGet(clock    = clock,      &
                   currTime = currentTime,&
                   timeStep = timeStep,   &
                   rc       = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

wrfRunStartTime = currentTime
wrfRunStopTime  = currentTime+timeStep

! Gets time in ISO 8601 format: YYYY-MM-DDThh:mm:ss[.f]
CALL ESMF_TimeGet(time              = wrfRunStartTime,    &
                  timeStringISOFrac = wrfTimeString_start,&
                  rc                = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

CALL ESMF_TimeGet(time              = wrfRunStopTime,    &
                  timeStringISOFrac = wrfTimeString_stop,&
                  rc                = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! Get rid of timezone character to switch to WRF string format:
wrfTimeString_start(11:11) = '_'
wrfTimeString_stop(11:11)  = '_'

! Create WRFU_Time objects from WRF time strings: 
CALL wrf_atotime(wrfTimeString_start,WRFU_StartTime)
CALL wrf_atotime(wrfTimeString_stop,WRFU_StopTime)

! Assign to head_grid:
head_grid%start_subtime = WRFU_StartTime
head_grid%stop_subtime  = WRFU_StopTime

CALL wrf_run()
CALL exportFields(rc=rc)

ENDSUBROUTINE wrf_component_run
!===============================================================================



SUBROUTINE wrf_component_finalize(gcomp,importState,exportState,clock,rc)
!===============================================================================

! ARGUMENTS
TYPE(ESMF_GridComp) :: gcomp
TYPE(ESMF_State)    :: importState
TYPE(ESMF_State)    :: exportState
TYPE(ESMF_Clock)    :: clock
INTEGER,INTENT(OUT) :: rc

!===============================================================================

! Call WRF "finalize" routine, suppressing call to MPI_FINALIZE so 
! ESMF can do it (if needed) during ESMF_Finalize().  
CALL wrf_finalize(no_shutdown = .TRUE.)

rc = ESMF_SUCCESS

ENDSUBROUTINE wrf_component_finalize
!==============================================================================>



SUBROUTINE exportFields(rc)
!==============================================================================>
!
! Exports atmosphere model fields into ESMF_Field objects.
!
!==============================================================================>

!USE module_sf_sfclay,ONLY:uwin_vconvc_water
USE UWIN_physics,    ONLY:meanSeaLevelPressure
USE mod_hycom,       ONLY:flxflg
USE module_state_description, ONLY:P_QV

INTEGER,INTENT(OUT),OPTIONAL :: rc

!REAL(KIND=ESMF_KIND_R4),DIMENSION(isa:iea,jsa:jea) :: altitude
!REAL(KIND=ESMF_KIND_R4),DIMENSION(isa:iea,jsa:jea) :: mslp
REAL(KIND=ESMF_KIND_R4),DIMENSION(isa:iea,jsa:jea) :: radiative_flux
REAL(KIND=ESMF_KIND_R4),DIMENSION(isa:iea,jsa:jea) :: enthalpy_flux
REAL(KIND=ESMF_KIND_R4),DIMENSION(isa:iea,jsa:jea) :: wspd
REAL(KIND=ESMF_KIND_R4),DIMENSION(isa:iea,jsa:jea) :: vconv
REAL(KIND=ESMF_KIND_R4),DIMENSION(isa:iea,jsa:jea) :: t2v
REAL(KIND=ESMF_KIND_R4),DIMENSION(isa:iea,jsa:jea) :: sstv
REAL(KIND=ESMF_KIND_R4),DIMENSION(isa:iea,jsa:jea) :: u_LML,v_LML,w_LML

REAL(KIND=ESMF_KIND_R4) :: ue,ve,tvcon

INTEGER :: i,j

!---------------------------------------------------------------------------->

rc = ESMF_FAILURE

! WRF -> HYCOM:

IF(modelIsEnabled(3))THEN

  ! Surface air temperature (airtmp)
  gc(1) % expFieldPtr(1,3) % Ptr(isa:iea,jsa:jea) = head_grid % t2(isa:iea,jsa:jea)

  ! Surface water vapor mixing ratio (vapmix)
  gc(1) % expFieldPtr(2,3) % Ptr(isa:iea,jsa:jea) = head_grid % q2(isa:iea,jsa:jea)

  ! SW radiative heat flux (shwflx)
  gc(1) % expFieldPtr(3,3) % Ptr(isa:iea,jsa:jea) = &
       head_grid % swdown(isa:iea,jsa:jea)*(1.-head_grid % albedo(isa:iea,jsa:jea))

  ! Surface wind speed (wndspd)
  wspd(isa:iea,jsa:jea) = SQRT(head_grid % u10(isa:iea,jsa:jea)**2&
                             + head_grid % v10(isa:iea,jsa:jea)**2)

  gc(1) % expFieldPtr(7,3) % Ptr(isa:iea,jsa:jea) = wspd(isa:iea,jsa:jea)

  DO j = jsa,jea
    DO i = isa,iea

      ! positive downward (into the ocean)
      radiative_flux(i,j) = head_grid % swdown(i,j) * (1-head_grid % albedo(i,j)) & !SW absorptivity = 1 - albedo
                          + head_grid % glw(i,j) * head_grid % emiss(i,j) & !Kirchhoff's Law, LW absorptivity = emissivity
                          - head_grid % emiss(i,j) * sigma * head_grid % tsk(i,j)**4

      ! positive downward (into the ocean)  
      enthalpy_flux(i,j) = -head_grid % hfx(i,j)-head_grid % lh(i,j)

    ENDDO
  ENDDO


  ! VCONV modulation
!  IF(.NOT. vconvOcean == vconvAtmosphere)THEN
!
!    DO j = jsa,jea
!      DO i = isa,iea
!
!        tvcon = 1+ep1*head_grid % q2(i,j)
!
!        t2v(i,j) = head_grid % t2(i,j)*tvcon
!        sstv(i,j) = head_grid % tsk(i,j)*tvcon
!
!      ENDDO
!    ENDDO
!
!    vconv = 0
!    FORALL(i=isa:iea,j=jsa:jea,sstv(i,j) > t2v(i,j))
!      vconv(i,j) = 2*SQRT(sstv(i,j)-t2v(i,j))
!    ENDFORALL
!
!    enthalpy_flux = enthalpy_flux * (wspd+vconvOcean*vconv)&
!                                  / (wspd+vconvAtmosphere*vconv)
!
!  ENDIF

  gc(1) % expFieldPtr(4,3) % Ptr(isa:iea,jsa:jea) = &
    radiative_flux(isa:iea,jsa:jea)+enthalpy_flux(isa:iea,jsa:jea)

  ! Precipitation rate (precip BK: This is now precip - evap. Use with 'empflg' = 3 in blkdat.input.)
  gc(1) % expFieldPtr(5,3) % Ptr(isa:iea,jsa:jea) = (head_grid % raincv(isa:iea,jsa:jea) &
                                                     +head_grid % rainncv(isa:iea,jsa:jea) &
                                                     -head_grid % qfx(isa:iea,jsa:jea)) &
                                                   / head_grid % time_step*1E-3

  ! Skin temperature (not necessary when coupled, but here for completeness) 
  ! (surtmp)
  gc(1) % expFieldPtr(6,3) % Ptr(isa:iea,jsa:jea) = head_grid% tsk(isa:iea,jsa:jea)

  DO j = jsa,jea
    DO i = isa,iea
 
      ! Rotate the wind vector from (x,y) -> (east,north)
      ue = head_grid % u10(i,j)*COS(gc(1) % alpha(i,j))&
         - head_grid % v10(i,j)*SIN(gc(1) % alpha(i,j))
      ve = head_grid % u10(i,j)*SIN(gc(1) % alpha(i,j))&
         + head_grid % v10(i,j)*COS(gc(1) % alpha(i,j))

      ! Eastward wind stress (tauewd)
      gc(1) % expFieldPtr(8,3) % Ptr(i,j) = head_grid % ust(i,j)**2*ue&
           /(gc(1) % expFieldPtr(7,3) % Ptr(i,j)*head_grid % alt(i,1,j))

      ! Northward wind stress (taunwd)
      gc(1) % expFieldPtr(9,3) % Ptr(i,j) = head_grid % ust(i,j)**2*ve&
           /(gc(1) % expFieldPtr(7,3) % Ptr(i,j)*head_grid % alt(i,1,j))

    ENDDO
  ENDDO
   
  !=========================================================================
  ! TODO sea-level pressure coupling with HYCOM
  ! Sea level pressure (mslprs)
  !altitude = (head_grid % ph_2(isa:iea,1,jsa:jea)&
  !           +head_grid % phb(isa:iea,1,jsa:jea))*invgrav
  !
  !CALL meanSeaLevelPressure(head_grid % psfc(isa:iea,jsa:jea),altitude,mslp)
  !gc(1) % expFieldPtr(10,3) % Ptr(isa:iea,jsa:jea) = mslp(isa:iea,jsa:jea)
  !=========================================================================

ENDIF ! modelIsEnabled(3)

!---------------------------------------------------------------------------->
! WRF -> UMWM:

IF(modelIsEnabled(2))THEN

  ! x-component of wind:
  gc(1)%expFieldPtr(1,2)%Ptr(isa:iea,jsa:jea) = head_grid%u10(isa:iea,jsa:jea)

  ! y-component of wind:
  gc(1)%expFieldPtr(2,2)%Ptr(isa:iea,jsa:jea) = head_grid%v10(isa:iea,jsa:jea)

  ! Air density:
  gc(1)%expFieldPtr(3,2)%Ptr(isa:iea,jsa:jea) = 1./head_grid%alt(isa:iea,1,jsa:jea)

  ! Universal stability function for momentum at 10-m height, Psim(10/L):
  gc(1)%expFieldPtr(4,2)%Ptr(isa:iea,jsa:jea) = head_grid%psim_esmf(isa:iea,jsa:jea)

  ! Inverse Monin-Obukhov length, 1/L:
  gc(1)%expFieldPtr(5,2)%Ptr(isa:iea,jsa:jea) = head_grid%rmol(isa:iea,jsa:jea)

  ! Height of lowest model level (uses Geopot Height on Z-staggered grid):
  gc(1)%expFieldPtr(6,2)%Ptr(isa:iea,jsa:jea) = &
          0.5*(head_grid%ph_2(isa:iea,1,jsa:jea) + head_grid%phb(isa:iea,1,jsa:jea)&
             + head_grid%ph_2(isa:iea,2,jsa:jea) + head_grid%phb(isa:iea,2,jsa:jea))/9.81    ! [m]

  ! Windspeed at lowest model level (uses horizontally-staggered windspeeds):
  u_LML(isa:iea,jsa:jea) = 0.5*(head_grid%u_2(isa:iea,1,jsa:jea) + head_grid%u_2(isa+1:iea+1,1,jsa:jea))
  v_LML(isa:iea,jsa:jea) = 0.5*(head_grid%v_2(isa:iea,1,jsa:jea) + head_grid%v_2(isa:iea,1,jsa+1:jea+1))
  gc(1)%expFieldPtr(7,2)%Ptr(isa:iea,jsa:jea) = SQRT(u_LML(isa:iea,jsa:jea)**2&
                                                   + v_LML(isa:iea,jsa:jea)**2)    ! [m s-1]

  ! Potential temperature at lowest model level:
  gc(1)%expFieldPtr(8,2)%Ptr(isa:iea,jsa:jea) = head_grid%t_2(isa:iea,1,jsa:jea) + 300.    ! Pot temp (dry) [K]
  
  ! Specific humidity at lowest model level:
  w_LML(isa:iea,jsa:jea) = head_grid%moist(isa:iea,1,jsa:jea,P_QV)    ! Mixing ratio [kg kg-1]
  gc(1)%expFieldPtr(9,2)%Ptr(isa:iea,jsa:jea) = w_LML(isa:iea,jsa:jea)/&
                                                (1.0 + w_LML(isa:iea,jsa:jea))    ! [kg kg-1]

  ! Surface pressure:
  gc(1)%expFieldPtr(10,2)%Ptr(isa:iea,jsa:jea) = head_grid%psfc(isa:iea,jsa:jea)    ! [Pa]

  ! Sea surface temperature (duplicate of expField (6,3) for running in AW mode):
  gc(1)%expFieldPtr(11,2)%Ptr(isa:iea,jsa:jea) = head_grid%tsk(isa:iea,jsa:jea)    ! [K]

ENDIF

rc = ESMF_SUCCESS

ENDSUBROUTINE exportFields
!==============================================================================>



SUBROUTINE SetServices(gridComp,rc)
!===============================================================================

TYPE(ESMF_GridComp) :: gridComp
INTEGER,INTENT(OUT) :: rc

!===============================================================================

CALL ESMF_GridCompSetEntryPoint(gridcomp    = gridComp,               &
                                methodflag  = ESMF_METHOD_INITIALIZE, &
                                userRoutine = wrf_component_init,     &
                                rc          = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

CALL ESMF_GridCompSetEntryPoint(gridcomp    = gridComp,         &
                                methodflag  = ESMF_METHOD_RUN,  &
                                userRoutine = wrf_component_run,&
                                rc          = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

CALL ESMF_GridCompSetEntryPoint(gridcomp    = gridComp,              &
                                methodflag  = ESMF_METHOD_FINALIZE,  &
                                userRoutine = wrf_component_finalize,&
                                rc          = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

ENDSUBROUTINE SetServices
!===============================================================================
ENDMODULE UWIN_interface_WRF
