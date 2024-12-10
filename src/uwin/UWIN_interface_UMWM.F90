MODULE UWIN_interface_UMWM
!===============================================================================

USE ESMF
USE mpi
USE UWIN_GriddedComponent
USE UWIN_global
USE UWIN_utility,ONLY:GridRotation
USE UMWM_top,    ONLY:umwm_initialize,umwm_run,umwm_finalize

!===============================================================================
IMPLICIT NONE

PRIVATE

PUBLIC :: SetServices

!===============================================================================
CONTAINS



SUBROUTINE umwm_component_init(gcomp,importState,exportState,clock,rc)
!===============================================================================
USE UMWM_module,ONLY:mm,nm,istart,iend,mi,ni,&
                     umwm_lon  => lon,       &
                     umwm_lat  => lat,       &
                     umwm_mask => mask
USE UMWM_stokes,ONLY:depth,us,vs,km => lm
!===============================================================================

! ARGUMENTS
TYPE(ESMF_GridComp) :: gcomp
TYPE(ESMF_State)    :: importState
TYPE(ESMF_State)    :: exportState
TYPE(ESMF_Clock)    :: clock
INTEGER,INTENT(OUT) :: rc

INTEGER :: i,j,k,n

INTEGER :: pet

INTEGER :: ierr
INTEGER :: status,ncid,varid

INTEGER :: src,tgt

TYPE(ESMF_Grid)     :: grid,grid3d
TYPE(ESMF_DistGrid) :: distgrid

REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),ALLOCATABLE :: alpha

REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),POINTER :: Xcoord,Ycoord
REAL(KIND=ESMF_KIND_R4),DIMENSION(:),  POINTER :: Zcoord

INTEGER(KIND=ESMF_KIND_I4),DIMENSION(:,:),POINTER :: mask_ptr

INTEGER,DIMENSION(1) :: lb1,ub1 ! Lower and upper bounds on grid
INTEGER,DIMENSION(2) :: lb,ub   ! Lower and upper bounds on grid
INTEGER,DIMENSION(3) :: lb3,ub3 ! Lower and upper bounds on grid3d

INTEGER :: ips,ipe,jps,jpe
INTEGER :: ids,ide,jds,jde

INTEGER(KIND=ESMF_KIND_I4),DIMENSION(:,:),  ALLOCATABLE :: tileDims
INTEGER(KIND=ESMF_KIND_I4),DIMENSION(:,:,:),ALLOCATABLE :: deBlockList

CHARACTER(LEN=256) :: couplingIntervalString

LOGICAL :: lonIsContinuous = .TRUE.

REAL :: t0,t1

!===============================================================================

! UMWM initialize routine:
CALL umwm_initialize()

! Figure out if longitude field is continuous:
IF(MINVAL(umwm_lon) < -175 .AND. MAXVAL(umwm_lon) > 175.)THEN
  ! Safe to assume:
  lonIsContinuous = .FALSE.
ENDIF

! Take care of discontinuity acros longitude:
IF(.NOT.lonIsContinuous)THEN
  WHERE(umwm_lon < 0)umwm_lon = umwm_lon+360
ENDIF

ALLOCATE(alpha(mm,nm))

alpha = GridRotation(umwm_lon,umwm_lat,rc=rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! Figure out tile dimensions:
IF(mm >= nm)THEN ! Row-major remapping 

  ips = mi(istart)
  ipe = mi(iend)
  jps = 1
  jpe = nm
  IF(localPet == 0)ips = ips-1
  IF(localPet == petCount-1)ipe = ipe+1

ELSEIF(mm < nm)THEN ! Column-major remapping

  ips = 1
  ipe = mm
  jps = ni(istart)
  jpe = ni(iend)
  IF(localPet==0)jps = jps-1
  IF(localPet==petCount-1)jpe = jpe+1

ENDIF

ALLOCATE(tileDims(4,petCount))

! First gather to root processor:
CALL ESMF_VMGather(vm       = VM,                    &
                   sendData = [ips,ipe,jps,jpe],     &
                   recvData = tileDims(:,localPet+1),&
                   count    = 4,                     &
                   rootPET  = 0,                     &
                   rc       = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! ESMF_VM communication routines seem to be broken, 
! use standard MPI instead:
CALL MPI_Bcast(tileDims,SIZE(tileDims),MPI_INTEGER,0,mpicomm,ierr)

! Report tile dimensions to standard output:
DO pet=1,petCount
  WRITE(*,*)pet,tileDims(:,pet)
ENDDO

! Now that we have domain decomposition information, we can 
! easily create a deLayout and deBlockList:
ALLOCATE(deBlockList(2,2,petCount)) ! (dimCount,2,deCount)
DO pet=1,petCount !             I                J
  deBlockList(:,1,pet) = [tileDims(1,pet),tileDims(3,pet)] ! START
  deBlockList(:,2,pet) = [tileDims(2,pet),tileDims(4,pet)] ! END
ENDDO

! Create wave distgrid:
distgrid = ESMF_DistGridCreate(minIndex    = [1,1],            &
                               maxIndex    = [mm,nm],          &
                               deBlockList = deBlockList,      &
                               indexflag   = ESMF_INDEX_GLOBAL,&
                               vm          = vm,               &
                               rc          = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

DEALLOCATE(deBlockList)

! Use distgrid to create grid:
grid = ESMF_GridCreate(name          = 'UMWM grid',          &
                       distGrid      = distgrid,             &
                       coordsys      = ESMF_COORDSYS_SPH_DEG,&
                       coordTypeKind = ESMF_TYPEKIND_R4,     &
                       coordDimCount = [2,2],                &
                       indexflag     = ESMF_INDEX_GLOBAL,    &
                       rc            = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! Add coordinates:
CALL ESMF_GridAddCoord(grid       = grid,                  &
                       staggerloc = ESMF_STAGGERLOC_CENTER,&
                       rc         = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! Set up coordinates:
CALL ESMF_GridGetCoord(grid            = grid,                  &
                       localDE         = 0,                     &
                       CoordDim        = 1,                     &
                       staggerloc      = ESMF_STAGGERLOC_CENTER,&
                       exclusiveLBound = lb,                    &
                       exclusiveUBound = ub,                    &
                       farrayPtr       = Xcoord,                &
                       rc              = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)
FORALL(i=lb(1):ub(1),j=lb(2):ub(2))Xcoord(i,j) = umwm_lon(i,j)

CALL ESMF_GridGetCoord(grid            = grid,                  &
                       localDE         = 0,                     &
                       CoordDim        = 2,                     &
                       staggerloc      = ESMF_STAGGERLOC_CENTER,&
                       exclusiveLBound = lb,                    &
                       exclusiveUBound = ub,                    &
                       farrayPtr       = Ycoord,                &
                       rc              = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)
FORALL(i=lb(1):ub(1),j=lb(2):ub(2))Ycoord(i,j) = umwm_lat(i,j)

! Add mask:
CALL ESMF_GridAddItem(grid       = grid,                  &
                      staggerloc = ESMF_STAGGERLOC_CENTER,&
                      itemflag   = ESMF_GRIDITEM_MASK,    &
                      rc         = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

CALL ESMF_GridGetItem(grid       = grid,                  &
                      localDE    = 0,                     &
                      staggerloc = ESMF_STAGGERLOC_CENTER,&
                      itemflag   = ESMF_GRIDITEM_MASK,    &
                      farrayPtr  = maskfarrayPtr(2)%Ptr,  &
                      rc         = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)
FORALL(i=lb(1):ub(1),j=lb(2):ub(2))maskfarrayPtr(2)%Ptr(i,j) = 1-umwm_mask(i,j)

!==============================================================================

! TEMPORARY HACK:
gc(2) % distgrid = distgrid
gc(2) % grid     = grid
gc(2) % lon      = umwm_lon
gc(2) % lat      = umwm_lat
gc(2) % mask     = 1-umwm_mask
gc(2) % alpha    = alpha

CALL gc(2) % createFields(arraySpec=arrspec2DR4,rc=rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

gc(2)%kdm = 12

gc(2)%auxFieldName(1) = 'sdcu'
gc(2)%auxFieldName(2) = 'sdcv'

isw = gc(2)%its; iew = gc(2)%ite
jsw = gc(2)%jts; jew = gc(2)%jte

! Create Stokes drift auxilliary fields:

!------------------------------------------------------------------------------>
! Stokes drift, x-component:
gc(2)%auxField(1) = ESMF_FieldCreate(grid            = gc(2) % grid,          &
                                     arrayspec       = arrspec3DR4,           &
                                     indexflag       = ESMF_INDEX_GLOBAL,     &
                                     staggerLoc      = ESMF_STAGGERLOC_CENTER,&
                                     totalLwidth     = [1,1],                 &
                                     totalUwidth     = [1,1],                 &
                                     ungriddedLBound = [1],                   &
                                     ungriddedUBound = [gc(2) % kdm],         &
                                     name            = 'sdcu',                &
                                     rc              = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

CALL ESMF_FieldGet(field     = gc(2)%auxField(1),       &
                   localDE   = 0,                       &
                   farrayPtr = gc(2)%auxFieldPtr(1)%Ptr,&
                   rc        = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

gc(2)%auxFieldPtr(1)%Ptr = 0

!------------------------------------------------------------------------------>
! Stokes drift, y-component:
gc(2)%auxField(2) = ESMF_FieldCreate(grid            = gc(2) % grid,          &
                                     arrayspec       = arrspec3DR4,           &
                                     indexflag       = ESMF_INDEX_GLOBAL,     &
                                     staggerLoc      = ESMF_STAGGERLOC_CENTER,&
                                     totalLwidth     = [1,1],                 &
                                     totalUwidth     = [1,1],                 &
                                     ungriddedLBound = [1],                   &
                                     ungriddedUBound = [gc(2) % kdm],         &
                                     name            = 'sdcv',                &
                                     rc              = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! Initialize:
CALL ESMF_FieldGet(field     = gc(2)%auxField(2),       &
                   localDE   = 0,                       &
                   farrayPtr = gc(2)%auxFieldPtr(2)%Ptr,&
                   rc        = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

gc(2)%auxFieldPtr(2)%Ptr = 0

!==============================================================================

CALL exportFields(rc=rc)

ENDSUBROUTINE umwm_component_init
!===============================================================================



SUBROUTINE umwm_component_run(gcomp,importState,exportState,clock,rc)
!===============================================================================

! ARGUMENTS
TYPE(ESMF_GridComp) :: gcomp
TYPE(ESMF_State)    :: importState
TYPE(ESMF_State)    :: exportState
TYPE(ESMF_Clock)    :: clock
INTEGER,INTENT(OUT) :: rc

! Timing:
TYPE(ESMF_Time)         :: umwmStartTime,umwmStopTime
TYPE(ESMF_TimeInterval) :: timeStep
CHARACTER(LEN=19)       :: umwmStartTimeStr,umwmStopTimeStr

REAL :: t0,t1

!===============================================================================

! Get start time and time step:
CALL ESMF_ClockGet(clock    = clock,        &
                   currTime = umwmStartTime,&
                   timeStep = timeStep,     &
                   rc       = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

umwmStopTime = umwmStartTime+timeStep

! Gets time in ISO 8601 format: YYYY-MM-DDThh:mm:ss
CALL ESMF_TimeGet(time              = umwmStartTime,   &
                  timeStringISOFrac = umwmStartTimeStr,&
                  rc                = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

CALL ESMF_TimeGet(time              = umwmStopTime,   &
                  timeStringISOFrac = umwmStopTimeStr,&
                  rc                = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! Get rid of timezone character to switch to UMWM string format:
umwmStartTimeStr(11:11) = ' '
umwmStopTimeStr(11:11)  = ' '

CALL importFields(rc=rc)
CALL umwm_run(umwmStartTimeStr,umwmStopTimeStr)
CALL exportFields(rc=rc)

ENDSUBROUTINE umwm_component_run
!===============================================================================



SUBROUTINE umwm_component_finalize(gcomp,importState,exportState,clock,rc)
!===============================================================================

! ARGUMENTS
TYPE(ESMF_GridComp) :: gcomp
TYPE(ESMF_State)    :: importState
TYPE(ESMF_State)    :: exportState
TYPE(ESMF_Clock)    :: clock
INTEGER,INTENT(OUT) :: rc

!===============================================================================

CALL umwm_finalize()

ENDSUBROUTINE umwm_component_finalize
!===============================================================================



SUBROUTINE importFields(rc)
!==============================================================================>
!
! Imports wave model fields into ESMF_Field objects.
!
!==============================================================================>

USE UMWM_module,ONLY:mm,nm,        &
                     mi,ni,ii,     &
                     istart,iend,  &
                     iistart,iiend,&
                     wspd,wdir,    &
                     uc,vc,        &
                     psim,         &
                     rhoa,rhow,rhoa0,rhow0,rhorat     
USE mod_hycom,ONLY:thbase
USE UWIN_physics,ONLY:stability

!===============================================================================

INTEGER,INTENT(OUT),OPTIONAL :: rc

REAL,DIMENSION(isw:iew,jsw:jew) :: umwm_wndspd,umwm_wnddir
REAL,DIMENSION(isw:iew,jsw:jew) :: umwm_rhoa,umwm_rhow

REAL :: t0,t1

REAL(KIND=ESMF_KIND_R4) :: cosalpha,sinalpha

INTEGER :: i,j,ierr

!===============================================================================

! Wind speed and direction from the atmosphere component:
umwm_wndspd(isw:iew,jsw:jew) = SQRT(gc(2) % impFieldPtr(1,1) % Ptr(isw:iew,jsw:jew)**2.&
                                   +gc(2) % impFieldPtr(2,1) % Ptr(isw:iew,jsw:jew)**2.)

umwm_wnddir(isw:iew,jsw:jew) = ATAN2(gc(2) % impFieldPtr(2,1) % Ptr(isw:iew,jsw:jew),&
                                     gc(2) % impFieldPtr(1,1) % Ptr(isw:iew,jsw:jew))

! Air density from the atmosphere component:
umwm_rhoa(isw:iew,jsw:jew) = gc(2) % impFieldPtr(3,1) % Ptr(isw:iew,jsw:jew)

! Water density from the ocean component:
umwm_rhow(isw:iew,jsw:jew) = gc(2) % impFieldPtr(3,3) % Ptr(isw:iew,jsw:jew)+1000+thbase

! If no density data from HYCOM, set to 1029 kg/m^3
FORALL(i=isw:iew,j=jsw:jew,gc(2) % impFieldPtr(3,3) % Ptr(i,j) == 0)
  umwm_rhow(i,j) = 1029.
ENDFORALL

! Remap to model space:
DO i = istart,iend
  wspd(i) = umwm_wndspd(mi(i),ni(i))
  wdir(i) = umwm_wnddir(mi(i),ni(i))
  rhoa(i) = umwm_rhoa  (mi(i),ni(i))
  rhow(i) = umwm_rhow  (mi(i),ni(i))
  psim(i) = gc(2) % impFieldPtr(4,1) % Ptr(mi(i),ni(i))
ENDDO

! Set density ratio:
rhorat = rhoa/rhow

! Calculate stability at lamda/2
CALL stability(psim10 = gc(2) % impField(4,1),&
               rmol   = gc(2) % impField(5,1),&
               rc     = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

IF(oceanAdvectsWaves)THEN

  ! Update Halo region:
  CALL ESMF_FieldHalo(field         = gc(2)%impField(1,3),        &
                      routehandle   = gc(2)%impHaloRouteHandle(1),& 
                      routesyncflag = ESMF_ROUTESYNC_BLOCKING,    &
                      checkflag     = .FALSE.,                    & 
                      rc            = rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

  CALL ESMF_FieldHalo(field         = gc(2)%impField(2,3),        &
                      routehandle   = gc(2)%impHaloRouteHandle(1),& 
                      routesyncflag = ESMF_ROUTESYNC_BLOCKING,    &
                      checkflag     = .FALSE.,                    & 
                      rc            = rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)
  
  ! Remap to model space:
  DO i = iistart,iiend

    ! Unrotated:
    !uc(i) = gc(2) % impFieldPtr(1,3) % Ptr(mi(i),ni(i))
    !vc(i) = gc(2) % impFieldPtr(2,3) % Ptr(mi(i),ni(i))

    ! Rotating back from spherical to Cartesian means
    ! we use negative angle for rotation
    cosalpha = COS(-gc(2) % alpha(mi(i),ni(i)))
    sinalpha = SIN(-gc(2) % alpha(mi(i),ni(i)))

    uc(i) = gc(2) % impFieldPtr(1,3) % Ptr(mi(i),ni(i))*cosalpha&
          - gc(2) % impFieldPtr(2,3) % Ptr(mi(i),ni(i))*sinalpha
    vc(i) = gc(2) % impFieldPtr(1,3) % Ptr(mi(i),ni(i))*sinalpha&
          + gc(2) % impFieldPtr(2,3) % Ptr(mi(i),ni(i))*cosalpha

  ENDDO

ELSE

  uc = 0.
  vc = 0.

ENDIF ! oceanAdvectsWaves

ENDSUBROUTINE importFields
!==============================================================================>



SUBROUTINE exportFields(rc)
!==============================================================================>
!
! Exports wave model fields into ESMF_Field objects.
!
!==============================================================================>

USE UMWM_module,ONLY:istart,iend,mi,ni,mm,nm,&
                     taux_form,tauy_form,    &
                     taux_skin,tauy_skin,    &
                     taux_ocntop,tauy_ocntop,&
                     taux_snl,tauy_snl

USE UMWM_stokes,ONLY:us,vs,km => lm

INTEGER,INTENT(OUT),OPTIONAL :: rc

INTEGER :: i,j,k,m,n

REAL(KIND=ESMF_KIND_R4),DIMENSION(isw:iew,jsw:jew) :: umwm_taux,umwm_tauy

INTEGER,SAVE :: waveTimeStep = 1

!===============================================================================

! 1) First export wind stress (skin + form drag. for ATM):

! Must initialize to 0 first, because the loop below fills in only active 
! wave model points (sea-points minus domain edges)
IF(modelIsEnabled(1))THEN
  
  umwm_taux = 0.
  umwm_tauy = 0.
  
  ! Remap from UMWM space:
  DO i = istart,iend
    umwm_taux(mi(i),ni(i)) = taux_form(i)+taux_skin(i)
    umwm_tauy(mi(i),ni(i)) = tauy_form(i)+tauy_skin(i)
  ENDDO
  
  ! Fix edges (which are 0 from UMWM):
  IF(localPet == 0)THEN
    umwm_taux(1,:)   = umwm_taux(2,:)
    umwm_taux(:,1)   = umwm_taux(:,2) 
    umwm_taux(:,jew) = umwm_taux(:,jew-1)
    umwm_tauy(1,:)   = umwm_tauy(2,:)
    umwm_tauy(:,1)   = umwm_tauy(:,2) 
    umwm_tauy(:,jew) = umwm_tauy(:,jew-1)
  ELSEIF(localPet == petCount-1)THEN
    umwm_taux(iew,:) = umwm_taux(iew-1,:)
    umwm_taux(:,1)   = umwm_taux(:,2) 
    umwm_taux(:,jew) = umwm_taux(:,jew-1)
    umwm_tauy(iew,:) = umwm_tauy(iew-1,:)
    umwm_tauy(:,1)   = umwm_tauy(:,2) 
    umwm_tauy(:,jew) = umwm_tauy(:,jew-1)
  ELSE
    umwm_taux(:,1)   = umwm_taux(:,2) 
    umwm_taux(:,jew) = umwm_taux(:,jew-1)
    umwm_tauy(:,1)   = umwm_tauy(:,2) 
    umwm_tauy(:,jew) = umwm_tauy(:,jew-1)
  ENDIF

  gc(2) % expFieldPtr(1,1) % Ptr(:,:) = 0
  gc(2) % expFieldPtr(2,1) % Ptr(:,:) = 0
  gc(2) % expFieldPtr(3,1) % Ptr(:,:) = 0
  gc(2) % expFieldPtr(4,1) % Ptr(:,:) = 0
  
  ! Atmospheric stress
  gc(2) % expFieldPtr(1,1) % Ptr(isw:iew,jsw:jew) = umwm_taux(isw:iew,jsw:jew)
  gc(2) % expFieldPtr(2,1) % Ptr(isw:iew,jsw:jew) = umwm_tauy(isw:iew,jsw:jew)

  ! Surface Stokes drift
  gc(2) % expFieldPtr(3,1) % Ptr(:,:) = 0
  gc(2) % expFieldPtr(4,1) % Ptr(:,:) = 0

  DO i = istart,iend
    gc(2) % expFieldPtr(3,1) % Ptr(mi(i),ni(i)) = us(i,1)
    gc(2) % expFieldPtr(4,1) % Ptr(mi(i),ni(i)) = vs(i,1)
  ENDDO

ENDIF

! 2) Export stress into ocean top (skin drag + wave breaking, for OCN):

IF(modelIsEnabled(3))THEN

  umwm_taux = 0.
  umwm_tauy = 0.

  ! Remap from UMWM space:
  DO i = istart,iend

    ! tau_ocntop already contains momentum loss due to Snl transfer;
    ! This momentum is supposed to go into currents and the exact 
    ! amount is stored in tau_snl; 
    umwm_taux(mi(i),ni(i)) = taux_ocntop(i)-taux_snl(i)
    umwm_tauy(mi(i),ni(i)) = tauy_ocntop(i)-tauy_snl(i)

  ENDDO

  ! Fix edges (which are 0 from UMWM):
  IF(localPet == 0)THEN
    umwm_taux(1,:)   = umwm_taux(2,:)
    umwm_taux(:,1)   = umwm_taux(:,2)
    umwm_taux(:,jew) = umwm_taux(:,jew-1)
    umwm_tauy(1,:)   = umwm_tauy(2,:)
    umwm_tauy(:,1)   = umwm_tauy(:,2)
    umwm_tauy(:,jew) = umwm_tauy(:,jew-1)
  ELSEIF(localPet == petCount-1)THEN
    umwm_taux(iew,:) = umwm_taux(iew-1,:)
    umwm_taux(:,1)   = umwm_taux(:,2)
    umwm_taux(:,jew) = umwm_taux(:,jew-1)
    umwm_tauy(iew,:) = umwm_tauy(iew-1,:)
    umwm_tauy(:,1)   = umwm_tauy(:,2)
    umwm_tauy(:,jew) = umwm_tauy(:,jew-1)
  ELSE
    umwm_taux(:,1)   = umwm_taux(:,2)
    umwm_taux(:,jew) = umwm_taux(:,jew-1)
    umwm_tauy(:,1)   = umwm_tauy(:,2)
    umwm_tauy(:,jew) = umwm_tauy(:,jew-1)
  ENDIF

  ! Constrain values during initial spin-up
  IF(waveTimeStep < 5)THEN
    umwm_taux = 0
    umwm_tauy = 0
  ENDIF
  
  ! UMWM expFields for HYCOM:
  !gc(2) % expFieldPtr(1,3) % Ptr(isw:iew,jsw:jew) = umwm_taux(isw:iew,jsw:jew)
  !gc(2) % expFieldPtr(2,3) % Ptr(isw:iew,jsw:jew) = umwm_tauy(isw:iew,jsw:jew)

  ! Rotate the stress vector onto HYCOM grid:
  gc(2) % expFieldPtr(1,3) % Ptr(:,:) = 0
  gc(2) % expFieldPtr(2,3) % Ptr(:,:) = 0
  DO j = jsw,jew
    DO i = isw,iew
      
      gc(2) % expFieldPtr(1,3) % Ptr(i,j) = umwm_taux(i,j)*COS(gc(2) % alpha(i,j))&
                                          - umwm_tauy(i,j)*SIN(gc(2) % alpha(i,j))
      gc(2) % expFieldPtr(2,3) % Ptr(i,j) = umwm_taux(i,j)*SIN(gc(2) % alpha(i,j))&
                                          + umwm_tauy(i,j)*COS(gc(2) % alpha(i,j))

    ENDDO
  ENDDO

  ! Stokes drift velocities:
  gc(2) % auxFieldPtr(1) % Ptr(:,:,:) = 0
  gc(2) % auxFieldPtr(2) % Ptr(:,:,:) = 0
  DO k = 1,km
    DO i = istart,iend
      gc(2) % auxFieldPtr(1) % Ptr(mi(i),ni(i),k) = us(i,k)
      gc(2) % auxFieldPtr(2) % Ptr(mi(i),ni(i),k) = vs(i,k)
    ENDDO
  ENDDO

ENDIF

waveTimeStep = waveTimeStep + 1

ENDSUBROUTINE exportFields
!==============================================================================>




SUBROUTINE SetServices(gridComp,rc)
!===============================================================================

TYPE(ESMF_GridComp) :: gridComp
INTEGER,INTENT(OUT) :: rc

!===============================================================================

CALL ESMF_GridCompSetEntryPoint(gridcomp    = gridComp,              &
                                methodflag  = ESMF_METHOD_INITIALIZE,&
                                userRoutine = umwm_component_init,   &
                                rc          = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

CALL ESMF_GridCompSetEntryPoint(gridcomp    = gridComp,          &
                                methodflag  = ESMF_METHOD_RUN,   &
                                userRoutine = umwm_component_run,&
                                rc          = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

CALL ESMF_GridCompSetEntryPoint(gridcomp    = gridComp,               &
                                methodflag  = ESMF_METHOD_FINALIZE,   &
                                userRoutine = umwm_component_finalize,&
                                rc          = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

ENDSUBROUTINE SetServices
!===============================================================================
ENDMODULE UWIN_interface_UMWM
