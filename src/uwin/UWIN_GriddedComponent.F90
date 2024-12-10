#define FILENAME "UWIN_GriddedComponent.F90"

#if defined(SINGLE)
#define _ESMF_KIND_RX _ESMF_KIND_R4
#define ESMF_KIND_RX ESMF_KIND_R4
#define ESMF_TYPEKIND_RX ESMF_TYPEKIND_R4
#else
#define _ESMF_KIND_RX _ESMF_KIND_R8
#define ESMF_KIND_RX ESMF_KIND_R8
#define ESMF_TYPEKIND_RX ESMF_TYPEKIND_R8
#endif

MODULE UWIN_GriddedComponent
!===============================================================================
USE ESMF
USE NUOPC
USE netcdf
!===============================================================================
IMPLICIT NONE

PRIVATE

! Utility arrays of pointer arrays of various types:

! Rank 2, 4-byte integer:
TYPE farrayPtrType2DI4
  INTEGER(KIND=ESMF_KIND_I4),DIMENSION(:,:),POINTER :: Ptr => NULL()
ENDTYPE farrayPtrType2DI4

! Rank 2, 4-byte real:
TYPE farrayPtrType2DR4
  REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),POINTER :: Ptr => NULL()
ENDTYPE farrayPtrType2DR4

! Rank 2, 8-byte real:
TYPE farrayPtrType2DR8 
  REAL(KIND=ESMF_KIND_R8),DIMENSION(:,:),POINTER :: Ptr => NULL()
ENDTYPE farrayPtrType2DR8

! Rank 3, 4-byte integer:
TYPE farrayPtrType3DI4
  INTEGER(KIND=ESMF_KIND_I4),DIMENSION(:,:,:),POINTER :: Ptr => NULL()
ENDTYPE farrayPtrType3DI4

! Rank 3, 4-byte real:
TYPE farrayPtrType3DR4
  REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:,:),POINTER :: Ptr => NULL()
ENDTYPE farrayPtrType3DR4

! Rank 3, 8-byte real:
TYPE farrayPtrType3DR8 
  REAL(KIND=ESMF_KIND_R8),DIMENSION(:,:,:),POINTER :: Ptr => NULL()
ENDTYPE farrayPtrType3DR8

PUBLIC :: GriddedComponent
PUBLIC :: farrayPtrType2DI4
PUBLIC :: farrayPtrType2DR4
PUBLIC :: farrayPtrType2DR8
PUBLIC :: farrayPtrType3DI4
PUBLIC :: farrayPtrType3DR4
PUBLIC :: farrayPtrType3DR8

!=== GRIDDED COMPONENT OBJECT =================================================>
TYPE GriddedComponent

  !- STATUS FLAGS ------------------------------------------------------------->
  LOGICAL :: isCreated   = .FALSE.
  LOGICAL :: isActive    = .FALSE.
  LOGICAL :: isDestroyed = .FALSE.

  !- VIRTUAL MACHINE ---------------------------------------------------------->
  TYPE(ESMF_VM) :: vm
  INTEGER       :: petCount,localPet,mpicomm

  !- GRIDDED COMPONENT IDENTIFICATION ----------------------------------------->
  INTEGER                    :: id
  CHARACTER(LEN=ESMF_MAXSTR) :: name
  CHARACTER(LEN=3)           :: tag

  !- CHILD GRIDDED COMPONENTS ------------------------------------------------->
  TYPE(griddedComponent),DIMENSION(:),POINTER :: nest

  !- SUPERSTRUCTURE OBJECTS --------------------------------------------------->
  TYPE(ESMF_GridComp)     :: gridComp
  TYPE(ESMF_Clock)        :: clock
  TYPE(ESMF_TimeInterval) :: timeStep
  TYPE(ESMF_Time)         :: currTime,startTime,stopTime
  TYPE(ESMF_State)        :: impState,expState

  !- GRID AND TILE PARAMETERS ------------------------------------------------->
  INTEGER :: idm,jdm,kdm

  ! Computational tile dimensions:
  INTEGER :: its,ite
  INTEGER :: jts,jte
  INTEGER :: kts,kte

  ! Total tile dimensions (computational + halo region):
  INTEGER :: its_total,ite_total
  INTEGER :: jts_total,jte_total
  INTEGER :: kts_total,kte_total

  REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),ALLOCATABLE :: lon,lat
  REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),ALLOCATABLE :: alpha
  REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),ALLOCATABLE :: dx,dy

  REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:,:),ALLOCATABLE :: dz

  INTEGER(KIND=ESMF_KIND_I4),DIMENSION(:,:),ALLOCATABLE :: mask
  INTEGER(KIND=ESMF_KIND_I4),DIMENSION(:,:),POINTER     :: maskPtr => NULL()

  !- UTILITY OBJECTS FOR GRID GENERATION -------------------------------------->
  INTEGER(KIND=ESMF_KIND_I4),DIMENSION(:,:),  ALLOCATABLE :: tileDims
  INTEGER(KIND=ESMF_KIND_I4),DIMENSION(:,:,:),ALLOCATABLE :: deBlockList
  TYPE(ESMF_DistGrid)                                     :: distGrid
  TYPE(ESMF_Grid)                                         :: grid

  !- INFRASTRUCTURE OBJECTS --------------------------------------------------->
  INTEGER(KIND=ESMF_KIND_I4) :: numImpFields,numExpFields
  INTEGER(KIND=ESMF_KIND_I4) :: numAuxFields = 3
  INTEGER(KIND=ESMF_KIND_I4) :: numAuxFields2D = 13
  INTEGER(KIND=ESMF_KIND_I4) :: numGriddedComponents

  CHARACTER(LEN=ESMF_MAXSTR),DIMENSION(:,:),ALLOCATABLE :: impFieldName
  CHARACTER(LEN=ESMF_MAXSTR),DIMENSION(:,:),ALLOCATABLE :: expFieldName

  CHARACTER(LEN=ESMF_MAXSTR),DIMENSION(:),ALLOCATABLE :: impBundleName
  CHARACTER(LEN=ESMF_MAXSTR),DIMENSION(:),ALLOCATABLE :: expBundleName

  TYPE(ESMF_Field),      DIMENSION(:,:),ALLOCATABLE :: impField,expField
  TYPE(ESMF_FieldBundle),DIMENSION(:),  ALLOCATABLE :: impBundle,expBundle

  TYPE(ESMF_RouteHandle),DIMENSION(:),ALLOCATABLE :: impRouteHandle
  TYPE(ESMF_RouteHandle),DIMENSION(:),ALLOCATABLE :: expRouteHandle

  TYPE(ESMF_RouteHandle) :: auxRouteHandle
  TYPE(ESMF_RouteHandle) :: aux2DRouteHandle

  TYPE(ESMF_RouteHandle),DIMENSION(:),ALLOCATABLE :: impHaloRouteHandle
  TYPE(ESMF_RouteHandle),DIMENSION(:),ALLOCATABLE :: expHaloRouteHandle

  TYPE(farrayPtrType2DR4),DIMENSION(:,:),ALLOCATABLE :: impFieldPtr
  TYPE(farrayPtrType2DR4),DIMENSION(:,:),ALLOCATABLE :: expFieldPtr

  TYPE(ESMF_Field),          DIMENSION(:),ALLOCATABLE :: auxField
  TYPE(farrayPtrType3DR4),   DIMENSION(:),ALLOCATABLE :: auxFieldPtr
  CHARACTER(LEN=ESMF_MAXSTR),DIMENSION(:),ALLOCATABLE :: auxFieldName

  TYPE(ESMF_Field),          DIMENSION(:),ALLOCATABLE :: auxField2D
  TYPE(farrayPtrType2DR4),   DIMENSION(:),ALLOCATABLE :: auxField2DPtr
  CHARACTER(LEN=ESMF_MAXSTR),DIMENSION(:),ALLOCATABLE :: auxField2DName

  CONTAINS
    
  PROCEDURE :: create 
  PROCEDURE :: createFields 
  PROCEDURE :: currentTime
  PROCEDURE :: currentTimeIsoformat
  PROCEDURE :: destroy
  PROCEDURE :: writeState

ENDTYPE GriddedComponent
!==============================================================================>
CONTAINS



SUBROUTINE create(self,id,numGriddedComponents,numImpFields,numExpFields,name,rc)
!==============================================================================>
!
! GriddedComponent-bound constructor procedure.
!
!==============================================================================>

  CLASS(griddedComponent),   INTENT(INOUT)        :: self
  INTEGER,                   INTENT(IN), OPTIONAL :: id
  INTEGER,                   INTENT(IN), OPTIONAL :: numGriddedComponents
  INTEGER,                   INTENT(IN), OPTIONAL :: numImpFields
  INTEGER,                   INTENT(IN), OPTIONAL :: numExpFields
  CHARACTER(LEN=ESMF_MAXSTR),INTENT(IN), OPTIONAL :: name
  INTEGER,                   INTENT(OUT),OPTIONAL :: rc

  INTEGER,DIMENSION(2) :: atmdims,wavdims,ocndims
  INTEGER,DIMENSION(6) :: startDate,endDate
  INTEGER,DIMENSION(3) :: timeSteps
  INTEGER :: uwinTimeSteps

  CHARACTER(LEN=3),DIMENSION(3),PARAMETER :: tags = ['ATM','WAV','OCN']

  NAMELIST /DIMENSIONS/ atmdims,wavdims,ocndims
  NAMELIST /TIME/       startDate,endDate,timeSteps,uwinTimeSteps

  rc = ESMF_FAILURE

  !---------------------------------------------------------------------------->
  ! Check if already created:

  IF(self%isCreated)THEN
    CALL ESMF_LogSetError(ESMF_FAILURE,                                     &
                          msg        =  'Cannot create GriddedComponent, '//&
                                        'GriddedComponent already created', &
                          line       =  __LINE__,                           &
                          file       = FILENAME,                            &
                          rcToReturn = rc)
    RETURN
  ENDIF

  ! Check if id number is valid:
  IF(id < 1 .OR. id > numGriddedComponents)THEN
    CALL ESMF_LogSetError(ESMF_FAILURE,                                      &
                          msg        =  'Cannot create GriddedComponent, ' //&
                                        'bad value of id; must be greater '//&
                                        'than 0 and less than '            //&
                                        'numGriddedComponents',              &
                          line       =  __LINE__,                            &
                          file       = FILENAME,                             &
                          rcToReturn = rc)
    RETURN
  ENDIF

  self%id   = id       ! id number; must be > 0 and <= numGriddedComponents
  self%name = name     ! GriddedComponent name
  self%tag  = tags(id) ! GriddedComponent tag

  !---------------------------------------------------------------------------->
  ! Read the namelist (uwin.nml):

  OPEN(UNIT=10+id,FILE='uwin.nml',FORM='FORMATTED',&
       ACCESS='SEQUENTIAL',STATUS='OLD',ACTION='READ')
  READ(UNIT=10+id,NML=DIMENSIONS)
  READ(UNIT=10+id,NML=TIME)  
  CLOSE(UNIT=10+id)

  !---------------------------------------------------------------------------->
  ! Assign grid dimensions:

  IF(id == 1)THEN
    self%idm = atmdims(1)
    self%jdm = atmdims(2)
  ELSEIF(id == 2)THEN
    self%idm = wavdims(1)
    self%jdm = wavdims(2)
  ELSEIF(id == 3)THEN
    self%idm = ocndims(1)
    self%jdm = ocndims(2)
  ENDIF

  ! TODO Allow k-dimension to have more than 1 element
  self%kdm = 1  

  !---------------------------------------------------------------------------->
  ! Set start and stop time, and time step:

  CALL ESMF_TimeSet(time = self%startTime,&
                    yy   = startDate(1),  &
                    mm   = startDate(2),  &
                    dd   = startDate(3),  &
                    h    = startDate(4),  &
                    m    = startDate(5),  &
                    s    = startDate(6),  &
                    rc   = rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

  CALL ESMF_TimeSet(time = self%stopTime,&
                    yy   = endDate(1),   &
                    mm   = endDate(2),   &
                    dd   = endDate(3),   &
                    h    = endDate(4),   &
                    m    = endDate(5),   &
                    s    = endDate(6),   &
                    rc   = rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

  CALL ESMF_TimeIntervalSet(timeinterval = self%timeStep,&
                            s            = timeSteps(id),&
                            rc           = rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

  ! Create clock:
  self%clock = ESMF_ClockCreate(timeStep  = self%timeStep, &
                                startTime = self%startTime,&
                                stopTime  = self%stopTime, &
                                rc        = rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

  ! Initialize currentTime:
  self%currTime = self%startTime

  ! Create ESMF_GridComp:
  self%gridComp = ESMF_GridCompCreate(name        = self%name,             &
                                      clock       = self%clock,            &
                                      contextflag = ESMF_CONTEXT_PARENT_VM,&
                                      rc          = rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

  !---------------------------------------------------------------------------->
  ! Create import and export states:

  self%impState = ESMF_StateCreate(name        = self%tag//'_impState',  &
                                   stateintent = ESMF_STATEINTENT_IMPORT,&
                                   rc          = rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

  self%expState = ESMF_StateCreate(name        = self%tag//'_expState',  &
                                   stateintent = ESMF_STATEINTENT_EXPORT,&
                                   rc          = rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

  !---------------------------------------------------------------------------->
  ! Allocate fields and field pointers:

  self%numImpFields = numImpFields
  self%numExpFields = numExpFields

  self%numGriddedComponents = numGriddedComponents
  
  ALLOCATE(self%impFieldName(numImpFields,numGriddedComponents))
  ALLOCATE(self%expFieldName(numExpFields,numGriddedComponents))
  ALLOCATE(self%auxFieldName(self%numAuxFields))
  ALLOCATE(self%auxField2DName(self%numAuxFields2D))

  ALLOCATE(self%impBundleName(numGriddedComponents))
  ALLOCATE(self%expBundleName(numGriddedComponents))

  ALLOCATE(self%impField(numImpFields,numGriddedComponents))
  ALLOCATE(self%expField(numExpFields,numGriddedComponents))
  ALLOCATE(self%auxField(self%numAuxFields))
  ALLOCATE(self%auxField2D(self%numAuxFields2D))

  ALLOCATE(self%impBundle(numGriddedComponents))
  ALLOCATE(self%expBundle(numGriddedComponents))

  ALLOCATE(self%impRouteHandle(numGriddedComponents))
  ALLOCATE(self%expRouteHandle(numGriddedComponents))

  ALLOCATE(self%impHaloRouteHandle(numImpFields))
  ALLOCATE(self%expHaloRouteHandle(numExpFields))

  ALLOCATE(self%impFieldPtr(numImpFields,numGriddedComponents))
  ALLOCATE(self%expFieldPtr(numExpFields,numGriddedComponents))
  ALLOCATE(self%auxFieldPtr(self%numAuxFields))
  ALLOCATE(self%auxField2DPtr(self%numAuxFields2D))

  ALLOCATE(self%lon(self%idm,self%jdm))
  ALLOCATE(self%lat(self%idm,self%jdm))
  ALLOCATE(self%dx(self%idm,self%jdm))
  ALLOCATE(self%dy(self%idm,self%jdm))
  !ALLOCATE(self%dz(self%idm,self%jdm,self%kdm))
  ALLOCATE(self%alpha(self%idm,self%jdm))
  ALLOCATE(self%mask(self%idm,self%jdm))

  self%lon  = 0
  self%lat  = 0
  self%dx   = 0
  self%dy   = 0
  !self%dz   = 0
  self%alpha = 0
  self%mask = 0

  self%isCreated = .TRUE.

  rc = ESMF_SUCCESS

ENDSUBROUTINE create
!==============================================================================>



SUBROUTINE createFields(self,arraySpec,rc)
!==============================================================================>
!
! GriddedComponent-bound procedure. Grid instance must be created before
! calling this routine.
!
!==============================================================================>

  CLASS(griddedComponent),INTENT(INOUT)        :: self
  TYPE(ESMF_ArraySpec),   INTENT(IN)           :: arraySpec
  INTEGER,                INTENT(OUT),OPTIONAL :: rc

  INTEGER :: dst
  INTEGER :: n

  INTEGER,DIMENSION(2) :: lb,ub
  INTEGER,DIMENSION(2) :: lb_total,ub_total

  CHARACTER(LEN=2) :: cn
  CHARACTER(LEN=1) :: cdst

  REAL :: t0,t1

  CALL CPU_TIME(t0)

  rc = ESMF_FAILURE

  !---------------------------------------------------------------------------->
  IF(.NOT. self % isCreated)THEN
    CALL ESMF_LogSetError(ESMF_FAILURE,                                &
                          msg        =  'Cannot create fields, '//     &
                                        'GriddedComponent not created',&
                          line       =  __LINE__,                      &
                          file       = FILENAME,                       &
                          rcToReturn = rc)
    RETURN
  ENDIF

  !---------------------------------------------------------------------------->
  ! Create fields:

  DO dst = 1,self % numGriddedComponents

    ! self cannot be destination component:
    IF(dst == self % id)CYCLE  

    WRITE(UNIT=cdst,FMT='(I1)')dst

    !-------------------------------------------------------------------------->
    ! Import Fields:
    DO n = 1,self % numImpFields

      WRITE(UNIT=cn,FMT='(I2.2)')n

      self % impFieldName(n,dst) = self % tag//'_'//cdst//'_impField_'//cn
 
      ! Create:
      self % impField(n,dst) = ESMF_FieldCreate(                              &
                               grid        = self % grid,                     &
                               arrayspec   = arraySpec,                       &
                               indexflag   = ESMF_INDEX_GLOBAL,               &
                               staggerLoc  = ESMF_STAGGERLOC_CENTER,          &
                               totalLwidth = [1,1],                           &
                               totalUwidth = [1,1],                           &
                               name        = TRIM(self % impFieldName(n,dst)),&
                               rc          = rc)
      IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

      ! Initialize:
      CALL ESMF_FieldGet(field     = self % impField(n,dst),         &
                         localDE   = 0,                              &
                         farrayPtr = self % impFieldPtr(n,dst) % Ptr,&
                         rc        = rc)
      IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)
    
      self % impFieldPtr(n,dst) % Ptr = 0

      ! Store halo route handle:
      CALL ESMF_FieldHaloStore(field       = self % impField(n,dst),      &
                               routehandle = self % impHaloRouteHandle(n),&
                               startregion = ESMF_STARTREGION_EXCLUSIVE,  &
                               rc          = rc)
      IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

    ENDDO

    !-------------------------------------------------------------------------->
    ! Export Fields: 
    DO n = 1,self % numExpFields

      WRITE(UNIT=cn,FMT='(I2.2)')n

      self % expFieldName(n,dst) = self % tag//'_'//cdst//'_expField_'//cn
  
      ! Create:
      self % expField(n,dst) = ESMF_FieldCreate(                              &
                               grid        = self % grid,                     &
                               arrayspec   = arraySpec,                       &
                               indexflag   = ESMF_INDEX_GLOBAL,               &
                               totalLwidth = [1,1],                           &
                               totalUwidth = [1,1],                           &
                               staggerLoc  = ESMF_STAGGERLOC_CENTER,          &
                               name        = TRIM(self % expFieldName(n,dst)),&
                               rc          = rc)
      IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

      ! Initialize:
      CALL ESMF_FieldGet(field     = self%expField(n,dst),       &
                         localDE   = 0,                          &
                         farrayPtr = self%expFieldPtr(n,dst)%Ptr,&
                         rc        = rc)
      IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

      self%expFieldPtr(n,dst)%Ptr = 0

      ! Store halo route handle:
      CALL ESMF_FieldHaloStore(field       = self%expField(n,dst),      &
                               routehandle = self%expHaloRouteHandle(n),&
                               rc          = rc)
      IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

    ENDDO

    !-------------------------------------------------------------------------->
    ! Add fields to states:

    CALL ESMF_StateAdd(state     = self%impState,       &
                       fieldList = self%impField(:,dst),&
                       rc        = rc)
    IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

    CALL ESMF_StateAdd(state     = self%expState,       &
                       fieldList = self%expField(:,dst),&
                       rc        = rc)
    IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

  ENDDO

  !---------------------------------------------------------------------------->
  ! Get tile bounds:

  TILE_BOUNDS: DO dst = 1,self%numGriddedComponents

    IF(.NOT. dst == self%id)THEN

      CALL ESMF_FieldGetBounds(field               = self%expField(1,dst),&
                               localDE             = 0,                   &
                               computationalLbound = lb,                  &
                               computationalUbound = ub,                  &
                               totalLbound         = lb_total,            &
                               totalUbound         = ub_total,            &
                               rc                  = rc)
      IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)
   
      self%its = lb(1)
      self%ite = ub(1)

      self%jts = lb(2)
      self%jte = ub(2)

      self%its_total = lb_total(1)
      self%ite_total = ub_total(1)

      self%jts_total = lb_total(2)
      self%jte_total = ub_total(2)

      ! TODO Allow k-dimension to have more than 1 element
      self%kts = 1
      self%kte = 1

      self%kts_total = 1
      self%kte_total = 1
      
      EXIT TILE_BOUNDS
 
    ENDIF
 
  ENDDO TILE_BOUNDS

  self % isActive = .TRUE.

  WRITE(0,*)
  WRITE(0,*)'GriddedComponent:',TRIM(self%name)
  WRITE(0,*)
  WRITE(0,*)'   Computational region:'
  WRITE(0,*)'         its,ite:',self%its,self%ite
  WRITE(0,*)'         jts,jte:',self%jts,self%jte
  WRITE(0,*)'         kts,kte:',self%kts,self%kte
  WRITE(0,*)
  WRITE(0,*)'   Total region (with halo):'
  WRITE(0,*)'         its_total,ite_total:',self%its_total,self%ite_total
  WRITE(0,*)'         jts_total,jte_total:',self%jts_total,self%jte_total
  WRITE(0,*)'         kts_total,kte_total:',self%kts_total,self%kte_total

  rc = ESMF_SUCCESS

  CALL CPU_TIME(t1)
  WRITE(0,*)
  WRITE(0,*)'Time elapsed:',t1-t0,'seconds'
  

ENDSUBROUTINE createFields
!==============================================================================>



TYPE(ESMF_time) FUNCTION currentTime(self,rc)
!==============================================================================>
!
! Returns current ESMF_time from UWIN_GriddedComponent.
!
!==============================================================================>

  CLASS(griddedComponent),INTENT(IN)           :: self
  INTEGER,                INTENT(OUT),OPTIONAL :: rc
  
  rc = ESMF_FAILURE

  CALL ESMF_ClockGet(clock    = self%clock, &
                     currTime = currentTime,&
                     rc       = rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

  rc = ESMF_SUCCESS

ENDFUNCTION currentTime
!==============================================================================>



CHARACTER(LEN=19) FUNCTION currentTimeIsoformat(self,rc)
!==============================================================================>
!
! Returns current time in ISO 8601 format, YYYY-MM-DDThh:mm:ss.
!
!==============================================================================>

  CLASS(griddedComponent),INTENT(IN)           :: self
  INTEGER,                INTENT(OUT),OPTIONAL :: rc
 
  rc = ESMF_FAILURE

  ! Gets time in ISO 8601 format: YYYY-MM-DDThh:mm:ss
  CALL ESMF_TimeGet(time              = self%currentTime(rc=rc),&
                    timeStringISOFrac = currentTimeIsoformat,   &
                    rc                = rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

  rc = ESMF_SUCCESS

ENDFUNCTION currentTimeIsoformat
!==============================================================================>



SUBROUTINE destroy(self,rc)
!==============================================================================>
!
! GriddedComponent-bound destructor procedure.
!
!==============================================================================>

  CLASS(griddedComponent),INTENT(INOUT)        :: self
  INTEGER,                INTENT(OUT),OPTIONAL :: rc

  rc = ESMF_FAILURE

  !---------------------------------------------------------------------------->
  ! Check if self can't be destroyed:

  IF(.NOT.self%isCreated)THEN
    CALL ESMF_LogSetError(ESMF_FAILURE,                                      &
                          msg        =  'Cannot destroy GriddedComponent, '//&
                                        'GriddedComponent not created',      &
                          line       =  __LINE__,                            &
                          file       = FILENAME,                             &
                          rcToReturn = rc)
    RETURN
  ENDIF

  IF(self%isDestroyed)THEN
    CALL ESMF_LogSetError(ESMF_FAILURE,                                      &
                          msg        =  'Cannot destroy GriddedComponent, '//&
                                        'GriddedComponent already destroyed',&
                          line       =  __LINE__,                            &
                          file       = FILENAME,                             &
                          rcToReturn = rc)
    RETURN
  ENDIF

  !---------------------------------------------------------------------------->
  ! Deallocate allocatables:

  DEALLOCATE(self%impFieldName,self%expFieldName)
  DEALLOCATE(self%auxFieldName)
  DEALLOCATE(self%impBundleName,self%expBundleName)
  DEALLOCATE(self%impField,self%expField,self%auxField)
  DEALLOCATE(self%impBundle,self%expBundle)
  DEALLOCATE(self%impRouteHandle,self%expRouteHandle)
  DEALLOCATE(self%impHaloRouteHandle,self%expHaloRouteHandle)
  DEALLOCATE(self%impFieldPtr,self%expFieldPtr,self%auxFieldPtr)
  DEALLOCATE(self%lon,self%lat,self%alpha,self%mask)
  DEALLOCATE(self%dx,self%dy)
  !DEALLOCATE(self%dz)

  !---------------------------------------------------------------------------->
  ! Destroy ESMF Superstructure objects:

  CALL ESMF_ClockDestroy(clock=self%clock,rc=rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

  CALL ESMF_StateDestroy(state=self%impState,rc=rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

  CALL ESMF_StateDestroy(state=self%expState,rc=rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

  CALL ESMF_GridCompDestroy(gridcomp=self%gridComp,rc=rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

  self%isActive    = .FALSE.
  self%isDestroyed = .TRUE.

  rc = ESMF_SUCCESS

ENDSUBROUTINE destroy
!==============================================================================>



SUBROUTINE writeState(self,rc)
!==============================================================================>
!
! Writes current state (import and export fields) to a netCDF file.
!
!==============================================================================>
  
  CLASS(griddedComponent),INTENT(IN)           :: self
  INTEGER,                INTENT(OUT),OPTIONAL :: rc
 
  CHARACTER(LEN=19)          :: timeStr
  CHARACTER(LEN=ESMF_MAXSTR) :: outputFileName
  INTEGER                    :: rcin 

  INTEGER :: dst,n 

  INTEGER :: status
  INTEGER :: ncid
  INTEGER :: xdimid,ydimid,zdimid
  INTEGER :: lonid,latid,maskid

  INTEGER,DIMENSION(self%numImpFields,self%numGriddedComponents) :: varimpid
  INTEGER,DIMENSION(self%numExpFields,self%numGriddedComponents) :: varexpid

  INTEGER,DIMENSION(self%numAuxFields) :: varauxid

  REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),ALLOCATABLE :: output_data

  REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:,:),ALLOCATABLE :: output_data_3d

  !---------------------------------------------------------------------------->
  ! Check if active:
  IF(.NOT.self%isActive)THEN
    CALL ESMF_LogSetError(ESMF_FAILURE,                                  &
                          msg        =  'Cannot write state to file, '// &
                                        'GriddedComponent is not active',&
                          line       =  __LINE__,                        &
                          file       = FILENAME,                         &
                          rcToReturn = rc)
    RETURN
  ENDIF

  !---------------------------------------------------------------------------->
  ! Gather and write fields to file:
  IF(self%localPet == 0)THEN

    timeStr = self%currentTimeIsoformat(rc=rcin)
    timeStr(11:11) = '_'

    outputFileName = self%tag // '_state_' // timeStr // '.nc'

    status = nf90_create(TRIM(outputFileName),NF90_CLOBBER,ncid)
    status = nf90_def_dim(ncid,'x',self%idm,xdimid)
    status = nf90_def_dim(ncid,'y',self%jdm,ydimid)
    status = nf90_def_dim(ncid,'z',self%kdm,zdimid)
    status = nf90_def_var(ncid,'lon',NF90_FLOAT,[xdimid,ydimid],lonid)
    status = nf90_def_var(ncid,'lat',NF90_FLOAT,[xdimid,ydimid],latid)
    status = nf90_def_var(ncid,'seamask',NF90_INT,[xdimid,ydimid],maskid)

    DO dst = 1,self%numGriddedComponents

      IF(dst == self%id)CYCLE

      DO n = 1,self%numImpFields
        status = nf90_def_var(ncid,TRIM(self%impFieldName(n,dst)),&
                              NF90_FLOAT,[xdimid,ydimid],varimpid(n,dst))
      ENDDO

      DO n = 1,self%numExpFields
        status = nf90_def_var(ncid,TRIM(self%expFieldName(n,dst)),&
                              NF90_FLOAT,[xdimid,ydimid],varexpid(n,dst))
      ENDDO

    ENDDO

    DO n = 1,self%numAuxFields
      status = nf90_def_var(ncid,TRIM(self%auxFieldName(n)),&
                            NF90_FLOAT,[xdimid,ydimid,zdimid],varauxid(n))
    ENDDO

    status = nf90_enddef(ncid)
    status = nf90_put_var(ncid,lonid,self%lon)
    status = nf90_put_var(ncid,latid,self%lat)

  ENDIF

  !---------------------------------------------------------------------------->
  ! Gather and write fields to file:

  ALLOCATE(output_data(self%idm,self%jdm))
  ALLOCATE(output_data_3d(self%idm,self%jdm,self%kdm))

  DO dst = 1,self%numGriddedComponents

    IF(dst == self%id)CYCLE

    DO n = 1,self%numImpFields

      output_data = 0

      CALL ESMF_FieldGather(field   = self%impField(n,dst),&
                            farray  = output_data,         &
                            rootPet = 0,                   &
                            vm      = self%vm,             &
                            rc      = rcin)
      IF(rcin/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rcin,endflag=ESMF_END_ABORT)

      IF(self%localPet == 0)status = nf90_put_var(ncid,varimpid(n,dst),output_data)

    ENDDO

    DO n = 1,self%numExpFields

      CALL ESMF_FieldGather(field   = self%expField(n,dst),&
                            farray  = output_data,         &
                            rootPet = 0,                   &
                            vm      = self%vm,             &
                            rc      = rcin)
      IF(rcin/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rcin,endflag=ESMF_END_ABORT)

      IF(self%localPet == 0)status = nf90_put_var(ncid,varexpid(n,dst),output_data)

    ENDDO

  ENDDO

  DO n = 1,self%numAuxFields

    CALL ESMF_FieldGather(field   = self%auxField(n),&
                          farray  = output_data_3d,  &
                          rootPet = 0,               &
                          vm      = self%vm,         &
                          rc      = rcin)
    IF(rcin/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rcin,endflag=ESMF_END_ABORT)

    IF(self%localPet == 0)status = nf90_put_var(ncid,varauxid(n),output_data_3d)

  ENDDO

  DEALLOCATE(output_data)
  DEALLOCATE(output_data_3d)

  IF(self%localPet == 0)status = nf90_close(ncid)

ENDSUBROUTINE writeState
!==============================================================================>
ENDMODULE UWIN_GriddedComponent
