PROGRAM uwin
!=======================================================================
!
! DESCRIPTION: Main application driver for a coupled 
!              atmosphere-wave-ocean model within the ESMF Framework. 
!              Initializes the framework and gridded components, runs 
!              the gridded components, and finalizes the framework and 
!              gridded components.
!
! REVISION HISTORY:
!
! 2020-01-09 :: Changes to allow for spray-mediated heat flux calculation
! 2016-09-28 :: Changes to uwin.nml
! 2014-06-01 :: Added atmospheric stability coupling for wave growth
! 2014-01-03 :: Added vortex force coupling in UWIN_physics module
! 2013-05-15 :: Upgrade to UWIN_GriddedComponent objects
! 2013-04-01 :: Upgrade to ESMF 6.2.0
! 2012-01-05 :: Added Exchange grid and WRF nest coupling support
! 2011-09-01 :: Working version with COAMPS regridding
! 2011-07-10 :: Origination, ESMF 4.0.0rp2
!
!=======================================================================

USE ESMF
USE NUOPC
USE UWIN_global
USE UWIN_GriddedComponent

USE UWIN_interface_WRF,  ONLY:ATM_SetServices => SetServices
USE UWIN_interface_UMWM, ONLY:WAV_SetServices => SetServices
USE UWIN_interface_HYCOM,ONLY:OCN_SetServices => SetServices
USE UWIN_coupler,        ONLY:CPL_SetServices

!=======================================================================

IMPLICIT NONE

INTEGER :: n
CHARACTER(LEN=1)  :: cn
CHARACTER(LEN=ESMF_MAXSTR) :: ctimer

TYPE(ESMF_CplComp) :: cplComp
TYPE(ESMF_State)   :: cplImpState,cplExpState

TYPE(ESMF_Clock)        :: worldClock
TYPE(ESMF_Time)         :: worldTime
TYPE(ESMF_TimeInterval) :: worldTimeStep

INTEGER,DIMENSION(6) :: startDate,endDate
INTEGER,DIMENSION(2) :: atmdims,wavdims,ocndims
INTEGER,DIMENSION(3) :: timeSteps
INTEGER :: uwinTimeSteps

REAL :: t0,t1

! Return codes for error checks:
INTEGER :: rc

! Main component switches:
NAMELIST /MAIN/ atmosphere,waves,ocean

! Component grid dimensions:
NAMELIST /DIMENSIONS/ atmdims,wavdims,ocndims

! BK read in xgRefinementRatio
NAMELIST /REFINEMENT/ xgRefinementRatio

! BK read in TIME parameters from namelist.
NAMELIST /TIME/ startDate,endDate,timeSteps,uwinTimeSteps

! Coupling switches
NAMELIST /COUPLER/ sstFromOcean,             &
                   currentsFromOcean,        &
                   windStressFromWavesScalar,&
                   windStressFromWavesVector,&
                   oceanStressFromWaves,     &
                   oceanAdvectsWaves,        &
                   waveCurrentInteraction,   &
                   sprayMediatedHeatFluxes

! Spray namelist inputs
NAMELIST /SPRAY/ SSGF_name,SSGF_sourcestrength,spraySubgridFeedback,waitToStartSpray

! Convective velocity switches
NAMELIST /VCONV/ vconvAtmosphere,vconvOcean

!=======================================================================
! Initialize ESMF
!=======================================================================

CALL ESMF_Initialize(defaultCalKind = ESMF_CALKIND_GREGORIAN,&
                     vm             = vm,                    &
                     logkindflag    = ESMF_LOGKIND_MULTI,    &
                     rc             = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

CALL ESMF_VMGet(vm              = vm,      &
                mpiCommunicator = mpicomm, &
                petCount        = petCount,&
                localPET        = localPet,&
                rc              = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

CALL ESMF_LogWrite('Main driver start',ESMF_LOGMSG_INFO,rc=rc)
CALL ESMF_LogFlush()

!=======================================================================
! Gridded component initialization
!=======================================================================
xgRefinementRatio = 3 ! BK default, if it's not in the namelist.
uwinTimeSteps = 60    ! BK default, if it's not in the namelist.

OPEN(UNIT=11,FILE='uwin.nml',STATUS='OLD',FORM='FORMATTED',&
     ACCESS='SEQUENTIAL',ACTION='READ')
READ(UNIT=11,NML=MAIN)
READ(UNIT=11,NML=DIMENSIONS)
READ(UNIT=11,NML=REFINEMENT)
READ(UNIT=11,NML=TIME)
READ(UNIT=11,NML=COUPLER)
READ(UNIT=11,NML=SPRAY)
!READ(UNIT=11,NML=VCONV)
CLOSE(UNIT=11)

! Determine which components are enabled:
modelIsEnabled = [atmosphere,waves,ocean]

! Assign component dimensions to global variables:
idma = atmdims(1)
jdma = atmdims(2)
idmw = wavdims(1)
jdmw = wavdims(2)
idmo = ocndims(1)
jdmo = ocndims(2)

idm = [idma,idmw,idmo]
jdm = [jdma,jdmw,jdmo]

DO n = 1,ngc
  IF(modelIsEnabled(n))THEN

    CALL ESMF_LogWrite('Model component '//TRIM(gridCompName(n))//&
                       ' is enabled',ESMF_LOGMSG_INFO,rc=rc)
    CALL ESMF_LogFlush()

  ENDIF
ENDDO

! Evaluate numExpFields (is a transpose matrix of numImpFields):
numExpFields = TRANSPOSE(numImpFields)

DO n=1,SIZE(expFieldName,DIM=1)
  expFieldName(n,:,:) = TRANSPOSE(impFieldName(n,:,:))
ENDDO

! Allocate UWIN_GriddedComponent objects:
ALLOCATE(gc(ngc))

DO n = 1,ngc

  IF(modelIsEnabled(n))THEN

    CALL ESMF_LogWrite('Initializing GriddedComponents',&
                       ESMF_LOGMSG_INFO)
    CALL ESMF_LogFlush()

    ! Create GriddedComponent:
    CALL gc(n) % create(numGriddedComponents = 3,              &
                        numImpFields         = 12,             &
                        numExpFields         = 12,             &
                        id                   = n,              &
                        name                 = gridCompName(n),&
                        rc                   = rc)
    IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

    ! Pass virtual machine information:
    gc(n) % vm       = vm
    gc(n) % petCount = petCount
    gc(n) % localPet = localPet
    gc(n) % mpicomm  = mpicomm

  ENDIF

ENDDO

!=======================================================================
! ESMF Array specifications   
!=======================================================================

! 4-byte integer, rank=2
CALL ESMF_ArraySpecSet(arrayspec = arrspec2DI4,     &
                       rank      = 2,               &
                       typekind  = ESMF_TYPEKIND_I4,&
                       rc        = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! 4-byte real, rank=2
CALL ESMF_ArraySpecSet(arrayspec = arrspec2DR4,     &
                       rank      = 2,               &
                       typekind  = ESMF_TYPEKIND_R4,&
                       rc        = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! 8-byte real, rank=2
CALL ESMF_ArraySpecSet(arrayspec = arrspec2DR8,     &
                       rank      = 2,               &
                       typekind  = ESMF_TYPEKIND_R8,&
                       rc        = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! 4-byte real, rank=3
CALL ESMF_ArraySpecSet(arrayspec = arrspec3DR4,     &
                       rank      = 3,               &
                       typekind  = ESMF_TYPEKIND_R4,&
                       rc        = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! 8-byte real, rank=3
CALL ESMF_ArraySpecSet(arrayspec = arrspec3DR8,     &
                       rank      = 3,               &
                       typekind  = ESMF_TYPEKIND_R8,&
                       rc        = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

!=======================================================================
! Create and initialize clocks
!=======================================================================

! TODO Make the worldClock more flexible/general in terms of 
! start and stop time and time stepping
! BK: Addressed the time stepping part of this by making it be a namelist variable.

CALL ESMF_TimeIntervalSet(timeinterval = worldTimeStep,&
                          s            = uwinTimeSteps,&
                          rc           = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! Initialize world clock based on namelist:
worldClock = ESMF_ClockCreate(timeStep  = worldTimeStep,  &
                              startTime = gc(1)%startTime,&
                              stopTime  = gc(1)%stopTime, &
                              rc        = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! Create coupler component:
cplComp = ESMF_CplCompCreate(name='AWO coupler',rc=rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

CALL ESMF_LogWrite('Created clocks and gridded components',&
                   ESMF_LOGMSG_INFO)
CALL ESMF_LogFlush()

!=======================================================================
! Initialize import and export states:
!=======================================================================

cplImpState = ESMF_StateCreate(name        = 'Coupler component import state',&
                               stateintent = ESMF_STATEINTENT_IMPORT,         &
                               rc          = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

cplExpState = ESMF_StateCreate(name        = 'Coupler component export state',&
                               stateintent = ESMF_STATEINTENT_EXPORT,         &
                               rc          = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

CALL ESMF_LogWrite('Created import/export states',ESMF_LOGMSG_INFO)
CALL ESMF_LogFlush()

!=======================================================================
! Register components
!=======================================================================

IF(modelIsEnabled(1))THEN
  CALL ESMF_GridCompSetServices(gridcomp    = gc(1)%gridComp, &
                                userRoutine = ATM_SetServices,&
                                rc          = rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)
  CALL ESMF_LogWrite('Registered component '//gridCompName(1),ESMF_LOGMSG_INFO)
  CALL ESMF_LogFlush()
ENDIF

IF(modelIsEnabled(2))THEN
  CALL ESMF_GridCompSetServices(gridcomp    = gc(2)%gridComp, &
                                userRoutine = WAV_SetServices,&
                                rc          = rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)
  CALL ESMF_LogWrite('Registered component '//gridCompName(2),ESMF_LOGMSG_INFO)
  CALL ESMF_LogFlush()
ENDIF

IF(modelIsEnabled(3))THEN
  CALL ESMF_GridCompSetServices(gridcomp    = gc(3)%gridComp, &
                                userRoutine = OCN_SetServices,&
                                rc          = rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)
  CALL ESMF_LogWrite('Registered component '//gridCompName(3),ESMF_LOGMSG_INFO)
  CALL ESMF_LogFlush()
ENDIF

CALL ESMF_CplCompSetServices(cplcomp     = cplComp,        &
                             userRoutine = CPL_SetServices,&
                             rc          = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)
CALL ESMF_LogWrite('Registered coupler component',ESMF_LOGMSG_INFO)
CALL ESMF_LogFlush()

!=======================================================================
! Initialize components
!=======================================================================

DO n = 1,ngc

  IF(gc(n) % isCreated)THEN

    CALL ESMF_GridCompInitialize(gridComp    = gc(n) % gridComp,  &
                                 importState = gc(n) % impState,  &
                                 exportState = gc(n) % expState,  &
                                 clock       = gc(n) % clock,     &
                                 phase       = 1,                 &
                                 syncflag    = ESMF_SYNC_BLOCKING,&
                                 rc          = rc)
    IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

    CALL ESMF_LogWrite('Initialized component '//gridCompName(n),ESMF_LOGMSG_INFO)
    CALL ESMF_LogFlush()

  ENDIF

ENDDO

CALL ESMF_CplCompInitialize(cplComp     = cplComp,           &
                            importState = cplImpState,       &
                            exportState = cplExpState,       &
                            clock       = worldClock,        &   
                            phase       = 1,                 &
                            syncflag    = ESMF_SYNC_BLOCKING,&
                            rc          = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

CALL ESMF_LogWrite('Initialized coupler component',ESMF_LOGMSG_INFO)
CALL ESMF_LogFlush()

!=======================================================================
! Run section     
!=======================================================================

DO WHILE(.NOT. ESMF_ClockIsStopTime(clock=worldClock,rc=rc))

  ! Get real time:
  CALL ESMF_ClockGet(clock    = worldClock,&
                     currTime = worldTime, &
                     rc       = rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

  gc % isActive = .FALSE. ! Assume components are inactive

  ! Check which components are active:
  DO n = 1,ngc

    IF(.NOT. gc(n) % isCreated)CYCLE

    ! Get component currentTime:
    CALL ESMF_ClockGet(clock    = gc(n) % clock,   &
                       currTime = gc(n) % currTime,&
                       rc       = rc)
    IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

    ! If time is right, activate component for coupling:
    IF(gc(n) % currTime <= worldTime)gc(n) % isActive = .TRUE.
    
  ENDDO
  
  ! Run coupler component:
  CALL ESMF_CplCompRun(cplComp     = cplComp,           &
                       importState = cplImpState,       &
                       exportState = cplExpState,       &
                       clock       = worldClock,        &
                       phase       = 1,                 &
                       syncflag    = ESMF_SYNC_BLOCKING,&
                       rc          = rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

  ! Advance coupler time (world time):
  CALL ESMF_ClockAdvance(clock = worldClock,&
                         rc    = rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

  ! Run model components:
  DO n = 1,ngc
    IF(gc(n) % isActive)THEN

      WRITE(UNIT=cn,FMT='(I1)')n

      CALL ESMF_VMBarrier(vm)

      CALL ESMF_LogWrite('Component '//cn//': run sequence start',&
                         ESMF_LOGMSG_INFO)
      CALL ESMF_LogFlush()

      CALL CPU_TIME(t0)

      CALL ESMF_GridCompRun(gridComp    = gc(n) % gridComp,  &
                            importState = gc(n) % impState,  &
                            exportState = gc(n) % expState,  &
                            clock       = gc(n) % clock,     &
                            phase       = 1,                 &
                            syncflag    = ESMF_SYNC_BLOCKING,&
                            rc          = rc)
      IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

      CALL CPU_TIME(t1)

      WRITE(UNIT=ctimer,FMT=*)t1-t0
      CALL ESMF_LogWrite('Component '//cn//': elapsed '//TRIM(ctimer)//' seconds',&
                         ESMF_LOGMSG_INFO)
      CALL ESMF_LogFlush()

      CALL ESMF_ClockAdvance(clock=gc(n) % clock,rc=rc)
      IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

    ENDIF
  ENDDO  

ENDDO
  
! Run coupler component one last time:
CALL ESMF_CplCompRun(cplComp     = cplComp,           &
                     importState = cplImpState,       &
                     exportState = cplExpState,       &
                     clock       = worldClock,        &
                     phase       = 1,                 &
                     syncflag    = ESMF_SYNC_BLOCKING,&
                     rc          = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

CALL ESMF_LogWrite('Run sequence finished',ESMF_LOGMSG_INFO)
CALL ESMF_LogFlush()

!=======================================================================
! Finalize and clean up
!=======================================================================

DO n = 1,ngc

  IF(modelIsEnabled(n))THEN

    CALL ESMF_GridCompFinalize(gridComp    = gc(n) % gridComp,     &
                               importState = gc(n) % impState,     &
                               exportState = gc(n) % expState,     &
                               clock       = gc(n) % clock,        &
                               phase       = 1,                    &
                               syncflag    = ESMF_SYNC_NONBLOCKING,&
                               rc          = rc)
    IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

    CALL ESMF_LogWrite('Finalized component '//gc(n) % name,&
                       ESMF_LOGMSG_INFO)
    CALL ESMF_LogFlush()

  ENDIF

ENDDO

CALL ESMF_CplCompFinalize(cplComp     = cplComp,              &
                          importState = cplImpState,          &
                          exportState = cplExpState,          &
                          clock       = worldClock,           &
                          phase       = 1,                    &
                          syncflag    = ESMF_SYNC_NONBLOCKING,&
                          rc          = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

CALL ESMF_LogWrite('Coupler component finalized',ESMF_LOGMSG_INFO)
CALL ESMF_LogFlush()

!=======================================================================
! Destroy objects
!=======================================================================

! Destroy coupler objects:
CALL ESMF_ClockDestroy(clock=worldClock,rc=rc)
CALL ESMF_StateDestroy(state=cplImpState,rc=rc)
CALL ESMF_StateDestroy(state=cplExpState,rc=rc)
CALL ESMF_CplCompDestroy(cplComp=cplComp,rc=rc)

DO n = 1,ngc

  IF(gc(n) % isCreated)THEN

    ! Destroy GriddedComponent:
    CALL gc(n) % destroy(rc=rc)
    IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

  ENDIF

ENDDO

DEALLOCATE(gc)

CALL ESMF_LogWrite('Objects destroyed',ESMF_LOGMSG_INFO)
CALL ESMF_LogWrite('ESMF Success',ESMF_LOGMSG_INFO)
CALL ESMF_LogFlush()

CALL ESMF_Finalize(rc=rc)

!=======================================================================
ENDPROGRAM uwin
