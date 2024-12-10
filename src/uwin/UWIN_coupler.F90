MODULE UWIN_coupler
!===============================================================================
USE ESMF
USE UWIN_global
USE UWIN_utility
USE UWIN_ExchangeGrid
USE UWIN_physics, ONLY:VortexForce
USE UWIN_spray_interface, ONLY:SprayMedHeatFluxes
USE module_domain,ONLY:head_grid
USE COAMPS_Util,  ONLY:FieldRemapStore,FieldRemap
USE netcdf
!===============================================================================

IMPLICIT NONE

PRIVATE

PUBLIC :: CPL_SetServices

INTEGER :: cnt

TYPE(ESMF_Time)   :: currentTime
CHARACTER(LEN=19) :: currentTimeStr

LOGICAL :: firstTimeStep = .TRUE.

!===============================================================================
CONTAINS



SUBROUTINE CPL_SetServices(cplComp,rc)
!===============================================================================
IMPLICIT NONE

TYPE(ESMF_CplComp)  :: cplComp
INTEGER,INTENT(OUT) :: rc

!===============================================================================

CALL ESMF_CplCompSetEntryPoint(cplcomp     = cplComp,               &
                               methodflag  = ESMF_METHOD_INITIALIZE,&
                               userRoutine = CPL_Init,              &
                               rc          = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

CALL ESMF_CplCompSetEntryPoint(cplcomp     = cplComp,        &
                               methodflag  = ESMF_METHOD_RUN,&
                               userRoutine = CPL_Run,        &
                               rc          = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

CALL ESMF_CplCompSetEntryPoint(cplcomp     = cplComp,             &
                               methodflag  = ESMF_METHOD_FINALIZE,&
                               userRoutine = CPL_Final,           &
                               rc          = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

ENDSUBROUTINE CPL_SetServices
!===============================================================================



SUBROUTINE CPL_Init(cplComp,cplImpState,cplExpState,clock,rc)
!===============================================================================

! ARGUMENTS
TYPE(ESMF_CplComp)  :: cplComp
TYPE(ESMF_State)    :: cplImpState
TYPE(ESMF_State)    :: cplExpState
TYPE(ESMF_Clock)    :: clock
INTEGER,INTENT(OUT) :: rc

INTEGER :: i,j,n
INTEGER :: src,tgt

INTEGER :: landmaskValue

character(len=6) :: remap_option

!===============================================================================

! Send wrf_grid to ESMF_ExchangeGridCreate:
CALL exchangeGridCreate(gc(1)%grid,xgRefinementRatio,rc=rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! Create import and export fields on exchange grid:
CALL exchangeGridFieldCreateAll(rc=rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

CALL ESMF_LogWrite('Exchange grid fields created',ESMF_LOGMSG_INFO)
CALL ESMF_LogFlush()

!=== COMPUTE REMAPPING =========================================================
      
!landmaskValue = 1 ! Extrapolation
landmaskValue = 9 ! No extrapolation

remap_option = 'bilinr'
!remap_option = 'bicubc'

DO src=1,ngc

  IF(.NOT.modelIsEnabled(src))CYCLE
  DO tgt=1,ngc
    IF(.NOT.modelIsEnabled(tgt))CYCLE
    IF(tgt == src)CYCLE

    ! If HYCOM -> WRF:
    IF(src == 3 .AND. tgt == 1)THEN
      ! Use extrapolation to coastline for SST
      !landmaskValue = 1 ! Produces a NaN in some cases
    ELSE
      ! No extrapolation
      landmaskValue = 9
    ENDIF

    ! Remap GriddedComponent -> ExchangeGrid
    CALL FieldRemapStore(srcField      = gc(src)%expField(1,tgt),&
                         dstField      = xg%impField(1,src,tgt), &
                         remapRH       = xg%RHandleIn(src,tgt),  &
                         srcMaskValues = [landmaskValue],        &
                         dstMaskValues = [landmaskValue],        &
                         vm            = vm,                     &
                         remapType     = remap_option,           &
                         rc            = rc)
    IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

    WRITE(0,*)'Finished FieldRemapStore model->xg: src,tgt = ',src,tgt

    ! Remap ExchangeGrid -> GriddedComponent
    CALL FieldRemapStore(srcField      = xg%expField(1,src,tgt), &
                         dstField      = gc(src)%impField(1,tgt),&
                         remapRH       = xg%RHandleOut(src,tgt), &
                         srcMaskValues = [landmaskValue],        &
                         dstMaskValues = [landmaskValue],        &
                         vm            = vm,                     &
                         remapType     = remap_option,           &
                         rc            = rc)
    IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

    WRITE(0,*)'Finished FieldRemapStore xg->model: src,tgt = ',src,tgt

  ENDDO
ENDDO

!=== PERFORM REMAPPING =========================================================

DO src=1,ngc

  IF(.NOT.modelIsEnabled(src))CYCLE

  DO tgt=1,ngc

    IF(.NOT.modelIsEnabled(tgt))CYCLE

    IF(tgt==src)CYCLE

    WRITE(0,*)'Entered FieldRemap loop: src,tgt = ',src,tgt

    DO n=1,numExpFields(src,tgt)
      CALL FieldRemap(srcField   = gc(src)%expField(n,tgt),&
                      dstField   = xg%expField(n,tgt,src), &
                      remapRH    = xg%RHandleIn(src,tgt),  &
                      zeroregion = ESMF_REGION_TOTAL,      &
                      checkflag  = .TRUE.,                 &
                      rc         = rc)
      IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)
    ENDDO

    DO n=1,numImpFields(src,tgt)
      CALL FieldRemap(srcField   = xg%expField(n,src,tgt), &
                      dstField   = gc(src)%impField(n,tgt),&
                      remapRH    = xg%RHandleOut(src,tgt), &
                      zeroregion = ESMF_REGION_TOTAL,      &
                      checkflag  = .TRUE.,                 &
                      rc         = rc)
      IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)
    ENDDO

  ENDDO
ENDDO

! Update initial SST and currents:
IF(modelIsEnabled(1).AND.modelIsEnabled(3))THEN

  !--------------------------------------------------------------------
  ! If UMWM is enabled, update surface Stokes drift in WRF
  IF(modelIsEnabled(2))THEN
    FORALL(i=isa:iea,j=jsa:jea,head_grid % landmask(i,j) == 0)

      head_grid % ust_esmf(i,j) = gc(1) % impFieldPtr(3,2) % Ptr(i,j)
      head_grid % vst_esmf(i,j) = gc(1) % impFieldPtr(4,2) % Ptr(i,j)

    ENDFORALL
  ENDIF
  !--------------------------------------------------------------------

  !--------------------------------------------------------------------
  ! If HYCOM is enabled, update SST and Eulerian current fields in WRF
  IF(modelIsEnabled(3))THEN
    FORALL(i=isa:iea,j=jsa:jea,head_grid % landmask(i,j) == 0 .AND. &
                               gc(1) % impFieldPtr(1,3) % Ptr(i,j) /= 0.)

      head_grid % tsk(i,j)     = gc(1) % impFieldPtr(1,3) % Ptr(i,j) + 300.15 ! (C -> K) + 27
      !head_grid % tsk(i,j)     = gc(1) % impFieldPtr(1,3) % Ptr(i,j) + 280.15 ! (C -> K) + 7

      ! Rotating back from spherical to Cartesian means
      ! we use negative angle for rotation
      head_grid % ue_esmf(i,j) = gc(1) % impFieldPtr(2,3) % Ptr(i,j)*COS(-gc(1) % alpha(i,j))&
                               - gc(1) % impFieldPtr(3,3) % Ptr(i,j)*SIN(-gc(1) % alpha(i,j))
      head_grid % ve_esmf(i,j) = gc(1) % impFieldPtr(2,3) % Ptr(i,j)*SIN(-gc(1) % alpha(i,j))&
                               + gc(1) % impFieldPtr(3,3) % Ptr(i,j)*COS(-gc(1) % alpha(i,j))

    ENDFORALL
  ENDIF
  !--------------------------------------------------------------------

ENDIF
    
!= WRITE NETCDF OUTPUT OF COUPLING FIELDS ======================================

! What is current time?
CALL ESMF_ClockGet(clock    = clock,      &
                   currTime = currentTime,&
                   rc       = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! Gets time in ISO 8601 format: YYYY-MM-DDThh:mm:ss
CALL ESMF_TimeGet(time              = currentTime,   &
                  timeStringISOFrac = currentTimeStr,&
                  rc                = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! Get rid of timezone character:
currentTimeStr(11:11) = '_'

CALL UMCM_cplFieldOutputXG(currentTimeStr)

ENDSUBROUTINE CPL_Init
!===============================================================================



SUBROUTINE CPL_Run(cplComp,impState,expState,clock,rc)
!===============================================================================

!USE module_sf_sfclay,ONLY:ESMF_CouplerStarted_sfc => ESMF_CouplerStarted,&
!                          uwin_vconvc_water
USE module_sf_sfclay,ONLY:ESMF_CouplerStarted_sfc => ESMF_CouplerStarted
USE module_bl_ysu,   ONLY:ESMF_CouplerStarted_bl  => ESMF_CouplerStarted,&
                          UMCM_VectorStressLimiter

USE module_first_rk_step_part1,ONLY:&
    WRF_sstFromOcean        => UMCM_sstFromOcean,&
    WRF_currentsFromOcean   => UMCM_currentDependentHeatFlux,&
    WRF_windStressFromWaves => UMCM_windStressFromWaves,&
    WRF_oceanSurfaceRelativeHeatFlux => UMCM_currentDependentHeatFlux

!===============================================================================

! ARGUMENTS
TYPE(ESMF_CplComp)  :: cplComp
TYPE(ESMF_State)    :: impState
TYPE(ESMF_State)    :: expState
TYPE(ESMF_Clock)    :: clock
INTEGER,INTENT(OUT) :: rc

INTEGER :: src,tgt,i,j,n,sprayWaitSteps

INTEGER,SAVE :: cplTimeStep = 0

REAL :: t0,t1

IF(waitToStartSpray)THEN    ! User may wait to start spray calcs to allow initial environment to equilibrate.
  sprayWaitSteps = 10
ELSE
  sprayWaitSteps = 1
ENDIF

!===============================================================================

cplTimeStep = cplTimeStep+1

IF(waveCurrentInteraction .AND. cplTimeStep > 1)THEN

  CALL VortexForce(uc     = gc(3) % auxField(1),&
                   vc     = gc(3) % auxField(2),&
                   us_src = gc(2) % auxField(1),&
                   vs_src = gc(2) % auxField(2),&
                   rc     = rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

ENDIF

!=== PERFORM REMAPPING =========================================================

DO src=1,ngc

  IF(.NOT.modelIsEnabled(src))CYCLE

  DO tgt=1,ngc

    IF(.NOT.modelIsEnabled(tgt))CYCLE

    IF(tgt==src)CYCLE

    DO n=1,numExpFields(src,tgt)
      CALL FieldRemap(srcField   = gc(src)%expField(n,tgt),&
                      dstField   = xg%expField(n,tgt,src), &
                      remapRH    = xg%RHandleIn(src,tgt),  &
                      zeroregion = ESMF_REGION_TOTAL,      &
                      checkflag  = .TRUE.,                 &
                      rc         = rc)
      IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)
    ENDDO

  ENDDO
ENDDO

CALL exchangeGridNestFeedback(rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

IF(sprayMediatedHeatFluxes .AND. cplTimeStep > sprayWaitSteps)THEN    ! Perform spray heat flux calculations

  CALL SprayMedHeatFluxes(rc = rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

ELSE    ! WRF will receive fields of zeros for spray components

  DO i = 1,numWRFAuxFields2D
    gc(1) % auxField2DPtr(i) % Ptr(isa:iea,jsa:jea) = 0.0
  ENDDO
  
ENDIF

CALL exchangeGridNestForce(cplTimeStep,rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

DO src=1,ngc

  IF(.NOT.modelIsEnabled(src))CYCLE

  DO tgt=1,ngc

    IF(.NOT.modelIsEnabled(tgt))CYCLE

    IF(tgt==src)CYCLE

    DO n=1,numImpFields(src,tgt)
      CALL FieldRemap(srcField   = xg%expField(n,src,tgt), &
                      dstField   = gc(src)%impField(n,tgt),&
                      remapRH    = xg%RHandleOut(src,tgt), &
                      zeroregion = ESMF_REGION_TOTAL,      &
                      checkflag  = .TRUE.,                 &
                      rc         = rc)
      IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)
    ENDDO

  ENDDO
ENDDO

!===============================================================================

! SST feedback to WRF parent domain:
IF(modelIsEnabled(1) .AND. gc(3) % isActive .AND. sstFromOcean)THEN

  FORALL(i=isa:iea,j=jsa:jea,head_grid % landmask(i,j) == 0           &
                       .AND. gc(1) % impFieldPtr(1,3) % Ptr(i,j) /= 0)

    head_grid % tsk(i,j) = gc(1) % impFieldPtr(1,3) % Ptr(i,j) + 300.15 ! (C -> K) + 27
    !head_grid % tsk(i,j) = gc(1) % impFieldPtr(1,3) % Ptr(i,j) + 280.15 ! (C -> K) + 7

    ! Rotating back from spherical to Cartesian means
    ! we use negative angle for rotation
    head_grid % ue_esmf(i,j) = gc(1) % impFieldPtr(2,3) % Ptr(i,j)*COS(-gc(1) % alpha(i,j))&
                             - gc(1) % impFieldPtr(3,3) % Ptr(i,j)*SIN(-gc(1) % alpha(i,j))
    head_grid % ve_esmf(i,j) = gc(1) % impFieldPtr(2,3) % Ptr(i,j)*SIN(-gc(1) % alpha(i,j))&
                             + gc(1) % impFieldPtr(3,3) % Ptr(i,j)*COS(-gc(1) % alpha(i,j))

  ENDFORALL

ENDIF

IF(modelIsEnabled(2))THEN

  head_grid % taux_esmf(isa:iea,jsa:jea) = gc(1) % impFieldPtr(1,2) % Ptr(isa:iea,jsa:jea)
  head_grid % tauy_esmf(isa:iea,jsa:jea) = gc(1) % impFieldPtr(2,2) % Ptr(isa:iea,jsa:jea)
  head_grid %  ust_esmf(isa:iea,jsa:jea) = gc(1) % impFieldPtr(3,2) % Ptr(isa:iea,jsa:jea)
  head_grid %  vst_esmf(isa:iea,jsa:jea) = gc(1) % impFieldPtr(4,2) % Ptr(isa:iea,jsa:jea)
  head_grid %  EPS_UMWM(isa:iea,jsa:jea) = gc(1) % impFieldPtr(5,2) % Ptr(isa:iea,jsa:jea)
  head_grid %   HS_UMWM(isa:iea,jsa:jea) = gc(1) % impFieldPtr(7,2) % Ptr(isa:iea,jsa:jea)

ENDIF
 
! Spray variables to WRF parent domain:
head_grid % dHS1spr(isa:iea,jsa:jea)  = gc(1) % auxField2DPtr(1) % Ptr(isa:iea,jsa:jea)
head_grid % dHL1spr(isa:iea,jsa:jea)  = gc(1) % auxField2DPtr(2) % Ptr(isa:iea,jsa:jea)
head_grid % HTspr(isa:iea,jsa:jea)    = gc(1) % auxField2DPtr(3) % Ptr(isa:iea,jsa:jea)
head_grid % HSspr(isa:iea,jsa:jea)    = gc(1) % auxField2DPtr(4) % Ptr(isa:iea,jsa:jea)
head_grid % HRspr(isa:iea,jsa:jea)    = gc(1) % auxField2DPtr(5) % Ptr(isa:iea,jsa:jea)
head_grid % HLspr(isa:iea,jsa:jea)    = gc(1) % auxField2DPtr(6) % Ptr(isa:iea,jsa:jea)
head_grid % alpha_S(isa:iea,jsa:jea)  = gc(1) % auxField2DPtr(7) % Ptr(isa:iea,jsa:jea)
head_grid % beta_S(isa:iea,jsa:jea)   = gc(1) % auxField2DPtr(8) % Ptr(isa:iea,jsa:jea)
head_grid % beta_L(isa:iea,jsa:jea)   = gc(1) % auxField2DPtr(9) % Ptr(isa:iea,jsa:jea)
head_grid % gamma_H(isa:iea,jsa:jea)  = gc(1) % auxField2DPtr(10)% Ptr(isa:iea,jsa:jea)
head_grid % delt_spr(isa:iea,jsa:jea) = gc(1) % auxField2DPtr(11)% Ptr(isa:iea,jsa:jea)
head_grid % delq_spr(isa:iea,jsa:jea) = gc(1) % auxField2DPtr(12)% Ptr(isa:iea,jsa:jea)
head_grid % Mspr(isa:iea,jsa:jea)     = gc(1) % auxField2DPtr(13)% Ptr(isa:iea,jsa:jea)

!===============================================================================
! Manage coupling switches in model components:
IF(firstTimeStep)THEN
  WRITE(*,*)
  WRITE(*,*)'UWIN_coupler: CPL_run: Setting up coupling switches in model components'
  WRITE(*,*)'UWIN_coupler: CPL_run: sstFromOcean:             ',sstFromOcean
  WRITE(*,*)'UWIN_coupler: CPL_run: currentsFromOcean:        ',currentsFromOcean
  WRITE(*,*)'UWIN_coupler: CPL_run: windStressFromWavesScalar:',windStressFromWavesScalar
  WRITE(*,*)'UWIN_coupler: CPL_run: windStressFromWavesVector:',windStressFromWavesVector
  WRITE(*,*)'UWIN_coupler: CPL_run: oceanStressFromWaves:     ',oceanStressFromWaves
  WRITE(*,*)'UWIN_coupler: CPL_run: oceanAdvectsWaves:        ',oceanAdvectsWaves
  WRITE(*,*)'UWIN_coupler: CPL_run: waveCurrentInteraction    ',waveCurrentInteraction
  WRITE(*,*)'UWIN_coupler: CPL_run: sprayMediatedHeatFluxes:  ',sprayMediatedHeatFluxes
! WRITE(*,*)'UWIN_coupler: CPL_run: vconvAtmosphere           ',vconvAtmosphere
! WRITE(*,*)'UWIN_coupler: CPL_run: vconvOcean                ',vconvOcean
  WRITE(*,*)
ENDIF

! Set coupling switches
IF(sstFromOcean)THEN
  WRF_sstFromOcean = .TRUE.        ! SST feedback to the atmosphere
ENDIF

IF(currentsFromOcean)THEN
  WRF_currentsFromOcean = .TRUE.   ! Use currents in heat flux calculation
ENDIF

IF(windStressFromWavesScalar)THEN
  WRF_windStressFromWaves = .TRUE. ! Use wave model wind stress
  ESMF_CouplerStarted_sfc = .TRUE. ! Scalar stress coupling in sfclay
ENDIF

IF(windStressFromWavesVector)THEN

  WRF_windStressFromWaves = .TRUE. ! Use wave model wind stress
  ESMF_CouplerStarted_sfc = .TRUE. ! Scalar stress coupling in sfclay
  ESMF_CouplerStarted_bl  = .TRUE. ! Vector stress coupling in bl_ysu

  UMCM_VectorStressLimiter = 0

ENDIF

! Adjust the heat fluxes in WRF for Eulerian current and Stokes drift
WRF_oceanSurfaceRelativeHeatFlux = .TRUE.

! Convective velocity control for WRF's surface layer
!uwin_vconvc_water = vconvAtmosphere

!= WRITE NETCDF OUTPUT OF COUPLING FIELDS ======================================

! What is current time?
CALL ESMF_ClockGet(clock    = clock,      &
                   currTime = currentTime,&
                   rc       = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! Gets time in ISO 8601 format: YYYY-MM-DDThh:mm:ss
CALL ESMF_TimeGet(time              = currentTime,   &
                  timeStringISOFrac = currentTimeStr,&
                  rc                = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! Get rid of timezone character:
currentTimeStr(11:11)='_'

firstTimeStep = .FALSE.

IF(currentTimeStr(15:19)=='00:00')THEN
  CALL UMCM_cplFieldOutputXG(currentTimeStr)
ENDIF

ENDSUBROUTINE CPL_Run
!===============================================================================



SUBROUTINE CPL_Final(cplComp,impState,expState,clock,rc)
!===============================================================================

! ARGUMENTS
TYPE(ESMF_CplComp)  :: cplComp
TYPE(ESMF_State)    :: impState
TYPE(ESMF_State)    :: expState
TYPE(ESMF_Clock)    :: clock
INTEGER,INTENT(OUT) :: rc

!===============================================================================

! Nothing to finalize here.
rc = ESMF_SUCCESS

CALL ESMF_LogWrite('Coupler component finalized',ESMF_LOGMSG_INFO)
CALL ESMF_LogFlush()

ENDSUBROUTINE CPL_Final
!===============================================================================
ENDMODULE UWIN_coupler
