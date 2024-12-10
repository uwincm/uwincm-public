MODULE UWIN_interface_HYCOM
!===============================================================================
!
! This interface is valid for HYCOM v2.2.34
!
!===============================================================================

USE ESMF
USE UWIN_GriddedComponent
USE UWIN_global
USE UWIN_utility,ONLY:GridRotation

!===============================================================================
IMPLICIT NONE

PRIVATE

PUBLIC :: SetServices

!===============================================================================
CONTAINS



SUBROUTINE hycom_component_init(gcomp,importState,exportState,clock,rc)
!===============================================================================

USE mod_hycom,     ONLY:HYCOM_Init,      &
                        depths,plon,plat,hycomMask,  &
                        hycom_distgrid => distgrid2d,&
                        hycom_grid => grid2d,        &
                        ubavg,vbavg,u,v
USE mod_dimensions,ONLY:itdm,jtdm,kdm
USE mod_xc,        ONLY:xclget,deBlockListHYCOM => deBlockList

!===============================================================================

! Dummy arguments:
TYPE(ESMF_GridComp) :: gcomp
TYPE(ESMF_State)    :: importState
TYPE(ESMF_State)    :: exportState
TYPE(ESMF_Clock)    :: clock
INTEGER,INTENT(OUT) :: rc

INTEGER :: i,j,k,m,n

CHARACTER(LEN=99) :: longStr 

INTEGER :: ntiles,ncols,nrows

INTEGER,DIMENSION(:),ALLOCATABLE :: ist,iext,iend
INTEGER,DIMENSION(:),ALLOCATABLE :: jst,jext,jend

TYPE(ESMF_Grid)     :: grid
TYPE(ESMF_DistGrid) :: distgrid

REAL(KIND=ESMF_KIND_R8),DIMENSION(idmo) :: plonrl
REAL(KIND=ESMF_KIND_R8),DIMENSION(jdmo) :: platrl

REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),ALLOCATABLE :: alpha

REAL   (KIND=ESMF_KIND_R8),DIMENSION(:,:),ALLOCATABLE :: hycom_lon,hycom_lat
INTEGER(KIND=ESMF_KIND_I4),DIMENSION(:,:),ALLOCATABLE :: hycom_mask

REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),POINTER :: Xcoord,Ycoord
REAL(KIND=ESMF_KIND_R4),DIMENSION(:),  POINTER :: Zcoord

INTEGER(KIND=ESMF_KIND_I4),DIMENSION(:,:),POINTER :: mask_ptr

INTEGER,DIMENSION(2) :: lb,ub   ! Lower and upper bounds on grid

INTEGER(KIND=ESMF_KIND_I4),DIMENSION(:,:),  ALLOCATABLE :: tileDims
INTEGER(KIND=ESMF_KIND_I4),DIMENSION(:,:,:),ALLOCATABLE :: deBlockList

INTEGER :: src,tgt

!===============================================================================

CALL HYCOM_Init(gcomp,importState,exportState,clock,rc)

! TEMPORARY HACK:

gc(3)%distgrid = hycom_distgrid
gc(3)%grid     = hycom_grid

CALL gc(3)%createFields(arraySpec=arrspec2DR4,rc=rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! Associate mask pointer and point to HYCOM mask:
CALL ESMF_GridGetItem(grid       = gc(3)%grid,            &
                      localDE    = 0,                     &
                      staggerloc = ESMF_STAGGERLOC_CENTER,&
                      itemflag   = ESMF_GRIDITEM_MASK,    &
                      farrayPtr  = maskfarrayPtr(3)%Ptr,  &
                      rc         = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)
maskfarrayPtr(3)%Ptr(:,:) = hycomMask(:,:)

gc(3)%kdm = 12

iso = gc(3)%its; ieo = gc(3)%ite
jso = gc(3)%jts; jeo = gc(3)%jte

!------------------------------------------------------------------------------>
! Current, x-component:
gc(3)%auxField(1) = ESMF_FieldCreate(grid            = gc(3)%grid,            &
                                     arrayspec       = arrspec3DR4,           &
                                     indexflag       = ESMF_INDEX_GLOBAL,     &
                                     staggerLoc      = ESMF_STAGGERLOC_CENTER,&
                                     totalLwidth     = [1,1],                 &
                                     totalUwidth     = [1,1],                 &
                                     ungriddedLBound = [1],                   &
                                     ungriddedUBound = [gc(3)%kdm],           &
                                     name            = 'uc',                  &
                                     rc              = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

CALL ESMF_FieldGet(field     = gc(3)%auxField(1),       &
                   localDE   = 0,                       &
                   farrayPtr = gc(3)%auxFieldPtr(1)%Ptr,&
                   rc        = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

gc(3)%auxFieldPtr(1)%Ptr = 0

!------------------------------------------------------------------------------>
! Current, y-component:
gc(3)%auxField(2) = ESMF_FieldCreate(grid            = gc(3)%grid,            &
                                     arrayspec       = arrspec3DR4,           &
                                     indexflag       = ESMF_INDEX_GLOBAL,     &
                                     staggerLoc      = ESMF_STAGGERLOC_CENTER,&
                                     totalLwidth     = [1,1],                 &
                                     totalUwidth     = [1,1],                 &
                                     ungriddedLBound = [1],                   &
                                     ungriddedUBound = [gc(3)%kdm],           &
                                     name            = 'vc',                  &
                                     rc              = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! Initialize:
CALL ESMF_FieldGet(field     = gc(3)%auxField(2),       &
                   localDE   = 0,                       &
                   farrayPtr = gc(3)%auxFieldPtr(2)%Ptr,&
                   rc        = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

gc(3)%auxFieldPtr(2)%Ptr = 0

!==============================================================================

CALL exportFields(rc=rc)

!ALLOCATE(alpha(idmo,jdmo))

!alpha = GridRotation(REAL(plon(1:idmo,1:jdmo)),REAL(plat(1:idmo,1:jdmo)),rc=rc)
!IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

ENDSUBROUTINE hycom_component_init
!===============================================================================



SUBROUTINE hycom_component_run(gcomp,importState,exportState,clock,rc)
!===============================================================================

USE mod_hycom,     ONLY:HYCOM_Run,dp,dpu,dpv,u,v
USE mod_dimensions,ONLY:kdm
USE UMWM_stokes,   ONLY:umwmDepth => depth

!===============================================================================

TYPE(ESMF_GridComp) :: gcomp
TYPE(ESMF_State)    :: importState
TYPE(ESMF_State)    :: exportState
TYPE(ESMF_Clock)    :: clock
INTEGER,INTENT(OUT) :: rc

INTEGER :: i,j,k,kk

REAL :: dpint

REAL,PARAMETER :: thicknessFactor = 9.8E-5

! timing
TYPE(ESMF_Time)         :: currentTime,wrfRunStartTime,wrfRunStopTime
TYPE(ESMF_TimeInterval) :: timeStep

!===============================================================================

CALL importFields(rc=rc)
CALL HYCOM_Run(gcomp,importState,exportState,clock,rc)
CALL exportFields(rc=rc)

ENDSUBROUTINE hycom_component_run
!===============================================================================



SUBROUTINE hycom_component_finalize(gcomp,importState,exportState,clock,rc)
!===============================================================================
USE mod_hycom,ONLY:HYCOM_Final
!===============================================================================
IMPLICIT NONE

! Dummy arguments:
TYPE(ESMF_GridComp) :: gcomp
TYPE(ESMF_State)    :: importState
TYPE(ESMF_State)    :: exportState
TYPE(ESMF_Clock)    :: clock
INTEGER,INTENT(OUT) :: rc

!===============================================================================

CALL HYCOM_Final(gcomp,importState,exportState,clock,rc)

ENDSUBROUTINE hycom_component_finalize
!===============================================================================



SUBROUTINE importFields(rc)
!==============================================================================>
!
! Imports ocean model fields into ESMF_Field objects.
!
!==============================================================================>
USE UWIN_global,   ONLY:oceanStressFromWaves
USE mod_xc,        ONLY:xctilr,halo_pv
USE mod_dimensions,ONLY:ii,jj,ip,nbdy
USE mod_hycom,     ONLY:taux,tauy,airtmp,vapmix,swflx,radflx,precip,wndspd,&
                        seatmp

INTEGER,INTENT(OUT),OPTIONAL :: rc

INTEGER :: i,j
INTEGER :: its,jts
INTEGER :: nx,ny,src

rc = ESMF_FAILURE

its = gc(3) % its
jts = gc(3) % jts

IF(oceanStressFromWaves)THEN
  nx  = 1
  ny  = 2
  src = 2
ELSE
  nx  = 8
  ny  = 9
  src = 1
ENDIF

FORALL(i=1:ii,j=1:jj,ip(i,j)==1)

  airtmp(i,j,2) = DBLE(gc(3) % impFieldPtr(1,1) % Ptr(i+its-1,j+jts-1))
  vapmix(i,j,2) = DBLE(gc(3) % impFieldPtr(2,1) % Ptr(i+its-1,j+jts-1))
   swflx(i,j,2) = DBLE(gc(3) % impFieldPtr(3,1) % Ptr(i+its-1,j+jts-1))
  radflx(i,j,2) = DBLE(gc(3) % impFieldPtr(4,1) % Ptr(i+its-1,j+jts-1))
  precip(i,j,2) = DBLE(gc(3) % impFieldPtr(5,1) % Ptr(i+its-1,j+jts-1))
  seatmp(i,j,2) = DBLE(gc(3) % impFieldPtr(6,1) % Ptr(i+its-1,j+jts-1))
  wndspd(i,j,2) = DBLE(gc(3) % impFieldPtr(7,1) % Ptr(i+its-1,j+jts-1))
    
  taux(i,j,2) = DBLE(gc(3) % impFieldPtr(nx,src) % Ptr(i+its-1,j+jts-1))
  tauy(i,j,2) = DBLE(gc(3) % impFieldPtr(ny,src) % Ptr(i+its-1,j+jts-1))

ENDFORALL

! In case where UMWM domain is smaller than HYCOM domain,
! we need to use atmospheric stress instead.
IF(oceanStressFromWaves)THEN
  DO j = 1,jj
    DO i = 1,ii
      IF(.NOT. ip(i,j) == 1)CYCLE
      IF(gc(3) % impFieldPtr(1,2) % Ptr(i+its-1,j+jts-1) == 0 .AND. &
         gc(3) % impFieldPtr(2,2) % Ptr(i+its-1,j+jts-1) == 0)THEN
        taux(i,j,2) = DBLE(gc(3) % impFieldPtr(8,1) % Ptr(i+its-1,j+jts-1))
        tauy(i,j,2) = DBLE(gc(3) % impFieldPtr(9,1) % Ptr(i+its-1,j+jts-1))
      ENDIF
    ENDDO
  ENDDO
ENDIF

CALL xctilr(taux(1-nbdy,1-nbdy,2),1,1,nbdy,nbdy,halo_pv)
CALL xctilr(tauy(1-nbdy,1-nbdy,2),1,1,nbdy,nbdy,halo_pv)

rc = ESMF_SUCCESS

ENDSUBROUTINE importFields
!==============================================================================>




SUBROUTINE exportFields(rc)
!==============================================================================>
!
! Exports ocean model fields into ESMF_Field objects.
!
!==============================================================================>

USE mod_dimensions,ONLY:ii,jj,ip,kdm
USE mod_hycom,     ONLY:temp,th3d,ubavg,vbavg,u,v,nstep

INTEGER,INTENT(OUT),OPTIONAL :: rc

INTEGER :: i,j,k,m,n
INTEGER :: its,jts

rc = ESMF_FAILURE

its = gc(3) % its
jts = gc(3) % jts

m = mod(nstep  ,2)+1
n = mod(nstep+1,2)+1

FORALL(i=1:ii,j=1:jj,ip(i,j) == 1)

  gc(3) % expFieldPtr(1,1) % Ptr(i+its-1,j+jts-1) = temp(i,j,1,n)-27.
  gc(3) % expFieldPtr(2,1) % Ptr(i+its-1,j+jts-1) = u(i,j,1,n)+ubavg(i,j,n)
  gc(3) % expFieldPtr(3,1) % Ptr(i+its-1,j+jts-1) = v(i,j,1,n)+vbavg(i,j,n)

  gc(3) % expFieldPtr(1,2) % Ptr(i+its-1,j+jts-1) = u(i,j,1,n)+ubavg(i,j,n)
  gc(3) % expFieldPtr(2,2) % Ptr(i+its-1,j+jts-1) = v(i,j,1,n)+vbavg(i,j,n)

  gc(3) % expFieldPtr(3,2) % Ptr(i+its-1,j+jts-1) = th3d(i,j,1,n)

ENDFORALL
  
FORALL(i=1:ii,j=1:jj,k=1:12,ip(i,j)==1)

  gc(3) % auxFieldPtr(1) % Ptr(i+its-1,j+jts-1,k) = u(i,j,k,n)+ubavg(i,j,n)
  gc(3) % auxFieldPtr(2) % Ptr(i+its-1,j+jts-1,k) = v(i,j,k,n)+vbavg(i,j,n)

ENDFORALL

rc = ESMF_SUCCESS

ENDSUBROUTINE exportFields
!==============================================================================>




SUBROUTINE SetServices(gridComp,rc)
!===============================================================================

TYPE(ESMF_GridComp) :: gridComp
INTEGER,INTENT(OUT) :: rc

!===============================================================================

CALL ESMF_GridCompSetEntryPoint(gridcomp    = gridComp,              &
                                methodflag  = ESMF_METHOD_INITIALIZE,&
                                userRoutine = hycom_component_init,  &
                                rc          = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

CALL ESMF_GridCompSetEntryPoint(gridcomp    = gridComp,           &
                                methodflag  = ESMF_METHOD_RUN,    &
                                userRoutine = hycom_component_run,&
                                rc          = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

CALL ESMF_GridCompSetEntryPoint(gridcomp    = gridComp,                &
                                methodflag  = ESMF_METHOD_FINALIZE,    &
                                userRoutine = hycom_component_finalize,&
                                rc          = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

ENDSUBROUTINE SetServices
!===============================================================================
ENDMODULE UWIN_interface_HYCOM
