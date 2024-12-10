MODULE UWIN_interface_HYCOM
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

!USE mod_hycom,     ONLY:HYCOM_Init,Export_ESMF,      &
!                        depths,plon,plat,hycomMask,  &
!                        hycom_distgrid => distgrid2d,&
!                        hycom_grid => grid2d,        &
!                        ubavg,vbavg,u,v

USE mpi

USE mod_cb_arrays, ONLY:plon,plat,depths
USE mod_hycom,     ONLY:HYCOM_Init,day1,day2
USE mod_dimensions,ONLY:itdm,jtdm,kdm,i0,j0,ii,jj
USE mod_xc,        ONLY:xclget,xcspmd

!===============================================================================

! ARGUMENTS
TYPE(ESMF_GridComp) :: gcomp
TYPE(ESMF_State)    :: importState
TYPE(ESMF_State)    :: exportState
TYPE(ESMF_Clock)    :: clock
INTEGER,INTENT(OUT) :: rc

TYPE(ESMF_Time) :: hycomTime,startTime,stopTime
TYPE(ESMF_TimeInterval) :: hycomDays

INTEGER :: start_day,end_day,start_hour,end_hour

INTEGER :: ierr
INTEGER :: i,j,k,m,n,pet

CHARACTER(LEN=99) :: longStr 

INTEGER :: ntiles,ncols,nrows

INTEGER,DIMENSION(:),ALLOCATABLE :: ist,iext,iend
INTEGER,DIMENSION(:),ALLOCATABLE :: jst,jext,jend

TYPE(ESMF_Grid)     :: grid
TYPE(ESMF_DistGrid) :: distgrid

!REAL(KIND=ESMF_KIND_R8),DIMENSION(idmo) :: plonrl
!REAL(KIND=ESMF_KIND_R8),DIMENSION(jdmo) :: platrl

REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),ALLOCATABLE :: curv

INTEGER(KIND=ESMF_KIND_I4),DIMENSION(:,:),ALLOCATABLE :: hycom_mask

REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),POINTER :: Xcoord,Ycoord
REAL(KIND=ESMF_KIND_R4),DIMENSION(:),  POINTER :: Zcoord

INTEGER(KIND=ESMF_KIND_I4),DIMENSION(:,:),POINTER :: mask_ptr

INTEGER,DIMENSION(2) :: lb,ub   ! Lower and upper bounds on grid

INTEGER(KIND=ESMF_KIND_I4),DIMENSION(:,:),  ALLOCATABLE :: tileDims
INTEGER(KIND=ESMF_KIND_I4),DIMENSION(:,:,:),ALLOCATABLE :: deBlockList

INTEGER :: src,tgt

!===============================================================================

CALL ESMF_TimeSet(hycomTime,yy=1900,mm=12,dd=31)

CALL ESMF_ClockGet(clock,startTime=startTime,rc=rc)
hycomDays = startTime-hycomTime

CALL ESMF_TimeIntervalGet(hycomDays,d=start_day,h=start_hour,rc=rc)

CALL ESMF_clockGet(clock,stopTime=stopTime,rc=rc)
hycomDays = stopTime-hycomTime
      
CALL ESMF_TimeIntervalGet(hycomDays,d=end_day,h=end_hour,rc=rc)

day1 = start_day+start_hour/24.
day2 = end_day+end_hour/24.

CALL xcspmd(mpicomm)
CALL HYCOM_Init()

ALLOCATE(tileDims(4,petCount))

! First gather to root processor:
CALL ESMF_VMGather(vm       = vm,                     &
                   sendData = [i0+1,i0+ii,j0+1,j0+jj],&
                   recvData = tileDims(:,localPet+1), &
                   count    = 4,                      &
                   rootPET  = 0,                      &
                   rc       = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

CALL MPI_Bcast(tileDims,SIZE(tileDims),MPI_INTEGER,0,mpicomm,ierr)

! Report tile dimensions to standard output:
DO pet = 1,petCount
  WRITE(*,*)pet,tileDims(:,pet)
ENDDO

! Now that we have domain decomposition information, we can 
! easily create a deLayout and deBlockList:
ALLOCATE(deBlockList(2,2,petCount)) ! (dimCount,2,deCount)
DO pet = 1,petCount !           I                J
  deBlockList(:,1,pet) = [tileDims(1,pet),tileDims(3,pet)] ! START
  deBlockList(:,2,pet) = [tileDims(2,pet),tileDims(4,pet)] ! END
ENDDO

distgrid = ESMF_DistGridCreate(minIndex    = [1,1],            &
                               maxIndex    = [itdm,jtdm],      &
                               deBlockList = deBlockList,      &
                               indexflag   = ESMF_INDEX_GLOBAL,&
                               rc          = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! Use distgrid to create grid:
grid = ESMF_GridCreate(name          = 'HYCOM grid',         &
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

CALL ESMF_GridGetCoord(grid            = grid,                  &
                       localDE         = 0,                     &
                       CoordDim        = 2,                     &
                       staggerloc      = ESMF_STAGGERLOC_CENTER,&
                       exclusiveLBound = lb,                    &
                       exclusiveUBound = ub,                    &
                       farrayPtr       = Ycoord,                &
                       rc              = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

DO j = 1,jj
  DO i = 1,ii
    Xcoord(i+lb(1)-1,j+lb(2)-1) = plon(i,j)
    Ycoord(i+lb(1)-1,j+lb(2)-1) = plat(i,j)
  ENDDO
ENDDO

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
                      farrayPtr  = maskfarrayPtr(3) % Ptr,&
                      rc         = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

DO j = lb(2),ub(2)
  DO i = lb(1),ub(1)
    IF(depths(i-lb(1)+1,j-lb(2)+1) == 0)THEN
      maskfarrayPtr(3) % Ptr(i,j) = 1
    ELSE
      maskfarrayPtr(3) % Ptr(i,j) = 0
    ENDIF
  ENDDO
ENDDO

! TEMPORARY HACK:

gc(3)%distgrid = distgrid
gc(3)%grid     = grid

CALL gc(3)%createFields(arraySpec=arrspec2DR4,rc=rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

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

!ALLOCATE(curv(idmo,jdmo))

!curv = GridRotation(REAL(plon(1:idmo,1:jdmo)),REAL(plat(1:idmo,1:jdmo)),rc=rc)
!IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

ENDSUBROUTINE hycom_component_init
!===============================================================================



SUBROUTINE hycom_component_run(gcomp,importState,exportState,clock,rc)
!===============================================================================

USE mod_hycom,ONLY:HYCOM_Run

!===============================================================================

! ARGUMENTS
TYPE(ESMF_GridComp) :: gcomp
TYPE(ESMF_State)    :: importState
TYPE(ESMF_State)    :: exportState
TYPE(ESMF_Clock)    :: clock
INTEGER,INTENT(OUT) :: rc

!===============================================================================

CALL importFields(rc=rc)
CALL HYCOM_Run()
CALL exportFields(rc=rc)

ENDSUBROUTINE hycom_component_run
!===============================================================================



SUBROUTINE hycom_component_finalize(gcomp,importState,exportState,clock,rc)
!===============================================================================
USE mod_hycom,ONLY:HYCOM_Final
!===============================================================================

! ARGUMENTS
TYPE(ESMF_GridComp) :: gcomp
TYPE(ESMF_State)    :: importState
TYPE(ESMF_State)    :: exportState
TYPE(ESMF_Clock)    :: clock
INTEGER,INTENT(OUT) :: rc

!===============================================================================

CALL HYCOM_Final()

ENDSUBROUTINE hycom_component_finalize
!===============================================================================



SUBROUTINE importFields(rc)
!==============================================================================>
!
! Imports ocean model fields into ESMF_Field objects.
!
!==============================================================================>
USE UWIN_global,   ONLY:oceanStressFromWaves
USE mod_xc,        ONLY:xctilr,halo_pv,halo_ps
USE mod_dimensions,ONLY:ii,jj,ip,nbdy
USE mod_cb_arrays, ONLY:taux,tauy,airtmp,vapmix,swflx,radflx,precip,wndspd,&
                        seatmp!,mslprs

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

  airtmp(i,j,2) = gc(3) % impFieldPtr(1,1) % Ptr(i+its-1,j+jts-1)
  vapmix(i,j,2) = gc(3) % impFieldPtr(2,1) % Ptr(i+its-1,j+jts-1)
   swflx(i,j,2) = gc(3) % impFieldPtr(3,1) % Ptr(i+its-1,j+jts-1)
  radflx(i,j,2) = gc(3) % impFieldPtr(4,1) % Ptr(i+its-1,j+jts-1)
  precip(i,j,2) = gc(3) % impFieldPtr(5,1) % Ptr(i+its-1,j+jts-1)
  seatmp(i,j,2) = gc(3) % impFieldPtr(6,1) % Ptr(i+its-1,j+jts-1)
  wndspd(i,j,2) = gc(3) % impFieldPtr(7,1) % Ptr(i+its-1,j+jts-1)
    
  taux(i,j,2) = gc(3) % impFieldPtr(nx,src) % Ptr(i+its-1,j+jts-1)
  tauy(i,j,2) = gc(3) % impFieldPtr(ny,src) % Ptr(i+its-1,j+jts-1)

  !mslprs(i,j,2) = gc(3) % impFieldPtr(10,1) % Ptr(i+its-1,j+jts-1)

ENDFORALL

! In regions where UMWM domain is smaller than HYCOM domain,
! we need to use atmospheric stress instead.
IF(oceanStressFromWaves)THEN
  DO j = 1,jj
    DO i = 1,ii
      IF(.NOT. ip(i,j) == 1)CYCLE
      IF(gc(3) % impFieldPtr(1,2) % Ptr(i+its-1,j+jts-1) == 0 .AND. &
         gc(3) % impFieldPtr(2,2) % Ptr(i+its-1,j+jts-1) == 0)THEN
        taux(i,j,2) = gc(3) % impFieldPtr(8,1) % Ptr(i+its-1,j+jts-1)
        tauy(i,j,2) = gc(3) % impFieldPtr(9,1) % Ptr(i+its-1,j+jts-1)
      ENDIF
    ENDDO
  ENDDO
ENDIF

! Stress vector and SLP need halo update
CALL xctilr(  taux(1-nbdy,1-nbdy,2),1,1,nbdy,nbdy,halo_pv)
CALL xctilr(  tauy(1-nbdy,1-nbdy,2),1,1,nbdy,nbdy,halo_pv)
!call xctilr(mslprs(1-nbdy,1-nbdy,2),1,1,nbdy,nbdy,halo_ps)

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
USE mod_cb_arrays, ONLY:temp,th3d,ubavg,vbavg,u,v
USE mod_hycom,     ONLY:nstep

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
  
FORALL(i=1:ii,j=1:jj,k=1:12,ip(i,j) == 1)

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
