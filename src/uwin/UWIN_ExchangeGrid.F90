MODULE UWIN_ExchangeGrid
!===============================================================================
USE ESMF
USE mpi
USE netcdf
USE UWIN_global
USE UWIN_utility
!===============================================================================
IMPLICIT NONE

!- EXCHANGE GRID OBJECT ------------------------------------------------------->
TYPE ExchangeGrid

  !- GRID AND TILE PARAMETERS ------------------------------------------------->
  INTEGER :: idm,jdm         ! Domain dimensions 
  INTEGER :: its,ite,jts,jte ! Tile dimensions
  INTEGER :: maxDomains      ! Maximum number of nests +1 (parent)
  INTEGER :: refinementLevel ! This number corresponds to the number of WRF
                             ! domain of the equivalent resolution
  !---------------------------------------------------------------------------->

  !- UTILITY OBJECTS FOR GRID GENERATION -------------------------------------->
  INTEGER(KIND=ESMF_KIND_I4),DIMENSION(:,:),  ALLOCATABLE :: tile_dims
  INTEGER(KIND=ESMF_KIND_I4),DIMENSION(:,:,:),ALLOCATABLE :: deBlockList
  TYPE(ESMF_DistGrid)                                     :: distGrid
  TYPE(ESMF_Grid)                                         :: grid
  !---------------------------------------------------------------------------->  

  !- FIELDS, FIELD BUNDLES AND POINTERS --------------------------------------->  
  INTEGER,DIMENSION(ngc,ngc) :: numImpFields,numExpFields

  CHARACTER(LEN=ESMF_MAXSTR),DIMENSION(maxNumFields,ngc,ngc) :: impFieldName
  CHARACTER(LEN=ESMF_MAXSTR),DIMENSION(maxNumFields,ngc,ngc) :: expFieldName

  CHARACTER(LEN=ESMF_MAXSTR),DIMENSION(ngc,ngc) :: impBundleName
  CHARACTER(LEN=ESMF_MAXSTR),DIMENSION(ngc,ngc) :: expBundleName

  TYPE(ESMF_Field),      DIMENSION(maxNumFields,ngc,ngc) :: impField,expField
  TYPE(ESMF_FieldBundle),DIMENSION(ngc,ngc)              :: impBundle,expBundle

  TYPE(ESMF_RouteHandle),DIMENSION(ngc,ngc) :: RHandleIn,RHandleOut

  TYPE(farrayPtrType2DR4),DIMENSION(maxNumFields,ngc,ngc) :: impFieldPtr
  TYPE(farrayPtrType2DR4),DIMENSION(maxNumFields,ngc,ngc) :: expFieldPtr

  INTEGER(KIND=ESMF_KIND_I4),POINTER,DIMENSION(:,:) :: maskfarrayPtr
  !---------------------------------------------------------------------------->  

ENDTYPE ExchangeGrid
!- END OF EXCHANGE GRID OBJECT ------------------------------------------------>

TYPE(ExchangeGrid) :: xg

REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),ALLOCATABLE :: lon_XG,lat_XG,mask_XG

!===============================================================================
CONTAINS



SUBROUTINE exchangeGridCreate(srcGrid,aspectRatio,rc)
!===============================================================================

! Dummy arguments:
TYPE(ESMF_Grid),INTENT(IN) :: srcGrid
INTEGER,INTENT(IN)         :: aspectRatio
INTEGER,INTENT(OUT)        :: rc

INTEGER,DIMENSION(2) :: lb,ub

INTEGER :: itm,jtm       ! Coarse grid tile size
INTEGER :: itm_XG,jtm_XG ! Exchange grid tile size
INTEGER :: idm_XG,jdm_XG ! Exchange grid domain size

INTEGER :: its,ite,jts,jte             ! Coarse grid tile extent
INTEGER :: its_XG,ite_XG,jts_XG,jte_XG ! Exchange grid tile extent

INTEGER,DIMENSION(0:petCount-1) :: is,ie,js,je ! Tile extent as seen from root process

INTEGER :: n,status(MPI_STATUS_SIZE),ierr
INTEGER :: stat,ncid,xdimid,ydimid,lonid,latid

REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),POINTER     :: Xcoord,Ycoord
REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),ALLOCATABLE :: lon,lat,landmask

INTEGER(KIND=ESMF_KIND_I4),DIMENSION(:,:),ALLOCATABLE :: mask
INTEGER(KIND=ESMF_KIND_I4),DIMENSION(:,:),POINTER     :: mask_ptr

INTEGER(KIND=ESMF_KIND_I4),DIMENSION(:,:),  ALLOCATABLE :: tile_dims
INTEGER(KIND=ESMF_KIND_I4),DIMENSION(:,:,:),ALLOCATABLE :: deBlockList

!===============================================================================

CALL ESMF_LogWrite('ESMF_ExchangeGridCreate started',ESMF_LOGMSG_INFO)
CALL ESMF_LogFlush

xg%refinementLevel = 1+NINT(LOG(FLOAT(aspectRatio))/LOG(FLOAT(3)))
xg%maxDomains      = 10 ! Hardwire this to a safely large number

WRITE(*,*)'ESMF_ExchangeGridCreate: Refinement level is:',xg%refinementLevel

! Get x-coordinate of coarse input grid:
CALL ESMF_GridGetCoord(grid            = srcGrid,               &
                       localDE         = 0,                     &
                       CoordDim        = 1,                     &
                       staggerloc      = ESMF_STAGGERLOC_CENTER,&
                       exclusiveLBound = lb,                    &
                       exclusiveUBound = ub,                    &
                       farrayPtr       = Xcoord,                &
                       rc              = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! Get y-coordinate of coarse input grid:
CALL ESMF_GridGetCoord(grid       = srcGrid,               &
                       localDE    = 0,                     &
                       CoordDim   = 2,                     &
                       staggerloc = ESMF_STAGGERLOC_CENTER,&
                       farrayPtr  = Ycoord,                &
                       rc         = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! Get the mask field of coarse input grid:
CALL ESMF_GridGetItem(grid       = srcGrid,               &
                      localDE    = 0,                     &
                      staggerloc = ESMF_STAGGERLOC_CENTER,&
                      itemflag   = ESMF_GRIDITEM_MASK,    &
                      farrayPtr  = mask_ptr,              &
                      rc         = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! Start and end tile indices on coarse grid:
its = lb(1) ; ite = ub(1)
jts = lb(2) ; jte = ub(2)

! Tile Dimension length on coarse grid:
itm = ite-its+1
jtm = jte-jts+1

! Gather tile size information to root process:
CALL MPI_Gather(its,1,MPI_INTEGER,is,1,MPI_INTEGER,0,mpicomm,ierr)
CALL MPI_Gather(ite,1,MPI_INTEGER,ie,1,MPI_INTEGER,0,mpicomm,ierr)
CALL MPI_Gather(jts,1,MPI_INTEGER,js,1,MPI_INTEGER,0,mpicomm,ierr)
CALL MPI_Gather(jte,1,MPI_INTEGER,je,1,MPI_INTEGER,0,mpicomm,ierr)

idm_XG = (idma-1)*aspectRatio+1
jdm_XG = (jdma-1)*aspectRatio+1

xg%idm = idm_XG
xg%jdm = jdm_XG

IF(localPet==0)THEN
  ALLOCATE(lon_XG(idm_XG,jdm_XG));  lon_XG  = 0.
  ALLOCATE(lat_XG(idm_XG,jdm_XG));  lat_XG  = 0.
  ALLOCATE(mask_XG(idm_XG,jdm_XG)); mask_XG = 0.
ENDIF

IF(localPet==0)THEN
  ALLOCATE(lon(idma,jdma))
  ALLOCATE(lat(idma,jdma))
  ALLOCATE(landmask(idma,jdma))
ELSE
  ALLOCATE(lon(its:ite,jts:jte))
  ALLOCATE(lat(its:ite,jts:jte))
  ALLOCATE(landmask(its:ite,jts:jte))
ENDIF

lon(its:ite,jts:jte)      = Xcoord(its:ite,jts:jte)
lat(its:ite,jts:jte)      = Ycoord(its:ite,jts:jte)
landmask(its:ite,jts:jte) = mask_ptr(its:ite,jts:jte)

! Gather to root PET:
CALL tileGather(lon,is,ie,js,je,rc)
CALL tileGather(lat,is,ie,js,je,rc)
CALL tileGather(landmask,is,ie,js,je,rc)

! Interpolate source grid and mask -> XG:
IF(localPet==0)THEN
  CALL exchangeGridRefineField(lon,lon_XG,aspectRatio,rc)
  CALL exchangeGridRefineField(lat,lat_XG,aspectRatio,rc)
  CALL exchangeGridRefineField(landmask,mask_XG,aspectRatio,rc)
ENDIF

! Exchange grid tile size:
its_XG = (its-1)*aspectRatio+1
jts_XG = (jts-1)*aspectRatio+1

ite_XG = its_XG+aspectRatio*itm-1
jte_XG = jts_XG+aspectRatio*jtm-1

IF(ite == idma)ite_XG = (ite-1)*aspectRatio+1
IF(jte == jdma)jte_XG = (jte-1)*aspectRatio+1

xg%its = its_XG
xg%ite = ite_XG
xg%jts = jts_XG
xg%jte = jte_XG

CALL MPI_Gather(its_XG,1,MPI_INTEGER,is,1,MPI_INTEGER,0,mpicomm,ierr)
CALL MPI_Gather(ite_XG,1,MPI_INTEGER,ie,1,MPI_INTEGER,0,mpicomm,ierr)
CALL MPI_Gather(jts_XG,1,MPI_INTEGER,js,1,MPI_INTEGER,0,mpicomm,ierr)
CALL MPI_Gather(jte_XG,1,MPI_INTEGER,je,1,MPI_INTEGER,0,mpicomm,ierr)

ALLOCATE(xg%tile_dims(4,petCount))

IF(localPet==0)THEN
  DO n=0,petCount-1
    xg%tile_dims(:,n+1) = [is(n),ie(n),js(n),je(n)]
  ENDDO
ENDIF

! Broadcast to everybody else:
CALL MPI_Bcast(xg%tile_dims,SIZE(xg%tile_dims),MPI_INTEGER,0,mpicomm,ierr)

WRITE(*,*)'Exchange Grid tiling:'
DO n=1,petCount
  WRITE(*,*)n,xg%tile_dims(:,n)
ENDDO

! Create deBlockList:
ALLOCATE(xg%deBlockList(2,2,petCount)) ! (dimCount,2,deCount)
DO n=1,petCount !                 I                 J
  xg%deBlockList(:,1,n) = [xg%tile_dims(1,n),xg%tile_dims(3,n)] ! START
  xg%deBlockList(:,2,n) = [xg%tile_dims(2,n),xg%tile_dims(4,n)] ! END
ENDDO

! Allocate XG longitude and latitude on other PETs:
IF(localPet/=0)THEN
  ALLOCATE(lon_XG(its_XG:ite_XG,jts_XG:jte_XG));  lon_XG  = 0.
  ALLOCATE(lat_XG(its_XG:ite_XG,jts_XG:jte_XG));  lat_XG  = 0.
  ALLOCATE(mask_XG(its_XG:ite_XG,jts_XG:jte_XG)); mask_XG = 0.
ENDIF

CALL tileScatter(lon_XG,is,ie,js,je,rc)
CALL tileScatter(lat_XG,is,ie,js,je,rc)
CALL tileScatter(mask_XG,is,ie,js,je,rc)

! Create XG distgrid:
xg%distgrid = ESMF_DistGridCreate(minIndex    = [1,1],            &
                                  maxIndex    = [idm_XG,jdm_XG],  &
                                  deBlockList = xg%deBlockList,   &
                                  indexflag   = ESMF_INDEX_GLOBAL,& 
                                  rc          = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! Use distgrid to create grid:
xg%grid = ESMF_GridCreate(name          = 'Exchange grid',      &
                          distGrid      = xg%distGrid,          &
                          coordsys      = ESMF_COORDSYS_SPH_DEG,&   
                          coordTypeKind = ESMF_TYPEKIND_R4,     &
                          coordDimCount = [2,2],                &
                          indexflag     = ESMF_INDEX_GLOBAL,    &
                          rc            = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! Add coordinates:
CALL ESMF_GridAddCoord(grid       = xg%grid,               &
                       staggerloc = ESMF_STAGGERLOC_CENTER,&
                       rc         = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! Set up coordinates:
CALL ESMF_GridGetCoord(grid            = xg%grid,               &
                       localDE         = 0,                     &
                       CoordDim        = 1,                     &
                       staggerloc      = ESMF_STAGGERLOC_CENTER,&
                       exclusiveLBound = lb,                    &
                       exclusiveUBound = ub,                    &
                       farrayPtr       = Xcoord,                &
                       rc              = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

its=lb(1) ; ite=ub(1)
jts=lb(2) ; jte=ub(2)

Xcoord(its:ite,jts:jte) = lon_XG

CALL ESMF_GridGetCoord(grid       = xg%grid,               &
                       localDE    = 0,                     &
                       CoordDim   = 2,                     &
                       staggerloc = ESMF_STAGGERLOC_CENTER,&
                       farrayPtr  = Ycoord,                &
                       rc         = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

Ycoord(its:ite,jts:jte) = lat_XG

! Add mask:
CALL ESMF_GridAddItem(grid       = xg%grid,               &
                      staggerloc = ESMF_STAGGERLOC_CENTER,&
                      itemflag   = ESMF_GRIDITEM_MASK,    &
                      rc         = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

CALL ESMF_GridGetItem(grid       = xg%grid,               &
                      localDE    = 0,                     &
                      staggerloc = ESMF_STAGGERLOC_CENTER,&
                      itemflag   = ESMF_GRIDITEM_MASK,    &
                      farrayPtr  = xg%maskfarrayPtr,      &
                      rc         = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)
xg%maskfarrayPtr(its:ite,jts:jte) = NINT(mask_XG)

!===============================================================================

CALL ESMF_LogWrite('ESMF_ExchangeGridCreate complete',ESMF_LOGMSG_INFO)
CALL ESMF_LogFlush()

rc = ESMF_SUCCESS

ENDSUBROUTINE exchangeGridCreate
!===============================================================================



SUBROUTINE exchangeGridFieldCreateAll(rc)
!===============================================================================

INTEGER,INTENT(OUT) :: rc

INTEGER :: n,nm
INTEGER :: src,tgt

INTEGER,DIMENSION(2) :: lb,ub
!===============================================================================

rc = ESMF_SUCCESS

xg % numImpFields = numExpFields ! impFields on XG are expFields from model
xg % numExpFields = numImpFields ! expFields from XG are impFields on model

xg % impBundleName = expBundleName
xg % expBundleName = impBundleName

DO tgt=1,ngc
  DO src=1,ngc

    IF(src==tgt)CYCLE    

    ! Import fields:
    DO n=1,xg%numImpFields(src,tgt)

      xg%impFieldName(n,src,tgt) = expFieldName(n,src,tgt)

      xg%impField(n,src,tgt) = ESMF_FieldCreate(grid      = xg%grid,                   &
                                                arrayspec = arrspec2DR4,               &
                                                indexflag = ESMF_INDEX_GLOBAL,         &
                                                name      = xg%impFieldName(n,src,tgt),&
                                                rc        = rc)
      IF(rc/=ESMF_SUCCESS)RETURN

      CALL ESMF_FieldGet(field           = xg%impField(n,src,tgt),       &
                         localDE         = 0,                            &
                         farrayPtr       = xg%impFieldPtr(n,src,tgt)%Ptr,&
                         exclusiveLbound = lb,                           &
                         exclusiveUbound = ub,                           &
                         rc              = rc)
      IF(rc/=ESMF_SUCCESS)RETURN
      xg%impFieldPtr(n,src,tgt)%Ptr = 0

    ENDDO

    nm = xg%numImpFields(src,tgt)

    ! Import bundle:
    xg%impBundle(src,tgt) = ESMF_FieldBundleCreate(fieldList = xg%impField(1:nm,src,tgt),&
                                                   name      = xg%impBundleName(src,tgt),&
                                                   rc        = rc)
    IF(rc/=ESMF_SUCCESS)RETURN

    ! Export fields:
    DO n=1,xg%numExpFields(src,tgt)

      xg%expFieldName(n,src,tgt) = impFieldName(n,src,tgt)

      xg%expField(n,src,tgt)=ESMF_FieldCreate(grid      = xg%grid,                   &
                                              arrayspec = arrspec2DR4,               &
                                              indexflag = ESMF_INDEX_GLOBAL,         &
                                              name      = xg%expFieldName(n,src,tgt),&
                                              rc        = rc)
      IF(rc/=ESMF_SUCCESS)RETURN

      CALL ESMF_FieldGet(field           = xg%expField(n,src,tgt),       &
                         localDE         = 0,                            &
                         farrayPtr       = xg%expFieldPtr(n,src,tgt)%Ptr,&
                         exclusiveLbound = lb,                           &
                         exclusiveUbound = ub,                           &
                         rc              = rc)
      IF(rc/=ESMF_SUCCESS)RETURN
      xg%expFieldPtr(n,src,tgt)%Ptr = 0

    ENDDO

    nm = xg%numExpFields(src,tgt)

    ! Export bundle:
    xg%expBundle(src,tgt) = ESMF_FieldBundleCreate(fieldList = xg%expField(1:nm,src,tgt),&
                                                   name      = xg%expBundleName(src,tgt),&
                                                   rc        = rc)
    IF(rc/=ESMF_SUCCESS)RETURN

    ! Set bounds:
    isx = lb(1) ; iex = ub(1)
    jsx = lb(2) ; jex = ub(2)

  ENDDO 
ENDDO

ENDSUBROUTINE exchangeGridFieldCreateAll
!===============================================================================



SUBROUTINE exchangeGridPhysics(src,tgt,rc)
!===============================================================================

! Dummy arguments:
INTEGER,INTENT(IN)  :: src
INTEGER,INTENT(IN)  :: tgt
INTEGER,INTENT(OUT) :: rc

INTEGER :: n

!===============================================================================

! Copy from XG impFields:
DO n = 1,maxNumFields

  IF(.NOT.ASSOCIATED(xg%impFieldPtr(n,tgt,src)%Ptr))CYCLE
  IF(.NOT.ASSOCIATED(xg%expFieldPtr(n,src,tgt)%Ptr))CYCLE

  xg%expFieldPtr(n,src,tgt)%Ptr(isx:iex,jsx:jex) = xg%impFieldPtr(n,tgt,src)%Ptr(isx:iex,jsx:jex)

ENDDO

rc = ESMF_SUCCESS

ENDSUBROUTINE exchangeGridPhysics
!===============================================================================



SUBROUTINE exchangeGridNestForce(rc)
!===============================================================================
USE UWIN_interface_WRF,ONLY:dom,wrfDomExists,wrfDomHasStopped
USE module_domain,   ONLY:get_ijk_from_grid
!===============================================================================

INTEGER,INTENT(OUT) :: rc

INTEGER :: i,j

INTEGER :: ierr,status(MPI_STATUS_SIZE)

INTEGER :: domNum      ! Domain number (2 for d02, 3 for d03 etc.)
INTEGER :: coarsenRatio,refineRatio

! Nest decomposition
INTEGER :: ids,ide,jds,jde,kds,kde ! Total domain space
INTEGER :: ims,ime,jms,jme,kms,kme ! Tile domain space w/ halo points
INTEGER :: ips,ipe,jps,jpe,kps,kpe ! Tile domain space w/o halo points

INTEGER,DIMENSION(0:petCount-1) :: is,ie,js,je

INTEGER              :: ic,jc
INTEGER,DIMENSION(2) :: coords

INTEGER :: idm_nest,jdm_nest,idm_util,jdm_util

REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),ALLOCATABLE :: lon,lat
REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),ALLOCATABLE :: tsk,taux,tauy

REAL(KIND=ESMF_KIND_R4),DIMENSION(xg%idm,xg%jdm) :: tsk_XG
REAL(KIND=ESMF_KIND_R4),DIMENSION(xg%idm,xg%jdm) :: sst_XG
REAL(KIND=ESMF_KIND_R4),DIMENSION(xg%idm,xg%jdm) :: taux_XG
REAL(KIND=ESMF_KIND_R4),DIMENSION(xg%idm,xg%jdm) :: tauy_XG

!===============================================================================

DO domNum = 2,xg % maxDomains

  IF(wrfDomHasStopped(domNum))CYCLE

  IF(wrfDomExists(domNum))THEN

    !==========================================================================>
    ! 1) Indices, tiles and domain information:
    CALL get_ijk_from_grid(dom(domNum)%Ptr,        &
                           ids,ide,jds,jde,kds,kde,&
                           ims,ime,jms,jme,kms,kme,&
                           ips,ipe,jps,jpe,kps,kpe)

    ! WRF returns staggered dimensions here; we want non-staggered, 
    ! so adjust accordingly:
    ide = ide-1 ; ipe = MIN(ide,ipe)
    jde = jde-1 ; jpe = MIN(jde,jpe)

    ! Gather tile indices on root PET:
    CALL MPI_Gather(ips,1,MPI_INTEGER,is,1,MPI_INTEGER,0,mpicomm,ierr)
    CALL MPI_Gather(ipe,1,MPI_INTEGER,ie,1,MPI_INTEGER,0,mpicomm,ierr)
    CALL MPI_Gather(jps,1,MPI_INTEGER,js,1,MPI_INTEGER,0,mpicomm,ierr)
    CALL MPI_Gather(jpe,1,MPI_INTEGER,je,1,MPI_INTEGER,0,mpicomm,ierr)
    !==========================================================================>  

    !==========================================================================>
    ! Allocate arrays:
    IF(localPet==0)THEN

      ALLOCATE(lon(ips:ipe,jps:jpe),lat(ips:ipe,jps:jpe))
      lon(ips:ipe,jps:jpe) = dom(domNum)%Ptr%xlong(ips:ipe,jps:jpe)
      lat(ips:ipe,jps:jpe) = dom(domNum)%Ptr%xlat(ips:ipe,jps:jpe)

      ALLOCATE(tsk(ids:ide,jds:jde))
      ALLOCATE(taux(ids:ide,jds:jde))
      ALLOCATE(tauy(ids:ide,jds:jde))
    
    ELSE
  
      ALLOCATE(tsk(ips:ipe,jps:jpe))
      ALLOCATE(taux(ips:ipe,jps:jpe))
      ALLOCATE(tauy(ips:ipe,jps:jpe))

    ENDIF
    !==========================================================================>

    !==========================================================================>
    ! Gather XG fields on root PET:

    ! Skin temperature from atmosphere model (use for land points only):
    CALL ESMF_FieldGather(field   = xg%expField(6,3,1),&
                          farray  = tsk_XG,            &
                          rootPet = 0,                 &
                          vm      = VM,                &
                          rc      = rc)
    IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

    ! SST from ocean model:
    CALL ESMF_FieldGather(field   = xg%expField(1,1,3),&
                          farray  = sst_XG,            &
                          rootPet = 0,                 &
                          vm      = VM,                &
                          rc      = rc)
    IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

    WHERE(sst_XG /= 0.)tsk_XG = sst_XG + 300.15
    !WHERE(sst_XG /= 0.)tsk_XG = sst_XG + 280.15

    ! Wind stress x-component from wave model:
    CALL ESMF_FieldGather(field   = xg%expField(1,1,2),&
                          farray  = taux_XG,           &
                          rootPet = 0,                 &
                          vm      = VM,                &
                          rc      = rc)
    IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

    ! Wind stress y-component from wave model:
    CALL ESMF_FieldGather(field   = xg%expField(2,1,2),&
                          farray  = tauy_XG,           &
                          rootPet = 0,                 &
                          vm      = VM,                &
                          rc      = rc)
    IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

    !==========================================================================>

    !==========================================================================>
    !  Find nest bottom-left corner index on exchange grid:
    IF(localPet == 0)THEN

      coords = MINLOC((lon_XG-lon(ips,jps))**2.&
                     +(lat_XG-lat(ips,jps))**2.)
      ic = coords(1)
      jc = coords(2)

      idm_nest = SIZE(tsk,DIM=1)
      jdm_nest = SIZE(tsk,DIM=2)
    
      ! If nest is coarser than XG, coarsen XG field:
      IF(domNum < xg % refinementLevel)THEN 

        coarsenRatio = 3**(xg%refinementLevel-domNum)

        idm_util = (idm_nest-1)*coarsenRatio+1
        jdm_util = (jdm_nest-1)*coarsenRatio+1

        !- SKIN TEMPERATURE ----------------------------------->
        CALL exchangeGridCoarsenField(tsk_XG(ic:ic+idm_util-1, &
                                             jc:jc+jdm_util-1),&
                                      tsk,coarsenRatio,rc)      

        !- WIND STRESS VECTOR ---------------------------------->
        CALL exchangeGridCoarsenField(taux_XG(ic:ic+idm_util-1, &
                                              jc:jc+jdm_util-1),&
                                      taux,coarsenRatio,rc)

        CALL exchangeGridCoarsenField(tauy_XG(ic:ic+idm_util-1, &
                                              jc:jc+jdm_util-1),&
                                      tauy,coarsenRatio,rc)

      ! If XG is same resolution as nest, copy fields:
      ELSEIF(domNum == xg % refinementLevel)THEN

        tsk  =  tsk_XG(ic:ic+idm_nest-1,jc:jc+jdm_nest-1)
        taux = taux_XG(ic:ic+idm_nest-1,jc:jc+jdm_nest-1)
        tauy = tauy_XG(ic:ic+idm_nest-1,jc:jc+jdm_nest-1)

      ELSE ! If nest is finer than XG, refine XG field 

        refineRatio = 3**(domNum-xg%refinementLevel)

        idm_util = (idm_nest-1)/refineRatio+2
        jdm_util = (jdm_nest-1)/refineRatio+2

        CALL exchangeGridRefineField(tsk_XG(ic:ic+idm_util-1,jc:jc+jdm_util-1),&
                                     tsk,refineRatio,rc)
        IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)
        
        CALL exchangeGridRefineField(taux_XG(ic:ic+idm_util-1,jc:jc+jdm_util-1),&
                                     taux,refineRatio,rc)
        IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

        CALL exchangeGridRefineField(tauy_XG(ic:ic+idm_util-1,jc:jc+jdm_util-1),&
                                     tauy,refineRatio,rc)
        IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

      ENDIF ! domNum<xg%refinementLevel

    ENDIF ! localPet==0
    !==========================================================================>

    CALL tileScatter( tsk,is,ie,js,je,rc)
    CALL tileScatter(taux,is,ie,js,je,rc)
    CALL tileScatter(tauy,is,ie,js,je,rc)

    ! Assign to nest:
    IF(sstFromOcean)THEN
      FORALL(i=ips:ipe,j=jps:jpe,dom(domNum) % Ptr % xland(i,j) > 1.5 &
                                 .AND. tsk(i,j) > 270)
        dom(domNum) % Ptr % tsk(i,j) = tsk(i,j)
      ENDFORALL
    ENDIF
 
    dom(domNum) % Ptr % taux_esmf(ips:ipe,jps:jpe) = taux(ips:ipe,jps:jpe)
    dom(domNum) % Ptr % tauy_esmf(ips:ipe,jps:jpe) = tauy(ips:ipe,jps:jpe)

    IF(localPet == 0)THEN
      DEALLOCATE(lon,lat)
    ENDIF
    DEALLOCATE(tsk,taux,tauy)
    !==========================================================================>

  ENDIF ! wrfDomExists(domNum)
ENDDO ! domNum=2,xg%maxDomains

rc = ESMF_SUCCESS

ENDSUBROUTINE exchangeGridNestForce
!===============================================================================



SUBROUTINE exchangeGridNestFeedback(rc)
!===============================================================================
!
! Gets fields from each WRF nest onto the exchange grid. This routine is 
! called from the run sequence of the coupler after regridding models -> XG,
! and before regridding XG -> models. Workflow is as follows: 
!
!   0) Check if WRF parent domain has any nests and how many;
!   1) Get index, tile and nest domain information;
!   2) Allocate, assign and gather fields to root PET;
!   3) If current nest domain /= XG resolution, refine;
!   4) Find nest bottom-left corner index on echange grid and patch the field.
!
! last edit :: 20120124 ~mc
!
!===============================================================================
USE UWIN_interface_WRF,ONLY:dom,wrfDomExists,wrfDomHasStopped
USE module_domain,   ONLY:get_ijk_from_grid,domain_clockisstoptime
!===============================================================================

INTEGER,INTENT(OUT) :: rc

INTEGER :: domNum      ! Domain number (2 for d02, 3 for d03 etc.)
INTEGER :: refineRatio

! Nest decomposition
INTEGER :: ids,ide,jds,jde,kds,kde ! Total domain space
INTEGER :: ims,ime,jms,jme,kms,kme ! Tile domain space w/ halo points
INTEGER :: ips,ipe,jps,jpe,kps,kpe ! Tile domain space w/o halo points

INTEGER,DIMENSION(0:petCount-1) :: is,ie,js,je

INTEGER :: idm_nest,jdm_nest,idm_util,jdm_util

INTEGER              :: ic,jc
INTEGER,DIMENSION(2) :: coords

REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),ALLOCATABLE :: lon,lat
REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),ALLOCATABLE :: u10,v10,wspd
REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),ALLOCATABLE :: t2,q2
REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),ALLOCATABLE :: rhoa

REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),ALLOCATABLE :: util_farray

INTEGER :: i,j,n,nproc
INTEGER :: ierr,status(MPI_STATUS_SIZE)

!===============================================================================

! 0) Does WRF parent domain have any nests?
CHECK_NESTS: DO domNum = 1,xg % maxDomains
  IF(dom(domNum) % Ptr % num_nests>0)THEN
    wrfDomExists(domNum+1) = .TRUE.
    IF(.NOT. ASSOCIATED(dom(domNum+1) % Ptr))THEN
      dom(domNum+1) % Ptr => dom(domNum) % Ptr % nests(1) % ptr
    ENDIF
  ELSE
    EXIT CHECK_NESTS
  ENDIF
ENDDO CHECK_NESTS

! Turn off inner nests if we reached their stop time:
DO domNum = 1,xg % maxDomains
  IF(wrfDomExists(domNum))THEN
    IF(domain_clockisstoptime(dom(domNum) % Ptr))THEN
      wrfDomHasStopped(domNum) = .TRUE.
    ENDIF
  ENDIF
ENDDO

DO domNum = 2,xg % refinementLevel

  IF(wrfDomHasStopped(domNum))CYCLE

  IF(wrfDomExists(domNum))THEN

    !================================================================>  
    ! 1) Indices, tiles and domain information:
    CALL get_ijk_from_grid(dom(domNum)%Ptr,        &
                           ids,ide,jds,jde,kds,kde,&
                           ims,ime,jms,jme,kms,kme,&
                           ips,ipe,jps,jpe,kps,kpe)

    ! WRF returns staggered dimensions here; 
    ! We want non-staggered, so adjust accordingly:
    ide = ide-1 ; ipe = MIN(ide,ipe)
    jde = jde-1 ; jpe = MIN(jde,jpe)

    ! Gather tile indices on root PET:
    CALL MPI_Gather(ips,1,MPI_INTEGER,is,1,MPI_INTEGER,0,mpicomm,ierr)
    CALL MPI_Gather(ipe,1,MPI_INTEGER,ie,1,MPI_INTEGER,0,mpicomm,ierr)
    CALL MPI_Gather(jps,1,MPI_INTEGER,js,1,MPI_INTEGER,0,mpicomm,ierr)
    CALL MPI_Gather(jpe,1,MPI_INTEGER,je,1,MPI_INTEGER,0,mpicomm,ierr)
    !================================================================>  

    !================================================================>
    ! 2) Allocate, assign and gather fields to root PET:
    IF(localPet == 0)THEN

      ! Exploit the fact that the root PET 
      ! is in SW corner of the domain:
      ALLOCATE(lon(ips:ipe,jps:jpe),lat(ips:ipe,jps:jpe))
      lon(ips:ipe,jps:jpe) = dom(domNum)%Ptr%xlong(ips:ipe,jps:jpe)
      lat(ips:ipe,jps:jpe) = dom(domNum)%Ptr%xlat(ips:ipe,jps:jpe)

      ALLOCATE(u10(ids:ide,jds:jde),v10(ids:ide,jds:jde))
      ALLOCATE( t2(ids:ide,jds:jde), q2(ids:ide,jds:jde))
      ALLOCATE(wspd(ids:ide,jds:jde))
      ALLOCATE(rhoa(ids:ide,jds:jde))

    ELSE

      ALLOCATE(u10(ips:ipe,jps:jpe),v10(ips:ipe,jps:jpe))
      ALLOCATE( t2(ips:ipe,jps:jpe), q2(ips:ipe,jps:jpe))
      ALLOCATE(wspd(ips:ipe,jps:jpe))
      ALLOCATE(rhoa(ips:ipe,jps:jpe))

    ENDIF

    u10(ips:ipe,jps:jpe) = dom(domNum)%Ptr%u10(ips:ipe,jps:jpe)
    v10(ips:ipe,jps:jpe) = dom(domNum)%Ptr%v10(ips:ipe,jps:jpe)

    t2(ips:ipe,jps:jpe)  = dom(domNum)%Ptr%t2(ips:ipe,jps:jpe)
    q2(ips:ipe,jps:jpe)  = dom(domNum)%Ptr%q2(ips:ipe,jps:jpe)

    rhoa(ips:ipe,jps:jpe)  = 1./dom(domNum)%Ptr%alt(ips:ipe,1,jps:jpe)

    wspd(ips:ipe,jps:jpe) = SQRT(u10(ips:ipe,jps:jpe)**2.&
                                +v10(ips:ipe,jps:jpe)**2.)

    ! Gather to root PET:
    CALL tileGather(u10,is,ie,js,je,rc)
    CALL tileGather(v10,is,ie,js,je,rc)
    CALL tileGather(wspd,is,ie,js,je,rc)
    CALL tileGather(t2,is,ie,js,je,rc)
    CALL tileGather(q2,is,ie,js,je,rc)
    CALL tileGather(rhoa,is,ie,js,je,rc)

    !================================================================>

    !================================================================>
    ! 3) If current domain is not on XG resolution, refine;
    !    (only root PET does this part)
    IF(localPet==0)THEN
      IF(domNum<xg%refinementLevel)THEN 

        refineRatio = (xg%refinementLevel-domNum)*3

        idm_nest = SIZE(u10,DIM=1)
        jdm_nest = SIZE(u10,DIM=2)

        idm_util = (idm_nest-1)*refineRatio+1
        jdm_util = (jdm_nest-1)*refineRatio+1

        ALLOCATE(util_farray(idm_util,jdm_util))

        !-------------------------------------------------------------------
        ! x-component of 10-m wind
        CALL exchangeGridRefineField(u10,util_farray,refineRatio,rc)
        IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)
        DEALLOCATE(u10)
        ALLOCATE(u10(idm_util,jdm_util))
        u10 = util_farray
        !-------------------------------------------------------------------

        !-------------------------------------------------------------------
        ! y-component of 10-m wind
        CALL exchangeGridRefineField(v10,util_farray,refineRatio,rc)
        IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)
        DEALLOCATE(v10)
        ALLOCATE(v10(idm_util,jdm_util))
        v10 = util_farray
        !-------------------------------------------------------------------

        !-------------------------------------------------------------------
        ! 10-m wind speed
        CALL exchangeGridRefineField(wspd,util_farray,refineRatio,rc)
        IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)
        DEALLOCATE(wspd)
        ALLOCATE(wspd(idm_util,jdm_util))
        wspd = util_farray
        !-------------------------------------------------------------------

        !-------------------------------------------------------------------
        ! 2-m air temperature
        CALL exchangeGridRefineField(t2,util_farray,refineRatio,rc)
        IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)
        DEALLOCATE(t2)
        ALLOCATE(t2(idm_util,jdm_util))
        t2 = util_farray
        !-------------------------------------------------------------------

        !-------------------------------------------------------------------
        ! 2-m specific humidity
        CALL exchangeGridRefineField(q2,util_farray,refineRatio,rc)
        IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)
        DEALLOCATE(q2)
        ALLOCATE(q2(idm_util,jdm_util))
        q2 = util_farray
        !-------------------------------------------------------------------

        !-------------------------------------------------------------------
        ! air density
        CALL exchangeGridRefineField(rhoa,util_farray,refineRatio,rc)
        IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)
        DEALLOCATE(rhoa)
        ALLOCATE(rhoa(idm_util,jdm_util))
        rhoa = util_farray
        !-------------------------------------------------------------------

        DEALLOCATE(util_farray)

      ENDIF ! domNum<xg%refinementLevel
    ENDIF ! localPet==0
    !================================================================>

    !================================================================>
    ! 4) Find nest bottom-left corner index on echange grid:
    IF(localPet==0)THEN
      coords = MINLOC((lon_XG-lon(ips,jps))**2.+(lat_XG-lat(ips,jps))**2.)
      ic = coords(1)
      jc = coords(2)
    ENDIF

    ! Patch nest on XG:
    CALL exchangeGridPatchNest(1,2,1,u10,ic,jc,rc)  ! u10 for UMWM
    CALL exchangeGridPatchNest(2,2,1,v10,ic,jc,rc)  ! u10 for UMWM
    CALL exchangeGridPatchNest(3,2,1,rhoa,ic,jc,rc) ! rhoa for UMWM
    CALL exchangeGridPatchNest(1,3,1,t2,ic,jc,rc)   ! airtmp for HYCOM
    CALL exchangeGridPatchNest(2,3,1,q2,ic,jc,rc)   ! vapmix for HYCOM
    CALL exchangeGridPatchNest(7,3,1,wspd,ic,jc,rc) ! wndspd for HYCOM
    !================================================================>

    IF(localPet == 0)THEN
      DEALLOCATE(lon,lat)
    ENDIF

    DEALLOCATE(u10,v10,t2,q2,wspd,rhoa)

  ENDIF ! wrfDomExists(domNum)
ENDDO ! domNum=2,xg%maxDomains

rc = ESMF_SUCCESS

ENDSUBROUTINE exchangeGridNestFeedback
!===============================================================================



SUBROUTINE exchangeGridPatchNest(fieldNum,src,tgt,nest_farray,ic,jc,rc)
!===============================================================================
!
! This routine gathers the whole XG expField((fieldNum,src,tgt) on root PET,
! patches the farray from the nest with SW corner on (ic,jc) on XG,
! and finally scatters back to other PETs. This routine is used for patching
! a nest field that is of equivalent resolution and coinciding grid with XG.
!
!===============================================================================

! Dummy arguments:
INTEGER,INTENT(IN)                           :: fieldNum,src,tgt
REAL(ESMF_KIND_R4),DIMENSION(:,:),INTENT(IN) :: nest_farray
INTEGER,INTENT(IN)                           :: ic,jc
INTEGER,INTENT(OUT)                          :: rc

INTEGER :: idnest,jdnest
INTEGER :: i,j

REAL(ESMF_KIND_R4),DIMENSION(xg%idm,xg%jdm) :: xg_farray

!===============================================================================

idnest = SIZE(nest_farray,DIM=1)
jdnest = SIZE(nest_farray,DIM=2)

! Gather XG fields on root PET:
CALL ESMF_FieldGather(field   = xg%expField(fieldNum,src,tgt),&
                      farray  = xg_farray,                    &
                      rootPet = 0,                            &
                      vm      = vm,                           &
                      rc      = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

! Patch onto XG:
IF(localPet == 0)THEN
  xg_farray(ic:ic+idnest-1,jc:jc+jdnest-1) = nest_farray
ENDIF

! Scatter back:
CALL ESMF_FieldScatter(field   = xg%expField(fieldNum,src,tgt),&
                       farray  = xg_farray,                    &
                       rootPet = 0,                            &
                       vm      = vm,                           &
                       rc      = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

rc = ESMF_SUCCESS

ENDSUBROUTINE exchangeGridPatchNest
!===============================================================================



SUBROUTINE tileGather(farray,is,ie,js,je,rc)
!===============================================================================
!  
! This is a blocking MPI_Gather routine that allows gather of uneven tiles.
!
! It is assumed that root PET already contains farray data on its own tile 
! so this part is not handled by this routine.
!
!===============================================================================

! Dummy arguments:
REAL(ESMF_KIND_R4),DIMENSION(:,:),INTENT(INOUT) :: farray
INTEGER,DIMENSION(0:petCount-1),INTENT(IN)      :: is,ie,js,je
INTEGER,INTENT(OUT)                             :: rc

INTEGER :: ierr,status(MPI_STATUS_SIZE)
INTEGER :: n
!===============================================================================

IF(localPet/=0)THEN
  CALL MPI_Send(farray,SIZE(farray),MPI_REAL,0,localPet,mpicomm,ierr)
ELSE
  DO n=1,petCount-1
    CALL MPI_Recv(farray(is(n):ie(n),js(n):je(n)),      &
                  SIZE(farray(is(n):ie(n),js(n):je(n))),&
                  MPI_REAL,n,n,mpicomm,status,ierr)
  ENDDO
ENDIF

rc = ESMF_SUCCESS

ENDSUBROUTINE tileGather
!===============================================================================



SUBROUTINE tileScatter(farray,is,ie,js,je,rc)
!===============================================================================
!  
! This is a blocking MPI_Scatter routine that allows scatter of uneven tiles.
!
!===============================================================================

! Dummy arguments:
REAL(ESMF_KIND_R4),DIMENSION(:,:),INTENT(INOUT) :: farray
INTEGER,DIMENSION(0:petCount-1),INTENT(IN)      :: is,ie,js,je
INTEGER,INTENT(OUT)                             :: rc

INTEGER :: ierr,status(MPI_STATUS_SIZE)
INTEGER :: n
!===============================================================================

IF(localPet/=0)THEN
  CALL MPI_Recv(farray,SIZE(farray),MPI_REAL,0,localPet,mpicomm,status,ierr)
ELSE
  DO n=1,petCount-1
    CALL MPI_Send(farray(is(n):ie(n),js(n):je(n)),      &
                  SIZE(farray(is(n):ie(n),js(n):je(n))),&
                  MPI_REAL,n,n,mpicomm,ierr)
  ENDDO
ENDIF

rc = ESMF_SUCCESS

ENDSUBROUTINE tileScatter
!===============================================================================



SUBROUTINE exchangeGridRefineField(field_src,field_dst,ratio,rc)
!===============================================================================

! Dummy arguments:
REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),INTENT(IN)  :: field_src
REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),INTENT(OUT) :: field_dst
INTEGER(KIND=ESMF_KIND_I4),            INTENT(IN)  :: ratio
INTEGER(KIND=ESMF_KIND_I4),            INTENT(OUT) :: rc

REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),ALLOCATABLE :: field_tmp

INTEGER :: i,j
INTEGER :: i2,j2
INTEGER :: isub,jsub
INTEGER :: im,jm
INTEGER :: im_dst,jm_dst
INTEGER :: im_src,jm_src

REAL(KIND=ESMF_KIND_R4) :: a,b
REAL(KIND=ESMF_KIND_R4) :: w1,w2,w3,w4

!===============================================================================

IF(ratio==1)THEN
  field_dst = field_src
  RETURN
ENDIF

! Get size of source field:
im_src = SIZE(field_src,DIM=1)
jm_src = SIZE(field_src,DIM=2)

! Compute the size of target field:
im = (im_src-1)*ratio+1
jm = (jm_src-1)*ratio+1

! Get size of the target field dummy argument:
im_dst = SIZE(field_dst,DIM=1)
jm_dst = SIZE(field_dst,DIM=2)

IF(im_dst>im.OR.jm_dst>jm)THEN
  WRITE(*,*)'ESMF_ExchangeGridRefineField: ERROR: Source field smaller than required by the target field size:'
  WRITE(*,*)'Source field:                im_src,jm_src = ',im_src,jm_src
  WRITE(*,*)'Interpolates to:                    im,jm  = ',im,jm
  WRITE(*,*)'Target field dummy argument: im_dst,jm_dst = ',im_dst,jm_dst
  rc = ESMF_FAILURE
  RETURN
ENDIF

ALLOCATE(field_tmp(im,jm))

! Outer loop over the coarse grid:
DO j=1,jm_src-1
  DO i=1,im_src-1
    i2 = (i-1)*ratio+1
    j2 = (j-1)*ratio+1
    DO jsub=0,ratio-1
      DO isub=0,ratio-1
        a = 1.-isub/FLOAT(ratio)
        b = 1.-jsub/FLOAT(ratio)
        w1 = a*b
        w2 = b*(1.-a)
        w3 = a*(1.-b)
        w4 = (1.-a)*(1.-b)
        field_tmp(i2+isub,j2+jsub) = w1*field_src(i  ,j  )&
                                    +w2*field_src(i+1,j  )&
                                    +w3*field_src(i  ,j+1)&
                                    +w4*field_src(i+1,j+1)
      ENDDO
    ENDDO
  ENDDO
ENDDO

! Close last row:
DO i=1,im_src-1
  i2 = (i-1)*ratio+1
  DO isub=0,ratio-1
    w1 = 1.-isub/FLOAT(ratio)
    w2 = 1.-w1
    field_tmp(i2+isub,jm) = w1*field_src(i,jm_src)&
                           +w2*field_src(i+1,jm_src)
  ENDDO
ENDDO

! Close last column:
DO j=1,jm_src-1
  j2 = (j-1)*ratio+1
  DO jsub=0,ratio-1
    w1 = 1.-jsub/FLOAT(ratio)
    w2 = 1.-w1
    field_tmp(im,j2+jsub) = w1*field_src(im_src,j)&
                           +w2*field_src(im_src,j+1)
  ENDDO
ENDDO

! Close top-right corner:
field_tmp(im,jm) = field_src(im_src,jm_src)

! Copy interpolated array into dummy argument array:
field_dst(1:im_dst,1:jm_dst) = field_tmp(1:im_dst,1:jm_dst)

DEALLOCATE(field_tmp)

rc = ESMF_SUCCESS

ENDSUBROUTINE exchangeGridRefineField
!===============================================================================



SUBROUTINE exchangeGridCoarsenField(field_src,field_dst,ratio,rc)
!===============================================================================

! Dummy arguments:
REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),INTENT(IN)  :: field_src
REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),INTENT(OUT) :: field_dst
INTEGER(KIND=ESMF_KIND_I4),            INTENT(IN)  :: ratio
INTEGER(KIND=ESMF_KIND_I4),            INTENT(OUT) :: rc

INTEGER :: i,j
INTEGER :: isub,jsub
INTEGER :: im,jm

INTEGER :: stride

REAL(KIND=ESMF_KIND_R4) :: invNumPoints

!===============================================================================

invNumPoints = (1./DBLE(ratio))**2.
stride = ratio-1

im = SIZE(field_dst,DIM=1)
jm = SIZE(field_dst,DIM=2)

! Loop over the coarse grid and average values from the fine grid:
DO j=1,jm
  jsub = (j-1)*ratio+1
  DO i=1,im
    isub = (i-1)*ratio+1 
    field_dst(i,j) = SUM(field_src(isub:isub+stride,jsub:jsub+stride))*invNumPoints
  ENDDO
ENDDO

rc = ESMF_SUCCESS

ENDSUBROUTINE exchangeGridCoarsenField
!===============================================================================



SUBROUTINE UMCM_cplFieldOutputXG(timeStr)
!===============================================================================
USE UMWM_stokes,ONLY:km => lm
!===============================================================================

CHARACTER(LEN=19) :: timeStr

INTEGER :: l,m,n
INTEGER :: rc

INTEGER :: status

INTEGER :: ncid
INTEGER :: xdimid,ydimid,sdxdim,sdydim,sdzdim
INTEGER :: lonid,latid,maskid
INTEGER :: usid,vsid

TYPE(ESMF_grid) :: grid

REAL(KIND=ESMF_KIND_R4),DIMENSION(:,:),ALLOCATABLE    :: output_data

INTEGER,DIMENSION(10,ngc,ngc) :: varid

!===============================================================================

ALLOCATE(output_data(xg%idm,xg%jdm))

IF(localPet==0)THEN

  status=nf90_create('cplout_'//timeStr//'.nc',nf90_clobber,ncid)
  status=nf90_def_dim(ncid,'X',xg%idm,xdimid)
  status=nf90_def_dim(ncid,'Y',xg%jdm,ydimid)

  status = nf90_def_var(ncid,'Longitude',nf90_float,[xdimid,ydimid],lonid)
  status = nf90_def_var(ncid,'Latitude',nf90_float,[xdimid,ydimid],latid)
  status = nf90_def_var(ncid,'landmask',nf90_int,[xdimid,ydimid],maskid)

  DO m=1,ngc
    IF(.NOT.modelIsEnabled(m))CYCLE
    DO n=1,ngc
      IF(.NOT.modelIsEnabled(n))CYCLE
      IF(m==n)CYCLE
      DO l=1,xg%numExpFields(m,n)
        status=nf90_def_var(ncid,xg%expFieldName(l,m,n),nf90_float,[xdimid,ydimid],varid(l,m,n))
      ENDDO
    ENDDO
  ENDDO

  status = nf90_enddef(ncid)
  status = nf90_put_var(ncid,lonid,lon_XG,start=[1,1],count=[xg%idm,xg%jdm])
  status = nf90_put_var(ncid,latid,lat_XG,start=[1,1],count=[xg%idm,xg%jdm])
  status = nf90_put_var(ncid,maskid,NINT(mask_XG),start=[1,1],count=[xg%idm,xg%jdm])

ENDIF

DO m=1,ngc
  IF(.NOT.modelIsEnabled(m))CYCLE
  DO n=1,ngc
    IF(.NOT.modelIsEnabled(n))CYCLE
    IF(m==n)CYCLE
    DO l=1,xg%numExpFields(m,n)
      CALL ESMF_FieldGather(field=xg%expField(l,m,n),&
                            farray=output_data,      &
                            rootPet=0,               &
                            vm=VM,                   &
                            rc=rc)
      IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)
      IF(localPet==0)status=nf90_put_var(ncid,varid(l,m,n),output_data,&
                                         start=[1,1],count=[xg%idm,xg%jdm])
    ENDDO
  ENDDO
ENDDO

DEALLOCATE(output_data)

IF(localPet==0)status=nf90_close(ncid)

ENDSUBROUTINE UMCM_cplFieldOutputXG
!===============================================================================
ENDMODULE UWIN_ExchangeGrid
