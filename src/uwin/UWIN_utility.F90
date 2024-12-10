#define FILENAME "UWIN_utility.F90"
MODULE UWIN_utility
!==============================================================================>
USE ESMF
USE netcdf
USE UWIN_constants
USE UWIN_global
!==============================================================================>
IMPLICIT NONE

PRIVATE

PUBLIC :: GridRotation
PUBLIC :: GridSpacing
PUBLIC :: RotateVector
PUBLIC :: FieldCreateFromField
PUBLIC :: FieldWriteNetCDF
PUBLIC :: nc_check

!==============================================================================>
CONTAINS



FUNCTION GridRotation(lon,lat,rc) RESULT(alpha)
!==============================================================================>

REAL(ESMF_KIND_R4),DIMENSION(:,:),INTENT(IN) :: lon
REAL(ESMF_KIND_R4),DIMENSION(:,:),INTENT(IN) :: lat

INTEGER(ESMF_KIND_I4),INTENT(OUT),OPTIONAL   :: rc

REAL(ESMF_KIND_R4),DIMENSION(:,:),ALLOCATABLE :: alpha
REAL(ESMF_KIND_R4),DIMENSION(:,:),ALLOCATABLE :: lonscale

INTEGER(ESMF_KIND_I4) :: im,jm
INTEGER(ESMF_KIND_I4) :: i,j

!==============================================================================>

! Get field size:
im = SIZE(lon,DIM=1)
jm = SIZE(lon,DIM=2)

ALLOCATE(lonscale(im,jm),alpha(im,jm))

! Check if lon and lat arrays have the same shape:
IF(im /= SIZE(lat,DIM=1) .OR. jm /= SIZE(lat,DIM=2))THEN

  CALL ESMF_LogSetError(ESMF_FAILURE,                      &
                        msg        =  ESMF_LOGERR_PASSTHRU,&
                        line       =  __LINE__,            &
                        file       = FILENAME,             &
                        rcToReturn = rc)

  RETURN

ENDIF

! Longitudinal scaling factor as function of latitude:
lonscale = ABS(COS(d2r*lat))

! Calculate grid rotation relative to East:
DO j = 2,jm-1
  DO i = 2,im-1
    alpha(i,j) = ATAN2(lat(i+1,j)-lat(i-1,j),&
                      (lon(i+1,j)-lon(i-1,j))*lonscale(i,j))
  ENDDO
ENDDO

! West and East boundaries:
DO j = 2,jm-1
  alpha(1,j)  = ATAN2(lat(2,j)-lat(1,j),&
                     (lon(2,j)-lon(1,j))*lonscale(1,j))
  alpha(im,j) = ATAN2(lat(im,j)-lat(im-1,j),&
                     (lon(im,j)-lon(im-1,j))*lonscale(im,j))
ENDDO

! South and North boundaries:
DO i=2,im-1
  alpha(i,1)  = ATAN2(lat(i+1,1)-lat(i-1,1),&
                     (lon(i+1,1)-lon(i-1,1))*lonscale(i,1))
  alpha(i,jm) = ATAN2(lat(i+1,jm)-lat(i-1,jm),&
                     (lon(i+1,jm)-lon(i-1,jm))*lonscale(i,jm))
ENDDO

! Corner cells:
alpha(1,1)   = ATAN2(lat(2,1)-lat(1,1),&
                    (lon(2,1)-lon(1,1))*lonscale(1,1))
alpha(im,1)  = ATAN2(lat(im,1)-lat(im-1,1),&
                    (lon(im,1)-lon(im-1,1))*lonscale(im,1))
alpha(1,jm)  = ATAN2(lat(2,jm)-lat(1,jm),&
                    (lon(2,jm)-lon(1,jm))*lonscale(1,jm))
alpha(im,jm) = ATAN2(lat(im,jm)-lat(im-1,jm),&
                    (lon(im,jm)-lon(im-1,jm))*lonscale(im,jm))

DEALLOCATE(lonscale)

rc = ESMF_SUCCESS

ENDFUNCTION GridRotation
!==============================================================================>



SUBROUTINE RotateVector(u,v,alpha,urot,vrot)
!==============================================================================>
!
! Rotates vector (u,v) by angle alpha.
!
!==============================================================================>

REAL(ESMF_KIND_R4),DIMENSION(:,:),INTENT(IN) :: u
REAL(ESMF_KIND_R4),DIMENSION(:,:),INTENT(IN) :: v
REAL(ESMF_KIND_R4),DIMENSION(:,:),INTENT(IN) :: alpha
REAL(ESMF_KIND_R4),DIMENSION(:,:),INTENT(OUT) :: urot
REAL(ESMF_KIND_R4),DIMENSION(:,:),INTENT(OUT) :: vrot

!==============================================================================>

urot = u*COS(alpha)-v*SIN(alpha)
vrot = u*SIN(alpha)+v*COS(alpha)

ENDSUBROUTINE RotateVector
!==============================================================================>



SUBROUTINE GridSpacing(lon,lat,dx,dy,rc)
!==============================================================================>

REAL(ESMF_KIND_R4),DIMENSION(:,:),INTENT(IN), OPTIONAL :: lon
REAL(ESMF_KIND_R4),DIMENSION(:,:),INTENT(IN), OPTIONAL :: lat
REAL(ESMF_KIND_R4),DIMENSION(:,:),INTENT(OUT),OPTIONAL :: dx
REAL(ESMF_KIND_R4),DIMENSION(:,:),INTENT(OUT),OPTIONAL :: dy
INTEGER(ESMF_KIND_I4),            INTENT(OUT),OPTIONAL :: rc

REAL(ESMF_KIND_R4),DIMENSION(:,:),ALLOCATABLE :: dlon,dlat

INTEGER(ESMF_KIND_I4) :: im,jm
INTEGER(ESMF_KIND_I4) :: i,j

!==============================================================================>

im = SIZE(lon,DIM=1)
jm = SIZE(lon,DIM=2)

ALLOCATE(dlon(im,jm),dlat(im,jm))

DO j=1,jm
  DO i=2,im-1
    dlon(i,j) = 0.5*(lon(i+1,j)-lon(i-1,j))
  ENDDO
ENDDO

dlon(1,:)  = 2*dlon(2,:)-dlon(3,:)
dlon(im,:) = 2*dlon(im-1,:)-dlon(im-2,:)

DO j=2,jm-1
  DO i=1,im
    dlat(i,j) = 0.5*(lat(i,j+1)-lat(i,j-1))
  ENDDO
ENDDO

dlat(:,1)  = 2*dlat(:,2)-dlat(:,3)
dlat(:,jm) = 2*dlat(:,jm-1)-dlat(:,jm-2)

dx = dlon*twopi*R_earth/360.*ABS(COS(d2r*lat))
dy = dlat*twopi*R_earth/360.

DEALLOCATE(dlon,dlat)

ENDSUBROUTINE GridSpacing
!==============================================================================>



SUBROUTINE FieldWriteNetCDF(field,im,jm,rc)
!==============================================================================>

TYPE(ESMF_Field),INTENT(IN)           :: field
INTEGER,         INTENT(IN)           :: im,jm
INTEGER,         INTENT(OUT),OPTIONAL :: rc

TYPE(ESMF_Grid) :: grid

CHARACTER(LEN=ESMF_MAXSTR) :: name
INTEGER,DIMENSION(2) :: lb,ub

INTEGER :: ncid,xdimid,ydimid,varid,status

REAL(KIND=ESMF_KIND_R4),DIMENSION(im,jm) :: output_data

!==============================================================================>

rc = ESMF_SUCCESS

CALL ESMF_FieldGet(field           = field,    &
!                  grid            = grid,     &
                   name            = name,     &
                   rc=rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

!CALL ESMF_GridGetCoord(grid      = grid,     &
!                       coordDim  = 1,        &
!                       localDE   = 0,        &
!                       farrayPtr = xcoordPtr,&
!                       rc        = rc) 
!IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

!WRITE(0,*)'Shape of Xcoord '//name,SHAPE(xcoordPtr)

!CALL ESMF_GridGetCoord(grid      = grid,     &
!                       coordDim  = 2,        &
!                       localDE   = 0,        &
!                       farrayPtr = ycoordPtr,&
!                       rc        = rc) 
!IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

!WRITE(0,*)'Shape of Ycoord '//name,SHAPE(ycoordPtr)

CALL ESMF_FieldGather(field   = field,      &
                      farray  = output_data,&
                      rootPet = 0,          &
                      vm      = vm,         &
                      rc      = rc)
IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

IF(localPET==0)THEN

  status = nf90_create(TRIM(name)//'.nc',nf90_clobber,ncid)
  status = nf90_def_dim(ncid,'x',im,xdimid)
  status = nf90_def_dim(ncid,'y',jm,ydimid)
  status = nf90_def_var(ncid,TRIM(name),nf90_float,[xdimid,ydimid],varid)
  status = nf90_enddef(ncid)
  status = nf90_put_var(ncid,varid,output_data)
  status = nf90_close(ncid)

ENDIF

ENDSUBROUTINE FieldWriteNetCDF
!==============================================================================>



FUNCTION FieldCreateFromField(fieldIn,name,rc) RESULT(fieldOut)
!==============================================================================>
!
! Returns a new ESMF_Field initialized to zero, based on the input field.
! The resulting field is on the same ESMF_Grid and ESMF_DistGrid as the input 
! field.
! 
!==============================================================================>

  ! ARGUMENTS:
  TYPE(ESMF_Field),INTENT(IN), OPTIONAL :: fieldIn
  CHARACTER(LEN=*),INTENT(IN), OPTIONAL :: name
  INTEGER,         INTENT(OUT),OPTIONAL :: rc
  TYPE(ESMF_Field)                      :: fieldOut

  TYPE(ESMF_Grid)      :: grid
  TYPE(ESMF_ArraySpec) :: arraySpec 
  INTEGER,DIMENSION(1) :: kmin,kmax

  CALL ESMF_FieldGet(field           = fieldIn,  &
                     grid            = grid,     &
                     arrayspec       = arraySpec,&
                     ungriddedLBound = kmin,     &
                     ungriddedUBound = kmax,     &
                     rc              = rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

  ! Create:
  fieldOut = ESMF_FieldCreate(grid            = grid,                  &
                              arrayspec       = arraySpec,             &
                              indexflag       = ESMF_INDEX_GLOBAL,     &
                              staggerLoc      = ESMF_STAGGERLOC_CENTER,&
                              totalLwidth     = [1,1],                 &
                              totalUwidth     = [1,1],                 &
                              ungriddedLBound = kmin,                  &
                              ungriddedUBound = kmax,                  &
                              name            = name,                  &
                              rc              = rc)
  IF(rc/=ESMF_SUCCESS)CALL ESMF_Finalize(rc=rc,endflag=ESMF_END_ABORT)

ENDFUNCTION FieldCreateFromField
!==============================================================================>



SUBROUTINE nc_check(status)
!==============================================================================>
INTEGER,INTENT(IN) :: status
!==============================================================================>

IF(status/=nf90_noerr)THEN
  WRITE(*,*)'Error in NetCDF I/O'
  WRITE(*,*)TRIM(nf90_strerror(status))
  STOP
ENDIF

ENDSUBROUTINE nc_check
!==============================================================================!
ENDMODULE UWIN_utility
