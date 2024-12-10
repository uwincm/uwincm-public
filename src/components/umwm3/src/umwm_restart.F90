MODULE UMWM_restart
!======================================================================!
!                                                                      !
! DESCRIPTION: Provides read/write routines for UMWM restart files     !
!                                                                      !
! CONTAINS: restart_read                                               !
!           restart_write                                              !
!                                                                      !
!======================================================================!

USE UMWM_module
USE netcdf
USE UMWM_util,ONLY:raiseException

#ifdef MPI
USE mpi
USE UMWM_mpi
#endif 

!======================================================================!
IMPLICIT NONE
!======================================================================!
CONTAINS



SUBROUTINE restart_read(timeStr)
!======================================================================!

! ARGUMENTS
CHARACTER(LEN=19),INTENT(IN) :: timeStr

CHARACTER(LEN=19) :: timeStrNew

CHARACTER(LEN=9999) :: fileName

INTEGER :: stat
INTEGER :: ncid,ustid,specid

!======================================================================!

IF(nproc == 0)THEN
  WRITE(*,'(A)')'umwm: restart_read: Reading restart file for '//timeStr
ENDIF

timeStrNew = timeStr
timeStrNew(11:11) = '_'

fileName = 'restart/umwmrst_'//timeStrNew//'.nc'

stat = nf90_open(TRIM(fileName),nf90_share,ncid)

IF(stat /= 0)THEN
  IF(nproc == 0)THEN
    CALL raiseException('ABORT','restart_read',nf90_strerror(stat))
    STOP
  ENDIF
ENDIF

stat = nf90_inq_varid(ncid,'F',specid)
stat = nf90_inq_varid(ncid,'ust',ustid)
stat = nf90_get_var(ncid,specid,e(:,:,istart:iend),&
                    start=[1,1,istart],count=[om,pm,iend-istart+1])
stat = nf90_get_var(ncid,ustid,ustar(istart:iend),&
                    start=[istart],count=[iend-istart+1])
stat = nf90_close(ncid)

#ifdef MPI
CALL MPI_Barrier(mpi_comm_world,ierr)
#endif

ENDSUBROUTINE restart_read
!======================================================================!



SUBROUTINE restart_write(timeStr)
!======================================================================!

! ARGUMENTS
CHARACTER(LEN=19),INTENT(IN) :: timeStr
  
INTEGER :: i,o,nn,p
  
INTEGER :: stat
INTEGER :: ncid
INTEGER :: xdimid,fdimid,thdimid
INTEGER :: kid,xid,lonid,latid,freqid,thetaid,ustid,specid
  
REAL,DIMENSION(im) :: lon_tmp,lat_tmp

!======================================================================!

IF(nproc == 0)THEN

  stat = nf90_create('restart/umwmrst_'//timeStr//'.nc',nf90_clobber,ncid)

  stat = nf90_def_dim(ncid,'x',im,xdimid)
  stat = nf90_def_dim(ncid,'f',om,fdimid)
  stat = nf90_def_dim(ncid,'th',pm,thdimid)

  stat = nf90_def_var(ncid,'lon',nf90_float,[xdimid],lonid)
  stat = nf90_put_att(ncid,lonid,name='description',values='Longitude')
  stat = nf90_put_att(ncid,lonid,name='units',values='degrees east')

  stat = nf90_def_var(ncid,'lat',nf90_float,[xdimid],latid)
  stat = nf90_put_att(ncid,latid,name='description',values='Latitude')
  stat = nf90_put_att(ncid,latid,name='units',values='degrees north')

  stat = nf90_def_var(ncid,'frequency',nf90_float,[fdimid],freqid)
  stat = nf90_put_att(ncid,freqid,name='description',values='Frequency')
  stat = nf90_put_att(ncid,freqid,name='units',values='Hz')

  stat = nf90_def_var(ncid,'theta',nf90_float,[thdimid],thetaid)
  stat = nf90_put_att(ncid,thetaid,name='description',values='Directions')
  stat = nf90_put_att(ncid,thetaid,name='units',values='rad')

  stat = nf90_def_var(ncid,'ust',nf90_float,[xdimid],ustid)
  stat = nf90_put_att(ncid,ustid,name='description',values='Friction velocity')
  stat = nf90_put_att(ncid,ustid,name='units',values='m s^-1')

  stat = nf90_def_var(ncid,'wavenumber',nf90_float,[fdimid,xdimid],kid)
  stat = nf90_put_att(ncid,kid,name='description',values='Wavenumber')
  stat = nf90_put_att(ncid,kid,name='units',values='rad m^-1')

  stat = nf90_def_var(ncid,'F',nf90_float,[fdimid,thdimid,xdimid],specid)
  stat = nf90_put_att(ncid,specid,name='description',values='Wave energy spectrum')
  stat = nf90_put_att(ncid,specid,name='units',values='m^4 rad^-1')

  stat = nf90_enddef(ncid)

  ! Fill in lon and lat arrays:
  DO i=1,im
    lon_tmp(i) = lon(mi(i),ni(i))
    lat_tmp(i) = lat(mi(i),ni(i))
  ENDDO

  stat = nf90_put_var(ncid,lonid,lon_tmp)
  stat = nf90_put_var(ncid,latid,lat_tmp)
  stat = nf90_put_var(ncid,freqid,f)
  stat = nf90_put_var(ncid,thetaid,th)

  stat = nf90_close(ncid)

ENDIF

! Loop over processes in order
DO nn = 0,size-1

  ! Write to file if it is my turn
  IF(nproc == nn)THEN

    stat = nf90_open('restart/umwmrst_'//timeStr//'.nc',nf90_write,ncid)
    stat = nf90_inq_dimid(ncid,'x',xdimid)
    stat = nf90_inq_dimid(ncid,'f',fdimid)
    stat = nf90_inq_dimid(ncid,'th',thdimid)
    stat = nf90_inq_varid(ncid,'F',specid)
    stat = nf90_inq_varid(ncid,'wavenumber',kid)
    stat = nf90_inq_varid(ncid,'ust',ustid)
    stat = nf90_put_var(ncid,specid,e(:,:,istart:iend),start=[1,1,istart],count=[om,pm,iend-istart+1])
    stat = nf90_put_var(ncid,kid,k(:,istart:iend),start=[1,istart],count=[om,iend-istart+1])
    stat = nf90_put_var(ncid,ustid,ustar(istart:iend),start=[istart],count=[iend-istart+1])
    stat = nf90_close(ncid)

  ENDIF

  ! This call ensures that all processes wait for each other
#ifdef MPI
  CALL MPI_Barrier(mpi_comm_world,ierr)
#endif

ENDDO

IF(nproc == 0)THEN
  WRITE(*,'(A)')'umwm: restart_write: Restart written to restart/umwmrst_'//timeStr//'.nc'
ENDIF

ENDSUBROUTINE restart_write
!======================================================================!
ENDMODULE UMWM_restart
