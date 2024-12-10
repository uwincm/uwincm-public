MODULE UMWM_io
!======================================================================!
!                                                                      !
! DESCRIPTION: Provides input/output routines for the wave model       !
!                                                                      !
! CONTAINS: input_nc                                                   !
!           output_grid                                                !
!           output_grid_nc                                             !
!           output_spectrum_nc                                         !
!           gatherField                                                !
!           nc_check                                                   !
!                                                                      !
!======================================================================!
USE umwm_module
USE netcdf
!======================================================================!

LOGICAL :: readFile
LOGICAL :: winds,currents,air_density,water_density

!======================================================================!
CONTAINS



SUBROUTINE input_nc(timeStr)
!======================================================================!
!                                                                      !
! DESCRIPTION: Reads input data files for atmospheric and oceanic      !
!              fields                                                  !
!                                                                      !
!======================================================================!
USE UMWM_util,ONLY:remap_mn2i
!======================================================================!

CHARACTER(LEN=19),INTENT(IN) :: timeStr

CHARACTER(LEN=999) :: nc_infile
CHARACTER(LEN=19) :: readStr

INTEGER :: m,n
INTEGER :: ncid,varid

!======================================================================!

readStr = timeStr
readStr(11:11) = '_'

! Forcing fields are input from this file:
nc_infile = 'input/umwmin_'//readStr//'.nc'

! Set the logical switch to .TRUE. only if from file is requested
! for any of the fields:
readFile = winds .OR. currents .OR. air_density .OR. water_density

IF(readFile)THEN
  CALL nc_check(nf90_open(TRIM(nc_infile),nf90_nowrite,ncID))
ENDIF

IF(winds)THEN
  CALL nc_check(nf90_inq_varid(ncid,'uw',varid))
  CALL nc_check(nf90_get_var(ncid,varid,uwf))
  CALL nc_check(nf90_inq_varid(ncid,'vw',varid))
  CALL nc_check(nf90_get_var(ncid,varid,vwf))
ELSE
  wspd = wspd0
  wdir = wdir0
ENDIF

IF(currents)THEN
  CALL nc_check(nf90_inq_varid(ncid,'uc',varid))
  CALL nc_check(nf90_get_var(ncid,varid,ucf))
  CALL nc_check(nf90_inq_varid(ncid,'vc',varid))
  CALL nc_check(nf90_get_var(ncid,varid,vcf))
ELSE
  ucf = uc0
  vcf = vc0
  uc  = uc0
  vc  = vc0
ENDIF

WHERE(mask == 0)
  ucf = 0
  vcf = 0
ENDWHERE

IF(air_density)THEN
  CALL nc_check(nf90_inq_varid(ncid,'rhoa',varid))
  CALL nc_check(nf90_get_var(ncid,varid,rhoa_2d))
ELSE
  rhoa_2d = rhoa0
  rhoa    = rhoa0
ENDIF

IF(water_density)THEN
  CALL nc_check(nf90_inq_varid(ncid,'rhow',varid))
  CALL nc_check(nf90_get_var(ncid,varid,rhow_2d))
ELSE
  rhow_2d = rhow0
  rhow    = rhow0
ENDIF

IF(readFile)THEN
  CALL nc_check(nf90_close(ncid))
ENDIF

! Remap to 1-D arrays:
rhoaf = remap_mn2i(rhoa_2d)
rhowf = remap_mn2i(rhow_2d)

ENDSUBROUTINE input_nc
!======================================================================!



SUBROUTINE output_grid
!======================================================================!
!                                                                      !
! DESCRIPTION: Outputs grid related fields into a NetCDF file.         !
!                                                                      !
!======================================================================!

INTEGER :: ncid,varid
INTEGER :: xdimid,ydimid
INTEGER :: lonid,latid,dlonid,dlatid,dxid,dyid,arid,maskid,did,nprocid
INTEGER :: xid,yid,curvid

!======================================================================!

IF(nproc == 0)THEN

  CALL nc_check(nf90_create('output/umwmout.grid',nf90_clobber,ncid))
  CALL nc_check(nf90_def_dim(ncid,'x',mm,xdimid))
  CALL nc_check(nf90_def_dim(ncid,'y',nm,ydimid))
  CALL nc_check(nf90_def_var(ncid,'lon',nf90_float,[xdimid,ydimid],lonid))
  CALL nc_check(nf90_def_var(ncid,'lat',nf90_float,[xdimid,ydimid],latid))
  CALL nc_check(nf90_def_var(ncid,'xx',nf90_float,[xdimid,ydimid],xid))
  CALL nc_check(nf90_def_var(ncid,'yy',nf90_float,[xdimid,ydimid],yid))
  CALL nc_check(nf90_def_var(ncid,'dlon',nf90_float,[xdimid,ydimid],dlonid))
  CALL nc_check(nf90_def_var(ncid,'dlat',nf90_float,[xdimid,ydimid],dlatid))
  CALL nc_check(nf90_def_var(ncid,'dx',nf90_float,[xdimid,ydimid],dxid))
  CALL nc_check(nf90_def_var(ncid,'dy',nf90_float,[xdimid,ydimid],dyid))
  CALL nc_check(nf90_def_var(ncid,'curvature',nf90_float,[xdimid,ydimid],curvid))
  CALL nc_check(nf90_def_var(ncid,'area',nf90_float,[xdimid,ydimid],arid))
  CALL nc_check(nf90_def_var(ncid,'depth',nf90_float,[xdimid,ydimid],did))
  CALL nc_check(nf90_def_var(ncid,'seamask',nf90_int,[xdimid,ydimid],maskid))
  CALL nc_check(nf90_def_var(ncid,'nproc',nf90_int,[xdimid,ydimid],nprocid))
  CALL nc_check(nf90_enddef(ncid))
  CALL nc_check(nf90_put_var(ncid,lonid,lon))
  CALL nc_check(nf90_put_var(ncid,latid,lat))
  CALL nc_check(nf90_put_var(ncid,xid,x))
  CALL nc_check(nf90_put_var(ncid,yid,y))
  CALL nc_check(nf90_put_var(ncid,dlonid,dlon))
  CALL nc_check(nf90_put_var(ncid,dlatid,dlat))
  CALL nc_check(nf90_put_var(ncid,dxid,dx_2d))
  CALL nc_check(nf90_put_var(ncid,dyid,dy_2d))
  CALL nc_check(nf90_put_var(ncid,curvid,curv))
  CALL nc_check(nf90_put_var(ncid,arid,ar_2d))
  CALL nc_check(nf90_put_var(ncid,maskid,mask))
  CALL nc_check(nf90_put_var(ncid,did,d_2d))
  CALL nc_check(nf90_put_var(ncid,nprocid,nproc_out))
  CALL nc_check(nf90_close(ncid))

  WRITE(*,'(A)')'umwm: output_grid: Grid definition written in output/umwmout.grid'

ENDIF

ENDSUBROUTINE output_grid
!=======================================================================



SUBROUTINE output_spectrum_nc(timeStr)
!======================================================================!
!                                                                      !
! DESCRIPTION: Writes out model spectrum output in a netCDF format     !
!                                                                      !
!======================================================================!

! ARGUMENTS:
CHARACTER(LEN=19),INTENT(IN) :: timeStr

CHARACTER(LEN=19),SAVE :: saveTimeStr

CHARACTER(LEN=9999) :: spectrumOutputFile

CHARACTER(LEN=2) :: coord_input

INTEGER,DIMENSION(2) :: xy_coords

INTEGER :: stat
INTEGER :: ncid
INTEGER :: xdimid,ydimid,zdimid,fdimid,thdimid,tdimid,scalarid
INTEGER :: lon_scalarid,lat_scalarid,wspdid,wdirid,timeid
INTEGER :: specid,sinid,sdsid,snlid,freqid,thetaid,wlid
INTEGER :: sdtid,sdvid,sbfid

INTEGER                               :: nn
INTEGER,SAVE                          :: npts = 0
INTEGER,          DIMENSION(999),SAVE :: mspec,nspec,ispec
CHARACTER(LEN= 3),DIMENSION(999),SAVE :: cnn
CHARACTER(LEN=40),DIMENSION(999),SAVE :: spectrumid

REAL :: latspec,lonspec
REAL :: wspdtmp,wdirtmp

INTEGER,SAVE :: counter = 1
LOGICAL,SAVE :: firstRun = .TRUE.

!======================================================================!

IF(firstRun)THEN

  saveTimeStr = timeStr
  saveTimeStr(11:11) = '_'

  ! Open spectrum points list file:
  OPEN(UNIT=21,FILE='namelists/spectrum.nml',statUS='OLD',&
       FORM='FORMATTED',ACCESS='SEQUENTIAL',ERR=100)

  ! Are points given in lat/lon or grid indices?
  READ(UNIT=21,FMT='(2A)')coord_input

  ! Loop over points in the list:
  DO
    npts=npts+1

    IF(coord_input == 'XY')THEN

      READ(UNIT=21,FMT=*,END=100)mspec(npts),nspec(npts),spectrumid(npts)

    ELSEIF(coord_input == 'LL')THEN

      READ(UNIT=21,FMT=*,END=100)lonspec,latspec,spectrumid(npts)
      xy_coords   = MINLOC((lonspec-lon)**2.+(latspec-lat)**2.)
      mspec(npts) = xy_coords(1)
      nspec(npts) = xy_coords(2)

    ELSE

      WRITE(*,'(A)')'umwm: output_nc: ERROR: First line in lists/spectrum.list must contain "XY" or "LL"'
      STOP

    ENDIF

    ispec(npts) = ii(mspec(npts),nspec(npts))
    WRITE(UNIT=cnn(npts),FMT='(I3)')npts
    IF(npts<100)cnn(npts) = '0'//ADJUSTL(cnn(npts))
    IF(npts< 10)cnn(npts) = '0'//ADJUSTL(cnn(npts))

  ENDDO

  100 npts=npts-1
  CLOSE(UNIT=21)

ENDIF

DO nn=1,npts
  IF(ispec(nn) >= istart .AND. ispec(nn) <= iend)THEN

    IF(firstRun)THEN

      ! Create output file:
      spectrumOutputFile = 'output/umwmspc_'//TRIM(spectrumid(nn))//'_'//saveTimeStr//'.nc'

      stat = nf90_create(TRIM(spectrumOutputFile),nf90_clobber,ncid)

      ! Define dimensions:
      stat = nf90_def_dim(ncid,'scalar',1,scalarid)
      stat = nf90_def_dim(ncid,'f',om,fdimid)
      stat = nf90_def_dim(ncid,'th',pm,thdimid)
      stat = nf90_def_dim(ncid,'Time',nf90_unlimited,tdimid)

      ! Define variables:
      stat = nf90_def_var(ncid,'Frequency',nf90_float,[fdimid],freqid)
      stat = nf90_def_var(ncid,'Wavenumber',nf90_float,[fdimid],wlid)
      stat = nf90_def_var(ncid,'Direction',nf90_float,[thdimid],thetaid)
      stat = nf90_def_var(ncid,'Longitude',nf90_float,[scalarid],lon_scalarid)
      stat = nf90_def_var(ncid,'Latitude',nf90_float,[scalarid],lat_scalarid)
      stat = nf90_def_var(ncid,'wspd',nf90_float,[scalarid,tdimid],wspdid)
      stat = nf90_def_var(ncid,'wdir',nf90_float,[scalarid,tdimid],wdirid)
      stat = nf90_def_var(ncid,'F',nf90_float,[fdimid,thdimid,tdimid],specid)
      stat = nf90_def_var(ncid,'Sin',nf90_float,[fdimid,thdimid,tdimid],sinid)
      stat = nf90_def_var(ncid,'Sds',nf90_float,[fdimid,thdimid,tdimid],sdsid)
      stat = nf90_def_var(ncid,'Sdt',nf90_float,[fdimid,tdimid],sdtid)
      stat = nf90_def_var(ncid,'Sdv',nf90_float,[fdimid],sdvid)
      stat = nf90_def_var(ncid,'Sbf',nf90_float,[fdimid],sbfid)
      stat = nf90_def_var(ncid,'Snl',nf90_float,[fdimid,thdimid,tdimid],snlid)

      ! End of definition mode:
      stat = nf90_enddef(ncid)

      ! Fill in static fields:
      stat = nf90_put_var(ncid,freqid,f,start=[1],count=[om])
      stat = nf90_put_var(ncid,wlid,k(:,ispec(nn)),start=[1],count=[om])
      stat = nf90_put_var(ncid,thetaid,th,start=[1],count=[pm])
      stat = nf90_put_var(ncid,lon_scalarid,lon(mspec(nn),nspec(nn)))
      stat = nf90_put_var(ncid,lat_scalarid,lat(mspec(nn),nspec(nn)))
      stat = nf90_put_var(ncid,sdvid,sdv(:,ispec(nn)),start=[1],count=[om])
      stat = nf90_put_var(ncid,sbfid,sbf(:,ispec(nn)),start=[1],count=[om])

    ELSE

      ! Open file for writing:
      spectrumOutputFile = 'output/umwmspc_'//TRIM(spectrumid(nn))//'_'//saveTimeStr//'.nc'

      stat = nf90_open(TRIM(spectrumOutputFile),nf90_write,ncid)

      stat = nf90_inq_varid(ncid,'F',specid)
      stat = nf90_inq_varid(ncid,'Sin',sinid)
      stat = nf90_inq_varid(ncid,'Sds',sdsid)
      stat = nf90_inq_varid(ncid,'Sdt',sdtid)
      stat = nf90_inq_varid(ncid,'Snl',snlid)
      stat = nf90_inq_varid(ncid,'wspd',wspdid)
      stat = nf90_inq_varid(ncid,'wdir',wdirid)

    ENDIF

    ! Fill in variables:
    stat = nf90_put_var(ncid,specid,e(:,:,ispec(nn)),start=[1,1,counter],count=[om,pm,1])
    stat = nf90_put_var(ncid,sinid,ssin(:,:,ispec(nn)),start=[1,1,counter],count=[om,pm,1])
    stat = nf90_put_var(ncid,sdsid,sds(:,:,ispec(nn)),start=[1,1,counter],count=[om,pm,1])
    stat = nf90_put_var(ncid,sdtid,sdt(:,ispec(nn)),start=[1,counter],count=[om,1])
    stat = nf90_put_var(ncid,snlid,snl(:,:,ispec(nn)),start=[1,1,counter],count=[om,pm,1])

    wspdtmp = wspd(ispec(nn))
    wdirtmp = wdir(ispec(nn))

    stat = nf90_put_var(ncid,wspdid,wspdtmp,start=[1,counter])
    stat = nf90_put_var(ncid,wdirid,wdirtmp,start=[1,counter])

    ! Close file:
    stat = nf90_close(ncid)

    WRITE(*,'(A)')'umwm: output_spectrum_nc: Spectrum written to '//TRIM(spectrumOutputFile)

  ENDIF
ENDDO

counter  = counter+1
firstRun = .FALSE.

ENDSUBROUTINE output_spectrum_nc
!======================================================================!



SUBROUTINE output_grid_nc(timeStr)
!======================================================================!
!                                                                      !
! DESCRIPTION: Writes out model gridded output in a netCDF format      !
!                                                                      !
!======================================================================!
USE UMWM_stokes,ONLY:depth,lm,us,vs,ds
!======================================================================!

CHARACTER(LEN=19),INTENT(IN) :: timeStr

CHARACTER(LEN=19) :: timeStrNew

INTEGER :: stat
INTEGER :: ncid
INTEGER :: xdimid,ydimid,zdimid,fdimid,thdimid,tdimid,scalarid
INTEGER :: lonid,latid,maskid,depthid,nprocid,swhid,mwpid
INTEGER :: freqid,thetaid
INTEGER :: wspdid,wdirid
INTEGER :: rhoaid,rhowid
INTEGER :: psimid
INTEGER :: zid,usid,vsid,dsid
INTEGER :: momxid,momyid
INTEGER :: cgmxxid,cgmxyid,cgmyyid
INTEGER :: tfdx1id,tfdy1id
INTEGER :: tfdx2id,tfdy2id
INTEGER :: tfdx3id,tfdy3id
INTEGER :: epsx_atmid, epsy_atmid
INTEGER :: epsx_ocnid, epsy_ocnid
INTEGER :: taux_formid,tauy_formid
INTEGER :: taux_skinid,tauy_skinid
INTEGER :: taux_diagid,tauy_diagid
INTEGER :: taux_ocnid,tauy_ocnid
INTEGER :: taux_botid,tauy_botid
INTEGER :: taux_snlid,tauy_snlid
INTEGER :: tailatmxid,tailatmyid
INTEGER :: tailocnxid,tailocnyid
INTEGER :: dwdid,dwlid,dwpid,mwdid,mwlid,uwndid,vwndid,ucid,vcid,wlid
INTEGER :: cdid,mssid,ustid,sheltid
INTEGER :: dcpid,dcp0id,dcgid,dcg0id
INTEGER :: slimid

INTEGER :: l

INTEGER,SAVE :: counter = 1

REAL,DIMENSION(mm,nm) :: output_field

!======================================================================!

timeStrNew = timeStr
timeStrNew(11:11) = '_'

! Super boring, boiler-plate code follows.

IF(nproc == 0)THEN

  stat = nf90_create('output/umwmout_'//timeStrNew//'.nc',nf90_clobber,ncid)

  stat = nf90_def_dim(ncid,'x',mm,xdimid)
  stat = nf90_def_dim(ncid,'y',nm,ydimid)
  stat = nf90_def_dim(ncid,'f',om,fdimid)
  stat = nf90_def_dim(ncid,'th',pm,thdimid)
  stat = nf90_def_dim(ncid,'Time',nf90_unlimited,tdimid)

  IF(stokes)THEN

    stat = nf90_def_dim(ncid,'z',lm,zdimid)

    stat = nf90_def_var(ncid,'z',nf90_float,[zdimid],zid)
    stat = nf90_put_att(ncid,zid,name='description',values='Depth')
    stat = nf90_put_att(ncid,zid,name='units',values='m')

    stat = nf90_def_var(ncid,'u_stokes',nf90_float,[xdimid,ydimid,zdimid,tdimid],usid)
    stat = nf90_put_att(ncid,usid,name='description',values='Stokes drift x-component')
    stat = nf90_put_att(ncid,usid,name='units',values='m/s')

    stat = nf90_def_var(ncid,'v_stokes',nf90_float,[xdimid,ydimid,zdimid,tdimid],vsid)
    stat = nf90_put_att(ncid,vsid,name='description',values='Stokes drift y-component')
    stat = nf90_put_att(ncid,vsid,name='units',values='m/s')

    stat = nf90_def_var(ncid,'d_stokes',nf90_float,[xdimid,ydimid,tdimid],dsid)
    stat = nf90_put_att(ncid,dsid,name='description',values='Stokes drift e-folding depth')
    stat = nf90_put_att(ncid,dsid,name='units',values='m')

  ENDIF

  stat = nf90_def_var(ncid,'frequency',nf90_float,[fdimid],freqid)
  stat = nf90_put_att(ncid,freqid,name='description',values='Frequency')
  stat = nf90_put_att(ncid,freqid,name='units',values='Hz')

  stat = nf90_def_var(ncid,'theta',nf90_float,[thdimid],thetaid)
  stat = nf90_put_att(ncid,thetaid,name='description',values='Directions')
  stat = nf90_put_att(ncid,thetaid,name='units',values='rad')

  stat = nf90_def_var(ncid,'lon',nf90_float,[xdimid,ydimid,tdimid],lonid)
  stat = nf90_put_att(ncid,lonid,name='description',values='Longitude')
  stat = nf90_put_att(ncid,lonid,name='units',values='degrees East')

  stat = nf90_def_var(ncid,'lat',nf90_float,[xdimid,ydimid,tdimid],latid)
  stat = nf90_put_att(ncid,latid,name='description',values='Latitude')
  stat = nf90_put_att(ncid,latid,name='units',values='degrees North')

  stat = nf90_def_var(ncid,'seamask',nf90_int,[xdimid,ydimid,tdimid],maskid)
  stat = nf90_put_att(ncid,maskid,name='description',values='Seamask')
  stat = nf90_put_att(ncid,maskid,name='units',values='non-dimensional')

  stat = nf90_def_var(ncid,'depth',nf90_float,[xdimid,ydimid,tdimid],depthid)
  stat = nf90_put_att(ncid,depthid,name='description',values='Ocean depth')
  stat = nf90_put_att(ncid,depthid,name='units',values='m')

  stat = nf90_def_var(ncid,'wspd',nf90_float,[xdimid,ydimid,tdimid],wspdid)
  stat = nf90_put_att(ncid,wspdid,name='description',values='Wind speed')
  stat = nf90_put_att(ncid,wspdid,name='units',values='m/s')

  stat = nf90_def_var(ncid,'wdir',nf90_float,[xdimid,ydimid,tdimid],wdirid)
  stat = nf90_put_att(ncid,wdirid,name='description',values='Wind direction')
  stat = nf90_put_att(ncid,wdirid,name='units',values='rad')

  stat = nf90_def_var(ncid,'uc',nf90_float,[xdimid,ydimid,tdimid],ucid)
  stat = nf90_put_att(ncid,ucid,name='description',values='Ocean current, x-component')
  stat = nf90_put_att(ncid,ucid,name='units',values='m/s')

  stat = nf90_def_var(ncid,'vc',nf90_float,[xdimid,ydimid,tdimid],vcid)
  stat = nf90_put_att(ncid,vcid,name='description',values='Ocean current, y-component')
  stat = nf90_put_att(ncid,vcid,name='units',values='m/s')

  stat = nf90_def_var(ncid,'rhoa',nf90_float,[xdimid,ydimid,tdimid],rhoaid)
  stat = nf90_put_att(ncid,rhoaid,name='description',values='Air density')
  stat = nf90_put_att(ncid,rhoaid,name='units',values='kg/m^3')

  stat = nf90_def_var(ncid,'rhow',nf90_float,[xdimid,ydimid,tdimid],rhowid)
  stat = nf90_put_att(ncid,rhowid,name='description',values='Water density')
  stat = nf90_put_att(ncid,rhowid,name='units',values='kg/m^3')

  stat = nf90_def_var(ncid,'psim',nf90_float,[xdimid,ydimid,tdimid],psimid)
  stat = nf90_put_att(ncid,psimid,name='description',values='Universal stability function for momentum')
  stat = nf90_put_att(ncid,psimid,name='units',values='non-dimensional')

  stat = nf90_def_var(ncid,'momx',nf90_float,[xdimid,ydimid,tdimid],momxid)
  stat = nf90_put_att(ncid,momxid,name='description',values='Momentum, x-component')
  stat = nf90_put_att(ncid,momxid,name='units',values='kgm/s')

  stat = nf90_def_var(ncid,'momy',nf90_float,[xdimid,ydimid,tdimid],momyid)
  stat = nf90_put_att(ncid,momyid,name='description',values='Momentum, y-component')
  stat = nf90_put_att(ncid,momyid,name='units',values='kgm/s')

  stat = nf90_def_var(ncid,'cgmxx',nf90_float,[xdimid,ydimid,tdimid],cgmxxid)
  stat = nf90_put_att(ncid,cgmxxid,name='description',values='Cg*Momentum, xx-component')
  stat = nf90_put_att(ncid,cgmxxid,name='units',values='kgm^2/s^2')

  stat = nf90_def_var(ncid,'cgmxy',nf90_float,[xdimid,ydimid,tdimid],cgmxyid)
  stat = nf90_put_att(ncid,cgmxyid,name='description',values='Cg*Momentum, xy-component')
  stat = nf90_put_att(ncid,cgmxyid,name='units',values='kgm^2/s^2')

  stat = nf90_def_var(ncid,'cgmyy',nf90_float,[xdimid,ydimid,tdimid],cgmyyid)
  stat = nf90_put_att(ncid,cgmyyid,name='description',values='Cg*Momentum, yy-component')
  stat = nf90_put_att(ncid,cgmyyid,name='units',values='kgm^2/s^2')

  stat = nf90_def_var(ncid,'shelt',nf90_float,[xdimid,ydimid,tdimid],sheltid)
  stat = nf90_put_att(ncid,sheltid,name='description',values='Sheltering coefficient')
  stat = nf90_put_att(ncid,sheltid,name='units',values='non-dimensional')

  stat = nf90_def_var(ncid,'epsx_atm',nf90_float,[xdimid,ydimid,tdimid],epsx_atmid)
  stat = nf90_put_att(ncid,epsx_atmid,name='description',values='Wave energy growth flux, x-component')
  stat = nf90_put_att(ncid,epsx_atmid,name='units',values='kg/s^3')

  stat = nf90_def_var(ncid,'epsy_atm',nf90_float,[xdimid,ydimid,tdimid],epsy_atmid)
  stat = nf90_put_att(ncid,epsy_atmid,name='description',values='Wave energy growth flux, y-component')
  stat = nf90_put_att(ncid,epsy_atmid,name='units',values='kg/s^3')

  stat = nf90_def_var(ncid,'epsx_ocn',nf90_float,[xdimid,ydimid,tdimid],epsx_ocnid)
  stat = nf90_put_att(ncid,epsx_ocnid,name='description',values='Wave energy dissipation flux, x-component')
  stat = nf90_put_att(ncid,epsx_ocnid,name='units',values='kg/s^3')

  stat = nf90_def_var(ncid,'epsy_ocn',nf90_float,[xdimid,ydimid,tdimid],epsy_ocnid)
  stat = nf90_put_att(ncid,epsy_ocnid,name='description',values='Wave energy dissipation flux, y-component')
  stat = nf90_put_att(ncid,epsy_ocnid,name='units',values='kg/s^3')

  stat = nf90_def_var(ncid,'taux_form',nf90_float,[xdimid,ydimid,tdimid],taux_formid)
  stat = nf90_put_att(ncid,taux_formid,name='description',values='Form drag, x-component')
  stat = nf90_put_att(ncid,taux_formid,name='units',values='N/m^2')

  stat = nf90_def_var(ncid,'tauy_form',nf90_float,[xdimid,ydimid,tdimid],tauy_formid)
  stat = nf90_put_att(ncid,tauy_formid,name='description',values='Form drag, y-component')
  stat = nf90_put_att(ncid,tauy_formid,name='units',values='N/m^2')

  stat = nf90_def_var(ncid,'taux_form_1',nf90_float,[xdimid,ydimid,tdimid],tfdx1id)
  stat = nf90_put_att(ncid,tfdx1id,name='description',values='Form drag, part 1, x-component')
  stat = nf90_put_att(ncid,tfdx1id,name='units',values='N/m^2')

  stat = nf90_def_var(ncid,'tauy_form_1',nf90_float,[xdimid,ydimid,tdimid],tfdy1id)
  stat = nf90_put_att(ncid,tfdy1id,name='description',values='Form drag, part 1, y-component')
  stat = nf90_put_att(ncid,tfdy1id,name='units',values='N/m^2')

  stat = nf90_def_var(ncid,'taux_form_2',nf90_float,[xdimid,ydimid,tdimid],tfdx2id)
  stat = nf90_put_att(ncid,tfdx2id,name='description',values='Form drag, part 2, x-component')
  stat = nf90_put_att(ncid,tfdx2id,name='units',values='N/m^2')

  stat = nf90_def_var(ncid,'tauy_form_2',nf90_float,[xdimid,ydimid,tdimid],tfdy2id)
  stat = nf90_put_att(ncid,tfdy2id,name='description',values='Form drag, part 2, y-component')
  stat = nf90_put_att(ncid,tfdy2id,name='units',values='N/m^2')

  stat = nf90_def_var(ncid,'taux_form_3',nf90_float,[xdimid,ydimid,tdimid],tfdx3id)
  stat = nf90_put_att(ncid,tfdx3id,name='description',values='Form drag, part 3, x-component')
  stat = nf90_put_att(ncid,tfdx3id,name='units',values='N/m^2')

  stat = nf90_def_var(ncid,'tauy_form_3',nf90_float,[xdimid,ydimid,tdimid],tfdy3id)
  stat = nf90_put_att(ncid,tfdy3id,name='description',values='Form drag, part 3, y-component')
  stat = nf90_put_att(ncid,tfdy3id,name='units',values='N/m^2')

  stat = nf90_def_var(ncid,'taux_skin',nf90_float,[xdimid,ydimid,tdimid],taux_skinid)
  stat = nf90_put_att(ncid,taux_skinid,name='description',values='Skin drag, x-component')
  stat = nf90_put_att(ncid,taux_skinid,name='units',values='N/m^2')

  stat = nf90_def_var(ncid,'tauy_skin',nf90_float,[xdimid,ydimid,tdimid],tauy_skinid)
  stat = nf90_put_att(ncid,tauy_skinid,name='description',values='Skin drag, y-component')
  stat = nf90_put_att(ncid,tauy_skinid,name='units',values='N/m^2')

  stat = nf90_def_var(ncid,'taux_diag',nf90_float,[xdimid,ydimid,tdimid],taux_diagid)
  stat = nf90_put_att(ncid,taux_diagid,name='description',values='Diagnostic form drag, x-component')
  stat = nf90_put_att(ncid,taux_diagid,name='units',values='N/m^2')

  stat = nf90_def_var(ncid,'tauy_diag',nf90_float,[xdimid,ydimid,tdimid],tauy_diagid)
  stat = nf90_put_att(ncid,tauy_diagid,name='description',values='Diagnostic form drag, y-component')
  stat = nf90_put_att(ncid,tauy_diagid,name='units',values='N/m^2')

  stat = nf90_def_var(ncid,'taux_ocn',nf90_float,[xdimid,ydimid,tdimid],taux_ocnid)
  stat = nf90_put_att(ncid,taux_ocnid,name='description',&
                      values='Momentum flux from breaking waves to ocean top, x-component')
  stat = nf90_put_att(ncid,taux_ocnid,name='units',values='N/m^2')

  stat = nf90_def_var(ncid,'tauy_ocn',nf90_float,[xdimid,ydimid,tdimid],tauy_ocnid)
  stat = nf90_put_att(ncid,tauy_ocnid,name='description',&
                      values='Momentum flux from breaking waves to ocean top, y-component')
  stat = nf90_put_att(ncid,tauy_ocnid,name='units',values='N/m^2')

  stat = nf90_def_var(ncid,'taux_bot',nf90_float,[xdimid,ydimid,tdimid],taux_botid)
  stat = nf90_put_att(ncid,taux_botid,name='description',&
                      values='Momentum flux from waves to ocean bottom, x-component')
  stat = nf90_put_att(ncid,taux_botid,name='units',values='N/m^2')

  stat = nf90_def_var(ncid,'tauy_bot',nf90_float,[xdimid,ydimid,tdimid],tauy_botid)
  stat = nf90_put_att(ncid,tauy_botid,name='description',&
                      values='Momentum flux from waves to ocean bottom, y-component')
  stat = nf90_put_att(ncid,tauy_botid,name='units',values='N/m^2')

  stat = nf90_def_var(ncid,'taux_snl',nf90_float,[xdimid,ydimid,tdimid],taux_snlid)
  stat = nf90_put_att(ncid,taux_snlid,name='description',&
                      values='Momentum flux due to Snl, x-component')
  stat = nf90_put_att(ncid,taux_snlid,name='units',values='N/m^2')

  stat = nf90_def_var(ncid,'tauy_snl',nf90_float,[xdimid,ydimid,tdimid],tauy_snlid)
  stat = nf90_put_att(ncid,tauy_snlid,name='description',&
                      values='Momentum flux due to Snl, y-component')
  stat = nf90_put_att(ncid,tauy_snlid,name='units',values='N/m^2')

  stat = nf90_def_var(ncid,'tailatmx',nf90_float,[xdimid,ydimid,tdimid],tailatmxid)
  stat = nf90_put_att(ncid,tailatmxid,name='description',&
                      values='Atmosphere tail stress part, x-component')
  stat = nf90_put_att(ncid,tailatmxid,name='units',values='N/m^2')

  stat = nf90_def_var(ncid,'tailatmy',nf90_float,[xdimid,ydimid,tdimid],tailatmyid)
  stat = nf90_put_att(ncid,tailatmyid,name='description',&
                      values='Atmosphere tail stress part, y-component')
  stat = nf90_put_att(ncid,tailatmyid,name='units',values='N/m^2')

  stat = nf90_def_var(ncid,'tailocnx',nf90_float,[xdimid,ydimid,tdimid],tailocnxid)
  stat = nf90_put_att(ncid,tailocnxid,name='description',&
                      values='Ocean tail stress part, x-component')
  stat = nf90_put_att(ncid,tailocnxid,name='units',values='N/m^2')

  stat = nf90_def_var(ncid,'tailocny',nf90_float,[xdimid,ydimid,tdimid],tailocnyid)
  stat = nf90_put_att(ncid,tailocnyid,name='description',&
                      values='Ocean tail stress part, y-component')
  stat = nf90_put_att(ncid,tailocnyid,name='units',values='N/m^2')

  stat = nf90_def_var(ncid,'cd',nf90_float,[xdimid,ydimid,tdimid],cdid)
  stat = nf90_put_att(ncid,cdid,name='description',values='Drag coefficient of air')
  stat = nf90_put_att(ncid,cdid,name='units',values='non-dimensional')

  stat = nf90_def_var(ncid,'ust',nf90_float,[xdimid,ydimid,tdimid],ustid)
  stat = nf90_put_att(ncid,ustid,name='description',values='Friction velocity of air')
  stat = nf90_put_att(ncid,ustid,name='units',values='m/s')

  stat = nf90_def_var(ncid,'swh',nf90_float,[xdimid,ydimid,tdimid],swhid)
  stat = nf90_put_att(ncid,swhid,name='description',values='Significant wave height')
  stat = nf90_put_att(ncid,swhid,name='units',values='m')

  stat = nf90_def_var(ncid,'mss',nf90_float,[xdimid,ydimid,tdimid],mssid)
  stat = nf90_put_att(ncid,mssid,name='description',values='Mean-squared slope')
  stat = nf90_put_att(ncid,mssid,name='units',values='non-dimensional')

  stat = nf90_def_var(ncid,'mwp',nf90_float,[xdimid,ydimid,tdimid],mwpid)
  stat = nf90_put_att(ncid,mwpid,name='description',values='Mean wave period')
  stat = nf90_put_att(ncid,mwpid,name='units',values='s')

  stat = nf90_def_var(ncid,'mwl',nf90_float,[xdimid,ydimid,tdimid],mwlid)
  stat = nf90_put_att(ncid,mwlid,name='description',values='Mean wavelength')
  stat = nf90_put_att(ncid,mwlid,name='units',values='m')

  stat = nf90_def_var(ncid,'mwd',nf90_float,[xdimid,ydimid,tdimid],mwdid)
  stat = nf90_put_att(ncid,mwdid,name='description',values='Mean wave direction')
  stat = nf90_put_att(ncid,mwdid,name='units',values='rad')

  stat = nf90_def_var(ncid,'dwp',nf90_float,[xdimid,ydimid,tdimid],dwpid)
  stat = nf90_put_att(ncid,dwpid,name='description',values='Dominant wave period')
  stat = nf90_put_att(ncid,dwpid,name='units',values='s')

  stat = nf90_def_var(ncid,'dwl',nf90_float,[xdimid,ydimid,tdimid],dwlid)
  stat = nf90_put_att(ncid,dwlid,name='description',values='Dominant wavelength')
  stat = nf90_put_att(ncid,dwlid,name='units',values='m')

  stat = nf90_def_var(ncid,'dwd',nf90_float,[xdimid,ydimid,tdimid],dwdid)
  stat = nf90_put_att(ncid,dwdid,name='description',values='Dominant wave direction')
  stat = nf90_put_att(ncid,dwdid,name='units',values='rad')

  stat = nf90_def_var(ncid,'dcp0',nf90_float,[xdimid,ydimid,tdimid],dcp0id)
  stat = nf90_put_att(ncid,dcp0id,name='description',values='Dominant phase speed, intrinsic')
  stat = nf90_put_att(ncid,dcp0id,name='units',values='m/s')

  stat = nf90_def_var(ncid,'dcg0',nf90_float,[xdimid,ydimid,tdimid],dcg0id)
  stat = nf90_put_att(ncid,dcg0id,name='description',values='Dominant group speed, intrinsic')
  stat = nf90_put_att(ncid,dcg0id,name='units',values='m/s')

  stat = nf90_def_var(ncid,'dcp',nf90_float,[xdimid,ydimid,tdimid],dcpid)
  stat = nf90_put_att(ncid,dcpid,name='description',values='Dominant phase speed')
  stat = nf90_put_att(ncid,dcpid,name='units',values='m/s')

  stat = nf90_def_var(ncid,'dcg',nf90_float,[xdimid,ydimid,tdimid],dcgid)
  stat = nf90_put_att(ncid,dcgid,name='description',values='Dominant group speed')
  stat = nf90_put_att(ncid,dcgid,name='units',values='m/s')

  stat = nf90_def_var(ncid,'slim',nf90_float,[xdimid,ydimid,tdimid],slimid)
  stat = nf90_put_att(ncid,slimid,name='description',values='time step limit of S growth')
  stat = nf90_put_att(ncid,slimid,name='units',values='s')

  stat = nf90_enddef(ncid)

ENDIF

IF(nproc == 0)THEN

  stat = nf90_put_var(ncid,freqid,f,start=[1],count=[om])
  stat = nf90_put_var(ncid,thetaid,th,start=[1],count=[pm])
  stat = nf90_put_var(ncid,lonid,lon,start=[1,1,1],count=[mm,nm,1])
  stat = nf90_put_var(ncid,latid,lat,start=[1,1,1],count=[mm,nm,1])
  stat = nf90_put_var(ncid,maskid,mask,start=[1,1,1],count=[mm,nm,1])
  stat = nf90_put_var(ncid,depthid,d_2d,start=[1,1,1],count=[mm,nm,1])

ENDIF

CALL gatherField(wspd(istart:iend),output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,wspdid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(wdir(istart:iend),output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,wdirid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(uc(istart:iend),output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,ucid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(vc(istart:iend),output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,vcid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(rhoa(istart:iend),output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,rhoaid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(rhow(istart:iend),output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,rhowid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(psim(istart:iend),output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,psimid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(momx,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,momxid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(momy,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,momyid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(cgmxx,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,cgmxxid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(cgmxy,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,cgmxyid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(cgmyy,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,cgmyyid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(shelt,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,sheltid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(epsx_atm,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,epsx_atmid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(epsy_atm,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,epsy_atmid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(epsx_ocn,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,epsx_ocnid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(epsy_ocn,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,epsy_ocnid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(taux_form,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,taux_formid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(tauy_form,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,tauy_formid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(taux1,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,tfdx1id,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(tauy1,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,tfdy1id,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(taux2,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,tfdx2id,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(tauy2,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,tfdy2id,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(taux3,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,tfdx3id,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(tauy3,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,tfdy3id,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(taux_skin,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,taux_skinid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(tauy_skin,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,tauy_skinid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(taux_diag,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,taux_diagid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(tauy_diag,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,tauy_diagid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(taux_ocntop,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,taux_ocnid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(tauy_ocntop,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,tauy_ocnid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(taux_ocnbot,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,taux_botid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(tauy_ocnbot,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,tauy_botid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(taux_snl,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,taux_snlid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(tauy_snl,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,tauy_snlid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(tailatmx,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,tailatmxid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(tailatmy,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,tailatmyid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(tailocnx,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,tailocnxid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(tailocny,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,tailocnyid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(cd,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,cdid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(ustar,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,ustid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(ht,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,swhid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(mss,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,mssid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(mwp,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,mwpid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(mwl,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,mwlid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(mwd,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,mwdid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(dwp,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,dwpid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(dwl,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,dwlid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(dwd,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,dwdid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(dcp0,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,dcp0id,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(dcg0,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,dcg0id,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(dcp,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,dcpid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(dcg,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,dcgid,output_field,start=[1,1,1],count=[mm,nm,1])

CALL gatherField(slim,output_field)
IF(nproc == 0)stat = nf90_put_var(ncid,slimid,output_field,start=[1,1,1],count=[mm,nm,1])

IF(stokes)THEN

  IF(nproc == 0)stat = nf90_put_var(ncid,zid,depth,start=[1],count=[lm])

  DO l=1,lm

    CALL gatherField(us(istart:iend,l),output_field)
    IF(nproc == 0)stat = nf90_put_var(ncid,usid,output_field,start=[1,1,l,1],count=[mm,nm,1,1])

    CALL gatherField(vs(istart:iend,l),output_field)
    IF(nproc == 0)stat = nf90_put_var(ncid,vsid,output_field,start=[1,1,l,1],count=[mm,nm,1,1])

  ENDDO

  CALL gatherField(ds(istart:iend),output_field)
  IF(nproc == 0)stat = nf90_put_var(ncid,dsid,output_field,start=[1,1,1],count=[mm,nm,1])

ENDIF

IF(nproc == 0)THEN
  stat = nf90_close(ncid)
  WRITE(UNIT=*,FMT='(A)')'umwm: output_nc: Output written to output/umwmout_'//timeStrNew//'.nc'
ENDIF

ENDSUBROUTINE output_grid_nc
!======================================================================!



SUBROUTINE gatherField(field,field_mn)
!======================================================================!
!                                                                      !
! DESCRIPTION: This subroutine gathers a field on root processor and   !
!              remaps it on a 2-D array.                               !
!                                                                      !
!======================================================================!
#ifdef MPI
USE mpi
USE UMWM_mpi
#endif
USE UMWM_util,ONLY:remap_i2mn
USE, INTRINSIC :: IEEE_ARITHMETIC

!======================================================================!

REAL,DIMENSION(istart:iend),INTENT(IN)  :: field
REAL,DIMENSION(mm,nm),      INTENT(OUT) :: field_mn

REAL,DIMENSION(imm) :: field_ii

INTEGER :: m,n,nn

REAL :: NaN

!======================================================================!

field_ii = IEEE_VALUE(NaN,IEEE_QUIET_NAN)

#ifdef MPI
IF(mpiIsBlocking)THEN

  IF(nproc/=0)THEN
    CALL MPI_Send(field(istart:iend),ilen,MPI_REAL,&
                  0,nproc,mpi_comm_world,ierr)
  ELSE
    DO nn=1,size-1
      CALL MPI_Recv(field_ii(istart_(nn):iend_(nn)),ilen_(nn),&
                    MPI_REAL,nn,nn,mpi_comm_world,status,ierr)
    ENDDO
  ENDIF

ELSE

 ! Non-blocking gather:
  CALL gatherArrayRank1(field,field_ii(1:im))

ENDIF
#endif

IF(nproc == 0)THEN
  field_ii(istart:iend) = field(istart:iend)
  field_mn = remap_i2mn(field_ii)
ENDIF

ENDSUBROUTINE gatherField
!======================================================================!



SUBROUTINE nc_check(stat)
!======================================================================!
!                                                                      !
! DESCRIPTION: Checks for NetCDF errors and if any, print and abort    !
!                                                                      !
!======================================================================!

INTEGER,INTENT(IN) :: stat
!======================================================================!

IF(stat /= nf90_noerr)THEN
  WRITE(*,*)'Error in NetCDF I/O'
  WRITE(*,*)TRIM(nf90_strerror(stat))
  STOP
ENDIF

ENDSUBROUTINE nc_check
!======================================================================!
ENDMODULE UMWM_io
