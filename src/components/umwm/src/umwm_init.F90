MODULE UMWM_init
!======================================================================!
#ifdef MPI
USE mpi
#endif
USE UMWM_module

IMPLICIT NONE

CHARACTER(LEN=1) :: remap_dir

!======================================================================!
CONTAINS



SUBROUTINE environment(option)
!======================================================================!
!                                                                      !
! DESCRIPTION: Sets up processing environment; MPI in case of parallel ! 
!              run or sets proc and size to 0 and 1 in case of serial  !
!              run                                                     !
!                                                                      !
!======================================================================!

CHARACTER(LEN=4),INTENT(IN) :: option

!======================================================================!

IF(option == 'INIT')THEN
#ifdef MPI
#ifndef ESMF
  CALL MPI_Init(ierr)                           ! Initialize MPI
#endif
  CALL MPI_Comm_rank(mpi_comm_world,nproc,ierr) ! Who am I?
  CALL MPI_Comm_size(mpi_comm_world,size,ierr)  ! How many processes?
#else
  nproc=0
  size=1
#endif
ENDIF

IF(option == 'STOP')THEN
#if defined(MPI) && !defined(ESMF)
  CALL MPI_Barrier(mpi_comm_world,ierr) ! Wait for all 
  CALL MPI_Finalize(ierr)               ! Finalize MPI
#endif
ENDIF

ENDSUBROUTINE environment
!======================================================================!



SUBROUTINE greeting
!======================================================================!
!                                                                      !
! DESCRIPTION: Prints a greeting and version number on screen.         !
!                                                                      !
!======================================================================!
USE netcdf
!======================================================================!

IF(nproc == 0)THEN

  WRITE(*,'(A)')
  WRITE(*,'(A)')' University of Miami Wave Model v'//version
  WRITE(*,'(A)')'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
  WRITE(*,'(A)')
  WRITE(*,'(A)')' Compiled using NetCDF library version '//TRIM(nf90_inq_libvers())
  WRITE(*,'(A)')

ENDIF

ENDSUBROUTINE greeting
!======================================================================!



SUBROUTINE nmlread
!======================================================================!
!                                                                      !
! DESCRIPTION: Opens namelist file list.nml and reads runtime input    !
!              parameters                                              !
!                                                                      !
!======================================================================!
!USE UMWM_module,ONLY:startTimeStr => startTimeStr_nml,&
!                     stopTimeStr => stopTimeStr_nml
USE UMWM_module,ONLY:startTimeStr_nml,stopTimeStr_nml
USE UMWM_io,  ONLY:winds,currents,air_density,water_density
USE UMWM_util,ONLY:raiseException
!======================================================================!

LOGICAL :: namelistOK

! Local variables for reading from namelist
CHARACTER(LEN=19) :: startTimeStr,stopTimeStr

!======================================================================!

NAMELIST /DOMAIN/ isGlobal,mm,nm,om,pm,fmin,fmax,fprog,startTimeStr,&
stopTimeStr,dtg,restart

NAMELIST /PHYSICS/ g,nu_air,nu_water,sfct,kappa,z,gustiness,dmin,    &
explim,sin_fac,sin_diss1,sin_diss2,sds_fac,sds_power,mss_fac,snl_fac,&
sdt_fac,sbf_fac,sbp_fac

NAMELIST /GRID/ gridFromFile,delx,dely,topoFromFile,dpt,fillEstuaries,&
fillLakes

NAMELIST /FORCING/ winds,currents,air_density,water_density

NAMELIST /FORCING_CONSTANT/ wspd0,wdir0,uc0,vc0,rhoa0,rhow0

NAMELIST /OUTPUT/ outgrid,outspec,outrst,xpl,ypl,stokes

!======================================================================!

! Read simulation parameters from the main namelist:
OPEN(UNIT=21,FILE='namelists/main.nml',STATUS='OLD',&
     FORM='FORMATTED',ACCESS='SEQUENTIAL',ACTION='READ')
  READ(UNIT=21,NML=DOMAIN)
  READ(UNIT=21,NML=PHYSICS)
  READ(UNIT=21,NML=GRID)
  READ(UNIT=21,NML=FORCING)
  READ(UNIT=21,NML=FORCING_CONSTANT)
  READ(UNIT=21,NML=OUTPUT)
CLOSE(UNIT=21)

! Check namelist values:
namelistOK = .TRUE.
IF(nproc == 0)THEN

  IF(mm < 3 .OR. nm < 3)&
  CALL raiseException('ERROR','nmlread',                             &
                      'Bad value in main.nml: mm and nm must be > 2',&
                      namelistOK)

  IF(MOD(pm,4) /= 0)THEN
    CALL raiseException('ERROR','nmlread',                                 &
                        'Bad value in main.nml: pm must be divisible by 4',&
                        namelistOK)
  ELSE
    IF(MOD(pm,8) /= 0)&
    CALL raiseException('WARNING','nmlread',&
                        'pm should be divisible by 8 for optimal propagation properties')
    
  ENDIF

  IF(fmin <= 0 .OR. fmax <= 0 .OR. fprog <= 0)&
  CALL raiseException('ERROR','nmlread',                                        &
                      'Bad value in main.nml: fmin, fmax and fprog must be > 0',&
                      namelistOK)

  IF(fmin >= fmax)&
  CALL raiseException('ERROR','nmlread',                           &
                      'Bad value in main.nml: fmin must be < fmax',&
                      namelistOK)

  IF(fprog > fmax)&
  CALL raiseException('WARNING','nmlread',&
                      'Bad value in main.nml: fprog must be <= fmax; using highest allowed value')

  IF(dtg <= 0)&
  CALL raiseException('ERROR','nmlread',                       &
                      'Bad value in main.nml: dtg must be > 0',&
                      namelistOK)

  IF( z <= 0)&
  CALL raiseException('ERROR','nmlread',                     &
                      'Bad value in main.nml: z must be > 0',&
                      namelistOK)

  IF(gustiness < 0)THEN
    CALL raiseException('ERROR','nmlread',                                  &
                        'Bad value in main.nml: gustiness must be positive',&
                        namelistOK)
  ELSEIF(gustiness > 0.2)THEN
    CALL raiseException('WARNING','nmlread',&
                        '(Bad) value in main.nml: gustiness > 0.2; proceed with caution')
  ENDIF

  IF(dmin <= 0)&
  CALL raiseException('ERROR','nmlread',                        &
                      'Bad value in main.nml: dmin must be > 0',&
                      namelistOK)

  IF(.NOT.gridFromFile)THEN
    IF(delx <= 0 .OR. dely <= 0)&
    CALL raiseException('ERROR','nmlread',                                 &
                        'Bad value in main.nml: delx and dely must be > 0',&
                        namelistOK)
  ENDIF

  IF(.NOT.topoFromFile)THEN
    IF(dpt <= 0)&
    CALL raiseException('ERROR','nmlread',                       &
                        'Bad value in main.nml: dpt must be > 0',&
                        namelistOK)
  ENDIF

  IF(.NOT.ANY(outgrid == allowedOutputTimes))&
  CALL raiseException('ERROR','nmlread',                                                     &
                      'Bad value in main.nml: outgrid must be 0, 1, 2, 3, 4, 6, 8, 12 or 24',&
                      namelistOK)

  IF(.NOT.ANY(outspec == allowedOutputTimes))&
  CALL raiseException('ERROR','nmlread',                                                     &
                      'Bad value in main.nml: outspec must be 0, 1, 2, 3, 4, 6, 8, 12 or 24',&
                      namelistOK)

  IF(.NOT.ANY(outrst == allowedOutputTimes))&
  CALL raiseException('ERROR','nmlread',                                                    &
                      'Bad value in main.nml: outrst must be 0, 1, 2, 3, 4, 6, 8, 12 or 24',&
                      namelistOK)

  IF(.NOT.namelistOK)THEN
    CALL raiseException('ABORT','nmlread',&
                        'Errors encountered in namelist read (see above)')
    STOP
  ENDIF    

ENDIF

! Copy values from namelist into global variables
startTimeStr_nml = startTimeStr
stopTimeStr_nml = stopTimeStr

ENDSUBROUTINE nmlread
!======================================================================!



SUBROUTINE alloc(option)
!======================================================================!
!                                                                      !
! DESCRIPTION: Allocation of arrays used in UMWM                       !
!                                                                      !
!======================================================================!

INTEGER,INTENT(IN) :: option

!======================================================================!

! Allocate 2-D native arrays:
IF(option==1)THEN 

  ALLOCATE(ar_2d(mm,nm))
  ALLOCATE(curv(mm,nm))
  ALLOCATE(d_2d(mm,nm),dx_2d(mm,nm),dy_2d(mm,nm))
  ALLOCATE(dlon(mm,nm),dlat(mm,nm))
  ALLOCATE(gustu(mm,nm),gustv(mm,nm))
  ALLOCATE(ii(mm,nm))
  ALLOCATE(lat(mm,nm),lon(mm,nm))
  ALLOCATE(x(mm,nm),y(mm,nm))
  ALLOCATE(mask(mm,nm))
  ALLOCATE(nproc_out(mm,nm))
  ALLOCATE(rhoa_2d(mm,nm),rhow_2d(mm,nm))
  ALLOCATE(wspd_2d(mm,nm))
  ALLOCATE(uc_2d(mm,nm),ucb(mm,nm),ucf(mm,nm))
  ALLOCATE(uw(mm,nm),uwb(mm,nm),uwf(mm,nm))
  ALLOCATE(vc_2d(mm,nm),vcb(mm,nm),vcf(mm,nm))
  ALLOCATE(vw(mm,nm),vwb(mm,nm),vwf(mm,nm))
  ALLOCATE(wdir_2d(mm,nm))

! Allocate remapped arrays: 
ELSEIF(option==2)THEN 

  ! 1-D arrays:
  ALLOCATE(dom(om),f(om))
  ALLOCATE(cth(pm),cth2(pm),pl(pm),pr(pm),sth(pm),th(pm))

  ALLOCATE(cth_curv(pm,istart:iend))
  ALLOCATE(sth_curv(pm,istart:iend))

  ALLOCATE(iw(imm),ie(imm),is(imm),in(imm))
  ALLOCATE(iiw(imm),iie(imm),iis(imm),iin(imm))

  ALLOCATE(mi(imm),ni(imm))

  ALLOCATE(oc(istart:iend))

  ! 2-D arrays (remapped):
  ALLOCATE(ar(istart:iend))        ! Grid cell surface area
  ALLOCATE(cd(istart:iend))        ! Air-side drag coefficient
  ALLOCATE(d(imm),dx(imm),dy(imm)) ! Depth and grid cell increments
  ALLOCATE(dxs(istart:iend),dxn(istart:iend)) ! Cell edges
  ALLOCATE(dyw(istart:iend),dye(istart:iend)) ! Cell edges
  ALLOCATE(fcutoff(istart:iend))   ! Cutoff frequency
  ALLOCATE(ht(istart:iend))        ! Significant wave height
  ALLOCATE(mss(istart:iend))       ! Mean-squared slope

  ALLOCATE(shelt(istart:iend))     ! Sheltering coefficient
  shelt = 0

  ! Mean spectrum quantities:
  ALLOCATE(mwd(istart:iend)) ! Direction
  ALLOCATE(mwp(istart:iend)) ! Period
  ALLOCATE(mwl(istart:iend)) ! Wavelength

  ! Dominant spectrum quantities:
  ALLOCATE(dwd(istart:iend))  ! Direction
  ALLOCATE(dwp(istart:iend))  ! Period
  ALLOCATE(dwl(istart:iend))  ! Wavelength
  ALLOCATE(dcp0(istart:iend)) ! Intrinsic phase speed
  ALLOCATE(dcp(istart:iend))  ! Phase speed
  ALLOCATE(dcg0(istart:iend)) ! Intrinsic group speed
  ALLOCATE(dcg(istart:iend))  ! Group speed

  dwd  = 0
  dwp  = 0
  dwl  = 0
  dcp0 = 0
  dcg0 = 0
  dcp  = 0
  dcg  = 0

  ! Inverse area and grid cell increments:
  ALLOCATE(oneovar(istart:iend),oneovdx(istart:iend),oneovdy(istart:iend))

  ALLOCATE(momx(istart:iend),momy(istart:iend)) ! Total wave momentum
  ALLOCATE(cgmxx(istart:iend),cgmxy(istart:iend),cgmyy(istart:iend)) ! Cg*M

  ! Momentum fluxes:
  ALLOCATE(taux(istart:iend),tauy(istart:iend))
  taux = 0; tauy = 0

  ALLOCATE(taux_form(istart:iend),tauy_form(istart:iend))
  taux_form = 0; tauy_form = 0

  ALLOCATE(taux_skin(istart:iend),tauy_skin(istart:iend))
  taux_skin = 0; tauy_skin = 0

  ALLOCATE(taux_diag(istart:iend),tauy_diag(istart:iend))
  taux_diag = 0; tauy_diag = 0

  ALLOCATE(taux_ocntop(istart:iend),tauy_ocntop(istart:iend))
  taux_ocntop = 0; tauy_ocntop = 0

  ALLOCATE(taux_ocnbot(istart:iend),tauy_ocnbot(istart:iend))
  taux_ocnbot = 0; tauy_ocnbot = 0

  ALLOCATE(taux_snl(istart:iend),tauy_snl(istart:iend))
  taux_snl = 0; tauy_snl = 0

  ALLOCATE(epsx_atm(istart:iend), epsy_atm(istart:iend))
  epsx_atm = 0; epsy_atm = 0

  ALLOCATE(epsx_ocn(istart:iend), epsy_ocn(istart:iend))
  epsx_ocn = 0; epsy_ocn = 0

  ALLOCATE(taux1(istart:iend),tauy1(istart:iend))
  ALLOCATE(taux2(istart:iend),tauy2(istart:iend))
  ALLOCATE(taux3(istart:iend),tauy3(istart:iend))

  taux1 = 0; tauy1 = 0
  taux2 = 0; tauy2 = 0
  taux3 = 0; tauy3 = 0

  ALLOCATE(tailatmx(istart:iend),tailatmy(istart:iend))
  ALLOCATE(tailocnx(istart:iend),tailocny(istart:iend))

  ALLOCATE(ustar(istart:iend)) ! Air-side friction velocity

  ALLOCATE(wspd(imm)) ! Wind speed
  wspd = 0

  ALLOCATE(wdir(imm))                       ! Wind direction
  wdir = 0
 
  ALLOCATE(uc(imm),vc(imm)) ! Ocean currents
  uc = 0; vc = 0

  ALLOCATE(rhoa(imm),rhoab(imm),rhoaf(imm)) ! Air density
  ALLOCATE(rhow(imm),rhowb(imm),rhowf(imm)) ! Water density
  ALLOCATE(rhorat(imm))                     ! Air/water density ratio

  ALLOCATE(psim(imm)) ! Integrated stability function for momentum
  psim = 0

  ! 3-D arrays (remapped):
  ALLOCATE(bf1_renorm(om,istart:iend))
  ALLOCATE(bf2_renorm(om,istart:iend))
  ALLOCATE(    cothkd(om,istart:iend))
  ALLOCATE(       dwn(om,istart:iend))
  ALLOCATE(     fkovg(om,istart:iend))
  ALLOCATE(    invcp0(om,istart:iend))
  ALLOCATE(        k4(om,istart:iend))
  ALLOCATE(       kdk(om,istart:iend))
  ALLOCATE(      k3dk(om,istart:iend))
  ALLOCATE( oneoverk4(om,istart:iend))
  ALLOCATE(       sbf(om,istart:iend))
  ALLOCATE(       sdt(om,istart:iend))
  ALLOCATE(       sdv(om,istart:iend))
  ALLOCATE(   snl_arg(om,istart:iend))
  ALLOCATE(        l2(om,istart:iend))
  ALLOCATE(logl2overz(om,istart:iend))

  ALLOCATE(psiml2(om,istart:iend))
  psiml2 = 0

  ! 4-D arrays (remapped):
  ALLOCATE(   ef(om,pm,istart:iend)) ! Wave variance spectrum, forward in time
  ALLOCATE(dummy(om,pm,istart:iend)) ! Dummy array, used in sds, advection and refraction
  ALLOCATE( rotl(om,pm,istart:iend)) ! Anti-clockwise rotation, used in refraction
  ALLOCATE( rotr(om,pm,istart:iend)) ! Clockwise rotation, used in refraction
  ALLOCATE(  sds(om,pm,istart:iend)) ! Wave dissipation sink function
  ALLOCATE(  snl(om,pm,istart:iend)) ! Wave downshifting source/sink function
  ALLOCATE( ssin(om,pm,istart:iend)) ! Wind input source/sink function

ENDIF ! IF(option)

ENDSUBROUTINE alloc
!======================================================================!



SUBROUTINE grid    
!======================================================================!
!                                                                      !
! DESCRIPTION: Defines grid spacing and grid cell areas                !
!                                                                      !
!======================================================================!
USE netcdf
USE UMWM_io,  ONLY:nc_check
USE UMWM_util,ONLY:raiseException
!======================================================================!

LOGICAL :: lonIsContinuous = .TRUE.

INTEGER :: m,n
INTEGER :: ncid,varid,stat

REAL,PARAMETER :: R_earth = 6.371009E6

REAL,DIMENSION(:,:),ALLOCATABLE :: absCosLat,lon_tmp,rotx,roty
REAL,DIMENSION(:,:),ALLOCATABLE :: rlon,rlat

!======================================================================!

IF(gridFromFile)THEN

  stat = nf90_open('input/umwm.gridtopo',nf90_nowrite,ncid)

  IF(stat/=0)THEN

    IF(nproc==0)THEN
      CALL raiseException('WARNING','grid',&
                          'input/umwm.gridtopo not found; trying input/umwm.grid')
    ENDIF
  
    stat = nf90_open('input/umwm.grid',nf90_nowrite,ncid)

    IF(stat/=0)THEN

      IF(nproc==0)THEN
        CALL raiseException('ABORT','grid',&
                            'input/umwm.grid not found. Make sure input files are in place')
      ENDIF
 
      STOP
      
    ENDIF

  ENDIF

  CALL nc_check(nf90_inq_varid(ncid,'lon',varid))
  CALL nc_check(nf90_get_var(ncid,varid,lon))
  CALL nc_check(nf90_inq_varid(ncid,'lat',varid))
  CALL nc_check(nf90_get_var(ncid,varid,lat))
  CALL nc_check(nf90_close(ncid))

  ALLOCATE(absCosLat(mm,nm),lon_tmp(mm,nm),rotx(mm,nm),roty(mm,nm))
  ALLOCATE(rlon(mm,nm),rlat(mm,nm))

  ! Figure out if longitude field is continuous:
  IF(MINVAL(lon) <- 175 .AND. MAXVAL(lon) >175)lonIsContinuous = .FALSE.
      
  lon_tmp = lon

  ! Store original lon array before modifying:
  IF(.NOT.lonIsContinuous)WHERE(lon < 0)lon = lon+360

  DO n=1,nm
    DO m=2,mm-1
      dlon(m,n) = 0.5*(lon(m+1,n)-lon(m-1,n))
    ENDDO
  ENDDO

  dlon(1,:)  = 2*dlon(2,:)-dlon(3,:)
  dlon(mm,:) = 2*dlon(mm-1,:)-dlon(mm-2,:)

  DO n=2,nm-1
    DO m=1,mm
      dlat(m,n) = 0.5*(lat(m,n+1)-lat(m,n-1))
    ENDDO
  ENDDO
  dlat(:,1)  = 2*dlat(:,2)-dlat(:,3)
  dlat(:,nm) = 2*dlat(:,nm-1)-dlat(:,nm-2)

  absCosLat = ABS(COS(dr*lat))

  !dy_2d = dlat*twopi*R_earth/360.
  !dx_2d = dlon*twopi*R_earth/360.*absCosLat

  ! Revert to original lon array:
  lon = lon_tmp

  rlon = lon*twopi/360.
  rlat = lat*twopi/360.

  DO n=1,nm
    DO m=2,mm-1
      dx_2d(m,n) = R_earth*2*asin(sqrt((sin(0.5*(rlat(m+1,n)-rlat(m-1,n))))**2&
                                       +cos(rlat(m-1,n))*cos(rlat(m+1,n))&
                                      *(sin(0.5*(rlon(m+1,n)-rlon(m-1,n))))**2))
    ENDDO
  ENDDO

  dx_2d(1,:)  = 2*dx_2d(2,:)-dx_2d(3,:)
  dx_2d(mm,:) = 2*dx_2d(mm-1,:)-dx_2d(mm-2,:)

  DO n=2,nm-1
    DO m=1,mm
      dy_2d(m,n) = R_earth*2*asin(sqrt((sin(0.5*(rlat(m,n+1)-rlat(m,n-1))))**2&
                                       +cos(rlat(m,n-1))*cos(rlat(m,n+1))&
                                      *(sin(0.5*(rlon(m,n+1)-rlon(m,n-1))))**2))
    ENDDO
  ENDDO

  dy_2d(:,1)  = 2*dy_2d(:,2)-dy_2d(:,3)
  dy_2d(:,nm) = 2*dy_2d(:,nm-1)-dy_2d(:,nm-2)

  ! Compute grid rotation for great circle propagation
  curv = 0
  do n = 1,nm
    do m = 2,mm-1
      curv(m,n) = atan2(sin(rlon(m+1,n)-rlon(m-1,n))*cos(rlat(m+1,n)),&
                        cos(rlat(m-1,n))*sin(rlat(m+1,n))&
                       -sin(rlat(m-1,n))*cos(rlat(m+1,n))*cos(rlon(m+1,n)-rlon(m-1,n)))
    enddo
  enddo

  if(isGlobal)then
    m = 1
    curv(m,:) = atan2(sin(rlon(m+1,:)-rlon(mm,:))*cos(rlat(m+1,:)),&
                      cos(rlat(mm,:))*sin(rlat(m+1,:))&
                     -sin(rlat(mm,:))*cos(rlat(m+1,:))*cos(rlon(m+1,:)-rlon(mm,:)))
    m = mm
    curv(m,:) = atan2(sin(rlon(1,:)-rlon(m-1,:))*cos(rlat(1,:)),&
                      cos(rlat(m-1,:))*sin(rlat(1,:))&
                     -sin(rlat(m-1,:))*cos(rlat(1,:))*cos(rlon(1,:)-rlon(m-1,:)))
  else
    curv(1,:) = curv(2,:)
    curv(mm,:) = curv(mm-1,:)
  endif

  curv = curv-0.5*pi

  DEALLOCATE(absCosLat,lon_tmp,rotx,roty,rlon,rlat)

ELSE ! Use constant value from namelist

  dx_2d = delx
  dy_2d = dely

  curv = 0

  lon = 0
  lat = 0
  dlon = 0
  dlat = 0

  x(1,:) = 0 
  DO m = 2,mm
    x(m,:) = x(m-1,:)+0.5*(dx_2d(m-1,:)+dx_2d(m,:))
  ENDDO

  y(:,1) = 0 
  DO n = 2,nm
    y(:,n) = y(:,n-1)+0.5*(dy_2d(:,n-1)+dy_2d(:,n))
  ENDDO

ENDIF

IF(topoFromFile)THEN ! Read depth field from file

  CALL nc_check(nf90_open('input/umwm.gridtopo',nf90_nowrite,ncid))
  CALL nc_check(nf90_inq_varid(ncid,'z',varid))
  CALL nc_check(nf90_get_var(ncid,varid,d_2d))
  CALL nc_check(nf90_close(ncid))

ELSE ! Use constant value from namelist

  d_2d = dpt 

ENDIF

! Compute cell areas and reciprocals:
ar_2d = dx_2d*dy_2d

IF(nproc == 0)THEN
  WRITE(*,FMT=101)'umwm: grid: dx min/max/mean [m]:     ',&
                  MINVAL(dx_2d),MAXVAL(dx_2d),SUM(dx_2d)/(mm*nm)
  WRITE(*,FMT=101)'umwm: grid: dy min/max/mean [m]:     ',&
                  MINVAL(dy_2d),MAXVAL(dy_2d),SUM(dy_2d)/(mm*nm)
  WRITE(*,FMT=101)'umwm: grid: area min/max/mean [m^2]: ',&
                  MINVAL(ar_2d),MAXVAL(ar_2d),SUM(ar_2d)/(mm*nm)
ENDIF

101 FORMAT(A,3(F15.2,1X))

ENDSUBROUTINE grid
!======================================================================!



SUBROUTINE masks
!======================================================================!
!                                                                      !
! DESCRIPTION: Defines landmasks, and in case of real bathymetry       !
!              removes one-cell wide estuaries and small lakes         !
!                                                                      !
!======================================================================!

LOGICAL :: iterate

INTEGER :: m,n,l,o,p
INTEGER :: exm,exn
INTEGER :: cnt,fillcount

!======================================================================!

! Set masks:
IF(topoFromFile)THEN

  ! Set initial seamask everywhere:
  mask = 1

  ! Set up boundary points:
  mask(:,1)  = 0
  mask(:,nm) = 0

  ! Close E and W edges if limited area:
  IF(.NOT.isGlobal)THEN
    mask(1,:)  = 0
    mask(mm,:) = 0 
  ENDIF

  ! Set land mask where depth is non-negative, and then
  ! set depth to dmin:
  WHERE(d_2d>=0)
    mask = 0
    d_2d = dmin
  ENDWHERE

  ! Make depths positive and limit to dmin:
  d_2d = ABS(d_2d)
  WHERE(d_2d<dmin)d_2d = dmin

  ! Fill estuaries and isolated sea points:
  IF(fillEstuaries)THEN
    fillcount = 0
    iterate = .TRUE.
    DO WHILE(iterate) ! Iterate as long as there are points to be modified
      iterate = .FALSE.
      DO n=2,nm-1 
        DO m=2,mm-1

          IF(mask(m,n)==1)THEN

            cnt = 0
            IF(mask(m-1,n)==1)cnt = cnt+1
            IF(mask(m+1,n)==1)cnt = cnt+1
            IF(mask(m,n-1)==1)cnt = cnt+1
            IF(mask(m,n+1)==1)cnt = cnt+1

            IF(cnt<=1)THEN
              mask(m,n) = 0
              fillcount = fillcount+1
              iterate   = .TRUE.
            ENDIF

          ENDIF ! IF(mask(m,n)==1)
       
        ENDDO
      ENDDO   
    ENDDO ! WHILE loop
    IF(nproc==0)WRITE(*,FMT=100)'umwm: masks: Filled cells with 3-land neighbours,',&
                                fillcount,' cells total.'
  ENDIF 

  ! Discard lakes or unwanted closed basin from the domain:
  IF(fillLakes)THEN
    OPEN(UNIT=24,FILE='namelists/exclude.nml')
    DO
      READ(UNIT=24,FMT=*,END=107)exm,exn
      fillcount = 0
      CALL fill(exm,exn,fillcount)
      IF(nproc==0)WRITE(*,FMT=101)'umwm: masks: Filled closed sea at i,j:',&
                                  exm,exn,', ',fillcount,' cells total.'
    ENDDO
  ENDIF ! IF(fillLakes)

107 CLOSE(UNIT=24)

100 FORMAT(A,I8,A)
101 FORMAT(A,2(I5,1X),A,I8,A)

ELSE 

  ! Set initial mask everywhere:
  mask = 1

  ! Set up boundary points
  mask(:, 1) = 0
  mask(:,nm) = 0

  ! Close E and W edges if limited area:
  IF(.NOT.isGlobal)THEN
    mask( 1,:)  = 0
    mask(mm,:) = 0 
  ENDIF
  
  WHERE(mask==0)d_2d = dmin

ENDIF

! Calculate the upper index for 1-D arrays:
im = COUNT(mask==1)
imm = mm*nm

! Print out summary:
IF(nproc==0)THEN
  WRITE(UNIT=*,FMT=302)imm 
  WRITE(UNIT=*,FMT=303)imm-im,FLOAT(imm-im)/FLOAT(imm)*100.
  WRITE(UNIT=*,FMT=304)im,FLOAT(im)/FLOAT(imm)*100.
  WRITE(UNIT=*,FMT=305)MINVAL(d_2d,mask==1)
  WRITE(UNIT=*,FMT=306)MAXVAL(d_2d,mask==1)
ENDIF

300 FORMAT(9999(A1))
302 FORMAT('umwm: masks: Total number of grid points: ',I9)
303 FORMAT('umwm: masks: Number of land points:       ',I9,', ',F5.1,'%')
304 FORMAT('umwm: masks: Number of sea points:        ',I9,', ',F5.1,'%')
305 FORMAT('umwm: masks: Shallowest point:            ',F9.3,' meters')
306 FORMAT('umwm: masks: Deepest point:               ',F9.3,' meters')

ENDSUBROUTINE masks
!======================================================================!



RECURSIVE SUBROUTINE fill(m,n,fillcount)
!=======================================================================
!                                                                      !
! DESCRIPTION: Mask out enclosed seas chosen by user                   !
!                                                                      !
!======================================================================!

INTEGER,INTENT(IN)    :: m,n
INTEGER,INTENT(INOUT) :: fillcount

!=======================================================================

! Return if reached domain edge:
IF(m==1)RETURN; IF(m==mm)RETURN
IF(n==1)RETURN; IF(n==nm)RETURN

! Fill:
mask(m,n) = 0

fillcount = fillcount+1

! Recurse in all directions:
IF(mask(m-1,n)==1)CALL fill(m-1,n,fillcount)
IF(mask(m+1,n)==1)CALL fill(m+1,n,fillcount)
IF(mask(m,n-1)==1)CALL fill(m,n-1,fillcount)
IF(mask(m,n+1)==1)CALL fill(m,n+1,fillcount)

ENDSUBROUTINE fill
!======================================================================!



SUBROUTINE partition
!======================================================================!
!                                                                      !
! DESCRIPTION: Establish the infrastructure between processes          !
!                                                                      !
!======================================================================!

#ifdef MPI
USE UMWM_mpi
#endif

INTEGER :: i,m,n,nn
INTEGER :: extend
!======================================================================!

#ifndef MPI
istart  = 1
iend    = im
iistart = istart
iiend   = iend
#else

ALLOCATE(istart_(0:size-1),iend_(0:size-1),ilen_(0:size-1))

! Find out what is the length of my part:
im_mod = MOD(im,size)
IF(im_mod==0)THEN
  ilen   = im/size
  istart = nproc*ilen+1
  iend   = istart+ilen-1
ELSE
  ilen   = FLOOR(FLOAT(im)/FLOAT(size))
  istart = nproc*ilen+1
  iend   = istart+ilen-1
  DO nn=1,im_mod
    IF(nproc==nn-1)ilen = ilen+1
  ENDDO
ENDIF

! Adjust start/end boundaries:
IF(nproc<im_mod)THEN
  IF(nproc==0)THEN
    iend = iend+1
  ELSE
    istart = istart+nproc
    iend   = iend+nproc+1 
  ENDIF
ELSE
  istart = istart+im_mod
  iend   = istart+ilen-1
ENDIF

! Which direction for remapping? 
! (matters only in parallel or global mode)
IF(mm>=nm)remap_dir = 'V'
IF(mm<nm)remap_dir  = 'H'

IF(isGlobal)remap_dir = 'V'

!======================================================================!
! ESMF related section;
!
! The code below adjusts the start and end indices of each tile
! because currently ESMF deBlockList accepts only regular rectangular
! domains.

#ifdef ESMF
IF(remap_dir=='H')THEN ! Row-major remapping

  ! Adjust ends first:
  i = 0 ; extend = 0
  OUTER1:DO n=1,nm
    INNER1:DO m=1,mm
      IF(mask(m,n)==1)i = i+1
      IF(iend==i)THEN
        IF(m==mm)EXIT OUTER1
        extend = COUNT(mask(m+1:mm,n)==1)
        iend   = iend+extend
        EXIT OUTER1
      ENDIF
    ENDDO INNER1
  ENDDO OUTER1

  ! Now adjust beginnings (all but proc 0):
  IF(nproc/=0)THEN
    i = 0 ; extend = 0
    OUTER2:DO n=1,nm
      INNER2:DO m=1,mm
        IF(mask(m,n)==1)i = i+1
        IF(istart==i)THEN
          IF(m==1)EXIT OUTER2
          extend = COUNT(mask(m:mm,n)==1)
          istart = istart+extend
          EXIT OUTER2
        ENDIF 
      ENDDO INNER2
    ENDDO OUTER2
  ENDIF

ELSEIF(remap_dir=='V')THEN ! Column-major remapping

  ! Adjust ends first:
  i = 0 ; extend = 0
  OUTER3:DO m=1,mm
    INNER3:DO n=1,nm
      IF(mask(m,n)==1)i = i+1
      IF(iend==i)THEN
        IF(n==nm)EXIT OUTER3
        extend = COUNT(mask(m,n+1:nm)==1)
        iend   = iend+extend
        EXIT OUTER3
      ENDIF 
    ENDDO INNER3
  ENDDO OUTER3

  ! Now adjust beginnings (all but proc 0):
  IF(nproc/=0)THEN
    i = 0 ; extend = 0
    OUTER4:DO m=1,mm
      INNER4:DO n=1,nm
        IF(mask(m,n)==1)i = i+1
        IF(istart==i)THEN
          IF(n==1)EXIT OUTER4
          extend = COUNT(mask(m,n:nm)==1)
          istart = istart+extend
          EXIT OUTER4
        ENDIF
      ENDDO INNER4
    ENDDO OUTER4
  ENDIF

ENDIF

! Adjust tile length:
ilen = iend-istart+1

#endif 

! End of ESMF related section;
!======================================================================!

! Gather tile size information to root process:
CALL MPI_Gather(istart,1,MPI_INTEGER,istart_,1,MPI_INTEGER,0,mpi_comm_world,ierr)
CALL MPI_Gather(iend,1,MPI_INTEGER,iend_,1,MPI_INTEGER,0,mpi_comm_world,ierr)
CALL MPI_Gather(ilen,1,MPI_INTEGER,ilen_,1,MPI_INTEGER,0,mpi_comm_world,ierr)

IF(nproc==0)THEN

  WRITE(*,FMT='(A)')'umwm: partition: Tiling summary:'
  WRITE(*,FMT='(A)')'+---------+------------+----------------+----------------+'
  WRITE(*,FMT='(A)')'|  nproc  |    ilen    |     istart     |      iend      |'
  WRITE(*,FMT='(A)')'+---------+------------+----------------+----------------+'
!                      1234567   1234567890   12345678901234   12345678901234
  DO nn=0,size-1
    WRITE(*,FMT=307)nn,ilen_(nn),istart_(nn),iend_(nn)
  ENDDO

  WRITE(*,FMT='(A)')'+---------+------------+----------------+----------------+'

ENDIF

307 FORMAT('| ',I7,' | ',I10,' | ',2(I14,' | '))

#endif

ENDSUBROUTINE partition
!======================================================================!



SUBROUTINE remap
!======================================================================!
!                                                                      !
! DESCRIPTION: Remaps two-dimensional arrays into one-dimensional      !
!              arrays while leaving out land points. It also assigns   !
!              and links neighboring points that are needed for        !
!              spatial differencing, and builds the communication      !
!              interface between processes;                            !
!                                                                      !
!======================================================================!
#ifdef MPI
USE UMWM_mpi
#endif
!======================================================================!

INTEGER :: i,m,n,nn
INTEGER :: counter, itemp

integer, dimension(:), allocatable :: n_exchange_indices

!======================================================================!
!
! Remapping section:
! Unroll 2-D array into a contiguous 1-D array of sea-only points.
! First part contains only sea-points; the second part contains only
! land-points, which are necessary for grid size information in advection
! routine.
!
! In serial mode this does not matter, but we must pick one:
#ifndef MPI
  remap_dir = 'V'
#endif

! First construct (m,n)->(i) transformation:
ii = 0
i  = 0 
IF(remap_dir == 'H')THEN ! Column-major

  ! Sea points:
  DO n=1,nm
    DO m=1,mm
      IF(mask(m,n) == 1)THEN
        i = i+1
        ii(m,n) = i
        dx(i) = dx_2d(m,n)
        dy(i) = dy_2d(m,n)
        d(i)  = d_2d(m,n)
      ENDIF
    ENDDO
  ENDDO

  ! Land points:
  DO n=1,nm
    DO m=1,mm
      IF(mask(m,n) == 0)THEN
        i = i+1
        ii(m,n) = i
        dx(i) = dx_2d(m,n)
        dy(i) = dy_2d(m,n)
        d(i)  = d_2d(m,n)
      ENDIF 
    ENDDO
  ENDDO

ELSEIF(remap_dir == 'V')THEN ! Row-major

  ! Sea points:
  DO m=1,mm
    DO n=1,nm
      IF(mask(m,n) == 1)THEN
        i = i+1
        ii(m,n) = i
        dx(i) = dx_2d(m,n)
        dy(i) = dy_2d(m,n)
        d(i)  = d_2d(m,n)
      ENDIF
    ENDDO
  ENDDO

  ! Land points:
  DO m=1,mm
    DO n=1,nm
      IF(mask(m,n) == 0)THEN
        i = i+1
        ii(m,n) = i
        dx(i) = dx_2d(m,n)
        dy(i) = dy_2d(m,n)
        d(i)  = d_2d(m,n)
      ENDIF 
    ENDDO
  ENDDO

ELSE

  WRITE(UNIT=*,FMT=*)'umwm: remap: ERROR - remap_dir must be ''H'' or ''V'''
  STOP

ENDIF 

! Now construct (i)->(m,n) transformation:
mi = 0
ni = 0
DO n=1,nm
  DO m=1,mm
    i = ii(m,n)
    mi(i) = m
    ni(i) = n
  ENDDO
ENDDO

! Neighboring point indices for advection and refraction:
is = 0
in = 0
iw = 0
ie = 0
DO n=2,nm-1
  DO m=2,mm-1
    i = ii(m,n)
    is(i) = ii(m,n-1)
    in(i) = ii(m,n+1)
    iw(i) = ii(m-1,n)
    ie(i) = ii(m+1,n)
  ENDDO
ENDDO

! Adjust periodic boundary:
IF(isGlobal)THEN
  DO n=2,nm-1

    is(ii(1,n)) = ii(1,n-1)
    in(ii(1,n)) = ii(1,n+1)
    iw(ii(1,n)) = ii(mm,n)
    ie(ii(1,n)) = ii(2,n)

    is(ii(mm,n)) = ii(mm,n-1)
    in(ii(mm,n)) = ii(mm,n+1)
    iw(ii(mm,n)) = ii(mm-1,n)
    ie(ii(mm,n)) = ii(1,n)

  ENDDO
ENDIF

! True indices:
! (is,in,iw,ie will be aliased for land points)
iis = is
iin = in
iiw = iw
iie = ie

! Cell edges in x and y:
DO i = istart,iend
  dxn(i) = 0.5*(dx(i)+dx(iin(i)))
  dxs(i) = 0.5*(dx(i)+dx(iis(i)))
  dye(i) = 0.5*(dy(i)+dy(iie(i)))
  dyw(i) = 0.5*(dy(i)+dy(iiw(i)))
ENDDO

#ifdef MPI
! Allocate c, cg, and e, and initialize:
IF(nproc==0)THEN ! Root process

  iistart = istart ! Start point
  itemp = iend     ! End point (first guess)

  DO   
    IF(remap_dir == 'H')iiend = in(itemp)
    IF(remap_dir == 'V')iiend = ie(itemp)
    ! If land point, go one cell back and cycle:
    IF(iiend>im)THEN 
      itemp = itemp-1
      CYCLE
   ! If sea point, exit loop:
    ELSE
      EXIT
    ENDIF
  ENDDO      

ELSEIF(nproc==size-1)THEN ! Last process

  itemp = istart ! Start point (first guess)
  iiend = iend   ! End point

  DO              
    IF(remap_dir == 'H')iistart = is(itemp)
    IF(remap_dir == 'V')iistart = iw(itemp)
    ! If land point, go one cell forward and cycle:
    IF(iistart>im)THEN 
      itemp = itemp+1
      CYCLE
    ! If sea point, exit loop:
    ELSE
      EXIT
    ENDIF
  ENDDO

ELSE ! Everybody else

  itemp = istart ! Start point (first guess)

  DO
    IF(remap_dir == 'H')iistart = is(itemp)
    IF(remap_dir == 'V')iistart = iw(itemp)
    ! If land point, go one cell forward and cycle:
    IF(iistart>im)THEN 
      itemp = itemp+1
      CYCLE
      ! If sea point, exit loop:
    ELSE
      EXIT
    ENDIF
  ENDDO

  itemp = iend ! End point (first guess)

  DO               
    IF(remap_dir == 'H')iiend = in(itemp)
    IF(remap_dir == 'V')iiend = ie(itemp)
    ! If land point, go one cell back and cycle:
    IF(iiend>im)THEN 
      itemp = itemp-1
      CYCLE
    ! If sea point, exit loop:
    ELSE
      EXIT
    ENDIF
  ENDDO      

ENDIF ! IF(nproc==0)

IF(isGlobal)THEN 

  allocate(n_exchange_indices(0))

  first_col_len = 0
  do n = 2, nm-1
    if (mask(1,n) == 1 .and. mask(mm,n) == 1) then
      n_exchange_indices = [n_exchange_indices, n]
      first_col_len = first_col_len + 1
    endif
  enddo
  last_col_len = first_col_len

  allocate(i_exchange_indices(first_col_len))

  ! find west neighbor indices on first processor
  if (nproc == 0) then

    ! find unrolled halo exchange indices
    do n = 1, first_col_len
      i_exchange_indices(n) = ii(1,n_exchange_indices(n))
    enddo

    ! adjust the start halo index for the number 
    ! of water points in easternmost column
    iistart = iistart - last_col_len

    counter = 0
    m = 1
    do n = 2, nm-1
      if (mask(m,n) == 1) then
        i = ii(m,n)
        if (mask(mi(iw(i)),ni(iw(i))) == 0) then
          ! west neighbor is land
          iw(i) = iistart - 1
        else
          ! west neighbor is water
          iw(i) = iistart + counter
          counter = counter + 1
        endif
      endif
    enddo

  endif

  ! find east neighbor indices on last processor
  if (nproc == size-1) then

    ! find unrolled halo exchange indices
    do n = 1, first_col_len
      i_exchange_indices(n) = ii(mm,n_exchange_indices(n))
    enddo

    ! adjust the end halo index for the number 
    ! of water points in easternmost column
    iiend = iiend + first_col_len

    counter = 1
    m = mm
    do n = 2, nm-1
      if (mask(m,n) == 1) then
        i = ii(m,n)
        if (mask(mi(ie(i)),ni(ie(i))) == 0) then
          ! east neighbor is land
          ie(i) = iistart - 1
        else
          ! east neighbor is water
          ie(i) = iend + counter
          counter = counter + 1
        endif
      endif
    enddo

  endif

ENDIF

#endif

! Allocate and add a ghost land point at the beggining: 
ALLOCATE(e(om,pm,iistart-1:iiend))
ALLOCATE(cp0(om,iistart-1:iiend))
ALLOCATE(cg0(om,iistart-1:iiend))
ALLOCATE(k(om,istart:iend))

! Initialize:
e = TINY(e)

#ifdef MPI
! Distribute iistart and iiend to everyone:
ALLOCATE(iistart_(0:size-1),iiend_(0:size-1))
CALL MPI_Allgather(iistart,1,MPI_INTEGER,iistart_,1,&
                   MPI_INTEGER,mpi_comm_world,ierr)
CALL MPI_Allgather(iiend,1,MPI_INTEGER,iiend_,1,&
                   MPI_INTEGER,mpi_comm_world,ierr)
#endif

! Treat land points for cp0, cg0, and e:
! (needed for advection terms)
DO i=istart,iend
  IF(is(i) < iistart .OR. is(i) > iiend)is(i) = iistart-1
  IF(in(i) < iistart .OR. in(i) > iiend)in(i) = iistart-1
  IF(ie(i) < iistart .OR. ie(i) > iiend)ie(i) = iistart-1
  IF(iw(i) < iistart .OR. iw(i) > iiend)iw(i) = iistart-1
ENDDO

#ifdef MPI
! Create the domain partiotioning field for output:
IF(nproc==0)THEN
nproc_out = -1
  DO n=1,nm
    DO m=1,mm
      DO nn=0,size-1
        IF(ii(m,n)>=istart_(nn).AND.ii(m,n)<=iend_(nn))THEN
          nproc_out(m,n) = nn
          EXIT
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDIF

IF(nproc==0)THEN

  WRITE(*,FMT='(A)')'umwm: remap: Tiling with halo summary:'
  WRITE(*,FMT='(A)')'+---------+------------+----------------+----------------+'
  WRITE(*,FMT='(A)')'|  nproc  |   iilen    |    iistart     |     iiend      |'
  WRITE(*,FMT='(A)')'+---------+------------+----------------+----------------+'
!                      1234567   1234567890   12345678901234   12345678901234
  DO nn=0,size-1
    WRITE(*,FMT=310)nn,iiend_(nn)-iistart_(nn)+1,iistart_(nn),iiend_(nn)
  ENDDO
  
  WRITE(*,FMT='(A)')'+---------+------------+----------------+----------------+'
  
ENDIF

310 FORMAT('| ',I7,' | ',I10,' | ',2(I14,' | '))

#endif

ENDSUBROUTINE remap
!======================================================================!



SUBROUTINE init
!======================================================================!
!                                                                      !
! DESCRIPTION: Subroutine to initialize many model parameters and      !
!              variables such as frequencies, direction angles,        !
!              phase speed and group velocity, wave numbers, etc.      !
!                                                                      !
!======================================================================!
#ifdef MPI
USE UMWM_mpi, ONLY:istart_,iend_
#endif
USE UMWM_util,ONLY:raiseException

use umwm_io, only: winds
use umwm_util, only: remap_mn2i

!======================================================================!

INTEGER :: i,m,n,o,p,pp,ind

!======================================================================!

! Set frequency increment:
dlnf = (LOG(fmax)-LOG(fmin))/FLOAT(om-1)

! Set frequency bins:
DO o=1,om
  f(o) = EXP(LOG(fmin)+(o-1)*dlnf) 
ENDDO

! Define various constants:
dth           = twopi/FLOAT(pm)
dthg          = dth*g
oneovdth      = 1./dth
log10overz    = LOG(10./z)
twopisds_fac  = twopi*sds_fac
twonu         = 2.*nu_water
fieldscale1   = sin_diss1/sin_fac
fieldscale2   = sin_diss2/sin_diss1
inv_sds_power = 1./sds_power

! This limits the Courant number to its theoretical value slightly
! larger than 1/SQRT(2), depending on the number of directional bins;
! this also limits the number of directions to be divisible by 8,
! not 4! (more isotropic in Cartesian projection) 

IF(MOD(pm,8) == 0)THEN
  cfllim = COS(0.25*pi-0.5*dth)
ELSE
  cfllim = 1./SQRT(2.)
ENDIF

! Compute grid cell areas and reciprocals:
ar      = dx(istart:iend)*dy(istart:iend)
oneovdx = 1./dx(istart:iend)
oneovdy = 1./dy(istart:iend)
oneovar = 1./ar

! Compute diffusion values in 2 frequenciess: 
bf1  = EXP(-16*dlnf*dlnf)
bf2  = EXP(-64*dlnf*dlnf)
bf1a = bf1/(bf1+bf2)
bf2  = bf2/(bf1+bf2)
bf1  = bf1a

DO p=1,pm
  th(p) = (p-0.5*(pm+1))*dth ! Angles
ENDDO

cth = COS(th) ! Cosines
sth = SIN(th) ! Sines

! Calculate wave ray directions adjusted for grid curvature:
DO i=istart,iend
  DO p=1,pm
    cth_curv(p,i) = COS(th(p)+curv(mi(i),ni(i)))
    sth_curv(p,i) = SIN(th(p)+curv(mi(i),ni(i)))
  ENDDO
ENDDO

! "Left" and "right" directional indices for refraction:
DO p=1,pm
  pl(p) = p+1
  pr(p) = p-1
ENDDO
pl(pm) = 1
pr(1)  = pm

dom = twopi*dlnf*f

DO p=1,pm
  cth2(p) = COS(dth*(p-1))**2
ENDDO

ALLOCATE(cth2pp(pm,pm))

DO p=1,pm
  DO pp=1,pm
    ind = pp-p+1
    IF(ind<=0)ind = pm+ind
    cth2pp(pp,p) = cth2(ind)*dth
  ENDDO
ENDDO

! Compute wave numbers, phase speeds, and group velocities:
CALL dispersion(1E-2) 

mindelx = MIN(MINVAL(dx_2d,mask==1),MINVAL(dy_2d,mask==1))
cgmax   = MAXVAL(cg0(:,istart:iend))
dtamin  = 0.98*cfllim*mindelx/cgmax

first    = .TRUE.

IF(restart)THEN
  firstdtg = .FALSE.
ELSE
  firstdtg = .TRUE.
ENDIF
  
! if forcing from file, update the wspd field for ustar first guess
if (winds) wspd = remap_mn2i(sqrt(uwf**2 + vwf**2))

! Initialize drag coefficient (Large and Pond, 1981):
cd = 1.2E-3
FORALL(i=istart:iend,wspd(i)>11.)cd(i) = (0.49+0.065*wspd(i))*1E-3

! Initialize friction velocity:
DO i=istart,iend
  ustar(i) = SQRT(cd(i))*wspd(i)
ENDDO

#ifdef MPI

! Figure out which process will print to screen:
iip = ii(xpl,ypl)
IF(mask(xpl,ypl)==0)THEN
  IF(nproc==0)THEN
    WRITE(0,*)xpl,ypl,mask(xpl,ypl)

    CALL raiseException('WARNING','init',&
                        'Land-point chosen for stdout, may go out of bounds')
    CALL raiseException('WARNING','init',&
                        'Check xpl and ypl in the OUTPUT namelist in namelists/main.nml')
    STOP
  ENDIF
ENDIF

IF(nproc==0)THEN
  DO n=0,size-1
    IF(iip>=istart_(n).AND.iip<=iend_(n))nproc_plot = n
  ENDDO
ENDIF

CALL MPI_Bcast(nproc_plot,1,MPI_INTEGER,0,mpi_comm_world,ierr)
CALL MPI_Bcast(iip,1,MPI_INTEGER,0,mpi_comm_world,ierr)

#else

iip = ii(xpl,ypl)

#endif

#ifndef MPI

WRITE(*,FMT=102)
WRITE(*,*)'Initialization summary:'
WRITE(*,FMT=103)
WRITE(*,*)'Bin  f[Hz]    T[s]    min(k)   max(k)   min(C)   max(C)   min(Cg)   max(Cg)'
WRITE(*,FMT=103)
DO o=1,om
  WRITE(*,FMT=101)o,f(o),1./f(o),                                     &
                  MINVAL(k(o,:),DIM=1),MAXVAL(k(o,:),DIM=1),          &
                  MINVAL(cp0(o,1:im),DIM=1),MAXVAL(cp0(o,1:im),DIM=1),&
                  MINVAL(cg0(o,1:im),DIM=1),MAXVAL(cg0(o,1:im),DIM=1)
ENDDO
101 FORMAT(1X,I2,8(2X,F7.4))
WRITE(*,FMT=102)

102 FORMAT('!',77('='),'!')
103 FORMAT('!',77('-'),'!')

#endif

ENDSUBROUTINE init
!======================================================================!



SUBROUTINE dispersion(tol)
!======================================================================!
!                                                                      !
! DESCRIPTION: Solves for the dispersion relation by iteration, and    !
!              and computes phase and group velocities in absolute     !
!              reference frame (w/ currents)                           !
!                                                                      !
!======================================================================!
#ifdef MPI
USE UMWM_mpi
#endif
!======================================================================!

INTEGER :: counter,i,o,p
REAL,INTENT(IN) :: tol
REAL :: dk

REAL,DIMENSION(istart:iend) :: b
REAL,DIMENSION(om,istart:iend) :: f_nd,kd,t

!======================================================================!

IF(nproc==0)WRITE(*,'(A)')'umwm: dispersion: Solving for dispersion relationship;'

! Non-dimesionalize frequencies, and use deep water limit 
! as initial guess:
FORALL(o=1:om,i=istart:iend)
  cp0(o,i)  = twopi*SQRT(d(i)/g)
  f_nd(o,i) = cp0(o,i)*f(o) 
  k(o,i)    = f_nd(o,i)*f_nd(o,i)
ENDFORALL

! Non-dimesionalize surface tension:
b = sfct/(rhow0*g*d(istart:iend)**2) 

DO i=istart,iend
  DO o=1,om

    counter = 1
    dk = 2.*tol

    DO WHILE(ABS(dk) > tol) ! Newton-Raphson iteration loop

      t(o,i) = TANH(k(o,i))
      dk = -(f_nd(o,i)*f_nd(o,i)-k(o,i)*t(o,i)  &
           *(1.+b(i)*k(o,i)*k(o,i)))            &
           /(3.*b(i)*k(o,i)*k(o,i)*t(o,i)+t(o,i)&
           +k(o,i)*(1.+b(i)*k(o,i)*k(o,i))*(1.-t(o,i)*t(o,i)))
      k(o,i) = k(o,i)-dk

      IF(counter == 1000)EXIT ! Escape if stuck
      counter = counter+1

    ENDDO

    k(o,i) = ABS(k(o,i))/d(i) ! f(k)=f(-k), so k>0 == k<0 roots

  ENDDO
ENDDO

IF(nproc==0)WRITE(*,'(A)')'umwm: dispersion: Dispersion relationship done;'

FORALL(o=1:om,i=istart:iend)kd(o,i) = k(o,i)*d(i)

! Limit kd to avoid floating overflow in transcendental functions:
WHERE(kd>20.)kd = 20.

! Phase speed and group velocity:
cp0 = TINY(cp0)
cg0 = TINY(cg0)
FORALL(o=1:om,i=istart:iend)

  cp0(o,i) = twopi*f(o)/k(o,i)
  cg0(o,i) = cp0(o,i)*(0.5+k(o,i)*d(i)/SINH(2.*kd(o,i))&
                      +sfct*k(o,i)*k(o,i)/(rhow0*g+sfct*k(o,i)*k(o,i)))
ENDFORALL

! Compute some frequently used arrays:
FORALL(o=1:om,i=istart:iend)

  dwn(o,i)       = dom(o)/ABS(cg0(o,i))                         ! dk
  l2(o,i)        = 0.5*ABS(cp0(o,i))/f(o)                       ! lambda/2 (half wavelength)
  k4(o,i)        = k(o,i)**4.                                   ! k^4
  oneoverk4(o,i) = 1./k4(o,i)                                   ! k^-4
  kdk(o,i)       = k(o,i)*dwn(o,i)                              ! k*dk
  k3dk(o,i)      = k(o,i)**3.*dwn(o,i)                          ! k*k*k*dk
  fkovg(o,i)     = f(o)*k(o,i)/g                                ! f*k/g
  cothkd(o,i)    = COSH(0.2*kd(o,i))/SINH(0.2*kd(o,i))          ! coth(0.2*kd) 
  invcp0(o,i)    = 1./cp0(o,i)                                  ! 1/Cp
  sbf(o,i)       = sbf_fac*k(o,i)/(SINH(2.*kd(o,i)))&           ! Bottom friction
                  +sbp_fac*k(o,i)/(COSH(kd(o,i))*COSH(kd(o,i))) ! Bottom percolation
  sdv(o,i)       = 4.*nu_water*k(o,i)**2.                       ! Viscosity

ENDFORALL

! Compute renormalization factors for Snl:
bf1_renorm = 0.
bf2_renorm = 0.
snl_arg    = 0.

DO i=istart,iend
  DO o=1,om-2
    bf1_renorm(o,i) = snl_fac*bf1*kdk(o+1,i)/kdk(o,i)
    bf2_renorm(o,i) = snl_fac*bf2*kdk(o+2,i)/kdk(o,i)
    snl_arg(o,i)    = 1.-(bf1_renorm(o,i)+bf2_renorm(o,i))
  ENDDO
ENDDO

! Half-wavelength over z:
logl2overz = LOG(l2/z)

! Limit wind input to be at 10 m for l/2 > 10 m:
WHERE(l2>20.)logl2overz = LOG(20./z)

!=======================================================================

#ifdef MPI
IF(nproc<size-1)THEN ! Communicate with process above:

  sendcount = om*(iend-iistart_(nproc+1)+1) ; dest = nproc+1 ; sendtag = nproc
  recvcount = om*(iiend-iend)               ; src  = nproc+1 ; recvtag = src

  CALL MPI_Sendrecv(cp0(:,iistart_(nproc+1):iend),sendcount,&
                    MPI_REAL,dest,sendtag,                  &
                    cp0(:,iend+1:iiend),recvcount,          &
                    MPI_REAL,src,recvtag,                   &
                    mpi_comm_world,status,ierr)

ENDIF

IF(nproc>0)THEN ! Communicate with process below:

  sendcount = om*(iiend_(nproc-1)-istart+1) ; dest = nproc-1 ; sendtag = nproc
  recvcount = om*(istart-iistart)           ; src  = nproc-1 ; recvtag = src
  
  CALL MPI_Sendrecv(cp0(:,istart:iiend_(nproc-1)),sendcount,&
                    MPI_REAL,dest,sendtag,                  &
                    cp0(:,iistart:istart-1),recvcount,      &
                    MPI_REAL,src,recvtag,                   &
                    mpi_comm_world,status,ierr)

ENDIF

CALL MPI_Barrier(mpi_comm_world,ierr)

IF(nproc<size-1)THEN ! Communicate with process above:

  sendcount = om*(iend-iistart_(nproc+1)+1) ; dest = nproc+1 ; sendtag = nproc
  recvcount = om*(iiend-iend)               ; src  = nproc+1 ; recvtag = src

  CALL MPI_Sendrecv(cg0(:,iistart_(nproc+1):iend),sendcount,&
                    MPI_REAL,dest,sendtag,                  &
                    cg0(:,iend+1:iiend),recvcount,          &
                    MPI_REAL,src,recvtag,                   &
                    mpi_comm_world,status,ierr)

ENDIF

IF(nproc>0)THEN ! Communicate with process below:

  sendcount = om*(iiend_(nproc-1)-istart+1) ; dest = nproc-1 ; sendtag = nproc
  recvcount = om*(istart-iistart)           ; src  = nproc-1 ; recvtag = src

  CALL MPI_Sendrecv(cg0(:,istart:iiend_(nproc-1)),sendcount,&
                    MPI_REAL,dest,sendtag,                  &
                    cg0(:,iistart:istart-1),recvcount,      &
                    MPI_REAL,src,recvtag,                   &
                    mpi_comm_world,status,ierr)

ENDIF

CALL MPI_Barrier(mpi_comm_world,ierr)

! If periodic domain, connect the east and west:
IF(isGlobal)THEN

  IF(nproc==0)THEN ! Communicate with last tile:

    sendcount = om*first_col_len ; dest = size-1 ; sendtag = nproc
    recvcount = om*last_col_len  ; src  = size-1 ; recvtag = src

    CALL MPI_Sendrecv(cp0(:,istart:(istart+first_col_len-1)),sendcount,&
                      MPI_REAL,dest,sendtag,                           &
                      cp0(:,iistart:istart-1),recvcount,               &
                      MPI_REAL,src,recvtag,                            &
                      mpi_comm_world,status,ierr)

  ENDIF

  IF(nproc==size-1)THEN ! Communicate with first tile:

    sendcount = om*last_col_len  ; dest = 0 ; sendtag = nproc
    recvcount = om*first_col_len ; src  = 0 ; recvtag = src

    CALL MPI_Sendrecv(cp0(:,(iend-last_col_len+1):iend),sendcount,&
                      MPI_REAL,dest,sendtag,                      &
                      cp0(:,iend+1:iiend),recvcount,              &
                      MPI_REAL,src,recvtag,                       &
                      mpi_comm_world,status,ierr)

  ENDIF

  CALL MPI_Barrier(mpi_comm_world,ierr)

  IF(nproc==0)THEN ! Communicate with last tile:

    sendcount = om*first_col_len ; dest = size-1 ; sendtag = nproc
    recvcount = om*last_col_len  ; src  = size-1 ; recvtag = src

    CALL MPI_Sendrecv(cg0(:,istart:(istart+first_col_len-1)),sendcount,&
                      MPI_REAL,dest,sendtag,                           &
                      cg0(:,iistart:istart-1),recvcount,               &
                      MPI_REAL,src,recvtag,                            &
                      mpi_comm_world,status,ierr)

  ENDIF

  IF(nproc==size-1)THEN ! Communicate with first tile:

    sendcount = om*last_col_len  ; dest = 0 ; sendtag = nproc
    recvcount = om*first_col_len ; src  = 0 ; recvtag = src

    CALL MPI_Sendrecv(cg0(:,(iend-last_col_len+1):iend),sendcount,&
                      MPI_REAL,dest,sendtag,                      &
                      cg0(:,iend+1:iiend),recvcount,              &
                      MPI_REAL,src,recvtag,                       &
                      mpi_comm_world,status,ierr)

  ENDIF

ENDIF
#endif

! Handle land points for Cp and Cg (needed for advection/refraction)
DO o=1,om
  cp0(o,iistart-1) = MINVAL(cp0(o,istart:iend))  
  cg0(o,iistart-1) = MINVAL(cg0(o,istart:iend))  
ENDDO

ENDSUBROUTINE dispersion
!======================================================================!
ENDMODULE UMWM_init
