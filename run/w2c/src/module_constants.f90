!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!    Definition of constant, and namelist variables.
!
!    modified by Chiaying March 2010 RSMAS/UM
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module constants

  implicit none

  real, parameter :: g=9.81,                       &! gravitational constant
                     To=300.,                      &! base pot. temp.
                     Cp=1004.5,                    &! specific heat
                     Rd=287.,                      &! dry air gas constant
                     Rv=461.6,                     &! moist air gas constant
                     L=2.50e6,                     &! latent heat of vap.
                     R_earth=6370.,                &! radius of the earth
                     Pr=100000.,                   &! reference pressure
                     Po=101325.,                   &! altimeter reference P
                     Talt=288.15,                  &! attimeter reference T
                     pbrh=611.,                    &! vapor pressure at 0 C (Pa)
                     T_frez=273.15,                &! water freezing point
                     RvRd=Rv/Rd,                   &! Rd/Rv
                     kappa=2./7.,                  &! kappa for pot. temp
                     pi=3.1415927,                 &
                     pid=pi/180.,                  &! radians to degrees
                     dip=180./pi,                  &
                     om_earth=(2.*3.1415927)/(24.*3600.),&! earth rot
                     lap_std_atmos=0.0065,         &! standard atmosphere
                     th_denom=0.03727594,          &! Pr**(-kappa)
                     missing=1.e20,                &! missing value
                     es_alpha = 611.2,             &! saturation vapor pressure
                     es_beta = 17.67,              &!
                     es_gamma = 243.5

!-------------------------------------------------------------------------------
!  Namelist variables
!-------------------------------------------------------------------------------

! input_output
  character       :: in_file*240
  integer         :: t_start
  integer         :: t_end
  integer         :: t_interval
  character       :: out_info_file*240

! first_guess
  real            :: fg_lon
  real            :: fg_lat
  real            :: search_range

! definition
  logical         :: simple_output
  real            :: center_level_1
  real            :: center_level_2
  integer         :: wind_level_type
  integer         :: wind_level_sigma
  real            :: wind_level_p
  character       :: basin*3
  character       :: runmodel*4
  integer         :: hurnum
! grads
  logical         :: output_grads
  character       :: out_grads_file*120
  integer         :: grads_level_type
  real            :: grads_p_interval
  real            :: grads_r_start
  real            :: grads_r_end
  real            :: grads_r_interval
!cyl w2c
  logical         :: output_w2c
  character       :: out_cg_file*120
  logical         :: ocean_model
!cyl cg
  logical         :: output_cg
  character       :: in_file1*120, in_file2*120, in_file3*120, out_cg_file2*120
  integer         :: exist_file1,exist_file2,exist_file3
! cyl vortex relocation vorrel
  logical         :: vortexrelocation
  character       :: inputcgfile1*240 
  character       :: inputcgfile2*240 
  character       :: inputwrffile*240 
  integer         :: radius_1
  integer         :: radius_2
!-------------------------------------------------------------------------------

  contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine read_namelist

      implicit none

      character (len=15)   :: namelist_file = 'namelist.vortex'
      integer              :: iost

      namelist /input_output/ in_file, &
                              t_start, &
                              t_end, &
                              t_interval, &
                              out_info_file
      namelist /first_guess/  fg_lon, &
                              fg_lat, &
                              search_range
      namelist /definition/   simple_output, &
                              center_level_1, &
                              center_level_2, &
                              wind_level_type, &
                              wind_level_sigma, &
                              wind_level_p, &
                              basin,        & 
                              runmodel,     &
                              hurnum
      namelist /grads/        output_grads, &
                              out_grads_file, &
                              grads_level_type, &
                              grads_p_interval, &
                              grads_r_start, &
                              grads_r_end, &
                              grads_r_interval
! cyl
      namelist /w2c/          output_w2c,&
                              out_cg_file,&
                              ocean_model
      namelist /cg_output/    output_cg,&
                              exist_file1,&
                              exist_file2,&
                              exist_file3,&
                              in_file1,&
                              in_file2,&
                              in_file3,&
                              out_cg_file2
      namelist / vorrel/     vortexrelocation, &
                             inputcgfile1, &
                             inputcgfile2, & 
                             inputwrffile, & 
                             radius_1,     &
                             radius_2
                             
   
!-------------------------------------------------------------------------------
!  Namelist defaults
!-------------------------------------------------------------------------------

! input_output
      in_file = ''
      t_start = 1
      t_end = 1
      t_interval = 1
      out_info_file = 'vortex.log'
! first_guess
      fg_lon = 999.
      fg_lat = 999.
      search_range = 200.
! definition
      simple_output = .FALSE.
      center_level_1 = 900.
      center_level_2 = 700.
      wind_level_type = 1
      wind_level_sigma = 1
      wind_level_p = 850.
      basin = '   ' 
      runmodel = '    ' 
      hurnum = 0
! grads
      output_grads = .TRUE.
      out_grads_file = 'vortex'
      grads_level_type = 1
      grads_p_interval = 10.
      grads_r_start = 0.
      grads_r_end = 400.
      grads_r_interval = 2.
!cyl w2c
      output_w2c =.TRUE.
      out_cg_file ='cg'
      ocean_model=.FALSE.
!cyl cg_output
      output_cg=.TRUE.
      in_file1=''
      in_file2=''
      in_file3=''
      exist_file1=0
      exist_file2=0
      exist_file3=0
      out_cg_file2=''
!cyl vortexrelocation vorrel
      vortexrelocation=.FALSE.
      inputcgfile1=''
      inputcgfile2='' 
      inputwrffile='' 
      radius_1=200
      radius_2=200
     
!-------------------------------------------------------------------------------
!  Read namelist variables
!-------------------------------------------------------------------------------

      open  (7, file = namelist_file, status = 'old', &
                form = 'formatted', access = 'sequential', iostat = iost)
      read  (7, input_output)
      close (7)

      open  (7, file = namelist_file, status = 'old', &
                form = 'formatted', access = 'sequential', iostat = iost)
      read  (7, first_guess)
      close (7)

      open  (7, file = namelist_file, status = 'old', &
                form = 'formatted', access = 'sequential', iostat = iost)
      read  (7, definition)
      print*,basin,runmodel,hurnum,wind_level_p  
      close (7)

      open  (7, file = namelist_file, status = 'old', &
                form = 'formatted', access = 'sequential', iostat = iost)
      read  (7, grads)
      close (7)
      open  (7, file = namelist_file, status = 'old', &
                form = 'formatted', access = 'sequential', iostat = iost)
      read  (7, w2c)
      close (7)
      print*,'kali'
      open  (7, file = namelist_file, status = 'old', &
                form = 'formatted', access = 'sequential', iostat = iost)
      read  (7, cg_output)
      close (7)
      open  (7, file = namelist_file, status = 'old', &
                form = 'formatted', access = 'sequential', iostat = iost)
      read  (7, vorrel)
      close (7)

      print*, vortexrelocation, inputcgfile1, inputwrffile , radius_1, radius_2 

      print*,'namelistw2c', output_w2c
!-------------------------------------------------------------------------------

  end subroutine read_namelist

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module constants

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
