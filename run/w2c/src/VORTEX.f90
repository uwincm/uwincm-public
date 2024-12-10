!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!    Find VORTEX center and tangential wind information
!    created  2008/03 Guo-yuan Lien, TDRC
!    modified 2008/06 Guo-yuan Lien, TDRC
!    modified 2009/04 Guo-yuan Lien, TDRC (add grads output)
!    modified 2010/01 Chiaying Lee , RSMAS/UM (wrf2cylindrical coordinate in NETCDF format)
!    modified 2010/02 Chiaying Lee , RSMAS/UM (combine wrf_d01, d02, d03 to one cg file, cg.f)
!    modified 2010/07 Chiaying Lee , RSMAS/UM (vortex relocation vortex_relocation.f90)
!    modified 2010/07 Chiaying Lee , RSMAS/UM (wind_radii)
!    modified 2022/08 Brandon Kerns, UW (Add csv output for Pandas, ect.)
!    modified 2022/08 Brandon Kerns, UW (Fix 3-d lat/lon output in CG files. Add 1-D storm center wrt height.)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!-------------------------------------------------------------------------------

program VORTEX

      use CONSTANTS
      use DEFINITION
      use WRF_TOOLS
      use WRF_IO
      use GET_VARIABLES
      use VORTEX_TOOLS
      use W2C_TOOLS
      use NETCDF_cylindrical
      use WIND_RADII
      implicit none

      external atan2_atmos_deg
      external grads_time_format
!cyl for atcf
      external get_date
      integer, parameter :: args              = 36
!cyl for w2c
      integer, parameter :: args_w2c          = 361
      integer :: iid
!cyl end
      integer, parameter :: average_level_max = 12
      integer, parameter :: Rmax_first_search = 4
      real,    parameter :: grads_p_low       = 1050.
      real,    parameter :: grads_p_top       = 50.
      real,    parameter :: inner_core_r      = 250.

!-------------------------------------------------------------------------------

      integer              :: ix, jx, kx, kpx, tx, rx, grads_kpx
      integer              :: i, j, k, kp, t, r, k_kp
      integer              :: ci1, ci2, cj1, cj2, cix, cjx

      integer              :: TT, file_len

      type (state_var)     :: STATE, cSTATE
      type (base_var)      :: BASE, cBASE
      type (ocean_var)     :: OCEAN, cOCEAN
      type (diag_var)      :: DIAG, cDIAG
      type (proj_info)     :: proj

      real                 :: ddx, ddx_c, msf
      integer              :: s_grid, s_grid_wind
      real                 :: ii_fg, jj_fg
      real                 :: ii_cc, jj_cc
      integer              :: ii_tmp, jj_tmp
      real                 :: ii_tmp2, jj_tmp2, ii_tmp3, jj_tmp3
      real, dimension(average_level_max) :: levels

      real                 :: r_t, wind_t
      real, dimension(Rmax_first_search) :: r_max_list, wind_max_list
      real                 :: tmp

      integer              :: one_time_level, offset
      real                 :: time_minute, time_minute_start, output_time_interval

      character (len=120)  :: out_grads_file_dat, out_grads_file_ctl

      real, allocatable    :: h_center(:,:,:)
      real, allocatable    :: u_wind(:,:), v_wind(:,:), slp(:,:)

      real, allocatable    :: temp(:,:,:), theta(:,:,:), u(:,:,:), v(:,:,:), w(:,:,:), pres(:,:,:)
      real, allocatable    :: gph(:,:,:), q(:,:,:), rh(:,:,:), qc(:,:,:), qi(:,:,:)
      real, allocatable    :: precip(:,:), precip_prev(:,:), rh2(:,:)

      real, allocatable    :: t_r(:,:), th_r(:,:), ut_r(:,:), ur_r(:,:), w_r(:,:), p_r(:,:), h_r(:,:)
      real, allocatable    :: q_r(:,:), rh_r(:,:), qc_r(:,:), qi_r(:,:), vort_r(:,:)
      real, allocatable    :: slp_r(:), precip_r(:), q2_r(:), rh2_r(:), t2_r(:), th2_r(:)
      real, allocatable    :: psfc_r(:), u10t_r(:), u10r_r(:), hflux_r(:), lhflux_r(:), vort10_r(:)

      character (len=19), allocatable :: time_str(:)
      real, allocatable    :: ii_center(:), jj_center(:)
      real, allocatable    :: lon_center(:), lat_center(:)
      real, allocatable    :: slp_min(:) !!, slp_min_r(:), slp_min_angle(:)
      real, allocatable    :: wspd_max(:), wspd_max_r(:), wspd_max_angle(:)
      real, allocatable    :: t_wspd_max(:), t_wspd_max_r(:)
      logical, allocatable :: is_found(:)

      real, allocatable    :: RR(:)
      real, allocatable    :: grads_levels(:)

      real, allocatable    :: grads_data(:,:)

      real                 :: atan2_atmos_deg
      character (len=15)   :: grads_time_format
      character (len=15)   :: grads_time
! cyl w2c
      double precision, allocatable    :: w_w2c(:,:,:)
      double precision, allocatable    :: meanp(:)
      real, allocatable    :: i_pcenter(:,:), j_pcenter(:,:)
      real, allocatable    :: lon_pcenter(:,:), lat_pcenter(:,:)
      real, allocatable    :: eta_i_center(:),eta_j_center(:),eta_lon_center(:),eta_lat_center(:)
      real                 :: pres_level1,pres_level2,pres_interval
      integer :: kpress,kkx,ii,jj,ii_eta,jj_eta
      integer :: i_weight,j_weight,count_weight
      integer,allocatable :: i_center_guess(:),j_center_guess(:)
      integer :: rrr,iaz
      real, allocatable :: slp_c(:,:),precip_c(:,:),q2_c(:,:),rh2_c (:,:),t2_c(:,:)
      real, allocatable :: th2_c(:,:),psfc_c(:,:),hflux_c(:,:),lhflux_c(:,:)
      real, allocatable :: u10t_c(:,:),u10r_c (:,:)
      real, allocatable :: tsk_c(:,:),mu_c(:,:)
      real, allocatable :: t_c(:,:,:),th_c(:,:,:),w_c(:,:,:),p_c(:,:,:),h_c(:,:,:),q_c(:,:,:),ph_c(:,:,:),pp_c(:,:,:)
      real, allocatable :: qv_c(:,:,:),rh_c(:,:,:),qc_c(:,:,:),qi_c(:,:,:),qs_c(:,:,:),qr_c(:,:,:)
      real, allocatable :: tracer1_c(:,:,:),tracer2_c(:,:,:),tracer3_c(:,:,:),tracer4_c(:,:,:)
      real, allocatable :: ut_c(:,:,:),ur_c(:,:,:),dbz_c(:,:,:)
      real, allocatable :: un_c(:,:,:),vn_c(:,:,:),un10_c(:,:),vn10_c(:,:)
      real, allocatable :: xlong_c(:,:,:),xlat_c(:,:,:)
      real, allocatable :: cg_x(:,:),cg_y(:,:),cg_lon,cg_lat
      real, allocatable :: cg_ang(:),cg_k(:),cg_ocean(:)
      character(len=80) ::w2cfile
      integer :: rcode,ncid
!cyl ocean model
      integer :: ox
      real, allocatable :: om_tmp_c(:,:,:),om_u_c(:,:,:),om_v_c(:,:,:),om_s_c(:,:,:)
      real, allocatable :: om_depth_c(:,:,:)
      real, allocatable :: om_ml_c(:,:)
!cyl wind radii
      real, allocatable :: WSPD10(:,:),uu10(:,:),vv10(:,:)
      real,dimension(7) :: rad1,rad2,rad3,rad4
      integer,dimension(7):: wind_thresh
      character (len=2),dimension(7) :: TCscale
! cyl atcf
      character(len=10)  :: get_date,atcfdate
      character (len=19) :: time_str_atcf
      character(len=19)  :: get_date_iso,isodate
      character (len=1)  :: lonmark,latmark
      character (len=2)  :: TCscalename
!cyl 2 percent tail
      real :: wspd_max1
      real :: percentile

      integer :: kkkkk
      logical :: exist
!-------------------------------------------------------------------------------
       data wind_thresh /34,50,64,83,96,114,135/
       data TCscale /'TS','TS','HU','HU','HU','HU','HU'/
      write (*, *)
      write (*, '(A)') ' Find VORTEX center and tangential wind information'
      write (*, *)

      call read_namelist

!-------------------------------------------------------------------------------
!  Get dimension and projection
!-------------------------------------------------------------------------------

      call get_ijk_dim (trim(in_file), ix, jx, kx,ox)
      call set_domain_proj (trim(in_file), proj)

      ddx = proj%dx

!-------------------------------------------------------------------------------
!  Initialize and allocate dependent variables
!-------------------------------------------------------------------------------

      if (output_grads) simple_output = .FALSE.

      file_len = min(len_trim(out_grads_file), 116)
      out_grads_file_dat = out_grads_file
      out_grads_file_ctl = out_grads_file
      write (out_grads_file_dat(file_len+1:file_len+4), '(A4)') '.dat'
      write (out_grads_file_ctl(file_len+1:file_len+4), '(A4)') '.ctl'

      call latlon_to_ij (proj, fg_lat, fg_lon, ii_fg, jj_fg)

      s_grid      = int(search_range*1000. / ddx + 0.5)
      s_grid_wind = int(inner_core_r*1000. / ddx + 0.5)

!cyl add do loop to search center at each level
      pres_level1=1000.0;
      pres_level2=100.0;
      pres_interval=-50.0;
      kkx = int((pres_level1-pres_level2)/abs(pres_interval) + 1.0)
      print*,'kkx',kkx
      allocate (i_pcenter      (1,kkx))
      allocate (j_pcenter      (1,kkx))
      allocate (lon_pcenter     (1,kkx))
      allocate (lat_pcenter     (1,kkx))
      kkx = 0
!------------------------------------------------------------------------------
kploop: do kpress = 1,1
!------------------------------------------------------------------------------
      kkx = kkx + 1
     ! center_level_1 = 1000.0
     ! center_level_2 = kpress+pres_interval
      kpx = int((abs(center_level_1 - center_level_2) + 99.999999) / 100.) + 1
      if (kpx .GT. average_level_max) then
        write (*, *) 'Error: wrong range of levels to find TC center.'
        stop
      end if
      do kp = 1, kpx
        levels(kp) = (center_level_1 + real(kp-1) / real(kpx-1) * (center_level_2 - center_level_1)) * 100.
      end do

      output_time_interval = 360.

!-------------------------------------------------------------------------------

      call state_allocate (ix, jx, kx, STATE)
      call base_allocate  (ix, jx, kx, BASE)
      call diag_allocate (ix, jx, DIAG)

      allocate (h_center (ix, jx, kpx))
      allocate (u_wind (ix, jx))
      allocate (v_wind (ix, jx))
      allocate (slp    (ix, jx))

!-------------------------------------------------------------------------------
      tx = (t_end - t_start) / t_interval + 1
      if (tx .LT. 1) then
        write (*, *) 'Error: bad time setting.'
        stop
      end if

      allocate (time_str       (tx))
      allocate (ii_center      (tx))
      allocate (jj_center      (tx))
      allocate (lon_center     (tx))
      allocate (lat_center     (tx))
      allocate (slp_min        (tx))
      allocate (wspd_max       (tx))
      allocate (wspd_max_r     (tx))
      allocate (wspd_max_angle (tx))
      allocate (t_wspd_max     (tx))
      allocate (t_wspd_max_r   (tx))
      allocate (is_found       (tx))

      is_found(:) = .TRUE.

      if (output_grads) then

      end if ! [ output_grads ]

!-------------------------------------------------------------------------------
tloop: do t = 1, tx
!-------------------------------------------------------------------------------

        TT = t_start + (t-1) * t_interval

        call get_time (trim(in_file), TT, time_str(t), time_minute)
        write (*, '(A,A,A,A19)') ' Processing file: ', trim(in_file), ', time = ', time_str(t)

        if (t .EQ. 1) then
          time_minute_start = time_minute
        end if
        if (t .EQ. 2) output_time_interval = time_minute - time_minute_start

        call get_state_var (trim(in_file), ix, jx, kx, TT, STATE)
        call get_base_var  (trim(in_file), ix, jx, kx, TT, BASE)
        call get_p_level_gph (ix, jx, kx, STATE, BASE, kpx, levels(1:kpx), h_center)
        call get_p_level_uv  (ix, jx, kx, STATE, BASE, wind_level_p*100., u_wind, v_wind)
        call get_slp         (ix, jx, kx, STATE, BASE, slp)
        call get_diag_var (trim(in_file), ix, jx, kx, TT, DIAG)
!-------------------------------------------------------------------------------
!  Search vortex center
!-------------------------------------------------------------------------------

        call find_vortex_center (ix, jx, kpx, missing, h_center, ddx, ii_fg, jj_fg, s_grid, &
                                 ii_center(t), jj_center(t))
! cyl fixed storm center
        if ( search_range .eq. 0.0) then
            ii_center(t)=ii_fg
            jj_center(t)=jj_fg
        end if
! end cyl
        if ((ii_center(t) .LT. 1.5) .OR. (ii_center(t) .GT. real(ix)-0.5) .OR. &
            (jj_center(t) .LT. 1.5) .OR. (jj_center(t) .GT. real(jx)-0.5)) then
          is_found(t) = .FALSE.
          if (output_grads) then
          end if
          cycle tloop
          cycle kploop
        end if

        call ij_to_latlon (proj, ii_center(t), jj_center(t), lat_center(t), lon_center(t))

      print*,'---cyl---',lon_center(t), lat_center(t)
!-------------------------------------------------------------------------------
!  Find map scale factor and relevant grid spacing in vortex center
!-------------------------------------------------------------------------------

        call compute_msf_lc (lat_center(t), proj%truelat1, proj%truelat2, msf)
        ddx_c = ddx / msf

!-------------------------------------------------------------------------------
!  Get minimum sea level pressure
!-------------------------------------------------------------------------------

        call find_extreme_value (ix, jx, missing, slp, ii_center(t), jj_center(t), s_grid, .FALSE., &
                                 slp_min(t), ii_tmp, jj_tmp)
        slp_min(t) = slp_min(t) / 100.

        if ((wind_level_type .EQ. 2) .AND. (slp_min(t) .LT. wind_level_p)) then
          write (*, '(A)') 'Warning: minimum sea level pressure of the vortex is lower than the given pressure level to find maximun wind.'
        end if
!-------------------------------------------------------------------------------
!        if (.NOT. simple_output) then
!-------------------------------------------------------------------------------
!  Get maximum wind speed
!-------------------------------------------------------------------------------
!
!--- cyl ----- do the smoothing twice ---

        allocate (uu10(ix,jx))
        allocate (vv10(ix,jx))
        uu10 = DIAG%u10
        vv10 = DIAG%v10
!        call wspd_smoothing_2d(vv10,ix,jx)

        if (wind_level_type .EQ. 1) then
          call find_max_stag_wspd (ix, jx, missing, STATE%u(:,:,wind_level_sigma), STATE%v(:,:,wind_level_sigma), &
                                   ii_center(t), jj_center(t), s_grid_wind, wspd_max(t), ii_tmp, jj_tmp)
        else
!--- cyl -------comment this out is due to the
!          call find_max_wspd (ix, jx, missing, u_wind, v_wind, &
!                              ii_center(t), jj_center(t), s_grid_wind, wspd_max(t), ii_tmp, jj_tmp)
!          call find_max_wspd (ix, jx, missing, DIAG%u10, DIAG%v10, &
!                              ii_center(t), jj_center(t), s_grid_wind, wspd_max(t), ii_tmp, jj_tmp)
          call find_max_wspd (ix, jx, missing, uu10, vv10, &
                              ii_center(t), jj_center(t), s_grid_wind, wspd_max(t), ii_tmp, jj_tmp)
          wspd_max1=wspd_max(t)
!          percentile = 0.0
!          call find_max_wspd_tail (ix, jx, missing, uu10, vv10, ii_center(t), jj_center(t), &
!                             s_grid_wind, wspd_max(t),percentile)

         print*,'cyl 2 percent tail',wspd_max1,wspd_max(t)
        end if
        ii_tmp2 = real(ii_tmp) - ii_center(t)
        jj_tmp2 = real(jj_tmp) - jj_center(t)
        wspd_max_r(t) = sqrt(ii_tmp2 * ii_tmp2 + jj_tmp2 * jj_tmp2) * ddx_c / 1000.
        call gridwind_to_truewind (lon_center(t), proj, ii_tmp2, jj_tmp2, ii_tmp3, jj_tmp3)
        wspd_max_angle(t) = atan2_atmos_deg(ii_tmp3, jj_tmp3)
        print*,'---cyl---',wspd_max(t),wspd_max_r(t),inner_core_r
        if (wspd_max_r(t) .GT. inner_core_r) then
!---- cyl comment it out for atcf
!          wspd_max(t)       = -99.
          wspd_max_r(t)     = -99.
          wspd_max_angle(t) = -99.
        end if

!-------------------------------------------------------------------------------
!  Get maximum circular-averaged tangential wind speed
!-------------------------------------------------------------------------------
       print*,'kkkkkkkkkkkkkkkkkkkkkkkkkkkali'
       allocate(WSPD10(ix,jx))
       WSPD10 = SQRT( uu10**2 + vv10**2 )

        !------ first search -------
        ! r = 2, 6, 10, ... 254 km

        wind_max_list(:) = -99.
        do r = 1, 64
          r_t = -2. + 4. * real(r)
          call circular_ave(ix,jx,missing,WSPD10,ii_center(t),jj_center(t), r_t*1000./ddx_c, args,wind_t);

        !  if (wind_level_type .EQ. 1) then
        !    call circular_ave_stag_uv (ix, jx, missing, STATE%u(:,:,wind_level_sigma), STATE%v(:,:,wind_level_sigma), &
        !                               ii_center(t), jj_center(t), r_t*1000./ddx_c, args, wind_t, tmp)
        !  else
        !    call circular_ave_uv (ix, jx, missing, u_wind, v_wind, &
        !                          ii_center(t), jj_center(t), r_t*1000./ddx_c, args, wind_t, tmp)
        !  end if

          if (wind_t .GT. wind_max_list(Rmax_first_search) .AND. (wind_t .NE. missing)) then
            r_max_list(Rmax_first_search) = r_t
            wind_max_list(Rmax_first_search) = wind_t
          end if
          do i = Rmax_first_search-1, 1, -1
            if (wind_max_list(i+1) .GT. wind_max_list(i)) then
              tmp = r_max_list(i)
              r_max_list(i) = r_max_list(i+1)
              r_max_list(i+1) = tmp
              tmp = wind_max_list(i)
              wind_max_list(i) = wind_max_list(i+1)
              wind_max_list(i+1) = tmp
            end if
          end do
        end do

        !------ second search -------
        ! r = (ri-1.8, ri-1.6, ... ri+1.8, ri+2) km
        ! for ri = r_max_list(1) ~ r_max_list(Rmax_first_search)

        t_wspd_max(t) = 0.
        t_wspd_max_r(t) = 0.
        do i = 1, 4
          do r = 1, 20
            r_t = r_max_list(i) - 2. + 0.2 * real(r)
          !  if (wind_level_type .EQ. 1) then
          !    call circular_ave_stag_uv (ix, jx, missing, STATE%u(:,:,wind_level_sigma), STATE%v(:,:,wind_level_sigma), &
          !                               ii_center(t), jj_center(t), r_t*1000./ddx_c, args, wind_t, tmp)
          !  else
          !    call circular_ave_uv (ix, jx, missing, u_wind, v_wind, &
          !                          ii_center(t), jj_center(t), r_t*1000./ddx_c, args, wind_t, tmp)
          !  end if
          call circular_ave(ix,jx,missing,WSPD10,ii_center(t),jj_center(t), r_t*1000./ddx_c, args,wind_t);

            if (wind_t .GT. t_wspd_max(t) .AND. (wind_t .NE. missing)) then
              t_wspd_max(t) = wind_t
              t_wspd_max_r(t) = r_t
            end if
          end do
        end do

        if (t_wspd_max_r(t) .GT. inner_core_r) then
          t_wspd_max(t)   = -99.
          t_wspd_max_r(t) = -99.
        end if
        t_wspd_max(t)=t_wspd_max(t)*1.95
        t_wspd_max_r(t)=t_wspd_max_r(t)*0.539956803
        print*,'RMW---cyl', t_wspd_max(t), t_wspd_max_r(t)

       deallocate(WSPD10)
!-------------------------------------------------------------------------------
!        end if ! [ .NOT. simple_output ]
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
        if (output_grads) then
!-------------------------------------------------------------------------------
        end if ! [ output_grads ]
!-------------------------------------------------------------------------------

        ii_fg = ii_center(t)
        jj_fg = jj_center(t)
      i_pcenter(1,kkx) = ii_center(t)
      j_pcenter(1,kkx) = jj_center(t)
      lon_pcenter(1,kkx) = lon_center(t)
      lat_pcenter(1,kkx) = lat_center(t)
!-------------------------------------------------------------------------------
      end do tloop ! [ t = 1, tx ]

      if (output_grads) then
      end if
!-------------------------------------------------------------------------------
! cyl for wind_radii
!-------------------------------------------------------------------------------
        call smoothing_2d_2(uu10,ix,jx)
        call smoothing_2d_2(uu10,ix,jx)
        call smoothing_2d_2(uu10,ix,jx)
        call smoothing_2d_2(vv10,ix,jx)
        call smoothing_2d_2(vv10,ix,jx)
        call smoothing_2d_2(vv10,ix,jx)
       percentile = 0.0
       allocate(WSPD10(ix,jx))
!       WSPD10 = SQRT( DIAG%u10**2 + DIAG%v10**2 )
       WSPD10 = SQRT( uu10**2 + vv10**2 )
       !----for TS----
       do i = 1,7
          if ( wspd_max(tx) .ge. real(wind_thresh(i)/1.95) ) then
             call get_thresh_radii( BASE%xlong,BASE%xlat,WSPD10,lon_center(tx),lat_center(tx),real(wind_thresh(i)/1.95),0,rad1(i),rad2(i),rad3(i),rad4(i) , percentile)
          else
             rad1(i)=0.0
             rad2(i)=0.0
             rad3(i)=0.0
             rad4(i)=0.0
          endif
          rad1(i)=rad1(i)*111.0*0.539956803
          rad2(i)=rad2(i)*111.0*0.539956803
          rad3(i)=rad3(i)*111.0*0.539956803
          rad4(i)=rad4(i)*111.0*0.539956803  !convert to nm
             print*,'---- maximum WSPD ----',wspd_max(tx),wind_thresh(i)/2
             print*,rad1(i), rad2(i), rad3(i), rad4(i)
       enddo
!-------------------------------------------------------------------------------
! cyl end wind_radii
!-------------------------------------------------------------------------------




       call diag_deallocate (DIAG)

!-------------------------------------------------------------------------------
!  Output vortex information: ATCF And CSV Format
!-------------------------------------------------------------------------------

      write (*, *)
      write (*, '(A,A)') ' Output information file: ', trim(out_info_file)

      open (50, file=trim(out_info_file), status='unknown', access='sequential', form='formatted')
      open (55, file=trim(out_info_file)//'.csv', status='unknown', access='sequential', form='formatted')
      print*,simple_output,is_found(1)
      do t = 1, tx
            if (is_found(t)) then
               if ( lat_center(t) .ge. 0 ) then
                  latmark = 'N'
               else
                  latmark = 'S'
               endif
               if ( lon_center(t) .ge. 0 ) then
                  lonmark = 'E'
               else
                  lonmark = 'W'
               endif
               !print*,'date',get_date(time_str(1)),slp_min(1)
               atcfdate=get_date(time_str(1))
               isodate=get_date_iso(time_str(1))
               if ( wspd_max(tx) .ge. 0.0 ) then
                  wspd_max = wspd_max*1.95
               endif
               if ( wspd_max(tx) .ge. real(wind_thresh(1)) ) then
                  do i = 1,7
                     if ( wspd_max(tx) .ge. real(wind_thresh(i)) ) then
                        TCscalename=TCscale(i)
                     endif
                  enddo
                  do i = 1,7
                     if ( wspd_max(tx) .ge. real(wind_thresh(i)) ) then
                        write(50,500) basin,',',0,',',atcfdate,',','  ',',',runmodel,',',0,',', &
                              (nint(lat_center(t)*10.0)),latmark,',',abs(nint((lon_center(t)*10.0))),lonmark, &
                              ',',nint(wspd_max(t)),',',nint(slp_min(t)),',',TCscalename,',',wind_thresh(i),',','NEQ',',',nint(rad1(i)),',', &
                              nint(rad4(i)),',',nint(rad3(i)),',',nint(rad2(i)),',',nint(t_wspd_max(tx)),',',nint(t_wspd_max_r(tx)),',',&
                              nint(wspd_max_r(t)*0.539956803),',',' ,',' ,',' ,',' ,',' ,',' ,',' ,',' ,',' ,'
                     endif
                  enddo
               else
                 write(50,500)basin,',',0,',',atcfdate,',','  ',',',runmodel,',',0,',', &
                              (nint(lat_center(t)*10.0)),latmark,',',abs(nint((lon_center(t)*10.0))),lonmark, &
                              ',',nint(wspd_max(t)),',',nint(slp_min(t)),',','  ',',',0,',','   ',',',0,',',0,',',0,',',0, &
                              ',',nint(t_wspd_max(tx)),',',nint(t_wspd_max_r(tx)),',',nint(wspd_max_r(t)*0.539956803),',',' ,',' ,',' ,',' ,',' ,',' ,',' ,',' ,',' ,'
               endif

               
               !TODO Add csv format output here.

               write(55,550) isodate,',',lat_center(t),',',lon_center(t),',',wspd_max(t),',',slp_min(t), &
                            ',',wspd_max_r(t)*0.539956803, &      
                            ',',rad1(1),',',rad4(1),',',rad3(1),',',rad2(1), &
                            ',',rad1(2),',',rad4(2),',',rad3(2),',',rad2(2), &
                            ',',rad1(3),',',rad4(3),',',rad3(3),',',rad2(3)      

            endif  ! [(is_found(t))]
      end do

500   format(A2,A1,I3,A1,A11,A1,A3,A1,A5,A1,I4,A1,I4,A1,A1,I5,A1,A1,I4,A1,I5,A1,A3,A1,I4,A1,A4,A1,I5,A1,I5,A1,I5,A1,I5,A1,I5,A1,I5,A1,I5,A1,9(A2))
550   format(A19,A1,F10.4,A1,F10.4,A1,F10.4,A1,F12.4,&
            A1,F7.1,&   ! RMW
            4(A1,F7.1),4(A1,F7.1),4(A1,F7.1),&   ! 34 kt Wind radii: 34, 50, 64 kts.
            4(A1,F7.1),4(A1,F7.1),4(A1,F7.1),&   ! 50 kt Wind radii: 34, 50, 64 kts.
            4(A1,F7.1),4(A1,F7.1),4(A1,F7.1))   ! 64 kt Wind radii: 34, 50, 64 kts.


      if (output_grads) then
      end if ![outpu_grads]

!-------------------------------------------------------------------------------
!  Deallocate variables
!-------------------------------------------------------------------------------


      call state_deallocate (STATE)
      call base_deallocate  (BASE)

      deallocate (h_center, u_wind, v_wind, slp)

      deallocate (ii_center, jj_center, lon_center, lat_center, slp_min)
!      deallocate (slp_min_r, slp_min_angle)
      deallocate (time_str, wspd_max, wspd_max_r, wspd_max_angle, t_wspd_max, t_wspd_max_r)

      if (output_grads) then
      end if

!-------------------------------------------------------------------------------
      end do kploop ! [ k = 1000, 100 ]
      close (50)
      close (55)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! below is for w2c
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  if (output_w2c) then
    print*, 'output_w2c', output_w2c
!-------------------------------------------------------------------------------
! cyl : find center for each eta level
!-------------------------------------------------------------------------------
! 0) assign the vortex center for each etalayer
      allocate (eta_i_center  (kx))
      allocate (eta_j_center  (kx))
      allocate (eta_lon_center  (kx))
      allocate (eta_lat_center  (kx))
      allocate (i_center_guess  (kx))
      allocate (j_center_guess  (kx))
! 1) initial guess from data above
     do k = 1, kx
      i_center_guess(k) = i_pcenter(1,1)
      j_center_guess(k) = j_pcenter(1,1)
     enddo
! 2) load data
      call state_allocate (ix, jx, kx, STATE)
      call base_allocate  (ix, jx, kx, BASE)
      call get_state_var (trim(in_file), ix, jx, kx, TT, STATE)
      call get_base_var  (trim(in_file), ix, jx, kx, TT, BASE)
      if (ocean_model) then
       call ocean_allocate (ix, jx, ox, OCEAN)
       call get_ocean_var  (trim(in_file), ix, jx, ox, TT, OCEAN)
      endif
! 2) load 3D varable

! do the iteration to find the correct pressure center
    irloop:do rrr = 50,10,-10
      do k = 1, kx ! each eta level
        !------ 3-D variables within serch radius rrr-------
        ci1 = max(1 , int(i_center_guess(k) - real(rrr)*1000./ddx_c - 0.5))
        ci2 = min(ix, int(i_center_guess(k) + real(rrr)*1000./ddx_c + 1.5))
        cj1 = max(1 , int(j_center_guess(k) - real(rrr)*1000./ddx_c - 0.5))
        cj2 = min(jx, int(j_center_guess(k) + real(rrr)*1000./ddx_c + 1.5))
        cix = ci2 - ci1 + 1
        cjx = cj2 - cj1 + 1
        ii_cc = i_center_guess(k) - real(ci1 - 1)
        jj_cc = j_center_guess(k) - real(cj1 - 1)
        call state_compact (ci1, ci2, cj1, cj2, kx, STATE, cSTATE)
        call base_compact  (ci1, ci2, cj1, cj2, kx, BASE , cBASE )
        allocate (temp  (cix, cjx, kx))
        allocate (theta (cix, cjx, kx))
        allocate (w     (cix, cjx, kx))
        allocate (pres  (cix, cjx, kx))
        allocate (gph   (cix, cjx, kx))
        allocate (q     (cix, cjx, kx))
        allocate (rh    (cix, cjx, kx))
        allocate (meanp (kx))

        call get_eta_level_variable(cix, cjx, kx, cSTATE, cBASE, temp, theta, w, pres, gph, q, rh)
        ! 2.1) first find the mean p over search area
        meanp(k)=sum(pres(:,:,k)) / count (pres(:,:,k) /= 0.0)/100 ! unit to mb
        ! 2.2)
        !   a. find area 'A' where press < meanp
        !   b. take average i, and j over A
        !   c. then this is new center
        i_weight=0
        j_weight=0
        count_weight=0
        do j = cj1,cj2
          do i = ci1,ci2
            jj = j-cj1+1;
            ii = i-ci1+1;
            if (pres(ii,jj,k)/100.0 .le. meanp(k)) then
              i_weight=i_weight+i
              j_weight=j_weight+j
              count_weight=count_weight+1
            endif
          enddo
        enddo
        eta_i_center(k) = (1.0*i_weight) / count_weight
        eta_j_center(k) = (1.0*j_weight) / count_weight
        print*, eta_i_center(k), eta_j_center(k)
        call ij_to_latlon (proj, eta_i_center(k), eta_j_center(k), eta_lat_center(k), eta_lon_center(k))
    
        !cyl fix storm cetner to first guess
        if ( search_range .eq. 0.0 ) then
          eta_i_center(k)=i_center_guess(k)
          eta_j_center(k)=j_center_guess(k)
        else
          i_center_guess(k) = eta_i_center(k)
          j_center_guess(k) = eta_j_center(k)
        endif
        !end cyl

        deallocate(temp,theta,w,pres,gph,q,rh,meanp)
        call state_nullify (cSTATE)
        call base_nullify  (cBASE )
      enddo
      ! print*,eta_i_center
      ! print*,eta_lon_center
    end do irloop![do rrr  ]

    !-------------------------------------------------------------------------------
    !cyl (x,y,z) to (r,theta,z)
    !-------------------------------------------------------------------------------
    ! radius
    rx = int((grads_r_end - grads_r_start) / grads_r_interval + 1.e-6) + 1
    allocate (RR       (rx))
    do r = 1, rx
      RR(r) = grads_r_start + (r-1) * grads_r_interval
    end do

    !------ 2-D variables -------
    call diag_allocate (ix, jx, DIAG)
    call get_diag_var (trim(in_file), ix, jx, kx, TT, DIAG)


    allocate (precip_prev (ix, jx))
    allocate (precip      (ix, jx))
    allocate (rh2         (ix, jx))
    allocate (slp         (ix, jx))
    ! cylindrical 2-D variables
    allocate (slp_c (rx,args_w2c))
    allocate (precip_c (rx,args_w2c))
    allocate (q2_c (rx,args_w2c))
    allocate (rh2_c (rx,args_w2c))
    allocate (t2_c (rx,args_w2c))
    allocate (th2_c (rx,args_w2c))
    allocate (psfc_c (rx,args_w2c))
    allocate (mu_c (rx,args_w2c))
    allocate (hflux_c (rx,args_w2c))
    allocate (lhflux_c (rx,args_w2c))
    allocate (u10t_c (rx,args_w2c))
    allocate (u10r_c (rx,args_w2c))
    allocate (un10_c (rx,args_w2c))
    allocate (vn10_c (rx,args_w2c))
    allocate (tsk_c (rx,args_w2c))
    allocate (cg_x (rx,args_w2c))
    allocate (cg_y (rx,args_w2c))


    call get_slp         (ix, jx, kx, STATE, BASE, slp)



    precip_prev(:,:) = 0.

    precip(:,:) = DIAG%rainc(:,:) + DIAG%rain(:,:) + DIAG%snow(:,:) - precip_prev(:,:)
    precip_prev(:,:) = DIAG%rainc(:,:) + DIAG%rain(:,:) + DIAG%snow(:,:)
    do j = 1, jx
      do i = 1, ix
        rh2(i,j) = rel_humidity(DIAG%q2(i,j), DIAG%t2(i,j), DIAG%psfc(i,j))
      end do
    end do

    do r = 1, rx
      call w2c_circular_ave (ix, jx, missing, slp        , eta_i_center(1), eta_j_center(1), RR(r)*1000./ddx_c, args_w2c, slp_c   (r,:))
      call w2c_circular_ave (ix, jx, missing, precip     , eta_i_center(1), eta_j_center(1), RR(r)*1000./ddx_c, args_w2c, precip_c(r,:))
      call w2c_circular_ave (ix, jx, missing, DIAG%q2    , eta_i_center(1), eta_j_center(1), RR(r)*1000./ddx_c, args_w2c, q2_c    (r,:))
      call w2c_circular_ave (ix, jx, missing, rh2        , eta_i_center(1), eta_j_center(1), RR(r)*1000./ddx_c, args_w2c, rh2_c   (r,:))
      call w2c_circular_ave (ix, jx, missing, DIAG%t2    , eta_i_center(1), eta_j_center(1), RR(r)*1000./ddx_c, args_w2c, t2_c    (r,:))
      call w2c_circular_ave (ix, jx, missing, DIAG%th2   , eta_i_center(1), eta_j_center(1), RR(r)*1000./ddx_c, args_w2c, th2_c   (r,:))
      call w2c_circular_ave (ix, jx, missing, DIAG%psfc  , eta_i_center(1), eta_j_center(1), RR(r)*1000./ddx_c, args_w2c, psfc_c  (r,:))
      call w2c_circular_ave (ix, jx, missing, DIAG%hflux , eta_i_center(1), eta_j_center(1), RR(r)*1000./ddx_c, args_w2c, hflux_c (r,:))
      call w2c_circular_ave (ix, jx, missing, DIAG%lhflux, eta_i_center(1), eta_j_center(1), RR(r)*1000./ddx_c, args_w2c, lhflux_c(r,:))
      call w2c_circular_ave (ix, jx, missing, DIAG%tsk, eta_i_center(1), eta_j_center(1), RR(r)*1000./ddx_c, args_w2c, tsk_c(r,:))
      call w2c_circular_ave_uv (ix, jx, missing, DIAG%u10, DIAG%v10, &
                            eta_i_center(1), eta_j_center(1), RR(r)*1000./ddx_c, args_w2c, u10t_c(r,:), u10r_c(r,:),un10_c(r,:),vn10_c(r,:))
    end do

    if (ocean_model) then
      allocate (om_ml_c (rx,args_w2c))
      do r = 1,rx
          call w2c_circular_ave (ix, jx, missing, OCEAN%om_ml, eta_i_center(1), eta_j_center(1), RR(r)*1000./ddx_c, args_w2c, om_ml_c(r,:))
      enddo
    endif

    !create netcdf file
    allocate (cg_ang(args_w2c))
    allocate (cg_k(kx))

    w2cfile = out_cg_file
    do iaz = 1,args_w2c
      cg_ang(iaz)=(iaz-1)*1.0
    enddo
    do k = 1,kx
      cg_k(k)=k*1.0
    enddo
    if (ocean_model) then
      allocate(cg_ocean(ox))
      cg_ocean=OCEAN%om_depth(1,1,:)
    else
      allocate(cg_ocean(kx))
      cg_ocean=cg_k
    endif
    do iaz = 1, args_w2c
      do r = 1, rx
        cg_x(r,iaz)=RR(r)*cos(cg_ang(iaz)*3.14159/180.0);
        cg_y(r,iaz)=RR(r)*sin(cg_ang(iaz)*3.14159/180.0);
      enddo
    enddo


    call create_nffile(w2cfile,RR,cg_ang,cg_k,cg_ocean,ncid)
    print*,'finish createnetcdf',ncid

    !!! Define the dimension variables
    call cg_add_1d_var(w2cfile,'radius','radius','km','Distance from vortex ceneter',RR)
    call cg_add_1d_var(w2cfile,'theta','theta','degree','Azimuth angle',cg_ang)
    call cg_add_1d_var(w2cfile,'bottom_top','bottom_top','','Eta_level',cg_k)
    call cg_add_1d_var(w2cfile,'bottom_top','storm_center_lon','degrees E','Longitude of Storm Center on Eta Levels',eta_lon_center)
    call cg_add_1d_var(w2cfile,'bottom_top','storm_center_lat','degrees N','Latitude of Storm Center on Eta Levels',eta_lat_center)
    !!! write 2d variable
    call cg_add_2d_var(w2cfile,'slp','hpa','Sea level pressure',slp_c)
    call cg_add_2d_var(w2cfile,'precipitation','mm','Instant rainrate',precip_c)
    call cg_add_2d_var(w2cfile,'q2','','2 m water vapor',q2_c)
    call cg_add_2d_var(w2cfile,'rh2','%','2 m relative humidity',rh2_c)
    call cg_add_2d_var(w2cfile,'t2','Kelvin','2 m temperature',t2_c)
    call cg_add_2d_var(w2cfile,'th2','Kelvin','2 m potential temperature',th2_c)
    call cg_add_2d_var(w2cfile,'shflux','w/m2','Sensible heat fluxes',hflux_c)
    call cg_add_2d_var(w2cfile,'lhflux','w/m2','Latent heat fluxes',lhflux_c)
    call cg_add_2d_var(w2cfile,'tsk','Kelvin','skin temperature',tsk_c)
    call cg_add_2d_var(w2cfile,'un10','m/s','u10',un10_c)
    call cg_add_2d_var(w2cfile,'vn10','m/s','v10',vn10_c)
    call cg_add_2d_var(w2cfile,'vt10','m/s','Tengential wind',u10t_c)
    call cg_add_2d_var(w2cfile,'vr10','m/s','Radial wind',u10r_c)
    call cg_add_2d_var(w2cfile,'cg_x','','x',cg_x)
    call cg_add_2d_var(w2cfile,'cg_y','','y',cg_y)
    if (ocean_model) then
      call cg_add_2d_var(w2cfile,'om_ml','m','mixed layer',om_ml_c)
    endif

    !        ! cylindrical 3-D variables


    ci1 = max(1 , int(eta_i_center(1) - RR(rx)*1000./ddx_c - 0.5))
    ci2 = min(ix, int(eta_i_center(1) + RR(rx)*1000./ddx_c + 1.5))
    cj1 = max(1 , int(eta_j_center(1) - RR(rx)*1000./ddx_c - 0.5))
    cj2 = min(jx, int(eta_j_center(1) + RR(rx)*1000./ddx_c + 1.5))
    cix = ci2 - ci1 + 1
    cjx = cj2 - cj1 + 1
    ii_cc = eta_i_center(1) - real(ci1 - 1)
    jj_cc = eta_j_center(1) - real(cj1 - 1)

    allocate(t_c (rx,args_w2c,kx))
    allocate(th_c (rx,args_w2c,kx))
    allocate(w_c (rx,args_w2c,kx))
    allocate(p_c (rx,args_w2c,kx))
    allocate(ph_c (rx,args_w2c,kx+1))
    allocate(pp_c (rx,args_w2c,kx))
    allocate(dbz_c (rx,args_w2c,kx))
    allocate(q_c (rx,args_w2c,kx))
    allocate(h_c (rx,args_w2c,kx))
    allocate(rh_c (rx,args_w2c,kx))
    allocate(qv_c (rx,args_w2c,kx))
    allocate(qr_c (rx,args_w2c,kx))
    allocate(qs_c (rx,args_w2c,kx))
    allocate(qc_c (rx,args_w2c,kx))
    allocate(qi_c (rx,args_w2c,kx))
    allocate(ut_c (rx,args_w2c,kx))
    allocate(ur_c (rx,args_w2c,kx))
    allocate(un_c (rx,args_w2c,kx))
    allocate(vn_c (rx,args_w2c,kx))
    allocate(tracer1_c (rx,args_w2c,kx))
    allocate(tracer2_c (rx,args_w2c,kx))
    allocate(tracer3_c (rx,args_w2c,kx))
    allocate(tracer4_c (rx,args_w2c,kx))
    allocate(xlong_c (rx,args_w2c,kx))
    allocate(xlat_c (rx,args_w2c,kx))

    if (ocean_model) then
      allocate (om_tmp_c  (rx,args_w2c,ox))
      allocate (om_u_c    (rx,args_w2c,ox))
      allocate (om_v_c    (rx,args_w2c,ox))
      allocate (om_s_c    (rx,args_w2c,ox))
      allocate (om_depth_c(rx,args_w2c,ox))
      ci1 = max(1 , int(eta_i_center(1) - RR(rx)*1000./ddx_c - 0.5))
      ci2 = min(ix, int(eta_i_center(1) + RR(rx)*1000./ddx_c + 1.5))
      cj1 = max(1 , int(eta_j_center(1) - RR(rx)*1000./ddx_c - 0.5))
      cj2 = min(jx, int(eta_j_center(1) + RR(rx)*1000./ddx_c + 1.5))
      cix = ci2 - ci1 + 1
      cjx = cj2 - cj1 + 1
      ii_cc = eta_i_center(1) - real(ci1 - 1)
      jj_cc = eta_j_center(1) - real(cj1 - 1)
      call ocean_compact (ci1, ci2, cj1, cj2, ox, OCEAN, cOCEAN)
      do k = 1, ox
        do r = 1, rx
        call w2c_circular_ave (cix, cjx, missing, cOCEAN%om_tmp  (:,:,k), ii_cc, jj_cc, RR(r)*1000./ddx_c, args_w2c, om_tmp_c (r,:,k))
        call w2c_circular_ave (cix, cjx, missing, cOCEAN%om_u  (:,:,k), ii_cc, jj_cc, RR(r)*1000./ddx_c, args_w2c, om_v_c (r,:,k))
        call w2c_circular_ave (cix, cjx, missing, cOCEAN%om_v  (:,:,k), ii_cc, jj_cc, RR(r)*1000./ddx_c, args_w2c, om_u_c (r,:,k))
        call w2c_circular_ave (cix, cjx, missing, cOCEAN%om_s  (:,:,k), ii_cc, jj_cc, RR(r)*1000./ddx_c, args_w2c, om_s_c (r,:,k))
        call w2c_circular_ave (cix, cjx, missing, cOCEAN%om_depth  (:,:,k), ii_cc, jj_cc, RR(r)*1000./ddx_c, args_w2c, om_depth_c (r,:,k))
        enddo
      enddo
      call ocean_nullify  (cOCEAN )
    endif

    do k = 1, kx

      ci1 = max(1 , int(eta_i_center(k) - RR(rx)*1000./ddx_c - 0.5))
      ci2 = min(ix, int(eta_i_center(k) + RR(rx)*1000./ddx_c + 1.5))
      cj1 = max(1 , int(eta_j_center(k) - RR(rx)*1000./ddx_c - 0.5))
      cj2 = min(jx, int(eta_j_center(k) + RR(rx)*1000./ddx_c + 1.5))
      cix = ci2 - ci1 + 1
      cjx = cj2 - cj1 + 1
      ii_cc = eta_i_center(k) - real(ci1 - 1)
      jj_cc = eta_j_center(k) - real(cj1 - 1)

      allocate (temp  (cix, cjx, kx))
      allocate (theta (cix, cjx, kx))
      allocate (w     (cix, cjx, kx))
      allocate (pres  (cix, cjx, kx))
      allocate (gph   (cix, cjx, kx))
      allocate (q     (cix, cjx, kx))
      allocate (rh    (cix, cjx, kx))
      call state_compact (ci1, ci2, cj1, cj2, kx, STATE, cSTATE)
      call base_compact  (ci1, ci2, cj1, cj2, kx, BASE , cBASE )

      call get_eta_level_variable(cix, cjx, kx, cSTATE, cBASE, temp, theta, w, pres, gph, q, rh)
        do r = 1, rx
          call w2c_circular_ave (cix, cjx, missing, temp     (:,:,k), ii_cc, jj_cc, RR(r)*1000./ddx_c, args_w2c, t_c (r,:,k))

          call w2c_circular_ave (cix, cjx, missing, theta    (:,:,k), ii_cc, jj_cc, RR(r)*1000./ddx_c, args_w2c, th_c(r,:,k))
          call w2c_circular_ave (cix, cjx, missing, w        (:,:,k), ii_cc, jj_cc, RR(r)*1000./ddx_c, args_w2c, w_c (r,:,k))
          call w2c_circular_ave (cix, cjx, missing, pres     (:,:,k), ii_cc, jj_cc, RR(r)*1000./ddx_c, args_w2c, p_c (r,:,k))
          call w2c_circular_ave (cix, cjx, missing, gph      (:,:,k), ii_cc, jj_cc, RR(r)*1000./ddx_c, args_w2c, h_c (r,:,k))
          call w2c_circular_ave (cix, cjx, missing, q        (:,:,k), ii_cc, jj_cc, RR(r)*1000./ddx_c, args_w2c, q_c (r,:,k))
          call w2c_circular_ave (cix, cjx, missing, rh       (:,:,k), ii_cc, jj_cc, RR(r)*1000./ddx_c, args_w2c, rh_c(r,:,k))
          call w2c_circular_ave (cix, cjx, missing, cSTATE%qc(:,:,k), ii_cc, jj_cc, RR(r)*1000./ddx_c, args_w2c, qc_c(r,:,k))
          call w2c_circular_ave (cix, cjx, missing, cSTATE%qv(:,:,k), ii_cc, jj_cc, RR(r)*1000./ddx_c, args_w2c, qv_c(r,:,k))
          call w2c_circular_ave (cix, cjx, missing, cSTATE%qr(:,:,k), ii_cc, jj_cc, RR(r)*1000./ddx_c, args_w2c, qr_c(r,:,k))
          call w2c_circular_ave (cix, cjx, missing, cSTATE%qs(:,:,k), ii_cc, jj_cc, RR(r)*1000./ddx_c, args_w2c, qs_c(r,:,k))
          call w2c_circular_ave (cix, cjx, missing, cSTATE%qi(:,:,k), ii_cc, jj_cc, RR(r)*1000./ddx_c, args_w2c, qi_c(r,:,k))
          call w2c_circular_ave (cix, cjx, missing, cSTATE%ph(:,:,k), ii_cc, jj_cc, RR(r)*1000./ddx_c, args_w2c, ph_c(r,:,k))
          call w2c_circular_ave (cix, cjx, missing, cSTATE%p(:,:,k) , ii_cc, jj_cc, RR(r)*1000./ddx_c, args_w2c, pp_c(r,:,k))
          call w2c_circular_ave (cix, cjx, missing, cBASE%xlong(:,:), ii_cc, jj_cc, RR(r)*1000./ddx_c, args_w2c, xlong_c(r,:,k))
          call w2c_circular_ave (cix, cjx, missing, cBASE%xlat(:,:), ii_cc, jj_cc, RR(r)*1000./ddx_c, args_w2c, xlat_c(r,:,k))
          call w2c_circular_ave (cix, cjx, missing, cSTATE%tracer1(:,:,k), ii_cc, jj_cc, RR(r)*1000./ddx_c, args_w2c, tracer1_c(r,:,k))
          call w2c_circular_ave (cix, cjx, missing, cSTATE%tracer2(:,:,k), ii_cc, jj_cc, RR(r)*1000./ddx_c, args_w2c, tracer2_c(r,:,k))
          call w2c_circular_ave (cix, cjx, missing, cSTATE%tracer3(:,:,k), ii_cc, jj_cc, RR(r)*1000./ddx_c, args_w2c, tracer3_c(r,:,k))
          call w2c_circular_ave (cix, cjx, missing, cSTATE%tracer4(:,:,k), ii_cc, jj_cc, RR(r)*1000./ddx_c, args_w2c, tracer4_c(r,:,k))

          call w2c_circular_ave_stag_uv (cix, cjx, missing, cSTATE%u(:,:,k), cSTATE%v(:,:,k), &
                ii_cc, jj_cc, RR(r)*1000./ddx_c, args_w2c, ut_c(r,:,k), ur_c(r,:,k),un_c(r,:,k),vn_c(r,:,k))
!cyl
          if ( k .eq. kx ) then
              call w2c_circular_ave (cix, cjx, missing, cSTATE%ph(:,:,kx+1), ii_cc, jj_cc, RR(r)*1000./ddx_c, args_w2c, ph_c(r,:,kx+1))
          endif
          if (k .eq. 1 ) then
              call w2c_circular_ave (cix, cjx, missing, cSTATE%mu(:,:), ii_cc, jj_cc, RR(r)*1000./ddx_c, args_w2c, mu_c(r,:))
          endif


      enddo ![r=1,rx]
      call state_nullify (cSTATE)
      call base_nullify  (cBASE )
      deallocate(temp,theta,w,pres,gph,q,rh)
    end do ![k = 1, kx]

    call caldbz(qv_c,qr_c,qc_c,qi_c,qs_c,t_c,p_c,dbz_c,rx,args_w2c,kx,missing)

    !write 3D variables in netcdf file

    rcode=cg_add_3d_var(w2cfile,'t','Kelvin','temperature',t_c)
    rcode=cg_add_3d_var(w2cfile,'th','Kelvin','potential temperature',th_c)
    rcode=cg_add_3d_var(w2cfile,'w','m/s','vertical velocity',w_c)
    rcode=cg_add_3d_var(w2cfile,'pressure','pa','pressure',p_c)
    rcode=cg_add_3d_var_zstag(w2cfile,'perturbation_ph','m','geo potential high perturbation',ph_c)
    rcode=cg_add_3d_var(w2cfile,'perturbation_p','pa','pressure perturbation',pp_c)
    rcode=cg_add_3d_var(w2cfile,'h','m','height',h_c)
    rcode=cg_add_3d_var(w2cfile,'q','','water vapor',q_c)
    rcode=cg_add_3d_var(w2cfile,'qv','','water vapor',qv_c)
    rcode=cg_add_3d_var(w2cfile,'rh','%','relative humidity',rh_c)
    rcode=cg_add_3d_var(w2cfile,'qc','','cloud water',qc_c)
    rcode=cg_add_3d_var(w2cfile,'qi','','ice water',qi_c)
    rcode=cg_add_3d_var(w2cfile,'vt','m/s','tangential wind',ut_c)
    rcode=cg_add_3d_var(w2cfile,'vr','m/s','radial wind',ur_c)
    rcode=cg_add_3d_var(w2cfile,'un','m/s','u',un_c)
    rcode=cg_add_3d_var(w2cfile,'vn','m/s','v',vn_c)
    rcode=cg_add_3d_var(w2cfile,'dbz',' ','Radar reflectivity',dbz_c)
    rcode=cg_add_3d_var(w2cfile,'xlong','degree','longitude',xlong_c)
    rcode=cg_add_3d_var(w2cfile,'xlat','degree','latitude',xlat_c)
    rcode=cg_add_3d_var(w2cfile,'tracer1','degree','latitude',tracer1_c)
    rcode=cg_add_3d_var(w2cfile,'tracer2','degree','latitude',tracer2_c)
    rcode=cg_add_3d_var(w2cfile,'tracer3','degree','latitude',tracer3_c)
    rcode=cg_add_3d_var(w2cfile,'tracer4','degree','latitude',tracer4_c)
    call cg_add_2d_var(w2cfile,'mu','pa','mu',mu_c)
    if (ocean_model) then
      rcode=cg_add_3d_oceanvar(w2cfile,'om_tmp','Kelvin','ocean temperature',om_tmp_c)
      rcode=cg_add_3d_oceanvar(w2cfile,'om_u','m/s','ocean u',om_v_c)
      rcode=cg_add_3d_oceanvar(w2cfile,'om_v','m/s','ocean v',om_u_c)
      rcode=cg_add_3d_oceanvar(w2cfile,'om_s','','ocean salinity',om_s_c)
      rcode=cg_add_3d_oceanvar(w2cfile,'om_depth','m','ocean depth',om_depth_c)
    endif


    print*,'ddx_c',ddx_c

    write (*, *)
    write (*, '(A)') ' DONE !!'
    write (*, *)
    endif ![if output_w2c]
!-------------------------------------------------------------------------------

end program VORTEX

!-------------------------------------------------------------------------------
function  get_date(trim_str)
     implicit none
     character (len=19) :: trim_str
     character (len=10) :: get_date
      write (get_date, '(A4,A2,A2,A2)') &
            trim_str(1:4),trim_str(6:7),trim_str(9:10),trim_str(12:13)

end function get_date
!-------------------------------------------------------------------------------
function  get_date_iso(trim_str)
  implicit none
  character (len=19) :: trim_str
  character (len=19) :: get_date_iso
   write (get_date_iso, '(A10,A1,A8)') &
         trim_str(1:10),'T',trim_str(12:19)

end function get_date_iso
!-------------------------------------------------------------------------------

function atan2_atmos_deg (x, y)

      implicit none
      real :: atan2_atmos_deg
      real :: x, y

      atan2_atmos_deg = 90. - atan2(x, y) * 57.295779513  ! [180. / pi]
      if (atan2_atmos_deg .LT. 0) atan2_atmos_deg = atan2_atmos_deg + 360.

end function atan2_atmos_deg

!-------------------------------------------------------------------------------

function grads_time_format (time_str)

      implicit none
      character (len=15) :: grads_time_format
      character (len=19) :: time_str
      character (len=3)  :: month

      select case (time_str(6:7))
      case ('01')
        month = 'jan'
      case ('02')
        month = 'feb'
      case ('03')
        month = 'mar'
      case ('04')
        month = 'apr'
      case ('05')
        month = 'may'
      case ('06')
        month = 'jun'
      case ('07')
        month = 'jul'
      case ('08')
        month = 'aug'
      case ('09')
        month = 'sep'
      case ('10')
        month = 'oct'
      case ('11')
        month = 'nov'
      case ('12')
        month = 'dec'
      end select

      write (grads_time_format, '(A5,A1,A2,A3,A4)') &
             time_str(12:16), 'Z', time_str(9:10), month, time_str(1:4)

end function grads_time_format

!-------------------------------------------------------------------------------
