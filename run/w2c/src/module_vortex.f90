!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  subroutine find_vortex_center   (ix, jx, kpx, missing, h_p, ddx, ii_fg, jj_fg, s_grid, ii_find, jj_find)
!  subroutine find_extreme_value   (ix, jx, missing, var,  ii_fg, jj_fg, s_grid, max_min, var_extrame, ii_find, jj_find)
!  subroutine find_max_wspd        (ix, jx, missing, u, v, ii_fg, jj_fg, s_grid, wspd_max, ii_find, jj_find)
!  subroutine find_max_stag_wspd   (ix, jx, missing, u, v, ii_fg, jj_fg, s_grid, wspd_max, ii_find, jj_find)
!  subroutine circular_ave         (ix, jx, missing, var,  ii_c,  jj_c,  r, args, var_ave)
!  subroutine circular_ave_uv      (ix, jx, missing, u, v, ii_c,  jj_c,  r, args, ut, ur)
!  subroutine circular_ave_stag_uv (ix, jx, missing, u, v, ii_c,  jj_c,  r, args, ut, ur)
!  subroutine smoothing 
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module VORTEX_TOOLS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use WRF_TOOLS
  implicit none

  real, parameter      :: ave_dist = 100.   ! distance to do the weighting procedure (km)

  contains

!-------------------------------------------------------------------------------
!  Search vortex center using vertical-averaging geopotential height
!-------------------------------------------------------------------------------

  subroutine find_vortex_center (ix, jx, kpx, missing, h_p, &
                                 ddx, ii_fg, jj_fg, s_grid, ii_find, jj_find)

      implicit none

!-------------------------------------------------------------------------------

      integer                         :: ix, jx, kpx          ! [in]
      real                            :: missing              ! [in]
      real, dimension(ix,  jx,  kpx ) :: h_p                  ! [in]

      real                            :: ddx                  ! [in]  grid distance (meter)
      real                            :: ii_fg, jj_fg         ! [in]  first guess in grid # (float)
      integer                         :: s_grid               ! [in]  search radius in grid #

      real, intent(out)               :: ii_find, jj_find     ! [out] vortex center in grid # (float)

!-------------------------------------------------------------------------------

      integer                         :: i, j, kp, i1, i2, j1, j2

      real, dimension(ix,  jx       ) :: h_low

      integer                         :: ave_grid
      integer                         :: ii_find_i, jj_find_i
      real                            :: h_low_min, h_tot, h_ave, weight_tot, weight_i, weight_j
      integer                         :: h_tot_count

!-------------------------------------------------------------------------------

      ave_grid = int(ave_dist*1000. / ddx + 0.5)

!-------------------------------------------------------------------------------

      h_low(:,:) = 0.

      i1 = max(1,  int(ii_fg+0.5) - s_grid - ave_grid - 2)
      i2 = min(ix, int(ii_fg+0.5) + s_grid + ave_grid + 2)
      j1 = max(1,  int(jj_fg+0.5) - s_grid - ave_grid - 2)
      j2 = min(jx, int(jj_fg+0.5) + s_grid + ave_grid + 2)

      do j = j1, j2
      do i = i1, i2
        do kp = 1, kpx
          h_low(i,j) = h_low(i,j) + h_p(i,j,kp)
        end do
      end do
      end do

!-------------------------------------------------------------------------------

      i1 = max(1,  int(ii_fg+0.5) - s_grid)
      i2 = min(ix, int(ii_fg+0.5) + s_grid)
      j1 = max(1,  int(jj_fg+0.5) - s_grid)
      j2 = min(jx, int(jj_fg+0.5) + s_grid)

      h_low_min = 1.e20
      ii_find_i = 0
      jj_find_i = 0

      do j = j1, j2
      do i = i1, i2
        if ((h_low(i,j) .LT. h_low_min) .AND. (h_low(i,j) .LT. missing/10.)) then
          h_low_min = h_low(i,j)
          ii_find_i = i
          jj_find_i = j
        endif
      end do
      end do

!-------------------------------------------------------------------------------

      i1 = ii_find_i - min(ave_grid, ii_find_i - 1, ix - ii_find_i)
      i2 = ii_find_i + min(ave_grid, ii_find_i - 1, ix - ii_find_i)
      j1 = jj_find_i - min(ave_grid, jj_find_i - 1, jx - jj_find_i)
      j2 = jj_find_i + min(ave_grid, jj_find_i - 1, jx - jj_find_i)

      h_tot = 0.
      h_tot_count = 0
      do j = j1, j2
      do i = i1, i2
        if (h_low(i,j) .LT. missing/10.) then
          h_tot = h_tot + h_low(i,j)
          h_tot_count = h_tot_count + 1
        end if
      end do
      end do

      h_ave = h_tot / real(h_tot_count)

      weight_tot = 0.
      weight_i = 0.
      weight_j = 0.

      do j = j1, j2
      do i = i1, i2
        if (h_low(i,j) .LT. h_ave) then
          weight_i = weight_i + real(i) * (h_ave - h_low(i,j))
          weight_j = weight_j + real(j) * (h_ave - h_low(i,j))
          weight_tot = weight_tot + (h_ave - h_low(i,j))
        end if
      end do
      end do

      ii_find = weight_i / weight_tot
      jj_find = weight_j / weight_tot

!-------------------------------------------------------------------------------

  end subroutine find_vortex_center

!-------------------------------------------------------------------------------
!  Find the extreme (min. and max.) value within a 2-D square
!-------------------------------------------------------------------------------

  subroutine find_extreme_value (ix, jx, missing, var, ii_fg, jj_fg, s_grid, max_min, &
                                 var_extreme, ii_find, jj_find)

      implicit none

!---------------------------------------------------------------------------------

      integer                 :: ix, jx           ! [in]
      real                    :: missing          ! [in]
      real, dimension(ix, jx) :: var              ! [in]
      real                    :: ii_fg, jj_fg     ! [in]  first guess in grid # (float)
      integer                 :: s_grid           ! [in]  search radius in grid #
      logical                 :: max_min          ! [in]  .true. for max search, .false. for min search

      real,       intent(out) :: var_extreme      ! [out] minimum sea level pressure (Pa)
      integer,    intent(out) :: ii_find, jj_find ! [out] location of min_slp in grid # (integer)

      real                    :: tmp
      integer                 :: i, j, i1, i2, j1, j2

!---------------------------------------------------------------------------------

      i1 = max(1,  int(ii_fg+0.5) - s_grid)
      i2 = min(ix, int(ii_fg+0.5) + s_grid)
      j1 = max(1,  int(jj_fg+0.5) - s_grid)
      j2 = min(jx, int(jj_fg+0.5) + s_grid)

      if (max_min) then
        var_extreme = -1.e20
      else
        var_extreme =  1.e20
      end if
      ii_find = 0.
      jj_find = 0.

      do j = j1, j2
      do i = i1, i2
        if (max_min) then
          tmp = var(i,j) - var_extreme
        else
          tmp = var_extreme - var(i,j)
        end if
        if ((tmp .GT. 0.) .AND. (var(i,j) .NE. missing)) then
          var_extreme = var(i,j)
          ii_find = i
          jj_find = j
        end if
      end do
      end do

      print*,'var_extreme',var_extreme,i1,i2,j1,j2
      print*,'ix,jx',ix,jx  
!---------------------------------------------------------------------------------

  end subroutine find_extreme_value
!-------------------------------------------------------------------------------
!  Find the maximum 2% tail wind speed within a 2-D square
!-------------------------------------------------------------------------------

  subroutine find_max_wspd_tail (ix, jx, missing, u, v, ii_fg, jj_fg, s_grid, &
                            wspd_max,xpercentile)

      implicit none

!---------------------------------------------------------------------------------

      integer                 :: ix, jx           ! [in]
      real                    :: missing          ! [in]
      real, dimension(ix, jx) :: u, v             ! [in]
      real                    :: ii_fg, jj_fg     ! [in]  first guess in grid # (float)
      integer                 :: s_grid           ! [in]  search radius in grid #

      real,       intent(out) :: wspd_max         ! [out] minimum sea level pressure (Pa)

      real ,allocatable      :: wspd_t(:)
      integer                 :: i, j, i1, i2, j1, j2,n3
      integer                 :: count_wspd
      real                    :: xpercentile

!---------------------------------------------------------------------------------

      i1 = max(1,  int(ii_fg+0.5) - s_grid)
      i2 = min(ix, int(ii_fg+0.5) + s_grid)
      j1 = max(1,  int(jj_fg+0.5) - s_grid)
      j2 = min(jx, int(jj_fg+0.5) + s_grid)
   
      n3 = (i2-i1+1)*(j2-j1+1)
      allocate (wspd_t(1:n3))

      wspd_max = -999.

      count_wspd = 1
      do j = j1, j2
      do i = i1, i2
        if ((u(i,j) .NE. missing) .AND. (v(i,j) .NE. missing)) then
          wspd_t(count_wspd) = sqrt(u(i,j) * u(i,j) + v(i,j) * v(i,j))
          count_wspd = count_wspd + 1
        end if
      end do
      end do
      call bubblesort ( wspd_t, count_wspd-1 )
      call averagepercentile ( wspd_t, count_wspd-1, xpercentile ,wspd_max )
      if ( wspd_max == 0.0 ) then
         wspd_max = -999.
      endif
      return
!---------------------------------------------------------------------------------

  end subroutine find_max_wspd_tail

!-------------------------------------------------------------------------------
! sort bubble
!-------------------------------------------------------------------------------
  subroutine bubblesort ( x, sizex )
  implicit none 
  integer :: sizex
  real, dimension(sizex), intent(inout) :: x
  integer :: n_tail, jmax, i, j
  real temp

  jmax = sizex - 1
  do i = 1, sizex-1
     temp = 1.e38
     do j = 1, jmax
        if (x(j) .le. x(j+1)) then
           temp = x(j)
           x(j) = x ( j+1 ) 
           x(j+1) = temp
        endif
     enddo
     jmax  = jmax - 1
  enddo

  end subroutine bubblesort 

!-------------------------------------------------------------------------------
! throw percentile away
!-------------------------------------------------------------------------------
  subroutine throwpercentile ( x, sizex, xpercentile,xout )
  implicit none
  integer :: sizex
  integer :: n_tile
  real, dimension(sizex), intent(in) :: x
  real, intent(in) :: xpercentile
  real, intent(out) :: xout
  
   n_tile = max(1, int ( real(sizex)*xpercentile))
   if ( sizex > 1 ) then
      xout = x(n_tile+1) 
   else
      xout = 0.0
   endif
  return
  end subroutine throwpercentile
!-------------------------------------------------------------------------------
! average percenile
!-------------------------------------------------------------------------------
  subroutine averagepercentile (x, sizex, xpercentile, xout )
  implicit none
  integer :: sizex, n_tile
  real, dimension(sizex), intent(in) :: x
  real, intent(in) :: xpercentile
  real, intent(out) :: xout
  n_tile = max(1, int ( real(sizex)*xpercentile))
  if ( sizex > 1 ) then
     xout=sum(x(1:n_tile))/real(n_tile)
  else
     xout=0.0
  endif
  return
  end subroutine averagepercentile
!-------------------------------------------------------------------------------
!  Find the maximum wind speed within a 2-D square
!-------------------------------------------------------------------------------

  subroutine find_max_wspd (ix, jx, missing, u, v, ii_fg, jj_fg, s_grid, &
                            wspd_max, ii_find, jj_find)

      implicit none

!---------------------------------------------------------------------------------

      integer                 :: ix, jx           ! [in]
      real                    :: missing          ! [in]
      real, dimension(ix, jx) :: u, v             ! [in]
      real                    :: ii_fg, jj_fg     ! [in]  first guess in grid # (float)
      integer                 :: s_grid           ! [in]  search radius in grid #

      real,       intent(out) :: wspd_max         ! [out] minimum sea level pressure (Pa)
      integer,    intent(out) :: ii_find, jj_find ! [out] location of min_slp in grid # (integer)

      real                    :: wspd_t
      integer                 :: i, j, i1, i2, j1, j2

!---------------------------------------------------------------------------------

      i1 = max(1,  int(ii_fg+0.5) - s_grid)
      i2 = min(ix, int(ii_fg+0.5) + s_grid)
      j1 = max(1,  int(jj_fg+0.5) - s_grid)
      j2 = min(jx, int(jj_fg+0.5) + s_grid)

      wspd_max = -999.
      ii_find = 0.
      jj_find = 0.

      do j = j1, j2
      do i = i1, i2
        if ((u(i,j) .NE. missing) .AND. (v(i,j) .NE. missing)) then
          wspd_t = sqrt(u(i,j) * u(i,j) + v(i,j) * v(i,j))
          if (wspd_t .GT. wspd_max) then
            wspd_max = wspd_t
            ii_find = i
            jj_find = j
          end if
        end if
      end do
      end do

!---------------------------------------------------------------------------------

  end subroutine find_max_wspd

!-------------------------------------------------------------------------------
!  Find the maximum wind speed in staggered grids within a 2-D square
!-------------------------------------------------------------------------------

  subroutine find_max_stag_wspd (ix, jx, missing, u, v, ii_fg, jj_fg, s_grid, &
                                      wspd_max, ii_find, jj_find)

      implicit none

!---------------------------------------------------------------------------------

      integer                 :: ix, jx           ! [in]
      real                    :: missing          ! [in]
      real, dimension(ix, jx) :: u, v             ! [in]
      real                    :: ii_fg, jj_fg     ! [in]  first guess in grid # (float)
      integer                 :: s_grid           ! [in]  search radius in grid #

      real,       intent(out) :: wspd_max         ! [out] minimum sea level pressure (Pa)
      integer,    intent(out) :: ii_find, jj_find ! [out] location of min_slp in grid # (integer)

      real                    :: u_t, v_t, wspd_t
      integer                 :: i, j, i1, i2, j1, j2

!---------------------------------------------------------------------------------

      i1 = max(1,  int(ii_fg+0.5) - s_grid)
      i2 = min(ix, int(ii_fg+0.5) + s_grid)
      j1 = max(1,  int(jj_fg+0.5) - s_grid)
      j2 = min(jx, int(jj_fg+0.5) + s_grid)

      wspd_max = -999.
      ii_find = 0.
      jj_find = 0.

      do j = j1, j2
      do i = i1, i2
        if ((u(i  ,j) .NE. missing) .AND. (v(i,j  ) .NE. missing) .AND. &
            (u(i+1,j) .NE. missing) .AND. (v(i,j+1) .NE. missing)) then
          u_t = 0.5 * (u(i,j) + u(i+1,j  ))
          v_t = 0.5 * (v(i,j) + v(i  ,j+1))
          wspd_t = sqrt(u_t * u_t + v_t * v_t)
          if (wspd_t .GT. wspd_max) then
            wspd_max = wspd_t
            ii_find = i
            jj_find = j
          end if
        end if
      end do
      end do

!---------------------------------------------------------------------------------

  end subroutine find_max_stag_wspd

!---------------------------------------------------------------------------------
!  Calculate the circular-averaging value
!---------------------------------------------------------------------------------

  subroutine circular_ave (ix, jx, missing, var, ii_c, jj_c, r, args, var_ave)

!      use WRF_TOOLS
      implicit none

      integer                :: ix, jx       ! [in]
      real                   :: missing      ! [in]
      real, dimension(ix,jx) :: var          ! [in]
      real                   :: ii_c, jj_c   ! [in]
      real                   :: r            ! [in]
      integer                :: args         ! [in]
      real, intent(out)      :: var_ave      ! [out]

!-------------------------------------------------------------------------------

      integer :: i_args
      real    :: ii, jj
      real    :: var_tot
      real    :: var_args
      integer :: io, jo
      real    :: dx, dxm, dy, dym
      logical :: is_missing

  real, parameter      :: pi = 3.1415926535
  real, parameter      :: two_pi = 2. * pi
!-------------------------------------------------------------------------------

      var_ave = 0.
      var_tot = 0.
      is_missing = .FALSE.

      do i_args = 1, args
!-------------------------------------------------------------------------------

        ii = ii_c + r * cos(two_pi * real(i_args)/real(args))
        jj = jj_c + r * sin(two_pi * real(i_args)/real(args))
        if ((ii .LT. 1.) .OR. (ii .GT. ix) .OR. &
            (jj .LT. 1.) .OR. (jj .GT. jx)) then
          is_missing = .TRUE.
          exit
        end if

        call toGrid(ii, 1, ix, io, dx, dxm)
        call toGrid(jj, 1, jx, jo, dy, dym)

        if ((var(io,jo  ) .EQ. missing) .OR. (var(io+1,jo  ) .EQ. missing) .OR. &
            (var(io,jo+1) .EQ. missing) .OR. (var(io+1,jo+1) .EQ. missing)) then
          is_missing = .TRUE.
          exit
        end if

        var_args = dym * (dxm * var(io,jo  ) + dx * var(io+1,jo  )) &
                 + dy  * (dxm * var(io,jo+1) + dx * var(io+1,jo+1))
        var_tot = var_tot + var_args

!-------------------------------------------------------------------------------
      end do ! [ i_args = 1, args ]

      if (.NOT. is_missing) then
        var_ave = var_tot / args
      else
        var_ave = missing
      end if

!-------------------------------------------------------------------------------

  end subroutine circular_ave

!---------------------------------------------------------------------------------
!  Calculate the circular-averaging tangential and radial wind
!---------------------------------------------------------------------------------

  subroutine circular_ave_uv (ix, jx, missing, u, v, ii_c, jj_c, r, args, ut, ur)

!      use WRF_TOOLS
      implicit none

      integer                :: ix, jx       ! [in]
      real                   :: missing      ! [in]
      real, dimension(ix,jx) :: u, v         ! [in]
      real                   :: ii_c, jj_c   ! [in]
      real                   :: r            ! [in]
      integer                :: args         ! [in]
      real, intent(out)      :: ut           ! [out]
      real, intent(out)      :: ur           ! [out]

!-------------------------------------------------------------------------------

      integer :: i_args
      real    :: ii, jj
      real    :: ut_tot, ur_tot
      real    :: tangent_unit_x, tangent_unit_y
      real    :: radial_unit_x, radial_unit_y
      real    :: u_args, v_args, ut_args, ur_args
      integer :: io1, jo1
      real    :: dx1, dxm1, dy1, dym1
      integer :: io2, jo2
      real    :: dx2, dxm2, dy2, dym2
      logical :: is_missing

  real, parameter      :: pi = 3.1415926535
  real, parameter      :: two_pi = 2. * pi
!-------------------------------------------------------------------------------

      ut = 0.
      ur = 0.
      ut_tot = 0.
      ur_tot = 0.
      is_missing = .FALSE.

      do i_args = 1, args
!-------------------------------------------------------------------------------

        ii = ii_c + r * cos(two_pi * real(i_args)/real(args))
        jj = jj_c + r * sin(two_pi * real(i_args)/real(args))
        if ((ii .LT. 1.) .OR. (ii .GT. ix) .OR. &
            (jj .LT. 1.) .OR. (jj .GT. jx)) then
          is_missing = .TRUE.
          exit
        end if

        call toGrid(ii, 1, ix, io1, dx1, dxm1)
        call toGrid(jj, 1, jx, jo1, dy1, dym1)
        call toGrid(ii, 1, ix, io2, dx2, dxm2)
        call toGrid(jj, 1, jx, jo2, dy2, dym2)

        u_args = dym1 * (dxm1 * u(io1,jo1  ) + dx1 * u(io1+1,jo1  )) &
               + dy1  * (dxm1 * u(io1,jo1+1) + dx1 * u(io1+1,jo1+1))
        v_args = dym2 * (dxm2 * v(io2,jo2  ) + dx2 * v(io2+1,jo2  )) &
               + dy2  * (dxm2 * v(io2,jo2+1) + dx2 * v(io2+1,jo2+1))
        if ((abs(u_args) .GT. 300.) .OR. (abs(v_args) .GT. 300.)) then
          is_missing = .TRUE.
          exit
        end if

        tangent_unit_x = cos(two_pi * real(i_args)/real(args) + 0.5*pi) !! unit vector of tangential dir.
        tangent_unit_y = sin(two_pi * real(i_args)/real(args) + 0.5*pi) !!
        radial_unit_x  = cos(two_pi * real(i_args)/real(args))          !! unit vector of radial dir.
        radial_unit_y  = sin(two_pi * real(i_args)/real(args))          !!

        ut_args = u_args * tangent_unit_x + v_args * tangent_unit_y !! tangential wind speed
        ur_args = u_args * radial_unit_x  + v_args * radial_unit_y  !! radial wind speed
        ut_tot = ut_tot + ut_args
        ur_tot = ur_tot + ur_args

!-------------------------------------------------------------------------------
      end do ! [ i_args = 1, args ]

      if (.NOT. is_missing) then
        ut = ut_tot / args
        ur = ur_tot / args
      else
        ut = missing
        ur = missing
      end if

!-------------------------------------------------------------------------------

  end subroutine circular_ave_uv

!---------------------------------------------------------------------------------
!  Calculate the circular-averaging tangential and radial wind in staggered grids
!---------------------------------------------------------------------------------

  subroutine circular_ave_stag_uv (ix, jx, missing, u, v, ii_c, jj_c, r, args, ut, ur)

!      use WRF_TOOLS
      implicit none

      integer                    :: ix, jx       ! [in]
      real                       :: missing      ! [in]
      real, dimension(ix+1,jx  ) :: u            ! [in]
      real, dimension(ix  ,jx+1) :: v            ! [in]
      real                       :: ii_c, jj_c   ! [in]
      real                       :: r            ! [in]
      integer                    :: args         ! [in]
      real, intent(out)          :: ut           ! [out]
      real, intent(out)          :: ur           ! [out]

!-------------------------------------------------------------------------------

      integer :: i_args
      real    :: ii, jj
      real    :: ut_tot, ur_tot
      real    :: tangent_unit_x, tangent_unit_y
      real    :: radial_unit_x, radial_unit_y
      real    :: u_args, v_args, ut_args, ur_args
      integer :: io1, jo1
      real    :: dx1, dxm1, dy1, dym1
      integer :: io2, jo2
      real    :: dx2, dxm2, dy2, dym2
      logical :: is_missing
  real, parameter      :: pi = 3.1415926535
  real, parameter      :: two_pi = 2. * pi

!-------------------------------------------------------------------------------

      ut = 0.
      ur = 0.
      ut_tot = 0.
      ur_tot = 0.
      is_missing = .FALSE.

      do i_args = 1, args
!-------------------------------------------------------------------------------

        ii = ii_c + r * cos(two_pi * real(i_args)/real(args))
        jj = jj_c + r * sin(two_pi * real(i_args)/real(args))
        if ((ii .LT. 1.) .OR. (ii .GT. ix) .OR. &
            (jj .LT. 1.) .OR. (jj .GT. jx)) then
          is_missing = .TRUE.
          exit
        end if

        call toGrid(ii+0.5, 1, ix+1, io1, dx1, dxm1)
        call toGrid(jj,     1, jx,   jo1, dy1, dym1)
        call toGrid(ii,     1, ix,   io2, dx2, dxm2)
        call toGrid(jj+0.5, 1, jx+1, jo2, dy2, dym2)

        u_args = dym1 * (dxm1 * u(io1,jo1  ) + dx1 * u(io1+1,jo1  )) &
               + dy1  * (dxm1 * u(io1,jo1+1) + dx1 * u(io1+1,jo1+1))
        v_args = dym2 * (dxm2 * v(io2,jo2  ) + dx2 * v(io2+1,jo2  )) &
               + dy2  * (dxm2 * v(io2,jo2+1) + dx2 * v(io2+1,jo2+1))
        if ((abs(u_args) .GT. 300.) .OR. (abs(v_args) .GT. 300.)) then
          is_missing = .TRUE.
          exit
        end if

        tangent_unit_x = cos(two_pi * real(i_args)/real(args) + 0.5*pi) !! unit vector of tangential dir.
        tangent_unit_y = sin(two_pi * real(i_args)/real(args) + 0.5*pi) !!
        radial_unit_x  = cos(two_pi * real(i_args)/real(args))          !! unit vector of radial dir.
        radial_unit_y  = sin(two_pi * real(i_args)/real(args))          !!

        ut_args = u_args * tangent_unit_x + v_args * tangent_unit_y !! tangential wind speed
        ur_args = u_args * radial_unit_x  + v_args * radial_unit_y  !! radial wind speed
        ut_tot = ut_tot + ut_args
        ur_tot = ur_tot + ur_args

!-------------------------------------------------------------------------------
      end do ! [ i_args = 1, args ]

      if (.NOT. is_missing) then
        ut = ut_tot / args
        ur = ur_tot / args
      else
        ut = missing
        ur = missing
      end if

!-------------------------------------------------------------------------------

  end subroutine circular_ave_stag_uv
!-------------------------------------------------------------------------------
  subroutine wspd_smoothing_2d(wspd,ix,jx)

    implicit none
    integer :: i, j
    integer :: ix, jx
    real, dimension(ix, jx),intent(inout) :: wspd
    real, dimension(ix, jx) :: wspd1
    real :: oneoversix

    oneoversix = 1./6. 
    wspd1 = wspd

    do j = 2, jx-1
       do i = 2, ix-1
          wspd1(i,j) =(wspd(i,j)*2 + wspd(i-1,j) + wspd(i+1,j) + wspd(i,j-1) + wspd(i,j+1))*oneoversix 
       enddo
    enddo

    wspd = wspd1

  end subroutine wspd_smoothing_2d

!-------------------------------------------------------------------------------
  subroutine smoothing_2d_2 (wspd,ix,jx)
    implicit none
    integer :: i, j
    integer :: ix, jx
    real, dimension(ix, jx),intent(inout) :: wspd
    real, dimension(ix, jx) :: wspd1
    real :: oneover59
 
    oneover59 = 1./59.
    wspd1 = wspd
 
    do j = 3, jx-3
       do i =3, ix -3
           wspd1(1,j) =(1.0*(wspd(i-2,j+1)+wspd(i-2,j-1)+wspd(i-1,j+2)+wspd(i-1,j-2)  + &
   &                         wspd(i+1,j+2)+wspd(i+1,j-2)+wspd(i+2,j+1)+wspd(i+2,j-1)) + &
   &                    3.0*(wspd(i-2,j  )+wspd(i-1,j+1)+wspd(i-1,j-1)+wspd(i  ,j+2)  + &
   &                         wspd(i  ,j-2)+wspd(i+1,j+1)+wspd(i+1,j-1)+wspd(i+2,j  )) + &
   &                    5.0*(wspd(i-1,j  )+wspd(i+1,j  )+wspd(i  ,j-1)+wspd(i  ,j+1)) + &
   &                    7.0*(wspd(i,j)))*oneover59                                                         
       enddo
    enddo
    wspd = wspd1
     
 
  end subroutine smoothing_2d_2
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module VORTEX_TOOLS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
