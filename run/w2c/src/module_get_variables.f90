!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  subroutine get_p_level_variable   (ix, jx, kx, iSTATE, iBASE, kpx, levels, t_p, th_p, u_p, v_p, w_p, h_p, q_p, rh_p, qc_p, qi_p)
!  subroutine get_p_level_gph        (ix, jx, kx, iSTATE, iBASE, kpx, levels, h_p)
!  subroutine get_p_level_uv         (ix, jx, kx, iSTATE, iBASE, level, u_p, v_p)
!  subroutine get_eta_level_variable (ix, jx, kx, iSTATE, iBASE, t, th, u, v, w, p, h, q, rh, qc, qi)
!  subroutine get_slp                (ix, jx, kx, iSTATE, iBASE, slp)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module GET_VARIABLES

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  real, parameter :: To = 300.
  real, parameter :: g  = 9.81

  contains

!-------------------------------------------------------------------------------
!  Get common 3-D variables at pressure level
!-------------------------------------------------------------------------------

  subroutine get_p_level_variable (ix, jx, kx, iSTATE, iBASE, kpx, levels, &
                                   t_p, th_p, u_p, v_p, w_p, h_p, q_p, rh_p, qc_p, qi_p)

      use DEFINITION
      use WRF_TOOLS

      implicit none

!-------------------------------------------------------------------------------

      integer                     :: ix, jx, kx  ! [in]
      type (state_var)            :: iSTATE      ! [in]
      type (base_var)             :: iBASE       ! [in]
      integer                     :: kpx         ! [in]
      real, dimension(kpx)        :: levels      ! [in]

      real, dimension(ix,jx,kpx)  :: t_p, th_p, u_p, v_p, w_p, h_p, q_p, rh_p, qc_p, qi_p ! [out]

      real, dimension(ix,jx,kx  ) :: t, w, rh, h, uu, vv
      real, dimension(      kx  ) :: pres
      real, dimension(      kx+1) :: hf
      integer                     :: i, j, k, kp

!-------------------------------------------------------------------------------

      do j = 1, jx
      do i = 1, ix

        call eta_to_pres(iBASE%znw, &
                         iSTATE%mu(i,j) + iBASE%mub(i,j), &
                         iSTATE%qv(i,j,:), &
                         iSTATE%ph(i,j,:) + iBASE%phb(i,j,:), &
                         iSTATE%t(i,j,:) + To, &
                         kx, pres)
        do k = 1, kx
          t(i,j,k) = theta_to_temp(iSTATE%t(i,j,k) + To, pres(k))
          rh(i,j,k) = rel_humidity(iSTATE%qv(i,j,k), t(i,j,k), pres(k))
        enddo

        hf(:) = (iSTATE%ph(i,j,:) + iBASE%phb(i,j,:)) / g
        do k = 1, kx
          h(i,j,k) = 0.5 * (hf(k) + hf(k+1))
          w(i,j,k) = 0.5 * (iSTATE%w(i,j,k) + iSTATE%w(i,j,k+1))
        enddo

        uu(i,j,:) = 0.5 * (iSTATE%u(i,j,:) + iSTATE%u(i+1,j  ,:))
        vv(i,j,:) = 0.5 * (iSTATE%v(i,j,:) + iSTATE%v(i  ,j+1,:))

        do kp = 1, kpx
          t_p(i,j,kp)  = interp_pres(t(i,j,1:kx)        , pres, levels(kp), kx)
          th_p(i,j,kp) = interp_pres(iSTATE%t(i,j,1:kx) , pres, levels(kp), kx) + To
          u_p(i,j,kp)  = interp_pres(uu(i,j,1:kx)       , pres, levels(kp), kx)
          v_p(i,j,kp)  = interp_pres(vv(i,j,1:kx)       , pres, levels(kp), kx)
          w_p(i,j,kp)  = interp_pres(w(i,j,1:kx)        , pres, levels(kp), kx)
          h_p(i,j,kp)  = interp_pres(h(i,j,1:kx)        , pres, levels(kp), kx)
          q_p(i,j,kp)  = interp_pres(iSTATE%qv(i,j,1:kx), pres, levels(kp), kx)
          rh_p(i,j,kp) = interp_pres(rh(i,j,1:kx)       , pres, levels(kp), kx)
          qc_p(i,j,kp) = interp_pres(iSTATE%qc(i,j,1:kx), pres, levels(kp), kx)
          qi_p(i,j,kp) = interp_pres(iSTATE%qi(i,j,1:kx), pres, levels(kp), kx)

          if (t_p(i,j,kp)       .LT. 0.)       t_p(i,j,kp)  = missing
          if (th_p(i,j,kp)      .LT. 0.)       th_p(i,j,kp) = missing
          if (abs(u_p(i,j,kp))  .GT. 200.)     u_p(i,j,kp)  = missing
          if (abs(v_p(i,j,kp))  .GT. 200.)     v_p(i,j,kp)  = missing
          if (abs(w_p(i,j,kp))  .GT. 200.)     w_p(i,j,kp)  = missing
          if (h_p(i,j,kp)       .LT. -800000.) h_p(i,j,kp)  = missing
          if (q_p(i,j,kp)       .LT. -10.)     q_p(i,j,kp)  = missing
          if (rh_p(i,j,kp)      .LT. -10.)     rh_p(i,j,kp) = missing
          if (qc_p(i,j,kp)      .LT. -10.)     qc_p(i,j,kp) = missing
          if (qi_p(i,j,kp)      .LT. -10.)     qi_p(i,j,kp) = missing
        end do

      end do
      end do

  end subroutine get_p_level_variable

!-------------------------------------------------------------------------------
!  Get 3-D geopotential height at pressure level
!-------------------------------------------------------------------------------

  subroutine get_p_level_gph (ix, jx, kx, iSTATE, iBASE, kpx, levels, h_p)

      use DEFINITION
      use WRF_TOOLS

      implicit none

!-------------------------------------------------------------------------------

      integer                     :: ix, jx, kx  ! [in]
      type (state_var)            :: iSTATE      ! [in]
      type (base_var)             :: iBASE       ! [in]
      integer                     :: kpx         ! [in]
      real, dimension(kpx)        :: levels      ! [in]

      real, dimension(ix,jx,kpx)  :: h_p         ! [out]

      real, dimension(ix,jx,kx  ) :: h
      real, dimension(      kx  ) :: pres
      real, dimension(      kx+1) :: hf
      integer                     :: i, j, k, kp

!-------------------------------------------------------------------------------

      do j = 1, jx
      do i = 1, ix

        call eta_to_pres(iBASE%znw, &
                         iSTATE%mu(i,j) + iBASE%mub(i,j), &
                         iSTATE%qv(i,j,:), &
                         iSTATE%ph(i,j,:) + iBASE%phb(i,j,:), &
                         iSTATE%t(i,j,:) + To, &
                         kx, pres)

        hf(:) = (iSTATE%ph(i,j,:) + iBASE%phb(i,j,:)) / g
        do k = 1, kx
          h(i,j,k) = 0.5 * (hf(k) + hf(k+1))
        enddo

        do kp = 1, kpx
          h_p(i,j,kp) = interp_pres(h(i,j,1:kx), pres, levels(kp), kx)
          if (h_p(i,j,kp) .LT. -800000.) h_p(i,j,kp)  = missing
        end do

      end do
      end do

  end subroutine get_p_level_gph

!-------------------------------------------------------------------------------
!  Get 2-D u,v-wind at a given pressure level
!-------------------------------------------------------------------------------

  subroutine get_p_level_uv (ix, jx, kx, iSTATE, iBASE, level, u_p, v_p)

      use DEFINITION
      use WRF_TOOLS

      implicit none

!-------------------------------------------------------------------------------

      integer                   :: ix, jx, kx  ! [in]
      type (state_var)          :: iSTATE      ! [in]
      type (base_var)           :: iBASE       ! [in]
      real                      :: level       ! [in]

      real, dimension(ix,jx)    :: u_p, v_p    ! [out]

      real, dimension(ix,jx,kx) :: uu, vv
      real, dimension(      kx) :: pres
      integer                   :: i, j, k

!-------------------------------------------------------------------------------

      do j = 1, jx
      do i = 1, ix

        call eta_to_pres(iBASE%znw, &
                         iSTATE%mu(i,j) + iBASE%mub(i,j), &
                         iSTATE%qv(i,j,:), &
                         iSTATE%ph(i,j,:) + iBASE%phb(i,j,:), &
                         iSTATE%t(i,j,:) + To, &
                         kx, pres)

        uu(i,j,:) = 0.5 * (iSTATE%u(i,j,:) + iSTATE%u(i+1,j  ,:))
        vv(i,j,:) = 0.5 * (iSTATE%v(i,j,:) + iSTATE%v(i  ,j+1,:))
        u_p(i,j) = interp_pres(uu(i,j,1:kx), pres, level, kx)
        v_p(i,j) = interp_pres(vv(i,j,1:kx), pres, level, kx)

        if (abs(u_p(i,j)) .GT. 200.) u_p(i,j) = missing
        if (abs(v_p(i,j)) .GT. 200.) v_p(i,j) = missing

      end do
      end do

  end subroutine get_p_level_uv

!-------------------------------------------------------------------------------
!  Get common 3-D variables at eta level
!-------------------------------------------------------------------------------

  subroutine get_eta_level_variable (ix, jx, kx, iSTATE, iBASE, &
                                     t, th, w, p, h, q, rh)

      use DEFINITION
      use WRF_TOOLS

      implicit none

!-------------------------------------------------------------------------------

      integer                     :: ix, jx, kx  ! [in]
      type (state_var)            :: iSTATE      ! [in]
      type (base_var)             :: iBASE       ! [in]

      real, dimension(ix,jx,kx)   :: t, th, w, p, h, q, rh ! [out]

      real, dimension(      kx+1) :: hf
      integer                     :: i, j, k

!-------------------------------------------------------------------------------

      th(:,:,:) = iSTATE%t (:,:,:) + To
      q (:,:,:) = iSTATE%qv(:,:,:)

      do j = 1, jx
      do i = 1, ix

        call eta_to_pres(iBASE%znw, &
                         iSTATE%mu(i,j) + iBASE%mub(i,j), &
                         iSTATE%qv(i,j,:), &
                         iSTATE%ph(i,j,:) + iBASE%phb(i,j,:), &
                         iSTATE%t(i,j,:) + To, &
                         kx, p(i,j,:))

!      print*, iBASE%mub(i,j)
        do k = 1, kx
          t(i,j,k) = theta_to_temp(th(i,j,k), p(i,j,k))
          rh(i,j,k) = rel_humidity(q(i,j,k), t(i,j,k), p(i,j,k))
        enddo

        hf(:) = (iSTATE%ph(i,j,:) + iBASE%phb(i,j,:)) / g
        do k = 1, kx
          h(i,j,k) = 0.5 * (hf(k) + hf(k+1))
          w(i,j,k) = 0.5 * (iSTATE%w(i,j,k) + iSTATE%w(i,j,k+1))

          if (t(i,j,k)      .LT. 0.)       t(i,j,k)  = missing
          if (th(i,j,k)     .LT. 0.)       th(i,j,k) = missing
          if (abs(w(i,j,k)) .GT. 200.)     w(i,j,k)  = missing
          if (p(i,j,k)      .LT. -10.)     h(i,j,k)  = missing
          if (h(i,j,k)      .LT. -800000.) h(i,j,k)  = missing
          if (q(i,j,k)      .LT. -10.)     q(i,j,k)  = missing
          if (rh(i,j,k)     .LT. -10.)     rh(i,j,k) = missing
        end do

      end do
      end do

  end subroutine get_eta_level_variable

!-------------------------------------------------------------------------------
!  Get sea level pressure
!-------------------------------------------------------------------------------

  subroutine get_slp (ix, jx, kx, iSTATE, iBASE, slp)

      use DEFINITION
      use WRF_TOOLS

      implicit none

!-------------------------------------------------------------------------------

      integer,intent(in)                     :: ix, jx, kx  ! [in]
      type (state_var),intent(in)            :: iSTATE      ! [in]
      type (base_var),intent(in)             :: iBASE       ! [in]

      real, dimension(ix,jx),intent(out) :: slp         ! [out]

      real, dimension(ix,jx,kx+1) :: hf
      real, dimension(ix,jx,kx  ) :: h, uu, vv
      real, dimension(      kx  ) :: pres, temp
      real, dimension(kx) :: w2cpres
      integer                     :: i, j, k

!-------------------------------------------------------------------------------

        
      do j = 1, jx
      do i = 1, ix

        call eta_to_prescyl(iBASE%znw, &
                         iSTATE%mu(i,j) + iBASE%mub(i,j), &
                         iSTATE%qv(i,j,:), &
                         iSTATE%ph(i,j,:) + iBASE%phb(i,j,:), &
                         iSTATE%t(i,j,:) + To, &
                         kx, pres,i,j)
        hf(i,j,:) = (iSTATE%ph(i,j,:) + iBASE%phb(i,j,:)) / g
!        print*, i,j
        do k = 1, kx
          h(i,j,k) = 0.5 * (hf(i,j,k) + hf(i,j,k+1))
          temp(k) = theta_to_temp(iSTATE%t(i,j,k) + To, pres(k))
        enddo
        slp(i,j) = slp_ben(temp, pres(1), iSTATE%qv(i,j,:), h(i,j,:), kx)
        !        print*,  slp_ben(temp, pres(1), iSTATE%qv(i,j,:), h(i,j,:), kx)
!        print*,'i,j',i,j
!        print*,slp(i,j)
        if (slp(i,j) .LT. 0.) slp(i,j) = missing


      end do
      end do

  end subroutine get_slp

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module GET_VARIABLES

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

