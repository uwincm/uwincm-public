!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  subroutine circular_ave         (ix, jx, missing, var,  ii_c,  jj_c,  r, args, var_ave)
!  subroutine circular_ave_uv      (ix, jx, missing, u, v, ii_c,  jj_c,  r, args, ut, ur)
!  subroutine circular_ave_stag_uv (ix, jx, missing, u, v, ii_c,  jj_c,  r, args, ut, ur)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module W2C_TOOLS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use WRF_TOOLS
  implicit none

  real, parameter      :: ave_dist = 100.   ! distance to do the weighting procedure (km)

  contains

!---------------------------------------------------------------------------------
!  Calculate the circular-averaging value
!---------------------------------------------------------------------------------

  subroutine w2c_var (ix, jx, missing, var, ii_c, jj_c, r, args, var_new)

      implicit none

      integer                :: ix, jx       ! [in]
      real                   :: missing      ! [in]
      real, dimension(ix,jx),intent(in) :: var          ! [in]
      real                   :: ii_c, jj_c   ! [in]
      real                   :: r            ! [in]
      integer                :: args         ! [in]
      real, dimension(args), intent(out)      :: var_new      ! [out]

!-------------------------------------------------------------------------------

      integer :: i_args,i_w2c
      real    :: ii, jj
      real    :: var_tot
      double precision    :: var_args
      integer :: io, jo,i
      real    :: dx, dxm, dy, dym
      logical :: is_missing
  real, parameter      :: pi = 3.1415926535
  real, parameter      :: two_pi = 2. * pi

!-------------------------------------------------------------------------------

     ! print*,'w2c',ix,jx
     ! write(170,*)var
      var_tot = 0.
      is_missing = .FALSE.
      do i_w2c = 1,args
      do i_args = i_w2c,i_w2c
      is_missing = .FALSE.
!-------------------------------------------------------------------------------

        ii = ii_c + r * cos(two_pi * real(i_args-1)/real(args-1))
        jj = jj_c + r * sin(two_pi * real(i_args-1)/real(args-1))
        if ((ii .LT. 1.) .OR. (ii .GT. ix) .OR. &
            (jj .LT. 1.) .OR. (jj .GT. jx)) then
          is_missing = .TRUE.
!          exit
        end if

        call toGrid(ii, 1, ix, io, dx, dxm)
     !   print*,'kaliix',ix,io,dx,dxm
        call toGrid(jj, 1, jx, jo, dy, dym)
     !   print*,'kaliiy',jx,jo,dy,dym

        if ((var(io,jo  ) .EQ. missing) .OR. (var(io+1,jo  ) .EQ. missing) .OR. &
            (var(io,jo+1) .EQ. missing) .OR. (var(io+1,jo+1) .EQ. missing)) then
          is_missing = .TRUE.
!          exit
        end if

        var_args = dym * (dxm * var(io,jo  ) + dx * var(io+1,jo  )) &
                 + dy  * (dxm * var(io,jo+1) + dx * var(io+1,jo+1))
!-------------------------------------------------------------------------------
      end do ! [ i_args ]

      if (.NOT. is_missing) then
        var_new(i_args) = var_args 
      else
        var_new(i_args) = missing
      end if

      end do ! [i_w2c= 1, args] 
!-------------------------------------------------------------------------------

  end subroutine w2c_var 

!---------------------------------------------------------------------------------
!  Calculate the circular-averaging value
!---------------------------------------------------------------------------------

  subroutine w2c_circular_ave (ix, jx, missing, var, ii_c, jj_c, r, args,var_args)

      implicit none

      integer                :: ix, jx       ! [in]
      real                   :: missing      ! [in]
      real, dimension(ix,jx) :: var          ! [in]
      real                   :: ii_c, jj_c   ! [in]
      real                   :: r            ! [in]
      integer                :: args         ! [in]
      real                   :: var_ave      ! [in]
      real, dimension(args) ,intent(out)      :: var_args

!-------------------------------------------------------------------------------

      integer :: i_args
      real    :: ii, jj
      real    :: var_tot
      integer :: io, jo
      real    :: dx, dxm, dy, dym
      logical :: is_missing
  real, parameter      :: pi = 3.1415926535
  real, parameter      :: two_pi = 2. * pi

!-------------------------------------------------------------------------------
      var_ave = 0.
      var_tot = 0.
      var_args(:)=0.
      is_missing = .FALSE.

      do i_args = 1, args
      is_missing = .FALSE.
!-------------------------------------------------------------------------------

        ii = ii_c + r * cos(two_pi * real(i_args-1)/real(args-1))
        jj = jj_c + r * sin(two_pi * real(i_args-1)/real(args-1))
        if ((ii .LT. 1.) .OR. (ii .GT. ix) .OR. &
            (jj .LT. 1.) .OR. (jj .GT. jx)) then
          is_missing = .TRUE.
      !    exit
        end if

      !cyl find nearest
        ii = nint(ii)
        jj = nint(jj)


        call toGrid(ii, 1, ix, io, dx, dxm)
        call toGrid(jj, 1, jx, jo, dy, dym)

        if ((var(io,jo  ) .EQ. missing) .OR. (var(io+1,jo  ) .EQ. missing) .OR. &
            (var(io,jo+1) .EQ. missing) .OR. (var(io+1,jo+1) .EQ. missing)) then
          is_missing = .TRUE.
       !   exit
        end if

       if (.NOT. is_missing) then
!         var_args(i_args)= dym * (dxm * var(io,jo  ) + dx * var(io+1,jo  )) &
!                         + dy  * (dxm * var(io,jo+1) + dx * var(io+1,jo+1))  
         var_args(i_args)=var(io,jo  );
       else
         var_args(i_args) = missing
       end if

!-------------------------------------------------------------------------------
      end do ! [ i_args = 1, args ]


!-------------------------------------------------------------------------------

  end subroutine w2c_circular_ave

!---------------------------------------------------------------------------------
!  Calculate the circular-averaging tangential and radial wind
!---------------------------------------------------------------------------------

  subroutine w2c_circular_ave_uv (ix, jx, missing, u, v, ii_c, jj_c, r, args, ut, ur,un,vn)

      implicit none

      integer                :: ix, jx       ! [in]
      real                   :: missing      ! [in]
      real, dimension(ix,jx) :: u, v         ! [in]
      real                   :: ii_c, jj_c   ! [in]
      real                   :: r            ! [in]
      integer                :: args         ! [in]
      real, intent(out), dimension(args)   :: ut,ur        ! [out]
      real, intent(out), dimension(args)   :: un,vn        ! [out]

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
      is_missing = .FALSE.
!-------------------------------------------------------------------------------

        ii = ii_c + r * cos(two_pi * real(i_args-1)/real(args-1))
        jj = jj_c + r * sin(two_pi * real(i_args-1)/real(args-1))
        if ((ii .LT. 1.) .OR. (ii .GT. ix) .OR. &
            (jj .LT. 1.) .OR. (jj .GT. jx)) then
          is_missing = .TRUE.
      !    exit
        end if

      ! cyl find nearest
        ii = nint(ii)
        jj = nint(jj)
        call toGrid(ii, 1, ix, io1, dx1, dxm1)
        call toGrid(jj, 1, jx, jo1, dy1, dym1)
        call toGrid(ii, 1, ix, io2, dx2, dxm2)
        call toGrid(jj, 1, jx, jo2, dy2, dym2)

!        u_args = dym1 * (dxm1 * u(io1,jo1  ) + dx1 * u(io1+1,jo1  )) &
!               + dy1  * (dxm1 * u(io1,jo1+1) + dx1 * u(io1+1,jo1+1))
!        v_args = dym2 * (dxm2 * v(io2,jo2  ) + dx2 * v(io2+1,jo2  )) &
!               + dy2  * (dxm2 * v(io2,jo2+1) + dx2 * v(io2+1,jo2+1))
        u_args = u(io1,jo1  );
        v_args = v(io2,jo2  );
   
        if ((abs(u_args) .GT. 300.) .OR. (abs(v_args) .GT. 300.)) then
          is_missing = .TRUE.
      !    exit
        end if
      if (.NOT. is_missing) then
        un(i_args) =  u_args
        vn(i_args) =  v_args
      else
        un(i_args) = missing
        vn(i_args)= missing
      end if
        tangent_unit_x = cos(two_pi * real(i_args-1)/real(args-1) + 0.5*pi) !! unit vector of tangential dir.
        tangent_unit_y = sin(two_pi * real(i_args-1)/real(args-1) + 0.5*pi) !!
        radial_unit_x  = cos(two_pi * real(i_args-1)/real(args-1))          !! unit vector of radial dir.
        radial_unit_y  = sin(two_pi * real(i_args-1)/real(args-1))          !!
        ut_args = u_args * tangent_unit_x + v_args * tangent_unit_y !! tangential wind speed
        ur_args = u_args * radial_unit_x  + v_args * radial_unit_y  !! radial wind speed

        ut(i_args) =  ut_args
        ur(i_args) =  ur_args
      if (.NOT. is_missing) then
        ut(i_args) =  ut_args
        ur(i_args) =  ur_args
      else
        ut(i_args) = missing
        ur(i_args)= missing
      end if

!-------------------------------------------------------------------------------
      end do ! [ i_args = 1, args ]


!-------------------------------------------------------------------------------

  end subroutine w2c_circular_ave_uv

!---------------------------------------------------------------------------------

!---------------------------------------------------------------------------------
!  Calculate the circular-averaging tangential and radial wind in staggered grids
!---------------------------------------------------------------------------------
                                                                                                       
  subroutine w2c_circular_ave_stag_uv (ix, jx, missing, u, v, ii_c, jj_c, r, args, ut, ur, un, vn)
                                                                                                       
      implicit none
                                                                                                       
      integer                                     :: ix, jx       ! [in]
      real                                        :: missing      ! [in]
      real, dimension(ix+1,jx  )                  :: u            ! [in]
      real, dimension(ix  ,jx+1)                  :: v            ! [in]
      real                                        :: ii_c, jj_c   ! [in]
      real                                        :: r            ! [in]
      integer                                     :: args         ! [in]
      real, dimension(args), intent(out)          :: ut,ur        ! [out]
      real, dimension(args), intent(out)          :: un,vn           ! [out]
                                                                                                       
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
      is_missing = .FALSE.
!-------------------------------------------------------------------------------
                                                                                                       
        ii = ii_c + r * cos(two_pi * real(i_args-1)/real(args-1))
        jj = jj_c + r * sin(two_pi * real(i_args-1)/real(args-1))
        if ((ii .LT. 1.) .OR. (ii .GT. ix) .OR. &
            (jj .LT. 1.) .OR. (jj .GT. jx)) then
          is_missing = .TRUE.
      !    exit
        end if
        
       !cyl find nearest
        ii = nint(ii)
        jj = nint(jj)
                                                                                               
        call toGrid(ii+0.5, 1, ix+1, io1, dx1, dxm1)
        call toGrid(jj,     1, jx,   jo1, dy1, dym1)
        call toGrid(ii,     1, ix,   io2, dx2, dxm2)
        call toGrid(jj+0.5, 1, jx+1, jo2, dy2, dym2)

        u_args = u(io1,jo1  )
        v_args = v(io2,jo2  )
                                                                                                       
!        u_args = dym1 * (dxm1 * u(io1,jo1  ) + dx1 * u(io1+1,jo1  )) &
!               + dy1  * (dxm1 * u(io1,jo1+1) + dx1 * u(io1+1,jo1+1))
!        v_args = dym2 * (dxm2 * v(io2,jo2  ) + dx2 * v(io2+1,jo2  )) &
!               + dy2  * (dxm2 * v(io2,jo2+1) + dx2 * v(io2+1,jo2+1))
        if ((abs(u_args) .GT. 300.) .OR. (abs(v_args) .GT. 300.)) then
          is_missing = .TRUE.
      !    exit
        end if
      if (.NOT. is_missing) then
        un(i_args) = u_args
        vn(i_args) = v_args 
      else
        un(i_args) = missing
        vn(i_args) = missing
      end if
                                                                                                       
        tangent_unit_x = cos(two_pi * real(i_args-1)/real(args-1) + 0.5*pi) !! unit vector of tangential dir.
        tangent_unit_y = sin(two_pi * real(i_args-1)/real(args-1) + 0.5*pi) !!
        radial_unit_x  = cos(two_pi * real(i_args-1)/real(args-1))          !! unit vector of radial dir.
        radial_unit_y  = sin(two_pi * real(i_args-1)/real(args-1))          !!
        ut_args = u_args * tangent_unit_x + v_args * tangent_unit_y !! tangential wind speed
        ur_args = u_args * radial_unit_x  + v_args * radial_unit_y  !! radial wind speed

      if (.NOT. is_missing) then
        ut(i_args) = ut_args
        ur(i_args) = ur_args 
      else
        ut(i_args) = missing
        ur(i_args) = missing
      end if
                                                                                                       
!-------------------------------------------------------------------------------
      end do ! [ i_args = 1, args ]
                                                                                                       
                                                                                                       
!-------------------------------------------------------------------------------
                                                                                                       
  end subroutine w2c_circular_ave_stag_uv
                                                                                                       

!-------------------------------------------------------------------------------
! caldbz is to calculate dbz from qrain, qvapor, qcloud, qice, qsnow
! this code works only for WSM5
! This code is origional from Dan stein from RSMAS/UM
! Modified by Chiaying Lee RSMAS/UM 2010 March
!-------------------------------------------------------------------------------
  subroutine  caldbz_ds(qv,qr,qc,qi,qs,t,p,dbz,ix,jx,kx,missing)
  implicit none

  !intercept parameters
  real, parameter :: nor=8.0e+6, nos=2.0e+6, nog=4.0e+6
  !partical density
  real ::rhow=1000.0,rhos=100.0,rhog=500.0
  !other consitant
  real, parameter ::rd=287.0,eps=0.622,qmin=1.0e-10,kapw=1.0,kapi=0.224
  real :: kap,xnos,dens
  
  integer, intent(in)::ix,jx,kx
  real,dimension(ix,jx,kx),intent(in)::qv,qr,qc,qi,qs,t,p
  real,dimension(ix,jx,kx),intent(out)::dbz
  real :: missing,lamr,lams,zdbz
  integer :: i,j,k

  real, parameter      :: pi = 3.1415926535
  real, parameter      :: two_pi = 2. * pi
  print*,ix,jx,kx,missing 
  do k = 1, kx
    do j = 1, jx
      do i = 1, ix
       dbz(i,j,k)=missing
       if (t(i,j,k) .ne. missing) then
        if (t(i,j,k) < 273.15) then
         kap=kapi
        else
         kap=kapw
        endif
!         zdbz=((rhow*qr(i,j,k))**1.75)*3.630803e-9*1.0e18
!         zdbz=zdbz+((rhos*qs(i,j,k))**1.75)*2.185e-10*1.0e18
!         dbz(i,j,k)=10*log10(zdbz)
 
        dens=p(i,j,k)/(rd*t(i,j,k))       
        lamr=(pi*rhow*nor/dens/qr(i,j,k))**0.25
        lams=(pi*rhos*nos/dens/qs(i,j,k))**0.25
  
        zdbz=720.0e+18*kapw*(nor/(lamr**7.0))
        xnos=nos*exp(0.12*(273.15-t(i,j,k)))
        if (xnos > 1.0e+11 ) xnos=1.0e+11
        zdbz=zdbz+720.0e+18*kap*(xnos/(lams**7.0))*((rhos/rhow)**2.0)
        zdbz=max(zdbz,0.000001)
        dbz(i,j,k)=10.0*log10(zdbz)
      endif
       ! print*,'dbz',dbz(i,j,k)
      enddo
    enddo
  enddo 





  end subroutine caldbz_ds

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!-------------------------------------------------------------------------------
! caldbz is to calculate dbz from qrain, qvapor, qcloud, qice, qsnow
! this code works only for WSM5
! This code is origional from ARWpost
! Modified by Chiaying Lee RSMAS/UM 2010 March
!-------------------------------------------------------------------------------
  subroutine  caldbz(qv,qr,qc,qi,qs,t,p,dbz,ix,jx,kx,missing)
  implicit none
  
  real                                                           :: temp_c
  real                                                           :: gonv, ronv, sonv
  real                                                           :: factor_g, factor_r, factor_s
  real                                                           :: factorb_g, factorb_r, factorb_s
  real                                                           :: rhoair, z_e
  integer, intent(in)::ix,jx,kx
  real,dimension(ix,jx,kx),intent(in)::qv,qr,qc,qi,qs,t,p
  real,dimension(ix,jx,kx),intent(out)::dbz

  !   Constants used to calculate variable intercepts
  real, parameter                                                :: r1 = 1.e-15
  real, parameter                                                :: ron = 8.e6
  real, parameter                                                :: ron2 = 1.e10
  real, parameter                                                :: son = 2.e7
  real, parameter                                                :: gon = 5.e7
  real, parameter                                                :: ron_min = 8.e6
  real, parameter                                                :: ron_qr0 = 0.00010
  real, parameter                                                :: ron_delqr0 = 0.25*ron_qr0
  real, parameter                                                :: ron_const1r = (ron2-ron_min)*0.5
  real, parameter                                                :: ron_const2r = (ron2+ron_min)*0.5
!   Other constants
  real, parameter                                                :: gamma_seven = 720.
  real, parameter                                                :: rho_r = 1000.           ! 1000. kg m^-3
  real, parameter                                                :: rho_s = 100.             ! kg m^-3
  real, parameter                                                :: rho_g = 400.             ! kg m^-3
  real, parameter                                                :: alpha = 0.224
! other
  integer                                                        :: i, j, k
  real, parameter :: PI = 3.141592653589793,RD=287.0 
  real            :: missing
 ! real, parameter      :: pi = 3.1415926535
  real, parameter      :: two_pi = 2. * pi
  
  factor_r = gamma_seven * 1.e18 * (1./(PI*rho_r))**1.75
  factor_s = gamma_seven * 1.e18 * (1./(PI*rho_s))**1.75   &
                    * (rho_s/rho_r)**2 * alpha
  factor_g = gamma_seven * 1.e18 * (1./(PI*rho_g))**1.75   &
                    * (rho_g/rho_r)**2 * alpha
                                                                                                                  
  do k = 1, kx
    do j = 1, jx
      do i = 1, ix
       dbz(i,j,k)=missing
       ronv=0.0
       gonv=0.0
       sonv=0.0
       if (t(i,j,k) .ne. missing) then
          rhoair=p(i,j,k)/ (Rd*virtual(t(i,j,k),qv(i,j,k)) )  ! air density
!      Adjust factor for brightband, where snow or graupel particle
!      scatters like liquid water (alpha=1.0) because it is assumed to
!      have a liquid skin.

           IF (t(i,j,k) .gt. 273.15) THEN
              factorb_s=factor_s/alpha
              factorb_g=factor_g/alpha
            ELSE
              factorb_s=factor_s
              factorb_g=factor_g
            ENDIF
!      Calculate variable intercept parameters
                                                                                                                  
            temp_c = amin1(-0.001, t(i,j,k)-273.15)
            sonv   = amin1(2.0e8, 2.0e6*exp(-0.12*temp_c))
                                                                                                                  
            gonv = gon
!            IF (qg(i,j,k).gt.r1 .and. qg .ne. missing ) THEN
!              gonv = 2.38*(PI*rho_g/(rhoair*qg(i,j,k)))**0.92
!              gonv = max(1.e4, min(gonv,gon))
!            ENDIF
                                                                                                                  
            ronv = ron2
            IF (qr(i,j,k).gt. r1 .and. qr(i,j,k) .ne. missing) THEN
               ronv = ron_const1r*tanh((ron_qr0-qr(i,j,k))     &
                      /ron_delqr0) + ron_const2r
            ENDIF
                                                                                                                              
!      Total equivalent reflectivity factor (z_e, in mm^6 m^-3) is
!      the sum of z_e for each hydrometeor species:
                                                                                                                  
            z_e =   factor_r  * (rhoair*qr(i,j,k))**1.75 /ronv**.75   &
                  + factorb_s * (rhoair*qs(i,j,k))**1.75 /sonv**.75   
!                  + factorb_g * (rhoair*qg(i,j,k))**1.75 /gonv**.75
                                                                                                                  
!      Adjust small values of Z_e so that dBZ is no lower than -30
            z_e = max(z_e,.001)
                                                                                                                  
!      Convert to dBZ
            dbz(i,j,k)=10. * log10(z_e)

       endif
      enddo
    enddo 
  enddo
  end subroutine caldbz

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   FUNCTION virtual (tmp,rmix)
!      This function returns virtual temperature in K, given temperature
!      in K and mixing ratio in kg/kg.
                                                                                                                  
     real                              :: tmp, rmix, virtual
                                                                                                                  
     virtual=tmp*(0.622+rmix)/(0.622*(1.+rmix))
                                                                                                                  
   END FUNCTION virtual


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module W2C_TOOLS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
