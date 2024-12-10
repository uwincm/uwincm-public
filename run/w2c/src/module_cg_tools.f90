!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!   module cg_tools -  module containing several subroutines and 
!                      functions to calculate and combine cg files
!
!     created March 2010 Chiaying Lee, RSMAS/UM
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      MODULE CG_TOOLS
      use CONSTANTS
      use DEFINITION

      CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine cg_replace(fine_file,coarse_file,ix,jx,kx)

      implicit none
      type(cg_var)::fine_file,coarse_file
      integer,intent(in) :: ix,jx,kx
      integer :: i,j,k

      do k = 1, kx
        coarse_file%storm_center_lon(k)=fine_file%storm_center_lon(k) 
        coarse_file%storm_center_lat(k)=fine_file%storm_center_lat(k) 
      end do
       do k = 1, kx
        do j = 1, jx
         do i = 1, ix
          if ( fine_file%t(i,j,k) .ne.  missing ) then
            coarse_file%t(i,j,k)=fine_file%t(i,j,k) 
            coarse_file%th(i,j,k)=fine_file%th(i,j,k) 
            coarse_file%w(i,j,k)=fine_file%w(i,j,k) 
            coarse_file%pressure(i,j,k)=fine_file%pressure(i,j,k) 
            coarse_file%p(i,j,k)=fine_file%p(i,j,k) 
            coarse_file%h(i,j,k)=fine_file%h(i,j,k) 
            coarse_file%q(i,j,k)=fine_file%q(i,j,k) 
            coarse_file%rh(i,j,k)=fine_file%rh(i,j,k) 
            coarse_file%qc(i,j,k)=fine_file%qc(i,j,k) 
            coarse_file%qi(i,j,k)=fine_file%qi(i,j,k) 
            coarse_file%vt(i,j,k)=fine_file%vt(i,j,k) 
            coarse_file%vr(i,j,k)=fine_file%vr(i,j,k) 
            coarse_file%un(i,j,k)=fine_file%un(i,j,k) 
            coarse_file%vn(i,j,k)=fine_file%vn(i,j,k) 
            coarse_file%dbz(i,j,k)=fine_file%dbz(i,j,k) 
            coarse_file%xlong(i,j,k)=fine_file%xlong(i,j,k) 
            coarse_file%xlat(i,j,k)=fine_file%xlat(i,j,k) 
            coarse_file%tracer1(i,j,k)=fine_file%tracer1(i,j,k) 
            coarse_file%tracer2(i,j,k)=fine_file%tracer2(i,j,k) 
            coarse_file%tracer3(i,j,k)=fine_file%tracer3(i,j,k) 
            coarse_file%tracer4(i,j,k)=fine_file%tracer4(i,j,k) 
          endif
         enddo
        enddo
       enddo
       do k = 1, kx+1
        do j = 1, jx
         do i = 1, ix
          if ( fine_file%ph(i,j,k) .ne.  missing ) then
             coarse_file%ph(i,j,k)=fine_file%ph(i,j,k)
          endif
         enddo
        enddo
       enddo

        do j = 1, jx
         do i = 1, ix
          if ( fine_file%slp(i,j) .ne.  missing ) then
            coarse_file%slp(i,j)=fine_file%slp(i,j)  
            coarse_file%precipitation(i,j)=fine_file%precipitation(i,j) 
            coarse_file%q2(i,j)=fine_file%q2(i,j)  
            coarse_file%rh2(i,j)=fine_file%rh2(i,j)
            coarse_file%t2(i,j)=fine_file%t2(i,j)  
            coarse_file%th2(i,j)=fine_file%th2(i,j)  
            coarse_file%tsk(i,j)=fine_file%tsk(i,j) 
            coarse_file%un10(i,j)=fine_file%un10(i,j) 
            coarse_file%vn10(i,j)=fine_file%vn10(i,j) 
            coarse_file%vt10(i,j)=fine_file%vt10(i,j) 
            coarse_file%vr10(i,j)=fine_file%vr10(i,j) 
            coarse_file%mu(i,j)=fine_file%mu(i,j) 
          endif
          if ( fine_file%shflux(i,j).ne.  missing  ) then
            coarse_file%shflux(i,j)=fine_file%shflux(i,j)
            coarse_file%lhflux(i,j)=fine_file%lhflux(i,j)
          endif
         enddo
        enddo

       
      end subroutine cg_replace
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine cgocean_replace(fine_file,coarse_file,ix,jx,kx)

      implicit none
      type(ocean_var)::fine_file,coarse_file
      integer,intent(in) :: ix,jx,kx
      integer :: i,j,k
       do k = 1, kx
        do j = 1, jx
         do i = 1, ix
          if ( fine_file%om_tmp(i,j,k) .ne.  missing ) then
            coarse_file%om_tmp(i,j,k)=fine_file%om_tmp(i,j,k) 
            coarse_file%om_u(i,j,k)=fine_file%om_u(i,j,k) 
            coarse_file%om_v(i,j,k)=fine_file%om_v(i,j,k) 
            coarse_file%om_s(i,j,k)=fine_file%om_s(i,j,k) 
          endif
         enddo
        enddo
       enddo

        do j = 1, jx
         do i = 1, ix
          if ( fine_file%om_ml(i,j) .ne.  missing ) then
            coarse_file%om_ml(i,j)=fine_file%om_ml(i,j)  
          endif
         enddo
        enddo

      end subroutine cgocean_replace
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       
      END MODULE CG_TOOLS
