!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!   module netcdf - module that contains several subroutines used to
!                   read and write netcdf data
!
!     created Oct. 2004 Ryan Torn, U. Washington
!     add some subroutines 07/2005 Zhiyong Meng TAMU
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



      MODULE NETCDF
                                                                                                                             
        include 'netcdf.inc'
                                                                                                                             
      CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!   open_file - subroutine that opens a netcdf file
!
!   filename - name of file to open
!    permiss - permissions of file to open
!        fid - integer file id
!
!     created June 2004 Ryan Torn, U. Washington
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine open_file(filename, permiss, fid)
                                                                                                                             
      character (len=*), intent(in) :: filename
      integer, intent(in)           :: permiss
      integer, intent(out)          :: fid
                                                                                                                             
      integer :: rcode
                                                                                                                             
      rcode = nf_open(filename, permiss, fid)
      if ( rcode .ne. 0 ) then
         call netcdf_error(filename, 'open', rcode, 0, 0, 0, 0)
      endif
                                                                                                                             
      return
      end subroutine
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!   netcdf_error - subroutine that prints an error and stops a program
!                  if a netcdf routine throws an error
!
!     place - program where the netcdf error happens
!       var - variable of error
!        rc - integer code of error
!        ix - dimension 1 of error
!        iy - dimension 2 of error
!        iz - dimension 3 of error
!        it - time dimension of error
!
!     created June 2004 Ryan Torn, U. Washington
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine netcdf_error(place, var, rc, ix, iy, iz, it)
                                                                                                                             
      character (len=*), intent(in) :: place, var
      integer, intent(in)           :: rc, ix, iy, iz, it
                                                                                                                             
      write(6,*) 'NETCDF ERROR !!!!!!!!'
      write(6,*)
      write(6,*) 'Error occurs at ',place
      write(6,*) 'With variable ',var
      write(6,*) 'Netcdf message ',nf_strerror(rc)
      write(6,*) 'ix iy iz it',ix,iy,iz,it
                                                                                                                             
!      open(25, file='enkf_error',status='unknown')
!      write(25,*) nf_strerror(rc); close(25)
                                                                                                                             
      stop
      end subroutine
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                                                                                             
  subroutine get_ijk_dim (wrf_file, ix, jx, kx, ox)
                                                                                                                             
      use WRF_TOOLS
      implicit none
                                                                                                                             
      character (len=*)  :: wrf_file   ! [in]
      integer            :: ix, jx, kx ,ox! [out]
      integer            :: fid
                                                                                                                             
      call open_file(wrf_file, nf_nowrite, fid)
      ix = get_dimlen(fid, 'west_east')
      jx = get_dimlen(fid, 'south_north')
      kx = get_dimlen(fid, 'bottom_top')
      ox = get_dimlen(fid, 'ocean_layer_stag')
      print*,'ox',ox
      call close_file(fid)
                                                                                                                             
  end subroutine get_ijk_dim
                                                                                                                             
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine get_variable3d(fid, var, i1, i2, i3, it, dat)
                                                                                                                             
      character (len=*), intent(in) :: var
      integer, intent(in)           :: fid, i1, i2, i3, it
      real, intent(out)             :: dat(i1,i2,i3)
                                                                                                                             
      integer :: istart(4), iend(4), varid, rcode
                                                                                                                             
      ! get the variable id
      rcode = nf_inq_varid(fid, var, varid)
                                                                                                                             
      ! set the dimension arrays for reading
      istart(1) = 1
      iend(1) = i1
      istart(2) = 1
      iend(2) = i2
      istart(3) = 1
      iend(3) = i3
      istart(4) = it
      iend(4) = 1
                                                                                                                             
      ! read the data
      rcode = nf_get_vara_real(fid, varid, istart, iend, dat)
                                                                                                                             
      if (rcode.ne.0) then
        call netcdf_error('get_variable3d', var, rcode, i1, i2, i3, it)
      endif
                                                                                                                             
      return
      end subroutine
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine get_variable2d_local(fid, var,
     &                           i1s, i1e, i2s, i2e, it, dat)
                                                                                                                             
      character (len=*), intent(in) :: var
      integer, intent(in)           :: fid, i1s, i1e, i2s, i2e, it
      real, intent(out)             :: dat(i1e-i1s+1,i2e-i2s+1)
                                                                                                                             
      integer :: istart(3), iend(3), varid, rcode
                                                                                                                             
      ! get the variable id
      rcode = nf_inq_varid(fid, var, varid)
                                                                                                                             
      ! set the dimension arrays for reading
      istart(1) = i1s
      iend(1) = i1e-i1s+1
      istart(2) = i2s
      iend(2) = i2e-i2s+1
      istart(3) = it
      iend(3) = 1
                                                                                                                             
      ! read the data
      rcode = nf_get_vara_real(fid, varid, istart, iend, dat)
                                                                                                                             
      if (rcode.ne.0) then
        print*,'i1s i2s', istart(1:2)
        call netcdf_error('get_variable2d_local', var, rcode,
     &                             iend(1), iend(2), 0, it)
      endif
                                                                                                                             
      return
      end subroutine
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!111
      end module      
