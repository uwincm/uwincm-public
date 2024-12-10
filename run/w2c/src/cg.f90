!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!    Combine cg file from d01, d02, d03 ... cg.nc
!    created  2010 March  Chiaying Lee, RSMAS/UM
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!-------------------------------------------------------------------------------

      program CG 

      use CONSTANTS
      use DEFINITION
      use WRF_TOOLS
      use WRF_IO
      use GET_VARIABLES
      use VORTEX_TOOLS
      use W2C_TOOLS
      use NETCDF_cylindrical 
      use CG_TOOLS
      implicit none

!-------------------------------------------------------------------------------

      integer, parameter :: args_w2c          = 361
      integer              :: ix, jx, kx, kpx, tx, rx, ox
      integer              :: i, j, k, kp, t, r, k_kp
      integer              :: ci1, ci2, cj1, cj2, cix, cjx
! need new type
      type (cg_var)     :: file1,file2,file3
      type (ocean_var)     :: ocean1,ocean2,ocean3
!
!-------------------------------------------------------------------------------

      write (*, *)
      write (*, *) 
      write (*, *)

      call read_namelist

!-------------------------------------------------------------------------------
!  Get dimension and projection
!-------------------------------------------------------------------------------

      call get_cg_ijk_dim (trim(in_file1), ix, jx, kx, ox)

      if (exist_file3 == 1) then
      call cg_allocate (ix, jx, kx, file3)
      call get_cg_var (trim(in_file3), ix, jx, kx, ox, file3)
      print*,'file3',ix,jx,kx,ox
      if (ocean_model) then
       call ocean_allocate (ix, jx, ox, ocean3)
       call  get_cgocean_var (trim(in_file3), ix, jx, ox, ox, ocean3)
      endif
      endif
      if (exist_file2 == 1) then
      call cg_allocate (ix, jx, kx, file2)
      call get_cg_var (trim(in_file2), ix, jx, kx, ox, file2)
      if (ocean_model) then
       call ocean_allocate (ix, jx, ox, ocean2)
       call  get_cgocean_var (trim(in_file2), ix, jx, ox, ox, ocean2)
      endif
      endif
      call cg_allocate (ix, jx, kx, file1)
      call get_cg_var (trim(in_file1), ix, jx, kx, ox, file1)
      if (ocean_model) then
       call ocean_allocate (ix, jx, ox, ocean1)
       call  get_cgocean_var (trim(in_file1), ix, jx, ox, ox, ocean1)
      endif
      if (exist_file3 == 1 .and. exist_file2 == 1) then 
      call cg_replace(file3,file2,ix,jx,kx)
      if (ocean_model) then
      call cgocean_replace(ocean3,ocean2,ix,jx,ox)
      endif
      endif
      if (exist_file2 == 1) then
      call cg_replace(file2,file1,ix,jx,kx)
      if (ocean_model) then
      call cgocean_replace(ocean2,ocean1,ix,jx,ox)
      endif
      endif     

      ! write out the final data to d01
      call cg_replace_1d_var(in_file1,'storm_center_lon',file1%storm_center_lon)
      call cg_replace_1d_var(in_file1,'storm_center_lat',file1%storm_center_lat)

      call cg_replace_2d_var(in_file1,'slp',file1%slp)
      call cg_replace_2d_var(in_file1,'precipitation', & 
                       file1%precipitation)
      call cg_replace_2d_var(in_file1,'q2',file1%q2)
      call cg_replace_2d_var(in_file1,'rh2',file1%rh2)       
      call cg_replace_2d_var(in_file1,'t2',file1%t2)
      call cg_replace_2d_var(in_file1,'th2',file1%th2)
      call cg_replace_2d_var(in_file1,'shflux',file1%shflux)
      call cg_replace_2d_var(in_file1,'lhflux',file1%lhflux)
      call cg_replace_2d_var(in_file1,'tsk',file1%tsk)
      call cg_replace_2d_var(in_file1,'un10',file1%un10)
      call cg_replace_2d_var(in_file1,'vn10',file1%vn10)
      call cg_replace_2d_var(in_file1,'vt10',file1%vt10)
      call cg_replace_2d_var(in_file1,'vr10',file1%vr10)
      call cg_replace_2d_var(in_file1,'mu',file1%mu)


      call cg_replace_3d_var(in_file1,'t',file1%t)
      call cg_replace_3d_var(in_file1,'th',file1%th)
      call cg_replace_3d_var(in_file1,'w',file1%w)
      call cg_replace_3d_var(in_file1,'pressure',file1%pressure)
      call cg_replace_3d_var(in_file1,'perturbation_ph',file1%ph)
      call cg_replace_3d_var(in_file1,'perturbation_p',file1%p)
      call cg_replace_3d_var(in_file1,'q',file1%q)
      call cg_replace_3d_var(in_file1,'rh',file1%rh)
      call cg_replace_3d_var(in_file1,'qc',file1%qc)
      call cg_replace_3d_var(in_file1,'qi',file1%qi)
      call cg_replace_3d_var(in_file1,'vt',file1%vt)
      call cg_replace_3d_var(in_file1,'vr',file1%vr)
      call cg_replace_3d_var(in_file1,'un',file1%un)
      call cg_replace_3d_var(in_file1,'vn',file1%vn)
      call cg_replace_3d_var(in_file1,'dbz',file1%dbz)
      call cg_replace_3d_var(in_file1,'xlong',file1%xlong)
      call cg_replace_3d_var(in_file1,'xlat',file1%xlat)
      call cg_replace_3d_var(in_file1,'tracer1',file1%tracer1)
      call cg_replace_3d_var(in_file1,'tracer2',file1%tracer2)
      call cg_replace_3d_var(in_file1,'tracer3',file1%tracer3)
      call cg_replace_3d_var(in_file1,'tracer4',file1%tracer4)

      if (ocean_model) then
      call cg_replace_3d_var(in_file1,'om_tmp',ocean1%om_tmp)
      call cg_replace_3d_var(in_file1,'om_u',ocean1%om_u)
      call cg_replace_3d_var(in_file1,'om_v',ocean1%om_v)
      call cg_replace_3d_var(in_file1,'om_s',ocean1%om_s)
      call cg_replace_2d_var(in_file1,'om_ml',ocean1%om_ml)

      endif
!-------------------------------------------------------------------------------

      end program CG 

!-------------------------------------------------------------------------------
