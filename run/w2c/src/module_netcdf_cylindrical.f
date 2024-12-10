!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!   module netcdf_cylindrical - module that contains several subroutines 
!                   used to creat cylindrical netcdf file
!
!     created March. 2010 Chiaying Lee, RSMAS/UM
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      MODULE NETCDF_cylindrical 

      include 'netcdf.inc'
      CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!    creat NetCDF file for writting
!    created March 2009  Chiaying Lee RSMAS/UM 
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine create_nffile(nffilename,radius,theta,z,ocean,ncid)
      implicit none
      character(len=*), intent(in)::nffilename
      integer,  intent(out)        ::ncid
      real, intent(in) ::radius(:),theta(:),z(:),ocean(:) 
      integer                      ::rcode
      integer                      ::dimid_time,dimid_strlen
      integer                      ::dimid_radius                      
      integer                      ::dimid_theta,dimid_z,dimid_zstag  
      integer                      ::dimid_ocean  
      integer                      ::varid_time,varid_strlen,varid 

      !creat NetCDF file 
      rcode=nf_create(nffilename,nf_clobber,ncid)
      !add dimension
      rcode=nf_def_dim(ncid,'Time',nf_unlimited,dimid_time)
      if (rcode .ne. nf_noerr) call handle_err(rcode)
      rcode=nf_def_dim(ncid,'DataStrLen',19,dimid_strlen)
      rcode=nf_def_dim(ncid,'bottom_top',size(z),dimid_z)
      rcode=nf_def_dim(ncid,'bottom_top_stag',size(z)+1,dimid_zstag)
      rcode=nf_def_dim(ncid,'radius',size(radius),dimid_radius)
      rcode=nf_def_dim(ncid,'theta',size(theta),dimid_theta)
      rcode=nf_def_dim(ncid,'ocean_depth',size(ocean),dimid_ocean)
      print*,(/dimid_time,dimid_strlen,dimid_z,dimid_radius,
     &       dimid_theta,dimid_zstag/)
      rcode=nf_put_att_text(ncid,nf_global,'Title', 21 
     &                           ,'Cylindrical grid file')
      rcode=nf_put_att_text(ncid,nf_global,'Gridtype', 16
     &                           ,'Cylindrical grid') 
     
      rcode=nf_close(ncid)
      return
      end subroutine create_nffile

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       subroutine cg_add_1d_var(cgfilename,dimname,varname,units, 
     &                description,data)
      character(len=*),intent(in)::cgfilename
      character(len=*),intent(in)::dimname,varname,units,description
      real,dimension(:)::data
      integer::rcode
      integer::ncid
      integer::dimid
      integer::varid
      integer::length 
!!!! Open in "Define" mode
      rcode=nf_open(cgfilename,nf_write,ncid)
      print*,'rocde',rcode,ncid
      rcode=nf_redef(ncid)
      ! rcode=nf_inq_dimid(ncid,'radius',dimid)
      ! rcode = nf_def_var(ncid,varname, NF_double, 1, (/dimids/), varid)
      rcode=nf_inq_dimid(ncid,dimname,dimid)
      rcode = nf_def_var(ncid,varname, NF_double, 1, (/dimid/), varid)
!print*,'VARDEF:',rcode
      length=len_trim(units)
      rcode=nf_put_att_text(ncid, varid, 'units',length, units)
      length=len_trim(description) 
      rcode=nf_put_att_text(ncid,varid,'description', 
     & length, description)
!print*,'Att:',rcode
!!!! Change to "Writing" mode
      rcode=nf_enddef(ncid)
      rcode=nf_put_var_real(ncid, varid, data)
cg_add_1d_var=rcode
      rcode=nf_close(ncid)
      end subroutine cg_add_1d_var

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine cg_add_2d_var(cgfilename,varname,units, 
     &       description,data)

      character(len=*),intent(in)::cgfilename
      character(len=*),intent(in)::varname,units,description
      real,dimension(:,:)::data
      integer::rcode
      integer::ncid
      integer::dimid_radius,dimid_theta
      integer::varid,length
      
      !!!! Open in "Define" mode

      rcode=nf_open(cgfilename,nf_Write,ncid)
      rcode=nf_redef(ncid)

      rcode=nf_inq_dimid(ncid,'radius',dimid_radius)
      rcode=nf_inq_dimid(ncid,'theta',dimid_theta)
      rcode = nf_def_var(ncid,varname, NF_double, 2,  
     &(/ dimid_radius,dimid_theta /), varid)
      print*,'2d',varid, description 

      length=len_trim(units)
      rcode=nf_put_att_text(ncid, varid, 'units', length ,units)
      length=len_trim(description)
      rcode=nf_put_att_text(ncid, varid, 'description', 
     & length ,description)

      !!!! Change to "Writing" mode

      rcode=nf_enddef(ncid)

      rcode=nf_put_var_real(ncid, varid, data)
!      cg_add_2d_var=rcode

      rcode=nf_close(ncid)



      end subroutine cg_add_2d_var


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      integer function cg_add_3d_var(cgfilename,varname, 
     &    units,description,data)

      character(len=*),intent(in)::cgfilename
      character(len=*)::varname,units,description
      real,dimension(:,:,:)::data
      integer::rcode
      integer::ncid
      integer::dimid_radius,dimid_theta,dimid_bt
      integer::varid,length


!!!! Open in "Define" mode

      rcode=nf_open(cgfilename,nf_Write,ncid)
      rcode=nf_redef(ncid)

      rcode=nf_inq_dimid(ncid,'radius',dimid_radius)
      rcode=nf_inq_dimid(ncid,'theta',dimid_theta)
      rcode=nf_inq_dimid(ncid,'bottom_top',dimid_bt)
      rcode=nf_def_var(ncid, varname, nf_double,3,  
     &       (/ dimid_radius,dimid_theta,dimid_bt /) , varid )
      print*,'3d',varname,varid 
      if (rcode .ne. nf_noerr) call handle_err(rcode)
      length=len_trim(units)
      rcode=nf_put_att_text(ncid, varid, 'units',length, units)
      length=len_trim(description)
      rcode=nf_put_att_text(ncid, varid, 'description', 
     &  length, description)
!print*,'Att:',rcode

!!!! Change to "Writing" mode

      rcode=nf_enddef(ncid)

      rcode=nf_put_var_real(ncid, varid, data)
      cg_add_3d_var=rcode

      rcode=nf_close(ncid)

      end function cg_add_3d_var

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      integer function cg_add_3d_var_zstag(cgfilename,varname, 
     &    units,description,data)

      character(len=*),intent(in)::cgfilename
      character(len=*)::varname,units,description
      real,dimension(:,:,:)::data
      integer::rcode
      integer::ncid
      integer::dimid_radius,dimid_theta,dimid_bt
      integer::varid,length


!!!! Open in "Define" mode

      rcode=nf_open(cgfilename,nf_Write,ncid)
      rcode=nf_redef(ncid)

      rcode=nf_inq_dimid(ncid,'radius',dimid_radius)
      rcode=nf_inq_dimid(ncid,'theta',dimid_theta)
      rcode=nf_inq_dimid(ncid,'bottom_top_stag',dimid_bt)
      rcode=nf_def_var(ncid, varname, nf_double,3,  
     &       (/ dimid_radius,dimid_theta,dimid_bt /) , varid )
      print*,'3d',varname,varid 
      if (rcode .ne. nf_noerr) call handle_err(rcode)
      length=len_trim(units)
      rcode=nf_put_att_text(ncid, varid, 'units',length, units)
      length=len_trim(description)
      rcode=nf_put_att_text(ncid, varid, 'description', 
     &  length, description)
!print*,'Att:',rcode

!!!! Change to "Writing" mode

      rcode=nf_enddef(ncid)

      rcode=nf_put_var_real(ncid, varid, data)
      cg_add_3d_var_zstag=rcode

      rcode=nf_close(ncid)

      end function cg_add_3d_var_zstag

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      integer function cg_add_3d_oceanvar(cgfilename,varname, 
     &    units,description,data)

      character(len=*),intent(in)::cgfilename
      character(len=*)::varname,units,description
      real,dimension(:,:,:)::data
      integer::rcode
      integer::ncid
      integer::dimid_radius,dimid_theta,dimid_bt
      integer::varid,length


!!!! Open in "Define" mode

      rcode=nf_open(cgfilename,nf_Write,ncid)
      rcode=nf_redef(ncid)

      rcode=nf_inq_dimid(ncid,'radius',dimid_radius)
      rcode=nf_inq_dimid(ncid,'theta',dimid_theta)
      rcode=nf_inq_dimid(ncid,'ocean_depth',dimid_bt)
      rcode=nf_def_var(ncid, varname, nf_double,3,  
     &       (/ dimid_radius,dimid_theta,dimid_bt /) , varid )
      print*,'ocean',varid 
      if (rcode .ne. nf_noerr) call handle_err(rcode)
      length=len_trim(units)
      rcode=nf_put_att_text(ncid, varid, 'units',length, units)
      length=len_trim(description)
      rcode=nf_put_att_text(ncid, varid, 'description', 
     &  length, description)
!print*,'Att:',rcode

!!!! Change to "Writing" mode

      rcode=nf_enddef(ncid)

      rcode=nf_put_var_real(ncid, varid, data)
      cg_add_3d_oceanvar=rcode

      rcode=nf_close(ncid)

      end function cg_add_3d_oceanvar

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine cg_replace_1d_var(cgfilename,varname,data)

            character(len=*),intent(in)::cgfilename
            character(len=*),intent(in)::varname
            real,dimension(:)::data
            integer::rcode
            integer::ncid
            integer::dimid_radius,dimid_theta
            integer::varid,length
            
            !!!! Open in "Define" mode
      
            rcode=nf_open(cgfilename,nf_Write,ncid)
            if (rcode .ne. nf_noerr) call handle_err(rcode)
            print*,'cgfilename',cgfilename,ncid
            !!!! Change to "Writing" mode
            rcode = nf_inq_varid ( ncid, varname, varid )
            if (rcode .ne. nf_noerr) call handle_err(rcode)
            print*,'replace 1d',varname,varid,rcode
            rcode=nf_put_var_real(ncid, varid, data)
      
            rcode=nf_close(ncid)

      end subroutine cg_replace_1d_var      

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine cg_replace_2d_var(cgfilename,varname,data)

      character(len=*),intent(in)::cgfilename
      character(len=*),intent(in)::varname
      real,dimension(:,:)::data
      integer::rcode
      integer::ncid
      integer::dimid_radius,dimid_theta
      integer::varid,length
      
      !!!! Open in "Define" mode

      rcode=nf_open(cgfilename,nf_Write,ncid)
      if (rcode .ne. nf_noerr) call handle_err(rcode)
      print*,'cgfilename',cgfilename,ncid
      !!!! Change to "Writing" mode
      rcode = nf_inq_varid ( ncid, varname, varid )
      if (rcode .ne. nf_noerr) call handle_err(rcode)
      print*,'replace 2d',varname,varid,rcode
      rcode=nf_put_var_real(ncid, varid, data)

      rcode=nf_close(ncid)
      end subroutine cg_replace_2d_var

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine cg_replace_3d_var(cgfilename,varname,data)

      character(len=*),intent(in)::cgfilename
      character(len=*),intent(in)::varname
      real,dimension(:,:,:)::data
      integer::rcode
      integer::ncid
      integer::dimid_radius,dimid_theta
      integer::varid,length
      
      !!!! Open in "Define" mode

      rcode=nf_open(cgfilename,nf_Write,ncid)

      !!!! Change to "Writing" mode
      rcode = nf_inq_varid ( ncid, varname, varid )
      print*,'replace 3d',varname,varid,rcode
      rcode=nf_put_var_real(ncid, varid, data)
        
      rcode=nf_close(ncid)
      end subroutine cg_replace_3d_var

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine handle_err(errcode)
      implicit none
      include 'netcdf.inc'
      integer errcode

      print *, 'Error: ', nf_strerror(errcode)
      stop 2
      end subroutine handle_err

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      END MODULE NETCDF_cylindrical
