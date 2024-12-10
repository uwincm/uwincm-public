      program vortex_relocation


      use CONSTANTS
      use DEFINITION
      use WRF_TOOLS
      use WRF_IO
      use GET_VARIABLES
      use VORTEX_TOOLS
      use W2C_TOOLS
      use NETCDF
      implicit none


      integer, parameter :: args_w2c          = 361
      integer              :: ix, jx, kx, kpx, tx, rx, ox
      integer              :: cix, cjx, ckx, cox
      integer              :: i, j, k, kp, t,  k_kp
      integer              :: ci1, ci2, cj1, cj2
      real, allocatable    :: RR(:) 
      integer              :: r, r_r1, r_r2  
      integer              :: ncid,ii,jj
      integer              :: varid
      real                 :: ii_fg,jj_fg
      real                 :: invr 
      real                 ::datatest
      real, allocatable    ::newu(:,:,:), newv(:,:,:),newp(:,:,:),newph(:,:,:),newqv(:,:,:),newt(:,:,:)
      real, allocatable    ::newslp(:,:),newmu(:,:), newun10(:,:), newvn10(:,:)
      integer              ::rcode
      integer, dimension(4):: istart, iend
      integer, dimension(3):: istart_2d, iend_2d
      real, allocatable    :: datatest_array(:,:,:)
      type (cg_var)        :: cgfile1,cgfile2
      type (cgstate)       :: cgSTATE1, cgSTATE2 
      type (state_var)    :: WRFIN
      type (diag_var)     :: WRFINDIA
      type (proj_info)     :: proj

!------------------------------------------------------------------------
      write (*,*) '----do the vortex relocation ----'
      call read_namelist
      if (vortexrelocation) then 
       !!!! this is the file used to replace wrfinput !!!!       
       call get_cg_ijk_dim (trim(inputcgfile1),cix,cjx, ckx ,cox) 
       call cg_allocate (cix,cjx,ckx,cgfile1)
       call get_cg_var (trim(inputcgfile1), cix, cjx, ckx, cox, cgfile1)

       !!!! this is the cg file from wrfinput !!!!
       call get_cg_ijk_dim (trim(inputcgfile2),cix,cjx, ckx ,cox) 
       call cg_allocate (cix,cjx,ckx,cgfile2)
       call cgstate_allocate (cix,cjx,ckx,cgSTATE2)
       call get_cg_var (trim(inputcgfile2), cix, cjx, ckx, cox, cgfile2)
       call get_cg_state_var(trim(inputcgfile2), cix, cjx, ckx, cox, cgSTATE2) 

       !!!! this is wrfinput !!!! 
       call get_ijk_dim (trim(inputwrffile), ix, jx, kx, ox)
       call state_allocate (ix, jx, kx, WRFIN)
       call set_domain_proj (trim(inputwrffile), proj)
       call get_state_var (trim(inputwrffile), ix, jx, kx, 1, WRFIN)
       print*,'ix',ix, jx, kx, ox
       
       call diag_allocate (ix, jx, WRFINDIA)
       call get_diag_var (trim(inputwrffile), ix, jx, kx, 1, WRFINDIA)  

       !!! get information about r !!! 
       rx = int((grads_r_end - grads_r_start) / grads_r_interval + 1.e-6) + 1
       allocate (RR       (rx))

       do r = 1, rx
            RR(r) = grads_r_start + (r-1) * grads_r_interval
       enddo
       r_r1 = int(( radius_1 - grads_r_start ) / grads_r_interval + 1 )
       r_r2 = int(( radius_2 - grads_r_start ) / grads_r_interval + 1 )
       invr = 1.0/( radius_2-radius_1 )
       print*, '--- relexation between',r_r1,' ',r_r2,'-------'         
       print*,cgSTATE2%xlong(1,1,1),r_r1,r_r2



!!!! put data in newu,newv
       allocate(newu(ix+1,jx,kx)) 
       allocate(newv(ix,jx+1,kx)) 
       allocate(newp(ix,jx,kx)) 
       allocate(newph(ix,jx,kx+1)) 
       allocate(newt(ix,jx,kx)) 
       allocate(newqv(ix,jx,kx)) 
       allocate(datatest_array(ix,jx,1)) 
       allocate(newslp(ix,jx)) 
       allocate(newmu(ix,jx)) 
       allocate(newun10(ix,jx))
       allocate(newvn10(ix,jx))
       print*,'--cyl--test' 

       newu=WRFIN%u
       newv=WRFIN%v  
       newp=WRFIN%p  
       newph=WRFIN%ph  
       newqv=WRFIN%qv  
       newt=WRFIN%t + 300.0 
       newmu=WRFIN%mu
       newslp=WRFIN%psfc
       newun10=WRFINDIA%u10
       newvn10=WRFINDIA%v10 
       print*,'--cyl--test2' 
       !!!! replace the data !!!!
       print*,'--cyl--test2' 
       do k = 1,ckx
          print*,k
          do j = 1, cjx
             do i = 1, r_r2 
                call latlon_to_ij (proj,cgSTATE2%xlat(i,j,k),cgSTATE2%xlong(i,j,k),ii_fg,jj_fg)
                ii=nint(ii_fg)
                jj=nint(jj_fg)
                if (cgfile1%un(i,j,k) .ne. missing) then
                  if ( i .le. r_r1 ) then
                      newu(ii,jj,k)=cgfile1%un(i,j,k)
                      newv(ii,jj,k)=cgfile1%vn(i,j,k)
                      newp(ii,jj,k)=cgfile1%p(i,j,k)
                      newph(ii,jj,k)=cgfile1%ph(i,j,k)
                      newt(ii,jj,k)=cgfile1%th(i,j,k)
                      newqv(ii,jj,k)=cgfile1%qv(i,j,k)
                      if ( k .eq. ckx ) then
                       newph(ii,jj,k+1)=cgfile1%ph(i,j,k+1)
                      endif
                      if ( k .eq. 1 ) then
                       newslp(ii,jj)=cgfile1%slp(i,j)
                       newmu(ii,jj)=cgfile1%mu(i,j)
                       newun10(ii,jj)=cgfile1%un10(i,j)
                       newvn10(ii,jj)=cgfile1%vn10(i,j)
                      endif
                   else
                      newu(ii,jj,k)=invr*(cgfile2%un(i,j,k)*(RR(i)-RR(r_r1))+ cgfile1%un(i,j,k)*(RR(r_r2)-RR(i)))
                      newv(ii,jj,k)=invr*(cgfile2%vn(i,j,k)*(RR(i)-RR(r_r1))+ cgfile1%vn(i,j,k)*(RR(r_r2)-RR(i)))
                      newp(ii,jj,k)=invr*(cgfile2%p(i,j,k)*(RR(i)-RR(r_r1))+ cgfile1%p(i,j,k)*(RR(r_r2)-RR(i)))
                      newph(ii,jj,k)=invr*(cgfile2%ph(i,j,k)*(RR(i)-RR(r_r1))+ cgfile1%ph(i,j,k)*(RR(r_r2)-RR(i)))
                      newt(ii,jj,k)=invr*(cgfile2%th(i,j,k)*(RR(i)-RR(r_r1))+ cgfile1%th(i,j,k)*(RR(r_r2)-RR(i)))
                      newqv(ii,jj,k)=invr*(cgfile2%qv(i,j,k)*(RR(i)-RR(r_r1))+ cgfile1%qv(i,j,k)*(RR(r_r2)-RR(i)))
                      if ( k .eq. ckx ) then
                         newph(ii,jj,k+1)=invr*(cgfile2%ph(i,j,k+1)*(RR(i)-RR(r_r1))+ cgfile1%ph(i,j,k+1)*(RR(r_r2)-RR(i)))
                      endif
                      if ( k .eq. 1 ) then
                         newslp(ii,jj)=invr*(cgfile2%slp(i,j)*(RR(i)-RR(r_r1))+ cgfile1%slp(i,j)*(RR(r_r2)-RR(i)))
                         newmu(ii,jj)=invr*(cgfile2%mu(i,j)*(RR(i)-RR(r_r1))+ cgfile1%mu(i,j)*(RR(r_r2)-RR(i)))
                         newun10(ii,jj)=invr*(cgfile2%un10(i,j)*(RR(i)-RR(r_r1))+ cgfile1%un10(i,j)*(RR(r_r2)-RR(i)))
                         newvn10(ii,jj)=invr*(cgfile2%vn10(i,j)*(RR(i)-RR(r_r1))+ cgfile1%vn10(i,j)*(RR(r_r2)-RR(i)))
 
                      endif
                   endif
                endif ![if cgfile1 .ne. missing]
             enddo ![i = 1, cix]
          enddo ![j = 1, cjx]
       enddo ![k = 1,ckx] 


       print*,'--cyl--test2' 
!!!!!!!!!!!!!!!!!balance  subroutine:

         

!!!! open wrfinput file to be writing mode
       istart(:)=1
       istart_2d(:)=1
       iend(4)=1 
       iend_2d(3)=1 
       rcode = nf_open(trim(inputwrffile),nf_Write,ncid)
       rcode = nf_inq_varid ( ncid, 'U', varid ) 
       iend(1)=size(WRFIN%u,1)
       iend(2)=size(WRFIN%u,2)
       iend(3)=size(WRFIN%u,3) 
       rcode = nf_put_vara_real(ncid,varid, istart, iend, newu)
       print*,'U',varid,rcode
       print*,istart,iend,ncid
       print*, nf_strerror(rcode)

       rcode = nf_inq_varid ( ncid, 'V', varid ) 
       iend(1)=size(WRFIN%v,1)
       iend(2)=size(WRFIN%v,2)
       iend(3)=size(WRFIN%v,3) 
       rcode = nf_put_vara_real(ncid,varid, istart, iend, newv)
       print*,'V',varid,rcode
       print*, nf_strerror(rcode)

       rcode = nf_inq_varid ( ncid, 'P', varid )
       iend(1)=size(WRFIN%p,1)
       iend(2)=size(WRFIN%p,2)
       iend(3)=size(WRFIN%p,3)
       rcode = nf_put_vara_real(ncid,varid, istart, iend, newp)
       print*,'P',varid,rcode
       print*, nf_strerror(rcode)

       rcode = nf_inq_varid ( ncid, 'PH', varid )
       iend(1)=size(WRFIN%ph,1)
       iend(2)=size(WRFIN%ph,2)
       iend(3)=size(WRFIN%ph,3)
       rcode = nf_put_vara_real(ncid,varid, istart, iend, newph)
       print*,'PH',varid,rcode
       print*, nf_strerror(rcode)


       rcode = nf_inq_varid ( ncid, 'T', varid )
       iend(1)=size(WRFIN%t,1)
       iend(2)=size(WRFIN%t,2)
       iend(3)=size(WRFIN%t,3)
       rcode = nf_put_vara_real(ncid,varid, istart, iend, newt-300.0)
       print*,'T',varid,rcode,iend
       print*, nf_strerror(rcode)

       rcode = nf_inq_varid ( ncid, 'QVAPOR', varid )
       iend(1)=size(WRFIN%qv,1)
       iend(2)=size(WRFIN%qv,2)
       iend(3)=size(WRFIN%qv,3)
       rcode = nf_put_vara_real(ncid,varid, istart, iend, newqv)
       print*,'QVAPOR',varid,rcode,iend
       print*, nf_strerror(rcode)

       rcode = nf_inq_varid ( ncid, 'PSFC', varid )
       iend_2d(1)=size(WRFIN%qv,1)
       iend_2d(2)=size(WRFIN%qv,2)
       rcode = nf_put_vara_real(ncid,varid,istart_2d,iend_2d, newslp)
       print*,'PSFC',varid,rcode,newslp(100,100)
       print*, nf_strerror(rcode)
       rcode=nf_close(ncid)
       rcode = nf_open(trim(inputwrffile),nf_Write,ncid)
       print*,inputwrffile
       rcode = nf_get_vara_real(ncid,varid,istart_2d,iend_2d,datatest_array)
       print*,datatest_array(100,100,1)
       print*, nf_strerror(rcode)

       rcode = nf_inq_varid ( ncid, 'MU', varid )
       iend_2d(1)=size(WRFIN%qv,1)
       iend_2d(2)=size(WRFIN%qv,2)
       rcode = nf_put_vara_real(ncid,varid,istart_2d,iend_2d, newmu)
       print*,'MU',varid,rcode,newmu(100,100)
      print*, nf_strerror(rcode)
       rcode=nf_close(ncid)
       rcode = nf_open(trim(inputwrffile),nf_Write,ncid)
       print*,inputwrffile
       rcode = nf_get_vara_real(ncid,varid,istart_2d,iend_2d,datatest_array)
       print*,datatest_array(100,100,1)
       print*, nf_strerror(rcode)

      rcode = nf_inq_varid ( ncid, 'U10', varid )
       iend_2d(1)=size(WRFIN%qv,1)
       iend_2d(2)=size(WRFIN%qv,2)
       rcode = nf_put_vara_real(ncid,varid,istart_2d,iend_2d, newun10)
       print*,'U10',varid,rcode,iend_2d
      print*, nf_strerror(rcode)
       rcode=nf_close(ncid)
       rcode = nf_open(trim(inputwrffile),nf_Write,ncid)
       print*,inputwrffile
       rcode = nf_get_vara_real(ncid,varid,istart_2d,iend_2d,datatest_array)
       print*,datatest_array(100,100,1)

       rcode = nf_inq_varid ( ncid, 'V10', varid )
       iend_2d(1)=size(WRFIN%qv,1)
       iend_2d(2)=size(WRFIN%qv,2)
       rcode = nf_put_vara_real(ncid,varid,istart_2d,iend_2d, newvn10)
       print*,'U10',varid,rcode,iend_2d
      print*, nf_strerror(rcode)
       rcode=nf_close(ncid)
       rcode = nf_open(trim(inputwrffile),nf_Write,ncid)
       print*,inputwrffile
       rcode = nf_get_vara_real(ncid,varid,istart_2d,iend_2d,datatest_array)
       print*,datatest_array(100,100,1)







       endif  ![if vortex_relocation] 

      end program vortex_relocation
