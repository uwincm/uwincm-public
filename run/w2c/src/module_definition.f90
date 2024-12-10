!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  Definition of types, and their operations.
!
!-------------------------------------------------------------------------------
!
!  subroutine state_allocate   (ix, jx, kx, iSTATE)
!  subroutine state_deallocate (iSTATE)
!  subroutine state_compact    (ci1, ci2, cj1, cj2, kx, iSTATE, cSTATE)
!  subroutine state_nullify    (cSTATE)
!  subroutine base_allocate    (ix, jx, kx, iBASE)
!  subroutine base_deallocate  (iBASE)
!  subroutine base_compact     (ci1, ci2, cj1, cj2, kx, iBASE, cBASE)
!  subroutine base_nullify     (cBASE)
!  subroutine diag_allocate    (ix, jx, iDIAG)
!  subroutine diag_deallocate  (iDIAG)
!  subroutine diag_compact     (ci1, ci2, cj1, cj2, kx, iBASE, cBASE)
!  subroutine diag_nullify     (cBASE)
!  subroutine ocean_allocate    (ix, jx, kx, iOCEAN)
!  subroutine ocean_deallocate  (iOCEAN)
!  subroutine ocean_compact     (ci1, ci2, cj1, cj2, kx, iOCEAN, cOCEAN)
!  subroutine ocean_nullify     (cOCEAN)
!  subroutine cg_allocate    (ix, jx, kx, iOCEAN)
!  subroutine cg_deallocate  (iOCEAN)
!  subroutine cgstate_allocate (cgstate)  
!  subroutine cgstate_deallocate (cgstate)  
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      module definition

        implicit none

!-------------------------------------------------------------------------------

      type state_var
      real, pointer    :: u(:,:,:)
      real, pointer    :: v(:,:,:)
      real, pointer    :: w(:,:,:)
      real, pointer    :: t(:,:,:)
      real, pointer    :: ph(:,:,:)
      real, pointer    :: mu(:,:)
      real, pointer    :: qv(:,:,:)
      real, pointer    :: qc(:,:,:)
      real, pointer    :: qr(:,:,:)
      real, pointer    :: qi(:,:,:)
      real, pointer    :: qs(:,:,:)
      real, pointer    :: qg(:,:,:)
      real, pointer    :: tracer1(:,:,:)
      real, pointer    :: tracer2(:,:,:)
      real, pointer    :: tracer3(:,:,:)
      real, pointer    :: tracer4(:,:,:)
      real, pointer    :: p(:,:,:)
      real, pointer    :: psfc(:,:)
      end type state_var

      type base_var
      real, pointer    :: phb(:,:,:)
      real, pointer    :: mub(:,:)
      real, pointer    :: xlong(:,:)
      real, pointer    :: xlat(:,:)
      real, pointer    :: znw(:)
      real, pointer    :: znu(:)
      end type base_var

      type diag_var
      real, pointer    :: q2(:,:)
      real, pointer    :: t2(:,:)
      real, pointer    :: th2(:,:)
      real, pointer    :: psfc(:,:)
      real, pointer    :: u10(:,:)
      real, pointer    :: v10(:,:)
      real, pointer    :: hflux(:,:)
      real, pointer    :: lhflux(:,:)
      real, pointer    :: rainc(:,:)
      real, pointer    :: rain(:,:)
      real, pointer    :: snow(:,:)
      real, pointer    :: tsk(:,:)
       end type diag_var
  
      type ocean_var
      real, pointer    :: om_tmp(:,:,:)
      real, pointer    :: om_u(:,:,:)
      real, pointer    :: om_v(:,:,:)
      real, pointer    :: om_s(:,:,:)
      real, pointer    :: om_ml(:,:)
      real, pointer    :: om_depth(:,:,:)
      end type ocean_var

      type cg_var
      real, pointer :: storm_center_lon(:)
      real, pointer :: storm_center_lat(:)
      real, pointer :: slp(:,:)
      real, pointer :: precipitation(:,:)
      real, pointer :: q2(:,:)
      real, pointer :: rh2(:,:)
      real, pointer :: t2(:,:)
      real, pointer :: th2(:,:)
      real, pointer :: shflux(:,:)
      real, pointer :: lhflux(:,:)
      real, pointer :: tsk(:,:)
      real, pointer :: un10(:,:)
      real, pointer :: vn10(:,:)
      real, pointer :: vt10(:,:)
      real, pointer :: vr10(:,:)
      real, pointer :: t(:,:,:)
      real, pointer :: th(:,:,:)
      real, pointer :: w(:,:,:)
      real, pointer :: pressure(:,:,:)
      real, pointer :: h(:,:,:)
      real, pointer :: q(:,:,:)
      real, pointer :: dbz(:,:,:)
      real, pointer :: xlong(:,:,:)
      real, pointer :: xlat(:,:,:)
      real, pointer :: qv(:,:,:)
      real, pointer :: qr(:,:,:)
      real, pointer :: qs(:,:,:)
      real, pointer :: rh(:,:,:)
      real, pointer :: qc(:,:,:)
      real, pointer :: qi(:,:,:)
      real, pointer :: vt(:,:,:)
      real, pointer :: vr(:,:,:)
      real, pointer :: vn(:,:,:)
      real, pointer :: un(:,:,:)
      real, pointer :: tracer1(:,:,:)
      real, pointer :: tracer2(:,:,:)
      real, pointer :: tracer3(:,:,:)
      real, pointer :: tracer4(:,:,:)
      real, pointer :: ph(:,:,:)
      real, pointer :: p(:,:,:)
      real, pointer :: mu(:,:)
      end type cg_var

      type cgstate
      real, pointer :: xlong(:,:,:) 
      real, pointer :: xlat(:,:,:) 
      end type cgstate

      contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine cg_allocate (ix, jx, kx, iCG)
      implicit none
      type (cg_var) :: iCG
      integer          :: ix, jx, kx
      allocate ( iCG%storm_center_lon         (kx  ) )
      allocate ( iCG%storm_center_lat         (kx  ) )
      allocate ( iCG%t         (ix  , jx  , kx  ) )
      allocate ( iCG%th        (ix  , jx  , kx  ) )
      allocate ( iCG%w         (ix  , jx  , kx  ) )
      allocate ( iCG%pressure  (ix  , jx  , kx  ) )
      allocate ( iCG%ph        (ix  , jx  , kx+1) )
      allocate ( iCG%p         (ix  , jx  , kx  ) )
      allocate ( iCG%h         (ix  , jx  , kx  ) )
      allocate ( iCG%rh        (ix  , jx  , kx  ) )
      allocate ( iCG%q         (ix  , jx  , kx  ) )
      allocate ( iCG%dbz       (ix  , jx  , kx  ) )
      allocate ( iCG%xlong     (ix  , jx  , kx  ) )
      allocate ( iCG%xlat      (ix  , jx  , kx  ) )
      allocate ( iCG%qc        (ix  , jx  , kx  ) )
      allocate ( iCG%qv        (ix  , jx  , kx  ) )
      allocate ( iCG%qr        (ix  , jx  , kx  ) )
      allocate ( iCG%qs        (ix  , jx  , kx  ) )
      allocate ( iCG%qi        (ix  , jx  , kx  ) )
      allocate ( iCG%vt        (ix  , jx  , kx  ) )
      allocate ( iCG%vr        (ix  , jx  , kx  ) )
      allocate ( iCG%vn        (ix  , jx  , kx  ) )
      allocate ( iCG%un        (ix  , jx  , kx  ) )
      allocate ( iCG%tracer1   (ix  , jx  , kx  ) )
      allocate ( iCG%tracer2   (ix  , jx  , kx  ) )
      allocate ( iCG%tracer3   (ix  , jx  , kx  ) )
      allocate ( iCG%tracer4   (ix  , jx  , kx  ) )
      allocate ( iCG%slp       (ix  , jx        ) )
      allocate ( iCG%precipitation   (ix  , jx        ) )
      allocate ( iCG%q2        (ix  , jx        ) )
      allocate ( iCG%rh2       (ix  , jx        ) )
      allocate ( iCG%t2        (ix  , jx        ) )
      allocate ( iCG%th2       (ix  , jx        ) )
      allocate ( iCG%shflux    (ix  , jx        ) )
      allocate ( iCG%lhflux    (ix  , jx        ) )
      allocate ( iCG%tsk       (ix  , jx        ) )
      allocate ( iCG%un10      (ix  , jx        ) )
      allocate ( iCG%vn10      (ix  , jx        ) )
      allocate ( iCG%vt10      (ix  , jx        ) )
      allocate ( iCG%vr10      (ix  , jx        ) )
      allocate ( iCG%mu        (ix  , jx        ) )

       end subroutine cg_allocate
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine cg_deallocate (ix, jx, kx, iCG)
      implicit none
      type (cg_var) :: iCG
      integer          :: ix, jx, kx
      deallocate ( iCG%storm_center_lon          )
      deallocate ( iCG%storm_center_lat          )
      deallocate ( iCG%t          )
      deallocate ( iCG%th         )
      deallocate ( iCG%w          )
      deallocate ( iCG%pressure   )
      deallocate ( iCG%ph         )
      deallocate ( iCG%p          )
      deallocate ( iCG%h          )
      deallocate ( iCG%rh         )
      deallocate ( iCG%q          )
      deallocate ( iCG%dbz        )
      deallocate ( iCG%xlong      )
      deallocate ( iCG%xlat       )
      deallocate ( iCG%qv         )
      deallocate ( iCG%qr         )
      deallocate ( iCG%qc         )
      deallocate ( iCG%qi         )
      deallocate ( iCG%vt         )
      deallocate ( iCG%vr         )
      deallocate ( iCG%vn         )
      deallocate ( iCG%un         )
      deallocate ( iCG%tracer1    )
      deallocate ( iCG%tracer2    )
      deallocate ( iCG%tracer3    )
      deallocate ( iCG%tracer4    )
      deallocate ( iCG%slp        )
      deallocate ( iCG%precipitation    )
      deallocate ( iCG%q2         )
      deallocate ( iCG%rh2        )
      deallocate ( iCG%t2         )
      deallocate ( iCG%th2        )
      deallocate ( iCG%shflux     )
      deallocate ( iCG%lhflux     )
      deallocate ( iCG%tsk        )
      deallocate ( iCG%un10       )
      deallocate ( iCG%vn10       )
      deallocate ( iCG%vt10       )
      deallocate ( iCG%vr10       )
      deallocate ( iCG%mu         )
       end subroutine cg_deallocate
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine cgstate_allocate(ix, jx, kx, iCGSTATE)
      implicit none
      type (cgstate) :: iCGSTATE
      integer        :: ix, jx, kx    
      allocate (iCGSTATE%xlong (ix, jx, kx ))
      allocate (iCGSTATE%xlat  (ix, jx, kx ))
      end subroutine cgstate_allocate
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine cgstate_deallocate(ix, jx, kx, iCGSTATE)
      implicit none
      type (cgstate) :: iCGSTATE
      integer        :: ix, jx, kx    
      deallocate (iCGSTATE%xlong )
      deallocate (iCGSTATE%xlat  )
      end subroutine cgstate_deallocate
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine ocean_allocate (ix, jx, ox, iOCEAN)
      implicit none
      type (ocean_var) :: iOCEAN
      integer          :: ix, jx, ox
      allocate ( iOCEAN%om_tmp  (ix  , jx  , ox  ) )
      allocate ( iOCEAN%om_u    (ix  , jx  , ox  ) )
      allocate ( iOCEAN%om_v    (ix  , jx  , ox  ) )
      allocate ( iOCEAN%om_s    (ix  , jx  , ox  ) )
      allocate ( iOCEAN%om_depth(ix  , jx  , ox  ) )
      allocate ( iOCEAN%om_ml   (ix  , jx        ) )
       end subroutine ocean_allocate
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       subroutine ocean_deallocate (iOCEAN)

      implicit none

      type (ocean_var) :: iOCEAN

      deallocate ( iOCEAN%om_tmp )
      deallocate ( iOCEAN%om_u )
      deallocate ( iOCEAN%om_v )
      deallocate ( iOCEAN%om_s )
      deallocate ( iOCEAN%om_depth )
      deallocate ( iOCEAN%om_ml )

      end subroutine ocean_deallocate

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine ocean_compact (ci1, ci2, cj1, cj2, ox, iOCEAN, cOCEAN)

      implicit none

      type (ocean_var) :: iOCEAN, cOCEAN
      integer          :: ci1, ci2, cj1, cj2, ox

      cOCEAN%om_tmp => iOCEAN%om_tmp(ci1:ci2  , cj1:cj2  , 1:ox  )
      cOCEAN%om_u   => iOCEAN%om_u(ci1:ci2  , cj1:cj2  , 1:ox  )
      cOCEAN%om_v   => iOCEAN%om_v(ci1:ci2  , cj1:cj2  , 1:ox  )
      cOCEAN%om_s   => iOCEAN%om_s(ci1:ci2  , cj1:cj2  , 1:ox  )
      cOCEAN%om_depth   => iOCEAN%om_depth(ci1:ci2  , cj1:cj2  , 1:ox  )
      cOCEAN%om_ml  => iOCEAN%om_ml(ci1:ci2  , cj1:cj2    )

      end subroutine ocean_compact

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine ocean_nullify (cOCEAN)

      implicit none

      type (ocean_var) :: cOCEAN

      nullify ( cOCEAN%om_tmp  )
      nullify ( cOCEAN%om_u  )
      nullify ( cOCEAN%om_v  )
      nullify ( cOCEAN%om_s  )
      nullify ( cOCEAN%om_depth  )
      nullify ( cOCEAN%om_ml )

      end subroutine ocean_nullify


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine state_allocate (ix, jx, kx, iSTATE)

      implicit none

      type (state_var) :: iSTATE
      integer          :: ix, jx, kx

      allocate ( iSTATE%u  (ix+1, jx  , kx  ) )
      allocate ( iSTATE%v  (ix  , jx+1, kx  ) )
      allocate ( iSTATE%w  (ix  , jx  , kx+1) )
      allocate ( iSTATE%t  (ix  , jx  , kx  ) )
      allocate ( iSTATE%ph (ix  , jx  , kx+1) )
      allocate ( iSTATE%mu (ix  , jx        ) )
      allocate ( iSTATE%qv (ix  , jx  , kx  ) )
      allocate ( iSTATE%qc (ix  , jx  , kx  ) )
      allocate ( iSTATE%qr (ix  , jx  , kx  ) )
      allocate ( iSTATE%qi (ix  , jx  , kx  ) )
      allocate ( iSTATE%qs (ix  , jx  , kx  ) )
      allocate ( iSTATE%qg (ix  , jx  , kx  ) )
      allocate ( iSTATE%tracer1 (ix  , jx  , kx  ) )
      allocate ( iSTATE%tracer2 (ix  , jx  , kx  ) )
      allocate ( iSTATE%tracer3 (ix  , jx  , kx  ) )
      allocate ( iSTATE%tracer4 (ix  , jx  , kx  ) )
      allocate ( iSTATE%p (ix  , jx  , kx  ) )
      allocate ( iSTATE%psfc (ix  , jx ) )

      end subroutine state_allocate

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine state_deallocate (iSTATE)

      implicit none

      type (state_var) :: iSTATE

      deallocate ( iSTATE%u  )
      deallocate ( iSTATE%v  )
      deallocate ( iSTATE%w  )
      deallocate ( iSTATE%t  )
      deallocate ( iSTATE%ph )
      deallocate ( iSTATE%mu )
      deallocate ( iSTATE%qv )
      deallocate ( iSTATE%qc )
      deallocate ( iSTATE%qr )
      deallocate ( iSTATE%qi )
      deallocate ( iSTATE%qs )
      deallocate ( iSTATE%qg )
      deallocate ( iSTATE%tracer1 )
      deallocate ( iSTATE%tracer2 )
      deallocate ( iSTATE%tracer3 )
      deallocate ( iSTATE%tracer4 )
      deallocate ( iSTATE%p  )
      deallocate ( iSTATE%psfc  )

      end subroutine state_deallocate

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine state_compact (ci1, ci2, cj1, cj2, kx, iSTATE, cSTATE)

      implicit none

      type (state_var) :: iSTATE, cSTATE
      integer          :: ci1, ci2, cj1, cj2, kx

      cSTATE%u  => iSTATE%u (ci1:ci2+1, cj1:cj2  , 1:kx  )
      cSTATE%v  => iSTATE%v (ci1:ci2  , cj1:cj2+1, 1:kx  )
      cSTATE%w  => iSTATE%w (ci1:ci2  , cj1:cj2  , 1:kx+1)
      cSTATE%t  => iSTATE%t (ci1:ci2  , cj1:cj2  , 1:kx  )
      cSTATE%ph => iSTATE%ph(ci1:ci2  , cj1:cj2  , 1:kx+1)
      cSTATE%mu => iSTATE%mu(ci1:ci2  , cj1:cj2          )
      cSTATE%qv => iSTATE%qv(ci1:ci2  , cj1:cj2  , 1:kx  )
      cSTATE%qc => iSTATE%qc(ci1:ci2  , cj1:cj2  , 1:kx  )
      cSTATE%qr => iSTATE%qr(ci1:ci2  , cj1:cj2  , 1:kx  )
      cSTATE%qi => iSTATE%qi(ci1:ci2  , cj1:cj2  , 1:kx  )
      cSTATE%qs => iSTATE%qs(ci1:ci2  , cj1:cj2  , 1:kx  )
      cSTATE%qg => iSTATE%qg(ci1:ci2  , cj1:cj2  , 1:kx  )
      cSTATE%tracer1 => iSTATE%tracer1(ci1:ci2  , cj1:cj2  , 1:kx  )
      cSTATE%tracer2 => iSTATE%tracer2(ci1:ci2  , cj1:cj2  , 1:kx  )
      cSTATE%tracer3 => iSTATE%tracer3(ci1:ci2  , cj1:cj2  , 1:kx  )
      cSTATE%tracer4 => iSTATE%tracer4(ci1:ci2  , cj1:cj2  , 1:kx  )
      cSTATE%p => iSTATE%p (ci1:ci2  , cj1:cj2  , 1:kx  )
      cSTATE%psfc => iSTATE%psfc (ci1:ci2  , cj1:cj2  )

      end subroutine state_compact

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       subroutine state_nullify (cSTATE)

      implicit none

      type (state_var) :: cSTATE

      nullify ( cSTATE%u  )
      nullify ( cSTATE%v  )
      nullify ( cSTATE%w  )
      nullify ( cSTATE%t  )
      nullify ( cSTATE%ph )
      nullify ( cSTATE%mu )
      nullify ( cSTATE%qv )
      nullify ( cSTATE%qc )
      nullify ( cSTATE%qr )
      nullify ( cSTATE%qi )
      nullify ( cSTATE%qs )
      nullify ( cSTATE%qg )
      nullify ( cSTATE%tracer1 )
      nullify ( cSTATE%tracer2 )
      nullify ( cSTATE%tracer3 )
      nullify ( cSTATE%tracer4 )
      nullify ( cSTATE%p  )
      nullify ( cSTATE%psfc  )

      end subroutine state_nullify

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine base_allocate (ix, jx, kx, iBASE)

      implicit none

      type (base_var) :: iBASE
      integer         :: ix, jx, kx

      allocate ( iBASE%phb (ix, jx, kx+1) )
      allocate ( iBASE%mub (ix, jx      ) )
      allocate ( iBASE%xlong (ix, jx      ) )
      allocate ( iBASE%xlat (ix, jx      ) )
      allocate ( iBASE%znw (        kx+1) )
      allocate ( iBASE%znu (        kx  ) )

      end subroutine base_allocate

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine base_deallocate (iBASE)

      implicit none

      type (base_var) :: iBASE

      deallocate ( iBASE%phb )
      deallocate ( iBASE%mub )
      deallocate ( iBASE%xlong )
      deallocate ( iBASE%xlat )
!cyl comment out znw
!      deallocate ( iBASE%znw )
      deallocate ( iBASE%znu )

      end subroutine base_deallocate

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine base_compact (ci1, ci2, cj1, cj2, kx, iBASE, cBASE)

      implicit none

      type (base_var) :: iBASE, cBASE
      integer         :: ci1, ci2, cj1, cj2, kx

      cBASE%phb => iBASE%phb(ci1:ci2, cj1:cj2, 1:kx+1)
      cBASE%mub => iBASE%mub(ci1:ci2, cj1:cj2        )
      cBASE%xlong => iBASE%xlong(ci1:ci2, cj1:cj2        )
      cBASE%xlat => iBASE%xlat(ci1:ci2, cj1:cj2        )
      cBASE%znw => iBASE%znw(                  1:kx+1)
      cBASE%znu => iBASE%znu(                  1:kx  )

      end subroutine base_compact

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine base_nullify (cBASE)

      implicit none

      type (base_var) :: cBASE

      nullify ( cBASE%phb )
      nullify ( cBASE%mub )
      nullify ( cBASE%xlong )
      nullify ( cBASE%xlat )
      nullify ( cBASE%znw )
      nullify ( cBASE%znu )

      end subroutine base_nullify

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine diag_allocate (ix, jx, iDIAG)

      implicit none

      type (diag_var) :: iDIAG
      integer         :: ix, jx

      allocate ( iDIAG%q2     (ix, jx) )
      allocate ( iDIAG%t2     (ix, jx) )
      allocate ( iDIAG%th2    (ix, jx) )
      allocate ( iDIAG%psfc   (ix, jx) )
      allocate ( iDIAG%u10    (ix, jx) )
      allocate ( iDIAG%v10    (ix, jx) )
      allocate ( iDIAG%hflux  (ix, jx) )
      allocate ( iDIAG%lhflux (ix, jx) )
      allocate ( iDIAG%rainc  (ix, jx) )
      allocate ( iDIAG%rain   (ix, jx) )
      allocate ( iDIAG%snow   (ix, jx) )
      allocate ( iDIAG%tsk   (ix, jx) )

      end subroutine diag_allocate

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine diag_deallocate (iDIAG)

      implicit none

      type (diag_var) :: iDIAG

      deallocate ( iDIAG%q2     )
      deallocate ( iDIAG%t2     )
      deallocate ( iDIAG%th2    )
      deallocate ( iDIAG%psfc   )
      deallocate ( iDIAG%u10    )
      deallocate ( iDIAG%v10    )
      deallocate ( iDIAG%hflux  )
      deallocate ( iDIAG%lhflux )
      deallocate ( iDIAG%rainc  )
      deallocate ( iDIAG%rain   )
      deallocate ( iDIAG%snow   )
      deallocate ( iDIAG%tsk   )

      end subroutine diag_deallocate

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine diag_compact (ci1, ci2, cj1, cj2, iDIAG, cDIAG)

      implicit none

      type (diag_var) :: iDIAG, cDIAG
      integer         :: ci1, ci2, cj1, cj2

      cDIAG%q2     => iDIAG%q2    (ci1:ci2, cj1:cj2)
      cDIAG%t2     => iDIAG%t2    (ci1:ci2, cj1:cj2)
      cDIAG%th2    => iDIAG%th2   (ci1:ci2, cj1:cj2)
      cDIAG%psfc   => iDIAG%psfc  (ci1:ci2, cj1:cj2)
      cDIAG%u10    => iDIAG%u10   (ci1:ci2, cj1:cj2)
      cDIAG%v10    => iDIAG%v10   (ci1:ci2, cj1:cj2)
      cDIAG%hflux  => iDIAG%hflux (ci1:ci2, cj1:cj2)
      cDIAG%lhflux => iDIAG%lhflux(ci1:ci2, cj1:cj2)
      cDIAG%rainc  => iDIAG%rainc (ci1:ci2, cj1:cj2)
      cDIAG%rain   => iDIAG%rain  (ci1:ci2, cj1:cj2)
      cDIAG%snow   => iDIAG%snow  (ci1:ci2, cj1:cj2)
      cDIAG%snow   => iDIAG%tsk  (ci1:ci2, cj1:cj2)

      end subroutine diag_compact

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      end module definition 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
