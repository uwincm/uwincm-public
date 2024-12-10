!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  Read or WRITE WRF
!  created 2008/05 -- cwudpf, TDRC
!
!-------------------------------------------------------------------------------
!
!  subroutine get_ijk_dim     (wrf_file, ix, jx, kx)
!  subroutine get_time        (wrf_file,             tt, time_str, time_minute)
!  subroutine get_state_var   (wrf_file, ix, jx, kx, tt, iSTATE)
!  subroutine get_base_var    (wrf_file, ix, jx, kx, tt, iBASE)
!  subroutine get_diag_var    (wrf_file, ix, jx, kx, tt, iDIAG)
!  subroutine write_state_var (wrf_file, ix, jx, kx, tt, iSTATE)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module WRF_IO

  contains

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
      call close_file(fid)

  end subroutine get_ijk_dim

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine get_cg_ijk_dim (wrf_file, ix, jx, kx, ox)

      use WRF_TOOLS
      implicit none

      character (len=*)  :: wrf_file   ! [in]
      integer            :: ix, jx, kx ,ox! [out]
      integer            :: fid

      call open_file(wrf_file, nf_nowrite, fid)
      ix = get_dimlen(fid, 'radius')
      jx = get_dimlen(fid, 'theta')
      kx = get_dimlen(fid, 'bottom_top')
      ox = get_dimlen(fid, 'ocean_depth')
      call close_file(fid)

  end subroutine get_cg_ijk_dim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine get_time (wrf_file, tt, time_str, time_minute)

      use WRF_TOOLS
      implicit none

      character (len=*)  :: wrf_file    ! [in]
      integer            :: tt          ! [in]
      character (len=19) :: time_str    ! [out]
      real               :: time_minute ! [out]
      integer            :: fid

      call open_file(wrf_file, nf_nowrite, fid)
      call get_text(fid, 'Times', 19, tt, time_str)
      call close_file(fid)

  end subroutine get_time

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine get_ocean_var (wrf_file, ix, jx, ox, tt, iOCEAN)

      use DEFINITION
      use WRF_TOOLS
      implicit none

!-------------------------------------------------------------------------------

      integer                  :: ix, jx, ox, fid, i, j, k, tt
      type (ocean_var)         :: iOCEAN
      character (len=*)        :: wrf_file

!-------------------------------------------------------------------------------

      call open_file (wrf_file, nf_nowrite, fid)

      call get_variable3d (fid, 'OM_TMP'   , ix  , jx  , ox  , tt, iOCEAN%om_tmp )
      call get_variable3d (fid, 'OM_U'     , ix  , jx  , ox  , tt, iOCEAN%om_u )
      call get_variable3d (fid, 'OM_V'     , ix  , jx  , ox  , tt, iOCEAN%om_v )
      call get_variable3d (fid, 'OM_S'     , ix  , jx  , ox  , tt, iOCEAN%om_s )
      call get_variable3d (fid, 'OM_DEPTH' , ix  , jx  , ox  , tt, iOCEAN%om_depth )
      call get_variable2d (fid, 'OM_ML'    , ix  , jx    , tt, iOCEAN%om_ml)

      call close_file(fid)

!-------------------------------------------------------------------------------

  end subroutine get_ocean_var


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  this is to get data from CG file
!  Chiaying Lee RSMAS/UM
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine get_cg_var (wrf_file, ix, jx, kx, tt, iFILE)

      use DEFINITION
      use WRF_TOOLS
      implicit none

!-------------------------------------------------------------------------------

      integer                  :: ix, jx, kx, fid, i, j, k, tt
      type (cg_var)         :: iFILE
      character (len=*)        :: wrf_file

!-------------------------------------------------------------------------------

      call open_file (wrf_file, nf_nowrite, fid)

      call get_variable1d (fid, 'storm_center_lon', kx  ,      tt, iFILE%storm_center_lon )
      call get_variable1d (fid, 'storm_center_lat', kx  ,      tt, iFILE%storm_center_lat )

      call get_variable2d (fid, 'slp'             , ix  , jx  ,      tt, iFILE%slp )
      call get_variable2d (fid, 'precipitation'   , ix  , jx  ,      tt, iFILE%precipitation )
      call get_variable2d (fid, 'q2'              , ix  , jx  ,      tt, iFILE%q2 )
      call get_variable2d (fid, 'rh2'             , ix  , jx  ,      tt, iFILE%rh2 )
      call get_variable2d (fid, 't2'              , ix  , jx  ,      tt, iFILE%t2 )
      call get_variable2d (fid, 'th2'             , ix  , jx  ,      tt, iFILE%th2 )
      call get_variable2d (fid, 'shflux'          , ix  , jx  ,      tt, iFILE%shflux )
      call get_variable2d (fid, 'lhflux'          , ix  , jx  ,      tt, iFILE%lhflux )
      call get_variable2d (fid, 'tsk'             , ix  , jx  ,      tt, iFILE%tsk )
      call get_variable2d (fid, 'un10'            , ix  , jx  ,      tt, iFILE%un10 )
      call get_variable2d (fid, 'vn10'            , ix  , jx  ,      tt, iFILE%vn10 )
      call get_variable2d (fid, 'vt10'            , ix  , jx  ,      tt, iFILE%vt10 )
      call get_variable2d (fid, 'vr10'            , ix  , jx  ,      tt, iFILE%vr10 )
      call get_variable2d (fid, 'mu  '            , ix  , jx  ,      tt, iFILE%mu )
      call get_variable3d (fid, 't'               , ix  , jx  , kx  , tt, iFILE%t )
      call get_variable3d (fid, 'th'              , ix  , jx  , kx  , tt, iFILE%th )
      call get_variable3d (fid, 'w'               , ix  , jx  , kx  , tt, iFILE%w )
      call get_variable3d (fid, 'pressure'        , ix  , jx  , kx  , tt, iFILE%pressure )
      call get_variable3d (fid, 'h'               , ix  , jx  , kx  , tt, iFILE%h )
      call get_variable3d (fid, 'rh'              , ix  , jx  , kx  , tt, iFILE%rh )
      call get_variable3d (fid, 'q'               , ix  , jx  , kx  , tt, iFILE%q )
      call get_variable3d (fid, 'dbz'             , ix  , jx  , kx  , tt, iFILE%dbz )
      call get_variable3d (fid, 'xlong'             , ix  , jx  , kx  , tt, iFILE%xlong )
      call get_variable3d (fid, 'xlat'             , ix  , jx  , kx  , tt, iFILE%xlat )
      call get_variable3d (fid, 'qv'              , ix  , jx  , kx  , tt, iFILE%qv )
      call get_variable3d (fid, 'qr'              , ix  , jx  , kx  , tt, iFILE%qr )
      call get_variable3d (fid, 'qs'              , ix  , jx  , kx  , tt, iFILE%qs )
      call get_variable3d (fid, 'qc'              , ix  , jx  , kx  , tt, iFILE%qc )
      call get_variable3d (fid, 'qi'              , ix  , jx  , kx  , tt, iFILE%qi )
      call get_variable3d (fid, 'vt'              , ix  , jx  , kx  , tt, iFILE%vt )
      call get_variable3d (fid, 'vr'              , ix  , jx  , kx  , tt, iFILE%vr )
      call get_variable3d (fid, 'un'              , ix  , jx  , kx  , tt, iFILE%un )
      call get_variable3d (fid, 'vn'              , ix  , jx  , kx  , tt, iFILE%vn )
      call get_variable3d (fid, 'tracer1'              , ix  , jx  , kx  , tt, iFILE%tracer1)
      call get_variable3d (fid, 'tracer2'              , ix  , jx  , kx  , tt, iFILE%tracer2)
      call get_variable3d (fid, 'tracer3'              , ix  , jx  , kx  , tt, iFILE%tracer3)
      call get_variable3d (fid, 'tracer4'              , ix  , jx  , kx  , tt, iFILE%tracer4)
      call get_variable3d (fid, 'perturbation_ph'      , ix  , jx  , kx+1, tt, iFILE%ph )
      call get_variable3d (fid, 'perturbation_p'       , ix  , jx  , kx  , tt, iFILE%p  )

      call close_file(fid)

!-------------------------------------------------------------------------------

  end subroutine get_cg_var

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine get_cgocean_var (wrf_file, ix, jx, kx, tt, iCGOCEAN)

      use DEFINITION
      use WRF_TOOLS
      implicit none

!-------------------------------------------------------------------------------

      integer                  :: ix, jx, kx, fid, i, j, k, tt
      type (ocean_var)       :: iCGOCEAN
      character (len=*)        :: wrf_file

!-------------------------------------------------------------------------------

      call open_file (wrf_file, nf_nowrite, fid)

      call get_variable2d (fid, 'om_ml'  , ix  , jx  ,       tt, iCGOCEAN%om_ml )
      call get_variable3d (fid, 'om_tmp' , ix  , jx  , kx  , tt, iCGOCEAN%om_tmp )
      call get_variable3d (fid, 'om_u'   , ix  , jx  , kx  , tt, iCGOCEAN%om_u )
      call get_variable3d (fid, 'om_v'   , ix  , jx  , kx  , tt, iCGOCEAN%om_v )
      call get_variable3d (fid, 'om_s'   , ix  , jx  , kx  , tt, iCGOCEAN%om_s )
      call close_file(fid)

!-------------------------------------------------------------------------------
  end subroutine get_cgocean_var

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine get_cg_state_var(wrf_file, ix, jx, kx, tt,iCGSTATE)

     use DEFINITION
     use WRF_TOOLS
     implicit none
!-------------------------------------------------------------------------------

     integer                  :: ix, jx, kx, fid, i, j, k, tt
     type (cgstate)       :: iCGSTATE
     character (len=*)        :: wrf_file
!-------------------------------------------------------------------------------
     call open_file (wrf_file, nf_nowrite, fid)
     call get_variable3d (fid, 'xlong' , ix  , jx  , kx  , tt, iCGSTATE%xlong )
     call get_variable3d (fid, 'xlat'  , ix  , jx  , kx  , tt, iCGSTATE%xlat  )
     call close_file(fid)
!-------------------------------------------------------------------------------

  end subroutine get_cg_state_var

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine get_state_var (wrf_file, ix, jx, kx, tt, iSTATE)

      use DEFINITION
      use WRF_TOOLS
      implicit none

!-------------------------------------------------------------------------------

      integer                  :: ix, jx, kx, fid, i, j, k, tt
      type (state_var)         :: iSTATE
      character (len=*)        :: wrf_file

!-------------------------------------------------------------------------------

      call open_file (wrf_file, nf_nowrite, fid)

      call get_variable3d (fid, 'U'     , ix+1, jx,   kx  , tt, iSTATE%u )
      call get_variable3d (fid, 'V'     , ix  , jx+1, kx  , tt, iSTATE%v )
      call get_variable3d (fid, 'W'     , ix  , jx  , kx+1, tt, iSTATE%w )
      call get_variable3d (fid, 'T'     , ix  , jx  , kx  , tt, iSTATE%t )
      call get_variable3d (fid, 'PH'    , ix  , jx  , kx+1, tt, iSTATE%ph)
      call get_variable2d (fid, 'MU'    , ix  , jx  ,       tt, iSTATE%mu)
      call get_variable3d (fid, 'QVAPOR', ix  , jx  , kx  , tt, iSTATE%qv)
      call get_variable3d (fid, 'QCLOUD', ix  , jx  , kx  , tt, iSTATE%qc)
      call get_variable3d (fid, 'QRAIN' , ix  , jx  , kx  , tt, iSTATE%qr)
      call get_variable3d (fid, 'QICE'  , ix  , jx  , kx  , tt, iSTATE%qi)
      call get_variable3d (fid, 'QSNOW' , ix  , jx  , kx  , tt, iSTATE%qs)
!TRACER
!     call get_variable3d (fid, 'tr17_1' , ix  , jx  , kx  , tt, iSTATE%tracer1)
!     call get_variable3d (fid, 'tr17_2' , ix  , jx  , kx  , tt, iSTATE%tracer2)
!     call get_variable3d (fid, 'tr17_3' , ix  , jx  , kx  , tt, iSTATE%tracer3)
!     call get_variable3d (fid, 'tr17_4' , ix  , jx  , kx  , tt, iSTATE%tracer4)
      call get_variable3d (fid, 'P'     , ix  , jx  , kx  , tt, iSTATE%p)
      call get_variable2d (fid, 'PSFC'  , ix  , jx  ,       tt, iSTATE%psfc)
!      call get_variable3d (fid, 'QGRAUP', ix  , jx  , kx  , tt, iSTATE%qg)

      call close_file(fid)

!-------------------------------------------------------------------------------

      do k = 1, kx
      do j = 1, jx
      do i = 1, ix
        if (iSTATE%qv(i,j,k) .LT. 1.E-8) iSTATE%qv(i,j,k) = 1.E-8
        if (iSTATE%qc(i,j,k) .LT. 1.E-8) iSTATE%qc(i,j,k) = 1.E-8
        if (iSTATE%qr(i,j,k) .LT. 1.E-8) iSTATE%qr(i,j,k) = 1.E-8
        if (iSTATE%qi(i,j,k) .LT. 1.E-8) iSTATE%qi(i,j,k) = 1.E-8
        if (iSTATE%qs(i,j,k) .LT. 1.E-8) iSTATE%qs(i,j,k) = 1.E-8
!        if (iSTATE%qg(i,j,k) .LT. 1.E-8) iSTATE%qg(i,j,k) = 1.E-8
      end do
      end do
      end do

!-------------------------------------------------------------------------------

  end subroutine get_state_var

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine get_base_var (wrf_file, ix, jx, kx, tt, iBASE)

      use DEFINITION
      use WRF_TOOLS
      implicit none

!-------------------------------------------------------------------------------

      integer                  :: ix, jx, kx, fid, tt
      type (base_var)          :: iBASE
      character (len=*)        :: wrf_file

!-------------------------------------------------------------------------------

      call open_file (wrf_file, nf_nowrite, fid)

      call get_variable3d (fid, 'PHB', ix  , jx  , kx+1, tt, iBASE%phb)
      call get_variable2d (fid, 'MUB', ix  , jx  ,       tt, iBASE%mub)
      call get_variable2d (fid, 'XLONG', ix  , jx  ,       tt, iBASE%xlong)
      call get_variable2d (fid, 'XLAT', ix  , jx  ,       tt, iBASE%xlat)
      call get_variable1d (fid, 'ZNW', kx+1,             tt, iBASE%znw)
      call get_variable1d (fid, 'ZNU', kx  ,             tt, iBASE%znu)

      call close_file(fid)

!-------------------------------------------------------------------------------

  end subroutine get_base_var

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine get_diag_var (wrf_file, ix, jx, kx, tt, iDIAG)

      use DEFINITION
      use WRF_TOOLS
      implicit none

!-------------------------------------------------------------------------------

      integer                  :: ix, jx, kx, fid, tt
      type (diag_var)          :: iDIAG
      character (len=*)        :: wrf_file

!-------------------------------------------------------------------------------

      call open_file (wrf_file, nf_nowrite, fid)

      call get_variable2d (fid, 'Q2'    , ix  , jx  , tt, iDIAG%q2)
      call get_variable2d (fid, 'T2'    , ix  , jx  , tt, iDIAG%t2)
      call get_variable2d (fid, 'TH2'   , ix  , jx  , tt, iDIAG%th2)
      call get_variable2d (fid, 'PSFC'  , ix  , jx  , tt, iDIAG%psfc)
      call get_variable2d (fid, 'U10'   , ix  , jx  , tt, iDIAG%u10)
      call get_variable2d (fid, 'V10'   , ix  , jx  , tt, iDIAG%v10)
      call get_variable2d (fid, 'HFX'   , ix  , jx  , tt, iDIAG%hflux)
      call get_variable2d (fid, 'LH'    , ix  , jx  , tt, iDIAG%lhflux)
      call get_variable2d (fid, 'RAINCV' , ix  , jx  , tt, iDIAG%rainc)
      call get_variable2d (fid, 'RAINNCV', ix  , jx  , tt, iDIAG%rain)
      call get_variable2d (fid, 'SNOWNC', ix  , jx  , tt, iDIAG%snow)
      call get_variable2d (fid, 'TSK'   , ix  , jx  , tt, iDIAG%tsk)

      call close_file(fid)

!-------------------------------------------------------------------------------

  end subroutine get_diag_var

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine write_state_var (wrf_file, ix, jx, kx, tt, iSTATE)

      use DEFINITION
      use WRF_TOOLS
      implicit none

!-------------------------------------------------------------------------------

      integer                  :: ix, jx, kx, fid, i, j, k, tt
      type (state_var)         :: iSTATE
      character (len=*)        :: wrf_file

!-------------------------------------------------------------------------------

      do k = 1, kx
      do j = 1, jx
      do i = 1, ix
        if (iSTATE%qv(i,j,k) .LT. 1.E-8) iSTATE%qv(i,j,k) = 1.E-8
        if (iSTATE%qc(i,j,k) .LT. 1.E-8) iSTATE%qc(i,j,k) = 1.E-8
        if (iSTATE%qr(i,j,k) .LT. 1.E-8) iSTATE%qr(i,j,k) = 1.E-8
        if (iSTATE%qi(i,j,k) .LT. 1.E-8) iSTATE%qi(i,j,k) = 1.E-8
        if (iSTATE%qs(i,j,k) .LT. 1.E-8) iSTATE%qs(i,j,k) = 1.E-8
        if (iSTATE%qg(i,j,k) .LT. 1.E-8) iSTATE%qg(i,j,k) = 1.E-8
      end do
      end do
      end do

!-------------------------------------------------------------------------------

      call open_file (wrf_file, nf_write, fid)

      call write_variable3d (fid, 'U'     , ix+1, jx,   kx  , tt, iSTATE%u )
      call write_variable3d (fid, 'V'     , ix  , jx+1, kx  , tt, iSTATE%v )
      call write_variable3d (fid, 'W'     , ix  , jx  , kx+1, tt, iSTATE%w )
      call write_variable3d (fid, 'T'     , ix  , jx  , kx  , tt, iSTATE%t )
      call write_variable3d (fid, 'PH'    , ix  , jx  , kx+1, tt, iSTATE%ph)
      call write_variable2d (fid, 'MU'    , ix  , jx  ,       tt, iSTATE%mu)
      call write_variable3d (fid, 'QVAPOR', ix  , jx  , kx  , tt, iSTATE%qv)
      call write_variable3d (fid, 'QCLOUD', ix  , jx  , kx  , tt, iSTATE%qc)
      call write_variable3d (fid, 'QRAIN' , ix  , jx  , kx  , tt, iSTATE%qr)
      call write_variable3d (fid, 'QICE'  , ix  , jx  , kx  , tt, iSTATE%qi)
      call write_variable3d (fid, 'QSNOW' , ix  , jx  , kx  , tt, iSTATE%qs)
      call write_variable3d (fid, 'QGRAUP', ix  , jx  , kx  , tt, iSTATE%qg)

      call close_file(fid)

!-------------------------------------------------------------------------------

  end subroutine write_state_var

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module WRF_IO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
