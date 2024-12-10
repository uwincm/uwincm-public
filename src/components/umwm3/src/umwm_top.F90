MODULE UMWM_top
!======================================================================!
!                                                                      !
! DESCRIPTION: A top-level module for the wave model. Sequentially     !
!              initializes, runs, and finalizes the model.             !
!                                                                      !
! CONTAINS: umwm_initialize                                            !
!           umwm_run                                                   !
!           umwm_finalize                                              !
!                                                                      !
!======================================================================!

IMPLICIT NONE

!======================================================================!
CONTAINS



SUBROUTINE umwm_initialize
!======================================================================!

USE UMWM_module,ONLY:startTimeStr => startTimeStr_nml
USE UMWM_init,  ONLY:environment,greeting,nmlread,alloc,grid,masks,&
                     partition,alloc,remap,init
USE UMWM_io,    ONLY:input_nc,output_grid
USE UMWM_stokes,ONLY:stokes_drift

!======================================================================!

CALL environment('INIT')    ! Initialize processing environment
CALL greeting               ! Print version number and license on screen
CALL nmlread                ! Read the namelist
CALL alloc(1)               ! Allocate 2-D arrays 
CALL grid                   ! Define model grid
CALL masks                  ! Define seamasks
CALL partition              ! Domain partitioning 
CALL alloc(2)               ! Allocate unrolled arrays 
CALL remap                  ! Remap 2-D arrays to 1-D
CALL output_grid            ! Output a grid file
CALL input_nc(startTimeStr) ! Read initial fields 
CALL init                   ! Initialize model variables
CALL stokes_drift('INIT')   ! Initialize Stokes drift arrays

ENDSUBROUTINE umwm_initialize
!======================================================================!



SUBROUTINE umwm_run(startTimeStr,stopTimeStr)
!======================================================================!
!                                                                      !
! DESCRIPTION: Solves for the wave variance spectrum F(k,th,x)         !
!              by computing source and advection terms and integrating !
!              in time. At this point, all state variables are         !
!              initialized.                                            !
!                                                                      !
!======================================================================!

#ifdef MPI
USE UMWM_mpi
USE mpi
#endif

USE UMWM_module
USE UMWM_physics,  ONLY:s_ds,s_nl,stress,source,diag
USE UMWM_advection,ONLY:propagation,refraction
USE UMWM_forcing,  ONLY:forcingInput,forcingInterpolate
USE UMWM_io,       ONLY:output_grid_nc,output_spectrum_nc
USE UMWM_restart,  ONLY:restart_read,restart_write
USE UMWM_stokes,   ONLY:stokes_drift
USE UMWM_util,     ONLY:sigWaveHeight,meanWavePeriod

use umwm_source_functions, only: sin_dccm12

USE datetime_module

!=======================================================================

! ARGUMENTS:
CHARACTER(LEN=19),INTENT(IN) :: startTimeStr
CHARACTER(LEN=19),INTENT(IN) :: stopTimeStr

TYPE(tm_struct) :: tm

CHARACTER(LEN=19) :: currentTimeStr

INTEGER :: rc

LOGICAL :: fullHour

!=======================================================================

! Convert start and stop time strings to datetime objects:
startTime = strptime(startTimeStr,'%Y-%m-%d %H:%M:%S')
stopTime  = strptime(stopTimeStr, '%Y-%m-%d %H:%M:%S')

currentTime = startTime

currentTimeStr = TRIM(currentTime % strftime('%Y-%m-%d_%H:%M:%S'))

! Read wave spectrum field from a restart file if necessary:
IF(first .AND. restart)THEN
  CALL restart_read(startTimeStr)
ENDIF

DO WHILE(currentTime < stopTime) ! Outer time loop

  ! Report current and next checkpoint time:
  IF(nproc == 0)WRITE(*,FMT='(A)')&
  'umwm: solver: Current time is:     '//currentTimeStr

  currentTime = currentTime + timedelta(seconds=nint(dtg))
  currentTimeStr = TRIM(currentTime % strftime('%Y-%m-%d_%H:%M:%S'))

  IF(nproc == 0)WRITE(*,FMT='(A)')&
  'umwm: solver: Integrating to time: '//currentTimeStr

  ! Read atmosphere and ocean input fields from file.
  ! In case of ESMF coupling, fields are assumed to be updated
  ! externally, and this call is not used.
#ifndef ESMF
  CALL forcingInput(currentTimeStr)
#endif

  ! Inner time loop: global time step
  sumt = 0.
  DO WHILE(sumt < dtg)

#ifndef ESMF
    CALL forcingInterpolate() ! Interpolate force fields in time
#endif

    CALL sin_dccm12() ! Compute source input term Sin
    CALL s_ds()   ! Compute source dissipation term Sds
    CALL s_nl()   ! Compute non-linear source term Snl
    CALL source() ! Integrate source functions

#ifdef MPI
    !IF(mpiIsBlocking)THEN 
    CALL haloCommBlocking() ! Exchange halo points
    !ELSE
    !CALL haloComm(e)       ! Exchange halo points (non-blocking)
    !ENDIF
#endif

    CALL propagation   ! Compute advection and integrate

#ifdef ESMF
    ! Update:
    e(:,:,istart:iend) = ef(:,:,istart:iend)
#endif

    CALL refraction    ! Compute refraction and integrate

    ! Update:
    e(:,:,istart:iend) = ef(:,:,istart:iend)

    CALL stress('ATM') ! Compute wind stress and drag coefficient

#ifdef ESMF
    CALL stress('OCN') ! Compute stress into ocean top and bottom
#endif

    IF(first)THEN

      ! Diagnostic calculations before output
      CALL diag()

      IF(outgrid > 0)THEN
        ! Gridded NetCDF output
        CALL output_grid_nc(startTimeStr)
      ENDIF

      IF(outspec > 0)THEN
        ! Spectrum NetCDF output
        CALL output_spectrum_nc(startTimeStr)
      ENDIF

#ifdef MPI
      CALL MPI_Barrier(mpi_comm_world,ierr)
#endif

      IF(nproc == 0)THEN
        ! Diagnostic output header on screen
        WRITE(*,FMT='(A)')'dtg frac   tstep [s]  wspd [m/s] wdir [rad]   '&
                        //'swh [m]    mwp [s]    Cd*10^3   fprog [Hz]'
      ENDIF

      first = .FALSE.

    ENDIF

    ! Diagnostic output on screen
    IF(nproc == nproc_plot)THEN
      WRITE(*,FMT=100)sumt/dtg,dts,wspd(iip),wdir(iip),      &
                      sigWaveHeight(iip),meanWavePeriod(iip),&
                      cd(iip)*1E3,f(oc(iip))
    ENDIF

  ENDDO ! END WHILE(sumt<dtg) loop

  IF(firstdtg)THEN
    firstdtg = .FALSE.
  ENDIF

  IF(stokes)CALL stokes_drift

  CALL diag ! Model diagnostics for output

  fullHour = currentTime % getMinute() == 0 .AND. currentTime % getSecond() == 0

  !--- GRIDDED OUTPUT --------------------------------------------------
  IF(outgrid > 0)THEN
    IF(MOD(currentTime % getHour(),outgrid) == 0 .AND. fullHour)THEN
#ifndef ESMF
      CALL stress('OCN')
#endif
      CALL output_grid_nc(currentTimeStr)
    ENDIF
  ELSEIF(outgrid == -1)THEN
#ifndef ESMF
    CALL stress('OCN')
#endif
    CALL output_grid_nc(currentTimeStr)
  ENDIF
  !---------------------------------------------------------------------

  !--- SPECTRUM OUTPUT -------------------------------------------------
  IF(outspec > 0)THEN
    IF(MOD(currentTime % getHour(),outspec) == 0 .AND. fullHour)THEN
      CALL output_spectrum_nc(currentTimeStr)
    ENDIF
  ELSEIF(outspec == -1)THEN
    CALL output_spectrum_nc(currentTimeStr)
  ENDIF
  !---------------------------------------------------------------------

  !--- RESTART OUTPUT --------------------------------------------------
  IF(outrst > 0)THEN
    IF(MOD(currentTime % getHour(),outrst) == 0 .AND. fullHour)&
    CALL restart_write(currentTimeStr)
  ELSEIF(outspec == -1)THEN
    CALL restart_write(currentTimeStr)
  ENDIF
  !---------------------------------------------------------------------

ENDDO ! End outer loop

100 format(2x, f4.2, 2x, f8.3, 6(1x, f9.6))

ENDSUBROUTINE umwm_run
!======================================================================!



SUBROUTINE umwm_finalize
!======================================================================!

USE UMWM_util,ONLY:dealloc
USE UMWM_init,ONLY:environment

!======================================================================!

CALL dealloc()           ! Deallocate arrays
CALL environment('STOP') ! Finalize the environment

ENDSUBROUTINE umwm_finalize
!======================================================================!
ENDMODULE UMWM_top
