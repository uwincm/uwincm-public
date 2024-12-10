PROGRAM umwm
!=======================================================================
!
!               !======================================!
!               !  ~ University of Miami Wave Model ~  !
!               !======================================!
!
! version 2.0.0
!
! Mark Donelan, RSMAS/AMP, University of Miami, USA
! Milan Curcic, RSMAS/MPO, University of Miami, USA
!
! DESCRIPTION: Main program that starts, runs, and finalizes
!              the wave model.
!
! REVISION HISTORY:
!
! 2016-09-26: 2.0.0
!                   - Updated default values of physics coefficients
!                   - Variable sheltering coefficient A1
!                   - Changed plunging breaking factor to 
!                     COTH(0.2*kd)
!                   - Added a ramp to explim
!                   - Added non-blocking MPI routines for gridded
!                     output;
!                   - Added dependence on Stokes drift velocities in
!                     calculation of skin drag;
!                   - Corrected the momentum flux into ocean top;
!                     momentum fluxes are now conservative;
!                   - Added output:
!                      * Total momentum in x and y;
!                      * Wind stress and tail components in x and y;
!                      * Wind stress from diagnostic spectrum;
!                      * Grid curvature;
!                      * Advective momentum flux;
!                      * Intrinsic and total phase and group speed
!                      * Mean squared slope;
!                      * Stokes e-folding depth;
!                   - NetCDF attribute clean-up;
!                   - Added time dimension to NetCDF gridded output;
!                   - Various bug fixes;
!
! 2012-09-05: 1.0.1
!                   - Added Stokes drift calculation and output;
!                   - Grid spacing now computed from lat/lon fields;
!                   - Fixed filling of isolated seas/lakes;
!                   - Code clean-up;
!                   - Various bug fixes;
!
! 2012-04-01: 1.0.0
!                   - First public release
!
!=======================================================================

USE UMWM_module,ONLY:startTimeStr => startTimeStr_nml,&
                     stopTimeStr  => stopTimeStr_nml
USE UMWM_top,ONLY:umwm_initialize,umwm_run,umwm_finalize

IMPLICIT NONE

!=======================================================================

CALL umwm_initialize()
CALL umwm_run(startTimeStr,stopTimeStr)
CALL umwm_finalize()

!=======================================================================
ENDPROGRAM umwm
