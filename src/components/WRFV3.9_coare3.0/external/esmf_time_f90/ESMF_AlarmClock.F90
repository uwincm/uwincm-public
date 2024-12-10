!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA license.
!
!==============================================================================
!
!     timelib Alarm-Clock Module
      module timelib_AlarmClockMod
!
!==============================================================================
!
! This file contains the AlarmCreate method.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <timelib_TimeMgr.inc>

!===============================================================================
!BOPI
!
! !MODULE: timelib_AlarmClockMod
!
! !DESCRIPTION:
! Separate module that uses both timelib_AlarmMod and timelib_ClockMod.  
! Separation is needed to avoid cyclic dependence.  
!
! Defines F90 wrapper entry points for corresponding
! C++ class {\tt timelibc\_Alarm}
!
! See {\tt ../include/timelibc\_Alarm.h} for complete description
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit timelib_Alarm and timelib_Clock
      use timelib_AlarmMod, only : timelib_Alarm, timelib_AlarmSet
      use timelib_ClockMod, only : timelib_Clock, timelib_ClockAddAlarm

      ! associated derived types
      use timelib_TimeIntervalMod, only : timelib_TimeInterval
      use timelib_TimeMod,         only : timelib_Time

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
     private
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:
      public timelib_AlarmCreate

!==============================================================================

      contains

!==============================================================================


! Create timelib_Alarm using timelib 2.1.0+ semantics
      FUNCTION timelib_AlarmCreate( clock, RingTime, RingInterval, &
                                 StopTime, Enabled, rc )

        ! return value
        type(timelib_Alarm) :: timelib_AlarmCreate
        ! !ARGUMENTS:
        type(timelib_Clock), intent(inout), optional :: clock
        type(timelib_Time), intent(in), optional :: RingTime
        type(timelib_TimeInterval), intent(in), optional :: RingInterval
        type(timelib_Time), intent(in), optional :: StopTime
        logical, intent(in), optional :: Enabled
        integer, intent(out), optional :: rc
        ! locals
        type(timelib_Alarm) :: alarmtmp
         ! TBH:  ignore allocate errors, for now
        ALLOCATE( alarmtmp%alarmint )
        CALL timelib_AlarmSet( alarmtmp,                  &
                            RingTime=RingTime,         &
                            RingInterval=RingInterval, &
                            StopTime=StopTime,         &
                            Enabled=Enabled,           &
                            rc=rc )
        IF ( PRESENT ( clock ) ) THEN
          CALL timelib_ClockAddAlarm( clock, alarmtmp, rc )
        ENDIF
        timelib_AlarmCreate = alarmtmp
      END FUNCTION timelib_AlarmCreate


!------------------------------------------------------------------------------

      end module timelib_AlarmClockMod
