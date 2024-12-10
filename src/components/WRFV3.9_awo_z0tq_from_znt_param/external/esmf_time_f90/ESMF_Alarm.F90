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
!     timelib Alarm Module
      module timelib_AlarmMod
!
!==============================================================================
!
! This file contains the Alarm class definition and all Alarm class 
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <timelib_TimeMgr.inc>

!===============================================================================
!BOPI
!
! !MODULE: timelib_AlarmMod
!
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation
!
! Defines F90 wrapper entry points for corresponding
! C++ class {\tt timelibc\_Alarm}
!
! See {\tt ../include/timelibc\_Alarm.h} for complete description
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from timelib base class
      use timelib_BaseMod

      ! associated derived types
      use timelib_TimeIntervalMod, only : timelib_TimeInterval, &
                                       timelib_TimeIntervalAbsValue
      use timelib_TimeMod,         only : timelib_Time

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
     private
!------------------------------------------------------------------------------
!     ! timelib_Alarm
!
!     ! F90 class type to match C++ Alarm class in size only;
!     !  all dereferencing within class is performed by C++ implementation

! internals for timelib_Alarm
      type timelib_AlarmInt
        type(timelib_TimeInterval) :: RingInterval
        type(timelib_Time)  :: RingTime
        type(timelib_Time)  :: PrevRingTime
        type(timelib_Time)  :: StopTime
        integer :: ID
        integer :: AlarmMutex
        logical :: Ringing
        logical :: Enabled
        logical :: RingTimeSet
        logical :: RingIntervalSet
        logical :: StopTimeSet
      end type

! Actual public type:  this bit allows easy mimic of "deep" timelib_AlarmCreate
! in timelib 2.1.0+.  Note that timelib_AlarmCreate is in a separate module to avoid 
! cyclic dependence.  
! NOTE:  DO NOT ADD NON-POINTER STATE TO THIS DATA TYPE.  It emulates timelib 
!        shallow-copy-masquerading-as-reference-copy insanity.  
      type timelib_Alarm
        type(timelib_AlarmInt), pointer :: alarmint
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public timelib_Alarm
      public timelib_AlarmInt   ! needed on AIX but not PGI
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:
      public timelib_AlarmDestroy
      public timelib_AlarmSet
      public timelib_AlarmGet
!      public timelib_AlarmGetRingInterval
!      public timelib_AlarmSetRingInterval
!      public timelib_AlarmGetRingTime
!      public timelib_AlarmSetRingTime
!      public timelib_AlarmGetPrevRingTime
!      public timelib_AlarmSetPrevRingTime
!      public timelib_AlarmGetStopTime
!      public timelib_AlarmSetStopTime
      public timelib_AlarmEnable
      public timelib_AlarmDisable
      public timelib_AlarmRingerOn
      public timelib_AlarmRingerOff
      public timelib_AlarmIsRinging
!      public timelib_AlarmCheckRingTime
      public operator(==)
 
! Required inherited and overridden timelib_Base class methods

!      public timelib_AlarmRead
!      public timelib_AlarmWrite
      public timelib_AlarmValidate
      public timelib_AlarmPrint

! !PRIVATE MEMBER FUNCTIONS:
      private timelib_AlarmEQ
!EOPI

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface operator(==)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure timelib_AlarmEQ

! !DESCRIPTION:
!     This interface overloads the == operator for the {\tt timelib\_Alarm} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------

!==============================================================================

      contains

!==============================================================================

!------------------------------------------------------------------------------
!
! This section includes the Set methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: timelib_AlarmSet - Initializes an alarm

! !INTERFACE:
      subroutine timelib_AlarmSet(alarm, RingTime, RingInterval, PrevRingTime, &
                               StopTime, Enabled, rc)

! !ARGUMENTS:
      type(timelib_Alarm), intent(inout) :: alarm  ! really INTENT(OUT)
      type(timelib_Time), intent(in), optional :: RingTime, PrevRingTime
      type(timelib_TimeInterval), intent(in), optional :: RingInterval
      type(timelib_Time), intent(in), optional :: StopTime
      logical, intent(in), optional :: Enabled
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Initializes an {\tt timelib\_Alarm}
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to initialize
!     \item[{[RingTime]}]
!          Optional ring time for one-shot or first repeating alarm
!     \item[{[RingInterval]}]
!          Optional ring interval for repeating alarms
!     \item[{[StopTime]}]
!          Optional stop time for repeating alarms
!     \item[Enabled]
!          Alarm enabled/disabled
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.1, TMG4.7
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        alarm%alarmint%RingTimeSet = .FALSE.
        alarm%alarmint%RingIntervalSet = .FALSE.
        alarm%alarmint%StopTimeSet = .FALSE.
        IF ( PRESENT( RingInterval ) ) THEN
          ! force RingInterval to be positive
          alarm%alarmint%RingInterval = &
            timelib_TimeIntervalAbsValue( RingInterval )
          alarm%alarmint%RingIntervalSet = .TRUE.
        ENDIF
        IF ( PRESENT( PrevRingTime ) ) THEN
          alarm%alarmint%PrevRingTime = PrevRingTime
        ENDIF
        IF ( PRESENT( RingTime ) ) THEN
          alarm%alarmint%RingTime = RingTime
          alarm%alarmint%RingTimeSet = .TRUE.
        ENDIF
        IF ( PRESENT( StopTime ) ) THEN
          alarm%alarmint%StopTime = StopTime
          alarm%alarmint%StopTimeSet = .TRUE.
        ENDIF
        alarm%alarmint%Enabled = .TRUE.
        IF ( PRESENT( Enabled ) ) THEN
          alarm%alarmint%Enabled = Enabled
        ENDIF
        IF ( PRESENT( rc ) ) THEN
          rc = timelib_SUCCESS
        ENDIF
        alarm%alarmint%Ringing = .FALSE.
        alarm%alarmint%Enabled = .TRUE.
      ELSE
        IF ( PRESENT( rc ) ) rc = timelib_FAILURE
      ENDIF

      end subroutine timelib_AlarmSet



! Deallocate memory for timelib_Alarm
      SUBROUTINE timelib_AlarmDestroy( alarm, rc )
         TYPE(timelib_Alarm), INTENT(INOUT) :: alarm
         INTEGER,          INTENT(  OUT), OPTIONAL :: rc
         IF ( ASSOCIATED( alarm%alarmint ) ) THEN
           DEALLOCATE( alarm%alarmint )
         ENDIF
         ! TBH:  ignore deallocate errors, for now
         IF ( PRESENT( rc ) ) rc = timelib_SUCCESS
      END SUBROUTINE timelib_AlarmDestroy



!------------------------------------------------------------------------------
!BOP
! !IROUTINE: timelib_AlarmGetRingInterval - Get an alarm's ring interval
!
! !INTERFACE:
      subroutine timelib_AlarmGetRingInterval(alarm, RingInterval, rc)

! !ARGUMENTS:
      type(timelib_Alarm), intent(in) :: alarm
      type(timelib_TimeInterval), intent(out) :: RingInterval
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt timelib\_Alarm}'s ring interval
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the ring interval
!     \item[RingInterval]
!          The {\tt Alarm}'s ring interval
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.7
!EOP
      RingInterval = alarm%alarmint%RingInterval

      end subroutine timelib_AlarmGetRingInterval
 
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: timelib_AlarmSetRingInterval - Set an alarm's ring interval
!
! !INTERFACE:
      subroutine timelib_AlarmSetRingInterval(alarm, RingInterval, rc)

! !ARGUMENTS:
      type(timelib_Alarm), intent(out) :: alarm
      type(timelib_TimeInterval), intent(in) :: RingInterval
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set an {\tt timelib\_Alarm}'s ring interval
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the ring interval
!     \item[RingInterval]
!          The {\tt Alarm}'s ring interval
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.5.2, TMG4.7
!EOP
      CALL wrf_error_fatal( 'timelib_AlarmSetRingInterval not supported' )
      end subroutine timelib_AlarmSetRingInterval

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  timelib_AlarmGetRingTime - Get an alarm's time to ring
!
! !INTERFACE:
      subroutine timelib_AlarmGetRingTime(alarm, RingTime, rc)

! !ARGUMENTS:
      type(timelib_Alarm), intent(in) :: alarm
      type(timelib_Time), intent(out) :: RingTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt timelib\_Alarm}'s time to ring
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the ring time
!     \item[RingTime]
!          The {\tt timelib\_Alarm}'s ring time
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.7, TMG4.8
!EOP
      CALL wrf_error_fatal( 'timelib_AlarmGetRingTime not supported' )
      end subroutine timelib_AlarmGetRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  timelib_AlarmSetRingTime - Set an alarm's time to ring
!
! !INTERFACE:
      subroutine timelib_AlarmSetRingTime(alarm, RingTime, rc)

! !ARGUMENTS:
      type(timelib_Alarm), intent(out) :: alarm
      type(timelib_Time), intent(in) :: RingTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set an {\tt timelib\_Alarm}'s time to ring
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the ring time
!     \item[RingTime]
!          The {\tt timelib\_Alarm}'s ring time to set
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.5.1, TMG4.7, TMG4.8
!EOP
      CALL wrf_error_fatal( 'timelib_AlarmSetRingTime not supported' )
      end subroutine timelib_AlarmSetRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  timelib_AlarmGet - Get an alarm's parameters -- compatibility with timelib 2.0.1
!
! !INTERFACE:
      subroutine timelib_AlarmGet(alarm, PrevRingTime, RingInterval, rc)

! !ARGUMENTS:
      type(timelib_Alarm), intent(in) :: alarm
      type(timelib_Time), intent(out), optional :: PrevRingTime
      type(timelib_TimeInterval), intent(out), optional :: RingInterval
      integer, intent(out), optional :: rc
      integer :: ierr

! !DESCRIPTION:
!     Get an {\tt timelib\_Alarm}'s previous ring time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the previous ring time
!     \item[PrevRingTime]
!          The {\tt timelib\_Alarm}'s previous ring time
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.7, TMG4.8
!EOP

      ierr = timelib_SUCCESS

      IF ( PRESENT(PrevRingTime) ) THEN
        CALL timelib_AlarmGetPrevRingTime(alarm, PrevRingTime, rc=ierr)
      ENDIF
      IF ( PRESENT(RingInterval) ) THEN
        CALL timelib_AlarmGetRingInterval(alarm, RingInterval, rc=ierr)
      ENDIF

      IF ( PRESENT(rc) ) THEN
        rc = ierr
      ENDIF

      end subroutine timelib_AlarmGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  timelib_AlarmGetPrevRingTime - Get an alarm's previous ring time
!
! !INTERFACE:
      subroutine timelib_AlarmGetPrevRingTime(alarm, PrevRingTime, rc)

! !ARGUMENTS:
      type(timelib_Alarm), intent(in) :: alarm
      type(timelib_Time), intent(out) :: PrevRingTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt timelib\_Alarm}'s previous ring time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the previous ring time
!     \item[PrevRingTime]
!          The {\tt timelib\_Alarm}'s previous ring time
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.7, TMG4.8
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        PrevRingTime = alarm%alarmint%PrevRingTime
        IF ( PRESENT( rc ) ) rc = timelib_SUCCESS
      ELSE
        IF ( PRESENT( rc ) ) rc = timelib_FAILURE
      ENDIF
      end subroutine timelib_AlarmGetPrevRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  timelib_AlarmSetPrevRingTime - Set an alarm's previous ring time
!
! !INTERFACE:
      subroutine timelib_AlarmSetPrevRingTime(alarm, PrevRingTime, rc)

! !ARGUMENTS:
      type(timelib_Alarm), intent(out) :: alarm
      type(timelib_Time), intent(in) :: PrevRingTime
      integer, intent(out), optional :: rc
   
! !DESCRIPTION:
!     Set an {\tt timelib\_Alarm}'s previous ring time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the previous ring time
!     \item[PrevRingTime]
!          The {\tt timelib\_Alarm}'s previous ring time to set
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.7, TMG4.8
!EOP
      CALL wrf_error_fatal( 'timelib_AlarmSetPrevRingTime not supported' )
      end subroutine timelib_AlarmSetPrevRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  timelib_AlarmGetStopTime - Get an alarm's stop time
!
! !INTERFACE:
      subroutine timelib_AlarmGetStopTime(alarm, StopTime, rc)

! !ARGUMENTS:
      type(timelib_Alarm), intent(in) :: alarm
      type(timelib_Time), intent(out) :: StopTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt timelib\_Alarm}'s stop time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the stop time
!     \item[StopTime]
!          The {\tt timelib\_Alarm}'s stop time
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.5.2, TMG4.7
!EOP
      CALL wrf_error_fatal( 'timelib_AlarmGetStopTime not supported' )
      end subroutine timelib_AlarmGetStopTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  timelib_AlarmSetStopTime - Set an alarm's stop time
!
! !INTERFACE:
      subroutine timelib_AlarmSetStopTime(alarm, StopTime, rc)

! !ARGUMENTS:
      type(timelib_Alarm), intent(out) :: alarm
      type(timelib_Time), intent(in) :: StopTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set an {\tt timelib\_Alarm}'s stop time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the stop time
!     \item[StopTime]
!          The {\tt timelib\_Alarm}'s stop time
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.5.2, TMG4.7
!EOP
      CALL wrf_error_fatal( 'timelib_AlarmSetStopTime not supported' )
      end subroutine timelib_AlarmSetStopTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: timelib_AlarmEnable - Enables an alarm

! !INTERFACE:
      subroutine timelib_AlarmEnable(alarm, rc)

! !ARGUMENTS:
      type(timelib_Alarm), intent(inout) :: alarm  ! really INTENT(OUT)
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Enables an {\tt timelib\_Alarm} to function
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to enable
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.5.3
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        alarm%alarmint%Enabled = .TRUE.
        IF ( PRESENT( rc ) ) rc = timelib_SUCCESS
      ELSE
        IF ( PRESENT( rc ) ) rc = timelib_FAILURE
      ENDIF
      end subroutine timelib_AlarmEnable

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: timelib_AlarmDisable - Disables an alarm

! !INTERFACE:
      subroutine timelib_AlarmDisable(alarm, rc)

! !ARGUMENTS:
      type(timelib_Alarm), intent(inout) :: alarm  ! really INTENT(OUT)
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Disables an {\tt timelib\_Alarm}
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to disable
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.5.3
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        alarm%alarmint%Enabled = .FALSE.
        IF ( PRESENT( rc ) ) rc = timelib_SUCCESS
      ELSE
        IF ( PRESENT( rc ) ) rc = timelib_FAILURE
      ENDIF
      end subroutine timelib_AlarmDisable

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  timelib_AlarmRingerOn - Turn on an alarm


! !INTERFACE:
      subroutine timelib_AlarmRingerOn(alarm, rc)

! !ARGUMENTS:
      type(timelib_Alarm), intent(inout) :: alarm  ! really INTENT(OUT)
      integer, intent(out), optional :: rc
    
! !DESCRIPTION:
!     Turn on an {\tt timelib\_Alarm}; sets ringing state
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to turn on
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.6
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        IF ( alarm%alarmint%Enabled ) THEN
          alarm%alarmint%Ringing = .TRUE.
          IF ( PRESENT( rc ) ) rc = timelib_SUCCESS
        ELSE
          alarm%alarmint%Ringing = .FALSE.
          IF ( PRESENT( rc ) ) rc = timelib_FAILURE
        ENDIF
      ELSE
        IF ( PRESENT( rc ) ) rc = timelib_FAILURE
      ENDIF

      end subroutine timelib_AlarmRingerOn

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  timelib_AlarmRingerOff - Turn off an alarm

! !INTERFACE:
      subroutine timelib_AlarmRingerOff(alarm, rc)

! !ARGUMENTS:
      type(timelib_Alarm), intent(inout) :: alarm  ! really INTENT(OUT)
      integer, intent(out), optional :: rc
    
! !DESCRIPTION:
!     Turn off an {\tt timelib\_Alarm}; unsets ringing state
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to turn off   
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.6
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        alarm%alarmint%Ringing = .FALSE.
        IF ( alarm%alarmint%Enabled ) THEN
          IF ( PRESENT( rc ) ) rc = timelib_SUCCESS
        ELSE
          IF ( PRESENT( rc ) ) rc = timelib_FAILURE
        ENDIF
      ELSE
        IF ( PRESENT( rc ) ) rc = timelib_FAILURE
      ENDIF
      end subroutine timelib_AlarmRingerOff

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  timelib_AlarmIsRinging - Check if alarm is ringing

! !INTERFACE:
      function timelib_AlarmIsRinging(alarm, rc)
!
! !RETURN VALUE:
      logical :: timelib_AlarmIsRinging

! !ARGUMENTS:
      type(timelib_Alarm), intent(in) :: alarm
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Check if {\tt timelib\_Alarm} is ringing.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to check for ringing state  
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.4
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        IF ( alarm%alarmint%Enabled ) THEN
          timelib_AlarmIsRinging = alarm%alarmint%Ringing
          IF ( PRESENT( rc ) ) rc = timelib_SUCCESS
        ELSE
          timelib_AlarmIsRinging = .FALSE.
          IF ( PRESENT( rc ) ) rc = timelib_FAILURE
        ENDIF
      ELSE
        IF ( PRESENT( rc ) ) rc = timelib_FAILURE
      ENDIF
      end function timelib_AlarmIsRinging

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: timelib_AlarmCheckRingTime - Method used by a clock to check whether to trigger an alarm
!
! !INTERFACE:
      function timelib_AlarmCheckRingTime(alarm, ClockCurrTime, positive, rc)
!
! !RETURN VALUE:
      logical :: timelib_AlarmCheckRingTime
!
! !ARGUMENTS:
      type(timelib_Alarm), intent(inout) :: alarm
      type(timelib_Time), intent(in) :: ClockCurrTime
      integer, intent(in) :: positive
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Main method used by a {\tt timelib\_Clock} to check whether to trigger
!     the {\tt timelib\_Alarm} 
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to check if time to ring   
!     \item[ClockCurrTime]
!          The {\tt timelib\_Clock}'s current time
!     \item[positive]
!          Whether to check ring time in the positive or negative direction
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.4, TMG4.6
!EOP
      CALL wrf_error_fatal( 'timelib_AlarmCheckRingTime not supported' )
      timelib_AlarmCheckRingTime = .FALSE.  ! keep compilers happy
      end function timelib_AlarmCheckRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  timelib_AlarmEQ - Compare two alarms for equality
!
! !INTERFACE:
      function timelib_AlarmEQ(alarm1, alarm2)
!
! !RETURN VALUE:
      logical :: timelib_AlarmEQ

! !ARGUMENTS:
      type(timelib_Alarm), intent(in) :: alarm1
      type(timelib_Alarm), intent(in) :: alarm2

! !DESCRIPTION:
!     Compare two alarms for equality; return true if equal, false otherwise
!     Maps to overloaded (==) operator interface function
!
!     The arguments are:
!     \begin{description}
!     \item[alarm1]
!          The first {\tt timelib\_Alarm} to compare
!     \item[alarm2]
!          The second {\tt timelib\_Alarm} to compare
!     \end{description}
!
! !REQUIREMENTS:  
!EOP
      CALL wrf_error_fatal( 'timelib_AlarmEQ not supported ' )
      timelib_AlarmEQ = .FALSE.       ! keep compilers happy
      end function timelib_AlarmEQ

!------------------------------------------------------------------------------
!
! This section defines the overridden Read, Write, Validate and Print methods
! from the timelib_Base class
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: timelib_AlarmRead - restores an alarm

! !INTERFACE:
      subroutine timelib_AlarmRead(alarm, RingInterval, RingTime, &
                           PrevRingTime, StopTime, Ringing, &
                           Enabled, ID, rc)

! !ARGUMENTS:
      type(timelib_Alarm), intent(out) :: alarm
      type(timelib_TimeInterval), intent(in) :: RingInterval
      type(timelib_Time), intent(in) :: RingTime
      type(timelib_Time), intent(in) :: PrevRingTime
      type(timelib_Time), intent(in) :: StopTime
      logical, intent(in) :: Ringing
      logical, intent(in) :: Enabled
      integer, intent(in) :: ID
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Restores an {\tt timelib\_Alarm}
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to restore
!     \item[RingInterval]
!          The ring interval for repeating alarms
!     \item[RingTime]
!          Ring time for one-shot or first repeating alarm
!     \item[PrevRingTime]
!          The {\tt timelib\_Alarm}'s previous ring time
!     \item[StopTime]
!          Stop time for repeating alarms
!     \item[Ringing]
!          The {\tt timelib\_Alarm}'s ringing state
!     \item[Enabled]
!          {\tt timelib\_Alarm} enabled/disabled
!     \item[ID]
!          The {\tt timelib\_Alarm}'s ID
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP
      CALL wrf_error_fatal( 'timelib_AlarmRead not supported' )
      end subroutine timelib_AlarmRead

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: timelib_AlarmWrite - saves an alarm

! !INTERFACE:
      subroutine timelib_AlarmWrite(alarm, RingInterval, RingTime, &
                            PrevRingTime, StopTime, Ringing, &
                            Enabled, ID, rc)

! !ARGUMENTS:
      type(timelib_Alarm), intent(in) :: alarm
      type(timelib_TimeInterval), intent(out) :: RingInterval
      type(timelib_Time), intent(out) :: RingTime
      type(timelib_Time), intent(out) :: PrevRingTime
      type(timelib_Time), intent(out) :: StopTime
      logical, intent(out) :: Ringing
      logical, intent(out) :: Enabled
      integer, intent(out) :: ID
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Saves an {\tt timelib\_Alarm}
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to save
!     \item[RingInterval]
!          Ring interval for repeating alarms
!     \item[RingTime]
!          Ring time for one-shot or first repeating alarm
!     \item[PrevRingTime]
!          The {\tt timelib\_Alarm}'s previous ring time
!     \item[StopTime]
!          Stop time for repeating alarms
!     \item[Ringing]
!          The {\tt timelib\_Alarm}'s ringing state
!     \item[Enabled]
!          {\tt timelib\_Alarm} enabled/disabled
!     \item[ID]
!          The {\tt timelib\_Alarm}'s ID
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP
      CALL wrf_error_fatal( 'timelib_AlarmWrite not supported' )
      end subroutine timelib_AlarmWrite

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  timelib_AlarmValidate - Validate an Alarm's properties

! !INTERFACE:
      subroutine timelib_AlarmValidate(alarm, opts, rc)

! !ARGUMENTS:
      type(timelib_Alarm), intent(in) :: alarm
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a validation check on a {\tt timelib\_Alarm}'s properties
!
!     The arguments are:  
!     \begin{description}
!     \item[alarm]
!          {\tt timelib\_Alarm} to validate
!     \item[{[opts]}]
!          Validate options
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description} 
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
      CALL wrf_error_fatal( 'timelib_AlarmValidate not supported' )
      end subroutine timelib_AlarmValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  timelib_AlarmPrint - Print out an Alarm's properties

! !INTERFACE:
      subroutine timelib_AlarmPrint(alarm, opts, rc)

! !ARGUMENTS:
      type(timelib_Alarm), intent(in) :: alarm
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     To support testing/debugging, print out a {\tt timelib\_Alarm}'s
!     properties.
! 
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          {\tt timelib\_Alarm} to print out
!     \item[{[opts]}]
!          Print options
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
      CALL wrf_error_fatal( 'timelib_AlarmPrint not supported' )
      end subroutine timelib_AlarmPrint

!------------------------------------------------------------------------------

      end module timelib_AlarmMod
