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
!     timelib Clock Module
      module timelib_ClockMod
!     
!==============================================================================
!     
! This file contains the Clock class definition and all Clock class methods.
!     
!------------------------------------------------------------------------------
! INCLUDES
#include <timelib_TimeMgr.inc> 

!==============================================================================
!BOPI
! !MODULE: timelib_ClockMod
!     
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation
!
! Defines F90 wrapper entry points for corresponding
! C++ class {\tt timelibc\_Time} implementation
!     
! See {\tt ../include/timelibc\_Clock.h} for complete description
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from timelib base class
      use timelib_BaseMod

      ! associated derived types
      use timelib_TimeIntervalMod   ! , only : timelib_TimeInterval, &
                                 !          timelib_TimeIntervalIsPositive
      use timelib_TimeMod           ! , only : timelib_Time
      use timelib_AlarmMod,        only : timelib_Alarm

      implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! timelib_Clock
!     
!     ! F90 class type to match C++ Clock class in size only;
!     !  all dereferencing within class is performed by C++ implementation

! internals for timelib_Clock
      type timelib_ClockInt
        type(timelib_TimeInterval) :: TimeStep
        type(timelib_Time)  :: StartTime
        type(timelib_Time)  :: StopTime
        type(timelib_Time)  :: RefTime
        type(timelib_Time)  :: CurrTime
        type(timelib_Time)  :: PrevTime
        integer(timelib_KIND_I8) :: AdvanceCount
        integer :: ClockMutex
        integer :: NumAlarms
        ! Note:  to mimic timelib 2.1.0+, AlarmList is maintained 
        ! within timelib_Clock even though copies of each alarm are 
        ! returned from timelib_AlarmCreate() at the same time they 
        ! are copied into the AlarmList!  This duplication is not 
        ! as hideous as it might be because the timelib_Alarm type 
        ! has data members that are all POINTERs (thus the horrible 
        ! shallow-copy-masquerading-as-reference-copy hack works).  
        type(timelib_Alarm), pointer, dimension(:) :: AlarmList
      end type

! Actual public type:  this bit allows easy mimic of "deep" timelib_ClockCreate 
! in timelib 2.1.0+
! NOTE:  DO NOT ADD NON-POINTER STATE TO THIS DATA TYPE.  It emulates timelib 
!        shallow-copy-masquerading-as-reference-copy.  
      type timelib_Clock
        type(timelib_ClockInt), pointer  :: clockint
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public timelib_Clock
      public timelib_ClockInt   ! needed on AIX but not PGI
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
      public timelib_ClockCreate
      public timelib_ClockDestroy
      public timelib_ClockSet
!      public timelib_ClockSetOLD
      public timelib_ClockGet
!      public timelib_ClockGetAdvanceCount
!      public timelib_ClockGetTimeStep
!      public timelib_ClockSetTimeStep
!      public timelib_ClockGetCurrTime
!      public timelib_ClockSetCurrTime
!      public timelib_ClockGetStartTime
!      public timelib_ClockGetStopTime
!      public timelib_ClockGetRefTime
!      public timelib_ClockGetPrevTime
!      public timelib_ClockGetCurrSimTime
!      public timelib_ClockGetPrevSimTime
! This must be public for timelib_AlarmClockMod...  
      public timelib_ClockAddAlarm
      public timelib_ClockGetAlarmList
!      public timelib_ClockGetNumAlarms
!      public timelib_ClockSyncToWallClock
      public timelib_ClockAdvance
      public timelib_ClockIsStopTime
      public timelib_ClockStopTimeDisable

! Required inherited and overridden timelib_Base class methods

!      public timelib_ClockRead
!      public timelib_ClockWrite
      public timelib_ClockValidate
      public timelib_ClockPrint
!EOPI

!==============================================================================

      contains

!==============================================================================
!
! This section includes the Set methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: timelib_ClockSetOLD - Initialize a clockint

! !INTERFACE:
      subroutine timelib_ClockSetOLD(clockint, TimeStep, StartTime, &
                                  StopTime, RefTime, rc)

! !ARGUMENTS:
      type(timelib_ClockInt), intent(out) :: clockint
      type(timelib_TimeInterval), intent(in), optional :: TimeStep
      type(timelib_Time), intent(in) :: StartTime
      type(timelib_Time), intent(in) :: StopTime
      type(timelib_Time), intent(in), optional :: RefTime
      integer, intent(out), optional :: rc
! Local
      integer i
    
! !DESCRIPTION:
!     Initialize an {\tt timelib\_Clock}
!     
!     The arguments are:
!     \begin{description}
!     \item[clockint]
!          The object instance to initialize
!     \item[{[TimeStep]}]
!          The {\tt timelib\_Clock}'s time step interval
!     \item[StartTime]
!          The {\tt timelib\_Clock}'s starting time
!     \item[StopTime]
!          The {\tt timelib\_Clock}'s stopping time
!     \item[{[RefTime]}]
!          The {\tt timelib\_Clock}'s reference time
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!     
! !REQUIREMENTS:
!     TMG3.1, TMG3.4.4
!EOP
      IF ( PRESENT(TimeStep) ) clockint%TimeStep = TimeStep
      IF ( PRESENT(RefTime) )THEN
         clockint%RefTime = RefTime
      ELSE
         clockint%RefTime = StartTime
      END IF
      clockint%CurrTime = StartTime
      clockint%StartTime = StartTime
      clockint%StopTime = StopTime
      clockint%NumAlarms = 0
      clockint%AdvanceCount = 0
      ALLOCATE(clockint%AlarmList(MAX_ALARMS))
      ! TBH:  This incredible hack can be removed once timelib_*Validate() 
      ! TBH:  can tell if a deep timelib_* was created or not.  
      DO i = 1, MAX_ALARMS
        NULLIFY( clockint%AlarmList( i )%alarmint )
      ENDDO
      IF ( PRESENT( rc ) ) rc = timelib_SUCCESS
    
      end subroutine timelib_ClockSetOLD


! !IROUTINE: timelib_ClockSet - Set clock properties -- for compatibility with timelib 2.0.1

! !INTERFACE:
      subroutine timelib_ClockSet(clock, TimeStep, StartTime, StopTime, &
                               RefTime, CurrTime, rc)

! !ARGUMENTS:
      type(timelib_Clock), intent(inout) :: clock
      type(timelib_TimeInterval), intent(in), optional :: TimeStep
      type(timelib_Time), intent(in), optional :: StartTime
      type(timelib_Time), intent(in), optional :: StopTime
      type(timelib_Time), intent(in), optional :: RefTime
      type(timelib_Time), intent(in), optional :: CurrTime
      integer, intent(out), optional :: rc
! Local
      integer ierr
    
! !DESCRIPTION:
!     Initialize an {\tt timelib\_Clock}
!     
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to initialize
!     \item[{[TimeStep]}]
!          The {\tt timelib\_Clock}'s time step interval
!     \item[StartTime]
!          The {\tt timelib\_Clock}'s starting time
!     \item[StopTime]
!          The {\tt timelib\_Clock}'s stopping time
!     \item[{[RefTime]}]
!          The {\tt timelib\_Clock}'s reference time
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!     
! !REQUIREMENTS:
!     TMG3.1, TMG3.4.4
!EOP
      ierr = timelib_SUCCESS
      IF ( PRESENT(TimeStep) ) THEN
        CALL timelib_ClockSetTimeStep ( clock, TimeStep, rc=ierr )
      ENDIF
      IF ( PRESENT(RefTime) ) clock%clockint%RefTime = RefTime
      IF ( PRESENT(StartTime) ) clock%clockint%StartTime = StartTime
      IF ( PRESENT(StopTime) ) clock%clockint%StopTime = StopTime
      IF ( PRESENT(CurrTime) ) THEN
        CALL timelib_ClockSetCurrTime(clock, CurrTime, rc=ierr)
      ENDIF
      IF ( PRESENT(rc) ) rc = ierr

      end subroutine timelib_ClockSet


! Create timelib_Clock using timelib 2.1.0+ semantics
      FUNCTION timelib_ClockCreate( name, TimeStep, StartTime, StopTime, &
                                 RefTime, rc )
        ! return value
        type(timelib_Clock) :: timelib_ClockCreate
        ! !ARGUMENTS:
        character (len=*),       intent(in),  optional :: name
        type(timelib_TimeInterval), intent(in), optional :: TimeStep
        type(timelib_Time), intent(in) :: StartTime
        type(timelib_Time), intent(in) :: StopTime
        type(timelib_Time), intent(in), optional :: RefTime
        integer, intent(out), optional :: rc
        ! locals
        type(timelib_Clock) :: clocktmp
         ! TBH:  ignore allocate errors, for now
        ALLOCATE( clocktmp%clockint )
        CALL timelib_ClockSetOLD( clocktmp%clockint,   &
                               TimeStep= TimeStep,  &
                               StartTime=StartTime, &
                               StopTime= StopTime,  &
                               RefTime=RefTime, rc=rc )
        timelib_ClockCreate = clocktmp
      END FUNCTION timelib_ClockCreate


! Deallocate memory for timelib_Clock
      SUBROUTINE timelib_ClockDestroy( clock, rc )
         TYPE(timelib_Clock), INTENT(INOUT) :: clock
         INTEGER,          INTENT(  OUT), OPTIONAL :: rc
         ! TBH:  ignore deallocate errors, for now
         DEALLOCATE( clock%clockint%AlarmList )
         DEALLOCATE( clock%clockint )
         IF ( PRESENT( rc ) ) rc = timelib_SUCCESS
      END SUBROUTINE timelib_ClockDestroy


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: timelib_ClockGet - Get clock properties -- for compatibility with timelib 2.0.1 

! !INTERFACE:
      subroutine timelib_ClockGet(clock, StartTime, CurrTime,       &
                               AdvanceCount, StopTime, TimeStep, &
                               PrevTime, RefTime, &
                               rc)

! !ARGUMENTS:
      type(timelib_Clock), intent(in) :: clock
      type(timelib_Time), intent(out), optional :: StartTime
      type(timelib_Time), intent(out), optional :: CurrTime
      type(timelib_Time), intent(out), optional :: StopTime
      type(timelib_Time), intent(out), optional :: PrevTime
      type(timelib_Time), intent(out), optional :: RefTime
      integer(timelib_KIND_I8), intent(out), optional :: AdvanceCount
      type(timelib_TimeInterval), intent(out), optional :: TimeStep
      integer, intent(out), optional :: rc
      integer :: ierr

! !DESCRIPTION:
!     Returns the number of times the {\tt timelib\_Clock} has been advanced
!     (time stepped)
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the advance count from
!     \item[StartTime]
!          The start time
!     \item[CurrTime]
!          The current time
!     \item[AdvanceCount]
!          The number of times the {\tt timelib\_Clock} has been advanced
!     \item[StopTime]
!          The {\tt timelib\_Clock}'s stopping time
!     \item[{[TimeStep]}]
!          The {\tt timelib\_Clock}'s time step interval
!     \item[{[PrevTime]}]
!          The {\tt timelib\_Clock}'s previous current time
!     \item[{[PrevTime]}]
!          The {\tt timelib\_Clock}'s reference time
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG3.5.1
!EOP
      ierr = timelib_SUCCESS

      IF ( PRESENT (StartTime) ) THEN
        CALL timelib_ClockGetStartTime( clock, StartTime=StartTime, rc=ierr )
      ENDIF
      IF ( PRESENT (CurrTime) ) THEN
        CALL timelib_ClockGetCurrTime( clock , CurrTime, ierr )
      ENDIF
      IF ( PRESENT (StopTime) ) THEN
        CALL timelib_ClockGetStopTime( clock , StopTime, ierr )
      ENDIF
      IF ( PRESENT (AdvanceCount) ) THEN
        CALL timelib_ClockGetAdvanceCount(clock, AdvanceCount, ierr)
      ENDIF
      IF ( PRESENT (TimeStep) ) THEN
        CALL timelib_ClockGetTimeStep(clock, TimeStep, ierr)
      ENDIF
      IF ( PRESENT (PrevTime) ) THEN
        CALL timelib_ClockGetPrevTime(clock, PrevTime, ierr)
      ENDIF
      IF ( PRESENT (RefTime) ) THEN
        CALL timelib_ClockGetRefTime(clock, RefTime, ierr)
      ENDIF

      IF ( PRESENT (rc) ) THEN
        rc = ierr
      ENDIF
    
      end subroutine timelib_ClockGet


! !IROUTINE: timelib_ClockGetAdvanceCount - Get the clock's advance count

! !INTERFACE:
      subroutine timelib_ClockGetAdvanceCount(clock, AdvanceCount, rc)

! !ARGUMENTS:
      type(timelib_Clock), intent(in) :: clock
      integer(timelib_KIND_I8), intent(out) :: AdvanceCount
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Returns the number of times the {\tt timelib\_Clock} has been advanced
!     (time stepped)
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the advance count from
!     \item[AdvanceCount]
!          The number of times the {\tt timelib\_Clock} has been advanced
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG3.5.1
!EOP

      AdvanceCount = clock%clockint%AdvanceCount

      IF ( PRESENT(rc) ) rc = timelib_SUCCESS
    
      end subroutine timelib_ClockGetAdvanceCount

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: timelib_ClockGetTimeStep - Get a clock's timestep interval

! !INTERFACE:
      subroutine timelib_ClockGetTimeStep(clock, TimeStep, rc)

! !ARGUMENTS:
      type(timelib_Clock), intent(in) :: clock
      type(timelib_TimeInterval), intent(out) :: TimeStep
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt timelib\_Clock}'s timestep interval
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the time step from
!     \item[TimeStep]
!          The time step
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.2
!EOP

      TimeStep = clock%clockint%TimeStep
      IF ( PRESENT(rc) ) rc = timelib_SUCCESS
    
      end subroutine timelib_ClockGetTimeStep

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: timelib_ClockSetTimeStep - Set a clock's timestep interval

! !INTERFACE:
      subroutine timelib_ClockSetTimeStep(clock, TimeStep, rc)

! !ARGUMENTS:
      type(timelib_Clock), intent(inout) :: clock  ! really INTENT(OUT)
      type(timelib_TimeInterval), intent(in) :: TimeStep
      integer, intent(out), optional      :: rc

! !DESCRIPTION:
!     Set an {\tt timelib\_Clock}'s timestep interval
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to set the time step
!     \item[TimeStep]
!          The time step
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.4.2
!EOP

      clock%clockint%TimeStep = TimeStep
      IF ( PRESENT(rc) ) rc = timelib_SUCCESS

      end subroutine timelib_ClockSetTimeStep

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: timelib_ClockGetCurrTime - Get a clock's current time

! !INTERFACE:
      subroutine timelib_ClockGetCurrTime(clock, CurrTime, rc)

! !ARGUMENTS:
      type(timelib_Clock), intent(in) :: clock
      type(timelib_Time), intent(out) :: CurrTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt timelib\_Clock}'s current time     
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the current time from
!     \item[CurrTime]
!          The current time
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.4
!EOP

      CurrTime = clock%clockint%CurrTime
      IF ( PRESENT(rc) ) rc = timelib_SUCCESS
      end subroutine timelib_ClockGetCurrTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: timelib_ClockSetCurrTime - Set a clock's current time

! !INTERFACE:
      subroutine timelib_ClockSetCurrTime(clock, CurrTime, rc)

! !ARGUMENTS:
      type(timelib_Clock), intent(inout) :: clock  ! really INTENT(OUT)
      type(timelib_Time), intent(in) :: CurrTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set an {\tt timelib\_Clock}'s current time
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to set the current time from
!     \item[CurrTime]
!          The current time
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.4.3
!EOP

      clock%clockint%CurrTime = CurrTime
      IF ( PRESENT(rc) ) rc = timelib_SUCCESS
    
      end subroutine timelib_ClockSetCurrTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: timelib_ClockGetStartTime - Get a clock's start time

! !INTERFACE:
      subroutine timelib_ClockGetStartTime(clock, StartTime, rc)

! !ARGUMENTS:
      type(timelib_Clock), intent(in) :: clock
      type(timelib_Time), intent(out) :: StartTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt timelib\_Clock}'s start time
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the start time from
!     \item[StartTime]
!          The start time
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.3
!EOP

      StartTime = clock%clockint%StartTime
      IF ( PRESENT(rc) ) rc = timelib_SUCCESS
    
      end subroutine timelib_ClockGetStartTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: timelib_ClockGetStopTime - Get a clock's stop time

! !INTERFACE:
      subroutine timelib_ClockGetStopTime(clock, StopTime, rc)

! !ARGUMENTS:
      type(timelib_Clock), intent(in) :: clock
      type(timelib_Time), intent(out) :: StopTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt timelib\_Clock}'s stop time
! 
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the stop time from
!     \item[StopTime]
!          The stop time
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.3
!EOP

      StopTime = clock%clockint%StopTime
      IF ( PRESENT(rc) ) rc = timelib_SUCCESS
    
      end subroutine timelib_ClockGetStopTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: timelib_ClockGetRefTime - Get a clock's reference time

! !INTERFACE:
      subroutine timelib_ClockGetRefTime(clock, RefTime, rc)

! !ARGUMENTS:
      type(timelib_Clock), intent(in) :: clock
      type(timelib_Time), intent(out) :: RefTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt timelib\_Clock}'s reference time
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the reference time from
!     \item[RefTime]
!          The reference time
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.3
!EOP
      refTime = clock%clockint%RefTime
      IF ( PRESENT(rc) ) rc = timelib_SUCCESS
      end subroutine timelib_ClockGetRefTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: timelib_ClockGetPrevTime - Get a clock's previous current time

! !INTERFACE:
      subroutine timelib_ClockGetPrevTime(clock, PrevTime, rc)

! !ARGUMENTS:
      type(timelib_Clock), intent(in) :: clock
      type(timelib_Time), intent(out) :: PrevTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt timelib\_Clock}'s previous current time
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the previous current time from
!     \item[PrevTime]
!          The previous current time
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.4
!EOP

! hack for bug in PGI 5.1-x
!      prevTime = Clock%clockint%CurrTime - Clock%clockint%TimeStep
      prevTime = timelib_TimeDec( Clock%clockint%CurrTime, &
                               Clock%clockint%TimeStep )

      IF ( PRESENT(rc) ) rc = timelib_SUCCESS
      end subroutine timelib_ClockGetPrevTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: timelib_ClockGetCurrSimTime - Get a clock's current simulation time

! !INTERFACE:
      subroutine timelib_ClockGetCurrSimTime(clock, CurrSimTime, rc)

! !ARGUMENTS:
      type(timelib_Clock), intent(in) :: clock
      type(timelib_TimeInterval), intent(out) :: CurrSimTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt timelib\_Clock}'s current simulation time
! 
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the current simulation time from
!     \item[CurrSimTime]
!          The current simulation time
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.5
!EOP
      CALL wrf_error_fatal( 'timelib_ClockGetCurrSimTime not supported' )
      end subroutine timelib_ClockGetCurrSimTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: timelib_ClockGetPrevSimTime - Get a clock's previous simulation time

! !INTERFACE:
      subroutine timelib_ClockGetPrevSimTime(clock, PrevSimTime, rc)

! !ARGUMENTS:
      type(timelib_Clock), intent(in) :: clock
      type(timelib_TimeInterval), intent(out) :: PrevSimTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt timelib\_Clock}'s previous simulation time
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the previous simulation time from
!     \item[PrevSimTime]
!          The previous simulation time
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.5
!EOP
      CALL wrf_error_fatal( 'timelib_ClockGetPrevSimTime not supported' )
      end subroutine timelib_ClockGetPrevSimTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: timelib_ClockAddAlarm - Add an alarm to a clock's alarm list

! !INTERFACE:
      subroutine timelib_ClockAddAlarm(clock, Alarm, rc)

! !ARGUMENTS:
      type(timelib_Clock), intent(inout) :: clock
      type(timelib_Alarm), intent(inout) :: Alarm
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Add an {\tt timelib\_Alarm} to an {\tt timelib\_Clock}'s {\tt timelib\_Alarm} list
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to add an {\tt timelib\_Alarm} to
!     \item[Alarm]
!          The {\tt timelib\_Alarm} to add to the {\tt timelib\_Clock}'s
!          {\tt timelib\_Alarm} list
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!   
! !REQUIREMENTS:
!     TMG4.1, TMG4.2
!EOP
    
      IF ( PRESENT( rc ) ) rc = timelib_SUCCESS
      clock%clockint%NumAlarms = clock%clockint%NumAlarms + 1
      IF ( clock%clockint%NumAlarms > SIZE (clock%clockint%AlarmList) ) THEN
        CALL wrf_error_fatal ( 'timelib_ClockAddAlarm:  too many alarms' )
      ELSE IF ( .NOT. ASSOCIATED( Alarm%alarmint ) ) THEN
        CALL wrf_error_fatal ( &
               'timelib_ClockAddAlarm:  alarm not created' )
      ELSE
        IF ( Alarm%alarmint%RingTimeSet ) THEN
           Alarm%alarmint%PrevRingTime = Alarm%alarmint%RingTime
        ELSE
!TBH:  This has the nasty side-effect of forcing us to explicitly turn on 
!TBH:  alarms that are created with RingInterval only, if we want them to start 
!TBH:  ringing right away.  And this is done (see 
!TBH:  COMPUTE_VORTEX_CENTER_ALARM).  Straighten this out...  
           Alarm%alarmint%PrevRingTime = clock%clockint%CurrTime
        ENDIF
        Alarm%alarmint%Ringing = .FALSE.

        ! finally, load the alarm into the list
! write(0,*)'timelib_ClockAddAlarm ',clock%clockint%NumAlarms
        clock%clockint%AlarmList(clock%clockint%NumAlarms) = Alarm
      ENDIF
    
      end subroutine timelib_ClockAddAlarm

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: timelib_ClockGetAlarmList - Get a clock's alarm list

! !INTERFACE:
      subroutine timelib_ClockGetAlarmList(clock, AlarmList, rc)

! !ARGUMENTS:
      type(timelib_Clock), intent(in) :: clock
      type(timelib_Alarm), pointer :: AlarmList(:)
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt timelib\_Clock}'s {\tt timelib\_Alarm} list     
!   
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the {\tt timelib\_Alarm} list from
!     \item[AlarmList]
!          The {\tt timelib\_Clock}'s {\tt timelib\_Alarm} list
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!   
! !REQUIREMENTS:
!     TMG4.3
!EOP

      AlarmList => clock%clockint%AlarmList
      IF ( PRESENT(rc) ) rc = timelib_SUCCESS

      end subroutine timelib_ClockGetAlarmList

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: timelib_ClockGetNumAlarms - Get the number of alarms in a clock's alarm list

! !INTERFACE:
      subroutine timelib_ClockGetNumAlarms(clock, NumAlarms, rc)

! !ARGUMENTS:
      type(timelib_Clock), intent(in) :: clock
      integer, intent(out) :: NumAlarms
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get the number of {\tt timelib\_Alarm}s in an {\tt timelib\_Clock}'s
!       {\tt timelib\_Alarm} list     
!   
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the number of {\tt timelib\_Alarm}s from
!     \item[NumAlarms]
!          The number of {\tt timelib\_Alarm}s in the {\tt timelib\_Clock}'s
!            {\tt timelib\_Alarm} list
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!   
! !REQUIREMENTS:
!     TMG4.3
!EOP

      NumAlarms = clock%clockint%NumAlarms
      IF ( PRESENT(rc) ) rc = timelib_SUCCESS
    
      end subroutine timelib_ClockGetNumAlarms

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: timelib_ClockSyncToWallClock - Set clock's current time to wall clock time

! !INTERFACE:
      subroutine timelib_ClockSyncToWallClock(clock, rc)

! !ARGUMENTS:
      type(timelib_Clock), intent(inout) :: clock
      integer, intent(out), optional :: rc
    
! !DESCRIPTION:
!     Set an {\tt timelib\_Clock}'s current time to wall clock time     
!   
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to synchronize to wall clock time
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!   
! !REQUIREMENTS:
!     TMG3.4.5
!EOP
      CALL wrf_error_fatal( 'timelib_ClockSyncToWallClock not supported' )
      end subroutine timelib_ClockSyncToWallClock

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: timelib_ClockAdvance - Advance a clock's current time by one time step

! !INTERFACE:
      subroutine timelib_ClockAdvance(clock, RingingAlarmList, &
                                   NumRingingAlarms, rc)

use timelib_timemod

! !ARGUMENTS:
      type(timelib_Clock), intent(inout) :: clock
      type(timelib_Alarm), dimension(MAX_ALARMS), intent(out), optional :: &
                                        RingingAlarmList
      integer, intent(out), optional :: NumRingingAlarms
      integer, intent(out), optional :: rc
! Local
      logical pred1, pred2, pred3
      integer i, n
      type(timelib_Alarm) :: alarm
      logical :: positive_timestep
!   
! !DESCRIPTION:
!     Advance an {\tt timelib\_Clock}'s current time by one time step
!  
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to advance
!     \item[{[RingingAlarmList]}]
!          Return a list of any ringing alarms after the time step
!     \item[{[NumRingingAlarms]}]
!          The number of ringing alarms returned
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!  
! !REQUIREMENTS:
!     TMG3.4.1
!EOP
! hack for bug in PGI 5.1-x
!      clock%clockint%CurrTime = clock%clockint%CurrTime + &
!                                clock%clockint%TimeStep
      clock%clockint%CurrTime = timelib_TimeInc( clock%clockint%CurrTime, &
                                              clock%clockint%TimeStep )
      positive_timestep = timelib_TimeIntervalIsPositive( clock%clockint%TimeStep )

      IF ( Present(NumRingingAlarms) ) NumRingingAlarms = 0
      clock%clockint%AdvanceCount = clock%clockint%AdvanceCount + 1
      DO i = 1, MAX_ALARMS
        alarm = clock%clockint%AlarmList(i)
        ! TBH:  This is really dangerous.  We need to be able to NULLIFY 
        ! TBH:  alarmint at compile-time (F95 synax) to make this safe.  
!$$$TBH:  see if F95 compile-time pointer-nullification is supported by all 
!$$$TBH:  compilers we support
        IF ( ASSOCIATED( alarm%alarmint ) ) THEN
          IF ( alarm%alarmint%Enabled ) THEN
            IF ( alarm%alarmint%RingIntervalSet ) THEN
              pred1 = .FALSE. ; pred2 = .FALSE. ; pred3 = .FALSE.
              ! alarm cannot ring if clock has passed the alarms stop time
              IF ( alarm%alarmint%StopTimeSet ) THEN
                IF ( positive_timestep ) THEN
! hack for bug in PGI 5.1-x
!                  PRED1 = clock%clockint%CurrTime > alarm%alarmint%StopTime
                  PRED1 = timelib_TimeGT( clock%clockint%CurrTime, &
                                       alarm%alarmint%StopTime )
                ELSE
                  ! in this case time step is negative and stop time is 
                  ! less than start time
!                  PRED1 = clock%clockint%CurrTime < alarm%alarmint%StopTime
                  PRED1 = timelib_TimeLT( clock%clockint%CurrTime, &
                                       alarm%alarmint%StopTime )
                ENDIF
              ENDIF
              ! one-shot alarm:  check for ring time 
! TBH:  Need to remove duplicated code.  Need to enforce only one of 
! TBH:  alarm%alarmint%RingTimeSet or alarm%alarmint%RingIntervalSet ever 
! TBH:  being .TRUE. and simplify the logic.  Also, the simpler 
! TBH:  implementation in the duplicated code below should be sufficient.  
              IF ( alarm%alarmint%RingTimeSet ) THEN
                IF ( positive_timestep ) THEN
! hack for bug in PGI 5.1-x
!                   PRED2 = ( alarm%alarmint%RingTime <= clock%clockint%CurrTime     &
!                          .AND. clock%clockint%CurrTime < alarm%alarmint%RingTime + &
!                                clock%clockint%TimeStep )
                   PRED2 = ( timelib_TimeLE( alarm%alarmint%RingTime,       &
                                          clock%clockint%CurrTime )      &
                             .AND. timelib_TimeLT( clock%clockint%CurrTime, &
                               timelib_TimeInc( alarm%alarmint%RingTime,    &
                                             clock%clockint%TimeStep ) ) )
                ELSE
                  ! in this case time step is negative and stop time is 
                  ! less than start time
! hack for bug in PGI 5.1-x
!                   PRED2 = ( alarm%alarmint%RingTime >= clock%clockint%CurrTime     &
!                          .AND. clock%clockint%CurrTime > alarm%alarmint%RingTime + &
!                                clock%clockint%TimeStep )
                   PRED2 = ( timelib_TimeGE( alarm%alarmint%RingTime,       &
                                          clock%clockint%CurrTime )      &
                             .AND. timelib_TimeGT( clock%clockint%CurrTime, &
                               timelib_TimeInc( alarm%alarmint%RingTime,    &
                                             clock%clockint%TimeStep ) ) )
                ENDIF
              ENDIF
              ! repeating alarm:  check for ring interval
              IF ( alarm%alarmint%RingIntervalSet ) THEN
                IF ( positive_timestep ) THEN
! hack for bug in PGI 5.1-x
!                   PRED3 = ( alarm%alarmint%PrevRingTime + alarm%alarmint%RingInterval <= &
!                             clock%clockint%CurrTime )

                   PRED3 = ( timelib_TimeLE( timelib_TimeInc(                  &
                                          alarm%alarmint%PrevRingTime,   &
                                          alarm%alarmint%RingInterval ), &
                             clock%clockint%CurrTime ) )
                ELSE
                  ! in this case time step is negative and stop time is 
                  ! less than start time
                  ! ring interval must always be positive
! hack for bug in PGI 5.1-x
!                   PRED3 = ( alarm%alarmint%PrevRingTime - alarm%alarmint%RingInterval >= &
!                             clock%clockint%CurrTime )

                   PRED3 = ( timelib_TimeGE( timelib_TimeDec(                  &
                                          alarm%alarmint%PrevRingTime,   &
                                          alarm%alarmint%RingInterval ), &
                             clock%clockint%CurrTime ) )
                ENDIF
              ENDIF
              IF ( (.NOT. pred1) .AND. pred2 ) THEN
                 alarm%alarmint%Ringing = .TRUE.
                 alarm%alarmint%PrevRingTime = clock%clockint%CurrTime
                 alarm%alarmint%RingTimeSet = .FALSE.  !it is a one time alarm, it rang, now let it resort to interval
                 IF ( PRESENT( RingingAlarmList ) .AND. &
                      PRESENT ( NumRingingAlarms ) ) THEN
                   NumRingingAlarms = NumRingingAlarms + 1
                   RingingAlarmList( NumRingingAlarms ) = alarm
                 ENDIF
              ELSE IF ( (.NOT. pred1) .AND. pred3 ) THEN
                 alarm%alarmint%Ringing = .TRUE.
                 IF ( positive_timestep ) THEN
! hack for bug in PGI 5.1-x
!                   IF ( PRED3) alarm%alarmint%PrevRingTime = alarm%alarmint%PrevRingTime + &
!                                                    alarm%alarmint%RingInterval
                   IF ( PRED3 )                                   &
                     alarm%alarmint%PrevRingTime =                &
                       timelib_TimeInc( alarm%alarmint%PrevRingTime, &
                                     alarm%alarmint%RingInterval )
                 ELSE
                   ! in this case time step is negative and stop time is
                   ! less than start time
                   ! ring interval must always be positive
! hack for bug in PGI 5.1-x
!                   IF ( PRED3) alarm%alarmint%PrevRingTime = alarm%alarmint%PrevRingTime - &
!                                                    alarm%alarmint%RingInterval
                   IF ( PRED3 )                                   &
                     alarm%alarmint%PrevRingTime =                &
                       timelib_TimeDec( alarm%alarmint%PrevRingTime, &
                                     alarm%alarmint%RingInterval )
                 ENDIF
                 IF ( PRESENT( RingingAlarmList ) .AND. &
                      PRESENT ( NumRingingAlarms ) ) THEN
                   NumRingingAlarms = NumRingingAlarms + 1
                   RingingAlarmList( NumRingingAlarms ) = alarm
                 ENDIF
              ENDIF
            ELSE IF ( alarm%alarmint%RingTimeSet ) THEN
! TBH:  Need to remove duplicated code.  Need to enforce only one of 
! TBH:  alarm%alarmint%RingTimeSet or alarm%alarmint%RingIntervalSet ever 
! TBH:  being .TRUE. and simplify the logic.  Also, the simpler 
! TBH:  implementation in here should be sufficient.  
              IF ( positive_timestep ) THEN
! hack for bug in PGI 5.1-x
!                IF ( alarm%alarmint%RingTime <= clock%clockint%CurrTime ) THEN
                IF ( timelib_TimeLE( alarm%alarmint%RingTime, &
                                  clock%clockint%CurrTime ) ) THEN
                   alarm%alarmint%RingTimeSet = .FALSE.  !it is a one time alarm, it rang, now let it resort to interval
                   alarm%alarmint%Ringing = .TRUE.
                   alarm%alarmint%PrevRingTime = clock%clockint%CurrTime
                   IF ( PRESENT( RingingAlarmList ) .AND. &
                        PRESENT ( NumRingingAlarms ) ) THEN
                     NumRingingAlarms = NumRingingAlarms + 1
                     RingingAlarmList( NumRingingAlarms ) = alarm
                   ENDIF
                ENDIF
              ELSE
                ! in this case time step is negative and stop time is 
                ! less than start time
! hack for bug in PGI 5.1-x
!                IF ( alarm%alarmint%RingTime >= clock%clockint%CurrTime ) THEN
                IF ( timelib_TimeGE( alarm%alarmint%RingTime, &
                                  clock%clockint%CurrTime ) ) THEN
                   alarm%alarmint%RingTimeSet = .FALSE.  !it is a one time alarm, it rang, now let it resort to interval
                   alarm%alarmint%Ringing = .TRUE.
                   alarm%alarmint%PrevRingTime = clock%clockint%CurrTime
                   IF ( PRESENT( RingingAlarmList ) .AND. &
                        PRESENT ( NumRingingAlarms ) ) THEN
                     NumRingingAlarms = NumRingingAlarms + 1
                     RingingAlarmList( NumRingingAlarms ) = alarm
                   ENDIF
                ENDIF
              ENDIF
            ENDIF
            IF ( alarm%alarmint%StopTimeSet ) THEN
! TBH:  what is this for???  
            ENDIF
          ENDIF
        ENDIF
        clock%clockint%AlarmList(i) = alarm
      ENDDO
      IF ( PRESENT( rc ) ) rc = timelib_SUCCESS
    
      end subroutine timelib_ClockAdvance

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: timelib_ClockStopTimeDisable - NOOP for compatibility with timelib 2.1.0+

! !INTERFACE:
      subroutine timelib_ClockStopTimeDisable(clock, rc)
!
! !ARGUMENTS:
      type(timelib_Clock), intent(in) :: clock
      integer, intent(out), optional :: rc

      rc = timelib_SUCCESS

      end subroutine timelib_ClockStopTimeDisable

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: timelib_ClockIsStopTime - Has the clock reached its stop time ?

! !INTERFACE:
      function timelib_ClockIsStopTime(clock, rc)
!
! !RETURN VALUE:
      logical :: timelib_ClockIsStopTime

! !ARGUMENTS:
      type(timelib_Clock), intent(in) :: clock
      integer, intent(out), optional :: rc
      logical :: positive_timestep

! !DESCRIPTION:
!     Return true if {\tt timelib\_Clock} has reached its stop time, false 
!     otherwise     
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to check
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG3.5.6
!EOP

      positive_timestep = timelib_TimeIntervalIsPositive( clock%clockint%TimeStep )
      IF ( positive_timestep ) THEN
! hack for bug in PGI 5.1-x
!        if ( clock%clockint%CurrTime .GE. clock%clockint%StopTime ) THEN
        if ( timelib_TimeGE( clock%clockint%CurrTime, &
                          clock%clockint%StopTime ) ) THEN
          timelib_ClockIsStopTime = .TRUE.
        else
          timelib_ClockIsStopTime = .FALSE.
        endif
      ELSE
! hack for bug in PGI 5.1-x
!        if ( clock%clockint%CurrTime .LE. clock%clockint%StopTime ) THEN
        if ( timelib_TimeLE( clock%clockint%CurrTime, &
                          clock%clockint%StopTime ) ) THEN
          timelib_ClockIsStopTime = .TRUE.
        else
          timelib_ClockIsStopTime = .FALSE.
        endif
      ENDIF
      IF ( PRESENT( rc ) ) rc = timelib_SUCCESS
    
      end function timelib_ClockIsStopTime

!------------------------------------------------------------------------------
!
! This section defines the overridden Read, Write, Validate and Print methods
! from the timelib_Base class
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: timelib_ClockRead - Restores a clock

! !INTERFACE:
      subroutine timelib_ClockRead(clock, TimeStep, StartTime, StopTime, &
                                RefTime, CurrTime, PrevTime, AdvanceCount, &
                                AlarmList, rc)

! !ARGUMENTS:
      type(timelib_Clock), intent(out) :: clock
      type(timelib_TimeInterval), intent(in) :: TimeStep
      type(timelib_Time), intent(in) :: StartTime
      type(timelib_Time), intent(in) :: StopTime
      type(timelib_Time), intent(in) :: RefTime
      type(timelib_Time), intent(in) :: CurrTime
      type(timelib_Time), intent(in) :: PrevTime
      integer(timelib_KIND_I8), intent(in) :: AdvanceCount
      type(timelib_Alarm), dimension(MAX_ALARMS), intent(in) :: AlarmList
      integer, intent(out), optional :: rc
    
! !DESCRIPTION:
!     Restore an {\tt timelib\_Clock}
!     
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to restore
!     \item[TimeStep]
!          The {\tt timelib\_Clock}'s time step interval
!     \item[StartTime]
!          The {\tt timelib\_Clock}'s starting time
!     \item[StopTime]
!          The {\tt timelib\_Clock}'s stopping time
!     \item[RefTime]
!          The {\tt timelib\_Clock}'s reference time
!     \item[CurrTime]
!          The {\tt timelib\_Clock}'s current time
!     \item[PrevTime]
!          The {\tt timelib\_Clock}'s previous time
!     \item[AdvanceCount]
!          The number of times the {\tt timelib\_Clock} has been advanced
!     \item[AlarmList]
!          The {\tt timelib\_Clock}'s {\tt timelib\_Alarm} list
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!     
! !REQUIREMENTS:
!EOP
      CALL wrf_error_fatal( 'timelib_ClockRead not supported' )
      end subroutine timelib_ClockRead

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: timelib_ClockWrite - Saves a clock

! !INTERFACE:
      subroutine timelib_ClockWrite(clock, TimeStep, StartTime, StopTime, &
                            RefTime, CurrTime, PrevTime, AdvanceCount, &
                            AlarmList, rc)

! !ARGUMENTS:
      type(timelib_Clock), intent(in) :: clock
      type(timelib_TimeInterval), intent(out) :: TimeStep
      type(timelib_Time), intent(out) :: StartTime
      type(timelib_Time), intent(out) :: StopTime
      type(timelib_Time), intent(out) :: RefTime
      type(timelib_Time), intent(out) :: CurrTime
      type(timelib_Time), intent(out) :: PrevTime
      integer(timelib_KIND_I8), intent(out) :: AdvanceCount
      type(timelib_Alarm), dimension(MAX_ALARMS), intent(out) :: AlarmList
      integer, intent(out), optional :: rc
    
! !DESCRIPTION:
!     Save an {\tt timelib\_Clock}
!     
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to save
!     \item[TimeStep]
!          The {\tt timelib\_Clock}'s time step interval
!     \item[StartTime]
!          The {\tt timelib\_Clock}'s starting time
!     \item[StopTime]
!          The {\tt timelib\_Clock}'s stopping time
!     \item[RefTime]
!          The {\tt timelib\_Clock}'s reference time
!     \item[CurrTime]
!          The {\tt timelib\_Clock}'s current time
!     \item[PrevTime]
!          The {\tt timelib\_Clock}'s previous time
!     \item[AdvanceCount]
!          The number of times the {\tt timelib\_Clock} has been advanced
!     \item[AlarmList]
!          The {\tt timelib\_Clock}'s {\tt timelib\_Alarm} list
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!     
! !REQUIREMENTS:
!EOP
      CALL wrf_error_fatal( 'timelib_ClockWrite not supported' )
      end subroutine timelib_ClockWrite

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  timelib_ClockValidate - Validate a Clock's properties

! !INTERFACE:
      subroutine timelib_ClockValidate(clock, opts, rc)

! !ARGUMENTS:
      type(timelib_Clock), intent(in) :: clock
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a validation check on an {\tt timelib\_Clock}'s properties
!
!     The arguments are:  
!     \begin{description}
!     \item[clock]
!          {\tt timelib\_Clock} to validate
!     \item[{[opts]}]
!          Validate options
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description} 
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
      CALL wrf_error_fatal( 'timelib_ClockValidate not supported' )
      end subroutine timelib_ClockValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  timelib_ClockPrint - Print out a Clock's properties

! !INTERFACE:
      subroutine timelib_ClockPrint(clock, opts, rc)

! !ARGUMENTS:
      type(timelib_Clock), intent(in) :: clock
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     To support testing/debugging, print out an {\tt timelib\_Clock}'s
!     properties.
! 
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          {\tt timelib\_Clock} to print out
!     \item[{[opts]}]
!          Print options
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
      CALL wrf_error_fatal( 'timelib_ClockPrint not supported' )
      end subroutine timelib_ClockPrint

!------------------------------------------------------------------------------

      end module timelib_ClockMod
