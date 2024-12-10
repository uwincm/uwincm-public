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
!     timelib Calendar Module
      module timelib_CalendarMod
!
!==============================================================================
!
! This file contains the Calendar class definition and all Calendar class
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <timelib_TimeMgr.inc>

!==============================================================================
!BOPI
! !MODULE: timelib_CalendarMod
!
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation
!
! Defines F90 wrapper entry points for corresponding
! C++ class { \tt timelibc\_Calendar} implementation
!
! See {\tt ../include/timelibc\_Calendar.h} for complete description
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from timelib base class
      use timelib_BaseMod

      ! inherit from base time class
      use timelib_BaseTimeMod

      implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------



      INTEGER, PARAMETER :: MONTHS_PER_YEAR = 12
      INTEGER, PARAMETER :: mday(MONTHS_PER_YEAR)   &
                          = (/31,28,31,30,31,30,31,31,30,31,30,31/)
      INTEGER, PARAMETER :: mdayleap(MONTHS_PER_YEAR) &
                          = (/31,29,31,30,31,30,31,31,30,31,30,31/)
      INTEGER, DIMENSION(365) :: daym
      INTEGER, DIMENSION(366) :: daymleap
      INTEGER :: mdaycum(0:MONTHS_PER_YEAR)
      INTEGER :: mdayleapcum(0:MONTHS_PER_YEAR)
      TYPE(timelib_BaseTime), TARGET :: monthbdys(0:MONTHS_PER_YEAR)
      TYPE(timelib_BaseTime), TARGET :: monthbdysleap(0:MONTHS_PER_YEAR)


!------------------------------------------------------------------------------
!     ! timelib_CalendarType
!
!     ! F90 "enum" type to match C++ timelibc_CalendarType enum

      type timelib_CalendarType
      private
        integer :: caltype
      end type

      type(timelib_CalendarType), parameter :: &
                               timelib_CAL_GREGORIAN =  timelib_CalendarType(1), &
                               timelib_CAL_JULIAN =     timelib_CalendarType(2), &
                           ! like Gregorian, except Feb always has 28 days
                               timelib_CAL_NOLEAP =     timelib_CalendarType(3), & 
                           ! 12 months, 30 days each
                               timelib_CAL_360DAY =     timelib_CalendarType(4), & 
                           ! user defined
                               timelib_CAL_GENERIC =    timelib_CalendarType(5), &
                           ! track base time seconds only
                               timelib_CAL_NOCALENDAR = timelib_CalendarType(6)

!------------------------------------------------------------------------------
!     ! timelib_Calendar
!
!     ! F90 class type to match C++ Calendar class in size only;
!     !  all dereferencing within class is performed by C++ implementation
!
!------------------------------------------------------------------------------
!
!     ! timelib_DaysPerYear
!
      type timelib_DaysPerYear
      private
        integer :: D        ! whole days per year
! Fractional days-per-year are not yet used in this implementation.  
!        integer :: Dn       ! fractional days per year numerator
!        integer :: Dd       ! fractional days per year denominator
      end type              ! e.g. for Venus, D=0, Dn=926, Dd=1000
!
!------------------------------------------------------------------------------
!     ! timelib_Calendar
!
!
      type timelib_Calendar
      private
        type(timelib_CalendarType) :: Type
! TBH:  When NO_DT_COMPONENT_INIT is set, code that uses F95 compile-time 
! TBH:  initialization of components of derived types is not included.  
! TBH:  Some older compilers, like PGI 5.x do not support this F95 feature.  
#ifdef NO_DT_COMPONENT_INIT
        logical :: Set
#else
        logical :: Set = .false.
#endif
        integer, dimension(MONTHS_PER_YEAR) :: DaysPerMonth
        integer :: SecondsPerDay
        integer :: SecondsPerYear
        type(timelib_DaysPerYear) :: DaysPerYear
      end type

!------------------------------------------------------------------------------
! !PUBLIC DATA:
   TYPE(timelib_Calendar), public, save, pointer :: defaultCal   ! Default Calendar


!
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public MONTHS_PER_YEAR
      public mday
      public mdayleap
      public monthbdys
      public monthbdysleap
      public daym
      public daymleap
      public mdaycum
      public mdayleapcum
      public timelib_CalendarType
      public timelib_CAL_GREGORIAN, timelib_CAL_NOLEAP, &
             timelib_CAL_360DAY, timelib_CAL_NOCALENDAR
!      public timelib_CAL_JULIAN
!      public timelib_CAL_GENERIC
      public timelib_Calendar

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
      public timelib_CalendarCreate

! Required inherited and overridden timelib_Base class methods

      public timelib_CalendarInitialized ! Only in this implementation, intended
                                      ! to be private within timelib methods
!EOPI

!==============================================================================

      contains


!==============================================================================
!BOP
! !IROUTINE: timelib_CalendarCreate - Create a new timelib Calendar of built-in type

! !INTERFACE:
      ! Private name; call using timelib_CalendarCreate()
      function timelib_CalendarCreate(name, calendartype, rc)

! !RETURN VALUE:
      type(timelib_Calendar) :: timelib_CalendarCreate

! !ARGUMENTS:
      character (len=*),       intent(in),  optional :: name
      type(timelib_CalendarType), intent(in)            :: calendartype
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Creates and sets a {\tt calendar} to the given built-in
!     {\tt timelib\_CalendarType}. 
!
!     This is a private method; invoke via the public overloaded entry point
!     {\tt timelib\_CalendarCreate()}.
!
!     The arguments are:
!     \begin{description}
!     \item[{[name]}]
!          The name for the newly created calendar.  If not specified, a
!          default unique name will be generated: "CalendarNNN" where NNN
!          is a unique sequence number from 001 to 999.
!     \item[calendartype]
!          The built-in {\tt timelib\_CalendarType}.  Valid values are:
!            {\tt timelib\_CAL\_360DAY}, {\tt timelib\_CAL\_GREGORIAN},
!            {\tt timelib\_CAL\_JULIANDAY}, {\tt timelib\_CAL\_NOCALENDAR}, and
!            {\tt timelib\_CAL\_NOLEAP}.
!          See the "Time Manager Reference" document for a description of
!          each calendar type.
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!    
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
      type(timelib_DaysPerYear) :: dayspy

      if ( present(rc) ) rc = timelib_FAILURE
! Calendar type is hard-coded.  Use timelib library if more flexibility is 
! needed.  
#ifdef NO_LEAP_CALENDAR
      if ( calendartype%caltype  /= timelib_CAL_NOLEAP%caltype ) then
         write(6,*) 'Not a valid calendar type for this implementation'
         write(6,*) 'This implementation only allows timelib_CAL_NOLEAP'
         write(6,*) 'calender type set to     = ', calendartype%caltype
         write(6,*) 'NO_LEAP calendar type is = ', timelib_CAL_NOLEAP%caltype
         return
      end if
      timelib_CalendarCreate%Type = timelib_CAL_NOLEAP
#else
      if ( calendartype%caltype  /= timelib_CAL_GREGORIAN%caltype ) then
         write(6,*) 'Not a valid calendar type for this implementation'
         write(6,*) 'This implementation only allows timelib_CAL_GREGORIAN'
         write(6,*) 'calender type set to     = ', calendartype%caltype
         write(6,*) 'GREGORIAN calendar type is = ', timelib_CAL_GREGORIAN%caltype
         return
      end if
      timelib_CalendarCreate%Type = timelib_CAL_GREGORIAN
#endif
! This is a bug on some systems -- need initial value set by compiler at 
! startup.  
! However, note that some older compilers do not support compile-time 
! initialization of data members of Fortran derived data types.  For example, 
! PGI 5.x compilers do not support this F95 feature.  See 
! NO_DT_COMPONENT_INIT.  
      timelib_CalendarCreate%Set = .true.
      timelib_CalendarCreate%SecondsPerDay = SECONDS_PER_DAY
! DaysPerYear and SecondsPerYear are incorrect for Gregorian calendars...  
      dayspy%D = size(daym)
!TBH:  TODO:  Replace DaysPerYear and SecondsPerYear with methods 
!TBH:  TODO:  since they only make sense for the NO_LEAP calendar!  
      timelib_CalendarCreate%DaysPerYear = dayspy
      timelib_CalendarCreate%SecondsPerYear = timelib_CalendarCreate%SecondsPerDay &
                                       * dayspy%D
!TBH:  TODO:  use mdayleap for leap-year calendar
      timelib_CalendarCreate%DaysPerMonth(:) = mday(:)

      if ( present(rc) ) rc = timelib_SUCCESS

      end function timelib_CalendarCreate


!==============================================================================
!BOP
! !IROUTINE: timelib_CalendarInitialized - check if calendar was created

! !INTERFACE:
      function timelib_CalendarInitialized(calendar)

! !RETURN VALUE:
      logical timelib_CalendarInitialized

! !ARGUMENTS:
      type(timelib_Calendar), intent(in)            :: calendar

! !DESCRIPTION:
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
! Note that return value from this function will be bogus for older compilers 
! that do not support compile-time initialization of data members of Fortran 
! derived data types.  For example, PGI 5.x compilers do not support this F95 
! feature.  At the moment, the call to this fuction is #ifdefd out when the 
! leap-year calendar is used so this is not an issue for WRF (see 
! NO_DT_COMPONENT_INIT).  
        timelib_CalendarInitialized = calendar%set

     end function timelib_CalendarInitialized

      end module timelib_CalendarMod
