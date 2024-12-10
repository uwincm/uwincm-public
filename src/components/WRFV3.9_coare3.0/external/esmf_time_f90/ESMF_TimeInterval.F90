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
!     timelib TimeInterval Module
      module timelib_TimeIntervalMod
!
!==============================================================================
!
! This file contains the TimeInterval class definition and all TimeInterval
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <timelib_TimeMgr.inc>
!
!===============================================================================
!BOPI
! !MODULE: timelib_TimeIntervalMod
!
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation
!
! Defines F90 wrapper entry points for corresponding
! C++ implementaion of class {\tt timelibc\_TimeInterval}
!
! See {\tt ../include/timelibc\_TimeInterval.h} for complete description
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from timelib base class
      use timelib_BaseMod

      ! inherit from base time class
      use timelib_BaseTimeMod

      ! associated derived types
      use timelib_FractionMod, only : timelib_Fraction
      use timelib_CalendarMod

      implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! timelib_TimeInterval
!
!     ! F90 class type to match C++ TimeInterval class in size only;
!     !  all dereferencing within class is performed by C++ implementation

      type timelib_TimeInterval
        ! time interval is expressed as basetime
        type(timelib_BaseTime) :: basetime  ! inherit base class
        ! Relative year and month fields support monthly or yearly time 
        ! intervals.  Many operations are undefined when these fields are 
        ! non-zero!  
        INTEGER :: YR                    ! relative year
   !jm Month has no meaning for an interval; get rid of it, 20100319
   !     INTEGER :: MM                    ! relative month
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public timelib_TimeInterval
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
      public timelib_TimeIntervalGet
      public timelib_TimeIntervalSet
      public timelibold_TimeIntervalGetString
      public timelib_TimeIntervalAbsValue
      public timelib_TimeIntervalNegAbsValue

! Required inherited and overridden timelib_Base class methods

!!!!!!!!! added 20051012, JM
!      public WRFADDITION_TimeIntervalDIVQuot 
!!!!!!!!! renamed to simplify testing 20060320, TH
      public timelib_TimeIntervalDIVQuot 

      ! This convenience routine is only used by other modules in 
      ! timelib_time_f90.  
      public timelib_TimeIntervalIsPositive


! !PRIVATE MEMBER FUNCTIONS:
 
! overloaded operator functions
 
      public operator(/)
      private timelib_TimeIntervalQuotI

      public operator(*)
      private timelib_TimeIntervalProdI

! Inherited and overloaded from timelib_BaseTime

      public operator(+)
      private timelib_TimeIntervalSum

      public operator(-)
      private timelib_TimeIntervalDiff

      public operator(.EQ.)
      private timelib_TimeIntervalEQ

      public operator(.NE.)
      private timelib_TimeIntervalNE

      public operator(.LT.)
      private timelib_TimeIntervalLT

      public operator(.GT.)
      private timelib_TimeIntervalGT

      public operator(.LE.)
      private timelib_TimeIntervalLE

      public operator(.GE.)
      private timelib_TimeIntervalGE
!EOPI

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface operator(*)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure timelib_TimeIntervalProdI

! !DESCRIPTION:
!     This interface overloads the * operator for the {\tt timelib\_TimeInterval}
!     class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(/)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure timelib_TimeIntervalQuotI

! !DESCRIPTION:
!     This interface overloads the / operator for the
!     {\tt timelib\_TimeInterval} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(+)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure timelib_TimeIntervalSum

! !DESCRIPTION:
!     This interface overloads the + operator for the
!     {\tt timelib\_TimeInterval} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(-)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure timelib_TimeIntervalDiff

! !DESCRIPTION:
!     This interface overloads the - operator for the
!     {\tt timelib\_TimeInterval} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.EQ.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure timelib_TimeIntervalEQ

! !DESCRIPTION:
!     This interface overloads the .EQ. operator for the
!     {\tt timelib\_TimeInterval} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.NE.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure timelib_TimeIntervalNE

! !DESCRIPTION:
!     This interface overloads the .NE. operator for the
!     {\tt timelib\_TimeInterval} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.LT.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure timelib_TimeIntervalLT

! !DESCRIPTION:
!     This interface overloads the .LT. operator for the
!     {\tt timelib\_TimeInterval} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.GT.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure timelib_TimeIntervalGT

! !DESCRIPTION:
!     This interface overloads the .GT. operator for the
!     {\tt timelib\_TimeInterval} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.LE.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure timelib_TimeIntervalLE

! !DESCRIPTION:
!     This interface overloads the .LE. operator for the
!     {\tt timelib\_TimeInterval} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.GE.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure timelib_TimeIntervalGE

! !DESCRIPTION:
!     This interface overloads the .GE. operator for the
!     {\tt timelib\_TimeInterval} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------

!==============================================================================

      contains

!==============================================================================
!
! Generic Get/Set routines which use F90 optional arguments
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: timelib_TimeIntervalGet - Get value in user-specified units

! !INTERFACE:
      subroutine timelib_TimeIntervalGet(timeinterval, D, d_r8, S, S_i8, Sn, Sd, &
                                      TimeString, rc )

! !ARGUMENTS:
      type(timelib_TimeInterval), intent(in) :: timeinterval
      integer, intent(out), optional :: D
      real(timelib_KIND_R8),     intent(out), optional :: d_r8
      integer(timelib_KIND_I8),  intent(out), optional :: S_i8
      integer, intent(out), optional :: S
      integer, intent(out), optional :: Sn
      integer, intent(out), optional :: Sd
      character*(*), optional, intent(out) :: TimeString
      integer, intent(out), optional :: rc


! !DESCRIPTION:
!     Get the value of the {\tt timelib\_TimeInterval} in units specified by the
!     user via F90 optional arguments.
!
!     Time manager represents and manipulates time internally with integers 
!     to maintain precision.  Hence, user-specified floating point values are
!     converted internally from integers.
!
!     See {\tt ../include/timelibc\_BaseTime.h} and
!     {\tt ../include/timelibc\_TimeInterval.h} for complete description.
!     
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to query
!     \item[{[YY]}]
!          Integer years (>= 32-bit)
!     \item[{[YYl]}]
!          Integer years (large, >= 64-bit)
!     \item[{[MO]}]
!          Integer months (>= 32-bit)
!     \item[{[MOl]}]
!          Integer months (large, >= 64-bit)
!     \item[{[D]}]
!          Integer days (>= 32-bit)
!     \item[{[Dl]}]
!          Integer days (large, >= 64-bit)
!     \item[{[H]}]
!          Integer hours
!     \item[{[M]}]
!          Integer minutes
!     \item[{[S]}]
!          Integer seconds (>= 32-bit)
!     \item[{[Sl]}]
!          Integer seconds (large, >= 64-bit)
!     \item[{[MS]}]
!          Integer milliseconds
!     \item[{[US]}]
!          Integer microseconds
!     \item[{[NS]}]
!          Integer nanoseconds
!     \item[{[d\_]}]
!          Double precision days
!     \item[{[h\_]}]
!          Double precision hours
!     \item[{[m\_]}]
!          Double precision minutes
!     \item[{[s\_]}]
!          Double precision seconds
!     \item[{[ms\_]}]
!          Double precision milliseconds
!     \item[{[us\_]}]
!          Double precision microseconds
!     \item[{[ns\_]}]
!          Double precision nanoseconds
!     \item[{[Sn]}]
!          Integer fractional seconds - numerator
!     \item[{[Sd]}]
!          Integer fractional seconds - denominator
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.1
!
! Added argument to output double precision seconds, S_i8
! William.Gustafson@pnl.gov; 9-May-2008
!
!EOP
      INTEGER(timelib_KIND_I8) :: seconds
      INTEGER :: ierr

      ierr = timelib_SUCCESS
      seconds = timeinterval%basetime%S
      ! note that S is overwritten below (if present) if other args are also 
      ! present
      IF ( PRESENT(S) ) S = seconds
      IF ( PRESENT(S_i8) ) S_i8 = seconds
      IF ( PRESENT( D ) ) THEN
        D = seconds / SECONDS_PER_DAY
        IF ( PRESENT(S) )    S    = MOD( seconds, SECONDS_PER_DAY )
        IF ( PRESENT(S_i8) ) S_i8 = MOD( seconds, SECONDS_PER_DAY )
      ENDIF
      IF ( PRESENT( d_r8 ) ) THEN
        D_r8 = REAL( seconds, timelib_KIND_R8 ) / &
               REAL( SECONDS_PER_DAY, timelib_KIND_R8 )
        IF ( PRESENT(S) )    S    = MOD( seconds, SECONDS_PER_DAY )
        IF ( PRESENT(S_i8) ) S_i8 = MOD( seconds, SECONDS_PER_DAY )
      ENDIF
      IF ( PRESENT(Sn) ) THEN
        Sn = timeinterval%basetime%Sn
      ENDIF
      IF ( PRESENT(Sd) ) THEN
        Sd = timeinterval%basetime%Sd
      ENDIF
      IF ( PRESENT( timeString ) ) THEN
        CALL timelibold_TimeIntervalGetString( timeinterval, timeString, rc=ierr )
      ENDIF
      IF ( PRESENT(rc) ) rc = ierr
    
      end subroutine timelib_TimeIntervalGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: timelib_TimeIntervalSet - Initialize via user-specified unit set

! !INTERFACE:
      subroutine timelib_TimeIntervalSet(timeinterval, YY, YYl, MM, MOl, D, Dl, &
                                      H, M, S, Sl, MS, US, NS, &
                                      d_, h_, m_, s_, ms_, us_, ns_, &
                                      Sn, Sd, rc)

! !ARGUMENTS:
      type(timelib_TimeInterval), intent(out) :: timeinterval
      integer, intent(in), optional :: YY
      integer(timelib_KIND_I8), intent(in), optional :: YYl
      integer, intent(in), optional :: MM
      integer(timelib_KIND_I8), intent(in), optional :: MOl
      integer, intent(in), optional :: D
      integer(timelib_KIND_I8), intent(in), optional :: Dl
      integer, intent(in), optional :: H
      integer, intent(in), optional :: M
      integer, intent(in), optional :: S
      integer(timelib_KIND_I8), intent(in), optional :: Sl
      integer, intent(in), optional :: MS
      integer, intent(in), optional :: US
      integer, intent(in), optional :: NS
      double precision, intent(in), optional :: d_
      double precision, intent(in), optional :: h_
      double precision, intent(in), optional :: m_
      double precision, intent(in), optional :: s_
      double precision, intent(in), optional :: ms_
      double precision, intent(in), optional :: us_
      double precision, intent(in), optional :: ns_
      integer, intent(in), optional :: Sn
      integer, intent(in), optional :: Sd
      integer, intent(out), optional :: rc
      ! locals
      INTEGER :: nfeb

! !DESCRIPTION:
!     Set the value of the {\tt timelib\_TimeInterval} in units specified by
!     the user via F90 optional arguments
!
!     Time manager represents and manipulates time internally with integers 
!     to maintain precision.  Hence, user-specified floating point values are
!     converted internally to integers.
!
!     See {\tt ../include/timelibc\_BaseTime.h} and
!     {\tt ../include/timelibc\_TimeInterval.h} for complete description.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to initialize
!     \item[{[YY]}]
!          Integer number of interval years (>= 32-bit)
!     \item[{[YYl]}]
!          Integer number of interval years (large, >= 64-bit)
!     \item[{[MM]}]
!          Integer number of interval months (>= 32-bit)
!     \item[{[MOl]}]
!          Integer number of interval months (large, >= 64-bit)
!     \item[{[D]}]
!          Integer number of interval days (>= 32-bit)
!     \item[{[Dl]}]
!          Integer number of interval days (large, >= 64-bit)
!     \item[{[H]}]
!          Integer hours
!     \item[{[M]}]
!          Integer minutes
!     \item[{[S]}]
!          Integer seconds (>= 32-bit)
!     \item[{[Sl]}]
!          Integer seconds (large, >= 64-bit)
!     \item[{[MS]}]
!          Integer milliseconds
!     \item[{[US]}]
!          Integer microseconds
!     \item[{[NS]}]
!          Integer nanoseconds
!     \item[{[d\_]}]
!          Double precision days
!     \item[{[h\_]}]
!          Double precision hours
!     \item[{[m\_]}]
!          Double precision minutes
!     \item[{[s\_]}]
!          Double precision seconds
!     \item[{[ms\_]}]
!          Double precision milliseconds
!     \item[{[us\_]}]
!          Double precision microseconds
!     \item[{[ns\_]}]
!          Double precision nanoseconds
!     \item[{[Sn]}]
!          Integer fractional seconds - numerator
!     \item[{[Sd]}]
!          Integer fractional seconds - denominator
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP

      IF ( PRESENT(rc) ) rc = timelib_FAILURE
      ! note that YR and MM are relative
      timeinterval%YR = 0
      IF ( PRESENT( YY ) ) THEN
        timeinterval%YR = YY
      ENDIF
!jm      timeinterval%MM = 0
!jm      IF ( PRESENT( MM ) ) THEN
!jm        timeinterval%MM = MM
!jm      ENDIF
!jm      ! Rollover months to years
!jm      IF      ( abs(timeinterval%MM) .GE. MONTHS_PER_YEAR ) THEN
!jm        timeinterval%YR = timeinterval%YR + timeinterval%MM/MONTHS_PER_YEAR
!jm        timeinterval%MM = mod(timeinterval%MM,MONTHS_PER_YEAR)
!jm      ENDIF

      timeinterval%basetime%S = 0
      ! For 365-day calendar, immediately convert years to days since we know 
      ! how to do it in this case.  
!$$$ replace this hack with something saner...
      IF ( nfeb( 2004 ) == 28 ) THEN
        timeinterval%basetime%S = timeinterval%basetime%S + &
          ( 365_timelib_KIND_I8 * &
            INT( timeinterval%YR, timelib_KIND_I8 ) * SECONDS_PER_DAY )
        timeinterval%YR = 0
      ENDIF
      IF ( PRESENT( D ) ) THEN
        timeinterval%basetime%S = timeinterval%basetime%S + &
          ( SECONDS_PER_DAY * INT( D, timelib_KIND_I8 ) )
      ENDIF
!$$$ Push H,M,S,Sn,Sd,MS down into BaseTime constructor from EVERYWHERE
!$$$ and THEN add timelib scaling behavior when other args are present...  
      IF ( PRESENT( H ) ) THEN
        timeinterval%basetime%S = timeinterval%basetime%S + &
          ( SECONDS_PER_HOUR * INT( H, timelib_KIND_I8 ) )
      ENDIF
      IF ( PRESENT( M ) ) THEN
        timeinterval%basetime%S = timeinterval%basetime%S + &
          ( SECONDS_PER_MINUTE * INT( M, timelib_KIND_I8 ) )
      ENDIF
      IF ( PRESENT( S ) ) THEN
        timeinterval%basetime%S = timeinterval%basetime%S + &
          INT( S, timelib_KIND_I8 )
      ENDIF
      IF ( PRESENT( Sn ) .AND. ( .NOT. PRESENT( Sd ) ) ) THEN
        CALL wrf_error_fatal( &
          "timelib_TimeIntervalSet:  Must specify Sd if Sn is specified")
      ENDIF
      IF ( PRESENT( Sd ) .AND. PRESENT( MS ) ) THEN
        CALL wrf_error_fatal( &
          "timelib_TimeIntervalSet:  Must not specify both Sd and MS")
      ENDIF
      timeinterval%basetime%Sn = 0
      timeinterval%basetime%Sd = 0
      IF ( PRESENT( MS ) ) THEN
        timeinterval%basetime%Sn = MS
        timeinterval%basetime%Sd = 1000_timelib_KIND_I8
      ELSE IF ( PRESENT( Sd ) ) THEN
        timeinterval%basetime%Sd = Sd
        IF ( PRESENT( Sn ) ) THEN
          timeinterval%basetime%Sn = Sn
        ENDIF
      ENDIF
      CALL normalize_timeint( timeinterval )

      IF ( PRESENT(rc) ) rc = timelib_SUCCESS

      end subroutine timelib_TimeIntervalSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  timelibold_TimeIntervalGetString - Get time interval value in string format

! !INTERFACE:
      subroutine timelibold_TimeIntervalGetString(timeinterval, TimeString, rc)

! !ARGUMENTS:
      type(timelib_TimeInterval), intent(in) :: timeinterval
      character*(*),  intent(out) :: TimeString
      integer, intent(out), optional :: rc
      ! locals
      integer :: signnormtimeint
      LOGICAL :: negative
      INTEGER(timelib_KIND_I8) :: iS, iSn, iSd, H, M, S
      character (len=1) :: signstr

! !DESCRIPTION:
!     Convert {\tt timelib\_TimeInterval}'s value into string format
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to convert
!     \item[TimeString]
!          The string to return
!     \item[{[rc]}]
!          Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.9
!EOP

! NOTE:  YR, MM, Sn, and Sd are not yet included in the returned string...  
!PRINT *,'DEBUG timelibold_TimeIntervalGetString():  YR,MM,S,Sn,Sd = ', &
!        timeinterval%YR, &
!        timeinterval%MM, &
!        timeinterval%basetime%S, &
!        timeinterval%basetime%Sn, &
!        timeinterval%basetime%Sd

      negative = ( signnormtimeint( timeInterval ) == -1 )
      IF ( negative ) THEN
        iS = -timeinterval%basetime%S
        iSn = -timeinterval%basetime%Sn
        signstr = '-'
      ELSE
        iS = timeinterval%basetime%S
        iSn = timeinterval%basetime%Sn
        signstr = ''
      ENDIF 
      iSd = timeinterval%basetime%Sd

      H = mod( iS, SECONDS_PER_DAY ) / SECONDS_PER_HOUR
      M = mod( iS, SECONDS_PER_HOUR) / SECONDS_PER_MINUTE
      S = mod( iS, SECONDS_PER_MINUTE )

!$$$here...  need to print Sn and Sd when they are used ???

      write(TimeString,FMT="(A,I10.10,'_',I3.3,':',I3.3,':',I3.3)") &
        TRIM(signstr), ( iS / SECONDS_PER_DAY ), H, M, S

!write(0,*)'TimeIntervalGetString Sn ',timeinterval%basetime%Sn,' Sd ',timeinterval%basetime%Sd

      rc = timelib_SUCCESS

      end subroutine timelibold_TimeIntervalGetString

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  timelib_TimeIntervalAbsValue - Get the absolute value of a time interval

! !INTERFACE:
      function timelib_TimeIntervalAbsValue(timeinterval)

! !RETURN VALUE:
      type(timelib_TimeInterval) :: timelib_TimeIntervalAbsValue

! !ARGUMENTS:
      type(timelib_TimeInterval), intent(in) :: timeinterval
! !LOCAL:
      integer    :: rc

! !DESCRIPTION:
!     Return a {\tt timelib\_TimeInterval}'s absolute value.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to take the absolute value of.
!          Absolute value returned as value of function.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.8
!EOP
      CALL timeintchecknormalized( timeinterval, 'timelib_TimeIntervalAbsValue arg1' )
      timelib_TimeIntervalAbsValue = timeinterval
!$$$here...  move implementation into BaseTime
      timelib_TimeIntervalAbsValue%basetime%S  = &
        abs(timelib_TimeIntervalAbsValue%basetime%S)
      timelib_TimeIntervalAbsValue%basetime%Sn = &
        abs(timelib_TimeIntervalAbsValue%basetime%Sn )

      end function timelib_TimeIntervalAbsValue

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  timelib_TimeIntervalNegAbsValue - Get the negative absolute value of a time interval

! !INTERFACE:
      function timelib_TimeIntervalNegAbsValue(timeinterval)

! !RETURN VALUE:
      type(timelib_TimeInterval) :: timelib_TimeIntervalNegAbsValue

! !ARGUMENTS:
      type(timelib_TimeInterval), intent(in) :: timeinterval
! !LOCAL:
      integer    :: rc

! !DESCRIPTION:
!     Return a {\tt timelib\_TimeInterval}'s negative absolute value.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to take the negative absolute value of.
!          Negative absolute value returned as value of function.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.8
!EOP
      CALL timeintchecknormalized( timeinterval, 'timelib_TimeIntervalNegAbsValue arg1' )
    
      timelib_TimeIntervalNegAbsValue = timeinterval
!$$$here...  move implementation into BaseTime
      timelib_TimeIntervalNegAbsValue%basetime%S  = &
        -abs(timelib_TimeIntervalNegAbsValue%basetime%S)
      timelib_TimeIntervalNegAbsValue%basetime%Sn = &
        -abs(timelib_TimeIntervalNegAbsValue%basetime%Sn )

      end function timelib_TimeIntervalNegAbsValue

!------------------------------------------------------------------------------
!
! This section includes overloaded operators defined only for TimeInterval
! (not inherited from BaseTime)
! Note:  these functions do not have a return code, since F90 forbids more
! than 2 arguments for arithmetic overloaded operators
!
!------------------------------------------------------------------------------

!!!!!!!!!!!!!!!!!! added jm 20051012
! new WRF-specific function, Divide two time intervals and return the whole integer, without remainder
      function timelib_TimeIntervalDIVQuot(timeinterval1, timeinterval2)

! !RETURN VALUE:
      INTEGER :: timelib_TimeIntervalDIVQuot 

! !ARGUMENTS:
      type(timelib_TimeInterval), intent(in) :: timeinterval1
      type(timelib_TimeInterval), intent(in) :: timeinterval2

! !LOCAL
      INTEGER :: retval, isgn, rc
      type(timelib_TimeInterval) :: zero, i1,i2

! !DESCRIPTION:
!     Returns timeinterval1 divided by timeinterval2 as a fraction quotient.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          The dividend
!     \item[timeinterval2]
!          The divisor
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.5
!EOP

      CALL timeintchecknormalized( timeinterval1, 'timelib_TimeIntervalDIVQuot arg1' )
      CALL timeintchecknormalized( timeinterval2, 'timelib_TimeIntervalDIVQuot arg2' )

      call timelib_TimeIntervalSet( zero, rc=rc )
      i1 = timeinterval1
      i2 = timeinterval2
      isgn = 1
      if ( i1 .LT. zero ) then
        i1 = timelib_TimeIntervalProdI(i1, -1)
        isgn = -isgn
      endif
      if ( i2 .LT. zero ) then
        i2 = timelib_TimeIntervalProdI(i2, -1)
        isgn = -isgn
      endif
! repeated subtraction
      retval = 0
      DO WHILE (  i1 .GE. i2 )
        i1 = i1 - i2
        retval = retval + 1
      ENDDO
      retval = retval * isgn

      timelib_TimeIntervalDIVQuot = retval

      end function timelib_TimeIntervalDIVQuot
!!!!!!!!!!!!!!!!!!



!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  timelib_TimeIntervalQuotI - Divide time interval by an integer, return time interval result 

! !INTERFACE:
      function timelib_TimeIntervalQuotI(timeinterval, divisor)

! !RETURN VALUE:
      type(timelib_TimeInterval) :: timelib_TimeIntervalQuotI

! !ARGUMENTS:
      type(timelib_TimeInterval), intent(in) :: timeinterval
      integer, intent(in) :: divisor

! !DESCRIPTION:
!     Divides a {\tt timelib\_TimeInterval} by an integer divisor, returns
!     quotient as a {\tt timelib\_TimeInterval}
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The dividend
!     \item[divisor]
!          Integer divisor
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.6, TMG5.3, TMG7.2
!EOP

!PRINT *,'DEBUG timelib_TimeIntervalQuotI() A:  S,Sn,Sd = ', &
!  timeinterval%basetime%S,timeinterval%basetime%Sn,timeinterval%basetime%Sd
!PRINT *,'DEBUG timelib_TimeIntervalQuotI() A:  divisor = ', divisor

      CALL timeintchecknormalized( timeinterval, 'timelib_TimeIntervalQuotI arg1' )

      IF ( divisor == 0 ) THEN
        CALL wrf_error_fatal( 'timelib_TimeIntervalQuotI:  divide by zero' )
      ENDIF
      timelib_TimeIntervalQuotI = timeinterval
!PRINT *,'DEBUG timelib_TimeIntervalQuotI() B:  S,Sn,Sd = ', &
!  timelib_TimeIntervalQuotI%basetime%S,timelib_TimeIntervalQuotI%basetime%Sn,timelib_TimeIntervalQuotI%basetime%Sd
      timelib_TimeIntervalQuotI%basetime = &
        timeinterval%basetime / divisor
!PRINT *,'DEBUG timelib_TimeIntervalQuotI() C:  S,Sn,Sd = ', &
!  timelib_TimeIntervalQuotI%basetime%S,timelib_TimeIntervalQuotI%basetime%Sn,timelib_TimeIntervalQuotI%basetime%Sd

      CALL normalize_timeint( timelib_TimeIntervalQuotI )
!PRINT *,'DEBUG timelib_TimeIntervalQuotI() D:  S,Sn,Sd = ', &
!  timelib_TimeIntervalQuotI%basetime%S,timelib_TimeIntervalQuotI%basetime%Sn,timelib_TimeIntervalQuotI%basetime%Sd

      end function timelib_TimeIntervalQuotI

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:   timelib_TimeIntervalProdI - Multiply a time interval by an integer

! !INTERFACE:
      function timelib_TimeIntervalProdI(timeinterval, multiplier)

! !RETURN VALUE:
      type(timelib_TimeInterval) :: timelib_TimeIntervalProdI

! !ARGUMENTS:
      type(timelib_TimeInterval), intent(in) :: timeinterval
      integer, intent(in) :: multiplier
! !LOCAL:
      integer    :: rc

! !DESCRIPTION:
!     Multiply a {\tt timelib\_TimeInterval} by an integer, return product as a
!     {\tt timelib\_TimeInterval}
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The multiplicand
!     \item[mutliplier]
!          Integer multiplier
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.7, TMG7.2
!EOP
      CALL timeintchecknormalized( timeinterval, 'timelib_TimeIntervalProdI arg1' )

      CALL timelib_TimeIntervalSet( timelib_TimeIntervalProdI, rc=rc )
!$$$move this into overloaded operator(*) in BaseTime
      timelib_TimeIntervalProdI%basetime%S  = &
        timeinterval%basetime%S * INT( multiplier, timelib_KIND_I8 )
      timelib_TimeIntervalProdI%basetime%Sn = &
        timeinterval%basetime%Sn * INT( multiplier, timelib_KIND_I8 )
      ! Don't multiply Sd
      timelib_TimeIntervalProdI%basetime%Sd = timeinterval%basetime%Sd
      CALL normalize_timeint( timelib_TimeIntervalProdI )

      end function timelib_TimeIntervalProdI

!------------------------------------------------------------------------------
!
! This section includes the inherited timelib_BaseTime class overloaded operators
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  timelib_TimeIntervalSum - Add two time intervals together

! !INTERFACE:
      function timelib_TimeIntervalSum(timeinterval1, timeinterval2)

! !RETURN VALUE:
      type(timelib_TimeInterval) :: timelib_TimeIntervalSum

! !ARGUMENTS:
      type(timelib_TimeInterval), intent(in) :: timeinterval1
      type(timelib_TimeInterval), intent(in) :: timeinterval2
! !LOCAL:
      integer                             :: rc
! !DESCRIPTION:
!     Add two {\tt timelib\_TimeIntervals}, return sum as a
!     {\tt timelib\_TimeInterval}.  Maps overloaded (+) operator interface
!     function to {\tt timelib\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          The augend 
!     \item[timeinterval2]
!          The addend
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.4, TMG2.4.4, TMG2.4.5, TMG2.4.6, TMG5.1, TMG5.2, 
!                 TMG7.2
!EOP
      CALL timeintchecknormalized( timeinterval1, 'timelib_TimeIntervalSum arg1' )
      CALL timeintchecknormalized( timeinterval2, 'timelib_TimeIntervalSum arg2' )

      timelib_TimeIntervalSum = timeinterval1
      timelib_TimeIntervalSum%basetime = timelib_TimeIntervalSum%basetime + &
                                      timeinterval2%basetime

      CALL normalize_timeint( timelib_TimeIntervalSum )

      end function timelib_TimeIntervalSum

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  timelib_TimeIntervalDiff - Subtract one time interval from another
   
! !INTERFACE:
      function timelib_TimeIntervalDiff(timeinterval1, timeinterval2)

! !RETURN VALUE:
      type(timelib_TimeInterval) :: timelib_TimeIntervalDiff

! !ARGUMENTS: 
      type(timelib_TimeInterval), intent(in) :: timeinterval1
      type(timelib_TimeInterval), intent(in) :: timeinterval2
! !LOCAL:
      integer                             :: rc
! !DESCRIPTION:
!     Subtract timeinterval2 from timeinterval1, return remainder as a 
!     {\tt timelib\_TimeInterval}.
!     Map overloaded (-) operator interface function to {\tt timelib\_BaseTime}
!     base class.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          The minuend 
!     \item[timeinterval2]
!          The subtrahend
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.4, TMG2.4.4, TMG2.4.5, TMG2.4.6, TMG5.1, TMG5.2, TMG7.2
!EOP
      CALL timeintchecknormalized( timeinterval1, 'timelib_TimeIntervalDiff arg1' )
      CALL timeintchecknormalized( timeinterval2, 'timelib_TimeIntervalDiff arg2' )

      timelib_TimeIntervalDiff = timeinterval1
      timelib_TimeIntervalDiff%basetime = timelib_TimeIntervalDiff%basetime - &
                                       timeinterval2%basetime
      CALL normalize_timeint( timelib_TimeIntervalDiff )

      end function timelib_TimeIntervalDiff

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: timelib_TimeIntervalEQ - Compare two time intervals for equality

! !INTERFACE:
      function timelib_TimeIntervalEQ(timeinterval1, timeinterval2)
!
! !RETURN VALUE:
      logical :: timelib_TimeIntervalEQ

! !ARGUMENTS:
      type(timelib_TimeInterval), intent(in) :: timeinterval1
      type(timelib_TimeInterval), intent(in) :: timeinterval2

!DESCRIPTION:
!     Return true if both given time intervals are equal, false otherwise.
!     Maps overloaded (==) operator interface function to {\tt timelib\_BaseTime}
!     base class.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          First time interval to compare
!     \item[timeinterval2]
!          Second time interval to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP
      CALL timeintchecknormalized( timeinterval1, 'timelib_TimeIntervalEQ arg1' )
      CALL timeintchecknormalized( timeinterval2, 'timelib_TimeIntervalEQ arg2' )

!$$$here...  move all this out of Meat.F90 ?  
      ! call timelibc_BaseTime base class function
      call c_timelibc_BaseTimeIntEQ(timeinterval1, timeinterval2, timelib_TimeIntervalEQ)

      end function timelib_TimeIntervalEQ

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  timelib_TimeIntervalNE - Compare two time intervals for inequality

! !INTERFACE:
      function timelib_TimeIntervalNE(timeinterval1, timeinterval2)
!
! !RETURN VALUE:
      logical :: timelib_TimeIntervalNE

! !ARGUMENTS:
      type(timelib_TimeInterval), intent(in) :: timeinterval1
      type(timelib_TimeInterval), intent(in) :: timeinterval2

! !DESCRIPTION:
!     Return true if both given time intervals are not equal, false otherwise.
!     Maps overloaded (/=) operator interface function to {\tt timelib\_BaseTime}
!     base class.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          First time interval to compare
!     \item[timeinterval2]
!          Second time interval to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP
      CALL timeintchecknormalized( timeinterval1, 'timelib_TimeIntervalNE arg1' )
      CALL timeintchecknormalized( timeinterval2, 'timelib_TimeIntervalNE arg2' )

      ! call timelibc_BaseTime base class function
      call c_timelibc_BaseTimeIntNE(timeinterval1, timeinterval2, timelib_TimeIntervalNE)

      end function timelib_TimeIntervalNE

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  timelib_TimeIntervalLT - Time interval 1 less than time interval 2 ?

! !INTERFACE:
      function timelib_TimeIntervalLT(timeinterval1, timeinterval2)
!
! !RETURN VALUE:
      logical :: timelib_TimeIntervalLT

! !ARGUMENTS:
      type(timelib_TimeInterval), intent(in) :: timeinterval1
      type(timelib_TimeInterval), intent(in) :: timeinterval2

! !DESCRIPTION:
!     Return true if first time interval is less than second time interval,
!     false otherwise. Maps overloaded (<) operator interface function to
!     {\tt timelib\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          First time interval to compare
!     \item[timeinterval2]
!          Second time interval to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP
      CALL timeintchecknormalized( timeinterval1, 'timelib_TimeIntervalLT arg1' )
      CALL timeintchecknormalized( timeinterval2, 'timelib_TimeIntervalLT arg2' )

      ! call timelibc_BaseTime base class function
      call c_timelibc_BaseTimeIntLT(timeinterval1, timeinterval2, timelib_TimeIntervalLT)

      end function timelib_TimeIntervalLT

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  timelib_TimeIntervalGT - Time interval 1 greater than time interval 2?

! !INTERFACE:
      function timelib_TimeIntervalGT(timeinterval1, timeinterval2)
!
! !RETURN VALUE:
      logical :: timelib_TimeIntervalGT

! !ARGUMENTS:
      type(timelib_TimeInterval), intent(in) :: timeinterval1
      type(timelib_TimeInterval), intent(in) :: timeinterval2

! !DESCRIPTION:
!     Return true if first time interval is greater than second time interval,
!     false otherwise.  Maps overloaded (>) operator interface function to
!     {\tt timelib\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          First time interval to compare
!     \item[timeinterval2]
!          Second time interval to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP
      CALL timeintchecknormalized( timeinterval1, 'timelib_TimeIntervalGT arg1' )
      CALL timeintchecknormalized( timeinterval2, 'timelib_TimeIntervalGT arg2' )

      ! call timelibc_BaseTime base class function
      call c_timelibc_BaseTimeIntGT(timeinterval1, timeinterval2, timelib_TimeIntervalGT)

      end function timelib_TimeIntervalGT

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  timelib_TimeIntervalLE - Time interval 1 less than or equal to time interval 2 ?

! !INTERFACE:
      function timelib_TimeIntervalLE(timeinterval1, timeinterval2)

! !RETURN VALUE:
      logical :: timelib_TimeIntervalLE

! !ARGUMENTS:
      type(timelib_TimeInterval), intent(in) :: timeinterval1
      type(timelib_TimeInterval), intent(in) :: timeinterval2

! !DESCRIPTION:
!     Return true if first time interval is less than or equal to second time
!     interval, false otherwise.
!     Maps overloaded (<=) operator interface function to {\tt timelib\_BaseTime}
!     base class.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          First time interval to compare
!     \item[timeinterval2]
!          Second time interval to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP
      CALL timeintchecknormalized( timeinterval1, 'timelib_TimeIntervalLE arg1' )
      CALL timeintchecknormalized( timeinterval2, 'timelib_TimeIntervalLE arg2' )

      ! call timelibc_BaseTime base class function
      call c_timelibc_BaseTimeIntLE(timeinterval1, timeinterval2, timelib_TimeIntervalLE)

      end function timelib_TimeIntervalLE

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  timelib_TimeIntervalGE - Time interval 1 greater than or equal to time interval 2 ?

! !INTERFACE:
      function timelib_TimeIntervalGE(timeinterval1, timeinterval2)
!
! !RETURN VALUE:
      logical :: timelib_TimeIntervalGE

! !ARGUMENTS:
      type(timelib_TimeInterval), intent(in) :: timeinterval1
      type(timelib_TimeInterval), intent(in) :: timeinterval2

! !DESCRIPTION:
!     Return true if first time interval is greater than or equal to second
!     time interval, false otherwise. Maps overloaded (>=) operator interface
!     function to {\tt timelib\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          First time interval to compare
!     \item[timeinterval2]
!          Second time interval to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP
      CALL timeintchecknormalized( timeinterval1, 'timelib_TimeIntervalGE arg1' )
      CALL timeintchecknormalized( timeinterval2, 'timelib_TimeIntervalGE arg2' )

      ! call timelibc_BaseTime base class function
      call c_timelibc_BaseTimeIntGE(timeinterval1, timeinterval2, timelib_TimeIntervalGE)

      end function timelib_TimeIntervalGE


!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  timelib_TimeIntervalIsPositive - Time interval greater than zero?

! !INTERFACE:
      function timelib_TimeIntervalIsPositive(timeinterval)
!
! !RETURN VALUE:
      logical :: timelib_TimeIntervalIsPositive

! !ARGUMENTS:
      type(timelib_TimeInterval), intent(in) :: timeinterval

! !LOCALS:
      type(timelib_TimeInterval) :: zerotimeint
      integer :: rcint

! !DESCRIPTION:
!     Return true if time interval is greater than zero,  
!     false otherwise. 
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          Time interval to compare
!     \end{description}
!EOP
      CALL timeintchecknormalized( timeinterval, &
                                   'timelib_TimeIntervalIsPositive arg' )

      CALL timelib_TimeIntervalSet ( zerotimeint, rc=rcint )
      IF ( rcint /= timelib_SUCCESS ) THEN
        CALL wrf_error_fatal( &
          'timelib_TimeIntervalIsPositive:  timelib_TimeIntervalSet failed' )
      ENDIF
! hack for bug in PGI 5.1-x
!      timelib_TimeIntervalIsPositive = timeinterval > zerotimeint
      timelib_TimeIntervalIsPositive = timelib_TimeIntervalGT( timeinterval, &
                                                         zerotimeint )
      end function timelib_TimeIntervalIsPositive

      end module timelib_TimeIntervalMod


