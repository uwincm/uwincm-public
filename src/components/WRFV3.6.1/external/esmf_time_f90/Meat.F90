#include <timelib_TimeMgr.inc>

! Factor so abs(Sn) < Sd and ensure that signs of S and Sn match.  
! Also, enforce consistency.  
! YR and MM fields are ignored.  

SUBROUTINE normalize_basetime( basetime )
  USE timelib_basemod
  USE timelib_basetimemod
  IMPLICIT NONE
  TYPE(timelib_BaseTime), INTENT(INOUT) :: basetime
!PRINT *,'DEBUG:  BEGIN normalize_basetime()'
  ! Consistency check...  
  IF ( basetime%Sd < 0 ) THEN
    CALL wrf_error_fatal( &
      'normalize_basetime:  denominator of seconds cannot be negative' )
  ENDIF
  IF ( ( basetime%Sd == 0 ) .AND. ( basetime%Sn .NE. 0 ) ) THEN
    CALL wrf_error_fatal( &
      'normalize_basetime:  denominator of seconds cannot be zero when numerator is non-zero' )
  ENDIF
  ! factor so abs(Sn) < Sd
  IF ( basetime%Sd > 0 ) THEN
    IF ( ABS( basetime%Sn ) .GE. basetime%Sd ) THEN
!PRINT *,'DEBUG:  normalize_basetime() A1:  S,Sn,Sd = ',basetime%S,basetime%Sn,basetime%Sd
      basetime%S = basetime%S + ( basetime%Sn / basetime%Sd )
      basetime%Sn = mod( basetime%Sn, basetime%Sd )
!PRINT *,'DEBUG:  normalize_basetime() A2:  S,Sn,Sd = ',basetime%S,basetime%Sn,basetime%Sd
    ENDIF
    ! change sign of Sn if it does not match S
    IF ( ( basetime%S > 0 ) .AND. ( basetime%Sn < 0 ) ) THEN
!PRINT *,'DEBUG:  normalize_basetime() B1:  S,Sn,Sd = ',basetime%S,basetime%Sn,basetime%Sd
      basetime%S = basetime%S - 1_timelib_KIND_I8
      basetime%Sn = basetime%Sn + basetime%Sd
!PRINT *,'DEBUG:  normalize_basetime() B2:  S,Sn,Sd = ',basetime%S,basetime%Sn,basetime%Sd
    ENDIF
    IF ( ( basetime%S < 0 ) .AND. ( basetime%Sn > 0 ) ) THEN
!PRINT *,'DEBUG:  normalize_basetime() C1:  S,Sn,Sd = ',basetime%S,basetime%Sn,basetime%Sd
      basetime%S = basetime%S + 1_timelib_KIND_I8
      basetime%Sn = basetime%Sn - basetime%Sd
!PRINT *,'DEBUG:  normalize_basetime() C2:  S,Sn,Sd = ',basetime%S,basetime%Sn,basetime%Sd
    ENDIF
  ENDIF
!PRINT *,'DEBUG:  END normalize_basetime()'
END SUBROUTINE normalize_basetime



! A normalized time has time%basetime >= 0, time%basetime less than the current 
! year expressed as a timeInterval, and time%YR can take any value
SUBROUTINE normalize_time( time )
  USE timelib_basemod
  USE timelib_basetimemod
  USE timelib_timemod
  IMPLICIT NONE
  TYPE(timelib_Time), INTENT(INOUT) :: time
  INTEGER(timelib_KIND_I8) :: nsecondsinyear
  ! locals
  TYPE(timelib_BaseTime) :: cmptime, zerotime
  INTEGER :: rc
  LOGICAL :: done

  ! first, normalize basetime
  ! this will force abs(Sn) < Sd and ensure that signs of S and Sn match
  CALL normalize_basetime( time%basetime )

!$$$ add tests for these edge cases

  ! next, underflow negative seconds into YEARS
  ! time%basetime must end up non-negative
!$$$ push this down into timelib_BaseTime constructor
  zerotime%S  = 0
  zerotime%Sn = 0
  zerotime%Sd = 0
  DO WHILE ( time%basetime < zerotime )
    time%YR = time%YR - 1 
!$$$ push this down into timelib_BaseTime constructor
    cmptime%S  = nsecondsinyear( time%YR )
    cmptime%Sn = 0
    cmptime%Sd = 0
    time%basetime = time%basetime + cmptime
  ENDDO

  ! next, overflow seconds into YEARS
  done = .FALSE.
  DO WHILE ( .NOT. done )
!$$$ push this down into timelib_BaseTime constructor
    cmptime%S  = nsecondsinyear( time%YR )
    cmptime%Sn = 0
    cmptime%Sd = 0
    IF ( time%basetime >= cmptime ) THEN
      time%basetime = time%basetime - cmptime
      time%YR = time%YR + 1 
    ELSE
      done = .TRUE.
    ENDIF
  ENDDO
END SUBROUTINE normalize_time



SUBROUTINE normalize_timeint( timeInt )
  USE timelib_basetimemod
  USE timelib_timeintervalmod
  IMPLICIT NONE
  TYPE(timelib_TimeInterval), INTENT(INOUT) :: timeInt

  ! normalize basetime
  ! this will force abs(Sn) < Sd and ensure that signs of S and Sn match
  ! YR and MM are ignored
  CALL normalize_basetime( timeInt%basetime )
END SUBROUTINE normalize_timeint




FUNCTION signnormtimeint ( timeInt )
  ! Compute the sign of a time interval.  
  ! YR and MM fields are *IGNORED*.  
  ! returns 1, 0, or -1 or exits if timeInt fields have inconsistent signs.
  USE timelib_basemod
  USE timelib_basetimemod
  USE timelib_timeintervalmod
  IMPLICIT NONE
  TYPE(timelib_TimeInterval), INTENT(IN) :: timeInt
  INTEGER :: signnormtimeint
  LOGICAL :: positive, negative

  positive = .FALSE.
  negative = .FALSE.
  signnormtimeint = 0
  ! Note that Sd is required to be non-negative.  This is enforced in 
  ! normalize_timeint().  
  ! Note that Sn is required to be zero when Sd is zero.  This is enforced 
  ! in normalize_timeint().  
  IF ( ( timeInt%basetime%S > 0 ) .OR. &
       ( timeInt%basetime%Sn > 0 ) ) THEN
    positive = .TRUE.
  ENDIF
  IF ( ( timeInt%basetime%S < 0 ) .OR. &
       ( timeInt%basetime%Sn < 0 ) ) THEN
    negative = .TRUE.
  ENDIF
  IF ( positive .AND. negative ) THEN
    CALL wrf_error_fatal( &
      'signnormtimeint:  signs of fields cannot be mixed' )
  ELSE IF ( positive ) THEN
    signnormtimeint = 1
  ELSE IF ( negative ) THEN
    signnormtimeint = -1
  ENDIF
END FUNCTION signnormtimeint


! Exits with error message if timeInt is not normalized.  
SUBROUTINE timeintchecknormalized( timeInt, msgstr )
  USE timelib_timeintervalmod
  IMPLICIT NONE
  TYPE(timelib_TimeInterval), INTENT(IN) :: timeInt
  CHARACTER(LEN=*), INTENT(IN) :: msgstr
  ! locals
  CHARACTER(LEN=256) :: outstr
  IF ( ( timeInt%YR /= 0 ) ) THEN
    outstr = 'un-normalized TimeInterval not allowed:  '//TRIM(msgstr)
    CALL wrf_error_fatal( outstr )
  ENDIF
END SUBROUTINE timeintchecknormalized


! added from share/module_date_time in WRF.
FUNCTION nfeb ( year ) RESULT (num_days)
      ! Compute the number of days in February for the given year
      IMPLICIT NONE
      INTEGER :: year
      INTEGER :: num_days
! TBH:  TODO:  Replace this hack with run-time decision based on 
! TBH:  TODO:  passed-in calendar.  
#ifdef NO_LEAP_CALENDAR
      num_days = 28 ! By default, February has 28 days ...
#else
      num_days = 28 ! By default, February has 28 days ...
      IF (MOD(year,4).eq.0) THEN
         num_days = 29  ! But every four years, it has 29 days ...
         IF (MOD(year,100).eq.0) THEN
            num_days = 28  ! Except every 100 years, when it has 28 days ...
            IF (MOD(year,400).eq.0) THEN
               num_days = 29  ! Except every 400 years, when it has 29 days.
            END IF
         END IF
      END IF
#endif
END FUNCTION nfeb



FUNCTION ndaysinyear ( year ) RESULT (num_diy)
  ! Compute the number of days in the given year
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: year
  INTEGER :: num_diy
  INTEGER :: nfeb
#if defined MARS
  num_diy = 669
#elif defined TITAN
  num_diy = 686
#else
  IF ( nfeb( year ) .EQ. 29 ) THEN
    num_diy = 366
  ELSE
    num_diy = 365
  ENDIF
#endif
END FUNCTION ndaysinyear



FUNCTION nsecondsinyear ( year ) RESULT (numseconds)
  ! Compute the number of seconds in the given year
  USE timelib_basemod
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: year
  INTEGER(timelib_KIND_I8) :: numseconds
  INTEGER :: ndaysinyear
  numseconds = SECONDS_PER_DAY * INT( ndaysinyear(year) , timelib_KIND_I8 )
END FUNCTION nsecondsinyear



SUBROUTINE initdaym 
  USE timelib_basemod
  USE timelib_basetimemod
  USE timelib_CalendarMod, only : months_per_year, mday, daym, mdaycum, monthbdys, &
                               mdayleap, mdayleapcum, monthbdysleap, daymleap
  IMPLICIT NONE
  INTEGER i,j,m
  m = 1
  mdaycum(0) = 0
!$$$ push this down into timelib_BaseTime constructor
  monthbdys(0)%S  = 0
  monthbdys(0)%Sn = 0
  monthbdys(0)%Sd = 0
  DO i = 1,MONTHS_PER_YEAR
    DO j = 1,mday(i)
      daym(m) = i
      m = m + 1
    ENDDO
    mdaycum(i) = mdaycum(i-1) + mday(i)
!$$$ push this down into timelib_BaseTime constructor
    monthbdys(i)%S  = SECONDS_PER_DAY * INT( mdaycum(i), timelib_KIND_I8 )
    monthbdys(i)%Sn = 0
    monthbdys(i)%Sd = 0
  ENDDO
  m = 1
  mdayleapcum(0) = 0
!$$$ push this down into timelib_BaseTime constructor
  monthbdysleap(0)%S  = 0
  monthbdysleap(0)%Sn = 0
  monthbdysleap(0)%Sd = 0
  DO i = 1,MONTHS_PER_YEAR
    DO j = 1,mdayleap(i)
      daymleap(m) = i
      m = m + 1
    ENDDO
    mdayleapcum(i) = mdayleapcum(i-1) + mdayleap(i)
!$$$ push this down into timelib_BaseTime constructor
    monthbdysleap(i)%S  = SECONDS_PER_DAY * INT( mdayleapcum(i), timelib_KIND_I8 )
    monthbdysleap(i)%Sn = 0
    monthbdysleap(i)%Sd = 0
  ENDDO
END SUBROUTINE initdaym


!$$$ useful, but not used at the moment...  
SUBROUTINE compute_dayinyear(YR,MM,DD,dayinyear)
  use timelib_CalendarMod, only : mday
IMPLICIT NONE
      INTEGER, INTENT(IN)  :: YR,MM,DD   ! DD is day of month
      INTEGER, INTENT(OUT) :: dayinyear
      INTEGER i
      integer nfeb

#ifdef PLANET
      dayinyear = DD
#else
      dayinyear = 0
      DO i = 1,MM-1
        if (i.eq.2) then
          dayinyear = dayinyear + nfeb(YR)
        else
          dayinyear = dayinyear + mday(i)
        endif
      ENDDO
      dayinyear = dayinyear + DD
#endif
END SUBROUTINE compute_dayinyear



SUBROUTINE timegetmonth( time, MM )
  USE timelib_basemod
  USE timelib_basetimemod
  USE timelib_timemod
  USE timelib_CalendarMod, only : MONTHS_PER_YEAR, monthbdys, monthbdysleap
  IMPLICIT NONE
  TYPE(timelib_Time), INTENT(IN) :: time
  INTEGER, INTENT(OUT) :: MM
  ! locals
  INTEGER :: nfeb
  INTEGER :: i
#if defined PLANET
  MM = 0
#else
  MM = -1
  IF ( nfeb(time%YR) == 29 ) THEN
    DO i = 1,MONTHS_PER_YEAR
      IF ( ( time%basetime >= monthbdysleap(i-1) ) .AND. ( time%basetime < monthbdysleap(i) ) ) THEN
        MM = i
        EXIT
      ENDIF
    ENDDO
  ELSE
    DO i = 1,MONTHS_PER_YEAR
      IF ( ( time%basetime >= monthbdys(i-1) ) .AND. ( time%basetime < monthbdys(i) ) ) THEN
        MM = i
        EXIT
      ENDIF
    ENDDO
  ENDIF
#endif
  IF ( MM == -1 ) THEN
    CALL wrf_error_fatal( 'timegetmonth:  could not extract month of year from time' )
  ENDIF
END SUBROUTINE timegetmonth


!$$$ may need to change dependencies in Makefile...  

SUBROUTINE timegetdayofmonth( time, DD )
  USE timelib_basemod
  USE timelib_basetimemod
  USE timelib_timemod
  USE timelib_calendarmod, only : monthbdys, monthbdysleap
  IMPLICIT NONE
  TYPE(timelib_Time), INTENT(IN) :: time
  INTEGER, INTENT(OUT) :: DD
  ! locals
  INTEGER :: nfeb
  INTEGER :: MM
  TYPE(timelib_BaseTime) :: tmpbasetime
#if defined PLANET
  tmpbasetime = time%basetime
#else
  CALL timegetmonth( time, MM )
  IF ( nfeb(time%YR) == 29 ) THEN
    tmpbasetime = time%basetime - monthbdysleap(MM-1)
  ELSE
    tmpbasetime = time%basetime - monthbdys(MM-1)
  ENDIF
#endif
  DD = ( tmpbasetime%S / SECONDS_PER_DAY ) + 1
END SUBROUTINE timegetdayofmonth


! Increment Time by number of seconds between start of year and start 
! of month MM.  
! 1 <= MM <= 12
! Time is NOT normalized.  
SUBROUTINE timeaddmonths( time, MM, ierr )
  USE timelib_basemod
  USE timelib_basetimemod
  USE timelib_timemod
  USE timelib_calendarmod, only : MONTHS_PER_YEAR, monthbdys, monthbdysleap
  IMPLICIT NONE
  TYPE(timelib_Time), INTENT(INOUT) :: time
  INTEGER, INTENT(IN) :: MM
  INTEGER, INTENT(OUT) :: ierr
  ! locals
  INTEGER :: nfeb
  ierr = timelib_SUCCESS
!  PRINT *,'DEBUG:  BEGIN timeaddmonths()'
#if defined PLANET
!  time%basetime = time%basetime
#else
  IF ( ( MM < 1 ) .OR. ( MM > MONTHS_PER_YEAR ) ) THEN
    ierr = timelib_FAILURE
  ELSE
    IF ( nfeb(time%YR) == 29 ) THEN
      time%basetime = time%basetime + monthbdysleap(MM-1)
    ELSE
      time%basetime = time%basetime + monthbdys(MM-1)
    ENDIF
  ENDIF
#endif
END SUBROUTINE timeaddmonths


! Increment Time by number of seconds in the current month.  
! Time is NOT normalized.  
SUBROUTINE timeincmonth( time )
  USE timelib_basemod
  USE timelib_basetimemod
  USE timelib_timemod
  USE timelib_calendarmod, only : mday, mdayleap
  IMPLICIT NONE
  TYPE(timelib_Time), INTENT(INOUT) :: time
  ! locals
  INTEGER :: nfeb
  INTEGER :: MM
#if defined PLANET
!    time%basetime%S = time%basetime%S
#else
  CALL timegetmonth( time, MM )
  IF ( nfeb(time%YR) == 29 ) THEN
    time%basetime%S = time%basetime%S + &
      ( INT( mdayleap(MM), timelib_KIND_I8 ) * SECONDS_PER_DAY )
  ELSE
    time%basetime%S = time%basetime%S + &
      ( INT( mday(MM), timelib_KIND_I8 ) * SECONDS_PER_DAY )
  ENDIF
#endif
END SUBROUTINE timeincmonth



! Decrement Time by number of seconds in the previous month.  
! Time is NOT normalized.  
SUBROUTINE timedecmonth( time )
  USE timelib_basemod
  USE timelib_basetimemod
  USE timelib_timemod
  USE timelib_calendarmod, only : mday, months_per_year, mdayleap
  IMPLICIT NONE
  TYPE(timelib_Time), INTENT(INOUT) :: time
  ! locals
  INTEGER :: nfeb
  INTEGER :: MM
#if defined PLANET
!    time%basetime%S = time%basetime%S
#else
  CALL timegetmonth( time, MM )  ! current month, 1-12
  ! find previous month
  MM = MM - 1
  IF ( MM == 0 ) THEN
    ! wrap around Jan -> Dec
    MM = MONTHS_PER_YEAR
  ENDIF
  IF ( nfeb(time%YR) == 29 ) THEN
    time%basetime%S = time%basetime%S - &
      ( INT( mdayleap(MM), timelib_KIND_I8 ) * SECONDS_PER_DAY )
  ELSE
    time%basetime%S = time%basetime%S - &
      ( INT( mday(MM), timelib_KIND_I8 ) * SECONDS_PER_DAY )
  ENDIF
#endif
END SUBROUTINE timedecmonth



! spaceship operator for Times
SUBROUTINE timecmp(time1, time2, retval )
  USE timelib_basemod
  USE timelib_basetimemod
  USE timelib_timemod
  IMPLICIT NONE
  INTEGER, INTENT(OUT) :: retval
!
! !ARGUMENTS:
  TYPE(timelib_Time), INTENT(IN) :: time1
  TYPE(timelib_Time), INTENT(IN) :: time2
  IF ( time1%YR .GT. time2%YR ) THEN ; retval = 1 ; RETURN ; ENDIF
  IF ( time1%YR .LT. time2%YR ) THEN ; retval = -1 ; RETURN ; ENDIF
  CALL seccmp( time1%basetime%S, time1%basetime%Sn, time1%basetime%Sd, &
               time2%basetime%S, time2%basetime%Sn, time2%basetime%Sd, &
               retval )
END SUBROUTINE timecmp



! spaceship operator for TimeIntervals
SUBROUTINE timeintcmp(timeint1, timeint2, retval )
  USE timelib_basemod
  USE timelib_basetimemod
  USE timelib_timeintervalmod
  IMPLICIT NONE
  INTEGER, INTENT(OUT) :: retval
!
! !ARGUMENTS:
  TYPE(timelib_TimeInterval), INTENT(IN) :: timeint1
  TYPE(timelib_TimeInterval), INTENT(IN) :: timeint2
  CALL timeintchecknormalized( timeint1, 'timeintcmp arg1' )
  CALL timeintchecknormalized( timeint2, 'timeintcmp arg2' )
  CALL seccmp( timeint1%basetime%S, timeint1%basetime%Sn, &
               timeint1%basetime%Sd,                      &
               timeint2%basetime%S, timeint2%basetime%Sn, &
               timeint2%basetime%Sd, retval )
END SUBROUTINE timeintcmp



! spaceship operator for seconds + Sn/Sd
SUBROUTINE seccmp(S1, Sn1, Sd1, S2, Sn2, Sd2, retval )
  USE timelib_basemod
  IMPLICIT NONE
  INTEGER, INTENT(OUT) :: retval
!
! !ARGUMENTS:
  INTEGER(timelib_KIND_I8), INTENT(IN) :: S1, Sn1, Sd1
  INTEGER(timelib_KIND_I8), INTENT(IN) :: S2, Sn2, Sd2
! local
  INTEGER(timelib_KIND_I8) :: lcd, n1, n2

  n1 = Sn1
  n2 = Sn2
  if ( ( n1 .ne. 0 ) .or. ( n2 .ne. 0 ) ) then
    CALL compute_lcd( Sd1, Sd2, lcd )
    if ( Sd1 .ne. 0 ) n1 = n1 * ( lcd / Sd1 )
    if ( Sd2 .ne. 0 ) n2 = n2 * ( lcd / Sd2 )
  endif

  if ( S1 .GT. S2 ) retval = 1
  if ( S1 .LT. S2 ) retval = -1
  IF ( S1 .EQ. S2 ) THEN
    IF (n1 .GT. n2) retval = 1
    IF (n1 .LT. n2) retval = -1
    IF (n1 .EQ. n2) retval = 0
  ENDIF
END SUBROUTINE seccmp


SUBROUTINE c_timelibc_basetimeeq (time1, time2, outflag)
  USE timelib_alarmmod
  USE timelib_basemod
  USE timelib_basetimemod
  USE timelib_calendarmod
  USE timelib_clockmod
  USE timelib_fractionmod
  USE timelib_timeintervalmod
  USE timelib_timemod
IMPLICIT NONE
      logical, intent(OUT) :: outflag
      type(timelib_Time), intent(in) :: time1
      type(timelib_Time), intent(in) :: time2
      integer res 
      CALL timecmp(time1,time2,res)
      outflag = (res .EQ. 0)
END SUBROUTINE c_timelibc_basetimeeq
SUBROUTINE c_timelibc_basetimege(time1, time2, outflag)
  USE timelib_alarmmod
  USE timelib_basemod
  USE timelib_basetimemod
  USE timelib_calendarmod
  USE timelib_clockmod
  USE timelib_fractionmod
  USE timelib_timeintervalmod
  USE timelib_timemod
      logical, intent(OUT) :: outflag
      type(timelib_Time), intent(in) :: time1
      type(timelib_Time), intent(in) :: time2
      integer res 
      CALL timecmp(time1,time2,res)
      outflag = (res .EQ. 1 .OR. res .EQ. 0)
END SUBROUTINE c_timelibc_basetimege
SUBROUTINE c_timelibc_basetimegt(time1, time2, outflag)
  USE timelib_alarmmod
  USE timelib_basemod
  USE timelib_basetimemod
  USE timelib_calendarmod
  USE timelib_clockmod
  USE timelib_fractionmod
  USE timelib_timeintervalmod
  USE timelib_timemod
IMPLICIT NONE
      logical, intent(OUT) :: outflag
      type(timelib_Time), intent(in) :: time1
      type(timelib_Time), intent(in) :: time2
      integer res 
      CALL timecmp(time1,time2,res)
      outflag = (res .EQ. 1)
END SUBROUTINE c_timelibc_basetimegt
SUBROUTINE c_timelibc_basetimele(time1, time2, outflag)
  USE timelib_alarmmod
  USE timelib_basemod
  USE timelib_basetimemod
  USE timelib_calendarmod
  USE timelib_clockmod
  USE timelib_fractionmod
  USE timelib_timeintervalmod
  USE timelib_timemod
IMPLICIT NONE
      logical, intent(OUT) :: outflag
      type(timelib_Time), intent(in) :: time1
      type(timelib_Time), intent(in) :: time2
      integer res 
      CALL timecmp(time1,time2,res)
      outflag = (res .EQ. -1 .OR. res .EQ. 0)
END SUBROUTINE c_timelibc_basetimele
SUBROUTINE c_timelibc_basetimelt(time1, time2, outflag)
  USE timelib_alarmmod
  USE timelib_basemod
  USE timelib_basetimemod
  USE timelib_calendarmod
  USE timelib_clockmod
  USE timelib_fractionmod
  USE timelib_timeintervalmod
  USE timelib_timemod
IMPLICIT NONE
      logical, intent(OUT) :: outflag
      type(timelib_Time), intent(in) :: time1
      type(timelib_Time), intent(in) :: time2
      integer res 
      CALL timecmp(time1,time2,res)
      outflag = (res .EQ. -1)
END SUBROUTINE c_timelibc_basetimelt
SUBROUTINE c_timelibc_basetimene(time1, time2, outflag)
  USE timelib_alarmmod
  USE timelib_basemod
  USE timelib_basetimemod
  USE timelib_calendarmod
  USE timelib_clockmod
  USE timelib_fractionmod
  USE timelib_timeintervalmod
  USE timelib_timemod
IMPLICIT NONE
      logical, intent(OUT) :: outflag
      type(timelib_Time), intent(in) :: time1
      type(timelib_Time), intent(in) :: time2
      integer res 
      CALL timecmp(time1,time2,res)
      outflag = (res .NE. 0)
END SUBROUTINE c_timelibc_basetimene

SUBROUTINE c_timelibc_basetimeinteq(timeint1, timeint2, outflag)
  USE timelib_timeintervalmod
  IMPLICIT NONE
  LOGICAL, INTENT(OUT) :: outflag
  TYPE(timelib_TimeInterval), INTENT(IN) :: timeint1
  TYPE(timelib_TimeInterval), INTENT(IN) :: timeint2
  INTEGER :: res 
  CALL timeintcmp(timeint1,timeint2,res)
  outflag = (res .EQ. 0)
END SUBROUTINE c_timelibc_basetimeinteq
SUBROUTINE c_timelibc_basetimeintne(timeint1, timeint2, outflag)
  USE timelib_timeintervalmod
  IMPLICIT NONE
  LOGICAL, INTENT(OUT) :: outflag
  TYPE(timelib_TimeInterval), INTENT(IN) :: timeint1
  TYPE(timelib_TimeInterval), INTENT(IN) :: timeint2
  INTEGER :: res 
  CALL timeintcmp(timeint1,timeint2,res)
  outflag = (res .NE. 0)
END SUBROUTINE c_timelibc_basetimeintne
SUBROUTINE c_timelibc_basetimeintlt(timeint1, timeint2, outflag)
  USE timelib_timeintervalmod
  IMPLICIT NONE
  LOGICAL, INTENT(OUT) :: outflag
  TYPE(timelib_TimeInterval), INTENT(IN) :: timeint1
  TYPE(timelib_TimeInterval), INTENT(IN) :: timeint2
  INTEGER :: res 
  CALL timeintcmp(timeint1,timeint2,res)
  outflag = (res .LT. 0)
END SUBROUTINE c_timelibc_basetimeintlt
SUBROUTINE c_timelibc_basetimeintgt(timeint1, timeint2, outflag)
  USE timelib_timeintervalmod
  IMPLICIT NONE
  LOGICAL, INTENT(OUT) :: outflag
  TYPE(timelib_TimeInterval), INTENT(IN) :: timeint1
  TYPE(timelib_TimeInterval), INTENT(IN) :: timeint2
  INTEGER :: res 
  CALL timeintcmp(timeint1,timeint2,res)
  outflag = (res .GT. 0)
END SUBROUTINE c_timelibc_basetimeintgt
SUBROUTINE c_timelibc_basetimeintle(timeint1, timeint2, outflag)
  USE timelib_timeintervalmod
  IMPLICIT NONE
  LOGICAL, INTENT(OUT) :: outflag
  TYPE(timelib_TimeInterval), INTENT(IN) :: timeint1
  TYPE(timelib_TimeInterval), INTENT(IN) :: timeint2
  INTEGER :: res 
  CALL timeintcmp(timeint1,timeint2,res)
  outflag = (res .LE. 0)
END SUBROUTINE c_timelibc_basetimeintle
SUBROUTINE c_timelibc_basetimeintge(timeint1, timeint2, outflag)
  USE timelib_timeintervalmod
  IMPLICIT NONE
  LOGICAL, INTENT(OUT) :: outflag
  TYPE(timelib_TimeInterval), INTENT(IN) :: timeint1
  TYPE(timelib_TimeInterval), INTENT(IN) :: timeint2
  INTEGER :: res 
  CALL timeintcmp(timeint1,timeint2,res)
  outflag = (res .GE. 0)
END SUBROUTINE c_timelibc_basetimeintge

SUBROUTINE compute_lcd( e1, e2, lcd )
  USE timelib_basemod
      IMPLICIT NONE
      INTEGER(timelib_KIND_I8), INTENT(IN) :: e1, e2
      INTEGER(timelib_KIND_I8), INTENT(OUT) :: lcd
      INTEGER, PARAMETER ::  nprimes = 9
      INTEGER(timelib_KIND_I8), DIMENSION(nprimes), PARAMETER :: primes = (/2,3,5,7,11,13,17,19,23/)
      INTEGER i
      INTEGER(timelib_KIND_I8) d1, d2, p

      d1 = e1 ; d2 = e2
      IF ( d1 .EQ. 0 .AND. d2 .EQ. 0 ) THEN ; lcd = 1 ; RETURN ; ENDIF
      IF ( d1 .EQ. 0 ) d1 = d2 
      IF ( d2 .EQ. 0 ) d2 = d1 
      IF ( d1 .EQ. d2 ) THEN ; lcd = d1 ; RETURN ; ENDIF
      lcd = d1 * d2
      DO i = 1, nprimes
        p = primes(i)
        DO WHILE (lcd/p .NE. 0 .AND. &
          mod(lcd/p,d1) .EQ. 0 .AND. mod(lcd/p,d2) .EQ. 0) 
          lcd = lcd / p 
        END DO
      ENDDO
END SUBROUTINE compute_lcd

SUBROUTINE simplify( ni, di, no, do ) 
  USE timelib_basemod
    IMPLICIT NONE
    INTEGER(timelib_KIND_I8), INTENT(IN)  :: ni, di
    INTEGER(timelib_KIND_I8), INTENT(OUT) :: no, do
    INTEGER, PARAMETER ::  nprimes = 9
    INTEGER(timelib_KIND_I8), DIMENSION(nprimes), PARAMETER :: primes = (/2,3,5,7,11,13,17,19,23/)
    INTEGER(timelib_KIND_I8) :: pr, d, n
    INTEGER :: np
    LOGICAL keepgoing
    IF ( ni .EQ. 0 ) THEN
      do = 1
      no = 0
      RETURN
    ENDIF
    IF ( mod( di , ni ) .EQ. 0 ) THEN
      do = di / ni
      no = 1
      RETURN
    ENDIF
    d = di
    n = ni
    DO np = 1, nprimes
      pr = primes(np)
      keepgoing = .TRUE.
      DO WHILE ( keepgoing )
        keepgoing = .FALSE.
        IF ( d/pr .NE. 0 .AND. n/pr .NE. 0 .AND. MOD(d,pr) .EQ. 0 .AND. MOD(n,pr) .EQ. 0 ) THEN
          d = d / pr
          n = n / pr
          keepgoing = .TRUE.
        ENDIF
      ENDDO
    ENDDO
    do = d
    no = n
    RETURN
END SUBROUTINE simplify


!$$$ this should be named "c_timelibc_timesum" or something less misleading
SUBROUTINE c_timelibc_basetimesum( time1, timeinterval, timeOut )
  USE timelib_basemod
  USE timelib_basetimemod
  USE timelib_timeintervalmod
  USE timelib_timemod
  IMPLICIT NONE
  TYPE(timelib_Time), INTENT(IN) :: time1
  TYPE(timelib_TimeInterval), INTENT(IN) :: timeinterval
  TYPE(timelib_Time), INTENT(INOUT) :: timeOut
  ! locals
  INTEGER :: m
  timeOut = time1
  timeOut%basetime = timeOut%basetime + timeinterval%basetime
#if 0
!jm Month has no meaning for a timeinterval; removed 20100319
#if defined PLANET
  ! Do nothing...
#else
 DO m = 1, abs(timeinterval%MM)
    IF ( timeinterval%MM > 0 ) THEN
      CALL timeincmonth( timeOut )
    ELSE
      CALL timedecmonth( timeOut )
    ENDIF
  ENDDO
#endif
#endif
  timeOut%YR = timeOut%YR + timeinterval%YR
  CALL normalize_time( timeOut )
END SUBROUTINE c_timelibc_basetimesum


!$$$ this should be named "c_timelibc_timedec" or something less misleading
SUBROUTINE c_timelibc_basetimedec( time1, timeinterval, timeOut )
  USE timelib_basemod
  USE timelib_basetimemod
  USE timelib_timeintervalmod
  USE timelib_timemod
  IMPLICIT NONE
  TYPE(timelib_Time), INTENT(IN) :: time1
  TYPE(timelib_TimeInterval), INTENT(IN) :: timeinterval
  TYPE(timelib_Time), INTENT(OUT) :: timeOut
  ! locals
  TYPE (timelib_TimeInterval)  :: neginterval 
  neginterval = timeinterval
!$$$push this down into a unary negation operator on TimeInterval
  neginterval%basetime%S = -neginterval%basetime%S
  neginterval%basetime%Sn = -neginterval%basetime%Sn
  neginterval%YR = -neginterval%YR
#if 0
!jm month has no meaning for an interval; removed 20100319
#ifndef PLANET
  neginterval%MM = -neginterval%MM
#endif
#endif
  timeOut = time1 + neginterval
END SUBROUTINE c_timelibc_basetimedec


!$$$ this should be named "c_timelibc_timediff" or something less misleading
SUBROUTINE c_timelibc_basetimediff( time1, time2, timeIntOut )
  USE timelib_basemod
  USE timelib_basetimemod
  USE timelib_timeintervalmod
  USE timelib_timemod
  IMPLICIT NONE
  TYPE(timelib_Time), INTENT(IN) :: time1
  TYPE(timelib_Time), INTENT(IN) :: time2
  TYPE(timelib_TimeInterval), INTENT(OUT) :: timeIntOut
  ! locals
  INTEGER(timelib_KIND_I8) :: nsecondsinyear
  INTEGER :: yr
  CALL timelib_TimeIntervalSet( timeIntOut )
  timeIntOut%basetime = time1%basetime - time2%basetime
  ! convert difference in years to basetime...  
  IF ( time1%YR > time2%YR ) THEN
    DO yr = time2%YR, ( time1%YR - 1 )
      timeIntOut%basetime%S = timeIntOut%basetime%S + nsecondsinyear( yr )
    ENDDO
  ELSE IF ( time2%YR > time1%YR ) THEN
    DO yr = time1%YR, ( time2%YR - 1 )
      timeIntOut%basetime%S = timeIntOut%basetime%S - nsecondsinyear( yr )
    ENDDO
  ENDIF
!$$$ add tests for multi-year differences
  CALL normalize_timeint( timeIntOut )
END SUBROUTINE c_timelibc_basetimediff


! some extra wrf stuff


! Convert fraction to string with leading sign.
! If fraction simplifies to a whole number or if
! denominator is zero, return empty string.
! INTEGER*8 interface.  
SUBROUTINE fraction_to_stringi8( numerator, denominator, frac_str )
  USE timelib_basemod
  IMPLICIT NONE
  INTEGER(timelib_KIND_I8), INTENT(IN) :: numerator
  INTEGER(timelib_KIND_I8), INTENT(IN) :: denominator
  CHARACTER (LEN=*), INTENT(OUT) :: frac_str
  IF ( denominator > 0 ) THEN
    IF ( mod( numerator, denominator ) /= 0 ) THEN
      IF ( numerator > 0 ) THEN
        WRITE(frac_str,FMT="('+',I2.2,'/',I2.2)") abs(numerator), denominator
      ELSE   ! numerator < 0
        WRITE(frac_str,FMT="('-',I2.2,'/',I2.2)") abs(numerator), denominator
      ENDIF
    ELSE   ! includes numerator == 0 case
      frac_str = ''
    ENDIF
  ELSE   ! no-fraction case
    frac_str = ''
  ENDIF
END SUBROUTINE fraction_to_stringi8


! Convert fraction to string with leading sign.
! If fraction simplifies to a whole number or if
! denominator is zero, return empty string.
! INTEGER interface.  
SUBROUTINE fraction_to_string( numerator, denominator, frac_str )
  USE timelib_basemod
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: numerator
  INTEGER, INTENT(IN) :: denominator
  CHARACTER (LEN=*), INTENT(OUT) :: frac_str
  ! locals
  INTEGER(timelib_KIND_I8) :: numerator_i8, denominator_i8
  numerator_i8 = INT( numerator, timelib_KIND_I8 )
  denominator_i8 = INT( denominator, timelib_KIND_I8 )
  CALL fraction_to_stringi8( numerator_i8, denominator_i8, frac_str )
END SUBROUTINE fraction_to_string


SUBROUTINE print_a_time( time )
   use timelib_basemod
   use timelib_Timemod
   IMPLICIT NONE
   type(timelib_Time) time
   character*128 :: s
   integer rc
   CALL timelib_TimeGet( time, timeString=s, rc=rc )
   print *,'Print a time|',TRIM(s),'|'
   return
END SUBROUTINE print_a_time

SUBROUTINE print_a_timeinterval( time )
   use timelib_basemod
   use timelib_TimeIntervalmod
   IMPLICIT NONE
   type(timelib_TimeInterval) time
   character*128 :: s
   integer rc
   CALL timelibold_TimeIntervalGetString( time, s, rc )
   print *,'Print a time interval|',TRIM(s),'|'
   return
END SUBROUTINE print_a_timeinterval

