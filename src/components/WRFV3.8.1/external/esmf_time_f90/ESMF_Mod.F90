! TBH:  This version is for use with the timelib library embedded in the WRF 
! TBH:  distribution.  
MODULE timelib_Mod
   USE timelib_alarmmod
   USE timelib_basemod
   USE timelib_basetimemod
   USE timelib_calendarmod
   USE timelib_clockmod
   USE timelib_fractionmod
   USE timelib_timeintervalmod
   USE timelib_timemod
   USE timelib_alarmclockmod
   USE timelib_stubs   ! add new dummy interfaces and typedefs here as needed
#include <timelib_TimeMgr.inc>
   INTEGER, PARAMETER :: timelib_MAX_ALARMS=MAX_ALARMS
!
END MODULE timelib_Mod
