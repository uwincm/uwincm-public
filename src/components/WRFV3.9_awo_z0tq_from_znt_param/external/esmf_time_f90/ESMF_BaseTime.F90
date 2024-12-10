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
!     timelib BaseTime Module
      module timelib_BaseTimeMod
!
!==============================================================================
!
! This file contains the BaseTime class definition and all BaseTime class
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES

#include <timelib_TimeMgr.inc>
!
!===============================================================================
!BOPI
! !MODULE: timelib_BaseTimeMod - Base timelib time definition 
!
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation
!
! This module serves only as the common Time definition inherited
! by {\tt timelib\_TimeInterval} and {\tt timelib\_Time}
!
! See {\tt ../include/timelibc\_BaseTime.h} for complete description
!
!------------------------------------------------------------------------------
! !USES:
      use timelib_BaseMod    ! timelib Base class
      implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! timelib_BaseTime
!
!     ! Base class type to match C++ BaseTime class in size only;
!     !  all dereferencing within class is performed by C++ implementation

      type timelib_BaseTime
        integer(timelib_KIND_I8) :: S   ! whole seconds
        integer(timelib_KIND_I8) :: Sn  ! fractional seconds, numerator
        integer(timelib_KIND_I8) :: Sd  ! fractional seconds, denominator
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public timelib_BaseTime
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! overloaded operators
      public operator(+)
      private timelib_BaseTimeSum
      public operator(-)
      private timelib_BaseTimeDifference
      public operator(/)
      private timelib_BaseTimeQuotI
      private timelib_BaseTimeQuotI8
      public operator(.EQ.)
      private timelib_BaseTimeEQ
      public operator(.NE.)
      private timelib_BaseTimeNE
      public operator(.LT.)
      private timelib_BaseTimeLT
      public operator(.GT.)
      private timelib_BaseTimeGT
      public operator(.LE.)
      private timelib_BaseTimeLE
      public operator(.GE.)
      private timelib_BaseTimeGE

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
      interface operator(+)
        module procedure timelib_BaseTimeSum
      end interface
      interface operator(-)
        module procedure timelib_BaseTimeDifference
      end interface
      interface operator(/)
        module procedure timelib_BaseTimeQuotI,timelib_BaseTimeQuotI8
      end interface
      interface operator(.EQ.)
        module procedure timelib_BaseTimeEQ
      end interface
      interface operator(.NE.)
        module procedure timelib_BaseTimeNE
      end interface
      interface operator(.LT.)
        module procedure timelib_BaseTimeLT
      end interface
      interface operator(.GT.)
        module procedure timelib_BaseTimeGT
      end interface
      interface operator(.LE.)
        module procedure timelib_BaseTimeLE
      end interface
      interface operator(.GE.)
        module procedure timelib_BaseTimeGE
      end interface


!==============================================================================

      contains

!==============================================================================


! Add two basetimes
      FUNCTION timelib_BaseTimeSum( basetime1, basetime2 )
        TYPE(timelib_BaseTime) :: timelib_BaseTimeSum
        TYPE(timelib_BaseTime), INTENT(IN) :: basetime1
        TYPE(timelib_BaseTime), INTENT(IN) :: basetime2
        ! locals
        INTEGER (timelib_KIND_I8) :: Sn1, Sd1, Sn2, Sd2, lcd
!  PRINT *,'DEBUG:  BEGIN timelib_BaseTimeSum()'
!  PRINT *,'DEBUG:  timelib_BaseTimeSum():  basetime1%S = ',basetime1%S
!  PRINT *,'DEBUG:  timelib_BaseTimeSum():  basetime1%Sn = ',basetime1%Sn
!  PRINT *,'DEBUG:  timelib_BaseTimeSum():  basetime1%Sd = ',basetime1%Sd
!  PRINT *,'DEBUG:  timelib_BaseTimeSum():  basetime2%S = ',basetime2%S
!  PRINT *,'DEBUG:  timelib_BaseTimeSum():  basetime2%Sn = ',basetime2%Sn
!  PRINT *,'DEBUG:  timelib_BaseTimeSum():  basetime2%Sd = ',basetime2%Sd
        timelib_BaseTimeSum   = basetime1
        timelib_BaseTimeSum%S = timelib_BaseTimeSum%S + basetime2%S
        Sn1 = basetime1%Sn
        Sd1 = basetime1%Sd
        Sn2 = basetime2%Sn
        Sd2 = basetime2%Sd
!  PRINT *,'DEBUG:  timelib_BaseTimeSum():  Sn1 = ',Sn1
!  PRINT *,'DEBUG:  timelib_BaseTimeSum():  Sd1 = ',Sd1
!  PRINT *,'DEBUG:  timelib_BaseTimeSum():  Sn2 = ',Sn2
!  PRINT *,'DEBUG:  timelib_BaseTimeSum():  Sd2 = ',Sd2
        IF      ( ( Sd1 .EQ. 0 ) .AND. ( Sd2 .EQ. 0 ) ) THEN
!  PRINT *,'DEBUG:  timelib_BaseTimeSum():  no fractions'
          timelib_BaseTimeSum%Sn = 0
          timelib_BaseTimeSum%Sd = 0
        ELSE IF ( ( Sd1 .NE. 0 ) .AND. ( Sd2 .EQ. 0 ) ) THEN
          timelib_BaseTimeSum%Sn = Sn1
          timelib_BaseTimeSum%Sd = Sd1
        ELSE IF ( ( Sd1 .EQ. 0 ) .AND. ( Sd2 .NE. 0 ) ) THEN
          timelib_BaseTimeSum%Sn = Sn2
          timelib_BaseTimeSum%Sd = Sd2
        ELSE IF ( ( Sd1 .NE. 0 ) .AND. ( Sd2 .NE. 0 ) ) THEN
          CALL compute_lcd( Sd1 , Sd2 , lcd )
          timelib_BaseTimeSum%Sd = lcd
          timelib_BaseTimeSum%Sn = (Sn1 * lcd / Sd1) + (Sn2 * lcd / Sd2)
        ENDIF
!  PRINT *,'DEBUG:  timelib_BaseTimeSum():  timelib_BaseTimeSum%S = ',timelib_BaseTimeSum%S
!  PRINT *,'DEBUG:  timelib_BaseTimeSum():  timelib_BaseTimeSum%Sn = ',timelib_BaseTimeSum%Sn
!  PRINT *,'DEBUG:  timelib_BaseTimeSum():  timelib_BaseTimeSum%Sd = ',timelib_BaseTimeSum%Sd
        CALL normalize_basetime( timelib_BaseTimeSum )
!  PRINT *,'DEBUG:  END timelib_BaseTimeSum()'
      END FUNCTION timelib_BaseTimeSum


! Subtract two basetimes
      FUNCTION timelib_BaseTimeDifference( basetime1, basetime2 )
        TYPE(timelib_BaseTime) :: timelib_BaseTimeDifference
        TYPE(timelib_BaseTime), INTENT(IN) :: basetime1
        TYPE(timelib_BaseTime), INTENT(IN) :: basetime2
        ! locals
        TYPE(timelib_BaseTime) :: neg2

        neg2%S  = -basetime2%S
        neg2%Sn = -basetime2%Sn
        neg2%Sd =  basetime2%Sd

        timelib_BaseTimeDifference = basetime1 + neg2

      END FUNCTION timelib_BaseTimeDifference


! Divide basetime by 8-byte integer
      FUNCTION timelib_BaseTimeQuotI8( basetime, divisor )
        TYPE(timelib_BaseTime) :: timelib_BaseTimeQuotI8
        TYPE(timelib_BaseTime), INTENT(IN) :: basetime
        INTEGER(timelib_KIND_I8), INTENT(IN) :: divisor
        ! locals
        INTEGER(timelib_KIND_I8) :: d, n, dinit

!PRINT *,'DEBUG timelib_BaseTimeQuotI8() A:  S,Sn,Sd = ', &
!  basetime%S,basetime%Sn,basetime%Sd
!PRINT *,'DEBUG timelib_BaseTimeQuotI8() A:  divisor = ', divisor
        IF ( divisor == 0_timelib_KIND_I8 ) THEN
          CALL wrf_error_fatal( 'timelib_BaseTimeQuotI8:  divide by zero' )
        ENDIF

!$$$ move to default constructor
        timelib_BaseTimeQuotI8%S  = 0
        timelib_BaseTimeQuotI8%Sn = 0
        timelib_BaseTimeQuotI8%Sd = 0

        ! convert to a fraction and divide by multipling the denonminator by 
        ! the divisor
        IF ( basetime%Sd == 0 ) THEN
          dinit = 1_timelib_KIND_I8
        ELSE
          dinit = basetime%Sd
        ENDIF
        n = basetime%S * dinit + basetime%Sn
        d = dinit * divisor
!PRINT *,'DEBUG timelib_BaseTimeQuotI8() B:  n,d = ',n,d
        CALL simplify( n, d, timelib_BaseTimeQuotI8%Sn, timelib_BaseTimeQuotI8%Sd )
!PRINT *,'DEBUG timelib_BaseTimeQuotI8() C:  S,Sn,Sd = ', &
!  timelib_BaseTimeQuotI8%S,timelib_BaseTimeQuotI8%Sn,timelib_BaseTimeQuotI8%Sd
        CALL normalize_basetime( timelib_BaseTimeQuotI8 )
!PRINT *,'DEBUG timelib_BaseTimeQuotI8() D:  S,Sn,Sd = ', &
!  timelib_BaseTimeQuotI8%S,timelib_BaseTimeQuotI8%Sn,timelib_BaseTimeQuotI8%Sd
      END FUNCTION timelib_BaseTimeQuotI8

! Divide basetime by integer
      FUNCTION timelib_BaseTimeQuotI( basetime, divisor )
        TYPE(timelib_BaseTime) :: timelib_BaseTimeQuotI
        TYPE(timelib_BaseTime), INTENT(IN) :: basetime
        INTEGER, INTENT(IN) :: divisor
        IF ( divisor == 0 ) THEN
          CALL wrf_error_fatal( 'timelib_BaseTimeQuotI:  divide by zero' )
        ENDIF
        timelib_BaseTimeQuotI = basetime / INT( divisor, timelib_KIND_I8 )
      END FUNCTION timelib_BaseTimeQuotI


! .EQ. for two basetimes
      FUNCTION timelib_BaseTimeEQ( basetime1, basetime2 )
        LOGICAL :: timelib_BaseTimeEQ
        TYPE(timelib_BaseTime), INTENT(IN) :: basetime1
        TYPE(timelib_BaseTime), INTENT(IN) :: basetime2
        INTEGER :: retval
        CALL seccmp( basetime1%S, basetime1%Sn, basetime1%Sd, &
                     basetime2%S, basetime2%Sn, basetime2%Sd, &
                     retval )
        timelib_BaseTimeEQ = ( retval .EQ. 0 )
      END FUNCTION timelib_BaseTimeEQ


! .NE. for two basetimes
      FUNCTION timelib_BaseTimeNE( basetime1, basetime2 )
        LOGICAL :: timelib_BaseTimeNE
        TYPE(timelib_BaseTime), INTENT(IN) :: basetime1
        TYPE(timelib_BaseTime), INTENT(IN) :: basetime2
        INTEGER :: retval
        CALL seccmp( basetime1%S, basetime1%Sn, basetime1%Sd, &
                     basetime2%S, basetime2%Sn, basetime2%Sd, &
                     retval )
        timelib_BaseTimeNE = ( retval .NE. 0 )
      END FUNCTION timelib_BaseTimeNE


! .LT. for two basetimes
      FUNCTION timelib_BaseTimeLT( basetime1, basetime2 )
        LOGICAL :: timelib_BaseTimeLT
        TYPE(timelib_BaseTime), INTENT(IN) :: basetime1
        TYPE(timelib_BaseTime), INTENT(IN) :: basetime2
        INTEGER :: retval
        CALL seccmp( basetime1%S, basetime1%Sn, basetime1%Sd, &
                     basetime2%S, basetime2%Sn, basetime2%Sd, &
                     retval )
        timelib_BaseTimeLT = ( retval .LT. 0 )
      END FUNCTION timelib_BaseTimeLT


! .GT. for two basetimes
      FUNCTION timelib_BaseTimeGT( basetime1, basetime2 )
        LOGICAL :: timelib_BaseTimeGT
        TYPE(timelib_BaseTime), INTENT(IN) :: basetime1
        TYPE(timelib_BaseTime), INTENT(IN) :: basetime2
        INTEGER :: retval
        CALL seccmp( basetime1%S, basetime1%Sn, basetime1%Sd, &
                     basetime2%S, basetime2%Sn, basetime2%Sd, &
                     retval )
        timelib_BaseTimeGT = ( retval .GT. 0 )
      END FUNCTION timelib_BaseTimeGT


! .LE. for two basetimes
      FUNCTION timelib_BaseTimeLE( basetime1, basetime2 )
        LOGICAL :: timelib_BaseTimeLE
        TYPE(timelib_BaseTime), INTENT(IN) :: basetime1
        TYPE(timelib_BaseTime), INTENT(IN) :: basetime2
        INTEGER :: retval
        CALL seccmp( basetime1%S, basetime1%Sn, basetime1%Sd, &
                     basetime2%S, basetime2%Sn, basetime2%Sd, &
                     retval )
        timelib_BaseTimeLE = ( retval .LE. 0 )
      END FUNCTION timelib_BaseTimeLE


! .GE. for two basetimes
      FUNCTION timelib_BaseTimeGE( basetime1, basetime2 )
        LOGICAL :: timelib_BaseTimeGE
        TYPE(timelib_BaseTime), INTENT(IN) :: basetime1
        TYPE(timelib_BaseTime), INTENT(IN) :: basetime2
        INTEGER :: retval
        CALL seccmp( basetime1%S, basetime1%Sn, basetime1%Sd, &
                     basetime2%S, basetime2%Sn, basetime2%Sd, &
                     retval )
        timelib_BaseTimeGE = ( retval .GE. 0 )
      END FUNCTION timelib_BaseTimeGE


      end module timelib_BaseTimeMod
