! Various dummy type definitions and routines for the sole purpose of 
! mimicking newer timelib interface features without necessarily implementing 
! them.  

MODULE timelib_Stubs

   IMPLICIT NONE

   PRIVATE

! Bogus typedefs
   TYPE timelib_Grid
      INTEGER :: dummy
   END TYPE

   TYPE timelib_GridComp
      INTEGER :: dummy
   END TYPE

   TYPE timelib_State
      INTEGER :: dummy
   END TYPE

   TYPE timelib_VM
      INTEGER :: dummy
   END TYPE

   TYPE timelib_MsgType
      INTEGER :: mtype
   END TYPE
   TYPE(timelib_MsgType), PARAMETER  ::      &
      timelib_LOG_INFO  =   timelib_MsgType(1), &
      timelib_LOG_WARNING = timelib_MsgType(2), &
      timelib_LOG_ERROR =   timelib_MsgType(3)

   TYPE timelib_LOG
      INTEGER :: dummy
   END TYPE

   LOGICAL, private, save :: initialized = .false.

   PUBLIC timelib_Grid, timelib_GridComp, timelib_State, timelib_VM
   PUBLIC timelib_Initialize, timelib_Finalize, timelib_IsInitialized
   PUBLIC timelib_LogWrite, timelib_LOG, timelib_MsgType
   PUBLIC timelib_LOG_INFO, timelib_LOG_WARNING, timelib_LOG_ERROR

CONTAINS


! NOOP
   SUBROUTINE timelib_Initialize( vm, defaultcalkind, rc )
      USE timelib_basemod
      USE timelib_calendarmod
      TYPE(timelib_VM),           INTENT(IN   ), OPTIONAL :: vm
      TYPE(timelib_CalendarType), INTENT(IN   ), OPTIONAL :: defaultcalkind
      INTEGER,                 INTENT(  OUT), OPTIONAL :: rc

      TYPE(timelib_CalendarType) :: defaultCalType
      INTEGER :: status

      IF ( PRESENT( rc ) ) rc = timelib_FAILURE
      ! Initialize the default time manager calendar
      IF ( PRESENT(defaultcalkind) )THEN
         defaultCalType = defaultcalkind
      ELSE
         defaultCalType = timelib_CAL_NOLEAP
      END IF
      allocate( defaultCal )
      defaultCal = timelib_CalendarCreate( calendarType=defaultCalType, &
                        rc=status)

      ! initialize tables in time manager
      CALL initdaym

      IF (status .ne. timelib_SUCCESS) THEN
          PRINT *, "Error initializing the default time manager calendar"
          RETURN
      END IF
      initialized = .true.

      IF ( PRESENT( rc ) ) rc = timelib_SUCCESS
   END SUBROUTINE timelib_Initialize


   FUNCTION timelib_IsInitialized()
      LOGICAL timelib_IsInitialized
      timelib_IsInitialized = initialized
   END FUNCTION timelib_IsInitialized


! NOOP
   SUBROUTINE timelib_Finalize( rc )
      USE timelib_basemod
      INTEGER, INTENT(  OUT), OPTIONAL :: rc
#if (defined SPMD) || (defined COUP_CSM)
#include <mpif.h>
#endif
      LOGICAL :: flag
      INTEGER :: ier

      IF ( PRESENT( rc ) ) rc = timelib_SUCCESS
#if (defined SPMD) || (defined COUP_CSM)
      CALL MPI_Finalized( flag, ier )
      IF ( ier .ne. mpi_success )THEN
        IF ( PRESENT( rc ) ) rc = timelib_FAILURE
      END IF
      IF ( .NOT. flag ) THEN
        CALL MPI_Finalize( ier ) 
        IF ( ier .ne. mpi_success )THEN
          IF ( PRESENT( rc ) ) rc = timelib_FAILURE
        END IF
      END IF
#endif
   END SUBROUTINE timelib_Finalize

! NOOP
   SUBROUTINE timelib_LogWrite( msg, MsgType, line, file, method, log, rc )
      USE timelib_basemod
      CHARACTER(LEN=*), INTENT(IN) :: msg
      TYPE(timelib_MsgType), INTENT(IN) :: msgtype
      INTEGER, INTENT(IN), OPTIONAL :: line
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: file
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: method
      TYPE(timelib_LOG),TARGET,OPTIONAL :: log
      INTEGER, INTENT(OUT),OPTIONAL :: rc
      IF ( PRESENT( rc ) ) rc = timelib_SUCCESS
   END SUBROUTINE timelib_LogWrite


END MODULE timelib_Stubs


