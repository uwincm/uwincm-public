!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA license.
!
! timelib Base Module
!
! (all lines between the !BOP and !EOP markers will be included in the
! automated document processing.)
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! module definition

      module timelib_BaseMod
 
!BOP
! !MODULE: timelib_BaseMod - Base class for all timelib classes
!
! !DESCRIPTION:
!
! The code in this file implements the Base defined type
!  and functions which operate on all types.  This is an
!  interface to the actual C++ base class implementation in the ../src dir.
!
! See the timelib Developers Guide document for more details.
!
!------------------------------------------------------------------------------

! !USES:
      implicit none
!
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!
!    Global integer parameters, used frequently

      integer, parameter :: timelib_SUCCESS = 0, timelib_FAILURE = -1
      integer, parameter :: timelib_MAXSTR = 128
      integer, parameter :: timelib_MAXDIM = 7, &
                            timelib_MAXDECOMPDIM=3, &
                            timelib_MAXGRIDDIM=2
     
      integer, parameter :: timelib_MAJOR_VERSION = 2
      integer, parameter :: timelib_MINOR_VERSION = 1
      integer, parameter :: timelib_REVISION      = 1
      integer, parameter :: timelib_PATCHLEVEL    = 0
      character(32), parameter :: timelib_VERSION_STRING = "2.1.1"

!------------------------------------------------------------------------------
!
      type timelib_Status
      private
          integer :: status
      end type

      type(timelib_Status), parameter :: timelib_STATE_UNINIT = timelib_Status(1), &
                                      timelib_STATE_READY = timelib_Status(2), &
                                      timelib_STATE_UNALLOCATED = timelib_Status(3), &
                                      timelib_STATE_ALLOCATED = timelib_Status(4), &
                                      timelib_STATE_BUSY = timelib_Status(5), &
                                      timelib_STATE_INVALID = timelib_Status(6)
 
!------------------------------------------------------------------------------
!
      type timelib_Pointer
      private
          integer*8 :: ptr
      end type

      type(timelib_Pointer), parameter :: timelib_NULL_POINTER = timelib_Pointer(0), &
                                       timelib_BAD_POINTER = timelib_Pointer(-1)


!------------------------------------------------------------------------------
!
      !! TODO: I believe if we define an assignment(=) operator to convert
      !!   a datatype into integer, then we could use the type and kind as
      !!   targets in a select case() statement and make the contents private.
      !!   (see pg 248 of the "big book")
      type timelib_DataType
      !!private
          integer :: dtype
      end type

      type(timelib_DataType), parameter :: timelib_DATA_INTEGER = timelib_DataType(1), &
                                        timelib_DATA_REAL = timelib_DataType(2), &
                                        timelib_DATA_LOGICAL = timelib_DataType(3), &
                                        timelib_DATA_CHARACTER = timelib_DataType(4)

!------------------------------------------------------------------------------

      integer, parameter :: &
                   timelib_KIND_I1 = selected_int_kind(2), &
                   timelib_KIND_I2 = selected_int_kind(4), &
                   timelib_KIND_I4 = selected_int_kind(9), &
                   timelib_KIND_I8 = selected_int_kind(14), &
                   timelib_KIND_R4 = selected_real_kind(3,25), &
                   timelib_KIND_R8 = selected_real_kind(6,45), &
                   timelib_KIND_C8 = selected_real_kind(3,25), &
                   timelib_KIND_C16 = selected_real_kind(6,45)

!------------------------------------------------------------------------------

      type timelib_DataValue
      private
          type(timelib_DataType) :: dt
          integer :: rank
          ! how do you do values of all types here ? TODO
          ! in C++ i'd do a union w/ overloaded access funcs
          integer :: vi
          !integer, dimension (:), pointer :: vip
          !real :: vr
          !real, dimension (:), pointer :: vrp
          !logical :: vl
          !logical, pointer :: vlp
          !character (len=timelib_MAXSTR) :: vc
          !character, pointer :: vcp
      end type

!------------------------------------------------------------------------------
!
      type timelib_Attribute
      private
          character (len=timelib_MAXSTR) :: attr_name
          type (timelib_DataType) :: attr_type
          type (timelib_DataValue) :: attr_value
      end type

!------------------------------------------------------------------------------
!
      !! TODO: this should be a shallow object, with a simple init() and
      !!  get() function, and the contents should go back to being private.
      type timelib_AxisIndex
!     !!private
          integer :: l
          integer :: r
          integer :: max
          integer :: decomp
          integer :: gstart
      end type

      !! TODO: same comment as above.
      type timelib_MemIndex
!     !!private
          integer :: l
          integer :: r
          integer :: str
          integer :: num
      end type

!------------------------------------------------------------------------------
!
      type timelib_BasePointer
      private
          integer*8 :: base_ptr
      end type

      integer :: global_count = 0

!------------------------------------------------------------------------------
!
!     ! WARNING: must match corresponding values in ../include/timelibc_Base.h
      type timelib_Logical
      private
          integer :: value
      end type

      type(timelib_Logical), parameter :: timelib_TF_UNKNOWN  = timelib_Logical(1), &
                                       timelib_TF_TRUE     = timelib_Logical(2), &
                                       timelib_TF_FALSE    = timelib_Logical(3)

!------------------------------------------------------------------------------
!
      type timelib_Base
      private
         integer :: ID
         integer :: ref_count
         type (timelib_Status) :: base_status
         character (len=timelib_MAXSTR) :: name
     end type

! !PUBLIC TYPES:

      public timelib_STATE_INVALID
!      public timelib_STATE_UNINIT, timelib_STATE_READY, &
!             timelib_STATE_UNALLOCATED, timelib_STATE_ALLOCATED, &
!             timelib_STATE_BUSY

      public timelib_DATA_INTEGER, timelib_DATA_REAL, &
             timelib_DATA_LOGICAL, timelib_DATA_CHARACTER

      public timelib_KIND_I1, timelib_KIND_I2, timelib_KIND_I4, timelib_KIND_I8, & 
             timelib_KIND_R4, timelib_KIND_R8, timelib_KIND_C8, timelib_KIND_C16

      public timelib_NULL_POINTER, timelib_BAD_POINTER


      public timelib_FAILURE, timelib_SUCCESS
      public timelib_MAXSTR
      public timelib_MAXDIM, timelib_MAXDECOMPDIM, timelib_MAXGRIDDIM
     
      public timelib_MAJOR_VERSION, timelib_MINOR_VERSION, timelib_REVISION
      public timelib_VERSION_STRING 

      public timelib_Status, timelib_Pointer, timelib_DataType
      public timelib_DataValue, timelib_Attribute
!      public timelib_MemIndex
!      public timelib_BasePointer
      public timelib_Base

      public timelib_AxisIndex, timelib_AxisIndexGet
!      public timelib_AxisIndexInit
      public timelib_Logical
!      public timelib_TF_TRUE, timelib_TF_FALSE

! !PUBLIC MEMBER FUNCTIONS:
!
! !DESCRIPTION:
!     The following routines apply to any type in the system.  
!     The attribute routines can be inherited as-is.  The other
!     routines need to be specialized by the higher level objects.
!
!   Base class methods
!      public timelib_BaseInit
   
!      public timelib_BaseGetConfig
!      public timelib_BaseSetConfig

!      public timelib_BaseGetInstCount

!      public timelib_BaseSetID
!      public timelib_BaseGetID

!      public timelib_BaseSetRefCount
!      public timelib_BaseGetRefCount

!      public timelib_BaseSetStatus
!      public timelib_BaseGetStatus

!   Virtual methods to be defined by derived classes
!      public timelib_Read
!      public timelib_Write
!      public timelib_Validate
!      public timelib_Print

!  Attribute methods
      public timelib_AttributeSet
      public timelib_AttributeGet
      public timelib_AttributeGetCount
      public timelib_AttributeGetbyNumber
      public timelib_AttributeGetNameList
      public timelib_AttributeSetList
      public timelib_AttributeGetList
      public timelib_AttributeSetObjectList
      public timelib_AttributeGetObjectList
      public timelib_AttributeCopy
      public timelib_AttributeCopyAll
 
!  Misc methods
      public timelib_SetName
      public timelib_GetName
      public timelib_SetPointer
      public timelib_SetNullPointer
      public timelib_GetPointer

!  Print methods for calling by higher level print functions
!  (they have little formatting other than the actual values)
      public timelib_StatusString, timelib_DataTypeString

!  Overloaded = operator functions
      public operator(.eq.), operator(.ne.), assignment(=)
!
!
!EOP

!------------------------------------------------------------------------------

! overload .eq. & .ne. with additional derived types so you can compare 
!  them as if they were simple integers.
 

interface operator (.eq.)
 module procedure timelib_sfeq
 module procedure timelib_dteq
 module procedure timelib_pteq
 module procedure timelib_tfeq
 module procedure timelib_aieq
end interface

interface operator (.ne.)
 module procedure timelib_sfne
 module procedure timelib_dtne
 module procedure timelib_ptne
 module procedure timelib_tfne
 module procedure timelib_aine
end interface

interface assignment (=)
 module procedure timelib_dtas
 module procedure timelib_ptas
end interface

!------------------------------------------------------------------------------

      contains

!------------------------------------------------------------------------------
! function to compare two timelib_Status flags to see if they're the same or not

function timelib_sfeq(sf1, sf2)
 logical timelib_sfeq
 type(timelib_Status), intent(in) :: sf1, sf2

 timelib_sfeq = (sf1%status .eq. sf2%status)
end function

function timelib_sfne(sf1, sf2)
 logical timelib_sfne
 type(timelib_Status), intent(in) :: sf1, sf2

 timelib_sfne = (sf1%status .ne. sf2%status)
end function

!------------------------------------------------------------------------------
! function to compare two timelib_DataTypes to see if they're the same or not

function timelib_dteq(dt1, dt2)
 logical timelib_dteq
 type(timelib_DataType), intent(in) :: dt1, dt2

 timelib_dteq = (dt1%dtype .eq. dt2%dtype)
end function

function timelib_dtne(dt1, dt2)
 logical timelib_dtne
 type(timelib_DataType), intent(in) :: dt1, dt2

 timelib_dtne = (dt1%dtype .ne. dt2%dtype)
end function

subroutine timelib_dtas(intval, dtval)
 integer, intent(out) :: intval
 type(timelib_DataType), intent(in) :: dtval

 intval = dtval%dtype
end subroutine


!------------------------------------------------------------------------------
! function to compare two timelib_Pointers to see if they're the same or not

function timelib_pteq(pt1, pt2)
 logical timelib_pteq
 type(timelib_Pointer), intent(in) :: pt1, pt2

 timelib_pteq = (pt1%ptr .eq. pt2%ptr)
end function

function timelib_ptne(pt1, pt2)
 logical timelib_ptne
 type(timelib_Pointer), intent(in) :: pt1, pt2

 timelib_ptne = (pt1%ptr .ne. pt2%ptr)
end function

subroutine timelib_ptas(ptval, intval)
 type(timelib_Pointer), intent(out) :: ptval
 integer, intent(in) :: intval

 ptval%ptr = intval
end subroutine

!------------------------------------------------------------------------------
! function to compare two timelib_Logicals to see if they're the same or not
! also need assignment to real f90 logical?

function timelib_tfeq(tf1, tf2)
 logical timelib_tfeq
 type(timelib_Logical), intent(in) :: tf1, tf2

 timelib_tfeq = (tf1%value .eq. tf2%value)
end function

function timelib_tfne(tf1, tf2)
 logical timelib_tfne
 type(timelib_Logical), intent(in) :: tf1, tf2

 timelib_tfne = (tf1%value .ne. tf2%value)
end function

!------------------------------------------------------------------------------
! function to compare two timelib_AxisIndex to see if they're the same or not

function timelib_aieq(ai1, ai2)
 logical timelib_aieq
 type(timelib_AxisIndex), intent(in) :: ai1, ai2

 timelib_aieq = ((ai1%l .eq. ai2%l) .and. &
              (ai1%r .eq. ai2%r) .and. &
              (ai1%max .eq. ai2%max) .and. &
              (ai1%decomp .eq. ai2%decomp) .and. &
              (ai1%gstart .eq. ai2%gstart))

end function

function timelib_aine(ai1, ai2)
 logical timelib_aine
 type(timelib_AxisIndex), intent(in) :: ai1, ai2

 timelib_aine = ((ai1%l .ne. ai2%l) .or. &
              (ai1%r .ne. ai2%r) .or. &
              (ai1%max .ne. ai2%max) .or. &
              (ai1%decomp .ne. ai2%decomp) .or. &
              (ai1%gstart .ne. ai2%gstart))

end function

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! 
! Base methods
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  timelib_BaseInit - initialize a Base object
!
! !INTERFACE:
      subroutine timelib_BaseInit(base, rc)
!
! !ARGUMENTS:
      type(timelib_Base) :: base                 
      integer, intent(out), optional :: rc     

!
! !DESCRIPTION:
!     Set initial state on a Base object.
!
!     \begin{description}
!     \item [base]
!           In the Fortran interface, this must in fact be a {\tt Base}
!           derived type object.  It is expected that all specialized 
!           derived types will include a {\tt Base} object as the first
!           entry.
!     \item [{[rc]}]
!           Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOP

      logical :: rcpresent                          ! Return code present   

!     !Initialize return code
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = timelib_FAILURE
      endif

      global_count = global_count + 1
      base%ID = global_count
      base%ref_count = 1
      base%base_status = timelib_STATE_READY
      base%name = "undefined"

      if (rcpresent) rc = timelib_SUCCESS

      end subroutine timelib_BaseInit

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  timelib_SetName - set the name of this object
!
! !INTERFACE:
      subroutine timelib_SetName(anytype, name, namespace, rc)
!
! !ARGUMENTS:
      type(timelib_Base) :: anytype                 
      character (len = *), intent(in), optional :: name   
      character (len = *), intent(in), optional :: namespace
      integer, intent(out), optional :: rc     

!
! !DESCRIPTION:
!     Associate a name with any object in the system.
!
!     \begin{description}
!     \item [anytype]
!           In the Fortran interface, this must in fact be a {\tt Base}
!           derived type object.  It is expected that all specialized 
!           derived types will include a {\tt Base} object as the first
!           entry.
!     \item [[name]]
!           Object name.  An error will be returned if a duplicate name 
!           is specified.  If a name is not given a unique name will be
!           generated and can be queried by the {\tt timelib_GetName} routine.
!     \item [[namespace]]
!           Object namespace (e.g. "Application", "Component", "Grid", etc).
!           If given, the name will be checked that it is unique within
!           this namespace.  If not given, the generated name will be 
!           unique within this namespace.  If namespace is not specified,
!           a default "global" namespace will be assumed and the same rules
!           for names will be followed.
!     \item [[rc]]
!           Return code; equals {\tt timelib\_SUCCESS} if there are no errors.
!
!     \end{description}
!
! 

!
!EOP
! !REQUIREMENTS:  FLD1.5, FLD1.5.3
      logical :: rcpresent                          ! Return code present   
      character (len = timelib_MAXSTR) :: ournamespace ! Namespace if not given
      character (len = timelib_MAXSTR) :: defaultname  ! Name if not given
      integer, save :: seqnum = 0       ! HACK - generate uniq names
                                        ! but not coordinated across procs

!     !Initialize return code
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = timelib_FAILURE
      endif

!     ! TODO: this code should generate a unique name if a name
!     !   is not given.  If a namespace is given, the name has to
!     !   be unique within that namespace.  Example namespaces could
!     !   be: Applications, Components, Fields/Bundles, Grids.
!      
!     ! Construct a default namespace if one is not given
      if( present(namespace) ) then
          if( namespace .eq. "" ) then
              ournamespace = "global"
          else
              ournamespace = namespace
          endif
      else
              ournamespace = "global"
      endif

!     ! Construct a default name if one is not given
      if( present(name) ) then
         if( name .eq. "" ) then
            write(defaultname, 20) trim(ournamespace), seqnum
20          format(A,I3.3)
            seqnum = seqnum + 1
            anytype%name = defaultname
         else
            anytype%name = name
         endif
      else
         write(defaultname, 20) trim(ournamespace), seqnum
         seqnum = seqnum + 1
         anytype%name = defaultname
      endif

      if (rcpresent) rc = timelib_SUCCESS

      end subroutine timelib_SetName

!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  timelib_GetName - get the name of this object
!
! !INTERFACE:
      subroutine timelib_GetName(anytype, name, rc)
!
! !ARGUMENTS:
      type(timelib_Base), intent(in) :: anytype             ! any timelib object/type
      character (len = *), intent(out) :: name           ! object/type name
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!     Return the name of any type in the system.

!
!EOP
! !REQUIREMENTS:  FLD1.5, FLD1.5.3

      name = anytype%name
      if (present(rc)) rc = timelib_SUCCESS

      end subroutine timelib_GetName


!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  timelib_AttributeSet - set attribute on an timelib type
!
! !INTERFACE:
      subroutine timelib_AttributeSet(anytype, name, value, rc)
!
! !ARGUMENTS:
      type(timelib_Base), intent(in) :: anytype             ! any timelib type
      character (len = *), intent(in) :: name            ! attribute name
      type(timelib_DataValue), intent(in) :: value              ! attribute value
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!     Associate a (name,value) pair with any type in the system.

!
!EOP
! !REQUIREMENTS:  FLD1.5, FLD1.5.3

      end subroutine timelib_AttributeSet


!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  timelib_AttributeGet - get attribute from an timelib type
!
! !INTERFACE:
      subroutine timelib_AttributeGet(anytype, name, type, value, rc)
!
! !ARGUMENTS:
      type(timelib_Base), intent(in) :: anytype           ! any timelib type
      character (len = *), intent(in) :: name          ! attribute name
      type(timelib_DataType), intent(out) :: type             ! all possible data types
      type(timelib_DataValue), intent(out) :: value           ! attribute value
      integer, intent(out), optional :: rc             ! return code

!
! !DESCRIPTION:

!
!EOP
! !REQUIREMENTS:  FLD1.5.1, FLD1.5.3

      end subroutine timelib_AttributeGet


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  timelib_AttributeGetCount - get an timelib object's number of attributes
!
! !INTERFACE:
      subroutine timelib_AttributeGetCount(anytype, count, rc)
!
! !ARGUMENTS:
      type(timelib_Base), intent(in) :: anytype             ! any timelib type
      integer, intent(out) :: count                      ! attribute count
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Returns number of attributes present.

!
!EOP
! !REQUIREMENTS:  FLD1.7.5

      end subroutine timelib_AttributeGetCount


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  timelib_AttributeGetbyNumber - get an timelib object's attribute by num ber
!
! !INTERFACE:
      subroutine timelib_AttributeGetbyNumber(anytype, number, name, type, value, rc)
!
! !ARGUMENTS:
      type(timelib_Base), intent(in) :: anytype             ! any timelib type
      integer, intent(in) :: number                      ! attribute number
      character (len = *), intent(in) :: name            ! attribute name
      type(timelib_DataType), intent(out) :: type               ! all possible data types
      type(timelib_DataValue), intent(out) :: value             ! attribute value
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Allows the caller to get attributes by number instead of by name.
! This can be useful in iterating through all attributes in a loop.
!
!EOP
! !REQUIREMENTS: 

      end subroutine timelib_AttributeGetbyNumber


!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  timelib_AttributeGetNameList - get an timelib object's attribute name list
!
! !INTERFACE:
      subroutine timelib_AttributeGetNameList(anytype, count, namelist, rc)
!
! !ARGUMENTS:
      type(timelib_Base), intent(in) :: anytype             ! any timelib type
      integer, intent(out) :: count                      ! attribute count
      character (len = *), dimension (:), intent(out) :: namelist   ! attribute names
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Return a list of all attribute names without returning the values.

!
!EOP
! !REQUIREMENTS:  FLD1.7.3

      end subroutine timelib_AttributeGetNameList


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  timelib_AttributeSetList - set an timelib object's attributes 
!
! !INTERFACE:
      subroutine timelib_AttributeSetList(anytype, namelist, valuelist, rc)

!
! !ARGUMENTS:
      type(timelib_Base), intent(in) :: anytype             ! any timelib type
      character (len = *), dimension (:), intent(in) :: namelist    ! attribute names
      type(timelib_DataValue), dimension (:), intent(in) :: valuelist      ! attribute values
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Set multiple attributes on an object in one call.  Depending on what is
! allowed by the interface, all attributes may have to have the same type.
!
!EOP
! !REQUIREMENTS:  (none.  added for completeness)

      end subroutine timelib_AttributeSetList


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  timelib_AttributeGetList - get an timelib object's attributes
!
! !INTERFACE:
      subroutine timelib_AttributeGetList(anytype, namelist, typelist, valuelist, rc)
!
! !ARGUMENTS:
      type(timelib_Base), intent(in) :: anytype             ! any timelib type
      character (len = *), dimension (:), intent(in) :: namelist    ! attribute names
      type(timelib_DataType), dimension (:), intent(out) :: typelist       ! all possible data types
      type(timelib_DataValue), dimension (:), intent(out) :: valuelist     ! attribute values
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Get multiple attributes from an object in a single call.

!
!EOP
! !REQUIREMENTS:  FLD1.7.4

      end subroutine timelib_AttributeGetList


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  timelib_AttributeSetObjectList - set an attribute on multiple timelib objects 
!
! !INTERFACE:
      subroutine timelib_AttributeSetObjectList(anytypelist, name, value, rc)
!
! !ARGUMENTS:
      type(timelib_Base), dimension (:), intent(in) :: anytypelist     ! list of any timelib types
      character (len = *), intent(in) :: name            ! attribute name
      type(timelib_DataValue), dimension (:), intent(in) :: value          ! attribute value
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Set the same attribute on multiple objects in one call.

!
!EOP
! !REQUIREMENTS:  FLD1.5.5 (pri 2)

      end subroutine timelib_AttributeSetObjectList


!-------------------------------------------------------------------------
!BOP
!
!
! !IROUTINE:  timelib_AttributeGetObjectList - get an attribute from multiple timelib objects 
!
! !INTERFACE:
      subroutine timelib_AttributeGetObjectList(anytypelist, name, typelist, valuelist, rc)
!
! !ARGUMENTS:
      type(timelib_Base), dimension (:), intent(in) :: anytypelist     ! list of any timelib types
      character (len = *), intent(in) :: name            ! attribute name
      type(timelib_DataType), dimension (:), intent(out) :: typelist       ! all possible data types
      type(timelib_DataValue), dimension (:), intent(out) :: valuelist     ! attribute values
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Get the same attribute name from multiple objects in one call.

!
!EOP
! !REQUIREMENTS:  FLD1.5.5 (pri 2)

      end subroutine timelib_AttributeGetObjectList


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  timelib_AttributeCopy - copy an attribute between two objects
!
! !INTERFACE:
      subroutine timelib_AttributeCopy(name, source, destination, rc)
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name            ! attribute name
      type(timelib_Base), intent(in) :: source              ! any timelib type
      type(timelib_Base), intent(in) :: destination         ! any timelib type
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! The specified attribute associated with the source object is
! copied to the destination object.  << does this assume overwriting the
! attribute if it already exists in the output or does this require yet
! another arg to say what to do with collisions? >>


!
!EOP
! !REQUIREMENTS:  FLD1.5.4

      end subroutine timelib_AttributeCopy


!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  timelibc_AttributeCopyAll - copy attributes between two objects

!
! !INTERFACE:
      subroutine timelib_AttributeCopyAll(source, destination, rc)
!
! !ARGUMENTS:
      type(timelib_Base), intent(in) :: source              ! any timelib type
      type(timelib_Base), intent(in) :: destination         ! any timelib type
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! All attributes associated with the source object are copied to the
! destination object.  Some attributes will have to be considered
! {\tt read only} and won't be updated by this call.  (e.g. an attribute
! like {\tt name} must be unique and therefore can't be duplicated.)

!
!EOP
! !REQUIREMENTS:  FLD1.5.4

      end subroutine timelib_AttributeCopyAll

!=========================================================================
! Misc utility routines, perhaps belongs in a utility file?
!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  timelibc_AxisIndexInit - initialize an AxisIndex object

!
! !INTERFACE:
      subroutine timelib_AxisIndexInit(ai, l, r, max, decomp, gstart, rc)
!
! !ARGUMENTS:
      type(timelib_AxisIndex), intent(inout) :: ai
      integer, intent(in) :: l, r, max, decomp, gstart
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!   Set the contents of an AxisIndex type.

!
!EOP
! !REQUIREMENTS:

      ai%l = l
      ai%r = r
      ai%max = max
      ai%decomp = decomp
      ai%gstart = gstart

      if (present(rc)) rc = timelib_SUCCESS

      end subroutine timelib_AxisIndexInit

!BOP
!
!IROUTINE:  timelibc_AxisIndexInit - initialize an AxisIndex object

!
! !INTERFACE:
      subroutine timelib_AxisIndexGet(ai, l, r, max, decomp, gstart, rc)
!
! !ARGUMENTS:
      type(timelib_AxisIndex), intent(inout) :: ai
      integer, intent(out), optional :: l, r, max, decomp, gstart
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!   Get the contents of an AxisIndex type.

!
!EOP
! !REQUIREMENTS:

      if (present(l)) l = ai%l
      if (present(r)) r = ai%r
      if (present(max)) max = ai%max
      if (present(decomp)) decomp = ai%decomp
      if (present(gstart)) gstart = ai%gstart

      if (present(rc)) rc = timelib_SUCCESS

      end subroutine timelib_AxisIndexGet

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  timelib_SetPointer - set an opaque value

!
! !INTERFACE:
      subroutine timelib_SetPointer(ptype, contents, rc)
!
! !ARGUMENTS:
      type(timelib_Pointer) :: ptype 
      integer*8, intent(in) :: contents
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Set the contents of an opaque pointer type.

!
!EOP
! !REQUIREMENTS:
      ptype%ptr = contents
      if (present(rc)) rc = timelib_SUCCESS

      end subroutine timelib_SetPointer

!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  timelib_SetNullPointer - set an opaque value

!
! !INTERFACE:
      subroutine timelib_SetNullPointer(ptype, rc)
!
! !ARGUMENTS:
      type(timelib_Pointer) :: ptype 
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Set the contents of an opaque pointer type.

!
!EOP
! !REQUIREMENTS:
      integer*8, parameter :: nullp = 0

      ptype%ptr = nullp
      if (present(rc)) rc = timelib_SUCCESS

      end subroutine timelib_SetNullPointer
!------------------------------------------------------------------------- 
!BOP 
!  !IROUTINE:  timelib_GetPointer - get an opaque value 
!  
! !INTERFACE: 
      function timelib_GetPointer(ptype, rc) 
!
! !RETURN VALUE:
      integer*8 :: timelib_GetPointer

! !ARGUMENTS:
      type(timelib_Pointer), intent(in) :: ptype 
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Get the contents of an opaque pointer type.

!
!EOP
! !REQUIREMENTS:
      timelib_GetPointer = ptype%ptr
      if (present(rc)) rc = timelib_SUCCESS

      end function timelib_GetPointer

!------------------------------------------------------------------------- 
! misc print routines
!------------------------------------------------------------------------- 
!BOP 
!  !IROUTINE:  timelib_StatusString - Return status as a string
!  
! !INTERFACE: 
      subroutine timelib_StatusString(status, string, rc)
!
! !ARGUMENTS:
      type(timelib_Status), intent(in) :: status
      character(len=*), intent(out) :: string
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Return a status variable as a string.

!
!EOP
! !REQUIREMENTS:

      if (status .eq. timelib_STATE_UNINIT) string = "Uninitialized"
      if (status .eq. timelib_STATE_READY) string = "Ready"
      if (status .eq. timelib_STATE_UNALLOCATED) string = "Unallocated"
      if (status .eq. timelib_STATE_ALLOCATED) string = "Allocated"
      if (status .eq. timelib_STATE_BUSY) string = "Busy"
      if (status .eq. timelib_STATE_INVALID) string = "Invalid"
 
      if (present(rc)) rc = timelib_SUCCESS

      end subroutine timelib_StatusString

!------------------------------------------------------------------------- 
!BOP 
!  !IROUTINE:  timelib_DataTypeString - Return DataType as a string
!  
! !INTERFACE: 
      subroutine timelib_DataTypeString(datatype, string, rc)
!
! !ARGUMENTS:
      type(timelib_DataType), intent(in) :: datatype
      character(len=*), intent(out) :: string
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Return a datatype variable as a string.

!
!EOP
! !REQUIREMENTS:

      if (datatype .eq. timelib_DATA_INTEGER) string = "Integer"
      if (datatype .eq. timelib_DATA_REAL) string = "Real"
      if (datatype .eq. timelib_DATA_LOGICAL) string = "Logical"
      if (datatype .eq. timelib_DATA_CHARACTER) string = "Character"
 
      if (present(rc)) rc = timelib_SUCCESS

      end subroutine timelib_DataTypeString

!-------------------------------------------------------------------------
!
!-------------------------------------------------------------------------
! put Print and Validate skeletons here - but they should be
!  overridden by higher level more specialized functions.
!-------------------------------------------------------------------------

      end module timelib_BaseMod
