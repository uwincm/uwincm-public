! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define FILENAME "src/addon/NUOPC/src/NUOPC_FieldDictionaryDef.F90"
!==============================================================================

module NUOPC_FieldDictionaryDef

  use ESMF
  use NUOPC_FreeFormatDef

  implicit none
  
  private

  ! public types
  public NUOPC_FieldDictionaryEntryS, NUOPC_FieldDictionaryEntry

  type NUOPC_FieldDictionaryEntryS
    character(ESMF_MAXSTR)          :: standardName
    character(ESMF_MAXSTR)          :: canonicalUnits
    character(ESMF_MAXSTR), pointer :: connectedOptions(:)
    character(ESMF_MAXSTR), pointer :: synonyms(:)
  end type
  
  type NUOPC_FieldDictionaryEntry
    type(NUOPC_FieldDictionaryEntryS), pointer :: wrap
  end type

  ! public module interfaces
  public NUOPC_FieldDictionaryAddEntryI
  public NUOPC_FieldDictionaryEgestI
  public NUOPC_FieldDictionaryGetEntryI
  public NUOPC_FieldDictionaryHasEntryI
  public NUOPC_FieldDictionaryMatchSynoI
  public NUOPC_FieldDictionarySetSynoI
  public NUOPC_FieldDictionaryDefinition

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_FieldDictionaryAddEntryI - Add an entry to the NUOPC Field dictionary
! !INTERFACE:
  subroutine NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
    standardName, canonicalUnits, rc)
! !ARGUMENTS:
    type(ESMF_Container),             intent(inout)         :: fieldDictionary
    character(*),                     intent(in)            :: standardName
    character(*),                     intent(in)            :: canonicalUnits
    integer,                          intent(out), optional :: rc
! !DESCRIPTION:
!   Add an entry to the NUOPC Field dictionary.
!EOPI
  !-----------------------------------------------------------------------------
    ! local variables
    type(NUOPC_FieldDictionaryEntry)  :: fdEntry
    integer                           :: stat
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    ! allocate fdEntry
    allocate(fdEntry%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(stat, msg="allocating fdEntry", &
      line=__LINE__, file=FILENAME)) return  ! bail out
    
    ! set values inside of fdEntry
    fdEntry%wrap%standardName     = standardName
    fdEntry%wrap%canonicalUnits   = canonicalUnits
    allocate(fdEntry%wrap%connectedOptions(2), stat=stat)
    if (ESMF_LogFoundAllocError(stat, msg="allocating fdEntry member", &
      line=__LINE__, file=FILENAME)) return  ! bail out
    fdEntry%wrap%connectedOptions(1)     = "false" ! default
    fdEntry%wrap%connectedOptions(2)     = "true"
    allocate(fdEntry%wrap%synonyms(0), stat=stat)
    if (ESMF_LogFoundAllocError(stat, msg="allocating synonyms member", &
      line=__LINE__, file=FILENAME)) return  ! bail out
    
    ! add fdEntry to the FieldDictionary
    call ESMF_ContainerAddUDT(fieldDictionary, &
      trim(fdEntry%wrap%standardName), fdEntry, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    
  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_FieldDictionaryEgestI - Egest NUOPC Field dictionary into FreeFormat
! !INTERFACE:
  subroutine NUOPC_FieldDictionaryEgestI(fieldDictionary, freeFormat, rc)
! !ARGUMENTS:
    type(ESMF_Container),   intent(inout)         :: fieldDictionary
    type(NUOPC_FreeFormat), intent(out)           :: freeFormat
    integer,                intent(out), optional :: rc
! !DESCRIPTION:
!   Egest the contents of the NUOPC Field dictionary into a FreeFormat object.
!   It is the caller's responsibility to destroy the created {\tt freeFormat}
!   object.
!EOPI
  !-----------------------------------------------------------------------------
    type(NUOPC_FieldDictionaryEntry)                :: fdEntry
    integer                                         :: stat, i, k, count
    character(len=NUOPC_FreeFormatLen)              :: tempString
    
    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_ContainerGet(fieldDictionary, itemCount=count, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) &
      return  ! bail out
    
    ! create free format object with estimated capacity
    freeFormat = NUOPC_FreeFormatCreate(capacity=4*count, rc=rc)

    do i=1, count
      call ESMF_ContainerGetUDTByIndex(fieldDictionary, i, fdEntry, &
        ESMF_ITEMORDER_ABC, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) &
        return  ! bail out
      write(tempString, "('standardName:          ',a40)") &
        trim(fdEntry%wrap%standardName)
      call NUOPC_FreeFormatAdd(freeFormat, (/adjustl(tempString)/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) &
        return  ! bail out
      write(tempString, "('canonicalUnits:        ',a40)") &
        trim(fdEntry%wrap%canonicalUnits)
      call NUOPC_FreeFormatAdd(freeFormat, (/adjustl(tempString)/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) &
        return  ! bail out
      do k=1, size(fdEntry%wrap%synonyms)
        write(tempString, "('synonym:               ',a40)") &
          trim(fdEntry%wrap%synonyms(k))
        call NUOPC_FreeFormatAdd(freeFormat, (/adjustl(tempString)/), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) &
          return  ! bail out
      enddo
      tempString="----------------------------------------------------------------"
      call NUOPC_FreeFormatAdd(freeFormat, (/adjustl(tempString)/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) &
        return  ! bail out
    enddo

  end subroutine
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_FieldDictionaryGetEntryI - Get information about a NUOPC Field dictionary entry
! !INTERFACE:
  subroutine NUOPC_FieldDictionaryGetEntryI(fieldDictionary, &
    standardName, canonicalUnits, rc)
! !ARGUMENTS:
    type(ESMF_Container),             intent(inout)         :: fieldDictionary
    character(*),                     intent(in)            :: standardName
    character(*),                     intent(out), optional :: canonicalUnits
    integer,                          intent(out), optional :: rc
! !DESCRIPTION:
!   Query an entry in the NUOPC Field dictionary.
!EOPI
  !-----------------------------------------------------------------------------
    ! local variables
    type(NUOPC_FieldDictionaryEntry)  :: fdEntry
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_ContainerGetUDT(fieldDictionary, trim(standardName), &
      fdEntry, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    if (present(canonicalUnits)) &
      canonicalUnits = trim(fdEntry%wrap%canonicalUnits)

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_FieldDictionaryHasEntryI - Check whether the NUOPC Field dictionary has a specific entry
! !INTERFACE:
  function NUOPC_FieldDictionaryHasEntryI(fieldDictionary, standardName, rc)
! !RETURN VALUE:
    logical :: NUOPC_FieldDictionaryHasEntryI
! !ARGUMENTS:
    type(ESMF_Container),             intent(inout)         :: fieldDictionary
    character(*),                     intent(in)            :: standardName
    integer,                          intent(out), optional :: rc
! !DESCRIPTION:
!   Returns {\tt .true.} if the NUOPC Field dictionary has an entry with the
!   specified StandardName, {\tt .false.} otherwise.
!EOPI
  !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Logical)            :: isPres
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    call c_ESMC_ContainerGetIsPresent(fieldDictionary, trim(standardName), &
      isPres, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    NUOPC_FieldDictionaryHasEntryI = isPres
    
  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_FieldDictionaryMatchSynoI - Check whether the NUOPC Field dictionary considers the standard names synonyms
! !INTERFACE:
  function NUOPC_FieldDictionaryMatchSynoI(fieldDictionary, standardName1, &
    standardName2, rc)
! !RETURN VALUE:
    logical :: NUOPC_FieldDictionaryMatchSynoI
! !ARGUMENTS:
    type(ESMF_Container),         intent(inout)         :: fieldDictionary
    character(*),                 intent(in)            :: standardName1
    character(*),                 intent(in)            :: standardName2
    integer,                      intent(out), optional :: rc
! !DESCRIPTION:
!   Return {\tt .true.} if the NUOPC Field dictionary considers
!   {\tt standardName1} and {\tt standardName2} synonyms, {\tt .false.} 
!   otherwise. An entry with standard name of {\tt standardName1} must
!   exist in the field dictionary, or else an error will be returned in 
!   {\tt rc}, and the return value set to {.false.}.
!   However, {\tt standardName2} need not correspond to an existing entry.
!   If {\tt standardName2} does not correspond to an existing entry in the 
!   field dictionary, the value of {.false.} will be returned.
!EOPI
  !-----------------------------------------------------------------------------
    ! local variables
    integer                           :: i
    type(NUOPC_FieldDictionaryEntry)  :: fdEntry
    
    if (present(rc)) rc = ESMF_SUCCESS
    NUOPC_FieldDictionaryMatchSynoI = .true. ! initialize
    
    if (trim(standardName2) == trim(standardName1)) return  ! early bail out
    
    NUOPC_FieldDictionaryMatchSynoI = .false. ! re-initialize

    call ESMF_ContainerGetUDT(fieldDictionary, trim(standardName1), &
      fdEntry, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    do i=1, size(fdEntry%wrap%synonyms)
      if (trim(standardName2) == trim(fdEntry%wrap%synonyms(i))) then
        NUOPC_FieldDictionaryMatchSynoI = .true.
        exit  ! bail out on finding synonym
      endif
    enddo
    
  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_FieldDictionarySetSynoI - Set synonyms in the NUOPC Field dictionary entry
! !INTERFACE:
  subroutine NUOPC_FieldDictionarySetSynoI(fieldDictionary, &
    standardNames, rc)
! !ARGUMENTS:
    type(ESMF_Container),             intent(inout)         :: fieldDictionary
    character(*),                     intent(in)            :: standardNames(:)
    integer,                          intent(out), optional :: rc
! !DESCRIPTION:
!   Set all of the elements of the {\tt standardNames} argument to be considered
!   synonyms by the field dictionary. Every element in {\tt standardNames} must
!   correspond to the standard name of already existing entries in the field 
!   dictionary, or else an error will be returned.
!EOPI
  !-----------------------------------------------------------------------------
    ! local variables
    integer                           :: i, k, j, stat
    type(NUOPC_FieldDictionaryEntry)  :: fdEntry
    character(ESMF_MAXSTR), pointer   :: synonyms(:)
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    ! first loop to ensure all of the provided standard names are valid entries
    do i=1, size(standardNames)
      call ESMF_ContainerGetUDT(fieldDictionary, trim(standardNames(i)), &
        fdEntry, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    enddo
    
    ! second loop to set the synonyms
    do i=1, size(standardNames)
    
      call ESMF_ContainerGetUDT(fieldDictionary, trim(standardNames(i)), &
        fdEntry, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      
      j = size(fdEntry%wrap%synonyms)
      allocate(synonyms(j+size(standardNames)-1), stat=stat)
      if (ESMF_LogFoundAllocError(stat, msg="allocating synonyms array", &
        line=__LINE__, file=FILENAME)) return  ! bail out
      
      do k=1, j
        synonyms(k) = fdEntry%wrap%synonyms(k)  ! fill in the existing entries
      enddo
      
      do k=1, size(standardNames)
        if (k==i) cycle
        j = j+1
        synonyms(j) = trim(standardNames(k))
      enddo
      
      deallocate(fdEntry%wrap%synonyms)
      fdEntry%wrap%synonyms => synonyms

    enddo

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOPI
! !IROUTINE: NUOPC_FieldDictionaryDefinition - Create the NUOPC Field dictionary
! !INTERFACE:
  subroutine NUOPC_FieldDictionaryDefinition(fieldDictionary, rc)
! !ARGUMENTS:
    type(ESMF_Container), intent(inout)          :: fieldDictionary
    integer,              intent(out),  optional :: rc
! !DESCRIPTION:
!   Create NUOPC Field dictionary.
!EOPI
  !-----------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS

!BOLT l l
! "{\bf StandardName}"
! "{\bf CanonicalUnits}"
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "air_pressure_at_sea_level", &
      canonicalUnits    = "Pa", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "magnitude_of_surface_downward_stress", &
      canonicalUnits    = "Pa", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "precipitation_flux", &
      canonicalUnits    = "kg m-2 s-1", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "sea_surface_height_above_sea_level", &
      canonicalUnits    = "m", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "sea_surface_salinity", &
      canonicalUnits    = "1e-3", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "sea_surface_temperature", &
      canonicalUnits    = "K", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "surface_eastward_sea_water_velocity", &
      canonicalUnits    = "m s-1", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "surface_downward_eastward_stress", &
      canonicalUnits    = "Pa", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "surface_downward_heat_flux_in_air", &
      canonicalUnits    = "W m-2", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "surface_downward_water_flux", &
      canonicalUnits    = "kg m-2 s-1", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "surface_downward_northward_stress", &
      canonicalUnits    = "Pa", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "surface_net_downward_shortwave_flux", &
      canonicalUnits    = "W m-2", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "surface_net_downward_longwave_flux", &
      canonicalUnits    = "W m-2", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!BOTL
    call NUOPC_FieldDictionaryAddEntryI(fieldDictionary, &
      standardName      = "surface_northward_sea_water_velocity", &
      canonicalUnits    = "m s-1", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
!EOTL
!EOLT
  end subroutine
  !-----------------------------------------------------------------------------

end module
