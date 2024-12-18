!  $Id$
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
!
! F77 interface files for C++ layer calling into F90 implementation layer.
!  This cannot use any F90 syntax, including modules, or allocatable 
!   arrays, or ...
!
!==============================================================================
#define ESMF_FILENAME "ESMF_Field_C.F90"
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!==============================================================================
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
!      character(*), parameter, private :: version = &
!      '$Id$'
!==============================================================================

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fieldcreategridas"
  subroutine f_esmf_fieldcreategridas(field, grid_pointer, arrayspec, &
    staggerloc, gridToFieldMap, len1, gtfmpresent, &
    ungriddedLBound, len2, uglbpresent, &
    ungriddedUBound, len3, ugubpresent, name, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_ArraySpecMod
    use ESMF_StaggerLocMod
    use ESMF_GridMod
    use ESMF_FieldMod
    use ESMF_FieldCreateMod

    implicit none

    ! arguments
    type(ESMF_Field)               :: field
    type(ESMF_Pointer)             :: grid_pointer
    type(ESMF_ArraySpec)           :: arrayspec
    type(ESMF_StaggerLoc)          :: staggerloc
    integer, intent(in)            :: len1, len2, len3
    integer, intent(in)            :: gtfmpresent, uglbpresent, ugubpresent
    integer                        :: gridToFieldMap(1:len1), &
                                      ungriddedLBound(1:len2), &
                                      ungriddedUBound(1:len3)
    character(len=*),intent(in)    :: name
    integer, intent(out)           :: rc              
  
    ! local variables  
    type(ESMF_Grid)          :: grid
  
    ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL

    grid%this = grid_pointer

    ESMF_INIT_SET_CREATED(grid)
    
    if (gtfmpresent == 0 .and. uglbpresent == 0 .and. ugubpresent == 0) then
      field = ESMF_FieldCreate(grid, arrayspec=arrayspec, staggerloc=staggerloc, &
        name=name, rc=rc)
    else if (gtfmpresent == 1 .and. uglbpresent == 0 .and. ugubpresent == 0) then
      field = ESMF_FieldCreate(grid, arrayspec=arrayspec, staggerloc=staggerloc, &
        gridToFieldMap=gridToFieldMap, &
        name=name, rc=rc)
    else if (gtfmpresent == 0 .and. uglbpresent == 1 .and. ugubpresent == 0) then
      field = ESMF_FieldCreate(grid, arrayspec=arrayspec, staggerloc=staggerloc, &
        ungriddedLBound=ungriddedLBound, &
        name=name, rc=rc)
    else if (gtfmpresent == 0 .and. uglbpresent == 0 .and. ugubpresent == 1) then
      field = ESMF_FieldCreate(grid, arrayspec=arrayspec, staggerloc=staggerloc, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)
    else if (gtfmpresent == 1 .and. uglbpresent == 1 .and. ugubpresent == 0) then
      field = ESMF_FieldCreate(grid, arrayspec=arrayspec, staggerloc=staggerloc, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedLBound=ungriddedLBound, &
        name=name, rc=rc)
    else if (gtfmpresent == 0 .and. uglbpresent == 1 .and. ugubpresent == 1) then
      field = ESMF_FieldCreate(grid, arrayspec=arrayspec, staggerloc=staggerloc, &
        ungriddedLBound=ungriddedLBound, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)
    else if (gtfmpresent == 1 .and. uglbpresent == 0 .and. ugubpresent == 1) then
      field = ESMF_FieldCreate(grid, arrayspec=arrayspec, staggerloc=staggerloc, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)
    else if (gtfmpresent == 1 .and. uglbpresent == 1 .and. ugubpresent == 1) then
      field = ESMF_FieldCreate(grid, arrayspec=arrayspec, staggerloc=staggerloc, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedLBound=ungriddedLBound, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)
    endif
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
   
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_fieldcreategridas

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fieldcreategridtk"
  subroutine f_esmf_fieldcreategridtk(field, grid_pointer, typekind, &
    staggerloc, gridToFieldMap, len1, gtfmpresent, &
    ungriddedLBound, len2, uglbpresent, &
    ungriddedUBound, len3, ugubpresent, name, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_FieldMod
    use ESMF_FieldCreateMod
    use ESMF_StaggerLocMod
    use ESMF_GridMod

    implicit none

    ! arguments
    type(ESMF_Field)               :: field
    type(ESMF_Pointer)             :: grid_pointer
    type(ESMF_TypeKind_Flag)       :: typekind
    type(ESMF_StaggerLoc)          :: staggerloc
    integer, intent(in)            :: len1, len2, len3
    integer, intent(in)            :: gtfmpresent, uglbpresent, ugubpresent
    integer                        :: gridToFieldMap(1:len1), &
                                      ungriddedLBound(1:len2), &
                                      ungriddedUBound(1:len3)
    character(len=*),intent(in)    :: name
    integer, intent(out)           :: rc              
  
    ! local variables  
    type(ESMF_Grid)          :: grid
 
  ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL

    grid%this = grid_pointer

    ESMF_INIT_SET_CREATED(grid)
 
    if (gtfmpresent == 0 .and. uglbpresent == 0 .and. ugubpresent == 0) then
    field = ESMF_FieldCreate(grid, typekind=typekind, staggerloc=staggerloc, &
        name=name, rc=rc)    
    else if (gtfmpresent == 1 .and. uglbpresent == 0 .and. ugubpresent == 0) then
    field = ESMF_FieldCreate(grid, typekind=typekind, staggerloc=staggerloc, &
        gridToFieldMap=gridToFieldMap, &
        name=name, rc=rc)    
    else if (gtfmpresent == 0 .and. uglbpresent == 1 .and. ugubpresent == 0) then
    field = ESMF_FieldCreate(grid, typekind=typekind, staggerloc=staggerloc, &
        ungriddedLBound=ungriddedLBound, &
        name=name, rc=rc)    
    else if (gtfmpresent == 0 .and. uglbpresent == 0 .and. ugubpresent == 1) then
    field = ESMF_FieldCreate(grid, typekind=typekind, staggerloc=staggerloc, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)    
    else if (gtfmpresent == 1 .and. uglbpresent == 1 .and. ugubpresent == 0) then
    field = ESMF_FieldCreate(grid, typekind=typekind, staggerloc=staggerloc, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedLBound=ungriddedLBound, &
        name=name, rc=rc)    
    else if (gtfmpresent == 0 .and. uglbpresent == 1 .and. ugubpresent == 1) then
    field = ESMF_FieldCreate(grid, typekind=typekind, staggerloc=staggerloc, &
        ungriddedLBound=ungriddedLBound, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)    
    else if (gtfmpresent == 1 .and. uglbpresent == 0 .and. ugubpresent == 1) then
    field = ESMF_FieldCreate(grid, typekind=typekind, staggerloc=staggerloc, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)    
    else if (gtfmpresent == 1 .and. uglbpresent == 1 .and. ugubpresent == 1) then
    field = ESMF_FieldCreate(grid, typekind=typekind, staggerloc=staggerloc, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedLBound=ungriddedLBound, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)    
    endif
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
   
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_fieldcreategridtk

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fieldcreatemeshas"
  subroutine f_esmf_fieldcreatemeshas(field, mesh_pointer, arrayspec, &
    gridToFieldMap, len1, gtfmpresent, &
    ungriddedLBound, len2, uglbpresent, &
    ungriddedUBound, len3, ugubpresent, name, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_ArraySpecMod
    use ESMF_MeshMod
    use ESMF_FieldMod
    use ESMF_FieldCreateMod

    implicit none

    ! arguments
    type(ESMF_Field)               :: field
    type(ESMF_Pointer)             :: mesh_pointer
    type(ESMF_ArraySpec)           :: arrayspec
    integer, intent(in)            :: len1, len2, len3
    integer, intent(in)            :: gtfmpresent, uglbpresent, ugubpresent
    integer                        :: gridToFieldMap(1:len1), &
                                      ungriddedLBound(1:len2), &
                                      ungriddedUBound(1:len3)
    character(len=*),intent(in)    :: name
    integer, intent(out)           :: rc              
  
    ! local variables  
    type(ESMF_Mesh)          :: mesh
  
  ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL

    mesh = ESMF_MeshCreateFromIntPtr(mesh_pointer)
  
    if (gtfmpresent == 0 .and. uglbpresent == 0 .and. ugubpresent == 0) then
    field = ESMF_FieldCreate(mesh, arrayspec=arrayspec, &
        name=name, rc=rc)    
    else if (gtfmpresent == 1 .and. uglbpresent == 0 .and. ugubpresent == 0) then
    field = ESMF_FieldCreate(mesh, arrayspec=arrayspec, &
        gridToFieldMap=gridToFieldMap, &
        name=name, rc=rc)    
    else if (gtfmpresent == 0 .and. uglbpresent == 1 .and. ugubpresent == 0) then
    field = ESMF_FieldCreate(mesh, arrayspec=arrayspec, &
        ungriddedLBound=ungriddedLBound, &
        name=name, rc=rc)    
    else if (gtfmpresent == 0 .and. uglbpresent == 0 .and. ugubpresent == 1) then
    field = ESMF_FieldCreate(mesh, arrayspec=arrayspec, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)    
    else if (gtfmpresent == 1 .and. uglbpresent == 1 .and. ugubpresent == 0) then
    field = ESMF_FieldCreate(mesh, arrayspec=arrayspec, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedLBound=ungriddedLBound, &
        name=name, rc=rc)    
    else if (gtfmpresent == 0 .and. uglbpresent == 1 .and. ugubpresent == 1) then
    field = ESMF_FieldCreate(mesh, arrayspec=arrayspec, &
        ungriddedLBound=ungriddedLBound, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)    
    else if (gtfmpresent == 1 .and. uglbpresent == 0 .and. ugubpresent == 1) then
    field = ESMF_FieldCreate(mesh, arrayspec=arrayspec, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)    
    else if (gtfmpresent == 1 .and. uglbpresent == 1 .and. ugubpresent == 1) then
    field = ESMF_FieldCreate(mesh, arrayspec=arrayspec, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedLBound=ungriddedLBound, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)    
    endif
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
   
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_fieldcreatemeshas

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fieldcreatemeshtk"
  subroutine f_esmf_fieldcreatemeshtk(field, mesh_pointer, typekind, meshloc, &
    gridToFieldMap, len1, gtfmpresent, &
    ungriddedLBound, len2, uglbpresent, &
    ungriddedUBound, len3, ugubpresent, name, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_MeshMod
    use ESMF_FieldMod
    use ESMF_FieldCreateMod

    implicit none

    ! arguments
    type(ESMF_Field)               :: field
    type(ESMF_Pointer)             :: mesh_pointer
    type(ESMF_TypeKind_Flag)       :: typekind
    type(ESMF_MeshLoc)             :: meshloc
    integer, intent(in)            :: len1, len2, len3
    integer, intent(in)            :: gtfmpresent, uglbpresent, ugubpresent
    integer                        :: gridToFieldMap(1:len1), &
                                      ungriddedLBound(1:len2), &
                                      ungriddedUBound(1:len3)
    character(len=*),intent(in)    :: name
    integer, intent(out)           :: rc              
  
    ! local variables  
    type(ESMF_Mesh)          :: mesh
  
  ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL

    mesh = ESMF_MeshCreateFromIntPtr(mesh_pointer)

    if (gtfmpresent == 0 .and. uglbpresent == 0 .and. ugubpresent == 0) then
    field = ESMF_FieldCreate(mesh, typekind=typekind, meshloc=meshloc, &
        name=name, rc=rc)    
    else if (gtfmpresent == 1 .and. uglbpresent == 0 .and. ugubpresent == 0) then
    field = ESMF_FieldCreate(mesh, typekind=typekind, meshloc=meshloc, &
        gridToFieldMap=gridToFieldMap, &
        name=name, rc=rc)    
    else if (gtfmpresent == 0 .and. uglbpresent == 1 .and. ugubpresent == 0) then
    field = ESMF_FieldCreate(mesh, typekind=typekind, meshloc=meshloc, &
        ungriddedLBound=ungriddedLBound, &
        name=name, rc=rc)    
    else if (gtfmpresent == 0 .and. uglbpresent == 0 .and. ugubpresent == 1) then
    field = ESMF_FieldCreate(mesh, typekind=typekind, meshloc=meshloc, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)    
    else if (gtfmpresent == 1 .and. uglbpresent == 1 .and. ugubpresent == 0) then
    field = ESMF_FieldCreate(mesh, typekind=typekind, meshloc=meshloc, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedLBound=ungriddedLBound, &
        name=name, rc=rc)    
    else if (gtfmpresent == 0 .and. uglbpresent == 1 .and. ugubpresent == 1) then
    field = ESMF_FieldCreate(mesh, typekind=typekind, meshloc=meshloc, &
        ungriddedLBound=ungriddedLBound, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)    
    else if (gtfmpresent == 1 .and. uglbpresent == 0 .and. ugubpresent == 1) then
    field = ESMF_FieldCreate(mesh, typekind=typekind, meshloc=meshloc, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)    
    else if (gtfmpresent == 1 .and. uglbpresent == 1 .and. ugubpresent == 1) then
    field = ESMF_FieldCreate(mesh, typekind=typekind, meshloc=meshloc, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedLBound=ungriddedLBound, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)    
    endif
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
   
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_fieldcreatemeshtk

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fieldcreatelocstreamas"
  subroutine f_esmf_fieldcreatelocstreamas(field, locstream_pointer, arrayspec, &
    staggerloc, gridToFieldMap, len1, gtfmpresent, &
    ungriddedLBound, len2, uglbpresent, &
    ungriddedUBound, len3, ugubpresent, name, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_ArraySpecMod
    use ESMF_StaggerLocMod
    use ESMF_LocStreamMod
    use ESMF_FieldMod
    use ESMF_FieldCreateMod

    implicit none

    ! arguments
    type(ESMF_Field)               :: field
    type(ESMF_LocStreamType)             :: locstream_pointer
    type(ESMF_ArraySpec)           :: arrayspec
    type(ESMF_StaggerLoc)          :: staggerloc
    integer, intent(in)            :: len1, len2, len3
    integer, intent(in)            :: gtfmpresent, uglbpresent, ugubpresent
    integer                        :: gridToFieldMap(1:len1), &
                                      ungriddedLBound(1:len2), &
                                      ungriddedUBound(1:len3)
    character(len=*),intent(in)    :: name
    integer, intent(out)           :: rc              
  
    ! local variables  
    type(ESMF_LocStream)          :: locstream
  
    ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL

    locstream%lstypep = locstream_pointer

    ESMF_INIT_SET_CREATED(locstream)
    
    if (gtfmpresent == 0 .and. uglbpresent == 0 .and. ugubpresent == 0) then
      field = ESMF_FieldCreate(locstream, arrayspec=arrayspec, &
        name=name, rc=rc)
    else if (gtfmpresent == 1 .and. uglbpresent == 0 .and. ugubpresent == 0) then
      field = ESMF_FieldCreate(locstream, arrayspec=arrayspec, &
        gridToFieldMap=gridToFieldMap, &
        name=name, rc=rc)
    else if (gtfmpresent == 0 .and. uglbpresent == 1 .and. ugubpresent == 0) then
      field = ESMF_FieldCreate(locstream, arrayspec=arrayspec, &
        ungriddedLBound=ungriddedLBound, &
        name=name, rc=rc)
    else if (gtfmpresent == 0 .and. uglbpresent == 0 .and. ugubpresent == 1) then
      field = ESMF_FieldCreate(locstream, arrayspec=arrayspec, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)
    else if (gtfmpresent == 1 .and. uglbpresent == 1 .and. ugubpresent == 0) then
      field = ESMF_FieldCreate(locstream, arrayspec=arrayspec, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedLBound=ungriddedLBound, &
        name=name, rc=rc)
    else if (gtfmpresent == 0 .and. uglbpresent == 1 .and. ugubpresent == 1) then
      field = ESMF_FieldCreate(locstream, arrayspec=arrayspec, &
        ungriddedLBound=ungriddedLBound, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)
    else if (gtfmpresent == 1 .and. uglbpresent == 0 .and. ugubpresent == 1) then
      field = ESMF_FieldCreate(locstream, arrayspec=arrayspec, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)
    else if (gtfmpresent == 1 .and. uglbpresent == 1 .and. ugubpresent == 1) then
      field = ESMF_FieldCreate(locstream, arrayspec=arrayspec, &
        gridToFieldMap=gridToFieldMap, &
        ungriddedLBound=ungriddedLBound, &
        ungriddedUBound=ungriddedUBound, &
        name=name, rc=rc)
    endif
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
   
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_fieldcreatelocstreamas

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fieldcreatelocstreamtk"
  subroutine f_esmf_fieldcreatelocstreamtk(field, locstream, typekind, &
    gridToFieldMap, len1, gtfmpresent, &
    ungriddedLBound, len2, uglbpresent, &
    ungriddedUBound, len3, ugubpresent, name, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_FieldMod
    use ESMF_FieldCreateMod
    use ESMF_StaggerLocMod
    use ESMF_LocStreamMod

    implicit none

    ! arguments
    type(ESMF_Field)               :: field
    type(ESMF_LocStream)             :: locstream
    type(ESMF_TypeKind_Flag)       :: typekind
    type(ESMF_StaggerLoc)          :: staggerloc
    integer, intent(in)            :: len1, len2, len3
    integer, intent(in)            :: gtfmpresent, uglbpresent, ugubpresent
    integer                        :: gridToFieldMap(1:len1), &
                                      ungriddedLBound(1:len2), &
                                      ungriddedUBound(1:len3)
    character(len=*),intent(in)    :: name
    integer, intent(out)           :: rc              
  
    ! local variables  
!    type(ESMF_LocStream)          :: locstream
 
  ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL

!    locstream%lstypep = locstream_pointer

    ESMF_INIT_SET_CREATED(locstream)
 
    if (gtfmpresent == 0 .and. uglbpresent == 0 .and. ugubpresent == 0) then
        field = ESMF_FieldCreate(locstream, typekind=typekind, &
                                 name=name, rc=rc)    
    else if (gtfmpresent == 1 .and. uglbpresent == 0 .and. ugubpresent == 0) then
        field = ESMF_FieldCreate(locstream, typekind=typekind, &
                                 gridToFieldMap=gridToFieldMap, &
                                 name=name, rc=rc)    
    else if (gtfmpresent == 0 .and. uglbpresent == 1 .and. ugubpresent == 0) then
        field = ESMF_FieldCreate(locstream, typekind=typekind, &
                                 ungriddedLBound=ungriddedLBound, &
                                 name=name, rc=rc)    
    else if (gtfmpresent == 0 .and. uglbpresent == 0 .and. ugubpresent == 1) then
        field = ESMF_FieldCreate(locstream, typekind=typekind, &
                                 ungriddedUBound=ungriddedUBound, &
                                 name=name, rc=rc)    
    else if (gtfmpresent == 1 .and. uglbpresent == 1 .and. ugubpresent == 0) then
        field = ESMF_FieldCreate(locstream, typekind=typekind, &
                                 gridToFieldMap=gridToFieldMap, &
                                 ungriddedLBound=ungriddedLBound, &
                                 name=name, rc=rc)    
    else if (gtfmpresent == 0 .and. uglbpresent == 1 .and. ugubpresent == 1) then
        field = ESMF_FieldCreate(locstream, typekind=typekind, &
                                 ungriddedLBound=ungriddedLBound, &
                                 ungriddedUBound=ungriddedUBound, &
                                 name=name, rc=rc)    
    else if (gtfmpresent == 1 .and. uglbpresent == 0 .and. ugubpresent == 1) then
        field = ESMF_FieldCreate(locstream, typekind=typekind, &
                                 gridToFieldMap=gridToFieldMap, &
                                 ungriddedUBound=ungriddedUBound, &
                                 name=name, rc=rc)    
    else if (gtfmpresent == 1 .and. uglbpresent == 1 .and. ugubpresent == 1) then
        field = ESMF_FieldCreate(locstream, typekind=typekind, &
                                 gridToFieldMap=gridToFieldMap, &
                                 ungriddedLBound=ungriddedLBound, &
                                 ungriddedUBound=ungriddedUBound, &
                                 name=name, rc=rc)    
    endif
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_fieldcreatelocstreamtk


#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fieldprint"
  subroutine f_esmf_fieldprint(field, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_ArraySpecMod
    use ESMF_FieldMod
    use ESMF_FieldPrMod

    implicit none

    type(ESMF_Field),intent(inout) :: field
    integer, intent(out)           :: rc              

    integer :: localrc

    localrc = ESMF_RC_NOT_IMPL

    call ESMF_FieldPrint(field, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_fieldprint

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fieldcast"
  subroutine f_esmf_fieldcast(fieldOut, fieldIn, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_ArraySpecMod
    use ESMF_FieldMod
    use ESMF_FieldPrMod

    implicit none

    type(ESMF_Field),intent(inout) :: fieldOut
    type(ESMF_Field),intent(inout) :: fieldIn
    integer, intent(out)           :: rc              

    integer :: localrc

    localrc = ESMF_RC_NOT_IMPL

    ! simple assignment
    fieldOut = fieldIn

    ! return successfully
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_fieldcast

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fieldgetmesh"
  subroutine f_esmf_fieldgetmesh(field, meshp, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_ArraySpecMod
    use ESMF_MeshMod
    use ESMF_FieldMod
    use ESMF_FieldGetMod

    implicit none

    type(ESMF_Field),intent(inout) :: field
    type(ESMF_Pointer)             :: meshp
    integer, intent(out)           :: rc              

    type(ESMF_Mesh)             :: mesh

    rc = ESMF_RC_NOT_IMPL

    call ESMF_FieldGet(field, mesh=mesh, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    meshp = mesh%this;

    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_fieldgetmesh

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fieldgetarray"
  subroutine f_esmf_fieldgetarray(field, array, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_ArraySpecMod
    use ESMF_ArrayMod
    use ESMF_FieldMod
    use ESMF_FieldGetMod

    implicit none

    ! arguments
    type(ESMF_Field),intent(inout) :: field
    type(ESMF_Array)               :: array
    integer, intent(out)           :: rc              

    ! local
    type(ESMF_Array)               :: l_array

    rc = ESMF_RC_NOT_IMPL

    call ESMF_FieldGet(field, array=l_array, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! because ESMF_Array.this is private, it cannot be accessed directly
    ! we use the public interface to do the ptr copy;
    ! the array object returned to the C interface must consist only of the
    ! this pointer. It must not contain the isInit member.
    call ESMF_ArrayCopyThis(l_array, array, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_fieldgetarray

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fielddestroy"
  subroutine f_esmf_fielddestroy(field, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_FieldMod
    use ESMF_FieldCreateMod

    implicit none

    type(ESMF_Field)               :: field
    integer, intent(out)           :: rc     
  
  ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL
  
    call ESMF_FieldDestroy(field, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
  
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_fielddestroy


#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fieldgetbounds"
  subroutine f_esmf_fieldgetbounds(field, localDe, exclusiveLBound, len1, exclusiveUBound, len2, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_FieldMod
    use ESMF_FieldCreateMod
    use ESMF_FieldGetMod

    implicit none

    type(ESMF_Field)     :: field
    integer              :: localDe
    integer              :: len1, len2
    integer              :: exclusiveLBound(1:len1)
    integer              :: exclusiveUBound(1:len2)
    integer, intent(out) :: rc

  ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL

    call ESMF_FieldGetBounds(field, localDe=localDe, &
      exclusiveLBound=exclusiveLBound, exclusiveUBound=exclusiveUBound, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    rc = ESMF_SUCCESS

  end subroutine f_esmf_fieldgetbounds


subroutine f_esmf_fieldcollectgarbage(field, rc)
#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fieldcollectgarbage()"
    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_FieldMod
    use ESMF_FieldCreateMod

    implicit none

    type(ESMF_Field)      :: field
    integer, intent(out)  :: rc     
  
    integer :: localrc              
  
    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    rc = ESMF_RC_NOT_IMPL
  
    !print *, "collecting Field garbage"
    
    ! destruct internal data allocations
    call ESMF_FieldDestruct(field%ftypep, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! deallocate actual FieldType allocation      
    if (associated(field%ftypep)) then
      !print *, "deallocate(field%ftypep)"
      deallocate(field%ftypep, stat=localrc)
      if (ESMF_LogFoundAllocError(localrc, msg="Deallocating Field", &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    nullify(field%ftypep)

    ! return successfully  
    rc = ESMF_SUCCESS

  end subroutine f_esmf_fieldcollectgarbage

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fieldread"
  subroutine f_esmf_fieldread (field,  &
      file, variableName, timeSlice, iofmt, rc)
    use ESMF_FieldMod
    use ESMF_FieldPrMod
    use ESMF_LogErrMod
    use ESMF_UtilTypesMod

    implicit none

    type(ESMF_Field), intent(inout)   :: field
    character(*),     intent(in)      :: file
    character(*),     intent(in)      :: variableName
    integer,          intent(in)      :: timeSlice
    type(ESMF_IOFmt_Flag), intent(in) :: iofmt
    integer,          intent(out)     :: rc

    integer :: localrc

! if (present (variableName)) then
! print *, ESMF_METHOD, ': file = ', file, ', variableName = ', variableName
! else
! print *, ESMF_METHOD, ': file = ', file, ', variableName not present'
! end if
! print *, ESMF_METHOD, ': timeSlice =', timeSlice
    call ESMF_FieldRead (field, file,  &
        variableName=variablename, timeSlice=timeSlice, iofmt=iofmt,  &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    rc = ESMF_SUCCESS

  end subroutine f_esmf_fieldread

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_fieldwrite"
  subroutine f_esmf_fieldwrite (field,  &
      file, variableName, overwrite, status, timeSlice, iofmt, rc)
    use ESMF_FieldMod
    use ESMF_FieldWrMod
    use ESMF_LogErrMod
    use ESMF_UtilTypesMod

    implicit none

    type(ESMF_Field), intent(inout)   :: field
    character(*),     intent(in)      :: file
    character(*),     intent(in), optional :: variableName
    logical,          intent(in)      :: overwrite
    type(ESMF_FileStatus_Flag), intent(in) :: status
    integer,          intent(in)      :: timeSlice
    type(ESMF_IOFmt_Flag), intent(in) :: iofmt
    integer,          intent(out)     :: rc

    integer :: localrc

! if (present (variableName)) then
! print *, ESMF_METHOD, ': file = ', file, ', variableName = ', variableName
! else
! print *, ESMF_METHOD, ': file = ', file, ', variableName not present'
! end if
! print *, ESMF_METHOD, ': overwrite = ', overwrite, ', timeSlice =', timeSlice
    call ESMF_FieldWrite (field, fileName=file,  &
        variableName=variablename,  &
        overwrite=overwrite, status=status, timeSlice=timeSlice, iofmt=iofmt,  &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    rc = ESMF_SUCCESS

  end subroutine f_esmf_fieldwrite

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_regridgetarea"
  subroutine f_esmf_regridgetarea(field, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_FieldRegridMod
    use ESMF_FieldMod

    implicit none

    type(ESMF_Field)        :: field
    integer                 :: rc

    integer :: localrc

    ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    call ESMF_FieldRegridGetArea(field, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    rc = ESMF_SUCCESS

  end subroutine f_esmf_regridgetarea


#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_regridstore"
  subroutine f_esmf_regridstore(srcField, dstField, &
                                srcMaskValues, len1, &
                                dstMaskValues, len2, &
                                routehandle, &
                                regridmethod, &
                                polemethod, &
                                regridPoleNPnts, &
                                linetype, &
                                normtype, &
                                unmappedaction, &
                                ignoreDegenerate, &
                                srcFracField, &
                                dstFracField, &
                                rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_RHandleMod
    use ESMF_FieldRegridMod
    use ESMF_FieldMod

    implicit none

      type(ESMF_Field)                        :: srcField
      type(ESMF_Field)                        :: dstField
      integer                                 :: len1, len2
      integer,optional                        :: srcMaskValues(len1), &
                                                 dstMaskValues(len2)
      type(ESMF_RouteHandle),optional         :: routehandle
      type(ESMF_RegridMethod_Flag),optional   :: regridmethod
      type(ESMF_PoleMethod_Flag),optional     :: polemethod
      integer,optional                        :: regridPoleNPnts

      type(ESMF_LineType_Flag),optional       :: linetype
      type(ESMF_NormType_Flag),optional       :: normtype
      type(ESMF_UnmappedAction_Flag),optional :: unmappedaction
      logical,optional                        :: ignoreDegenerate
      type(ESMF_Field),optional               :: srcFracField
      type(ESMF_Field),optional               :: dstFracField
      integer,optional                        :: rc 

    integer :: localrc
    type(ESMF_RouteHandle) :: l_routehandle

    ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    call ESMF_FieldRegridStore(srcField, dstField, &
                               srcMaskValues=srcMaskValues, &
                               dstMaskValues=dstMaskValues, &
                               routehandle=l_routehandle, &
                               regridmethod=regridmethod, &
                               polemethod=polemethod, &
                               regridPoleNPnts=regridPoleNPnts, &
                               lineType=linetype, &
                               normType=normtype, &
                               unmappedaction=unmappedaction, &
                               ignoreDegenerate=ignoreDegenerate, &
                               srcFracField=srcFracField, &
                               dstFracField=dstFracField, &
                               rc=localrc)

    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! because ESMF_RouteHandle.this is private, it cannot be accessed directly
    ! we use the public interface to do the ptr copy;
    ! the array object returned to the C interface must consist only of the
    ! this pointer. It must not contain the isInit member.
    call ESMF_RoutehandleCopyThis(l_routehandle, routehandle, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_regridstore

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_regrid"
  subroutine f_esmf_regrid(srcField, dstField, routehandle, zeroregion, zrpresent, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_RHandleMod
    use ESMF_FieldRegridMod
    use ESMF_FieldMod

    implicit none

    type(ESMF_Field)        :: srcField
    type(ESMF_Field)        :: dstField
    type(ESMF_RouteHandle)  :: routehandle
    type(ESMF_Region_Flag)  :: zeroregion
    integer, intent(in)     :: zrpresent
    integer                 :: rc 

    integer :: localrc
    type(ESMF_RouteHandle)  :: l_routehandle
  
    ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! Must first create a proper ESMF_RouteHandle that contains the 
    ! required "isInit" class member.
    ! Copy the this pointer a new ESMF_RouteHandle object
    call ESMF_RouteHandleCopyThis(routehandle, l_routehandle, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! set the valid init code of the new object
    call ESMF_RouteHandleSetInitCreated(l_routehandle, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! handle the zeroregion flag
    if (zrpresent == 0) then
      call ESMF_FieldRegrid(srcField, dstField, routehandle=l_routehandle, &
        rc=localrc)
    elseif (zrpresent == 1) then
      call ESMF_FieldRegrid(srcField, dstField, routehandle=l_routehandle, &
        zeroregion=zeroregion, rc=localrc)
    endif
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
  
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_regrid

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_regridrelease"
  subroutine f_esmf_regridrelease(routehandle, rc)

    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_RHandleMod
    use ESMF_FieldRegridMod
    use ESMF_FieldMod

    implicit none

    type(ESMF_RouteHandle)  :: routehandle
    integer                 :: rc 

    integer :: localrc
    type(ESMF_RouteHandle)  :: l_routehandle
  
    ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! Must first create a proper ESMF_RouteHandle that contains the 
    ! required "isInit" class member.
    ! Copy the this pointer a new ESMF_RouteHandle object
    call ESMF_RouteHandleCopyThis(routehandle, l_routehandle, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! set the valid init code of the new object
    call ESMF_RouteHandleSetInitCreated(l_routehandle, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_FieldRegridRelease(routehandle=l_routehandle, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
  
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_regridrelease


