!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!!-------------------------------------------------------------------------------------
!==============================================================================
#define ESMF_FILENAME "ESMF_IO_NCPutGet.F90"
!==============================================================================
!
module ESMF_IO_NCPutGetMod
!
!==============================================================================
!
! This file contains type specific putgets for a NetCDF file
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!------------------------------------------------------------------------------
! !USES:
  use ESMF_LogErrMod
  use ESMF_UtilStringMod
  use ESMF_UtilTypesMod
#ifdef ESMF_NETCDF
  use netcdf
#endif
  implicit none
  private
#define ESMF_NO_GREATER_THAN_4D
  interface ESMF_IO_NCPutGetVar
    !------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
#ifndef PIO_TKR 
#ifndef ESMF_NO_INTEGER_1_BYTE 
 module procedure ESMF_IO_NCPutGetVar1DI1 
 module procedure ESMF_IO_NCPutGetVar2DI1 
 module procedure ESMF_IO_NCPutGetVar3DI1 
 module procedure ESMF_IO_NCPutGetVar4DI1 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_IO_NCPutGetVar5DI1 
 module procedure ESMF_IO_NCPutGetVar6DI1 
 module procedure ESMF_IO_NCPutGetVar7DI1 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
 module procedure ESMF_IO_NCPutGetVar1DI2 
 module procedure ESMF_IO_NCPutGetVar2DI2 
 module procedure ESMF_IO_NCPutGetVar3DI2 
 module procedure ESMF_IO_NCPutGetVar4DI2 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_IO_NCPutGetVar5DI2 
 module procedure ESMF_IO_NCPutGetVar6DI2 
 module procedure ESMF_IO_NCPutGetVar7DI2 
#endif 
#endif 
#endif 
 module procedure ESMF_IO_NCPutGetVar1DI4 
#ifndef PIO_TKR 
 module procedure ESMF_IO_NCPutGetVar1DI8 
#endif 
 module procedure ESMF_IO_NCPutGetVar1DR4 
 module procedure ESMF_IO_NCPutGetVar1DR8 
 module procedure ESMF_IO_NCPutGetVar2DI4 
#ifndef PIO_TKR 
 module procedure ESMF_IO_NCPutGetVar2DI8 
#endif 
 module procedure ESMF_IO_NCPutGetVar2DR4 
 module procedure ESMF_IO_NCPutGetVar2DR8 
 module procedure ESMF_IO_NCPutGetVar3DI4 
#ifndef PIO_TKR 
 module procedure ESMF_IO_NCPutGetVar3DI8 
#endif 
 module procedure ESMF_IO_NCPutGetVar3DR4 
 module procedure ESMF_IO_NCPutGetVar3DR8 
 module procedure ESMF_IO_NCPutGetVar4DI4 
#ifndef PIO_TKR 
 module procedure ESMF_IO_NCPutGetVar4DI8 
#endif 
 module procedure ESMF_IO_NCPutGetVar4DR4 
 module procedure ESMF_IO_NCPutGetVar4DR8 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_IO_NCPutGetVar5DI4 
#ifndef PIO_TKR 
 module procedure ESMF_IO_NCPutGetVar5DI8 
#endif 
 module procedure ESMF_IO_NCPutGetVar5DR4 
 module procedure ESMF_IO_NCPutGetVar5DR8 
#ifndef PIO_TKR 
 module procedure ESMF_IO_NCPutGetVar6DI4 
 module procedure ESMF_IO_NCPutGetVar6DI8 
 module procedure ESMF_IO_NCPutGetVar6DR4 
 module procedure ESMF_IO_NCPutGetVar6DR8 
 module procedure ESMF_IO_NCPutGetVar7DI4 
 module procedure ESMF_IO_NCPutGetVar7DI8 
 module procedure ESMF_IO_NCPutGetVar7DR4 
 module procedure ESMF_IO_NCPutGetVar7DR8 
#endif 
#endif 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

  end interface
!
! !PUBLIC MEMBER FUNCTIONS:
!
! - ESMF-public methods:
  public ESMF_IO_NCPutGetVar
  public ESMF_IO_NCCheckError
! -------------------------- ESMF-public method -------------------------------
contains
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVarmrankDmtypekind 
! 
! !INTERFACE: 
! subroutine ESMF_IO_NCPutGetVarmrankDmtypekind(grid_filename, var_name, & 
! var_buffer, start, count, putflag, rc) 
! character(len=*), intent(in) :: grid_filename 
! character(len=*), intent(in) :: var_name 
! <type> (<kind>), intent(inout) :: var_buffer(<rank>) 
! integer, intent(in), optional :: start(:), count(:) 
! logical, intent(in), optional :: putflag 
! integer, intent(out), optional:: rc 
!EOPI 
 
#ifndef PIO_TKR 
#ifndef ESMF_NO_INTEGER_1_BYTE 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar1Di1 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar1Di1(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 integer (ESMF_KIND_i1), intent(inout) :: var_buffer(:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 integer(ESMF_KIND_i1), pointer :: fptr(:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "1"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar1Di1 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar2Di1 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar2Di1(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 integer (ESMF_KIND_i1), intent(inout) :: var_buffer(:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 integer(ESMF_KIND_i1), pointer :: fptr(:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "2"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar2Di1 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar3Di1 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar3Di1(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 integer (ESMF_KIND_i1), intent(inout) :: var_buffer(:,:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 integer(ESMF_KIND_i1), pointer :: fptr(:,:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 3) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "3"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar3Di1 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar4Di1 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar4Di1(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 integer (ESMF_KIND_i1), intent(inout) :: var_buffer(:,:,:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 integer(ESMF_KIND_i1), pointer :: fptr(:,:,:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "4"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar4Di1 
#ifndef ESMF_NO_GREATER_THAN_4D 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar5Di1 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar5Di1(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 integer (ESMF_KIND_i1), intent(inout) :: var_buffer(:,:,:,:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 integer(ESMF_KIND_i1), pointer :: fptr(:,:,:,:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 5) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "5"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar5Di1 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar6Di1 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar6Di1(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 integer (ESMF_KIND_i1), intent(inout) :: var_buffer(:,:,:,:,:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 integer(ESMF_KIND_i1), pointer :: fptr(:,:,:,:,:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 6) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "6"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar6Di1 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar7Di1 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar7Di1(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 integer (ESMF_KIND_i1), intent(inout) :: var_buffer(:,:,:,:,:,:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 integer(ESMF_KIND_i1), pointer :: fptr(:,:,:,:,:,:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 7) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "7"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar7Di1 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar1Di2 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar1Di2(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 integer (ESMF_KIND_i2), intent(inout) :: var_buffer(:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 integer(ESMF_KIND_i2), pointer :: fptr(:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "1"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar1Di2 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar2Di2 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar2Di2(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 integer (ESMF_KIND_i2), intent(inout) :: var_buffer(:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 integer(ESMF_KIND_i2), pointer :: fptr(:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "2"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar2Di2 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar3Di2 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar3Di2(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 integer (ESMF_KIND_i2), intent(inout) :: var_buffer(:,:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 integer(ESMF_KIND_i2), pointer :: fptr(:,:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 3) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "3"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar3Di2 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar4Di2 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar4Di2(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 integer (ESMF_KIND_i2), intent(inout) :: var_buffer(:,:,:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 integer(ESMF_KIND_i2), pointer :: fptr(:,:,:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "4"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar4Di2 
#ifndef ESMF_NO_GREATER_THAN_4D 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar5Di2 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar5Di2(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 integer (ESMF_KIND_i2), intent(inout) :: var_buffer(:,:,:,:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 integer(ESMF_KIND_i2), pointer :: fptr(:,:,:,:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 5) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "5"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar5Di2 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar6Di2 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar6Di2(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 integer (ESMF_KIND_i2), intent(inout) :: var_buffer(:,:,:,:,:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 integer(ESMF_KIND_i2), pointer :: fptr(:,:,:,:,:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 6) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "6"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar6Di2 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar7Di2 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar7Di2(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 integer (ESMF_KIND_i2), intent(inout) :: var_buffer(:,:,:,:,:,:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 integer(ESMF_KIND_i2), pointer :: fptr(:,:,:,:,:,:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 7) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "7"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar7Di2 
#endif 
#endif 
#endif 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar1Di4 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar1Di4(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 integer (ESMF_KIND_i4), intent(inout) :: var_buffer(:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 integer(ESMF_KIND_i4), pointer :: fptr(:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "1"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar1Di4 
#ifndef PIO_TKR 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar1Di8 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar1Di8(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 integer (ESMF_KIND_i8), intent(inout) :: var_buffer(:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 integer(ESMF_KIND_i8), pointer :: fptr(:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "1"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar1Di8 
#endif 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar1Dr4 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar1Dr4(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 real (ESMF_KIND_r4), intent(inout) :: var_buffer(:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 real(ESMF_KIND_r4), pointer :: fptr(:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "1"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar1Dr4 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar1Dr8 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar1Dr8(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 real (ESMF_KIND_r8), intent(inout) :: var_buffer(:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 real(ESMF_KIND_r8), pointer :: fptr(:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "1"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar1Dr8 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar2Di4 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar2Di4(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 integer (ESMF_KIND_i4), intent(inout) :: var_buffer(:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 integer(ESMF_KIND_i4), pointer :: fptr(:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "2"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar2Di4 
#ifndef PIO_TKR 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar2Di8 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar2Di8(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 integer (ESMF_KIND_i8), intent(inout) :: var_buffer(:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 integer(ESMF_KIND_i8), pointer :: fptr(:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "2"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar2Di8 
#endif 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar2Dr4 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar2Dr4(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 real (ESMF_KIND_r4), intent(inout) :: var_buffer(:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 real(ESMF_KIND_r4), pointer :: fptr(:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "2"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar2Dr4 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar2Dr8 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar2Dr8(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 real (ESMF_KIND_r8), intent(inout) :: var_buffer(:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 real(ESMF_KIND_r8), pointer :: fptr(:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "2"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar2Dr8 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar3Di4 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar3Di4(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 integer (ESMF_KIND_i4), intent(inout) :: var_buffer(:,:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 integer(ESMF_KIND_i4), pointer :: fptr(:,:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 3) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "3"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar3Di4 
#ifndef PIO_TKR 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar3Di8 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar3Di8(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 integer (ESMF_KIND_i8), intent(inout) :: var_buffer(:,:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 integer(ESMF_KIND_i8), pointer :: fptr(:,:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 3) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "3"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar3Di8 
#endif 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar3Dr4 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar3Dr4(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 real (ESMF_KIND_r4), intent(inout) :: var_buffer(:,:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 real(ESMF_KIND_r4), pointer :: fptr(:,:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 3) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "3"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar3Dr4 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar3Dr8 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar3Dr8(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 real (ESMF_KIND_r8), intent(inout) :: var_buffer(:,:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 real(ESMF_KIND_r8), pointer :: fptr(:,:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 3) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "3"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar3Dr8 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar4Di4 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar4Di4(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 integer (ESMF_KIND_i4), intent(inout) :: var_buffer(:,:,:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 integer(ESMF_KIND_i4), pointer :: fptr(:,:,:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "4"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar4Di4 
#ifndef PIO_TKR 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar4Di8 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar4Di8(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 integer (ESMF_KIND_i8), intent(inout) :: var_buffer(:,:,:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 integer(ESMF_KIND_i8), pointer :: fptr(:,:,:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "4"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar4Di8 
#endif 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar4Dr4 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar4Dr4(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 real (ESMF_KIND_r4), intent(inout) :: var_buffer(:,:,:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 real(ESMF_KIND_r4), pointer :: fptr(:,:,:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "4"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar4Dr4 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar4Dr8 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar4Dr8(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 real (ESMF_KIND_r8), intent(inout) :: var_buffer(:,:,:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 real(ESMF_KIND_r8), pointer :: fptr(:,:,:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "4"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar4Dr8 
#ifndef ESMF_NO_GREATER_THAN_4D 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar5Di4 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar5Di4(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 integer (ESMF_KIND_i4), intent(inout) :: var_buffer(:,:,:,:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 integer(ESMF_KIND_i4), pointer :: fptr(:,:,:,:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 5) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "5"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar5Di4 
#ifndef PIO_TKR 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar5Di8 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar5Di8(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 integer (ESMF_KIND_i8), intent(inout) :: var_buffer(:,:,:,:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 integer(ESMF_KIND_i8), pointer :: fptr(:,:,:,:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 5) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "5"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar5Di8 
#endif 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar5Dr4 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar5Dr4(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 real (ESMF_KIND_r4), intent(inout) :: var_buffer(:,:,:,:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 real(ESMF_KIND_r4), pointer :: fptr(:,:,:,:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 5) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "5"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar5Dr4 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar5Dr8 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar5Dr8(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 real (ESMF_KIND_r8), intent(inout) :: var_buffer(:,:,:,:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 real(ESMF_KIND_r8), pointer :: fptr(:,:,:,:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 5) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "5"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar5Dr8 
#ifndef PIO_TKR 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar6Di4 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar6Di4(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 integer (ESMF_KIND_i4), intent(inout) :: var_buffer(:,:,:,:,:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 integer(ESMF_KIND_i4), pointer :: fptr(:,:,:,:,:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 6) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "6"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar6Di4 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar6Di8 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar6Di8(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 integer (ESMF_KIND_i8), intent(inout) :: var_buffer(:,:,:,:,:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 integer(ESMF_KIND_i8), pointer :: fptr(:,:,:,:,:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 6) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "6"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar6Di8 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar6Dr4 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar6Dr4(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 real (ESMF_KIND_r4), intent(inout) :: var_buffer(:,:,:,:,:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 real(ESMF_KIND_r4), pointer :: fptr(:,:,:,:,:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 6) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "6"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar6Dr4 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar6Dr8 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar6Dr8(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 real (ESMF_KIND_r8), intent(inout) :: var_buffer(:,:,:,:,:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 real(ESMF_KIND_r8), pointer :: fptr(:,:,:,:,:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 6) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "6"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar6Dr8 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar7Di4 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar7Di4(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 integer (ESMF_KIND_i4), intent(inout) :: var_buffer(:,:,:,:,:,:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 integer(ESMF_KIND_i4), pointer :: fptr(:,:,:,:,:,:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 7) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "7"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar7Di4 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar7Di8 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar7Di8(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 integer (ESMF_KIND_i8), intent(inout) :: var_buffer(:,:,:,:,:,:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 integer(ESMF_KIND_i8), pointer :: fptr(:,:,:,:,:,:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 7) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "7"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar7Di8 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar7Dr4 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar7Dr4(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 real (ESMF_KIND_r4), intent(inout) :: var_buffer(:,:,:,:,:,:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 real(ESMF_KIND_r4), pointer :: fptr(:,:,:,:,:,:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 7) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "7"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar7Dr4 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_IO_NCPutGetVar##mrank##D##mtypekind" 
!BOPI 
! !ROUTINE: ESMF_IO_NCPutGetVar7Dr8 
! 
! !INTERFACE: 
 subroutine ESMF_IO_NCPutGetVar7Dr8(grid_filename, var_name, & 
 var_buffer, start, count, putflag, rc) 
 character(len=*), intent(in) :: grid_filename 
 character(len=*), intent(in) :: var_name 
 real (ESMF_KIND_r8), intent(inout) :: var_buffer(:,:,:,:,:,:,:) 
 integer, intent(in), optional :: start(:), count(:) 
 logical, intent(in), optional :: putflag 
 integer, intent(out), optional:: rc 
!EOPI 
 
 integer:: ncStatus 
 integer:: gridid, varid, ndims 
 integer:: vardimids(4) 
 integer:: len, i, vartype 
 character(len=ESMF_MAXPATHLEN) :: errmsg 
 logical:: localPutFlag 
 real(ESMF_KIND_r8), pointer :: fptr(:,:,:,:,:,:,:) 
 integer, parameter :: nf90_noerror = 0 
 
#ifdef ESMF_NETCDF 
 
 if (present(putFlag)) then 
 localPutFlag = putFlag 
 else 
 localPutFlag = .FALSE. 
 endif 
 ! Open the grid and mosaic files 
 if (localPutFlag) then 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_write, ncid=gridid) 
 else 
 ncStatus = nf90_open (path=trim(grid_filename), mode=nf90_nowrite, ncid=gridid) 
 endif 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename), & 
 rc)) return 
 
 ncStatus = nf90_inq_varid( gridid, var_name, varid) 
 errmsg = ESMF_StringConcat ("variable ", & 
 ESMF_StringConcat (trim (var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_inquire_variable(gridid, varid, xtype=vartype, & 
 ndims=ndims, dimids=vardimids) 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 if (ndims /= 7) then 
 call ESMF_LogSetError(rcToCheck=ESMF_FAILURE, & 
 msg=ESMF_StringConcat ("- variable dimension is not equal to ", "7"), & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 end if 
 
 if (present(start) .and. present(count)) then 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer, start=start, & 
 count=count) 
 endif 
 else 
 if (localPutFlag) then 
 ncStatus = nf90_put_var(gridid, varid, var_buffer) 
 else 
 ncStatus = nf90_get_var(gridid, varid, var_buffer) 
 endif 
 endif 
 errmsg = ESMF_StringConcat ("Variable ", & 
 ESMF_StringConcat (trim(var_name), & 
 ESMF_StringConcat (" in ", grid_filename))) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 errmsg, & 
 rc)) return 
 
 ncStatus = nf90_close(gridid) 
 if (ESMF_IO_NCCheckError (ncStatus, & 
 ESMF_METHOD, & 
 ESMF_SRCLINE,& 
 trim(grid_filename),& 
 rc)) return 
 
 if(present(rc)) rc = ESMF_SUCCESS 
 return 
#else 
 call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
 msg="- ESMF_NETCDF not defined when lib was compiled", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
#endif 
 
end subroutine ESMF_IO_NCPutGetVar7Dr8 
#endif 
#endif 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------
!
! check CDF file error code
!
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_IO_NCCheckError"
function ESMF_IO_NCCheckError (ncStatus, module, fileName, lineNo, errmsg, rc)
    logical :: ESMF_IO_NCCheckError
    integer, intent(in) :: ncStatus
    character(len=*), intent(in) :: module
    character(len=*), intent(in) :: fileName
    integer, intent(in) :: lineNo
    character(len=*), intent(in) :: errmsg
    integer, intent(out),optional :: rc
    integer, parameter :: nf90_noerror = 0
    ESMF_IO_NCCheckError = .FALSE.
#ifdef ESMF_NETCDF
    if ( ncStatus .ne. nf90_noerror) then
        call ESMF_LogWrite (msg="netCDF Status Return Error", logmsgFlag=ESMF_LOGMSG_ERROR, &
            line=lineNo, file=fileName, method=module)
        print '("NetCDF Error: ", A, " : ", A)', &
                trim(errmsg),trim(nf90_strerror(ncStatus))
        call ESMF_LogFlush()
        if (present(rc)) rc = ESMF_FAILURE
        ESMF_IO_NCCheckError = .TRUE.
    else
       if (present(rc)) rc = ESMF_SUCCESS
       return
    end if
#else
    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, &
                 msg="- ESMF_NETCDF not defined when lib was compiled", &
                 ESMF_CONTEXT, rcToReturn=rc)
    return
#endif
end function ESMF_IO_NCCheckError
end module
