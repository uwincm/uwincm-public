// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMF Interface C++ definition include file
//
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
//
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMCI_F90INTERFACE_H
#define ESMCI_F90INTERFACE_H

//-------------------------------------------------------------------------
//BOP
//
// !DESCRIPTION:
// This file contains types and macros that are used on the C++ side to
// deal with the C++-to-F90 and F90-to-C++ interface.
//-------------------------------------------------------------------------
//EOP

//-------------------------------------------------------------------------
// Class that helps with [optinal] F90 array arguments on the interface
//-------------------------------------------------------------------------

#include <vector>
#include <cstddef>
#include <iostream>

namespace ESMCI {

  class F90ClassHolder{
    void *memoryHolder[16]; // Reserves 16 times the space of a void pointer.
                            // This value has been determined empirically to
                            // work on all the supported platforms.
    friend std::ostream& operator<<(std::ostream& out,
      const F90ClassHolder& f90p){
      out << f90p.memoryHolder[0];
      return out;
    }
   public:
    F90ClassHolder(){}              // default constructor
    F90ClassHolder(void **udtPtr);  // constructor that converts from UDT
    int castToFortranUDT(void **udtPtr);
  };

  //--------------------------------------------------------------------------

  class InterfaceInt{
    public: // this thin class is public to make it's usage uncomplicated
      int *array;
      int dimCount;
      int extent[7];    // size 7 reflects the Fortran limit
    public:
      InterfaceInt();                           // constructor
      InterfaceInt(int *arrayArg, int lenArg);  // constructor
      InterfaceInt(std::vector<int> &arrayArg); // constructor
      InterfaceInt(int *arrayArg, int dimArg, const int *lenArg); // constructor
      ~InterfaceInt(void);                      // destructor
      void set();
      void set(int *arrayArg, int lenArg);
      void set(std::vector<int> &arrayArg);
      void set(int *arrayArg, int dimArg, const int *lenArg);
  };
  
  bool present(InterfaceInt *ptr);  // simplify checking whether object present

} // namespace ESMCI


//-------------------------------------------------------------------------
// Macros that are used in the C glue code for F90-to-C++
//-------------------------------------------------------------------------

// Pass not-present F90 optional argument through to C++ as NULL, otherwise
// leave unchanged.
#define ESMC_NOT_PRESENT_FILTER(arg) \
                           (!ESMC_PRESENT(arg) ? ESMC_NULL_POINTER : (arg))

// Macros for dynamicaly allocated (deep) objects to detect NULL object
// pointers being passed to C++.  This prevents the otherwise subsequent
// method invocations and member dereferences on non-existent objects,
// thereby preventing crashing or other corruptive behavior.

#if !defined(ESMF_NO_INITIALIZERS)
  #define ESMF_CHECK_POINTER(ptr, status) \
            if ((ptr) == ESMC_NULL_POINTER) { \
              if (ESMC_PRESENT(status)) *(status) = ESMF_FAILURE; \
              return; \
            }

  #define ESMF_CHECK_BINARY_OPERATOR_POINTERS(ptr1, ptr2, result) \
          if ((ptr1) == ESMC_NULL_POINTER || (ptr2) == ESMC_NULL_POINTER) { \
            *(result) = ESMF_FALSE; \
            return; \
          }
#else
  // TODO: delete this else clause when F95 initializers universally supported
  // TODOgjt: actually I think with the initializer macros it's o.k. to remove
  // TODOgjt: on the other hand I wonder how useful these macros are anyway
  // TODOgjt: especially because they don't do correct LogErr handling!
  #define ESMF_CHECK_POINTER(ptr, status)
  #define ESMF_CHECK_BINARY_OPERATOR_POINTERS(ptr1, ptr2, result)
#endif

#endif // ESMCI_F90INTERFACE_H
