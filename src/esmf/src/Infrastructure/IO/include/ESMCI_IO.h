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
//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef __ESMCI_IO_H
#define __ESMCI_IO_H

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::IO - IO
//
// !DESCRIPTION:
//
// The code in this file defines the C++ {\tt IO} members and method
// signatures (prototypes).  The companion file {\tt ESMCI\_IO.C}
// contains the full code (bodies) for the {\tt IO} methods.
//
//EOPI
//-------------------------------------------------------------------------

#include "ESMCI_Base.h"       // Base is superclass to IO
#include "ESMCI_VM.h"
#include "ESMCI_Array.h"
#include "ESMC_Util.h"
#include "ESMCI_IO_Handler.h"

#include <cstdio>
#include <vector>

//-------------------------------------------------------------------------

namespace ESMCI {

  enum IOListObjectType { IO_NULL = 1,
                          IO_ARRAY,
                          IO_ATTRIBUTE,
                          IO_GRID,
                          IO_MESH };

  typedef union {
    Array *arr;
    Attribute *attr;
  } IO_ObjectType;

  struct IO_ObjectContainer {
    enum IOListObjectType type;
    IO_ObjectType object;           // e.g., Array, Attribute
    char name[ESMF_MAXSTR];
    ESMC_I8 number;

    IO_ObjectContainer () {
      type = IO_NULL;
      object.arr = (Array *)NULL;
      name[0] = '\0';
      number = 0;
    }
    IO_ObjectContainer (Array *arr_p, const char * const arrName) {
      type = IO_ARRAY;
      object.arr = arr_p;
      if ((const char * const)NULL != arrName) {
        int len = strlen(arrName);
        if (len >= ESMF_MAXSTR) {
          strncpy(name, arrName, (ESMF_MAXSTR - 1));
          name[ESMF_MAXSTR - 1] = '\0';
        } else {
          strcpy(name, arrName);
        }
      } else {
        name[0] = '\0';
      }
      number = 0;
    }
    ~IO_ObjectContainer() {
      name[0] = '\0';
      object.arr = (Array *)NULL;
      number = 0;
      type = IO_NULL;
    }
    Array *getArray(void) {
      return object.arr;
    }
    Attribute *getAttribute(void) {
      return object.attr;
    }
    const char *getName(void) {
      return name;
    }

  };


  // classes and structs

  class IO;

  // class definitions
  
  //===========================================================================
  
  //===========================================================================
  
  //===========================================================================
  class IO {
  
  private:
  // global information
    IO_Handler    *ioHandler;
    std::vector<IO_ObjectContainer *> objects;
    
  public:
    // native constructor and destructor
    IO(int *rc = NULL) {
      ioHandler = (IO_Handler *)NULL;
      // No constructor call for objects -- use default Allocator
      // return successfully
      if (rc != NULL) {
        *rc = ESMF_SUCCESS;
      }
    }
  public:
    ~IO() { destruct(); }
    // create() and destroy()
    static IO *create(int *rc = NULL);
    static int destroy(IO **io);
  private:
    void destruct(void);
  public:
    // read()
    // An atomic read function which transparently handles open and close
    int read(const char * const file, ESMC_IOFmt_Flag iofmt,
             int *timeslice = NULL);

    // A non-atomic read which is only successful on an open IO stream
    int read(int *timeslice = NULL);

    // write()
    // An atomic write function which transparently handles open and close
    // This version closely matches the functionality in the ESMF
    // interface. If NULL is passed for any arguments (except for file which is
    // is required), defaults will be used.
    int write(const char * const file,
              ESMC_IOFmt_Flag iofmt,
              bool overwrite,
              ESMC_FileStatus_Flag status,
              int *timeslice = NULL);

    // A non-atomic write which is only successful on an open IO stream
    int write(int *timeslice = NULL);

    // get() and set()
    const char *getName() const { return "ESMCI::IO"; }

    // match()
    static bool match(IO const * const io1, IO const * const io2,
                      int *rc = NULL) {
      if (rc != (int *)NULL) {
        *rc = ESMF_SUCCESS;
      }
      return (io1 == io2);
    }

    // open() and close()
    int open(const char * const file,
             ESMC_FileStatus_Flag filestatusflag,
             ESMC_IOFmt_Flag iofmt,
             bool overwrite = false,
             bool readonly = false);
    int flush(void);
    int close(void);

    // add and remove objects
    int addArray(Array *arr_p,
                 const char * const variableName);
// TBI
#if 0
    void addAttributes(ESMC_Base *obj_p,
                       const char *schemaFileName,
                       const char *convention,
                       const char *purpose,
                       int *rc=NULL);
    void addGrid(ESMC_Base *grid_p, char *gridName,
                 int *rc=NULL);
#endif // TBI
    bool redist_check(Array *array_p, int *rc);
    void redist_arraycreate1de(Array *src_array_p, Array **dst_array_p, int *rc);
    bool undist_check(Array *array_p, int *rc);
    void undist_arraycreate_alldist(Array *src_array_p, Array **dst_array_p, int *rc);
    void clear();

// TBI
#if 0
    int print() const;
    int validate() const;

    // serialize() and deserialize()
    int serialize(char *buffer, int *length, int *offset,
                  const ESMC_AttReconcileFlag &attreconflag,
                  const ESMC_InquireFlag &inquireflag) const;
    int deserialize(char *buffer, int *offset,
                    const ESMC_AttReconcileFlag &attreconflag);
#endif // TBI

// Don't know yet if we will need these
#if 0
  private:
    // Attribute writing
    int writeStartElement(const std::string& name,
                          const std::string& value,
                          const int     indentLevel,
                          const int     nPairs, ...); // nPairs of
                 // (char *attrName, char *attrValue)

    int writeElement(const std::string& name,
                     const std::string& value,
                     const int     indentLevel,
                     const int     nPairs, ...); // nPairs of
                 // (char *attrName, char *attrValue)

    int writeEndElement(const std::string& name,
                        const int     indentLevel);

    // write an XML comment
    int writeComment(const std::string& comment, const int indentLevel=0);

    int write(int fileNameLen, const char* fileName,
              const char* outChars, int flag);
#endif
  };  // class IO
  //===========================================================================
  
} // namespace ESMCI

#endif // __ESMCI_IO_H
