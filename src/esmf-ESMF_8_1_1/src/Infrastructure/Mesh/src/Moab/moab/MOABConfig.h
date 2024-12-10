#ifndef _SRC_MOAB_MOABCONFIG_H
#define _SRC_MOAB_MOABCONFIG_H 1
 
/* src/moab/MOABConfig.h. Generated automatically at end of configure. */
/* src/moab/MOABConfig.h.  Generated from MOABConfig.h.in by configure.  */
/* config/MOABConfig.h.in.  Generated from configure.ac by autoheader.  */

/* Define if building universal (internal helper macro) */
/* #undef AC_APPLE_UNIVERSAL_BUILD */

/* Configuration command along with user-specified options. */
#ifndef MOAB_CONFIGURE_COMMAND
#define MOAB_CONFIGURE_COMMAND "./configure  '--enable-shared' '--enable-debug' '--with-mpi=/usr/local' '--with-netcdf=/usr/local' '--prefix=/home/ryan/sandbox/MOAB510'"
#endif

/* Configuration information. */
#ifndef MOAB_CONFIGURE_INFO
#define MOAB_CONFIGURE_INFO "./configure run on Sun Jun 23 09:56:23 MDT 2019"
#endif

/* Define to alternate name for `main' routine that is called from a `main' in
   the Fortran libraries. */
#ifndef MOAB_F77_MAIN
#define MOAB_F77_MAIN main
#endif

/* Define to a macro mangling the given C identifier (in lower and upper
   case), which must not contain underscores, for linking with Fortran. */
#ifndef MOAB_FC_FUNC
#define MOAB_FC_FUNC(name,NAME) name ## _
#endif

/* As FC_FUNC, but for C identifiers containing underscores. */
#ifndef MOAB_FC_FUNC_
#define MOAB_FC_FUNC_(name,NAME) name ## _
#endif

/* Define to alternate name for `main' routine that is called from a `main' in
   the Fortran libraries. */
#ifndef MOAB_FC_MAIN
#define MOAB_FC_MAIN main
#endif

/* Use int32_t for handles */
/* #undef FORCE_32_BIT_HANDLES */

/* Use int64_t for handles */
/* #undef FORCE_64_BIT_HANDLES */

/* Enable use of AHF data-structures for querying adjacency information */
/* #undef HAVE_AHF */

/* Define if you have a BLAS library. */
#ifndef MOAB_HAVE_BLAS
#define MOAB_HAVE_BLAS 1
#endif

/* Define if cbrt is available. */
/* #undef HAVE_CBRT */

/* "Define if configured with CCM I/O support." */
/* #undef HAVE_CCMIO */

/* Define to 1 if you have the <ccmiocore.h> header file. */
/* #undef HAVE_CCMIOCORE_H */

/* Define to 1 if you have the <ccmioutility.h> header file. */
/* #undef HAVE_CCMIOUTILITY_H */

/* Define to 1 if you have the <ccmio.h> header file. */
/* #undef HAVE_CCMIO_H */

/* "Define if configured with CGM support." */
/* #undef HAVE_CGM */

/* MOAB uses CGM configured with Facet Engine only */
/* #undef HAVE_CGM_FACET */

/* "Define if configured with CGM and Ray fire support." */
/* #undef HAVE_CGM_FIRE_RAY */

/* MOAB uses CGM configured with OpenCascade */
/* #undef HAVE_CGM_OCC */

/* "Define if configured with CGNS support." */
/* #undef HAVE_CGNS */

/* Define if clock is available. */
#ifndef MOAB_HAVE_CLOCK
#define MOAB_HAVE_CLOCK 1
#endif

/* "Define if configured with Damsel I/O format support." */
/* #undef HAVE_DAMSEL */

/* Define to 1 if you have the <damsel.h> header file. */
/* #undef HAVE_DAMSEL_H */

/* Define to 1 if you have the <dlfcn.h> header file. */
#ifndef MOAB_HAVE_DLFCN_H
#define MOAB_HAVE_DLFCN_H 1
#endif

/* Flag indicating whether the library will be compiled with Eigen support */
/* #undef HAVE_EIGEN */

/* Define if configured with FBiGeom interfaces. */
/* #undef HAVE_FBIGEOM */

/* Define to 1 if you have the <fcntl.h> header file. */
#ifndef MOAB_HAVE_FCNTL_H
#define MOAB_HAVE_FCNTL_H 1
#endif

/* define if compiler has finite */
/* #undef MOAB_HAVE_FINITE */

/* Configure with tool: GSETS */
#ifndef MOAB_HAVE_GSETS
#define MOAB_HAVE_GSETS 1
#endif

/* Configure with tool: H5MTOOLS */
/* #undef HAVE_H5MTOOLS */

/* Define if configured with HDF5 support. */
/* #undef HAVE_HDF5 */

/* Define to 1 if you have the <hdf5.h> header file. */
/* #undef HAVE_HDF5_H */

/* Define if configured with Parallel HDF5 support. */
/* #undef HAVE_HDF5_PARALLEL */

/* Configure with tool: HEXMODOPS */
#ifndef MOAB_HAVE_HEXMODOPS
#define MOAB_HAVE_HEXMODOPS 1
#endif

/* "Define if configured with HYPRE solver support." */
/* #undef HAVE_HYPRE */

/* Defined if configured with IEEE Floating point support */
/* #undef HAVE_IEEEFP */

/* MOAB qualified HAVE_IEEEFP_H */
/* #undef HAVE_IEEEFP_H */

/* Define if configured with iMesh interfaces. */
#ifndef MOAB_HAVE_IMESH
#define MOAB_HAVE_IMESH 1
#endif

/* MOAB qualified HAVE_INTTYPES_H */
#ifndef MOAB_HAVE_INTTYPES_H
#define MOAB_HAVE_INTTYPES_H 1
#endif

/* Define if configured with iRel interfaces. */
/* #undef HAVE_IREL */

/* define if compiler has isfinite */
/* #undef HAVE_ISFINITE */

/* Define if you have LAPACK library. */
#ifndef MOAB_HAVE_LAPACK
#define MOAB_HAVE_LAPACK 1
#endif

/* Define to 1 if you have the `lmpe' library (-llmpe). */
/* #undef HAVE_LIBLMPE */

/* Define to 1 if you have the `mpe' library (-lmpe). */
/* #undef HAVE_LIBMPE */

/* Configure with tool: MBCONVERT */
#ifndef MOAB_HAVE_MBCONVERT
#define MOAB_HAVE_MBCONVERT 1
#endif

/* Configure with tool: MBCOUPLER */
/* #undef HAVE_MBCOUPLER */

/* Configure with tool: MBCSLAM */
/* #undef HAVE_MBCSLAM */

/* Configure with tool: MBDEPTH */
#ifndef MOAB_HAVE_MBDEPTH
#define MOAB_HAVE_MBDEPTH 1
#endif

/* Configure with tool: MBHONODES */
#ifndef MOAB_HAVE_MBHONODES
#define MOAB_HAVE_MBHONODES 1
#endif

/* Configure with tool: MBMEM */
#ifndef MOAB_HAVE_MBMEM
#define MOAB_HAVE_MBMEM 1
#endif

/* Configure with tool: MBMERGE */
#ifndef MOAB_HAVE_MBMERGE
#define MOAB_HAVE_MBMERGE 1
#endif

/* Configure with tool: MBPART */
/* #undef HAVE_MBPART */

/* Configure with tool: MBQUALITY */
#ifndef MOAB_HAVE_MBQUALITY
#define MOAB_HAVE_MBQUALITY 1
#endif

/* Configure with tool: MBSIZE */
#ifndef MOAB_HAVE_MBSIZE
#define MOAB_HAVE_MBSIZE 1
#endif

/* Configure with tool: MBSKIN */
#ifndef MOAB_HAVE_MBSKIN
#define MOAB_HAVE_MBSKIN 1
#endif

/* Configure with tool: MBSURFPLOT */
#ifndef MOAB_HAVE_MBSURFPLOT
#define MOAB_HAVE_MBSURFPLOT 1
#endif

/* Configure with tool: MBTAGPROP */
#ifndef MOAB_HAVE_MBTAGPROP
#define MOAB_HAVE_MBTAGPROP 1
#endif

/* Configure with tool: MBTEMPEST */
/* #undef HAVE_MBTEMPEST */

/* Configure with tool: MBUMR */
#ifndef MOAB_HAVE_MBUMR
#define MOAB_HAVE_MBUMR 1
#endif

/* Configure with tool: MCNPMIT */
/* #undef HAVE_MCNPMIT */

/* Define to 1 if you have the <memory.h> header file. */
#ifndef MOAB_HAVE_MEMORY_H
#define MOAB_HAVE_MEMORY_H 1
#endif

/* "MOAB configured with Mesquite support" */
/* #undef HAVE_MESQUITE */

/* Flag indicating whether the library will be compiled with Metis support */
/* #undef HAVE_METIS */

/* Define if configured with support for parallel computations. */
#ifndef MOAB_HAVE_MPI
#define MOAB_HAVE_MPI 1
#endif

/* "Define if configured with NetCDF support." */
/* #undef HAVE_NETCDF */

/* Define to 1 if you have the <netcdf.h> header file. */
/* #undef HAVE_NETCDF_H */

/* Flag indicating whether the library will be compiled with ParMetis support
   */
/* #undef HAVE_PARMETIS */

/* "Define if configured with PNetCDF support." */
/* #undef HAVE_PNETCDF */

/* Define to 1 if you have the <pnetcdf.h> header file. */
/* #undef HAVE_PNETCDF_H */

/* System provides ptrdiff_t typedef */
#ifndef MOAB_HAVE_PTRDIFF_T
#define MOAB_HAVE_PTRDIFF_T 1
#endif

/* If available, contains the Python version number currently in use. */
/* #undef HAVE_PYTHON */

/* Configure with tool: REFINER */
/* #undef HAVE_REFINER */

/* "Define if configured with Parallel Scotch library partitioning support."
   */
/* #undef HAVE_SCOTCH */

/* System provides size_t typedef */
#ifndef MOAB_HAVE_SIZE_T
#define MOAB_HAVE_SIZE_T 1
#endif

/* Configure with tool: SPHEREDECOMP */
#ifndef MOAB_HAVE_SPHEREDECOMP
#define MOAB_HAVE_SPHEREDECOMP 1
#endif

/* MOAB qualified HAVE_STDDEF_H */
#ifndef MOAB_HAVE_STDDEF_H
#define MOAB_HAVE_STDDEF_H 1
#endif

/* MOAB qualified HAVE_STDINT_H */
#ifndef MOAB_HAVE_STDINT_H
#define MOAB_HAVE_STDINT_H 1
#endif

/* define if compiler has std::isfinite */
#if defined (ESMF_OS_Darwin) || defined (ESMF_OS_Cygwin) || defined (ESMF_OS_MinGW)
#ifndef MOAB_HAVE_STDISFINITE
#define MOAB_HAVE_STDISFINITE 1
#endif
#endif

#if defined (ESMF_PGIVERSION_MAJOR)
#if (ESMF_PGIVERSION_MAJOR >= 16)
#ifndef MOAB_HAVE_STDISFINITE
#define MOAB_HAVE_STDISFINITE 1
#endif
#else
#ifndef MOAB_HAVE_ISFINITE
#define MOAB_HAVE_ISFINITE 1
#endif
#endif
#endif
/*#undef MOAB_HAVE_STDISFINITE */

/* MOAB qualified HAVE_STDLIB_H */
#ifndef MOAB_HAVE_STDLIB_H
#define MOAB_HAVE_STDLIB_H 1
#endif

/* Define to 1 if you have the <strings.h> header file. */
#ifndef MOAB_HAVE_STRINGS_H
#define MOAB_HAVE_STRINGS_H 1
#endif

/* Define to 1 if you have the <string.h> header file. */
#ifndef MOAB_HAVE_STRING_H
#define MOAB_HAVE_STRING_H 1
#endif

/* Define to 1 if you have the <sys/file.h> header file. */
#ifndef MOAB_HAVE_SYS_FILE_H
#define MOAB_HAVE_SYS_FILE_H 1
#endif

/* MOAB qualified HAVE_SYS_IOCTL_H */
#ifndef MOAB_HAVE_SYS_IOCTL_H
#define MOAB_HAVE_SYS_IOCTL_H 1
#endif

/* Define to 1 if you have the <sys/stat.h> header file. */
#ifndef MOAB_HAVE_SYS_STAT_H
#define MOAB_HAVE_SYS_STAT_H 1
#endif

/* Define to 1 if you have the <sys/time.h> header file. */
#ifndef MOAB_HAVE_SYS_TIME_H
#define MOAB_HAVE_SYS_TIME_H 1
#endif

/* MOAB qualified HAVE_SYS_TYPES_H */
#ifndef MOAB_HAVE_SYS_TYPES_H
#define MOAB_HAVE_SYS_TYPES_H 1
#endif

/* Enable use of TempestRemap library API for computing intersection and
   remapping for Climate problems */
/* #undef HAVE_TEMPESTREMAP */

/* MOAB qualified HAVE_TERMIOS_H */
#ifndef MOAB_HAVE_TERMIOS_H
#define MOAB_HAVE_TERMIOS_H 1
#endif

/* Define if times is available. */
#ifndef MOAB_HAVE_TIMES
#define MOAB_HAVE_TIMES 1
#endif

/* Define to 1 if you have the <unistd.h> header file. */
#ifndef MOAB_HAVE_UNISTD_H
#define MOAB_HAVE_UNISTD_H 1
#endif

/* Specify if unordered map is available */
#ifndef MOAB_HAVE_UNORDERED_MAP
#define MOAB_HAVE_UNORDERED_MAP unordered_map
#endif

/* Specify if unordered set is available */
#ifndef MOAB_HAVE_UNORDERED_SET
#define MOAB_HAVE_UNORDERED_SET unordered_set
#endif

/* Defined if configured with Valgrind support */
/* #undef HAVE_VALGRIND */

/* Define if vsnprintf is available. */
#ifndef MOAB_HAVE_VSNPRINTF
#define MOAB_HAVE_VSNPRINTF 1
#endif

/* Define if vsprintf is available. */
#ifndef MOAB_HAVE_VSPRINTF
#define MOAB_HAVE_VSPRINTF 1
#endif

/* "Define if configured with VTK I/O library support." */
/* #undef HAVE_VTK */

/* Configure with tool: VTKMOABREADER */
/* #undef HAVE_VTKMOABREADER */

/* "Define if configured with Zoltan library partitioning support." */
/* #undef HAVE_ZOLTAN */

/* Define to the sub-directory where libtool stores uninstalled libraries. */
#ifndef MOAB_LT_OBJDIR
#define MOAB_LT_OBJDIR ".libs/"
#endif

/* MPICH_IGNORE_CXX_SEEK is not sufficient to avoid conflicts */
/* #undef MPI_CXX_CONFLICT */

/* Do not use template vector insertions */
/* #undef NO_VECTOR_TEMPLATE_INSERT */

/* Use old-style C++ std::count calls */
/* #undef OLD_STD_COUNT */

/* Name of package */
#ifndef MOAB_PACKAGE
#define MOAB_PACKAGE "moab"
#endif

/* Define to the address where bug reports for this package should be sent. */
#ifndef MOAB_PACKAGE_BUGREPORT
#define MOAB_PACKAGE_BUGREPORT "moab-dev@mcs.anl.gov"
#endif

/* Define to the full name of this package. */
#ifndef MOAB_PACKAGE_NAME
#define MOAB_PACKAGE_NAME "MOAB"
#endif

/* Define to the full name and version of this package. */
#ifndef MOAB_PACKAGE_STRING
#define MOAB_PACKAGE_STRING "MOAB 5.1.0"
#endif

/* Define to the one symbol short name of this package. */
#ifndef MOAB_PACKAGE_TARNAME
#define MOAB_PACKAGE_TARNAME "moab"
#endif

/* Define to the home page for this package. */
#ifndef MOAB_PACKAGE_URL
#define MOAB_PACKAGE_URL "http://sigma.mcs.anl.gov"
#endif

/* Define to the version of this package. */
#ifndef MOAB_PACKAGE_VERSION
#define MOAB_PACKAGE_VERSION "5.1.0"
#endif

/* "Value of C SEEK_CUR" */
/* #undef SEEK_CUR */

/* "Value of C SEEK_END" */
/* #undef SEEK_END */

/* "Value of C SEEK_SET" */
/* #undef SEEK_SET */

/* The size of `long', as computed by sizeof. */
/* #undef SIZEOF_LONG */

/* The size of `unsigned long', as computed by sizeof. */
/* #undef SIZEOF_UNSIGNED_LONG */

/* The size of `void *', as computed by sizeof. */
#ifndef MOAB_SIZEOF_VOID_P
#define MOAB_SIZEOF_VOID_P 8
#endif

/* Define to 1 if you have the ANSI C header files. */
#ifndef MOAB_STDC_HEADERS
#define MOAB_STDC_HEADERS 1
#endif

/* Use template function specializations */
#ifndef MOAB_TEMPLATE_FUNC_SPECIALIZATION
#define MOAB_TEMPLATE_FUNC_SPECIALIZATION 1
#endif

/* Use template class specializations */
#ifndef MOAB_TEMPLATE_SPECIALIZATION
#define MOAB_TEMPLATE_SPECIALIZATION 1
#endif

/* Unordered map namespace */
#ifndef MOAB_UNORDERED_MAP_NS
#define MOAB_UNORDERED_MAP_NS std
#endif

/* MOAB Version */
#ifndef MOAB_VERSION
#define MOAB_VERSION "5.1.0"
#endif

/* MOAB Major Version */
#ifndef MOAB_VERSION_MAJOR
#define MOAB_VERSION_MAJOR 5
#endif

/* MOAB Minor Version */
#ifndef MOAB_VERSION_MINOR
#define MOAB_VERSION_MINOR 1
#endif

/* MOAB Patch Level */
#ifndef MOAB_VERSION_PATCH
#define MOAB_VERSION_PATCH 0
#endif

/* MOAB Version String */
#ifndef MOAB_VERSION_STRING
#define MOAB_VERSION_STRING "MOAB 5.1.0"
#endif

/* Define WORDS_BIGENDIAN to 1 if your processor stores words with the most
   significant byte first (like Motorola and SPARC, unlike Intel). */
#if defined AC_APPLE_UNIVERSAL_BUILD
# if defined __BIG_ENDIAN__
#  define WORDS_BIGENDIAN 1
# endif
#else
# ifndef WORDS_BIGENDIAN
/* #  undef WORDS_BIGENDIAN */
# endif
#endif

/* Define to 1 if the X Window System is missing or not being used. */
/* #undef X_DISPLAY_MISSING */
 
/* once: _SRC_MOAB_MOABCONFIG_H */
#endif
