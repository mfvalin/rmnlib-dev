!
! purpose:
!   inform the Fortran compiler that type, kind, and rank checking should not be performed for
!   one or more formal arguments
!
! usage:
!   #define VarName name1 [, name2] [, name3] ...
!   #include <blind_argment.h>
!
! type(*) is known to be supported by gfortran and Intel Fortran compiler
!
#if defined(__GFORTRAN__) || defined(__INTEL_COMPILER)
      type(*), dimension(*) :: VarName
#else
      logical, dimension(*) :: VarName  ! Cray compiler, PGI/Nvidia, flang, xlf, SunStudio
#endif
!
! ignore type, kind, and rank through the use of compiler directives (pragmas)
!
#if defined(WITH_GNU) || defined(__GFORTRAN__)
!GCC$ ATTRIBUTES NO_ARG_CHECK :: VarName
#elif defined(WITH_INTEL) || defined(__INTEL_COMPILER)
!DIR$ ATTRIBUTES NO_ARG_CHECK :: VarName
#elif defined(WITH_PGI) || defined(WITH_NVIDIA) || defined(WITH_CRAY) || defined(_CRAYFTN) || defined(__PGI)
!DIR$ ignore_tkr VarName
#elif defined(WITH_IBM)
!ibm* ignore_tkr VarName
#else
!
! unrecognized compiler, use all known directives hoping that one gets recognized
!
!DEC$ ATTRIBUTES NO_ARG_CHECK :: VarName
!$PRAGMA IGNORE_TKR VarName
!DIR$ IGNORE_TKR VarName
!IBM* IGNORE_TKR VarName
#endif
#undef VarName
