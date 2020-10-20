!
! purpose:
!   inform the Fortran compiler that type, kind, and rank checking should not be performed for
!   some of the formal arguments
!
! usage:
!   #define VarName name1 [, name2] [, name3] ...
!   #include <blind_argment.h>
!
#if defined(WITH_GNU) || defined(__GFORTRAN__)
      type(*), dimension(*) :: VarName
!GCC$ ATTRIBUTES NO_ARG_CHECK :: VarName
#elif defined(WITH_INTEL) || defined(__INTEL_COMPILER)
      type(*), dimension(*) :: VarName
!DIR$ ATTRIBUTES NO_ARG_CHECK :: VarName
#elif defined(WITH_PGI) || defined(WITH_NVIDIA) || defined(WITH_CRAY) || defined(_CRAYFTN) || defined(__PGI)
      logical, dimension(*) :: VarName
!DIR$ ignore_tkr VarName
#elif defined(WITH_IBM)
      logical, dimension(*) :: VarName
!ibm* ignore_tkr VarName
#else
      logical, dimension(*) :: VarName
!DEC$ ATTRIBUTES NO_ARG_CHECK :: VarName
!$PRAGMA IGNORE_TKR VarName
!DIR$ IGNORE_TKR VarName
!IBM* IGNORE_TKR VarName
#endif
#undef VarName
