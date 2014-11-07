#if (defined  __STDC__ || defined __cplusplus)

// ---------- C and C++ block ----------

#include "ESMC.h"
extern "C" {
  void FTN_X(setservices_atmf)(ESMC_GridComp gcomp, int *rc);
}

#else

!! ---------- Fortran block ----------

interface
  subroutine setservices_atmf(gcomp, rc)
    use ESMF
    type(ESMF_GridComp) :: gcomp
    integer, intent(out) :: rc
  end subroutine
end interface

#endif
