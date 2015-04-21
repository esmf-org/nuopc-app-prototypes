// standard C headers
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

// ESMF header -- provides access to the entire public ESMF C API
#include "ESMC.h"
#include "atmF.h"

//-------------------------------------------------------------------------
//-------------------------------------------------------------------------

void Initialize(ESMC_GridComp model, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock *clock, int *rc){

  // initialize return code
  *rc = ESMF_SUCCESS;
}


void Run(ESMC_GridComp model, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock *clock, int *rc){

  // initialize return code
  *rc = ESMF_SUCCESS;
}


void Finalize(ESMC_GridComp model, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock *clock, int *rc){

  // initialize return code
  *rc = ESMF_SUCCESS;
}

//-------------------------------------------------------------------------
//-------------------------------------------------------------------------

extern "C" {
  // The SetServices entry point must ensure to have external C linkage,
  // that way it can be called from C or Fortran.
  
  void FTN_X(setservices_atmf)(ESMC_GridComp model, int *rc){
    
    // initialize return code
    *rc = ESMF_SUCCESS;
    
    // This is where the NUOPC_Model SetServices needs to be called once it is
    // available for C code.
    
    *rc = ESMC_GridCompSetEntryPoint(model, ESMF_METHOD_INITIALIZE,Initialize,1);
    if (*rc!=ESMF_SUCCESS) return;  // bail out
    *rc = ESMC_GridCompSetEntryPoint(model, ESMF_METHOD_RUN, Run, 1);
    if (*rc!=ESMF_SUCCESS) return;  // bail out
    *rc = ESMC_GridCompSetEntryPoint(model, ESMF_METHOD_FINALIZE, Finalize, 1);
    if (*rc!=ESMF_SUCCESS) return;  // bail out
  }
} //extern "C"

//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
