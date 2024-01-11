//==============================================================================
// Earth System Modeling Framework
// Copyright (c) 2002-2024, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//==============================================================================

// standard C headers
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

// ESMF header -- provides access to the entire public ESMF C API
#include "NUOPC.h"
#include "ESMC.h"

//-------------------------------------------------------------------------
//-------------------------------------------------------------------------

void Advance(ESMC_GridComp model, int *rc){
  // initialize return code to success
  *rc = ESMF_SUCCESS;
}

void Run(ESMC_GridComp model, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock *clock, int *rc){
  // initialize return code to success
  *rc = ESMF_SUCCESS;

  *rc = ESMC_LogWrite("Message from inside C component RUN", ESMC_LOGMSG_INFO);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
}

//-------------------------------------------------------------------------
//-------------------------------------------------------------------------

void SetServices(ESMC_GridComp model, int *rc){
  // initialize return code to success
  *rc = ESMF_SUCCESS;

  *rc = NUOPC_CompDerive(model, NUOPC_ModelSetServices);
  if (*rc!=ESMF_SUCCESS) return;  // bail out

  // here call specialize once C API is available

  // for now must override RUN method, or else there is an issue because of
  // missing advance specialization
  *rc = ESMC_GridCompSetEntryPoint(model, ESMF_METHOD_RUN, Run, 1);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
}

void SetVM(ESMC_GridComp model, int *rc){
  // initialize return code to success
  *rc = ESMF_SUCCESS;
}

//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
