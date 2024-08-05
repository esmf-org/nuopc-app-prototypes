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

#include <julia.h>

// ESMF header -- provides access to the entire public ESMF C API
#include "NUOPC.h"
#include "ESMC.h"

//-------------------------------------------------------------------------
//-------------------------------------------------------------------------

void Advertise(ESMC_GridComp model, int *rc){
  // initialize return code to success
  *rc = ESMF_SUCCESS;

  *rc = ESMC_LogWrite("Message from inside C component Advertise() method",
    ESMC_LOGMSG_INFO);

  // jl_init should only be called once per process. If we are guaranteed to only have a
  // single Julia-based component, then it can be called from the cap as is done here. But
  // if there's a chance that we may have multiple Julia components, then we should do
  // something different to ensure it only gets called once. This could mean moving this
  // call into ESMF somehow, or at least having ESMF support some way of checking whether
  // jl_init has already been called... the latter could be done in a way that avoids
  // requiring ESMF to have any julia calls itself, e.g.,:
  //
  // if (!ESMC_Is_Julia_Initialized()) {
  //    jl_init();
  //    ESMC_Set_Jula_Initialized()
  // }
  //
  // where ESMC_Set_Julia_Initialized and ESMC_Is_Julia_Initialized simply set and get a
  // boolean flag.
  jl_init();

  if (*rc!=ESMF_SUCCESS) return;  // bail out
}

void Advance(ESMC_GridComp model, int *rc){
  // initialize return code to success
  *rc = ESMF_SUCCESS;

  *rc = ESMC_LogWrite("Message from inside C component Advance() method",
    ESMC_LOGMSG_INFO);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
}

//-------------------------------------------------------------------------
//-------------------------------------------------------------------------

void SetServices(ESMC_GridComp model, int *rc){
  // initialize return code to success
  *rc = ESMF_SUCCESS;

  // derive from NUOPC_Model
  *rc = NUOPC_CompDerive(model, NUOPC_ModelSetServices);
  if (*rc!=ESMF_SUCCESS) return;  // bail out

  // specialize model
  *rc = NUOPC_CompSpecialize(model, label_Advertise, Advertise);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
  *rc = NUOPC_CompSpecialize(model, label_Advance, Advance);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
}

void SetVM(ESMC_GridComp model, int *rc){
  // initialize return code to success
  *rc = ESMF_SUCCESS;
  // call NUOPC_Model generic SetVM
  NUOPC_ModelSetVM(model, rc);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
}

//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
