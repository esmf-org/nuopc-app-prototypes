//==============================================================================
// Earth System Modeling Framework
// Copyright (c) 2002-2025, University Corporation for Atmospheric Research,
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

jl_function_t *model_init;
jl_function_t *model_run;

void initialize_julia(int *rc) {
  // initialize return code to success
  *rc = ESMF_SUCCESS;

  /* Initialize Julia runtime

  jl_init should only be called once per process. If we are guaranteed to only have a
  single Julia-based component, then it can be called from the cap as is done here. But
  if there's a chance that we may have multiple Julia components, then we should do
  something different to ensure it only gets called once. This could mean moving this
  call into ESMF somehow, or at least having ESMF support some way of checking whether
  jl_init has already been called... the latter could be done in a way that avoids
  requiring ESMF to have any julia calls itself, e.g.,:

    if (!ESMC_Is_Julia_Initialized()) {
     jl_init();
     ESMC_Set_Jula_Initialized()
    }

  where ESMC_Set_Julia_Initialized and ESMC_Is_Julia_Initialized simply set and get a
  boolean flag.
  */
  jl_init();

  /* Load Julia file with function definitions

  Note that PATH_TO_SOURCE is expected to be provided via a compile-time definition.
  */
  const char *julia_file = "simocojulia_model.jl";
  size_t path_len = strlen(PATH_TO_SOURCE);
  size_t filename_len = strlen(julia_file);
  size_t full_path_len = path_len + filename_len + 2;  // +1 for '/' and +1 for null terminator
  char *full_path = malloc(full_path_len * sizeof(char));
  strcpy(full_path, PATH_TO_SOURCE);
  strcat(full_path, "/");
  strcat(full_path, julia_file);

  const char *include_start = "include(\"";
  const char *include_end = "\")";
  char *include_statement = malloc((strlen(include_start) + strlen(full_path) + strlen(include_end) + 1) * sizeof(char));
  strcpy(include_statement, include_start);
  strcat(include_statement, full_path);
  strcat(include_statement, include_end);
  *rc = ESMC_LogWrite(include_statement, ESMC_LOGMSG_INFO);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
  jl_eval_string(include_statement);

  /* Get the julia functions that we'll call

  Store these in global variables so they can be accessed from other functions
  */
  model_init = jl_get_function(jl_main_module, "init");
  model_run = jl_get_function(jl_main_module, "run");
}

void Advertise(ESMC_GridComp model, int *rc){
  // initialize return code to success
  *rc = ESMF_SUCCESS;

  *rc = ESMC_LogWrite("Message from inside C component Advertise() method",
    ESMC_LOGMSG_INFO);
  if (*rc!=ESMF_SUCCESS) return;  // bail out

  initialize_julia(rc);
  if (*rc!=ESMF_SUCCESS) return;  // bail out

  jl_value_t *path_to_c_library = jl_cstr_to_string(PATH_TO_LIBRARY);
  jl_call1(model_init, path_to_c_library);
}

void Advance(ESMC_GridComp model, int *rc){
  // initialize return code to success
  *rc = ESMF_SUCCESS;

  *rc = ESMC_LogWrite("Message from inside C component Advance() method",
    ESMC_LOGMSG_INFO);
  if (*rc!=ESMF_SUCCESS) return;  // bail out

  jl_call0(model_run);
}

void Finalize(ESMC_GridComp model, int *rc){
  // initialize return code to success
  *rc = ESMF_SUCCESS;

  *rc = ESMC_LogWrite("Message from inside C component Finalize() method",
    ESMC_LOGMSG_INFO);
  if (*rc!=ESMF_SUCCESS) return;  // bail out

  // Perform clean-up tasks in Julia
  jl_atexit_hook(0);
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
  *rc = NUOPC_CompSpecialize(model, label_Finalize, Finalize);
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
