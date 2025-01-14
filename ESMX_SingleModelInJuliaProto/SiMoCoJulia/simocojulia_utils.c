//==============================================================================
// Earth System Modeling Framework
// Copyright (c) 2002-2025, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//==============================================================================

// Utilities that can be called from Julia as well as C

#include "ESMC.h"

void write_logmsg(const char *msg) {
   // For now, we just ignore the return code to avoid trying to handle that from Julia
   int rc = ESMC_LogWrite(msg, ESMC_LOGMSG_INFO);
}