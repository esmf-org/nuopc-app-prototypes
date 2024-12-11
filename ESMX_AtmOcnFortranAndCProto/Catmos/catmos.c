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

void Advertise(ESMC_GridComp model, int *rc){
  // initialize return code to success
  *rc = ESMF_SUCCESS;

  *rc = ESMC_LogWrite("Message from inside C component Advertise() method",
    ESMC_LOGMSG_INFO);
  if (*rc!=ESMF_SUCCESS) return;  // bail out

  // query for importState and exportState
  ESMC_State importState = NUOPC_ModelGetImportState(model, rc);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
  ESMC_State exportState = NUOPC_ModelGetExportState(model, rc);
  if (*rc!=ESMF_SUCCESS) return;  // bail out

  // exportable field: air_pressure_at_sea_level
  *rc = NUOPC_Advertise(exportState, "air_pressure_at_sea_level", "pmsl");
  if (*rc!=ESMF_SUCCESS) return;  // bail out

  // exportable field: surface_net_downward_shortwave_flux
  *rc = NUOPC_Advertise(exportState, "surface_net_downward_shortwave_flux",
    "rsns");
  if (*rc!=ESMF_SUCCESS) return;  // bail out

  // exportable field: precipitation_flux
  *rc = NUOPC_Advertise(exportState, "precipitation_flux", "precip");
  if (*rc!=ESMF_SUCCESS) return;  // bail out

  // importable field: sea_surface_temperature
  *rc = NUOPC_Advertise(importState, "sea_surface_temperature", "sst");
  if (*rc!=ESMF_SUCCESS) return;  // bail out
}

//-------------------------------------------------------------------------

void Realize(ESMC_GridComp model, int *rc){
  // initialize return code to success
  *rc = ESMF_SUCCESS;

  *rc = ESMC_LogWrite("Message from inside C component Realize() method",
    ESMC_LOGMSG_INFO);
  if (*rc!=ESMF_SUCCESS) return;  // bail out

  // query for importState and exportState
  ESMC_State importState = NUOPC_ModelGetImportState(model, rc);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
  ESMC_State exportState = NUOPC_ModelGetExportState(model, rc);
  if (*rc!=ESMF_SUCCESS) return;  // bail out

  // create Grid
  int maxIndex[2];
  maxIndex[0]=10;   // number of points in x globally
  maxIndex[1]=100;  // number of points in y globally
  ESMC_InterArrayInt i_maxIndex;
  *rc = ESMC_InterArrayIntSet(&i_maxIndex, maxIndex, 2);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
  enum ESMC_CoordSys_Flag coordsys = ESMC_COORDSYS_CART;
  enum ESMC_TypeKind_Flag typekind = ESMC_TYPEKIND_R8;
  enum ESMC_IndexFlag indexflag = ESMC_INDEX_GLOBAL;
  ESMC_Grid grid = ESMC_GridCreateNoPeriDim(&i_maxIndex, &coordsys, &typekind,
    &indexflag, rc);
  if (*rc!=ESMF_SUCCESS) return;  // bail out

  // add coordinates to Grid
  *rc = ESMC_GridAddCoord(grid, ESMC_STAGGERLOC_CENTER);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
  int exLBound[2], exUBound[2];
  double *gridXCoord = (double *)ESMC_GridGetCoord(grid, 1,
    ESMC_STAGGERLOC_CENTER, NULL, exLBound, exUBound, rc);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
  double *gridYCoord = (double *)ESMC_GridGetCoord(grid, 2,
    ESMC_STAGGERLOC_CENTER, NULL, exLBound, exUBound, rc);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
  // coordinate dims
  double max_x, min_x, cellwidth_x;
  double max_y, min_y, cellwidth_y;
  min_x = 10.;  min_y = 20.;
  max_x = 100.; max_y = 200.;
  cellwidth_x = (max_x-min_x)/(double)maxIndex[0];
  cellwidth_y = (max_y-min_y)/(double)maxIndex[1];
  // generate coordinates
  int p = 0;
  for (int iy=exLBound[1]; iy<=exUBound[1]; ++iy) {
    for (int ix=exLBound[0]; ix<=exUBound[0]; ++ix) {
      gridXCoord[p]=((double)(ix-1)*cellwidth_x) + min_x
        + (double)(cellwidth_x/2.0);
      gridYCoord[p]=((double)(iy-1)*cellwidth_y) + min_y
        + (double)(cellwidth_y/2.0);
      ++p;
    }
  }

  ESMC_Field field; // Field variable, used to realize fields in States

  // exportable field: air_pressure_at_sea_level
  field = ESMC_FieldCreateGridTypeKind(grid, ESMC_TYPEKIND_R8,
    ESMC_STAGGERLOC_CENTER, NULL, NULL, NULL, "pmsl", rc);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
  *rc = NUOPC_Realize(exportState, field);
  if (*rc!=ESMF_SUCCESS) return;  // bail out

  // exportable field: surface_net_downward_shortwave_flux
  field = ESMC_FieldCreateGridTypeKind(grid, ESMC_TYPEKIND_R8,
    ESMC_STAGGERLOC_CENTER, NULL, NULL, NULL, "rsns", rc);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
  *rc = NUOPC_Realize(exportState, field);
  if (*rc!=ESMF_SUCCESS) return;  // bail out

  // exportable field: precipitation_flux
  field = ESMC_FieldCreateGridTypeKind(grid, ESMC_TYPEKIND_R8,
    ESMC_STAGGERLOC_CENTER, NULL, NULL, NULL, "precip", rc);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
  *rc = NUOPC_Realize(exportState, field);
  if (*rc!=ESMF_SUCCESS) return;  // bail out

  // importable field: sea_surface_temperature
  field = ESMC_FieldCreateGridTypeKind(grid, ESMC_TYPEKIND_R8,
    ESMC_STAGGERLOC_CENTER, NULL, NULL, NULL, "sst", rc);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
  *rc = NUOPC_Realize(importState, field);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
}

//-------------------------------------------------------------------------

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
  *rc = NUOPC_CompSpecialize(model, label_RealizeProvided, Realize);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
  *rc = NUOPC_CompSpecialize(model, label_Advance, Advance);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
}

//-------------------------------------------------------------------------

void SetVM(ESMC_GridComp model, int *rc){
  // initialize return code to success
  *rc = ESMF_SUCCESS;
  // call NUOPC_Model generic SetVM
  NUOPC_ModelSetVM(model, rc);
  if (*rc!=ESMF_SUCCESS) return;  // bail out
}

//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
