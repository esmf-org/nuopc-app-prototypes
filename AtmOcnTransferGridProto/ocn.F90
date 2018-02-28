!==============================================================================
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

! Make sure to define test macros consistently across all source files:
#define TEST_GRID_EDGE_WIDTHS
#define TEST_GRID_EDGE_WIDTHS_KEEP_FACTORIZED_COORDS___off
#define TEST_MULTI_TILE_GRID

module OCN

  !-----------------------------------------------------------------------------
  ! OCN Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    model_routine_SS      => SetServices, &
    model_label_SetClock  => label_SetClock, &
    model_label_Advance   => label_Advance
  
  implicit none
  
  private
  
  public SetServices
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine SetServices(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    ! the NUOPC model component will register the generic methods
    call NUOPC_CompDerive(model, model_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p1"/), userRoutine=InitializeAdvertise, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p2"/), userRoutine=InitializeRealize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! attach specializing method(s)
    call NUOPC_CompSpecialize(model, specLabel=model_label_SetClock, &
      specRoutine=SetClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(model, specLabel=model_label_Advance, &
      specRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeAdvertise(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    ! importable field: air_pressure_at_sea_level
    ! -> use default, i.e. marked as "will provide"
    call NUOPC_Advertise(importState, &
      StandardName="air_pressure_at_sea_level", name="pmsl", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! importable field: surface_net_downward_shortwave_flux
    ! -> use default, i.e. marked as "will provide"
    call NUOPC_Advertise(importState, &
      StandardName="surface_net_downward_shortwave_flux", name="rsns", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: precipitation_flux
    ! -> use default, i.e. marked as "will provide"
    call NUOPC_Advertise(importState, &
      StandardName="precipitation_flux", name="precip", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_LogWrite("Done advertising fields in OCN importState", &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: sea_surface_temperature
    ! -> use default, i.e. marked as "will provide"
    call NUOPC_Advertise(exportState, &
      StandardName="sea_surface_temperature", name="sst", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: sea_surface_salinity
    ! -> use default, i.e. marked as "will provide"
    call NUOPC_Advertise(exportState, &
      StandardName="sea_surface_salinity", name="sss", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: sea_surface_height_above_sea_level
    ! -> use default, i.e. marked as "will provide"
    call NUOPC_Advertise(exportState, &
      StandardName="sea_surface_height_above_sea_level", name="ssh", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_LogWrite("Done advertising fields in OCN exportState", &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeRealize(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables    
    type(ESMF_Grid)       :: gridIn, gridOut, gridArb, gridAux
    type(ESMF_DistGrid)   :: distgrid
    integer               :: deBlockList(2,2,0:1) ! 2 DEs
    integer               :: arbIndexCount, extra
    integer, allocatable  :: arbIndexList(:,:)
    integer               :: iCount, jCount, petCount, localPet, ind, i
    type(ESMF_Field)      :: fieldArb, fieldAux, sstField
    real(ESMF_KIND_R8),   pointer :: fptr(:), dataPtr(:)
    real(ESMF_KIND_R8),   pointer :: coordPtr(:,:)
    type(ESMF_RouteHandle):: rh
    real(ESMF_KIND_R8),   pointer :: factorList(:)
    integer(ESMF_KIND_I4),pointer :: factorIndexList(:,:)
    type(ESMF_DistGridConnection), allocatable :: connectionList(:)
    type(ESMF_Array)      :: array
    integer               :: dimCount, rank
    integer               :: coordDimMap(2,2)
    character(160)        :: msgString

    rc = ESMF_SUCCESS
    
    !--- regDecomp Grid -------------------------------------------------------
    ! create Grid objects for import Fields
    gridIn = ESMF_GridCreate1PeriDimUfrm(maxIndex=(/100, 150/), &
      minCornerCoord=(/0._ESMF_KIND_R8, -60._ESMF_KIND_R8/), &
      maxCornerCoord=(/360._ESMF_KIND_R8, 80._ESMF_KIND_R8/), &
      staggerLocList=(/ESMF_STAGGERLOC_CENTER/), name="OCN-GridIn", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
#ifdef TEST_GRID_EDGE_WIDTHS
    ! Test Grid created from DG -> test transfer of various pieces of info
    ! Note that the DG holds the topology info of the original GridCreate()
    ! short-cut, so here simple periodic connection along dim=1.
    
    gridAux = gridIn  ! hold on to original gridIn
    
    call ESMF_GridGet(gridAux, distgrid=distgrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
#ifdef TEST_GRID_EDGE_WIDTHS_KEEP_FACTORIZED_COORDS
    coordDimMap(1,1) = 1
    coordDimMap(1,2) = 1
    coordDimMap(2,1) = 2
    coordDimMap(2,2) = 2
#endif

    gridIn = ESMF_GridCreate(distgrid, &
#ifdef TEST_GRID_EDGE_WIDTHS_KEEP_FACTORIZED_COORDS
      coordDimCount=(/1,1/), coordDimMap=coordDimMap, &
#endif
      indexflag=ESMF_INDEX_GLOBAL, &
      gridEdgeLWidth=(/1,1/),     &
      gridEdgeUWidth=(/1,1/),     &
      name="OCN-GridInFromDG", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
#ifdef TEST_GRID_EDGE_WIDTHS_KEEP_FACTORIZED_COORDS
    call ESMF_GridGetCoord(gridAux, coordDim=1, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridSetCoord(gridIn, coordDim=1, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_GridGetCoord(gridAux, coordDim=2, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridSetCoord(gridIn, coordDim=2, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#else
    ! Caution: make sure to choose staggerEdge widths that are consistent
    ! with the topology. Here because of periodicity in dim=1, there cannot
    ! be any staggerEdge L/U width along the 1st dimension!
    ! - actually let CENTER just default to its typical (/0,0/) edge widths
    call ESMF_GridAddCoord(gridIn, staggerloc=ESMF_STAGGERLOC_CENTER, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! - set some values in the coordinates so that output does look messed up
    call ESMF_GridGetCoord(gridIn, coordDim=1, farrayPtr=coordPtr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    do i=lbound(coordPtr,1), ubound(coordPtr,1)
      coordPtr(i,:) = real(i, ESMF_KIND_R8)
    enddo
    call ESMF_GridGetCoord(gridIn, coordDim=2, farrayPtr=coordPtr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    do i=lbound(coordPtr,2), ubound(coordPtr,2)
      coordPtr(:,i) = real(i, ESMF_KIND_R8)
    enddo
#endif

    ! add CORNER stagger with edge width
    call ESMF_GridAddCoord(gridIn, staggerloc=ESMF_STAGGERLOC_CORNER, &
      staggerEdgeLWidth = (/1,1/), &
      staggerEdgeUWidth = (/0,0/), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! add EDGE1 stagger with edge width
    call ESMF_GridAddCoord(gridIn, staggerloc=ESMF_STAGGERLOC_EDGE1, &
      staggerEdgeLWidth = (/0,1/), &
      staggerEdgeUWidth = (/1,1/), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! add EDGE2 stagger with edge width
    call ESMF_GridAddCoord(gridIn, staggerloc=ESMF_STAGGERLOC_EDGE2, &
      staggerEdgeLWidth = (/1,0/), &
      staggerEdgeUWidth = (/0,1/), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
  
#if 1
    ! testing the output of coord arrays
    !TODO:
    ! Coords are currently written in 2D index space even if there is coordinate
    ! factorization used, e.g. in the Ufrm() GridCreate. Therefore the coord
    ! arrays have replicated dims, and underlying allocation is only 1D. This 
    ! should be changed in the ArrayWrite() where Arrays with replicated dims
    ! should write out only the non-degenerate data, i.e. according to the 
    ! actual data allocation. 
    ! -> here that would be a 1D array for each coordiante dim.
    ! center:
    call ESMF_GridGetCoord(gridIn, coordDim=1, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayWrite(array, "array_OCN-gridIn_center_coord1.nc", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridGetCoord(gridIn, coordDim=2, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayWrite(array, "array_OCN-gridIn_center_coord2.nc", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#ifdef TEST_GRID_EDGE_WIDTHS
    ! corner:
    call ESMF_GridGetCoord(gridIn, staggerloc=ESMF_STAGGERLOC_CORNER, &
      coordDim=1, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayWrite(array, "array_OCN-gridIn_corner_coord1.nc", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridGetCoord(gridIn, staggerloc=ESMF_STAGGERLOC_CORNER, &
      coordDim=2, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayWrite(array, "array_OCN-gridIn_corner_coord2.nc", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! edge1:
    call ESMF_GridGetCoord(gridIn, staggerloc=ESMF_STAGGERLOC_EDGE1, &
      coordDim=1, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayWrite(array, "array_OCN-gridIn_edge1_coord1.nc", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridGetCoord(gridIn, staggerloc=ESMF_STAGGERLOC_EDGE1, &
      coordDim=2, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayWrite(array, "array_OCN-gridIn_edge1_coord2.nc", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! edge2:
    call ESMF_GridGetCoord(gridIn, staggerloc=ESMF_STAGGERLOC_EDGE2, &
      coordDim=1, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayWrite(array, "array_OCN-gridIn_edge2_coord1.nc", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridGetCoord(gridIn, staggerloc=ESMF_STAGGERLOC_EDGE2, &
      coordDim=2, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayWrite(array, "array_OCN-gridIn_edge2_coord2.nc", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
#endif
   
#if 1
    ! write out the Grid into VTK file for inspection
    call ESMF_GridWriteVTK(gridIn, staggerloc=ESMF_STAGGERLOC_CENTER, &
      filename="OCN-GridIn_centers", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LogWrite("Done writing OCN-GridIn_centers VTK", &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

    ! importable field: air_pressure_at_sea_level
    call NUOPC_Realize(importState, gridIn, fieldName="pmsl", &
      typekind=ESMF_TYPEKIND_R8, selection="realize_connected_remove_others", &
      dataFillScheme="sincos", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! importable field: surface_net_downward_shortwave_flux
    call NUOPC_Realize(importState, gridIn, fieldName="rsns", &
      typekind=ESMF_TYPEKIND_R8, selection="realize_connected_remove_others", &
      dataFillScheme="sincos", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !--- deBlock Grid -------------------------------------------------------
    ! create Grid objects for export Fields
    ! DE 0
    deBlockList(:,1,0) = (/1,1/)      ! min
    deBlockList(:,2,0) = (/100,20/)   ! max
    ! DE 1
    deBlockList(:,1,1) = (/61,31/)      ! min
    deBlockList(:,2,1) = (/100,120/)   ! max
    gridOut = ESMF_GridCreate1PeriDimUfrm(maxIndex=(/100, 120/), &
      minCornerCoord=(/0._ESMF_KIND_R8, -50._ESMF_KIND_R8/), &
      maxCornerCoord=(/360._ESMF_KIND_R8, 60._ESMF_KIND_R8/), &
      deBlockList=deBlockList, &
      staggerLocList=(/ESMF_STAGGERLOC_CENTER/), name="OCN-GridOut", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
#if 0
    ! write out the Grid into VTK file for inspection
    ! -> This currently only works if there are no holes in the index space
    ! -> coverage. If the deBlocks do not fully cover the index space, the
    ! -> GridToMesh conversion fails: therefore no VTK output or regrid.
    call ESMF_GridWriteVTK(gridOut, staggerloc=ESMF_STAGGERLOC_CENTER, &
      filename="OCN-GridOut_centers", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LogWrite("Done writing OCN-GridOut_centers VTK", &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

    ! exportable field: sea_surface_temperature
    call NUOPC_Realize(exportState, gridOut, fieldName="sst", &
      typekind=ESMF_TYPEKIND_R8, selection="realize_connected_remove_others", &
      dataFillScheme="sincos", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: sea_surface_salinity
    call NUOPC_Realize(exportState, gridOut, fieldName="sss", &
      typekind=ESMF_TYPEKIND_R8, selection="realize_connected_remove_others", &
      dataFillScheme="sincos", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
#ifdef TEST_MULTI_TILE_GRID    
    !--- 6-tile cubed-sphere Grid: for ssh field below ------------------------
    gridOut = ESMF_GridCreateCubedSphere(tileSize=16, name="OCN-CubedSphere", &
      staggerLocList=(/ESMF_STAGGERLOC_CENTER/), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! write cubed sphere grid out to VTK
    call ESMF_GridWriteVTK(gridOut, staggerloc=ESMF_STAGGERLOC_CENTER, &
      filename="OCN-GridCS_centers", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
    
    ! exportable field: sea_surface_height_above_sea_level
    call NUOPC_Realize(exportState, gridOut, fieldName="ssh", &
      typekind=ESMF_TYPEKIND_R8, selection="realize_connected_remove_others", &
      dataFillScheme="sincos", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out


    !--- arbDistr Grid: for precip field below -------------------------------
    ! set up the index space
    iCount = 120
    jCount = 180
    ! set up arbitrary distribution scheme
    call ESMF_GridCompGet(model, petCount=petCount, localPet=localPet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    arbIndexCount = (iCount*jCount)/petCount
    extra = (iCount*jCount) - arbIndexCount*petCount
    ! add extra elements to the last PET
    if (localPet==petCount-1) arbIndexCount = arbIndexCount + extra
    ! distributed via "card dealing" scheme across the PETs
    allocate(arbIndexList(arbIndexCount,2))
    ind = localPet
    do i=1, arbIndexCount
      arbIndexList(i,1)=ind/jCount + 1
      arbIndexList(i,2)=mod(ind,jCount)+1
      ind = ind + petCount
    enddo
    if (localPet == petCount-1) then
      ind = iCount*jCount-extra+1
      do i=arbIndexCount-extra+1,arbIndexCount
        arbIndexList(i,1)=ind/jCount+1
        arbIndexList(i,2)=mod(ind,jCount)+1
        ind = ind + 1
      enddo
    endif
    
    ! create the grid
    gridArb = ESMF_GridCreate1PeriDim(maxIndex=(/iCount, jCount/), &
      arbIndexCount=arbIndexCount, arbIndexList=arbIndexList, &
      name="OCN-GridArb", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! add the center stagger coordinates
    call ESMF_GridAddCoord(gridArb, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! fill longitudes
    call ESMF_GridGetCoord(gridArb, coordDim=1, farrayPtr=fptr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    do i=lbound(fptr,1),ubound(fptr,1)
      fptr(i) = real((arbIndexList(i,1)-1)) * 360.0 / real(iCount)
    enddo
      
    ! fill latitudes
    call ESMF_GridGetCoord(gridArb, coordDim=2, farrayPtr=fptr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    do i=lbound(fptr,1),ubound(fptr,1)
      fptr(i) = real((arbIndexList(i,2)-1)) * 160.0 / real(jCount) - 80.0
    enddo
    
    ! create a field on the grid
    fieldArb = ESMF_FieldCreate(gridArb, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! fill the field with some easy to visualize data
    call ESMF_FieldFill(fieldArb, dataFillScheme="sincos", member=2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! write the field out to file, which will be 1d and with the arbitrary order
    call ESMF_FieldWrite(fieldArb, fileName="field_gridArb.nc", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! create an auxiliary regDecomp grid with the same index space as gridArb
    ! periodic along i (must set here explicitly to match the gridArb
    ! ESMF_GridCreate1PeriDim() from above). In the long run,
    ! an arbDistr Grid will actually store a regDecomp auxiliary DistGrid 
    ! internally which holds the correct connections.
    allocate(connectionList(1))
    call ESMF_DistGridConnectionSet(connection=connectionList(1), &
      tileIndexA=1, tileIndexB=1, positionVector=(/iCount, 0/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    distgrid = ESMF_DistGridCreate(minIndex=(/1, 1/), &
      maxIndex=(/iCount, jCount/), connectionList=connectionList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! now create a regDecomp grid from arbDistr Grid, with auto redist coords
    gridAux = ESMF_GridCreate(gridArb, distgrid=distgrid, &
      name="OCN-GridAux", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#if 1
    ! since this is simple regDecomp grid, it can be written to VTK
    call ESMF_GridWriteVTK(gridAux, staggerloc=ESMF_STAGGERLOC_CENTER, &
      filename="OCN-GridAux_centers", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
    ! ceate an auxiliary field on gridAux to be used in RegridStore
    fieldAux = ESMF_FieldCreate(gridAux, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

#if 0
    ! test the fieldAux
    call ESMF_FieldFill(fieldAux, dataFillScheme="sincos", member=3, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldWrite(fieldAux, fileName="field_gridAux.nc", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

    ! pull out the sst field from exportState for testing regridding to it
    call ESMF_StateGet(exportState, itemName="sst", field=sstField, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! compute sparse matrix (factorList and factorIndexList) for a substitute
    ! call to RegridStore(), neither side is arbDistr
    call ESMF_FieldRegridStore(fieldAux, sstField, factorList=factorList, &
      factorIndexList=factorIndexList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! now use the regrid sparse matrix (factorList and factorIndexList) from
    ! above to precompute the routehandle for the arbDistr case. This works
    ! because the index space is identical.
    call ESMF_FieldSMMStore(fieldArb, sstField, factorList=factorList, &
      factorIndexList=factorIndexList, routehandle=rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! for final test fill the arbDistr side with identifiable data
    call ESMF_FieldFill(fieldArb, dataFillScheme="sincos", member=5, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! execute the sparse matrix to do the regrid (could have called FieldRegrid
    ! for clarity if desired)
    call ESMF_FieldSMM(fieldArb, sstField, routehandle=rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

#ifndef TEST_MULTI_TILE_GRID
! Write() does not currently support fields on multi-tile grids
    ! finally write the destination side (which is located in the exportState)
    ! to file for inspection
    call NUOPC_Write(exportState, fileNamePrefix="field_ocn_init_export_", &
      relaxedFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

    ! -->> use the arbDistr grid for "precip" field in the importState
    ! This is to test how a arbDistr grid gets handled during the transfer
    ! protocol. Search for "precip" in the ATM component to see the other side.

    ! importable field: surface_net_downward_shortwave_flux
    call NUOPC_Realize(importState, gridArb, fieldName="precip", &
      typekind=ESMF_TYPEKIND_R8, selection="realize_connected_remove_others", &
      dataFillScheme="sincos", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_LogWrite("Done realizing fields in OCN import/exportStates", &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine SetClock(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_TimeInterval)       :: stabilityTimeStep

    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! initialize internal clock
    ! here: parent Clock and stability timeStep determine actual model timeStep
    !TODO: stabilityTimeStep should be read in from configuation
    !TODO: or computed from internal Grid information
    call ESMF_TimeIntervalSet(stabilityTimeStep, m=5, rc=rc) ! 5 minute steps
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetClock(model, clock, stabilityTimeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_State)              :: importState, exportState
    type(ESMF_Field)              :: field
    type(ESMF_Time)               :: currTime
    type(ESMF_TimeInterval)       :: timeStep
    integer, save                 :: slice=1

    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep
    
    ! Because of the way that the internal Clock was set in SetClock(),
    ! its timeStep is likely smaller than the parent timeStep. As a consequence
    ! the time interval covered by a single parent timeStep will result in 
    ! multiple calls to the ModelAdvance() routine. Every time the currTime
    ! will come in by one internal timeStep advanced. This goes until the
    ! stopTime of the internal Clock has been reached.
    
    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing OCN from: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_TimePrint(currTime + timeStep, &
      preString="--------------------------------> to: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! - update the export fields
    call ESMF_StateGet(exportState, itemName="sss", field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldFill(field, dataFillScheme="sincos", member=1, step=slice, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_StateGet(exportState, itemName="sst", field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldFill(field, dataFillScheme="sincos", member=2, step=slice, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! write out the Fields in the importState and exportState
    call NUOPC_Write(importState, fileNamePrefix="field_ocn_import_", &
      timeslice=slice, overwrite=.true., relaxedFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#ifndef TEST_MULTI_TILE_GRID
! Write() does not currently support fields on multi-tile grids
    call NUOPC_Write(exportState, fileNamePrefix="field_ocn_export_", &
      timeslice=slice, overwrite=.true., relaxedFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
    slice = slice+1

  end subroutine

  !-----------------------------------------------------------------------------

end module
