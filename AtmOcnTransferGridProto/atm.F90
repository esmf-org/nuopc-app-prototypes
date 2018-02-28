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
#define TEST_MULTI_TILE_GRID

module ATM

  !-----------------------------------------------------------------------------
  ! ATM Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    model_routine_SS            => SetServices, &
    model_label_DataInitialize  => label_DataInitialize, &
    model_label_Advance         => label_Advance
  
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

    ! -> switching to IPD version that supports GeomObject transfer
    call ESMF_GridCompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p1"/), userRoutine=InitializeP1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p3"/), userRoutine=InitializeP3, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p4"/), userRoutine=InitializeP4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p5"/), userRoutine=InitializeP5, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! attach specializing method(s)
    call NUOPC_CompSpecialize(model, specLabel=model_label_Advance, &
      specRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(model, specLabel=model_label_DataInitialize, &
      specRoutine=DataInitialize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP0(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: model
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    rc = ESMF_SUCCESS

    ! Switch to IPDv03 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(model, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv03p"/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP1(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    ! importable field: sea_surface_salinity
    ! -> marked as "cannot provide"
    call NUOPC_Advertise(importState, &
      StandardName="sea_surface_salinity", name="sss", &
      TransferOfferGeomObject="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: sea_surface_temperature
    ! -> marked as "can provide"
    call NUOPC_Advertise(importState, &
      StandardName="sea_surface_temperature", name="sst", &
      TransferOfferGeomObject="can provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: sea_surface_height_above_sea_level
    ! -> marked as "can provide"
    call NUOPC_Advertise(importState, &
      StandardName="sea_surface_height_above_sea_level", name="ssh", &
      TransferOfferGeomObject="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_LogWrite("Done advertising fields in ATM importState", &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: air_pressure_at_sea_level
    ! -> marked as "cannot provide"
    call NUOPC_Advertise(exportState, &
      StandardName="air_pressure_at_sea_level", name="pmsl", &
      TransferOfferGeomObject="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! exportable field: surface_net_downward_shortwave_flux
    ! -> use default, i.e. marked as "will provide"
    call NUOPC_Advertise(exportState, &
      StandardName="surface_net_downward_shortwave_flux", name="rsns", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: precipitation_flux
    ! -> marked as "cannot provide"
    call NUOPC_Advertise(exportState, &
      StandardName="precipitation_flux", name="precip", &
      TransferOfferGeomObject="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_LogWrite("Done advertising fields in ATM exportState", &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP3(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables    
    type(ESMF_Field)                  :: field
    type(ESMF_Grid)                   :: gridIn, gridOut
    character(ESMF_MAXSTR)            :: transferAction
    
    rc = ESMF_SUCCESS
    
    ! create Grid objects for Fields
    gridIn = ESMF_GridCreate1PeriDimUfrm(maxIndex=(/100, 150/), &
      minCornerCoord=(/0._ESMF_KIND_R8, -50._ESMF_KIND_R8/), &
      maxCornerCoord=(/360._ESMF_KIND_R8, 70._ESMF_KIND_R8/), &
      staggerLocList=(/ESMF_STAGGERLOC_CENTER/), name="ATM-Grid", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    gridOut = gridIn ! for now out same as in

    ! importable field: sea_surface_temperature
    ! This Field was marked with TransferOfferGeomObject="can provide", so here
    ! we need to see what TransferActionGeomObject the Connector determined for
    ! this Field:
    call ESMF_StateGet(importState, field=field, itemName="sst", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_GetAttribute(field, name="TransferActionGeomObject", &
      value=transferAction, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (trim(transferAction)=="provide") then
      ! the Connector instructed the ATM to provide the Grid object for "sst"
      call ESMF_LogWrite("ATM is providing Grid for Field 'sst'.", &
        ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      field = ESMF_FieldCreate(name="sst", grid=gridIn, &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call NUOPC_Realize(importState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else  ! transferAction=="accept"
      ! the Connector instructed the ATM to accept the Grid from OCN for "sst"
      call ESMF_LogWrite("ATM is accepting Grid for Field 'sst'.", &
        ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    !NOTE: The sea_surface_salinity and air_pressure_at_sea_level Fields are
    !NOTE: not realized here because they were marked with 
    !NOTE: TransferOfferGeomObject="cannot provide".
    !NOTE: Expect the Connector to fill in a Grid object for these Fields.

    ! exportable field: surface_net_downward_shortwave_flux
    field = ESMF_FieldCreate(name="rsns", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_LogWrite("Done realizing fields in ATM import/exportStates "// &
      "that do not need grid transfer", ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP4(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Field)              :: field
    type(ESMF_Grid)               :: grid
    integer                       :: localDeCount
    character(80)                 :: name
    character(160)                :: msgString

    type(ESMF_DistGrid)           :: distgrid
    integer                       :: dimCount, tileCount, arbDimCount
    integer, allocatable          :: minIndexPTile(:,:), maxIndexPTile(:,:)
    integer                       :: connectionCount
    type(ESMF_DistGridConnection), allocatable :: connectionList(:)
    character(ESMF_MAXSTR)        :: transferAction
    logical                       :: regDecompFlag

    rc = ESMF_SUCCESS
    
    !NOTE: The air_pressure_at_sea_level (pmsl) Field should now have the
    !NOTE: accepted Grid available. It is still an empty Field, but with Grid,
    !NOTE: that contains a DistGrid with the provider decomposition.
    !NOTE: If the decomposition and distribution of the provided Grid is to
    !NOTE: be changed on the acceptor side (i.e. the ATM here) then this
    !NOTE: phase of Initialize is the place to do so and make the changes to
    !NOTE: the Grid object that is referenced by the "pmsl" Field.

    ! access the "pmsl" field in the exportState
    call ESMF_StateGet(exportState, field=field, itemName="pmsl", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! while this is still an empty field, it does now hold a Grid with DistGrid
    call ESMF_FieldGet(field, grid=grid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! inspect the Grid name
    call ESMF_GridGet(grid, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    write (msgString,*) "ATM - InitializeP4: transferred Grid name = ", name
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! access localDeCount to show this is a real Grid
    call ESMF_GridGet(grid, localDeCount=localDeCount, distgrid=distgrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    write (msgString,*) "ATM - InitializeP4: localDeCount = ", localDeCount
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Create a custom DistGrid, based on the minIndex, maxIndex of the 
    ! accepted DistGrid, but with a default regDecomp for the current VM
    ! that leads to 1DE/PET (as long as there are more PETs than tiles).
    
    ! get dimCount and tileCount
    call ESMF_DistGridGet(distgrid, dimCount=dimCount, tileCount=tileCount, &
      connectionCount=connectionCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! allocate minIndexPTile and maxIndexPTile accord. to dimCount and tileCount
    allocate(minIndexPTile(dimCount, tileCount), &
      maxIndexPTile(dimCount, tileCount))
    allocate(connectionList(connectionCount))
    
    ! get minIndex and maxIndex arrays
    call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, &
      maxIndexPTile=maxIndexPTile, connectionList=connectionList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

#if 0      
    ! report on the connections    
    print *, "connectionCount=", connectionCount
    do i=1, connectionCount
      call ESMF_DistGridConnectionPrint(connectionList(i))
    enddo
#endif

    ! create the new DistGrid with the same minIndexPTile and maxIndexPTile,
    ! but use default multi-tile regDecomp
    ! If the default regDecomp is not suitable, a custome one could be set
    ! up here and used.
    distgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile, &
      maxIndexPTile=maxIndexPTile, connectionList=connectionList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    deallocate(minIndexPTile, maxIndexPTile, connectionList)

    ! Create a new Grid on the new DistGrid
    grid = ESMF_GridCreate(distgrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! access localDeCount of the final Grid
    call ESMF_GridGet(grid, localDeCount=localDeCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    write (msgString,*) "ATM - InitializeP4: final Grid localDeCount = ", &
      localDeCount
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Swap out the transferred for new Grid in "pmsl" Field
    call ESMF_FieldEmptySet(field, grid=grid, rc=rc)    
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! -- deal with "precip" field in the exportState
    ! access the field
    call ESMF_StateGet(exportState, field=field, itemName="precip", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! access the grid
    call ESMF_FieldGet(field, grid=grid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! log the grid name
    call ESMF_GridGet(grid, name=name, distgrid=distgrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    write (msgString,*) "ATM - InitializeP4: transferred Grid name = ", name
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! get basic information out of the transferred DistGrid
    call ESMF_DistGridGet(distgrid, dimCount=dimCount, tileCount=tileCount, &
      connectionCount=connectionCount, regDecompFlag=regDecompFlag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    allocate(minIndexPTile(dimCount, tileCount), &
      maxIndexPTile(dimCount, tileCount))
    allocate(connectionList(connectionCount))
    call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, &
      maxIndexPTile=maxIndexPTile, connectionList=connectionList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! right now the ArbDimCount is transferred as an attribute on each field
    ! This should change in the future to be a propoerty that can be natively
    ! be queried of the transferred Grid, but that is not currently implemented.
    call ESMF_AttributeGet(field, name="ArbDimCount", value=arbDimCount, &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! make decision on whether the incoming Grid is arbDistr or not
    if (arbDimCount>0) then
      ! The provider defined an arbDistr grid
      !
      ! Need to make a choice here to either represent the grid as a
      ! regDecomp grid on the acceptor side, or to stay with arbDistr grid:
      !
      ! Setting the PRECIP_REGDECOMP macro will set up a regDecomp grid on the
      ! acceptor side.
      !
      ! Not setting the PRECIP_REGDECOMP macro will default into keeping the
      ! original arbDistr Grid.
      
#define PRECIP_REGDECOMP

#ifdef PRECIP_REGDECOMP
      ! Use a regDecomp representation for the grid
      ! first get tile min/max, only single tile supported for arbDistr Grid
      deallocate(minIndexPTile,maxIndexPTile)
      allocate(minIndexPTile(arbDimCount,1),maxIndexPTile(arbDimCount,1))
      call ESMF_AttributeGet(field, name="MinIndex", &
        valueList=minIndexPTile(:,1), &
        convention="NUOPC", purpose="Instance", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_AttributeGet(field, name="MaxIndex", &
        valueList=maxIndexPTile(:,1), &
        convention="NUOPC", purpose="Instance", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! create default regDecomp DistGrid
      distgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile, &
        maxIndexPTile=maxIndexPTile, connectionList=connectionList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! Create default regDecomp Grid
      grid = ESMF_GridCreate(distgrid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! swap out the transferred grid for the newly created one
      call ESMF_FieldEmptySet(field, grid=grid, rc=rc)    
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
#if 1
      write (msgString,*) "ATM - 'precip' minIndex = ", minIndexPTile, &
        "maxIndex = ", maxIndexPTile
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
#endif
      call ESMF_LogWrite("ATM - Just set Grid for 'precip' Field", &
        ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
#else
      ! Stick with the arbDistr representation of the grid:
      ! There is nothing to do here if the same number of DEs is kept on the
      ! acceptor side. Alternatively, the acceptor side could set up a more
      ! natural number of DEs (maybe same number as acceptor PETs), and then
      ! redistribute the arbSeqIndexList. Here simply keep the DEs of the
      ! provider Grid.
      call ESMF_GridGet(grid, localDeCount=localDeCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      write (msgString,*) &
        "ATM - InitializeP4: arbDistr: precip localDeCount = ", &
        localDeCount
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

#endif
    else
      ! Not arbDistr grid, so either regDecomp or deBlock grid
      if (regDecompFlag) then
        ! The provider used a regDecomp scheme for DistGrid creation:
        ! This means that the entire index space is covered (no holes), and
        ! it the easieast is just to use a regDecomp scheme on the acceptor
        ! side as well.
        distgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile, &
          maxIndexPTile=maxIndexPTile, connectionList=connectionList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        grid = ESMF_GridCreate(distgrid, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        ! swap out the transferred grid for the newly created one
        call ESMF_FieldEmptySet(field, grid=grid, rc=rc)    
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_LogWrite("ATM - Just set regDecomp Grid for 'precip' Field", &
          ESMF_LOGMSG_INFO, rc=rc)
      else
        ! The provider did NOT use a regDecomp scheme for DistGrid creation:
        ! This means that the provider was using deBlock lists to decompose the
        ! index space, which can lead to holes in the coverage.
        ! The acceptor side can either ignore holes, and use a regDecomp of the
        ! entire index space, or also use a deBlock approach to only cover the
        ! exact index space covered by the provider grid.
        ! If a regDecomp scheme is used, Redist() between provider side and
        ! acceptor side is still possible (both ways). It just means that 
        ! not all src/dst index points send/receive data.
        ! Using the regDecomp scheme is identical to the regDecompFlag branch
        ! of this if statement.
        ! Using the deBlock scheme can either mean that the transferred grid is
        ! directly used. It just means that the number provider DEs are using a
        ! default distribution across the acceptor PETs.
        ! Alternatively the DEs could be distributed differently by constructing
        ! a deBlockList out of the minIndexPDe and maxIndexPDe arrays here, and
        ! calling a deBlock DistGridCreate() and then build the Grid on it.
        ! -> here we just accept the provided Grid without change (same number 
        ! of DEs with the same deBlocks).
      endif
    endif
    deallocate(minIndexPTile, maxIndexPTile, connectionList)
     
    !------------------------------------------------------------------------
    ! Also must deal with transferred Grids in the importState
    call ESMF_LogWrite("ATM - InitializeP4: now dealing with importState", &
       ESMF_LOGMSG_INFO, rc=rc)
    
    ! access the "sss" field in the importState and set the Grid
    call ESMF_StateGet(importState, field=field, itemName="sss", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! construct a local Grid according to the transferred grid
    call ESMF_FieldGet(field, grid=grid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridGet(grid, distgrid=distgrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_DistGridGet(distgrid, dimCount=dimCount, tileCount=tileCount, &
      connectionCount=connectionCount, regDecompFlag=regDecompFlag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    allocate(minIndexPTile(dimCount, tileCount), &
      maxIndexPTile(dimCount, tileCount))
    allocate(connectionList(connectionCount))
    call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, &
      maxIndexPTile=maxIndexPTile, connectionList=connectionList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (regDecompFlag) then
      ! The provider used a regDecomp scheme for DistGrid creation:
      ! This means that the entire index space is covered (no holes), and
      ! it the easieast is just to use a regDecomp scheme on the acceptor
      ! side as well.
      distgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile, &
        maxIndexPTile=maxIndexPTile, connectionList=connectionList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      grid = ESMF_GridCreate(distgrid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! swap out the transferred grid for the newly created one
      call ESMF_FieldEmptySet(field, grid=grid, rc=rc)    
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_LogWrite("ATM - Just set Grid for 'sss' Field", &
        ESMF_LOGMSG_INFO, rc=rc)
    else
      ! The provider did NOT use a regDecomp scheme for DistGrid creation:
      ! This means that the provider was using deBlock lists to decompose the
      ! index space, which can lead to holes in the coverage.
      ! The acceptor side can either ignore holes, and use a regDecomp of the
      ! entire index space, or also use a deBlock approach to only cover the
      ! exact index space covered by the provider grid.
      ! If a regDecomp scheme is used, Redist() between provider side and
      ! acceptor side is still possible (both ways). It just means that 
      ! not all src/dst index points send/receive data.
      ! Using the regDecomp scheme is identical to the regDecompFlag branch
      ! of this if statement.
      ! Using the deBlock scheme can either mean that the transferred grid is
      ! directly used. It just means that the number provider DEs are using a
      ! default distribution across the acceptor PETs.
      ! Alternatively the DEs could be distributed differently by constructing
      ! a deBlockList out of the minIndexPDe and maxIndexPDe arrays here, and
      ! calling a deBlock DistGridCreate() and then build the Grid on it.
      ! -> here we just accept the provided Grid without change (same number of
      ! DEs with the same deBlocks).
    endif
    deallocate(minIndexPTile, maxIndexPTile, connectionList)
  
    ! Also must swap the Grid for the "sst" Field in the importState
    ! if transferAction indicates "accept". Assume that SST is defined on 
    ! the same grid as SSS.

    ! access the "sst" field in the importState
    call ESMF_StateGet(importState, field=field, itemName="sst", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_GetAttribute(field, name="TransferActionGeomObject", &
      value=transferAction, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (trim(transferAction)=="accept") then
      ! accept the incoming Grid object for the "sst" Field
      if (regDecompFlag) then
        ! for a regDecomp scheme, definitely the newly constructed Grid on the
        ! acceptor side makes a lot more sense.
        call ESMF_FieldEmptySet(field, grid=grid, rc=rc)    
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_LogWrite("ATM - Just set Grid for 'sst' Field", &
          ESMF_LOGMSG_INFO, rc=rc)
      else
        ! for deBlock scheme, just keep the provided Grid with the same 
        ! deBlocks as provider defined.
      endif
    endif

    call ESMF_LogWrite("ATM - InitializeP4: DONE", &
       ESMF_LOGMSG_INFO, rc=rc)

  end subroutine
    
  !-----------------------------------------------------------------------------

  subroutine InitializeP5(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Field)              :: field
    type(ESMF_Grid)               :: grid
    type(ESMF_Array)              :: array
    character(80)                 :: name
    character(160)                :: msgString
    type(ESMF_FieldStatus_Flag)   :: fieldStatus
    integer                       :: staggerEdgeLWidth(2)
    integer                       :: staggerEdgeUWidth(2)
    integer                       :: staggerAlign(2)

    rc = ESMF_SUCCESS

    ! access the "sss" field in the importState
    call ESMF_StateGet(importState, field=field, itemName="sss", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! the transferred Grid is already set, allocate memory for data by complete
    call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! log a message
    call ESMF_LogWrite("ATM - Just completed the 'sss' Field", &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! access the "sst" field in the importState
    call ESMF_StateGet(importState, field=field, itemName="sst", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! check status of "sst" field and decide on action
    call ESMF_FieldGet(field, status=fieldStatus, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (fieldStatus==ESMF_FIELDSTATUS_COMPLETE) then
      ! log a message
      call ESMF_LogWrite("ATM - The 'sst' Field was already complete", &
        ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      ! the transferred Grid is already set, allocate memory for data by complete
      call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! log a message
      call ESMF_LogWrite("ATM - Just completed the 'sst' Field", &
        ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! access the "ssh" field in the importState
    call ESMF_StateGet(importState, field=field, itemName="ssh", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! the transferred Grid is already set, allocate memory for data by complete
    call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! log a message
    call ESMF_LogWrite("ATM - Just completed the 'ssh' Field", &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#ifdef TEST_MULTI_TILE_GRID    
    ! write cubed sphere grid out to VTK
    call ESMF_FieldGet(field, grid=grid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridWriteVTK(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
      filename="ATM-accepted-Grid-ssh_centers", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

    ! access the "pmsl" field in the exportState
    call ESMF_StateGet(exportState, field=field, itemName="pmsl", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! the transferred Grid is already set, allocate memory for data by complete
    call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, &
      totalLWidth=(/1,1/), totalUWidth=(/1,1/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! log a message
    call ESMF_LogWrite("ATM - Just completed the 'pmsl' Field", &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! inspect the Grid name
    call ESMF_FieldGet(field, grid=grid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridGet(grid, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    write (msgString,*) "ATM - InitializeP5: transferred Grid name = ", name
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! check the staggerEdgeWidth of the transferred grid 
#ifdef TEST_GRID_EDGE_WIDTHS
    ! center stagger
    call ESMF_GridGet(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
      staggerEdgeLWidth=staggerEdgeLWidth, &
      staggerEdgeUWidth=staggerEdgeUWidth, &
      staggerAlign=staggerAlign, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    print *, "staggerEdgeLWidth", staggerEdgeLWidth
    print *, "staggerEdgeUWidth", staggerEdgeUWidth
    print *, "staggerAlign", staggerAlign
    if (any(staggerEdgeLWidth /= (/0,0/))) then
      call ESMF_LogSetError(ESMF_RC_VAL_WRONG, &
        msg="Wrong staggerEdgeLWidth for ESMF_STAGGERLOC_CENTER", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    endif
    if (any(staggerEdgeUWidth /= (/0,0/))) then
      call ESMF_LogSetError(ESMF_RC_VAL_WRONG, &
        msg="Wrong staggerEdgeUWidth for ESMF_STAGGERLOC_CENTER", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    endif
    ! corner stagger
    call ESMF_GridGet(grid, staggerloc=ESMF_STAGGERLOC_CORNER, &
      staggerEdgeLWidth=staggerEdgeLWidth, &
      staggerEdgeUWidth=staggerEdgeUWidth, &
      staggerAlign=staggerAlign, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    print *, "staggerEdgeLWidth", staggerEdgeLWidth
    print *, "staggerEdgeUWidth", staggerEdgeUWidth
    print *, "staggerAlign", staggerAlign
    if (any(staggerEdgeLWidth /= (/1,1/))) then
      call ESMF_LogSetError(ESMF_RC_VAL_WRONG, &
        msg="Wrong staggerEdgeLWidth for ESMF_STAGGERLOC_CORNER", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    endif
    if (any(staggerEdgeUWidth /= (/0,0/))) then
      call ESMF_LogSetError(ESMF_RC_VAL_WRONG, &
        msg="Wrong staggerEdgeuWidth for ESMF_STAGGERLOC_CORNER", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    endif
    ! edge1 stagger
    call ESMF_GridGet(grid, staggerloc=ESMF_STAGGERLOC_EDGE1, &
      staggerEdgeLWidth=staggerEdgeLWidth, &
      staggerEdgeUWidth=staggerEdgeUWidth, &
      staggerAlign=staggerAlign, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    print *, "staggerEdgeLWidth", staggerEdgeLWidth
    print *, "staggerEdgeUWidth", staggerEdgeUWidth
    print *, "staggerAlign", staggerAlign
    if (any(staggerEdgeLWidth /= (/0,1/))) then
      call ESMF_LogSetError(ESMF_RC_VAL_WRONG, &
        msg="Wrong staggerEdgeLWidth for ESMF_STAGGERLOC_EDGE1", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    endif
    if (any(staggerEdgeUWidth /= (/1,1/))) then
      call ESMF_LogSetError(ESMF_RC_VAL_WRONG, &
        msg="Wrong staggerEdgeUWidth for ESMF_STAGGERLOC_EDGE1", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    endif
    ! edge2 stagger
    call ESMF_GridGet(grid, staggerloc=ESMF_STAGGERLOC_EDGE2, &
      staggerEdgeLWidth=staggerEdgeLWidth, &
      staggerEdgeUWidth=staggerEdgeUWidth, &
      staggerAlign=staggerAlign, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    print *, "staggerEdgeLWidth", staggerEdgeLWidth
    print *, "staggerEdgeUWidth", staggerEdgeUWidth
    print *, "staggerAlign", staggerAlign
    if (any(staggerEdgeLWidth /= (/1,0/))) then
      call ESMF_LogSetError(ESMF_RC_VAL_WRONG, &
        msg="Wrong staggerEdgeLWidth for ESMF_STAGGERLOC_EDGE2", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    endif
    if (any(staggerEdgeUWidth /= (/0,1/))) then
      call ESMF_LogSetError(ESMF_RC_VAL_WRONG, &
        msg="Wrong staggerEdgeUWidth for ESMF_STAGGERLOC_EDGE2", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    endif
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
    call ESMF_GridGetCoord(grid, coordDim=1, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayWrite(array, "array_ATM-grid_center_coord1.nc", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridGetCoord(grid, coordDim=2, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayWrite(array, "array_ATM-grid_center_coord2.nc", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#ifdef TEST_GRID_EDGE_WIDTHS
    ! corner:
    call ESMF_GridGetCoord(grid, staggerloc=ESMF_STAGGERLOC_CORNER, &
      coordDim=1, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayWrite(array, "array_ATM-grid_corner_coord1.nc", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridGetCoord(grid, staggerloc=ESMF_STAGGERLOC_CORNER, &
      coordDim=2, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayWrite(array, "array_ATM-grid_corner_coord2.nc", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! edge1:
    call ESMF_GridGetCoord(grid, staggerloc=ESMF_STAGGERLOC_EDGE1, &
      coordDim=1, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayWrite(array, "array_ATM-grid_edge1_coord1.nc", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridGetCoord(grid, staggerloc=ESMF_STAGGERLOC_EDGE1, &
      coordDim=2, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayWrite(array, "array_ATM-grid_edge1_coord2.nc", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! edge2:
    call ESMF_GridGetCoord(grid, staggerloc=ESMF_STAGGERLOC_EDGE2, &
      coordDim=1, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayWrite(array, "array_ATM-grid_edge2_coord1.nc", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridGetCoord(grid, staggerloc=ESMF_STAGGERLOC_EDGE2, &
      coordDim=2, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayWrite(array, "array_ATM-grid_edge2_coord2.nc", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
#endif

#if 1
    ! write out the Grid into VTK file for inspection
    call ESMF_GridWriteVTK(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
      filename="ATM-accepted-Grid-pmsl_centers", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LogWrite("Done writing ATM-accepted-Grid-pmsl_centers VTK", &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

    ! access the "precip" field in the exportState
    call ESMF_StateGet(exportState, field=field, itemName="precip", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! the transferred Grid is already set, allocate memory for data by complete
    call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#if 0
    ! This does NOT currently work if precip on acceptor side stays arbGrid
    call ESMF_FieldGet(field, grid=grid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! write out the Grid into VTK file for inspection
    call ESMF_GridWriteVTK(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
      filename="ATM-accepted-Grid-precip_centers", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
    ! log a message
    call ESMF_LogWrite("ATM - Just completed the 'precip' Field", &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine
    
  !-----------------------------------------------------------------------------

  subroutine DataInitialize(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_State)                  :: exportState
    type(ESMF_Field)                  :: field
    real(kind=ESMF_KIND_R8),  pointer :: dataPtr(:,:)
    integer                           :: i, j
    integer                           :: localDe, localDeCount

    rc = ESMF_SUCCESS

    ! query the Component for its exportState
    call NUOPC_ModelGet(model, exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! air_pressure_at_sea_level
    call ESMF_StateGet(exportState, field=field, itemName="pmsl", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! initialize data
    call ESMF_FieldGet(field, localDeCount=localDeCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    do localDe=0, localDeCount-1
      call ESMF_FieldGet(field, localDe=localDe, farrayPtr=dataPtr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      do j=lbound(dataPtr,2),ubound(dataPtr,2)
      do i=lbound(dataPtr,1),ubound(dataPtr,1)
        dataPtr(i,j) = real(i)
      enddo
      enddo
    enddo
    ! output to file
    call NUOPC_Write(field, fileName="field_atm_init_export_pmsl.nc", &
      status=ESMF_FILESTATUS_REPLACE, relaxedflag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! set "Updated"
    call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! surface_net_downward_shortwave_flux
    call ESMF_StateGet(exportState, field=field, itemName="rsns", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! initialize data
    call ESMF_FieldGet(field, localDeCount=localDeCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    do localDe=0, localDeCount-1
      call ESMF_FieldGet(field, localDe=localDe, farrayPtr=dataPtr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      do j=lbound(dataPtr,2),ubound(dataPtr,2)
      do i=lbound(dataPtr,1),ubound(dataPtr,1)
        dataPtr(i,j) = real(j)
      enddo
      enddo
    enddo
    ! output to file
    call NUOPC_Write(field, fileName="field_atm_init_export_rsns.nc", &
      status=ESMF_FILESTATUS_REPLACE, relaxedflag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! set "Updated"
    call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! precipitation_flux
    call ESMF_StateGet(exportState, field=field, itemName="precip", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! initialize data
    call ESMF_FieldGet(field, localDeCount=localDeCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldFill(field, dataFillScheme="sincos", member=10, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! output to file
    call NUOPC_Write(field, fileName="field_atm_init_export_precip.nc", &
      status=ESMF_FILESTATUS_REPLACE, relaxedflag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! set "Updated"
    call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! indicate that data initialization is complete (breaking out of init-loop)
    call NUOPC_CompAttributeSet(model, &
      name="InitializeDataComplete", value="true", rc=rc)
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
    
    ! Because of the way that the internal Clock was set by default,
    ! its timeStep is equal to the parent timeStep. As a consequence the
    ! currTime + timeStep is equal to the stopTime of the internal Clock
    ! for this call of the ModelAdvance() routine.
    
    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing ATM from: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_ClockPrint(clock, options="stopTime", &
      preString="--------------------------------> to: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! write out the Fields in the importState and exportState
#ifndef TEST_MULTI_TILE_GRID
! Write() does not currently support fields on multi-tile grids
    call NUOPC_Write(importState, fileNamePrefix="field_atm_import_", &
      timeslice=slice, overwrite=.true., relaxedFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
    call NUOPC_Write(exportState, fileNamePrefix="field_atm_export_", &
      timeslice=slice, overwrite=.true., relaxedFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    slice = slice+1

  end subroutine

  !-----------------------------------------------------------------------------

end module
