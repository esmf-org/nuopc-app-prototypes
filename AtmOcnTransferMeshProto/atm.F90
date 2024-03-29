!==============================================================================
! Earth System Modeling Framework
! Copyright (c) 2002-2024, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module ATM

  !-----------------------------------------------------------------------------
  ! ATM Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    modelSS    => SetServices

  implicit none

  private

  public SetVM, SetServices

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    ! derive from NUOPC_Model
    call NUOPC_CompDerive(model, modelSS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! specialize model
    call NUOPC_CompSpecialize(model, specLabel=label_Advertise, &
      specRoutine=Advertise, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(model, specLabel=label_RealizeProvided, &
      specRoutine=RealizeProvided, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(model, specLabel=label_AcceptTransfer, &
      specRoutine=AcceptTransfer, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(model, specLabel=label_RealizeAccepted, &
      specRoutine=RealizeAccepted, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(model, specLabel=label_DataInitialize, &
      specRoutine=DataInitialize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(model, specLabel=label_Advance, &
      specRoutine=Advance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Advertise(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)        :: importState, exportState

    rc = ESMF_SUCCESS

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
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

    ! importable field: sea_surface_salinity
    ! -> marked as "cannot provide"
    call NUOPC_Advertise(importState, &
      StandardName="sea_surface_salinity", name="sss", &
      TransferOfferGeomObject="cannot provide", rc=rc)
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

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine RealizeProvided(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)                  :: importState, exportState
    type(ESMF_Field)                  :: field
    type(ESMF_Grid)                   :: gridIn, gridOut
    integer                           :: i, j
    real(kind=ESMF_KIND_R8),  pointer :: lonPtr(:,:), latPtr(:,:)
    character(ESMF_MAXSTR)            :: transferAction

    rc = ESMF_SUCCESS

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! create Grid objects for Fields
    gridIn = ESMF_GridCreate1PeriDim(minIndex=(/1,1/), maxIndex=(/200,100/), &
      indexflag=ESMF_INDEX_GLOBAL, coordSys=ESMF_COORDSYS_SPH_DEG, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridAddCoord(gridIn, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridGetCoord(gridIn, coordDim=1, farrayPtr=lonPtr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridGetCoord(gridIn, coordDim=2, farrayPtr=latPtr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    do j=lbound(lonPtr,2),ubound(lonPtr,2)
    do i=lbound(lonPtr,1),ubound(lonPtr,1)
      lonPtr(i,j) = 360./real(200) * (i-1)
      latPtr(i,j) = 100./real(100) * (j-1) - 50.
    enddo
    enddo

    gridOut = gridIn ! for now out same as in

    ! importable field: sea_surface_temperature
    ! This Field was marked with TransferOfferGeomObject="can provide", so here
    ! we need to see what ConsumerTransferAction the Connector determined for
    ! this Field:
    call ESMF_StateGet(importState, field=field, itemName="sst", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_GetAttribute(field, name="ConsumerTransferAction", &
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
      ! the Connector instructed the ATM to accept the Mesh from OCN for "sst"
      call ESMF_LogWrite("ATM is accepting Mesh for Field 'sst'.", &
        ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! destroy the grid actually not needed
      call ESMF_GridDestroy(gridIn, noGarbage=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    !NOTE: The air_pressure_at_sea_level (pmsl) Field is not realized here
    !NOTE: because it was marked with TransferOfferGeomObject="cannot provide".
    !NOTE: It is expected that the Connector will fill in a Grid object for it.

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine AcceptTransfer(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)          :: importState, exportState
    type(ESMF_Field)          :: field
    type(ESMF_Mesh)           :: mesh
    integer                   :: localDeCount
    character(160)            :: msgString
    type(ESMF_DistGrid)       :: elementDG, nodalDG
    type(ESMF_DistGrid)       :: newElementDG, newNodalDG
    type(ESMF_DELayout)       :: delayout
    character(80)             :: name

    rc = ESMF_SUCCESS

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !NOTE: The air_pressure_at_sea_level (pmsl) Field should now have the
    !NOTE: accepted Mesh available. It is still an empty Field, but with Mesh.
    !NOTE: If the decomposition and distribution of the accepted Mesh is to
    !NOTE: be changed on the acceptor side (i.e. the ATM here) then this
    !NOTE: phase of Initialize is the place to do so and make the changes to
    !NOTE: the Mesh object that is referenced by the "pmsl" Field.

    ! access the "pmsl" field in the exportState
    call ESMF_StateGet(exportState, field=field, itemName="pmsl", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! while this is still an empty field, it does now hold a Mesh with DistGrid
    call ESMF_FieldGet(field, mesh=mesh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! NOTE:
    ! cannot write the transferred Mesh here, because it does NOT contain
    ! coordinates yet

    ! get distgrids out of mesh
    call ESMF_MeshGet(mesh, nodalDistgrid=nodalDG, elementDistgrid=elementDG, &
      name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_PointerLog(mesh%this, &
      prefix="ATM - AcceptTransfer: exportState Mesh name="//trim(name)//": ",&
      rc=rc)

    ! The acceptor side can either use the nodal DistGrid, or the element
    ! DistGrid, or both to define its own decomposition and distribution of
    ! the transferred Mesh. Use the following two macros to define which
    ! DistGrid to use for defining the acceptor side decomposition.

#define USE_NODAL_DG_off
#define USE_ELEMENT_DG

#ifdef USE_NODAL_DG
    ! Create a custom DistGrid, based on the minIndex, maxIndex of the
    ! accepted DistGrid, but with a default regDecomp for the current VM
    ! that leads to 1DE/PET (as long as there are more PETs than tiles).

#if 1
    ! inspect the transferred nodalDG
    call ESMF_DistGridGet(nodalDG, delayout=delayout, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! access localDeCount to show some info
    call ESMF_DELayoutGet(delayout, localDeCount=localDeCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! report localDeCount to log
    write (msgString,"(A,I3)") &
      "ATM - AcceptTransfer: exportState Mesh="//trim(name)// &
        " nodal DistGrid localDeCount = ", localDeCount
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

    ! Use an ESMF method to create a balanced DistGrid with 1DE/PET
    newNodalDG = ESMF_DistGridCreate(nodalDG, balanceFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

#ifdef USE_ELEMENT_DG
    ! Create a custom DistGrid, based on the minIndex, maxIndex of the
    ! accepted DistGrid, but with a default regDecomp for the current VM
    ! that leads to 1DE/PET (as long as there are more PETs than tiles).

#if 1
    ! inspect the transferred elementDG
    call ESMF_DistGridGet(elementDG, delayout=delayout, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! access localDeCount to show some info
    call ESMF_DELayoutGet(delayout, localDeCount=localDeCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! report localDeCount to log
    write (msgString,"(A,I3)") &
      "ATM - AcceptTransfer: exportState Mesh="//trim(name)// &
      " element DistGrid localDeCount = ", localDeCount
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

    ! Use an ESMF method to create a balanced DistGrid with 1DE/PET
    newElementDG = ESMF_DistGridCreate(elementDG, balanceFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

#if (defined USE_NODAL_DG && defined USE_ELEMENT_DG)
    ! Create a new Mesh on both new DistGrid
    mesh = ESMF_MeshEmptyCreate(nodalDistGrid=newNodalDG, &
      elementDistGrid=newElementDG, name="ATM-custom-"//trim(name), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#elif (defined USE_NODAL_DG)
    ! Create a new Mesh on new nodal DistGrid
    mesh = ESMF_MeshEmptyCreate(nodalDistGrid=newNodalDG, &
      name="ATM-custom-"//trim(name), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#elif (defined USE_ELEMENT_DG)
    ! Create a new Mesh on new element DistGrid
    mesh = ESMF_MeshEmptyCreate(elementDistGrid=newElementDG, &
      name="ATM-custom-"//trim(name), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

    ! Swap Mesh in the "pmsl" field
    call ESMF_FieldEmptySet(field, mesh=mesh, rc=rc)
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

    ! while this is still an empty field, it does now hold a Mesh with DistGrid
    call ESMF_FieldGet(field, mesh=mesh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! NOTE:
    ! cannot write the transferred Mesh here, because it does NOT contain
    ! coordinates yet

    ! get distgrids out of mesh
    call ESMF_MeshGet(mesh, nodalDistgrid=nodalDG, elementDistgrid=elementDG, &
      name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_PointerLog(mesh%this, &
      prefix="ATM - AcceptTransfer: importState Mesh name="//trim(name)//": ",&
      rc=rc)

    ! The acceptor side can either use the nodal DistGrid, or the element
    ! DistGrid, or both to define its own decomposition and distribution of
    ! the transferred Mesh. Use the following two macros to define which
    ! DistGrid to use for defining the acceptor side decomposition.

#undef USE_NODAL_DG
#undef USE_ELEMENT_DG
#define USE_NODAL_DG
#define USE_ELEMENT_DG

#ifdef USE_NODAL_DG
    ! Create a custom DistGrid, based on the minIndex, maxIndex of the
    ! accepted DistGrid, but with a default regDecomp for the current VM
    ! that leads to 1DE/PET (as long as there are more PETs than tiles).

#if 1
    ! inspect the transferred nodalDG
    call ESMF_DistGridGet(nodalDG, delayout=delayout, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! access localDeCount to show some info
    call ESMF_DELayoutGet(delayout, localDeCount=localDeCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! report localDeCount to log
    write (msgString,"(A,I3)") &
      "ATM - AcceptTransfer: importState Mesh="//trim(name)// &
      " nodal DistGrid localDeCount = ", localDeCount
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

    ! Use an ESMF method to create a balanced DistGrid with 1DE/PET
    newNodalDG = ESMF_DistGridCreate(nodalDG, balanceFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

#ifdef USE_ELEMENT_DG
    ! Create a custom DistGrid, based on the minIndex, maxIndex of the
    ! accepted DistGrid, but with a default regDecomp for the current VM
    ! that leads to 1DE/PET (as long as there are more PETs than tiles).

#if 1
    ! inspect the transferred elementDG
    call ESMF_DistGridGet(elementDG, delayout=delayout, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! access localDeCount to show some info
    call ESMF_DELayoutGet(delayout, localDeCount=localDeCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! report localDeCount to log
    write (msgString,"(A,I3)") &
      "ATM - AcceptTransfer: importState Mesh="//trim(name)// &
      " element DistGrid localDeCount = ", localDeCount
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

    ! Use an ESMF method to create a balanced DistGrid with 1DE/PET
    newElementDG = ESMF_DistGridCreate(elementDG, balanceFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

#if (defined USE_NODAL_DG && defined USE_ELEMENT_DG)
    ! Create a new Mesh on both new DistGrid
    mesh = ESMF_MeshEmptyCreate(nodalDistGrid=newNodalDG, &
      elementDistGrid=newElementDG, name="ATM-custom-"//trim(name), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#elif (defined USE_NODAL_DG)
    ! Create a new Mesh on new nodal DistGrid
    mesh = ESMF_MeshEmptyCreate(nodalDistGrid=newNodalDG, &
      name="ATM-custom-"//trim(name), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#elif (defined USE_ELEMENT_DG)
    ! Create a new Mesh on new element DistGrid
    mesh = ESMF_MeshEmptyCreate(elementDistGrid=newElementDG, &
      name="ATM-custom-"//trim(name), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

    ! Swap Mesh in the "sst" field
    call ESMF_FieldEmptySet(field, mesh=mesh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! access the "sss" field in the importState
    call ESMF_StateGet(importState, field=field, itemName="sss", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Swap Mesh in the "sss" field to be the same one that was constructed
    ! for "sst". This way the NUOPC Layer knows that both fields are to be
    ! built on the same Mesh (with same distribution) and will transfer the
    ! Mesh coordinates only once.
    ! If on the other hand a different distribution is desired for different
    ! fields, then construct separate Meshes and swap those into the field.
    call ESMF_FieldEmptySet(field, mesh=mesh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine RealizeAccepted(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)              :: importState, exportState
    type(ESMF_Field)              :: field
    character(160)                :: msgString
    type(ESMF_Field)              :: fieldIn, fieldOut
    type(ESMF_RouteHandle)        :: rh
    type(ESMF_Mesh)               :: mesh
    character(80)                 :: fieldName, meshName

    rc = ESMF_SUCCESS

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! realize "pmsl" field in the exportState, with transferred Mesh
    call NUOPC_Realize(exportState, fieldName="pmsl", field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

#if 1
    call ESMF_FieldGet(field, mesh=mesh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_MeshWrite(mesh, filename="Atm-MeshOut_corners", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LogWrite("Done writing ATM-MeshOut_corners VTK", &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

#if 1
    ! analyze the Mesh on which this field is created
    call ESMF_FieldGet(field, mesh=mesh, name=fieldName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_MeshGet(mesh, name=meshName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_PointerLog(mesh%this, &
      prefix="Atm-exportState Mesh fieldName="//trim(fieldName)// &
      " meshName="//trim(meshName)//": ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif 

    fieldOut = field ! keep field for RegridStore() test

    call ESMF_LogWrite("ATM - Just completed the 'pmsl' Field", &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! realize "sst" field in the importState, with transferred Mesh
    ! test the option here to specify an explicit typekind
    call NUOPC_Realize(importState, fieldName="sst", &
      typekind=ESMF_TYPEKIND_R4, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

#if 1
    call ESMF_FieldGet(field, mesh=mesh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_MeshWrite(mesh, filename="Atm-MeshIn_corners", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LogWrite("Done writing ATM-MeshIn_corners VTK", &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

#if 1
    ! analyze the Mesh on which this field is created
    call ESMF_FieldGet(field, mesh=mesh, name=fieldName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_MeshGet(mesh, name=meshName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_PointerLog(mesh%this, &
      prefix="Atm-importState Mesh fieldName="//trim(fieldName)// &
      " meshName="//trim(meshName)//": ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif 

    fieldIn = field ! keep field for RegridStore() test

    call ESMF_LogWrite("ATM - Just completed the 'sst' Field", &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! test a regrid between to fields that were created on transferred Meshes
    call ESMF_FieldRegridStore(fieldIn, fieldOut, routehandle=rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! release the RouteHandle
    call ESMF_FieldRegridRelease(rh, noGarbage=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! realize "sss" field in the importState, with transferred Mesh
    ! test the option here to specify an explicit typekind
    call NUOPC_Realize(importState, fieldName="sss", &
      typekind=ESMF_TYPEKIND_R4, field=field, rc=rc)
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
    real(kind=ESMF_KIND_R8),  pointer :: dataPtr(:)
    integer                           :: i, j
    integer                           :: localDe, localDeCount

    rc = ESMF_SUCCESS

    ! query for exportState
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
#if 0
    ! FieldFill is currently limited wrt Mesh and undstr. dims
    call ESMF_FieldFill(field, dataFillScheme="sincos", member=2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

    ! output to file
    call NUOPC_Write(field, fileName="field_pmsl_init.nc", &
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

  subroutine Advance(model, rc)
!$  use omp_lib
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    integer, save               :: slice=1
    type(ESMF_VM)               :: vm
    integer                     :: currentSsiPe, i, tid, unit, localPet
    character(len=160)          :: msgString

    rc = ESMF_SUCCESS

    ! query for clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridCompGet(model, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_VMGet(vm, localPet=localPet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Now can use OpenMP for fine grained parallelism...
    ! Here just write info about the PET-local OpenMP threads to Log.
!$omp parallel private(msgString, currentSsiPe)
!$omp critical
!$    call ESMF_VMGet(vm, currentSsiPe=currentSsiPe)
!$    write(msgString,'(A,I4,A,I4,A,I4,A,I4,A,I4)') &
!$      "thread_num=", omp_get_thread_num(), &
!$      "   currentSsiPe=", currentSsiPe, &
!$      "   num_threads=", omp_get_num_threads(), &
!$      "   max_threads=", omp_get_max_threads(), &
!$      "   num_procs=", omp_get_num_procs()
!$    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
!$omp end critical
!$omp end parallel

#define FILEOUT_off
! Activating FILEOUT triggers a hang for Intel < 19.1.1 when multi-threaded
! due to an Intel bug
#ifdef FILEOUT
    unit = localPet + 100
#else
    unit = 6
#endif
!$omp parallel private(tid)
    tid = -1 ! initialize to obvious value if building without OpenMP
!$  tid = omp_get_thread_num()
!$omp do
    do i=1, 100
      write(unit,*) "ATM test write, localPet=", localPet, "  tid=", tid, &
        "  slice=", slice, "  i=", i
    enddo
!$omp end parallel

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep

    ! Because of the way that the internal Clock was set by default,
    ! its timeStep is equal to the parent timeStep. As a consequence the
    ! currTime + timeStep is equal to the stopTime of the internal Clock
    ! for this call of the Advance() routine.

    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing ATM from: ", unit=msgString, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_ClockPrint(clock, options="stopTime", &
      preString="---------------------> to: ", unit=msgString, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    slice = slice+1

  end subroutine

  !-----------------------------------------------------------------------------

end module
