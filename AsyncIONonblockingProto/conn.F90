!==============================================================================
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module CON

  !-----------------------------------------------------------------------------
  ! Connector Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Connector, &
    connSS      => SetServices

  implicit none

  private

  logical, save                 :: outstanding=.false.

  public SetServices

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(connector, rc)
    type(ESMF_CplComp)   :: connector
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    ! derive from NUOPC_Connector
    call NUOPC_CompDerive(connector, connSS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! specialize connector
    call NUOPC_CompSpecialize(connector, specLabel=label_ComputeRouteHandle, &
      specRoutine=ComputeRH, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(connector, specLabel=label_ExecuteRouteHandle, &
      specRoutine=ExecuteRH, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(connector, specLabel=label_ReleaseRouteHandle, &
      specRoutine=ReleaseRH, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ComputeRH(connector, rc)
    type(ESMF_CplComp)  :: connector
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)              :: state
    type(ESMF_FieldBundle)        :: dstFields, srcFields
    type(ESMF_Field), allocatable :: fields(:)
    integer                       :: fieldCount, i
    type(ESMF_Field)              :: field
    type(ESMF_RouteHandle)        :: rh
    type(ESMF_Array)              :: dstArray, srcArray

    rc = ESMF_SUCCESS

    call NUOPC_ConnectorGet(connector, srcFields=srcFields, &
      dstFields=dstFields, state=state, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Pull out first Array from FieldBundles and precompute RH from ArrayRedist
    call ESMF_FieldBundleGet(dstFields, fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    allocate(fields(fieldCount))
    call ESMF_FieldBundleGet(dstFields, fieldList=fields, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldGet(fields(1), array=dstArray, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    deallocate(fields)
    call ESMF_FieldBundleGet(srcFields, fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    allocate(fields(fieldCount))
    call ESMF_FieldBundleGet(srcFields, fieldList=fields, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldGet(fields(1), array=srcArray, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    deallocate(fields)
    ! compute the RouteHandle for srcArray->dstArray
    call ESMF_ArrayRedistStore(srcArray, dstArray, routehandle=rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_ConnectorSet(connector, rh=rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ExecuteRH(connector, rc)
    type(ESMF_CplComp)  :: connector
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_FieldBundle)        :: dstFields, srcFields
    type(ESMF_Field), allocatable :: fields(:)
    integer                       :: fieldCount, i
    type(ESMF_Field)              :: field
    type(ESMF_RouteHandle)        :: rh
    type(ESMF_Array)              :: dstArray, srcArray
    integer                       :: localPet, petCount

    rc = ESMF_SUCCESS

    call NUOPC_ConnectorGet(connector, srcFields=srcFields, &
      dstFields=dstFields, rh=rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_CplCompGet(connector, localPet=localPet, petCount=petCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! for now assume that there is a single Field on both the src and dst side

    ! Pull out first Array from FieldBundles and execute RH for ArrayRedist
    call ESMF_FieldBundleGet(dstFields, fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    allocate(fields(fieldCount))
    call ESMF_FieldBundleGet(dstFields, fieldList=fields, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldGet(fields(1), array=dstArray, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    deallocate(fields)
    call ESMF_FieldBundleGet(srcFields, fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    allocate(fields(fieldCount))
    call ESMF_FieldBundleGet(srcFields, fieldList=fields, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldGet(fields(1), array=srcArray, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    deallocate(fields)

    if (outstanding) then
      call ESMF_ArrayRedist(srcArray, dstArray, routehandle=rh, &
        routesyncflag=ESMF_ROUTESYNC_NBWAITFINISH, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      outstanding = .false.
    endif

    ! execute the RouteHandle for srcArray->dstArray
    call ESMF_ArrayRedist(srcArray, dstArray, routehandle=rh, &
      routesyncflag=ESMF_ROUTESYNC_NBSTART, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    outstanding = .true.

    if (localPet==petCount-1) then
      call ESMF_ArrayRedist(srcArray, dstArray, routehandle=rh, &
        routesyncflag=ESMF_ROUTESYNC_NBWAITFINISH, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      outstanding = .false.
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ReleaseRH(connector, rc)
    type(ESMF_CplComp)  :: connector
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_FieldBundle)        :: dstFields, srcFields
    type(ESMF_Field), allocatable :: fields(:)
    integer                       :: fieldCount, i
    type(ESMF_Field)              :: field
    type(ESMF_RouteHandle)        :: rh
    type(ESMF_Array)              :: dstArray, srcArray

    rc = ESMF_SUCCESS

    ! Use this specialization point to flush the SRC side if there is an
    ! outstanding RH (as it is expected here)

    call NUOPC_ConnectorGet(connector, srcFields=srcFields, &
      dstFields=dstFields, rh=rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! for now assume that there is a single Field on both the src and dst side

    ! Pull out first Array from FieldBundles and execute RH for ArrayRedist
    call ESMF_FieldBundleGet(dstFields, fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    allocate(fields(fieldCount))
    call ESMF_FieldBundleGet(dstFields, fieldList=fields, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldGet(fields(1), array=dstArray, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    deallocate(fields)
    call ESMF_FieldBundleGet(srcFields, fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    allocate(fields(fieldCount))
    call ESMF_FieldBundleGet(srcFields, fieldList=fields, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldGet(fields(1), array=srcArray, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    deallocate(fields)

    if (outstanding) then
      call ESMF_ArrayRedist(srcArray, dstArray, routehandle=rh, &
        routesyncflag=ESMF_ROUTESYNC_NBWAITFINISH, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      outstanding = .false.
    endif

  end subroutine

  !-----------------------------------------------------------------------------

end module

