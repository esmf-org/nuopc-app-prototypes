!==============================================================================
! Earth System Modeling Framework
! Copyright (c) 2002-2025, University Corporation for Atmospheric Research,
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

    call NUOPC_CompSpecialize(model, specLabel=label_RealizeAccepted, &
      specRoutine=RealizeAccepted, rc=rc)
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
    
    call NUOPC_SetAttribute(importState, "FieldTransferPolicy", &
      "transferAllWithNamespace", rc=rc)
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
    integer                 :: i, j
    type(ESMF_State)        :: importState, exportState
    integer                 :: importItemCount, importNestedItemCount
    type(ESMF_State)        :: importNestedState
    character(ESMF_MAXSTR)  :: nestedStateName
    character(ESMF_MAXSTR), allocatable     :: importItemNameList(:)
    type(ESMF_StateItem_Flag), allocatable  :: importItemTypeList(:)
    character(ESMF_MAXSTR), allocatable     :: importNestedItemNameList(:)
    type(ESMF_StateItem_Flag), allocatable  :: importNestedItemTypeList(:)
    
    rc = ESMF_SUCCESS
    
    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! query nested States
    call ESMF_StateGet(importState, itemCount=importItemCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! allocate temporary data structures
    allocate(importItemNameList(importItemCount))
    allocate(importItemTypeList(importItemCount))

    ! query importState
    call ESMF_StateGet(importState, nestedFlag=.false., &
      itemNameList=importItemNameList, itemTypeList=importItemTypeList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! loop over items in importState
    do i = 1, importItemCount
       ! query item
       if (importItemTypeList(i) == ESMF_STATEITEM_STATE) then
          ! pull out nested state
          call ESMF_StateGet(importState, itemName=importItemNameList(i), &
            nestedState=importNestedState, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

          ! query nested state
          call ESMF_StateGet(importNestedState, name=nestedStateName, &
            itemCount=importNestedItemCount, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

          ! allocate temporary data structures
          allocate(importNestedItemNameList(importNestedItemCount))
          allocate(importNestedItemTypeList(importNestedItemCount))

          ! query item name and types in the nested state
          call ESMF_StateGet(importNestedState, &
            itemNameList=importNestedItemNameList, &
            itemTypeList=importNestedItemTypeList, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

          ! loop over items in importNestedState
          do j = 1, importNestedItemCount
             call ESMF_LogWrite('realize '//&
              trim(importNestedItemNameList(j))//&
              ' import field received from '//trim(nestedStateName), &
              ESMF_LOGMSG_INFO)

             call NUOPC_Realize(importNestedState, &
              fieldName=trim(importNestedItemNameList(j)), rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, &
               file=__FILE__)) &
               return  ! bail out
          end do

          ! clean memory
          deallocate(importNestedItemNameList)
          deallocate(importNestedItemTypeList)
       end if
    end do

    ! clean memory
    deallocate(importItemNameList)
    deallocate(importItemTypeList)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Advance(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(ESMF_State)            :: importNestedState
    character(ESMF_MAXSTR)      :: namespace
    character(ESMF_MAXSTR), allocatable    :: importItemNameList(:)
    type(ESMF_StateItem_Flag), allocatable :: importItemTypeList(:)
    integer                     :: i, importItemCount
    integer, save               :: slice=1
    character(len=160)          :: msgString

    rc = ESMF_SUCCESS

    ! query for clock, importState and exportState
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

    ! query nested States
    call ESMF_StateGet(importState, nestedFlag=.false., &
      itemCount=importItemCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! query number of item in importState
    call ESMF_StateGet(importState, itemCount=importItemCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! allocate temporary data structures
    allocate(importItemNameList(importItemCount))
    allocate(importItemTypeList(importItemCount))

    ! query importState
    call ESMF_StateGet(importState, nestedFlag=.false., &
      itemNameList=importItemNameList, itemTypeList=importItemTypeList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! loop over items in importState
    do i = 1, importItemCount
      ! query item
      if (importItemTypeList(i) == ESMF_STATEITEM_STATE) then
        ! pull out nested state
        call ESMF_StateGet(importState, &
          itemName=importItemNameList(i), nestedState=importNestedState, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call NUOPC_GetAttribute(importNestedState, name="Namespace", &
          value=namespace, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        ! convert state name to lower case
        namespace = ESMF_UtilStringLowerCase(namespace)

        ! write out the Fields in the importState and exportState
        call NUOPC_Write(importNestedState, &
          fileNamePrefix='field_atm_import_namespace:'//trim(namespace)//'_', &
          timeslice=slice, overwrite=.true., relaxedFlag=.true., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      end if
    end do
    slice = slice+1

  end subroutine

  !-----------------------------------------------------------------------------

end module
