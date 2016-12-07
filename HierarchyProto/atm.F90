module ATM

  !-----------------------------------------------------------------------------
  ! Code that specializes generic NUOPC_Driver to be ATM model component
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Driver, &
    driver_routine_SS             => SetServices, &
    driver_label_SetModelServices => label_SetModelServices
  
  use DYN, only: dynSS => SetServices
  use PHY, only: phySS => SetServices

  use NUOPC_Connector, only: cplSS => SetServices

  implicit none
  
  private
  
  public SetServices
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    ! NUOPC_Driver registers the generic methods
    call NUOPC_CompDerive(driver, driver_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! attach specializing method(s)
    call NUOPC_CompSpecialize(driver, specLabel=driver_label_SetModelServices, &
      specRoutine=SetModelServices, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! set entry points that driver uses internally to interact with models
    call NUOPC_CompSetInternalEntryPoint(driver, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv05p1"/), userRoutine=IInitAdvertize, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetInternalEntryPoint(driver, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv05p2"/), userRoutine=IInitAdvertizeFinish, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetInternalEntryPoint(driver, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv05p4"/), userRoutine=IInitCheck, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetInternalEntryPoint(driver, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv05p6"/), userRoutine=IInitRealize, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc
    
    ! local variables
    integer                       :: localrc
    type(ESMF_GridComp)           :: child
    type(ESMF_CplComp)            :: conn

    rc = ESMF_SUCCESS
    
    ! SetServices for DYN
    call NUOPC_DriverAddComp(driver, "DYN", dynSS, comp=child, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! SetServices for PHY
    call NUOPC_DriverAddComp(driver, "PHY", phySS, comp=child, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! SetServices for PHY2DYN
    call NUOPC_DriverAddComp(driver, srcCompLabel="PHY", dstCompLabel="DYN", &
      compSetServicesRoutine=cplSS, comp=conn, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(conn, name="Verbosity", value="high", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! The ATM subcomponents use fields with new standardNames
    call NUOPC_FieldDictionaryAddEntry("PHYEX", canonicalUnits="1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine IInitAdvertize(driver, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: driver
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    ! Disabling the following macro, e.g. renaming to WITHIMPORTFIELDS_disable,
    ! will result in a model component that does not advertise any importable
    ! Fields. Use this if you want to drive the model independently.
#define WITHIMPORTFIELDS
#ifdef WITHIMPORTFIELDS
    call NUOPC_SetAttribute(importState, name="FieldTransferPolicy", &
      value="transferAll", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
    
#define WITHEXPORTFIELDS
#ifdef WITHEXPORTFIELDS
    call NUOPC_SetAttribute(exportState, name="FieldTransferPolicy", &
      value="transferAll", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine IInitAdvertizeFinish(driver, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: driver
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS

    ! must reset the FieldTransferPolicy here in order to prevent
    ! interaction of this state with uppler hierarchy layer
    call NUOPC_SetAttribute(importState, name="FieldTransferPolicy", &
      value="transferNone", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_SetAttribute(exportState, name="FieldTransferPolicy", &
      value="transferNone", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine IInitCheck(driver, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: driver
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables    
    integer                         :: i
    type(ESMF_Field), pointer       :: fieldList(:)
    character(ESMF_MAXSTR), pointer :: itemNameList(:)
    logical                         :: connected
    logical                         :: producerConnected, consumerConnected
    
    rc = ESMF_SUCCESS
    
#ifdef WITHIMPORTFIELDS
    nullify(fieldList)
    nullify(itemNameList)
    call NUOPC_GetStateMemberLists(importState, itemNameList=itemNameList, &
      fieldList=fieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (associated(fieldList)) then
      do i=1, size(fieldList)
        call checkConnections(fieldList(i), connected, producerConnected, &
          consumerConnected, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        if (connected .and. .not.producerConnected) then
          ! a connected field in a Driver state must have a ProducerConnection
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg="Connected Field in Driver State must have ProducerConnection:"//&
            trim(itemNameList(i)), &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)
          return ! bail out
        endif
      enddo
    endif
    if (associated(fieldList)) deallocate(fieldList)
    if (associated(itemNameList)) deallocate(itemNameList)
#endif

#ifdef WITHEXPORTFIELDS
    nullify(fieldList)
    nullify(itemNameList)
    call NUOPC_GetStateMemberLists(exportState, itemNameList=itemNameList, &
      fieldList=fieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (associated(fieldList)) then
      do i=1, size(fieldList)
        call checkConnections(fieldList(i), connected, producerConnected, &
          consumerConnected, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        if (connected .and. .not.producerConnected) then
          ! a connected field in a Driver state must have a ProducerConnection
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg="Connected Field in Driver State must have ProducerConnection:"//&
            trim(itemNameList(i)), &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)
          return ! bail out
        endif
      enddo
    endif
    if (associated(fieldList)) deallocate(fieldList)
    if (associated(itemNameList)) deallocate(itemNameList)
#endif

  contains
    subroutine checkConnections(field, connected, producerConnected, &
      consumerConnected, rc)
      type(ESMF_Field), intent(in)  :: field
      logical, intent(out)          :: connected
      logical, intent(out)          :: producerConnected
      logical, intent(out)          :: consumerConnected
      integer, intent(out)          :: rc
      ! local variables
      character(len=80)             :: value
      rc = ESMF_SUCCESS
      call NUOPC_GetAttribute(field, name="Connected", &
        value=value, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      connected = (value=="true")
#if 1
      call ESMF_LogWrite("Attribute - ??? - Connected: "//trim(value), &
        ESMF_LOGMSG_INFO)
#endif
      call NUOPC_GetAttribute(field, name="ProducerConnection", &
        value=value, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      producerConnected = (value/="open")
#if 1
      call ESMF_LogWrite("Attribute - ??? - ProducerConnection: "//trim(value), &
        ESMF_LOGMSG_INFO)
#endif
      call NUOPC_GetAttribute(field, name="ConsumerConnection", &
        value=value, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      consumerConnected = (value/="open")
#if 1
      call ESMF_LogWrite("Attribute - ??? - ConsumerConnection: "//trim(value), &
        ESMF_LOGMSG_INFO)
#endif
    end subroutine
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine IInitRealize(driver, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: driver
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables    
    integer                         :: i
    type(ESMF_Field), pointer       :: fieldList(:)
    character(ESMF_MAXSTR), pointer :: itemNameList(:)
    
    rc = ESMF_SUCCESS
    
#ifdef WITHIMPORTFIELDS
    nullify(fieldList)
    nullify(itemNameList)
    call NUOPC_GetStateMemberLists(importState, itemNameList=itemNameList, &
      fieldList=fieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (associated(fieldList)) then
      do i=1, size(fieldList)
        ! the transferred Grid is already set, allocate memory for data by complete
        call ESMF_FieldEmptyComplete(fieldList(i), &
          typekind=ESMF_TYPEKIND_R8, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      enddo
    endif
    if (associated(fieldList)) deallocate(fieldList)
    if (associated(itemNameList)) deallocate(itemNameList)
#endif

#ifdef WITHEXPORTFIELDS
    nullify(fieldList)
    nullify(itemNameList)
    call NUOPC_GetStateMemberLists(exportState, itemNameList=itemNameList, &
      fieldList=fieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (associated(fieldList)) then
      do i=1, size(fieldList)
        ! the transferred Grid is already set, allocate memory for data by complete
        call ESMF_FieldEmptyComplete(fieldList(i), &
          typekind=ESMF_TYPEKIND_R8, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      enddo
    endif
    if (associated(fieldList)) deallocate(fieldList)
    if (associated(itemNameList)) deallocate(itemNameList)
#endif

  end subroutine
  
  !-----------------------------------------------------------------------------
  
end module
