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
      phaseLabelList=(/"IPDv05p4"/), userRoutine=IInitRealize, &
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
    type(ESMF_CplComp)            :: connector

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
#if 0
    ! importable field: sea_surface_temperature
    call NUOPC_Advertise(importState, &
      StandardName="sea_surface_temperature", name="sea_surface_temperature", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#else
    call NUOPC_SetAttribute(importState, name="FieldTransferPolicy", &
      value="transferAll", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
#endif
    
#define WITHEXPORTFIELDS
#ifdef WITHEXPORTFIELDS
#if 0
    ! exportable field: air_pressure_at_sea_level
    call NUOPC_Advertise(exportState, &
      StandardName="air_pressure_at_sea_level", name="pmsl", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! exportable field: surface_net_downward_shortwave_flux
    call NUOPC_Advertise(exportState, &
      StandardName="surface_net_downward_shortwave_flux", name="rsns", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#else
    call NUOPC_SetAttribute(exportState, name="FieldTransferPolicy", &
      value="transferAll", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
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

  subroutine IInitRealize(driver, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: driver
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables    
    type(ESMF_Field)        :: field
    type(ESMF_Grid)         :: gridIn
    type(ESMF_Grid)         :: gridOut
    character(len=80)       :: itemName
    logical                 :: connected, producerConnected, consumerConnected
    
    rc = ESMF_SUCCESS
    
    ! create a Grid object for Fields
    gridIn = ESMF_GridCreateNoPeriDimUfrm(maxIndex=(/200, 200/), &
      minCornerCoord=(/-20._ESMF_KIND_R8, -90._ESMF_KIND_R8/), &
      maxCornerCoord=(/400._ESMF_KIND_R8, 90._ESMF_KIND_R8/), &
      coordSys=ESMF_COORDSYS_CART, staggerLocList=(/ESMF_STAGGERLOC_CENTER/), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    gridOut = gridIn ! for now out same as in

#ifdef WITHIMPORTFIELDS
    itemName="sea_surface_temperature"
    call ESMF_StateGet(importState, field=field, itemName=itemName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call checkConnections(field, connected, producerConnected, &
      consumerConnected, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (connected) then
      if (.not.producerConnected) then
        ! a connected field in a Driver state must have a ProducerConnection
        call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
          msg="Connected Field in Driver State must have ProducerConnection:"//&
          trim(itemName), &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return ! bail out
      endif
      ! realize the field
      field = ESMF_FieldCreate(name=itemName, grid=gridIn, &
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
    else
      ! remove the field from the state
      call ESMF_StateRemove(importState, itemName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
#endif

#ifdef WITHEXPORTFIELDS
    itemName="air_pressure_at_sea_level"
    call ESMF_StateGet(exportState, field=field, itemName=itemName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call checkConnections(field, connected, producerConnected, &
      consumerConnected, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (connected) then
      if (.not.producerConnected) then
        ! a connected field in a Driver state must have a ProducerConnection
        call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
          msg="Connected Field in Driver State must have ProducerConnection:"//&
          trim(itemName), &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return ! bail out
      endif
      ! realize the field
      field = ESMF_FieldCreate(name=itemName, grid=gridOut, &
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
    else
      ! remove the field from the state
      call ESMF_StateRemove(exportState, itemName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    itemName="surface_net_downward_shortwave_flux"
    call ESMF_StateGet(exportState, field=field, itemName=itemName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call checkConnections(field, connected, producerConnected, &
      consumerConnected, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (connected) then
      if (.not.producerConnected) then
        ! a connected field in a Driver state must have a ProducerConnection
        call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
          msg="Connected Field in Driver State must have ProducerConnection:"//&
          trim(itemName), &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return ! bail out
      endif
      ! realize the field
      field = ESMF_FieldCreate(name=itemName, grid=gridOut, &
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
    else
      ! remove the field from the state
      call ESMF_StateRemove(exportState, itemName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
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
  
end module
