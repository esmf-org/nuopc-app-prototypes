module CON

  !-----------------------------------------------------------------------------
  ! Connector Component.
  !-----------------------------------------------------------------------------
  

  use ESMF
  use NUOPC
  use NUOPC_Connector, only: &
    con_routine_SS      => SetServices, &
    con_label_ComputeRH => label_ComputeRouteHandle, &
    con_label_ExecuteRH => label_ExecuteRouteHandle, &
    con_label_ReleaseRH => label_ReleaseRouteHandle, &
    NUOPC_ConnectorGet, NUOPC_ConnectorSet
  
  implicit none
  
  private
  
  public SetServices
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine SetServices(connector, rc)
    type(ESMF_CplComp)  :: connector
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    ! the NUOPC connector component will register the generic methods
    call NUOPC_CompDerive(connector, con_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! attach specializing method to compute the connection RouteHandle
    call NUOPC_CompSpecialize(connector, specLabel=con_label_ComputeRH, &
      specRoutine=ComputeRH, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(connector, specLabel=con_label_ExecuteRH, &
      specRoutine=ExecuteRH, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(connector, specLabel=con_label_ReleaseRH, &
      specRoutine=ReleaseRH, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! add "custom" run phase
    call NUOPC_CompSetEntryPoint(connector, ESMF_METHOD_RUN, &
      phaseLabelList=(/"custom"/), userRoutine=RunCustom, rc=rc)
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
    integer                       :: localrc
    type(ESMF_State)              :: state
    type(ESMF_FieldBundle)        :: dstFields, srcFields

    rc = ESMF_SUCCESS
    
    call NUOPC_ConnectorGet(connector, srcFields=srcFields, &
      dstFields=dstFields, state=state, rc=rc)
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
    integer                       :: localrc
    type(ESMF_State)              :: state
    type(ESMF_FieldBundle)        :: dstFields, srcFields

    rc = ESMF_SUCCESS
    
    call NUOPC_ConnectorGet(connector, srcFields=srcFields, &
      dstFields=dstFields, state=state, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
 
 
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine ReleaseRH(connector, rc)
    type(ESMF_CplComp)  :: connector
    integer, intent(out) :: rc
    
    ! local variables
    integer                       :: localrc
    type(ESMF_State)              :: state
    type(ESMF_FieldBundle)        :: dstFields, srcFields

    rc = ESMF_SUCCESS
    
    call NUOPC_ConnectorGet(connector, srcFields=srcFields, &
      dstFields=dstFields, state=state, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
 
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine RunCustom(connector, importState, exportState, clock, rc)
    type(ESMF_CplComp)    :: connector
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    rc = ESMF_SUCCESS
    
    call ESMF_LogWrite("From inside RunCustom", ESMF_LOGMSG_INFO)

  end subroutine
  
  !-----------------------------------------------------------------------------

end module

