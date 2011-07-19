module CON

  !-----------------------------------------------------------------------------
  ! Connector Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Connector, only: &
    con_routine_SS      => routine_SetServices, &
    con_type_IS         => type_InternalState, &
    con_label_IS        => label_InternalState, &
    con_label_ComputeRH => label_ComputeRouteHandle
  
  implicit none
  
  private
  
  public SetServices
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine SetServices(cplcomp, rc)
    type(ESMF_CplComp)  :: cplcomp
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    ! the NUOPC connector component will register the generic methods
    call con_routine_SS(cplcomp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! attach specializing method to compute the connection RouteHandle
    call ESMF_MethodAdd(cplcomp, label=con_label_ComputeRH, &
      userRoutine=ComputeRH, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine ComputeRH(cplcomp, rc)
    type(ESMF_CplComp)  :: cplcomp
    integer, intent(out) :: rc
    
    ! local variables
    integer                       :: localrc
    type(con_type_IS)             :: is

    rc = ESMF_SUCCESS
    
    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(cplcomp, con_label_IS, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! specialize with Redist, instead of the default Regrid
    call ESMF_FieldBundleRedistStore(is%wrap%srcFields, is%wrap%dstFields, &
      routehandle=is%wrap%rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
  end subroutine
  
  
end module

