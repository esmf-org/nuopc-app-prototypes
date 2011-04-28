module MED

  !-----------------------------------------------------------------------------
  ! Mediator Component.
  !-----------------------------------------------------------------------------

  use ESMF_Mod
  use NUOPC
  use NUOPC_MediatorExplicit, only: &
    model_routine_SS    => routine_SetServices, &
    model_label_Advance => label_Advance
  
  implicit none
  
  private
  
  public SetServices
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    ! the NUOPC model component will register the generic methods
    call model_routine_SS(gcomp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! set entry point for methods that require specific implementation
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETINIT, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETINIT, &
      userRoutine=InitializeP1, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! attach specializing method(s)
    call ESMF_MethodAdd(gcomp, label=model_label_Advance, &
      userRoutine=MediatorAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    ! importable field: sea_surface_temperature
    call NUOPC_StateAddPotentialField(importState, name="sst", &
      StandardName="sea_surface_temperature", Units="K", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! importable field: air_pressure_at_sea_level
    call NUOPC_StateAddPotentialField(importState, name="pmsl", &
      StandardName="air_pressure_at_sea_level", Units="Pa", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! importable field: isotropic_shortwave_radiance_in_air
    call NUOPC_StateAddPotentialField(importState, name="risw", &
      StandardName="isotropic_shortwave_radiance_in_air", Units="W m-2 sr-1", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: sea_surface_temperature
    call NUOPC_StateAddPotentialField(exportState, name="sst", &
      StandardName="sea_surface_temperature", Units="K", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: air_pressure_at_sea_level
    call NUOPC_StateAddPotentialField(exportState, name="pmsl", &
      StandardName="air_pressure_at_sea_level", Units="Pa", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! exportable field: isotropic_shortwave_radiance_in_air
    call NUOPC_StateAddPotentialField(exportState, name="risw", &
      StandardName="isotropic_shortwave_radiance_in_air", Units="W m-2 sr-1", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP1(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables    
    type(ESMF_Field)        :: field
    type(ESMF_Grid)         :: gridIn
    type(ESMF_Grid)         :: gridOut
    
    rc = ESMF_SUCCESS
    
    ! create a Grid object for Fields
    gridIn = NUOPC_GridCreateSimpleXY(10._ESMF_KIND_R8, 20._ESMF_KIND_R8, &
      100._ESMF_KIND_R8, 200._ESMF_KIND_R8, 20, 200, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    gridOut = gridIn ! for now out same as in

    ! importable field: sea_surface_temperature
    field = ESMF_FieldCreate(name="sst", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_StateReplaceWRealField(importState, field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: air_pressure_at_sea_level
    field = ESMF_FieldCreate(name="pmsl", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_StateReplaceWRealField(importState, field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! importable field: isotropic_shortwave_radiance_in_air
    field = ESMF_FieldCreate(name="risw", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_StateReplaceWRealField(importState, field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: sea_surface_temperature
    field = ESMF_FieldCreate(name="sst", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_StateReplaceWRealField(exportState, field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: air_pressure_at_sea_level
    field = ESMF_FieldCreate(name="pmsl", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_StateReplaceWRealField(exportState, field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: isotropic_shortwave_radiance_in_air
    field = ESMF_FieldCreate(name="risw", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_StateReplaceWRealField(exportState, field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine MediatorAdvance(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_State)              :: importState, exportState

    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! HERE THE MEDIATOR does the mediation of Fields that come in on the
    ! importState with a timestamp consistent to the currTime of the 
    ! mediators Clock.
    
    ! The Mediator uses the data on the import Fields to update the data
    ! held by Fields in the exportState.
    
    ! After this routine returns the generic MediatorExplicit will correctly
    ! timestamp the export Fields and update the Mediator Clock to:
    !
    !       currTime -> currTime + timeStep
    !
    ! Where the timeStep is equal to the parent timeStep.
    
    call NUOPC_ClockPrintCurrTime(clock, &
      "-------->MED Advance() mediating for: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call NUOPC_ClockPrintStopTime(clock, &
      "----------------> model time step to: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
     
  end subroutine

end module
