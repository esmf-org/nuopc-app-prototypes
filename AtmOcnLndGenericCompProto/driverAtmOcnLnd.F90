module DriverAtmOcnLnd

  !-----------------------------------------------------------------------------
  ! Generic Driver Component for ATM, OCN, LND with default explicit time stepping
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Driver, only: &
    Driver_routine_SS             => routine_SetServices, &
    Driver_type_IS                => type_InternalState, &
    Driver_label_IS               => label_InternalState, &
    Driver_label_SetModelCount    => label_SetModelCount, &
    Driver_label_SetModelPetLists => label_SetModelPetLists, &
    Driver_label_SetModelServices => label_SetModelServices, &
    Driver_label_Finalize         => label_Finalize

  implicit none
  
  private
  
  public routine_SetServices
  public type_InternalState, type_InternalStateStruct
  public label_InternalState, label_SetModelPetLists
  public label_SetModelServices, label_Finalize
  
  character(*), parameter :: &
    label_InternalState = "DriverAtmOcnLnd_InternalState"
  character(*), parameter :: &
    label_SetModelPetLists = "DriverAtmOcnLnd_SetModelPetLists"
  character(*), parameter :: &
    label_SetModelServices = "DriverAtmOcnLnd_SetModelServices"
  character(*), parameter :: &
    label_Finalize = "DriverAtmOcnLnd_Finalize"
  
  type type_InternalStateStruct
    integer, pointer    :: atmPetList(:)
    integer, pointer    :: ocnPetList(:)
    integer, pointer    :: lndPetList(:)
    type(ESMF_GridComp) :: atm
    type(ESMF_GridComp) :: ocn
    type(ESMF_GridComp) :: lnd
    type(ESMF_State)    :: atmIS, atmES
    type(ESMF_State)    :: ocnIS, ocnES
    type(ESMF_State)    :: lndIS, lndES
    integer, pointer    :: atm2ocnPetList(:)
    integer, pointer    :: atm2lndPetList(:)
    integer, pointer    :: ocn2atmPetList(:)
    integer, pointer    :: lnd2atmPetList(:)
    type(ESMF_CplComp)  :: atm2ocn, ocn2atm
    type(ESMF_CplComp)  :: atm2lnd, lnd2atm
    type(NUOPC_RunSequence), pointer  :: runSeq(:)
  end type

  type type_InternalState
    type(type_InternalStateStruct), pointer :: wrap
  end type

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine routine_SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    ! NUOPC_Driver registers the generic methods
    call Driver_routine_SS(gcomp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! attach specializing method(s)
    call ESMF_MethodAdd(gcomp, label=Driver_label_SetModelCount, &
      userRoutine=SetModelCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_MethodAdd(gcomp, label=Driver_label_SetModelPetLists, &
      userRoutine=SetModelPetLists, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_MethodAdd(gcomp, label=Driver_label_SetModelServices, &
      userRoutine=SetModelServices, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_MethodAdd(gcomp, label=Driver_label_Finalize, &
      userRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------
  
  subroutine SetModelCount(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(Driver_type_IS)  :: superIS

    rc = ESMF_SUCCESS
    
    ! query Component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(gcomp, Driver_label_IS, superIS, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! set the modelCount for ATM-OCN-LND coupling
    superIS%wrap%modelCount = 3
    
  end subroutine
  
  !-----------------------------------------------------------------------------
  
  subroutine SetModelPetLists(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    integer                   :: localrc, stat
    type(type_InternalState)  :: is
    type(Driver_type_IS)      :: superIS
    logical                   :: existflag

    rc = ESMF_SUCCESS
    
    ! allocate memory for this internal state and set it in the Component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state memory failed.", &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out
    call ESMF_UserCompSetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! nullify the petLists
    nullify(is%wrap%atmPetList)
    nullify(is%wrap%ocnPetList)
    nullify(is%wrap%lndPetList)
    nullify(is%wrap%atm2ocnPetList)
    nullify(is%wrap%ocn2atmPetList)
    nullify(is%wrap%atm2lndPetList)
    nullify(is%wrap%lnd2atmPetList)
    
    ! SPECIALIZE by calling into optional attached method to set modelPetLists
    call ESMF_MethodExecute(gcomp, label=label_SetModelPetLists, &
      existflag=existflag, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return  ! bail out

    if (existflag) then
      ! query Component for super internal State
      nullify(superIS%wrap)
      call ESMF_UserCompGetInternalState(gcomp, Driver_label_IS, superIS, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      
      ! set the petLists
      superIS%wrap%modelPetLists(1)%petList => is%wrap%atmPetList
      superIS%wrap%modelPetLists(2)%petList => is%wrap%ocnPetList
      superIS%wrap%modelPetLists(3)%petList => is%wrap%lndPetList
      superIS%wrap%connectorPetLists(1,2)%petList => is%wrap%atm2ocnPetList
      superIS%wrap%connectorPetLists(2,1)%petList => is%wrap%ocn2atmPetList
      superIS%wrap%connectorPetLists(1,3)%petList => is%wrap%atm2lndPetList
      superIS%wrap%connectorPetLists(3,1)%petList => is%wrap%lnd2atmPetList
    endif
    
  end subroutine
  
  !-----------------------------------------------------------------------------
  
  subroutine SetModelServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    integer                   :: localrc, stat
    type(Driver_type_IS)      :: superIS
    type(type_InternalState)  :: is

    rc = ESMF_SUCCESS
    
    ! query Component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(gcomp, Driver_label_IS, superIS, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! allocate memory for this internal state and set it in the Component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state memory failed.", &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out
    call ESMF_UserCompSetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! map components and states for ATM-OCN pair coupling
    is%wrap%atm = superIS%wrap%modelComp(1)
    is%wrap%atmIS = superIS%wrap%modelIS(1)
    is%wrap%atmES = superIS%wrap%modelES(1)
    is%wrap%ocn = superIS%wrap%modelComp(2)
    is%wrap%ocnIS = superIS%wrap%modelIS(2)
    is%wrap%ocnES = superIS%wrap%modelES(2)
    is%wrap%lnd = superIS%wrap%modelComp(3)
    is%wrap%lndIS = superIS%wrap%modelIS(3)
    is%wrap%lndES = superIS%wrap%modelES(3)
    is%wrap%atm2ocn = superIS%wrap%connectorComp(1,2)
    is%wrap%ocn2atm = superIS%wrap%connectorComp(2,1)
    is%wrap%atm2lnd = superIS%wrap%connectorComp(1,3)
    is%wrap%lnd2atm = superIS%wrap%connectorComp(3,1)
    
    ! maybe too much? but maybe nice to have the component names specified?
    call ESMF_GridCompSet(is%wrap%atm, name="ATM", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridCompSet(is%wrap%ocn, name="OCN", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridCompSet(is%wrap%lnd, name="LND", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_CplCompSet(is%wrap%atm2ocn, name="ATM2OCN", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_CplCompSet(is%wrap%ocn2atm, name="OCN2ATM", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_CplCompSet(is%wrap%atm2lnd, name="ATM2LND", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_CplCompSet(is%wrap%lnd2atm, name="LND2ATM", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! The default run sequence defined by the generic Driver Component is not 
    ! suitable for the ATM-OCN-LND case. The default RunSeq must be overwritten.
    call NUOPC_RunSequenceDeallocate(superIS%wrap%runSeq, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    ! add a single run sequence elements
    call NUOPC_RunSequenceAdd(superIS%wrap%runSeq, 1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out    
    ! atm2ocn in runSeq(1)
    call NUOPC_RunElementAdd(superIS%wrap%runSeq(1), i=1, j=2, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    ! ocn2atm in runSeq(1)
    call NUOPC_RunElementAdd(superIS%wrap%runSeq(1), i=2, j=1, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    ! atm2lnd in runSeq(1)
    call NUOPC_RunElementAdd(superIS%wrap%runSeq(1), i=1, j=3, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    ! lnd2atm in runSeq(1)
    call NUOPC_RunElementAdd(superIS%wrap%runSeq(1), i=3, j=1, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    ! atm in runSeq(1)
    call NUOPC_RunElementAdd(superIS%wrap%runSeq(1), i=1, j=-1, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    ! ocn in runSeq(1)
    call NUOPC_RunElementAdd(superIS%wrap%runSeq(1), i=2, j=-1, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    ! lnd in runSeq(1)
    call NUOPC_RunElementAdd(superIS%wrap%runSeq(1), i=3, j=-1, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    
    ! nullify the runSeq
    nullify(is%wrap%runSeq)
    
    ! SPECIALIZE by calling into attached method to SetModelServices
    call ESMF_MethodExecute(gcomp, label=label_SetModelServices, &
      userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out
      
    ! optionally overwrite the default run sequence
    if (associated(is%wrap%runSeq)) then
      call NUOPC_RunSequenceDeallocate(superIS%wrap%runSeq, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      superIS%wrap%runSeq => is%wrap%runSeq
    endif
      
  end subroutine
    
  !-----------------------------------------------------------------------------
  
  subroutine Finalize(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    integer                   :: localrc, stat
    type(type_InternalState)  :: is
    logical                   :: existflag

    rc = ESMF_SUCCESS
    
    ! SPECIALIZE by calling into optional attached method
    call ESMF_MethodExecute(gcomp, label=label_Finalize, existflag=existflag, &
      userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out

    ! query Component for this internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! deallocate internal state memory
    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out
      
  end subroutine
      
end module
