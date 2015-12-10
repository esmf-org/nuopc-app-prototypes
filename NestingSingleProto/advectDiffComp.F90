module advectDiffComp

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    model_routine_SS      => SetServices, &
    model_label_SetClock  => label_SetClock, &
    model_label_Advance   => label_Advance
  
  implicit none
  
  private

  ! private module data --> ONLY PARAMETERS
  integer, parameter            :: iCount = 100
  integer, parameter            :: jCount = 20
  integer, parameter            :: iHaloDepth = 1
  integer, parameter            :: jHaloDepth = 0

  real(ESMF_KIND_R8), parameter :: dtTop  = 30.D0   ! simulation time step [s]
                                                    ! on the parent level!

!  real(ESMF_KIND_R8), parameter :: dtTop  = 300.D0   ! simulation time step [s]
                                                    ! on the parent level!

  real(ESMF_KIND_R8), parameter :: childSteps = 3.D0 ! child steps per parent step

  real(ESMF_KIND_R8), parameter :: N_0    = 1.D12   ! start density [molec/cm3]
  real(ESMF_KIND_R8), parameter :: dxTop  = 5.D3    ! grid spacing [m]
  real(ESMF_KIND_R8), parameter :: u      = 5.D0    ! horiz. x velocity [m/s]
  real(ESMF_KIND_R8), parameter :: Kh     = 2500.D0 ! horiz. diff coef [m2/s]
  
!  real(ESMF_KIND_R8), parameter :: sbf    = 30.D0  ! seconds between frames [s]
  real(ESMF_KIND_R8), parameter :: sbf    = 120.D0  ! seconds between frames [s]
!  real(ESMF_KIND_R8), parameter :: sbf    = 300.D0  ! seconds between frames [s]
  
  ! internal state to keep instance private data
  type InternalStateStruct
    type(ESMF_Grid)               :: grid
    type(ESMF_Field)              :: field
    type(ESMF_RouteHandle)        :: haloHandle
    type(ESMF_Alarm)              :: timeToWriteAlarm
    type(ESMF_Grid)               :: gridTrans
    type(ESMF_Field)              :: fieldTrans
    type(ESMF_Alarm)              :: timeToTransfer
    integer                       :: slice
  end type

  type InternalState
    type(InternalStateStruct), pointer :: wrap
  end type

  ! single public entry point
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
    
    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p1"/), userRoutine=InitializeP1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p2"/), userRoutine=InitializeP2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_FINALIZE, &
      phaseLabelList=(/""/), userRoutine=Finalize, rc=rc)
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
    call NUOPC_CompSpecialize(model, specLabel=model_label_SetClock, &
      specRoutine=SetClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
  end subroutine

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------

  subroutine InitializeP1(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    integer :: nestingGeneration
    
    rc = ESMF_SUCCESS
    
    ! look at the "NestingGeneration" Attribute of this instance
    call NUOPC_CompAttributeGet(model, name="NestingGeneration", &
      value=nestingGeneration, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! exportable field: density
    call NUOPC_Advertise(exportState, &
      StandardName="density", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    if (nestingGeneration == 1) then
      ! Child Domain
      ! importable field: density
      call NUOPC_Advertise(importState, &
        StandardName="density", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    
  end subroutine
    
  !-----------------------------------------------------------------------------

  subroutine InitializeP2(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    type(InternalState)           :: is
    real(ESMF_KIND_R8), pointer   :: coordPtr(:)
    real(ESMF_KIND_R8), pointer   :: dataPtr(:,:)
    real(ESMF_KIND_R8)            :: dx
    integer                       :: i, j, stat
    integer                       :: nestingGeneration, nestling
    character(len=80)             :: name

    rc = ESMF_SUCCESS

    ! look at the "NestingGeneration" Attribute of this instance
    call NUOPC_CompAttributeGet(model, name="NestingGeneration", &
      value=nestingGeneration, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! look at the "Nestling" Attribute of this instance
    call NUOPC_CompAttributeGet(model, name="Nestling", &
      value=nestling, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! -> allocate memory for this internal state and set it in the Component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridCompSetInternalState(model, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (nestingGeneration == 0) then
      ! This is the parent domain
      
      dx = dxTop
      ! -> create the Grid
      is%wrap%grid = ESMF_GridCreate1PeriDim( &
        ! Define a regular distribution (no regDecomp arg -> divide along i w/ DEs)
        minIndex=(/1,1/), &           ! begin of index space
        maxIndex=(/iCount,jCount/), & ! end of index space
        ! Specify mapping of coords dim to Grid dim
        coordDep1=(/1/), & ! 1st coord is 1D and depends on 1st Grid dim
        coordDep2=(/2/), & ! 2nd coord is 1D and depends on 2nd Grid dim
        indexflag=ESMF_INDEX_GLOBAL, & ! use global indices to index into local data
        coordSys=ESMF_COORDSYS_CART, & ! use cartesian coordinates
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return

      ! -> add coordinates to the Grid
      call ESMF_GridAddCoord(is%wrap%grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return
      ! x-coordinates
      call ESMF_GridGetCoord(is%wrap%grid, coordDim=1, farrayPtr=coordPtr, rc=rc)
      do i=lbound(coordPtr,1),ubound(coordPtr,1)
        coordPtr(i) = i * dx
      enddo
      ! y-coordinates
      call ESMF_GridGetCoord(is%wrap%grid, coordDim=2, farrayPtr=coordPtr, rc=rc)
      do i=lbound(coordPtr,1),ubound(coordPtr,1)
        coordPtr(i) = i * dx
      enddo
    
    else if ((nestingGeneration == 1) .and. (nestling == 0)) then
      ! This is nestling 0 in the first nest generation
    
      dx = dxTop / 2.d0 ! twice the resolution in x
      ! -> create the Grid
      is%wrap%grid = ESMF_GridCreateNoPeriDim( &
        ! Define a regular distribution (no regDecomp arg -> divide along i w/ DEs)
        minIndex=(/1,1/), &           ! begin of index space
        maxIndex=(/iCount,jCount/), & ! end of index space
        ! Specify mapping of coords dim to Grid dim
        coordDep1=(/1/), & ! 1st coord is 1D and depends on 1st Grid dim
        coordDep2=(/2/), & ! 2nd coord is 1D and depends on 2nd Grid dim
        indexflag=ESMF_INDEX_GLOBAL, & ! use global indices to index into local data
        coordSys=ESMF_COORDSYS_CART, & ! use cartesian coordinates
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return

      ! -> add coordinates to the Grid
      call ESMF_GridAddCoord(is%wrap%grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return
      ! x-coordinates
      call ESMF_GridGetCoord(is%wrap%grid, coordDim=1, farrayPtr=coordPtr, rc=rc)
      do i=lbound(coordPtr,1),ubound(coordPtr,1)
        coordPtr(i) = 50 * dxTop + i * dx  ! shifted by half of parent domain
      enddo
      ! y-coordinates
      call ESMF_GridGetCoord(is%wrap%grid, coordDim=2, farrayPtr=coordPtr, rc=rc)
      do i=lbound(coordPtr,1),ubound(coordPtr,1)
        coordPtr(i) = i * dxTop
      enddo
      
      ! the child domain also needs a transfer Grid/Field for coupling to parent
      is%wrap%gridTrans = ESMF_GridCreateNoPeriDim( &
        ! Define a regular distribution (no regDecomp arg -> divide along i w/ DEs)
        minIndex=(/1-iHaloDepth,1-jHaloDepth/), &         ! begin of index space
        maxIndex=(/iCount+iHaloDepth,jCount+jHaloDepth/), & ! end of index space
        ! Specify mapping of coords dim to Grid dim
        coordDep1=(/1/), & ! 1st coord is 1D and depends on 1st Grid dim
        coordDep2=(/2/), & ! 2nd coord is 1D and depends on 2nd Grid dim
        indexflag=ESMF_INDEX_GLOBAL, & ! use global indices to index into local data
        coordSys=ESMF_COORDSYS_CART, & ! use cartesian coordinates
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return

      ! -> add coordinates to the transfer Grid
      call ESMF_GridAddCoord(is%wrap%gridTrans, &
        staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return
      ! x-coordinates
      call ESMF_GridGetCoord(is%wrap%gridTrans, coordDim=1, farrayPtr=coordPtr,&
        rc=rc)
      do i=lbound(coordPtr,1),ubound(coordPtr,1)
        coordPtr(i) = 50 * dxTop + i * dx  ! shifted by half of parent domain
      enddo
      ! y-coordinates
      call ESMF_GridGetCoord(is%wrap%gridTrans, coordDim=2, farrayPtr=coordPtr,&
        rc=rc)
      do i=lbound(coordPtr,1),ubound(coordPtr,1)
        coordPtr(i) = i * dxTop
      enddo
      
      ! -> create the transfer Field
      is%wrap%fieldTrans = ESMF_FieldCreate(grid=is%wrap%gridTrans, &
        typekind=ESMF_TYPEKIND_R8, &
        totalLWidth=(/iHaloDepth,jHaloDepth/), &
        totalUWidth=(/iHaloDepth,jHaloDepth/), &
        indexflag=ESMF_INDEX_GLOBAL, name="density", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return
    
    endif

    ! -> create the Field
    is%wrap%field = ESMF_FieldCreate(grid=is%wrap%grid, &
      typekind=ESMF_TYPEKIND_R8, &
      totalLWidth=(/iHaloDepth,jHaloDepth/), &
      totalUWidth=(/iHaloDepth,jHaloDepth/), &
      indexflag=ESMF_INDEX_GLOBAL, name="density", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return
    
    ! -> precompute halo operation
    call ESMF_FieldHaloStore(is%wrap%field, routehandle=is%wrap%haloHandle, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return
      
    ! exportable field: density
    call NUOPC_Realize(exportState, field=is%wrap%field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (nestingGeneration == 0) then
      ! -> get pointer to Field data
      call ESMF_FieldGet(is%wrap%field, farrayPtr=dataPtr, rc=rc)  
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return
      ! -> initialize Field data to initial conditions
      do j=lbound(dataPtr,2), ubound(dataPtr,2)
        do i=lbound(dataPtr,1), ubound(dataPtr,1)
          dataPtr(i,j) = N_0 * exp(-((i-50)**2)/8.)
        enddo
      enddo
    else
      ! importable field: density
      call NUOPC_Realize(importState, field=is%wrap%fieldTrans, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    
    ! intialize instance private slice counter
    is%wrap%slice = 1
    
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Finalize(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    type(InternalState)  :: is
    integer              :: stat

    rc = ESMF_SUCCESS
  
    ! -> get internal state from Component
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(model, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! -> destroy objects inside of internal state

    call ESMF_FieldHaloRelease(routehandle=is%wrap%haloHandle, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return
      
    call ESMF_FieldDestroy(is%wrap%field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return
    
    call ESMF_GridDestroy(is%wrap%grid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return
      
    ! -> deallocate internal state memory  
    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetClock(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_TimeInterval)       :: callerTimeStep
    type(ESMF_TimeInterval)       :: stabilityTimeStep, alarmInterval
    type(ESMF_Time)               :: currTime
    type(InternalState)           :: is
    integer                       :: nestingGeneration
    real(ESMF_KIND_R8)            :: dt

    rc = ESMF_SUCCESS
    
    ! look at the "NestingGeneration" Attribute of this instance of the model component
    call NUOPC_CompAttributeGet(model, name="NestingGeneration", &
      value=nestingGeneration, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    if (nestingGeneration == 0) then
      dt = dtTop        ! parent domain time step
    else
      dt = dtTop / childSteps ! child domain time step
    endif
    
    ! query the Component for its clock
    call NUOPC_ModelGet(model, modelClock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! -> get the caller's timeStep
    call ESMF_ClockGet(clock, timeStep=callerTimeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! initialize internal Clock
    ! here: parent Clock and stability timeStep determine actual model timeStep
    call ESMF_TimeIntervalSet(stabilityTimeStep, s_r8=dt, rc=rc) ! sim. timeStep
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetClock(model, clock, stabilityTimeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! -> get internal state from Component
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(model, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! query the Component for its clock again, since it has changed
    call NUOPC_ModelGet(model, modelClock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! -> set currTime
    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -> set I/O alarmInterval
    if (nestingGeneration == 0) then
      ! This is the parent domain
      call ESMF_TimeIntervalSet(alarmInterval, s_r8 = sbf, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return
    else
      ! This is the child domain
      call ESMF_TimeIntervalSet(alarmInterval, s_r8 = sbf, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return
    endif

    ! -> create I/O Alarm
    is%wrap%timeToWriteAlarm = ESMF_AlarmCreate(clock=clock, &
      ringTime = currTime + callerTimeStep - stabilityTimeStep, &
      ! ^^^^ shift ring time to be always at the _end_ of forward loop
      ringInterval=alarmInterval, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return
          
    ! -> create transfer Alarm
    if (nestingGeneration == 0) then
      ! This is the parent domain
      is%wrap%timeToTransfer = ESMF_AlarmCreate(clock=clock, &
        ringTime=currTime, enabled=.false., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return
    else
      ! This is the child domain
      call ESMF_TimeIntervalSet(alarmInterval, s_r8 = dtTop, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return
      is%wrap%timeToTransfer = ESMF_AlarmCreate(clock=clock, &
        ringTime=currTime, ringInterval=alarmInterval, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    ! local variables
    type(InternalState)           :: is
    type(ESMF_State)              :: exportState
    real(ESMF_KIND_R8), pointer   :: coordPtr(:)
    real(ESMF_KIND_R8), pointer   :: dataPtr(:,:)
    real(ESMF_KIND_R8), pointer   :: dataTransPtr(:,:)
    real(ESMF_KIND_R8)            :: up1Data, up2Data
    logical                       :: up1Flag, up2Flag
    integer                       :: i, j
    character(len=80)             :: name
    type(ESMF_Clock)              :: clock
    type(ESMF_TimeInterval)       :: timeStep
    real(ESMF_KIND_R8)            :: dt, dx

    rc = ESMF_SUCCESS
    
    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep
    ! on the internal model Clock. This may be (and likely is) a smaller
    ! timeStep than the parent's timeStep on the incoming Clock, which is
    ! equal to the coupling timeStep.
    
    ! access exportState
    call NUOPC_ModelGet(model, exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! -> get internal state from Component
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(model, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! -> get pointer to Field data
    call ESMF_FieldGet(is%wrap%field, farrayPtr=dataPtr, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return
      
    ! See if this is a child nest domain first time coming in after coupling
    if (ESMF_AlarmIsRinging(is%wrap%timeToTransfer)) then
      ! -> need transfer
      call ESMF_LogWrite("timeToTransfer is ringing", ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return
      call ESMF_AlarmRingerOff(is%wrap%timeToTransfer, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return
      ! -> get pointer to transfer Field data
      call ESMF_FieldGet(is%wrap%fieldTrans, farrayPtr=dataTransPtr, rc=rc)  
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return
      ! -> copy data from transfer Field into nested domain Field
      do j=lbound(dataPtr,2), ubound(dataPtr,2)
        do i=lbound(dataPtr,1), ubound(dataPtr,1)
          dataPtr(i,j) = dataTransPtr(i,j)
        enddo
      enddo
      
    endif

    ! -> get the time step of the internal model Clock -->> dt
    call ESMF_GridCompGet(model, clock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ClockGet(clock, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_TimeIntervalGet(timeStep, s_r8=dt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! -> get x-coordinates from Grid to determine spacing -->> dx
    call ESMF_GridGetCoord(is%wrap%grid, coordDim=1, farrayPtr=coordPtr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    dx = coordPtr(lbound(coordPtr,1)+1) - coordPtr(lbound(coordPtr,1))
    
    ! -> get pointer to Field data
    call ESMF_FieldGet(is%wrap%field, farrayPtr=dataPtr, rc=rc)  
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return
      
    ! -> first update the halo region on each DE
    call ESMF_FieldHalo(is%wrap%field, routehandle=is%wrap%haloHandle, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return
      
    ! -> then apply simple forward Euler solver
    do j=lbound(dataPtr,2)+jHaloDepth, ubound(dataPtr,2)-jHaloDepth
      up1Flag = .false.
      up2Flag = .false.
      do i=lbound(dataPtr,1)+iHaloDepth, ubound(dataPtr,1)-iHaloDepth
        if (up2Flag) then
          dataPtr(i-2,j) = up2Data
        endif
        if (up1Flag) then
          up2Data = up1Data
          up2Flag = .true.
        endif
        up1Data = dataPtr(i,j) &
          - .5 * dt * u * (dataPtr(i+1,j) - dataPtr(i-1,j)) / dx &
          + dt * Kh * (dataPtr(i+1,j) - 2.*dataPtr(i,j) + dataPtr(i-1,j))/dx**2
        up1Flag = .true.
      enddo
      dataPtr(i-2,j) = up2Data
      dataPtr(i-1,j) = up1Data
    enddo
    
    ! see if it is time to write for I/O
    if (ESMF_AlarmIsRinging(is%wrap%timeToWriteAlarm)) then
      ! -> finally write a new timeslice to file
      call ESMF_LogWrite("timeToWriteAlarm is ringing", ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return
      call ESMF_AlarmRingerOff(is%wrap%timeToWriteAlarm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return
      call ESMF_GridCompGet(model, name=name, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return
      call NUOPC_Write(exportState, fileNamePrefix=trim(name)//"_export_", &
        timeslice=is%wrap%slice, overwrite=.true., relaxedFlag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! advance the time slice counter
      is%wrap%slice = is%wrap%slice + 1
    endif
    
  end subroutine

end module
