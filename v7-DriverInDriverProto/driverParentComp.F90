module driverParentComp

  !-----------------------------------------------------------------------------
  ! Code that specializes generic NUOPC_Driver
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Driver, &
    driver_routine_SS             => SetServices, &
    driver_label_SetModelServices => label_SetModelServices
  
  use driverChildComp, only: driver_SS => SetServices
  use OCN, only: ocnSS => SetServices
  
  use NUOPC_Connector, only: cplSS => SetServices

  implicit none
  
  private
  
  ! private module data --> ONLY PARAMETERS
  integer, parameter            :: stepCount = 5
  real(ESMF_KIND_R8), parameter :: stepTime  = 30.D0  ! step time [s]
                                                      ! should be parent step

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

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc
    
    ! local variables
    integer                       :: localrc
    type(ESMF_GridComp)           :: child
    type(ESMF_CplComp)            :: connector
    type(ESMF_Time)               :: startTime
    type(ESMF_Time)               :: stopTime
    type(ESMF_TimeInterval)       :: timeStep
    type(ESMF_Clock)              :: internalClock
    integer                       :: petCount, i
    integer, allocatable          :: petList(:)

    rc = ESMF_SUCCESS
    
    ! get the petCount
    call ESMF_GridCompGet(driver, petCount=petCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! SetServices for the child driver, acting as ATM, on custom petList
    allocate(petList(petCount/2))
    do i=1, petCount/2
      petList(i) = i-1 ! PET labeling goes from 0 to petCount-1
    enddo
    call NUOPC_DriverAddComp(driver, "driverChild", driver_SS, &
      petList=petList, comp=child, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    deallocate(petList)
      
    ! SetServices for the OCN
    call NUOPC_DriverAddComp(driver, "OCN", ocnSS, comp=child, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! SetServices for OCN2ATM
    call NUOPC_DriverAddComp(driver, srcCompLabel="OCN", &
      dstCompLabel="driverChild", &
      compSetServicesRoutine=cplSS, comp=connector, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(connector, name="Verbosity", value="high", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! SetServices for ATM2OCN
    call NUOPC_DriverAddComp(driver, srcCompLabel="driverChild", &
      dstCompLabel="OCN", &
      compSetServicesRoutine=cplSS, comp=connector, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(connector, name="Verbosity", value="high", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! set the model clock
    call ESMF_TimeSet(startTime, s = 0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeSet(stopTime, s_r8 = stepTime * stepCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeIntervalSet(timeStep, s_r8 = stepTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    internalClock = ESMF_ClockCreate(name="Driver Clock", &
      timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
      
    call ESMF_GridCompSet(driver, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
  end subroutine

end module
