!==============================================================================
! Earth System Modeling Framework
! Copyright (c) 2002-2025, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

program explorerApp

  !-----------------------------------------------------------------------------
  ! Generic NUOPC application
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC

  use nuopcExplorerDriver,      only: explorerDriverSS => SetServices

  use NUOPC_Compliance_Driver,  only: registerIC

  implicit none

  integer                 :: rc, urc
  type(ESMF_GridComp)     :: driver
  type(ESMF_VM)           :: vm
  integer                 :: localPet, petCount

  type(ESMF_Config)       :: config
  integer                 :: start_year
  integer                 :: start_month
  integer                 :: start_day
  integer                 :: start_hour
  integer                 :: start_minute
  integer                 :: start_second
  integer                 :: stop_year
  integer                 :: stop_month
  integer                 :: stop_day
  integer                 :: stop_hour
  integer                 :: stop_minute
  integer                 :: stop_second
  integer                 :: step_seconds

  type(ESMF_Time)         :: startTime
  type(ESMF_Time)         :: stopTime
  type(ESMF_TimeInterval) :: timeStep
  type(ESMF_Clock)        :: clock

  character(len=80)       :: enable_run_string
  logical                 :: enable_run

  character(len=80)       :: enable_finalize_string
  logical                 :: enable_finalize

  type(ESMF_State) :: importState, exportState

  ! Initialize ESMF
  call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN, &
    logkindflag=ESMF_LOGKIND_MULTI, vm=vm, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_LogSet(flush=.true., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_LogWrite("explorerApp STARTING", ESMF_LOGMSG_INFO, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  if (localPet==0) then
    print *
    print *, "NUOPC Component Explorer App executing on ", petCount, "PETs"
    print *, "------------------------------------------------------------"
  endif

  ! Set the clock according to the explorer.config file

  if (localPet==0) then
    print *, "Accessing start, stop, and step time info from 'explorer.config':"
  endif

  config = ESMF_ConfigCreate(rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)  ! bail out
  call ESMF_ConfigLoadFile(config, "explorer.config", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)  ! bail out

  call ESMF_ConfigGetAttribute(config, start_year, label="start_year:", &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)  ! bail out

  call ESMF_ConfigGetAttribute(config, start_month, label="start_month:", &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)  ! bail out

  call ESMF_ConfigGetAttribute(config, start_day, label="start_day:", &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)  ! bail out

  call ESMF_ConfigGetAttribute(config, start_hour, label="start_hour:", &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)  ! bail out

  call ESMF_ConfigGetAttribute(config, start_minute, label="start_minute:", &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)  ! bail out

  call ESMF_ConfigGetAttribute(config, start_second, label="start_second:", &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)  ! bail out

  call ESMF_ConfigGetAttribute(config, stop_year, label="stop_year:", &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)  ! bail out

  call ESMF_ConfigGetAttribute(config, stop_month, label="stop_month:", &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)  ! bail out

  call ESMF_ConfigGetAttribute(config, stop_day, label="stop_day:", &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)  ! bail out

  call ESMF_ConfigGetAttribute(config, stop_hour, label="stop_hour:", &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)  ! bail out

  call ESMF_ConfigGetAttribute(config, stop_minute, label="stop_minute:", &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)  ! bail out

  call ESMF_ConfigGetAttribute(config, stop_second, label="stop_second:", &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)  ! bail out

  call ESMF_ConfigGetAttribute(config, step_seconds, label="step_seconds:", &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)  ! bail out

  call ESMF_TimeSet(startTime, &
    yy = start_year, &
    mm = start_month, &
    dd = start_day, &
    h  = start_hour, &
    m  = start_minute, &
    s  = start_second, &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_TimeSet(stopTime, &
    yy = stop_year, &
    mm = stop_month, &
    dd = stop_day, &
    h  = stop_hour, &
    m  = stop_minute, &
    s  = stop_second, &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_TimeIntervalSet(timeStep, s = step_seconds, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  clock = ESMF_ClockCreate(name="Driver Clock", &
    timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  if (localPet==0) then
    print *, "  start_year:   ", start_year
    print *, "  start_month:  ", start_month
    print *, "  start_day:    ", start_day
    print *, "  start_hour:   ", start_hour
    print *, "  start_minute: ", start_minute
    print *, "  start_second: ", start_second
    print *, "  - "
    print *, "  stop_year:    ", stop_year
    print *, "  stop_month:   ", stop_month
    print *, "  stop_day:     ", stop_day
    print *, "  stop_hour:    ", stop_hour
    print *, "  stop_minute:  ", stop_minute
    print *, "  stop_second:  ", start_second
    print *, "  - "
    print *, "  step_seconds: ", step_seconds
  endif

#if 0
  call ESMF_ClockPrint(clock, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)  ! bail out
#endif

  call ESMF_ConfigGetAttribute(config, enable_run_string, &
    label="enable_run:", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)  ! bail out

  enable_run = .false.  ! initialize
  if (trim(enable_run_string)=="yes") then
    enable_run = .true.
  endif

  call ESMF_ConfigGetAttribute(config, enable_finalize_string, &
    label="enable_finalize:", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)  ! bail out

  enable_finalize = .false.  ! initialize
  if (trim(enable_finalize_string)=="yes") then
    enable_finalize = .true.
  endif

  ! Create the Driver Component
  driver = ESMF_GridCompCreate(name="explorerDriver", config=config, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! SetServices
  call ESMF_GridCompSetServices(driver, explorerDriverSS, userRc=urc, &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call NUOPC_CompAttributeSet(driver, name="CompLabel", &
    value="explorerDriver", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Call 0 phase Initialize
  call ESMF_GridCompInitialize(driver, phase=0, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Create importState and init NUOPC AttPack
  importState = ESMF_StateCreate(name="explorerDriver Import State", &
     stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call NUOPC_InitAttributes(importState, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Create exportState and init NUOPC AttPack
  exportState = ESMF_StateCreate(name="explorerDriver Export State", &
     stateintent=ESMF_STATEINTENT_EXPORT, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call NUOPC_InitAttributes(exportState, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Call Driver Initialize, with Clock to set Driver internal Clock
  if (localPet==0) print *, "Calling into Initialize()..."
  call ESMF_GridCompInitialize(driver, importState=importState, &
    exportState=exportState, clock=clock, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Change timeStep in Clock to be only a single timeStep from start to stop
  call ESMF_ClockSet(clock, timeStep=stopTime-startTime, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  if (enable_run) then
    ! Call Driver Run,
    ! with Clock that steps from start to stop in one single timeStep
    if (localPet==0) print *, "Calling into Run()..."
    call ESMF_GridCompRun(driver, importState=importState, &
      exportState=exportState, clock=clock, userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif

  if (enable_finalize) then
    ! Call Driver Finalize
    if (localPet==0) print *, "Calling into Finalize()..."
    call ESMF_GridCompFinalize(driver, importState=importState, &
      exportState=exportState, userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif

  ! Destroy the Driver Component
  call ESMF_GridCompDestroy(driver, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Destroy the Config
  call ESMF_ConfigDestroy(config, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)  ! bail out

  ! Destroy the importState
  call ESMF_StateDestroy(importState, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Destroy the importState
  call ESMF_StateDestroy(ExportState, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Log
  call ESMF_LogWrite("explorerApp FINISHED", ESMF_LOGMSG_INFO, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  if (localPet==0) then
    print *, "NUOPC Component Explorer App done."
    print *, "See PET*.ESMF_LogFile's for additional information."
    print *, "------------------------------------------------------------"
  endif

  ! Finalize ESMF
  call ESMF_Finalize()

end program
