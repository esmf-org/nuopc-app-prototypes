!==============================================================================
! Earth System Modeling Framework
! Copyright 2002-2020, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module ESM

  !-----------------------------------------------------------------------------
  ! Code that specializes generic ESM Component code.
  !-----------------------------------------------------------------------------

  ! Enabling the followng macro, i.e. setting it to WITHPETLISTS_on, will 
  ! activate sections of code that demonstrate how
  ! the ATM and OCN components can run on exclusive sets of PETs. Turning this
  ! on/off does not affect how the Connector component is specialized.
#define WITHPETLISTS_on

  use ESMF
  use NUOPC
  use NUOPC_Driver, &
    driver_routine_SS             => SetServices, &
    driver_label_SetModelServices => label_SetModelServices, &
    driver_label_SetRunSequence   => label_SetRunSequence
  
  use ATM, only: atmSS => SetServices
  use OCN, only: ocnSS => SetServices
  
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
    call NUOPC_CompSpecialize(driver, specLabel=driver_label_SetRunSequence, &
      specRoutine=SetRunSequence, rc=rc)
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
    type(ESMF_Grid)               :: grid
    type(ESMF_Field)              :: field
    type(ESMF_Time)               :: startTime
    type(ESMF_Time)               :: stopTime
    type(ESMF_TimeInterval)       :: timeStep
    type(ESMF_Clock)              :: internalClock
    type(ESMF_GridComp)           :: child
    integer                       :: petCount, i
    integer, allocatable          :: petList(:)

    rc = ESMF_SUCCESS
    
   ! get the petCount
    call ESMF_GridCompGet(driver, petCount=petCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! SetServices for ATM with petList on first half of PETs
#ifdef WITHPETLISTS_on
    allocate(petList(petCount/2))
    do i=1, petCount/2
      petList(i) = i-1 ! PET labeling goes from 0 to petCount-1
    enddo
#endif
    call NUOPC_DriverAddComp(driver, "ATM", atmSS, &
#ifdef WITHPETLISTS_on
      petList=petList, &
#endif
      comp=child, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(child, name="Verbosity", value="1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#ifdef WITHPETLISTS_on
    deallocate(petList)
#endif
      
    ! SetServices for OCN with petList on second half of PETs
#ifdef WITHPETLISTS_on
    allocate(petList(petCount/2))
    do i=1, petCount/2
      petList(i) = petCount/2 + i-1 ! PET labeling goes from 0 to petCount-1
    enddo
#endif
    call NUOPC_DriverAddComp(driver, "OCN", ocnSS, &
#ifdef WITHPETLISTS_on
      petList=petList, &
#endif
      comp=child, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(child, name="Verbosity", value="1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#ifdef WITHPETLISTS_on
    deallocate(petList)
#endif
      
    ! set the driver clock
    call ESMF_TimeIntervalSet(timeStep, m=15, rc=rc) ! 15 minute steps
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_TimeSet(startTime, yy=2010, mm=6, dd=1, h=0, m=0, &
      calkindflag=ESMF_CALKIND_GREGORIAN, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_TimeSet(stopTime, yy=2010, mm=6, dd=1, h=1, m=0, &
      calkindflag=ESMF_CALKIND_GREGORIAN, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    internalClock = ESMF_ClockCreate(name="Application Clock", &
      timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridCompSet(driver, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetRunSequence(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc
    
    ! local variables
    character(ESMF_MAXSTR)              :: name
    type(NUOPC_FreeFormat)              :: runSeqFF

    rc = ESMF_SUCCESS
    
    ! query the driver for its name
    call ESMF_GridCompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    
    ! set up free format run sequence
    runSeqFF = NUOPC_FreeFormatCreate(stringList=(/ &
      " @*            ",    &
      "   OCN -> ATM  ",    &
      "   ATM         ",    &
      "   ATM -> OCN  ",    &
      "   OCN         ",    &
      " @             " /), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! ingest FreeFormat run sequence
    call NUOPC_DriverIngestRunSequence(driver, runSeqFF, &
      autoAddConnectors=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

end module
