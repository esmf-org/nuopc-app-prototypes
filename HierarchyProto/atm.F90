!==============================================================================
! Earth System Modeling Framework
! Copyright 2002-2020, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

#define CUSTOMRUNSEQUENCE_on

module ATM

  !-----------------------------------------------------------------------------
  ! Code specializing generic NUOPC_Driver as ATM model with DYN+PHY children
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Driver, &
    driver_routine_SS             => SetServices, &
    driver_label_SetModelServices => label_SetModelServices, &
    driver_label_SetRunSequence   => label_SetRunSequence
  
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
    
#ifdef CUSTOMRUNSEQUENCE_on
    call NUOPC_CompSpecialize(driver, specLabel=driver_label_SetRunSequence, &
      specRoutine=SetRunSequence, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_GridComp)           :: child
    type(ESMF_CplComp)            :: conn
    integer                       :: verbosity
    character(len=10)             :: vString

    rc = ESMF_SUCCESS
    
    ! SetServices for DYN
    call NUOPC_DriverAddComp(driver, "DYN", dynSS, petList=(/0/), &
      comp=child, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    verbosity = 0 ! reset
    verbosity = ibset(verbosity,0)  ! log basic intro/extro and indentation
    verbosity = ibset(verbosity,11) ! log info about data dependency loop
    verbosity = ibset(verbosity,12) ! log info about run time-loop
    write(vString,"(I10)") verbosity
!    call NUOPC_CompAttributeSet(child, name="Verbosity", value=vString, rc=rc)
    call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! SetServices for PHY
    call NUOPC_DriverAddComp(driver, "PHY", phySS, petList=(/1/), &
      comp=child, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    verbosity = 0 ! reset
    verbosity = ibset(verbosity,0)  ! log basic intro/extro and indentation
    verbosity = ibset(verbosity,11) ! log info about data dependency loop
    verbosity = ibset(verbosity,12) ! log info about run time-loop
!    call NUOPC_CompAttributeSet(child, name="Verbosity", value=vString, rc=rc)
    call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

#ifndef CUSTOMRUNSEQUENCE_on
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
#endif

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
      "   PHY -> DYN  ",    &
      "   DYN         ",    &
      "   PHY         ",    &
      " @             " /), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
      
    ! ingest FreeFormat run sequence
    call NUOPC_DriverIngestRunSequence(driver, runSeqFF, &
      autoAddConnectors=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! clean-up
    call NUOPC_FreeFormatDestroy(runSeqFF, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
      
  end subroutine

  !-----------------------------------------------------------------------------

end module
