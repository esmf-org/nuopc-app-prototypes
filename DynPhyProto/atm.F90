!==============================================================================
! Earth System Modeling Framework
! Copyright (c) 2002-2024, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module ATM

  !-----------------------------------------------------------------------------
  ! Code specializing generic NUOPC_Driver as ATM model with DYN+PHY children
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Driver, &
    driverSS             => SetServices

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

    ! derive from NUOPC_Driver
    call NUOPC_CompDerive(driver, driverSS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! specialize driver
    call NUOPC_CompSpecialize(driver, specLabel=label_SetModelServices, &
      specRoutine=SetModelServices, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! set driver verbosity
    call NUOPC_CompAttributeSet(driver, name="Verbosity", value="high", rc=rc)
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
    type(ESMF_GridComp)           :: child
    type(ESMF_CplComp)            :: conn
    integer                       :: verbosity, diagnostic
    character(len=10)             :: attrStr

    rc = ESMF_SUCCESS

    ! Turn on profiling for driver component
    call NUOPC_CompAttributeSet(driver, name="Profiling", value="0", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! SetServices for DYN
    call NUOPC_DriverAddComp(driver, "DYN", dynSS, comp=child, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    verbosity = 0 ! reset
    verbosity = ibset(verbosity,0)  ! log basic intro/extro and indentation
!    verbosity = ibset(verbosity,11) ! log info about data dependency loop
!    verbosity = ibset(verbosity,12) ! log info about run time-loop
    write(attrStr,"(I10)") verbosity
!    call NUOPC_CompAttributeSet(child, name="Verbosity", value=attrStr, rc=rc)
    call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(child, name="Diagnostic", value="max", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! SetServices for PHY
    call NUOPC_DriverAddComp(driver, "PHY", phySS, comp=child, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    verbosity = 0 ! reset
    verbosity = ibset(verbosity,0)  ! log basic intro/extro and indentation
!    verbosity = ibset(verbosity,11) ! log info about data dependency loop
!    verbosity = ibset(verbosity,12) ! log info about run time-loop
    write(attrStr,"(I10)") verbosity
!    call NUOPC_CompAttributeSet(child, name="Verbosity", value=attrStr, rc=rc)
    call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(child, name="Diagnostic", value="max", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! SetServices for PHY2DYN
    call NUOPC_DriverAddComp(driver, srcCompLabel="PHY", dstCompLabel="DYN", &
      compSetServicesRoutine=cplSS, comp=conn, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    verbosity = 0 ! reset
    verbosity = ibset(verbosity,0)  ! log basic intro/extro and indentation
!    verbosity = ibset(verbosity,8)  ! log transferPolicy info
!    verbosity = ibset(verbosity,10) ! log cplList construction
!    verbosity = ibset(verbosity,12) ! log RH computation
    write(attrStr,"(I10)") verbosity
!    call NUOPC_CompAttributeSet(conn, name="Verbosity", value=attrStr, rc=rc)
    call NUOPC_CompAttributeSet(conn, name="Verbosity", value="high", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    diagnostic = 0 ! reset
    diagnostic = ibset(diagnostic,0)  ! dump import fields when entering Init.
    diagnostic = ibset(diagnostic,1)  ! dump export fields when entering Init.
    diagnostic = ibset(diagnostic,2)  ! dump import fields when exiting Init.
    diagnostic = ibset(diagnostic,3)  ! dump export fields when exiting Init.
    diagnostic = ibset(diagnostic,4)  ! dump import fields when entering Run
    diagnostic = ibset(diagnostic,5)  ! dump export fields when entering Run
    diagnostic = ibset(diagnostic,6)  ! dump import fields when exiting Run
    diagnostic = ibset(diagnostic,7)  ! dump export fields when exiting Run
    diagnostic = ibset(diagnostic,8)  ! dump import fields when entering Final.
    diagnostic = ibset(diagnostic,9)  ! dump export fields when entering Final.
    diagnostic = ibset(diagnostic,10) ! dump import fields when exiting Final.
    diagnostic = ibset(diagnostic,11) ! dump export fields when exiting Final.
    write(attrStr,"(I10)") diagnostic
    call NUOPC_CompAttributeSet(conn, name="Diagnostic", value=attrStr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

end module
