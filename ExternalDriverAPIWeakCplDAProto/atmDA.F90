!==============================================================================
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module atmDA

  !-----------------------------------------------------------------------------
  ! ATM DA Code
  !-----------------------------------------------------------------------------

  use MPI
  use ESMF
  use NUOPC

  use nuopc_da, only: &
    nuopc_da_connect => connect, &
    nuopc_da_drive   => drive

  implicit none

  private

  public exec

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine exec(comm)
    integer             :: comm

    integer             :: rc

    call ESMF_LogWrite("Starting into ATM DA code", ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Connect DA code with NUOPC system top component
    call nuopc_da_connect(toNuopcTopStandardNames=(/"precipitation_flux"/), &
      fmNuopcTopStandardNames=(/"surface_net_downward_shortwave_flux"/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    !TODO: implement an explicit time loop here...
    !TODO: call into component specific step
    call nuopc_da_drive(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_LogWrite("Finished with ATM DA code", ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

  end subroutine

end module
