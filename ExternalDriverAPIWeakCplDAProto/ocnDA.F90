!==============================================================================
! Earth System Modeling Framework
! Copyright (c) 2002-2025, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module ocnDA

  !-----------------------------------------------------------------------------
  ! OCN DA Code
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC

  use nuopc_da, only: &
    nuopc_da_connect => connect, &
    nuopc_da_step    => step

  implicit none

  private

  public exec

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine exec(vm)
    type(ESMF_VM) :: vm ! context of this interaction

    integer             :: rc

    call ESMF_LogWrite("Starting into OCN DA code", ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Connect DA code with NUOPC system top component
    call nuopc_da_connect(vm=vm, &
      fmNuopcTopStandardNames=(/"sea_surface_temperature"/), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !--> explicitly unrolled time loop to demonstrate stepping more clearly

    ! take 30min timestep
    call nuopc_da_step(tStart="2010-06-01T00:00:00", &
                       tFinal="2010-06-01T00:30:00", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! take 30min timestep
    call nuopc_da_step(tStart="2010-06-01T00:30:00", &
                       tFinal="2010-06-01T01:00:00", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !<-- end of explicitly unrolled time loop

    call ESMF_LogWrite("Finished with OCN DA code", ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

  end subroutine

end module
