!==============================================================================
! Earth System Modeling Framework
! Copyright (c) 2002-2025, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

program externalApp

  !-----------------------------------------------------------------------------
  ! Generic external application driver
  !-----------------------------------------------------------------------------

  use MPI
  
  use ESMF
  use NUOPC
  use ESM, only: esmSS => SetServices

  use nuopc_da, only: &
    nuopc_da_init     => init, &
    nuopc_da_commToVM => commToVM, &
    nuopc_da_final    => final

  use atmDA, only: atmDAexec => exec
  use ocnDA, only: ocnDAexec => exec

  implicit none

  integer                 :: rc
  integer                 :: size, rank
  integer                 :: commAtmDA, commOcnDA, splitComm
  type(ESMF_VM)           :: vmAtmDA, vmOcnDA

  ! Initialize the NUOPC-DA interface
  call nuopc_da_init(nuopcTopSetServices=esmSS, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Split up the MPI_COMM_WORLD into atmDA (first half) and ocnDA (second half)
  ! of MPI ranks.
  call MPI_Comm_size(MPI_COMM_WORLD, size, rc)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, rc)
  if (rank < size/2) then
    ! atm DA processes
    call MPI_Comm_split(MPI_COMM_WORLD, 1, rank, splitComm, rc)
    commAtmDA = splitComm
    commOcnDA = MPI_COMM_NULL
  else
    ! ocn DA processes
    call MPI_Comm_split(MPI_COMM_WORLD, 2, rank, splitComm, rc)
    commOcnDA = splitComm
    commAtmDA = MPI_COMM_NULL
  endif

  ! Create corresponding ESMF_VM objects for atmDA and ocnDA
  vmAtmDA = nuopc_da_commToVM(commAtmDA, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  vmOcnDA = nuopc_da_commToVM(commOcnDA, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Call into the respective DA routine for ATM and OCN
  if (rank < size/2) then
    ! atm DA processes
    call atmDAexec(vmAtmDA)
  else
    ! ocn DA processes
    call ocnDAexec(vmOcnDA)
  endif

  ! Finalize the NUOPC-DA interface
  call nuopc_da_final(rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

end program
