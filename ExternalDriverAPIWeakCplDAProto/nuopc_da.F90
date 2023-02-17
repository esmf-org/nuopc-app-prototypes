!==============================================================================
! Earth System Modeling Framework
! Copyright (c) 2002-2022, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module nuopc_da

  !-----------------------------------------------------------------------------
  ! NUOPC - DA interface code
  !-----------------------------------------------------------------------------

  use MPI
  use ESMF
  use NUOPC

  implicit none

  private
  
  type(ESMF_GridComp) :: nuopcTop
  type(ESMF_State)    :: toNuopcTop, fmNuopcTop
  type(ESMF_Clock)    :: clock

  public init, connect, step, final

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine init(nuopcTopSetServices, rc)
    interface
      recursive subroutine nuopcTopSetServices(gridcomp, rc)
        use ESMF
        implicit none
        type(ESMF_GridComp)        :: gridcomp ! must not be optional
        integer, intent(out)       :: rc       ! must not be optional
      end subroutine
    end interface
    integer, intent(out)           :: rc

    integer                 :: urc
    type(ESMF_Time)         :: startTime, stopTime
    type(ESMF_TimeInterval) :: timeStep

    ! Initialize ESMF
    call ESMF_Initialize(logkindflag=ESMF_LOGKIND_MULTI, &
      defaultCalkind=ESMF_CALKIND_GREGORIAN, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return

    call ESMF_LogSet(flush=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return

    call ESMF_LogWrite("INIT...", ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return

    ! Create the application Clock
    call ESMF_TimeIntervalSet(timeStep, m=15, rc=rc) ! 15 minute steps
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return
    call ESMF_TimeSet(startTime, yy=2010, mm=6, dd=1, h=0, m=0, &
      calkindflag=ESMF_CALKIND_GREGORIAN, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return
    call ESMF_TimeSet(stopTime, yy=2010, mm=6, dd=1, h=1, m=0, &
      calkindflag=ESMF_CALKIND_GREGORIAN, rc=rc)
    clock = ESMF_ClockCreate(name="Application Clock", &
      timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return

    ! Create the external level import/export States
    ! NOTE: The "stateintent" must be specified, and it must be set from the
    ! perspective of the external level:
    ! -> state holding fields exported by the external level to the ESM component
    toNuopcTop = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_EXPORT, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return
    ! -> state holding fields imported by the external level from the ESM component
    fmNuopcTop = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return

    ! Create the earth system Component
    nuopcTop = ESMF_GridCompCreate(name="nuopcTop", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return

    ! SetServices for the earth system Component
    call ESMF_GridCompSetServices(nuopcTop, nuopcTopSetServices, userRc=urc, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, rcToReturn=rc)) return

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine connect(toNuopcTopStandardNames, fmNuopcTopStandardNames, rc)
    character(*), intent(in), optional  :: toNuopcTopStandardNames(:)
    character(*), intent(in), optional  :: fmNuopcTopStandardNames(:)
    integer,      intent(out)           :: rc

    integer                   :: urc, phase, i
    type(ESMF_Field)          :: field
    type(ESMF_StateItem_Flag) :: itemType

    if (present(toNuopcTopStandardNames)) then
      call NUOPC_Advertise(toNuopcTop, StandardNames=toNuopcTopStandardNames, &
        TransferOfferGeomObject="cannot provide", SharePolicyField="share", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return
    endif

    if (present(fmNuopcTopStandardNames)) then
      call NUOPC_Advertise(fmNuopcTop, StandardNames=fmNuopcTopStandardNames, &
        TransferOfferGeomObject="cannot provide", SharePolicyField="share", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return
    endif

    ! Call "ExternalAdvertise" Initialize for the earth system Component
    ! -> This method removes any fields previously advertised in the states,
    ! but ended up not connected.
    call NUOPC_CompSearchPhaseMap(nuopcTop, methodflag=ESMF_METHOD_INITIALIZE, &
      phaseLabel=label_ExternalAdvertise, phaseIndex=phase, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, rcToReturn=rc)) return
    call ESMF_GridCompInitialize(nuopcTop, phase=phase, &
      importState=toNuopcTop, exportState=fmNuopcTop, &
      userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, rcToReturn=rc)) return

    ! Call "ExternalRealize" Initialize for the earth system Component
    ! -> This method realizes any fields previously advertised in the states,
    ! for which enough information is available to realize (e.g. due to sharing,
    ! or transfers).
    call NUOPC_CompSearchPhaseMap(nuopcTop, methodflag=ESMF_METHOD_INITIALIZE, &
      phaseLabel=label_ExternalRealize, phaseIndex=phase, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, rcToReturn=rc)) return
    call ESMF_GridCompInitialize(nuopcTop, phase=phase, &
      importState=toNuopcTop, exportState=fmNuopcTop, &
      userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, rcToReturn=rc)) return

    ! Fill fields provided from this level to nuopcTop with data
    if (present(toNuopcTopStandardNames)) then
      do i=1, size(toNuopcTopStandardNames)
        call ESMF_StateGet(toNuopcTop, itemName=toNuopcTopStandardNames(i), &
          itemType=itemType, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) return
        if (itemType /= ESMF_STATEITEM_NOTFOUND) then
          call ESMF_StateGet(toNuopcTop, itemName=toNuopcTopStandardNames(i), &
            field=field, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) return
          call ESMF_FieldFill(field, dataFillScheme="sincos", member=1, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) return
        endif
      enddo
    endif

    ! Call "ExternalDataInit" Initialize for the earth system Component
    call NUOPC_CompSearchPhaseMap(nuopcTop, methodflag=ESMF_METHOD_INITIALIZE, &
      phaseLabel=label_ExternalDataInit, phaseIndex=phase, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, rcToReturn=rc)) return
    call ESMF_GridCompInitialize(nuopcTop, phase=phase, &
      importState=toNuopcTop, exportState=fmNuopcTop, &
      userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, rcToReturn=rc)) return

    ! Write out the Fields in the toNuopcTop after initialize
    call NUOPC_Write(toNuopcTop, &
      fileNamePrefix="field_toNuopcTop_init_", &
      overwrite=.true., relaxedFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, rcToReturn=rc)) return
    ! Write out the Fields in the fmNuopcTop after initialize
    call NUOPC_Write(fmNuopcTop, &
      fileNamePrefix="field_fmNuopcTop_init_", &
      overwrite=.true., relaxedFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, rcToReturn=rc)) return

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine step(tStart, tFinal, rc)
    ! Take a step forward from start time to final time.
    ! Different MPI ranks are allowed to call with different values for times.
    ! On each MPI rank the method will block until tFinal has been reached by
    ! the underlying coupled NUOPC system.
    character(*), intent(in)    :: tStart ! start time
    character(*), intent(in)    :: tFinal ! final time
    integer,      intent(out)   :: rc

    type(ESMF_Time)         :: startTime, stopTime, currTime
    type(ESMF_TimeInterval) :: timeStep
    integer                 :: urc, i
    integer(ESMF_KIND_I8)   :: sec(1), secMin(1)
    type(ESMF_VM)           :: vm
    integer, save           :: slice=1

    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, rcToReturn=rc)) return

    ! Convert string to ESMF_Time
    call ESMF_TimeSet(startTime, timeString=tStart, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, rcToReturn=rc)) return

    ! Convert string to ESMF_Time
    call ESMF_TimeSet(stopTime, timeString=tFinal, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, rcToReturn=rc)) return

    ! Set the currTime and stopTime on Clock
    call ESMF_ClockSet(clock, currTime=startTime, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, rcToReturn=rc)) return

    ! Loop until the current MPI rank has reached the local stopTime, taking
    ! time steps that are coordinated across all MPI ranks.
    do while (.not.ESMF_ClockIsStopTime(clock, rc=rc))

      ! Get the currTime as the basis for the next step
      call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return

      ! Check for consistency of currTime across all MPI ranks
      call ESMF_TimeGet(currTime, s_i8=sec(1), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return
      call ESMF_VMAllReduce(vm, sendData=sec, recvData=secMin, &
        count=1, reduceflag=ESMF_REDUCE_MIN, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return
      if (sec(1) > secMin(1)) then
        call ESMF_LogSetError(ESMF_RC_INTNRL_INCONS, &
          msg="Inconsistent currTime detected arcoss MPI ranks!", &
          line=__LINE__, &
          file=__FILE__, rcToReturn=rc)
        return  ! bail out
      endif

      ! Determine local timeStep
      timeStep = stopTime - currTime

      ! Determine consistent timeStep across all MPI ranks
      call ESMF_TimeIntervalGet(timeStep, s_i8=sec(1), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return
      call ESMF_VMAllReduce(vm, sendData=sec, recvData=secMin, &
        count=1, reduceflag=ESMF_REDUCE_MIN, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return
      call ESMF_TimeIntervalSet(timeStep, s_i8=secMin(1), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return

      ! Set the consistent timeStep in Clock for the next step
      call ESMF_ClockSet(clock, timeStep=timeStep, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return

      ! Timestamp the fields in the toNuopcTop state
      call NUOPC_SetTimestamp(toNuopcTop, clock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return

      ! Run the earth system Component: i.e. step nuopcTop forward by timestep
      call ESMF_GridCompRun(nuopcTop, clock=clock, &
        importState=toNuopcTop, exportState=fmNuopcTop, &
        userRc=urc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) return
      if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return

      ! Write out the Fields in the toNuopcTop
      call NUOPC_Write(toNuopcTop, &
        fileNamePrefix="field_toNuopcTop_", &
        timeslice=slice, overwrite=.true., relaxedFlag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return

      ! Write out the Fields in the fmNuopcTop after initialize
      call NUOPC_Write(fmNuopcTop, &
        fileNamePrefix="field_fmNuopcTop_", &
        timeslice=slice, overwrite=.true., relaxedFlag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return

      ! Increment the time slice counter
      slice = slice + 1

      ! Advance the clock
      call ESMF_ClockAdvance(clock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return

    enddo

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine final(rc)
    integer,      intent(out)           :: rc

    integer                   :: urc, i

    ! Finalize the nuopcTop Component
    call ESMF_GridCompFinalize(nuopcTop, &
      importState=toNuopcTop, exportState=fmNuopcTop, &
      userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, rcToReturn=rc)) return

    ! Destroy the nuopcTop Component
    call ESMF_GridCompDestroy(nuopcTop, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return

    call ESMF_LogWrite("...FINAL", ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) return

    ! Finalize ESMF
    call ESMF_Finalize(rc=rc)

  end subroutine

  !-----------------------------------------------------------------------------

end module
