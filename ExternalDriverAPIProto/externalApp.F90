!==============================================================================
! Earth System Modeling Framework
! Copyright (c) 2002-2022, University Corporation for Atmospheric Research,
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

  use ESMF
  use NUOPC
  use ESM, only: esmSS => SetServices

  implicit none

  integer                 :: rc, urc
  type(ESMF_GridComp)     :: esmComp
  type(ESMF_Time)         :: startTime, stopTime
  type(ESMF_TimeInterval) :: timeStep
  type(ESMF_Clock)        :: clock
  type(ESMF_State)        :: externalExportState, externalImportState
  type(ESMF_StateItem_Flag):: itemType
  type(ESMF_Field)        :: field
  integer                 :: phase
  character(40)           :: sharePolicy
  integer, save           :: slice=1

  ! Initialize ESMF
  call ESMF_Initialize(logkindflag=ESMF_LOGKIND_MULTI, &
    defaultCalkind=ESMF_CALKIND_GREGORIAN, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_LogSet(flush=.true., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_LogWrite("externalApp STARTING", ESMF_LOGMSG_INFO, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Create the application Clock
  call ESMF_TimeIntervalSet(timeStep, m=15, rc=rc) ! 15 minute steps
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_TimeSet(startTime, yy=2010, mm=6, dd=1, h=0, m=0, &
    calkindflag=ESMF_CALKIND_GREGORIAN, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_TimeSet(stopTime, yy=2010, mm=6, dd=1, h=1, m=0, &
    calkindflag=ESMF_CALKIND_GREGORIAN, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  clock = ESMF_ClockCreate(name="Application Clock", &
    timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Create the external level import/export States
  ! NOTE: The "stateintent" must be specified, and it must be set from the
  ! perspective of the external level:
  ! -> state holding fields exported by the external level to the ESM component
  externalExportState = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_EXPORT, &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! -> state holding fields imported by the external level from the ESM component
  externalImportState = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_IMPORT, &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Create the earth system Component
  esmComp = ESMF_GridCompCreate(name="esm", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! SetServices for the earth system Component
  call ESMF_GridCompSetServices(esmComp, esmSS, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

#if 1
  ! Advertise field(s) in external import state in order to receive from ESM
#define SHARED
#ifdef SHARED
  ! request reference sharing for field and geomobject
  ! -> still the child component may not allow sharing and we end up with copy
  sharePolicy = "share"
#else
  ! prevent reference sharing for field and geomobject
  ! -> definitely create local copy of the field here
  sharePolicy = "not share"
#endif
  call NUOPC_Advertise(externalImportState, &
    StandardNames=(/"sea_surface_temperature            ", &
                    "surface_net_downward_shortwave_flux"/), &
    TransferOfferGeomObject="cannot provide", SharePolicyField=sharePolicy, &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
#endif

#if 1
  ! Advertise field(s) in external export state in order to send to ESM
#define SHARED
#ifdef SHARED
  ! request reference sharing for field and geomobject
  ! -> still the child component may not allow sharing and we end up with copy
  sharePolicy = "share"
#else
  ! prevent reference sharing for field and geomobject
  ! -> definitely create local copy of the field here
  sharePolicy = "not share"
#endif
  call NUOPC_Advertise(externalExportState, &
    StandardNames=(/"precipitation_flux"/), &
    TransferOfferGeomObject="cannot provide", SharePolicyField=sharePolicy, &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
#endif

  ! Call "ExternalAdvertise" Initialize for the earth system Component
  ! -> This method removes any fields previously advertised in the states,
  ! but ended up not connected.
  call NUOPC_CompSearchPhaseMap(esmComp, methodflag=ESMF_METHOD_INITIALIZE, &
    phaseLabel=label_ExternalAdvertise, phaseIndex=phase, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_GridCompInitialize(esmComp, phase=phase, clock=clock, &
    importState=externalExportState, exportState=externalImportState, &
    userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Call "ExternalRealize" Initialize for the earth system Component
  ! -> This method realizes any fields previously advertised in the states,
  ! for which enough information is available to realize (e.g. due to sharing,
  ! or transfers).
  call NUOPC_CompSearchPhaseMap(esmComp, methodflag=ESMF_METHOD_INITIALIZE, &
    phaseLabel=label_ExternalRealize, phaseIndex=phase, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_GridCompInitialize(esmComp, phase=phase, clock=clock, &
    importState=externalExportState, exportState=externalImportState, &
    userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Fill fields provided from this level with data
  call ESMF_StateGet(externalExportState, itemName="precipitation_flux", &
    itemType=itemType, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (itemType /= ESMF_STATEITEM_NOTFOUND) then
    call ESMF_StateGet(externalExportState, itemName="precipitation_flux", &
      field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_FieldFill(field, dataFillScheme="sincos", member=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif

  ! Call "ExternalDataInit" Initialize for the earth system Component
  call NUOPC_CompSearchPhaseMap(esmComp, methodflag=ESMF_METHOD_INITIALIZE, &
    phaseLabel=label_ExternalDataInit, phaseIndex=phase, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_GridCompInitialize(esmComp, phase=phase, clock=clock, &
    importState=externalExportState, exportState=externalImportState, &
    userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Write out the Fields in the externalImportState after initialize
  call NUOPC_Write(externalImportState, &
    fileNamePrefix="field_externalImportState_init_", &
    overwrite=.true., relaxedFlag=.true., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! Write out the Fields in the externalExportState after initialize
  call NUOPC_Write(externalExportState, &
    fileNamePrefix="field_externalExportState_init_", &
    overwrite=.true., relaxedFlag=.true., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Explicit time stepping loop on the external level, here based on ESMF_Clock
  do while (.not.ESMF_ClockIsStopTime(clock, rc=rc))
    ! Timestamp the fields in the externalExportState
    call NUOPC_SetTimestamp(externalExportState, clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    ! Run the earth system Component: i.e. step ESM forward by timestep=15min
    call ESMF_GridCompRun(esmComp, clock=clock, &
      importState=externalExportState, exportState=externalImportState, &
      userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    ! Write out the Fields in the externalImportState as a time slice
    call NUOPC_Write(externalImportState, &
      fileNamePrefix="field_externalImportState_", &
      timeslice=slice, overwrite=.true., relaxedFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    ! Write out the Fields in the externalExportState as a time slice
    call NUOPC_Write(externalExportState, &
      fileNamePrefix="field_externalExportState_", &
      timeslice=slice, overwrite=.true., relaxedFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    ! Increment the time slice counter
    slice = slice + 1
    ! Advance the clock
    call ESMF_ClockAdvance(clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
  end do

  ! Finalize the earth system Component
  call ESMF_GridCompFinalize(esmComp, clock=clock, &
    importState=externalExportState, exportState=externalImportState, &
    userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Destroy the earth system Component
  call ESMF_GridCompDestroy(esmComp, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_LogWrite("externalApp FINISHED", ESMF_LOGMSG_INFO, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Finalize ESMF
  call ESMF_Finalize()

end program
