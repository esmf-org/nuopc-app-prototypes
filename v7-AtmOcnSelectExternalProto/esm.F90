!- conditional module blocks that support modules maybe being supplied in C
#ifdef FRONT_H_ATMF
module atmF
#include FRONT_H_ATMF
end module
#endif
!--------------------------------------------------------------------------

module ESM

  !-----------------------------------------------------------------------------
  ! Code that specializes generic ESM Component code.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Driver, only: &
    driver_routine_SS             => routine_SetServices, &
    driver_type_IS                => type_InternalState, &
    driver_label_IS               => label_InternalState, &
    driver_label_SetModelCount    => label_SetModelCount, &
    driver_label_SetModelServices => label_SetModelServices, &
    NUOPC_DriverSet

  !- select exactly one ATM component 
#if (defined FRONT_ATMA && !defined FRONT_ATMB && !defined FRONT_ATMC && !defined FRONT_ATMD && !defined FRONT_ATME && !defined FRONT_SO_ATME && !defined FRONT_ATMF && !defined FRONT_H_ATMF )
  use FRONT_ATMA, only: atmSS => SetServices
#elif (!defined FRONT_ATMA && defined FRONT_ATMB && !defined FRONT_ATMC && !defined FRONT_ATMD && !defined FRONT_ATME && !defined FRONT_SO_ATME && !defined FRONT_ATMF && !defined FRONT_H_ATMF )
  use FRONT_ATMB, only: atmSS => SetServices
#elif (!defined FRONT_ATMA && !defined FRONT_ATMB && defined FRONT_ATMC && !defined FRONT_ATMD && !defined FRONT_ATME && !defined FRONT_SO_ATME && !defined FRONT_ATMF && !defined FRONT_H_ATMF )
  use FRONT_ATMC, only: atmSS => SetServices
#elif (!defined FRONT_ATMA && !defined FRONT_ATMB && !defined FRONT_ATMC && defined FRONT_ATMD && !defined FRONT_ATME && !defined FRONT_SO_ATME && !defined FRONT_ATMF && !defined FRONT_H_ATMF )
  use FRONT_ATMD, only: atmSS => SetServices
#elif (!defined FRONT_ATMA && !defined FRONT_ATMB && !defined FRONT_ATMC && !defined FRONT_ATMD && defined FRONT_ATME && !defined FRONT_SO_ATME && !defined FRONT_ATMF && !defined FRONT_H_ATMF )
  use FRONT_ATME, only: atmSS => SetServices
#elif (!defined FRONT_ATMA && !defined FRONT_ATMB && !defined FRONT_ATMC && !defined FRONT_ATMD && !defined FRONT_ATME && defined FRONT_SO_ATME && !defined FRONT_ATMF && !defined FRONT_H_ATMF )
#define ATM_FRONT_SO FRONT_SO_ATME
#elif (!defined FRONT_ATMA && !defined FRONT_ATMB && !defined FRONT_ATMC && !defined FRONT_ATMD && !defined FRONT_ATME && !defined FRONT_SO_ATME && defined FRONT_ATMF && !defined FRONT_H_ATMF )
  use FRONT_ATMF, only: atmSS => SetServices
#elif (!defined FRONT_ATMA && !defined FRONT_ATMB && !defined FRONT_ATMC && !defined FRONT_ATMD && !defined FRONT_ATME && !defined FRONT_SO_ATME && !defined FRONT_ATMF && defined FRONT_H_ATMF )
  use atmF, only: atmSS => FRONT_H_ATMF_SS
#else
#error "Exactly one valid ATM option must be specified!"
#endif

  !- select as many OCN components as specified
#ifdef FRONT_OCNA
  use FRONT_OCNA, only: ocnA_SS => SetServices
#endif
#ifdef FRONT_OCNB
  use FRONT_OCNB, only: ocnB_SS => SetServices
#endif
  
  use NUOPC_Connector, only: cplSS => routine_SetServices
  
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
    
    ! NUOPC_DriverAtmOcn registers the generic methods
    call NUOPC_CompDerive(driver, driver_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! attach specializing method(s)
    call ESMF_MethodAdd(driver, label=driver_label_SetModelCount, &
      userRoutine=SetModelCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_MethodAdd(driver, label=driver_label_SetModelServices, &
      userRoutine=SetModelServices, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelCount(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS

    ! Set model count for two models
    call NUOPC_DriverSet(driver, modelCount=2, rc=rc)
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
    type(driver_type_IS)          :: is
    type(ESMF_Grid)               :: grid
    type(ESMF_Field)              :: field
    type(ESMF_Time)               :: startTime
    type(ESMF_Time)               :: stopTime
    type(ESMF_TimeInterval)       :: timeStep
    type(ESMF_Clock)              :: internalClock
    type(ESMF_Config)             :: config
    character(len=1)              :: ocn_select
    character(len=80)             :: logMsg

    rc = ESMF_SUCCESS

    ! Create and read Config
    config = ESMF_ConfigCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ConfigLoadFile(config, "esm.config", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ConfigGetAttribute(config, value=ocn_select, label="OCN_SELECT:",&
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ConfigDestroy(config, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, driver_label_IS, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! SetServices for ATM
#ifndef ATM_FRONT_SO
    call ESMF_GridCompSetServices(is%wrap%modelComp(1), atmSS, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out
#else
    call NUOPC_GridCompSetServices(is%wrap%modelComp(1), sharedObj="./"//ATM_FRONT_SO, &
      userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out
#endif

    call ESMF_AttributeSet(is%wrap%modelComp(1), name="Verbosity", value="high", &
      convention="NUOPC", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! SetServices for OCN
    if (.false.) then
#ifdef FRONT_OCNA
    elseif (ocn_select=="A") then
      call ESMF_GridCompSetServices(is%wrap%modelComp(2), ocnA_SS, userRc=localrc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) &
        return  ! bail out
#endif
#ifdef FRONT_OCNB
    elseif (ocn_select=="B") then
      call ESMF_GridCompSetServices(is%wrap%modelComp(2), ocnB_SS, userRc=localrc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) &
        return  ! bail out
#endif
#ifdef FRONT_SO_OCNC
    elseif (ocn_select=="C") then
      call NUOPC_GridCompSetServices(is%wrap%modelComp(2), sharedObj="./"//FRONT_SO_OCNC, &
        userRc=localrc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) &
        return  ! bail out
#endif
    else
      write (logMsg,*) "Not a valid ocn_select: ", ocn_select
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg=logMsg, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    endif
    
    call ESMF_AttributeSet(is%wrap%modelComp(2), name="Verbosity", value="high", &
      convention="NUOPC", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! SetServices for atm2ocn
    call ESMF_CplCompSetServices(is%wrap%connectorComp(1,2), cplSS, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out
      
    call ESMF_AttributeSet(is%wrap%connectorComp(1,2), name="Verbosity", value="high", &
      convention="NUOPC", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! SetServices for ocn2atm
    call ESMF_CplCompSetServices(is%wrap%connectorComp(2,1), cplSS, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out
      
    call ESMF_AttributeSet(is%wrap%connectorComp(2,1), name="Verbosity", value="high", &
      convention="NUOPC", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! set the model clock
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

    internalClock = ESMF_ClockCreate(name="Application Clock", &
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
