module NUOPC

  !-----------------------------------------------------------------------------
  ! Generic code collection
  !-----------------------------------------------------------------------------

  use ESMF_Mod

  implicit none
  
  private
  
  public NUOPC_FieldAttributeGet
  public NUOPC_FieldAttributeAdd
  public NUOPC_CplCompAttributeGet
  public NUOPC_CplCompAttributeAdd
  public NUOPC_TimePrint
  public NUOPC_ClockPrintTime
  public NUOPC_ClockInitialize
  public NUOPC_GridCompSetClock
  public NUOPC_GridCompCheckSetClock
  public NUOPC_StateBuildStdList
  public NUOPC_StateIsAllConnected
  public NUOPC_StateSetTimestamp
  public NUOPC_StateAddPotentialField
  public NUOPC_StateReplaceWRealField
  public NUOPC_StateIsCurrentTimestamp
  public NUOPC_FieldBundleUpdateTime
  public NUOPC_GridCreateSimpleXY
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine NUOPC_FieldAttributeGet(field, name, value, rc)
    type(ESMF_Field)                      :: field
    character(*), intent(in)              :: name
    character(*), intent(out)             :: value
    integer,      intent(out), optional   :: rc
    
    ! local variables
    character(ESMF_MAXSTR)  :: defaultvalue
    
    if (present(rc)) rc = ESMF_SUCCESS

    defaultvalue = "CheckThisDefaultValue"

    call ESMF_AttributeGet(field, name=name, value=value, &
      defaultvalue=defaultvalue, convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (trim(value) == trim(defaultvalue)) then
      ! attribute not present
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="Attribute not present",&
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    else if (len_trim(value) == 0) then
      ! attribute present but not set
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="Attribute not set",&
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    endif
    
  end subroutine

  !-----------------------------------------------------------------------------
  
  subroutine NUOPC_FieldAttributeAdd(field, StandardName, Units, Connected, rc)
    type(ESMF_Field)                      :: field
    character(*), intent(in)              :: StandardName
    character(*), intent(in)              :: Units
    character(*), intent(in)              :: Connected
    integer,      intent(out), optional   :: rc

    ! local variables
    character(ESMF_MAXSTR)  :: attrList(2)

    if (present(rc)) rc = ESMF_SUCCESS

    ! Set up a customized list of Attributes to be added to the Fields
    attrList(1) = "Connected"  ! values: "true" or "false"
    attrList(2) = "TimeStamp"  ! values: list of 9 integers: yy,mm,dd,h,m,s,ms,us,ns
    
    ! add Attribute packages
    call ESMF_AttributeAdd(field, convention="ESG", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_AttributeAdd(field, convention="NUOPC", purpose="General",   &
      attrList=attrList, nestConvention="ESG", nestPurpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! set Attributes
    call ESMF_AttributeSet(field, &
      name="StandardName", value=StandardName, &
      convention="ESG", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_AttributeSet(field, &
      name="Units", value=Units, &
      convention="ESG", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_AttributeSet(field, &
      name="Connected", value=Connected, &
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_AttributeSet(field, &
      name="TimeStamp", valueList=(/0,0,0,0,0,0,0,0,0/), &
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  end subroutine

  !-----------------------------------------------------------------------------
  
  subroutine NUOPC_CplCompAttributeGet(comp, cplList, cplListSize, rc)
    type(ESMF_CplComp)                    :: comp
    character(*), intent(inout)           :: cplList(:)
    integer,      intent(inout)           :: cplListSize
    integer,      intent(out), optional   :: rc
    
    ! local variables
    character(ESMF_MAXSTR)  :: defaultvalue
    
    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_AttributeGet(comp, name="CplList", valueList=cplList, &
      itemCount=cplListSize, &
      convention="NUOPC", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine

  !-----------------------------------------------------------------------------
  
  subroutine NUOPC_CplCompAttributeAdd(comp, importState, exportState, rc)
    type(ESMF_CplComp), intent(inout)         :: comp
    type(ESMF_State),   intent(in)            :: importState
    type(ESMF_State),   intent(in)            :: exportState
    integer,            intent(out), optional :: rc
    
    ! local variables
    character(ESMF_MAXSTR)  :: attrList(2)
    integer, parameter      :: maxCount=10
    character(ESMF_MAXSTR)  :: cplListValues(maxCount)
    integer                 :: count

    if (present(rc)) rc = ESMF_SUCCESS
    
    ! Set up a customized list of Attributes to be added to the CplComp
    attrList(1) = "LongName"
    attrList(2) = "CplList"
    
    ! add Attribute packages
    call ESMF_AttributeAdd(comp, convention="NUOPC", purpose="General",   &
      attrList=attrList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! find cplListValues
    call NUOPC_FillCplList(importState, exportState, cplList=cplListValues, &
      count=count, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! set Attributes
    call ESMF_AttributeSet(comp, &
      name="LongName", value="NUOPC Generic Connector Component", &
      convention="NUOPC", purpose="General", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (count>0) then
      call ESMF_AttributeSet(comp, &
        name="CplList", valueList=cplListValues(1:count), &
        convention="NUOPC", purpose="General", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
      
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine NUOPC_FillCplList(importState, exportState, cplList, count, rc)
    type(ESMF_State),       intent(in)            :: importState
    type(ESMF_State),       intent(in)            :: exportState
    character(ESMF_MAXSTR), intent(inout)         :: cplList(:)
    integer,                intent(out)           :: count
    integer,                intent(out), optional :: rc
    
    integer                         :: maxCount, i, j
    character(ESMF_MAXSTR), pointer :: importStandardNameList(:)
    character(ESMF_MAXSTR), pointer :: exportStandardNameList(:)
    
    if (present(rc)) rc = ESMF_SUCCESS

    maxCount = size(cplList)
    count = 0 ! initialize
    
    if (maxCount < 2) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="Not enough space in cplList",&
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    endif
    
    ! fill in the dummy elements as a current work-around for Attr. code issues
    cplList(1) = "dummyEntryWorkaround1" !needed b/c of Attr code issue
    cplList(2) = "dummyEntryWorkaround2" !needed b/c of Attr code issue
    count = 2
    
    ! build list of standard names of all Fields inside of importState
    call NUOPC_StateBuildStdList(importState, importStandardNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_StateBuildStdList(exportState, exportStandardNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! simple linear search of items that match between both lists
    do i=1, size(importStandardNameList)
      do j=1, size(exportStandardNameList)
        if (importStandardNameList(i) == exportStandardNameList(j)) then
          count = count+1
          if (count > maxCount) then
            call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
              msg="Not enough space in cplList",&
              line=__LINE__, &
              file=__FILE__, &
              rcToReturn=rc)
              return  ! bail out
          endif
          cplList(count) = importStandardNameList(i)
          exit
        endif
      enddo
    enddo
      
    if (associated(importStandardNameList)) deallocate(importStandardNameList)
    if (associated(exportStandardNameList)) deallocate(exportStandardNameList)
    
  end subroutine

  !-----------------------------------------------------------------------------
  
  subroutine NUOPC_TimePrint(time, string, rc)
    type(ESMF_Time)                               :: time
    character(*),           intent(in),  optional :: string
    integer,                intent(out), optional :: rc
    
    integer                 :: yy, mm, dd, h, m, s, ms
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_TimeGet(time, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, ms=ms, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  
    if (present(string)) then
      write (*, "(A, I4, I3, I3, I3, I3, I3, I4)") string, &
        yy, mm, dd, h, m, s, ms
    else
      write (*, "(I4, I3, I3, I3, I3, I3, I4)") &
        yy, mm, dd, h, m, s, ms
    endif
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine NUOPC_ClockPrintTime(clock, string, rc)
    type(ESMF_Clock)                              :: clock
    character(*),           intent(in),  optional :: string
    integer,                intent(out), optional :: rc
    
    type(ESMF_Time)         :: currTime
    if (present(rc)) rc = ESMF_SUCCESS
  
    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call NUOPC_TimePrint(currTime, string, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  end subroutine

  !-----------------------------------------------------------------------------

  function NUOPC_ClockInitialize(externalClock, stabilityTimeStep, rc)
    type(ESMF_Clock) :: NUOPC_ClockInitialize
    type(ESMF_Clock)                              :: externalClock
    type(ESMF_TimeInterval)                       :: stabilityTimeStep
    integer,                intent(out), optional :: rc
    
    type(ESMF_Clock)        :: internalClock
    type(ESMF_TimeInterval) :: externalTimeStep
    type(ESMF_TimeInterval) :: actualTimeStep
    integer                 :: internalStepCount
    
    if (present(rc)) rc = ESMF_SUCCESS
    
      ! make a copy of the external externalClock
    internalClock = ESMF_ClockCreate(externalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! determine the internal timeStep
    ! The external (parent) timeStep must be a multiple of the internal
    ! timeStep. At the same time there is typically a physical/stability limit
    ! for the internal timeStep. The following procedure finds an internal
    ! timeStep that is as close as possible to the provided stability limit, 
    ! while <= that limit. At the same time the external timeStep is a multiple
    ! of the internal timeStep.
    call ESMF_ClockGet(externalClock, timeStep=externalTimeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    internalStepCount = ceiling(externalTimeStep / stabilityTimeStep)
    actualTimeStep = externalTimeStep / internalStepCount
    
    call ESMF_ClockSet(internalClock, timeStep=actualTimeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    NUOPC_ClockInitialize = internalClock
  end function

  !-----------------------------------------------------------------------------

  subroutine NUOPC_GridCompSetClock(comp, externalClock, stabilityTimeStep, rc)
    type(ESMF_GridComp),     intent(inout)         :: comp
    type(ESMF_Clock),        intent(in)            :: externalClock
    type(ESMF_TimeInterval), intent(in)            :: stabilityTimeStep
    integer,                 intent(out), optional :: rc
    
    ! local variables
    type(ESMF_Clock)        :: internalClock

    if (present(rc)) rc = ESMF_SUCCESS
    
    internalClock = NUOPC_ClockInitialize(externalClock, stabilityTimeStep, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_GridCompSet(comp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  end subroutine
  
  !-----------------------------------------------------------------------------
  
  subroutine NUOPC_GridCompCheckSetClock(comp, externalClock, rc)
    type(ESMF_GridComp),     intent(inout)         :: comp
    type(ESMF_Clock),        intent(in)            :: externalClock
    integer,                 intent(out), optional :: rc
  
    ! local variables    
    type(ESMF_Clock)        :: internalClock
    type(ESMF_Time)         :: externalCurrTime, currTime, stopTime
    type(ESMF_TimeInterval) :: externalTimeStep, timeStep
    type(ESMF_Direction)    :: direction

    if (present(rc)) rc = ESMF_SUCCESS
    
    ! compare external and internal Clocks for consistency
    call ESMF_ClockGet(externalClock, currTime=externalCurrTime, &
      timeStep=externalTimeStep, direction=direction, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_GridCompGet(comp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_ClockGet(internalClock, currTime=currTime, timeStep=timeStep, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! ensure the current times match between external and internal Clock
    if (currTime /= externalCurrTime) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="internal and external Clocks do not match in current time!", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    endif
    
    ! ensure that the external timestep is still a multiple of the internal one
    if (ceiling(externalTimeStep/timeStep) /= floor(externalTimeStep/timeStep))&
      then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="external timestep is not multiple of internal timestep!", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    endif
    
    ! set the new stopTime of the internalClock
    if (direction==ESMF_MODE_FORWARD) then
      stopTime = currTime + externalTimeStep
    else
      stopTime = currTime - externalTimeStep
    endif
    call ESMF_ClockSet(internalClock, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine NUOPC_StateBuildStdList(state, stdAttrNameList, stdItemNameList, &
    stdConnectedList, rc)
    type(ESMF_State),       intent(in)            :: state
    character(ESMF_MAXSTR), pointer               :: stdAttrNameList(:)
    character(ESMF_MAXSTR), pointer, optional     :: stdItemNameList(:)
    character(ESMF_MAXSTR), pointer, optional     :: stdConnectedList(:)
    integer,                intent(out), optional :: rc
    
    integer           :: item, itemCount, fieldCount, stat
    type(ESMF_Field)  :: field
    character(ESMF_MAXSTR), allocatable   :: itemNameList(:)
    type(ESMF_StateItemType), allocatable :: stateitemtypeList(:)
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_StateGet(state, itemCount=itemCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
          
    if (itemCount > 0) then
      allocate(itemNameList(itemCount))
      allocate(stateitemtypeList(itemCount))
      call ESMF_StateGet(state, itemNameList=itemNameList, &
        itemtypeList=stateitemtypeList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
        
      fieldCount = 0  ! reset
      do item=1, itemCount
        if (stateitemtypeList(item) == ESMF_STATEITEM_FIELD) &
          fieldCount = fieldCount + 1
      enddo
      
      allocate(stdAttrNameList(fieldCount), stat=stat)
      if (ESMF_LogFoundAllocError(stat, msg= "allocating stdAttrNameList", &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      if (present(stdItemNameList)) then
        allocate(stdItemNameList(fieldCount), stat=stat)
        if (ESMF_LogFoundAllocError(stat, msg= "allocating stdItemNameList", &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif

      if (present(stdConnectedList)) then
        allocate(stdConnectedList(fieldCount), stat=stat)
        if (ESMF_LogFoundAllocError(stat, msg= "allocating stdConnectedList", &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif

      fieldCount = 1  ! reset

      do item=1, itemCount
        if (stateitemtypeList(item) == ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(state, itemName=itemNameList(item), &
            field=field, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          call NUOPC_FieldAttributeGet(field, name="StandardName", &
            value=stdAttrNameList(fieldCount), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          if (present(stdItemNameList)) then
            stdItemNameList(fieldCount)=itemNameList(item)
          endif
          if (present(stdConnectedList)) then
            call NUOPC_FieldAttributeGet(field, name="Connected", &
              value=stdConnectedList(fieldCount), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
          endif
          fieldCount = fieldCount + 1
        endif
      enddo
        
      deallocate(itemNameList)
      deallocate(stateitemtypeList)
    endif
    
  end subroutine
  
  !-----------------------------------------------------------------------------
  
  function NUOPC_StateIsAllConnected(state, rc)
    logical :: NUOPC_StateIsAllConnected
    type(ESMF_State)                          :: state
    integer,            intent(out), optional :: rc
  
    character(ESMF_MAXSTR), pointer           :: stdAttrNameList(:)
    character(ESMF_MAXSTR), pointer           :: stdConnectedList(:)
    logical                                   :: allConnected
    integer                                   :: i

    if (present(rc)) rc = ESMF_SUCCESS
    
    call NUOPC_StateBuildStdList(state, stdAttrNameList=stdAttrNameList, &
      stdConnectedList=stdConnectedList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    allConnected = .true.  ! initialize
    do i=1, size(stdConnectedList)
      if (stdConnectedList(i) /= "true") then
        allConnected = .false.
        exit
      endif
    enddo

    if (associated(stdAttrNameList)) deallocate(stdAttrNameList)
    if (associated(stdConnectedList)) deallocate(stdConnectedList)

    NUOPC_StateIsAllConnected = allConnected

  end function
  
  !-----------------------------------------------------------------------------
  
  subroutine NUOPC_StateSetTimestamp(state, clock, rc)
    type(ESMF_State),        intent(inout)         :: state
    type(ESMF_Clock),        intent(in)            :: clock
    integer,                 intent(out), optional :: rc
  
    ! local variables    
    character(ESMF_MAXSTR), pointer       :: stdAttrNameList(:)
    character(ESMF_MAXSTR), pointer       :: stdItemNameList(:)
    type(ESMF_Field)                      :: field
    type(ESMF_Time)         :: time
    integer                 :: yy, mm, dd, h, m, s, ms, us, ns
    integer                 :: i
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_ClockGet(clock, currTime=time, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_TimeGet(time, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, ms=ms, us=us, &
      ns=ns, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  
    call NUOPC_StateBuildStdList(state, stdAttrNameList=stdAttrNameList, &
      stdItemNameList=stdItemNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    do i=1, size(stdItemNameList)
      call ESMF_StateGet(state, field=field, itemName=stdItemNameList(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_AttributeSet(field, &
        name="TimeStamp", valueList=(/yy,mm,dd,h,m,s,ms,us,ns/), &
        convention="NUOPC", purpose="General", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo
    
    if (associated(stdAttrNameList)) deallocate(stdAttrNameList)
    if (associated(stdItemNameList)) deallocate(stdItemNameList)
    
  end subroutine

  !-----------------------------------------------------------------------------
  
  subroutine NUOPC_StateAddPotentialField(state, name, StandardName, Units, rc)
    type(ESMF_State), intent(inout)         :: state
    character(*),     intent(in)            :: name
    character(*),     intent(in)            :: StandardName
    character(*),     intent(in)            :: Units
    integer,          intent(out), optional :: rc
  
    ! local variables    
    type(ESMF_Field)        :: field
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    field = ESMF_FieldCreateEmpty(name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_FieldAttributeAdd(field, StandardName=StandardName, &
      Units=Units, Connected="false", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_StateAdd(state, field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine NUOPC_StateReplaceWRealField(state, field, rc)
    type(ESMF_State), intent(inout)         :: state
    type(ESMF_Field), intent(in)            :: field
    integer,          intent(out), optional :: rc
  
    ! local variables
    type(ESMF_Field)        :: potentialField
    character(ESMF_MAXSTR)  :: name
    character(ESMF_MAXSTR)  :: StandardName
    character(ESMF_MAXSTR)  :: Units
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_FieldGet(field, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_StateGet(state, field=potentialField, itemName=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_FieldAttributeGet(potentialField, name="StandardName", &
      value=StandardName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_FieldAttributeGet(potentialField, name="Units", &
      value=Units, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_FieldAttributeAdd(field, StandardName=StandardName,&
      Units=Units, Connected="false", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_StateReplace(state, field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine

  !-----------------------------------------------------------------------------

  function NUOPC_StateIsCurrentTimestamp(state, clock, rc)
    logical :: NUOPC_StateIsCurrentTimestamp
    type(ESMF_State),        intent(inout)         :: state
    type(ESMF_Clock),        intent(in)            :: clock
    integer,                 intent(out), optional :: rc
  
    ! local variables    
    character(ESMF_MAXSTR), pointer       :: stdAttrNameList(:)
    character(ESMF_MAXSTR), pointer       :: stdItemNameList(:)
    type(ESMF_Field)                      :: field
    type(ESMF_Time)         :: time, fieldTime
    integer                 :: i, valueList(9)
    logical                 :: isCurrent
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_ClockGet(clock, currTime=time, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateBuildStdList(state, stdAttrNameList=stdAttrNameList, &
      stdItemNameList=stdItemNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    isCurrent = .true. ! initialize
    
    do i=1, size(stdItemNameList)
      call ESMF_StateGet(state, field=field, itemName=stdItemNameList(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_AttributeGet(field, &
        name="TimeStamp", valueList=valueList, &
        convention="NUOPC", purpose="General", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_TimeSet(fieldTime, &
        yy=valueList(1), mm=ValueList(2), dd=ValueList(3), &
         h=valueList(4),  m=ValueList(5),  s=ValueList(6), &
        ms=valueList(7), us=ValueList(8), ns=ValueList(9), &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if (fieldTime /= time) then
        isCurrent = .false.
        exit
      endif
    enddo
    
    if (associated(stdAttrNameList)) deallocate(stdAttrNameList)
    if (associated(stdItemNameList)) deallocate(stdItemNameList)
    
    NUOPC_StateIsCurrentTimestamp = isCurrent
    
  end function

  !-----------------------------------------------------------------------------

  subroutine NUOPC_FieldBundleUpdateTime(srcFields, dstFields, rc)
    type(ESMF_FieldBundle),  intent(inout)         :: srcFields
    type(ESMF_FieldBundle),  intent(inout)         :: dstFields
    integer,                 intent(out), optional :: rc
  
    ! local variables    
    character(ESMF_MAXSTR), pointer       :: stdAttrNameList(:)
    character(ESMF_MAXSTR), pointer       :: stdItemNameList(:)
    type(ESMF_Field)        :: srcField, dstField
    integer                 :: i, valueList(9), srcCount, dstCount
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_FieldBundleGet(srcFields, fieldCount=srcCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldBundleGet(dstFields, fieldCount=dstCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (srcCount /= dstCount) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg="count mismatch",&
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    endif

    do i=1, srcCount    
      call ESMF_FieldBundleGet(srcFields, fieldIndex=i, field=srcField, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_FieldBundleGet(dstFields, fieldIndex=i, field=dstField, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_AttributeGet(srcField, &
        name="TimeStamp", valueList=valueList, &
        convention="NUOPC", purpose="General", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_AttributeSet(dstField, &
        name="TimeStamp", valueList=valueList, &
        convention="NUOPC", purpose="General", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo
    
  end subroutine
    
  !-----------------------------------------------------------------------------

  function NUOPC_GridCreateSimpleXY(x_min, y_min, x_max, y_max, &
    i_count, j_count, rc)
    type(ESMF_Grid):: NUOPC_GridCreateSimpleXY
    real(ESMF_KIND_R8), intent(in)            :: x_min, x_max, y_min, y_max
    integer,            intent(in)            :: i_count, j_count
    integer,            intent(out), optional :: rc
  
    integer :: i, j, imin_t, imax_t, jmin_t, jmax_t
    real(ESMF_KIND_R8), pointer :: CoordX(:), CoordY(:)
    real(ESMF_KIND_R8):: dx, dy
    type(ESMF_Grid):: grid
    
    if (present(rc)) rc = ESMF_SUCCESS

    dx = (x_max-x_min)/i_count
    dy = (y_max-y_min)/j_count

    grid = ESMF_GridCreateShapeTile(maxIndex=(/i_count,j_count/), &
      coordDep1=(/1/), coordDep2=(/2/), &
      gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
      indexflag=ESMF_INDEX_GLOBAL, name="SimpleXY", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! add center stagger
    call ESMF_GridAddCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridGetCoord(grid, localDE=0, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, &
      coordDim=1, farrayPtr=coordX, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridGetCoord(grid, localDE=0, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, &
      coordDim=2, farrayPtr=coordY, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! compute center stagger coordinate values
    imin_t = lbound(CoordX,1)
    imax_t = ubound(CoordX,1)
    jmin_t = lbound(CoordY,1)
    jmax_t = ubound(CoordY,1)
      
    coordX(imin_t) = (imin_t-1)*dx + 0.5*dx
    do i = imin_t+1, imax_t
      coordX(i) = coordX(i-1) + dx
    enddo
    coordY(jmin_t) = (jmin_t-1)*dy + 0.5*dy
    do j = jmin_t+1, jmax_t
      coordY(j) = coordY(j-1) + dy
    enddo
    
    NUOPC_GridCreateSimpleXY = grid
    
  end function

end module


!=== Model Component with Explicit integraton ==================================

module NUOPC_ModelExplicit

  !-----------------------------------------------------------------------------
  ! Generic Model Component for explicit integration
  !-----------------------------------------------------------------------------

  use ESMF_Mod
  use NUOPC

  implicit none
  
  private
  
  public SetServices
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETINIT, &
      userRoutine=InitializeP2, phase=2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETINIT, &
      userRoutine=InitializeP3, phase=3, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETRUN, &
      userRoutine=Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETFINAL, &
      userRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP2(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables    
    logical                 :: allConnected
        
    rc = ESMF_SUCCESS

    ! query if all import Fields are connected
    allConnected = NUOPC_StateIsAllConnected(importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! compatibility check
    if (.not.allConnected) then
      !TODO: introduce and use INCOMPATIBILITY return codes!!!!
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="NUOPC INCOMPATIBILITY DETECTED: Import Fields not all connected", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    endif
    
    !TODO: remove Fields that aren't connected from import and export State
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP3(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables    
    type(ESMF_Clock)  :: internalClock
    logical           :: allConnected
        
    rc = ESMF_SUCCESS
    
    !TODO: fill all export Fields with valid initial data for current time
    ! note that only connected Fields reside in exportState at this time
    
    ! update timestamp on export Fields
    call ESMF_GridCompGet(gcomp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_StateSetTimestamp(exportState, internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine Run(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    integer                 :: localrc
    type(ESMF_Clock)        :: internalClock
    logical                 :: allCurrent
    character(ESMF_MAXSTR)  :: modelName

    rc = ESMF_SUCCESS
    
    call ESMF_GridCompGet(gcomp, name=modelName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! check and set the internal clock against the external clock
    call NUOPC_GridCompCheckSetClock(gcomp, clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! get the internal clock for the time stepping loop
    call ESMF_GridCompGet(gcomp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_ClockPrintTime(internalClock, ">>>"// &
      trim(modelName)//" entered Run with current time: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! check that Fields in the importState show correct timestamp
    allCurrent = NUOPC_StateIsCurrentTimestamp(importState, internalClock, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    if (.not.allCurrent) then
      !TODO: introduce and use INCOMPATIBILITY return codes!!!!
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="NUOPC INCOMPATIBILITY DETECTED: Import Fields not at current time", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    endif
    
    ! model time stepping loop
    do while (.not. ESMF_ClockIsStopTime(internalClock, rc=rc))
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
        
      ! SPECIALIZE by calling into attached method to advance the model t->t+dt
      call ESMF_MethodExecute(gcomp, label="ModelExplicit_Advance", &
        userRc=localrc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRPASS, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOG_ERRPASS, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) &
        return  ! bail out
      
      ! advance the internalClock to the new current time
      call ESMF_ClockAdvance(internalClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    
      call NUOPC_ClockPrintTime(internalClock, &
        trim(modelName)//" time stepping loop, current time: ", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
        
    enddo ! end of time stepping loop
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! update timestamp on export Fields
    call NUOPC_StateSetTimestamp(exportState, internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine Finalize(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS
    
  end subroutine

end module


!=== Explicit Coupling Driver ==================================================

module NUOPC_DriverExplicit

  !-----------------------------------------------------------------------------
  ! Generic Driver Component with explicit time stepping
  !-----------------------------------------------------------------------------

  use ESMF_Mod
  use NUOPC

  implicit none
  
  private
  
  public SetServices, InternalState
  
  type InternalStateStruct
    integer                       :: modelCount
    type(ESMF_GridComp), pointer  :: modelComp(:)
    type(ESMF_State),    pointer  :: modelIS(:), modelES(:)
    type(ESMF_CplComp),  pointer  :: connectorComp(:,:)
  end type

  type InternalState
    type(InternalStateStruct), pointer :: wrap
  end type

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETINIT, &
      userRoutine=Initialize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETRUN, &
      userRoutine=Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETFINAL, &
      userRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine Initialize(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    integer                 :: localrc, stat
    type(InternalState)     :: is
    type(ESMF_Clock)        :: internalClock
    integer                 :: i, j
    character(ESMF_MAXSTR)  :: iString, jString

    rc = ESMF_SUCCESS
    
    ! allocate memory for the internal state and set it in the Component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state memory failed.", &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out
    call ESMF_UserCompSetInternalState(gcomp, "NUOPC_DriverExplicit", is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! SPECIALIZE by calling into attached method to set modelCount
    call ESMF_MethodExecute(gcomp, label="DriverExplicit_SetModelCount", &
      userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRPASS, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOG_ERRPASS, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out

    ! allocate lists inside the internal state according to modelCount
    allocate(is%wrap%modelComp(is%wrap%modelCount), &
      is%wrap%modelIS(is%wrap%modelCount), is%wrap%modelES(is%wrap%modelCount),&
      is%wrap%connectorComp(is%wrap%modelCount,is%wrap%modelCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state memory failed.", &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out

    ! create modelComps and their import and export States + connectorComps
    do i=1, is%wrap%modelCount
      !TODO: there should be petList members in the internal State that 
      !TODO: can be specialized and would be used here
      write (iString, *) i
      is%wrap%modelComp(i) = ESMF_GridCompCreate(name="modelComp "//&
        trim(adjustl(iString)), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      is%wrap%modelIS(i) = ESMF_StateCreate(name="modelComp "//&
        trim(adjustl(iString))//" Import State", &
        statetype=ESMF_STATE_IMPORT, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      is%wrap%modelES(i) = ESMF_StateCreate(name="modelComp "//&
        trim(adjustl(iString))//" Export State", &
        statetype=ESMF_STATE_EXPORT, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
        
      do j=1, is%wrap%modelCount
        if (j==i) cycle ! skip self connection
        write (jString, *) j
        is%wrap%connectorComp(i,j) = ESMF_CplCompCreate(name="connectorComp "//&
          trim(adjustl(iString))//" -> "//trim(adjustl(jString)), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      enddo
    enddo
    
    ! SPECIALIZE by calling into attached method to SetServices for modelComps
    call ESMF_MethodExecute(gcomp, label="DriverExplicit_SetModelServices", &
      userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRPASS, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOG_ERRPASS, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out
      
    ! query Component for its Clock (set during specialization)
    call ESMF_GridCompGet(gcomp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! InitP0: modelComps
    do i=1, is%wrap%modelCount
      write (iString, *) i
      call ESMF_GridCompInitialize(is%wrap%modelComp(i), &
        importState=is%wrap%modelIS(i), exportState=is%wrap%modelES(i), &
        clock=internalClock, phase=0, userRc=localrc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase 0 "// &
        "Initialize for modelComp "//trim(adjustl(iString)), &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase 0 "//&
        "Initialize for modelComp "//trim(adjustl(iString))//" did not "// &
        "return ESMF_SUCCESS", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) &
        return  ! bail out
    enddo
    
    ! InitP0: connectorComps
    do i=1, is%wrap%modelCount
      write (iString, *) i
      do j=1, is%wrap%modelCount
        if (j==i) cycle ! skip self connection
        write (jString, *) j
        call ESMF_CplCompInitialize(is%wrap%connectorComp(i,j), &
          importState=is%wrap%modelES(i), exportState=is%wrap%modelIS(j), &
          clock=internalClock, phase=0, userRc=localrc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase 0 "// &
          "Initialize for connectorComp "// &
          trim(adjustl(iString))//" -> "//trim(adjustl(jString)), &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase 0 "//&
          "Initialize for connectorComp "// &
          trim(adjustl(iString))//" -> "//trim(adjustl(jString))//" did not "// &
          "return ESMF_SUCCESS", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) &
          return  ! bail out
      enddo
    enddo

    ! InitP1: modelComps
    do i=1, is%wrap%modelCount
      write (iString, *) i
      call ESMF_GridCompInitialize(is%wrap%modelComp(i), &
        importState=is%wrap%modelIS(i), exportState=is%wrap%modelES(i), &
        clock=internalClock, phase=1, userRc=localrc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase 1 "// &
        "Initialize for modelComp "//trim(adjustl(iString)), &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase 1 "//&
        "Initialize for modelComp "//trim(adjustl(iString))//" did not "// &
        "return ESMF_SUCCESS", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) &
        return  ! bail out
    enddo
    
    ! InitP1: connectorComps
    do i=1, is%wrap%modelCount
      write (iString, *) i
      do j=1, is%wrap%modelCount
        if (j==i) cycle ! skip self connection
        write (jString, *) j
        call ESMF_CplCompInitialize(is%wrap%connectorComp(i,j), &
          importState=is%wrap%modelES(i), exportState=is%wrap%modelIS(j), &
          clock=internalClock, phase=1, userRc=localrc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase 1 "// &
          "Initialize for connectorComp "// &
          trim(adjustl(iString))//" -> "//trim(adjustl(jString)), &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase 1 "//&
          "Initialize for connectorComp "// &
          trim(adjustl(iString))//" -> "//trim(adjustl(jString))//" did not "// &
          "return ESMF_SUCCESS", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) &
          return  ! bail out
      enddo
    enddo

    ! InitP2: modelComps
    do i=1, is%wrap%modelCount
      write (iString, *) i
      call ESMF_GridCompInitialize(is%wrap%modelComp(i), &
        importState=is%wrap%modelIS(i), exportState=is%wrap%modelES(i), &
        clock=internalClock, phase=2, userRc=localrc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase 2 "// &
        "Initialize for modelComp "//trim(adjustl(iString)), &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase 2 "//&
        "Initialize for modelComp "//trim(adjustl(iString))//" did not "// &
        "return ESMF_SUCCESS", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) &
        return  ! bail out
    enddo
    
    ! InitP3: modelComps
    do i=1, is%wrap%modelCount
      write (iString, *) i
      call ESMF_GridCompInitialize(is%wrap%modelComp(i), &
        importState=is%wrap%modelIS(i), exportState=is%wrap%modelES(i), &
        clock=internalClock, phase=3, userRc=localrc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase 3 "// &
        "Initialize for modelComp "//trim(adjustl(iString)), &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase 3 "//&
        "Initialize for modelComp "//trim(adjustl(iString))//" did not "// &
        "return ESMF_SUCCESS", &
        file=__FILE__, &
        rcToReturn=rc)) &
        return  ! bail out
    enddo
        
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine Run(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer                 :: localrc
    type(InternalState)     :: is
    type(ESMF_Clock)        :: internalClock
    integer                 :: i, j
    character(ESMF_MAXSTR)  :: iString, jString

    rc = ESMF_SUCCESS
    
    ! query Component for its Clock
    call ESMF_GridCompGet(gcomp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! query Component for this internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, "NUOPC_DriverExplicit", is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! time stepping loop
    do while (.not. ESMF_ClockIsStopTime(internalClock, rc=rc))
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! Run: connectorComps
      do i=1, is%wrap%modelCount
        write (iString, *) i
        do j=1, is%wrap%modelCount
          if (j==i) cycle ! skip self connection
          write (jString, *) j
          call ESMF_CplCompRun(is%wrap%connectorComp(i,j), &
            importState=is%wrap%modelES(i), exportState=is%wrap%modelIS(j), &
            clock=internalClock, phase=1, userRc=localrc, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase 1 "// &
            "Run for connectorComp "// &
            trim(adjustl(iString))//" -> "//trim(adjustl(jString)), &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase 1 "//&
            "Run for connectorComp "// &
            trim(adjustl(iString))//" -> "//trim(adjustl(jString))//" did not "// &
            "return ESMF_SUCCESS", &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)) &
            return  ! bail out
        enddo
      enddo

      ! Run: modelComps
      do i=1, is%wrap%modelCount
        write (iString, *) i
        call ESMF_GridCompRun(is%wrap%modelComp(i), &
          importState=is%wrap%modelIS(i), exportState=is%wrap%modelES(i), &
          clock=internalClock, phase=1, userRc=localrc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase 1 "// &
          "Run for modelComp "//trim(adjustl(iString)), &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase 1 "//&
          "Run for modelComp "//trim(adjustl(iString))//" did not "// &
          "return ESMF_SUCCESS", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) &
          return  ! bail out
      enddo
    
      ! advance to next time step
      call ESMF_ClockAdvance(internalClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    
    enddo ! end of time stepping loop
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine Finalize(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer                 :: localrc, stat
    type(InternalState)     :: is
    type(ESMF_Clock)        :: internalClock
    integer                 :: i, j
    character(ESMF_MAXSTR)  :: iString, jString
    logical                 :: existflag

    rc = ESMF_SUCCESS
    
    ! SPECIALIZE by calling into optional attached method
    call ESMF_MethodExecute(gcomp, label="DriverExplicit_Finalize", &
      existflag=existflag, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRPASS, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOG_ERRPASS, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out

    ! query Component for its Clock
    call ESMF_GridCompGet(gcomp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! query Component for this internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, "NUOPC_DriverExplicit", is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! Finalize: connectorComps
    do i=1, is%wrap%modelCount
      write (iString, *) i
      do j=1, is%wrap%modelCount
        if (j==i) cycle ! skip self connection
        write (jString, *) j
        call ESMF_CplCompFinalize(is%wrap%connectorComp(i,j), &
          importState=is%wrap%modelES(i), exportState=is%wrap%modelIS(j), &
          clock=internalClock, phase=1, userRc=localrc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase 1 "// &
          "Finalize for connectorComp "// &
          trim(adjustl(iString))//" -> "//trim(adjustl(jString)), &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase 1 "//&
          "Finalize for connectorComp "// &
          trim(adjustl(iString))//" -> "//trim(adjustl(jString))//" did not "// &
          "return ESMF_SUCCESS", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) &
          return  ! bail out
      enddo
    enddo

    ! Finalize: modelComps
    do i=1, is%wrap%modelCount
      write (iString, *) i
      call ESMF_GridCompFinalize(is%wrap%modelComp(i), &
        importState=is%wrap%modelIS(i), exportState=is%wrap%modelES(i), &
        clock=internalClock, phase=1, userRc=localrc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed calling phase 1 "// &
        "Finalize for modelComp "//trim(adjustl(iString)), &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if (ESMF_LogFoundError(rcToCheck=localrc, msg="Phase 1 "//&
        "Finalize for modelComp "//trim(adjustl(iString))//" did not "// &
        "return ESMF_SUCCESS", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) &
        return  ! bail out
    enddo
    
    ! destroy modelComps and their import and export States + connectorComps
    do i=1, is%wrap%modelCount
      write (iString, *) i
      call ESMF_GridCompDestroy(is%wrap%modelComp(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_StateDestroy(is%wrap%modelIS(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_StateDestroy(is%wrap%modelES(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      do j=1, is%wrap%modelCount
        if (j==i) cycle ! skip self connection
        write (jString, *) j
        call ESMF_CplCompDestroy(is%wrap%connectorComp(i,j), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      enddo
    enddo

    ! deallocate lists inside the internal state
    deallocate(is%wrap%modelComp, is%wrap%modelIS, is%wrap%modelES, &
      is%wrap%connectorComp, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out

    ! deallocate internal state memory
    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out
      
  end subroutine

end module


!=== Explicit ATM-OCN Coupling Driver ==========================================

module NUOPC_DriverExplicitAtmOcn

  !-----------------------------------------------------------------------------
  ! Generic Driver Component for ATM and OCN with explicit time stepping
  !-----------------------------------------------------------------------------

  use ESMF_Mod
  use NUOPC
  use NUOPC_DriverExplicit, only: &
    DriverExplicit_SS => SetServices, &
    DriverExplicit_IS => InternalState

  implicit none
  
  private
  
  public SetServices, InternalState
  
  type InternalStateStruct
    type(ESMF_GridComp) :: atm
    type(ESMF_State)    :: atmIS, atmES
    type(ESMF_GridComp) :: ocn
    type(ESMF_State)    :: ocnIS, ocnES
    type(ESMF_CplComp)  :: atm2ocn, ocn2atm
  end type

  type InternalState
    type(InternalStateStruct), pointer :: wrap
  end type

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    ! NUOPC_DriverExplicit registers the generic methods
    call DriverExplicit_SS(gcomp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! attach specializing method(s)
    call ESMF_MethodAdd(gcomp, label="DriverExplicit_SetModelCount", &
      userRoutine=SetModelCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_MethodAdd(gcomp, label="DriverExplicit_SetModelServices", &
      userRoutine=SetModelServices, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_MethodAdd(gcomp, label="DriverExplicit_Finalize", &
      userRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------
  
  subroutine SetModelCount(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(DriverExplicit_IS)  :: superIS

    rc = ESMF_SUCCESS
    
    ! query Component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(gcomp, "NUOPC_DriverExplicit", superIS, &
      rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! set the modelCount for ATM-OCN pair coupling
    superIS%wrap%modelCount = 2
      
  end subroutine
  
  !-----------------------------------------------------------------------------
  
  subroutine SetModelServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    integer                 :: localrc, stat
    type(DriverExplicit_IS) :: superIS
    type(InternalState)     :: is

    rc = ESMF_SUCCESS
    
    ! query Component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(gcomp, "NUOPC_DriverExplicit", superIS, &
      rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! allocate memory for this internal state and set it in the Component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state memory failed.", &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out
    call ESMF_UserCompSetInternalState(gcomp, "DriverExplicitAtmOcn", &
      is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! map components and states for ATM-OCN pair coupling
    is%wrap%atm = superIS%wrap%modelComp(1)
    is%wrap%atmIS = superIS%wrap%modelIS(1)
    is%wrap%atmES = superIS%wrap%modelES(1)
    is%wrap%ocn = superIS%wrap%modelComp(2)
    is%wrap%ocnIS = superIS%wrap%modelIS(2)
    is%wrap%ocnES = superIS%wrap%modelES(2)
    is%wrap%atm2ocn = superIS%wrap%connectorComp(1,2)
    is%wrap%ocn2atm = superIS%wrap%connectorComp(2,1)
    
    ! maybe too much? but maybe nice to have the component names specified?
    call ESMF_GridCompSet(is%wrap%atm, name="ATM", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRPASS, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridCompSet(is%wrap%ocn, name="OCN", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRPASS, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_CplCompSet(is%wrap%atm2ocn, name="ATM2OCN", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRPASS, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_CplCompSet(is%wrap%ocn2atm, name="OCN2ATM", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRPASS, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! SPECIALIZE by calling into attached method to SetServices for modelComps
    call ESMF_MethodExecute(gcomp, &
      label="DriverExplicitAtmOcn_SetModelServices", &
      userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRPASS, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOG_ERRPASS, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out
      
  end subroutine
    
  !-----------------------------------------------------------------------------
  
  subroutine Finalize(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    integer                 :: localrc, stat
    type(InternalState)     :: is
    logical                 :: existflag

    rc = ESMF_SUCCESS
    
    ! SPECIALIZE by calling into optional attached method
    call ESMF_MethodExecute(gcomp, label="DriverExplicitAtmOcn_Finalize", &
      existflag=existflag, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRPASS, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOG_ERRPASS, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out

    ! query Component for this internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, "DriverExplicitAtmOcn", is, &
      rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! deallocate internal state memory
    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out
      
  end subroutine
      
end module


!=== Generic Connector Component ===============================================

module NUOPC_Connector

  !-----------------------------------------------------------------------------
  ! Generic Coupler Component.
  !-----------------------------------------------------------------------------

  use ESMF_Mod
  use NUOPC

  implicit none
  
  private
  
  public SetServices
  
  type internalState
    type(ESMF_FieldBundle)  :: srcFields
    type(ESMF_FieldBundle)  :: dstFields
    type(ESMF_RouteHandle)  :: rh
  end type

  type internalStateWrap
    type(internalState), pointer :: wrap
  end type

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine SetServices(cplcomp, rc)
    type(ESMF_CplComp)   :: cplcomp
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_SETINIT, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_SETINIT, &
      userRoutine=InitializeP1, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_SETRUN, &
      userRoutine=Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_SETFINAL, &
      userRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP0(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_StateType)                  :: isType, esType
    integer                               :: isItemCount, esItemCount
    type(ESMF_VM)                         :: vm
    
    rc = ESMF_SUCCESS
    
    ! get current VM because StateReconcile needs it
    !TODO: StateReconcile should have VM optional and this is obsolete
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! reconcile the States
    call ESMF_StateReconcile(importState, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_StateReconcile(exportState, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! access the state types
    call ESMF_StateGet(importState, statetype=isType, itemCount=isItemCount, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_StateGet(exportState, statetype=esType, itemCount=esItemCount, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    if (.not.((isType==ESMF_STATE_EXPORT).and.(esType==ESMF_STATE_IMPORT))) then
      ! not ES -> IS ==> should indicate problem???
    endif
    
    ! look for matching Fields and add them to the CPL component metadata
    call NUOPC_CplCompAttributeAdd(cplcomp, importState, exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP1(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_StateType)                  :: isType, esType
    integer                               :: isItemCount, esItemCount
    character(ESMF_MAXSTR)                :: cplList(100) !TODO make dynamic
    integer                               :: cplListSize, i, j
    character(ESMF_MAXSTR), pointer       :: importStdAttrNameList(:)
    character(ESMF_MAXSTR), pointer       :: importStdItemNameList(:)
    character(ESMF_MAXSTR), pointer       :: exportStdAttrNameList(:)
    character(ESMF_MAXSTR), pointer       :: exportStdItemNameList(:)
    integer                               :: iMatch, eMatch
    type(ESMF_Field)                      :: iField, eField
    type(ESMF_VM)                         :: vm
    integer                               :: stat
    type(internalStateWrap)               :: isw
    
    rc = ESMF_SUCCESS
    
    ! allocate memory for the internal state and set it in the Component
    allocate(isw%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state memory failed.", &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out
    call ESMF_CplCompSetInternalState(cplcomp, isw, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! get current VM because StateReconcile needs it
    !TODO: StateReconcile should have VM optional and this is obsolete
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! reconcile the States
    call ESMF_StateReconcile(importState, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_StateReconcile(exportState, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! access the state types
    call ESMF_StateGet(importState, statetype=isType, itemCount=isItemCount, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_StateGet(exportState, statetype=esType, itemCount=esItemCount, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    if (.not.((isType==ESMF_STATE_EXPORT).and.(esType==ESMF_STATE_IMPORT))) then
      ! not ES -> IS ==> should indicate problem???
    endif
    
    ! get the cplList Attribute
    cplListSize=100
    call NUOPC_CplCompAttributeGet(cplcomp, cplList, cplListSize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! get the importState std lists
    call NUOPC_StateBuildStdList(importState, importStdAttrNameList, &
      importStdItemNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! get the exportState std lists
    call NUOPC_StateBuildStdList(exportState, exportStdAttrNameList, &
      exportStdItemNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! prepare FieldBundles to store src and dst Fields
    isw%wrap%srcFields = ESMF_FieldBundleCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    isw%wrap%dstFields = ESMF_FieldBundleCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    do i=3, cplListSize ! todo: starting at 3 because of Attribute workaround
!print *, "cplList(",i,")=", trim(cplList(i))

      iMatch = 0  ! reset
      do j=1, size(importStdAttrNameList)
        if (importStdAttrNameList(j) == cplList(i)) then
          iMatch = j
          exit
        endif
      enddo
      
!if (iMatch > 0) &
!print *, "found match for importStdItemNameList()=", importStdItemNameList(iMatch)

      eMatch = 0  ! reset
      do j=1, size(exportStdAttrNameList)
        if (exportStdAttrNameList(j) == cplList(i)) then
          eMatch = j
          exit
        endif
      enddo
      
!if (eMatch > 0) &
!print *, "found match for exportStdItemNameList()=", exportStdItemNameList(eMatch)

      if (iMatch>0 .and. eMatch>0) then
        ! there are matching Fields in the import and export States
        call ESMF_StateGet(importState, field=iField, &
          itemName=importStdItemNameList(iMatch), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_StateGet(exportState, field=eField, &
          itemName=exportStdItemNameList(iMatch), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        
        ! add the import and export Fields to FieldBundles
        call ESMF_FieldBundleAdd(isw%wrap%srcFields, field=iField, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_FieldBundleAdd(isw%wrap%dstFields, field=eField, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
          
        ! set the connected Attribute on import Field
        call ESMF_AttributeSet(iField, &
          name="Connected", value="true", &
          convention="NUOPC", purpose="General", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        ! set the connected Attribute on export Field
        call ESMF_AttributeSet(eField, &
          name="Connected", value="true", &
          convention="NUOPC", purpose="General", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      else
        !TODO: Fields mentioned via stdname in Cpl metadata not found -> error?
      endif

    enddo
    
    ! precompute the regrid for all src to dst Fields
    call ESMF_FieldBundleRegridStore(isw%wrap%srcFields, isw%wrap%dstFields, &
      unmappedDstAction=ESMF_UNMAPPEDACTION_IGNORE, &
      routehandle=isw%wrap%rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (associated(importStdAttrNameList)) deallocate(importStdAttrNameList)
    if (associated(importStdItemNameList)) deallocate(importStdItemNameList)
    if (associated(exportStdAttrNameList)) deallocate(exportStdAttrNameList)
    if (associated(exportStdItemNameList)) deallocate(exportStdItemNameList)
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine Run(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(internalStateWrap) :: isw
    type(ESMF_VM)           :: vm

    rc = ESMF_SUCCESS
    
    ! query Component for its internal State
    nullify(isw%wrap)
    call ESMF_CplCompGetInternalState(cplcomp, isw, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    !TODO: here may be the place to ensure incoming States are consistent
    !TODO: with the Fields held in the FieldBundle inside the internal State?
      
    ! execute the regrid operation
    call ESMF_FieldBundleRegrid(isw%wrap%srcFields, isw%wrap%dstFields, &
      routehandle=isw%wrap%rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! update the timestamp on all of the dst fields to that on the src side
    call NUOPC_FieldBundleUpdateTime(isw%wrap%srcFields, isw%wrap%dstFields, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! get current VM because AttributeUpdate needs it
    !TODO: AttributeUpdate should have VM optional and this is obsolete
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! ensure that Attributes are correctly updated across the exportState    
    !TODO: rootList should be that of owner of importState, because that is
    !TODO: the petList on which attributes (timestamp) have been set ->
    !TODO: not sure how to find this out here... -> for now use PET 0 and hope!
    call ESMF_AttributeUpdate(exportState, vm=vm, rootList=(/0/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine Finalize(cplcomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer                 :: stat
    type(internalStateWrap) :: isw

    rc = ESMF_SUCCESS
    
    ! query Component for its internal State
    nullify(isw%wrap)
    call ESMF_CplCompGetInternalState(cplcomp, isw, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! destroy the objects in the internal state
    call ESMF_FieldBundleRegridRelease(isw%wrap%rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldBundleDestroy(isw%wrap%srcFields, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldBundleDestroy(isw%wrap%dstFields, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOG_ERRMSG, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! deallocate internal state memory
    deallocate(isw%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out
      
  end subroutine

end module
