!- conditional module blocks that support modules maybe being supplied in C
#ifdef FRONT_H_COMP
module compFront
#include FRONT_H_COMP
end module
#endif
!--------------------------------------------------------------------------

module nuopcExplorerDriver

  !-----------------------------------------------------------------------------
  ! Code that specializes generic NUOPC_Driver
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Driver, &
    driver_routine_SS             => SetServices, &
    driver_label_SetModelServices => label_SetModelServices, &
    driver_label_ModifyInitializePhaseMap => label_ModifyInitializePhaseMap
    
#ifdef FRONT_COMP
  use FRONT_COMP, only: compSS => SetServices
#elif (defined FRONT_H_COMP)
  use compFront, only: compSS => SetServices
#endif

  implicit none
  
  private
  
  public SetServices
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc
    
    character(len=80)       :: filter_initialize_phases

    rc = ESMF_SUCCESS
    
    ! NUOPC_Driver registers the generic methods
    call NUOPC_CompDerive(driver, driver_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! set entry point for an internal initialize phase 2
    call NUOPC_CompSetInternalEntryPoint(driver, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p2"/), userRoutine=InitializeP2, rc=rc)
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
      
    ! see if initialize phases need to be filtered
    call ESMF_AttributeGet(driver, name="filter_initialize_phases", &
      value=filter_initialize_phases, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (trim(filter_initialize_phases)=="yes") then
      call NUOPC_CompSpecialize(driver, &
        specLabel=driver_label_ModifyInitializePhaseMap, &
        specRoutine=ModifyInitializePhaseMap, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    
  end subroutine

  !-----------------------------------------------------------------------------
  
  subroutine SetModelServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc
    
    ! local variables
    integer                       :: localrc
    type(ESMF_Grid)               :: grid
    type(ESMF_Field)              :: field
    integer                       :: localPet
#if (!defined FRONT_COMP && !defined FRONT_H_COMP && !defined FRONT_SO_COMP)
    type(ESMF_VM)                 :: vm
    integer                       :: argCount
    character(len=160)            :: soName
#endif
    type(ESMF_GridComp)           :: child

    rc = ESMF_SUCCESS
    
    ! query Driver for localPet
    call ESMF_GridCompGet(driver, localPet=localPet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! SetServices for the COMPONENT as modelComp(1)
#if (defined FRONT_COMP || defined FRONT_H_COMP)
    ! the component front is provided as Fortran module or C header
    if (localPet==0) then
      print *, "Exploring a component with Fortran module or C header front..."
    endif
    call NUOPC_DriverAddComp(driver, "Component", compSS, comp=child, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#elif (defined FRONT_SO_COMP)
    ! the component front is provided as a shared object with a file name that
    ! is known at compile time
    if (localPet==0) then
      print *, "Exploring a component with a shared object front, known at "// &
        "compile time..."
    endif
    call NUOPC_DriverAddComp(driver, "Component", &
      sharedObj="./"//FRONT_SO_COMP, comp=child, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#else
    ! the component front is provided as a shared object with a file name that
    ! is passed in at run-time
    if (localPet==0) then
      print *, "Exploring a component with a shared object front that is "// &
        "supplied at run time as command line argument..."
    endif
    
    ! Obtain shared object name from the command line argument
    call ESMF_GridCompGet(driver, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (localPet == 0) then
      call ESMF_UtilGetArgC(count=argCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if (argCount /= 1) then
        print *, "This executable was compiled to expect exactly one "// &
          "command line argument. ABORTING..."
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="Executable was expecting exactly one command line argument.", &
          line=__LINE__, &
          file=__FILE__)
        return  ! bail out
      endif
      call ESMF_UtilGetArg(argindex=1, argvalue=soName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    call ESMF_VMBroadcast(vm, soName, count=len(soName), rootPet=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call NUOPC_DriverAddComp(driver, "Component", &
      sharedObj=trim(soName), comp=child, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
    call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Use an internal NUOPC Layer call to allow AutoAdd field dictionary entries
    call NUOPC_FieldDictionarySetAutoAdd(.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModifyInitializePhaseMap(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc
    
    ! local variables
    integer                       :: i, j, k
    character(len=80)             :: iString
    character(len=ESMF_MAXSTR)    :: name
    integer                       :: phaseCount
    integer                       :: localPet
    character(len=NUOPC_PhaseMapStringLength), allocatable :: phaseMap(:)
    character(len=NUOPC_PhaseMapStringLength) :: initPhase(2)
    type(ESMF_GridComp), pointer  :: compList(:)

    rc = ESMF_SUCCESS
    
    ! query Driver for localPet
    call ESMF_GridCompGet(driver, localPet=localPet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! query Driver for child components
    nullify(compList)
    call NUOPC_DriverGetComp(driver, compList, rc=rc)    
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! remove the InitializePhaseMap Attribute
    do i=0, size(compList)
      write(iString, *) i
      ! Models
      if (i > 0) then
        if (NUOPC_CompAreServicesSet(compList(i))) then
          ! clean out all phases out of InitializePhaseMap, except p1
          ! step1: determine how many phases are in the InitializePhaseMap
          call ESMF_AttributeGet(compList(i), &
            name="InitializePhaseMap", &
            itemCount=phaseCount, &
            convention="NUOPC", purpose="General", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          ! step2: allocate phaseMap array and obtain it from InitializePhaseMap
          allocate(phaseMap(phaseCount))
          if (localPet == 0) then
            print *, "Model component # "//trim(adjustl(iString))// &
              " InitializePhaseMap:"
          endif
          if (phaseCount > 0) then
            call ESMF_AttributeGet(compList(i), &
              name="InitializePhaseMap", &
              valueList=phaseMap, &
              convention="NUOPC", purpose="General", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            if (localPet == 0) then
              do k=1, phaseCount
                print *, "  "//trim(phaseMap(k))
              enddo
            endif
          else
            if (localPet == 0) then
              print *, "  << unavailable >>"
            endif            
          endif
          ! step3: set the initPhase variable to only include the p1 phase map
          initPhase(:) = ""   ! initialize empty
          do k=1, phaseCount
            if (index(trim(phaseMap(k)), "p1") > 0) then
              ! found a p1 mapping
              initPhase(1) = trim(phaseMap(k))
            elseif (index(trim(phaseMap(k)), "p2") > 0) then
              ! found a p1 mapping
              initPhase(2) = trim(phaseMap(k))
            endif
          enddo
          ! step4: replace the full InitializePhaseMap with only the p1 mapping
          call ESMF_AttributeSet(compList(i), &
            name="InitializePhaseMap", &
            valueList=initPhase, &
            convention="NUOPC", purpose="General", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        endif
      endif
    enddo
    
    ! clean-up
    deallocate(compList)
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP2(driver, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: driver
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_GridComp)           :: comp
    type(ESMF_State)              :: iState, eState
    integer                       :: localPet
    integer                       :: guardCount
    integer                       :: i, j, k
    character(len=80)             :: iString
    character(len=ESMF_MAXSTR)    :: name
    character(len=4096)           :: valueString
    logical                       :: isPresent
    integer                       :: itemCount
    type(ESMF_GridComp), pointer  :: compList(:)

    rc = ESMF_SUCCESS
    
    ! query Driver for localPet
    call ESMF_GridCompGet(driver, localPet=localPet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! query Driver for child components
    nullify(compList)
    call NUOPC_DriverGetComp(driver, compList, rc=rc)    
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (localPet==0) then
    
      ! report details about model and connector components to stdout
      do i=0, size(compList)
        ! Models
        if (i > 0) then
          if (NUOPC_CompAreServicesSet(compList(i))) then
          
            write(iString, *) i
            comp = compList(i) ! alias to the component to be explored
            
            ! report name of the ESMF_GridComp object
            call ESMF_GridCompGet(comp, name=name, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            print *, "Model component # "//trim(adjustl(iString))//&
              " // name = "//trim(name)

            ! report GridComp level attribute: LongName              
            call ESMF_AttributeGet(comp, name="LongName", &
              itemCount=guardCount, isPresent=isPresent, &
              convention="NUOPC", purpose="General", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            if (isPresent .and. guardCount>0) then
              call ESMF_AttributeGet(comp, name="LongName", &
                value=valueString, &
                convention="NUOPC", purpose="General", rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              print *, "  "//trim(name)//": <LongName>    = "// &
                trim(valueString)
            else if (isPresent .and. guardCount==0) then
              print *, "  "//trim(name)//": <LongName>    : "// &
                "Attribute is present but NOT set!"
            else
              print *, "  "//trim(name)//": <LongName>    : "// &
                "Attribute is NOT present!"
            endif
            
            ! report GridComp level attribute: ShortName              
            call ESMF_AttributeGet(comp, name="ShortName", &
              itemCount=guardCount, isPresent=isPresent, &
              convention="NUOPC", purpose="General", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            if (isPresent .and. guardCount>0) then
              call ESMF_AttributeGet(comp, name="ShortName", &
                value=valueString, &
                convention="NUOPC", purpose="General", rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              print *, "  "//trim(name)//": <ShortName>   = "// &
                trim(valueString)
            else if (isPresent .and. guardCount==0) then
              print *, "  "//trim(name)//": <ShortName>   : "// &
                "Attribute is present but NOT set!"
            else
              print *, "  "//trim(name)//": <ShortName>   : "// &
                "Attribute is NOT present!"
            endif
            
            ! report GridComp level attribute: Description              
            call ESMF_AttributeGet(comp, name="Description", &
              itemCount=guardCount, isPresent=isPresent, &
              convention="NUOPC", purpose="General", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            if (isPresent .and. guardCount>0) then
              call ESMF_AttributeGet(comp, name="Description", &
                value=valueString, &
                convention="NUOPC", purpose="General", rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              print *, "  "//trim(name)//": <Description> = "// &
                trim(valueString)
            else if (isPresent .and. guardCount==0) then
              print *, "  "//trim(name)//": <Description> : "// &
                "Attribute is present but NOT set!"
            else
              print *, "  "//trim(name)//": <Description> : "// &
                "Attribute is NOT present!"
            endif
            
            ! obtain import and export state
            call ESMF_GridCompGet(comp, importState=iState, &
              exportState=eState, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            
            print *, "     --------"

            ! explore the importState
            call exploreState(iState, stateIdentifier="importState", &
              compName=name, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out

            print *, "     --------"

            ! explore the exportState
            call exploreState(eState, stateIdentifier="exportState", &
              compName=name, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            
          endif
        endif
        ! Connectors
        do j=0, size(compList)
          
            ! not expecting any Connectors in the explorer right now
              
        enddo
      enddo
      
    endif
    
    ! clean-up
    deallocate(compList)
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine exploreField(field, attributeName, rc)
    type(ESMF_Field)                      :: field
    character(*), intent(in)              :: attributeName
    integer,      intent(out), optional   :: rc
    
    integer             :: guardCount
    logical             :: isPresent
    character(len=80)   :: valueString
    character(len=16)   :: nameString

    if (present(rc)) rc = ESMF_SUCCESS

    write (nameString, "(A16)") "<"//trim(attributeName)//">"

    call ESMF_AttributeGet(field, name=trim(attributeName), &
      itemCount=guardCount, isPresent=isPresent, &
      convention="NUOPC", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
      msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (isPresent .and. guardCount>0) then
      call ESMF_AttributeGet(field, name=trim(attributeName), &
        value=valueString, &
        convention="NUOPC", purpose="General", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, &
        msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      print *, "            " // &
        nameString//" = "//trim(valueString)
    else if (isPresent .and. guardCount==0) then
      print *, "            " // &
        nameString//" : Attribute is present but NOT set!"
    else
      print *, "            " // &
        nameString//" : Attribute is NOT present!"
    endif
  end subroutine
  
  !-----------------------------------------------------------------------------
  
  subroutine exploreState(state, stateIdentifier, compName, rc)
    type(ESMF_State)                      :: state
    character(*), intent(in)              :: stateIdentifier
    character(*), intent(in)              :: compName
    integer,      intent(out), optional   :: rc
    
    character(len=80)                       :: valueString
    integer                                 :: itemCount, item
    character(ESMF_MAXSTR), allocatable     :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable  :: stateitemtypeList(:)
    type(ESMF_Field)                        :: field

    if (present(rc)) rc = ESMF_SUCCESS
    
    call ESMF_StateGet(state, itemCount=itemCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    write (valueString, *) itemCount
    print *, "  "//trim(compName)//": "//trim(stateIdentifier)// &
      " // itemCount = "// trim(adjustl(valueString))
    
    if (itemCount > 0) then
      allocate(itemNameList(itemCount))
      allocate(stateitemtypeList(itemCount))
      call ESMF_StateGet(state, itemNameList=itemNameList, &
        itemtypeList=stateitemtypeList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
        
      do item=1, itemCount
        write (valueString, "(I3.3)") item
        if (stateitemtypeList(item) == ESMF_STATEITEM_FIELD) then
          print *, "  "//trim(compName)//": "//trim(stateIdentifier)// &
            " // item # "// &
            trim(adjustl(valueString))//" // [FIELD] name = "// &
            trim(itemNameList(item))
          call ESMF_StateGet(state, itemName=itemNameList(item), &
            field=field, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, &
            msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          call exploreField(field, attributeName="StandardName", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, &
            msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          call exploreField(field, attributeName="Units", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, &
            msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          call exploreField(field, attributeName="LongName", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, &
            msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          call exploreField(field, attributeName="ShortName", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, &
            msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        else if (stateitemtypeList(item) == ESMF_STATEITEM_FIELDBUNDLE) then
          print *, "  "//trim(compName)//": "//trim(stateIdentifier)// &
            " // item # "// &
            trim(adjustl(valueString))//" // [FIELDBUNDLE] name = "// &
            trim(itemNameList(item))
        else if (stateitemtypeList(item) == ESMF_STATEITEM_ARRAY) then
          print *, "  "//trim(compName)//": "//trim(stateIdentifier)// &
            " // item # "// &
            trim(adjustl(valueString))//" // [ARRAY] name: "// &
            trim(itemNameList(item))
        else if (stateitemtypeList(item) == ESMF_STATEITEM_ARRAYBUNDLE) then
          print *, "  "//trim(compName)//": "//trim(stateIdentifier)// &
            " // item # "// &
            trim(adjustl(valueString))//" // [ARRAYBUNDLE] name = "// &
            trim(itemNameList(item))
        else if (stateitemtypeList(item) == ESMF_STATEITEM_ROUTEHANDLE) then
          print *, "  "//trim(compName)//": "//trim(stateIdentifier)// &
            " // item # "// &
            trim(adjustl(valueString))//" // [ROUTEHANDLE] name = "// &
            trim(itemNameList(item))
        else if (stateitemtypeList(item) == ESMF_STATEITEM_STATE) then
          print *, "  "//trim(compName)//": "//trim(stateIdentifier)// &
            " // item # "// &
            trim(adjustl(valueString))//" // [STATE] name = "// &
            trim(itemNameList(item))
        else if (stateitemtypeList(item) == ESMF_STATEITEM_UNKNOWN) then
          print *, "  "//trim(compName)//": "//trim(stateIdentifier)// &
            " // item # "// &
            trim(adjustl(valueString))//" // [UNKNOWN] name = "// &
            trim(itemNameList(item))
        else if (stateitemtypeList(item) == ESMF_STATEITEM_NOTFOUND) then
          print *, "  "//trim(compName)//": "//trim(stateIdentifier)// &
            " // item # "// &
            trim(adjustl(valueString))//" // [NOTFOUND] name = "// &
            trim(itemNameList(item))
        endif
      enddo  
    
    endif
  end subroutine

  !-----------------------------------------------------------------------------
  
end module
