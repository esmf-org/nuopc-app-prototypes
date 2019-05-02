!==============================================================================
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

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
    driver_routine_SS                     => SetServices, &
    driver_label_SetModelServices         => label_SetModelServices, &
    driver_label_SetRunSequence           => label_SetRunSequence
  
#if defined(__GFORTRAN__) || defined(NAGFOR)
# define STRINGIFY_START(X) "&
# define STRINGIFY_END(X) &X"
#else /* default stringification */
# define STRINGIFY_(X) #X
# define STRINGIFY_START(X) &
# define STRINGIFY_END(X) STRINGIFY_(X)
#endif

#ifdef FRONT_COMP
  use FRONT_COMP, only: compSS => SetServices
#ifndef FRONT_COMP_LABEL
#define FRONT_COMP_LABEL FRONT_COMP    
#endif
#elif (defined FRONT_H_COMP)
  use compFront, only: compSS => SetServices
#endif

#ifndef FRONT_COMP_LABEL
#define FRONT_COMP_LABEL Component
#endif

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

    call NUOPC_CompDerive(driver, driver_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! The explorer implementation depends to a large degree on the generic
    ! NUOPC_Driver implementation. Only a few methods are overwritten here
    ! e.g. to report explorer info to stdout.

    !TODO: The specializing code here could actually be moved into the generic
    !TODO: NUOPC_Driver implementation, triggered for HierarchyProtocol=Explorer

    call NUOPC_CompSetInternalEntryPoint(driver, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv05p2"/), userRoutine=InitReport, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetInternalEntryPoint(driver, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv05p8"/), userRoutine=TimestampImportState, rc=rc)
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
    call NUOPC_CompSpecialize(driver, specLabel=driver_label_SetRunSequence, &
      specRoutine=SetRunSequence, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! internal run phase to timestamp the fields in the importState
    call NUOPC_CompSetInternalEntryPoint(driver, ESMF_METHOD_RUN, &
      phaseLabelList=(/"RunTimestampImportState"/), &
      userRoutine=RunTimestampImportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! set Verbosity on the driver
    call NUOPC_CompAttributeSet(driver, name="Verbosity", value="high", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! set HierarchyProtocol on the driver
    call NUOPC_CompAttributeSet(driver, name="HierarchyProtocol", &
      value="Explorer", rc=rc)
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
    integer                       :: localPet
#if (!defined FRONT_COMP && !defined FRONT_H_COMP && !defined FRONT_SO_COMP)
    type(ESMF_VM)                 :: vm
    integer                       :: argCount
    character(len=160)            :: soName
#endif
    type(ESMF_GridComp)           :: child
    character(len=80)             :: compName        

    rc = ESMF_SUCCESS

    compName = STRINGIFY_START(FRONT_COMP_LABEL)
      STRINGIFY_END(FRONT_COMP_LABEL)

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

    call NUOPC_DriverAddComp(driver, compName, compSS, comp=child, rc=rc)
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
    call NUOPC_DriverAddComp(driver, compName, &
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

    call NUOPC_DriverAddComp(driver, compName, &
      sharedObj=trim(soName), comp=child, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

    ! Set Verbosity on the explored component
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

  subroutine InitReport(driver, importState, exportState, clock, rc)
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
    character(len=4096), allocatable :: valueStringList(:)
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
              convention="NUOPC", purpose="Instance", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            if (isPresent .and. guardCount>0) then
              call ESMF_AttributeGet(comp, name="LongName", &
                value=valueString, &
                convention="NUOPC", purpose="Instance", rc=rc)
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
              convention="NUOPC", purpose="Instance", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            if (isPresent .and. guardCount>0) then
              call ESMF_AttributeGet(comp, name="ShortName", &
                value=valueString, &
                convention="NUOPC", purpose="Instance", rc=rc)
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
              convention="NUOPC", purpose="Instance", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            if (isPresent .and. guardCount>0) then
              call ESMF_AttributeGet(comp, name="Description", &
                value=valueString, &
                convention="NUOPC", purpose="Instance", rc=rc)
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
  
            ! report GridComp level attribute: Kind
            call ESMF_AttributeGet(comp, name="Kind", &
              itemCount=guardCount, isPresent=isPresent, &
              convention="NUOPC", purpose="Instance", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            if (isPresent .and. guardCount>0) then
              call ESMF_AttributeGet(comp, name="Kind", &
                value=valueString, &
                convention="NUOPC", purpose="Instance", rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              print *, "  "//trim(name)//": <Kind> = "// &
                trim(valueString)
            else if (isPresent .and. guardCount==0) then
              print *, "  "//trim(name)//": <Kind> : "// &
                "Attribute is present but NOT set!"
            else
              print *, "  "//trim(name)//": <Kind> : "// &
                "Attribute is NOT present!"
            endif
  
            ! report GridComp level attribute: CompLabel
            call ESMF_AttributeGet(comp, name="CompLabel", &
              itemCount=guardCount, isPresent=isPresent, &
              convention="NUOPC", purpose="Instance", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            if (isPresent .and. guardCount>0) then
              call ESMF_AttributeGet(comp, name="CompLabel", &
                value=valueString, &
                convention="NUOPC", purpose="Instance", rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              print *, "  "//trim(name)//": <CompLabel> = "// &
                trim(valueString)
            else if (isPresent .and. guardCount==0) then
              print *, "  "//trim(name)//": <CompLabel> : "// &
                "Attribute is present but NOT set!"
            else
              print *, "  "//trim(name)//": <CompLabel> : "// &
                "Attribute is NOT present!"
            endif
  
            ! report GridComp level attribute: InitializePhaseMap
            call ESMF_AttributeGet(comp, name="InitializePhaseMap", &
              itemCount=guardCount, isPresent=isPresent, &
              convention="NUOPC", purpose="Instance", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            if (isPresent .and. guardCount>0) then
              allocate(valueStringList(guardCount))
              call ESMF_AttributeGet(comp, name="InitializePhaseMap", &
                valueList=valueStringList, &
                convention="NUOPC", purpose="Instance", rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              do k=1, guardCount
                print *, "  "//trim(name)//": <InitializePhaseMap>(",k,") = "&
                  //trim(valueStringList(k))
              enddo
              deallocate(valueStringList)
            else if (isPresent .and. guardCount==0) then
              print *, "  "//trim(name)//": <InitializePhaseMap> : "// &
                "Attribute is present but NOT set!"
            else
              print *, "  "//trim(name)//": <InitializePhaseMap> : "// &
                "Attribute is NOT present!"
            endif
  
            ! report GridComp level attribute: RunPhaseMap
            call ESMF_AttributeGet(comp, name="RunPhaseMap", &
              itemCount=guardCount, isPresent=isPresent, &
              convention="NUOPC", purpose="Instance", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            if (isPresent .and. guardCount>0) then
              allocate(valueStringList(guardCount))
              call ESMF_AttributeGet(comp, name="RunPhaseMap", &
                valueList=valueStringList, &
                convention="NUOPC", purpose="Instance", rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              do k=1, guardCount
                print *, "  "//trim(name)//": <RunPhaseMap>(",k,") = "&
                  //trim(valueStringList(k))
              enddo
              deallocate(valueStringList)
            else if (isPresent .and. guardCount==0) then
              print *, "  "//trim(name)//": <RunPhaseMap> : "// &
                "Attribute is present but NOT set!"
            else
              print *, "  "//trim(name)//": <RunPhaseMap> : "// &
                "Attribute is NOT present!"
            endif
  
            ! report GridComp level attribute: FinalizePhaseMap
            call ESMF_AttributeGet(comp, name="FinalizePhaseMap", &
              itemCount=guardCount, isPresent=isPresent, &
              convention="NUOPC", purpose="Instance", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            if (isPresent .and. guardCount>0) then
              allocate(valueStringList(guardCount))
              call ESMF_AttributeGet(comp, name="FinalizePhaseMap", &
                valueList=valueStringList, &
                convention="NUOPC", purpose="Instance", rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              do k=1, guardCount
                print *, "  "//trim(name)//": <FinalizePhaseMap>(",k,") = "&
                  //trim(valueStringList(k))
              enddo
              deallocate(valueStringList)
            else if (isPresent .and. guardCount==0) then
              print *, "  "//trim(name)//": <FinalizePhaseMap> : "// &
                "Attribute is present but NOT set!"
            else
              print *, "  "//trim(name)//": <FinalizePhaseMap> : "// &
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

  subroutine TimestampImportState(driver, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: driver
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    type(ESMF_Clock)              :: internalClock

    rc = ESMF_SUCCESS

    ! timestamp the fields in the importState

    call ESMF_GridCompGet(driver, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_SetTimestamp(importState, internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_AttributeSet(driver, name="InitializeDataComplete", &
      value="true", convention="NUOPC",  purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

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
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
      msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (isPresent .and. guardCount>0) then
      call ESMF_AttributeGet(field, name=trim(attributeName), &
        value=valueString, convention="NUOPC", purpose="Instance", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, &
        msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      print *, "            " //nameString//" = "//trim(valueString)
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
          call ESMF_StateGet(state, itemName=itemNameList(item), field=field, &
            rc=rc)
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

  subroutine SetRunSequence(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc
    
    ! local variables
    character(ESMF_MAXSTR)        :: name
    type(NUOPC_FreeFormat)        :: runSeqFF
    character(len=80)             :: compName
    integer                       :: localPet

    rc = ESMF_SUCCESS
    
    compName = STRINGIFY_START(FRONT_COMP_LABEL)
      STRINGIFY_END(FRONT_COMP_LABEL)

    ! query the driver for its name and localPet
    call ESMF_GridCompGet(driver, name=name, localPet=localPet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    
    ! set up free format run sequence
    runSeqFF = NUOPC_FreeFormatCreate(stringList=(/"@* "/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    call NUOPC_FreeFormatAdd(runSeqFF, stringList=(/compName/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    call NUOPC_FreeFormatAdd(runSeqFF, stringList=(/&
      trim(compName)//" -> explorerDriver"/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    call NUOPC_FreeFormatAdd(runSeqFF, stringList=&
      (/"explorerDriver RunTimestampImportState"/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    call NUOPC_FreeFormatAdd(runSeqFF, stringList=(/&
      "explorerDriver -> "//trim(compName)/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    call NUOPC_FreeFormatAdd(runSeqFF, stringList=(/"@ " /), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
     
    if (localPet==0) then
      call NUOPC_FreeFormatPrint(runSeqFF, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    endif
    
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

  subroutine RunTimestampImportState(driver, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: driver
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out)    :: rc
    
    type(ESMF_Clock)        :: internalClock
    type(ESMF_Time)         :: time
    type(ESMF_TimeInterval) :: timeStep

    rc = ESMF_SUCCESS
    
    ! update timestamp on all the import & export Fields
    call ESMF_GridCompGet(driver, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      return  ! bail out
     
    call ESMF_ClockGet(internalClock, currTime=time, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      return  ! bail out
     
    ! must timestamp the fields in importState for the next timeStep
    time = time+timeStep
    call NUOPC_SetTimestamp(importState, time, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

end module
