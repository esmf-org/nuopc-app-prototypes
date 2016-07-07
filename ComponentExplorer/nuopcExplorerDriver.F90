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
        driver_label_ModifyInitializePhaseMap => label_ModifyInitializePhaseMap, &
        driver_label_SetRunSequence => label_SetRunSequence, &
        driver_label_SetRunClock => label_SetRunClock
  
!    use NUOPC_Compliance_Model, only: registerIC
!    use NUOPC_Compliance_Connector, only :  registerIC_Connector => registerIC
!    use NUOPC_Compliance_Driver, only:      registerIC_Driver => registerIC


#define xstr(a) str(a)
#define str(a) #a

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
    
        character(len=80)       :: filter_initialize_phases
        character(len=80)       :: enable_field_mirroring
        integer                 :: urc

        rc = ESMF_SUCCESS
    
        ! NUOPC_Driver registers the generic methods
        call NUOPC_CompDerive(driver, driver_routine_SS, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
    
        call NUOPC_CompSetInternalEntryPoint(driver, ESMF_METHOD_INITIALIZE, &
            phaseLabelList=(/"IPDv05p1"/), userRoutine=InitializeP1, rc=rc)
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

        call NUOPC_CompSetInternalEntryPoint(driver, ESMF_METHOD_INITIALIZE, &
            phaseLabelList=(/"IPDv05p6"/), userRoutine=RealizeMirroredFields, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call NUOPC_CompSetInternalEntryPoint(driver, ESMF_METHOD_INITIALIZE, &
            phaseLabelList=(/"IPDv05p8"/), userRoutine=TimestampMirroredFields, rc=rc)
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
        call ESMF_AttributeGet(driver, &
            convention="compexplorer", purpose="compexplorer", &
            name="filter_initialize_phases", value=filter_initialize_phases, rc=rc)
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
    
        call ESMF_AttributeGet(driver, &
            convention="compexplorer", purpose="compexplorer", &
            name="enable_field_mirroring", value=enable_field_mirroring, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            call ESMF_Finalize(endflag=ESMF_END_ABORT)

        if (trim(enable_field_mirroring)=="yes") then

            call NUOPC_CompSetInternalEntryPoint(driver, ESMF_METHOD_INITIALIZE, &
                phaseLabelList=(/"IPDv01p2"/), userRoutine=ModifyCplList, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out

            ! this run phase just timestamps the export fields
            call NUOPC_CompSetEntryPoint(driver, ESMF_METHOD_RUN, &
                phaseLabelList=(/"RunInternal"/), userRoutine=RunInternal, rc=rc)
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

        end if

#define COMPLIANCE_CHECK_DRIVER__off
#ifdef COMPLIANCE_CHECK_DRIVER
        ! compliance output for driver itself
        call ESMF_GridCompSetServices(driver, userRoutine=registerIC_Driver, &
            userRc=urc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            call ESMF_Finalize(endflag=ESMF_END_ABORT)
        if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            call ESMF_Finalize(endflag=ESMF_END_ABORT)
#endif

    end subroutine

    !-----------------------------------------------------------------------------
  
    subroutine SetModelServices(driver, rc)
        type(ESMF_GridComp)  :: driver
        integer, intent(out) :: rc
    
        ! local variables
        integer                       :: urc
        type(ESMF_Grid)               :: grid
        type(ESMF_Field)              :: field
        integer                       :: localPet
#if (!defined FRONT_COMP && !defined FRONT_H_COMP && !defined FRONT_SO_COMP)
        type(ESMF_VM)                 :: vm
        integer                       :: argCount
        character(len=160)            :: soName
#endif
        type(ESMF_GridComp)           :: child
        type(ESMF_CplComp)            :: drv2comp, comp2drv
        character(len=80)             :: enable_compliance_check
        character(len=80)             :: enable_field_mirroring

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
    call NUOPC_DriverAddComp(driver, xstr(FRONT_COMP_LABEL), compSS, comp=child, rc=rc)
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
    call NUOPC_DriverAddComp(driver, xstr(FRONT_COMP_LABEL), &
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
    
        call NUOPC_DriverAddComp(driver, xstr(FRONT_COMP_LABEL), &
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

        ! set up connector for field mirroring, if enabled
        call ESMF_AttributeGet(driver, &
            convention="compexplorer", purpose="compexplorer", &
            name="enable_field_mirroring", &
            value=enable_field_mirroring, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            call ESMF_Finalize(endflag=ESMF_END_ABORT)

        if (trim(enable_field_mirroring)=="yes") then

            call NUOPC_DriverAddComp(driver, srcCompLabel="explorerDriver", &
                dstCompLabel=xstr(FRONT_COMP_LABEL), compSetServicesRoutine=cplSS, &
                comp=drv2comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
            call NUOPC_CompAttributeSet(drv2comp, name="Verbosity", &
                value="high", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out

            call NUOPC_DriverAddComp(driver, srcCompLabel=xstr(FRONT_COMP_LABEL), &
                dstCompLabel="explorerDriver", compSetServicesRoutine=cplSS, &
                comp=comp2drv, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
            call NUOPC_CompAttributeSet(comp2drv, name="Verbosity", &
                value="high", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out

        endif
    
        ! Use an internal NUOPC Layer call to allow AutoAdd field dictionary entries
        call NUOPC_FieldDictionarySetAutoAdd(.true., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
    
        ! see if compliance checking is enabled
        call ESMF_AttributeGet(driver, &
            convention="compexplorer", purpose="compexplorer", &
            name="enable_compliance_check", value=enable_compliance_check, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            call ESMF_Finalize(endflag=ESMF_END_ABORT)
        if (trim(enable_compliance_check)=="yes") then
            ! Explicitly register compliance IC for Driver
            !TODO: future versions of ESMF/NUOPC may provide RUNTIME environemnt to 
            !TODO: switch NUOPC component specific compliance checking on/off.
!            call ESMF_GridCompSetServices(child, userRoutine=registerIC, &
!                userRc=urc, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                call ESMF_Finalize(endflag=ESMF_END_ABORT)
            if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                call ESMF_Finalize(endflag=ESMF_END_ABORT)
            ! compliance check connector, if present
            if (trim(enable_field_mirroring)=="yes") then
!                call ESMF_CplCompSetServices(drv2comp, userRoutine=registerIC_Connector, &
!                    userRc=urc, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__, &
                    file=__FILE__)) &
                    call ESMF_Finalize(endflag=ESMF_END_ABORT)
                if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__, &
                    file=__FILE__)) &
                    call ESMF_Finalize(endflag=ESMF_END_ABORT)
!                call ESMF_CplCompSetServices(comp2drv, userRoutine=registerIC_Connector, &
!                    userRc=urc, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__, &
                    file=__FILE__)) &
                    call ESMF_Finalize(endflag=ESMF_END_ABORT)
                if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__, &
                    file=__FILE__)) &
                    call ESMF_Finalize(endflag=ESMF_END_ABORT)
            end if
        endif

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
        character(len=NUOPC_PhaseMapStringLength), allocatable :: initPhase(:)
        type(ESMF_GridComp), pointer  :: compList(:)
        integer                       :: reducedPhaseCount, phaseIdx

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
                        convention="NUOPC", purpose="Instance", rc=rc)
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
                            convention="NUOPC", purpose="Instance", rc=rc)
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
                    reducedPhaseCount = 0
                    phaseIdx = 1
                    do k=1, phaseCount
                        if ( index(trim(phaseMap(k)), "p1")       > 0 .or. &
                            index(trim(phaseMap(k)), "p2")       > 0 .or. &
                            index(trim(phaseMap(k)), "IPDv05p3") > 0 .or. &
                            index(trim(phaseMap(k)), "IPDv05p4") > 0) then
                            reducedPhaseCount = reducedPhaseCount + 1
                        endif
                    enddo
                    allocate(initPhase(reducedPhaseCount))
                    initPhase(:) = ""   ! initialize empty
                    do k=1, phaseCount
                        if ( index(trim(phaseMap(k)), "p1")       > 0 .or. &
                            index(trim(phaseMap(k)), "p2")       > 0 .or. &
                            index(trim(phaseMap(k)), "IPDv05p3") > 0 .or. &
                            index(trim(phaseMap(k)), "IPDv05p4") > 0) then
                            initPhase(phaseIdx) = trim(phaseMap(k))
                            phaseIdx = phaseIdx + 1
                        endif
                    enddo          ! step4: replace the full InitializePhaseMap with only the p1 mapping
                    call NUOPC_CompAttributeSet(compList(i), &
                        name="InitializePhaseMap", valueList=initPhase, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                        line=__LINE__, &
                        file=__FILE__)) &
                        return  ! bail out
                    deallocate(initPhase)
                endif
            endif
        enddo
    
        ! clean-up
        deallocate(compList)
    
    end subroutine
  

    subroutine InitializeP1(driver, importState, exportState, clock, rc)
        type(ESMF_GridComp)  :: driver
        type(ESMF_State)     :: importState, exportState
        type(ESMF_Clock)     :: clock
        integer, intent(out) :: rc

        character(len=80)  :: enable_field_mirroring
        type(ESMF_GridComp) :: comp
        type(ESMF_State)   :: compImport, compExport

        rc = ESMF_SUCCESS
        !print *, "Inside nuopcExplorerDriver.InitializeP1"

        call ESMF_AttributeGet(driver, name="enable_field_mirroring", &
            convention="compexplorer", purpose="compexplorer", &
            value=enable_field_mirroring, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            call ESMF_Finalize(endflag=ESMF_END_ABORT)

        if (trim(enable_field_mirroring)=="yes") then
            call NUOPC_DriverGetComp(driver, compLabel=xstr(FRONT_COMP_LABEL), &
                comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out

            call ESMF_GridCompGet(comp, importState=compImport, &
                exportState=compExport, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out

            call NUOPC_SetAttribute(compImport, "FieldTransferPolicy", &
                "transferAll", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
            call NUOPC_SetAttribute(compExport, "FieldTransferPolicy", &
                "transferAll", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out

            ! set driver's own import/export to accept field transfers
            call NUOPC_SetAttribute(importState, "FieldTransferPolicy", &
                "transferAll", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
            call NUOPC_SetAttribute(exportState, "FieldTransferPolicy", &
                "transferAll", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out

        end if
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
  
    subroutine RealizeMirroredFields(driver, importState, exportState, clock, rc)
        type(ESMF_GridComp)  :: driver
        type(ESMF_State)     :: importState, exportState
        type(ESMF_Clock)     :: clock
        integer, intent(out) :: rc

        character(len=80)      :: enable_field_mirroring, statename
        integer                :: i, itemCount, stat
        character(ESMF_MAXSTR) :: transferGeom
        character(ESMF_MAXSTR), allocatable :: itemNameList(:)
        type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
        type(ESMF_Field)       :: field

        rc = ESMF_SUCCESS

        !print *, "Inside RealizeMirroredFields"

        ! realize mirrored fields that have accepted grid from other comp

        call ESMF_AttributeGet(driver, name="enable_field_mirroring", &
            convention="compexplorer", purpose="compexplorer", &
            value=enable_field_mirroring, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return ! bail out

        if (trim(enable_field_mirroring)/="yes") return

        call MirrorFieldsInState(importState, rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return ! bail out

        call MirrorFieldsInState(exportState, rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return ! bail out

    end subroutine

    subroutine MirrorFieldsInState(state, rc)
        type(ESMF_State), intent(in) :: state
        integer, intent(out) :: rc

        integer                :: i, itemCount, stat
        character(ESMF_MAXSTR) :: transferGeom
        character(ESMF_MAXSTR), allocatable :: itemNameList(:)
        type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
        type(ESMF_Field)       :: field

        type(ESMF_Grid)        :: grid
        type(ESMF_DistGrid)    :: distgrid
        integer, allocatable   :: minIndexPTile(:,:), maxIndexPTile(:,:)
        integer                :: dimCount
        character(len=80)      :: valueString, attrString
        logical                :: isPresent
        type(ESMF_AttPack)     :: attpack
        integer, pointer       :: ungriddedLBound(:), ungriddedUBound(:)

        rc = ESMF_SUCCESS

        call ESMF_StateGet(state, itemCount=itemCount, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out

        allocate(itemNameList(itemCount),stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
            msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__, rcToReturn=rc)) &
            return  ! bail out

        allocate(itemTypeList(itemCount),stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__, rcToReturn=rc)) &
            return  ! bail out

        call ESMF_StateGet(state, itemNameList=itemNameList, &
            itemTypeList=itemTypeList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        ! WARNING: does not currently deal with nested states or field bundles
        do i=lbound(itemNameList,1), ubound(itemNameList,1)
            if (itemTypeList(i)==ESMF_STATEITEM_FIELD) then

                ! TODO: condition on NUOPC_IsConnected first
                ! NUOPC_IsConnected(state, fieldName=fieldNameList(i))

                call ESMF_StateGet(state, &
                    itemNameList(i), field, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__, &
                    file=__FILE__)) &
                    return  ! bail out

                call NUOPC_GetAttribute(field, name="TransferActionGeomObject", &
                    value=transferGeom, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__, &
                    file=__FILE__)) &
                    return  ! bail out

                if (trim(transferGeom)=="accept") then

                    call ESMF_LogWrite("Completing mirrored field: "//itemNameList(i), &
                        ESMF_LOGMSG_INFO, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                        line=__LINE__, file=__FILE__)) &
                        return  ! bail out

                    nullify(ungriddedLBound)
                    nullify(ungriddedUBound)

                    call ESMF_AttributeGetAttPack(field, attpack=attpack, &
                      convention="NUOPC", purpose="Instance", isPresent=isPresent, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                        line=__LINE__, file=__FILE__)) &
                        return  ! bail out
                    if (.not. isPresent) then
                      ! attpack not present
                      call ESMF_LogWrite("Field level attpack NOT present!", &
                          ESMF_LOGMSG_WARNING, rc=rc)
                      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                        line=__LINE__, file=__FILE__)) &
                        return  ! bail out
                    else
                      ! retrieve ungridded dimension bounds and mirror
                      ! match those as well
                      call ESMF_AttributeGet(field, name="UngriddedLBound", &
                        attpack=attpack, itemCount=itemCount, isPresent=isPresent, &
                        attnestflag=ESMF_ATTNEST_ON, rc=rc)
                      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                        line=__LINE__, file=__FILE__)) &
                        return  ! bail out

                      if (isPresent .and. itemCount > 0) then
                        allocate(ungriddedLBound(itemCount),stat=stat)
                        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
                          msg="Allocation of internal ungriddedLBound failed.", &
                          line=__LINE__, file=__FILE__, rcToReturn=rc)) &
                          return  ! bail out

                        call ESMF_AttributeGet(field, &
                          name="UngriddedLBound", valueList=ungriddedLBound, &
                          convention="NUOPC", purpose="Instance", &
                          attnestflag=ESMF_ATTNEST_ON, rc=rc)
                        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                          line=__LINE__, &
                          file=__FILE__)) &
                          return  ! bail out

                        !print *, "UNGRIDDED LBOUND = ", ungriddedLBound
                      endif

                      call ESMF_AttributeGet(field, name="UngriddedUBound", &
                        attpack=attpack, itemCount=itemCount, isPresent=isPresent, &
                        attnestflag=ESMF_ATTNEST_ON, rc=rc)
                      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                        line=__LINE__, file=__FILE__)) &
                        return  ! bail out

                      if (isPresent .and. itemCount > 0) then
                        allocate(ungriddedUBound(itemCount),stat=stat)
                        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
                          msg="Allocation of internal ungriddedUBound failed.", &
                          line=__LINE__, file=__FILE__, rcToReturn=rc)) &
                          return  ! bail out

                        call ESMF_AttributeGet(field, &
                          name="UngriddedUBound", valueList=ungriddedUBound, &
                          convention="NUOPC", purpose="Instance", &
                          attnestflag=ESMF_ATTNEST_ON, rc=rc)
                        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                          line=__LINE__, &
                          file=__FILE__)) &
                          return  ! bail out

                        !print *, "UNGRIDDED UBOUND = ", ungriddedUBound
                      endif
                    endif

                    if (associated(ungriddedLBound) .and. &
                        associated(ungriddedUBound)) then
                      call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, &
                        ungriddedLBound=ungriddedLBound, &
                        ungriddedUBound=ungriddedUBound, &
                        rc=rc)
                      deallocate(ungriddedLBound)
                      deallocate(ungriddedUBound)
                    else
                      call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, rc=rc)
                    endif

                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                          line=__LINE__, &
                          file=__FILE__)) &
                          return  ! bail out

#define DEBUG_DISTGRID
#ifdef DEBUG_DISTGRID

                    call ESMF_FieldGet(field, grid=grid, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                        line=__LINE__, &
                        file=__FILE__)) &
                        return  ! bail out
                    call ESMF_GridGet(grid, distgrid=distgrid, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                        line=__LINE__, &
                        file=__FILE__)) &
                        return  ! bail out
                    call ESMF_DistGridGet(distgrid, dimCount=dimCount, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                        line=__LINE__, &
                        file=__FILE__)) &
                        return  ! bail out

                    allocate(minIndexPTile(dimCount,1), maxIndexPTile(dimCount,1))

                    call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, &
                        maxIndexPTile=maxIndexPTile, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                        line=__LINE__, &
                        file=__FILE__)) &
                        return  ! bail out

                    write(valueString, *) "DistGrid minIndexPTile(:,1) = ", minIndexPTile(:,1)
                    call ESMF_LogWrite(valueString, ESMF_LOGMSG_INFO, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                        line=__LINE__, &
                        file=__FILE__)) &
                        return  ! bail out

                    write(valueString, *) "DistGrid maxIndexPTile(:,1) = ", maxIndexPTile(:,1)
                    call ESMF_LogWrite(valueString, ESMF_LOGMSG_INFO, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                        line=__LINE__, &
                        file=__FILE__)) &
                        return  ! bail out

                    deallocate(minIndexPTile)
                    deallocate(maxIndexPTile)

#endif
                else
                    !print *, "NOT COMPLETING FIELD: ", itemNameList(i), trim(transferGeom)
                    call ESMF_LogWrite("CANNOT complete mirrored field: "//itemNameList(i), &
                        ESMF_LOGMSG_INFO, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                        line=__LINE__, &
                        file=__FILE__)) &
                        return  ! bail out
                end if

            end if
        end do

        deallocate(itemNameList)
        deallocate(itemTypeList)

    end subroutine


    subroutine TimestampMirroredFields(driver, importState, exportState, clock, rc)
        type(ESMF_GridComp)  :: driver
        type(ESMF_State)     :: importState, exportState
        type(ESMF_Clock)     :: clock
        integer, intent(out) :: rc

        character(len=80)      :: enable_field_mirroring, statename
        integer                :: i, itemCount, stat
        character(ESMF_MAXSTR) :: transferGeom
        character(ESMF_MAXSTR), allocatable :: itemNameList(:)
        type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
        type(ESMF_Field)       :: field

        rc = ESMF_SUCCESS

        !print *, "Inside TimestampMirroredFields"

        ! timestamp mirrored fields

        call ESMF_AttributeGet(driver, name="enable_field_mirroring", &
            convention="compexplorer", purpose="compexplorer", &
            value=enable_field_mirroring, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            call ESMF_Finalize(endflag=ESMF_END_ABORT)

        if (trim(enable_field_mirroring)/="yes") return

        call NUOPC_UpdateTimestamp(importState, clock, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            call ESMF_Finalize(endflag=ESMF_END_ABORT)

        call NUOPC_UpdateTimestamp(exportState, clock, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            call ESMF_Finalize(endflag=ESMF_END_ABORT)

        call ESMF_AttributeSet(driver, name="InitializeDataComplete", &
            value="true", convention="NUOPC",  purpose="Instance", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            call ESMF_Finalize(endflag=ESMF_END_ABORT)

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
                value=valueString, &
                convention="NUOPC", purpose="Instance", rc=rc)
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

    subroutine RunInternal(driver, importState, exportState, clock, rc)
        type(ESMF_GridComp)  :: driver
        type(ESMF_State)     :: importState, exportState
        type(ESMF_Clock)     :: clock
        integer, intent(out) :: rc

        type(ESMF_Clock)     :: internalClock

        rc = ESMF_SUCCESS

        ! update timestamp on all the import & export Fields
        call ESMF_GridCompGet(driver, clock=internalClock, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            return  ! bail out
        call NUOPC_UpdateTimestamp(importState, internalClock, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            return  ! bail out
        call NUOPC_UpdateTimestamp(exportState, internalClock, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            return  ! bail out

    end subroutine

    subroutine SetRunSequence(driver, rc)
        type(ESMF_GridComp)  :: driver
        integer, intent(out) :: rc

        rc = ESMF_SUCCESS

        !print *, "Resetting run sequence"

        ! Replace the default RunSequence with a customized one
        call NUOPC_DriverNewRunSequence(driver, slotCount=1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call NUOPC_DriverAddRunElement(driver, slot=1, &
            compLabel="explorerDriver", phaseLabel="RunInternal", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call NUOPC_DriverAddRunElement(driver, slot=1, &
            srcCompLabel="explorerDriver", dstCompLabel=xstr(FRONT_COMP_LABEL), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call NUOPC_DriverAddRunElement(driver, slot=1, &
            srcCompLabel=xstr(FRONT_COMP_LABEL), dstCompLabel="explorerDriver", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call NUOPC_DriverAddRunElement(driver, slot=1, &
            compLabel=xstr(FRONT_COMP_LABEL), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

    end subroutine
				
    recursive subroutine ModifyCplList(driver, importState, exportState, clock, rc)
        type(ESMF_GridComp)  :: driver
        type(ESMF_State)     :: importState, exportState
        type(ESMF_Clock)     :: clock
        integer, intent(out) :: rc

        type(ESMF_CplComp), pointer     :: connectorList(:)
        integer                         :: i, j, cplListSize
        character(len=160), allocatable :: cplList(:)

        rc = ESMF_SUCCESS

        nullify(connectorList)
        call NUOPC_DriverGetComp(driver, compList=connectorList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
     
        do i=1, size(connectorList)
            ! query the cplList for connector i
            call NUOPC_CompAttributeGet(connectorList(i), name="CplList", &
                itemCount=cplListSize, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
            if (cplListSize>0) then
                allocate(cplList(cplListSize))
                call NUOPC_CompAttributeGet(connectorList(i), name="CplList", &
                    valueList=cplList, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__, &
                    file=__FILE__)) &
                    return  ! bail out
                ! go through all of the entries in the cplList
                do j=1, cplListSize
                    ! switch from default regrid to redist
                    cplList(j) = trim(cplList(j))//":REMAPMETHOD=redist"
                enddo
                ! store the modified cplList in CplList attribute of connector i
                call NUOPC_CompAttributeSet(connectorList(i), &
                    name="CplList", valueList=cplList, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__, &
                    file=__FILE__)) &
                    return  ! bail out
                deallocate(cplList)
            endif
        enddo
  
        deallocate(connectorList)

    end subroutine				


  !-----------------------------------------------------------------------------
  
end module
