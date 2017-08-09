#include "MAPL_Generic.h"

!=============================================================================
!BOP
!
! !MODULE: ExtData
!
! !INTERFACE:
!
      module ExtData
!
! !USESs:
      use ESMF
      use NUOPC
      use NUOPC_Model, &
           model_routine_SS      => SetServices, &
           model_label_Advance   => label_Advance, &
           model_label_DataInitialize   => label_DataInitialize
      use NUOPC_Generic
      
      implicit none
!
      private
!
! !PUBLIC MEMBER FUNCTIONS:
      public SetServices
!
! !DESCRIPTION: 
!
!EOP
!=============================================================================
!
!=============================================================================
contains
!=============================================================================
!BOP
!
! !IROUTINE: SetServices

! !INTERFACE:

      subroutine SetServices ( GC, RC )
!
! !OUTPUT PARAMETERS:
      integer, intent(out) :: RC  ! return code
!
! !INPUT/OUTPUT PARAMETERS:
      type(ESMF_GridComp) :: GC  ! gridded component

! !DESCRIPTION: Sets Initialize and Run services. 
! 
!
!EOP
!=============================================================================
!BOC
!
! ErrLog Variables
      character(len=ESMF_MAXSTR)       :: IAm = 'SetServices'
      integer                          :: STATUS
      character(len=ESMF_MAXSTR)       :: COMP_NAME
! Local derived type aliases
      type (mystates_WRAP)             :: mystates_ptr
      type (my_States), pointer        :: mystates
      
      ! Get my name and set-up traceback handle
      !----------------------------------------

      call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=STATUS )
      VERIFY_(STATUS)

      Iam = trim(COMP_NAME) //"::"// trim(Iam)
      
      call ESMF_LogWrite(Iam, ESMF_LOGMSG_INFO, rc=rc)      

      allocate(mystates)
      mystates_ptr%ptr => mystates
      call ESMF_UserCompSetInternalState(GC, 'MAPL_VarSpec', mystates_ptr, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out           
      
      ! the NUOPC model component will register the generic methods
      call NUOPC_CompDerive(GC, model_routine_SS, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! Register services for this component
      ! ------------------------------------
      ! Provide InitializeP0 to switch to custom IPD version
      call ESMF_GridCompSetEntryPoint(GC, ESMF_METHOD_INITIALIZE, &
           userRoutine=InitializeP0, phase=0, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
      ! set entry point for methods that require specific implementation
      call NUOPC_CompSetEntryPoint(GC, ESMF_METHOD_INITIALIZE, &
           phaseLabelList=(/"IPDv05p1"/), userRoutine=InitAdvertise, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
        return  ! bail out
      call NUOPC_CompSetEntryPoint(GC, ESMF_METHOD_INITIALIZE, &
           phaseLabelList=(/"IPDv05p6"/), userRoutine=InitRealize, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
      
     call NUOPC_CompSpecialize(GC, specLabel=model_label_DataInitialize, &
       specRoutine=dataInitialize, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out
      
      call NUOPC_CompSpecialize(GC, specLabel=model_label_Advance, &
           specRoutine=ModelAdvance, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
      
    end subroutine SetServices

    !-----------------------------------------------------------------------------

    subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: gcomp
      type(ESMF_State)      :: importState, exportState
      type(ESMF_Clock)      :: clock
      integer, intent(out)  :: rc
      
      rc = ESMF_SUCCESS
      call ESMF_LogWrite("ExtData InitializeP0", ESMF_LOGMSG_INFO, rc=rc)
      
      ! Switch to IPDv02 (for datainitialize dependency loop) 
      ! by filtering all other phaseMap entries
      call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, &
           acceptStringList=(/"IPDv05p"/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
      
    end subroutine InitializeP0

  !-----------------------------------------------------------------------------

    subroutine InitAdvertise(GC, IMPORT, EXPORT, CLOCK, rc)
      type(ESMF_GridComp)  :: GC
      type(ESMF_State)     :: IMPORT
      type(ESMF_State)     :: EXPORT
      type(ESMF_Clock)     :: CLOCK
      integer, intent(out) :: rc

      ! locals
      type (mystates_WRAP)         :: mystates_ptr
      type(MAPL_VarSpec), pointer  :: exportSpec(:)
      character(len=ESMF_MAXSTR)   :: short_name
      character(len=ESMF_MAXSTR)   :: long_name
      character(len=ESMF_MAXSTR)   :: units
      integer                      :: mytype
      integer                      :: i
     
      rc = ESMF_SUCCESS
      
      call ESMF_LogWrite("ExtData:InitAdvertise", ESMF_LOGMSG_INFO, rc=rc)      

      ! instead of explicitly advertising fields, set the
      ! FieldTransferPolicy to "transferAll" so that all fields
      ! in the CTM import state will be mirrored here
      call NUOPC_SetAttribute(EXPORT, name="FieldTransferPolicy", &
           value="transferAll", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
      
    end subroutine InitAdvertise
!EOC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BOP
!
! !IROUTINE: InitRealize
! 
! !INTERFACE:
!
    subroutine InitRealize ( GC, IMPORT, EXPORT, CLOCK, RC )
!
      type(ESMF_GridComp) :: GC     ! Gridded component 
      type(ESMF_State)    :: IMPORT ! Import state
      type(ESMF_State)    :: EXPORT ! Export state
      type(ESMF_Clock)    :: CLOCK  ! The clock
!
! !OUTPUT PARAMETERS:
      integer, intent(  out) :: RC     ! Error code!
! !DESCRIPTION:
!
!EOP
!=============================================================================
!BOC
!
      character(len=ESMF_MAXSTR)       :: IAm = 'initRealize'
      integer                          :: STATUS, ic, nSpc, i, j, k
      character(len=ESMF_MAXSTR)       :: COMP_NAME

      ! locals

      RC = ESMF_SUCCESS
      
      call ESMF_GridCompGet ( GC, name=COMP_NAME, RC=rc )
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      Iam = trim(COMP_NAME)//'::InitRealize'

      call ESMF_LogWrite(Iam, ESMF_LOGMSG_INFO, rc=rc)      

      call MirrorFieldsInState(EXPORT, rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return ! bail out

      if ( MAPL_am_I_root() ) then
         print *,  trim(Iam)//": EXPORT State" 
                                 call ESMF_StatePrint ( EXPORT )
      endif

    end subroutine initRealize

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

        print *, "EXTDATA export items;", itemCount

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

                call ESMF_LogWrite(trim(itemNameList(i))//trim(transferGeom), &
                              ESMF_LOGMSG_INFO, rc=rc)

!                if (trim(transferGeom)=="accept") then

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

                        print *, "UNGRIDDED UBOUND = ", ungriddedUBound
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
#if 0
                else
                    !print *, "NOT COMPLETING FIELD: ", itemNameList(i), trim(transferGeom)
                    call ESMF_LogWrite("CANNOT complete mirrored field: "//itemNameList(i), &
                        ESMF_LOGMSG_INFO, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                        line=__LINE__, &
                        file=__FILE__)) &
                        return  ! bail out
                end if
#endif
            end if
        end do

        deallocate(itemNameList)
        deallocate(itemTypeList)

    end subroutine

!----------------------------------------------------------------------
      subroutine dataInitialize ( GC, RC )
!
      type(ESMF_GridComp)      :: GC     ! Gridded component 
      integer,   intent(  out) :: RC     ! Error code
!
    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_State)              :: importState, exportState

    RC = ESMF_SUCCESS

    call ESMF_LogWrite("EXTDATA:dataInitialize", ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out

    ! query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(GC, modelClock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! must explicitly set time stamp on all export fields
    call NUOPC_UpdateTimestamp(exportState, clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! indicate that data initialization is complete (breaking out of init-loop)
    call NUOPC_CompAttributeSet(GC, &
      name="InitializeDataComplete", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

     return
     end subroutine dataInitialize
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BOP
!
! !IROUTINE: RUN
!
! !INTERFACE:
!
    subroutine ModelAdvance( GC, RC )
!
! !INPUT/OUTPUT PARAMETERS:
      type(ESMF_GridComp) :: GC     ! Gridded component 
!
! !OUTPUT PARAMETERS:
      integer, intent(  out) :: RC     ! Error code
!
! !DESCRIPTION: Does nothing for now.
! 
!
!EOP 
!=============================================================================
!BOC
!
! ErrLog Variables
      character(len=ESMF_MAXSTR)        :: IAm = "ModelAdvance"
      integer                           :: STATUS
      character(len=ESMF_MAXSTR)        :: COMP_NAME
! Local derived types

      ! Get the target components name and set-up traceback handle.
      ! -----------------------------------------------------------
      
      RC = ESMF_SUCCESS
      
    end subroutine modelAdvance
!EOC
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Finalize_ 
!
! !INTERFACE:
!
   subroutine Finalize_ ( GC, IMPORT, EXPORT, CLOCK, RC )
!
! !INPUT/OUTPUT PARAMETERS:
      type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
      type(ESMF_State),    intent(inout) :: IMPORT ! Import state
      type(ESMF_State),    intent(inout) :: EXPORT ! Export state
      type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock
!
! !OUTPUT PARAMETERS:
      integer, optional,   intent(  out) :: RC     ! Error code
!
! !DESCRIPTION: 
!
! !REVISION HISTORY:
!
!EOP
!-------------------------------------------------------------------------
!BOC
      RC = ESMF_SUCCESS
    end subroutine Finalize_
!EOC
!---------------------------------------------------------------------------

    
    
  end module EXTDATA
