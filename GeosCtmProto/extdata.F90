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
!   use MAPL_Mod
#if 1
   use MAPL_BaseMod
   use MAPL_CommsMod
   use ESMFL_Mod
   use MAPL_GenericMod
   use MAPL_VarSpecMod
   use ESMF_CFIOFileMod
   use ESMF_CFIOMod
   use ESMF_CFIOUtilMod
   use MAPL_CFIOMod
   use MAPL_HorzTransformMod
   use MAPL_NewArthParserMod
   use MAPL_ConstantsMod, only: MAPL_PI,MAPL_PI_R8
   use MAPL_IOMod, only: MAPL_NCIOParseTimeUnits
   use, intrinsic :: iso_fortran_env, only: REAL64
#endif
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

  integer, parameter :: MAPL_ExtDataVectorItem    = 32
  integer, parameter :: MAPL_ExtDataNullFrac      = -9999
  logical            :: hasRun

! Primary Exports
! ---------------
  type PrimaryExport
     PRIVATE
     character(len=ESMF_MAXSTR)   :: name
     character(len=ESMF_MAXSTR)   :: units
     integer                      :: dim
     integer                      :: vloc
     character(len=ESMF_MAXSTR)   :: cyclic
     character(len=ESMF_MAXSTR)   :: refresh_template
     integer                      :: Trans
     real                         :: scale, offset
     logical                      :: do_offset, do_scale
     character(len=ESMF_MAXSTR)   :: var
     character(len=ESMF_MAXPATHLEN)   :: file
     logical                      :: hasFileReffTime
     character(len=ESMF_MAXSTR)   :: FileReffTime

     type(ESMF_Time), pointer     :: refresh_time => null()
     logical                      :: doInterpolate = .true.
     logical                      :: isConst
     real                         :: Const
     integer                      :: vartype ! MAPL_FieldItem or MAPL_BundleItem

!    variables for zero refresh time
     type(ESMF_Field)             :: finterp1, finterp2
     type(ESMF_FieldBundle)       :: binterp1, binterp2
     type(ESMF_Time)              :: time1, time2
     type(ESMF_Time)              :: interp_time1, interp_time2
     integer                      :: climyear
     type(ESMF_TimeInterval)      :: frequency
     type(ESMF_Time)              :: reff_time

     ! if primary export represents a pair of vector fields
     logical                      :: isVector, foundComp1, foundComp2
     ! fields to store endpoints for interpolation of a vector pair
     type(ESMF_Field)             :: v1_finterp1, v1_finterp2
     type(ESMF_Field)             :: v2_finterp1, v2_finterp2

     ! names of the two vector components in the gridded component where import is declared
     character(len=ESMF_MAXSTR)   :: vcomp1, vcomp2
     ! the corresponding names of the two vector components on file
     character(len=ESMF_MAXSTR)   :: fcomp1, fcomp2

     logical                      :: ExtDataAlloc
     ! time shifting during continuous update
     type(ESMF_TimeInterval)      :: tshift
     type(ESMF_Alarm)             :: update_alarm
     logical                      :: alarmIsEnabled = .false.
     integer                      :: FracVal = MAPL_ExtDataNullFrac 
  end type PrimaryExport

  type PrimaryExports
     PRIVATE
     integer :: nItems = 0
     type(PrimaryExport), pointer :: item(:) => null() 
  end type PrimaryExports

  type DerivedExport
     PRIVATE
     character(len=ESMF_MAXSTR)   :: name
     character(len=ESMF_MAXSTR)   :: expression
     character(len=ESMF_MAXSTR)   :: refresh_template
     logical                      :: ExtDataAlloc
     logical                      :: masking
     type(ESMF_Time), pointer     :: refresh_time => null()
     ! time shifting during continuous update
     type(ESMF_TimeInterval)      :: tshift
     type(ESMF_Alarm)             :: update_alarm
     logical                      :: alarmIsEnabled = .false.
  end type DerivedExport

  type DerivedExports
     PRIVATE
     integer :: nItems = 0
     type(DerivedExport), pointer :: item(:) => null()
  end type DerivedExports

! Legacy state
! ------------
  type MAPL_ExtData_State
     PRIVATE
     type(PrimaryExports) :: Primary
     type(DerivedExports) :: Derived
     ! will add fields from export state to this state
     ! will also add new fields that could be mask
     ! or primary exports that were not in the export
     ! state recieved by ExtData, i.e. fields that are
     ! needed by a derived field where the primary fields
     ! are not actually required
     type(ESMF_State)     :: ExtDataState
     logical              :: active
     logical              :: ignoreCase
  end type MAPL_ExtData_State

! Hook for the ESMF
! -----------------
  type MAPL_ExtData_Wrap
     type (MAPL_ExtData_State), pointer :: PTR => null()
  end type MAPL_ExtData_WRAP

! This EXTERNAL subroutine is in the fv directory
!  and has real*8 interfaces

  interface
     subroutine A2D2C(U, V, lm, getC)
       real,    intent(INOUT)           :: U(:,:,:)
       real,    intent(INOUT)           :: V(:,:,:)
       integer, intent(   IN)           :: lm
       logical, intent(   IN), optional :: getC
     end subroutine A2D2C
  end interface

#if 0
  type T_EXTDATA_STATE
     type(ESMF_State)    :: expState
     type(ESMF_GridComp) :: gc
  end type T_EXTDATA_STATE
#endif

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
           phaseLabelList=(/"IPDv05p4"/), userRoutine=InitForceGridTransfer, rc=rc)
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

      ! Changing to DataInitialize specialization after change to
      ! NUOPC_Model.F90 to use default IPDv05p8 implementation
      
 !     call NUOPC_CompSetEntryPoint(GC, ESMF_METHOD_INITIALIZE, &
 !          phaseLabelList=(/"IPDv05p8"/), userRoutine=dataInitialize, rc=rc)
 !     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
 !          line=__LINE__, &
 !          file=__FILE__)) &
 !          return  ! bail out

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
      
      ! Switch to IPDv05
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


  !-----------------------------------------------------------------------------

    subroutine InitForceGridTransfer(gcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: gcomp
      type(ESMF_State)      :: importState, exportState
      type(ESMF_Clock)      :: clock
      integer, intent(out)  :: rc
      
      ! local variables
      integer                   :: i
      type(ESMF_Field), pointer :: fieldList(:)
      character(len=40)         :: value
      
      rc = ESMF_SUCCESS
      call ESMF_LogWrite("ExtData InitForceGridTransfer", ESMF_LOGMSG_INFO, rc=rc)
      
      ! By default all of the mirrored fields will have "TransferActionGeomObject"
      ! set to "provide". This is because transfers are not transitive through the
      ! hierarchy right now!! This means that a grid that is being transferred from
      ! a child to its parent level, e.g. from unsatisfied fields in child import
      ! states, cannot also be transferred on to a sibling of the parent.
      
      ! For the case of the extData component the above is really more a protocol
      ! level limitation rather than a fundamental issue. Specifically, by the time
      ! the driver component (which is the parent to the sub-components) would need
      ! to transfer the grid to one of its sibling components, it actually does have
      ! the grid available to do so (already transferred from the children). However,
      ! the decision about whether this transfer is possible had to be made earlier,
      ! following current NUOPC protocol. At that time the driver was still on the 
      ! receiving end of the transfer on the lower level of the hierarchy, and could
      ! not act as a provider yet.
      
      ! The implementation in this routine works around this protocol limitation 
      ! and forces a transfer. It does so by directly setting the 
      ! "TransferActionGeomObject" attribute to "accept". This triggers the connector
      ! to go through the actual GeomObject transfer.
      !
      ! Note that "TransferActionGeomObject" is NOT an attribute that should typically
      ! be modified by user level code. However, we know what we are doing here, 
      ! so let's do it!

      nullify(fieldList)
      call NUOPC_GetStateMemberLists(exportState, fieldList=fieldList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      
      if (associated(fieldList)) then
        do i=1, size(fieldList)
          ! first diagnose what the default setting ended up being on the
          ! mirrored fields, expect "will provide"
          call NUOPC_GetAttribute(fieldList(i), name="TransferActionGeomObject",&
            value=value, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          call ESMF_LogWrite("TransferActionGeomObject in InitForceGridTransfer: "//&
            trim(value), ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          ! now force the TransferActionGeomObject setting on the mirrored fields
          ! to be "cannot provide"
          call NUOPC_SetAttribute(fieldList(i), name="TransferActionGeomObject",&
            value="accept", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        enddo
        deallocate(fieldList)
      endif
      
    end subroutine InitForceGridTransfer

  !-----------------------------------------------------------------------------



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
        type(ESMF_FieldStatus_Flag)   :: fieldStatus
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
        integer                :: ungrid

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

                call ESMF_FieldGet(field, status=fieldStatus, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__, &
                    file=__FILE__)) &
                    return  ! bail out

                if (fieldStatus/=ESMF_FIELDSTATUS_COMPLETE) then

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

                    call ESMF_FieldGet(field, grid=grid, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                        line=__LINE__, &
                        file=__FILE__)) &
                        return  ! bail out
                    call ESMF_AttributeGet(grid, name='GRID_LM', &
                                 value=UNGRID, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                        line=__LINE__, &
                        file=__FILE__)) &
                        return  ! bail out
                    print *, 'grid GRID_LM:', UNGRID

#define DEBUG_DISTGRID_off
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

!------------------------------------------------------------------
    subroutine DataInitialize( GC, RC )
!
! !INPUT/OUTPUT PARAMETERS:
      type(ESMF_GridComp) :: GC     ! Gridded component 
!
! !OUTPUT PARAMETERS:
      integer, intent(  out) :: RC     ! Error code

      ! locals
      type(ESMF_Clock) :: clock
      type(ESMF_State) :: importState, exportState
      
      RC = ESMF_SUCCESS
      
      call ESMF_LogWrite("EXTDATA:dataInitialize", ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! Initialize the data by reading the EXTDATA config file
      call initialize_(GC, RC)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

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
      
    end subroutine DataInitialize

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Initialize_ --- Initialize MAPL_ExtData
!
! !INTERFACE:
!

   SUBROUTINE Initialize_ ( GC, RC )

! !OUTPUT PARAMETERS:

   type(ESMF_GridComp), intent(inout) :: GC      ! Grid Component
   integer, intent(out)               :: rc      ! Error return code:


! !DESCRIPTION: This is a simple ESMF wrapper.
!
! !REVISION HISTORY:
!
!  12Dec2009  da Silva  Design and first implementation.
!
!EOP
!-------------------------------------------------------------------------

   type(MAPL_ExtData_state), pointer :: self        ! Legacy state
   type (MAPL_ExtData_wrap)          :: wrap
   type(ESMF_Grid)                   :: GRID        ! Grid
   type(ESMF_Config)                 :: CF_master          ! Universal Config 
   type(ESMF_State)                  :: EXPORT

   character(len=ESMF_MAXSTR)        :: comp_name
   character(len=ESMF_MAXSTR)        :: Iam
   integer                           :: Status
   character(len=ESMF_MAXSTR)        :: buffer

   type(PrimaryExports)              :: Primary
   type(PrimaryExport), pointer      :: item
   type(DerivedExports)              :: Derived
   type(DerivedExport), pointer      :: derivedItem 
   integer                           :: nLines, nCols
   integer                           :: i, iret
   integer                           :: ItemCount, itemCounter, j
   integer                           :: PrimaryItemCount, DerivedItemCount
   integer                           :: dims, vloc, knd, hw
   logical                           :: found

   type(ESMF_Time)                   :: time
   character(len=ESMF_MAXSTR)        :: VarName

   type (ESMF_Field)                 :: field,fieldnew
   integer                           :: fieldRank
   type (ESMF_FieldBundle)           :: bundle
   integer                           :: fieldcount
   type (ESMF_StateItem_Flag), pointer    :: ITEMTYPES(:)
   character(len=ESMF_MAXSTR), allocatable   :: ITEMNAMES(:)

   character(len=ESMF_MAXSTR),allocatable    :: PrimaryVarNames(:)
   character(len=ESMF_MAXSTR),allocatable    :: VarNames(:)
   integer                                   :: NumVarNames

!  logical to keep track of primary variables needed by derived fields, that ARE NOT in export state
   logical, allocatable              :: PrimaryVarNeeded(:)
   logical, allocatable              :: DerivedVarNeeded(:)
   logical, allocatable              :: LocalVarNeeded(:)

   type(ESMF_CFIO)                   :: cfio
   integer                           :: counter
   real, pointer                     :: ptr2d(:,:) => null()
   real, pointer                     :: ptr3d(:,:,:) => null()
   real                              :: const
   integer                           :: k, ios
   character(len=ESMF_MAXSTR)        :: c_offset, c_scale
   integer                           :: nlists, nlist
   character(len=ESMF_MAXSTR)        :: EXTDATA_CF
   type(ESMF_Config) :: CF
   type(ESMF_Config) :: localCF
   integer           :: totalPrimaryEntries
   integer           :: totalDerivedEntries
   logical           :: caseSensitiveVarNames
   character(len=ESMF_MAXSTR) :: component1,component2,expression
   integer           :: idx,nhms,ihr,imn,isc
   logical           :: isNegative
   integer           :: curTime, curDate, yy, mm, dd, hh, mn, ss, begDate, begTime, incSecs, fHours, tInterval
   integer           :: nymdB, nhmsB
   character(len=ESMF_MAXPATHLEN) :: file_processed
   character(len=ESMF_MAXSTR)     :: thisLine,vunit
   logical           :: inBlock
   type(ESMF_VM) :: vm
   type(ESMF_Clock) :: clock
   type(MAPL_MetaComp),pointer :: MAPLSTATE 

!  Get my name and set-up traceback handle
!  ---------------------------------------
   Iam = 'Initialize_'

   call ESMF_GridCompGet( GC, name=comp_name, config=CF, rc=rc )
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
   Iam = trim(comp_name) // '::' // trim(Iam)

   call NUOPC_ModelGet(GC, exportState = EXPORT, modelClock=clock, RC=RC)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

! Get information from export state
!----------------------------------
    call ESMF_StateGet(EXPORT, ITEMCOUNT=ItemCount, RC=STATUS)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    allocate ( self )
    wrap%ptr => self
    self%active = .true.
    caseSensitiveVarNames = .false.

    ! CF_master is GEOSCTM.rc, which is the config for ctm component
    ! set ExtData on by default, let user turn it off if they want
    !call ESMF_ConfigGetAttribute(CF_master,self%active, Label='USE_EXTDATA:',default=.true.,rc=status)

    ! set extdata to ignore case on variable names in files
    !call ESMF_ConfigGetAttribute(CF_master,caseSensitiveVarNames, Label='CASE_SENSITIVE_VARIABLE_NAMES:',default=.false.,rc=status)
!   Store internal state in GC
!   --------------------------
    call ESMF_UserCompSetInternalState ( GC, 'MAPL_ExtData_state', wrap, rc )
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    ! no need to run ExtData if there are no imports to fill
    if (ItemCount == 0) then
       self%active = .false.
    end if

    if (.not. self%active) then
       RETURN_(ESMF_SUCCESS)
    end if

!   Greetings
!   ---------
    call ESMF_LogWrite("ExtData:Initialize", ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    allocate(ITEMNAMES(ITEMCOUNT))
    allocate(ITEMTYPES(ITEMCOUNT))

    call ESMF_StateGet(EXPORT, ITEMNAMELIST=ITEMNAMES, &
                       ITEMTYPELIST=ITEMTYPES, RC=STATUS)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out


   totalPrimaryEntries=0
   totalDerivedEntries=0
   call ESMF_ConfigNextLine(CF,RC=status)
   do while (status == ESMF_SUCCESS) 
      call ESMF_ConfigNextLine(CF,rc=status)
      if (status == ESMF_SUCCESS) then 
         call ESMF_ConfigGetAttribute(CF,thisLine,rc=status)
         VERIFY_(STATUS)
         if (trim(thisLine) == "PrimaryExports%%" .or. trim(thisLine) == "DerivedExports%%" ) then
             call advanceAndCount(CF,nLines,rc=status)
             VERIFY_(STATUS)
            select case (trim(thisLine))
               case ("PrimaryExports%%")
                   totalPrimaryEntries = totalPrimaryEntries + nLines
               case ("DerivedExports%%")
                   totalDerivedEntries = totalDerivedEntries + nLines
            end select
         end if
      end if
   enddo

   primary%nItems = totalPrimaryEntries
   if (totalPrimaryEntries > 0) then
      allocate (PrimaryVarNames(totalPrimaryEntries), stat=STATUS)
      VERIFY_(STATUS)
      allocate (PrimaryVarNeeded(totalPrimaryEntries), stat=STATUS)
      VERIFY_(STATUS)
      PrimaryVarNeeded = .false.
      allocate(primary%item(totalPrimaryEntries), stat=STATUS)
      VERIFY_(STATUS)
   end if
   
   derived%nItems = totalDerivedEntries
   if (totalDerivedEntries > 0) then 
      Allocate(DerivedVarNeeded(totalDerivedEntries),stat=status)
      VERIFY_(STATUS)
      DerivedVarNeeded = .false.
      allocate(derived%item(totalDerivedEntries),stat=status)
      VERIFY_(STATUS) 
   end if

!  Primary Exports
!  ---------------

  ! Parse the config file for the second time from the beginning
   totalPrimaryEntries = 0
   totalDerivedEntries = 0
   ! reload file and parse it

   call ESMF_ConfigRewind(CF, rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
   !CF = ESMF_ConfigCreate (rc=STATUS )
   !VERIFY_(STATUS)
   !call ESMF_ConfigLoadFile(CF,EXTDATA_CF,rc=status)
   !VERIFY_(STATUS)

   call ESMF_ConfigNextLine(CF,rc=rc)
   do while(status == ESMF_SUCCESS) 

      call ESMF_ConfigNextLine(CF,rc=status)
      if (status == ESMF_SUCCESS) then
         call ESMF_ConfigGetAttribute(CF,thisLine,rc=status)
         if (trim(thisLine) == "PrimaryExports%%" .or. trim(thisLine) == "DerivedExports%%" ) then
            select case (trim(thisLine))
               case ("PrimaryExports%%")
                  inBlock = .true.
                  do while(inBLock)
                     call ESMF_ConfigNextLine(CF, rc=rc)
                     call ESMF_ConfigGetAttribute(CF,thisLine,rc=rc)
                     if (trim(thisLine) == "%%") then
                        inBlock = .false.
                     else

                        totalPrimaryEntries = totalPrimaryEntries + 1
                        ! name entry
                        primary%item(totalPrimaryEntries)%name = trim(thisLine)
                        !call ESMF_ConfigGetAttribute(CF, primary%item(totalPrimaryEntries)%name,  rc=rc)
                        PrimaryVarNames(totalPrimaryEntries) = primary%item(totalPrimaryEntries)%name
                        ! check if this represents a vector by looking for semicolon
                        primary%item(totalPrimaryEntries)%isVector = ( index(primary%item(totalPrimaryEntries)%name,';').ne.0 )
                        primary%item(totalPrimaryEntries)%vartype = MAPL_ExtDataVectorItem
                        primary%item(totalPrimaryEntries)%foundComp2 = .false.
                        primary%item(totalPrimaryEntries)%foundComp1 = .false.

                        ! units entry
                        call ESMF_ConfigGetAttribute(CF, primary%item(totalPrimaryEntries)%units, rc=rc)

                        !! horizontal information entry
                        !call ESMF_ConfigGetAttribute(CF, buffer, rc=rc)
                        !call ESMF_StringLowerCase(buffer, iret)
            
                        !if (buffer == 'xy') then
                           !primary%item(totalPrimaryEntries)%dim = MAPL_DimsHorzOnly
                        !else if (buffer == 'xyz') then
                           !primary%item(totalPrimaryEntries)%dim = MAPL_DimsHorzVert
                        !else
                           !__raise__(MAPL_RC_ERROR, "invalid dimension for Primary Export")
                        !end if

                        !! vertical information entry
                        !call ESMF_ConfigGetAttribute(CF, buffer, rc=rc)
                        !call ESMF_StringLowerCase(buffer, iret)
                        !if (buffer == 'c') then
                           !primary%item(totalPrimaryEntries)%vloc = MAPL_VLocationCenter
                        !else if (buffer == 'e') then
                           !primary%item(totalPrimaryEntries)%vloc = MAPL_VLocationEdge
                        !else
                           !__raise__(MAPL_RC_ERROR, "invalid vLocation for Primary Export")
                        !end if
                  
                        ! climatology entry
                        call ESMF_ConfigGetAttribute(CF, buffer, rc=rc)
                        buffer = ESMF_UtilStringLowerCase(buffer, rc=iret)
                        primary%item(totalPrimaryEntries)%cyclic=buffer

                       
                        ! regridding keyword, controls what type of regridding is performed
                        ! options are
                        ! N - conventional bilinear regridding
                        ! Y - conservative tile based regridding
                        ! V - voting, tile based
                        ! F;val - fractional, returns the fraction of the input cells with value, val
                        !         that overlap the target cell
                        call ESMF_ConfigGetAttribute(CF, buffer, rc=rc)
                        buffer = ESMF_UtilStringLowerCase(buffer, rc=iret)
                        buffer = trim(buffer)
                        if (trim(buffer) == 'y') then
                           primary%item(totalPrimaryEntries)%Trans = MAPL_HorzTransOrderBinning
                        else if (trim(buffer) == 'n') then
                           primary%item(totalPrimaryEntries)%Trans = MAPL_HorzTransOrderBilinear
                        else if (trim(buffer) == 'v') then
                           primary%item(totalPrimaryEntries)%Trans = MAPL_HorzTransOrderSample
                        else if (index(trim(buffer),'f') ==1 ) then
                           primary%item(totalPrimaryEntries)%Trans = MAPL_HorzTransOrderFraction
                           k = index(buffer,';')
                           ASSERT_(k > 0)
                           read(buffer(k+1:),*,iostat=ios) primary%item(totalPrimaryEntries)%FracVal
                        else
                           __raise__(MAPL_RC_ERROR, "the regridding keyword for extdata primary export must be N, Y, V, or F")
                        end if

                        ! refresh template entry
                        call ESMF_ConfigGetAttribute(CF, buffer, rc=rc)
                        ! check if first entry is an F for no interpolation
                        buffer = trim(buffer)
                        if (buffer(1:1) == 'F') then
                           primary%item(totalPrimaryEntries)%refresh_template = buffer(2:)
                           primary%item(totalPrimaryEntries)%doInterpolate = .false.
                        else
                           primary%item(totalPrimaryEntries)%refresh_template = buffer
                           primary%item(totalPrimaryEntries)%doInterpolate = .true.
                        end if
                        ! offset entry
                        call ESMF_ConfigGetAttribute(CF, c_offset, rc=rc)
                        if (trim(c_offset) == "none") then
                           primary%item(totalPrimaryEntries)%do_offset = .false.
                        else
                           primary%item(totalPrimaryEntries)%do_offset = .true.
                           read(c_offset,*,iostat=ios) primary%item(totalPrimaryEntries)%offset
                        end if
                        ! scaling entry
                        call ESMF_ConfigGetAttribute(CF, c_scale, rc=rc)
                        if (trim(c_scale) == "none") then
                           primary%item(totalPrimaryEntries)%do_scale = .false.
                        else
                           primary%item(totalPrimaryEntries)%do_scale = .true.
                           read(c_scale,*,iostat=ios) primary%item(totalPrimaryEntries)%scale
                        end if
                     
                        ! variable name on file entry
                        call ESMF_ConfigGetAttribute(CF, primary%item(totalPrimaryEntries)%var,    rc=rc)
                        ! file template entry
                        call ESMF_ConfigGetAttribute(CF, primary%item(totalPrimaryEntries)%file,   rc=rc)

                        ! the next  three are optional entries to describe the time information about the file template
                        ! these are what is the first valid time you can apply to the file template to get a file that exists
                        ! then you can specify the frequnecy of the file and the units of the frequency
                        call ESMF_ConfigGetAttribute(CF, primary%item(totalPrimaryEntries)%fileReffTime, rc=status)
                        if (status /= ESMF_SUCCESS) then
                           primary%item(totalPrimaryEntries)%FileReffTime = ""
                           primary%item(totalPrimaryEntries)%hasFileReffTime = .false.
                        else
                           primary%item(totalPrimaryEntries)%hasFileReffTime = .true.
                        end if            
 
              !         assume we will allocate
                        primary%item(totalPrimaryEntries)%ExtDataAlloc = .true.
              !         check if this is going to be a constant
                        primary%item(totalPrimaryEntries)%isConst = .false.
                        if (primary%item(totalPrimaryEntries)%file(1:9) == '/dev/null') then
                           primary%item(totalPrimaryEntries)%isConst = .true.
                           ios = -1
                           k = index(primary%item(totalPrimaryEntries)%file,':')
                           if ( k > 9 ) then
                                read(primary%item(totalPrimaryEntries)%file(k+1:),*,iostat=ios) primary%item(totalPrimaryEntries)%const
                           end if
                           if ( ios /= 0 ) primary%item(totalPrimaryEntries)%const = 0.0
                           ! finally override whatever the cyclic arguement is
                           primary%item(totalPrimaryEntries)%cyclic='n'
                        end if

                        if ( primary%item(totalPrimaryEntries)%isConst .eqv. .false. )  then
                           call CreateTimeInterval(primary%item(totalPrimaryEntries),clock,rc=rc)
                        end if
                     end if
                  enddo
               !  Derived Exports
               !  ---------------
               case ("DerivedExports%%")
                  inBlock = .true.
                  do while(inBlock)
                     call ESMF_ConfigNextLine(CF, rc=rc)
                     call ESMF_ConfigGetAttribute(CF,thisLine,rc=rc)
                     if (trim(thisLine) == "%%") then
                        inBlock = .false.
                     else
                        totalDerivedEntries = totalDerivedEntries + 1
                        derived%item(totalDerivedEntries)%name = trim(thisLine)
                        call ESMF_ConfigGetAttribute(CF,derived%item(totalDerivedEntries)%expression,rc=rc)
                        call ESMF_ConfigGetAttribute(CF,derived%item(totalDerivedEntries)%refresh_template, rc=rc)
                        derived%item(totalDerivedEntries)%ExtDataAlloc = .true.
                     end if
                  enddo
            end select
         end if
      end if
   end do

   PrimaryItemCount = 0
   DerivedItemCount = 0
   itemCounter = 0

!   find items in primary and derived to fullfill Export state
!   once we find primary or derived put in namespace
    self%ExtDataState = ESMF_StateCreate(Name="ExtDataNameSpace",rc=rc)
    do I = 1, ItemCount

       found = .false.
       do J = 1, primary%nItems
          ! special handling if it is a vector
          if (primary%item(J)%isVector) then
             idx = index(primary%item(J)%name,";")
             component1 = primary%item(J)%name(1:idx-1)
             component2 = primary%item(J)%name(idx+1:)
             if ( trim(ItemNames(I)) == trim(component1) ) then
                primary%item(j)%vcomp1 = component1
                idx = index(primary%item(j)%var,";")
                primary%item(j)%fcomp1 = primary%item(j)%var(1:idx-1)
                itemCounter = itemCounter + 1
                found = .true.
                primary%item(j)%foundComp1 = .true.
                PrimaryVarNeeded(j) = .true.
                primary%item(j)%ExtDataAlloc = .false.
                if ( primary%item(j)%foundComp1 .and. primary%item(j)%foundComp2 ) PrimaryItemCount = PrimaryItemCount + 1
                call ESMF_StateGet(Export,component1,field,rc=rc)
                call MAPL_StateAdd(self%ExtDataState,field,rc=rc)
                ! put protection in, if you are filling vector pair, they must be fields, no bundles
                ASSERT_( ITEMTYPES(I) == ESMF_StateItem_Field )
                exit
             else if ( trim(ItemNames(I)) == trim(component2) ) then
                primary%item(j)%vcomp2 = component2
                idx = index(primary%item(j)%var,";")
                primary%item(j)%fcomp2 = primary%item(j)%var(idx+1:)
                itemCounter = itemCounter + 1
                found = .true.
                primary%item(j)%foundComp2 = .true.
                PrimaryVarNeeded(j) = .true.
                primary%item(j)%ExtDataAlloc = .false.
                if ( primary%item(j)%foundComp1 .and. primary%item(j)%foundComp2 ) PrimaryItemCount = PrimaryItemCount + 1
                call ESMF_StateGet(Export,component2,field,rc=rc)
                call MAPL_StateAdd(self%ExtDataState,field,rc=rc)
                ! put protection in, if you are filling vector pair, they must be fields, no bundles
                ASSERT_( ITEMTYPES(I) == ESMF_StateItem_Field )
                exit
             end if
          else
             if (ItemNames(I) == primary%item(J)%name) then
                itemCounter = itemCounter + 1
                found = .true.
                if (primary%item(j)%isConst .and. ITEMTYPES(I) == ESMF_StateItem_FieldBundle) then
                   if (mapl_am_I_root()) write(*,*)'Can not have constant bundle in ExtData.rc file'
                   ASSERT_(.false.)
                end if
                PrimaryItemCount = PrimaryItemCount + 1
                PrimaryVarNeeded(j) = .true.
                primary%item(j)%ExtDataAlloc = .false.
                VarName=trim(primary%item(J)%name)
    
                if (ITEMTYPES(I) == ESMF_StateItem_Field) then
                   primary%item(J)%vartype = MAPL_FieldItem
                   call ESMF_StateGet(Export,VarName,field,rc=rc)
                   call MAPL_StateAdd(self%ExtDataState,field,rc=rc)
                else if (ITEMTYPES(I) == ESMF_StateItem_FieldBundle) then
                   primary%item(J)%vartype = MAPL_BundleItem
                   call ESMF_StateGet(Export,VarName,bundle,rc=rc)
                   call MAPL_StateAdd(self%ExtDataState,bundle,rc=rc)
                end if
                exit

             end if
          end if
       end do
       if ( (.not.found) .and. (derived%nItems > 0) ) then
          do J = 1, derived%nItems
             if (ItemNames(I) == derived%item(J)%name) then

                if (ITEMTYPES(I) == ESMF_StateItem_FieldBundle) then
                   ASSERT_(.false.)
                end if
                found = .true.
                DerivedVarNeeded(j) = .true.
                itemCounter = itemCounter + 1
                DerivedItemCount = DerivedItemCount + 1
                derived%item(j)%ExtDataAlloc = .false.
                VarName=derived%item(j)%name
                call ESMF_StateGet(Export,VarName,field,rc=rc)
                call MAPL_StateAdd(self%ExtDataState,field,rc=rc)
                exit

             end if
          end do
       end if
       if (.not.found) then
          if (mapl_am_I_root()) then
             write(*,*)'ExtData could not satisfy import ',trim(ItemNames(I))
          end if
       end if
    end do

    call ESMF_VMGetCurrent(VM) 
    call ESMF_VMBarrier(VM)
!   we have better found all the items in the export in either a primary or derived item
    if ( itemCounter /= ItemCount ) then
       if (mapl_am_I_root()) then
          write(*,'(A6,I3,A31)')'Found ',ItemCount-itemCounter,' unfullfilled imports in extdata'
       end if
        ASSERT_(.false.)
    end if

    NumVarNames=primary%nItems
    allocate(VarNames(NumVarNames))
    allocate(LocalVarNeeded(NumVarNames))
    do i=1,primary%nItems
       VarNames(i)=PrimaryVarNames(i)
    end do

    IF (MAPL_am_I_root()) THEN
       PRINT*, '************************************************'
       PRINT*, '** Variables to be provided by the ExtData Component **'
       PRINT*, '************************************************'
       do I = 1, ItemCount
          PRINT*, '---- ', I, ': ', TRIM(ItemNames(I))
       END DO
       PRINT*, '************************************************'
       PRINT*
    END IF

!   search for other primary variables we may need to fill derived types that were not in the export state
!   if we find them allocate them based on grid of variable we are trying to fill
    do i=1, derived%nItems
       if (DerivedVarNeeded(i)) then
          LocalVarNeeded=.false.

          ! first check if it is a non-arithmetic function
          expression = derived%item(i)%expression
          expression = ESMF_UtilStringLowerCase(expression, rc=iret)
          if ( index(expression,"mask") /=0  ) then
             derived%item(i)%masking = .true.
          else
             derived%item(i)%masking = .false.
          end if
          if (derived%item(i)%masking) then
             call GetMaskName(derived%item(i)%expression,VarNames,LocalVarNeeded,rc=rc)
          else
             call CheckSyntax(derived%item(i)%expression,VarNames,LocalVarNeeded,rc=rc)
          end if

          do j=1, primary%nItems
             if (LocalVarNeeded(j)) then
                VarName = trim(primary%item(j)%name)
                call ESMF_StateGet(self%ExtDataState,VarName,field,rc=status)
                if (status /= ESMF_SUCCESS) then
                   VarName = trim(derived%item(i)%name)
                   call ESMF_StateGet(self%ExtDataState,VarName,field,rc=rc)
                   VarName=trim(primary%item(j)%name)
                   fieldnew = MAPL_FieldCreate(field,varname,doCopy=.true.,rc=rc) 
                   call MAPL_StateAdd(self%ExtDataState,fieldnew,rc=rc)
                   PrimaryVarNeeded(j) = .true.
                   primary%item(j)%ExtDataAlloc = .true.
                   primary%item(j)%vartype = MAPL_FieldItem
                   PrimaryItemCount = PrimaryItemCount + 1
                end if
             end if
          end do
       end if
    end do

    self%primary%nItems = count(PrimaryVarNeeded)
    if (DerivedItemCount > 0) self%derived%nItems = count(DerivedVarNeeded)

    allocate(self%primary%item(PrimaryItemCount),__STAT__)
    if (DerivedItemCount > 0) allocate(self%derived%item(DerivedItemCount),__STAT__)

    counter = 0
    do i=1,primary%nItems
       if (PrimaryVarNeeded(i)) then
          counter = counter + 1
          self%primary%item(counter) = primary%item(i)
          ! put in a special check if it is a vector item
          ! both components must have bubbled up
          if (self%primary%item(counter)%isVector) then
             ASSERT_( self%primary%item(counter)%foundComp1 .and. self%primary%item(counter)%foundComp2 )
          end if
       end if
    end do
    ASSERT_(counter==PrimaryItemCount)

    if (DerivedItemCount > 0) then
       counter = 0
       do i=1,derived%nItems
          if (derivedVarNeeded(i)) then
             counter = counter + 1
             self%derived%item(counter) = derived%item(i)
          end if
       end do
       ASSERT_(counter==DerivedItemCount)
    end if

   PrimaryLoop: do i = 1, self%primary%nItems

      item => self%primary%item(i)

      ! parse refresh template to see if we have a time shift during constant updating
      k = index(item%refresh_template,';')
      call ESMF_TimeIntervalSet(item%tshift,rc=rc)
      if (k.ne.0) then
         ASSERT_(trim(item%refresh_template(:k-1))=="0")
         if (item%refresh_template(k+1:k+1) == '-' ) then
            isNegative = .true.
            read(item%refresh_template(k+2:),*,iostat=ios)nhms
         else
            isNegative = .false.
            read(item%refresh_template(k+1:),*,iostat=ios)nhms
         end if
         call MAPL_UnpackTime(nhms,ihr,imn,isc)
         if (isNegative) then
            ihr = -ihr
            imn = -imn
            isc = -isc
         end if
         call ESMF_TimeIntervalSet(item%tshift,h=ihr,m=imn,s=isc,rc=rc)
         item%refresh_template = "0"
      end if
      call SetRefreshAlarms(clock,primaryItem=item,rc=rc)

      if (item%vartype == MAPL_BundleItem) then

         call ESMF_ClockGet(CLOCK, currTIME=time, rc=rc)
         call ESMF_StateGet(self%ExtDataState, trim(item%name), bundle,rc=rc)
         ! let us check that bundle is empty
         call ESMF_FieldBundleGet(bundle, fieldcount = fieldcount , rc=rc)
         ASSERT_(fieldcount == 0)
         call MAPL_CFIORead(item%file,time,bundle,noread=.true.,ignorecase=self%ignorecase, only_vars=item%var,rc=rc)

      end if

!  Read the single step files (read interval equal to zero)
!  --------------------------------------------------------

      if (item%isConst) then

         if (item%vartype == MAPL_FieldItem) then
            call ESMF_StateGet(self%ExtDataState,trim(item%name),field,rc=rc)
            call ESMF_FieldGet(field,dimCount=fieldRank,rc=rc)
            if (fieldRank == 2) then
                  call MAPL_GetPointer(self%ExtDataState, ptr2d, trim(item%name),rc=rc)
                  ptr2d = item%const
            else if (fieldRank == 3) then
                  call MAPL_GetPointer(self%ExtDataState, ptr3d, trim(item%name), rc=rc)
                  ptr3d = item%const
            endif
         else if (item%vartype == MAPL_BundleItem) then
            ASSERT_(.false.)
         else if (item%vartype == MAPL_ExtDataVectorItem) then
            ! add this capability later
            ASSERT_(.false.)
         end if
         cycle
      end if

      ! check if this is a single piece of data if user put - for refresh template
      ! by that it is an untemplated file with one time that could not possibly be time interpolated
      if (PrimaryExportIsConstant_(item)) then
         if (index(item%file,'%') == 0) then
            cfio = ESMF_CFIOCreate(cfioObjname='cfio_obj',rc=rc)
            call ESMF_CFIOSet(cfio,fName=trim(item%file),rc=rc)
            call ESMF_CFIOFileOpen(cfio,fmode=1,rc=rc)
            if (cfio%tsteps == 1) then
               item%cyclic = 'single'
            end if
         end if
      end if

      ! get clim year if this is cyclic
      call GetClimYear(item,rc=rc)

      if (item%vartype == MAPL_FieldItem) then

         call ESMF_StateGet(self%ExtDataState, trim(item%name), field,rc=rc)
         
         item%finterp1 = MAPL_FieldCreate(field,item%var,doCopy=.true.,rc=rc)
         item%finterp2 = MAPL_FieldCreate(field,item%var,doCopy=.true.,rc=rc)

      else if (item%vartype == MAPL_BundleItem) then

         call ESMF_StateGet(self%ExtDataState, trim(item%name), bundle,rc=rc)
         call ESMF_FieldBundleGet(bundle,grid=grid,rc=rc)
         call ESMF_ClockGet(CLOCK, currTIME=time, rc=rc)
         item%binterp1 = ESMF_FieldBundleCreate( rc=rc)
         call ESMF_FieldBundleSet(item%binterp1, GRID=GRID, rc=rc)
         item%binterp2 = ESMF_FieldBundleCreate( rc=rc)
         call ESMF_FieldBundleSet(item%binterp2, GRID=GRID, rc=rc)
         call MAPL_CFIORead(item%file,time,item%binterp1,noread=.true.,ignorecase=self%ignorecase,only_vars=item%var,rc=rc)
         call MAPL_CFIORead(item%file,time,item%binterp2,noread=.true.,ignorecase=self%ignorecase,only_vars=item%var,rc=rc)
 
      else if (item%vartype == MAPL_ExtDataVectorItem) then
     
         ! check that we are not asking for conservative regridding
         if (item%Trans /= MAPL_HorzTransOrderBilinear) then
            if (mapl_am_i_root()) write(*,*)"No conservative regridding with vectors"
            ASSERT_(.false.)
         end if 

         call ESMF_StateGet(self%ExtDataState, trim(item%vcomp1), field,rc=rc)
         item%v1_finterp1 = MAPL_FieldCreate(field, item%vcomp1,doCopy=.true.,rc=rc)
         item%v1_finterp2 = MAPL_FieldCreate(field, item%vcomp1,doCopy=.true.,rc=rc)
         call ESMF_StateGet(self%ExtDataState, trim(item%vcomp2), field,rc=rc)
         item%v2_finterp1 = MAPL_FieldCreate(field, item%vcomp2,doCopy=.true.,rc=rc)
         item%v2_finterp2 = MAPL_FieldCreate(field, item%vcomp2,doCopy=.true.,rc=rc)

      end if

      allocate(item%refresh_time,__STAT__)

      call ESMF_TimeSet(item%refresh_time, yy=0, rc=rc)
   end do PrimaryLoop

   DerivedLoop: do i =1, self%derived%nItems
      allocate(self%derived%item(i)%refresh_time,__STAT__)

      derivedItem => self%derived%item(i)

      ! parse refresh template to see if we have a time shift during constant updating
      k = index(derivedItem%refresh_template,';')
      call ESMF_TimeIntervalSet(derivedItem%tshift,rc=rc)
      if (k.ne.0) then
         ASSERT_(trim(derivedItem%refresh_template(:k-1))=="0")
         if (derivedItem%refresh_template(k+1:k+1) == '-' ) then
            isNegative = .true.
            read(derivedItem%refresh_template(k+2:),*,iostat=ios)nhms
         else
            isNegative = .false.
            read(derivedItem%refresh_template(k+1:),*,iostat=ios)nhms
         end if
         call MAPL_UnpackTime(nhms,ihr,imn,isc)
         if (isNegative) then
            ihr = -ihr
            imn = -imn
            isc = -isc
         end if
         call ESMF_TimeIntervalSet(derivedItem%tshift,h=ihr,m=imn,s=isc,rc=rc)
         derivedItem%refresh_template = "0"
      end if

      call SetRefreshAlarms(clock,derivedItem=derivedItem,rc=rc)

      call ESMF_TimeSet(self%derived%item(i)%refresh_time, yy=0, rc=rc)

   end do DerivedLoop

#ifdef DEBUG
   if (MAPL_AM_I_ROOT()) then
      print *, trim(Iam)//': IMPORT   State during Initialize():'
      call ESMF_StatePrint ( IMPORT )
      print *
      print *, trim(Iam)//': EXPORT   State during Initialize():' 
      call ESMF_StatePrint ( EXPORT )
   end if
#endif

! Clean up
! --------
   if (associated(primary%item)) deallocate(primary%item)
   if (associated(derived%item)) deallocate(derived%item)
   deallocate(ItemTypes)
   deallocate(ItemNames)
   if (allocated(PrimaryVarNames)) deallocate(PrimaryVarNames)
   if (allocated(PrimaryVarNeeded)) deallocate(PrimaryVarNeeded)
   if (allocated(VarNames)) deallocate(VarNames)
   if (allocated(DerivedVarNeeded)) deallocate(DerivedVarNeeded)
   if (allocated(LocalVarNeeded)) deallocate(LocalVarNeeded)

!  Set has run to false to we know when we first go to run method it is first call
   hasRun = .false.

!  All done
!  --------
   RETURN_(ESMF_SUCCESS)

   END SUBROUTINE Initialize_
    
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

    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_State)              :: import, export
    type(ESMF_TimeInterval)       :: timestep

#if 1
   type(MAPL_ExtData_state), pointer :: self        ! Legacy state
   type(ESMF_Field)                  :: field       ! Field
   type(ESMF_FieldBundle)            :: bundle
   type(ESMF_Config)                 :: CF          ! Universal Config 

   type(PrimaryExport), pointer      :: item
   type(DerivedExport), pointer      :: derivedItem
   integer                           :: i, j

   type(ESMF_Time)                   :: time, refresh_time, time0
   logical                           :: time_interp, time_is_cyclic
   type(MAPL_MetaComp), pointer      :: MAPLSTATE

   real, pointer, dimension(:,:)     :: var2d_prev, var2d_next
   real, pointer, dimension(:,:,:)   :: var3d_prev, var3d_next
   logical                           :: doUpdate_
   integer                           :: fieldCount, fieldRank
   character(len=ESMF_MAXSTR), ALLOCATABLE  :: NAMES (:)
   type(ESMF_Field)                  :: field1, field2
   character(len=ESMF_MAXPATHLEN)    :: file_processed, file_processed1, file_processed2
   integer                           :: NX, NY
   logical                           :: NotSingle
   logical                           :: updateL, updateR, swap
   logical, allocatable              :: doUpdate(:)
   type(ESMF_Time), allocatable      :: useTime(:)

!  Get my name and set-up traceback handle
!  ---------------------------------------
      
   RC = ESMF_SUCCESS

   Iam = 'ModelAdvance'
   call ESMF_GridCompGet( GC, name=comp_name, __RC__ )
   call NUOPC_ModelGet(GC, modelClock=clock, importState=IMPORT, &
        exportState=EXPORT, rc=rc)
   Iam = trim(comp_name) // '::' // trim(Iam)

!  Extract relevant runtime information
!  ------------------------------------
   call extract_ ( GC, self, CF, __RC__ )

   if (.not. self%active) then
      RETURN_(ESMF_SUCCESS)
   end if

   call ESMF_ClockGet(CLOCK, currTIME=time0, __RC__)


!  Fill in the internal state with data from the files 
!  ---------------------------------------------------

   allocate(doUpdate(self%primary%nitems),stat=status)
   VERIFY_(STATUS)
   doUpdate = .false.
   allocate(useTime(self%primary%nitems),stat=status)
   VERIFY_(STATUS)

   READ_LOOP: do i = 1, self%primary%nItems

      item => self%primary%item(i)

      if (item%isConst) cycle

      NotSingle = .true.
      if (trim(item%cyclic) == 'single') NotSingle = .false.

      call CheckUpdate(doUpdate_,time,time0,hasRun,primaryItem=item,__RC__)
      doUpdate(i) = doUpdate_

      DO_UPDATE: if (doUpdate_) then

         HAS_RUN: if ( hasRun .eqv. .false.) then

            if (NotSingle) then
               ! update left time
               call UpdateBracketTime(item%file,time,item%reff_time,item%frequency,"L",item%cyclic,item%climYear,item%interp_time1, & 
                    item%time1,file_processed1,rc=status)
               VERIFY_(status)

               ! update right time
               call UpdateBracketTime(item%file,time,item%reff_time,item%frequency,"R",item%cyclic,item%climYear,item%interp_time2, &
                    item%time2,file_processed2,rc=status)
               VERIFY_(STATUS)
            else
               ! just get time on the file
               item%time1 = MAPL_ExtDataGetFStartTime(trim(item%file),__RC__)
               item%interp_time1 = item%time1
               file_processed1 = item%file
            end if
            ! call MAPL_TimerOff(MAPLSTATE,"--Bracket")

            ! read bracketing data

            !call MAPL_TimerOn(MAPLSTATE,"--Read")
            if (item%vartype == MAPL_FieldItem) then

               call MAPL_ExtDataGridCompRead(file_processed1,item%time1,item%trans, &
                    item%FracVal,self%ignorecase,field=item%finterp1,name=trim(item%var),__RC__)

               if (NotSingle) then
                  call MAPL_ExtDataGridCompRead(file_processed2,item%time2,item%trans, &
                       item%FracVal,self%ignorecase,field=item%finterp2,name=trim(item%var),__RC__)
               end if

            else if (item%vartype == MAPL_BundleItem) then

               call MAPL_ExtDataGridCompRead(file_processed1,item%time1,item%trans, &
                    item%FracVal,self%ignorecase,bundle=item%binterp1,__RC__)
               if (NotSingle) then
               call MAPL_ExtDataGridCompRead(file_processed2,item%time2,item%trans, &
                    item%FracVal,self%ignorecase,bundle=item%binterp2,__RC__)
               end if
          
            else if (item%vartype == MAPL_ExtDataVectorItem) then

               call ESMF_ConfigGetAttribute(cf, value = NX, Label="NX:", __RC__)
               call ESMF_ConfigGetAttribute(cf, value = NY, Label="NY:", __RC__)
               call MAPL_ExtDataReadVector(file_processed1,item%fcomp1,item%fcomp2, &
                                           item%time1,item%v1_finterp1,item%v2_finterp1,NX,NY,self%ignoreCase,__RC__)
               if (NotSingle) then
                  call MAPL_ExtDataReadVector(file_processed2,item%fcomp1,item%fcomp2, &
                                              item%time2,item%v1_finterp2,item%v2_finterp2,NX,NY,self%ignoreCase,__RC__)
               end if

            end if
            !call MAPL_TimerOff(MAPLSTATE,"--Read")

         endif HAS_RUN
 
         ! now update bracketing times if neccessary

         NOT_SINGLE: if (NotSingle) then

            if (time >= item%interp_time2) then
               ! normal flow assume clock is moving forward
               updateR = .true.
               updateL = .false.
               swap    = .true.
            else if (time < item%interp_time1) then
               ! the can only happen if clock was rewound like in replay update both
               updateR = .true.
               updateL = .true.
               swap    = .false.
            else
               updateR = .false.
               updateL = .false.
               swap    = .false.
            end if

            !call MAPL_TimerOn(MAPLSTATE,'--Swap')
            DO_SWAP: if (swap) then

               item%interp_time1 = item%interp_time2

               if (item%vartype == MAPL_FieldItem) then

                  call ESMF_FieldGet(item%finterp1, dimCount=fieldRank,__RC__)
                  if (fieldRank == 2) then
                     call ESMF_FieldGet(item%finterp1, localDE=0, farrayPtr=var2d_prev, __RC__)
                     call ESMF_FieldGet(item%finterp2, localDE=0, farrayPtr=var2d_next, __RC__)
                     var2d_prev=var2d_next
                  else if (fieldRank == 3) then
                     call ESMF_FieldGet(item%finterp1, localDE=0, farrayPtr=var3d_prev, __RC__)
                     call ESMF_FieldGet(item%finterp2, localDE=0, farrayPtr=var3d_next, __RC__)
                     var3d_prev=var3d_next
                  endif

               else if (item%vartype == MAPL_BundleItem) then

                  call ESMF_FieldBundleGet(item%binterp2, fieldCount = fieldCount, __RC__)
                  allocate(names(fieldCount),__STAT__)
                  call ESMF_FieldBundleGet(item%binterp2, fieldNameList = Names, __RC__)
                  do j = 1,fieldCount
                     call ESMF_FieldBundleGet(item%binterp1, names(j), field=field1, __RC__)
                     call ESMF_FieldBundleGet(item%binterp2, names(j), field=field2, __RC__)
                     call ESMF_FieldGet(field1, dimCount=fieldRank, __RC__) 
                     if (fieldRank == 2) then
                        call ESMF_FieldGet(field1, localDE=0, farrayPtr=var2d_prev, __RC__)
                        call ESMF_FieldGet(field2, localDE=0, farrayPtr=var2d_next, __RC__)
                        var2d_prev=var2d_next
                     else if (fieldRank == 3) then
                        call ESMF_FieldGet(field1, localDE=0, farrayPtr=var3d_prev, __RC__)
                        call ESMF_FieldGet(field2, localDE=0, farrayPtr=var3d_next, __RC__)
                        var3d_prev=var3d_next
                     endif
                  enddo

                  deallocate(names)

               else if (item%vartype == MAPL_ExtDataVectorItem) then

                  call ESMF_FieldGet(item%v1_finterp1, dimCount=fieldRank, __RC__) 
                  if (fieldRank == 2) then
                     call ESMF_FieldGet(item%v1_finterp1, localDE=0, farrayPtr=var2d_prev, __RC__)
                     call ESMF_FieldGet(item%v1_finterp2, localDE=0, farrayPtr=var2d_next, __RC__)
                     var2d_prev=var2d_next
                     call ESMF_FieldGet(item%v2_finterp1, localDE=0, farrayPtr=var2d_prev, __RC__)
                     call ESMF_FieldGet(item%v2_finterp2, localDE=0, farrayPtr=var2d_next, __RC__)
                     var2d_prev=var2d_next
                  else if (fieldRank == 3) then
                     call ESMF_FieldGet(item%v1_finterp1, localDE=0, farrayPtr=var3d_prev, __RC__)
                     call ESMF_FieldGet(item%v1_finterp2, localDE=0, farrayPtr=var3d_next, __RC__)
                     var3d_prev=var3d_next
                     call ESMF_FieldGet(item%v2_finterp1, localDE=0, farrayPtr=var3d_prev, __RC__)
                     call ESMF_FieldGet(item%v2_finterp2, localDE=0, farrayPtr=var3d_next, __RC__)
                     var3d_prev=var3d_next
                  endif

               end if

            end if DO_SWAP

            !call MAPL_TimerOff(MAPLSTATE,'--Swap')

            if (updateR) then

            !call MAPL_TimerOn(MAPLSTATE,'--Bracket')
            call UpdateBracketTime(item%file,time,item%reff_time,item%frequency,"R",item%cyclic,item%climYear,item%interp_time2, &
                    item%time2, file_processed, __RC__)
            !call MAPL_TimerOff(MAPLSTATE,'--Bracket')

               !call MAPL_TimerOn(MAPLSTATE,"--Read")
               if (item%vartype == MAPL_FieldItem) then

                  call MAPL_ExtDataGridCompRead(file_processed,item%time2,item%trans, &
                       item%FracVal,self%ignorecase,field=item%finterp2,name=trim(item%var),__RC__)

               else if (item%vartype == MAPL_BundleItem) then

                  call MAPL_ExtDataGridCompRead(file_processed,item%time2,item%trans, &
                       item%FracVal,self%ignorecase,bundle=item%binterp2,__RC__)

               else if (item%vartype == MAPL_ExtDataVectorItem) then

                  call ESMF_ConfigGetAttribute(cf, value = NX, Label="NX:", __RC__)
                  call ESMF_ConfigGetAttribute(cf, value = NY, Label="NY:", __RC__)
                  call MAPL_ExtDataReadVector(file_processed,item%fcomp1,item%fcomp2, & 
                                              item%time2,item%v1_finterp2,item%v2_finterp2,&
                                              NX,NY,self%ignoreCase,__RC__)

               end if
               !call MAPL_TimerOff(MAPLSTATE,"--Read")

            end if

            if (updateL) then

            !call MAPL_TimerOn(MAPLSTATE,'--Bracket')
               call UpdateBracketTime(item%file,time,item%reff_time,item%frequency,"L",item%cyclic,item%climYear,item%interp_time1, &
                    item%time1, file_processed, __RC__)
            !call MAPL_TimerOff(MAPLSTATE,'--Bracket')

               !call MAPL_TimerOn(MAPLSTATE,"--Read")
               if (item%vartype == MAPL_FieldItem) then

                  call MAPL_ExtDataGridCompRead(file_processed,item%time1,item%trans, &
                       item%FracVal,self%ignorecase,field=item%finterp1,name=trim(item%var),__RC__)

               else if (item%vartype == MAPL_BundleItem) then

                  call MAPL_ExtDataGridCompRead(file_processed,item%time1,item%trans, &
                       item%FracVal,self%ignorecase,bundle=item%binterp1,__RC__)

               else if (item%vartype == MAPL_ExtDataVectorItem) then

                  call ESMF_ConfigGetAttribute(cf, value = NX, Label="NX:", __RC__)
                  call ESMF_ConfigGetAttribute(cf, value = NY, Label="NY:", __RC__)
                  call MAPL_ExtDataReadVector(file_processed,item%fcomp1,item%fcomp1, & 
                                              item%time1,item%v1_finterp1,item%v2_finterp1,&
                                              NX,NY,self%ignoreCase,__RC__)

               end if
               !call MAPL_TimerOff(MAPLSTATE,"--Read")

            end if

         endif NOT_SINGLE

         useTime(i) = time

      end if DO_UPDATE

      if (PrimaryExportIsConstant_(item) .and. associated(item%refresh_time)) then
         deallocate(item%refresh_time)
         item%refresh_time => null()
      end if

   end do READ_LOOP

   !call MAPL_TimerOff(MAPLSTATE,"-Read_Loop")

   !call MAPL_TimerOn(MAPLSTATE,"-Interpolate")
 
   INTERP_LOOP: do i = 1, self%primary%nItems

      item => self%primary%item(i)

      if (doUpdate(i)) then
        
         ! finally interpolate between bracketing times

         if (item%vartype == MAPL_FieldItem) then

               call ESMF_StateGet(self%ExtDataState, item%name, field, __RC__)
               call MAPL_ExtDataInterpField(item,useTime(i),field,__RC__)

         else if (item%vartype == MAPL_BundleItem) then

               call ESMF_StateGet(self%ExtDataState, item%name, bundle, __RC__)
               call ESMF_FieldBundleGet(bundle, fieldCount = fieldCount, __RC__)
               allocate(names(fieldCount),__STAT__)
               call ESMF_FieldBundleGet(bundle, fieldNameList = Names, __RC__)
               do j = 1,fieldCount
                  call ESMF_FieldBundleGet(bundle,names(j), field=field, __RC__)
                  call MAPL_ExtDataInterpField(item,useTime(i),field,__RC__)
               enddo
               deallocate(names)

         else if (item%vartype == MAPL_ExtDataVectorItem) then

               call ESMF_StateGet(self%ExtDataState, item%vcomp1, field, __RC__)
               call MAPL_ExtDataInterpField(item,useTime(i),field,vector_comp=1,__RC__)
               call ESMF_StateGet(self%ExtDataState, item%vcomp2, field, __RC__)
               call MAPL_ExtDataInterpField(item,useTime(i),field,vector_comp=2,__RC__)
 
         end if

      endif

      nullify(item) 

   end do INTERP_LOOP

   !call MAPL_TimerOff(MAPLSTATE,"-Interpolate")

   ! now take care of derived fields
   do i=1,self%derived%nItems

      derivedItem => self%derived%item(i)

      call CheckUpdate(doUpdate_,time,time0,hasRun,derivedItem=deriveditem,__RC__)

      if (doUpdate_) then

         call CalcDerivedField(self%ExtDataState,self%primary,derivedItem%name,derivedItem%expression, &
              derivedItem%masking,__RC__)

      end if

      if (DerivedExportIsConstant_(derivedItem) .and. associated(derivedItem%refresh_time)) then
          deallocate(self%derived%item(i)%refresh_time)
          self%derived%item(i)%refresh_time => null()
      end if

   end do

!  All done
!  --------
   deallocate(doUpdate)
   deallocate(useTime)

   if (hasRun .eqv. .false.) hasRun = .true.
   !call MAPL_TimerOff(MAPLSTATE,"Run")
   !call MAPL_TimerOff(MAPLSTATE,"TOTAL")

#endif
   RETURN_(ESMF_SUCCESS)

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

! ............................................................................

   logical function PrimaryExportIsConstant_(item)
   
      type(PrimaryExport), intent(in) :: item

      __Iam__('PrimaryExportIsConstant_')


      if ( trim(item%refresh_template) == '-' .or. &
           trim(item%file) == '/dev/null' ) then
          PrimaryExportIsConstant_ = .true. 
      else
          PrimaryExportIsConstant_ = .false.
      end if

   end function PrimaryExportIsConstant_

! ............................................................................

   logical function DerivedExportIsConstant_(item)
   
      type(DerivedExport), intent(in) :: item

      __Iam__('DerivedExportIsConstant_')


      if ( trim(item%refresh_template) == '-') then
          DerivedExportIsConstant_ = .true. 
      else
          DerivedExportIsConstant_ = .false.
      end if

   end function DerivedExportIsConstant_

  subroutine AdvanceAndCount(CF,nLines,rc)

     type(ESMF_Config), intent(inout) :: cf
     integer, intent(out)             :: nLines
     integer, optional, intent(out)   :: rc

     integer :: iCnt
     logical :: inBlock
     character(len=ESMF_MAXSTR) :: thisLine
     integer :: status
     character(len=ESMF_MAXSTR) :: Iam
     Iam = "AdvanceAndCount"

     inBlock = .true.
     iCnt = 0
     do while(inBlock)
         call ESMF_ConfigNextLine(CF,rc=status)
         VERIFY_(STATUS)
         call ESMF_ConfigGetAttribute(CF,thisLine,rc=status)
         VERIFY_(STATUS)
         if (trim(thisLine) == "%%") then
            inBlock = .false.
         else
            iCnt = iCnt + 1
         end if
     end do
     nLines = iCnt

     RETURN_(ESMF_SUCCESS)

  end subroutine advanceAndCount
    
  subroutine GetMaskName(FuncStr,Var,Needed,rc)
     character(len=*),               intent(in)    :: FuncStr
     character(len=*),               intent(in)    :: Var(:)
     logical,                        intent(inout) :: needed(:)
     integer, optional,              intent(out)   :: rc

     character(len=ESMF_MAXSTR)      :: Iam = "GetMaskName"
     integer                         :: status
     integer                         :: i1,i2,i,ivar
     logical                         :: found,twovar
     character(len=ESMF_MAXSTR)      :: tmpstring,tmpstring1,tmpstring2,functionname

     i1 = index(Funcstr,"(")
     ASSERT_(i1 > 0)
     functionname = adjustl(Funcstr(:i1-1))
     functionname=ESMF_UtilStringLowerCase(functionname,rc=status)
     if (trim(functionname) == "regionmask") twovar = .true.
     if (trim(functionname) == "zonemask") twovar = .false.
     if (trim(functionname) == "boxmask") twovar = .false.
     tmpstring = adjustl(Funcstr(i1+1:))
     i1 = index(tmpstring,",")
     ASSERT_(i1 > 0)
     i2 = index(tmpstring,";")
     if (twovar) then
        tmpstring1 = adjustl(tmpstring(1:i1-1))
        tmpstring2 = adjustl(tmpstring(i1+1:i2-1))
     else
        tmpstring1 = adjustl(tmpstring(1:i1-1))
     end if

     found = .false.
     do i=1,size(var)
        if ( trim(tmpstring1) == trim(var(i)) ) then
           ivar = i
           found = .true.
           exit
        end if
     end do
     ASSERT_(found)
     needed(ivar) = .true.

     if (twovar) then
        found = .false.
        do i=1,size(var)
           if ( trim(tmpstring2) == trim(var(i)) ) then
              ivar = i
              found = .true.
              exit
           end if
        end do
        ASSERT_(found)
        needed(ivar) = .true.
     end if
     RETURN_(ESMF_SUCCESS)
  end subroutine GetMaskName

  subroutine SetRefreshAlarms(clock,primaryItem,derivedItem,rc) 
     type(ESMF_Clock),              intent(inout) :: Clock
     type(PrimaryExport), optional, intent(inout) :: primaryItem
     type(DerivedExport), optional, intent(inout) :: derivedItem
     integer,             optional, intent(out  ) :: rc

     integer                    :: pindex,cindex,iyy,imm,idd,ihh,imn,isc
     character(len=ESMF_MAXSTR) :: refresh_template,ctInt
     character(len=ESMF_MAXSTR) :: Iam
     type(ESMF_TimeInterval)    :: tInterval
     integer                    :: status
     Iam = "SetRefreshAlarms"

     if (present(primaryItem)) then
        refresh_template = primaryItem%refresh_template
     else if (present(derivedItem)) then
        refresh_template = derivedItem%refresh_template
     end if
     pindex = index(refresh_template,'P')
     if (pindex > 0) then
        ! now get time interval. Put 0000-00-00 in front if not there so parsetimeunits doesn't complain
        ctInt = refresh_template(pindex+1:)
        cindex = index(ctInt,'T')
        if (cindex == 0) ctInt = '0000-00-00T'//trim(ctInt)
        call MAPL_NCIOParseTimeUnits(ctInt,iyy,imm,idd,ihh,imn,isc,status)
        VERIFY_(STATUS)
        call ESMF_TimeIntervalSet(tInterval,yy=iyy,mm=imm,d=idd,h=ihh,m=imn,s=isc,rc=status)
        VERIFY_(STATUS) 
        if (present(primaryItem)) then 
           primaryItem%update_alarm = ESMF_AlarmCreate(clock=clock,ringInterval=tInterval,sticky=.false.,rc=status)
           VERIFY_(STATUS)
           primaryItem%alarmIsEnabled = .true.
        else if (present(derivedItem)) then
           DerivedItem%update_alarm = ESMF_AlarmCreate(clock=clock,ringInterval=tInterval,sticky=.false.,rc=status)
           VERIFY_(STATUS)
           derivedItem%alarmIsEnabled = .true.
        end if
     end if

     RETURN_(ESMF_SUCCESS)
  end subroutine SetRefreshAlarms

  subroutine CreateTimeInterval(item,clock,rc)
     type(PrimaryExport)      , intent(inout) :: item
     type(ESMF_Clock)          , intent(in   ) :: clock
     integer, optional         , intent(out  ) :: rc

     __Iam__('CreateTimeInterval')
     
     integer                    :: iyy,imm,idd,ihh,imn,isc
     integer                    :: lasttoken
     character(len=2)           :: token
     type(ESMF_Time)            :: time
     integer                    :: spos(2),cindex,pindex
     character(len=ESMF_MAXSTR) :: creffTime, ctInt
     
     creffTime = ''
     ctInt     = ''
     call ESMF_ClockGet (CLOCK, currTIME=time, __RC__)
     if (.not.item%hasFileReffTime) then
        ! if int_frequency is less than zero than try to guess it from the file template
        ! if that fails then it must be a single file or a climatology 

        call ESMF_TimeGet(time, yy=iyy, mm=imm, dd=idd,h=ihh, m=imn, s=isc  ,__RC__)
        lasttoken = index(item%file,'%',back=.true.)
        if (lasttoken.gt.0) then
           token = item%file(lasttoken+1:lasttoken+2)
           select case(token)
           case("y4") 
              call ESMF_TimeSet(item%reff_time,yy=iyy,mm=1,dd=1,h=0,m=0,s=0,__RC__)
              call ESMF_TimeIntervalSet(item%frequency,yy=1,__RC__)
           case("m2") 
              call ESMF_TimeSet(item%reff_time,yy=iyy,mm=imm,dd=1,h=0,m=0,s=0,__RC__)
              call ESMF_TimeIntervalSet(item%frequency,mm=1,__RC__)
           case("d2") 
              call ESMF_TimeSet(item%reff_time,yy=iyy,mm=imm,dd=idd,h=0,m=0,s=0,__RC__)
              call ESMF_TimeIntervalSet(item%frequency,d=1,__RC__)
           case("h2") 
              call ESMF_TimeSet(item%reff_time,yy=iyy,mm=imm,dd=idd,h=ihh,m=0,s=0,__RC__)
              call ESMF_TimeIntervalSet(item%frequency,h=1,__RC__)
           case("n2") 
              call ESMF_TimeSet(item%reff_time,yy=iyy,mm=imm,dd=idd,h=ihh,m=imn,s=0,__RC__)
              call ESMF_TimeIntervalSet(item%frequency,m=1,__RC__)
           end select
        else
           ! couldn't find any tokens so all the data must be on one file
           call ESMF_TimeIntervalSet(item%frequency,__RC__)
        end if
     else
        ! Get refference time, if not provided use current model date
        pindex=index(item%FileReffTime,'P')
        if (pindex==0) then 
           ASSERT_(.false.)
        end if
        cReffTime = item%FileReffTime(1:pindex-1) 
        if (trim(cReffTime) == '') then
           item%reff_time = Time
        else
           call MAPL_NCIOParseTimeUnits(cReffTime,iyy,imm,idd,ihh,imn,isc,status)
           VERIFY_(STATUS)
           call ESMF_TimeSet(item%reff_time,yy=iyy,mm=imm,dd=idd,h=ihh,m=imn,s=isc,rc=status)
           VERIFY_(STATUS)
        end if
        ! now get time interval. Put 0000-00-00 in front if not there so parsetimeunits doesn't complain
        ctInt = item%FileReffTime(pindex+1:)
        cindex = index(ctInt,'T')
        if (cindex == 0) ctInt = '0000-00-00T'//trim(ctInt)
        call MAPL_NCIOParseTimeUnits(ctInt,iyy,imm,idd,ihh,imn,isc,status)
        VERIFY_(STATUS)
        call ESMF_TimeIntervalSet(item%frequency,yy=iyy,mm=imm,d=idd,h=ihh,m=imn,s=isc,rc=status)
        VERIFY_(STATUS) 
     end if

     RETURN_(ESMF_SUCCESS) 

  end subroutine CreateTimeInterval

  subroutine GetClimYear(item, rc)

     type(PrimaryExport)      , intent(inout) :: item
     integer, optional        , intent(out  ) :: rc

     __Iam__('GetClimYear')

     type(ESMF_CFIO)            :: cfio
     integer(ESMF_KIND_I4)      :: iyr,imm,idd
     integer                    :: begDate
     type(ESMF_TimeInterval)    :: zero
     integer                    :: lasttoken
     character(len=ESMF_MAXPATHLEN) :: file
     character(len=2)           :: token
     integer                    :: nymd, nhms, climYear
     character(len=ESMF_MAXSTR) :: buffer
     logical                    :: inRange

     buffer = trim(item%cyclic)

     if (trim(buffer) == 'n') then

        item%cyclic = "n"
        RETURN_(ESMF_SUCCESS)

     else if (trim(buffer) == 'single') then
   
        RETURN_(ESMF_SUCCESS)

     else if (trim(buffer) == 'y') then

        item%cyclic = "y"

        call ESMF_TimeIntervalSet(zero,__RC__)
        cfio =  ESMF_CFIOCreate (cfioObjName='cfio_obj',__RC__)

        if (item%frequency == zero) then
           file = item%file
        else
           lasttoken = index(item%file,'%',back=.true.)
           token = item%file(lasttoken+1:lasttoken+2)
           ASSERT_(token == "m2")
           ! just put a time in so we can evaluate the template to open a file
           nymd = 20000101
           nhms = 0
           call ESMF_CFIOStrTemplate(file,item%file,'GRADS',nymd=nymd,nhms=nhms,__STAT__)
        end if
        call ESMF_CFIOSet(CFIO, fName=trim(file),__RC__)
        call ESMF_CFIOFileOpen  (CFIO, FMODE=1,__RC__)
        begDate = cfio%date
        call MAPL_UnpackTime(begDate,iyr,imm,idd)
        item%climyear = iyr
        call ESMF_CFIODestroy(CFIO,__RC__)
        RETURN_(ESMF_SUCCESS)
     else
        read(buffer,'(I4)') climYear
        inRange = 0 <= climYear .and. climYear <= 3000
        if (inRange) then
           item%cyclic = "y"
           item%climYear = climYear
           RETURN_(ESMF_SUCCESS)
        else
           WRITE(*,*)'cyclic keyword was not y, n, or a year'
           ASSERT_(.false.)
        end if
     end if

  end subroutine GetClimYear

 subroutine extract_ ( GC, self, CF, rc)

    type(ESMF_GridComp), intent(INout)  :: GC           ! Grid Comp object

    type(MAPL_ExtData_state), pointer   :: self         ! Legacy state
    type(ESMF_Config),   intent(out)    :: CF           ! Universal Config 

    integer, intent(out), optional      :: rc

!                            ---

    character(len=ESMF_MAXSTR) :: comp_name
    character(len=ESMF_MAXSTR) :: Iam
    integer                    :: status

    type(MAPL_ExtData_Wrap)  :: wrap

!   Get my name and set-up traceback handle
!   ---------------------------------------
    Iam = 'extract_'
    call ESMF_GridCompGet( GC, NAME=comp_name, __RC__ )
    Iam = trim(COMP_NAME) // '::' // trim(Iam)

    rc = 0

!   Get my internal state
!   ---------------------
    call ESMF_UserCompGetInternalState(gc, 'MAPL_ExtData_state', WRAP, STATUS)
    VERIFY_(STATUS)
    self => wrap%ptr

!   Get the configuration
!   ---------------------
    call ESMF_GridCompGet ( GC, config=CF, __RC__ )

    
    RETURN_(ESMF_SUCCESS)

  end subroutine extract_

  subroutine UpdateBracketTime(file_tmpl,cTime,reffTime,frequency,bSide,cyclic,climYear,interpTime,fileTime,file_processed,rc)
     character(len=*          ),          intent(in   ) :: file_tmpl
     type(ESMF_Time),                     intent(inout) :: cTime
     type(ESMF_Time),                     intent(inout) :: reffTime
     type(ESMF_TimeInterval),             intent(inout) :: frequency
     character(len=1),                    intent(in   ) :: bSide
     character(len=*),                    intent(in   ) :: cyclic
     integer,                             intent(in   ) :: climYear
     type(ESMF_TIME),                     intent(inout) :: interpTime
     type(ESMF_TIME),                     intent(inout) :: fileTime
     character(len=*),                    intent(inout) :: file_processed
     integer, optional,                   intent(out  ) :: rc

     __Iam__('UpdateBracketTime')

     type(ESMF_Time)                            :: newTime
     integer                                    :: curDate,curTime,n
     integer(ESMF_KIND_I4)                      :: iyr, imm, idd, ihr, imn, isc, cYear,cMonth,oldyear
     type(ESMF_TimeInterval)                    :: tint
     type(ESMF_TimeInterval)                    :: zero
     type(ESMF_Time)                            :: fTime
     logical                                    :: UniFileClim
     type(ESMF_Time)                            :: readTime,nTime
  
     call ESMF_TimeIntervalSet(zero,__RC__)
     if (frequency == zero) then
        UniFileClim = .false.
        ! if the file is constant, i.e. no tokens in in the template
        ! but it was marked as cyclic we must have a year long climatology 
        ! on one file, set UniFileClim to true
        if (trim(cyclic)/='n') then
            UniFileClim = .true.
            call GetBracketTimeOnFile(file_tmpl,cTime,bSide,UniFileClim,interpTime,fileTime,climYear=climYear,rc=status)
        else
            call GetBracketTimeOnFile(file_tmpl,cTime,bSide,UniFileClim,interpTime,fileTime,rc=status)
        end if
        file_processed = file_tmpl
        if (status /= ESMF_SUCCESS) then
           RETURN_(ESMF_FAILURE)
        end if
     else
 
        UniFileClim = .false.

        readTime = cTime
        if (trim(cyclic)=='y') then
           ! if climatology, get current year, substitute climatological year
           call ESMF_TimeGet(cTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
           cYear = iyr
           iyr = climYear
           if (imm == 2 .and. idd==29) idd = 28
           call ESMF_TimeSet(readTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
        end if
         
        call TimeIntervalFloor(reffTime,readTime,frequency,fTime,__RC__)
        ! untemplate file

        call ESMF_TimeGet(fTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)

        call MAPL_PackTime(curDate,iyr,imm,idd)
        call MAPL_PackTime(curTime,ihr,imn,isc)
        call gx_(file_processed,file_tmpl,nymd=curDate,nhms=curTime,__STAT__)
        ! try to get bracketing time on file using current time
        call GetBracketTimeOnFile(file_processed,readTime,bSide,UniFileClim,interpTime,fileTime,rc=status)
        
        ! if we didn't find the bracketing time look forwards or backwards depending on
        ! whether it is the right or left time   
        if (status /= ESMF_SUCCESS) then

           if (bSide == "R") then

              ! check next time
              call ESMF_TimeGet(fTime,yy=oldyear)
              newTime = fTime + frequency
              if (trim(cyclic)=='y') then
                 call ESMF_TimeGet(newTime,yy=iyr,__RC__)
                 if (oldyear/=iyr) then 
                    iyr = climYear-1
                    cYear=cYear+1
                    call ESMF_TimeGet(readTime,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                    call ESMF_TimeSet(readTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                    call ESMF_TimeGet(newTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                    call ESMF_TimeSet(newTime,yy=climYear,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                 end if
              end if

              call ESMF_TimeGet(newTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
              call MAPL_PackTime(curDate,iyr,imm,idd)
              call MAPL_PackTime(curTime,ihr,imn,isc)
              call gx_(file_processed,file_tmpl,nymd=curDate,nhms=curTime,__STAT__)
              ! try to get bracketing time on file using new time
              call GetBracketTimeOnFile(file_processed,readTime,bSide,UniFileClim,interpTime,fileTime,rc=status)

              if (status /= ESMF_SUCCESS) then
                 if (mapl_am_I_root()) write(*,*)'ExtData could not find bracketing data from file template ',trim(file_tmpl),' for side ',bSide
                 RETURN_(ESMF_FAILURE)

              end if

           else if (bSide == "L") then

              ! check next time
              call ESMF_TimeGet(fTime,yy=oldyear)
              newTime = fTime - frequency
              if (trim(cyclic)=='y') then
                 call ESMF_TimeGet(newTime,yy=iyr,__RC__)
                 if (oldyear/=iyr) then
                    iyr = climYear+1
                    cYear=cYear-1
                    call ESMF_TimeGet(readTime,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                    call ESMF_TimeSet(readTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                    call ESMF_TimeGet(newTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                    call ESMF_TimeSet(newTime,yy=climYear,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                 end if
              end if

              call ESMF_TimeGet(NewTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
              call MAPL_PackTime(curDate,iyr,imm,idd)
              call MAPL_PackTime(curTime,ihr,imn,isc)
              call gx_(file_processed,file_tmpl,nymd=curDate,nhms=curTime,__STAT__)
              ! try to get bracketing time on file using new time
              call GetBracketTimeOnFile(file_processed,readTime,bSide,UniFileClim,interpTime,fileTime,rc=status)

              if (status /= ESMF_SUCCESS) then
                 if (mapl_am_I_root()) write(*,*)'ExtData could not find bracketing data from file template ',trim(file_tmpl),' for side ',bSide
                 RETURN_(ESMF_FAILURE)

              end if

           end if

        end if

        if (trim(cyclic)=='y') then
           call ESMF_TimeGet(fileTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
           call ESMF_TimeSet(interpTime,yy=cYear,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
        end if
        RETURN_(ESMF_SUCCESS)

     end if
    
  end subroutine UpdateBracketTime

  subroutine GetBracketTimeOnFile(file,cTime,bSide,UniFileClim,interpTime,fileTime,climYear,rc)
     character(len=*),                    intent(in   ) :: file
     type(ESMF_Time),                     intent(inout) :: cTime
     character(len=1),                    intent(in   ) :: bSide
     logical,                             intent(in   ) :: UniFileClim
     type(ESMF_TIME),                     intent(inout) :: interpTime
     type(ESMF_TIME),                     intent(inout) :: fileTime
     integer, optional,                   intent(in   ) :: climYear
     integer, optional,                   intent(out  ) :: rc

     __Iam__('GetBracketTimeOnFile')

     type(ESMF_CFIO)                    :: cfio
     integer(ESMF_KIND_I4)              :: iyr,imm,idd,ihr,imn,isc,curYear
     integer                            :: iCurrInterval,i
     integer                            :: nhmsB, nymdB, incSecs
     integer                            :: begDate, begTime
     type(ESMF_Time), pointer           :: tSeries(:)
     type(ESMF_Time)                    :: climTime
     logical                            :: found,foundYear
     integer                            :: fSize,tSteps,iStart

     cfio =  ESMF_CFIOCreate (cfioObjName='cfio_obj',__RC__)
     call ESMF_CFIOSet(CFIO, fName=trim(file),__RC__)
     call ESMF_CFIOFileOpen  (CFIO, FMODE=1, __RC__)
     call GetBegDateTime(cfio%fid,begDate,begTime,incSecs,__RC__)
     fSize = cfio%tSteps
     climTime = cTime

     if (UniFileClim) then
        call MAPL_UnpackTime(begDate,iyr,imm,idd)
        call ESMF_TimeGet(cTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
        ASSERT_(present(climYear))
        iyr = climYear
        if (idd == 29 .and. imm == 2) idd = 28
        call ESMF_TimeSet(climTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)

        ! pick out times from the file that are in the climatological year
        foundYear = .false.
        tSteps = 0
        do i=1,fSize
           iCurrInterval = (i-1)*incSecs
           call GetDate ( begDate, begTime, iCurrInterval, nymdB, nhmsB, status )
           call MAPL_UnpackTime(nymdB,iyr,imm,idd)
           if (iyr==climYear) then
              if (foundYear .eqv. .false.) then 
                 istart = i
                 foundYear = .true.
              end if
              tSteps=tSteps+1
           end if
        enddo
        iCurrInterval = (iStart-1)*incSecs
        call GetDate ( begDate, begTime, iCurrInterval, nymdB, nhmsB, status )
        begDate=nymdB
        begTime=nhmsB
        fSize = tSteps
     end if

     allocate(tSeries(fsize))
     do i=1,fsize
        iCurrInterval = (i-1)*incSecs
        call GetDate ( begDate, begTime, iCurrInterval, nymdB, nhmsB, status )
        call MAPL_UnpackTime(nymdB,iyr,imm,idd)
        call MAPL_UnpackTime(nhmsB,ihr,imn,isc)
        call ESMF_TimeSet(tSeries(i), yy=iyr, mm=imm, dd=idd,  h=ihr,  m=imn, s=isc,__RC__)
     enddo
     found = .false.
     ! we will have to specially handle a climatology in one file
     ! might be better way but can not think of one
     if (bSide == "L") then
        if ( UniFileClim .and. (climTime < tSeries(1)) ) then
           fileTime = tSeries(fsize)
           call ESMF_TimeGet(tSeries(fsize),yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
           call ESMF_TimeGet(cTime,yy=curYear,__RC__)
           iyr = curYear - 1 
           call ESMF_TimeSet(interpTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
           found = .true.
        else
           do i=fsize,1,-1
              if (climTime >= tSeries(i)) then
                 fileTime = tSeries(i)
                 if (UniFileClim) then
                    call ESMF_TimeGet(cTime,yy=curYear,__RC__)
                    call ESMF_TimeGet(fileTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                    call ESMF_TimeSet(interpTime,yy=curYear,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                 else
                    interpTime = tSeries(i)
                 end if
                 found = .true.
                 exit
              end if
           end do
        end if
     else if (bSide == "R") then
        if (UniFileClim .and. (climTime >= tSeries(fsize)) ) then
           fileTime = tSeries(1)
           call ESMF_TimeGet(tSeries(1),yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
           call ESMF_TimeGet(cTime,yy=curYear,__RC__)
           iyr = curYear + 1 
           call ESMF_TimeSet(interpTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
           found = .true.
        else
           do i=1,fsize
              if (climTime < tSeries(i)) then
                 fileTime = tSeries(i)
                 if (UniFileClim) then
                    call ESMF_TimeGet(cTime,yy=curYear,__RC__)
                    call ESMF_TimeGet(fileTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                    call ESMF_TimeSet(interpTime,yy=curYear,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                 else
                    interpTime = tSeries(i)
                 end if
                 found = .true.
                 exit
              end if
           end do
        end if
     end if              
     call ESMF_CFIODestroy(CFIO,__RC__)
     deallocate(tSeries)
     if (found) then
        rc=ESMF_SUCCESS
        return
     else
        rc=ESMF_FAILURE
        return
     endif
     !end if 

  end subroutine GetBracketTimeOnFile

  subroutine TimeIntervalFloor(rTime,cTime,freq,fTime,rc)
     type(ESMF_Time), intent(inout) :: rTime
     type(ESMF_Time), intent(inout) :: cTime
     type(ESMF_TimeInterval), intent(inout) :: freq
     type(ESMF_Time), intent(inout) :: fTime
     integer, optional, intent(out) :: rc

     integer :: status
     character(len=ESMF_MAXSTR) :: Iam
     type(ESMF_Time) :: nTime, tLast,tNext
     logical         :: found

     ! for a given rTime and freq, find the floor of the two times
     ! that bracket cTime without doing division on esmf time intervals
     found = .false.
     nTime = rTime+freq
     if (nTime > cTime .and. cTime >= rTime) then
        fTime = rTime
        found = .true.
     else if (ctime < rTime) then
        tLast = rTime
        do while(.not.found)
           tNext = tLast - freq
           if (tLast > cTime .and. cTime >= tNext) then
              found = .true.
              fTime = tNext
           end if
           tlast = tNext
        enddo
     else if (ctime >= nTime) then
        tLast = nTime
        do while(.not.found)
           tNext = tLast + freq
           if (tNext > cTime .and. cTime >= tLast) then
              found = .true.
              fTime = tLast
           end if
           tlast = tNext
        enddo
     end if
        
 
     Iam = "TimeIntervalFloor"

     RETURN_(ESMF_SUCCESS)

  end subroutine TimeIntervalFloor

#if 1
 subroutine CalcDerivedField(state,primaries,exportName,exportExpr,masking,rc)
     type(ESMF_State),        intent(inout) :: state
     type(PrimaryExports),    intent(inout) :: primaries
     character(len=*),        intent(in   ) :: exportName     
     character(len=*),        intent(in   ) :: exportExpr
     logical,                 intent(in   ) :: masking               
     integer, optional,       intent(out  ) :: rc

     __Iam__('CalcDerivedField')

     type(ESMF_Field)                   :: field

     if (masking) then
        call MAPL_ExtDataEvaluateMask(state,exportName,exportExpr,__RC__)
     else
        call ESMF_StateGet(state,exportName,field,__RC__)
        call MAPL_StateEval(state,exportExpr,field,__RC__)
     end if
     RETURN_(ESMF_SUCCESS)
  end subroutine CalcDerivedField

  subroutine MAPL_ExtDataInterpField(item,time,field,vector_comp,rc)
     type(PrimaryExport), intent(inout) :: item
     type(ESMF_Time),     intent(in   ) :: time
     type(ESMF_Field),    intent(inout) :: field
     integer, optional,   intent(in   ) :: vector_comp
     integer, optional,   intent(out  ) :: rc

     character(len=ESMF_MAXSTR) :: Iam
     integer                    :: status

     type(ESMF_TimeInterval)    :: tinv1, tinv2
     real                       :: alpha
     real, pointer              :: var2d(:,:)   => null()
     real, pointer              :: var3d(:,:,:) => null()
     real, pointer              :: var2d_prev(:,:)   => null() 
     real, pointer              :: var2d_next(:,:)   => null()
     real, pointer              :: var3d_prev(:,:,:) => null()
     real, pointer              :: var3d_next(:,:,:) => null()
     integer                    :: fieldRank,i,j,k
     character(len=ESMF_MAXSTR) :: name


     Iam = "MAPL_ExtDataInterpField"
     tinv1 = time - item%interp_time1
     tinv2 = item%interp_time2 - item%interp_time1
     alpha = tinv1/tinv2
     call ESMF_FieldGet(FIELD, dimCount=fieldRank,name=name,__RC__)
     if (fieldRank == 2) then
           if (item%vartype == MAPL_FieldItem) then
              call ESMF_FieldGet(item%finterp1, localDE=0, farrayPtr=var2d_prev, __RC__)
              call ESMF_FieldGet(item%finterp2, localDE=0, farrayPtr=var2d_next, __RC__)
           else if (item%vartype == MAPL_BundleItem) then
              call ESMFL_BundleGetPointerToData(item%binterp1,name,var2d_prev,__RC__)
              call ESMFL_BundleGetPointerToData(item%binterp2,name,var2d_next,__RC__)
           else if (item%vartype == MAPL_ExtDataVectorItem) then
              ASSERT_(present(vector_comp))
              if (vector_comp == 1) then
                 call ESMF_FieldGet(item%v1_finterp1, localDE=0, farrayPtr=var2d_prev, __RC__)
                 call ESMF_FieldGet(item%v1_finterp2, localDE=0, farrayPtr=var2d_next, __RC__)
              else if (vector_comp == 2) then
                 call ESMF_FieldGet(item%v2_finterp1, localDE=0, farrayPtr=var2d_prev, __RC__)
                 call ESMF_FieldGet(item%v2_finterp2, localDE=0, farrayPtr=var2d_next, __RC__)
              end if
           end if
           call ESMF_FieldGet(field, localDE=0, farrayPtr=var2d, __RC__)
           ! only interpolate if we have to
           if (time == item%interp_time1 .or. item%doInterpolate .eqv. .false.) then
              var2d = var2d_prev
           else if (time == item%interp_time2) then
              var2d = var2d_next
           else
              do j=1,size(var2d,2)
                 do i=1,size(var2d,1)
                    if (var2d_next(i,j) /= MAPL_UNDEF .and. var2d_prev(i,j) /= MAPL_UNDEF) then
                        var2d(i,j) = var2d_prev(i,j) + alpha*(var2d_next(i,j)-var2d_prev(i,j))
                    else
                        var2d(i,j) = MAPL_UNDEF
                    end if
                 enddo
              enddo
           end if
           do j=1,size(var2d,2)
              do i=1,size(var2d,1)
                 if (var2d(i,j) /= MAPL_UNDEF) then
                    if (item%do_scale .and. (.not.item%do_offset)) var2d(i,j) = item%scale*var2d(i,j)
                    if ((.not.item%do_scale) .and. item%do_offset) var2d(i,j) = var2d(i,j)+item%offset
                    if (item%do_scale .and. item%do_offset) var2d(i,j) = item%offset + (item%scale * var2d(i,j))
                 else
                     var2d(i,j) = MAPL_UNDEF
                 end if
              enddo
           enddo
      else if (fieldRank == 3) then
           if (item%vartype == MAPL_FieldItem) then
              call ESMF_FieldGet(item%finterp1, localDE=0, farrayPtr=var3d_prev, __RC__)
              call ESMF_FieldGet(item%finterp2, localDE=0, farrayPtr=var3d_next, __RC__)
           else if (item%vartype == MAPL_BundleItem) then
              call ESMFL_BundleGetPointerToData(item%binterp1,name,var3d_prev,__RC__)
              call ESMFL_BundleGetPointerToData(item%binterp2,name,var3d_next,__RC__)
           else if (item%vartype == MAPL_ExtDataVectorItem) then
              ASSERT_(present(vector_comp))
              if (vector_comp == 1) then
                 call ESMF_FieldGet(item%v1_finterp1, localDE=0, farrayPtr=var3d_prev, __RC__)
                 call ESMF_FieldGet(item%v1_finterp2, localDE=0, farrayPtr=var3d_next, __RC__)
              else if (vector_comp == 2) then
                 call ESMF_FieldGet(item%v2_finterp1, localDE=0, farrayPtr=var3d_prev, __RC__)
                 call ESMF_FieldGet(item%v2_finterp2, localDE=0, farrayPtr=var3d_next, __RC__)
              end if
           end if
           call ESMF_FieldGet(field, localDE=0, farrayPtr=var3d, __RC__)
           ! only interpolate if we have to
           if (time == item%interp_time1 .or. item%doInterpolate .eqv. .false.) then
              var3d = var3d_prev
           else if (time == item%interp_time2) then
              var3d = var3d_next
           else
              do k=lbound(var3d,3),ubound(var3d,3)
                 do j=1,size(var3d,2)
                    do i=1,size(var3d,1)
                       if (var3d_next(i,j,k) /= MAPL_UNDEF .and. var3d_prev(i,j,k) /= MAPL_UNDEF) then
                           var3d(i,j,k) = var3d_prev(i,j,k) + alpha*(var3d_next(i,j,k)-var3d_prev(i,j,k))
                       else
                           var3d(i,j,k) = MAPL_UNDEF
                       end if
                    enddo
                 enddo
               enddo
           end if
           do k=lbound(var3d,3),ubound(var3d,3)
              do j=1,size(var3d,2)
                 do i=1,size(var3d,1)
                    if (var3d(i,j,k) /= MAPL_UNDEF) then
                       if (item%do_scale .and. (.not.item%do_offset)) var3d(i,j,k) = item%scale*var3d(i,j,k)
                       if ((.not.item%do_scale) .and. item%do_offset) var3d(i,j,k) = var3d(i,j,k)+item%offset
                       if (item%do_scale .and. item%do_offset) var3d(i,j,k) = item%offset + (item%scale * var3d(i,j,k))
                    else
                        var3d(i,j,k) = MAPL_UNDEF
                    end if
                 enddo
              enddo
           enddo
           
     endif

     RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_ExtDataInterpField

  ! routine to read two components of a vector on a Lat-Lon A grid
  ! and do proper vector transform to cube-sphere A grid
  ! as we will be making a lat-lon grid to call the HorzTransform
  ! we will need to pass the layout, follows what is done in the
  ! mkiau grid comp
  subroutine  MAPL_ExtDataReadVector(file,fname1,fname2,time,v1out,v2out,NX,NY,ignoreCase,rc)
     character(len=*),  intent(in   ) :: file
     character(len=*),  intent(in   ) :: fname1
     character(len=*),  intent(in   ) :: fname2
     type(ESMF_Time),   intent(inout) :: time
     type(ESMF_Field),  intent(inout) :: v1out
     type(ESMF_Field),  intent(inout) :: v2out
     integer,           intent(in   ) :: NX
     integer,           intent(in   ) :: NY
     logical,           intent(in   ) :: ignoreCase
     integer, optional, intent(out  ) :: rc

     integer                    :: status
     character(len=ESMF_MAXSTR) :: Iam

     integer :: fid
     integer :: im_world, jm_world, lmIn, nt, nvars, natts
     integer :: imIn, jmIn, imOut, jmOut, lmOut
     type(ESMF_Grid) :: gridIn, gridOut
     type(MAPL_HorzTransform) :: trans

     real, pointer :: v1out_3d(:,:,:) => null()
     real, pointer :: v2out_3d(:,:,:) => null()
     real, pointer :: v1out_2d(:,:) => null()
     real, pointer :: v2out_2d(:,:) => null()

     real, pointer :: v1in_3d(:,:,:) => null()
     real, pointer :: v2in_3d(:,:,:) => null()
     real, pointer :: v1in_2d(:,:) => null()
     real, pointer :: v2in_2d(:,:) => null()

     integer           :: gridStagger1,gridStagger2
     integer           :: gridRotation1,gridRotation2
     logical           :: doRotation

     integer           :: fieldRank 
     integer           :: DIMS(ESMF_MAXGRIDDIM)

     Iam = "MAPL_ExtDataReadVector"

     ! query the file to get the size of the lat-lon grid on the file
     call CFIO_Open(file,1,fid,status)
     VERIFY_(STATUS)
     call CFIO_DimInquire(fid,im_world,jm_world,lmIn,nt,nvars,natts,status)
     VERIFY_(STATUS)
     call CFIO_Close(fid,status)
     VERIFY_(STATUS)

     call ESMF_AttributeGet(v1out, NAME='STAGGERING', value=gridStagger1, __RC__)
     call ESMF_AttributeGet(v2out, NAME='STAGGERING', value=gridStagger2, __RC__)
     call ESMF_AttributeGet(v1out, NAME='ROTATION',value=gridRotation1,__RC__)
     call ESMF_AttributeGet(v2out, NAME='ROTATION',value=gridRotation2,__RC__)

     ASSERT_(gridStagger1 == gridStagger2)
     ASSERT_(gridRotation1 == gridRotation2)

     if (gridStagger1 == MAPL_AGrid) then
        if (gridRotation1 == MAPL_RotateLL) then
           doRotation = .false.
        else if (gridRotation1 == MAPL_RotateCube) then
           doRotation = .true.
        end if
     else if (gridStagger1 == MAPL_DGrid) then
        if (gridRotation1 /= MAPL_RotateCube) then
           ASSERT_(.false.)
        else
           doRotation = .false.
        end if 
     else if (gridStagger1 == MAPL_CGrid) then
        if (gridRotation1 /= MAPL_RotateCube) then
           ASSERT_(.false.)
        else
           doRotation = .false.
        end if
     end if
          

     gridIn = MAPL_LatLonGridCreate(name='gridll',       &
                                    NX = NX,             &
                                    NY = NY,             &
                                    IM_World = IM_World, &
                                    JM_World = JM_World, &
                                    LM_World = lmIn ,    &
                                    __RC__)

     call MAPL_GridGet(gridIn, localCellCountPerDim=dims,__RC__)
     imIn = dims(1)
     jmIn = dims(2)

     call ESMF_FieldGet(v1out,grid=gridOut,dimCount=fieldRank,__RC__)
     call MAPL_GridGet(gridOut, localCellCountPerDim=dims,__RC__)
     imOut = dims(1)
     jmOut = dims(2)


     call MAPL_HorzTransformCreate(trans,gridIn,gridOut,__RC__)

     if (fieldRank == 2) then

         call ESMF_FieldGet(v1out,localDe=0,farrayPtr=v1out_2d,__RC__)
         call ESMF_FieldGet(v2out,localDe=0,farrayPtr=v2out_2d,__RC__)
         
         allocate(v1in_3d(imIn,jmIn,1),__STAT__)
         allocate(v2in_3d(imIn,jmIn,1),__STAT__)
         allocate(v1in_2d(imIn,jmIn),__STAT__)
         allocate(v2in_2d(imIn,jmIn),__STAT__)
         allocate(v1out_3d(imOut,jmOut,1),__STAT__)
         allocate(v2out_3d(imOut,jmOut,1),__STAT__)
         

         call MAPL_CFIORead(fname1, file, time, gridIn, v1in_2d,ignoreCase=ignoreCase,__RC__)
         call MAPL_CFIORead(fname2, file, time, gridIn, v2in_2d,ignoreCase=ignoreCase,__RC__)
         
         v1in_3d(:,:,1) = v1in_2d
         v2in_3d(:,:,1) = v2in_2d
         v1out_3d(:,:,1) = v1out_2d
         v2out_3d(:,:,1) = v2out_2d
         
         call MAPL_HorzTransformRun (trans, v1in_3d, v2in_3d, v2out_3d, v2out_3d, rotate=doRotation, __RC__)

         if (gridStagger1 == MAPL_CGrid) then
             call A2D2C(v1out_3d,v2out_3d,1,getC=.true.)
         else if (gridStagger1 == MAPL_DGrid) then
             call A2D2C(v1out_3d,v2out_3d,1,getC=.false.)
         end if

         v1out_2d = v1out_3d(:,:,1)
         v2out_2d = v2out_3d(:,:,1)

         deallocate(v1in_3d,v2in_3d,v1in_2d,v2in_2d,v1out_3d,v2out_3d,__STAT__)

     else if (fieldRank == 3) then
        
         call ESMF_FieldGet(v1out,localDe=0,farrayPtr=v1out_3d,__RC__)
         call ESMF_FieldGet(v2out,localDe=0,farrayPtr=v2out_3d,__RC__)
         lmOut = size(v1out_3d,3)
         ASSERT_(lmOut == lmIn)
         
         allocate(v1in_3d(imIn,jmIn,lmIn),__STAT__)
         allocate(v2in_3d(imIn,jmIn,lmIn),__STAT__)

         call MAPL_CFIORead(fname1, file, time, gridIn, v1in_3d,ignoreCase=ignoreCase,__RC__)
         call MAPL_CFIORead(fname2, file, time, gridIn, v2in_3d,ignoreCase=ignoreCase,__RC__)
         
         call MAPL_HorzTransformRun (trans, v1in_3d, v2in_3d, v1out_3d, v2out_3d, rotate=doRotation, __RC__)

         if (gridStagger1 == MAPL_CGrid) then
            call A2D2C(v1out_3d,v2out_3d,lmOut,getC=.true.)
         else if (gridStagger1 == MAPL_DGrid) then
            call A2D2C(v1out_3d,v2out_3d,lmOut,getC=.false.)
         end if

         deallocate(v1in_3d,v2in_3d,__STAT__)

     end if

     RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_ExtDataReadVector

  subroutine MAPL_ExtDataEvaluateMask(state,exportName,exportExpr,rc)

    type(ESMF_STATE),           intent(inout) :: state
    character(len=*),           intent(in)    :: exportName
    character(len=*),           intent(in)    :: exportExpr
    integer, optional,          intent(out)   :: rc

    character(len=ESMF_MAXSTR) :: Iam = "EvaluateMask"
    integer :: status

    integer :: k,i
    character(len=ESMF_MAXSTR) :: maskString,maskname,vartomask,functionname,clatS,clatN
    character(len=ESMF_MAXSTR) :: strtmp
    integer, allocatable :: regionNumbers(:), flag(:)
    integer, allocatable :: mask(:,:)
    real, pointer        :: rmask(:,:)    => null()
    real, pointer        :: rvar2d(:,:)   => null()
    real, pointer        :: rvar3d(:,:,:) => null()
    real, pointer        :: var2d(:,:)    => null()
    real, pointer        :: var3d(:,:,:)  => null()
    real(REAL64), pointer :: lats(:,:)     => null()
    real(REAL64), pointer :: lons(:,:)     => null()
    real(REAL64)         :: limitS, limitN, limitE, limitW
    real(REAL64)         :: limitE1, limitW1
    real(REAL64)         :: limitE2, limitW2
    type(ESMF_Field)     :: field
    type(ESMF_Grid)      :: grid
    integer              :: rank,ib,ie,is,i1,nargs
    integer              :: counts(3)
    logical              :: isCube, twoBox
    real, allocatable    :: temp2d(:,:)
    character(len=ESMF_MAXSTR) :: args(5)

    call ESMF_StateGet(state,exportName,field,__RC__)
    call ESMF_FieldGet(field,rank=rank,grid=grid,__RC__)
    i1 = index(exportExpr,"(")
    ASSERT_(i1 > 0)
    functionname = adjustl(exportExpr(:i1-1))
    functionname = ESMF_UtilStringLowerCase(functionname,rc=status)

    if (trim(functionname) == "regionmask") then

       ! get mask string
       ib = index(exportExpr,";")
       ie = index(exportExpr,")")
       maskString = trim(exportExpr(ib+1:ie-1))
       ! get mask name
       ie = index(exportExpr,";")
       is = index(exportExpr,"(")
       ib = index(exportExpr,",")
       vartomask = trim(exportExpr(is+1:ib-1))
       maskname = trim(exportExpr(ib+1:ie-1))
       call MAPL_GetPointer(state,rmask,maskName,__RC__)
       if (rank == 2) then
          call MAPL_GetPointer(state,rvar2d,vartomask,__RC__)
          call MAPL_GetPointer(state,var2d,exportName,__RC__)
       else if (rank == 3) then
          call MAPL_GetPointer(state,rvar3d,vartomask,__RC__)
          call MAPL_GetPointer(state,var3d,exportName,__RC__)
       else
          ASSERT_(.false.)
       end if

       k=32
       allocate(regionNumbers(k), flag(k), stat=status)
       VERIFY_(STATUS)
       regionNumbers = 0
       call MAPL_ExtDataExtractIntegers(maskString,k,regionNumbers,rc=status)
       VERIFY_(STATUS)
       flag(:) = 1
       WHERE(regionNumbers(:) == 0) flag(:) = 0
       k = SUM(flag)
       deallocate(flag,stat=status)
       VERIFY_(STATUS)

   !   Set local mask to 1 where gridMask matches each integer (within precision!) 
   !   ---------------------------------------------------------------------------
       allocate(mask(size(rmask,1),size(rmask,2)),stat=status)
       VERIFY_(STATUS)
       mask = 0
       DO i=1,k
        WHERE(regionNumbers(i)-0.01 <= rmask .AND. &
              rmask <= regionNumbers(i)+0.01) mask = 1
       END DO

       if (rank == 2) then
          var2d = rvar2d
          where(mask == 0) var2d = 0.0
       else if (rank == 3) then
          var3d = rvar3d
          do i=1,size(var3d,3)
             where(mask == 0) var3d(:,:,i) = 0.0
          enddo
       end if
       deallocate( mask)

    elseif(trim(functionname) == "zonemask") then

       ib = index(exportExpr,"(")
       ie = index(exportExpr,",")
       vartomask = trim(exportExpr(ib+1:ie-1))
       ib = index(exportExpr,",")
       is = index(exportExpr,",",back=.true.)
       ie = index(exportExpr,")")
       clatS = trim(exportExpr(ib+1:is-1))
       clatN = trim(exportExpr(is+1:ie-1))
       READ(clatS,*,IOSTAT=status) limitS
       VERIFY_(status)
       READ(clatN,*,IOSTAT=status) limitN
       VERIFY_(status)

       call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
            staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=lats, rc=status)
       VERIFY_(status)
       limitN=limitN*MAPL_PI_R8/180.0d0
       limitS=limitS*MAPL_PI_R8/180.0d0

       if (rank == 2) then
          call MAPL_GetPointer(state,rvar2d,vartomask,__RC__)
          call MAPL_GetPointer(state,var2d,exportName,__RC__)
       else if (rank == 3) then
          call MAPL_GetPointer(state,rvar3d,vartomask,__RC__)
          call MAPL_GetPointer(state,var3d,exportName,__RC__)
       else
          ASSERT_(.false.)
       end if

       if (rank == 2) then
          var2d = 0.0
          where(limitS <= lats .and. lats <=limitN) var2d = rvar2d
       else if (rank == 3) then
          var3d = 0.0
          do i=1,size(var3d,3)
             where(limitS <= lats .and. lats <=limitN) var3d(:,:,i) = rvar3d(:,:,i)
          enddo
       end if

    elseif(trim(functionname) == "boxmask") then

       is=index(exportExpr,'(')
       ie=index(exportExpr,')')
       strtmp = exportExpr(is+1:ie-1)
       do nargs=1,5
          is = index(strtmp,',')
          if (is >0) then
             args(nargs) = strtmp(:is-1)
          else
             args(nargs) = strtmp
          end if
          strtmp = strtmp(is+1:)
       end do

       varToMask=args(1) 

       READ(args(2),*,IOSTAT=status) limitS
       VERIFY_(status)
       READ(args(3),*,IOSTAT=status) limitN
       VERIFY_(status)
       READ(args(4),*,IOSTAT=status) limitW
       VERIFY_(status)
       READ(args(5),*,IOSTAT=status) limitE
       VERIFY_(status)
       ASSERT_(limitE > limitW)
       ASSERT_(limitE /= limitW)
       ASSERT_(limitN /= limitS)
       ASSERT_((limitE-limitW)<=360.0d0)

       call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
            staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=lons, rc=status)
       VERIFY_(status)
       call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
            staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=lats, rc=status)
       VERIFY_(status)
        
       ! do some tests if cube goes from 0 to 360, lat-lon -180 to 180
       call MAPL_GridGet(grid, globalCellCountPerDim=COUNTS,rc=status)
       VERIFY_(STATUS)
       if (counts(2)==6*counts(1)) then
          isCube=.true.
       else
          isCube=.false.
       end if

       twoBox = .false.
       if (isCube) then

          if (limitW < 0.0d0 .and. limitE >=0.0d0) then
             ! need two boxes
             twoBox=.true.
             limitW1=0.0d0
             limitE1=limitE
             limitW2=limitW+360.0d0
             limitE2=360.0d0
 
          else if (limitW <0.0d0 .and. limitE <0.0d0) then
             ! just shift
             limitW1=limitW+360.d0
             limitE1=limitE+360.d0

          else
             ! normal case
             limitW1=limitW
             limitE1=limitE
          end if

       else

          if (limitW  <= 180.0d0 .and. limitE > 180.0d0) then
             ! need two boxes
             twoBox=.true.
             limitW1=limitW
             limitE1=180.0d0
             limitW2=-180.d0
             limitE2=limitE-360.0d0
          else if (limitW > 180.0d0 .and. limitE > 180.0d0) then
             ! just shift
             limitW1=limitW-360.d0
             limitE1=limitE-360.d0
          else
             ! normal case
             limitW1=limitW
             limitE1=limitE
          end if

       end if

       limitE1=limitE1*MAPL_PI_R8/180.0d0
       limitW1=limitW1*MAPL_PI_R8/180.0d0
       limitE2=limitE2*MAPL_PI_R8/180.0d0
       limitW2=limitW2*MAPL_PI_R8/180.0d0

       limitN=limitN*MAPL_PI_R8/180.0d0
       limitS=limitS*MAPL_PI_R8/180.0d0

       if (rank == 2) then
          call MAPL_GetPointer(state,rvar2d,vartomask,__RC__)
          call MAPL_GetPointer(state,var2d,exportName,__RC__)
       else if (rank == 3) then
          call MAPL_GetPointer(state,rvar3d,vartomask,__RC__)
          call MAPL_GetPointer(state,var3d,exportName,__RC__)
       else
          ASSERT_(.false.)
       end if
      
       if (rank == 2) then
          var2d = 0.0
          where(limitS <= lats .and. lats <=limitN .and. limitW1 <= lons .and. lons <= limitE1 ) var2d = rvar2d
       else if (rank == 3) then
          var3d = 0.0
          do i=1,size(var3d,3)
             where(limitS <= lats .and. lats <=limitN .and. limitW1 <= lons .and. lons <= limitE1 ) var3d(:,:,i) = rvar3d(:,:,i)
          enddo
       end if

       if (twoBox) then
          allocate(temp2d(size(var2d,1),size(var2d,2)),stat=status)
          VERIFY_(STATUS)
          if (rank == 2) then
             temp2d = 0.0
             where(limitS <= lats .and. lats <=limitN .and. limitW2 <= lons .and. lons <= limitE2 ) temp2d = rvar2d
             var2d=var2d+temp2d
          else if (rank == 3) then
             do i=1,size(var3d,3)
                temp2d = 0.0
                where(limitS <= lats .and. lats <=limitN .and. limitW2 <= lons .and. lons <= limitE2 ) temp2d = rvar3d(:,:,i)
                var3d(:,:,i)=var3d(:,:,i)+temp2d
             enddo
          end if
          deallocate(temp2d)
       end if

    end if

    RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_ExtDataEvaluateMask

  SUBROUTINE MAPL_ExtDataExtractIntegers(string,iSize,iValues,delimiter,verbose,rc)

! !USES:

  IMPLICIT NONE

! !INPUT/OUTPUT PARAMETERS:

  CHARACTER(LEN=*), INTENT(IN)   :: string     ! Character-delimited string of integers
  INTEGER, INTENT(IN)            :: iSize
  INTEGER, INTENT(INOUT)         :: iValues(iSize)! Space allocated for extracted integers
  CHARACTER(LEN=*), OPTIONAL     :: delimiter     ! 1-character delimiter
  LOGICAL, OPTIONAL, INTENT(IN)  :: verbose    ! Let me know iValues as they are found. 
                                      ! DEBUG directive turns on the message even 
                                      ! if verbose is not present or if 
                                      ! verbose = .FALSE.
  INTEGER, OPTIONAL, INTENT(OUT) :: rc            ! Return code
! !DESCRIPTION: 
!
!  Extract integers from a character-delimited string, for example, "-1,45,256,7,10".  In the context
!  of Chem_Util, this is provided for determining the numerically indexed regions over which an 
!  emission might be applied.
!
!  In multiple passes, the string is parsed for the delimiter, and the characters up to, but not
!  including the delimiter are taken as consecutive digits of an integer.  A negative sign ("-") is
!  allowed.  After the first pass, each integer and its trailing delimiter are lopped of the head of
!  the (local copy of the) string, and the process is started over.
!
!  The default delimiter is a comma (",").
!
!  "Unfilled" iValues are zero.
!  
!  Return codes:
!  1 Zero-length string.
!  2 iSize needs to be increased.
!
!  Assumptions/bugs:
!
!  A non-zero return code does not stop execution.
!  Allowed numerals are: 0,1,2,3,4,5,6,7,8,9.
!  A delimiter must be separated from another delimiter by at least one numeral.
!  The delimiter cannot be a numeral or a negative sign.
!  The character following a negative sign must be an allowed numeral.
!  The first character must be an allowed numeral or a negative sign.
!  The last character must be an allowed numeral.
!  The blank character (" ") cannot serve as a delimiter.
!
!  Examples of strings that will work:
!  "1"
!  "-1"
!  "-1,2004,-3"
!  "1+-2+3"
!  "-1A100A5"
!  Examples of strings that will not work:
!  "1,--2,3"
!  "1,,2,3"
!  "1,A,3"
!  "1,-,2"
!  "1,2,3,4,"
!  "+1"
!  "1 3 6"
!
! !REVISION HISTORY: 
!
!  Taken from chem utilities.
!
!EOP
 CHARACTER(LEN=*), PARAMETER :: Iam = 'Chem_UtilExtractIntegers'

 INTEGER :: base,count,i,iDash,last,lenStr,status
 INTEGER :: multiplier,pos,posDelim,sign
 CHARACTER(LEN=255) :: str
 CHARACTER(LEN=1) :: char,delimChar
 LOGICAL :: Done
 LOGICAL :: tellMe

! Initializations
! ---------------
 rc = 0
 count = 1
 Done = .FALSE.
 iValues(:) = 0
 base = ICHAR("0")
 iDash = ICHAR("-")

! Determine verbosity, letting the DEBUG 
! directive override local specification
! --------------------------------------
  tellMe = .FALSE.
  IF(PRESENT(verbose)) THEN
   IF(verbose) tellMe = .TRUE.
 END IF
#ifdef DEBUG
  tellMe = .TRUE.
#endif
! Check for zero-length string
! ----------------------------
 lenStr = LEN_TRIM(string)
 IF(lenStr == 0) THEN
  rc = 1
  PRINT *,trim(IAm),": ERROR - Found zero-length string."
  RETURN
 END IF

! Default delimiter is a comma
! ----------------------------
 delimChar = ","
 IF(PRESENT(delimiter)) delimChar(1:1) = delimiter(1:1)

! Work on a local copy
! --------------------
 str = TRIM(string)

! One pass for each delimited integer
! -----------------------------------
 Parse: DO

  lenStr = LEN_TRIM(str)

! Parse the string for the delimiter
! ----------------------------------
  posDelim = INDEX(TRIM(str),TRIM(delimChar))
  IF(tellMe) PRINT *,trim(Iam),": Input string is >",TRIM(string),"<"

! If the delimiter does not exist,
! one integer remains to be extracted.
! ------------------------------------
  IF(posDelim == 0) THEN
   Done = .TRUE.
   last = lenStr
  ELSE
   last = posDelim-1
  END IF
  multiplier = 10**last

! Examine the characters of this integer
! --------------------------------------
  Extract: DO pos=1,last

   char = str(pos:pos)
   i = ICHAR(char)

! Account for a leading "-"
! -------------------------
   IF(pos == 1) THEN
    IF(i == iDash) THEN
     sign = -1
    ELSE
     sign = 1
    END IF
   END IF

! "Power" of 10 for this character
! --------------------------------
   multiplier = multiplier/10

   IF(pos == 1 .AND. sign == -1) CYCLE Extract

! Integer comes from remaining characters
! ---------------------------------------
   i = (i-base)*multiplier
   iValues(count) = iValues(count)+i
   IF(pos == last) THEN
    iValues(count) = iValues(count)*sign
    IF(tellMe) PRINT *,trim(Iam),":Integer number ",count," is ",iValues(count)
   END IF

  END DO Extract

  IF(Done) EXIT

! Lop off the leading integer and try again
! -----------------------------------------
  str(1:lenStr-posDelim) = str(posDelim+1:lenStr)
  str(lenStr-posDelim+1:255) = " "
  count = count+1

! Check size
! ----------
  IF(count > iSize) THEN
   rc = 2
   PRINT *,trim(Iam),": ERROR - iValues does not have enough elements."
  END IF

 END DO Parse

 RETURN_(ESMF_SUCCESS)

 END SUBROUTINE MAPL_ExtDataExtractIntegers

  function MAPL_ExtDataGetFStartTime(fname, rc) result(stime)

     character(len=*), intent(in   ) :: fname
     integer, optional, intent(out  ) :: rc

     type(ESMF_Time) :: stime

     character(len=ESMF_MAXSTR), parameter :: IAM="MAPL_ExtDataGetFStartTime"
     integer                               :: status

     integer :: iyr,imm,idd,ihr,imn,isc,begDate,begTime
     type(ESMF_CFIO) :: cfio

     cfio =  ESMF_CFIOCreate (cfioObjName='cfio_obj',__RC__)
     call ESMF_CFIOSet(CFIO, fName=fname,__RC__)
     call ESMF_CFIOFileOpen  (CFIO, FMODE=1, cyclic=.false.,__RC__)
     begDate = cfio%date
     begTime = cfio%begTime
     call MAPL_UnpackTime(begDate,iyr,imm,idd)
     call MAPL_UnpackTime(begTime,ihr,imn,isc)
     call ESMF_TimeSet(sTime, yy=iyr, mm=imm, dd=idd,  h=ihr,  m=imn, s=isc, __RC__)
     call ESMF_CFIODestroy(CFIO,__RC__)

     RETURN_(ESMF_SUCCESS)

  end function MAPL_ExtDataGetFStartTime

  subroutine CheckUpdate(doUpdate,updateTime,currTime,hasRun,primaryItem,derivedItem,rc) 
     logical,                       intent(out  ) :: doUpdate
     type(ESMF_Time),               intent(inout) :: updateTime
     type(ESMF_Time),               intent(inout) :: currTime
     logical        ,               intent(in   ) :: hasRun
     type(PrimaryExport), optional, intent(inout) :: primaryItem
     type(DerivedExport), optional, intent(inout) :: derivedItem
     integer,             optional, intent(out  ) :: rc

     logical                    :: isEnabled
     character(len=ESMF_MAXSTR) :: Iam
     integer                    :: status
     type(ESMF_Time)            :: time,time0,refresh_time
     Iam = "CheckUpdate"

     time0 = currTime
     time  = currTime
     if (present(primaryItem)) then
       
        if (primaryItem%AlarmIsEnabled) then
           doUpdate = ESMF_AlarmIsRinging(primaryItem%update_alarm,__RC__)
           if (hasRun .eqv. .false.) doUpdate = .true.
           updateTime = currTime
        else if (trim(primaryItem%cyclic) == 'single') then
           doUpdate = .true.
        else
           if (primaryItem%refresh_template == "0") then
              doUpdate = .true.
              updateTime = time0 + PrimaryItem%tshift
           else
              updateTime = time0
              if (.not. associated(PrimaryItem%refresh_time)) then
                doUpdate = .false.
              else
                 refresh_time = timestamp_(time, PrimaryItem%refresh_template, __RC__)
                 if (refresh_time /= primaryItem%refresh_time) then
                    doUpdate = .true.
                    primaryItem%refresh_time = refresh_time
                    updateTime = refresh_time
                 else
                    doUpdate = .false.
                 end if
              end if
           end if
        end if
     else if (present(derivedItem)) then
        if (DerivedItem%AlarmIsEnabled) then
           doUpdate = ESMF_AlarmIsRinging(derivedItem%update_alarm,__RC__)
           updateTime = currTime
        else
           if (derivedItem%refresh_template == "0") then
              doUpdate = .true.
              updateTime = time0 + derivedItem%tshift
           else
              updateTime = time0
              if (.not. associated(derivedItem%refresh_time)) then
                doUpdate = .false.
              end if
              refresh_time = timestamp_(time, derivedItem%refresh_template, __RC__)
              if (refresh_time /= derivedItem%refresh_time) then
                 doUpdate = .true.
                 derivedItem%refresh_time = refresh_time
                 time = refresh_time
              else
                 doUpdate = .false.
              end if
           end if
        end if
     end if
     
     RETURN_(ESMF_SUCCESS)
  end subroutine CheckUpdate

  ! This is a routine to make the appropriate call to MAPL_CFIO so we do not need to
  ! duplicate the calls with lots of if/else for voting, fractional etc ...
  subroutine MAPL_ExtDataGridCompRead(file,time,trans,val,ignorecase,field,bundle,name,rc)
     character(len=*),                 intent(IN   ) :: file
     type(ESMF_Time),                  intent(INOUT) :: time
     integer,                          intent(IN   ) :: trans
     integer,                          intent(IN   ) :: val
     logical,                          intent(IN   ) :: ignorecase
     type(ESMF_Field),       optional, intent(INOUT) :: field
     type(ESMF_FieldBundle), optional, intent(INOUT) :: bundle
     character(len=*),       optional, intent(IN   ) :: name
     integer,                optional, intent(  OUT) :: rc

     character(len = ESMF_MAXSTR) :: Iam
     integer                      :: Status

     Iam = "MAPL_ExtDataGridCompRead"

     if (present(field)) then
        ASSERT_(present(name))
     end if

     if (present(field)) then
        if (trans == MAPL_HorzTransOrderBilinear) then
           call MAPL_CFIORead(name, file, time, field, &
                time_is_cyclic=.false., time_interp=.false., ignoreCase = ignoreCase, &
                __RC__) 
        else if (trans == MAPL_HorzTransOrderBinning) then
           call MAPL_CFIORead(name, file, time, field, &
                time_is_cyclic=.false., time_interp=.false., ignoreCase = ignoreCase, &
                Conservative = .true., __RC__) 
        else if (trans == MAPL_HorzTransOrderSample) then
           call MAPL_CFIORead(name, file, time, field, &
                time_is_cyclic=.false., time_interp=.false., ignoreCase = ignoreCase, &
                Conservative = .true., Voting = .true., __RC__) 
        else if (trans == MAPL_HorzTransOrderFraction) then
           call MAPL_CFIORead(name, file, time, field, &
                time_is_cyclic=.false., time_interp=.false., ignoreCase = ignoreCase, &
                Conservative = .true., getFrac = val, __RC__) 
        end if

     else if (present(bundle)) then

        if (trans == MAPL_HorzTransOrderBilinear) then
           call MAPL_CFIORead(file, time, bundle, &
                time_is_cyclic=.false., time_interp=.false., ignoreCase = ignoreCase, &
                __RC__)
        else if (trans == MAPL_HorzTransOrderBinning) then
           call MAPL_CFIORead(file, time, bundle, &
                time_is_cyclic=.false., time_interp=.false., ignoreCase = ignoreCase, &
                Conservative = .true., __RC__)
        else if (trans == MAPL_HorzTransOrderSample) then
           call MAPL_CFIORead(file, time, bundle, &
                time_is_cyclic=.false., time_interp=.false., ignoreCase = ignoreCase, &
                Conservative = .true., Voting = .true., __RC__)
        else if (trans == MAPL_HorzTransOrderFraction) then
           call MAPL_CFIORead(file, time, bundle, &
                time_is_cyclic=.false., time_interp=.false., ignoreCase = ignoreCase, &
                Conservative = .true., getFrac = val, __RC__)
        end if

     end if

     RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_ExtDataGridCompRead

! ............................................................................

  type (ESMF_Time) function timestamp_(time, template, rc)
     type(ESMF_Time), intent(inout)         :: time
     character(len=ESMF_MAXSTR), intent(in) :: template
     integer, optional, intent(inout)       :: rc 

      __Iam__('timestamp_')

     ! locals
     integer, parameter :: DATETIME_MAXSTR_ = 32
     integer :: yy, mm, dd, hs, ms, ss
     character(len=DATETIME_MAXSTR_) :: buff, buff_date, buff_time
     character(len=DATETIME_MAXSTR_) :: str_yy, str_mm, str_dd
     character(len=DATETIME_MAXSTR_) :: str_hs, str_ms, str_ss

     integer :: i, il, ir
     integer :: stat
    
     ! test the length of the timestamp template
     ASSERT_(len_trim(template) < DATETIME_MAXSTR_)

     buff = trim(template)
     buff = ESMF_UtilStringLowerCase(buff, rc=stat)
      
     ! test if the template is empty and return the current time as result
     if (buff == '-'  .or. buff == '--'   .or. buff == '---' .or. &
         buff == 'na' .or. buff == 'none' .or. buff == 'n/a') then

        timestamp_ = time
     else   
        ! split the time stamp template into a date and time strings
        i = scan(buff, 't')
        ASSERT_(i > 3)

        buff_date = buff(1:i-1)
        buff_time = buff(i+1:)

        ! parse the date string
        il = scan(buff_date, '-', back=.false.)
        ir = scan(buff_date, '-', back=.true. )
        str_yy = trim(buff_date(1:il-1))
        str_mm = trim(buff_date(il+1:ir-1))
        str_dd = trim(buff_date(ir+1:))

        ! parse the time string
        il = scan(buff_time, ':', back=.false.)
        ir = scan(buff_time, ':', back=.true. )
        str_hs = trim(buff_time(1:il-1))
        str_ms = trim(buff_time(il+1:ir-1))
        str_ss = trim(buff_time(ir+1:))
     
        ! remove the trailing 'Z' from the seconds string
        i = scan(str_ss, 'z')
        if (i > 0) then
           str_ss = trim(str_ss(1:i-1))
        end if

        ! apply the timestamp template
        call ESMF_TimeGet(time, yy=yy, mm=mm, dd=dd, h=hs, m=ms, s=ss, __RC__)

        i = scan(str_yy, '%'); if (i == 0) read (str_yy, '(I4)'), yy
        i = scan(str_mm, '%'); if (i == 0) read (str_mm, '(I2)'), mm
        i = scan(str_dd, '%'); if (i == 0) read (str_dd, '(I2)'), dd
        i = scan(str_hs, '%'); if (i == 0) read (str_hs, '(I2)'), hs
        i = scan(str_ms, '%'); if (i == 0) read (str_ms, '(I2)'), ms
        i = scan(str_ss, '%'); if (i == 0) read (str_ss, '(I2)'), ss

        call ESMF_TimeSet(timestamp_, yy=yy, mm=mm, dd=dd, h=hs, m=ms, s=ss, __RC__)
     end if

     RETURN_(ESMF_SUCCESS)

  end function timestamp_

#endif
    
  end module EXTDATA
