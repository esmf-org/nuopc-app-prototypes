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
   USE ESMF
   use MAPL_BaseMod
   use MAPL_CommsMod
   use ESMFL_Mod
   use MAPL_GenericMod
   use MAPL_VarSpecMod
   use ESMF_CFIOFileMod
   use ESMF_CFIOMod
   use ESMF_CFIOUtilMod
   use MAPL_CFIOMod
!   use MAPL_HorzTransformMod
   use MAPL_NewArthParserMod
   use MAPL_ConstantsMod, only: MAPL_PI,MAPL_PI_R8
   use MAPL_IOMod, only: MAPL_NCIOParseTimeUnits
   use MAPL_regridderSpecMod
   use, intrinsic :: iso_fortran_env, only: REAL64
   use linearVerticalInterpolation_mod
#endif
   use NUOPC
   use NUOPC_Mediator, &
           model_routine_SS      => SetServices
!           model_label_Advance   => label_Advance, &
!           model_label_DataInitialize   => label_DataInitialize
   use NUOPC_Generic
      
   implicit none
!
   private
!
! !PUBLIC MEMBER FUNCTIONS:

   public SetServices
!   public T_EXTDATA_STATE
!   public EXTDATA_WRAP
!
! !DESCRIPTION: 
!
!EOP
!=============================================================================

  integer                    :: Ext_Debug
  character(len=ESMF_MAXSTR) :: Ext_TilePath
  integer, parameter         :: MAPL_ExtDataVectorItem    = 32
  integer, parameter         :: MAPL_ExtDataNullFrac      = -9999
  integer, parameter         :: MAPL_ExtDataLeft          = 1
  integer, parameter         :: MAPL_ExtDataRight         = 2
  logical                    :: hasRun

! ---------------
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

     ! Experimental: store CFIO and time vectors for each file
     type(ESMF_CFIO)              :: cfio1, cfio2
     type(ESMF_Time), Allocatable :: tSeries1(:), tSeries2(:)
     logical                      :: isAssigned1 = .False.
     logical                      :: isAssigned2 = .False.
     
     logical                      :: ExtDataAlloc
     ! time shifting during continuous update
     type(ESMF_TimeInterval)      :: tshift
     type(ESMF_Alarm)             :: update_alarm
     logical                      :: alarmIsEnabled = .false.
     integer                      :: FracVal = MAPL_ExtDataNullFrac
     ! do we have to do vertical interpolation
     logical                      :: do_VertInterp = .false.
     logical                      :: do_Fill = .false.
     integer                      :: LM
     real, allocatable            :: levs(:)
     character(len=ESMF_MAXSTR)   :: levUnit
     logical                      :: havePressure = .false.
  end type PrimaryExport

  type PrimaryExports
     PRIVATE
     integer :: nItems = 0
     logical :: have_phis
     type(PrimaryExport), pointer :: item(:) => null() 
  end type PrimaryExports

  type DerivedExport
     PRIVATE
     character(len=ESMF_MAXSTR)     :: name
     character(len=ESMF_MAXPATHLEN) :: expression
     character(len=ESMF_MAXSTR)     :: refresh_template
     logical                        :: ExtDataAlloc
     logical                        :: masking
     type(ESMF_Time), pointer       :: refresh_time => null()
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
     type(ESMF_Config)    :: CF
     logical              :: active
     logical              :: ignoreCase
     logical              :: AllowExtrap
     integer, allocatable :: PrimaryOrder(:)
  end type MAPL_ExtData_State

! Hook for the ESMF
! -----------------
  type MAPL_ExtData_Wrap
     type (MAPL_ExtData_State), pointer :: PTR => null()
  end type MAPL_ExtData_WRAP

#if 0
  type T_EXTDATA_STATE
     type(ESMF_State)    :: expState
     type(ESMF_GridComp) :: gc
  end type T_EXTDATA_STATE

  ! Wrapper for extracting internal state
  ! -------------------------------------
  type EXTDATA_WRAP
     type (T_EXTDATA_STATE), pointer :: PTR
  end type EXTDATA_WRAP
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
      type (MAPL_ExtData_State), pointer  :: self   ! internal, that is
      type (MAPL_ExtData_wrap)            :: wrap
      
      ! Get my name and set-up traceback handle
      !----------------------------------------

      call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=STATUS )
      VERIFY_(STATUS)

      Iam = trim(COMP_NAME) //"::"// trim(Iam)
      
      call ESMF_LogWrite(Iam, ESMF_LOGMSG_INFO, rc=rc)      

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

      call NUOPC_CompSpecialize(GC, specLabel=label_DataInitialize, &
        specRoutine=dataInitialize, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
      
      ! attach specializing method(s)                                                                             
      ! -> NUOPC specializes by default --->>> first need to remove the default                                   
      call ESMF_MethodRemove(GC, label_SetRunClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out                                                                                        
      call NUOPC_CompSpecialize(GC, specLabel=label_SetRunClock, &
        specRoutine=SetRunClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out                                                                                        

     call ESMF_MethodRemove(GC, label=label_CheckImport,rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out
     call NUOPC_CompSpecialize(GC, specLabel=label_CheckImport, &
       specRoutine=NUOPC_NoOp, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out


      call NUOPC_CompSpecialize(GC, specLabel=label_Advance, &
           specRoutine=ModelAdvance, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out

  !   Wrap internal state for storing in GC; rename legacyState
  !   -------------------------------------
      allocate ( self, stat=STATUS )
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
      wrap%ptr => self

  !   Store internal state in GC
  !   --------------------------
      call ESMF_UserCompSetInternalState ( GC, 'MAPL_ExtData_state', wrap, STATUS )
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

  subroutine SetRunClock(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc

    ! local variables                                                                                           
    type(ESMF_Clock)              :: mediatorClock, driverClock
    type(ESMF_Time)               :: currTime

    rc = ESMF_SUCCESS

    call ESMF_LogWrite("ExtData: SetRunClock", ESMF_LOGMSG_INFO, rc=rc)

    ! query the Mediator for clocks                                                                             
    call NUOPC_MediatorGet(mediator, mediatorClock=mediatorClock, &
      driverClock=driverClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out                                                                                        

    ! set the mediatorClock to have the current start time as the driverClock                                   
    call ESMF_ClockGet(driverClock, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out                                                                                        
    call ESMF_ClockSet(mediatorClock, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out                                                                                        

    ! check and set the component clock against the driver clock                                                
    call NUOPC_CompCheckSetClock(mediator, driverClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
      msg="NUOPC INCOMPATIBILITY DETECTED: between model and driver clocks", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out                                                                                        

  end subroutine


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
        integer                :: knd
        type(ESMF_TypeKind_Flag) :: typekind

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

                        call ESMF_AttributeGet(field, &
                          name="PRECISION", value=knd, rc=rc)
                        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                          line=__LINE__, &
                          file=__FILE__)) &
                          return  ! bail out
			if (knd == ESMF_KIND_R4) then
			   typekind = ESMF_TYPEKIND_R4
			else
			   typekind = ESMF_TYPEKIND_R8
			endif   
                    endif

                    if (associated(ungriddedLBound) .and. &
                        associated(ungriddedUBound)) then
                      call ESMF_FieldEmptyComplete(field, typekind=typekind, &
                        ungriddedLBound=ungriddedLBound, &
                        ungriddedUBound=ungriddedUBound, &
                        rc=rc)
                      deallocate(ungriddedLBound)
                      deallocate(ungriddedUBound)
                    else
                      call ESMF_FieldEmptyComplete(field, typekind=typekind, rc=rc)
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
                    !print *, 'grid GRID_LM:', UNGRID

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

     ! call NUOPC_ModelGet(GC, modelClock=clock, importState=importState, &
      call ESMF_GridCompGet(GC, clock=clock, importState=importState, &
        exportState=exportState, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! Initialize the data by reading the EXTDATA config file
      call initialize_(GC, importState, exportState, clock, RC)
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

   SUBROUTINE Initialize_ ( GC, IMPORT, EXPORT, CLOCK, rc )

! !USES:

   implicit NONE

! !INPUT PARAMETERS:

   type(ESMF_Clock),  intent(inout)   :: CLOCK   ! The clock

! !OUTPUT PARAMETERS:

   type(ESMF_GridComp), intent(inout) :: GC      ! Grid Component
   type(ESMF_State), intent(inout)    :: IMPORT  ! Import State
   type(ESMF_State), intent(inout)    :: EXPORT  ! Export State
   integer, intent(out)               :: rc      ! Error return code:
                                                 !  0 - all is well
                                                 !  1 - 

! !DESCRIPTION: This is a simple ESMF wrapper.
!
! !REVISION HISTORY:
!
!  12Dec2009  da Silva  Design and first implementation.
!
!EOP
!-------------------------------------------------------------------------

   type(MAPL_ExtData_state), pointer :: self        ! Legacy state
   type(ESMF_Grid)                   :: GRID        ! Grid
   type(ESMF_Config)                 :: CF_master          ! Universal Config 

   character(len=ESMF_MAXSTR)        :: comp_name
   character(len=ESMF_MAXSTR)        :: Iam
   integer                           :: Status
   character(len=ESMF_MAXSTR)        :: buffer

   type(PrimaryExports)              :: Primary
   type(PrimaryExport), pointer      :: item
   type(DerivedExports)              :: Derived
   type(DerivedExport), pointer      :: derivedItem 
   integer                           :: nLines
   integer                           :: i
   integer                           :: ItemCount, itemCounter, j
   integer                           :: PrimaryItemCount, DerivedItemCount
   logical                           :: found

   type(ESMF_Time)                   :: time
   character(len=ESMF_MAXSTR)        :: VarName

   type (ESMF_Field)                 :: field,fieldnew
   integer                           :: fieldRank, lm
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
   integer                           :: k, ios
   character(len=ESMF_MAXSTR)        :: c_offset, c_scale
   character(len=ESMF_MAXSTR)        :: EXTDATA_CF, ROOT_CF
   type(ESMF_Config) :: CFtemp, CFtemp1
   integer           :: totalPrimaryEntries
   integer           :: totalDerivedEntries
   logical           :: caseSensitiveVarNames
   character(len=ESMF_MAXSTR) :: component1,component2
   character(len=ESMF_MAXPATHLEN) :: expression
   integer           :: idx,nhms,ihr,imn,isc
   logical           :: isNegative
   character(len=ESMF_MAXSTR) :: thisLine
   logical           :: inBlock
   type(ESMF_VM) :: vm
   type(MAPL_MetaComp),pointer :: MAPLSTATE

!  Get my name and set-up traceback handle
!  ---------------------------------------
   Iam = 'Initialize_'
   call ESMF_GridCompGet( GC, name=comp_name, config=CF_master, __RC__ )
   Iam = trim(comp_name) // '::' // trim(Iam)

!  Extract relevant runtime information
!  ------------------------------------
   call extract_ ( GC, self, CF_master, __RC__)
   call ESMF_ConfigGetAttribute(CF_master,ROOT_CF, Label='ROOT_CF:',default='GEOSCTM.rc',rc=status)
   if (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out   
   CFtemp1 = ESMF_ConfigCreate (rc=STATUS )
   VERIFY_(STATUS)
   call ESMF_ConfigLoadFile(CFtemp1,ROOT_CF,rc=status)
   VERIFY_(STATUS)
   self%CF = CFtemp1

!  Start Some Timers
!  -----------------
!   call MAPL_GetObjectFromGC ( gc, MAPLSTATE, RC=STATUS)
!   VERIFY_(STATUS) 
!   call MAPL_TimerOn(MAPLSTATE,"TOTAL")
!   call MAPL_TimerOn(MAPLSTATE,"Initialize")

! Get information from export state
!----------------------------------
    call ESMF_StateGet(EXPORT, ITEMCOUNT=ItemCount, RC=STATUS)
    VERIFY_(STATUS)

    ! set ExtData on by default, let user turn it off if they want
    call ESMF_ConfigGetAttribute(CF_master,self%active, Label='USE_EXTDATA:',default=.true.,rc=status)
    if (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    ! set extdata to ignore case on variable names in files
    call ESMF_ConfigGetAttribute(CF_master,caseSensitiveVarNames, Label='CASE_SENSITIVE_VARIABLE_NAMES:',default=.false.,rc=status)
    if (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    self%ignoreCase = .not. caseSensitiveVarNames

    ! no need to run ExtData if there are no imports to fill
    if (ItemCount == 0) then
       self%active = .false.
    end if

    if (.not.self%active) then
       RETURN_(ESMF_SUCCESS)
    end if

!   Greetings
!   ---------
    if (MAPL_am_I_root()) then
         print *, TRIM(Iam)//': ACTIVE'
         print *
    end if

    allocate(ITEMNAMES(ITEMCOUNT), STAT=STATUS)
    VERIFY_(STATUS)
    allocate(ITEMTYPES(ITEMCOUNT), STAT=STATUS)
    VERIFY_(STATUS)

    call ESMF_StateGet(EXPORT, ITEMNAMELIST=ITEMNAMES, &
                       ITEMTYPELIST=ITEMTYPES, RC=STATUS)
    VERIFY_(STATUS)

!                               --------
!  Initialize MAPL Generic
!  -----------------------
!   call MAPL_GenericInitialize ( GC, IMPORT, EXPORT, clock,  __RC__ )


!                         ---------------------------
!                         Parse ExtData Resource File
!                         ---------------------------

!   call ESMF_ConfigGetAttribute(CF_Master,value=EXTDATA_CF,Label="CF_EXTDATA:",rc=status)
   call ESMF_ConfigGetAttribute(CF_Master,value=EXTDATA_CF,Label="EXTDATA_CF:",rc=status)
   if (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
   VERIFY_(STATUS)
  call ESMF_ConfigGetAttribute(CF_Master,value=self%allowExtrap,Label="Ext_AllowExtrap:", default=.true., rc=status)
   if (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
   VERIFY_(STATUS)

   call ESMF_ConfigGetAttribute(CF_Master,value=Ext_Debug,Label="DEBUG_LEVEL:",default=0, rc=status)
   if (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
   VERIFY_(STATUS)
   !Ext_Debug = 20
   CFtemp = ESMF_ConfigCreate (rc=STATUS )
   VERIFY_(STATUS)
   call ESMF_ConfigLoadFile(CFtemp,EXTDATA_CF,rc=status)
   VERIFY_(STATUS)

   totalPrimaryEntries=0
   totalDerivedEntries=0
   call ESMF_ConfigNextLine(CFtemp,__RC__)
   do while (status == ESMF_SUCCESS) 
      call ESMF_ConfigNextLine(CFtemp,rc=status)
      if (status == ESMF_SUCCESS) then 
         call ESMF_ConfigGetAttribute(CFtemp,thisLine,rc=status)
         VERIFY_(STATUS)
         if (trim(thisLine) == "PrimaryExports%%" .or. trim(thisLine) == "DerivedExports%%" ) then
             call advanceAndCount(CFtemp,nLines,rc=status)
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
   ! destroy the config and reopen since there is no rewind function
   call ESMF_ConfigDestroy(CFtemp,rc=status)
   VERIFY_(STATUS)

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

   totalPrimaryEntries = 0
   totalDerivedEntries = 0
   ! reload file and parse it
   CFtemp = ESMF_ConfigCreate (rc=STATUS )
   VERIFY_(STATUS)
   call ESMF_ConfigLoadFile(CFtemp,EXTDATA_CF,rc=status)
   VERIFY_(STATUS)
   call ESMF_ConfigNextLine(CFtemp,__RC__)
   do while(status == ESMF_SUCCESS) 

      call ESMF_ConfigNextLine(CFtemp,rc=status)
      if (status == ESMF_SUCCESS) then
         call ESMF_ConfigGetAttribute(CFtemp,thisLine,rc=status)
         if (trim(thisLine) == "PrimaryExports%%" .or. trim(thisLine) == "DerivedExports%%" ) then
            select case (trim(thisLine))
               case ("PrimaryExports%%")
                  inBlock = .true.
                  do while(inBLock)
                     call ESMF_ConfigNextLine(CFtemp, __RC__)
                     call ESMF_ConfigGetAttribute(CFtemp,thisLine,__RC__)
                     if (trim(thisLine) == "%%") then
                        inBlock = .false.
                     else

                        totalPrimaryEntries = totalPrimaryEntries + 1
                        ! name entry
                        primary%item(totalPrimaryEntries)%name = trim(thisLine)
                        !call ESMF_ConfigGetAttribute(CFtemp, primary%item(totalPrimaryEntries)%name,  __RC__)
                        PrimaryVarNames(totalPrimaryEntries) = primary%item(totalPrimaryEntries)%name
                        ! check if this represents a vector by looking for semicolon
                        primary%item(totalPrimaryEntries)%isVector = ( index(primary%item(totalPrimaryEntries)%name,';').ne.0 )
                        primary%item(totalPrimaryEntries)%vartype = MAPL_ExtDataVectorItem
                        primary%item(totalPrimaryEntries)%foundComp2 = .false.
                        primary%item(totalPrimaryEntries)%foundComp1 = .false.

                        ! units entry
                        call ESMF_ConfigGetAttribute(CFtemp, primary%item(totalPrimaryEntries)%units, __RC__)

                        ! climatology entry
                        call ESMF_ConfigGetAttribute(CFtemp, buffer, __RC__)
                        buffer = ESMF_UtilStringLowerCase(buffer, __RC__)
                        primary%item(totalPrimaryEntries)%cyclic=buffer

                        ! regridding keyword, controls what type of regridding is performed
                        ! options are
                        ! N - conventional bilinear regridding
                        ! Y - conservative tile based regridding
                        ! V - voting, tile based
                        ! F;val - fractional, returns the fraction of the input cells with value, val
                        !         that overlap the target cell
                        call ESMF_ConfigGetAttribute(CFtemp, buffer, __RC__)
                        buffer = ESMF_UtilStringLowerCase(buffer, __RC__)
                        buffer = trim(buffer)
                        if (trim(buffer) == 'y') then
!!$                           primary%item(totalPrimaryEntries)%Trans = MAPL_HorzTransOrderBinning
                           primary%item(totalPrimaryEntries)%trans = REGRID_METHOD_CONSERVE
                        else if (trim(buffer) == 'n') then
!!$                           primary%item(totalPrimaryEntries)%Trans = MAPL_HorzTransOrderBilinear
                           primary%item(totalPrimaryEntries)%trans = REGRID_METHOD_BILINEAR
                        else if (trim(buffer) == 'v') then
!!$                           primary%item(totalPrimaryEntries)%Trans = MAPL_HorzTransOrderSample
                           primary%item(totalPrimaryEntries)%trans = REGRID_METHOD_VOTE
                        else if (index(trim(buffer),'f') ==1 ) then
!!$                           primary%item(totalPrimaryEntries)%Trans = MAPL_HorzTransOrderFraction
                           primary%item(totalPrimaryEntries)%trans = REGRID_METHOD_FRACTION
                           k = index(buffer,';')
                           ASSERT_(k > 0)
                           read(buffer(k+1:),*,iostat=ios) primary%item(totalPrimaryEntries)%FracVal
                        else
                           __raise__(MAPL_RC_ERROR, "the regridding keyword for extdata primary export must be N, Y, V, or F")
                        end if

                        ! refresh template entry
                        call ESMF_ConfigGetAttribute(CFtemp, buffer, __RC__)
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
                        call ESMF_ConfigGetAttribute(CFtemp, c_offset, __RC__)
                        if (trim(c_offset) == "none") then
                           primary%item(totalPrimaryEntries)%do_offset = .false.
                        else
                           primary%item(totalPrimaryEntries)%do_offset = .true.
                           read(c_offset,*,iostat=ios) primary%item(totalPrimaryEntries)%offset
                        end if
                        ! scaling entry
                        call ESMF_ConfigGetAttribute(CFtemp, c_scale, __RC__)
                        if (trim(c_scale) == "none") then
                           primary%item(totalPrimaryEntries)%do_scale = .false.
                        else
                           primary%item(totalPrimaryEntries)%do_scale = .true.
                           read(c_scale,*,iostat=ios) primary%item(totalPrimaryEntries)%scale
                        end if
                     
                        ! variable name on file entry
                        call ESMF_ConfigGetAttribute(CFtemp, primary%item(totalPrimaryEntries)%var,    __RC__)
                        ! file template entry
                        call ESMF_ConfigGetAttribute(CFtemp, primary%item(totalPrimaryEntries)%file,   __RC__)

                        ! the next  three are optional entries to describe the time information about the file template
                        ! these are what is the first valid time you can apply to the file template to get a file that exists
                        ! then you can specify the frequnecy of the file and the units of the frequency
                        call ESMF_ConfigGetAttribute(CFtemp, primary%item(totalPrimaryEntries)%fileReffTime, rc=status)
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
                                read(primary%item(totalPrimaryEntries)%file(k+1:),*,iostat=ios) &
                                     & primary%item(totalPrimaryEntries)%const
                           end if
                           if ( ios /= 0 ) primary%item(totalPrimaryEntries)%const = 0.0
                           ! finally override whatever the cyclic arguement is
                           primary%item(totalPrimaryEntries)%cyclic='n'
                        end if


                        if ( primary%item(totalPrimaryEntries)%isConst .eqv. .false. )  then
                           call CreateTimeInterval(primary%item(totalPrimaryEntries),clock,__RC__)
                        end if
                     end if
                  enddo
               !  Derived Exports
               !  ---------------
               case ("DerivedExports%%")
                  inBlock = .true.
                  do while(inBlock)
                     call ESMF_ConfigNextLine(CFtemp, __RC__)
                     call ESMF_ConfigGetAttribute(CFtemp,thisLine,__RC__)
                     if (trim(thisLine) == "%%") then
                        inBlock = .false.
                     else
                        totalDerivedEntries = totalDerivedEntries + 1
                        derived%item(totalDerivedEntries)%name = trim(thisLine)
                        call ESMF_ConfigGetAttribute(CFtemp,derived%item(totalDerivedEntries)%expression,__RC__)
                        call ESMF_ConfigGetAttribute(CFtemp,derived%item(totalDerivedEntries)%refresh_template, __RC__)
                        derived%item(totalDerivedEntries)%ExtDataAlloc = .true.
                     end if
                  enddo
            end select
         end if
      end if
   end do
   !Done parsing resource file    

   PrimaryItemCount = 0
   DerivedItemCount = 0
   itemCounter = 0

!   find items in primary and derived to fullfill Export state
!   once we find primary or derived put in namespace
    self%ExtDataState = ESMF_StateCreate(Name="ExtDataNameSpace",__RC__)
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
                call ESMF_StateGet(Export,component1,field,__RC__)
                call MAPL_StateAdd(self%ExtDataState,field,__RC__)
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
                call ESMF_StateGet(Export,component2,field,__RC__)
                call MAPL_StateAdd(self%ExtDataState,field,__RC__)
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
                   call ESMF_StateGet(Export,VarName,field,__RC__)
                   call MAPL_StateAdd(self%ExtDataState,field,__RC__)
                else if (ITEMTYPES(I) == ESMF_StateItem_FieldBundle) then
                   primary%item(J)%vartype = MAPL_BundleItem
                   call ESMF_StateGet(Export,VarName,bundle,__RC__)
                   call MAPL_StateAdd(self%ExtDataState,bundle,__RC__)
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
                call ESMF_StateGet(Export,VarName,field,__RC__)
                call MAPL_StateAdd(self%ExtDataState,field,__RC__)
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
          expression = ESMF_UtilStringLowerCase(expression, __RC__)
          if ( index(expression,"mask") /=0  ) then
             derived%item(i)%masking = .true.
          else
             derived%item(i)%masking = .false.
          end if
          if (derived%item(i)%masking) then
             call GetMaskName(derived%item(i)%expression,VarNames,LocalVarNeeded,__RC__)
          else
             call CheckSyntax(derived%item(i)%expression,VarNames,LocalVarNeeded,__RC__)
          end if

          do j=1, primary%nItems
             if (LocalVarNeeded(j)) then
                VarName = trim(primary%item(j)%name)
                call ESMF_StateGet(self%ExtDataState,VarName,field,rc=status)
                if (status /= ESMF_SUCCESS) then
                   VarName = trim(derived%item(i)%name)
                   call ESMF_StateGet(self%ExtDataState,VarName,field,__RC__)
                   VarName=trim(primary%item(j)%name)
                   fieldnew = MAPL_FieldCreate(field,varname,doCopy=.true.,__RC__) 
                   call MAPL_StateAdd(self%ExtDataState,fieldnew,__RC__)
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

   call ESMF_ClockGet(CLOCK, currTIME=time, __RC__)

   PrimaryLoop: do i = 1, self%primary%nItems

      item => self%primary%item(i)

      ! parse refresh template to see if we have a time shift during constant updating
      k = index(item%refresh_template,';')
      call ESMF_TimeIntervalSet(item%tshift,__RC__)
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
         call ESMF_TimeIntervalSet(item%tshift,h=ihr,m=imn,s=isc,__RC__)
         item%refresh_template = "0"
      end if
      call SetRefreshAlarms(clock,primaryItem=item,__RC__)

      if (item%vartype == MAPL_BundleItem) then

         call ESMF_StateGet(self%ExtDataState, trim(item%name), bundle,__RC__)
         ! let us check that bundle is empty
         call ESMF_FieldBundleGet(bundle, fieldcount = fieldcount , __RC__)
         ASSERT_(fieldcount == 0)
         call MAPL_CFIORead(item%file,time,bundle,noread=.true.,ignorecase=self%ignorecase, only_vars=item%var,__RC__)

      end if

!  Read the single step files (read interval equal to zero)
!  --------------------------------------------------------

      if (item%isConst) then

         if (item%vartype == MAPL_FieldItem) then
            call ESMF_StateGet(self%ExtDataState,trim(item%name),field,__RC__)
            call ESMF_FieldGet(field,dimCount=fieldRank,__RC__)
            if (fieldRank == 2) then
                  call MAPL_GetPointer(self%ExtDataState, ptr2d, trim(item%name),__RC__)
                  ptr2d = item%const
            else if (fieldRank == 3) then
                  call MAPL_GetPointer(self%ExtDataState, ptr3d, trim(item%name), __RC__)
                  ptr3d = item%const
            endif
         else if (item%vartype == MAPL_BundleItem) then
            ASSERT_(.false.)
         else if (item%vartype == MAPL_ExtDataVectorItem) then
            call ESMF_StateGet(self%ExtDataState,trim(item%vcomp1),field,__RC__)
            call ESMF_FieldGet(field,dimCount=fieldRank,__RC__)
            if (fieldRank == 2) then 
                  call MAPL_GetPointer(self%ExtDataState, ptr2d, trim(item%vcomp1),__RC__)
                  ptr2d = item%const
            else if (fieldRank == 3) then 
                  call MAPL_GetPointer(self%ExtDataState, ptr3d, trim(item%vcomp1), __RC__)
                  ptr3d = item%const
            endif
            call ESMF_StateGet(self%ExtDataState,trim(item%vcomp2),field,__RC__)
            call ESMF_FieldGet(field,dimCount=fieldRank,__RC__)
            if (fieldRank == 2) then 
                  call MAPL_GetPointer(self%ExtDataState, ptr2d, trim(item%vcomp2),__RC__)
                  ptr2d = item%const
            else if (fieldRank == 3) then 
                  call MAPL_GetPointer(self%ExtDataState, ptr3d, trim(item%vcomp2), __RC__)
                  ptr3d = item%const
            endif
         end if
         cycle
      end if
 
      ! check if this is a single piece of data if user put - for refresh template
      ! by that it is an untemplated file with one time that could not possibly be time interpolated
      if (PrimaryExportIsConstant_(item)) then
         if (index(item%file,'%') == 0) then
            cfio = ESMF_CFIOCreate(cfioObjname='cfio_obj',__RC__)
            call ESMF_CFIOSet(cfio,fName=trim(item%file),__RC__)
            call ESMF_CFIOFileOpen(cfio,fmode=1,__RC__)
            if (cfio%tsteps == 1) then
               item%cyclic = 'single'
            end if
         end if
      end if

      ! get clim year if this is cyclic
      call GetClimYear(item,__RC__)
      ! get levels, other information
      call GetLevs(item,time,self%allowExtrap,__RC__)

      ! create interpolating fields, check if the vertical levels match the file
      if (item%vartype == MAPL_FieldItem) then

         call ESMF_StateGet(self%ExtDataState, trim(item%name), field,__RC__)
         call ESMF_FieldGet(field,grid=grid,rank=fieldRank,__RC__)

         lm=0
         if (fieldRank==3) then
            call ESMF_FieldGet(field,0,farrayPtr=ptr3d, rc=status)
           if (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out

            lm = size(ptr3d,3)
         end if   
         if (item%lm /= lm .and. lm /= 0 .and. item%havePressure) then
            item%do_VertInterp = .true.
         else if (item%lm /= lm .and. lm /= 0) then
            item%do_Fill = .true.
         end if
         item%finterp1 = MAPL_FieldCreate(field,item%var,doCopy=.true.,__RC__)
         item%finterp2 = MAPL_FieldCreate(field,item%var,doCopy=.true.,__RC__)

      else if (item%vartype == MAPL_BundleItem) then

         call ESMF_StateGet(self%ExtDataState, trim(item%name), bundle,__RC__)
         call ESMF_FieldBundleGet(bundle,grid=grid,__RC__)
         call ESMF_ClockGet(CLOCK, currTIME=time, __RC__)
         item%binterp1 = ESMF_FieldBundleCreate( __RC__)
         call ESMF_FieldBundleSet(item%binterp1, GRID=GRID, __RC__)
         item%binterp2 = ESMF_FieldBundleCreate( __RC__)
         call ESMF_FieldBundleSet(item%binterp2, GRID=GRID, __RC__)
         call MAPL_CFIORead(item%file,time,item%binterp1,noread=.true.,ignorecase=self%ignorecase,only_vars=item%var,__RC__)
         call MAPL_CFIORead(item%file,time,item%binterp2,noread=.true.,ignorecase=self%ignorecase,only_vars=item%var,__RC__)
 
      else if (item%vartype == MAPL_ExtDataVectorItem) then
     
         ! check that we are not asking for conservative regridding
!!$         if (item%Trans /= MAPL_HorzTransOrderBilinear) then
         if (item%Trans /= REGRID_METHOD_BILINEAR) then
            if (mapl_am_i_root()) write(*,*)"No conservative regridding with vectors"
            ASSERT_(.false.)
         end if 

         call ESMF_StateGet(self%ExtDataState, trim(item%vcomp1), field,__RC__)
         call ESMF_FieldGet(field,grid=grid,rank=fieldRank,__RC__)

         lm = 0 
         if (fieldRank==3) then
            call ESMF_FieldGet(field,0,farrayPtr=ptr3d,rc=status)
            if (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            lm = size(ptr3d,3)
         end if
         if (item%lm /= lm .and. item%havePressure) then
            item%do_VertInterp = .true.
         else if (item%lm /= lm .and. lm /= 0) then
            item%do_Fill = .true.
         end if
         item%v1_finterp1 = MAPL_FieldCreate(field, item%vcomp1,doCopy=.true.,__RC__)
         item%v1_finterp2 = MAPL_FieldCreate(field, item%vcomp1,doCopy=.true.,__RC__)
         call ESMF_StateGet(self%ExtDataState, trim(item%vcomp2), field,__RC__)
         item%v2_finterp1 = MAPL_FieldCreate(field, item%vcomp2,doCopy=.true.,__RC__)
         item%v2_finterp2 = MAPL_FieldCreate(field, item%vcomp2,doCopy=.true.,__RC__)
      end if

      allocate(item%refresh_time,__STAT__)

      call ESMF_TimeSet(item%refresh_time, yy=0, __RC__)
   end do PrimaryLoop

   DerivedLoop: do i =1, self%derived%nItems
      allocate(self%derived%item(i)%refresh_time,__STAT__)

      derivedItem => self%derived%item(i)

      ! parse refresh template to see if we have a time shift during constant updating
      k = index(derivedItem%refresh_template,';')
      call ESMF_TimeIntervalSet(derivedItem%tshift,__RC__)
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
         call ESMF_TimeIntervalSet(derivedItem%tshift,h=ihr,m=imn,s=isc,__RC__)
         derivedItem%refresh_template = "0"
      end if

      call SetRefreshAlarms(clock,derivedItem=derivedItem,__RC__)

      call ESMF_TimeSet(self%derived%item(i)%refresh_time, yy=0, __RC__)
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

! Check if we have any files that would need to be vertically interpolated
! if so ensure that PS is done first
   allocate(self%primaryOrder(size(self%primary%item)),__STAT__)
   do i=1,size(self%primary%item)
      self%primaryOrder(i)=i
   enddo
!  check for PS
   idx = -1
   if (any(self%primary%item%do_VertInterp .eqv. .true.)) then
      do i=1,size(self%primary%item)
         if (self%primary%item(i)%name=='PS') then
            idx =i
         end if
         if (self%primary%item(i)%vartype==MAPL_BundleItem) then
            ASSERT_(.false.)
         end if
      enddo
      ASSERT_(idx/=-1)
      self%primaryOrder(1)=idx
      self%primaryOrder(idx)=1
      self%primary%item(idx)%units = ESMF_UtilStringUppercase(self%primary%item(idx)%units,rc=status)
      ASSERT_(trim(self%primary%item(idx)%units)=="PA")
   end if
!  check for PHIS
   idx = -1
   if (any(self%primary%item%do_VertInterp .eqv. .true.)) then
      do i=1,size(self%primary%item)
         if (self%primary%item(i)%name=='PHIS') then
            idx =i
         end if
         if (self%primary%item(i)%vartype==MAPL_BundleItem) then
            ASSERT_(.false.)
         end if
      enddo
      if (idx/=-1) then
         self%primaryOrder(2)=idx
         self%primaryOrder(idx)=2
         self%primary%have_phis=.true.
      end if
   end if

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

   !Done parsing resource file    

!  Set has run to false to we know when we first go to run method it is first call
   hasRun = .false.

!   call MAPL_TimerOff(MAPLSTATE,"Initialize")
!   call MAPL_TimerOff(MAPLSTATE,"TOTAL")
!  All done
!  --------
   RETURN_(ESMF_SUCCESS)

   END SUBROUTINE Initialize_


!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Run_ --- Runs MAPL_ExtData
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

! !DESCRIPTION: This is a simple ESMF wrapper.
!
! !REVISION HISTORY:
!
!  12Dec2009  da Silva  Design and first implementation.
!
!EOP
!-------------------------------------------------------------------------

    ! local variables
   type(ESMF_Clock)                  :: clock
   type(ESMF_State)                  :: import, export

   type(MAPL_ExtData_state), pointer :: self        ! Legacy state
   type(ESMF_Field)                  :: field       ! Field
   type(ESMF_FieldBundle)            :: bundle
   type(ESMF_Config)                 :: CF          ! Universal Config 

   character(len=ESMF_MAXSTR)        :: comp_name
   character(len=ESMF_MAXSTR)        :: Iam
   integer                           :: status

   type(PrimaryExport), pointer      :: item
   type(DerivedExport), pointer      :: derivedItem
   integer                           :: i, j

   type(ESMF_Time)                   :: time, time0
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
   real, pointer, dimension(:,:,:)   :: fptr
   integer                           :: is, js, ie, je


!  Declare pointers to IMPORT/EXPORT/INTERNAL states 
!  -------------------------------------------------
!  #include "MAPL_ExtData_DeclarePointer___.h"
  
!  Get my name and set-up traceback handle
!  ---------------------------------------
   Iam = 'ModelAdvance'
   call ESMF_GridCompGet( GC, name=comp_name, __RC__ )
   Iam = trim(comp_name) // '::' // trim(Iam)
   call ESMF_GridCompGet(GC, clock=clock, importState=IMPORT, &
   !call NUOPC_ModelGet(GC, modelClock=clock, importState=IMPORT, &
        exportState=EXPORT, rc=rc)
   if (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
   
    call ESMF_LogWrite('ExtData: modelAdvance', ESMF_LOGMSG_INFO, rc=rc)

!  Call Run for every Child
!  -------------------------
!   call MAPL_GenericRun ( GC, IMPORT, EXPORT, CLOCK,  __RC__)


!  Extract relevant runtime information
!  ------------------------------------
   call extract_ ( GC, self, CF, __RC__ )

   if (.not. self%active) then
      RETURN_(ESMF_SUCCESS)
   end if

!   call MAPL_GetObjectFromGC ( gc, MAPLSTATE, RC=STATUS)
!   VERIFY_(STATUS) 
!   call MAPL_TimerOn(MAPLSTATE,"TOTAL")
!   call MAPL_TimerOn(MAPLSTATE,"Run")

   call ESMF_ClockGet(CLOCK, currTIME=time0, __RC__)


!  Fill in the internal state with data from the files 
!  ---------------------------------------------------

   allocate(doUpdate(self%primary%nitems),stat=status)
   VERIFY_(STATUS)
   doUpdate = .false.
   allocate(useTime(self%primary%nitems),stat=status)
   VERIFY_(STATUS)

!   call MAPL_TimerOn(MAPLSTATE,"-Read_Loop")
 
  READ_LOOP: do i = 1, self%primary%nItems

      item => self%primary%item(self%primaryOrder(i))

      if (item%isConst) cycle
      ! Debug level 1
!      If (Ext_Debug > 0) Then
         If (MAPL_AM_I_ROOT()) Then
            Write(*,'(a,3(x,a))') '>> Reading ', trim(item%var), 'from', trim(item%file)
         End If
!      End If

      NotSingle = .true.
      if (trim(item%cyclic) == 'single') NotSingle = .false.
      
 !     call MAPL_TimerOn(MAPLSTATE,"--CheckUpd")

      call CheckUpdate(doUpdate_,time,time0,hasRun,primaryItem=item,__RC__)
      doUpdate(i) = doUpdate_
!      call MAPL_TimerOff(MAPLSTATE,"--CheckUpd")

      DO_UPDATE: if (doUpdate_) then

         HAS_RUN: if ( hasRun .eqv. .false.) then

!            call MAPL_TimerOn(MAPLSTATE,"--Bracket")
            if (NotSingle) then
               ! update left time
               call UpdateBracketTime(item,time,"L",item%interp_time1, & 
                    item%time1,file_processed1,item%isAssigned1,item%cfio1,item%tSeries1,self%allowExtrap,rc=status)
               VERIFY_(status)

               ! update right time
               call UpdateBracketTime(item,time,"R",item%interp_time2, &
                    item%time2,file_processed2,item%isAssigned2,item%cfio2,item%tSeries2,self%allowExtrap,rc=status)
               VERIFY_(STATUS)
            else
               ! just get time on the file
               item%time1 = MAPL_ExtDataGetFStartTime(trim(item%file),__RC__)
               item%interp_time1 = item%time1
               file_processed1 = item%file
            end if
!            call MAPL_TimerOff(MAPLSTATE,"--Bracket")

            ! read bracketing data

!            call MAPL_TimerOn(MAPLSTATE,"--Read")
            if (item%vartype /= MAPL_ExtDataVectorItem) then

               call MAPL_ExtDataGridCompRead(item,file_processed1, self,MAPL_ExtDataLeft,__RC__)

               if (NotSingle) then
                  call MAPL_ExtDataGridCompRead(item,file_processed2, self,MAPL_ExtDataRight,__RC__)

               end if

            else

               call ESMF_ConfigGetAttribute(self%cf, value = NX, Label="NX:", __RC__)
               call ESMF_ConfigGetAttribute(self%cf, value = NY, Label="NY:", __RC__)
               call MAPL_ExtDataReadVector(item,file_processed1,self,MAPL_ExtDataLeft,__RC__)

               if (NotSingle) then
                  call MAPL_ExtDataReadVector(item,file_processed2,self,MAPL_ExtDataRight,__RC__)
               end if

            end if
!            call MAPL_TimerOff(MAPLSTATE,"--Read")

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

!            call MAPL_TimerOn(MAPLSTATE,'--Swap')
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

!            call MAPL_TimerOff(MAPLSTATE,'--Swap')

            if (updateR) then

!               call MAPL_TimerOn(MAPLSTATE,'--Bracket')

               call UpdateBracketTime(item,time,"R",item%interp_time2, &
                    item%time2,file_processed,item%isAssigned2,item%cfio2,item%tSeries2,self%allowExtrap,rc=status)
               VERIFY_(STATUS)

!               call MAPL_TimerOff(MAPLSTATE,'--Bracket')

!               call MAPL_TimerOn(MAPLSTATE,"--Read")
    
               if (item%vartype /= MAPL_ExtDataVectorItem) then

                  call MAPL_ExtDataGridCompRead(item,file_processed,self,MAPL_ExtDataRight,__RC__)

               else if (item%vartype == MAPL_ExtDataVectorItem) then

                  call ESMF_ConfigGetAttribute(self%cf, value = NX, Label="NX:", __RC__)
                  call ESMF_ConfigGetAttribute(self%cf, value = NY, Label="NY:", __RC__)
                  call MAPL_ExtDataReadVector(item,file_processed,self,MAPL_ExtDataRight,__RC__)

               end if
!               call MAPL_TimerOff(MAPLSTATE,"--Read")

            end if

            if (updateL) then
!               call MAPL_TimerOn(MAPLSTATE,'--Bracket')

               call UpdateBracketTime(item,time,"L",item%interp_time1, &
                    item%time1,file_processed,item%isAssigned1,item%cfio1,item%tSeries1,self%allowExtrap,rc=status)
               VERIFY_(STATUS)

!               call MAPL_TimerOff(MAPLSTATE,'--Bracket')

!               call MAPL_TimerOn(MAPLSTATE,"--Read")

               if (item%vartype /= MAPL_ExtDataVectorItem) then

                  call MAPL_ExtDataGridCompRead(item,file_processed,self,MAPL_ExtDataLeft,__RC__)

               else if (item%vartype == MAPL_ExtDataVectorItem) then

                  call ESMF_ConfigGetAttribute(self%cf, value = NX, Label="NX:", __RC__)
                  call ESMF_ConfigGetAttribute(self%cf, value = NY, Label="NY:", __RC__)
                  call MAPL_ExtDataReadVector(item,file_processed,self,MAPL_ExtDataLeft,__RC__)

               end if

!               call MAPL_TimerOff(MAPLSTATE,"--Read")

            end if

         endif NOT_SINGLE

         useTime(i) = time

      end if DO_UPDATE

      if (PrimaryExportIsConstant_(item) .and. associated(item%refresh_time)) then
         deallocate(item%refresh_time)
         item%refresh_time => null()
      end if

   end do READ_LOOP

!   call MAPL_TimerOff(MAPLSTATE,"-Read_Loop")

!   call MAPL_TimerOn(MAPLSTATE,"-Interpolate")
 
   INTERP_LOOP: do i = 1, self%primary%nItems

      item => self%primary%item(self%primaryOrder(i))

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

!   call MAPL_TimerOff(MAPLSTATE,"-Interpolate")

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

#define PEGGY_DEBUG
#ifdef PEGGY_DEBUG
!  Check the value of the PLE0 
   call ESMF_StateGet(EXPORT,'pressure_at_layer_edges_before_advection',field, rc=rc)   
   call ESMF_FieldGet(field, farrayPtr=fptr, rc=rc)
   if (MAPL_Am_I_Root()) then
    	 print *, "EXTDATA: PLE0", lbound(fptr), ubound(fptr)
    endif
#endif
   
   if (hasRun .eqv. .false.) hasRun = .true.
!   call MAPL_TimerOff(MAPLSTATE,"Run")
!   call MAPL_TimerOff(MAPLSTATE,"TOTAL")

   RETURN_(ESMF_SUCCESS)

   END SUBROUTINE ModelAdvance

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Finalize_ --- Finalize MAPL_ExtData
!
! !INTERFACE:
!

   SUBROUTINE Finalize_ ( GC, IMPORT, EXPORT, CLOCK, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   type(ESMF_Clock),  intent(inout) :: CLOCK      ! The clock

! !OUTPUT PARAMETERS:

   type(ESMF_GridComp), intent(inout)  :: GC     ! Grid Component
   type(ESMF_State), intent(inout) :: IMPORT     ! Import State
   type(ESMF_State), intent(inout) :: EXPORT     ! Export State
   integer, intent(out) ::  rc                   ! Error return code:
                                                 !  0 - all is well
                                                 !  1 - 

! !DESCRIPTION: This is a simple ESMF wrapper.
!
! !REVISION HISTORY:
!
!  12Dec2009  da Silva  Design and first implementation.
!
!EOP
!-------------------------------------------------------------------------

   type(MAPL_ExtData_state), pointer :: self        ! Legacy state
   type(ESMF_Config)                 :: CF          ! Universal Config 

   character(len=ESMF_MAXSTR)        :: comp_name
   character(len=ESMF_MAXSTR)        :: Iam
   integer                           :: status

   integer                           :: i


!  Get my name and set-up traceback handle
!  ---------------------------------------
   Iam = 'Finalize_'
   call ESMF_GridCompGet( GC, name=comp_name, __RC__ )
   Iam = trim(comp_name) // trim(Iam)

!  Finalize MAPL Generic
!  ---------------------
   call MAPL_GenericFinalize ( GC, IMPORT, EXPORT, CLOCK,  __RC__ )

!  Extract relevant runtime information
!  ------------------------------------
   call extract_ ( GC, self, CF, __RC__)

!  Free the memory used for the bracketing arrays
!  -----------------------------------------------------------
   if (self%active) then
      do i = 1, self%primary%nItems

         if (self%primary%item(i)%isConst) cycle

         if (trim(self%primary%item(i)%refresh_template) == "0") then
            if (self%primary%item(i)%vartype == MAPL_FieldItem) then
               call ESMF_FieldDestroy(self%primary%item(i)%finterp1,__RC__)
               call ESMF_FieldDestroy(self%primary%item(i)%finterp2,__RC__)
            end if
         end if

         if (associated(self%primary%item(i)%refresh_time)) then
            deallocate(self%primary%item(i)%refresh_time)
         end if

!  Destroy CFIO associated with each item
         !if (self%primary%item(i)%isAssigned1) then
            !call ESMF_CFIODestroy(self%primary%item(i)%cfio1,__RC__)
            !if (allocated(self%primary%item(i)%tSeries1)) then
               !deallocate(self%primary%item(i)%tSeries1)
            !end if
            !self%primary%item(i)%isAssigned1 = .False.
         !end if
         !if (self%primary%item(i)%isAssigned2) then
            !call ESMF_CFIODestroy(self%primary%item(i)%cfio2,__RC__)
            !if (allocated(self%primary%item(i)%tSeries2)) then
               !deallocate(self%primary%item(i)%tSeries2)
            !end if
            !self%primary%item(i)%isAssigned2 = .False.
         !end if
      end do


!  Free the memory used to hold the primary export items
!  -----------------------------------------------------
      if (associated(self%primary%item)) then
         deallocate(self%primary%item)
      end if
   end if


!  All done
!  --------
   RETURN_(ESMF_SUCCESS)

 end SUBROUTINE Finalize_

!.......................................................................

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
   
! ............................................................................

   logical function PrimaryExportIsConstant_(item)
   
      type(PrimaryExport), intent(in) :: item

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

      if ( trim(item%refresh_template) == '-') then
          DerivedExportIsConstant_ = .true. 
      else
          DerivedExportIsConstant_ = .false.
      end if

   end function DerivedExportIsConstant_

! ............................................................................

  subroutine scale_field_(offset, scale_factor, field, rc)
     real, intent(in)                  :: scale_factor
     real, intent(in)                  :: offset
     type (ESMF_Field), intent(inout)  :: field
     integer, optional, intent (inout) :: rc

     __Iam__('scale_field_')

     integer       :: fieldRank
     real, pointer :: xy(:,:)    => null()
     real, pointer :: xyz(:,:,:) => null()


     call ESMF_FieldGet(field, dimCount=fieldRank, __RC__)

     ASSERT_(fieldRank == 2 .or. fieldRank == 3)

     if (fieldRank == 2) then
        call ESMF_FieldGet(field, farrayPtr=xy, __RC__)

        if (associated(xy)) then
           xy = offset + scale_factor*xy
        end if
     else if (fieldRank == 3) then
        call ESMF_FieldGet(field, farrayPtr=xyz, __RC__)

        if (associated(xyz)) then
           xyz = offset + scale_factor*xyz
        end if
     end if

     return 
  end subroutine scale_field_

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
    
     ! test the length of the timestamp template
     ASSERT_(len_trim(template) < DATETIME_MAXSTR_)

     buff = trim(template)
     buff = ESMF_UtilStringLowerCase(buff, __RC__)
      
     ! test if the template is empty and return the current time as result
     if (buff == '-'  .or. buff == '--'   .or. buff == '---' .or. &
         buff == 'na' .or. buff == 'none' .or. buff == 'n/a') then

        timestamp_ = time
     else   
        ! split the time stamp template into a date and time strings
        i = scan(buff, 't')
        If (.not.(i > 3)) Then
           Write(*,'(a,a,a)') 'ERROR: Time stamp ', Trim(template), &
           ' uses the fixed format, and must therefore contain a T'
           ASSERT_(.False.)
        End If

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
 
  subroutine CreateTimeInterval(item,clock,rc)
     type(PrimaryExport)      , intent(inout) :: item
     type(ESMF_Clock)          , intent(in   ) :: clock
     integer, optional         , intent(out  ) :: rc

     __Iam__('CreateTimeInterval')
     
     integer                    :: iyy,imm,idd,ihh,imn,isc
     integer                    :: lasttoken
     character(len=2)           :: token
     type(ESMF_Time)            :: time
     integer                    :: cindex,pindex
     character(len=ESMF_MAXSTR) :: creffTime, ctInt
     
     creffTime = ''
     ctInt     = ''
     call ESMF_ClockGet (CLOCK, currTIME=time, __RC__)
     if (.not.item%hasFileReffTime) then
        ! if int_frequency is less than zero than try to guess it from the file template
        ! if that fails then it must be a single file or a climatology 

        call ESMF_TimeGet(time, yy=iyy, mm=imm, dd=idd,h=ihh, m=imn, s=isc  ,__RC__)
        !=======================================================================
        ! Using "now" as a reference time makes it difficult to find a file if
        ! we need to extrapolate, and doesn't make an awful lot of sense anyway.
        ! Instead, use the start of (current year - 20) or 1985, whichever is
        ! earlier (SDE 2016-12-30)
        iyy = Min(iyy-20,1985)
        imm = 1
        idd = 1
        ihh = 0
        imn = 0
        isc = 0
        !=======================================================================
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
        ! Reference time should look like:
        ! YYYY-MM-DDThh:mm:ssPYYYY-MM-DDThh:mm:ss
        ! The date before the P is the reference time, from which future times
        ! will be taken. The date after the P is the frequency with which the
        ! file changes. For example, if the data is referenced to 1985 and
        ! there is 1 file per year, the reference time should be
        ! 1985-01-01T00:00:00P0001-00-00T00:00:00
        ! Get refference time, if not provided use current model date
        pindex=index(item%FileReffTime,'P')
        if (pindex==0) then 
           Write(*,'(a,a,a)') 'ERROR: File template ', item%file, ' has invalid reference date format'
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

     If (Mapl_Am_I_Root().and.(Ext_Debug > 19)) Then
        Write(*,'(5(a))') ' >> REFFTIME for ',trim(item%file),': ',trim(item%FileReffTime)
        call ESMF_TimeGet(item%reff_time,yy=iyy,mm=imm,dd=idd,h=ihh,m=imn,s=isc,rc=status)
        Write(*,'(a,I0.4,5(a,I0.2))') ' >> Reference time: ',iYy,'-',iMm,'-',iDd,' ',iHh,':',iMn,':',iSc
        call ESMF_TimeIntervalGet(item%frequency,yy=iyy,mm=imm,d=idd,h=ihh,m=imn,s=isc,rc=status)
        Write(*,'(a,I0.4,5(a,I0.2))') ' >> Frequency     : ',iYy,'-',iMm,'-',iDd,' ',iHh,':',iMn,':',iSc
     End If
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

  subroutine GetLevs(item, time, allowExtrap, rc)

     type(PrimaryExport)      , intent(inout) :: item
     type(ESMF_Time)          , intent(inout) :: time
     logical                  , intent(in   ) :: allowExtrap
     integer, optional        , intent(out  ) :: rc

     __Iam__('GetLevs')

     type(ESMF_CFIO)            :: cfio
     type(ESMF_CFIOGRID),pointer :: cfiogrid
     integer(ESMF_KIND_I4)      :: iyr,imm,idd,ihr,imn,iss,i,n,refYear
     character(len=ESMF_MAXPATHLEN) :: file
     integer                    :: nymd, nhms
     type(ESMF_Time)            :: fTime
     real, pointer              :: levFile(:) => null()
     integer                    :: nVars
     type(ESMF_CFIOVarInfo), pointer :: vars(:)
     character(len=ESMF_MAXSTR) :: cfiovarname,units,buff,varname
     logical                    :: found,intOK,lFound
     integer                    :: maxOffset

     type(ESMF_TimeInterval)    :: zero

     call ESMF_TimeIntervalSet(zero,__RC__)

     if (item%frequency == zero) then

        file = item%file
        Inquire(file=trim(file),EXIST=found)

     else
        buff = trim(item%refresh_template)
        buff = ESMF_UtilStringLowerCase(buff, __RC__)
        if ( index(buff,'t')/=0) then
           if (index(buff,'p') == 0) then
              ftime = timestamp_(time,buff,__RC__)
           else
              ftime = time
           end if
        else
           ftime = time
        end if

        call ESMF_TimeGet(fTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=iss,__RC__)
        if (item%cyclic == 'y') then
           iyr = item%climyear
        end if
       call MAPL_PackTime(nymd,iyr,imm,idd)
       call MAPL_PackTime(nhms,ihr,imn,iss)
       call ESMF_CFIOStrTemplate(file,item%file,'GRADS',nymd=nymd,nhms=nhms,__STAT__)
       Inquire(file=trim(file),EXIST=found)

     end if

     if (found) then
        call MakeCFIO(file, cfio, found, __RC__)
     else
        if (allowExtrap .and. (item%cyclic == 'n') ) then

           ftime = item%reff_time
           n = 0
           maxOffSet = 100
           call ESMF_TimeGet(item%reff_time,yy=refYear)
           lfound = .false.
           intOK = .true.
           do while (intOK .and. (.not.lfound))
              call ESMF_TimeGet(fTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=iss,__RC__)
              call MAPL_PackTime(nymd,iyr,imm,idd)
              call MAPL_PackTime(nhms,ihr,imn,iss)
              call gx_(file,item%file,nymd=nymd,nhms=nhms,__STAT__)
              call MakeCFIO(file,cfio,lfound)
              Inquire(FILE=trim(file),EXIST=found)
              intOK = (abs(iYr-refYear)<maxOffset)
              if (.not.lfound) then
                 n = n + 1
                 ftime = ftime + item%frequency
              end if
           enddo

           if (.not.lfound) then
              if (mapl_am_i_root()) write(*,*)'From ',trim(item%file),'coud not find file with extrapolation'
              ASSERT_(.false.)
           end if
        else
           if (mapl_am_i_root()) write(*,*)'From ',trim(item%file),' could not find time no extrapolation',item%climYear
           ASSERT_(.false.)
        end if

     end if
     call ESMF_CFIOGet       (CFIO,     grid=CFIOGRID,__RC__)
     call ESMF_CFIOGridGet   (CFIOGRID,KM=item%LM,__RC__)
     if (item%LM /= 0) then
        call ESMF_CFIOGridGet   (CFIOGRID,lev=levfile,levUnit=item%levUnit,__RC__)
        allocate(item%levs(size(levFile)),__STAT__)

        if (levFile(1)>levFile(size(levFile))) then
           do i=1,size(levFile)
              item%levs(i)=levFile(size(levFile)-i+1)
           enddo
        else
           item%levs=levFile
        end if
        if (trim(item%levUnit)=='hPa') item%levs=item%levs*100.0
        ! check if the level unit matches something that is pressure
        if (trim(item%levUnit) == 'hPa') then
           item%havePressure = .true.
        end if

     end if

     call ESMF_CFIOGet (CFIO,varObjs=VARS, nVars=nVars, RC=STATUS)
     VERIFY_(STATUS)
     varname = trim(item%var)
     varname = ESMF_UtilStringUpperCase(varname,rc=status)
     do i=1,nVars
        call ESMF_CFIOVarInfoGet(Vars(i),vname=CFIOVARNAME, vunits=UNITS,rc=status)
        VERIFY_(STATUS)
        cfiovarname = ESMF_UtilStringUpperCase(cfiovarname,rc=status)
        if (trim(cfiovarname)==trim(varname)) then
           item%units=units
        endif
     enddo
     call ESMF_CFIODestroy(cfio,__RC__)
     RETURN_(ESMF_SUCCESS)

  end subroutine GetLevs

  subroutine UpdateBracketTime(item,cTime,bSide,interpTime,fileTime,file_processed,isAssigned,xCFIO,xTSeries,allowExtrap,rc)
     type(PrimaryExport),                 intent(inout) :: item
     type(ESMF_Time),                     intent(inout) :: cTime
     character(len=1),                    intent(in   ) :: bSide
     type(ESMF_TIME),                     intent(inout) :: interpTime
     type(ESMF_TIME),                     intent(inout) :: fileTime
     character(len=*),                    intent(inout) :: file_processed
     type(ESMF_CFIO),                     intent(inout) :: xCFIO
     type(ESMF_Time), Allocatable,        intent(inout) :: xTSeries(:)
     logical,                             intent(inout) :: isAssigned
     logical,                             intent(in   ) :: allowExtrap
     integer, optional,                   intent(out  ) :: rc

     __Iam__('UpdateBracketTime')

     type(ESMF_Time)                            :: newTime
     integer                                    :: curDate,curTime,n
     integer(ESMF_KIND_I4)                      :: iyr, imm, idd, ihr, imn, isc, oldYear
     integer(ESMF_KIND_I4)                      :: fyr, fmm, fdd, fhr, fmn, fsc
     type(ESMF_TimeInterval)                    :: zero
     type(ESMF_Time)                            :: fTime
     logical                                    :: UniFileClim
     type(ESMF_Time)                            :: readTime

     ! Allow for extrapolation.. up to a limit
     integer                                    :: yrOffset,yrOffsetStamp
     integer(ESMF_KIND_I4)                      :: cYearOff, refYear
     character(len=ESMF_MAXSTR)                 :: buff
     integer, parameter                         :: maxOffset=10000
     logical                                    :: found, newFile
     logical                                    :: LExtrap, RExtrap, LExact, RExact
     logical                                    :: LSide, RSide, intOK, bracketScan
   
     call ESMF_TimeIntervalSet(zero,__RC__)

     ! Default
     fTime = cTime
     bracketScan = .False.

     
     ! Is there only one file for this dataset?
     if (item%frequency == zero) then
        if (mapl_am_I_root().and.(Ext_Debug > 19)) Then
           write(*,'(a,a,a,a)') ' DEBUG: Scanning fixed file ',trim(item%file),' for side ',bSide
        end if
        UniFileClim = .false.
        ! if the file is constant, i.e. no tokens in in the template
        ! but it was marked as cyclic we must have a year long climatology 
        ! on one file, set UniFileClim to true
        if (trim(item%cyclic)=='y') UniFileClim = .true.
        file_processed = item%file
        ! Generate CFIO if needed
        If (.not.isAssigned) Then
           call MakeCFIO(file_processed,xCFIO,found,rc=status)
           ! Retrieve the time series
           allocate(xTSeries(xCFIO%tSteps))
           call GetTimesOnFile(xCFIO,xTSeries,rc=status)
           isAssigned = .True.
        End If
        If (status /= ESMF_SUCCESS) then
           if (mapl_am_I_root()) Then
              write(*,'(a,a)') ' ERROR: Time vector retrieval failed on fixed file ',trim(item%file)
           end if
           RETURN_(ESMF_FAILURE)
        end if
        call GetBracketTimeOnSingleFile(xCFIO,xTSeries,cTime,bSide,UniFileClim,interpTime,fileTime,allowExtrap,item%climYear,rc=status)
        if (status /= ESMF_SUCCESS) then
           if (mapl_am_I_root()) Then
              write(*,'(a,a,a,a)') ' ERROR: Bracket timing request failed on fixed file ',trim(item%file),' for side ',bSide
           end if
           RETURN_(ESMF_FAILURE)
        end if
     else 
        if (mapl_am_I_root().and.(Ext_Debug > 19)) Then
           write(*,'(a,a,a,a)') ' DEBUG: Scanning template ',trim(item%file),' for side ',bSide
           call ESMF_TimeGet(cTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
           Write(*,'(a,I0.4,5(a,I0.2))') ' >> >> >> Target time   : ',iYr,'-',iMM,'-',iDD,' ',iHr,':',iMn,':',iSc
           call ESMF_TimeGet(item%reff_time,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
           Write(*,'(a,I0.4,5(a,I0.2))') ' >> >> >> Reference time: ',iYr,'-',iMM,'-',iDD,' ',iHr,':',iMn,':',iSc
        end if
        UniFileClim = .false.
        found = .false.
        ! Start by assuming the file we want exists
        if (trim(item%cyclic)=='y') then
           ! if climatology compute year offset
           call ESMF_TimeGet(cTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
           yrOffset = item%climYear - iyr
           call ESMF_TimeSet(fTime,yy=item%climYear,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
        else
           yrOffset = 0
           if (item%reff_time > cTime) then
              Write(*,'(a,a,a)') 'ERROR: Reference time for file ', Trim(item%file),' is too late'
              ASSERT_(.False.)
           end if
           ! This approach causes a problem if cTime and item%reff_time are too far
           ! apart - do it the hard way instead... 
           ftime = item%reff_time
           n = 0
           ! SDE DEBUG: This caused problems in the past but the
           ! alternative is far too slow... need to keep an eye 
           ! on this but the Max(0,...) should help.
           n = max(0,floor((cTime-item%reff_time)/item%frequency))
           if (n>0) fTime = fTime + (n*item%frequency)
           do while (.not.found)
              ! SDE: This needs to be ">"
              found = ((ftime + item%frequency) > ctime)
              if (.not.found) then
                 n = n + 1
                 ftime = fTime+item%frequency
              end if
           end do
           if (mapl_am_I_root().and.(Ext_Debug > 19)) Then
              write(*,'(a,a,a,a)') ' DEBUG: Untemplating ',trim(item%file)
              call ESMF_TimeGet(cTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
              Write(*,'(a,I0.4,5(a,I0.2))') ' >> >> >> Target time   : ',iYr,'-',iMM,'-',iDD,' ',iHr,':',iMn,':',iSc
              call ESMF_TimeGet(fTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
              Write(*,'(a,I0.4,5(a,I0.2))') ' >> >> >> File time     : ',iYr,'-',iMM,'-',iDD,' ',iHr,':',iMn,':',iSc
              call ESMF_TimeIntervalGet(item%frequency,yy=iyr,mm=imm,d=idd,h=ihr,m=imn,s=isc,__RC__)
              Write(*,'(a,I0.4,5(a,I0.2))') ' >> >> >> item%frequency     : ',iYr,'-',iMM,'-',iDD,' ',iHr,':',iMn,':',iSc
              Write(*,'(a,I5)')             ' >> >> >> N             : ',n
           end if
        end if
        readTime = cTime
        call ESMF_TimeGet(fTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
        call MAPL_PackTime(curDate,iyr,imm,idd)
        call MAPL_PackTime(curTime,ihr,imn,isc)
        call gx_(file_processed,item%file,nymd=curDate,nhms=curTime,__STAT__)
        Inquire(FILE=trim(file_processed),EXIST=found)
        If (found) Then
           if (mapl_am_I_root().and.(Ext_Debug > 19)) Then 
              write(*,'(a,a,a,a)') ' DEBUG: Target file for ',trim(item%file),' found and is ',trim(file_processed)
           end if
           !yrOffset = 0
        Else if (allowExtrap) then 
           if (mapl_am_I_root().and.(Ext_Debug > 19)) Then
              write(*,'(a,a,a)') ' DEBUG: Propagating forwards on ',trim(item%file),' from reference time'
              call ESMF_TimeGet(item%reff_time,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
              Write(*,'(a,I0.4,5(a,I0.2))') ' >> >> >> Reference time: ',iYr,'-',iMM,'-',iDD,' ',iHr,':',iMn,':',iSc
           end if
           ! Go back to the reference time, and propagate forwards until we find
           ! the first valid file
           ftime = item%reff_time
           call ESMF_TimeGet(item%reff_time,yy=refYear,__RC__)
           If (refYear.lt.1850) Then
              if (mapl_am_I_root()) Then
                 write(*,'(a,I0.4,a,a)') 'Reference year too early (', refYear, '). Aborting search for data from ',trim(item%file)
              end if
              RETURN_(ESMF_FAILURE)
           End If
           intOK = .True.
           found = .false.
           ! yrOffset currently tracking how far we are from the reference year
           n = 0
           yrOffset = 0
           ftime = item%reff_time
           Do While (intOK.and.(.not.found))
              call ESMF_TimeGet(fTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
              call MAPL_PackTime(curDate,iyr,imm,idd)
              call MAPL_PackTime(curTime,ihr,imn,isc)
              call gx_(file_processed,item%file,nymd=curDate,nhms=curTime,__STAT__)
              Inquire(FILE=trim(file_processed),EXIST=found)
              yrOffset = iYr-refYear
              intOK = (abs(yrOffset)<maxOffset)
              if (.not.found) then
                 n = n + 1
                 ftime = ftime + item%frequency
              end if
           End Do
           If (.not.found) Then
              if (mapl_am_I_root()) Then
                 write(*,'(a,a)') 'Could not find data within maximum offset range from ',trim(item%file)
                 write(*,'(a,2(x,I0.5))') 'Test year and reference year: ', iYr, refYear
                 call ESMF_TimeGet(item%reff_time,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                 Write(*,'(a,I0.4,5(a,I0.2))') ' >> >> >> Reference time: ',iYr,'-',iMM,'-',iDD,' ',iHr,':',iMn,':',iSc
                 call ESMF_TimeGet(fTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                 Write(*,'(a,I0.4,5(a,I0.2))') ' >> >> >> Last check    : ',iYr,'-',iMM,'-',iDD,' ',iHr,':',iMn,':',iSc
              end if
              RETURN_(ESMF_FAILURE)
           End If
           If (isAssigned) Then
              ! Do we still have the right file?
              If (Trim(xCFIO%fNAME) .ne. Trim(file_processed)) Then
                 isAssigned = .False.
                 If (allocated(xTSeries)) Deallocate(xTSeries)
                 Call ESMF_CFIODestroy(xCFIO,__RC__)
              End If
           End If
           If (.not.isAssigned) Then
              ! Generate CFIO
              call MakeCFIO(file_processed,xCFIO,found,rc=status)
              ! Retrieve the time series
              allocate(xTSeries(xCFIO%tSteps))
              call GetTimesOnFile(xCFIO,xTSeries,rc=rc)
              isAssigned = .True.
           End If
           ! Is this before or after our target time?
           LSide   = (bSide == "L")
           RSide   = (.not.LSide)
           LExact  = (cTime == xTSeries(1))
           LExtrap = (cTime <  xTSeries(1))
           RExact  = (cTime == xTSeries(xCFIO%tSteps))
           RExtrap = (cTime >  xTSeries(xCFIO%tSteps))
           found = .false.
           If (LExtrap.or.(LExact.and.RSide)) Then
              if (mapl_am_I_root().and.(Ext_Debug>2)) Then
                 write(*,'(a,a,a,a)') ' Extrapolating BACKWARD for bracket ', bSide, ' for file ',trim(item%file)
              end if
              ! We have data from future years
              ! Advance the target time until we can have what we want
              call ESMF_TimeGet(cTime,yy=cYearOff,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
              iYr = refYear + yrOffset
              ! Convert year offset to the future value
              yrOffset = iYr - cYearOff
              ! Determine the template time
              call ESMF_TimeSet(newTime,yy=iYr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
              ftime = item%reff_time
              n = 0
              do while (.not.found)
                 found = ((ftime + item%frequency) > newtime)
                 if (.not.found) then
                    n = n + 1
                    ftime = fTime+item%frequency
                 end if
              end do
              ! untemplate file
              call ESMF_TimeGet(fTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
              ! Build file name
              call MAPL_PackTime(curDate,iyr,imm,idd)
              call MAPL_PackTime(curTime,ihr,imn,isc)
              call gx_(file_processed,item%file,nymd=curDate,nhms=curTime,__STAT__)
              Inquire(FILE=trim(file_processed),EXIST=found)
              If (.not.found) Then
                 if (mapl_am_I_root()) Then
                    write(*,'(a,a,a,a)') ' ERROR: Failed to project data from ',trim(item%file),' for side ',bSide
                 end if
                 RETURN_(ESMF_FAILURE)
              End If
           ElseIf (RExtrap.or.(RExact.and.RSide)) Then
              if (mapl_am_I_root().and.(Ext_Debug>2)) Then
                 write(*,'(a,a,a,a)') ' Extrapolating FORWARD for bracket ', bSide, ' for file ',trim(item%file)
              end if
              ! We have data from past years
              ! Rewind the target time until we can have what we want
              call ESMF_TimeGet(cTime,yy=refYear,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
              yrOffset = 0
              fTime = cTime
              ! yrOffset: Number of years added from current time to get file time
              Do While ((.not.found).and.(abs(yrOffset).lt.maxOffset))
                 yrOffset = yrOffset - 1
                 iYr = refYear + yrOffset
                 call ESMF_TimeSet(newTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                 ! Error check - if the new time is before the first file time,
                 ! all is lost
                 If (newTime.lt.xTSeries(1)) exit
                 do while (ftime > newTime)
                    fTime = fTime - item%frequency
                    n = n - 1
                 end do 
                 ! untemplate file
                 call ESMF_TimeGet(fTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                 call MAPL_PackTime(curDate,iyr,imm,idd)
                 call MAPL_PackTime(curTime,ihr,imn,isc)
                 call gx_(file_processed,item%file,nymd=curDate,nhms=curTime,__STAT__)
                 Inquire(FILE=trim(file_processed),EXIST=found)
              End Do
              If (.not.found) Then
                 if (mapl_am_I_root()) Then
                    write(*,'(a,a,a,a)') ' ERROR: Could not determine upper bounds on ',trim(item%file),' for side ',bSide
                 end if
                 RETURN_(ESMF_FAILURE)
              End If
           Else
              if (mapl_am_I_root()) Then
                 write(*,'(a,a,a,a)') ' ERROR: Unkown error while scanning ',trim(item%file),' for side ',bSide
              end if
              RETURN_(ESMF_FAILURE)
           End If
        End If

        ! Should now have the "correct" time
        ! Generate CFIO for the "current" file
        If (MAPL_Am_I_Root().and.(Ext_Debug > 19)) Write(*,'(a,a)') ' DEBUG: Generating CFIO for ', trim(file_processed)
        If (isAssigned) Then
           ! Do we still have the right file?
           If (Trim(xCFIO%fNAME) .ne. Trim(file_processed)) Then
              isAssigned = .False.
              If (allocated(xTSeries)) Deallocate(xTSeries)
              Call ESMF_CFIODestroy(xCFIO,__RC__)
           End If
        End If
        If (.not.isAssigned) Then
           ! Generate CFIO
           call MakeCFIO(file_processed,xCFIO,found,rc=status)
           ! Retrieve the time series
           allocate(xTSeries(xCFIO%tSteps))
           call GetTimesOnFile(xCFIO,xTSeries,rc=rc)
           isAssigned = .True.
        End If

        ! We now have a time which, when passed to the FILE TEMPLATE, returns a valid file
        ! However, if the file template does not include a year token, then the file in
        ! question could actually be for a different year. We therefore feed the file time
        ! into the refresh template and see if the result has the same year. If it doesn't,
        ! then we can assume that the year is actually fixed, and the times in the file will
        ! correspond to the year in the refresh template. In this case, an additional year 
        ! offset must be applied.
        yrOffsetStamp = 0
        buff = trim(item%refresh_template)
        buff = ESMF_UtilStringLowerCase(buff, __RC__)
        If (buff /= "0" .and. index(buff,"p")==0) Then
           newTime = timestamp_(fTime,item%refresh_template,__RC__)
           if (newTime .ne. fTime) Then
              call ESMF_TimeGet(fTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
              call ESMF_TimeGet(newTime,yy=fyr,mm=fmm,dd=fdd,h=fhr,m=fmn,s=fsc,__RC__)
              yrOffsetStamp = fYr - iYr
           End If
        End If

        ! try to get bracketing time on file using current time
        call GetBracketTimeOnFile(xCFIO,xTSeries,readTime,bSide,UniFileClim,interpTime,fileTime,yrOffsetInt=yrOffset+yrOffsetStamp,rc=status)
        found = (status==ESMF_SUCCESS)

        If (MAPL_Am_I_Root().and.(Ext_Debug > 19)) Write(*,'(a,a,a,L1)') ' DEBUG: Status of ', trim(file_processed),': ', found

        ! if we didn't find the bracketing time look forwards or backwards depending on
        ! whether it is the right or left time   
        if (.not.found) then
           If (MAPL_Am_I_Root().and.(Ext_Debug > 19)) Write(*,'(a,a,a,a,a,L1)') ' DEBUG: Scanning for bracket ', bSide, ' of ', trim(file_processed), '. RSide: ', (bSide=="R")
           bracketScan = .True.
           newTime = fTime
           if (bSide == "R") then
              found=.false.
              newFile=allowExtrap
              status = ESMF_SUCCESS
              If (MAPL_Am_I_Root().and.(Ext_Debug > 19)) Write(*,'(a,a,a,I5,x,2L1)') ' DEBUG: Sanity check on file ', trim(file_processed), ' with flags: ', status, status==ESMF_SUCCESS,found
              do while ((status==ESMF_SUCCESS).and.(.not.found))
                 ! check next time
                 newTime = fTime + item%frequency
                 if (trim(item%cyclic)=='y') then
                    call ESMF_TimeGet(fTime,yy=OldYear)
                    call ESMF_TimeGet(newTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                    if (oldyear/=iyr) then
                       call ESMF_TimeSet(newTime,yy=oldyear,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                       yrOffset = yrOffset - 1
                       If (MAPL_Am_I_Root()) Write(*,'(a,I0.4,5(a,I0.2))') ' DEBUG: IN clim after ',Oldyear,'-',iMm,'-',iDd,' ',iHr,':',iMn,':',iSc
                    end if
                 end if
                 ! untemplate file
                 call ESMF_TimeGet(newTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)

                 call MAPL_PackTime(curDate,iyr,imm,idd)
                 call MAPL_PackTime(curTime,ihr,imn,isc)
                 call gx_(file_processed,item%file,nymd=curDate,nhms=curTime,__STAT__)
                 If (MAPL_Am_I_Root().and.(Ext_Debug > 19)) Write(*,'(a,a,a,I0.4,5(a,I0.2))') ' DEBUG: Testing for file ', trim(file_processed), ' for target time ',iYr,'-',iMm,'-',iDd,' ',iHr,':',iMn,':',iSc
                 Inquire(FILE=trim(file_processed),EXIST=found)
                 If (found) Then
                    fTime = newTime
                 Else If (newFile) Then
                    ! We went RIGHT - cycle round by SUBTRACTING a year
                    yrOffset = yrOffset - 1
                    newFile = .False. ! Only one attempt
                    call OffsetTimeYear(fTime,-1,newTime,rc)
                    fTime = newTime
                 Else
                    status = ESMF_FAILURE
                 End If
              End Do
              if (status /= ESMF_SUCCESS) then
                 if (mapl_am_I_root()) write(*,*)'ExtData could not find appropriate file from file template ',trim(item%file),' for side ',bSide
                 RETURN_(ESMF_FAILURE)
              end if
           else if (bSide == "L") then
              found=.false.
              newFile=allowExtrap
              status = ESMF_SUCCESS
              do while ((status==ESMF_SUCCESS).and.(.not.found))
                 ! check next time
                 newTime = fTime - item%frequency
                 if (trim(item%cyclic)=='y') then
                    call ESMF_TimeGet(fTime,yy=OldYear)
                    call ESMF_TimeGet(newTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                    if (oldyear/=iyr) then
                       call ESMF_TimeSet(newTime,yy=oldyear,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                       yrOffset = yrOffset + 1
                       If (MAPL_Am_I_Root()) Write(*,'(a,I0.4,5(a,I0.2))') ' DEBUG: IN clim after ',Oldyear,'-',iMm,'-',iDd,' ',iHr,':',iMn,':',iSc
                    end if
                 end if
                 ! untemplate file
                 call ESMF_TimeGet(newTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)

                 call MAPL_PackTime(curDate,iyr,imm,idd)
                 call MAPL_PackTime(curTime,ihr,imn,isc)
                 call gx_(file_processed,item%file,nymd=curDate,nhms=curTime,__STAT__)
                 Inquire(FILE=trim(file_processed),EXIST=found)
                 If (found) Then
                    fTime = newTime
                 Else If (newFile) Then
                    ! We went LEFT - cycle round by ADDING a year
                    yrOffset = yrOffset + 1
                    newFile = .False. ! Only one attempt
                    call OffsetTimeYear(fTime,+1,newTime,rc)
                    fTime = newTime
                 Else
                    status = ESMF_FAILURE
                 End If
              End Do
              if (status /= ESMF_SUCCESS) then
                 if (mapl_am_I_root()) write(*,*)'ExtData could not find appropriate file from file template ',trim(item%file),' for side ',bSide
                 RETURN_(ESMF_FAILURE)
              end if
           end if

           ! fTime is now ALWAYS the time which was applied to the file template to get the current file
           If (isAssigned) Then
              ! Do we still have the right file?
              If (Trim(xCFIO%fNAME) .ne. Trim(file_processed)) Then
                 isAssigned = .False.
                 If (allocated(xTSeries)) Deallocate(xTSeries)
                 Call ESMF_CFIODestroy(xCFIO,__RC__)
              End If
           End If
           If (.not.isAssigned) Then
              ! Generate CFIO
              call MakeCFIO(file_processed,xCFIO,found,rc=status)
              ! Retrieve the time series
              allocate(xTSeries(xCFIO%tSteps))
              call GetTimesOnFile(xCFIO,xTSeries,rc=rc)
              isAssigned = .True.
           End If

           !If (Mapl_Am_I_Root()) Write (*,'(a,a,x,a)') ' SUPERDEBUG: File/template: ',Trim(file_processed),Trim(item%refresh_template)
           ! The file template may be "hiding" a year offset from us
           yrOffsetStamp = 0
           buff = trim(item%refresh_template)
           buff = ESMF_UtilStringLowerCase(buff, __RC__)
           If (buff /= "0" .and. index(buff,"p")==0 ) Then
              newTime = timestamp_(fTime,item%refresh_template,__RC__)
              If (Mapl_Am_I_Root().and.Ext_Debug > 24) Then
                 call ESMF_TimeGet(fTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                 call ESMF_TimeGet(newTime,yy=fyr,mm=fmm,dd=fdd,h=fhr,m=fmn,s=fsc,__RC__)
                 Write(*,'(3a,I0.4,5(a,I0.2),a,I0.4,5(a,I0.2),2a)') ' DEBUG: Template ',Trim(item%refresh_template),' applied: ',&
                    iyr,'-',imm,'-',idd,' ',ihr,':',imn,':',isc,' -> ',&
                    fyr,'-',fmm,'-',fdd,' ',fhr,':',fmn,':',fsc,&
                    ' on file ',Trim(file_processed)
              End If
              if (newTime .ne. fTime) Then
                 call ESMF_TimeGet(fTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                 call ESMF_TimeGet(newTime,yy=fyr,mm=fmm,dd=fdd,h=fhr,m=fmn,s=fsc,__RC__)
                 yrOffsetStamp = fYr - iYr
                 If (Mapl_Am_I_Root().and.Ext_Debug > 19) Then
                    Write(*,'(2(a,I4),2a)') ' DEBUG: Year offset modified from ',yrOffset,' to ',yrOffset+yrOffsetStamp,' to satisfy refresh template for ', Trim(file_processed)
                 End If
              End If
           End If

           ! try to get bracketing time on file using new time
           call GetBracketTimeOnFile(xCFIO,xTSeries,readTime,bSide,UniFileClim,interpTime,fileTime,yrOffsetInt=yrOffset+yrOffsetStamp,rc=status)
           found = (status == ESMF_SUCCESS)
           if (.not.found) then
              if (mapl_am_I_root()) write(*,*)'ExtData could not find bracketing data from file template ',trim(item%file),' for side ',bSide
              RETURN_(ESMF_FAILURE)

           end if

        end if

     end if

     ! Debug
     If (Mapl_Am_I_Root().and.Ext_Debug > 10) Then 
        Write(*,'(a,a,a,a)') '  >> >> Updated bracket ', bSide, ' for ', Trim(file_processed)
        call ESMF_TimeGet(cTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
        Write(*,'(a,I0.4,5(a,I0.2))') ' >> >> >> Time requested: ',iYr,'-',iMM,'-',iDD,' ',iHr,':',iMn,':',iSc
        call ESMF_TimeGet(fileTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
        Write(*,'(a,I0.4,5(a,I0.2))') ' >> >> >> Record time   : ',iYr,'-',iMM,'-',iDD,' ',iHr,':',iMn,':',iSc
        call ESMF_TimeGet(interpTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
        Write(*,'(a,I0.4,5(a,I0.2))') ' >> >> >> Effective time: ',iYr,'-',iMM,'-',iDD,' ',iHr,':',iMn,':',iSc
     End If
     ! If we made it this far, then I guess we are OK?
     RETURN_(ESMF_SUCCESS)
    
  end subroutine UpdateBracketTime

  subroutine MakeCFIO(file,cfio,found,rc)
     character(len=ESMF_MAXSTR), intent(in   ) :: file
     type(ESMF_CFIO)                           :: cfio
     logical                                   :: found
     integer, optional,          intent(out  ) :: rc

     __Iam__('MakeCFIO')

     Inquire(FILE=trim(file),EXIST=found)
     If (.not.found) Then
        Write(6,'(a,a)') 'WARNING: File not found while getting times: ', Trim(file)
        RC = ESMF_FAILURE
        return
     Else If (Mapl_Am_I_Root().and.(Ext_Debug > 9)) Then
        Write(6,'(a,a)') ' DEBUG: Opening file: ', Trim(file)
     End If
     cfio =  ESMF_CFIOCreate (cfioObjName='cfio_obj',__RC__)
     call ESMF_CFIOSet(CFIO, fName=trim(file),__RC__)
     call ESMF_CFIOFileOpen  (CFIO, FMODE=1, __RC__)
     rc = ESMF_SUCCESS
     return
  end subroutine

  subroutine GetTimesOnFile(cfio,tSeries,rc)
     type(ESMF_CFIO)                           :: cfio
     type(ESMF_Time)                           :: tSeries(:)
     integer, optional,          intent(out  ) :: rc

     __Iam__('GetTimesOnFile')

     integer(ESMF_KIND_I4)              :: iyr,imm,idd,ihr,imn,isc
     integer                            :: i
     integer(ESMF_KIND_I8)              :: iCurrInterval
     integer                            :: nhmsB, nymdB
     integer                            :: begDate, begTime
     integer(ESMF_KIND_I8),allocatable  :: tSeriesInt(:)

     allocate(tSeriesInt(cfio%tSteps))
     call getDateTimeVec(cfio%fid,begDate,begTime,tSeriesInt,__RC__)
     
     ! Assume success
     rc=ESMF_SUCCESS

     ! Debug level 3
     If (Mapl_Am_I_Root().and.(Ext_Debug > 2)) Then
        Write(*,'(a,a)') '  >> >> Reading times from ', Trim(cfio%fName)
        Write(*,'(a,2(x,I0.10),x,I0.4)') '  >> >> File timing info:', begDate, begTime, cfio%tSteps
     End If

     do i=1,cfio%tSteps
        iCurrInterval = tSeriesInt(i)
        call GetDate ( begDate, begTime, iCurrInterval, nymdB, nhmsB, status )
        call MAPL_UnpackTime(nymdB,iyr,imm,idd)
        call MAPL_UnpackTime(nhmsB,ihr,imn,isc)
        ! Debug level 4
        If (Mapl_Am_I_Root()) Then
           If ((Ext_Debug > 4).or.((Ext_Debug > 3).and.((i.eq.1).or.(i.eq.cfio%tSteps)))) Then
              Write(*,'(a,I0.6,a,I0.4,5(a,I0.2))') ' >> >> STD Sample ',i,':  ',iYr,'-',iMM,'-',iDD,' ',iHr,':',iMn,':',iSc
           End If
        End If
        call ESMF_TimeSet(tSeries(i), yy=iyr, mm=imm, dd=idd,  h=ihr,  m=imn, s=isc,__RC__)
     enddo

     deallocate(tSeriesInt)
     return

  end subroutine GetTimesOnFile

  subroutine OffsetTimeYear(inTime,yrOffset,outTime,rc)
     type(ESMF_Time),            intent(in   ) :: inTime
     integer                                   :: yrOffset
     type(ESMF_Time),            intent(out  ) :: outTime
     integer, optional,          intent(out  ) :: rc

     __Iam__('OffsetTimeYear')

     integer(ESMF_KIND_I4)              :: iyr,imm,idd,ihr,imn,isc
     logical                            :: srcLeap, targLeap

     call ESMF_TimeGet(inTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
     ! If the source year is a leap year but the new one isn't, modify to day 28
     iYr = iYr + yrOffset
     targLeap = ((imm.eq.2).and.(idd.eq.29))
     if (targLeap) then
        if (iyr.lt.1582) then
           srcLeap = .False.
        else if (modulo(iYr,4).ne.0) then
           srcLeap = .False.
        else if (modulo(iYr,100).ne.0) then
           srcLeap = .True.
        else if (modulo(iYr,400).ne.0) then
           srcLeap = .False.
        else
           srcLeap = .True.
        end if
     else
        srcLeap = .True.
     end if
     if (targLeap.and.(.not.srcLeap)) idd=28
     call ESMF_TimeSet(outTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)

     rc=ESMF_SUCCESS
     return

  end subroutine OffsetTimeYear

  subroutine GetBracketTimeOnSingleFile(cfio,tSeries,cTime,bSide,UniFileClim,interpTime,fileTime,allowExtrap,climyear,rc)
     type(ESMF_CFIO),                     intent(in   ) :: cfio
     type(ESMF_Time),                     intent(in   ) :: tSeries(:)
     type(ESMF_Time),                     intent(inout) :: cTime
     character(len=1),                    intent(in   ) :: bSide
     logical,                             intent(in   ) :: UniFileClim
     type(ESMF_TIME),                     intent(inout) :: interpTime
     type(ESMF_TIME),                     intent(inout) :: fileTime
     logical,                             intent(in   ) :: allowExtrap
     integer,                             intent(in   ) :: climYear
     integer, optional,                   intent(out  ) :: rc

     __Iam__('GetBracketTimeOnSingleFile')

     integer(ESMF_KIND_I4)              :: iyr,imm,idd,ihr,imn,isc
     integer                            :: i
     type(ESMF_Time)                    :: climTime
     logical                            :: found
     logical                            :: LExtrap,RExtrap,LExact,RExact
     logical                            :: LSide, RSide
     integer                            :: yrOffset, yrOffsetNeg, targYear
     integer                            :: iEntry
     type(ESMF_Time), pointer           :: tSeriesC(:)
     logical                            :: foundYear
     integer                            :: tSteps, curYear

     ! Store the target time which was actually requested
     call ESMF_TimeGet(cTime,yy=targYear,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)

     ! Debug output
     If (Mapl_Am_I_Root().and.(Ext_Debug > 15)) Then
        Write(6,'(a,a)') ' DEBUG: GetBracketTimeOnSingleFile called for ', trim(cfio%fName)
     End If

     ! Debug level 3
     If (Mapl_Am_I_Root().and.(Ext_Debug > 2) ) Then
        Write(*,'(a,L1,a,a)') '  >> >> Reading times from fixed (',UniFileClim,') file ', Trim(cfio%fName)
        call ESMF_TimeGet(tSeries(1),yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
        Write(*,'(a,I0.4,5(a,I0.2))') ' >> >> >> File start    : ',iYr,'-',iMM,'-',iDD,' ',iHr,':',iMn,':',iSc
        call ESMF_TimeGet(tSeries(cfio%tSteps),yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
        Write(*,'(a,I0.4,5(a,I0.2))') ' >> >> >> File end      : ',iYr,'-',iMM,'-',iDD,' ',iHr,':',iMn,':',iSc
        call ESMF_TimeGet(cTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
        Write(*,'(a,I0.4,5(a,I0.2))') ' >> >> >> Time requested: ',iYr,'-',iMM,'-',iDD,' ',iHr,':',iMn,':',iSc
     End If

     if (uniFileClim) then

        call ESMF_TimeGet(cTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
        iyr = climYear
        if (idd == 29 .and. imm == 2) idd = 28
        call ESMF_TimeSet(climTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)

        tsteps=0
        foundYear = .false.
        do i=1,cfio%tSteps

           call ESMF_TimeGet(tseries(i),yy=iyr,__RC__)
           if (iyr==climYear) then
              if (foundYear .eqv. .false.) then
                 iEntry = i
                 foundYear = .true.
              end if
              tsteps=tsteps+1
           end if

        end do
    
        allocate(tSeriesC(tsteps),__STAT__)
        do i=1,tsteps
           tSeriesC(i)=tSeries(iEntry+i-1)
        enddo

        found = .false.
        if (bSide == "L") then
           if ( (climTime < tSeriesC(1)) ) then
              fileTime = tSeriesC(tSteps)
              call ESMF_TimeGet(tSeriesC(tSteps),yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
              call ESMF_TimeGet(cTime,yy=curYear,__RC__)
              iyr = curYear - 1
              call ESMF_TimeSet(interpTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
              found = .true.
           else
              do i=tSteps,1,-1
                 if (climTime >= tSeriesC(i)) then
                    fileTime = tSeriesC(i)
                    if (UniFileClim) then
                       call ESMF_TimeGet(cTime,yy=curYear,__RC__)
                       call ESMF_TimeGet(fileTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                       call ESMF_TimeSet(interpTime,yy=curYear,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                    else
                       interpTime = tSeriesC(i)
                    end if
                    found = .true.
                    exit
                 end if
              end do
           end if
        else if (bSide == "R") then
           if ( (climTime >= tSeriesC(tSteps)) ) then
              fileTime = tSeriesC(1)
              call ESMF_TimeGet(tSeriesC(1),yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
              call ESMF_TimeGet(cTime,yy=curYear,__RC__)
              iyr = curYear + 1
              call ESMF_TimeSet(interpTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
              found = .true.
           else
              do i=1,tSteps
                 if (climTime < tSeriesC(i)) then
                    fileTime = tSeriesC(i)
                    if (UniFileClim) then
                       call ESMF_TimeGet(cTime,yy=curYear,__RC__)
                       call ESMF_TimeGet(fileTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                       call ESMF_TimeSet(interpTime,yy=curYear,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
                    else
                       interpTime = tSeriesC(i)
                    end if
                    found = .true.
                    exit
                 end if
              end do
           end if
        end if 

     else

        yrOffset = 0
        climTime = cTime

        ! Is the requested time within the range of values offered?
        LSide = (bSide == "L")
        RSide = (.not.LSide)
        LExact  = (cLimTime == tSeries(1))
        RExact  = (cLimTime == tSeries(cfio%tSteps))
        LExtrap = (cLimTime <  tSeries(1)) 
        RExtrap = (cLimTime >  tSeries(cfio%tSteps))
        found = .false.
        If (Mapl_Am_I_Root().and.(Ext_Debug > 19)) Then
           Write(*,'(a,4(L1,x),a,a)') ' DEBUG: Extrapolation flags (0) are ',LExact,RExact,LExtrap,RExtrap,'for file ', trim(cfio%fName)
        End If

        if (allowExtrap) then
           If (LExtrap) Then
              If (Mapl_Am_I_Root().and.(Ext_Debug > 19)) Then
                 Write(6,'(a,a)') ' DEBUG: Requested time is before first available sample in file ', trim(cfio%fName)
              End If
              ! Increase the target time until it is within range
              Do While (LExtrap)
                 yrOffset = yrOffset + 1
                 iYr = targYear + yrOffset
                 call OffsetTimeYear(cTime,yrOffset,cLimTime,rc)
                 If (LSide) Then
                    LExtrap = (cLimTime <  tSeries(1))
                 Else
                    LExtrap = (cLimTime <= tSeries(1))
                    ! When scanning for the right side, if we find that we
                    ! have an exact match to the first entry, then as long
                    ! as there is a second entry, we have the correct offset
                    If (LExtrap.and.(cfio%tSteps > 1)) Then
                       LExact  = (cLimTime == tSeries(1))
                       LExtrap = (LExtrap.and.(.not.LExact))
                    End If
                 End If
              End Do
           Else If (RExtrap.or.(RExact.and.RSide)) Then
              If (Mapl_Am_I_Root().and.(Ext_Debug > 19)) Then
                 Write(6,'(a,a)') ' DEBUG: Requested time is after or on last available sample in file ', trim(cfio%fName)
              End If
              Do While (RExtrap.or.(RExact.and.RSide))
                 yrOffset = yrOffset - 1
                 iYr = targYear + yrOffset
                 call OffsetTimeYear(cTime,yrOffset,cLimTime,rc)
                 If (LSide) Then
                    RExtrap = (cLimTime >  tSeries(cfio%tSteps))
                 Else
                    RExtrap = (cLimTime >= tSeries(cfio%tSteps))
                    RExact  = (cLimTime == tSeries(cfio%tSteps))
                 End If
              End Do
           End If

           ! Retest for an exact match - note this is only useful if we want bracket L
           LExact  =  (cLimTime == tSeries(1))
           RExact  =  (cLimTime == tSeries(cfio%tSteps))
           If (Mapl_Am_I_Root().and.(Ext_Debug > 19)) Then
              Write(*,'(a,4(L1,x),a,a)') ' DEBUG: Extrapolation flags (2) are ',LExact,RExact,LExtrap,RExtrap,'for file ', trim(cfio%fName)
           End If

        End IF

        If (LSide.and.LExact) Then
           found = .true.
           iEntry = 1
        Else If (LSide.and.RExact) Then
           found = .true.
           iEntry = cfio%tSteps
        Else
           if (bSide == "L") then
              do i=cfio%tSteps,1,-1
                 if (climTime >= tSeries(i)) then
                    iEntry = i
                    found = .true.
                    exit
                 end if
              end do
           else if (bSide == "R") then
              do i=1,cfio%tSteps
                 if (climTime < tSeries(i)) then
                    iEntry = i
                    found = .true.
                    exit
                 end if
              end do
           end if
        end if

        if (found) then
           fileTime = tSeries(iEntry)
           if (yrOffset == 0) Then
              interpTime = fileTime
           Else
              yrOffsetNeg = -1*yrOffset
              call OffsetTimeYear(fileTime,yrOffsetNeg,interpTime,rc)
           End If
        end if

     end if

     if (found) then
        If (Mapl_Am_I_Root().and.(Ext_Debug > 15)) Then
           call ESMF_TimeGet(fileTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
           Write(*,'(a,I0.4,a,I0.2,a,I0.2,a,I0.2,a,I0.2,a,a,a,a)') &
              ' DEBUG: Data from time ', iYr, '-', iMm, '-', iDd, &
              ' ', iHr, ':', iMn, ' set for bracket ', bSide,&
              ' of file ', Trim(cfio%fName)
           Write(*,'(a,I5)') ' DEBUG: ==>> Year offset: ', yrOffset
           If (yrOffset .ne. 0) Then
              call ESMF_TimeGet(interpTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
              Write(*,'(a,I0.4,a,I0.2,a,I0.2,a,I0.2,a,I0.2)') &
                 ' DEBUG: ==> Mapped to: ', iYr, '-', iMm, '-', iDd, &
                 ' ', iHr, ':', iMn
              call ESMF_TimeGet(interpTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
              Write(*,'(a,I0.4,a,I0.2,a,I0.2,a,I0.2,a,I0.2)') &
                 ' DEBUG: ==> Target to: ', iYr, '-', iMm, '-', iDd, &
                 ' ', iHr, ':', iMn
           End If
        End If
        rc=ESMF_SUCCESS
        return
     else
        Write(6,'(a,a)') 'WARNING: Requested sample not found in file ', trim(cfio%fName)
        rc=ESMF_FAILURE
        return
     endif

  end subroutine GetBracketTimeOnSingleFile

  subroutine GetBracketTimeOnFile(cfio,tSeries,cTime,bSide,UniFileClim,interpTime,fileTime,yrOffsetInt,rc)
     type(ESMF_CFIO),                     intent(in   ) :: cfio
     type(ESMF_Time),                     intent(in   ) :: tSeries(:)
     type(ESMF_Time),                     intent(inout) :: cTime
     character(len=1),                    intent(in   ) :: bSide
     logical,                             intent(in   ) :: UniFileClim
     type(ESMF_TIME),                     intent(inout) :: interpTime
     type(ESMF_TIME),                     intent(inout) :: fileTime
     integer, optional,                   intent(in   ) :: yrOffsetInt
     integer, optional,                   intent(out  ) :: rc

     __Iam__('GetBracketTimeOnFile')

     integer(ESMF_KIND_I4)              :: iyr,imm,idd,ihr,imn,isc
     integer                            :: i
     type(ESMF_Time)                    :: climTime
     logical                            :: found, outOfBounds
     integer                            :: yrOffset, yrOffsetNeg
     integer                            :: climSize

     ! Assume that the requested time is within range
     If (Present(yrOffsetInt)) Then
        yrOffset = yrOffsetInt
     Else
        yrOffset = 0
     End If

     if (UniFileClim) then
        If (MAPL_Am_I_Root()) Write(*,'(a)') 'GetBracketTimeOnFile called with UniFileClim'
        RC = ESMF_FAILURE
        Return
     end if

     ! Debug output
     If (Mapl_Am_I_Root().and.(Ext_Debug > 15)) Then
        Write(6,'(4a)') ' DEBUG: GetBracketTimeOnFile (',Trim(bSide),') called for ', trim(cfio%fName)
     End If

     if (yrOffset.ne.0) then
        ! If the source year is a leap year but this isn't, modify to day 28
        call OffsetTimeYear(cTime,yrOffset,cLimTime,rc)
     else
        climTime = cTime
     end if   
     climSize = 1

     ! Debug output
     If (Mapl_Am_I_Root().and.(Ext_Debug > 19)) Then
        call ESMF_TimeGet(cLimTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
        Write(6,'(a,I2,3a,I0.4,5(a,I0.2))') ' DEBUG: Year offset of ',yrOffset,&
           ' applied while scanning ', trim(cfio%fName),&
           ' to give target time ',iYr,'-',iMm,'-',iDd,' ',iHr,':',iMn,':',iSc
     End If

     found = .false.
     ! we will have to specially handle a climatology in one file
     ! might be better way but can not think of one
     if (bSide == "L") then
        OutOfBounds = (cLimTime < tSeries(1))
        If (OutOfBounds) Then
           ! This can be an acceptable outcome - no printout
           RC = ESMF_FAILURE
           Return
        End If
        do i=cfio%tSteps,1,-1
           if (climTime >= tSeries(i)) then
              fileTime = tSeries(i)
              if (yrOffset .ne. 0) Then
                 yrOffsetNeg = -1*yrOffset
                 Call OffsetTimeYear(fileTime,yrOffsetNeg,interpTime,rc)
              else
                 interpTime = fileTime
              end if
              found = .true.
              exit
           end if
        end do
     else if (bSide == "R") then
        ! Is the requested time within the range of values offered?
        OutOfBounds = (cLimTime >= tSeries(cfio%tSteps))
        If (OutOfBounds) Then
           ! This can be an acceptable outcome - no printout
           RC = ESMF_FAILURE
           Return
        End If
        do i=1,cfio%tSteps
           if (climTime < tSeries(i)) then
              fileTime = tSeries(i)
              if (yrOffset .ne. 0) Then
                 yrOffsetNeg = -1*yrOffset
                 Call OffsetTimeYear(fileTime,yrOffsetNeg,interpTime,rc)
              else
                 interpTime = fileTime
              end if
              found = .true.
              exit
           end if
        end do
     end if

     if (found) then
        If (Mapl_Am_I_Root().and.(Ext_Debug > 15)) Then 
           call ESMF_TimeGet(fileTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
           Write(*,'(a,I0.4,a,I0.2,a,I0.2,a,I0.2,a,I0.2,a,a,a,a)') &
              ' DEBUG: Data from time ', iYr, '-', iMm, '-', iDd, &
              ' ', iHr, ':', iMn, ' set for bracket ', bSide,&
              ' of file ', Trim(cfio%fName)
           If (yrOffset .ne. 0) Then
              call ESMF_TimeGet(interpTime,yy=iyr,mm=imm,dd=idd,h=ihr,m=imn,s=isc,__RC__)
              !Write(*,'(a,I0.4,a,I0.2,a,I0.2,a,I0.2,a,I0.2)') &
                 !' DEBUG: ==> Mapped to: ', iYr, '-', iMm, '-', iDd, &
                 !' ', iHr, ':', iMn
              Write(*,'(a,I0.4,a,I0.2,a,I0.2,a,I0.2,a,I0.2,a,I0.2)') &
                 ' DEBUG: ==> Mapped to: ', iYr, '-', iMm, '-', iDd, &
                 ' ', iHr, ':', iMn, ' Offset:', yrOffset
           End If
        End If
        rc=ESMF_SUCCESS
        return
     else
        Write(6,'(a,a)') 'WARNING: Requested sample not found in file ', trim(cfio%fName)
        rc=ESMF_FAILURE
        return
     endif
     !end if 

  end subroutine GetBracketTimeOnFile

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

     integer :: yr,mm,dd,hr,mn,sc,nhms1,nymd1,nhms2,nymd2

     Iam = "MAPL_ExtDataInterpField"
     tinv1 = time - item%interp_time1
     tinv2 = item%interp_time2 - item%interp_time1
     alpha = tinv1/tinv2
     call ESMF_FieldGet(FIELD, dimCount=fieldRank,name=name,__RC__)
     If (Mapl_Am_I_Root()) Then
        If (Ext_Debug > 9) Then
           call ESMF_TimeGet(item%interp_time1,yy=yr,mm=mm,dd=dd,h=hr,m=mn,s=sc,__RC__)
           call MAPL_PackTime(nhms1,hr,mn,sc)
           call MAPL_PackTime(nymd1,yr,mm,dd)
           If (item%doInterpolate) Then
              If (alpha .gt. 0.0) Then
                 call ESMF_TimeGet(item%interp_time2,yy=yr,mm=mm,dd=dd,h=hr,m=mn,s=sc,__RC__)
                 call MAPL_PackTime(nhms2,hr,mn,sc)
                 call MAPL_PackTime(nymd2,yr,mm,dd)
              Else
                 nhms2=0
                 nymd2=0
              End If
           Else
              nhms2=0
              nymd2=0
           End If

           If (.not.(item%doInterpolate)) Then
              Write(*,'(a,a,a,a,I0.8,x,I0.6)') ' >> >> >> ', &
                'Uninterpolated field ', Trim(item%name), &
                ' set to sample L: ', nymd1, nhms1
           Else If (time == item%interp_time1) Then
              Write(*,'(a,a,a,a,I0.8,x,I0.6)') ' >> >> >> ', &
                '  Interpolated field ', Trim(item%name), &
                ' set to sample L: ', nymd1, nhms1
           Else If (time == item%interp_time2) Then
              Write(*,'(a,a,a,a,I0.8,x,I0.6)') ' >> >> >> ', &
                '  Interpolated field ', Trim(item%name), &
                ' set to sample R: ', nymd2, nhms2
           Else
              Write(*,'(a,a,a,a,2(I0.8,x,I0.6,a),F10.6,a)') ' >> >> >> ', &
                '  Interpolated field ', Trim(item%name), &
                ' between ', nymd1,nhms1,' and ',nymd2,nhms2,' (', &
                alpha,' fraction)'
           End If
        End If
     End If
     call ESMF_FieldGet(FIELD, dimCount=fieldRank,name=name, __RC__)
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
  subroutine  MAPL_ExtDataReadVector(item,file,ExtState,side,rc )
     use MAPL_AbstractRegridderMod
     use MAPL_RegridderManagerMod
     use MAPL_LatLonGridFactoryMod
     use MAPL_GridManagerMod
     type(PrimaryExport),              intent(INOUT) :: item
     character(len=*),                 intent(IN   ) :: file
     type(MAPL_ExtData_State),         intent(INOUT) :: ExtState
     integer,                          intent(IN   ) :: side

     integer, optional, intent(out  ) :: rc

     integer                    :: status
     character(len=ESMF_MAXSTR) :: Iam

     integer :: NX, NY
     type(ESMF_Time)  :: time
     type(ESMF_Field) :: v1out, v2out
     character(len=ESMF_MAXSTR) :: fname1, fname2
     logical :: ignoreCase
     integer :: fid
     integer :: im_world, jm_world, lmIn, nt, nvars, natts, vdir
     integer :: imIn, jmIn, imOut, jmOut, lmOut
     type(ESMF_Grid) :: gridIn, gridOut, newGrid
     class (AbstractRegridder), pointer :: regridder

     real, pointer :: v1out_3d(:,:,:) => null()
     real, pointer :: v2out_3d(:,:,:) => null()
     real, pointer :: v1out_2d(:,:) => null()
     real, pointer :: v2out_2d(:,:) => null()

     real, pointer :: v1in_3d(:,:,:) => null()
     real, pointer :: v2in_3d(:,:,:) => null()
     real, pointer :: v1in_2d(:,:) => null()
     real, pointer :: v2in_2d(:,:) => null()
     real, pointer :: ptr1_3d(:,:,:) => null()
     real, pointer :: ptr2_3d(:,:,:) => null()

     type(ESMF_Field) :: field,newField1,newField2, psF

     integer           :: gridStagger1,gridStagger2
     integer           :: gridRotation1,gridRotation2
     logical           :: doRotation

     integer           :: fieldRank 
     integer           :: DIMS(ESMF_MAXGRIDDIM)
     type (LatLonGridFactory) :: factory

     Iam = "MAPL_ExtDataReadVector"

     fname1 = item%fcomp1
     fname2 = item%fcomp2
     call ESMF_ConfigGetAttribute(ExtState%cf,value=NX,Label="NX:",__RC__)
     call ESMF_ConfigGetAttribute(ExtState%cf,value=NY,Label="NY:",__RC__)
     ignorecase=ExtState%ignoreCase

     ! query the file to get the size of the lat-lon grid on the file
     call CFIO_Open(file,1,fid,status)
     VERIFY_(STATUS)
     call CFIO_DimInquire(fid,im_world,jm_world,lmIn,nt,nvars,natts,vdir,status)
     VERIFY_(STATUS)
     call CFIO_Close(fid,status)
     VERIFY_(STATUS)

     if (item%do_vertInterp .or. item%do_Fill) then
        call MAPL_ExtDataGetBracket(item,side,field=NewField1,vcomp=1,__RC__)
        call MAPL_ExtDataGetBracket(item,side,field=NewField2,vcomp=2,__RC__)
        call ESMF_FieldGet(NewField1,grid=gridOut,__RC__)
        newGrid = MAPL_ExtDataGridChangeLev(gridOut,ExtState%cf,item%lm,__RC__)
        call MAPL_GridGet(gridOut, localCellCountPerDim=dims,__RC__)
        imOut = dims(1)
        jmOut = dims(2)
        allocate(ptr1_3d(imOut,jmOut,lmIn),__STAT__)
        allocate(ptr2_3d(imOut,jmOut,lmIn),__STAT__)
        v1out = ESMF_FieldCreate(grid=newGrid, &
                dataCopyFlag = ESMF_DATACOPY_REFERENCE, &
                farrayPtr = ptr1_3d, __RC__) 
        v2out = ESMF_FieldCreate(grid=newGrid, &
                dataCopyFlag = ESMF_DATACOPY_REFERENCE, &
                farrayPtr = ptr2_3d, __RC__)
        call MAPL_FieldCopyAttributes(NewField1,v1out,__RC__)
        call MAPL_FieldCopyAttributes(NewField2,v2out,__RC__)
     else
        call MAPL_ExtDataGetBracket(item,side,field=v1out,vcomp=1,__RC__)
        call MAPL_ExtDataGetBracket(item,side,field=v2out,vcomp=2,__RC__)
     end if

     if (side == MAPL_ExtDataLeft) then
        time  = item%time1
     else if (side == MAPL_ExtDataRight) then
        time  = item%time2
     end if

     call MAPL_ExtDataGetBracket(item,side,field=Field,vcomp=1,__RC__)
     call ESMF_AttributeGet(field, NAME='STAGGERING', value=gridStagger1, __RC__)
     call ESMF_AttributeGet(field, NAME='ROTATION',value=gridRotation1,  __RC__)
     call MAPL_ExtDataGetBracket(item,side,field=Field,vcomp=2,__RC__)
     call ESMF_AttributeGet(field, NAME='STAGGERING', value=gridStagger2, __RC__)
     call ESMF_AttributeGet(field, NAME='ROTATION',value=gridRotation2, __RC__)

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
          
     factory = LatLonGridFactory(im_world=IM_WORLD, jm_world=jm_world, &
          & nx=nx, ny=ny, lm=lmIn, pole='PC',dateline='DC',__RC__)
     gridIn = grid_manager%make_grid(factory,__RC__)

     call MAPL_GridGet(gridIn, localCellCountPerDim=dims,__RC__)
     imIn = dims(1)
     jmIn = dims(2)

     call ESMF_FieldGet(v1out,grid=gridOut,dimCount=fieldRank,__RC__)
     call MAPL_GridGet(gridOut, localCellCountPerDim=dims,__RC__)
     imOut = dims(1)
     jmOut = dims(2)


!!$     call MAPL_HorzTransformCreate(trans,gridIn,gridOut,__RC__)
     if (doRotation) then
        regridder => regridder_manager%make_regridder(gridIn, gridOut, REGRID_METHOD_BILINEAR_ROTATE,__RC__)
     else
        regridder => regridder_manager%make_regridder(gridIn, gridOut, REGRID_METHOD_BILINEAR,__RC__)
     end if

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
         
!!$         call MAPL_HorzTransformRun (trans, v1in_3d, v2in_3d, v2out_3d, v2out_3d, rotate=doRotation, __RC__)
         call regridder%regrid(v1in_3d, v2in_3d, v1out_3d, v2out_3d)

         if (gridStagger1 == MAPL_CGrid) then
             call A2D2C(gridOut,v1out_3d,v2out_3d,1,getC=.true.)
         else if (gridStagger1 == MAPL_DGrid) then
             call A2D2C(gridOut,v1out_3d,v2out_3d,1,getC=.false.)
         end if

         v1out_2d = v1out_3d(:,:,1)
         v2out_2d = v2out_3d(:,:,1)

         deallocate(v1in_3d,v2in_3d,v1in_2d,v2in_2d,v1out_3d,v2out_3d,__STAT__)
     else if (fieldRank == 3) then
        
         call ESMF_FieldGet(v1out,localDe=0,farrayPtr=v1out_3d,__RC__)
         call ESMF_FieldGet(v2out,localDe=0,farrayPtr=v2out_3d,__RC__)
         
         allocate(v1in_3d(imIn,jmIn,lmIn),__STAT__)
         allocate(v2in_3d(imIn,jmIn,lmIn),__STAT__)

         call MAPL_CFIORead(fname1, file, time, gridIn, v1in_3d,ignoreCase=ignoreCase,__RC__)
         call MAPL_CFIORead(fname2, file, time, gridIn, v2in_3d,ignoreCase=ignoreCase,__RC__)
         
!!$         call MAPL_HorzTransformRun (trans, v1in_3d, v2in_3d, v2out_3d, v2out_3d, rotate=doRotation, __RC__)
         call regridder%regrid(v1in_3d, v2in_3d, v1out_3d, v2out_3d)

         if (item%do_vertInterp) then
            call MAPL_ExtDataGetBracket(item,side,field=newField1,vcomp=1,__RC__)
            call MAPL_ExtDataGetBracket(item,side,field=newField2,vcomp=2,__RC__)
            nt = ExtState%PrimaryOrder(1)
            call MAPL_ExtDataGetBracket(ExtState%primary%item(nt),side,field=psF,__RC__)
           
            call vertInterpolation_pressKappa(newfield1,v1out,psF,item%levs,MAPL_UNDEF,__RC__)
            call vertInterpolation_pressKappa(newfield2,v2out,psF,item%levs,MAPL_UNDEF,__RC__)
            call ESMF_FieldGet(newField1,localDe=0,farrayPtr=v1out_3d,__RC__)
            call ESMF_FieldGet(newField2,localDe=0,farrayPtr=v2out_3d,__RC__)
            deallocate(ptr1_3d,ptr2_3d)
         else if (item%do_Fill) then
            call MAPL_ExtDataGetBracket(item,side,field=newField1,vcomp=1,__RC__)
            call MAPL_ExtDataGetBracket(item,side,field=newField2,vcomp=2,__RC__)
            call MAPL_ExtDataFillField(newfield1,v1out,__RC__)
            call MAPL_ExtDataFillField(newfield2,v2out,__RC__)
            call ESMF_FieldGet(newField1,localDe=0,farrayPtr=v1out_3d,__RC__)
            call ESMF_FieldGet(newField2,localDe=0,farrayPtr=v2out_3d,__RC__)
            deallocate(ptr1_3d,ptr2_3d)
         end if

         lmout = size(v1out_3d,3)
         if (gridStagger1 == MAPL_CGrid) then
            call A2D2C(gridOut,v1out_3d,v2out_3d,lmOut,getC=.true.)
         else if (gridStagger1 == MAPL_DGrid) then
            call A2D2C(gridOut,v1out_3d,v2out_3d,lmOut,getC=.false.)
         end if

         deallocate(v1in_3d,v2in_3d,__STAT__)

     end if

     RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_ExtDataReadVector


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
     functionname = ESMF_UtilStringLowerCase(functionname, __RC__)
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
    functionname = ESMF_UtilStringLowerCase(functionname, __RC__)

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

 INTEGER :: base,count,i,iDash,last,lenStr
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

  subroutine AdvanceAndCount(CF,nLines,rc)

     type(ESMF_Config), intent(inout) :: cf
     integer, intent(out)             :: nLines
     integer, optional, intent(out)   :: rc

     integer :: iCnt
     logical :: inBlock
     character(len=ESMF_MAXPATHLEN) :: thisLine
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

  subroutine CheckUpdate(doUpdate,updateTime,currTime,hasRun,primaryItem,derivedItem,rc) 
     logical,                       intent(out  ) :: doUpdate
     type(ESMF_Time),               intent(inout) :: updateTime
     type(ESMF_Time),               intent(inout) :: currTime
     logical        ,               intent(in   ) :: hasRun
     type(PrimaryExport), optional, intent(inout) :: primaryItem
     type(DerivedExport), optional, intent(inout) :: derivedItem
     integer,             optional, intent(out  ) :: rc

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
              else
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
     end if
     
     RETURN_(ESMF_SUCCESS)
  end subroutine CheckUpdate

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

  ! This is a routine to make the appropriate call to MAPL_CFIO so we do not need to
  ! duplicate the calls with lots of if/else for voting, fractional etc ...
  subroutine MAPL_ExtDataGridCompRead(item,file,ExtState,side,rc)
     type(PrimaryExport),              intent(INOUT) :: item
     character(len=*),                 intent(IN   ) :: file
     type(MAPL_ExtData_State),         intent(INOUT) :: ExtState
     integer,                          intent(IN   ) :: side
     integer,                optional, intent(  OUT) :: rc

     character(len = ESMF_MAXSTR) :: Iam
     integer                      :: Status

     type(ESMF_Field)             :: field
     type(ESMF_FieldBundle)       :: bundle
     type(ESMF_Time)              :: time
     character(len=ESMF_MAXSTR)   :: name
     integer                      :: trans
     integer                      :: val
     integer                      :: id_ps
     logical                      :: ignoreCase
     type(ESMF_Grid)              :: newGrid, Grid
     type(ESMF_Field)             :: newField,psF

     Iam = "MAPL_ExtDataGridCompRead"

     if (side == MAPL_ExtDataLeft) then
        time = item%time1
     else if (side == MAPL_ExtDataRight) then 
        time = item%time2
     end if
     name  = trim(item%var)
     trans = item%trans
     val   = item%FracVal
     ignoreCase = ExtState%ignoreCase

     if ((item%do_Fill .or. item%do_VertInterp) .and. item%VarType == MAPL_FieldItem) then

        ASSERT_(item%VarType == MAPL_FieldItem)
        call MAPL_ExtDataGetBracket(item,side,field=Field,__RC__) 
        call ESMF_FieldGet(field,grid=grid,__RC__)
        newGrid = MAPL_ExtDataGridChangeLev(grid,ExtState%cf,item%lm,__RC__)
        newField = MAPL_FieldCreate(field,newGrid,lm=item%lm,newName=name,__RC__)
     
     else if (.not. item%do_VertInterp .and. .not. item%do_Fill .and. item%VarType == MAPL_FieldItem) then
 
        call MAPL_ExtDataGetBracket(item,side,field=NewField,__RC__)

     end if

     if (item%VarType == MAPL_FieldItem) then
!!$        if (trans == MAPL_HorzTransOrderBilinear) then
        if (trans == REGRID_METHOD_BILINEAR) then
           call MAPL_CFIORead(name, file, time, newfield, &
                time_is_cyclic=.false., time_interp=.false., ignoreCase = ignoreCase, &
                __RC__)
!!$        else if (trans == MAPL_HorzTransOrderBinning) then
        else if (trans == REGRID_METHOD_CONSERVE) then
           call MAPL_CFIORead(name, file, time, newfield, &
                time_is_cyclic=.false., time_interp=.false., ignoreCase = ignoreCase, &
                Conservative = .true., __RC__)
!!$        else if (trans == MAPL_HorzTransOrderSample) then
        else if (trans == REGRID_METHOD_VOTE) then
           call MAPL_CFIORead(name, file, time, newfield, &
                time_is_cyclic=.false., time_interp=.false., ignoreCase = ignoreCase, &
                Conservative = .true., Voting = .true., __RC__)
!!$        else if (trans == MAPL_HorzTransOrderFraction) then
        else if (trans == REGRID_METHOD_FRACTION) then
           call MAPL_CFIORead(name, file, time, newfield, &
                time_is_cyclic=.false., time_interp=.false., ignoreCase = ignoreCase, &
                Conservative = .true., getFrac = val, __RC__)
        end if

     else if (item%VarType == MAPL_BundleItem) then

        call MAPL_ExtDataGetBracket(item,side,bundle=bundle,__RC__)

!!$        if (trans == MAPL_HorzTransOrderBilinear) then
        if (trans == REGRID_METHOD_BILINEAR) then
           call MAPL_CFIORead(file, time, bundle, &
                time_is_cyclic=.false., time_interp=.false., ignoreCase = ignoreCase, &
                __RC__)
!!$        else if (trans == MAPL_HorzTransOrderBinning) then
        else if (trans == REGRID_METHOD_CONSERVE) then
           call MAPL_CFIORead(file, time, bundle, &
                time_is_cyclic=.false., time_interp=.false., ignoreCase = ignoreCase, &
                Conservative = .true., __RC__)
!!$        else if (trans == MAPL_HorzTransOrderSample) then
        else if (trans == REGRID_METHOD_VOTE) then
           call MAPL_CFIORead(file, time, bundle, &
                time_is_cyclic=.false., time_interp=.false., ignoreCase = ignoreCase, &
                Conservative = .true., Voting = .true., __RC__)
!!$        else if (trans == MAPL_HorzTransOrderFraction) then
        else if (trans == REGRID_METHOD_FRACTION) then
           call MAPL_CFIORead(file, time, bundle, &
                time_is_cyclic=.false., time_interp=.false., ignoreCase = ignoreCase, &
                Conservative = .true., getFrac = val, __RC__)
        end if

     end if

     if (item%do_VertInterp .and. item%VarType == MAPL_FieldItem) then
        call MAPL_ExtDataGetBracket(item,side,field=Field,__RC__) 
        id_ps = ExtState%primaryOrder(1)
        call MAPL_ExtDataGetBracket(ExtState%primary%item(id_ps),side,field=psF,__RC__)
        call vertInterpolation_pressKappa(field,newfield,psF,item%levs,MAPL_UNDEF,__RC__)

     else if (item%do_Fill .and. item%VarType == MAPL_FieldItem) then
        call MAPL_ExtDataFillField(field,newfield,rc=status)
        VERIFY_(STATUS)
     end if

     RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_ExtDataGridCompRead

  function MAPL_ExtDataGridChangeLev(Grid,CF,lm,rc) result(NewGrid)

     type(ESMF_Grid), intent(inout) :: Grid
     type(ESMF_Config), intent(inout) :: CF
     integer,         intent(in)    :: lm
     integer, optional, intent(out) :: rc

     integer :: status
     character(len=ESMF_MAXSTR) :: Iam

     character(len=ESMF_MAXSTR) :: gname
     integer :: counts(3)
     integer :: NX,NY
     type(ESMF_Grid)           :: newGrid
     !type(ESMF_Grid), external :: AppGridCreateF

     IAM = "MAPL_ExtDataGridChangeLev"

     call MAPL_GridGet(grid,globalCellCountPerDim=counts,__RC__)
     call ESMF_GridGet(grid,name=gName,__RC__)
     call ESMF_ConfigGetAttribute(CF, value = NX, Label="NX:", __RC__)
     call ESMF_ConfigGetAttribute(CF, value = NY, Label="NY:", __RC__)
     print *, IAM, ' count ', counts
     if (counts(2) == 6*counts(1)) then
        newgrid = AppGridCreateF(counts(1),counts(2),lm,NX,NY,status)
        VERIFY_(STATUS)
     else
        newgrid = MAPL_LatLonGridCreate(name="gName", &
                              NX = NX,                &
                              NY = NY,                &
                              IM_World = counts(1),   &
                              JM_World = counts(2),   &
                              LM_World = lm,     &
                                    RC=STATUS )
        VERIFY_(STATUS)
     end if
     RETURN_(ESMF_SUCCESS)

  end function MAPL_ExtDataGridChangeLev

  subroutine MAPL_ExtDataGetBracket(item,Bside,field,bundle,vcomp,rc)

     type(PrimaryExport),              intent(inout) :: item
     integer,                          intent(in   ) :: bside
     type(ESMF_Field),       optional, intent(inout) ::field
     type(ESMF_FieldBundle), optional, intent(inout) ::bundle
     integer,                optional, intent(in   ) :: vcomp
     integer,                optional, intent(out  ) :: rc

     character(len=ESMF_MAXSTR) :: Iam
     
     Iam = "MAPL_ExtDataGetBracket"

     if (present(vcomp)) then

        if (present(field)) then
           if (Bside == MAPL_ExtDataLeft .and. vcomp == 1) then 
              field = item%v1_finterp1
              RETURN_(ESMF_SUCCESS)
           else if (Bside == MAPL_ExtDataLeft .and. vcomp == 2) then 
              field = item%v2_finterp1
              RETURN_(ESMF_SUCCESS)
           else if (Bside == MAPL_ExtDataRight .and. vcomp == 1) then 
              field = item%v1_finterp2
              RETURN_(ESMF_SUCCESS)
           else if (Bside == MAPL_ExtDataRight .and. vcomp == 2) then 
              field = item%v2_finterp2
              RETURN_(ESMF_SUCCESS)
           end if
        else if (present(bundle)) then
           RETURN_(ESMF_FAILURE)
        end if

     else

        if (present(field)) then
           if (Bside == MAPL_ExtDataLeft) then 
              field = item%finterp1
              RETURN_(ESMF_SUCCESS)
           else if (Bside == MAPL_ExtDataRight) then 
              field = item%finterp2
              RETURN_(ESMF_SUCCESS)
           end if
        else if (present(bundle)) then
           if (Bside == MAPL_ExtDataLeft) then 
              bundle = item%binterp1
              RETURN_(ESMF_SUCCESS)
           else if (Bside == MAPL_ExtDataRight) then 
              bundle = item%binterp2
              RETURN_(ESMF_SUCCESS)
           end if

        end if

     end if
     RETURN_(ESMF_FAILURE)

  end subroutine MAPL_ExtDataGetBracket

  subroutine MAPL_ExtDataFillField(FieldF,FieldR,rc)

  type(ESMF_Field), intent(inout) :: FieldF
  type(ESMF_Field), intent(inout) :: FieldR
  integer, optional, intent(out)  :: rc

  character(len=ESMF_MAXSTR) :: Iam
  integer :: status

  real, pointer :: ptrF(:,:,:),ptrR(:,:,:)
  integer :: lm_in,lm_out,i

  Iam = "MAPL_ExtDataFillField"

  call ESMF_FieldGet(FieldF,0,farrayPtr=ptrF,rc=status)
  VERIFY_(STATUS)
  call ESMF_FieldGet(FieldR,0,farrayPtr=ptrR,rc=status)
  VERIFY_(STATUS)
  ptrF = 0.0
  lm_in= size(ptrR,3)
  lm_out = size(ptrF,3)
  do i=1,lm_in
     ptrF(:,:,lm_out-i+1)=ptrR(:,:,i)
  enddo

  RETURN_(ESMF_SUCCESS)
  
  end subroutine MAPL_ExtDataFillField
    
  end module EXTDATA
