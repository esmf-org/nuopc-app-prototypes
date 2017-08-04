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
           phaseLabelList=(/"IPDv02p1"/), userRoutine=InitAdvertise, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
        return  ! bail out
      call NUOPC_CompSetEntryPoint(GC, ESMF_METHOD_INITIALIZE, &
           phaseLabelList=(/"IPDv02p3"/), userRoutine=InitRealize, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
      
#if 0
      call NUOPC_CompSpecialize(GC, specLabel=model_label_DataInitialize, &
           specRoutine=initTracer, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out
#endif
      
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
           acceptStringList=(/"IPDv02p"/), rc=rc)
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

      
#if 0      
      ! explicit field advertising below
      
      call ESMF_UserCompGetInternalState(gc, 'MAPL_VarSpec', mystates_ptr, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out

      !FIXME: Not sure how ExtData interacts with RUN_DT
      !Setting here because required by NUOPC_AddExportSpec call below.
      !This needs to be cleaned up.
      call ESMF_AttributeSet(GC, "RUN_DT:", 900, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
      
      call NUOPC_AddExportSpec(GC,                                  &
           SHORT_NAME         = 'TRADV',                             &
           LONG_NAME          = 'advected_quantities',               &
           units              = 'X',                                 &
           DIMS               = MAPL_DimsHorzVert,                   &
           VLOCATION          = MAPL_VLocationCenter,                &
           !DATATYPE           = MAPL_BundleItem,                     &
           RC=RC  )
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out

      exportSpec => mystates_ptr%ptr%exportSpec
                  
      ! below is a candidate for a generic behavior to move into NUOPC
      do i=1,size(exportSpec)
         call MAPL_VarSpecGet(exportSpec(i), SHORT_NAME=short_name, LONG_NAME=long_name, &
              STAT=mytype, UNITS=units, rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
         
         call NUOPC_Advertise(EXPORT, &
              StandardName=long_name, name=short_name, units=units, rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out        
      end do
#endif
      
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
      integer, intent(  out) :: RC     ! Error code
!
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
      type(ESMF_Grid)              :: esmfGrid
      type (mystates_WRAP)         :: mystates_ptr
      type(MAPL_VarSpec), pointer  :: exportSpec(:)
      

      RC = ESMF_SUCCESS
      
      call ESMF_GridCompGet ( GC, name=COMP_NAME, RC=rc )
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      Iam = trim(COMP_NAME)//'::InitRealize'

      call ESMF_LogWrite(Iam, ESMF_LOGMSG_INFO, rc=rc)      

      !FIXME:  Explicitly create a grid here
      call My_GridCreate  (GC, rc )
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
            
      ! Get the grid related information
      !---------------------------------
      call ESMF_GridCompGet ( GC, GRID=esmfGrid, rc=RC)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      
      ! Get the exportSpec from internal state
      call ESMF_UserCompGetInternalState(GC, "MAPL_VarSpec", mystates_ptr, rc) 
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
      exportSpec => mystates_ptr%ptr%exportSpec
      
      ! realize connected Fields in the importState
      call realizeConnectedFields(EXPORT, exportspec, esmfGrid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    contains  !--------------------------------------------------------
      
      subroutine realizeConnectedFields(state, spec, grid, rc)
        ! TODO: this method may move into the NUOPC_ utility layer
        type(ESMF_State)                :: state
        type(MAPL_VarSpec),pointer      :: spec(:)
        type(ESMF_Grid)                 :: grid
        integer, intent(out), optional  :: rc
        ! local variables
        character(len=ESMF_MAXSTR)      :: fieldName
        character(len=ESMF_MAXSTR)      :: name
        integer                         :: i, itemCount, k
        type(ESMF_Field)                :: field
        real(ESMF_KIND_R8), pointer     :: fptr(:)
        
        if (present(rc)) rc = ESMF_SUCCESS
        
        itemCount=size(spec)
        
        k=1 ! initialize
        do i=1, itemCount 
           ! find the VarSpec with matching long_name
           call MAPL_VarSpecGet(spec(i),LONG_NAME=fieldName, SHORT_NAME=name, rc=rc)
           if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
           call MAPL_VarSpecSet(spec(i), GRID=grid,rc=rc)
           if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
           if (NUOPC_IsConnected(state, fieldName=name)) then
              ! create a Field
              field = NUOPC_FieldCreateFromSpec(spec(i),rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                   line=__LINE__, &
                   file=__FILE__)) &
                   return  ! bail out
              ! realize the connected Field using the just created Field
              call NUOPC_Realize(state, field=field, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                   line=__LINE__, &
                   file=__FILE__)) &
                   return  ! bail out
           else
              ! We might need to keep them for now to allow the connection between
              ! the friendly fields and 
              ! remove a not connected Field from State in a later phase
              call ESMF_StateRemove(state, (/name/), rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                   line=__LINE__, &
                   file=__FILE__)) &
                   return  ! bail out
              print *, 'ExtData remove field ', name
              
           endif
        enddo
        
      end subroutine realizeConnectedFields
      
    end subroutine initRealize
    
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

    
    ! FIXME:  this copied over from ctm.F90 temporarily
    ! so we have a way of getting the grid.  How does ExtData
    ! currently deal with grids?  Can each component ask for
    ! data on its own grid?
    subroutine My_GridCreate(GC, rc)
      type(ESMF_GridComp) :: GC
      integer             :: rc
      
      ! local variables
      type(ESMF_Config)               :: config
      integer                         :: NX, NY
      character(len=ESMF_MAXSTR)      :: Gridname
      type (ESMF_Grid)                :: GRID
      integer                         :: IM_WORLD
      integer                         :: JM_WORLD
      integer                         :: LM,L,NN
      character(len=4)                :: imsz
      character(len=5)                :: jmsz
      character(len=2)                :: date
      character(len=2)                :: pole
      integer, allocatable            :: regDecomp(:,:)
      
      rc = ESMF_SUCCESS
      
      call ESMF_GridCompGet(GC, config=config, rc=rc)   
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
      call ESMF_ConfigGetAttribute(config, value=NX, label='NX:', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
      call ESMF_ConfigGetAttribute(config, value=NY, label='NY:', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
      call ESMF_ConfigGetAttribute(config, value=Gridname, label='GRIDNAME:', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
      call ESMF_ConfigGetAttribute(config, value=LM, label='LM:', default=1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
      
      Gridname = AdjustL(Gridname)
      nn   = len_trim(Gridname)
      imsz = Gridname(3:index(Gridname,'x')-1)
      jmsz = Gridname(index(Gridname,'x')+1:nn-3)
      pole = Gridname(1:2)
      date = Gridname(nn-1:nn)
      
      read(IMSZ,*) IM_WORLD
      read(JMSZ,*) JM_WORLD
      
      ! date='CF' for cubed sphere
      if (date=='CF' .or. date=='DP') then
         ! make sure NY is divisble by 6 and JM_WORLD is IM_WORLD*6
         if (mod(NY, 6) /= 0) then
            call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg='NY has to be multiple of 6', &
                 line=__LINE__, &
                 file=__FILE__, rcToReturn=rc) 
            return
         endif
         allocate(regDecomp(2,6))
         regDecomp(1,:)=NX
         regDecomp(2,:)=NY/6
         print *, regDecomp(:,1), IM_WORLD, trim(Gridname)
         grid = ESMF_GridCreateCubedSphere(IM_WORLD, regDecompPTile = regDecomp, &
              name=trim(Gridname), rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
      endif
      
      call ESMF_AttributeSet(grid, name='GRID_LM', value=LM, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
      
      ! Set grid to the GridComp
      call ESMF_GridCompSet(GC, grid=grid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
      
      return
    end subroutine My_GridCreate
    
    
    
  end module EXTDATA
