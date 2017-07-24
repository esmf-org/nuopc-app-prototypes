#include "MAPL_Generic.h"
!=============================================================================
!            NASA/GSFC, Software System Support Office, Code 610.3           !
!=============================================================================
!BOPe
!
! !MODULE: PTRACER
!
! !INTERFACE:
!
      module PTRACER
!
! !USESs:
      use ESMF
      use NUOPC
      use NUOPC_Model, &
           model_routine_SS      => SetServices, &
           model_label_Advance   => label_Advance, &
           model_label_DataInitialize   => label_DataInitialize
      use NUOPC_Generic
!      use m_set_eta,       only: set_eta
      use MAPL_Mod
!      USE Chem_UtilMod
      
!      USE jw, only : tracer_q, tracer_q1_q2, tracer_q3

      implicit none
!
      private
!
! !PUBLIC MEMBER FUNCTIONS:
      public SetServices
!
! !DESCRIPTION: 
!   {\tt CTM\_pTracers} is a passive tracer component that is friendly
!   to DYNAMICS. A set of tracers is initialized through either through
!   internal computations (idealized tracers) or through a restart file
!   and advected for the desired period of time. The only operation
!   involved is Advection.
!
!EOP
!=============================================================================

!#     include "GmiParameters.h"

#define PRINT_STATES

      type t_ArrayBundle
         real, pointer  :: pArray3D(:,:,:) => null()
      end type t_ArrayBundle

      type T_pTracers_STATE
         private
         type(t_ArrayBundle),        pointer :: tr(:)     => null()
         CHARACTER(LEN=ESMF_MAXSTR), pointer :: vname(:)  => null()
         CHARACTER(LEN=ESMF_MAXSTR), pointer :: vtitle(:) => null()
         CHARACTER(LEN=ESMF_MAXSTR), pointer :: vunits(:) => null()
      end type T_pTracers_STATE

      type pTracers_WRAP
         type (T_pTracers_STATE), pointer :: PTR
      end type pTracers_WRAP

      logical :: do_AdvColdStart
      logical :: do_ComputeTracerMass
      integer, parameter :: REAL4 = 4 !kind(1.00)
      integer, parameter :: REAL8 = 8 !kind(1.d0)
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
      integer, optional :: RC  ! return code
!
! !INPUT/OUTPUT PARAMETERS:
      type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component

! !DESCRIPTION: Sets Initialize and Run services. 
! \newline
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
      type (ESMF_Config )              :: configFile
      type (mystates_WRAP)             :: mystates_ptr
      type (my_States), pointer        :: mystates
      INTEGER                          :: numTracers, dims(3)

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
    call ESMF_LogWrite("dyn InitializeP0", ESMF_LOGMSG_INFO, rc=rc)

    ! Switch to IPDv02 (for datainitialize dependency loop) 
    ! by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv02p"/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitAdvertise(GC, IMPORT, EXPORT, CLOCK, rc)
     type(ESMF_GridComp)  :: GC
     type(ESMF_State)     :: IMPORT
     type(ESMF_State)     :: EXPORT
     type(ESMF_Clock)     :: CLOCK
     integer, intent(out) :: rc
    

     CHARACTER(LEN=ESMF_MAXSTR)       :: rcfilen = 'pTracers_GridComp.rc'
     character(len=ESMF_MAXSTR)       :: IAm = 'InitAdvertise'
     type(ESMF_Config)                :: configFile
     type(MAPL_VarSpec), pointer      :: importSpec(:), exportSpec(:)
     character(len=ESMF_MAXSTR)       :: FRIENDLIES
     type (mystates_WRAP)             :: mystates_ptr
     type (T_pTracers_STATE), pointer :: state 
     type (pTracers_wrap)             :: wrap
     INTEGER                          :: numTracers, dims(3)
     INTEGER                          :: n, i, ic
     CHARACTER(LEN=ESMF_MAXSTR)       :: tempNames(150)
     CHARACTER(LEN=38400)             :: longList
     character(len=2)                 :: id
     character(len=ESMF_MAXSTR)       :: short_name, long_name
     character(len=ESMF_MAXSTR)       :: units
     
     rc = ESMF_SUCCESS

    call ESMF_LogWrite("PTRACER:InitAdvertise", ESMF_LOGMSG_INFO, rc=rc)      

     configFile = ESMF_ConfigCreate(rc=rc )
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

     call ESMF_ConfigLoadFile(configFile, TRIM(rcfilen), rc=rc )
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

     call ESMF_GridCompSet(GC, config=ConfigFile, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

     call ESMF_ConfigGetAttribute(configFile, do_ComputeTracerMass, &
                         Default  = .FALSE.,                         &
                         Label    = "do_ComputeTracerMass:", rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

     call ESMF_ConfigGetAttribute(configFile, do_AdvColdStart,      &
                         Default  = .FALSE.,                         &
                         Label    = "do_AdvColdStart:",      rc=rc )
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

      ! Wrap internal state for storing in GC; rename legacyState
      ! -------------------------------------
      allocate ( state, stat=rc )
      wrap%ptr => state

! !IMPORT STATE:

     call NUOPC_AddImportSpec ( GC,                                  &
           SHORT_NAME = 'AREA',                                      &
           LONG_NAME  = 'agrid_cell_area',                           &
           UNITS      = 'm+2'  ,                                     &
           DIMS       = MAPL_DimsHorzOnly,                           &
           VLOCATION  = MAPL_VLocationNone,               RC=rc  )
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

     call NUOPC_AddImportSpec(GC,                                  &
          SHORT_NAME = 'PLE',                                       &
!  name inconsistent with the name used in ECTM
!          LONG_NAME  = 'air_pressure',                              &
          LONG_NAME  = 'pressure_at_layer_edges',                     &
          UNITS      = 'Pa',                                        &
          DIMS       =  MAPL_DimsHorzVert,                          &
          VLOCATION  =  MAPL_VLocationEdge,                         &
                                                         RC=rc  )
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

      call ESMF_UserCompGetInternalState(gc, 'MAPL_VarSpec', mystates_ptr, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out

     !Advertize the import fields
     importSpec => mystates_ptr%ptr%importSpec

     print *, 'PTRACER number of import fields: ', size(importSpec)
     
     do i=1,size(importSpec)
        call MAPL_VarSpecGet(importSpec(i), SHORT_NAME=short_name, &
        LONG_NAME=long_name, UNITS=units, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
        call NUOPC_Advertise(IMPORT, &
           StandardName=long_name, name=short_name, units=units, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
        !print *, 'PTRACER: advertise import field ', long_name
     end do


     if (do_AdvColdStart) then
         numTracers = 5

         allocate(state%vname(numTracers), stat=rc)
         allocate(state%vunits(numTracers), stat=rc)
         allocate(state%vtitle(numTracers), stat=rc)
  
         do ic = 1, numTracers
            write (id  ,'(i2.2)') ic-1
            state%vname (ic) = 'Q'//id
            state%vtitle(ic) = 'Tracer Q'//id
            state%vunits(ic) = 'mol/mol'
         end do

      else
         call rcEsmfReadTable2String(configFile, longList, "vNames::", rc=rc)
     
         !-------------------------------------
         ! Obtain the name of all the tracers
         !-------------------------------------
         tempNames(:) = ''

         call constructListNames(tempNames, longList)

         numTracers = Count (tempNames(:) /= '')

         allocate(state%vname(numTracers))

         state%vname(:) = tempNames(1:numTracers)

         allocate(state%vunits(numTracers))

         allocate(state%vtitle(numTracers))
         state%vtitle(:) = ''

         ! Obtain the units of tracers
         !----------------------------
         call rcEsmfReadTable2String(configFile, longList, "vunits::", rc=rc)

         tempNames(:) = ''
         call constructListNames(tempNames, longList)

         state%vunits(:) = tempNames(1:numTracers)

      end if

      IF ( MAPL_AM_I_ROOT() ) THEN
         WRITE(*,101) numTracers
         DO n = 1, numTracers
            WRITE(*,201) n, TRIM(state%vname(n)), TRIM(state%vunits(n))
         END DO
         PRINT *," "
      ENDIF

  101 FORMAT(/,'  ---------------------------------------',/, &
             '          There are ',I3,' tracers        ',/, &
             ' ---',2X,'----------------',2X,'----------------',/, &
             ' ID ',2X,'      Name      ',2X,'      Unit      ',/, &
             ' ---',2X,'----------------',2X,'----------------')
  201 FORMAT(' ',I3,2X,A16,2X,A16)
      
      !call ESMF_GridCompGet( GC, GRID=grid, RC=STATUS )
      !VERIFY_(STATUS)

      !---------------------------
      ! Allocate the state tracers
      !---------------------------
      allocate(state%tr(numTracers))


!                       ------------------------
!                       ESMF Functional Services
!                       ------------------------

      call ESMF_UserCompSetInternalState ( GC, 'PTRACERS', WRAP, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

! !INTERNAL STATE:
! !When FRIENDLYTO is given, these fields will be added into the exportSpec automatically

       FRIENDLIES="DYNAMICS:TURBULENCE:MOIST"

       do n = 1, numTracers
          CALL NUOPC_AddInternalSpec(GC,                          &
               SHORT_NAME         = TRIM(state%vname(n)),        &
               LONG_NAME          = TRIM(state%vtitle(n)),       &
               UNITS              = TRIM(state%vunits(n)),       &
               FRIENDLYTO         = TRIM(FRIENDLIES),            &
               DIMS               = MAPL_DimsHorzVert,           &
               VLOCATION          = MAPL_VLocationCenter,  RC=rc  )
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
             line=__LINE__, &
             file=__FILE__)) &
             return  ! bail out
       enddo

     !Advertize the export fields
     exportSpec => mystates_ptr%ptr%exportSpec

     do i=1,size(exportSpec)
        call MAPL_VarSpecGet(exportSpec(i), SHORT_NAME=short_name, &
           LONG_NAME=long_name,UNITS=units, rc=rc)
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
        !print *, 'PTRACER: advertise export field ', long_name
    end do

#if 0
      ! Set the Profiling timers
      ! ------------------------
      call MAPL_TimerAdd ( GC, name = "RUN",        RC=STATUS )
      VERIFY_(STATUS)
      call MAPL_TimerAdd ( GC, name = "INITIALIZE", RC=STATUS )
      VERIFY_(STATUS)
      call MAPL_TimerAdd ( GC, name = "FINALIZE",   RC=STATUS )
      VERIFY_(STATUS)
#endif
  
      ! Generic Set Services
      ! --------------------

      !call MAPL_GenericSetServices ( GC,RC=STATUS )
      !VERIFY_(STATUS)

      RETURN
  
      end subroutine initAdvertise
!EOC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BOP
!
! !IROUTINE: initRealize
! 
! !INTERFACE:
!
      subroutine initRealize ( GC, IMPORT, EXPORT, CLOCK, RC )
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
! !DESCRIPTION: The Initialize method of pTracers gridded component.
!   \newline
!
!EOP
!=============================================================================
!BOC
!
! ErrLog Variables
      character(len=ESMF_MAXSTR)       :: IAm = 'initRealize'
      integer                          :: nymd, nhms  ! time of day
      real                             :: cdt         ! timestep (secs)
      integer                          :: STATUS, ic, nSpc, i, j, k
      character(len=ESMF_MAXSTR)       :: COMP_NAME
! Local derived type aliases
      character(len=ESMF_MAXSTR)      :: short_name
      type (T_pTracers_STATE), pointer :: pTracers_STATE 
      type (pTracers_wrap)             :: WRAP
      type(ESMF_Grid)                  :: esmfGrid
      type(mystates_wrap)              :: mystates_ptr
      type(MAPL_VarSpec), pointer   :: importSpec(:)
      type(MAPL_VarSpec), pointer   :: exportSpec(:)

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !! Need to get the grid from its parent
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ! Get the target components name and set-up traceback handle.
      ! -----------------------------------------------------------

      call ESMF_GridCompGet ( GC, name=COMP_NAME, RC=rc )
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      Iam = trim(COMP_NAME)//'::InitRealize'

      call ESMF_LogWrite(Iam, ESMF_LOGMSG_INFO, rc=rc)      

      ! Get the grid related information
      !---------------------------------
      call ESMF_GridCompGet ( GC, GRID=esmfGrid, rc=RC)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! Get the importSpec and exportSpec from internal state
      call ESMF_UserCompGetInternalState(GC, "MAPL_VarSpec", mystates_ptr, rc) 
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      importSpec => mystates_ptr%ptr%importSpec
      exportSpec => mystates_ptr%ptr%exportSpec

      ! Start timer
      !------------      
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !! Need to get the grid from its parent
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !call MAPL_TimerOn (ggState,"TOTAL"  )
      !call MAPL_TimerOn (ggState,"INITIALIZE"  )

      ! Need to create fields and realize them
      
      ! realize connected Fields in the importState
      call realizeConnectedFields(IMPORT, importspec, esmfGrid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! realize connected Fields in the importState
      call realizeConnectedFields(EXPORT, exportspec,esmfGrid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      call initTracer(GC, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      return

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
          print *, 'PTRACER remove field ', name

        endif
      enddo

    end subroutine realizeConnectedFields

  end subroutine initRealize

      
!----------------------------------------------------------------------
      subroutine initTracer ( GC, RC )
!
      type(ESMF_GridComp)      :: GC     ! Gridded component 
      integer,   intent(  out) :: RC     ! Error code
!
! !DESCRIPTION: The Initialize method of pTracers gridded component.
!   \newline
!
!EOP
!=============================================================================

      REAL(REAL8), POINTER, DIMENSION(:)       :: AK
      REAL(REAL8), POINTER, DIMENSION(:)       :: BK
      integer                  :: IS, IE, JS, JE, KS,KE, KM, IM, JM, LS
      integer  :: p_ks
      real(REAL8) :: dummy_1, dummy_2, dummy_3, dummy_4, dummy_5, dummy_6
      real(REAL8) :: dz, ztop, height, pressure
      real(REAL8) :: LONc,LATc
      real(REAL8) :: eta, eta_top, rot_ang, ptop, pint
      real(ESMF_KIND_R8), pointer  :: LONS   (:,:)
      real(ESMF_KIND_R8), pointer  :: LATS   (:,:)
      type (mystates_WRAP)             :: mystates_ptr
      type (MAPL_VarSpec), pointer        :: internalSpec(:)
      type (T_pTracers_STATE), pointer :: pTracers_STATE 
      type (pTracers_wrap)             :: WRAP
      real(REAL8), parameter           :: r0_6=0.6
      real(REAL8), parameter           :: r1_0=1.0
      type(ESMF_Grid)                  :: esmfGrid
      type(ESMF_Field)                 :: field
      integer                          :: minIndex(2,6)
      integer                          :: maxIndex(2,6)
      integer                          :: tileno, nSpc
      integer                          :: i, j, k, ic
      character(len=ESMF_MAXSTR)       :: short_name
      character(len=ESMF_MAXSTR)       :: Iam = 'SetServices'
      type(ESMF_State)                 :: IMPORT, EXPORT
      type(ESMF_State)                 :: INTERNAL

      ! Get my private state from the component
      !----------------------------------------

      call ESMF_UserCompGetInternalState(gc, 'MAPL_VarSpec', mystates_ptr, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out

      internalSpec => mystates_ptr%ptr%InternalSpec

      call ESMF_UserCompGetInternalState(gc, 'PTRACERS', WRAP, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out

      pTracers_STATE => WRAP%PTR

      call ESMF_GridCompGet ( GC, GRID=esmfGrid, importState=IMPORT, &
                            exportState=EXPORT, rc=RC)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out

      call ESMF_AttributeGet(esmfGrid, value=KM, name="GRID_LM", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out

      ! Get the coordinates of the local grid      
      call ESMF_GridGetCoord(esmfGrid, 1, farrayPtr=LONS, rc=rc) 
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
      call ESMF_GridGetCoord(esmfGrid, 2, farrayPtr=LATS, rc=rc) 
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
      
      !  Associate the Internal State fields with our legacy state 
      !  ---------------------------------------------------------
      ! call MAPL_Get ( ggSTATE, INTERNALSPEC=InternalSpec, &
      !                INTERNAL_ESMF_STATE=internal, &
      !                LM = KM, lats = LATS, lons = LONS, RC=RC  )
      ! VERIFY_(RC)

      ! How do I find out KM that is defined in GEOSCTM.rc, the config for ctm component?
      ! defined it as an attribute to the grid
      KS = 1
      KE = KM

      IS = lbound(LONS,1)
      IE = ubound(LONS,1)
      JS = lbound(LONS,2)
      JE = ubound(LONS,2)

      print *, 'Local grid dimension:', IS, IE, JS, JE, KS, KE

      if (do_AdvColdStart) then
         ! Get AKs and BKs for vertical grid
         !----------------------------------
         AllOCATE( AK(0:KM))
         AllOCATE( BK(0:KM))
  
         !call set_eta(KM,LS,PTOP,PINT,AK,BK)
         !rot_ang = 0
      end if

      nSpc = size(pTracers_STATE%tr(:))

      ! Consistency Checks
      !-------------------
      ! ASSERT_ ( size(InternalSpec) == nSpc )

      do ic = 1, size(InternalSpec)
         call MAPL_VarSpecGet ( InternalSpec(ic),          &
                             SHORT_NAME = short_name,  &
                             RC=RC )
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out

         ! Should I get it from the export state????
         call ESMF_StateGet(EXPORT, short_name, field, rc=rc)  
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out

         ! Get the pointer from the field
         call ESMF_FieldGet(field, farrayptr = pTracers_STATE%tr(ic)%pArray3D, rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out

        !CALL MAPL_GetPointer ( internal, &
         !                       NAME = short_name, &
         !                       ptr  = pTracers_STATE%tr(ic)%pArray3D, &
         !                       rc   = RC )
         !VERIFY_(RC)

         ! Determine the concentrations of idealized tracers
         !--------------------------------------------------
         if (do_AdvColdStart) then
            if (TRIM(short_name) == 'Q00') then
               do k=KS,KE
                  do j=JS,JE
                     do i=IS,IE
                        pTracers_STATE%tr(ic)%pArray3D(i,j,k) = 0.0
                     enddo
                  enddo
               enddo
            elseif (TRIM(short_name) == 'Q01') then
               do k=KS,KE
                  eta = 0.5*( (AK(k-1)+AK(k))/1.e5 + BK(k-1)+BK(k) )
                  do j=JS,JE
                     do i=IS,IE
                        LONc = LONS(i,j)
                        LATc = LATS(i,j)
                        !dummy_1 = tracer_q1_q2(LONc,LATc,eta,rot_ang,r0_6)
                        dummy_1 = 2.0
                        pTracers_STATE%tr(ic)%pArray3D(i,j,k) = dummy_1
                     enddo
                  enddo
               enddo
            elseif (TRIM(short_name) == 'Q02') then
               do k=KS,KE
                  eta = 0.5*( (AK(k-1)+AK(k))/1.e5 + BK(k-1)+BK(k) )
                  do j=JS,JE
                     do i=IS,IE
                        LONc = LONS(i,j)
                        LATc = LATS(i,j)
                        !dummy_1 = tracer_q1_q2(LONc,LATc,eta,rot_ang,r1_0)
                        dummy_1 = 3.0
                        pTracers_STATE%tr(ic)%pArray3D(i,j,k) = dummy_1
                     enddo
                  enddo
               enddo
            elseif (TRIM(short_name) == 'Q03') then
               do k=KS,KE
                  eta = 0.5*( (AK(k-1)+AK(k))/1.e5 + BK(k-1)+BK(k) )
                  do j=JS,JE
                     do i=IS,IE
                        LONc = LONS(i,j)
                        LATc = LATS(i,j)
                        !dummy_1 = tracer_q3(LONc,LATc,eta,rot_ang)
                        dummy_1 = 4.0
                        pTracers_STATE%tr(ic)%pArray3D(i,j,k) = dummy_1
                     enddo
                  enddo
               enddo
            elseif (TRIM(short_name) == 'Q04') then
               pTracers_STATE%tr(ic)%pArray3D(:,:,:)  = 1.0_REAL4
            end if
         end if

      end do

      if (do_AdvColdStart) DEALLOCATE(AK, BK)

      km = size(pTracers_STATE%tr(1)%pArray3D, 3)

      !call extract_ ( GC, clock, nymd, nhms, cdt, rc )


#ifdef PRINT_STATES
      ! Print what my states are
      !-------------------------
      if ( MAPL_am_I_root() ) then
         print *,  trim(Iam)//": IMPORT State" 
                                 call ESMF_StatePrint ( IMPORT)
!         print *,  trim(Iam)//": INTERNAL State" 
!                                 call ESMF_StatePrint ( WRAP )
         print *,  trim(Iam)//": EXPORT State" 
                                 call ESMF_StatePrint ( EXPORT )
      end if
#endif

      !call MAPL_TimerOff (ggState,"INITIALIZE" )
      !call MAPL_TimerOff (ggState,"TOTAL"      )

      ! All Done
      !---------

      RETURN_(ESMF_SUCCESS)

      end subroutine initTracer
!EOC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BOP
!
! !IROUTINE: RUN
!
! !INTERFACE:
!
      subroutine modelAdvance( GC, RC )
!
! !INPUT/OUTPUT PARAMETERS:
      type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
!
! !OUTPUT PARAMETERS:
      integer, optional,   intent(  out) :: RC     ! Error code
!
! !DESCRIPTION: Does nothing for now.
! \newline
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
      type (MAPL_MetaComp), pointer    :: ggState
      type(MAPL_VarSpec), pointer     :: InternalSpec(:)
      type (ESMF_State)               :: internal
      character(len=ESMF_MAXSTR)      :: short_name
      type (T_pTracers_STATE), pointer :: pTracers_STATE
      type (pTracers_wrap)             :: WRAP
      type(ESMF_Grid)                  :: esmfGrid

! Imports
      real, pointer, dimension(:,:,:)   ::  pe => null()
      real, pointer, dimension(:,:)     :: gridCellArea => null()

      real, pointer, dimension(:,:,:)   ::  dp => null()
      real     , allocatable :: qsum(:,:)
      real*8                 :: totMass

      REAL, POINTER :: PLE(:,:,:) => null()
      REAL, POINTER :: cellArea(:,:) => null()
      integer :: DIMS(3), im, jm, km, k, nSpc, ic

!
      ! Get the target components name and set-up traceback handle.
      ! -----------------------------------------------------------

      ! Get the target components name and set-up traceback handle.
      ! -----------------------------------------------------------

      call ESMF_GridCompGet( GC, name=COMP_NAME, GRID=esmfGrid, RC=STATUS )
      VERIFY_(STATUS)
      Iam = trim(COMP_NAME)//"::Run_"

      ! Retrieve the pointer to the generic state
      !------------------------------------------

#if 0
      call MAPL_GetObjectFromGC ( GC, ggState, RC=STATUS)
      VERIFY_(STATUS)

      ! Start timer
      !------------

      call MAPL_TimerOn (ggState,"TOTAL")
      call MAPL_TimerOn (ggState,"RUN"  )

      ! Retrieve the pointer to the private internal state
      !---------------------------------------------------

      call ESMF_UserCompGetInternalState(GC, 'PTRACERS', WRAP, STATUS)
      VERIFY_(STATUS)

      pTracers_STATE => WRAP%PTR

      !  Associate the Internal State fields with our legacy state 
      !  ---------------------------------------------------------
      call MAPL_Get ( ggSTATE, INTERNALSPEC=InternalSpec, &
                      INTERNAL_ESMF_STATE=internal, RC=STATUS  )
      VERIFY_(STATUS)


      nSpc = size(pTracers_STATE%tr(:))

      ! Consistency Checks
      !-------------------
      ASSERT_ ( size(InternalSpec) == nSpc )

      IF (do_ComputeTracerMass) THEN
         CALL MAPL_GetPointer(IMPORT, PLE,  'PLE', ALLOC = .TRUE., RC=STATUS)
         VERIFY_(STATUS)

         call MAPL_GetPointer(IMPORT, cellArea, 'AREA', ALLOC=.true., rc=status)
         VERIFY_(STATUS)

         if ( MAPL_am_I_root() ) then
            PRINT*, "------------------------------------------------"
            PRINT*, "------   Compute the mass of each tracer  ------"
            PRINT*, "------------------------------------------------"
         end if
         do ic = 1, nSpc
            call computeTracerMass(pTracers_STATE%tr(ic)%pArray3D, &
                                   TRIM(pTracers_STATE%vname(ic)), &
                                   esmfGrid, cellArea, PLE)
         end do
         if ( MAPL_am_I_root() ) then
            PRINT*, "------------------------------------------------"
         end if
      END IF

      call MAPL_TimerOff (ggState,"RUN" )
      call MAPL_TimerOff (ggState,"TOTAL"      )

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
! !DESCRIPTION: This is a simple ESMF wrapper.
!
! !REVISION HISTORY:
!
!EOP
!-------------------------------------------------------------------------
!BOC
!

!  ErrLog Variables
!  ----------------
      character(len=ESMF_MAXSTR)   :: IAm = 'Finalize_'
      integer                      :: STATUS
      character(len=ESMF_MAXSTR)   :: COMP_NAME
   
      integer                      :: nymd, nhms  ! time
      real                         :: cdt         ! chemistry timestep (secs)
      type(MAPL_MetaComp), pointer :: ggState     ! GEOS Generic State
      type (T_pTracers_STATE),  pointer :: pTracers_STATE 
      type (pTracers_wrap)              :: WRAP
      !  Get my name and set-up traceback handle
      !  ---------------------------------------
      call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=STATUS )
      VERIFY_(STATUS)
      Iam = trim(COMP_NAME) // '::' // 'Finalize_'

      !  Get my internal MAPL_Generic state
      !  -----------------------------------
      call MAPL_GetObjectFromGC ( GC, ggState, __RC__)

      call MAPL_TimerON(ggState, 'TOTAL')
      call MAPL_TimerON(ggState, 'FINALIZE')

      ! Retrieve the pointer to the private internal state
      !---------------------------------------------------

      call ESMF_UserCompGetInternalState(GC, 'PTRACERS', WRAP, STATUS)
      VERIFY_(STATUS)

      pTracers_STATE => WRAP%PTR

      !  Destroy Legacy state
      !  --------------------
      deallocate ( pTracers_STATE%vname,  pTracers_STATE%vunits, &
                   pTracers_STATE%vtitle, pTracers_STATE%tr, stat = STATUS )
      VERIFY_(STATUS)

      call MAPL_TimerOff(ggState, 'FINALIZE')
      call MAPL_TimerOff(ggState, 'TOTAL')

      !  Finalize GEOS Generic
      !  ---------------------
      call MAPL_GenericFinalize ( gc, IMPORT, EXPORT, clock,  RC=STATUS )
      VERIFY_(STATUS)

      RETURN_(ESMF_SUCCESS)

      end subroutine Finalize_
!EOC
!---------------------------------------------------------------------------
!!BOP
      subroutine extract_ ( gc, clock, nymd, nhms, cdt, &
                          rc, state )
!!
!! !INPUT PARAMETERS:
      type(ESMF_Clock), intent(in)     :: clock
!!
!! !OUTPUT PARAMETERS:
      integer, intent(out)             :: nymd, nhms
      real, intent(out)                :: cdt
      integer, intent(out)             :: rc
!!
!! !INPUT/OUTPUT PARAMETERS:
      type(ESMF_GridComp), intent(INout)  :: gc
      type(T_pTracers_STATE), pointer, optional   :: state
!!
!! !LOCAL VARIABLES:
      type(MAPL_MetaComp), pointer    :: ggState
      type(T_pTracers_STATE), pointer :: myState
      character(len=ESMF_MAXSTR)      :: IAm
      integer                         :: STATUS
      character(len=ESMF_MAXSTR)      :: COMP_NAME
      type(ESMF_Time)      :: TIME
      type(ESMF_Config)    :: CF
      type(pTracers_Wrap)    :: wrap
      integer              :: IYR, IMM, IDD, IHR, IMN, ISC
      integer :: ic
!!
!!EOP
!!---------------------------------------------------------------------------
!!BOC
      ! Get my name and set-up traceback handle
      ! ---------------------------------------
      call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=STATUS )
      VERIFY_(STATUS)
      Iam = trim(COMP_NAME) // '::' // 'extract_'

      rc = 0

     ! Get my internal MAPL_Generic state
     ! -----------------------------------
     call MAPL_GetObjectFromGC ( GC, ggState, __RC__ )

      ! Get my internal state
      ! ---------------------
      call ESMF_UserCompGetInternalState(gc, 'PTRACERS', WRAP, STATUS)
      VERIFY_(STATUS)

      myState => wrap%ptr

      if ( present(state) ) then
           state => wrap%ptr
      end if

!      nSpc = size(myState%tr(:))
!      if (nSpc > 0) then
!         do ic = 1, nSpc
!            PRINT*, ic, TRIM(myState%vname(ic)), sum(myState%tr(ic)%pArray3D)
!         end do
!      end if

      ! This is likely to be allocated during initialize only
      ! -----------------------------------------------------

      ! Get the configuration
      ! ---------------------
      call ESMF_GridCompGet ( GC, CONFIG = CF, RC=STATUS )
      VERIFY_(STATUS)

      ! Get time step
      ! -------------
      call ESMF_AttributeGet ( GC, "RUN_DT:", cdt, RC=STATUS )
      VERIFY_(STATUS)

      ! Need code to extract nymd(20050205), nhms(120000) from clock
      ! ------------------------------------------

      call ESMF_ClockGet(CLOCK,currTIME=TIME,rc=STATUS)
      VERIFY_(STATUS)

      call ESMF_TimeGet(TIME ,YY=IYR, MM=IMM, DD=IDD, H=IHR, M=IMN, S=ISC, rc=STATUS)
      VERIFY_(STATUS)

      call MAPL_PackTime(NYMD,IYR,IMM,IDD)
      call MAPL_PackTime(NHMS,IHR,IMN,ISC)

      RETURN_(ESMF_SUCCESS)

      end subroutine extract_
!!EOC
!---------------------------------------------------------------------------
!BOP
      subroutine computeTracerMass (tracerArray, tracerName, esmfGrid, &
                                    gridCellArea, PLE)

!
! !INPUT PARAMETERS:
      REAL  , intent(in)         :: tracerArray(:,:,:)
      REAL  , intent(in)         :: PLE(:,:,:)
      REAL  , intent(in)         :: gridCellArea(:,:)
      character(len=*) :: tracerName
      type (ESMF_GRID)           :: esmfGrid
!
! !LOCAL VARIABLES:
      INTEGER :: im, jm, lm, k, STATUS, RC
      real, pointer, dimension(:,:,:)   ::  dp => null()
      real     , allocatable :: qsum(:,:)
      real*8                 :: totMass
      character(len=ESMF_MAXSTR) :: Iam = "computeTracerMass"
!EOP
!------------------------------------------------------------------------------
!BOC
      im = size(tracerArray, 1)
      jm = size(tracerArray, 2)
      lm = size(tracerArray, 3)

      allocate( dp(im,jm,lm) )                      !  pressure thickness
      dp(:,:,1:lm) = PLE(:,:,2:lm+1)-PLE(:,:,1:lm)

      allocate( qsum(im,jm) )
      qsum = 0.0
      do k = 1, lm
         qsum(:,:) = qsum(:,:) + tracerArray(:,:,k)*dp(:,:,k)
      end do

      call MAPL_AreaMean( totMass, qsum, gridCellArea, esmfGrid, rc=STATUS )
      VERIFY_(STATUS)

      IF (MAPL_AM_I_ROOT()) then
         PRINT *, TRIM(tracerName),totMass
      ENDIF

      deallocate(qsum, dp)

      return

      end subroutine computeTracerMass
!EOC
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: constructListNames
!
! !INTERFACE:

     subroutine constructListNames(tracerNames, names)
!
     implicit none
!
! OUTPUT PARAMETERS:
     character (len=*), intent(  out) :: tracerNames(*)
!
! !INPUT/OUTPUT PARAMETERS:
     character (len=*), intent(inout) :: names
!
! !DESCRIPTION:
!  This routine takes the long string "names" (containing a list
!  of tracers, separated by commas) and construct a new list
!  where each tracer name is a string.
!
! !LOCAL VARIABLES:
     integer             :: loc, namesLen, i
!
! !AUTHOR:
!  Jules Kouatchou
!
! !REVISION HISTORY:
!  Initial code.
!
!EOP
!-------------------------------------------------------------------------
!BOC

      i = 1
      loc = index(names, ',')
      if (loc == 0) then
         tracerNames(i) = names
         tracerNames(i) = adjustl(tracerNames(i))
         tracerNames(i) = trim   (tracerNames(i))
      else
         do while(loc > 0)
            tracerNames(i) = names(1:loc-1)
            tracerNames(i) = adjustl(tracerNames(i))
            tracerNames(i) = trim   (tracerNames(i))
            namesLen = len(names)
            names = names(loc+1:namesLen)
            i = i + 1
            loc = index(names, ',')
         end do
         tracerNames(i) = names
         tracerNames(i) = adjustl(tracerNames(i))
         tracerNames(i) = trim   (tracerNames(i))
      end if

      return

      end subroutine constructListNames
!EOC
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: rcEsmfReadTable2String
!
! !INTERFACE:
!
      subroutine rcEsmfReadTable2String(config, value, label, rc)
! 
      implicit none

      INTEGER, PARAMETER :: MAX_STRING_LENGTH = 128*50
!
! !INPUT PARAMETERS:
      character(len=*), intent(in) :: label
! 
! !OUTPUT PARAMETERS:
      character(len=MAX_STRING_LENGTH), intent(out) :: value
      integer, optional, intent(out) :: rc
! 
! !INPUT/OUTPUT PARAMETERS:
      type(ESMF_Config), intent(inOut) :: config
!     
! !DESCRIPTION:
! Reads in a table of words from a resource file.
!
! !LOCAL VARIABLES:
      integer :: STATUS
      logical :: firstIter, endTable
      character(len=1) :: cValue
      character(len=ESMF_MAXSTR) :: tempWord
      character(len=ESMF_MAXSTR), parameter :: IAm = "rcEsmfReadTable2String"
!EOP
!------------------------------------------------------------------------------
!BOC     
      firstIter = .true.

      call ESMF_ConfigFindLabel(config, label=label, rc=STATUS )

      value = ''

      if (STATUS == ESMF_SUCCESS) then
         call ESMF_ConfigNextLine  (config, tableEnd=endTable, rc=STATUS )
         VERIFY_(STATUS)

         do while (.not. endTable)
            call ESMF_ConfigGetAttribute(config, tempWord, rc=STATUS )
            VERIFY_(STATUS)

            call reconstructPhrase(tempWord)

            if (firstIter) then
               firstIter = .false.
               value = trim(tempWord)
            else
               value = trim(value)//', '//trim(tempWord)
            end if

            call ESMF_ConfigNextLine  (config, tableEnd=endTable, rc=STATUS )
            VERIFY_(STATUS)
         end do
      end if

      if (present(rc)) rc = STATUS

      return

      end subroutine rcEsmfReadTable2String
!EOC
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: reconstructPhrase
!
! !INTERFACE:

      subroutine reconstructPhrase(phrase)
!
      implicit none
!
! !INPUT/OUTPUT PARAMETERS:
      character (len=*), intent(inOut) :: phrase
!
! !DESCRIPTION:
!  Takes a long string and reconstruct the phrase/name it represents.
!
! !LOCAL VARIABLES:
      integer                     :: loc, namesLen
      logical                     :: first
      character (len=ESMF_MAXSTR) :: names
!
! !AUTHOR:
!  Jules Kouatchou
!
! !REVISION HISTORY:
!  Initial code.
!
!EOP
!-------------------------------------------------------------------------
!BOC
      first = .true.
      names = phrase

      loc = index(names, '*')
      if (loc == 0) then
         phrase = names
         phrase = adjustl(phrase)
         phrase = trim   (phrase)
      else
         do while(loc > 0)
            if (first) then
               first  = .false.
               phrase = names(1:loc-1)
            else
               phrase = trim(phrase)//' '//names(1:loc-1)
            end if

            phrase = adjustl(phrase)
            phrase = trim   (phrase)
            namesLen = len(names)
            names = names(loc+2:namesLen)
            loc = index(names, '*')
         end do
         phrase = trim(phrase)//' '//names
         phrase = adjustl(phrase)
         phrase = trim   (phrase)
      end if

      return

      end subroutine reconstructPhrase

!EOC
!-------------------------------------------------------------------------

      end module PTRACER
