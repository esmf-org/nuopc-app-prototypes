 !------------------------------------------------------------------------------
#include "MAPL_Generic.h"
!
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: ADVCORE
!
! !DESCRIPTION: 
!    This a MAPL component that can be used in
!    either with offline or online applications to advect an arbitrary set
!    of constituents.
!
! \paragraph{Scientific Description:}
!
!   The advection scheme used is that from the FVdycore grid-point
!   dynamical core.  It runs on a sphere and uses finite-volume
!   discretization techniques. The advection is time split into a
!   horizontal phase that is assumed to be vertically Lagrangian and a
!   vertical remap phase. A complete description of the core from
!   which this component is taken may be found in:
!
!   \begin{quote}
!   Lin, S.-J. 2004, A vertically Lagrangian Finite-Volume Dynamical 
!   Core for Global Models. {\em Mon. Wea. Rev.}, {\bf 132}, 2293-2307.
!   \end{quote}
!
!  \paragraph{Code Implementation:}
!
!    It code uses the MAPL (http://MAPLCode.org/maplwiki/) to
!    encapsulate the FV advection scheme as an ESMF gridded component
!    using the ESMF paradigm of initialize, run and finalize methods,
!    and their SetServices routine. As in all ESMF codes, only
!    SetServices is public and the interface consists of of a Clock
!    and Import and Export states.  The import state includes a
!    specialized description of the motion field in terms of C-grid
!    winds and mass fluxes. These are assumed to have been accumulated
!    over the time interval specified in the resource file. The
!    default of this interval is 1800 seconds. The layer pressure
!    thicknesses in the import state are assumed to be the
!    instantaneous values valid at the beginning of this interval.  If
!    these thicknesses are friendly they will be updated to values
!    valid at the end of the interval, consistent with the given
!    motion field.  Mixing ratios of the constituents to be advected
!    are placed ESMF Fields within an ESMF Bundle in the Import
!    state. Each Field in the Bundle is tested for ``Friendliness'' to
!    advection; if friendly it is advected and its values updated.
!
!    Currently no Export capability is implemented. 
!
! !INTERFACE:

module ADVCORE

!
! !USES:

      use ESMF
      use MAPL_Mod
      use NUOPC
      use NUOPC_Model, &
           model_routine_SS      => SetServices, &
           model_label_Advance   => label_Advance
      use NUOPC_Generic
#if 0
      use m_set_eta,       only: set_eta
      use fv_arrays_mod,   only: fv_atmos_type, FVPRC, REAL4, REAL8
      use fms_mod,         only: fms_init, set_domain, nullify_domain
      use fv_control_mod,  only: fv_init, fv_end
      use fv_tracer2d_mod, only: offline_tracer_advection
      use fv_control_mod,  only: npx,npy,npz,ntiles
      use fv_mp_mod,       only: npes_x, npes_y, is,ie, js,je, gid, masterproc
      use fv_grid_tools_mod, only: globalsum

      USE FV_StateMod,     only: AdvCoreTracers => T_TRACERS
      USE fv_grid_tools_mod, only: gridCellArea => area
#endif
      implicit none
      private

      integer     :: nx, ny
      integer     :: hord_tr, kord_tr
      integer     :: q_split
      logical     :: z_tracer
      logical     :: fill
!      real(FVPRC) :: dt
      logical     :: FV3_DynCoreIsRunning=.false.
      integer     :: AdvCore_Advection=1
      logical     :: chk_mass=.false.

      integer,  parameter :: ntiles_per_pe = 1
!      type(fv_atmos_type), save :: FV_Atm(ntiles_per_pe)

! Tracer I/O History stuff
! -------------------------------------
      integer, parameter         :: ntracers=9
      integer                    :: ntracer
      character(len=ESMF_MAXSTR) :: myTracer
      character(len=ESMF_MAXSTR) :: tMassStr
      real(ESMF_KIND_R8), SAVE          :: TMASS0(ntracers)
      real(ESMF_KIND_R8), SAVE          ::  MASS0
      logical    , SAVE          :: firstRun=.true.

! !PUBLIC MEMBER FUNCTIONS:

      public SetServices

!EOP

!------------------------------------------------------------------------------
contains
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: SetServices - Externally visible registration routine
!
! !INTERFACE:
!
      subroutine SetServices(GC, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: GC
      integer, optional,   intent(  out) :: RC
!
! !DESCRIPTION:
!
!     User-supplied setservices routine.
!     The register routine sets the subroutines to be called
!     as the init, run, and finalize routines.  Note that those are
!     private to the module.
!
!EOP

      character(len=ESMF_MAXSTR)              :: IAm
      character(len=ESMF_MAXSTR)              :: COMP_NAME
      character(len=ESMF_MAXSTR)              :: DYCORE
      CHARACTER(LEN=ESMF_MAXSTR)              :: rcfilen = 'fvcore_layout.rc'
      type(ESMF_Config)                       :: configFile
      type (mystates_WRAP)                    :: mystates_ptr
      type (my_States), pointer               :: mystates

!=============================================================================

! Begin...

      ! Get my name and set-up traceback handle
      ! ---------------------------------------
    
      call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=rc )
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      Iam = trim(COMP_NAME) // '::SetServices'

      call ESMF_LogWrite(Iam, ESMF_LOGMSG_INFO, rc=rc)      

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

     call NUOPC_CompSpecialize(GC, specLabel=model_label_Advance, &
       specRoutine=ModelAdvance, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

!BOS

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

     type (mystates_WRAP)         :: mystates_ptr
     type(MAPL_VarSpec), pointer  :: importSpec(:)
     type(MAPL_VarSpec), pointer  :: exportSpec(:)
     character(len=ESMF_MAXSTR)   :: short_name
     character(len=ESMF_MAXSTR)   :: long_name
     character(len=ESMF_MAXSTR)   :: units
     integer                      :: mytype
     integer                      :: i
         
! !IMPORT STATE:
!
    call ESMF_LogWrite("ADVCORE:InitAdvertise", ESMF_LOGMSG_INFO, rc=rc)      

    call NUOPC_AddImportSpec ( gc,                                  &
         SHORT_NAME = 'MFX',                                       &
         LONG_NAME  = 'pressure_weighted_eastward_mass_flux',      &
         UNITS      = 'Pa m+2 s-1',                                &
         PRECISION  = ESMF_KIND_R8,                                &
         DIMS       = MAPL_DimsHorzVert,                           &
         VLOCATION  = MAPL_VLocationCenter,             RC=RC  )
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

    call NUOPC_AddImportSpec ( gc,                                  &
         SHORT_NAME = 'MFY',                                       &
         LONG_NAME  = 'pressure_weighted_northward_mass_flux',     &
         UNITS      = 'Pa m+2 s-1',                                &
         PRECISION  = ESMF_KIND_R8,                                &
         DIMS       = MAPL_DimsHorzVert,                           &
         VLOCATION  = MAPL_VLocationCenter,             RC=RC  )
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

    call NUOPC_AddImportSpec ( gc,                                  &
         SHORT_NAME = 'CX',                                        &
         LONG_NAME  = 'eastward_accumulated_courant_number',       &
         UNITS      = '1',                                          &
         PRECISION  = ESMF_KIND_R8,                                &
         DIMS       = MAPL_DimsHorzVert,                           &
         VLOCATION  = MAPL_VLocationCenter,             RC=RC  )
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

    call NUOPC_AddImportSpec ( gc,                                  &
         SHORT_NAME = 'CY',                                        &
         LONG_NAME  = 'northward_accumulated_courant_number',      &
         UNITS      = '1',                                          &
         PRECISION  = ESMF_KIND_R8,                                &
         DIMS       = MAPL_DimsHorzVert,                           &
         VLOCATION  = MAPL_VLocationCenter,             RC=RC  )
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

    call NUOPC_AddImportSpec ( gc,                                  &
         SHORT_NAME = 'PLE0',                                      &
         LONG_NAME  = 'pressure_at_layer_edges_before_advection',  &
         UNITS      = 'Pa',                                        &
         PRECISION  = ESMF_KIND_R8,                                &
         DIMS       = MAPL_DimsHorzVert,                           &
         VLOCATION  = MAPL_VLocationEdge,             RC=RC  )
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

    call NUOPC_AddImportSpec ( gc,                                  &
         SHORT_NAME = 'PLE1',                                      &
         LONG_NAME  = 'pressure_at_layer_edges_after_advection',   &                
         UNITS      = 'Pa',                                        &
         PRECISION  = ESMF_KIND_R8,                                &
         DIMS       = MAPL_DimsHorzVert,                           &
         VLOCATION  = MAPL_VLocationEdge,             RC=RC  )
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

    call NUOPC_AddImportSpec(GC,                                  &
       SHORT_NAME         = 'TRADV',                             &
       LONG_NAME          = 'advected_quantities',               &
       units              = 'X',                                 &
       DIMS               = MAPL_DimsHorzVert,                   &
       VLOCATION          = MAPL_VLocationCenter,                &
!       DATATYPE           = MAPL_BundleItem,                     &
                                                      RC=RC  )
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
     !print *, 'ADVCORE number of import fields: ', size(importSpec)
    do i=1,size(importSpec)
        call MAPL_VarSpecGet(importSpec(i), SHORT_NAME=short_name, LONG_NAME=long_name, &
             STAT=mytype, UNITS=units, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out

       !!! Need to do something different if mytype is MAPL_BundleItem, Nothing has been added into
       !!! the bundle yet.  Ignore it for now
       !!! if (mytype .ne. MAPL_BundleItem) then
        call NUOPC_Advertise(IMPORT, &
           StandardName=long_name, name=short_name, units=units, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
        !print *, 'ADVCORE: advertise import field ', long_name
       !!! endif
     end do

! !EXPORT STATE:

     call NUOPC_AddExportSpec ( gc,                                  &
          SHORT_NAME = 'AREA',                                      &
          LONG_NAME  = 'agrid_cell_area',                           &
          UNITS      = 'm+2'  ,                                     &
          DIMS       = MAPL_DimsHorzOnly,                           &
          VLOCATION  = MAPL_VLocationNone,               RC=RC  )
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

! 3D Tracers
     do ntracer=1,ntracers
        write(myTracer, "('TEST_TRACER',i1.1)") ntracer-1
        call NUOPC_AddExportSpec ( gc,                             &
             SHORT_NAME = TRIM(myTracer),                         &
             LONG_NAME  = TRIM(myTracer),                         &
             UNITS      = '1',                                    &
             DIMS       = MAPL_DimsHorzVert,                      &
             VLOCATION  = MAPL_VLocationCenter,               RC=RC  )
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out
     enddo

     !Advertize the import fields
     exportSpec => mystates_ptr%ptr%exportSpec
     !print *, 'ADVCORE number of export fields: ', size(exportSpec)

     do i=1,size(exportSpec)
        call MAPL_VarSpecGet(exportSpec(i), SHORT_NAME=short_name, LONG_NAME=long_name, &
             STAT=mytype, UNITS=units, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out

       !!! Need to do something different if mytype is MAPL_BundleItem, Nothing has been added into
       !!! the bundle yet.  Ignore it for now
       !!! if (mytype .ne. MAPL_BundleItem) then
        call NUOPC_Advertise(EXPORT, &
           StandardName=long_name, name=short_name, units=units, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
        !print *, 'ADVCORE: advertise export field ', long_name
       !!! endif
     end do

!EOS

#if 0
      ! Set the Profiling timers
      !-------------------------
      call MAPL_TimerAdd(GC,    name="INITIALIZE"  ,RC=RC)
      VERIFY_(STATUS)
      call MAPL_TimerAdd(GC,    name="RUN"         ,RC=RC)
      VERIFY_(STATUS)
      call MAPL_TimerAdd(GC,    name="FINALIZE"    ,RC=RC)
      VERIFY_(STATUS)
      call MAPL_TimerAdd(GC,    name="TOTAL"       ,RC=RC)
      VERIFY_(STATUS)

      ! Ending with a Generic SetServices call is a MAPL requirement 
      !-------------------------------------------------------------
      call MAPL_GenericSetServices    ( GC, rc=STATUS)
      VERIFY_(STATUS)
#endif

      RC=ESMF_SUCCESS

      RETURN

      end subroutine InitAdvertise

!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: InitRealize - initialization routine
!
! !INTERFACE:
!
  subroutine InitRealize(GC, IMPORT, EXPORT, CLOCK, RC)
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
!     This initialization routine creates the import and export states,
!     as well as the internal state, which is attached to the component.
!     It also determines the distribution (and therefore the grid) 
!     and performs allocations of persistent data, 
!
!EOP
!=============================================================================
!BOC

      character(len=ESMF_MAXSTR)         :: IAm
      integer                            :: STATUS
      character(len=ESMF_MAXSTR)         :: COMP_NAME
      type(ESMF_Config)                  :: CF
      type (MAPL_MetaComp),      pointer :: MAPL
      character (len=ESMF_MAXSTR)        :: LAYOUT_FILE
      type (ESMF_VM)                     :: VM
      integer                            :: ndt, comm
      real, pointer                      :: temp2d(:,:)
      integer                            :: IS, IE, JS, JE
      integer                            :: dt
      type(ESMF_Grid)               :: esmfGrid
      type(mystates_wrap)           :: mystates_ptr
      type(MAPL_VarSpec), pointer   :: importSpec(:)
      type(MAPL_VarSpec), pointer   :: exportSpec(:)

! Begin... 

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

      Iam = "::InitRealize"

      !  Get my name and set-up traceback handle
      !  ---------------------------------------
      call ESMF_GridCompGet( GC, NAME=COMP_NAME, GRID=esmfGrid, RC=rc )
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      Iam = trim(COMP_NAME) // trim(Iam)

     call ESMF_LogWrite(Iam, ESMF_LOGMSG_INFO, rc=rc)      

      !call MAPL_TimerOn(ggSTATE,"TOTAL")
      !call MAPL_TimerOn(ggSTATE,"INITIALIZE")

      !Get VarSpec info
      call ESMF_UserCompGetInternalState(GC, "MAPL_VarSpec", mystates_ptr, rc) 
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      importSpec => mystates_ptr%ptr%importSpec
      exportSpec => mystates_ptr%ptr%exportSpec
 
      ! Create the fields in the import and export state
      !---------------------------------
      ! realize connected Fields in the importState
      call realizeConnectedFields(IMPORT, importSpec, esmfGrid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! realize connected Fields in the importState
      call realizeConnectedFields(EXPORT, exportSpec, esmfGrid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

#if 0
      call MAPL_TimerOn(MAPL,"TOTAL")
      call MAPL_TimerOn(MAPL,"INITIALIZE")
#endif

      ! Get the time-step
      ! -----------------------
      ! call MAPL_GetResource( MAPL, ndt, 'RUN_DT:', default=0, RC=STATUS )
      call ESMF_AttributeGet ( GC, "RUN_DT:", dt, RC=STATUS )
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

#if 0
      ! Check if AdvCore is running without FV3_DynCoreIsRunning, if yes then setup the MAPL Grid 
      ! ----------------------------------------------------------------------------

      call MAPL_GetObjectFromGC (GC, MAPL,  RC=STATUS )
      VERIFY_(STATUS)
      call MAPL_GetResource(MAPL, DYCORE, 'DYCORE:', default="", RC=STATUS )
      VERIFY_(STATUS)
      call MAPL_GetResource(MAPL, AdvCore_Advection , label='AdvCore_Advection:', &
                                  default=AdvCore_Advection, RC=STATUS )
      VERIFY_(STATUS)
      if(adjustl(DYCORE)=="FV3") FV3_DynCoreIsRunning = .true.
      if (.NOT. FV3_DynCoreIsRunning) then
         call MAPL_GridCreate(GC, rc=status)
         VERIFY_(STATUS)
      endif

      ! Get Domain decomposition
      !-------------------------
      call ESMF_ConfigGetAttribute( CF, npes_x, label='npes_x:', RC=STATUS )
      if (STATUS /= ESMF_SUCCESS) then
          call MAPL_GetResource( MAPL, nx, 'NX:', default=0, RC=STATUS )

          VERIFY_(STATUS)
          npes_x = nx
      endif
      call ESMF_ConfigGetAttribute( CF, npes_y, label='npes_y:', RC=STATUS )
      if (STATUS /= ESMF_SUCCESS) then
          call MAPL_GetResource( MAPL, ny, 'NY:', default=0, RC=STATUS )
          VERIFY_(STATUS)
          npes_y = ny / 6
          ASSERT_( 6*npes_y == ny )
      endif

      ! Get Resolution Information
      !---------------------------
      call ESMF_ConfigGetAttribute( CF, npx, label='npx:', RC=STATUS )
      if (STATUS /= ESMF_SUCCESS) then
          call MAPL_GetResource( MAPL, npx, 'AGCM_IM:', default=32, RC=STATUS )
          VERIFY_(STATUS)
      endif
      ! FV likes npx;npy in terms of cell vertices
      npx=npx+1
      call ESMF_ConfigGetAttribute( CF, npy, label='npy:', RC=STATUS )
      if (STATUS /= ESMF_SUCCESS) then
          call MAPL_GetResource( MAPL, npy, 'AGCM_IM:', default=32, RC=STATUS )
          VERIFY_(STATUS)
      endif
      ! FV likes npx;npy in terms of cell vertices
      npy=npy+1
      call ESMF_ConfigGetAttribute( CF, npz, label='npz:', RC=STATUS )
      if (STATUS /= ESMF_SUCCESS) then
          call MAPL_GetResource( MAPL, npz, 'AGCM_LM:', default=72, RC=STATUS )
          VERIFY_(STATUS)
      endif
      ! Check for cubed grid tiles=6
      call ESMF_ConfigGetAttribute( CF, ntiles, label='ntiles:', default=6, RC=STATUS )
      VERIFY_(STATUS)

      ! Get tracer advection and remapping schemes
      !-------------------------------------------
      call ESMF_ConfigGetAttribute( CF, hord_tr , label='hord_tr:' , default=12, rc = STATUS )
      VERIFY_(STATUS)
      call ESMF_ConfigGetAttribute( CF, kord_tr , label='kord_tr:' , default=8, rc = STATUS )
      VERIFY_(STATUS)
      call ESMF_ConfigGetAttribute( CF, q_split , label='qsplit:'  , default=0, rc = STATUS )
      VERIFY_(STATUS)
      call ESMF_ConfigGetAttribute( CF, z_tracer, label='z_tracer:', default=.false., rc = STATUS )
      VERIFY_(STATUS)
      call ESMF_ConfigGetAttribute( CF, fill    , label='fill:'    , default=.false., rc = STATUS )
      VERIFY_(STATUS)
      call ESMF_ConfigGetAttribute( CF, chk_mass, label='chk_mass:', default=.false., rc = STATUS )
      VERIFY_(STATUS)
#endif

#if 0
      ! Start up FMS/MPP
      !-------------------------------------------
      call ESMF_VMGet(VM,mpiCommunicator=comm,rc=STATUS)
      VERIFY_(STATUS)
      call fms_init(comm)
      VERIFY_(STATUS)
      ! Start up FV if AdvCore is running without FV3_DynCoreIsRunning
      !--------------------------------------------------
      if (.NOT. FV3_DynCoreIsRunning) then
         call fv_init(FV_Atm, dt)
      endif

      ! Compute Grid-Cell Area
      ! ----------------------
      if (.NOT. FV3_DynCoreIsRunning) then
         IS = FV_Atm(1)%isc
         IE = FV_Atm(1)%iec
         JS = FV_Atm(1)%jsc
         JE = FV_Atm(1)%jec

         call MAPL_GetPointer(EXPORT, temp2d, 'AREA', ALLOC=.TRUE., rc=status)
         VERIFY_(STATUS)
         temp2d = gridCellArea(IS:IE,JS:JE)
      endif

      call MAPL_TimerOff(MAPL,"INITIALIZE")
      call MAPL_TimerOff(MAPL,"TOTAL")
#endif

      if ( MAPL_am_I_root() ) then
         print *,  trim(Iam)//": IMPORT State" 
                                 call ESMF_StatePrint ( IMPORT)
!         print *,  trim(Iam)//": INTERNAL State" 
!                                 call ESMF_StatePrint ( WRAP )
         print *,  trim(Iam)//": EXPORT State" 
                                 call ESMF_StatePrint ( EXPORT )
      end if

      RETURN_(ESMF_SUCCESS)

    contains  !--------------------------------------------------------

    subroutine realizeConnectedFields(state, spec, grid, rc)
      ! TODO: this method may move into the NUOPC_ utility layer
      type(ESMF_State)                :: state
      type(MAPL_VarSpec), pointer     :: spec(:)
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
          ! print *, 'ADVCORE Realize field ', name
        else
          ! remove a not connected Field from State
          call ESMF_StateRemove(state, (/name/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          print *, 'ADVCORE remove field ', name
        endif
      enddo

    end subroutine realizeConnectedFields

  end subroutine initRealize

!EOC
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Run - run routine
!
! !INTERFACE:
!
      subroutine modelAdvance(GC, RC)
!
      type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
      integer,             intent(  out) :: RC     ! Error code
!
! !DESCRIPTION:
! 
! The Run method advanced the advection one long time step, as
! specified in the configuration.  This may be broken down int a
! number of internal, small steps, also configurable.
!
!EOP
!=============================================================================
!BOC
! !LOCAL VARIABLES:
      character(len=ESMF_MAXSTR)    :: IAm
      integer                       :: STATUS
      character(len=ESMF_MAXSTR)    :: COMP_NAME
      type (ESMF_Grid)              :: ESMFGRID
      type (MAPL_MetaComp), pointer :: MAPL
      type (ESMF_Alarm)             :: ALARM
      type(ESMF_Config)             :: CF          ! Universal Config 

#if 0
! Imports
      REAL(ESMF_KIND_R8), POINTER, DIMENSION(:,:,:)   :: CX
      REAL(ESMF_KIND_R8), POINTER, DIMENSION(:,:,:)   :: CY
      REAL(ESMF_KIND_R8), POINTER, DIMENSION(:,:,:)   :: MFX
      REAL(ESMF_KIND_R8), POINTER, DIMENSION(:,:,:)   :: MFY
      REAL(ESMF_KIND_R8), POINTER, DIMENSION(:,:,:)   :: PLE0
      REAL(ESMF_KIND_R8), POINTER, DIMENSION(:,:,:)   :: PLE1

! Locals
      REAL(ESMF_KIND_R8), POINTER, DIMENSION(:)       :: AK
      REAL(ESMF_KIND_R8), POINTER, DIMENSION(:)       :: BK
      REAL(ESMF_KIND_R8), POINTER, DIMENSION(:,:,:,:) :: TRACERS
      REAL(ESMF_KIND_R8) :: tmassL, MASSfac, MASS1, TMASS1(ntracers)
      TYPE(AdvCoreTracers), POINTER :: advTracers(:)
      type(ESMF_FieldBundle) :: TRADV
      type(ESMF_Field)       :: field
      type(ESMF_Array)       :: array
      INTEGER :: IM, JM, LM, N, NQ, I,J,L, LS
      REAL(ESMF_KIND_R8) :: PTOP, PINT
! Temporaries for exports/tracers
      REAL, POINTER :: temp3D(:,:,:)
      REAL, POINTER :: ptArray3D(:,:,:)
      real(REAL4),        pointer     :: tracer_r4 (:,:,:)
      real(ESMF_KIND_R8),        pointer     :: tracer_r8 (:,:,:)
      character(len=ESMF_MAXSTR)    :: fieldName
      type(ESMF_TypeKind_Flag)      :: kind

! Get my name and set-up traceback handle
! ---------------------------------------

      Iam = 'Run'
      call ESMF_GridCompGet( GC, name=COMP_NAME, grid=ESMFGRID, RC=STATUS )
      VERIFY_(STATUS)
      Iam = trim(COMP_NAME) // Iam

!WMP  if (AdvCore_Advection>0) then

! Get parameters from generic state.
!-----------------------------------
      call MAPL_GetObjectFromGC ( GC, MAPL, RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_Get( MAPL, IM=IM, JM=JM, LM=LM,   &
                                RUNALARM = ALARM, &
                                      RC = STATUS )
      VERIFY_(STATUS)

      call MAPL_TimerOn(MAPL,"TOTAL")
      call MAPL_TimerOn(MAPL,"RUN")

! Get AKs and BKs for vertical grid
!----------------------------------
      AllOCATE( AK(LM+1) ,stat=STATUS )
      VERIFY_(STATUS)
      AllOCATE( BK(LM+1) ,stat=STATUS )
      VERIFY_(STATUS)
      call set_eta(LM,LS,PTOP,PINT,AK,BK)

      CALL MAPL_GetPointer(IMPORT, PLE0, 'PLE0', ALLOC = .TRUE., RC=STATUS)
      VERIFY_(STATUS)
      CALL MAPL_GetPointer(IMPORT, PLE1, 'PLE1', ALLOC = .TRUE., RC=STATUS)
      VERIFY_(STATUS)
      CALL MAPL_GetPointer(IMPORT, MFX,   'MFX', ALLOC = .TRUE., RC=STATUS)
      VERIFY_(STATUS)
      CALL MAPL_GetPointer(IMPORT, MFY,   'MFY', ALLOC = .TRUE., RC=STATUS)
      VERIFY_(STATUS)
      CALL MAPL_GetPointer(IMPORT, CX,     'CX', ALLOC = .TRUE., RC=STATUS)
      VERIFY_(STATUS)
      CALL MAPL_GetPointer(IMPORT, CY,     'CY', ALLOC = .TRUE., RC=STATUS)
      VERIFY_(STATUS)

      ! The quantities to be advected come as friendlies in a bundle
      !  in the import state.
      !--------------------------------------------------------------

      call ESMF_StateGet(IMPORT, "TRADV", TRADV, rc=STATUS)
      VERIFY_(STATUS)
      call ESMF_FieldBundleGet(TRADV, fieldCount=NQ,    rc=STATUS)
      VERIFY_(STATUS)

      if (NQ > 0) then
         ! We allocate a list of tracers big enough to hold all items in the bundle
         !-------------------------------------------------------------------------
         ALLOCATE( TRACERS(IM,JM,LM,NQ),stat=STATUS )
         VERIFY_(STATUS)
         ALLOCATE( advTracers(NQ),stat=STATUS )
         VERIFY_(STATUS)

         ! Go through the bundle copying the friendlies into the tracer list.
         !-------------------------------------------------------------------------
         do N=1,NQ
            call ESMF_FieldBundleGet (TRADV, fieldIndex=N, field=FIELD, RC=STATUS)
            VERIFY_(STATUS)
            call ESMF_FieldGet  (field, array=array, name=fieldName, RC=STATUS)
            VERIFY_(STATUS)
            call ESMF_ArrayGet(array,typekind=kind, rc=status )
            VERIFY_(STATUS)
            advTracers(N)%is_r4 = (kind == ESMF_TYPEKIND_R4)   ! Is real*4?
            advTracers(N)%tName = fieldName

            if (advTracers(N)%is_r4) then
               call ESMF_ArrayGet(array,farrayptr=tracer_r4, rc=status )
               VERIFY_(STATUS)
               advTracers(N)%content_r4 => tracer_r4
               TRACERS(:,:,:,N) = advTracers(N)%content_r4
            else
               call ESMF_ArrayGet(array,farrayptr=tracer_r8, rc=status )
               VERIFY_(STATUS)
               advTracers(N)%content => tracer_r8
               TRACERS(:,:,:,N) = advTracers(N)%content
            end if
         end do
         if (chk_mass) then
        ! Check Mass conservation
            if (firstRun .and. AdvCore_Advection>0) then
               MASS0 = globalsum(PLE0(:,:,LM), npx, npy, is,ie, js,je)
               call global_integral(TMASS0, TRACERS, PLE0, IM,JM,LM,NQ)
               if (MASS0 /= 0.0) TMASS0=TMASS0/MASS0
            elseif (firstRun) then
               MASS0 = globalsum(PLE1(:,:,LM), npx, npy, is,ie, js,je)
               call global_integral(TMASS0, TRACERS, PLE1, IM,JM,LM,NQ)
               if (MASS0 /= 0.0) TMASS0=TMASS0/MASS0
            endif
         endif
         firstRun=.false.

         ! Run FV3 advection
         !------------------
         if (AdvCore_Advection>0) then
         call WRITE_PARALLEL("offline_tracer_advection")
         call offline_tracer_advection(TRACERS, PLE0, PLE1, MFX, MFY, CX, CY, AK, BK, PTOP, npx, npy, npz,   &
                                       NQ, hord_tr, kord_tr, q_split, dt, z_tracer, fill)
         endif

         ! Update tracer mass conservation
         !-------------------------------------------------------------------------
         if (chk_mass) then 
            MASS1 = globalsum(PLE1(:,:,LM), npx, npy, is,ie, js,je)
            call global_integral(TMASS1, TRACERS, PLE1, IM,JM,LM,NQ)
            if (MASS1 /= 0.0) TMASS1=TMASS1/MASS1
         endif

         if (chk_mass .and. gid==masterproc) then
#ifdef PRINT_MASS
            write(6,100)  MASS0   , &
                         TMASS0(2), &
                         TMASS0(3), &
                         TMASS0(4), &
                         TMASS0(5)
            write(6,102)  MASS1   , &
                         TMASS1(2), &
                         TMASS1(3), &
                         TMASS1(4), &
                         TMASS1(5)
#endif
            write(6,103) ( MASS1   - MASS0   )/ MASS0   , &
                         (TMASS1(2)-TMASS0(2))/TMASS0(2), &
                         (TMASS1(3)-TMASS0(3))/TMASS0(3), &
                         (TMASS1(4)-TMASS0(4))/TMASS0(4), &
                         (TMASS1(5)-TMASS0(5))/TMASS0(5)
 100        format('Tracer M0  : ',e21.14,' ',e21.14,' ',e21.14,' ',e21.14,' ',e21.14)
 101        format('Tracer Ma  : ',e21.14,' ',e21.14,' ',e21.14,' ',e21.14,' ',e21.14)
 102        format('Tracer M1  : ',e21.14,' ',e21.14,' ',e21.14,' ',e21.14,' ',e21.14)
 103        format('Tracer Mdif: ',e21.14,' ',e21.14,' ',e21.14,' ',e21.14,' ',e21.14)
         endif

         ! Go through the bundle copying tracers back to the bundle.
         !-------------------------------------------------------------------------
         do N=1,NQ
            if (advTracers(N)%is_r4) then
               advTracers(N)%content_r4 = TRACERS(:,:,:,N)
            else
               advTracers(N)%content    = TRACERS(:,:,:,N)
            end if 
! Fill Export States
            write(myTracer, "('TEST_TRACER',i1.1)") N-1
            call MAPL_GetPointer(EXPORT, temp3D, TRIM(myTracer), rc=status)
            VERIFY_(STATUS)
            if ((associated(temp3D)) .and. (N<=ntracers)) then
               temp3D = TRACERS(:,:,:,N)
            endif
         enddo

         ! Deallocate the list of tracers
         !-------------------------------------------------------------------------
         DEALLOCATE( TRACERS,stat=STATUS )
         VERIFY_(STATUS)

      end if ! NQ > 0

      deallocate( advTracers, stat=STATUS )
      VERIFY_(STATUS)
      DEALLOCATE( AK ,stat=STATUS )
      VERIFY_(STATUS)
      DEALLOCATE( BK ,stat=STATUS )
      VERIFY_(STATUS)

      call MAPL_TimerOff(MAPL,"RUN")
      call MAPL_TimerOff(MAPL,"TOTAL")

!WMP  end if ! AdvCore_Advection

#endif
      RETURN_(ESMF_SUCCESS)

      end subroutine ModelAdvance
!EOC

#if 0
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Finalize - user supplied finalize routine
!
! !INTERFACE:
!
  subroutine Finalize(GC, IMPORT, EXPORT, CLOCK, RC)
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
!    Finalize merely destroys the FVadv object that was created in Initialize
!    and releases the space for the persistent data .
!
!EOP
!=============================================================================
!BOC
! !LOCAL VARIABLES:

      character(len=ESMF_MAXSTR)    :: IAm
      integer                       :: STATUS
      character(len=ESMF_MAXSTR)    :: COMP_NAME

! Get my name and set-up traceback handle
! ---------------------------------------

      Iam = 'Finalize'
      call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=STATUS )
      VERIFY_(STATUS)
      Iam = trim(COMP_NAME) // TRIM(Iam)

      ! Clean up FV if AdvCore is running without FV3_DynCoreIsRunning
      !--------------------------------------------------
      if (.NOT. FV3_DynCoreIsRunning) then
         call fv_end(FV_Atm)
      endif

      call MAPL_GenericFinalize(GC, IMPORT, EXPORT, CLOCK, RC)
      VERIFY_(STATUS)

      RETURN_(ESMF_SUCCESS)
      end subroutine Finalize

subroutine global_integral (QG,Q,PLE,IM,JM,KM,NQ)

      real(ESMF_KIND_R8), intent(OUT)   :: QG(NQ)
      real(ESMF_KIND_R8), intent(IN)    :: Q(IM,JM,KM,NQ)
      real(ESMF_KIND_R8), intent(IN)    :: PLE(IM,JM,KM+1)
      integer,     intent(IN)    :: IM,JM,KM,NQ
! Locals
      integer   :: k,n
      real(ESMF_KIND_R8), allocatable ::    dp(:,:,:)
      real(ESMF_KIND_R8), allocatable :: qsum1(:,:)

      allocate(    dp(im,jm,km) )
      allocate( qsum1(im,jm)    )

! Compute Pressure Thickness
! --------------------------
      do k=1,KM
         dp(:,:,k) = PLE(:,:,k+1)-PLE(:,:,k)
      enddo

! Loop over Tracers
! -----------------
     do n=1,NQ
        qsum1(:,:) = 0.d0
        do k=1,KM
           qsum1(:,:) = qsum1(:,:) + Q(:,:,k,n)*dp(:,:,k)
        enddo
        qg(n) = globalsum(qsum1, npx, npy, is,ie, js,je)
     enddo

     deallocate( dp )
     deallocate( qsum1 )

end subroutine global_integral
#endif

!EOC
!------------------------------------------------------------------------------

end module ADVCORE
