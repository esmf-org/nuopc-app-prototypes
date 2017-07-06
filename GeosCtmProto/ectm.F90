#include "MAPL_Generic.h"
!-------------------------------------------------------------------------
!         NASA/GSFC, Software Systems Support Office, Code 610.3         !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE: ENVCTM -- Prepares derived variables for GEOSctm
!
! !INTERFACE:
!
      module ENVCTM
!
! !USES:
      use ESMF
      use MAPL_mod
      use NUOPC
      use NUOPC_Model, &
           model_routine_SS      => SetServices, &
           model_label_Advance   => label_Advance
      use NUOPC_Generic  
      implicit none
      private

! !PUBLIC MEMBER FUNCTIONS:

      public SetServices
      public compAreaWeightedAverage

      interface compAreaWeightedAverage
         module procedure compAreaWeightedAverage_2d
         module procedure compAreaWeightedAverage_3d
      end interface

!
! !DESCRIPTION:
! This GC is used to derive variables needed by the CTM GC children.
!
! !AUTHORS:
! Jules.Kouatchou-1@nasa.gov
!
!EOP
!-------------------------------------------------------------------------
      integer,  parameter :: r8     = 8
      integer,  parameter :: r4     = 4

      INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(6,30)
      INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(14,300)
      INTEGER, PARAMETER :: qp = SELECTED_REAL_KIND(18,400)

      real(r8), parameter :: RADIUS = MAPL_RADIUS
      real(r8), parameter :: PI     = MAPL_PI_R8
      real(r8), parameter :: D0_0   = 0.0_r8
      real(r8), parameter :: D0_5   = 0.5_r8
      real(r8), parameter :: D1_0   = 1.0_r8
      real(r8), parameter :: GPKG   = 1000.0d0
      real(r8), parameter :: MWTAIR =   28.96d0
      real(r8), parameter :: SecondsPerMinute = 60.0d0

      logical             :: enable_pTracers  = .FALSE.
      character(len=ESMF_MAXSTR) :: metType ! MERRA2 or MERRA1 or FPIT

!-------------------------------------------------------------------------
      CONTAINS
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: SetServices -- Sets ESMF services for this component
!
! !INTERFACE:
!
      subroutine SetServices ( GC, RC )
!
! !INPUT/OUTPUT PARAMETERS:
      type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component
!
! !OUTPUT PARAMETERS:
      integer, intent(OUT)               :: RC  ! return code
!
! !DESCRIPTION:  
!   The SetServices for the CTM Der GC needs to register its
!   Initialize and Run.  
!
!EOP
!-------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
      integer                    :: STATUS
      integer                    :: I
      type (ESMF_Config)         :: configFile
      character(len=ESMF_MAXSTR) :: COMP_NAME
      character(len=ESMF_MAXSTR) :: IAm = 'SetServices'
      type (mystates_WRAP)        :: mystates_ptr
      type (my_States), pointer   :: mystates

      rc = ESMF_SUCCESS

     ! Get my name and set-up traceback handle
     ! ---------------------------------------
      call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=rc )
      VERIFY_(STATUS)
      Iam = trim(COMP_NAME) // TRIM(Iam)

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
     ! set entry point for methods that require specific implementation
     call NUOPC_CompSetEntryPoint(GC, ESMF_METHOD_INITIALIZE, &
        phaseLabelList=(/"IPDv00p1"/), userRoutine=InitAdvertise, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
     call NUOPC_CompSetEntryPoint(GC, ESMF_METHOD_INITIALIZE, &
        phaseLabelList=(/"IPDv00p2"/), userRoutine=InitRealize, rc=rc)
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

  subroutine InitAdvertise(GC, importState, exportState, clock, rc)
     type(ESMF_GridComp)  :: GC
     type(ESMF_State)     :: importState, exportState
     type(ESMF_Clock)     :: clock
     integer, intent(out) :: rc
    

     CHARACTER(LEN=ESMF_MAXSTR)  :: rcfilen = 'CTM_GridComp.rc'
     character(len=ESMF_MAXSTR)  :: IAm = 'SetServices'
     type(ESMF_Config)           :: configFile 
     type(mystates_wrap)         :: mystates_ptr
     type(MAPL_VarSpec), pointer :: importSpec(:), exportSpec(:)
     character(len=ESMF_MAXSTR)  :: short_name, long_name
     integer                     :: i

     rc = ESMF_SUCCESS

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

     call ESMF_ConfigGetAttribute(configFile, enable_pTracers,             &
                                     Default  = .FALSE.,                    &
                                     Label    = "ENABLE_pTracers:", rc=rc )

     ! Type of meteological fields (MERRA2 or MERRA1 or FPIT)
     call ESMF_ConfigGetAttribute(configFile, metType,             &
                                     Default  = 'MERRA2',           &
                                     Label    = "metType:", rc=rc )

! !IMPORT STATE:

     call NUOPC_AddImportSpec(GC,                             &
           SHORT_NAME        = 'AREA',                         &
           LONG_NAME         = 'agrid_cell_area',              &
           UNITS             = 'm+2',                          &
           DIMS              = MAPL_DimsHorzOnly,              &
           VLOCATION         = MAPL_VLocationNone,    RC=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      

     call NUOPC_AddImportSpec ( GC,                                  &
           SHORT_NAME = 'PLE0',                                      &
           LONG_NAME  = 'pressure_at_layer_edges_before_advection',  &
           UNITS      = 'Pa',                                        &
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationEdge,             RC=rc  )
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      

     call NUOPC_AddImportSpec ( GC,                                  &
           SHORT_NAME = 'PLE1',                                      &
           LONG_NAME  = 'pressure_at_layer_edges_after_advection',   &
           UNITS      = 'Pa',                                        &
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationEdge,             RC=rc  )
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      

     call NUOPC_AddImportSpec ( GC,                                  &
           SHORT_NAME = 'UC0',                                       &
           LONG_NAME  = 'eastward_wind_on_C-Grid_before_advection',  &
           UNITS      = 'm s-1',                                     &
           STAGGERING = MAPL_CGrid,                                  &
           ROTATION   = MAPL_RotateCube,                             & 
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationCenter,             RC=rc  )
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      

     call NUOPC_AddImportSpec ( GC,                                  &
           SHORT_NAME = 'UC1',                                       &
           LONG_NAME  = 'eastward_wind_on_C-Grid_after_advection',   &
           UNITS      = 'm s-1',                                     &
           STAGGERING = MAPL_CGrid,                                  &
           ROTATION   = MAPL_RotateCube,                             &
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationCenter,             RC=rc  )
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      

     call NUOPC_AddImportSpec ( GC,                                  &
           SHORT_NAME = 'VC0',                                       &
           LONG_NAME  = 'northward_wind_on_C-Grid_before_advection', &
           UNITS      = 'm s-1',                                     &
           STAGGERING = MAPL_CGrid,                                  &
           ROTATION   = MAPL_RotateCube,                             &
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationCenter,             RC=rc  )
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      

     call NUOPC_AddImportSpec ( GC,                                  &
           SHORT_NAME = 'VC1',                                       &
           LONG_NAME  = 'northward_wind_on_C-Grid_after_advection',  &
           UNITS      = 'm s-1',                                     &
           STAGGERING = MAPL_CGrid,                                  &
           ROTATION   = MAPL_RotateCube,                             &
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationCenter,             RC=rc  )
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      
     !Advertize the import fields
     call ESMF_UserCompGetInternalState(GC, "MAPL_VarSpec", mystates_ptr, rc) 
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
     importSpec => mystates_ptr%ptr%importSpec

     do i=1,size(importSpec)
        call MAPL_VarSpecGet(importSpec(i), SHORT_NAME=short_name, LONG_NAME=long_name,rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
        call NUOPC_Advertise(importState, &
           StandardName=long_name, name=short_name, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
     end do
    
! !EXPORT STATE:

     ! Default exports
     !----------------
     call NUOPC_AddExportSpec ( GC,                                  &
           SHORT_NAME = 'CXr8',                                      &
           LONG_NAME  = 'eastward_accumulated_courant_number',       &
           UNITS      = '',                                          &
           PRECISION  = ESMF_KIND_R8,                                &
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationCenter,             RC=rc  )
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      

     call NUOPC_AddExportSpec ( GC,                                  &
           SHORT_NAME = 'CYr8',                                      &
           LONG_NAME  = 'northward_accumulated_courant_number',      &
           UNITS      = '',                                          &
           PRECISION  = ESMF_KIND_R8,                                &
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationCenter,             RC=rc  )
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      

     call NUOPC_AddExportSpec ( GC,                                  &
           SHORT_NAME = 'MFXr8',                                     &
           LONG_NAME  = 'pressure_weighted_accumulated_eastward_mass_flux', &
           UNITS      = 'Pa m+2 s-1',                                &
           PRECISION  = ESMF_KIND_R8,                                &
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationCenter,             RC=rc  )
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      

     call NUOPC_AddExportSpec ( GC,                                  &
           SHORT_NAME = 'MFYr8',                                     &
           LONG_NAME  = 'pressure_weighted_accumulated_northward_mass_flux', &
           UNITS      = 'Pa m+2 s-1',                                &
           PRECISION  = ESMF_KIND_R8,                                &
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationCenter,             RC=rc  )
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      

     call NUOPC_AddExportSpec ( GC,                                  &
           SHORT_NAME = 'PLE1r8',                                    &
           LONG_NAME  = 'pressure_at_layer_edges_after_advection',   &
           UNITS      = 'Pa',                                        &
           PRECISION  = ESMF_KIND_R8,                                &
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationEdge,             RC=rc  )
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      

     call NUOPC_AddExportSpec ( GC,                                  &
           SHORT_NAME = 'PLE0r8',                                    &
           LONG_NAME  = 'pressure_at_layer_edges_before_advection',  &
           UNITS      = 'Pa',                                        &
           PRECISION  = ESMF_KIND_R8,                                &
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationEdge,             RC=rc  )
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      

     call NUOPC_AddExportSpec ( GC,                                  &
           SHORT_NAME = 'PLE',                                       &
           LONG_NAME  = 'pressure_at_layer_edges',                   &
           UNITS      = 'Pa',                                        &
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationEdge,             RC=rc  )
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      

     call NUOPC_AddExportSpec ( GC,                                  &
           SHORT_NAME = 'TH',                                        &
           LONG_NAME  = 'potential_temperature',                     &
           UNITS      = 'K',                                         &
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationCenter,             RC=rc  )
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      

     call NUOPC_AddExportSpec(GC,                               &
           SHORT_NAME         = 'AIRDENS',                      &
           LONG_NAME          = 'air_density',                  &
           UNITS              = 'kg m-3',                       &
           DIMS               = MAPL_DimsHorzVert,              &
           VLOCATION          = MAPL_VLocationCenter,  RC=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      
     call NUOPC_AddExportSpec(GC, &
           SHORT_NAME         = 'LWI',  &
           LONG_NAME          = 'land-ocean-ice_mask',  &
           UNITS              = '1', &
           DIMS               = MAPL_DimsHorzOnly,    &
           VLOCATION          = MAPL_VLocationNone,    &
                                                      RC=rc  )
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      
     call NUOPC_AddExportSpec(GC,                               &
           SHORT_NAME         = 'MASS',                         &
           LONG_NAME          = 'total_mass',                   &
           UNITS              = 'kg',                           &
           DIMS               = MAPL_DimsHorzVert,              &
           VLOCATION          = MAPL_VLocationCenter,  RC=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      
     IF ( TRIM(metType) == 'MERRA1' ) THEN
         call NUOPC_AddExportSpec(GC, &
              SHORT_NAME         = 'RH2',  &
              LONG_NAME          = 'relative_humidity_after_moist',  &
              UNITS              = '1', &
              DIMS               = MAPL_DimsHorzVert,    &
              VLOCATION          = MAPL_VLocationCenter,    &
                                                             RC=rc  )
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
         
     END IF

     call NUOPC_AddExportSpec(GC,                                    &
           SHORT_NAME         = 'ZLE',                               &
           LONG_NAME          = 'geopotential_height',               &
           UNITS              = 'm',                                 &
           DIMS               = MAPL_DimsHorzVert,                   &
           VLOCATION          = MAPL_VLocationEdge,       RC=rc  )
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

     !Advertize the export fields
     exportSpec => mystates_ptr%ptr%exportSpec

     do i=1,size(exportSpec)
        call MAPL_VarSpecGet(exportSpec(i), SHORT_NAME=short_name, LONG_NAME=long_name,rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
        call NUOPC_Advertise(exportState, &
           StandardName=long_name, name=short_name, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
     end do
    

     ! Set the Profiling timers
     !-------------------------
     call MAPL_TimerAdd(GC,    name="INITIALIZE"  ,RC=rc)
      
     call MAPL_TimerAdd(GC,    name="RUN"         ,RC=rc)
      

     ! Create children's gridded components and invoke their SetServices
     ! -----------------------------------------------------------------
     !call MAPL_GenericSetServices    ( GC, RC=rc )
     !VERIFY_(rc)

     RETURN_(ESMF_SUCCESS)
  
     end subroutine initAdvertise
!
!EOC
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: InitRealize -- Initialized method for composite the CTMder
!
! !INTERFACE:
!
  subroutine InitRealize(GC, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: GC
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
!
! !DESCRIPTION: 
!  The Initialize method of the CTM Cinderella Component.
!
!EOP
!-------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
      __Iam__('Initialize')
      character(len=ESMF_MAXSTR)    :: COMP_NAME
      type(ESMF_Grid)               :: esmfGrid
      integer                       :: lm
      type (ESMF_Config)            :: CF
      type(mystates_wrap)           :: mystates_ptr
      type(MAPL_VarSpec), pointer   :: importSpec(:)
      type(MAPL_VarSpec), pointer   :: exportSpec(:)

      !  Get my name and set-up traceback handle
      !  ---------------------------------------
      call ESMF_GridCompGet( GC, NAME=COMP_NAME, CONFIG=CF, GRID=esmfGrid, RC=rc )
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      Iam = TRIM(COMP_NAME)//"::InitRealize"

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
      call realizeConnectedFields(importState, importSpec, esmfGrid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! realize connected Fields in the importState
      call realizeConnectedFields(exportState, exportSpec, esmfGrid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    contains  !--------------------------------------------------------

    subroutine realizeConnectedFields(state, spec, grid, rc)
      ! TODO: this method may move into the NUOPC_ utility layer
      type(ESMF_State)                :: state
      type(MAPL_VarSpec), pointer     :: spec(:)
      type(ESMF_Grid)                 :: grid
      integer, intent(out), optional  :: rc
      ! local variables
      character(len=80)               :: fieldName
      integer                         :: i, itemCount, k
      type(ESMF_Field)                :: field
      real(ESMF_KIND_R8), pointer     :: fptr(:)

      if (present(rc)) rc = ESMF_SUCCESS
      
      itemCount=size(spec)

      k=1 ! initialize
      do i=1, itemCount 
       ! find the VarSpec with matching long_name
       call MAPL_VarSpecGet(spec(i),LONG_NAME=fieldName, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
       call MAPL_VarSpecSet(spec(i), GRID=grid,rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
       if (NUOPC_IsConnected(state, fieldName=fieldName)) then
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
          ! remove a not connected Field from State
          call ESMF_StateRemove(state, (/fieldName/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        endif
      enddo

    end subroutine realizeConnectedFields

  end subroutine initRealize

!EOC
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Run -- Run method
!
! !INTERFACE:
!
      subroutine modelAdvance( GC, RC )
!
! !INPUT/OUTPUT PARAMETERS:
      type(ESMF_GridComp) :: GC     ! Gridded component 
!
! !OUTPUT PARAMETERS:
      integer, intent(  out) :: RC     ! Error code
!
! !DESCRIPTION: 
! The Run method of the CTM Cinderalla Component.
!
!EOP
!-------------------------------------------------------------------------
!BOC 
!
! !LOCAL VARIABLES:
      character(len=ESMF_MAXSTR)      :: IAm = "Run"
      integer                         :: STATUS
      character(len=ESMF_MAXSTR)      :: COMP_NAME
      !type (MAPL_MetaComp), pointer   :: ggState
      type (ESMF_Grid)                :: esmfGrid

#if 0
      ! Imports
      !--------
      real, pointer, dimension(:,:,:) ::      PLE1 => null()
      real, pointer, dimension(:,:,:) ::      PLE0 => null()
      real, pointer, dimension(:,:,:) ::       UC0 => null()
      real, pointer, dimension(:,:,:) ::       UC1 => null()
      real, pointer, dimension(:,:,:) ::       VC0 => null()
      real, pointer, dimension(:,:,:) ::       VC1 => null()
      real, pointer, dimension(:,:,:) ::         T => null()
      real, pointer, dimension(:,:,:) ::         Q => null()
      real, pointer, dimension(:,:,:) ::       ZLE => null()
      real, pointer, dimension(:,:,:) ::     QITOT => null()
      real, pointer, dimension(:,:,:) ::     QLTOT => null()
      real, pointer, dimension(:,:,:) ::    QITOT1 => null()
      real, pointer, dimension(:,:,:) ::    QLTOT1 => null()
      real, pointer, dimension(:,:)   ::  cellArea => null()

      real, pointer, dimension(:,:)   ::        TS => null()
      real, pointer, dimension(:,:)   ::   FROCEAN => null()
      real, pointer, dimension(:,:)   ::   CN_PRCP => null()
      real, pointer, dimension(:,:,:) ::   CNV_MFC => null()
      real, pointer, dimension(:,:)   ::    FRLAKE => null()
      real, pointer, dimension(:,:,:) ::     PFLCU => null()
      real, pointer, dimension(:,:,:) ::     PFICU => null()
      real, pointer, dimension(:,:,:) ::   PFLLSAN => null()
      real, pointer, dimension(:,:,:) ::   PFILSAN => null()

      ! Exports
      !--------
      real,     pointer, dimension(:,:,:) ::        TH => null()
      real,     pointer, dimension(:,:,:) ::       PLE => null()
      real,     pointer, dimension(:,:,:) ::   AIRDENS => null()
      real,     pointer, dimension(:,:,:) ::      MASS => null()
      real(r8), pointer, dimension(:,:,:) ::      CXr8 => null()
      real(r8), pointer, dimension(:,:,:) ::      CYr8 => null()
      real(r8), pointer, dimension(:,:,:) ::    PLE1r8 => null()
      real(r8), pointer, dimension(:,:,:) ::    PLE0r8 => null()
      real(r8), pointer, dimension(:,:,:) ::     MFXr8 => null()
      real(r8), pointer, dimension(:,:,:) ::     MFYr8 => null()

      real(r8), pointer, dimension(:,:,:) ::      UCr8 => null()
      real(r8), pointer, dimension(:,:,:) ::      VCr8 => null()
      real(r8), pointer, dimension(:,:,:) ::     PLEr8 => null()

      real,     pointer, dimension(:,:,:) ::         U => null()
      real,     pointer, dimension(:,:,:) ::         V => null()
      real,     pointer, dimension(:,:,:) ::     QCTOT => null()
      real,     pointer, dimension(:,:,:) ::    CNV_QC => null()

      real,     pointer, dimension(:,:)   ::     FRACI => null()
      real,     pointer, dimension(:,:)   ::       LFR => null()
      real,     pointer, dimension(:,:)   :: flashRate => null()

      real,     pointer, dimension(:,:,:) ::       RH2 => null()
      real,     pointer, dimension(:,:,:) ::    PKAPPA => null()
      real,     pointer, dimension(:,:)   ::  CNV_TOPP => null()
      real,     pointer, dimension(:,:)   ::      CAPE => null()
      real,     pointer, dimension(:,:)   ::       LWI => null()
      real,     pointer, dimension(:,:)   ::       ITY => null()
      real,     pointer, dimension(:,:,:) ::      DQDT => null()
      real,     pointer, dimension(:,:,:) ::      DQRL => null()
      real,     pointer, dimension(:,:,:) ::     BYNCY => null()
      real,     pointer, dimension(:,:,:) ::   totflux => null()
      real,     pointer, dimension(:,:,:) ::    expZLE => null()

      real,     pointer, dimension(:,:,:) ::        QI => null()
      real,     pointer, dimension(:,:,:) ::        QL => null()
      real,     pointer, dimension(:,:,:) ::      QICN => null()
      real,     pointer, dimension(:,:,:) ::      QLCN => null()
      real,     pointer, dimension(:,:)   ::   PRECCON => null()
      real,     pointer, dimension(:,:)   ::   PRECANV => null()
      real,     pointer, dimension(:,:)   ::   PRECLSC => null()
      real,     pointer, dimension(:,:)   ::     TPREC => null()
      real,     pointer, dimension(:,:)   ::  tempFrac => null()

      integer :: km, k, is, ie, js, je, lm, ik, nc
      integer :: ndt, isd, ied, jsd, jed, i, j, l
      real(r8) :: DT

      ! Get the target components name and set-up traceback handle.
      ! -----------------------------------------------------------
      call ESMF_GridCompGet ( GC, name=COMP_NAME, Grid=esmfGrid, RC=STATUS )
      VERIFY_(STATUS)
      Iam = trim(COMP_NAME) // TRIM(Iam)

      ! Get my internal MAPL_Generic state
      !-----------------------------------
      ! call MAPL_GetObjectFromGC ( GC, ggState, __RC__ )

      !call MAPL_TimerOn(ggState,"TOTAL")
      !call MAPL_TimerOn(ggState,"RUN")

      ! Get the time-step
      ! -----------------------
      ! call MAPL_GetResource( ggState, ndt, 'RUN_DT:', default=0, __RC__ )

      call ESMF_AttributeGet ( GC, DT, Label="RUN_DT:", RC=STATUS )
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

      !-----------------------------
      ! Required Imports and Exports
      !-----------------------------
      call MAPL_GetPointer ( IMPORT,     PLE0,  'PLE0', __RC__ )
      call MAPL_GetPointer ( IMPORT,     PLE1,  'PLE1', __RC__ )
      call MAPL_GetPointer ( IMPORT,      UC0,   'UC0', __RC__ )
      call MAPL_GetPointer ( IMPORT,      UC1,   'UC1', __RC__ )
      call MAPL_GetPointer ( IMPORT,      VC0,   'VC0', __RC__ )
      call MAPL_GetPointer ( IMPORT,      VC1,   'VC1', __RC__ )
      call MAPL_GetPointer ( IMPORT, cellArea,  'AREA', __RC__ )

      call MAPL_GetPointer ( EXPORT,     PLE,    'PLE', __RC__ )
      PLE    = PLE0
      call MAPL_GetPointer ( EXPORT,  PLE0r8, 'PLE0r8', __RC__ )
      PLE0r8 = PLE0
      call MAPL_GetPointer ( EXPORT,  PLE1r8, 'PLE1r8', __RC__ )
      PLE1r8 = PLE1
      call MAPL_GetPointer ( EXPORT,   MFXr8,  'MFXr8', __RC__ )
      call MAPL_GetPointer ( EXPORT,   MFYr8,  'MFYr8', __RC__ )
      call MAPL_GetPointer ( EXPORT,    CXr8,   'CXr8', __RC__ )
      call MAPL_GetPointer ( EXPORT,    CYr8,   'CYr8', __RC__ )

      is = lbound(PLE,1); ie = ubound(PLE,1)
      js = lbound(PLE,2); je = ubound(PLE,2)
      LM = size  (PLE,3) - 1
      nc = (ie-is+1)*(je-js+1)

      !--------------------------------------------
      ! courant numbers and mass fluxes for AdvCore
      !--------------------------------------------
      ALLOCATE( UCr8(is:ie,js:je,lm),   STAT=STATUS); VERIFY_(STATUS)
      ALLOCATE( VCr8(is:ie,js:je,lm),   STAT=STATUS); VERIFY_(STATUS)
      ALLOCATE(PLEr8(is:ie,js:je,lm+1), STAT=STATUS); VERIFY_(STATUS)

      UCr8  = 0.50d0*(UC1  + UC0)
      VCr8  = 0.50d0*(VC1  + VC0)
      PLEr8 = 0.50d0*(PLE1 + PLE0)

      call calcCourantNumberMassFlux(UCr8, VCr8, PLEr8, &
                                MFXr8, MFYr8, CXr8, CYr8, DT)
    
      DEALLOCATE(UCr8, VCr8, PLEr8)

      IF (.NOT. enable_pTracers) THEN

         !---------------------------
         ! Potential Temperature (TH)
         !---------------------------
         call MAPL_GetPointer ( IMPORT,      T,     'T', ALLOC=.TRUE., __RC__  )
         call MAPL_GetPointer ( EXPORT,     TH,    'TH', ALLOC=.TRUE., __RC__  )

         ALLOCATE( PKAPPA(is:ie,js:je,LM),   STAT=STATUS); VERIFY_(STATUS)

         PKAPPA(:,:,:) = ((0.5*(PLE(:,:,0:LM-1) +  PLE(:,:,1:LM  ) ))/100000.)**(MAPL_RGAS/MAPL_CP)
         TH(:,:,:) = T(:,:,:)/PKAPPA(:,:,:)

         DEALLOCATE(PKAPPA)

         call MAPL_GetPointer ( IMPORT,      TS,      'TS', __RC__ )
         call MAPL_GetPointer ( IMPORT, FROCEAN, 'FROCEAN', __RC__ )
         call MAPL_GetPointer ( IMPORT,   FRACI,   'FRACI', __RC__ )
         call MAPL_GetPointer ( IMPORT,  FRLAKE,  'FRLAKE', __RC__ )
         call MAPL_GetPointer ( IMPORT,       Q,       'Q', __RC__ )

         !----
         ! LWI
         !----
         call MAPL_GetPointer ( EXPORT,     LWI,     'LWI', ALLOC=.TRUE., __RC__ )
         call computeLWI      (LWI, TS, FRLAKE, FROCEAN, FRACI)

         IF ( (TRIM(metType) == 'MERRA2') .OR. (TRIM(metType) == 'FPIT') ) THEN
            call MAPL_GetPointer ( IMPORT,    ZLE,      'ZLE', __RC__ )
            call MAPL_GetPointer ( EXPORT, expZLE,      'ZLE', ALLOC=.TRUE., __RC__ )
            expZLE = ZLE

         ELSEIF ( TRIM(metType) == 'MERRA1') THEN
            !---------------------------------
            ! RH2 and ZLE if using MERRA1 data
            !---------------------------------
            call MAPL_GetPointer ( EXPORT,    RH2,    'RH2', ALLOC=.TRUE., __RC__ )
            call MAPL_GetPointer ( EXPORT,    ZLE,    'ZLE', ALLOC=.TRUE., __RC__ )

            call compute_ZLE_RH2 (ZLE, RH2, TH, Q, PLE, ie-is+1, je-js+1, LM)
         END IF

         !---------------------
         ! Derive QICN and QLCN
         !---------------------
         call MAPL_GetPointer ( IMPORT,  PRECCON, 'PRECCON', __RC__ )
         call MAPL_GetPointer ( IMPORT,  PRECANV, 'PRECANV', __RC__ )
         call MAPL_GetPointer ( IMPORT,  PRECLSC, 'PRECLSC', __RC__ )
         call MAPL_GetPointer ( IMPORT,       QI,      'QI', __RC__ )
         call MAPL_GetPointer ( IMPORT,       QL,      'QL', __RC__ )
         call MAPL_GetPointer ( EXPORT,     QICN,    'QICN', ALLOC=.TRUE., __RC__ )
         call MAPL_GetPointer ( EXPORT,     QLCN,    'QLCN', ALLOC=.TRUE., __RC__ )

         isd = lbound(QLCN,1); ied = ubound(QLCN,1)
         jsd = lbound(QLCN,2); jed = ubound(QLCN,2)

         ALLOCATE( tempFrac(isd:ied,jsd:jed), STAT=STATUS); VERIFY_(STATUS)
         ALLOCATE(    TPREC(isd:ied,jsd:jed), STAT=STATUS); VERIFY_(STATUS)

         call computeTotalPrecip(TPREC, PRECANV, PRECCON, PRECLSC)

         tempFrac(:,:) = 0.0
         WHERE (TPREC(:,:) .NE. 0.0) 
             tempFrac(:,:) = PRECANV(:,:) /TPREC(:,:)
         END WHERE

         DO k=1,LM
            QICN(:,:,k) = QI(:,:,k)*tempFrac(:,:)
            QLCN(:,:,k) = QL(:,:,k)*tempFrac(:,:)
         ENDDO
         DEALLOCATE(tempFrac, TPREC)

         ! ---------------------------------
         ! Air Desnsity and Atmospheric Mass
         ! ---------------------------------
         call MAPL_GetPointer ( EXPORT, AIRDENS, 'AIRDENS', ALLOC=.TRUE., __RC__ )
         call MAPL_GetPointer ( EXPORT,    MASS,    'MASS', ALLOC=.TRUE., __RC__ )

         ! Compute air density
         call airdens_ ( AIRDENS, PLE, TH, Q, ie-is+1, je-js+1, LM)

         ! Compute the total mass
         DO k = 1, LM
            MASS(:,:,k) = AIRDENS(:,:,k)*cellArea(:,:)*(ZLE(:,:,k-1)-ZLE(:,:,k))
         END DO

         !-------------------------------------------
         ! Mass Fraction of Total Cloud Water (QCTOT)
         ! Grid Mean Convective Condensate (CNV_QC)
         !-------------------------------------------
         call MAPL_GetPointer ( IMPORT,    QITOT,  'QITOT', __RC__  )
         call MAPL_GetPointer ( IMPORT,    QLTOT,  'QLTOT',  __RC__  )
         call MAPL_GetPointer ( EXPORT,    QCTOT,  'QCTOT', ALLOC=.TRUE., __RC__  )
         QCTOT(:,:,:) = QLTOT(:,:,:) + QITOT(:,:,:)

         call MAPL_GetPointer ( IMPORT,   QITOT1, 'QITOT1', __RC__  )
         call MAPL_GetPointer ( IMPORT,   QLTOT1, 'QLTOT1', __RC__  )
         call MAPL_GetPointer ( EXPORT,   CNV_QC, 'CNV_QC', ALLOC=.TRUE., __RC__  )
         CNV_QC(:,:,:) = QCTOT(:,:,:) - ( QLTOT1(:,:,:) + QITOT1(:,:,:) )

         WHERE ( CNV_QC(:,:,:) < 0.0 ) CNV_QC(:,:,:) = 0.0
         CNV_QC(:,:,:) = CNV_QC(:,:,:) *(DT/(30.0*SecondsPerMinute))

         !----------------
         ! Vegetation Type
         !----------------
         call MAPL_GetPointer ( EXPORT, ITY, 'ITY', ALLOC=.TRUE., __RC__ )
         ITY = 1.0

         !------------------------------------------------
         ! Flash Rate (LFR) for Lighting Parameterization
         !------------------------------------------------
         call MAPL_GetPointer ( IMPORT,  CNV_MFC, 'CNV_MFC', __RC__ )
         call MAPL_GetPointer ( IMPORT,  CN_PRCP, 'CN_PRCP', __RC__ )
         call MAPL_GetPointer ( EXPORT,      LFR,     'LFR', ALLOC=.TRUE., __RC__ )
         call MAPL_GetPointer ( EXPORT,    BYNCY,   'BYNCY', ALLOC=.TRUE., __RC__ )

         ! Determine the pressure at convective cloud top
         ALLOCATE( CNV_TOPP(is:ie,js:je),   STAT=STATUS); VERIFY_(STATUS)
         CNV_TOPP(:,:) = MAPL_UNDEF
         do j=js, je
            do i=is, ie
               do l=1,lm
                  if (CNV_MFC(i,j,l)/=0.0) then
                     CNV_TOPP(i,j) = PLE(i,j,l)
                     exit
                  endif
               enddo
            enddo
         enddo
         
         ALLOCATE( CAPE(is:ie,js:je),   STAT=STATUS); VERIFY_(STATUS)
         call computeCAPE (TH, Q, PLE, CAPE, BYNCY, ie-is+1, je-js+1, LM)

         ALLOCATE( flashRate(is:ie,js:je),   STAT=STATUS); VERIFY_(STATUS)

         call computeFlashRate (ggState, nc, LM, TS, CNV_TOPP, FROCEAN, &
                          CN_PRCP, CAPE, CNV_MFC, TH, PLE, ZLE, flashRate, RC=STATUS)
         VERIFY_(STATUS)

         LFR(:,:) = flashRate(:,:)
     
         DEALLOCATE(CNV_TOPP, CAPE, flashRate)

         !----------------------
         ! DQDT and DQRL
         !----------------------
         call MAPL_GetPointer ( IMPORT,   PFLCU,   'PFL_CN', ALLOC=.TRUE., __RC__  )
         call MAPL_GetPointer ( IMPORT,   PFICU,    'PFICU', ALLOC=.TRUE., __RC__  )
         call MAPL_GetPointer ( IMPORT, PFLLSAN, 'PFL_LSAN', ALLOC=.TRUE., __RC__  )
         call MAPL_GetPointer ( IMPORT, PFILSAN,  'PFILSAN', ALLOC=.TRUE., __RC__  )

         call MAPL_GetPointer ( EXPORT,    DQDT,     'DQDT', ALLOC=.TRUE., __RC__  )
         call MAPL_GetPointer ( EXPORT,    DQRL,     'DQRL', ALLOC=.TRUE., __RC__  )

         ALLOCATE(totflux(is:ie,js:je,0:lm), STAT=STATUS); VERIFY_(STATUS)

         totflux = PFLCU+PFLLSAN+PFICU+PFILSAN

         ! The minus sign is to have it be compatible with DQDT 
         ! (which is the negative of the precip generation)
         DO k = 1, LM
            DQDT(:,:,k) = -(totflux(:,:,k-1) - totflux(:,:,k)) / MASS(:,:,k)
         END DO

         DQRL = 0.0

         DEALLOCATE(totflux)
      END IF ! .NOT. enable_pTracers

      call MAPL_TimerOff(ggState,"RUN")
      call MAPL_TimerOff(ggState,"TOTAL")

#endif

      ! All Done
      ! --------
      RETURN_(ESMF_SUCCESS)

      end subroutine modelAdvance
!EOC
!------------------------------------------------------------------------------
!BOP
      subroutine computeEdgePressure(PLE, PS, AK, BK, km)
!
! !INPUT PARAMETERS:
      INTEGER,  intent(in) :: km      ! number of vertical levels
      REAL(r4), intent(in) :: PS(:,:) ! Surface pressure (Pa)
      REAL(r8), intent(in) :: ak(km+1), bk(km+1)
!
! !OUTPUT PARAMETERS:
      REAL(r4), intent(out) :: PLE(:,:,:)  ! Edge pressure (Pa)
!EOP
!------------------------------------------------------------------------------
!BOC
      INTEGER  :: L
      
      DO L = 1, km
         PLE(:,:,L) = ak(L) + bk(L)*PS(:,:)
      END DO

      RETURN

      end subroutine computeEdgePressure
!EOC
!------------------------------------------------------------------------------
!BOP
      subroutine computeTotalPrecip(TPREC, PRECANV, PRECCON, PRECLSC)
!
! !INPUT PARAMETERS:
     REAL(r4), intent(in) :: PRECANV(:,:) ! Surface precipitation flux from anvils (kg/m2/s)
     REAL(r4), intent(in) :: PRECCON(:,:) ! Surface precipitation flux from convection (kg/m2/s)
     REAL(r4), intent(in) :: PRECLSC(:,:) ! Surface precipitation flux from large-scale (kg/m2/s)
!
! !OUTPUT PARAMETERS:
     REAL(r4), intent(out) :: TPREC(:,:)  ! Total precipitation (kg/m2/s)
!EOP
!------------------------------------------------------------------------------
!BOC
      TPREC = PRECANV + PRECCON + PRECLSC

      RETURN

      end subroutine computeTotalPrecip
!EOC
!------------------------------------------------------------------------------
!BOP
      subroutine computeLWI(LWI, TSKIN, FRLAKE, FROCEAN, FRACI)
!
! !INPUT PARAMETERS:
     REAL(r4), intent(in) :: TSKIN(:,:)    ! Surface skin temperature (K)
     REAL(r4), intent(in) :: FRLAKE(:,:)   ! Fraction of lake type in grid box (1)
     REAL(r4), intent(in) :: FROCEAN(:,:)  ! Fraction of ocean in grid box (1)
     REAL(r4), intent(in) :: FRACI(:,:)    ! Ice covered fraction of tile (1)
!
! !OUTPUT PARAMETERS:
     REAL(r4), intent(out) :: LWI(:,:) ! Land water ice flag (1)
!
!EOP
!------------------------------------------------------------------------------
!BOC

                                          LWI = 1.0  ! Land
      where ( FROCEAN+FRLAKE >= 0.6     ) LWI = 0.0  ! Water
      where ( LWI==0 .and. FRACI>0.5    ) LWI = 2.0  ! Ice
      where ( LWI==0 .and. TSKIN<271.40 ) LWI = 2.0  ! Ice

      RETURN

      end subroutine computeLWI
!EOC
!------------------------------------------------------------------------------
!BOP
      subroutine computeRelativeHumidity(RH2, PRESS3D, T, QV)

!
! !INPUT PARAMETERS:
      REAL, intent(in) :: PRESS3D(:,:,:)  ! Pressure (Pa)
      REAL, intent(in) :: T      (:,:,:)  ! Air temperature (K)
      REAL, intent(in) :: QV     (:,:,:)  ! Specific humidity (kg/kg)
!
! !OUTPUT PARAMETERS:
      REAL, intent(out) :: RH2(:,:,:) ! Relative humidity (1)
!
!EOP
!------------------------------------------------------------------------------
!BOC

      ! -----------------------------------------------------------------
      ! First calculate relative humidity from Seinfeld (1986) p. 181.
      ! The first  RH2 is the temperature dependent parameter a.
      ! The second RH2 is the saturation vapor pressure of water.
      ! The third  RH2 is the actual relative humidity as a fraction.
      ! Then make sure RH2 is between 0 and 0.95.
      !-----------------------------------------------------------------

      RH2(:,:,:) = 1.0d0 - (373.15d0 / T(:,:,:))

      RH2(:,:,:) =  &
             1013.25d0 * Exp (13.3185d0 * RH2(:,:,:)    -  &
                               1.9760d0 * RH2(:,:,:)**2 -  &
                               0.6445d0 * RH2(:,:,:)**3 -  &
                               0.1299d0 * RH2(:,:,:)**4)

      RH2(:,:,:) = QV(:,:,:) * MWTAIR / 18.0d0 /  &
                      GPKG * PRESS3D(:,:,:) / RH2(:,:,:)

      RH2(:,:,:) = Max (Min (RH2(:,:,:), 0.95d0), 0.0d0)

      RETURN 

      end subroutine computeRelativeHumidity
!EOC
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINES: airdens
!
! !INTERFACE:

      subroutine airdens_ ( AIRDENS, PLE, TH, Q, im, jm, lm )
!
! !INPUT PARAMETERS:
      integer, intent(in) :: im, jm , lm
      real,    intent(in) :: PLE(im,jm,lm+1)   ! pressure edges
      real,    intent(in) :: TH(im,jm,lm)      ! (dry) potential temperature
      real,    intent(in) :: Q(im,jm,lm)       ! apecific humidity
!
! !OUTPUT PARAMETERS:
      real,    intent(out) :: AIRDENS(:,:,:)    ! air density [kg/m3]
!
! !DESCRIPTION:
! Computes the air density that might be needed when GEOSchem is not
! exercised.
!
!EOP
!-----------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
      integer :: k
      real :: eps
      integer :: STATUS, RC
      character(len=ESMF_MAXSTR)      :: IAm = "airdens_"
      real, allocatable :: npk(:,:,:) ! normalized pk = (PLE/p0)^kappa

      allocate(npk(im,jm,lm+1),stat=STATUS) ! work space
      VERIFY_(STATUS)

      eps = MAPL_RVAP / MAPL_RGAS - 1.0

      ! Compute normalized PLE**Kappa
      ! ----------------------------
      npk = (PLE/MAPL_P00)**MAPL_KAPPA

      ! Compute AIRDENS from hydrostatic equation
      ! -------------------------------------
      do k = 1, lm
         AIRDENS(:,:,k) =       ( PLE(:,:,k+1) - PLE(:,:,k) ) /      &
                      ( MAPL_CP * ( TH(:,:,k)*(1. + eps*Q(:,:,k) ) ) &
                              * ( npk(:,:,k+1) - npk(:,:,k) ) )
      end do

      deallocate(npk)

      end subroutine airdens_
!EOC
!-----------------------------------------------------------------------
!BOP
      function compAreaWeightedAverage_2d (var2D, vm, cellArea) result(wAverage)
!
! !INPUT PARAMETER:
      real            :: var2D(:,:)
      real            :: cellArea(:,:)
      type (ESMF_VM)  :: VM
!
! RETURNED VALUE:
      real  :: wAverage
!
! DESCRIPTION:
! Computes the area weighted average of a 2d variable.
!
!EOP
!-----------------------------------------------------------------------
!BOC
      logical, save :: first = .true.
      real(r8) , save :: sumArea
      real(r8) :: sumWeight
      integer :: ik, im, jm, STATUS, RC
      real(r8), pointer :: weightVals(:,:)
      real(r8) :: sumWeight_loc, sumArea_loc
      character(len=ESMF_MAXSTR) :: IAm = 'compAreaWeightedAverage_2d'

      ! Determine the earth surface area
      if (first) then
         sumArea_loc   = SUM( cellArea  (:,:)  )
         call MAPL_CommsAllReduceSum(vm, sendbuf= sumArea_loc, &
                                         recvbuf= sumArea, &
                                         cnt=1, RC=status)
         VERIFY_(STATUS)

         first = .false.
      end if

      im = size(cellArea,1)
      jm = size(cellArea,2)

      allocate(weightVals(im,jm))
      weightVals(:,:) = cellArea(:,:)*var2D(:,:)

      sumWeight_loc = SUM( weightVals(:,:) )

      call MAPL_CommsAllReduceSum(vm, sendbuf= sumWeight_loc, recvbuf= sumWeight, &
         cnt=1, RC=status)
      VERIFY_(STATUS)

      wAverage = sumWeight/sumArea

      deallocate(weightVals)

      return

      end function compAreaWeightedAverage_2d
!EOC
!-----------------------------------------------------------------------
!BOP
      function compAreaWeightedAverage_3d (var3D, vm, cellArea) result(wAverage)
!
! !INPUT PARAMETER:
      real            :: var3D(:,:,:)
      real            :: cellArea(:,:)
      type (ESMF_VM)  :: VM
!
! RETURNED VALUE:
      real  :: wAverage
!
! DESCRIPTION:
! Computes the area weighted average of a 3d variable.
!
!EOP
!-----------------------------------------------------------------------
!BOC
      logical, save :: first = .true.
      real(r8) , save :: sumArea
      real(r8) :: sumWeight
      integer :: ik, im, jm, STATUS, RC
      real(r8), pointer :: weightVals(:,:)
      real(r8) :: sumWeight_loc, sumArea_loc
      character(len=ESMF_MAXSTR) :: IAm = 'compAreaWeightedAverage_3d'

      ! Determine the earth surface area
      if (first) then
         sumArea_loc   = SUM( cellArea  (:,:)  )
         call MAPL_CommsAllReduceSum(vm, sendbuf= sumArea_loc, &
                                         recvbuf= sumArea, &
                                         cnt=1, RC=status)
         VERIFY_(STATUS)

         first = .false.
      end if

      im = size(cellArea,1)
      jm = size(cellArea,2)

      allocate(weightVals(im,jm))
      weightVals(:,:) = 0.0d0
      DO ik = lbound(var3D,3), ubound(var3D,3)
         weightVals(:,:) = weightVals(:,:) + cellArea(:,:)*var3D(:,:,ik)
      END DO

      sumWeight_loc = SUM( weightVals(:,:) )

      call MAPL_CommsAllReduceSum(vm, sendbuf= sumWeight_loc, recvbuf= sumWeight, &
         cnt=1, RC=status)
      VERIFY_(STATUS)

      wAverage = sumWeight/sumArea

      deallocate(weightVals)

      return

      end function compAreaWeightedAverage_3d
!EOC
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: computeFlashRate
!
! !INTERFACE:
!
      subroutine computeFlashRate (STATE, nc, lm, TS, CCTP, FROCEAN, CN_PRCP, &
                        CAPE, CNV_MFC, TH, PLE, ZLE, strokeRate, RC)
!
! !INPUT PARAMETERS:
      INTEGER, INTENT(IN) :: nc     ! Number of cells
      INTEGER, INTENT(IN) :: lm     ! Number of layers
    
      REAL, INTENT(IN), DIMENSION(nc) :: TS       ! Surface temperature [K]
      REAL, INTENT(IN), DIMENSION(nc) :: CCTP     ! Convective cloud top pressure [Pa] with MAPL_UNDEFs
      REAL, INTENT(IN), DIMENSION(nc) :: FROCEAN  ! Areal ocean fraction
      REAL, INTENT(IN), DIMENSION(nc) :: CN_PRCP  ! Convective precipitation [kg m^{-2} s^{-1}]
      REAL, INTENT(IN), DIMENSION(nc) :: CAPE     ! Convective available potential energy [J m^{-2}]

      REAL, INTENT(IN), DIMENSION(nc,lm) :: TH        ! Potential temperature [K]
      REAL, INTENT(IN), DIMENSION(nc,0:lm) :: CNV_MFC ! Convective mass flux [kg m^{-2} s^{-1}]
      REAL, INTENT(IN), DIMENSION(nc,0:lm) :: PLE     ! Layer interface pressures  [Pa]
      REAL, INTENT(IN), DIMENSION(nc,0:lm) :: ZLE     ! Layer depths [m]

!
! !OUTPUT PARAMETERS:
      REAL, INTENT(OUT), DIMENSION(nc) :: strokeRate  ! Flashes per second
      INTEGER, OPTIONAL, INTENT(OUT) :: RC
!
! !INPUT/OUTPUT PARAMETERS:
      TYPE(MAPL_MetaComp), POINTER :: STATE ! Internal MAPL_Generic state
!
! !DESCRIPTION:
!  Generate lightning flash rates [km$^{-2}$ s$^{-1}$] using a six-variable polynomial fit.\\
!
!
!  ORIGIN AND CONTACT\\
!  Dr. Dale Allen, Associate Research Scientist\\
!  Dept. of Atmospheric and Oceanic Science\\
!  University of Maryland\\
!  College Park, MD 20742\\
!  301-405-7629 (ph); 301-314-9482 (fax)\\
!  http://www.meto.umd.edu/~allen\\
!  
!
!  FORMULATION NOTES\\
!  Predictor variables are set to zero where CN\_PRCP is zero or where the 
!   optical depth cloud top height is less than 5.5 km.
!  The fit returns flash rates in units km$^{-2}$ day$^{-1}$.  Convert to 
!   km$^{-2}$ s$^{-1}$ for the export state.\\
!
!
!  OTHER NOTES OF INTEREST\\
!  MOIST sets CNV\_TOPP to zero if there is an absence of convection.
!
! !REVISION HISTORY:
! 30 Nov 2011 Nielsen     First crack
! 29 Feb 2012 Nielsen     Accomodate CNV\_TOPP MAPL\_UNDEF for and after Fortuna-2\_5\_p4
! 04 Nov 2014 Kouatchou   Adapted the subroutine for GEOSctm
!EOP
!-----------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
      ! Error log variables
      ! -------------------
      INTEGER :: STATUS

      REAL, DIMENSION(nc) :: A1X1
      REAL, DIMENSION(nc) :: A2X2
      REAL, DIMENSION(nc) :: A3X3
      REAL, DIMENSION(nc) :: A4X4
      REAL, DIMENSION(nc) :: A5X5

      ! Local variables
      ! ---------------
      INTEGER :: i            ! General-purpose integers
      INTEGER :: k
      INTEGER :: n
    
      REAL :: a0c,a0m         ! Coefficients at continental and marine locations
      REAL :: a1c,a1m
      REAL :: a2c,a2m
      REAL :: a3c,a3m
      REAL :: a4c,a4m
      REAL :: a5c,a5m
    
      REAL :: x1Divisor       ! Divisors for x1-x5.
      REAL :: x2Divisor
      REAL :: x3Divisor
      REAL :: x4Divisor
      REAL :: x5Divisor
    
      REAL :: x5Power         ! Exponent for the surface temperature deviation predictor
    
      REAL :: sfcTLimit       ! Temperature thresholds
      REAL :: airTLimit
    
      REAL :: hPaCldTop       ! Cloud top limiter for weak/no convection
    
      REAL, ALLOCATABLE, DIMENSION(:) :: x1         ! Five independent variables
      REAL, ALLOCATABLE, DIMENSION(:) :: x2
      REAL, ALLOCATABLE, DIMENSION(:) :: x3
      REAL, ALLOCATABLE, DIMENSION(:) :: x4
      REAL, ALLOCATABLE, DIMENSION(:) :: x5
    
      REAL, ALLOCATABLE, DIMENSION(:) :: cloudTopAG ! Cloud top height above ground
      REAL, ALLOCATABLE, DIMENSION(:) :: cnv_topp   ! Convective cloud top pressure with MAPL_UNDEFs
                                                    ! changed to zero
    
      REAL, ALLOCATABLE, DIMENSION(:,:) :: dZ       ! Layer depths [m]
      REAL, ALLOCATABLE, DIMENSION(:,:) :: p        ! Pressure at middle of layer [Pa]
      REAL, ALLOCATABLE, DIMENSION(:,:) :: T        ! Air temperature at middle of layer [K]
    
      INTEGER, ALLOCATABLE, DIMENSION(:)   :: weakCnvMask   ! Weak or no convection mask
      INTEGER, ALLOCATABLE, DIMENSION(:,:) :: mask          ! Working mask
      INTEGER, ALLOCATABLE, DIMENSION(:,:) :: cloudTopMask  ! Mask is 1 below cloud top

      character(len=ESMF_MAXSTR)    :: IAm = 'computeFlashRate'


      ! Preliminaries
      ! -------------
      RC = 0
      strokeRate(:) = 0.0

      ! Coefficients of the predictors, marine locations
      ! ------------------------------------------------
      CALL MAPL_GetResource(STATE,a0m,'MARINE_A0:',DEFAULT= 0.0139868,RC=STATUS)
      VERIFY_(STATUS)
      CALL MAPL_GetResource(STATE,a1m,'MARINE_A1:',DEFAULT= 0.0358764,RC=STATUS)
      VERIFY_(STATUS)
      CALL MAPL_GetResource(STATE,a2m,'MARINE_A2:',DEFAULT=-0.0610214,RC=STATUS)
      VERIFY_(STATUS)
      CALL MAPL_GetResource(STATE,a3m,'MARINE_A3:',DEFAULT=-0.0102320,RC=STATUS)
      VERIFY_(STATUS)
      CALL MAPL_GetResource(STATE,a4m,'MARINE_A4:',DEFAULT= 0.0031352,RC=STATUS)
      VERIFY_(STATUS)
      CALL MAPL_GetResource(STATE,a5m,'MARINE_A5:',DEFAULT= 0.0346241,RC=STATUS)
      VERIFY_(STATUS)
    
      ! Coefficients of the predictors, continental locations
      ! -----------------------------------------------------
      CALL MAPL_GetResource(STATE,a0c,'CONTINENT_A0:',DEFAULT=-0.0183172,RC=STATUS)
      VERIFY_(STATUS)
      CALL MAPL_GetResource(STATE,a1c,'CONTINENT_A1:',DEFAULT=-0.0562338,RC=STATUS)
      VERIFY_(STATUS)
      CALL MAPL_GetResource(STATE,a2c,'CONTINENT_A2:',DEFAULT= 0.1862740,RC=STATUS)
      VERIFY_(STATUS)
      CALL MAPL_GetResource(STATE,a3c,'CONTINENT_A3:',DEFAULT=-0.0023363,RC=STATUS)
      VERIFY_(STATUS)
      CALL MAPL_GetResource(STATE,a4c,'CONTINENT_A4:',DEFAULT=-0.0013838,RC=STATUS)
      VERIFY_(STATUS)
      CALL MAPL_GetResource(STATE,a5c,'CONTINENT_A5:',DEFAULT= 0.0114759,RC=STATUS)
      VERIFY_(STATUS)
    
      ! Divisors for nondimensionalization of the predictors
      ! ----------------------------------------------------
      CALL MAPL_GetResource(STATE,x1Divisor,'X1_DIVISOR:',DEFAULT=4.36,RC=STATUS)
      VERIFY_(STATUS)
      CALL MAPL_GetResource(STATE,x2Divisor,'X2_DIVISOR:',DEFAULT=9.27,RC=STATUS)
      VERIFY_(STATUS)
      CALL MAPL_GetResource(STATE,x3Divisor,'X3_DIVISOR:',DEFAULT=34.4,RC=STATUS)
      VERIFY_(STATUS)
      CALL MAPL_GetResource(STATE,x4Divisor,'X4_DIVISOR:',DEFAULT=21.4,RC=STATUS)
      VERIFY_(STATUS)
      CALL MAPL_GetResource(STATE,x5Divisor,'X5_DIVISOR:',DEFAULT=14600.,RC=STATUS)
      VERIFY_(STATUS)
    
      ! Exponent for the surface temperature deviation predictor
      ! --------------------------------------------------------
      CALL MAPL_GetResource(STATE,x5Power,'X5_EXPONENT:',DEFAULT=3.00,RC=STATUS)
      VERIFY_(STATUS)
    
      ! Threshold temperatures
      ! ----------------------
      CALL MAPL_GetResource(STATE,sfcTLimit,'SFC_T_LIMIT:',DEFAULT=273.0,RC=STATUS)
      VERIFY_(STATUS)
      CALL MAPL_GetResource(STATE,airTLimit,'AIR_T_LIMIT:',DEFAULT=263.0,RC=STATUS)
      VERIFY_(STATUS)
    
      ! Cloud-top pressure limiter
      ! --------------------------
      CALL MAPL_GetResource(STATE,hPaCldTop,'CLOUD_TOP_LIMIT:',DEFAULT=500.,RC=STATUS)
      VERIFY_(STATUS)
    
      ! Layer depths [m]
      ! ----------------
      ALLOCATE(dZ(nc,lm),STAT=STATUS)
      VERIFY_(STATUS)
      dZ = zle(:,0:lm-1)-zle(:,1:lm)
    
      ! Pressure at mid-layer [Pa]
      ! --------------------------
      ALLOCATE(p(nc,lm),STAT=STATUS)
      VERIFY_(STATUS)
      p = (ple(:,1:lm)+ple(:,0:lm-1))*0.50
    
      ! Temperature at mid-layer [K]
      ! ----------------------------
      ALLOCATE(T(nc,lm),STAT=STATUS)
      VERIFY_(STATUS)
      T = TH*((p*1.00E-05)**(MAPL_RGAS/MAPL_CP))
    
      ! Reset CNV_TOPP's MAPL_UNDEFs to zeroes
      ! --------------------------------------
      ALLOCATE(cnv_topp(nc),STAT=STATUS)
      WHERE(CCTP == MAPL_UNDEF)
         cnv_topp = 0.00
      ELSEWHERE
         cnv_topp = CCTP
      END WHERE
    
      ! Set weak/no convection mask
      ! ---------------------------
      ALLOCATE(weakCnvMask(nc),STAT=STATUS)
      VERIFY_(STATUS)
      weakCnvMask = 0
      WHERE(cn_prcp == 0.00 .OR. cnv_topp >= hPaCldTop*100.00 .OR. CAPE >= MAPL_UNDEF) weakCnvMask = 1
    
      ! Convective cloud top mask
      ! -------------------------
      ALLOCATE(cloudTopMask(nc,lm),STAT=STATUS)
      VERIFY_(STATUS)
      cloudTopMask = 0
      DO k = 1,lm
         WHERE(ple(1:nc,k) > cnv_topp(1:nc) .AND. cnv_topp(1:nc) > 0.00) cloudTopMask(1:nc,k) = 1
      END DO
    
      ! Cloud top distance above ground [m]
      ! -----------------------------------
      ALLOCATE(cloudTopAG(nc),STAT=STATUS)
      VERIFY_(STATUS)
      cloudTopAG = 0.00
      DO i = 1,nc
         n = SUM(cloudTopMask(i,1:lm))
         IF(n > 0) cloudTopAG(i) = SUM(dZ(i,lm-n+1:lm))
      END DO
    
      ! X1: Cold cloud depth: Vertical extent [km] where T < airTLimit and p > cnv_topp
      ! -------------------------------------------------------------------------------
      ALLOCATE(x1(nc),STAT=STATUS)
      VERIFY_(STATUS)
      ALLOCATE(mask(nc,lm),STAT=STATUS)
      VERIFY_(STATUS)
    
      mask = 0
      WHERE(T < airTLimit .AND. cloudTopMask == 1) mask = 1
    
      x1 = 0.00
      DO i = 1,nc
         DO k = 1,lm
            IF(mask(i,k) == 1) x1(i) = x1(i)+dZ(i,k)*0.001
         END DO
      END DO
      WHERE(weakCnvMask == 1) x1 = 0.00
      x1 = x1/x1Divisor
    
      ! X4: Integrated convective mass flux
      ! -----------------------------------
      ALLOCATE(x4(nc),STAT=STATUS)
      VERIFY_(STATUS)
      x4 = 0.00
      DO i = 1,nc
         DO k = 1,lm
            IF(mask(i,k) == 1) x4(i) = x4(i)+cnv_mfc(i,k)*dZ(i,k)
         END DO
      END DO
      WHERE(weakCnvMask == 1) x4 = 0.00
      x4 = x4/x4Divisor
    
      ! X5: Surface temperature deviation from sfcTLimit, positive only.
      ! Note: UNDEF TS test retains the ability to boot-strap moist_import_rst.
      ! -----------------------------------------------------------------------
      ALLOCATE(x5(nc),STAT=STATUS)
      VERIFY_(STATUS)
      WHERE(TS == MAPL_UNDEF)
         x5 = 0.00
      ELSEWHERE
         x5 = TS-sfcTLimit
      END WHERE
      WHERE(weakCnvMask == 1) x5 = 0.00
      WHERE(x5 < 0.00) x5 = 0.00
      x5 = x5**x5Power/x5Divisor

      ! X2: Total cloud depth [km]
      ! --------------------------
      ALLOCATE(x2(nc),STAT=STATUS)
      VERIFY_(STATUS)
      x2 = cloudTopAG*0.001
      WHERE(weakCnvMask == 1) x2 = 0.00
      x2 = x2/x2Divisor
    
      ! X3: CAPE
      ! --------
      ALLOCATE(x3(nc),STAT=STATUS)
      VERIFY_(STATUS)
      x3 = CAPE
      WHERE(weakCnvMask == 1) x3 = 0.00
      x3 = x3/x3Divisor

      ! Polynomial fit [units: km^{-2} s^{-1}] and individual
      ! terms including marine and continental discrimination
      ! -----------------------------------------------------
      WHERE(frOcean >= 0.01)
         strokeRate = (a0m + a1m*x1 + a2m*x2 + a3m*x3 + a4m*x4 + a5m*x5)/86400.00
         A1X1 = a1m*x1/86400.00
         A2X2 = a2m*x2/86400.00
         A3X3 = a3m*x3/86400.00
         A4X4 = a4m*x4/86400.00
         A5X5 = a5m*x5/86400.00
      ELSEWHERE
         strokeRate = (a0c + a1c*x1 + a2c*x2 + a3c*x3 + a4c*x4 + a5c*x5)/86400.00
         A1X1 = a1c*x1/86400.00
         A2X2 = a2c*x2/86400.00
         A3X3 = a3c*x3/86400.00
         A4X4 = a4c*x4/86400.00
         A5X5 = a5c*x5/86400.00
      END WHERE

      ! Eliminate negatives
      ! -------------------
      WHERE(strokeRate < 0.00) strokeRate = 0.00

      ! Set rate to zero where any of x1 through x5 are zero
      ! ----------------------------------------------------
      WHERE(x1 == 0.00) strokeRate = 0.00
      WHERE(x2 == 0.00) strokeRate = 0.00
      WHERE(x3 == 0.00) strokeRate = 0.00
      WHERE(x4 == 0.00) strokeRate = 0.00
      WHERE(x5 == 0.00) strokeRate = 0.00

      ! Clean up
      ! --------
      DEALLOCATE(x1,STAT=STATUS)
      VERIFY_(STATUS)
      DEALLOCATE(x2,STAT=STATUS)
      VERIFY_(STATUS)
      DEALLOCATE(x3,STAT=STATUS)
      VERIFY_(STATUS)
      DEALLOCATE(x4,STAT=STATUS)
      VERIFY_(STATUS)
      DEALLOCATE(x5,STAT=STATUS)
      VERIFY_(STATUS)
      DEALLOCATE(cnv_topp,STAT=STATUS)
      VERIFY_(STATUS)
      DEALLOCATE(dZ,STAT=STATUS)
      VERIFY_(STATUS)
      DEALLOCATE(p,STAT=STATUS)
      VERIFY_(STATUS)
      DEALLOCATE(T,STAT=STATUS)
      VERIFY_(STATUS)
      DEALLOCATE(cloudTopAG,STAT=STATUS)
      VERIFY_(STATUS)
      DEALLOCATE(mask,STAT=STATUS)
      VERIFY_(STATUS)
      DEALLOCATE(cloudTopMask,STAT=STATUS)
      VERIFY_(STATUS)
      DEALLOCATE(weakCnvMask,STAT=STATUS)
      VERIFY_(STATUS)

      return

      end subroutine computeFlashRate
!EOC
!-----------------------------------------------------------------------
!BOP
!
      subroutine computeCAPE (TH, Q, PLE, CAPE, BUOY, IM, JM, LM)

      !use GEOS_UtilsMod

      integer,                     intent(in)  :: IM,JM,LM
      real, dimension(IM,JM,LM),   intent(in)  :: TH  ! potential temperature
      real, dimension(IM,JM,LM),   intent(in)  :: Q   ! specific humidity
      real, dimension(IM,JM,0:LM), intent(in)  :: PLE   ! pressure
      real, dimension(IM,JM),      intent(out) :: CAPE
      real, dimension(IM,JM,LM),   intent(out) :: BUOY
!EOP
!-----------------------------------------------------------------------
!BOC
#if 0
      integer                         :: L
      real,    dimension(IM,JM,  LM)  :: DQS, QSS, PLO, TEMP, PK, DM, DP
      real,    dimension(IM,JM,  LM)  :: ZLO
      real,    dimension(IM,JM,0:LM)  :: ZLE
      real,    dimension(IM,JM,0:LM)    :: CNV_PLE
      real,    dimension(IM,JM,0:LM)    :: PKE
      real,    dimension(IM,JM   )  :: HC
      logical, dimension(IM,JM   )  :: UNSTABLE

      CNV_PLE  = PLE*.01
      PLO      = 0.5*(CNV_PLE(:,:,0:LM-1) +  CNV_PLE(:,:,1:LM  ) )
      PKE      = (CNV_PLE/1000.)**(MAPL_RGAS/MAPL_CP)
      DP       = ( PLE(:,:,1:LM)-PLE(:,:,0:LM-1) )
      PK       = (PLO/1000.)**(MAPL_RGAS/MAPL_CP)
      DM       = DP*(1./MAPL_GRAV)
      TEMP     = TH*PK
      DQS      = GEOS_DQSAT(TEMP, PLO, qsat=QSS)

      ZLE(:,:,LM) = 0.
      do L=LM,1,-1
         ZLE(:,:,L-1) = TH (:,:,L) * (1.+MAPL_VIREPS*Q(:,:,L))
         ZLO(:,:,L  ) = ZLE(:,:,L) + (MAPL_CP/MAPL_GRAV)*( PKE(:,:,L)-PK (:,:,L  ) ) * ZLE(:,:,L-1)
         ZLE(:,:,L-1) = ZLO(:,:,L) + (MAPL_CP/MAPL_GRAV)*( PK (:,:,L)-PKE(:,:,L-1) ) * ZLE(:,:,L-1)
      end do

      ! From BUYOANCY

       HC  =  TEMP(:,:,LM) + (MAPL_GRAV/MAPL_CP)*ZLO(:,:,LM) + (MAPL_ALHL/MAPL_CP)*Q(:,:,LM)

       do L=LM-1,1,-1
          BUOY(:,:,L) = HC - (TEMP(:,:,L) + (MAPL_GRAV/MAPL_CP)*ZLO(:,:,L) + (MAPL_ALHL/MAPL_CP)*QSS(:,:,L))
          BUOY(:,:,L) = BUOY(:,:,L) / ( (1.+ (MAPL_ALHL/MAPL_CP)*DQS(:,:,L))*TEMP(:,:,L) )
       enddo

       BUOY(:,:,LM) = 0.0

       UNSTABLE = .false.

       CAPE = 0.

! New formulation
       do L=1,LM-1
         where(BUOY(:,:,L)>0.)
            CAPE = CAPE + BUOY(:,:,L)*DM(:,:,L)
         end where
      end do

! Old formulation
!       do L=1,LM-1
!          where(BUOY(:,:,L)>0.) UNSTABLE=.true.
!          where(UNSTABLE)
!             CAPE = CAPE + BUOY(:,:,L)*DM(:,:,L)
!          end where
!       end do

       UNSTABLE = CAPE > 0.0

       where(.not.UNSTABLE)
          CAPE=MAPL_UNDEF
       end where
#endif
      
      return

      end subroutine computeCAPE
!EOC
!-----------------------------------------------------------------------
!BOP
!
      subroutine compute_ZLE_RH2 (ZLE, RH2, TH, Q, PLE, IM, JM, LM)

      !use GEOS_UtilsMod

      integer,                     intent(in)  :: IM,JM,LM
      real, dimension(IM,JM,LM),   intent(in)  :: TH  ! potential temperature
      real, dimension(IM,JM,LM),   intent(in)  :: Q   ! specific humidity
      real, dimension(IM,JM,0:LM), intent(in)  :: PLE   ! pressure
      real, dimension(IM,JM,LM),   intent(out) :: RH2   ! relative humidity
      real, dimension(IM,JM,0:LM), intent(out) :: ZLE   ! geopotential height
!EOP
!-----------------------------------------------------------------------
!BOC
#if 0
      integer                         :: L
      real,    dimension(IM,JM,  LM)  :: PLO, PK
      real,    dimension(IM,JM,  LM)  :: ZLO
      real,    dimension(IM,JM,0:LM)  :: CNV_PLE
      real,    dimension(IM,JM,0:LM)  :: PKE

      CNV_PLE  = PLE*.01
      PLO      = 0.5*(CNV_PLE(:,:,0:LM-1) +  CNV_PLE(:,:,1:LM  ) )
      PKE      = (CNV_PLE/1000.)**(MAPL_RGAS/MAPL_CP)
      PK       = (PLO/1000.)**(MAPL_RGAS/MAPL_CP)

      ZLE(:,:,LM) = 0.0
      do L=LM,1,-1
         ZLE(:,:,L-1) = TH (:,:,L) * (1.+MAPL_VIREPS*Q(:,:,L))
         ZLO(:,:,L  ) = ZLE(:,:,L) + (MAPL_CP/MAPL_GRAV)*( PKE(:,:,L)-PK (:,:,L  ) ) * ZLE(:,:,L-1)
         ZLE(:,:,L-1) = ZLO(:,:,L) + (MAPL_CP/MAPL_GRAV)*( PK (:,:,L)-PKE(:,:,L-1) ) * ZLE(:,:,L-1)
      end do

      RH2     = max(MIN( Q/GEOS_QSAT (TH*PK, PLO) , 1.02 ),0.0)
#endif
      return

      end subroutine compute_ZLE_RH2
!EOC
!-----------------------------------------------------------------------
      end module ENVCTM
