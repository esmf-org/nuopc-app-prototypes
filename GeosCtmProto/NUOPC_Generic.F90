#include "MAPL_Generic.h"
!-------------------------------------------------------------------------
!BOP
!
! !MODULE: NUOPC_Generic -- Mimicking some MAPL subroutines handling MAPL_VarSpec
!
! !INTERFACE:
!
module NUOPC_Generic

      use ESMF
      use MAPL_mod
      use NUOPC
      implicit none

! !PUBLIC TYPES:
 public my_States
 public mystates_WRAP

! !PUBLIC MEMBER FUNCTIONS:

 public NUOPC_AddImportSpec
 public NUOPC_AddExportSpec
 public NUOPC_AddInternalSpec

 type my_States
    type (MAPL_VarSpec),     pointer   :: ImportSpec(:)
    type (MAPL_VarSpec),     pointer   :: ExportSpec(:)
    type (MAPL_VarSpec),     pointer   :: InternalSpec(:)
 end type my_States 

 type mystates_WRAP
         type (my_States), pointer :: PTR
 end type mystates_WRAP

contains

  !BOPI
  ! !IROUTINE: NUOPC_AddImportSpec --- Sets the specifications for an item in the {\tt IMPORT} state.

  !INTERFACE:
  subroutine NUOPC_AddImportSpec(GC, SHORT_NAME, LONG_NAME,               &
                                      UNITS,  Dims, VLocation,                 &
                                      DATATYPE,NUM_SUBTILES, REFRESH_INTERVAL, &
                                      AVERAGING_INTERVAL, HALOWIDTH, PRECISION, DEFAULT,  &
                                      RESTART, UNGRIDDED_DIMS, FIELD_TYPE,     &
                                      STAGGERING, ROTATION, RC)

    !ARGUMENTS:
    type (ESMF_GridComp)            , intent(INOUT)   :: GC
    character (len=*)               , intent(IN)      :: SHORT_NAME
    character (len=*)  , optional   , intent(IN)      :: LONG_NAME
    character (len=*)  , optional   , intent(IN)      :: UNITS
    integer            , optional   , intent(IN)      :: DIMS
    integer            , optional   , intent(IN)      :: DATATYPE
    integer            , optional   , intent(IN)      :: NUM_SUBTILES
    integer            , optional   , intent(IN)      :: VLOCATION
    integer            , optional   , intent(IN)      :: REFRESH_INTERVAL
    integer            , optional   , intent(IN)      :: AVERAGING_INTERVAL
    integer            , optional   , intent(IN)      :: HALOWIDTH
    integer            , optional   , intent(IN)      :: PRECISION
    real               , optional   , intent(IN)      :: DEFAULT
    integer            , optional   , intent(IN)      :: RESTART
    integer            , optional   , intent(IN)      :: UNGRIDDED_DIMS(:)
    integer            , optional   , intent(IN)      :: FIELD_TYPE
    integer            , optional   , intent(IN)      :: STAGGERING
    integer            , optional   , intent(IN)      :: ROTATION
    integer            , optional   , intent(OUT)     :: RC
    !EOPI

    type (mystates_WRAP)                               :: mystate
    type (MAPL_VarSpec)             , pointer         :: STATE(:)

    RC = ESMF_SUCCESS

    call ESMF_UserCompGetInternalState(GC, 'MAPL_VarSpec', mystate, rc) 
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

    STATE => mystate%ptr%importSpec

    call NUOPC_AddVarSpec(GC, STATE, SHORT_NAME, LONG_NAME,               &
                              UNITS,  Dims, VLocation,                 &
                              DATATYPE,NUM_SUBTILES, REFRESH_INTERVAL, &
                              AVERAGING_INTERVAL, HALOWIDTH, PRECISION, DEFAULT,  &
                              RESTART, UNGRIDDED_DIMS, FIELD_TYPE,     &
                              STAGGERING, ROTATION, RC)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

    return
  end subroutine NUOPC_AddImportSpec

  !BOPI
  ! !IROUTINE: NUOPC_AddExportSpec --- Sets the specifications for an item in the {\tt IMPORT} state.

  !INTERFACE:
  subroutine NUOPC_AddExportSpec(GC, SHORT_NAME, LONG_NAME,               &
                                      UNITS,  Dims, VLocation,                 &
                                      DATATYPE,NUM_SUBTILES, REFRESH_INTERVAL, &
                                      AVERAGING_INTERVAL, HALOWIDTH, PRECISION, DEFAULT,  &
                                      RESTART, UNGRIDDED_DIMS, FIELD_TYPE,     &
                                      STAGGERING, ROTATION, RC)

    !ARGUMENTS:
    type (ESMF_GridComp)            , intent(INOUT)   :: GC
    character (len=*)               , intent(IN)      :: SHORT_NAME
    character (len=*)  , optional   , intent(IN)      :: LONG_NAME
    character (len=*)  , optional   , intent(IN)      :: UNITS
    integer            , optional   , intent(IN)      :: DIMS
    integer            , optional   , intent(IN)      :: DATATYPE
    integer            , optional   , intent(IN)      :: NUM_SUBTILES
    integer            , optional   , intent(IN)      :: VLOCATION
    integer            , optional   , intent(IN)      :: REFRESH_INTERVAL
    integer            , optional   , intent(IN)      :: AVERAGING_INTERVAL
    integer            , optional   , intent(IN)      :: HALOWIDTH
    integer            , optional   , intent(IN)      :: PRECISION
    real               , optional   , intent(IN)      :: DEFAULT
    integer            , optional   , intent(IN)      :: RESTART
    integer            , optional   , intent(IN)      :: UNGRIDDED_DIMS(:)
    integer            , optional   , intent(IN)      :: FIELD_TYPE
    integer            , optional   , intent(IN)      :: STAGGERING
    integer            , optional   , intent(IN)      :: ROTATION
    integer            , optional   , intent(OUT)     :: RC
    !EOPI

    type (mystates_WRAP)                               :: mystate
    type (MAPL_VarSpec)             , pointer         :: STATE(:)

    RC = ESMF_SUCCESS

    call ESMF_UserCompGetInternalState(GC, 'MAPL_VarSpec', mystate, rc) 
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

    STATE => mystate%ptr%exportSpec

    call NUOPC_AddVarSpec(GC, STATE, SHORT_NAME, LONG_NAME,               &
                              UNITS,  Dims, VLocation,                 &
                              DATATYPE,NUM_SUBTILES, REFRESH_INTERVAL, &
                              AVERAGING_INTERVAL, HALOWIDTH, PRECISION, DEFAULT,  &
                              RESTART, UNGRIDDED_DIMS, FIELD_TYPE,     &
                              STAGGERING, ROTATION, RC)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

    return
  end subroutine NUOPC_AddExportSpec

!BOPI
! !IROUTINE: MAPL_AddInternalSpec
! !IIROUTINE: MAPL_AddInternalSpec --- Sets specifications for an item in the \texttt{INTERNAL} state

! !INTERFACE:
  subroutine NUOPC_StateAddInternalSpec(GC,                 &
                                       SHORT_NAME,         &
                                       LONG_NAME,          &
                                       UNITS,              &
                                       DIMS,               &
                                       VLOCATION,          &
                                       DATATYPE,           &
                                       NUM_SUBTILES,       &
                                       REFRESH_INTERVAL,   &
                                       AVERAGING_INTERVAL, &
                                       DEFAULT,            &
                                       RESTART,            &
                                       HALOWIDTH,          &
                                       PRECISION,          &
                                       FRIENDLYTO,         &
                                       ADD2EXPORT,         &
                                       ATTR_RNAMES,        &
                                       ATTR_INAMES,        &
                                       ATTR_RVALUES,       &
                                       ATTR_IVALUES,       &
                                       UNGRIDDED_DIMS,     &
                                       FIELD_TYPE,         &
                                       STAGGERING,         &
                                       ROTATION,           &
                                       RC)

! !ARGUMENTS:

    type (ESMF_GridComp)            , intent(INOUT)   :: GC
    character (len=*)               , intent(IN)      :: SHORT_NAME
    character (len=*)  , optional   , intent(IN)      :: LONG_NAME
    character (len=*)  , optional   , intent(IN)      :: UNITS
    integer            , optional   , intent(IN)      :: DIMS
    integer            , optional   , intent(IN)      :: DATATYPE
    integer            , optional   , intent(IN)      :: VLOCATION
    integer            , optional   , intent(IN)      :: NUM_SUBTILES
    integer            , optional   , intent(IN)      :: REFRESH_INTERVAL
    integer            , optional   , intent(IN)      :: AVERAGING_INTERVAL
    integer            , optional   , intent(IN)      :: PRECISION
    real               , optional   , intent(IN)      :: DEFAULT
    integer            , optional   , intent(IN)      :: RESTART
    character (len=*)  , optional   , intent(IN)      :: HALOWIDTH
    character (len=*)  , optional   , intent(IN)      :: FRIENDLYTO
    logical            , optional   , intent(IN)      :: ADD2EXPORT
    character (len=*)  , optional   , intent(IN)      :: ATTR_INAMES(:)
    character (len=*)  , optional   , intent(IN)      :: ATTR_RNAMES(:)
    integer            , optional   , intent(IN)      :: ATTR_IVALUES(:)
    real               , optional   , intent(IN)      :: ATTR_RVALUES(:)
    integer            , optional   , intent(IN)      :: UNGRIDDED_DIMS(:)
    integer            , optional   , intent(IN)      :: FIELD_TYPE
    integer            , optional   , intent(IN)      :: STAGGERING
    integer            , optional   , intent(IN)      :: ROTATION
    integer            , optional   , intent(OUT)     :: RC

! !DESCRIPTION:

!  Sets the specifications for an item in the {\tt INTERNAL} state.

!EOPI

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_AddInternalSpec"
    integer                               :: STATUS
    integer                               :: usable_RS
    integer                               :: usable_HW
    integer                               :: I
    type (MAPL_VarSpec),  pointer         :: SPEC
    integer                               :: default_dt
    integer                               :: interval
    real                                  :: dt
    type (mystates_WRAP)                  :: mystate
    type (MAPL_VarSpec) , pointer         :: STATE(:)

    if (present) RC = ESMF_SUCCESS

    call ESMF_UserCompGetInternalState(GC, 'MAPL_VarSpec', mystate, rc) 
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

    STATE => mystate%ptr%internalSpec

    if (present(HALOWIDTH)) then
       read(HALOWIDTH,'(I1)') usable_HW
    else
       usable_HW = 0
    endif

    call NUOPC_AddVarSpec(GC, STATE,                                         &
       LONG_NAME  = LONG_NAME,                                               &
       UNITS      = UNITS,                                                   &
       SHORT_NAME = SHORT_NAME,                                              &
       DIMS       = DIMS,                                                    &
       STAT       = DATATYPE,                                                &
       NUM_SUBTILES=NUM_SUBTILES,                                            &
       ACCMLT_INTERVAL= AVERAGING_INTERVAL,                                  &
       COUPLE_INTERVAL= REFRESH_INTERVAL,                                    &
       VLOCATION  = VLOCATION,                                               &
       DEFAULT    = DEFAULT, FRIENDLYTO = FRIENDLYTO,                        &
       HALOWIDTH  = usable_HW, PRECISION=PRECISION,                          &
       RESTART    = usable_RS,                                               &
       ATTR_RNAMES=ATTR_RNAMES, ATTR_INAMES=ATTR_INAMES,                     &
       ATTR_RVALUES=ATTR_RVALUES, ATTR_IVALUES=ATTR_IVALUES,                 &
       UNGRIDDED_DIMS=UNGRIDDED_DIMS,                                        &
       FIELD_TYPE = FIELD_TYPE,                                              &
       STAGGERING = STAGGERING,                                              &
       ROTATION   = ROTATION,                                                &
       RC=rc  )
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

!ALT: the next section is added here upon the request of Arlindo:
!     if FRIENDLYTO is set, we automatically 
!     add the spec/field to the export

    if (present(FRIENDLYTO) .or. present(ADD2EXPORT)) then

       I=MAPL_VarSpecGetIndex(STATE, SHORT_NAME, RC=RC)
       if (I == -1) then
          RETURN_(ESMF_FAILURE)
       endif

       call MAPL_VarSpecAddRefToList(mystates%ptr%export_spec, STATE(I), RC=RC)
       VERIFY_(RC)

    endif

    RETURN

  end subroutine MAPL_AddInternalSpec

  subroutine NUOPC_AddVarSpec(GC, STATE, SHORT_NAME, LONG_NAME,               &
                                      UNITS,  Dims, VLocation,                 &
                                      DATATYPE,NUM_SUBTILES, REFRESH_INTERVAL, &
                                      AVERAGING_INTERVAL, HALOWIDTH, PRECISION, FRIENDLYTO,  &
                                      ATTR_RNAMES, ATTR_INAMES, ATTR_RVALUES, ATTR_IVALUES, &
                                      DEFAULT, RESTART, UNGRIDDED_DIMS, FIELD_TYPE,     &
                                      STAGGERING, ROTATION, RC)

    !ARGUMENTS:
    type (ESMF_GridComp)            , intent(INOUT)   :: GC
    type (MAPL_VarSpec)             , pointer         :: STATE(:)
    character (len=*)               , intent(IN)      :: SHORT_NAME
    character (len=*)  , optional   , intent(IN)      :: LONG_NAME
    character (len=*)  , optional   , intent(IN)      :: UNITS
    integer            , optional   , intent(IN)      :: DIMS
    integer            , optional   , intent(IN)      :: DATATYPE
    integer            , optional   , intent(IN)      :: NUM_SUBTILES
    integer            , optional   , intent(IN)      :: VLOCATION
    integer            , optional   , intent(IN)      :: REFRESH_INTERVAL
    integer            , optional   , intent(IN)      :: AVERAGING_INTERVAL
    integer            , optional   , intent(IN)      :: HALOWIDTH
    integer            , optional   , intent(IN)      :: PRECISION
    character (len=*)  , optional   , intent(IN)      :: FRIENDLYTO
    real               , optional   , intent(IN)      :: DEFAULT
    integer            , optional   , intent(IN)      :: RESTART
    character (len=*)  , optional   , intent(IN)      :: ATTR_INAMES(:)
    character (len=*)  , optional   , intent(IN)      :: ATTR_RNAMES(:)
    integer            , optional   , intent(IN)      :: ATTR_IVALUES(:)
    real               , optional   , intent(IN)      :: ATTR_RVALUES(:)
    integer            , optional   , intent(IN)      :: UNGRIDDED_DIMS(:)
    integer            , optional   , intent(IN)      :: FIELD_TYPE
    integer            , optional   , intent(IN)      :: STAGGERING
    integer            , optional   , intent(IN)      :: ROTATION
    integer            , optional   , intent(OUT)     :: RC
    !EOPI

    character(len=ESMF_MAXSTR), parameter :: IAm="NUOPC_AddVarSpec"
    integer                               :: STATUS
    integer                               :: usable_AI
    integer                               :: usable_RI
    integer                               :: usable_RS
    real                                  :: dt
    type (ESMF_Config)                    :: CF

    call ESMF_GridCompGet(GC, config=CF, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

 !  Get the clock increment interval
    call ESMF_ConfigGetAttribute( CF, dt,  Label="RUN_DT:", RC=STATUS)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out


    if (present(REFRESH_INTERVAL)) then
       usable_RI = REFRESH_INTERVAL
    else
       usable_RI = nint(dt)
    endif

    if (present(AVERAGING_INTERVAL)) then
       usable_AI = AVERAGING_INTERVAL
    else
       usable_AI = nint(dt)
    endif

    if (present(Restart)) then
       usable_RS  = Restart
    else
       usable_RS = MAPL_RestartOptional
    endif

    if (present(DIMS)) then
       ASSERT_(DIMS /= MAPL_DimsNone)
    end if

    call MAPL_VarSpecCreateInList(STATE,                                     &
       LONG_NAME  = LONG_NAME,                                               &
       UNITS      = UNITS,                                                   &
       SHORT_NAME = SHORT_NAME,                                              &
       DIMS       = DIMS,                                                    &
       STAT       = DATATYPE,                                                &
       NUM_SUBTILES=NUM_SUBTILES,                                            &
       ACCMLT_INTERVAL= usable_AI,                                           &
       COUPLE_INTERVAL= usable_RI,                                           &
       VLOCATION  = VLOCATION,                                               &
       HALOWIDTH  = HALOWIDTH,                                               &
       PRECISION  = PRECISION,                                               &
       FRIENDLYTO = FRIENDLYTO,                                              &
       RESTART    = usable_RS,                                               &
       DEFAULT    = DEFAULT,                                                 &
       ATTR_RNAMES=ATTR_RNAMES,                                              &
       ATTR_INAMES=ATTR_INAMES,                                              &
       ATTR_RVALUES=ATTR_RVALUES,                                            &
       ATTR_IVALUES=ATTR_IVALUES,                                            &
       UNGRIDDED_DIMS = UNGRIDDED_DIMS,                                      &
       FIELD_TYPE = FIELD_TYPE,                                              &
       STAGGERING = STAGGERING,                                              &
       ROTATION = ROTATION,                                                  &
       RC=STATUS  )
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

     if (.not. NUOPC_FieldDictionaryHasEntry(LONG_NAME)) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName=LONG_NAME, &
          canonicalUnits=UNITS, &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif

    RETURN_(ESMF_SUCCESS)
  end subroutine NUOPC_AddVarSpec

end module NUOPC_Generic