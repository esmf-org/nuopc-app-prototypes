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

    call NUOPC_AddVarSpec(GC, STATE, SHORT_NAME,                      & 
                              LONG_NAME=LONG_NAME,                    &
                              UNITS=UNITS,                            &
                              DIMS=Dims, VLOCATION=VLocation,         &
                              DATATYPE=DATATYPE,                      &
                              NUM_SUBTILES=NUM_SUBTILES,              &
                              REFRESH_INTERVAL=REFRESH_INTERVAL,     &
                              AVERAGING_INTERVAL=AVERAGING_INTERVAL,  &
                              HALOWIDTH=HALOWIDTH,                    &
                              PRECISION=PRECISION,                    &
                              DEFAULT=DEFAULT,                        &
                              RESTART=RESTART,                        &
                              UNGRIDDED_DIMS=UNGRIDDED_DIMS,          &
                              FIELD_TYPE=FIELD_TYPE,                  &
                              STAGGERING=STAGGERING,                  &
                              ROTATION=ROTATION, RC=RC)
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

    call NUOPC_AddVarSpec(GC, STATE, SHORT_NAME,                      & 
                              LONG_NAME=LONG_NAME,                    &
                              UNITS=UNITS,                            &
                              DIMS=Dims, VLOCATION=VLocation,         &
                              DATATYPE=DATATYPE,                      &
                              NUM_SUBTILES=NUM_SUBTILES,              &
                              REFRESH_INTERVAL=REFRESH_INTERVAL,     &
                              AVERAGING_INTERVAL=AVERAGING_INTERVAL,  &
                              HALOWIDTH=HALOWIDTH,                    &
                              PRECISION=PRECISION,                    &
                              DEFAULT=DEFAULT,                        &
                              RESTART=RESTART,                        &
                              UNGRIDDED_DIMS=UNGRIDDED_DIMS,          &
                              FIELD_TYPE=FIELD_TYPE,                  &
                              STAGGERING=STAGGERING,                  &
                              ROTATION=ROTATION, RC=RC)
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
  subroutine NUOPC_AddInternalSpec(GC,                 &
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

    if (present(RC)) RC = ESMF_SUCCESS

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
       DATATYPE   = DATATYPE,                                                &
       NUM_SUBTILES=NUM_SUBTILES,                                            &
       AVERAGING_INTERVAL= AVERAGING_INTERVAL,                               &
       REFRESH_INTERVAL= REFRESH_INTERVAL,                                   &
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

       call MAPL_VarSpecAddRefToList(mystate%ptr%exportSpec, STATE(I), RC=RC)
       VERIFY_(RC)

    endif

    RETURN

  end subroutine NUOPC_AddInternalSpec

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


    ! If DATATYPE is MAPL_Bundleitem, we need to do something different:: what??
    ! ignore it for now
    if (DATATYPE .ne. MAPL_Bundleitem) then
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
     endif
     RETURN_(ESMF_SUCCESS)
  end subroutine NUOPC_AddVarSpec

  function NUOPC_FieldCreateFromSpec(SPEC,DEFER,RC)
    type(MAPL_VarSpec),               intent(INOUT) :: SPEC
    logical, optional,                intent(IN   ) :: DEFER
    integer, optional,                intent(  OUT) :: RC

! Return value
    type(ESMF_Field)      :: NUOPC_FieldCreateFromSpec

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_StateCreateFromSpec"
    integer                               :: STATUS

    integer               :: COUNTS(ESMF_MAXDIM)
    integer               :: L
    type(ESMF_DistGrid)   :: distgrid
    type (ESMF_Array)     :: Array
    type (ESMF_Field)     :: FIELD
    type (ESMF_FieldBundle) :: BUNDLE
    type (ESMF_Field)       :: SPEC_FIELD
    type (ESMF_FieldBundle) :: SPEC_BUNDLE
    real(kind=ESMF_KIND_R4), pointer         :: VAR_1D(:), VAR_2D(:,:), VAR_3D(:,:,:)
    real(kind=ESMF_KIND_R8), pointer         :: VR8_1D(:), VR8_2D(:,:), VR8_3D(:,:,:)
    logical               :: usableDEFER
    logical               :: deferAlloc
    integer               :: RANK
    integer               :: DIMS
    integer               :: STAT
    integer               :: KND
    integer               :: LOCATION
    character(ESMF_MAXSTR):: SHORT_NAME
    character(ESMF_MAXSTR):: LONG_NAME
    character(ESMF_MAXSTR):: UNITS
    character(ESMF_MAXSTR):: FRIENDLYTO
    integer               :: REFRESH
    integer               :: AVGINT
    real                  :: DEFAULT_VALUE
    type(ESMF_Grid)       :: GRD, GRID
    integer               :: I
    logical               ::  done
    integer               :: N, N1, N2, NE
    integer               :: HW
    integer               :: RESTART
    integer               :: maplist(ESMF_MAXDIM)          ! mapping between array and grid
    character(len=ESMF_MAXSTR), pointer     :: ATTR_INAMES(:)
    character(len=ESMF_MAXSTR), pointer     :: ATTR_RNAMES(:)
    integer,                    pointer     :: ATTR_IVALUES(:)
    real,                       pointer     :: ATTR_RVALUES(:)
    integer,                    pointer     :: UNGRD(:)
    integer                                 :: attr
    integer                                 :: initStatus
    logical                                 :: defaultProvided
    integer                                 :: fieldRank
    real(kind=ESMF_KIND_R8)                 :: def_val_8
    type(ESMF_TypeKind_Flag)                :: typekind
    logical                                 :: has_ungrd
    logical                                 :: doNotAllocate
    logical                                 :: alwaysAllocate
    integer                                 :: field_type
    integer                                 :: staggering
    integer                                 :: rotation
    type(ESMF_State)                        :: SPEC_STATE
    type(ESMF_State)                        :: nestSTATE
    character(ESMF_MAXSTR)                  :: ungridded_unit
    character(ESMF_MAXSTR)                  :: ungridded_name
    real,                    pointer        :: ungridded_coords(:)
    integer                                 :: szUngrd
    integer                                 :: rstReq


   if (present(DEFER)) then
      usableDEFER = DEFER
   else
      usableDEFER = .false.
   end if

   attr = 0
   rstReq = 0

      call MAPL_VarSpecGet(SPEC,DIMS=DIMS,VLOCATION=LOCATION,   &
                           SHORT_NAME=SHORT_NAME, LONG_NAME=LONG_NAME, UNITS=UNITS,&
                           FIELD=SPEC_FIELD, &
                           BUNDLE=SPEC_BUNDLE, &
                           STATE=SPEC_STATE, &
                           STAT=STAT, DEFAULT = DEFAULT_VALUE, &
                           defaultProvided = defaultProvided, &
                           FRIENDLYTO=FRIENDLYTO, &
                           COUPLE_INTERVAL=REFRESH, &
                           ACCMLT_INTERVAL=AVGINT, &
                           HALOWIDTH=HW, &
                           RESTART=RESTART, &
                           PRECISION=KND, &
                           ATTR_RNAMES=ATTR_RNAMES, &
                           ATTR_INAMES=ATTR_INAMES, &
                           ATTR_RVALUES=ATTR_RVALUES, &
                           ATTR_IVALUES=ATTR_IVALUES, &
                           UNGRIDDED_DIMS=UNGRD, &
                           UNGRIDDED_UNIT=UNGRIDDED_UNIT, &
                           UNGRIDDED_NAME=UNGRIDDED_NAME, &
                           UNGRIDDED_COORDS=UNGRIDDED_COORDS, &
                           GRID=GRID, &
                           doNotAllocate=doNotAllocate, &
                           alwaysAllocate=alwaysAllocate, &
                           FIELD_TYPE=FIELD_TYPE, &
                           STAGGERING=STAGGERING, &
                           ROTATION=ROTATION, &
                           RC=STATUS )
      VERIFY_(STATUS)

      if (RESTART == MAPL_RestartRequired) then
         rstReq = 1
      end if

      if (DIMS == MAPL_DimsTileOnly .OR. DIMS == MAPL_DimsTileTile) then
         ATTR = IOR(ATTR, MAPL_AttrTile)
      else
         ATTR = IOR(ATTR, MAPL_AttrGrid)
      end if
      
      deferAlloc = usableDefer
      if (usableDefer) deferAlloc = .not. alwaysAllocate

! Create the appropriate ESMF FIELD
! ---------------------------------

         field = MAPL_FieldCreateEmpty(name=SHORT_NAME, grid=grid, rc=status)
         VERIFY_(STATUS)

         has_ungrd = associated(UNGRD)

         if (.not. deferAlloc) then

!ALT we check if doNotAllocate is set only for fields that are not deferred
            if (.not. doNotAllocate) then
               if (has_ungrd) then
                  if (defaultProvided) then
                     call MAPL_FieldAllocCommit(field, dims=dims, location=location, typekind=knd, &
                          hw=hw, ungrid=ungrd, default_value=default_value, rc=status)
                     VERIFY_(STATUS)
                  else
                     call MAPL_FieldAllocCommit(field, dims=dims, location=location, typekind=knd, &
                          hw=hw, ungrid=ungrd, rc=status)
                     VERIFY_(STATUS)
                  endif
               else
                  if (defaultProvided) then
                     call MAPL_FieldAllocCommit(field, dims=dims, location=location, typekind=knd, &
                          hw=hw, default_value=default_value, rc=status)
                     VERIFY_(STATUS)
                  else
                     call MAPL_FieldAllocCommit(field, dims=dims, location=location, typekind=knd, &
                          hw=hw, rc=status)
                     VERIFY_(STATUS)
                  end if

               end if
            else
               call ESMF_AttributeSet(FIELD, NAME='doNotAllocate', VALUE=1, RC=STATUS)
               VERIFY_(STATUS)
            end if
         else
            call ESMF_AttributeSet(FIELD, NAME='PRECISION', VALUE=KND, RC=STATUS)
            VERIFY_(STATUS)
            call ESMF_AttributeSet(FIELD, NAME='HAS_UNGRIDDED_DIMS', &
                 value=has_ungrd, RC=STATUS)
            VERIFY_(STATUS)
            call ESMF_AttributeSet(FIELD, NAME='DEFAULT_PROVIDED', &
                 value=defaultProvided, RC=STATUS)
            VERIFY_(STATUS)
            if (defaultProvided) then
               call ESMF_AttributeSet(FIELD, NAME='DEFAULT_VALUE', &
                    value=default_value, RC=STATUS)
               VERIFY_(STATUS)
            end if
            if (has_ungrd) then
               call ESMF_AttributeSet(FIELD, NAME='UNGRIDDED_DIMS', valueList=UNGRD, RC=STATUS)
               VERIFY_(STATUS)
            end if
         end if

! Put the FIELD in the MAPL FIELD (VAR SPEC)
! --------------------------------

      call MAPL_VarSpecSet(SPEC,FIELD=FIELD,RC=STATUS)
      VERIFY_(STATUS)
! and in the FIELD in the state
! --------------------------

      if (deferAlloc) then
         initStatus = MAPL_Uninitialized
      else
         if (defaultProvided) initStatus = MAPL_InitialDefault
      end if

! Add SPECs to the FIELD

      call ESMF_AttributeSet(FIELD, NAME='STAT', VALUE=STAT, RC=STATUS)
      VERIFY_(STATUS)
      call ESMF_AttributeSet(FIELD, NAME='DIMS', VALUE=DIMS, RC=STATUS)
      VERIFY_(STATUS)
      call ESMF_AttributeSet(FIELD, NAME='VLOCATION', VALUE=LOCATION, RC=STATUS)
      VERIFY_(STATUS)
      call ESMF_AttributeSet(FIELD, NAME='LONG_NAME', VALUE=LONG_NAME, RC=STATUS)
      VERIFY_(STATUS)
      call ESMF_AttributeSet(FIELD, NAME='UNITS', VALUE=UNITS, RC=STATUS)
      VERIFY_(STATUS)

      call ESMF_AttributeSet(FIELD, NAME='REFRESH_INTERVAL', VALUE=REFRESH, RC=STATUS)
      VERIFY_(STATUS)
      call ESMF_AttributeSet(FIELD, NAME='AVERAGING_INTERVAL', VALUE=AVGINT, RC=STATUS)
      VERIFY_(STATUS)
      call ESMF_AttributeSet(FIELD, NAME='HALOWIDTH', VALUE=HW, RC=STATUS)
      VERIFY_(STATUS)
      call ESMF_AttributeSet(FIELD, NAME='RESTART', VALUE=RESTART, RC=STATUS)
      VERIFY_(STATUS)
      call ESMF_AttributeSet(FIELD, NAME='FIELD_TYPE', VALUE=FIELD_TYPE, RC=STATUS)
      VERIFY_(STATUS)
      call ESMF_AttributeSet(FIELD, NAME='STAGGERING', VALUE=STAGGERING, RC=STATUS)
      VERIFY_(STATUS)
      call ESMF_AttributeSet(FIELD, NAME='ROTATION', VALUE=ROTATION, RC=STATUS)
      VERIFY_(STATUS)
      if (associated(UNGRD)) Then
         call ESMF_AttributeSet(FIELD, NAME='UNGRIDDED_NAME', VALUE=UNGRIDDED_NAME, RC=STATUS)
         VERIFY_(STATUS)
         call ESMF_AttributeSet(FIELD, NAME='UNGRIDDED_UNIT', VALUE=UNGRIDDED_UNIT, RC=STATUS)
         VERIFY_(STATUS)
         if (associated(UNGRIDDED_COORDS)) then
            szUngrd = size(ungridded_coords)
            call ESMF_AttributeSet(FIELD, NAME='UNGRIDDED_COORDS', itemCount=szUngrd, &
                                   valuelist=ungridded_coords, rc=status)
            VERIFY_(STATUS)
         end if      
      end if

      if (associated(ATTR_RNAMES)) then
         DO N = 1, size(ATTR_RNAMES) 
            call ESMF_AttributeSet(FIELD, NAME=trim(ATTR_RNAMES(N)), &
                                        VALUE=ATTR_RVALUES(N), RC=STATUS)
            VERIFY_(STATUS)
         END DO
      end if

      if (associated(ATTR_INAMES)) then
         DO N = 1, size(ATTR_INAMES) 
            call ESMF_AttributeSet(FIELD, NAME=trim(ATTR_INAMES(N)), &
                                        VALUE=ATTR_IVALUES(N), RC=STATUS)
            VERIFY_(STATUS)
         END DO
      end if

10    if (FRIENDLYTO /= "") then

! parse the string for ":" word delimiters
         done = .false.
         n1 = 1
         NE = len(FRIENDLYTO)

         DO WHILE(.not. DONE)
            N = INDEX(FRIENDLYTO(N1:NE), ':')
            IF (N == 0) then
               DONE = .TRUE.
               N2 = NE
            ELSE
               N2 = N1 + N - 2
            END IF
            if (N1 <= N2 .and. N2 > 0) then
!print *,"DEBUG: setting FieldAttr:FriendlyTo"//trim(FRIENDLYTO(N1:N2))
                  call ESMF_AttributeSet(FIELD, &
                       NAME='FriendlyTo'//trim(FRIENDLYTO(N1:N2)), &
                       VALUE=.TRUE., RC=STATUS)
                  VERIFY_(STATUS)
            end if

            N1 = N1 + N
         END DO

      end if

   NUOPC_FieldCreateFromSpec = FIELD

   RETURN

  end function NUOPC_FieldCreateFromSpec


end module NUOPC_Generic