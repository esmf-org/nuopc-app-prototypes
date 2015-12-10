module IOComp

  !-----------------------------------------------------------------------------
  ! IO Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, only: &
    model_routine_SS            => SetServices, &
    model_label_DataInitialize  => label_DataInitialize, &
    model_label_Advance         => label_Advance
  
  implicit none
  
  private
  
  public SetServices
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    ! the NUOPC model component will register the generic methods
    call NUOPC_CompDerive(gcomp, model_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! --- Initialization phases --------------------------------------

    ! Provide InitializeP0 to switch from default IPDv00 to IPDv03
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! IPDv03p1: advertise Fields
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p1"/), userRoutine=InitializeP1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! IPDv03p3: realize connected Fields with transfer action "provide"
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p3"/), userRoutine=InitializeP3, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! IPDv03p4: optionally modify the decomp/distr of transferred Grid/Mesh
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p4"/), userRoutine=InitializeP4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! IPDv03p5: realize all Fields with transfer action "accept"
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p5"/), userRoutine=InitializeP5, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! attach specializing method(s)
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Advance, &
      specRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_DataInitialize, &
      specRoutine=DataInitialize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    rc = ESMF_SUCCESS

    ! Switch to IPDv03 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv03p"/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP1(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS

    ! importable field: air_pressure_at_sea_level
    call NUOPC_Advertise(importState, &
      StandardName="air_pressure_at_sea_level", name="pmsl", &
#define WITH_GRID_TRANSFERoff
#ifdef WITH_GRID_TRANSFER
      TransferOfferGeomObject="cannot provide", rc=rc)
#else
      TransferOfferGeomObject="will provide", rc=rc)
#endif
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP3(gcomp, importState, exportState, clock, rc)
    ! IPDv03p3: realize connected Fields with transfer action "provide"
    ! and remove Fields that are not connected
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    call checkConnectedFlagProvide(importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call checkConnectedFlagProvide(exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    contains ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    
    subroutine checkConnectedFlagProvide(state, rc)
      ! Look at all of the fields in state, including in nested states. Error
      ! out if a connected field is found for which geom object must be 
      ! provided here. Remove all not connected fields.
      type(ESMF_State)  :: state
      integer, optional :: rc
      ! local variables
      integer                                 :: itemCount, item
      character(len=80)                       :: stateName
      type(ESMF_Field)                        :: field
      character(len=80)                       :: connectedValue
      character(len=20)                       :: transferAction
      character(len=80), allocatable          :: itemNameList(:)
      type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
      type(ESMF_Config)                       :: config
      integer                                 :: gridDims(2)
      type(ESMF_Grid)                         :: gridIn
      
      if (present(rc)) rc = ESMF_SUCCESS
    
      call ESMF_StateGet(state, name=stateName, nestedFlag=.true., &
        itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    
      allocate(itemNameList(itemCount), itemTypeList(itemCount))
    
      call ESMF_StateGet(state, nestedFlag=.true., &
        itemNameList=itemNameList, itemTypeList=itemTypeList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      do item=1, itemCount
        if (itemTypeList(item)==ESMF_STATEITEM_FIELD) then
          ! this is a field -> get more info
          call ESMF_StateGet(state, field=field, itemName=itemNameList(item), &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          call NUOPC_GetAttribute(field, name="Connected", &
            value=connectedValue, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          if (connectedValue=="false") then
            ! remove the field from the state
            call ESMF_StateRemove(state, itemNameList(item), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
          else
            call NUOPC_GetAttribute(field, name="TransferActionGeomObject", &
              value=transferAction, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            if (trim(transferAction)=="provide") then
              ! the Connector instructed the gcomp to provide geom object
              
              ! create and open the config
              config = ESMF_ConfigCreate(rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              call ESMF_ConfigLoadFile(config, "test.config", rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out

              ! determine the grid size
              call ESMF_ConfigGetAttribute(config, gridDims, &
                label="grid_dims:", default=-1, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
              if (gridDims(1)==-1 .or. gridDims(2)==-1) then
                ! default tiny grid
                gridDims(1) = 10
                gridDims(2) = 20
              endif
             
              ! create a Grid object for Fields
              gridIn = ESMF_GridCreate1PeriDimUfrm(maxIndex=gridDims, &
                minCornerCoord=(/0._ESMF_KIND_R8, -60._ESMF_KIND_R8/), &
                maxCornerCoord=(/360._ESMF_KIND_R8, 80._ESMF_KIND_R8/), &
                staggerLocList=(/ESMF_STAGGERLOC_CENTER/), rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out

              ! importable field: air_pressure_at_sea_level
              field = ESMF_FieldCreate(name="pmsl", grid=gridIn, &
                typekind=ESMF_TYPEKIND_R8, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              call NUOPC_Realize(importState, field=field, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
                
            endif
          endif
        endif
      enddo
      
      deallocate(itemNameList, itemTypeList)
    
    end subroutine

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP4(gcomp, importState, exportState, clock, rc)
    ! IPDv03p4: optionally modify the decomp/distr of transferred Grid/Mesh
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS

    call adjustAcceptedGeom(importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call adjustAcceptedGeom(exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    contains ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    
    subroutine adjustAcceptedGeom(state, rc)
      ! Look at all of the fields in state, including in nested states. Adjust
      ! the distribution of the accepted geom object to a 1 DE/PET distribution.
      type(ESMF_State)  :: state
      integer, optional :: rc
      ! local variables
      integer                                 :: itemCount, item
      type(ESMF_Field)                        :: field
      character(len=20)                       :: transferAction
      character(len=80), allocatable          :: itemNameList(:)
      type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
      type(ESMF_GeomType_Flag)                :: geomtype
      type(ESMF_Grid)                         :: grid
      type(ESMF_Mesh)                         :: mesh
      character(160)                          :: msgString
      type(ESMF_DistGrid)                     :: distgrid
      integer                                 :: dimCount, tileCount, petCount
      integer                                 :: deCountPTile, extraDEs
      integer, allocatable                    :: minIndexPTile(:,:), maxIndexPTile(:,:)
      integer, allocatable                    :: regDecompPTile(:,:)
      integer                                 :: i, j
    
      if (present(rc)) rc = ESMF_SUCCESS
      
      call ESMF_StateGet(state, nestedFlag=.true., itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    
      allocate(itemNameList(itemCount), itemTypeList(itemCount))
    
      call ESMF_StateGet(state, nestedFlag=.true., &
        itemNameList=itemNameList, itemTypeList=itemTypeList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      do item=1, itemCount
        if (itemTypeList(item)==ESMF_STATEITEM_FIELD) then
          ! this is a field -> get more info
          call ESMF_StateGet(state, field=field, itemName=itemNameList(item), &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          call NUOPC_GetAttribute(field, name="TransferActionGeomObject", &
            value=transferAction, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          if (trim(transferAction)=="accept") then
            ! the Connector instructed the gcomp to accept geom object
            ! -> find out which type geom object the field holds
            call ESMF_FieldGet(field, geomtype=geomtype, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            if (geomtype==ESMF_GEOMTYPE_GRID) then
              ! empty field holds a Grid with DistGrid
              call ESMF_FieldGet(field, grid=grid, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              ! access the DistGrid
              call ESMF_GridGet(grid, distgrid=distgrid, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              ! Create a custom DistGrid, based on the minIndex, maxIndex of the 
              ! accepted DistGrid, but with a default regDecomp for the current VM
              ! that leads to 1DE/PET.
              ! get dimCount and tileCount
              call ESMF_DistGridGet(distgrid, dimCount=dimCount, &
                tileCount=tileCount, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              ! allocate minIndexPTile and maxIndexPTile accord. to dimCount and tileCount
              allocate(minIndexPTile(dimCount, tileCount), &
                maxIndexPTile(dimCount, tileCount))
              ! get minIndex and maxIndex arrays
              call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, &
                maxIndexPTile=maxIndexPTile, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              ! construct a default regDecompPTile -> TODO: move this into ESMF as default
              call ESMF_GridCompGet(gcomp, petCount=petCount, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              allocate(regDecompPTile(dimCount, tileCount))
              deCountPTile = petCount/tileCount
              extraDEs = max(0, petCount-deCountPTile)
              do i=1, tileCount
                if (i<=extraDEs) then
                  regDecompPTile(1, i) = deCountPTile + 1
                else
                  regDecompPTile(1, i) = deCountPTile
                endif
                do j=2, dimCount
                  regDecompPTile(j, i) = 1
                enddo
              enddo
              ! create the new DistGrid with the same minIndexPTile and maxIndexPTile,
              ! but with a default regDecompPTile
              distgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile, &
                maxIndexPTile=maxIndexPTile, regDecompPTile=regDecompPTile, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              ! Create a new Grid on the new DistGrid and swap it in the Field
              grid = ESMF_GridCreate(distgrid, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              call ESMF_FieldEmptySet(field, grid=grid, rc=rc)    
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              ! local clean-up
              deallocate(minIndexPTile, maxIndexPTile, regDecompPTile)
            elseif (geomtype==ESMF_GEOMTYPE_MESH) then
              ! empty field holds a Mesh with DistGrid
              call ESMF_FieldGet(field, mesh=mesh, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              ! access the DistGrid
              call ESMF_MeshGet(mesh, elementDistgrid=distgrid, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              ! Create a custom DistGrid, based on the minIndex, maxIndex of the 
              ! accepted DistGrid, but with a default regDecomp for the current VM
              ! that leads to 1DE/PET.
              ! get dimCount and tileCount
              call ESMF_DistGridGet(distgrid, dimCount=dimCount, &
                tileCount=tileCount, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              ! allocate minIndexPTile and maxIndexPTile accord. to dimCount and tileCount
              allocate(minIndexPTile(dimCount, tileCount), &
                maxIndexPTile(dimCount, tileCount))
              ! get minIndex and maxIndex arrays
              call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, &
                maxIndexPTile=maxIndexPTile, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              ! construct a default regDecompPTile -> TODO: move this into ESMF as default
              call ESMF_GridCompGet(gcomp, petCount=petCount, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              allocate(regDecompPTile(dimCount, tileCount))
              deCountPTile = petCount/tileCount
              extraDEs = max(0, petCount-deCountPTile)
              do i=1, tileCount
                if (i<=extraDEs) then
                  regDecompPTile(1, i) = deCountPTile + 1
                else
                  regDecompPTile(1, i) = deCountPTile
                endif
                do j=2, dimCount
                  regDecompPTile(j, i) = 1
                enddo
              enddo
              ! create the new DistGrid with the same minIndexPTile and maxIndexPTile,
              ! but with a default regDecompPTile
              distgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile, &
                maxIndexPTile=maxIndexPTile, regDecompPTile=regDecompPTile, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              ! Create a new Grid on the new DistGrid and swap it in the Field
              mesh = ESMF_MeshCreate(distgrid, distgrid, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              call ESMF_FieldEmptySet(field, mesh=mesh, rc=rc)    
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              ! local clean-up
              deallocate(minIndexPTile, maxIndexPTile, regDecompPTile)
            else
              call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
                msg="Unsupported geom object found in "// &
                trim(itemNameList(item)), &
                line=__LINE__, &
                file=__FILE__, &
                rcToReturn=rc)
              return ! bail out
            endif
          endif
        endif
      enddo
      
      deallocate(itemNameList, itemTypeList)
    
    end subroutine
    
  end subroutine
    
  !-----------------------------------------------------------------------------

  subroutine InitializeP5(gcomp, importState, exportState, clock, rc)
    ! IPDv03p5: realize all Fields with transfer action "accept"
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Field)              :: field

    rc = ESMF_SUCCESS

    call realizeWithAcceptedGeom(importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call realizeWithAcceptedGeom(exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    contains ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    
    subroutine realizeWithAcceptedGeom(state, rc)
      ! Look at all of the fields in state, including in nested states. Realize
      ! with the accepted and adjusted geom object.
      type(ESMF_State)  :: state
      integer, optional :: rc
      ! local variables
      integer                                 :: itemCount, item
      type(ESMF_Field)                        :: field
      character(len=20)                       :: transferAction
      character(len=80), allocatable          :: itemNameList(:)
      type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
    
      if (present(rc)) rc = ESMF_SUCCESS
      
      call ESMF_StateGet(state, nestedFlag=.true., itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    
      allocate(itemNameList(itemCount), itemTypeList(itemCount))
    
      call ESMF_StateGet(state, nestedFlag=.true., &
        itemNameList=itemNameList, itemTypeList=itemTypeList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      do item=1, itemCount
        if (itemTypeList(item)==ESMF_STATEITEM_FIELD) then
          ! this is a field -> get more info
          call ESMF_StateGet(state, field=field, itemName=itemNameList(item), &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          call NUOPC_GetAttribute(field, name="TransferActionGeomObject", &
            value=transferAction, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          if (trim(transferAction)=="accept") then
            ! the Connector instructed the gcomp to accept geom object
            ! the transferred geom object is already set, allocate memory for data by complete
            call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
          endif
        endif
      enddo
      
      deallocate(itemNameList, itemTypeList)
    
    end subroutine

  end subroutine
    
  !-----------------------------------------------------------------------------

  subroutine DataInitialize(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS

    ! indicate that data initialization is complete (breaking out of init-loop)
    call NUOPC_CompAttributeSet(gcomp, &
      name="InitializeDataComplete", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Time)             :: currTime
    type(ESMF_TimeInterval)     :: timeStep
    type(ESMF_Field)            :: field
    type(ESMF_StateItem_Flag)   :: itemType
    real(ESMF_KIND_R8), pointer :: dataPtr(:,:)
    integer                     :: i,j
    integer, save               :: slice=1
    character(len=5)            :: sChar

    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep
    
#ifdef PRINT    
    call ESMF_ClockPrint(clock, options=currTime, ESMF_ClockPrint(clock, &
      "------>Advancing IO from: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call NUOPC_TimePrint(currTime + timeStep, &
      "--------------------------------> to: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

    ! write out the Fields in the importState
#ifdef SINGLEFILE
    call NUOPC_Write(importState, fileNamePrefix="field_", &
      timeslice=slice, overwrite=.true., relaxedFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#else
    write (sChar,"(i5.5)") slice
    call NUOPC_Write(importState, fileNamePrefix="field_"//trim(sChar)//"_", &
      overwrite=.true., relaxedFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
    ! advance the time slice counter
    slice = slice + 1

  end subroutine

end module
