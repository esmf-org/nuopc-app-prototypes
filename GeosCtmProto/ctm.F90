#include "MAPL_Generic.h"
#define PRINT_STATES

!-------------------------------------------------------------------------
!         NASA/GSFC, Software Systems Support Office, Code 610.3         !
!-------------------------------------------------------------------------
!BOP

! !MODULE:  CTM -- A Module to combine Chemistry, 
!                  advCore (Transport), Convection and 
!                  Diffusion Gridded Components
!
! !INTERFACE:
!
      module CTM
!
! !USES:
      use ESMF
      use MAPL_Mod
      use NUOPC
      use NUOPC_Driver, &
        driver_routine_SS             => SetServices, &
        driver_label_SetModelServices => label_SetModelServices
      use NUOPC_Connector, only : cplSS            => SetServices
      use ENVCTM,          only : EctmSetServices  => SetServices
      use ADVCORE,         only : AdvCSetServices  => SetServices
      use PTRACER,         only : pTraSetServices  => SetServices
!
      implicit none
      private
!
! !PUBLIC MEMBER FUNCTIONS:

      public SetServices

!
! !DESCRIPTION: 
!   This gridded component (GC) combines Transport (AdvCore), 
!   Convection, GEOSchem, and Diffusion GCs into a new 
!   composite GEOSctm GC.
!   The friendly tracers are variables from GEOSchem.  
! 
! \paragraph{Runnng the Code:}
!
!   The code acn be run in two main configurations:
!   \begin{enumerate}
!   \item \textbf{Passive Tracer Run:} This experiment is done to verify how well 
!         AdvCore transports the tracers. We want to find out if the advection 
!         module conserves the mass of each tracer over time. To carry out this 
!         experiment, you need to initialize the tracers. This can be done in
!         two possibe ways:
!         \begin{enumerate} 
!         \item Idealized tracers which concentrations are computed in the
!               initialize method of the pTracer component. To select this
!               option, set \emph{do\_AdvColdStart: T} in the resource file
!               \texttt{pTracer\_GridComp.rc}.
!         \item User provided restart file (on the cubed sphere grid) with
!               concentration of each tracer.
!         \end{enumerate} 
!   \item \textbf{ Chemistry Run:} Here we exercise any Chemistry configuration
!         (GOCART, GMI, GEOS CHEM).
!          We have the option to determine if we want to do Convection and/or
!          Diffusion by setting the variables \emph{do\_ctmConvection} and
!          \emph{do\_ctmDiffusion} in the resource file \texttt{CTM\_GridComp.rc}.
!   \end{enumerate}
!
! \paragraph{External Data Files:}
!
!   To run this code, you need to have data files that have at least
!   the following variables:
!
!  \begin{description}
!  \item[PLE]:  edge pressure 
!  \item[U]:    eastward wind
!  \item[V]:    northward wind
!  \item[ZLE]:  geopotential height
!  \item[Q]:    specific humidity
!  \item[T]:    temperatute
!  \end{description}
!
!  The above variables are required to for passive tracer experiments.
!  More variables should be provided for other experiments.
!  Regarless of the type of run you choose to carry out, you will need
!  to edit a resource file (named \texttt{MAPL_ExtData_state.rc}) that
!  lists (among other information) each variable and the external file
!  which contains the variable.
!
! 
!EOP
!------------------------------------------------------------------------------

      logical :: enable_pTracers  = .FALSE.
      logical :: do_ctmConvection = .FALSE.
      logical :: do_ctmDiffusion  = .FALSE.
      character(len=ESMF_MAXSTR) :: metType ! MERRA2 or MERRA1 or FPIT

!------------------------------------------------------------------------------
      contains
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: SetServices -- Sets ESMF services for this component
!
! !INTERFACE:
!
      subroutine SetServices ( GC, RC )
!
! !INPUT/OUTPUT PARAMETERS:
    type(ESMF_GridComp)                :: GC  ! gridded component
!
! !OUTPUT PARAMETERS:
    integer,             intent(  OUT) :: RC  ! return code
!
! !DESCRIPTION:  
!   The SetServices for the GEOSctm GC needs to register its
!   Initialize and Run.  It uses the MAPL\_Generic construct for defining 
!   state specs and couplings among its children.  In addition, it creates the   
!   children GCs (CHEM, Diffusion, Convection, advCore) and runs their
!   respective SetServices.
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
      integer                       :: STATUS
      integer                       :: I
      character(len=ESMF_MAXSTR)    :: COMP_NAME
      character(len=ESMF_MAXSTR)    :: IAm = 'SetServices'

      ! Get my name and set-up traceback handle
      ! ---------------------------------------
      RC=ESMF_SUCCESS

      Iam = 'SetServices'
      call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=rc )
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      Iam = trim(COMP_NAME) // "::" // Iam

      call ESMF_LogWrite(Iam, ESMF_LOGMSG_INFO, rc=rc)

      ! NUOPC_Driver registers the generic methods
      call NUOPC_CompDerive(GC, driver_routine_SS, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      
      ! Register services for this component
      ! ------------------------------------

      ! attach specializing method(s)
      call NUOPC_CompSpecialize(GC, specLabel=driver_label_SetModelServices, &
        specRoutine=SetModelServices, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

#if 0
      call NUOPC_CompSetEntryPoint (GC, ESMF_METHOD_INITIALIZE, &
      	   phaseLabelList=(/"IPDv02p2"/), userRoutine=Initialize, RC=rc )
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

      call NUOPC_CompSetEntryPoint (GC, ESMF_METHOD_RUN, &
      	   phaseLabelList=(/'RunPhase1'/), userRoutine=Run, RC=rc )
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
#endif

      return
  end subroutine SetServices

  !-----------------------------------------------------------------------------

  subroutine SetModelServices(GC, rc)
    type(ESMF_GridComp)  :: GC
    integer, intent(out) :: rc

      type (ESMF_Config)                  :: configFile
      CHARACTER(LEN=ESMF_MAXSTR)          :: rcfilen = 'CTM_GridComp.rc'
      CHARACTER(LEN=ESMF_MAXSTR)          :: rootfile
      type (ESMF_CplComp)                 :: conn
      type (ESMF_GridComp)                :: comp
      integer                             :: i
      
      rc = ESMF_SUCCESS

      call ESMF_LogWrite("CTM::SetModelServices", ESMF_LOGMSG_INFO, rc=rc)
    
      ! Choose children to birth and which children not to conceive
      ! -----------------------------------------------------------
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
                                     Label    = "ENABLE_pTracers:",  rc=rc )
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out

      call ESMF_ConfigGetAttribute(configFile, do_ctmConvection,            &
                                     Default  = .FALSE.,                    &
                                     Label    = "do_ctmConvection:", rc=rc )
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out

      call ESMF_ConfigGetAttribute(configFile, do_ctmDiffusion,             &
                                     Default  = .FALSE.,                    &
                                     Label    = "do_ctmDiffusion:",  rc=rc )
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out

      ! Type of meteological fields (MERRA2 or MERRA1 or FPIT)
      call ESMF_ConfigGetAttribute(configFile, metType,                     &
                                     Default  = 'MERRA2',                   &
                                     Label    = "metType:",          rc=rc )
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out

      IF ( MAPL_am_I_root() ) THEN
         PRINT*
         PRINT*, "---------------------------------------------------"
         PRINT*, "-----             GEOS CTM Settings           -----"
         PRINT*, "---------------------------------------------------"
         PRINT*,'   Doing Passive Tracer?: ', enable_pTracers
         PRINT*,'              Convection: ', do_ctmConvection
         PRINT*,'               Diffusion: ', do_ctmDiffusion
         PRINT*,'     Meteological Fields: ', TRIM(metType)
         PRINT*, "---------------------------------------------------"
         PRINT*
      END IF

      ! -----------------------------------------------------------------
      ! Create children`s gridded components and invoke their SetServices
      ! -----------------------------------------------------------------

      call ESMF_GridCompGet(GC, configFile=rootfile, rc=rc)      
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
      print *, 'CTM config file:', trim(rootfile)
      IF (enable_pTracers) THEN
         ! Doing passive tracer experiment
         !--------------------------------
         call NUOPC_DriverAddComp(GC, 'CTMenv', EctmSetServices, &
            comp=comp, rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out

         call ESMF_GridCompSet(comp, configfile=trim(rootfile), rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out

         call NUOPC_DriverAddComp(GC, 'DYNAMICS', AdvCSetServices, &
            comp=comp, rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out

         call ESMF_GridCompSet(comp, configfile=trim(rootfile), rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out

         call NUOPC_DriverAddComp(GC, 'PTRACERS', pTraSetServices, &
            comp=comp, rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out

         call ESMF_GridCompSet(comp, configfile=rootfile, rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out

         ! Add connectors to connect the components
         ! ECTM -> ADVCORE
         call NUOPC_DriverAddComp(GC, srcCompLabel="CTMenv", dstCompLabel="DYNAMICS", &
              compSetServicesRoutine=cplSS, comp=conn, rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
         call NUOPC_CompAttributeSet(conn, name="Verbosity", value="65521", &
            rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
         ! Add connectors to connect the components
         ! ADVCORE -> ECTM
         call NUOPC_DriverAddComp(GC, srcCompLabel="DYNAMICS", dstCompLabel="CTMenv", &
              compSetServicesRoutine=cplSS, comp=conn, rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
         call NUOPC_CompAttributeSet(conn, name="Verbosity", value="65521", &
            rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
         ! Add connectors to connect the components
         ! ECTM -> PTRACER
         call NUOPC_DriverAddComp(GC, srcCompLabel="CTMenv", dstCompLabel="PTRACERS", &
              compSetServicesRoutine=cplSS, comp=conn, rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
         call NUOPC_CompAttributeSet(conn, name="Verbosity", value="65521", &
            rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

         ! Add connectors to connect the components
         ! PTRACERS -> ADVCORE
         ! This is the friendly fields TRACER_Q00 to Q04from PTRACER to the fieldbundle TRADV
         call NUOPC_DriverAddComp(GC, srcCompLabel="PTRACERS", dstCompLabel="DYNAMICS", &
              compSetServicesRoutine=cplSS, comp=conn, rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
         call NUOPC_CompAttributeSet(conn, name="Verbosity", value="65521", &
            rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

         ! Add connectors to connect the components
         ! ADVCORE -> PTRACERS
         call NUOPC_DriverAddComp(GC, srcCompLabel="DYNAMICS", dstCompLabel="PTRACERS", &
              compSetServicesRoutine=cplSS, comp=conn, rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
         call NUOPC_CompAttributeSet(conn, name="Verbosity", value="65521", &
            rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

      END IF

#if 0
      call MAPL_TimerAdd(GC, name="INITIALIZE"    ,RC=rc)
      VERIFY_(STATUS)

      call MAPL_TimerAdd(GC, name="RUN"           ,RC=rc)
      VERIFY_(STATUS)

      ! -------------------------------
      ! Connectivities between Children
      ! -------------------------------
      CALL MAPL_AddConnectivity ( GC, &
               SHORT_NAME  = (/'AREA'/), &
               DST_ID = ECTM, SRC_ID = ADV3, __RC__  )

      CALL MAPL_AddConnectivity ( GC, &
               SRC_NAME  = (/ 'CXr8  ', 'CYr8  ', 'MFXr8 ', 'MFYr8 ', 'PLE0r8', 'PLE1r8' /), &
               DST_NAME  = (/ 'CX    ', 'CY    ', 'MFX   ', 'MFY   ', 'PLE0  ', 'PLE1  ' /), &
               DST_ID = ADV3, SRC_ID = ECTM, __RC__  )
      CALL MAPL_TerminateImport    ( GC,    &
               SHORT_NAME = (/'TRADV'/),          &
               CHILD = ADV3,    __RC__  )


      IF (enable_pTracers) THEN
         CALL MAPL_AddConnectivity ( GC, &
                 SHORT_NAME  = (/'AREA'/), &
                 DST_ID = PTRA, SRC_ID = ADV3, __RC__  )

         CALL MAPL_AddConnectivity ( GC, &
                 SHORT_NAME  = (/'PLE'/), &
                 DST_ID = PTRA, SRC_ID = ECTM, __RC__  )
      END IF

      call MAPL_GenericSetServices ( GC, RC=STATUS )
      VERIFY_(STATUS)
#endif

! Move grid creation code from Initialize to here
      call Initialize(GC, RC)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
      RETURN
  
      end subroutine SetModelServices

!EOC
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: Initialize -- Initialize method for the GEOS CTM Gridded Component
!
! !INTERFACE:
!
      subroutine Initialize ( GC,  RC )
!
! !INPUT/OUTPUT PARAMETERS:
      type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
!      type(ESMF_State)     :: IMPORT
!      type(ESMF_State)     :: EXPORT
!      type(ESMF_Clock)     :: CLOCK

! !INPUT/OUTPUT PARAMETERS:
      integer,             intent(  out) :: RC     ! Error code

! !DESCRIPTION: 
!  The Initialize method of the GEOS CTM Gridded Component.
!  It acts as a driver for the initializtion of the four children: 
!  GEOSchem, advCore, Diffusion and Convection. 
!  It also sets up the frieldly connections between the children.
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
      integer                             :: STATUS
      type (ESMF_Config)                  :: CF
      type (ESMF_FieldBundle)             :: BUNDLE, iBUNDLE
      type (ESMF_Field)                   :: FIELD
      type (ESMF_State)                   :: DUMMY
      type (ESMF_Grid)                    :: grid
      type (ESMF_GridComp), pointer       :: childcomp(:)
      integer                             :: NUM_TRACERS
      integer                             :: I
      integer                             :: NA
      character(len=ESMF_MAXSTR), pointer :: NAMES(:)
      character(len=ESMF_MAXSTR)          :: myNAME
      character(len=ESMF_MAXSTR)          ::  iNAME
      character(len=ESMF_MAXSTR)          :: COMP_NAME
      type(ESMF_Config)                   :: config
      character(len=ESMF_MAXSTR)          :: IAm = "Initialize"
      integer(ESMF_KIND_I4)               :: heartbeat_dt
      integer(ESMF_KIND_I4)               :: dt
      logical                             :: existDT


      ! Get the target components name and set-up traceback handle.
      ! -----------------------------------------------------------

      RC = ESMF_SUCCESS

      call ESMF_GridCompGet ( GC, name=COMP_NAME, RC=rc )
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
      Iam = trim(COMP_NAME) // "::" // TRIM(Iam)
      call ESMF_LogWrite(Iam, ESMF_LOGMSG_INFO, rc=rc)

      !call MAPL_TimerOn(STATE,"TOTAL")
      !call MAPL_TimerOn(STATE,"INITIALIZE")

      call ESMF_GridCompGet ( GC, config=config, RC=rc )
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
 
      call ESMF_ConfigFindLabel(config, "RUN_DT:", isPresent=existDT, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
      if (existDT) then 
          call ESMF_ConfigGetAttribute(config, dt,                     &
                                   Label = "RUN_DT:",  rc=rc )
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
             line=__LINE__, &
             file=__FILE__)) &
             return  ! bail out
      else 
          call ESMF_ConfigGetAttribute(config, heartbeat_dt,             &
                                   Label = "HEARTBEAT_DT:",  rc=rc )
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
             line=__LINE__, &
             file=__FILE__)) &
             return  ! bail out
          dt = heartbeat_dt
          call ESMF_ConfigSetAttribute(config, dt,                      &
                                       Label = "RUN_DT:", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
             line=__LINE__, &
             file=__FILE__)) &
             return  ! bail out
      endif

      ! Create grid for this GC
      !------------------------
      call My_GridCreate  (GC, rc )
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

      ! Set the children component's grid to be parent's grid
      ! also pass the vertical layer information down to child components by setting it as an 
      ! attribute to the component.
      call ESMF_GridCompGet(GC, grid=grid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
      nullify(childcomp)
      call NUOPC_DriverGetComp(GC, compList=childcomp, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
      do i=1, size(childcomp)
        call ESMF_GridCompSet(childcomp(i), grid=grid, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        !!! Set dt as RUN_DT to children component's attribute
        call ESMF_AttributeSet(childcomp(i), "RUN_DT:", dt, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        
      enddo  

      ! Call Initialize for every Child
      !--------------------------------
      ! call MAPL_GenericInitialize ( GC, IMPORT, EXPORT, CLOCK,  RC=rc)
      !  VERIFY_(STATUS)



#if DOMAPL
      ! Get children and their im/ex states from my generic state.
      !----------------------------------------------------------

      !call MAPL_Get ( STATE, GCS=GCS, GIM=GIM, GEX=GEX, RC=STATUS )
      !VERIFY_(STATUS)

      ! Extract the friendly tracers
      !-----------------------------
      IF (enable_pTracers) THEN
         !----------------
         ! AdvCore Tracers
         !----------------
         call ESMF_StateGet       (GIM(ADV3), 'TRADV', BUNDLE, RC=STATUS )
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
  
         call MAPL_GridCompGetFriendlies(GCS(PTRA), "DYNAMICS", BUNDLE, RC=STATUS )
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

#ifdef PRINT_STATES
         call WRITE_PARALLEL ( trim(Iam)//": AdvCore Tracer Bundle" )
         if ( MAPL_am_I_root() ) call ESMF_FieldBundlePrint ( BUNDLE, rc=STATUS )
#endif

         call ESMF_FieldBundleGet(BUNDLE,FieldCount=NUM_TRACERS, RC=STATUS)
         VERIFY_(STATUS)

         ! Get the names of all tracers to fill other turbulence bundles.
         !---------------------------------------------------------------

         allocate(NAMES(NUM_TRACERS),STAT=STATUS)
         VERIFY_(STATUS)

         call ESMF_FieldBundleGet(BUNDLE, fieldNameList=NAMES,  RC=STATUS)
         VERIFY_(STATUS)

      ENDIF
#endif

#if 0
      call MAPL_TimerOff(STATE,"INITIALIZE")
      call MAPL_TimerOff(STATE,"TOTAL")
#endif
      ! All Done
      !---------
     
      RETURN

      end subroutine Initialize

#if 0
!EOC
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Run -- Run method for the GEOS CTM Gridded Component
!
! !INTERFACE:

   subroutine Run ( GC, IMPORT, EXPORT, CLOCK, RC )
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
!  The run method for the GEOSctm calls the children`s
!   run methods. 
!
!EOP  
!=============================================================================
!BOC
!



! !LOCAL VARIABLES:
      integer                             :: STATUS
      type (MAPL_MetaComp),      pointer  :: STATE
      type (ESMF_GridComp),      pointer  :: GCS(:)
      type (ESMF_State),         pointer  :: GIM(:)
      type (ESMF_State),         pointer  :: GEX(:)
      type (ESMF_State)                   :: INTERNAL
      type (ESMF_Config)                  :: CF
      character(len=ESMF_MAXSTR),pointer  :: GCNames(:)
      integer                             :: I, L, K
      integer                             :: IM, JM, LM
      real                                :: DT
      character(len=ESMF_MAXSTR)          :: COMP_NAME
      character(len=ESMF_MAXSTR)          :: IAm = "Run"

      type (ESMF_FieldBundle)             :: Bundle

      character(len=ESMF_MAXSTR), pointer :: Names(:)
      character(len=ESMF_MAXSTR)          :: STRING

      CHARACTER(LEN=ESMF_MAXSTR)          :: fieldName


      ! Get the target components name and set-up traceback handle.
      ! -----------------------------------------------------------

      call ESMF_GridCompGet ( GC, name=COMP_NAME, config=CF, RC=STATUS )
      VERIFY_(STATUS)
      Iam = trim(COMP_NAME) // "::" // TRIM(Iam)

      ! Get my internal MAPL_Generic state
      !-----------------------------------

      call MAPL_GetObjectFromGC ( GC, STATE, RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_TimerOn(STATE,"TOTAL")
      call MAPL_TimerOn(STATE,"RUN")

      ! Get the children`s states from the generic state
      !-------------------------------------------------

      call MAPL_Get ( STATE,   &
          GCS=GCS, GIM=GIM, GEX=GEX,       &
          IM = IM, JM = JM, LM = LM,       &
          GCNames = GCNames,               &
          INTERNAL_ESMF_STATE = INTERNAL,  &
                               RC=STATUS )
      VERIFY_(STATUS)

      call ESMF_ConfigGetAttribute(CF, DT, Label="RUN_DT:" , RC=STATUS)
      VERIFY_(STATUS)     

      !---------------------
      ! Cinderella Component: to derive variables for other components
      !---------------------
      I=ECTM

      call MAPL_TimerOn (STATE,GCNames(I))
      call ESMF_GridCompRun (GCS(I),               &
                             importState = GIM(I), &
                             exportState = GEX(I), &
                             clock       = CLOCK,  &
                             userRC      = STATUS  )
      VERIFY_(STATUS)
      call MAPL_TimerOff(STATE,GCNames(I))

      !--------
      ! advCore
      !--------
      I=ADV3

      call MAPL_TimerOn (STATE,GCNames(I))
      call ESMF_GridCompRun (GCS(I),               &
                           importState = GIM(I), &
                           exportState = GEX(I), &
                           clock       = CLOCK,  &
                           userRC      = STATUS  )
      VERIFY_(STATUS)
      call MAPL_TimerOff(STATE,GCNames(I))

      !-----------
      ! Convection
      !-----------
     !IF (do_ctmConvection) THEN
     !   I=CONV

     !   call MAPL_TimerOn (STATE,GCNames(I))
     !   call ESMF_GridCompRun (GCS(I),               &
     !                          importState = GIM(I), &
     !                          exportState = GEX(I), &
     !                             clock       = CLOCK,  &
     !                          userRC      = STATUS  )
     !   VERIFY_(STATUS)
     !   call MAPL_TimerOff(STATE,GCNames(I))
     !END IF

     !IF (.NOT. enable_pTracers) THEN
         !----------
         ! Chemistry: Phase 1
         !----------
     !   I=CHEM   

     !   call MAPL_TimerOn (STATE,GCNames(I))
     !   call ESMF_GridCompRun (GCS(I),               &
     !                          importState = GIM(I), &
     !                          exportState = GEX(I), &
     !                          clock       = CLOCK,  &
     !                          phase       = 1,      &
     !                          userRC      = STATUS  )
     !   VERIFY_(STATUS)
     !   call MAPL_TimerOff(STATE,GCNames(I))
     !END IF


      !----------
      ! Diffusion
      !----------
     !IF (do_ctmDiffusion) THEN
     !   I=DIFF

     !   call MAPL_TimerOn (STATE,GCNames(I))
     !   call ESMF_GridCompRun (GCS(I),               &
     !                          importState = GIM(I), &
     !                             exportState = GEX(I), &
     !                          clock       = CLOCK,  &
     !                          userRC      = STATUS  )
     !   VERIFY_(STATUS)
     !   call MAPL_TimerOff(STATE,GCNames(I))
     !END IF

      IF (enable_pTracers) THEN
         !---------------
         ! Passive Tracer
         !---------------
         I=PTRA

         call MAPL_TimerOn (STATE,GCNames(I))
         call ESMF_GridCompRun (GCS(I),               &
                                importState = GIM(I), &
                                exportState = GEX(I), &
                                clock       = CLOCK,  &
                                userRC      = STATUS  )
         VERIFY_(STATUS)
         call MAPL_TimerOff(STATE,GCNames(I))
      ELSE
         !----------
         ! Chemistry: Phase 2
         !----------
     !   I=CHEM   

     !   call MAPL_TimerOn (STATE,GCNames(I))
     !   call ESMF_GridCompRun (GCS(I),               &
     !                          importState = GIM(I), &
     !                          exportState = GEX(I), &
     !                          clock       = CLOCK,  &
     !                          phase       = 2,      &
     !                          userRC      = STATUS  )
     !   VERIFY_(STATUS)
     !   call MAPL_TimerOff(STATE,GCNames(I))
      END IF

      call MAPL_TimerOff(STATE,"RUN")
      call MAPL_TimerOff(STATE,"TOTAL")

      RETURN_(ESMF_SUCCESS)

      end subroutine Run
#endif
!

! Mimicking the MAPL_GridCreate() by using the grid definition in the config file, example:
!               NX: 4
!               NY: 24
!       GEOSCTM_IM: 48
!       GEOSCTM_JM: 288
!               LM: 72
!         GRIDNAME: PE48x288-CF

subroutine My_GridCreate(GC, rc)
    use CubedSphereGridFactoryMod, only: CubedSphereGridFactory
    use MAPL_GridManagerMod, only: grid_manager
   
    type(ESMF_GridComp) :: GC
    integer             :: rc

   ! local variables
    type(ESMF_Config)               :: config
    integer                         :: NX, NY
    character(len=ESMF_MAXSTR)      :: Gridname
    type (ESMF_Grid)                :: GRID
    integer                         :: IM_WORLD, val
    integer                         :: JM_WORLD
    integer                         :: LM,L,NN
    character(len=4)                :: imsz
    character(len=5)                :: jmsz
    character(len=2)                :: date
    character(len=2)                :: pole
    integer, allocatable            :: regDecomp(:,:)
    type(CubedSphereGridFactory)    :: factory

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

! The new code set NY=NX
    ! date='CF' for cubed sphere
    if (date=='CF' .or. date=='DP') then
#if 0
      ! make sure NY is divisble by 6 and JM_WORLD is IM_WORLD*6
      if (mod(NY, 6) /= 0) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, msg='NY has to be multiple of 6', &
          line=__LINE__, &
          file=__FILE__, rcToReturn=rc) 
        return
      endif
#endif

#if 0
    ! Need "IM_WORLD:" in the config, but the following code does not work.
    ! Somehow the standalone test program works (adding a new attribute in the
    ! config)  So, as a workaround, I added IM_WORLD: 48 in GEOSCTM.rc

    call ESMF_ConfigSetAttribute(config, value=IM_WORLD, label='IM_WORLD:', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_ConfigGetAttribute(config, value=val, label='IM_WORLD:', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

     ! print *, 'IM_WORLD:', val
#endif

    ! Register to the MAPL GridManager and MAPL RegridderManager
    call register_grid_and_regridders()
    ! create ESMF cubed sphere grid using the new MAPL and FV3 code
    grid = grid_manager%make_grid(config, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

#if 0
      allocate(regDecomp(2,6))
      regDecomp(1,:)=NX
      regDecomp(2,:)=NY/6
      !print *, regDecomp(:,1), IM_WORLD, trim(Gridname)
      grid = ESMF_GridCreateCubedSphere(IM_WORLD, regDecompPTile = regDecomp, &
           staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
           name=trim(Gridname), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    call ESMF_AttributeSet(grid, name='GRID_LM', value=LM, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    call ESMF_AttributeSet(grid, name='GridType', value='Cubed-Sphere', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

#endif
    endif

    ! Set grid to the GridComp
    call ESMF_GridCompSet(GC, grid=grid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    return
end subroutine My_GridCreate

  subroutine register_grid_and_regridders()
    use MAPL_GridManagerMod, only: grid_manager
    use MAPL_RegridderManagerMod, only: regridder_manager
    use MAPL_RegridderSpecMod, only: REGRID_METHOD_BILINEAR
    use CubedSphereGridFactoryMod, only: CubedSphereGridFactory    
    use LatLonToCubeRegridderMod
    use CubeToLatLonRegridderMod
    use CubeToCubeRegridderMod
    
    type (CubedSphereGridFactory) :: factory
    
    type (CubeToLatLonRegridder) :: cube_to_latlon_prototype
    type (LatLonToCubeRegridder) :: latlon_to_cube_prototype
    type (CubeToCubeRegridder) :: cube_to_cube_prototype
    
    call grid_manager%add_prototype('Cubed-Sphere',factory)
    associate (method => REGRID_METHOD_BILINEAR, mgr => regridder_manager)
      call mgr%add_prototype('Cubed-Sphere', 'LatLon', method, cube_to_latlon_prototype)
      call mgr%add_prototype('LatLon', 'Cubed-Sphere', method, latlon_to_cube_prototype)
      call mgr%add_prototype('Cubed-Sphere', 'Cubed-Sphere', method, cube_to_cube_prototype)
    end associate

  end subroutine register_grid_and_regridders


!EOC
!------------------------------------------------------------------------------
      end module CTM
