!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2014  DNV KEMA Renewables, Inc.
!
!    This file is part of the IceFloe suite of subroutines
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!     http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.
!************************************************************************

!**********************************************************************************************************************************
! File last committed: $Date: 2014-02-13 12:30:17 -0800 (Thu, 13 Feb 2014) $
! (File) Revision #: $Rev: 134 $
! URL: $HeadURL: http://sel1004.verit.dnv.com:8080/svn/LoadSimCtl_SurfaceIce/trunk/IceDyn_IntelFortran/IceDyn/source/IceFloe/IceFloe.f90 $
!**********************************************************************************************************************************!

!**********************************************************************************************************************************
!    IceFloe is a set of routines that calculate ice floe loading on structures and is developed for
!    use with wind turbine aeroelastic simulation codes.
!
!    User manual:   
!    Theory manual:
!
!    This particular file is the interface between the core IceFloe routines and the FAST Framework
!
!    References:
!
!    Gasmi, A., M. A. Sprague, J. M. Jonkman, and W. B. Jones, Numerical stability and accuracy of temporally coupled
!    multi-physics modules in wind turbine CAE tools. In proceedings of the 32nd ASME Wind Energy Symposium, 51st AIAA
!    Aerospace Sciences Meeting including the New Horizons Forum and Aerospace Exposition, Grapevine, TX, January 7-10,
!    2013.   Also published as NREL Report No. CP-2C00-57298.   Available in pdf format at:
!    http://www.nrel.gov/docs/fy13osti/57298.pdf
!
!**********************************************************************************************************************************
MODULE IceFloe

   use IceFloe_Types, dum1=>errID_none, dum2=>aborterrlev, dum3=>errID_warn, dum4=>errID_fatal, dum5=>newline
   use IceInputParams
   use IceCrushingISO
   use IceCrushingIEC
   use IceIntermittentCrushing
   use IceFlexISO
   use IceFlexIEC
   use IceLockInCrushingISO
   use randomCrushing
   use IceCpldCrushing
   use iso_c_binding
   use NWTC_IO, only : DispNVD

   IMPLICIT NONE

   PRIVATE

   character(*), parameter    :: Progname_ice = 'IceFloe'
   character(*), parameter    :: Progver_ice  = 'v1.00.00'
   character(*), parameter    :: Progdate_ice = 'August-2013'
   TYPE(ProgDesc), PARAMETER  :: IceFloe_Ver = ProgDesc( Progname_ice, Progver_ice, Progdate_ice )

! ..... Public Subroutines ...................................................................................................

   PUBLIC :: IceFloe_Init                           ! Initialization routine
   PUBLIC :: IceFloe_End                            ! Ending routine (includes clean up)

   PUBLIC :: IceFloe_UpdateStates                   ! Loose coupling routine for solving for constraint states, integrating
                                                    !   continuous states, and updating discrete states
   PUBLIC :: IceFloe_UpdateDiscState                ! Tight coupling routine for updating discrete states
   PUBLIC :: IceFloe_CalcOutput                     ! Routine for computing outputs

!   PUBLIC :: IceFloe_CalcConstrStateResidual        ! Tight coupling routine for returning the constraint state residual
!   PUBLIC :: IceFloe_CalcContStateDeriv             ! Tight coupling routine for computing derivatives of continuous states

CONTAINS

!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE IceFloe_Init( InitInp, u, p, x, xd, z, OtherState, y, Interval, InitOut, ErrStat, ErrMsg )
!
! This routine is called at the start of the simulation to perform initialization steps.
! The parameters are set here and not changed during the simulation.
! The initial states and initial guess for the input are defined.
!..................................................................................................................................

      TYPE(IceFloe_InitInputType),       INTENT(  IN )  :: InitInp     ! Input data from FAST for initialization routine
      TYPE(IceFloe_InputType),           INTENT(  OUT)  :: u           ! An initial guess for the input
      TYPE(IceFloe_ParameterType),       INTENT(  OUT)  :: p           ! Parameters
      TYPE(IceFloe_OutputType),          INTENT(  OUT)  :: y           ! Initial system outputs (outputs are not calculated;
                                                                      !    only the output mesh is initialized)
      REAL(DbKi),                       INTENT(INOUT)  :: Interval    ! Coupling interval in seconds: the rate that
                                                                      !   (1) IceFloe_UpdateStates() is called in loose coupling &
                                                                      !   (2) IceFloe_UpdateDiscState() is called in tight coupling.
                                                                      !   Input is the suggested time from the glue code;
                                                                      !   Output is the actual coupling interval that will be used
                                                                      !   by the glue code.
      TYPE(IceFloe_InitOutputType),      INTENT(  OUT)  :: InitOut     ! Output for initialization routine
      INTEGER(IntKi),                   INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                     INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None

! Below are not currently used in IceFloe - dummy variables and stubbed routines are included however
      TYPE(IceFloe_ContinuousStateType), INTENT(  OUT)  :: x           ! Initial continuous states
      TYPE(IceFloe_DiscreteStateType),   INTENT(  OUT)  :: xd          ! Initial discrete states
      TYPE(IceFloe_ConstraintStateType), INTENT(  OUT)  :: z           ! Initial guess of the constraint states
      TYPE(IceFloe_OtherStateType),      INTENT(  OUT)  :: OtherState  ! Initial other/optimization states

! More (unregistered) IceFloe types
      type(iceFloeSaveParams)    :: icep     ! Parameters that mirror the iceFloe_parameterTypes from FAST
      type(iceInputType)         :: iceInput ! hold list of input names and values from file
      type(iceFloe_LoggingType)  :: iceLog   ! structure with message and error logging variables
      character(132)             :: logFile  ! File name for message logging

! Other local variables
      real(ReKi)                 :: duration
      INTEGER(IntKi)             :: nSteps      ! number of steps in load time series
      INTEGER(IntKi)             :: numLdsOut   ! number of load output points, could be numlegs or 1 for single load only
      INTEGER(IntKi)             :: Err         ! for array allocation error
      INTEGER(IntKi)             :: n

      p%initFlag = .false.

    ! Define initial system states here:

      x%DummyContStateVar = 0.

      ! Initialize the NWTC Subroutine Library
!      CALL NWTC_Init( )

      ! Display the module information
      CALL DispNVD( IceFloe_Ver )

      call NameOFile ( 1, 'log', logFile )   ! The filename comes from the command line

!   Set up error logging
      iceLog%warnFlag = .false.
      iceLog%ErrID    = ErrID_None
      iceLog%ErrMsg   = ""
      
      call openIceLog (iceLog, logFile)
      if (iceLog%ErrID >= AbortErrLev) then   ! Couldn't open the message logging fle, so this error won't be logged!
         ErrStat = iceLog%ErrID
         ErrMsg = 'Fatal error in routine: IceFloe_Init => '//trim(iceLog%ErrMsg)
         return   
      endif

      call logMessage(iceLog, ' Running: '//ProgName_ice//', '//ProgVer_ice//', '//Progdate_ice)
      call logMessage(iceLog, ' This run started on: '//curdate()//' at '//curtime()//newLine)

   ! go through the inputs: first count them then read them into a structure
      call countIceInputs(InitInp%inputFile, iceLog, iceInput)
      call readIceInputs(iceLog, iceInput)
      if (iceLog%ErrID >= AbortErrLev) then   ! Couldn't open the parameter input fle
         ErrStat = iceLog%ErrID
         ErrMsg  = 'Fatal error in routine: IceFloe_Init=> '//trim(iceLog%ErrMsg)
         return   
      endif
      call logMessage(iceLog, ' Parameter input file read finished.'//newLine)

   ! get selected input parameter
      call getIceInput(iceInput, 'iceType',icep%iceType, iceLog, lowTypeLimit, hiTypeLimit)
      if (iceLog%ErrID >= AbortErrLev) then
         ErrStat = iceLog%ErrID
         ErrMsg = 'Error retrieving ice type from inputs in IceFloe_Init '//newLine//trim(iceLog%ErrMsg)
         return   
      endif

   !  Set the time step as the minimum of the suggested interval and the time step from the ice input file
      call getIceInput(iceInput, 'timeStep',icep%dt, iceLog, 0.0)
      if (iceLog%ErrID >= AbortErrLev) then
         ErrStat = iceLog%ErrID
         ErrMsg = 'Error retrieving time step from inputs in IceFloe_Init '//newLine//trim(iceLog%ErrMsg)
         return   
      endif

   !  Select the smallest time step between calling program suggestion and user input
   !  Then store as a parameter
      icep%dt = min(icep%dt, REAL(interval,ReKi) )                      ! bjj: if icep%dt == interval, the type casting may be a problem.
      interval = icep%dt   ! for return back to the calling program     ! bjj: icep%dt is single precision; interval is double; FAST may complain that the icep%dt isn't an integer divisor of interval

   ! get the duration of the simulation
   ! this should eventually come from FAST
      call getIceInput(iceInput, 'duration', duration, iceLog, 0.0)
      call logMessage(iceLog, ' Load time series duration = '//TRIM(Num2LStr(duration))//' sec')

   ! get the number of legs on the support structure
      call getIceInput(iceInput, 'numLegs', icep%numLegs, iceLog, 1, 4)
      if (iceLog%ErrID >= AbortErrLev) then
         ErrStat = iceLog%ErrID
         ErrMsg = 'Error retrieving duration or number of legs from ice input file '//newLine//trim(iceLog%ErrMsg)
         return   
      endif

   ! allocate storage for load series
      nSteps = ceiling(duration/icep%dt) + 1  ! + 1 for zero point
      allocate(p%loadSeries(nSteps, icep%numLegs), stat=err)
      if (err /= 0) then
         call iceErrorHndlr (iceLog, ErrID_Fatal, 'Error in time series array allocation in IceFloe_init', 1)
         ErrStat = iceLog%ErrID
         ErrMsg  = trim(iceLog%ErrMsg)
         return
      endif   
   ! point internal iceFloe array to the load series stored by FAST
   ! this is a bit tricky because FAST registered types can't be set as targets
      call c_f_pointer(c_loc(p%loadseries), icep%loadSeries, [nSteps,icep%numLegs])

   ! allocate storage for the leg positions
      allocate (p%legX(icep%numLegs), stat=err)
      allocate (p%legY(icep%numLegs), stat=err)
      allocate (p%ks(icep%numLegs), stat=err)
      if (err /= 0) then
         call iceErrorHndlr (iceLog, ErrID_Fatal, 'Error in allocation of leg data in parameters', 1)
         return  !  error in array allocation
      endif   
   ! point internal iceFloe parameter array to leg positions stored by FAST
      call c_f_pointer(c_loc(p%legX), icep%legX, [icep%numLegs])
      call c_f_pointer(c_loc(p%legY), icep%legY, [icep%numLegs])
      call c_f_pointer(c_loc(p%ks), icep%ks, [icep%numLegs])

      iceType: select case (icep%iceType)
        case (randomCrush)
           call initRandomCrushing(iceInput, icep, iceLog)
           if (iceLog%ErrID <= ErrID_Warn)  &
              call logMessage(iceLog, newLine//' Random continuous ice crushing via ISO/Karna initialized'//newLine)
        case (interCrush)
           call initInterCrushing(iceInput, icep, iceLog)
           if (iceLog%ErrID <= ErrID_Warn)  &
              call logMessage(iceLog, newLine//' Intermittent ice crushing loads initialized'//newLine)
        case (crushIEC)
           call initCrushingIEC(iceInput, icep, iceLog)
           if (iceLog%ErrID <= ErrID_Warn)  &
              call logMessage(iceLog, newLine//' Ice crushing loads IEC/Korzhavin initialized'//newLine)
        case (flexFailISO)
           call initFlexISO(iceInput, icep, iceLog)
           if (iceLog%ErrID <= ErrID_Warn)  &
              call logMessage(iceLog, newLine//' ISO/Croasdale ice flexural failure loads initialized'//newLine)
        case (flexFailIEC)
           call initFlexIEC(iceInput, icep, iceLog)
           if (iceLog%ErrID <= ErrID_Warn)  &
              call logMessage(iceLog, newLine//' IEC/Ralston ice flexural failure loads initialized'//newLine)
        case (lockInISO)
           call initLockInCrushingISO(iceInput, icep, iceLog)
           if (iceLog%ErrID <= ErrID_Warn)  &
              call logMessage(iceLog, newLine//' Frequency lock-in ice crushing loads via ISO initialized'//newLine)
        case (cpldCrush)
           call initCpldCrushing(iceInput, icep, iceLog)
           if (iceLog%ErrID <= ErrID_Warn)  &
              call logMessage(iceLog, newLine//' Coupled crushing ice loads (Maattanen) initialized'//newLine)
        case default
           call iceErrorHndlr(iceLog, ErrID_Fatal, 'Invalid ice type, '//TRIM(Num2LStr(icep%IceType))//' is not a valid selection', 1)
      end select iceType

 ! initialize mesh for tower/leg load point velocities for input to iceFloe
      numLdsOut = icep%numLegs
      if (icep%singleLoad) numLdsOut = 1
      CALL MeshCreate( BlankMesh       = u%iceMesh              &
                      ,IOS             = COMPONENT_INPUT        &
                      ,NNodes          = numLdsOut              &
                      ,TranslationVel  = .TRUE.                 &
                      ,nScalars        = 0                      &
                      ,ErrStat         = ErrStat                &
                      ,ErrMess         = ErrMsg                 )
      call iceErrorHndlr(iceLog, ErrStat, ErrMsg//' in IceFloe_init ', 1)
      
      do n = 1, numLdsOut
         CALL MeshConstructElement ( Mesh     = u%iceMesh          &
                                   , Xelement = ELEMENT_POINT      &
                                   , P1       = n                  &
                                   , ErrStat  = ErrStat            &
                                   , ErrMess  = ErrMsg             )
         call iceErrorHndlr(iceLog, ErrStat, ErrMsg//' in IceFloe_init ', 1)

         CALL MeshPositionNode ( Mesh = u%iceMesh                    &
                               , INode = n                           &
                               , Pos = (/icep%legX(n), icep%legX(n), 0.0/) &  ! TODO get these from FAST, Z would be height at water line
                               , ErrStat   = ErrStat                 &
                               , ErrMess   = ErrMsg                  )
         call iceErrorHndlr(iceLog, ErrStat, ErrMsg//' in IceFloe_init ', 1)
      enddo

      CALL MeshCommit ( Mesh    = u%iceMesh        &
                       ,ErrStat = ErrStat          &
                       ,ErrMess = ErrMsg           )
      call iceErrorHndlr(iceLog, ErrStat, ErrMsg//' in IceFloe_init ', 1)

!  Set up the mesh for load output from iceFloe
      CALL MeshCopy ( SrcMesh  = u%iceMesh         &
                    , DestMesh = y%iceMesh         &
                    , CtrlCode = MESH_SIBLING      &
                    , Force    = .TRUE.            &
                    , Moment   = .TRUE.            &
                    , ErrStat  = ErrStat           &
                    , ErrMess  = ErrMsg            )
      call iceErrorHndlr(iceLog, ErrStat, ErrMsg//' in IceFloe_init ', 1)

   !  Assign initial values, all nodes and components at once
      u%iceMesh%TranslationVel = 0.0
      y%iceMesh%Force          = 0.0
      y%iceMesh%Moment         = 0.0
      
   !  Let the user know if there have been warnings
      if (iceLog%WarnFlag) then
         call addMessage (iceLog, 'Warning message(s) in routine IceFloe_Init, please see the IceFloe log file')
      endif

   !  return message and error logging data
      ErrStat = iceLog%ErrID
      ErrMsg  = trim(iceLog%ErrMsg)

      if (ErrStat <= ErrID_Warn) p%initFlag = .true.

   ! Map iceFloe parameters to FAST parameter type
      p%iceVel          = icep%iceVel
      p%iceDirection    = icep%iceDirection
      p%minStrength     = icep%minStrength
      p%minStrengthNegVel = icep%minStrengthNegVel
      p%minStressRate   = icep%minStressRate
      p%crushArea       = icep%crushArea
      p%coeffStressRate = icep%coeffStressRate
      p%C               = icep%C
      p%dt              = icep%dt
      p%numLegs         = icep%numLegs
      p%iceType         = icep%iceType
      p%singleLoad      = icep%singleLoad
      p%logUnitNum      = iceLog%unitNum

   !  deallocate pointers
      icep%loadSeries => null()
      icep%legX => null()
      icep%legY => null()
      icep%ks   => null()

END SUBROUTINE IceFloe_Init

!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE IceFloe_CalcOutput( t, u, p, x, xd, z, OtherState, y, ErrStat, ErrMsg )
!
! Routine for computing outputs, used in both loose and tight coupling.
!..................................................................................................................................

      REAL(DbKi),                       INTENT(IN   )  :: t           ! Current simulation time in seconds
      TYPE(IceFloe_InputType),           INTENT(IN   )  :: u           ! Inputs at t
      TYPE(IceFloe_ParameterType),       INTENT(IN   )  :: p           ! Parameters
      TYPE(IceFloe_OutputType),          INTENT(INOUT)  :: y           ! Outputs computed at t (Input only so that mesh con-
                                                                    !   nectivity information does not have to be recalculated)
      INTEGER(IntKi),                   INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                     INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None

! Below are not currently used in IceFloe - dummy variables and stubbed routines are included however
      TYPE(IceFloe_ContinuousStateType), INTENT(IN   )  :: x           ! Continuous states at t
      TYPE(IceFloe_DiscreteStateType),   INTENT(IN   )  :: xd          ! Discrete states at t
      TYPE(IceFloe_ConstraintStateType), INTENT(IN   )  :: z           ! Constraint states at t
      TYPE(IceFloe_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states

! IceFloe specific types and other internal variables
      type(iceFloeSaveParams)     :: icep     ! Parameters that mirror the iceFloe_parameterTypes from FAST
      type(iceFloe_LoggingType)   :: iceLog                  ! structure with message and error logging variables
      real(ReKi)                 :: loadVect(6,p%numLegs)
      real(ReKi)                 :: inVels(2,p%numLegs)
      integer(IntKi)             :: nL, nSteps

!  map FAST parameter type to iceFloe internal parameters
      icep%iceVel          = p%iceVel
      icep%iceDirection    = p%iceDirection
      icep%minStrength     = p%minStrength
      icep%minStrengthNegVel  = p%minStrengthNegVel
      icep%minStressRate   = p%minStressRate
      icep%crushArea       = p%crushArea
      icep%coeffStressRate = p%coeffStressRate
      icep%C               = p%C
      icep%dt              = p%dt
      icep%numLegs         = p%numLegs
      icep%iceType         = p%iceType
      icep%singleLoad      = p%singleLoad
   ! point internal iceFloe parameter array to load time series stored by FAST
      nSteps = size(p%loadSeries,1)
      call c_f_pointer(c_loc(p%loadSeries), icep%loadSeries, [nSteps, icep%numLegs])
   ! point internal iceFloe parameter array to leg positions stored by FAST
      call c_f_pointer(c_loc(p%legX), icep%legX, [icep%numLegs])
      call c_f_pointer(c_loc(p%legY), icep%legY, [icep%numLegs])
      call c_f_pointer(c_loc(p%ks), icep%ks, [icep%numLegs])

!   Re-Set up error logging
      iceLog%unitNum  = p%logUnitNum
      iceLog%warnFlag = .false.
      iceLog%ErrID    = ErrID_None
      iceLog%ErrMsg   = ""

!   Check for successful initialization
      if (.not.p%initFlag) then
         call iceErrorHndlr (iceLog, ErrID_Fatal, 'Attempt to calculate ice loads without a successful initialization', 1)
         return      
      endif

!  Map input point velocities to vectors for iceFloe
      do nL = 1, p%numLegs
         if (p%singleLoad) then
            inVels(1,nL) = u%iceMesh%TranslationVel(1,1)
            inVels(2,nL) = u%iceMesh%TranslationVel(2,1)
         else
            inVels(1,nL) = u%iceMesh%TranslationVel(1,nL)
            inVels(2,nL) = u%iceMesh%TranslationVel(2,nL)
         endif
      enddo

!  get loads from IceFloe
      iceType: select case (p%iceType)
        case (randomCrush)
           loadVect = outputRandomCrushLoad(icep, iceLog, t)
        case (crushIEC)
           loadVect = outputCrushLoadIEC(icep, iceLog, t)
        case (interCrush)
           loadVect = outputInterCrushLoad(icep, iceLog, t)
        case (flexFailISO)
           loadVect = outputFlexLoadISO(icep, iceLog, t)
        case (flexFailIEC)
           loadVect = outputFlexLoadIEC(icep, iceLog, t)
        case (lockInISO)
           loadVect = outputLockInLoadISO(icep, iceLog, t)
        case (cpldCrush)
           loadVect = outputCpldCrushLoad(icep, iceLog, inVels, t)
        case default   ! this should have been caught during init but just in case
           call iceErrorHndlr (iceLog, ErrID_Fatal, 'Invalid ice floe type, '//TRIM(Num2LStr(p%IceType))//     &
                                                    ' is not a valid selection', 1)
      end select iceType

   !  Map the ice load vectors from IceFloe onto the output mesh
      if (p%singleLoad) then
         y%iceMesh%Force(:,1)  = loadVect(1:3,1)
         y%iceMesh%Moment(:,1) = loadVect(4:6,1)
      else         
         y%iceMesh%Force  = loadVect(1:3,:)
         y%iceMesh%Moment = loadVect(4:6,:)
      endif

   !  Let the user know if there have been warnings
      if (iceLog%WarnFlag) then
         call addMessage (iceLog, 'Warning message(s) in routine IceFloe_Init, please see the IceFloe log file')
      endif

   !  return message and error logging data
      ErrStat = iceLog%ErrID
      ErrMsg  = trim(iceLog%ErrMsg)

   !  deallocate pointers
      icep%loadSeries => null()
      icep%legX => null()
      icep%legY => null()
      icep%ks   => null()

END SUBROUTINE IceFloe_CalcOutput

!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE IceFloe_End( u, p, x, xd, z, OtherState, y, ErrStat, ErrMsg )
!
! This routine is called at the end of the simulation.
!..................................................................................................................................

      TYPE(IceFloe_InputType),           INTENT(INOUT)  :: u           ! System inputs
      TYPE(IceFloe_ParameterType),       INTENT(INOUT)  :: p           ! Parameters
      TYPE(IceFloe_ContinuousStateType), INTENT(INOUT)  :: x           ! Continuous states
      TYPE(IceFloe_DiscreteStateType),   INTENT(INOUT)  :: xd          ! Discrete states
      TYPE(IceFloe_ConstraintStateType), INTENT(INOUT)  :: z           ! Constraint states
      TYPE(IceFloe_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states
      TYPE(IceFloe_OutputType),          INTENT(INOUT)  :: y           ! System outputs
      INTEGER(IntKi),                   INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                     INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None

      type(iceFloe_LoggingType)   :: iceLog   ! structure with message and error logging variables

!   Set up error logging
      iceLog%unitNum  = p%logUnitNum
      iceLog%warnFlag = .false.
      iceLog%ErrID    = ErrID_None
      iceLog%ErrMsg   = ""

      ! Place any last minute operations or calculations here:

      ! Close files here:

      ! Destroy the input data:

      CALL IceFloe_DestroyInput( u, ErrStat, ErrMsg )

      ! Destroy the parameter data:

      CALL IceFloe_DestroyParam( p, ErrStat, ErrMsg )

      ! Destroy the state data:

      CALL IceFloe_DestroyContState(   x,           ErrStat, ErrMsg )
      call iceErrorHndlr (iceLog, ErrStat, ErrMsg, 1)
      CALL IceFloe_DestroyDiscState(   xd,          ErrStat, ErrMsg )
      call iceErrorHndlr (iceLog, ErrStat, ErrMsg, 1)
      CALL IceFloe_DestroyConstrState( z,           ErrStat, ErrMsg )
      call iceErrorHndlr (iceLog, ErrStat, ErrMsg, 1)
      CALL IceFloe_DestroyOtherState(  OtherState,  ErrStat, ErrMsg )
      call iceErrorHndlr (iceLog, ErrStat, ErrMsg, 1)

      ! Destroy the output data:

      CALL IceFloe_DestroyOutput( y, ErrStat, ErrMsg )
      call iceErrorHndlr (iceLog, ErrStat, ErrMsg, 1)

      call logMessage(iceLog, newLine//' IceFloe run complete on: '//curdate()//' at '//curtime())

   !  Let the user know if there have been warnings
      if (iceLog%WarnFlag) then
         call addMessage (iceLog, 'Warning message(s) in routine IceFloe_Init, please see the IceFloe log file')
      endif

   !  return message and error logging data
      ErrStat = iceLog%ErrID
      ErrMsg  = trim(iceLog%ErrMsg)

      call closeIceLog(iceLog)

END SUBROUTINE IceFloe_End

!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE IceFloe_UpdateStates( t, n, u, utimes, p, x, xd, z, OtherState, ErrStat, ErrMsg )
!
! Routine for solving for constraint states, integrating continuous states, and updating discrete states
! Constraint states are solved for input t; Continuous and discrete states are updated for t + p%dt
! (stepsize dt assumed to be in ModName parameter)
!..................................................................................................................................

      REAL(DbKi),                         INTENT(IN   ) :: t          ! Current simulation time in seconds
      INTEGER(IntKi),                     INTENT(IN   ) :: n          ! Current simulation time step n = 0,1,...
      TYPE(IceFloe_InputType),             INTENT(IN   ) :: u(:)       ! Inputs at utimes
      REAL(DbKi),                         INTENT(IN   ) :: utimes(:)  ! Times associated with u(:), in seconds
      TYPE(IceFloe_ParameterType),         INTENT(IN   ) :: p          ! Parameters
      TYPE(IceFloe_ContinuousStateType),   INTENT(INOUT) :: x          ! Input: Continuous states at t;
                                                                      !   Output: Continuous states at t + Interval
      TYPE(IceFloe_DiscreteStateType),     INTENT(INOUT) :: xd         ! Input: Discrete states at t;
                                                                      !   Output: Discrete states at t  + Interval
      TYPE(IceFloe_ConstraintStateType),   INTENT(INOUT) :: z          ! Input: Constraint states at t;
                                                                      !   Output: Constraint states at t+dt
      TYPE(IceFloe_OtherStateType),        INTENT(INOUT) :: OtherState ! Other/optimization states
      INTEGER(IntKi),                     INTENT(  OUT) :: ErrStat    ! Error status of the operation
      CHARACTER(*),                       INTENT(  OUT) :: ErrMsg     ! Error message if ErrStat /= ErrID_None

      ! local variables

      TYPE(IceFloe_InputType)            :: u_interp  ! input interpolated from given u at utimes
      TYPE(IceFloe_ContinuousStateType)  :: xdot      ! continuous state time derivative

      type(iceFloe_LoggingType)   :: iceLog   ! structure with message and error logging variables

!   Set up error logging
      iceLog%unitNum = p%logUnitNum
      iceLog%warnFlag = .false.
      iceLog%ErrID    = ErrID_None
      iceLog%ErrMsg   = ""

      ! Update discrete states here:

!      xd%DummyContStateVar = 0.0

   !  Let the user know if there have been warnings
      if (iceLog%WarnFlag) then
         call addMessage (iceLog, 'Warning message(s) in routine IceFloe_Init, please see the IceFloe log file')
      endif

   !  return message and error logging data
      ErrStat = iceLog%ErrID
      ErrMsg  = trim(iceLog%ErrMsg)

END SUBROUTINE IceFloe_UpdateStates

!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE IceFloe_UpdateDiscState( t, n, u, p, x, xd, z, OtherState, ErrStat, ErrMsg )
!
! Routine for updating discrete states
!..................................................................................................................................

      REAL(DbKi),                       INTENT(IN   )  :: t           ! Current simulation time in seconds
      INTEGER(IntKi),                   INTENT(IN   )  :: n           ! Current step of the simulation: t = n*Interval
      TYPE(IceFloe_InputType),           INTENT(IN   )  :: u           ! Inputs at t
      TYPE(IceFloe_ParameterType),       INTENT(IN   )  :: p           ! Parameters
      TYPE(IceFloe_ContinuousStateType), INTENT(IN   )  :: x           ! Continuous states at t
      TYPE(IceFloe_DiscreteStateType),   INTENT(INOUT)  :: xd          ! Input: Discrete states at t;
                                                                    !   Output: Discrete states at t + Interval
      TYPE(IceFloe_ConstraintStateType), INTENT(IN   )  :: z           ! Constraint states at t
      TYPE(IceFloe_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states
      INTEGER(IntKi),                   INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                     INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None

      type(iceFloe_LoggingType)   :: iceLog   ! structure with message and error logging variables

!   Set up error logging
      iceLog%unitNum = p%logUnitNum
      iceLog%warnFlag = .false.
      iceLog%ErrID    = ErrID_None
      iceLog%ErrMsg   = ""

!   Update discrete states here:

      xd%DummyDiscStateVar = 0.0

   !  Let the user know if there have been warnings
      if (iceLog%WarnFlag) then
         call addMessage (iceLog, 'Warning message(s) in routine IceFloe_Init, please see the IceFloe log file')
      endif

   !  return message and error logging data
      ErrStat = iceLog%ErrID
      ErrMsg  = trim(iceLog%ErrMsg)

END SUBROUTINE IceFloe_UpdateDiscState
!----------------------------------------------------------------------------------------------------------------------------------

END MODULE IceFloe

!**********************************************************************************************************************************
