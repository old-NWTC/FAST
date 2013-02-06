!**********************************************************************************************************************************
! The Controls and Controls_Types modules make up a template for creating user-defined calculations in the FAST Modularization
! Framework. Controls_Types will be auto-generated based on a description of the variables for the module.
!
! "ModuleName" should be replaced with the name of your module. Example: Controls
! "ModName" (in ModName_*) should be replaced with the module name or an abbreviation of it. Example: Ctrl
!..................................................................................................................................
! LICENSING
! Copyright (C) 2013  National Renewable Energy Laboratory
!
!    This file is part of FAST's Controls Module, "Controls".
!
!    Controls is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as
!    published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
!    of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License along with Controls.
!    If not, see <http://www.gnu.org/licenses/>.
!
!**********************************************************************************************************************************
MODULE Controls

   USE Controls_Types
   USE NWTC_Library

   IMPLICIT NONE

   PRIVATE

   TYPE(ProgDesc), PARAMETER            :: Ctrl_Ver = ProgDesc( 'Controls', 'v1.00.00', '31-March-2013' )


      ! ..... Public Subroutines ...................................................................................................

   PUBLIC :: Ctrl_Init                           ! Initialization routine
   PUBLIC :: Ctrl_End                            ! Ending routine (includes clean up)

   PUBLIC :: Ctrl_UpdateStates                   ! Loose coupling routine for solving for constraint states, integrating
                                                 !   continuous states, and updating discrete states
   PUBLIC :: Ctrl_CalcOutput                     ! Routine for computing outputs

   PUBLIC :: Ctrl_CalcConstrStateResidual        ! Tight coupling routine for returning the constraint state residual
   PUBLIC :: Ctrl_CalcContStateDeriv             ! Tight coupling routine for computing derivatives of continuous states
   PUBLIC :: Ctrl_UpdateDiscState                ! Tight coupling routine for updating discrete states

   !PUBLIC :: Ctrl_JacobianPInput                 ! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete-
   !                                              !   (Xd), and constraint-state (Z) equations all with respect to the inputs (u)
   !PUBLIC :: Ctrl_JacobianPContState             ! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete-
   !                                              !   (Xd), and constraint-state (Z) equations all with respect to the continuous
   !                                              !   states (x)
   !PUBLIC :: Ctrl_JacobianPDiscState             ! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete-
   !                                              !   (Xd), and constraint-state (Z) equations all with respect to the discrete
   !                                              !   states (xd)
   !PUBLIC :: Ctrl_JacobianPConstrState           ! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete-
   !                                              !   (Xd), and constraint-state (Z) equations all with respect to the constraint
   !                                              !   states (z)
   
CONTAINS
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE Ctrl_Init( InitInp, u, p, x, xd, z, OtherState, y, Interval, InitOut, ErrStat, ErrMsg )
! This routine is called at the start of the simulation to perform initialization steps.
! The parameters are set here and not changed during the simulation.
! The initial states and initial guess for the input are defined.
!..................................................................................................................................

      TYPE(Ctrl_InitInputType),       INTENT(IN   )  :: InitInp     ! Input data for initialization routine
      TYPE(Ctrl_InputType),           INTENT(  OUT)  :: u           ! An initial guess for the input; input mesh must be defined
      TYPE(Ctrl_ParameterType),       INTENT(  OUT)  :: p           ! Parameters
      TYPE(Ctrl_ContinuousStateType), INTENT(  OUT)  :: x           ! Initial continuous states
      TYPE(Ctrl_DiscreteStateType),   INTENT(  OUT)  :: xd          ! Initial discrete states
      TYPE(Ctrl_ConstraintStateType), INTENT(  OUT)  :: z           ! Initial guess of the constraint states
      TYPE(Ctrl_OtherStateType),      INTENT(  OUT)  :: OtherState  ! Initial other/optimization states
      TYPE(Ctrl_OutputType),          INTENT(  OUT)  :: y           ! Initial system outputs (outputs are not calculated;
                                                                    !   only the output mesh is initialized)
      REAL(DbKi),                     INTENT(INOUT)  :: Interval    ! Coupling interval in seconds: the rate that
                                                                    !   (1) Ctrl_UpdateStates() is called in loose coupling &
                                                                    !   (2) Ctrl_UpdateDiscState() is called in tight coupling.
                                                                    !   Input is the suggested time from the glue code;
                                                                    !   Output is the actual coupling interval that will be used
                                                                    !   by the glue code.
      TYPE(Ctrl_InitOutputType),      INTENT(  OUT)  :: InitOut     ! Output for initialization routine
      INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None

         ! local variables

      INTEGER(IntKi)                                 :: NumOuts


         ! Initialize variables

      ErrStat = ErrID_None
      ErrMsg  = ""
      NumOuts = 2


         ! Initialize the NWTC Subroutine Library

      CALL NWTC_Init( )

         ! Display the module information

      CALL DispNVD( Ctrl_Ver )


         ! Define parameters here:

      p%DT  = Interval


         ! Define initial system states here:

      x%DummyContState           = 0
      xd%DummyDiscState          = 0
      z%DummyConstrState         = 0
      OtherState%DummyOtherState = 0


         ! Define initial guess for the system inputs here:

      u%DummyInput = 0


         ! Define system output initializations (set up mesh) here:
      ALLOCATE( y%WriteOutput(NumOuts), STAT = ErrStat )
      IF ( ErrStat/= 0 ) THEN
         ErrStat = ErrID_Fatal
         ErrMsg  = 'Error allocating output header and units arrays in Ctrl_Init'
         RETURN
      END IF

      y%WriteOutput = 0


         ! Define initialization-routine output here:
      ALLOCATE( InitOut%WriteOutputHdr(NumOuts), InitOut%WriteOutputUnt(NumOuts), STAT = ErrStat )
      IF ( ErrStat/= 0 ) THEN
         ErrStat = ErrID_Fatal
         ErrMsg  = 'Error allocating output header and units arrays in Ctrl_Init'
         RETURN
      END IF

      InitOut%WriteOutputHdr = (/ 'Time   ', 'Column2' /)
      InitOut%WriteOutputUnt = (/ '(s)',     '(-)'     /)


         ! If you want to choose your own rate instead of using what the glue code suggests, tell the glue code the rate at which
         !   this module must be called here:

       !Interval = p%DT


END SUBROUTINE Ctrl_Init
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE Ctrl_End( u, p, x, xd, z, OtherState, y, ErrStat, ErrMsg )
! This routine is called at the end of the simulation.
!..................................................................................................................................

      TYPE(Ctrl_InputType),           INTENT(INOUT)  :: u           ! System inputs
      TYPE(Ctrl_ParameterType),       INTENT(INOUT)  :: p           ! Parameters
      TYPE(Ctrl_ContinuousStateType), INTENT(INOUT)  :: x           ! Continuous states
      TYPE(Ctrl_DiscreteStateType),   INTENT(INOUT)  :: xd          ! Discrete states
      TYPE(Ctrl_ConstraintStateType), INTENT(INOUT)  :: z           ! Constraint states
      TYPE(Ctrl_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states
      TYPE(Ctrl_OutputType),          INTENT(INOUT)  :: y           ! System outputs
      INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None



         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Place any last minute operations or calculations here:


         ! Close files here:



         ! Destroy the input data:

      CALL Ctrl_DestroyInput( u, ErrStat, ErrMsg )


         ! Destroy the parameter data:

      CALL Ctrl_DestroyParam( p, ErrStat, ErrMsg )


         ! Destroy the state data:

      CALL Ctrl_DestroyContState(   x,           ErrStat, ErrMsg )
      CALL Ctrl_DestroyDiscState(   xd,          ErrStat, ErrMsg )
      CALL Ctrl_DestroyConstrState( z,           ErrStat, ErrMsg )
      CALL Ctrl_DestroyOtherState(  OtherState,  ErrStat, ErrMsg )


         ! Destroy the output data:

      CALL Ctrl_DestroyOutput( y, ErrStat, ErrMsg )




END SUBROUTINE Ctrl_End
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE Ctrl_UpdateStates( Time, u, p, x, xd, z, OtherState, ErrStat, ErrMsg )
! Loose coupling routine for solving for constraint states, integrating continuous states, and updating discrete states
! Constraint states are solved for input Time; Continuous and discrete states are updated for Time + Interval
!..................................................................................................................................

      REAL(DbKi),                      INTENT(IN   ) :: Time        ! Current simulation time in seconds
      TYPE(Ctrl_InputType),            INTENT(IN   ) :: u           ! Inputs at Time
      TYPE(Ctrl_ParameterType),        INTENT(IN   ) :: p           ! Parameters
      TYPE(Ctrl_ContinuousStateType),  INTENT(INOUT) :: x           ! Input: Continuous states at Time;
                                                                    !   Output: Continuous states at Time + Interval
      TYPE(Ctrl_DiscreteStateType),    INTENT(INOUT) :: xd          ! Input: Discrete states at Time;
                                                                    !   Output: Discrete states at Time  + Interval
      TYPE(Ctrl_ConstraintStateType),  INTENT(INOUT) :: z           ! Input: Initial guess of constraint states at Time;
                                                                    !   Output: Constraint states at Time
      TYPE(Ctrl_OtherStateType),       INTENT(INOUT) :: OtherState  ! Other/optimization states
      INTEGER(IntKi),                  INTENT(  OUT) :: ErrStat     ! Error status of the operation
      CHARACTER(*),                    INTENT(  OUT) :: ErrMsg      ! Error message if ErrStat /= ErrID_None

         ! Local variables

      TYPE(Ctrl_ContinuousStateType)                 :: dxdt        ! Continuous state derivatives at Time
      TYPE(Ctrl_ConstraintStateType)                 :: z_Residual  ! Residual of the constraint state equations (Z)

      INTEGER(IntKi)                                 :: ErrStat2    ! Error status of the operation (occurs after initial error)
      CHARACTER(LEN(ErrMsg))                         :: ErrMsg2     ! Error message if ErrStat2 /= ErrID_None

         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""



         ! Solve for the constraint states (z) here:

         ! Check if the z guess is correct and update z with a new guess.
         ! Iterate until the value is within a given tolerance.

      CALL Ctrl_CalcConstrStateResidual( Time, u, p, x, xd, z, OtherState, z_Residual, ErrStat, ErrMsg )
      IF ( ErrStat >= AbortErrLev ) THEN
         CALL Ctrl_DestroyConstrState( z_Residual, ErrStat2, ErrMsg2)
         ErrMsg = TRIM(ErrMsg)//' '//TRIM(ErrMsg2)
         RETURN
      END IF

      ! DO WHILE ( z_Residual% > tolerance )
      !
      !  z =
      !
      !  CALL Ctrl_CalcConstrStateResidual( Time, u, p, x, xd, z, OtherState, z_Residual, ErrStat, ErrMsg )
      !  IF ( ErrStat >= AbortErrLev ) THEN
      !     CALL Ctrl_DestroyConstrState( z_Residual, ErrStat2, ErrMsg2)
      !     ErrMsg = TRIM(ErrMsg)//' '//TRIM(ErrMsg2)
      !     RETURN
      !  END IF
      !
      ! END DO


         ! Destroy z_Residual because it is not necessary for the rest of the subroutine:

      CALL Ctrl_DestroyConstrState( z_Residual, ErrStat, ErrMsg)
      IF ( ErrStat >= AbortErrLev ) RETURN



         ! Get first time derivatives of continuous states (dxdt):

      CALL Ctrl_CalcContStateDeriv( Time, u, p, x, xd, z, OtherState, dxdt, ErrStat, ErrMsg )
      IF ( ErrStat >= AbortErrLev ) THEN
         CALL Ctrl_DestroyContState( dxdt, ErrStat2, ErrMsg2)
         ErrMsg = TRIM(ErrMsg)//' '//TRIM(ErrMsg2)
         RETURN
      END IF


         ! Update discrete states:
         !   Note that xd [discrete state] is changed in Ctrl_UpdateDiscState(), so Ctrl_CalcOutput(),
         !   Ctrl_CalcContStateDeriv(), and Ctrl_CalcConstrStates() must be called first (see above).

      CALL Ctrl_UpdateDiscState(Time, u, p, x, xd, z, OtherState, ErrStat, ErrMsg )
      IF ( ErrStat >= AbortErrLev ) THEN
         CALL Ctrl_DestroyContState( dxdt, ErrStat2, ErrMsg2)
         ErrMsg = TRIM(ErrMsg)//' '//TRIM(ErrMsg2)
         RETURN
      END IF


         ! Integrate (update) continuous states (x) here:

      !x = function of dxdt and x


         ! Destroy dxdt because it is not necessary for the rest of the subroutine

      CALL Ctrl_DestroyContState( dxdt, ErrStat, ErrMsg)
      IF ( ErrStat >= AbortErrLev ) RETURN



END SUBROUTINE Ctrl_UpdateStates
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE Ctrl_CalcOutput( Time, u, p, x, xd, z, OtherState, y, ErrStat, ErrMsg )
! Routine for computing outputs, used in both loose and tight coupling.
!..................................................................................................................................

      REAL(DbKi),                     INTENT(IN   )  :: Time        ! Current simulation time in seconds
      TYPE(Ctrl_InputType),           INTENT(IN   )  :: u           ! Inputs at Time
      TYPE(Ctrl_ParameterType),       INTENT(IN   )  :: p           ! Parameters
      TYPE(Ctrl_ContinuousStateType), INTENT(IN   )  :: x           ! Continuous states at Time
      TYPE(Ctrl_DiscreteStateType),   INTENT(IN   )  :: xd          ! Discrete states at Time
      TYPE(Ctrl_ConstraintStateType), INTENT(IN   )  :: z           ! Constraint states at Time
      TYPE(Ctrl_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states
      TYPE(Ctrl_OutputType),          INTENT(INOUT)  :: y           ! Outputs computed at Time (Input only so that mesh con-
                                                                    !   nectivity information does not have to be recalculated)
      INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None


         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Compute outputs here:


END SUBROUTINE Ctrl_CalcOutput
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE Ctrl_CalcContStateDeriv( Time, u, p, x, xd, z, OtherState, dxdt, ErrStat, ErrMsg )
! Tight coupling routine for computing derivatives of continuous states
!..................................................................................................................................

      REAL(DbKi),                     INTENT(IN   )  :: Time        ! Current simulation time in seconds
      TYPE(Ctrl_InputType),           INTENT(IN   )  :: u           ! Inputs at Time
      TYPE(Ctrl_ParameterType),       INTENT(IN   )  :: p           ! Parameters
      TYPE(Ctrl_ContinuousStateType), INTENT(IN   )  :: x           ! Continuous states at Time
      TYPE(Ctrl_DiscreteStateType),   INTENT(IN   )  :: xd          ! Discrete states at Time
      TYPE(Ctrl_ConstraintStateType), INTENT(IN   )  :: z           ! Constraint states at Time
      TYPE(Ctrl_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states
      TYPE(Ctrl_ContinuousStateType), INTENT(  OUT)  :: dxdt        ! Continuous state derivatives at Time
      INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None


         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Compute the first time derivatives of the continuous states here:

      dxdt%DummyContState = 0


END SUBROUTINE Ctrl_CalcContStateDeriv
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE Ctrl_UpdateDiscState( Time, u, p, x, xd, z, OtherState, ErrStat, ErrMsg )
! Tight coupling routine for updating discrete states
!..................................................................................................................................

      REAL(DbKi),                     INTENT(IN   )  :: Time        ! Current simulation time in seconds
      TYPE(Ctrl_InputType),           INTENT(IN   )  :: u           ! Inputs at Time
      TYPE(Ctrl_ParameterType),       INTENT(IN   )  :: p           ! Parameters
      TYPE(Ctrl_ContinuousStateType), INTENT(IN   )  :: x           ! Continuous states at Time
      TYPE(Ctrl_DiscreteStateType),   INTENT(INOUT)  :: xd          ! Input: Discrete states at Time;
                                                                    !   Output: Discrete states at Time + Interval
      TYPE(Ctrl_ConstraintStateType), INTENT(IN   )  :: z           ! Constraint states at Time
      TYPE(Ctrl_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states
      INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None


         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Update discrete states here:

      ! StateData%DiscState =

END SUBROUTINE Ctrl_UpdateDiscState
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE Ctrl_CalcConstrStateResidual( Time, u, p, x, xd, z, OtherState, z_residual, ErrStat, ErrMsg )
! Tight coupling routine for solving for the residual of the constraint state equations
!..................................................................................................................................

      REAL(DbKi),                     INTENT(IN   )  :: Time        ! Current simulation time in seconds
      TYPE(Ctrl_InputType),           INTENT(IN   )  :: u           ! Inputs at Time
      TYPE(Ctrl_ParameterType),       INTENT(IN   )  :: p           ! Parameters
      TYPE(Ctrl_ContinuousStateType), INTENT(IN   )  :: x           ! Continuous states at Time
      TYPE(Ctrl_DiscreteStateType),   INTENT(IN   )  :: xd          ! Discrete states at Time
      TYPE(Ctrl_ConstraintStateType), INTENT(IN   )  :: z           ! Constraint states at Time (possibly a guess)
      TYPE(Ctrl_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states
      TYPE(Ctrl_ConstraintStateType), INTENT(  OUT)  :: z_residual  ! Residual of the constraint state equations using
                                                                    !     the input values described above
      INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None


         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Solve for the constraint states here:

      z_residual%DummyConstrState = 0

END SUBROUTINE Ctrl_CalcConstrStateResidual
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! WE ARE NOT YET IMPLEMENTING THE JACOBIANS...
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!!----------------------------------------------------------------------------------------------------------------------------------
!SUBROUTINE Ctrl_JacobianPInput( Time, u, p, x, xd, z, OtherState, dYdu, dXdu, dXddu, dZdu, ErrStat, ErrMsg )
!! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete- (Xd), and constraint-state (Z) equations
!! with respect to the inputs (u). The partial derivatives dY/du, dX/du, dXd/du, and DZ/du are returned.
!!..................................................................................................................................
!
!      REAL(DbKi),                             INTENT(IN   )           :: Time       ! Current simulation time in seconds
!      TYPE(Ctrl_InputType),                   INTENT(IN   )           :: u          ! Inputs at Time
!      TYPE(Ctrl_ParameterType),               INTENT(IN   )           :: p          ! Parameters
!      TYPE(Ctrl_ContinuousStateType),         INTENT(IN   )           :: x          ! Continuous states at Time
!      TYPE(Ctrl_DiscreteStateType),           INTENT(IN   )           :: xd         ! Discrete states at Time
!      TYPE(Ctrl_ConstraintStateType),         INTENT(IN   )           :: z          ! Constraint states at Time
!      TYPE(Ctrl_OtherStateType),              INTENT(INOUT)           :: OtherState ! Other/optimization states
!      TYPE(Ctrl_PartialOutputPInputType),     INTENT(  OUT), OPTIONAL :: dYdu       ! Partial derivatives of output equations
!                                                                                    !   (Y) with respect to the inputs (u)
!      TYPE(Ctrl_PartialContStatePInputType),  INTENT(  OUT), OPTIONAL :: dXdu       ! Partial derivatives of continuous state
!                                                                                    !   equations (X) with respect to inputs (u)
!      TYPE(Ctrl_PartialDiscStatePInputType),  INTENT(  OUT), OPTIONAL :: dXddu      ! Partial derivatives of discrete state
!                                                                                    !   equations (Xd) with respect to inputs (u)
!      TYPE(Ctrl_PartialConstrStatePInputType),INTENT(  OUT), OPTIONAL :: dZdu       ! Partial derivatives of constraint state
!                                                                                    !   equations (Z) with respect to inputs (u)
!      INTEGER(IntKi),                         INTENT(  OUT)           :: ErrStat    ! Error status of the operation
!      CHARACTER(*),                           INTENT(  OUT)           :: ErrMsg     ! Error message if ErrStat /= ErrID_None
!
!
!         ! Initialize ErrStat
!
!      ErrStat = ErrID_None
!      ErrMsg  = ""
!
!
!      IF ( PRESENT( dYdu ) ) THEN
!
!         ! Calculate the partial derivative of the output equations (Y) with respect to the inputs (u) here:
!
!         dYdu%DummyOutput%DummyInput = 0
!
!      END IF
!
!      IF ( PRESENT( dXdu ) ) THEN
!
!         ! Calculate the partial derivative of the continuous state equations (X) with respect to the inputs (u) here:
!
!         dXdu%DummyContState%DummyInput = 0
!
!      END IF
!
!      IF ( PRESENT( dXddu ) ) THEN
!
!         ! Calculate the partial derivative of the discrete state equations (Xd) with respect to the inputs (u) here:
!
!         dXddu%DummyDiscState%DummyInput = 0
!
!      END IF
!
!      IF ( PRESENT( dZdu ) ) THEN
!
!         ! Calculate the partial derivative of the constraint state equations (Z) with respect to the inputs (u) here:
!
!         dZdu%DummyConstrState%DummyInput = 0
!
!      END IF
!
!
!END SUBROUTINE Ctrl_JacobianPInput
!!----------------------------------------------------------------------------------------------------------------------------------
!SUBROUTINE Ctrl_JacobianPContState( Time, u, p, x, xd, z, OtherState, dYdx, dXdx, dXddx, dZdx, ErrStat, ErrMsg )
!! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete- (Xd), and constraint-state (Z) equations
!! with respect to the continuous states (x). The partial derivatives dY/dx, dX/dx, dXd/dx, and DZ/dx are returned.
!!..................................................................................................................................
!
!      REAL(DbKi),                                 INTENT(IN   )           :: Time       ! Current simulation time in seconds
!      TYPE(Ctrl_InputType),                       INTENT(IN   )           :: u          ! Inputs at Time
!      TYPE(Ctrl_ParameterType),                   INTENT(IN   )           :: p          ! Parameters
!      TYPE(Ctrl_ContinuousStateType),             INTENT(IN   )           :: x          ! Continuous states at Time
!      TYPE(Ctrl_DiscreteStateType),               INTENT(IN   )           :: xd         ! Discrete states at Time
!      TYPE(Ctrl_ConstraintStateType),             INTENT(IN   )           :: z          ! Constraint states at Time
!      TYPE(Ctrl_OtherStateType),                  INTENT(INOUT)           :: OtherState ! Other/optimization states
!      TYPE(Ctrl_PartialOutputPContStateType),     INTENT(  OUT), OPTIONAL :: dYdx       ! Partial derivatives of output equations
!                                                                                        !   (Y) with respect to the continuous
!                                                                                        !   states (x)
!      TYPE(Ctrl_PartialContStatePContStateType),  INTENT(  OUT), OPTIONAL :: dXdx       ! Partial derivatives of continuous state
!                                                                                        !   equations (X) with respect to
!                                                                                        !   the continuous states (x)
!      TYPE(Ctrl_PartialDiscStatePContStateType),  INTENT(  OUT), OPTIONAL :: dXddx      ! Partial derivatives of discrete state
!                                                                                        !   equations (Xd) with respect to
!                                                                                        !   the continuous states (x)
!      TYPE(Ctrl_PartialConstrStatePContStateType),INTENT(  OUT), OPTIONAL :: dZdx       ! Partial derivatives of constraint state
!                                                                                        !   equations (Z) with respect to
!                                                                                        !   the continuous states (x)
!      INTEGER(IntKi),                             INTENT(  OUT)           :: ErrStat    ! Error status of the operation
!      CHARACTER(*),                               INTENT(  OUT)           :: ErrMsg     ! Error message if ErrStat /= ErrID_None
!
!
!         ! Initialize ErrStat
!
!      ErrStat = ErrID_None
!      ErrMsg  = ""
!
!
!
!      IF ( PRESENT( dYdx ) ) THEN
!
!         ! Calculate the partial derivative of the output equations (Y) with respect to the continuous states (x) here:
!
!         dYdx%DummyOutput%DummyContState = 0
!
!      END IF
!
!      IF ( PRESENT( dXdx ) ) THEN
!
!         ! Calculate the partial derivative of the continuous state equations (X) with respect to the continuous states (x) here:
!
!         dXdx%DummyContState%DummyContState = 0
!
!      END IF
!
!      IF ( PRESENT( dXddx ) ) THEN
!
!         ! Calculate the partial derivative of the discrete state equations (Xd) with respect to the continuous states (x) here:
!
!         dXddx%DummyDiscState%DummyContState = 0
!
!      END IF
!
!      IF ( PRESENT( dZdx ) ) THEN
!
!
!         ! Calculate the partial derivative of the constraint state equations (Z) with respect to the continuous states (x) here:
!
!         dZdx%DummyConstrState%DummyContState = 0
!
!      END IF
!
!
!   END SUBROUTINE Ctrl_JacobianPContState
!!----------------------------------------------------------------------------------------------------------------------------------
!SUBROUTINE Ctrl_JacobianPDiscState( Time, u, p, x, xd, z, OtherState, dYdxd, dXdxd, dXddxd, dZdxd, ErrStat, ErrMsg )
!! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete- (Xd), and constraint-state (Z) equations
!! with respect to the discrete states (xd). The partial derivatives dY/dxd, dX/dxd, dXd/dxd, and DZ/dxd are returned.
!!..................................................................................................................................
!
!      REAL(DbKi),                                 INTENT(IN   )           :: Time       ! Current simulation time in seconds
!      TYPE(Ctrl_InputType),                       INTENT(IN   )           :: u          ! Inputs at Time
!      TYPE(Ctrl_ParameterType),                   INTENT(IN   )           :: p          ! Parameters
!      TYPE(Ctrl_ContinuousStateType),             INTENT(IN   )           :: x          ! Continuous states at Time
!      TYPE(Ctrl_DiscreteStateType),               INTENT(IN   )           :: xd         ! Discrete states at Time
!      TYPE(Ctrl_ConstraintStateType),             INTENT(IN   )           :: z          ! Constraint states at Time
!      TYPE(Ctrl_OtherStateType),                  INTENT(INOUT)           :: OtherState ! Other/optimization states
!      TYPE(Ctrl_PartialOutputPDiscStateType),     INTENT(  OUT), OPTIONAL :: dYdxd      ! Partial derivatives of output equations
!                                                                                        !  (Y) with respect to the discrete
!                                                                                        !  states (xd)
!      TYPE(Ctrl_PartialContStatePDiscStateType),  INTENT(  OUT), OPTIONAL :: dXdxd      ! Partial derivatives of continuous state
!                                                                                        !   equations (X) with respect to the
!                                                                                        !   discrete states (xd)
!      TYPE(Ctrl_PartialDiscStatePDiscStateType),  INTENT(  OUT), OPTIONAL :: dXddxd     ! Partial derivatives of discrete state
!                                                                                        !   equations (Xd) with respect to the
!                                                                                        !   discrete states (xd)
!      TYPE(Ctrl_PartialConstrStatePDiscStateType),INTENT(  OUT), OPTIONAL :: dZdxd      ! Partial derivatives of constraint state
!                                                                                        !   equations (Z) with respect to the
!                                                                                        !   discrete states (xd)
!      INTEGER(IntKi),                             INTENT(  OUT)           :: ErrStat    ! Error status of the operation
!      CHARACTER(*),                               INTENT(  OUT)           :: ErrMsg     ! Error message if ErrStat /= ErrID_None
!
!
!         ! Initialize ErrStat
!
!      ErrStat = ErrID_None
!      ErrMsg  = ""
!
!
!      IF ( PRESENT( dYdxd ) ) THEN
!
!         ! Calculate the partial derivative of the output equations (Y) with respect to the discrete states (xd) here:
!
!         dYdxd%DummyOutput%DummyDiscState = 0
!
!      END IF
!
!      IF ( PRESENT( dXdxd ) ) THEN
!
!         ! Calculate the partial derivative of the continuous state equations (X) with respect to the discrete states (xd) here:
!
!         dXdxd%DummyContState%DummyDiscState = 0
!
!      END IF
!
!      IF ( PRESENT( dXddxd ) ) THEN
!
!         ! Calculate the partial derivative of the discrete state equations (Xd) with respect to the discrete states (xd) here:
!
!         dXddxd%DummyDiscState%DummyDiscState = 0
!
!      END IF
!
!      IF ( PRESENT( dZdxd ) ) THEN
!
!         ! Calculate the partial derivative of the constraint state equations (Z) with respect to the discrete states (xd) here:
!
!         dZdxd%DummyConstrState%DummyDiscState = 0
!
!      END IF
!
!
!
!END SUBROUTINE Ctrl_JacobianPDiscState
!!----------------------------------------------------------------------------------------------------------------------------------
!SUBROUTINE Ctrl_JacobianPConstrState( Time, u, p, x, xd, z, OtherState, dYdz, dXdz, dXddz, dZdz, ErrStat, ErrMsg )
!! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete- (Xd), and constraint-state (Z) equations
!! with respect to the constraint states (z). The partial derivatives dY/dz, dX/dz, dXd/dz, and DZ/dz are returned.
!!..................................................................................................................................
!
!      REAL(DbKi),                                   INTENT(IN   )           :: Time       ! Current simulation time in seconds
!      TYPE(Ctrl_InputType),                         INTENT(IN   )           :: u          ! Inputs at Time
!      TYPE(Ctrl_ParameterType),                     INTENT(IN   )           :: p          ! Parameters
!      TYPE(Ctrl_ContinuousStateType),               INTENT(IN   )           :: x          ! Continuous states at Time
!      TYPE(Ctrl_DiscreteStateType),                 INTENT(IN   )           :: xd         ! Discrete states at Time
!      TYPE(Ctrl_ConstraintStateType),               INTENT(IN   )           :: z          ! Constraint states at Time
!      TYPE(Ctrl_OtherStateType),                    INTENT(INOUT)           :: OtherState ! Other/optimization states
!      TYPE(Ctrl_PartialOutputPConstrStateType),     INTENT(  OUT), OPTIONAL :: dYdz       ! Partial derivatives of output
!                                                                                          !  equations (Y) with respect to the
!                                                                                          !  constraint states (z)
!      TYPE(Ctrl_PartialContStatePConstrStateType),  INTENT(  OUT), OPTIONAL :: dXdz       ! Partial derivatives of continuous
!                                                                                          !  state equations (X) with respect to
!                                                                                          !  the constraint states (z)
!      TYPE(Ctrl_PartialDiscStatePConstrStateType),  INTENT(  OUT), OPTIONAL :: dXddz      ! Partial derivatives of discrete state
!                                                                                          !  equations (Xd) with respect to the
!                                                                                          !  constraint states (z)
!      TYPE(Ctrl_PartialConstrStatePConstrStateType),INTENT(  OUT), OPTIONAL :: dZdz       ! Partial derivatives of constraint
!                                                                                          ! state equations (Z) with respect to
!                                                                                          !  the constraint states (z)
!      INTEGER(IntKi),                               INTENT(  OUT)           :: ErrStat    ! Error status of the operation
!      CHARACTER(*),                                 INTENT(  OUT)           :: ErrMsg     ! Error message if ErrStat /= ErrID_None
!
!
!         ! Initialize ErrStat
!
!      ErrStat = ErrID_None
!      ErrMsg  = ""
!
!      IF ( PRESENT( dYdz ) ) THEN
!
!            ! Calculate the partial derivative of the output equations (Y) with respect to the constraint states (z) here:
!
!         dYdz%DummyOutput%DummyConstrState = 0
!
!      END IF
!
!      IF ( PRESENT( dXdz ) ) THEN
!
!            ! Calculate the partial derivative of the continuous state equations (X) with respect to the constraint states (z) here:
!
!         dXdz%DummyContState%DummyConstrState = 0
!
!      END IF
!
!      IF ( PRESENT( dXddz ) ) THEN
!
!            ! Calculate the partial derivative of the discrete state equations (Xd) with respect to the constraint states (z) here:
!
!         dXddz%DummyDiscState%DummyConstrState = 0
!
!      END IF
!
!      IF ( PRESENT( dZdz ) ) THEN
!
!            ! Calculate the partial derivative of the constraint state equations (Z) with respect to the constraint states (z) here:
!
!         dZdz%DummyConstrState%DummyConstrState = 0
!
!      END IF
!
!
!END SUBROUTINE Ctrl_JacobianPConstrState
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

END MODULE Controls
!**********************************************************************************************************************************
