!**********************************************************************************************************************************
! The ServoDyn and ServoDyn_Types modules make up a template for creating user-defined calculations in the FAST Modularization
! Framework. ServoDyn_Types will be auto-generated based on a description of the variables for the module.
!
! "ModuleName" should be replaced with the name of your module. Example: ServoDyn
! "ModName" (in ModName_*) should be replaced with the module name or an abbreviation of it. Example: SrvD
!..................................................................................................................................
! LICENSING
! Copyright (C) 2013  National Renewable Energy Laboratory
!
!    This file is part of FAST's Controls and Electrical Drive Module, "ServoDyn".
!
!    ServoDyn is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as
!    published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
!    of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License along with ServoDyn.
!    If not, see <http://www.gnu.org/licenses/>.
!
!**********************************************************************************************************************************
MODULE ServoDyn

   USE ServoDyn_Types
   USE NWTC_Library

   IMPLICIT NONE

   PRIVATE

   TYPE(ProgDesc), PARAMETER            :: SrvD_Ver = ProgDesc( 'ServoDyn', 'v1.00.00', '31-March-2013' )


      ! ..... Public Subroutines ...................................................................................................

   PUBLIC :: SrvD_Init                           ! Initialization routine
   PUBLIC :: SrvD_End                            ! Ending routine (includes clean up)

   PUBLIC :: SrvD_UpdateStates                   ! Loose coupling routine for solving for constraint states, integrating
                                                 !   continuous states, and updating discrete states
   PUBLIC :: SrvD_CalcOutput                     ! Routine for computing outputs

   PUBLIC :: SrvD_CalcConstrStateResidual        ! Tight coupling routine for returning the constraint state residual
   PUBLIC :: SrvD_CalcContStateDeriv             ! Tight coupling routine for computing derivatives of continuous states
   PUBLIC :: SrvD_UpdateDiscState                ! Tight coupling routine for updating discrete states

   !PUBLIC :: SrvD_JacobianPInput                 ! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete-
   !                                              !   (Xd), and constraint-state (Z) equations all with respect to the inputs (u)
   !PUBLIC :: SrvD_JacobianPContState             ! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete-
   !                                              !   (Xd), and constraint-state (Z) equations all with respect to the continuous
   !                                              !   states (x)
   !PUBLIC :: SrvD_JacobianPDiscState             ! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete-
   !                                              !   (Xd), and constraint-state (Z) equations all with respect to the discrete
   !                                              !   states (xd)
   !PUBLIC :: SrvD_JacobianPConstrState           ! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete-
   !                                              !   (Xd), and constraint-state (Z) equations all with respect to the constraint
   !                                              !   states (z)
   
CONTAINS
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE SrvD_Init( InitInp, u, p, x, xd, z, OtherState, y, Interval, InitOut, ErrStat, ErrMsg )
! This routine is called at the start of the simulation to perform initialization steps.
! The parameters are set here and not changed during the simulation.
! The initial states and initial guess for the input are defined.
!..................................................................................................................................

      TYPE(SrvD_InitInputType),       INTENT(IN   )  :: InitInp     ! Input data for initialization routine
      TYPE(SrvD_InputType),           INTENT(  OUT)  :: u           ! An initial guess for the input; input mesh must be defined
      TYPE(SrvD_ParameterType),       INTENT(  OUT)  :: p           ! Parameters
      TYPE(SrvD_ContinuousStateType), INTENT(  OUT)  :: x           ! Initial continuous states
      TYPE(SrvD_DiscreteStateType),   INTENT(  OUT)  :: xd          ! Initial discrete states
      TYPE(SrvD_ConstraintStateType), INTENT(  OUT)  :: z           ! Initial guess of the constraint states
      TYPE(SrvD_OtherStateType),      INTENT(  OUT)  :: OtherState  ! Initial other/optimization states
      TYPE(SrvD_OutputType),          INTENT(  OUT)  :: y           ! Initial system outputs (outputs are not calculated;
                                                                    !   only the output mesh is initialized)
      REAL(DbKi),                     INTENT(INOUT)  :: Interval    ! Coupling interval in seconds: the rate that
                                                                    !   (1) SrvD_UpdateStates() is called in loose coupling &
                                                                    !   (2) SrvD_UpdateDiscState() is called in tight coupling.
                                                                    !   Input is the suggested time from the glue code;
                                                                    !   Output is the actual coupling interval that will be used
                                                                    !   by the glue code.
      TYPE(SrvD_InitOutputType),      INTENT(  OUT)  :: InitOut     ! Output for initialization routine
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

      CALL DispNVD( SrvD_Ver )


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
         ErrMsg  = 'Error allocating output header and units arrays in SrvD_Init'
         RETURN
      END IF

      y%WriteOutput = 0


         ! Define initialization-routine output here:
      ALLOCATE( InitOut%WriteOutputHdr(NumOuts), InitOut%WriteOutputUnt(NumOuts), STAT = ErrStat )
      IF ( ErrStat/= 0 ) THEN
         ErrStat = ErrID_Fatal
         ErrMsg  = 'Error allocating output header and units arrays in SrvD_Init'
         RETURN
      END IF

      InitOut%WriteOutputHdr = (/ 'Time   ', 'Column2' /)
      InitOut%WriteOutputUnt = (/ '(s)',     '(-)'     /)


         ! If you want to choose your own rate instead of using what the glue code suggests, tell the glue code the rate at which
         !   this module must be called here:

       !Interval = p%DT


END SUBROUTINE SrvD_Init
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE SrvD_End( u, p, x, xd, z, OtherState, y, ErrStat, ErrMsg )
! This routine is called at the end of the simulation.
!..................................................................................................................................

      TYPE(SrvD_InputType),           INTENT(INOUT)  :: u           ! System inputs
      TYPE(SrvD_ParameterType),       INTENT(INOUT)  :: p           ! Parameters
      TYPE(SrvD_ContinuousStateType), INTENT(INOUT)  :: x           ! Continuous states
      TYPE(SrvD_DiscreteStateType),   INTENT(INOUT)  :: xd          ! Discrete states
      TYPE(SrvD_ConstraintStateType), INTENT(INOUT)  :: z           ! Constraint states
      TYPE(SrvD_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states
      TYPE(SrvD_OutputType),          INTENT(INOUT)  :: y           ! System outputs
      INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None



         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Place any last minute operations or calculations here:


         ! Close files here:



         ! Destroy the input data:

      CALL SrvD_DestroyInput( u, ErrStat, ErrMsg )


         ! Destroy the parameter data:

      CALL SrvD_DestroyParam( p, ErrStat, ErrMsg )


         ! Destroy the state data:

      CALL SrvD_DestroyContState(   x,           ErrStat, ErrMsg )
      CALL SrvD_DestroyDiscState(   xd,          ErrStat, ErrMsg )
      CALL SrvD_DestroyConstrState( z,           ErrStat, ErrMsg )
      CALL SrvD_DestroyOtherState(  OtherState,  ErrStat, ErrMsg )


         ! Destroy the output data:

      CALL SrvD_DestroyOutput( y, ErrStat, ErrMsg )




END SUBROUTINE SrvD_End
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE SrvD_UpdateStates( Time, u, p, x, xd, z, OtherState, ErrStat, ErrMsg )
! Loose coupling routine for solving for constraint states, integrating continuous states, and updating discrete states
! Constraint states are solved for input Time; Continuous and discrete states are updated for Time + Interval
!..................................................................................................................................

      REAL(DbKi),                      INTENT(IN   ) :: Time        ! Current simulation time in seconds
      TYPE(SrvD_InputType),            INTENT(IN   ) :: u           ! Inputs at Time
      TYPE(SrvD_ParameterType),        INTENT(IN   ) :: p           ! Parameters
      TYPE(SrvD_ContinuousStateType),  INTENT(INOUT) :: x           ! Input: Continuous states at Time;
                                                                    !   Output: Continuous states at Time + Interval
      TYPE(SrvD_DiscreteStateType),    INTENT(INOUT) :: xd          ! Input: Discrete states at Time;
                                                                    !   Output: Discrete states at Time  + Interval
      TYPE(SrvD_ConstraintStateType),  INTENT(INOUT) :: z           ! Input: Initial guess of constraint states at Time;
                                                                    !   Output: Constraint states at Time
      TYPE(SrvD_OtherStateType),       INTENT(INOUT) :: OtherState  ! Other/optimization states
      INTEGER(IntKi),                  INTENT(  OUT) :: ErrStat     ! Error status of the operation
      CHARACTER(*),                    INTENT(  OUT) :: ErrMsg      ! Error message if ErrStat /= ErrID_None

         ! Local variables

      TYPE(SrvD_ContinuousStateType)                 :: dxdt        ! Continuous state derivatives at Time
      TYPE(SrvD_ConstraintStateType)                 :: z_Residual  ! Residual of the constraint state equations (Z)

      INTEGER(IntKi)                                 :: ErrStat2    ! Error status of the operation (occurs after initial error)
      CHARACTER(LEN(ErrMsg))                         :: ErrMsg2     ! Error message if ErrStat2 /= ErrID_None

         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""



         ! Solve for the constraint states (z) here:

         ! Check if the z guess is correct and update z with a new guess.
         ! Iterate until the value is within a given tolerance.

      CALL SrvD_CalcConstrStateResidual( Time, u, p, x, xd, z, OtherState, z_Residual, ErrStat, ErrMsg )
      IF ( ErrStat >= AbortErrLev ) THEN
         CALL SrvD_DestroyConstrState( z_Residual, ErrStat2, ErrMsg2)
         ErrMsg = TRIM(ErrMsg)//' '//TRIM(ErrMsg2)
         RETURN
      END IF

      ! DO WHILE ( z_Residual% > tolerance )
      !
      !  z =
      !
      !  CALL SrvD_CalcConstrStateResidual( Time, u, p, x, xd, z, OtherState, z_Residual, ErrStat, ErrMsg )
      !  IF ( ErrStat >= AbortErrLev ) THEN
      !     CALL SrvD_DestroyConstrState( z_Residual, ErrStat2, ErrMsg2)
      !     ErrMsg = TRIM(ErrMsg)//' '//TRIM(ErrMsg2)
      !     RETURN
      !  END IF
      !
      ! END DO


         ! Destroy z_Residual because it is not necessary for the rest of the subroutine:

      CALL SrvD_DestroyConstrState( z_Residual, ErrStat, ErrMsg)
      IF ( ErrStat >= AbortErrLev ) RETURN



         ! Get first time derivatives of continuous states (dxdt):

      CALL SrvD_CalcContStateDeriv( Time, u, p, x, xd, z, OtherState, dxdt, ErrStat, ErrMsg )
      IF ( ErrStat >= AbortErrLev ) THEN
         CALL SrvD_DestroyContState( dxdt, ErrStat2, ErrMsg2)
         ErrMsg = TRIM(ErrMsg)//' '//TRIM(ErrMsg2)
         RETURN
      END IF


         ! Update discrete states:
         !   Note that xd [discrete state] is changed in SrvD_UpdateDiscState(), so SrvD_CalcOutput(),
         !   SrvD_CalcContStateDeriv(), and SrvD_CalcConstrStates() must be called first (see above).

      CALL SrvD_UpdateDiscState(Time, u, p, x, xd, z, OtherState, ErrStat, ErrMsg )
      IF ( ErrStat >= AbortErrLev ) THEN
         CALL SrvD_DestroyContState( dxdt, ErrStat2, ErrMsg2)
         ErrMsg = TRIM(ErrMsg)//' '//TRIM(ErrMsg2)
         RETURN
      END IF


         ! Integrate (update) continuous states (x) here:

      !x = function of dxdt and x


         ! Destroy dxdt because it is not necessary for the rest of the subroutine

      CALL SrvD_DestroyContState( dxdt, ErrStat, ErrMsg)
      IF ( ErrStat >= AbortErrLev ) RETURN



END SUBROUTINE SrvD_UpdateStates
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE SrvD_CalcOutput( Time, u, p, x, xd, z, OtherState, y, ErrStat, ErrMsg )
! Routine for computing outputs, used in both loose and tight coupling.
!..................................................................................................................................

      REAL(DbKi),                     INTENT(IN   )  :: Time        ! Current simulation time in seconds
      TYPE(SrvD_InputType),           INTENT(IN   )  :: u           ! Inputs at Time
      TYPE(SrvD_ParameterType),       INTENT(IN   )  :: p           ! Parameters
      TYPE(SrvD_ContinuousStateType), INTENT(IN   )  :: x           ! Continuous states at Time
      TYPE(SrvD_DiscreteStateType),   INTENT(IN   )  :: xd          ! Discrete states at Time
      TYPE(SrvD_ConstraintStateType), INTENT(IN   )  :: z           ! Constraint states at Time
      TYPE(SrvD_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states
      TYPE(SrvD_OutputType),          INTENT(INOUT)  :: y           ! Outputs computed at Time (Input only so that mesh con-
                                                                    !   nectivity information does not have to be recalculated)
      INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None


         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Compute outputs here:


END SUBROUTINE SrvD_CalcOutput
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE SrvD_CalcContStateDeriv( Time, u, p, x, xd, z, OtherState, dxdt, ErrStat, ErrMsg )
! Tight coupling routine for computing derivatives of continuous states
!..................................................................................................................................

      REAL(DbKi),                     INTENT(IN   )  :: Time        ! Current simulation time in seconds
      TYPE(SrvD_InputType),           INTENT(IN   )  :: u           ! Inputs at Time
      TYPE(SrvD_ParameterType),       INTENT(IN   )  :: p           ! Parameters
      TYPE(SrvD_ContinuousStateType), INTENT(IN   )  :: x           ! Continuous states at Time
      TYPE(SrvD_DiscreteStateType),   INTENT(IN   )  :: xd          ! Discrete states at Time
      TYPE(SrvD_ConstraintStateType), INTENT(IN   )  :: z           ! Constraint states at Time
      TYPE(SrvD_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states
      TYPE(SrvD_ContinuousStateType), INTENT(  OUT)  :: dxdt        ! Continuous state derivatives at Time
      INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None


         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Compute the first time derivatives of the continuous states here:

      dxdt%DummyContState = 0


END SUBROUTINE SrvD_CalcContStateDeriv
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE SrvD_UpdateDiscState( Time, u, p, x, xd, z, OtherState, ErrStat, ErrMsg )
! Tight coupling routine for updating discrete states
!..................................................................................................................................

      REAL(DbKi),                     INTENT(IN   )  :: Time        ! Current simulation time in seconds
      TYPE(SrvD_InputType),           INTENT(IN   )  :: u           ! Inputs at Time
      TYPE(SrvD_ParameterType),       INTENT(IN   )  :: p           ! Parameters
      TYPE(SrvD_ContinuousStateType), INTENT(IN   )  :: x           ! Continuous states at Time
      TYPE(SrvD_DiscreteStateType),   INTENT(INOUT)  :: xd          ! Input: Discrete states at Time;
                                                                    !   Output: Discrete states at Time + Interval
      TYPE(SrvD_ConstraintStateType), INTENT(IN   )  :: z           ! Constraint states at Time
      TYPE(SrvD_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states
      INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None


         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Update discrete states here:

      ! StateData%DiscState =

END SUBROUTINE SrvD_UpdateDiscState
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE SrvD_CalcConstrStateResidual( Time, u, p, x, xd, z, OtherState, z_residual, ErrStat, ErrMsg )
! Tight coupling routine for solving for the residual of the constraint state equations
!..................................................................................................................................

      REAL(DbKi),                     INTENT(IN   )  :: Time        ! Current simulation time in seconds
      TYPE(SrvD_InputType),           INTENT(IN   )  :: u           ! Inputs at Time
      TYPE(SrvD_ParameterType),       INTENT(IN   )  :: p           ! Parameters
      TYPE(SrvD_ContinuousStateType), INTENT(IN   )  :: x           ! Continuous states at Time
      TYPE(SrvD_DiscreteStateType),   INTENT(IN   )  :: xd          ! Discrete states at Time
      TYPE(SrvD_ConstraintStateType), INTENT(IN   )  :: z           ! Constraint states at Time (possibly a guess)
      TYPE(SrvD_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states
      TYPE(SrvD_ConstraintStateType), INTENT(  OUT)  :: z_residual  ! Residual of the constraint state equations using
                                                                    !     the input values described above
      INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None


         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Solve for the constraint states here:

      z_residual%DummyConstrState = 0

END SUBROUTINE SrvD_CalcConstrStateResidual
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! WE ARE NOT YET IMPLEMENTING THE JACOBIANS...
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

END MODULE ServoDyn
!**********************************************************************************************************************************
