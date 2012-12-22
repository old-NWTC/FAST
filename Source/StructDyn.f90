
MODULE StructDyn_Parameters

      ! This module contains definitions of compile-time PARAMETERS for the StrucyDyn module. 
      ! Every variable defined here MUST have the PARAMETER attribute.
      

   USE NWTC_Library


      ! Parameters related to degrees of freedom (formerly MODULE DOFs)

   INTEGER(IntKi), PARAMETER        :: MaxBl    =  3                                   ! Maximum number of blades allowed in simulation
   INTEGER(IntKi), PARAMETER        :: NumBE    =  1                                   ! Number of blade-edge modes
   INTEGER(IntKi), PARAMETER        :: NumBF    =  2                                   ! Number of blade-flap modes

   INTEGER(IntKi), PARAMETER        :: DOF_Sg   =  1                                   ! DOF index for platform surge
   INTEGER(IntKi), PARAMETER        :: DOF_Sw   =  2                                   ! DOF index for platform sway
   INTEGER(IntKi), PARAMETER        :: DOF_Hv   =  3                                   ! DOF index for platform heave
   INTEGER(IntKi), PARAMETER        :: DOF_R    =  4                                   ! DOF index for platform roll
   INTEGER(IntKi), PARAMETER        :: DOF_P    =  5                                   ! DOF index for platform pitch
   INTEGER(IntKi), PARAMETER        :: DOF_Y    =  6                                   ! DOF index for platform yaw
   INTEGER(IntKi), PARAMETER        :: DOF_TFA1 =  7                                   ! DOF index for 1st tower fore-aft mode
   INTEGER(IntKi), PARAMETER        :: DOF_TSS1 =  8                                   ! DOF index for 1st tower side-to-side mode
   INTEGER(IntKi), PARAMETER        :: DOF_TFA2 =  9                                   ! DOF index for 2nd tower fore-aft mode
   INTEGER(IntKi), PARAMETER        :: DOF_TSS2 = 10                                   ! DOF index for 2nd tower side-to-side mode
   INTEGER(IntKi), PARAMETER        :: DOF_Yaw  = 11                                   ! DOF index for nacelle-yaw
   INTEGER(IntKi), PARAMETER        :: DOF_RFrl = 12                                   ! DOF index for rotor-furl
   INTEGER(IntKi), PARAMETER        :: DOF_GeAz = 13                                   ! DOF index for the generator azimuth
   INTEGER(IntKi), PARAMETER        :: DOF_DrTr = 14                                   ! DOF index for drivetrain rotational-flexibility
   INTEGER(IntKi), PARAMETER        :: DOF_TFrl = 15                                   ! DOF index for tail-furl

   INTEGER(IntKi), PARAMETER        :: DOF_BE (MaxBl,NumBE) = RESHAPE(  &              ! DOF indices for blade edge:
                                               (/ 17, 20, 23 /),   (/MaxBl,NumBE/) )   !    1st blade edge mode for blades 1,2, and 3, respectively 17 + 3*(K-1)
   INTEGER(IntKi), PARAMETER        :: DOF_BF (MaxBl,NumBF) = RESHAPE(  &              ! DOF indices for blade flap:
                                               (/ 16, 19, 22,           &              !    1st blade flap mode for blades 1,2, and 3, respectively 16 + 3*(K-1)
                                                  18, 21, 24 /),   (/MaxBl,NumBF/) )   !    2nd blade flap mode for blades 1,2, and 3, respectively 18 + 3*(K-1)


   INTEGER(IntKi), PARAMETER        :: DOF_Teet = 22 !DOF_TFrl + 2*(NumBE+NumBF)+ 1    ! DOF index for rotor-teeter


   
   
      ! Parameters related to coupling scheme -- Possibly a local variable elsewhere????


   INTEGER(IntKi), PARAMETER        :: NMX      =  9                                   ! Used in updating predictor-corrector values.



      ! Parameters related to coupling scheme -- Possibly a local variable elsewhere????


   !INTEGER(IntKi), PARAMETER        :: PolyOrd  =  6                                   ! Order of the polynomial describing the mode shape
   

      ! Parameters related to output length -- Possibly a local variable elsewhere????
   INTEGER(IntKi), PARAMETER        :: OutStrLen  = 10                                  ! number of characters allowed in the output data headers              
   INTEGER(IntKi), PARAMETER        :: OutStrLenM = OutStrLen-1                         ! number of characters allowed in the output data headers, excluding a minus sign or "M"           
!   INTEGER(IntKi), PARAMETER        ::MaxOutPts =  986 
   
END MODULE StructDyn_Parameters
!**********************************************************************************************************************************
!**********************************************************************************************************************************
! The StructDyn.f90, StructDyn_Types.f90, and StructDyn_Parameters.f90 files make up the StructDyn module of the
! FAST Modularization Framework. StructDyn_Types is auto-generated based on FAST_Registry.txt.
!
!..................................................................................................................................
! LICENSING
! Copyright (C) 2012  National Renewable Energy Laboratory
!
!    This file is part of StructDyn.
!
!    StructDyn is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as
!    published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
!    of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License along with StructDyn.
!    If not, see <http://www.gnu.org/licenses/>.
!
!**********************************************************************************************************************************
MODULE StructDyn

   USE NWTC_Library

   USE StructDyn_Parameters
   USE StructDyn_Types


   IMPLICIT NONE

   PRIVATE

   TYPE(ProgDesc), PARAMETER  :: StrD_Ver = ProgDesc( 'StructDyn', 'v1.00.00a-bjj', '01-January-2013' )



      ! ..... Public Subroutines ...................................................................................................

   PUBLIC :: StrD_Init                           ! Initialization routine
   PUBLIC :: StrD_End                            ! Ending routine (includes clean up)

   PUBLIC :: StrD_UpdateStates                   ! Loose coupling routine for solving for constraint states, integrating
                                                 !   continuous states, and updating discrete states
   PUBLIC :: StrD_CalcOutput                     ! Routine for computing outputs

   PUBLIC :: StrD_CalcConstrStateResidual        ! Tight coupling routine for returning the constraint state residual
   PUBLIC :: StrD_CalcContStateDeriv             ! Tight coupling routine for computing derivatives of continuous states
   PUBLIC :: StrD_UpdateDiscState                ! Tight coupling routine for updating discrete states

   !PUBLIC :: StrD_JacobianPInput                 ! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete-
   !                                              !   (Xd), and constraint-state (Z) equations all with respect to the inputs (u)
   !PUBLIC :: StrD_JacobianPContState             ! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete-
   !                                              !   (Xd), and constraint-state (Z) equations all with respect to the continuous
   !                                              !   states (x)
   !PUBLIC :: StrD_JacobianPDiscState             ! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete-
   !                                              !   (Xd), and constraint-state (Z) equations all with respect to the discrete
   !                                              !   states (xd)
   !PUBLIC :: StrD_JacobianPConstrState           ! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete-
   !                                              !   (Xd), and constraint-state (Z) equations all with respect to the constraint
   !                                              !   states (z)


CONTAINS
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE StrD_Init( InitInp, u, p, x, xd, z, OtherState, y, Interval, InitOut, ErrStat, ErrMsg )
! This routine is called at the start of the simulation to perform initialization steps.
! The parameters are set here and not changed during the simulation.
! The initial states and initial guess for the input are defined.
!..................................................................................................................................

      TYPE(StrD_InitInputType),       INTENT(IN   )  :: InitInp     ! Input data for initialization routine
      TYPE(StrD_InputType),           INTENT(  OUT)  :: u           ! An initial guess for the input; input mesh must be defined
      TYPE(StrD_ParameterType),       INTENT(  OUT)  :: p           ! Parameters
      TYPE(StrD_ContinuousStateType), INTENT(  OUT)  :: x           ! Initial continuous states
      TYPE(StrD_DiscreteStateType),   INTENT(  OUT)  :: xd          ! Initial discrete states
      TYPE(StrD_ConstraintStateType), INTENT(  OUT)  :: z           ! Initial guess of the constraint states
      TYPE(StrD_OtherStateType),      INTENT(  OUT)  :: OtherState  ! Initial other/optimization states
      TYPE(StrD_OutputType),          INTENT(  OUT)  :: y           ! Initial system outputs (outputs are not calculated;
                                                                    !   only the output mesh is initialized)
      REAL(DbKi),                     INTENT(INOUT)  :: Interval    ! Coupling interval in seconds: the rate that
                                                                    !   (1) StrD_UpdateStates() is called in loose coupling &
                                                                    !   (2) StrD_UpdateDiscState() is called in tight coupling.
                                                                    !   Input is the suggested time from the glue code;
                                                                    !   Output is the actual coupling interval that will be used
                                                                    !   by the glue code.
      TYPE(StrD_InitOutputType),      INTENT(  OUT)  :: InitOut     ! Output for initialization routine
      INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None

      
         ! Local variables
         
      TYPE(StrD_InputFile)                           :: InputFileData  ! Data stored in the module's input file       
      

      
         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Initialize the NWTC Subroutine Library

      CALL NWTC_Init( )

         ! Display the module information

      CALL DispNVD( StrD_Ver )

      
         ! Read the input file and validate the data
         
      CALL StrD_ReadInput( InitInp%InputFile, InputFileData, ErrStat, ErrMsg )
      CALL StrD_ValidateInput( InputFileData, p, ErrStat, ErrMsg )   
      
      
      
         ! Define parameters here:
         

      p%DT  = Interval


         ! Define initial system states here:

      xd%DummyDiscState          = 0
      z%DummyConstrState         = 0

      ! x =
      ! OtherState = 

         ! Define initial guess for the system inputs here:

      u%DummyInput = 0


         ! Define system output initializations (set up mesh) here:

      y%WriteOutput = 0


         ! Define initialization-routine output here:

      InitOut%WriteOutputHdr = (/ 'Time      ', 'Column2   ' /)
      InitOut%WriteOutputUnt = (/ '(s)',  '(-)'     /)


         ! If you want to choose your own rate instead of using what the glue code suggests, tell the glue code the rate at which
         !   this module must be called here:

       !Interval = p%DT


END SUBROUTINE StrD_Init
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE StrD_End( u, p, x, xd, z, OtherState, y, ErrStat, ErrMsg )
! This routine is called at the end of the simulation.
!..................................................................................................................................

      TYPE(StrD_InputType),           INTENT(INOUT)  :: u           ! System inputs
      TYPE(StrD_ParameterType),       INTENT(INOUT)  :: p           ! Parameters
      TYPE(StrD_ContinuousStateType), INTENT(INOUT)  :: x           ! Continuous states
      TYPE(StrD_DiscreteStateType),   INTENT(INOUT)  :: xd          ! Discrete states
      TYPE(StrD_ConstraintStateType), INTENT(INOUT)  :: z           ! Constraint states
      TYPE(StrD_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states
      TYPE(StrD_OutputType),          INTENT(INOUT)  :: y           ! System outputs
      INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None



         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Place any last minute operations or calculations here:


         ! Close files here:



         ! Destroy the input data:

      CALL StrD_DestroyInput( u, ErrStat, ErrMsg )


         ! Destroy the parameter data:

      CALL StrD_DestroyParam( p, ErrStat, ErrMsg )


         ! Destroy the state data:

      CALL StrD_DestroyContState(   x,           ErrStat, ErrMsg )
      CALL StrD_DestroyDiscState(   xd,          ErrStat, ErrMsg )
      CALL StrD_DestroyConstrState( z,           ErrStat, ErrMsg )
      CALL StrD_DestroyOtherState(  OtherState,  ErrStat, ErrMsg )


         ! Destroy the output data:

      CALL StrD_DestroyOutput( y, ErrStat, ErrMsg )




END SUBROUTINE StrD_End
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE StrD_UpdateStates( Time, u, p, x, xd, z, OtherState, ErrStat, ErrMsg )
! Loose coupling routine for solving for constraint states, integrating continuous states, and updating discrete states
! Constraint states are solved for input Time; Continuous and discrete states are updated for Time + Interval
!..................................................................................................................................

      REAL(DbKi),                      INTENT(IN   ) :: Time        ! Current simulation time in seconds
      TYPE(StrD_InputType),            INTENT(IN   ) :: u           ! Inputs at Time
      TYPE(StrD_ParameterType),        INTENT(IN   ) :: p           ! Parameters
      TYPE(StrD_ContinuousStateType),  INTENT(INOUT) :: x           ! Input: Continuous states at Time;
                                                                    !   Output: Continuous states at Time + Interval
      TYPE(StrD_DiscreteStateType),    INTENT(INOUT) :: xd          ! Input: Discrete states at Time;
                                                                    !   Output: Discrete states at Time  + Interval
      TYPE(StrD_ConstraintStateType),  INTENT(INOUT) :: z           ! Input: Initial guess of constraint states at Time;
                                                                    !   Output: Constraint states at Time
      TYPE(StrD_OtherStateType),       INTENT(INOUT) :: OtherState  ! Other/optimization states
      INTEGER(IntKi),                  INTENT(  OUT) :: ErrStat     ! Error status of the operation
      CHARACTER(*),                    INTENT(  OUT) :: ErrMsg      ! Error message if ErrStat /= ErrID_None

         ! Local variables

      TYPE(StrD_ContinuousStateType)                 :: dxdt        ! Continuous state derivatives at Time
      TYPE(StrD_ConstraintStateType)                 :: z_Residual  ! Residual of the constraint state equations (Z)

      INTEGER(IntKi)                                 :: ErrStat2    ! Error status of the operation (occurs after initial error)
      CHARACTER(LEN(ErrMsg))                         :: ErrMsg2     ! Error message if ErrStat2 /= ErrID_None

         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""



         ! Solve for the constraint states (z) here:

         ! Check if the z guess is correct and update z with a new guess.
         ! Iterate until the value is within a given tolerance.

      CALL StrD_CalcConstrStateResidual( Time, u, p, x, xd, z, OtherState, z_Residual, ErrStat, ErrMsg )
      IF ( ErrStat >= AbortErrLev ) THEN
         CALL StrD_DestroyConstrState( z_Residual, ErrStat2, ErrMsg2)
         ErrMsg = TRIM(ErrMsg)//' '//TRIM(ErrMsg2)
         RETURN
      END IF

      ! DO WHILE ( z_Residual% > tolerance )
      !
      !  z =
      !
      !  CALL StrD_CalcConstrStateResidual( Time, u, p, x, xd, z, OtherState, z_Residual, ErrStat, ErrMsg )
      !  IF ( ErrStat >= AbortErrLev ) THEN
      !     CALL StrD_DestroyConstrState( z_Residual, ErrStat2, ErrMsg2)
      !     ErrMsg = TRIM(ErrMsg)//' '//TRIM(ErrMsg2)
      !     RETURN
      !  END IF
      !
      ! END DO


         ! Destroy z_Residual because it is not necessary for the rest of the subroutine:

      CALL StrD_DestroyConstrState( z_Residual, ErrStat, ErrMsg)
      IF ( ErrStat >= AbortErrLev ) RETURN



         ! Get first time derivatives of continuous states (dxdt):

      CALL StrD_CalcContStateDeriv( Time, u, p, x, xd, z, OtherState, dxdt, ErrStat, ErrMsg )
      IF ( ErrStat >= AbortErrLev ) THEN
         CALL StrD_DestroyContState( dxdt, ErrStat2, ErrMsg2)
         ErrMsg = TRIM(ErrMsg)//' '//TRIM(ErrMsg2)
         RETURN
      END IF


         ! Update discrete states:
         !   Note that xd [discrete state] is changed in StrD_UpdateDiscState(), so StrD_CalcOutput(),
         !   StrD_CalcContStateDeriv(), and StrD_CalcConstrStates() must be called first (see above).

      CALL StrD_UpdateDiscState(Time, u, p, x, xd, z, OtherState, ErrStat, ErrMsg )
      IF ( ErrStat >= AbortErrLev ) THEN
         CALL StrD_DestroyContState( dxdt, ErrStat2, ErrMsg2)
         ErrMsg = TRIM(ErrMsg)//' '//TRIM(ErrMsg2)
         RETURN
      END IF


         ! Integrate (update) continuous states (x) here:

      !x = function of dxdt and x


         ! Destroy dxdt because it is not necessary for the rest of the subroutine

      CALL StrD_DestroyContState( dxdt, ErrStat, ErrMsg)
      IF ( ErrStat >= AbortErrLev ) RETURN



END SUBROUTINE StrD_UpdateStates
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE StrD_CalcOutput( Time, u, p, x, xd, z, OtherState, y, ErrStat, ErrMsg )
! Routine for computing outputs, used in both loose and tight coupling.
!..................................................................................................................................

      REAL(DbKi),                     INTENT(IN   )  :: Time        ! Current simulation time in seconds
      TYPE(StrD_InputType),           INTENT(IN   )  :: u           ! Inputs at Time
      TYPE(StrD_ParameterType),       INTENT(IN   )  :: p           ! Parameters
      TYPE(StrD_ContinuousStateType), INTENT(IN   )  :: x           ! Continuous states at Time
      TYPE(StrD_DiscreteStateType),   INTENT(IN   )  :: xd          ! Discrete states at Time
      TYPE(StrD_ConstraintStateType), INTENT(IN   )  :: z           ! Constraint states at Time
      TYPE(StrD_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states
      TYPE(StrD_OutputType),          INTENT(INOUT)  :: y           ! Outputs computed at Time (Input only so that mesh con-
                                                                    !   nectivity information does not have to be recalculated)
      INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None


         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Compute outputs here:

!      y%WriteOutput(1) = REAL(Time,ReKi)
!      y%WriteOutput(2) = 1.0_ReKi


END SUBROUTINE StrD_CalcOutput
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE StrD_CalcContStateDeriv( Time, u, p, x, xd, z, OtherState, dxdt, ErrStat, ErrMsg )
! Tight coupling routine for computing derivatives of continuous states
!..................................................................................................................................

      REAL(DbKi),                     INTENT(IN   )  :: Time        ! Current simulation time in seconds
      TYPE(StrD_InputType),           INTENT(IN   )  :: u           ! Inputs at Time
      TYPE(StrD_ParameterType),       INTENT(IN   )  :: p           ! Parameters
      TYPE(StrD_ContinuousStateType), INTENT(IN   )  :: x           ! Continuous states at Time
      TYPE(StrD_DiscreteStateType),   INTENT(IN   )  :: xd          ! Discrete states at Time
      TYPE(StrD_ConstraintStateType), INTENT(IN   )  :: z           ! Constraint states at Time
      TYPE(StrD_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states
      TYPE(StrD_ContinuousStateType), INTENT(  OUT)  :: dxdt        ! Continuous state derivatives at Time
      INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None


         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Compute the first time derivatives of the continuous states here:

!      dxdt%DummyContState = 0


END SUBROUTINE StrD_CalcContStateDeriv
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE StrD_UpdateDiscState( Time, u, p, x, xd, z, OtherState, ErrStat, ErrMsg )
! Tight coupling routine for updating discrete states
!..................................................................................................................................

      REAL(DbKi),                     INTENT(IN   )  :: Time        ! Current simulation time in seconds
      TYPE(StrD_InputType),           INTENT(IN   )  :: u           ! Inputs at Time
      TYPE(StrD_ParameterType),       INTENT(IN   )  :: p           ! Parameters
      TYPE(StrD_ContinuousStateType), INTENT(IN   )  :: x           ! Continuous states at Time
      TYPE(StrD_DiscreteStateType),   INTENT(INOUT)  :: xd          ! Input: Discrete states at Time;
                                                                       !   Output: Discrete states at Time + Interval
      TYPE(StrD_ConstraintStateType), INTENT(IN   )  :: z           ! Constraint states at Time
      TYPE(StrD_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states
      INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None


         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Update discrete states here:

      ! StateData%DiscState =

END SUBROUTINE StrD_UpdateDiscState
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE StrD_CalcConstrStateResidual( Time, u, p, x, xd, z, OtherState, z_residual, ErrStat, ErrMsg )
! Tight coupling routine for solving for the residual of the constraint state equations
!..................................................................................................................................

      REAL(DbKi),                     INTENT(IN   )  :: Time        ! Current simulation time in seconds
      TYPE(StrD_InputType),           INTENT(IN   )  :: u           ! Inputs at Time
      TYPE(StrD_ParameterType),       INTENT(IN   )  :: p           ! Parameters
      TYPE(StrD_ContinuousStateType), INTENT(IN   )  :: x           ! Continuous states at Time
      TYPE(StrD_DiscreteStateType),   INTENT(IN   )  :: xd          ! Discrete states at Time
      TYPE(StrD_ConstraintStateType), INTENT(IN   )  :: z           ! Constraint states at Time (possibly a guess)
      TYPE(StrD_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states
      TYPE(StrD_ConstraintStateType), INTENT(  OUT)  :: z_residual  ! Residual of the constraint state equations using
                                                                    !     the input values described above
      INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None


         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Solve for the constraint states here:

      z_residual%DummyConstrState = 0

END SUBROUTINE StrD_CalcConstrStateResidual
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! WE ARE NOT YET IMPLEMENTING THE JACOBIANS...
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!!----------------------------------------------------------------------------------------------------------------------------------
!SUBROUTINE StrD_JacobianPInput( Time, u, p, x, xd, z, OtherState, dYdu, dXdu, dXddu, dZdu, ErrStat, ErrMsg )
!! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete- (Xd), and constraint-state (Z) equations
!! with respect to the inputs (u). The partial derivatives dY/du, dX/du, dXd/du, and DZ/du are returned.
!!..................................................................................................................................
!
!      REAL(DbKi),                                INTENT(IN   )           :: Time       ! Current simulation time in seconds
!      TYPE(StrD_InputType),                   INTENT(IN   )           :: u          ! Inputs at Time
!      TYPE(StrD_ParameterType),               INTENT(IN   )           :: p          ! Parameters
!      TYPE(StrD_ContinuousStateType),         INTENT(IN   )           :: x          ! Continuous states at Time
!      TYPE(StrD_DiscreteStateType),           INTENT(IN   )           :: xd         ! Discrete states at Time
!      TYPE(StrD_ConstraintStateType),         INTENT(IN   )           :: z          ! Constraint states at Time
!      TYPE(StrD_OtherStateType),              INTENT(INOUT)           :: OtherState ! Other/optimization states
!      TYPE(StrD_PartialOutputPInputType),     INTENT(  OUT), OPTIONAL :: dYdu       ! Partial derivatives of output equations
!                                                                                       !   (Y) with respect to the inputs (u)
!      TYPE(StrD_PartialContStatePInputType),  INTENT(  OUT), OPTIONAL :: dXdu       ! Partial derivatives of continuous state
!                                                                                       !   equations (X) with respect to inputs (u)
!      TYPE(StrD_PartialDiscStatePInputType),  INTENT(  OUT), OPTIONAL :: dXddu      ! Partial derivatives of discrete state
!                                                                                       !   equations (Xd) with respect to inputs (u)
!      TYPE(StrD_PartialConstrStatePInputType),INTENT(  OUT), OPTIONAL :: dZdu       ! Partial derivatives of constraint state
!                                                                                       !   equations (Z) with respect to inputs (u)
!      INTEGER(IntKi),                            INTENT(  OUT)           :: ErrStat    ! Error status of the operation
!      CHARACTER(*),                              INTENT(  OUT)           :: ErrMsg     ! Error message if ErrStat /= ErrID_None
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
!END SUBROUTINE StrD_JacobianPInput
!!----------------------------------------------------------------------------------------------------------------------------------
!SUBROUTINE StrD_JacobianPContState( Time, u, p, x, xd, z, OtherState, dYdx, dXdx, dXddx, dZdx, ErrStat, ErrMsg )
!! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete- (Xd), and constraint-state (Z) equations
!! with respect to the continuous states (x). The partial derivatives dY/dx, dX/dx, dXd/dx, and DZ/dx are returned.
!!..................................................................................................................................
!
!      REAL(DbKi),                                    INTENT(IN   )           :: Time       ! Current simulation time in seconds
!      TYPE(StrD_InputType),                       INTENT(IN   )           :: u          ! Inputs at Time
!      TYPE(StrD_ParameterType),                   INTENT(IN   )           :: p          ! Parameters
!      TYPE(StrD_ContinuousStateType),             INTENT(IN   )           :: x          ! Continuous states at Time
!      TYPE(StrD_DiscreteStateType),               INTENT(IN   )           :: xd         ! Discrete states at Time
!      TYPE(StrD_ConstraintStateType),             INTENT(IN   )           :: z          ! Constraint states at Time
!      TYPE(StrD_OtherStateType),                  INTENT(INOUT)           :: OtherState ! Other/optimization states
!      TYPE(StrD_PartialOutputPContStateType),     INTENT(  OUT), OPTIONAL :: dYdx       ! Partial derivatives of output equations
!                                                                                           !   (Y) with respect to the continuous
!                                                                                           !   states (x)
!      TYPE(StrD_PartialContStatePContStateType),  INTENT(  OUT), OPTIONAL :: dXdx       ! Partial derivatives of continuous state
!                                                                                           !   equations (X) with respect to
!                                                                                           !   the continuous states (x)
!      TYPE(StrD_PartialDiscStatePContStateType),  INTENT(  OUT), OPTIONAL :: dXddx      ! Partial derivatives of discrete state
!                                                                                           !   equations (Xd) with respect to
!                                                                                           !   the continuous states (x)
!      TYPE(StrD_PartialConstrStatePContStateType),INTENT(  OUT), OPTIONAL :: dZdx       ! Partial derivatives of constraint state
!                                                                                           !   equations (Z) with respect to
!                                                                                           !   the continuous states (x)
!      INTEGER(IntKi),                                INTENT(  OUT)           :: ErrStat    ! Error status of the operation
!      CHARACTER(*),                                  INTENT(  OUT)           :: ErrMsg     ! Error message if ErrStat /= ErrID_None
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
!   END SUBROUTINE StrD_JacobianPContState
!!----------------------------------------------------------------------------------------------------------------------------------
!SUBROUTINE StrD_JacobianPDiscState( Time, u, p, x, xd, z, OtherState, dYdxd, dXdxd, dXddxd, dZdxd, ErrStat, ErrMsg )
!! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete- (Xd), and constraint-state (Z) equations
!! with respect to the discrete states (xd). The partial derivatives dY/dxd, dX/dxd, dXd/dxd, and DZ/dxd are returned.
!!..................................................................................................................................
!
!      REAL(DbKi),                                    INTENT(IN   )           :: Time       ! Current simulation time in seconds
!      TYPE(StrD_InputType),                       INTENT(IN   )           :: u          ! Inputs at Time
!      TYPE(StrD_ParameterType),                   INTENT(IN   )           :: p          ! Parameters
!      TYPE(StrD_ContinuousStateType),             INTENT(IN   )           :: x          ! Continuous states at Time
!      TYPE(StrD_DiscreteStateType),               INTENT(IN   )           :: xd         ! Discrete states at Time
!      TYPE(StrD_ConstraintStateType),             INTENT(IN   )           :: z          ! Constraint states at Time
!      TYPE(StrD_OtherStateType),                  INTENT(INOUT)           :: OtherState ! Other/optimization states
!      TYPE(StrD_PartialOutputPDiscStateType),     INTENT(  OUT), OPTIONAL :: dYdxd      ! Partial derivatives of output equations
!                                                                                           !  (Y) with respect to the discrete
!                                                                                           !  states (xd)
!      TYPE(StrD_PartialContStatePDiscStateType),  INTENT(  OUT), OPTIONAL :: dXdxd      ! Partial derivatives of continuous state
!                                                                                           !   equations (X) with respect to the
!                                                                                           !   discrete states (xd)
!      TYPE(StrD_PartialDiscStatePDiscStateType),  INTENT(  OUT), OPTIONAL :: dXddxd     ! Partial derivatives of discrete state
!                                                                                           !   equations (Xd) with respect to the
!                                                                                           !   discrete states (xd)
!      TYPE(StrD_PartialConstrStatePDiscStateType),INTENT(  OUT), OPTIONAL :: dZdxd      ! Partial derivatives of constraint state
!                                                                                           !   equations (Z) with respect to the
!                                                                                           !   discrete states (xd)
!      INTEGER(IntKi),                                INTENT(  OUT)           :: ErrStat    ! Error status of the operation
!      CHARACTER(*),                                  INTENT(  OUT)           :: ErrMsg     ! Error message if ErrStat /= ErrID_None
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
!END SUBROUTINE StrD_JacobianPDiscState
!!----------------------------------------------------------------------------------------------------------------------------------
!SUBROUTINE StrD_JacobianPConstrState( Time, u, p, x, xd, z, OtherState, dYdz, dXdz, dXddz, dZdz, ErrStat, ErrMsg )
!! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete- (Xd), and constraint-state (Z) equations
!! with respect to the constraint states (z). The partial derivatives dY/dz, dX/dz, dXd/dz, and DZ/dz are returned.
!!..................................................................................................................................
!
!      REAL(DbKi),                                      INTENT(IN   )           :: Time       ! Current simulation time in seconds
!      TYPE(StrD_InputType),                         INTENT(IN   )           :: u          ! Inputs at Time
!      TYPE(StrD_ParameterType),                     INTENT(IN   )           :: p          ! Parameters
!      TYPE(StrD_ContinuousStateType),               INTENT(IN   )           :: x          ! Continuous states at Time
!      TYPE(StrD_DiscreteStateType),                 INTENT(IN   )           :: xd         ! Discrete states at Time
!      TYPE(StrD_ConstraintStateType),               INTENT(IN   )           :: z          ! Constraint states at Time
!      TYPE(StrD_OtherStateType),                    INTENT(INOUT)           :: OtherState ! Other/optimization states
!      TYPE(StrD_PartialOutputPConstrStateType),     INTENT(  OUT), OPTIONAL :: dYdz       ! Partial derivatives of output
!                                                                                             !  equations (Y) with respect to the
!                                                                                             !  constraint states (z)
!      TYPE(StrD_PartialContStatePConstrStateType),  INTENT(  OUT), OPTIONAL :: dXdz       ! Partial derivatives of continuous
!                                                                                             !  state equations (X) with respect to
!                                                                                             !  the constraint states (z)
!      TYPE(StrD_PartialDiscStatePConstrStateType),  INTENT(  OUT), OPTIONAL :: dXddz      ! Partial derivatives of discrete state
!                                                                                             !  equations (Xd) with respect to the
!                                                                                             !  constraint states (z)
!      TYPE(StrD_PartialConstrStatePConstrStateType),INTENT(  OUT), OPTIONAL :: dZdz       ! Partial derivatives of constraint
!                                                                                             ! state equations (Z) with respect to
!                                                                                             !  the constraint states (z)
!      INTEGER(IntKi),                                  INTENT(  OUT)           :: ErrStat    ! Error status of the operation
!      CHARACTER(*),                                    INTENT(  OUT)           :: ErrMsg     ! Error message if ErrStat /= ErrID_None
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
!END SUBROUTINE StrD_JacobianPConstrState
!!----------------------------------------------------------------------------------------------------------------------------------

!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE StrD_ReadInput( InputFileName, InputFileData, ErrStat, ErrMsg )
! This subroutine reads the input file and stores all the data in the StrD_InputFile structure. 
! It does not perform data validation.
!..................................................................................................................................

      ! Passed varialbes
      
   CHARACTER(*), INTENT(IN)               :: InputFileName  ! Name of the input file       

   TYPE(StrD_InputFile), INTENT(OUT)      :: InputFileData  ! Data stored in the module's input file       
   INTEGER(IntKi),       INTENT(OUT)      :: ErrStat        ! The error status code 
   CHARACTER(*),         INTENT(OUT)      :: ErrMsg         ! The error message, if an error occurred 
   
      ! local variables
      
   INTEGER(IntKi)                         :: UnIn           ! Unit number for the input file
   INTEGER(IntKi)                         :: UnEcho         ! Unit number for the echo file
   
   ErrStat = ErrID_None
   ErrMsg  = ''
   
   
!===================== FAST_Input
   
   CALL ExitThisRoutine(ErrID_None, '')

   
CONTAINS
   !............................................................................................................................
   SUBROUTINE ExitThisRoutine(ErrID,Msg)
   ! This subroutine cleans up all the allocatable arrays, closes the file, and sets the error status/message
   !............................................................................................................................

         ! Passed arguments
      INTEGER(IntKi), INTENT(IN) :: ErrID       ! The error ID (ErrStat)
      CHARACTER(*),   INTENT(IN) :: Msg         ! The error message (ErrMsg)

         ! Set error status/message

      ErrStat = ErrID
      ErrMsg  = Msg
      IF ( ErrStat /= ErrID_None ) THEN
         ErrMsg = 'Error in StrD_ReadInput: '//TRIM(ErrMsg)
      END IF


      !.........................................................................................................................
      ! Close files
      !.........................................................................................................................
      CLOSE(UnIn)
      CLOSE(UnEcho)

   END SUBROUTINE ExitThisRoutine


END SUBROUTINE StrD_ReadInput
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE StrD_ValidateInput( InputFileData, p, ErrStat, ErrMsg )
! This subroutine validates the input file data
!..................................................................................................................................

   TYPE(StrD_InputFile),     INTENT(IN)       :: InputFileData  ! Data stored in the module's input file
   TYPE(StrD_ParameterType), INTENT(INOUT)    :: p              ! The module's parameter data
   INTEGER(IntKi),           INTENT(OUT)      :: ErrStat        ! The error status code 
   CHARACTER(*),             INTENT(OUT)      :: ErrMsg         ! The error message, if an error occurred 
   
      ! local variables
      
   INTEGER(IntKi)                             :: UnIn           ! Unit number for input file           
      
   
      ! Initialize variables
      
   ErrStat = ErrID_None
   ErrMsg  = ''
   
   CALL GetNewUnit( UnIn, ErrStat, ErrMsg )
   IF ( ErrStat >= AbortErrLev ) THEN
      RETURN
   ELSE
      ErrStat = ErrID_Info
   END IF
   
!!!!!!!!!!!!!!!!!!!   
      ! Check to see if any inputted output channels are ill-conditioned (and if so, Abort)
   !    and set values for OutParam(:):

  ! CALL ChckOutLst( InputFileData%OutList, p, ErrStat, ErrMsg )
   
!!!!!!!!!!!!!!!!!

CONTAINS
   !............................................................................................................................
   SUBROUTINE ExitThisRoutine(ErrID,Msg)
   ! This subroutine cleans up all the allocatable arrays, closes the file, and sets the error status/message
   !............................................................................................................................

         ! Passed arguments
      INTEGER(IntKi), INTENT(IN) :: ErrID       ! The error ID (ErrStat)
      CHARACTER(*),   INTENT(IN) :: Msg         ! The error message (ErrMsg)

         ! Set error status/message

      ErrStat = ErrID
      ErrMsg  = Msg
      IF ( ErrStat /= ErrID_None ) THEN
         ErrMsg = 'Error in StrD_ValidateInput: '//TRIM(ErrMsg)
      END IF


      !.........................................................................................................................
      ! Close file
      !.........................................................................................................................
      CLOSE(UnIn)

   END SUBROUTINE ExitThisRoutine


END SUBROUTINE StrD_ValidateInput
!----------------------------------------------------------------------------------------------------------------------------------

END MODULE StructDyn
!**********************************************************************************************************************************

