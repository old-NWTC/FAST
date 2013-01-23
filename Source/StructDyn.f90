
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
  
   

   INTEGER(IntKi), PARAMETER        :: NPA      =  9                                   ! Number of DOFs that contribute to the angular velocity of the tail (body A) in the inertia frame.
   INTEGER(IntKi), PARAMETER        :: NPB      =  7                                   ! Number of DOFs that contribute to the angular velocity of the tower top / baseplate (body B) in the inertia frame.
   INTEGER(IntKi), PARAMETER        :: NPF      =  7                                   ! Number of DOFs that contribute to the angular velocity of the tower elements (body F) in the inertia frame                                           (body F) in the inertia frame.
   INTEGER(IntKi), PARAMETER        :: NPG      = 10                                   ! Number of DOFs that contribute to the angular velocity of the generator (body G) in the inertia frame.
   INTEGER(IntKi), PARAMETER        :: NPL      = 11                                   ! Number of DOFs that contribute to the angular velocity of the low-speed shaft (body L) in the inertia frame.
   INTEGER(IntKi), PARAMETER        :: NPN      =  8                                   ! Number of DOFs that contribute to the angular velocity of the nacelle (body N) in the inertia frame.
   INTEGER(IntKi), PARAMETER        :: NPR      =  9                                   ! Number of DOFs that contribute to the angular velocity of the structure that furls with the rotor (not including rotor) (body R) in the inertia frame.
   INTEGER(IntKi), PARAMETER        :: NPX      =  3                                   ! Number of DOFs that contribute to the angular velocity of the platform (body X) in the inertia frame.

   INTEGER(IntKi), PARAMETER        :: PX(NPX)  = (/ DOF_R, DOF_P, DOF_Y /)                                                                                          ! Array of DOF indices (pointers) that contribute to the angular velocity of the platform                                                  (body X) in the inertia frame.
   INTEGER(IntKi), PARAMETER        :: PF(NPF)  = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2 /)                                                  ! Array of DOF indices (pointers) that contribute to the angular velocity of the tower elements                                            (body F) in the inertia frame.
   INTEGER(IntKi), PARAMETER        :: PB(NPB)  = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2 /)                                                  ! Array of DOF indices (pointers) that contribute to the angular velocity of the tower top / baseplate                                     (body B) in the inertia frame.
   INTEGER(IntKi), PARAMETER        :: PN(NPN)  = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2, DOF_Yaw /)                                         ! Array of DOF indices (pointers) that contribute to the angular velocity of the nacelle                                                   (body N) in the inertia frame.
   INTEGER(IntKi), PARAMETER        :: PR(NPR)  = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2, DOF_Yaw, DOF_RFrl /)                               ! Array of DOF indices (pointers) that contribute to the angular velocity of the structure that furls with the rotor (not including rotor) (body R) in the inertia frame.
   INTEGER(IntKi), PARAMETER        :: PL(NPL)  = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2, DOF_Yaw, DOF_RFrl, DOF_GeAz, DOF_DrTr /)           ! Array of DOF indices (pointers) that contribute to the angular velocity of the low-speed shaft                                           (body L) in the inertia frame.
   INTEGER(IntKi), PARAMETER        :: PG(NPG)  = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2, DOF_Yaw, DOF_RFrl, DOF_GeAz /)                     ! Array of DOF indices (pointers) that contribute to the angular velocity of the generator                                                 (body G) in the inertia frame.
   INTEGER(IntKi), PARAMETER        :: PA(NPA)  = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2, DOF_Yaw, DOF_TFrl /)                               ! Array of DOF indices (pointers) that contribute to the angular velocity of the tail                                                      (body A) in the inertia frame.
   
   


      ! Parameters related to coupling scheme -- Possibly a local variable elsewhere????


   INTEGER(IntKi), PARAMETER        :: NMX      =  9                                   ! Used in updating predictor-corrector values.



      ! Parameters related to coupling scheme -- Possibly a local variable elsewhere????


   INTEGER(IntKi), PARAMETER        :: PolyOrd  =  6                                    ! Order of the polynomial describing the mode shape


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

!BJJ REMOVE FOR NOW:   PRIVATE

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

!      CALL StrD_ReadInput( InitInp%InputFile, InputFileData, ErrStat, ErrMsg )
!      CALL StrD_ValidateInput( InputFileData, p, ErrStat, ErrMsg )


      CALL StrD_InitDOFs( OtherState%DOFs, p, ErrStat, ErrMsg )


         ! Define parameters here:


      p%DT  = Interval


         ! Define initial system states here:

      xd%DummyDiscState          = 0
      z%DummyConstrState         = 0

      ! x =
      ! OtherState =

         ! Define initial guess for the system inputs here:

!      u%DummyInput = 0


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



         ! Local variables:

      REAL(ReKi)                   :: AnchTe                        ! Instantaneous effective tension in a mooring line at the anchor   (N  )
      REAL(ReKi)                   :: AnchTeAng                     ! Instantaneous vertical angle    of a mooring line at the anchor   (rad)
      REAL(ReKi)                   :: AngAccEB  (3)                 ! Angular acceleration of the base plate                                                (body B) in the inertia frame (body E for earth).
      REAL(ReKi)                   :: AngAccER  (3)                 ! Angular acceleration of the structure that furls with the rotor (not including rotor) (body R) in the inertia frame (body E for earth).
      REAL(ReKi)                   :: AngAccEX  (3)                 ! Angular acceleration of the platform                                                  (body X) in the inertia frame (body E for earth).
      REAL(ReKi)                   :: ComDenom                      ! Common denominator used in several expressions.
      REAL(ReKi)                   :: CThrstys                      ! Estimate of the ys-location of the center of thrust.
      REAL(ReKi)                   :: CThrstzs                      ! Estimate of the zs-location of the center of thrust.
      REAL(ReKi)                   :: FairTe                        ! Instantaneous effective tension in a mooring line at the fairlead (N  )
      REAL(ReKi)                   :: FairTeAng                     ! Instantaneous vertical angle    of a mooring line at the fairlead (rad)
      REAL(ReKi)                   :: FrcMGagB  (3)                 ! Total force at the blade element   (body M) / blade strain gage location            (point S) due to the blade above the strain gage.
      REAL(ReKi)                   :: FrcFGagT  (3)                 ! Total force at the tower element   (body F) / tower strain gage location            (point T) due to the nacelle and rotor and tower above the strain gage.
      REAL(ReKi)                   :: FrcONcRt  (3)                 ! Total force at the yaw bearing (point O  ) due to the nacelle, generator, and rotor.
      REAL(ReKi)                   :: FrcPRot   (3)                 ! Total force at the teeter pin  (point P  ) due to the rotor.
      REAL(ReKi)                   :: FrcT0Trb  (3)                 ! Total force at the base of flexible portion of the tower (point T(0)) due to the entire wind turbine.
      REAL(ReKi)                   :: FZHydro   (3)                 ! Total platform hydrodynamic force at the platform reference (point Z).
      REAL(ReKi)                   :: HHWndVec  (3)                 ! Hub-height wind vector in the AeroDyn coordinate system.
      REAL(ReKi)                   :: LinAccEIMU(3)                 ! Total linear acceleration of the nacelle IMU (point IMU) in the inertia frame (body E for earth).
      REAL(ReKi)                   :: LinAccEO  (3)                 ! Total linear acceleration of the base plate (point O) in the inertia frame (body E for earth).
      REAL(ReKi)                   :: LinAccEZ  (3)                 ! Total linear acceleration of the platform refernce (point Z) in the inertia frame (body E for earth).
      REAL(ReKi)                   :: MomBNcRt  (3)                 ! Total moment at the base plate      (body B) / yaw bearing                           (point O) due to the nacelle, generator, and rotor.
      REAL(ReKi)                   :: MomFGagT  (3)                 ! Total moment at the tower element   (body F) / tower strain gage location            (point T) due to the nacelle and rotor and tower above the strain gage.
      REAL(ReKi)                   :: MomLPRot  (3)                 ! Total moment at the low-speed shaft (body L) / teeter pin                            (point P) due to the rotor.
      REAL(ReKi)                   :: MomMGagB  (3)                 ! Total moment at the blade element   (body M) / blade strain gage location            (point S) due to the blade above the strain gage.
      REAL(ReKi)                   :: MomNGnRt  (3)                 ! Total moment at the nacelle         (body N) / specified point on rotor-furl axis    (point V) due to the structure that furls with the rotor, generator, and rotor.
      REAL(ReKi)                   :: MomNTail  (3)                 ! Total moment at the nacelle         (body N) / specified point on  tail-furl axis    (point W) due to the tail.
      REAL(ReKi)                   :: MomX0Trb  (3)                 ! Total moment at the tower base      (body X) / base of flexible portion of the tower (point T(0)) due to the entire wind turbine.
      REAL(ReKi)                   :: MXHydro   (3)                 ! Total platform hydrodynamic moment acting at the platform (body X) / platform reference (point Z).
      REAL(ReKi)                   :: rOPO      (3)                 ! Position vector from the undeflected tower top (point O prime) to the deflected tower top (point O).
      REAL(ReKi)                   :: rOSTip    (3)                 ! Position vector from the deflected tower top (point O) to the deflected blade tip (point S tip).
      REAL(ReKi)                   :: rOSTipxn                      ! Component of rOSTip directed along the xn-axis.
      REAL(ReKi)                   :: rOSTipyn                      ! Component of rOSTip directed along the yn-axis.
      REAL(ReKi)                   :: rOSTipzn                      ! Component of rOSTip directed along the zn-axis.
      REAL(ReKi)                   :: rTPT      (3)                 ! Position vector from the undeflected tower node (point T prime) to the deflected node (point T)
      REAL(ReKi)                   :: rSPS      (3)                 ! Position vector from the undeflected blade node (point S prime) to the deflected node (point S)
      REAL(ReKi)                   :: rSTipPSTip(3)                 ! Position vector from the undeflected blade tip (point S tip prime) to the deflected blade tip (point S tip).
      REAL(ReKi)                   :: TmpVec    (3)                 ! A temporary vector used in various computations.
      REAL(ReKi)                   :: TmpVec2   (3)                 ! A temporary vector.

      INTEGER(IntKi)               :: I                             ! Generic index
      INTEGER(IntKi)               :: J                             ! Loops through nodes / elements.
      INTEGER(IntKi)               :: K                             ! Loops through blades.


         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Compute outputs here:

!      y%WriteOutput(1) = REAL(Time,ReKi)
!      y%WriteOutput(2) = 1.0_ReKi


   RETURN

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

      ! Passed variables

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
SUBROUTINE StrD_InitDOFs( DOFs, p, ErrStat, ErrMsg )
! This subroutine initialized the ActiveDOF data type
!..................................................................................................................................

   TYPE(ActiveDOFs),         INTENT(INOUT)    :: DOFs           ! ActiveDOF data 
   TYPE(StrD_ParameterType), INTENT(INOUT)    :: p              ! The module's parameter data
   INTEGER(IntKi),           INTENT(OUT)      :: ErrStat        ! The error status code
   CHARACTER(*),             INTENT(OUT)      :: ErrMsg         ! The error message, if an error occurred

      ! Local variables
   INTEGER(IntKi)                             :: K              ! Loop counter (for blades)
      
      ! Initialize variables

   ErrStat = ErrID_None
   ErrMsg  = ''

   
   
      ! BJJ: note that this method will cause an error if allocating data that has already been allocated...

   ALLOCATE ( DOFs%NPSBE(p%NumBl), DOFs%NPSE(p%NumBl),  STAT=ErrStat )
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine( ErrID_Fatal, ' Could not allocate memory for the ActiveAOFs NPSBE and NPSE arrays.' )
      RETURN
   ENDIF

   
   ALLOCATE ( DOFs%PCE(p%NDOF), DOFs%PDE(p%NDOF), DOFs%PIE(p%NDOF), STAT=ErrStat )
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine( ErrID_Fatal, ' Could not allocate memory for the ActiveAOFs PCE, PDE, and PIE arrays.' )
      RETURN
   ENDIF
   
   
   ALLOCATE (  DOFs%PTTE(p%NDOF), DOFs%PTE(p%NDOF), DOFs%PS(p%NDOF), STAT=ErrStat )
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine( ErrID_Fatal, ' Could not allocate memory for the ActiveAOFs PTTE, PTE, and PS arrays.' )
      RETURN
   ENDIF

   
   ALLOCATE ( DOFs%PUE(p%NDOF), DOFs%PYE(p%NDOF),  STAT=ErrStat )
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine( ErrID_Fatal, ' Could not allocate memory for the ActiveAOFs PUE and PYE arrays.' )
      RETURN
   ENDIF

   
!bjj was   ALLOCATE ( DOFs%PSBE(p%NumBl,3), DOFs%PSE(p%NumBl,p%NDOF),  STAT=ErrStat )
   ALLOCATE ( DOFs%PSBE(p%NumBl,(NumBE+NumBF)), DOFs%PSE(p%NumBl,p%NDOF),  STAT=ErrStat )
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine( ErrID_Fatal, ' Could not allocate memory for the ActiveAOFs PSBE and PSE arrays.' )
      RETURN
   ENDIF

   
   ALLOCATE ( DOFs%SrtPS(p%NDOF), DOFs%SrtPSNAUG(p%NAug),  DOFs%Diag(p%NDOF), STAT=ErrStat )
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine( ErrID_Fatal, ' Could not allocate memory for the ActiveAOFs SrtPS, SrtPSNAUG, and Diag arrays.' )
      RETURN
   ENDIF
   

   !...............................................................................................................................
! BJJ: these are now parameters....

   !...............................................................................................................................
   
      ! Allocate and Initialize arrays for DOFS that contribute to the angular velocity of the hub and blade elements
   
   IF ( p%NumBl == 2 )  THEN ! 2-blader
      p%NPH = 12                         ! Number of DOFs that contribute to the angular velocity of the hub            (body H) in the inertia frame.
      p%NPM = 15                         ! Number of DOFs that contribute to the angular velocity of the blade elements (body M) in the inertia frame.
   ELSE                    ! 3-blader
      p%NPH = 11                         ! Number of DOFs that contribute to the angular velocity of the hub            (body H) in the inertia frame.
      p%NPM = 14                         ! Number of DOFs that contribute to the angular velocity of the blade elements (body M) in the inertia frame.
   ENDIF
        

   ALLOCATE ( p%PH(p%NPH),  p%PM(p%NumBl,p%NPM), STAT=ErrStat )
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine( ErrID_Fatal, ' Could not allocate memory for the ActiveAOFs PH and PM arrays.' )
      RETURN
   ENDIF     
  
      ! Array of DOF indices (pointers) that contribute to the angular velocity of the hub (body H) in the inertia frame:
   p%PH(1:11) = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2, DOF_Yaw, DOF_RFrl, DOF_GeAz, DOF_DrTr /) 
   
   IF ( p%NumBl == 2 )  THEN ! 2-blader (add DOF_Teet to the arrays)

      p%PH(12) = DOF_Teet
      
         ! Array of DOF indices (pointers) that contribute to the angular velocity of the blade elements (body M) in the inertia frame:
      DO K = 1,p%NumBl ! Loop through all blades
         p%PM(K,:) = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2, DOF_Yaw, DOF_RFrl, DOF_GeAz, DOF_DrTr, DOF_Teet, & 
                           DOF_BF(K,1) , DOF_BE(K,1)    , DOF_BF(K,2)                                                                   /)
      ENDDO          ! K - All blades

   ELSE                    ! 3-blader

         ! Array of DOF indices (pointers) that contribute to the angular velocity of the blade elements (body M) in the inertia frame:
      DO K = 1,p%NumBl ! Loop through all blades
         p%PM(K,:) = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2, DOF_Yaw, DOF_RFrl, DOF_GeAz, DOF_DrTr, &           
                           DOF_BF(K,1) , DOF_BE(K,1)    , DOF_BF(K,2)                                                         /)
      ENDDO          ! K - All blades

   ENDIF

   
   
   
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
         ErrMsg = 'Error in StrD_AllocDOFs: '//TRIM(ErrMsg)
      END IF


   END SUBROUTINE ExitThisRoutine


END SUBROUTINE StrD_InitDOFs
!----------------------------------------------------------------------------------------------------------------------------------
FUNCTION SHP(Fract, FlexL, ModShpAry, Deriv, ErrStat, ErrMsg)
! SHP calculates the Derive-derivative of the shape function ModShpAry at Fract.
! NOTE: This function only works for Deriv = 0, 1, or 2.
!----------------------------------------------------------------------------------------------------------------------------------

      ! Passed variables:

   REAL(ReKi),     INTENT(IN )    :: FlexL                     ! Length of flexible beam, (m)
   REAL(ReKi),     INTENT(IN )    :: Fract                     ! Fractional distance along flexible beam, 0<=Frac<=1
   REAL(ReKi),     INTENT(IN )    :: ModShpAry(:)              ! Array holding mode shape coefficients (2:PolyOrd)
   REAL(ReKi)                     :: SHP                       ! The shape function returned by this function.

   INTEGER(IntKi), INTENT(IN )    :: Deriv                     ! Which derivative to compute Deriv = 0 (regular function SHP), 1 (D(SHP)/DZ), 2 (D2(SHP)/DZ2)
   INTEGER(IntKi), INTENT(OUT)    :: ErrStat                   ! A error level that indicates if/what error occurred
   CHARACTER(*),   INTENT(OUT)    :: ErrMsg                    ! A message indicating the error if one occurred
      

      ! Local variables:

   INTEGER(IntKi)                 :: CoefTmp                   ! Temporary coefficient
   INTEGER(IntKi)                 :: I                         ! Counts through polynomial array.
   INTEGER(IntKi)                 :: J                         ! I+1
   INTEGER(IntKi)                 :: Swtch(0:2)                ! Corresponds to which derivative to compute.  Sets all portions of the coefficient = 0 except those that are relevant.
   

   IF ( Deriv < 0 .OR. Deriv > 2 ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = 'Function SHP input Deriv='//TRIM(Num2LStr(Deriv))//' is invalid. Deriv must be 0, 1, or 2.' 
      RETURN
   ELSEIF ( Fract < 0.0_ReKi .OR. Fract > 1.0_ReKi ) THEN
      ErrStat = ErrID_Warn
      ErrMsg  = 'Function SHP input Fract='//TRIM(Num2LStr(Fract))//' does not meet the condition 0<=Fract<=1.'
   ELSE
      ErrStat = ErrID_None
   END IF
      
   Swtch        = 0 ! Initialize Swtch(:) to 0
   Swtch(Deriv) = 1
   SHP          = 0.0

   DO I = 1,SIZE(ModShpAry,DIM=1,KIND=IntKi) ! =2,PolyOrd
      J = I + 1
      CoefTmp = Swtch(0) + Swtch(1)*J + Swtch(2)*I*J
         
      IF ( (J == 2) .AND. (Deriv == 2) ) THEN !bjj this could be removed as Fract**0 = 1 (0**0 = 1 in Fortran)
         SHP =       ModShpAry(I)*CoefTmp                         /( FlexL**Deriv )
      ELSE
         SHP = SHP + ModShpAry(I)*CoefTmp*( Fract**( J - Deriv ) )/( FlexL**Deriv )
      ENDIF
   ENDDO !I

   RETURN
   
END FUNCTION SHP
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE CoordSys_Alloc( CoordSys, p, ErrStat, ErrMsg )

   ! This subroutine allocates the coordinate systems in the StrD_CoordSys type.

IMPLICIT NONE

   ! passed arguments

TYPE(StrD_CoordSys),      INTENT(OUT) :: CoordSys       ! The coordinate systems, with arrays to be allocated
TYPE(StrD_ParameterType), INTENT(IN)  :: p              ! Parameters of the structural dynamics module

INTEGER(IntKi),           INTENT(OUT) :: ErrStat        ! Error status
CHARACTER(*),             INTENT(OUT) :: ErrMsg         ! Err msg


   ! local variables

CHARACTER(200), PARAMETER        :: ErrTxt = 'coordinate system arrays in SUBROUTINE CoordSys_Alloc.'


   ! Initialize ErrStat and ErrMsg

ErrStat = ErrID_None
ErrMsg  = ""


  ! Allocate coordinate system arrays:

ALLOCATE ( CoordSys%i1(p%NumBl,3), CoordSys%i2(p%NumBl,3), CoordSys%i3(p%NumBl,3), STAT=ErrStat ) !this argument doesn't work in IVF 10.1: , ERRMSG=ErrMsg
IF ( ErrStat /= 0 )  THEN
   ErrStat = ErrID_Fatal
   ErrMsg  = 'Error allocating the i1, i2, and i3 '//TRIM(ErrTxt)//' '//TRIM(ErrMsg)
   RETURN
END IF


ALLOCATE ( CoordSys%j1(p%NumBl,3), CoordSys%j2(p%NumBl,3), CoordSys%j3(p%NumBl,3), STAT=ErrStat ) !this argument doesn't work in IVF 10.1: , ERRMSG=ErrMsg
IF ( ErrStat /= 0 )  THEN
   ErrStat = ErrID_Fatal
   ErrMsg  = 'Error allocating the j1, j2, and j3 '//TRIM(ErrTxt)//' '//TRIM(ErrMsg)
   RETURN
END IF


ALLOCATE ( CoordSys%m1(p%NumBl,p%BldNodes,3), CoordSys%m2(p%NumBl,p%BldNodes,3), &
           CoordSys%m3(p%NumBl,p%BldNodes,3), STAT=ErrStat ) !this argument doesn't work in IVF 10.1: , ERRMSG=ErrMsg
IF ( ErrStat /= 0 )  THEN
   ErrStat = ErrID_Fatal
   ErrMsg  = 'Error allocating the m1, m2, and m3 '//TRIM(ErrTxt)//' '//TRIM(ErrMsg)
   RETURN
END IF


ALLOCATE ( CoordSys%n1(p%NumBl,p%BldNodes,3), CoordSys%n2(p%NumBl,p%BldNodes,3), &
           CoordSys%n3(p%NumBl,p%BldNodes,3), STAT=ErrStat ) !this argument doesn't work in IVF 10.1: , ERRMSG=ErrMsg
IF ( ErrStat /= 0 )  THEN
   ErrStat = ErrID_Fatal
   ErrMsg  = 'Error allocating the n1, n2, and n3 '//TRIM(ErrTxt)//' '//TRIM(ErrMsg)
   RETURN
END IF


ALLOCATE ( CoordSys%t1(p%TwrNodes,3), CoordSys%t2(p%TwrNodes,3), CoordSys%t3(p%TwrNodes,3), STAT=ErrStat ) !this argument doesn't work in IVF 10.1: , ERRMSG=ErrMsg
IF ( ErrStat /= 0 )  THEN
   ErrStat = ErrID_Fatal
   ErrMsg  = 'Error allocating the t1, t2, and t3 '//TRIM(ErrTxt)//' '//TRIM(ErrMsg)
   RETURN
END IF


ALLOCATE ( CoordSys%te1(p%NumBl,p%BldNodes,3), CoordSys%te2(p%NumBl,p%BldNodes,3), &
           CoordSys%te3(p%NumBl,p%BldNodes,3), STAT=ErrStat ) !this argument doesn't work in IVF 10.1: , ERRMSG=ErrMsg
IF ( ErrStat /= 0 )  THEN
   ErrStat = ErrID_Fatal
   ErrMsg  = 'Error allocating the te1, te2, and te3 '//TRIM(ErrTxt)//' '//TRIM(ErrMsg)
   RETURN
END IF


RETURN
END SUBROUTINE CoordSys_Alloc
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ValidateBladeData ( p, BladeKInputFileData, ChkAdmVals, ErrStat, ErrMsg )
! This routine checks the blade file input data for errors
!----------------------------------------------------------------------------------------------------------------------------------
   TYPE(StrD_ParameterType), INTENT(INOUT)  :: p                                   ! Parameters of the structural dynamics module
   TYPE(BladeInputData),     INTENT(INOUT)  :: BladeKInputFileData                 ! Data for Blade K stored in the module's input file
   LOGICAL,                  INTENT(IN)     :: ChkAdmVals                          ! Logical to determine if Adams inputs should be validated 
   INTEGER(IntKi),           INTENT(OUT)    :: ErrStat                             ! Error status
   CHARACTER(*),             INTENT(OUT)    :: ErrMsg                              ! Err msg

      ! local variables
   REAL(ReKi)                               :: TipDispl                            ! Blade tip displacement for a mode shape.
   INTEGER                                  :: I                                   ! Loop counter


   ErrStat = ErrID_None
   ErrMsg= ''
   
   
      ! Check that BlFract goes from 0.0 to 1.0 in increasing order:
   
   IF ( .NOT. EqualRealNos( BladeKInputFileData%BlFract(1), 0.0_ReKi ) ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = TRIM(ErrMsg)//NewLine//'  BlFract(1) must be 0.0.'
   END IF 

   IF ( BladeKInputFileData%NBlInpSt /= 1 .AND. &
      .NOT. EqualRealNos( BladeKInputFileData%BlFract(BladeKInputFileData%NBlInpSt), 1.0_ReKi )  ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = TRIM(ErrMsg)//NewLine//'  BlFract('//TRIM( Num2LStr( BladeKInputFileData%NBlInpSt ) )//') must be 1.0.' 
   END IF

   DO I = 2,BladeKInputFileData%NBlInpSt
      ErrStat = ErrID_Fatal
      ErrMsg  = TRIM(ErrMsg)//NewLine//'  BlFract('//TRIM( Num2LStr( BladeKInputFileData%NBlInpSt ) )//') must be 1.0.' 

      IF ( BladeKInputFileData%BlFract(I) <= BladeKInputFileData%BlFract(I-1) )  THEN
         ErrStat = ErrID_Fatal
         ErrMsg  = TRIM(ErrMsg)//NewLine//'  BlFract('//TRIM( Num2LStr( I ) )//') must be greater than BlFract('&
                                                      //TRIM( Num2LStr(I-1) )//').'
         
      ENDIF
   END DO 
   


   DO I = 1,BladeKInputFileData%NBlInpSt
   
         ! Check that AerCen is contained in [0.0, 1.0]:
      IF ( ( BladeKInputFileData%AerCen(I) ) < 0.0_ReKi .OR. ( BladeKInputFileData%AerCen(I) > 1.0_ReKi ) )  THEN
         ErrStat = ErrID_Fatal
         ErrMsg  = TRIM(ErrMsg)//NewLine//'  AerCen('//TRIM( Num2LStr( I ) )//') must be between 0 and 1 (inclusive).'
      END IF
      
         ! Check that StrcTwst is contained in (-180.0, 180.0]:
      IF ( ( BladeKInputFileData%StrcTwst(I) <= -180.0_ReKi ) .OR. ( BladeKInputFileData%StrcTwst(I) > 180.0_ReKi ) )  THEN
         ErrStat = ErrID_Fatal
         ErrMsg  = TRIM(ErrMsg)//NewLine//'  StrcTwst('//TRIM( Num2LStr( I ) ) // &
                     ') must be greater than -180 and less than or equal to 180.'
      END IF
   
         ! Check that BMassDen is contained in (0.0, inf):
      IF ( BladeKInputFileData%BMassDen(I) <= 0.0_ReKi )  THEN
         ErrStat = ErrID_Fatal
         ErrMsg  = TRIM(ErrMsg)//NewLine//'  BMassDen('//TRIM( Num2LStr( I ) )//') must be greater than zero.'
      END IF
      
         ! Check that FlpStff is contained in (0.0, inf):
      IF ( BladeKInputFileData%FlpStff (I) <= 0.0_ReKi )  THEN
         ErrStat = ErrID_Fatal
         ErrMsg  = TRIM(ErrMsg)//NewLine//'  FlpStff('//TRIM( Num2LStr( I ) )//') must be greater than zero.'
      END IF
   
         ! Check that EdgStff is contained in (0.0, inf):
      IF ( BladeKInputFileData%EdgStff (I) <= 0.0_ReKi )  THEN
         ErrStat = ErrID_Fatal
         ErrMsg  = TRIM(ErrMsg)//NewLine//'  EdgStff('//TRIM( Num2LStr( I ) )//') must be greater than zero.'
      END IF
   
   END DO
   
   
   IF ( ChkAdmVals ) THEN  ! Check values for Adams input

      
         ! The reference axis must be coincident with the pitch axis at the blade root (I == 1):
      IF ( .NOT. EqualRealNos( BladeKInputFileData%PrecrvRef(1), 0.0_ReKi ) .OR. &
            .NOT. EqualRealNos( BladeKInputFileData%PreswpRef(1), 0.0_ReKi )      )  THEN
         ErrStat = ErrID_Fatal
         ErrMsg  = TRIM(ErrMsg)//NewLine//'  Both PrecrvRef(1) and PreswpRef(1) must be zero '//&
                            '(the reference axis must be coincident with the pitch axis at the blade root).'
      END IF       

      
      DO I = 1,BladeKInputFileData%NBlInpSt
   
            ! Check that GJStff is contained in (0.0, inf):
         IF ( BladeKInputFileData%GJStff(I) <= 0.0_ReKi )  THEN
            ErrStat = ErrID_Fatal
            ErrMsg  = TRIM(ErrMsg)//NewLine//'  GJStff('//TRIM( Num2LStr( I ) )//') must be greater than zero.'
         END IF
         
            ! Check that EAStff is contained in (0.0, inf):
         IF ( BladeKInputFileData%EAStff(I) <= 0.0_ReKi )  THEN
            ErrStat = ErrID_Fatal
            ErrMsg  = TRIM(ErrMsg)//NewLine//'  EAStff('//TRIM( Num2LStr( I ) )//') must be greater than zero.'
         END IF
         
            ! Check that Alpha is contained in (-1.0, 1):
         IF ( ( BladeKInputFileData%Alpha(I) <= -1.0_ReKi ) .OR. ( BladeKInputFileData%Alpha(I) >= 1.0_ReKi ) )  THEN
            ErrStat = ErrID_Fatal
            ErrMsg  = TRIM(ErrMsg)//NewLine//'  Alpha('//TRIM( Num2LStr( I ) )//') (the blade flap/twist'// &
                         ' coupling coefficient) must be between -1 and 1 (exclusive).'
         END IF
         
            ! Check that FlpIner is contained in [0.0, inf):
         IF ( BladeKInputFileData%FlpIner(I) <  0.0_ReKi )  THEN
            ErrStat = ErrID_Fatal
            ErrMsg  = TRIM(ErrMsg)//NewLine//'  FlpIner('//TRIM( Num2LStr( I ) )//') must not be less than zero.'
         END IF
         
            ! Check that EdgIner is contained in [0.0, inf):
         IF ( BladeKInputFileData%EdgIner(I) <  0.0_ReKi )  THEN
            ErrStat = ErrID_Fatal
            ErrMsg  = TRIM(ErrMsg)//NewLine//'  EdgIner('//TRIM( Num2LStr( I ) )//') must not be less than zero.'
         END IF
                           
            ! Check that PrecrvRef is 0.0 for Adams models:
         IF ( .NOT. EqualRealNos( BladeKInputFileData%PrecrvRef(I), 0.0_ReKi) )  THEN
            ErrStat = ErrID_Fatal
            ErrMsg  = TRIM(ErrMsg)//NewLine//'  PrecrvRef('//TRIM( Num2LStr( I ) )//') must be zero for Adams models.'
         END IF
         
            ! Check that GJStff is contained in (0.0, inf):
         IF ( .NOT. EqualRealNos( BladeKInputFileData%PreswpRef(I), 0.0_ReKi) )  THEN
            ErrStat = ErrID_Fatal
            ErrMsg  = TRIM(ErrMsg)//NewLine//'  PreswpRef('//TRIM( Num2LStr( I ) )//') must be zero for Adams models.'
         END IF
         
      END DO
               
   END IF  ! check for Adams models
   
      
      ! Check that the blade damping is not negative:  
      
   IF ( ANY( BladeKInputFileData%BldFlDmp < 0.0_ReKi ) ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = TRIM(ErrMsg)//NewLine//'  BldFlDmp must not be negative.'
   END IF

   IF ( ANY( BladeKInputFileData%BldEdDmp < 0.0_ReKi ) ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = TRIM(ErrMsg)//NewLine//'  BldEdDmp must not be negative.'
   END IF
   

      ! Check that the stiffness tuner isn't negative:
      
   IF ( ANY( BladeKInputFileData%FlStTunr <= 0.0_ReKi ) ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = TRIM(ErrMsg)//NewLine//'  FlStTunr must be greater than zero.'
   END IF

   
      ! Check that the mode shape coefficients add to 1.0:
      ! bjj: old check was this:
      ! ( ABS( TipDispl - 1.0 ) > 0.001 )

   TipDispl = SUM(BladeKInputFileData%BldFl1Sh)
   IF ( .NOT. EqualRealNos( TipDispl, 1.0_ReKi ) ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = TRIM(ErrMsg)//NewLine//'  Blade-flap mode-1 shape coefficients must add to 1.0.'
   END IF
   

   TipDispl = SUM(BladeKInputFileData%BldFl2Sh)
   IF ( .NOT. EqualRealNos( TipDispl, 1.0_ReKi ) ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = TRIM(ErrMsg)//NewLine//'  Blade-flap mode-2 shape coefficients must add to 1.0.'
   END IF   
   

   TipDispl = SUM(BladeKInputFileData%BldEdgSh)
   IF ( .NOT. EqualRealNos( TipDispl, 1.0_ReKi ) ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = TRIM(ErrMsg)//NewLine//'  Blade-edge mode shape coefficients must add to 1.0.'
   END IF  


END SUBROUTINE ValidateBladeData
!----------------------------------------------------------------------------------------------------------------------------------

SUBROUTINE ReadBladeFile ( BldFile, p, BladeKInputFileData, ReadAdmVals, ErrStat, ErrMsg )


   ! This routine reads a blade file and validates the input.

IMPLICIT                        NONE


   ! Passed variables:

TYPE(StrD_ParameterType), INTENT(INOUT)  :: p                                   ! Parameters of the structural dynamics module
TYPE(BladeInputData),     INTENT(INOUT)  :: BladeKInputFileData                 ! Data for Blade K stored in the module's input file
CHARACTER(*),             INTENT(IN)     :: BldFile                             ! Name of the blade input file data
LOGICAL,                  INTENT(IN)     :: ReadAdmVals                         ! Logical to determine if Adams inputs should be read from file 

INTEGER(IntKi),           INTENT(OUT)    :: ErrStat        ! Error status
CHARACTER(*),             INTENT(OUT)    :: ErrMsg         ! Err msg


   ! Local variables:
   
REAL(ReKi)                   :: AdjBlMs                                         ! Factor to adjust blade mass density.
REAL(ReKi)                   :: AdjEdSt                                         ! Factor to adjust edge stiffness.
REAL(ReKi)                   :: AdjFlSt                                         ! Factor to adjust flap stiffness.

REAL(ReKi)                   :: TmpRAry(17)                                     ! Temporary variable to read table from file (up to 17 columns)
   
INTEGER(IntKi)               :: I                                               ! A generic DO index.
INTEGER( IntKi )             :: UnIn                                            ! Unit number for reading file
INTEGER( IntKi )             :: NInputCols                                      ! Number of columns to be read from the file
INTEGER(IntKi)               :: ErrStat2                                        ! Temporary Error status
CHARACTER(LEN(ErrMsg))       :: ErrMsg2                                         ! Temporary Err msg



CALL GetNewUnit( UnIn, ErrStat, ErrMsg )
IF ( ErrStat >= AbortErrLev ) RETURN


   ! Open the input file for blade K.

CALL OpenFInpFile ( UnIn, BldFile, ErrStat2, ErrMsg2 )
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN


!  -------------- HEADER -------------------------------------------------------

   ! Ship the header.

CALL ReadCom ( UnIn, BldFile, 'unused blade file header line 1', ErrStat2, ErrMsg2 )
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN

CALL ReadCom ( UnIn, BldFile, 'unused blade file header line 2', ErrStat2, ErrMsg2 )
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN

CALL ReadCom ( UnIn, BldFile, 'unused blade file header line 3', ErrStat2, ErrMsg2 )
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN

!  -------------- BLADE PARAMETERS ---------------------------------------------


   ! Skip the comment line.

CALL ReadCom ( UnIn, BldFile, 'blade parameters', ErrStat2, ErrMsg2  )
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN

   ! NBlInpSt - Number of blade input stations.

CALL ReadVar ( UnIn, BldFile, BladeKInputFileData%NBlInpSt, 'NBlInpSt', 'Number of blade input stations' )
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN
   

   ! Allocate the arrays based on this NBlInpSt input 
CALL Alloc_BladeProperties( BladeKInputFileData, ErrStat2, ErrMsg2 )
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN

   
   ! CalcBMode - Calculate blade mode shapes (switch).

!JASON: ADD LOGIC FOR THIS NEW VARIABLE:
!JASON:CALL ReadVar ( UnIn, BldFile, BladeKInputFileData%CalcBMode, 'CalcBMode', 'Calculate blade mode shapes' )
CALL ReadCom ( UnIn, BldFile, 'currently ignored CalcBMode', ErrStat2, ErrMsg2  )
   BladeKInputFileData%CalcBMode = .FALSE.
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN


   ! BldFlDmp - Blade structural damping ratios in flapwise direction.

CALL ReadAryLines( UnIn, BldFile, BladeKInputFileData%BldFlDmp, SIZE(BladeKInputFileData%BldFlDmp), 'BldFlDmp', &
                                    'Blade structural damping ratios in flapwise direction', ErrStat2  )
   ErrMsg2 = ' Error reading BldFlDmp array from '//TRIM(BldFile)//'.'
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN



   ! BldEdDmp - Blade structural damping ratios in edgewise direction.

CALL ReadAryLines( UnIn, BldFile, BladeKInputFileData%BldEdDmp, SIZE(BladeKInputFileData%BldEdDmp), 'BldEdDmp', &
                                    'Blade structural damping ratios in edgewise direction', ErrStat2 )
   ErrMsg2 = ' Error reading BldEdDmp array from '//TRIM(BldFile)//'.'
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN

!  -------------- BLADE ADJUSTMENT FACTORS -------------------------------------


   ! Skip the comment line.

CALL ReadCom ( UnIn, BldFile, 'blade adjustment factors', ErrStat2, ErrMsg2  )
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN

   
   ! FlStTunr(1) - Blade flapwise modal stiffness tuners.

CALL ReadAryLines ( UnIn, BldFile, BladeKInputFileData%FlStTunr, SIZE(BladeKInputFileData%FlStTunr), 'FlStTunr', &
                                               'Blade flapwise modal stiffness tuners', ErrStat2 )
   ErrMsg2 = ' Error reading FlStTunr array from '//TRIM(BldFile)//'.'
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN



   ! AdjBlMs - Factor to adjust blade mass density.

CALL ReadVar ( UnIn, BldFile, AdjBlMs, 'AdjBlMs', 'Factor to adjust blade mass density', ErrStat2, ErrMsg2 )
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN



   ! AdjFlSt - Factor to adjust blade flap stiffness.

CALL ReadVar ( UnIn, BldFile, AdjFlSt, 'AdjFlSt', 'Factor to adjust blade flap stiffness', ErrStat2, ErrMsg2 )
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN



   ! AdjEdSt - Factor to adjust blade edge stiffness.

CALL ReadVar ( UnIn, BldFile, AdjEdSt, 'AdjEdSt', 'Factor to adjust blade edge stiffness', ErrStat2, ErrMsg2 )
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN


   
      ! Check the locally-defined adjustment factors: AdjBlMs, AdjFlSt, AdjEdSt
   
   IF ( AdjBlMs <= 0.0_ReKi ) THEN
      CALL CheckError( ErrID_Warn, ' AdjBlMs must be greater than zero.' )
      IF ( ErrStat >= AbortErrLev ) RETURN
   END IF

   IF ( AdjFlSt <= 0.0_ReKi ) THEN
      CALL CheckError( ErrID_Warn, ' AdjFlSt must be greater than zero.' )
      IF ( ErrStat >= AbortErrLev ) RETURN
   END IF
   
   IF ( AdjEdSt <= 0.0_ReKi ) THEN
      CALL CheckError( ErrID_Warn, ' AdjEdSt must be greater than zero.' )
      IF ( ErrStat >= AbortErrLev ) RETURN
   END IF   


!  -------------- DISTRIBUTED BLADE PROPERTIES ---------------------------------


   ! Skip the comment lines.

CALL ReadCom ( UnIn, BldFile, 'distributed blade parameters'     , ErrStat2, ErrMsg2 )
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN

CALL ReadCom ( UnIn, BldFile, 'distributed-blade-parameter names', ErrStat2, ErrMsg2 )
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN

CALL ReadCom ( UnIn, BldFile, 'distributed-blade-parameter units', ErrStat2, ErrMsg2 )
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN



   ! Read the table.

IF ( ReadAdmVals ) THEN
   NInputCols = 17
ELSE
   NInputCols = 6
END IF   


DO I=1,BladeKInputFileData%NBlInpSt

   CALL ReadAry( UnIn, BldFile, TmpRAry, NInputCols, 'Line'//TRIM(Num2LStr(I)), 'Blade input station table', ErrStat2 )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

   BladeKInputFileData%BlFract( I) = TmpRAry(1)
   BladeKInputFileData%AerCen(  I) = TmpRAry(2)
   BladeKInputFileData%StrcTwst(I) = TmpRAry(3)
   BladeKInputFileData%BMassDen(I) = TmpRAry(4)*AdjBlMs  ! Apply the correction factors to the elemental data.
   BladeKInputFileData%FlpStff( I) = TmpRAry(5)*AdjFlSt  ! Apply the correction factors to the elemental data.
   BladeKInputFileData%EdgStff( I) = TmpRAry(6)*AdjEdSt  ! Apply the correction factors to the elemental data.
   
   IF ( NInputCols > 6 ) THEN
      BladeKInputFileData%GJStff(   I) = TmpRAry( 7)
      BladeKInputFileData%EAStff(   I) = TmpRAry( 8)
      BladeKInputFileData%Alpha(    I) = TmpRAry( 9)
      BladeKInputFileData%FlpIner(  I) = TmpRAry(10)
      BladeKInputFileData%EdgIner(  I) = TmpRAry(11)
      BladeKInputFileData%PrecrvRef(I) = TmpRAry(12)
      BladeKInputFileData%PreswpRef(I) = TmpRAry(13)
      BladeKInputFileData%FlpcgOf(  I) = TmpRAry(14)
      BladeKInputFileData%EdgcgOf(  I) = TmpRAry(15)
      BladeKInputFileData%FlpEAOf(  I) = TmpRAry(16)
      BladeKInputFileData%EdgEAOf(  I) = TmpRAry(17)
   END IF        
ENDDO ! I



!  -------------- BLADE MODE SHAPES --------------------------------------------


   ! Skip the comment line.

CALL ReadCom ( UnIn, BldFile, 'blade mode shapes', ErrStat2, ErrMsg2  )
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN


   ! BldFl1Sh - Blade-flap mode-1 shape coefficients.
CALL ReadAryLines ( UnIn, BldFile, BladeKInputFileData%BldFl1Sh, SIZE(BladeKInputFileData%BldFl1Sh), 'BldFl1Sh', &
                        'Blade-flap mode-1 shape coefficients', ErrStat2 )
   ErrMsg2 = ' Error reading BldFl1Sh array from '//TRIM(BldFile)//'.'
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN


   ! BldFl2Sh - Blade-flap mode-2 shape coefficients.

CALL ReadAryLines ( UnIn, BldFile, BladeKInputFileData%BldFl2Sh, SIZE(BladeKInputFileData%BldFl2Sh), 'BldFl2Sh', &
                 'Blade-flap mode-2 shape coefficients', ErrStat2 )
   ErrMsg2 = ' Error reading BldFl2Sh array from '//TRIM(BldFile)//'.'
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN


   ! BldEdgSh - Blade-edge mode shape coefficients.
   
CALL ReadAryLines ( UnIn, BldFile, BladeKInputFileData%BldEdgSh, SIZE(BladeKInputFileData%BldEdgSh), 'BldEdgSh', &
                  'Blade-edge mode shape coefficients', ErrStat2 )
   ErrMsg2 = ' Error reading BldEdgSh array from '//TRIM(BldFile)//'.'
   CALL CheckError( ErrStat2, ErrMsg2 )
   IF ( ErrStat >= AbortErrLev ) RETURN



!  -------------- END OF FILE --------------------------------------------
   
   ! Close the blade file.

CLOSE ( UnIn )
RETURN


CONTAINS
   !...............................................................................................................................
   SUBROUTINE CheckError(ErrID,Msg)
   ! This subroutine sets the error message and level
   !...............................................................................................................................
   
         ! Passed arguments
      INTEGER(IntKi), INTENT(IN) :: ErrID       ! The error identifier (ErrStat)
      CHARACTER(*),   INTENT(IN) :: Msg         ! The error message (ErrMsg)
      
      
      !............................................................................................................................
      ! Set error status/message; 
      !............................................................................................................................
   
      IF ( ErrID /= ErrID_None ) THEN
      
         ErrMsg = TRIM(ErrMsg)//NewLine//' '//TRIM(Msg)
         ErrStat = MAX(ErrStat, ErrID)
         
         !.........................................................................................................................
         ! Clean up if we're going to return on error: close file, deallocate local arrays
         !.........................................................................................................................
         IF ( ErrStat >= AbortErrLev ) THEN
            CLOSE( UnIn )
         END IF        
         
      END IF                  
            
         
   END SUBROUTINE CheckError

END SUBROUTINE ReadBladeFile
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE Alloc_BladeProperties( BladeKInputFileData, ErrStat, ErrMsg )
! This routine allocates arrays for the blade properties from the input file
!----------------------------------------------------------------------------------------------------------------------------------

   TYPE(BladeInputData),     INTENT(INOUT)  :: BladeKInputFileData                 ! Data for Blade K stored in the module's input file
   INTEGER(IntKi),           INTENT(OUT)    :: ErrStat        ! Error status
   CHARACTER(*),             INTENT(OUT)    :: ErrMsg         ! Err msg

   
   IF ( BladeKInputFileData%NBlInpSt < 1 )  THEN
      ErrStat = ErrID_Fatal
      ErrMsg = ' Error allocating arrays for blade input properties: NBlInpSt must be at least 1.' 
      RETURN
   END IF
   
   
      ! Allocate the arrays.

   CALL AllocAry  ( BladeKInputFileData%BlFract,  BladeKInputFileData%NBlInpSt, 'BlFract'  , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( BladeKInputFileData%AerCen,   BladeKInputFileData%NBlInpSt, 'AerCen'   , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( BladeKInputFileData%StrcTwst, BladeKInputFileData%NBlInpSt, 'StrcTwst' , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( BladeKInputFileData%BMassDen, BladeKInputFileData%NBlInpSt, 'BMassDen' , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( BladeKInputFileData%FlpStff,  BladeKInputFileData%NBlInpSt, 'FlpStff'  , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( BladeKInputFileData%EdgStff,  BladeKInputFileData%NBlInpSt, 'EdgStff'  , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( BladeKInputFileData%GJStff,   BladeKInputFileData%NBlInpSt, 'GJStff'   , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( BladeKInputFileData%EAStff,   BladeKInputFileData%NBlInpSt, 'EAStff'   , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( BladeKInputFileData%Alpha,    BladeKInputFileData%NBlInpSt, 'Alpha'    , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( BladeKInputFileData%FlpIner,  BladeKInputFileData%NBlInpSt, 'FlpIner'  , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( BladeKInputFileData%EdgIner,  BladeKInputFileData%NBlInpSt, 'EdgIner'  , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( BladeKInputFileData%PrecrvRef,BladeKInputFileData%NBlInpSt, 'PrecrvRef', ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( BladeKInputFileData%PreswpRef,BladeKInputFileData%NBlInpSt, 'PreswpRef', ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( BladeKInputFileData%FlpcgOf,  BladeKInputFileData%NBlInpSt, 'FlpcgOf'  , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( BladeKInputFileData%EdgcgOf,  BladeKInputFileData%NBlInpSt, 'EdgcgOf'  , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( BladeKInputFileData%FlpEAOf,  BladeKInputFileData%NBlInpSt, 'FlpEAOf'  , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( BladeKInputFileData%EdgEAOf,  BladeKInputFileData%NBlInpSt, 'EdgEAOf'  , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN

   
      ! BJJ: note that these used to be allocated 2:PolyOrd  :
   
   CALL AllocAry  ( BladeKInputFileData%BldFl1Sh,  PolyOrd-1, 'BldFl1Sh'  , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( BladeKInputFileData%BldFl2Sh,  PolyOrd-1, 'BldFl2Sh'  , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry  ( BladeKInputFileData%BldEdgSh,  PolyOrd-1, 'BldEdgSh'  , ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   
   
END SUBROUTINE Alloc_BladeProperties
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE SetBladeParams( p, BladeInData, SetAdmVals, ErrStat, ErrMsg )
! This takes the blade input file data and sets the corresponding blade parameters, performing linear interpolation of the
! input data to the specified blade mesh.
!----------------------------------------------------------------------------------------------------------------------------------

   TYPE(StrD_ParameterType), INTENT(INOUT)  :: p                                   ! The parameters of the structural dynamics module
   TYPE(BladeInputData),     INTENT(INOUT)  :: BladeInData(:)                      ! Program input data for all blades
   LOGICAL,                  INTENT(IN)     :: SetAdmVals                          ! Logical to determine if Adams inputs should be set 
   INTEGER(IntKi),           INTENT(OUT)    :: ErrStat                             ! Error status
   CHARACTER(*),             INTENT(OUT)    :: ErrMsg                              ! Error message
   
      ! Local variables:
   REAL(ReKi)                               :: x                                   ! Fractional location between two points in linear interpolation
   INTEGER(IntKi )                          :: K                                   ! Blade number
   INTEGER(IntKi )                          :: J                                   ! Index for the node arrays
   INTEGER(IntKi)                           :: InterpInd                           ! Index for the interpolation routine

   ErrStat = ErrID_None
   ErrMsg  = ''
   
!bjj: it would probably make sense to allocate all of these arrays here (shape as well as the interpolated/mesh arrays)
!
!
!      ! Allocate space for the arrays (BJJ check that this isn't done elsewhere...)
!ALLOCATE( p%BldEdgSh(2:PolyOrd,p%NumBl), p%BldFl1Sh(2:PolyOrd,p%NumBl), BldFl2Sh(2:PolyOrd,p%NumBl), STAT = ErrStat )
!IF ( ErrStat /= 0 ) THEN
!   ErrStat = ErrID_Fatal
!   ErrMsg  = ' Error allocating BldEdgSh, BldFl1Sh, and BldFl2Sh arrays.'
!   RETURN
!END IF   
   

   ! Array definitions:

   !    Input      Interp    Description
   !    -----      ------    -----------
   !    BlFract    RNodesNorm Fractional radius (0 at root, 1 at tip)
   !    AerCen     AeroCent   Aerodynamic center (0 at LE, 1 at TE)
   !    StrcTwst   ThetaS     Structural twist
   !    BMassDen   MassB      Lineal mass density
   !    FlpStff    StiffBF    Flapwise stiffness
   !    EdgStff    StiffBE    Edgewise stiffness
   !    GJStff     StiffBGJ   Blade torsional stiffness
   !    EAStff     StiffBEA   Blade extensional stiffness
   !    Alpha      BAlpha     Blade flap/twist coupling coefficient
   !    FlpIner    InerBFlp   Blade flap (about local structural yb-axis) mass inertia per unit length
   !    EdgIner    InerBEdg   Blade edge (about local structural xb-axis) mass inertia per unit length
   !    PrecrvRef  RefAxisxb  Blade offset for defining the reference axis from the pitch axis for precurved blades (along xb-axis)
   !    PreswpRef  RefAxisyb  Blade offset for defining the reference axis from the pitch axis for preswept  blades (along yb-axis)
   !    FlpcgOf    cgOffBFlp  Blade flap mass cg offset
   !    EdgcgOf    cgOffBEdg  Blade edge mass cg offset
   !    FlpEAOf    EAOffBFlp  Blade flap elastic axis offset
   !    EdgEAOf    EAOffBEdg  Blade edge elastic axis offset

   
      ! Perform a linear interpolation of the input data to map to the meshed data for simulation:
    
   DO K=1,p%NumBl      
      InterpInd = 1

      DO J=1,p%BldNodes
         
            ! Get the index into BlFract for all of the arrays, using the NWTC Subroutine Library
         p%AeroCent(K,J) = InterpStp( p%RNodesNorm(J), BladeInData(K)%BlFract, BladeInData(K)%AerCen, &
                                      InterpInd, BladeInData(K)%NBlInpSt )
      
            ! The remaining arrays will have the same x value for the linear interpolation, 
            ! so we'll do it manually (with a local subroutine) instead of calling the InterpStp routine again
         x = ( p%RNodesNorm(InterpInd)             - BladeInData(K)%BlFract(InterpInd) ) / &
             ( BladeInData(K)%BlFract(InterpInd+1) - BladeInData(K)%BlFract(InterpInd) )
       
         p%ThetaS  (K,J) = InterpAry( x, BladeInData(K)%StrcTwst, InterpInd )
         p%MassB   (K,J) = InterpAry( x, BladeInData(K)%BMassDen, InterpInd )
         p%StiffBF (K,J) = InterpAry( x, BladeInData(K)%FlpStff , InterpInd )
         p%StiffBE (K,J) = InterpAry( x, BladeInData(K)%EdgStff , InterpInd )
      
         IF ( SetAdmVals ) THEN
            p%StiffBGJ (K,J) = InterpAry( x, BladeInData(K)%GJStff   , InterpInd )
            p%StiffBEA (K,J) = InterpAry( x, BladeInData(K)%EAStff   , InterpInd )
            p%BAlpha   (K,J) = InterpAry( x, BladeInData(K)%Alpha    , InterpInd )
            p%InerBFlp (K,J) = InterpAry( x, BladeInData(K)%FlpIner  , InterpInd )
            p%InerBEdg (K,J) = InterpAry( x, BladeInData(K)%EdgIner  , InterpInd )
            p%RefAxisxb(K,J) = InterpAry( x, BladeInData(K)%PrecrvRef, InterpInd )
            p%RefAxisyb(K,J) = InterpAry( x, BladeInData(K)%PreswpRef, InterpInd )
            p%cgOffBFlp(K,J) = InterpAry( x, BladeInData(K)%FlpcgOf  , InterpInd )
            p%cgOffBEdg(K,J) = InterpAry( x, BladeInData(K)%EdgcgOf  , InterpInd )
            p%EAOffBFlp(K,J) = InterpAry( x, BladeInData(K)%FlpEAOf  , InterpInd )
            p%EAOffBEdg(K,J) = InterpAry( x, BladeInData(K)%EdgEAOf  , InterpInd )
         END IF
      
      
      END DO ! J (Blade nodes)
   
      IF ( SetAdmVals ) THEN
            ! Set the valus for the tip node
         p%RefAxisxb(K,p%TipNode) = BladeInData(K)%PrecrvRef( BladeInData(K)%NBlInpSt )
         p%RefAxisyb(K,p%TipNode) = BladeInData(K)%PreswpRef( BladeInData(K)%NBlInpSt )  
      END IF
   
      
         ! Set the blade damping and stiffness tuner
      p%BldFDamp(K,:) = BladeInData(K)%BldFlDmp
      p%FStTunr (K,:) = BladeInData(K)%FlStTunr

      
      
         ! Set the mode shape arrays
       !p%CalcBModes(K) = BladeInData(K)%CalcBMode
     
      p%BldEdgSh(:,K) = BladeInData(K)%BldEdgSh
      p%BldFl1Sh(:,K) = BladeInData(K)%BldFl1Sh
      p%BldFl2Sh(:,K) = BladeInData(K)%BldFl2Sh
      

   END DO ! ( Blades )

   
   p%ThetaS  = D2R*p%ThetaS
   p%CThetaS = COS(p%ThetaS)
   p%CThetaS = SIN(p%ThetaS)
   
 
   
   
   
   
RETURN
   ! bjj: also set mode shapes (I've changed the inputs...).

   
   
   

CONTAINS
   FUNCTION InterpAry( x, YAry, Ind )
      ! This subroutine is used to interpolate the arrays more efficiently (all arrays have the same X value)
      ! See InterpStpReal() for comparison. This assumes we already know Ind and that 
      ! x = ( XVal - XAry(Ind) )/( XAry(Ind+1) - XAry(Ind) )
      
      
      REAL(ReKi),      INTENT(IN) :: x                ! the relative distance between Ind and Ind+ 1
      REAL(ReKi),      INTENT(IN) :: YAry (:)         ! Array of Y values to be interpolated.
      INTEGER(IntKi) , INTENT(IN) :: Ind              ! the index into the array 
      
      REAL(ReKi)                  :: InterpAry        ! the value calculated in this function
      
      InterpAry = ( YAry(Ind+1) - YAry(Ind) ) * x  + YAry(Ind)
      
   END FUNCTION InterpAry
   
END SUBROUTINE SetBladeParams


END MODULE StructDyn
!**********************************************************************************************************************************

