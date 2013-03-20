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

  ! PRIVATE

   TYPE(ProgDesc), PARAMETER            :: SrvD_Ver = ProgDesc( 'ServoDyn', 'v1.00.00', '31-March-2013' )
!   INTEGER(IntKi), PARAMETER            :: MaxBl = 3
   LOGICAL, PARAMETER, PUBLIC           :: Cmpl4SFun  = .FALSE.                            ! Is the module being compiled as an S-Function for Simulink?
   LOGICAL, PARAMETER, PUBLIC           :: Cmpl4LV    = .FALSE.                            ! Is the module being compiled for Labview?


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

   TYPE(SrvD_InputFile)                           :: InputFileData  ! Data stored in the module's input file


      ! Initialize variables

   ErrStat = ErrID_None
   ErrMsg  = ""

      
      ! Initialize the NWTC Subroutine Library

   CALL NWTC_Init( )

      ! Display the module information

   CALL DispNVD( SrvD_Ver )

         
      !............................................................................................      
      ! Read the input file and validate the data
      ! (note p%NumBl and p%RootName must be set first!) 
      !............................................................................................      
   p%RootName = TRIM(InitInp%RootName)//'_'//TRIM(SrvD_Ver%Name) ! all of the output file names from this module will end with '_ModuleName'
   p%NumBl    = InitInp%NumBl         
      
   CALL SrvD_ReadInput( InitInp%InputFile, InputFileData, p%RootName, ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) THEN
      CALL WrScr( ErrMsg )
      RETURN
   END IF
      
   CALL ValidatePrimaryData( InputFileData, InitInp%NumBl, ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) THEN
      CALL WrScr( ErrMsg )
      RETURN
   END IF

      
      !............................................................................................
      ! Define parameters here:
      !............................................................................................
   CALL SrvD_SetParameters( InputFileData, p, ErrStat, ErrMsg )           
   IF ( ErrStat /= ErrID_None ) THEN
      CALL WrScr( ErrMsg )
      RETURN
   END IF
      
   p%DT  = Interval

      ! Set and verify BlPitchInit, which comes from InitInputData (not the inputfiledata)
   CALL AllocAry( p%BlPitchInit, p%NumBl, 'BlPitchInit', ErrStat, ErrMsg )
   p%BlPitchInit = InitInp%BlPitchInit

   IF ( ANY( p%BlPitchInit <= -pi ) .OR. ANY( p%BlPitchInit > pi ) )  THEN
      ErrStat = ErrID_Fatal
      ErrMsg = 'BlPitchInit must be in the range (-pi,pi] radians (i.e., (-180,180] degrees).'
      RETURN
   END IF
   
      !............................................................................................
      ! Define initial system states here:
      !............................................................................................

   x%DummyContState           = 0
   xd%DummyDiscState          = 0
   z%DummyConstrState         = 0
   

      !............................................................................................
      ! Define initial guess for the system inputs here:
      !............................................................................................

   CALL AllocAry( u%BlPitch, p%NumBl, 'BlPitch', ErrStat, ErrMsg )
   u%BlPitch = p%BlPitchInit


      !............................................................................................
      ! Define system output initializations (set up mesh) here:
      !............................................................................................
   CALL AllocAry( y%WriteOutput, p%NumBl, 'WriteOutput', ErrStat, ErrMsg )
   y%WriteOutput = 0
      
   CALL AllocAry( y%BlPitchCom, p%NumBl, 'BlPitchCom', ErrStat, ErrMsg )

      !............................................................................................
      ! Define initialization-routine output here:
      !............................................................................................
   ALLOCATE( InitOut%WriteOutputHdr(p%NumOuts), InitOut%WriteOutputUnt(p%NumOuts), STAT = ErrStat )
   IF ( ErrStat /= 0_IntKi ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = 'Error allocating output header and units arrays in SrvD_Init'
      RETURN
   END IF

   !InitOut%WriteOutputHdr = (/ 'Time   ', 'Column2' /)
   !InitOut%WriteOutputUnt = (/ '(s)',     '(-)'     /)
   InitOut%Ver = SrvD_Ver

   
      !............................................................................................
      ! Initialize other states here:
      !............................................................................................
   CALL AllocAry( OtherState%BlPitchI,  p%NumBl,   'BlPitchI',  ErrStat, ErrMsg )
   CALL AllocAry( OtherState%BegPitMan, p%NumBl,   'BegPitMan', ErrStat, ErrMsg )
   CALL AllocAry( OtherState%BlPitchFrct, p%NumBl, 'BlPitchFrct', ErrStat, ErrMsg )
      
   OtherState%BegPitMan  = HUGE(OtherState%BegPitMan)         ! Pitch maneuvers didn't actually start, yet (pick a number larger than TPitManS)
   OtherState%BegYawMan  = HUGE(OtherState%BegYawMan)         ! Yaw maneuver didn't actually start, yet (pick a number larger than TYawManS)
   
   OtherState%YawManRat = InputFileData%YawManRat  ! we change the sign of this variable later, so we'll store it as an other state instead of a parameter
   
      !............................................................................................
      ! If you want to choose your own rate instead of using what the glue code suggests, tell the glue code the rate at which
      !   this module must be called here:
      !............................................................................................

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
SUBROUTINE SrvD_UpdateStates( t, u, p, x, xd, z, OtherState, ErrStat, ErrMsg )
! Loose coupling routine for solving for constraint states, integrating continuous states, and updating discrete states
! Constraint states are solved for input t; Continuous and discrete states are updated for t + Interval
!..................................................................................................................................

      REAL(DbKi),                      INTENT(IN   ) :: t           ! Current simulation time in seconds
      TYPE(SrvD_InputType),            INTENT(IN   ) :: u           ! Inputs at t
      TYPE(SrvD_ParameterType),        INTENT(IN   ) :: p           ! Parameters
      TYPE(SrvD_ContinuousStateType),  INTENT(INOUT) :: x           ! Input: Continuous states at t;
                                                                    !   Output: Continuous states at t + Interval
      TYPE(SrvD_DiscreteStateType),    INTENT(INOUT) :: xd          ! Input: Discrete states at t;
                                                                    !   Output: Discrete states at t  + Interval
      TYPE(SrvD_ConstraintStateType),  INTENT(INOUT) :: z           ! Input: Initial guess of constraint states at t;
                                                                    !   Output: Constraint states at t
      TYPE(SrvD_OtherStateType),       INTENT(INOUT) :: OtherState  ! Other/optimization states
      INTEGER(IntKi),                  INTENT(  OUT) :: ErrStat     ! Error status of the operation
      CHARACTER(*),                    INTENT(  OUT) :: ErrMsg      ! Error message if ErrStat /= ErrID_None

         ! Local variables

      TYPE(SrvD_ContinuousStateType)                 :: dxdt        ! Continuous state derivatives at t
      TYPE(SrvD_ConstraintStateType)                 :: z_Residual  ! Residual of the constraint state equations (Z)

      INTEGER(IntKi)                                 :: ErrStat2    ! Error status of the operation (occurs after initial error)
      CHARACTER(LEN(ErrMsg))                         :: ErrMsg2     ! Error message if ErrStat2 /= ErrID_None

         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""



         ! Solve for the constraint states (z) here:

         ! Check if the z guess is correct and update z with a new guess.
         ! Iterate until the value is within a given tolerance.

      CALL SrvD_CalcConstrStateResidual( t, u, p, x, xd, z, OtherState, z_Residual, ErrStat, ErrMsg )
      IF ( ErrStat >= AbortErrLev ) THEN
         CALL SrvD_DestroyConstrState( z_Residual, ErrStat2, ErrMsg2)
         ErrMsg = TRIM(ErrMsg)//' '//TRIM(ErrMsg2)
         RETURN
      END IF

      ! DO WHILE ( z_Residual% > tolerance )
      !
      !  z =
      !
      !  CALL SrvD_CalcConstrStateResidual( t, u, p, x, xd, z, OtherState, z_Residual, ErrStat, ErrMsg )
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

      CALL SrvD_CalcContStateDeriv( t, u, p, x, xd, z, OtherState, dxdt, ErrStat, ErrMsg )
      IF ( ErrStat >= AbortErrLev ) THEN
         CALL SrvD_DestroyContState( dxdt, ErrStat2, ErrMsg2)
         ErrMsg = TRIM(ErrMsg)//' '//TRIM(ErrMsg2)
         RETURN
      END IF


         ! Update discrete states:
         !   Note that xd [discrete state] is changed in SrvD_UpdateDiscState(), so SrvD_CalcOutput(),
         !   SrvD_CalcContStateDeriv(), and SrvD_CalcConstrStates() must be called first (see above).

      CALL SrvD_UpdateDiscState(t, u, p, x, xd, z, OtherState, ErrStat, ErrMsg )
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
SUBROUTINE SrvD_CalcOutput( t, u, p, x, xd, z, OtherState, y, ErrStat, ErrMsg )
! Routine for computing outputs, used in both loose and tight coupling.
!..................................................................................................................................

      REAL(DbKi),                     INTENT(IN   )  :: t           ! Current simulation time in seconds
      TYPE(SrvD_InputType),           INTENT(IN   )  :: u           ! Inputs at t
      TYPE(SrvD_ParameterType),       INTENT(IN   )  :: p           ! Parameters
      TYPE(SrvD_ContinuousStateType), INTENT(IN   )  :: x           ! Continuous states at t
      TYPE(SrvD_DiscreteStateType),   INTENT(IN   )  :: xd          ! Discrete states at t
      TYPE(SrvD_ConstraintStateType), INTENT(IN   )  :: z           ! Constraint states at t
      TYPE(SrvD_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states
      TYPE(SrvD_OutputType),          INTENT(INOUT)  :: y           ! Outputs computed at t (Input only so that mesh con-
                                                                    !   nectivity information does not have to be recalculated)
      INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None


         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Compute outputs here:

      ! Yaw control
   CALL Yaw_CalcOutput( t, u, p, x, xd, z, OtherState, y, ErrStat, ErrMsg )      
         

END SUBROUTINE SrvD_CalcOutput
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE SrvD_CalcContStateDeriv( t, u, p, x, xd, z, OtherState, dxdt, ErrStat, ErrMsg )
! Tight coupling routine for computing derivatives of continuous states
!..................................................................................................................................

      REAL(DbKi),                     INTENT(IN   )  :: t           ! Current simulation time in seconds
      TYPE(SrvD_InputType),           INTENT(IN   )  :: u           ! Inputs at t
      TYPE(SrvD_ParameterType),       INTENT(IN   )  :: p           ! Parameters
      TYPE(SrvD_ContinuousStateType), INTENT(IN   )  :: x           ! Continuous states at t
      TYPE(SrvD_DiscreteStateType),   INTENT(IN   )  :: xd          ! Discrete states at t
      TYPE(SrvD_ConstraintStateType), INTENT(IN   )  :: z           ! Constraint states at t
      TYPE(SrvD_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states
      TYPE(SrvD_ContinuousStateType), INTENT(  OUT)  :: dxdt        ! Continuous state derivatives at t
      INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None


         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Compute the first time derivatives of the continuous states here:

      dxdt%DummyContState = 0


END SUBROUTINE SrvD_CalcContStateDeriv
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE SrvD_UpdateDiscState( t, u, p, x, xd, z, OtherState, ErrStat, ErrMsg )
! Tight coupling routine for updating discrete states
!..................................................................................................................................

      REAL(DbKi),                     INTENT(IN   )  :: t           ! Current simulation time in seconds
      TYPE(SrvD_InputType),           INTENT(IN   )  :: u           ! Inputs at t
      TYPE(SrvD_ParameterType),       INTENT(IN   )  :: p           ! Parameters
      TYPE(SrvD_ContinuousStateType), INTENT(IN   )  :: x           ! Continuous states at t
      TYPE(SrvD_DiscreteStateType),   INTENT(INOUT)  :: xd          ! Input: Discrete states at t;
                                                                    !   Output: Discrete states at t + Interval
      TYPE(SrvD_ConstraintStateType), INTENT(IN   )  :: z           ! Constraint states at t
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
SUBROUTINE SrvD_CalcConstrStateResidual( t, u, p, x, xd, z, OtherState, z_residual, ErrStat, ErrMsg )
! Tight coupling routine for solving for the residual of the constraint state equations
!..................................................................................................................................

      REAL(DbKi),                     INTENT(IN   )  :: t           ! Current simulation time in seconds
      TYPE(SrvD_InputType),           INTENT(IN   )  :: u           ! Inputs at t
      TYPE(SrvD_ParameterType),       INTENT(IN   )  :: p           ! Parameters
      TYPE(SrvD_ContinuousStateType), INTENT(IN   )  :: x           ! Continuous states at t
      TYPE(SrvD_DiscreteStateType),   INTENT(IN   )  :: xd          ! Discrete states at t
      TYPE(SrvD_ConstraintStateType), INTENT(IN   )  :: z           ! Constraint states at t (possibly a guess)
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
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE SrvD_ReadInput( InputFileName, InputFileData, OutFileRoot, ErrStat, ErrMsg )
! This subroutine reads the input file and stores all the data in the SrvD_InputFile structure.
! It does not perform data validation.
!..................................................................................................................................

      ! Passed variables

   CHARACTER(*), INTENT(IN)               :: InputFileName  ! Name of the input file
   CHARACTER(*), INTENT(IN)               :: OutFileRoot    ! The rootname of all the output files written by this routine.

   !BJJ MODIFIED HERE ONLY FOR TESTING:
!   TYPE(SrvD_InputFile),   INTENT(OUT)      :: InputFileData  ! Data stored in the module's input file
   TYPE(SrvD_InputFile),   INTENT(inOUT)      :: InputFileData  ! Data stored in the module's input file

   INTEGER(IntKi),       INTENT(OUT)      :: ErrStat        ! The error status code
   CHARACTER(*),         INTENT(OUT)      :: ErrMsg         ! The error message, if an error occurred

      ! local variables

   INTEGER(IntKi)                         :: UnEcho         ! Unit number for the echo file
   INTEGER(IntKi)                         :: ErrStat2       ! The error status code
   CHARACTER(LEN(ErrMsg))                 :: ErrMsg2        ! The error message, if an error occurred
   
      ! initialize values: 
   
   ErrStat = ErrID_None
   ErrMsg  = ''

   
      ! get the primary/platform input-file data
   
   CALL ReadPrimaryFile( InputFileName, InputFileData, OutFileRoot, UnEcho, ErrStat2, ErrMsg2 )
      CALL CheckError(ErrStat2,ErrMsg2)
      IF ( ErrStat >= AbortErrLev ) RETURN
      

      ! we may need to read additional files here (e.g., Bladed Interface)
   
      
      ! close any echo file that was opened
      
   IF ( UnEcho > 0 ) CLOSE( UnEcho )        

CONTAINS
   !...............................................................................................................................
   SUBROUTINE CheckError(ErrID,Msg)
   ! This subroutine sets the error message and level and cleans up if the error is >= AbortErrLev
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
         ! Clean up if we're going to return on error: close files, deallocate local arrays
         !.........................................................................................................................
         IF ( ErrStat >= AbortErrLev ) THEN
            IF ( UnEcho > 0 ) CLOSE( UnEcho )
         END IF

      END IF


   END SUBROUTINE CheckError     

END SUBROUTINE SrvD_ReadInput
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ReadPrimaryFile( InputFile, InputFileData, OutFileRoot, UnEc, ErrStat, ErrMsg )
! This routine reads in the primary ServoDyn input file and places the values it reads in the InputFileData structure.
!   It opens and prints to an echo file if requested.
!..................................................................................................................................


   IMPLICIT                        NONE

      ! Passed variables
   INTEGER(IntKi),     INTENT(OUT)     :: UnEc                                ! I/O unit for echo file. If > 0, file is open for writing.
   INTEGER(IntKi),     INTENT(OUT)     :: ErrStat                             ! Error status

   CHARACTER(*),       INTENT(IN)      :: InputFile                           ! Name of the file containing the primary input data
   CHARACTER(*),       INTENT(OUT)     :: ErrMsg                              ! Error message
   CHARACTER(*),       INTENT(IN)      :: OutFileRoot                         ! The rootname of the echo file, possibly opened in this routine

   TYPE(SrvD_InputFile), INTENT(INOUT) :: InputFileData                       ! All the data in the ServoDyn input file
   
      ! Local variables:
   INTEGER(IntKi)                :: I                                         ! loop counter
   INTEGER(IntKi)                :: NumOuts                                   ! Number of output channel names read from the file 
   INTEGER(IntKi)                :: UnIn                                      ! Unit number for reading file
     
   INTEGER(IntKi)                :: ErrStat2                                  ! Temporary Error status
   LOGICAL                       :: Echo                                      ! Determines if an echo file should be written
   CHARACTER(LEN(ErrMsg))        :: ErrMsg2                                   ! Temporary Error message
   CHARACTER(1024)               :: PriPath                                   ! Path name of the primary file
   CHARACTER(1024)               :: FTitle                                    ! "File Title": the 2nd line of the input file, which contains a description of its contents

   
      ! Initialize some variables:
   UnEc = -1
   Echo = .FALSE.
   
      ! Get an available unit number for the file.

   CALL GetNewUnit( UnIn, ErrStat, ErrMsg )
   IF ( ErrStat >= AbortErrLev ) RETURN


      ! Open the Primary input file.

   CALL OpenFInpFile ( UnIn, InputFile, ErrStat2, ErrMsg2 )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
                  
      
   ! Read the lines up/including to the "Echo" simulation control variable
   ! If echo is FALSE, don't write these lines to the echo file. 
   ! If Echo is TRUE, rewind and write on the second try.
   
   I = 1 !set the number of times we've read the file
   DO 
   !-------------------------- HEADER ---------------------------------------------
   
      CALL ReadCom( UnIn, InputFile, 'File header: Module Version (line 1)', ErrStat2, ErrMsg2, UnEc )
         CALL CheckError( ErrStat2, ErrMsg2 )
         IF ( ErrStat >= AbortErrLev ) RETURN
   
      CALL ReadStr( UnIn, InputFile, FTitle, 'FTitle', 'File Header: File Description (line 2)', ErrStat2, ErrMsg2, UnEc )
         CALL CheckError( ErrStat2, ErrMsg2 )
         IF ( ErrStat >= AbortErrLev ) RETURN
   
   
   !---------------------- SIMULATION CONTROL --------------------------------------
   
      CALL ReadCom( UnIn, InputFile, 'Section Header: Simulation Control', ErrStat2, ErrMsg2, UnEc )
         CALL CheckError( ErrStat2, ErrMsg2 )
         IF ( ErrStat >= AbortErrLev ) RETURN
   
         ! Echo - Echo input to "<RootName>.ech".
   
      CALL ReadVar( UnIn, InputFile, Echo, 'Echo',   'Echo switch', ErrStat2, ErrMsg2, UnEc )
         CALL CheckError( ErrStat2, ErrMsg2 )
         IF ( ErrStat >= AbortErrLev ) RETURN
   
   
      IF (.NOT. Echo .OR. I > 1) EXIT !exit this loop
   
         ! Otherwise, open the echo file, then rewind the input file and echo everything we've read
      
      I = I + 1         ! make sure we do this only once (increment counter that says how many times we've read this file)
   
      CALL OpenEcho ( UnEc, TRIM(OutFileRoot)//'.ech', ErrStat2, ErrMsg2, SrvD_Ver )
         CALL CheckError( ErrStat2, ErrMsg2 )
         IF ( ErrStat >= AbortErrLev ) RETURN
   
      CALL WrScr( ' Heading of the '//TRIM(SrvD_Ver%Name)//' input file: '//TRIM( FTitle ) )      
      IF ( UnEc > 0 )  WRITE (UnEc,'(//,A,/)')  'Data from '//TRIM(SrvD_Ver%Name)//' primary input file "'//TRIM( InputFile )//'":'
   
      REWIND( UnIn, IOSTAT=ErrStat2 )   
         CALL CheckError( ErrID_Fatal, 'Error rewinding file "'//TRIM(InputFile)//'".' )
         IF ( ErrStat >= AbortErrLev ) RETURN
      
   END DO    
                                      
      ! DT - Communication interval for controllers (s):
   CALL ReadVar( UnIn, InputFile, InputFileData%DT, "DT", "Communication interval for controllers (s)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   
   !---------------------- PITCH CONTROL -------------------------------------------
   CALL ReadCom( UnIn, InputFile, 'Section Header: Pitch Control', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
      ! PCMode - Pitch control mode (-):
   CALL ReadVar( UnIn, InputFile, InputFileData%PCMode, "PCMode", "Pitch control mode (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! TPCOn - Time to enable active pitch control [unused when PCMode=0] (s):
   CALL ReadVar( UnIn, InputFile, InputFileData%TPCOn, "TPCOn", "Time to enable active pitch control (s)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! TPitManS - Time to start override pitch maneuver for blade (K) and end standard pitch control (s):
   CALL ReadAryLines( UnIn, InputFile, InputFileData%TPitManS, SIZE(InputFileData%TPitManS), "TPitManS", &
        "Time to start override pitch maneuver for blade K and end standard pitch control (s)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! TPitManE - Time at which override pitch maneuver for blade (K) reaches final pitch (s):
   CALL ReadAryLines( UnIn, InputFile, InputFileData%TPitManE, SIZE(InputFileData%TPitManE), "TPitManE", "Time at which override pitch maneuver for blade K reaches final pitch (s)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! BlPitchF - Blade (K) final pitch for pitch maneuvers (deg) (read from file in degrees and converted to radians here):
   CALL ReadAryLines( UnIn, InputFile, InputFileData%BlPitchF, SIZE(InputFileData%BlPitchF), "BlPitchF", "Blade K final pitch for pitch maneuvers (deg)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   InputFileData%BlPitchF = InputFileData%BlPitchF*D2R
   
   !---------------------- GENERATOR AND TORQUE CONTROL ----------------------------
   CALL ReadCom( UnIn, InputFile, 'Section Header: Generator and Torque Control', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
      ! VSContrl - Variable-speed control mode {0: none, 1: simple VS, 2: user-defined from routine UserVSCont, 3: user-defined from Simulink/Labview} (-):
   CALL ReadVar( UnIn, InputFile, InputFileData%VSContrl, "VSContrl", "Variable-speed control mode (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! GenModel - Generator model {1: simple, 2: Thevenin, 3: user-defined from routine UserGen} [used only when VSContrl=0] (-):
   CALL ReadVar( UnIn, InputFile, InputFileData%GenModel, "GenModel", "Generator model {1: simple, 2: Thevenin, 3: user-defined from routine UserGen} [used only when VSContrl=0] (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! GenEff - Generator efficiency [ignored by the Thevenin and user-defined generator models] (%) (read in percent and converted to a fraction here):
   CALL ReadVar( UnIn, InputFile, InputFileData%GenEff, "GenEff", "Generator efficiency [ignored by the Thevenin and user-defined generator models] (%)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   InputFileData%GenEff = InputFileData%GenEff*0.01      

      ! GenTiStr - Method to start the generator {T: timed using TimGenOn, F: generator speed using SpdGenOn} (flag):
   CALL ReadVar( UnIn, InputFile, InputFileData%GenTiStr, "GenTiStr", "Method to start the generator {T: timed using TimGenOn, F: generator speed using SpdGenOn} (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! GenTiStp - Method to stop the generator {T: timed using TimGenOf, F: when generator power = 0} (flag):
   CALL ReadVar( UnIn, InputFile, InputFileData%GenTiStp, "GenTiStp", "Method to stop the generator {T: timed using TimGenOf, F: when generator power = 0} (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! SpdGenOn - Generator speed to turn on the generator for a startup (HSS speed) [used only when GenTiStr=False] (rpm) (read in rpm and converted to rad/sec here):
   CALL ReadVar( UnIn, InputFile, InputFileData%SpdGenOn, "SpdGenOn", "Generator speed to turn on the generator for a startup (HSS speed) [used only when GenTiStr=False] (rpm)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   InputFileData%SpdGenOn = InputFileData%SpdGenOn*RPM2RPS

      ! TimGenOn - Time to turn on the generator for a startup [used only when GenTiStr=True] (s):
   CALL ReadVar( UnIn, InputFile, InputFileData%TimGenOn, "TimGenOn", "Time to turn on the generator for a startup [used only when GenTiStr=True] (s)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! TimGenOf - Time to turn off the generator [used only when GenTiStp=True] (s):
   CALL ReadVar( UnIn, InputFile, InputFileData%TimGenOf, "TimGenOf", "Time to turn off the generator [used only when GenTiStp=True] (s)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
   !---------------------- SIMPLE VARIABLE-SPEED TORQUE CONTROL --------------------
   CALL ReadCom( UnIn, InputFile, 'Section Header: Simple Variable-Speed Torque Control', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
      ! VS_RtGnSp - Rated generator speed for simple variable-speed generator control (HSS side) [used only when VSContrl=1] (rpm) (read in rpm and converted to rad/sec here):
   CALL ReadVar( UnIn, InputFile, InputFileData%VS_RtGnSp, "VS_RtGnSp", "Rated generator speed for simple variable-speed generator control (HSS side) [used only when VSContrl=1] (rpm)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   InputFileData%VS_RtGnSp = InputFileData%VS_RtGnSp*RPM2RPS
      
      ! VS_RtTq - Rated generator torque/constant generator torque in Region 3 for simple variable-speed generator control (HSS side) [used only when VSContrl=1] (N-m):
   CALL ReadVar( UnIn, InputFile, InputFileData%VS_RtTq, "VS_RtTq", "Rated generator torque/constant generator torque in Region 3 for simple variable-speed generator control (HSS side) [used only when VSContrl=1] (N-m)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! VS_Rgn2K - Generator torque constant in Region 2 for simple variable-speed generator control (HSS side) [used only when VSContrl=1] (N-m/rpm^2) (read in N-m/rpm^2 and converted to N-m/(rad/s)^2 here:
   CALL ReadVar( UnIn, InputFile, InputFileData%VS_Rgn2K, "VS_Rgn2K", "Generator torque constant in Region 2 for simple variable-speed generator control (HSS side) [used only when VSContrl=1] (N-m/rpm^2)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   InputFileData%VS_Rgn2K = InputFileData%VS_Rgn2K/( RPM2RPS**2 )

      ! VS_SlPc - Rated generator slip percentage in Region 2 1/2 for simple variable-speed generator control [used only when VSContrl=1] (%) (read in percent and converted to a fraction here):
   CALL ReadVar( UnIn, InputFile, InputFileData%VS_SlPc, "VS_SlPc", "Rated generator slip percentage in Region 2 1/2 for simple variable-speed generator control [used only when VSContrl=1] (%)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN     
   InputFileData%VS_SlPc = InputFileData%VS_SlPc*.01
   
   !---------------------- SIMPLE INDUCTION GENERATOR ------------------------------
   CALL ReadCom( UnIn, InputFile, 'Section Header: Simple Induction Generator', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
      ! SIG_SlPc - Rated generator slip percentage [used only when VSContrl=0 and GenModel=1] (%) (read in percent and converted to a fraction here):
   CALL ReadVar( UnIn, InputFile, InputFileData%SIG_SlPc, "SIG_SlPc", "Rated generator slip percentage [used only when VSContrl=0 and GenModel=1] (%)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   InputFileData%SIG_SlPc = InputFileData%SIG_SlPc*.01

      ! SIG_SySp - Synchronous (zero-torque) generator speed [used only when VSContrl=0 and GenModel=1] (rpm) (read in rpm and convert to rad/sec here):
   CALL ReadVar( UnIn, InputFile, InputFileData%SIG_SySp, "SIG_SySp", "Synchronous (zero-torque) generator speed [used only when VSContrl=0 and GenModel=1] (rpm)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   InputFileData%SIG_SySp = InputFileData%SIG_SySp*RPM2RPS  
      

      ! SIG_RtTq - Rated torque [used only when VSContrl=0 and GenModel=1] (N-m):
   CALL ReadVar( UnIn, InputFile, InputFileData%SIG_RtTq, "SIG_RtTq", "Rated torque [used only when VSContrl=0 and GenModel=1] (N-m)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! SIG_PORt - Pull-out ratio (Tpullout/Trated) [used only when VSContrl=0 and GenModel=1] (-):
   CALL ReadVar( UnIn, InputFile, InputFileData%SIG_PORt, "SIG_PORt", "Pull-out ratio (Tpullout/Trated) [used only when VSContrl=0 and GenModel=1] (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
   !---------------------- THEVENIN-EQUIVALENT INDUCTION GENERATOR -----------------
   CALL ReadCom( UnIn, InputFile, 'Section Header: Thevenin-Equivalent Induction Generator', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
      ! TEC_Freq - Line frequency [50 or 60] [used only when VSContrl=0 and GenModel=2] (Hz):
   CALL ReadVar( UnIn, InputFile, InputFileData%TEC_Freq, "TEC_Freq", "Line frequency [50 or 60] [used only when VSContrl=0 and GenModel=2] (Hz)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! TEC_NPol - Number of poles [even integer > 0] [used only when VSContrl=0 and GenModel=2] (-):
   CALL ReadVar( UnIn, InputFile, InputFileData%TEC_NPol, "TEC_NPol", "Number of poles [even integer > 0] [used only when VSContrl=0 and GenModel=2] (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! TEC_SRes - Stator resistance [used only when VSContrl=0 and GenModel=2] (ohms):
   CALL ReadVar( UnIn, InputFile, InputFileData%TEC_SRes, "TEC_SRes", "Stator resistance [used only when VSContrl=0 and GenModel=2] (ohms)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! TEC_RRes - Rotor resistance [used only when VSContrl=0 and GenModel=2] (ohms):
   CALL ReadVar( UnIn, InputFile, InputFileData%TEC_RRes, "TEC_RRes", "Rotor resistance [used only when VSContrl=0 and GenModel=2] (ohms)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! TEC_VLL - Line-to-line RMS voltage [used only when VSContrl=0 and GenModel=2] (volts):
   CALL ReadVar( UnIn, InputFile, InputFileData%TEC_VLL, "TEC_VLL", "Line-to-line RMS voltage [used only when VSContrl=0 and GenModel=2] (volts)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! TEC_SLR - Stator leakage reactance [used only when VSContrl=0 and GenModel=2] (ohms):
   CALL ReadVar( UnIn, InputFile, InputFileData%TEC_SLR, "TEC_SLR", "Stator leakage reactance [used only when VSContrl=0 and GenModel=2] (ohms)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! TEC_RLR - Rotor leakage reactance [used only when VSContrl=0 and GenModel=2] (ohms):
   CALL ReadVar( UnIn, InputFile, InputFileData%TEC_RLR, "TEC_RLR", "Rotor leakage reactance [used only when VSContrl=0 and GenModel=2] (ohms)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! TEC_MR - Magnetizing reactance [used only when VSContrl=0 and GenModel=2] (ohms):
   CALL ReadVar( UnIn, InputFile, InputFileData%TEC_MR, "TEC_MR", "Magnetizing reactance [used only when VSContrl=0 and GenModel=2] (ohms)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
   !---------------------- HIGH-SPEED SHAFT BRAKE ----------------------------------
   CALL ReadCom( UnIn, InputFile, 'Section Header: High-Speed Shaft Brake', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
      ! HSSBrMode - HSS brake model {1: simple, 2: user-defined from routine UserHSSBr, 3: user-defined from Labview} (-):
   CALL ReadVar( UnIn, InputFile, InputFileData%HSSBrMode, "HSSBrMode", "HSS brake model {1: simple, 2: user-defined from routine UserHSSBr, 3: user-defined from Labview} (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! THSSBrDp - Time to initiate deployment of the HSS brake (s):
   CALL ReadVar( UnIn, InputFile, InputFileData%THSSBrDp, "THSSBrDp", "Time to initiate deployment of the HSS brake (s)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! HSSBrDT - Time for HSS-brake to reach full deployment once initiated [used only when HSSBrMode=1] (sec):
   CALL ReadVar( UnIn, InputFile, InputFileData%HSSBrDT, "HSSBrDT", "Time for HSS-brake to reach full deployment once initiated [used only when HSSBrMode=1] (sec)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! HSSBrTqF - Fully deployed HSS-brake torque (N-m):
   CALL ReadVar( UnIn, InputFile, InputFileData%HSSBrTqF, "HSSBrTqF", "Fully deployed HSS-brake torque (N-m)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
   !---------------------- YAW CONTROL ---------------------------------------------
   CALL ReadCom( UnIn, InputFile, 'Section Header: Yaw Control', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
      ! YCMode - Yaw control mode {0: none, 1: simple, 2: user-defined from routine UserYawCont, 3: user-defined from Simulink/Labview} (-):
   CALL ReadVar( UnIn, InputFile, InputFileData%YCMode, "YCMode", "Yaw control mode (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! TYCOn - Time to enable active yaw control [unused when YCMode=0] (s):
   CALL ReadVar( UnIn, InputFile, InputFileData%TYCOn, "TYCOn", "Time to enable active yaw control [unused when YCMode=0] (s)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! YawNeut - Neutral yaw position--yaw spring force is zero at this yaw (deg) (read from file in degrees and converted to radians here):
   CALL ReadVar( UnIn, InputFile, InputFileData%YawNeut, "YawNeut", "Neutral yaw position--yaw spring force is zero at this yaw (deg)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   InputFileData%YawNeut = InputFileData%YawNeut*D2R

      ! YawSpr - Nacelle-yaw spring constant (N-m/rad):
   CALL ReadVar( UnIn, InputFile, InputFileData%YawSpr, "YawSpr", "Nacelle-yaw spring constant (N-m/rad)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! YawDamp - Nacelle-yaw constant (N-m/(rad/s)):
   CALL ReadVar( UnIn, InputFile, InputFileData%YawDamp, "YawDamp", "Nacelle-yaw constant (N-m/(rad/s))", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN      
      
      ! TYawManS - Time to start override yaw maneuver and end standard yaw control (s):
   CALL ReadVar( UnIn, InputFile, InputFileData%TYawManS, "TYawManS", "Time to start override yaw maneuver and end standard yaw control (s)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! YawManRat - Yaw maneuver rate (in absolute value) (deg/s) (read in degrees/second and converted to radians/second here):
   CALL ReadVar( UnIn, InputFile, InputFileData%YawManRat, "YawManRat", "Yaw maneuver rate (in absolute value) (deg/s)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   InputFileData%YawManRat = InputFileData%YawManRat*D2R

      ! NacYawF - Final yaw angle for override yaw maneuvers (deg) (read from file in degrees and converted to radians here):
   CALL ReadVar( UnIn, InputFile, InputFileData%NacYawF, "NacYawF", "Final yaw angle for override yaw maneuvers (deg)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   InputFileData%NacYawF = InputFileData%NacYawF*D2R
      
   !---------------------- OUTPUT --------------------------------------------------         
   CALL ReadCom( UnIn, InputFile, 'Section Header: Output', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! SumPrint - Print summary data to <RootName>.sum (flag):
   CALL ReadVar( UnIn, InputFile, InputFileData%SumPrint, "SumPrint", "Print summary data to <RootName>.sum (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! OutFile - Switch to determine where output will be placed: (1: in module output file only; 2: in glue code output file only; 3: both) (-):
   CALL ReadVar( UnIn, InputFile, InputFileData%OutFile, "OutFile", "Switch to determine where output will be placed: {1: in module output file only; 2: in glue code output file only; 3: both} (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

   !   ! OutFileFmt - Format for module tabular (time-marching) output: (1: text file [<RootName>.out], 2: binary file [<RootName>.outb], 3: both):
   !CALL ReadVar( UnIn, InputFile, InputFileData%OutFileFmt, "OutFileFmt", "Format for module tabular (time-marching) output: (1: text file [<RootName>.out], 2: binary file [<RootName>.outb], 3: both)", ErrStat2, ErrMsg2, UnEc)
   !   CALL CheckError( ErrStat2, ErrMsg2 )
   !   IF ( ErrStat >= AbortErrLev ) RETURN      
      
      ! TabDelim - Flag to cause tab-delimited text output (delimited by space otherwise) (flag):
   CALL ReadVar( UnIn, InputFile, InputFileData%TabDelim, "TabDelim", "Flag to cause tab-delimited text output (delimited by space otherwise) (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! OutFmt - Format used for module's text tabult output (except time); resulting field should be 10 characters (-):
   CALL ReadVar( UnIn, InputFile, InputFileData%OutFmt, "OutFmt", "Format used for module's text tabular output (except time); resulting field should be 10 characters (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! Tstart - Time to start module's tabular output (seconds):
   CALL ReadVar( UnIn, InputFile, InputFileData%Tstart, "Tstart", "Time to start module's tabular output (seconds)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   !
   !   ! DecFact - Decimation factor for module's tabular output (1=output every step) (-):
   !CALL ReadVar( UnIn, InputFile, InputFileData%DecFact, "DecFact", "Decimation factor for module's tabular output (1=output every step) (-)", ErrStat2, ErrMsg2, UnEc)
   !   CALL CheckError( ErrStat2, ErrMsg2 )
   !   IF ( ErrStat >= AbortErrLev ) RETURN

   !---------------------- OUTLIST  --------------------------------------------
      CALL ReadCom( UnIn, InputFile, 'Section Header: OutList', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! OutList - List of user-requested output channels (-):
   CALL ReadOutputList ( UnIn, InputFile, InputFileData%OutList, InputFileData%NumOuts, 'OutList', "List of user-requested output channels", ErrStat2, ErrMsg2, UnEc  )     ! Routine in NWTC Subroutine Library
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN     
      
   !---------------------- END OF FILE -----------------------------------------
      
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

         ErrMsg = TRIM(ErrMsg)//NewLine//' Error in ServoDyn ReadPrimaryFile: '//TRIM(Msg)
         ErrStat = MAX(ErrStat, ErrID)

         !.........................................................................................................................
         ! Clean up if we're going to return on error: close file, deallocate local arrays
         !.........................................................................................................................
         IF ( ErrStat >= AbortErrLev ) THEN
            CLOSE( UnIn )
!            IF ( UnEc > 0 ) CLOSE ( UnEc )
         END IF

      END IF


   END SUBROUTINE CheckError
   !...............................................................................................................................
END SUBROUTINE ReadPrimaryFile      
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE SetPrimaryParameters( p, InputFileData, ErrStat, ErrMsg  )
! This takes the primary input file data and sets the corresponding parameters.
!..................................................................................................................................

   IMPLICIT                        NONE


      ! Passed variables

   TYPE(SrvD_ParameterType), INTENT(INOUT)  :: p                            ! Module's parameters
   TYPE(SrvD_InputFile),     INTENT(IN)     :: InputFileData                ! Data stored in the module's input file
   INTEGER(IntKi),           INTENT(OUT)    :: ErrStat                      ! Error status
   CHARACTER(*),             INTENT(OUT)    :: ErrMsg                       ! Error message


      ! Initialize error data
   ErrStat = ErrID_None
   ErrMsg  = ''
   
   
      ! Direct copy of InputFileData to parameters
      
      
      
!ALLOCATE ( p%TTpBrDp(p%NumBl) , STAT=Sttus )
!ALLOCATE ( p%TBDepISp(p%NumBl) , STAT=Sttus )

!ALLOCATE ( p%BlPitch(p%NumBl) , STAT=Sttus )
!ALLOCATE ( () , STAT=Sttus )
!
   CALL AllocAry( p%BlPitchF, p%NumBl, 'BlPitchF', ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   p%BlPitchF = InputFileData%BlPitchF(1:p%NumBl)
   
   CALL AllocAry( p%TPitManS, p%NumBl, 'TPitManS', ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   p%TPitManS = InputFileData%TPitManS(1:p%NumBl)
   
   CALL AllocAry( p%TPitManE, p%NumBl, 'TPitManE', ErrStat, ErrMsg )
   IF ( ErrStat /= ErrID_None ) RETURN
   p%TPitManE = InputFileData%TPitManE(1:p%NumBl)

   IF ( InputFileData%TabDelim ) THEN
      p%Delim = TAB
   ELSE
      p%Delim = ' '
   END IF 


END SUBROUTINE SetPrimaryParameters
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ValidatePrimaryData( InputFileData, NumBl, ErrStat, ErrMsg )
! This routine validates the inputs from the primary input file.
!..................................................................................................................................
      
      ! Passed variables:

   TYPE(SrvD_InputFile),     INTENT(IN)     :: InputFileData                       ! All the data in the ServoDyn input file
   INTEGER(IntKi),           INTENT(IN)     :: NumBl                               ! Number of blades
   INTEGER(IntKi),           INTENT(OUT)    :: ErrStat                             ! Error status
   CHARACTER(*),             INTENT(OUT)    :: ErrMsg                              ! Error message

   
      ! local variables
   INTEGER(IntKi)                           :: K                                   ! Blade number
   
      ! Some special checks based on whether inputs will come from external source (e.g., Simulink, LabVIEW)
   IF ( .NOT. Cmpl4SFun .AND. .NOT. Cmpl4LV ) THEN
      IF ( InputFileData%YCMode == 2_IntKi )  THEN
         CALL SetErrors( ErrID_Fatal, 'YCMode can only equal 2 when ServoDyn is interfaced with Simulink or Labview.'// &
                   '  Set YCMode to 0 or 1 or interface ServoDyn with Simulink or Labview.' )          
      END IF
      IF ( InputFileData%PCMode == 2_IntKi )  THEN
         CALL SetErrors( ErrID_Fatal, 'PCMode can only equal 2 when ServoDyn is interfaced with Simulink or Labview.'// &
                   '  Set PCMode to 0 or 1 or interface ServoDyn with Simulink or Labview.' )          
      END IF
      
      IF ( InputFileData%VSContrl == 3 )  THEN
         CALL SetErrors( ErrID_Fatal, 'VSContrl can only equal 3 when ServoDyn is interfaced with Simulink or Labview.'// &
                '  Set VSContrl to 0, 1, or 2 or interface ServoDyn with Simulink or Labview.' )
      END IF
         
   ELSE
      
      IF ( InputFileData%YCMode == 2_IntKi .AND. .NOT. EqualRealNos( InputFileData%TYCOn, 0.0_DbKi ) )  THEN
         CALL SetErrors( ErrID_Fatal, 'Yaw control must be enabled at time zero when implemented in Simulink or Labview.'//&
                '  Set TYCon to 0.0, set YCMode to 0 or 1, or use the standard version of ServoDyn.' )
      END IF
      
      IF ( InputFileData%PCMode == 2_IntKi .AND. .NOT. EqualRealNos( InputFileData%TPCOn, 0.0_DbKi ) )  THEN
         CALL SetErrors( ErrID_Fatal, 'Pitch control must be enabled at time zero when implemented in Simulink or Labview.'//&
                '  Set TPCon to 0.0, set PCMode to 0 or 1, or use the standard version of ServoDyn.' )
      END IF           
      
      IF ( InputFileData%VSContrl == 3_IntKi  ) THEN  !we don't know TMax anymore...
         
         IF ( .NOT. InputFileData%GenTiStr .OR. .NOT. EqualRealNos( InputFileData%TimGenOn, 0.0_DbKi ) )  THEN
            CALL SetErrors( ErrID_Fatal, 'Variable-speed, generator torque control must be enabled at time zero when '//&
               'implemented in Simulink or Labview. Set GenTiStr to True and TimGenOn to 0.0, set VSContrl to 0, 1, or 2,'//&
               ' or use the standard version of ServoDyn.' )
         END IF
         
!         IF ( .NOT. InputFileData%GenTiStp .OR. InputFileData%TimGenOf <= TMax ) THEN   !BJJ SET p%TimGenOf = HUGE() when Cmpl4SFun .OR. Cmpl4LV
         IF ( .NOT. InputFileData%GenTiStp ) THEN
            CALL SetErrors( ErrID_Fatal, 'Variable-speed, generator torque control must not be disabled during simulation'//&
                ' when implemented in Simulink or Labview. Set GenTiStp to True and TimGenOf > TMax, '//                    &
                ' set VSContrl to 0, 1, or 2, or use the standard version of ServoDyn.'   )            
         END IF         
         
      END IF         
           
      !IF ( Cmpl4SFun .AND. ( InputFileData%THSSBrDp <= TMax ) )  THEN  !BJJ SET p%THSSBrDp = HUGE() when Cmpl4SFun
      !   CALL SetErrors( ErrID_Fatal, 'A high-speed shaft brake shutdown event can''t be initiated when ServoDyn is '// &
      !                'interfaced  with Simulink. Set THSSBrDp > TMax or use the standard version of ServoDyn.'        )
      !ENDIF
      !
   END IF
   
   IF ( .NOT. Cmpl4LV .AND. InputFileData%HSSBrMode == 3_IntKi )  THEN
      CALL SetErrors( ErrID_Fatal, 'HSSBrMode can be 3 only when when implemented in Labview.' )
   ENDIF      


      ! checks for yaw control:
   IF ( ( InputFileData%YCMode < 0_IntKi ) .OR. ( InputFileData%YCMode > 2_IntKi ) )  THEN
      CALL SetErrors( ErrID_Fatal, 'YCMode must be 0, 1, or 2.' )
   ENDIF

   IF ( InputFileData%TYCOn < 0.0_DbKi )  THEN
      CALL SetErrors( ErrID_Fatal, 'TYCOn must not be negative.' )
   ENDIF
   
   
      ! checks for pitch control:      
   IF ( ( InputFileData%PCMode < 0_IntKi ) .OR. ( InputFileData%PCMode > 2_IntKi ) )  THEN
      CALL SetErrors( ErrID_Fatal, 'PCMode must be 0, 1, or 2.' )
   ENDIF

   IF ( InputFileData%TPCOn < 0.0_DbKi )  THEN
      CALL SetErrors( ErrID_Fatal, 'TPCOn must not be negative.' )
   ENDIF

   IF ( NumBl > SIZE(InputFileData%TPitManS,1) ) CALL SetErrors( ErrID_Fatal, 'Number of blades exceeds input values.')
   
   DO K=1,MIN(NumBl,SIZE(InputFileData%TPitManS))
      !IF ( InputFileData%TTpBrDp(K)  < 0.0_DbKi ) &
      !   CALL SetErrors( ErrID_Fatal, 'TTpBrDp(' //TRIM( Num2LStr( K ) )//') must not be negative.' )
      !IF ( InputFileData%TBDepISp(K) < 0.0_DbKi ) &
      !   CALL SetErrors( ErrID_Fatal, 'TBDepISp('//TRIM( Num2LStr( K ) )//') must not be negative.' )
      IF ( InputFileData%TPitManS(K) < 0.0_DbKi ) &
         CALL SetErrors( ErrID_Fatal, 'TPitManS('//TRIM( Num2LStr( K ) )//') must not be negative.' )   
      IF ( InputFileData%TPitManE(K) < InputFileData%TPitManS(K) ) CALL SetErrors( ErrID_Fatal, &
                          'TPitManE('//TRIM( Num2LStr(K) )//') must not be less than TPitManS('//TRIM( Num2LStr(K) )//').' )
   ENDDO ! K   
   
!??? IF ( ANY( p%BlPitchInit <= -pi ) .OR. ANY( p%BlPitchInit > pi ) )  THEN
!      CALL SetErrors( ErrID_Fatal, 'BlPitchInit('//TRIM( Num2LStr( K ) )//') must be in the range (-pi,pi] radians (i.e., (-180,180] degrees).' )
!   
   
      ! checks for generator and torque control:           
   IF ( ( InputFileData%VSContrl < 0_IntKi ) .OR. ( InputFileData%VSContrl > 3_IntKi ) )  THEN
      CALL SetErrors( ErrID_Fatal, 'VSContrl must be either 0, 1, 2, or 3.' )
   ENDIF
   
   IF ( InputFileData%SpdGenOn < 0.0_ReKi ) CALL SetErrors( ErrID_Fatal, 'SpdGenOn must not be negative.' )
   IF ( InputFileData%TimGenOn < 0.0_DbKi ) CALL SetErrors( ErrID_Fatal, 'TimGenOn must not be negative.' )
   IF ( InputFileData%TimGenOf < 0.0_DbKi ) CALL SetErrors( ErrID_Fatal, 'TimGenOf must not be negative.' )
   IF ( InputFileData%GenEff   < 0.0_ReKi  .OR.  InputFileData%GenEff > 1.0_ReKi )  THEN
      CALL SetErrors( ErrID_Fatal, 'GenEff must be in the range [0, 1] (i.e., [0, 100] percent)' )
   END IF
   
   
      ! checks for variable-speed torque control:           
   IF ( InputFileData%VSContrl == 1_IntKi ) THEN
      IF ( InputFileData%VS_RtGnSp <= 0.0_ReKi )  CALL SetErrors( ErrID_Fatal, 'VS_RtGnSp must be greater than zero.' )
      IF ( InputFileData%VS_RtTq   < 0.0_ReKi  )  CALL SetErrors( ErrID_Fatal, 'VS_RtTq must not be negative.' )
      IF ( InputFileData%VS_Rgn2K  < 0.0_ReKi  )  CALL SetErrors( ErrID_Fatal, 'VS_Rgn2K must not be negative.' )
      IF ( InputFileData%VS_Rgn2K*InputFileData%VS_RtGnSp**2 >  InputFileData%VS_RtTq )  &
         CALL SetErrors( ErrID_Fatal, 'VS_Rgn2K*VS_RtGnSp^2 must not be greater than VS_RtTq.' )
      IF ( InputFileData%VS_SlPc  <= 0.0_ReKi  )  CALL SetErrors( ErrID_Fatal, 'VS_SlPc must be greater than zero.' )
   END IF

      ! checks for generator models (VSControl == 0):           
   IF ( InputFileData%VSContrl == 0_IntKi ) THEN
      
      IF ( InputFileData%GenModel < 1_IntKi .OR. InputFileData%GenModel > 3_IntKi )  THEN
         CALL SetErrors( ErrID_Fatal, 'GenModel must be either 1, 2, or 3.' )
      ENDIF            
      
         ! checks for simple induction generator (VSControl=0 & GenModel=1):      
      IF ( InputFileData%GenModel == 1_IntKi ) THEN
         IF ( InputFileData%SIG_SlPc <= 0.0_ReKi )  CALL SetErrors( ErrID_Fatal, 'SIG_SlPc must be greater than zero.' )
         IF ( InputFileData%SIG_SySp <= 0.0_ReKi )  CALL SetErrors( ErrID_Fatal, 'SIG_SySp must be greater than zero.' )
         IF ( InputFileData%SIG_RtTq <= 0.0_ReKi )  CALL SetErrors( ErrID_Fatal, 'SIG_RtTq must be greater than zero.' )
         IF ( InputFileData%SIG_PORt <  1.0_ReKi )  CALL SetErrors( ErrID_Fatal, 'SIG_PORt must not be less than 1.' )
      END IF

         ! checks for Thevenin-equivalent induction generator (VSControl=0 & GenModel=2):      
      IF ( InputFileData%GenModel == 2_IntKi ) THEN
         IF ( InputFileData%TEC_Freq <= 0.0_ReKi ) CALL SetErrors( ErrID_Fatal, 'TEC_Freq must be greater than zero.' )
         IF ( InputFileData%TEC_NPol <= 0_IntKi .OR. MOD( InputFileData%TEC_NPol, 2_IntKi ) /= 0_IntKi ) &
                                     CALL SetErrors( ErrID_Fatal, 'TEC_NPol must be an even number greater than zero.' )
         IF ( InputFileData%TEC_SRes <= 0.0_ReKi ) CALL SetErrors( ErrID_Fatal, 'TEC_SRes must be greater than zero.' )
         IF ( InputFileData%TEC_RRes <= 0.0_ReKi ) CALL SetErrors( ErrID_Fatal, 'TEC_RRes must be greater than zero.' )
         IF ( InputFileData%TEC_VLL  <= 0.0_ReKi ) CALL SetErrors( ErrID_Fatal, 'TEC_VLL must be greater than zero.'  )
         IF ( InputFileData%TEC_SLR  <= 0.0_ReKi ) CALL SetErrors( ErrID_Fatal, 'TEC_SLR must be greater than zero.'  )
         IF ( InputFileData%TEC_RLR  <= 0.0_ReKi ) CALL SetErrors( ErrID_Fatal, 'TEC_RLR must be greater than zero.'  )
         IF ( InputFileData%TEC_MR   <= 0.0_ReKi ) CALL SetErrors( ErrID_Fatal, 'TEC_MR must be greater than zero.'   )
      END IF      
      
   END IF
   
      
      ! checks for high-speed shaft brake:       
   IF ( InputFileData%HSSBrMode < 1_IntKi .OR. InputFileData%HSSBrMode > 3_IntKi )  &
                                             CALL SetErrors( ErrID_Fatal, 'HSSBrMode must be 1, 2 or 3.' )
   IF ( InputFileData%THSSBrDp < 0.0_DbKi )  CALL SetErrors( ErrID_Fatal, 'THSSBrDp must not be negative.' )
   IF ( InputFileData%HSSBrDT  < 0.0_ReKi )  CALL SetErrors( ErrID_Fatal, 'HSSBrDT must not be negative.'  )
   IF ( InputFileData%HSSBrTqF < 0.0_ReKi )  CALL SetErrors( ErrID_Fatal, 'HSSBrTqF must not be negative.' )

      ! checks for nacelle-yaw control:
   IF ( InputFileData%TYawManS < 0.0_DbKi )  CALL SetErrors( ErrID_Fatal, 'TYawManS must not be negative.' )
!   IF ( InputFileData%TYawManE < InputFileData%TYawManS ) CALL SetErrors( ErrID_Fatal, 'TYawManE must not be less than TYawManS.')
   

   IF ( InputFileData%YawSpr  < 0.0_ReKi )  CALL SetErrors( ErrID_Fatal, 'YawSpr must not be negative.' )
   IF ( InputFileData%YawDamp < 0.0_ReKi )  CALL SetErrors( ErrID_Fatal, 'YawDamp must not be negative.' )
   IF ( InputFileData%YawNeut <= -pi  .OR.  InputFileData%YawNeut > pi )  &
      CALL SetErrors( ErrID_Fatal, 'YawNeut must be in the range (-pi, pi] radians (i.e., (-180,180] degrees).' )
      
   RETURN

CONTAINS
   !-------------------------------------------------------------------------------------------------------------------------------
   SUBROUTINE SetErrors( ErrStat3, ErrMsg3 )
   ! This routine sets the error message and flag when an error has occurred
   !...............................................................................................................................
   INTEGER(IntKi), INTENT(IN) :: ErrStat3     ! Error status for this error
   CHARACTER(*),   INTENT(IN) :: ErrMsg3      ! Error message for this error

      ErrStat = MAX( ErrStat, ErrStat3 )
      ErrMsg  = TRIM(ErrMsg)//NewLine//'  '//TRIM(ErrMsg3)

   END SUBROUTINE SetErrors
   !-------------------------------------------------------------------------------------------------------------------------------      
END SUBROUTINE ValidatePrimaryData
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE SrvD_SetParameters( InputFileData, p, ErrStat, ErrMsg )
! This subroutine sets the parameters, based on the data stored in InputFileData
!..................................................................................................................................

   TYPE(SrvD_InputFile),     INTENT(IN)       :: InputFileData  ! Data stored in the module's input file
   TYPE(SrvD_ParameterType), INTENT(INOUT)    :: p              ! The module's parameter data
   INTEGER(IntKi),           INTENT(OUT)      :: ErrStat        ! The error status code
   CHARACTER(*),             INTENT(OUT)      :: ErrMsg         ! The error message, if an error occurred

      ! Local variables
   REAL(ReKi)                                 :: ComDenom       ! Common denominator of variables used in the TEC model
   REAL(ReKi)                                 :: SIG_RtSp       ! Rated speed
   REAL(ReKi)                                 :: TEC_K1         ! K1 term for Thevenin-equivalent circuit
   REAL(ReKi)                                 :: TEC_K2         ! K2 term for Thevenin-equivalent circuit
   
   INTEGER(IntKi)                             :: K              ! Loop counter (for blades)
   INTEGER(IntKi)                             :: ErrStat2       ! Temporary error ID   
   CHARACTER(LEN(ErrMsg))                     :: ErrMsg2        ! Temporary message describing error


   
      ! Initialize variables

   ErrStat = ErrID_None
   ErrMsg  = ''

      
   
      ! Set parameters from primary input file        
   CALL SetPrimaryParameters( p, InputFileData, ErrStat2, ErrMsg2  )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
         
      !.............................................
      ! Pitch control parameters
      !.............................................
   p%PCMode   = InputFileData%PCMode
   p%TPCOn    = InputFileData%TPCOn      

      
      !.............................................
      ! Set generator and torque control parameters:
      !.............................................
   p%VSContrl  = InputFileData%VSContrl
   p%GenModel  = InputFileData%GenModel
   p%GenEff    = InputFileData%GenEff
   p%GenTiStr  = InputFileData%GenTiStr
   p%GenTiStp  = InputFileData%GenTiStp
   p%SpdGenOn  = InputFileData%SpdGenOn
   p%TimGenOn  = InputFileData%TimGenOn
      
   
   
   IF ( Cmpl4SFun .OR. Cmpl4LV ) THEN
      p%TimGenOf = HUGE( p%TimGenOf ) 
      CALL CheckError( ErrID_Info, 'TimGenOf is ignored when compiled for LabVIEW or Simulink.' )
   ELSE
      p%TimGenOf  = InputFileData%TimGenOf
   END IF
   
   
   p%THSSBrFl  = InputFileData%THSSBrDp + InputFileData%HSSBrDT   ! Time at which shaft brake is fully deployed
   
   SELECT CASE ( p%VSContrl )      
   CASE ( 0_IntKi )  ! None
      
      IF ( p%GenModel == 1_IntKi )     THEN   ! Simple induction generator

           SIG_RtSp  = InputFileData%SIG_SySp*( 1.0 + InputFileData%SIG_SlPc )                                 ! Rated speed
         p%SIG_POSl  = InputFileData%SIG_PORt*( SIG_RtSp - InputFileData%SIG_SySp )                                 ! Pullout slip
         p%SIG_POTq  = InputFileData%SIG_RtTq*InputFileData%SIG_PORt                                                ! Pullout torque
         p%SIG_Slop  = InputFileData%SIG_RtTq/( SIG_RtSp - InputFileData%SIG_SySp )                                 ! SIG torque/speed slope

         p%SIG_SySp = InputFileData%SIG_SySp
      ELSEIF ( p%GenModel == 2_IntKi )  THEN   ! Thevenin-equivalent induction generator

         ComDenom  = InputFileData%TEC_SRes**2 + ( InputFileData%TEC_SLR + InputFileData%TEC_MR )**2     ! common denominator used in many of the following equations
         
         p%TEC_Re1   = InputFileData%TEC_SRes*( InputFileData%TEC_MR**2 )/ComDenom                       ! Thevenin's equivalent stator resistance (ohms)
         p%TEC_Xe1   = InputFileData%TEC_MR*( InputFileData%TEC_SRes**2 + InputFileData%TEC_SLR* &
                                    ( InputFileData%TEC_SLR + InputFileData%TEC_MR) )/ComDenom           ! Thevenin's equivalent stator leakage reactance (ohms)
         p%TEC_V1a   = InputFileData%TEC_MR*InputFileData%TEC_VLL/SQRT( 3.0*ComDenom )                   ! Thevenin equivalent source voltage
         p%TEC_SySp  = 4.0*Pi*InputFileData%TEC_Freq/InputFileData%TEC_NPol                              ! Thevenin equivalent synchronous speed
           TEC_K1    = ( p%TEC_Xe1 + InputFileData%TEC_RLR )**2                                          ! Thevenin equivalent K1 term
           TEC_K2    = ( InputFileData%TEC_MR**2 )/ComDenom                                              ! Thevenin equivalent K2 term
         p%TEC_A0    = InputFileData%TEC_RRes*TEC_K2/p%TEC_SySp                                          ! Thevenin equivalent A0 term
         p%TEC_C0    = InputFileData%TEC_RRes**2                                                         ! Thevenin equivalent C0 term
         p%TEC_C1    = -2.0*p%TEC_Re1*InputFileData%TEC_RRes                                             ! Thevenin equivalent C1 term
         p%TEC_C2    = p%TEC_Re1**2 + TEC_K1                                                             ! Thevenin equivalent C2 term

         p%TEC_MR    = InputFileData%TEC_MR
         p%TEC_RLR   = InputFileData%TEC_RLR
         p%TEC_RRes  = InputFileData%TEC_RRes
         p%TEC_SRes  = InputFileData%TEC_SRes
         p%TEC_VLL   = InputFileData%TEC_VLL

      ENDIF
         
      
   CASE ( 1_IntKi ) ! Simple variable-speed control
      
      p%VS_SySp   = InputFileData%VS_RtGnSp/( 1.0 +  InputFileData%VS_SlPc )                                            ! Synchronous speed of region 2 1/2 induction generator.
      IF ( InputFileData%VS_SlPc < SQRT(EPSILON(InputFileData%VS_SlPc) ) ) THEN                                         ! We don't have a region 2 so we'll use VS_TrGnSp = VS_RtGnSp
         p%VS_Slope = 9999.9
         p%VS_TrGnSp = InputFileData%VS_RtGnSp
      ELSE
         p%VS_Slope  = InputFileData%VS_RtTq  /( InputFileData%VS_RtGnSp - p%VS_SySp )                                  ! Torque/speed slope of region 2 1/2 induction generator.
         IF ( ABS(InputFileData%VS_Rgn2K) < EPSILON(InputFileData%VS_SlPc) )  THEN  ! .TRUE. if the Region 2 torque is flat, and thus, the denominator in the ELSE condition is zero
            p%VS_TrGnSp = p%VS_SySp                                                                                     ! Transitional generator speed between regions 2 and 2 1/2.
         ELSE                          ! .TRUE. if the Region 2 torque is quadratic with speed
            p%VS_TrGnSp = ( p%VS_Slope - SQRT( p%VS_Slope*( p%VS_Slope - 4.0*InputFileData%VS_Rgn2K*p%VS_SySp ) ) ) &
                              / ( 2.0*InputFileData%VS_Rgn2K )                                                          ! Transitional generator speed between regions 2 and 2 1/2.
         ENDIF
      END IF
   
      p%VS_Rgn2K   = InputFileData%VS_Rgn2K
      p%VS_RtGnSp  = InputFileData%VS_RtGnSp
      p%VS_RtTq    = InputFileData%VS_RtTq
      
   END SELECT 
      
      !.............................................
      ! High-speed shaft brake parameters
      !.............................................   
   p%HSSBrMode = InputFileData%HSSBrMode
   IF ( Cmpl4SFun ) THEN
      p%THSSBrDp  = HUGE(p%THSSBrDp) ! Make sure this doesn't get shut off during simulation
      CALL CheckError( ErrID_Info, 'THSSBrDp is ignored when compiled for Simulink.' )
   ELSE      
      p%THSSBrDp  = InputFileData%THSSBrDp
   END IF 
   p%HSSBrDT   = InputFileData%HSSBrDT
   p%HSSBrTqF  = InputFileData%HSSBrTqF
      
      !.............................................
      ! Nacelle-yaw control parameters
      !.............................................
   p%YCMode   = InputFileData%YCMode
   p%TYCOn    = InputFileData%TYCOn
   p%TYawManS = InputFileData%TYawManS
   
   p%NacYawF  = InputFileData%NacYawF   
   p%YawNeut  = InputFileData%YawNeut !bjj: this should be renamed...
   p%YawSpr   = InputFileData%YawSpr
   p%YawDamp  = InputFileData%YawDamp

   
!removed:   
   !p%TYawManE = InputFileData%TYawManE   
   
   
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
         ! Clean up if we're going to return on error: close files, deallocate local arrays
         !.........................................................................................................................
         IF ( ErrStat >= AbortErrLev ) THEN
         END IF

      END IF


   END SUBROUTINE CheckError

END SUBROUTINE SrvD_SetParameters
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE Yaw_CalcOutput( t, u, p, x, xd, z, OtherState, y, ErrStat, ErrMsg )
! Routine for computing the yaw output: a yaw moment. This routine is used in both loose and tight coupling.
!..................................................................................................................................

   REAL(DbKi),                     INTENT(IN   )  :: t           ! Current simulation time in seconds
   TYPE(SrvD_InputType),           INTENT(IN   )  :: u           ! Inputs at t
   TYPE(SrvD_ParameterType),       INTENT(IN   )  :: p           ! Parameters
   TYPE(SrvD_ContinuousStateType), INTENT(IN   )  :: x           ! Continuous states at t
   TYPE(SrvD_DiscreteStateType),   INTENT(IN   )  :: xd          ! Discrete states at t
   TYPE(SrvD_ConstraintStateType), INTENT(IN   )  :: z           ! Constraint states at t
   TYPE(SrvD_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states
   TYPE(SrvD_OutputType),          INTENT(INOUT)  :: y           ! Outputs computed at t (Input only so that mesh con-
                                                                  !   nectivity information does not have to be recalculated)
   INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat     ! Error status of the operation
   CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None
  
      ! local variables      
   REAL(ReKi)                   :: YawPosCom                                       ! Commanded yaw angle from user-defined routines, rad.
   REAL(ReKi)                   :: YawRateCom                                      ! Commanded yaw rate  from user-defined routines, rad/s.


INTEGER(IntKi), PARAMETER :: ControlMode_None   = 0
INTEGER(IntKi), PARAMETER :: ControlMode_Simple = 2 
INTEGER(IntKi), PARAMETER :: ControlMode_User   = 1
INTEGER(IntKi), PARAMETER :: ControlMode_Extern = 3 !bjj: is this necessary (other than in the input file?); this would be needed by the glue code, not the module...
   

         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


   !...................................................................
   ! Calculate standard yaw position and rate commands:
   !...................................................................

         
   IF ( t >= p%TYCOn  .AND.  p%YCMode /= ControlMode_None )  THEN   ! Time now to enable active yaw control.


      SELECT CASE ( p%YCMode )  ! Which yaw control mode are we using? (we already took care of ControlMode_None)
            
         CASE ( ControlMode_Simple )            ! Simple ... BJJ: THIS IS NEW
            !
            !YawPosCom  = p%YawNeut
            !YawRateCom = p%YawRateNeut

            
         CASE ( ControlMode_User )              ! User-defined from routine UserYawCont().
         !
         !
         !! Calculate horizontal hub-height wind direction and the nacelle yaw error
         !!   estimate (both positive about zi-axis); these are zero if there is no
         !!   wind input when AeroDyn is not used:
         !
         !   IF ( p_FAST%CompAero )  THEN   ! AeroDyn has been used.
         !
         !      HHWndVec(:) = AD_GetUndisturbedWind( REAL(t, ReKi), (/ REAL(0.0, ReKi), REAL(0.0, ReKi), p_ED%FASTHH /), ErrStat )
         !
         !      WindDir  = ATAN2( HHWndVec(2), HHWndVec(1) )
         !      YawError = WindDir - x_ED%QT(DOF_Yaw) - x_ED%QT(DOF_Y)
         !
         !   ELSE                    ! No AeroDynamics.
         !
         !      WindDir  = 0.0
         !      YawError = 0.0
         !
         !   ENDIF
         !
         !! Call the user-defined yaw control routine:
         !
         !   CALL UserYawCont ( x_ED%QT(DOF_Yaw), x_ED%QDT(DOF_Yaw), WindDir, YawError, p%NumBl, t, DT, DirRoot, y%YawPosCom, y%YawRateCom )
         !

         CASE ( ControlMode_Extern )              ! User-defined from Simulink or Labview

            YawPosCom  = u%ExternalYawPosCom
            YawRateCom = u%ExternalYawRateCom


      END SELECT


   ELSE  ! Do not control yaw, maintain initial (neutral) yaw angles
      
         YawPosCom  = p%YawNeut
         YawRateCom = 0.0_ReKi

   ENDIF         
         
   !...................................................................
   ! Override standard yaw control with a linear maneuver if necessary:
   !...................................................................

   IF ( t >= p%TYawManS )  THEN  ! Override yaw maneuver is occuring.


      IF ( t < OtherState%BegYawMan )  THEN  ! Override yaw maneuver is just beginning (possibly again).

         OtherState%NacYawI   = YawPosCom  !bjj: was u%Yaw                                               ! Store the initial (current) yaw, at the start of the yaw maneuver
         OtherState%YawManRat = SIGN( OtherState%YawManRat, p%NacYawF - OtherState%NacYawI )             ! Modify the sign of YawManRat based on the direction of the yaw maneuever
         OtherState%TYawManE  = p%TYawManS + ( p%NacYawF - OtherState%NacYawI ) / OtherState%YawManRat   ! Calculate the end time of the override yaw maneuver         

         OtherState%BegYawMan = t                                                                        ! Let's remember when we stored this these values

      ENDIF
      
      
      IF ( t >= OtherState%TYawManE )  THEN   ! Override yaw maneuver has ended; yaw command is fixed at NacYawF

         YawPosCom     = p%NacYawF
         YawRateCom    = 0.0_ReKi

      ELSE                             ! Override yaw maneuver in linear ramp

            ! Increment the command yaw and rate using OtherState%YawManRat         

         YawPosCom     = OtherState%NacYawI + OtherState%YawManRat*( t - p%TYawManS )
         YawRateCom    = OtherState%YawManRat                                                                           

      ENDIF   
      
   ELSE
      
      OtherState%BegYawMan = HUGE( OtherState%BegYawMan )      ! We haven't started the yaw maneuver (or need to restart)

   ENDIF
                
   !...................................................................
   ! Calculate the yaw moment:
   !...................................................................
      
   y%YawMom = -  p%YawSpr *( u%Yaw     - YawPosCom  )     &          ! {-f(qd,q,t)}SpringYaw
              -  p%YawDamp*( u%YawRate - YawRateCom )                ! {-f(qd,q,t)}DampYaw;
   
   
END SUBROUTINE Yaw_CalcOutput
!----------------------------------------------------------------------------------------------------------------------------------
END MODULE ServoDyn
!**********************************************************************************************************************************
