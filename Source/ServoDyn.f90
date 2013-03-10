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
   INTEGER(IntKi), PARAMETER            :: MaxBl = 3

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

      p%RootName = TRIM(InitInp%RootName)//'_'//TRIM(SrvD_Ver%Name) ! all of the output file names from this module will end with '_ModuleName'

         ! Initialize the NWTC Subroutine Library

      CALL NWTC_Init( )

         ! Display the module information

      CALL DispNVD( SrvD_Ver )


         ! Read the input file and validate the data

      CALL SrvD_ReadInput( InitInp%InputFile, InputFileData, p%RootName, ErrStat, ErrMsg )
      !CALL SrvD_ValidateInput( InputFileData, ErrStat, ErrMsg )

         ! Define parameters here:
      !CALL SrvD_SetParameters( InputFileData, p, ErrStat, ErrMsg )           

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
      ALLOCATE( y%WriteOutput(p%NumOuts), STAT = ErrStat )
      IF ( ErrStat/= 0 ) THEN
         ErrStat = ErrID_Fatal
         ErrMsg  = 'Error allocating output header and units arrays in SrvD_Init'
         RETURN
      END IF

      y%WriteOutput = 0


         ! Define initialization-routine output here:
      ALLOCATE( InitOut%WriteOutputHdr(p%NumOuts), InitOut%WriteOutputUnt(p%NumOuts), STAT = ErrStat )
      IF ( ErrStat/= 0 ) THEN
         ErrStat = ErrID_Fatal
         ErrMsg  = 'Error allocating output header and units arrays in SrvD_Init'
         RETURN
      END IF

      !InitOut%WriteOutputHdr = (/ 'Time   ', 'Column2' /)
      !InitOut%WriteOutputUnt = (/ '(s)',     '(-)'     /)
      InitOut%Ver = SrvD_Ver

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

   TYPE(SrvD_InputFile), INTENT(INOUT) :: InputFileData                       ! All the data in the ElastoDyn input file
   
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

      ! GenEff - Generator efficiency [ignored by the Thevenin and user-defined generator models] (%):
   CALL ReadVar( UnIn, InputFile, InputFileData%GenEff, "GenEff", "Generator efficiency [ignored by the Thevenin and user-defined generator models] (%)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! GenTiStr - Method to start the generator {T: timed using TimGenOn, F: generator speed using SpdGenOn} (flag):
   CALL ReadVar( UnIn, InputFile, InputFileData%GenTiStr, "GenTiStr", "Method to start the generator {T: timed using TimGenOn, F: generator speed using SpdGenOn} (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! GenTiStp - Method to stop the generator {T: timed using TimGenOf, F: when generator power = 0} (flag):
   CALL ReadVar( UnIn, InputFile, InputFileData%GenTiStp, "GenTiStp", "Method to stop the generator {T: timed using TimGenOf, F: when generator power = 0} (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! SpdGenOn - Generator speed to turn on the generator for a startup (HSS speed) [used only when GenTiStr=False] (rpm):
   CALL ReadVar( UnIn, InputFile, InputFileData%SpdGenOn, "SpdGenOn", "Generator speed to turn on the generator for a startup (HSS speed) [used only when GenTiStr=False] (rpm)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

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
      
      ! VS_RtGnSp - Rated generator speed for simple variable-speed generator control (HSS side) [used only when VSContrl=1] (rpm):
   CALL ReadVar( UnIn, InputFile, InputFileData%VS_RtGnSp, "VS_RtGnSp", "Rated generator speed for simple variable-speed generator control (HSS side) [used only when VSContrl=1] (rpm)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! VS_RtTq - Rated generator torque/constant generator torque in Region 3 for simple variable-speed generator control (HSS side) [used only when VSContrl=1] (N-m):
   CALL ReadVar( UnIn, InputFile, InputFileData%VS_RtTq, "VS_RtTq", "Rated generator torque/constant generator torque in Region 3 for simple variable-speed generator control (HSS side) [used only when VSContrl=1] (N-m)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! VS_Rgn2K - Generator torque constant in Region 2 for simple variable-speed generator control (HSS side) [used only when VSContrl=1] (N-m/rpm^2):
   CALL ReadVar( UnIn, InputFile, InputFileData%VS_Rgn2K, "VS_Rgn2K", "Generator torque constant in Region 2 for simple variable-speed generator control (HSS side) [used only when VSContrl=1] (N-m/rpm^2)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! VS_SlPc - Rated generator slip percentage in Region 2 1/2 for simple variable-speed generator control [used only when VSContrl=1] (%):
   CALL ReadVar( UnIn, InputFile, InputFileData%VS_SlPc, "VS_SlPc", "Rated generator slip percentage in Region 2 1/2 for simple variable-speed generator control [used only when VSContrl=1] (%)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN     
      
   !---------------------- SIMPLE INDUCTION GENERATOR ------------------------------
   CALL ReadCom( UnIn, InputFile, 'Section Header: Simple Induction Generator', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
      ! SIG_SlPc - Rated generator slip percentage [used only when VSContrl=0 and GenModel=1] (%):
   CALL ReadVar( UnIn, InputFile, InputFileData%SIG_SlPc, "SIG_SlPc", "Rated generator slip percentage [used only when VSContrl=0 and GenModel=1] (%)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! SIG_SySp - Synchronous (zero-torque) generator speed [used only when VSContrl=0 and GenModel=1] (rpm):
   CALL ReadVar( UnIn, InputFile, InputFileData%SIG_SySp, "SIG_SySp", "Synchronous (zero-torque) generator speed [used only when VSContrl=0 and GenModel=1] (rpm)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

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

      ! TYawManE - Time at which override yaw maneuver reaches final yaw angle (s):
   CALL ReadVar( UnIn, InputFile, InputFileData%TYawManE, "TYawManE", "Time at which override yaw maneuver reaches final yaw angle (s)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

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

      ! OutList - List of user-requested output channels (-):
   CALL ReadOutputList ( UnIn, InputFile, InputFileData%OutList, InputFileData%NumOuts, 'OutList', "List of user-requested output channels", ErrStat2, ErrMsg2, UnEc  )     ! Routine in NWTC Subroutine Library
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN     
      
   !---------------------- END OF FILE -----------------------------------------
      
   CLOSE ( UnIn )
   IF ( UnEc > 0 ) CLOSE ( UnEc )
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
            IF ( UnEc > 0 ) CLOSE ( UnEc )
         END IF

      END IF


   END SUBROUTINE CheckError
   !...............................................................................................................................

END SUBROUTINE ReadPrimaryFile      
      

END MODULE ServoDyn
!**********************************************************************************************************************************
