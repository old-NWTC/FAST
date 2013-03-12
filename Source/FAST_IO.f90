MODULE FAST_IO_Subs

   USE   NWTC_Library
   USE   ElastoDyn_Parameters
   USE   ElastoDyn_Types
   USE   ElastoDyn
use ServoDyn
use ServoDyn_Types
   
   USE GlueCodeVars
   
CONTAINS
!====================================================================================================
SUBROUTINE AeroInput(p_ED)
! This subroutine sets up the information needed to initialize AeroDyn, then initializes AeroDyn
!----------------------------------------------------------------------------------------------------

   USE                     AeroElem !,   ONLY: ADAeroMarkers, ADCurrentOutputs, ADIntrfaceOptions, ADFirstLoop, Prev_Aero_t
   USE                     General,    ONLY: RootName, SumPrint


   USE                     AeroDyn

   IMPLICIT NONE

   ! Passed variables:
   TYPE(ED_ParameterType),  INTENT(INOUT)  :: p_ED      ! The parameters of the structural dynamics module
   
      ! Local variables

   TYPE(AD_InitOptions)       :: ADOptions                  ! Options for AeroDyn
   INTEGER                    :: NumADBldNodes              ! Number of blade nodes in AeroDyn

   
   INTEGER                    :: ErrStat


      ! Set up the AeroDyn parameters
   ADOptions%ADInputFile      = ADFile
   ADOptions%OutRootName      = RootName
   ADOptions%WrSumFile        = SumPrint
   
   
      ! Hub position and orientation (relative here, but does not need to be)

   ADInterfaceComponents%Hub%Position(:)      = 0.0
   
   ADInterfaceComponents%Hub%Orientation(:,:) = 0.0
   ADInterfaceComponents%Hub%Orientation(1,1) = 1.0
   ADInterfaceComponents%Hub%Orientation(2,2) = 1.0
   ADInterfaceComponents%Hub%Orientation(3,3) = 1.0
   
   
      ! Blade root position and orientation (relative here, but does not need to be)

   IF (.NOT. ALLOCATED( ADInterfaceComponents%Blade ) ) THEN
      ALLOCATE( ADInterfaceComponents%Blade( p_ED%NumBl ), STAT = ErrStat )
      IF ( ErrStat /= 0 ) THEN
         CALL ProgAbort( ' Error allocating space for ADInterfaceComponents%Blade.' )
      END IF
   END IF

  ADInterfaceComponents%Blade(:)%Position(1)      = p_ED%HubRad*p_ED%SinPreC(:)
  ADInterfaceComponents%Blade(:)%Position(2)      =  0.0_ReKi
  ADInterfaceComponents%Blade(:)%Position(3)      = p_ED%HubRad*p_ED%CosPreC(:)
  
  ADInterfaceComponents%Blade(:)%Orientation(1,1) =               p_ED%CosPreC(:)
  ADInterfaceComponents%Blade(:)%Orientation(2,1) =  0.0_ReKi
  ADInterfaceComponents%Blade(:)%Orientation(3,1) =  1.0 *        p_ED%SinPreC(:)
  
  ADInterfaceComponents%Blade(:)%Orientation(1,2) =  0.0_ReKi
  ADInterfaceComponents%Blade(:)%Orientation(2,2) =  1.0_ReKi
  ADInterfaceComponents%Blade(:)%Orientation(3,2) =  0.0_ReKi

  ADInterfaceComponents%Blade(:)%Orientation(1,3) = -1.0 *         p_ED%SinPreC(:)
  ADInterfaceComponents%Blade(:)%Orientation(2,3) =  0.0_ReKi
  ADInterfaceComponents%Blade(:)%Orientation(3,3) =                p_ED%CosPreC(:)


      ! Blade length
   
   ADInterfaceComponents%BladeLength = p_ED%TipRad - p_ED%HubRad
   
     
      ! Initialize AeroDyn

   ADAeroMarkers = AD_Init(ADOptions, ADInterfaceComponents, ErrStat)    ! relative markers are returned



      ! get the number of blade nodes from the returned data structure'
   IF (.NOT. ALLOCATED( ADAeroMarkers%Blade ) ) THEN
      CALL ProgAbort( 'AeroDyn blade nodes are required to calculate aerodynamic loads.' )
      NumADBldNodes = 0
   ELSE
      NumADBldNodes = SIZE( ADAeroMarkers%Blade, 1 )
   END IF

      ! allocate variables for aerodyn forces

   IF (.NOT. ALLOCATED(ADIntrfaceOptions%SetMulTabLoc)) THEN
      ALLOCATE( ADIntrfaceOptions%SetMulTabLoc(NumADBldNodes, p_ED%NumBl), STAT = ErrStat )
      IF ( ErrStat /= 0 ) CALL ProgAbort ( ' Error allocating memory for ADIntrfaceOptions%SetMulTabLoc array.' )
   END IF

   ADIntrfaceOptions%SetMulTabLoc(:,:) = .FALSE.
   ADIntrfaceOptions%LinearizeFlag     = .FALSE.

!   bjj: we don't use this, so don't allocate it
!
!   IF (.NOT. ALLOCATED(ADIntrfaceOptions%MulTabLoc)) THEN
!      ALLOCATE(ADIntrfaceOptions%MulTabLoc(NumADBldNodes, NumBl), STAT = Sttus )
!      IF ( Sttus /= 0 ) CALL ProgAbort ( ' Error allocating memory for ADIntrfaceOptions%MulTabLoc array.' )
!   END IF


   CALL Set_FAST_Params( p_ED )

   RETURN
END SUBROUTINE AeroInput
!====================================================================================================
SUBROUTINE FAST_Begin( InFile, InFileRoot, InFileRootAbs )


   ! This subroutine checks for command-line arguments, gets the root name of the input files 
   ! (including full path name), and creates the names of the output files.



IMPLICIT                        NONE

   ! Passed variables
   
CHARACTER(*), INTENT(INOUT)  :: InFile                                          ! A CHARACTER string containing the name of the input file
CHARACTER(*), INTENT(OUT)    :: InFileRoot                                      ! A CHARACTER string containing the root name of the input file (may be a relative path)
CHARACTER(*), INTENT(OUT)    :: InFileRootAbs                                   ! A CHARACTER string containing the root name of the input file, including the full path


   ! Local variables.

INTEGER                      :: Stat                                            ! The status of the call to GET_CWD
CHARACTER(1024)              :: DirName                                         ! A CHARACTER string containing the path of the current working directory.





   ! Check for command line arguments.  Maybe the user specified an input file name.
   ! Don't perform this check when interfaced with Simulink or Labview:

IF ( .NOT. Cmpl4SFun .AND. .NOT. Cmpl4LV  )  CALL CheckArgs( InFile )

CALL GetRoot( InFile, InFileRoot )


   ! Let's create a root file name variable, DirRoot, which includes the full
   !   path to the current working directory.

IF ( PathIsRelative(InFileRoot) ) THEN
   CALL Get_CWD  ( DirName, Stat )
   IF (Stat /= 0) CALL ProgAbort('Error retreiving current working directory.')
   
   InFileRootAbs = TRIM( DirName )//PathSep//TRIM( InFileRoot )
ELSE
   InFileRootAbs = InFileRoot
END IF   


   ! Let's create the name of the output file.

IF ( Cmpl4SFun )  InFileRoot = TRIM( InFileRoot )//'_SFunc'


RETURN
END SUBROUTINE FAST_Begin
!=======================================================================
SUBROUTINE GetPrimary( InputFileData, p, ErrStat, ErrMsg, UnEc )


   ! This routine reads the primary parameter file and validates the input.


USE                             Drivetrain
USE                             General
USE                             InitCond
USE                             NacelleYaw
USE                             SimCont
USE                             TipBrakes
USE                             TurbCont

IMPLICIT                        NONE

   ! Passed variables
TYPE(ED_InputFile),     INTENT(OUT)      :: InputFileData                   ! Data stored in the module's input file
TYPE(ED_ParameterType), INTENT(INOUT)    :: p                               ! The module's parameter data
INTEGER(IntKi),           INTENT(OUT)      :: ErrStat                         ! The error status code 
CHARACTER(*),             INTENT(OUT)      :: ErrMsg                          ! The error message, if an error occurred 
INTEGER(IntKi),           INTENT(OUT)      :: UnEc                            ! unit number for echo file   

   ! Local variables.

INTEGER(4)                   :: I                                               ! A generic index.
INTEGER(4)                   :: IOS                                             ! I/O status returned from the read statement.
INTEGER(4)                   :: K                                               ! Blade number.
INTEGER(4)                   :: Sttus                                           ! Status returned from an allocation request.
INTEGER(IntKi)               :: OutFileFmt                                      ! The switch indicating the format (text/binary) for the tabular output file(s)
LOGICAL                      :: WrEcho

CHARACTER(1024)              :: Comment                                         ! String to temporarily hold the comment line.
CHARACTER(  35), PARAMETER   :: Frmt      = "( 2X, L11, 2X, A, T30, ' - ', A )" ! Output format for logical parameters. (matches NWTC Subroutine Library format)
CHARACTER(1024)              :: PriPath                                         ! The path to the primary input file
CHARACTER(1024)              :: Msg                                             ! Temporary error message


ErrMsg = ''

   ! Note that all the errors in reading from the input file are ErrID_Fatal
   
   
   ! Open the primary input file.

CALL OpenFInpFile ( UnIn, PriFile, ErrStat, ErrMsg )
IF ( ErrStat >= AbortErrLev ) RETURN   

CALL GetPath( PriFile, PriPath )    ! Input files will be relative to the path where the primary input file is located.



   ! TMax - Total run time.

CALL ReadVar ( UnIn, PriFile, TMax, 'TMax', 'Total run time', UnEc=UnEc  )

IF ( TMax < 0.0  )  CALL ProgAbort ( ' TMax must not be a negative number.' )

   ! DT - Integration time step.

CALL ReadVar ( UnIn, PriFile, DT, 'DT', 'Integration time step', UnEc=UnEc  )

IF ( DT <= 0.0_DbKi )  CALL ProgAbort ( ' DT must be greater than 0.' )
IF ( DT <= TMax*EPSILON(DT) )  CALL ProgAbort ( ' DT must be greater than '//TRIM ( Num2LStr( TMax*EPSILON(DT) ) )//' seconds.' ) ! Test DT and TMax to ensure numerical stability -- HINT: see the use of OnePlusEps.



IF ( (Cmpl4SFun .OR. Cmpl4LV) .AND. ( InputFileData%OoPDefl /= 0.0 ) )  &
   CALL ProgAbort ( ' Initial out-of-plane blade-tip displacements must be zero when FAST is interfaced with Simulink'// &
                ' or Labview. Set OoPDefl to 0.0 or use the standard version of FAST.'                )

IF ( (Cmpl4SFun .OR. Cmpl4LV) .AND. ( InputFileData%IPDefl  /= 0.0 ) )  &
   CALL ProgAbort ( ' Initial in-plane blade-tip displacements must be zero when FAST is interfaced with Simulink'// &
                ' or Labview. Set IPDefl to 0.0 or use the standard version of FAST.'                 )

IF ( (Cmpl4SFun .OR. Cmpl4LV) .AND. ( InputFileData%TTDspFA /= 0.0 ) )  &
   CALL ProgAbort ( ' Initial fore-aft tower-top displacements must be zero when FAST is interfaced with Simulink'// &
                ' or Labview. Set TTDspFA to 0.0 or use the standard version of FAST.'               )

IF ( (Cmpl4SFun .OR. Cmpl4LV) .AND. ( InputFileData%TTDspSS /= 0.0 ) )  &
   CALL ProgAbort ( ' Initial side-to-side tower-top displacements must be zero when FAST is interfaced with Simulink'// &
                ' or Labview. Set TTDspSS to 0.0 or use the standard version of FAST.'                      )


IF ( ( p%GBoxEff <= 0.0 ) .OR. ( p%GBoxEff > 100.0 ) ) THEN
   CALL ProgAbort ( ' GBoxEff must be greater than 0 and less than or equal to 100.' )
END IF   

IF ( ( p%GenEff < 0.0 ) .OR. ( p%GenEff > 100.0 ) )  CALL ProgAbort ( ' GenEff must be between 0 and 100 (inclusive).' )


IF ( p%TwrLdMod /= 0 .AND. p%TwrLdMod /= 1 ) THEN
   CALL ProgAbort ( ' TwrLdMod must be either 0 or 1.' )
END IF   


!  -------------- TIP-BRAKE PARAMETERS -----------------------------------------

IF ( TBDrConN < 0.0 )  CALL ProgAbort ( ' TBDrConN must not be negative.' )
IF ( TBDrConD < TBDrConN )  CALL ProgAbort( ' TBDrConD must not be less than TBDrConN.' )
IF ( TpBrDT < 0.0 )  CALL ProgAbort ( ' TpBrDT must not be negative.' )


!  -------------- AERODYN INPUT FILE PARAMETERS -------------------------------
   ! ADFile - Name of file containing AeroDyn parameters.

CALL ReadVar( UnIn, PriFile, ADFile, 'ADFile', 'Name of file containing AeroDyn parameters', UnEc=UnEc )

!  -------------- OUTPUT PARAMETERS --------------------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'output parameters', UnEc=UnEc )


   ! SumPrint - Print summary data to "*.fsm".

CALL ReadVar ( UnIn, PriFile, SumPrint, 'SumPrint', 'Print summary data to "*.fsm"', UnEc=UnEc )


   ! OutFileFmt - Format for output file(s).

CALL ReadVar ( UnIn, PriFile, OutFileFmt, 'OutFileFmt', 'Format for output file(s)', UnEc=UnEc )
SELECT CASE (OutFileFmt)
   CASE (1_IntKi)
      WrBinOutFile = .FALSE.
      WrTxtOutFile = .TRUE.
   CASE (2_IntKi)
      WrBinOutFile = .TRUE.
      WrTxtOutFile = .FALSE.
   CASE (3_IntKi)
      WrBinOutFile = .TRUE.
      WrTxtOutFile = .TRUE.
   CASE DEFAULT
     CALL ProgAbort ( ' OutFileFmt must be 1, 2, or 3.' )
END SELECT

IF ( WrTxtOutFile .AND. ( TMax > 9999.999 ) )  THEN
   CALL ProgAbort ( ' TMax must not exceed 9999.999 seconds with text tabular (time-marching) output files.' )
END IF   


   ! TabDelim - Generate a tab-delimited output file.

CALL ReadVar ( UnIn, PriFile, InputFileData%TabDelim, 'TabDelim', 'Use tab delimiters in text output file', UnEc=UnEc )

   ! set the delimiter
   
IF ( InputFileData%TabDelim ) THEN
   p%Delim = TAB
ELSE
   p%Delim = ' '
END IF      


   ! OutFmt - Output format for tabular data.

CALL ReadVar ( UnIn, PriFile, p%OutFmt, 'OutFmt', 'Output format for text tabular data', UnEc=UnEc )

IF ( LEN_TRIM( p%OutFmt ) == 0 )  CALL ProgAbort ( ' OutFmt must not be an empty string.' )


   ! TStart - Time to start tabular output.

CALL ReadVar ( UnIn, PriFile, TStart, 'TStart', 'Time to begin tabular output', UnEc=UnEc )

IF ( TStart < 0.0  )  CALL ProgAbort ( ' TStart must not be less than 0.' )
IF ( TMax < TStart )  CALL ProgAbort ( ' TMax must not be less than TStart.' )


   ! DecFact - Decimation factor for tabular output.

CALL ReadVar ( UnIn, PriFile, DecFact, 'DecFact', 'Decimation factor for tabular output', UnEc=UnEc )

IF ( DecFact < 1 )  CALL ProgAbort ( ' DecFact must be greater than 0.' )


   ! SttsTime - Amount of time between screen status messages.

CALL ReadVar ( UnIn, PriFile, SttsTime, 'SttsTime', 'Amount of time between screen status messages', UnEc=UnEc )

IF ( SttsTime <= 0.0_DbKi )  CALL ProgAbort ( ' SttsTime must be greater than 0.' )



   ! Skip the comment line.

CALL ReadCom ( UnIn, PriFile, 'output-parameters list', UnEc=UnEc )


   ! OutList - Output parameter list.
  
CALL AllocAry( InputFileData%OutList, MaxOutPts, "ElastoDyn InputFile's Outlist", ErrStat, ErrMsg )
IF ( ErrStat >= AbortErrLev ) RETURN
   
   InputFileData%OutList = ''   ! Initialize OutList(:) to ''.

   ! Lets read in all of the lines containing output parameters and store them in OutList(:).
   ! The end of this list (and the end of the output file) is specified with the line
   !    beginning with END.

CALL ReadOutputList ( UnIn, PriFile, InputFileData%OutList, p%NumOuts, 'OutList', 'Output list', ErrStat, ErrMsg, UnEc  )     ! Routine in NWTC Subroutine Library
IF ( ErrStat >= AbortErrLev ) RETURN

   ! Check to make sure some outputs have been entered when time-marching;
   !   if not, ProgAbort:

IF ( ( p%NumOuts == 0 ) .AND. ( AnalMode == 1 ) )  THEN
   CALL ProgAbort ( ' No output channels specified!' )
ENDIF



   ! Close primary input file.

CLOSE ( UnIn )


RETURN
END SUBROUTINE GetPrimary
!=======================================================================
SUBROUTINE FAST_Input( p, p_SrvD, OtherState, InputFileData, ErrStat, ErrMsg )


   ! This routine reads the input files and does some preliminary processing.


   ! FAST Modules:

USE                             DriveTrain
USE                             General
USE                             InitCond
USE                             NacelleYaw
USE                             SimCont
USE                             TipBrakes
USE                             TurbCont

USE HydroVals
USE                             FAST_Hydro

   ! AeroDyn Modules:
USE                             Airfoil,        ONLY: NumFoil
USE                             InducedVel,     ONLY: AToler
USE                             Switch,         ONLY: DSTALL, DYNINFL, DYNINIT


IMPLICIT                        NONE

   ! passed variables
TYPE(ED_ParameterType), INTENT(INOUT) :: p                                 ! Parameter data type for structural dynamics module
TYPE(SrvD_ParameterType), INTENT(INOUT) :: p_SrvD                            ! Parameter data type for ServoDyn module
TYPE(ED_OtherStateType),INTENT(INOUT) :: OtherState                        ! Other State data type for Structural dynamics module
TYPE(ED_InputFile),     INTENT(OUT)   :: InputFileData                     ! all the data in the ElastoDyn input file
INTEGER,          INTENT(OUT),OPTIONAL  :: ErrStat                           ! Error status
CHARACTER(*),     INTENT(OUT),OPTIONAL  :: ErrMsg                            ! Error message corresponding to ErrStat 

   ! Local variables.

REAL(ReKi)                   :: ComDenom                                        ! Common denominator used in computation of TEC model coefficients
REAL(ReKi)                   :: SumCosPreC                                      ! SUM( CosPreC(K) ) for K = 1,NumBl

INTEGER(4)                   :: I                                               ! A generic index for DO loops.
INTEGER(4)                   :: K                                               ! Index for blade number.
INTEGER(4)                   :: Sttus                                           ! Status of an attempted array allocation.

INTEGER(IntKi)               :: EchoUn



      !TYPE(ED_InitInputType)        :: InitInp     ! Input data for initialization routine
      !TYPE(ED_InputType)            :: u           ! An initial guess for the input; input mesh must be defined
      !TYPE(ED_ContinuousStateType)  :: x           ! Initial continuous states
      !TYPE(ED_DiscreteStateType)    :: xd          ! Initial discrete states
      !TYPE(ED_ConstraintStateType)  :: z           ! Initial guess of the constraint states
      !TYPE(ED_OutputType)           :: y           ! Initial system outputs (outputs are not calculated; only the output mesh is initialized)
      !TYPE(ED_InitOutputType)       :: InitOut     ! Output for initialization routine

      

!  -------------- PRIMARY PARAMETERS -------------------------------------------



!   ! Read the primary parameter file (set EchoUn here):
!
!CALL GetPrimary( InputFileData, p, ErrStat, ErrMsg, EchoUn )
!
!
!!!---------------- START ED_INIT -------------------------------------
!!InitInp%ADInputFile = ADFile
!!InitInp%InputFile = PriFile
!!CALL ED_Init( InitInp, u, p, x, xd, z, OtherState, y, DT, InitOut, ErrStat, ErrMsg )
!
!CALL DispNVD( ED_Ver )
!
!   ! Read the input file and validate the data
!
!CALL ED_ReadInput( PriFile, ADFile, InputFileData, .TRUE., RootName, ErrStat, ErrMsg )
!CALL ED_ValidateInput( InputFileData, ErrStat, ErrMsg )
!
!   ! Define parameters here:
!CALL ED_SetParameters( InputFileData, p, ErrStat, ErrMsg )         
!CALL InitDOFs( p, ErrStat, ErrMsg )
!
!CALL Alloc_CoordSys( OtherState%CoordSys, p, ErrStat, ErrMsg )
!!---------------- END ED_INIT -------------------------------------
!

   ! Calculate some parameters that are not input directly.  Convert units if appropriate.

DT24      = DT/24.0_DbKi                                                        ! Time-step parameter needed for Solver().
p%GenEff  = p%GenEff *0.01

SpdGenOn  = SpdGenOn*RPM2RPS
TBDepISp  = TBDepISp*RPM2RPS
THSSBrFl  = THSSBrDp + HSSBrDT

IF ( VSContrl == 1 )  THEN       ! Simple variable-speed control

   VS_RtGnSp = VS_RtGnSp*  RPM2RPS                                                                    ! Convert rated speed to rad/sec.
   VS_Rgn2K  = VS_Rgn2K /( RPM2RPS*RPM2RPS )                                                          ! Convert Region 2 torque constant to Nm/(rad/sec)^2.
   VS_SySp   = VS_RtGnSp/( 1.0 +  0.01*VS_SlPc )                                                      ! Synchronous speed of region 2 1/2 induction generator.
   IF ( VS_SlPc < SQRT(EPSILON(VS_SlPc) ) ) THEN                                                      ! We don't have a region 2 so we'll use VS_TrGnSp = VS_RtGnSp
      VS_Slope = 9999.9
      VS_TrGnSp = VS_RtGnSp
   ELSE
      VS_Slope  = VS_RtTq  /( VS_RtGnSp - VS_SySp )                                                   ! Torque/speed slope of region 2 1/2 induction generator.
      IF ( ABS(VS_Rgn2K) < EPSILON(VS_SlPc) )  THEN  ! .TRUE. if the Region 2 torque is flat, and thus, the denominator in the ELSE condition is zero
         VS_TrGnSp = VS_SySp                                                                                ! Transitional generator speed between regions 2 and 2 1/2.
      ELSE                          ! .TRUE. if the Region 2 torque is quadratic with speed
         VS_TrGnSp = ( VS_Slope - SQRT( VS_Slope*( VS_Slope - 4.0*VS_Rgn2K*VS_SySp ) ) )/( 2.0*VS_Rgn2K )   ! Transitional generator speed between regions 2 and 2 1/2.
      ENDIF
   END IF


ELSEIF ( GenModel == 1 )  THEN   ! Simple induction generator

   SIG_SySp  = SIG_SySp*RPM2RPS                                                 ! Convert synchronous speed to rad/sec.
   SIG_RtSp  = SIG_SySp*( 1.0 + 0.01*SIG_SlPc )                                 ! Rated speed.
   SIG_POSl  = SIG_PORt*( SIG_RtSp - SIG_SySp )                                 ! Pullout slip.
   SIG_POTq  = SIG_RtTq*SIG_PORt                                                ! Pullout torque.
   SIG_Slop  = SIG_RtTq/( SIG_RtSp - SIG_SySp )                                 ! SIG torque/speed slope.

ELSEIF ( GenModel == 2 )  THEN   ! Thevenin-equivalent generator

   ComDenom  = TEC_SRes**2 + ( TEC_SLR + TEC_MR )**2                            ! common denominator used in many of the following equations
   TEC_Re1   = TEC_SRes*( TEC_MR**2 )/ComDenom                                  ! Thevenin's equivalent stator resistance (ohms)
   TEC_Xe1   = TEC_MR*( TEC_SRes**2 + TEC_SLR*( TEC_SLR + TEC_MR) )/ComDenom    ! Thevenin's equivalent stator leakage reactance (ohms)
   TEC_V1a   = TEC_MR*TEC_VLL/SQRT( 3.0*ComDenom )                              ! Thevenin equivalent source voltage.
   TEC_SySp  = 4.0*Pi*TEC_Freq/TEC_NPol                                         ! Thevenin equivalent synchronous speed.
   TEC_K1    = ( TEC_Xe1 + TEC_RLR )**2                                         ! Thevenin equivalent K1 term.
   TEC_K2    = ( TEC_MR**2 )/ComDenom                                           ! Thevenin equivalent K2 term.
   TEC_A0    = TEC_RRes*TEC_K2/TEC_SySp                                         ! Thevenin equivalent A0 term.
   TEC_C0    = TEC_RRes**2                                                      ! Thevenin equivalent C0 term.
   TEC_C1    = -2.0*TEC_Re1*TEC_RRes                                            ! Thevenin equivalent C1 term.
   TEC_C2    = TEC_Re1**2 + TEC_K1                                              ! Thevenin equivalent C2 term.

ENDIF



   ! ALLOCATE some arrays:

ALLOCATE ( BlPitchInit(p%NumBl) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort(' Error allocating memory for the BlPitchInit array.')
ENDIF

ALLOCATE ( BegPitMan(p%NumBl) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort(' Error allocating memory for the BegPitMan array.')
ENDIF

ALLOCATE ( TTpBrFl(p%NumBl) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort(' Error allocating memory for the TTpBrFl array.')
ENDIF



   ! Initialize this variable to zero:

DO K=1,p%NumBl
   !BlPitch    (K) = BlPitch (K)*D2R
   BlPitchF   (K) = BlPitchF(K)*D2R
   BlPitchInit(K) = BlPitch (K)
   BegPitMan  (K) = .TRUE.
   TTpBrFl    (K) = TTpBrDp (K) + TpBrDT
ENDDO ! K



!  -------------- FINISHING UP -------------------------------------------------


   ! Close echo file, if appropriate.

IF ( EchoUn > 0 ) CLOSE ( EchoUn )


!  -------------- AERODYN PARAMETERS -------------------------------------------


   ! Get the AeroDyn input now.

CALL AeroInput(p)             ! Read in the ADFile



!  -------------- CONTROL ------------------------------------------------------


   ! bjj: these should be moved later...
p_SrvD%NumBl   = p%NumBl   
p_SrvD%GBRatio = p%GBRatio
p_SrvD%GBoxEff = p%GBoxEff
p_SrvD%GenEff  = p%GenEff

RETURN
END SUBROUTINE FAST_Input
!=======================================================================
SUBROUTINE PrintSum( p, OtherState )


   ! This routine generates the summary file, which contains a regurgitation of
   !  the input data and interpolated flexible body data.

USE                             AeroDyn
USE                             General
USE                             SimCont


IMPLICIT                        NONE

   ! passed variables
TYPE(ED_ParameterType),  INTENT(IN)  :: p                                    ! Parameters of the structural dynamics module
TYPE(ED_OtherStateType), INTENT(IN)  :: OtherState                           ! Other/optimization states of the structural dynamics module 


   ! Local variables.

INTEGER(4)                   :: I                                               ! Index for the nodes.
INTEGER(4)                   :: K                                               ! Index for the blade number.

INTEGER                      :: ErrStat

CHARACTER(24)                :: Fmt1      = "(34X,3(6X,'Blade',I2,:))"          ! Format for outputting blade headings.
CHARACTER(15)                :: Fmt2      = "(34X,3(6X,A,:))"                   ! Format for outputting blade headings.
CHARACTER(18)                :: FmtDat    = '(A,T35,3(:,F13.3))'                ! Format for outputting mass and modal data.
CHARACTER(18)                :: FmtDatT   = '(A,T35,1(:,F13.8))'                ! Format for outputting time steps.
CHARACTER( 8)                :: FmtHead   = '(//,A,/)'                          ! Format for outputting headings.
CHARACTER( 9)                :: FmtTitl   = '(//,1X,A)'                         ! Format for outputting title.
CHARACTER( 3)                :: FmtTxt    = '(A)'                               ! Format for outputting pure text.
CHARACTER(99)                :: RotorType                                       ! Text description of rotor.

CHARACTER(30)                :: OutPFmt                                         ! Format to print list of selected output channels to summary file

   ! Open the summary file and give it a heading.

CALL OpenFOutFile ( UnSu, TRIM( RootName )//'.fsm' )

WRITE (UnSu,'(/,A)')  'This summary information was generated by '//TRIM(ProgName)//' '//TRIM( ProgVer )// &
                      ' on '//CurDate()//' at '//CurTime()//'.'
WRITE (UnSu,FmtTitl)  TRIM( FTitle )


   ! Turbine features.

WRITE (UnSu,FmtHead)  'Turbine features:'

IF ( p%OverHang > 0.0 )  THEN
   RotorType = 'Downwind,'
ELSE
   RotorType = 'Upwind,'
ENDIF
IF ( p%NumBl == 2 )  THEN
   RotorType = TRIM(RotorType)//' two-bladed rotor'
   IF ( p%DOF_Flag(DOF_Teet) )  THEN
      RotorType = TRIM(RotorType)//' with teetering hub.'
   ELSE
      RotorType = TRIM(RotorType)//' with rigid hub.'
   ENDIF
ELSE
   RotorType = TRIM(RotorType)//' three-bladed rotor with rigid hub.'
ENDIF

WRITE    (UnSu,FmtTxt)  '            '//TRIM(RotorType)

WRITE    (UnSu,FmtTxt)  '            The model has '//TRIM(Num2LStr( p%DOFs%NActvDOF ))//' of '// &
                                     TRIM(Num2LStr( p%NDOF ))//' DOFs active (enabled) at start-up.'

IF ( p%DOF_Flag( DOF_BF(1,1) ) )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    First flapwise blade mode DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   First flapwise blade mode DOF.'
ENDIF

IF ( p%DOF_Flag( DOF_BF(1,2) ) )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Second flapwise blade mode DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Second flapwise blade mode DOF.'
ENDIF

IF ( p%DOF_Flag( DOF_BE(1,1) ) )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Edgewise blade mode DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Edgewise blade mode DOF.'
ENDIF

IF ( p%NumBl == 2 .AND. p%DOF_Flag(DOF_Teet) )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Rotor-teeter DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Rotor-teeter DOF.'
ENDIF

IF ( p%DOF_Flag(DOF_DrTr) )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Drivetrain rotational-flexibility DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Drivetrain rotational-flexibility DOF.'
ENDIF

IF ( p%DOF_Flag(DOF_GeAz) )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Generator DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Generator DOF.'
ENDIF

IF ( p%DOF_Flag(DOF_RFrl) )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Rotor-furl DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Rotor-furl DOF.'
ENDIF

IF ( p%DOF_Flag(DOF_TFrl) )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Tail-furl DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Tail-furl DOF.'
ENDIF

IF ( p%DOF_Flag(DOF_Yaw ) )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Yaw DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Yaw DOF.'
ENDIF

IF ( p%DOF_Flag(DOF_TFA1) )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    First tower fore-aft bending-mode DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   First tower fore-aft bending-mode DOF.'
ENDIF

IF ( p%DOF_Flag(DOF_TFA2) )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Second tower fore-aft bending-mode DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Second tower fore-aft bending-mode DOF.'
ENDIF

IF ( p%DOF_Flag(DOF_TSS1) )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    First tower side-to-side bending-mode DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   First tower side-to-side bending-mode DOF.'
ENDIF

IF ( p%DOF_Flag(DOF_TSS2) )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Second tower side-to-side bending-mode DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Second tower side-to-side bending-mode DOF.'
ENDIF

IF ( p%DOF_Flag(DOF_Sg  ) )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Platform horizontal surge translation DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Platform horizontal surge translation DOF.'
ENDIF

IF ( p%DOF_Flag(DOF_Sw  ) )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Platform horizontal sway translation DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Platform horizontal sway translation DOF.'
ENDIF

IF ( p%DOF_Flag(DOF_Hv  ) )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Platform vertical heave translation DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Platform vertical heave translation DOF.'
ENDIF

IF ( p%DOF_Flag(DOF_R   ) )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Platform roll tilt rotation DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Platform roll tilt rotation DOF.'
ENDIF

IF ( p%DOF_Flag(DOF_P   ) )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Platform pitch tilt rotation DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Platform pitch tilt rotation DOF.'
ENDIF

IF ( p%DOF_Flag(DOF_Y   ) )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Platform yaw rotation DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Platform yaw rotation DOF.'
ENDIF

IF ( CompAero )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Computation of aerodynamic loads.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Computation of aerodynamic loads.'
ENDIF

IF ( CompHydro )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Computation of hydrodynamic loads.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Computation of hydrodynamic loads.'
ENDIF



   ! Time steps.

WRITE (UnSu,FmtHead)  'Time steps:'

WRITE (UnSu,FmtDatT) '    Structural            (s)     ', DT
!bjj: does this really belong in this file????
WRITE (UnSu,FmtDatT) '    Aerodynamic           (s)     ', DT*CEILING( AD_GetConstant('dtAero',ErrStat) / REAL(DT,ReKi)) ! AeroDyn will be called at integer multiples of DT that are greater than or equal to DTAero, since FAST's integration scheme marches with a constant time step of DT.

   ! Some calculated parameters.

WRITE (UnSu,FmtHead)  'Some calculated parameters:'

WRITE (UnSu,FmtDat ) '    Hub-Height            (m)     ', p%FASTHH
WRITE (UnSu,FmtDat ) '    Flexible Tower Length (m)     ', p%TwrFlexL
WRITE (UnSu,FmtDat ) '    Flexible Blade Length (m)     ', p%BldFlexL


   ! Rotor properties:

WRITE (UnSu,FmtHead)  'Rotor mass properties:'

WRITE (UnSu,FmtDat ) '    Rotor Mass            (kg)    ', p%RotMass
WRITE (UnSu,FmTDat ) '    Rotor Inertia         (kg-m^2)', p%RotINer

WRITE (UnSu,Fmt1   ) ( K,         K=1,p%NumBl )
WRITE (UnSu,Fmt2   ) ( '-------', K=1,p%NumBl )

WRITE (UnSu,FmtDat ) '    Mass                  (kg)    ', ( p%BldMass  (K), K=1,p%NumBl )
WRITE (UnSu,FmtDat ) '    Second Mass Moment    (kg-m^2)', ( p%SecondMom(K), K=1,p%NumBl )
WRITE (UnSu,FmtDat ) '    First Mass Moment     (kg-m)  ', ( p%FirstMom (K), K=1,p%NumBl )
WRITE (UnSu,FmtDat ) '    Center of Mass        (m)     ', ( p%BldCG    (K), K=1,p%NumBl )


   ! Output additional masses:

WRITE (UnSu,FmtHead)  'Additional mass properties:'

WRITE (UnSu,FmtDat ) '    Tower-top Mass        (kg)    ', p%TwrTpMass
WRITE (UnSu,FmtDat ) '    Tower Mass            (kg)    ', p%TwrMass
!WRITE (UnSu,FmtDat ) '    Turbine Mass          (kg)    ', p%TurbMass
WRITE (UnSu,FmtDat ) '    Platform Mass         (kg)    ', p%PtfmMass
WRITE (UnSu,FmtDat ) '    Mass Incl. Platform   (kg)    ', p%TurbMass + p%PtfmMass !TotalMass !bjj TotalMass not used anywhere else so removed it


   ! Interpolated tower properties.

WRITE (UnSu,"(//,'Interpolated tower properties:',/)")
IF ( ( ADAMSPrep == 2 ) .OR. ( ADAMSPrep == 3 ) )  THEN  ! An ADAMS model will be created; thus, print out all the cols.

   WRITE (UnSu,FmtTxt)  'Node  TwFract   HNodes  DHNodes  TMassDen    FAStiff    SSStiff'// &
                        '    GJStiff    EAStiff    FAIner    SSIner  FAcgOff  SScgOff'
   WRITE (UnSu,FmtTxt)  ' (-)      (-)      (m)      (m)    (kg/m)     (Nm^2)     (Nm^2)'// &
                        '     (Nm^2)        (N)    (kg m)    (kg m)      (m)      (m)'

   DO I=1,p%TwrNodes
      WRITE(UnSu,'(I4,3F9.3,F10.3,4ES11.3,2F10.3,2F9.3)')  I, p%HNodesNorm(I), p%HNodes(I)+p%TwrRBHt, p%DHNodes(I), p%MassT(I), &
                                                              p%StiffTFA(I), p%StiffTSS(I), p%StiffTGJ(I), p%StiffTEA(I),       &
                                                              p%InerTFA(I), p%InerTSS(I), p%cgOffTFA(I), p%cgOffTSS(I)
   ENDDO ! I

ELSE                                                     ! Only FAST will be run; thus, only print out the necessary cols.

   WRITE (UnSu,FmtTxt)  'Node  TwFract   HNodes  DHNodes  TMassDen    FAStiff    SSStiff'
   WRITE (UnSu,FmtTxt)  ' (-)      (-)      (m)      (m)    (kg/m)     (Nm^2)     (Nm^2)'

   DO I=1,p%TwrNodes
      WRITE(UnSu,'(I4,3F9.3,F10.3,2ES11.3)')  I, p%HNodesNorm(I), p%HNodes(I) + p%TwrRBHt, p%DHNodes(I), p%MassT(I), &
                                                 p%StiffTFA(I), p%StiffTSS(I)
   ENDDO ! I

ENDIF


   ! Interpolated blade properties.

DO K=1,p%NumBl

   WRITE (UnSu,'(//,A,I1,A,/)')  'Interpolated blade ', K, ' properties:'
   IF ( ( ADAMSPrep == 2 ) .OR. ( ADAMSPrep == 3 ) )  THEN  ! An ADAMS model will be created; thus, print out all the cols.

      WRITE (UnSu,FmtTxt)  'Node  BlFract   RNodes  DRNodes  AeroCent  StrcTwst  BMassDen    FlpStff    EdgStff'//       &
                           '     GJStff     EAStff    Alpha   FlpIner   EdgIner PrecrvRef PreswpRef  FlpcgOf  EdgcgOf'// &
                           '  FlpEAOf  EdgEAOf'
      WRITE (UnSu,FmtTxt)  ' (-)      (-)      (m)      (m)       (-)     (deg)    (kg/m)     (Nm^2)     (Nm^2)'//       &
                           '     (Nm^2)     (Nm^2)      (-)    (kg m)    (kg m)       (m)       (m)      (m)      (m)'// &
                           '      (m)      (m)'

      DO I=1,p%BldNodes

         WRITE(UnSu,'(I4,3F9.3,3F10.3,4ES11.3,F9.3,4F10.3,4F9.3)')  I, p%RNodesNorm(I),  p%RNodes(I) + p%HubRad, p%DRNodes(I), &
                                                                       p%AeroCent(K,I),  p%ThetaS(K,I)*R2D,      p%MassB(K,I), &
                                                                       p%StiffBF(K,I),   p%StiffBE(K,I),                       &
                                                                       p%StiffBGJ(K,I),  p%StiffBEA(K,I),                      &
                                                                       p%BAlpha(K,I),    p%InerBFlp(K,I), p%InerBEdg(K,I),     &
                                                                       p%RefAxisxb(K,I), p%RefAxisyb(K,I),                     &
                                                                       p%cgOffBFlp(K,I), p%cgOffBEdg(K,I),                     &
                                                                       p%EAOffBFlp(K,I), p%EAOffBEdg(K,I)
      ENDDO ! I

   ELSE                                                     ! Only FAST will be run; thus, only print out the necessary cols.

      WRITE (UnSu,FmtTxt)  'Node  BlFract   RNodes  DRNodes  AeroCent  StrcTwst  BMassDen    FlpStff    EdgStff'
      WRITE (UnSu,FmtTxt)  ' (-)      (-)      (m)      (m)       (-)     (deg)    (kg/m)     (Nm^2)     (Nm^2)'

      DO I=1,p%BldNodes
         WRITE(UnSu,'(I4,3F9.3,3F10.3,2ES11.3)')  I, p%RNodesNorm(I), p%RNodes(I) + p%HubRad, p%DRNodes(I), p%AeroCent(K,I), &
                                                     p%ThetaS(K,I)*R2D, p%MassB(K,I), p%StiffBF(K,I), p%StiffBE(K,I)
      ENDDO ! I

   ENDIF

ENDDO ! K


!bjj: put this in the summary file, no? was in echo file   
   OutPFmt = '( I4, 3X,A '//TRIM(Num2LStr(OutStrLen))//',1 X, A'//TRIM(Num2LStr(OutStrLen))//' )'
   WRITE (UnSu,'(//,A,/)')  'Requested Outputs:'
   WRITE (UnSu,"(/, '  Col  Parameter  Units', /, '  ---  ---------  -----')")
   DO I = 0,p%NumOuts
      WRITE (UnSu,OutPFmt)  I, p%OutParam(I)%Name, p%OutParam(I)%Units
   END DO             
! END IF ! Echo


RETURN
END SUBROUTINE PrintSum
!=======================================================================
SUBROUTINE RunTimes()


   ! This routine displays a message that gives that status of the simulation
   !  and the predicted end time of day.


USE                             General
USE                             SimCont

IMPLICIT                        NONE


   ! Local variables.

REAL(ReKi)                   :: ClckTime                                        ! Elapsed clock time for the simulation phase of the run.
REAL(ReKi)                   :: Factor                                          ! Ratio of seconds to a specified time period.
REAL(ReKi)                   :: TRatio                                          ! Ration of simulation time to elapsed clock time.

REAL(4)                      :: UsrTime                                         ! User CPU time for entire run.

INTEGER(4)                   :: EndTimes (8)                                    ! An array holding the ending clock time of the simulation.

CHARACTER( 8)                :: TimePer


   ! Get the end times to compare with start times.

CALL DATE_AND_TIME ( VALUES=EndTimes )
CALL CPU_TIME ( UsrTime )


   ! Calculate the elapsed wall-clock time in seconds.
   
!bjj: I think this calculation will be wrong at certain times (e.g. if it's near midnight on the last day of the month), but to my knowledge, no one has complained...

ClckTime =  0.001*( EndTimes(8) - StrtTime(8) ) + ( EndTimes(7) - StrtTime(7) ) + 60.0*( EndTimes(6) - StrtTime(6) ) &
         + 3600.0*( EndTimes(5) - StrtTime(5) ) + 86400.0*( EndTimes(3) - StrtTime(3) )  
         


   ! Calculate CPU times.

UsrTime  = UsrTime - UsrTime1


IF ( UsrTime /= 0.0 )  THEN

   TRatio = ZTime / UsrTime

   IF     ( UsrTime > 86400.0 )  THEN
      Factor = 1.0/86400.0
      TimePer = ' days'
   ELSEIF ( UsrTime >  3600.0 )  THEN
      Factor = 1.0/3600.0
      TimePer = ' hours'
   ELSEIF ( UsrTime >    60.0 )  THEN
      Factor = 1.0/60.0
      TimePer = ' minutes'
   ELSE
      Factor = 1.0
      TimePer = ' seconds'
   ENDIF

   CALL WrOver( '                                                                                   ' )
   CALL WrScr1( ' Total Real Time:       '//TRIM( Num2LStr( Factor*ClckTime      ) )//TRIM( TimePer ) )
   CALL WrScr ( ' Total CPU Time:        '//TRIM( Num2LStr( Factor*UsrTime       ) )//TRIM( TimePer ) )
   CALL WrScr ( ' Simulated Time:        '//TRIM( Num2LStr( Factor*REAL( ZTime ) ) )//TRIM( TimePer ) )
   CALL WrScr ( ' Time Ratio (Sim/CPU):  '//TRIM( Num2LStr( TRatio ) ) )


ENDIF


RETURN
END SUBROUTINE RunTimes
!=======================================================================
SUBROUTINE SimStatus


   ! This routine displays a message that gives that status of the simulation
   !  and the predicted end time of day.


USE                             SimCont

IMPLICIT                        NONE


   ! Local variables.

REAL(ReKi)                   :: CurrTime                                        ! Current time in seconds past midnight.
REAL(ReKi)                   :: DeltTime                                        ! The amount of time elapsed since the lst call.
REAL(ReKi)                   :: EndTime                                         ! Approximate time of day when simulation will complete.
REAL(ReKi)                   :: InSecHr   = 1.0/3600.0                          ! Inverse of the number of seconds in an hour.
REAL(ReKi)                   :: InSecMn   = 1.0/  60.0                          ! Inverse of the number of seconds in a minute.
REAL(ReKi), SAVE             :: PrevTime                                        ! Previous time in seconds past midnight.
REAL(ReKi)                   :: TimeLeft                                        ! Approximate clock time remaining before simulation completes.

INTEGER(4)                   :: EndHour                                         ! The hour when the simulations is expected to complete.
INTEGER(4)                   :: EndMin                                          ! The minute when the simulations is expected to complete.
INTEGER(4)                   :: EndSec                                          ! The second when the simulations is expected to complete.
INTEGER(4)                   :: TimeAry  (8)                                    ! An array containing the elements of the start time.

LOGICAL,    SAVE             :: FirstPas = .TRUE.                               ! When true, indicates we're on the first pass of sim.

CHARACTER( 8)                :: ETimeStr                                        ! String containing the end time.



   ! On the first pass, get the start time.

IF ( FirstPas )  THEN

   CALL DATE_AND_TIME ( Values=TimeAry )

   PrevTime = 60.0*( 60.0*TimeAry(5) + TimeAry(6) ) + TimeAry(7) + 0.001*TimeAry(8)

   FirstPas = .FALSE.

   RETURN

ENDIF


   ! How many seconds past midnight?

CALL DATE_AND_TIME ( Values=TimeAry )

CurrTime = 60.0*( 60.0*TimeAry(5) + TimeAry(6) ) + TimeAry(7) + 0.001*TimeAry(8)


   ! Calculate elapsed time.

DeltTime = CurrTime - PrevTime


   ! We may have passed midnight since the last revoultion.  We will assume
   !  that one second of simulation time doesn't take more than a day.

IF ( CurrTime < PrevTime )  THEN
   DeltTime = DeltTime + 86400.0
ENDIF


   ! Estimate the end time in hours, minutes, and seconds.

TimeLeft = DeltTime*( TMax - ZTime )                                            ! Simulation time between calls is presumed to be 1 second.
EndTime  = MOD( CurrTime+TimeLeft, 86400.0 )
EndHour  =  INT(   EndTime*InSecHr )
EndMin   =  INT( ( EndTime - REAL( 3600*EndHour ) )*InSecMn )
EndSec   = NINT(   EndTime - REAL( 3600*EndHour + 60*EndMin ) )

WRITE (ETimeStr,"(I2.2,2(':',I2.2))")  EndHour, EndMin, EndSec

CALL WrOver ( ' Timestep: '//TRIM( Num2LStr( NINT( ZTime ) ) )//' of '//TRIM( Num2LStr( TMax ) )// &
              ' seconds.  Estimated final completion at '//ETimeStr//'.'                             )


   ! Let's save this time as the previous time.

PrevTime = CurrTime


RETURN
END SUBROUTINE SimStatus
!=======================================================================
SUBROUTINE WrOutHdr(p_ED)


   ! This routine generates the header for the primary output file.


USE                             General
USE                             AeroDyn
USE                             SimCont, ONLY: TMax,DT !BJJ perhaps we should do this a better way


IMPLICIT                        NONE


   ! passed variables
TYPE(ED_ParameterType), INTENT(IN)  :: p_ED                                 ! Parameters of the structural dynamics module


   ! Local variables.
INTEGER                      :: ErrStat

INTEGER(4)                   :: I                                               ! A generic index for DO loops.



FileDesc = 'These predictions were generated by '//TRIM(ProgName)//' '//TRIM(ProgVer)//' on '//CurDate()//' at '//CurTime()//'.'


   ! Open the output file(s):

IF (WrTxtOutFile) THEN      

   CALL OpenFOutFile ( UnOu, TRIM(RootName)//'.out' )
                      
      ! Add some file information:

   WRITE (UnOu,'(/,A)') TRIM(FileDesc)  
   WRITE (UnOu,'(  A)')  'The aerodynamic calculations were made by '//TRIM(GetNVD(AD_Prog))//'.'
   WRITE (UnOu,'(/,1X,A,/)')  TRIM( FTitle )


      ! Write the names of the output parameters:
      
         ! names
   CALL WrFileNR ( UnOu, TRIM( p_ED%OutParam(0)%Name ) )
   DO I=1,p_ED%NumOuts
      CALL WrFileNR ( UnOu, p_ED%Delim//TRIM( p_ED%OutParam(I)%Name ) )
   ENDDO ! I

         ! units
   WRITE (UnOu,'()')
   CALL WrFileNR ( UnOu, TRIM( p_ED%OutParam(0)%Units ) )
   DO I=1,p_ED%NumOuts
      CALL WrFileNR ( UnOu, p_ED%Delim//TRIM( p_ED%OutParam(I)%Units ) )
   ENDDO ! I
      
   WRITE (UnOu,'()')

END IF   

IF (WrBinOutFile) THEN      
   
   NOutSteps = NINT ( (TMax - TStart) / (DT*DecFact) ) + 1
   IF (.NOT. ALLOCATED(AllOutData) ) THEN
      ALLOCATE ( AllOutData(1:p_ED%NumOuts,NOutSteps) , STAT=ErrStat )
      IF ( ErrStat /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the AllOutData array.' )
      END IF
   END IF
   
   IF ( ALLOCATED(TimeData) ) DEALLOCATE(TimeData)
   
   IF ( OutputFileFmtID == FileFmtID_WithoutTime ) THEN
   
      ALLOCATE ( TimeData(2) , STAT=ErrStat )
      IF ( ErrStat /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the TimeData array.' )
      END IF

      TimeData(1) = 0                  ! This is the first output time, which we will set later
      TimeData(2) = DT*DecFact         ! This
   
   ELSE
   
      ALLOCATE ( TimeData( SIZE(AllOutData) ) , STAT=ErrStat )
      IF ( ErrStat /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the TimeData array.' )
      END IF
   
   END IF
   
   CurrOutStep = 0
   FileDesc = TRIM(FileDesc)//' The aerodynamic calculations were made by '//TRIM(GetNVD(AD_Prog))//'.'
   
END IF



RETURN
END SUBROUTINE WrOutHdr
!=======================================================================
SUBROUTINE WrOutput(p_ED,y_ED)


   ! This routine writes output to the primary output file.


USE                             General

USE                             AeroGenSubs, ONLY: ElemOut

IMPLICIT                        NONE


   ! Passed variables
TYPE(ED_ParameterType),INTENT(IN) :: p_ED                                    ! Parameters of the structural dynamics module
TYPE(ED_OutputType),   INTENT(IN) :: y_ED                                    ! System outputs of the structural dynamics module
   

   ! Local variables.

INTEGER(IntKi)                   :: I                                            ! A generic index for DO loops.
CHARACTER(200)                   :: Frmt                                         ! A string to hold a format specifier.



IF (WrTxtOutFile) THEN      

      ! Write normal tabular output:

   Frmt = '(F8.3,'//TRIM(Num2LStr(p_ED%NumOuts))//'(:,A,'//TRIM( p_ED%OutFmt )//'))'

   WRITE(UnOu,Frmt)  y_ED%WriteOutput(Time), ( p_ED%Delim, y_ED%WriteOutput(I), I=1,p_ED%NumOuts )
   
END IF

IF (WrBinOutFile) THEN               
   
      ! Write data to array for binary output file
      
   IF ( CurrOutStep == NOutSteps ) THEN
      CALL ProgWarn( 'Not all data could be written to the binary output file.' )
   ELSE      
      CurrOutStep = CurrOutStep + 1
      AllOutData(:,CurrOutStep) = y_ED%WriteOutput(1:p_ED%NumOuts)
      
      IF ( CurrOutStep == 1_IntKi .OR. OutputFileFmtID == FileFmtID_WithTime ) THEN
         TimeData(CurrOutStep) = y_ED%WriteOutput(Time)   ! Time associated with these outputs (bjj: fix this when we convert time to double precision)         
      END IF
               
   END IF
   
END IF


   ! Generate AeroDyn's element data if desired:

CALL ElemOut()


RETURN
END SUBROUTINE WrOutput
!=======================================================================
END MODULE FAST_IO_Subs
