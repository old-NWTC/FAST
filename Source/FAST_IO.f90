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
SUBROUTINE GetVersion(ProgVer)
! This routine returns a string describing the glue code and some of the compilation options we're using.
!..................................................................................................................................

   IMPLICIT                        NONE


   ! Passed Variables:

   CHARACTER(1024), INTENT(OUT)  :: ProgVer                                           ! String containing a description of the as-compiled precision.


   
   ProgVer = TRIM(GetNVD(FAST_Ver))//' (compiled using '

   ! determine precision
      IF ( ReKi == SiKi )  THEN     ! Single precision
         ProgVer = TRIM(ProgVer)//'SINGLE'
      ELSEIF ( ReKi == R8Ki )  THEN ! Double precision
         ProgVer = TRIM(ProgVer)//'DOUBLE'
      ELSE                          ! Unknown precision
         ProgVer = TRIM(ProgVer)//'UNKNOWN'
      ENDIF

   ProgVer = TRIM(ProgVer)//' precision'


   ! determine if we've done some other modifications
      IF ( Cmpl4SFun )  THEN     ! FAST has been compiled as an S-Function for Simulink
         ProgVer = TRIM(ProgVer)//' as S-Function for Simulink'  
      ELSEIF ( Cmpl4LV )  THEN     ! FAST has been compiled as a DLL for Labview
         ProgVer = TRIM(ProgVer)//' as a DLL for Labview'
      ENDIF

      !IF ( OC3HywindMods ) THEN
      !   ProgVer = TRIM(ProgVer)//' with OC3 Hywind Modifications'
      !END IF
            
   ProgVer = TRIM(ProgVer)//')'
   

   RETURN
END SUBROUTINE GetVersion
!====================================================================================================

SUBROUTINE AeroInput(InputFile, p_ED, p_FAST)
! This subroutine sets up the information needed to initialize AeroDyn, then initializes AeroDyn
!----------------------------------------------------------------------------------------------------

   USE                     AeroDyn_Types !,   ONLY: ADAeroMarkers, ADCurrentOutputs, ADIntrfaceOptions, ADFirstLoop, Prev_Aero_t   
   USE                     AeroDyn

   IMPLICIT NONE

   ! Passed variables:
   TYPE(ED_ParameterType),  INTENT(IN)  :: p_ED             ! The parameters of the structural dynamics module
   TYPE(FAST_ParameterType),INTENT(IN)  :: p_FAST           ! The parameters of the glue code
   
      ! Local variables

   TYPE(AD_InitOptions)       :: ADOptions                  ! Options for AeroDyn
   INTEGER                    :: NumADBldNodes              ! Number of blade nodes in AeroDyn
   REAL(ReKi)                 :: AD_RefHt
   
   INTEGER                    :: ErrStat


      ! Set up the AeroDyn parameters
   ADOptions%ADInputFile      = p_FAST%ADFile
   ADOptions%OutRootName      = p_FAST%RootName
   ADOptions%WrSumFile        = p_FAST%SumPrint
   
   
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

   
      ! Check that the hub-heights are Set up other parameters only if we need them

   IF ( CompAero )  THEN

      ! Let's see if the hub-height in AeroDyn and FAST are within 10%:
      AD_RefHt = AD_GetConstant('RefHt', ErrStat)

      IF ( ABS( p_ED%FASTHH - AD_RefHt ) > 0.1*( p_ED%FASTHH ) )  THEN  !bjj: I believe that this should not be done in the future

         CALL ProgWarn( ' The ElastoDyn hub height ('//TRIM(Num2LStr( p_ED%FASTHH ))//') and AeroDyn input'// &
                       ' reference hub height ('//TRIM(Num2LStr(AD_RefHt))//') differ by more than 10%.' )
      ENDIF

   ENDIF   
   
   
   RETURN
END SUBROUTINE AeroInput
!====================================================================================================
SUBROUTINE FAST_Init( p, ErrStat, ErrMsg, InFile  )
! This subroutine checks for command-line arguments, gets the root name of the input files 
! (including full path name), and creates the names of the output files.
!..................................................................................................................................

      IMPLICIT                        NONE

   ! Passed variables
   
   TYPE(FAST_ParameterType), INTENT(INOUT)         :: p                 ! The parameter data for the FAST (glue-code) simulation   
   INTEGER(IntKi),           INTENT(OUT)           :: ErrStat           ! Error status
   CHARACTER(*),             INTENT(OUT)           :: ErrMsg            ! Error message
   CHARACTER(*),             INTENT(IN), OPTIONAL  :: InFile            ! A CHARACTER string containing the name of the primary FAST input file (if not present, we'll get it from the command line)

      ! Local variables

   REAL(DbKi)                   :: TmpTime                              ! A temporary variable for error checking      
   INTEGER                      :: Stat                                 ! The status of the call to GET_CWD
   CHARACTER(1024)              :: DirName                              ! A CHARACTER string containing the path of the current working directory
   CHARACTER(1024)              :: InputFile                            ! A CHARACTER string containing the name of the primary FAST input file
   CHARACTER(1024)              :: CompiledVer                          ! A string describing the FAST version as well as some of the compile options we're using

         
      ! Initialize some variables
   ErrStat = ErrID_None
   ErrMsg = ''
         
   

      ! Tell our nice users what they're running
   CALL GetVersion( CompiledVer )
   CALL WrScr( CompiledVer )
      
   
      ! Get the name of the input file from the command line if it isn't an input to this routine
   
   IF ( PRESENT(InFile) ) THEN 
      InputFile = InFile
   ELSE ! get it from the command line
      CALL CheckArgs( InputFile )
   END IF
   
      ! Determine the root name of the primary file (will be used for output files)
   CALL GetRoot( InputFile, p%OutFileRoot, ErrStat )
   IF ( ErrStat >= AbortErrLev ) RETURN
   IF ( Cmpl4SFun )  p%OutFileRoot = TRIM( p%OutFileRoot )//'_SFunc'


   ! Let's create a root file name variable, DirRoot, which includes the full path to the current working directory.
   ! bjj: not really sure this is necessary... why can't we use a relative path?

   p%DirRoot  = p%OutFileRoot

   IF ( PathIsRelative(p%DirRoot) ) THEN
      CALL Get_CWD  ( DirName, Stat )
      IF (Stat /= 0) THEN
         CALL SetErrors( ErrID_Warn, 'Error retreiving current working directory. DirRoot will contain a relative path.' )
         IF ( ErrStat >= AbortErrLev ) RETURN
      ELSE        
         p%DirRoot = TRIM( DirName )//PathSep//TRIM( p%DirRoot )
      END IF
   END IF   


      ! Read the primary file for the glue code:
   CALL FAST_ReadPrimaryFile( InputFile, p, ErrStat, ErrMsg )

   
      ! Do some error checking:
   IF ( p%TMax < 0.0_DbKi  )  THEN
      CALL SetErrors( ErrID_Fatal, 'TMax must not be a negative number.' )
      IF ( ErrStat >= AbortErrLev ) RETURN
   ELSE IF ( p%TMax < p%TStart )  THEN
      CALL SetErrors( ErrID_Fatal, 'TMax must not be less than TStart.' )
      RETURN
   END IF
         
   IF ( p%DT <= 0.0_DbKi )  THEN
      CALL SetErrors( ErrID_Fatal, 'DT must be greater than 0.' )
      IF ( ErrStat >= AbortErrLev ) RETURN
   ELSE ! Test DT and TMax to ensure numerical stability -- HINT: see the use of OnePlusEps
      TmpTime = p%TMax*EPSILON(p%DT)
      IF ( p%DT <= TmpTime ) THEN
         CALL SetErrors( ErrID_Fatal, 'DT must be greater than '//TRIM ( Num2LStr( TmpTime ) )//' seconds.' )
         RETURN
      END IF         
   END IF
   
   IF ( p%WrTxtOutFile .AND. ( p%TMax > 9999.999_DbKi ) )  THEN
      CALL ProgAbort ( ' TMax must not exceed 9999.999 seconds with text tabular (time-marching) output files.' )
   END IF   

   IF ( p%TStart   <  0.0_DbKi ) CALL SetErrors( ErrID_Fatal, 'TStart must not be less than 0 seconds.' )   
   IF ( p%SttsTime <= 0.0_DbKi ) CALL SetErrors( ErrID_Fatal, 'SttsTime must be greater than 0 seconds.' )
      
   
!.....   
p%SumPrint = .TRUE.  !are we going to keep this? (used now for AeroDyn)
!.....   


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
END SUBROUTINE FAST_Init
!=======================================================================
SUBROUTINE FAST_ReadPrimaryFile( InputFile, p, ErrStat, ErrMsg )
! This routine reads in the primary FAST input file, does some validation, and places the values it reads in the 
!   parameter structure (p). It prints to an echo file if requested.
!..................................................................................................................................


   IMPLICIT                        NONE

      ! Passed variables
   TYPE(FAST_ParameterType), INTENT(INOUT) :: p                               ! The parameter data for the FAST (glue-code) simulation
   INTEGER(IntKi),           INTENT(OUT)   :: ErrStat                         ! Error status

   CHARACTER(*),             INTENT(IN)    :: InputFile                       ! Name of the file containing the primary input data
   CHARACTER(*),             INTENT(OUT)   :: ErrMsg                          ! Error message
   
      ! Local variables:
   INTEGER(IntKi)                :: I                                         ! loop counter
   INTEGER(IntKi)                :: NumOuts                                   ! Number of output channel names read from the file 
   INTEGER(IntKi)                :: UnIn                                      ! Unit number for reading file
   INTEGER(IntKi)                :: UnEc                                      ! I/O unit for echo file. If > 0, file is open for writing.
     
   INTEGER(IntKi)                :: ErrStat2                                  ! Temporary Error status
   INTEGER(IntKi)                :: FmtWidth                                  ! width of the field returned by the specified OutFmt
   INTEGER(IntKi)                :: OutFileFmt                                ! An integer that indicates what kind of tabular output should be generated (1=text, 2=binary, 3=both)
   LOGICAL                       :: Echo                                      ! Determines if an echo file should be written
   LOGICAL                       :: TabDelim                                  ! Determines if text output should be delimited by tabs (true) or space (false)
   CHARACTER(LEN(ErrMsg))        :: ErrMsg2                                   ! Temporary Error message
   CHARACTER(1024)               :: PriPath                                   ! Path name of the primary file
   CHARACTER(1024)               :: FTitle                                    ! "File Title": the 2nd line of the input file, which contains a description of its contents

   
   
      ! Initialize some variables:
   UnEc = -1
   Echo = .FALSE.                        ! Don't echo until we've read the "Echo" flag
   CALL GetPath( InputFile, PriPath )    ! Input files will be relative to the path where the primary input file is located.

   
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
   
         ! Echo - Echo input data to <RootName>.ech (flag):
      CALL ReadVar( UnIn, InputFile, Echo, "Echo", "Echo input data to <RootName>.ech (flag)", ErrStat2, ErrMsg2, UnEc)
         CALL CheckError( ErrStat2, ErrMsg2 )
         IF ( ErrStat >= AbortErrLev ) RETURN
   
   
      IF (.NOT. Echo .OR. I > 1) EXIT !exit this loop
   
         ! Otherwise, open the echo file, then rewind the input file and echo everything we've read
      
      I = I + 1         ! make sure we do this only once (increment counter that says how many times we've read this file)
   
      CALL OpenEcho ( UnEc, TRIM(OutFileRoot)//'.ech', ErrStat2, ErrMsg2, SrvD_Ver )
         CALL CheckError( ErrStat2, ErrMsg2 )
         IF ( ErrStat >= AbortErrLev ) RETURN
   
      CALL WrScr( ' Heading of the '//TRIM(FAST_Ver%Name)//' input file: '//TRIM( FTitle ) )      
      IF ( UnEc > 0 )  WRITE (UnEc,'(//,A,/)')  'Data from '//TRIM(FAST_Ver%Name)//' primary input file "'//TRIM( InputFile )//'":'
   
      REWIND( UnIn, IOSTAT=ErrStat2 )   
         CALL CheckError( ErrID_Fatal, 'Error rewinding file "'//TRIM(InputFile)//'".' )
         IF ( ErrStat >= AbortErrLev ) RETURN
      
   END DO    
                                             

      ! TMax - Total run time (s):
   CALL ReadVar( UnIn, InputFile, p%TMax, "TMax", "Total run time (s)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! DT - Recommended module time step (s):
   CALL ReadVar( UnIn, InputFile, p%DT, "DT", "Recommended module time step (s)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

   !---------------------- FEATURE FLAGS -------------------------------------------
   CALL ReadCom( UnIn, InputFile, 'Section Header: Feature Flags', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
      ! CompAero - Compute aerodynamic forces (flag):
   CALL ReadVar( UnIn, InputFile, p%CompAero, "CompAero", "Compute aerodynamic forces (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! CompServo - Compute servodynamics (flag):
   CALL ReadVar( UnIn, InputFile, p%CompServo, "CompServo", "Compute servodynamics (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! CompHydro - Compute hydrodynamics forces (flag):
   CALL ReadVar( UnIn, InputFile, p%CompHydro, "CompHydro", "Compute hydrodynamics forces (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! CompSub - Compute sub-structural dynamics (flag):
   CALL ReadVar( UnIn, InputFile, p%CompSub, "CompSub", "Compute sub-structural dynamics (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! CompUserPtfmLd - Compute additional platform loading {false: none, true: user-defined from routine UserPtfmLd} (flag):
   CALL ReadVar( UnIn, InputFile, p%CompUserPtfmLd, "CompUserPtfmLd", "Compute additional platform loading (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! CompUserTwrLd - Compute additional tower loading {false: none, true: user-defined from routine UserTwrLd} (flag):
   CALL ReadVar( UnIn, InputFile, p%CompUserTwrLd, "CompUserTwrLd", "Compute additional tower loading (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

   !---------------------- INPUT FILES ---------------------------------------------
   CALL ReadCom( UnIn, InputFile, 'Section Header: Input Files', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN      
      
      ! EDFile - Name of file containing ElastoDyn input parameters (-):
   CALL ReadVar( UnIn, InputFile, p%EDFile, "EDFile", "Name of file containing ElastoDyn input parameters (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   IF ( PathIsRelative( p%EDFile ) ) p%EDFile = TRIM(PriPath)//TRIM(p%EDFile)         

      ! ADFile - Name of file containing AeroDyn input parameters (-):
   CALL ReadVar( UnIn, InputFile, p%ADFile, "ADFile", "Name of file containing AeroDyn input parameters (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   IF ( PathIsRelative( p%ADFile ) ) p%ADFile = TRIM(PriPath)//TRIM(p%ADFile)         

      ! SrvDFile - Name of file containing ServoDyn input parameters (-):
   CALL ReadVar( UnIn, InputFile, p%SrvDFile, "SrvDFile", "Name of file containing ServoDyn input parameters (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   IF ( PathIsRelative( p%SrvDFile ) ) p%SrvDFile = TRIM(PriPath)//TRIM(p%SrvDFile)         

      ! HDFile - Name of file containing HydroDyn input parameters (-):
   CALL ReadVar( UnIn, InputFile, p%HDFile, "HDFile", "Name of file containing HydroDyn input parameters (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   IF ( PathIsRelative( p%HDFile ) ) p%HDFile = TRIM(PriPath)//TRIM(p%HDFile)         

      ! SDFile - Name of file containing SubDyn input parameters (-):
   CALL ReadVar( UnIn, InputFile, p%SDFile, "SDFile", "Name of file containing SubDyn input parameters (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   IF ( PathIsRelative( p%SDFile ) ) p%SDFile = TRIM(PriPath)//TRIM(p%SDFile)         

   !---------------------- OUTPUT --------------------------------------------------         
   CALL ReadCom( UnIn, InputFile, 'Section Header: Output', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
      ! SttsTime - Amount of time between screen status messages (s):
   CALL ReadVar( UnIn, InputFile, p%SttsTime, "SttsTime", "Amount of time between screen status messages (s)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN            

      ! DT_Out - Time step for tabular output (s):
   CALL ReadVar( UnIn, InputFile, p%DT_Out, "DT_Out", "Time step for tabular output (s)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! TStart - Time to begin tabular output (s):
   CALL ReadVar( UnIn, InputFile, p%TStart, "TStart", "Time to begin tabular output (s)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
      ! OutFileFmt - Format for tabular (time-marching) output file(s) (1: text file [<RootName>.out], 2: binary file [<RootName>.outb], 3: both) (-):
   CALL ReadVar( UnIn, InputFile, OutFileFmt, "OutFileFmt", "Format for tabular (time-marching) output file(s) (1: text file [<RootName>.out], 2: binary file [<RootName>.outb], 3: both) (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
      SELECT CASE (OutFileFmt)
         CASE (1_IntKi)
            p%WrBinOutFile = .FALSE.
            p%WrTxtOutFile = .TRUE.
         CASE (2_IntKi)
            p%WrBinOutFile = .TRUE.
            p%WrTxtOutFile = .FALSE.
         CASE (3_IntKi)
            p%WrBinOutFile = .TRUE.
            p%WrTxtOutFile = .TRUE.
         CASE DEFAULT
           CALL CheckError( ErrID_Fatal, " FAST's OutFileFmt must be 1, 2, or 3." )
           RETURN
      END SELECT

      ! TabDelim - Use tab delimiters in text tabular output file? (flag):
   CALL ReadVar( UnIn, InputFile, TabDelim, "TabDelim", "Use tab delimiters in text tabular output file? (flag)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
      
      IF ( TabDelim ) THEN
         p%Delim = TAB
      ELSE
         p%Delim = ' '
      END IF
   

      ! OutFmt - Format used for text tabular output (except time).  Resulting field should be 10 characters. (-):
   CALL ReadVar( UnIn, InputFile, p%OutFmt, "OutFmt", "Format used for text tabular output (except time).  Resulting field should be 10 characters. (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN


      ! Check that InputFileData%OutFmt is a valid format specifier and will fit over the column headings
   CALL ChkRealFmtStr( p%OutFmt, 'OutFmt', FmtWidth, ErrStat2, ErrMsg2 )   
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
            
      IF ( FmtWidth /= OutStrLen ) CALL CheckError( ErrID_Warn, 'OutFmt produces a column width of '// &
            TRIM(Num2LStr(FmtWidth))//' instead of '//TRIM(Num2LStr(OutStrLen))//' characters.' )
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

         ErrMsg = TRIM(ErrMsg)//NewLine//' Error in FAST_ReadPrimaryFile: '//TRIM(Msg)
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
END SUBROUTINE FAST_ReadPrimaryFile      
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE FAST_Input( p, p_SrvD, OtherState, InputFileData, ErrStat, ErrMsg )


   ! This routine reads the input files and does some preliminary processing.


   ! FAST Modules:

USE                             DriveTrain
USE                             InitCond
USE                             NacelleYaw
USE                             TipBrakes
USE                             TurbCont

!USE HydroDyn_Types

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

INTEGER(4)                   :: I                                               ! A generic index for DO loops.
INTEGER(4)                   :: K                                               ! Index for blade number.
INTEGER(4)                   :: Sttus                                           ! Status of an attempted array allocation.



   ! Calculate some parameters that are not input directly.  Convert units if appropriate.

DT24      = DT/24.0_DbKi                                                        ! Time-step parameter needed for Solver().

!TBDepISp  = TBDepISp*RPM2RPS




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





RETURN
END SUBROUTINE FAST_Input
!=======================================================================
SUBROUTINE PrintSum( p, OtherState )


   ! This routine generates the summary file, which contains a regurgitation of
   !  the input data and interpolated flexible body data.

USE                             AeroDyn


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

INTEGER(4)                   :: UnSu      = 22                                  ! I/O unit number for the summary output file.

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

close(unsu)

RETURN
END SUBROUTINE PrintSum
!=======================================================================
SUBROUTINE RunTimes( StrtTime, UsrTime1, ZTime )


   ! This routine displays a message that gives that status of the simulation
   !  and the predicted end time of day.


   IMPLICIT                        NONE

      ! Passed variables

   INTEGER                      :: StrtTime (8)                                    ! Start time of simulation
   REAL                         :: UsrTime1                                        ! User CPU time for simulation initialization.
   REAL(DbKi)                   :: ZTime                                           ! The final simulation time (not necessarially TMax)

      ! Local variables

   REAL                         :: ClckTime                                        ! Elapsed clock time for the simulation phase of the run.
   REAL                         :: Factor                                          ! Ratio of seconds to a specified time period.
   REAL                         :: TRatio                                          ! Ration of simulation time to elapsed clock time.

   REAL                         :: UsrTime                                         ! User CPU time for entire run.
   INTEGER                      :: EndTimes (8)                                    ! An array holding the ending clock time of the simulation.

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


   IF ( .NOT. EqualRealNos( UsrTime, 0.0 ) )  THEN

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


USE                             AeroDyn


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
