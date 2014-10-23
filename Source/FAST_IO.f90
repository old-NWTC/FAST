!**********************************************************************************************************************************
! The FAST_Prog.f90, FAST_IO.f90, and FAST_Mods.f90 make up the FAST glue code in the FAST Modularization Framework.
!..................................................................................................................................
! LICENSING
! Copyright (C) 2013-2014  National Renewable Energy Laboratory
!
!    This file is part of FAST.
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
!
!**********************************************************************************************************************************
! File last committed: $Date$
! (File) Revision #: $Rev$
! URL: $HeadURL$
!**********************************************************************************************************************************
MODULE FAST_IO_Subs

   USE NWTC_Library
   USE NWTC_LAPACK

   USE FAST_Types

   USE AeroDyn_Types
   USE ElastoDyn_Types
   USE FEAMooring_Types
   USE HydroDyn_Types
   USE IceDyn_Types
   USE IceFloe_Types
   USE MAP_Types
   USE ServoDyn_Types
   USE SubDyn_Types
   
   USE ServoDyn, ONLY: Cmpl4SFun, Cmpl4LV
    

   IMPLICIT NONE

CONTAINS
!----------------------------------------------------------------------------------------------------------------------------------
FUNCTION GetVersion()
! This function returns a string describing the glue code and some of the compilation options we're using.
!..................................................................................................................................

   IMPLICIT                        NONE


   ! Passed Variables:

   CHARACTER(1024)  :: GetVersion                      ! String containing a description of the compiled precision.



   GetVersion = TRIM(GetNVD(FAST_Ver))//', compiled for '//TRIM(Num2LStr(BITS_IN_ADDR))//'-bit systems using'

   ! determine precision

      IF ( ReKi == SiKi )  THEN     ! Single precision
         GetVersion = TRIM(GetVersion)//' single'
      ELSEIF ( ReKi == R8Ki )  THEN ! Double precision
         GetVersion = TRIM(GetVersion)// ' double'
      ELSE                          ! Unknown precision
         GetVersion = TRIM(GetVersion)//' unknown'
      ENDIF

!   GetVersion = TRIM(GetVersion)//' precision with '//OS_Desc
   GetVersion = TRIM(GetVersion)//' precision'


   ! determine if we've done some other modifications
      IF ( Cmpl4SFun )  THEN     ! FAST has been compiled as an S-Function for Simulink
         GetVersion = TRIM(GetVersion)//' as S-Function for Simulink'
      ELSEIF ( Cmpl4LV )  THEN     ! FAST has been compiled as a DLL for Labview
         GetVersion = TRIM(GetVersion)//' as a DLL for LabVIEW'
      ENDIF

      !IF ( OC3HywindMods ) THEN
      !   GetVersion = TRIM(GetVersion)//' with OC3 Hywind Modifications'
      !END IF


   RETURN
END FUNCTION GetVersion
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE FAST_End( p_FAST, y_FAST, ErrStat, ErrMsg )
! This subroutine is called at program termination. It writes any additional output files,
! deallocates variables and closes files.
!----------------------------------------------------------------------------------------------------

   TYPE(FAST_ParameterType), INTENT(INOUT) :: p_FAST                    ! FAST Parameters
   TYPE(FAST_OutputType),    INTENT(INOUT) :: y_FAST                    ! FAST Output

   INTEGER(IntKi),           INTENT(OUT)   :: ErrStat                   ! Error status
   CHARACTER(*),             INTENT(OUT)   :: ErrMsg                    ! Message associated with errro status

      ! local variables
   CHARACTER(LEN(y_FAST%FileDescLines)*3)  :: FileDesc                  ! The description of the run, to be written in the binary output file


      ! Initialize some values

   ErrStat = ErrID_None
   ErrMsg  = ''

   !-------------------------------------------------------------------------------------------------
   ! Write the binary output file if requested
   !-------------------------------------------------------------------------------------------------

   IF (p_FAST%WrBinOutFile .AND. y_FAST%n_Out > 0) THEN

      FileDesc = TRIM(y_FAST%FileDescLines(1))//' '//TRIM(y_FAST%FileDescLines(2))//'; '//TRIM(y_FAST%FileDescLines(3))

      CALL WrBinFAST(TRIM(p_FAST%OutFileRoot)//'.outb', OutputFileFmtID, TRIM(FileDesc), &
            y_FAST%ChannelNames, y_FAST%ChannelUnits, y_FAST%TimeData, y_FAST%AllOutData(:,1:y_FAST%n_Out), ErrStat, ErrMsg)

      IF ( ErrStat /= ErrID_None ) CALL WrScr( TRIM(GetErrStr(ErrStat))//' when writing binary output file: '//TRIM(ErrMsg) )

   END IF


   !-------------------------------------------------------------------------------------------------
   ! Close the text tabular output file and summary file (if opened)
   !-------------------------------------------------------------------------------------------------
   IF (y_FAST%UnOu  > 0) THEN ! I/O unit number for the tabular output file
      CLOSE( y_FAST%UnOu )         
      y_FAST%UnOu = -1
   END IF
   
   IF (y_FAST%UnSum > 0) THEN ! I/O unit number for the tabular output file
      CLOSE( y_FAST%UnSum )        
      y_FAST%UnSum = -1
   END IF

   IF (y_FAST%UnGra > 0) THEN ! I/O unit number for the graphics output file
      CLOSE( y_FAST%UnGra )        
      y_FAST%UnGra = -1
   END IF
   
   !-------------------------------------------------------------------------------------------------
   ! Deallocate arrays
   !-------------------------------------------------------------------------------------------------

      ! Output
   IF ( ALLOCATED(y_FAST%AllOutData                  ) ) DEALLOCATE(y_FAST%AllOutData                  )
   IF ( ALLOCATED(y_FAST%TimeData                    ) ) DEALLOCATE(y_FAST%TimeData                    )
   IF ( ALLOCATED(y_FAST%ChannelNames                ) ) DEALLOCATE(y_FAST%ChannelNames                )
   IF ( ALLOCATED(y_FAST%ChannelUnits                ) ) DEALLOCATE(y_FAST%ChannelUnits                )


END SUBROUTINE FAST_End
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE FAST_Init( p, y_FAST, ErrStat, ErrMsg, InFile  )
! This subroutine checks for command-line arguments, gets the root name of the input files
! (including full path name), and creates the names of the output files.
!..................................................................................................................................

      IMPLICIT                        NONE

   ! Passed variables

   TYPE(FAST_ParameterType), INTENT(INOUT)         :: p                 ! The parameter data for the FAST (glue-code) simulation
   TYPE(FAST_OutputType),    INTENT(INOUT)         :: y_FAST            ! The output data for the FAST (glue-code) simulation
   INTEGER(IntKi),           INTENT(OUT)           :: ErrStat           ! Error status
   CHARACTER(*),             INTENT(OUT)           :: ErrMsg            ! Error message
   CHARACTER(*),             INTENT(IN), OPTIONAL  :: InFile            ! A CHARACTER string containing the name of the primary FAST input file (if not present, we'll get it from the command line)

      ! Local variables

   REAL(DbKi)                   :: TmpTime                              ! A temporary variable for error checking
   INTEGER                      :: i                                    ! loop counter
   INTEGER                      :: Stat                                 ! The status of the call to GET_CWD
   CHARACTER(1024)              :: DirName                              ! A CHARACTER string containing the path of the current working directory
   CHARACTER(1024)              :: LastArg                              ! A second command-line argument that will allow DWM module to be used in AeroDyn
   CHARACTER(1024)              :: InputFile                            ! A CHARACTER string containing the name of the primary FAST input file
   CHARACTER(1024)              :: CompiledVer                          ! A string describing the FAST version as well as some of the compile options we're using

      ! Initialize some variables
   ErrStat = ErrID_None
   ErrMsg = ''

      ! Display the copyright notice
   CALL DispCopyrightLicense( FAST_Ver )


      ! Tell our nice users what they're running
   CALL WrScr( ' Running '//GetVersion()//NewLine//' linked with '//TRIM( GetNVD( NWTC_Ver ))//NewLine )

   !...............................................................................................................................
   ! Get the name of the input file from the command line if it isn't an input to this routine
   ! and set the root name of the output files based on the input file name
   !...............................................................................................................................

   p%UseDWM = .FALSE.  ! by default, we're not going to use the DWM module
   
   IF ( PRESENT(InFile) ) THEN
      InputFile = InFile
      
   ELSE ! get it from the command line
      InputFile = ""  ! initialize to empty string to make sure it's input from the command line
      CALL CheckArgs( InputFile, Stat, LastArg )  ! if Stat /= ErrID_None, we'll ignore and deal with the problem when we try to read the input file
      
      IF (LEN_TRIM(InputFile) == 0) THEN ! no input file was specified
         CALL SetErrors( ErrID_Fatal, 'The required input file was not specified on the command line.') 
         CALL NWTC_DisplaySyntax( InputFile, 'FAST_Win32.exe' )
                  !bjj: FAST_Win32.exe isn't correct for x64 or other versions of the executable, but it IS the only full version we
                  ! distribute, and if people have compiled for other architectures, they should be able to figure that out, right?
         RETURN
      END IF            
      
      IF (LEN_TRIM(LastArg) > 0) THEN ! see if DWM was specified as the second option
         CALL Conv2UC( LastArg )
         IF ( TRIM(LastArg) == "DWM" ) THEN
            p%UseDWM    = .TRUE.
         END IF
      END IF
            
   END IF
   

   

      ! Determine the root name of the primary file (will be used for output files)
   CALL GetRoot( InputFile, p%OutFileRoot )
   !IF ( ErrStat >= AbortErrLev ) RETURN
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

   !...............................................................................................................................
   ! Initialize the module name/date/version info:
   !...............................................................................................................................

   DO i=1,NumModules
      y_FAST%Module_Ver(i)%Date = 'unknown date'
      y_FAST%Module_Ver(i)%Ver  = 'unknown version'
   END DO       
   y_FAST%Module_Ver( Module_IfW  )%Name = 'InflowWind'
   y_FAST%Module_Ver( Module_ED   )%Name = 'ElastoDyn'
   y_FAST%Module_Ver( Module_AD   )%Name = 'AeroDyn'
   y_FAST%Module_Ver( Module_SrvD )%Name = 'ServoDyn'
   y_FAST%Module_Ver( Module_HD   )%Name = 'HydroDyn'
   y_FAST%Module_Ver( Module_SD   )%Name = 'SubDyn'
   y_FAST%Module_Ver( Module_MAP  )%Name = 'MAP'
   y_FAST%Module_Ver( Module_FEAM )%Name = 'FEAMooring'
   y_FAST%Module_Ver( Module_IceF )%Name = 'IceFloe'
   y_FAST%Module_Ver( Module_IceD )%Name = 'IceDyn'
   y_FAST%Module_Ver( Module_BD   )%Name = 'BeamDyn'
         
   p%n_substeps = 1                                                ! number of substeps for between modules and global/FAST time
         
   !...............................................................................................................................
   ! Read the primary file for the glue code:
   !...............................................................................................................................
   CALL FAST_ReadPrimaryFile( InputFile, p, ErrStat, ErrMsg )
   IF ( ErrStat >= AbortErrLev ) RETURN


   p%KMax = 1                 ! after more checking, we may put this in the input file...
   !IF (p%CompIce == Module_IceF) p%KMax = 2
   p%SizeJac_ED_SD_HD = 0     ! initialize this vector to zero; after we figure out what size the ED/SD/HD meshes are, we'll fill this
   
   p%numIceLegs = 0           ! initialize number of support-structure legs in contact with ice (IceDyn will set this later)

      ! determine what kind of turbine we're modeling:
   IF ( p%CompHydro == Module_HD ) THEN
      IF ( p%CompSub == Module_SD ) THEN
         p%TurbineType = Type_Offshore_Fixed
      ELSE
         p%TurbineType = Type_Offshore_Floating
      END IF
   ELSE
      p%TurbineType = Type_LandBased
   END IF   
         
      ! figure out how many time steps we should go before writing screen output:      
    p%n_SttsTime = MAX( 1, NINT( p%SttsTime / p%DT ) )
    
    p%WrGraphics = .FALSE. !.TRUE.
   
   !...............................................................................................................................
   ! Do some error checking on the inputs (validation):
   !...............................................................................................................................
   IF ( p%TMax < 0.0_DbKi  )  THEN
      CALL SetErrors( ErrID_Fatal, 'TMax must not be a negative number.' )
   ELSE IF ( p%TMax < p%TStart )  THEN
      CALL SetErrors( ErrID_Fatal, 'TMax must not be less than TStart.' )
   END IF

   IF ( p%DT <= 0.0_DbKi )  THEN
      CALL SetErrors( ErrID_Fatal, 'DT must be greater than 0.' )
   ELSE ! Test DT and TMax to ensure numerical stability -- HINT: see the use of OnePlusEps
      TmpTime = p%TMax*EPSILON(p%DT)
      IF ( p%DT <= TmpTime ) THEN
         CALL SetErrors( ErrID_Fatal, 'DT must be greater than '//TRIM ( Num2LStr( TmpTime ) )//' seconds.' )
      END IF
   END IF

   IF ( p%WrTxtOutFile .AND. ( p%TMax > 9999.999_DbKi ) )  THEN
      CALL SetErrors( ErrID_Fatal, 'TMax must not exceed 9999.999 seconds with text tabular (time-marching) output files.' )
   END IF

   IF ( p%TStart   <  0.0_DbKi     ) CALL SetErrors( ErrID_Fatal, 'TStart must not be less than 0 seconds.' )
   IF ( p%SttsTime <= 0.0_DbKi     ) CALL SetErrors( ErrID_Fatal, 'SttsTime must be greater than 0 seconds.' )
   IF ( p%KMax     <   1_IntKi     ) CALL SetErrors( ErrID_Fatal, 'KMax must be greater than 0.' )
   
   IF (p%CompElast   == Module_Unknown) CALL SetErrors( ErrID_Fatal, 'CompElast must be 1 (ElastoDyn) or 2 (BeamDyn).' )   
   IF (p%CompAero    == Module_Unknown) CALL SetErrors( ErrID_Fatal, 'CompAero must be 0 (None) or 1 (AeroDyn).' )
   IF (p%CompServo   == Module_Unknown) CALL SetErrors( ErrID_Fatal, 'CompServo must be 0 (None) or 1 (ServoDyn).' )
   IF (p%CompHydro   == Module_Unknown) CALL SetErrors( ErrID_Fatal, 'CompHydro must be 0 (None) or 1 (HydroDyn).' )
   IF (p%CompSub     == Module_Unknown) CALL SetErrors( ErrID_Fatal, 'CompSub must be 0 (None) or 1 (SubDyn).' )
   IF (p%CompMooring == Module_Unknown) CALL SetErrors( ErrID_Fatal, 'CompMooring must be 0 (None), 1 (MAP), or 2 (FEAMooring).' )
   IF (p%CompIce     == Module_Unknown) CALL SetErrors( ErrID_Fatal, 'CompIce must be 0 (None) or 1 (IceFloe).' )
   IF (p%CompHydro /= Module_HD) THEN
      IF (p%CompMooring == Module_MAP) THEN
         CALL SetErrors( ErrID_Fatal, 'HydroDyn must be used when MAP is used. Set CompHydro > 0 or CompMooring = 0 in the FAST input file.' )
      ELSEIF (p%CompMooring == Module_FEAM) THEN
         CALL SetErrors( ErrID_Fatal, 'HydroDyn must be used when FEAMooring is used. Set CompHydro > 0 or CompMooring = 0 in the FAST input file.' )
      END IF
   END IF
   
   IF (p%CompIce == Module_IceF) THEN
      IF (p%CompSub   /= Module_SD) CALL SetErrors( ErrID_Fatal, 'SubDyn must be used when IceFloe is used. Set CompSub > 0 or CompIce = 0 in the FAST input file.' )
      IF (p%CompHydro /= Module_HD) CALL SetErrors( ErrID_Fatal, 'HydroDyn must be used when IceFloe is used. Set CompHydro > 0 or CompIce = 0 in the FAST input file.' )
   ELSEIF (p%CompIce == Module_IceD) THEN
      IF (p%CompSub   /= Module_SD) CALL SetErrors( ErrID_Fatal, 'SubDyn must be used when IceDyn is used. Set CompSub > 0 or CompIce = 0 in the FAST input file.' )
      IF (p%CompHydro /= Module_HD) CALL SetErrors( ErrID_Fatal, 'HydroDyn must be used when IceDyn is used. Set CompHydro > 0 or CompIce = 0 in the FAST input file.' )
   END IF
   
   
!   IF ( p%InterpOrder < 0 .OR. p%InterpOrder > 2 ) THEN
   IF ( p%InterpOrder < 1 .OR. p%InterpOrder > 2 ) THEN
      CALL SetErrors( ErrID_Fatal, 'InterpOrder must be 1 or 2.' ) ! 5/13/14 bjj: MAS and JMJ compromise for certain integrators is that InterpOrder cannot be 0
      p%InterpOrder = 1    ! Avoid problems in error handling by setting this to 0
   END IF

   IF ( p%NumCrctn < 0_IntKi ) THEN
      CALL SetErrors( ErrID_Fatal, 'NumCrctn must be 0 or greater.' )
   END IF   
   
   
   IF ( ErrStat >= AbortErrLev ) RETURN

   !...............................................................................................................................

      ! temporary check on p_FAST%DT_out (bjj: fix this later)

   IF ( .NOT. EqualRealNos( p%DT_out, p%DT ) ) THEN
      IF ( p%DT_out < p%DT ) THEN
         CALL SetErrors( ErrID_Fatal, 'DT_out must be at least DT ('//TRIM(Num2LStr(p%DT))//' s).' )
      ELSEIF ( .NOT. EqualRealNos( p%DT_out, p%DT * NINT(p%DT_out / p%DT ) )  ) THEN
         CALL SetErrors( ErrID_Fatal, 'DT_out must currently be an integer multiple of DT.' )
      END IF
   END IF

   IF ( p%CompUserTwrLd ) THEN
      CALL SetErrors( ErrID_Info, 'CompUserTwrLd will be ignored in this version of FAST.' )
      p%CompUserTwrLd = .FALSE.
   END IF

   IF ( p%CompUserPtfmLd ) THEN
      CALL SetErrors( ErrID_Info, 'CompUserPtfmLd will be ignored in this version of FAST.' )
      p%CompUserPtfmLd = .FALSE.
   END IF

   
   RETURN
CONTAINS
   !-------------------------------------------------------------------------------------------------------------------------------
   SUBROUTINE SetErrors( ErrStat3, ErrMsg3 )
   ! This routine sets the error message and flag when an error has occurred
   !...............................................................................................................................
   INTEGER(IntKi), INTENT(IN) :: ErrStat3     ! Error status for this error
   CHARACTER(*),   INTENT(IN) :: ErrMsg3      ! Error message for this error

      ErrStat = MAX( ErrStat, ErrStat3 )
      IF ( LEN_TRIM(ErrMsg) > 0 ) ErrMsg = TRIM(ErrMsg)//NewLine
      ErrMsg = TRIM(ErrMsg)//TRIM(ErrMsg3)

   END SUBROUTINE SetErrors
   !-------------------------------------------------------------------------------------------------------------------------------
END SUBROUTINE FAST_Init
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE FAST_InitOutput( p_FAST, y_FAST, InitOutData_ED, InitOutData_SrvD, InitOutData_AD, InitOutData_HD, &
                            InitOutData_SD, InitOutData_MAP, InitOutData_FEAM, InitOutData_IceF, InitOutData_IceD, ErrStat, ErrMsg )
! This routine initializes the output for the glue code, including writing the header for the primary output file.
! was previously called WrOutHdr()
!..................................................................................................................................

   IMPLICIT NONE

      ! Passed variables
   TYPE(FAST_ParameterType),       INTENT(IN)           :: p_FAST                                ! Glue-code simulation parameters
   TYPE(FAST_OutputType),          INTENT(INOUT)        :: y_FAST                                ! Glue-code simulation outputs

   TYPE(ED_InitOutputType),        INTENT(IN)           :: InitOutData_ED                        ! Initialization output for ElastoDyn
   TYPE(SrvD_InitOutputType),      INTENT(IN)           :: InitOutData_SrvD                      ! Initialization output for ServoDyn
   TYPE(AD_InitOutputType),        INTENT(IN)           :: InitOutData_AD                        ! Initialization output for ServoDyn
   TYPE(HydroDyn_InitOutputType),  INTENT(IN)           :: InitOutData_HD                        ! Initialization output for HydroDyn
   TYPE(SD_InitOutputType),        INTENT(IN)           :: InitOutData_SD                        ! Initialization output for SubDyn
   TYPE(MAP_InitOutputType),       INTENT(IN)           :: InitOutData_MAP                       ! Initialization output for MAP
   TYPE(FEAM_InitOutputType),      INTENT(IN)           :: InitOutData_FEAM                      ! Initialization output for FEAMooring
   TYPE(IceFloe_InitOutputType),   INTENT(IN)           :: InitOutData_IceF                      ! Initialization output for IceFloe
   TYPE(IceD_InitOutputType),      INTENT(IN)           :: InitOutData_IceD                      ! Initialization output for IceDyn

   INTEGER(IntKi),                 INTENT(OUT)          :: ErrStat                               ! Error status
   CHARACTER(*),                   INTENT(OUT)          :: ErrMsg                                ! Error message corresponding to ErrStat


      ! Local variables.

   INTEGER(IntKi)                   :: I, J                                            ! Generic index for DO loops.
   INTEGER(IntKi)                   :: indxLast                                        ! The index of the last value to be written to an array
   INTEGER(IntKi)                   :: indxNext                                        ! The index of the next value to be written to an array
   INTEGER(IntKi)                   :: NumOuts                                         ! number of channels to be written to the output file(s)



   !......................................................
   ! Set the description lines to be printed in the output file
   !......................................................
   y_FAST%FileDescLines(1)  = 'Predictions were generated on '//CurDate()//' at '//CurTime()//' using '//TRIM(GetVersion())
   y_FAST%FileDescLines(2)  = 'linked with ' //' '//TRIM(GetNVD(NWTC_Ver            ))  ! we'll get the rest of the linked modules in the section below
   y_FAST%FileDescLines(3)  = 'Description from the FAST input file: '//TRIM(p_FAST%FTitle)
   
   !......................................................
   ! We'll fill out the rest of FileDescLines(2), 
   ! and save the module version info for later use, too:
   !......................................................

   y_FAST%Module_Ver( Module_ED )   = InitOutData_ED%Ver
   y_FAST%FileDescLines(2)          = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%Module_Ver( Module_ED )  ))

   IF ( p_FAST%CompAero == Module_AD )  THEN
      y_FAST%Module_Ver( Module_IfW ) = InitOutData_AD%IfW_InitOutput%Ver ! call copy routine?
      y_FAST%Module_Ver( Module_AD  ) = InitOutData_AD%Ver
     
      y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%Module_Ver( Module_IfW ))) 
      y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%Module_Ver( Module_AD  ) ))                  
   END IF

   IF ( p_FAST%CompServo == Module_SrvD ) THEN
      y_FAST%Module_Ver( Module_SrvD ) = InitOutData_SrvD%Ver
      y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%Module_Ver( Module_SrvD )))
   END IF
         
   IF ( p_FAST%CompHydro == Module_HD ) THEN
      y_FAST%Module_Ver( Module_HD )   = InitOutData_HD%Ver
      y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%Module_Ver( Module_HD )))
   END IF

   IF ( p_FAST%CompSub == Module_SD ) THEN
      y_FAST%Module_Ver( Module_SD )   = InitOutData_SD%Ver
      y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%Module_Ver( Module_SD )))
   END IF

   IF ( p_FAST%CompMooring == Module_MAP ) THEN
      y_FAST%Module_Ver( Module_MAP )   = InitOutData_MAP%Ver
      y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%Module_Ver( Module_MAP )))
   ELSEIF ( p_FAST%CompMooring == Module_FEAM ) THEN
      y_FAST%Module_Ver( Module_FEAM )   = InitOutData_FEAM%Ver
      y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%Module_Ver( Module_FEAM )))
   END IF   
   
   IF ( p_FAST%CompIce == Module_IceF ) THEN
      y_FAST%Module_Ver( Module_IceF )   = InitOutData_IceF%Ver
      y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%Module_Ver( Module_IceF )))
   ELSEIF ( p_FAST%CompIce == Module_IceD ) THEN
      y_FAST%Module_Ver( Module_IceD )   = InitOutData_IceD%Ver
      y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%Module_Ver( Module_IceD )))   
   END IF      
   
   !......................................................
   ! Set the number of output columns from each module
   !......................................................
   y_FAST%numOuts = 0.0    ! Inintialize entire array
   
   
   y_FAST%numOuts(Module_AD)   = 0
   
   !y_FAST%numOuts(Module_InfW)  = 3  !hack for now: always output 3 wind speeds at hub-height
   IF ( ALLOCATED( InitOutData_AD%IfW_InitOutput%WriteOutputHdr ) ) &
                                                       y_FAST%numOuts(Module_IfW)  = SIZE(InitOutData_AD%IfW_InitOutput%WriteOutputHdr)
   IF ( ALLOCATED( InitOutData_ED%WriteOutputHdr   ) ) y_FAST%numOuts(Module_ED)   = SIZE(InitOutData_ED%WriteOutputHdr)
   IF ( ALLOCATED( InitOutData_SrvD%WriteOutputHdr ) ) y_FAST%numOuts(Module_SrvD) = SIZE(InitOutData_SrvD%WriteOutputHdr)
   IF ( ALLOCATED( InitOutData_HD%WriteOutputHdr   ) ) y_FAST%numOuts(Module_HD)   = SIZE(InitOutData_HD%WriteOutputHdr)
   IF ( ALLOCATED( InitOutData_SD%WriteOutputHdr   ) ) y_FAST%numOuts(Module_SD)   = SIZE(InitOutData_SD%WriteOutputHdr)
   IF ( ALLOCATED( InitOutData_MAP%WriteOutputHdr  ) ) y_FAST%numOuts(Module_MAP)  = SIZE(InitOutData_MAP%WriteOutputHdr)
   IF ( ALLOCATED( InitOutData_FEAM%WriteOutputHdr ) ) y_FAST%numOuts(Module_FEAM) = SIZE(InitOutData_FEAM%WriteOutputHdr)
   IF ( ALLOCATED( InitOutData_IceF%WriteOutputHdr ) ) y_FAST%numOuts(Module_IceF) = SIZE(InitOutData_IceF%WriteOutputHdr)
   IF ( ALLOCATED( InitOutData_IceD%WriteOutputHdr ) ) y_FAST%numOuts(Module_IceD) = SIZE(InitOutData_IceD%WriteOutputHdr)*p_FAST%numIceLegs         
   
   !......................................................
   ! Initialize the output channel names and units
   !......................................................
   NumOuts   = 1 + SUM( y_FAST%numOuts )

   CALL AllocAry( y_FAST%ChannelNames,NumOuts, 'ChannelNames', ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry( y_FAST%ChannelUnits,NumOuts, 'ChannelUnits', ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN

   y_FAST%ChannelNames(1) = 'Time'
   y_FAST%ChannelUnits(1) = '(s)'

   indxLast = 1
   indxNext = 2

   IF ( y_FAST%numOuts(Module_IfW) > 0_IntKi ) THEN  !InflowWind: hack for now
      indxLast = indxNext + y_FAST%numOuts(Module_IfW) - 1
      y_FAST%ChannelNames(indxNext:indxLast) = InitOutData_AD%IfW_InitOutput%WriteOutputHdr
      y_FAST%ChannelUnits(indxNext:indxLast) = InitOutData_AD%IfW_InitOutput%WriteOutputUnt
      
      indxNext = indxLast + 1
   END IF


   IF ( y_FAST%numOuts(Module_ED) > 0_IntKi ) THEN !ElastoDyn
      indxLast = indxNext + y_FAST%numOuts(Module_ED) - 1
      y_FAST%ChannelNames(indxNext:indxLast) = InitOutData_ED%WriteOutputHdr
      y_FAST%ChannelUnits(indxNext:indxLast) = InitOutData_ED%WriteOutputUnt
      indxNext = indxLast + 1
   END IF

   ! AeroDyn next
   
   IF ( y_FAST%numOuts(Module_SrvD) > 0_IntKi ) THEN !ServoDyn
      indxLast = indxNext + y_FAST%numOuts(Module_SrvD) - 1
      y_FAST%ChannelNames(indxNext:indxLast) = InitOutData_SrvD%WriteOutputHdr
      y_FAST%ChannelUnits(indxNext:indxLast) = InitOutData_SrvD%WriteOutputUnt
      indxNext = indxLast + 1
   END IF

   IF ( y_FAST%numOuts(Module_HD) > 0_IntKi ) THEN !HydroDyn
      indxLast = indxNext + y_FAST%numOuts(Module_HD) - 1
      y_FAST%ChannelNames(indxNext:indxLast) = InitOutData_HD%WriteOutputHdr
      y_FAST%ChannelUnits(indxNext:indxLast) = InitOutData_HD%WriteOutputUnt
      indxNext = indxLast + 1
   END IF

   
   IF ( y_FAST%numOuts(Module_SD) > 0_IntKi ) THEN !SubDyn
      indxLast = indxNext + y_FAST%numOuts(Module_SD) - 1
      y_FAST%ChannelNames(indxNext:indxLast) = InitOutData_SD%WriteOutputHdr
      y_FAST%ChannelUnits(indxNext:indxLast) = InitOutData_SD%WriteOutputUnt
      indxNext = indxLast + 1
   END IF

   
   IF ( y_FAST%numOuts(Module_MAP) > 0_IntKi ) THEN !MAP
      indxLast = indxNext + y_FAST%numOuts(Module_MAP) - 1
      y_FAST%ChannelNames(indxNext:indxLast) = InitOutData_MAP%WriteOutputHdr
      y_FAST%ChannelUnits(indxNext:indxLast) = InitOutData_MAP%WriteOutputUnt
      indxNext = indxLast + 1
   ELSEIF ( y_FAST%numOuts(Module_FEAM) > 0_IntKi ) THEN !FEAMooring
      indxLast = indxNext + y_FAST%numOuts(Module_FEAM) - 1
      y_FAST%ChannelNames(indxNext:indxLast) = InitOutData_FEAM%WriteOutputHdr
      y_FAST%ChannelUnits(indxNext:indxLast) = InitOutData_FEAM%WriteOutputUnt
      indxNext = indxLast + 1
   END IF
   
   
   IF ( y_FAST%numOuts(Module_IceF) > 0_IntKi ) THEN !IceFloe
      indxLast = indxNext + y_FAST%numOuts(Module_IceF) - 1
      y_FAST%ChannelNames(indxNext:indxLast) = InitOutData_IceF%WriteOutputHdr
      y_FAST%ChannelUnits(indxNext:indxLast) = InitOutData_IceF%WriteOutputUnt
      indxNext = indxLast + 1
   ELSEIF ( y_FAST%numOuts(Module_IceD) > 0_IntKi ) THEN !IceDyn
      DO I=1,p_FAST%numIceLegs         
         DO J=1,SIZE(InitOutData_IceD%WriteOutputHdr) 
            y_FAST%ChannelNames(indxNext) =TRIM(InitOutData_IceD%WriteOutputHdr(J))//'L'//TRIM(Num2Lstr(I))  !bjj: do we want this "Lx" at the end?
            y_FAST%ChannelUnits(indxNext) = InitOutData_IceD%WriteOutputUnt(J)
            indxNext = indxNext + 1
         END DO ! J
      END DO ! I
   END IF   
      
   
   !......................................................
   ! Open the text output file and print the headers
   !......................................................

   IF (p_FAST%WrTxtOutFile) THEN

      CALL GetNewUnit( y_FAST%UnOu, ErrStat, ErrMsg )
         IF ( ErrStat >= AbortErrLev ) RETURN

      CALL OpenFOutFile ( y_FAST%UnOu, TRIM(p_FAST%OutFileRoot)//'.out', ErrStat, ErrMsg )
         IF ( ErrStat >= AbortErrLev ) RETURN

         ! Add some file information:

      WRITE (y_FAST%UnOu,'(/,A)')  TRIM( y_FAST%FileDescLines(1) )
      WRITE (y_FAST%UnOu,'(1X,A)') TRIM( y_FAST%FileDescLines(2) )
      WRITE (y_FAST%UnOu,'()' )    !print a blank line
      WRITE (y_FAST%UnOu,'(A)'   ) TRIM( y_FAST%FileDescLines(3) )
      WRITE (y_FAST%UnOu,'()' )    !print a blank line


         !......................................................
         ! Write the names of the output parameters on one line:
         !......................................................

      CALL WrFileNR ( y_FAST%UnOu, y_FAST%ChannelNames(1) )

      DO I=2,NumOuts
         CALL WrFileNR ( y_FAST%UnOu, p_FAST%Delim//y_FAST%ChannelNames(I) )
      ENDDO ! I

      WRITE (y_FAST%UnOu,'()')

         !......................................................
         ! Write the units of the output parameters on one line:
         !......................................................

      CALL WrFileNR ( y_FAST%UnOu, y_FAST%ChannelUnits(1) )

      DO I=2,NumOuts
         CALL WrFileNR ( y_FAST%UnOu, p_FAST%Delim//y_FAST%ChannelUnits(I) )
      ENDDO ! I

      WRITE (y_FAST%UnOu,'()')

   END IF

   !......................................................
   ! Allocate data for binary output file
   !......................................................
   IF (p_FAST%WrBinOutFile) THEN

         ! calculate the size of the array of outputs we need to store
      y_FAST%NOutSteps = NINT ( (p_FAST%TMax - p_FAST%TStart) / p_FAST%DT_OUT ) + 1

      CALL AllocAry( y_FAST%AllOutData, NumOuts-1, y_FAST%NOutSteps, 'AllOutData', ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN

      IF ( OutputFileFmtID == FileFmtID_WithoutTime ) THEN

         CALL AllocAry( y_FAST%TimeData, 2_IntKi, 'TimeData', ErrStat, ErrMsg )
         IF ( ErrStat /= ErrID_None ) RETURN

         y_FAST%TimeData(1) = 0.0_DbKi           ! This is the first output time, which we will set later
         y_FAST%TimeData(2) = p_FAST%DT_out      ! This is the (constant) time between subsequent writes to the output file

      ELSE  ! we store the entire time array

         CALL AllocAry( y_FAST%TimeData, y_FAST%NOutSteps, 'TimeData', ErrStat, ErrMsg )
         IF ( ErrStat /= ErrID_None ) RETURN

      END IF

      y_FAST%n_Out = 0  !number of steps actually written to the file

   END IF


RETURN
END SUBROUTINE FAST_InitOutput
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE FAST_WrSum( p_FAST, y_FAST, MeshMapData, ErrStat, ErrMsg )
! This subroutine opens and writes data to the FAST summary file. The file gets closed at the end of program (not in this 
! subroutine).
!..................................................................................................................................

   TYPE(FAST_ParameterType), INTENT(IN)    :: p_FAST                             ! Glue-code simulation parameters
   TYPE(FAST_OutputType),    INTENT(INOUT) :: y_FAST                             ! Glue-code simulation outputs (changes value of UnSum)
   TYPE(FAST_ModuleMapType), INTENT(IN)    :: MeshMapData                        ! Data for mapping between modules
   INTEGER(IntKi),           INTENT(OUT)   :: ErrStat                            ! Error status (level)
   CHARACTER(*),             INTENT(OUT)   :: ErrMsg                             ! Message describing error reported in ErrStat

      ! local variables
   INTEGER(IntKi)                          :: I                                  ! temporary counter
   INTEGER(IntKi)                          :: J                                  ! temporary counter
   INTEGER(IntKi)                          :: Module_Number                      ! loop counter through the modules
   INTEGER(IntKi)                          :: JacSize                            ! variable describing size of jacobian matrix
   CHARACTER(200)                          :: Fmt                                ! temporary format string
   CHARACTER(200)                          :: DescStr                            ! temporary string to write text
   CHARACTER(*), PARAMETER                 :: NotUsedTxt = " [not called]"       ! text written if a module is not called

      ! Get a unit number and open the file:

   CALL GetNewUnit( y_FAST%UnSum, ErrStat, ErrMsg )
      IF ( ErrStat >= AbortErrLev ) RETURN

   CALL OpenFOutFile ( y_FAST%UnSum, TRIM(p_FAST%OutFileRoot)//'.sum', ErrStat, ErrMsg )
      IF ( ErrStat >= AbortErrLev ) RETURN

         ! Add some file information:

   !.......................... Module Versions .....................................................
   !bjj: modules in this list are ordered by the order they are specified in the FAST input file

   WRITE (y_FAST%UnSum,'(/A)') 'FAST Summary File'
   WRITE (y_FAST%UnSum,'(/A)')  TRIM( y_FAST%FileDescLines(1) )

   WRITE (y_FAST%UnSum,'(2X,A)'   )  'compiled with'
   Fmt = '(4x,A)'
   WRITE (y_FAST%UnSum,Fmt)  TRIM( GetNVD(        NWTC_Ver ) )
   WRITE (y_FAST%UnSum,Fmt)  TRIM( GetNVD( y_FAST%Module_Ver( Module_ED )   ) )

   DescStr = GetNVD( y_FAST%Module_Ver( Module_AD ) )
   IF ( p_FAST%CompAero /= Module_AD ) DescStr = TRIM(DescStr)//NotUsedTxt
   WRITE (y_FAST%UnSum,Fmt)  TRIM( DescStr )
   
   DescStr = GetNVD( y_FAST%Module_Ver( Module_IfW ) )
   IF ( p_FAST%CompAero /= Module_AD ) DescStr = TRIM(DescStr)//NotUsedTxt !IfW is a submodule of AD right now
   WRITE (y_FAST%UnSum,Fmt)  TRIM( DescStr )
   
   DescStr = GetNVD( y_FAST%Module_Ver( Module_SrvD ) )
   IF ( p_FAST%CompServo /= Module_SrvD ) DescStr = TRIM(DescStr)//NotUsedTxt
   WRITE (y_FAST%UnSum,Fmt)  TRIM( DescStr )  
   
   DescStr = GetNVD( y_FAST%Module_Ver( Module_HD ) )
   IF ( p_FAST%CompHydro /= Module_HD  ) DescStr = TRIM(DescStr)//NotUsedTxt
   WRITE (y_FAST%UnSum,Fmt)  TRIM( DescStr )
   
   DescStr = GetNVD( y_FAST%Module_Ver( Module_SD ) )
   IF ( p_FAST%CompSub /= Module_SD ) DescStr = TRIM(DescStr)//NotUsedTxt
   WRITE (y_FAST%UnSum,Fmt)  TRIM( DescStr )
   
   DescStr = GetNVD( y_FAST%Module_Ver( Module_MAP ) )
   IF ( p_FAST%CompMooring /= Module_MAP ) DescStr = TRIM(DescStr)//NotUsedTxt
   WRITE (y_FAST%UnSum,Fmt)  TRIM( DescStr )

   DescStr = GetNVD( y_FAST%Module_Ver( Module_FEAM ) )
   IF ( p_FAST%CompMooring /= Module_FEAM ) DescStr = TRIM(DescStr)//NotUsedTxt
   WRITE (y_FAST%UnSum,Fmt)  TRIM( DescStr )
   
   DescStr = GetNVD( y_FAST%Module_Ver( Module_IceF ) )
   IF ( p_FAST%CompIce /= Module_IceF ) DescStr = TRIM(DescStr)//NotUsedTxt
   WRITE (y_FAST%UnSum,Fmt)  TRIM( DescStr )
   
   DescStr = GetNVD( y_FAST%Module_Ver( Module_IceD ) )
   IF ( p_FAST%CompIce /= Module_IceD ) DescStr = TRIM(DescStr)//NotUsedTxt
   WRITE (y_FAST%UnSum,Fmt)  TRIM( DescStr )
   
   
   !.......................... Information from FAST input File ......................................
! OTHER information we could print here:   
! current working directory
! output file root name
! output file time step
! output file format (text/binary)
! coupling method

   SELECT CASE ( p_FAST%TurbineType )
   CASE ( Type_LandBased )
      DescStr = 'Modeling a land-based turbine'
   CASE ( Type_Offshore_Fixed )
      DescStr = 'Modeling a fixed-bottom offshore turbine'
   CASE ( Type_Offshore_Floating )
      DescStr = 'Modeling a floating offshore turbine'
   CASE DEFAULT ! This should never happen
      DescStr=""
   END SELECT                  
   WRITE(y_FAST%UnSum,'(//A)') TRIM(DescStr)

   WRITE (y_FAST%UnSum,'(A)' )   'Description from the FAST input file: '
   WRITE (y_FAST%UnSum,'(2X,A)')  TRIM(p_FAST%FTitle)

   !.......................... Requested Features ...................................................
   
   SELECT CASE ( p_FAST%InterpOrder )
   CASE (0)
      DescStr = ' (nearest neighbor)'
   CASE (1)
      DescStr = ' (linear)'
   CASE (2)
      DescStr = ' (quadratic)'
   CASE DEFAULT 
      DescStr = ' ( )'
   END SELECT               
   
   WRITE(y_FAST%UnSum,'(/A,I1,A)'  ) 'Interpolation order for input/output time histories: ', p_FAST%InterpOrder, TRIM(DescStr)
   WRITE(y_FAST%UnSum,'( A,I2)'    ) 'Number of correction iterations: ', p_FAST%NumCrctn
   
      
   !.......................... Information About Coupling ...................................................
      
   IF ( ALLOCATED( MeshMapData%Jacobian_ED_SD_HD ) ) then
      
      IF ( p_FAST%CompHydro == Module_HD ) THEN ! HydroDyn <-> {ElastoDyn or SubDyn}
      
         IF ( p_FAST%CompSub /= Module_SD ) THEN ! HydroDyn-ElastoDyn        
            DescStr = "ElastoDyn to HydroDyn"
         ELSE ! HydroDyn <-> SubDyn <-> ElastoDyn (in ED_SD_HD coupling)
            DescStr = "ElastoDyn, SubDyn, and HydroDyn"
         END IF ! HydroDyn <-> {ElastoDyn or SubDyn}
   
      ELSEIF ( p_FAST%CompSub == Module_SD ) THEN  ! SubDyn-ElastoDyn    
         DescStr = "ElastoDyn to SubDyn"
      !ELSE ! no HD or SD:
      END IF ! ElastoDyn <-> {HydroDyn or SubDyn}      
            
      WRITE(y_FAST%UnSum,'( A,I6)'  ) 'Number of rows in Jacobian matrix used for coupling '//TRIM(DescStr)//': ', &
                                       SIZE(MeshMapData%Jacobian_ED_SD_HD, 1)
   END IF

   !.......................... Time step information: ...................................................
   
   WRITE (y_FAST%UnSum,'(//,2X,A)') " Requested Time Steps  "
   WRITE (y_FAST%UnSum,   '(2X,A)') "-------------------------------------------------"
   Fmt = '(2X,A17,2X,A15,2X,A13)'
   WRITE (y_FAST%UnSum, Fmt ) "Component        ", "Time Step (s)  ", "Subcycles (-)"
   WRITE (y_FAST%UnSum, Fmt ) "-----------------", "---------------", "-------------"
   Fmt = '(2X,A17,2X,'//TRIM(p_FAST%OutFmt)//',:,T37,2X,I8,:,A)'
   WRITE (y_FAST%UnSum, Fmt ) "FAST (glue code) ", p_FAST%DT
   DO Module_Number=1,NumModules
      IF (p_FAST%ModuleInitialized(Module_Number)) THEN
         WRITE (y_FAST%UnSum, Fmt ) y_FAST%Module_Ver(Module_Number)%Name, p_FAST%DT_module(Module_Number), p_FAST%n_substeps(Module_Number)
      END IF
   END DO
   IF ( NINT( p_FAST%DT_out / p_FAST%DT )  == 1_IntKi ) THEN
      WRITE (y_FAST%UnSum, Fmt ) "FAST output files", p_FAST%DT_out, 1_IntKi   ! we'll write "1" instead of "1^-1"
   ELSE
      WRITE (y_FAST%UnSum, Fmt ) "FAST output files", p_FAST%DT_out, NINT( p_FAST%DT_out / p_FAST%DT ),"^-1"
   END IF
   
   
   !.......................... Requested Output Channels ............................................

   WRITE (y_FAST%UnSum,'(//,2X,A)') " Requested Channels in FAST Output File(s)  "
   WRITE (y_FAST%UnSum,   '(2X,A)') "--------------------------------------------"
   Fmt = '(2X,A6,2(2X,A'//TRIM(num2lstr(ChanLen))//'),2X,A)'
   WRITE (y_FAST%UnSum, Fmt ) "Number", "Name      ", "Units     ", "Generated by"
   WRITE (y_FAST%UnSum, Fmt ) "------", "----------", "----------", "------------"

   Fmt = '(4X,I4,2(2X,A'//TRIM(num2lstr(ChanLen))//'),2X,A)'
   I = 1
   WRITE (y_FAST%UnSum, Fmt ) I, y_FAST%ChannelNames(I), y_FAST%ChannelUnits(I), TRIM(FAST_Ver%Name)

   
   DO Module_Number = 1,NumModules
      DO J = 1,y_FAST%numOuts( Module_Number )
         I = I + 1
         WRITE (y_FAST%UnSum, Fmt ) I, y_FAST%ChannelNames(I), y_FAST%ChannelUnits(I), TRIM(y_FAST%Module_Ver( Module_Number )%Name)
      END DO
   END DO
      
   
   !.......................... End of Summary File ............................................
   
   ! bjj: note that I'm not closing the summary file here, though at the present time we don't write to this file again.
   ! In the future, we may want to write additional information to this file during the simulation.
   

END SUBROUTINE FAST_WrSum
!----------------------------------------------------------------------------------------------------------------------------------
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

   CHARACTER(10)                 :: AbortLevel                                ! String that indicates which error level should be used to abort the program: WARNING, SEVERE, or FATAL


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

      CALL ReadStr( UnIn, InputFile, p%FTitle, 'FTitle', 'File Header: File Description (line 2)', ErrStat2, ErrMsg2, UnEc )
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

      CALL OpenEcho ( UnEc, TRIM(p%OutFileRoot)//'.ech', ErrStat2, ErrMsg2, FAST_Ver )
         CALL CheckError( ErrStat2, ErrMsg2 )
         IF ( ErrStat >= AbortErrLev ) RETURN

      IF ( UnEc > 0 )  WRITE (UnEc,'(/,A,/)')  'Data from '//TRIM(FAST_Ver%Name)//' primary input file "'//TRIM( InputFile )//'":'

      REWIND( UnIn, IOSTAT=ErrStat2 )
         IF (ErrStat2 /= 0_IntKi ) THEN
            CALL CheckError( ErrID_Fatal, 'Error rewinding file "'//TRIM(InputFile)//'".' )
            IF ( ErrStat >= AbortErrLev ) RETURN
         END IF

   END DO

   CALL WrScr( ' Heading of the '//TRIM(FAST_Ver%Name)//' input file: ' )
   CALL WrScr( '   '//TRIM( p%FTitle ) )


      ! AbortLevel - Error level when simulation should abort:
   CALL ReadVar( UnIn, InputFile, AbortLevel, "AbortLevel", "Error level when simulation should abort (string)", &
                        ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! Let's set the abort level here.... knowing that everything before this aborted only on FATAL errors!
      CALL Conv2UC( AbortLevel ) !convert to upper case
      SELECT CASE( TRIM(AbortLevel) )
         CASE ( "WARNING" )
            AbortErrLev = ErrID_Warn
         CASE ( "SEVERE" )
            AbortErrLev = ErrID_Severe
         CASE ( "FATAL" )
            AbortErrLev = ErrID_Fatal
         CASE DEFAULT
            CALL CheckError( ErrID_Fatal, 'Invalid AbortLevel specified in FAST input file. '// &
                                 'Valid entries are "WARNING", "SEVERE", or "FATAL".' )
            RETURN
      END SELECT


      ! TMax - Total run time (s):
   CALL ReadVar( UnIn, InputFile, p%TMax, "TMax", "Total run time (s)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! DT - Recommended module time step (s):
   CALL ReadVar( UnIn, InputFile, p%DT, "DT", "Recommended module time step (s)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! InterpOrder - Interpolation order for inputs and outputs {0=nearest neighbor ,1=linear, 2=quadratic}
   CALL ReadVar( UnIn, InputFile, p%InterpOrder, "InterpOrder", "Interpolation order "//&
                   "for inputs and outputs {0=nearest neighbor ,1=linear, 2=quadratic} (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! NumCrctn - Number of predictor-corrector iterations {1=explicit calculation, i.e., no corrections}
   CALL ReadVar( UnIn, InputFile, p%NumCrctn, "NumCrctn", "Number of corrections"//&
                   "{0=explicit calculation, i.e., no corrections} (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! DT_UJac - Time between calls to get Jacobians (s)
   CALL ReadVar( UnIn, InputFile, p%DT_UJac, "DT_UJac", "Time between calls to get Jacobians (s)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! UJacSclFact - Scaling factor used in Jacobians (-)
   CALL ReadVar( UnIn, InputFile, p%UJacSclFact, "UJacSclFact", "Scaling factor used in Jacobians (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN              
                  
   !---------------------- FEATURE SWITCHES AND FLAGS --------------------------------
   CALL ReadCom( UnIn, InputFile, 'Section Header: Feature Switches and Flags', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! CompElast - Compute structural dynamics (switch) {1=ElastoDyn; 2=ElastoDyn + BeamDyn for blades}:
   CALL ReadVar( UnIn, InputFile, p%CompElast, "CompElast", "Compute structural dynamics (switch) {1=ElastoDyn; 2=ElastoDyn + BeamDyn for blades}", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
          ! immediately convert to values used inside the code:
         IF ( p%CompElast == 1 ) THEN 
            p%CompElast = Module_ED
         ELSEIF ( p%CompElast == 2 ) THEN
            p%CompElast = Module_BD
         ELSE
            p%CompElast = Module_Unknown
         END IF
                                  
      ! CompAero - Compute aerodynamic loads (switch) {0=None; 1=AeroDyn}:
   CALL ReadVar( UnIn, InputFile, p%CompAero, "CompAero", "Compute aerodynamic loads (switch) {0=None; 1=AeroDyn}", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
          ! immediately convert to values used inside the code:
         IF ( p%CompAero == 0 ) THEN 
            p%CompAero = Module_NONE
         ELSEIF ( p%CompAero == 1 ) THEN
            p%CompAero = Module_AD
         ELSE
            p%CompAero = Module_Unknown
         END IF

      ! CompServo - Compute control and electrical-drive dynamics (switch) {0=None; 1=ServoDyn}:
   CALL ReadVar( UnIn, InputFile, p%CompServo, "CompServo", "Compute control and electrical-drive dynamics (switch) {0=None; 1=ServoDyn}", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
          ! immediately convert to values used inside the code:
         IF ( p%CompServo == 0 ) THEN 
            p%CompServo = Module_NONE
         ELSEIF ( p%CompServo == 1 ) THEN
            p%CompServo = Module_SrvD
         ELSE
            p%CompServo = Module_Unknown
         END IF
      
      
      ! CompHydro - Compute hydrodynamic loads (switch) {0=None; 1=HydroDyn}:
   CALL ReadVar( UnIn, InputFile, p%CompHydro, "CompHydro", "Compute hydrodynamic loads (switch) {0=None; 1=HydroDyn}", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
          ! immediately convert to values used inside the code:
         IF ( p%CompHydro == 0 ) THEN 
            p%CompHydro = Module_NONE
         ELSEIF ( p%CompHydro == 1 ) THEN
            p%CompHydro = Module_HD
         ELSE
            p%CompHydro = Module_Unknown
         END IF
         
      ! CompSub - Compute sub-structural dynamics (switch) {0=None; 1=SubDyn}:
   CALL ReadVar( UnIn, InputFile, p%CompSub, "CompSub", "Compute sub-structural dynamics (switch) {0=None; 1=SubDyn}", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
          ! immediately convert to values used inside the code:
         IF ( p%CompSub == 0 ) THEN 
            p%CompSub = Module_NONE
         ELSEIF ( p%CompSub == 1 ) THEN
            p%CompSub = Module_SD
         ELSE
            p%CompSub = Module_Unknown
         END IF
         
      ! CompMooring - Compute mooring line dynamics (flag):
   CALL ReadVar( UnIn, InputFile, p%CompMooring, "CompMooring", "Compute mooring system (switch) {0=None; 1=MAP, 2=FEAMooring}", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN         
          ! immediately convert to values used inside the code:
         IF ( p%CompMooring == 0 ) THEN 
            p%CompMooring = Module_NONE
         ELSEIF ( p%CompMooring == 1 ) THEN
            p%CompMooring = Module_MAP
         ELSEIF ( p%CompMooring == 2 ) THEN
            p%CompMooring = Module_FEAM
         ELSE
            p%CompMooring = Module_Unknown
         END IF      
      
      ! CompIce - Compute ice loads (switch) {0=None; 1=IceFloe}:
   CALL ReadVar( UnIn, InputFile, p%CompIce, "CompIce", "Compute ice loads (switch) {0=None; 1=IceFloe}", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
          ! immediately convert to values used inside the code:
         IF ( p%CompIce == 0 ) THEN 
            p%CompIce = Module_NONE
         ELSEIF ( p%CompIce == 1 ) THEN
            p%CompIce = Module_IceF
         ELSEIF ( p%CompIce == 2 ) THEN
            p%CompIce = Module_IceD
         ELSE
            p%CompIce = Module_Unknown
         END IF
         
         
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

DO i=1,MaxNBlades
      ! BDBldFile - Name of file containing BeamDyn blade input parameters (-):
   CALL ReadVar( UnIn, InputFile, p%BDBldFile(i), "BDBldFile("//TRIM(num2LStr(i))//")", "Name of file containing BeamDyn blade "//trim(num2lstr(i))//"input parameters (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   IF ( PathIsRelative( p%BDBldFile(i) ) ) p%BDBldFile(i) = TRIM(PriPath)//TRIM(p%BDBldFile(i))
END DO
   
      ! AeroFile - Name of file containing aerodynamic input parameters (-):
   CALL ReadVar( UnIn, InputFile, p%AeroFile, "AeroFile", "Name of file containing aerodynamic input parameters (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   IF ( PathIsRelative( p%AeroFile ) ) p%AeroFile = TRIM(PriPath)//TRIM(p%AeroFile)

      ! ServoFile - Name of file containing control and electrical-drive input parameters (-):
   CALL ReadVar( UnIn, InputFile, p%ServoFile, "ServoFile", "Name of file containing control and electrical-drive input parameters (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   IF ( PathIsRelative( p%ServoFile ) ) p%ServoFile = TRIM(PriPath)//TRIM(p%ServoFile)

      ! HydroFile - Name of file containing hydrodynamic input parameters (-):
   CALL ReadVar( UnIn, InputFile, p%HydroFile, "HydroFile", "Name of file containing hydrodynamic input parameters (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   IF ( PathIsRelative( p%HydroFile ) ) p%HydroFile = TRIM(PriPath)//TRIM(p%HydroFile)

      ! SubFile - Name of file containing sub-structural input parameters (-):
   CALL ReadVar( UnIn, InputFile, p%SubFile, "SubFile", "Name of file containing sub-structural input parameters (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   IF ( PathIsRelative( p%SubFile ) ) p%SubFile = TRIM(PriPath)//TRIM(p%SubFile)

      ! MooringFile - Name of file containing mooring system input parameters (-):
   CALL ReadVar( UnIn, InputFile, p%MooringFile, "MooringFile", "Name of file containing mooring system input parameters (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   IF ( PathIsRelative( p%MooringFile ) ) p%MooringFile = TRIM(PriPath)//TRIM(p%MooringFile)
  
      ! IceFile - Name of file containing ice input parameters (-):
   CALL ReadVar( UnIn, InputFile, p%IceFile, "IceFile", "Name of file containing ice input parameters (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   IF ( PathIsRelative( p%IceFile ) ) p%IceFile = TRIM(PriPath)//TRIM(p%IceFile)
   
   
   !---------------------- OUTPUT --------------------------------------------------
   CALL ReadCom( UnIn, InputFile, 'Section Header: Output', ErrStat2, ErrMsg2, UnEc )
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN

      ! SumPrint - Print summary data to <RootName>.sum (flag):
   CALL ReadVar( UnIn, InputFile, p%SumPrint, "SumPrint", "Print summary data to <RootName>.sum (flag)", ErrStat2, ErrMsg2, UnEc)
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

      IF ( FmtWidth /= ChanLen ) CALL CheckError( ErrID_Warn, 'OutFmt produces a column width of '// &
            TRIM(Num2LStr(FmtWidth))//' instead of '//TRIM(Num2LStr(ChanLen))//' characters.' )
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

         IF (ErrStat /= ErrID_None) ErrMsg = TRIM(ErrMsg)//NewLine
         ErrMsg = TRIM(ErrMsg)//'FAST_ReadPrimaryFile:'//TRIM(Msg)
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
SUBROUTINE RunTimes( StrtTime, UsrTime1, SimStrtTime, UsrTime2, ZTime, UsrTime_out )
! This routine displays a message that gives that status of the simulation and the predicted end time of day.
!..................................................................................................................................

   IMPLICIT                        NONE

      ! Passed variables

   INTEGER   , INTENT(IN)       :: StrtTime (8)                                    ! Start time of simulation (including initialization)
   INTEGER   , INTENT(IN)       :: SimStrtTime (8)                                 ! Start time of simulation (after initialization)
   REAL      , INTENT(IN)       :: UsrTime1                                        ! User CPU time for simulation initialization.
   REAL,       INTENT(IN)       :: UsrTime2                                        ! User CPU time for simulation (without intialization)
   REAL(DbKi), INTENT(IN)       :: ZTime                                           ! The final simulation time (not necessarially TMax)
   REAL,OPTIONAL, INTENT(OUT)   :: UsrTime_out                                     ! User CPU time for entire run - optional value returned to calling routine

      ! Local variables

   REAL                         :: ClckTime                                        ! Elapsed clock time for the entire run.
   REAL                         :: ClckTimeSim                                     ! Elapsed clock time for the simulation phase of the run.
   REAL                         :: Factor                                          ! Ratio of seconds to a specified time period.
   REAL                         :: TRatio                                          ! Ratio of simulation time to elapsed clock time.
   REAL(ReKi), PARAMETER        :: SecPerDay = 24*60*60.0_ReKi                     ! Number of seconds per day

   REAL                         :: UsrTime                                         ! User CPU time for entire run.
   REAL                         :: UsrTimeSim                                      ! User CPU time for simulation (not including initialization).
   INTEGER                      :: EndTimes (8)                                    ! An array holding the ending clock time of the simulation.

   CHARACTER( 8)                :: TimePer
   CHARACTER(MaxWrScrLen)       :: BlankLine

      ! Get the end times to compare with start times.

   CALL DATE_AND_TIME ( VALUES=EndTimes )
   CALL CPU_TIME ( UsrTime )


   ! Calculate the elapsed wall-clock time in seconds.

   ClckTime     = GetClockTime(StrtTime,      EndTimes)
  !ClckTimeInit = GetClockTime(StrtTime,   SimStrtTime)
   ClckTimeSim  = GetClockTime(SimStrtTime,   EndTimes)

      ! Calculate CPU times.

   UsrTime    = UsrTime - UsrTime1
   UsrTimeSim = UsrTime - UsrTime2


   IF ( .NOT. EqualRealNos( UsrTimeSim, 0.0 ) )  THEN

      TRatio = REAL(ZTime) / UsrTimeSim

      IF     ( UsrTime > SecPerDay )  THEN
         Factor = 1.0/SecPerDay
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

      BlankLine = ""
      CALL WrOver( BlankLine )  ! BlankLine contains MaxWrScrLen spaces
      CALL WrScr1( ' Total Real Time:       '//TRIM( Num2LStr( Factor*ClckTime      ) )//TRIM( TimePer ) )
      CALL WrScr ( ' Total CPU Time:        '//TRIM( Num2LStr( Factor*UsrTime       ) )//TRIM( TimePer ) )
!     CALL WrScr ( ' ')
!     CALL WrScr ( ' Simulation Real Time:  '//TRIM( Num2LStr( Factor*ClckTimeSim   ) )//TRIM( TimePer ) )
      CALL WrScr ( ' Simulation CPU Time:   '//TRIM( Num2LStr( Factor*UsrTimeSim    ) )//TRIM( TimePer ) )      
      CALL WrScr ( ' Simulated Time:        '//TRIM( Num2LStr( Factor*REAL( ZTime ) ) )//TRIM( TimePer ) )
      CALL WrScr ( ' Time Ratio (Sim/CPU):  '//TRIM( Num2LStr( TRatio ) ) )

   ENDIF

   UsrTime_out = UsrTime
   RETURN
CONTAINS

   FUNCTION GetClockTime(StartClockTime, EndClockTime)
   ! return the number of seconds between StartClockTime and EndClockTime
   
      REAL                         :: GetClockTime          ! Elapsed clock time for the simulation phase of the run.
      INTEGER   , INTENT(IN)       :: StartClockTime (8)                                 ! Start time of simulation (after initialization)
      INTEGER   , INTENT(IN)       :: EndClockTime (8)                                 ! Start time of simulation (after initialization)
   
   !bjj: This calculation will be wrong at certain times (e.g. if it's near midnight on the last day of the month), but to my knowledge, no one has complained...
      GetClockTime =       0.001*( EndClockTime(8) - StartClockTime(8) ) &  ! Is the milliseconds of the second (range 0 to 999) - local time
                     +           ( EndClockTime(7) - StartClockTime(7) ) &  ! Is the seconds of the minute (range 0 to 59) - local time
                     +      60.0*( EndClockTime(6) - StartClockTime(6) ) &  ! Is the minutes of the hour (range 0 to 59) - local time
                     +    3600.0*( EndClockTime(5) - StartClockTime(5) ) &  ! Is the hour of the day (range 0 to 23) - local time
                     + SecPerDay*( EndClockTime(3) - StartClockTime(3) )    ! Is the day of the month
   
   
   END FUNCTION
   
END SUBROUTINE RunTimes
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE SimStatus_FirstTime( PrevSimTime, PrevClockTime, SimStrtTime, UsrTimeSim, ZTime, TMax )
! This routine displays a message that gives that status of the simulation.
!..................................................................................................................................

   IMPLICIT                        NONE

      ! Passed variables
   REAL(DbKi), INTENT(IN   )    :: ZTime                                           ! Current simulation time (s)
   REAL(DbKi), INTENT(IN   )    :: TMax                                            ! Expected simulation time (s)
   REAL(DbKi), INTENT(  OUT)    :: PrevSimTime                                     ! Previous time message was written to screen (s > 0)
   REAL(ReKi), INTENT(  OUT)    :: PrevClockTime                                   ! Previous clock time in seconds past midnight
   INTEGER,    INTENT(  OUT)    :: SimStrtTime (8)                                 ! An array containing the elements of the start time.
   REAL,       INTENT(  OUT)    :: UsrTimeSim                                      ! User CPU time for simulation (without intialization)

      ! Local variables.

   REAL(ReKi)                   :: CurrClockTime                                   ! Current time in seconds past midnight.


      ! How many seconds past midnight?

   CALL DATE_AND_TIME ( Values=SimStrtTime )
   CALL CPU_TIME ( UsrTimeSim )                                                    ! Initial CPU time   
   CurrClockTime = TimeValues2Seconds( SimStrtTime )


   CALL WrScr ( ' Timestep: '//TRIM( Num2LStr( NINT( ZTime ) ) )//' of '//TRIM( Num2LStr( TMax ) )//' seconds.')


   ! Let's save this time as the previous time for the next call to the routine
   PrevClockTime = CurrClockTime
   PrevSimTime   = ZTime

   RETURN
END SUBROUTINE SimStatus_FirstTime
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE SimStatus( PrevSimTime, PrevClockTime, ZTime, TMax )
! This routine displays a message that gives that status of the simulation and the predicted end time of day.
!..................................................................................................................................

   IMPLICIT                        NONE

      ! Passed variables
   REAL(DbKi), INTENT(IN)       :: ZTime                                           ! Current simulation time (s)
   REAL(DbKi), INTENT(IN)       :: TMax                                            ! Expected simulation time (s)
   REAL(DbKi), INTENT(INOUT)    :: PrevSimTime                                     ! Previous time message was written to screen (s > 0)
   REAL(ReKi), INTENT(INOUT)    :: PrevClockTime                                   ! Previous clock time in seconds past midnight


      ! Local variables.

   REAL(ReKi)                   :: CurrClockTime                                   ! Current time in seconds past midnight.
   REAL(ReKi)                   :: DeltTime                                        ! The amount of time elapsed since the last call.
   REAL(ReKi)                   :: EndTime                                         ! Approximate time of day when simulation will complete.
   REAL(ReKi), PARAMETER        :: InSecHr  = 1.0_ReKi/3600.0_ReKi                 ! Inverse of the number of seconds in an hour
   REAL(ReKi), PARAMETER        :: InSecMn  = 1.0_ReKi/  60.0_ReKi                 ! Inverse of the number of seconds in a minute
   REAL(ReKi)                   :: SimTimeLeft                                     ! Approximate clock time remaining before simulation completes

   REAL(ReKi), PARAMETER        :: SecPerDay = 24*60*60.0_ReKi                     ! Number of seconds per day

   INTEGER(4)                   :: EndHour                                         ! The hour when the simulations is expected to complete.
   INTEGER(4)                   :: EndMin                                          ! The minute when the simulations is expected to complete.
   INTEGER(4)                   :: EndSec                                          ! The second when the simulations is expected to complete.
   INTEGER(4)                   :: TimeAry  (8)                                    ! An array containing the elements of the start time.

   CHARACTER( 8)                :: ETimeStr                                        ! String containing the end time.


   IF ( ZTime <= PrevSimTime ) RETURN


      ! How many seconds past midnight?

   CALL DATE_AND_TIME ( Values=TimeAry )
   CurrClockTime = TimeValues2Seconds( TimeAry )

      ! Calculate elapsed clock time

   DeltTime = CurrClockTime - PrevClockTime


      ! We may have passed midnight since the last revoultion.  We will assume that (ZTime - PrevSimTime) of simulation time doesn't take more than a day.

   IF ( CurrClockTime < PrevClockTime )  THEN
      DeltTime = DeltTime + SecPerDay
   ENDIF


      ! Estimate the end time in hours, minutes, and seconds

   SimTimeLeft = REAL( ( TMax - ZTime )*DeltTime/( ZTime - PrevSimTime ), ReKi )          ! DeltTime/( ZTime - PrevSimTime ) is the delta_ClockTime divided by the delta_SimulationTime
   EndTime  =  MOD( CurrClockTime+SimTimeLeft, SecPerDay )
   EndHour  =  INT(   EndTime*InSecHr )
   EndMin   =  INT( ( EndTime - REAL( 3600*EndHour ) )*InSecMn )
   EndSec   = NINT(   EndTime - REAL( 3600*EndHour + 60*EndMin ) ) !bjj: this NINT can make the seconds say "60"

   WRITE (ETimeStr,"(I2.2,2(':',I2.2))")  EndHour, EndMin, EndSec

   CALL WrOver ( ' Timestep: '//TRIM( Num2LStr( NINT( ZTime ) ) )//' of '//TRIM( Num2LStr( TMax ) )// &
                 ' seconds.  Estimated final completion at '//ETimeStr//'.'                             )


      ! Let's save this time as the previous time for the next call to the routine
   PrevClockTime = CurrClockTime
   PrevSimTime   = ZTime

   RETURN
END SUBROUTINE SimStatus
!----------------------------------------------------------------------------------------------------------------------------------
FUNCTION TimeValues2Seconds( TimeAry )
! This routine takes an array of time values such as that returned from
!     CALL DATE_AND_TIME ( Values=TimeAry )
! and converts TimeAry to the number of seconds past midnight.
!..................................................................................................................................

      ! Passed variables:
   INTEGER, INTENT(IN)          :: TimeAry  (8)                                    ! An array containing the elements of the time
   REAL(ReKi)                   :: TimeValues2Seconds                              ! Current time in seconds past midnight


   TimeValues2Seconds = 3600*TimeAry(5) + 60*TimeAry(6) + TimeAry(7) + 0.001_ReKi*TimeAry(8)

END FUNCTION TimeValues2Seconds
!----------------------------------------------------------------------------------------------------------------------------------

!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE WrOutputLine( t, p_FAST, y_FAST, IfWOutput, EDOutput, SrvDOutput, HDOutput, SDOutput, MAPOutput, FEAMOutput, &
                        IceFOutput, y_IceD, ErrStat, ErrMsg)
! This routine writes the module output to the primary output file(s).
!..................................................................................................................................

   IMPLICIT                        NONE


      ! Passed variables
   REAL(DbKi), INTENT(IN)                  :: t                                  ! Current simulation time, in seconds
   TYPE(FAST_ParameterType), INTENT(IN)    :: p_FAST                             ! Glue-code simulation parameters
   TYPE(FAST_OutputType),    INTENT(INOUT) :: y_FAST                             ! Glue-code simulation outputs


   REAL(ReKi),               INTENT(IN)    :: IfWOutput (:)                      ! InflowWind WriteOutput values
   REAL(ReKi),               INTENT(IN)    :: EDOutput (:)                       ! ElastoDyn WriteOutput values
   REAL(ReKi),               INTENT(IN)    :: SrvDOutput (:)                     ! ServoDyn WriteOutput values
   REAL(ReKi),               INTENT(IN)    :: HDOutput (:)                       ! HydroDyn WriteOutput values
   REAL(ReKi),               INTENT(IN)    :: SDOutput (:)                       ! SubDyn WriteOutput values
   REAL(ReKi),               INTENT(IN)    :: MAPOutput (:)                      ! MAP WriteOutput values
   REAL(ReKi),               INTENT(IN)    :: FEAMOutput (:)                     ! FEAMooring WriteOutput values
   REAL(ReKi),               INTENT(IN)    :: IceFOutput (:)                     ! IceFloe WriteOutput values
   TYPE(IceD_OutputType),    INTENT(IN)    :: y_IceD (:)                         ! IceDyn outputs (WriteOutput values are subset)

   INTEGER(IntKi),           INTENT(OUT)   :: ErrStat
   CHARACTER(*),             INTENT(OUT)   :: ErrMsg

      ! Local variables.

   INTEGER(IntKi)                   :: i                                         ! loop counter
   INTEGER(IntKi)                   :: indxLast                                  ! The index of the last row value to be written to AllOutData for this time step (column).
   INTEGER(IntKi)                   :: indxNext                                  ! The index of the next row value to be written to AllOutData for this time step (column).

   CHARACTER(200)                   :: Frmt                                      ! A string to hold a format specifier
   CHARACTER(ChanLen)               :: TmpStr                                    ! temporary string to print the time output as text


   ErrStat = ErrID_None
   ErrMsg  = ''

   IF (p_FAST%WrTxtOutFile) THEN

         ! Write one line of tabular output:
   !   Frmt = '(F8.3,'//TRIM(Num2LStr(p%NumOuts))//'(:,A,'//TRIM( p%OutFmt )//'))'
      Frmt = '"'//p_FAST%Delim//'"'//p_FAST%OutFmt      ! format for array elements from individual modules

            ! time
      WRITE( TmpStr, '(F10.4)' ) t
      CALL WrFileNR( y_FAST%UnOu, TmpStr )


         ! write the individual module output
      IF ( y_FAST%numOuts(Module_IfW) > 0 ) THEN !Inflow Wind
         CALL WrReAryFileNR ( y_FAST%UnOu, IfWOutput,   Frmt, ErrStat, ErrMsg )
         IF ( ErrStat >= AbortErrLev ) RETURN
      END IF

      IF ( y_FAST%numOuts(Module_ED) > 0 ) THEN !ElastoDyn
         CALL WrReAryFileNR ( y_FAST%UnOu, EDOutput,   Frmt, ErrStat, ErrMsg )
         IF ( ErrStat >= AbortErrLev ) RETURN
      END IF

      IF ( y_FAST%numOuts(Module_AD) > 0 ) THEN !AeroDyn
        ! CALL WrReAryFileNR ( y_FAST%UnOu, ADOutput,   Frmt, ErrStat, ErrMsg )
        ! IF ( ErrStat >= AbortErrLev ) RETURN
      END IF
      
      IF ( y_FAST%numOuts(Module_SrvD) > 0 ) THEN !ServoDyn
         CALL WrReAryFileNR ( y_FAST%UnOu, SrvDOutput, Frmt, ErrStat, ErrMsg )
         IF ( ErrStat >= AbortErrLev ) RETURN
      END IF

      IF ( y_FAST%numOuts(Module_HD) > 0 ) THEN !HydroDyn
         CALL WrReAryFileNR ( y_FAST%UnOu, HDOutput,   Frmt, ErrStat, ErrMsg )
         IF ( ErrStat >= AbortErrLev ) RETURN
      END IF

      IF ( y_FAST%numOuts(Module_SD) > 0 ) THEN !SubDyn
         CALL WrReAryFileNR ( y_FAST%UnOu, SDOutput,   Frmt, ErrStat, ErrMsg )
         IF ( ErrStat >= AbortErrLev ) RETURN
      END IF
      
      IF ( y_FAST%numOuts(Module_MAP) > 0 ) THEN !MAP (Mooring Analysis Program)
         CALL WrReAryFileNR ( y_FAST%UnOu, MAPOutput,   Frmt, ErrStat, ErrMsg )
         IF ( ErrStat >= AbortErrLev ) RETURN
      ELSEIF ( y_FAST%numOuts(Module_FEAM) > 0 ) THEN !FEAMooring
         CALL WrReAryFileNR ( y_FAST%UnOu, FEAMOutput,   Frmt, ErrStat, ErrMsg )
         IF ( ErrStat >= AbortErrLev ) RETURN
      END IF

      IF ( y_FAST%numOuts(Module_IceF) > 0 ) THEN !IceFloe
         CALL WrReAryFileNR ( y_FAST%UnOu, IceFOutput,   Frmt, ErrStat, ErrMsg )
         IF ( ErrStat >= AbortErrLev ) RETURN
      ELSEIF ( y_FAST%numOuts(Module_IceD) > 0 ) THEN !IceDyn
         DO i=1,p_FAST%numIceLegs
            CALL WrReAryFileNR ( y_FAST%UnOu, y_IceD(i)%WriteOutput,   Frmt, ErrStat, ErrMsg )
            IF ( ErrStat >= AbortErrLev ) RETURN
         END DO
      END IF      
      
      
         ! write a new line (advance to the next line)
      WRITE (y_FAST%UnOu,'()')

   END IF


   IF (p_FAST%WrBinOutFile) THEN

         ! Write data to array for binary output file

      IF ( y_FAST%n_Out == y_FAST%NOutSteps ) THEN
         CALL ProgWarn( 'Not all data could be written to the binary output file.' )
         !this really would only happen if we have an error somewhere else, right?
         !otherwise, we could allocate a new, larger array and move existing data
      ELSE
         y_FAST%n_Out = y_FAST%n_Out + 1

            ! store time data
         IF ( y_FAST%n_Out == 1_IntKi .OR. OutputFileFmtID == FileFmtID_WithTime ) THEN
            y_FAST%TimeData(y_FAST%n_Out) = t   ! Time associated with these outputs
         END IF

            ! store individual module data

         indxLast = 0
         indxNext = 1

         IF ( y_FAST%numOuts(Module_IfW) > 0 ) THEN
            indxLast = indxNext + SIZE(IfWOutput) - 1
            y_FAST%AllOutData(indxNext:indxLast, y_FAST%n_Out) = IfWOutput
            indxNext = IndxLast + 1
         END IF

         IF ( y_FAST%numOuts(Module_ED) > 0 ) THEN
            indxLast = indxNext + SIZE(EDOutput) - 1
            y_FAST%AllOutData(indxNext:indxLast, y_FAST%n_Out) = EDOutput
            indxNext = IndxLast + 1
         END IF
         
!AD would be next:
         !IF ( y_FAST%numOuts(Module_AD) > 0 ) THEN
         !   indxLast = indxNext + SIZE(ADOutput) - 1
         !   y_FAST%AllOutData(indxNext:indxLast, y_FAST%n_Out) = ADOutput
         !   indxNext = IndxLast + 1
         !END IF
         
         IF ( y_FAST%numOuts(Module_SrvD) > 0 ) THEN
            indxLast = indxNext + SIZE(SrvDOutput) - 1
            y_FAST%AllOutData(indxNext:indxLast, y_FAST%n_Out) = SrvDOutput
            indxNext = IndxLast + 1
         END IF

         IF ( y_FAST%numOuts(Module_HD) > 0 ) THEN
            indxLast = indxNext + SIZE(HDOutput) - 1
            y_FAST%AllOutData(indxNext:indxLast, y_FAST%n_Out) = HDOutput
            indxNext = IndxLast + 1
         END IF

         IF ( y_FAST%numOuts(Module_SD) > 0 ) THEN
            indxLast = indxNext + SIZE(SDOutput) - 1
            y_FAST%AllOutData(indxNext:indxLast, y_FAST%n_Out) = SDOutput
            indxNext = IndxLast + 1
         END IF
                  
         IF ( y_FAST%numOuts(Module_MAP) > 0 ) THEN
            indxLast = indxNext + SIZE(MAPOutput) - 1
            y_FAST%AllOutData(indxNext:indxLast, y_FAST%n_Out) = MAPOutput
            indxNext = IndxLast + 1
         ELSEIF ( y_FAST%numOuts(Module_FEAM) > 0 ) THEN
            indxLast = indxNext + SIZE(FEAMOutput) - 1
            y_FAST%AllOutData(indxNext:indxLast, y_FAST%n_Out) = FEAMOutput
            indxNext = IndxLast + 1
         END IF
         
         IF ( y_FAST%numOuts(Module_IceF) > 0 ) THEN
            indxLast = indxNext + SIZE(IceFOutput) - 1
            y_FAST%AllOutData(indxNext:indxLast, y_FAST%n_Out) = IceFOutput
            indxNext = IndxLast + 1
         ELSEIF ( y_FAST%numOuts(Module_IceD) > 0 ) THEN
            DO i=1,p_FAST%numIceLegs
               indxLast = indxNext + SIZE(y_IceD(i)%WriteOutput) - 1
               y_FAST%AllOutData(indxNext:indxLast, y_FAST%n_Out) = y_IceD(i)%WriteOutput
               indxNext = IndxLast + 1
            END DO            
         END IF     
         
      END IF      

         
   END IF

   RETURN
END SUBROUTINE WrOutputLine
!----------------------------------------------------------------------------------------------------------------------------------


!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ED_InputSolve( p_FAST, u_ED, y_ED, y_AD, y_SrvD, MeshMapData, ErrStat, ErrMsg )
! This routine sets the inputs required for ED--using the Option 2 solve method; currently the only input not solved in this routine
! ar the fields on PlatformPtMesh,  which is solved in option 1.
!..................................................................................................................................

   TYPE(FAST_ParameterType),       INTENT(IN   )  :: p_FAST                   ! Glue-code simulation parameters
   TYPE(ED_InputType),             INTENT(INOUT)  :: u_ED                     ! ED Inputs at t
   TYPE(ED_OutputType),            INTENT(IN   )  :: y_ED                     ! ElastoDyn outputs (need translation displacement on meshes for loads mapping)
   TYPE(AD_OutputType),            INTENT(IN   )  :: y_AD                     ! AeroDyn outputs
   TYPE(SrvD_OutputType),          INTENT(IN   )  :: y_SrvD                   ! ServoDyn outputs
   
   TYPE(FAST_ModuleMapType),       INTENT(INOUT)  :: MeshMapData              ! Data for mapping between modules
   INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat                  ! Error status
   CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg                   ! Error message
   
      ! local variables
   INTEGER(IntKi)                                 :: J                        ! Loops through nodes / elements
   INTEGER(IntKi)                                 :: K                        ! Loops through blades
   INTEGER(IntKi)                                 :: NodeNum                  ! Node number for blade/element in mesh
   TYPE(MeshType)                                 :: u_mapped                 ! interpolated value of input
   INTEGER(IntKi)                                 :: ErrStat2                 ! temporary Error status of the operation
   CHARACTER(LEN(ErrMsg))                         :: ErrMsg2                  ! temporary Error message if ErrStat /= ErrID_None


      ! Initialize error status
      
   ErrStat = ErrID_None
   ErrMsg = ""

   
      ! ED inputs from ServoDyn
   IF ( p_FAST%CompServo == Module_SrvD ) THEN

      u_ED%GenTrq     = y_SrvD%GenTrq
      u_ED%HSSBrTrq   = y_SrvD%HSSBrTrq
      u_ED%BlPitchCom = y_SrvD%BlPitchCom
      u_ED%YawMom     = y_SrvD%YawMom
   !   u_ED%TBDrCon    = y_SrvD%TBDrCon !array
   ELSE !we'll just take the initial guesses..
   END IF

   
   
      ! ED inputs from UserTwrLd

   !IF ( p_FAST%CompUserTwrLd ) THEN
   !   u_ED%TwrAddedMass(:,:,J) = y_UsrTwr%AddedMass(:,:,J)
   !   u_ED%TowerLn2Mesh%Force  = y_UsrTwr%Force
   !   u_ED%TowerLn2Mesh%Moment = y_UsrTwr%Moment
   !ELSE
      u_ED%TwrAddedMass        = 0.0_ReKi
      u_ED%TowerLn2Mesh%Force  = 0.0_ReKi
      u_ED%TowerLn2Mesh%Moment = 0.0_ReKi
   !END IF
   
   
   
      ! ED inputs from AeroDyn
!   IF ( p_FAST%CompAero == Module_AD .and. ALLOCATED(ADAeroLoads%Blade) ) THEN
!bjj: need another check on this perhaps
   IF ( p_FAST%CompAero == Module_AD ) THEN
      DO K = 1,SIZE(u_ED%BladeLn2Mesh,1) ! Loop through all blades (p_ED%NumBl)
         DO J = 1,y_AD%OutputLoads(K)%Nnodes ! Loop through the blade nodes / elements (p_ED%BldNodes)

            u_ED%BladeLn2Mesh(K)%Force(:,J)  = y_AD%OutputLoads(K)%Force(:,J)
            u_ED%BladeLn2Mesh(K)%Moment(:,J) = y_AD%OutputLoads(K)%Moment(:,J)
            
         END DO !J
      END DO   !K
      
      
         ! add aero force to the tower, if it's provided:
      IF ( y_AD%Twr_OutputLoads%Committed ) THEN
      
         ! we're mapping loads, so we also need the sibling meshes' displacements:
         
   !      CALL Transfer_Line2_to_Line2( )
      
         J = y_AD%Twr_OutputLoads%NNodes
         
         IF (y_AD%Twr_OutputLoads%FIELDMASK(MASKID_FORCE) ) &
            u_ED%TowerLn2Mesh%Force(:,1:J)  = u_ED%TowerLn2Mesh%Force( :,1:J) + y_AD%Twr_OutputLoads%Force
         
         IF (y_AD%Twr_OutputLoads%FIELDMASK(MASKID_MOMENT) ) &
            u_ED%TowerLn2Mesh%Moment(:,1:J) = u_ED%TowerLn2Mesh%Moment(:,1:J) + y_AD%Twr_OutputLoads%Moment 
      
      END IF   
         
   ELSE
      DO K = 1,SIZE(u_ED%BladeLn2Mesh,1) ! Loop through all blades (p_ED%NumBl)
         u_ED%BladeLn2Mesh(K)%Force  = 0.0_ReKi
         u_ED%BladeLn2Mesh(K)%Moment = 0.0_ReKi
      END DO
   END IF
   
   u_ED%PtfmAddedMass = 0.0_ReKi
   
         
   
CONTAINS
   !...............................................................................................................................
   SUBROUTINE CheckError(ErrID,Msg)
   ! This subroutine sets the error message and level and cleans up if the error is >= AbortErrLev
   !...............................................................................................................................

         ! Passed arguments
      INTEGER(IntKi), INTENT(IN) :: ErrID       ! The error identifier (ErrStat)
      CHARACTER(*),   INTENT(IN) :: Msg         ! The error message (ErrMsg)

      INTEGER(IntKi)             :: ErrStat3    ! The error identifier (ErrStat)
      CHARACTER(1024)            :: ErrMsg3     ! The error message (ErrMsg)

      !............................................................................................................................
      ! Set error status/message;
      !............................................................................................................................

      IF ( ErrID /= ErrID_None ) THEN

         CALL WrScr( ' ED_InputSolve:'//TRIM(Msg) )

         !.........................................................................................................................
         ! Clean up if we're going to return on error: close files, deallocate local arrays
         !.........................................................................................................................
         !IF ( ErrStat >= AbortErrLev ) THEN
         !END IF
         !
      END IF


   END SUBROUTINE CheckError

END SUBROUTINE ED_InputSolve
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE SrvD_InputSolve( p_FAST, u_SrvD, y_ED, y_IfW, y_SrvD_prev )
! This routine sets the inputs required for ServoDyn
!..................................................................................................................................

   TYPE(FAST_ParameterType),         INTENT(IN)     :: p_FAST       ! Glue-code simulation parameters
   TYPE(SrvD_InputType),             INTENT(INOUT)  :: u_SrvD       ! ServoDyn Inputs at t
   TYPE(ED_OutputType),              INTENT(IN)     :: y_ED         ! ElastoDyn outputs
   REAL(ReKi),                       INTENT(IN)     :: y_IfW(3)     ! InflowWind outputs
   TYPE(SrvD_OutputType), OPTIONAL,  INTENT(IN)     :: y_SrvD_prev  ! ServoDyn outputs from t - dt
!  TYPE(AD_OutputType),              INTENT(IN)     :: y_AD         ! AeroDyn outputs


      ! ServoDyn inputs from combination of InflowWind and ElastoDyn



   u_SrvD%YawAngle  = y_ED%YawAngle !nacelle yaw plus platform yaw

      ! Calculate horizontal hub-height wind direction and the nacelle yaw error estimate (both positive about zi-axis); these are
      !   zero if there is no wind input when AeroDyn is not used:

      !bjj: rename pass YawAngle (not YawErr from ED)
   IF ( p_FAST%CompAero == Module_AD )  THEN   ! AeroDyn has been used.

      u_SrvD%WindDir  = ATAN2( y_IfW(2), y_IfW(1) )
      u_SrvD%YawErr   = u_SrvD%WindDir - y_ED%YawAngle
      u_SrvD%HorWindV = SQRT( y_IfW(1)**2 + y_IfW(2)**2 )

   ELSE                    ! No AeroDynamics.

      u_SrvD%WindDir  = 0.0
      u_SrvD%YawErr   = 0.0
      u_SrvD%HorWindV = 0.0

   ENDIF

      ! ServoDyn inputs from ServoDyn outputs at previous step
      ! Jason says this violates the framework, but it's only for the Bladed DLL, which itself violates the framework, so I don't care.
   IF (PRESENT(y_SrvD_prev)) THEN
      u_SrvD%ElecPwr_prev = y_SrvD_prev%ElecPwr  ! we want to know the electrical power from the previous time step  (for the Bladed DLL)
      u_SrvD%GenTrq_prev  = y_SrvD_prev%GenTrq   ! we want to know the electrical generator torque from the previous time step  (for the Bladed DLL)
   ! Otherwise, we'll use the guess provided by the module (this only happens at Step=0)
   END IF

      ! ServoDyn inputs from ElastoDyn
   u_SrvD%Yaw       = y_ED%Yaw  !nacelle yaw
   u_SrvD%YawRate   = y_ED%YawRate
   u_SrvD%BlPitch   = y_ED%BlPitch
   u_SrvD%LSS_Spd   = y_ED%LSS_Spd
   u_SrvD%HSS_Spd   = y_ED%HSS_Spd
   u_SrvD%RotSpeed  = y_ED%RotSpeed
   u_SrvD%RootMxc   = y_ED%RootMxc
   u_SrvD%RootMyc   = y_ED%RootMyc
   u_SrvD%YawBrTAxp = y_ED%YawBrTAxp
   u_SrvD%YawBrTAyp = y_ED%YawBrTAyp
   u_SrvD%LSSTipPxa = y_ED%LSSTipPxa

   u_SrvD%LSSTipMya = y_ED%LSSTipMya
   u_SrvD%LSSTipMza = y_ED%LSSTipMza
   u_SrvD%LSSTipMys = y_ED%LSSTipMys
   u_SrvD%LSSTipMzs = y_ED%LSSTipMzs
   u_SrvD%YawBrMyn  = y_ED%YawBrMyn
   u_SrvD%YawBrMzn  = y_ED%YawBrMzn
   u_SrvD%NcIMURAxs = y_ED%NcIMURAxs
   u_SrvD%NcIMURAys = y_ED%NcIMURAys
   u_SrvD%NcIMURAzs = y_ED%NcIMURAzs

   u_SrvD%RotPwr    = y_ED%RotPwr

   !   ! ServoDyn inputs from AeroDyn
   !IF ( p_FAST%CompAero == Module_AD ) THEN
   !ELSE
   !END IF
   !

END SUBROUTINE SrvD_InputSolve
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE Transfer_SD_to_HD( y_SD, u_HD_M_LumpedMesh, u_HD_M_DistribMesh, MeshMapData, ErrStat, ErrMsg )
! This routine transfers the SD outputs into inputs required for HD
!..................................................................................................................................
   TYPE(SD_OutputType),         INTENT(IN   ) :: y_SD                         ! The outputs of the structural dynamics module
   TYPE(MeshType),              INTENT(INOUT) :: u_HD_M_LumpedMesh            ! HydroDyn input mesh (separated here so that we can use temp meshes in ED_SD_HD_InputSolve)
   TYPE(MeshType),              INTENT(INOUT) :: u_HD_M_DistribMesh           ! HydroDyn input mesh (separated here so that we can use temp meshes in ED_SD_HD_InputSolve)
   TYPE(FAST_ModuleMapType),    INTENT(INOUT) :: MeshMapData

   INTEGER(IntKi),              INTENT(  OUT) :: ErrStat                      ! Error status of the operation
   CHARACTER(*),                INTENT(  OUT) :: ErrMsg                       ! Error message if ErrStat /= ErrID_None
   
      ! local variables
   INTEGER(IntKi)                             :: ErrStat2                     ! temporary Error status of the operation
   CHARACTER(LEN(ErrMsg))                     :: ErrMsg2                      ! temporary Error message if ErrStat /= ErrID_None
      
      
   ErrStat = ErrID_None
   ErrMsg = ""
   
       
   IF ( u_HD_M_LumpedMesh%Committed ) THEN 

      ! These are the motions for the lumped point loads associated viscous drag on the WAMIT body and/or filled/flooded lumped forces of the WAMIT body
      CALL Transfer_Point_to_Point( y_SD%y2Mesh, u_HD_M_LumpedMesh, MeshMapData%SD_P_2_HD_M_P, ErrStat2, ErrMsg2 )
         IF (ErrStat /= ErrID_None)  THEN
            IF ( LEN_TRIM(ErrMsg) > 0 ) ErrMsg = TRIM(ErrMsg)//NewLine
            ErrMsg = TRIM(ErrMsg)//' Transfer_SD_to_HD (u_HD%Morison%LumpedMesh):'//TRIM(ErrMsg2)
            ErrStat = MAX(ErrStat, ErrStat2)
         END IF
         
   END IF
   
   IF ( u_HD_M_DistribMesh%Committed ) THEN 
         
      ! These are the motions for the HD line2 (distributed) loads associated viscous drag on the WAMIT body and/or filled/flooded distributed forces of the WAMIT body
      CALL Transfer_Point_to_Line2( y_SD%y2Mesh, u_HD_M_DistribMesh, MeshMapData%SD_P_2_HD_M_L, ErrStat2, ErrMsg2 )
         IF (ErrStat /= ErrID_None)  THEN
            IF ( LEN_TRIM(ErrMsg) > 0 ) ErrMsg = TRIM(ErrMsg)//NewLine
            ErrMsg = TRIM(ErrMsg)//' Transfer_SD_to_HD (u_HD%Morison%DistribMesh):'//TRIM(ErrMsg2)
            ErrStat = MAX(ErrStat, ErrStat2)
         END IF

   END IF
   
END SUBROUTINE Transfer_SD_to_HD
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE Transfer_ED_to_HD( y_ED, u_HD, MeshMapData, ErrStat, ErrMsg )
! This routine transfers the ED outputs into inputs required for HD
!..................................................................................................................................
   TYPE(ED_OutputType),         INTENT(IN   ) :: y_ED                         ! The outputs of the structural dynamics module
   TYPE(HydroDyn_InputType),    INTENT(INOUT) :: u_HD                         ! HydroDyn input
   TYPE(FAST_ModuleMapType),    INTENT(INOUT) :: MeshMapData

   INTEGER(IntKi),              INTENT(OUT)   :: ErrStat                      ! Error status of the operation
   CHARACTER(*),                INTENT(OUT)   :: ErrMsg                       ! Error message if ErrStat /= ErrID_None
   
      ! local variables
   INTEGER(IntKi)                             :: ErrStat2                     ! temporary Error status of the operation
   CHARACTER(LEN(ErrMsg))                     :: ErrMsg2                      ! temporary Error message if ErrStat /= ErrID_None
      
      
   ErrStat = ErrID_None
   ErrMsg = ""
   
   !bjj: We do this without all the extra meshcopy/destroy calls with u_mapped because these inputs are only from one mesh
   
   IF ( u_HD%Mesh%Committed ) THEN

      ! These are the motions for the lumped point loads associated the WAMIT body and include: hydrostatics, radiation memory effect,
      !    wave kinematics, additional preload, additional stiffness, additional linear damping, additional quadratic damping,
      !    hydrodynamic added mass

      CALL Transfer_Point_to_Point( y_ED%PlatformPtMesh, u_HD%Mesh, MeshMapData%ED_P_2_HD_W_P, ErrStat2, ErrMsg2 )
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat, ErrMsg,'Transfer_ED_to_HD (u_HD%Mesh)' )

   END IF !WAMIT
   
   
   IF ( u_HD%Morison%LumpedMesh%Committed ) THEN 

      ! These are the motions for the lumped point loads associated viscous drag on the WAMIT body and/or filled/flooded lumped forces of the WAMIT body
      CALL Transfer_Point_to_Point( y_ED%PlatformPtMesh, u_HD%Morison%LumpedMesh, MeshMapData%ED_P_2_HD_M_P, ErrStat2, ErrMsg2 )
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat, ErrMsg,'Transfer_ED_to_HD (u_HD%Morison%LumpedMesh)' )
         
   END IF
   
   IF ( u_HD%Morison%DistribMesh%Committed ) THEN 
         
      ! These are the motions for the line2 (distributed) loads associated viscous drag on the WAMIT body and/or filled/flooded distributed forces of the WAMIT body
      CALL Transfer_Point_to_Line2( y_ED%PlatformPtMesh, u_HD%Morison%DistribMesh, MeshMapData%ED_P_2_HD_M_L, ErrStat2, ErrMsg2 )
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat, ErrMsg,'Transfer_ED_to_HD (u_HD%Morison%DistribMesh)' )

   END IF
   
END SUBROUTINE Transfer_ED_to_HD
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE Transfer_ED_to_HD_SD_Mooring( p_FAST, y_ED, u_HD, u_SD, u_MAP, u_FEAM, MeshMapData, ErrStat, ErrMsg )
! This routine transfers the ED outputs into inputs required for HD, SD, MAP, and/or FEAM
!..................................................................................................................................
   TYPE(FAST_ParameterType),    INTENT(IN)    :: p_FAST                       ! Glue-code simulation parameters
   TYPE(ED_OutputType),         INTENT(IN   ) :: y_ED                         ! The outputs of the structural dynamics module
   TYPE(HydroDyn_InputType),    INTENT(INOUT) :: u_HD                         ! HydroDyn input
   TYPE(SD_InputType),          INTENT(INOUT) :: u_SD                         ! SubDyn input
   TYPE(MAP_InputType),         INTENT(INOUT) :: u_MAP                        ! MAP input
   TYPE(FEAM_InputType),        INTENT(INOUT) :: u_FEAM                       ! FEAM input
   TYPE(FAST_ModuleMapType),    INTENT(INOUT) :: MeshMapData

   INTEGER(IntKi),              INTENT(OUT)   :: ErrStat                      ! Error status of the operation
   CHARACTER(*),                INTENT(OUT)   :: ErrMsg                       ! Error message if ErrStat /= ErrID_None
   
      ! local variables
   INTEGER(IntKi)                             :: ErrStat2                     ! temporary Error status of the operation
   CHARACTER(LEN(ErrMsg))                     :: ErrMsg2                      ! temporary Error message if ErrStat /= ErrID_None
      
      
   ErrStat = ErrID_None
   ErrMsg = ""
     
      ! transfer ED outputs to other modules used in option 1:
            
   IF ( p_FAST%CompSub == Module_SD  ) THEN
      
         ! Map ED (motion) outputs to SD inputs:                     
      CALL Transfer_Point_to_Point( y_ED%PlatformPtMesh, u_SD%TPMesh, MeshMapData%ED_P_2_SD_TP, ErrStat2, ErrMsg2 ) 
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat, ErrMsg,'Transfer_ED_to_HD_SD_Mooring (u_SD%TPMesh)' )
               
         
         ! HD's other inputs are set by SD:
         
      !IF ( p_FAST%CompHydro == Module_HD  ) THEN
      !      
      !      ! SD motions to HD:
      !   CALL Transfer_SD_to_HD( y_SD, u_HD%Morison%LumpedMesh, u_HD%Morison%DistribMesh, MeshMapData, ErrStat, ErrMsg )
      !      CALL ChecEkrror( ErrStat, 'Message from Transfer_ED_to_HD_SD_Mooring: '//NewLine//ErrMsg  )    
      !                                                                            
      !END IF         
         
                  
   ELSEIF ( p_FAST%CompHydro == Module_HD ) THEN
         ! Map ED outputs to HD inputs:
      CALL Transfer_ED_to_HD( y_ED, u_HD, MeshMapData, ErrStat2, ErrMsg2 )                        
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat, ErrMsg,'Transfer_ED_to_HD_SD_Mooring' )
            
   END IF      
   
   
   IF ( p_FAST%CompMooring == Module_MAP ) THEN
      
         ! motions:
      CALL Transfer_Point_to_Point( y_ED%PlatformPtMesh, u_MAP%PtFairleadDisplacement, MeshMapData%ED_P_2_MAP_P, ErrStat, ErrMsg )
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat, ErrMsg,'Transfer_ED_to_HD_SD_Mooring (u_MAP%PtFairleadDisplacement)' )
                                 
   ELSEIF ( p_FAST%CompMooring == Module_FEAM ) THEN
         ! motions:
      CALL Transfer_Point_to_Point( y_ED%PlatformPtMesh, u_FEAM%PtFairleadDisplacement, MeshMapData%ED_P_2_FEAM_P, ErrStat, ErrMsg )
         CALL SetErrStat(ErrStat2,ErrMsg2,ErrStat, ErrMsg,'Transfer_ED_to_HD_SD_Mooring (u_FEAM%PtFairleadDisplacement)' )
                        
   END IF
   
            
END SUBROUTINE Transfer_ED_to_HD_SD_Mooring
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE MAP_InputSolve(  u_MAP, y_ED, MeshMapData, ErrStat, ErrMsg )
! This routine sets the inputs required for MAP.
!..................................................................................................................................

      ! Passed variables
   TYPE(MAP_InputType),         INTENT(INOUT) :: u_MAP                        ! MAP input
   TYPE(ED_OutputType),         INTENT(IN   ) :: y_ED                         ! The outputs of the structural dynamics module
   TYPE(FAST_ModuleMapType),    INTENT(INOUT) :: MeshMapData

   INTEGER(IntKi),              INTENT(  OUT) :: ErrStat                      ! Error status of the operation
   CHARACTER(*)  ,              INTENT(  OUT) :: ErrMsg                       ! Error message if ErrStat /= ErrID_None


      !----------------------------------------------------------------------------------------------------
      ! Map ED outputs to MAP inputs
      !----------------------------------------------------------------------------------------------------
      ! motions:
   CALL Transfer_Point_to_Point( y_ED%PlatformPtMesh, u_MAP%PtFairleadDisplacement, MeshMapData%ED_P_2_MAP_P, ErrStat, ErrMsg )


END SUBROUTINE MAP_InputSolve
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE FEAM_InputSolve(  u_FEAM, y_ED, MeshMapData, ErrStat, ErrMsg )
! This routine sets the inputs required for FEAM.
!..................................................................................................................................

      ! Passed variables
   TYPE(FEAM_InputType),        INTENT(INOUT) :: u_FEAM                       ! FEAM input
   TYPE(ED_OutputType),         INTENT(IN   ) :: y_ED                         ! The outputs of the structural dynamics module
   TYPE(FAST_ModuleMapType),    INTENT(INOUT) :: MeshMapData

   INTEGER(IntKi),              INTENT(  OUT) :: ErrStat                      ! Error status of the operation
   CHARACTER(*)  ,              INTENT(  OUT) :: ErrMsg                       ! Error message if ErrStat /= ErrID_None


      !----------------------------------------------------------------------------------------------------
      ! Map ED outputs to MAP inputs
      !----------------------------------------------------------------------------------------------------
      ! motions:
   CALL Transfer_Point_to_Point( y_ED%PlatformPtMesh, u_FEAM%PtFairleadDisplacement, MeshMapData%ED_P_2_FEAM_P, ErrStat, ErrMsg )


END SUBROUTINE FEAM_InputSolve
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE IceFloe_InputSolve(  u_IceF, y_SD, MeshMapData, ErrStat, ErrMsg )
! This routine sets the inputs required for IceFloe.
!..................................................................................................................................

      ! Passed variables
   TYPE(IceFloe_InputType),     INTENT(INOUT) :: u_IceF                       ! IceFloe input
   TYPE(SD_OutputType),         INTENT(IN   ) :: y_SD                         ! SubDyn outputs
   TYPE(FAST_ModuleMapType),    INTENT(INOUT) :: MeshMapData

   INTEGER(IntKi),              INTENT(  OUT) :: ErrStat                      ! Error status of the operation
   CHARACTER(*)  ,              INTENT(  OUT) :: ErrMsg                       ! Error message if ErrStat /= ErrID_None


      !----------------------------------------------------------------------------------------------------
      ! Map SD outputs to IceFloe inputs
      !----------------------------------------------------------------------------------------------------
      ! motions:
   CALL Transfer_Point_to_Point( y_SD%y2Mesh, u_IceF%IceMesh, MeshMapData%SD_P_2_IceF_P, ErrStat, ErrMsg )

END SUBROUTINE IceFloe_InputSolve
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE IceD_InputSolve(  u_IceD, y_SD, MeshMapData, legNum, ErrStat, ErrMsg )
! This routine sets the inputs required for IceFloe.
!..................................................................................................................................

      ! Passed variables
   TYPE(IceD_InputType),        INTENT(INOUT) :: u_IceD                       ! IceDyn input
   TYPE(SD_OutputType),         INTENT(IN   ) :: y_SD                         ! SubDyn outputs
   TYPE(FAST_ModuleMapType),    INTENT(INOUT) :: MeshMapData
   INTEGER(IntKi),              INTENT(IN   ) :: legNum

   INTEGER(IntKi),              INTENT(  OUT) :: ErrStat                      ! Error status of the operation
   CHARACTER(*)  ,              INTENT(  OUT) :: ErrMsg                       ! Error message if ErrStat /= ErrID_None


      !----------------------------------------------------------------------------------------------------
      ! Map SD outputs to IceFloe inputs
      !----------------------------------------------------------------------------------------------------
      ! motions:
   CALL Transfer_Point_to_Point( y_SD%y2Mesh, u_IceD%PointMesh, MeshMapData%SD_P_2_IceD_P(legNum), ErrStat, ErrMsg )

END SUBROUTINE IceD_InputSolve
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE Transfer_HD_to_SD( u_mapped, u_SD_LMesh, u_mapped_positions, y_HD, u_HD_M_LumpedMesh, u_HD_M_DistribMesh, MeshMapData, ErrStat, ErrMsg )
! This routine transfers the HD outputs into inputs required for ED. Note that this *adds* to the values already in 
! u_SD_LMesh (so initialize it before calling this routine).
!..................................................................................................................................
   TYPE(MeshType),                 INTENT(INOUT)  :: u_mapped                 ! temporary copy of SD mesh (an argument to avoid another temporary mesh copy)
   TYPE(MeshType),                 INTENT(INOUT)  :: u_SD_LMesh               ! SD Inputs on LMesh at t (separate so we can call from ED_SD_HD_InputOutput solve with temp meshes)
   TYPE(MeshType),                 INTENT(IN   )  :: u_mapped_positions       ! Mesh sibling of u_mapped, with displaced positions
   TYPE(HydroDyn_OutputType),      INTENT(IN   )  :: y_HD                     ! HydroDyn outputs
   TYPE(MeshType),                 INTENT(IN   )  :: u_HD_M_LumpedMesh        ! HydroDyn input mesh (separate so we can call from ED_SD_HD_InputOutput solve with temp meshes)
   TYPE(MeshType),                 INTENT(IN   )  :: u_HD_M_DistribMesh       ! HydroDyn input mesh (separate so we can call from ED_SD_HD_InputOutput solve with temp meshes)
   
   TYPE(FAST_ModuleMapType),       INTENT(INOUT)  :: MeshMapData              ! Data for mapping between modules
   INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat                  ! Error status
   CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg                   ! Error message
   
      ! local variables
   INTEGER(IntKi)                                 :: ErrStat2                  ! temporary Error status of the operation
   CHARACTER(LEN(ErrMsg))                         :: ErrMsg2                   ! temporary Error message if ErrStat /= ErrID_None

      
   ErrStat = ErrID_None
   ErrMsg = ""
         
   !assumes u_SD%LMesh%Committed   (i.e., u_SD_LMesh%Committed)
         
      IF ( y_HD%Morison%LumpedMesh%Committed ) THEN      
         ! we're mapping loads, so we also need the sibling meshes' displacements:
         CALL Transfer_Point_to_Point( y_HD%Morison%LumpedMesh, u_mapped, MeshMapData%HD_M_P_2_SD_P, ErrStat2, ErrMsg2, u_HD_M_LumpedMesh, u_mapped_positions )   
            CALL CheckError( ErrStat2, ErrMsg2 )
            IF (ErrStat >= AbortErrLev) RETURN
         
         u_SD_LMesh%Force  = u_SD_LMesh%Force  + u_mapped%Force
         u_SD_LMesh%Moment = u_SD_LMesh%Moment + u_mapped%Moment     
         
#ifdef DEBUG_MESH_TRANSFER              
         CALL WrScr('********************************************************')
         CALL WrScr('****   SD to HD point-to-point (morison lumped)    *****')
         CALL WrScr('********************************************************')
         CALL WriteMappingTransferToFile(u_mapped, u_mapped_positions, u_HD_M_LumpedMesh, y_HD%Morison%LumpedMesh,&
               MeshMapData%SD_P_2_HD_M_P, MeshMapData%HD_M_P_2_SD_P, &
               'SD_y2_HD_ML_Meshes_t'//TRIM(Num2LStr(0))//'.bin' )
         !print *
         !pause
         
#endif         
                  
      END IF
      
      IF ( y_HD%Morison%DistribMesh%Committed ) THEN      
         ! we're mapping loads, so we also need the sibling meshes' displacements:
         CALL Transfer_Line2_to_Point( y_HD%Morison%DistribMesh, u_mapped, MeshMapData%HD_M_L_2_SD_P, ErrStat2, ErrMsg2, u_HD_M_DistribMesh, u_mapped_positions )   
            CALL CheckError( ErrStat2, ErrMsg2 )
            IF (ErrStat >= AbortErrLev) RETURN
         
         u_SD_LMesh%Force  = u_SD_LMesh%Force  + u_mapped%Force
         u_SD_LMesh%Moment = u_SD_LMesh%Moment + u_mapped%Moment

#ifdef DEBUG_MESH_TRANSFER        
         CALL WrScr('********************************************************')
         CALL WrScr('**** SD to HD point-to-line2 (morison distributed) *****')
         CALL WrScr('********************************************************')
         CALL WriteMappingTransferToFile(u_mapped, u_mapped_positions, u_HD_M_DistribMesh,y_HD%Morison%DistribMesh,&
               MeshMapData%SD_P_2_HD_M_L, MeshMapData%HD_M_L_2_SD_P, &
               'SD_y2_HD_MD_Meshes_t'//TRIM(Num2LStr(0))//'.bin' )         
         !print *
        ! pause
#endif         
                  
      END IF
                                             
      
CONTAINS   
   !...............................................................................................................................
   SUBROUTINE CheckError(ErrID,Msg)
   ! This subroutine sets the error message and level and cleans up if the error is >= AbortErrLev
   !...............................................................................................................................

         ! Passed arguments
      INTEGER(IntKi), INTENT(IN) :: ErrID       ! The error identifier (ErrStat)
      CHARACTER(*),   INTENT(IN) :: Msg         ! The error message (ErrMsg)

      INTEGER(IntKi)             :: ErrStat3    ! The error identifier (ErrStat)
      CHARACTER(1024)            :: ErrMsg3     ! The error message (ErrMsg)

      !............................................................................................................................
      ! Set error status/message;
      !............................................................................................................................
      
      IF ( ErrID /= ErrID_None ) THEN

         IF (ErrStat /= ErrID_None) ErrMsg = TRIM(ErrMsg)//NewLine
         ErrMsg = TRIM(ErrMsg)//'Transfer_HD_to_SD:'//TRIM(Msg)
         ErrStat = MAX(ErrStat, ErrID)
         
         !.........................................................................................................................
         ! Clean up if we're going to return on error: close files, deallocate local arrays
         !.........................................................................................................................
      END IF

   END SUBROUTINE CheckError
   
END SUBROUTINE Transfer_HD_to_SD
!----------------------------------------------------------------------------------------------------------------------------------
REAL(ReKi) FUNCTION GetPerturb(x)
   REAL(ReKi), INTENT(IN) :: x
      
   !GetPerturb = sqrt( EPSILON(x)) * max( abs(x, 1._ReKi)  
!      GetPerturb = 1.0e6
   GetPerturb = 1.0
      
END FUNCTION GetPerturb
!----------------------------------------------------------------------------------------------------------------------------------

SUBROUTINE ED_HD_InputOutputSolve(  this_time, p_FAST, calcJacobian &
                                  , u_ED, p_ED, x_ED, xd_ED, z_ED, OtherSt_ED, y_ED &
                                  , u_HD, p_HD, x_HD, xd_HD, z_HD, OtherSt_HD, y_HD & 
                                  , u_MAP, y_MAP, u_FEAM, y_FEAM & 
                                  , MeshMapData , ErrStat, ErrMsg )
! This routine performs the Input-Output solve for ED and HD.
! Note that this has been customized for the physics in the problems and is not a general solution.
!..................................................................................................................................

   USE ElastoDyn
   USE HydroDyn

      ! Passed variables

   REAL(DbKi)                        , INTENT(IN   ) :: this_time                 ! The current simulation time (actual or time of prediction)
   TYPE(FAST_ParameterType)          , INTENT(IN   ) :: p_FAST                    ! Glue-code simulation parameters
   LOGICAL                           , INTENT(IN   ) :: calcJacobian              ! Should we calculate Jacobians this time? (should be TRUE on initialization, then can be false [significantly reducing computational time])
   
      !ElastoDyn:                    
   TYPE(ED_ContinuousStateType)      , INTENT(IN   ) :: x_ED                      ! Continuous states
   TYPE(ED_DiscreteStateType)        , INTENT(IN   ) :: xd_ED                     ! Discrete states
   TYPE(ED_ConstraintStateType)      , INTENT(IN   ) :: z_ED                      ! Constraint states
   TYPE(ED_OtherStateType)           , INTENT(INOUT) :: OtherSt_ED                ! Other/optimization states
   TYPE(ED_ParameterType)            , INTENT(IN   ) :: p_ED                      ! Parameters
   TYPE(ED_InputType)                , INTENT(INOUT) :: u_ED                      ! System inputs
   TYPE(ED_OutputType)               , INTENT(INOUT) :: y_ED                      ! System outputs
   
      !HydroDyn: 
   TYPE(HydroDyn_ContinuousStateType), INTENT(IN   ) :: x_HD                      ! Continuous states
   TYPE(HydroDyn_DiscreteStateType)  , INTENT(IN   ) :: xd_HD                     ! Discrete states
   TYPE(HydroDyn_ConstraintStateType), INTENT(IN   ) :: z_HD                      ! Constraint states
   TYPE(HydroDyn_OtherStateType)     , INTENT(INOUT) :: OtherSt_HD                ! Other/optimization states
   TYPE(HydroDyn_ParameterType)      , INTENT(IN   ) :: p_HD                      ! Parameters
   TYPE(HydroDyn_InputType)          , INTENT(INOUT) :: u_HD                      ! System inputs
   TYPE(HydroDyn_OutputType)         , INTENT(INOUT) :: y_HD                      ! System outputs

      ! MAP/FEAM:
   TYPE(MAP_OutputType),              INTENT(IN   )  :: y_MAP                     ! MAP outputs
   TYPE(MAP_InputType),               INTENT(INOUT)  :: u_MAP                     ! MAP inputs (INOUT just because I don't want to use another tempoarary mesh and we'll overwrite this later)
   TYPE(FEAM_OutputType),             INTENT(IN   )  :: y_FEAM                    ! FEAM outputs
   TYPE(FEAM_InputType),              INTENT(INOUT)  :: u_FEAM                    ! FEAM inputs (INOUT just because I don't want to use another tempoarary mesh and we'll overwrite this later)
      
   TYPE(FAST_ModuleMapType)          , INTENT(INOUT) :: MeshMapData
   INTEGER(IntKi)                    , INTENT(  OUT) :: ErrStat                   ! Error status of the operation
   CHARACTER(*)                      , INTENT(  OUT) :: ErrMsg                    ! Error message if ErrStat /= ErrID_None

   ! Local variables:
   INTEGER,                                PARAMETER :: NumInputs = SizeJac_ED_HD !12
   REAL(ReKi),                             PARAMETER :: TOL_Squared = (1.0E-4)**2 !not currently used because KMax = 1
   REAL(ReKi)                                        :: ThisPerturb               ! an arbitrary perturbation (these are linear, so it shouldn't matter)
   
   REAL(ReKi)                                        :: u(           NumInputs)   ! 6 loads, 6 accelerations
   REAL(ReKi)                                        :: u_perturb(   NumInputs)   ! 6 loads, 6 accelerations
   REAL(ReKi)                                        :: u_delta(     NumInputs)   !
   REAL(ReKi)                                        :: Fn_U_perturb(NumInputs)   ! value of U with perturbations
   REAL(ReKi)                                        :: Fn_U_Resid(  NumInputs)   ! Residual of U
   
                                                                                  
   TYPE(ED_OutputType)                               :: y_ED_input                ! Copy of system outputs sent to this routine (routine input value)
   TYPE(ED_InputType)                                :: u_ED_perturb              ! Perturbed system inputs
   TYPE(ED_OutputType)                               :: y_ED_perturb              ! Perturbed system outputs
   TYPE(HydroDyn_InputType)                          :: u_HD_perturb              ! Perturbed system inputs
   TYPE(HydroDyn_OutputType)                         :: y_HD_perturb              ! Perturbed system outputs
                                                                                  
                                                                                  
   INTEGER(IntKi)                                    :: i                         ! loop counter (jacobian column number)
   INTEGER(IntKi)                                    :: K                         ! Input-output-solve iteration counter
   INTEGER(IntKi)                                    :: ErrStat2                  ! temporary Error status of the operation
   CHARACTER(LEN(ErrMsg))                            :: ErrMsg2                   ! temporary Error message if ErrStat /= ErrID_None
   
#ifdef OUTPUT_ADDEDMASS   
   REAL(ReKi)                                        :: AddedMassMatrix(6,6)
   INTEGER                                           :: UnAM
#endif
#ifdef OUTPUT_JACOBIAN
   INTEGER                                           :: UnJac
#endif

   ! Note: p_FAST%UJacSclFact is a scaling factor that gets us similar magnitudes between loads and accelerations...
 
!bjj: note, that this routine may have a problem if there is remapping done
    
   ErrStat = ErrID_None
   ErrMsg  = ""

   ! note this routine should be called only
   ! IF ( p_FAST%CompHydro == Module_HD .AND. p_FAST%CompSub /= Module_SD ) 
      
   
      !----------------------------------------------------------------------------------------------------
      ! Copy u_ED%PlatformPtMesh so we have the underlying mesh (nodes/elements)
      !----------------------------------------------------------------------------------------------------
      !CALL MeshCopy ( u_ED%PlatformPtMesh, MeshMapData%u_ED_PlatformPtMesh, MESH_NEWCOPY, ErrStat2, ErrMsg2 )      
      !   CALL CheckError( ErrStat2, 'u_PlatformPtMesh:'//ErrMsg2  )
      !   IF ( ErrStat >= AbortErrLev ) RETURN
               
   
      !----------------------------------------------------------------------------------------------------
      ! Some more record keeping stuff:
      !---------------------------------------------------------------------------------------------------- 
         
         ! We need to know the outputs that were sent to this routine:
      CALL ED_CopyOutput( y_ED, y_ED_input, MESH_NEWCOPY, ErrStat2, ErrMsg2)
         CALL CheckError( ErrStat2, ErrMsg2  )
         IF ( ErrStat >= AbortErrLev ) RETURN
         
         ! Local copies for perturbing inputs and outputs (computing Jacobian):
      IF ( calcJacobian ) THEN         
         CALL ED_CopyInput(  u_ED, u_ED_perturb, MESH_NEWCOPY, ErrStat2, ErrMsg2 )
            CALL CheckError( ErrStat2, ErrMsg2  )
            IF ( ErrStat >= AbortErrLev ) RETURN                 
         CALL ED_CopyOutput( y_ED, y_ED_perturb, MESH_NEWCOPY, ErrStat2, ErrMsg2 )         
            CALL CheckError( ErrStat2, ErrMsg2  )
            IF ( ErrStat >= AbortErrLev ) RETURN
         CALL HydroDyn_CopyInput(  u_HD, u_HD_perturb, MESH_NEWCOPY, ErrStat2, ErrMsg2 )           
            CALL CheckError( ErrStat2, ErrMsg2  )
            IF ( ErrStat >= AbortErrLev ) RETURN
         CALL HydroDyn_CopyOutput( y_HD, y_HD_perturb, MESH_NEWCOPY, ErrStat2, ErrMsg2 )  
            CALL CheckError( ErrStat2, ErrMsg2  )
            IF ( ErrStat >= AbortErrLev ) RETURN
      END IF
         
      !----------------------------------------------------------------------------------------------------
      ! set up u vector, using local initial guesses:
      !----------------------------------------------------------------------------------------------------                      
      
         ! make hydrodyn inputs consistant with elastodyn outputs 
         ! (do this because we're using outputs in the u vector):
      CALL Transfer_ED_to_HD(y_ED_input,  u_HD, MeshMapData, ErrStat2, ErrMsg2 ) ! get u_HD from y_ED_input
         CALL CheckError( ErrStat2, ErrMsg2  )
         IF ( ErrStat >= AbortErrLev ) RETURN
            
      
      u( 1: 3) = u_ED%PlatformPtMesh%Force(:,1) / p_FAST%UJacSclFact
      u( 4: 6) = u_ED%PlatformPtMesh%Moment(:,1) / p_FAST%UJacSclFact  
      u( 7: 9) = y_ED_input%PlatformPtMesh%TranslationAcc(:,1)
      u(10:12) = y_ED_input%PlatformPtMesh%RotationAcc(:,1)
            
      K = 0
      
      DO
         
         !-------------------------------------------------------------------------------------------------
         ! Calculate outputs at this_time, based on inputs at this_time
         !-------------------------------------------------------------------------------------------------
         
         CALL ED_CalcOutput( this_time, u_ED, p_ED, x_ED, xd_ED, z_ED, OtherSt_ED, y_ED, ErrStat2, ErrMsg2 )
            CALL CheckError( ErrStat2, ErrMsg2  )
            IF ( ErrStat >= AbortErrLev ) RETURN
                                 
         CALL HydroDyn_CalcOutput( this_time, u_HD, p_HD, x_HD, xd_HD, z_HD, OtherSt_HD, y_HD, ErrStat2, ErrMsg2 )
            CALL CheckError( ErrStat2, ErrMsg2  )
            IF ( ErrStat >= AbortErrLev ) RETURN      
            
         IF ( K >= p_FAST%KMax ) EXIT
         
                                                            
         !-------------------------------------------------------------------------------------------------
         ! Calculate Jacobian: partial U/partial u:
         ! (note that we don't want to change u_ED or u_HD here)
         !-------------------------------------------------------------------------------------------------
         
         CALL U_ED_HD_Residual(y_ED, y_HD, u, Fn_U_Resid) 
            IF ( ErrStat >= AbortErrLev ) RETURN ! U_ED_HD_Residual checks for error
                        
         
         
         IF ( calcJacobian ) THEN
            
            !...............................
            ! Get ElastoDyn's contribution:
            !...............................
            DO i=1,6 !call ED_CalcOutput
                  
               CALL ED_CopyInput(  u_ED, u_ED_perturb, MESH_UPDATECOPY, ErrStat2, ErrMsg2 )
                  CALL CheckError( ErrStat2, ErrMsg2  )
                  IF ( ErrStat >= AbortErrLev ) RETURN            
               u_perturb = u            
               CALL Perturb_u( i, u_perturb, u_ED_perturb=u_ED_perturb, perturb=ThisPerturb ) ! perturb u and u_ED by ThisPerturb [routine sets ThisPerturb]
                  
               ! calculate outputs with perturbed inputs:
               CALL ED_CalcOutput( this_time, u_ED_perturb, p_ED, x_ED, xd_ED, z_ED, OtherSt_ED, y_ED_perturb, ErrStat2, ErrMsg2 ) !calculate y_ED_perturb
                  CALL CheckError( ErrStat2, ErrMsg2  )
                  IF ( ErrStat >= AbortErrLev ) RETURN            
                  
                  
               CALL U_ED_HD_Residual(y_ED_perturb, y_HD, u_perturb, Fn_U_perturb) ! get this perturbation, U_perturb
                  IF ( ErrStat >= AbortErrLev ) RETURN ! U_ED_HD_Residual checks for error
                  
               MeshMapData%Jacobian_ED_SD_HD(:,i) = (Fn_U_perturb - Fn_U_Resid) / ThisPerturb
                  
            END DO ! ElastoDyn contribution ( columns 1-6 )
               
            !...............................
            ! Get HydroDyn's contribution:
            !...............................               
            DO i=7,12 !call HD_CalcOutput
                  
               ! we want to perturb u_HD, but we're going to perturb the input y_ED and transfer that to HD to get u_HD
               CALL ED_CopyOutput( y_ED_input, y_ED_perturb, MESH_UPDATECOPY, ErrStat2, ErrMsg2 )         
                  CALL CheckError( ErrStat2, ErrMsg2  )
                  IF ( ErrStat >= AbortErrLev ) RETURN                                   
               u_perturb = u            
               CALL Perturb_u( i, u_perturb, y_ED_perturb=y_ED_perturb, perturb=ThisPerturb ) ! perturb u and y_ED by ThisPerturb [routine sets ThisPerturb]
               CALL Transfer_ED_to_HD( y_ED_perturb, u_HD_perturb, MeshMapData, ErrStat2, ErrMsg2 ) ! get u_HD_perturb
                  CALL CheckError( ErrStat2, ErrMsg2  )
                  IF ( ErrStat >= AbortErrLev ) RETURN                                   
                  
               ! calculate outputs with perturbed inputs:
               CALL HydroDyn_CalcOutput( this_time, u_HD_perturb, p_HD, x_HD, xd_HD, z_HD, OtherSt_HD, y_HD_perturb, ErrStat2, ErrMsg2 )
                  CALL CheckError( ErrStat2, ErrMsg2  )
                  IF ( ErrStat >= AbortErrLev ) RETURN
                  
                  
               CALL U_ED_HD_Residual(y_ED, y_HD_perturb, u_perturb, Fn_U_perturb) ! get this perturbation                        
                  IF ( ErrStat >= AbortErrLev ) RETURN ! U_ED_HD_Residual checks for error
                  
               MeshMapData%Jacobian_ED_SD_HD(:,i) = (Fn_U_perturb - Fn_U_Resid) / ThisPerturb
                                                                  
            END DO ! HydroDyn contribution ( columns 7-12 )
               
#ifdef OUTPUT_ADDEDMASS  
   UnAM = -1
   CALL GetNewUnit( UnAM, ErrStat, ErrMsg )
   CALL OpenFOutFile( UnAM, TRIM(p_FAST%OutFileRoot)//'.AddedMassMatrix', ErrStat2, ErrMsg2)
      CALL CheckError( ErrStat2, ErrMsg2  )
      IF ( ErrStat >= AbortErrLev ) RETURN               

   AddedMassMatrix = MeshMapData%Jacobian_ED_SD_HD(1:6,7:12) * p_FAST%UJacSclFact   
   CALL WrMatrix(AddedMassMatrix,UnAM, p_FAST%OutFmt)
   CLOSE( UnAM )
#endif   
#ifdef OUTPUT_JACOBIAN
   UnJac = -1
   CALL GetNewUnit( UnJac, ErrStat2, ErrMsg2 )
   CALL OpenFOutFile( UnJac, TRIM(p_FAST%OutFileRoot)//'.'//TRIM(num2lstr(this_time))//'.Jacobian2', ErrStat2, ErrMsg2)
      CALL CheckError( ErrStat2, ErrMsg2  )
      IF ( ErrStat >= AbortErrLev ) RETURN               
      
   CALL WrFileNR(UnJac, '  ')
      CALL WrFileNR(UnJac, ' ElastoDyn_Force_X') 
      CALL WrFileNR(UnJac, ' ElastoDyn_Force_Y') 
      CALL WrFileNR(UnJac, ' ElastoDyn_Force_Z') 
      CALL WrFileNR(UnJac, ' ElastoDyn_Moment_X') 
      CALL WrFileNR(UnJac, ' ElastoDyn_Moment_Y') 
      CALL WrFileNR(UnJac, ' ElastoDyn_Moment_Z') 
   
      CALL WrFileNR(UnJac, ' y_ED_TranslationAcc_X') 
      CALL WrFileNR(UnJac, ' y_ED_TranslationAcc_Y') 
      CALL WrFileNR(UnJac, ' y_ED_TranslationAcc_Z') 
      CALL WrFileNR(UnJac, ' y_ED_RotationAcc_X') 
      CALL WrFileNR(UnJac, ' y_ED_RotationAcc_Y') 
      CALL WrFileNR(UnJac, ' y_ED_RotationAcc_Z') 
   WRITE(UnJac,'()')    
      
   CALL WrMatrix(MeshMapData%Jacobian_ED_SD_HD,UnJac, p_FAST%OutFmt)
   CLOSE( UnJac )      
#endif   
            
            
               ! Get the LU decomposition of this matrix using a LAPACK routine: 
               ! The result is of the form MeshMapDat%Jacobian_ED_SD_HD = P * L * U 

            CALL LAPACK_getrf( M=NumInputs, N=NumInputs, A=MeshMapData%Jacobian_ED_SD_HD, IPIV=MeshMapData%Jacobian_pivot, ErrStat=ErrStat2, ErrMsg=ErrMsg2 )
               CALL CheckError( ErrStat2, ErrMsg2  )
               IF ( ErrStat >= AbortErrLev ) RETURN
            
         END IF         
            
         !-------------------------------------------------------------------------------------------------
         ! Solve for delta u: Jac*u_delta = - Fn_U_Resid
         !  using the LAPACK routine 
         !-------------------------------------------------------------------------------------------------
         
         u_delta = -Fn_U_Resid
         CALL LAPACK_getrs( TRANS='N', N=NumInputs, A=MeshMapData%Jacobian_ED_SD_HD, IPIV=MeshMapData%Jacobian_pivot, B=u_delta, &
                            ErrStat=ErrStat2, ErrMsg=ErrMsg2 )
               CALL CheckError( ErrStat2, ErrMsg2  )
               IF ( ErrStat >= AbortErrLev ) RETURN 
                           
         !-------------------------------------------------------------------------------------------------
         ! check for error, update inputs (u_ED and u_HD), and iterate again
         !-------------------------------------------------------------------------------------------------
                  
!         IF ( DOT_PRODUCT(u_delta, u_delta) <= TOL_Squared ) EXIT
         
         u = u + u_delta
                  
         u_ED%PlatformPtMesh%Force( :,1)               = u_ED%PlatformPtMesh%Force( :,1)               + u_delta( 1: 3) * p_FAST%UJacSclFact 
         u_ED%PlatformPtMesh%Moment(:,1)               = u_ED%PlatformPtMesh%Moment(:,1)               + u_delta( 4: 6) * p_FAST%UJacSclFact
         y_ED_input%PlatformPtMesh%TranslationAcc(:,1) = y_ED_input%PlatformPtMesh%TranslationAcc(:,1) + u_delta( 7: 9)
         y_ED_input%PlatformPtMesh%RotationAcc(   :,1) = y_ED_input%PlatformPtMesh%RotationAcc(   :,1) + u_delta(10:12)
                  
         CALL Transfer_ED_to_HD( y_ED_input, u_HD, MeshMapData, ErrStat2, ErrMsg2 ) ! get u_HD with u_delta changes
            CALL CheckError( ErrStat2, ErrMsg2  )
            IF ( ErrStat >= AbortErrLev ) RETURN
         
         
         K = K + 1
         
      END DO ! K
            
   
      CALL CleanUp()
      
CONTAINS                                 
   !...............................................................................................................................
   SUBROUTINE Perturb_u( n, u_perturb, u_ED_perturb, y_ED_perturb, perturb )
   ! This routine perturbs the nth element of the u array (and ED input/output it corresponds to)
   !...............................................................................................................................
!   REAL( ReKi ),                       INTENT(IN)    :: this_U(NumInputs)
   INTEGER( IntKi )                  , INTENT(IN   ) :: n
   REAL( ReKi )                      , INTENT(INOUT) :: u_perturb(numInputs)
   TYPE(ED_InputType) , OPTIONAL     , INTENT(INOUT) :: u_ED_perturb           ! System inputs   (needed only when 1 <= n <=  6)
   TYPE(ED_OutputType), OPTIONAL     , INTENT(INOUT) :: y_ED_perturb           ! System outputs  (needed only when 7 <= n <= 12)
   REAL( ReKi )                      , INTENT(  OUT) :: perturb
   
   if ( n <= 6 ) then ! ED u
   
      if ( n <= 3 ) then         
         perturb = GetPerturb( u_ED_perturb%PlatformPtMesh%Force(n   ,1) )         
         u_ED_perturb%PlatformPtMesh%Force(n   ,1) = u_ED_perturb%PlatformPtMesh%Force(n   ,1) + perturb * p_FAST%UJacSclFact 
      else
         perturb = GetPerturb( u_ED_perturb%PlatformPtMesh%Moment(n-3,1) )         
         u_ED_perturb%PlatformPtMesh%Moment(n-3,1) = u_ED_perturb%PlatformPtMesh%Moment(n-3,1) + perturb * p_FAST%UJacSclFact 
      end if
                  
   else ! ED y = HD u
      
      if ( n <= 9 ) then         
         perturb = GetPerturb( y_ED_perturb%PlatformPtMesh%TranslationAcc(n-6,1) )         
         y_ED_perturb%PlatformPtMesh%TranslationAcc(n-6,1) = y_ED_perturb%PlatformPtMesh%TranslationAcc(n-6,1) + perturb
      else
         perturb = GetPerturb( y_ED_perturb%PlatformPtMesh%RotationAcc(n-9,1) )         
         y_ED_perturb%PlatformPtMesh%RotationAcc(   n-9,1) = y_ED_perturb%PlatformPtMesh%RotationAcc(   n-9,1) + perturb
      end if
                  
   end if
           
   u_perturb(n) = u_perturb(n) + perturb
   
        
   END SUBROUTINE Perturb_u
   !...............................................................................................................................
   SUBROUTINE U_ED_HD_Residual( y_ED2, y_HD2, u_IN, U_Resid)
   !...............................................................................................................................
                                  
   TYPE(ED_OutputType)               , INTENT(IN   ) :: y_ED2                  ! System outputs
   TYPE(HydroDyn_OutputType)         , INTENT(IN   ) :: y_HD2                  ! System outputs
   REAL(ReKi)                        , INTENT(IN   ) :: u_in(NumInputs)
   REAL(ReKi)                        , INTENT(  OUT) :: U_Resid(NumInputs)


   
   
      !   ! Transfer motions:
      
   !..................
   ! Set mooring line inputs (which don't have acceleration fields)
   !..................
   
      IF ( p_FAST%CompMooring == Module_MAP ) THEN
         
         ! note: MAP_InputSolve must be called before setting ED loads inputs (so that motions are known for loads [moment] mapping)      
         CALL MAP_InputSolve( u_map, y_ED2, MeshMapData, ErrStat2, ErrMsg2 )
            CALL CheckError(ErrStat2,ErrMsg2)
            IF (ErrStat >= AbortErrLev) RETURN       
                                 
         CALL Transfer_Point_to_Point( y_MAP%PtFairleadLoad, MeshMapData%u_ED_PlatformPtMesh_2, MeshMapData%MAP_P_2_ED_P, ErrStat2, ErrMsg2, u_MAP%PtFairleadDisplacement, y_ED2%PlatformPtMesh ) !u_MAP and y_ED contain the displacements needed for moment calculations
            CALL CheckError( ErrStat2, ErrMsg2 )
            IF (ErrStat >= AbortErrLev) RETURN               
                        
      ELSEIF ( p_FAST%CompMooring == Module_FEAM ) THEN
         
         ! note: FEAM_InputSolve must be called before setting ED loads inputs (so that motions are known for loads [moment] mapping)      
         CALL FEAM_InputSolve( u_FEAM, y_ED2, MeshMapData, ErrStat2, ErrMsg2 )
            CALL CheckError(ErrStat2,ErrMsg2)
            IF (ErrStat >= AbortErrLev) RETURN       
                 
         CALL Transfer_Point_to_Point( y_FEAM%PtFairleadLoad, MeshMapData%u_ED_PlatformPtMesh_2, MeshMapData%FEAM_P_2_ED_P, ErrStat2, ErrMsg2, u_FEAM%PtFairleadDisplacement, y_ED2%PlatformPtMesh ) !u_FEAM and y_ED contain the displacements needed for moment calculations
            CALL CheckError( ErrStat2, ErrMsg2 )
            IF (ErrStat >= AbortErrLev) RETURN              
            
      ELSE
         
         MeshMapData%u_ED_PlatformPtMesh_2%Force  = 0.0_ReKi
         MeshMapData%u_ED_PlatformPtMesh_2%Moment = 0.0_ReKi
         
      END IF        

   ! we use copies of the input meshes (we don't need to update values in the original data structures):            
      
!bjj: why don't we update u_HD2 here? shouldn't we update before using it to transfer the loads?
   
      CALL Transfer_Point_to_Point( y_ED2%PlatformPtMesh, MeshMapData%u_HD_Mesh, MeshMapData%ED_P_2_HD_W_P, ErrStat2, ErrMsg2) 
         CALL CheckError( ErrStat2, ErrMsg2 )
         IF (ErrStat >= AbortErrLev) RETURN         
  
         
         ! we're mapping loads, so we also need the sibling meshes' displacements:
      CALL Transfer_Point_to_Point( y_HD2%AllHdroOrigin, MeshMapData%u_ED_PlatformPtMesh, MeshMapData%HD_W_P_2_ED_P, ErrStat2, ErrMsg2, MeshMapData%u_HD_Mesh, y_ED2%PlatformPtMesh) !u_HD and u_mapped_positions contain the displaced positions for load calculations
         CALL CheckError( ErrStat2, ErrMsg2 )
         IF (ErrStat >= AbortErrLev) RETURN

      MeshMapData%u_ED_PlatformPtMesh%Force  = MeshMapData%u_ED_PlatformPtMesh%Force  + MeshMapData%u_ED_PlatformPtMesh_2%Force
      MeshMapData%u_ED_PlatformPtMesh%Moment = MeshMapData%u_ED_PlatformPtMesh%Moment + MeshMapData%u_ED_PlatformPtMesh_2%Moment
                                       
            
      U_Resid( 1: 3) = u_in( 1: 3) - MeshMapData%u_ED_PlatformPtMesh%Force(:,1) / p_FAST%UJacSclFact
      U_Resid( 4: 6) = u_in( 4: 6) - MeshMapData%u_ED_PlatformPtMesh%Moment(:,1) / p_FAST%UJacSclFact      
      U_Resid( 7: 9) = u_in( 7: 9) - y_ED2%PlatformPtMesh%TranslationAcc(:,1)
      U_Resid(10:12) = u_in(10:12) - y_ED2%PlatformPtMesh%RotationAcc(:,1)
   
            
   END SUBROUTINE U_ED_HD_Residual   
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

         CALL WrScr( ' ED_HD_InputOutputSolve: '//TRIM(Msg) )
         ErrStat = MAX(ErrStat, ErrID)

         !.........................................................................................................................
         ! Clean up if we're going to return on error: close files, deallocate local arrays
         !.........................................................................................................................
         IF ( ErrStat >= AbortErrLev ) CALL CleanUp()

      END IF


   END SUBROUTINE CheckError   
   !...............................................................................................................................
   SUBROUTINE CleanUp()
      INTEGER(IntKi)             :: ErrStat3    ! The error identifier (ErrStat)
      CHARACTER(1024)            :: ErrMsg3     ! The error message (ErrMsg)
                  
      CALL ED_DestroyOutput(y_ED_input, ErrStat3, ErrMsg3 )
         IF (ErrStat3 /= ErrID_None) CALL WrScr(' ED_HD_InputOutputSolve/ED_DestroyOutput: '//TRIM(ErrMsg3) )
         
      IF ( calcJacobian ) THEN
         CALL ED_DestroyInput( u_ED_perturb, ErrStat3, ErrMsg3 )
            IF (ErrStat3 /= ErrID_None) CALL WrScr(' ED_HD_InputOutputSolve/ED_DestroyInput: '//TRIM(ErrMsg3) )
         CALL ED_DestroyOutput(y_ED_perturb, ErrStat3, ErrMsg3 )
            IF (ErrStat3 /= ErrID_None) CALL WrScr(' ED_HD_InputOutputSolve/ED_DestroyOutput: '//TRIM(ErrMsg3) )
         
         CALL HydroDyn_DestroyInput( u_HD_perturb, ErrStat3, ErrMsg3 )
            IF (ErrStat3 /= ErrID_None) CALL WrScr(' ED_HD_InputOutputSolve/HydroDyn_DestroyInput: '//TRIM(ErrMsg3) )
         CALL HydroDyn_DestroyOutput(y_HD_perturb, ErrStat3, ErrMsg3 )
            IF (ErrStat3 /= ErrID_None) CALL WrScr(' ED_HD_InputOutputSolve/HydroDyn_DestroyOutput: '//TRIM(ErrMsg3) )                        
      END IF
      
   
      
   END SUBROUTINE CleanUp
   !...............................................................................................................................
END SUBROUTINE ED_HD_InputOutputSolve
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ED_SD_HD_InputOutputSolve(  this_time, p_FAST, calcJacobian &
                                     , u_ED, p_ED, x_ED, xd_ED, z_ED, OtherSt_ED, y_ED &
                                     , u_SD, p_SD, x_SD, xd_SD, z_SD, OtherSt_SD, y_SD & 
                                     , u_HD, p_HD, x_HD, xd_HD, z_HD, OtherSt_HD, y_HD & 
                                     , u_MAP,  y_MAP  &
                                     , u_FEAM, y_FEAM & 
                                     , u_IceF, y_IceF & 
                                     , u_IceD, y_IceD & 
                                     , MeshMapData , ErrStat, ErrMsg )
! This routine performs the Input-Output solve for ED, SD, and HD.
! Note that this has been customized for the physics in the problems and is not a general solution.
!..................................................................................................................................

   USE ElastoDyn
   USE SubDyn
   USE HydroDyn

      ! Passed variables

   REAL(DbKi)                        , INTENT(IN   ) :: this_time                 ! The current simulation time (actual or time of prediction)
   TYPE(FAST_ParameterType)          , INTENT(IN   ) :: p_FAST                    ! Glue-code simulation parameters
   LOGICAL                           , INTENT(IN   ) :: calcJacobian              ! Should we calculate Jacobians this time?
                                                                                  
      !ElastoDyn:                                                                 
   TYPE(ED_ContinuousStateType)      , INTENT(IN   ) :: x_ED                      ! Continuous states
   TYPE(ED_DiscreteStateType)        , INTENT(IN   ) :: xd_ED                     ! Discrete states
   TYPE(ED_ConstraintStateType)      , INTENT(IN   ) :: z_ED                      ! Constraint states
   TYPE(ED_OtherStateType)           , INTENT(INOUT) :: OtherSt_ED                ! Other/optimization states
   TYPE(ED_ParameterType)            , INTENT(IN   ) :: p_ED                      ! Parameters
   TYPE(ED_InputType)                , INTENT(INOUT) :: u_ED                      ! System inputs
   TYPE(ED_OutputType)               , INTENT(INOUT) :: y_ED                      ! System outputs
                                                                                  
      !SubDyn:                                                                    
   TYPE(SD_ContinuousStateType)      , INTENT(IN   ) :: x_SD                      ! Continuous states
   TYPE(SD_DiscreteStateType)        , INTENT(IN   ) :: xd_SD                     ! Discrete states
   TYPE(SD_ConstraintStateType)      , INTENT(IN   ) :: z_SD                      ! Constraint states
   TYPE(SD_OtherStateType)           , INTENT(INOUT) :: OtherSt_SD                ! Other/optimization states
   TYPE(SD_ParameterType)            , INTENT(IN   ) :: p_SD                      ! Parameters
   TYPE(SD_InputType)                , INTENT(INOUT) :: u_SD                      ! System inputs
   TYPE(SD_OutputType)               , INTENT(INOUT) :: y_SD                      ! System outputs
          
      !HydroDyn: 
   TYPE(HydroDyn_ContinuousStateType), INTENT(IN   ) :: x_HD                      ! Continuous states
   TYPE(HydroDyn_DiscreteStateType)  , INTENT(IN   ) :: xd_HD                     ! Discrete states
   TYPE(HydroDyn_ConstraintStateType), INTENT(IN   ) :: z_HD                      ! Constraint states
   TYPE(HydroDyn_OtherStateType)     , INTENT(INOUT) :: OtherSt_HD                ! Other/optimization states
   TYPE(HydroDyn_ParameterType)      , INTENT(IN   ) :: p_HD                      ! Parameters
   TYPE(HydroDyn_InputType)          , INTENT(INOUT) :: u_HD                      ! System inputs
   TYPE(HydroDyn_OutputType)         , INTENT(INOUT) :: y_HD                      ! System outputs
   
      ! MAP/FEAM/IceFloe/IceDyn:
   TYPE(MAP_OutputType),              INTENT(IN   )  :: y_MAP                     ! MAP outputs 
   TYPE(MAP_InputType),               INTENT(INOUT)  :: u_MAP                     ! MAP inputs (INOUT just because I don't want to use another tempoarary mesh and we'll overwrite this later)
   TYPE(FEAM_OutputType),             INTENT(IN   )  :: y_FEAM                    ! FEAM outputs  
   TYPE(FEAM_InputType),              INTENT(INOUT)  :: u_FEAM                    ! FEAM inputs (INOUT just because I don't want to use another tempoarary mesh and we'll overwrite this later)
   TYPE(IceFloe_OutputType),          INTENT(IN   )  :: y_IceF                    ! IceFloe outputs  
   TYPE(IceFloe_InputType),           INTENT(INOUT)  :: u_IceF                    ! IceFloe inputs (INOUT just because I don't want to use another tempoarary mesh and we'll overwrite this later)
   TYPE(IceD_OutputType),             INTENT(IN   )  :: y_IceD(:)                 ! IceDyn outputs  
   TYPE(IceD_InputType),              INTENT(INOUT)  :: u_IceD(:)                 ! IceDyn inputs (INOUT just because I don't want to use another tempoarary mesh and we'll overwrite this later)
      
   TYPE(FAST_ModuleMapType)          , INTENT(INOUT) :: MeshMapData
   INTEGER(IntKi)                    , INTENT(  OUT) :: ErrStat                   ! Error status of the operation
   CHARACTER(*)                      , INTENT(  OUT) :: ErrMsg                    ! Error message if ErrStat /= ErrID_None

   ! Local variables:
   REAL(ReKi),                             PARAMETER :: TOL_Squared = (1.0E-4)**2 !not currently used because KMax = 1
   REAL(ReKi)                                        :: ThisPerturb               ! an arbitrary perturbation (these are linear, so it shouldn't matter)
   
!bjj: store these so that we don't reallocate every time?   
   REAL(ReKi)                                        :: u(           p_FAST%SizeJac_ED_SD_HD(4))   ! size of loads/accelerations passed between the 3 modules
   REAL(ReKi)                                        :: u_perturb(   p_FAST%SizeJac_ED_SD_HD(4))   ! size of loads/accelerations passed between the 3 modules
   REAL(ReKi)                                        :: u_delta(     p_FAST%SizeJac_ED_SD_HD(4))   ! size of loads/accelerations passed between the 3 modules
   REAL(ReKi)                                        :: Fn_U_perturb(p_FAST%SizeJac_ED_SD_HD(4))   ! value of U with perturbations
   REAL(ReKi)                                        :: Fn_U_Resid(  p_FAST%SizeJac_ED_SD_HD(4))   ! Residual of U
                                                                                           
   TYPE(ED_InputType)                                :: u_ED_perturb              ! Perturbed system inputs
   TYPE(ED_OutputType)                               :: y_ED_perturb              ! Perturbed system outputs
   TYPE(SD_InputType)                                :: u_SD_perturb              ! Perturbed system inputs
   TYPE(SD_OutputType)                               :: y_SD_perturb              ! Perturbed system outputs
   TYPE(HydroDyn_InputType)                          :: u_HD_perturb              ! Perturbed system inputs
   TYPE(HydroDyn_OutputType)                         :: y_HD_perturb              ! Perturbed system outputs
                                                                                  
                                                                                  
   INTEGER(IntKi)                                    :: i,j                       ! loop counters (jacobian column number)
   INTEGER(IntKi)                                    :: K                         ! Input-output-solve iteration counter
   INTEGER(IntKi)                                    :: ErrStat2                  ! temporary Error status of the operation
   CHARACTER(LEN(ErrMsg))                            :: ErrMsg2                   ! temporary Error message if ErrStat /= ErrID_None
   
#ifdef OUTPUT_ADDEDMASS   
   REAL(ReKi)                                        :: AddedMassMatrix(6,6)
   INTEGER                                           :: UnAM
   INTEGER                                           :: AMIndx   
#endif
#ifdef OUTPUT_JACOBIAN
   INTEGER                                           :: UnJac
   INTEGER                                           :: TmpIndx   
#endif
   
      
   ! Note: p_FAST%UJacSclFact is a scaling factor that gets us similar magnitudes between loads and accelerations...
 
!bjj: note, that this routine may have a problem if there is remapping done
    
   ErrStat = ErrID_None
   ErrMsg  = ""

   ! note this routine should be called only
   ! IF ( p_FAST%CompHydro == Module_HD .AND. p_FAST%CompSub == Module_SD ) 
   
      !----------------------------------------------------------------------------------------------------
      ! Some record keeping stuff:
      !----------------------------------------------------------------------------------------------------      

      ! if remap flag or

         ! Temporary meshes for transfering inputs to ED, HD, and SD
         
      !CALL MeshCopy ( u_SD%TPMesh, MeshMapData%u_SD_TPMesh, MESH_UPDATEREFERENCE, ErrStat2, ErrMsg2 )      
      !   CALL CheckError( ErrStat2, 'u_SD_TPMesh:'//ErrMsg2  )
      !   IF ( ErrStat >= AbortErrLev ) RETURN
         
      !CALL MeshCopy ( u_SD%LMesh, MeshMapData%u_SD_LMesh, MESH_UPDATEREFERENCE, ErrStat2, ErrMsg2 )      
      !   CALL CheckError( ErrStat2, 'u_SD_LMesh:'//ErrMsg2  )
      !   IF ( ErrStat >= AbortErrLev ) RETURN
         
      !CALL MeshCopy ( u_HD%Morison%LumpedMesh, MeshMapData%u_HD_M_LumpedMesh, MESH_UPDATEREFERENCE, ErrStat2, ErrMsg2 )      
      !   CALL CheckError( ErrStat2, 'u_HD_M_LumpedMesh:'//ErrMsg2  )
      !   IF ( ErrStat >= AbortErrLev ) RETURN

      !CALL MeshCopy ( u_HD%Morison%DistribMesh, MeshMapData%u_HD_M_DistribMesh, MESH_UPDATEREFERENCE, ErrStat2, ErrMsg2 )      
      !   CALL CheckError( ErrStat2, 'u_HD_M_DistribMesh:'//ErrMsg2  )
      !   IF ( ErrStat >= AbortErrLev ) RETURN
         
      !CALL MeshCopy ( u_HD%Mesh, MeshMapData%u_HD_Mesh, MESH_UPDATEREFERENCE, ErrStat2, ErrMsg2 )      
      !   CALL CheckError( ErrStat2, 'u_HD_Mesh:'//ErrMsg2  )
      !   IF ( ErrStat >= AbortErrLev ) RETURN
         
      !CALL MeshCopy ( u_ED%PlatformPtMesh, MeshMapData%u_ED_PlatformPtMesh_2, MESH_UPDATEREFERENCE, ErrStat2, ErrMsg2 )      
      !   CALL CheckError( ErrStat2, 'MeshMapData%u_ED_PlatformPtMesh_2:'//ErrMsg2  )
      !   IF ( ErrStat >= AbortErrLev ) RETURN         
         
      !CALL MeshCopy ( u_SD%LMesh, MeshMapData%u_SD_LMesh_2, MESH_UPDATEREFERENCEY, ErrStat2, ErrMsg2 )      
      !   CALL CheckError( ErrStat2, 'u_LMesh_mapped:'//ErrMsg2  )
      !   IF ( ErrStat >= AbortErrLev ) RETURN         
         
               
                  
         ! Local copies for perturbing inputs and outputs (computing Jacobian):
      IF ( calcJacobian ) THEN         
         
         CALL ED_CopyInput(  u_ED, u_ED_perturb, MESH_NEWCOPY, ErrStat2, ErrMsg2 )
            CALL CheckError( ErrStat2, 'u_ED_perturb:'//ErrMsg2  )
            IF ( ErrStat >= AbortErrLev ) RETURN                 
         CALL ED_CopyOutput( y_ED, y_ED_perturb, MESH_NEWCOPY, ErrStat2, ErrMsg2 )         
            CALL CheckError( ErrStat2, 'y_ED_perturb:'//ErrMsg2  )
            IF ( ErrStat >= AbortErrLev ) RETURN
            
         IF ( p_FAST%CompSub == Module_SD ) THEN   
            CALL SD_CopyInput(  u_SD, u_SD_perturb, MESH_NEWCOPY, ErrStat2, ErrMsg2 )           
               CALL CheckError( ErrStat2, 'u_SD_perturb:'//ErrMsg2  )
               IF ( ErrStat >= AbortErrLev ) RETURN
            CALL SD_CopyOutput( y_SD, y_SD_perturb, MESH_NEWCOPY, ErrStat2, ErrMsg2 )  
               CALL CheckError( ErrStat2, 'y_SD_perturb:'//ErrMsg2  )
               IF ( ErrStat >= AbortErrLev ) RETURN
         END IF
            
         IF ( p_FAST%CompHydro == Module_HD ) THEN            
            CALL HydroDyn_CopyInput(  u_HD, u_HD_perturb, MESH_NEWCOPY, ErrStat2, ErrMsg2 )           
               CALL CheckError( ErrStat2, 'u_HD_perturb:'//ErrMsg2  )
               IF ( ErrStat >= AbortErrLev ) RETURN
            CALL HydroDyn_CopyOutput( y_HD, y_HD_perturb, MESH_NEWCOPY, ErrStat2, ErrMsg2 )  
               CALL CheckError( ErrStat2, 'y_HD_perturb:'//ErrMsg2  )
               IF ( ErrStat >= AbortErrLev ) RETURN                        
         END IF

      END IF
         
      !----------------------------------------------------------------------------------------------------
      ! set up u vector, using local initial guesses:
      !----------------------------------------------------------------------------------------------------                      
      
      CALL Create_ED_SD_HD_UVector(u, u_ED%PlatformPtMesh, u_SD%TPMesh, u_SD%LMesh, u_HD%Morison%LumpedMesh, u_HD%Morison%DistribMesh, u_HD%Mesh, p_FAST )
                  
      K = 0
      
      DO
         
         !-------------------------------------------------------------------------------------------------
         ! Calculate outputs at this_time, based on inputs at this_time
         !-------------------------------------------------------------------------------------------------
         
         CALL ED_CalcOutput( this_time, u_ED, p_ED, x_ED, xd_ED, z_ED, OtherSt_ED, y_ED, ErrStat2, ErrMsg2 )
            CALL CheckError( ErrStat2, ErrMsg2  )
            IF ( ErrStat >= AbortErrLev ) RETURN
                                 
         IF ( p_FAST%CompSub == Module_SD ) THEN            
            CALL SD_CalcOutput( this_time, u_SD, p_SD, x_SD, xd_SD, z_SD, OtherSt_SD, y_SD, ErrStat2, ErrMsg2 )
               CALL CheckError( ErrStat2, ErrMsg2  )
               IF ( ErrStat >= AbortErrLev ) RETURN      
         END IF
            
         IF ( p_FAST%CompHydro == Module_HD ) THEN 
            CALL HydroDyn_CalcOutput( this_time, u_HD, p_HD, x_HD, xd_HD, z_HD, OtherSt_HD, y_HD, ErrStat2, ErrMsg2 )
               CALL CheckError( ErrStat2, ErrMsg2  )
               IF ( ErrStat >= AbortErrLev ) RETURN      
         END IF
         
         IF ( K >= p_FAST%KMax ) EXIT
         
                                                            
         !-------------------------------------------------------------------------------------------------
         ! Calculate Jacobian: partial U/partial u:
         ! (note that we don't want to change u_ED, u_SD, or u_HD here)
         !-------------------------------------------------------------------------------------------------
         
         CALL U_ED_SD_HD_Residual(y_ED, y_SD, y_HD, u, Fn_U_Resid)                  
            IF ( ErrStat >= AbortErrLev ) RETURN
         
         IF ( calcJacobian ) THEN
            
            !...............................
            ! Get ElastoDyn's contribution:
            !...............................
            DO i=1,p_FAST%SizeJac_ED_SD_HD(1) !call ED_CalcOutput
                  
               ! perturb u_ED:
               CALL ED_CopyInput(  u_ED, u_ED_perturb, MESH_UPDATECOPY, ErrStat2, ErrMsg2 )
                  CALL CheckError( ErrStat2, ErrMsg2  )
                  IF ( ErrStat >= AbortErrLev ) RETURN            
               u_perturb = u            
               CALL Perturb_u_ED_SD_HD( p_FAST, MeshMapData%Jac_u_indx, i, u_perturb, u_ED_perturb=u_ED_perturb, perturb=ThisPerturb ) ! perturb u and u_ED by ThisPerturb [routine sets ThisPerturb]
                  
               ! calculate outputs with perturbed inputs:
               CALL ED_CalcOutput( this_time, u_ED_perturb, p_ED, x_ED, xd_ED, z_ED, OtherSt_ED, y_ED_perturb, ErrStat2, ErrMsg2 ) !calculate y_ED_perturb
                  CALL CheckError( ErrStat2, ErrMsg2  )
                  IF ( ErrStat >= AbortErrLev ) RETURN            
                  
                  
               CALL U_ED_SD_HD_Residual(y_ED_perturb, y_SD, y_HD, u_perturb, Fn_U_perturb) ! get this perturbation, U_perturb
                  IF ( ErrStat >= AbortErrLev ) RETURN    
               
                MeshMapData%Jacobian_ED_SD_HD(:,i) = (Fn_U_perturb - Fn_U_Resid) / ThisPerturb
                  
            END DO ! ElastoDyn contribution ( columns 1-6 )
               
            i = p_FAST%SizeJac_ED_SD_HD(1)
            !...............................
            ! Get SubDyn's contribution:  (note if p_FAST%CompHydro /= Module_SD, SizeJac_ED_SD_HD(2) = 0)
            !...............................               
            DO j=1,p_FAST%SizeJac_ED_SD_HD(2) !call SD_CalcOutput
               i = i + 1 ! i = j + p_FAST%SizeJac_ED_SD_HD(1)
                              
               ! perturb u_SD:
               CALL SD_CopyInput(  u_SD, u_SD_perturb, MESH_UPDATECOPY, ErrStat2, ErrMsg2 )
                  CALL CheckError( ErrStat2, ErrMsg2  )
                  IF ( ErrStat >= AbortErrLev ) RETURN
               u_perturb = u            
               CALL Perturb_u_ED_SD_HD( p_FAST, MeshMapData%Jac_u_indx, i, u_perturb, u_SD_perturb=u_SD_perturb, perturb=ThisPerturb ) ! perturb u and u_SD by ThisPerturb [routine sets ThisPerturb]
                  
               ! calculate outputs with perturbed inputs:
               CALL SD_CalcOutput( this_time, u_SD_perturb, p_SD, x_SD, xd_SD, z_SD, OtherSt_SD, y_SD_perturb, ErrStat2, ErrMsg2 )
                  CALL CheckError( ErrStat2, ErrMsg2  )
                  IF ( ErrStat >= AbortErrLev ) RETURN
                  
                  
               CALL U_ED_SD_HD_Residual(y_ED, y_SD_perturb, y_HD, u_perturb, Fn_U_perturb) ! get this perturbation                        
                  IF ( ErrStat >= AbortErrLev ) RETURN
                  
                MeshMapData%Jacobian_ED_SD_HD(:,i) = (Fn_U_perturb - Fn_U_Resid) / ThisPerturb
                                    
            END DO ! SubDyn contribution
            
                  
            !...............................
            ! Get HydroDyn's contribution: (note if p_FAST%CompHydro /= Module_HD, SizeJac_ED_SD_HD(3) = 0)
            !...............................             
            DO j=1,p_FAST%SizeJac_ED_SD_HD(3) !call HydroDyn_CalcOutput            
               i = i + 1 ! i = j + p_FAST%SizeJac_ED_SD_HD(1) + p_FAST%SizeJac_ED_SD_HD(2) 

               ! perturb u_HD:
               CALL HydroDyn_CopyInput(  u_HD, u_HD_perturb, MESH_UPDATECOPY, ErrStat2, ErrMsg2 )
                  CALL CheckError( ErrStat2, ErrMsg2  )
                  IF ( ErrStat >= AbortErrLev ) RETURN
               u_perturb = u            
               CALL Perturb_u_ED_SD_HD( p_FAST, MeshMapData%Jac_u_indx, i, u_perturb, u_HD_perturb=u_HD_perturb, perturb=ThisPerturb ) ! perturb u and u_HD by ThisPerturb [routine sets ThisPerturb]
                  
               ! calculate outputs with perturbed inputs:
               CALL HydroDyn_CalcOutput( this_time, u_HD_perturb, p_HD, x_HD, xd_HD, z_HD, OtherSt_HD, y_HD_perturb, ErrStat2, ErrMsg2 )
                  CALL CheckError( ErrStat2, ErrMsg2  )
                  IF ( ErrStat >= AbortErrLev ) RETURN               
                  
               CALL U_ED_SD_HD_Residual(y_ED, y_SD, y_HD_perturb, u_perturb, Fn_U_perturb) ! get this perturbation                        
                  IF ( ErrStat >= AbortErrLev ) RETURN               
                  
                MeshMapData%Jacobian_ED_SD_HD(:,i) = (Fn_U_perturb - Fn_U_Resid) / ThisPerturb
                                             
            END DO !HydroDyn contribution
            
            
#ifdef OUTPUT_ADDEDMASS  
IF (p_FAST%CompHydro == Module_HD ) THEN
   UnAM = -1
   CALL GetNewUnit( UnAM, ErrStat2, ErrMsg2 )
   CALL OpenFOutFile( UnAM, TRIM(p_FAST%OutFileRoot)//'.AddedMassMatrix', ErrStat2, ErrMsg2)
      CALL CheckError( ErrStat2, ErrMsg2  )
      IF ( ErrStat >= AbortErrLev ) RETURN               
   
   AMIndx = p_FAST%SizeJac_ED_SD_HD(4) - 5 !the start of the HydroDyn Mesh inputs in the Jacobian
   AddedMassMatrix = MeshMapData%Jacobian_ED_SD_HD(1:6,AMIndx:p_FAST%SizeJac_ED_SD_HD(4)) * p_FAST%UJacSclFact   
   CALL WrMatrix(AddedMassMatrix,UnAM, p_FAST%OutFmt)
   CLOSE( UnAM )
END IF
#endif
#ifdef OUTPUT_JACOBIAN
   UnJac = -1
   CALL GetNewUnit( UnJac, ErrStat2, ErrMsg2 )
   CALL OpenFOutFile( UnJac, TRIM(p_FAST%OutFileRoot)//'.'//TRIM(num2lstr(this_time))//'.Jacobian', ErrStat2, ErrMsg2)
      CALL CheckError( ErrStat2, ErrMsg2  )
      IF ( ErrStat >= AbortErrLev ) RETURN               
      
   CALL WrFileNR(UnJac, '  ')
      CALL WrFileNR(UnJac, ' ElastoDyn_Force_X') 
      CALL WrFileNR(UnJac, ' ElastoDyn_Force_Y') 
      CALL WrFileNR(UnJac, ' ElastoDyn_Force_Z') 
      CALL WrFileNR(UnJac, ' ElastoDyn_Moment_X') 
      CALL WrFileNR(UnJac, ' ElastoDyn_Moment_Y') 
      CALL WrFileNR(UnJac, ' ElastoDyn_Moment_Z') 
   
   DO TmpIndx=1,u_SD%TPMesh%NNodes
      CALL WrFileNR(UnJac, ' SD_TPMesh_TranslationAcc_X_'//TRIM(Num2LStr(TmpIndx))) 
      CALL WrFileNR(UnJac, ' SD_TPMesh_TranslationAcc_Y_'//TRIM(Num2LStr(TmpIndx))) 
      CALL WrFileNR(UnJac, ' SD_TPMesh_TranslationAcc_Z_'//TRIM(Num2LStr(TmpIndx))) 
   END DO

   DO TmpIndx=1,u_SD%TPMesh%NNodes
      CALL WrFileNR(UnJac, ' SD_TPMesh_RotationAcc_X_'//TRIM(Num2LStr(TmpIndx))) 
      CALL WrFileNR(UnJac, ' SD_TPMesh_RotationAcc_Y_'//TRIM(Num2LStr(TmpIndx))) 
      CALL WrFileNR(UnJac, ' SD_TPMesh_RotationAcc_Z_'//TRIM(Num2LStr(TmpIndx))) 
   END DO
            
   IF ( p_FAST%CompHydro == Module_HD ) THEN   ! this SD mesh linked only when HD is enabled
      DO TmpIndx=1,u_SD%LMesh%NNodes
         CALL WrFileNR(UnJac, ' SD_LMesh_Force_X_'//TRIM(Num2LStr(TmpIndx))) 
         CALL WrFileNR(UnJac, ' SD_LMesh_Force_Y_'//TRIM(Num2LStr(TmpIndx))) 
         CALL WrFileNR(UnJac, ' SD_LMesh_Force_Z_'//TRIM(Num2LStr(TmpIndx))) 
      END DO      
      DO TmpIndx=1,u_SD%LMesh%NNodes
         CALL WrFileNR(UnJac, ' SD_LMesh_Moment_X_'//TRIM(Num2LStr(TmpIndx))) 
         CALL WrFileNR(UnJac, ' SD_LMesh_Moment_Y_'//TRIM(Num2LStr(TmpIndx))) 
         CALL WrFileNR(UnJac, ' SD_LMesh_Moment_Z_'//TRIM(Num2LStr(TmpIndx))) 
      END DO                  
   END IF
   
   DO TmpIndx=1,u_HD%Morison%LumpedMesh%NNodes
      CALL WrFileNR(UnJac, ' HD_M_Lumped_TranslationAcc_X_'//TRIM(Num2LStr(TmpIndx))) 
      CALL WrFileNR(UnJac, ' HD_M_Lumped_TranslationAcc_Y_'//TRIM(Num2LStr(TmpIndx))) 
      CALL WrFileNR(UnJac, ' HD_M_Lumped_TranslationAcc_Z_'//TRIM(Num2LStr(TmpIndx))) 
   END DO   
   DO TmpIndx=1,u_HD%Morison%LumpedMesh%NNodes
      CALL WrFileNR(UnJac, ' HD_M_Lumped_RotationAcc_X_'//TRIM(Num2LStr(TmpIndx))) 
      CALL WrFileNR(UnJac, ' HD_M_Lumped_RotationAcc_Y_'//TRIM(Num2LStr(TmpIndx))) 
      CALL WrFileNR(UnJac, ' HD_M_Lumped_RotationAcc_Z_'//TRIM(Num2LStr(TmpIndx))) 
   END DO   

   DO TmpIndx=1,u_HD%Morison%DistribMesh%NNodes
      CALL WrFileNR(UnJac, ' HD_M_Distrib_TranslationAcc_X_'//TRIM(Num2LStr(TmpIndx))) 
      CALL WrFileNR(UnJac, ' HD_M_Distrib_TranslationAcc_Y_'//TRIM(Num2LStr(TmpIndx))) 
      CALL WrFileNR(UnJac, ' HD_M_Distrib_TranslationAcc_Z_'//TRIM(Num2LStr(TmpIndx))) 
   END DO   
   DO TmpIndx=1,u_HD%Morison%DistribMesh%NNodes
      CALL WrFileNR(UnJac, ' HD_M_Distrib_RotationAcc_X_'//TRIM(Num2LStr(TmpIndx))) 
      CALL WrFileNR(UnJac, ' HD_M_Distrib_RotationAcc_Y_'//TRIM(Num2LStr(TmpIndx))) 
      CALL WrFileNR(UnJac, ' HD_M_Distrib_RotationAcc_Z_'//TRIM(Num2LStr(TmpIndx))) 
   END DO     
       
   DO TmpIndx=1,u_HD%Mesh%NNodes
      CALL WrFileNR(UnJac, ' HD_Mesh_TranslationAcc_X_'//TRIM(Num2LStr(TmpIndx))) 
      CALL WrFileNR(UnJac, ' HD_Mesh_TranslationAcc_Y_'//TRIM(Num2LStr(TmpIndx))) 
      CALL WrFileNR(UnJac, ' HD_Mesh_TranslationAcc_Z_'//TRIM(Num2LStr(TmpIndx))) 
   END DO   
   DO TmpIndx=1,u_HD%Mesh%NNodes
      CALL WrFileNR(UnJac, ' HD_Mesh_RotationAcc_X_'//TRIM(Num2LStr(TmpIndx))) 
      CALL WrFileNR(UnJac, ' HD_Mesh_RotationAcc_Y_'//TRIM(Num2LStr(TmpIndx))) 
      CALL WrFileNR(UnJac, ' HD_Mesh_RotationAcc_Z_'//TRIM(Num2LStr(TmpIndx))) 
   END DO 
   WRITE(UnJac,'()')    
      
   CALL WrMatrix(MeshMapData%Jacobian_ED_SD_HD,UnJac, p_FAST%OutFmt)
   CLOSE( UnJac )

#endif               
            
            
               ! Get the LU decomposition of this matrix using a LAPACK routine: 
               ! The result is of the form MeshMapDat%Jacobian_ED_SD_HD = P * L * U 

            CALL LAPACK_getrf( M=p_FAST%SizeJac_ED_SD_HD(4), N=p_FAST%SizeJac_ED_SD_HD(4), A=MeshMapData%Jacobian_ED_SD_HD, IPIV=MeshMapData%Jacobian_pivot, &
                              ErrStat=ErrStat2, ErrMsg=ErrMsg2 )
               CALL CheckError( ErrStat2, ErrMsg2  )
               IF ( ErrStat >= AbortErrLev ) RETURN 
            
         END IF         
            
         !-------------------------------------------------------------------------------------------------
         ! Solve for delta u: Jac*u_delta = - Fn_U_Resid
         !  using the LAPACK routine 
         !-------------------------------------------------------------------------------------------------
         
         u_delta = -Fn_U_Resid
         CALL LAPACK_getrs( TRANS="N", N=p_FAST%SizeJac_ED_SD_HD(4), A=MeshMapData%Jacobian_ED_SD_HD, &
                            IPIV=MeshMapData%Jacobian_pivot, B=u_delta, ErrStat=ErrStat2, ErrMsg=ErrMsg2 )
               CALL CheckError( ErrStat2, ErrMsg2  )
               IF ( ErrStat >= AbortErrLev ) RETURN 

         !-------------------------------------------------------------------------------------------------
         ! check for error, update inputs (u_ED and u_HD), and iterate again
         !-------------------------------------------------------------------------------------------------
                  
!         IF ( DOT_PRODUCT(u_delta, u_delta) <= TOL_Squared ) EXIT
         
         u = u + u_delta                  
         CALL Add_ED_SD_HD_u_delta( p_FAST, MeshMapData%Jac_u_indx, u_delta, u_ED, u_SD, u_HD )
                           
         K = K + 1
         
      END DO ! K
               
      !...............................................
      ! This is effectively doing option 2, where we set the input velocities and displacements based on the outputs we just calculated
      !...............................................
      
      !...............
      ! HD motion inputs: (from SD and ED)
      IF (p_FAST%CompHydro == Module_HD ) THEN
      
            ! Make copies of the accelerations we just solved for (so we don't overwrite them)         
         MeshMapData%u_HD_M_LumpedMesh%RotationAcc     = u_HD%Morison%LumpedMesh%RotationAcc
         MeshMapData%u_HD_M_LumpedMesh%TranslationAcc  = u_HD%Morison%LumpedMesh%TranslationAcc
         MeshMapData%u_HD_M_DistribMesh%RotationAcc    = u_HD%Morison%DistribMesh%RotationAcc
         MeshMapData%u_HD_M_DistribMesh%TranslationAcc = u_HD%Morison%DistribMesh%TranslationAcc
         MeshMapData%u_HD_Mesh%RotationAcc             = u_HD%Mesh%RotationAcc   
         MeshMapData%u_HD_Mesh%TranslationAcc          = u_HD%Mesh%TranslationAcc

            ! transfer the output data to inputs
            
         IF ( p_FAST%CompSub == Module_SD ) THEN
               ! Map SD outputs to HD inputs (keeping the accelerations we just calculated)
         
            CALL Transfer_SD_to_HD( y_SD, u_HD%Morison%LumpedMesh, u_HD%Morison%DistribMesh, MeshMapData, ErrStat2, ErrMsg2 )      
               CALL CheckError( ErrStat2, ErrMsg2  )
               IF ( ErrStat >= AbortErrLev ) RETURN     
               
               ! Map ED outputs to HD inputs (keeping the accelerations we just calculated):
               
            CALL Transfer_Point_to_Point( y_ED%PlatformPtMesh, u_HD%Mesh, MeshMapData%ED_P_2_HD_W_P, ErrStat2, ErrMsg2 ) 
               CALL CheckError(ErrStat2,ErrMsg2)
               IF (ErrStat >= AbortErrLev) RETURN      
                                             
         ELSE
            
            CALL Transfer_ED_to_HD( y_ED, u_HD, MeshMapData, ErrStat2, ErrMsg2 )
               CALL CheckError(ErrStat2,ErrMsg2)
               IF (ErrStat >= AbortErrLev) RETURN      
         
         END IF

         
            ! put the acceleration data (calucluted in this routine) back         
         u_HD%Morison%LumpedMesh%RotationAcc     = MeshMapData%u_HD_M_LumpedMesh%RotationAcc
         u_HD%Morison%LumpedMesh%TranslationAcc  = MeshMapData%u_HD_M_LumpedMesh%TranslationAcc  
         u_HD%Morison%DistribMesh%RotationAcc    = MeshMapData%u_HD_M_DistribMesh%RotationAcc    
         u_HD%Morison%DistribMesh%TranslationAcc = MeshMapData%u_HD_M_DistribMesh%TranslationAcc 
         u_HD%Mesh%RotationAcc                   = MeshMapData%u_HD_Mesh%RotationAcc    
         u_HD%Mesh%TranslationAcc                = MeshMapData%u_HD_Mesh%TranslationAcc 
         
         !......
                          
      END IF
      
      IF ( p_FAST%CompSub == Module_SD ) THEN       
         !...............
         ! SD motion inputs: (from ED)
                
            ! Map ED outputs to SD inputs (keeping the accelerations we just calculated):
      
         MeshMapData%u_SD_TPMesh%RotationAcc    = u_SD%TPMesh%RotationAcc   
         MeshMapData%u_SD_TPMesh%TranslationAcc = u_SD%TPMesh%TranslationAcc
         
         CALL Transfer_Point_to_Point( y_ED%PlatformPtMesh, u_SD%TPMesh, MeshMapData%ED_P_2_SD_TP, ErrStat2, ErrMsg2 ) 
            CALL CheckError(ErrStat2,ErrMsg2)
            IF (ErrStat >= AbortErrLev) RETURN      
               
         u_SD%TPMesh%RotationAcc    = MeshMapData%u_SD_TPMesh%RotationAcc    
         u_SD%TPMesh%TranslationAcc = MeshMapData%u_SD_TPMesh%TranslationAcc    
      END IF
         
      !...............................................
      ! We're finished
      !...............................................
      CALL CleanUp()
      
CONTAINS                                 
   !...............................................................................................................................
   SUBROUTINE U_ED_SD_HD_Residual( y_ED2, y_SD2, y_HD2, u_IN, U_Resid)
   ! transfer outputs of ED, HD, and SD (and any additional loads that get summed with them) into inputs for ED, HD, and SD
   !...............................................................................................................................
                                  
   TYPE(ED_OutputType)               , INTENT(IN   ) :: y_ED2                  ! System outputs
   TYPE(SD_OutputType)               , INTENT(IN   ) :: y_SD2                  ! System outputs
   TYPE(HydroDyn_OutputType)         , INTENT(IN   ) :: y_HD2                  ! System outputs
   REAL(ReKi)                        , INTENT(IN   ) :: u_in(:)
   REAL(ReKi)                        , INTENT(  OUT) :: U_Resid(:)

   INTEGER(IntKi)                                    :: i                      ! counter for ice leg loop
   
   !..................
   ! Set mooring line and ice inputs (which don't have acceleration fields and aren't used elsewhere in this routine, thus we're using the actual inputs (not a copy) 
   ! Note that these values get overwritten at the completion of this routine.)
   !..................
   
      IF ( p_FAST%CompMooring == Module_MAP ) THEN
         
         ! note: MAP_InputSolve must be called before setting ED loads inputs (so that motions are known for loads [moment] mapping)      
         CALL MAP_InputSolve( u_map, y_ED2, MeshMapData, ErrStat2, ErrMsg2 )
            CALL CheckError(ErrStat2,ErrMsg2)
            IF (ErrStat >= AbortErrLev) RETURN       
                                 
      ELSEIF ( p_FAST%CompMooring == Module_FEAM ) THEN
         
         ! note: FEAM_InputSolve must be called before setting ED loads inputs (so that motions are known for loads [moment] mapping)      
         CALL FEAM_InputSolve( u_FEAM, y_ED2, MeshMapData, ErrStat2, ErrMsg2 )
            CALL CheckError(ErrStat2,ErrMsg2)
            IF (ErrStat >= AbortErrLev) RETURN       
                        
      END IF     
   
      
      IF ( p_FAST%CompIce == Module_IceF ) THEN
         
         CALL IceFloe_InputSolve(  u_IceF, y_SD2, MeshMapData, ErrStat2, ErrMsg2 )
            CALL CheckError(ErrStat2,ErrMsg2)
            IF (ErrStat >= AbortErrLev) RETURN       
                                 
      ELSEIF ( p_FAST%CompIce == Module_IceD ) THEN
         
         DO i=1,p_FAST%numIceLegs
            
            CALL IceD_InputSolve(  u_IceD(i), y_SD2, MeshMapData, i, ErrStat2, ErrMsg2 )
               CALL CheckError(ErrStat2,ErrMsg2)
               IF (ErrStat >= AbortErrLev) RETURN  
               
         END DO
         
      END IF        
      
      
      IF ( p_FAST%CompSub == Module_SD ) THEN
      
         IF ( p_FAST%CompHydro == Module_HD ) THEN
            
         ! initialize these SD loads inputs here in case HD is used  (note from initialiazation that these meshes don't exist if HD isn't used)       
         MeshMapData%u_SD_LMesh%Force  = 0.0_ReKi
         MeshMapData%u_SD_LMesh%Moment = 0.0_ReKi
      
            
      !..................
      ! Get HD inputs on Morison%LumpedMesh and Morison%DistribMesh
      !..................
   
               ! SD motions to HD:
            CALL Transfer_SD_to_HD( y_SD2, MeshMapData%u_HD_M_LumpedMesh, MeshMapData%u_HD_M_DistribMesh, MeshMapData, ErrStat2, ErrMsg2 )
               CALL CheckError(ErrStat2,ErrMsg2)
               IF (ErrStat >= AbortErrLev) RETURN       
      
   
               ! Map ED motion output to HD inputs:
            CALL Transfer_Point_to_Point( y_ED2%PlatformPtMesh, MeshMapData%u_HD_Mesh, MeshMapData%ED_P_2_HD_W_P, ErrStat2, ErrMsg2 ) 
               CALL CheckError(ErrStat2,ErrMsg2)
               IF (ErrStat >= AbortErrLev) RETURN      
               
      !..................
      ! Get SD loads inputs (MeshMapData%u_HD_M_LumpedMesh and MeshMapData%u_HD_M_DistribMesh meshes must be set first)
      !..................
         
            ! Loads (outputs) from HD meshes transfered to SD LMesh (zero them out first because they get summed in Transfer_HD_to_SD)
         
            CALL Transfer_HD_to_SD( MeshMapData%u_SD_LMesh_2, MeshMapData%u_SD_LMesh, y_SD2%Y2Mesh, y_HD2, MeshMapData%u_HD_M_LumpedMesh, MeshMapData%u_HD_M_DistribMesh, MeshMapData, ErrStat2, ErrMsg2 )
               CALL CheckError(ErrStat2,ErrMsg2)
               IF (ErrStat >= AbortErrLev) RETURN               
         

            IF ( p_FAST%CompIce == Module_IceF ) THEN
               
               ! SD loads from IceFloe:
               IF ( y_IceF%iceMesh%Committed ) THEN      
                  ! we're mapping loads, so we also need the sibling meshes' displacements:
                  CALL Transfer_Point_to_Point( y_IceF%iceMesh, MeshMapData%u_SD_LMesh_2, MeshMapData%IceF_P_2_SD_P, ErrStat2, ErrMsg2, u_IceF%iceMesh, y_SD2%Y2Mesh )   
                     CALL CheckError( ErrStat2, ErrMsg2 )
                     IF (ErrStat >= AbortErrLev) RETURN

                  MeshMapData%u_SD_LMesh%Force  = MeshMapData%u_SD_LMesh%Force  + MeshMapData%u_SD_LMesh_2%Force
                  MeshMapData%u_SD_LMesh%Moment = MeshMapData%u_SD_LMesh%Moment + MeshMapData%u_SD_LMesh_2%Moment    
                  
!...          
#ifdef DEBUG_MESH_TRANSFER_ICE
   if (.not. calcJacobian) then
         CALL WrScr('********************************************************')
         CALL WrScr('****   IceF to SD point-to-point                   *****')
         CALL WrScr('********************************************************')
         CALL WriteMappingTransferToFile(MeshMapData%u_SD_LMesh_2, y_SD2%Y2Mesh, u_IceF%iceMesh, y_IceF%iceMesh,&
               MeshMapData%SD_P_2_IceF_P, MeshMapData%IceF_P_2_SD_P, &
               'SD_y2_IceF_Meshes_t'//TRIM(Num2LStr(this_time))//'.I.bin' )
         !print *
         !pause         
   end IF         
#endif                
                                    
               END IF
               
            ELSEIF ( p_FAST%CompIce == Module_IceD ) THEN
               
                ! SD loads from IceDyn:
               DO i=1,p_FAST%numIceLegs
                  
                  IF ( y_IceD(i)%PointMesh%Committed ) THEN      
                     ! we're mapping loads, so we also need the sibling meshes' displacements:
                     CALL Transfer_Point_to_Point( y_IceD(i)%PointMesh, MeshMapData%u_SD_LMesh_2, MeshMapData%IceD_P_2_SD_P(i), ErrStat2, ErrMsg2, u_IceD(i)%PointMesh, y_SD2%Y2Mesh )   
                        CALL CheckError( ErrStat2, ErrMsg2 )
                        IF (ErrStat >= AbortErrLev) RETURN

                     MeshMapData%u_SD_LMesh%Force  = MeshMapData%u_SD_LMesh%Force  + MeshMapData%u_SD_LMesh_2%Force
                     MeshMapData%u_SD_LMesh%Moment = MeshMapData%u_SD_LMesh%Moment + MeshMapData%u_SD_LMesh_2%Moment    
                     
                  END IF
                  
               END DO
               
            END IF   ! Ice loading
               
         END IF ! HD is used (IceFloe/IceDyn can't be used unless HydroDyn is used)


         
      !..................
      ! Get SD motions input
      !..................
         
         ! Motions (outputs) at ED platform ref point transfered to SD transition piece (input):
         CALL Transfer_Point_to_Point( y_ED2%PlatformPtMesh, MeshMapData%u_SD_TPMesh, MeshMapData%ED_P_2_SD_TP, ErrStat2, ErrMsg2 )   
            CALL CheckError( ErrStat2, ErrMsg2 )
            IF (ErrStat >= AbortErrLev) RETURN
            
      !..................
      ! Get ED loads input (from SD and possibly HD)
      !..................
            
         ! Loads (outputs) on the SD transition piece transfered to ED input location/mesh:
            ! we're mapping loads, so we also need the sibling meshes' displacements:
         CALL Transfer_Point_to_Point( y_SD2%Y1Mesh, MeshMapData%u_ED_PlatformPtMesh, MeshMapData%SD_TP_2_ED_P, ErrStat2, ErrMsg2, MeshMapData%u_SD_TPMesh, y_ED2%PlatformPtMesh ) !MeshMapData%u_SD_TPMesh contains the orientations needed for moment calculations
            CALL CheckError( ErrStat2, ErrMsg2 )
            IF (ErrStat >= AbortErrLev) RETURN
                        
               ! WAMIT loads from HD get added to this load:
         IF ( y_HD2%Mesh%Committed  ) THEN
   
            ! we're mapping loads, so we also need the sibling meshes' displacements:
            CALL Transfer_Point_to_Point( y_HD2%Mesh, MeshMapData%u_ED_PlatformPtMesh_2, MeshMapData%HD_W_P_2_ED_P, ErrStat2, ErrMsg2, MeshMapData%u_HD_Mesh, y_ED2%PlatformPtMesh ) !u_SD contains the orientations needed for moment calculations
               CALL CheckError( ErrStat2, ErrMsg2 )
               IF (ErrStat >= AbortErrLev) RETURN
         
            MeshMapData%u_ED_PlatformPtMesh%Force  = MeshMapData%u_ED_PlatformPtMesh%Force  + MeshMapData%u_ED_PlatformPtMesh_2%Force
            MeshMapData%u_ED_PlatformPtMesh%Moment = MeshMapData%u_ED_PlatformPtMesh%Moment + MeshMapData%u_ED_PlatformPtMesh_2%Moment
   
         END IF              
            
      ELSE !IF ( p_FAST%CompHydro == Module_HD ) THEN ! This routine assumes either SubDyn or HydroDyn is in use, so I'm not going to do this check

      !..................
      ! Get HD inputs on 3 meshes
      !..................
         
         ! Map ED motion outputs to HD inputs:
         ! basically, we want to call Transfer_ED_to_HD, except we have the meshes in a different data structure (not a copy of u_HD)
         ! CALL Transfer_ED_to_HD( y_ED2, u_HD, MeshMapData, ErrStat2, ErrMsg2 ) 
         ! so, here are the transfers, again.
         
         
         ! These are the motions for the lumped point loads associated the WAMIT body:
         CALL Transfer_Point_to_Point( y_ED2%PlatformPtMesh, MeshMapData%u_HD_Mesh, MeshMapData%ED_P_2_HD_W_P, ErrStat2, ErrMsg2 )
            CALL CheckError( ErrStat2, ErrMsg2 )
            IF (ErrStat >= AbortErrLev) RETURN
   
         ! These are the motions for the lumped point loads associated viscous drag on the WAMIT body and/or filled/flooded lumped forces of the WAMIT body
         CALL Transfer_Point_to_Point( y_ED2%PlatformPtMesh, MeshMapData%u_HD_M_LumpedMesh, MeshMapData%ED_P_2_HD_M_P, ErrStat2, ErrMsg2 )
            CALL CheckError( ErrStat2, ErrMsg2 )
            IF (ErrStat >= AbortErrLev) RETURN
                  
         ! These are the motions for the line2 (distributed) loads associated viscous drag on the WAMIT body and/or filled/flooded distributed forces of the WAMIT body
         CALL Transfer_Point_to_Line2( y_ED2%PlatformPtMesh, MeshMapData%u_HD_M_DistribMesh, MeshMapData%ED_P_2_HD_M_L, ErrStat2, ErrMsg2 )
            CALL CheckError( ErrStat2, ErrMsg2 )
            IF (ErrStat >= AbortErrLev) RETURN
            
      !..................
      ! Get ED loads input (from HD only)
      !..................
            
            ! we're mapping loads, so we also need the sibling meshes' displacements:
         CALL Transfer_Point_to_Point( y_HD2%AllHdroOrigin, MeshMapData%u_ED_PlatformPtMesh, MeshMapData%HD_W_P_2_ED_P, ErrStat2, ErrMsg2, MeshMapData%u_HD_Mesh, y_ED2%PlatformPtMesh) !u_HD and u_mapped_positions contain the displaced positions for load calculations
            CALL CheckError( ErrStat2, ErrMsg2 )
            IF (ErrStat >= AbortErrLev) RETURN
                                                                           
      END IF
      
   !..................
   ! Get remaining portion of ED loads input on MeshMapData%u_ED_PlatformPtMesh (must do this after MeshMapData%u_SD_TPMesh and MeshMapData%u_HD_Mesh are set)
   !   at this point, MeshMapData%u_ED_PlatformPtMesh contains the portion of loads from SD and/or HD
   !..................
                     
         
         ! Get the loads for ED from a mooring module and add them:
      IF ( p_FAST%CompMooring == Module_MAP ) THEN
         CALL Transfer_Point_to_Point( y_MAP%PtFairleadLoad, MeshMapData%u_ED_PlatformPtMesh_2, MeshMapData%MAP_P_2_ED_P, ErrStat2, ErrMsg2, u_MAP%PtFairleadDisplacement, y_ED2%PlatformPtMesh ) !u_MAP and y_ED contain the displacements needed for moment calculations
            CALL CheckError( ErrStat2, ErrMsg2 )
            IF (ErrStat >= AbortErrLev) RETURN               
            
         MeshMapData%u_ED_PlatformPtMesh%Force  = MeshMapData%u_ED_PlatformPtMesh%Force  + MeshMapData%u_ED_PlatformPtMesh_2%Force
         MeshMapData%u_ED_PlatformPtMesh%Moment = MeshMapData%u_ED_PlatformPtMesh%Moment + MeshMapData%u_ED_PlatformPtMesh_2%Moment
            
      ELSEIF ( p_FAST%CompMooring == Module_FEAM ) THEN
         CALL Transfer_Point_to_Point( y_FEAM%PtFairleadLoad, MeshMapData%u_ED_PlatformPtMesh_2, MeshMapData%FEAM_P_2_ED_P, ErrStat2, ErrMsg2, u_FEAM%PtFairleadDisplacement, y_ED2%PlatformPtMesh ) !u_FEAM and y_ED contain the displacements needed for moment calculations
            CALL CheckError( ErrStat2, ErrMsg2 )
            IF (ErrStat >= AbortErrLev) RETURN  
            
         MeshMapData%u_ED_PlatformPtMesh%Force  = MeshMapData%u_ED_PlatformPtMesh%Force  + MeshMapData%u_ED_PlatformPtMesh_2%Force
         MeshMapData%u_ED_PlatformPtMesh%Moment = MeshMapData%u_ED_PlatformPtMesh%Moment + MeshMapData%u_ED_PlatformPtMesh_2%Moment            
      END IF
                                                   
   !..................
   ! Calculate the residual with these new inputs:
   !..................                  
         
      CALL Create_ED_SD_HD_UVector(U_Resid, MeshMapData%u_ED_PlatformPtMesh, MeshMapData%u_SD_TPMesh, MeshMapData%u_SD_LMesh, &
                                            MeshMapData%u_HD_M_LumpedMesh,   MeshMapData%u_HD_M_DistribMesh, MeshMapData%u_HD_Mesh, p_FAST )         
         
      U_Resid = u_in - U_Resid
   
            
   END SUBROUTINE U_ED_SD_HD_Residual   
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

         CALL WrScr( ' ED_SD_HD_InputOutputSolve:'//TRIM(Msg) )
         ErrStat = MAX(ErrStat, ErrID)

         !.........................................................................................................................
         ! Clean up if we're going to return on error: close files, deallocate local arrays
         !.........................................................................................................................
         IF ( ErrStat >= AbortErrLev ) CALL CleanUp()

      END IF


   END SUBROUTINE CheckError   
   !...............................................................................................................................
   SUBROUTINE CleanUp()
      INTEGER(IntKi)             :: ErrStat3    ! The error identifier (ErrStat)
      CHARACTER(1024)            :: ErrMsg3     ! The error message (ErrMsg)
                  
         
      IF ( calcJacobian ) THEN
         CALL ED_DestroyInput( u_ED_perturb, ErrStat3, ErrMsg3 )
            IF (ErrStat3 /= ErrID_None) CALL WrScr(' ED_SD_HD_InputOutputSolve/ED_DestroyInput: '//TRIM(ErrMsg3) )
         CALL ED_DestroyOutput(y_ED_perturb, ErrStat3, ErrMsg3 )
            IF (ErrStat3 /= ErrID_None) CALL WrScr(' ED_SD_HD_InputOutputSolve/ED_DestroyOutput: '//TRIM(ErrMsg3) )
         
         CALL SD_DestroyInput( u_SD_perturb, ErrStat3, ErrMsg3 )
            IF (ErrStat3 /= ErrID_None) CALL WrScr(' ED_SD_HD_InputOutputSolve/SD_DestroyInput: '//TRIM(ErrMsg3) )
         CALL SD_DestroyOutput(y_SD_perturb, ErrStat3, ErrMsg3 )
            IF (ErrStat3 /= ErrID_None) CALL WrScr(' ED_SD_HD_InputOutputSolve/SD_DestroyOutput: '//TRIM(ErrMsg3) )                        

         CALL HydroDyn_DestroyInput( u_HD_perturb, ErrStat3, ErrMsg3 )
            IF (ErrStat3 /= ErrID_None) CALL WrScr(' ED_SD_HD_InputOutputSolve/HydroDyn_DestroyInput: '//TRIM(ErrMsg3) )
         CALL HydroDyn_DestroyOutput(y_HD_perturb, ErrStat3, ErrMsg3 )
            IF (ErrStat3 /= ErrID_None) CALL WrScr(' ED_SD_HD_InputOutputSolve/HydroDyn_DestroyOutput: '//TRIM(ErrMsg3) )                              
      END IF
      
   
   END SUBROUTINE CleanUp
   !...............................................................................................................................
END SUBROUTINE ED_SD_HD_InputOutputSolve
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE Init_ED_SD_HD_Jacobian( p_FAST, MeshMapData, ED_PlatformPtMesh, SD_TPMesh, SD_LMesh, HD_M_LumpedMesh, HD_M_DistribMesh, &
                                   HD_WAMIT_Mesh, ErrStat, ErrMsg)

   TYPE(FAST_ParameterType)          , INTENT(INOUT) :: p_FAST                     
   TYPE(FAST_ModuleMapType)          , INTENT(INOUT) :: MeshMapData
   
      ! input meshes for each of the 3 modules:
   TYPE(MeshType)                    , INTENT(IN   ) :: ED_PlatformPtMesh
   TYPE(MeshType)                    , INTENT(IN   ) :: SD_TPMesh
   TYPE(MeshType)                    , INTENT(IN   ) :: SD_LMesh
   TYPE(MeshType)                    , INTENT(IN   ) :: HD_M_LumpedMesh
   TYPE(MeshType)                    , INTENT(IN   ) :: HD_M_DistribMesh
   TYPE(MeshType)                    , INTENT(IN   ) :: HD_WAMIT_Mesh
   
   INTEGER(IntKi)                    , INTENT(  OUT) :: ErrStat                   ! Error status of the operation
   CHARACTER(*)                      , INTENT(  OUT) :: ErrMsg                    ! Error message if ErrStat /= ErrID_None
   
   
   
      ! local variables:
   INTEGER(IntKi)                :: i, j, index
   
   ErrStat = ErrID_None
   ErrMsg  = ""
   
      ! determine how many inputs there are between the 3 modules (ED, SD, HD)
   
   p_FAST%SizeJac_ED_SD_HD(1) = ED_PlatformPtMesh%NNodes*6        ! ED inputs: 3 forces and 3 moments per node (only 1 node)
                  
   p_FAST%SizeJac_ED_SD_HD(2) = SD_TPMesh%NNodes*6                ! SD inputs: 6 accelerations per node (size of SD input from ED) 
   IF ( p_FAST%CompHydro == Module_HD ) THEN   
      p_FAST%SizeJac_ED_SD_HD(2) = p_FAST%SizeJac_ED_SD_HD(2) &   
                                 + SD_LMesh%NNodes *6             ! SD inputs: 6 loads per node (size of SD input from HD)       
   END IF
               
   p_FAST%SizeJac_ED_SD_HD(3) = HD_M_LumpedMesh%NNodes *6 &    ! HD inputs: 6 accelerations per node (on each Morison mesh) 
                              + HD_M_DistribMesh%NNodes*6 &    ! HD inputs: 6 accelerations per node (on each Morison mesh)
                              + HD_WAMIT_Mesh%NNodes*6         ! HD inputs: 6 accelerations per node (on the WAMIT mesh)      
   
   p_FAST%SizeJac_ED_SD_HD(4) = p_FAST%SizeJac_ED_SD_HD(1) &   ! all the inputs from these 3 modules
                              + p_FAST%SizeJac_ED_SD_HD(2) &
                              + p_FAST%SizeJac_ED_SD_HD(3)
                  

      ! allocate matrix to store jacobian 
   CALL AllocAry( MeshMapData%Jacobian_ED_SD_HD, p_FAST%SizeJac_ED_SD_HD(4), p_FAST%SizeJac_ED_SD_HD(4), "Jacobian for ED-SD-HD", ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
         
      ! allocate matrix to store index to help us figure out what the ith value of the u vector really means
   ALLOCATE ( MeshMapData%Jac_u_indx( p_FAST%SizeJac_ED_SD_HD(4), 3 ), STAT = ErrStat )
      IF ( ErrStat /= 0 ) THEN
         ErrStat = ErrID_Fatal
         ErrMsg = 'Cannot allocate Jac_u_indx.'
         RETURN
      END IF
         
   ! fill matrix to store index to help us figure out what the ith value of the u vector really means
   ! ( see Create_ED_SD_HD_UVector() ... these MUST match )
   ! column 1 indicates module's mesh and field
   ! column 2 indicates the first index of the acceleration/load field
   ! column 3 is the node
      
   !...............
   ! ED inputs:   
   !...............
   
   index = 1
   do i=1,ED_PlatformPtMesh%NNodes
      do j=1,3
         MeshMapData%Jac_u_indx(index,1) =  1 !Module/Mesh/Field: u_ED%PlatformPtMesh%Force = 1
         MeshMapData%Jac_u_indx(index,2) =  j !index:  j
         MeshMapData%Jac_u_indx(index,3) =  i !Node:   i
         index = index + 1
      end do !j      
   end do !i
   
   do i=1,ED_PlatformPtMesh%NNodes
      do j=1,3
         MeshMapData%Jac_u_indx(index,1) =  2 !Module/Mesh/Field: u_ED%PlatformPtMesh%Moment = 2
         MeshMapData%Jac_u_indx(index,2) =  j !index:  j
         MeshMapData%Jac_u_indx(index,3) =  i !Node:   i
         index = index + 1
      end do !j      
   end do !i
   
      
   !...............
   ! SD inputs:   
   !...............
      
   ! SD_TPMesh                        
   do i=1,SD_TPMesh%NNodes
      do j=1,3
         MeshMapData%Jac_u_indx(index,1) =  3 !Module/Mesh/Field: u_SD%TPMesh%TranslationAcc = 3
         MeshMapData%Jac_u_indx(index,2) =  j !index:  j
         MeshMapData%Jac_u_indx(index,3) =  i !Node:   i
         index = index + 1                  
      end do !j                             
   end do !i                                
                                            
   do i=1,SD_TPMesh%NNodes                  
      do j=1,3                              
         MeshMapData%Jac_u_indx(index,1) =  4 !Module/Mesh/Field:  u_SD%TPMesh%RotationAcc = 4
         MeshMapData%Jac_u_indx(index,2) =  j !index:  j
         MeshMapData%Jac_u_indx(index,3) =  i !Node:   i
         index = index + 1
      end do !j      
   end do !i   
   
   IF ( p_FAST%CompHydro == Module_HD ) THEN   ! this SD mesh linked only when HD is enabled
   
      ! SD_LMesh
      do i=1,SD_LMesh%NNodes
         do j=1,3
            MeshMapData%Jac_u_indx(index,1) =  5 !Module/Mesh/Field: u_SD%LMesh%Force = 5
            MeshMapData%Jac_u_indx(index,2) =  j !index:  j
            MeshMapData%Jac_u_indx(index,3) =  i !Node:   i
            index = index + 1                  
         end do !j                             
      end do !i                                
                                            
      do i=1,SD_LMesh%NNodes                   
         do j=1,3                              
            MeshMapData%Jac_u_indx(index,1) =  6 !Module/Mesh/Field: u_SD%LMesh%Moment = 6
            MeshMapData%Jac_u_indx(index,2) =  j !index:  j
            MeshMapData%Jac_u_indx(index,3) =  i !Node:   i
            index = index + 1
         end do !j      
      end do !i 
      
   END IF
   
   !...............
   ! HD inputs:
   !...............
         
   !(Morison%LumpedMesh)
   do i=1,HD_M_LumpedMesh%NNodes
      do j=1,3
         MeshMapData%Jac_u_indx(index,1) =  7 !Module/Mesh/Field: u_HD%Morison%LumpedMesh%TranslationAcc = 7
         MeshMapData%Jac_u_indx(index,2) =  j !index:  j
         MeshMapData%Jac_u_indx(index,3) =  i !Node:   i
         index = index + 1
      end do !j      
   end do !i
   
   do i=1,HD_M_LumpedMesh%NNodes
      do j=1,3
         MeshMapData%Jac_u_indx(index,1) =  8 !Module/Mesh/Field:  u_HD%Morison%LumpedMesh%RotationAcc = 8
         MeshMapData%Jac_u_indx(index,2) =  j !index:  j
         MeshMapData%Jac_u_indx(index,3) =  i !Node:   i
         index = index + 1
      end do !j      
   end do !i     
   
   
   !(Morison%DistribMesh)
   do i=1,HD_M_DistribMesh%NNodes
      do j=1,3
         MeshMapData%Jac_u_indx(index,1) =  9 !Module/Mesh/Field: u_HD%Morison%DistribMesh%TranslationAcc = 9
         MeshMapData%Jac_u_indx(index,2) =  j !index:  j
         MeshMapData%Jac_u_indx(index,3) =  i !Node:   i
         index = index + 1
      end do !j      
   end do !i
   
   do i=1,HD_M_DistribMesh%NNodes
      do j=1,3
         MeshMapData%Jac_u_indx(index,1) = 10 !Module/Mesh/Field:  u_HD%Morison%DistribMesh%RotationAcc = 10
         MeshMapData%Jac_u_indx(index,2) =  j !index:  j
         MeshMapData%Jac_u_indx(index,3) =  i !Node:   i
         index = index + 1
      end do !j      
   end do !i     
   
   !(Mesh)
   do i=1,HD_WAMIT_Mesh%NNodes
      do j=1,3
         MeshMapData%Jac_u_indx(index,1) = 11 !Module/Mesh/Field: u_HD%Mesh%TranslationAcc = 11
         MeshMapData%Jac_u_indx(index,2) =  j !index:  j
         MeshMapData%Jac_u_indx(index,3) =  i !Node:   i
         index = index + 1
      end do !j      
   end do !i
   
   do i=1,HD_WAMIT_Mesh%NNodes
      do j=1,3
         MeshMapData%Jac_u_indx(index,1) = 12 !Module/Mesh/Field:  u_HD%Mesh%RotationAcc = 12
         MeshMapData%Jac_u_indx(index,2) =  j !index:  j
         MeshMapData%Jac_u_indx(index,3) =  i !Node:   i
         index = index + 1
      end do !j      
   end do !i        
   
END SUBROUTINE Init_ED_SD_HD_Jacobian
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE Create_ED_SD_HD_UVector(u, ED_PlatformPtMesh, SD_TPMesh, SD_LMesh, HD_M_LumpedMesh, HD_M_DistribMesh, HD_WAMIT_Mesh, p_FAST )
! This routine basically packs the relevant parts of the modules' input meshes for use in this InputOutput solve.
! Do not change the order of this packing without changing subroutine Init_ED_SD_HD_Jacobian()!
!..................................................................................................................................
   
   REAL(ReKi)                        , INTENT(INOUT) :: u(:)                     ! output u vector
   
      ! input meshes for each of the 3 modules:
   TYPE(MeshType)                    , INTENT(IN   ) :: ED_PlatformPtMesh
   TYPE(MeshType)                    , INTENT(IN   ) :: SD_TPMesh
   TYPE(MeshType)                    , INTENT(IN   ) :: SD_LMesh
   TYPE(MeshType)                    , INTENT(IN   ) :: HD_M_LumpedMesh
   TYPE(MeshType)                    , INTENT(IN   ) :: HD_M_DistribMesh
   TYPE(MeshType)                    , INTENT(IN   ) :: HD_WAMIT_Mesh
   
   TYPE(FAST_ParameterType)          , INTENT(IN   ) :: p_FAST
   
   
      ! local variables:
   INTEGER(IntKi)                :: i, indx_first, indx_last
   
   !...............
   ! ED inputs:   
   !...............
   u( 1: 3) = ED_PlatformPtMesh%Force(:,1) / p_FAST%UJacSclFact
   u( 4: 6) = ED_PlatformPtMesh%Moment(:,1) / p_FAST%UJacSclFact  
            
   !...............
   ! SD inputs (SD_TPMesh):      
   !...............
   indx_first = 7   
   do i=1,SD_TPMesh%NNodes 
      indx_last  = indx_first + 2
      u(indx_first:indx_last) = SD_TPMesh%TranslationAcc(:,i) 
      indx_first = indx_last + 1
   end do

   do i=1,SD_TPMesh%NNodes 
      indx_last  = indx_first + 2
      u(indx_first:indx_last) = SD_TPMesh%RotationAcc(:,i) 
      indx_first = indx_last + 1
   end do
         
   IF ( p_FAST%CompHydro == Module_HD ) THEN   ! this SD mesh linked only when HD is enabled
      ! SD inputs (SD_LMesh):        
      do i=1,SD_LMesh%NNodes
         indx_last  = indx_first + 2 
         u(indx_first:indx_last) = SD_LMesh%Force(:,i) / p_FAST%UJacSclFact
         indx_first = indx_last + 1
      end do
     
      do i=1,SD_LMesh%NNodes
         indx_last  = indx_first + 2 
         u(indx_first:indx_last) = SD_LMesh%Moment(:,i) / p_FAST%UJacSclFact
         indx_first = indx_last + 1
      end do
   END IF
   
   !...............
   ! HD inputs (Morison%LumpedMesh):
   !...............
   do i=1,HD_M_LumpedMesh%NNodes
      indx_last  = indx_first + 2 
      u(indx_first:indx_last) = HD_M_LumpedMesh%TranslationAcc(:,i)
      indx_first = indx_last + 1
   end do
      
   do i=1,HD_M_LumpedMesh%NNodes
      indx_last  = indx_first + 2 
      u(indx_first:indx_last) = HD_M_LumpedMesh%RotationAcc(:,i)
      indx_first = indx_last + 1
   end do
      
   ! HD inputs (Morison%DistribMesh):
   do i=1,HD_M_DistribMesh%NNodes
      indx_last  = indx_first + 2 
      u(indx_first:indx_last) = HD_M_DistribMesh%TranslationAcc(:,i)
      indx_first = indx_last + 1
   end do
      
   do i=1,HD_M_DistribMesh%NNodes
      indx_last  = indx_first + 2 
      u(indx_first:indx_last) = HD_M_DistribMesh%RotationAcc(:,i)
      indx_first = indx_last + 1
   end do
                  
   ! HD inputs (Mesh):
   do i=1,HD_WAMIT_Mesh%NNodes
      indx_last  = indx_first + 2 
      u(indx_first:indx_last) = HD_WAMIT_Mesh%TranslationAcc(:,i)
      indx_first = indx_last + 1
   end do
      
   do i=1,HD_WAMIT_Mesh%NNodes
      indx_last  = indx_first + 2 
      u(indx_first:indx_last) = HD_WAMIT_Mesh%RotationAcc(:,i)
      indx_first = indx_last + 1
   end do   
   
   
END SUBROUTINE Create_ED_SD_HD_UVector
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE Add_ED_SD_HD_u_delta( p_FAST, Jac_u_indx, u_delta, u_ED, u_SD, u_HD )
! This routine adds u_delta to the corresponding mesh field and scales it as appropriate
!..................................................................................................................................
   TYPE(FAST_ParameterType)            , INTENT(IN   ) :: p_FAST         ! Glue-code simulation parameters
   INTEGER( IntKi )                    , INTENT(IN   ) :: Jac_u_indx(:,:)
   REAL( ReKi )                        , INTENT(IN   ) :: u_delta(:)
   TYPE(ED_InputType)                  , INTENT(INOUT) :: u_ED           ! ED System inputs 
   TYPE(SD_InputType)                  , INTENT(INOUT) :: u_SD           ! SD System inputs 
   TYPE(HydroDyn_InputType)            , INTENT(INOUT) :: u_HD           ! SD System inputs 
   
   ! local variables
   INTEGER                                             :: n
   INTEGER                                             :: fieldIndx
   INTEGER                                             :: node
   
   
   DO n = 1,SIZE(u_delta)
      
      fieldIndx = Jac_u_indx(n,2) 
      node      = Jac_u_indx(n,3) 
   
         ! determine which mesh we're trying to perturb and perturb the input:
      SELECT CASE( Jac_u_indx(n,1) )
      
      CASE ( 1) !Module/Mesh/Field: u_ED%PlatformPtMesh%Force = 1
         u_ED%PlatformPtMesh%Force( fieldIndx,node) = u_ED%PlatformPtMesh%Force( fieldIndx,node) + u_delta(n) * p_FAST%UJacSclFact       
      CASE ( 2) !Module/Mesh/Field: u_ED%PlatformPtMesh%Moment = 2
         u_ED%PlatformPtMesh%Moment(fieldIndx,node) = u_ED%PlatformPtMesh%Moment(fieldIndx,node) + u_delta(n) * p_FAST%UJacSclFact       
      
      CASE ( 3) !Module/Mesh/Field: u_SD%TPMesh%TranslationAcc = 3
         u_SD%TPMesh%TranslationAcc(fieldIndx,node) = u_SD%TPMesh%TranslationAcc(fieldIndx,node) + u_delta(n)        
      CASE ( 4) !Module/Mesh/Field: u_SD%TPMesh%RotationAcc = 4
         u_SD%TPMesh%RotationAcc(   fieldIndx,node) = u_SD%TPMesh%RotationAcc(   fieldIndx,node) + u_delta(n)        
      CASE ( 5) !Module/Mesh/Field: u_SD%LMesh%Force = 5
         u_SD%LMesh%Force( fieldIndx,node) = u_SD%LMesh%Force( fieldIndx,node) + u_delta(n) * p_FAST%UJacSclFact     
      CASE ( 6) !Module/Mesh/Field: u_SD%LMesh%Moment = 6
         u_SD%LMesh%Moment(fieldIndx,node) = u_SD%LMesh%Moment(fieldIndx,node) + u_delta(n) * p_FAST%UJacSclFact     
      
      CASE ( 7) !Module/Mesh/Field: u_HD%Morison%LumpedMesh%TranslationAcc = 7
         u_HD%Morison%LumpedMesh%TranslationAcc(fieldIndx,node) = u_HD%Morison%LumpedMesh%TranslationAcc(fieldIndx,node) + u_delta(n)        
      CASE ( 8) !Module/Mesh/Field: u_HD%Morison%LumpedMesh%RotationAcc = 8
         u_HD%Morison%LumpedMesh%RotationAcc(   fieldIndx,node) = u_HD%Morison%LumpedMesh%RotationAcc(   fieldIndx,node) + u_delta(n)        
      CASE ( 9) !Module/Mesh/Field: u_HD%Morison%DistribMesh%TranslationAcc = 9
         u_HD%Morison%DistribMesh%TranslationAcc(fieldIndx,node) = u_HD%Morison%DistribMesh%TranslationAcc(fieldIndx,node) + u_delta(n)        
      CASE (10) !Module/Mesh/Field: u_HD%Morison%DistribMesh%RotationAcc = 10
         u_HD%Morison%DistribMesh%RotationAcc(   fieldIndx,node) = u_HD%Morison%DistribMesh%RotationAcc(   fieldIndx,node) + u_delta(n)      
      CASE (11) !Module/Mesh/Field: u_HD%Mesh%TranslationAcc = 11
         u_HD%Mesh%TranslationAcc(   fieldIndx,node) = u_HD%Mesh%TranslationAcc(   fieldIndx,node) + u_delta(n)      
      CASE (12) !Module/Mesh/Field: u_HD%Mesh%RotationAcc = 12
         u_HD%Mesh%RotationAcc(   fieldIndx,node) = u_HD%Mesh%RotationAcc(   fieldIndx,node) + u_delta(n)      
      
      END SELECT
                                   
   END DO
   
        
END SUBROUTINE Add_ED_SD_HD_u_delta
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE Perturb_u_ED_SD_HD( p_FAST, Jac_u_indx, n, u_perturb, u_ED_perturb, u_SD_perturb, u_HD_perturb, perturb )
! This routine perturbs the nth element of the u array (and ED input/output it corresponds to)
!...............................................................................................................................
   TYPE(FAST_ParameterType)            , INTENT(IN   ) :: p_FAST                ! Glue-code simulation parameters
   INTEGER( IntKi )                    , INTENT(IN   ) :: Jac_u_indx(:,:)
   INTEGER( IntKi )                    , INTENT(IN   ) :: n
   REAL( ReKi )                        , INTENT(INOUT) :: u_perturb(:)
   TYPE(ED_InputType),        OPTIONAL , INTENT(INOUT) :: u_ED_perturb           ! ED System inputs  (needed only when 1 <= n <=  6)
   TYPE(SD_InputType),        OPTIONAL , INTENT(INOUT) :: u_SD_perturb           ! SD System inputs  (needed only when 7 <= n <= 12) [if SD is used]
   TYPE(HydroDyn_InputType),  OPTIONAL , INTENT(INOUT) :: u_HD_perturb           ! HD System inputs  (needed only when 6+NumSDNodes <= n <= inf) [if HD is used and SD is used. if SD not used, 
   REAL( ReKi )                        , INTENT(  OUT) :: perturb
   
   ! local variables
   INTEGER                                             :: fieldIndx
   INTEGER                                             :: node
   
      
   fieldIndx = Jac_u_indx(n,2) 
   node      = Jac_u_indx(n,3) 
   
      ! determine which mesh we're trying to perturb and perturb the input:
   SELECT CASE( Jac_u_indx(n,1) )
      
   CASE ( 1) !Module/Mesh/Field: u_ED%PlatformPtMesh%Force = 1
      perturb = GetPerturb( u_ED_perturb%PlatformPtMesh%Force(fieldIndx , node) )
      u_ED_perturb%PlatformPtMesh%Force( fieldIndx,node) = u_ED_perturb%PlatformPtMesh%Force( fieldIndx,node) + perturb * p_FAST%UJacSclFact       
   CASE ( 2) !Module/Mesh/Field: u_ED%PlatformPtMesh%Moment = 2
      perturb = GetPerturb( u_ED_perturb%PlatformPtMesh%Moment(fieldIndx , node) )
      u_ED_perturb%PlatformPtMesh%Moment(fieldIndx,node) = u_ED_perturb%PlatformPtMesh%Moment(fieldIndx,node) + perturb * p_FAST%UJacSclFact       
      
   CASE ( 3) !Module/Mesh/Field: u_SD%TPMesh%TranslationAcc = 3
      perturb = GetPerturb( u_SD_perturb%TPMesh%TranslationAcc(fieldIndx , node) )
      u_SD_perturb%TPMesh%TranslationAcc(fieldIndx,node) = u_SD_perturb%TPMesh%TranslationAcc(fieldIndx,node) + perturb        
   CASE ( 4) !Module/Mesh/Field: u_SD%TPMesh%RotationAcc = 4
      perturb = GetPerturb( u_SD_perturb%TPMesh%RotationAcc(fieldIndx , node) )
      u_SD_perturb%TPMesh%RotationAcc(   fieldIndx,node) = u_SD_perturb%TPMesh%RotationAcc(   fieldIndx,node) + perturb        
   CASE ( 5) !Module/Mesh/Field: u_SD%LMesh%Force = 5
      perturb = GetPerturb( u_SD_perturb%LMesh%Force(fieldIndx , node) )
      u_SD_perturb%LMesh%Force( fieldIndx,node) = u_SD_perturb%LMesh%Force( fieldIndx,node) + perturb * p_FAST%UJacSclFact     
   CASE ( 6) !Module/Mesh/Field: u_SD%LMesh%Moment = 6
      perturb = GetPerturb( u_SD_perturb%LMesh%Moment(fieldIndx , node) )
      u_SD_perturb%LMesh%Moment(fieldIndx,node) = u_SD_perturb%LMesh%Moment(fieldIndx,node) + perturb * p_FAST%UJacSclFact     
      
   CASE ( 7) !Module/Mesh/Field: u_HD%Morison%LumpedMesh%TranslationAcc = 7
      perturb = GetPerturb( u_HD_perturb%Morison%LumpedMesh%TranslationAcc(fieldIndx , node) )
      u_HD_perturb%Morison%LumpedMesh%TranslationAcc(fieldIndx,node) = u_HD_perturb%Morison%LumpedMesh%TranslationAcc(fieldIndx,node) + perturb        
   CASE ( 8) !Module/Mesh/Field: u_HD%Morison%LumpedMesh%RotationAcc = 8
      perturb = GetPerturb( u_HD_perturb%Morison%LumpedMesh%RotationAcc(fieldIndx , node) )
      u_HD_perturb%Morison%LumpedMesh%RotationAcc(   fieldIndx,node) = u_HD_perturb%Morison%LumpedMesh%RotationAcc(   fieldIndx,node) + perturb        
   CASE ( 9) !Module/Mesh/Field: u_HD%Morison%DistribMesh%TranslationAcc = 9
      perturb = GetPerturb( u_HD_perturb%Morison%DistribMesh%TranslationAcc(fieldIndx , node) )
      u_HD_perturb%Morison%DistribMesh%TranslationAcc(fieldIndx,node) = u_HD_perturb%Morison%DistribMesh%TranslationAcc(fieldIndx,node) + perturb        
   CASE (10) !Module/Mesh/Field: u_HD%Morison%DistribMesh%RotationAcc = 10
      perturb = GetPerturb( u_HD_perturb%Morison%DistribMesh%RotationAcc(fieldIndx , node) )
      u_HD_perturb%Morison%DistribMesh%RotationAcc(   fieldIndx,node) = u_HD_perturb%Morison%DistribMesh%RotationAcc(   fieldIndx,node) + perturb      
   CASE (11) !Module/Mesh/Field: u_HD%Mesh%TranslationAcc = 11
      perturb = GetPerturb( u_HD_perturb%Mesh%TranslationAcc(fieldIndx , node) )
      u_HD_perturb%Mesh%TranslationAcc(   fieldIndx,node) = u_HD_perturb%Mesh%TranslationAcc(   fieldIndx,node) + perturb      
   CASE (12) !Module/Mesh/Field: u_HD%Mesh%RotationAcc = 12
      perturb = GetPerturb( u_HD_perturb%Mesh%RotationAcc(fieldIndx , node) )
      u_HD_perturb%Mesh%RotationAcc(   fieldIndx,node) = u_HD_perturb%Mesh%RotationAcc(   fieldIndx,node) + perturb      
            
      
   END SELECT
                                   
   u_perturb(n) = u_perturb(n) + perturb
   
        
END SUBROUTINE Perturb_u_ED_SD_HD
!----------------------------------------------------------------------------------------------------------------------------------


!----------------------------------------------------------------------------------------------------------------------------------

!====================================================================================================
SUBROUTINE AD_InputSolve( u_AD, y_ED, MeshMapData, ErrStat, ErrMsg )
! THIS ROUTINE IS A HACK TO GET THE OUTPUTS FROM ELASTODYN INTO AERODYN. IT WILL BE REPLACED WHEN THIS CODE LINKS WITH
! AERODYN IN THE NEW FRAMEWORK
!,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

      ! Passed variables
   TYPE(AD_InputType),          INTENT(INOUT)   :: u_AD        ! The inputs to AeroDyn
   TYPE(ED_OutputType),         INTENT(IN)      :: y_ED        ! The outputs of the structural dynamics module
   TYPE(FAST_ModuleMapType),    INTENT(INOUT)   :: MeshMapData ! Data for mapping between modules
   INTEGER(IntKi)                               :: ErrStat     ! Error status of the operation
   CHARACTER(*)                                 :: ErrMsg      ! Error message if ErrStat /= ErrID_None

      ! Local variables:

   INTEGER(IntKi)                               :: J           ! Loops through nodes / elements.
   INTEGER(IntKi)                               :: K           ! Loops through blades.
   INTEGER(IntKi)                               :: NodeNum     ! Node number for blade/node on mesh
   INTEGER(IntKi)                               :: NumBl
   INTEGER(IntKi)                               :: BldNodes

   
   NumBl    = SIZE(u_AD%InputMarkers,1)
   BldNodes = u_AD%InputMarkers(1)%Nnodes
   
   !-------------------------------------------------------------------------------------------------
   ! Blade positions, orientations, and velocities:
   !-------------------------------------------------------------------------------------------------
   
   DO K = 1,NumBl !p%NumBl ! Loop through all blades
      
      !CALL Transfer_Line2_to_Line2( y_ED%BladeLn2Mesh(K), u_AD%InputMarkers(K), MeshMapData%ED_L_2_AD_L_B(K), ErrStat, ErrMsg )
      !   IF (ErrStat >= AbortErrLev ) RETURN
         
      u_AD%InputMarkers(K)%RotationVel = 0.0_ReKi ! bjj: we don't need this field
      
      DO J = 1,BldNodes !p%BldNodes ! Loop through the blade nodes / elements

         NodeNum = J         ! note that this assumes ED has same discretization as AD
         
         u_AD%InputMarkers(K)%Position(:,J)       = y_ED%BladeLn2Mesh(K)%TranslationDisp(:,NodeNum) + y_ED%BladeLn2Mesh(K)%Position(:,NodeNum) 
         u_AD%InputMarkers(K)%Orientation(:,:,J)  = y_ED%BladeLn2Mesh(K)%Orientation(:,:,NodeNum)
         u_AD%InputMarkers(K)%TranslationVel(:,J) = y_ED%BladeLn2Mesh(K)%TranslationVel(:,NodeNum)
         u_AD%InputMarkers(K)%TranslationAcc(:,J) = y_ED%BladeLn2Mesh(K)%TranslationAcc(:,NodeNum)
                  
      END DO !J = 1,p%BldNodes ! Loop through the blade nodes / elements
   END DO !K = 1,p%NumBl
   
   !-------------------------------------------------------------------------------------------------
   ! Hub positions, orientations, and velocities:
   !  (note that these may have to be adjusted in ElastoDyn as AeroDyn gets rewritten)
   !-------------------------------------------------------------------------------------------------
   u_AD%TurbineComponents%Hub%Position    = y_ED%HubPtMotion%TranslationDisp(:,1) +  y_ED%HubPtMotion%Position(:,1)
   u_AD%TurbineComponents%Hub%Orientation = y_ED%HubPtMotion%Orientation(:,:,1)   
   u_AD%TurbineComponents%Hub%RotationVel = y_ED%HubPtMotion%RotationVel(:,1)
   
   u_AD%TurbineComponents%Hub%TranslationVel = 0.0_ReKi !bjj we don't need this field
   !-------------------------------------------------------------------------------------------------
   ! Blade root orientations:
   !-------------------------------------------------------------------------------------------------
   
   DO K=1,NumBl
      u_AD%TurbineComponents%Blade(K)%Orientation = y_ED%BladeRootMotions%Orientation(:,:,K)
      
      u_AD%TurbineComponents%Blade(K)%Position       = 0.0_ReKi !bjj we don't need this field
      u_AD%TurbineComponents%Blade(K)%RotationVel    = 0.0_ReKi !bjj we don't need this field
      u_AD%TurbineComponents%Blade(K)%TranslationVel = 0.0_ReKi !bjj we don't need this field
   END DO
            

   !-------------------------------------------------------------------------------------------------
   ! RotorFurl position, orientation, rotational velocity:
   !-------------------------------------------------------------------------------------------------

   u_AD%TurbineComponents%RotorFurl%Position    = y_ED%RotorFurlMotion%TranslationDisp(:,1) + y_ED%RotorFurlMotion%Position(:,1)  
   u_AD%TurbineComponents%RotorFurl%Orientation = y_ED%RotorFurlMotion%Orientation(:,:,1)         
   u_AD%TurbineComponents%RotorFurl%RotationVel = y_ED%RotorFurlMotion%RotationVel(:,1)
   u_AD%TurbineComponents%RotorFurl%TranslationVel = 0.0_ReKi !bjj we don't need this field
   
   !-------------------------------------------------------------------------------------------------
   ! Nacelle position, orientation, rotational velocity:
   !-------------------------------------------------------------------------------------------------      

   u_AD%TurbineComponents%Nacelle%Position    = y_ED%NacelleMotion%TranslationDisp(:,1) + y_ED%NacelleMotion%Position(:,1)
   u_AD%TurbineComponents%Nacelle%Orientation = y_ED%NacelleMotion%Orientation(:,:,1)      
   u_AD%TurbineComponents%Nacelle%RotationVel = y_ED%NacelleMotion%RotationVel(:,1)  
   u_AD%TurbineComponents%Nacelle%TranslationVel = 0.0_ReKi !bjj we don't need this field
   
   !-------------------------------------------------------------------------------------------------
   ! Tower base position, rotational velocity:
   !-------------------------------------------------------------------------------------------------      
   
   
      ! Tower base position should be rT(0) instead of rZ, but AeroDyn needs this for
      ! the HubVDue2Yaw calculation:
   u_AD%TurbineComponents%Tower%Position     = y_ED%TowerMotion%TranslationDisp(:,1) + y_ED%TowerMotion%Position(:,1)
   u_AD%TurbineComponents%Tower%RotationVel  = y_ED%TowerMotion%RotationVel(:,1)
   u_AD%TurbineComponents%Tower%Orientation    = 0.0_ReKi !bjj we don't need this field
   u_AD%TurbineComponents%Tower%TranslationVel = 0.0_ReKi !bjj we don't need this field
  

   !-------------------------------------------------------------------------------------------------
   ! Tower mesh info: Twr_InputMarkers
   !-------------------------------------------------------------------------------------------------      
   
   IF ( u_AD%Twr_InputMarkers%Committed ) THEN
      
      !CALL Transfer_Line2_to_Line2( y_ED%TowerLn2Mesh, u_AD%Twr_InputMarkers, MeshMapData%ED_L_2_AD_L_T, ErrStat, ErrMsg )
      !   IF (ErrStat >= AbortErrLev ) RETURN   
      
      J = u_AD%Twr_InputMarkers%NNodes
      u_AD%Twr_InputMarkers%TranslationDisp = y_ED%TowerLn2Mesh%TranslationDisp(:,1:J)
      u_AD%Twr_InputMarkers%Orientation     = y_ED%TowerLn2Mesh%Orientation    (:,:,1:J)
      
   END IF
      
   !-------------------------------------------------------------------------------------------------
   ! If using MulTabLoc feature, set it here:
   !-------------------------------------------------------------------------------------------------      
   
   !  u_AD%MulTabLoc(IElements,IBlades) = ???
   
END SUBROUTINE AD_InputSolve
!====================================================================================================
SUBROUTINE AD_SetInitInput(InitInData_AD, InitOutData_ED, y_ED, p_FAST, ErrStat, ErrMsg)
! This subroutine sets up the information needed to initialize AeroDyn, then initializes AeroDyn
!----------------------------------------------------------------------------------------------------

   ! Passed variables:
   TYPE(AD_InitInputType),  INTENT(INOUT) :: InitInData_AD    ! The initialization input to AeroDyn
   TYPE(ED_InitOutputType), INTENT(IN)    :: InitOutData_ED   ! The initialization output from structural dynamics module
   TYPE(ED_OutputType),     INTENT(IN)    :: y_ED             ! The outputs of the structural dynamics module (meshes with position/RefOrientation set)
   TYPE(FAST_ParameterType),INTENT(IN)    :: p_FAST           ! The parameters of the glue code
   INTEGER(IntKi)                         :: ErrStat          ! Error status of the operation
   CHARACTER(*)                           :: ErrMsg           ! Error message if ErrStat /= ErrID_None

      ! Local variables

   !TYPE(AD_InitOptions)       :: ADOptions                  ! Options for AeroDyn
   INTEGER                    :: NumADBldNodes              ! Number of blade nodes in AeroDyn
   REAL(ReKi)                 :: AD_RefHt

   INTEGER                    :: K


   ErrStat = ErrID_None
   ErrMsg  = ""
   
   
      ! Set up the AeroDyn parameters
   InitInData_AD%ADFileName   = p_FAST%AeroFile
   InitInData_AD%OutRootName  = p_FAST%OutFileRoot
   InitInData_AD%WrSumFile    = p_FAST%SumPrint      
   InitInData_AD%NumBl        = InitOutData_ED%NumBl
   InitInData_AD%UseDWM       = p_FAST%UseDWM
   
      ! Hub position and orientation (relative here, but does not need to be)

   InitInData_AD%TurbineComponents%Hub%Position(:)      = y_ED%HubPtMotion%Position(:,1) - y_ED%HubPtMotion%Position(:,1)  ! bjj: was 0; mesh was changed by adding p_ED%HubHt to 3rd component
   InitInData_AD%TurbineComponents%Hub%Orientation(:,:) = y_ED%HubPtMotion%RefOrientation(:,:,1)
   InitInData_AD%TurbineComponents%Hub%TranslationVel   = 0.0_ReKi ! bjj: we don't need this field
   InitInData_AD%TurbineComponents%Hub%RotationVel      = 0.0_ReKi ! bjj: we don't need this field

      ! Blade root position and orientation (relative here, but does not need to be)

   IF (.NOT. ALLOCATED( InitInData_AD%TurbineComponents%Blade ) ) THEN
      ALLOCATE( InitInData_AD%TurbineComponents%Blade( InitInData_AD%NumBl ), STAT = ErrStat )
      IF ( ErrStat /= 0 ) THEN
         ErrStat = ErrID_Fatal
         ErrMsg = ' Error allocating space for InitInData_AD%TurbineComponents%Blade.'
         RETURN
      ELSE
         ErrStat = ErrID_None !reset to ErrID_None, just in case ErrID_None /= 0
      END IF
   END IF

   DO K=1, InitInData_AD%NumBl
      InitInData_AD%TurbineComponents%Blade(K)%Position        = y_ED%BladeRootMotions%Position(:,K)
      InitInData_AD%TurbineComponents%Blade(K)%Orientation     = y_ED%BladeRootMotions%RefOrientation(:,:,K)
      InitInData_AD%TurbineComponents%Blade(K)%TranslationVel  = 0.0_ReKi ! bjj: we don't need this field
      InitInData_AD%TurbineComponents%Blade(K)%RotationVel     = 0.0_ReKi ! bjj: we don't need this field      
   END DO
  

      ! Blade length

   InitInData_AD%TurbineComponents%BladeLength = InitOutData_ED%BladeLength
   
   
      ! Tower mesh ( here only because we currently need line2 meshes to contain the same nodes/elements )
      
   InitInData_AD%NumTwrNodes = y_ED%TowerLn2Mesh%NNodes - 2
   IF (.NOT. ALLOCATED( InitInData_AD%TwrNodeLocs ) ) THEN
      ALLOCATE( InitInData_AD%TwrNodeLocs( 3, InitInData_AD%NumTwrNodes ), STAT = ErrStat )
      IF ( ErrStat /= 0 ) THEN
         ErrStat = ErrID_Fatal
         ErrMsg = ' Error allocating space for InitInData_AD%TwrNodeLocs.'
         RETURN
      ELSE
         ErrStat = ErrID_None
      END IF
   END IF   
   
   IF ( InitInData_AD%NumTwrNodes > 0 ) THEN
      InitInData_AD%TwrNodeLocs = y_ED%TowerLn2Mesh%Position(:,1:InitInData_AD%NumTwrNodes)  ! ED has extra nodes at beginning and top and bottom of tower
   END IF
   
   
      ! Initialize AeroDyn


   !
   !   ! Check that the hub-heights are Set up other parameters only if we need them
   !
   !IF ( p_FAST%CompAero == Module_AD )  THEN
   !
   !   ! Let's see if the hub-height in AeroDyn and FAST are within 10%:
   !   AD_RefHt = AD_GetConstant('RefHt', ErrStat)
   !
   !   IF ( ABS( p_ED%HubHt - AD_RefHt ) > 0.1*( p_ED%HubHt ) )  THEN  !bjj: I believe that this should not be done in the future
   !
   !      CALL ProgWarn( ' The ElastoDyn hub height ('//TRIM(Num2LStr( p_ED%HubHt ))//') and AeroDyn input'// &
   !                    ' reference hub height ('//TRIM(Num2LStr(AD_RefHt))//') differ by more than 10%.' )
   !   ENDIF
   !
   !ENDIF


   RETURN
END SUBROUTINE AD_SetInitInput
!...............................................................................................................................
SUBROUTINE WriteInputMeshesToFile(u_ED, u_SD, u_HD, u_MAP, u_AD, FileName, ErrStat, ErrMsg) 
   TYPE(ED_InputType),        INTENT(IN)  :: u_ED         
   TYPE(SD_InputType),        INTENT(IN)  :: u_SD         
   TYPE(HydroDyn_InputType),  INTENT(IN)  :: u_HD         
   TYPE(MAP_InputType),       INTENT(IN)  :: u_MAP         
   TYPE(AD_InputType),        INTENT(IN)  :: u_AD         
   CHARACTER(*),              INTENT(IN) :: FileName
   
   INTEGER(IntKi)                         :: ErrStat          ! Error status of the operation
   CHARACTER(*)                           :: ErrMsg           ! Error message if ErrStat /= ErrID_None
   
   
      !FileName = TRIM(p_FAST%OutFileRoot)//'.InputMeshes.bin'
   
      
   INTEGER(IntKi)           :: unOut
   INTEGER(IntKi)           :: K_local
   INTEGER(B4Ki), PARAMETER :: File_ID = 1
   INTEGER(B4Ki)            :: NumBl
      

      ! Open the binary output file:
   unOut=-1      
   CALL GetNewUnit( unOut, ErrStat, ErrMsg )
   CALL OpenBOutFile ( unOut, TRIM(FileName), ErrStat, ErrMsg )
      IF (ErrStat /= ErrID_None) RETURN
               
      
   ! note that I'm not doing anything with the errors here, so it won't tell
   ! you there was a problem writing the data unless it was the last call.
          
      ! Add a file identification number (in case we ever have to change this):
   WRITE( unOut, IOSTAT=ErrStat )   File_ID
   

      ! Add how many blade meshes there are:
   NumBl =  SIZE(u_ED%BladeLn2Mesh,1)   ! Note that NumBl is B4Ki 
   WRITE( unOut, IOSTAT=ErrStat )   NumBl
      
      ! Add all of the input meshes:
   DO K_local = 1,SIZE(u_ED%BladeLn2Mesh,1)
      CALL MeshWrBin( unOut, u_ED%BladeLn2Mesh(K_local), ErrStat, ErrMsg )
   END DO            
   CALL MeshWrBin( unOut, u_ED%TowerLn2Mesh,            ErrStat, ErrMsg )
   CALL MeshWrBin( unOut, u_ED%PlatformPtMesh,          ErrStat, ErrMsg )
   CALL MeshWrBin( unOut, u_SD%TPMesh,                  ErrStat, ErrMsg )
   CALL MeshWrBin( unOut, u_SD%LMesh,                   ErrStat, ErrMsg )
   CALL MeshWrBin( unOut, u_HD%Morison%distribMesh,     ErrStat, ErrMsg )
   CALL MeshWrBin( unOut, u_HD%Morison%lumpedMesh,      ErrStat, ErrMsg )
   CALL MeshWrBin( unOut, u_HD%Mesh,                    ErrStat, ErrMsg )
   CALL MeshWrBin( unOut, u_MAP%PtFairleadDisplacement, ErrStat, ErrMsg )
      
      
      ! Close the file
   CLOSE(unOut)
         
END SUBROUTINE WriteInputMeshesToFile   
!...............................................................................................................................
SUBROUTINE WriteMotionMeshesToFile(time, y_ED, u_SD, y_SD, u_HD, u_MAP, UnOut, ErrStat, ErrMsg, FileName) 
   REAL(DbKi),                 INTENT(IN)    :: time
   TYPE(ED_OutputType),        INTENT(IN)    :: y_ED         
   TYPE(SD_InputType),         INTENT(IN)    :: u_SD         
   TYPE(SD_OutputType),        INTENT(IN)    :: y_SD         
   TYPE(HydroDyn_InputType),   INTENT(IN)    :: u_HD         
   TYPE(MAP_InputType),        INTENT(IN)    :: u_MAP         
   INTEGER(IntKi) ,            INTENT(INOUT) :: unOut
   CHARACTER(*),   OPTIONAL,   INTENT(IN)    :: FileName
   
   INTEGER(IntKi)                            :: ErrStat          ! Error status of the operation
   CHARACTER(*)                              :: ErrMsg           ! Error message if ErrStat /= ErrID_None
   
   
      !FileName = TRIM(p_FAST%OutFileRoot)//'.InputMeshes.bin'
   REAL(R8Ki)               :: t
      
   INTEGER(IntKi)           :: K_local
   INTEGER(B4Ki), PARAMETER :: File_ID = 100
   INTEGER(B4Ki)            :: NumBl
      
   t = time  ! convert to 8-bytes if necessary (DbKi might not be R8Ki)
   
   ! note that I'm not doing anything with the errors here, so it won't tell
   ! you there was a problem writing the data unless it was the last call.
   
   
      ! Open the binary output file and write a header:
   if (unOut<0) then
      CALL GetNewUnit( unOut, ErrStat, ErrMsg )
      
      CALL OpenBOutFile ( unOut, TRIM(FileName), ErrStat, ErrMsg )
         IF (ErrStat /= ErrID_None) RETURN
               
         ! Add a file identification number (in case we ever have to change this):
      WRITE( unOut, IOSTAT=ErrStat )   File_ID
      
         ! Add how many blade meshes there are:
      NumBl =  SIZE(y_ED%BladeLn2Mesh,1)   ! Note that NumBl is B4Ki 
      WRITE( unOut, IOSTAT=ErrStat )   NumBl
   end if
   
   WRITE( unOut, IOSTAT=ErrStat ) t          
   
      ! Add all of the meshes with motions:
   DO K_local = 1,SIZE(y_ED%BladeLn2Mesh,1)
      CALL MeshWrBin( unOut, y_ED%BladeLn2Mesh(K_local), ErrStat, ErrMsg )
   END DO            
   CALL MeshWrBin( unOut, y_ED%TowerLn2Mesh,            ErrStat, ErrMsg )
   CALL MeshWrBin( unOut, y_ED%PlatformPtMesh,          ErrStat, ErrMsg )
   CALL MeshWrBin( unOut, u_SD%TPMesh,                  ErrStat, ErrMsg )
   CALL MeshWrBin( unOut, y_SD%y2Mesh,                  ErrStat, ErrMsg )
   CALL MeshWrBin( unOut, u_HD%Morison%distribMesh,     ErrStat, ErrMsg )
   CALL MeshWrBin( unOut, u_HD%Morison%lumpedMesh,      ErrStat, ErrMsg )
   CALL MeshWrBin( unOut, u_HD%Mesh,                    ErrStat, ErrMsg )
   CALL MeshWrBin( unOut, u_MAP%PtFairleadDisplacement, ErrStat, ErrMsg )
      
   !   
   !   ! Close the file
   !CLOSE(unOut)
   !      
END SUBROUTINE WriteMotionMeshesToFile   
!...............................................................................................................................
SUBROUTINE WriteMappingTransferToFile(Mesh1_I,Mesh1_O,Mesh2_I,Mesh2_O,Map_Mod1_Mod2,Map_Mod2_Mod1,BinOutputName)
! this routine is used for debugging mesh mapping
!...............................................................................................................................

   TYPE(meshtype),    intent(in) :: mesh1_I
   TYPE(meshtype),    intent(in) :: mesh1_O
   TYPE(meshtype),    intent(in) :: mesh2_I
   TYPE(meshtype),    intent(in) :: mesh2_O 
   
   TYPE(MeshMapType), intent(in) :: Map_Mod1_Mod2        ! Data for mapping meshes from mod1 to mod2
   TYPE(MeshMapType), intent(in) :: Map_Mod2_Mod1        ! Data for mapping meshes from mod1 to mod2

   CHARACTER(*),      INTENT(IN) :: BinOutputName
   
   
   ! local variables:
   TYPE(meshtype)                         :: mesh_Motion_1PT, mesh1_I_1PT, mesh2_O_1PT
   TYPE(MeshMapType)                      :: Map_Mod2_O_1PT, Map_Mod1_I_1PT
      
   INTEGER(IntKi)                         :: i
   INTEGER(IntKi)                         :: un_out
   INTEGER(IntKi)                         :: ErrStat          ! Error status of the operation
   CHARACTER(1024)                        :: ErrMsg           ! Error message if ErrStat /= ErrID_None
   CHARACTER(256)                         :: PrintWarnF, PrintWarnM, TmpValues

   !------------------------------------------------------------------------
   ! Make sure the meshes are committed before checking them:
   !------------------------------------------------------------------------
   
   IF (.NOT. mesh1_I%Committed .OR. .NOT. mesh1_O%Committed ) RETURN
   IF (.NOT. mesh2_I%Committed .OR. .NOT. mesh2_O%Committed ) RETURN
      
   !------------------------------------------------------------------------
   !lump the loads to one point and compare:
   !------------------------------------------------------------------------       
   
   ! create one loads mesh with one point:
   CALL MeshCreate( BlankMesh       = mesh1_I_1PT        &
                  , IOS              = COMPONENT_INPUT   &
                  , NNodes           = 1                 &
                  , Force            = .TRUE.            &
                  , Moment           = .TRUE.            &
                  , ErrStat          = ErrStat           &
                  , ErrMess          = ErrMsg            )
      
   IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
         
         
   CALL MeshPositionNode ( mesh1_I_1PT, 1, (/0.0_ReKi, 0.0_ReKi, 0.0_ReKi/), ErrStat, ErrMsg ) ; IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))             
   CALL MeshConstructElement ( mesh1_I_1PT, ELEMENT_POINT, ErrStat, ErrMsg, 1 );                 IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))                                       
   CALL MeshCommit ( mesh1_I_1PT, ErrStat, ErrMsg )                                       ;      IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg)) 
      
   !.....         
   ! create a corresponding motion mesh with one point:
   
   CALL MeshCopy( mesh1_I_1PT, mesh_Motion_1PT, MESH_SIBLING, ErrStat, ErrMsg &
                  , IOS              = COMPONENT_OUTPUT  &
                  , TranslationDisp  = .TRUE.            ) ;                                     IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))                     
   !.....         
   ! create a second loads mesh with one point:
   CALL MeshCopy( mesh1_I_1PT, mesh2_O_1PT, MESH_NEWCOPY, ErrStat, ErrMsg )  ! This thinks it's for input, but really it's for output. I don't think it matters...       
       
   !.....         
   ! create the mapping data structures:       
   CALL MeshMapCreate( Mesh1_I, Mesh1_I_1PT, Map_Mod1_I_1PT,  ErrStat, ErrMsg );                 IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   CALL MeshMapCreate( Mesh2_O, Mesh2_O_1PT, Map_Mod2_O_1PT,  ErrStat, ErrMsg );                 IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))

   !.....         
   ! transfer MESH1_I (loads) to single point:
   
   IF ( mesh1_I%ElemTable(ELEMENT_POINT)%nelem > 0 ) THEN
      CALL Transfer_Point_to_Point( Mesh1_I, Mesh1_I_1PT, Map_Mod1_I_1PT,ErrStat,ErrMsg,mesh1_O,mesh_Motion_1PT);  IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))         
   ELSEIF ( mesh1_I%ElemTable(ELEMENT_LINE2)%nelem > 0 ) THEN
      CALL Transfer_Line2_to_Point( Mesh1_I, Mesh1_I_1PT, Map_Mod1_I_1PT,ErrStat,ErrMsg,mesh1_O,mesh_Motion_1PT);  IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))          
   END IF
   
   !.....         
   ! transfer Mesh2_O (loads) to single point:      
   IF ( Mesh2_O%ElemTable(ELEMENT_POINT)%nelem > 0 ) THEN
      CALL Transfer_Point_to_Point( Mesh2_O, Mesh2_O_1PT, Map_Mod2_O_1PT,ErrStat,ErrMsg,mesh2_I,mesh_Motion_1PT);  IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg)) 
   ELSEIF ( Mesh2_O%ElemTable(ELEMENT_LINE2)%nelem > 0 ) THEN 
      CALL Transfer_Line2_to_Point( Mesh2_O, Mesh2_O_1PT, Map_Mod2_O_1PT,ErrStat,ErrMsg,mesh2_I,mesh_Motion_1PT);  IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))          
   END IF
   !............
            
   ! dsisplay a warning if the point loads are not equal:         
   PrintWarnF=""
   PrintWarnM=""
   do i=1,3
      if (.NOT. equalrealnos(mesh1_I_1PT%Force( i,1),mesh2_O_1PT%Force( i,1)) ) PrintWarnF=NewLine//"  <----------- WARNING: Forces are not equal ----------->  "//NewLine//NewLine
      if (.NOT. equalrealnos(mesh1_I_1PT%Moment(i,1),mesh2_O_1PT%Moment(i,1)) ) PrintWarnM=NewLine//"  <----------- WARNING: Moments are not equal ----------->  "//NewLine//NewLine
   end do

   
   call wrscr(TRIM(PrintWarnF)//'Total Force:' )
   write(TmpValues,*) mesh1_I_1PT%Force;   call wrscr('     Mesh 1: '//TRIM(TmpValues))
   write(TmpValues,*) mesh2_O_1PT%Force;   call wrscr('     Mesh 2: '//TRIM(TmpValues))
   call wrscr(TRIM(PrintWarnM)//'Total Moment:' )
   write(TmpValues,*) mesh1_I_1PT%Moment;  call wrscr('     Mesh 1: '//TRIM(TmpValues))
   write(TmpValues,*) mesh2_O_1PT%Moment;  call wrscr('     Mesh 2: '//TRIM(TmpValues))
   !............
   
   !------------------------------------------------------------------------
   ! now we'll write all the mesh info to a file for debugging:   
   !------------------------------------------------------------------------

   un_out = -1
   CALL MeshWrBin ( un_out, Mesh1_I,         ErrStat, ErrMsg, BinOutputName);  IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   CALL MeshWrBin ( un_out, Mesh1_O,         ErrStat, ErrMsg, BinOutputName);  IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   CALL MeshWrBin ( un_out, Mesh2_I,         ErrStat, ErrMsg, BinOutputName);  IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   CALL MeshWrBin ( un_out, Mesh2_O,         ErrStat, ErrMsg, BinOutputName);  IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
      
   CALL MeshWrBin ( un_out, mesh1_I_1PT,     ErrStat, ErrMsg, BinOutputName);  IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   CALL MeshWrBin ( un_out, mesh2_O_1PT,     ErrStat, ErrMsg, BinOutputName);  IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))         
   CALL MeshWrBin ( un_out, mesh_Motion_1PT, ErrStat, ErrMsg, BinOutputName);  IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))   
      
   CALL MeshMapWrBin( un_out, Mesh1_O, Mesh2_I, Map_Mod1_Mod2, ErrStat, ErrMsg, BinOutputName );  IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg)) 
   CALL MeshMapWrBin( un_out, Mesh2_O, Mesh1_I, Map_Mod2_Mod1, ErrStat, ErrMsg, BinOutputName );  IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg)) 
      
   CALL MeshMapWrBin( un_out, Mesh1_I, Mesh1_I_1PT, Map_Mod1_I_1PT, ErrStat, ErrMsg, BinOutputName );  IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg)) 
   CALL MeshMapWrBin( un_out, Mesh2_O, Mesh2_O_1PT, Map_Mod2_O_1PT, ErrStat, ErrMsg, BinOutputName );  IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg)) 

   close( un_out )
   
   !------------------------------------------------------------------------
   ! destroy local copies:
   !------------------------------------------------------------------------
   
   CALL MeshDestroy( mesh_Motion_1PT, ErrStat, ErrMsg ); IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   CALL MeshDestroy( mesh1_I_1PT, ErrStat, ErrMsg );     IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   CALL MeshDestroy( mesh2_O_1PT, ErrStat, ErrMsg );     IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   
   call MeshMapDestroy(Map_Mod1_I_1PT, ErrStat, ErrMsg);IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   call MeshMapDestroy(Map_Mod2_O_1PT, ErrStat, ErrMsg);IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   
   

END SUBROUTINE WriteMappingTransferToFile 

END MODULE FAST_IO_Subs
