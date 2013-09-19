!**********************************************************************************************************************************
! The FAST_Prog.f90, FAST_IO.f90, and FAST_Mods.f90 make up the FAST glue code in the FAST Modularization Framework.
!..................................................................................................................................
! LICENSING
! Copyright (C) 2013  National Renewable Energy Laboratory
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
MODULE FAST_IO_Subs

   USE NWTC_Library

   USE FAST_Types
   USE ElastoDyn_Types
   USE ServoDyn_Types
   USE HydroDyn_Types
   USE SubDyn_Types
   USE MAP_Types


   USE AeroDyn
   USE AeroDyn_Types

!   USE InflowWind, ONLY: WindInfVer
   
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
   IF (y_FAST%UnOu  > 0) CLOSE( y_FAST%UnOu )         ! I/O unit number for the tabular output file
   IF (y_FAST%UnSum > 0) CLOSE( y_FAST%UnSum )        ! I/O unit number for the summary file

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

      ! Display the copyright notice
   CALL DispCopyrightLicense( FAST_Ver )


      ! Tell our nice users what they're running
   CALL WrScr( ' Running '//GetVersion()//NewLine//' linked with '//TRIM( GetNVD( NWTC_Ver ))//NewLine )

   !...............................................................................................................................
   ! Get the name of the input file from the command line if it isn't an input to this routine
   ! and set the root name of the output files based on the input file name
   !...............................................................................................................................

   IF ( PRESENT(InFile) ) THEN
      InputFile = InFile
   ELSE ! get it from the command line
      InputFile = ""  ! initialize to empty string to make sure it's input from the command line
      CALL CheckArgs( InputFile, Stat )  ! if Stat /= ErrID_None, we'll ignore and deal with the problem when we try to read the input file
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
   ! Read the primary file for the glue code:
   !...............................................................................................................................
   CALL FAST_ReadPrimaryFile( InputFile, p, ErrStat, ErrMsg )
   IF ( ErrStat >= AbortErrLev ) RETURN


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

   IF ( p%TStart   <  0.0_DbKi ) CALL SetErrors( ErrID_Fatal, 'TStart must not be less than 0 seconds.' )
   IF ( p%SttsTime <= 0.0_DbKi ) CALL SetErrors( ErrID_Fatal, 'SttsTime must be greater than 0 seconds.' )


   IF ( p%InterpOrder < 0 .OR. p%InterpOrder > 2 ) THEN
      CALL SetErrors( ErrID_Fatal, 'InterpOrder must not be 0, 1, or 2.' )
      p%InterpOrder = 0    ! Avoid problems in error handling by setting this to 0
   END IF

   IF ( p%NumCrctn < 0_IntKi ) THEN
      CALL SetErrors( ErrID_Fatal, 'NumCrctn must be 1 or greater.' )
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
                            InitOutData_SD, InitOutData_MAP, ErrStat, ErrMsg )
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

   INTEGER(IntKi),                 INTENT(OUT)          :: ErrStat                               ! Error status
   CHARACTER(*),                   INTENT(OUT)          :: ErrMsg                                ! Error message corresponding to ErrStat


      ! Local variables.

   INTEGER(IntKi)                   :: I                                               ! A generic index for DO loops.
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

   y_FAST%ED_Ver   = InitOutData_ED%Ver
   y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%ED_Ver  ))

   IF ( p_FAST%CompAero )  THEN
      y_FAST%IfW_Ver  = InitOutData_AD%IfW_InitOutput%Ver
      y_FAST%AD_Ver   = InitOutData_AD%Ver
     
      y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%IfW_Ver)) 
      y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%AD_Ver ))      
   ELSE
      y_FAST%AD_Ver%Name = 'AeroDyn'
      y_FAST%AD_Ver%Date = 'unknown date'
      y_FAST%AD_Ver%Ver  = 'unknown version'
      
      y_FAST%IfW_Ver%Name = 'InflowWind'
      y_FAST%IfW_Ver%Date = 'unknown date'
      y_FAST%IfW_Ver%Ver  = 'unknown version'
            
   END IF

   IF ( p_FAST%CompServo ) THEN
      y_FAST%SrvD_Ver = InitOutData_SrvD%Ver
      y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%SrvD_Ver))
   ELSE
      y_FAST%SrvD_Ver%Name = 'ServoDyn'
      y_FAST%SrvD_Ver%Date = 'unknown date'
      y_FAST%SrvD_Ver%Ver  = 'unknown version'
   END IF
         
   IF ( p_FAST%CompHydro ) THEN
      y_FAST%HD_Ver   = InitOutData_HD%Ver
      y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%HD_Ver))
   ELSE
      y_FAST%HD_Ver%Name = 'HydroDyn'
      y_FAST%HD_Ver%Date = 'unknown date'
      y_FAST%HD_Ver%Ver  = 'unknown version'
   END IF

   IF ( p_FAST%CompSub ) THEN
   !   y_FAST%SD_Ver   = InitOutData_SD%Ver
   !   y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%SD_Ver))
   ELSE
      y_FAST%SD_Ver%Name = 'SubDyn'
      y_FAST%SD_Ver%Date = 'unknown date'
      y_FAST%SD_Ver%Ver  = 'unknown version'
   END IF

   IF ( p_FAST%CompMAP ) THEN
      y_FAST%MAP_Ver   = InitOutData_MAP%Ver
      y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) //'; '//TRIM(GetNVD(y_FAST%MAP_Ver))
   ELSE
      y_FAST%MAP_Ver%Name = 'MAP'
      y_FAST%MAP_Ver%Date = 'unknown date'
      y_FAST%MAP_Ver%Ver  = 'unknown version'
   END IF   
   
            
   !......................................................
   ! Set the number of output columns from each module
   !......................................................

   y_FAST%numOuts_AD   = 0
   
   !y_FAST%numOuts_IfW  = 3  !hack for now: always output 3 wind speeds at hub-height
   y_FAST%numOuts_IfW  = 0 
   IF ( ALLOCATED( InitOutData_AD%IfW_InitOutput%WriteOutputHdr ) ) &
                                                       y_FAST%numOuts_IfW  = SIZE(InitOutData_AD%IfW_InitOutput%WriteOutputHdr)
   
   y_FAST%numOuts_ED   = 0
   IF ( ALLOCATED( InitOutData_ED%WriteOutputHdr   ) ) y_FAST%numOuts_ED   = SIZE(InitOutData_ED%WriteOutputHdr)

   y_FAST%numOuts_SrvD = 0
   IF ( ALLOCATED( InitOutData_SrvD%WriteOutputHdr ) ) y_FAST%numOuts_SrvD = SIZE(InitOutData_SrvD%WriteOutputHdr)

   y_FAST%numOuts_HD = 0
   IF ( ALLOCATED( InitOutData_HD%WriteOutputHdr   ) ) y_FAST%numOuts_HD   = SIZE(InitOutData_HD%WriteOutputHdr)
   
   y_FAST%numOuts_SD = 0
   IF ( ALLOCATED( InitOutData_SD%WriteOutputHdr   ) ) y_FAST%numOuts_SD   = SIZE(InitOutData_SD%WriteOutputHdr)
   
   y_FAST%numOuts_MAP = 0
   IF ( ALLOCATED( InitOutData_MAP%WriteOutputHdr  ) ) y_FAST%numOuts_MAP  = SIZE(InitOutData_MAP%WriteOutputHdr)
   
   
   !......................................................
   ! Initialize the output channel names and units
   !......................................................
   NumOuts   = 1 + y_FAST%numOuts_IfW + y_FAST%numOuts_ED + y_FAST%numOuts_SrvD + y_FAST%numOuts_AD + y_FAST%numOuts_HD &
                 + y_FAST%numOuts_SD  + y_FAST%numOuts_MAP

   CALL AllocAry( y_FAST%ChannelNames,NumOuts, 'ChannelNames', ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN
   CALL AllocAry( y_FAST%ChannelUnits,NumOuts, 'ChannelUnits', ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN

   y_FAST%ChannelNames(1) = 'Time'
   y_FAST%ChannelUnits(1) = '(s)'

   indxLast = 1
   indxNext = 2

   IF ( y_FAST%numOuts_IfW > 0_IntKi ) THEN  !InflowWind: hack for now
      indxLast = indxNext + y_FAST%numOuts_IfW - 1
      y_FAST%ChannelNames(indxNext:indxLast) = InitOutData_AD%IfW_InitOutput%WriteOutputHdr
      y_FAST%ChannelUnits(indxNext:indxLast) = InitOutData_AD%IfW_InitOutput%WriteOutputUnt
      
      !y_FAST%ChannelNames(indxNext:indxLast) = (/ 'WindVxi   ', 'WindVyi   ', 'WindVzi   ' /)
      !IF ( p_FAST%CompAero ) THEN
      !   y_FAST%ChannelUnits(indxNext:indxLast) = (/ '(m/s)     ', '(m/s)     ', '(m/s)     ' /)
      !ELSE
      !   y_FAST%ChannelUnits(indxNext:indxLast) = (/ 'INVALID   ', 'INVALID   ', 'INVALID   ' /)
      !END IF
      indxNext = indxLast + 1
   END IF


   IF ( y_FAST%numOuts_ED > 0_IntKi ) THEN !ElasoDyn
      indxLast = indxNext + y_FAST%numOuts_ED - 1
      y_FAST%ChannelNames(indxNext:indxLast) = InitOutData_ED%WriteOutputHdr
      y_FAST%ChannelUnits(indxNext:indxLast) = InitOutData_ED%WriteOutputUnt
      indxNext = indxLast + 1
   END IF

   IF ( y_FAST%numOuts_SrvD > 0_IntKi ) THEN !ServoDyn
      indxLast = indxNext + y_FAST%numOuts_SrvD - 1
      y_FAST%ChannelNames(indxNext:indxLast) = InitOutData_SrvD%WriteOutputHdr
      y_FAST%ChannelUnits(indxNext:indxLast) = InitOutData_SrvD%WriteOutputUnt
      indxNext = indxLast + 1
   END IF

   IF ( y_FAST%numOuts_HD > 0_IntKi ) THEN !HydroDyn
      indxLast = indxNext + y_FAST%numOuts_HD - 1
      y_FAST%ChannelNames(indxNext:indxLast) = InitOutData_HD%WriteOutputHdr
      y_FAST%ChannelUnits(indxNext:indxLast) = InitOutData_HD%WriteOutputUnt
      indxNext = indxLast + 1
   END IF

   
   IF ( y_FAST%numOuts_SD > 0_IntKi ) THEN !SubDyn
      indxLast = indxNext + y_FAST%numOuts_SD - 1
      y_FAST%ChannelNames(indxNext:indxLast) = InitOutData_SD%WriteOutputHdr
      y_FAST%ChannelUnits(indxNext:indxLast) = InitOutData_SD%WriteOutputUnt
      indxNext = indxLast + 1
   END IF

   
   IF ( y_FAST%numOuts_MAP > 0_IntKi ) THEN !MAP
      indxLast = indxNext + y_FAST%numOuts_MAP - 1
      y_FAST%ChannelNames(indxNext:indxLast) = InitOutData_MAP%WriteOutputHdr
      y_FAST%ChannelUnits(indxNext:indxLast) = InitOutData_MAP%WriteOutputUnt
      indxNext = indxLast + 1
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

         y_FAST%TimeData(1) = 0                  ! This is the first output time, which we will set later
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
SUBROUTINE FAST_WrSum( p_FAST, y_FAST, ErrStat, ErrMsg )
! This subroutine opens and writes data to the FAST summary file. The file gets closed on the end of program execution.
!..................................................................................................................................

   TYPE(FAST_ParameterType), INTENT(IN)    :: p_FAST                             ! Glue-code simulation parameters
   TYPE(FAST_OutputType),    INTENT(INOUT) :: y_FAST                             ! Glue-code simulation outputs
   INTEGER(IntKi),           INTENT(OUT)   :: ErrStat                            ! Error status (level)
   CHARACTER(*),             INTENT(OUT)   :: ErrMsg                             ! Message describing error reported in ErrStat

      ! local variables
   INTEGER(IntKi)                          :: I                                  ! temporary counter
   INTEGER(IntKi)                          :: J                                  ! temporary counter
   CHARACTER(200)                          :: Fmt                                ! temporary format string


      ! Get a unit number and open the file:

   CALL GetNewUnit( y_FAST%UnSum, ErrStat, ErrMsg )
      IF ( ErrStat >= AbortErrLev ) RETURN

   CALL OpenFOutFile ( y_FAST%UnSum, TRIM(p_FAST%OutFileRoot)//'.sum', ErrStat, ErrMsg )
      IF ( ErrStat >= AbortErrLev ) RETURN

         ! Add some file information:

   !.......................... Module Versions .....................................................
   WRITE (y_FAST%UnSum,'(/A)') 'FAST Summary File'
   WRITE (y_FAST%UnSum,'(/A)')  TRIM( y_FAST%FileDescLines(1) )

   WRITE (y_FAST%UnSum,'(2X,A)'   )  'compiled with'
   Fmt = '(4x,A)'
   WRITE (y_FAST%UnSum,Fmt)  TRIM( GetNVD(        NWTC_Ver ) )
   WRITE (y_FAST%UnSum,Fmt)  TRIM( GetNVD( y_FAST%ED_Ver   ) )
   WRITE (y_FAST%UnSum,Fmt)  TRIM( GetNVD( y_FAST%SrvD_Ver ) )
   WRITE (y_FAST%UnSum,Fmt)  TRIM( GetNVD( y_FAST%AD_Ver   ) )
   WRITE (y_FAST%UnSum,Fmt)  TRIM( GetNVD( y_FAST%IfW_Ver  ) )
   WRITE (y_FAST%UnSum,Fmt)  TRIM( GetNVD( y_FAST%HD_Ver   ) )
   WRITE (y_FAST%UnSum,Fmt)  TRIM( GetNVD( y_FAST%SD_Ver   ) )
   WRITE (y_FAST%UnSum,Fmt)  TRIM( GetNVD( y_FAST%MAP_Ver  ) )

   !.......................... Information from FAST input File ......................................
! current working directory
! output file root name
! output file time step
! output file format (text/binary)
! which modules are being used (feature flags)
! coupling method

   WRITE (y_FAST%UnSum,'(//A)' )   'Description from the FAST input file: '
   WRITE (y_FAST%UnSum,'(2X,A)')  TRIM(p_FAST%FTitle)

   !.......................... Requested Features ...................................................

   !BJJ: write "nearest neighbor, linear, quadratic" instead of order here (or print both)
   WRITE(y_FAST%UnSum,'(//A,I1)') 'Interpolation order for input/output time histories: ', p_FAST%InterpOrder
   WRITE(y_FAST%UnSum,'(  A,I2)') 'Number of correction iterations: ', p_FAST%NumCrctn

   !.......................... Requested Output Channels ............................................

   WRITE (y_FAST%UnSum,'(//,2X,A)') " Requested Channels in FAST Output File(s) "
   WRITE (y_FAST%UnSum,   '(2X,A)') "-------------------------------------------"
   Fmt = '(2X,A6,2(2X,A'//trim(num2lstr(ChanLen))//'),2X,A)'
   WRITE (y_FAST%UnSum, Fmt ) "Number", "Name      ", "Units     ", "Generated by"
   WRITE (y_FAST%UnSum, Fmt ) "------", "----------", "----------", "------------"

   Fmt = '(4X,I4,2(2X,A'//trim(num2lstr(ChanLen))//'),2X,A)'
   I = 1
   WRITE (y_FAST%UnSum, Fmt ) I, y_FAST%ChannelNames(I), y_FAST%ChannelUnits(I), TRIM(FAST_Ver%Name)

      ! InflowWind
   DO J = 1,y_FAST%numOuts_IfW
      I = I + 1
      WRITE (y_FAST%UnSum, Fmt ) I, y_FAST%ChannelNames(I), y_FAST%ChannelUnits(I), TRIM(y_FAST%IfW_Ver%Name)
   END DO

      ! ElastoDyn
   DO J = 1,y_FAST%numOuts_ED
      I = I + 1
      WRITE (y_FAST%UnSum, Fmt ) I, y_FAST%ChannelNames(I), y_FAST%ChannelUnits(I), TRIM(y_FAST%ED_Ver%Name)
   END DO

      ! ServoDyn
   DO J = 1,y_FAST%numOuts_SrvD
      I = I + 1
      WRITE (y_FAST%UnSum, Fmt ) I, y_FAST%ChannelNames(I), y_FAST%ChannelUnits(I), TRIM(y_FAST%SrvD_Ver%Name)
   END DO

      ! HydroDyn
   DO J = 1,y_FAST%numOuts_HD
      I = I + 1
      WRITE (y_FAST%UnSum, Fmt ) I, y_FAST%ChannelNames(I), y_FAST%ChannelUnits(I), TRIM(y_FAST%HD_Ver%Name)
   END DO

      ! SubDyn
   DO J = 1,y_FAST%numOuts_SD
      I = I + 1
      WRITE (y_FAST%UnSum, Fmt ) I, y_FAST%ChannelNames(I), y_FAST%ChannelUnits(I), TRIM(y_FAST%SD_Ver%Name)
   END DO

      ! MAP (Mooring Analysis Program)
   DO J = 1,y_FAST%numOuts_MAP
      I = I + 1
!      WRITE (y_FAST%UnSum, Fmt ) I, y_FAST%ChannelNames(I), 'units', TRIM(y_FAST%MAP_Ver%Name)
      WRITE (y_FAST%UnSum, Fmt ) I, y_FAST%ChannelNames(I), y_FAST%ChannelUnits(I), TRIM(y_FAST%MAP_Ver%Name)
   END DO
      
   

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

      ! CompMAP - Compute mooring line dynamics (flag):
   CALL ReadVar( UnIn, InputFile, p%CompMAP, "CompMAP", "Compute mooring line dynamics (flag)", ErrStat2, ErrMsg2, UnEc)
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

      ! MAPFile - Name of file containing MAP input parameters (-):
   CALL ReadVar( UnIn, InputFile, p%MAPFile, "MAPFile", "Name of file containing MAP input parameters (-)", ErrStat2, ErrMsg2, UnEc)
      CALL CheckError( ErrStat2, ErrMsg2 )
      IF ( ErrStat >= AbortErrLev ) RETURN
   IF ( PathIsRelative( p%MAPFile ) ) p%MAPFile = TRIM(PriPath)//TRIM(p%MAPFile)
         
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

         IF ( LEN_TRIM(ErrMsg) > 0 ) ErrMsg = TRIM(ErrMsg)//NewLine
         ErrMsg = TRIM(ErrMsg)//'Error in FAST_ReadPrimaryFile: '//TRIM(Msg)
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
SUBROUTINE RunTimes( StrtTime, UsrTime1, ZTime )
! This routine displays a message that gives that status of the simulation and the predicted end time of day.
!..................................................................................................................................

   IMPLICIT                        NONE

      ! Passed variables

   INTEGER                      :: StrtTime (8)                                    ! Start time of simulation
   REAL                         :: UsrTime1                                        ! User CPU time for simulation initialization.
   REAL(DbKi)                   :: ZTime                                           ! The final simulation time (not necessarially TMax)

      ! Local variables

   REAL                         :: ClckTime                                        ! Elapsed clock time for the simulation phase of the run.
   REAL                         :: Factor                                          ! Ratio of seconds to a specified time period.
   REAL                         :: TRatio                                          ! Ration of simulation time to elapsed clock time.
   REAL(ReKi), PARAMETER        :: SecPerDay = 24*60*60.0_ReKi                     ! Number of seconds per day

   REAL                         :: UsrTime                                         ! User CPU time for entire run.
   INTEGER                      :: EndTimes (8)                                    ! An array holding the ending clock time of the simulation.

   CHARACTER( 8)                :: TimePer
   CHARACTER(MaxWrScrLen)       :: BlankLine

      ! Get the end times to compare with start times.

   CALL DATE_AND_TIME ( VALUES=EndTimes )
   CALL CPU_TIME ( UsrTime )


   ! Calculate the elapsed wall-clock time in seconds.

!bjj: I think this calculation will be wrong at certain times (e.g. if it's near midnight on the last day of the month), but to my knowledge, no one has complained...
   ClckTime =  0.001*( EndTimes(8) - StrtTime(8) ) + ( EndTimes(7) - StrtTime(7) ) + 60.0*( EndTimes(6) - StrtTime(6) ) &
            + 3600.0*( EndTimes(5) - StrtTime(5) ) + SecPerDay*( EndTimes(3) - StrtTime(3) )


      ! Calculate CPU times.

   UsrTime  = UsrTime - UsrTime1


   IF ( .NOT. EqualRealNos( UsrTime, 0.0 ) )  THEN

      TRatio = ZTime / UsrTime

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
      CALL WrScr ( ' Simulated Time:        '//TRIM( Num2LStr( Factor*REAL( ZTime ) ) )//TRIM( TimePer ) )
      CALL WrScr ( ' Time Ratio (Sim/CPU):  '//TRIM( Num2LStr( TRatio ) ) )

   ENDIF

   RETURN
END SUBROUTINE RunTimes
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

   SimTimeLeft = ( TMax - ZTime )*DeltTime/( ZTime - PrevSimTime )            ! DeltTime/( ZTime - PrevSimTime ) is the delta_ClockTime divided by the delta_SimulationTime
   EndTime  =  MOD( CurrClockTime+SimTimeLeft, SecPerDay )
   EndHour  =  INT(   EndTime*InSecHr )
   EndMin   =  INT( ( EndTime - REAL( 3600*EndHour ) )*InSecMn )
   EndSec   = NINT(   EndTime - REAL( 3600*EndHour + 60*EndMin ) )

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
SUBROUTINE WrOutputLine( t, p_FAST, y_FAST, IfWOutput, EDOutput, SrvDOutput, HDOutput, SDOutput, MAPOutput, ErrStat, ErrMsg)
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

   INTEGER(IntKi),           INTENT(OUT)   :: ErrStat
   CHARACTER(*),             INTENT(OUT)   :: ErrMsg

      ! Local variables.

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
      IF ( y_FAST%numOuts_IfW > 0 ) THEN !Inflow Wind
         CALL WrReAryFileNR ( y_FAST%UnOu, IfWOutput,   Frmt, ErrStat, ErrMsg )
         IF ( ErrStat >= AbortErrLev ) RETURN
      END IF

      IF ( y_FAST%numOuts_ED > 0 ) THEN !ElastoDyn
         CALL WrReAryFileNR ( y_FAST%UnOu, EDOutput,   Frmt, ErrStat, ErrMsg )
         IF ( ErrStat >= AbortErrLev ) RETURN
      END IF

      IF ( y_FAST%numOuts_SrvD > 0 ) THEN !ServoDyn
         CALL WrReAryFileNR ( y_FAST%UnOu, SrvDOutput, Frmt, ErrStat, ErrMsg )
         IF ( ErrStat >= AbortErrLev ) RETURN
      END IF

      IF ( y_FAST%numOuts_HD > 0 ) THEN !HydroDyn
         CALL WrReAryFileNR ( y_FAST%UnOu, HDOutput,   Frmt, ErrStat, ErrMsg )
         IF ( ErrStat >= AbortErrLev ) RETURN
      END IF

      IF ( y_FAST%numOuts_SD > 0 ) THEN !SubDyn
         CALL WrReAryFileNR ( y_FAST%UnOu, SDOutput,   Frmt, ErrStat, ErrMsg )
         IF ( ErrStat >= AbortErrLev ) RETURN
      END IF
      
      IF ( y_FAST%numOuts_MAP > 0 ) THEN !MAP (Mooring Analysis Program)
         CALL WrReAryFileNR ( y_FAST%UnOu, MAPOutput,   Frmt, ErrStat, ErrMsg )
         IF ( ErrStat >= AbortErrLev ) RETURN
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

         IF ( y_FAST%numOuts_IfW > 0 ) THEN
            indxLast = indxNext + SIZE(IfWOutput) - 1
            y_FAST%AllOutData(indxNext:indxLast, y_FAST%n_Out) = IfWOutput
            indxNext = IndxLast + 1
         END IF

         IF ( y_FAST%numOuts_ED > 0 ) THEN
            indxLast = indxNext + SIZE(EDOutput) - 1
            y_FAST%AllOutData(indxNext:indxLast, y_FAST%n_Out) = EDOutput
            indxNext = IndxLast + 1
         END IF

         IF ( y_FAST%numOuts_SrvD > 0 ) THEN
            indxLast = indxNext + SIZE(SrvDOutput) - 1
            y_FAST%AllOutData(indxNext:indxLast, y_FAST%n_Out) = SrvDOutput
            indxNext = IndxLast + 1
         END IF

         IF ( y_FAST%numOuts_HD > 0 ) THEN
            indxLast = indxNext + SIZE(HDOutput) - 1
            y_FAST%AllOutData(indxNext:indxLast, y_FAST%n_Out) = HDOutput
            indxNext = IndxLast + 1
         END IF


      END IF

   END IF

   RETURN
END SUBROUTINE WrOutputLine
!----------------------------------------------------------------------------------------------------------------------------------


!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ED_InputSolve( p_FAST, u_ED, y_AD, y_SrvD, y_HD, u_HD, y_MAP, u_MAP, MeshMapData, ErrStat, ErrMsg )
! This routine sets the inputs required for ED
!..................................................................................................................................

   TYPE(FAST_ParameterType),       INTENT(IN)     :: p_FAST                   ! Glue-code simulation parameters
   TYPE(ED_InputType),             INTENT(INOUT)  :: u_ED                     ! ED Inputs at t
   TYPE(AD_OutputType),            INTENT(IN)     :: y_AD                     ! AeroDyn outputs
   TYPE(SrvD_OutputType),          INTENT(IN)     :: y_SrvD                   ! ServoDyn outputs
   TYPE(HydroDyn_OutputType),      INTENT(INOUT)  :: y_HD                     ! HydroDyn outputs
   TYPE(HydroDyn_InputType),       INTENT(INOUT)  :: u_HD                     ! HydroDyn inputs
   TYPE(MAP_OutputType),           INTENT(INOUT)  :: y_MAP                    ! MAP outputs
   TYPE(MAP_InputType),            INTENT(INOUT)  :: u_MAP                    ! MAP inputs
   
   TYPE(FAST_ModuleMapType),       INTENT(INOUT)  :: MeshMapData              ! Data for mapping between modules
   INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat                  ! Error status
   CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg                   ! Error message
   
      ! local variables
   INTEGER(IntKi)                                 :: J                        ! Loops through nodes / elements
   INTEGER(IntKi)                                 :: K                        ! Loops through blades
   INTEGER(IntKi)                                 :: NodeNum                  ! Node number for blade/element in mesh
   TYPE(MeshType)                                 :: u_mapped                 ! interpolated value of input


      ! Initialize error status
      
   ErrStat = ErrID_None
   ErrMsg = ""

      ! Create a local copy of the ED mesh (we really need the Position and RefOrientation fields on the nodes; we'll sum the total loads from each module being used):
   CALL MeshCopy ( u_ED%PlatformPtMesh, u_mapped, MESH_NEWCOPY, ErrStat, ErrMsg )
      CALL CheckError( ErrStat, ErrMsg )
      IF (ErrStat >= AbortErrLev) RETURN

   
      ! ED inputs from ServoDyn
   IF ( p_FAST%CompServo ) THEN

      u_ED%GenTrq     = y_SrvD%GenTrq
      u_ED%HSSBrTrq   = y_SrvD%HSSBrTrq
      u_ED%BlPitchCom = y_SrvD%BlPitchCom
      u_ED%YawMom     = y_SrvD%YawMom
   !   u_ED%TBDrCon    = y_SrvD%TBDrCon !array
   ELSE !we'll just take the initial guesses..
   END IF

      ! ED inputs from AeroDyn
!   IF ( p_FAST%CompAero .and. ALLOCATED(ADAeroLoads%Blade) ) THEN
!bjj: need another check on this perhaps
   IF ( p_FAST%CompAero  ) THEN
      DO K = 1,SIZE(y_AD%OutputLoads,1) ! Loop through all blades (p_ED%NumBl)
         DO J = 1,y_AD%OutputLoads(K)%Nnodes ! Loop through the blade nodes / elements (p_ED%BldNodes)

            NodeNum = (K-1)*(y_AD%OutputLoads(K)%Nnodes+2) + J 
            u_ED%BladeLn2Mesh%Force(:,NodeNum)  = y_AD%OutputLoads(K)%Force(:,J)
            u_ED%BladeLn2Mesh%Moment(:,NodeNum) = y_AD%OutputLoads(K)%Moment(:,J)
            
         END DO !J
      END DO   !K
   ELSE
      u_ED%BladeLn2Mesh%Force  = 0.0_ReKi
      u_ED%BladeLn2Mesh%Moment = 0.0_ReKi
   END IF


      ! ED inputs from UserTwrLd

   !IF ( p_FAST%CompUserTwrLd ) THEN
   !   u_ED%TwrAddedMass(:,:,J) = y_UsrTwr%AddedMass(:,:,J)
   !   u_ED%TowerLn2Mesh%Force  = y_UsrTwr%Force
   !   u_ED%TowerLn2Mesh%Moment = y_UsrTwr%Moment
   !ELSE
      u_ED%TwrAddedMass = 0.0_ReKi
      u_ED%TowerLn2Mesh%Force  = 0.0_ReKi
      u_ED%TowerLn2Mesh%Moment = 0.0_ReKi
   !END IF


      ! ED inputs from UserPtfmLd

   !IF ( p_FAST%CompUserPtfmLd ) THEN
   !   u_ED%PtfmAddedMass = y_UsrPtfm%AddedMass
   !   u_ED%PlatformPtMesh%Force  = y_UsrPtfm%Force
   !   u_ED%PlatformPtMesh%Moment = y_UsrPtfm%Moment
   !ELSE
      u_ED%PtfmAddedMass = 0.0_ReKi
      u_ED%PlatformPtMesh%Force(:,1) = 0.0_ReKi
      u_ED%PlatformPtMesh%Moment(:,1) = 0.0_ReKi
   !END IF


      ! ED inputs from HydroDyn

         ! Morison loads and added mass



         ! WAMIT loads and added mass and possibly Morison Elements for viscous drag, filled/flooded bouyancy, and filled/flooded added mass
   IF ( y_HD%WAMIT%Mesh%Initialized  ) THEN

      CALL Transfer_Point_to_Point( y_HD%WAMIT%Mesh, u_mapped, MeshMapData%HD_W_P_2_ED_P, ErrStat, ErrMsg, u_HD%WAMIT%Mesh ) !u_HD contains the orientations needed for moment calculations
         CALL CheckError( ErrStat, ErrMsg )
         IF (ErrStat >= AbortErrLev) RETURN

      u_ED%PlatformPtMesh%Force  = u_ED%PlatformPtMesh%Force  + u_mapped%Force
      u_ED%PlatformPtMesh%Moment = u_ED%PlatformPtMesh%Moment + u_mapped%Moment

            ! bjj: I left this in for now. Per JMJ, we will remove AddedMass later.
      u_ED%PtfmAddedMass = u_ED%PtfmAddedMass + y_HD%WAMIT%Mesh%AddedMass(:,:,1)

      IF ( y_HD%Morison%LumpedMesh%Initialized ) THEN

            ! This is viscous drag associate with the WAMIT body and/or filled/flooded forces of the WAMIT body

         CALL Transfer_Point_to_Point( y_HD%Morison%LumpedMesh, u_mapped, MeshMapData%HD_M_P_2_ED_P, ErrStat, ErrMsg, u_HD%Morison%LumpedMesh )
            CALL CheckError( ErrStat, ErrMsg )
            IF (ErrStat >= AbortErrLev) RETURN
            
         u_ED%PlatformPtMesh%Force  = u_ED%PlatformPtMesh%Force  + u_mapped%Force
         u_ED%PlatformPtMesh%Moment = u_ED%PlatformPtMesh%Moment + u_mapped%Moment

         ! TODO : Need to add Added Mass from morison pt elements. GJH 7/9/13

         CALL Transfer_Line2_to_Point( y_HD%Morison%DistribMesh, u_mapped, MeshMapData%HD_M_L_2_ED_P, ErrStat, ErrMsg, u_HD%Morison%DistribMesh )
            CALL CheckError( ErrStat, ErrMsg )
            IF (ErrStat >= AbortErrLev) RETURN
 
         u_ED%PlatformPtMesh%Force  = u_ED%PlatformPtMesh%Force  + u_mapped%Force
         u_ED%PlatformPtMesh%Moment = u_ED%PlatformPtMesh%Moment + u_mapped%Moment
         !
      END IF


   ELSE IF ( y_HD%Morison%LumpedMesh%Initialized ) THEN
      ! Only Morison Elements!
        ! Map the Morison element loads onto the ED tower: TODO
      DO J=1,u_ED%TowerLn2Mesh%Nnodes
         !u_ED%TwrAddedMass(:,:,J)      = u_ED%TwrAddedMass(:,:,J) + HD_AllLoads%Substructure(J)%AddedMass
         !u_ED%TowerLn2Mesh%Force(:,J)  = u_ED%TowerLn2Mesh%Force(:,J)  + HD_AllLoads%Substructure(J)%Force
         !u_ED%TowerLn2Mesh%Moment(:,J) = u_ED%TowerLn2Mesh%Moment(:,J) + HD_AllLoads%Substructure(J)%Moment
      END DO
      
   END IF
   
   
   IF ( p_FAST%CompMap ) THEN
      
      CALL Transfer_Point_to_Point( y_MAP%PtFairleadLoad, u_mapped, MeshMapData%MAP_P_2_ED_P, ErrStat, ErrMsg, u_MAP%PtFairleadDisplacement ) !u_MAP contains the orientations needed for moment calculations
         CALL CheckError( ErrStat, ErrMsg )
         IF (ErrStat >= AbortErrLev) RETURN

      u_ED%PlatformPtMesh%Force  = u_ED%PlatformPtMesh%Force  + u_mapped%Force
      u_ED%PlatformPtMesh%Moment = u_ED%PlatformPtMesh%Moment + u_mapped%Moment      
      
   END IF
   
   
   
   
      ! Destroy local copy of ED inputs
   CALL MeshDestroy ( u_mapped, ErrStat, ErrMsg )
      CALL CheckError( ErrStat, ErrMsg )
      IF (ErrStat >= AbortErrLev) RETURN
   
   
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

         CALL WrScr( ' ED_InputSolve: '//TRIM(Msg) )

         !.........................................................................................................................
         ! Clean up if we're going to return on error: close files, deallocate local arrays
         !.........................................................................................................................
         IF ( ErrStat >= AbortErrLev ) THEN
            CALL MeshDestroy ( u_mapped, ErrStat3, ErrMsg3 )
            IF (ErrStat3 /= ErrID_None) CALL WrScr(' ED_InputSolve/MeshDestroy: '//TRIM(ErrMsg3) )
         END IF

      END IF


   END SUBROUTINE CheckError

END SUBROUTINE ED_InputSolve
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE SrvD_InputSolve( p_FAST, u_SrvD, y_ED, y_IfW, y_SrvD_prev )
! This routine sets the inputs required for ServoDyn
!..................................................................................................................................

   TYPE(FAST_ParameterType),   INTENT(IN)     :: p_FAST       ! Glue-code simulation parameters
   TYPE(SrvD_InputType),       INTENT(INOUT)  :: u_SrvD       ! ServoDyn Inputs at t
   TYPE(ED_OutputType),        INTENT(IN)     :: y_ED         ! ElastoDyn outputs
   REAL(ReKi),                 INTENT(IN)     :: y_IfW(3)     ! InflowWind outputs
   TYPE(SrvD_OutputType),      INTENT(IN)     :: y_SrvD_prev  ! ServoDyn outputs from t - dt
!  TYPE(AD_OutputType),        INTENT(IN)     :: y_AD         ! AeroDyn outputs


      ! ServoDyn inputs from combination of InflowWind and ElastoDyn



   u_SrvD%YawAngle  = y_ED%YawAngle !nacelle yaw plus platform yaw

      ! Calculate horizontal hub-height wind direction and the nacelle yaw error estimate (both positive about zi-axis); these are
      !   zero if there is no wind input when AeroDyn is not used:

      !bjj: rename pass YawAngle (not YawErr from ED)
   IF ( p_FAST%CompAero )  THEN   ! AeroDyn has been used.

      u_SrvD%WindDir  = ATAN2( y_IfW(2), y_IfW(1) )
      u_SrvD%YawErr   = u_SrvD%WindDir - y_ED%YawAngle
      u_SrvD%HorWindV = SQRT( y_IfW(1)**2 + y_IfW(2)**2 )

   ELSE                    ! No AeroDynamics.

      u_SrvD%WindDir  = 0.0
      u_SrvD%YawErr   = 0.0
      u_SrvD%HorWindV = 0.0

   ENDIF

      ! ServoDyn inputs from ServoDyn outputs at previous step

   u_SrvD%ElecPwr_prev = y_SrvD_prev%ElecPwr  ! we want to know the electrical power from the previous time step  (for the Bladed DLL)
   u_SrvD%GenTrq_prev  = y_SrvD_prev%GenTrq   ! we want to know the electrical generator torque from the previous time step  (for the Bladed DLL)

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

      ! ServoDyn inputs from AeroDyn
   IF ( p_FAST%CompAero ) THEN
   ELSE
   END IF


      ! ServoDyn inputs from UserTwrLd

   !IF ( p_FAST%CompUserTwrLd ) THEN
   !END IF


      ! ServoDyn inputs from UserPtfmLd

   !IF ( p_FAST%CompUserPtfmLd ) THEN
   !END IF


      ! ServoDyn inputs from HydroDyn

   IF ( p_FAST%CompHydro ) THEN
   END IF


END SUBROUTINE SrvD_InputSolve
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE HD_InputSolve(  y_ED, u_HD, MeshMapData, ErrStat, ErrMsg )
! This routine sets the inputs required for HydroDyn.
!..................................................................................................................................

      ! Passed variables
   TYPE(ED_OutputType),         INTENT(INOUT) :: y_ED                         ! The outputs of the structural dynamics module
   TYPE(HydroDyn_InputType),    INTENT(INOUT) :: u_HD                         ! HydroDyn input
   TYPE(FAST_ModuleMapType),    INTENT(INOUT) :: MeshMapData

   INTEGER(IntKi)                            :: ErrStat     ! Error status of the operation
   CHARACTER(*)                              :: ErrMsg      ! Error message if ErrStat /= ErrID_None

   INTEGER(IntKi)               :: J                                               ! Loops through nodes / elements.

   TYPE(MeshType)               :: u_mapped    ! interpolated value of input
      !----------------------------------------------------------------------------------------------------
      ! Map ED outputs to HydroDyn inputs
      !----------------------------------------------------------------------------------------------------

      ! Set the markers required for HydroDyn

 !       IF ( HD_TwrNodes ) THEN

            ! Set the tower markers required for HydroDyn  (note this is for only the tower loading per unit length (not platform point source) !!!!!
! SubDyn:
 !        DO J = 1,y_ED%TowerLn2Mesh%NNodes  ! Loop through the tower nodes / elements
 !           HD_AllMarkers%Substructure(J)%Position       = y_ED%TowerLn2Mesh%TranslationDisp(:,J)
 !           HD_AllMarkers%Substructure(J)%TranslationVel = y_ED%TowerLn2Mesh%TranslationVel(:,J)
 !           HD_AllMarkers%Substructure(J)%RotationVel    = y_ED%TowerLn2Mesh%RotationVel(:,J)
 !           HD_AllMarkers%Substructure(J)%Orientation    = y_ED%TowerLn2Mesh%Orientation(:,:,J)
 !
         !also available: do we need the accelerations, too?


      IF ( u_HD%WAMIT%Mesh%Initialized ) THEN

      ! These are the lumped point loads associated the WAMIT body and include: hydrostatics, radiation memory effect,
      !    wave kinematics, additional preload, additional stiffness, additional linear damping, additional quadratic damping,
      !    hydrodynamic added mass

         CALL MeshCopy ( SrcMesh  = u_HD%WAMIT%Mesh      &
                       , DestMesh = u_mapped             &
                       , CtrlCode = MESH_NEWCOPY         &
                       , ErrStat  = ErrStat              &
                       , ErrMess  = ErrMsg               )

         CALL Transfer_Point_to_Point( y_ED%PlatformPtMesh, u_mapped, MeshMapData%ED_P_2_HD_W_P, ErrStat, ErrMsg )

!bjj: We could do this without all the extra meshcopy/destroy because this input is only from one mesh
!        CALL Transfer_Point_to_Point( y_ED%PlatformPtMesh, u_HD%WAMIT%Mesh, MeshMapData%ED_P_2_HD_W_P, ErrStat, ErrMsg )

         CALL MeshCopy ( SrcMesh   = u_mapped            &
                       , DestMesh = u_HD%WAMIT%Mesh      &
                       , CtrlCode = MESH_UPDATECOPY      &
                       , ErrStat  = ErrStat              &
                       , ErrMess  = ErrMsg               )


         !u_HD%WAMIT%Mesh%Orientation(:,:,1)    =  y_ED%PlatformPtMesh%Orientation(:,:,1)  ! TODO: Orientations are not dealt with in the transfer, yet 6/26/13 GJH
         !

         CALL MeshDestroy ( u_mapped, ErrStat, ErrMsg )

         IF ( u_HD%Morison%LumpedMesh%Initialized ) THEN


         ! These are the lumped point loads associated viscous drag on the WAMIT body and/or filled/flooded lumped forces of the WAMIT body

            CALL MeshCopy ( SrcMesh   = u_HD%Morison%LumpedMesh      &
                     , DestMesh = u_mapped             &
                     , CtrlCode = MESH_NEWCOPY         &
                     , ErrStat  = ErrStat              &
                     , ErrMess  = ErrMsg               )


            CALL Transfer_Point_to_Point( y_ED%PlatformPtMesh, u_mapped, MeshMapData%ED_P_2_HD_M_P, ErrStat, ErrMsg )

            CALL MeshCopy ( SrcMesh   = u_mapped      &
                     , DestMesh = u_HD%Morison%LumpedMesh             &
                     , CtrlCode = MESH_UPDATECOPY             &
                     , ErrStat  = ErrStat              &
                     , ErrMess  = ErrMsg               )

            CALL MeshDestroy ( u_mapped, ErrStat, ErrMsg )


         ! These are the line2 (distributed) loads associated viscous drag on the WAMIT body and/or filled/flooded distributed forces of the WAMIT body

            CALL MeshCopy ( SrcMesh   = u_HD%Morison%DistribMesh      &
                     , DestMesh = u_mapped             &
                     , CtrlCode = MESH_NEWCOPY         &
                     , ErrStat  = ErrStat              &
                     , ErrMess  = ErrMsg               )

            CALL Transfer_Point_to_Line2( y_ED%PlatformPtMesh, u_mapped, MeshMapData%ED_P_2_HD_M_L, ErrStat, ErrMsg )

            CALL MeshCopy ( SrcMesh   = u_mapped      &
                     , DestMesh = u_HD%Morison%DistribMesh             &
                     , CtrlCode = MESH_UPDATECOPY             &
                     , ErrStat  = ErrStat              &
                     , ErrMess  = ErrMsg               )

            CALL MeshDestroy ( u_mapped       &
                    , ErrStat  = ErrStat         &
                    , ErrMess  = ErrMsg           )

      END IF



   ELSE IF ( u_HD%Morison%LumpedMesh%Initialized ) THEN

      ! Only Morison Elements, no WAMIT body.  All HydroDyn Outputs go to SubDyn in this case

   END IF


END SUBROUTINE HD_InputSolve
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE MAP_InputSolve(  y_ED, u_MAP, MeshMapData, ErrStat, ErrMsg )
! This routine sets the inputs required for MAP.
!..................................................................................................................................

      ! Passed variables
   TYPE(ED_OutputType),         INTENT(INOUT) :: y_ED                         ! The outputs of the structural dynamics module
   TYPE(MAP_InputType),         INTENT(INOUT) :: u_MAP                        ! MAP input
   TYPE(FAST_ModuleMapType),    INTENT(INOUT) :: MeshMapData

   INTEGER(IntKi)                             :: ErrStat                      ! Error status of the operation
   CHARACTER(*)                               :: ErrMsg                       ! Error message if ErrStat /= ErrID_None


      !----------------------------------------------------------------------------------------------------
      ! Map ED outputs to MAP inputs
      !----------------------------------------------------------------------------------------------------

   CALL Transfer_Point_to_Point( y_ED%PlatformPtMesh, u_MAP%PtFairleadDisplacement, MeshMapData%ED_P_2_MAP_P, ErrStat, ErrMsg )


END SUBROUTINE MAP_InputSolve
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE SD_InputSolve(  y_ED, y_HD, u_SD, MeshMapData, ErrStat, ErrMsg )
! This routine sets the inputs required for MAP.
!..................................................................................................................................

      ! Passed variables
   TYPE(ED_OutputType),         INTENT(INOUT) :: y_ED                         ! The outputs of the structural dynamics module
   TYPE(HydroDyn_OutputType),   INTENT(INOUT) :: y_HD                         ! The outputs of the hydrodynamics module
   TYPE(SD_InputType),          INTENT(INOUT) :: u_SD                         ! SubDyn input
   TYPE(FAST_ModuleMapType),    INTENT(INOUT) :: MeshMapData

   INTEGER(IntKi)                             :: ErrStat                      ! Error status of the operation
   CHARACTER(*)                               :: ErrMsg                       ! Error message if ErrStat /= ErrID_None


      !----------------------------------------------------------------------------------------------------
      ! Map outputs to SD inputs
      !----------------------------------------------------------------------------------------------------



END SUBROUTINE SD_InputSolve
!----------------------------------------------------------------------------------------------------------------------------------



!====================================================================================================
SUBROUTINE AD_InputSolve( u_AD, y_ED, ErrStat, ErrMsg )
! THIS ROUTINE IS A HACK TO GET THE OUTPUTS FROM ELASTODYN INTO AERODYN. IT WILL BE REPLACED WHEN THIS CODE LINKS WITH
! AERODYN IN THE NEW FRAMEWORK
!,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

      ! Passed variables
   TYPE(AD_InputType),          INTENT(INOUT):: u_AD        ! The inputs to AeroDyn
   TYPE(ED_OutputType),         INTENT(IN)   :: y_ED        ! The outputs of the structural dynamics module
   INTEGER(IntKi)                            :: ErrStat     ! Error status of the operation
   CHARACTER(*)                              :: ErrMsg      ! Error message if ErrStat /= ErrID_None

      ! Local variables:

   INTEGER(IntKi)               :: J                                               ! Loops through nodes / elements.
   INTEGER(IntKi)               :: K                                               ! Loops through blades.
   INTEGER(IntKi)               :: NodeNum                                         ! Node number for blade/node on mesh
   INTEGER(IntKi)               :: NumBl
   INTEGER(IntKi)               :: BldNodes

   
   NumBl    = SIZE(u_AD%InputMarkers,1)
   BldNodes = u_AD%InputMarkers(1)%Nnodes
   
   !-------------------------------------------------------------------------------------------------
   ! Blade positions, orientations, and velocities:
   !-------------------------------------------------------------------------------------------------
   
   DO K = 1,NumBl !p%NumBl ! Loop through all blades
      DO J = 1,BldNodes !p%BldNodes ! Loop through the blade nodes / elements

         NodeNum = (K-1)*(BldNodes + 2) + J         ! note that this assumes ED has same discretization as AD
         
         u_AD%InputMarkers(K)%Position(:,J)       = y_ED%BladeLn2Mesh%TranslationDisp(:,NodeNum) + y_ED%BladeLn2Mesh%Position(:,NodeNum) 
         u_AD%InputMarkers(K)%Orientation(:,:,J)  = y_ED%BladeLn2Mesh%Orientation(:,:,NodeNum)
         u_AD%InputMarkers(K)%TranslationVel(:,J) = y_ED%BladeLn2Mesh%TranslationVel(:,NodeNum)
                  
      END DO !J = 1,p%BldNodes ! Loop through the blade nodes / elements
   END DO !K = 1,p%NumBl
   
   !-------------------------------------------------------------------------------------------------
   ! Hub positions, orientations, and velocities:
   !  (note that these may have to be adjusted in ElastoDyn as AeroDyn gets rewritten)
   !-------------------------------------------------------------------------------------------------
   u_AD%TurbineComponents%Hub%Position    = y_ED%HubPtMotion%TranslationDisp(:,1) +  y_ED%HubPtMotion%Position(:,1)
   u_AD%TurbineComponents%Hub%Orientation = y_ED%HubPtMotion%Orientation(:,:,1)   
   u_AD%TurbineComponents%Hub%RotationVel = y_ED%HubPtMotion%RotationVel(:,1)
   
   !-------------------------------------------------------------------------------------------------
   ! Blade root orientations:
   !-------------------------------------------------------------------------------------------------
   
   DO K=1,NumBl
      u_AD%TurbineComponents%Blade(K)%Orientation = y_ED%BladeRootMotions%Orientation(:,:,K)
   END DO
            

   !-------------------------------------------------------------------------------------------------
   ! RotorFurl position, orientation, rotational velocity:
   !-------------------------------------------------------------------------------------------------

   u_AD%TurbineComponents%RotorFurl%Position    = y_ED%RotorFurlMotion%TranslationDisp(:,1) + y_ED%RotorFurlMotion%Position(:,1)  
   u_AD%TurbineComponents%RotorFurl%Orientation = y_ED%RotorFurlMotion%Orientation(:,:,1)         
   u_AD%TurbineComponents%RotorFurl%RotationVel = y_ED%RotorFurlMotion%RotationVel(:,1)
   
   !-------------------------------------------------------------------------------------------------
   ! Nacelle position, orientation, rotational velocity:
   !-------------------------------------------------------------------------------------------------      

   u_AD%TurbineComponents%Nacelle%Position    = y_ED%NacelleMotion%TranslationDisp(:,1) + y_ED%NacelleMotion%Position(:,1)
   u_AD%TurbineComponents%Nacelle%Orientation = y_ED%NacelleMotion%Orientation(:,:,1)      
   u_AD%TurbineComponents%Nacelle%RotationVel = y_ED%NacelleMotion%RotationVel(:,1)  
   
   !-------------------------------------------------------------------------------------------------
   ! Tower base position, rotational velocity:
   !-------------------------------------------------------------------------------------------------      
   
   
      ! Tower base position should be rT(0) instead of rZ, but AeroDyn needs this for
      ! the HubVDue2Yaw calculation:
   u_AD%TurbineComponents%Tower%Position     = y_ED%TowerMotion%TranslationDisp(:,1) + y_ED%TowerMotion%Position(:,1)
   u_AD%TurbineComponents%Tower%RotationVel  = y_ED%TowerMotion%RotationVel(:,1)
  

END SUBROUTINE AD_InputSolve
!====================================================================================================
SUBROUTINE AeroInput(InitInData_AD, InitOutData_ED, y_ED, p_ED, p_FAST)
! This subroutine sets up the information needed to initialize AeroDyn, then initializes AeroDyn
!----------------------------------------------------------------------------------------------------

   USE                     AeroDyn_Types !,   ONLY: u_AD%InputMarkers, ADCurrentOutputs, ADIntrfaceOptions, ADFirstLoop, Prev_Aero_t
   USE                     AeroDyn
!   USE AD_IOParams, ONLY : UnEc !we'll reassign this variable so that it doesn't interfere with HydroDyn

   IMPLICIT NONE

   ! Passed variables:
   TYPE(AD_InitInputType),  INTENT(INOUT) :: InitInData_AD    ! The initialization input to AeroDyn
   TYPE(ED_InitOutputType), INTENT(IN)    :: InitOutData_ED   ! The initialization output from structural dynamics module
   TYPE(ED_ParameterType),  INTENT(IN)    :: p_ED             ! The parameters of the structural dynamics module
   TYPE(ED_OutputType),     INTENT(IN)    :: y_ED             ! The outputs of the structural dynamics module (meshes with position/RefOrientation set)
   TYPE(FAST_ParameterType),INTENT(IN)    :: p_FAST           ! The parameters of the glue code

      ! Local variables

   !TYPE(AD_InitOptions)       :: ADOptions                  ! Options for AeroDyn
   INTEGER                    :: NumADBldNodes              ! Number of blade nodes in AeroDyn
   REAL(ReKi)                 :: AD_RefHt

   INTEGER                    :: ErrStat, K


      ! Set up the AeroDyn parameters
   InitInData_AD%ADFileName       = p_FAST%ADFile
   InitInData_AD%OutRootName      = p_FAST%OutFileRoot
   InitInData_AD%WrSumFile        = p_FAST%SumPrint   
   
   InitInData_AD%NumBl     = y_ED%BladeRootMotions%NNodes
!BJJ: this should not be necessary   >>>>
!   InitInData_AD%NBlInpSt  = 0
!<<<<
   
      ! Hub position and orientation (relative here, but does not need to be)

   InitInData_AD%TurbineComponents%Hub%Position(:)      = y_ED%HubPtMotion%Position(:,1) - y_ED%HubPtMotion%Position(:,1)  ! bjj: was 0; mesh was changed by adding p_ED%HubHt to 3rd component
   InitInData_AD%TurbineComponents%Hub%Orientation(:,:) = y_ED%HubPtMotion%RefOrientation(:,:,1)


      ! Blade root position and orientation (relative here, but does not need to be)

   IF (.NOT. ALLOCATED( InitInData_AD%TurbineComponents%Blade ) ) THEN
      ALLOCATE( InitInData_AD%TurbineComponents%Blade( y_ED%BladeRootMotions%NNodes ), STAT = ErrStat )
      IF ( ErrStat /= 0 ) THEN
         CALL ProgAbort( ' Error allocating space for InitInData_AD%TurbineComponents%Blade.' )
      END IF
   END IF

   DO K=1, SIZE(InitInData_AD%TurbineComponents%Blade)
      InitInData_AD%TurbineComponents%Blade(K)%Position    = y_ED%BladeRootMotions%Position(:,K)
      InitInData_AD%TurbineComponents%Blade(K)%Orientation = y_ED%BladeRootMotions%RefOrientation(:,:,K)
   END DO
  

      ! Blade length

   InitInData_AD%TurbineComponents%BladeLength = InitOutData_ED%BladeLength


      ! Initialize AeroDyn

!CALL GetNewUnit(UnEc) ! I/O unit number for the echo file.
!   u_AD%InputMarkers = AD_Init(ADOptions, ADInterfaceComponents, ErrStat)    ! relative markers are returned
!CLOSE(UnEc)   
!UnEc = -1


   !   ! get the number of blade nodes from the returned data structure'
   !IF (.NOT. ALLOCATED( u_AD%InputMarkers%Blade ) ) THEN
   !   CALL ProgAbort( 'AeroDyn blade nodes are required to calculate aerodynamic loads.' )
   !   NumADBldNodes = 0
   !ELSE
   !   NumADBldNodes = SIZE( u_AD%InputMarkers%Blade, 1 )
   !END IF
   !
   !   ! allocate variables for aerodyn forces
   !
   !IF (.NOT. ALLOCATED(u_AD%SetMulTabLoc)) THEN
   !   ALLOCATE( u_AD%SetMulTabLoc(NumADBldNodes, InitOutData_ED%NumBl), STAT = ErrStat )
   !   IF ( ErrStat /= 0 ) CALL ProgAbort ( ' Error allocating memory for ADIntrfaceOptions%SetMulTabLoc array.' )
   !END IF
   !
   !ADIntrfaceOptions%SetMulTabLoc(:,:) = .FALSE.
   !ADIntrfaceOptions%LinearizeFlag     = .FALSE.

!   bjj: we don't use this, so don't allocate it
!
!   IF (.NOT. ALLOCATED(ADIntrfaceOptions%MulTabLoc)) THEN
!      ALLOCATE(ADIntrfaceOptions%MulTabLoc(NumADBldNodes, NumBl), STAT = Sttus )
!      IF ( Sttus /= 0 ) CALL ProgAbort ( ' Error allocating memory for ADIntrfaceOptions%MulTabLoc array.' )
!   END IF

   !
   !   ! Check that the hub-heights are Set up other parameters only if we need them
   !
   !IF ( p_FAST%CompAero )  THEN
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
END SUBROUTINE AeroInput
!=======================================================================
END MODULE FAST_IO_Subs
