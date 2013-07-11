!**********************************************************************************************************************************
! The FAST_Prog.f90, FAST_IO.f90, and FAST_Mods.f90 make up the FAST glue code in the FAST Modularization Framework. 
!..................................................................................................................................
! LICENSING
! Copyright (C) 2013  National Renewable Energy Laboratory
!
!    This file is part of FAST.
!
!    ElastoDyn is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as
!    published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
!    of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License along with ElastoDyn.
!    If not, see <http://www.gnu.org/licenses/>.
!
!**********************************************************************************************************************************
MODULE FAST_IO_Subs

   USE NWTC_Library
   USE FAST_Types
   
   USE ElastoDyn_Types
   USE ElastoDyn
   
   USE ServoDyn
   USE ServoDyn_Types

   USE AeroDyn
   USE AeroDyn_Types
   
   USE HydroDyn
   USE HydroDyn_Types
   
   USE InflowWind, ONLY: WindInfVer
   
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
   CALL DispCopyrightGPL( FAST_Ver )

   
      ! Tell our nice users what they're running
   CALL WrScr( ' Running '//GetVersion()//NewLine//' linked with '//TRIM( GetNVD( NWTC_Ver ))//NewLine )
   
   !...............................................................................................................................   
   ! Get the name of the input file from the command line if it isn't an input to this routine
   ! and set the root name of the output files based on the input file name
   !...............................................................................................................................   
   
   IF ( PRESENT(InFile) ) THEN 
      InputFile = InFile
   ELSE ! get it from the command line
      CALL CheckArgs( InputFile )
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
      
   IF ( ErrStat >= AbortErrLev ) RETURN

   
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
SUBROUTINE FAST_InitOutput( p_FAST, y_FAST, InitOutData_ED, InitOutData_SrvD, AD_Prog, InitOutData_HD, ErrStat, ErrMsg )
! This routine initializes the output for the glue code, including writing the header for the primary output file.
! was previously called WrOutHdr()
!..................................................................................................................................

   IMPLICIT NONE

      ! Passed variables
   TYPE(FAST_ParameterType), INTENT(IN)           :: p_FAST                                ! Glue-code simulation parameters
   TYPE(FAST_OutputType),    INTENT(INOUT)        :: y_FAST                                ! Glue-code simulation outputs

   TYPE(ED_InitOutputType),  INTENT(IN)           :: InitOutData_ED                        ! Initialization output for ElastoDyn
   TYPE(SrvD_InitOutputType),INTENT(IN)           :: InitOutData_SrvD                      ! Initialization output for ServoDyn
   TYPE(HD_InitOutputType),  INTENT(IN)           :: InitOutData_HD                        ! Initialization output for HydroDyn

   TYPE(ProgDesc), INTENT(IN) :: AD_prog ! aerodyn version    

   INTEGER(IntKi),           INTENT(OUT)          :: ErrStat                               ! Error status
   CHARACTER(*),             INTENT(OUT)          :: ErrMsg                                ! Error message corresponding to ErrStat 


      ! Local variables.

   INTEGER(IntKi)                   :: I                                               ! A generic index for DO loops.
   INTEGER(IntKi)                   :: indxLast                                        ! The index of the last value to be written to an array
   INTEGER(IntKi)                   :: indxNext                                        ! The index of the next value to be written to an array
   INTEGER(IntKi)                   :: NumOuts                                         ! number of channels to be written to the output file(s)


   !......................................................    
   ! Set the description lines to be printed in the output file
   !......................................................    
   y_FAST%FileDescLines(1)  = 'Predictions were generated on '//CurDate()//' at '//CurTime()//' using '//TRIM(GetVersion())
   y_FAST%FileDescLines(2)  = 'linked with ' &
                            //' '//TRIM(GetNVD(NWTC_Ver            )) &
                           //'; '//TRIM(GetNVD(InitOutData_ED%Ver  )) 
   IF ( p_FAST%CompServo ) y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) &
                           //'; '//TRIM(GetNVD(InitOutData_SrvD%Ver))
   IF ( p_FAST%CompHydro ) y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) &
                           //'; '//TRIM(GetNVD(InitOutData_HD%Ver))
   IF ( p_FAST%CompAero ) y_FAST%FileDescLines(2)  = TRIM(y_FAST%FileDescLines(2) ) &
                           //'; '//TRIM(GetNVD(WindInfVer)) &
                           //'; '//TRIM(GetNVD(AD_Prog))
   y_FAST%FileDescLines(3)  = 'Description from the FAST input file: '//TRIM(p_FAST%FTitle)

   !......................................................    
   ! Save the module version info for later use
   !......................................................    
   
   y_FAST%ED_Ver   = InitOutData_ED%Ver
   
   IF ( p_FAST%CompServo ) THEN
      y_FAST%SrvD_Ver = InitOutData_SrvD%Ver
   ELSE
      y_FAST%SrvD_Ver%Name = 'ServoDyn'
      y_FAST%SrvD_Ver%Date = ''
      y_FAST%SrvD_Ver%Ver = 'unknown version'
   END IF

   IF ( p_FAST%CompAero )  THEN
      y_FAST%AD_Ver   = AD_Prog
      y_FAST%IfW_Ver  = WindInfVer
   ELSE
      y_FAST%AD_Ver   = AD_Prog
      y_FAST%IfW_Ver  = WindInfVer
   END IF
   
   IF ( p_FAST%CompHydro ) THEN
      y_FAST%HD_Ver   = InitOutData_HD%Ver
   ELSE
      y_FAST%HD_Ver%Name = 'HydroDyn'
      y_FAST%HD_Ver%Date = ''
      y_FAST%HD_Ver%Ver = 'unknown version'
   END IF
      
   
   !......................................................    
   ! Set the number of output columns from each module
   !......................................................   
   
   y_FAST%numOuts_AD   = 0
   y_FAST%numOuts_IfW  = 3  !hack for now: always output 3 wind speeds at hub-height
      
   y_FAST%numOuts_ED   = 0
   IF ( ALLOCATED( InitOutData_ED%WriteOutputHdr   ) ) y_FAST%numOuts_ED   = SIZE(InitOutData_ED%WriteOutputHdr)

   y_FAST%numOuts_SrvD = 0   
   IF ( ALLOCATED( InitOutData_SrvD%WriteOutputHdr ) ) y_FAST%numOuts_SrvD = SIZE(InitOutData_SrvD%WriteOutputHdr)
   
   y_FAST%numOuts_HD = 0   
   IF ( ALLOCATED( InitOutData_HD%WriteOutputHdr   ) ) y_FAST%numOuts_HD   = SIZE(InitOutData_HD%WriteOutputHdr)
   
   !......................................................    
   ! Initialize the output channel names and units
   !......................................................    
   NumOuts   = 1 + y_FAST%numOuts_IfW + y_FAST%numOuts_ED + y_FAST%numOuts_SrvD + y_FAST%numOuts_AD + y_FAST%numOuts_HD  
   
   CALL AllocAry( y_FAST%ChannelNames,NumOuts, 'ChannelNames', ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN            
   CALL AllocAry( y_FAST%ChannelUnits,NumOuts, 'ChannelUnits', ErrStat, ErrMsg )
      IF ( ErrStat /= ErrID_None ) RETURN          
   
   y_FAST%ChannelNames(1) = 'Time'
   y_FAST%ChannelUnits(1) = '(s)'
  
   indxLast = 1
   indxNext = 2

   IF ( y_FAST%numOuts_IfW > 0_IntKi ) THEN  !hack for now
      indxLast = indxNext + y_FAST%numOuts_IfW - 1         
      y_FAST%ChannelNames(indxNext:indxLast)= (/ 'WindVxi   ', 'WindVyi   ', 'WindVzi   ' /)
      IF ( p_FAST%CompAero ) THEN
         y_FAST%ChannelUnits(indxNext:indxLast) = (/ '(m/s)     ', '(m/s)     ', '(m/s)     ' /)
      ELSE
         y_FAST%ChannelUnits(indxNext:indxLast) = (/ 'INVALID   ', 'INVALID   ', 'INVALID   ' /)
      END IF
      indxNext = indxLast + 1
   END IF

   
   IF ( y_FAST%numOuts_ED > 0_IntKi ) THEN
      indxLast = indxNext + y_FAST%numOuts_ED - 1         
      y_FAST%ChannelNames(indxNext:indxLast) = InitOutData_ED%WriteOutputHdr
      y_FAST%ChannelUnits(indxNext:indxLast) = InitOutData_ED%WriteOutputUnt
      indxNext = indxLast + 1
   END IF

   IF ( y_FAST%numOuts_SrvD > 0_IntKi ) THEN
      indxLast = indxNext + y_FAST%numOuts_SrvD - 1         
      y_FAST%ChannelNames(indxNext:indxLast) = InitOutData_SrvD%WriteOutputHdr
      y_FAST%ChannelUnits(indxNext:indxLast) = InitOutData_SrvD%WriteOutputUnt
      indxNext = indxLast + 1
   END IF
                               
   IF ( y_FAST%numOuts_HD > 0_IntKi ) THEN
      indxLast = indxNext + y_FAST%numOuts_HD - 1         
      y_FAST%ChannelNames(indxNext:indxLast) = InitOutData_HD%WriteOutputHdr
      y_FAST%ChannelUnits(indxNext:indxLast) = InitOutData_HD%WriteOutputUnt
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
      
      CALL AllocAry( y_FAST%AllOutData, NumOuts, y_FAST%NOutSteps, 'AllOutData', ErrStat, ErrMsg )
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
   
   CALL WrScr( ' Heading of the '//TRIM(FAST_Ver%Name)//' input file: '//TRIM( p%FTitle ) )      
                                   
   
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

      CALL WrOver( '                                                                                   ' )
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
SUBROUTINE WrOutputLine( t, p_FAST, y_FAST, IfWOutput, EDOutput, SrvDOutput, HDOutput, ErrStat, ErrMsg)
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
      IF ( y_FAST%numOuts_IfW > 0 ) THEN
         CALL WrReAryFileNR ( y_FAST%UnOu, IfWOutput,   Frmt, ErrStat, ErrMsg )
         IF ( ErrStat >= AbortErrLev ) RETURN
      END IF
      
      IF ( y_FAST%numOuts_ED > 0 ) THEN
         CALL WrReAryFileNR ( y_FAST%UnOu, EDOutput,   Frmt, ErrStat, ErrMsg )
         IF ( ErrStat >= AbortErrLev ) RETURN
      END IF
      
      IF ( y_FAST%numOuts_SrvD > 0 ) THEN
         CALL WrReAryFileNR ( y_FAST%UnOu, SrvDOutput, Frmt, ErrStat, ErrMsg )
         IF ( ErrStat >= AbortErrLev ) RETURN
      END IF

      IF ( y_FAST%numOuts_HD > 0 ) THEN
         CALL WrReAryFileNR ( y_FAST%UnOu, HDOutput,   Frmt, ErrStat, ErrMsg )
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
SUBROUTINE ED_InputSolve( p_FAST, p_ED, u_ED, y_SrvD )
! This routine sets the inputs required for ED
!..................................................................................................................................

   TYPE(FAST_ParameterType), INTENT(IN)     :: p_FAST                             ! Glue-code simulation parameters
   TYPE(ED_ParameterType),   INTENT(IN)     :: p_ED     ! ED parameters
   TYPE(ED_InputType),       INTENT(INOUT)  :: u_ED     ! ED Inputs at t
   TYPE(SrvD_OutputType),    INTENT(IN)     :: y_SrvD   ! ServoDyn outputs
!   TYPE(AD_OutputType),            INTENT(IN)     :: y_AD     ! AeroDyn outputs

   INTEGER(IntKi)               :: J                                               ! Loops through nodes / elements.
   INTEGER(IntKi)               :: K                                               ! Loops through blades.
   
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
   IF ( p_FAST%CompAero ) THEN
      DO K = 1,p_ED%NumBl ! Loop through all blades
         DO J = 1,p_ED%BldNodes ! Loop through the blade nodes / elements  
            u_ED%AeroBladeForce(:,J,K)  = ADAeroLoads%Blade(J,K)%Force
            u_ED%AeroBladeMoment(:,J,K) = ADAeroLoads%Blade(J,K)%Moment
         END DO !J
      END DO   !K      
   ELSE
      u_ED%AeroBladeForce = 0.0
      u_ED%AeroBladeMoment = 0.0
   END IF
      
      
      ! ED inputs from UserTwrLd
      
   !IF ( p_FAST%CompUserTwrLd ) THEN
   !   u_ED%TwrAddedMass(:,:,J) = y_UsrTwr%AddedMass(:,:,J)
   !   u_ED%TwrFT(1:3,J)        = y_UsrTwr%Force(:,J) 
   !   u_ED%TwrFT(4:6,J)        = y_UsrTwr%Moment(:,J)
   !ELSE
      u_ED%TwrAddedMass = 0.0_ReKi
!      u_ED%TwrFT        = 0.0_ReKi
      u_ED%TowerLn2Mesh%Force  = 0.0_ReKi
      u_ED%TowerLn2Mesh%Moment = 0.0_ReKi
   !END IF

   
      ! ED inputs from UserPtfmLd
         
   !IF ( p_FAST%CompUserPtfmLd ) THEN
   !   u_ED%PtfmAddedMass = y_UsrPtfm%AddedMass
   !   u_ED%PtfmFt(1:3)   = y_UsrPtfm%Force 
   !   u_ED%PtfmFt(4:6)   = y_UsrPtfm%Moment      
   !ELSE
      u_ED%PtfmAddedMass = 0.0_ReKi
      u_ED%PlatformPtMesh%Force(:,1) = 0.0_ReKi
      u_ED%PlatformPtMesh%Moment(:,1) = 0.0_ReKi
   !END IF   
   

      ! ED inputs from HydroDyn

   IF ( p_FAST%CompHydro ) THEN
      
      IF ( HD_TwrNodes ) THEN !SubDyn
         
         DO J=1,p_ED%TwrNodes
            
            u_ED%TowerLn2Mesh%Force( :,J) = u_ED%TowerLn2Mesh%Force( :,J) + HD_AllLoads%Substructure(J)%Force
            u_ED%TowerLn2Mesh%Moment(:,J) = u_ED%TowerLn2Mesh%Moment(:,J) + HD_AllLoads%Substructure(J)%Moment

            
            u_ED%TwrAddedMass(:,:,J) = u_ED%TwrAddedMass(:,:,J) + HD_AllLoads%Substructure(J)%AddedMass
         END DO
         
      ELSE ! HydroDyn; floating
!         CALL Transfer_Point_to_Point( )
         u_ED%PlatformPtMesh%Force(:,1)  = u_ED%PlatformPtMesh%Force(:,1)  + HD_AllLoads%Substructure(1)%Force
         u_ED%PlatformPtMesh%Moment(:,1) = u_ED%PlatformPtMesh%Moment(:,1) + HD_AllLoads%Substructure(1)%Moment
         
            ! bjj: I left this in for now. I need to talk to Jason about how removing the AddedMass 
            ! will change ElastoDyn's calculations.
         u_ED%PtfmAddedMass = u_ED%PtfmAddedMass + HD_AllLoads%Substructure(1)%AddedMass
         
      END IF         
   END IF    
   
   
END SUBROUTINE ED_InputSolve
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE SrvD_InputSolve( p_FAST, u_SrvD, y_ED, y_IfW, y_SrvD )
! This routine sets the inputs required for ServoDyn
!..................................................................................................................................

   TYPE(FAST_ParameterType),   INTENT(IN)     :: p_FAST     ! Glue-code simulation parameters
   TYPE(SrvD_InputType),       INTENT(INOUT)  :: u_SrvD     ! ServoDyn Inputs at t
   TYPE(ED_OutputType),        INTENT(IN)     :: y_ED       ! ElastoDyn outputs
   REAL(ReKi),                 INTENT(IN)     :: y_IfW(3)   ! InflowWind outputs
   TYPE(SrvD_OutputType),      INTENT(IN)     :: y_SrvD     ! ServoDyn outputs
!  TYPE(AD_OutputType),        INTENT(IN)     :: y_AD       ! AeroDyn outputs
  
   
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
      
   u_SrvD%ElecPwr_prev = y_SrvD%ElecPwr  ! we want to know the electrical power from the previous time step  (for the Bladed DLL)
   u_SrvD%GenTrq_prev  = y_SrvD%GenTrq   ! we want to know the electrical generator torque from the previous time step  (for the Bladed DLL)
   
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

!====================================================================================================
SUBROUTINE HD_InputSolve(  y_ed, ErrStat, ErrMsg  )
! THIS ROUTINE IS A HACK TO GET THE OUTPUTS FROM ELASTODYN INTO HYDRODYN. IT WILL BE REPLACED WHEN THIS CODE LINKS WITH 
! HYDRODYN IN THE NEW FRAMEWORK
!,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
  

      ! Passed variables
   !TYPE(HD_InputType),          INTENT(IN)   :: u_hd             ! The inputs for the hydrodynamics dynamics module
   TYPE(ED_OutputType),         INTENT(IN)   :: y_ed              ! The outputs of the structural dynamics module
   INTEGER(IntKi)                            :: ErrStat           ! Error status of the operation
   CHARACTER(*)                              :: ErrMsg            ! Error message if ErrStat /= ErrID_None
   
       !local variables:
   INTEGER(IntKi)                            :: J                 ! Loops through nodes / elements.
     
      !----------------------------------------------------------------------------------------------------
      ! Map ED outputs to HydroDyn inputs
      !----------------------------------------------------------------------------------------------------

      ! Set the markers required for HydroDyn

      IF ( HD_TwrNodes ) THEN

            ! Set the tower markers required for HydroDyn  (note this is for only the tower loading per unit length (not platform point source) !!!!!
! SubDyn:
         DO J = 1,y_ED%TowerLn2Mesh%NNodes  ! Loop through the tower nodes / elements
            HD_AllMarkers%Substructure(J)%Position       = y_ED%TowerLn2Mesh%TranslationDisp(:,J)
            HD_AllMarkers%Substructure(J)%TranslationVel = y_ED%TowerLn2Mesh%TranslationVel(:,J)
            HD_AllMarkers%Substructure(J)%RotationVel    = y_ED%TowerLn2Mesh%RotationVel(:,J)
            HD_AllMarkers%Substructure(J)%Orientation    = y_ED%TowerLn2Mesh%Orientation(:,:,J)
            
         !also available: do we need the accelerations, too?
            
         END DO

      ELSE
!HydroDyn
            ! Set the platform markers required for HydroDyn

         J = SIZE( HD_AllMarkers%Substructure, 1)

         HD_AllMarkers%Substructure(J)%Position      = y_ED%PlatformPtMesh%TranslationDisp(:,1)
         HD_AllMarkers%Substructure(J)%RotationVel   = y_ED%PlatformPtMesh%RotationVel(:,1)
         HD_AllMarkers%Substructure(J)%TranslationVel= y_ED%PlatformPtMesh%TranslationVel(:,1)
         HD_AllMarkers%Substructure(J)%Orientation   = y_ED%PlatformPtMesh%Orientation(:,:,1)
         
         !also available:
         !y_ED%PlatformPtMesh%RotationAcc(:,1)
         !y_ED%PlatformPtMesh%TranslationAcc(:,1)
         
         IF ( ErrStat /= ErrID_None ) RETURN


      END IF



END SUBROUTINE HD_InputSolve
!====================================================================================================
SUBROUTINE AD_InputSolve( p, x, OtherState, u, y, ErrStat, ErrMsg )
! THIS ROUTINE IS A HACK TO GET THE OUTPUTS FROM ELASTODYN INTO AERODYN. IT WILL BE REPLACED WHEN THIS CODE LINKS WITH 
! AERODYN IN THE NEW FRAMEWORK
!,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

      ! Passed variables
   TYPE(ED_InputType),          INTENT(IN)   :: u                            ! The inputs for the structural dynamics module
   TYPE(ED_OutputType),         INTENT(IN)   :: y                            ! The outputs of the structural dynamics module
   TYPE(ED_ParameterType),      INTENT(IN)   :: p                            ! The parameters of the structural dynamics module
   TYPE(ED_ContinuousStateType),INTENT(IN)   :: x                            ! The structural dynamics module's continuous states
   TYPE(ED_OtherStateType),     INTENT(IN)   :: OtherState                   ! Other State data type for Structural dynamics module
   INTEGER(IntKi)                            :: ErrStat     ! Error status of the operation
   CHARACTER(*)                              :: ErrMsg      ! Error message if ErrStat /= ErrID_None

      ! Local variables:

   REAL(ReKi)                   :: rAerCen   (3)                                   ! Position vector from inertial frame origin to current blade analysis node aerodynamic center.
   REAL(ReKi)                   :: rPAerCen  (3)                                   ! Position vector from teeter pin (point P) to current blade analysis node aerodynamic center.
   REAL(ReKi)                   :: rSAerCen  (3)                                   ! Position vector from a blade analysis node (point S) on the current blade to the aerodynamic center associated with the element.
   REAL(ReKi)                   :: LinAccEO  (3)                                   ! Total linear acceleration of the base plate (point O) in the inertia frame (body E for earth).

      ! local integer variables

   INTEGER(IntKi)               :: I                                               ! Loops through some or all of the DOFs.
   INTEGER(IntKi)               :: J                                               ! Loops through nodes / elements.
   INTEGER(IntKi)               :: K                                               ! Loops through blades.
   INTEGER(IntKi)               :: L                                               ! Generic index



   !-------------------------------------------------------------------------------------------------
   ! Blade positions:
   !-------------------------------------------------------------------------------------------------
   DO K = 1,p%NumBl ! Loop through all blades
      DO J = 1,p%BldNodes ! Loop through the blade nodes / elements

      ! Calculate the aerodynamic pitching moment arm (i.e., the position vector from point S on the blade to the aerodynamic center of the element):

            rSAerCen = p%rSAerCenn1(K,J)*OtherState%CoordSys%n1(K,J,:) + p%rSAerCenn2(K,J)*OtherState%CoordSys%n2(K,J,:) !bjj: make rSAerCen a matrix? we recalculate it later


      ! Define positions USEd by AeroDyn.

            rPAerCen     = OtherState%RtHS%rPQ + OtherState%RtHS%rQS(K,J,:) + rSAerCen         ! Position vector from teeter pin (point P)  to blade analysis node aerodynamic center.
            rAerCen      =                       OtherState%RtHS%rS (K,J,:) + rSAerCen         ! Position vector from inertial frame origin to blade analysis node aerodynamic center.

            ADAeroMarkers%Blade(J,K)%Position(1)      =     rAerCen(1)                ! = the distance from the undeflected tower centerline                                     to the current blade aerodynamic center in the xi ( z1) direction
            ADAeroMarkers%Blade(J,K)%Position(2)      = -1.*rAerCen(3)                ! = the distance from the undeflected tower centerline                                     to the current blade aerodynamic center in the yi (-z3) direction
            ADAeroMarkers%Blade(J,K)%Position(3)      =     rAerCen(2) + p%PtfmRefzt  ! = the distance from the nominal tower base position (i.e., the undeflected position of the tower base) to the current blade aerodynamic center in the zi ( z2) direction

      END DO !J = 1,p%BldNodes ! Loop through the blade nodes / elements
   END DO !K = 1,p%NumBl

   !JASON: WE SHOULD REALLY BE PASSING TO AERODYN THE LINEAR VELOCITIES OF THE AERODYNAMIC CENTER IN THE INERTIA FRAME, NOT SIMPLY THE LINEAR VELOCITIES OF POINT S.  IS THERE ANY WAY OF GETTING THIS VELOCITY?<--DO THIS, WHEN YOU ADD THE COUPLED MODE SHAPES!!!!


      ! the hub position should use rQ instead of rP, but the current version of AeroDyn treats
      ! teeter deflections like blade deflections:

   ADInterfaceComponents%Hub%Position  = (/ OtherState%RtHS%rP(1), -1.*OtherState%RtHS%rP(3), OtherState%RtHS%rP(2) + p%PtfmRefzt /)


      ! Rotor furl position should be rP instead of rV, but AeroDyn needs this for the
      ! HubVDue2Yaw calculation:

   ADInterfaceComponents%RotorFurl%Position(:) = (/ OtherState%RtHS%rV(1), -1.*OtherState%RtHS%rV(3), OtherState%RtHS%rV(2) + p%PtfmRefzt /)

   ADInterfaceComponents%Nacelle%Position(:)   = (/ OtherState%RtHS%rO(1), -1.*OtherState%RtHS%rO(3), OtherState%RtHS%rO(2) + p%PtfmRefzt /)

      ! Tower base position should be rT(0) instead of rZ, but AeroDyn needs this for
      ! the HubVDue2Yaw calculation:
   ADInterfaceComponents%Tower%Position(:)     = (/ OtherState%RtHS%rZ(1), -1.*OtherState%RtHS%rZ(3), OtherState%RtHS%rZ(2) + p%PtfmRefzt /)


   !-------------------------------------------------------------------------------------------------
   ! Orientations
   !-------------------------------------------------------------------------------------------------

   DO K = 1,p%NumBl
      DO J = 1,p%BldNodes

         ADAeroMarkers%Blade(J,K)%Orientation(1,1) =     OtherState%CoordSys%te1(K,J,1)
         ADAeroMarkers%Blade(J,K)%Orientation(2,1) =     OtherState%CoordSys%te2(K,J,1)
         ADAeroMarkers%Blade(J,K)%Orientation(3,1) =     OtherState%CoordSys%te3(K,J,1)
         ADAeroMarkers%Blade(J,K)%Orientation(1,2) = -1.*OtherState%CoordSys%te1(K,J,3)
         ADAeroMarkers%Blade(J,K)%Orientation(2,2) = -1.*OtherState%CoordSys%te2(K,J,3)
         ADAeroMarkers%Blade(J,K)%Orientation(3,2) = -1.*OtherState%CoordSys%te3(K,J,3)
         ADAeroMarkers%Blade(J,K)%Orientation(1,3) =     OtherState%CoordSys%te1(K,J,2)
         ADAeroMarkers%Blade(J,K)%Orientation(2,3) =     OtherState%CoordSys%te2(K,J,2)
         ADAeroMarkers%Blade(J,K)%Orientation(3,3) =     OtherState%CoordSys%te3(K,J,2)

      END DO !J = 1,p%BldNodes ! Loop through the blade nodes / elements
   END DO !K = 1,p%NumBl



         ! Blade root orientations should use the j instead of i system, but the current version
         ! of AeroDyn calculates forces normal and tangential to the cone of rotation

   ADInterfaceComponents%Blade(:)%Orientation(1,1) =     OtherState%CoordSys%i1(:,1)
   ADInterfaceComponents%Blade(:)%Orientation(2,1) =     OtherState%CoordSys%i2(:,1)
   ADInterfaceComponents%Blade(:)%Orientation(3,1) =     OtherState%CoordSys%i3(:,1)
   ADInterfaceComponents%Blade(:)%Orientation(1,2) = -1.*OtherState%CoordSys%i1(:,3)
   ADInterfaceComponents%Blade(:)%Orientation(2,2) = -1.*OtherState%CoordSys%i2(:,3)
   ADInterfaceComponents%Blade(:)%Orientation(3,2) = -1.*OtherState%CoordSys%i3(:,3)
   ADInterfaceComponents%Blade(:)%Orientation(1,3) =     OtherState%CoordSys%i1(:,2)
   ADInterfaceComponents%Blade(:)%Orientation(2,3) =     OtherState%CoordSys%i2(:,2)
   ADInterfaceComponents%Blade(:)%Orientation(3,3) =     OtherState%CoordSys%i3(:,2)

        ! Hub orientation should use the g instead of e system, but the current version
        ! of AeroDyn calculates forces normal and tangential to the cone of rotation

   ADInterfaceComponents%Hub%Orientation(:,1)  =     (/ OtherState%CoordSys%e1(1), OtherState%CoordSys%e2(1), OtherState%CoordSys%e3(1) /)
   ADInterfaceComponents%Hub%Orientation(:,2)       = -1.*(/ OtherState%CoordSys%e1(3), OtherState%CoordSys%e2(3), OtherState%CoordSys%e3(3) /)
   ADInterfaceComponents%Hub%Orientation(:,3)       =     (/ OtherState%CoordSys%e1(2), OtherState%CoordSys%e2(2), OtherState%CoordSys%e3(2) /)

        ! Rotor furl orientation (note the different order than hub and blade root!)

   ADInterfaceComponents%RotorFurl%Orientation(:,1) = (/      OtherState%CoordSys%c1(1), -1.*OtherState%CoordSys%c3(1),     OtherState%CoordSys%c2(1) /)
   ADInterfaceComponents%RotorFurl%Orientation(:,2) = (/ -1.* OtherState%CoordSys%c1(3),     OtherState%CoordSys%c3(3), -1.*OtherState%CoordSys%c2(3) /)
   ADInterfaceComponents%RotorFurl%Orientation(:,3) = (/      OtherState%CoordSys%c1(2), -1.*OtherState%CoordSys%c3(2),     OtherState%CoordSys%c2(2) /)

         ! Nacelle orientation (note the different order than hub and blade root!)

   ADInterfaceComponents%Nacelle%Orientation(:,1) = (/      OtherState%CoordSys%d1(1), -1.*OtherState%CoordSys%d3(1),     OtherState%CoordSys%d2(1) /)
   ADInterfaceComponents%Nacelle%Orientation(:,2) = (/ -1.* OtherState%CoordSys%d1(3),     OtherState%CoordSys%d3(3), -1.*OtherState%CoordSys%d2(3) /)
   ADInterfaceComponents%Nacelle%Orientation(:,3) = (/      OtherState%CoordSys%d1(2), -1.*OtherState%CoordSys%d3(2),     OtherState%CoordSys%d2(2) /)

   !-------------------------------------------------------------------------------------------------
   ! Velocities
   !-------------------------------------------------------------------------------------------------      
   
      ! Note the hub rotational velocity should be AngVelEH instead AngVelEL, but AeroDyn (13.00.00)
      ! treats teeter deflections like blade deflections:

   ADInterfaceComponents%Hub%RotationVel(:)       = (/ OtherState%RtHS%AngVelEL(1), -1.*OtherState%RtHS%AngVelEL(3), OtherState%RtHS%AngVelEL(2) /)
   ADInterfaceComponents%RotorFurl%RotationVel(:) = (/ OtherState%RtHS%AngVelER(1), -1.*OtherState%RtHS%AngVelER(3), OtherState%RtHS%AngVelER(2) /)
   ADInterfaceComponents%Nacelle%RotationVel(:)   = (/ OtherState%RtHS%AngVelEN(1), -1.*OtherState%RtHS%AngVelEN(3), OtherState%RtHS%AngVelEN(2) /)
   ADInterfaceComponents%Tower%RotationVel(:)     = (/ OtherState%RtHS%AngVelEX(1), -1.*OtherState%RtHS%AngVelEX(3), OtherState%RtHS%AngVelEX(2) /)

      
   DO K = 1,p%NumBl ! Loop through all blades
      DO J = 1,p%BldNodes ! Loop through the blade nodes / elements
         ADAeroMarkers%Blade(J,K)%TranslationVel(:)= (/ OtherState%RtHS%LinVelES(1,J,K), -1.*OtherState%RtHS%LinVelES(3,J,K),  OtherState%RtHS%LinVelES(2,J,K)  /)  !AeroDyn's coordinates
      END DO !J = 1,p%BldNodes ! Loop through the blade nodes / elements
   END DO !K = 1,p%NumBl
   

END SUBROUTINE AD_InputSolve
!====================================================================================================
SUBROUTINE AeroInput(p_ED, p_FAST)
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
   ADOptions%OutRootName      = p_FAST%OutFileRoot
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

   IF ( p_FAST%CompAero )  THEN

      ! Let's see if the hub-height in AeroDyn and FAST are within 10%:
      AD_RefHt = AD_GetConstant('RefHt', ErrStat)

      IF ( ABS( p_ED%FASTHH - AD_RefHt ) > 0.1*( p_ED%FASTHH ) )  THEN  !bjj: I believe that this should not be done in the future

         CALL ProgWarn( ' The ElastoDyn hub height ('//TRIM(Num2LStr( p_ED%FASTHH ))//') and AeroDyn input'// &
                       ' reference hub height ('//TRIM(Num2LStr(AD_RefHt))//') differ by more than 10%.' )
      ENDIF

   ENDIF   
   
   
   RETURN
END SUBROUTINE AeroInput
!=======================================================================
SUBROUTINE AeroDyn_End(ErrStat)

   USE AeroDyn_Types
   
   INTEGER(IntKi),               INTENT(  OUT)  :: ErrStat     ! Error status of the operation

   
   
   
   CALL AD_Terminate(   ErrStat )
      
      
      
      ! MODULE AeroElem

   IF ( ALLOCATED(ADAeroMarkers%Blade                ) ) DEALLOCATE(ADAeroMarkers%Blade                )
   IF ( ALLOCATED(ADAeroMarkers%Hub                  ) ) DEALLOCATE(ADAeroMarkers%Hub                  )
   IF ( ALLOCATED(ADAeroMarkers%RotorFurl            ) ) DEALLOCATE(ADAeroMarkers%RotorFurl            )
   IF ( ALLOCATED(ADAeroMarkers%Nacelle              ) ) DEALLOCATE(ADAeroMarkers%Nacelle              )
   IF ( ALLOCATED(ADAeroMarkers%Tower                ) ) DEALLOCATE(ADAeroMarkers%Tower                )
   IF ( ALLOCATED(ADAeroMarkers%Tail                 ) ) DEALLOCATE(ADAeroMarkers%Tail                 )

   IF ( ALLOCATED(ADAeroLoads%Blade                  ) ) DEALLOCATE(ADAeroLoads%Blade                  )
   IF ( ALLOCATED(ADAeroLoads%Hub                    ) ) DEALLOCATE(ADAeroLoads%Hub                    )
   IF ( ALLOCATED(ADAeroLoads%RotorFurl              ) ) DEALLOCATE(ADAeroLoads%RotorFurl              )
   IF ( ALLOCATED(ADAeroLoads%Nacelle                ) ) DEALLOCATE(ADAeroLoads%Nacelle                )
   IF ( ALLOCATED(ADAeroLoads%Tower                  ) ) DEALLOCATE(ADAeroLoads%Tower                  )
   IF ( ALLOCATED(ADAeroLoads%Tail                   ) ) DEALLOCATE(ADAeroLoads%Tail                   )

   IF ( ALLOCATED(ADIntrfaceOptions%SetMulTabLoc     ) ) DEALLOCATE(ADIntrfaceOptions%SetMulTabLoc     )
   IF ( ALLOCATED(ADIntrfaceOptions%MulTabLoc        ) ) DEALLOCATE(ADIntrfaceOptions%MulTabLoc        )

   IF ( ALLOCATED(ADInterfaceComponents%Blade        ) ) DEALLOCATE(ADInterfaceComponents%Blade        )

   
   
   
END SUBROUTINE AeroDyn_End
!=======================================================================
END MODULE FAST_IO_Subs
