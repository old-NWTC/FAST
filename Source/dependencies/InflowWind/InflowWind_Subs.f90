MODULE InflowWind_Subs
! This module is used to read and process the (undisturbed) inflow winds.  It must be initialized
! using InflowWind_Init() with the name of the file, the file type, and possibly reference height and
! width (depending on the type of wind file being used).  This module calls appropriate routines
! in the wind modules so that the type of wind becomes seamless to the user.  InflowWind_Terminate()
! should be called when the program has finshed.
!
! Data are assumed to be in units of meters and seconds.  Z is measured from the ground (NOT the hub!).
!
!  7 Oct 2009    Initial Release with AeroDyn 13.00.00      B. Jonkman, NREL/NWTC
! 14 Nov 2011    v1.00.01b-bjj                              B. Jonkman
!  1 Aug 2012    v1.01.00a-bjj                              B. Jonkman
! 10 Aug 2012    v1.01.00b-bjj                              B. Jonkman
!----------------------------------------------------------------------------------------------------
! File last committed: $Date: 2015-04-14 14:33:00 -0600 (Tue, 14 Apr 2015) $
! (File) Revision #: $Rev: 161 $
! URL: $HeadURL: https://windsvn.nrel.gov/InflowWind/branches/modularization/Source/InflowWind_Subs.f90 $
!----------------------------------------------------------------------------------------------------
!
!..................................................................................................................................
! LICENSING
! Copyright (C) 2012  National Renewable Energy Laboratory
!
!    This file is part of InflowWind.
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

   USE                              NWTC_Library
   USE                              InflowWind_Types
         
   !-------------------------------------------------------------------------------------------------
   ! The included wind modules
   !-------------------------------------------------------------------------------------------------

   USE                              IfW_HHWind           ! Types used in IfW_HHWind
   USE                              IfW_FFWind           ! Types used in IfW_FFWind
!   USE                              HHWind               ! hub-height text wind files
!   USE                              FFWind               ! full-field binary wind files
!   USE                              HAWCWind             ! full-field binary wind files in HAWC format
!   USE                              FDWind               ! 4-D binary wind files
!   USE                              CTWind               ! coherent turbulence from KH billow - binary file superimposed on another wind type
!   USE                              UserWind             ! user-defined wind module


   IMPLICIT                         NONE
            
!NOTE: don't make this private. Then none of these routines will be available to the module. Don't make it public either because then anything can get to them.
!bjj: hmm?? if it isn't private, it's public. :) 
CONTAINS
!====================================================================================================
SUBROUTINE GetWindType( ParamData, ErrStat, ErrMsg )
!  This subroutine checks the file FileName to see what kind of wind file we are using.  Used when
!  the wind file type is unknown.
!----------------------------------------------------------------------------------------------------
!FIXME: may want to change this to a subroutine that sets stuff in the passed IfW_ParameterType variable

   IMPLICIT             NONE


      ! Passed Variables:

   TYPE( IfW_ParameterType),        INTENT(INOUT)     :: ParamData
   INTEGER(IntKi),                  INTENT(  OUT)     :: ErrStat
   CHARACTER(*),                    INTENT(  OUT)     :: ErrMsg


      ! Local Variables:

   INTEGER                                            :: IND
   LOGICAL                                            :: Exists

   CHARACTER(1024)                                    :: FileName       ! Temporary name holder
   CHARACTER(  3)                                     :: FileNameEnd
   CHARACTER(  8)                                     :: WndFilNam      ! Temporary name holder

!   CHARACTER(1024)                                    :: FileRoot


   ErrStat  = ErrID_None
   ErrMsg   = ""

   !-------------------------------------------------------------------------------------------------
   ! Check for user-defined wind file first; file starts with "USERWIND"
   !-------------------------------------------------------------------------------------------------

   WndFilNam = ParamData%WindFileName
   FileName  = ParamData%WindFileName
   CALL Conv2UC( WndFilNam )              ! convert name to upper case

   IF ( WndFilNam == 'USERWIND' )  THEN

      CALL WrScr( NewLine//'   Detected user-defined wind file.' )
      ParamData%WindFileType = UD_WindNumber

      RETURN
   END IF

   !-------------------------------------------------------------------------------------------------
   ! Get the file extension (or at least what we expect the extension to be)
   !-------------------------------------------------------------------------------------------------
   CALL GetRoot ( ParamData%WindFileName, ParamData%WindFileNameRoot )            ! Get the root name

   IND = LEN_TRIM( ParamData%WindFileNameRoot ) + 1
   IF ( IND < LEN_TRIM( ParamData%WindFileName ) ) THEN
         ! Get the extention, starting at first character past (may not be the whole "extension")
      FileNameEnd = ParamData%WindFileName(IND+1:)
      CALL Conv2UC (FileNameEnd)
   ELSE
      FileNameEnd = ""
      IND = 0
   END IF


   !-------------------------------------------------------------------------------------------------
   ! If there was no '.' in the file name, assume FF, and add a .wnd extension
   !-------------------------------------------------------------------------------------------------
   IF ( IND == 0 ) THEN
      CALL WrScr( NewLine//'   No file extension found. Assuming '//TRIM(FileName)// &
                  ' is a binary FF wind file with a ".wnd" extension.')
      ParamData%WindFileType = FF_WindNumber
      ParamData%WindFileName = TRIM(ParamData%WindFileName)//'.wnd'
      !FileName = TRIM(FileName)//'.wnd'
      RETURN
   END IF


   !-------------------------------------------------------------------------------------------------
   ! Base the file type on the extension
   !-------------------------------------------------------------------------------------------------
   SELECT CASE ( TRIM(FileNameEnd) )
      CASE ('WND')

            ! If a summary file exists, assume FF; otherwise, assume HH file.

         INQUIRE ( FILE=FileName(1:IND)//'sum' , EXIST=Exists )
         IF (Exists) THEN
            CALL WrScr(NewLine//'   Assuming '//TRIM(FileName)//' is a binary FF wind file.')
            ParamData%WindFileType = FF_WindNumber
         ELSE
            CALL WrScr(NewLine//'   Assuming '//TRIM(FileName)//' is a formatted HH wind file.')
            ParamData%WindFileType = HH_WindNumber
         END IF

      CASE ('BTS')
         CALL WrScr(NewLine//'   Assuming '//TRIM(FileName)//' is a binary FF wind file.')
         ParamData%WindFileType = FF_WindNumber

      CASE ('CTP')
         CALL WrScr(NewLine//'   Assuming '//TRIM(FileName)//' is a coherent turbulence wind file.')
         ParamData%WindFileType = CTP_WindNumber

      CASE ('FDP')
         CALL WrScr(NewLine//'   Assuming '//TRIM(FileName)//' is a binary 4-dimensional wind file.')
         ParamData%WindFileType = FD_WindNumber

      CASE ('HWC')
         CALL WrScr(NewLine//'   Assuming '//TRIM(FileName)//' contains full-field wind parameters in HAWC format.')
         ParamData%WindFileType = HAWC_WindNumber

      CASE DEFAULT
         CALL WrScr(NewLine//'   Assuming '//TRIM(FileName)//' is a formatted HH wind file.')
         ParamData%WindFileType = HH_WindNumber

   END SELECT


RETURN
END SUBROUTINE GetWindType
!====================================================================================================
SUBROUTINE CalculateOutput( Time, InputData, ParamData, &
                              ContStates, DiscStates, ConstrStates, OtherStates, &   ! States -- none in this case
                              OutputData, ErrStat, ErrMsg )
   ! This routine takes an input dataset of type InputType which contains a position array of dimensions 3*n. It then calculates
   ! and returns the output dataset of type OutputType which contains a corresponding velocity array of dimensions 3*n. The input

   ! array contains XYZ triplets for each position of interest (first index is X/Y/Z for values 1/2/3, second index is the point
   ! number to evaluate). The returned values in the OutputData are similar with U/V/W for the first index of 1/2/3.
   !----------------------------------------------------------------------------------------------------

         ! Inputs / Outputs

      REAL( DbKi ),                       INTENT(IN   )  :: Time              ! Current simulation time in seconds
      TYPE( IfW_InputType ),              INTENT(IN   )  :: InputData         ! Inputs at Time
      TYPE( Ifw_ParameterType ),          INTENT(IN   )  :: ParamData         ! Parameters
      TYPE( IfW_ContinuousStateType ),    INTENT(IN   )  :: ContStates        ! Continuous states at Time
      TYPE( IfW_DiscreteStateType ),      INTENT(IN   )  :: DiscStates        ! Discrete states at Time
      TYPE( IfW_ConstraintStateType ),    INTENT(IN   )  :: ConstrStates      ! Constraint states at Time
      TYPE( IfW_OtherStateType ),         INTENT(INOUT)  :: OtherStates       ! Other/optimization states at Time
      TYPE( IfW_OutputType ),             INTENT(INOUT)  :: OutputData        ! Outputs computed at Time (IN for mesh reasons -- not used here)

      INTEGER( IntKi ),                   INTENT(  OUT)  :: ErrStat           ! Error status of the operation
      CHARACTER(*),                       INTENT(  OUT)  :: ErrMsg            ! Error message if ErrStat /= ErrID_None


         ! Local variables

      TYPE(IfW_HHWind_InputType)                         :: HH_InData         ! input positions.
      TYPE(IfW_HHWind_ContinuousStateType)               :: HH_ContStates     ! Unused
      TYPE(IfW_HHWind_DiscreteStateType)                 :: HH_DiscStates     ! Unused
      TYPE(IfW_HHWind_ConstraintStateType)               :: HH_ConstrStates   ! Unused
      TYPE(IfW_HHWind_OutputType)                        :: HH_OutData        ! output velocities

      TYPE(IfW_FFWind_InputType)                         :: FF_InData         ! input positions.
      TYPE(IfW_FFWind_ContinuousStateType)               :: FF_ContStates     ! Unused
      TYPE(IfW_FFWind_DiscreteStateType)                 :: FF_DiscStates     ! Unused
      TYPE(IfW_FFWind_ConstraintStateType)               :: FF_ConstrStates   ! Unused
      TYPE(IfW_FFWind_OutputType)                        :: FF_OutData        ! output velocities




         ! Sub modules use the InflIntrpOut derived type to store the wind information
!     TYPE(CT_Backgr)                                    :: BackGrndValues
!      TYPE(InflIntrpOut)                                 :: CTWindSpeed       ! U, V, W velocities to superimpose on background wind
!      TYPE(InflIntrpOut)                                 :: TempWindSpeed     ! U, V, W velocities returned
!      REAL(ReKi)                                         :: CTWindSpeed(3)     ! U, V, W velocities to superimpose on background wind
!      REAL(ReKi)                                         :: TempWindSpeed(3)   ! Temporary U, V, W velocities



         ! Temporary variables for error handling
      INTEGER(IntKi)                                     :: TmpErrStat
      CHARACTER(LEN(ErrMsg))                             :: TmpErrMsg      ! temporary error message


      CHARACTER(*), PARAMETER                            :: RoutineName = 'CalculateOutput'
      
         ! Initialize ErrStat
      ErrStat  = ErrID_None
      ErrMsg   = ""


         ! Allocate the velocity array to get out
      IF (.NOT. ALLOCATED(OutputData%Velocity) ) THEN
         CALL AllocAry( OutputData%Velocity, 3, SIZE(InputData%Position,2), &
                        "Velocity array returned from IfW_CalcOutput", TmpErrStat, TmpErrMsg )
            CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName )                  
            IF ( ErrStat >= AbortErrLev ) RETURN
      END IF
            
         ! Compute the wind velocities by stepping through all the data points and calling the appropriate GetWindSpeed routine
      SELECT CASE ( ParamData%WindFileType )
         CASE (HH_WindNumber)

               ! Allocate the position array to pass in
            CALL AllocAry( HH_InData%Position, 3, SIZE(InputData%Position,2), &
                           "Position grid for passing to IfW_HHWind_CalcOutput", TmpErrStat, TmpErrMsg )
            CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName )                  
            IF ( ErrStat >= AbortErrLev ) RETURN

               ! Copy positions over
            HH_InData%Position   = InputData%Position

            CALL  IfW_HHWind_CalcOutput(  Time,          HH_InData,     ParamData%HHWind,                         &
                                          HH_ContStates, HH_DiscStates, HH_ConstrStates,     OtherStates%HHWind,  &
                                          HH_OutData,    TmpErrStat,    TmpErrMsg)            
            
               ! Copy the velocities over
            OutputData%Velocity  = HH_OutData%Velocity

            CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName )                  
            IF ( ErrStat >= AbortErrLev ) RETURN
            

         CASE (FF_WindNumber)

               ! Allocate the position array to pass in
            CALL AllocAry( FF_InData%Position, 3, SIZE(InputData%Position,2), &
                           "Position grid for passing to IfW_FFWind_CalcOutput", TmpErrStat, TmpErrMsg )
            CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName )                  
            IF ( ErrStat >= AbortErrLev ) RETURN

            ! Copy positions over
            FF_InData%Position   = InputData%Position

            CALL  IfW_FFWind_CalcOutput(  Time,          FF_InData,     ParamData%FFWind,                         &
                                          FF_ContStates, FF_DiscStates, FF_ConstrStates,     OtherStates%FFWind,  &
                                          FF_OutData,    TmpErrStat,    TmpErrMsg)

               ! Copy the velocities over
            OutputData%Velocity  = FF_OutData%Velocity

            CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName )                  
            IF ( ErrStat >= AbortErrLev ) RETURN                       
            
            
!               OutputData%Velocity(:,PointCounter) = FF_GetWindSpeed(     Time, InputData%Position(:,PointCounter), ErrStat, ErrMsg)


!         CASE (UD_WindNumber)

!               OutputData%Velocity(:,PointCounter) = UsrWnd_GetWindSpeed( Time, InputData%Position(:,PointCounter), ErrStat )!, ErrMsg)


!         CASE (FD_WindNumber)

!               OutputData%Velocity(:,PointCounter) = FD_GetWindSpeed(     Time, InputData%Position(:,PointCounter), ErrStat )



!         CASE (HAWC_WindNumber)

!               OutputData%Velocity(:,PointCounter) = HW_GetWindSpeed(     Time, InputData%Position(:,PointCounter), ErrStat )



            ! If it isn't one of the above cases, we have a problem and won't be able to continue

         CASE DEFAULT

            CALL SetErrStat( ErrID_Fatal, 'Error: Undefined wind type.', ErrStat, ErrMsg, RoutineName )                  
            
            OutputData%Velocity(:,:) = 0.0
            RETURN

      END SELECT


         ! If we had a severe or fatal error, we need to make sure we zero out the result and return.
!BJJ: not sure we need this anymore... 

      !IF (ErrStat >= ErrID_Severe) THEN 
      !   OutputData%Velocity(:,:) = 0.0
      !   RETURN
      !
      !ELSE

            ! Add coherent turbulence to background wind

!         IF (ParamData%CT_Flag) THEN
!
!            DO PointCounter = 1, SIZE(InputData%Position, 2)
!
!               TempWindSpeed = CT_GetWindSpeed(     Time, InputData%Position(:,PointCounter), ErrStat, ErrMsg )
!
!                  ! Error Handling -- move ErrMsg inside CT_GetWindSPeed and simplify
!               IF (ErrStat >= ErrID_Severe) THEN
!                  ErrMsg   = 'IfW_CalcOutput: Error in CT_GetWindSpeed for point number '//TRIM(Num2LStr(PointCounter))
!                  EXIT        ! Exit the loop
!               ENDIF
!
!               OutputData%Velocity(:,PointCounter) = OutputData%Velocity(:,PointCounter) + TempWindSpeed
!
!            ENDDO
!
!               ! If something went badly wrong, Return
!            IF (ErrStat >= ErrID_Severe ) RETURN
!
!         ENDIF
!
      !ENDIF



END SUBROUTINE CalculateOutput                             
!------------------------------------------------------------------------------------    
SUBROUTINE InflowWind_GetMean( StartTime, EndTime, delta_time, InputPosition, MeanVelocity, &
                               p, x, xd, z, OtherState, ErrStat, ErrMsg )
! this routine calculates the mean wind speed 
!----------------------------------------------------------------------------------------------------
      ! Inputs / Outputs
   REAL(DbKi),                         INTENT(IN   )  :: StartTime
   REAL(DbKi),                         INTENT(IN   )  :: EndTime
   REAL(DbKi),                         INTENT(IN   )  :: delta_time
   REAL(ReKi),                         INTENT(IN   )  :: InputPosition(3)  ! X, Y, Z positions
   REAL(ReKi),                         INTENT(  OUT)  :: MeanVelocity(3)   ! at InputPosition
      
   TYPE( IfW_ParameterType ),          INTENT(IN   )  :: p                 ! Parameters
   TYPE( IfW_ContinuousStateType ),    INTENT(IN   )  :: x                 ! Continuous states at Time
   TYPE( IfW_DiscreteStateType ),      INTENT(IN   )  :: xd                ! Discrete states at Time
   TYPE( IfW_ConstraintStateType ),    INTENT(IN   )  :: z                 ! Constraint states at Time
   TYPE( IfW_OtherStateType ),         INTENT(INOUT)  :: OtherState        ! Other/optimization states at Time
      
      
   INTEGER( IntKi ),                   INTENT(  OUT)  :: ErrStat           ! Error status of the operation
   CHARACTER(*),                       INTENT(  OUT)  :: ErrMsg            ! Error message if ErrStat /= ErrID_None


      ! local variables
   REAL(DbKi)                                         :: Time
   REAL(DbKi)                                         :: SumVel(3)
   INTEGER(IntKi)                                     :: I
   INTEGER(IntKi)                                     :: Nt
   INTEGER(IntKi)                                     :: ErrStat2
   CHARACTER(LEN(ErrMsg))                             :: ErrMsg2

   
   CHARACTER(*), PARAMETER                            :: RoutineName = 'InflowWind_GetMean'
      
   TYPE(IfW_InputType)                                :: Input                ! position where wind speed should be returned
   TYPE(IfW_OutputType)                               :: Output               ! velocity at InputPosition
       
       ! main body:
              
   IF ( EqualRealNos(delta_time,0.0_DbKi) ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = RoutineName//":delta time must be non-zero."
      RETURN
   END IF
       
   ErrStat   = ErrID_None
   ErrMsg    = ""
   
   
      ! allocate arrays to compute outputs
   CALL AllocAry(Input%Position, 3,1, 'Input%Position',ErrStat2, ErrMsg2)
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

   CALL AllocAry(Output%Velocity, 3,1, 'Output%Position',ErrStat2, ErrMsg2)
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN      
   END IF
              

   Nt = (EndTime - StartTime) / delta_time
   SumVel = 0.0
   Input%Position(:,1) = InputPosition      

   DO I=1,Nt

      Time = StartTime + (I-1)*delta_time

   CALL CalculateOutput( Time, Input, p, x, xd, z, OtherState, Output, ErrStat2, ErrMsg2 )      
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )                
         
      IF ( ErrStat >= AbortErrLev ) THEN
         MeanVelocity = SumVel / REAL(I-1, ReKi)
         CALL Cleanup()
         RETURN
      ELSE
         SumVel = SumVel + Output%Velocity(:,1)
      END IF

   END DO

   MeanVelocity = SumVel / REAL(Nt, ReKi)
   CALL Cleanup()
   RETURN
   
CONTAINS
!............................
   SUBROUTINE Cleanup()
      
      IF (ALLOCATED(Input%Position)) DEALLOCATE(Input%Position)
      IF (ALLOCATED(Output%Velocity)) DEALLOCATE(Output%Velocity)
   
   END SUBROUTINE Cleanup   
!............................
END SUBROUTINE InflowWind_GetMean
!**********************************************************************************************************************************
                              
END MODULE InflowWind_Subs




!!    !====================================================================================================
!!    FUNCTION InflowWind_GetStdDev(StartTime, EndTime, delta_time, InputPosition,  ErrStat )
!!    !  This function returns the mean wind speed (mean, std, TI, etc)
!!    !----------------------------------------------------------------------------------------------------
!!
!!          ! passed variables
!!       REAL(ReKi),       INTENT(IN)  :: StartTime
!!       REAL(ReKi),       INTENT(IN)  :: EndTime
!!       REAL(ReKi),       INTENT(IN)  :: delta_time
!!       REAL(ReKi),       INTENT(IN)  :: InputPosition(3)        ! X, Y, Z positions
!!       INTEGER,          INTENT(OUT) :: ErrStat                 ! Return 0 if no error; non-zero otherwise
!!
!!          ! function definition
!!       REAL(ReKi)                    :: InflowWind_GetStdDev(3)    ! STD U, V, W
!!
!!          ! local variables
!!       REAL(ReKi)                    :: Time
!!       REAL(ReKi), ALLOCATABLE       :: Velocity(:,:)
!!       REAL(DbKi)                    :: SumAry(3)
!!       REAL(DbKi)                    :: MeanVel(3)
!!       INTEGER                       :: I
!!       INTEGER                       :: Nt
!!
!!       TYPE(InflIntrpOut)            :: NewVelocity             ! U, V, W velocities
!!
!!
!!       !-------------------------------------------------------------------------------------------------
!!       ! Initialize
!!       !-------------------------------------------------------------------------------------------------
!!
!!       InflowWind_GetStdDev(:) = 0.0
!!
!!       Nt = (EndTime - StartTime) / delta_time
!!
!!       IF ( Nt < 2 ) RETURN    ! StdDev is 0
!!
!!
!!       IF (.NOT. ALLOCATED(Velocity)) THEN
!!    !      CALL AllocAry( Velocity, 3, Nt, 'StdDev velocity', ErrStat)
!!          ALLOCATE ( Velocity(3, Nt), STAT=ErrStat )
!!
!!          IF ( ErrStat /= 0 )  THEN
!!             CALL WrScr ( ' Error allocating memory for the StdDev velocity array.' )
!!             RETURN
!!          END IF
!!       END IF
!!
!!
!!       !-------------------------------------------------------------------------------------------------
!!       ! Calculate the mean, storing the velocity for later
!!       !-------------------------------------------------------------------------------------------------
!!       SumAry(:) = 0.0
!!
!!       DO I=1,Nt
!!
!!          Time = StartTime + (I-1)*delta_time
!!
!!          NewVelocity = InflowWind_GetVelocity(Time, InputPosition, ErrStat)
!!          IF ( ErrStat /= 0 ) RETURN
!!          Velocity(:,I) = NewVelocity%Velocity(:)
!!          SumAry(:)     = SumAry(:) + NewVelocity%Velocity(:)
!!
!!       END DO
!!
!!       MeanVel(:) = SumAry(:) / REAL(Nt, ReKi)
!!
!!
!!       !-------------------------------------------------------------------------------------------------
!!       ! Calculate the standard deviation
!!       !-------------------------------------------------------------------------------------------------
!!       SumAry(:) = 0.0
!!
!!       DO I=1,Nt
!!
!!          SumAry(:) = SumAry(:) + ( Velocity(:,I) - MeanVel(:) )**2
!!
!!       END DO ! I
!!
!!       InflowWind_GetStdDev(:) = SQRT( SumAry(:) / ( Nt - 1 ) )
!!
!!
!!       !-------------------------------------------------------------------------------------------------
!!       ! Deallocate
!!       !-------------------------------------------------------------------------------------------------
!!       IF ( ALLOCATED(Velocity) ) DEALLOCATE( Velocity )
!!
!!
!!    END FUNCTION InflowWind_GetStdDev
!!    !====================================================================================================
!!    FUNCTION InflowWind_GetTI(StartTime, EndTime, delta_time, InputPosition,  ErrStat )
!!    !  This function returns the TI of the wind speed.  It's basically a copy of InflowWind_GetStdDev,
!!    !  except the return value is divided by the mean U-component wind speed.
!!    !----------------------------------------------------------------------------------------------------
!!
!!          ! passed variables
!!       REAL(ReKi),       INTENT(IN)  :: StartTime
!!       REAL(ReKi),       INTENT(IN)  :: EndTime
!!       REAL(ReKi),       INTENT(IN)  :: delta_time
!!       REAL(ReKi),       INTENT(IN)  :: InputPosition(3)        ! X, Y, Z positions
!!       INTEGER,          INTENT(OUT) :: ErrStat                 ! Return 0 if no error; non-zero otherwise
!!
!!          ! function definition
!!       REAL(ReKi)                    :: InflowWind_GetTI(3)        ! TI U, V, W
!!
!!          ! local variables
!!       REAL(ReKi)                    :: Time
!!       REAL(ReKi), ALLOCATABLE       :: Velocity(:,:)
!!       REAL(DbKi)                    :: SumAry(3)
!!       REAL(DbKi)                    :: MeanVel(3)
!!       INTEGER                       :: I
!!       INTEGER                       :: Nt
!!
!!       TYPE(InflIntrpOut)            :: NewVelocity             ! U, V, W velocities
!!
!!
!!       !-------------------------------------------------------------------------------------------------
!!       ! Initialize
!!       !-------------------------------------------------------------------------------------------------
!!
!!       InflowWind_GetTI(:) = 0.0
!!
!!       Nt = (EndTime - StartTime) / delta_time
!!
!!       IF ( Nt < 2 ) RETURN    ! StdDev is 0
!!
!!
!!       IF (.NOT. ALLOCATED(Velocity)) THEN
!!    !      CALL AllocAry( Velocity, 3, Nt, 'TI velocity', ErrStat)
!!          ALLOCATE ( Velocity(3, Nt), STAT=ErrStat )
!!
!!          IF ( ErrStat /= 0 )  THEN
!!             CALL WrScr ( ' Error allocating memory for the TI velocity array.' )
!!             RETURN
!!          END IF
!!       END IF
!!
!!
!!       !-------------------------------------------------------------------------------------------------
!!       ! Calculate the mean, storing the velocity for later
!!       !-------------------------------------------------------------------------------------------------
!!       SumAry(:) = 0.0
!!
!!       DO I=1,Nt
!!
!!          Time = StartTime + (I-1)*delta_time
!!
!!          NewVelocity = InflowWind_GetVelocity(Time, InputPosition, ErrStat)
!!          IF ( ErrStat /= 0 ) RETURN
!!          Velocity(:,I) = NewVelocity%Velocity(:)
!!          SumAry(:)     = SumAry(:) + NewVelocity%Velocity(:)
!!
!!       END DO
!!
!!       MeanVel(:) = SumAry(:) / REAL(Nt, ReKi)
!!
!!       IF ( ABS(MeanVel(1)) <= EPSILON(MeanVel(1)) ) THEN
!!          CALL WrScr( ' Wind speed is small in InflowWind_GetTI(). TI is undefined.' )
!!          ErrStat = 1
!!          RETURN
!!       END IF
!!
!!       !-------------------------------------------------------------------------------------------------
!!       ! Calculate the standard deviation
!!       !-------------------------------------------------------------------------------------------------
!!       SumAry(:) = 0.0
!!
!!       DO I=1,Nt
!!
!!          SumAry(:) = SumAry(:) + ( Velocity(:,I) - MeanVel(:) )**2
!!
!!       END DO ! I
!!
!!       InflowWind_GetTI(:) = SQRT( SumAry(:) / ( Nt - 1 ) ) / MeanVel(1)
!!
!!
!!       !-------------------------------------------------------------------------------------------------
!!       ! Deallocate
!!       !-------------------------------------------------------------------------------------------------
!!       IF ( ALLOCATED(Velocity) ) DEALLOCATE( Velocity )
!!
!!
!!    END FUNCTION InflowWind_GetTI
