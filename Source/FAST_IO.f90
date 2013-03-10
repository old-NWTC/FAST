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
SUBROUTINE ChckOutLst(OutList, p_ED, y, ErrStat, ErrMsg )


   ! This routine checks to see if any inputted output channels (stored
   !    in the OutList(:)) are ill-conditioned (and if so, FAST Aborts)
   !    and assigns the settings for OutParam(:) (i.e, the
   !    index, name, and units of the output channels, WriteOutput(:)).


   USE                             General
   
   
   IMPLICIT                        NONE


      ! Passed variables
      
   CHARACTER(OutStrLen),      INTENT(IN)     :: OutList(:)                        ! The list out user-requested outputs
   TYPE(ED_ParameterType),    INTENT(INOUT)  :: p_ED                            ! The parameters of the structural dynamics module
   INTEGER(IntKi),            INTENT(OUT)    :: ErrStat                           ! The error status code; If not present code aborts
   CHARACTER(*),              INTENT(OUT)    :: ErrMsg                            ! The error message, if an error occurred 
   TYPE(ED_OutputType),       INTENT(INOUT)  :: y                                 ! System outputs of the structural dynamics module
   
      
   
      ! Local variables.

   INTEGER                      :: I                                               ! Generic loop-counting index.
   INTEGER                      :: J                                               ! Generic loop-counting index.
   INTEGER                      :: INDX                                            ! Index for valid arrays.
   INTEGER                      :: Sttus                                           ! Status returned from an allocation request.
   
   LOGICAL                      :: CheckOutListAgain                               ! Flag used to determine if output parameter starting with "M" is valid (or the negative of another parameter)
   
   CHARACTER(OutStrLen)         :: OutListTmp                                      ! A string to temporarily hold OutList(I)
   

! NOTE: The following lines of code were generated by a Matlab script called "Write_ChckOutLst.m"
!      using the parameters listed in the "OutListParameters.xlsx" Excel file. Any changes to these 
!      lines should be modified in the Matlab script and/or Excel worksheet as necessary. 
! This code was generated by Write_ChckOutLst.m at 14-Feb-2013 13:25:07.
   CHARACTER(OutStrLenM1), PARAMETER  :: ValidParamAry(1019) =  (/ &                         ! This lists the names of the allowed parameters, which must be sorted alphabetically
                               "AZIMUTH  ","BLDPITCH1","BLDPITCH2","BLDPITCH3","BLPITCH1 ","BLPITCH2 ","BLPITCH3 ", &
                               "CTHRSTARM","CTHRSTAZM","CTHRSTRAD","GENACCEL ","GENCP    ","GENCQ    ","GENPWR   ", &
                               "GENSPEED ","GENTQ    ","HORWINDV ","HORWNDDIR","HSSBRTQ  ","HSSHFTA  ","HSSHFTCP ", &
                               "HSSHFTCQ ","HSSHFTPWR","HSSHFTTQ ","HSSHFTV  ","IPDEFL1  ","IPDEFL2  ","IPDEFL3  ", &
                               "LSSGAGA  ","LSSGAGAXA","LSSGAGAXS","LSSGAGFXA","LSSGAGFXS","LSSGAGFYA","LSSGAGFYS", &
                               "LSSGAGFZA","LSSGAGFZS","LSSGAGMXA","LSSGAGMXS","LSSGAGMYA","LSSGAGMYS","LSSGAGMZA", &
                               "LSSGAGMZS","LSSGAGP  ","LSSGAGPXA","LSSGAGPXS","LSSGAGV  ","LSSGAGVXA","LSSGAGVXS", &
                               "LSSHFTCP ","LSSHFTCQ ","LSSHFTCT ","LSSHFTFXA","LSSHFTFXS","LSSHFTFYA","LSSHFTFYS", &
                               "LSSHFTFZA","LSSHFTFZS","LSSHFTMXA","LSSHFTMXS","LSSHFTPWR","LSSHFTTQ ","LSSTIPA  ", &
                               "LSSTIPAXA","LSSTIPAXS","LSSTIPMYA","LSSTIPMYS","LSSTIPMZA","LSSTIPMZS","LSSTIPP  ", &
                               "LSSTIPPXA","LSSTIPPXS","LSSTIPV  ","LSSTIPVXA","LSSTIPVXS","NACYAW   ","NACYAWA  ", &
                               "NACYAWERR","NACYAWP  ","NACYAWV  ","NCIMURAXS","NCIMURAYS","NCIMURAZS","NCIMURVXS", &
                               "NCIMURVYS","NCIMURVZS","NCIMUTAXS","NCIMUTAYS","NCIMUTAZS","NCIMUTVXS","NCIMUTVYS", &
                               "NCIMUTVZS","OOPDEFL1 ","OOPDEFL2 ","OOPDEFL3 ","PTCHDEFL1","PTCHDEFL2","PTCHDEFL3", &
                               "PTCHPMZB1","PTCHPMZB2","PTCHPMZB3","PTCHPMZC1","PTCHPMZC2","PTCHPMZC3","PTFMFXI  ", &
                               "PTFMFXT  ","PTFMFYI  ","PTFMFYT  ","PTFMFZI  ","PTFMFZT  ","PTFMHEAVE","PTFMMXI  ", &
                               "PTFMMXT  ","PTFMMYI  ","PTFMMYT  ","PTFMMZI  ","PTFMMZT  ","PTFMPITCH","PTFMRAXI ", &
                               "PTFMRAXT ","PTFMRAYI ","PTFMRAYT ","PTFMRAZI ","PTFMRAZT ","PTFMRDXI ","PTFMRDYI ", &
                               "PTFMRDZI ","PTFMROLL ","PTFMRVXI ","PTFMRVXT ","PTFMRVYI ","PTFMRVYT ","PTFMRVZI ", &
                               "PTFMRVZT ","PTFMSURGE","PTFMSWAY ","PTFMTAXI ","PTFMTAXT ","PTFMTAYI ","PTFMTAYT ", &
                               "PTFMTAZI ","PTFMTAZT ","PTFMTDXI ","PTFMTDXT ","PTFMTDYI ","PTFMTDYT ","PTFMTDZI ", &
                               "PTFMTDZT ","PTFMTVXI ","PTFMTVXT ","PTFMTVYI ","PTFMTVYT ","PTFMTVZI ","PTFMTVZT ", &
                               "PTFMYAW  ","QD2_B1E1 ","QD2_B1F1 ","QD2_B1F2 ","QD2_B2E1 ","QD2_B2F1 ","QD2_B2F2 ", &
                               "QD2_B3E1 ","QD2_B3F1 ","QD2_B3F2 ","QD2_DRTR ","QD2_GEAZ ","QD2_HV   ","QD2_P    ", &
                               "QD2_R    ","QD2_RFRL ","QD2_SG   ","QD2_SW   ","QD2_TEET ","QD2_TFA1 ","QD2_TFA2 ", &
                               "QD2_TFRL ","QD2_TSS1 ","QD2_TSS2 ","QD2_Y    ","QD2_YAW  ","QD_B1E1  ","QD_B1F1  ", &
                               "QD_B1F2  ","QD_B2E1  ","QD_B2F1  ","QD_B2F2  ","QD_B3E1  ","QD_B3F1  ","QD_B3F2  ", &
                               "QD_DRTR  ","QD_GEAZ  ","QD_HV    ","QD_P     ","QD_R     ","QD_RFRL  ","QD_SG    ", &
                               "QD_SW    ","QD_TEET  ","QD_TFA1  ","QD_TFA2  ","QD_TFRL  ","QD_TSS1  ","QD_TSS2  ", &
                               "QD_Y     ","QD_YAW   ","Q_B1E1   ","Q_B1F1   ","Q_B1F2   ","Q_B2E1   ","Q_B2F1   ", &
                               "Q_B2F2   ","Q_B3E1   ","Q_B3F1   ","Q_B3F2   ","Q_DRTR   ","Q_GEAZ   ","Q_HV     ", &
                               "Q_P      ","Q_R      ","Q_RFRL   ","Q_SG     ","Q_SW     ","Q_TEET   ","Q_TFA1   ", &
                               "Q_TFA2   ","Q_TFRL   ","Q_TSS1   ","Q_TSS2   ","Q_Y      ","Q_YAW    ","RFRLBRM  ", &
                               "ROLLDEFL1","ROLLDEFL2","ROLLDEFL3","ROOTFXB1 ","ROOTFXB2 ","ROOTFXB3 ","ROOTFXC1 ", &
                               "ROOTFXC2 ","ROOTFXC3 ","ROOTFYB1 ","ROOTFYB2 ","ROOTFYB3 ","ROOTFYC1 ","ROOTFYC2 ", &
                               "ROOTFYC3 ","ROOTFZB1 ","ROOTFZB2 ","ROOTFZB3 ","ROOTFZC1 ","ROOTFZC2 ","ROOTFZC3 ", &
                               "ROOTMEDG1","ROOTMEDG2","ROOTMEDG3","ROOTMFLP1","ROOTMFLP2","ROOTMFLP3","ROOTMIP1 ", &
                               "ROOTMIP2 ","ROOTMIP3 ","ROOTMOOP1","ROOTMOOP2","ROOTMOOP3","ROOTMXB1 ","ROOTMXB2 ", &
                               "ROOTMXB3 ","ROOTMXC1 ","ROOTMXC2 ","ROOTMXC3 ","ROOTMYB1 ","ROOTMYB2 ","ROOTMYB3 ", &
                               "ROOTMYC1 ","ROOTMYC2 ","ROOTMYC3 ","ROOTMZB1 ","ROOTMZB2 ","ROOTMZB3 ","ROOTMZC1 ", &
                               "ROOTMZC2 ","ROOTMZC3 ","ROTACCEL ","ROTCP    ","ROTCQ    ","ROTCT    ","ROTFURL  ", &
                               "ROTFURLA ","ROTFURLP ","ROTFURLV ","ROTPWR   ","ROTSPEED ","ROTTEETA ","ROTTEETP ", &
                               "ROTTEETV ","ROTTHRUST","ROTTORQ  ","SPN1ALXB1","SPN1ALXB2","SPN1ALXB3","SPN1ALYB1", &
                               "SPN1ALYB2","SPN1ALYB3","SPN1ALZB1","SPN1ALZB2","SPN1ALZB3","SPN1FLXB1","SPN1FLXB2", &
                               "SPN1FLXB3","SPN1FLYB1","SPN1FLYB2","SPN1FLYB3","SPN1FLZB1","SPN1FLZB2","SPN1FLZB3", &
                               "SPN1MLXB1","SPN1MLXB2","SPN1MLXB3","SPN1MLYB1","SPN1MLYB2","SPN1MLYB3","SPN1MLZB1", &
                               "SPN1MLZB2","SPN1MLZB3","SPN1RDXB1","SPN1RDXB2","SPN1RDXB3","SPN1RDYB1","SPN1RDYB2", &
                               "SPN1RDYB3","SPN1RDZB1","SPN1RDZB2","SPN1RDZB3","SPN1TDXB1","SPN1TDXB2","SPN1TDXB3", &
                               "SPN1TDYB1","SPN1TDYB2","SPN1TDYB3","SPN1TDZB1","SPN1TDZB2","SPN1TDZB3","SPN2ALXB1", &
                               "SPN2ALXB2","SPN2ALXB3","SPN2ALYB1","SPN2ALYB2","SPN2ALYB3","SPN2ALZB1","SPN2ALZB2", &
                               "SPN2ALZB3","SPN2FLXB1","SPN2FLXB2","SPN2FLXB3","SPN2FLYB1","SPN2FLYB2","SPN2FLYB3", &
                               "SPN2FLZB1","SPN2FLZB2","SPN2FLZB3","SPN2MLXB1","SPN2MLXB2","SPN2MLXB3","SPN2MLYB1", &
                               "SPN2MLYB2","SPN2MLYB3","SPN2MLZB1","SPN2MLZB2","SPN2MLZB3","SPN2RDXB1","SPN2RDXB2", &
                               "SPN2RDXB3","SPN2RDYB1","SPN2RDYB2","SPN2RDYB3","SPN2RDZB1","SPN2RDZB2","SPN2RDZB3", &
                               "SPN2TDXB1","SPN2TDXB2","SPN2TDXB3","SPN2TDYB1","SPN2TDYB2","SPN2TDYB3","SPN2TDZB1", &
                               "SPN2TDZB2","SPN2TDZB3","SPN3ALXB1","SPN3ALXB2","SPN3ALXB3","SPN3ALYB1","SPN3ALYB2", &
                               "SPN3ALYB3","SPN3ALZB1","SPN3ALZB2","SPN3ALZB3","SPN3FLXB1","SPN3FLXB2","SPN3FLXB3", &
                               "SPN3FLYB1","SPN3FLYB2","SPN3FLYB3","SPN3FLZB1","SPN3FLZB2","SPN3FLZB3","SPN3MLXB1", &
                               "SPN3MLXB2","SPN3MLXB3","SPN3MLYB1","SPN3MLYB2","SPN3MLYB3","SPN3MLZB1","SPN3MLZB2", &
                               "SPN3MLZB3","SPN3RDXB1","SPN3RDXB2","SPN3RDXB3","SPN3RDYB1","SPN3RDYB2","SPN3RDYB3", &
                               "SPN3RDZB1","SPN3RDZB2","SPN3RDZB3","SPN3TDXB1","SPN3TDXB2","SPN3TDXB3","SPN3TDYB1", &
                               "SPN3TDYB2","SPN3TDYB3","SPN3TDZB1","SPN3TDZB2","SPN3TDZB3","SPN4ALXB1","SPN4ALXB2", &
                               "SPN4ALXB3","SPN4ALYB1","SPN4ALYB2","SPN4ALYB3","SPN4ALZB1","SPN4ALZB2","SPN4ALZB3", &
                               "SPN4FLXB1","SPN4FLXB2","SPN4FLXB3","SPN4FLYB1","SPN4FLYB2","SPN4FLYB3","SPN4FLZB1", &
                               "SPN4FLZB2","SPN4FLZB3","SPN4MLXB1","SPN4MLXB2","SPN4MLXB3","SPN4MLYB1","SPN4MLYB2", &
                               "SPN4MLYB3","SPN4MLZB1","SPN4MLZB2","SPN4MLZB3","SPN4RDXB1","SPN4RDXB2","SPN4RDXB3", &
                               "SPN4RDYB1","SPN4RDYB2","SPN4RDYB3","SPN4RDZB1","SPN4RDZB2","SPN4RDZB3","SPN4TDXB1", &
                               "SPN4TDXB2","SPN4TDXB3","SPN4TDYB1","SPN4TDYB2","SPN4TDYB3","SPN4TDZB1","SPN4TDZB2", &
                               "SPN4TDZB3","SPN5ALXB1","SPN5ALXB2","SPN5ALXB3","SPN5ALYB1","SPN5ALYB2","SPN5ALYB3", &
                               "SPN5ALZB1","SPN5ALZB2","SPN5ALZB3","SPN5FLXB1","SPN5FLXB2","SPN5FLXB3","SPN5FLYB1", &
                               "SPN5FLYB2","SPN5FLYB3","SPN5FLZB1","SPN5FLZB2","SPN5FLZB3","SPN5MLXB1","SPN5MLXB2", &
                               "SPN5MLXB3","SPN5MLYB1","SPN5MLYB2","SPN5MLYB3","SPN5MLZB1","SPN5MLZB2","SPN5MLZB3", &
                               "SPN5RDXB1","SPN5RDXB2","SPN5RDXB3","SPN5RDYB1","SPN5RDYB2","SPN5RDYB3","SPN5RDZB1", &
                               "SPN5RDZB2","SPN5RDZB3","SPN5TDXB1","SPN5TDXB2","SPN5TDXB3","SPN5TDYB1","SPN5TDYB2", &
                               "SPN5TDYB3","SPN5TDZB1","SPN5TDZB2","SPN5TDZB3","SPN6ALXB1","SPN6ALXB2","SPN6ALXB3", &
                               "SPN6ALYB1","SPN6ALYB2","SPN6ALYB3","SPN6ALZB1","SPN6ALZB2","SPN6ALZB3","SPN6FLXB1", &
                               "SPN6FLXB2","SPN6FLXB3","SPN6FLYB1","SPN6FLYB2","SPN6FLYB3","SPN6FLZB1","SPN6FLZB2", &
                               "SPN6FLZB3","SPN6MLXB1","SPN6MLXB2","SPN6MLXB3","SPN6MLYB1","SPN6MLYB2","SPN6MLYB3", &
                               "SPN6MLZB1","SPN6MLZB2","SPN6MLZB3","SPN6RDXB1","SPN6RDXB2","SPN6RDXB3","SPN6RDYB1", &
                               "SPN6RDYB2","SPN6RDYB3","SPN6RDZB1","SPN6RDZB2","SPN6RDZB3","SPN6TDXB1","SPN6TDXB2", &
                               "SPN6TDXB3","SPN6TDYB1","SPN6TDYB2","SPN6TDYB3","SPN6TDZB1","SPN6TDZB2","SPN6TDZB3", &
                               "SPN7ALXB1","SPN7ALXB2","SPN7ALXB3","SPN7ALYB1","SPN7ALYB2","SPN7ALYB3","SPN7ALZB1", &
                               "SPN7ALZB2","SPN7ALZB3","SPN7FLXB1","SPN7FLXB2","SPN7FLXB3","SPN7FLYB1","SPN7FLYB2", &
                               "SPN7FLYB3","SPN7FLZB1","SPN7FLZB2","SPN7FLZB3","SPN7MLXB1","SPN7MLXB2","SPN7MLXB3", &
                               "SPN7MLYB1","SPN7MLYB2","SPN7MLYB3","SPN7MLZB1","SPN7MLZB2","SPN7MLZB3","SPN7RDXB1", &
                               "SPN7RDXB2","SPN7RDXB3","SPN7RDYB1","SPN7RDYB2","SPN7RDYB3","SPN7RDZB1","SPN7RDZB2", &
                               "SPN7RDZB3","SPN7TDXB1","SPN7TDXB2","SPN7TDXB3","SPN7TDYB1","SPN7TDYB2","SPN7TDYB3", &
                               "SPN7TDZB1","SPN7TDZB2","SPN7TDZB3","SPN8ALXB1","SPN8ALXB2","SPN8ALXB3","SPN8ALYB1", &
                               "SPN8ALYB2","SPN8ALYB3","SPN8ALZB1","SPN8ALZB2","SPN8ALZB3","SPN8FLXB1","SPN8FLXB2", &
                               "SPN8FLXB3","SPN8FLYB1","SPN8FLYB2","SPN8FLYB3","SPN8FLZB1","SPN8FLZB2","SPN8FLZB3", &
                               "SPN8MLXB1","SPN8MLXB2","SPN8MLXB3","SPN8MLYB1","SPN8MLYB2","SPN8MLYB3","SPN8MLZB1", &
                               "SPN8MLZB2","SPN8MLZB3","SPN8RDXB1","SPN8RDXB2","SPN8RDXB3","SPN8RDYB1","SPN8RDYB2", &
                               "SPN8RDYB3","SPN8RDZB1","SPN8RDZB2","SPN8RDZB3","SPN8TDXB1","SPN8TDXB2","SPN8TDXB3", &
                               "SPN8TDYB1","SPN8TDYB2","SPN8TDYB3","SPN8TDZB1","SPN8TDZB2","SPN8TDZB3","SPN9ALXB1", &
                               "SPN9ALXB2","SPN9ALXB3","SPN9ALYB1","SPN9ALYB2","SPN9ALYB3","SPN9ALZB1","SPN9ALZB2", &
                               "SPN9ALZB3","SPN9FLXB1","SPN9FLXB2","SPN9FLXB3","SPN9FLYB1","SPN9FLYB2","SPN9FLYB3", &
                               "SPN9FLZB1","SPN9FLZB2","SPN9FLZB3","SPN9MLXB1","SPN9MLXB2","SPN9MLXB3","SPN9MLYB1", &
                               "SPN9MLYB2","SPN9MLYB3","SPN9MLZB1","SPN9MLZB2","SPN9MLZB3","SPN9RDXB1","SPN9RDXB2", &
                               "SPN9RDXB3","SPN9RDYB1","SPN9RDYB2","SPN9RDYB3","SPN9RDZB1","SPN9RDZB2","SPN9RDZB3", &
                               "SPN9TDXB1","SPN9TDXB2","SPN9TDXB3","SPN9TDYB1","SPN9TDYB2","SPN9TDYB3","SPN9TDZB1", &
                               "SPN9TDZB2","SPN9TDZB3","TAILFURL ","TAILFURLA","TAILFURLP","TAILFURLV","TEETAYA  ", &
                               "TEETDEFL ","TEETPYA  ","TEETVYA  ","TFINALPHA","TFINCDRAG","TFINCLIFT","TFINCPFX ", &
                               "TFINCPFY ","TFINDNPRS","TFRLBRM  ","TIP2TWR1 ","TIP2TWR2 ","TIP2TWR3 ","TIPALXB1 ", &
                               "TIPALXB2 ","TIPALXB3 ","TIPALYB1 ","TIPALYB2 ","TIPALYB3 ","TIPALZB1 ","TIPALZB2 ", &
                               "TIPALZB3 ","TIPCLRNC1","TIPCLRNC2","TIPCLRNC3","TIPDXB1  ","TIPDXB2  ","TIPDXB3  ", &
                               "TIPDXC1  ","TIPDXC2  ","TIPDXC3  ","TIPDYB1  ","TIPDYB2  ","TIPDYB3  ","TIPDYC1  ", &
                               "TIPDYC2  ","TIPDYC3  ","TIPDZB1  ","TIPDZB2  ","TIPDZB3  ","TIPDZC1  ","TIPDZC2  ", &
                               "TIPDZC3  ","TIPRDXB1 ","TIPRDXB2 ","TIPRDXB3 ","TIPRDYB1 ","TIPRDYB2 ","TIPRDYB3 ", &
                               "TIPRDZB1 ","TIPRDZB2 ","TIPRDZB3 ","TIPRDZC1 ","TIPRDZC2 ","TIPRDZC3 ","TIPSPDRAT", &
                               "TOTWINDV ","TSR      ","TTDSPAX  ","TTDSPFA  ","TTDSPPTCH","TTDSPROLL","TTDSPSS  ", &
                               "TTDSPTWST","TWHT1ALXT","TWHT1ALYT","TWHT1ALZT","TWHT1FLXT","TWHT1FLYT","TWHT1FLZT", &
                               "TWHT1MLXT","TWHT1MLYT","TWHT1MLZT","TWHT1RDXT","TWHT1RDYT","TWHT1RDZT","TWHT1RPXI", &
                               "TWHT1RPYI","TWHT1RPZI","TWHT1TDXT","TWHT1TDYT","TWHT1TDZT","TWHT1TPXI","TWHT1TPYI", &
                               "TWHT1TPZI","TWHT2ALXT","TWHT2ALYT","TWHT2ALZT","TWHT2FLXT","TWHT2FLYT","TWHT2FLZT", &
                               "TWHT2MLXT","TWHT2MLYT","TWHT2MLZT","TWHT2RDXT","TWHT2RDYT","TWHT2RDZT","TWHT2RPXI", &
                               "TWHT2RPYI","TWHT2RPZI","TWHT2TDXT","TWHT2TDYT","TWHT2TDZT","TWHT2TPXI","TWHT2TPYI", &
                               "TWHT2TPZI","TWHT3ALXT","TWHT3ALYT","TWHT3ALZT","TWHT3FLXT","TWHT3FLYT","TWHT3FLZT", &
                               "TWHT3MLXT","TWHT3MLYT","TWHT3MLZT","TWHT3RDXT","TWHT3RDYT","TWHT3RDZT","TWHT3RPXI", &
                               "TWHT3RPYI","TWHT3RPZI","TWHT3TDXT","TWHT3TDYT","TWHT3TDZT","TWHT3TPXI","TWHT3TPYI", &
                               "TWHT3TPZI","TWHT4ALXT","TWHT4ALYT","TWHT4ALZT","TWHT4FLXT","TWHT4FLYT","TWHT4FLZT", &
                               "TWHT4MLXT","TWHT4MLYT","TWHT4MLZT","TWHT4RDXT","TWHT4RDYT","TWHT4RDZT","TWHT4RPXI", &
                               "TWHT4RPYI","TWHT4RPZI","TWHT4TDXT","TWHT4TDYT","TWHT4TDZT","TWHT4TPXI","TWHT4TPYI", &
                               "TWHT4TPZI","TWHT5ALXT","TWHT5ALYT","TWHT5ALZT","TWHT5FLXT","TWHT5FLYT","TWHT5FLZT", &
                               "TWHT5MLXT","TWHT5MLYT","TWHT5MLZT","TWHT5RDXT","TWHT5RDYT","TWHT5RDZT","TWHT5RPXI", &
                               "TWHT5RPYI","TWHT5RPZI","TWHT5TDXT","TWHT5TDYT","TWHT5TDZT","TWHT5TPXI","TWHT5TPYI", &
                               "TWHT5TPZI","TWHT6ALXT","TWHT6ALYT","TWHT6ALZT","TWHT6FLXT","TWHT6FLYT","TWHT6FLZT", &
                               "TWHT6MLXT","TWHT6MLYT","TWHT6MLZT","TWHT6RDXT","TWHT6RDYT","TWHT6RDZT","TWHT6RPXI", &
                               "TWHT6RPYI","TWHT6RPZI","TWHT6TDXT","TWHT6TDYT","TWHT6TDZT","TWHT6TPXI","TWHT6TPYI", &
                               "TWHT6TPZI","TWHT7ALXT","TWHT7ALYT","TWHT7ALZT","TWHT7FLXT","TWHT7FLYT","TWHT7FLZT", &
                               "TWHT7MLXT","TWHT7MLYT","TWHT7MLZT","TWHT7RDXT","TWHT7RDYT","TWHT7RDZT","TWHT7RPXI", &
                               "TWHT7RPYI","TWHT7RPZI","TWHT7TDXT","TWHT7TDYT","TWHT7TDZT","TWHT7TPXI","TWHT7TPYI", &
                               "TWHT7TPZI","TWHT8ALXT","TWHT8ALYT","TWHT8ALZT","TWHT8FLXT","TWHT8FLYT","TWHT8FLZT", &
                               "TWHT8MLXT","TWHT8MLYT","TWHT8MLZT","TWHT8RDXT","TWHT8RDYT","TWHT8RDZT","TWHT8RPXI", &
                               "TWHT8RPYI","TWHT8RPZI","TWHT8TDXT","TWHT8TDYT","TWHT8TDZT","TWHT8TPXI","TWHT8TPYI", &
                               "TWHT8TPZI","TWHT9ALXT","TWHT9ALYT","TWHT9ALZT","TWHT9FLXT","TWHT9FLYT","TWHT9FLZT", &
                               "TWHT9MLXT","TWHT9MLYT","TWHT9MLZT","TWHT9RDXT","TWHT9RDYT","TWHT9RDZT","TWHT9RPXI", &
                               "TWHT9RPYI","TWHT9RPZI","TWHT9TDXT","TWHT9TDYT","TWHT9TDZT","TWHT9TPXI","TWHT9TPYI", &
                               "TWHT9TPZI","TWRBSFXT ","TWRBSFYT ","TWRBSFZT ","TWRBSMXT ","TWRBSMYT ","TWRBSMZT ", &
                               "TWRCLRNC1","TWRCLRNC2","TWRCLRNC3","TWSTDEFL1","TWSTDEFL2","TWSTDEFL3","UWIND    ", &
                               "VERWNDDIR","VWIND    ","WINDVXI  ","WINDVYI  ","WINDVZI  ","WWIND    ","YAWACCEL ", &
                               "YAWAZN   ","YAWAZP   ","YAWBRFXN ","YAWBRFXP ","YAWBRFYN ","YAWBRFYP ","YAWBRFZN ", &
                               "YAWBRFZP ","YAWBRMXN ","YAWBRMXP ","YAWBRMYN ","YAWBRMYP ","YAWBRMZN ","YAWBRMZP ", &
                               "YAWBRRAXP","YAWBRRAYP","YAWBRRAZP","YAWBRRDXT","YAWBRRDYT","YAWBRRDZT","YAWBRRVXP", &
                               "YAWBRRVYP","YAWBRRVZP","YAWBRTAXP","YAWBRTAYP","YAWBRTAZP","YAWBRTDXP","YAWBRTDXT", &
                               "YAWBRTDYP","YAWBRTDYT","YAWBRTDZP","YAWBRTDZT","YAWMOM   ","YAWPOS   ","YAWPZN   ", &
                               "YAWPZP   ","YAWRATE  ","YAWVZN   ","YAWVZP   "/)
   INTEGER(IntKi), PARAMETER :: ParamIndxAry(1019) =  (/ &                          ! This lists the index into AllOuts(:) of the allowed parameters ValidParamAry(:)
                                LSSTipPxa , PtchPMzc1 , PtchPMzc2 , PtchPMzc3 , PtchPMzc1 , PtchPMzc2 , PtchPMzc3 , &
                                CThrstRad , CThrstAzm , CThrstRad ,   HSShftA ,     GenCp ,     GenCq ,    GenPwr , &
                                  HSShftV ,     GenTq ,  HorWindV , HorWndDir ,   HSSBrTq ,   HSShftA ,  HSShftCp , &
                                 HSShftCq , HSShftPwr ,  HSShftTq ,   HSShftV ,   TipDyc1 ,   TipDyc2 ,   TipDyc3 , &
                                LSSGagAxa , LSSGagAxa , LSSGagAxa , LSShftFxa , LSShftFxa , LSShftFya , LSShftFys , &
                                LSShftFza , LSShftFzs , LSShftMxa , LSShftMxa , LSSGagMya , LSSGagMys , LSSGagMza , &
                                LSSGagMzs , LSSGagPxa , LSSGagPxa , LSSGagPxa , LSSGagVxa , LSSGagVxa , LSSGagVxa , &
                                    RotCp ,     RotCq ,     RotCt , LSShftFxa , LSShftFxa , LSShftFya , LSShftFys , &
                                LSShftFza , LSShftFzs , LSShftMxa , LSShftMxa ,    RotPwr , LSShftMxa , LSSTipAxa , &
                                LSSTipAxa , LSSTipAxa , LSSTipMya , LSSTipMys , LSSTipMza , LSSTipMzs , LSSTipPxa , &
                                LSSTipPxa , LSSTipPxa , LSSTipVxa , LSSTipVxa , LSSTipVxa ,    YawPzn ,    YawAzn , &
                                NacYawErr ,    YawPzn ,    YawVzn , NcIMURAxs , NcIMURAys , NcIMURAzs , NcIMURVxs , &
                                NcIMURVys , NcIMURVzs , NcIMUTAxs , NcIMUTAys , NcIMUTAzs , NcIMUTVxs , NcIMUTVys , &
                                NcIMUTVzs ,   TipDxc1 ,   TipDxc2 ,   TipDxc3 ,  TipRDyb1 ,  TipRDyb2 ,  TipRDyb3 , &
                                PtchPMzc1 , PtchPMzc2 , PtchPMzc3 , PtchPMzc1 , PtchPMzc2 , PtchPMzc3 ,   PtfmFxi , &
                                  PtfmFxt ,   PtfmFyi ,   PtfmFyt ,   PtfmFzi ,   PtfmFzt ,  PtfmTDzi ,   PtfmMxi , &
                                  PtfmMxt ,   PtfmMyi ,   PtfmMyt ,   PtfmMzi ,   PtfmMzt ,  PtfmRDyi ,  PtfmRAxi , &
                                 PtfmRAxt ,  PtfmRAyi ,  PtfmRAyt ,  PtfmRAzi ,  PtfmRAzt ,  PtfmRDxi ,  PtfmRDyi , &
                                 PtfmRDzi ,  PtfmRDxi ,  PtfmRVxi ,  PtfmRVxt ,  PtfmRVyi ,  PtfmRVyt ,  PtfmRVzi , &
                                 PtfmRVzt ,  PtfmTDxi ,  PtfmTDyi ,  PtfmTAxi ,  PtfmTAxt ,  PtfmTAyi ,  PtfmTAyt , &
                                 PtfmTAzi ,  PtfmTAzt ,  PtfmTDxi ,  PtfmTDxt ,  PtfmTDyi ,  PtfmTDyt ,  PtfmTDzi , &
                                 PtfmTDzt ,  PtfmTVxi ,  PtfmTVxt ,  PtfmTVyi ,  PtfmTVyt ,  PtfmTVzi ,  PtfmTVzt , &
                                 PtfmRDzi ,  QD2_B1E1 ,  QD2_B1F1 ,  QD2_B1F2 ,  QD2_B2E1 ,  QD2_B2F1 ,  QD2_B2F2 , &
                                 QD2_B3E1 ,  QD2_B3F1 ,  QD2_B3F2 ,  QD2_DrTr ,  QD2_GeAz ,    QD2_Hv ,     QD2_P , &
                                    QD2_R ,  QD2_RFrl ,    QD2_Sg ,    QD2_Sw ,  QD2_Teet ,  QD2_TFA1 ,  QD2_TFA2 , &
                                 QD2_TFrl ,  QD2_TSS1 ,  QD2_TSS2 ,     QD2_Y ,   QD2_Yaw ,   QD_B1E1 ,   QD_B1F1 , &
                                  QD_B1F2 ,   QD_B2E1 ,   QD_B2F1 ,   QD_B2F2 ,   QD_B3E1 ,   QD_B3F1 ,   QD_B3F2 , &
                                  QD_DrTr ,   QD_GeAz ,     QD_Hv ,      QD_P ,      QD_R ,   QD_RFrl ,     QD_Sg , &
                                    QD_Sw ,   QD_Teet ,   QD_TFA1 ,   QD_TFA2 ,   QD_TFrl ,   QD_TSS1 ,   QD_TSS2 , &
                                     QD_Y ,    QD_Yaw ,    Q_B1E1 ,    Q_B1F1 ,    Q_B1F2 ,    Q_B2E1 ,    Q_B2F1 , &
                                   Q_B2F2 ,    Q_B3E1 ,    Q_B3F1 ,    Q_B3F2 ,    Q_DrTr ,    Q_GeAz ,      Q_Hv , &
                                      Q_P ,       Q_R ,    Q_RFrl ,      Q_Sg ,      Q_Sw ,    Q_Teet ,    Q_TFA1 , &
                                   Q_TFA2 ,    Q_TFrl ,    Q_TSS1 ,    Q_TSS2 ,       Q_Y ,     Q_Yaw ,   RFrlBrM , &
                                 TipRDxb1 ,  TipRDxb2 ,  TipRDxb3 ,  RootFxb1 ,  RootFxb2 ,  RootFxb3 ,  RootFxc1 , &
                                 RootFxc2 ,  RootFxc3 ,  RootFyb1 ,  RootFyb2 ,  RootFyb3 ,  RootFyc1 ,  RootFyc2 , &
                                 RootFyc3 ,  RootFzc1 ,  RootFzc2 ,  RootFzc3 ,  RootFzc1 ,  RootFzc2 ,  RootFzc3 , &
                                 RootMxb1 ,  RootMxb2 ,  RootMxb3 ,  RootMyb1 ,  RootMyb2 ,  RootMyb3 ,  RootMxc1 , &
                                 RootMxc2 ,  RootMxc3 ,  RootMyc1 ,  RootMyc2 ,  RootMyc3 ,  RootMxb1 ,  RootMxb2 , &
                                 RootMxb3 ,  RootMxc1 ,  RootMxc2 ,  RootMxc3 ,  RootMyb1 ,  RootMyb2 ,  RootMyb3 , &
                                 RootMyc1 ,  RootMyc2 ,  RootMyc3 ,  RootMzc1 ,  RootMzc2 ,  RootMzc3 ,  RootMzc1 , &
                                 RootMzc2 ,  RootMzc3 , LSSTipAxa ,     RotCp ,     RotCq ,     RotCt ,  RotFurlP , &
                                 RotFurlA ,  RotFurlP ,  RotFurlV ,    RotPwr , LSSTipVxa ,   TeetAya ,   TeetPya , &
                                  TeetVya , LSShftFxa , LSShftMxa , Spn1ALxb1 , Spn1ALxb2 , Spn1ALxb3 , Spn1ALyb1 , &
                                Spn1ALyb2 , Spn1ALyb3 , Spn1ALzb1 , Spn1ALzb2 , Spn1ALzb3 , Spn1FLxb1 , Spn1FLxb2 , &
                                Spn1FLxb3 , Spn1FLyb1 , Spn1FLyb2 , Spn1FLyb3 , Spn1FLzb1 , Spn1FLzb2 , Spn1FLzb3 , &
                                Spn1MLxb1 , Spn1MLxb2 , Spn1MLxb3 , Spn1MLyb1 , Spn1MLyb2 , Spn1MLyb3 , Spn1MLzb1 , &
                                Spn1MLzb2 , Spn1MLzb3 , Spn1RDxb1 , Spn1RDxb2 , Spn1RDxb3 , Spn1RDyb1 , Spn1RDyb2 , &
                                Spn1RDyb3 , Spn1RDzb1 , Spn1RDzb2 , Spn1RDzb3 , Spn1TDxb1 , Spn1TDxb2 , Spn1TDxb3 , &
                                Spn1TDyb1 , Spn1TDyb2 , Spn1TDyb3 , Spn1TDzb1 , Spn1TDzb2 , Spn1TDzb3 , Spn2ALxb1 , &
                                Spn2ALxb2 , Spn2ALxb3 , Spn2ALyb1 , Spn2ALyb2 , Spn2ALyb3 , Spn2ALzb1 , Spn2ALzb2 , &
                                Spn2ALzb3 , Spn2FLxb1 , Spn2FLxb2 , Spn2FLxb3 , Spn2FLyb1 , Spn2FLyb2 , Spn2FLyb3 , &
                                Spn2FLzb1 , Spn2FLzb2 , Spn2FLzb3 , Spn2MLxb1 , Spn2MLxb2 , Spn2MLxb3 , Spn2MLyb1 , &
                                Spn2MLyb2 , Spn2MLyb3 , Spn2MLzb1 , Spn2MLzb2 , Spn2MLzb3 , Spn2RDxb1 , Spn2RDxb2 , &
                                Spn2RDxb3 , Spn2RDyb1 , Spn2RDyb2 , Spn2RDyb3 , Spn2RDzb1 , Spn2RDzb2 , Spn2RDzb3 , &
                                Spn2TDxb1 , Spn2TDxb2 , Spn2TDxb3 , Spn2TDyb1 , Spn2TDyb2 , Spn2TDyb3 , Spn2TDzb1 , &
                                Spn2TDzb2 , Spn2TDzb3 , Spn3ALxb1 , Spn3ALxb2 , Spn3ALxb3 , Spn3ALyb1 , Spn3ALyb2 , &
                                Spn3ALyb3 , Spn3ALzb1 , Spn3ALzb2 , Spn3ALzb3 , Spn3FLxb1 , Spn3FLxb2 , Spn3FLxb3 , &
                                Spn3FLyb1 , Spn3FLyb2 , Spn3FLyb3 , Spn3FLzb1 , Spn3FLzb2 , Spn3FLzb3 , Spn3MLxb1 , &
                                Spn3MLxb2 , Spn3MLxb3 , Spn3MLyb1 , Spn3MLyb2 , Spn3MLyb3 , Spn3MLzb1 , Spn3MLzb2 , &
                                Spn3MLzb3 , Spn3RDxb1 , Spn3RDxb2 , Spn3RDxb3 , Spn3RDyb1 , Spn3RDyb2 , Spn3RDyb3 , &
                                Spn3RDzb1 , Spn3RDzb2 , Spn3RDzb3 , Spn3TDxb1 , Spn3TDxb2 , Spn3TDxb3 , Spn3TDyb1 , &
                                Spn3TDyb2 , Spn3TDyb3 , Spn3TDzb1 , Spn3TDzb2 , Spn3TDzb3 , Spn4ALxb1 , Spn4ALxb2 , &
                                Spn4ALxb3 , Spn4ALyb1 , Spn4ALyb2 , Spn4ALyb3 , Spn4ALzb1 , Spn4ALzb2 , Spn4ALzb3 , &
                                Spn4FLxb1 , Spn4FLxb2 , Spn4FLxb3 , Spn4FLyb1 , Spn4FLyb2 , Spn4FLyb3 , Spn4FLzb1 , &
                                Spn4FLzb2 , Spn4FLzb3 , Spn4MLxb1 , Spn4MLxb2 , Spn4MLxb3 , Spn4MLyb1 , Spn4MLyb2 , &
                                Spn4MLyb3 , Spn4MLzb1 , Spn4MLzb2 , Spn4MLzb3 , Spn4RDxb1 , Spn4RDxb2 , Spn4RDxb3 , &
                                Spn4RDyb1 , Spn4RDyb2 , Spn4RDyb3 , Spn4RDzb1 , Spn4RDzb2 , Spn4RDzb3 , Spn4TDxb1 , &
                                Spn4TDxb2 , Spn4TDxb3 , Spn4TDyb1 , Spn4TDyb2 , Spn4TDyb3 , Spn4TDzb1 , Spn4TDzb2 , &
                                Spn4TDzb3 , Spn5ALxb1 , Spn5ALxb2 , Spn5ALxb3 , Spn5ALyb1 , Spn5ALyb2 , Spn5ALyb3 , &
                                Spn5ALzb1 , Spn5ALzb2 , Spn5ALzb3 , Spn5FLxb1 , Spn5FLxb2 , Spn5FLxb3 , Spn5FLyb1 , &
                                Spn5FLyb2 , Spn5FLyb3 , Spn5FLzb1 , Spn5FLzb2 , Spn5FLzb3 , Spn5MLxb1 , Spn5MLxb2 , &
                                Spn5MLxb3 , Spn5MLyb1 , Spn5MLyb2 , Spn5MLyb3 , Spn5MLzb1 , Spn5MLzb2 , Spn5MLzb3 , &
                                Spn5RDxb1 , Spn5RDxb2 , Spn5RDxb3 , Spn5RDyb1 , Spn5RDyb2 , Spn5RDyb3 , Spn5RDzb1 , &
                                Spn5RDzb2 , Spn5RDzb3 , Spn5TDxb1 , Spn5TDxb2 , Spn5TDxb3 , Spn5TDyb1 , Spn5TDyb2 , &
                                Spn5TDyb3 , Spn5TDzb1 , Spn5TDzb2 , Spn5TDzb3 , Spn6ALxb1 , Spn6ALxb2 , Spn6ALxb3 , &
                                Spn6ALyb1 , Spn6ALyb2 , Spn6ALyb3 , Spn6ALzb1 , Spn6ALzb2 , Spn6ALzb3 , Spn6FLxb1 , &
                                Spn6FLxb2 , Spn6FLxb3 , Spn6FLyb1 , Spn6FLyb2 , Spn6FLyb3 , Spn6FLzb1 , Spn6FLzb2 , &
                                Spn6FLzb3 , Spn6MLxb1 , Spn6MLxb2 , Spn6MLxb3 , Spn6MLyb1 , Spn6MLyb2 , Spn6MLyb3 , &
                                Spn6MLzb1 , Spn6MLzb2 , Spn6MLzb3 , Spn6RDxb1 , Spn6RDxb2 , Spn6RDxb3 , Spn6RDyb1 , &
                                Spn6RDyb2 , Spn6RDyb3 , Spn6RDzb1 , Spn6RDzb2 , Spn6RDzb3 , Spn6TDxb1 , Spn6TDxb2 , &
                                Spn6TDxb3 , Spn6TDyb1 , Spn6TDyb2 , Spn6TDyb3 , Spn6TDzb1 , Spn6TDzb2 , Spn6TDzb3 , &
                                Spn7ALxb1 , Spn7ALxb2 , Spn7ALxb3 , Spn7ALyb1 , Spn7ALyb2 , Spn7ALyb3 , Spn7ALzb1 , &
                                Spn7ALzb2 , Spn7ALzb3 , Spn7FLxb1 , Spn7FLxb2 , Spn7FLxb3 , Spn7FLyb1 , Spn7FLyb2 , &
                                Spn7FLyb3 , Spn7FLzb1 , Spn7FLzb2 , Spn7FLzb3 , Spn7MLxb1 , Spn7MLxb2 , Spn7MLxb3 , &
                                Spn7MLyb1 , Spn7MLyb2 , Spn7MLyb3 , Spn7MLzb1 , Spn7MLzb2 , Spn7MLzb3 , Spn7RDxb1 , &
                                Spn7RDxb2 , Spn7RDxb3 , Spn7RDyb1 , Spn7RDyb2 , Spn7RDyb3 , Spn7RDzb1 , Spn7RDzb2 , &
                                Spn7RDzb3 , Spn7TDxb1 , Spn7TDxb2 , Spn7TDxb3 , Spn7TDyb1 , Spn7TDyb2 , Spn7TDyb3 , &
                                Spn7TDzb1 , Spn7TDzb2 , Spn7TDzb3 , Spn8ALxb1 , Spn8ALxb2 , Spn8ALxb3 , Spn8ALyb1 , &
                                Spn8ALyb2 , Spn8ALyb3 , Spn8ALzb1 , Spn8ALzb2 , Spn8ALzb3 , Spn8FLxb1 , Spn8FLxb2 , &
                                Spn8FLxb3 , Spn8FLyb1 , Spn8FLyb2 , Spn8FLyb3 , Spn8FLzb1 , Spn8FLzb2 , Spn8FLzb3 , &
                                Spn8MLxb1 , Spn8MLxb2 , Spn8MLxb3 , Spn8MLyb1 , Spn8MLyb2 , Spn8MLyb3 , Spn8MLzb1 , &
                                Spn8MLzb2 , Spn8MLzb3 , Spn8RDxb1 , Spn8RDxb2 , Spn8RDxb3 , Spn8RDyb1 , Spn8RDyb2 , &
                                Spn8RDyb3 , Spn8RDzb1 , Spn8RDzb2 , Spn8RDzb3 , Spn8TDxb1 , Spn8TDxb2 , Spn8TDxb3 , &
                                Spn8TDyb1 , Spn8TDyb2 , Spn8TDyb3 , Spn8TDzb1 , Spn8TDzb2 , Spn8TDzb3 , Spn9ALxb1 , &
                                Spn9ALxb2 , Spn9ALxb3 , Spn9ALyb1 , Spn9ALyb2 , Spn9ALyb3 , Spn9ALzb1 , Spn9ALzb2 , &
                                Spn9ALzb3 , Spn9FLxb1 , Spn9FLxb2 , Spn9FLxb3 , Spn9FLyb1 , Spn9FLyb2 , Spn9FLyb3 , &
                                Spn9FLzb1 , Spn9FLzb2 , Spn9FLzb3 , Spn9MLxb1 , Spn9MLxb2 , Spn9MLxb3 , Spn9MLyb1 , &
                                Spn9MLyb2 , Spn9MLyb3 , Spn9MLzb1 , Spn9MLzb2 , Spn9MLzb3 , Spn9RDxb1 , Spn9RDxb2 , &
                                Spn9RDxb3 , Spn9RDyb1 , Spn9RDyb2 , Spn9RDyb3 , Spn9RDzb1 , Spn9RDzb2 , Spn9RDzb3 , &
                                Spn9TDxb1 , Spn9TDxb2 , Spn9TDxb3 , Spn9TDyb1 , Spn9TDyb2 , Spn9TDyb3 , Spn9TDzb1 , &
                                Spn9TDzb2 , Spn9TDzb3 , TailFurlP , TailFurlA , TailFurlP , TailFurlV ,   TeetAya , &
                                  TeetPya ,   TeetPya ,   TeetVya , TFinAlpha , TFinCDrag , TFinCLift ,  TFinCPFx , &
                                 TFinCPFy , TFinDnPrs ,   TFrlBrM , TipClrnc1 , TipClrnc2 , TipClrnc3 ,  TipALxb1 , &
                                 TipALxb2 ,  TipALxb3 ,  TipALyb1 ,  TipALyb2 ,  TipALyb3 ,  TipALzb1 ,  TipALzb2 , &
                                 TipALzb3 , TipClrnc1 , TipClrnc2 , TipClrnc3 ,   TipDxb1 ,   TipDxb2 ,   TipDxb3 , &
                                  TipDxc1 ,   TipDxc2 ,   TipDxc3 ,   TipDyb1 ,   TipDyb2 ,   TipDyb3 ,   TipDyc1 , &
                                  TipDyc2 ,   TipDyc3 ,   TipDzc1 ,   TipDzc2 ,   TipDzc3 ,   TipDzc1 ,   TipDzc2 , &
                                  TipDzc3 ,  TipRDxb1 ,  TipRDxb2 ,  TipRDxb3 ,  TipRDyb1 ,  TipRDyb2 ,  TipRDyb3 , &
                                 TipRDzc1 ,  TipRDzc2 ,  TipRDzc3 ,  TipRDzc1 ,  TipRDzc2 ,  TipRDzc3 , TipSpdRat , &
                                 TotWindV , TipSpdRat , YawBrTDzt , YawBrTDxt , YawBrRDyt , YawBrRDxt , YawBrTDyt , &
                                YawBrRDzt , TwHt1ALxt , TwHt1ALyt , TwHt1ALzt , TwHt1FLxt , TwHt1FLyt , TwHt1FLzt , &
                                TwHt1MLxt , TwHt1MLyt , TwHt1MLzt , TwHt1RDxt , TwHt1RDyt , TwHt1RDzt , TwHt1RPxi , &
                                TwHt1RPyi , TwHt1RPzi , TwHt1TDxt , TwHt1TDyt , TwHt1TDzt , TwHt1TPxi , TwHt1TPyi , &
                                TwHt1TPzi , TwHt2ALxt , TwHt2ALyt , TwHt2ALzt , TwHt2FLxt , TwHt2FLyt , TwHt2FLzt , &
                                TwHt2MLxt , TwHt2MLyt , TwHt2MLzt , TwHt2RDxt , TwHt2RDyt , TwHt2RDzt , TwHt2RPxi , &
                                TwHt2RPyi , TwHt2RPzi , TwHt2TDxt , TwHt2TDyt , TwHt2TDzt , TwHt2TPxi , TwHt2TPyi , &
                                TwHt2TPzi , TwHt3ALxt , TwHt3ALyt , TwHt3ALzt , TwHt3FLxt , TwHt3FLyt , TwHt3FLzt , &
                                TwHt3MLxt , TwHt3MLyt , TwHt3MLzt , TwHt3RDxt , TwHt3RDyt , TwHt3RDzt , TwHt3RPxi , &
                                TwHt3RPyi , TwHt3RPzi , TwHt3TDxt , TwHt3TDyt , TwHt3TDzt , TwHt3TPxi , TwHt3TPyi , &
                                TwHt3TPzi , TwHt4ALxt , TwHt4ALyt , TwHt4ALzt , TwHt4FLxt , TwHt4FLyt , TwHt4FLzt , &
                                TwHt4MLxt , TwHt4MLyt , TwHt4MLzt , TwHt4RDxt , TwHt4RDyt , TwHt4RDzt , TwHt4RPxi , &
                                TwHt4RPyi , TwHt4RPzi , TwHt4TDxt , TwHt4TDyt , TwHt4TDzt , TwHt4TPxi , TwHt4TPyi , &
                                TwHt4TPzi , TwHt5ALxt , TwHt5ALyt , TwHt5ALzt , TwHt5FLxt , TwHt5FLyt , TwHt5FLzt , &
                                TwHt5MLxt , TwHt5MLyt , TwHt5MLzt , TwHt5RDxt , TwHt5RDyt , TwHt5RDzt , TwHt5RPxi , &
                                TwHt5RPyi , TwHt5RPzi , TwHt5TDxt , TwHt5TDyt , TwHt5TDzt , TwHt5TPxi , TwHt5TPyi , &
                                TwHt5TPzi , TwHt6ALxt , TwHt6ALyt , TwHt6ALzt , TwHt6FLxt , TwHt6FLyt , TwHt6FLzt , &
                                TwHt6MLxt , TwHt6MLyt , TwHt6MLzt , TwHt6RDxt , TwHt6RDyt , TwHt6RDzt , TwHt6RPxi , &
                                TwHt6RPyi , TwHt6RPzi , TwHt6TDxt , TwHt6TDyt , TwHt6TDzt , TwHt6TPxi , TwHt6TPyi , &
                                TwHt6TPzi , TwHt7ALxt , TwHt7ALyt , TwHt7ALzt , TwHt7FLxt , TwHt7FLyt , TwHt7FLzt , &
                                TwHt7MLxt , TwHt7MLyt , TwHt7MLzt , TwHt7RDxt , TwHt7RDyt , TwHt7RDzt , TwHt7RPxi , &
                                TwHt7RPyi , TwHt7RPzi , TwHt7TDxt , TwHt7TDyt , TwHt7TDzt , TwHt7TPxi , TwHt7TPyi , &
                                TwHt7TPzi , TwHt8ALxt , TwHt8ALyt , TwHt8ALzt , TwHt8FLxt , TwHt8FLyt , TwHt8FLzt , &
                                TwHt8MLxt , TwHt8MLyt , TwHt8MLzt , TwHt8RDxt , TwHt8RDyt , TwHt8RDzt , TwHt8RPxi , &
                                TwHt8RPyi , TwHt8RPzi , TwHt8TDxt , TwHt8TDyt , TwHt8TDzt , TwHt8TPxi , TwHt8TPyi , &
                                TwHt8TPzi , TwHt9ALxt , TwHt9ALyt , TwHt9ALzt , TwHt9FLxt , TwHt9FLyt , TwHt9FLzt , &
                                TwHt9MLxt , TwHt9MLyt , TwHt9MLzt , TwHt9RDxt , TwHt9RDyt , TwHt9RDzt , TwHt9RPxi , &
                                TwHt9RPyi , TwHt9RPzi , TwHt9TDxt , TwHt9TDyt , TwHt9TDzt , TwHt9TPxi , TwHt9TPyi , &
                                TwHt9TPzi ,  TwrBsFxt ,  TwrBsFyt ,  TwrBsFzt ,  TwrBsMxt ,  TwrBsMyt ,  TwrBsMzt , &
                                TipClrnc1 , TipClrnc2 , TipClrnc3 ,  TipRDzc1 ,  TipRDzc2 ,  TipRDzc3 ,   WindVxi , &
                                VerWndDir ,   WindVyi ,   WindVxi ,   WindVyi ,   WindVzi ,   WindVzi ,    YawAzn , &
                                   YawAzn ,    YawAzn ,  YawBrFxn ,  YawBrFxp ,  YawBrFyn ,  YawBrFyp ,  YawBrFzn , &
                                 YawBrFzn ,  YawBrMxn ,  YawBrMxp ,  YawBrMyn ,  YawBrMyp ,  YawBrMzn ,  YawBrMzn , &
                                YawBrRAxp , YawBrRAyp , YawBrRAzp , YawBrRDxt , YawBrRDyt , YawBrRDzt , YawBrRVxp , &
                                YawBrRVyp , YawBrRVzp , YawBrTAxp , YawBrTAyp , YawBrTAzp , YawBrTDxp , YawBrTDxt , &
                                YawBrTDyp , YawBrTDyt , YawBrTDzp , YawBrTDzt ,  YawBrMzn ,    YawPzn ,    YawPzn , &
                                   YawPzn ,    YawVzn ,    YawVzn ,    YawVzn /)
   CHARACTER(OutStrLen), PARAMETER :: ParamUnitsAry(1019) =  (/ &                         ! This lists the units corresponding to the allowed parameters
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(-)       ","(deg)     ","(-)       ","(deg/s^2) ","(-)       ","(-)       ","(kW)      ", &
                               "(rpm)     ","(kN·m)    ","(m/s)     ","(deg)     ","(kN·m)    ","(deg/s^2) ","(-)       ", &
                               "(-)       ","(kW)      ","(kN·m)    ","(rpm)     ","(m)       ","(m)       ","(m)       ", &
                               "(deg/s^2) ","(deg/s^2) ","(deg/s^2) ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(rpm)     ","(rpm)     ","(rpm)     ", &
                               "(-)       ","(-)       ","(-)       ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN·m)    ","(kN·m)    ","(kW)      ","(kN·m)    ","(deg/s^2) ", &
                               "(deg/s^2) ","(deg/s^2) ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ", &
                               "(deg)     ","(deg)     ","(rpm)     ","(rpm)     ","(rpm)     ","(deg)     ","(deg/s^2) ", &
                               "(deg)     ","(deg)     ","(deg/s)   ","(deg/s^2) ","(deg/s^2) ","(deg/s^2) ","(deg/s)   ", &
                               "(deg/s)   ","(deg/s)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s)     ","(m/s)     ", &
                               "(m/s)     ","(m)       ","(m)       ","(m)       ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(m)       ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg/s^2) ", &
                               "(deg/s^2) ","(deg/s^2) ","(deg/s^2) ","(deg/s^2) ","(deg/s^2) ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg/s)   ","(deg/s)   ","(deg/s)   ","(deg/s)   ","(deg/s)   ", &
                               "(deg/s)   ","(m)       ","(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m/s)     ","(m/s)     ","(m/s)     ","(m/s)     ","(m/s)     ","(m/s)     ", &
                               "(deg)     ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(rad/s^2) ","(rad/s^2) ","(m/s^2)   ","(rad/s^2) ", &
                               "(rad/s^2) ","(rad/s^2) ","(m/s^2)   ","(m/s^2)   ","(rad/s^2) ","(m/s^2)   ","(m/s^2)   ", &
                               "(rad/s^2) ","(m/s^2)   ","(m/s^2)   ","(rad/s^2) ","(rad/s^2) ","(m/s)     ","(m/s)     ", &
                               "(m/s)     ","(m/s)     ","(m/s)     ","(m/s)     ","(m/s)     ","(m/s)     ","(m/s)     ", &
                               "(rad/s)   ","(rad/s)   ","(m/s)     ","(rad/s)   ","(rad/s)   ","(rad/s)   ","(m/s)     ", &
                               "(m/s)     ","(rad/s)   ","(m/s)     ","(m/s)     ","(rad/s)   ","(m/s)     ","(m/s)     ", &
                               "(rad/s)   ","(rad/s)   ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(rad)     ","(rad)     ","(m)       ", &
                               "(rad)     ","(rad)     ","(rad)     ","(m)       ","(m)       ","(rad)     ","(m)       ", &
                               "(m)       ","(rad)     ","(m)       ","(m)       ","(rad)     ","(rad)     ","(kN·m)    ", &
                               "(deg)     ","(deg)     ","(deg)     ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(deg/s^2) ","(-)       ","(-)       ","(-)       ","(deg)     ", &
                               "(deg/s^2) ","(deg)     ","(deg/s)   ","(kW)      ","(rpm)     ","(deg/s^2) ","(deg)     ", &
                               "(deg/s)   ","(kN)      ","(kN·m)    ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(deg)     ","(deg/s^2) ","(deg)     ","(deg/s)   ","(deg/s^2) ", &
                               "(deg)     ","(deg)     ","(deg/s)   ","(deg)     ","(-)       ","(-)       ","(kN)      ", &
                               "(kN)      ","(Pa)      ","(kN·m)    ","(m)       ","(m)       ","(m)       ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(-)       ", &
                               "(m/s)     ","(-)       ","(m)       ","(m)       ","(deg)     ","(deg)     ","(m)       ", &
                               "(deg)     ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(kN)      ","(kN)      ","(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(m)       ","(m)       ","(m)       ","(deg)     ","(deg)     ","(deg)     ","(m/s)     ", &
                               "(deg)     ","(m/s)     ","(m/s)     ","(m/s)     ","(m/s)     ","(m/s)     ","(deg/s^2) ", &
                               "(deg/s^2) ","(deg/s^2) ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(deg/s^2) ","(deg/s^2) ","(deg/s^2) ","(deg)     ","(deg)     ","(deg)     ","(deg/s)   ", &
                               "(deg/s)   ","(deg/s)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(kN·m)    ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg/s)   ","(deg/s)   ","(deg/s)   "/)
   LOGICAL                  :: InvalidOutput(0:MaxOutPts)                        ! This array determines if the output channel is valid for this configuration


   InvalidOutput            = .FALSE.
!End of code generated by Matlab script


!BJJ: THE FOLLOWING USED TO BE PART OF THE SCRIPT, BUT I REWROTE IT TO BE MORE EFFICIENT
   
   IF ( .NOT. CompAero ) THEN
      InvalidOutput(  WindVxi) = .TRUE.
      InvalidOutput(  WindVyi) = .TRUE.
      InvalidOutput(  WindVzi) = .TRUE.
      InvalidOutput( TotWindV) = .TRUE.
      InvalidOutput( HorWindV) = .TRUE.
      InvalidOutput(HorWndDir) = .TRUE.
      InvalidOutput(VerWndDir) = .TRUE.
      
      InvalidOutput(TipSpdRat) = .TRUE.
      InvalidOutput(NacYawErr) = .TRUE.
      
      InvalidOutput(    RotCq) = .TRUE.
      InvalidOutput(    RotCp) = .TRUE.
      InvalidOutput(    RotCt) = .TRUE.
      InvalidOutput( HSShftCq) = .TRUE.
      InvalidOutput( HSShftCp) = .TRUE.
      InvalidOutput(    GenCq) = .TRUE.
      InvalidOutput(    GenCp) = .TRUE.
      InvalidOutput(TFinAlpha) = .TRUE.
      InvalidOutput(TFinCLift) = .TRUE.
      InvalidOutput(TFinCDrag) = .TRUE.
      InvalidOutput(TFinDnPrs) = .TRUE.
      InvalidOutput( TFinCPFx) = .TRUE.
      InvalidOutput( TFinCPFy) = .TRUE.
   END IF
                            
   DO I = p_ED%NumBl+1,3  ! Invalid blades
      
         ! motions
      
      InvalidOutput(   TipDxc(  I) ) = .TRUE.
      InvalidOutput(   TipDyc(  I) ) = .TRUE.
      InvalidOutput(   TipDzc(  I) ) = .TRUE.
      InvalidOutput(   TipDxb(  I) ) = .TRUE.
      InvalidOutput(   TipDyb(  I) ) = .TRUE.
      InvalidOutput(  TipALxb(  I) ) = .TRUE.
      InvalidOutput(  TipALyb(  I) ) = .TRUE.
      InvalidOutput(  TipALzb(  I) ) = .TRUE.
      InvalidOutput(  TipRDxb(  I) ) = .TRUE.
      InvalidOutput(  TipRDyb(  I) ) = .TRUE.
      InvalidOutput(  TipRDzc(  I) ) = .TRUE.
      InvalidOutput( TipClrnc(  I) ) = .TRUE.

         ! loads
         
      InvalidOutput(  RootFxc(  I) ) = .TRUE.
      InvalidOutput(  RootFyc(  I) ) = .TRUE.
      InvalidOutput(  RootFzc(  I) ) = .TRUE.
      InvalidOutput(  RootFxb(  I) ) = .TRUE.
      InvalidOutput(  RootFyb(  I) ) = .TRUE.
      InvalidOutput(  RootMxc(  I) ) = .TRUE.
      InvalidOutput(  RootMyc(  I) ) = .TRUE.
      InvalidOutput(  RootMzc(  I) ) = .TRUE.
      InvalidOutput(  RootMxb(  I) ) = .TRUE.
      InvalidOutput(  RootMyb(  I) ) = .TRUE.

         ! Blade node motions
 
      InvalidOutput(  SpnALxb(:,I) ) = .TRUE.
      InvalidOutput(  SpnALyb(:,I) ) = .TRUE.
      InvalidOutput(  SpnALzb(:,I) ) = .TRUE.

      InvalidOutput(  SpnTDxb(:,I) ) = .TRUE.
      InvalidOutput(  SpnTDyb(:,I) ) = .TRUE.
      InvalidOutput(  SpnTDzb(:,I) ) = .TRUE.

      InvalidOutput(  SpnRDxb(:,I) ) = .TRUE.
      InvalidOutput(  SpnRDyb(:,I) ) = .TRUE.
      InvalidOutput(  SpnRDzb(:,I) ) = .TRUE.

         ! Blade node loads
                  
      InvalidOutput(  SpnMLxb(:,I) ) = .TRUE.
      InvalidOutput(  SpnMLyb(:,I) ) = .TRUE.
      InvalidOutput(  SpnMLzb(:,I) ) = .TRUE.

      InvalidOutput(  SpnFLxb(:,I) ) = .TRUE.
      InvalidOutput(  SpnFLyb(:,I) ) = .TRUE.
      InvalidOutput(  SpnFLzb(:,I) ) = .TRUE.

   END DO

                                     
   DO I = 1,p_ED%NumBl    
      
      DO J = p_ED%NBlGages+1,9 ! Invalid blade gages

         InvalidOutput(  SpnALxb(J,I) ) = .TRUE.
         InvalidOutput(  SpnALyb(J,I) ) = .TRUE.
         InvalidOutput(  SpnALzb(J,I) ) = .TRUE.

         InvalidOutput(  SpnTDxb(J,I) ) = .TRUE.
         InvalidOutput(  SpnTDyb(J,I) ) = .TRUE.
         InvalidOutput(  SpnTDzb(J,I) ) = .TRUE.

         InvalidOutput(  SpnRDxb(J,I) ) = .TRUE.
         InvalidOutput(  SpnRDyb(J,I) ) = .TRUE.
         InvalidOutput(  SpnRDzb(J,I) ) = .TRUE.

            ! Loads
            
         InvalidOutput(  SpnMLxb(J,I) ) = .TRUE.
         InvalidOutput(  SpnMLyb(J,I) ) = .TRUE.
         InvalidOutput(  SpnMLzb(J,I) ) = .TRUE.
            
         InvalidOutput(  SpnFLxb(J,I) ) = .TRUE.
         InvalidOutput(  SpnFLyb(J,I) ) = .TRUE.
         InvalidOutput(  SpnFLzb(J,I) ) = .TRUE.


      END DO !J
      
   END DO !I
   
   DO J = p_ED%NTwGages+1,9 !Invalid tower gages

         ! Motions
         
      InvalidOutput( TwHtALxt(J) ) = .TRUE.
      InvalidOutput( TwHtALyt(J) ) = .TRUE.
      InvalidOutput( TwHtALzt(J) ) = .TRUE.

      InvalidOutput( TwHtTDxt(J) ) = .TRUE.
      InvalidOutput( TwHtTDyt(J) ) = .TRUE.
      InvalidOutput( TwHtTDzt(J) ) = .TRUE.

      InvalidOutput( TwHtRDxt(J) ) = .TRUE.
      InvalidOutput( TwHtRDyt(J) ) = .TRUE.
      InvalidOutput( TwHtRDzt(J) ) = .TRUE.

      InvalidOutput( TwHtTPxi(J) ) = .TRUE.
      InvalidOutput( TwHtTPyi(J) ) = .TRUE.
      InvalidOutput( TwHtTPzi(J) ) = .TRUE.

      InvalidOutput( TwHtRPxi(J) ) = .TRUE.
      InvalidOutput( TwHtRPyi(J) ) = .TRUE.
      InvalidOutput( TwHtRPzi(J) ) = .TRUE.

         ! Loads

      InvalidOutput( TwHtMLxt(J) ) = .TRUE.
      InvalidOutput( TwHtMLyt(J) ) = .TRUE.
      InvalidOutput( TwHtMLzt(J) ) = .TRUE.

      InvalidOutput( TwHtFLxt(J) ) = .TRUE.
      InvalidOutput( TwHtFLyt(J) ) = .TRUE.
      InvalidOutput( TwHtFLzt(J) ) = .TRUE.

   END DO      

   IF ( p_ED%NumBl < 3 ) THEN
      InvalidOutput(PtchPMzc3) = .TRUE.
      
      InvalidOutput(   Q_B3E1) = .TRUE.
      InvalidOutput(   Q_B3F1) = .TRUE.
      InvalidOutput(   Q_B3F2) = .TRUE.
      
      InvalidOutput(  QD_B3E1) = .TRUE.
      InvalidOutput(  QD_B3F1) = .TRUE.
      InvalidOutput(  QD_B3F2) = .TRUE.
   
      InvalidOutput( QD2_B3E1) = .TRUE.
      InvalidOutput( QD2_B3F1) = .TRUE.
      InvalidOutput( QD2_B3F2) = .TRUE.
   ELSE IF ( p_ED%NumBl > 2 ) THEN
      InvalidOutput(  TeetPya) = .TRUE.
      InvalidOutput(  TeetVya) = .TRUE.
      InvalidOutput(  TeetAya) = .TRUE.

      InvalidOutput(   Q_Teet) = .TRUE.
      InvalidOutput(  QD_Teet) = .TRUE.
      InvalidOutput( QD2_Teet) = .TRUE.
   END IF
     
      ! initialize ErrStat and ErrMsg
   ErrStat = ErrID_none
   ErrMsg  = ""
   
!bjj: should we first check that NumOuts is a valid number (or at least initialize it somewhere)?   
   
      ! ALLOCATE some arrays:
   ALLOCATE ( y%WriteOutput(0:p_ED%NumOuts) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      ErrStat = ErrID_Fatal
      ErrMsg = 'Error allocating memory for the ElastoDyn WriteOutput array.' 
      RETURN
   ENDIF
            
   ALLOCATE ( p_ED%OutParam(0:p_ED%NumOuts) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = 'Error allocating memory for the ElastoDyn OutParam array.'
      RETURN
   ENDIF
     
   !-------------------------------------------------------------------------------------------------
   ! Set index, name, and units for the output channels
   ! If a selected output channel is not available in this module, set error flag.
   !-------------------------------------------------------------------------------------------------     

!   OutPFmt = '( I4, 3X,A '//TRIM(Num2LStr(OutStrLen))//',1 X, A'//TRIM(Num2LStr(OutStrLen))//' )'

   
      ! Set index, name, and units for the time output channel:
   
   p_ED%OutParam(0)%Indx  = Time      !
   p_ED%OutParam(0)%Name  = 'Time'    ! WriteOutput(0) is the time channel by default.
   p_ED%OutParam(0)%Units = '(s)'     !
   p_ED%OutParam(0)%SignM = 1
      
   
      ! Set index, name, and units for all of the output channels.
      ! If a selected output channel is not available by FAST, ProgAbort.
   
   DO I = 1,p_ED%NumOuts
   
      p_ED%OutParam(I)%Name  = OutList(I)   
      OutListTmp               = OutList(I)
   
      ! Reverse the sign (+/-) of the output channel if the user prefixed the
      !   channel name with a '-', '_', 'm', or 'M' character indicating "minus".
   
      
      CheckOutListAgain = .FALSE.
      
      IF      ( INDEX( '-_', OutListTmp(1:1) ) > 0 ) THEN
         p_ED%OutParam(I)%SignM = -1                    ! ex, '-TipDxc1' causes the sign of TipDxc1 to be switched.
         OutListTmp               = OutListTmp(2:)
      ELSE IF ( INDEX( 'mM', OutListTmp(1:1) ) > 0 ) THEN ! We'll assume this is a variable name for now, (if not, we will check later if OutListTmp(2:) is also a variable name)
         CheckOutListAgain            = .TRUE.
         p_ED%OutParam(I)%SignM = 1
      ELSE
         p_ED%OutParam(I)%SignM = 1
      END IF          
      
      CALL Conv2UC( OutListTmp )    ! Convert OutListTmp to upper case
   
   
      Indx =  IndexCharAry( OutListTmp(1:OutStrLenM1), ValidParamAry )
      
      
         ! If it started with an "M" (CheckOutListAgain) we didn't find the value in our list (Indx < 1)
         
      IF ( CheckOutListAgain .AND. Indx < 1 ) THEN    ! Let's assume that "M" really meant "minus" and then test again         
         p_ED%OutParam(I)%SignM = -1                ! ex, 'MTipDxc1' causes the sign of TipDxc1 to be switched.
         OutListTmp               = OutListTmp(2:)
         
         Indx = IndexCharAry( OutListTmp(1:OutStrLenM1), ValidParamAry )         
      END IF
            
      
      IF ( Indx > 0 ) THEN
         p_ED%OutParam(I)%Indx     = ParamIndxAry(Indx)
         IF ( InvalidOutput( ParamIndxAry(Indx) ) ) THEN
            p_ED%OutParam(I)%Units = 'INVALID'   
            p_ED%OutParam(I)%SignM = 0
         ELSE
            p_ED%OutParam(I)%Units = ParamUnitsAry(Indx)
         END IF
      ELSE
         p_ED%OutParam(I)%Indx  = Time                 ! pick any valid channel
         p_ED%OutParam(I)%Units = 'INVALID'            
         p_ED%OutParam(I)%SignM = 0                    ! multiply all results by zero 
         
         ErrStat = ErrID_Warn
         ErrMsg  = p_ED%OutParam(I)%Name//' is not an available output channel. '//TRIM(ErrMsg)
      END IF
      
   END DO             
   
  
   
      ! Initialize all invalid output channels to zero so that we can avoid doing resetting them to zero at every time step.
      ! ALSO, set others to zero: (e.g. TipRDzc1, TipRDzc2, TipRDzc3 are always zero and so are not calculated in CalcOuts()

!   y%AllOuts(InvalidOutput) = 0.0
!   y%AllOuts = 0.0 !not necessary when SignM is initialized to zero??
   
!bjj: perhaps InvalidOutput should be in the Output Module... then we can check that each time in RtHS() instead of checking the criteria again...
! or not.... (some if statements can be combined...)
   

RETURN
END SUBROUTINE ChckOutLst
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


   ! Read the lines up/including to the "Echo" simulation control variable
   ! If echo is FALSE, don't write these lines to the echo file. 
   ! If Echo is TRUE, rewind and write on the second try.
   
I = 1
WrEcho = .FALSE.
UnEc   = -1
DO 
!-------------------------- HEADER ---------------------------------------------
   
   CALL ReadCom( UnIn, PriFile,                   'file header line 1', ErrStat, ErrMsg, UnEc )
   IF ( ErrStat >= AbortErrLev ) RETURN   

   CALL ReadCom( UnIn, PriFile,                   'file header line 2', ErrStat, ErrMsg, UnEc )
   IF ( ErrStat >= AbortErrLev ) RETURN   

   CALL ReadStr( UnIn, PriFile, FTitle, 'FTitle', 'file header line 3', ErrStat, ErrMsg, UnEc )
   IF ( ErrStat >= AbortErrLev ) RETURN   

   CALL ReadCom( UnIn, PriFile,                   'file header line 4', ErrStat, ErrMsg, UnEc )
   IF ( ErrStat >= AbortErrLev ) RETURN   

!-------------------------- SIMULATION CONTROL PARAMETERS ----------------------

   CALL ReadCom( UnIn, PriFile, 'simulation control parameters comment', ErrStat, ErrMsg, UnEc )
   IF ( ErrStat >= AbortErrLev ) RETURN   

      ! Echo - Echo input to "echo.out".
   
   CALL ReadVar( UnIn, PriFile, WrEcho, 'Echo',   'Echo switch', ErrStat, ErrMsg, UnEc )
   IF ( ErrStat >= AbortErrLev ) RETURN   
   
   !Echo = WrEcho
   
   IF (.NOT. WrEcho .OR. I > 1) EXIT
   
      ! Otherwise, open the echo file, then rewind the input file and echo everything we've read
      
   I = I + 1         ! make sure we do this only once (increment counter that says how many times we've read this file)

   CALL OpenEcho ( UnEc, TRIM(RootName)//'.ech', ErrStat, ErrMsg, ED_Ver )
   IF ( ErrStat >= AbortErrLev ) RETURN   
      
   REWIND( UnIn, IOSTAT=ErrStat )   
   IF ( ErrStat /= 0 ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg = 'Error rewinding file "'//TRIM(PriFile)//'".'
      RETURN
   END IF
      
END DO    

CALL WrScr1( ' Heading of the FAST input file: '//TRIM( FTitle ) )


   ! ADAMSPrep - ADAMS preprocossor mode.

CALL ReadVar ( UnIn, PriFile, ADAMSPrep, 'ADAMSPrep', 'ADAMS preprocessor mode', UnEc=UnEc )
IF ( ErrStat /= 0 ) THEN
   ErrMsg = TRIM(Msg)//' '//TRIM(ErrMsg)
   IF ( ErrStat >= AbortErrLev ) RETURN
END IF


IF ( (Cmpl4SFun .OR. Cmpl4LV) .AND. ( ADAMSPrep /= 1 ) )  THEN
   CALL ProgWarn ( " An ADAMS dataset can't be built when FAST is interfaced with Simulink or Labview."// & 
      " ADAMSPrep is being set to 1.")
   ADAMSPrep = 1
ELSEIF ( ( ADAMSPrep < 1 ) .OR. ( ADAMSPrep > 3 ) )  THEN
   CALL ProgAbort ( ' ADAMSPrep must be 1, 2, or 3.' )
ENDIF


   ! AnalMode - FAST analysis mode.

CALL ReadVar ( UnIn, PriFile, AnalMode, 'AnalMode', 'Analysis mode', UnEc=UnEc  )

IF ( (Cmpl4SFun .OR. Cmpl4LV) .AND. ( AnalMode /= 1 ) )  THEN
   CALL ProgAbort ( ' FAST can''t linearize the model when interfaced with Simulink or Labview.'// &
                '  Set AnalMode to 1 or use the standard version of FAST.'            )
ELSEIF ( ( AnalMode < 1 ) .OR. ( AnalMode > 2 ) )  THEN
   CALL ProgAbort ( ' AnalMode must be 1 or 2.' )
ENDIF


   ! p%NumBl - Number of blades.

CALL ReadVar ( UnIn, PriFile, InputFileData%NumBl, 'NumBl', 'Number of blades', UnEc=UnEc  )

IF ( ( InputFileData%NumBl < 2 ) .OR. ( InputFileData%NumBl > 3 ) )  CALL ProgAbort ( ' NumBl must be either 2 or 3.' )
p%NumBl = InputFileData%NumBl

   ! TMax - Total run time.

CALL ReadVar ( UnIn, PriFile, TMax, 'TMax', 'Total run time', UnEc=UnEc  )

!IF ( ( TMax < 0.0 ) .OR. ( TMax > 9999.999 ) )  CALL ProgAbort ( ' TMax must be between 0.0 and 9999.999 (inclusive).' )
IF ( TMax < 0.0  )  CALL ProgAbort ( ' TMax must not be a negative number.' )

   ! DT - Integration time step.

CALL ReadVar ( UnIn, PriFile, DT, 'DT', 'Integration time step', UnEc=UnEc  )

IF ( DT <= 0.0_DbKi )  CALL ProgAbort ( ' DT must be greater than 0.' )
IF ( DT <= TMax*EPSILON(DT) )  CALL ProgAbort ( ' DT must be greater than '//TRIM ( Num2LStr( TMax*EPSILON(DT) ) )//' seconds.' ) ! Test DT and TMax to ensure numerical stability -- HINT: see the use of OnePlusEps.


!-------------------------- TURBINE CONTROL PARAMETERS -------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'Turbine control parameters', UnEc=UnEc  )


   ! YCMode - Yaw control mode.

CALL ReadVar ( UnIn, PriFile, YCMode, 'YCMode', 'Yaw control mode', UnEc=UnEc  )

IF ( ( .NOT. Cmpl4SFun .AND. .NOT. Cmpl4LV ) .AND. ( YCMode == 2 ) )  THEN
   CALL ProgAbort ( ' YCMode can only equal 2 when FAST is interfaced with Simulink or Labview.'// &
                '  Set YCMode to 0 or 1 or interface FAST with Simulink or Labview.'             )
ELSEIF ( ( YCMode < 0 ) .OR. ( YCMode > 2 ) )  THEN
   CALL ProgAbort ( ' YCMode must be 0, 1, or 2.' )
ENDIF


   ! TYCOn - Time to enable yaw control.

CALL ReadVar ( UnIn, PriFile, TYCOn, 'TYCOn', 'Time to enable yaw control', UnEc=UnEc  )

IF ( (Cmpl4SFun .OR. Cmpl4LV) .AND. ( YCMode == 2 ) .AND. ( TYCOn /= 0.0_DbKi ) )  THEN
   CALL ProgAbort ( ' Yaw control must be enabled at time zero when implemented in Simulink or Labview.'//      &
                '  Set TYCon to 0.0, set YCMode to 0 or 1, or use the standard version of FAST.'   )
ELSEIF ( TYCOn < 0.0_DbKi )  THEN
   CALL ProgAbort ( ' TYCOn must not be negative.' )
ENDIF


   ! PCMode - Pitch control mode.

CALL ReadVar ( UnIn, PriFile, PCMode, 'PCMode', 'Pitch control mode', UnEc=UnEc  )

IF ( ( .NOT. Cmpl4SFun .AND. .NOT. Cmpl4LV ) .AND. ( PCMode == 2 ) )  THEN
   CALL ProgAbort ( ' PCMode can only equal 2 when FAST is interfaced with Simulink or Labview.'// &
                '  Set PCMode to 0 or 1 or interface FAST with Simulink or Labview.'             )
ELSEIF ( ( PCMode < 0 ) .OR. ( PCMode > 2 ) )  THEN
   CALL ProgAbort ( ' PCMode must be 0, 1, or 2.' )
ENDIF


   ! TPCOn - Time to enable pitch control.

CALL ReadVar ( UnIn, PriFile, TPCOn, 'TPCOn', 'Time to enable pitch control', UnEc=UnEc  )

IF ( (Cmpl4SFun .OR. Cmpl4LV)  .AND. ( PCMode == 2 ) .AND. ( TPCOn /= 0.0_DbKi ) )  THEN
   CALL ProgAbort ( ' Pitch control must be enabled at time zero when implemented in Simulink or Labview.'//    &
                '  Set TPCon to 0.0, set PCMode to 0 or 1, or use the standard version of FAST.'   )
ELSEIF ( TPCOn < 0.0_DbKi )  THEN
   CALL ProgAbort ( ' TPCOn must not be negative.' )
ENDIF


   ! VSContrl - Variable-speed-generator control switch.

CALL ReadVar ( UnIn, PriFile, VSContrl, 'VSContrl', 'Variable-speed-generator control switch', UnEc=UnEc  )

IF ( ( .NOT. Cmpl4SFun .AND. .NOT. Cmpl4LV ) .AND. ( VSContrl == 3 ) )  THEN
   CALL ProgAbort ( ' VSContrl can only equal 3 when FAST is interfaced with Simulink or Labview.'// &
                '  Set VSContrl to 0, 1, or 2 or interface FAST with Simulink or Labview.'         )
ELSEIF ( ( VSContrl < 0 ) .OR. ( VSContrl > 3 ) )  THEN
   CALL ProgAbort ( ' VSContrl must be either 0, 1, 2, or 3.' )
ENDIF


   ! VS_RtGnSp - Rated generator speed for simple variable-speed generator control.

CALL ReadVar ( UnIn, PriFile, VS_RtGnSp, 'VS_RtGnSp', 'Rated generator speed for simple variable-speed generator control', UnEc=UnEc  )

IF ( ( VSContrl == 1 ) .AND. ( VS_RtGnSp <= 0.0 ) )  CALL ProgAbort ( ' VS_RtGnSp must be greater than zero.' )


   ! VS_RtTq - Rated generator torque/constant generator torque in Region 3 for simple variable-speed generator control.

CALL ReadVar ( UnIn, PriFile, VS_RtTq, 'VS_RtTq', &
   'Rated generator torque/constant generator torque in Region 3 for simple variable-speed generator control', UnEc=UnEc  )

IF ( ( VSContrl == 1 ) .AND. ( VS_RtTq < 0.0 ) )  CALL ProgAbort ( ' VS_RtTq must not be negative.' )


   ! VS_Rgn2K - Generator torque constant in Region 2 for simple variable-speed generator control.

CALL ReadVar ( UnIn, PriFile, VS_Rgn2K, 'VS_Rgn2K', &
   'Generator torque constant in Region 2 for simple variable-speed generator control', UnEc=UnEc  )

IF ( ( VSContrl == 1 ) .AND. ( VS_Rgn2K < 0.0 ) )  CALL ProgAbort ( ' VS_Rgn2K must not be negative.' )
IF ( ( VSContrl == 1 ) .AND. ( VS_Rgn2K*VS_RtGnSp*VS_RtGnSp >  VS_RtTq ) )  &
   CALL ProgAbort ( ' VS_Rgn2K*VS_RtGnSp^2 must not be greater than VS_RtTq.' )


   ! VS_SlPc - Rated generator slip percentage in Region 2 1/2 for simple variable-speed generator control.

CALL ReadVar ( UnIn, PriFile, VS_SlPc, 'VS_SlPc', &
   'Rated generator slip percentage in Region 2 1/2 for simple variable-speed generator control', UnEc=UnEc  )

IF ( ( VSContrl == 1 ) .AND. ( VS_SlPc <= 0.0 ) )  CALL ProgAbort ( ' VS_SlPc must be greater than zero.' )

   ! GenModel - Generator model.

CALL ReadVar ( UnIn, PriFile, GenModel, 'GenModel', 'Generator model', UnEc=UnEc  )

IF ( ( GenModel < 1 ) .OR. ( GenModel > 3 ) )  CALL ProgAbort ( ' GenModel must be either 1, 2, or 3.' )


   ! GenTiStr - Start generator based upon T: time or F: generator speed.

CALL ReadVar ( UnIn, PriFile, GenTiStr, 'GenTiStr', 'Start generator based upon T: time or F: generator speed', UnEc=UnEc  )

IF ( (Cmpl4SFun .OR. Cmpl4LV) .AND. ( VSContrl == 3 ) .AND. ( .NOT. GenTiStr ) )  &
   CALL ProgAbort ( ' Variable-speed, generator torque control must be enabled at time zero when implemented in Simulink'//&
      'or Labview. Set GenTiStr to True and TimGenOn to 0.0, set VSContrl to 0, 1, or 2, or use the standard version of FAST.' )


   ! GenTiStp - Stop generator based upon T: time or F: generator power = 0.

CALL ReadVar ( UnIn, PriFile, GenTiStp, 'GenTiStp', 'Stop generator based upon T: time or F: generator power = 0', UnEc=UnEc  )

IF ( (Cmpl4SFun .OR. Cmpl4LV) .AND. ( VSContrl == 3 ) .AND. ( .NOT. GenTiStp ) )  &
   CALL ProgAbort ( ' Variable-speed, generator torque control must not be disabled during simulation when'//   &
                ' implemented in Simulink or Labview.'//                          &
                '  Set GenTiStp to True and TimGenOf > TMax, set VSContrl to 0, 1, or 2, or use the standard version of FAST.'   )


   ! SpdGenOn - Generator speed to turn on the generator for a startup.

CALL ReadVar ( UnIn, PriFile, SpdGenOn, 'SpdGenOn', 'Generator speed to turn on the generator', UnEc=UnEc  )

IF ( SpdGenOn < 0.0 )  CALL ProgAbort ( ' SpdGenOn must not be negative.' )


   ! TimGenOn - Time to turn on generator for startup.

CALL ReadVar ( UnIn, PriFile, TimGenOn, 'TimGenOn', 'Time to turn on generator', UnEc=UnEc  )

IF ( (Cmpl4SFun .OR. Cmpl4LV) .AND. ( VSContrl == 3 ) .AND. ( TimGenOn /= 0.0 ) )  THEN
   CALL ProgAbort ( ' Variable-speed, generator torque control must be enabled at time zero when implemented in Simulink'//&
   ' or Labview. Set GenTiStr to True and TimGenOn to 0.0, set VSContrl to 0, 1, or 2, or use the standard version of FAST.' )
ELSEIF ( TimGenOn < 0.0 )  THEN
   CALL ProgAbort ( ' TimGenOn must not be negative.' )
ENDIF


   ! TimGenOf - Time to turn off generator for braking or modeling a run-away.

CALL ReadVar ( UnIn, PriFile, TimGenOf, 'TimGenOf', 'Time to turn off generator', UnEc=UnEc  )

IF ( (Cmpl4SFun .OR. Cmpl4LV) .AND. ( VSContrl == 3 ) .AND. ( TimGenOf <= TMax ) )  THEN
   CALL ProgAbort ( ' Variable-speed, generator torque control must not be disabled during simulation when'//         &
                ' implemented in Simulink or Labview.'//                                                              &
                '  Set GenTiStp to True and TimGenOf > TMax, set VSContrl to 0, 1, or 2, or use the standard version of FAST.' )
ELSEIF ( TimGenOf < 0.0 )  THEN
   CALL ProgAbort ( ' TimGenOf must not be negative.' )
ENDIF


   ! HSSBrMode - HSS brake model.

CALL ReadVar ( UnIn, PriFile, HSSBrMode, 'HSSBrMode', 'HSS brake model', UnEc=UnEc )

IF ( ( HSSBrMode < 1 ) .OR. ( HSSBrMode > 3 ) )  CALL ProgAbort ( ' HSSBrMode must be 1, 2 or 3.' )

IF ( ( .NOT. Cmpl4LV) .AND. ( HSSBrMode == 3 ) )  THEN
   CALL ProgAbort ( ' HSSBrMode can be 3 only when when implemented in Labview.' )
ENDIF



   ! THSSBrDp - Time to initiate deployment of the HSS brake.

CALL ReadVar ( UnIn, PriFile, THSSBrDp, 'THSSBrDp', 'Time to initiate deployment of the HSS brake', UnEc=UnEc )

IF ( Cmpl4SFun .AND. ( THSSBrDp <= TMax ) )  THEN
   CALL ProgAbort ( ' A high-speed shaft brake shutdown event can''t be initiated when FAST is interfaced with Simulink'// &
                ' or Labview.  Set THSSBrDp > TMax or use the standard version of FAST.'        )
ELSEIF ( THSSBrDp < 0.0 )  THEN
   CALL ProgAbort ( ' THSSBrDp must not be negative.' )
ENDIF


   ! TiDynBrk - Time to initiate deployment of the dynamic generator brake.

!JASON: ADD LOGIC FOR THIS NEW VARIABLE:
!JASON:CALL ReadVar ( UnIn, PriFile, TiDynBrk, 'TiDynBrk', 'Time to initiate deployment of the dynamic generator brake', UnEc=UnEc )
!JASON:
!JASON:IF ( TiDynBrk < 0.0 )  CALL ProgAbort ( ' TiDynBrk must not be negative.' )
   CALL ReadCom ( UnIn, PriFile, 'currently ignored TiDynBrk', UnEc=UnEc )


   ! TTpBrDp - Time to initiate deployment of tip brakes.

ALLOCATE ( TTpBrDp(p%NumBl) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the TTpBrDp array.' )
ENDIF

CALL ReadAryLines ( UnIn, PriFile, TTpBrDp, p%NumBl, 'TTpBrDp', 'Time to initiate deployment of tip brakes', ErrStat, ErrMsg, UnEc=UnEc )

DO K=1,p%NumBl
   IF ( TTpBrDp(K) < 0.0   )  THEN
      CALL ProgAbort ( ' TTpBrDp('//TRIM( Num2LStr( K ) )//') must not be negative.' )
   ENDIF
ENDDO ! K

IF ( p%NumBl == 2 )  THEN
   CALL ReadCom ( UnIn, PriFile, 'unused TTpBrDp(3)', UnEc=UnEc )
ENDIF


   ! TBDepISp - Deployment-initiation speed for the tip brakes.

ALLOCATE ( TBDepISp(p%NumBl) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the TBDepISp array.' )
ENDIF

CALL ReadAryLines ( UnIn, PriFile, TBDepISp, p%NumBl, 'TBDepISp', 'Deployment-initiation speed for the tip brakes', ErrStat, ErrMsg, UnEc=UnEc )

DO K=1,p%NumBl
   IF ( TBDepISp(K) < 0.0 )  THEN
      CALL ProgAbort ( ' TBDepISp('//TRIM( Num2LStr( K ) )//') must not be negative.' )
   ENDIF
ENDDO ! K

IF ( p%NumBl == 2 )  THEN
   CALL ReadCom ( UnIn, PriFile, 'unused TBDepISp(3)', UnEc=UnEc )
ENDIF


   ! TYawManS - Time to start yaw maneuver.

CALL ReadVar ( UnIn, PriFile, TYawManS, 'TYawManS', 'Time to start yaw maneuver', UnEc=UnEc )

IF ( TYawManS < 0.0 )  CALL ProgAbort ( ' TYawManS must not be negative.' )


   ! TYawManE - Time to end yaw maneuver.

CALL ReadVar ( UnIn, PriFile, TYawManE, 'TYawManE', 'Time to end yaw maneuver', UnEc=UnEc )

IF ( TYawManE < TYawManS )  CALL ProgAbort ( ' TYawManE must not be less than TYawManS.' )


   ! NacYawF - Final nacelle-yaw angle for maneuvers.

CALL ReadVar ( UnIn, PriFile, NacYawF, 'NacYawF', 'Final nacelle-yaw angle for maneuvers', UnEc=UnEc )
NacYawF   = D2R*NacYawF                                                         ! Final nacelle yaw (after override yaw maneuver) in radians.


   ! TPitManS - Time to start pitch maneuvers.

ALLOCATE ( TPitManS(p%NumBl) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the TPitManS array.' )
ENDIF

CALL ReadAryLines ( UnIn, PriFile, TPitManS, p%NumBl, 'TPitManS', 'Time to start pitch maneuvers', ErrStat, ErrMsg, UnEc=UnEc )

DO K=1,p%NumBl
   IF ( TPitManS(K) < 0.0 )  CALL ProgAbort ( ' TPitManS('//TRIM( Num2LStr( K ) )//') must not be negative.' )
ENDDO ! K

IF ( p%NumBl == 2 )  THEN
   CALL ReadCom ( UnIn, PriFile, 'unused TPitManS(3)', UnEc=UnEc )
ENDIF


   ! TPitManE - Time to end pitch maneuvers.

ALLOCATE ( TPitManE(p%NumBl) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the TPitManE array.' )
ENDIF

CALL ReadAryLines ( UnIn, PriFile, TPitManE, p%NumBl, 'TPitManE', 'Time to end pitch maneuvers', ErrStat, ErrMsg,  UnEc=UnEc )

DO K=1,p%NumBl
   IF ( TPitManE(K) <TPitManS(K) )  &
      CALL ProgAbort ( ' TPitManE('//TRIM( Num2LStr( K ) )//') must not be less than TPitManS('//TRIM( Num2LStr( K ) )//').' )
ENDDO ! K

IF ( p%NumBl == 2 )  THEN
   CALL ReadCom ( UnIn, PriFile, 'unused TPitManE(3)', UnEc=UnEc )
ENDIF


   ! BlPitch - Initial pitch angle.

ALLOCATE ( BlPitch(p%NumBl) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the BlPitch array.' )
ENDIF

CALL ReadAryLines ( UnIn, PriFile, BlPitch, p%NumBl, 'BlPitch', 'Initial pitch angle', ErrStat, ErrMsg, UnEc=UnEc )

DO K=1,p%NumBl
   IF ( ( BlPitch(K) <= -180.0 ) .OR. ( BlPitch(K) > 180.0 ) )  THEN
      CALL ProgAbort ( ' BlPitch('//TRIM( Num2LStr( K ) )//') must be greater than -180 and less than or equal to 180.' )
   ENDIF
ENDDO ! K

IF ( p%NumBl == 2 )  THEN
   CALL ReadCom ( UnIn, PriFile, 'unused BlPitch(3)', UnEc=UnEc )
ENDIF


   ! BlPitchF - Final pitch angle for maneuvers.

ALLOCATE ( BlPitchF(p%NumBl) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the BlPitchF array.' )
ENDIF

CALL ReadAryLines ( UnIn, PriFile, BlPitchF, p%NumBl, 'BlPitchF', 'Final pitch angle for maneuvers', ErrStat, ErrMsg, UnEc=UnEc )

IF ( p%NumBl == 2 )  THEN
   CALL ReadCom ( UnIn, PriFile, 'unused BlPitchF(3)', UnEc=UnEc )
ENDIF



!  ------- ENVIRONMENTAL CONDITIONS --------------------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'environmental conditions', UnEc=UnEc )


   ! Gravity - Gravitational acceleration.

CALL ReadVar ( UnIn, PriFile, p%Gravity, 'Gravity', 'Gravitational acceleration', UnEc=UnEc )

IF ( p%Gravity < 0.0 )  CALL ProgAbort ( ' Gravity must not be negative.' )



!  -------------- FEATURE SWITCHES ---------------------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'degree of freedom switches', UnEc=UnEc )


   ! FlapDOF1 - First flapwise blade mode DOF.

CALL ReadVar ( UnIn, PriFile, InputFileData%FlapDOF1, 'FlapDOF1', 'First flapwise blade mode DOF', UnEc=UnEc )


   ! FlapDOF2 - Second flapwise blade mode DOF.

CALL ReadVar ( UnIn, PriFile, InputFileData%FlapDOF2, 'FlapDOF2', 'Second flapwise blade mode DOF', UnEc=UnEc )

IF ( InputFileData%FlapDOF2 .AND. ( .NOT. InputFileData%FlapDOF1 ) )  THEN  ! Print out warning when flap mode 1 is not enabled and flap mode 2 is enabled
   CALL ProgWarn('  2nd blade flap mode is enabled without the 1st. This designation is only recommended for debugging purposes.' )
ENDIF


   ! EdgeDOF - First edgewise blade mode DOF.

CALL ReadVar ( UnIn, PriFile, InputFileData%EdgeDOF, 'EdgeDOF', 'First edgewise blade mode DOF', UnEc=UnEc )


   ! TeetDOF - Teeter DOF.

IF ( p%NumBl == 2 )  THEN
   CALL ReadVar ( UnIn, PriFile, InputFileData%TeetDOF, 'TeetDOF', 'Teeter DOF', UnEc=UnEc )
ELSE
   InputFileData%TeetDOF = .FALSE.
   CALL ReadCom ( UnIn, PriFile, 'unused TeetDOF', UnEc=UnEc )
ENDIF


   ! DrTrDOF - Drivetrain rotational-flexibility DOF.

CALL ReadVar ( UnIn, PriFile, InputFileData%DrTrDOF, 'DrTrDOF', 'Drivetrain rotational-flexibility DOF', UnEc=UnEc )


   ! GenDOF - Generator DOF.

CALL ReadVar ( UnIn, PriFile, InputFileData%GenDOF, 'GenDOF', 'Generator DOF', UnEc=UnEc )


   ! YawDOF - Yaw DOF.

CALL ReadVar ( UnIn, PriFile, InputFileData%YawDOF, 'YawDOF', 'Yaw DOF', UnEc=UnEc )


   ! TwFADOF1 - First tower fore-aft bending-mode DOF.

CALL ReadVar ( UnIn, PriFile, InputFileData%TwFADOF1, 'TwFADOF1', 'First tower fore-aft bending-mode DOF', UnEc=UnEc )


   ! TwFADOF2 - Second tower fore-aft bending-mode DOF.

CALL ReadVar ( UnIn, PriFile, InputFileData%TwFADOF2, 'TwFADOF2', 'Second tower fore-aft bending-mode DOF', UnEc=UnEc )

IF ( InputFileData%TwFADOF2 .AND. ( .NOT. InputFileData%TwFADOF1 ) )  THEN  ! Print out warning when tower fore-aft mode 1 is not enabled and fore-aft mode 2 is enabled
   CALL ProgWarn('  2nd tower fore-aft mode is enabled without the 1st. This designation is only recommended for debugging purposes.' )
ENDIF


   ! TwSSDOF1 - First tower side-to-side bending-mode DOF.

CALL ReadVar ( UnIn, PriFile, InputFileData%TwSSDOF1, 'TwSSDOF1', 'First tower side-to-side bending-mode DOF', UnEc=UnEc )


   ! TwSSDOF2 - Second tower side-to-side bending-mode DOF.

CALL ReadVar ( UnIn, PriFile, InputFileData%TwSSDOF2, 'TwSSDOF2', 'Second tower side-to-side bending-mode DOF', UnEc=UnEc )

IF ( InputFileData%TwSSDOF2 .AND. ( .NOT. InputFileData%TwSSDOF1 ) )  THEN  ! Print out warning when tower side-to-side mode 1 is not enabled and side-to-side mode 2 is enabled
   CALL ProgWarn('  2nd tower side-to-side mode is enabled without the 1st. This designation is only recommended for debugging purposes.')
ENDIF


   ! CompAero - Compute aerodynamic forces.

CALL ReadVar ( UnIn, PriFile, CompAero, 'CompAero', 'Compute aerodynamic forces', UnEc=UnEc )


   ! CompNoise - Compute aerodynamic noise.

CALL ReadVar ( UnIn, PriFile, CompNoise, 'CompNoise', 'Compute aerodynamic noise', UnEc=UnEc )



!  -------------- INITIAL CONDITIONS -------------------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'initial conditions', UnEc=UnEc )


   ! OoPDefl - Initial out-of-plane blade-tip deflection.

CALL ReadVar ( UnIn, PriFile, InputFileData%OoPDefl, 'OoPDefl', 'Initial out-of-plane blade-tip deflection', UnEc=UnEc )

IF ( (Cmpl4SFun .OR. Cmpl4LV) .AND. ( InputFileData%OoPDefl /= 0.0 ) )  &
   CALL ProgAbort ( ' Initial out-of-plane blade-tip displacements must be zero when FAST is interfaced with Simulink'// &
                ' or Labview. Set OoPDefl to 0.0 or use the standard version of FAST.'                )


   ! IPDefl - Initial in-plane blade-tip deflection.

CALL ReadVar ( UnIn, PriFile, InputFileData%IPDefl, 'IPDefl', 'Initial in-plane blade-tip deflection', UnEc=UnEc )

IF ( (Cmpl4SFun .OR. Cmpl4LV) .AND. ( InputFileData%IPDefl  /= 0.0 ) )  &
   CALL ProgAbort ( ' Initial in-plane blade-tip displacements must be zero when FAST is interfaced with Simulink'// &
                ' or Labview. Set IPDefl to 0.0 or use the standard version of FAST.'                 )


   ! TeetDefl - Initial or fixed teeter angle.

IF ( p%NumBl == 2 )  THEN
   CALL ReadVar ( UnIn, PriFile, InputFileData%TeetDefl, 'TeetDefl', 'Initial or fixed teeter angle', UnEc=UnEc )
   IF ( ( InputFileData%TeetDefl <= -180.0 ) .OR. ( InputFileData%TeetDefl > 180.0 ) )  &
      CALL ProgAbort ( ' TeetDefl must be greater than -180 and less than or equal to 180.' )
   InputFileData%TeetDefl = InputFileData%TeetDefl*D2R   
ELSE
   CALL ReadCom ( UnIn, PriFile, 'unused Teeter', UnEc=UnEc )
   InputFileData%TeetDefl = 0.0
ENDIF


   ! Azimuth - Initial azimuth position for blade 1.

CALL ReadVar ( UnIn, PriFile, InputFileData%Azimuth, 'Azimuth', 'Initial azimuth position for blade 1', UnEc=UnEc )

IF ( ( InputFileData%Azimuth < 0.0 ) .OR. ( InputFileData%Azimuth >= 360.0 ) )  &
   CALL ProgAbort ( ' Azimuth must be greater or equal to 0 and less than 360.' )


   ! RotSpeed - Initial or fixed rotor speed.

CALL ReadVar ( UnIn, PriFile, InputFileData%RotSpeed, 'RotSpeed', 'Initial or fixed rotor speed', UnEc=UnEc )
IF ( InputFileData%RotSpeed < 0.0 )  CALL ProgAbort ( ' RotSpeed must not be negative.' )
InputFileData%RotSpeed = InputFileData%RotSpeed*RPM2RPS


   ! NacYaw - Initial or fixed nacelle-yaw angle.

CALL ReadVar ( UnIn, PriFile, NacYaw, 'NacYaw', 'Initial or fixed nacelle-yaw angle', UnEc=UnEc )

IF ( ( NacYaw <= -180.0 ) .OR. ( NacYaw > 180.0 ) )  &
   CALL ProgAbort ( ' NacYaw must be greater than -180 and less than or equal to 180.' )

NacYaw    = D2R*NacYaw                                                          ! Nacelle yaw in radians.

   ! TTDspFA - Initial fore-aft tower-top displacement.

CALL ReadVar ( UnIn, PriFile, InputFileData%TTDspFA, 'TTDspFA', 'Initial fore-aft tower-top displacement', UnEc=UnEc )

IF ( (Cmpl4SFun .OR. Cmpl4LV) .AND. ( InputFileData%TTDspFA /= 0.0 ) )  &
   CALL ProgAbort ( ' Initial fore-aft tower-top displacements must be zero when FAST is interfaced with Simulink'// &
                ' or Labview. Set TTDspFA to 0.0 or use the standard version of FAST.'               )


   ! TTDspSS - Initial side-to-side tower-top displacement.

CALL ReadVar ( UnIn, PriFile, InputFileData%TTDspSS, 'TTDspSS', 'Initial side-to-side tower-top displacement', UnEc=UnEc )

IF ( (Cmpl4SFun .OR. Cmpl4LV) .AND. ( InputFileData%TTDspSS /= 0.0 ) )  &
   CALL ProgAbort ( ' Initial side-to-side tower-top displacements must be zero when FAST is interfaced with Simulink'// &
                ' or Labview. Set TTDspSS to 0.0 or use the standard version of FAST.'                      )



!  -------------- TURBINE CONFIGURATION ----------------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'turbine configuration', UnEc=UnEc )


   ! TipRad - Preconed blade-tip radius.

CALL ReadVar ( UnIn, PriFile, p%TipRad, 'TipRad', 'Preconed blade-tip radius', UnEc=UnEc )

IF ( p%TipRad < 0.0 )  CALL ProgAbort ( ' TipRad must be greater than 0.' )


   ! HubRad - Preconed hub radius.

CALL ReadVar ( UnIn, PriFile, p%HubRad, 'HubRad', 'Preconed hub radius', UnEc=UnEc )

IF ( ( p%HubRad < 0.0 ) .OR. ( p%HubRad >= p%TipRad ) ) THEN
   CALL ProgAbort ( ' HubRad must be between 0 (inclusive) and TipRad (exclusive).' )
END IF


   ! PSpnElN - Number of the innermost blade element which is still part of the pitchable portion of the blade for partial-span pitch control.

!JASON: ADD LOGIC FOR THIS NEW VARIABLE:
!JASON:CALL ReadVar ( UnIn, PriFile, p%PSpnElN, 'PSpnElN', 'Partial-span pitch control element number', UnEc=UnEc )
   CALL ReadCom ( UnIn, PriFile, 'currently ignored PSpnElN', UnEc=UnEc )

   ! UndSling - Undersling length.

IF ( p%NumBl == 2 )  THEN
   CALL ReadVar ( UnIn, PriFile, p%UndSling, 'UndSling', 'Undersling length', UnEc=UnEc )
ELSE
   CALL ReadCom ( UnIn, PriFile, 'unused UndSling', UnEc=UnEc )
   p%UndSling = 0.0
ENDIF


   ! HubCM - Distance from rotor apex to hub mass.

CALL ReadVar ( UnIn, PriFile, p%HubCM, 'HubCM', 'Distance from rotor apex to hub mass', UnEc=UnEc )


   ! OverHang - Distance from yaw axis to rotor apex or teeter pin.

CALL ReadVar ( UnIn, PriFile, p%OverHang, 'OverHang', 'Distance from yaw axis to rotor apex or teeter pin', UnEc=UnEc )


   ! NacCMxn - Downwind distance from tower-top to nacelle CM.

CALL ReadVar ( UnIn, PriFile, p%NacCMxn, 'NacCMxn', 'Downwind distance from tower-top to nacelle CM', UnEc=UnEc )


   ! NacCMyn - Lateral  distance from tower-top to nacelle CM.

CALL ReadVar ( UnIn, PriFile, p%NacCMyn, 'NacCMyn', 'Lateral  distance from tower-top to nacelle CM', UnEc=UnEc )


   ! NacCMzn - Vertical distance from tower-top to nacelle CM.

CALL ReadVar ( UnIn, PriFile, p%NacCMzn, 'NacCMzn', 'Vertical distance from tower-top to nacelle CM', UnEc=UnEc )


   ! TowerHt - Tower height.

CALL ReadVar ( UnIn, PriFile, p%TowerHt, 'TowerHt', 'Tower height', UnEc=UnEc )

IF ( p%TowerHt <= 0.0 )  CALL ProgAbort ( ' TowerHt must be greater than zero.' )


   ! Twr2Shft - Vertical distance from tower-top rotor shaft.

CALL ReadVar ( UnIn, PriFile, InputFileData%Twr2Shft, 'Twr2Shft', 'Vertical distance from tower-top to rotor shaft', UnEc=UnEc )

IF ( InputFileData%Twr2Shft < 0.0 )  CALL ProgAbort ( ' Twr2Shft cannot be negative.' )


   ! TwrRBHt - Tower rigid base height.

CALL ReadVar ( UnIn, PriFile, p%TwrRBHt, 'TwrRBHt', 'Tower rigid base height', UnEc=UnEc )

IF ( p%TwrRBHt < 0.0 )  CALL ProgAbort ( ' TwrRBHt must be greater or equal to 0 and less than TowerHt + TwrDraft.' )


   ! ShftTilt - Rotor shaft tilt angle.

CALL ReadVar ( UnIn, PriFile, InputFileData%ShftTilt, 'ShftTilt', 'Rotor shaft tilt angle', UnEc=UnEc )

IF ( ( InputFileData%ShftTilt < -90.0 ) .OR. ( InputFileData%ShftTilt > 90.0 ) )              &
   CALL ProgAbort ( ' ShftTilt must be between -90 and 90 (inclusive).' )

InputFileData%ShftTilt  = InputFileData%ShftTilt*D2R

IF ( p%TowerHt + InputFileData%Twr2Shft + p%OverHang*SIN(InputFileData%ShftTilt) <= p%TipRad )  &
   CALL ProgAbort ( ' TowerHt + Twr2Shft + OverHang*SIN(ShftTilt) must be greater than TipRad.' )



   ! Delta3 - Delta-3 angle for teetering rotors.

IF ( p%NumBl == 2 )  THEN
   CALL ReadVar ( UnIn, PriFile, InputFileData%Delta3, 'Delta3', 'Delta-3 angle for teetering rotors', UnEc=UnEc )
   IF ( ( InputFileData%Delta3 <= -90.0 ) .OR. ( InputFileData%Delta3 >= 90.0 ) )  CALL ProgAbort ( ' Delta3 must be between -90 and 90 (exclusive).' )
   InputFileData%Delta3   = InputFileData%Delta3  *D2R
ELSE
   CALL ReadCom ( UnIn, PriFile, 'unused Delta3', UnEc=UnEc )
   InputFileData%Delta3 = 0.0
ENDIF


   ! PreCone - Blade coning angle.

ALLOCATE ( p%PreCone(p%NumBl) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the PreCone array.' )
ENDIF

CALL ReadAryLines ( UnIn, PriFile, p%PreCone, p%NumBl, 'PreCone', 'Blade coning angle', ErrStat, ErrMsg, UnEc=UnEc )

DO K=1,p%NumBl
   IF ( ( p%PreCone(K) <= -90.0 ) .OR. ( p%PreCone(K) >= 90.0 ) )  THEN
      CALL ProgAbort ( ' PreCone('//TRIM( Num2LStr( K ) )//') must be between -90 and 90 degrees (exclusive).' )
   ENDIF
   
   p%Precone  (K) = p%Precone (K)*D2R   
ENDDO ! K

IF ( p%NumBl == 2 )  THEN
   CALL ReadCom ( UnIn, PriFile, 'unused Beta(3)', UnEc=UnEc )
ENDIF


   ! AzimB1Up - Azimuth value to use for I/O when blade 1 points up.

CALL ReadVar ( UnIn, PriFile, p%AzimB1Up, 'AzimB1Up', 'Azimuth value to use for I/O when blade 1 points up', UnEc=UnEc )

IF ( ( p%AzimB1Up < 0.0 ) .OR. ( p%AzimB1Up > 360.0 ) )  CALL ProgAbort ( ' AzimB1Up must be between 0 and 360 (inclusive).' )



!  -------------- MASS AND INERTIA ---------------------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'mass and inertia', UnEc=UnEc )


   ! YawBrMass - Yaw bearing mass.

CALL ReadVar ( UnIn, PriFile, p%YawBrMass, 'YawBrMass', 'Yaw bearing mass', UnEc=UnEc )

IF ( p%YawBrMass < 0.0 )  CALL ProgAbort ( ' YawBrMass must not be negative.' )


   ! NacMass - Nacelle mass.

CALL ReadVar ( UnIn, PriFile, p%NacMass, 'NacMass', 'Nacelle mass', UnEc=UnEc )

IF ( p%NacMass < 0.0 )  CALL ProgAbort ( ' NacMass must not be negative.' )


   ! HubMass - Hub mass.

CALL ReadVar ( UnIn, PriFile, p%HubMass, 'HubMass', 'Hub mass', UnEc=UnEc )

IF ( p%HubMass < 0.0 )  CALL ProgAbort ( ' HubMass must not be negative.' )


   ! TipMass - Tip-brake mass.

ALLOCATE ( p%TipMass(p%NumBl) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the TipMass array.' )
ENDIF

CALL ReadAryLines ( UnIn, PriFile, p%TipMass, p%NumBl, 'TipMass', 'Tip-brake mass', ErrStat, ErrMsg, UnEc=UnEc )

DO K=1,p%NumBl
   IF ( p%TipMass(K) < 0.0 )  THEN
      CALL ProgAbort ( ' TipMass('//TRIM( Num2LStr( K ) )//') must not be negative.' )
   ENDIF
ENDDO ! K

IF ( p%NumBl == 2 )  THEN
   CALL ReadCom ( UnIn, PriFile, 'unused TipMass(3)', UnEc=UnEc )
ENDIF


   ! NacYIner - Nacelle yaw inertia.

CALL ReadVar ( UnIn, PriFile, InputFileData%NacYIner, 'NacYIner', 'Nacelle yaw inertia', UnEc=UnEc )

IF ( InputFileData%NacYIner < 0.0 )  CALL ProgAbort ( ' NacYIner must not be negative.' )


   ! GenIner - Generator inertia about HSS.

CALL ReadVar ( UnIn, PriFile, p%GenIner, 'GenIner', 'Generator inertia about HSS', UnEc=UnEc )

IF ( p%GenIner < 0.0 )  CALL ProgAbort ( ' GenIner must not be negative.' )


   ! HubIner - Hub inertia about teeter axis (2-blader) or rotor axis (3-blader).

IF ( p%NumBl == 2 )  THEN
   CALL ReadVar ( UnIn, PriFile, InputFileData%HubIner, 'HubIner', 'Hub inertia about teeter axis', UnEc=UnEc )
ELSE
   CALL ReadVar ( UnIn, PriFile, InputFileData%HubIner, 'HubIner', 'Hub inertia about rotor axis', UnEc=UnEc )
ENDIF

IF ( InputFileData%HubIner < 0.0 )  CALL ProgAbort ( ' HubIner must not be negative.' )



!  -------------- DRIVETRAIN PARAMETERS ----------------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'drivetrain parameters', UnEc=UnEc )


   ! GBoxEff - Gearbox efficiency.

CALL ReadVar ( UnIn, PriFile, p%GBoxEff, 'GBoxEff', 'Gearbox efficiency', UnEc=UnEc )

IF ( ( p%GBoxEff <= 0.0 ) .OR. ( p%GBoxEff > 100.0 ) ) THEN
   CALL ProgAbort ( ' GBoxEff must be greater than 0 and less than or equal to 100.' )
END IF   




   ! GenEff - Generator efficiency.

CALL ReadVar ( UnIn, PriFile, p%GenEff, 'GenEff', 'Generator efficiency', UnEc=UnEc )

IF ( ( p%GenEff < 0.0 ) .OR. ( p%GenEff > 100.0 ) )  CALL ProgAbort ( ' GenEff must be between 0 and 100 (inclusive).' )


   ! GBRatio - Gearbox ratio.

CALL ReadVar ( UnIn, PriFile, p%GBRatio, 'GBRatio', 'Gearbox ratio', UnEc=UnEc )

!IF ( p%GBRatio <= 0.0 )  CALL ProgAbort ( ' GBRatio must be greater than 0.' )


!!JASON: ELIMINATE THIS INPUT BY ALLOWING p%GBRatio TO BE NEGATIVE!!!!!<--ACTUALLY, DON'T DO THIS SINCE WE ALWAYS WANT THE HSS SPEED TO REMAIN POSITIVE AND THE TORQUE TO DEPEND ON WHETHER WE ARE PRODUCING POWER OR MOTORING UP.
!   ! GBRevers - Gearbox reversal flag.
!
!CALL ReadVar ( UnIn, PriFile, InputFileData%GBRevers, 'GBRevers', 'Gearbox reversal flag', UnEc=UnEc )
!
CALL ReadCom ( UnIn, PriFile, 'unused Gearbox reversal flag', UnEc=UnEc )

   ! HSSBrTqF - Fully deployed HSS brake torque.

CALL ReadVar ( UnIn, PriFile, HSSBrTqF, 'HSSBrTqF', 'Fully deployed HSS brake torque', UnEc=UnEc )

IF ( HSSBrTqF < 0.0 )  CALL ProgAbort ( ' HSSBrTqF must not be negative.' )


   ! HSSBrDT - Time for HSS-brake to reach full deployment once initiated.

CALL ReadVar ( UnIn, PriFile, HSSBrDT, 'HSSBrDT', 'Time for HSS-brake to reach full deployment once initiated', UnEc=UnEc )

IF ( HSSBrDT < 0.0 )  CALL ProgAbort ( ' HSSBrDT must not be negative.' )


   ! DynBrkFi - Name of file containing dynamic generator brake properties.

!JASON: ADD LOGIC FOR THIS NEW VARIABLE:
!JASON:CALL ReadVar ( UnIn, PriFile, DynBrkFi, 'DynBrkFi', 'Name of file containing dynamic generator brake properties', UnEc=UnEc )
!JASON:
!JASON:IF ( LEN_TRIM( DynBrkFi ) == 0 )  CALL ProgAbort ( ' DynBrkFi must not be an empty string.' )

   CALL ReadCom ( UnIn, PriFile, 'currently ignored DynBrkFi', UnEc=UnEc )


   ! DTTorSpr - Drivetrain torsional spring.

CALL ReadVar ( UnIn, PriFile, p%DTTorSpr, 'DTTorSpr', 'Drivetrain torsional spring', UnEc=UnEc )

IF ( p%DTTorSpr < 0.0 )  CALL ProgAbort ( ' DTTorSpr must not be negative.' )


   ! DTTorDmp - Drivetrain torsional damper.

CALL ReadVar ( UnIn, PriFile, p%DTTorDmp, 'DTTorDmp', 'Drivetrain torsional damper', UnEc=UnEc )

IF ( p%DTTorDmp < 0.0 )  CALL ProgAbort ( ' DTTorDmp must not be negative.' )



!  -------------- SIMPLE-INDUCTION-GENERATOR PARAMETERS ------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'simple-induction-generator parameters', UnEc=UnEc )


   ! SIG_SlPc - Rated generator slip percentage.

CALL ReadVar ( UnIn, PriFile, SIG_SlPc, 'SIG_SlPc', 'Rated generator slip percentage', UnEc=UnEc )

IF ( ( GenModel == 1 )  .AND. ( SIG_SlPc <= 0.0 ) )  CALL ProgAbort ( ' SIG_SlPc must be greater than zero.' )


   ! SIG_SySp - Synchronous (zero-torque) generator speed.

CALL ReadVar ( UnIn, PriFile, SIG_SySp, 'SIG_SySp', 'Synchronous (zero-torque) generator speed', UnEc=UnEc )

IF ( ( GenModel == 1 )  .AND. ( SIG_SySp <= 0 ) )  CALL ProgAbort ( ' SIG_SySp must be greater than zero.' )


   ! SIG_RtTq - Rated torque.

CALL ReadVar ( UnIn, PriFile, SIG_RtTq, 'SIG_RtTq', 'Rated torque', UnEc=UnEc )

IF ( ( GenModel == 1 )  .AND. ( SIG_RtTq <= 0.0 ) )  CALL ProgAbort ( ' SIG_RtTq must be greater than zero.' )


   ! SIG_PORt - Pull-out ratio.

CALL ReadVar ( UnIn, PriFile, SIG_PORt, 'SIG_PORt', 'Pull-out ratio', UnEc=UnEc )

IF ( ( GenModel == 1 )  .AND. ( SIG_PORt < 1.0 ) )  CALL ProgAbort ( ' SIG_PORt must not be less than 1.' )



!  -------------- THEVENIN-EQUIVALENT INDUCTION-GENERATOR PARAMETERS -----------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'Thevenin-equivalent induction-generator parameters', UnEc=UnEc )


   ! TEC_Freq - Line frequency.

CALL ReadVar ( UnIn, PriFile, TEC_Freq, 'TEC_Freq', 'Line frequency', UnEc=UnEc )

IF ( ( GenModel == 2 )  .AND. ( TEC_Freq <= 0.0 ) )  CALL ProgAbort ( ' TEC_Freq must be greater than zero.' )


   ! TEC_NPol - Number of poles.

CALL ReadVar ( UnIn, PriFile, TEC_NPol, 'TEC_NPol', 'Number of poles', UnEc=UnEc )

IF ( ( GenModel == 2 )  .AND. ( ( TEC_NPol <= 0 ) .OR. ( MOD( TEC_NPol, 2 ) /= 0 ) ) )  &
     CALL ProgAbort ( ' TEC_NPol must be an even number greater than zero.' )


   ! TEC_SRes - Stator resistance.

CALL ReadVar ( UnIn, PriFile, TEC_SRes, 'TEC_SRes', 'Stator resistance', UnEc=UnEc )

IF ( ( GenModel == 2 )  .AND. ( TEC_SRes <= 0.0 ) )  CALL ProgAbort ( ' TEC_SRes must be greater than zero.' )


   ! TEC_RRes - Rotor resistance.

CALL ReadVar ( UnIn, PriFile, TEC_RRes, 'TEC_RRes', 'Rotor resistance', UnEc=UnEc )

IF ( ( GenModel == 2 )  .AND. ( TEC_RRes <= 0.0 ) )  CALL ProgAbort ( ' TEC_RRes must be greater than zero.' )


   ! TEC_VLL - Line-to-line RMS voltage.

CALL ReadVar ( UnIn, PriFile, TEC_VLL, 'TEC_VLL', 'Line-to-line RMS voltage', UnEc=UnEc )

IF ( ( GenModel == 2 )  .AND. ( TEC_VLL <= 0.0 ) )  CALL ProgAbort ( ' TEC_VLL must be greater than zero.' )


   ! TEC_SLR - Stator leakage reactance.

CALL ReadVar ( UnIn, PriFile, TEC_SLR, 'TEC_SLR', 'Stator leakage reactance', UnEc=UnEc )

IF ( ( GenModel == 2 )  .AND. ( TEC_SLR <= 0.0 ) )  CALL ProgAbort ( ' TEC_SLR must be greater than zero.' )


   ! TEC_RLR  - Rotor leakage reactance.

CALL ReadVar ( UnIn, PriFile, TEC_RLR, 'TEC_RLR', 'Rotor leakage reactance', UnEc=UnEc )

IF ( ( GenModel == 2 )  .AND. ( TEC_RLR <= 0.0 ) )  CALL ProgAbort ( ' TEC_RLR must be greater than zero.' )


   ! TEC_MR - Magnetizing reactance.

CALL ReadVar ( UnIn, PriFile, TEC_MR, 'TEC_MR', 'Magnetizing reactance', UnEc=UnEc )

IF ( ( GenModel == 2 )  .AND. ( TEC_MR <= 0.0 ) )  CALL ProgAbort ( ' TEC_MR must be greater than zero.' )


!  -------------- PLATFORM PARAMETERS ------------------------------------------


   ! Skip the comment line.

CALL ReadCom ( UnIn, PriFile, 'platform parameters', UnEc=UnEc  )

!   ! PtfmModel - Platform model switch.
!
!CALL ReadVar ( UnIn, PriFile, PtfmModel, 'PtfmModel', 'Platform model switch', UnEc=UnEc )
!
!IF ( ( PtfmModel < 0 ) .OR. ( PtfmModel > 3 ) )  CALL ProgAbort ( ' PtfmModel must be either 0, 1, 2, or 3.' )
!
   ! Platform - Flag for additional platform data from file 

CALL ReadVar( UnIn, PriFile, Platform, 'Platform', 'Read additional platform properties from file?' )


   ! PtfmFile - Name of file containing platform properties.

CALL ReadVar ( UnIn, PriFile, PtfmFile, 'PtfmFile', 'Name of file containing platform properties', UnEc=UnEc )

IF ( Platform .AND. LEN_TRIM( PtfmFile ) == 0 )  CALL ProgAbort ( ' PtfmFile must not be an empty string.' ) 
!IF ( LEN_TRIM( PtfmFile ) == 0 .AND. PtfmModel /= 0 )  CALL ProgAbort ( ' PtfmFile must not be an empty string.' ) 
IF ( PathIsRelative( PtfmFile ) ) PtfmFile = TRIM(PriPath)//TRIM(PtfmFile)



!  -------------- TOWER PARAMETERS ---------------------------------------------


   ! Skip the comment line.

CALL ReadCom ( UnIn, PriFile, 'tower parameters', UnEc=UnEc )

   ! TwrNodes - Number of tower nodes used for analysis.

CALL ReadVar ( UnIn, PriFile, InputFileData%TwrNodes, 'TwrNodes', 'Number of tower nodes used for analysis', UnEc=UnEc )

IF ( InputFileData%TwrNodes < 1 )  CALL ProgAbort ( ' TwrNodes must not be less than 1.' )


   ! TwrFile - Name of file containing tower properties.

CALL ReadVar ( UnIn, PriFile, TwrFile, 'TwrFile', 'Name of file containing tower properties', UnEc=UnEc )

IF ( LEN_TRIM( TwrFile ) == 0 )  CALL ProgAbort ( ' TwrFile must not be an empty string.' )
IF ( PathIsRelative( TwrFile ) ) TwrFile = TRIM(PriPath)//TRIM(TwrFile)


   ! TwrLdMod - Additional tower loading model
CALL ReadVar ( UnIn, PriFile, p%TwrLdMod, 'TwrLdMod', 'Additional tower loading model' )

IF ( p%TwrLdMod /= 0 .AND. p%TwrLdMod /= 1 ) THEN
   CALL ProgAbort ( ' TwrLdMod must be either 0 or 1.' )
END IF   


!  -------------- NACELLE-YAW PARAMETERS ---------------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'nacelle-yaw parameters', UnEc=UnEc )


   ! YawSpr - Yaw spring constant.

CALL ReadVar ( UnIn, PriFile, p%YawSpr, 'YawSpr', 'Nacelle-yaw spring constant', UnEc=UnEc )

IF ( p%YawSpr < 0.0 )  CALL ProgAbort ( ' YawSpr must not be negative.' )


   ! YawDamp - Yaw damping constant.

CALL ReadVar ( UnIn, PriFile, p%YawDamp, 'YawDamp', 'Nacelle-yaw damping constant', UnEc=UnEc )

IF ( p%YawDamp < 0.0 )  CALL ProgAbort ( ' YawDamp must not be negative.' )


   ! YawNeut - Neutral yaw position.

CALL ReadVar ( UnIn, PriFile, YawNeut, 'YawNeut', 'Neutral yaw position', UnEc=UnEc )

IF ( ( YawNeut <= -180.0 ) .OR. ( YawNeut > 180.0 ) )  &
   CALL ProgAbort ( ' YawNeut must be greater than -180 and less than or equal to 180.' )

YawNeut   = D2R*YawNeut                                                         ! Neutral yaw in radians.


!  -------------- FURLING PARAMETERS -------------------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'furling parameters', UnEc=UnEc )

   ! Furling - Read in additional model properties for furling turbine.

CALL ReadVar ( UnIn, PriFile, InputFileData%Furling, 'Furling', 'Read in additional model properties for furling turbine', UnEc=UnEc )
InputFileData%Furling = .FALSE.

IF ( InputFileData%Furling .AND. ( p%OverHang > 0.0 ) )  THEN   ! Print out warning when downwind turbine is modeled with furling.
   CALL UsrAlarm

   CALL WrScr1(' WARNING: ')
   CALL WrScr ('  Furling designation (Furling = True) specified for downwind rotor configuration (OverHang > 0). ')
   CALL WrScr ('  Check for possible errors in the input file(s). ')
ENDIF


   ! FurlFile - Name of file containing furling properties.

CALL ReadVar ( UnIn, PriFile, FurlFile, 'FurlFile', 'Name of file containing furling properties', UnEc=UnEc )

IF ( LEN_TRIM( FurlFile ) == 0 .AND. InputFileData%Furling )  CALL ProgAbort ( ' FurlFile must not be an empty string.' )
IF ( PathIsRelative( FurlFile ) ) FurlFile = TRIM(PriPath)//TRIM(FurlFile)


!  -------------- ROTOR-TEETER PARAMETERS --------------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'rotor-teeter parameters', UnEc=UnEc )


IF ( p%NumBl == 2 )  THEN

      ! TeetMod - Rotor-teeter spring/damper model switch.

   CALL ReadVar ( UnIn, PriFile, p%TeetMod, 'TeetMod', 'Rotor-teeter spring/damper model switch', UnEc=UnEc )

   IF ( ( p%TeetMod /= 0 ) .AND. ( p%TeetMod /= 1 ) .AND. ( p%TeetMod /= 2 ) )  CALL ProgAbort ( ' TeetMod must be 0, 1, or 2.' )


      ! TeetDmpP - Rotor-teeter damper position.

   CALL ReadVar ( UnIn, PriFile, p%TeetDmpP, 'TeetDmpP', 'Rotor-teeter damper position', UnEc=UnEc )

   IF ( ( p%TeetDmpP < 0.0 ) .OR. ( p%TeetDmpP > 180.0 ) )  CALL ProgAbort ( ' TeetDmpP must be between 0 and 180 (inclusive).' )
   p%TeetDmpP = p%TeetDmpP*D2R

      ! TeetDmp - Rotor-teeter damping constant.

   CALL ReadVar ( UnIn, PriFile, p%TeetDmp, 'TeetDmp', 'Rotor-teeter damping constant', UnEc=UnEc )

   IF ( p%TeetDmp < 0.0 )  CALL ProgAbort ( ' TeetDmp must not be negative.' )


      ! TeetCDmp - Rotor-teeter rate-independent Coulomb-damping moment.

   CALL ReadVar ( UnIn, PriFile, p%TeetCDmp, 'TeetCDmp', 'Rotor-teeter rate-independent Coulomb-damping moment', UnEc=UnEc )

   IF ( p%TeetCDmp < 0.0 )  CALL ProgAbort ( ' TeetCDmp must not be negative.' )


      ! TeetSStP - Rotor-teeter soft-stop position.

   CALL ReadVar ( UnIn, PriFile, p%TeetSStP, 'TeetSStP', 'Rotor-teeter soft-stop position', UnEc=UnEc )

   IF ( ( p%TeetSStP < 0.0 ) .OR. ( p%TeetSStP > 180.0 ) )  CALL ProgAbort ( ' TeetSStP must be between 0 and 180 (inclusive).' )
   p%TeetSStP = p%TeetSStP*D2R

      ! TeetHStP - Rotor-teeter hard-stop position.

   CALL ReadVar ( UnIn, PriFile, p%TeetHStP, 'TeetHStP', 'Rotor-teeter hard-stop position', UnEc=UnEc )
   p%TeetHStP = p%TeetHStP*D2R
   IF ( ( p%TeetHStP < p%TeetSStP ) .OR. ( p%TeetHStP > pi ) )  &
      CALL ProgAbort ( ' TeetHStP must be between TeetSStP  and 180 (inclusive).' )


      ! TeetSSSp - Rotor-teeter soft-stop linear-spring constant.

   CALL ReadVar ( UnIn, PriFile, p%TeetSSSp, 'TeetSSSp', 'Rotor-teeter soft-stop linear-spring constant', UnEc=UnEc )

   IF ( p%TeetSSSp < 0.0 )  CALL ProgAbort ( ' TeetSSSp must not be negative.' )


      ! TeetHSSp - Rotor-teeter hard-stop linear-spring constant.

   CALL ReadVar ( UnIn, PriFile, p%TeetHSSp, 'TeetHSSp', 'Rotor-teeter hard-stop linear-spring constant', UnEc=UnEc )

   IF ( p%TeetHSSp < 0.0 )  CALL ProgAbort ( ' TeetHSSp must not be negative.' )

ELSE


      ! Three-bladed turbines don't use these parameters, so skip them.

   CALL ReadCom ( UnIn, PriFile, 'unused TeetMod', UnEc=UnEc  )
   CALL ReadCom ( UnIn, PriFile, 'unused TeetDmpP', UnEc=UnEc )
   CALL ReadCom ( UnIn, PriFile, 'unused TeetDmp' , UnEc=UnEc )
   CALL ReadCom ( UnIn, PriFile, 'unused TeetCDmp', UnEc=UnEc )
   CALL ReadCom ( UnIn, PriFile, 'unused TeetSStP', UnEc=UnEc )
   CALL ReadCom ( UnIn, PriFile, 'unused TeetHStP', UnEc=UnEc )
   CALL ReadCom ( UnIn, PriFile, 'unused TeetSSSp', UnEc=UnEc )
   CALL ReadCom ( UnIn, PriFile, 'unused TeetHSSp', UnEc=UnEc )

   p%TeetMod  = 0
   p%TeetCDmp = 0.0
   p%TeetDmp  = 0.0
   p%TeetDmpP = 0.0
   p%TeetHSSp = 0.0
   p%TeetHStP = 0.0
   p%TeetSSSp = 0.0
   p%TeetSStP = 0.0
ENDIF



!  -------------- TIP-BRAKE PARAMETERS -----------------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'tip-brake parameters', UnEc=UnEc )


   ! TBDrConN - Tip-brake drag constant during normal operation.

CALL ReadVar ( UnIn, PriFile, TBDrConN, 'TBDrConN', 'Tip-brake drag constant during normal operation', UnEc=UnEc )

IF ( TBDrConN < 0.0 )  CALL ProgAbort ( ' TBDrConN must not be negative.' )


   ! TBDrConD - Tip-brake drag constant during fully-deployed operation.

CALL ReadVar ( UnIn, PriFile, TBDrConD, 'TBDrConD', 'Tip-brake drag constant during fully-deployed operation', UnEc=UnEc )

IF ( TBDrConD < TBDrConN )  CALL ProgAbort( ' TBDrConD must not be less than TBDrConN.' )


   ! TpBrDT - Time for tip-brake to reach full deployment once released.

CALL ReadVar ( UnIn, PriFile, TpBrDT, 'TpBrDT', 'Time for tip-brake to reach full deployment once released', UnEc=UnEc )

IF ( TpBrDT < 0.0 )  CALL ProgAbort ( ' TpBrDT must not be negative.' )


!  -------------- BLADE PARAMETERS ---------------------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'blade parameters', UnEc=UnEc )


   ! BldFile - Names of files containing blade properties.

ALLOCATE ( BldFile(p%NumBl) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the BldFile array.' )
ENDIF

CALL ReadAryLines( UnIn, PriFile, BldFile, p%NumBl, 'BldFile', 'Names of files containing blade properties', ErrStat, ErrMsg, UnEc=UnEc )

DO K=1,p%NumBl
   IF ( LEN_TRIM( BldFile(K) ) == 0 )  THEN
      CALL ProgAbort ( 'BldFile('//TRIM( Num2LStr( K ) )//') must not be an empty string.' )
   ENDIF
   IF ( PathIsRelative( BldFile(K) ) ) BldFile(K) = TRIM(PriPath)//TRIM(BldFile(K))
ENDDO ! K

IF ( p%NumBl == 2 )  THEN
   CALL ReadCom ( UnIn, PriFile, 'unused BldFile(3)', UnEc=UnEc )
ENDIF



!  -------------- AERODYN INPUT FILE PARAMETERS -------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'AeroDyn parameters', UnEc=UnEc )


   ! ADFile - Name of file containing AeroDyn parameters.

CALL ReadVar( UnIn, PriFile, ADFile, 'ADFile', 'Name of file containing AeroDyn parameters', UnEc=UnEc )

IF ( LEN_TRIM( ADFile ) == 0 )  CALL ProgAbort ( 'ADFile must not be an empty string.' )
IF ( PathIsRelative( ADFile ) ) ADFile = TRIM(PriPath)//TRIM(ADFile)



!  -------------- NOISE INPUT FILE PARAMETERS --------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'Noise parameters', UnEc=UnEc )


   ! NoiseFile - Name of file containing aerodynamic noise parameters.

CALL ReadVar ( UnIn, PriFile, NoiseFile, 'NoiseFile', 'Name of file containing aerodynamic noise parameters', UnEc=UnEc )



!  -------------- ADAMS INPUT FILE PARAMETERS ----------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'ADAMS parameters', UnEc=UnEc )

   ! ADAMSFile - Name of file containing ADAMS-specific parameters.

CALL ReadVar ( UnIn, PriFile, ADAMSFile, 'ADAMSFile', 'Name of file containing ADAMS-specific properties', UnEc=UnEc )

IF ( LEN_TRIM( ADAMSFile ) == 0 .AND. ADAMSPrep /= 1)  CALL ProgAbort ( ' ADAMSFile must not be an empty string.' )
IF ( PathIsRelative( ADAMSFile ) ) ADAMSFile = TRIM(PriPath)//TRIM(ADAMSFile)


!  -------------- FAST LINEARIZATION CONTROL PARAMETERS ------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'Linearization parameters', UnEc=UnEc )


   ! LinFile - Name of file containing FAST linearization parameters.

CALL ReadVar ( UnIn, PriFile, LinFile, 'LinFile', 'Name of file containing FAST linearization parameters', UnEc=UnEc )

IF ( LEN_TRIM( LinFile ) == 0 .AND. AnalMode /= 1)  CALL ProgAbort ( ' LinFile must not be an empty string.' )
IF ( PathIsRelative( LinFile ) ) LinFile = TRIM(PriPath)//TRIM(LinFile)


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


   ! NcIMUxn - Downwind distance from the tower-top to the nacelle IMU.

CALL ReadVar ( UnIn, PriFile, InputFileData%NcIMUxn, 'NcIMUxn', 'Downwind distance from the tower-top to the nacelle IMU', UnEc=UnEc )


   ! NcIMUyn - Lateral distance from the tower-top to the nacelle IMU.

CALL ReadVar ( UnIn, PriFile, InputFileData%NcIMUyn, 'NcIMUyn', 'Lateral distance from the tower-top to the nacelle IMU', UnEc=UnEc )


   ! NcIMUzn - Vertical distance from the tower-top to the nacelle IMU.

CALL ReadVar ( UnIn, PriFile, InputFileData%NcIMUzn, 'NcIMUzn', 'Vertical distance from the tower-top to the nacelle IMU', UnEc=UnEc )


   ! ShftGagL - Distance from hub or teeter pin to shaft strain gages.

CALL ReadVar ( UnIn, PriFile, p%ShftGagL, 'ShftGagL', 'Distance from hub or teeter pin to shaft strain gages', UnEc=UnEc )


   ! NTwGages - Number of tower "strain-gage" output stations.

CALL ReadVar ( UnIn, PriFile, p%NTwGages, 'NTwGages', 'Number of tower "strain-gage" output stations', UnEc=UnEc )

IF ( ( p%NTwGages < 0 ) .OR. ( p%NTwGages > 9 ) )  CALL ProgAbort ( ' NTwGages must be between 0 and 9 (inclusive).' )


   ! TwrGagNd - List of tower nodes that have strain gages.

CALL ReadAry( UnIn, PriFile, InputFileData%TwrGagNd, p%NTwGages, 'TwrGagNd', 'List of tower nodes that have strain gages', ErrStat, ErrMsg, UnEc )
   

   ! NBlGages - Number of blade "strain-gage" output stations.

CALL ReadVar ( UnIn, PriFile, p%NBlGages, 'NBlGages', 'Number of blade "strain-gage" output stations', UnEc=UnEc )

IF ( ( p%NBlGages < 0 ) .OR. ( p%NBlGages > 9 ) )  CALL ProgAbort ( ' NBlGages must be between 0 and 9 (inclusive).' )


   ! BldGagNd - List of blade nodes that have strain gages.

CALL ReadAry( UnIn, PriFile, InputFileData%BldGagNd, p%NBlGages, 'BldGagNd', 'List of blade nodes that have strain gages', ErrStat, ErrMsg, UnEc )


   ! Skip the comment line.

CALL ReadCom ( UnIn, PriFile, 'output-parameters list', UnEc=UnEc )


   ! OutList - Output parameter list.
  
CALL AllocAry( InputFileData%OutList, MaxOutPts, "ElastoDyn InputFile's Outlist", ErrStat, ErrMsg )
IF ( ErrStat >= AbortErrLev ) RETURN
   
   InputFileData%OutList = ''   ! Initialize OutList(:) to ''.
   p%NumOuts             = 0    ! Initialize NumOuts to zero.


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
SUBROUTINE GetPtfm( p, InputFileData, UnEc )

   ! This routine reads in the FAST platform input parameters from
   !   PtfmFile and validates the input.

USE                             General
USE                             InitCond
USE                             SimCont
USE                             Waves, ONLY:WavePkShpDefault
USE HydroVals
USE                             FAST_Hydro

IMPLICIT                        NONE

   ! Passed variables  
TYPE(ED_ParameterType),     INTENT(INOUT)    :: p                             ! Parameters of the structural dynamics module
TYPE(ED_InputFile),         INTENT(INOUT)    :: InputFileData                 ! all the data in the ElastoDyn input file
INTEGER(IntKi),               INTENT(IN)       :: UnEc                          ! I/O unit for echo file. If present and > 0, write to UnEc


   ! Local variables:
INTEGER(4)                   :: IOS                                             ! I/O status returned from the read statement.
CHARACTER(1024)              :: FilePath                                        ! Path name of the PtfmFile
CHARACTER(1024)              :: TmpPath                                         ! a variable to temporarially store the name of a file's path.


   ! Get the path name from the file so we can assume file names contained in this file are relative to this path
CALL GetPath( PtfmFile, FilePath )


   ! Open the FAST platform input file:

CALL OpenFInpFile ( UnIn, PtfmFile )



   ! Add a separator to the echo file if appropriate.

IF ( UnEc > 0 )  WRITE (UnEc,'(//,A,/)')  'Platform input data from file "'//TRIM( PtfmFile )//'":'



!  -------------- HEADER -------------------------------------------------------


   ! Skip the header.

READ (UnIn,'(//)',IOSTAT=IOS)

IF ( IOS < 0 )  THEN
   CALL PremEOF ( PtfmFile , 'unused FAST platform-file header' )
ENDIF



!  -------------- FEATURE SWITCHES (CONT) --------------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PtfmFile, 'degree of freedom switches (cont)', UnEc=UnEc )



   ! PtfmSgDOF - Platform horizontal surge translation DOF.

CALL ReadVar ( UnIn, PtfmFile, InputFileData%PtfmSgDOF, 'PtfmSgDOF', 'Platform surge DOF', UnEc=UnEc )


   ! PtfmSwDOF - Platform horizontal sway translation DOF.

CALL ReadVar ( UnIn, PtfmFile, InputFileData%PtfmSwDOF, 'PtfmSwDOF', 'Platform sway DOF', UnEc=UnEc )


   ! PtfmHvDOF - Platform vertical heave translation DOF.

CALL ReadVar ( UnIn, PtfmFile, InputFileData%PtfmHvDOF, 'PtfmHvDOF', 'Platform heave DOF', UnEc=UnEc )


   ! PtfmRDOF - Platform roll tilt rotation DOF.

CALL ReadVar ( UnIn, PtfmFile, InputFileData%PtfmRDOF, 'PtfmRDOF', 'Platform roll DOF', UnEc=UnEc )


   ! PtfmPDOF - Platform pitch tilt rotation DOF.

CALL ReadVar ( UnIn, PtfmFile, InputFileData%PtfmPDOF, 'PtfmPDOF', 'Platform pitch DOF', UnEc=UnEc )


   ! PtfmYDOF - Platform yaw rotation DOF.

CALL ReadVar ( UnIn, PtfmFile, InputFileData%PtfmYDOF, 'PtfmYDOF', 'Platform yaw DOF', UnEc=UnEc )


   ! CompHydro - Use HydroDyn?

CALL ReadVar ( UnIn, PtfmFile, CompHydro, 'CompHydro', 'Compute hydrodynamics using HydroDyn?' )



!  -------------- INITIAL CONDITIONS (CONT) ------------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PtfmFile, 'initial conditions (cont)', UnEc=UnEc )


   ! PtfmSurge - Initial or fixed horizontal surge translational displacement of platform.

CALL ReadVar ( UnIn, PtfmFile, InputFileData%PtfmSurge, 'PtfmSurge', 'Initial or fixed platform surge', UnEc=UnEc )


   ! PtfmSway - Initial or fixed horizontal sway translational displacement of platform.

CALL ReadVar ( UnIn, PtfmFile, InputFileData%PtfmSway, 'PtfmSway', 'Initial or fixed platform sway', UnEc=UnEc )


   ! PtfmHeave - Initial or fixed vertical heave translational displacement of platform.

CALL ReadVar ( UnIn, PtfmFile, InputFileData%PtfmHeave, 'PtfmHeave', 'Initial or fixed platform heave', UnEc=UnEc )


   ! PtfmRoll - Initial or fixed roll tilt rotational displacement of platform.

CALL ReadVar ( UnIn, PtfmFile, InputFileData%PtfmRoll, 'PtfmRoll', 'Initial or fixed platform roll', UnEc=UnEc )

IF ( ( InputFileData%PtfmRoll < -15.0 ) .OR. ( InputFileData%PtfmRoll > 15.0 ) )  &
   CALL ProgAbort ( ' PtfmRoll must be between -15 and 15 degrees (inclusive).' )

   InputFileData%PtfmRoll  = InputFileData%PtfmRoll*D2R


   ! PtfmPitch - Initial or fixed pitch tilt rotational displacement of platform.

CALL ReadVar ( UnIn, PtfmFile, InputFileData%PtfmPitch, 'PtfmPitch', 'Initial or fixed platform pitch', UnEc=UnEc )

IF ( ( InputFileData%PtfmPitch < -15.0 ) .OR. ( InputFileData%PtfmPitch > 15.0 ) )  &
   CALL ProgAbort ( ' PtfmPitch must be between -15 and 15 degrees (inclusive).' )

   InputFileData%PtfmPitch = InputFileData%PtfmPitch*D2R

   
   ! PtfmYaw - Initial or fixed yaw rotational displacement of platform.

CALL ReadVar ( UnIn, PtfmFile, InputFileData%PtfmYaw, 'PtfmYaw', 'Initial or fixed platform yaw', UnEc=UnEc )

IF ( ( InputFileData%PtfmYaw < -15.0 ) .OR. ( InputFileData%PtfmYaw > 15.0 ) )  &
   CALL ProgAbort ( ' PtfmYaw must be between -15 and 15 degrees (inclusive).' )

   InputFileData%PtfmYaw   = InputFileData%PtfmYaw*D2R


!  -------------- TURBINE CONFIGURATION (CONT) ---------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PtfmFile, 'turbine configuration (cont)', UnEc=UnEc )


   ! TwrDraft - Downward distance from the ground [onshore] or MSL [offshore] to the tower base platform connection.

CALL ReadVar ( UnIn, PtfmFile, p%TwrDraft, 'TwrDraft', &
   'Downward distance from ground [onshore] or MSL [offshore] to tower base platform connection', UnEc=UnEc )

IF ( p%TwrDraft <= -p%TowerHt )  CALL ProgAbort ( ' TwrDraft must be greater than -TowerHt.' )

IF ( p%TwrRBHt >= ( p%TowerHt + p%TwrDraft ) )  &
         CALL ProgAbort ( ' TwrRBHt must be greater or equal to 0 and less than TowerHt + TwrDraft.' )


   ! PtfmCM - Downward distance from the ground [onshore] or MSL [offshore] to the platform CM.

CALL ReadVar ( UnIn, PtfmFile, InputFileData%PtfmCM, 'PtfmCM', &
   'Downward distance from ground [onshore] or MSL [offshore] to platform CM', UnEc=UnEc )

IF ( InputFileData%PtfmCM < p%TwrDraft )  CALL ProgAbort ( ' PtfmCM must not be less than TwrDraft.' )


   ! PtfmRef - Downward distance from the ground [onshore] or MSL [offshore] to the platform reference point.

CALL ReadVar ( UnIn, PtfmFile, p%PtfmRef, 'PtfmRef', &
   'Downward distance from ground [onshore] or MSL [offshore] to platform reference point', UnEc=UnEc )

IF ( p%PtfmRef < p%TwrDraft )  CALL ProgAbort ( ' PtfmRef must not be less than TwrDraft.' )



!  -------------- MASS AND INERTIA (CONT) --------------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PtfmFile, 'mass and inertia (cont)', UnEc=UnEc )



   ! PtfmMass - Platform mass.

CALL ReadVar ( UnIn, PtfmFile, p%PtfmMass, 'PtfmMass', 'Platform mass', UnEc=UnEc )

IF ( p%PtfmMass < 0.0 )  CALL ProgAbort ( ' PtfmMass must not be negative.' )


   ! PtfmRIner - Platform inertia for roll tilt rotation about the platform CM.

CALL ReadVar ( UnIn, PtfmFile, p%PtfmRIner, 'PtfmRIner', 'Platform inertia for roll tilt rotation about the platform CM', UnEc=UnEc )

IF ( p%PtfmRIner < 0.0 )  CALL ProgAbort ( ' PtfmRIner must not be negative.' )


   ! PtfmPIner - Platform inertia for pitch tilt rotation about the platform CM.

CALL ReadVar ( UnIn, PtfmFile, p%PtfmPIner, 'PtfmPIner', 'Platform inertia for pitch tilt rotation about the platform CM', UnEc=UnEc )

IF ( p%PtfmPIner < 0.0 )  CALL ProgAbort ( ' PtfmPIner must not be negative.' )


   ! PtfmYIner - Platform inertia for yaw rotation about the platform CM.

CALL ReadVar ( UnIn, PtfmFile, p%PtfmYIner, 'PtfmYIner', 'Platform inertia for yaw rotation about the platform CM', UnEc=UnEc )

IF ( p%PtfmYIner < 0.0 )  CALL ProgAbort ( ' PtfmYIner must not be negative.' )


!  -------------- PLATFORM (CONT) ----------------------------------------------


   ! Skip the comment line.

CALL ReadCom ( UnIn, PtfmFile, 'platform (cont)', UnEc=UnEc  )


   ! PtfmLdMod - Platform loading model switch.

   CALL ReadVar ( UnIn, PtfmFile, PtfmLdMod, 'PtfmLdMod', 'Platform loading model switch', UnEc=UnEc )

   IF ( ( PtfmLdMod /= 0 ) .AND. ( PtfmLdMod /= 1 ) )  CALL ProgAbort ( ' PtfmLdMod must be 0 or 1.' )


! ---------------------- HYDRODYN ------------------------------------------------

CALL ReadCom ( UnIn, PtfmFile, 'HydroDyn'  )

   ! HDFile
   
CALL ReadVar ( UnIn, PtfmFile, HDFile, 'HDFile', 'Name of the HydroDyn input file' )

   ! if this is a relative path, let's make it relative to the location of the platform input file
   
IF ( PathIsRelative( HDFile ) ) THEN      
   CALL GetPath( PtfmFile, TmpPath ) 
   HDFile = TRIM(TmpPath)//TRIM(HDFile)
END IF   
   
   
   ! Close the FAST platform file:

CLOSE ( UnIn )



RETURN
END SUBROUTINE GetPtfm
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



   ! Read the primary parameter file (set EchoUn here):

CALL GetPrimary( InputFileData, p, ErrStat, ErrMsg, EchoUn )



   ! Read the platform parameter file if necessary, calculate some parameters
   !   that are not input directly, and convert units where appropriate:

IF (  Platform )  THEN
   
   CALL GetPtfm( p, InputFileData, EchoUn  )

   !InputFileData%PtfmRoll  = InputFileData%PtfmRoll *D2R
   !InputFileData%PtfmPitch = InputFileData%PtfmPitch*D2R
   !InputFileData%PtfmYaw   = InputFileData%PtfmYaw  *D2R
   !
   p%rZYzt     = p%PtfmRef  - InputFileData%PtfmCM

ELSE ! make sure these values from GetPtfm get properly initialized

   InputFileData%PtfmSgDOF   = .FALSE.
   InputFileData%PtfmSwDOF   = .FALSE.
   InputFileData%PtfmHvDOF   = .FALSE.
   InputFileData%PtfmRDOF    = .FALSE.
   InputFileData%PtfmPDOF    = .FALSE.
   InputFileData%PtfmYDOF    = .FALSE.
   CompHydro   = .FALSE.
   HD_TwrNodes = .FALSE.
   
   InputFileData%PtfmSurge = 0.0
   InputFileData%PtfmSway  = 0.0
   InputFileData%PtfmHeave = 0.0
   InputFileData%PtfmRoll  = 0.0
   InputFileData%PtfmPitch = 0.0
   InputFileData%PtfmYaw   = 0.0   

ENDIF

!---------------- START ED_INIT -------------------------------------
!InitInp%ADInputFile = ADFile
!InitInp%InputFile = PriFile
!CALL ED_Init( InitInp, u, p, x, xd, z, OtherState, y, DT, InitOut, ErrStat, ErrMsg )

CALL DispNVD( ED_Ver )

   ! Read the input file and validate the data

CALL ED_ReadInput( PriFile, ADFile, InputFileData, .TRUE., RootName, ErrStat, ErrMsg )
CALL ED_ValidateInput( InputFileData, ErrStat, ErrMsg )

   ! Define parameters here:
CALL ED_SetParameters( InputFileData, p, ErrStat, ErrMsg )         
CALL InitDOFs( p, ErrStat, ErrMsg )

CALL Alloc_CoordSys( OtherState%CoordSys, p, ErrStat, ErrMsg )
!---------------- END ED_INIT -------------------------------------


   ! Calculate some parameters that are not input directly.  Convert units if appropriate.

DT24      = DT/24.0_DbKi                                                        ! Time-step parameter needed for Solver().
p%GBoxEff   = p%GBoxEff*0.01
p%GenEff    = p%GenEff *0.01

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
   BlPitch    (K) = BlPitch (K)*D2R
   BlPitchF   (K) = BlPitchF(K)*D2R
   BlPitchInit(K) = BlPitch (K)
   BegPitMan  (K) = .TRUE.
   TTpBrFl    (K) = TTpBrDp (K) + TpBrDT
ENDDO ! K


!  -------------- BLADE PARAMETERS ---------------------------------------------


   ! Check to see if PSpnElN is an existing analysis point:

IF ( ( p%PSpnElN < 1 ) .OR. ( p%PSpnElN > p%BldNodes ) )  &
   CALL ProgAbort(' PSpnElN must be between 1 and '//TRIM( Num2LStr( p%BldNodes ) )//' (inclusive).' )



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
