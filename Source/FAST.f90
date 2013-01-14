MODULE FASTSubs

   USE   NWTC_Library
   USE   StructDyn_Types
   USE   StructDyn_Parameters
   USE   StructDyn

CONTAINS
!=======================================================================
SUBROUTINE Alloc(p,x,y, OtherState)


   ! This routine allocates many of the variable-length arrays.


USE                             Blades
USE                             DriveTrain
USE                             InitCond
USE                             MassInert
USE                             Modes
USE                             Output
USE                             RtHndSid
USE                             SimCont
USE                             Tower
USE                             TurbCont

USE                             NOISE !AllocNoise

IMPLICIT                        NONE


   ! Passed variables.

TYPE(StrD_ParameterType),        INTENT(INOUT) :: p                             ! Parameters of the structural dynamics module
TYPE(StrD_ContinuousStateType),  INTENT(INOUT) :: x                             ! Continuous states of the structural dynamics module
TYPE(StrD_OutputType),           INTENT(INOUT) :: y                             ! System outputs of the structural dynamics module
TYPE(StrD_OtherStateType),       INTENT(INOUT) :: OtherState                    ! Other State data type for the structural dynamics module


   ! Local variables.

INTEGER(IntKi)               :: Sttus                                           ! Status returned by an attempted allocation.
CHARACTER(1024)              :: ErrMsg



   ! Allocate some arrays:

IF (.NOT. ALLOCATED( y%AllOuts ) ) THEN
   ALLOCATE ( y%AllOuts(0:MaxOutPts) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the AllOuts array.')
   ENDIF
   y%AllOuts = 0.0
ENDIF

IF (.NOT. ALLOCATED( BlPitchFrct ) ) THEN
   ALLOCATE ( BlPitchFrct(p%NumBl) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the BlPitchFrct array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( BlPitchI ) ) THEN
   ALLOCATE ( BlPitchI(p%NumBl) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the BlPitchI array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( BlPitchCom ) ) THEN
   ALLOCATE ( BlPitchCom(p%NumBl) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the BlPitchCom array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( AxRedTFA ) ) THEN
   ALLOCATE ( AxRedTFA(2,2,TTopNode) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the AxRedTFA array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( AxRedTSS ) ) THEN
   ALLOCATE ( AxRedTSS(2,2,TTopNode) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the AxRedTSS array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( TwrFASF ) ) THEN
   ALLOCATE ( TwrFASF(2,TTopNode,0:2) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the TwrFASF array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( TwrSSSF ) ) THEN
   ALLOCATE ( TwrSSSF(2,TTopNode,0:2) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the TwrSSSF array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( AxRedBld ) ) THEN
   ALLOCATE ( AxRedBld(p%NumBl,3,3,p%TipNode) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the AxRedBld array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( TwistedSF ) ) THEN
   ALLOCATE ( TwistedSF(p%NumBl,2,3,p%TipNode,0:2) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the TwistedSF array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( BldCG ) ) THEN
   ALLOCATE ( BldCG(p%NumBl) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the BldCG array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( KBF ) ) THEN
   ALLOCATE ( KBF(p%NumBl,2,2) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the KBF array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( KBE ) ) THEN
   ALLOCATE ( KBE(p%NumBl,1,1) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the KBE array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( CBF ) ) THEN
   ALLOCATE ( CBF(p%NumBl,2,2) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the CBF array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( CBE ) ) THEN
   ALLOCATE ( CBE(p%NumBl,1,1) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the CBE array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( SecondMom ) ) THEN
   ALLOCATE ( SecondMom(p%NumBl) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the SecondMom array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( FirstMom ) ) THEN
   ALLOCATE ( FirstMom(p%NumBl) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the FirstMom array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( FreqBE ) ) THEN
   ALLOCATE ( FreqBE(p%NumBl,1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the FreqBE array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( FreqBF ) ) THEN
   ALLOCATE ( FreqBF(p%NumBl,2,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the FreqBF array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( BldMass ) ) THEN
   ALLOCATE ( BldMass(p%NumBl) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the BldMass array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( rSAerCenn1 ) ) THEN
   ALLOCATE ( rSAerCenn1(p%NumBl,p%BldNodes) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the rSAerCenn1 array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( rSAerCenn2 ) ) THEN
   ALLOCATE ( rSAerCenn2(p%NumBl,p%BldNodes) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the rSAerCenn2 array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( p%DOF_Flag ) ) THEN
   ALLOCATE ( p%DOF_Flag(p%NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the DOF_Flag array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( DOF_FlagInit ) ) THEN
   ALLOCATE ( DOF_FlagInit(p%NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the DOF_FlagInit array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( p%DOF_Desc ) ) THEN
   ALLOCATE ( p%DOF_Desc(p%NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the DOF_Desc array.' )
   ENDIF
ENDIF



IF (.NOT. ALLOCATED( OtherState%QD2 ) ) THEN
   ALLOCATE ( OtherState%QD2(p%NDOF,NMX) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the QD2 array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( IC ) ) THEN
   ALLOCATE ( IC(NMX) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the IC array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( OtherState%Q ) ) THEN
   ALLOCATE ( OtherState%Q(p%NDOF,NMX) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the Q array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( OtherState%QD ) ) THEN
   ALLOCATE ( OtherState%QD(p%NDOF,NMX) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the QD array.' )
   ENDIF
ENDIF


   ! Allocate RtHS arrays:

IF (.NOT. ALLOCATED( AngPosEF ) ) THEN
   ALLOCATE ( AngPosEF(p%TwrNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the AngPosEF array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( AngPosXF ) ) THEN
   ALLOCATE ( AngPosXF(p%TwrNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the AngPosXF array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( AngVelEF ) ) THEN
   ALLOCATE ( AngVelEF(p%TwrNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the AngVelEF array.' )
   ENDIF
ENDIF


IF (.NOT. ALLOCATED( AugMat ) ) THEN
   ALLOCATE ( AugMat(p%NDOF,p%NAug) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the AugMat array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( LinAccES ) ) THEN
   ALLOCATE ( LinAccES(p%NumBl,p%TipNode,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the LinAccES array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( LinAccESt ) ) THEN
   ALLOCATE ( LinAccESt(p%NumBl,p%TipNode,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the LinAccESt array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( LinAccET ) ) THEN
   ALLOCATE ( LinAccET(p%TwrNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the LinAccET array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( LinAccETt ) ) THEN
   ALLOCATE ( LinAccETt(p%TwrNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the LinAccETt array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( LinVelET ) ) THEN
   ALLOCATE ( LinVelET(p%TwrNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the LinVelET array.' )
   ENDIF
ENDIF


ALLOCATE ( AngAccEFt(p%TwrNodes,3) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the AngAccEFt array.' )
ENDIF


IF (.NOT. ALLOCATED( LinVelESm2 ) ) THEN
   ALLOCATE ( LinVelESm2(p%NumBl) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the LinVelESm2 array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( FrcS0B ) ) THEN
   ALLOCATE ( FrcS0B(p%NumBl,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the FrcS0B array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PFrcS0B ) ) THEN
   ALLOCATE ( PFrcS0B(p%NumBl,p%NDOF,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PFrcS0B array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( FrcS0Bt ) ) THEN
   ALLOCATE ( FrcS0Bt(p%NumBl,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the FrcS0Bt array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( MomH0B ) ) THEN
   ALLOCATE ( MomH0B(p%NumBl,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the MomH0B array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PMomH0B ) ) THEN
   ALLOCATE ( PMomH0B(p%NumBl,p%NDOF,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PMomH0B array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( MomH0Bt ) ) THEN
   ALLOCATE ( MomH0Bt(p%NumBl,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the MomH0Bt array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PFrcPRot ) ) THEN
   ALLOCATE ( PFrcPRot(p%NDOF,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PFrcPRot array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PMomLPRot ) ) THEN
   ALLOCATE ( PMomLPRot(p%NDOF,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PMomLPRot array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PFrcVGnRt ) ) THEN
   ALLOCATE ( PFrcVGnRt(p%NDOF,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PFrcVGnRt array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PMomNGnRt ) ) THEN
   ALLOCATE ( PMomNGnRt(p%NDOF,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PMomNGnRt array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PFrcWTail ) ) THEN
   ALLOCATE ( PFrcWTail(p%NDOF,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PFrcWTail array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PMomNTail ) ) THEN
   ALLOCATE ( PMomNTail(p%NDOF,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PMomNTail array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PFrcONcRt ) ) THEN
   ALLOCATE ( PFrcONcRt(p%NDOF,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PFrcONcRt array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PMomBNcRt ) ) THEN
   ALLOCATE ( PMomBNcRt(p%NDOF,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PMomBNcRt array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PFrcT0Trb ) ) THEN
   ALLOCATE ( PFrcT0Trb(p%NDOF,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PFrcT0Trb array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PMomX0Trb ) ) THEN
   ALLOCATE ( PMomX0Trb(p%NDOF,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PMomX0Trb array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PFrcZAll ) ) THEN
   ALLOCATE ( PFrcZAll(p%NDOF,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PFrcZAll array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PMomXAll ) ) THEN
   ALLOCATE ( PMomXAll(p%NDOF,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PMomXAll array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( FSAero ) ) THEN
   ALLOCATE ( FSAero(p%NumBl,p%BldNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the FSAero array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( FSTipDrag ) ) THEN
   ALLOCATE ( FSTipDrag(p%NumBl,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the FSTipDrag array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( MMAero ) ) THEN
   ALLOCATE ( MMAero(p%NumBl,p%BldNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the MMAero array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( rS ) ) THEN
   ALLOCATE ( rS(p%NumBl,p%TipNode,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the rS array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( rQS ) ) THEN
   ALLOCATE ( rQS(p%NumBl,p%TipNode,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the rQS array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( rS0S ) ) THEN
   ALLOCATE ( rS0S(p%NumBl,p%TipNode,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the rS0S array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( AngPosHM ) ) THEN
   ALLOCATE ( AngPosHM(p%NumBl,p%TipNode,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the AngPosHM array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( FTAero ) ) THEN
   ALLOCATE ( FTAero(p%TwrNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the FTAero array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( MFAero ) ) THEN
   ALLOCATE ( MFAero(p%TwrNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the MFAero array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( FTHydro ) ) THEN
   ALLOCATE ( FTHydro(p%TwrNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the FTHydro array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( MFHydro ) ) THEN
   ALLOCATE ( MFHydro(p%TwrNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the MFHydro array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PFTHydro ) ) THEN
   ALLOCATE ( PFTHydro(p%TwrNodes,p%NDOF,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PFTHydro array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PMFHydro ) ) THEN
   ALLOCATE ( PMFHydro(p%TwrNodes,p%NDOF,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PMFHydro array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( FTHydrot ) ) THEN
   ALLOCATE ( FTHydrot(p%TwrNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the FTHydrot array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( MFHydrot ) ) THEN
   ALLOCATE ( MFHydrot(p%TwrNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the MFHydrot array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( rZT ) ) THEN
   ALLOCATE ( rZT(p%TwrNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the rZT array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( rT ) ) THEN
   ALLOCATE ( rT(p%TwrNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the rT array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( rT0T ) ) THEN
   ALLOCATE ( rT0T(p%TwrNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the rT0T array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PAngVelEA ) ) THEN
   ALLOCATE ( PAngVelEA(p%NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PAngVelEA array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PAngVelEB ) ) THEN
   ALLOCATE ( PAngVelEB(p%NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PAngVelEB array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PAngVelEF ) ) THEN
   ALLOCATE ( PAngVelEF(p%TwrNodes,p%NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PAngVelEF array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PAngVelEG ) ) THEN
   ALLOCATE ( PAngVelEG(p%NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PAngVelEG array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PAngVelEH ) ) THEN
   ALLOCATE ( PAngVelEH(p%NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PAngVelEH array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PAngVelEL ) ) THEN
   ALLOCATE ( PAngVelEL(p%NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PAngVelEL array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PAngVelEM ) ) THEN
   ALLOCATE ( PAngVelEM(p%NumBl,p%TipNode,p%NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PAngVelEM array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PAngVelEN ) ) THEN
   ALLOCATE ( PAngVelEN(p%NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PAngVelEN array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PAngVelER ) ) THEN
   ALLOCATE ( PAngVelER(p%NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PAngVelER array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PAngVelEX ) ) THEN
   ALLOCATE ( PAngVelEX(p%NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PAngVelEX array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PLinVelEC ) ) THEN
   ALLOCATE ( PLinVelEC(p%NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PLinVelEC array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PLinVelED ) ) THEN
   ALLOCATE ( PLinVelED(p%NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PLinVelED array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PLinVelEI ) ) THEN
   ALLOCATE ( PLinVelEI(p%NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PLinVelEI array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PLinVelEIMU ) ) THEN
   ALLOCATE ( PLinVelEIMU(p%NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PLinVelEIMU array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PLinVelEJ ) ) THEN
   ALLOCATE ( PLinVelEJ(p%NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PLinVelEJ array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PLinVelEK ) ) THEN
   ALLOCATE ( PLinVelEK(p%NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PLinVelEK array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PLinVelEO ) ) THEN
   ALLOCATE ( PLinVelEO(p%NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PLinVelEO array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PLinVelEP ) ) THEN
   ALLOCATE ( PLinVelEP(p%NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PLinVelEP array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PLinVelEQ ) ) THEN
   ALLOCATE ( PLinVelEQ(p%NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PLinVelEQ array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PLinVelES ) ) THEN
   ALLOCATE ( PLinVelES(p%NumBl,p%TipNode,p%NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PLinVelES array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PLinVelET ) ) THEN
   ALLOCATE ( PLinVelET(p%TwrNodes,p%NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PLinVelET array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PLinVelEU ) ) THEN
   ALLOCATE ( PLinVelEU(p%NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PLinVelEU array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PLinVelEV ) ) THEN
   ALLOCATE ( PLinVelEV(p%NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PLinVelEV array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PLinVelEW ) ) THEN
   ALLOCATE ( PLinVelEW(p%NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PLinVelEW array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PLinVelEY ) ) THEN
   ALLOCATE ( PLinVelEY(p%NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PLinVelEY array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PLinVelEZ ) ) THEN
   ALLOCATE ( PLinVelEZ(p%NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PLinVelEZ array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( SolnVec ) ) THEN
   ALLOCATE ( SolnVec(p%NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the SolnVec array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( QD2T ) ) THEN
   ALLOCATE ( QD2T(p%NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the QD2T array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( x%QDT ) ) THEN
   ALLOCATE ( x%QDT(p%NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the QDT array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( x%QT ) ) THEN
   ALLOCATE ( x%QT(p%NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the QT array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( QD2TC ) ) THEN
   ALLOCATE ( QD2TC(p%NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the QD2TC array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( OgnlGeAzRo ) ) THEN
   ALLOCATE ( OgnlGeAzRo(p%NAUG) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the OgnlGeAzRo array.' )
   ENDIF
ENDIF




   ! Allocate noise arrays:

CALL AllocNoise( p )



RETURN
END SUBROUTINE Alloc
!=======================================================================


SUBROUTINE CalcOuts( p,x,y,OtherState )


   ! This SUBROUTINE is used to compute the selected output channels
   !   (motions and loads) and place them in the OutData() array.
   ! NOTE: the descriptions of the output channels are not given here.
   !   Please see the FAST User's Guide for a complete description of
   !   each output parameter.
   ! NOTE: no matter how many output channels are selected, all of the
   !   outputs are calcalated (it could be more time consuming to
   !   check to see if an output need be calculated than to actually
   !   calculate it).  This is also important since some users may want to
   !   access any of of the output channels in their user-defined routines
   !   without actually outputting those values to the output file.  All
   !   of the calculated output channels are placed into the y%AllOuts(:)
   !   array.


USE                             Blades
USE                             DriveTrain
USE                             EnvCond
USE                             Features
USE                             FloatingPlatform, ONLY:AnchorTension, FairleadTension
USE                             Linear
USE                             MassInert
USE                             Output
USE                             Platform
USE                             RtHndSid
USE                             SimCont
USE                             TailAero
USE                             Tower
USE                             TurbCont
USE                             Waves, ONLY:WaveElevation, WaveVelocity, WaveAcceleration

USE                             AeroDyn


IMPLICIT                        NONE

   ! passed variables:
!TYPE(StrD_InputType),           INTENT(IN   )  :: u           ! Inputs at Time
TYPE(StrD_ParameterType),       INTENT(IN   )  :: p           ! Parameters
TYPE(StrD_ContinuousStateType), INTENT(IN   )  :: x           ! Continuous states at Time
!TYPE(StrD_DiscreteStateType),   INTENT(IN   )  :: xd          ! Discrete states at Time
!TYPE(StrD_ConstraintStateType), INTENT(IN   )  :: z           ! Constraint states at Time
TYPE(StrD_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states (The coordinate systems previously calculated)
TYPE(StrD_OutputType),          INTENT(INOUT)  :: y           ! Outputs computed at Time (Input only so that mesh con-
                                                              !   nectivity information does not have to be recalculated)
!INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat     ! Error status of the operation
!CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None


   ! Local variables:

REAL(ReKi)                   :: AnchTe                                          ! Instantaneous effective tension in a mooring line at the anchor   (N  )
REAL(ReKi)                   :: AnchTeAng                                       ! Instantaneous vertical angle    of a mooring line at the anchor   (rad)
REAL(ReKi)                   :: AngAccEB  (3)                                   ! Angular acceleration of the base plate                                                (body B) in the inertia frame (body E for earth).
REAL(ReKi)                   :: AngAccER  (3)                                   ! Angular acceleration of the structure that furls with the rotor (not including rotor) (body R) in the inertia frame (body E for earth).
REAL(ReKi)                   :: AngAccEX  (3)                                   ! Angular acceleration of the platform                                                  (body X) in the inertia frame (body E for earth).
REAL(ReKi)                   :: ComDenom                                        ! Common denominator used in several expressions.
REAL(ReKi)                   :: CThrstys                                        ! Estimate of the ys-location of the center of thrust.
REAL(ReKi)                   :: CThrstzs                                        ! Estimate of the zs-location of the center of thrust.
REAL(ReKi)                   :: FairTe                                          ! Instantaneous effective tension in a mooring line at the fairlead (N  )
REAL(ReKi)                   :: FairTeAng                                       ! Instantaneous vertical angle    of a mooring line at the fairlead (rad)
REAL(ReKi)                   :: FrcMGagB  (3)                                   ! Total force at the blade element   (body M) / blade strain gage location            (point S) due to the blade above the strain gage.
REAL(ReKi)                   :: FrcFGagT  (3)                                   ! Total force at the tower element   (body F) / tower strain gage location            (point T) due to the nacelle and rotor and tower above the strain gage.
REAL(ReKi)                   :: FrcONcRt  (3)                                   ! Total force at the yaw bearing (point O  ) due to the nacelle, generator, and rotor.
REAL(ReKi)                   :: FrcPRot   (3)                                   ! Total force at the teeter pin  (point P  ) due to the rotor.
REAL(ReKi)                   :: FrcT0Trb  (3)                                   ! Total force at the base of flexible portion of the tower (point T(0)) due to the entire wind turbine.
REAL(ReKi)                   :: FZHydro   (3)                                   ! Total platform hydrodynamic force at the platform reference (point Z).
REAL(ReKi)                   :: HHWndVec  (3)                                   ! Hub-height wind vector in the AeroDyn coordinate system.
REAL(ReKi)                   :: LinAccEIMU(3)                                   ! Total linear acceleration of the nacelle IMU (point IMU) in the inertia frame (body E for earth).
REAL(ReKi)                   :: LinAccEO  (3)                                   ! Total linear acceleration of the base plate (point O) in the inertia frame (body E for earth).
REAL(ReKi)                   :: LinAccEZ  (3)                                   ! Total linear acceleration of the platform refernce (point Z) in the inertia frame (body E for earth).
REAL(ReKi)                   :: MomBNcRt  (3)                                   ! Total moment at the base plate      (body B) / yaw bearing                           (point O) due to the nacelle, generator, and rotor.
REAL(ReKi)                   :: MomFGagT  (3)                                   ! Total moment at the tower element   (body F) / tower strain gage location            (point T) due to the nacelle and rotor and tower above the strain gage.
REAL(ReKi)                   :: MomLPRot  (3)                                   ! Total moment at the low-speed shaft (body L) / teeter pin                            (point P) due to the rotor.
REAL(ReKi)                   :: MomMGagB  (3)                                   ! Total moment at the blade element   (body M) / blade strain gage location            (point S) due to the blade above the strain gage.
REAL(ReKi)                   :: MomNGnRt  (3)                                   ! Total moment at the nacelle         (body N) / specified point on rotor-furl axis    (point V) due to the structure that furls with the rotor, generator, and rotor.
REAL(ReKi)                   :: MomNTail  (3)                                   ! Total moment at the nacelle         (body N) / specified point on  tail-furl axis    (point W) due to the tail.
REAL(ReKi)                   :: MomX0Trb  (3)                                   ! Total moment at the tower base      (body X) / base of flexible portion of the tower (point T(0)) due to the entire wind turbine.
REAL(ReKi)                   :: MXHydro   (3)                                   ! Total platform hydrodynamic moment acting at the platform (body X) / platform reference (point Z).
REAL(ReKi)                   :: rOPO      (3)                                   ! Position vector from the undeflected tower top (point O prime) to the deflected tower top (point O).
REAL(ReKi)                   :: rOSTip    (3)                                   ! Position vector from the deflected tower top (point O) to the deflected blade tip (point S tip).
REAL(ReKi)                   :: rOSTipxn                                        ! Component of rOSTip directed along the xn-axis.
REAL(ReKi)                   :: rOSTipyn                                        ! Component of rOSTip directed along the yn-axis.
REAL(ReKi)                   :: rOSTipzn                                        ! Component of rOSTip directed along the zn-axis.
REAL(ReKi)                   :: rTPT      (3)                                   ! Position vector from the undeflected tower node (point T prime) to the deflected node (point T)
REAL(ReKi)                   :: rSPS      (3)                                   ! Position vector from the undeflected blade node (point S prime) to the deflected node (point S)
REAL(ReKi)                   :: rSTipPSTip(3)                                   ! Position vector from the undeflected blade tip (point S tip prime) to the deflected blade tip (point S tip).
REAL(ReKi)                   :: TmpVec    (3)                                   ! A temporary vector used in various computations.
REAL(ReKi)                   :: TmpVec2   (3)                                   ! A temporary vector.


INTEGER                      :: I                                               ! Generic index
INTEGER                      :: J                                               ! Loops through nodes / elements.
INTEGER                      :: K                                               ! Loops through blades.

INTEGER                      :: ErrStat




!Array y%AllOuts() is initialized to 0.0 in subroutine ChckOutLst(), so we are not going to reinitialize it here.


   ! Calculate all of the total forces and moments using all of the
   !   partial forces and moments calculated in RtHS().  Also,
   !   calculate all of the total angular and linear accelerations
   !   using all of the partial accelerations calculated in RtHS().
   !   To do this, first initialize the variables using the portions
   !   not associated with the accelerations.  Then add the portions
   !   associated with the accelerations one by one:

AngAccEB   = AngAccEBt
AngAccER   = AngAccERt
AngAccEX   = AngAccEXt
LinAccEIMU = LinAccEIMUt
LinAccEO   = LinAccEOt
LinAccEZ   = LinAccEZt
FrcONcRt   = FrcONcRtt
FrcPRot    = FrcPRott
FrcT0Trb   = FrcT0Trbt
FZHydro    = FZHydrot
MomBNcRt   = MomBNcRtt
MomLPRot   = MomLPRott
MomNGnRt   = MomNGnRtt
MomNTail   = MomNTailt
MomX0Trb   = MomX0Trbt
MXHydro    = MXHydrot

DO I = 1,OtherState%DOFs%NActvDOF ! Loop through all active (enabled) DOFs
   AngAccEB   = AngAccEB   + PAngVelEB  (OtherState%DOFs%SrtPS(I),0,:)*QD2T(OtherState%DOFs%SrtPS(I))
   AngAccER   = AngAccER   + PAngVelER  (OtherState%DOFs%SrtPS(I),0,:)*QD2T(OtherState%DOFs%SrtPS(I))
   LinAccEIMU = LinAccEIMU + PLinVelEIMU(OtherState%DOFs%SrtPS(I),0,:)*QD2T(OtherState%DOFs%SrtPS(I))
   LinAccEO   = LinAccEO   + PLinVelEO  (OtherState%DOFs%SrtPS(I),0,:)*QD2T(OtherState%DOFs%SrtPS(I))
   FrcONcRt   = FrcONcRt   + PFrcONcRt  (OtherState%DOFs%SrtPS(I),  :)*QD2T(OtherState%DOFs%SrtPS(I))
   FrcPRot    = FrcPRot    + PFrcPRot   (OtherState%DOFs%SrtPS(I),  :)*QD2T(OtherState%DOFs%SrtPS(I))
   FrcT0Trb   = FrcT0Trb   + PFrcT0Trb  (OtherState%DOFs%SrtPS(I),  :)*QD2T(OtherState%DOFs%SrtPS(I))
   MomBNcRt   = MomBNcRt   + PMomBNcRt  (OtherState%DOFs%SrtPS(I),  :)*QD2T(OtherState%DOFs%SrtPS(I))
   MomLPRot   = MomLPRot   + PMomLPRot  (OtherState%DOFs%SrtPS(I),  :)*QD2T(OtherState%DOFs%SrtPS(I))
   MomNGnRt   = MomNGnRt   + PMomNGnRt  (OtherState%DOFs%SrtPS(I),  :)*QD2T(OtherState%DOFs%SrtPS(I))
   MomNTail   = MomNTail   + PMomNTail  (OtherState%DOFs%SrtPS(I),  :)*QD2T(OtherState%DOFs%SrtPS(I))
   MomX0Trb   = MomX0Trb   + PMomX0Trb  (OtherState%DOFs%SrtPS(I),  :)*QD2T(OtherState%DOFs%SrtPS(I))
ENDDO             ! I - All active (enabled) DOFs
DO I = 1,OtherState%DOFs%NPYE     ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the platform center of mass (point Y)
   AngAccEX   = AngAccEX   + PAngVelEX  (OtherState%DOFs%PYE  (I),0,:)*QD2T(OtherState%DOFs%PYE  (I))
   LinAccEZ   = LinAccEZ   + PLinVelEZ  (OtherState%DOFs%PYE  (I),0,:)*QD2T(OtherState%DOFs%PYE  (I))
   FZHydro    = FZHydro    + PFZHydro   (OtherState%DOFs%PYE  (I),  :)*QD2T(OtherState%DOFs%PYE  (I))
   MXHydro    = MXHydro    + PMXHydro   (OtherState%DOFs%PYE  (I),  :)*QD2T(OtherState%DOFs%PYE  (I))
ENDDO             ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the platform center of mass (point Y)



DO K = 1,p%NumBl ! Loop through all blades

   LinAccES(K,p%TipNode,:) = LinAccESt(K,p%TipNode,:)
   FrcS0B  (K,          :) = FrcS0Bt  (K,          :)
   MomH0B  (K,          :) = MomH0Bt  (K,          :)

   DO I = 1,OtherState%DOFs%NPSE(K)  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of blade K
      LinAccES(K,p%TipNode,:) = LinAccES(K,p%TipNode,:) + PLinVelES(K,p%TipNode,OtherState%DOFs%PSE(K,I),0,:)*QD2T(OtherState%DOFs%PSE(K,I))
      FrcS0B  (K,          :) = FrcS0B  (K,          :) + PFrcS0B  (K,          OtherState%DOFs%PSE(K,I),  :)*QD2T(OtherState%DOFs%PSE(K,I))
      MomH0B  (K,          :) = MomH0B  (K,          :) + PMomH0B  (K,          OtherState%DOFs%PSE(K,I),  :)*QD2T(OtherState%DOFs%PSE(K,I))
   ENDDO             ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of blade K

   DO J = 1,p%BldNodes ! Loop through the blade nodes / elements

      LinAccES(K,J,:) = LinAccESt(K,J,:)

      DO I = 1,OtherState%DOFs%NPSE(K)  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of blade K
         LinAccES(K,J,:) = LinAccES(K,J,:) + PLinVelES(K,J,OtherState%DOFs%PSE(K,I),0,:)*QD2T(OtherState%DOFs%PSE(K,I))
      ENDDO             ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of blade K

   ENDDO             ! J - Blade nodes / elements

ENDDO          ! K - All blades

DO J = 1,p%TwrNodes  ! Loop through the tower nodes / elements

   LinAccET(J,:) = LinAccETt(J,:)
   FTHydro (J,:) = FTHydrot (J,:)
   MFHydro (J,:) = MFHydrot (J,:)

   DO I = 1,OtherState%DOFs%NPTE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the yaw bearing center of mass (point O)
      LinAccET(J,:) = LinAccET(J,:) + PLinVelET(J,OtherState%DOFs%PTE(I),0,:)*QD2T(OtherState%DOFs%PTE(I))
      FTHydro (J,:) = FTHydro (J,:) + PFTHydro (J,OtherState%DOFs%PTE(I),  :)*QD2T(OtherState%DOFs%PTE(I))
      MFHydro (J,:) = MFHydro (J,:) + PMFHydro (J,OtherState%DOFs%PTE(I),  :)*QD2T(OtherState%DOFs%PTE(I))
   ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the yaw bearing center of mass (point O)

ENDDO ! J - Tower nodes / elements


   ! Convert the units of the forces and moments from N and N-m
   !    to kN and kN-m:

FrcONcRt = 0.001*FrcONcRt
FrcPRot  = 0.001*FrcPRot
FrcT0Trb = 0.001*FrcT0Trb
FZHydro  = 0.001*FZHydro
MomBNcRt = 0.001*MomBNcRt
MomLPRot = 0.001*MomLPRot
MomNGnRt = 0.001*MomNGnRt
MomNTail = 0.001*MomNTail
MomX0Trb = 0.001*MomX0Trb
MXHydro  = 0.001*MXHydro
FrcS0B   = 0.001*FrcS0B
MomH0B   = 0.001*MomH0B



   ! Define the output channel specifying the current simulation time:

y%AllOuts(  Time) = ZTime


IF ( CompAero )  THEN   ! AeroDyn has been used


   ! Wind Motions:

   HHWndVec(:) = AD_GetUndisturbedWind( ZTime, (/REAL(0.0, ReKi), REAL(0.0, ReKi), p%FASTHH /), ErrStat )

   y%AllOuts(  WindVxi) = HHWndVec(1)
   y%AllOuts(  WindVyi) = HHWndVec(2)
   y%AllOuts(  WindVzi) = HHWndVec(3)
   y%AllOuts( TotWindV) = SQRT(   ( y%AllOuts(  WindVxi)*y%AllOuts(  WindVxi) ) &
                                + ( y%AllOuts(  WindVyi)*y%AllOuts(  WindVyi) ) &
                                + ( y%AllOuts(  WindVzi)*y%AllOuts(  WindVzi) )   )
   y%AllOuts( HorWindV) = SQRT(   ( y%AllOuts(  WindVxi)*y%AllOuts(  WindVxi) ) &
                                + ( y%AllOuts(  WindVyi)*y%AllOuts(  WindVyi) )   )
   y%AllOuts(HorWndDir) = ATAN2( y%AllOuts(  WindVyi), y%AllOuts(  WindVxi) )*R2D
   y%AllOuts(VerWndDir) = ATAN2( y%AllOuts(  WindVzi), y%AllOuts( HorWindV) )*R2D


   ! Tail Fin Element Aerodynamics:

   y%AllOuts(TFinAlpha) = TFinAOA*R2D
   y%AllOuts(TFinCLift) = TFinCL
   y%AllOuts(TFinCDrag) = TFinCD
   y%AllOuts(TFinDnPrs) = TFinQ
   y%AllOuts(TFinCPFx ) = TFinKFx*0.001
   y%AllOuts(TFinCPFy ) = TFinKFy*0.001


ENDIF

IF ( CompHydro )  THEN  ! Hydrodynamics have been used


   ! Wave Motions:

   y%AllOuts(WaveElev ) = WaveElevation ( 1, ZTime )

   DO I = 1, NWaveKin

         y%AllOuts( WaveVxi(I) ) = WaveVelocity     ( WaveKinNd(I), 1, ZTime )
         y%AllOuts( WaveVyi(I) ) = WaveVelocity     ( WaveKinNd(I), 2, ZTime )
         y%AllOuts( WaveVzi(I) ) = WaveVelocity     ( WaveKinNd(I), 3, ZTime )
         y%AllOuts( WaveAxi(I) ) = WaveAcceleration ( WaveKinNd(I), 1, ZTime )
         y%AllOuts( WaveAyi(I) ) = WaveAcceleration ( WaveKinNd(I), 2, ZTime )
         y%AllOuts( WaveAzi(I) ) = WaveAcceleration ( WaveKinNd(I), 3, ZTime )

   END DO

END IF


   ! Blade (1-3) Tip Motions:

DO K = 1,p%NumBl
   rSTipPSTip = rS0S(K,p%TipNode,:) - BldFlexL*OtherState%CoordSys%j3(K,:)  ! Position vector from the undeflected blade tip (point S tip prime) to the deflected blade tip (point S tip) of blade 1.
   rOSTip     = rS  (K,p%TipNode,:) - rO                ! Position vector from the deflected tower top (point O) to the deflected blade tip (point S tip) of blade 1.
   rOSTipxn   =      DOT_PRODUCT( rOSTip, OtherState%CoordSys%d1 )                ! Component of rOSTip directed along the xn-axis.
   rOSTipyn   = -1.0*DOT_PRODUCT( rOSTip, OtherState%CoordSys%d3 )                ! Component of rOSTip directed along the yn-axis.
   rOSTipzn   =      DOT_PRODUCT( rOSTip, OtherState%CoordSys%d2 )                ! Component of rOSTip directed along the zn-axis.

   y%AllOuts(  TipDxc(K) ) = DOT_PRODUCT(            rSTipPSTip, OtherState%CoordSys%i1(K,         :) )
   y%AllOuts(  TipDyc(K) ) = DOT_PRODUCT(            rSTipPSTip, OtherState%CoordSys%i2(K,         :) )
   y%AllOuts(  TipDzc(K) ) = DOT_PRODUCT(            rSTipPSTip, OtherState%CoordSys%i3(K,         :) )
   y%AllOuts(  TipDxb(K) ) = DOT_PRODUCT(            rSTipPSTip, OtherState%CoordSys%j1(K,         :) )
   y%AllOuts(  TipDyb(K) ) = DOT_PRODUCT(            rSTipPSTip, OtherState%CoordSys%j2(K,         :) )
   !JASON: USE TipNode HERE INSTEAD OF BldNodes IF YOU ALLOCATE AND DEFINE n1, n2, n3, m1, m2, AND m3 TO USE TipNode.  THIS WILL REQUIRE THAT THE AERODYNAMIC AND STRUCTURAL TWISTS, AeroTwst() AND ThetaS(), BE KNOWN AT THE TIP!!!
   y%AllOuts( TipALxb(K) ) = DOT_PRODUCT( LinAccES(K,p%TipNode,:), OtherState%CoordSys%n1(K,p%BldNodes,:) )
   y%AllOuts( TipALyb(K) ) = DOT_PRODUCT( LinAccES(K,p%TipNode,:), OtherState%CoordSys%n2(K,p%BldNodes,:) )
   y%AllOuts( TipALzb(K) ) = DOT_PRODUCT( LinAccES(K,p%TipNode,:), OtherState%CoordSys%n3(K,p%BldNodes,:) )
   y%AllOuts( TipRDxb(K) ) = DOT_PRODUCT( AngPosHM(K,p%TipNode,:), OtherState%CoordSys%j1(K,         :) )*R2D
   y%AllOuts( TipRDyb(K) ) = DOT_PRODUCT( AngPosHM(K,p%TipNode,:), OtherState%CoordSys%j2(K,         :) )*R2D
   ! There is no sense computing AllOuts( TipRDzc(K) ) here since it is always zero for FAST simulation results.
   IF ( rOSTipzn > 0.0 )  THEN   ! Tip of blade K is above the yaw bearing.
      y%AllOuts(TipClrnc(K) ) = SQRT( rOSTipxn*rOSTipxn + rOSTipyn*rOSTipyn + rOSTipzn*rOSTipzn ) ! Absolute distance from the tower top / yaw bearing to the tip of blade 1.
   ELSE                          ! Tip of blade K is below the yaw bearing.
      y%AllOuts(TipClrnc(K) ) = SQRT( rOSTipxn*rOSTipxn + rOSTipyn*rOSTipyn                     ) ! Perpendicular distance from the yaw axis / tower centerline to the tip of blade 1.
   ENDIF

END DO !K

   ! Blade (1-3) Local Span Motions:

DO K = 1,p%NumBl
   DO I = 1, p%NBlGages

      y%AllOuts( SpnALxb(I,K) ) = DOT_PRODUCT( LinAccES(K,BldGagNd(I),:), OtherState%CoordSys%n1(K,BldGagNd(I),:) )
      y%AllOuts( SpnALyb(I,K) ) = DOT_PRODUCT( LinAccES(K,BldGagNd(I),:), OtherState%CoordSys%n2(K,BldGagNd(I),:) )
      y%AllOuts( SpnALzb(I,K) ) = DOT_PRODUCT( LinAccES(K,BldGagNd(I),:), OtherState%CoordSys%n3(K,BldGagNd(I),:) )

      rSPS                    = rS0S(K,BldGagNd(I),:) - RNodes(BldGagNd(I))*OtherState%CoordSys%j3(K,:)

      y%AllOuts( SpnTDxb(I,K) ) = DOT_PRODUCT( rSPS, OtherState%CoordSys%j1(K,:) )
      y%AllOuts( SpnTDyb(I,K) ) = DOT_PRODUCT( rSPS, OtherState%CoordSys%j2(K,:) )
      y%AllOuts( SpnTDzb(I,K) ) = DOT_PRODUCT( rSPS, OtherState%CoordSys%j3(K,:) )

      y%AllOuts( SpnRDxb(I,K) ) = DOT_PRODUCT( AngPosHM(K,BldGagNd(I),:), OtherState%CoordSys%j1(K,:) )*R2D
      y%AllOuts( SpnRDyb(I,K) ) = DOT_PRODUCT( AngPosHM(K,BldGagNd(I),:), OtherState%CoordSys%j2(K,:) )*R2D
     !y%AllOuts( SpnRDzb(I,K) ) = DOT_PRODUCT( AngPosHM(K,BldGagNd(I),:), OtherState%CoordSys%j3(K,:) )*R2D           ! this is always zero for FAST

   END DO !I
END DO !K



   ! Blade Pitch Motions:

y%AllOuts(PtchPMzc1) = BlPitch(1)*R2D
y%AllOuts(PtchPMzc2) = BlPitch(2)*R2D
IF ( p%NumBl == 3 )  THEN ! 3-blader

   y%AllOuts(PtchPMzc3) = BlPitch(3)*R2D

ELSE  ! 2-blader


   ! Teeter Motions:

   y%AllOuts(  TeetPya) =x%QT  (DOF_Teet)*R2D
   y%AllOuts(  TeetVya) =x%QDT (DOF_Teet)*R2D
   y%AllOuts(  TeetAya) =  QD2T(DOF_Teet)*R2D

ENDIF


   ! Shaft Motions:

IF ( IgnoreMOD )  THEN  ! Don't use MOD when computing y%AllOuts(LSSTipPxa) and y%AllOuts(LSSGagPxa) -- IgnoreMOD is needed when computing CMat for output measurements during FAST linearization.
   y%AllOuts(LSSTipPxa) =      ( x%QT (DOF_GeAz) + x%QT  (DOF_DrTr) )*R2D + p%AzimB1Up + 90.0
   y%AllOuts(LSSGagPxa) =        x%QT (DOF_GeAz)                     *R2D + p%AzimB1Up + 90.0
ELSE                    ! Do    use MOD when computing y%AllOuts(LSSTipPxa) and y%AllOuts(LSSGagPxa)
   y%AllOuts(LSSTipPxa) = MOD( ( x%QT (DOF_GeAz) + x%QT  (DOF_DrTr) )*R2D + p%AzimB1Up + 90.0, 360.0 )
   y%AllOuts(LSSGagPxa) = MOD(   x%QT (DOF_GeAz)                     *R2D + p%AzimB1Up + 90.0, 360.0 )
ENDIF
y%AllOuts(   LSSTipVxa) =    ( x%QDT (DOF_GeAz) +  x%QDT (DOF_DrTr) )*RPS2RPM
y%AllOuts(   LSSTipAxa) =    (   QD2T(DOF_GeAz) +    QD2T(DOF_DrTr) )*R2D
y%AllOuts(   LSSGagVxa) =      x%QDT (DOF_GeAz)                      *RPS2RPM
y%AllOuts(   LSSGagAxa) =        QD2T(DOF_GeAz)                      *R2D
y%AllOuts(     HSShftV) = GBRatio*y%AllOuts(LSSGagVxa)
y%AllOuts(     HSShftA) = GBRatio*y%AllOuts(LSSGagAxa)
IF ( y%AllOuts(  WindVxi) /= 0.0 )  THEN  ! .TRUE. if the denominator in the following equation is not zero.

   y%AllOuts(TipSpdRat) =      ( x%QDT (DOF_GeAz) + x%QDT (DOF_DrTr) )*p%AvgNrmTpRd / y%AllOuts(  WindVxi)

ELSE

   y%AllOuts(TipSpdRat) = 0.0

ENDIF


   ! Nacelle IMU Motions:

y%AllOuts(NcIMUTVxs) =      DOT_PRODUCT( LinVelEIMU, OtherState%CoordSys%c1 )
y%AllOuts(NcIMUTVys) = -1.0*DOT_PRODUCT( LinVelEIMU, OtherState%CoordSys%c3 )
y%AllOuts(NcIMUTVzs) =      DOT_PRODUCT( LinVelEIMU, OtherState%CoordSys%c2 )
y%AllOuts(NcIMUTAxs) =      DOT_PRODUCT( LinAccEIMU, OtherState%CoordSys%c1 )
y%AllOuts(NcIMUTAys) = -1.0*DOT_PRODUCT( LinAccEIMU, OtherState%CoordSys%c3 )
y%AllOuts(NcIMUTAzs) =      DOT_PRODUCT( LinAccEIMU, OtherState%CoordSys%c2 )
y%AllOuts(NcIMURVxs) =      DOT_PRODUCT( AngVelER  , OtherState%CoordSys%c1 )*R2D
y%AllOuts(NcIMURVys) = -1.0*DOT_PRODUCT( AngVelER  , OtherState%CoordSys%c3 )*R2D
y%AllOuts(NcIMURVzs) =      DOT_PRODUCT( AngVelER  , OtherState%CoordSys%c2 )*R2D
y%AllOuts(NcIMURAxs) =      DOT_PRODUCT( AngAccER  , OtherState%CoordSys%c1 )*R2D
y%AllOuts(NcIMURAys) = -1.0*DOT_PRODUCT( AngAccER  , OtherState%CoordSys%c3 )*R2D
y%AllOuts(NcIMURAzs) =      DOT_PRODUCT( AngAccER  , OtherState%CoordSys%c2 )*R2D


   ! Rotor-Furl Motions:

y%AllOuts( RotFurlP) = x%QT  (DOF_RFrl)*R2D
y%AllOuts( RotFurlV) = x%QDT (DOF_RFrl)*R2D
y%AllOuts( RotFurlA) =   QD2T(DOF_RFrl)*R2D


   ! Tail-Furl Motions:

y%AllOuts(TailFurlP) = x%QT  (DOF_TFrl)*R2D
y%AllOuts(TailFurlV) = x%QDT (DOF_TFrl)*R2D
y%AllOuts(TailFurlA) =   QD2T(DOF_TFrl)*R2D


   ! Yaw Motions:

y%AllOuts(   YawPzn) = x%QT  (DOF_Yaw )*R2D
y%AllOuts(   YawVzn) = x%QDT (DOF_Yaw )*R2D
y%AllOuts(   YawAzn) =   QD2T(DOF_Yaw )*R2D


   ! Tower-Top / Yaw Bearing Motions:

rOPO     = rT0O - TwrFlexL*OtherState%CoordSys%a2 ! Position vector from the undeflected tower top (point O prime) to the deflected tower top (point O).

y%AllOuts(YawBrTDxp) =  DOT_PRODUCT(     rOPO, OtherState%CoordSys%b1 )
y%AllOuts(YawBrTDyp) = -DOT_PRODUCT(     rOPO, OtherState%CoordSys%b3 )
y%AllOuts(YawBrTDzp) =  DOT_PRODUCT(     rOPO, OtherState%CoordSys%b2 )
y%AllOuts(YawBrTDxt) =  DOT_PRODUCT(     rOPO, OtherState%CoordSys%a1 )
y%AllOuts(YawBrTDyt) = -DOT_PRODUCT(     rOPO, OtherState%CoordSys%a3 )
y%AllOuts(YawBrTDzt) =  DOT_PRODUCT(     rOPO, OtherState%CoordSys%a2 )
y%AllOuts(YawBrTAxp) =  DOT_PRODUCT( LinAccEO, OtherState%CoordSys%b1 )
y%AllOuts(YawBrTAyp) = -DOT_PRODUCT( LinAccEO, OtherState%CoordSys%b3 )
y%AllOuts(YawBrTAzp) =  DOT_PRODUCT( LinAccEO, OtherState%CoordSys%b2 )
y%AllOuts(YawBrRDxt) =  DOT_PRODUCT( AngPosXB, OtherState%CoordSys%a1 )*R2D
y%AllOuts(YawBrRDyt) = -DOT_PRODUCT( AngPosXB, OtherState%CoordSys%a3 )*R2D
! There is no sense computing y%AllOuts(YawBrRDzt) here since it is always zero for FAST simulation results.
y%AllOuts(YawBrRVxp) =  DOT_PRODUCT( AngVelEB, OtherState%CoordSys%b1 )*R2D
y%AllOuts(YawBrRVyp) = -DOT_PRODUCT( AngVelEB, OtherState%CoordSys%b3 )*R2D
y%AllOuts(YawBrRVzp) =  DOT_PRODUCT( AngVelEB, OtherState%CoordSys%b2 )*R2D
y%AllOuts(YawBrRAxp) =  DOT_PRODUCT( AngAccEB, OtherState%CoordSys%b1 )*R2D
y%AllOuts(YawBrRAyp) = -DOT_PRODUCT( AngAccEB, OtherState%CoordSys%b3 )*R2D
y%AllOuts(YawBrRAzp) =  DOT_PRODUCT( AngAccEB, OtherState%CoordSys%b2 )*R2D


   ! Local Tower Motions:

DO I = 1, p%NTwGages

   y%AllOuts( TwHtALxt(I) ) =      DOT_PRODUCT( LinAccET(TwrGagNd(I),:), OtherState%CoordSys%t1(TwrGagNd(I),:) )
   y%AllOuts( TwHtALyt(I) ) = -1.0*DOT_PRODUCT( LinAccET(TwrGagNd(I),:), OtherState%CoordSys%t3(TwrGagNd(I),:) )
   y%AllOuts( TwHtALzt(I) ) =      DOT_PRODUCT( LinAccET(TwrGagNd(I),:), OtherState%CoordSys%t2(TwrGagNd(I),:) )

   rTPT                   = rT0T(TwrGagNd(I),:) - HNodes(TwrGagNd(I))*OtherState%CoordSys%a2(:)

   y%AllOuts( TwHtTDxt(I) ) =      DOT_PRODUCT( rTPT,     OtherState%CoordSys%a1 )
   y%AllOuts( TwHtTDyt(I) ) = -1.0*DOT_PRODUCT( rTPT,     OtherState%CoordSys%a3 )
   y%AllOuts( TwHtTDzt(I) ) =      DOT_PRODUCT( rTPT,     OtherState%CoordSys%a2 )

   y%AllOuts( TwHtRDxt(I) ) =      DOT_PRODUCT( AngPosXF(TwrGagNd(I),:), OtherState%CoordSys%a1 )*R2D  !why is this zero???
   y%AllOuts( TwHtRDyt(I) ) = -1.0*DOT_PRODUCT( AngPosXF(TwrGagNd(I),:), OtherState%CoordSys%a3 )*R2D
!   y%AllOuts( TwHtRDzt(I) ) =     DOT_PRODUCT( AngPosXF(TwrGagNd(I),:), OtherState%CoordSys%a2 )*R2D  !this will always be 0 in FAST, so no need to calculate


   y%AllOuts( TwHtTPxi(I) ) =      rT(TwrGagNd(I),1)
   y%AllOuts( TwHtTPyi(I) ) = -1.0*rT(TwrGagNd(I),3)
   y%AllOuts( TwHtTPzi(I) ) =      rT(TwrGagNd(I),2) - p%PtfmRef

   y%AllOuts( TwHtRPxi(I) ) =  AngPosEF(TwrGagNd(I),1)*R2D
   y%AllOuts( TwHtRPyi(I) ) = -AngPosEF(TwrGagNd(I),3)*R2D
   y%AllOuts( TwHtRPzi(I) ) =  AngPosEF(TwrGagNd(I),2)*R2D

END DO !I

   ! Platform Motions:

y%AllOuts( PtfmTDxt) =  DOT_PRODUCT(       rZ, OtherState%CoordSys%a1 )
y%AllOuts( PtfmTDyt) = -DOT_PRODUCT(       rZ, OtherState%CoordSys%a3 )
y%AllOuts( PtfmTDzt) =  DOT_PRODUCT(       rZ, OtherState%CoordSys%a2 )
y%AllOuts( PtfmTDxi) = x%QT  (DOF_Sg  )
y%AllOuts( PtfmTDyi) = x%QT  (DOF_Sw  )
y%AllOuts( PtfmTDzi) = x%QT  (DOF_Hv  )
y%AllOuts( PtfmTVxt) =  DOT_PRODUCT( LinVelEZ, OtherState%CoordSys%a1 )
y%AllOuts( PtfmTVyt) = -DOT_PRODUCT( LinVelEZ, OtherState%CoordSys%a3 )
y%AllOuts( PtfmTVzt) =  DOT_PRODUCT( LinVelEZ, OtherState%CoordSys%a2 )
y%AllOuts( PtfmTVxi) = x%QDT (DOF_Sg  )
y%AllOuts( PtfmTVyi) = x%QDT (DOF_Sw  )
y%AllOuts( PtfmTVzi) = x%QDT (DOF_Hv  )
y%AllOuts( PtfmTAxt) =  DOT_PRODUCT( LinAccEZ, OtherState%CoordSys%a1 )
y%AllOuts( PtfmTAyt) = -DOT_PRODUCT( LinAccEZ, OtherState%CoordSys%a3 )
y%AllOuts( PtfmTAzt) =  DOT_PRODUCT( LinAccEZ, OtherState%CoordSys%a2 )
y%AllOuts( PtfmTAxi) = QD2T(DOF_Sg  )
y%AllOuts( PtfmTAyi) = QD2T(DOF_Sw  )
y%AllOuts( PtfmTAzi) = QD2T(DOF_Hv  )
y%AllOuts( PtfmRDxi) = x%QT  (DOF_R   )                       *R2D
y%AllOuts( PtfmRDyi) = x%QT  (DOF_P   )                       *R2D
y%AllOuts( PtfmRDzi) = x%QT  (DOF_Y   )                       *R2D
y%AllOuts( PtfmRVxt) =  DOT_PRODUCT( AngVelEX, OtherState%CoordSys%a1 )*R2D
y%AllOuts( PtfmRVyt) = -DOT_PRODUCT( AngVelEX, OtherState%CoordSys%a3 )*R2D
y%AllOuts( PtfmRVzt) =  DOT_PRODUCT( AngVelEX, OtherState%CoordSys%a2 )*R2D
y%AllOuts( PtfmRVxi) = x%QDT (DOF_R   )                       *R2D
y%AllOuts( PtfmRVyi) = x%QDT (DOF_P   )                       *R2D
y%AllOuts( PtfmRVzi) = x%QDT (DOF_Y   )                       *R2D
y%AllOuts( PtfmRAxt) =  DOT_PRODUCT( AngAccEX, OtherState%CoordSys%a1 )*R2D
y%AllOuts( PtfmRAyt) = -DOT_PRODUCT( AngAccEX, OtherState%CoordSys%a3 )*R2D
y%AllOuts( PtfmRAzt) =  DOT_PRODUCT( AngAccEX, OtherState%CoordSys%a2 )*R2D
y%AllOuts( PtfmRAxi) = QD2T(DOF_R   )                       *R2D
y%AllOuts( PtfmRAyi) = QD2T(DOF_P   )                       *R2D
y%AllOuts( PtfmRAzi) = QD2T(DOF_Y   )                       *R2D


   ! Nacelle Yaw Error Estimate:

IF ( CompAero )  THEN   ! AeroDyn has been used
!print *, y%AllOuts(HorWndDir)
!print *, y%AllOuts(YawPzn), y%AllOuts(YawBrRDzt), y%AllOuts(PtfmRDzi)

   y%AllOuts(NacYawErr) = y%AllOuts(HorWndDir) - y%AllOuts(YawPzn) - y%AllOuts(YawBrRDzt) - y%AllOuts(PtfmRDzi)
!print *, y%AllOuts(NacYawErr)

ENDIF



   ! Blade Root Loads:

DO K=1,p%NumBl
   y%AllOuts( RootFxc(K) ) = DOT_PRODUCT( FrcS0B(K,:), OtherState%CoordSys%i1(K,:) )
   y%AllOuts( RootFyc(K) ) = DOT_PRODUCT( FrcS0B(K,:), OtherState%CoordSys%i2(K,:) )
   y%AllOuts( RootFzc(K) ) = DOT_PRODUCT( FrcS0B(K,:), OtherState%CoordSys%i3(K,:) )
   y%AllOuts( RootFxb(K) ) = DOT_PRODUCT( FrcS0B(K,:), OtherState%CoordSys%j1(K,:) )
   y%AllOuts( RootFyb(K) ) = DOT_PRODUCT( FrcS0B(K,:), OtherState%CoordSys%j2(K,:) )
   y%AllOuts( RootMxc(K) ) = DOT_PRODUCT( MomH0B(K,:), OtherState%CoordSys%i1(K,:) )
   y%AllOuts( RootMyc(K) ) = DOT_PRODUCT( MomH0B(K,:), OtherState%CoordSys%i2(K,:) )
   y%AllOuts( RootMzc(K) ) = DOT_PRODUCT( MomH0B(K,:), OtherState%CoordSys%i3(K,:) )
   y%AllOuts( RootMxb(K) ) = DOT_PRODUCT( MomH0B(K,:), OtherState%CoordSys%j1(K,:) )
   y%AllOuts( RootMyb(K) ) = DOT_PRODUCT( MomH0B(K,:), OtherState%CoordSys%j2(K,:) )
END DO !K


   ! Blade Local Span Loads:

DO K = 1,p%NumBl
   DO I = 1,p%NBlGages

      ! Initialize FrcMGagB and MomMGagB using the tip brake effects:

         FrcMGagB = FSTipDrag(K,:) - TipMass(K)*( Gravity*OtherState%CoordSys%z2 + LinAccES(K,p%TipNode,:) )
         MomMGagB = CROSS_PRODUCT( rS0S(K,p%TipNode,:) - rS0S(K,BldGagNd(I),:), FrcMGagB )


      ! Integrate to find FrcMGagB and MomMGagB using all of the nodes / elements above the current strain gage location:
         DO J = ( BldGagNd(I) + 1 ),p%BldNodes ! Loop through blade nodes / elements above strain gage node

            TmpVec2  = FSAero(K,J,:) - MassB(K,J)*( Gravity*OtherState%CoordSys%z2 + LinAccES(K,J,:) )  ! Portion of FrcMGagB associated with element J
            FrcMGagB = FrcMGagB + TmpVec2*DRNodes(J)

            TmpVec = CROSS_PRODUCT( rS0S(K,J,:) - rS0S(K,BldGagNd(I),:), TmpVec2 )           ! Portion of MomMGagB associated with element J
            MomMGagB = MomMGagB + ( TmpVec + MMAero(K,J,:) )*DRNodes(J)

         ENDDO ! J - Blade nodes / elements above strain gage node

      ! Add the effects of 1/2 the strain gage element:
      ! NOTE: for the radius in this calculation, assume that there is no
      !   shortening effect (due to blade bending) within the element.  Thus,
      !   the moment arm for the force is 1/4 of DRNodes() and the element
      !   length is 1/2 of DRNodes().

         TmpVec2  = FSAero(K,BldGagNd(I),:) - MassB(K,BldGagNd(I))* ( Gravity*OtherState%CoordSys%z2 + LinAccES(K,BldGagNd(I),:) ) ! Portion of FrcMGagB associated with 1/2 of the strain gage element
         FrcMGagB = FrcMGagB + TmpVec2 * 0.5 * DRNodes(BldGagNd(I))                                                    ! Portion of FrcMGagB associated with 1/2 of the strain gage element
         FrcMGagB = 0.001*FrcMGagB           ! Convert the local force to kN


         TmpVec = CROSS_PRODUCT( ( 0.25*DRNodes(BldGagNd(I)) )*OtherState%CoordSys%j3(K,:), TmpVec2 )                              ! Portion of MomMGagB associated with 1/2 of the strain gage element

         MomMGagB = MomMGagB + ( TmpVec + MMAero(K,BldGagNd(I),:) )* ( 0.5 *DRNodes(BldGagNd(I)) )
         MomMGagB = 0.001*MomMGagB           ! Convert the local moment to kN-m


         y%AllOuts(SpnFLxb(I,K)) = DOT_PRODUCT( FrcMGagB, OtherState%CoordSys%n1(K,BldGagNd(I),:) )
         y%AllOuts(SpnFLyb(I,K)) = DOT_PRODUCT( FrcMGagB, OtherState%CoordSys%n2(K,BldGagNd(I),:) )
         y%AllOuts(SpnFLzb(I,K)) = DOT_PRODUCT( FrcMGagB, OtherState%CoordSys%n3(K,BldGagNd(I),:) )

         y%AllOuts(SpnMLxb(I,K)) = DOT_PRODUCT( MomMGagB, OtherState%CoordSys%n1(K,BldGagNd(I),:) )
         y%AllOuts(SpnMLyb(I,K)) = DOT_PRODUCT( MomMGagB, OtherState%CoordSys%n2(K,BldGagNd(I),:) )
         y%AllOuts(SpnMLzb(I,K)) = DOT_PRODUCT( MomMGagB, OtherState%CoordSys%n3(K,BldGagNd(I),:) )
   END DO ! I
END DO ! K



   ! Hub and Rotor Loads:

ComDenom = 0.5*AirDens*p%ProjArea*y%AllOuts(  WindVxi)*y%AllOuts(  WindVxi)   ! Common denominator used in several expressions

y%AllOuts(LSShftFxa) =  DOT_PRODUCT(  FrcPRot, OtherState%CoordSys%e1 )
y%AllOuts(LSShftFya) =  DOT_PRODUCT(  FrcPRot, OtherState%CoordSys%e2 )
y%AllOuts(LSShftFza) =  DOT_PRODUCT(  FrcPRot, OtherState%CoordSys%e3 )
y%AllOuts(LSShftFys) = -DOT_PRODUCT(  FrcPRot, OtherState%CoordSys%c3 )
y%AllOuts(LSShftFzs) =  DOT_PRODUCT(  FrcPRot, OtherState%CoordSys%c2 )
y%AllOuts(LSShftMxa) =  DOT_PRODUCT( MomLPRot, OtherState%CoordSys%e1 )
y%AllOuts(LSSTipMya) =  DOT_PRODUCT( MomLPRot, OtherState%CoordSys%e2 )
y%AllOuts(LSSTipMza) =  DOT_PRODUCT( MomLPRot, OtherState%CoordSys%e3 )
y%AllOuts(LSSTipMys) = -DOT_PRODUCT( MomLPRot, OtherState%CoordSys%c3 )
y%AllOuts(LSSTipMzs) =  DOT_PRODUCT( MomLPRot, OtherState%CoordSys%c2 )
IF ( y%AllOuts(LSShftFxa) /= 0.0 )  THEN ! .TRUE. if the denominator in the following equations is not zero.

   CThrstys = -y%AllOuts(LSSTipMzs)/y%AllOuts(LSShftFxa)  ! Estimate of the ys-location of the center of thrust
   CThrstzs =  y%AllOuts(LSSTipMys)/y%AllOuts(LSShftFxa)  ! Estimate of the zs-location of the center of thrust

   IF ( IgnoreMOD )  THEN  ! Don't use MOD when computing y%AllOuts(CThrstAzm) -- IgnoreMOD is needed when computing CMat for azimuth measurements during FAST linearization.
      y%AllOuts(CThrstAzm) =      ATAN2( -CThrstzs, -CThrstys )*R2D + 360.0 + p%AzimB1Up + 90.0
   ELSE                    ! Do    use MOD when computing y%AllOuts(CThrstAzm)
      y%AllOuts(CThrstAzm) = MOD( ATAN2( -CThrstzs, -CThrstys )*R2D + 360.0 + p%AzimB1Up + 90.0, 360.0 )
   ENDIF
   y%AllOuts(   CThrstRad) = MIN( 1.0, SQRT( CThrstys*CThrstys + CThrstzs*CThrstzs )/p%AvgNrmTpRd )

ELSE

   y%AllOuts(CThrstAzm) = 0.0
   y%AllOuts(CThrstRad) = 0.0

ENDIF
y%AllOuts(   RotPwr) = ( x%QDT(DOF_GeAz) + x%QDT(DOF_DrTr) )*y%AllOuts(LSShftMxa)
IF ( ComDenom /= 0.0 )  THEN  ! .TRUE. if the denominator in the following equations is not zero.

   y%AllOuts( RotCq) = 1000.0*y%AllOuts(LSShftMxa) / ( ComDenom*p%TipRad )
   y%AllOuts( RotCp) = 1000.0*y%AllOuts(   RotPwr) / ( ComDenom*y%AllOuts(  WindVxi) )
   y%AllOuts( RotCt) = 1000.0*y%AllOuts(LSShftFxa) /   ComDenom

ELSE

   y%AllOuts( RotCq) = 0.0
   y%AllOuts( RotCp) = 0.0
   y%AllOuts( RotCt) = 0.0

ENDIF


   ! Shaft Strain Gage Loads:

y%AllOuts(LSSGagMya) = y%AllOuts(LSSTipMya) + ShftGagL*y%AllOuts(LSShftFza)
y%AllOuts(LSSGagMza) = y%AllOuts(LSSTipMza) - ShftGagL*y%AllOuts(LSShftFya)
y%AllOuts(LSSGagMys) = y%AllOuts(LSSTipMys) + ShftGagL*y%AllOuts(LSShftFzs)
y%AllOuts(LSSGagMzs) = y%AllOuts(LSSTipMzs) - ShftGagL*y%AllOuts(LSShftFys)


   ! Generator and High-Speed Shaft Loads:

y%AllOuts( HSShftTq) = y%AllOuts(LSShftMxa)*GBoxEffFac/GBRatio
y%AllOuts(HSShftPwr) = y%AllOuts( HSShftTq)*GBRatio*x%QDT(DOF_GeAz)
y%AllOuts(  HSSBrTq) = 0.001*HSSBrTrq
y%AllOuts(    GenTq) = 0.001*GenTrq
y%AllOuts(   GenPwr) = 0.001*ElecPwr
IF ( ComDenom /= 0.0 )  THEN  ! .TRUE. if the denominator in the following equations is not zero (ComDenom is the same as it is calculated above).

   y%AllOuts( HSShftCq) = 1000.0*y%AllOuts( HSShftTq) / ( ComDenom*p%TipRad )
   y%AllOuts( HSShftCp) = 1000.0*y%AllOuts(HSShftPwr) / ( ComDenom*y%AllOuts(  WindVxi) )
   y%AllOuts(    GenCq) = 1000.0*y%AllOuts(    GenTq) / ( ComDenom*p%TipRad )
   y%AllOuts(    GenCp) = 1000.0*y%AllOuts(   GenPwr) / ( ComDenom*y%AllOuts(  WindVxi) )

ELSE

   y%AllOuts( HSShftCq) = 0.0
   y%AllOuts( HSShftCp) = 0.0
   y%AllOuts(    GenCq) = 0.0
   y%AllOuts(    GenCp) = 0.0

ENDIF


   ! Rotor-Furl Axis Loads:

y%AllOuts(RFrlBrM  ) =  DOT_PRODUCT( MomNGnRt, OtherState%CoordSys%rfa )


   ! Tail-Furl Axis Loads:

y%AllOuts(TFrlBrM  ) =  DOT_PRODUCT( MomNTail, OtherState%CoordSys%tfa )


   ! Tower-Top / Yaw Bearing Loads:

y%AllOuts( YawBrFxn) =  DOT_PRODUCT( FrcONcRt, OtherState%CoordSys%d1 )
y%AllOuts( YawBrFyn) = -DOT_PRODUCT( FrcONcRt, OtherState%CoordSys%d3 )
y%AllOuts( YawBrFzn) =  DOT_PRODUCT( FrcONcRt, OtherState%CoordSys%d2 )
y%AllOuts( YawBrFxp) =  DOT_PRODUCT( FrcONcRt, OtherState%CoordSys%b1 )
y%AllOuts( YawBrFyp) = -DOT_PRODUCT( FrcONcRt, OtherState%CoordSys%b3 )
y%AllOuts( YawBrMxn) =  DOT_PRODUCT( MomBNcRt, OtherState%CoordSys%d1 )
y%AllOuts( YawBrMyn) = -DOT_PRODUCT( MomBNcRt, OtherState%CoordSys%d3 )
y%AllOuts( YawBrMzn) =  DOT_PRODUCT( MomBNcRt, OtherState%CoordSys%d2 )
y%AllOuts( YawBrMxp) =  DOT_PRODUCT( MomBNcRt, OtherState%CoordSys%b1 )
y%AllOuts( YawBrMyp) = -DOT_PRODUCT( MomBNcRt, OtherState%CoordSys%b3 )


   ! Tower Base Loads:

y%AllOuts( TwrBsFxt) =  DOT_PRODUCT( FrcT0Trb, OtherState%CoordSys%a1 )
y%AllOuts( TwrBsFyt) = -DOT_PRODUCT( FrcT0Trb, OtherState%CoordSys%a3 )
y%AllOuts( TwrBsFzt) =  DOT_PRODUCT( FrcT0Trb, OtherState%CoordSys%a2 )
y%AllOuts( TwrBsMxt) =  DOT_PRODUCT( MomX0Trb, OtherState%CoordSys%a1 )
y%AllOuts( TwrBsMyt) = -DOT_PRODUCT( MomX0Trb, OtherState%CoordSys%a3 )
y%AllOuts( TwrBsMzt) =  DOT_PRODUCT( MomX0Trb, OtherState%CoordSys%a2 )


   ! Local Tower Loads:

FrcONcRt = 1000.0*FrcONcRt ! Convert the units of these forces and moments
MomBNcRt = 1000.0*MomBNcRt ! from kN and kN-m back to N and N-m, respectively.

DO I=1,p%NTwGages

   ! Initialize FrcFGagT and MomFGagT using the tower-top and yaw bearing mass effects:
   FrcFGagT = FrcONcRt - YawBrMass*( Gravity*OtherState%CoordSys%z2 + LinAccEO )
   MomFGagT = CROSS_PRODUCT( rZO - rZT(TwrGagNd(I),:), FrcFGagT )
   MomFGagT = MomFGagT + MomBNcRt

   ! Integrate to find FrcFGagT and MomFGagT using all of the nodes / elements above the current strain gage location:
   DO J = ( TwrGagNd(I) + 1 ),p%TwrNodes ! Loop through tower nodes / elements above strain gage node
      TmpVec2  = FTAero(J,:) + FTHydro(J,:) - MassT(J)*( Gravity*OtherState%CoordSys%z2 + LinAccET(J,:) )           ! Portion of FrcFGagT associated with element J
      FrcFGagT = FrcFGagT + TmpVec2*DHNodes(J)

      TmpVec = CROSS_PRODUCT( rZT(J,:) - rZT(TwrGagNd(I),:), TmpVec2 )                          ! Portion of MomFGagT associated with element J
      MomFGagT = MomFGagT + ( TmpVec + MFAero(J,:) + MFHydro(J,:) )*DHNodes(J)
   ENDDO ! J -Tower nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no shortening
   !   effect (due to tower bending) within the element.  Thus, the moment arm
   !   for the force is 1/4 of DHNodes() and the element length is 1/2 of DHNodes().

   TmpVec2  = FTAero(TwrGagNd(I),:) + FTHydro(TwrGagNd(I),:) - MassT(TwrGagNd(I))*( Gravity*OtherState%CoordSys%z2 + LinAccET(TwrGagNd(I),:))

   FrcFGagT = FrcFGagT + TmpVec2 * 0.5 * DHNodes(TwrGagNd(I))
   FrcFGagT = 0.001*FrcFGagT  ! Convert the local force to kN

   TmpVec = CROSS_PRODUCT( ( 0.25*DHNodes( TwrGagNd(I)) )*OtherState%CoordSys%a2, TmpVec2 )              ! Portion of MomFGagT associated with 1/2 of the strain gage element
   TmpVec   = TmpVec   + MFAero(TwrGagNd(I),:) + MFHydro(TwrGagNd(I),:)
   MomFGagT = MomFGagT + TmpVec * 0.5 * DHNodes(TwrGagNd(I))
   MomFGagT = 0.001*MomFGagT  ! Convert the local moment to kN-m

   y%AllOuts( TwHtFLxt(I) ) =     DOT_PRODUCT( FrcFGagT, OtherState%CoordSys%t1(TwrGagNd(I),:) )
   y%AllOuts( TwHtFLyt(I) ) = -1.*DOT_PRODUCT( FrcFGagT, OtherState%CoordSys%t3(TwrGagNd(I),:) )
   y%AllOuts( TwHtFLzt(I) ) =     DOT_PRODUCT( FrcFGagT, OtherState%CoordSys%t2(TwrGagNd(I),:) )

   y%AllOuts( TwHtMLxt(I) ) =     DOT_PRODUCT( MomFGagT, OtherState%CoordSys%t1(TwrGagNd(I),:) )
   y%AllOuts( TwHtMLyt(I) ) = -1.*DOT_PRODUCT( MomFGagT, OtherState%CoordSys%t3(TwrGagNd(I),:) )
   y%AllOuts( TwHtMLzt(I) ) =     DOT_PRODUCT( MomFGagT, OtherState%CoordSys%t2(TwrGagNd(I),:) )

END DO


   ! Platform Loads:

y%AllOuts(  PtfmFxt) =  DOT_PRODUCT( FZHydro, OtherState%CoordSys%a1 )
y%AllOuts(  PtfmFyt) = -DOT_PRODUCT( FZHydro, OtherState%CoordSys%a3 )
y%AllOuts(  PtfmFzt) =  DOT_PRODUCT( FZHydro, OtherState%CoordSys%a2 )
y%AllOuts(  PtfmFxi) =  DOT_PRODUCT( FZHydro, OtherState%CoordSys%z1 )
y%AllOuts(  PtfmFyi) = -DOT_PRODUCT( FZHydro, OtherState%CoordSys%z3 )
y%AllOuts(  PtfmFzi) =  DOT_PRODUCT( FZHydro, OtherState%CoordSys%z2 )
y%AllOuts(  PtfmMxt) =  DOT_PRODUCT( MXHydro, OtherState%CoordSys%a1 )
y%AllOuts(  PtfmMyt) = -DOT_PRODUCT( MXHydro, OtherState%CoordSys%a3 )
y%AllOuts(  PtfmMzt) =  DOT_PRODUCT( MXHydro, OtherState%CoordSys%a2 )
y%AllOuts(  PtfmMxi) =  DOT_PRODUCT( MXHydro, OtherState%CoordSys%z1 )
y%AllOuts(  PtfmMyi) = -DOT_PRODUCT( MXHydro, OtherState%CoordSys%z3 )
y%AllOuts(  PtfmMzi) =  DOT_PRODUCT( MXHydro, OtherState%CoordSys%z2 )


IF ( CompHydro )  THEN  ! Hydrodynamics have been used


   ! Mooring Line Loads:
   DO I = 1,NumLines
      CALL FairleadTension ( I, FairTe, FairTeAng )
      CALL AnchorTension   ( I, AnchTe, AnchTeAng )
      y%AllOuts( FairTen(I) ) = FairTe   *0.001   ! Convert to kN
      y%AllOuts( FairAng(I) ) = FairTeAng*R2D     ! Convert to degrees
      y%AllOuts( AnchTen(I) ) = AnchTe   *0.001   ! Convert to kN
      y%AllOuts( AnchAng(I) ) = AnchTeAng*R2D     ! Convert to degrees
   END DO

END IF


   ! Internal DOF outputs:

y%AllOuts( Q_B1E1   ) = x%QT(   DOF_BE(1,1) )
y%AllOuts( Q_B2E1   ) = x%QT(   DOF_BE(2,1) )
y%AllOuts( Q_B1F1   ) = x%QT(   DOF_BF(1,1) )
y%AllOuts( Q_B2F1   ) = x%QT(   DOF_BF(2,1) )
y%AllOuts( Q_B1F2   ) = x%QT(   DOF_BF(1,2) )
y%AllOuts( Q_B2F2   ) = x%QT(   DOF_BF(2,2) )
y%AllOuts( Q_DrTr   ) = x%QT(   DOF_DrTr    )
y%AllOuts( Q_GeAz   ) = x%QT(   DOF_GeAz    )
y%AllOuts( Q_RFrl   ) = x%QT(   DOF_RFrl    )
y%AllOuts( Q_TFrl   ) = x%QT(   DOF_TFrl    )
y%AllOuts( Q_Yaw    ) = x%QT(   DOF_Yaw     )
y%AllOuts( Q_TFA1   ) = x%QT(   DOF_TFA1    )
y%AllOuts( Q_TSS1   ) = x%QT(   DOF_TSS1    )
y%AllOuts( Q_TFA2   ) = x%QT(   DOF_TFA2    )
y%AllOuts( Q_TSS2   ) = x%QT(   DOF_TSS2    )
y%AllOuts( Q_Sg     ) = x%QT(   DOF_Sg      )
y%AllOuts( Q_Sw     ) = x%QT(   DOF_Sw      )
y%AllOuts( Q_Hv     ) = x%QT(   DOF_Hv      )
y%AllOuts( Q_R      ) = x%QT(   DOF_R       )
y%AllOuts( Q_P      ) = x%QT(   DOF_P       )
y%AllOuts( Q_Y      ) = x%QT(   DOF_Y       )

y%AllOuts( QD_B1E1  ) = x%QDT(  DOF_BE(1,1) )
y%AllOuts( QD_B2E1  ) = x%QDT(  DOF_BE(2,1) )
y%AllOuts( QD_B1F1  ) = x%QDT(  DOF_BF(1,1) )
y%AllOuts( QD_B2F1  ) = x%QDT(  DOF_BF(2,1) )
y%AllOuts( QD_B1F2  ) = x%QDT(  DOF_BF(1,2) )
y%AllOuts( QD_B2F2  ) = x%QDT(  DOF_BF(2,2) )
y%AllOuts( QD_DrTr  ) = x%QDT(  DOF_DrTr    )
y%AllOuts( QD_GeAz  ) = x%QDT(  DOF_GeAz    )
y%AllOuts( QD_RFrl  ) = x%QDT(  DOF_RFrl    )
y%AllOuts( QD_TFrl  ) = x%QDT(  DOF_TFrl    )
y%AllOuts( QD_Yaw   ) = x%QDT(  DOF_Yaw     )
y%AllOuts( QD_TFA1  ) = x%QDT(  DOF_TFA1    )
y%AllOuts( QD_TSS1  ) = x%QDT(  DOF_TSS1    )
y%AllOuts( QD_TFA2  ) = x%QDT(  DOF_TFA2    )
y%AllOuts( QD_TSS2  ) = x%QDT(  DOF_TSS2    )
y%AllOuts( QD_Sg    ) = x%QDT(  DOF_Sg      )
y%AllOuts( QD_Sw    ) = x%QDT(  DOF_Sw      )
y%AllOuts( QD_Hv    ) = x%QDT(  DOF_Hv      )
y%AllOuts( QD_R     ) = x%QDT(  DOF_R       )
y%AllOuts( QD_P     ) = x%QDT(  DOF_P       )
y%AllOuts( QD_Y     ) = x%QDT(  DOF_Y       )

y%AllOuts( QD2_B1E1 ) = QD2T( DOF_BE(1,1) )
y%AllOuts( QD2_B2E1 ) = QD2T( DOF_BE(2,1) )
y%AllOuts( QD2_B1F1 ) = QD2T( DOF_BF(1,1) )
y%AllOuts( QD2_B2F1 ) = QD2T( DOF_BF(2,1) )
y%AllOuts( QD2_B1F2 ) = QD2T( DOF_BF(1,2) )
y%AllOuts( QD2_B2F2 ) = QD2T( DOF_BF(2,2) )
y%AllOuts( QD2_DrTr ) = QD2T( DOF_DrTr    )
y%AllOuts( QD2_GeAz ) = QD2T( DOF_GeAz    )
y%AllOuts( QD2_RFrl ) = QD2T( DOF_RFrl    )
y%AllOuts( QD2_TFrl ) = QD2T( DOF_TFrl    )
y%AllOuts( QD2_Yaw  ) = QD2T( DOF_Yaw     )
y%AllOuts( QD2_TFA1 ) = QD2T( DOF_TFA1    )
y%AllOuts( QD2_TSS1 ) = QD2T( DOF_TSS1    )
y%AllOuts( QD2_TFA2 ) = QD2T( DOF_TFA2    )
y%AllOuts( QD2_TSS2 ) = QD2T( DOF_TSS2    )
y%AllOuts( QD2_Sg   ) = QD2T( DOF_Sg      )
y%AllOuts( QD2_Sw   ) = QD2T( DOF_Sw      )
y%AllOuts( QD2_Hv   ) = QD2T( DOF_Hv      )
y%AllOuts( QD2_R    ) = QD2T( DOF_R       )
y%AllOuts( QD2_P    ) = QD2T( DOF_P       )
y%AllOuts( QD2_Y    ) = QD2T( DOF_Y       )

IF ( p%NumBl > 2 ) THEN
   y%AllOuts( Q_B3E1   ) = x%QT(   DOF_BE(3,1) )
   y%AllOuts( Q_B3F1   ) = x%QT(   DOF_BF(3,1) )
   y%AllOuts( Q_B3F2   ) = x%QT(   DOF_BF(3,2) )

   y%AllOuts( QD_B3E1  ) = x%QDT(  DOF_BE(3,1) )
   y%AllOuts( QD_B3F1  ) = x%QDT(  DOF_BF(3,1) )
   y%AllOuts( QD_B3F2  ) = x%QDT(  DOF_BF(3,2) )

   y%AllOuts( QD2_B3E1 ) = QD2T( DOF_BE(3,1) )
   y%AllOuts( QD2_B3F1 ) = QD2T( DOF_BF(3,1) )
   y%AllOuts( QD2_B3F2 ) = QD2T( DOF_BF(3,2) )

ELSE
   y%AllOuts( Q_Teet   ) = x%QT(   DOF_Teet    )
   y%AllOuts( QD_Teet  ) = x%QDT(  DOF_Teet    )
   y%AllOuts( QD2_Teet ) =   QD2T( DOF_Teet    )
END IF




   ! Place the selected output channels into the OutData(:) array with
   !   the proper sign:

DO I = 0,p%NumOuts  ! Loop through all selected output channels

   OutData(I) = p%OutParam(I)%SignM * y%AllOuts( p%OutParam(I)%Indx )

ENDDO             ! I - All selected output channels



RETURN
END SUBROUTINE CalcOuts
!=======================================================================
SUBROUTINE Coeff(p)


   ! This routine is used to compute rotor (blade and hub) properties:
   !   KBF(), KBE(), CBF(), CBE(), FreqBF(), FreqBE(), AxRedBld(),
   !   TwistedSF(), BldMass(), FirstMom(), SecondMom(), BldCG(),
   !   RotMass, RotIner, Hubg1Iner, Hubg2Iner, rSAerCenn1(), and
   !   rSAerCenn2()
   ! tower properties:
   !   KTFA(), KTSS(), CTFA(), CTSS(), FreqTFA(), FreqTSS(),
   !   AxRedTFA(), AxRedTSS(), TwrFASF(), TwrSSSF(), TwrMass, and
   !   TwrTpMass
   ! structure that furls with the rotor (not including rotor) properties:
   !   RrfaIner
   ! tail boom properties:
   !   AtfaIner
   ! nacelle properties:
   !   Nacd2Iner
   ! and generator properties:
   !   GenDir.


USE                             Blades
USE                             DriveTrain
USE                             EnvCond
USE                             Features
USE                             InitCond
USE                             MassInert
USE                             Modes
USE                             Tower


IMPLICIT                        NONE


   ! Passed variables

TYPE(StrD_ParameterType),        INTENT(IN)    :: p                             ! Parameters of the structural dynamics module


   ! Local variables.

REAL(ReKi)                   :: AxRdBld   (3,3)                                 ! Temporary result holding the current addition to the AxRedBld() array.
REAL(ReKi)                   :: AxRdBldOld(3,3)                                 ! Previous AxRdBld (i.e., AxRdBld from the previous node)
REAL(ReKi)                   :: AxRdTFA   (2,2)                                 ! Temporary result holding the current addition to the AxRedTFA() array.
REAL(ReKi)                   :: AxRdTFAOld(2,2)                                 ! Previous AxRdTFA (i.e., AxRdTFA from the previous node)
REAL(ReKi)                   :: AxRdTSS   (2,2)                                 ! Temporary result holding the current addition to the AxRedTSS() array.
REAL(ReKi)                   :: AxRdTSSOld(2,2)                                 ! Previous AxRdTSS (i.e., AxRdTSS from the previous node)
REAL(ReKi)                   :: TmpDist                                         ! Temporary distance used in the calculation of the aero center locations.
REAL(ReKi)                   :: TmpDistj1                                       ! Temporary distance used in the calculation of the aero center locations.
REAL(ReKi)                   :: TmpDistj2                                       ! Temporary distance used in the calculation of the aero center locations.
REAL(ReKi)                   :: ElMassOld                                       ! Previous ElmntMass (i.e., ElmntMass from the previous node)
REAL(ReKi)                   :: ElmntMass                                       ! (Temporary) mass of an element.
REAL(ReKi)                   :: ElmntStff                                       ! (Temporary) stiffness of an element.
REAL(ReKi)                   :: ElStffFA                                        ! (Temporary) tower fore-aft stiffness of an element
REAL(ReKi)                   :: ElStffSS                                        ! (Temporary) tower side-to-side  stiffness of an element
REAL(ReKi), ALLOCATABLE      :: FMomAbvNd (:,:)                                 ! FMomAbvNd(K,J) = portion of the first moment of blade K about the rotor centerline (not root, like FirstMom(K)) associated with everything above node J (including tip brake masses).
REAL(ReKi), ALLOCATABLE      :: KBECent   (:,:,:)                               ! Centrifugal-term of generalized edgewise stiffness of the blades.
REAL(ReKi), ALLOCATABLE      :: KBFCent   (:,:,:)                               ! Centrifugal-term of generalized flapwise stiffness of the blades.
REAL(ReKi)                   :: KTFAGrav  (2,2)                                 ! Gravitational-term of generalized fore-aft stiffness of the tower.
REAL(ReKi)                   :: KTSSGrav  (2,2)                                 ! Gravitational-term of generalized side-to-side stiffness of the tower.
REAL(ReKi), ALLOCATABLE      :: MBE       (:,:,:)                               ! Generalized edgewise mass of the blades.
REAL(ReKi), ALLOCATABLE      :: MBF       (:,:,:)                               ! Generalized flapwise mass of the blades.
REAL(ReKi)                   :: MTFA      (2,2)                                 ! Generalized fore-aft mass of the tower.
REAL(ReKi)                   :: MTSS      (2,2)                                 ! Generalized side-to-side mass of the tower.
REAL(ReKi)                   :: Shape                                           ! Temporary result holding a value from the SHP function
REAL(ReKi)                   :: Shape1                                          ! Temporary result holding a value from the SHP function
REAL(ReKi)                   :: Shape2                                          ! Temporary result holding a value from the SHP function
REAL(ReKi), ALLOCATABLE      :: TMssAbvNd (:)                                   ! Portion of the tower mass associated with everything above node J (including tower-top effects)
REAL(ReKi)                   :: TwstdSF   (2,3,0:1)                             ! Temperory result holding the current addition to the TwistedSF() array.
REAL(ReKi)                   :: TwstdSFOld(2,3,0:1)                             ! Previous TwstdSF (i.e., TwstdSF from the previous node)

INTEGER(4)                   :: I                                               ! Generic index.
INTEGER(4)                   :: J                                               ! Loops through nodes / elements.
INTEGER(4)                   :: K                                               ! Loops through blades.
INTEGER(4)                   :: L                                               ! Generic index
INTEGER(4)                   :: Sttus                                           ! Status returned from an allocation request.


   ! ALLOCATE some local arrays:
Sttus = 0.0

IF (.NOT. ALLOCATED( FMomAbvNd )) ALLOCATE ( FMomAbvNd(p%NumBl,p%BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the FMomAbvNd array.' )
ENDIF

IF (.NOT. ALLOCATED( KBECent )) ALLOCATE ( KBECent(p%NumBl,1,1) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the KBECent array.' )
ENDIF

IF (.NOT. ALLOCATED( KBFCent )) ALLOCATE ( KBFCent(p%NumBl,2,2) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the KBFCent array.' )
ENDIF

IF (.NOT. ALLOCATED( MBE )) ALLOCATE ( MBE(p%NumBl,1,1) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the MBE array.' )
ENDIF

IF (.NOT. ALLOCATED( MBF )) ALLOCATE ( MBF(p%NumBl,2,2) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the MBF array.' )
ENDIF

IF (.NOT. ALLOCATED( TMssAbvNd )) ALLOCATE ( TMssAbvNd(p%TwrNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the TMssAbvNd array.' )
ENDIF



   ! Calculate the distances from point S on a blade to the aerodynamic
   !   center in the j1 and j2 directions:

DO K = 1,p%NumBl          ! Loop through the blades

   DO J = 1,p%BldNodes    ! Loop through the blade nodes / elements

      TmpDist         = ( AeroCent(K,J) - 0.25 )*Chord(J)   ! Distance along the chordline from point S (25% chord) to the aerodynamic center of the blade element J--positive towards the trailing edge.
      TmpDistj1       = TmpDist*SAeroTwst(J)                ! Distance along the j1-axis   from point S (25% chord) to the aerodynamic center of the blade element J
      TmpDistj2       = TmpDist*CAeroTwst(J)                ! Distance along the j2-axis   from point S (25% chord) to the aerodynamic center of the blade element J
      rSAerCenn1(K,J) = TmpDistj1*CThetaS(K,J) - TmpDistj2*SThetaS(K,J)
      rSAerCenn2(K,J) = TmpDistj1*SThetaS(K,J) + TmpDistj2*CThetaS(K,J)

   ENDDO ! J - Blade nodes / elements

ENDDO    ! K - Blades


   ! Calculate the generator direction using GBRevers:

IF ( GBRevers )  THEN   ! HSS and LSS rotate in opposite directions
   GenDir = -1
ELSE                    ! HSS and LSS rotate in the same direction
   GenDir =  1
ENDIF


   ! Calculate the structure that furls with the rotor inertia term:

RrfaIner  = RFrlIner - RFrlMass*(      (p%rVDxn**2    )*( 1.0 - p%CRFrlSkw2*p%CRFrlTlt2 ) &
                                  +    (p%rVDzn**2    )*                    p%CRFrlTlt2   &
                                  +    (p%rVDyn**2    )*( 1.0 - p%SRFrlSkw2*p%CRFrlTlt2 ) &
                                  - 2.0*p%rVDxn*p%rVDzn*        p%CRFrlSkew*p%CSRFrlTlt   &
                                  - 2.0*p%rVDxn*p%rVDyn*        p%CSRFrlSkw*p%CRFrlTlt2   &
                                  - 2.0*p%rVDzn*p%rVDyn*        p%SRFrlSkew*p%CSRFrlTlt     )
IF ( RrfaIner < 0.0 )   CALL ProgAbort ( ' RFrlIner must not be less than RFrlMass*( perpendicular distance between rotor-furl'// &
                                     ' axis and CM of the structure that furls with the rotor [not including rotor] )^2.'       )


   ! Calculate the tail boom inertia term:

AtfaIner  = TFrlIner - BoomMass*(       p%rWIxn*p%rWIxn*( 1.0 - p%CTFrlSkw2*p%CTFrlTlt2 ) &
                                  +     p%rWIzn*p%rWIzn*                    p%CTFrlTlt2   &
                                  +     p%rWIyn*p%rWIyn*( 1.0 - p%STFrlSkw2*p%CTFrlTlt2 ) &
                                  - 2.0*p%rWIxn*p%rWIzn*        p%CTFrlSkew*p%CSTFrlTlt   &
                                  - 2.0*p%rWIxn*p%rWIyn*        p%CSTFrlSkw*p%CTFrlTlt2   &
                                  - 2.0*p%rWIzn*p%rWIyn*        p%STFrlSkew*p%CSTFrlTlt     )
IF ( AtfaIner < 0.0 )   CALL ProgAbort ( ' TFrlIner must not be less than BoomMass*( perpendicular distance between tail-furl'// &
                                     ' axis and tail boom CM )^2.'                                                             )


   ! Calculate the nacelle inertia terms:

Nacd2Iner = NacYIner - NacMass*( p%NacCMxn**2 + p%NacCMyn**2 ) ! Nacelle inertia about the d2-axis
IF ( Nacd2Iner < 0.0 )  CALL ProgAbort ( ' NacYIner must not be less than NacMass*( NacCMxn^2 + NacCMyn^2 ).' )


   ! Calculate hub inertia about its centerline passing through its c.g..
   !   This calculation assumes that the hub for a 2-blader is essentially
   !   a uniform cylinder whose centerline is transverse through the cylinder
   !   passing through its c.g..  That is, for a 2-blader, Hubg1Iner =
   !   Hubg2Iner is the inertia of the hub about both the g1- and g2- axes.  For
   !   3-bladers, Hubg1Iner is simply equal to HubIner and Hubg2Iner is zero.
   ! Also, Initialize RotMass and RotIner to associated hub properties:

IF ( p%NumBl == 2 )  THEN ! 2-blader
   Hubg1Iner = ( HubIner - HubMass*( ( p%UndSling - p%HubCM )**2 ) )/( p%CosDel3**2 )
   Hubg2Iner = Hubg1Iner
   IF ( Hubg1Iner < 0.0 )  CALL ProgAbort ( ' HubIner must not be less than HubMass*( UndSling - HubCM )^2 for 2-blader.' )
ELSE                    ! 3-blader
   Hubg1Iner = HubIner
   Hubg2Iner = 0.0
ENDIF

RotMass   = HubMass
RotIner   = Hubg1Iner



   ! Initialize several variables to 0.0:

KBF       = 0.0
KBE       = 0.0
KBFCent   = 0.0
KBECent   = 0.0

TwrMass   = 0.0
KTFA      = 0.0
KTSS      = 0.0
KTFAGrav  = 0.0
KTSSGrav  = 0.0



DO K = 1,p%NumBl          ! Loop through the blades


   ! Initialize BldMass(), FirstMom(), and SecondMom() using TipMass() effects:

   BldMass  (K) = TipMass(K)
   FirstMom (K) = TipMass(K)*BldFlexL
   SecondMom(K) = TipMass(K)*BldFlexL*BldFlexL


   DO J = p%BldNodes,1,-1 ! Loop through the blade nodes / elements in reverse


   ! Calculate the mass of the current element

      ElmntMass    = MassB(K,J)*DRNodes(J)                        ! Mass of blade element J


   ! Integrate to find some blade properties which will be output in .fsm

      BldMass  (K) = BldMass  (K) + ElmntMass
      FirstMom (K) = FirstMom (K) + ElmntMass*RNodes(J)
      SecondMom(K) = SecondMom(K) + ElmntMass*RNodes(J)*RNodes(J)


   ! Integrate to find FMomAbvNd:

      FMomAbvNd   (K,J) = ( 0.5*ElmntMass )*( p%HubRad + RNodes(J  ) + 0.5*DRNodes(J  ) )

      IF ( J == p%BldNodes )  THEN ! Outermost blade element
   ! Add the TipMass() effects:

         FMomAbvNd(K,J) = FmomAbvNd(K,J) + TipMass(K)*p%TipRad
      ELSE                       ! All other blade elements
   ! Add to FMomAbvNd(K,J) the effects from the (not yet used) portion of element J+1

         FMomAbvNd(K,J) = FMomAbvNd(K,J) + FMomAbvNd(K,J+1) &
                        + ( 0.5*ElMassOld )*( p%HubRad + RNodes(J+1) - 0.5*DRNodes(J+1) )
      ENDIF


   ! Store the mass of the current element (this will be used for the next element)

      ElMassOld    = ElmntMass


   ENDDO ! J - Blade nodes / elements in reverse


   ! Calculate BldCG() using FirstMom() and BldMass(); and calculate
   !   RotMass and RotIner:

   BldCG    (K) = FirstMom (K) / BldMass    (K)
   RotMass      = RotMass      + BldMass    (K)
!   RotIner      = RotIner      + ( SecondMom(K) + BldMass  (K)*p%HubRad*( 2.0*BldCG(K) + p%HubRad ) )*( ( COS(p%PreCone(K)) )**2 )
   RotIner      = RotIner      + ( SecondMom(K) + BldMass  (K)*p%HubRad*( 2.0*BldCG(K) + p%HubRad ) )*( p%CosPreC(K)**2 )
ENDDO ! K - Blades



DO K = 1,p%NumBl          ! Loop through the blades


   ! Initialize the generalized blade masses using tip mass effects:

   MBF(K,1,1) = TipMass(K)
   MBF(K,2,2) = TipMass(K)
   MBE(K,1,1) = TipMass(K)


   DO J = 1,p%BldNodes    ! Loop through the blade nodes / elements


   ! Integrate to find the generalized mass of the blade (including tip mass effects).
   !   Ignore the cross-correlation terms of MBF (i.e. MBF(i,j) where i /= j) since
   !   these terms will never be used.

      ElmntMass     = MassB(K,J)*DRNodes(J)                          ! Mass of blade element J

      Shape1 = SHP( RNodesNorm(J), BldFlexL, BldFl1Sh(:,K), 0 )
      Shape2 = SHP( RNodesNorm(J), BldFlexL, BldFl2Sh(:,K), 0 )
      MBF    (K,1,1) = MBF    (K,1,1) + ElmntMass*Shape1*Shape1
      MBF    (K,2,2) = MBF    (K,2,2) + ElmntMass*Shape2*Shape2

      Shape  = SHP( RNodesNorm(J), BldFlexL, BldEdgSh(:,K), 0 )
      MBE    (K,1,1) = MBE    (K,1,1) + ElmntMass*Shape *Shape


   ! Integrate to find the generalized stiffness of the blade (not including centrifugal
   !    effects).

      ElmntStff      = StiffBF(K,J)*DRNodes(J)                       ! Flapwise stiffness of blade element J
      Shape1 = SHP( RNodesNorm(J), BldFlexL, BldFl1Sh(:,K), 2 )
      Shape2 = SHP( RNodesNorm(J), BldFlexL, BldFl2Sh(:,K), 2 )
      KBF    (K,1,1) = KBF    (K,1,1) + ElmntStff*Shape1*Shape1
      KBF    (K,1,2) = KBF    (K,1,2) + ElmntStff*Shape1*Shape2
      KBF    (K,2,1) = KBF    (K,2,1) + ElmntStff*Shape2*Shape1
      KBF    (K,2,2) = KBF    (K,2,2) + ElmntStff*Shape2*Shape2

      ElmntStff      = StiffBE(K,J)*DRNodes(J)                       ! Edgewise stiffness of blade element J
      Shape  = SHP( RNodesNorm(J), BldFlexL, BldEdgSh(:,K), 2 )
      KBE    (K,1,1) = KBE    (K,1,1) + ElmntStff*Shape *Shape


   ! Integrate to find the centrifugal-term of the generalized flapwise and edgewise
   !   stiffness of the blades.  Ignore the cross-correlation terms of KBFCent (i.e.
   !   KBFCent(i,j) where i /= j) since these terms will never be used.

      ElmntStff      = FMomAbvNd(K,J)*DRNodes(J)*RotSpeed*RotSpeed   ! Centrifugal stiffness of blade element J

      Shape1 = SHP( RNodesNorm(J), BldFlexL, BldFl1Sh(:,K), 1 )
      Shape2 = SHP( RNodesNorm(J), BldFlexL, BldFl2Sh(:,K), 1 )
      KBFCent(K,1,1) = KBFCent(K,1,1) + ElmntStff*Shape1*Shape1
      KBFCent(K,2,2) = KBFCent(K,2,2) + ElmntStff*Shape2*Shape2

      Shape  = SHP( RNodesNorm(J), BldFlexL, BldEdgSh(:,K), 1 )
      KBECent(K,1,1) = KBECent(K,1,1) + ElmntStff*Shape *Shape


   ! Calculate the 2nd derivatives of the twisted shape functions:

      Shape  = SHP( RNodesNorm(J), BldFlexL, BldFl1Sh(:,K), 2 )
      TwistedSF(K,1,1,J,2) =  Shape*CThetaS(K,J)                  ! 2nd deriv. of Phi1(J) for blade K
      TwistedSF(K,2,1,J,2) = -Shape*SThetaS(K,J)                  ! 2nd deriv. of Psi1(J) for blade K

      Shape  = SHP( RNodesNorm(J), BldFlexL, BldFl2Sh(:,K), 2 )
      TwistedSF(K,1,2,J,2) =  Shape*CThetaS(K,J)                  ! 2nd deriv. of Phi2(J) for blade K
      TwistedSF(K,2,2,J,2) = -Shape*SThetaS(K,J)                  ! 2nd deriv. of Psi2(J) for blade K

      Shape  = SHP( RNodesNorm(J), BldFlexL, BldEdgSh(:,K), 2 )
      TwistedSF(K,1,3,J,2) =  Shape*SThetaS(K,J)                  ! 2nd deriv. of Phi3(J) for blade K
      TwistedSF(K,2,3,J,2) =  Shape*CThetaS(K,J)                  ! 2nd deriv. of Psi3(J) for blade K


   ! Integrate to find the 1st derivatives of the twisted shape functions:

      DO I = 1,2     ! Loop through Phi and Psi
         DO L = 1,3  ! Loop through all blade DOFs
            TwstdSF     (  I,L,  1) = TwistedSF(K,I,L,J,2)*0.5*DRNodes(J)
            TwistedSF   (K,I,L,J,1) = TwstdSF   ( I,L,  1)
         ENDDO       ! L - All blade DOFs
      ENDDO          ! I - Phi and Psi

      IF ( J /= 1 )  THEN  ! All but the innermost blade element
   ! Add the effects from the (not yet used) portion of element J-1

         DO I = 1,2     ! Loop through Phi and Psi
            DO L = 1,3  ! Loop through all blade DOFs
               TwistedSF(K,I,L,J,1) = TwistedSF(K,I,L,J,1) + TwistedSF(K,I,L,J-1,1) &
                                    + TwstdSFOld( I,L,  1)
            ENDDO       ! L - All blade DOFs
         ENDDO          ! I - Phi and Psi
      ENDIF


   ! Integrate to find the twisted shape functions themselves (i.e., their zeroeth
   !   derivative):

      DO I = 1,2     ! Loop through Phi and Psi
         DO L = 1,3  ! Loop through all blade DOFs
            TwstdSF     (  I,L,  0) = TwistedSF(K,I,L,J,1)*0.5*DRNodes(J)
            TwistedSF   (K,I,L,J,0) = TwstdSF   ( I,L,  0)
         ENDDO       ! L - All blade DOFs
      ENDDO          ! I - Phi and Psi

      IF ( J /= 1 )  THEN  ! All but the innermost blade element
   ! Add the effects from the (not yet used) portion of element J-1

         DO I = 1,2     ! Loop through Phi and Psi
            DO L = 1,3  ! Loop through all blade DOFs
               TwistedSF(K,I,L,J,0) = TwistedSF(K,I,L,J,0) + TwistedSF(K,I,L,J-1,0) &
                                    + TwstdSFOld( I,L,  0)
            ENDDO       ! L - All blade DOFs
         ENDDO          ! I - Phi and Psi
      ENDIF


   ! Integrate to find the blade axial reduction shape functions:

      DO I = 1,3     ! Loop through all blade DOFs
         DO L = 1,3  ! Loop through all blade DOFs
            AxRdBld    (  I,L  ) = 0.5*DRNodes(J)*(                          &
                                   TwistedSF(K,1,I,J,1)*TwistedSF(K,1,L,J,1) &
                                 + TwistedSF(K,2,I,J,1)*TwistedSF(K,2,L,J,1) )
            AxRedBld   (K,I,L,J) = AxRdBld(I,L)
         ENDDO       ! L - All blade DOFs
      ENDDO          ! I - All blade DOFs

      IF ( J /= 1 )  THEN  ! All but the innermost blade element
   ! Add the effects from the (not yet used) portion of element J-1

         DO I = 1,3     ! Loop through all blade DOFs
            DO L = 1,3  ! Loop through all blade DOFs
               AxRedBld(K,I,L,J) = AxRedBld(K,I,L,J) + AxRedBld(K,I,L,J-1)   &
                                 + AxRdBldOld(I,L)
            ENDDO       ! L - All blade DOFs
         ENDDO          ! I - All blade DOFs
      ENDIF


   ! Store the TwstdSF and AxRdBld terms of the current element (these will be used for the next element)

      TwstdSFOld = TwstdSF
      AxRdBldOld = AxRdBld


   ENDDO ! J - Blade nodes / elements


   ! Apply the flapwise modal stiffness tuners of the blades to KBF():

   DO I = 1,2     ! Loop through flap DOFs
      DO L = 1,2  ! Loop through flap DOFs
         KBF(K,I,L) = SQRT( FStTunr(K,I)*FStTunr(K,L) )*KBF(K,I,L)
      ENDDO       ! L - Flap DOFs
   ENDDO          ! I - Flap DOFs


   ! Calculate the blade natural frequencies:


   DO I = 1,2     ! Loop through flap DOFs
      FreqBF(K,I,1) = Inv2Pi*SQRT(   KBF(K,I,I)                   /( MBF(K,I,I) - TipMass(K) ) )   ! Natural blade I-flap frequency w/o centrifugal stiffening nor     tip mass effects
      FreqBF(K,I,2) = Inv2Pi*SQRT(   KBF(K,I,I)                   /  MBF(K,I,I)                )   ! Natural blade I-flap frequency w/o centrifugal stiffening, but w/ tip mass effects
      FreqBF(K,I,3) = Inv2Pi*SQRT( ( KBF(K,I,I) + KBFCent(K,I,I) )/  MBF(K,I,I)                )   ! Natural blade I-flap frequency w/  centrifugal stiffening and     tip mass effects
   ENDDO          ! I - Flap DOFs

   FreqBE   (K,1,1) = Inv2Pi*SQRT(   KBE(K,1,1)                   /( MBE(K,1,1) - TipMass(K) ) )   ! Natural blade 1-edge frequency w/o centrifugal stiffening nor      tip mass effects
   FreqBE   (K,1,2) = Inv2Pi*SQRT(   KBE(K,1,1)                   /  MBE(K,1,1)                )   ! Natural Blade 1-edge frequency w/o  centrifugal stiffening, but w/ tip mass effects
   FreqBE   (K,1,3) = Inv2Pi*SQRT( ( KBE(K,1,1) + KBECent(K,1,1) )/  MBE(K,1,1)                )   ! Natural Blade 1-edge frequency w/  centrifugal stiffening and      tip mass effects


   ! Calculate the generalized damping of the blades:

   DO I = 1,2     ! Loop through flap DOFs
      DO L = 1,2  ! Loop through flap DOFs
         CBF(K,I,L) = ( 0.01*BldFDamp(K,L) )*KBF(K,I,L)/( Pi*FreqBF(K,L,1) )
      ENDDO       ! L - Flap DOFs
   ENDDO          ! I - Flap DOFs

   CBE      (K,1,1) = ( 0.01*BldEDamp(K,1) )*KBE(K,1,1)/( Pi*FreqBE(K,1,1) )


   ! Calculate the 2nd derivatives of the twisted shape functions at the tip:

   Shape  = SHP( 1.0, BldFlexL, BldFl1Sh(:,K), 2 )
   TwistedSF(K,1,1,p%TipNode,2) =  Shape*CThetaS(K,p%BldNodes)        ! 2nd deriv. of Phi1(p%TipNode) for blade K
   TwistedSF(K,2,1,p%TipNode,2) = -Shape*SThetaS(K,p%BldNodes)        ! 2nd deriv. of Psi1(p%TipNode) for blade K

   Shape  = SHP( 1.0, BldFlexL, BldFl2Sh(:,K), 2 )
   TwistedSF(K,1,2,p%TipNode,2) =  Shape*CThetaS(K,p%BldNodes)        ! 2nd deriv. of Phi2(p%TipNode) for blade K
   TwistedSF(K,2,2,p%TipNode,2) = -Shape*SThetaS(K,p%BldNodes)        ! 2nd deriv. of Psi2(p%TipNode) for blade K

   Shape  = SHP( 1.0, BldFlexL, BldEdgSh(:,K), 2 )
   TwistedSF(K,1,3,p%TipNode,2) =  Shape*SThetaS(K,p%BldNodes)        ! 2nd deriv. of Phi3(p%TipNode) for blade K
   TwistedSF(K,2,3,p%TipNode,2) =  Shape*CThetaS(K,p%BldNodes)        ! 2nd deriv. of Psi3(p%TipNode) for blade K


   ! Integrate to find the 1st and zeroeth derivatives of the twisted shape functions
   !   at the tip:

   DO I = 1,2     ! Loop through Phi and Psi
      DO L = 1,3  ! Loop through all blade DOFs
         TwistedSF(K,I,L,p%TipNode,1) = TwistedSF(K,I,L,p%BldNodes,1) + TwstdSFOld(I,L,1)
         TwistedSF(K,I,L,p%TipNode,0) = TwistedSF(K,I,L,p%BldNodes,0) + TwstdSFOld(I,L,0)
      ENDDO       ! L - All blade DOFs
   ENDDO          ! I - Phi and Psi


   ! Integrate to find the blade axial reduction shape functions at the tip:

   DO I = 1,3     ! Loop through all blade DOFs
      DO L = 1,3  ! Loop through all blade DOFs
         AxRedBld(K,I,L,p%TipNode) = AxRedBld(K,I,L,p%BldNodes) + AxRdBldOld(I,L)
      ENDDO       ! L - All blade DOFs
   ENDDO          ! I - All blade DOFs


ENDDO ! K - Blades



   ! Calculate the tower-top mass:

TwrTpMass = RotMass + RFrlMass + BoomMass + TFinMass + NacMass + YawBrMass


DO J = p%TwrNodes,1,-1 ! Loop through the tower nodes / elements in reverse


   ! Calculate the mass of the current element

   ElmntMass    = MassT(J)*DHNodes(J)     ! Mass of tower element J


   ! Integrate to find the tower mass which will be output in .fsm

   TwrMass      = TwrMass + ElmntMass


   ! Integrate to find TMssAbvNd:

   TMssAbvNd   (J) = 0.5*ElmntMass

   IF ( J == p%TwrNodes )  THEN ! Uppermost tower element
   ! Add the TwrTpMass effects:

      TMssAbvNd(J) = TMssAbvNd(J) + TwrTpMass
   ELSE                       ! All other tower elements
   ! Add to TMssAbvNd(J) the effects from the (not yet used) portion of element J+1

      TMssAbvNd(J) = 0.5*ElMassOld + TMssAbvNd(J) + TMssAbvNd(J+1)
   ENDIF


   ! Store the mass of the current element (this will be used for the next element)

   ElMassOld    = ElmntMass


ENDDO ! J - Tower nodes / elements in reverse



   ! Initialize the generalized tower masses using tower-top mass effects:

DO I = 1,2  ! Loop through all tower modes in a single direction
   MTFA(I,I) = TwrTpMass
   MTSS(I,I) = TwrTpMass
ENDDO       ! I - All tower modes in a single direction


DO J = 1,p%TwrNodes    ! Loop through the tower nodes / elements


   ! Calculate the tower shape functions (all derivatives):

   TwrFASF(1,J,2) = SHP( HNodesNorm(J), TwrFlexL, TwFAM1Sh(:), 2 )
   TwrFASF(2,J,2) = SHP( HNodesNorm(J), TwrFlexL, TwFAM2Sh(:), 2 )
   TwrFASF(1,J,1) = SHP( HNodesNorm(J), TwrFlexL, TwFAM1Sh(:), 1 )
   TwrFASF(2,J,1) = SHP( HNodesNorm(J), TwrFlexL, TwFAM2Sh(:), 1 )
   TwrFASF(1,J,0) = SHP( HNodesNorm(J), TwrFlexL, TwFAM1Sh(:), 0 )
   TwrFASF(2,J,0) = SHP( HNodesNorm(J), TwrFlexL, TwFAM2Sh(:), 0 )

   TwrSSSF(1,J,2) = SHP( HNodesNorm(J), TwrFlexL, TwSSM1Sh(:), 2 )
   TwrSSSF(2,J,2) = SHP( HNodesNorm(J), TwrFlexL, TwSSM2Sh(:), 2 )
   TwrSSSF(1,J,1) = SHP( HNodesNorm(J), TwrFlexL, TwSSM1Sh(:), 1 )
   TwrSSSF(2,J,1) = SHP( HNodesNorm(J), TwrFlexL, TwSSM2Sh(:), 1 )
   TwrSSSF(1,J,0) = SHP( HNodesNorm(J), TwrFlexL, TwSSM1Sh(:), 0 )
   TwrSSSF(2,J,0) = SHP( HNodesNorm(J), TwrFlexL, TwSSM2Sh(:), 0 )


   ! Integrate to find the generalized mass of the tower (including tower-top mass effects).
   !   Ignore the cross-correlation terms of MTFA (i.e. MTFA(i,j) where i /= j) and MTSS
   !   since these terms will never be used.

   ElmntMass      = MassT(J)*DHNodes(J)                           ! Mass of tower element J

   DO I = 1,2     ! Loop through all tower DOFs in one direction
      MTFA  (I,I) = MTFA  (I,I) + ElmntMass*TwrFASF(I,J,0)*TwrFASF(I,J,0)
      MTSS  (I,I) = MTSS  (I,I) + ElmntMass*TwrSSSF(I,J,0)*TwrSSSF(I,J,0)
   ENDDO          ! I - through all tower DOFs in one direction


   ! Integrate to find the generalized stiffness of the tower (not including gravitational
   !    effects).

   ElStffFA       = StiffTFA(J)*DHNodes(J)                        ! Fore-aft stiffness of tower element J
   ElStffSS       = StiffTSS(J)*DHNodes(J)                        ! Side-to-side stiffness of tower element J

   DO I = 1,2     ! Loop through all tower DOFs in one direction
      DO L = 1,2  ! Loop through all tower DOFs in one direction
         KTFA (I,L) = KTFA    (I,L) + ElStffFA *TwrFASF(I,J,2)*TwrFASF(L,J,2)
         KTSS (I,L) = KTSS    (I,L) + ElStffSS *TwrSSSF(I,J,2)*TwrSSSF(L,J,2)
      ENDDO       ! L - All tower DOFs in one direction
   ENDDO          ! I - through all tower DOFs in one direction


   ! Integrate to find the gravitational-term of the generalized stiffness of the tower.
   !   Ignore the cross-correlation terms of KTFAGrav (i.e. KTFAGrav(i,j) where i /= j)
   !   and KTSSGrav since these terms will never be used.

   ElmntStff      = -TMssAbvNd(J)*DHNodes(J)*Gravity              ! Gravitational stiffness of tower element J

   DO I = 1,2     ! Loop through all tower DOFs in one direction
      KTFAGrav(I,I) = KTFAGrav(I,I) + ElmntStff*TwrFASF(I,J,1)*TwrFASF(I,J,1)
      KTSSGrav(I,I) = KTSSGrav(I,I) + ElmntStff*TwrSSSF(I,J,1)*TwrSSSF(I,J,1)
   ENDDO


   ! Integrate to find the tower axial reduction shape functions:

   DO I = 1,2     ! Loop through all tower DOFs in one direction
      DO L = 1,2  ! Loop through all tower DOFs in one direction
         AxRdTFA (I,L) = 0.5*DHNodes(J)*TwrFASF(I,J,1)*TwrFASF(L,J,1)
         AxRdTSS (I,L) = 0.5*DHNodes(J)*TwrSSSF(I,J,1)*TwrSSSF(L,J,1)

         AxRedTFA(I,L,J) = AxRdTFA(I,L)
         AxRedTSS(I,L,J) = AxRdTSS(I,L)
      ENDDO       ! L - All tower DOFs in one direction
   ENDDO

   IF ( J /= 1 )  THEN  ! All but the lowermost tower element
   ! Add the effects from the (not yet used) portion of element J-1

      DO I = 1,2     ! Loop through all tower DOFs in one direction
         DO L = 1,2  ! Loop through all tower DOFs in one direction
            AxRedTFA(I,L,J) = AxRedTFA(I,L,J) + AxRedTFA(I,L,J-1)+ AxRdTFAOld(I,L)
            AxRedTSS(I,L,J) = AxRedTSS(I,L,J) + AxRedTSS(I,L,J-1)+ AxRdTSSOld(I,L)
         ENDDO       ! L - All tower DOFs in one direction
      ENDDO
   ENDIF


   ! Store the AxRdTFA and AxRdTSS terms of the current element (these will be used for the next element)

   AxRdTFAOld = AxRdTFA
   AxRdTSSOld = AxRdTSS


ENDDO ! J - Tower nodes / elements


! Apply the modal stiffness tuners of the tower to KTFA() and KTSS():

DO I = 1,2     ! Loop through all tower DOFs in one direction
   DO L = 1,2  ! Loop through all tower DOFs in one direction
      KTFA(I,L) = SQRT( FAStTunr(I)*FAStTunr(L) )*KTFA(I,L)

      KTSS(I,L) = SQRT( SSStTunr(I)*SSStTunr(L) )*KTSS(I,L)
   ENDDO       ! L - All tower DOFs in one direction
ENDDO          ! I - through all tower DOFs in one direction


   ! Calculate the tower natural frequencies:

DO I = 1,2     ! Loop through all tower DOFs in one direction
   FreqTFA(I,1) = Inv2Pi*SQRT(   KTFA(I,I)                  /( MTFA(I,I) - TwrTpMass ) )  ! Natural tower I-fore-aft frequency w/o gravitational destiffening nor tower-top mass effects
   FreqTFA(I,2) = Inv2Pi*SQRT( ( KTFA(I,I) + KTFAGrav(I,I) )/  MTFA(I,I)               )  ! Natural tower I-fore-aft frequency w/  gravitational destiffening and tower-top mass effects
   FreqTSS(I,1) = Inv2Pi*SQRT(   KTSS(I,I)                  /( MTSS(I,I) - TwrTpMass ) )  ! Natural tower I-side-to-side frequency w/o gravitational destiffening nor tower-top mass effects
   FreqTSS(I,2) = Inv2Pi*SQRT( ( KTSS(I,I) + KTSSGrav(I,I) )/  MTSS(I,I)               )  ! Natural tower I-side-to-side frequency w/  gravitational destiffening and tower-top mass effects
ENDDO          ! I - All tower DOFs in one direction


   ! Calculate the generalized damping of the tower:

DO I = 1,2     ! Loop through all tower DOFs in one direction
   DO L = 1,2  ! Loop through all tower DOFs in one direction
      CTFA(I,L) = ( 0.01*TwrFADmp(L) )*KTFA(I,L)/( Pi*FreqTFA(L,1) )

      CTSS(I,L) = ( 0.01*TwrSSDmp(L) )*KTSS(I,L)/( Pi*FreqTSS(L,1) )
   ENDDO       ! L - All tower DOFs in one direction
ENDDO          ! I - All tower DOFs in one direction


   ! Calculate the tower shape functions (all derivatives) at the tower-top:

TwrFASF(1,TTopNode,2) = SHP( 1.0, TwrFlexL, TwFAM1Sh(:), 2 )
TwrFASF(2,TTopNode,2) = SHP( 1.0, TwrFlexL, TwFAM2Sh(:), 2 )
TwrFASF(1,TTopNode,1) = SHP( 1.0, TwrFlexL, TwFAM1Sh(:), 1 )
TwrFASF(2,TTopNode,1) = SHP( 1.0, TwrFlexL, TwFAM2Sh(:), 1 )
TwrFASF(1,TTopNode,0) = SHP( 1.0, TwrFlexL, TwFAM1Sh(:), 0 )
TwrFASF(2,TTopNode,0) = SHP( 1.0, TwrFlexL, TwFAM2Sh(:), 0 )

TwrSSSF(1,TTopNode,2) = SHP( 1.0, TwrFlexL, TwSSM1Sh(:), 2 )
TwrSSSF(2,TTopNode,2) = SHP( 1.0, TwrFlexL, TwSSM2Sh(:), 2 )
TwrSSSF(1,TTopNode,1) = SHP( 1.0, TwrFlexL, TwSSM1Sh(:), 1 )
TwrSSSF(2,TTopNode,1) = SHP( 1.0, TwrFlexL, TwSSM2Sh(:), 1 )
TwrSSSF(1,TTopNode,0) = SHP( 1.0, TwrFlexL, TwSSM1Sh(:), 0 )
TwrSSSF(2,TTopNode,0) = SHP( 1.0, TwrFlexL, TwSSM2Sh(:), 0 )


   ! Integrate to find the tower axial reduction shape functions at the tower-top:

DO I = 1,2     ! Loop through all tower DOFs in one direction
   DO L = 1,2  ! Loop through all tower DOFs in one direction
      AxRedTFA(I,L,TTopNode) = AxRedTFA(I,L,p%TwrNodes)+ AxRdTFAOld(I,L)
      AxRedTSS(I,L,TTopNode) = AxRedTSS(I,L,p%TwrNodes)+ AxRdTSSOld(I,L)
   ENDDO       ! L - All tower DOFs in one direction
ENDDO


   ! Calculate the turbine and total masses:

TurbMass  = TwrTpMass + TwrMass
TotalMass = TurbMass + PtfmMass

   ! deallocate local variables:

IF (ALLOCATED( FMomAbvNd )) DEALLOCATE ( FMomAbvNd )
IF (ALLOCATED( KBECent   )) DEALLOCATE ( KBECent   )
IF (ALLOCATED( KBFCent   )) DEALLOCATE ( KBFCent   )
IF (ALLOCATED( MBE       )) DEALLOCATE ( MBE       )
IF (ALLOCATED( MBF       )) DEALLOCATE ( MBF       )
IF (ALLOCATED( TMssAbvNd )) DEALLOCATE ( TMssAbvNd )


RETURN
END SUBROUTINE Coeff
!=======================================================================
SUBROUTINE Control( p, x, OtherState, b1 )


   ! This is the main control routine.


USE                             DriveTrain
USE                             Features
USE                             General
USE                             InitCond
USE                             NacelleYaw
USE                             RtHndSid
USE                             SimCont
USE                             TurbCont


USE                             AeroDyn

IMPLICIT                        NONE

   ! Passed variables:
TYPE(StrD_ParameterType),      INTENT(IN)    :: p                               ! Parameters of the structural dynamics module
TYPE(StrD_ContinuousStateType),INTENT(INOUT) :: x                               ! The structural dynamics module's continuous states
TYPE(StrD_OtherStateType),     INTENT(INOUT) :: OtherState                      ! Other State data type for Structural dynamics module
REAL(ReKi)                                   :: b1(3)                           ! Vector / direction b1 (=  xp from the IEC coord. system)

   ! Local variables:

REAL(ReKi)                   :: HHWndVec  (3)                                   ! Hub-height wind vector in the AeroDyn coordinate system, m/s.
REAL(ReKi)                   :: LinAccEO  (3)                                   ! Total linear acceleration of the base plate (point O) in the inertia frame (body E for earth).
REAL(ReKi), SAVE             :: NacYawFrct                                      ! Nacelle yaw angle fractions used for the override yaw maneuver calculation.
REAL(ReKi), SAVE             :: NacYawI                                         ! Initial yaw angle at the start of the override yaw maneuver.
REAL(ReKi)                   :: TwrAccel                                        ! Tower Acceleration.  Used for tower feedback control.
REAL(ReKi)                   :: WindDir                                         ! Horizontal hub-height wind direction (positive about the zi-axis), rad.
REAL(ReKi)                   :: YawError                                        ! Nacelle yaw error (positve about the zi-axis), rad.

INTEGER(4)                   :: I                                               ! Generic index
INTEGER(4)                   :: K                                               ! Loops through blades.

INTEGER                      :: ErrStat


LOGICAL,    SAVE             :: BegYawMan = .TRUE.                              ! .TRUE. before the override yaw manuever has begun (begin yaw manuever).




   ! ------------------------------ YAW CONTROL -------------------------------
   ! Control yaw if requested:

IF ( ZTime >= TYCOn )  THEN   ! Time now to enable active yaw control.


   SELECT CASE ( YCMode )  ! Which yaw control mode are we using?

   CASE ( 0 )              ! None!


   ! Use the initial yaw angle and rate:

      IF ( p%DOF_Flag(DOF_Yaw) )  THEN   ! Yaw DOF is currently enabled (use FAST's built-in actuator initial conditions).

         YawPosCom  = YawNeut
         YawRateCom = YawRateNeut

      ELSE                             ! Yaw DOF is currently disabled (no built-in actuator) (use FAST's initial yaw conditions).

         YawPosCom  = NacYaw
         YawRateCom = 0.0

      ENDIF


   CASE ( 1 )              ! User-defined from routine UserYawCont().


   ! Calculate horizontal hub-height wind direction and the nacelle yaw error
   !   estimate (both positive about zi-axis); these are zero if there is no
   !   wind input when AeroDyn is not used:

      IF ( CompAero )  THEN   ! AeroDyn has been used.

         HHWndVec(:) = AD_GetUndisturbedWind( ZTime, (/ REAL(0.0, ReKi), REAL(0.0, ReKi), p%FASTHH /), ErrStat )

         WindDir  = ATAN2( HHWndVec(2), HHWndVec(1) )
         YawError = WindDir - x%QT(DOF_Yaw) - x%QT(DOF_Y)

      ELSE                    ! No AeroDynamics.

         WindDir  = 0.0
         YawError = 0.0

      ENDIF


   ! Call the user-defined yaw control routine:

      CALL UserYawCont ( x%QT(DOF_Yaw), x%QDT(DOF_Yaw), WindDir, YawError, p%NumBl, ZTime, DT, DirRoot, YawPosCom, YawRateCom )


   CASE ( 2 )              ! User-defined from Simulink or Labview.


   ! Do nothing here since yaw angle and yaw rate are defined externally from Simulink or Labview.


   ENDSELECT


ELSE                          ! Do not control yaw yet, maintain initial yaw angles.


   ! Use the initial yaw angle and rate:

   IF ( p%DOF_Flag(DOF_Yaw) )  THEN   ! Yaw DOF is currently enabled (use FAST's built-in actuator initial conditions).

      YawPosCom  = YawNeut
      YawRateCom = YawRateNeut

   ELSE                             ! Yaw DOF is currently disabled (no built-in actuator) (use FAST's initial yaw conditions).

      YawPosCom  = NacYaw
      YawRateCom = 0.0

   ENDIF


ENDIF


   ! Override standard yaw control with a linear maneuver if necessary:

IF ( ZTime >= TYawManE )  THEN      ! Override yaw maneuver has ended, yaw is locked at NacYawF.


   YawPosCom     = NacYawF
   YawRateCom    = 0.0


ELSEIF ( ZTime >= TYawManS )  THEN  ! Override yaw maneuver is occuring.


   IF ( BegYawMan )  THEN  ! Override yaw maneuver is just beginning.

      NacYawI    = x%QT(DOF_Yaw)                               ! Store the initial (current) yaw, at the start of the yaw maneuver.

      NacYawFrct = ( NacYawF  - NacYawI  ) / &                 ! Calculate the yaw rate (fraction) that will occur during the maneuver.
                   ( TYawManE - TYawManS )


      BegYawMan  = .FALSE.                                     ! Don't enter this part of the IF-structure again

   ENDIF


   YawPosCom     = NacYawI + NacYawFrct*( ZTime - TYawManS )   ! Increment the command yaw
   YawRateCom    = NacYawFrct                                  !   and rate using NacYawFrct


ENDIF


   ! If the yaw DOF is enabled, the command yaw angle and rate become the
   !   neutral yaw angle and rate in FAST's built-in second-order actuator
   !   model defined by inputs YawSpr and YawDamp.  If the yaw DOF is disabled
   !   (no yaw DOF), then the command yaw angle and rate become the actual yaw
   !   angle and rate (no built-in actuator) and the yaw acceleration will be
   !   zero.
   ! NOTE: I don't want to test the value of YawDOF here, since the value of
   !       DOF_Flag(DOF_Yaw) can be controlled by the user-defined routine:

IF ( p%DOF_Flag(DOF_Yaw) )  THEN   ! Yaw DOF is currently enabled (use FAST's built-in actuator).

   YawNeut              = YawPosCom
   YawRateNeut          = YawRateCom

ELSE                             ! Yaw DOF is currently disabled (no built-in actuator).

   OtherState%Q  (DOF_Yaw,IC(NMX)) = YawPosCom    ! Update the saved values
   OtherState%QD (DOF_Yaw,IC(NMX)) = YawRateCom   !   used in routine Solver()
   x%QT          (DOF_Yaw)         = YawPosCom    ! Update the current, intermediate
   x%QDT         (DOF_Yaw)         = YawRateCom   !    values used in routine RtHS()

ENDIF



   ! ----------------------------- PITCH CONTROL ------------------------------
   ! Control pitch if requested:

IF ( ZTime >= TPCOn )  THEN   ! Time now to enable active pitch control.


   SELECT CASE ( PCMode )  ! Which pitch control mode are we using?

   CASE ( 0 )              ! None!


   ! Use the initial blade pitch angles:

      BlPitchCom = BlPitchInit


   CASE ( 1 )              ! User-defined from routine PitchCntrl().


   ! Calculate tower-top acceleration (fore-aft mode only) in the tower-top
   !   system:

      LinAccEO = LinAccEOt
      DO I = 1,OtherState%DOFs%NPTE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the yaw bearing center of mass (point O)
         LinAccEO = LinAccEO + PLinVelEO(OtherState%DOFs%PTE(I),0,:)*QD2T(OtherState%DOFs%PTE(I))
      ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the yaw bearing center of mass (point O)

      TwrAccel = DOT_PRODUCT( LinAccEO, b1 )


   ! Call the user-defined pitch control routine:

      CALL PitchCntrl ( BlPitch, ElecPwr, GBRatio*x%QDT(DOF_GeAz), GBRatio, TwrAccel, p%NumBl, ZTime, DT, DirRoot, BlPitchCom )


   CASE ( 2 )              ! User-defined from Simulink or Labview.


   ! Do nothing here since blade pitch is defined externally from Simulink or Labview.


   ENDSELECT


ELSE                          ! Do not control pitch yet, maintain initial pitch angles.


   ! Use the initial blade pitch angles:

   BlPitchCom = BlPitchInit


ENDIF


   ! Override standard pitch control with a linear maneuver if necessary:

DO K = 1,p%NumBl ! Loop through all blades


   IF ( ZTime >= TPitManE(K) )  THEN      ! Override pitch maneuver has ended, blade is locked at BlPitchF.


      BlPitchCom    (K) = BlPitchF(K)


   ELSEIF ( ZTime >= TPitManS(K) )  THEN  ! Override pitch maneuver is occuring for this blade.


      IF ( BegPitMan(K) )  THEN  ! Override pitch maneuver is just beginning.

         BlPitchI   (K) = BlPitch(K)                                             ! Store the initial (current) pitch, at the start of the pitch maneuver.

         BlPitchFrct(K) = ( BlPitchF(K) - BlPitchI(K) ) / &                      ! Calculate the pitch rate (fraction) that will occur during the maneuver.
                          ( TPitManE(K) - TPitManS(K) )


         BegPitMan  (K) = .FALSE.                                                ! Don't enter this part of the IF-structure again

      ENDIF


      BlPitchCom    (K) = BlPitchI(K) + BlPitchFrct(K)*( ZTime - TPitManS(K) )   ! Increment the blade pitch using BlPitchFrct

!bjj: check that BlPitchI and BlPitchFrct are explicitly initialized...
   ENDIF


ENDDO ! K - blades


   ! Set the command pitch angles to the actual pitch angles since we have no
   !   built-in pitch actuator:

BlPitch = BlPitchCom



RETURN
END SUBROUTINE Control
!=======================================================================
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
!=======================================================================
SUBROUTINE CoordSys_Dealloc( CoordSys, ErrStat, ErrMsg )

   ! This subroutine deallocates the coordinate systems in the StrD_CoordSys type.

IMPLICIT NONE

   ! passed arguments

TYPE(StrD_CoordSys),      INTENT(INOUT) :: CoordSys       ! The coordinate systems to be deallocated

INTEGER(IntKi),           INTENT(OUT)   :: ErrStat        ! Error status
CHARACTER(*),             INTENT(OUT)   :: ErrMsg         ! Err msg


   ! local variables

CHARACTER(200), PARAMETER          :: ErrTxt = 'coordinate system arrays in SUBROUTINE CoordSys_Dealloc.'


   ! Initialize ErrStat and ErrMsg

ErrStat = ErrID_None
ErrMsg  = ''


  ! Deallocate coordinate system arrays:

IF ( ALLOCATED(CoordSys%i1) ) THEN  ! We'll assume that if one is allocated, all the arrays are allocated

   DEALLOCATE ( CoordSys%i1, CoordSys%i2, CoordSys%i3, STAT=ErrStat ) !this argument doesn't work in IVF 10.1: , ERRMSG=ErrMsg
   IF ( ErrStat /= 0 )  THEN
      ErrStat = ErrID_Info
      ErrMsg  = 'Error deallocating the i1, i2, and i3 '//TRIM(ErrTxt)//' '//TRIM(ErrMsg)
   END IF

   DEALLOCATE ( CoordSys%j1, CoordSys%j2, CoordSys%j3, STAT=ErrStat ) !this argument doesn't work in IVF 10.1: , ERRMSG=ErrMsg
   IF ( ErrStat /= 0 )  THEN
      ErrStat = ErrID_Info
      ErrMsg  = 'Error deallocating the j1, j2, and j3 '//TRIM(ErrTxt)//' '//TRIM(ErrMsg)
   END IF


   DEALLOCATE ( CoordSys%m1, CoordSys%m2, CoordSys%m3, STAT=ErrStat ) !this argument doesn't work in IVF 10.1: , ERRMSG=ErrMsg
   IF ( ErrStat /= 0 )  THEN
      ErrStat = ErrID_Info
      ErrMsg  = 'Error deallocating the m1, m2, and m3 '//TRIM(ErrTxt)//' '//TRIM(ErrMsg)
   END IF


   DEALLOCATE ( CoordSys%n1, CoordSys%n2, CoordSys%n3, STAT=ErrStat ) !this argument doesn't work in IVF 10.1: , ERRMSG=ErrMsg
   IF ( ErrStat /= 0 )  THEN
      ErrStat = ErrID_Info
      ErrMsg  = 'Error deallocating the n1, n2, and n3 '//TRIM(ErrTxt)//' '//TRIM(ErrMsg)
   END IF


   DEALLOCATE ( CoordSys%t1, CoordSys%t2, CoordSys%t3, STAT=ErrStat ) !this argument doesn't work in IVF 10.1: , ERRMSG=ErrMsg
   IF ( ErrStat /= 0 )  THEN
      ErrStat = ErrID_Info
      ErrMsg  = 'Error deallocating the t1, t2, and t3 '//TRIM(ErrTxt)//' '//TRIM(ErrMsg)
   END IF


   DEALLOCATE ( CoordSys%te1, CoordSys%te2, CoordSys%te3, STAT=ErrStat ) !this argument doesn't work in IVF 10.1: , ERRMSG=ErrMsg
   IF ( ErrStat /= 0 )  THEN
      ErrStat = ErrID_Info
      ErrMsg  = 'Error deallocating the te1, te2, and te3 '//TRIM(ErrTxt)//' '//TRIM(ErrMsg)
   END IF

END IF



RETURN
END SUBROUTINE CoordSys_Dealloc
!=======================================================================
SUBROUTINE DrvTrTrq ( p_StrD, LSS_Spd, GBoxTrq )


   ! This routine calculates the drive-train torque.


USE                             DriveTrain
USE                             General
USE                             Linear
USE                             SimCont
USE                             TurbCont

IMPLICIT                        NONE


   ! Passed variables:
TYPE(StrD_ParameterType),INTENT(IN) :: p_StrD                                    ! Parameters of the structural dynamics module
REAL(ReKi), INTENT(OUT)      :: GBoxTrq                                         ! Gearbox torque on the LSS side in N-m (output).
REAL(ReKi), INTENT(IN )      :: LSS_Spd                                         ! LSS speed in rad/sec (input).


   ! Local variables:

COMPLEX(ReKi)                :: Current1                                        ! Current passing through the stator (amps)
COMPLEX(ReKi)                :: Current2                                        ! Current passing through the rotor (amps)
COMPLEX(ReKi)                :: Currentm                                        ! Magnitizing current (amps)

REAL(ReKi)                   :: ComDenom                                        ! Common denominator of variables used in the TEC model
REAL(ReKi)                   :: HSS_Spd                                         ! HSS speed in rad/sec.
REAL(ReKi)                   :: PwrLossS                                        ! Power loss in the stator (watts)
REAL(ReKi)                   :: PwrLossR                                        ! Power loss in the rotor (watts)
REAL(ReKi)                   :: PwrMech                                         ! Mechanical power (watts)
REAL(ReKi)                   :: Slip                                            ! Generator slip.
REAL(ReKi)                   :: SlipRat                                         ! Generator slip ratio.

LOGICAL,    SAVE             :: GenOnLin = .FALSE.                              ! Is the generator online?
LOGICAL,    SAVE             :: Off4Good = .FALSE.                              ! Is the generator offline for good?


   ! Calculate the generator speed.

HSS_Spd = GBRatio*LSS_Spd


   ! See if the generator is on line.

IF ( .NOT. Off4Good )  THEN

   ! The generator is either on-line or has never been turned online.

   IF ( GenOnLin )  THEN   ! The generator is on-line.

      IF ( ( GenTiStp ) .AND. ( ZTime >= TimGenOf ) )  THEN   ! Shut-down of generator determined by time, TimGenOf
         GenOnLin = .FALSE.
         Off4Good = .TRUE.
      ENDIF

   ELSE                    ! The generator has never been turned online.

      IF ( GenTiStr )  THEN   ! Start-up of generator determined by time, TimGenOn
         IF ( ZTime >= TimGenOn )    GenOnLin = .TRUE.
      ELSE                    ! Start-up of generator determined by HSS speed, SpdGenOn
         IF ( HSS_Spd >= SpdGenOn )  GenOnLin = .TRUE.
      ENDIF

   ENDIF

ENDIF


IF ( GenOnLin )  THEN                     ! Generator is on line.


   ! Are we doing simple variable-speed control, or using a generator model?

   SELECT CASE ( VSContrl )               ! Are we using variable-speed control?

   CASE ( 0 )                             ! No variable-speed control.  Using a generator model.


      SELECT CASE ( GenModel )            ! Which generator model are we using?

      CASE ( 1 )                          ! Simple induction-generator model.


         Slip = HSS_Spd - SIG_SySp

         IF ( ABS( Slip ) > SIG_POSl  )  THEN
            GenTrq  = SIGN( SIG_POTq, Slip )
         ELSE
            GenTrq  = Slip*SIG_Slop
         ENDIF
         GenTrq     = GenTrq + DelGenTrq  ! Add the pertubation on generator torque, DelGenTrq.  This is used only for FAST linearization (it is zero otherwise).


   ! The generator efficiency is either additive for motoring,
   !   or subtractive for generating power.

         IF ( GenTrq > 0.0 )  THEN
            ElecPwr = GenTrq*HSS_Spd*GenEff
         ELSE
            ElecPwr = GenTrq*HSS_Spd/GenEff
         ENDIF


      CASE ( 2 )                          ! Thevenin-equivalent generator model.


         SlipRat  = ( HSS_Spd - TEC_SySp )/TEC_SySp

         GenTrq   = TEC_A0*TEC_VLL*TEC_VLL*SlipRat &
                    /( TEC_C0 + TEC_C1*SlipRat + TEC_C2*SlipRat*SlipRat )
         GenTrq   = GenTrq + DelGenTrq ! Add the pertubation on generator torque, DelGenTrq.  This is used only for FAST linearization (it is zero otherwise).

         ComDenom = ( TEC_Re1 - TEC_RRes/SlipRat )**2 + ( TEC_Xe1 + TEC_RLR )**2
         Current2 = CMPLX(  TEC_V1a*( TEC_Re1 - TEC_RRes/SlipRat )/ComDenom , &
                           -TEC_V1a*( TEC_Xe1 + TEC_RLR          )/ComDenom     )
         Currentm = CMPLX( 0.0 , -TEC_V1a/TEC_MR )
         Current1 = Current2 + Currentm
         PwrLossS = 3.0*( ( ABS( Current1 ) )**2 )*TEC_SRes
         PwrLossR = 3.0*( ( ABS( Current2 ) )**2 )*TEC_RRes
         PwrMech  = GenTrq*HSS_Spd
         ElecPwr  = PwrMech - PwrLossS - PwrLossR


      CASE ( 3 )                          ! User-defined generator model.


         CALL UserGen ( HSS_Spd, GBRatio, p_StrD%NumBl, ZTime, DT, GenEff, DelGenTrq, DirRoot, GenTrq, ElecPwr )


      ENDSELECT


   CASE ( 1 )                             ! Simple variable-speed control.


   ! Compute the generator torque, which depends on which region we are in:

      IF ( HSS_Spd >= VS_RtGnSp )  THEN      ! We are in region 3 - torque is constant
         GenTrq = VS_RtTq
      ELSEIF ( HSS_Spd < VS_TrGnSp )  THEN   ! We are in region 2 - torque is proportional to the square of the generator speed
         GenTrq = VS_Rgn2K*HSS_Spd*HSS_Spd
      ELSE                                   ! We are in region 2 1/2 - simple induction generator transition region
         GenTrq = VS_Slope*( HSS_Spd - VS_SySp )
      ENDIF

      GenTrq  = GenTrq + DelGenTrq  ! Add the pertubation on generator torque, DelGenTrq.  This is used only for FAST linearization (it is zero otherwise).


   ! It's not possible to motor using this control scheme,
   !   so the generator efficiency is always subtractive.

      ElecPwr = GenTrq*HSS_Spd*GenEff


   CASE ( 2 )                             ! User-defined variable-speed control for routine UserVSCont().


      CALL UserVSCont ( HSS_Spd, GBRatio, p_StrD%NumBl, ZTime, DT, GenEff, DelGenTrq, DirRoot, GenTrq, ElecPwr )


   CASE ( 3 )                             ! User-defined variable-speed control from Simulink or Labview.


   ! No need to define GenTrq or ElecPwr here since this is defined externally
   !   by Simulink or Labview.  Also, no reason to perturb generator torque here either,
   !   since linearization does not work with Simulink or Labview.


   CASE ( 9999 )                          ! Overridden generator torque caused by trimming generator torque during a FAST linearization analysis (TrimCase == 2)


   ! The generator torque during the trim analysis is computed in SUBROUTINE
   !   FAST_Lin.f90/CalcSteady() and the generator torque pertubation is
   !   computed in FAST_Lin.f90/Linearize(); thus, there is no reason to define
   !   the generator torque here.


   ! The generator efficiency is either additive for motoring,
   !   or subtractive for generating power.

      IF ( GenTrq > 0.0 )  THEN
         ElecPwr = GenTrq*HSS_Spd*GenEff
      ELSE
         ElecPwr = GenTrq*HSS_Spd/GenEff
      ENDIF


   ENDSELECT


   ! Lets turn the generator offline for good if ( GenTiStp = .FALSE. )
   !   .AND. ( ElecPwr <= 0.0 ):

   IF ( ( .NOT. GenTiStp ) .AND. ( ElecPwr <= 0.0 ) ) THEN   ! Shut-down of generator determined by generator power = 0
      GenTrq   = 0.0
      ElecPwr  = 0.0

      GenOnLin = .FALSE.
      Off4Good = .TRUE.
   ENDIF

ELSE                                     ! Generator is off line.

   GenTrq  = 0.0
   ElecPwr = 0.0

ENDIF



   ! Calculate the fraction of applied HSS-brake torque, HSSBrFrac:

IF ( ZTime < THSSBrDp )  THEN    ! HSS brake not deployed yet.


   HSSBrFrac = 0.0


ELSE                             ! HSS brake deployed.


   SELECT CASE ( HSSBrMode )                 ! Which HSS brake model are we using?

   CASE ( 1 )                                ! Simple built-in HSS brake model with linear ramp.

      IF ( ZTime < THSSBrFl )  THEN ! Linear ramp
         HSSBrFrac = ( ZTime - THSSBrDp )/HSSBrDT
      ELSE                          ! Full braking torque
         HSSBrFrac = 1.0
      ENDIF

   CASE ( 2 )                                ! User-defined HSS brake model.

      CALL UserHSSBr ( GenTrq, ElecPwr, HSS_Spd, GBRatio, p_StrD%NumBl, ZTime, DT, DirRoot, HSSBrFrac )

      IF ( ( HSSBrFrac < 0.0 ) .OR. ( HSSBrFrac > 1.0 ) )  &   ! 0 (off) <= HSSBrFrac <= 1 (full); else Abort.
         CALL ProgAbort ( ' HSSBrFrac must be between 0.0 (off) and 1.0 (full) (inclusive).  Fix logic in routine UserHSSBr().' )

   CASE ( 3 )                                ! HSS brake model from Labview.

   ! No need to define HSSBrFrac here because this is defined externally by Labview.


   ENDSELECT


ENDIF


   ! Calculate the magnitude of HSS brake torque:

HSSBrTrq = SIGN( HSSBrFrac*HSSBrTqF, HSS_Spd )  ! Scale the full braking torque by the brake torque fraction and make sure the brake torque resists motion.


   ! Make a copy of the current value of HSSBrTrq for future use:

HSSBrTrqC = HSSBrTrq



   ! Add the gearbox losses to total HSS torque and project to the LSS side of
   !   the gearbox.  The gearbox efficiency effects, however, are included in
   !   FAST.f90/RtHS().

GBoxTrq = ( GenTrq + HSSBrTrq )*GBRatio



RETURN
END SUBROUTINE DrvTrTrq
!=======================================================================
SUBROUTINE FixHSSBrTq ( Integrator, p, OtherState )


   ! This routine is used to adjust the HSSBrTrq value if the absolute
   !   magnitudue of the HSS brake torque was strong enough to reverse
   !   the direction of the HSS, which is a physically impossible
   !   situation.  The problem arises since we are integrating in
   !   discrete time, not continuous time.


   ! AeroDyn MODULES:

USE                             Switch


   ! FAST MODULES:

USE                             DriveTrain
USE                             RtHndSid
USE                             SimCont


IMPLICIT                        NONE


   ! Passed variables:

CHARACTER(9), INTENT(IN )    :: Integrator                                      ! A string holding the current integrator being used.
TYPE(StrD_ParameterType),  INTENT(IN)   :: p                                    ! The parameters of the structural dynamics module
TYPE(StrD_OtherStateType), INTENT(INOUT):: OtherState                           ! Other State data type for Structural dynamics module


   ! Local variables:

REAL(ReKi)                   :: RqdFrcGeAz                                      ! The force term required to produce RqdQD2GeAz.
REAL(ReKi)                   :: RqdQD2GeAz                                      ! The required QD2T(DOF_GeAz) to cause the HSS to stop rotating.

INTEGER(4)                   :: I                                               ! Loops through all DOFs.



   ! Make a copy of the current value of QD2T for future use:

QD2TC = QD2T


   ! The absolute magnitude of the HSS brake must have been too great
   !   that the HSS direction was reversed.  What should have happened
   !   is that the HSS should have stopped rotating.  In other words,
   !   QD(DOF_GeAz,IC(NMX)) should equal zero!  Determining what
   !   QD2T(DOF_GeAz) will make QD(DOF_GeAz,IC(NMX)) = 0, depends on
   !   which integrator we are using.

SELECT CASE (Integrator)

CASE ('Corrector')

   ! Find the required QD2T(DOF_GeAz) to cause the HSS to stop rotating (RqdQD2GeAz).
   ! This is found by solving the corrector formula for QD2(DOF_GeAz,IC(NMX))
   !   when QD(DOF_GeAz,IC(NMX)) equals zero.

   RqdQD2GeAz = ( -      OtherState%QD (DOF_GeAz,IC(1))/DT24 - 19.0*OtherState%QD2(DOF_GeAz,IC(1)) &
                  +  5.0*OtherState%QD2(DOF_GeAz,IC(2))      -      OtherState%QD2(DOF_GeAz,IC(3))   )/ 9.0

CASE ('Predictor')

   ! Find the required QD2T(DOF_GeAz) to cause the HSS to stop rotating (RqdQD2GeAz).
   ! This is found by solving the predictor formula for QD2(DOF_GeAz,IC(1))
   !   when QD(DOF_GeAz,IC(NMX)) equals zero.

   RqdQD2GeAz = ( -      OtherState%QD (DOF_GeAz,IC(1))/DT24 + 59.0*OtherState%QD2(DOF_GeAz,IC(2)) &
                  - 37.0*OtherState%QD2(DOF_GeAz,IC(3))      +  9.0*OtherState%QD2(DOF_GeAz,IC(4))   )/55.0

END SELECT


   ! Rearrange the augmented matrix of equations of motion to account
   !   for the known acceleration of the generator azimuth DOF.  To
   !   do this, make the known inertia like an applied force to the
   !   system.  Then set force QD2T(DOF_GeAz) to equal the known
   !   acceleration in the augmented matrix of equations of motion:
   ! Here is how the new equations are derived.  First partition the
   !   augmented matrix as follows, where Qa are the unknown
   !   accelerations, Qb are the known accelerations, Fa are the
   !   known forces, and Fb are the unknown forces:
   !      [Caa Cab]{Qa}={Fa}
   !      [Cba Cbb]{Qb}={Fb}
   !   By rearranging, the equations for the unknown and known
   !   accelerations are as follows:
   !      [Caa]{Qa}={Fa}-[Cab]{Qb} and [I]{Qb}={Qb}
   !   Combining these two sets of equations into one set yields:
   !      [Caa 0]{Qa}={{Fa}-[Cab]{Qb}}
   !      [  0 I]{Qb}={          {Qb}}
   !   Once this equation is solved, the unknown force can be found from:
   !      {Fb}=[Cba]{Qa}+[Cbb]{Qb}

DO I = 1,OtherState%DOFs%NActvDOF ! Loop through all active (enabled) DOFs

   AugMat(OtherState%DOFs%SrtPS(I),    p%NAug) = AugMat(OtherState%DOFs%SrtPS(I),p%NAug) - AugMat(OtherState%DOFs%SrtPS(I),DOF_GeAz)*RqdQD2GeAz  ! {{Fa}-[Cab]{Qb}}
   AugMat(OtherState%DOFs%SrtPS(I),DOF_GeAz) = 0.0                                                           ! [0]
   AugMat(DOF_GeAz,OtherState%DOFs%SrtPS(I)) = 0.0                                                           ! [0]

ENDDO             ! I - All active (enabled) DOFs

   AugMat(DOF_GeAz,DOF_GeAz) = 1.0                                                           ! [I]{Qb}={Qb}
   AugMat(DOF_GeAz,    p%NAug) = RqdQD2GeAz                                                    !


   ! Invert the matrix to solve for the new (updated) accelerations.  Like in
   !   RtHS(), the accelerations are returned by Gauss() in the first NActvDOF
   !   elements of the solution vector, SolnVec().  These are transfered to the
   !   proper index locations of the acceleration vector QD2T() using the
   !   vector subscript array SrtPS(), after Gauss() has been called:
   ! NOTE: QD2T( SrtPS(1:NActvDOF) ) cannot be sent directly because arrays
   !   sections with vector subscripts must not be used in INTENT(OUT)
   !   arguments.

CALL Gauss( AugMat( OtherState%DOFs%SrtPS    (1: OtherState%DOFs%NActvDOF  )   ,         &
                    OtherState%DOFs%SrtPSNAUG(1:(OtherState%DOFs%NActvDOF+1)) ),         &
                                                 OtherState%DOFs%NActvDOF,       SolnVec )

QD2T = 0.0
DO I = 1,OtherState%DOFs%NActvDOF ! Loop through all active (enabled) DOFs
   QD2T(OtherState%DOFs%SrtPS(I)) = SolnVec(I)
ENDDO             ! I - All active (enabled) DOFs


   ! Find the force required to produce RqdQD2GeAz from the equations of
   !   motion using the new accelerations:

RqdFrcGeAz = 0.0
DO I = 1,OtherState%DOFs%NActvDOF ! Loop through all active (enabled) DOFs
   RqdFrcGeAz = RqdFrcGeAz + OgnlGeAzRo(OtherState%DOFs%SrtPS(I))*QD2T(OtherState%DOFs%SrtPS(I))  ! {Fb}=[Cba]{Qa}+[Cbb]{Qb}
ENDDO             ! I - All active (enabled) DOFs


   ! Find the HSSBrTrq necessary to bring about this force:

HSSBrTrq = HSSBrTrqC + ( ( OgnlGeAzRo(p%NAug) - RqdFrcGeAz )*GBoxEffFac/GBRatio )


   ! Make sure this new HSSBrTrq isn't larger in absolute magnitude than
   !   the original HSSBrTrq.  Indeed, the new HSSBrTrq can't be larger than
   !   the old HSSBrTrq, since the old HSSBrTrq was found solely as a
   !   function of time--and is thus the maximum possible at the current
   !   time.  If the new HSSBrTrq is larger, then the reversal in direction
   !   was caused by factors other than the HSS brake--thus the original HSS
   !   brake torque values were OK to begin with.  Thus, restore the
   !   variables changed by this subroutine, back to their original values:

IF ( ABS( HSSBrTrq ) > ABS( HSSBrTrqC ) )  THEN

   HSSBrTrq = HSSBrTrqC
   QD2T     = QD2TC

ELSE


   ! Use the new accelerations to update the DOF values.  Again, this
   !   depends on the integrator type:

   SELECT CASE (Integrator)

   CASE ('Corrector')

   ! Update QD and QD2 with the new accelerations using the corrector.
   ! This will make QD(DOF_GeAz,IC(NMX)) equal to zero and adjust all
   !    of the other QDs as necessary.
   ! The Q's are unnaffected by this change.

      OtherState%QD2(:,IC(NMX)) = QD2T

      DO I = 1,p%NDOF  ! Loop through all DOFs
         OtherState%QD(I,IC(NMX)) = OtherState%QD(I,IC(1)) + DT24*( 9.0*OtherState%QD2(I,IC(NMX)) &
                                                                 + 19.0*OtherState%QD2(I,IC(1  )) &
                                                                 -  5.0*OtherState%QD2(I,IC(2  )) &
                                                                 +      OtherState%QD2(I,IC(3  )) )
      ENDDO          ! I - All DOFs

   CASE ('Predictor')

   ! Update QD2 with the new accelerations.  Use IC(1) instead of IC(NMX)
   !   since the IC array has already been incremented.
   ! This will make QD(DOF_GeAz,IC(NMX)) equal to zero and adjust all
   !    of the other QDs as necessary during the next time step.

      OtherState%QD2(:,IC(  1)) = QD2T

   END SELECT


!JASON: GET RID OF THIS LOGIC WHEN YOU INTERFACE DAVID LAINO'S NEW VERSION OF AeroDyn WITH DYNAMIC INFLOW INSTABILITIES FIXED:
   ! NOTE: I don't like the following IF...THEN construct, but it works.
   !       AeroDyn should be able to prevent itself from exploding when
   !          the rotor slows down!  This shouldn't need to be controlled
   !          by the dynamics program!
   ! Switch to EQUIL Inflow model since many variables in DYNIN are
   !    normalized by tip speed, which is now very small!:

   IF ( DYNINFL .OR. DYNINIT )  THEN   ! .TRUE. if DYNamic INflow model is engaged.

      DYNINFL = .FALSE.
      DYNINIT = .FALSE.


   ! Inform the user of this switch!

      CALL WrOver(' WARNING:                                           ')
      CALL WrScr ('  "DYNIN" InfModel switched to "EQUIL" by FAST to prevent instability ')
      CALL WrScr ('     of AeroDyn. This happened because the rotor has nearly stopped.  ')
      CALL WrScr ('                                                                      ')

      CALL UsrAlarm


! NOTE: This method suggested by D. Laino did not work:
!       Turn off all induction terms since the rotor speed is so low
!          and we don't want to have the DYNamic INflow model explode
!
!      WAKE  = .FALSE.
!      SWIRL = .FALSE.
   END IF


ENDIF



RETURN
END SUBROUTINE FixHSSBrTq
!=======================================================================
SUBROUTINE FAST_Terminate( ErrStat )
! This subroutine is called at program termination.  It deallocates variables and closes files.
!----------------------------------------------------------------------------------------------------

   USE            AeroElem
   USE            Blades
   USE            General                                   ! contains file units, too
   USE            InitCond
   USE            Linear
   USE            MassInert
   USE            Modes
   USE            Output
   USE            RtHndSid
   USE            Tower
   USE            TurbCont

   INTEGER,       INTENT(OUT) :: ErrStat                    ! Determines if an error was encountered

   !-------------------------------------------------------------------------------------------------
   ! Deallocate arrays
   !-------------------------------------------------------------------------------------------------

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



      ! MODULE Blades

   IF ( ALLOCATED(AerCen                             ) ) DEALLOCATE(AerCen                             )
   IF ( ALLOCATED(AeroCent                           ) ) DEALLOCATE(AeroCent                           )
   IF ( ALLOCATED(AeroTwst                           ) ) DEALLOCATE(AeroTwst                           )
   IF ( ALLOCATED(Alpha                              ) ) DEALLOCATE(Alpha                              )
   IF ( ALLOCATED(AxRedBld                           ) ) DEALLOCATE(AxRedBld                           )
   IF ( ALLOCATED(BAlpha                             ) ) DEALLOCATE(BAlpha                             )
   IF ( ALLOCATED(BldEDamp                           ) ) DEALLOCATE(BldEDamp                           )
   IF ( ALLOCATED(BldFDamp                           ) ) DEALLOCATE(BldFDamp                           )
   IF ( ALLOCATED(BlFract                            ) ) DEALLOCATE(BlFract                            )
   IF ( ALLOCATED(BMassDen                           ) ) DEALLOCATE(BMassDen                           )
   IF ( ALLOCATED(CAeroTwst                          ) ) DEALLOCATE(CAeroTwst                          )
   IF ( ALLOCATED(CBE                                ) ) DEALLOCATE(CBE                                )
   IF ( ALLOCATED(CBF                                ) ) DEALLOCATE(CBF                                )
   IF ( ALLOCATED(cgOffBEdg                          ) ) DEALLOCATE(cgOffBEdg                          )
   IF ( ALLOCATED(cgOffBFlp                          ) ) DEALLOCATE(cgOffBFlp                          )
   IF ( ALLOCATED(Chord                              ) ) DEALLOCATE(Chord                              )
   IF ( ALLOCATED(CThetaS                            ) ) DEALLOCATE(CThetaS                            )
   IF ( ALLOCATED(DRNodes                            ) ) DEALLOCATE( DRNodes                           )
   IF ( ALLOCATED(EAOffBEdg                          ) ) DEALLOCATE(EAOffBEdg                          )
   IF ( ALLOCATED(EAOffBFlp                          ) ) DEALLOCATE(EAOffBFlp                          )
   IF ( ALLOCATED(EAStff                             ) ) DEALLOCATE(EAStff                             )
   IF ( ALLOCATED(EdgcgOf                            ) ) DEALLOCATE(EdgcgOf                            )
   IF ( ALLOCATED(EdgEAOf                            ) ) DEALLOCATE(EdgEAOf                            )
   IF ( ALLOCATED(EdgIner                            ) ) DEALLOCATE(EdgIner                            )
   IF ( ALLOCATED(EdgStff                            ) ) DEALLOCATE(EdgStff                            )
   IF ( ALLOCATED(FlpcgOf                            ) ) DEALLOCATE(FlpcgOf                            )
   IF ( ALLOCATED(FlpEAOf                            ) ) DEALLOCATE(FlpEAOf                            )
   IF ( ALLOCATED(FlpIner                            ) ) DEALLOCATE(FlpIner                            )
   IF ( ALLOCATED(FlpStff                            ) ) DEALLOCATE(FlpStff                            )
   IF ( ALLOCATED(FStTunr                            ) ) DEALLOCATE(FStTunr                            )
   IF ( ALLOCATED(GJStff                             ) ) DEALLOCATE(GJStff                             )
   IF ( ALLOCATED(InerBEdg                           ) ) DEALLOCATE(InerBEdg                           )
   IF ( ALLOCATED(InerBFlp                           ) ) DEALLOCATE(InerBFlp                           )
   IF ( ALLOCATED(KBE                                ) ) DEALLOCATE(KBE                                )
   IF ( ALLOCATED(KBF                                ) ) DEALLOCATE(KBF                                )
   IF ( ALLOCATED(MassB                              ) ) DEALLOCATE(MassB                              )
   IF ( ALLOCATED(PrecrvRef                          ) ) DEALLOCATE(PrecrvRef                          )
   IF ( ALLOCATED(PreswpRef                          ) ) DEALLOCATE(PreswpRef                          )
   IF ( ALLOCATED(RefAxisxb                          ) ) DEALLOCATE(RefAxisxb                          )
   IF ( ALLOCATED(RefAxisyb                          ) ) DEALLOCATE(RefAxisyb                          )
   IF ( ALLOCATED(RNodes                             ) ) DEALLOCATE(RNodes                             )
   IF ( ALLOCATED(RNodesNorm                         ) ) DEALLOCATE(RNodesNorm                         )
   IF ( ALLOCATED(rSAerCenn1                         ) ) DEALLOCATE(rSAerCenn1                         )
   IF ( ALLOCATED(rSAerCenn2                         ) ) DEALLOCATE(rSAerCenn2                         )
   IF ( ALLOCATED(SAeroTwst                          ) ) DEALLOCATE(SAeroTwst                          )
   IF ( ALLOCATED(StiffBE                            ) ) DEALLOCATE(StiffBE                            )
   IF ( ALLOCATED(StiffBEA                           ) ) DEALLOCATE(StiffBEA                           )
   IF ( ALLOCATED(StiffBF                            ) ) DEALLOCATE(StiffBF                            )
   IF ( ALLOCATED(StiffBGJ                           ) ) DEALLOCATE(StiffBGJ                           )
   IF ( ALLOCATED(SThetaS                            ) ) DEALLOCATE(SThetaS                            )
   IF ( ALLOCATED(StrcTwst                           ) ) DEALLOCATE(StrcTwst                           )
   IF ( ALLOCATED(ThetaS                             ) ) DEALLOCATE(ThetaS                             )
   IF ( ALLOCATED(TwistedSF                          ) ) DEALLOCATE(TwistedSF                          )


      ! MODULE DOFs

   IF ( ALLOCATED(IC                                 ) ) DEALLOCATE(IC                                 )


      ! MODULE General

   IF ( ALLOCATED(BldFile                            ) ) DEALLOCATE(BldFile                            )


      ! MODULE InitCond

   IF ( ALLOCATED(BlPitchInit                        ) ) DEALLOCATE(BlPitchInit                        )
   IF ( ALLOCATED(DOF_FlagInit                       ) ) DEALLOCATE(DOF_FlagInit                       )

      ! MODULE Linear

   IF ( ALLOCATED(QD2op                              ) ) DEALLOCATE(QD2op                              )
   IF ( ALLOCATED(QDop                               ) ) DEALLOCATE(QDop                               )
   IF ( ALLOCATED(Qop                                ) ) DEALLOCATE(Qop                                )


      ! MODULE MassInert

   IF ( ALLOCATED(BldCG                              ) ) DEALLOCATE(BldCG                              )
   IF ( ALLOCATED(BldMass                            ) ) DEALLOCATE(BldMass                            )
   IF ( ALLOCATED(FirstMom                           ) ) DEALLOCATE(FirstMom                           )
   IF ( ALLOCATED(SecondMom                          ) ) DEALLOCATE(SecondMom                          )
   IF ( ALLOCATED(TipMass                            ) ) DEALLOCATE(TipMass                            )


      ! MODULE Modes

   IF ( ALLOCATED(BldEdgSh                           ) ) DEALLOCATE(BldEdgSh                           )
   IF ( ALLOCATED(BldFl1Sh                           ) ) DEALLOCATE(BldFl1Sh                           )
   IF ( ALLOCATED(BldFl2Sh                           ) ) DEALLOCATE(BldFl2Sh                           )
   IF ( ALLOCATED(FreqBE                             ) ) DEALLOCATE(FreqBE                             )
   IF ( ALLOCATED(FreqBF                             ) ) DEALLOCATE(FreqBF                             )
   IF ( ALLOCATED(CalcBModes                         ) ) DEALLOCATE(CalcBModes                         )


      ! MODULE Output

   IF ( ALLOCATED(LinAccES                           ) ) DEALLOCATE(LinAccES                           )
   IF ( ALLOCATED(LinAccET                           ) ) DEALLOCATE(LinAccET                           )
   IF ( ALLOCATED(FrcS0B                             ) ) DEALLOCATE(FrcS0B                             )
   IF ( ALLOCATED(FTHydro                            ) ) DEALLOCATE(FTHydro                            )
   IF ( ALLOCATED(MFHydro                            ) ) DEALLOCATE(MFHydro                            )
   IF ( ALLOCATED(MomH0B                             ) ) DEALLOCATE(MomH0B                             )
   IF ( ALLOCATED(OutData                            ) ) DEALLOCATE(OutData                            )
   IF ( ALLOCATED(AllOutData                         ) ) DEALLOCATE(AllOutData                         )
   IF ( ALLOCATED(TimeData                           ) ) DEALLOCATE(TimeData                           )
!   IF ( ALLOCATED(p%OutParam                           ) ) DEALLOCATE(p%OutParam                           )


      ! MODULE RtHndSid

   IF ( ALLOCATED(AngAccEFt                          ) ) DEALLOCATE(AngAccEFt                          )
   IF ( ALLOCATED(AngPosEF                           ) ) DEALLOCATE(AngPosEF                           )
   IF ( ALLOCATED(AngPosXF                           ) ) DEALLOCATE(AngPosXF                           )
   IF ( ALLOCATED(AngPosHM                           ) ) DEALLOCATE(AngPosHM                           )
   IF ( ALLOCATED(AngVelEF                           ) ) DEALLOCATE(AngVelEF                           )
   IF ( ALLOCATED(AugMat                             ) ) DEALLOCATE(AugMat                             )
   IF ( ALLOCATED(FrcS0Bt                            ) ) DEALLOCATE(FrcS0Bt                            )
   IF ( ALLOCATED(FSAero                             ) ) DEALLOCATE(FSAero                             )
   IF ( ALLOCATED(FSTipDrag                          ) ) DEALLOCATE(FSTipDrag                          )
   IF ( ALLOCATED(FTAero                             ) ) DEALLOCATE(FTAero                             )
   IF ( ALLOCATED(FTHydrot                           ) ) DEALLOCATE(FTHydrot                           )
   IF ( ALLOCATED(LinAccESt                          ) ) DEALLOCATE(LinAccESt                          )
   IF ( ALLOCATED(LinAccETt                          ) ) DEALLOCATE(LinAccETt                          )
   IF ( ALLOCATED(LinVelESm2                         ) ) DEALLOCATE(LinVelESm2                         )
   IF ( ALLOCATED(LinVelET                           ) ) DEALLOCATE(LinVelET                           )
   IF ( ALLOCATED(MFAero                             ) ) DEALLOCATE(MFAero                             )
   IF ( ALLOCATED(MFHydrot                           ) ) DEALLOCATE(MFHydrot                           )
   IF ( ALLOCATED(MomH0Bt                            ) ) DEALLOCATE(MomH0Bt                            )
   IF ( ALLOCATED(MMAero                             ) ) DEALLOCATE(MMAero                             )
   IF ( ALLOCATED(PAngVelEA                          ) ) DEALLOCATE(PAngVelEA                          )
   IF ( ALLOCATED(PAngVelEB                          ) ) DEALLOCATE(PAngVelEB                          )
   IF ( ALLOCATED(PAngVelEF                          ) ) DEALLOCATE(PAngVelEF                          )
   IF ( ALLOCATED(PAngVelEG                          ) ) DEALLOCATE(PAngVelEG                          )
   IF ( ALLOCATED(PAngVelEH                          ) ) DEALLOCATE(PAngVelEH                          )
   IF ( ALLOCATED(PAngVelEL                          ) ) DEALLOCATE(PAngVelEL                          )
   IF ( ALLOCATED(PAngVelEM                          ) ) DEALLOCATE(PAngVelEM                          )
   IF ( ALLOCATED(PAngVelEN                          ) ) DEALLOCATE(PAngVelEN                          )
   IF ( ALLOCATED(PAngVelER                          ) ) DEALLOCATE(PAngVelER                          )
   IF ( ALLOCATED(PAngVelEX                          ) ) DEALLOCATE(PAngVelEX                          )
   IF ( ALLOCATED(PFrcONcRt                          ) ) DEALLOCATE(PFrcONcRt                          )
   IF ( ALLOCATED(PFrcPRot                           ) ) DEALLOCATE(PFrcPRot                           )
   IF ( ALLOCATED(PFrcS0B                            ) ) DEALLOCATE(PFrcS0B                            )
   IF ( ALLOCATED(PFrcT0Trb                          ) ) DEALLOCATE(PFrcT0Trb                          )
   IF ( ALLOCATED(PFrcVGnRt                          ) ) DEALLOCATE(PFrcVGnRt                          )
   IF ( ALLOCATED(PFrcWTail                          ) ) DEALLOCATE(PFrcWTail                          )
   IF ( ALLOCATED(PFrcZAll                           ) ) DEALLOCATE(PFrcZAll                           )
   IF ( ALLOCATED(PFTHydro                           ) ) DEALLOCATE(PFTHydro                           )
   IF ( ALLOCATED(PLinVelEC                          ) ) DEALLOCATE(PLinVelEC                          )
   IF ( ALLOCATED(PLinVelED                          ) ) DEALLOCATE(PLinVelED                          )
   IF ( ALLOCATED(PLinVelEI                          ) ) DEALLOCATE(PLinVelEI                          )
   IF ( ALLOCATED(PLinVelEIMU                        ) ) DEALLOCATE(PLinVelEIMU                        )
   IF ( ALLOCATED(PLinVelEJ                          ) ) DEALLOCATE(PLinVelEJ                          )
   IF ( ALLOCATED(PLinVelEK                          ) ) DEALLOCATE(PLinVelEK                          )
   IF ( ALLOCATED(PLinVelEO                          ) ) DEALLOCATE(PLinVelEO                          )
   IF ( ALLOCATED(PLinVelEP                          ) ) DEALLOCATE(PLinVelEP                          )
   IF ( ALLOCATED(PLinVelEQ                          ) ) DEALLOCATE(PLinVelEQ                          )
   IF ( ALLOCATED(PLinVelES                          ) ) DEALLOCATE(PLinVelES                          )
   IF ( ALLOCATED(PLinVelET                          ) ) DEALLOCATE(PLinVelET                          )
   IF ( ALLOCATED(PLinVelEU                          ) ) DEALLOCATE(PLinVelEU                          )
   IF ( ALLOCATED(PLinVelEV                          ) ) DEALLOCATE(PLinVelEV                          )
   IF ( ALLOCATED(PLinVelEW                          ) ) DEALLOCATE(PLinVelEW                          )
   IF ( ALLOCATED(PLinVelEY                          ) ) DEALLOCATE(PLinVelEY                          )
   IF ( ALLOCATED(PLinVelEZ                          ) ) DEALLOCATE(PLinVelEZ                          )
   IF ( ALLOCATED(PMFHydro                           ) ) DEALLOCATE(PMFHydro                           )
   IF ( ALLOCATED(PMomBNcRt                          ) ) DEALLOCATE(PMomBNcRt                          )
   IF ( ALLOCATED(PMomH0B                            ) ) DEALLOCATE(PMomH0B                            )
   IF ( ALLOCATED(PMomLPRot                          ) ) DEALLOCATE(PMomLPRot                          )
   IF ( ALLOCATED(PMomNGnRt                          ) ) DEALLOCATE(PMomNGnRt                          )
   IF ( ALLOCATED(PMomNTail                          ) ) DEALLOCATE(PMomNTail                          )
   IF ( ALLOCATED(PMomX0Trb                          ) ) DEALLOCATE(PMomX0Trb                          )
   IF ( ALLOCATED(PMomXAll                           ) ) DEALLOCATE(PMomXAll                           )
   IF ( ALLOCATED(QD2T                               ) ) DEALLOCATE(QD2T                               )
   IF ( ALLOCATED(QD2TC                              ) ) DEALLOCATE(QD2TC                              )
   IF ( ALLOCATED(OgnlGeAzRo                         ) ) DEALLOCATE(OgnlGeAzRo                         )
   IF ( ALLOCATED(rQS                                ) ) DEALLOCATE(rQS                                )
   IF ( ALLOCATED(rS                                 ) ) DEALLOCATE(rS                                 )
   IF ( ALLOCATED(rS0S                               ) ) DEALLOCATE(rS0S                               )
   IF ( ALLOCATED(rT                                 ) ) DEALLOCATE(rT                                 )
   IF ( ALLOCATED(rT0T                               ) ) DEALLOCATE(rT0T                               )
   IF ( ALLOCATED(rZT                                ) ) DEALLOCATE(rZT                                )
   IF ( ALLOCATED(SolnVec                            ) ) DEALLOCATE(SolnVec                            )



      ! MODULE Tower


   IF ( ALLOCATED(AxRedTFA                           ) ) DEALLOCATE(AxRedTFA                           )
   IF ( ALLOCATED(AxRedTSS                           ) ) DEALLOCATE(AxRedTSS                           )
   IF ( ALLOCATED(CAT                                ) ) DEALLOCATE(CAT                                )
   IF ( ALLOCATED(CDT                                ) ) DEALLOCATE(CDT                                )
   IF ( ALLOCATED(cgOffTFA                           ) ) DEALLOCATE(cgOffTFA                           )
   IF ( ALLOCATED(cgOffTSS                           ) ) DEALLOCATE(cgOffTSS                           )
   IF ( ALLOCATED(DHNodes                            ) ) DEALLOCATE(DHNodes                            )
   IF ( ALLOCATED(DiamT                              ) ) DEALLOCATE(DiamT                              )
   IF ( ALLOCATED(HNodes                             ) ) DEALLOCATE(HNodes                             )
   IF ( ALLOCATED(HNodesNorm                         ) ) DEALLOCATE(HNodesNorm                         )
   IF ( ALLOCATED(HtFract                            ) ) DEALLOCATE(HtFract                            )
   IF ( ALLOCATED(InerTFA                            ) ) DEALLOCATE(InerTFA                            )
   IF ( ALLOCATED(InerTSS                            ) ) DEALLOCATE(InerTSS                            )
   IF ( ALLOCATED(MassT                              ) ) DEALLOCATE(MassT                              )
   IF ( ALLOCATED(StiffTEA                           ) ) DEALLOCATE(StiffTEA                           )
   IF ( ALLOCATED(StiffTFA                           ) ) DEALLOCATE(StiffTFA                           )
   IF ( ALLOCATED(StiffTGJ                           ) ) DEALLOCATE(StiffTGJ                           )
   IF ( ALLOCATED(StiffTSS                           ) ) DEALLOCATE(StiffTSS                           )
   IF ( ALLOCATED(TMassDen                           ) ) DEALLOCATE(TMassDen                           )
   IF ( ALLOCATED(TwEAStif                           ) ) DEALLOCATE(TwEAStif                           )
   IF ( ALLOCATED(TwFAcgOf                           ) ) DEALLOCATE(TwFAcgOf                           )
   IF ( ALLOCATED(TwFAIner                           ) ) DEALLOCATE(TwFAIner                           )
   IF ( ALLOCATED(TwFAStif                           ) ) DEALLOCATE(TwFAStif                           )
   IF ( ALLOCATED(TwGJStif                           ) ) DEALLOCATE(TwGJStif                           )
   IF ( ALLOCATED(TwrFASF                            ) ) DEALLOCATE(TwrFASF                            )
   IF ( ALLOCATED(TwrSSSF                            ) ) DEALLOCATE(TwrSSSF                            )
   IF ( ALLOCATED(TwSScgOf                           ) ) DEALLOCATE(TwSScgOf                           )
   IF ( ALLOCATED(TwSSIner                           ) ) DEALLOCATE(TwSSIner                           )
   IF ( ALLOCATED(TwSSStif                           ) ) DEALLOCATE(TwSSStif                           )


      ! MODULE TurbCont

   IF ( ALLOCATED(BlPitch                            ) ) DEALLOCATE(BlPitch                            )
   IF ( ALLOCATED(BlPitchCom                         ) ) DEALLOCATE(BlPitchCom                         )
   IF ( ALLOCATED(BlPitchF                           ) ) DEALLOCATE(BlPitchF                           )
   IF ( ALLOCATED(BlPitchFrct                        ) ) DEALLOCATE(BlPitchFrct                        )
   IF ( ALLOCATED(BlPitchI                           ) ) DEALLOCATE(BlPitchI                           )
   IF ( ALLOCATED(TBDepISp                           ) ) DEALLOCATE(TBDepISp                           )
   IF ( ALLOCATED(TPitManE                           ) ) DEALLOCATE(TPitManE                           )
   IF ( ALLOCATED(TPitManS                           ) ) DEALLOCATE(TPitManS                           )
   IF ( ALLOCATED(TTpBrDp                            ) ) DEALLOCATE(TTpBrDp                            )
   IF ( ALLOCATED(TTpBrFl                            ) ) DEALLOCATE(TTpBrFl                            )
   IF ( ALLOCATED(BegPitMan                          ) ) DEALLOCATE(BegPitMan                          )

   !-------------------------------------------------------------------------------------------------
   ! Close any open files
   !-------------------------------------------------------------------------------------------------
   CALL CloseEcho()           ! NWTC Library


   CLOSE( UnIn )     !20      ! I/O unit number for the input files.
   CLOSE( UnOu )     !21      ! I/O unit number for the tabular output file.
   CLOSE( UnSu )     !22      ! I/O unit number for the summary output file.
   CLOSE( UnAD )     !23      ! I/O unit number for the ADAMS dataset output file (.adm).
   CLOSE( UnAC )     !24      ! I/O unit number for the ADAMS control output file (.acf) useful for an ADAMS SIMULATE analysis.
   CLOSE( UnAL )     !25      ! I/O unit number for the ADAMS control output file (.acf) useful for an ADAMS LINEAR analysis.
   CLOSE( UnLn )     !26      ! I/O unit number for the FAST linear output file (.lin).
   CLOSE( UnNoSpec ) !27      ! I/O unit number for the noise spectr output file.
   CLOSE( UnNoSPL )  !28      ! I/O unit number for the noise SPL output file.
   CLOSE( UnOuBin )  !29      ! I/O unit number for the binary output file.

   !-------------------------------------------------------------------------------------------------
   ! Reset the initialization flag
   !-------------------------------------------------------------------------------------------------
!   Initialized = .FALSE.
   ErrStat     = 0


END SUBROUTINE FAST_Terminate
!=======================================================================
SUBROUTINE Gauss( AugMatIn, NumEq, SolnVec )


   ! This routine uses the Gauss-Jordan elimination method for the
   !   solution of a given set of simultaneous linear equations.
   ! NOTE: this routine works if no pivot points are zero and you
   !   don't want the eschelon or reduced eschelon form of the
   !   augmented matrix.  The form of the original augmented matrix
   !   IS preserved in this call.



IMPLICIT                        NONE


   ! Passed variables:

INTEGER(4), INTENT(IN )      :: NumEq                                           ! Number of equations in augmented matrix.

REAL(ReKi), INTENT(IN )      :: AugMatIn (NumEq,( NumEq + 1 ))                  ! Augmented matrix passed into this subroutine.
REAL(ReKi), INTENT(OUT)      :: SolnVec  (NumEq)                                ! Solution vector.


   ! Local variables:

REAL(ReKi)                   :: AugMat   (NumEq,( NumEq + 1 ))                  ! A copy of the augmented matrix.

INTEGER(4)                   :: I                                               ! Steps through columns
INTEGER(4)                   :: J                                               ! Steps through rows
INTEGER(4)                   :: L                                               ! Steps through rows
INTEGER(4)                   :: NAug                                            ! Column dimension of augmented matrix



   ! Transfer the data from AugMatIn to AugMat:

AugMat = AugMatIn


   ! Find the column dimension of the augmented matrix:

NAug = NumEq + 1


   ! Perform Gauss-Jordan elimination and store the solution vector
   !   in the last column of the augmented matrix:

DO L = 1,NumEq             ! Loop through all rows
   DO I = ( L + 1 ), NAug  ! Loop through all columns above current row number
      AugMat(L,I) = AugMat(L,I) / AugMat(L,L)
      DO J = 1,NumEq       ! Loop through all rows except L
         IF ( J /= L )  AugMat(J,I) = AugMat(J,I) - ( AugMat(J,L)*AugMat(L,I) )
      ENDDO                ! J - All rows except L
   ENDDO                   ! I - All columns above current row number
ENDDO                      ! L - All rows


   ! Transfer the solution vector from AugMat() to SolnVec():

SolnVec = AugMat(:,NAug)



RETURN
END SUBROUTINE Gauss
!=======================================================================
SUBROUTINE InitBlDefl ( K, InitQF1, InitQF2, InitQE1, p )


   ! This routine calculates the initial blade deflections.
   ! Base the intial values of the blade DOFs, INITQF1, INITQF2, and
   !   INITQE1, on OoPDefl and IPDefl.
   ! Write messages to the screen if the specified initial tip displacements
   !  are incompatible with the enabled DOFs.


USE                             Blades
USE                             Features
USE                             InitCond
USE                             TurbCont

IMPLICIT                        NONE


   ! Passed variables:

REAL(ReKi), INTENT(OUT)      :: InitQE1                                         ! Initial edge deflection (output).
REAL(ReKi), INTENT(OUT)      :: InitQF1                                         ! Initial flap deflection for mode 1 (output).
REAL(ReKi), INTENT(OUT)      :: InitQF2                                         ! Initial flap deflection for mode 2 (output).

INTEGER(4), INTENT(IN )      :: K                                               ! Blade number (input).
TYPE(StrD_ParameterType),  INTENT(IN)  :: p                                     ! The parameters of the structural dynamics module


   ! Local variables:

REAL(ReKi)                   :: A(2,3)                                          ! Augmented matrix for solution of initial deflections.
REAL(ReKi)                   :: CosPitch                                        ! Cosine of the pitch for this blade.
REAL(ReKi)                   :: Det                                             ! Determinate of right-hand side of A.
REAL(ReKi)                   :: SinPitch                                        ! Sine of the pitch for this blade.
REAL(ReKi)                   :: TotResid                                        ! Generator torque.

CHARACTER(*), PARAMETER      :: Approx   = ' An approximate characterization of the specified blade deflection will be made.'
CHARACTER(*), PARAMETER      :: BadIP    = ' Initial blade in-plane tip displacement will be ignored.'
CHARACTER(*), PARAMETER      :: BadOoP   = ' Initial blade out-of-plane tip displacement will be ignored.'
CHARACTER(9)                 :: BladeStr = ' Blade 1:'                          ! String to hold the blade name.
CHARACTER(*), PARAMETER      :: Ignore   = ' All initial blade tip displacements will be ignored.'
CHARACTER(*), PARAMETER      :: Incompat = ' Initial blade tip displacements are Incompat with enabled DOFs.'



   ! Generate a string to hold the blade name.

WRITE (BladeStr(8:8),'(I1)')  K


   ! Calculate the array of deflections(???).

CosPitch = COS( BlPitch(K) )
SinPitch = SIN( BlPitch(K) )

A(1,2) =  TwistedSF(K,1,3,p%TipNode,0)*CosPitch + TwistedSF(K,2,3,p%TipNode,0)*SinPitch
A(2,2) = -TwistedSF(K,1,3,p%TipNode,0)*SinPitch + TwistedSF(K,2,3,p%TipNode,0)*CosPitch
A(1,3) =  OoPDefl
A(2,3) =  IPDefl

IF ( FlapDOF1 )  THEN                       ! Blade flap mode 1 is enabled

   InitQF2 = 0.0

   A(1,1) =  TwistedSF(K,1,1,p%TipNode,0)*CosPitch + TwistedSF(K,2,1,p%TipNode,0)*SinPitch
   A(2,1) = -TwistedSF(K,1,1,p%TipNode,0)*SinPitch + TwistedSF(K,2,1,p%TipNode,0)*CosPitch

   DET = ( A(1,1)*A(2,2) - A(1,2)*A(2,1) )

   IF ( DET /= 0.0 )  THEN                  ! Apply all flap deflection to mode 1

      InitQF1 = ( A(1,3)*A(2,2) - A(1,2)*A(2,3) )/DET
      InitQE1 = ( A(1,1)*A(2,3) - A(1,3)*A(2,1) )/DET

   ELSEIF ( .NOT. EdgeDOF )  THEN          ! Blade edge mode 1 is not enabled which caused DET = 0.

      InitQE1 = 0.0

      IF ( A(1,1) /= 0.0 )  THEN

         IF ( A(2,1) /= 0.0 )  THEN         ! Find a solution of the 2 equations in 1 variable that
                                            !  minimizes the sum of the squares of the equation's residuals.

            InitQF1 = ( A(1,1)*A(1,3) + A(2,1)*A(2,3) )/( A(1,1)**2 + A(2,1)**2 )

            TotResid = SQRT( ( A(1,1)*InitQF1 - A(1,3) )**2 + ( A(2,1)*InitQF1 - A(2,3) )**2 )

            IF (TotResid /= 0.0 )  THEN
               CALL UsrAlarm
               CALL WrScr ( BladeStr//Incompat )
               CALL WrScr ( BladeStr//Approx   )
               CALL WrScr ( '' )
            ENDIF

         ELSE

            InitQF1 = A(1,3)/A(1,1)

            IF ( IPDefl /= 0.0 )  THEN
               CALL UsrAlarm
               CALL WrScr ( BladeStr//Incompat )
               CALL WrScr ( BladeStr//BadIP    )
               CALL WrScr ( '' )
            ENDIF

         ENDIF

      ELSE

         IF ( A(2,1) /= 0.0 )  THEN

            InitQF1 = A(2,3)/A(2,1)

            IF ( OoPDefl /= 0.0 )  THEN
               CALL UsrAlarm
               CALL WrScr ( BladeStr//Incompat )
               CALL WrScr ( BladeStr//BadOoP   )
               CALL WrScr ( ' ' )
            ENDIF

         ELSE

            InitQF1 = 0.0

            IF ( ( OoPDefl /= 0.0 ) .OR. ( IPDefl /= 0.0 ) )  THEN
               CALL UsrAlarm
               CALL WrScr ( BladeStr//Incompat )
               IF ( OoPDefl /= 0.0 )  CALL WrScr ( BladeStr//BadOoP  )
               IF ( IPDefl  /= 0.0 )  CALL WrScr ( BladeStr//BadIP   )
               CALL WrScr ( ' ' )
            ENDIF

         ENDIF
      ENDIF

   ELSE                                     ! It is impossible to find any "good" solution, so ignore the initial tip displacements

      InitQF1 = 0.0
      InitQE1 = 0.0

      IF ( ( OoPDefl /= 0.0 ) .OR. ( IPDefl /= 0.0 ) )  THEN
         CALL UsrAlarm
         CALL WrScr ( BladeStr//Incompat )
         CALL WrScr ( BladeStr//Ignore   )
         CALL WrScr ( ' ' )

      ENDIF

   ENDIF

ELSE                                        ! Blade flap mode 1 is not enabled.

   InitQF1 = 0.0

   IF ( FlapDOF2 )  THEN                    ! Blade flap mode 2 is enabled.

      A(1,1) =  TwistedSF(K,1,2,p%TipNode,0)*CosPitch + TwistedSF(K,2,2,p%TipNode,0)*SinPitch
      A(2,1) = -TwistedSF(K,1,2,p%TipNode,0)*SinPitch + TwistedSF(K,2,2,p%TipNode,0)*CosPitch

      DET = ( A(1,1)*A(2,2) - A(1,2)*A(2,1) )

      IF ( DET /= 0.0 )  THEN               ! Apply all flap deflection to mode 2
         InitQF2 = ( A(1,3)*A(2,2) - A(1,2)*A(2,3) )/DET
         InitQE1 = ( A(1,1)*A(2,3) - A(1,3)*A(2,1) )/DET

      ELSEIF ( .NOT. EdgeDOF )  THEN          ! Blade edge mode 1 is not enabled which caused DET = 0.

         InitQE1 = 0.0

         IF ( A(1,1) /= 0.0 )  THEN

            IF ( A(2,1) /= 0.0 )  THEN      ! Find a solution of the 2 equations in 1 variable that
                                            !  minimizes the sum of the squares of the equation's residuals
               InitQF2 = ( A(1,1)*A(1,3) + A(2,1)*A(2,3) )/( A(1,1)**2 + A(2,1)**2 )

               TotResid = SQRT( ( A(1,1)*InitQF2 - A(1,3))**2 + ( A(2,1)*InitQF2 - A(2,3) )**2 )

               IF ( TotResid /= 0.0 )  THEN
                  CALL UsrAlarm
                  CALL WrScr ( BladeStr//Incompat )
                  CALL WrScr ( BladeStr//Approx   )
                  CALL WrScr ( ' ' )
               ENDIF
            ELSE
               InitQF2 = A(1,3)/A(1,1)

               IF ( IPDefl /= 0.0 )  THEN
                  CALL UsrAlarm
                  CALL WrScr ( BladeStr//Incompat )
                  CALL WrScr ( BladeStr//BadIP    )
                  CALL WrScr ( ' ' )
               ENDIF
            ENDIF
         ELSE
            IF ( A(2,1) /= 0.0 )  THEN
               InitQF2 = A(2,3)/A(2,1)

               IF ( OoPDefl /= 0.0 )  THEN
                  CALL UsrAlarm
                  CALL WrScr ( BladeStr//Incompat )
                  CALL WrScr ( BladeStr//BadOoP   )
                  CALL WrScr ( ' ' )
               ENDIF
            ELSE
               InitQF2 = 0.0

               IF ( ( OoPDefl /= 0.0 ) .OR. ( IPDefl /= 0.0 ) )  THEN
                  CALL UsrAlarm
                  CALL WrScr ( BladeStr//Incompat )
                  IF ( OoPDefl /= 0.0 )  CALL WrScr ( BladeStr//BadOoP  )
                  IF ( IPDefl  /= 0.0 )  CALL WrScr ( BladeStr//BadIP   )
                  CALL WrScr ( ' ' )
               ENDIF
            ENDIF
         ENDIF

      ELSE                                  ! It is impossible to find any "good" solution, so ignore
                                            ! the initial tip displacements.
         InitQF2 = 0.0
         InitQE1 = 0.0

         IF ( ( OoPDefl /= 0.0 ) .OR. ( IPDefl /= 0.0 ) )  THEN
            CALL UsrAlarm
            CALL WrScr ( BladeStr//Incompat )
            CALL WrScr ( BladeStr//Ignore   )
            CALL WrScr ( ' ' )
          ENDIF
      ENDIF

   ELSE                                     ! Blade flap mode 2 is not enabled.

      InitQF2 = 0.0

      IF ( A(1,2) /= 0.0 )  THEN

         IF ( A(2,2) /= 0.0 )  THEN         ! Find a solution of the 2 equations in 1 variable that minimizes
                                            !  the sum of the squares of the equation's residuals.
            InitQE1 = ( A(1,2)*A(1,3) + A(2,2)*A(2,3) )/( A(1,2)**2 + A(2,2)**2 )

            TotResid = SQRT( ( A(1,2)*InitQE1 - A(1,3) )**2 + ( A(2,2)*InitQE1 - A(2,3) )**2)

            IF ( TotResid /= 0.0 )  THEN
               CALL UsrAlarm
               CALL WrScr ( BladeStr//Incompat )
               CALL WrScr ( BladeStr//Approx   )
               CALL WrScr ( ' ' )
            ENDIF

         ELSE

            InitQE1 = A(1,3)/A(1,2)

            IF ( IPDefl /= 0.0 )  THEN
               CALL UsrAlarm
               CALL WrScr ( BladeStr//Incompat )
               CALL WrScr ( BladeStr//BadIP    )
               CALL WrScr ( ' ' )
            ENDIF

         ENDIF

      ELSE

         IF ( A(2,2) /= 0.0 )  THEN

            InitQE1 = A(2,3)/A(2,2)

            IF ( OoPDefl /= 0.0 )  THEN
               CALL UsrAlarm
               CALL WrScr ( BladeStr//Incompat )
               CALL WrScr ( BladeStr//BadOoP   )
               CALL WrScr ( ' ' )
            ENDIF

         ELSE

            InitQE1 = 0.0

            IF ( ( OoPDefl /= 0. ) .OR. ( IPDefl /= 0. ) )  THEN
               CALL UsrAlarm
               CALL WrScr ( BladeStr//Incompat )
               IF ( OoPDefl /= 0.0 )  CALL WrScr ( BladeStr//BadOoP  )
               IF ( IPDefl  /= 0.0 )  CALL WrScr ( BladeStr//BadIP   )
               CALL WrScr ( ' ' )
            ENDIF

         ENDIF

      ENDIF

   ENDIF

ENDIF


RETURN
END SUBROUTINE InitBlDefl
!=======================================================================
SUBROUTINE FAST_Initialize(p,x,y,OtherState)


   ! Initialize sets up starting values for each degree of freedom.
   ! NOTE: DOF_Flag(L) is a LOGICAL array storing the value of the feature
   !       flag for the (L)th DOF.  It is used in routine RtHS() to ensure that
   !       the (L)th DOF acceleration is zero.


USE                             EnvCond
USE                             Features
USE                             FloatingPlatform, ONLY:InitFltngPtfmLd
USE                             General
USE                             InitCond
USE                             Output
USE                             Platform
USE                             SimCont
USE                             Tower
USE                             Waves, ONLY:InitWaves

IMPLICIT                        NONE


   ! Passed variables

TYPE(StrD_ParameterType),        INTENT(INOUT) :: p                             ! Parameters of the structural dynamics module
TYPE(StrD_ContinuousStateType),  INTENT(INOUT) :: x                             ! Continuous states of the structural dynamics module
TYPE(StrD_OutputType),           INTENT(INOUT) :: y                             ! System outputs of the structural dynamics module
TYPE(StrD_OtherStateType),       INTENT(INOUT) :: OtherState                    ! Other State data type for the structural dynamics module


   ! Local variables:

REAL(ReKi), ALLOCATABLE      :: DZNodesPtfm(:)                                  ! Length of variable-length support platform elements (meters)

REAL(ReKi)                   :: InitQE1                                         ! Initial value of the 1st blade edge DOF
REAL(ReKi)                   :: InitQF1                                         ! Initial value of the 1st blade flap DOF
REAL(ReKi)                   :: InitQF2                                         ! Initial value of the 2nd blade flap DOF

REAL(ReKi), ALLOCATABLE      :: WaveKinzi0 (:)                                  ! zi-coordinates for points along a vertical line passing through the platform reference point where the incident wave kinematics will be computed; these are relative to the mean see level (meters)
INTEGER(4)                   :: I                                               ! Loops through all DOFs.
INTEGER(4)                   :: J                                               ! Loops through nodes / elements.
INTEGER(4)                   :: K                                               ! Loops through blades.

INTEGER(4)                   :: NWaveKin0                                       ! Number of points along a vertical line passing through the platform reference point where the incident wave kinematics will be computed (-)
INTEGER(IntKi)               :: Sttus                                           ! Status returned by an attempted allocation.

CHARACTER(1024)              :: ErrMsg                                          ! Message when error occurs

   ! Allocate many of the variable-length arrays:

CALL Alloc(p,x,y, OtherState)

   ! Initialize the DOF data
CALL StrD_InitDOFs( OtherState%DOFs, p, Sttus, ErrMsg )
IF ( Sttus > AbortErrLev ) CALL ProgAbort( ErrMsg )


   ! Initialize the IC array:

IC(1) = 1
DO I = 2,NMX
   IC(I) = IC(1) - I + 1 + NMX
ENDDO



   ! Define arrays of DOF indices (pointers) that contribute to the angular
   !   velocities of each rigid body of the wind turbine in the inertia frame:
   ! NOTE: We must include ALL of the appropriate DOF indices in these arrays,
   !       not just the indices of the enabled DOFs, since disabling a DOF only
   !       implies that each DOF acceleration is zero--it does not imply
   !       that each DOF velocity is zero (for example, consider disabled
   !       generator DOF, which still spins at constant speed).

!------------

   ! Compute the constant blade and tower properties:

CALL Coeff(p)


   ! Initialize the accelerations to zero.

OtherState%QD2 = 0.0


   ! Blade motions:

DO K = 1,p%NumBl   ! Loop through all blades


   ! Calculate the initial blade deflections:

   CALL InitBlDefl ( K, InitQF1, InitQF2, InitQE1, p )


   ! Apply these initial blade DOF values to the corresponding
   !   DOFs for this blade.

   OtherState%Q ( DOF_BF(K,1), 1 ) = InitQF1   !
   OtherState%Q ( DOF_BF(K,2), 1 ) = InitQF2   ! These come from InitBlDefl().
   OtherState%Q ( DOF_BE(K,1), 1 ) = InitQE1   !
   OtherState%QD( DOF_BF(K,1), 1 ) = 0.0
   OtherState%QD( DOF_BF(K,2), 1 ) = 0.0
   OtherState%QD( DOF_BE(K,1), 1 ) = 0.0

   p%DOF_Flag( DOF_BF(K,1) ) = FlapDOF1
   p%DOF_Flag( DOF_BF(K,2) ) = FlapDOF2
   p%DOF_Flag( DOF_BE(K,1) ) = EdgeDOF

   p%DOF_Desc( DOF_BF(K,1) ) = '1st flapwise bending-mode DOF of blade '//TRIM(Int2LStr( K ))// &
                             ' (internal DOF index = DOF_BF('         //TRIM(Int2LStr( K ))//',1))'
   p%DOF_Desc( DOF_BE(K,1) ) = '1st edgewise bending-mode DOF of blade '//TRIM(Int2LStr( K ))// &
                             ' (internal DOF index = DOF_BE('         //TRIM(Int2LStr( K ))//',1))'
   p%DOF_Desc( DOF_BF(K,2) ) = '2nd flapwise bending-mode DOF of blade '//TRIM(Int2LStr( K ))// &
                             ' (internal DOF index = DOF_BF('         //TRIM(Int2LStr( K ))//',2))'

ENDDO          ! K - All blades


IF ( p%NumBl == 2 )  THEN


   ! Teeter Motion

   ! Set initial teeter angle to TeetDefl and initial teeter angular velocity
   !   to 0.

   OtherState%Q (DOF_Teet,1) = TeetDefl
   OtherState%QD(DOF_Teet,1) = 0.0

   p%DOF_Flag(DOF_Teet) = TeetDOF

   p%DOF_Desc(DOF_Teet) = 'Hub teetering DOF (internal DOF index = DOF_Teet)'

ENDIF


   ! Shaft compliance

   ! The initial shaft compliance displacements and velocities are all zero.
   !   They will remain zero if the drivetrain DOF is disabled:

OtherState%Q (DOF_DrTr,1) = 0.0
OtherState%QD(DOF_DrTr,1) = 0.0

p%DOF_Flag(DOF_DrTr) = DrTrDOF

p%DOF_Desc(DOF_DrTr) = 'Drivetrain rotational-flexibility DOF (internal DOF index = DOF_DrTr)'


   ! Generator azimuth

   ! Set initial generator azimuth angle.  Turn rotor on, whether it is
   !   fixed or variable speed.  If it is fixed speed, set up the
   !   fixed rpm.

!JASON: CHANGE THESE MOD() FUNCTIONS INTO MODULO() FUNCTIONS SO THAT YOU CAN ELIMINATE ADDING 360:
QAzimInit      = MOD( Azimuth - p%AzimB1Up + 270.0 + 360.0, 360.0 )*D2R   ! Internal position of blade 1.

OtherState%Q (DOF_GeAz,1) = QAzimInit
OtherState%QD(DOF_GeAz,1) = RotSpeed                                               ! Rotor speed in rad/sec.

p%DOF_Flag(DOF_GeAz) = GenDOF

p%DOF_Desc(DOF_GeAz) = 'Variable speed generator DOF (internal DOF index = DOF_GeAz)'


   ! Rotor-furl motion

   ! Set initial rotor-furl angle to RotFurl.  If rotor-furl is off, this
   !   becomes a fixed rotor-furl angle.

OtherState%Q (DOF_RFrl,1) = RotFurl
OtherState%QD(DOF_RFrl,1) = 0.0

p%DOF_Flag(DOF_RFrl) = RFrlDOF

p%DOF_Desc(DOF_RFrl) = 'Rotor-furl DOF (internal DOF index = DOF_RFrl)'


   ! Tail-furl motion

   ! Set initial tail-furl angle to TailFurl.  If tail-furl is off, this
   !   becomes a fixed tail-furl angle.

OtherState%Q (DOF_TFrl,1) = TailFurl
OtherState%QD(DOF_TFrl,1) = 0.0

p%DOF_Flag(DOF_TFrl) = TFrlDOF

p%DOF_Desc(DOF_TFrl) = 'Tail-furl DOF (internal DOF index = DOF_TFrl)'


   ! Yaw Motion

   ! Set initial yaw angle to NacYaw.  If yaw is off, this becomes a
   !   fixed yaw angle.

OtherState%Q (DOF_Yaw ,1) = NacYaw
OtherState%QD(DOF_Yaw ,1) = 0.0

p%DOF_Flag(DOF_Yaw ) = YawDOF

p%DOF_Desc(DOF_Yaw ) = 'Nacelle yaw DOF (internal DOF index = DOF_Yaw)'


   ! Tower motion

   ! Assign all the displacements to mode 1 unless it is disabled.  If mode 1
   !   is disabled and mode 2 is enabled, assign all displacements to mode 2.
   ! If both modes are disabled, set the displacements to zero.

OtherState%Q   (DOF_TFA1,1) =  0.0
OtherState%Q   (DOF_TSS1,1) =  0.0
OtherState%Q   (DOF_TFA2,1) =  0.0
OtherState%Q   (DOF_TSS2,1) =  0.0

IF (    TwFADOF1 )  THEN   ! First fore-aft tower mode is enabled.
   OtherState%Q(DOF_TFA1,1) =  TTDspFA
ELSEIF( TwFADOF2 )  THEN   ! Second fore-aft tower mode is enabled, but first is not.
   OtherState%Q(DOF_TFA2,1) =  TTDspFA
ENDIF

IF (    TwSSDOF1 )  THEN   ! First side-to-side tower mode is enabled.
   OtherState%Q(DOF_TSS1,1) = -TTDspSS
ELSEIF( TwSSDOF2 )  THEN   ! Second side-to-side tower mode is enabled, but first is not.
   OtherState%Q(DOF_TSS2,1) = -TTDspSS
ENDIF

OtherState%QD  (DOF_TFA1,1) =  0.0
OtherState%QD  (DOF_TSS1,1) =  0.0
OtherState%QD  (DOF_TFA2,1) =  0.0
OtherState%QD  (DOF_TSS2,1) =  0.0

p%DOF_Flag(DOF_TFA1) = TwFADOF1
p%DOF_Flag(DOF_TSS1) = TwSSDOF1
p%DOF_Flag(DOF_TFA2) = TwFADOF2
p%DOF_Flag(DOF_TSS2) = TwSSDOF2

p%DOF_Desc(DOF_TFA1) = '1st tower fore-aft bending mode DOF (internal DOF index = DOF_TFA1)'
p%DOF_Desc(DOF_TSS1) = '1st tower side-to-side bending mode DOF (internal DOF index = DOF_TSS1)'
p%DOF_Desc(DOF_TFA2) = '2nd tower fore-aft bending mode DOF (internal DOF index = DOF_TFA2)'
p%DOF_Desc(DOF_TSS2) = '2nd tower side-to-side bending mode DOF (internal DOF index = DOF_TSS2)'


   ! Platform Motion

   ! Set initial platform displacements.  If platform DOFs are off, these
   !   become fixed platform displacements.

OtherState%Q (DOF_Sg  ,1) = PtfmSurge
OtherState%Q (DOF_Sw  ,1) = PtfmSway
OtherState%Q (DOF_Hv  ,1) = PtfmHeave
OtherState%Q (DOF_R   ,1) = PtfmRoll
OtherState%Q (DOF_P   ,1) = PtfmPitch
OtherState%Q (DOF_Y   ,1) = PtfmYaw
OtherState%QD(DOF_Sg  ,1) = 0.0
OtherState%QD(DOF_Sw  ,1) = 0.0
OtherState%QD(DOF_Hv  ,1) = 0.0
OtherState%QD(DOF_R   ,1) = 0.0
OtherState%QD(DOF_P   ,1) = 0.0
OtherState%QD(DOF_Y   ,1) = 0.0

p%DOF_Flag(DOF_Sg  ) = PtfmSgDOF
p%DOF_Flag(DOF_Sw  ) = PtfmSwDOF
p%DOF_Flag(DOF_Hv  ) = PtfmHvDOF
p%DOF_Flag(DOF_R   ) = PtfmRDOF
p%DOF_Flag(DOF_P   ) = PtfmPDOF
p%DOF_Flag(DOF_Y   ) = PtfmYDOF

p%DOF_Desc(DOF_Sg  ) = 'Platform horizontal surge translation DOF (internal DOF index = DOF_Sg)'
p%DOF_Desc(DOF_Sw  ) = 'Platform horizontal sway translation DOF (internal DOF index = DOF_Sw)'
p%DOF_Desc(DOF_Hv  ) = 'Platform vertical heave translation DOF (internal DOF index = DOF_Hv)'
p%DOF_Desc(DOF_R   ) = 'Platform roll tilt rotation DOF (internal DOF index = DOF_R)'
p%DOF_Desc(DOF_P   ) = 'Platform pitch tilt rotation DOF (internal DOF index = DOF_P)'
p%DOF_Desc(DOF_Y   ) = 'Platform yaw rotation DOF (internal DOF index = DOF_Y)'



   ! Define DOF_FlagInit, which is DOF_Flag during model initialization (at the
   !   start of the simulation):

DOF_FlagInit = p%DOF_Flag



   ! Calculate the number of active (enabled) DOFs in the model, OtherState%DOFs%NActvDOF:

CALL SetEnabledDOFIndexArrays( p, OtherState%DOFs )

   ! Initialize the variables associated with the incident waves and
   !   hydrodynamic loading, if necessary:

IF (     ( PtfmModel == 2 ) .AND. CompHydro )  THEN   ! .TRUE. if we have a fixed bottom offshore turbine and we are using the undocumented monopile features.


   ! Increase the value of WaveTMax to ensure that the maximum value within
   !   array WaveTime is at least as large as TMax+DT:

   IF ( ( WaveTMax - WaveDT ) < ( TMax + DT ) )  WaveTMax = TMax + WaveDT + DT   ! NOTE: The + WaveDT is needed because the internal array WaveTime has a maximum value of WaveTMax - WaveDT; the + DT is needed since the simulation time can reach TMax + DT in ADAMS.


   ! Determine the zi-coordinates for points along a vertical line passing
   !   through the platform reference point where the incident wave kinematics
   !   will be computed.  These are the vertical locations of the undeflected
   !   tower nodes relative to the mean sea level when the support platform is
   !   undisplaced:

   NWaveKin0 = p%TwrNodes

   ALLOCATE ( WaveKinzi0 (NWaveKin0) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the WaveKinzi0 array.')
   ENDIF

   DO J = 1,NWaveKin0   ! Loop through the tower nodes / elements
      WaveKinzi0(J) = p%TwrRBHt + HNodes(J) - p%TwrDraft
   ENDDO                ! J - Tower nodes / elements


   ! Initialize the variables used in the undocumented time domain hydrodynamic
   !   loading routines:

   CALL InitWaves ( WtrDens   , WtrDpth   , WaveMod  , WaveStMod, &
                    WaveTMax  , WaveDT    , WaveHs   , WaveTp   , &
                    WavePkShp , WaveDir   , WaveSeed , GHWvFile , &
                    CurrMod   , CurrSSV0  , CurrSSDir, CurrNSRef, &
                    CurrNSV0  , CurrNSDir , CurrDIV  , CurrDIDir, &
                    NWaveKin0 , WaveKinzi0, DHNodes  , NWaveElev, &
                    WaveElevxi, WaveElevyi, Gravity  , DirRoot      )


ELSEIF ( ( PtfmModel == 3 ) .AND. CompHydro )  THEN   ! .TRUE. if we have floating offshore turbine and we are using the undocumented platform features.


   ! Increase the value of WaveTMax to ensure that the maximum value within
   !   array WaveTime is at least as large as TMax+DT:

   IF ( ( WaveTMax - WaveDT ) < ( TMax + DT ) )  WaveTMax = TMax + WaveDT + DT   ! NOTE: The + WaveDT is needed because the internal array WaveTime has a maximum value of WaveTMax - WaveDT; the + DT is needed since the simulation time can reach TMax + DT in ADAMS.


   ! Determine the zi-coordinates for points along a vertical line passing
   !   through the platform reference point where the incident wave kinematics
   !   will be computed.  These are the vertical locations of the support
   !   platform nodes relative to the mean sea level when the support platform
   !   is undisplaced.  Also, determine the lengths of the elements
   !   corresponding to these nodes:

   NWaveKin0 = PtfmNodes

   ALLOCATE ( DZNodesPtfm(NWaveKin0) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the DZNodesPtfm array.')
   ENDIF

   ALLOCATE ( WaveKinzi0 (NWaveKin0) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the WaveKinzi0 array.')
   ENDIF

   DO J = 1,NWaveKin0   ! Loop through the platform nodes / elements
      DZNodesPtfm(J) = PtfmDraft/NWaveKin0   ! Lets used constant-spaced nodes for now, but the rest of the code is written to handle variable-spaced nodes--this will be a future input.
      IF ( J == 1 ) THEN   ! Lowest analysis point
         WaveKinzi0(J) = 0.5*DZNodesPtfm(J) - PtfmDraft
      ELSE                 ! All other analysis points
         WaveKinzi0(J) = WaveKinzi0( J - 1 ) + 0.5*( DZNodesPtfm(J) + DZNodesPtfm( J - 1 ) )
      ENDIF
   ENDDO                ! J - Platform nodes / elements


   ! Initialize the variables used in the undocumented time domain hydrodynamic
   !   loading and mooring system dynamics routines:

   CALL InitWaves       ( WtrDens   , WtrDpth   , WaveMod    , WaveStMod, &
                          WaveTMax  , WaveDT    , WaveHs     , WaveTp   , &
                          WavePkShp , WaveDir   , WaveSeed   , GHWvFile , &
                          CurrMod   , CurrSSV0  , CurrSSDir  , CurrNSRef, &
                          CurrNSV0  , CurrNSDir , CurrDIV    , CurrDIDir, &
                          NWaveKin0 , WaveKinzi0, DZNodesPtfm, NWaveElev, &
                          WaveElevxi, WaveElevyi, Gravity    , DirRoot      )
   CALL InitFltngPtfmLd ( WAMITFile , PtfmVol0  , PtfmDiam   , PtfmCD   , &
                          RdtnTMax  , RdtnDT    , NumLines   , LineMod  , &
                          LAnchxi   , LAnchyi   , LAnchzi    , LFairxt  , &
                          LFairyt   , LFairzt   , LUnstrLen  , LDiam    , &
                          LMassDen  , LEAStff   , LSeabedCD  , LTenTol  , &
                          LineNodes , LSNodes   , OtherState%Q(1:6,1)       )

   IF (ALLOCATED( DZNodesPtfm ) ) DEALLOCATE( DZNodesPtfm )
   IF (ALLOCATED( WaveKinzi0 ) )  DEALLOCATE( WaveKinzi0 )


ENDIF




RETURN
END SUBROUTINE FAST_Initialize
!=======================================================================
SUBROUTINE PtfmLoading(x)


   ! This routine computes the platform loading; that is PtfmAM(1:6,1:6)
   !   and PtfmFt(1:6).

USE                             FloatingPlatform, ONLY:FltngPtfmLd
USE                             General
USE                             Platform
USE                             RtHndSid
USE                             SimCont

IMPLICIT                        NONE


   ! passed variables

TYPE(StrD_ContinuousStateType),INTENT(IN)       :: x                          ! The structural dynamics module's continuous states

   ! Local variables:

REAL(ReKi), PARAMETER        :: SymTol   = 9.999E-4                             ! Tolerance used to determine if matrix PtfmAM is symmetric.

INTEGER(4)                   :: I                                               ! Loops through all platform DOFs.
INTEGER(4)                   :: J                                               ! Loops through all platform DOFs.



SELECT CASE ( PtfmModel )  ! Which platform model are we using?

CASE ( 0 )                 ! None!


   ! Do nothing here since PtfmAM and PtfmFt are all initialized to zero.


CASE ( 1 )                 ! Onshore.


   SELECT CASE ( PtfmLdMod )  ! Which platform loading model are we using?

   CASE ( 0 )                 ! None!


   ! Do nothing here since PtfmAM and PtfmFt are all initialized to zero.


   CASE ( 1 )                 ! User-defined platform loading.


   ! CALL the user-defined platform loading model:

      CALL UserPtfmLd ( x%QT(1:6), x%QDT(1:6), ZTime, DirRoot, PtfmAM, PtfmFt )


   ! Ensure that the platform added mass matrix returned by UserPtfmLd,
   !   PtfmAM, is symmetric; Abort if necessary:

      DO I = 1,5        ! Loop through the 1st 5 rows (columns) of PtfmAM

         DO J = (I+1),6 ! Loop through all columns (rows) passed I

            IF ( ABS( PtfmAM(I,J) - PtfmAM(J,I) ) > SymTol )  &
               CALL ProgAbort ( ' The user-defined platform added mass matrix is unsymmetric.'// &
                                '  Make sure PtfmAM returned by UserPtfmLd() is symmetric.'        )

         ENDDO          ! J - All columns (rows) passed I

      ENDDO             ! I - The 1st 5 rows (columns) of PtfmAM


   ENDSELECT


CASE ( 2 )                 ! Fixed bottom offshore.


   SELECT CASE ( PtfmLdMod )  ! Which platform loading model are we using?

   CASE ( 0 )                 ! None!


   ! Do nothing here since PtfmAM and PtfmFt are all initialized to zero.


   CASE ( 1 )                 ! User-defined platform loading.


   ! CALL the user-defined platform loading model:

      CALL UserPtfmLd ( x%QT(1:6), x%QDT(1:6), ZTime, DirRoot, PtfmAM, PtfmFt )


   ! Ensure that the platform added mass matrix returned by UserPtfmLd,
   !   PtfmAM, is symmetric; Abort if necessary:

      DO I = 1,5        ! Loop through the 1st 5 rows (columns) of PtfmAM

         DO J = (I+1),6 ! Loop through all columns (rows) passed I

            IF ( ABS( PtfmAM(I,J) - PtfmAM(J,I) ) > SymTol )  &
               CALL ProgAbort ( ' The user-defined platform added mass matrix is unsymmetric.'// &
                                '  Make sure PtfmAM returned by UserPtfmLd() is symmetric.'        )

         ENDDO          ! J - All columns (rows) passed I

      ENDDO             ! I - The 1st 5 rows (columns) of PtfmAM


   ENDSELECT


CASE ( 3 )                 ! Floating offshore.


   SELECT CASE ( PtfmLdMod )  ! Which platform loading model are we using?

   CASE ( 0 )                 ! None!


   ! Do nothing here since PtfmAM and PtfmFt are all initialized to zero.


   CASE ( 1 )                 ! User-defined platform loading.


   ! CALL the user-defined platform loading model:

      CALL UserPtfmLd ( x%QT(1:6), x%QDT(1:6), ZTime, DirRoot, PtfmAM, PtfmFt )


   ! Ensure that the platform added mass matrix returned by UserPtfmLd,
   !   PtfmAM, is symmetric; Abort if necessary:

      DO I = 1,5        ! Loop through the 1st 5 rows (columns) of PtfmAM

         DO J = (I+1),6 ! Loop through all columns (rows) passed I

            IF ( ABS( PtfmAM(I,J) - PtfmAM(J,I) ) > SymTol )  &
               CALL ProgAbort ( ' The user-defined platform added mass matrix is unsymmetric.'// &
                                '  Make sure PtfmAM returned by UserPtfmLd() is symmetric.'        )

         ENDDO          ! J - All columns (rows) passed I

      ENDDO             ! I - The 1st 5 rows (columns) of PtfmAM


   CASE ( 9999 )              ! Undocumented loading for a floating platform.


   ! CALL the undocumented time domain hydrodynamic loading and mooring system
   !   dynamics routine:

      CALL FltngPtfmLd ( x%QT(1:6), x%QDT(1:6), ZTime, PtfmAM, PtfmFt )


   ENDSELECT


ENDSELECT

RETURN
END SUBROUTINE PtfmLoading
!=======================================================================
SUBROUTINE RFurling( RFrlDef, RFrlRate, RFrlMom )


   ! This routine computes the rotor-furl moment due to rotor-furl deflection
   !   and rate.


USE                             General
USE                             SimCont
USE                             RotorFurling


IMPLICIT                        NONE


   ! Passed Variables:

REAL(ReKi), INTENT(IN )      :: RFrlDef                                         ! The rotor-furl deflection, x%QT(DOF_RFrl).
REAL(ReKi), INTENT(OUT)      :: RFrlMom                                         ! The total moment supplied by the springs, and dampers.
REAL(ReKi), INTENT(IN )      :: RFrlRate                                        ! The rotor-furl rate, x%QDT(DOF_RFrl).


   ! Local variables:

REAL(ReKi)                   :: RFrlDMom                                        ! The moment supplied by the rotor-furl dampers.
REAL(ReKi)                   :: RFrlSMom                                        ! The moment supplied by the rotor-furl springs.



SELECT CASE ( RFrlMod ) ! Which rotor-furl model are we using?

CASE ( 0 )              ! None!


   RFrlMom = 0.0


CASE ( 1 )              ! Standard (using inputs from the FAST furling input file).


   ! Linear spring:

   RFrlSMom = -RFrlSpr*RFrlDef


   ! Add spring-stops:

   IF ( RFrlDef > RFrlUSSP )  THEN       ! Up-stop
      RFrlSMom = RFrlSMom - RFrlUSSpr*( RFrlDef - RFrlUSSP )
   ELSEIF ( RFrlDef < RFrlDSSP )  THEN   ! Down-stop
      RFrlSMom = RFrlSMom - RFrlDSSpr*( RFrlDef - RFrlDSSP )
   ENDIF


   ! Linear damper:

   RFrlDMom = -RFrlDmp*RFrlRate


   ! Add coulomb friction:

   IF ( RFrlRate /= 0.0 )  THEN
      RFrlDMom = RFrlDMom - SIGN( RFrlCDmp, RFrlRate )
   ENDIF


   ! Add damper-stops:

   IF ( RFrlDef > RFrlUSDP )  THEN       ! Up-stop
      RFrlDMom = RFrlDMom - RFrlUSDmp*RFrlRate
   ELSEIF ( RFrlDef < RFrlDSDP )  THEN   ! Down-stop
      RFrlDMom = RFrlDMom - RFrlDSDmp*RFrlRate
   ENDIF


   ! Total up all the moments.

   RFrlMom = RFrlSMom + RFrlDMom


CASE ( 2 )              ! User-defined rotor-furl spring/damper model.


   CALL UserRFrl ( RFrlDef, RFrlRate, ZTime, DirRoot, RFrlMom )


ENDSELECT



RETURN
END SUBROUTINE RFurling
!=======================================================================
SUBROUTINE RtHS( p, CoordSys, x, OtherState )


   ! This routine is used to set up and solve the equations of motion
   !   for a particular time step.


USE                             AeroElem
USE                             Blades
USE                             DriveTrain
USE                             EnvCond
USE                             Features
USE                             General
USE                             InitCond
USE                             MassInert
USE                             NacelleYaw
USE                             Output
USE                             Platform
USE                             RtHndSid
USE                             SimCont
USE                             TailAero
USE                             TipBrakes
USE                             Tower
USE                             TurbCont


IMPLICIT                        NONE


   ! Passed variables

TYPE(StrD_ParameterType),  INTENT(IN)           :: p                            ! The parameters of the structural dynamics module
TYPE(StrD_CoordSys), INTENT(INOUT)              :: CoordSys                     ! The coordinate systems to be set
TYPE(StrD_ContinuousStateType),INTENT(INOUT)    :: x                            ! The structural dynamics module's continuous states
TYPE(StrD_OtherStateType),INTENT(INOUT)         :: OtherState                        ! Other State data type for Structural dynamics module


   ! Local variables:

REAL(ReKi)                   :: AngAccEAt (3)                                   ! Portion of the angular acceleration of the tail                                                      (body A) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.
REAL(ReKi)                   :: AngAccEGt (3)                                   ! Portion of the angular acceleration of the generator                                                 (body G) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.
REAL(ReKi)                   :: AngAccEHt (3)                                   ! Portion of the angular acceleration of the hub                                                       (body H) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.
REAL(ReKi)                   :: AngAccELt (3)                                   ! Portion of the angular acceleration of the low-speed shaft                                           (body L) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.
REAL(ReKi)                   :: AngAccENt (3)                                   ! Portion of the angular acceleration of the nacelle                                                   (body N) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.

REAL(ReKi)                   :: AngPosEX  (3)                                   ! Angular position of the platform                   (body X) in the inertial frame (body E for earth).
REAL(ReKi)                   :: AngVelEA  (3)                                   ! Angular velocity of the tail                                                      (body A) in the inertia frame (body E for earth).
REAL(ReKi)                   :: AngVelEG  (3)                                   ! Angular velocity of the generator                                                 (body G) in the inertia frame (body E for earth).
REAL(ReKi)                   :: AngVelEH  (3)                                   ! Angular velocity of the hub                                                       (body H) in the inertia frame (body E for earth).
REAL(ReKi)                   :: AngVelEL  (3)                                   ! Angular velocity of the low-speed shaft                                           (body L) in the inertia frame (body E for earth).
REAL(ReKi)                   :: AngVelEN  (3)                                   ! Angular velocity of the nacelle                                                   (body N) in the inertia frame (body E for earth).
REAL(ReKi)                   :: ElmntMass                                       ! (Temporary) mass of an element.
REAL(ReKi)                   :: EwAXrWI   (3)                                   ! = AngVelEA X rWI
REAL(ReKi)                   :: EwAXrWJ   (3)                                   ! = AngVelEA X rWJ
REAL(ReKi)                   :: EwAXrWK   (3)                                   ! = AngVelEA X rWK
REAL(ReKi)                   :: EwHXrPQ   (3)                                   ! = AngVelEH X rPQ
REAL(ReKi)                   :: EwHXrQC   (3)                                   ! = AngVelEH X rQC
REAL(ReKi)                   :: EwHXrQS   (3)                                   ! = AngVelEH X rQS of the current blade point S.
REAL(ReKi)                   :: EwNXrOU   (3)                                   ! = AngVelEN X rOU
REAL(ReKi)                   :: EwNXrOV   (3)                                   ! = AngVelEN X rOV
REAL(ReKi)                   :: EwNXrOW   (3)                                   ! = AngVelEN X rOW
REAL(ReKi)                   :: EwRXrVD   (3)                                   ! = AngVelER X rVD
REAL(ReKi)                   :: EwRXrVIMU (3)                                   ! = AngVelER X rVIMU
REAL(ReKi)                   :: EwRXrVP   (3)                                   ! = AngVelER X rVP
REAL(ReKi)                   :: EwXXrZO   (3)                                   ! = AngVelEX X rZO
REAL(ReKi)                   :: EwXXrZT   (3)                                   ! = AngVelEX X rZT
REAL(ReKi)                   :: EwXXrZY   (3)                                   ! = AngVelEX X rZY
REAL(ReKi)                   :: GBoxEffFac2                                     ! A second gearbox efficiency factor = ( 1 / GBoxEff^SgnPrvLSTQ - 1 )
REAL(ReKi)                   :: GBoxTrq                                         ! Gearbox torque on the LSS side in N-m.
REAL(ReKi)                   :: LinAccECt (3)                                   ! Portion of the linear acceleration of the hub center of mass                                                              (point C) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.
REAL(ReKi)                   :: LinAccEDt (3)                                   ! Portion of the linear acceleration of the center of mass of the structure that furls with the rotor (not including rotor) (point D) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.
REAL(ReKi)                   :: LinAccEIt (3)                                   ! Portion of the linear acceleration of the tail boom center of mass                                                        (point I) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.
REAL(ReKi)                   :: LinAccEJt (3)                                   ! Portion of the linear acceleration of the tail fin  center of mass                                                        (point J) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.
REAL(ReKi)                   :: LinAccEKt (3)                                   ! Portion of the linear acceleration of the tail fin  center of pressure                                                    (point K) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.
REAL(ReKi)                   :: LinAccEPt (3)                                   ! Portion of the linear acceleration of the teeter pin                                                                      (point P) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.
REAL(ReKi)                   :: LinAccEQt (3)                                   ! Portion of the linear acceleration of the apex of rotation                                                                (point Q) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.
REAL(ReKi)                   :: LinAccEUt (3)                                   ! Portion of the linear acceleration of the nacelle center of mass                                                          (point U) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.
REAL(ReKi)                   :: LinAccEVt (3)                                   ! Portion of the linear acceleration of the selected point on the rotor-furl axis                                           (point V) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.
REAL(ReKi)                   :: LinAccEWt (3)                                   ! Portion of the linear acceleration of the selected point on the  tail-furl axis                                           (point W) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.
REAL(ReKi)                   :: LinAccEYt (3)                                   ! Portion of the linear acceleration of the platform center of mass                                                         (point Y) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.
REAL(ReKi)                   :: LinVelEK  (3)                                   ! Linear velocity of tail fin center-of-pressure        (point K) in the inertia frame.
REAL(ReKi)                   :: LinVelES  (3)                                   ! Linear velocity of current point on the current blade (point S) in the inertia frame.
REAL(ReKi)                   :: LinVelEPYaw(3)                                  ! This is the linear velocity of the hub in the inertia frame due solely to yaw and rotor-furl effects
REAL(ReKi)                   :: LinVelHS  (3)                                   ! Relative linear velocity of the current point on the current blade (point S) in the hub frame (body H)
REAL(ReKi)                   :: LinVelXO  (3)                                   ! Relative linear velocity of the tower-top / base plate             (point O) in the platform  (body X).
REAL(ReKi)                   :: LinVelXT  (3)                                   ! Relative linear velocity of the current point on the tower         (point T) in the platform  (body X).
REAL(ReKi)                   :: MomLPRot  (3)                                   ! The total moment on the low-speed shaft at point P caused by the rotor.
REAL(ReKi)                   :: PLinVelHS (3,3)                                 ! Partial  linear velocity of the current point on the current blade (point S) in the hub frame (body H) (this is like a relative partial linear velocity).
REAL(ReKi)                   :: rAerCen   (3)                                   ! Position vector from inertial frame origin to current blade analysis node aerodynamic center.
REAL(ReKi)                   :: RFrlMom                                         ! The total rotor-furl spring and damper moment.
REAL(ReKi)                   :: rK        (3)                                   ! Position vector from inertial frame origin to tail fin center of pressure (point K).
REAL(ReKi)                   :: rOU       (3)                                   ! Position vector from tower-top / base plate (point O) to nacelle center of mass (point U).
REAL(ReKi)                   :: rOV       (3)                                   ! Position vector from tower-top / base plate (point O) to specified point on rotor-furl axis (point V).
REAL(ReKi)                   :: rOW       (3)                                   ! Position vector from tower-top / base plate (point O) to specified point on  tail-furl axis (point W).
REAL(ReKi)                   :: rP        (3)                                   ! Position vector from inertial frame origin to teeter pin (point P).
REAL(ReKi)                   :: rPAerCen  (3)                                   ! Position vector from teeter pin (point P) to current blade analysis node aerodynamic center.
REAL(ReKi)                   :: rPC       (3)                                   ! Position vector from teeter pin (point P) to hub center of mass (point C).
REAL(ReKi)                   :: rPQ       (3)                                   ! Position vector from teeter pin (point P) to apex of rotation (point Q).
REAL(ReKi)                   :: rPS0      (3)                                   ! Position vector from teeter pin (point P) to blade root (point S(0)).
REAL(ReKi)                   :: rQ        (3)                                   ! Position vector from inertial frame origin to apex of rotation (point Q).
REAL(ReKi)                   :: rQC       (3)                                   ! Position vector from apex of rotation (point Q) to hub center of mass (point C).
REAL(ReKi)                   :: rV        (3)                                   ! Position vector from inertial frame origin to specified point on rotor-furl axis (point V).
REAL(ReKi)                   :: rVD       (3)                                   ! Position vector from specified point on rotor-furl axis (point V) to center of mass of structure that furls with the rotor (not including rotor) (point D).
REAL(ReKi)                   :: rVIMU     (3)                                   ! Position vector from specified point on rotor-furl axis (point V) to nacelle IMU (point IMU).
REAL(ReKi)                   :: rVP       (3)                                   ! Position vector from specified point on rotor-furl axis (point V) to teeter pin (point P).
REAL(ReKi)                   :: rWI       (3)                                   ! Position vector from specified point on  tail-furl axis (point W) to tail boom center of mass     (point I).
REAL(ReKi)                   :: rWJ       (3)                                   ! Position vector from specified point on  tail-furl axis (point W) to tail fin  center of mass     (point J).
REAL(ReKi)                   :: rWK       (3)                                   ! Position vector from specified point on  tail-furl axis (point W) to tail fin  center of pressure (point K).
REAL(ReKi)                   :: rSAerCen  (3)                                   ! Position vector from a blade analysis node (point S) on the current blade to the aerodynamic center associated with the element.

REAL(ReKi)                   :: rZT0      (3)                                   ! Position vector from platform reference (point Z) to tower base (point T(0)).
REAL(ReKi)                   :: rZY       (3)                                   ! Position vector from platform reference (point Z) to platform mass center (point Y).
REAL(ReKi)                   :: TeetMom                                         ! The total moment supplied by the stop, spring, and damper of the teeter mechanism.
REAL(ReKi)                   :: TFrlMom                                         ! The total tail-furl spring and damper moment.
REAL(ReKi)                   :: TmpVec    (3)                                   ! A temporary vector used in various computations.
REAL(ReKi)                   :: TmpVec0   (3)                                   ! A temporary vector used in various computations.
REAL(ReKi)                   :: TmpVec1   (3)                                   ! A temporary vector used in various computations.
REAL(ReKi)                   :: TmpVec2   (3)                                   ! A temporary vector used in various computations.
REAL(ReKi)                   :: TmpVec3   (3)                                   ! A temporary vector used in various computations.
REAL(ReKi)                   :: TmpVec4   (3)                                   ! A temporary vector used in various computations.
REAL(ReKi)                   :: TmpVec5   (3)                                   ! A temporary vector used in various computations.

INTEGER(4)                   :: I                                               ! Loops through some or all of the DOFs.
INTEGER(4)                   :: J                                               ! Loops through nodes / elements.
INTEGER(4)                   :: K                                               ! Loops through blades.
INTEGER(4)                   :: L                                               ! Generic index
INTEGER(4), SAVE             :: SgnPrvLSTQ = 1                                  ! The sign of the low-speed shaft torque from the previous call to RtHS().  This is calculated at the end of RtHS().  NOTE: The low-speed shaft torque is assumed to be positive at the beginning of the run!

INTEGER                      :: ErrStat




   ! Determine how many DOFs are currently enabled (this can change within
   !   user-defined routines) and set vector subscript arrays accordingly:

CALL SetEnabledDOFIndexArrays( p, OtherState%DOFs )



   ! Control the turbine's yaw and pitch, except during the first time step and
   !   only during a time-marching analysis (we can't call Control during the
   !   first time step since none of the output parameters needed for feedback
   !   of control measurements are computed until the end of the first time
   !   step):

IF ( ( ZTime > 0.0 ) .AND. ( AnalMode == 1 ) )  CALL Control( p, x, OtherState, CoordSys%b1 ) !bjj: note that b1 hasn't been set yet when the simulation starts....



   ! Initialize several variables to 0.0:

AugMat      = 0.0


LinAccECt   = 0.0
LinAccEDt   = 0.0
LinAccEIMUt = 0.0
LinAccEIt   = 0.0
LinAccEJt   = 0.0
LinAccEKt   = 0.0
LinAccEOt   = 0.0
LinAccEPt   = 0.0
LinAccEQt   = 0.0
LinAccESt   = 0.0
LinAccETt   = 0.0
LinAccEUt   = 0.0
LinAccEVt   = 0.0
LinAccEWt   = 0.0
LinAccEYt   = 0.0
LinAccEZt   = 0.0



   ! Let's define the coordinate systems that will be used throughout this routine:

CALL SetCoordSy( CoordSys, p, x )

   !-------------------------------------------------------------------------------------------------
   ! Positions
   !-------------------------------------------------------------------------------------------------

   ! Define the position vectors between the various points on the wind turbine
   !   that are not dependent on the distributed tower or blade parameters:

rZ    = x%QT(DOF_Sg)* CoordSys%z1 + x%QT(DOF_Hv)* CoordSys%z2 - x%QT(DOF_Sw)* CoordSys%z3                   ! Position vector from inertia frame origin to platform reference (point Z).
rZY   =        p%rZYzt* CoordSys%a2                                                                           ! Position vector from platform reference (point Z) to platform mass center (point Y).
rZT0  =       p%rZT0zt* CoordSys%a2                                                                           ! Position vector from platform reference (point Z) to tower base (point T(0))
rZO   = ( x%QT(DOF_TFA1) + x%QT(DOF_TFA2)                                             )*CoordSys%a1 &       ! Position vector from platform reference (point Z) to tower-top / base plate (point O).
      + ( p%RefTwrHt - 0.5*(       AxRedTFA(1,1,TTopNode)*x%QT(DOF_TFA1)*x%QT(DOF_TFA1) &
                           +     AxRedTFA(2,2,TTopNode)*x%QT(DOF_TFA2)*x%QT(DOF_TFA2) &
                           + 2.0*AxRedTFA(1,2,TTopNode)*x%QT(DOF_TFA1)*x%QT(DOF_TFA2) &
                           +     AxRedTSS(1,1,TTopNode)*x%QT(DOF_TSS1)*x%QT(DOF_TSS1) &
                           +     AxRedTSS(2,2,TTopNode)*x%QT(DOF_TSS2)*x%QT(DOF_TSS2) &
                           + 2.0*AxRedTSS(1,2,TTopNode)*x%QT(DOF_TSS1)*x%QT(DOF_TSS2)   ) )*CoordSys%a2 &
      + ( x%QT(DOF_TSS1) + x%QT(DOF_TSS2)                                                 )*CoordSys%a3
rOU   =    p%NacCMxn*CoordSys%d1  +  p%NacCMzn*  CoordSys%d2  -  p%NacCMyn  *CoordSys%d3                          ! Position vector from tower-top / base plate (point O) to nacelle center of mass (point U).
rOV   =  p%RFrlPntxn*CoordSys%d1  +  p%RFrlPntzn*CoordSys%d2  -  p%RFrlPntyn*CoordSys%d3                          ! Position vector from tower-top / base plate (point O) to specified point on rotor-furl axis (point V).
rVIMU =    p%rVIMUxn*CoordSys%rf1 +  p%rVIMUzn  *CoordSys%rf2 -   p%rVIMUyn *CoordSys%rf3                         ! Position vector from specified point on rotor-furl axis (point V) to nacelle IMU (point IMU).
rVD   =      p%rVDxn*CoordSys%rf1 +    p%rVDzn  *CoordSys%rf2 -     p%rVDyn *CoordSys%rf3                         ! Position vector from specified point on rotor-furl axis (point V) to center of mass of structure that furls with the rotor (not including rotor) (point D).
rVP   =      p%rVPxn*CoordSys%rf1 +    p%rVPzn  *CoordSys%rf2 -     p%rVPyn *CoordSys%rf3 + p%OverHang*CoordSys%c1  ! Position vector from specified point on rotor-furl axis (point V) to teeter pin (point P).
rPQ   =  -p%UndSling*CoordSys%g1                                                                              ! Position vector from teeter pin (point P) to apex of rotation (point Q).
rQC   =      p%HubCM*CoordSys%g1                                                                              ! Position vector from apex of rotation (point Q) to hub center of mass (point C).
rOW   =  p%TFrlPntxn*CoordSys%d1  + p%TFrlPntzn* CoordSys%d2 -  p%TFrlPntyn*CoordSys%d3                          ! Position vector from tower-top / base plate (point O) to specified point on  tail-furl axis (point W).
rWI   =    p%rWIxn*CoordSys%tf1 +       p%rWIzn* CoordSys%tf2 -     p%rWIyn*CoordSys%tf3                         ! Position vector from specified point on  tail-furl axis (point W) to tail boom center of mass     (point I).
rWJ   =    p%rWJxn*CoordSys%tf1 +       p%rWJzn* CoordSys%tf2 -     p%rWJyn*CoordSys%tf3                         ! Position vector from specified point on  tail-furl axis (point W) to tail fin  center of mass     (point J).
rWK   =    p%rWKxn*CoordSys%tf1 +       p%rWKzn* CoordSys%tf2 -     p%rWKyn*CoordSys%tf3                         ! Position vector from specified point on  tail-furl axis (point W) to tail fin  center of pressure (point K).
rPC   = rPQ + rQC                                                                                           ! Position vector from teeter pin (point P) to hub center of mass (point C).
rT0O  = rZO - rZT0                                                                                          ! Position vector from the tower base (point T(0)) to tower-top / base plate (point O).
rO    = rZ  + rZO                                                                                           ! Position vector from inertial frame origin to tower-top / base plate (point O).
rV    = rO  + rOV                                                                                           ! Position vector from inertial frame origin to specified point on rotor-furl axis (point V)
!rP    = rO  + rOV + rVP                                                                                    ! Position vector from inertial frame origin to teeter pin (point P).
rP    = rV  + rVP                                                                                           ! Position vector from inertial frame origin to teeter pin (point P).
rQ    = rP  + rPQ                                                                                           ! Position vector from inertial frame origin to apex of rotation (point Q).
rK    = rO  + rOW + rWK                                                                                     ! Position vector from inertial frame origin to tail fin center of pressure (point K).


DO K = 1,p%NumBl ! Loop through all blades



   ! Calculate the position vector of the tip:

   rS0S(K,p%TipNode,:) = (   TwistedSF(K,1,1,p%TipNode,0)*x%QT( DOF_BF(K,1) ) &  ! Position vector from the blade root (point S(0)) to the blade tip (point S(BldFlexL)).
                         + TwistedSF(K,1,2,p%TipNode,0)*x%QT( DOF_BF(K,2) ) &
                         + TwistedSF(K,1,3,p%TipNode,0)*x%QT( DOF_BE(K,1) )                          )*CoordSys%j1(K,:) &
                     + (   TwistedSF(K,2,1,p%TipNode,0)*x%QT( DOF_BF(K,1) ) &
                         + TwistedSF(K,2,2,p%TipNode,0)*x%QT( DOF_BF(K,2) ) &
                         + TwistedSF(K,2,3,p%TipNode,0)*x%QT( DOF_BE(K,1) )                          )*CoordSys%j2(K,:) &
                     + ( BldFlexL - 0.5* &
                         (       AxRedBld(K,1,1,p%TipNode)*x%QT( DOF_BF(K,1) )*x%QT( DOF_BF(K,1) ) &
                           +     AxRedBld(K,2,2,p%TipNode)*x%QT( DOF_BF(K,2) )*x%QT( DOF_BF(K,2) ) &
                           +     AxRedBld(K,3,3,p%TipNode)*x%QT( DOF_BE(K,1) )*x%QT( DOF_BE(K,1) ) &
                           + 2.0*AxRedBld(K,1,2,p%TipNode)*x%QT( DOF_BF(K,1) )*x%QT( DOF_BF(K,2) ) &
                           + 2.0*AxRedBld(K,2,3,p%TipNode)*x%QT( DOF_BF(K,2) )*x%QT( DOF_BE(K,1) ) &
                           + 2.0*AxRedBld(K,1,3,p%TipNode)*x%QT( DOF_BF(K,1) )*x%QT( DOF_BE(K,1) )   ) )*CoordSys%j3(K,:)
   rQS (K,p%TipNode,:) = rS0S(K,p%TipNode,:) + p%HubRad*CoordSys%j3(K,:)                  ! Position vector from apex of rotation (point Q) to the blade tip (point S(BldFlexL)).
   rS  (K,p%TipNode,:) = rQS (K,p%TipNode,:) + rQ                              ! Position vector from inertial frame origin      to the blade tip (point S(BldFlexL)).


   DO J = 1,p%BldNodes ! Loop through the blade nodes / elements


   ! Calculate the position vector of the current node:

      rS0S(K,J,:) = (   TwistedSF(K,1,1,J,0)*x%QT( DOF_BF(K,1) ) &  ! Position vector from the blade root (point S(0)) to the current node (point S(RNodes(J)).
                      + TwistedSF(K,1,2,J,0)*x%QT( DOF_BF(K,2) ) &
                      + TwistedSF(K,1,3,J,0)*x%QT( DOF_BE(K,1) )                          )*CoordSys%j1(K,:) &
                  + (   TwistedSF(K,2,1,J,0)*x%QT( DOF_BF(K,1) ) &
                      + TwistedSF(K,2,2,J,0)*x%QT( DOF_BF(K,2) ) &
                      + TwistedSF(K,2,3,J,0)*x%QT( DOF_BE(K,1) )                          )*CoordSys%j2(K,:) &
                  + ( RNodes(J) - 0.5* &
                      (       AxRedBld(K,1,1,J)*x%QT( DOF_BF(K,1) )*x%QT( DOF_BF(K,1) ) &
                        +     AxRedBld(K,2,2,J)*x%QT( DOF_BF(K,2) )*x%QT( DOF_BF(K,2) ) &
                        +     AxRedBld(K,3,3,J)*x%QT( DOF_BE(K,1) )*x%QT( DOF_BE(K,1) ) &
                        + 2.0*AxRedBld(K,1,2,J)*x%QT( DOF_BF(K,1) )*x%QT( DOF_BF(K,2) ) &
                        + 2.0*AxRedBld(K,2,3,J)*x%QT( DOF_BF(K,2) )*x%QT( DOF_BE(K,1) ) &
                        + 2.0*AxRedBld(K,1,3,J)*x%QT( DOF_BF(K,1) )*x%QT( DOF_BE(K,1) )   ) )*CoordSys%j3(K,:)
      rQS (K,J,:) = rS0S(K,J,:) + p%HubRad*CoordSys%j3(K,:)                  ! Position vector from apex of rotation (point Q) to the current node (point S(RNodes(J)).
      rS  (K,J,:) = rQS (K,J,:) + rQ                              ! Position vector from inertial frame origin      to the current node (point S(RNodes(J)).

      IF ( CompAero )  THEN   ! Calculate the blade element aerodynamic loads using AeroDyn.


   ! Calculate the aerodynamic pitching moment arm (i.e., the position vector
   !   from point S on the blade to the aerodynamic center of the element):

         rSAerCen = rSAerCenn1(K,J)*CoordSys%n1(K,J,:) + rSAerCenn2(K,J)*CoordSys%n2(K,J,:) !bjj: make rSAerCen a matrix? we recalculate it later


   ! Define positions USEd by AeroDyn.

         rPAerCen     = rPQ + rQS(K,J,:) + rSAerCen         ! Position vector from teeter pin (point P)  to blade analysis node aerodynamic center.
         rAerCen      =       rS (K,J,:) + rSAerCen         ! Position vector from inertial frame origin to blade analysis node aerodynamic center.

         ADAeroMarkers%Blade(J,K)%Position(1)      =     rAerCen(1)              ! = the distance from the undeflected tower centerline                                     to the current blade aerodynamic center in the xi ( z1) direction
         ADAeroMarkers%Blade(J,K)%Position(2)      = -1.*rAerCen(3)              ! = the distance from the undeflected tower centerline                                     to the current blade aerodynamic center in the yi (-z3) direction
         ADAeroMarkers%Blade(J,K)%Position(3)      =     rAerCen(2) - p%PtfmRef  ! = the distance from the nominal tower base position (i.e., the undeflected position of the tower base) to the current blade aerodynamic center in the zi ( z2) direction


!JASON: WE SHOULD REALLY BE PASSING TO AERODYN THE LINEAR VELOCITIES OF THE AERODYNAMIC CENTER IN THE INERTIA FRAME, NOT SIMPLY THE LINEAR VELOCITIES OF POINT S.  IS THERE ANY WAY OF GETTING THIS VELOCITY?<--DO THIS, WHEN YOU ADD THE COUPLED MODE SHAPES!!!!
      END IF  ! CompAero

   END DO !J = 1,p%BldNodes ! Loop through the blade nodes / elements

END DO !K = 1,p%NumBl


   ! the hub position should use rQ instead of rP, but the current version of AeroDyn treats
   ! teeter deflections like blade deflections:


ADInterfaceComponents%Hub%Position(:)       = (/ rP(1), -1.*rP(3), rP(2) - p%PtfmRef /)


   ! Rotor furl position should be rP instead of rV, but AeroDyn needs this for the
   ! HubVDue2Yaw calculation:

ADInterfaceComponents%RotorFurl%Position(:) = (/ rV(1), -1.*rV(3), rV(2) - p%PtfmRef /)

ADInterfaceComponents%Nacelle%Position(:)   = (/ rO(1), -1.*rO(3), rO(2) - p%PtfmRef /)

   ! Tower base position should be rT(0) instead of rZ, but AeroDyn needs this for
   ! the HubVDue2Yaw calculation:
ADInterfaceComponents%Tower%Position(:)     = (/ rZ(1), -1.*rZ(3), rZ(2) - p%PtfmRef /)


   !-------------------------------------------------------------------------------------------------
   ! Orientations - bjj: should this be moved to SetCoordSys ?
   !-------------------------------------------------------------------------------------------------

DO K = 1,p%NumBl
   DO J = 1,p%BldNodes

      ADAeroMarkers%Blade(J,K)%Orientation(1,1) =     CoordSys%te1(K,J,1)
      ADAeroMarkers%Blade(J,K)%Orientation(2,1) =     CoordSys%te2(K,J,1)
      ADAeroMarkers%Blade(J,K)%Orientation(3,1) =     CoordSys%te3(K,J,1)
      ADAeroMarkers%Blade(J,K)%Orientation(1,2) = -1.*CoordSys%te1(K,J,3)
      ADAeroMarkers%Blade(J,K)%Orientation(2,2) = -1.*CoordSys%te2(K,J,3)
      ADAeroMarkers%Blade(J,K)%Orientation(3,2) = -1.*CoordSys%te3(K,J,3)
      ADAeroMarkers%Blade(J,K)%Orientation(1,3) =     CoordSys%te1(K,J,2)
      ADAeroMarkers%Blade(J,K)%Orientation(2,3) =     CoordSys%te2(K,J,2)
      ADAeroMarkers%Blade(J,K)%Orientation(3,3) =     CoordSys%te3(K,J,2)

   END DO !J = 1,p%BldNodes ! Loop through the blade nodes / elements
END DO !K = 1,p%NumBl



      ! Blade root orientations should use the j instead of i system, but the current version
      ! of AeroDyn calculates forces normal and tangential to the cone of rotation

ADInterfaceComponents%Blade(:)%Orientation(1,1) =     CoordSys%i1(:,1)
ADInterfaceComponents%Blade(:)%Orientation(2,1) =     CoordSys%i2(:,1)
ADInterfaceComponents%Blade(:)%Orientation(3,1) =     CoordSys%i3(:,1)
ADInterfaceComponents%Blade(:)%Orientation(1,2) = -1.*CoordSys%i1(:,3)
ADInterfaceComponents%Blade(:)%Orientation(2,2) = -1.*CoordSys%i2(:,3)
ADInterfaceComponents%Blade(:)%Orientation(3,2) = -1.*CoordSys%i3(:,3)
ADInterfaceComponents%Blade(:)%Orientation(1,3) =     CoordSys%i1(:,2)
ADInterfaceComponents%Blade(:)%Orientation(2,3) =     CoordSys%i2(:,2)
ADInterfaceComponents%Blade(:)%Orientation(3,3) =     CoordSys%i3(:,2)

     ! Hub orientation should use the g instead of e system, but the current version
     ! of AeroDyn calculates forces normal and tangential to the cone of rotation

ADInterfaceComponents%Hub%Orientation(:,1)       =     (/ CoordSys%e1(1), CoordSys%e2(1), CoordSys%e3(1) /)
ADInterfaceComponents%Hub%Orientation(:,2)       = -1.*(/ CoordSys%e1(3), CoordSys%e2(3), CoordSys%e3(3) /)
ADInterfaceComponents%Hub%Orientation(:,3)       =     (/ CoordSys%e1(2), CoordSys%e2(2), CoordSys%e3(2) /)

     ! Rotor furl orientation (note the different order than hub and blade root!)

ADInterfaceComponents%RotorFurl%Orientation(:,1) = (/      CoordSys%c1(1), -1.*CoordSys%c3(1),     CoordSys%c2(1) /)
ADInterfaceComponents%RotorFurl%Orientation(:,2) = (/ -1.* CoordSys%c1(3),     CoordSys%c3(3), -1.*CoordSys%c2(3) /)
ADInterfaceComponents%RotorFurl%Orientation(:,3) = (/      CoordSys%c1(2), -1.*CoordSys%c3(2),     CoordSys%c2(2) /)

      ! Nacelle orientation (note the different order than hub and blade root!)

ADInterfaceComponents%Nacelle%Orientation(:,1) = (/      CoordSys%d1(1), -1.*CoordSys%d3(1),     CoordSys%d2(1) /)
ADInterfaceComponents%Nacelle%Orientation(:,2) = (/ -1.* CoordSys%d1(3),     CoordSys%d3(3), -1.*CoordSys%d2(3) /)
ADInterfaceComponents%Nacelle%Orientation(:,3) = (/      CoordSys%d1(2), -1.*CoordSys%d3(2),     CoordSys%d2(2) /)



   !-------------------------------------------------------------------------------------------------
   ! Angular and partial angular velocities
   !-------------------------------------------------------------------------------------------------

   ! Define the angular and partial angular velocities of all of the rigid
   !   bodies in the inertia frame:
   ! NOTE: PAngVelEN(I,D,:) = the Dth-derivative of the partial angular velocity
   !   of DOF I for body N in body E.

PAngVelEX(       :,0,:) = 0.0
PAngVelEX(DOF_R   ,0,:) =  CoordSys%z1
PAngVelEX(DOF_P   ,0,:) = -CoordSys%z3
PAngVelEX(DOF_Y   ,0,:) =  CoordSys%z2
 AngVelEX               =             x%QDT(DOF_R   )*PAngVelEX(DOF_R   ,0,:) &
                                    + x%QDT(DOF_P   )*PAngVelEX(DOF_P   ,0,:) &
                                    + x%QDT(DOF_Y   )*PAngVelEX(DOF_Y   ,0,:)
 AngPosEX               =             x%QT (DOF_R   )*PAngVelEX(DOF_R   ,0,:) &
                                    + x%QT (DOF_P   )*PAngVelEX(DOF_P   ,0,:) &
                                    + x%QT (DOF_Y   )*PAngVelEX(DOF_Y   ,0,:)

PAngVelEB(       :,0,:) = PAngVelEX(:,0,:)
PAngVelEB(DOF_TFA1,0,:) = -TwrFASF(1,TTopNode,1)*CoordSys%a3
PAngVelEB(DOF_TSS1,0,:) =  TwrSSSF(1,TTopNode,1)*CoordSys%a1
PAngVelEB(DOF_TFA2,0,:) = -TwrFASF(2,TTopNode,1)*CoordSys%a3
PAngVelEB(DOF_TSS2,0,:) =  TwrSSSF(2,TTopNode,1)*CoordSys%a1
 AngVelEB               =  AngVelEX + x%QDT(DOF_TFA1)*PAngVelEB(DOF_TFA1,0,:) &
                                    + x%QDT(DOF_TSS1)*PAngVelEB(DOF_TSS1,0,:) &
                                    + x%QDT(DOF_TFA2)*PAngVelEB(DOF_TFA2,0,:) &
                                    + x%QDT(DOF_TSS2)*PAngVelEB(DOF_TSS2,0,:)
 AngPosXB               =             x%QT (DOF_TFA1)*PAngVelEB(DOF_TFA1,0,:) &
                                    + x%QT (DOF_TSS1)*PAngVelEB(DOF_TSS1,0,:) &
                                    + x%QT (DOF_TFA2)*PAngVelEB(DOF_TFA2,0,:) &
                                    + x%QT (DOF_TSS2)*PAngVelEB(DOF_TSS2,0,:)

PAngVelEN(       :,0,:) = PAngVelEB(:,0,:)
PAngVelEN(DOF_Yaw ,0,:) =  CoordSys%d2
 AngVelEN               =  AngVelEB + x%QDT(DOF_Yaw )*PAngVelEN(DOF_Yaw ,0,:)

PAngVelER(       :,0,:) = PAngVelEN(:,0,:)
PAngVelER(DOF_RFrl,0,:) = CoordSys%rfa
 AngVelER               =  AngVelEN + x%QDT(DOF_RFrl)*PAngVelER(DOF_RFrl,0,:)

PAngVelEL(       :,0,:) = PAngVelER(:,0,:)
PAngVelEL(DOF_GeAz,0,:) =  CoordSys%c1
PAngVelEL(DOF_DrTr,0,:) =  CoordSys%c1
 AngVelEL               =  AngVelER + x%QDT(DOF_GeAz)*PAngVelEL(DOF_GeAz,0,:) &
                                    + x%QDT(DOF_DrTr)*PAngVelEL(DOF_DrTr,0,:)

PAngVelEH(       :,0,:) = PAngVelEL(:,0,:)
 AngVelEH               =  AngVelEL
IF ( p%NumBl == 2 )  THEN ! 2-blader
   PAngVelEH(DOF_Teet,0,:) = CoordSys%f2
    AngVelEH            =  AngVelEH + x%QDT(DOF_Teet)*PAngVelEH(DOF_Teet,0,:)
ENDIF

PAngVelEG(       :,0,:) = PAngVelER(:,0,:)
PAngVelEG(DOF_GeAz,0,:) = GenDir*GBRatio*CoordSys%c1
 AngVelEG               =  AngVelER + x%QDT(DOF_GeAz)*PAngVelEG(DOF_GeAz,0,:)

PAngVelEA(       :,0,:) = PAngVelEN(:,0,:)
PAngVelEA(DOF_TFrl,0,:) = CoordSys%tfa
 AngVelEA               =  AngVelEN + x%QDT(DOF_TFrl)*PAngVelEA(DOF_TFrl,0,:)


   ! Note the hub rotational velocity should be AngVelEH instead AngVelEL, but AeroDyn (13.00.00)
   ! treats teeter deflections like blade deflections:

ADInterfaceComponents%Hub%RotationVel(:)       = (/ AngVelEL(1), -1.*AngVelEL(3), AngVelEL(2) /)
ADInterfaceComponents%RotorFurl%RotationVel(:) = (/ AngVelER(1), -1.*AngVelER(3), AngVelER(2) /)
ADInterfaceComponents%Nacelle%RotationVel(:)   = (/ AngVelEN(1), -1.*AngVelEN(3), AngVelEN(2) /)
ADInterfaceComponents%Tower%RotationVel(:)     = (/ AngVelEX(1), -1.*AngVelEX(3), AngVelEX(2) /)

   ! Define the 1st derivatives of the partial angular velocities of all
   !   of the rigid bodies in the inertia frame and the portion of the angular
   !   acceleration of the rigid bodies in the inertia frame associated with
   !   everything but the QD2T()'s:

PAngVelEX(       :,1,:) = 0.0
AngAccEXt               = 0.0

PAngVelEB(       :,1,:) =                 PAngVelEX(:,1,:)
PAngVelEB(DOF_TFA1,1,:) = CROSS_PRODUCT(   AngVelEX,                   PAngVelEB(DOF_TFA1,0,:) )
PAngVelEB(DOF_TSS1,1,:) = CROSS_PRODUCT(   AngVelEX,                   PAngVelEB(DOF_TSS1,0,:) )
PAngVelEB(DOF_TFA2,1,:) = CROSS_PRODUCT(   AngVelEX,                   PAngVelEB(DOF_TFA2,0,:) )
PAngVelEB(DOF_TSS2,1,:) = CROSS_PRODUCT(   AngVelEX,                   PAngVelEB(DOF_TSS2,0,:) )
AngAccEBt               =                  AngAccEXt + x%QDT(DOF_TFA1)*PAngVelEB(DOF_TFA1,1,:) &
                                                     + x%QDT(DOF_TSS1)*PAngVelEB(DOF_TSS1,1,:) &
                                                     + x%QDT(DOF_TFA2)*PAngVelEB(DOF_TFA2,1,:) &
                                                     + x%QDT(DOF_TSS2)*PAngVelEB(DOF_TSS2,1,:)

PAngVelEN(       :,1,:) =                 PAngVelEB(:,1,:)
PAngVelEN(DOF_Yaw ,1,:) = CROSS_PRODUCT(   AngVelEB,                   PAngVelEN(DOF_Yaw ,0,:) )
AngAccENt               =                  AngAccEBt + x%QDT(DOF_Yaw )*PAngVelEN(DOF_Yaw ,1,:)

PAngVelER(       :,1,:) =                 PAngVelEN(:,1,:)
PAngVelER(DOF_RFrl,1,:) = CROSS_PRODUCT(   AngVelEN,                   PAngVelER(DOF_RFrl,0,:) )
AngAccERt               =                  AngAccENt + x%QDT(DOF_RFrl)*PAngVelER(DOF_RFrl,1,:)

PAngVelEL(       :,1,:) =                 PAngVelER(:,1,:)
PAngVelEL(DOF_GeAz,1,:) = CROSS_PRODUCT(   AngVelER,                   PAngVelEL(DOF_GeAz,0,:) )
PAngVelEL(DOF_DrTr,1,:) = CROSS_PRODUCT(   AngVelER,                   PAngVelEL(DOF_DrTr,0,:) )
AngAccELt               =                  AngAccERt + x%QDT(DOF_GeAz)*PAngVelEL(DOF_GeAz,1,:) &
                                                     + x%QDT(DOF_DrTr)*PAngVelEL(DOF_DrTr,1,:)

PAngVelEH(       :,1,:) = PAngVelEL(:,1,:)
AngAccEHt               =                  AngAccELt
IF ( p%NumBl == 2 )  THEN ! 2-blader
   PAngVelEH(DOF_Teet,1,:) = CROSS_PRODUCT(AngVelEH,                   PAngVelEH(DOF_Teet,0,:) )
    AngAccEHt              =               AngAccEHt + x%QDT(DOF_Teet)*PAngVelEH(DOF_Teet,1,:)
ENDIF

PAngVelEG(       :,1,:) = PAngVelER(:,1,:)
PAngVelEG(DOF_GeAz,1,:) = CROSS_PRODUCT(   AngVelER,                   PAngVelEG(DOF_GeAz,0,:) )
AngAccEGt              =                   AngAccERt + x%QDT(DOF_GeAz)*PAngVelEG(DOF_GeAz,1,:)

PAngVelEA(       :,1,:) = PAngVelEN(:,1,:)
PAngVelEA(DOF_TFrl,1,:) = CROSS_PRODUCT(   AngVelEN,                   PAngVelEA(DOF_TFrl,0,:) )
AngAccEAt               =                  AngAccENt + x%QDT(DOF_TFrl)*PAngVelEA(DOF_TFrl,1,:)



DO K = 1,p%NumBl ! Loop through all blades

   ! Define the partial angular velocities of the tip (body M(BldFlexL)) in the  inertia frame:
   ! NOTE: PAngVelEM(K,J,I,D,:) = the Dth-derivative of the partial angular velocity of DOF I for body M of blade K, element J in body E.

   PAngVelEM(K,p%TipNode,          :,0,:) = PAngVelEH(:,0,:)
   PAngVelEM(K,p%TipNode,DOF_BF(K,1),0,:) = - TwistedSF(K,2,1,p%TipNode,1)*CoordSys%j1(K,:) &
                                          + TwistedSF(K,1,1,p%TipNode,1)*CoordSys%j2(K,:)
   PAngVelEM(K,p%TipNode,DOF_BF(K,2),0,:) = - TwistedSF(K,2,2,p%TipNode,1)*CoordSys%j1(K,:) &
                                          + TwistedSF(K,1,2,p%TipNode,1)*CoordSys%j2(K,:)
   PAngVelEM(K,p%TipNode,DOF_BE(K,1),0,:) = - TwistedSF(K,2,3,p%TipNode,1)*CoordSys%j1(K,:) &
                                          + TwistedSF(K,1,3,p%TipNode,1)*CoordSys%j2(K,:)
!    AngVelHM(K,p%TipNode              ,:) =  AngVelEH + x%QDT(DOF_BF(K,1))*PAngVelEM(K,p%TipNode,DOF_BF(K,1),0,:) & ! Currently
!                                                    + x%QDT(DOF_BF(K,2))*PAngVelEM(K,p%TipNode,DOF_BF(K,2),0,:) & ! unused
!                                                    + x%QDT(DOF_BE(K,1))*PAngVelEM(K,p%TipNode,DOF_BE(K,1),0,:)   ! calculations
    AngPosHM(K,p%TipNode              ,:) =             x%QT (DOF_BF(K,1))*PAngVelEM(K,p%TipNode,DOF_BF(K,1),0,:) &
                                                    + x%QT (DOF_BF(K,2))*PAngVelEM(K,p%TipNode,DOF_BF(K,2),0,:) &
                                                    + x%QT (DOF_BE(K,1))*PAngVelEM(K,p%TipNode,DOF_BE(K,1),0,:)


   ! Define the 1st derivatives of the partial angular velocities of the tip
   !   (body M(BldFlexL)) in the inertia frame:

! NOTE: These are currently unused by the code, therefore, they need not
!       be calculated.  Thus, they are currently commented out.  If it
!       turns out that they are ever needed (i.e., if inertias of the
!       blade elements are ever added, etc...) simply uncomment out these
!       computations:
!   PAngVelEM(K,p%TipNode,          :,1,:) = PAngVelEH(:,1,:)
!   PAngVelEM(K,p%TipNode,DOF_BF(K,1),1,:) = CROSS_PRODUCT(   AngVelEH, PAngVelEM(K,p%TipNode,DOF_BF(K,1),0,:)    )
!   PAngVelEM(K,p%TipNode,DOF_BF(K,2),1,:) = CROSS_PRODUCT(   AngVelEH, PAngVelEM(K,p%TipNode,DOF_BF(K,2),0,:)    )
!   PAngVelEM(K,p%TipNode,DOF_BE(K,1),1,:) = CROSS_PRODUCT(   AngVelEH, PAngVelEM(K,p%TipNode,DOF_BE(K,1),0,:)    )


   DO J = 1,p%BldNodes ! Loop through the blade nodes / elements
   ! Define the partial angular velocities of the current node (body M(RNodes(J))) in the inertia frame:
   ! NOTE: PAngVelEM(K,J,I,D,:) = the Dth-derivative of the partial angular velocity
   !   of DOF I for body M of blade K, element J in body E.

      PAngVelEM(K,J,          :,0,:) = PAngVelEH(:,0,:)
      PAngVelEM(K,J,DOF_BF(K,1),0,:) = - TwistedSF(K,2,1,J,1)*CoordSys%j1(K,:) &
                                       + TwistedSF(K,1,1,J,1)*CoordSys%j2(K,:)
      PAngVelEM(K,J,DOF_BF(K,2),0,:) = - TwistedSF(K,2,2,J,1)*CoordSys%j1(K,:) &
                                       + TwistedSF(K,1,2,J,1)*CoordSys%j2(K,:)
      PAngVelEM(K,J,DOF_BE(K,1),0,:) = - TwistedSF(K,2,3,J,1)*CoordSys%j1(K,:) &
                                       + TwistedSF(K,1,3,J,1)*CoordSys%j2(K,:)
!       AngVelHM(K,J              ,:) =  AngVelEH + x%QDT(DOF_BF(K,1))*PAngVelEM(K,J,DOF_BF(K,1),0,:) & ! Currently
!                                                 + x%QDT(DOF_BF(K,2))*PAngVelEM(K,J,DOF_BF(K,2),0,:) & ! unused
!                                                 + x%QDT(DOF_BE(K,1))*PAngVelEM(K,J,DOF_BE(K,1),0,:)   ! calculations
       AngPosHM(K,J              ,:) =             x%QT (DOF_BF(K,1))*PAngVelEM(K,J,DOF_BF(K,1),0,:) &
                                                 + x%QT (DOF_BF(K,2))*PAngVelEM(K,J,DOF_BF(K,2),0,:) &
                                                 + x%QT (DOF_BE(K,1))*PAngVelEM(K,J,DOF_BE(K,1),0,:)


   ! Define the 1st derivatives of the partial angular velocities of the current node (body M(RNodes(J))) in the inertia frame:

! NOTE: These are currently unused by the code, therefore, they need not
!       be calculated.  Thus, they are currently commented out.  If it
!       turns out that they are ever needed (i.e., if inertias of the
!       blade elements are ever added, etc...) simply uncomment out these
!       computations:
!      PAngVelEM(K,J,          :,1,:) = PAngVelEH(:,1,:)
!      PAngVelEM(K,J,DOF_BF(K,1),1,:) = CROSS_PRODUCT(   AngVelEH, PAngVelEM(K,J,DOF_BF(K,1),0,:) )
!      PAngVelEM(K,J,DOF_BF(K,2),1,:) = CROSS_PRODUCT(   AngVelEH, PAngVelEM(K,J,DOF_BF(K,2),0,:) )
!      PAngVelEM(K,J,DOF_BE(K,1),1,:) = CROSS_PRODUCT(   AngVelEH, PAngVelEM(K,J,DOF_BE(K,1),0,:) )


   END DO !J = 1,p%BldNodes ! Loop through the blade nodes / elements

END DO !K = 1,p%NumBl


   !-------------------------------------------------------------------------------------------------
   ! Partial linear velocities and accelerations
   !-------------------------------------------------------------------------------------------------

   ! Define the partial linear velocities (and their 1st derivatives) of all of
   !   the points on the wind turbine in the inertia frame that are not
   !   dependent on the distributed tower or blade parameters.  Also, define
   !   the portion of the linear acceleration of the points in the inertia
   !   frame associated with everything but the QD2T()'s:
   ! NOTE: PLinVelEX(I,D,:) = the Dth-derivative of the partial linear velocity
   !   of DOF I for point X in body E.

EwXXrZY   = CROSS_PRODUCT( AngVelEX, rZY   ) !
EwXXrZO   = CROSS_PRODUCT( AngVelEX, rZO   ) !
EwNXrOU   = CROSS_PRODUCT( AngVelEN, rOU   ) !
EwNXrOV   = CROSS_PRODUCT( AngVelEN, rOV   ) !
EwRXrVD   = CROSS_PRODUCT( AngVelER, rVD   ) ! Cross products
EwRXrVIMU = CROSS_PRODUCT( AngVelER, rVIMU ) ! that are used
EwRXrVP   = CROSS_PRODUCT( AngVelER, rVP   ) ! in the following
EwHXrPQ   = CROSS_PRODUCT( AngVelEH, rPQ   ) ! DO...LOOPs
EwHXrQC   = CROSS_PRODUCT( AngVelEH, rQC   ) !
EwNXrOW   = CROSS_PRODUCT( AngVelEN, rOW   ) !
EwAXrWI   = CROSS_PRODUCT( AngVelEA, rWI   ) !
EwAXrWJ   = CROSS_PRODUCT( AngVelEA, rWJ   ) !
EwAXrWK   = CROSS_PRODUCT( AngVelEA, rWK   ) !


PLinVelEZ(       :,:,:) = 0.0
PLinVelEZ(DOF_Sg  ,0,:) =  CoordSys%z1
PLinVelEZ(DOF_Sw  ,0,:) = -CoordSys%z3
PLinVelEZ(DOF_Hv  ,0,:) =  CoordSys%z2

 LinVelEZ               =              x%QDT(DOF_Sg  )*PLinVelEZ(DOF_Sg  ,0,:) &
                                     + x%QDT(DOF_Sw  )*PLinVelEZ(DOF_Sw  ,0,:) &
                                     + x%QDT(DOF_Hv  )*PLinVelEZ(DOF_Hv  ,0,:)


PLinVelEY(       :,:,:) = PLinVelEZ(:,:,:)
DO I = 1,NPX   ! Loop through all DOFs associated with the angular motion of the platform (body X)

   TmpVec0              = CROSS_PRODUCT(            PAngVelEX(PX(I)   ,0,:),     rZY  )
   TmpVec1              = CROSS_PRODUCT(            PAngVelEX(PX(I)   ,0,:), EwXXrZY  )

   PLinVelEY(PX(I),0,:) = TmpVec0   +               PLinVelEY(PX(I)   ,0,:)
   PLinVelEY(PX(I),1,:) = TmpVec1   +               PLinVelEY(PX(I)   ,1,:)

    LinAccEYt           = LinAccEYt + x%QDT(PX(I) )*PLinVelEY(PX(I)   ,1,:)

ENDDO          ! I - all DOFs associated with the angular motion of the platform (body X)


PLinVelEO(       :,:,:) = PLinVelEZ(:,:,:)
PLinVelEO(DOF_TFA1,0,:) = CoordSys%a1      - (   AxRedTFA(1,1,TTopNode)* x%QT(DOF_TFA1) &
                                               + AxRedTFA(1,2,TTopNode)* x%QT(DOF_TFA2)   )*CoordSys%a2
PLinVelEO(DOF_TSS1,0,:) = CoordSys%a3      - (   AxRedTSS(1,1,TTopNode)* x%QT(DOF_TSS1) &
                                               + AxRedTSS(1,2,TTopNode)* x%QT(DOF_TSS2)   )*CoordSys%a2
PLinVelEO(DOF_TFA2,0,:) = CoordSys%a1      - (   AxRedTFA(2,2,TTopNode)* x%QT(DOF_TFA2) &
                                               + AxRedTFA(1,2,TTopNode)* x%QT(DOF_TFA1)   )*CoordSys%a2
PLinVelEO(DOF_TSS2,0,:) = CoordSys%a3      - (   AxRedTSS(2,2,TTopNode)* x%QT(DOF_TSS2) &
                                               + AxRedTSS(1,2,TTopNode)* x%QT(DOF_TSS1)   )*CoordSys%a2

TmpVec1 = CROSS_PRODUCT(   AngVelEX   , PLinVelEO(DOF_TFA1,0,:) )
TmpVec2 = CROSS_PRODUCT(   AngVelEX   , PLinVelEO(DOF_TSS1,0,:) )
TmpVec3 = CROSS_PRODUCT(   AngVelEX   , PLinVelEO(DOF_TFA2,0,:) )
TmpVec4 = CROSS_PRODUCT(   AngVelEX   , PLinVelEO(DOF_TSS2,0,:) )

PLinVelEO(DOF_TFA1,1,:) = TmpVec1 - (   AxRedTFA(1,1,TTopNode)*x%QDT(DOF_TFA1) &
                                      + AxRedTFA(1,2,TTopNode)*x%QDT(DOF_TFA2)   )*CoordSys%a2
PLinVelEO(DOF_TSS1,1,:) = TmpVec2 - (   AxRedTSS(1,1,TTopNode)*x%QDT(DOF_TSS1) &
                                      + AxRedTSS(1,2,TTopNode)*x%QDT(DOF_TSS2)   )*CoordSys%a2
PLinVelEO(DOF_TFA2,1,:) = TmpVec3 - (   AxRedTFA(2,2,TTopNode)*x%QDT(DOF_TFA2) &
                                      + AxRedTFA(1,2,TTopNode)*x%QDT(DOF_TFA1)   )*CoordSys%a2
PLinVelEO(DOF_TSS2,1,:) = TmpVec4 - (   AxRedTSS(2,2,TTopNode)*x%QDT(DOF_TSS2) &
                                      + AxRedTSS(1,2,TTopNode)*x%QDT(DOF_TSS1)   )*CoordSys%a2

 LinVelXO               =              x%QDT(DOF_TFA1)*PLinVelEO(DOF_TFA1,0,:) &
                                     + x%QDT(DOF_TSS1)*PLinVelEO(DOF_TSS1,0,:) &
                                     + x%QDT(DOF_TFA2)*PLinVelEO(DOF_TFA2,0,:) &
                                     + x%QDT(DOF_TSS2)*PLinVelEO(DOF_TSS2,0,:)
 LinAccEOt              =              x%QDT(DOF_TFA1)*PLinVelEO(DOF_TFA1,1,:) &
                                     + x%QDT(DOF_TSS1)*PLinVelEO(DOF_TSS1,1,:) &
                                     + x%QDT(DOF_TFA2)*PLinVelEO(DOF_TFA2,1,:) &
                                     + x%QDT(DOF_TSS2)*PLinVelEO(DOF_TSS2,1,:)

DO I = 1,NPX   ! Loop through all DOFs associated with the angular motion of the platform (body X)

   TmpVec0              = CROSS_PRODUCT(             PAngVelEX(PX(I)   ,0,:),     rZO                 )
   TmpVec1              = CROSS_PRODUCT(             PAngVelEX(PX(I)   ,0,:), EwXXrZO + LinVelXO      )

   PLinVelEO(PX(I),0,:) = TmpVec0    +               PLinVelEO(PX(I)   ,0,:)
   PLinVelEO(PX(I),1,:) = TmpVec1    +               PLinVelEO(PX(I)   ,1,:)

    LinAccEOt           =  LinAccEOt + x%QDT(PX(I) )*PLinVelEO(PX(I)   ,1,:)

ENDDO          ! I - all DOFs associated with the angular motion of the platform (body X)


PLinVelEU(       :,:,:) = PLinVelEO(:,:,:)
DO I = 1,NPN   ! Loop through all DOFs associated with the angular motion of the nacelle (body N)

   TmpVec0              = CROSS_PRODUCT(             PAngVelEN(PN(I)   ,0,:),     rOU                 )
   TmpVec1              = CROSS_PRODUCT(             PAngVelEN(PN(I)   ,0,:), EwNXrOU                 )
   TmpVec2              = CROSS_PRODUCT(             PAngVelEN(PN(I)   ,1,:),     rOU                 )

   PLinVelEU(PN(I),0,:) = TmpVec0    +               PLinVelEU(PN(I)   ,0,:)
   PLinVelEU(PN(I),1,:) = TmpVec1    + TmpVec2 +     PLinVelEU(PN(I)   ,1,:)

    LinAccEUt           =  LinAccEUt + x%QDT(PN(I) )*PLinVelEU(PN(I)   ,1,:)

ENDDO          ! I - all DOFs associated with the angular motion of the nacelle (body N)


PLinVelEV(       :,:,:) = PLinVelEO(:,:,:)
DO I = 1,NPN   ! Loop through all DOFs associated with the angular motion of the nacelle (body N)

   TmpVec0              = CROSS_PRODUCT(             PAngVelEN(PN(I)   ,0,:),     rOV                 )
   TmpVec1              = CROSS_PRODUCT(             PAngVelEN(PN(I)   ,0,:), EwNXrOV                 )
   TmpVec2              = CROSS_PRODUCT(             PAngVelEN(PN(I)   ,1,:),     rOV                 )

   PLinVelEV(PN(I),0,:) = TmpVec0    +               PLinVelEV(PN(I)   ,0,:)
   PLinVelEV(PN(I),1,:) = TmpVec1    + TmpVec2 +     PLinVelEV(PN(I)   ,1,:)

    LinAccEVt           =  LinAccEVt + x%QDT(PN(I) )*PLinVelEV(PN(I)   ,1,:)

ENDDO          ! I - all DOFs associated with the angular motion of the nacelle (body N)


PLinVelED(       :,:,:) = PLinVelEV(:,:,:)
DO I = 1,NPR   ! Loop through all DOFs associated with the angular motion of the structure that furls with the rotor (not including rotor) (body R)

   TmpVec0              = CROSS_PRODUCT(             PAngVelER(PR(I)   ,0,:),     rVD                 )
   TmpVec1              = CROSS_PRODUCT(             PAngVelER(PR(I)   ,0,:), EwRXrVD                 )
   TmpVec2              = CROSS_PRODUCT(             PAngVelER(PR(I)   ,1,:),     rVD                 )

   PLinVelED(PR(I),0,:) = TmpVec0    +               PLinVelED(PR(I)   ,0,:)
   PLinVelED(PR(I),1,:) = TmpVec1    + TmpVec2 +     PLinVelED(PR(I)   ,1,:)

    LinAccEDt           =  LinAccEDt + x%QDT(PR(I) )*PLinVelED(PR(I)   ,1,:)

ENDDO          ! I - all DOFs associated with the angular motion of the structure that furls with the rotor (not including rotor) (body R)


PLinVelEIMU(     :,:,:) = PLinVelEV(:,:,:)
 LinVelEIMU             =  LinVelEZ
DO I = 1,NPR   ! Loop through all DOFs associated with the angular motion of the structure that furls with the rotor (not including rotor) (body R)

   TmpVec0                = CROSS_PRODUCT(              PAngVelER(PR(I)   ,0,:),     rVIMU               )
   TmpVec1                = CROSS_PRODUCT(              PAngVelER(PR(I)   ,0,:), EwRXrVIMU               )
   TmpVec2                = CROSS_PRODUCT(              PAngVelER(PR(I)   ,1,:),     rVIMU               )

   PLinVelEIMU(PR(I),0,:) = TmpVec0    +                PLinVelEIMU(PR(I) ,0,:)
   PLinVelEIMU(PR(I),1,:) = TmpVec1    + TmpVec2 +      PLinVelEIMU(PR(I) ,1,:)

    LinVelEIMU            =  LinVelEIMU  + x%QDT(PR(I) )*PLinVelEIMU(PR(I) ,0,:)
    LinAccEIMUt           =  LinAccEIMUt + x%QDT(PR(I) )*PLinVelEIMU(PR(I) ,1,:)

ENDDO          ! I - all DOFs associated with the angular motion of the structure that furls with the rotor (not including rotor) (body R)


PLinVelEP(       :,:,:) = PLinVelEV(:,:,:)
DO I = 1,NPR   ! Loop through all DOFs associated with the angular motion of the structure that furls with the rotor (not including rotor) (body R)

   TmpVec0              = CROSS_PRODUCT(             PAngVelER(PR(I)   ,0,:),     rVP                 )
   TmpVec1              = CROSS_PRODUCT(             PAngVelER(PR(I)   ,0,:), EwRXrVP                 )
   TmpVec2              = CROSS_PRODUCT(             PAngVelER(PR(I)   ,1,:),     rVP                 )

   PLinVelEP(PR(I),0,:) = TmpVec0    +               PLinVelEP(PR(I)   ,0,:)
   PLinVelEP(PR(I),1,:) = TmpVec1    + TmpVec2 +     PLinVelEP(PR(I)   ,1,:)

    LinAccEPt           =  LinAccEPt + x%QDT(PR(I) )*PLinVelEP(PR(I)   ,1,:)

ENDDO          ! I - all DOFs associated with the angular motion of the structure that furls with the rotor (not including rotor) (body R)


PLinVelEQ(       :,:,:) = PLinVelEP(:,:,:)
DO I = 1,p%NPH   ! Loop through all DOFs associated with the angular motion of the hub (body H)

   TmpVec0                              = CROSS_PRODUCT(             PAngVelEH(p%PH(I)   ,0,:),     rPQ  )
   TmpVec1                              = CROSS_PRODUCT(             PAngVelEH(p%PH(I)   ,0,:), EwHXrPQ  )
   TmpVec2                              = CROSS_PRODUCT(             PAngVelEH(p%PH(I)   ,1,:),     rPQ  )

   PLinVelEQ(p%PH(I),0,:) = TmpVec0    +               PLinVelEQ(p%PH(I)   ,0,:)
   PLinVelEQ(p%PH(I),1,:) = TmpVec1    + TmpVec2 +     PLinVelEQ(p%PH(I)   ,1,:)

    LinAccEQt           =  LinAccEQt + x%QDT(p%PH(I) )*PLinVelEQ(p%PH(I)   ,1,:)

ENDDO          ! I - all DOFs associated with the angular motion of the hub (body H)


PLinVelEC(       :,:,:) = PLinVelEQ(:,:,:)
DO I = 1,p%NPH   ! Loop through all DOFs associated with the angular motion of the hub (body H)

   TmpVec0                              = CROSS_PRODUCT(             PAngVelEH(p%PH(I)   ,0,:),     rQC )
   TmpVec1                              = CROSS_PRODUCT(             PAngVelEH(p%PH(I)   ,0,:), EwHXrQC )
   TmpVec2                              = CROSS_PRODUCT(             PAngVelEH(p%PH(I)   ,1,:),     rQC )

   PLinVelEC(p%PH(I),0,:) = TmpVec0    +               PLinVelEC(p%PH(I)   ,0,:)
   PLinVelEC(p%PH(I),1,:) = TmpVec1    + TmpVec2 +     PLinVelEC(p%PH(I)   ,1,:)

    LinAccECt           =  LinAccECt + x%QDT(p%PH(I) )*PLinVelEC(p%PH(I)   ,1,:)

ENDDO          ! I - all DOFs associated with the angular motion of the hub (body H)




DO K = 1,p%NumBl ! Loop through all blades

   ! Define the partial linear velocities (and their 1st derivatives) of the
   !   blade tip (point S(BldFlexL)) in the inertia frame.  Also define the
   !   overall linear velocity of the blade tip in the inertia frame.  Also,
   !   define the portion of the linear acceleration of the blade tip in the
   !   inertia frame associated with everything but the QD2T()'s:

   EwHXrQS = CROSS_PRODUCT( AngVelEH, rQS(K,p%TipNode,:) )

   PLinVelES(K,p%TipNode,          :,:,:) = PLinVelEQ(:,:,:)
   PLinVelES(K,p%TipNode,DOF_BF(K,1),0,:) = TwistedSF(K,1,1,p%TipNode,0)                          *CoordSys%j1(K,:) &
                                          + TwistedSF(K,2,1,p%TipNode,0)                          *CoordSys%j2(K,:) &
                                          - (   AxRedBld(K,1,1,p%TipNode)*x%QT ( DOF_BF(K,1) ) &
                                              + AxRedBld(K,1,2,p%TipNode)*x%QT ( DOF_BF(K,2) ) &
                                              + AxRedBld(K,1,3,p%TipNode)*x%QT ( DOF_BE(K,1) )   )*CoordSys%j3(K,:)
   PLinVelES(K,p%TipNode,DOF_BE(K,1),0,:) = TwistedSF(K,1,3,p%TipNode,0)                          *CoordSys%j1(K,:) &
                                          + TwistedSF(K,2,3,p%TipNode,0)                          *CoordSys%j2(K,:) &
                                          - (   AxRedBld(K,3,3,p%TipNode)*x%QT ( DOF_BE(K,1) ) &
                                              + AxRedBld(K,2,3,p%TipNode)*x%QT ( DOF_BF(K,2) ) &
                                              + AxRedBld(K,1,3,p%TipNode)*x%QT ( DOF_BF(K,1) )   )*CoordSys%j3(K,:)
   PLinVelES(K,p%TipNode,DOF_BF(K,2),0,:) = TwistedSF(K,1,2,p%TipNode,0)                          *CoordSys%j1(K,:) &
                                          + TwistedSF(K,2,2,p%TipNode,0)                          *CoordSys%j2(K,:) &
                                          - (   AxRedBld(K,2,2,p%TipNode)*x%QT ( DOF_BF(K,2) ) &
                                              + AxRedBld(K,1,2,p%TipNode)*x%QT ( DOF_BF(K,1) ) &
                                              + AxRedBld(K,2,3,p%TipNode)*x%QT ( DOF_BE(K,1) )   )*CoordSys%j3(K,:)

   TmpVec1 = CROSS_PRODUCT( AngVelEH, PLinVelES(K,p%TipNode,DOF_BF(K,1),0,:) )
   TmpVec2 = CROSS_PRODUCT( AngVelEH, PLinVelES(K,p%TipNode,DOF_BE(K,1),0,:) )
   TmpVec3 = CROSS_PRODUCT( AngVelEH, PLinVelES(K,p%TipNode,DOF_BF(K,2),0,:) )

   PLinVelES(K,p%TipNode,DOF_BF(K,1),1,:) = TmpVec1 &
                                        - (   AxRedBld(K,1,1,p%TipNode)*x%QDT( DOF_BF(K,1) ) &
                                            + AxRedBld(K,1,2,p%TipNode)*x%QDT( DOF_BF(K,2) ) &
                                            + AxRedBld(K,1,3,p%TipNode)*x%QDT( DOF_BE(K,1) )   )*CoordSys%j3(K,:)
   PLinVelES(K,p%TipNode,DOF_BE(K,1),1,:) = TmpVec2 &
                                        - (   AxRedBld(K,3,3,p%TipNode)*x%QDT( DOF_BE(K,1) ) &
                                            + AxRedBld(K,2,3,p%TipNode)*x%QDT( DOF_BF(K,2) ) &
                                            + AxRedBld(K,1,3,p%TipNode)*x%QDT( DOF_BF(K,1) )   )*CoordSys%j3(K,:)
   PLinVelES(K,p%TipNode,DOF_BF(K,2),1,:) = TmpVec3 &
                                        - (   AxRedBld(K,2,2,p%TipNode)*x%QDT( DOF_BF(K,2) ) &
                                            + AxRedBld(K,1,2,p%TipNode)*x%QDT( DOF_BF(K,1) ) &
                                            + AxRedBld(K,2,3,p%TipNode)*x%QDT( DOF_BE(K,1) )   )*CoordSys%j3(K,:)

   LinVelHS                 = x%QDT( DOF_BF(K,1) )*PLinVelES(K,p%TipNode,DOF_BF(K,1),0,:) &
                            + x%QDT( DOF_BE(K,1) )*PLinVelES(K,p%TipNode,DOF_BE(K,1),0,:) &
                            + x%QDT( DOF_BF(K,2) )*PLinVelES(K,p%TipNode,DOF_BF(K,2),0,:)
   LinAccESt(K,p%TipNode,:) = x%QDT( DOF_BF(K,1) )*PLinVelES(K,p%TipNode,DOF_BF(K,1),1,:) &
                            + x%QDT( DOF_BE(K,1) )*PLinVelES(K,p%TipNode,DOF_BE(K,1),1,:) &
                            + x%QDT( DOF_BF(K,2) )*PLinVelES(K,p%TipNode,DOF_BF(K,2),1,:)

   LinVelES               = LinVelHS + LinVelEZ
   DO I = 1,p%NPH   ! Loop through all DOFs associated with the angular motion of the hub (body H)

      TmpVec0 = CROSS_PRODUCT( PAngVelEH(p%PH(I),0,:),     rQS(K,p%TipNode,:)            )
      TmpVec1 = CROSS_PRODUCT( PAngVelEH(p%PH(I),0,:), EwHXrQS              + LinVelHS   )
      TmpVec2 = CROSS_PRODUCT( PAngVelEH(p%PH(I),1,:),     rQS(K,p%TipNode,:)            )

      PLinVelES(K,p%TipNode,p%PH(I),0,:) = PLinVelES(K,p%TipNode,p%PH(I),0,:) + TmpVec0
      PLinVelES(K,p%TipNode,p%PH(I),1,:) = PLinVelES(K,p%TipNode,p%PH(I),1,:) + TmpVec1 + TmpVec2

      LinVelES                  = LinVelES                   + x%QDT(p%PH(I)    )*&
                                               PLinVelES(K,p%TipNode,p%PH(I),0,:)
      LinAccESt(K,p%TipNode, :) = LinAccESt(K,p%TipNode,  :) + x%QDT(p%PH(I)    )* &
                                               PLinVelES(K,p%TipNode,p%PH(I),1,:)

   ENDDO          ! I - all DOFs associated with the angular motion of the hub (body H)

!JASON: USE TipNode HERE INSTEAD OF BldNodes IF YOU ALLOCATE AND DEFINE n1, n2, n3, m1, m2, AND m3 TO USE TipNode.  THIS WILL REQUIRE THAT THE AERODYNAMIC AND STRUCTURAL TWISTS, AeroTwst() AND ThetaS(), BE KNOWN AT THE TIP!!!
   LinVelESm2(K) = DOT_PRODUCT( LinVelES, CoordSys%m2(K,p%BldNodes,:) )


   DO J = 1,p%BldNodes ! Loop through the blade nodes / elements

   ! Define the partial linear velocities (and their 1st derivatives) of the
   !   current node (point S(RNodes(J))) in the inertia frame.  Also define
   !   the overall linear velocity of the current node in the inertia frame.
   !   Also, define the portion of the linear acceleration of the current node
   !   in the inertia frame associated with everything but the QD2T()'s:

      EwHXrQS = CROSS_PRODUCT(  AngVelEH, rQS(K,J,:) )

      PLinVelES(K,J,          :,:,:) = PLinVelEQ(:,:,:)
      PLinVelES(K,J,DOF_BF(K,1),0,:) = TwistedSF(K,1,1,J,0)                          *CoordSys%j1(K,:) &
                                     + TwistedSF(K,2,1,J,0)                          *CoordSys%j2(K,:) &
                                     - (   AxRedBld(K,1,1,J)*x%QT ( DOF_BF(K,1) ) &
                                         + AxRedBld(K,1,2,J)*x%QT ( DOF_BF(K,2) ) &
                                         + AxRedBld(K,1,3,J)*x%QT ( DOF_BE(K,1) )   )*CoordSys%j3(K,:)
      PLinVelES(K,J,DOF_BE(K,1),0,:) = TwistedSF(K,1,3,J,0)                          *CoordSys%j1(K,:) &
                                     + TwistedSF(K,2,3,J,0)                          *CoordSys%j2(K,:) &
                                     - (   AxRedBld(K,3,3,J)*x%QT ( DOF_BE(K,1) ) &
                                         + AxRedBld(K,2,3,J)*x%QT ( DOF_BF(K,2) ) &
                                         + AxRedBld(K,1,3,J)*x%QT ( DOF_BF(K,1) )   )*CoordSys%j3(K,:)
      PLinVelES(K,J,DOF_BF(K,2),0,:) = TwistedSF(K,1,2,J,0)                          *CoordSys%j1(K,:) &
                                     + TwistedSF(K,2,2,J,0)                          *CoordSys%j2(K,:) &
                                     - (   AxRedBld(K,2,2,J)*x%QT ( DOF_BF(K,2) ) &
                                         + AxRedBld(K,1,2,J)*x%QT ( DOF_BF(K,1) ) &
                                         + AxRedBld(K,2,3,J)*x%QT ( DOF_BE(K,1) )   )*CoordSys%j3(K,:)

      TmpVec1 = CROSS_PRODUCT( AngVelEH, PLinVelES(K,J,DOF_BF(K,1),0,:) )
      TmpVec2 = CROSS_PRODUCT( AngVelEH, PLinVelES(K,J,DOF_BE(K,1),0,:) )
      TmpVec3 = CROSS_PRODUCT( AngVelEH, PLinVelES(K,J,DOF_BF(K,2),0,:) )

      PLinVelES(K,J,DOF_BF(K,1),1,:) = TmpVec1 &
                                     - (   AxRedBld(K,1,1,J)*x%QDT( DOF_BF(K,1) ) &
                                         + AxRedBld(K,1,2,J)*x%QDT( DOF_BF(K,2) ) &
                                         + AxRedBld(K,1,3,J)*x%QDT( DOF_BE(K,1) )   )*CoordSys%j3(K,:)
      PLinVelES(K,J,DOF_BE(K,1),1,:) = TmpVec2 &
                                     - (   AxRedBld(K,3,3,J)*x%QDT( DOF_BE(K,1) ) &
                                         + AxRedBld(K,2,3,J)*x%QDT( DOF_BF(K,2) ) &
                                         + AxRedBld(K,1,3,J)*x%QDT( DOF_BF(K,1) )   )*CoordSys%j3(K,:)
      PLinVelES(K,J,DOF_BF(K,2),1,:) = TmpVec3 &
                                     - (   AxRedBld(K,2,2,J)*x%QDT( DOF_BF(K,2) ) &
                                         + AxRedBld(K,1,2,J)*x%QDT( DOF_BF(K,1) ) &
                                         + AxRedBld(K,2,3,J)*x%QDT( DOF_BE(K,1) )   )*CoordSys%j3(K,:)

      LinVelHS         = x%QDT( DOF_BF(K,1) )*PLinVelES(K,J,DOF_BF(K,1),0,:) &
                       + x%QDT( DOF_BE(K,1) )*PLinVelES(K,J,DOF_BE(K,1),0,:) &
                       + x%QDT( DOF_BF(K,2) )*PLinVelES(K,J,DOF_BF(K,2),0,:)
      LinAccESt(K,J,:) = x%QDT( DOF_BF(K,1) )*PLinVelES(K,J,DOF_BF(K,1),1,:) &
                       + x%QDT( DOF_BE(K,1) )*PLinVelES(K,J,DOF_BE(K,1),1,:) &
                       + x%QDT( DOF_BF(K,2) )*PLinVelES(K,J,DOF_BF(K,2),1,:)

      LinVelES         = LinVelHS + LinVelEZ
      DO I = 1,p%NPH   ! Loop through all DOFs associated with the angular motion of the hub (body H)

         TmpVec0 = CROSS_PRODUCT(   PAngVelEH(p%PH(I),0,:),     rQS(K,J,:)            )
         TmpVec1 = CROSS_PRODUCT(   PAngVelEH(p%PH(I),0,:), EwHXrQS        + LinVelHS )
         TmpVec2 = CROSS_PRODUCT(   PAngVelEH(p%PH(I),1,:),     rQS(K,J,:)            )

         PLinVelES(K,J,p%PH(I),0,:) = PLinVelES(K,J,p%PH(I),0,:) + TmpVec0
         PLinVelES(K,J,p%PH(I),1,:) = PLinVelES(K,J,p%PH(I),1,:) + TmpVec1 + TmpVec2

         LinVelES                 = LinVelES                 + x%QDT(p%PH(I))*PLinVelES(K,J,p%PH(I),0,:)
         LinAccESt(K,J,        :) = LinAccESt(K,J,        :) + x%QDT(p%PH(I))*PLinVelES(K,J,p%PH(I),1,:)

      END DO ! I - all DOFs associated with the angular motion of the hub (body H)

      ADAeroMarkers%Blade(J,K)%TranslationVel(:)= (/ LinVelES(1), -1.*LinVelES(3),  LinVelES(2)  /)  !AeroDyn's coordinates


   END DO !J = 1,p%BldNodes ! Loop through the blade nodes / elements

END DO !K = 1,p%NumBl




PLinVelEW(       :,:,:) = PLinVelEO(:,:,:)
DO I = 1,NPN   ! Loop through all DOFs associated with the angular motion of the nacelle (body N)

   TmpVec0              = CROSS_PRODUCT(             PAngVelEN(PN(I)   ,0,:),     rOW                 )
   TmpVec1              = CROSS_PRODUCT(             PAngVelEN(PN(I)   ,0,:), EwNXrOW                 )
   TmpVec2              = CROSS_PRODUCT(             PAngVelEN(PN(I)   ,1,:),     rOW                 )

   PLinVelEW(PN(I),0,:) = TmpVec0    +               PLinVelEW(PN(I)   ,0,:)
   PLinVelEW(PN(I),1,:) = TmpVec1    + TmpVec2 +     PLinVelEW(PN(I)   ,1,:)

    LinAccEWt           =  LinAccEWt + x%QDT(PN(I) )*PLinVelEW(PN(I)   ,1,:)

ENDDO          ! I - all DOFs associated with the angular motion of the nacelle (body N)


PLinVelEI(       :,:,:) = PLinVelEW(:,:,:)
DO I = 1,NPA   ! Loop through all DOFs associated with the angular motion of the tail (body A)

   TmpVec0              = CROSS_PRODUCT(             PAngVelEA(PA(I)   ,0,:),     rWI                 )
   TmpVec1              = CROSS_PRODUCT(             PAngVelEA(PA(I)   ,0,:), EwAXrWI                 )
   TmpVec2              = CROSS_PRODUCT(             PAngVelEA(PA(I)   ,1,:),     rWI                 )

   PLinVelEI(PA(I),0,:) = TmpVec0    +               PLinVelEI(PA(I)   ,0,:)
   PLinVelEI(PA(I),1,:) = TmpVec1    + TmpVec2 +     PLinVelEI(PA(I)   ,1,:)

    LinAccEIt           =  LinAccEIt + x%QDT(PA(I) )*PLinVelEI(PA(I)   ,1,:)

ENDDO          ! I - all DOFs associated with the angular motion of the tail (body A)


PLinVelEJ(       :,:,:) = PLinVelEW(:,:,:)
DO I = 1,NPA   ! Loop through all DOFs associated with the angular motion of the tail (body A)

   TmpVec0              = CROSS_PRODUCT(             PAngVelEA(PA(I)   ,0,:),     rWJ                 )
   TmpVec1              = CROSS_PRODUCT(             PAngVelEA(PA(I)   ,0,:), EwAXrWJ                 )
   TmpVec2              = CROSS_PRODUCT(             PAngVelEA(PA(I)   ,1,:),     rWJ                 )

   PLinVelEJ(PA(I),0,:) = TmpVec0    +               PLinVelEJ(PA(I)   ,0,:)
   PLinVelEJ(PA(I),1,:) = TmpVec1    + TmpVec2 +     PLinVelEJ(PA(I)   ,1,:)

    LinAccEJt           =  LinAccEJt + x%QDT(PA(I) )*PLinVelEJ(PA(I)   ,1,:)

ENDDO          ! I - all DOFs associated with the angular motion of the tail (body A)

PLinVelEK(       :,:,:) = PLinVelEW(:,:,:)
 LinVelEK               =  LinVelEZ
DO I = 1,NPA   ! Loop through all DOFs associated with the angular motion of the tail (body A)

   TmpVec0              = CROSS_PRODUCT(             PAngVelEA(PA(I)   ,0,:),     rWK                 )
   TmpVec1              = CROSS_PRODUCT(             PAngVelEA(PA(I)   ,0,:), EwAXrWK                 )
   TmpVec2              = CROSS_PRODUCT(             PAngVelEA(PA(I)   ,1,:),     rWK                 )

   PLinVelEK(PA(I),0,:) = TmpVec0    +               PLinVelEK(PA(I)   ,0,:)
   PLinVelEK(PA(I),1,:) = TmpVec1    + TmpVec2 +     PLinVelEK(PA(I)   ,1,:)

    LinVelEK            =  LinVelEK  + x%QDT(PA(I) )*PLinVelEK(PA(I)   ,0,:)
    LinAccEKt           =  LinAccEKt + x%QDT(PA(I) )*PLinVelEK(PA(I)   ,1,:)

ENDDO          ! I - all DOFs associated with the angular motion of the tail (body A)



   ! Initialize the partial forces and moments (including those associated
   !   with the QD2T()'s and those that are not) at the teeter pin (point P)
   !   using the hub mass effects:

PFrcPRot  = 0.0   ! Initialize these partial
PMomLPRot = 0.0   ! forces and moments to zero
DO I = 1,OtherState%DOFs%NPCE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the hub center of mass (point C)

   TmpVec1 = -HubMass*PLinVelEC(OtherState%DOFs%PCE(I),0,:)     ! The portion of PFrcPRot  associated with the HubMass
   TmpVec2 = CROSS_PRODUCT( rPC, TmpVec1 )      ! The portion of PMomLPRot associated with the HubMass

   PFrcPRot (OtherState%DOFs%PCE(I),:) = TmpVec1

   PMomLPRot(OtherState%DOFs%PCE(I),:) = TmpVec2 - Hubg1Iner*CoordSys%g1*DOT_PRODUCT( CoordSys%g1, PAngVelEH(OtherState%DOFs%PCE(I),0,:) ) &
                                 - Hubg2Iner*CoordSys%g2*DOT_PRODUCT( CoordSys%g2, PAngVelEH(OtherState%DOFs%PCE(I),0,:) )

ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the hub center of mass (point C)

TmpVec1 = -HubMass*( Gravity*CoordSys%z2 + LinAccECt )                     ! The portion of FrcPRott  associated with the HubMass
TmpVec2 = CROSS_PRODUCT( rPC, TmpVec1 )                                    ! The portion of MomLPRott associated with the HubMass
TmpVec  = Hubg1Iner*CoordSys%g1*DOT_PRODUCT( CoordSys%g1, AngVelEH ) &     ! = ( Hub inertia dyadic ) dot ( angular velocity of hub in the inertia frame )
        + Hubg2Iner*CoordSys%g2*DOT_PRODUCT( CoordSys%g2, AngVelEH )
TmpVec3 = CROSS_PRODUCT( -AngVelEH, TmpVec )                               ! = ( -angular velocity of hub in the inertia frame ) cross ( TmpVec )

FrcPRott  = TmpVec1
MomLPRott = TmpVec2 + TmpVec3 - Hubg1Iner*CoordSys%g1*DOT_PRODUCT( CoordSys%g1, AngAccEHt ) &
                              - Hubg2Iner*CoordSys%g2*DOT_PRODUCT( CoordSys%g2, AngAccEHt )


   !-------------------------------------------------------------------------------------------------
   ! Call AeroDyn to calculate aerodynamic forces
   !-------------------------------------------------------------------------------------------------

IF ( CompAero ) ADAeroLoads = AD_CalculateLoads( ZTime, ADAeroMarkers, ADInterfaceComponents, ADIntrfaceOptions, ErrStat )


DO K = 1,p%NumBl ! Loop through all blades

   ! Calculate the position vector from the teeter pin to the blade root:

   rPS0 = rPQ + p%HubRad*CoordSys%j3(K,:)   ! Position vector from teeter pin (point P) to blade root (point S(0)).


   ! Calculate the tip drag forces if necessary:

   IF ( CompAero )  THEN   ! Calculate the tip drag using the built-in model.

      IF ( ZTime >= TTpBrDp(K) )  THEN                                  ! The tip brakes have been deployed due to time.

         TBDrCon    = TBDrConN + ( TBDrConD - TBDrConN )*&
                      TBFract( ZTime, TTpBrDp(K), TTpBrFl(K) )

      ELSEIF ( ( x%QDT(DOF_GeAz) + x%QDT(DOF_DrTr) ) >= TBDepISp(K) )  THEN ! The tip brakes deploy due to speed.

         TTpBrDp(K) = ZTime                                             ! Use the check on time the next time step.
         TTpBrFl(K) = ZTime + TpBrDT

         TBDrCon    = TBDrConN

      ELSE                                                              ! The tip brakes haven't been deployed yet.

         TBDrCon    = TBDrConN

      ENDIF


      FSTipDrag(K,:) = CoordSys%m2(K,p%BldNodes,:)*SIGN( 0.5*AirDens*LinVelESm2(K)*LinVelESm2(K)*TBDrCon, -1.*LinVelESm2(K) )

   ELSE                    ! Wind turbine in vacuum, no aerodynamic forces.

      FSTipDrag(K,:) = 0.0

   ENDIF


   ! Initialize the partial forces and moments (including those associated
   !   with the QD2T()'s and those that are not) at the blade root (point S(0))
   !   using the tip brake effects:

   PFrcS0B(K,:,:) = 0.0 ! Initialize these partial
   PMomH0B(K,:,:) = 0.0 ! forces and moments to zero
   DO I = 1,OtherState%DOFs%NPSE(K)  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of blade K

      TmpVec1 = -TipMass(K)*PLinVelES(K,p%TipNode,OtherState%DOFs%PSE(K,I),0,:)  ! The portion of PFrcS0B associated with the tip brake
      TmpVec2 = CROSS_PRODUCT( rS0S(K,p%TipNode,:), TmpVec1 )                    ! The portion of PMomH0B associated with the tip brake

      PFrcS0B(K,OtherState%DOFs%PSE(K,I),:) = TmpVec1

      PMomH0B(K,OtherState%DOFs%PSE(K,I),:) = TmpVec2

   ENDDO             ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of blade K

   TmpVec1 = FSTipDrag(K,:) - TipMass(K)*( Gravity*CoordSys%z2 + LinAccESt(K,p%TipNode,:) ) ! The portion of FrcS0Bt associated with the tip brake
   TmpVec2 = CROSS_PRODUCT(  rS0S(K,p%TipNode,:), TmpVec1 )                                 ! The portion of MomH0Bt associated with the tip brake

   FrcS0Bt(K,:) = TmpVec1

   MomH0Bt(K,:) = TmpVec2


   ! Initialize the portions of the mass matrix on and below the
   !   diagonal associated with purely blade DOFs (these portions can't
   !   be calculated using partial loads) using the tip mass effects.
   !   Also, initialize the portions of the forcing vector associated
   !   with purely blade DOFs (these portions can't be calculated using
   !   partial loads) using the tip mass effects:
   ! NOTE: The vector subscript array, PSBE(), used in the following loops must
   !       be sorted from smallest to largest DOF index in order for the loops
   !       to work to enter values only on and below the diagonal of AugMat():

   DO L = 1,OtherState%DOFs%NPSBE(K)    ! Loop through all active (enabled) blade DOFs that contribute to the QD2T-related linear accelerations of the tip of blade K (point S(BldFlexL))
      DO I = L,OtherState%DOFs%NPSBE(K) ! Loop through all active (enabled) blade DOFs greater than or equal to L
         AugMat(OtherState%DOFs%PSBE(K,I),OtherState%DOFs%PSBE(K,L)) = TipMass(K)*&
                                     DOT_PRODUCT( PLinVelES(K, p%TipNode, OtherState%DOFs%PSBE(K,I),0,:), &   ! [C(q,t)]B
                                                  PLinVelES(K, p%TipNode, OtherState%DOFs%PSBE(K,L),0,:)    )
      ENDDO             ! I - All active (enabled) blade DOFs greater than or equal to L
   ENDDO                ! L - All active (enabled) blade DOFs that contribute to the QD2T-related linear accelerations of the tip of blade K (point S(BldFlexL))
   DO I = 1,OtherState%DOFs%NPSBE(K)    ! Loop through all active (enabled) blade DOFs that contribute to the QD2T-related linear accelerations of the tip of blade K (point S(BldFlexL))
         AugMat(OtherState%DOFs%PSBE(K,I), p%NAug) = DOT_PRODUCT( PLinVelES(K,p%TipNode,OtherState%DOFs%PSBE(K,I),0,:), &   ! {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB
                                                           TmpVec1                               ) ! NOTE: TmpVec1 is still the portion of FrcS0Bt associated with the tip brake
   ENDDO                ! I - All active (enabled) blade DOFs that contribute to the QD2T-related linear accelerations of the tip of blade K (point S(BldFlexL))



   DO J = 1,p%BldNodes ! Loop through the blade nodes / elements

   ! Calculate the normal and tangential aerodynamic forces and the aerodynamic
   !   pitching moment at the current element per unit span by calling AeroDyn,
   !   if necessary:

      IF ( CompAero )  THEN   ! Calculate the blade element aerodynamic loads using AeroDyn.


   ! Calculate the aerodynamic pitching moment arm (i.e., the position vector
   !   from point S on the blade to the aerodynamic center of the element):

         rSAerCen = rSAerCenn1(K,J)*CoordSys%n1(K,J,:) + rSAerCenn2(K,J)*CoordSys%n2(K,J,:)        ! bjj this is now re-calculated.


!JASON: WE SHOULD REALLY BE PASSING TO AERODYN THE LINEAR VELOCITIES OF THE AERODYNAMIC CENTER IN THE INERTIA FRAME, NOT SIMPLY THE LINEAR VELOCITIES OF POINT S.  IS THERE ANY WAY OF GETTING THIS VELOCITY?<--DO THIS, WHEN YOU ADD THE COUPLED MODE SHAPES!!!!

   ! Call AeroDyn through AeroCalc() and fill FSAero() and MMAero() with
   !   the resulting forces (AeroForces(:)):
   ! NOTE: AeroForces(1) = element normal     force per unit span in the  m1 direction (N/m).
   !       AeroForces(2) = element tangential force per unit span in the -m2 direction (N/m).
   !       AeroForces(3) = element pitching moment  per unit span in about the m3-axis (N-m/m).


         FSAero(K,J,:) = ADAeroLoads%Blade(J, K)%Force(1) * CoordSys%te1(K,J,:) &
                       + ADAeroLoads%Blade(J, K)%Force(2) * CoordSys%te2(K,J,:)

         MMAero(K,J,:) = CROSS_PRODUCT( rSAerCen, FSAero(K,J,:) )
         MMAero(K,J,:) = MMAero(K,J,:) + ADAeroLoads%Blade(J, K)%Moment(3) * CoordSys%te3(K,J,:)


      ELSE                    ! Wind turbine in vacuum, no aerodynamic forces.

         FSAero(K,J,:) = 0.0
         MMAero(K,J,:) = 0.0

      ENDIF


   ! Calculate the mass of the current element

      ElmntMass = MassB(K,J)*DRNodes(J)   ! Mass of blade element J


   ! Integrate to find the partial forces and moments (including those associated
   !   with the QD2T()'s and those that are not) at the blade root (point S(0)):

      DO I = 1,OtherState%DOFs%NPSE(K)  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of blade K

         TmpVec1 = -ElmntMass*PLinVelES(K,J,OtherState%DOFs%PSE(K,I),0,:)   ! The portion of PFrcS0B associated with blade element J
         TmpVec2 = CROSS_PRODUCT( rS0S(K,J,:), TmpVec1 )    ! The portion of PMomH0B associated with blade element J

         PFrcS0B(K,OtherState%DOFs%PSE(K,I),:) = PFrcS0B(K,OtherState%DOFs%PSE(K,I),:) + TmpVec1

         PMomH0B(K,OtherState%DOFs%PSE(K,I),:) = PMomH0B(K,OtherState%DOFs%PSE(K,I),:) + TmpVec2

      ENDDO             ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of blade K

      TmpVec1 = FSAero(K,J,:)*DRNodes(J) - ElmntMass*( Gravity*CoordSys%z2 + LinAccESt(K,J,:) ) ! The portion of FrcS0Bt associated with blade element J
      TmpVec2 = CROSS_PRODUCT( rS0S(K,J,:), TmpVec1 )                                  ! The portion of MomH0Bt associated with blade element J
      TmpVec3 = MMAero(K,J,:)*DRNodes(J)                                               ! The total external moment applied to blade element J

      FrcS0Bt(K,:) = FrcS0Bt(K,:) + TmpVec1

      MomH0Bt(K,:) = MomH0Bt(K,:) + TmpVec2 + TmpVec3


   ! Integrate to find the portions of the mass matrix on and below the
   !   diagonal associated with purely blade DOFs (these portions can't
   !   be calculated using partial loads).  Also, integrate to find the
   !   portions of the forcing vector associated with purely blade DOFs
   !   (these portions can't be calculated using partial loads):
   ! NOTE: The vector subscript array, PSBE(), used in the following loops must
   !       be sorted from smallest to largest DOF index in order for the loops
   !       to work to enter values only on and below the diagonal of AugMat():

      DO L = 1,OtherState%DOFs%NPSBE(K)    ! Loop through all active (enabled) blade DOFs that contribute to the QD2T-related linear accelerations of the blade
         DO I = L,OtherState%DOFs%NPSBE(K) ! Loop through all active (enabled) blade DOFs greater than or equal to L
            AugMat(OtherState%DOFs%PSBE(K,I),OtherState%DOFs%PSBE(K,L)) = AugMat(OtherState%DOFs%PSBE(K,I),OtherState%DOFs%PSBE(K,L)) + ElmntMass*&
                                          DOT_PRODUCT( PLinVelES(K,J,OtherState%DOFs%PSBE(K,I),0,:), &           ! [C(q,t)]B
                                                       PLinVelES(K,J,OtherState%DOFs%PSBE(K,L),0,:)   )
         ENDDO             ! I - All active (enabled) blade DOFs greater than or equal to L
      ENDDO                ! L - All active (enabled) blade DOFs that contribute to the QD2T-related linear accelerations of the blade
      DO I = 1,OtherState%DOFs%NPSBE(K)    ! Loop through all active (enabled) blade DOFs that contribute to the QD2T-related linear accelerations of the blade
            AugMat(OtherState%DOFs%PSBE(K,I), p%NAug) = AugMat(OtherState%DOFs%PSBE(K,I),     p%NAug)                      & ! {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB
                                        + DOT_PRODUCT( PLinVelES(K,J,OtherState%DOFs%PSBE(K,I),0,:), TmpVec1 ) & ! NOTE: TmpVec1 is still the portion of FrcS0Bt associated with blade element J
                                        + DOT_PRODUCT( PAngVelEM(K,J,OtherState%DOFs%PSBE(K,I),0,:), TmpVec3 )   !       and TmpVec3 is still the total external moment applied to blade element J
      ENDDO                ! I - All active (enabled) blade DOFs that contribute to the QD2T-related linear accelerations of the blade


   ENDDO ! J - Blade nodes / elements



   ! Add the blade effects to the partial forces and moments (including
   !   those associated with the QD2T()'s and those that are not) at the
   !   teeter pin (point P):

   DO I = 1,OtherState%DOFs%NPSE(K)  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of blade K

      TmpVec = CROSS_PRODUCT( rPS0, PFrcS0B(K,OtherState%DOFs%PSE(K,I),:) ) ! The portion of PMomLPRot associated with PFrcS0B.

      PFrcPRot (OtherState%DOFs%PSE(K,I),:) = PFrcPRot (OtherState%DOFs%PSE(K,I),:) + PFrcS0B(K,OtherState%DOFs%PSE(K,I),:)

      PMomLPRot(OtherState%DOFs%PSE(K,I),:) = PMomLPRot(OtherState%DOFs%PSE(K,I),:) + PMomH0B(K,OtherState%DOFs%PSE(K,I),:)+TmpVec

   ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of blade K

   TmpVec = CROSS_PRODUCT( rPS0, FrcS0Bt(K,:) )       ! The portion of MomLPRott associated with FrcS0Bt.

   FrcPRott  = FrcPRott  + FrcS0Bt(K,:)

   MomLPRott = MomLPRott + MomH0Bt(K,:) + TmpVec



   ! Initialize the portions of the mass matrix below the diagonal associated
   !   with the teeter and pure blade DOFs using the partial loads at the
   !   teeter pin; only do this if necessary:

   IF ( ( p%NumBl == 2 ) .AND. ( p%DOF_Flag(DOF_Teet) ) )  THEN
      DO L = 1,OtherState%DOFs%NPSBE(K) ! Loop through all active (enabled) blade DOFs that contribute to the QD2T-related linear accelerations of the blade
         AugMat(DOF_Teet,OtherState%DOFs%PSBE(K,L)) = -DOT_PRODUCT( PAngVelEH(DOF_Teet,0,:), &
                                                            PMomLPRot(OtherState%DOFs%PSBE(K,L),:) )  ! [C(q,t)]B
      ENDDO             ! L - All active (enabled) blade DOFs that contribute to the QD2T-related linear accelerations of the blade
   ENDIF



   ! If the associated DOFs are enabled, add the blade elasticity and damping
   !   forces to the forcing vector (these portions can't be calculated using
   !   partial loads):

   IF ( p%DOF_Flag(DOF_BF(K,1)) )  THEN
      AugMat(    DOF_BF(K,1),p%NAug) = AugMat(DOF_BF(K,1),p%NAug)      & !
                                   - KBF(K,1,1)*x%QT( DOF_BF(K,1)) &
                                   - KBF(K,1,2)*x%QT( DOF_BF(K,2)) &
                                   - CBF(K,1,1)*x%QDT(DOF_BF(K,1)) &
                                   - CBF(K,1,2)*x%QDT(DOF_BF(K,2))
   ENDIF
   IF ( p%DOF_Flag(DOF_BF(K,2)) )  THEN
      AugMat(    DOF_BF(K,2),p%NAug) = AugMat(DOF_BF(K,2),p%NAug)      & ! {-f(qd,q,t)}ElasticB + {-f(qd,q,t)}DampB
                                   - KBF(K,2,1)*x%QT( DOF_BF(K,1)) &
                                   - KBF(K,2,2)*x%QT( DOF_BF(K,2)) &
                                   - CBF(K,2,1)*x%QDT(DOF_BF(K,1)) &
                                   - CBF(K,2,2)*x%QDT(DOF_BF(K,2))
   ENDIF
   IF ( p%DOF_Flag(DOF_BE(K,1)) )  THEN
      AugMat(    DOF_BE(K,1),p%NAug) = AugMat(DOF_BE(K,1),p%NAug)      & !
                                   - KBE(K,1,1)*x%QT( DOF_BE(K,1)) &
                                   - CBE(K,1,1)*x%QDT(DOF_BE(K,1))
   ENDIF


ENDDO ! K - Blades



   ! Define the partial forces and moments (including those associated with
   !   the QD2T()'s and those that are not) at the specified point on the
   !   rotor-furl axis (point V) / nacelle (body N) using the structure that
   !   furls with the rotor, generator, and rotor effects.

PFrcVGnRt = PFrcPRot    ! Initialize these partial forces and
PMomNGnRt = PMomLPRot   ! moments using the rotor effects
DO I = 1,OtherState%DOFs%NActvDOF ! Loop through all active (enabled) DOFs

   TmpVec = CROSS_PRODUCT( rVP, PFrcPRot(OtherState%DOFs%SrtPS(I),:) )  ! The portion of PMomNGnRt associated with the PFrcPRot

   PMomNGnRt(OtherState%DOFs%SrtPS(I),:) = PMomNGnRt(OtherState%DOFs%SrtPS(I),:) + TmpVec

ENDDO             ! I - All active (enabled) DOFs
DO I = 1,OtherState%DOFs%NPDE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the center of mass of the structure that furls with the rotor (not including rotor) (point D)

   TmpVec1 = -RFrlMass*PLinVelED(OtherState%DOFs%PDE(I)  ,0,:)           ! The portion of PFrcVGnRt associated with the RFrlMass
   TmpVec2 = CROSS_PRODUCT( rVD,              TmpVec1 )  ! The portion of PMomNGnRt associated with the RFrlMass

   PFrcVGnRt(OtherState%DOFs%PDE(I)  ,:) = PFrcVGnRt(OtherState%DOFs%PDE(I)  ,:) + TmpVec1

   PMomNGnRt(OtherState%DOFs%PDE(I)  ,:) = PMomNGnRt(OtherState%DOFs%PDE(I)  ,:) + TmpVec2                                   &
                         - RrfaIner*CoordSys%rfa*DOT_PRODUCT( CoordSys%rfa, PAngVelER(OtherState%DOFs%PDE(I)  ,0,:) ) &
                         -  GenIner*CoordSys%c1 *DOT_PRODUCT( CoordSys%c1 , PAngVelEG(OtherState%DOFs%PDE(I)  ,0,:) )

ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the center of mass of the structure that furls with the rotor (not including rotor) (point D)
IF ( p%DOF_Flag(DOF_GeAz) )  THEN

   PMomNGnRt(DOF_GeAz,:) = PMomNGnRt(DOF_GeAz,:)                                             &           ! The previous loop (DO I = 1,NPDE) misses the DOF_GeAz-contribution to: ( Generator inertia dyadic ) dot ( partial angular velocity of the generator in the inertia frame )
                         -  GenIner*CoordSys%c1 *DOT_PRODUCT( CoordSys%c1 , PAngVelEG(DOF_GeAz,0,:) )     ! Thus, add this contribution if necessary.

ENDIF

TmpVec1 = -RFrlMass*( Gravity*CoordSys%z2 + LinAccEDt )                    ! The portion of FrcVGnRtt associated with the RFrlMass
TmpVec2 = CROSS_PRODUCT( rVD      ,  TmpVec1 )                             ! The portion of MomNGnRtt associated with the RFrlMass
TmpVec3 = CROSS_PRODUCT( rVP      , FrcPRott )                             ! The portion of MomNGnRtt associated with the FrcPRott
TmpVec  = RrfaIner*CoordSys%rfa*DOT_PRODUCT( CoordSys%rfa, AngVelER )      ! = ( R inertia dyadic ) dot ( angular velocity of structure that furls with the rotor in the inertia frame )
TmpVec4 = CROSS_PRODUCT( -AngVelER, TmpVec )                               ! = ( -angular velocity of structure that furls with the rotor in the inertia frame ) cross ( TmpVec )
TmpVec  =  GenIner*CoordSys%c1* DOT_PRODUCT( CoordSys%c1 , AngVelEG )      ! = ( Generator inertia dyadic ) dot ( angular velocity of generator in the inertia frame )
TmpVec5 = CROSS_PRODUCT( -AngVelEG, TmpVec )                               ! = ( -angular velocity of generator in the inertia frame ) cross ( TmpVec )

FrcVGnRtt = FrcPRott  + TmpVec1

MomNGnRtt = MomLPRott + TmpVec2 + TmpVec3 + TmpVec4 + TmpVec5            &
          - RrfaIner*CoordSys%rfa*DOT_PRODUCT( CoordSys%rfa, AngAccERt ) &
          -  GenIner*CoordSys%c1 *DOT_PRODUCT( CoordSys%c1 , AngAccEGt )



   ! Let's compute the tail aerodynamic loads, if necessary:

IF ( CompAero )  THEN   ! Calculate the tail aerodynamic forces using AeroDyn.


   ! Compute TFinKFx, TFinKFy, and TFinKMz:

   CALL TFinAero( rK(1), -rK(3), rK(2),                  &
                  DOT_PRODUCT( LinVelEK,  CoordSys%p1 ), &
                  DOT_PRODUCT( LinVelEK, -CoordSys%p3 ), &
                  DOT_PRODUCT( LinVelEK,  CoordSys%p2 ), CoordSys, x, p )


   ! Vectorize these values into FKAero and MAAero:

   FKAero = TFinKFx*CoordSys%p1 - TFinKFy*CoordSys%p3
   MAAero = TFinKMz*CoordSys%p2


ELSE                    ! Wind turbine in vacuum, no aerodynamic forces.


   FKAero = 0.0
   MAAero = 0.0


ENDIF



   ! Define the partial forces and moments (including those associated with
   !   the QD2T()'s and those that are not) at the specified point on the
   !   tail-furl axis (point W) / nacelle (body N) using the tail effects.

PFrcWTail = 0.0   ! Initialize these partial
PMomNTail = 0.0   ! forces and moments to zero
DO I = 1,OtherState%DOFs%NPIE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the tail boom center of mass (point I)

   TmpVec1 = -BoomMass*PLinVelEI(OtherState%DOFs%PIE(I),0,:)    ! The portion of PFrcWTail associated with the BoomMass
   TmpVec2 = -TFinMass*PLinVelEJ(OtherState%DOFs%PIE(I),0,:)    ! The portion of PFrcWTail associated with the TFinMass
   TmpVec3 = CROSS_PRODUCT( rWI, TmpVec1 )                      ! The portion of PMomNTail associated with the BoomMass
   TmpVec4 = CROSS_PRODUCT( rWJ, TmpVec2 )                      ! The portion of PMomNTail associated with the TFinMass

   PFrcWTail(OtherState%DOFs%PIE(I),:) = TmpVec1 + TmpVec2

   PMomNTail(OtherState%DOFs%PIE(I),:) = TmpVec3 + TmpVec4 &
                       - AtfaIner*CoordSys%tfa*DOT_PRODUCT( CoordSys%tfa, PAngVelEA(OtherState%DOFs%PIE(I),0,:) )

ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the tail boom center of mass (point I)

TmpVec1 = -BoomMass*( Gravity*CoordSys%z2 + LinAccEIt )                 ! The portion of FrcWTailt associated with the BoomMass
TmpVec2 = -TFinMass*( Gravity*CoordSys%z2 + LinAccEJt )                 ! The portion of FrcWTailt associated with the TFinMass
TmpVec3 = CROSS_PRODUCT( rWI      , TmpVec1 )                           ! The portion of MomNTailt associated with the BoomMass
TmpVec4 = CROSS_PRODUCT( rWJ      , TmpVec2 )                           ! The portion of MomNTailt associated with the TFinMass
TmpVec  = AtfaIner*CoordSys%tfa*DOT_PRODUCT( CoordSys%tfa, AngVelEA )   ! = ( A inertia dyadic ) dot ( angular velocity of the tail in the inertia frame )
TmpVec5 = CROSS_PRODUCT( -AngVelEA, TmpVec  )                           ! = ( -angular velocity of the tail in the inertia frame ) cross ( TmpVec )
TmpVec  = CROSS_PRODUCT( rWK      , FKAero  )                           ! The portion of MomNTailt associated with FKAero

FrcWTailt = FKAero + TmpVec1 + TmpVec2

MomNTailt = MAAero + TmpVec3 + TmpVec4 + TmpVec5 + TmpVec &
          - AtfaIner*CoordSys%tfa*DOT_PRODUCT( CoordSys%tfa, AngAccEAt )



   ! Define the partial forces and moments (including those associated with
   !   the QD2T()'s and those that are not) at the yaw bearing (point O) /
   !   base plate (body B) using the nacelle, generator, rotor, and tail effects.

PFrcONcRt = PFrcVGnRt + PFrcWTail   ! Initialize these partial forces and moments using
PMomBNcRt = PMomNGnRt + PMomNTail   ! the rotor, rotor-furl, generator, and tail effects
DO I = 1,OtherState%DOFs%NActvDOF ! Loop through all active (enabled) DOFs

   TmpVec = CROSS_PRODUCT( rOV, PFrcVGnRt(OtherState%DOFs%SrtPS(I),:) ) ! The portion of PMomBNcRt associated with the PFrcVGnRt

   PMomBNcRt(OtherState%DOFs%SrtPS(I),:) = PMomBNcRt(OtherState%DOFs%SrtPS(I),:) + TmpVec

ENDDO             ! I - All active (enabled) DOFs
DO I = 1,OtherState%DOFs%NPIE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the tail boom center of mass (point I)

   TmpVec = CROSS_PRODUCT( rOW, PFrcWTail(OtherState%DOFs%PIE(I)  ,:) ) ! The portion of PMomBNcRt associated with the PFrcWTail

   PMomBNcRt(OtherState%DOFs%PIE(I)  ,:) = PMomBNcRt(OtherState%DOFs%PIE(I)  ,:) + TmpVec

ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the tail boom center of mass (point I)
DO I = 1,OtherState%DOFs%NPUE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the nacelle center of mass (point U)

   TmpVec1 = -NacMass*PLinVelEU(OtherState%DOFs%PUE(I),0,:)              ! The portion of PFrcONcRt associated with the NacMass
   TmpVec2 = CROSS_PRODUCT( rOU,               TmpVec1 ) ! The portion of PMomBNcRt associated with the NacMass

   PFrcONcRt(OtherState%DOFs%PUE(I)  ,:) = PFrcONcRt(OtherState%DOFs%PUE(I)  ,:) + TmpVec1

   PMomBNcRt(OtherState%DOFs%PUE(I)  ,:) = PMomBNcRt(OtherState%DOFs%PUE(I)  ,:) + TmpVec2 &
                         - Nacd2Iner*CoordSys%d2*DOT_PRODUCT( CoordSys%d2, PAngVelEN(OtherState%DOFs%PUE(I),0,:) )

ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the nacelle center of mass (point U)

TmpVec1 = -NacMass*( Gravity*CoordSys%z2 + LinAccEUt )                  ! The portion of FrcONcRtt associated with the NacMass
TmpVec2 = CROSS_PRODUCT( rOU      ,   TmpVec1 )                         ! The portion of MomBNcRtt associated with the NacMass
TmpVec3 = CROSS_PRODUCT( rOV      , FrcVGnRtt )                         ! The portion of MomBNcRtt associated with the FrcVGnRtt
TmpVec4 = CROSS_PRODUCT( rOW      , FrcWTailt )                         ! The portion of MomBNcRtt associated with the FrcWTailt
TmpVec  = Nacd2Iner*CoordSys%d2*DOT_PRODUCT( CoordSys%d2, AngVelEN )    ! = ( Nacelle inertia dyadic ) dot ( angular velocity of nacelle in the inertia frame )
TmpVec5 = CROSS_PRODUCT( -AngVelEN, TmpVec    )                         ! = ( -angular velocity of nacelle in the inertia frame ) cross ( TmpVec )

FrcONcRtt = FrcVGnRtt + FrcWTailt + TmpVec1

MomBNcRtt = MomNGnRtt + MomNTailt + TmpVec2 + TmpVec3 + TmpVec4 + TmpVec5 &
          - Nacd2Iner*CoordSys%d2*DOT_PRODUCT( CoordSys%d2, AngAccENt )



   ! Initialize the partial forces and moments (including those associated
   !   with the QD2T()'s and those that are not) at the tower base (point T(0))
   !   using everything but the tower:

PFrcT0Trb = PFrcONcRt   ! Initialize these partial forces and moments
PMomX0Trb = PMomBNcRt   ! using all of the effects above the yaw bearing
DO I = 1,OtherState%DOFs%NActvDOF ! Loop through all active (enabled) DOFs

   TmpVec  = CROSS_PRODUCT(  rT0O, PFrcONcRt(OtherState%DOFs%SrtPS(I),:) )   ! The portion of PMomX0Trb associated with the PFrcONcRt

   PMomX0Trb(OtherState%DOFs%SrtPS(I),:) = PMomX0Trb(OtherState%DOFs%SrtPS(I),:) + TmpVec

ENDDO             ! I - All active (enabled) DOFs
DO I = 1,OtherState%DOFs%NPTE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the yaw bearing center of mass (point O)

   TmpVec1 = -YawBrMass*PLinVelEO(OtherState%DOFs%PTE(I),0,:)               ! The portion of PFrcT0Trb associated with the YawBrMass
   TmpVec2 = CROSS_PRODUCT( rT0O,               TmpVec1 )   ! The portion of PMomX0Trb associated with the YawBrMass

   PFrcT0Trb(OtherState%DOFs%PTE(I)  ,:) = PFrcT0Trb(OtherState%DOFs%PTE(I)  ,:) + TmpVec1

   PMomX0Trb(OtherState%DOFs%PTE(I)  ,:) = PMomX0Trb(OtherState%DOFs%PTE(I)  ,:) + TmpVec2

ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the yaw bearing center of mass (point O)

TmpVec1 = -YawBrMass*( Gravity*CoordSys%z2 + LinAccEOt ) ! The portion of FrcT0Trbt associated with the YawBrMass
TmpVec2 = CROSS_PRODUCT( rT0O,   TmpVec1 )               ! The portion of MomX0Trbt associated with the YawBrMass
TmpVec3 = CROSS_PRODUCT( rT0O, FrcONcRtt )               ! The portion of MomX0Trbt associated with the FrcONcRtt

FrcT0Trbt = FrcONcRtt + TmpVec1

MomX0Trbt = MomBNcRtt + TmpVec2 + TmpVec3



   ! Initialize the portions of the mass matrix on and below the diagonal
   !   associated with purely tower DOFs (these portions can't be calculated
   !   using partial loads) using the yaw bearing mass effects.
   !   Also, initialize the portions of the forcing vector associated with
   !   purely blade DOFs (these portions can't be calculated using partial
   !   loads) using the yaw bearing mass effects:
   ! NOTE: The vector subscript array, PTTE(), used in the following loops must
   !       be sorted from smallest to largest DOF index in order for the loops
   !       to work to enter values only on and below the diagonal of AugMat():

DO L = 1,OtherState%DOFs%NPTTE    ! Loop through all active (enabled) tower DOFs that contribute to the QD2T-related linear accelerations of the yaw bearing (point O)
   DO I = L,OtherState%DOFs%NPTTE ! Loop through all active (enabled) tower DOFs greater than or equal to L
      AugMat(OtherState%DOFs%PTTE(I),OtherState%DOFs%PTTE(L)) = YawBrMass*DOT_PRODUCT( PLinVelEO(OtherState%DOFs%PTTE(I),0,:), &     ! [C(q,t)]T of YawBrMass
                                                   PLinVelEO(OtherState%DOFs%PTTE(L),0,:)    )
   ENDDO          ! I - All active (enabled) tower DOFs greater than or equal to L
ENDDO             ! L - All active (enabled) tower DOFs that contribute to the QD2T-related linear accelerations of the yaw bearing (point O)
DO I = 1,OtherState%DOFs%NPTTE    ! Loop through all active (enabled) tower DOFs that contribute to the QD2T-related linear accelerations of the yaw bearing (point O)
      AugMat(OtherState%DOFs%PTTE(I),   p%NAug) =           DOT_PRODUCT( PLinVelEO(OtherState%DOFs%PTTE(I),0,:), &     ! {-f(qd,q,t)}T + {-f(qd,q,t)}GravT of YawBrMass
                                                   TmpVec1                   )   ! NOTE: TmpVec1 is still the portion of FrcT0Trbt associated with YawBrMass
ENDDO             ! I - All active (enabled) tower DOFs that contribute to the QD2T-related linear accelerations of the yaw bearing (point O)

!----------------------------------------------------------------------------------------------------
! Get the tower element positions, velocities, and partial velocities
!----------------------------------------------------------------------------------------------------


DO J = 1,p%TwrNodes  ! Loop through the tower nodes / elements


   ! Calculate the position vector of the current node:

   rT0T(J,:) = ( TwrFASF(1,J,0)*x%QT(DOF_TFA1) + TwrFASF(2,J,0)*x%QT(DOF_TFA2)             )*CoordSys%a1 &   ! Position vector from base of flexible portion of tower (point T(0)) to current node (point T(J)).
             + ( HNodes(J) - 0.5*(       AxRedTFA(1,1,J)*x%QT(DOF_TFA1)*x%QT(DOF_TFA1) &
                                   +     AxRedTFA(2,2,J)*x%QT(DOF_TFA2)*x%QT(DOF_TFA2) &
                                   + 2.0*AxRedTFA(1,2,J)*x%QT(DOF_TFA1)*x%QT(DOF_TFA2) &
                                   +     AxRedTSS(1,1,J)*x%QT(DOF_TSS1)*x%QT(DOF_TSS1) &
                                   +     AxRedTSS(2,2,J)*x%QT(DOF_TSS2)*x%QT(DOF_TSS2) &
                                   + 2.0*AxRedTSS(1,2,J)*x%QT(DOF_TSS1)*x%QT(DOF_TSS2)   ) )*CoordSys%a2 &
             + ( TwrSSSF(1,J,0)*x%QT(DOF_TSS1) + TwrSSSF(2,J,0)*x%QT(DOF_TSS2)             )*CoordSys%a3
   rZT (J,:) = rZT0 + rT0T(J,:)                                                                          ! Position vector from platform reference (point Z) to the current node (point T(HNodes(J)).


   rT(J,:)      = rZ   + rZT (J,:)                                                                       ! Position vector from inertial frame origin        to the current node (point T(HNodes(J)).


   ! Define the partial angular velocities (and their 1st derivatives) of the
   !   current node (body F(HNodes(J))  in the inertia frame.  Also define
   !   the overall angular velocity of the current node in the inertia frame.
   !   Also, define the portion of the angular acceleration of the current node
   !   in the inertia frame associated with everything but the QD2T()'s:

   ! NOTE: PAngVelEF(J,I,D,:) = the Dth-derivative of the partial angular velocity
   !   of DOF I for body F of element J in body E.

   PAngVelEF (J,       :,0,:) = PAngVelEX(:,0,:)
   PAngVelEF (J,DOF_TFA1,0,:) = -TwrFASF(1,J,1)*CoordSys%a3
   PAngVelEF (J,DOF_TSS1,0,:) =  TwrSSSF(1,J,1)*CoordSys%a1
   PAngVelEF (J,DOF_TFA2,0,:) = -TwrFASF(2,J,1)*CoordSys%a3
   PAngVelEF (J,DOF_TSS2,0,:) =  TwrSSSF(2,J,1)*CoordSys%a1

   PAngVelEF (J,       :,1,:) = PAngVelEX(:,1,:)
   PAngVelEF (J,DOF_TFA1,1,:) = CROSS_PRODUCT(  AngVelEX  ,                 PAngVelEF(J,DOF_TFA1,0,:) )
   PAngVelEF (J,DOF_TSS1,1,:) = CROSS_PRODUCT(  AngVelEX  ,                 PAngVelEF(J,DOF_TSS1,0,:) )
   PAngVelEF (J,DOF_TFA2,1,:) = CROSS_PRODUCT(  AngVelEX  ,                 PAngVelEF(J,DOF_TFA2,0,:) )
   PAngVelEF (J,DOF_TSS2,1,:) = CROSS_PRODUCT(  AngVelEX  ,                 PAngVelEF(J,DOF_TSS2,0,:) )


    AngVelEF (J,:)            =                 AngVelEX  + x%QDT(DOF_TFA1)*PAngVelEF(J,DOF_TFA1,0,:) &
                                                          + x%QDT(DOF_TSS1)*PAngVelEF(J,DOF_TSS1,0,:) &
                                                          + x%QDT(DOF_TFA2)*PAngVelEF(J,DOF_TFA2,0,:) &
                                                          + x%QDT(DOF_TSS2)*PAngVelEF(J,DOF_TSS2,0,:)

    AngPosXF (J,:)            =                             x%QT (DOF_TFA1)*PAngVelEF(J,DOF_TFA1,0,:) &
                                                          + x%QT (DOF_TSS1)*PAngVelEF(J,DOF_TSS1,0,:) &
                                                          + x%QT (DOF_TFA2)*PAngVelEF(J,DOF_TFA2,0,:) &
                                                          + x%QT (DOF_TSS2)*PAngVelEF(J,DOF_TSS2,0,:)
    AngPosEF (J,:)            =                 AngPosEX  + AngPosXF(J,:)
    AngAccEFt(J,:)            =                 AngAccEXt + x%QDT(DOF_TFA1)*PAngVelEF(J,DOF_TFA1,1,:) &
                                                          + x%QDT(DOF_TSS1)*PAngVelEF(J,DOF_TSS1,1,:) &
                                                          + x%QDT(DOF_TFA2)*PAngVelEF(J,DOF_TFA2,1,:) &
                                                          + x%QDT(DOF_TSS2)*PAngVelEF(J,DOF_TSS2,1,:)


   ! Define the partial linear velocities (and their 1st derivatives) of the
   !   current node (point T(HNodes(J))) in the inertia frame.  Also define
   !   the overall linear velocity of the current node in the inertia frame.
   !   Also, define the portion of the linear acceleration of the current node
   !   in the inertia frame associated with everything but the QD2T()'s:

   EwXXrZT                   = CROSS_PRODUCT(  AngVelEX, rZT(J,:) )

   PLinVelET(J,       :,:,:) = PLinVelEZ(:,:,:)
   PLinVelET(J,DOF_TFA1,0,:) = TwrFASF(1,J,0)*CoordSys%a1 - (   AxRedTFA(1,1,J)* x%QT(DOF_TFA1) &
                                                              + AxRedTFA(1,2,J)* x%QT(DOF_TFA2)   )*CoordSys%a2
   PLinVelET(J,DOF_TSS1,0,:) = TwrSSSF(1,J,0)*CoordSys%a3 - (   AxRedTSS(1,1,J)* x%QT(DOF_TSS1) &
                                                              + AxRedTSS(1,2,J)* x%QT(DOF_TSS2)   )*CoordSys%a2
   PLinVelET(J,DOF_TFA2,0,:) = TwrFASF(2,J,0)*CoordSys%a1 - (   AxRedTFA(2,2,J)* x%QT(DOF_TFA2) &
                                                              + AxRedTFA(1,2,J)* x%QT(DOF_TFA1)   )*CoordSys%a2
   PLinVelET(J,DOF_TSS2,0,:) = TwrSSSF(2,J,0)*CoordSys%a3 - (   AxRedTSS(2,2,J)* x%QT(DOF_TSS2) &
                                                              + AxRedTSS(1,2,J)* x%QT(DOF_TSS1)   )*CoordSys%a2

   TmpVec1                   = CROSS_PRODUCT( AngVelEX, PLinVelET(J,DOF_TFA1,0,:) )
   TmpVec2                   = CROSS_PRODUCT( AngVelEX, PLinVelET(J,DOF_TSS1,0,:) )
   TmpVec3                   = CROSS_PRODUCT( AngVelEX, PLinVelET(J,DOF_TFA2,0,:) )
   TmpVec4                   = CROSS_PRODUCT( AngVelEX, PLinVelET(J,DOF_TSS2,0,:) )

   PLinVelET(J,DOF_TFA1,1,:) = TmpVec1                    - (   AxRedTFA(1,1,J)*x%QDT(DOF_TFA1) &
                                                              + AxRedTFA(1,2,J)*x%QDT(DOF_TFA2)   )*CoordSys%a2
   PLinVelET(J,DOF_TSS1,1,:) = TmpVec2                    - (   AxRedTSS(1,1,J)*x%QDT(DOF_TSS1) &
                                                              + AxRedTSS(1,2,J)*x%QDT(DOF_TSS2)   )*CoordSys%a2
   PLinVelET(J,DOF_TFA2,1,:) = TmpVec3                    - (   AxRedTFA(2,2,J)*x%QDT(DOF_TFA2) &
                                                              + AxRedTFA(1,2,J)*x%QDT(DOF_TFA1)   )*CoordSys%a2
   PLinVelET(J,DOF_TSS2,1,:) = TmpVec4                    - (   AxRedTSS(2,2,J)*x%QDT(DOF_TSS2) &
                                                              + AxRedTSS(1,2,J)*x%QDT(DOF_TSS1)   )*CoordSys%a2

   LinVelXT       = x%QDT(DOF_TFA1)*PLinVelET(J,DOF_TFA1,0,:) &
                  + x%QDT(DOF_TSS1)*PLinVelET(J,DOF_TSS1,0,:) &
                  + x%QDT(DOF_TFA2)*PLinVelET(J,DOF_TFA2,0,:) &
                  + x%QDT(DOF_TSS2)*PLinVelET(J,DOF_TSS2,0,:)
   LinAccETt(J,:) = x%QDT(DOF_TFA1)*PLinVelET(J,DOF_TFA1,1,:) &
                  + x%QDT(DOF_TSS1)*PLinVelET(J,DOF_TSS1,1,:) &
                  + x%QDT(DOF_TFA2)*PLinVelET(J,DOF_TFA2,1,:) &
                  + x%QDT(DOF_TSS2)*PLinVelET(J,DOF_TSS2,1,:)

   LinVelET(J,:)  = LinVelXT + LinVelEZ
   DO I = 1,NPX   ! Loop through all DOFs associated with the angular motion of the platform (body X)

      TmpVec0   = CROSS_PRODUCT( PAngVelEX(PX(I),0,:),     rZT(J,:)            )
      TmpVec1   = CROSS_PRODUCT( PAngVelEX(PX(I),0,:), EwXXrZT      + LinVelXT )

      PLinVelET(J,PX(I),0,:) = PLinVelET(J,PX(I),0,:) + TmpVec0
      PLinVelET(J,PX(I),1,:) = PLinVelET(J,PX(I),1,:) + TmpVec1

      LinVelET( J,        :) = LinVelET( J,        :) + x%QDT(PX(I))*PLinVelET(J,PX(I),0,:)
      LinAccETt(J,        :) = LinAccETt(J,        :) + x%QDT(PX(I))*PLinVelET(J,PX(I),1,:)

   ENDDO          ! I - all DOFs associated with the angular motion of the platform (body X)


!----------------------------------------------------------------------------------------------------
! Calculate tower loads (aerodynamic and hydrodynamic)
!----------------------------------------------------------------------------------------------------
   ! Calculate the aerodynamic forces and moments per unit length at the
   !   current tower element:
   ! NOTE: FTAero(J,:) = aerodynamic force per unit length acting on tower node J.
   ! NOTE: MFAero(J,:) = aerodynamic moment per unit length acting on tower element F at node J.

   IF ( CompAero )  THEN   ! Calculate the tower element aerodynamic loads using AeroDyn.

      FTAero(J,:) = 0.0    !JASON: ADD TOWER AERODYNAMIC LOAD CALCULATIONS HERE!!!
      MFAero(J,:) = 0.0    !JASON: ADD TOWER AERODYNAMIC LOAD CALCULATIONS HERE!!!

   ELSE                    ! Wind turbine in vacuum, no aerodynamic forces.

      FTAero(J,:) = 0.0
      MFAero(J,:) = 0.0

   ENDIF

   ! Let's compute the tower hydrodynamic loading; that is TwrAM(1:6,1:6) and
   !   TwrFt(1:6).


   CALL TwrLoading ( J, rT(   J,1), -rT(      J,3), ( rT(      J,2) - p%PtfmRef ), AngPosEF(J,1), -AngPosEF(J,3), AngPosEF(J,2), &
                     LinVelET(J,1), -LinVelET(J,3),   LinVelET(J,2)            , AngVelEF(J,1), -AngVelEF(J,3), AngVelEF(J,2)  )




   ! Compute the partial hydrodynamic forces and moments per unit length
   !   (including those associated with the QD2T()'s and those that are not) at
   !   the current tower element (point T) / (body F):

   ! NOTE: These forces are named PFTHydro, PMFHydro, FTHydrot, and MFHydrot.
   !       However, the names should not imply that the forces are a result of
   !       hydrodynamic contributions only.  These tower forces contain
   !       contributions from any external load acting on the tower other
   !       than loads transmitted from aerodynamics.  For example, these tower
   !       forces contain contributions from foundation stiffness and damping
   !       [not floating] or mooring line restoring and damping, as well as
   !       hydrostatic and hydrodynamic contributions [offshore].

   PFTHydro(J,:,:) = 0.0
   PMFHydro(J,:,:) = 0.0
   DO I = 1,OtherState%DOFs%NPTE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the tower

      PFTHydro(J,OtherState%DOFs%PTE(I),:) = CoordSys%z1*( - TwrAM(DOF_Sg,DOF_Sg)*PLinVelET(J,OtherState%DOFs%PTE(I),0,1) &
                                           + TwrAM(DOF_Sg,DOF_Sw)*PLinVelET(J,OtherState%DOFs%PTE(I),0,3) &
                                           - TwrAM(DOF_Sg,DOF_Hv)*PLinVelET(J,OtherState%DOFs%PTE(I),0,2) &
                                           - TwrAM(DOF_Sg,DOF_R )*PAngVelEF(J,OtherState%DOFs%PTE(I),0,1) &
                                           + TwrAM(DOF_Sg,DOF_P )*PAngVelEF(J,OtherState%DOFs%PTE(I),0,3) &
                                           - TwrAM(DOF_Sg,DOF_Y )*PAngVelEF(J,OtherState%DOFs%PTE(I),0,2)   ) &
                           - CoordSys%z3*( - TwrAM(DOF_Sw,DOF_Sg)*PLinVelET(J,OtherState%DOFs%PTE(I),0,1) &
                                           + TwrAM(DOF_Sw,DOF_Sw)*PLinVelET(J,OtherState%DOFs%PTE(I),0,3) &
                                           - TwrAM(DOF_Sw,DOF_Hv)*PLinVelET(J,OtherState%DOFs%PTE(I),0,2) &
                                           - TwrAM(DOF_Sw,DOF_R )*PAngVelEF(J,OtherState%DOFs%PTE(I),0,1) &
                                           + TwrAM(DOF_Sw,DOF_P )*PAngVelEF(J,OtherState%DOFs%PTE(I),0,3) &
                                           - TwrAM(DOF_Sw,DOF_Y )*PAngVelEF(J,OtherState%DOFs%PTE(I),0,2)   ) &
                           + CoordSys%z2*( - TwrAM(DOF_Hv,DOF_Sg)*PLinVelET(J,OtherState%DOFs%PTE(I),0,1) &
                                           + TwrAM(DOF_Hv,DOF_Sw)*PLinVelET(J,OtherState%DOFs%PTE(I),0,3) &
                                           - TwrAM(DOF_Hv,DOF_Hv)*PLinVelET(J,OtherState%DOFs%PTE(I),0,2) &
                                           - TwrAM(DOF_Hv,DOF_R )*PAngVelEF(J,OtherState%DOFs%PTE(I),0,1) &
                                           + TwrAM(DOF_Hv,DOF_P )*PAngVelEF(J,OtherState%DOFs%PTE(I),0,3) &
                                           - TwrAM(DOF_Hv,DOF_Y )*PAngVelEF(J,OtherState%DOFs%PTE(I),0,2)   )
      PMFHydro(J,OtherState%DOFs%PTE(I),:) = CoordSys%z1*( - TwrAM(DOF_R ,DOF_Sg)*PLinVelET(J,OtherState%DOFs%PTE(I),0,1) &
                                           + TwrAM(DOF_R ,DOF_Sw)*PLinVelET(J,OtherState%DOFs%PTE(I),0,3) &
                                           - TwrAM(DOF_R ,DOF_Hv)*PLinVelET(J,OtherState%DOFs%PTE(I),0,2) &
                                           - TwrAM(DOF_R ,DOF_R )*PAngVelEF(J,OtherState%DOFs%PTE(I),0,1) &
                                           + TwrAM(DOF_R ,DOF_P )*PAngVelEF(J,OtherState%DOFs%PTE(I),0,3) &
                                           - TwrAM(DOF_R ,DOF_Y )*PAngVelEF(J,OtherState%DOFs%PTE(I),0,2)   ) &
                           - CoordSys%z3*( - TwrAM(DOF_P ,DOF_Sg)*PLinVelET(J,OtherState%DOFs%PTE(I),0,1) &
                                           + TwrAM(DOF_P ,DOF_Sw)*PLinVelET(J,OtherState%DOFs%PTE(I),0,3) &
                                           - TwrAM(DOF_P ,DOF_Hv)*PLinVelET(J,OtherState%DOFs%PTE(I),0,2) &
                                           - TwrAM(DOF_P ,DOF_R )*PAngVelEF(J,OtherState%DOFs%PTE(I),0,1) &
                                           + TwrAM(DOF_P ,DOF_P )*PAngVelEF(J,OtherState%DOFs%PTE(I),0,3) &
                                           - TwrAM(DOF_P ,DOF_Y )*PAngVelEF(J,OtherState%DOFs%PTE(I),0,2)   ) &
                           + CoordSys%z2*( - TwrAM(DOF_Y ,DOF_Sg)*PLinVelET(J,OtherState%DOFs%PTE(I),0,1) &
                                           + TwrAM(DOF_Y ,DOF_Sw)*PLinVelET(J,OtherState%DOFs%PTE(I),0,3) &
                                           - TwrAM(DOF_Y ,DOF_Hv)*PLinVelET(J,OtherState%DOFs%PTE(I),0,2) &
                                           - TwrAM(DOF_Y ,DOF_R )*PAngVelEF(J,OtherState%DOFs%PTE(I),0,1) &
                                           + TwrAM(DOF_Y ,DOF_P )*PAngVelEF(J,OtherState%DOFs%PTE(I),0,3) &
                                           - TwrAM(DOF_Y ,DOF_Y )*PAngVelEF(J,OtherState%DOFs%PTE(I),0,2)   )

   ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the tower

   FTHydrot(J,:) = CoordSys%z1*( TwrFt(DOF_Sg) - TwrAM(DOF_Sg,DOF_Sg)*LinAccETt(J,1) &
                                               + TwrAM(DOF_Sg,DOF_Sw)*LinAccETt(J,3) &
                                               - TwrAM(DOF_Sg,DOF_Hv)*LinAccETt(J,2) &
                                               - TwrAM(DOF_Sg,DOF_R )*AngAccEFt(J,1) &
                                               + TwrAM(DOF_Sg,DOF_P )*AngAccEFt(J,3) &
                                               - TwrAM(DOF_Sg,DOF_Y )*AngAccEFt(J,2)   ) &
                 - CoordSys%z3*( TwrFt(DOF_Sw) - TwrAM(DOF_Sw,DOF_Sg)*LinAccETt(J,1) &
                                               + TwrAM(DOF_Sw,DOF_Sw)*LinAccETt(J,3) &
                                               - TwrAM(DOF_Sw,DOF_Hv)*LinAccETt(J,2) &
                                               - TwrAM(DOF_Sw,DOF_R )*AngAccEFt(J,1) &
                                               + TwrAM(DOF_Sw,DOF_P )*AngAccEFt(J,3) &
                                               - TwrAM(DOF_Sw,DOF_Y )*AngAccEFt(J,2)   ) &
                 + CoordSys%z2*( TwrFt(DOF_Hv) - TwrAM(DOF_Hv,DOF_Sg)*LinAccETt(J,1) &
                                               + TwrAM(DOF_Hv,DOF_Sw)*LinAccETt(J,3) &
                                               - TwrAM(DOF_Hv,DOF_Hv)*LinAccETt(J,2) &
                                               - TwrAM(DOF_Hv,DOF_R )*AngAccEFt(J,1) &
                                               + TwrAM(DOF_Hv,DOF_P )*AngAccEFt(J,3) &
                                               - TwrAM(DOF_Hv,DOF_Y )*AngAccEFt(J,2)   )
   MFHydrot(J,:) = CoordSys%z1*( TwrFt(DOF_R ) - TwrAM(DOF_R ,DOF_Sg)*LinAccETt(J,1) &
                                               + TwrAM(DOF_R ,DOF_Sw)*LinAccETt(J,3) &
                                               - TwrAM(DOF_R ,DOF_Hv)*LinAccETt(J,2) &
                                               - TwrAM(DOF_R ,DOF_R )*AngAccEFt(J,1) &
                                               + TwrAM(DOF_R ,DOF_P )*AngAccEFt(J,3) &
                                               - TwrAM(DOF_R ,DOF_Y )*AngAccEFt(J,2)   ) &
                 - CoordSys%z3*( TwrFt(DOF_P ) - TwrAM(DOF_P ,DOF_Sg)*LinAccETt(J,1) &
                                               + TwrAM(DOF_P ,DOF_Sw)*LinAccETt(J,3) &
                                               - TwrAM(DOF_P ,DOF_Hv)*LinAccETt(J,2) &
                                               - TwrAM(DOF_P ,DOF_R )*AngAccEFt(J,1) &
                                               + TwrAM(DOF_P ,DOF_P )*AngAccEFt(J,3) &
                                               - TwrAM(DOF_P ,DOF_Y )*AngAccEFt(J,2)   ) &
                 + CoordSys%z2*( TwrFt(DOF_Y ) - TwrAM(DOF_Y ,DOF_Sg)*LinAccETt(J,1) &
                                               + TwrAM(DOF_Y ,DOF_Sw)*LinAccETt(J,3) &
                                               - TwrAM(DOF_Y ,DOF_Hv)*LinAccETt(J,2) &
                                               - TwrAM(DOF_Y ,DOF_R )*AngAccEFt(J,1) &
                                               + TwrAM(DOF_Y ,DOF_P )*AngAccEFt(J,3) &
                                               - TwrAM(DOF_Y ,DOF_Y )*AngAccEFt(J,2)   )

   ! Calculate the mass of the current element:

   ElmntMass = MassT(J)*DHNodes(J)   ! Mass of tower element J


   ! Integrate to find the total partial forces and moments (including those
   !   associated with the QD2T()'s and those that are not) at the tower base
   !   (point T(0)):

   DO I = 1,OtherState%DOFs%NPTE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the tower

      TmpVec1 = PFTHydro(J,OtherState%DOFs%PTE(I),:)*DHNodes(J) &
              - ElmntMass*PLinVelET(J,OtherState%DOFs%PTE(I),0,:)           ! The portion of PFrcT0Trb associated with tower element J
      TmpVec2 = CROSS_PRODUCT( rT0T(J,:), TmpVec1 )         ! The portion of PMomX0Trb associated with tower element J
      TmpVec3 = PMFHydro(J,OtherState%DOFs%PTE(I),:)*DHNodes(J)             ! The added moment applied at tower element J

      PFrcT0Trb(OtherState%DOFs%PTE(I),:) = PFrcT0Trb(OtherState%DOFs%PTE(I),:) + TmpVec1

      PMomX0Trb(OtherState%DOFs%PTE(I),:) = PMomX0Trb(OtherState%DOFs%PTE(I),:) + TmpVec2 + TmpVec3

   ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the tower

   TmpVec1 = ( FTAero(J,:) + FTHydrot(J,:) )*DHNodes(J) &
           - ElmntMass*( Gravity*CoordSys%z2 + LinAccETt(J,:) )   ! The portion of FrcT0Trbt associated with tower element J
   TmpVec2 = CROSS_PRODUCT( rT0T(J,:), TmpVec1 )                  ! The portion of MomX0Trbt associated with tower element J
   TmpVec3 = ( MFAero(J,:) + MFHydrot(J,:) )*DHNodes(J)           ! The external moment applied to tower element J

   FrcT0Trbt = FrcT0Trbt + TmpVec1

   MomX0Trbt = MomX0Trbt + TmpVec2 + TmpVec3


   ! Integrate to find the portions of the mass matrix on and below the
   !   diagonal associated with purely tower DOFs (these portions can't
   !   be calculated using partial loads).  Also, integrate to find the
   !   portions of the forcing vector associated with purely tower DOFs
   !   (these portions can't be calculated using partial loads).
   ! NOTE: The vector subscript array, PTTE(), used in the following loops must
   !       be sorted from smallest to largest DOF index in order for the loops
   !       to work to enter values only on and below the diagonal of AugMat():

   DO L = 1,OtherState%DOFs%NPTTE    ! Loop through all active (enabled) tower DOFs that contribute to the QD2T-related linear accelerations of the tower
      DO I = L,OtherState%DOFs%NPTTE ! Loop through all active (enabled) tower DOFs greater than or equal to L
         AugMat(OtherState%DOFs%PTTE(I),OtherState%DOFs%PTTE(L)) = AugMat(OtherState%DOFs%PTTE(I),OtherState%DOFs%PTTE(L))  &
                                 + ElmntMass *DOT_PRODUCT( PLinVelET(J,OtherState%DOFs%PTTE(I),0,:),  &
                                                           PLinVelET(J,OtherState%DOFs%PTTE(L),0,:) ) &  ! [C(q,t)]T + [C(q,t)]HydroT
                                 - DHNodes(J)*DOT_PRODUCT( PLinVelET(J,OtherState%DOFs%PTTE(I),0,:),  &
                                                           PFTHydro (J,OtherState%DOFs%PTTE(L),  :) ) &
                                 - DHNodes(J)*DOT_PRODUCT( PAngVelEF(J,OtherState%DOFs%PTTE(I),0,:),  &
                                                           PMFHydro (J,OtherState%DOFs%PTTE(L),  :) )
      ENDDO          ! I - All active (enabled) tower DOFs greater than or equal to L
   ENDDO             ! L - All active (enabled) tower DOFs that contribute to the QD2T-related linear accelerations of the tower
   DO I = 1,OtherState%DOFs%NPTTE    ! Loop through all active (enabled) tower DOFs that contribute to the QD2T-related linear accelerations of the tower
         AugMat(OtherState%DOFs%PTTE(I),   p%NAug) = AugMat(OtherState%DOFs%PTTE(I),   p%NAug)                         &      ! {-f(qd,q,t)}T + {-f(qd,q,t)}GravT + {-f(qd,q,t)}AeroT + {-f(qd,q,t)}HydroT
                                 +            DOT_PRODUCT( PLinVelET(J,OtherState%DOFs%PTTE(I),0,:), TmpVec1             ) &  ! NOTE: TmpVec1 is still the portion of FrcT0Trbt associated with tower element J
                                 +            DOT_PRODUCT( PAngVelEF(J,OtherState%DOFs%PTTE(I),0,:), TmpVec3             )    !       and TmpVec3 is still the total external moment to tower element J
   ENDDO             ! I - All active (enabled) tower DOFs that contribute to the QD2T-related linear accelerations of the tower

ENDDO ! J - Tower nodes / elements


   ! If the associated DOFs are enabled, add the tower elasticity and damping
   !   forces to the forcing vector (these portions can't be calculated using
   !   partial loads):

IF ( p%DOF_Flag(DOF_TFA1) )  THEN
   AugMat(    DOF_TFA1,p%NAug) = AugMat(DOF_TFA1,p%NAug)                             &
                             - KTFA(1,1)*x%QT( DOF_TFA1) - KTFA(1,2)*x%QT( DOF_TFA2) &  !
                             - CTFA(1,1)*x%QDT(DOF_TFA1) - CTFA(1,2)*x%QDT(DOF_TFA2)
ENDIF
IF ( p%DOF_Flag(DOF_TSS1) )  THEN
   AugMat(    DOF_TSS1,p%NAug) = AugMat(DOF_TSS1,p%NAug)                             &
                             - KTSS(1,1)*x%QT( DOF_TSS1) - KTSS(1,2)*x%QT( DOF_TSS2) &  ! {-f(qd,q,t)}ElasticT + {-f(qd,q,t)}DampT
                             - CTSS(1,1)*x%QDT(DOF_TSS1) - CTSS(1,2)*x%QDT(DOF_TSS2)
ENDIF
IF ( p%DOF_Flag(DOF_TFA2) )  THEN
   AugMat(    DOF_TFA2,p%NAug) = AugMat(DOF_TFA2,p%NAug)                             &
                             - KTFA(2,1)*x%QT( DOF_TFA1) - KTFA(2,2)*x%QT( DOF_TFA2) &  !
                             - CTFA(2,1)*x%QDT(DOF_TFA1) - CTFA(2,2)*x%QDT(DOF_TFA2)
ENDIF
IF ( p%DOF_Flag(DOF_TSS2) )  THEN
   AugMat(    DOF_TSS2,p%NAug) = AugMat(DOF_TSS2,p%NAug)                             &
                             - KTSS(2,1)*x%QT( DOF_TSS1) - KTSS(2,2)*x%QT( DOF_TSS2) &  !
                             - CTSS(2,1)*x%QDT(DOF_TSS1) - CTSS(2,2)*x%QDT(DOF_TSS2)
ENDIF



   ! Let's compute the platform loading; that is PtfmAM(1:6,1:6), and
   !   PtfmFt(1:6):

CALL PtfmLoading( x )


   ! Compute the partial platform forces and moments (including those
   !   associated with the QD2T()'s and those that are not) at the platform
   !   reference (point Z) / (body X).
   ! NOTE: These forces are named PFZHydro, PMXHydro, FZHydrot, and MXHydrot.
   !       However, the names should not imply that the forces are a result of
   !       hydrodynamic contributions only.  These platform forces contain
   !       contributions from any external load acting on the platform other
   !       than loads transmitted from the wind turbine.  For example, these
   !       platform forces contain contributions from foundation stiffness and
   !       damping [not floating] or mooring line restoring and damping
   !       [floating], as well as hydrostatic and hydrodynamic contributions
   !       [offshore].

PFZHydro = 0.0
PMXHydro = 0.0
DO I = 1,OtherState%DOFs%NPYE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the platform center of mass (point Y)

   PFZHydro(OtherState%DOFs%PYE(I),:) = - PtfmAM(DOF_Sg,OtherState%DOFs%PYE(I))*PLinVelEZ(DOF_Sg,0,:) &
                                        - PtfmAM(DOF_Sw,OtherState%DOFs%PYE(I))*PLinVelEZ(DOF_Sw,0,:) &
                                        - PtfmAM(DOF_Hv,OtherState%DOFs%PYE(I))*PLinVelEZ(DOF_Hv,0,:)
   PMXHydro(OtherState%DOFs%PYE(I),:) = - PtfmAM(DOF_R ,OtherState%DOFs%PYE(I))*PAngVelEX(DOF_R ,0,:) &
                                        - PtfmAM(DOF_P ,OtherState%DOFs%PYE(I))*PAngVelEX(DOF_P ,0,:) &
                                        - PtfmAM(DOF_Y ,OtherState%DOFs%PYE(I))*PAngVelEX(DOF_Y ,0,:)

ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the platform center of mass (point Y)

FZHydrot = PtfmFt(DOF_Sg)*PLinVelEZ(DOF_Sg,0,:) &
         + PtfmFt(DOF_Sw)*PLinVelEZ(DOF_Sw,0,:) &
         + PtfmFt(DOF_Hv)*PLinVelEZ(DOF_Hv,0,:)
MXHydrot = PtfmFt(DOF_R )*PAngVelEX(DOF_R ,0,:) &
         + PtfmFt(DOF_P )*PAngVelEX(DOF_P ,0,:) &
         + PtfmFt(DOF_Y )*PAngVelEX(DOF_Y ,0,:)



   ! Define the partial forces and moments (including those associated with
   !   the QD2T()'s and those that are not) at the platform reference (point Z)
   !   / (body X) using the turbine and platform effects:

PFrcZAll = PFrcT0Trb ! Initialize these partial forces and moments
PMomXAll = PMomX0Trb ! using the effects from the wind turbine
DO I = 1,OtherState%DOFs%NActvDOF ! Loop through all active (enabled) DOFs

   TmpVec = CROSS_PRODUCT( rZT0, PFrcT0Trb(OtherState%DOFs%SrtPS(I),:) )   ! The portion of PMomXAll associated with the PFrcT0Trb

   PMomXAll(OtherState%DOFs%SrtPS(I),:) = PMomXAll(OtherState%DOFs%SrtPS(I),:) + TmpVec

ENDDO             ! I - All active (enabled) DOFs
DO I = 1,OtherState%DOFs%NPYE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the platform center of mass (point Y)

   TmpVec1 = -PtfmMass*PLinVelEY(OtherState%DOFs%PYE(I),0,:)                ! The portion of PFrcZAll associated with the PtfmMass
   TmpVec2 = CROSS_PRODUCT( rZY ,               TmpVec1 )   ! The portion of PMomXAll associated with the PtfmMass

   PFrcZAll(OtherState%DOFs%PYE(I)  ,:) = PFrcZAll(OtherState%DOFs%PYE(I)  ,:) + PFZHydro(OtherState%DOFs%PYE(I),:) + TmpVec1

   PMomXAll(OtherState%DOFs%PYE(I)  ,:) = PMomXAll(OtherState%DOFs%PYE(I)  ,:) + PMXHydro(OtherState%DOFs%PYE(I),:) + TmpVec2 &
                        - PtfmRIner*CoordSys%a1*DOT_PRODUCT( CoordSys%a1, PAngVelEX(OtherState%DOFs%PYE(I),0,:) )   &
                        - PtfmYIner*CoordSys%a2*DOT_PRODUCT( CoordSys%a2, PAngVelEX(OtherState%DOFs%PYE(I),0,:) )   &
                        - PtfmPIner*CoordSys%a3*DOT_PRODUCT( CoordSys%a3, PAngVelEX(OtherState%DOFs%PYE(I),0,:) )

ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the platform center of mass (point Y)

TmpVec1 = -PtfmMass*( Gravity*CoordSys%z2 + LinAccEYt  )                   ! The portion of FrcZAllt associated with the PtfmMass
TmpVec2 = CROSS_PRODUCT( rZY      ,   TmpVec1 )                            ! The portion of MomXAllt associated with the PtfmMass
TmpVec3 = CROSS_PRODUCT( rZT0     , FrcT0Trbt )                            ! The portion of MomXAllt associated with the FrcT0Trbt
TmpVec  = PtfmRIner*CoordSys%a1*DOT_PRODUCT( CoordSys%a1, AngVelEX  ) &    ! = ( Platform inertia dyadic ) dot ( angular velocity of platform in the inertia frame )
        + PtfmYIner*CoordSys%a2*DOT_PRODUCT( CoordSys%a2, AngVelEX  ) &
        + PtfmPIner*CoordSys%a3*DOT_PRODUCT( CoordSys%a3, AngVelEX  )
TmpVec4 = CROSS_PRODUCT( -AngVelEX,   TmpVec  )                            ! = ( -angular velocity of platform in the inertia frame ) cross ( TmpVec )

FrcZAllt = FrcT0Trbt + FZHydrot + TmpVec1

MomXAllt = MomX0Trbt + MXHydrot + TmpVec2 + TmpVec3 + TmpVec4



   ! Compute the moments from teeter springs and dampers, rotor-furl springs
   !   and dampers, tail-furl springs and dampers, and the generator and
   !   high-speed shaft brake torque:

CALL Teeter  ( TeetAng     , TeetAngVel   , TeetMom ) ! Compute moment from teeter     springs and dampers, TeetMom; NOTE: TeetMom will be zero for a 3-blader since TeetAng = TeetAngVel = 0
CALL RFurling( x%QT(DOF_RFrl), x%QDT(DOF_RFrl), RFrlMom ) ! Compute moment from rotor-furl springs and dampers, RFrlMom
CALL TFurling( x%QT(DOF_TFrl), x%QDT(DOF_TFrl), TFrlMom ) ! Compute moment from tail-furl  springs and dampers, TFrlMom
CALL DrvTrTrq( p,              x%QDT(DOF_GeAz), GBoxTrq ) ! Compute generator and HSS-brake torque on LSS-side, GBoxTrq


   ! Now that all of the partial loads have been found, lets fill in the
   !   portions of the mass matrix on and below the diagonal that may be
   !   calculated with the help of the partial loads.  Also, lets fill in the
   !   portions of the forcing vector that may be calculated with the help of
   !   the partial loads.  Also let's add in additional terms to the forcing
   !   function that can't be added with the help of the partial loads:
   ! NOTE: The vector subscript array, SrtPS(), used in the following loops
   !       must be sorted from smallest to largest DOF index in order for the
   !       loops to work to enter values only on and below the diagonal of
   !       AugMat():

IF ( p%DOF_Flag (DOF_Sg  ) )  THEN
   DO I = OtherState%DOFs%Diag(DOF_Sg  ),OtherState%DOFs%NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(OtherState%DOFs%SrtPS(I),DOF_Sg  ) = -1.*DOT_PRODUCT( PLinVelEZ(DOF_Sg  ,0,:), PFrcZAll (OtherState%DOFs%SrtPS(I),:) )    ! [C(q,t)]X + [C(q,t)]HydroX + [C(q,t)]T + [C(q,t)]HydroT + [C(q,t)]N + [C(q,t)]R + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_Sg  ,    p%NAug) =  DOT_PRODUCT( PLinVelEZ(DOF_Sg  ,0,:), FrcZAllt              )    ! {-f(qd,q,t)}X + {-f(qd,q,t)}HydroX + {-f(qd,q,t)}T + {-f(qd,q,t)}AeroT + {-f(qd,q,t)}HydroT + {-f(qd,q,t)}N + {-f(qd,q,t)}R + {-f(qd,q,t)}H + {-f(qd,q,t)}B + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}AeroA
ENDIF

IF ( p%DOF_Flag (DOF_Sw  ) )  THEN
   DO I = OtherState%DOFs%Diag(DOF_Sw  ),OtherState%DOFs%NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(OtherState%DOFs%SrtPS(I),DOF_Sw  ) = -1.*DOT_PRODUCT( PLinVelEZ(DOF_Sw  ,0,:), PFrcZAll (OtherState%DOFs%SrtPS(I),:) )    ! [C(q,t)]X + [C(q,t)]HydroX + [C(q,t)]T + [C(q,t)]HydroT + [C(q,t)]N + [C(q,t)]R + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_Sw  ,    p%NAug) =  DOT_PRODUCT( PLinVelEZ(DOF_Sw  ,0,:), FrcZAllt              )    ! {-f(qd,q,t)}X + {-f(qd,q,t)}HydroX + {-f(qd,q,t)}T + {-f(qd,q,t)}AeroT + {-f(qd,q,t)}HydroT + {-f(qd,q,t)}N + {-f(qd,q,t)}R + {-f(qd,q,t)}H + {-f(qd,q,t)}B + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}AeroA
ENDIF

IF ( p%DOF_Flag (DOF_Hv  ) )  THEN
   DO I = OtherState%DOFs%Diag(DOF_Hv  ),OtherState%DOFs%NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(OtherState%DOFs%SrtPS(I),DOF_Hv  ) = -1.*DOT_PRODUCT( PLinVelEZ(DOF_Hv  ,0,:), PFrcZAll (OtherState%DOFs%SrtPS(I),:) )    ! [C(q,t)]X + [C(q,t)]HydroX + [C(q,t)]T + [C(q,t)]HydroT + [C(q,t)]N + [C(q,t)]R + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_Hv  ,    p%NAug) =  DOT_PRODUCT( PLinVelEZ(DOF_Hv  ,0,:), FrcZAllt              )    ! {-f(qd,q,t)}X + {-f(qd,q,t)}GravX + {-f(qd,q,t)}HydroX + {-f(qd,q,t)}T + {-f(qd,q,t)}GravT + {-f(qd,q,t)}AeroT + {-f(qd,q,t)}HydroT + {-f(qd,q,t)}N + {-f(qd,q,t)}GravN + {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
ENDIF

IF ( p%DOF_Flag (DOF_R   ) )  THEN
   DO I = OtherState%DOFs%Diag(DOF_R   ),OtherState%DOFs%NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal

      AugMat(OtherState%DOFs%SrtPS(I),DOF_R   ) = -1.*DOT_PRODUCT( PAngVelEX(DOF_R   ,0,:), PMomXAll (OtherState%DOFs%SrtPS(I),:) )    ! [C(q,t)]X + [C(q,t)]HydroX + [C(q,t)]T + [C(q,t)]HydroT + [C(q,t)]N + [C(q,t)]R + [C(q,t)]G + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_R   ,    p%NAug) =  DOT_PRODUCT( PAngVelEX(DOF_R   ,0,:), MomXAllt              )    ! {-f(qd,q,t)}X + {-f(qd,q,t)}GravX + {-f(qd,q,t)}HydroX + {-f(qd,q,t)}T + {-f(qd,q,t)}GravT + {-f(qd,q,t)}AeroT + {-f(qd,q,t)}HydroT + {-f(qd,q,t)}N + {-f(qd,q,t)}GravN + {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}G + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
ENDIF

IF ( p%DOF_Flag (DOF_P   ) )  THEN
   DO I = OtherState%DOFs%Diag(DOF_P   ),OtherState%DOFs%NActvDOF    ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(OtherState%DOFs%SrtPS(I),DOF_P   ) = -1.*DOT_PRODUCT( PAngVelEX(DOF_P  ,0,:), PMomXAll (OtherState%DOFs%SrtPS(I),:) ) ! [C(q,t)]X + [C(q,t)]HydroX + [C(q,t)]T + [C(q,t)]HydroT + [C(q,t)]N + [C(q,t)]R + [C(q,t)]G + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
   ENDDO                                                             ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_P                   ,p%NAug  ) =     DOT_PRODUCT( PAngVelEX(DOF_P  ,0,:), MomXAllt              )                 ! {-f(qd,q,t)}X + {-f(qd,q,t)}GravX + {-f(qd,q,t)}HydroX + {-f(qd,q,t)}T + {-f(qd,q,t)}GravT + {-f(qd,q,t)}AeroT + {-f(qd,q,t)}HydroT + {-f(qd,q,t)}N + {-f(qd,q,t)}GravN + {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}G + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
END IF

IF ( p%DOF_Flag (DOF_Y   ) )  THEN
   DO I = OtherState%DOFs%Diag(DOF_Y   ),OtherState%DOFs%NActvDOF    ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(OtherState%DOFs%SrtPS(I),DOF_Y   ) = -1.*DOT_PRODUCT( PAngVelEX(DOF_Y   ,0,:), PMomXAll (OtherState%DOFs%SrtPS(I),:) )    ! [C(q,t)]X + [C(q,t)]HydroX + [C(q,t)]T + [C(q,t)]HydroT + [C(q,t)]N + [C(q,t)]R + [C(q,t)]G + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
   ENDDO                                                             ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_Y   ,    p%NAug) =     DOT_PRODUCT( PAngVelEX(DOF_Y   ,0,:), MomXAllt              )    ! {-f(qd,q,t)}X + {-f(qd,q,t)}GravX + {-f(qd,q,t)}HydroX + {-f(qd,q,t)}T + {-f(qd,q,t)}GravT + {-f(qd,q,t)}AeroT + {-f(qd,q,t)}HydroT + {-f(qd,q,t)}N + {-f(qd,q,t)}GravN + {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}G + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
ENDIF

IF ( p%DOF_Flag (DOF_TFA1) )  THEN
   DO I = OtherState%DOFs%Diag(DOF_TFA1),OtherState%DOFs%NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(OtherState%DOFs%SrtPS(I),DOF_TFA1) = AugMat(OtherState%DOFs%SrtPS(I),DOF_TFA1)                                  &
                                -  DOT_PRODUCT( PLinVelEO(DOF_TFA1,0,:), PFrcONcRt(OtherState%DOFs%SrtPS(I),:) ) &  ! [C(q,t)]N + [C(q,t)]R + [C(q,t)]G + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
                                -  DOT_PRODUCT( PAngVelEB(DOF_TFA1,0,:), PMomBNcRt(OtherState%DOFs%SrtPS(I),:) )
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_TFA1,    p%NAug) = AugMat(DOF_TFA1,    p%NAug)                                  &
                                +  DOT_PRODUCT( PLinVelEO(DOF_TFA1,0,:), FrcONcRtt             ) &  ! {-f(qd,q,t)}N + {-f(qd,q,t)}GravN + {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}G + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
                                +  DOT_PRODUCT( PAngVelEB(DOF_TFA1,0,:), MomBNcRtt             )
ENDIF

IF ( p%DOF_Flag (DOF_TSS1) )  THEN
   DO I = OtherState%DOFs%Diag(DOF_TSS1),OtherState%DOFs%NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(OtherState%DOFs%SrtPS(I),DOF_TSS1) = AugMat(OtherState%DOFs%SrtPS(I),DOF_TSS1)                                  &
                                -  DOT_PRODUCT( PLinVelEO(DOF_TSS1,0,:), PFrcONcRt(OtherState%DOFs%SrtPS(I),:) ) &  ! [C(q,t)]N + [C(q,t)]R + [C(q,t)]G + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
                                -  DOT_PRODUCT( PAngVelEB(DOF_TSS1,0,:), PMomBNcRt(OtherState%DOFs%SrtPS(I),:) )
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_TSS1,    p%NAug) = AugMat(DOF_TSS1,    p%NAug)                                  &
                                +  DOT_PRODUCT( PLinVelEO(DOF_TSS1,0,:), FrcONcRtt             ) &  ! {-f(qd,q,t)}N + {-f(qd,q,t)}GravN + {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}G + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
                                +  DOT_PRODUCT( PAngVelEB(DOF_TSS1,0,:), MomBNcRtt             )
ENDIF

IF ( p%DOF_Flag (DOF_TFA2) )  THEN
   DO I = OtherState%DOFs%Diag(DOF_TFA2),OtherState%DOFs%NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(OtherState%DOFs%SrtPS(I),DOF_TFA2) = AugMat(OtherState%DOFs%SrtPS(I),DOF_TFA2)                                  &
                                -  DOT_PRODUCT( PLinVelEO(DOF_TFA2,0,:), PFrcONcRt(OtherState%DOFs%SrtPS(I),:) ) &  ! [C(q,t)]N + [C(q,t)]R + [C(q,t)]G + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
                                -  DOT_PRODUCT( PAngVelEB(DOF_TFA2,0,:), PMomBNcRt(OtherState%DOFs%SrtPS(I),:) )
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_TFA2,    p%NAug) = AugMat(DOF_TFA2,    p%NAug)                                  &
                                +  DOT_PRODUCT( PLinVelEO(DOF_TFA2,0,:), FrcONcRtt             ) &  ! {-f(qd,q,t)}N + {-f(qd,q,t)}GravN + {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}G + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
                                +  DOT_PRODUCT( PAngVelEB(DOF_TFA2,0,:), MomBNcRtt             )
ENDIF

IF ( p%DOF_Flag (DOF_TSS2) )  THEN
   DO I = OtherState%DOFs%Diag(DOF_TSS2),OtherState%DOFs%NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(OtherState%DOFs%SrtPS(I),DOF_TSS2) = AugMat(OtherState%DOFs%SrtPS(I),DOF_TSS2)                                  &
                                -  DOT_PRODUCT( PLinVelEO(DOF_TSS2,0,:), PFrcONcRt(OtherState%DOFs%SrtPS(I),:) ) &  ! [C(q,t)]N + [C(q,t)]R + [C(q,t)]G + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
                                -  DOT_PRODUCT( PAngVelEB(DOF_TSS2,0,:), PMomBNcRt(OtherState%DOFs%SrtPS(I),:) )
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_TSS2,    p%NAug) = AugMat(DOF_TSS2,    p%NAug)                                  &
                                +  DOT_PRODUCT( PLinVelEO(DOF_TSS2,0,:), FrcONcRtt             ) &  ! {-f(qd,q,t)}N + {-f(qd,q,t)}GravN + {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}G + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
                                +  DOT_PRODUCT( PAngVelEB(DOF_TSS2,0,:), MomBNcRtt             )
ENDIF

IF ( p%DOF_Flag (DOF_Yaw ) )  THEN
   DO I = OtherState%DOFs%Diag(DOF_Yaw ),OtherState%DOFs%NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(OtherState%DOFs%SrtPS(I),DOF_Yaw ) = -DOT_PRODUCT( PAngVelEN(DOF_Yaw ,0,:), PMomBNcRt(OtherState%DOFs%SrtPS(I),:) )    ! [C(q,t)]N + [C(q,t)]R + [C(q,t)]G + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_Yaw ,    p%NAug) =  DOT_PRODUCT( PAngVelEN(DOF_Yaw ,0,:), MomBNcRtt             ) &  ! {-f(qd,q,t)}N + {-f(qd,q,t)}GravN + {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}G + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
                                -  YawSpr *( x%QT (DOF_Yaw) - YawNeut     )                    &  ! + {-f(qd,q,t)}SpringYaw
                                -  YawDamp*( x%QDT(DOF_Yaw) - YawRateNeut )                       ! + {-f(qd,q,t)}DampYaw; NOTE: The neutral yaw rate, YawRateNeut, defaults to zero.  It is only used for yaw control.
ENDIF

IF ( p%DOF_Flag (DOF_RFrl) )  THEN
   DO I = OtherState%DOFs%Diag(DOF_RFrl),OtherState%DOFs%NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(OtherState%DOFs%SrtPS(I),DOF_RFrl) = -DOT_PRODUCT( PAngVelER(DOF_RFrl,0,:), PMomNGnRt(OtherState%DOFs%SrtPS(I),:) )    ! [C(q,t)]R + [C(q,t)]G + [C(q,t)]H + [C(q,t)]B
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_RFrl,    p%NAug) =  DOT_PRODUCT( PAngVelER(DOF_RFrl,0,:), MomNGnRtt             ) &  ! {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}G + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB
                                +  RFrlMom                                                      ! + {-f(qd,q,t)}SpringRF + {-f(qd,q,t)}DampRF
ENDIF

TmpVec = GenIner*CoordSys%c1*DOT_PRODUCT( CoordSys%c1, PAngVelEG(DOF_GeAz,0,:) )  ! = ( generator inertia dyadic ) Dot ( partial angular velocity of G in E for DOF_GeAz )

IF ( p%DOF_Flag (DOF_GeAz) )  THEN
   DO I = OtherState%DOFs%Diag(DOF_GeAz),OtherState%DOFs%NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(OtherState%DOFs%SrtPS(I),DOF_GeAz) = -DOT_PRODUCT( PAngVelEL(DOF_GeAz,0,:), PMomLPRot(OtherState%DOFs%SrtPS(I),:) )    ! [C(q,t)]H + [C(q,t)]B
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_GeAz,    p%NAug) =  DOT_PRODUCT( PAngVelEL(DOF_GeAz,0,:), MomLPRott             ) &  ! {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB
                                -  GBoxTrq                                                      ! + {-f(qd,q,t)}Gen + {-f(qd,q,t)}Brake


   ! The previous loop (DO I = OtherState%DOFs%Diag(DOF_GeAz),OtherState%DOFs%NActvDOF) misses the
   !   generator inertia-contribution to the mass matrix and forcing function.
   !   Thus, add these in as well:


      AugMat(DOF_GeAz,DOF_GeAz) = AugMat(DOF_GeAz,DOF_GeAz)                                  &
                                +  DOT_PRODUCT( PAngVelEG(DOF_GeAz,0,:), TmpVec                )    ! [C(q,t)]G
      AugMat(DOF_GeAz,    p%NAug) = AugMat(DOF_GeAz,    p%NAug)                                  &
                                -  DOT_PRODUCT( AngAccEGt              , TmpVec                )    ! {-f(qd,q,t)}G


ENDIF

IF ( p%DOF_Flag (DOF_DrTr) )  THEN
   DO I = OtherState%DOFs%Diag(DOF_DrTr),OtherState%DOFs%NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(OtherState%DOFs%SrtPS(I),DOF_DrTr) = -DOT_PRODUCT( PAngVelEL(DOF_DrTr,0,:), PMomLPRot(OtherState%DOFs%SrtPS(I),:) )    ! [C(q,t)]H + [C(q,t)]B
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_DrTr,    p%NAug) =  DOT_PRODUCT( PAngVelEL(DOF_DrTr,0,:), MomLPRott             ) &  ! {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB
                                -  DTTorSpr*x%QT (DOF_DrTr)                                    &  ! + {-f(qd,q,t)}ElasticDrive
                                -  DTTorDmp*x%QDT(DOF_DrTr)                                       ! + {-f(qd,q,t)}DampDrive
ENDIF

IF ( p%DOF_Flag (DOF_TFrl) )  THEN
   ! The tail-furl DOF does not affect any DOF index larger than DOF_TFrl.  Therefore, there is no need to perform the loop: DO I = Diag(DOF_TFrl),NActvDOF
      AugMat(DOF_TFrl,DOF_TFrl) = -DOT_PRODUCT( PAngVelEA(DOF_TFrl,0,:), PMomNTail(DOF_TFrl,:) )    ! [C(q,t)]A
      AugMat(DOF_TFrl,    p%NAug) =  DOT_PRODUCT( PAngVelEA(DOF_TFrl,0,:), MomNTailt             ) &  ! {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
                                +  TFrlMom                                                      ! + {-f(qd,q,t)}SpringTF + {-f(qd,q,t)}DampTF
ENDIF

IF ( ( p%NumBl == 2 ) .AND. ( p%DOF_Flag(DOF_Teet) ) )  THEN
   ! The teeter DOF does not affect any DOF index larger than DOF_Teet.  Therefore, there is no need to perform the loop: DO I = Diag(DOF_Teet),NActvDOF
      AugMat(DOF_Teet,DOF_Teet) = -DOT_PRODUCT( PAngVelEH(DOF_Teet,0,:), PMomLPRot(DOF_Teet,:) )    ! [C(q,t)]H + [C(q,t)]B
      AugMat(DOF_Teet,    p%NAug) =  DOT_PRODUCT( PAngVelEH(DOF_Teet,0,:), MomLPRott             ) &  ! {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB
                                +  TeetMom                                                      ! + {-f(qd,q,t)}SpringTeet + {-f(qd,q,t)}DampTeet
ENDIF



   ! So far, we have only filled in the portions of the mass matrix on and
   !   below the diagonal.  Since the mass matrix is symmetric up to this
   !   point, let's fill in the portion above the diagonal by mirroring the
   !   values from below:
   ! NOTE: The vector subscript array, SrtPS(), used in the following loops
   !       must be sorted from smallest to largest DOF index in order for the
   !       loops to work to enter values only on and below the diagonal of
   !       AugMat():

DO L = 2,OtherState%DOFs%NActvDOF ! Loop through all active (enabled) DOFs above the diagonal (columns)
   DO I = 1,L-1   ! Loop through all active (enabled) DOFs above the diagonal (rows)
      AugMat(OtherState%DOFs%SrtPS(I),OtherState%DOFs%SrtPS(L)) = AugMat(OtherState%DOFs%SrtPS(L),OtherState%DOFs%SrtPS(I))
   ENDDO          ! I - All active (enabled) DOFs above the diagonal (rows)
ENDDO             ! L - All active (enabled) DOFs above the diagonal (columns)



   ! Let's add the gearbox friction terms to the mass matrix and forcing
   !   function.  These only effect the equation for the generator azimuth
   !   DOF.
   ! NOTE: the MASS MATRIX WILL NO LONGER BE SYMMETRIC after adding these
   !       terms, unless the gearbox efficiency, GBoxEff, was set to 100%:

GBoxEffFac  = GBoxEff**SgnPrvLSTQ      ! = GBoxEff if SgnPrvLSTQ = 1 OR 1/GBoxEff if SgnPrvLSTQ = -1
GBoxEffFac2 = ( 1.0/GBoxEffFac - 1.0 ) ! = ( 1 / GBoxEff^SgnPrvLSTQ - 1 )

DO I = 1,OtherState%DOFs%NActvDOF ! Loop through all active (enabled) DOFs

   AugMat(DOF_GeAz,OtherState%DOFs%SrtPS(I)) = AugMat(DOF_GeAz,OtherState%DOFs%SrtPS(I)) &                                            ! NOTE: TmpVec is still = ( generator inertia dyadic ) Dot ( partial angular velocity of G in E for DOF_GeAz ) in the following equation
                             + GBoxEffFac2*  DOT_PRODUCT( PAngVelEG(OtherState%DOFs%SrtPS(I),0,:), TmpVec )               ! [C(q,t)]GBFric

ENDDO             ! I - All active (enabled) DOFs

AugMat(   DOF_GeAz,    p%NAug) = AugMat(DOF_GeAz,    p%NAug) &                                            ! NOTE: TmpVec is still = ( generator inertia dyadic ) Dot ( partial angular velocity of G in E for DOF_GeAz ) in the following equation
                             - GBoxEffFac2*( DOT_PRODUCT( AngAccEGt              , TmpVec ) + GBoxTrq )   ! {-f(qd,q,t)}GBFric



   ! Store the row of coefficients associated with the generator azimuth DOF
   !   for future use:

OgnlGeAzRo = AugMat(DOF_GeAz,:)



   ! Invert the matrix to solve for the accelerations.  The accelerations are
   !   returned by Gauss() in the first NActvDOF elements of the solution
   !   vector, SolnVec().  These are transfered to the proper index locations
   !   of the acceleration vector QD2T() using the vector subscript array
   !   SrtPS(), after Gauss() has been called:
   ! NOTE: QD2T( SrtPS(1:NActvDOF) ) cannot be sent directly because arrays
   !   sections with vector subscripts must not be used in INTENT(OUT)
   !   arguments.

!do i=1,p%NDOF
!   write(*,'("GaussInp: ",i2,1x,700(G15.7,1X))') i, AugMat( i, : )
!end do

CALL Gauss( AugMat( OtherState%DOFs%SrtPS    (1: OtherState%DOFs%NActvDOF   ),     &
                    OtherState%DOFs%SrtPSNAUG(1:(OtherState%DOFs%NActvDOF+1)) ),   &
                                                 OtherState%DOFs%NActvDOF, SolnVec )


QD2T = 0.0
DO I = 1,OtherState%DOFs%NActvDOF ! Loop through all active (enabled) DOFs
   QD2T(OtherState%DOFs%SrtPS(I)) = SolnVec(I)
ENDDO             ! I - All active (enabled) DOFs



   ! Lets calculate the sign (+/-1) of the low-speed shaft torque for
   !   this time step and store it in SgnPrvLSTQ.  This will be used
   !   during the next call to RtHS.  MomLPRot is the moment on the
   !   low-speed shaft at the teeter pin caused by the rotor.

MomLPRot = MomLPRott ! Initialize MomLPRot using MomLPRott
DO I = 1,OtherState%DOFs%NActvDOF ! Loop through all active (enabled) DOFs

   MomLPRot = MomLPRot + PMomLPRot(OtherState%DOFs%SrtPS(I),:)*QD2T(OtherState%DOFs%SrtPS(I))  ! Add the moments associated with the accelerations of the DOFs

ENDDO             ! I - All active (enabled) DOFs

   ! MomLProt has now been found.  Now dot this with e1 to get the
   !   low-speed shaft torque and take the SIGN of the result:

SgnPrvLSTQ = NINT( SIGN( 1.0, DOT_PRODUCT( MomLPRot, CoordSys%e1 ) ) )



   ! If we are linearizing a model and DOFs were enabled or disabled within the
   !   user-defined routines called from RtHS(), abort:

IF ( AnalMode == 2 )  THEN ! .TRUE. when we are in the process of linearizing the FAST model

   DO I = 1,p%NDOF     ! Loop through all DOFs
      IF ( p%DOF_Flag(I) .NEQV. DOF_FlagInit(I) )  &
         CALL ProgAbort ( ' FAST can''t linearize a model when DOFs are being switched on-or-off from within user-defined'// &
                      ' routines.  Make sure no user-defined routines change the value of DOF_Flag().'                     )
   ENDDO             ! I - All DOFs

ENDIF



RETURN



CONTAINS



!=======================================================================
   FUNCTION TBFract( ZTTmp, BrakStrt, BrakEnd )


      ! A math S-function for the fraction of tip brake drag b/n normal and
      !   fully deployed operation.




   IMPLICIT                        NONE


      ! Passed Variables:

   REAL(ReKi), INTENT(IN )      :: BrakEnd                                         ! Time at which brakes are fully deployed
   REAL(ReKi), INTENT(IN )      :: BrakStrt                                        ! Time at which brakes are first deployed
   REAL(ReKi)                   :: TBFract                                         ! This function.
   REAL(ReKi), INTENT(IN )      :: ZTTmp                                           ! Current time


      ! Local Variables.

   REAL(ReKi)                   :: TmpVar                                          ! A temporary variable



   IF ( ZTTmp <= BrakStrt )  THEN

      TBFract = 0.0

   ELSEIF ( ZTTmp < BrakEnd )  THEN

      TmpVar  = ( ( ZTTmp - BrakStrt )/( BrakStrt - BrakEnd ) )**2
      TBFract = TmpVar*( 2.0 - TmpVar )

   ELSE

      TBFract = 1.0

   ENDIF



   RETURN
   END FUNCTION TBFract
!=======================================================================



END SUBROUTINE RtHS
!=======================================================================
SUBROUTINE SetCoordSy( CoordSys, p, x )


   ! This routine is used to define the internal coordinate systems for
   !   this particular time step.


USE                             Blades
USE                             RtHndSid
USE                             Tower
USE                             SimCont, ONLY: ZTime
USE                             TurbCont


IMPLICIT                        NONE

   ! Subroutine arguments (passed variables)

TYPE(StrD_CoordSys),            INTENT(INOUT) :: CoordSys                      ! The coordinate systems to be set
TYPE(StrD_ParameterType),       INTENT(IN)    :: p                             ! The module's parameters
TYPE(StrD_ContinuousStateType), INTENT(IN)    :: x                             ! The module's continuous states


   ! Local variables

REAL(ReKi)                   :: CAzimuth                                        ! COS( rotor azimuth angle ).
REAL(ReKi)                   :: CgRotAng                                        ! COS( gRotAng ).
REAL(ReKi)                   :: CNacYaw                                         ! COS( nacelle yaw angle ).
REAL(ReKi)                   :: CosPitch                                        ! COS( the current pitch angle ).
REAL(ReKi)                   :: CPitPTwstA                                      ! COS( BlPitch(K) + AeroTwst(J) ) found using the sum of angles formulae of cosine.
REAL(ReKi)                   :: CPitPTwstS                                      ! COS( BlPitch(K) + ThetaS(K,J) ) found using the sum of angles formulae of cosine.
REAL(ReKi)                   :: CRotFurl                                        ! COS( rotor-furl angle ).
REAL(ReKi)                   :: CTailFurl                                       ! COS( tail-furl angle ).
REAL(ReKi)                   :: CTeetAng                                        ! COS( TeetAng ).
REAL(ReKi)                   :: g1Prime   (3)                                   ! = g1.
REAL(ReKi)                   :: g2Prime   (3)                                   ! completes the right-handed gPrime-vector triad
REAL(ReKi)                   :: g3Prime   (3)                                   ! = g3 rotated about g1 so that parallel to the pitching axis of blade K (i.e., the current blade in the blade loop).
REAL(ReKi)                   :: gRotAng                                         ! Angle of rotation about g1 to get from the g to the gPrime system.
REAL(ReKi)                   :: Lj1       (3)                                   ! vector / direction Lj1 at node J for blade K.
REAL(ReKi)                   :: Lj2       (3)                                   ! vector / direction Lj2 at node J for blade K.
REAL(ReKi)                   :: Lj3       (3)                                   ! vector / direction Lj3 at node J for blade K.
REAL(ReKi)                   :: SAzimuth                                        ! SIN( rotor azimuth angle ).
REAL(ReKi)                   :: SgRotAng                                        ! SIN( gRotAng ).
REAL(ReKi)                   :: SinPitch                                        ! SIN( the current pitch angle ).
REAL(ReKi)                   :: SNacYaw                                         ! SIN( nacelle yaw angle ).
REAL(ReKi)                   :: SPitPTwstA                                      ! SIN( BlPitch(K) + AeroTwst(J) ) found using the sum of angles formulae of sine.
REAL(ReKi)                   :: SPitPTwstS                                      ! SIN( BlPitch(K) + ThetaS(K,J) ) found using the sum of angles formulae of sine.
REAL(ReKi)                   :: SRotFurl                                        ! SIN( rotor-furl angle ).
REAL(ReKi)                   :: STailFurl                                       ! SIN( tail-furl angle ).
REAL(ReKi)                   :: STeetAng                                        ! SIN( TeetAng ).
REAL(ReKi)                   :: ThetaFA                                         ! Tower fore-aft tilt deflection angle.
REAL(ReKi)                   :: ThetaIP                                         ! Blade in-plane deflection angle at node J for blade K.
REAL(ReKi)                   :: ThetaLxb                                        ! Blade deflection angle about the Lxb (n1) -axis at node J for blade K.
REAL(ReKi)                   :: ThetaLyb                                        ! Blade deflection angle about the Lyb (n2) -axis at node J for blade K.
REAL(ReKi)                   :: ThetaOoP                                        ! Blade out-of-plane deflection angle at node J for blade K.
REAL(ReKi)                   :: ThetaSS                                         ! Tower side-to-side tilt deflection angle.
REAL(ReKi)                   :: TransMat  (3,3)                                 ! The resulting transformation matrix due to three orthogonal rotations, (-).

INTEGER(4)                   :: J                                               ! Loops through nodes / elements.
INTEGER(4)                   :: K                                               ! Loops through blades.



   ! Inertial frame coordinate system:

CoordSys%z1 = (/ 1.0, 0.0, 0.0 /)   ! Vector / direction z1 (=  xi from the IEC coord. system).
CoordSys%z2 = (/ 0.0, 1.0, 0.0 /)   ! Vector / direction z2 (=  zi from the IEC coord. system).
CoordSys%z3 = (/ 0.0, 0.0, 1.0 /)   ! Vector / direction z3 (= -yi from the IEC coord. system).


   ! Tower base / platform coordinate system:

CALL SmllRotTrans( 'platform displacement', x%QT(DOF_R), x%QT(DOF_Y), -x%QT(DOF_P), TransMat, TRIM(Num2LStr(ZTime))//' s' )  ! Get the transformation matrix, TransMat, from inertial frame to tower base / platform coordinate systems.

CoordSys%a1 = TransMat(1,1)*CoordSys%z1 + TransMat(1,2)*CoordSys%z2 + TransMat(1,3)*CoordSys%z3 ! Vector / direction a1 (=  xt from the IEC coord. system).
CoordSys%a2 = TransMat(2,1)*CoordSys%z1 + TransMat(2,2)*CoordSys%z2 + TransMat(2,3)*CoordSys%z3 ! Vector / direction a2 (=  zt from the IEC coord. system).
CoordSys%a3 = TransMat(3,1)*CoordSys%z1 + TransMat(3,2)*CoordSys%z2 + TransMat(3,3)*CoordSys%z3 ! Vector / direction a3 (= -yt from the IEC coord. system).


DO J = 1,p%TwrNodes ! Loop through the tower nodes / elements


   ! Tower element-fixed coordinate system:

   ThetaFA = -TwrFASF(1,J       ,1)*x%QT(DOF_TFA1) - TwrFASF(2,J       ,1)*x%QT(DOF_TFA2)
   ThetaSS =  TwrSSSF(1,J       ,1)*x%QT(DOF_TSS1) + TwrSSSF(2,J       ,1)*x%QT(DOF_TSS2)

   CALL SmllRotTrans( 'tower deflection', ThetaSS, 0.0, ThetaFA, TransMat, TRIM(Num2LStr(ZTime))//' s' )   ! Get the transformation matrix, TransMat, from tower-base to tower element-fixed coordinate systems.

   CoordSys%t1(J,:) = TransMat(1,1)*CoordSys%a1 + TransMat(1,2)*CoordSys%a2 + TransMat(1,3)*CoordSys%a3  ! Vector / direction t1 for tower node J (=  Lxt from the IEC coord. system).
   CoordSys%t2(J,:) = TransMat(2,1)*CoordSys%a1 + TransMat(2,2)*CoordSys%a2 + TransMat(2,3)*CoordSys%a3  ! Vector / direction t2 for tower node J (=  Lzt from the IEC coord. system).
   CoordSys%t3(J,:) = TransMat(3,1)*CoordSys%a1 + TransMat(3,2)*CoordSys%a2 + TransMat(3,3)*CoordSys%a3  ! Vector / direction t3 for tower node J (= -Lyt from the IEC coord. system).


ENDDO ! J - Tower nodes / elements


   ! Tower-top / base plate coordinate system:

ThetaFA    = -TwrFASF(1,TTopNode,1)*x%QT(DOF_TFA1) - TwrFASF(2,TTopNode,1)*x%QT(DOF_TFA2)
ThetaSS    =  TwrSSSF(1,TTopNode,1)*x%QT(DOF_TSS1) + TwrSSSF(2,TTopNode,1)*x%QT(DOF_TSS2)

CALL SmllRotTrans( 'tower deflection', ThetaSS, 0.0, ThetaFA, TransMat, TRIM(Num2LStr(ZTime))//' s' )   ! Get the transformation matrix, TransMat, from tower-base to tower-top/base-plate coordinate systems.

CoordSys%b1 = TransMat(1,1)*CoordSys%a1 + TransMat(1,2)*CoordSys%a2 + TransMat(1,3)*CoordSys%a3 ! Vector / direction b1 (=  xp from the IEC coord. system).
CoordSys%b2 = TransMat(2,1)*CoordSys%a1 + TransMat(2,2)*CoordSys%a2 + TransMat(2,3)*CoordSys%a3 ! Vector / direction b2 (=  zp from the IEC coord. system).
CoordSys%b3 = TransMat(3,1)*CoordSys%a1 + TransMat(3,2)*CoordSys%a2 + TransMat(3,3)*CoordSys%a3 ! Vector / direction b3 (= -yp from the IEC coord. system).


   ! Nacelle / yaw coordinate system:

CNacYaw  = COS( x%QT(DOF_Yaw ) )
SNacYaw  = SIN( x%QT(DOF_Yaw ) )

CoordSys%d1 = CNacYaw*CoordSys%b1 - SNacYaw*CoordSys%b3     ! Vector / direction d1 (=  xn from the IEC coord. system).
CoordSys%d2 = CoordSys%b2                                   ! Vector / direction d2 (=  zn from the IEC coord. system).
CoordSys%d3 = SNacYaw*CoordSys%b1 + CNacYaw*CoordSys%b3     ! Vector / direction d3 (= -yn from the IEC coord. system).


   ! Rotor-furl coordinate system:

CRotFurl = COS( x%QT(DOF_RFrl) )
SRotFurl = SIN( x%QT(DOF_RFrl) )

CoordSys%rf1 = ( (   1.0 - p%CRFrlSkw2*p%CRFrlTlt2 )*CRotFurl   + p%CRFrlSkw2*p%CRFrlTlt2          )*CoordSys%d1 &
             + ( p%CRFrlSkew*p%CSRFrlTlt*( 1.0 -     CRotFurl ) - p%SRFrlSkew*p%CRFrlTilt*SRotFurl )*CoordSys%d2 &
             + ( p%CSRFrlSkw*p%CRFrlTlt2*( CRotFurl - 1.0     ) -             p%SRFrlTilt*SRotFurl )*CoordSys%d3
CoordSys%rf2 = ( p%CRFrlSkew*p%CSRFrlTlt*( 1.0 -     CRotFurl ) + p%SRFrlSkew*p%CRFrlTilt*SRotFurl )*CoordSys%d1 &
             + (             p%CRFrlTlt2*            CRotFurl   +             p%SRFrlTlt2          )*CoordSys%d2 &
             + ( p%SRFrlSkew*p%CSRFrlTlt*( CRotFurl - 1.0     ) + p%CRFrlSkew*p%CRFrlTilt*SRotFurl )*CoordSys%d3
CoordSys%rf3 = ( p%CSRFrlSkw*p%CRFrlTlt2*( CRotFurl - 1.0     ) +             p%SRFrlTilt*SRotFurl )*CoordSys%d1 &
             + ( p%SRFrlSkew*p%CSRFrlTlt*( CRotFurl - 1.0     ) - p%CRFrlSkew*p%CRFrlTilt*SRotFurl )*CoordSys%d2 &
             + ( (   1.0 - p%SRFrlSkw2*p%CRFrlTlt2 )*CRotFurl   + p%SRFrlSkw2*p%CRFrlTlt2          )*CoordSys%d3
CoordSys%rfa = p%CRFrlSkew*p%CRFrlTilt*CoordSys%d1 + p%SRFrlTilt*CoordSys%d2 - p%SRFrlSkew*p%CRFrlTilt*CoordSys%d3


   ! Shaft coordinate system:

CoordSys%c1 =  p%CShftSkew*p%CShftTilt*CoordSys%rf1 + p%SShftTilt*CoordSys%rf2 - p%SShftSkew*p%CShftTilt*CoordSys%rf3  ! Vector / direction c1 (=  xs from the IEC coord. system).
CoordSys%c2 = -p%CShftSkew*p%SShftTilt*CoordSys%rf1 + p%CShftTilt*CoordSys%rf2 + p%SShftSkew*p%SShftTilt*CoordSys%rf3  ! Vector / direction c2 (=  zs from the IEC coord. system).
CoordSys%c3 =  p%SShftSkew*           CoordSys%rf1                            + p%CShftSkew*            CoordSys%rf3  ! Vector / direction c3 (= -ys from the IEC coord. system).


   ! Azimuth coordinate system:

CAzimuth = COS( x%QT(DOF_DrTr) + x%QT(DOF_GeAz) )
SAzimuth = SIN( x%QT(DOF_DrTr) + x%QT(DOF_GeAz) )

CoordSys%e1 =  CoordSys%c1                                  ! Vector / direction e1 (=  xa from the IEC coord. system).
CoordSys%e2 =  CAzimuth*CoordSys%c2 + SAzimuth*CoordSys%c3  ! Vector / direction e2 (=  ya from the IEC coord. system).
CoordSys%e3 = -SAzimuth*CoordSys%c2 + CAzimuth*CoordSys%c3  ! Vector / direction e3 (=  za from the IEC coord. system).


   ! Teeter coordinate system:

   ! Lets define TeetAng, which is the current teeter angle (= QT(DOF_Teet) for
   !   2-blader or 0 for 3-blader) and is used in place of QT(DOF_Teet)
   !   throughout SUBROUTINE RtHS().  Doing it this way, we can run the same
   !   equations of motion for both the 2 and 3-blader configurations even
   !   though a 3-blader does not have a teetering DOF.

IF ( p%NumBl == 2 )  THEN ! 2-blader
   TeetAng    = x%QT (DOF_Teet)
   TeetAngVel = x%QDT(DOF_Teet)
ELSE                    ! 3-blader
   TeetAng    = 0.0  ! Teeter is not an available DOF for a 3-blader
   TeetAngVel = 0.0  ! Teeter is not an available DOF for a 3-blader
ENDIF
CTeetAng = COS( TeetAng )
STeetAng = SIN( TeetAng )

CoordSys%f1 = CTeetAng*CoordSys%e1 - STeetAng*CoordSys%e3       ! Vector / direction f1.
CoordSys%f2 = CoordSys%e2                                       ! Vector / direction f2.
CoordSys%f3 = STeetAng*CoordSys%e1 + CTeetAng*CoordSys%e3       ! Vector / direction f3.


   ! Hub / delta-3 coordinate system:

CoordSys%g1 =  CoordSys%f1                                      ! Vector / direction g1 (=  xh from the IEC coord. system).
CoordSys%g2 =  p%CosDel3*CoordSys%f2 + p%SinDel3*CoordSys%f3    ! Vector / direction g2 (=  yh from the IEC coord. system).
CoordSys%g3 = -p%SinDel3*CoordSys%f2 + p%CosDel3*CoordSys%f3    ! Vector / direction g3 (=  zh from the IEC coord. system).


DO K = 1,p%NumBl ! Loop through all blades


   ! Hub (Prime) coordinate system rotated to match blade K.

    gRotAng = p%TwoPiNB*(K-1)
   CgRotAng = COS( gRotAng )
   SgRotAng = SIN( gRotAng )

   g1Prime =  CoordSys%g1
   g2Prime =  CgRotAng*CoordSys%g2 + SgRotAng*CoordSys%g3
   g3Prime = -SgRotAng*CoordSys%g2 + CgRotAng*CoordSys%g3


   ! Coned coordinate system:

   CoordSys%i1(K,:) = p%CosPreC(K)*g1Prime - p%SinPreC(K)*g3Prime  ! i1(K,:) = vector / direction i1 for blade K (=  xcK from the IEC coord. system).
   CoordSys%i2(K,:) = g2Prime                                      ! i2(K,:) = vector / direction i2 for blade K (=  ycK from the IEC coord. system).
   CoordSys%i3(K,:) = p%SinPreC(K)*g1Prime + p%CosPreC(K)*g3Prime  ! i3(K,:) = vector / direction i3 for blade K (=  zcK from the IEC coord. system).


   ! Blade / pitched coordinate system:

   CosPitch = COS( BlPitch(K) )
   SinPitch = SIN( BlPitch(K) )

   CoordSys%j1(K,:) = CosPitch*CoordSys%i1(K,:) - SinPitch*CoordSys%i2(K,:)      ! j1(K,:) = vector / direction j1 for blade K (=  xbK from the IEC coord. system).
   CoordSys%j2(K,:) = SinPitch*CoordSys%i1(K,:) + CosPitch*CoordSys%i2(K,:)      ! j2(K,:) = vector / direction j2 for blade K (=  ybK from the IEC coord. system).
   CoordSys%j3(K,:) = CoordSys%i3(K,:)                                           ! j3(K,:) = vector / direction j3 for blade K (=  zbK from the IEC coord. system).


!JASON: USE TipNode HERE INSTEAD OF p%BldNodes IF YOU ALLOCATE AND DEFINE n1, n2, n3, m1, m2, AND m3 TO USE TipNode.  THIS WILL REQUIRE THAT THE AERODYNAMIC AND STRUCTURAL TWISTS, AeroTwst() AND ThetaS(), BE KNOWN AT THE TIP!!!
   DO J = 1,p%BldNodes ! Loop through the blade nodes / elements


   ! Blade coordinate system aligned with local structural axes (not element fixed):

      Lj1 = CThetaS(K,J)*CoordSys%j1(K,:) - SThetaS(K,J)*CoordSys%j2(K,:)  ! vector / direction Lj1 at node J for blade K
      Lj2 = SThetaS(K,J)*CoordSys%j1(K,:) + CThetaS(K,J)*CoordSys%j2(K,:)  ! vector / direction Lj2 at node J for blade K
      Lj3 = CoordSys%j3(K,:)                                               ! vector / direction Lj3 at node J for blade K


   ! Blade element-fixed coordinate system aligned with local structural axes:

      ThetaOoP =   TwistedSF(K,1,1,J,1)*x%QT( DOF_BF(K,1) ) &
                 + TwistedSF(K,1,2,J,1)*x%QT( DOF_BF(K,2) ) &
                 + TwistedSF(K,1,3,J,1)*x%QT( DOF_BE(K,1) )
      ThetaIP  = - TwistedSF(K,2,1,J,1)*x%QT( DOF_BF(K,1) ) &
                 - TwistedSF(K,2,2,J,1)*x%QT( DOF_BF(K,2) ) &
                 - TwistedSF(K,2,3,J,1)*x%QT( DOF_BE(K,1) )

      ThetaLxb = CThetaS(K,J)*ThetaIP - SThetaS(K,J)*ThetaOoP
      ThetaLyb = SThetaS(K,J)*ThetaIP + CThetaS(K,J)*ThetaOoP

      CALL SmllRotTrans( 'blade deflection', ThetaLxb, ThetaLyb, 0.0, TransMat, TRIM(Num2LStr(ZTime))//' s' ) ! Get the transformation matrix, TransMat, from blade coordinate system aligned with local structural axes (not element fixed) to blade element-fixed coordinate system aligned with local structural axes.

      CoordSys%n1(K,J,:) = TransMat(1,1)*Lj1 + TransMat(1,2)*Lj2 + TransMat(1,3)*Lj3   ! Vector / direction n1 for node J of blade K (= LxbK from the IEC coord. system).
      CoordSys%n2(K,J,:) = TransMat(2,1)*Lj1 + TransMat(2,2)*Lj2 + TransMat(2,3)*Lj3   ! Vector / direction n2 for node J of blade K (= LybK from the IEC coord. system).
      CoordSys%n3(K,J,:) = TransMat(3,1)*Lj1 + TransMat(3,2)*Lj2 + TransMat(3,3)*Lj3   ! Vector / direction n3 for node J of blade K (= LzbK from the IEC coord. system).


   ! Blade element-fixed coordinate system used for calculating and returning
   !    aerodynamics loads:
   ! This coordinate system is rotated about positive n3 by the angle
   !    BlPitch(K) + ThetaS(K,J) and is coincident with the i-vector triad
   !    when the blade is undeflected.

      CPitPTwstS = CosPitch*CThetaS(K,J) - SinPitch*SThetaS(K,J)  ! = COS( BlPitch(K) + ThetaS(K,J) ) found using the sum of angles formulae of cosine.
      SPitPTwstS = CosPitch*SThetaS(K,J) + SinPitch*CThetaS(K,J)  ! = SIN( BlPitch(K) + ThetaS(K,J) ) found using the sum of angles formulae of   sine.

      CoordSys%m1(K,J,:)  =  CPitPTwstS*CoordSys%n1(K,J,:) + SPitPTwstS*CoordSys%n2(K,J,:)   ! m1(K,J,:) = vector / direction m1 for node J of blade K (used to calc. and return aerodynamic loads from AeroDyn).
      CoordSys%m2(K,J,:)  = -SPitPTwstS*CoordSys%n1(K,J,:) + CPitPTwstS*CoordSys%n2(K,J,:)   ! m2(K,J,:) = vector / direction m2 for node J of blade K (used to calc. and return aerodynamic loads from AeroDyn).
      CoordSys%m3(K,J,:)  =  CoordSys%n3(K,J,:)                                              ! m3(K,J,:) = vector / direction m3 for node J of blade K (used to calc. and return aerodynamic loads from AeroDyn).


   ! Calculate the trailing edge coordinate system used in noise calculations.
   ! This coordinate system is blade element-fixed and oriented with the local
   !   aerodynamic axes (te2 points toward trailing edge, te1 points toward
   !   suction surface):

      CPitPTwstA = CosPitch*CAeroTwst(J) - SinPitch*SAeroTwst(J)  ! = COS( BlPitch(K) + AeroTwst(J) ) found using the sum of angles formulae of cosine.
      SPitPTwstA = CosPitch*SAeroTwst(J) + SinPitch*CAeroTwst(J)  ! = SIN( BlPitch(K) + AeroTwst(J) ) found using the sum of angles formulae of   sine.

      CoordSys%te1(K,J,:) =  CPitPTwstA*CoordSys%m1(K,J,:) - SPitPTwstA*CoordSys%m2(K,J,:)   ! te1(K,J,:) = vector / direction te1 for node J of blade K (used to calc. noise and to calc. and return aerodynamic loads from AeroDyn).
      CoordSys%te2(K,J,:) =  SPitPTwstA*CoordSys%m1(K,J,:) + CPitPTwstA*CoordSys%m2(K,J,:)   ! te2(K,J,:) = vector / direction te2 for node J of blade K (used to calc. noise and to calc. and return aerodynamic loads from AeroDyn).
      CoordSys%te3(K,J,:) =  CoordSys%m3(K,J,:)                                              ! te3(K,J,:) = vector / direction te3 for node J of blade K (used to calc. noise and to calc. and return aerodynamic loads from AeroDyn).


   ENDDO ! J - Blade nodes / elements


ENDDO ! K - Blades


   ! Tail-furl coordinate system:

CTailFurl = COS( x%QT(DOF_TFrl) )
STailFurl = SIN( x%QT(DOF_TFrl) )

CoordSys%tf1 = ( ( 1.0 - p%CTFrlSkw2*p%CTFrlTlt2 )*CTailFurl  + p%CTFrlSkw2*p%CTFrlTlt2           )*CoordSys%d1 &
             + ( p%CTFrlSkew*p%CSTFrlTlt*(  1.0 - CTailFurl ) - p%STFrlSkew*p%CTFrlTilt*STailFurl )*CoordSys%d2 &
             + ( p%CSTFrlSkw*p%CTFrlTlt2*( CTailFurl - 1.0  ) -             p%STFrlTilt*STailFurl )*CoordSys%d3
CoordSys%tf2 = ( p%CTFrlSkew*p%CSTFrlTlt*(  1.0 - CTailFurl ) + p%STFrlSkew*p%CTFrlTilt*STailFurl )*CoordSys%d1 &
             + (             p%CTFrlTlt2*         CTailFurl +               p%STFrlTlt2           )*CoordSys%d2 &
             + ( p%STFrlSkew*p%CSTFrlTlt*( CTailFurl - 1.0  ) + p%CTFrlSkew*p%CTFrlTilt*STailFurl )*CoordSys%d3
CoordSys%tf3 = ( p%CSTFrlSkw*p%CTFrlTlt2*( CTailFurl - 1.0  ) +             p%STFrlTilt*STailFurl )*CoordSys%d1 &
             + ( p%STFrlSkew*p%CSTFrlTlt*( CTailFurl - 1.0  ) - p%CTFrlSkew*p%CTFrlTilt*STailFurl )*CoordSys%d2 &
             + ( ( 1.0 - p%STFrlSkw2*p%CTFrlTlt2 )*CTailFurl  + p%STFrlSkw2*p%CTFrlTlt2           )*CoordSys%d3
CoordSys%tfa = p%CTFrlSkew*p%CTFrlTilt*CoordSys%d1 + p%STFrlTilt*CoordSys%d2 - p%STFrlSkew*p%CTFrlTilt*CoordSys%d3


   ! Tail fin coordinate system:

CoordSys%p1 = (                           p%CTFinSkew*p%CTFinTilt             )*CoordSys%tf1 &   ! Vector / direction p1 (= tail fin  x).
            + (                                       p%STFinTilt             )*CoordSys%tf2 &
            + (                         - p%STFinSkew*p%CTFinTilt             )*CoordSys%tf3
CoordSys%p2 = ( p%STFinSkew*p%STFinBank - p%CTFinSkew*p%STFinTilt*p%CTFinBank )*CoordSys%tf1 &   ! Vector / direction p2 (= tail fin  z).
            + (                                       p%CTFinTilt*p%CTFinBank )*CoordSys%tf2 &
            + ( p%CTFinSkew*p%STFinBank + p%STFinSkew*p%STFinTilt*p%CTFinBank )*CoordSys%tf3
CoordSys%p3 = ( p%STFinSkew*p%CTFinBank + p%CTFinSkew*p%STFinTilt*p%STFinBank )*CoordSys%tf1 &   ! Vector / direction p3 (= tail fin -y).
            + (                         -             p%CTFinTilt*p%STFinBank )*CoordSys%tf2 &
            + ( p%CTFinSkew*p%CTFinBank - p%STFinSkew*p%STFinTilt*p%STFinBank )*CoordSys%tf3



RETURN
END SUBROUTINE SetCoordSy
!=======================================================================
SUBROUTINE SetEnabledDOFIndexArrays( p, DOF )


   ! This routine is used create arrays of DOF indices (pointers /
   !   (vector susbscript arrays) that contribute to the QD2T-related
   !   linear accelerations of various points within the system in the
   !   inertia frame, based on which DOFs are presently enabled.
   ! NOTE: The order in which the DOFs are tested within this routine,
   !       and hence the order in which the DOF indices appear in the
   !       vector subscript arrays, determines the order in which the
   !       states will appear in the linearized model created by FAST
   !       when AnalMode == 2.  This order is not necessarily sorted
   !       from smallest to largest DOF index.



IMPLICIT                        NONE


   ! passed variables
TYPE(StrD_ParameterType), INTENT(IN)      :: p                                  ! Parameters of the structural dynamics module
TYPE(ActiveDOFs),         INTENT(INOUT)   :: DOF                                ! Active DOFs (from other/optimization states)


   ! Local Variables:

INTEGER(4)                   :: I                                               ! Loops through all DOFs.
INTEGER(4)                   :: K                                               ! Loops through blades.



   ! Initialize total counts to zero.

DOF%NActvDOF = 0
DOF%NPCE     = 0
DOF%NPDE     = 0
DOF%NPIE     = 0
DOF%NPTTE    = 0
DOF%NPTE     = 0
DOF%NPSBE(:) = 0
DOF%NPSE (:) = 0
DOF%NPUE     = 0
DOF%NPYE     = 0


   ! Test each DOF and include the appropriate indices in the subscript arrays
   !  and total counts:

IF ( p%DOF_Flag(DOF_Sg  ) )  THEN  ! Platform surge.

   DOF%NActvDOF = DOF%NActvDOF + 1
   DOF%NPCE     = DOF%NPCE     + 1
   DOF%NPDE     = DOF%NPDE     + 1
   DOF%NPIE     = DOF%NPIE     + 1
   DOF%NPTE     = DOF%NPTE     + 1
   DOF%NPSE (:) = DOF%NPSE (:) + 1
   DOF%NPUE     = DOF%NPUE     + 1
   DOF%NPYE     = DOF%NPYE     + 1

   DOF%PS      (  DOF%NActvDOF) = DOF_Sg
   DOF%PCE     (  DOF%NPCE    ) = DOF_Sg
   DOF%PDE     (  DOF%NPDE    ) = DOF_Sg
   DOF%PIE     (  DOF%NPIE    ) = DOF_Sg
   DOF%PTE     (  DOF%NPTE    ) = DOF_Sg
   DOF%PSE     (:,DOF%NPSE (:)) = DOF_Sg
   DOF%PUE     (  DOF%NPUE    ) = DOF_Sg
   DOF%PYE     (  DOF%NPYE    ) = DOF_Sg

ENDIF


IF ( p%DOF_Flag(DOF_Sw  ) )  THEN  ! Platform sway.

   DOF%NActvDOF = DOF%NActvDOF + 1
   DOF%NPCE     = DOF%NPCE     + 1
   DOF%NPDE     = DOF%NPDE     + 1
   DOF%NPIE     = DOF%NPIE     + 1
   DOF%NPTE     = DOF%NPTE     + 1
   DOF%NPSE (:) = DOF%NPSE (:) + 1
   DOF%NPUE     = DOF%NPUE     + 1
   DOF%NPYE     = DOF%NPYE     + 1

    DOF%PS     (  DOF%NActvDOF) = DOF_Sw
    DOF%PCE    (  DOF%NPCE    ) = DOF_Sw
    DOF%PDE    (  DOF%NPDE    ) = DOF_Sw
    DOF%PIE    (  DOF%NPIE    ) = DOF_Sw
    DOF%PTE    (  DOF%NPTE    ) = DOF_Sw
    DOF%PSE    (:,DOF%NPSE (:)) = DOF_Sw
    DOF%PUE    (  DOF%NPUE    ) = DOF_Sw
    DOF%PYE    (  DOF%NPYE    ) = DOF_Sw

ENDIF


IF ( p%DOF_Flag(DOF_Hv  ) )  THEN  ! Platform heave.

   DOF%NActvDOF = DOF%NActvDOF + 1
   DOF%NPCE     = DOF%NPCE     + 1
   DOF%NPDE     = DOF%NPDE     + 1
   DOF%NPIE     = DOF%NPIE     + 1
   DOF%NPTE     = DOF%NPTE     + 1
   DOF%NPSE (:) = DOF%NPSE (:) + 1
   DOF%NPUE     = DOF%NPUE     + 1
   DOF%NPYE     = DOF%NPYE     + 1

   DOF%PS      (  DOF%NActvDOF) = DOF_Hv
   DOF%PCE     (  DOF%NPCE    ) = DOF_Hv
   DOF%PDE     (  DOF%NPDE    ) = DOF_Hv
   DOF%PIE     (  DOF%NPIE    ) = DOF_Hv
   DOF%PTE     (  DOF%NPTE    ) = DOF_Hv
   DOF%PSE     (:,DOF%NPSE (:)) = DOF_Hv
   DOF%PUE     (  DOF%NPUE    ) = DOF_Hv
   DOF%PYE     (  DOF%NPYE    ) = DOF_Hv

ENDIF


IF ( p%DOF_Flag(DOF_R   ) )  THEN  ! Platform roll.

   DOF%NActvDOF = DOF%NActvDOF + 1
   DOF%NPCE     = DOF%NPCE     + 1
   DOF%NPDE     = DOF%NPDE     + 1
   DOF%NPIE     = DOF%NPIE     + 1
   DOF%NPTE     = DOF%NPTE     + 1
   DOF%NPSE (:) = DOF%NPSE (:) + 1
   DOF%NPUE     = DOF%NPUE     + 1
   DOF%NPYE     = DOF%NPYE     + 1

   DOF%PS      (  DOF%NActvDOF) = DOF_R
   DOF%PCE     (  DOF%NPCE    ) = DOF_R
   DOF%PDE     (  DOF%NPDE    ) = DOF_R
   DOF%PIE     (  DOF%NPIE    ) = DOF_R
   DOF%PTE     (  DOF%NPTE    ) = DOF_R
   DOF%PSE     (:,DOF%NPSE (:)) = DOF_R
   DOF%PUE     (  DOF%NPUE    ) = DOF_R
   DOF%PYE     (  DOF%NPYE    ) = DOF_R

ENDIF


IF ( p%DOF_Flag(DOF_P   ) )  THEN  ! Platform pitch.

   DOF%NActvDOF = DOF%NActvDOF + 1
   DOF%NPCE     = DOF%NPCE     + 1
   DOF%NPDE     = DOF%NPDE     + 1
   DOF%NPIE     = DOF%NPIE     + 1
   DOF%NPTE     = DOF%NPTE     + 1
   DOF%NPSE (:) = DOF%NPSE (:) + 1
   DOF%NPUE     = DOF%NPUE     + 1
   DOF%NPYE     = DOF%NPYE     + 1

   DOF%PS      (  DOF%NActvDOF) = DOF_P
   DOF%PCE     (  DOF%NPCE    ) = DOF_P
   DOF%PDE     (  DOF%NPDE    ) = DOF_P
   DOF%PIE     (  DOF%NPIE    ) = DOF_P
   DOF%PTE     (  DOF%NPTE    ) = DOF_P
   DOF%PSE     (:,DOF%NPSE (:)) = DOF_P
   DOF%PUE     (  DOF%NPUE    ) = DOF_P
   DOF%PYE     (  DOF%NPYE    ) = DOF_P

ENDIF


IF ( p%DOF_Flag(DOF_Y   ) )  THEN  ! Platform yaw.

   DOF%NActvDOF = DOF%NActvDOF + 1
   DOF%NPCE     = DOF%NPCE     + 1
   DOF%NPDE     = DOF%NPDE     + 1
   DOF%NPIE     = DOF%NPIE     + 1
   DOF%NPTE     = DOF%NPTE     + 1
   DOF%NPSE (:) = DOF%NPSE (:) + 1
   DOF%NPUE     = DOF%NPUE     + 1
   DOF%NPYE     = DOF%NPYE     + 1

   DOF%PS      (  DOF%NActvDOF) = DOF_Y
   DOF%PCE     (  DOF%NPCE    ) = DOF_Y
   DOF%PDE     (  DOF%NPDE    ) = DOF_Y
   DOF%PIE     (  DOF%NPIE    ) = DOF_Y
   DOF%PTE     (  DOF%NPTE    ) = DOF_Y
   DOF%PSE     (:,DOF%NPSE (:)) = DOF_Y
   DOF%PUE     (  DOF%NPUE    ) = DOF_Y
   DOF%PYE     (  DOF%NPYE    ) = DOF_Y

ENDIF


IF ( p%DOF_Flag(DOF_TFA1) )  THEN  ! 1st tower fore-aft.

   DOF%NActvDOF = DOF%NActvDOF + 1
   DOF%NPCE     = DOF%NPCE     + 1
   DOF%NPDE     = DOF%NPDE     + 1
   DOF%NPIE     = DOF%NPIE     + 1
   DOF%NPTTE    = DOF%NPTTE    + 1
   DOF%NPTE     = DOF%NPTE     + 1
   DOF%NPSE (:) = DOF%NPSE (:) + 1
   DOF%NPUE     = DOF%NPUE     + 1

   DOF%PS      (  DOF%NActvDOF) = DOF_TFA1
   DOF%PCE     (  DOF%NPCE    ) = DOF_TFA1
   DOF%PDE     (  DOF%NPDE    ) = DOF_TFA1
   DOF%PIE     (  DOF%NPIE    ) = DOF_TFA1
   DOF%PTTE    (  DOF%NPTTE   ) = DOF_TFA1
   DOF%PTE     (  DOF%NPTE    ) = DOF_TFA1
   DOF%PSE     (:,DOF%NPSE (:)) = DOF_TFA1
   DOF%PUE     (  DOF%NPUE    ) = DOF_TFA1

ENDIF


IF ( p%DOF_Flag(DOF_TSS1) )  THEN  ! 1st tower side-to-side.

   DOF%NActvDOF = DOF%NActvDOF + 1
   DOF%NPCE     = DOF%NPCE     + 1
   DOF%NPDE     = DOF%NPDE     + 1
   DOF%NPIE     = DOF%NPIE     + 1
   DOF%NPTTE    = DOF%NPTTE    + 1
   DOF%NPTE     = DOF%NPTE     + 1
   DOF%NPSE (:) = DOF%NPSE (:) + 1
   DOF%NPUE     = DOF%NPUE     + 1

   DOF%PS      (  DOF%NActvDOF) = DOF_TSS1
   DOF%PCE     (  DOF%NPCE    ) = DOF_TSS1
   DOF%PDE     (  DOF%NPDE    ) = DOF_TSS1
   DOF%PIE     (  DOF%NPIE    ) = DOF_TSS1
   DOF%PTTE    (  DOF%NPTTE   ) = DOF_TSS1
   DOF%PTE     (  DOF%NPTE    ) = DOF_TSS1
   DOF%PSE     (:,DOF%NPSE (:)) = DOF_TSS1
   DOF%PUE     (  DOF%NPUE    ) = DOF_TSS1

ENDIF


IF ( p%DOF_Flag(DOF_TFA2) )  THEN  ! 2nd tower fore-aft.

   DOF%NActvDOF = DOF%NActvDOF + 1
   DOF%NPCE     = DOF%NPCE     + 1
   DOF%NPDE     = DOF%NPDE     + 1
   DOF%NPIE     = DOF%NPIE     + 1
   DOF%NPTTE    = DOF%NPTTE    + 1
   DOF%NPTE     = DOF%NPTE     + 1
   DOF%NPSE (:) = DOF%NPSE (:) + 1
   DOF%NPUE     = DOF%NPUE     + 1

   DOF%PS      (  DOF%NActvDOF) = DOF_TFA2
   DOF%PCE     (  DOF%NPCE    ) = DOF_TFA2
   DOF%PDE     (  DOF%NPDE    ) = DOF_TFA2
   DOF%PIE     (  DOF%NPIE    ) = DOF_TFA2
   DOF%PTTE    (  DOF%NPTTE   ) = DOF_TFA2
   DOF%PTE     (  DOF%NPTE    ) = DOF_TFA2
   DOF%PSE     (:,DOF%NPSE (:)) = DOF_TFA2
   DOF%PUE     (  DOF%NPUE    ) = DOF_TFA2

ENDIF


IF ( p%DOF_Flag(DOF_TSS2) )  THEN  ! 2nd tower side-to-side.

   DOF%NActvDOF = DOF%NActvDOF + 1
   DOF%NPCE     = DOF%NPCE     + 1
   DOF%NPDE     = DOF%NPDE     + 1
   DOF%NPIE     = DOF%NPIE     + 1
   DOF%NPTTE    = DOF%NPTTE    + 1
   DOF%NPTE     = DOF%NPTE     + 1
   DOF%NPSE (:) = DOF%NPSE (:) + 1
   DOF%NPUE     = DOF%NPUE     + 1

   DOF%PS      (  DOF%NActvDOF) = DOF_TSS2
   DOF%PCE     (  DOF%NPCE    ) = DOF_TSS2
   DOF%PDE     (  DOF%NPDE    ) = DOF_TSS2
   DOF%PIE     (  DOF%NPIE    ) = DOF_TSS2
   DOF%PTTE    (  DOF%NPTTE   ) = DOF_TSS2
   DOF%PTE     (  DOF%NPTE    ) = DOF_TSS2
   DOF%PSE     (:,DOF%NPSE (:)) = DOF_TSS2
   DOF%PUE     (  DOF%NPUE    ) = DOF_TSS2

ENDIF


IF ( p%DOF_Flag(DOF_Yaw ) )  THEN  ! Nacelle yaw.

   DOF%NActvDOF = DOF%NActvDOF + 1
   DOF%NPCE     = DOF%NPCE     + 1
   DOF%NPDE     = DOF%NPDE     + 1
   DOF%NPIE     = DOF%NPIE     + 1
   DOF%NPSE (:) = DOF%NPSE (:) + 1
   DOF%NPUE     = DOF%NPUE     + 1

   DOF%PS      (  DOF%NActvDOF) = DOF_Yaw
   DOF%PCE     (  DOF%NPCE    ) = DOF_Yaw
   DOF%PDE     (  DOF%NPDE    ) = DOF_Yaw
   DOF%PIE     (  DOF%NPIE    ) = DOF_Yaw
   DOF%PSE     (:,DOF%NPSE (:)) = DOF_Yaw
   DOF%PUE     (  DOF%NPUE    ) = DOF_Yaw

ENDIF


IF ( p%DOF_Flag(DOF_TFrl) )  THEN  ! Tail-furl.

   DOF%NActvDOF = DOF%NActvDOF + 1
   DOF%NPIE     = DOF%NPIE     + 1

   DOF%PS      (  DOF%NActvDOF) = DOF_TFrl
   DOF%PIE     (  DOF%NPIE    ) = DOF_TFrl

ENDIF


IF ( p%DOF_Flag(DOF_RFrl) )  THEN  ! Rotor-furl.

   DOF%NActvDOF = DOF%NActvDOF + 1
   DOF%NPCE     = DOF%NPCE     + 1
   DOF%NPDE     = DOF%NPDE     + 1
   DOF%NPSE (:) = DOF%NPSE (:) + 1

   DOF%PS     (  DOF%NActvDOF) = DOF_RFrl
   DOF%PCE    (  DOF%NPCE    ) = DOF_RFrl
   DOF%PDE    (  DOF%NPDE    ) = DOF_RFrl
   DOF%PSE    (:,DOF%NPSE (:)) = DOF_RFrl

ENDIF


IF ( p%DOF_Flag(DOF_GeAz) )  THEN  ! Generator azimuth.

   DOF%NActvDOF = DOF%NActvDOF + 1
   DOF%NPCE     = DOF%NPCE     + 1
   DOF%NPSE (:) = DOF%NPSE (:) + 1

   DOF%PS      (  DOF%NActvDOF) = DOF_GeAz
   DOF%PCE     (  DOF%NPCE    ) = DOF_GeAz
   DOF%PSE     (:,DOF%NPSE (:)) = DOF_GeAz

ENDIF


IF ( p%DOF_Flag(DOF_DrTr) )  THEN  ! Drivetrain torsion.

   DOF%NActvDOF = DOF%NActvDOF + 1
   DOF%NPCE     = DOF%NPCE     + 1
   DOF%NPSE (:) = DOF%NPSE (:) + 1

   DOF%PS      (  DOF%NActvDOF) = DOF_DrTr
   DOF%PCE     (  DOF%NPCE    ) = DOF_DrTr
   DOF%PSE     (:,DOF%NPSE (:)) = DOF_DrTr

ENDIF


IF ( p%NumBl == 2 )  THEN
   IF ( p%DOF_Flag(DOF_Teet   ) )  THEN  ! Rotor-teeter.

      DOF%NActvDOF = DOF%NActvDOF + 1
      DOF%NPCE     = DOF%NPCE     + 1
      DOF%NPSE (:) = DOF%NPSE (:) + 1

      DOF%PS      (  DOF%NActvDOF) = DOF_Teet
      DOF%PCE     (  DOF%NPCE    ) = DOF_Teet
      DOF%PSE     (:,DOF%NPSE (:)) = DOF_Teet

   ENDIF
ENDIF


DO K = 1,p%NumBl ! Loop through all blades
   IF ( p%DOF_Flag(DOF_BF(K,1)) )  THEN  ! 1st blade flap.

      DOF%NActvDOF = DOF%NActvDOF + 1
      DOF%NPSBE(K) = DOF%NPSBE(K) + 1
      DOF%NPSE (K) = DOF%NPSE (K) + 1

      DOF%PS      (  DOF%NActvDOF) = DOF_BF(K,1)
      DOF%PSBE    (K,DOF%NPSBE(K)) = DOF_BF(K,1)
      DOF%PSE     (K,DOF%NPSE (K)) = DOF_BF(K,1)

   ENDIF
ENDDO          ! K - Blades


DO K = 1,p%NumBl ! Loop through all blades
   IF ( p%DOF_Flag(DOF_BE(K,1)) )  THEN  ! 1st blade edge.

      DOF%NActvDOF = DOF%NActvDOF + 1
      DOF%NPSBE(K) = DOF%NPSBE(K) + 1
      DOF%NPSE (K) = DOF%NPSE (K) + 1

      DOF%PS      (  DOF%NActvDOF) = DOF_BE(K,1)
      DOF%PSBE    (K,DOF%NPSBE(K)) = DOF_BE(K,1)
      DOF%PSE     (K,DOF%NPSE (K)) = DOF_BE(K,1)

   ENDIF
ENDDO          ! K - Blades


DO K = 1,p%NumBl ! Loop through all blades
   IF ( p%DOF_Flag(DOF_BF(K,2)) )  THEN  ! 2nd blade flap.

      DOF%NActvDOF = DOF%NActvDOF + 1
      DOF%NPSBE(K) = DOF%NPSBE(K) + 1
      DOF%NPSE (K) = DOF%NPSE (K) + 1

      DOF%PS      (  DOF%NActvDOF) = DOF_BF(K,2)
      DOF%PSBE    (K,DOF%NPSBE(K)) = DOF_BF(K,2)
      DOF%PSE     (K,DOF%NPSE (K)) = DOF_BF(K,2)

   ENDIF
ENDDO          ! K - Blades



   ! Compute the sorted (from smallest to largest DOF index) version of PS(),
   !   SrtPS(), and SrtPSNAUG().  At the same time compute Diag(), which is an
   !   array containing the indices of SrtPS() associated with each enabled
   !   DOF; that is, SrtPS(Diag(I)) = I:
   ! NOTE: This calculation is recomputing NActvDOF as computed above.  This is
   !       of no concern however, since the resulting value will be the same.

DOF%NActvDOF = 0
DO I = 1,p%NDOF  ! Loop through all DOFs
   IF ( p%DOF_Flag(I) )  THEN   ! .TRUE. if the corresponding DOF is enabled

      DOF%NActvDOF = DOF%NActvDOF + 1

      DOF%SrtPS     (DOF%NActvDOF) = I
      DOF%SrtPSNAUG (DOF%NActvDOF) = I
      DOF%Diag      (I           ) = DOF%NActvDOF

   ENDIF
ENDDO          ! I - All DOFs

DOF%SrtPSNAUG ( DOF%NActvDOF + 1 ) = p%NAug



RETURN
END SUBROUTINE SetEnabledDOFIndexArrays
!=======================================================================
SUBROUTINE Solver( p, x, y, OtherState )


   ! Solver solves the equations of motion by marching in time using a
   !   predictor-corrector scheme.  Fourth order Runge-Kutta is used to
   !   get the first 4 points from the initial degrees of freedom and
   !   velocities.


USE                             RtHndSid
USE                             SimCont
USE                             TurbCont


IMPLICIT                        NONE


   ! Subroutine arguments (Passed variables):

TYPE(StrD_ParameterType),      INTENT(IN)       :: p                           ! The parameters of the structural dynamics module
TYPE(StrD_ContinuousStateType),INTENT(INOUT)    :: x                           ! The structural dynamics module's continuous states
TYPE(StrD_OtherStateType),     INTENT(INOUT)    :: OtherState                  ! The structural dynamics "other" states (including CoordSys coordinate systems)
TYPE(StrD_OutputType),         INTENT(INOUT)    :: y                           ! System outputs of the structural dynamics module


   ! Local variables:

REAL(ReKi), ALLOCATABLE      :: ZK1      (:)                                    ! Runga-Kutta intermediate function used to estimate Q  at next time step.
REAL(ReKi), ALLOCATABLE      :: ZK1D     (:)                                    ! Runga-Kutta intermediate function used to estimate QD at next time step.
REAL(ReKi), ALLOCATABLE      :: ZK2      (:)                                    ! Runga-Kutta intermediate function used to estimate Q  at next time step.
REAL(ReKi), ALLOCATABLE      :: ZK2D     (:)                                    ! Runga-Kutta intermediate function used to estimate QD at next time step.
REAL(ReKi), ALLOCATABLE      :: ZK3      (:)                                    ! Runga-Kutta intermediate function used to estimate Q  at next time step.
REAL(ReKi), ALLOCATABLE      :: ZK3D     (:)                                    ! Runga-Kutta intermediate function used to estimate QD at next time step.
REAL(ReKi), ALLOCATABLE      :: ZK4      (:)                                    ! Runga-Kutta intermediate function used to estimate Q  at next time step.
REAL(ReKi), ALLOCATABLE      :: ZK4D     (:)                                    ! Runga-Kutta intermediate function used to estimate QD at next time step.


INTEGER(4)                   :: I                                               ! Loops through all DOFs
INTEGER(4)                   :: Sttus                                           ! Status returned from an attempt to allocate an array.



IF ( Step < 3 )  THEN   ! Use Runge-Kutta integration at the the start of the simulation (first 3 steps).


   ! Allocate arrays that vary with the number of DOFs..


   Sttus = 0

   IF (.NOT. ALLOCATED(ZK1)) ALLOCATE ( ZK1(p%NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the ZK1 array.' )
   ENDIF

   IF (.NOT. ALLOCATED(ZK1D)) ALLOCATE ( ZK1D(p%NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the ZK1D array.' )
   ENDIF

   IF (.NOT. ALLOCATED(ZK2)) ALLOCATE ( ZK2(p%NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the ZK2 array.' )
   ENDIF

   IF (.NOT. ALLOCATED(ZK2D)) ALLOCATE ( ZK2D(p%NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the ZK2D array.' )
   ENDIF

   IF (.NOT. ALLOCATED(ZK3)) ALLOCATE ( ZK3(p%NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the ZK3 array.' )
   ENDIF

   IF (.NOT. ALLOCATED(ZK3D)) ALLOCATE ( ZK3D(p%NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the ZK3D array.' )
   ENDIF

   IF (.NOT. ALLOCATED(ZK4)) ALLOCATE ( ZK4(p%NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the ZK4 array.' )
   ENDIF

   IF (.NOT. ALLOCATED(ZK4D)) ALLOCATE ( ZK4D(p%NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the ZK4D array.' )
   ENDIF


   ! First call to dynamics routine:

   x%QT  = OtherState%Q (:,IC(1))
   x%QDT = OtherState%QD(:,IC(1))

   CALL RtHS( p, OtherState%CoordSys, x, OtherState )


   ! Compute intermediate functions to estimate next Q and QD.

   DO I = 1,p%NDOF  ! Loop through all DOFs
      ZK1 (I) = DT*OtherState%QD  (I,IC(1))
      ZK1D(I) = DT*QD2T(I)

      x%QT  (I) = OtherState%Q (I,IC(1)) + 0.5*ZK1 (I)
      x%QDT (I) = OtherState%QD(I,IC(1)) + 0.5*ZK1D(I)
   ENDDO          ! I - All DOFs


   CALL RtHS( p, OtherState%CoordSys, x, OtherState )


   ! Repeat above steps for each ZK, ZKD:

   DO I = 1,p%NDOF  ! Loop through all DOFs
      ZK2 (I) = DT*( OtherState%QD  (I,IC(1)) + 0.5*ZK1D(I) )
      ZK2D(I) = DT*  QD2T(I)

      x%QT  (I) = OtherState%Q (I,IC(1)) + 0.5*ZK2 (I)
      x%QDT (I) = OtherState%QD(I,IC(1)) + 0.5*ZK2D(I)
   ENDDO          ! I - All DOFs


   CALL RtHS( p, OtherState%CoordSys, x, OtherState )


   DO I = 1,p%NDOF  ! Loop through all DOFs
      ZK3 (I) = DT*( OtherState%QD  (I,IC(1)) + 0.5*ZK2D(I) )
      ZK3D(I) = DT*  QD2T(I)

      x%QT  (I) = OtherState%Q (I,IC(1)) + ZK3 (I)
      x%QDT (I) = OtherState%QD(I,IC(1)) + ZK3D(I)
   ENDDO          ! I - All DOFs


   CALL RtHS( p, OtherState%CoordSys, x, OtherState )


   ! Compute best estimate for Q, QD at next time step using
   !   the intermediate functions (Runge-Kutta).
   ! IC(NMX) locates the i + 1 value of Q, QD.

   DO I = 1,p%NDOF  ! Loop through all DOFs
      ZK4 (I) = DT*( OtherState%QD  (I,IC(1)) + ZK3D(I) )
      ZK4D(I) = DT*  QD2T(I)

      OtherState%Q (I,IC(NMX)) = OtherState%Q (I,IC(1)) + ( ZK1 (I) + 2.0*ZK2 (I) + 2.0*ZK3 (I) + ZK4 (I) ) / 6.0
      OtherState%QD(I,IC(NMX)) = OtherState%QD(I,IC(1)) + ( ZK1D(I) + 2.0*ZK2D(I) + 2.0*ZK3D(I) + ZK4D(I) ) / 6.0
   ENDDO          ! I - All DOFs


   IF (ALLOCATED(ZK1) ) DEALLOCATE ( ZK1  )
   IF (ALLOCATED(ZK1D)) DEALLOCATE ( ZK1D )
   IF (ALLOCATED(ZK2) ) DEALLOCATE ( ZK2  )
   IF (ALLOCATED(ZK2D)) DEALLOCATE ( ZK2D )
   IF (ALLOCATED(ZK3) ) DEALLOCATE ( ZK3  )
   IF (ALLOCATED(ZK3D)) DEALLOCATE ( ZK3D )
   IF (ALLOCATED(ZK4) ) DEALLOCATE ( ZK4  )
   IF (ALLOCATED(ZK4D)) DEALLOCATE ( ZK4D )


ELSE                    ! User Adams-Bashforth predictor and Adams-Moulton corrector integration scheme for all other time steps.


   ! Predictor (Adams-Bashforth)

   ! Compute predictor from current (IC(1)) and 3 previous values of
   !   Q, QD, and QD2().  IC(1) = i, IC(2) = i-1, IC(3) = i-2 etc...

   DO I = 1,p%NDOF  ! Loop through all DOFs
      OtherState%Q (I,IC(NMX)) = OtherState%Q (I,IC(1)) + DT24*( 55.0*OtherState%QD (I,IC(1)) - 59.0*OtherState%QD (I,IC(2)) &
                                                               + 37.0*OtherState%QD (I,IC(3)) -  9.0*OtherState%QD (I,IC(4)) )
      OtherState%QD(I,IC(NMX)) = OtherState%QD(I,IC(1)) + DT24*( 55.0*OtherState%QD2(I,IC(1)) - 59.0*OtherState%QD2(I,IC(2)) &
                                                               + 37.0*OtherState%QD2(I,IC(3)) -  9.0*OtherState%QD2(I,IC(4)) )
   ENDDO          ! I - All DOFs

   x%QT  = OtherState%Q (:,IC(NMX))
   x%QDT = OtherState%QD(:,IC(NMX))

   CALL RtHS( p, OtherState%CoordSys, x, OtherState )

   OtherState%QD2(:,IC(NMX)) = QD2T


   ! Corrector (Adams-Moulton)

   ! Compute corrector from predictor value of Q, QD (IC(1)) and 3
   !   previous values of Q, QD, and QD2().  IC(1) = i, IC(2) = i-1,
   !   IC(3) = i-2 etc...

   DO I = 1,p%NDOF  ! Loop through all DOFs
      OtherState%Q (I,IC(NMX)) = OtherState%Q (I,IC(1)) + DT24*( 9.0*OtherState%QD (I,IC(NMX)) + 19.0*OtherState%QD (I,IC(1)) &
                                                               - 5.0*OtherState%QD (I,IC(2  )) +      OtherState%QD (I,IC(3)) )
      OtherState%QD(I,IC(NMX)) = OtherState%QD(I,IC(1)) + DT24*( 9.0*OtherState%QD2(I,IC(NMX)) + 19.0*OtherState%QD2(I,IC(1)) &
                                                               - 5.0*OtherState%QD2(I,IC(2  )) +      OtherState%QD2(I,IC(3)) )
   ENDDO          ! I - All DOFs


    ! Make sure the HSS brake has not reversed the direction of the HSS:

   IF ( p%DOF_Flag(DOF_GeAz) .AND. ( ZTime > THSSBrDp ) )  CALL FixHSSBrTq ( 'Corrector', p, OtherState )


ENDIF


   ! Compute the final value of QD2T from the best estimates for Q and
   !   QD, last call to RtHS:

x%QT  = OtherState%Q (:,IC(NMX))
x%QDT = OtherState%QD(:,IC(NMX))

CALL RtHS( p, OtherState%CoordSys, x, OtherState )

OtherState%QD2(:,IC(NMX)) = QD2T


   ! Update IC() index so IC(1) is the location of current Q values.

IC(1) = IC(1) + 1
IF ( IC(1) > NMX )  IC(1) = IC(1) - NMX
DO I = 2,NMX
   IC(I) = IC(1) - I + 1
   IF ( IC(I) <= 0 )  IC(I) = IC(I) + NMX
ENDDO


   ! Make sure the HSS brake will not reverse the direction of the HSS
   !   for the next time step.  Do this by computing the predicted value
   !   of QD(DOF_GeAz,IC(NMX)) as will be done during the next time step.
   ! Only do this after the first few time steps since it doesn't work
   !   for the Runga-Kutta integration scheme.

IF ( p%DOF_Flag(DOF_GeAz) .AND. ( ZTime > THSSBrDp ) .AND. ( Step >= 3 ) )  THEN

   OtherState%QD(DOF_GeAz,IC(NMX)) = OtherState%QD(DOF_GeAz,IC(1)) + DT24*(   55.0*OtherState%QD2(DOF_GeAz,IC(1)) &
                                                                            - 59.0*OtherState%QD2(DOF_GeAz,IC(2)) &
                                                                            + 37.0*OtherState%QD2(DOF_GeAz,IC(3)) &
                                                                            -  9.0*OtherState%QD2(DOF_GeAz,IC(4))   )

   CALL FixHSSBrTq ( 'Predictor', p, OtherState )

ENDIF



RETURN
END SUBROUTINE Solver
!=======================================================================
SUBROUTINE Teeter( TeetDef, TeetRate, TeetMom )


   ! This routine computes the teeter moment due to teeter deflection
   !   and rate.


USE                             General
USE                             SimCont
USE                             TeeterVars


IMPLICIT                        NONE


   ! Passed Variables:

REAL(ReKi), INTENT(IN )      :: TeetDef                                         ! The teeter deflection, x%QT(DOF_Teet).
REAL(ReKi), INTENT(OUT)      :: TeetMom                                         ! The total moment supplied by the stop, spring, and damper.
REAL(ReKi), INTENT(IN )      :: TeetRate                                        ! The teeter rate, x%QDT(DOF_Teet).


   ! Local variables:

REAL(ReKi)                   :: AbsDef                                          ! Absolute value of the teeter deflection.
REAL(ReKi)                   :: SprgDef                                         ! Deflection past the spring.
REAL(ReKi)                   :: StopDef                                         ! Deflection past the stop.
REAL(ReKi)                   :: TeetDMom                                        ! The moment supplied by the damper.
REAL(ReKi)                   :: TeetFMom                                        ! The moment supplied by Coulomb-friction damping.
REAL(ReKi)                   :: TeetKMom                                        ! The moment supplied by the spring.
REAL(ReKi)                   :: TeetSMom                                        ! The moment supplied by the stop.



SELECT CASE ( TeetMod ) ! Which teeter model are we using?

CASE ( 0 )              ! None!


   TeetMom = 0.0


CASE ( 1 )              ! Standard (using inputs from the primary FAST input file).


   ! Compute the absulute value of the deflection.

   AbsDef  = ABS( TeetDef )


   ! Linear teeter spring.

   SprgDef = AbsDef - TeetSStP

   IF ( SprgDef > 0.0 )  THEN
      TeetKMom = -SIGN( SprgDef*TeetSSSp, TeetDef )
   ELSE
      TeetKMom = 0
   ENDIF


   ! Compute teeter-stop moment if hard stop has been contacted.

   StopDef = AbsDef - TeetHStP

   IF ( StopDef > 0.0 )  THEN
      TeetSMom = -TeetHSSp*SIGN( StopDef, TeetDef )
   ELSE
      TeetSMom = 0.0
   ENDIF


   ! Compute linear teeter-damper moment.

   TeetDMom = -TeetDmp*TeetRate


   ! Add coulomb friction to the teeter hinge.

   IF ( TeetRate == 0.0 )  THEN
      TeetFMom = 0.0
   ELSE
      TeetFMom = -SIGN( TeetCDmp, TeetRate )
   ENDIF


   ! Total up all the moments.

   TeetMom = TeetSMom + TeetDMom + TeetKMom + TeetFMom


CASE ( 2 )              ! User-defined teeter spring/damper model.


   CALL UserTeet ( TeetDef, TeetRate, ZTime, DirRoot, TeetMom )


ENDSELECT



RETURN
END SUBROUTINE Teeter
!=======================================================================
SUBROUTINE TFurling( TFrlDef, TFrlRate, TFrlMom )


   ! This routine computes the tail-furl moment due to tail-furl deflection
   !   and rate.


USE                             General
USE                             SimCont
USE                             TailFurling


IMPLICIT                        NONE


   ! Passed Variables:

REAL(ReKi), INTENT(IN )      :: TFrlDef                                         ! The tail-furl deflection, QT(DOF_TFrl).
REAL(ReKi), INTENT(OUT)      :: TFrlMom                                         ! The total moment supplied by the springs, and dampers.
REAL(ReKi), INTENT(IN )      :: TFrlRate                                        ! The tail-furl rate, QDT(DOF_TFrl).


   ! Local variables:

REAL(ReKi)                   :: TFrlDMom                                        ! The moment supplied by the tail-furl dampers.
REAL(ReKi)                   :: TFrlSMom                                        ! The moment supplied by the tail-furl springs.



SELECT CASE ( TFrlMod ) ! Which tail-furl model are we using?

CASE ( 0 )              ! None!


   TFrlMom = 0.0


CASE ( 1 )              ! Standard (using inputs from the FAST furling input file).


   ! Linear spring:

   TFrlSMom = -TFrlSpr*TFrlDef


   ! Add spring-stops:

   IF ( TFrlDef > TFrlUSSP )  THEN      ! Up-stop
      TFrlSMom = TFrlSMom - TFrlUSSpr*( TFrlDef - TFrlUSSP )
   ELSEIF ( TFrlDef < TFrlDSSP )  THEN  ! Down-stop
      TFrlSMom = TFrlSMom - TFrlDSSpr*( TFrlDef - TFrlDSSP )
   ENDIF


   ! Linear damper:

   TFrlDMom = -TFrlDmp*TFrlRate


   ! Add coulomb friction:

   IF ( TFrlRate /= 0.0 )  THEN
      TFrlDMom = TFrlDMom - SIGN( TFrlCDmp, TFrlRate )
   ENDIF


   ! Add damper-stops:

   IF ( TFrlDef > TFrlUSDP )  THEN      ! Up-stop
      TFrlDMom = TFrlDMom - TFrlUSDmp*TFrlRate
   ELSEIF ( TFrlDef < TFrlDSDP )  THEN  ! Down-stop
      TFrlDMom = TFrlDMom - TFrlDSDmp*TFrlRate
   ENDIF


   ! Total up all the moments.

   TFrlMom = TFrlSMom + TFrlDMom


CASE ( 2 )              ! User-defined tail-furl spring/damper model.


   CALL UserTFrl ( TFrlDef, TFrlRate, ZTime, DirRoot, TFrlMom )


ENDSELECT



RETURN
END SUBROUTINE TFurling
!=======================================================================
SUBROUTINE TimeMarch( p_StrD, x_StrD, OtherSt_StrD, y_StrD, ErrStat, ErrMsg  )


   ! TimeMarch controls the execution of the typical time-marching
   !   simulation of the FAST code.


USE                             Features
USE                             General, ONLY : UnOuBin, Cmpl4LV
USE                             Output
USE                             SimCont
USE                             FAST_IO_Subs       ! WrOutHdr(),  SimStatus(), WrOutput()
USE                             NOISE              ! PredictNoise(), WriteAveSpecOut()

IMPLICIT                        NONE


   ! passed variables
TYPE(StrD_ParameterType),      INTENT(IN)       :: p_StrD                     ! The parameters of the structural dynamics module
TYPE(StrD_ContinuousStateType),INTENT(INOUT)    :: x_StrD                     ! The structural dynamics module's continuous states
TYPE(StrD_OtherStateType),     INTENT(INOUT)    :: OtherSt_StrD               ! The structural dynamics "other" states (including CoordSys coordinate systems)
TYPE(StrD_OutputType),         INTENT(INOUT)    :: y_StrD                     ! System outputs of the structural dynamics module


INTEGER(IntKi),                INTENT(OUT)      :: ErrStat                    ! Error status
CHARACTER(*),                  INTENT(OUT)      :: ErrMsg                     ! Error message


   ! Local variables.

REAL(ReKi)                                      :: TiLstPrn  = 0.0                           ! The time of the last print.


   ! Allocate space for coordinate systems

CALL CoordSys_Alloc( OtherSt_StrD%CoordSys, p_StrD, ErrStat, ErrMsg )

IF (ErrStat /= ErrID_none) THEN
   CALL WrScr( ' Error in SUBROUTINE TimeMarch: ' )
   CALL WrScr( '   '//TRIM(ErrMsg) )
   IF ( ErrStat >= AbortErrLev ) THEN
      CALL ProgAbort( "" )
   END IF
END IF



   ! Set up output file format.

CALL WrOutHdr( p_StrD )


   ! Start simulation.  Initialize the simulation status.

CALL WrScr1 ( '' )
CALL SimStatus



   ! Loop through time.

DO


   ! Call predictor-corrector routine:

   CALL Solver( p_StrD, x_StrD, y_StrD, OtherSt_StrD  )


   ! Make sure the rotor azimuth is not greater or equal to 360 degrees: (can't we do a mod here?)

   IF ( ( OtherSt_StrD%Q(DOF_GeAz,IC(1)) + OtherSt_StrD%Q(DOF_DrTr,IC(1)) ) >= TwoPi )  THEN
          OtherSt_StrD%Q(DOF_GeAz,IC(1)) = OtherSt_StrD%Q(DOF_GeAz,IC(1)) - TwoPi
   ENDIF


   ! Advance time:

   Step  = Step + 1
   ZTime = Step*DT


   ! Compute all of the output channels and fill in the OutData() array:

   CALL CalcOuts( p_StrD, x_StrD, y_StrD, OtherSt_StrD  )
!   CALL StrD_CalcOutput( REAL(ZTime,DbKi), u, p_StrD, x_StrD, xd, z, OtherSt_StrD, y_StrD, ErrStat, ErrMsg )

   ! Check to see if we should output data this time step:

   IF ( ZTime >= TStart )  THEN
      IF ( CompNoise                 )  CALL PredictNoise( p_StrD,                    OtherSt_StrD%CoordSys%te1, &
                                                           OtherSt_StrD%CoordSys%te2, OtherSt_StrD%CoordSys%te3 )
      IF ( MOD( Step, DecFact ) == 0 )  CALL WrOutput( p_StrD, y_StrD )
   ENDIF


   ! Display simulation status every SttsTime-seconds:

   IF ( ZTime - TiLstPrn >= SttsTime )  THEN

      TiLstPrn = ZTime

      CALL SimStatus

   ENDIF


   ! If we've reached TMax, exit the DO loop:

   IF ( ZTime > TMax .OR. Cmpl4LV )  EXIT

ENDDO


   ! We're done!


   ! Output the binary file if requested

IF (WrBinOutFile) THEN
   CALL WrBinOutput(UnOuBin, OutputFileFmtID, FileDesc, p_StrD%OutParam(:)%Name, p_StrD%OutParam(:)%Units, TimeData, &
                     AllOutData(:,1:CurrOutStep), ErrStat, ErrMsg)

   IF ( ErrStat /= ErrID_None ) THEN
      CALL WrScr( 'Error '//Num2LStr(ErrStat)//' writing binary output file: '//TRIM(ErrMsg) )
   END IF
END IF

   ! Output noise if desired:

IF ( CompNoise )  CALL WriteAveSpecOut



RETURN
END SUBROUTINE TimeMarch
!=======================================================================
SUBROUTINE TwrLoading ( JNode, X1 , X2 , X3 , X4 , X5 , X6 , &
                               XD1, XD2, XD3, XD4, XD5, XD6    )


   ! This routine computes the tower hydrodynamic loading; that is
   !   TwrAM(1:6,1:6) and TwrFt(1:6).


USE                             General
USE                             FixedBottomSupportStructure, ONLY:MorisonTwrLd
USE                             SimCont
USE                             Tower

IMPLICIT                        NONE


   ! Passed Variables:

REAL(ReKi), INTENT(IN )      :: X1                                              ! The xi-component of the translational    displacement (in m    ) of the current tower node    relative to the inertial frame origin at ground level [onshore] or MSL [offshore].
REAL(ReKi), INTENT(IN )      :: X2                                              ! The yi-component of the translational    displacement (in m    ) of the current tower node    relative to the inertial frame origin at ground level [onshore] or MSL [offshore].
REAL(ReKi), INTENT(IN )      :: X3                                              ! The zi-component of the translational    displacement (in m    ) of the current tower node    relative to the inertial frame origin at ground level [onshore] or MSL [offshore].
REAL(ReKi), INTENT(IN )      :: X4                                              ! The xi-component of the rotational       displacement (in rad  ) of the current tower element relative to the inertial frame origin at ground level [onshore] or MSL [offshore].
REAL(ReKi), INTENT(IN )      :: X5                                              ! The yi-component of the rotational       displacement (in rad  ) of the current tower element relative to the inertial frame origin at ground level [onshore] or MSL [offshore].
REAL(ReKi), INTENT(IN )      :: X6                                              ! The zi-component of the rotational       displacement (in rad  ) of the current tower element relative to the inertial frame origin at ground level [onshore] or MSL [offshore].
REAL(ReKi), INTENT(IN )      :: XD1                                             ! The xi-component of the translational        velocity (in m/s  ) of the current tower node    relative to the inertial frame origin at ground level [onshore] or MSL [offshore].
REAL(ReKi), INTENT(IN )      :: XD2                                             ! The yi-component of the translational        velocity (in m/s  ) of the current tower node    relative to the inertial frame origin at ground level [onshore] or MSL [offshore].
REAL(ReKi), INTENT(IN )      :: XD3                                             ! The zi-component of the translational        velocity (in m/s  ) of the current tower node    relative to the inertial frame origin at ground level [onshore] or MSL [offshore].
REAL(ReKi), INTENT(IN )      :: XD4                                             ! The xi-component of the rotational (angular) velocity (in rad/s) of the current tower element relative to the inertial frame origin at ground level [onshore] or MSL [offshore].
REAL(ReKi), INTENT(IN )      :: XD5                                             ! The yi-component of the rotational (angular) velocity (in rad/s) of the current tower element relative to the inertial frame origin at ground level [onshore] or MSL [offshore].
REAL(ReKi), INTENT(IN )      :: XD6                                             ! The zi-component of the rotational (angular) velocity (in rad/s) of the current tower element relative to the inertial frame origin at ground level [onshore] or MSL [offshore].

INTEGER(4), INTENT(IN )      :: JNode                                           ! The number of the current tower node / element. [1 to TwrNodes]


   ! Local variables:

REAL(ReKi), PARAMETER        :: SymTol   = 9.999E-4                             ! Tolerance used to determine if matrix PtfmAM is symmetric.
REAL(ReKi)                   :: X        (6)                                    ! The 3 components of the translational displacement (in m  ) of the current tower node and the 3 components of the rotational displacement       (in rad  ) of the current tower element relative to the inertial frame origin at ground level [onshore] or MSL [offshore].
REAL(ReKi)                   :: XD       (6)                                    ! The 3 components of the translational velocity     (in m/s) of the current tower node and the 3 components of the rotational (angular) velocity (in rad/s) of the current tower element relative to the inertial frame origin at ground level [onshore] or MSL [offshore].

INTEGER(4)                   :: I                                               ! Loops through all platform DOFs.
INTEGER(4)                   :: J                                               ! Loops through all platform DOFs.



   ! Place the displacement and velocity arguments into the local arrays,
   !   X(1:6) and XD(1:6), respectively:

X (1) = X1
X (2) = X2
X (3) = X3
X (4) = X4
X (5) = X5
X (6) = X6
XD(1) = XD1
XD(2) = XD2
XD(3) = XD3
XD(4) = XD4
XD(5) = XD5
XD(6) = XD6



   ! Compute the tower hydrodynamic loading for the current tower node /
   !   element:

SELECT CASE ( PtfmModel )  ! Which platform model are we using?

CASE ( 0 )                 ! None!


   ! Do nothing here since TwrAM and TwrFt are all initialized to zero.


CASE ( 1 )                 ! Onshore.


   ! Do nothing here since TwrAM and TwrFt are all initialized to zero.


CASE ( 2 )                 ! Fixed bottom offshore.


   SELECT CASE ( TwrLdMod )   ! Which tower loading model are we using?

   CASE ( 0 )                 ! None!


   ! Do nothing here since TwrAM and TwrFt are all initialized to zero.


   CASE ( 1 )                 ! Undocumented hydrodynamic loading using Morison's equation.


   ! CALL the undocumented Morison's equation tower loading model:

      CALL MorisonTwrLd ( JNode, DiamT(JNode), CAT(JNode), CDT(JNode), X, XD, ZTime, TwrAM, TwrFt )


   CASE ( 2 )                 ! User-defined tower loading.


   ! CALL the user-defined tower loading model:

      CALL UserTwrLd ( JNode, X, XD, ZTime, DirRoot, TwrAM, TwrFt )


   ! Ensure that the tower element added mass matrix returned by UserTwrLd,
   !   TwrAM, is symmetric; Abort if necessary:

      DO I = 1,5        ! Loop through the 1st 5 rows (columns) of TwrAM

         DO J = (I+1),6 ! Loop through all columns (rows) passed I

            IF ( ABS( TwrAM(I,J) - TwrAM(J,I) ) > SymTol )  &
               CALL ProgAbort ( ' The user-defined tower element added mass matrix is unsymmetric.'// &
                                '  Make sure TwrAM returned by UserTwrLd() is symmetric.'               )

         ENDDO          ! J - All columns (rows) passed I

      ENDDO             ! I - The 1st 5 rows (columns) of TwrAM


   ENDSELECT


CASE ( 3 )                 ! Floating offshore.


   ! Do nothing here since TwrAM and TwrFt are all initialized to zero.


ENDSELECT



RETURN
END SUBROUTINE TwrLoading
!=======================================================================
END MODULE FASTSubs
