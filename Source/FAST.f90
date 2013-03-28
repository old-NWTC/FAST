MODULE FASTSubs

   USE   NWTC_Library
   
   USE   ElastoDyn_Types
   USE   ElastoDyn_Parameters
   USE   ElastoDyn

   USE   ServoDyn_Types
   USE   ServoDyn
   
   USE   FAST_Types
   
   USE AeroDyn_Types
   USE HydroDyn_Types

   
CONTAINS
!=======================================================================
!----------------------------------------------------------------------------------------------------------------------------------  
SUBROUTINE Control( t, u, p, x, xd, z, OtherState, y, ErrStat, ErrMsg,   p_ED, x_ED, OtherState_ED )


   ! This is the main control routine.

USE                             AeroDyn

IMPLICIT                        NONE

   ! Passed variables:

   REAL(DbKi),                     INTENT(IN   )  :: t           ! Current simulation time in seconds
   TYPE(SrvD_InputType),           INTENT(IN   )  :: u           ! Inputs at t
   TYPE(SrvD_ParameterType),       INTENT(IN   )  :: p           ! Parameters
   TYPE(SrvD_ContinuousStateType), INTENT(IN   )  :: x           ! Continuous states at t
   TYPE(SrvD_DiscreteStateType),   INTENT(IN   )  :: xd          ! Discrete states at t
   TYPE(SrvD_ConstraintStateType), INTENT(IN   )  :: z           ! Constraint states at t
   TYPE(SrvD_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states
   TYPE(SrvD_OutputType),          INTENT(INOUT)  :: y           ! Outputs computed at t (Input only so that mesh con-
                                                                  !   nectivity information does not have to be recalculated)
   !INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat     ! Error status of the operation
   !CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None


TYPE(ED_ParameterType),      INTENT(IN)    :: p_ED                            ! Parameters of the structural dynamics module
TYPE(ED_ContinuousStateType),INTENT(INOUT) :: x_ED                            ! The structural dynamics module's continuous states
TYPE(ED_OtherStateType),     INTENT(INOUT) :: OtherState_ED                      ! Other State data type for Structural dynamics module

   ! Local variables:

REAL(ReKi)                   :: HHWndVec  (3)                                   ! Hub-height wind vector in the AeroDyn coordinate system, m/s.
REAL(ReKi)                   :: LinAccEO  (3)                                   ! Total linear acceleration of the base plate (point O) in the inertia frame (body E for earth).
REAL(ReKi)                   :: TwrAccel                                        ! Tower Acceleration.  Used for tower feedback control.
REAL(ReKi)                   :: WindDir                                         ! Horizontal hub-height wind direction (positive about the zi-axis), rad.
REAL(ReKi)                   :: YawError                                        ! Nacelle yaw error (positve about the zi-axis), rad.

INTEGER(4)                   :: I                                               ! Generic index
INTEGER(4)                   :: K                                               ! Loops through blades.

INTEGER(IntKi)               :: ErrStat
CHARACTER(1024)              :: ErrMsg

 
! ------------------------------ YAW CONTROL -------------------------------
 CALL Yaw_CalcOutput( t, u, p, x, xd, z, OtherState, y, ErrStat, ErrMsg )


   ! ----------------------------- PITCH CONTROL ------------------------------
   ! Control pitch if requested:

IF ( t >= p%TPCOn )  THEN   ! Time now to enable active pitch control.


   SELECT CASE ( p%PCMode )  ! Which pitch control mode are we using?

   CASE ( 0 )              ! None!


   ! Use the initial blade pitch angles:

      y%BlPitchCom = p%BlPitchInit


   CASE ( 1 )              ! User-defined from routine PitchCntrl().


   ! Calculate tower-top acceleration (fore-aft mode only) in the tower-top
   !   system:

      LinAccEO = OtherState_ED%RtHS%LinAccEOt
      DO I = 1,p_ED%DOFs%NPTE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the yaw bearing center of mass (point O)
         LinAccEO = LinAccEO + OtherState_ED%RtHS%PLinVelEO(p_ED%DOFs%PTE(I),0,:)*OtherState_ED%QD2T(p_ED%DOFs%PTE(I))
      ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the yaw bearing center of mass (point O)

      TwrAccel = DOT_PRODUCT( LinAccEO, OtherState_ED%CoordSys%b1 )


   ! Call the user-defined pitch control routine:

      CALL PitchCntrl ( u%BlPitch, y%ElecPwr, u%LSS_Spd, TwrAccel, p%NumBl, t, p%DT, p%RootName, y%BlPitchCom )

   CASE ( 2 )              ! User-defined from Simulink or Labview.


   ! Do nothing here since blade pitch is defined externally from Simulink or Labview.


   ENDSELECT


ELSE                          ! Do not control pitch yet, maintain initial pitch angles.


   ! Use the initial blade pitch angles:

   y%BlPitchCom = p%BlPitchInit


ENDIF


   ! Override standard pitch control with a linear maneuver if necessary:

DO K = 1,p%NumBl ! Loop through all blades


   IF ( t >= p%TPitManS(K) )  THEN  ! Override pitch maneuver is occuring for this blade.


      IF ( t < OtherState%BegPitMan(K) )  THEN  ! Override pitch maneuver is just beginning.

         OtherState%BlPitchI   (K) = u%BlPitch(K)                                                           ! Store the initial (current) pitch, at the start of the pitch maneuver.

         OtherState%PitManRat(K) = SIGN( OtherState%PitManRat(K), p%BlPitchF(K) - OtherState%BlPitchI(K) )   ! Modify the sign of PitManRat based on the direction of the pitch maneuever
         OtherState%TPitManE (K) = p%TPitManS(K) + ( p%BlPitchF(K) - OtherState%BlPitchI(K) )/OtherState%PitManRat(K) ! Calculate the end time of the override pitch maneuver      
         
         OtherState%BegPitMan(K) = t    ! Don't enter this part of the IF-structure again (unless we're backing up in time)

      ENDIF

      
      IF ( t >= OtherState%TPitManE(K) )  THEN      ! Override pitch maneuver has ended, blade is locked at BlPitchF.

         y%BlPitchCom(K) = p%BlPitchF(K)

      ELSE  
         
         y%BlPitchCom(K) = OtherState%BlPitchI(K) + OtherState%PitManRat(K)*( t - p%TPitManS(K) )         ! Increment the blade pitch using PitManRat
         
      END IF                  
      
   ELSE
      
      
      OtherState%BegPitMan(K) = HUGE( OtherState%BegPitMan(K) )      ! We haven't started the pitch maneuver (or will need to restart)

   ENDIF

    
ENDDO ! K - blades


   ! Set the command pitch angles to the actual pitch angles since we have no
   !   built-in pitch actuator:




RETURN
END SUBROUTINE Control
!----------------------------------------------------------------------------------------------------------------------------------  
SUBROUTINE DrvTrTrq ( t, p, u, y, ErrStat, ErrMsg )
! This routine calculates the drive-train torque (outputs GenTrq, ElecPwr, and HSSBrTrq)
!..................................................................................................................................

   IMPLICIT                        NONE


      ! Passed variables:
   REAL(DbKi), INTENT(IN)              :: t                                        ! Simulation time in seconds
   TYPE(SrvD_ParameterType),INTENT(IN) :: p                                        ! Parameters of the ServoDyn module
   TYPE(SrvD_OutputType),INTENT(INOUT) :: y                                        ! Outputs of the ServoDyn module
   TYPE(SrvD_InputType), INTENT(IN)    :: u                                        ! Inputs to the ServoDyn module

   INTEGER(IntKi),               INTENT(OUT)    :: ErrStat                      ! Error status
   CHARACTER(*),                 INTENT(OUT)    :: ErrMsg                       ! Error message


      ! Local variables:

   COMPLEX(ReKi)                :: Current1                                        ! Current passing through the stator (amps)
   COMPLEX(ReKi)                :: Current2                                        ! Current passing through the rotor (amps)
   COMPLEX(ReKi)                :: Currentm                                        ! Magnitizing current (amps)

   REAL(ReKi)                   :: ComDenom                                        ! Common denominator of variables used in the TEC model
   REAL(ReKi)                   :: HSSBrFrac                                       ! Fraction of full braking torque {0 (off) <= HSSBrFrac <= 1 (full)} (-)
   REAL(ReKi)                   :: PwrLossS                                        ! Power loss in the stator (watts)
   REAL(ReKi)                   :: PwrLossR                                        ! Power loss in the rotor (watts)
   REAL(ReKi)                   :: PwrMech                                         ! Mechanical power (watts)
   REAL(ReKi)                   :: Slip                                            ! Generator slip.
   REAL(ReKi)                   :: SlipRat                                         ! Generator slip ratio.

LOGICAL,    SAVE             :: GenOnLin = .FALSE.                              ! Is the generator online?
LOGICAL,    SAVE             :: Off4Good = .FALSE.                              ! Is the generator offline for good?


INTEGER(IntKi), PARAMETER :: ControlMode_None   = 0
INTEGER(IntKi), PARAMETER :: ControlMode_Simple = 1 
INTEGER(IntKi), PARAMETER :: ControlMode_User   = 2
INTEGER(IntKi), PARAMETER :: ControlMode_Extern = 3


      ! Initialize variables
   ErrStat = ErrID_None
   ErrMsg  = ''



      ! See if the generator is on line.

   IF (  .NOT. Off4Good )  THEN

      ! The generator is either on-line or has never been turned online.

      IF ( GenOnLin )  THEN   ! The generator is on-line.

         IF ( ( p%GenTiStp ) .AND. ( t >= p%TimGenOf ) )  THEN   ! Shut-down of generator determined by time, TimGenOf
            GenOnLin = .FALSE.
            Off4Good = .TRUE.
         ENDIF

      ELSE                    ! The generator has never been turned online.

         IF ( p%GenTiStr )  THEN   ! Start-up of generator determined by time, TimGenOn
            IF ( t >= p%TimGenOn )    GenOnLin = .TRUE.
         ELSE                    ! Start-up of generator determined by HSS speed, SpdGenOn
            IF ( u%HSS_Spd >= p%SpdGenOn )  GenOnLin = .TRUE.
         ENDIF

      ENDIF

   ENDIF


   IF ( GenOnLin )  THEN                    ! Generator is on line.


      ! Are we doing simple variable-speed control, or using a generator model?

      SELECT CASE ( p%VSContrl )               ! Are we using variable-speed control?

         CASE ( ControlMode_None )                ! No variable-speed control.  Using a generator model.


            SELECT CASE ( p%GenModel )            ! Which generator model are we using?

               CASE ( 1_IntKi )                          ! Simple induction-generator model.


                  Slip = u%HSS_Spd - p%SIG_SySp

                  IF ( ABS( Slip ) > p%SIG_POSl  )  THEN
                     y%GenTrq  = SIGN( p%SIG_POTq, Slip )
                  ELSE
                     y%GenTrq  = Slip*p%SIG_Slop
                  ENDIF
                  !GenTrq     = GenTrq + DelGenTrq  ! Add the pertubation on generator torque, DelGenTrq.  This is used only for FAST linearization (it is zero otherwise).


            ! The generator efficiency is either additive for motoring,
            !   or subtractive for generating power.

                  IF ( y%GenTrq > 0.0 )  THEN
                     y%ElecPwr = y%GenTrq * u%HSS_Spd * p%GenEff
                  ELSE
                     y%ElecPwr = y%GenTrq * u%HSS_Spd / p%GenEff
                  ENDIF


               CASE ( 2_IntKi )                          ! Thevenin-equivalent generator model.


                  SlipRat  = ( u%HSS_Spd - p%TEC_SySp )/p%TEC_SySp

                  y%GenTrq  = p%TEC_A0*(p%TEC_VLL**2)*SlipRat &
                             /( p%TEC_C0 + p%TEC_C1*SlipRat + p%TEC_C2*(SlipRat**2) )

                  ComDenom = ( p%TEC_Re1 - p%TEC_RRes/SlipRat )**2 + ( p%TEC_Xe1 + p%TEC_RLR )**2
                  Current2 = CMPLX(  p%TEC_V1a*( p%TEC_Re1 - p%TEC_RRes/SlipRat )/ComDenom , &
                                    -p%TEC_V1a*( p%TEC_Xe1 + p%TEC_RLR          )/ComDenom     )
                  Currentm = CMPLX( 0.0 , -p%TEC_V1a/p%TEC_MR )
                  Current1 = Current2 + Currentm
                  PwrLossS = 3.0*( ( ABS( Current1 ) )**2 )*p%TEC_SRes
                  PwrLossR = 3.0*( ( ABS( Current2 ) )**2 )*p%TEC_RRes
                  PwrMech  = y%GenTrq*u%HSS_Spd
                  y%ElecPwr  = PwrMech - PwrLossS - PwrLossR


               CASE ( 3_IntKi )                          ! User-defined generator model.


         !        CALL UserGen ( u%HSS_Spd, u%LSS_Spd, p%NumBl, t, DT, p%GenEff, DelGenTrq, DirRoot, GenTrq, ElecPwr )
                  CALL UserGen ( u%HSS_Spd, u%LSS_Spd, p%NumBl, t, p%DT, p%GenEff, 0.0_ReKi, p%RootName, y%GenTrq, y%ElecPwr )

      !bjj check the ABS here (above)
            END SELECT


         CASE ( ControlMode_Simple )              ! Simple variable-speed control.


         ! Compute the generator torque, which depends on which region we are in:

            IF ( u%HSS_Spd >= p%VS_RtGnSp )  THEN      ! We are in region 3 - torque is constant
               y%GenTrq = p%VS_RtTq
            ELSEIF ( u%HSS_Spd < p%VS_TrGnSp )  THEN   ! We are in region 2 - torque is proportional to the square of the generator speed
               y%GenTrq = p%VS_Rgn2K* (u%HSS_Spd**2)
            ELSE                                   ! We are in region 2 1/2 - simple induction generator transition region
               y%GenTrq = p%VS_Slope*( u%HSS_Spd - p%VS_SySp )
            ENDIF

            !GenTrq  = GenTrq + DelGenTrq  ! Add the pertubation on generator torque, DelGenTrq.  This is used only for FAST linearization (it is zero otherwise).


         ! It's not possible to motor using this control scheme,
         !   so the generator efficiency is always subtractive.

            y%ElecPwr = y%GenTrq*u%HSS_Spd*p%GenEff


         CASE ( ControlMode_User )                              ! User-defined variable-speed control for routine UserVSCont().


      !      CALL UserVSCont ( u%HSS_Spd, u%LSS_Spd, p%NumBl, t, DT, p%GenEff, DelGenTrq, DirRoot, GenTrq, ElecPwr )
            CALL UserVSCont ( u%HSS_Spd, u%LSS_Spd, p%NumBl, t, p%DT, p%GenEff, 0.0_ReKi, p_FAST%DirRoot, y%GenTrq, y%ElecPwr )


         CASE ( ControlMode_Extern )                             ! User-defined variable-speed control from Simulink or Labview.

         ! No need to define GenTrq or ElecPwr here since this is defined externally
         !   by Simulink or Labview.  Also, no reason to perturb generator torque here either,
         !   since linearization does not work with Simulink or Labview.
   
            y%GenTrq  = u%ExternalGenTrq
            y%ElecPwr = u%ExternalElecPwr      

      END SELECT


      ! Lets turn the generator offline for good if ( GenTiStp = .FALSE. ) .AND. ( ElecPwr <= 0.0 ):

      IF ( ( .NOT. p%GenTiStp ) .AND. ( y%ElecPwr <= 0.0_ReKi ) ) THEN   ! Shut-down of generator determined by generator power = 0
         y%GenTrq   = 0.0
         y%ElecPwr  = 0.0

         GenOnLin = .FALSE.
         Off4Good = .TRUE.
      ENDIF

   ELSE                                     ! Generator is off line.

      y%GenTrq  = 0.0
      y%ElecPwr = 0.0

   ENDIF


   !.................................................................................
   ! Calculate the fraction of applied HSS-brake torque, HSSBrFrac:
   !.................................................................................

   IF ( t < p%THSSBrDp )  THEN    ! HSS brake not deployed yet.

      HSSBrFrac = 0.0

   ELSE                             ! HSS brake deployed.


      SELECT CASE ( p%HSSBrMode )                 ! Which HSS brake model are we using?

      CASE ( ControlMode_Simple )                 ! Simple built-in HSS brake model with linear ramp.

         IF ( t < p%THSSBrFl )  THEN ! Linear ramp
            HSSBrFrac = ( t - p%THSSBrDp )/p%HSSBrDT
         ELSE                        ! Full braking torque
            HSSBrFrac = 1.0
         ENDIF

      CASE ( ControlMode_User )                   ! User-defined HSS brake model.

         CALL UserHSSBr ( y%GenTrq, y%ElecPwr, u%HSS_Spd, p%NumBl, t, p%DT, p%RootName, HSSBrFrac )

         IF ( ( HSSBrFrac < 0.0 ) .OR. ( HSSBrFrac > 1.0 ) )  THEN   ! 0 (off) <= HSSBrFrac <= 1 (full); else Abort.
            ErrStat = ErrID_Fatal
            ErrMsg  = 'HSSBrFrac must be between 0.0 (off) and 1.0 (full) (inclusive).  Fix logic in routine UserHSSBr().'
            RETURN
         END IF
      
      CASE ( ControlMode_Extern )                 ! HSS brake model from Labview.

         HSSBrFrac = u%ExternalHSSBrFrac

      ENDSELECT


   ENDIF


      ! Calculate the magnitude of HSS brake torque:

   y%HSSBrTrq = SIGN( HSSBrFrac*p%HSSBrTqF, u%HSS_Spd )  ! Scale the full braking torque by the brake torque fraction and make sure the brake torque resists motion.

   RETURN
END SUBROUTINE DrvTrTrq
!----------------------------------------------------------------------------------------------------------------------------------  
SUBROUTINE FixHSSBrTq ( Integrator, p, OtherState, AugMat, HSSBrTrq  )


   ! This routine is used to adjust the HSSBrTrq value if the absolute
   !   magnitudue of the HSS brake torque was strong enough to reverse
   !   the direction of the HSS, which is a physically impossible
   !   situation.  The problem arises since we are integrating in
   !   discrete time, not continuous time.


   ! AeroDyn MODULES:

USE                             Switch


IMPLICIT                        NONE


   ! Passed variables:

CHARACTER(9),            INTENT(IN )  :: Integrator                           ! A string holding the current integrator being used.
TYPE(ED_ParameterType),  INTENT(IN)   :: p                                    ! The parameters of the structural dynamics module
TYPE(ED_OtherStateType), INTENT(INOUT):: OtherState                           ! Other State data type for Structural dynamics module
REAL(ReKi),              INTENT(INOUT):: AugMat   (p%NDOF,p%NAug)             ! The augmented matrix used for the solution of the QD2T()s.
REAL(ReKi),              INTENT(INOUT):: HSSBrTrq                             ! Instantaneous HSS brake torque


   ! Local variables:

REAL(ReKi)                   :: HSSBrTrqC                                       ! A copy of the value of HSSBrTrq calculated in SUBROUTINE DrvTrTrq().
REAL(ReKi)                   :: RqdFrcGeAz                                      ! The force term required to produce RqdQD2GeAz.
REAL(ReKi)                   :: RqdQD2GeAz                                      ! The required QD2T(DOF_GeAz) to cause the HSS to stop rotating.

REAL(ReKi)                   :: SolnVec(   p%NDOF)                              ! Solution vector found by solving the equations of motion
REAL(ReKi)                   :: QD2TC     (p%NDOF)                              ! A copy of the value of QD2T
REAL(ReKi)                   :: OgnlGeAzRo(p%NAUG)                              ! The original elements of AugMat that formed the DOF_GeAz equation before application of known initial conditions.

INTEGER(4)                   :: I                                               ! Loops through all DOFs.


INTEGER(IntKi)  :: ErrStat     ! Error status of the operation
CHARACTER(1024) :: ErrMsg      ! Error message if ErrStat /= ErrID_None


   ! Make a copy of the current value of HSSBrTrq and QD2Tfor future use:
HSSBrTrqC = HSSBrTrq
QD2TC     = OtherState%QD2T

   ! Store the row of coefficients associated with the generator azimuth DOF for future use:
OgnlGeAzRo = AugMat(DOF_GeAz,:)


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

   RqdQD2GeAz = ( -      OtherState%QD (DOF_GeAz,OtherState%IC(1))/p%DT24 - 19.0*OtherState%QD2(DOF_GeAz,OtherState%IC(1)) &
                  +  5.0*OtherState%QD2(DOF_GeAz,OtherState%IC(2))        -      OtherState%QD2(DOF_GeAz,OtherState%IC(3))   )/ 9.0

CASE ('Predictor')

   ! Find the required QD2T(DOF_GeAz) to cause the HSS to stop rotating (RqdQD2GeAz).
   ! This is found by solving the predictor formula for QD2(DOF_GeAz,IC(1))
   !   when QD(DOF_GeAz,IC(NMX)) equals zero.

   RqdQD2GeAz = ( -      OtherState%QD (DOF_GeAz,OtherState%IC(1))/p%DT24 + 59.0*OtherState%QD2(DOF_GeAz,OtherState%IC(2)) &
                  - 37.0*OtherState%QD2(DOF_GeAz,OtherState%IC(3))        +  9.0*OtherState%QD2(DOF_GeAz,OtherState%IC(4))   )/55.0

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

DO I = 1,p%DOFs%NActvDOF ! Loop through all active (enabled) DOFs

   AugMat(p%DOFs%SrtPS(I),    p%NAug) = AugMat(p%DOFs%SrtPS(I),p%NAug) - AugMat(p%DOFs%SrtPS(I),DOF_GeAz)*RqdQD2GeAz  ! {{Fa}-[Cab]{Qb}}
   AugMat(p%DOFs%SrtPS(I),DOF_GeAz) = 0.0                                                           ! [0]
   AugMat(DOF_GeAz,p%DOFs%SrtPS(I)) = 0.0                                                           ! [0]

ENDDO             ! I - All active (enabled) DOFs

   AugMat(DOF_GeAz,DOF_GeAz) = 1.0                                                           ! [I]{Qb}={Qb}
   AugMat(DOF_GeAz,    p%NAug) = RqdQD2GeAz                                                    !


   ! Invert the matrix to solve for the new (updated) accelerations.  Like in
   !   RtHS(), the accelerations are returned by GaussElim() in the first NActvDOF
   !   elements of the solution vector, SolnVec().  These are transfered to the
   !   proper index locations of the acceleration vector QD2T() using the
   !   vector subscript array SrtPS(), after Gauss() has been called:
   ! NOTE: QD2T( SrtPS(1:NActvDOF) ) cannot be sent directly because arrays
   !   sections with vector subscripts must not be used in INTENT(OUT)
   !   arguments.

CALL GaussElim( AugMat( p%DOFs%SrtPS    (1: p%DOFs%NActvDOF  )   ,         &
                        p%DOFs%SrtPSNAUG(1:(p%DOFs%NActvDOF+1)) ),         &
                                                     p%DOFs%NActvDOF,       SolnVec, ErrStat, ErrMsg )

!IF ( ErrStat /= ErrID_None ) CALL WrScr( ' Message from FixHSSBrTq: '//TRIM(ErrMsg) )

OtherState%QD2T = 0.0
DO I = 1,p%DOFs%NActvDOF ! Loop through all active (enabled) DOFs
   OtherState%QD2T(p%DOFs%SrtPS(I)) = SolnVec(I)
ENDDO             ! I - All active (enabled) DOFs


   ! Find the force required to produce RqdQD2GeAz from the equations of
   !   motion using the new accelerations:

RqdFrcGeAz = 0.0
DO I = 1,p%DOFs%NActvDOF ! Loop through all active (enabled) DOFs
   RqdFrcGeAz = RqdFrcGeAz + OgnlGeAzRo(p%DOFs%SrtPS(I))*OtherState%QD2T(p%DOFs%SrtPS(I))  ! {Fb}=[Cba]{Qa}+[Cbb]{Qb}
ENDDO             ! I - All active (enabled) DOFs


   ! Find the HSSBrTrq necessary to bring about this force:

HSSBrTrq = HSSBrTrqC + ( ( OgnlGeAzRo(p%NAug) - RqdFrcGeAz )*OtherState%RtHS%GBoxEffFac/ABS(p%GBRatio) )


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
   OtherState%QD2T     = QD2TC

ELSE


   ! Use the new accelerations to update the DOF values.  Again, this
   !   depends on the integrator type:

   SELECT CASE (Integrator)

   CASE ('Corrector')

   ! Update QD and QD2 with the new accelerations using the corrector.
   ! This will make QD(DOF_GeAz,IC(NMX)) equal to zero and adjust all
   !    of the other QDs as necessary.
   ! The Q's are unnaffected by this change.

      OtherState%QD2(:,OtherState%IC(NMX)) = OtherState%QD2T

      DO I = 1,p%NDOF  ! Loop through all DOFs
         OtherState%QD(I,OtherState%IC(NMX)) = OtherState%QD(I,OtherState%IC(1)) + p%DT24*( 9.0*OtherState%QD2(I,OtherState%IC(NMX)) &
                                                                 + 19.0*OtherState%QD2(I,OtherState%IC(1  )) &
                                                                 -  5.0*OtherState%QD2(I,OtherState%IC(2  )) &
                                                                 +      OtherState%QD2(I,OtherState%IC(3  )) )
      ENDDO          ! I - All DOFs

   CASE ('Predictor')

   ! Update QD2 with the new accelerations.  Use IC(1) instead of IC(NMX)
   !   since the IC array has already been incremented.
   ! This will make QD(DOF_GeAz,IC(NMX)) equal to zero and adjust all
   !    of the other QDs as necessary during the next time step.

      OtherState%QD2(:,OtherState%IC(  1)) = OtherState%QD2T

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
SUBROUTINE FAST_End( p_FAST, y_FAST, ErrStat, ErrMsg )
! This subroutine is called at program termination. It writes any additional output files,
! deallocates variables and closes files.
!----------------------------------------------------------------------------------------------------

!add for bladed dll   USE            BladedDLLParameters

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
   
   IF (p_FAST%WrBinOutFile) THEN
      
      FileDesc = TRIM(y_FAST%FileDescLines(1))//' '//TRIM(y_FAST%FileDescLines(2))//'; '//TRIM(y_FAST%FileDescLines(3))
      
      CALL WrBinFAST(TRIM(p_FAST%OutFileRoot)//'.outb', OutputFileFmtID, TRIM(FileDesc), &
            y_FAST%ChannelNames, y_FAST%ChannelUnits, y_FAST%TimeData, y_FAST%AllOutData(:,1:y_FAST%n_Out), ErrStat, ErrMsg)

      IF ( ErrStat /= ErrID_None ) CALL WrScr( TRIM(GetErrStr(ErrStat))//' when writing binary output file: '//TRIM(ErrMsg) )
      
   END IF   
   
   
   !-------------------------------------------------------------------------------------------------
   ! Close the text tabular output file 
   !-------------------------------------------------------------------------------------------------
   CLOSE( y_FAST%UnOu )       ! I/O unit number for the tabular output file
         
   
   !-------------------------------------------------------------------------------------------------
   ! Deallocate arrays
   !-------------------------------------------------------------------------------------------------

      ! Output
   IF ( ALLOCATED(y_FAST%AllOutData                  ) ) DEALLOCATE(y_FAST%AllOutData                  )
   IF ( ALLOCATED(y_FAST%TimeData                    ) ) DEALLOCATE(y_FAST%TimeData                    )
   IF ( ALLOCATED(y_FAST%ChannelNames                ) ) DEALLOCATE(y_FAST%ChannelNames                )
   IF ( ALLOCATED(y_FAST%ChannelUnits                ) ) DEALLOCATE(y_FAST%ChannelUnits                )   
   
   
!.............   
   
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


   !-------------------------------------------------------------------------------------------------
   ! Reset stored variables
   !-------------------------------------------------------------------------------------------------
!add for bladed dll    CALL DLL_Terminate()



END SUBROUTINE FAST_End
!=======================================================================
SUBROUTINE PtfmLoading(t, x, PtfmAM, PtfmFt)


   ! This routine computes the platform loading; that is PtfmAM(1:6,1:6)
   !   and PtfmFt(1:6).

USE                             HydroDyn_Types

IMPLICIT                        NONE


   ! passed variables
REAL(DbKi), INTENT(IN) :: t ! simulation time
TYPE(ED_ContinuousStateType),INTENT(IN)  :: x                          ! The structural dynamics module's continuous states
REAL(ReKi),INTENT(OUT)                     :: PtfmAM (6,6)               ! Platform added mass matrix.
REAL(ReKi),INTENT(OUT)                     :: PtfmFt   (6)               ! The surge/xi (1), sway/yi (2), and heave/zi (3)-components of the portion of the platform force at the platform reference (point Z) and the roll/xi (4), pitch/yi (5), and yaw/zi (6)-components of the portion of the platform moment acting at the platform (body X) / platform reference (point Z) associated with everything but the QD2T()'s.

   ! Local variables:

!REAL(ReKi), PARAMETER        :: SymTol   = 9.999E-4                     ! Tolerance used to determine if matrix PtfmAM is symmetric.

INTEGER(4)                   :: I                                        ! Loops through all platform DOFs.
INTEGER(4)                   :: J                                        ! Loops through all platform DOFs.



   IF ( p_FAST%CompUserPtfmLd ) THEN

      ! CALL the user-defined platform loading model:


      CALL UserPtfmLd ( x%QT(1:6), x%QDT(1:6), t, p_FAST%DirRoot, PtfmAM, PtfmFt )

         ! Ensure that the platform added mass matrix returned by UserPtfmLd, PtfmAM, is symmetric; Abort if necessary:
      IF ( .NOT. IsSymmetric( PtfmAM ) ) THEN
         CALL ProgAbort ( ' The user-defined platform added mass matrix is unsymmetric.'// &
                           '  Make sure PtfmAM returned by UserPtfmLd() is symmetric.'        )
      END IF      
         
   ELSE
      
      ! set PtfmAM and PtfmFt to zero
      
      PtfmAM = 0.0
      PtfmFt = 0.0
      
   END IF



IF ( p_FAST%CompHydro .AND. .NOT. HD_TwrNodes ) THEN ! bjj: make sure these are point measurements, not per unit length!!!

   PtfmAM      = PtfmAM      + HD_AllLoads%Substructure(1)%AddedMass
   PtfmFt(1:3) = PtfmFt(1:3) + HD_AllLoads%Substructure(1)%Force
   PtfmFt(4:6) = PtfmFt(4:6) + HD_AllLoads%Substructure(1)%Moment

END IF


RETURN
END SUBROUTINE PtfmLoading
!=======================================================================

!=======================================================================
SUBROUTINE RtHS( t, p, x, OtherState, u, y, p_SrvD, y_SrvD, u_SrvD, OtherState_SrvD, AugMatOut )


   ! This routine is used to set up and solve the equations of motion
   !   for a particular time step.


IMPLICIT                        NONE


   ! Passed variables
REAL(DbKi), INTENT(IN) :: t ! time   

TYPE(ED_InputType),          INTENT( INOUT)  :: u                            ! The inputs for the structural dynamics module
TYPE(ED_OutputType),         INTENT( INOUT)  :: y                            ! The outputs of the structural dynamics module
TYPE(ED_ParameterType),      INTENT(IN)      :: p                            ! The parameters of the structural dynamics module
TYPE(ED_ContinuousStateType),INTENT(INOUT)   :: x                            ! The structural dynamics module's continuous states
TYPE(ED_OtherStateType),     INTENT(INOUT)   :: OtherState                   ! Other State data type for Structural dynamics module

REAL(ReKi), OPTIONAL,        INTENT(OUT)      :: AugMatOut (p%NDOF,p%NAug)    ! The augmented matrix used for the solution of the QD2T()s.

!bjj: should be type IN only: (change when AeroDyn and HydroDyn are not called from this routine)

TYPE(SrvD_ParameterType),    INTENT(IN)     :: p_SrvD           ! The parameters of the ServoDyn module
TYPE(SrvD_OutputType),       INTENT(INOUT)  :: y_SrvD           ! Outputs of the ServoDyn module
TYPE(SrvD_InputType),        INTENT(INOUT)  :: u_SrvD           ! Inputs at t
TYPE(SrvD_OtherStateType),   INTENT(INOUT)  :: OtherState_SrvD  ! Other/optimization states

   ! temporary (unused) local variables:
   TYPE(SrvD_ContinuousStateType)  :: x_SrvD           ! Continuous states at t
   TYPE(SrvD_DiscreteStateType)    :: xd_SrvD          ! Discrete states at t
   TYPE(SrvD_ConstraintStateType)  :: z_SrvD           ! Constraint states at t
   
   
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
REAL(ReKi)                   :: LinVelHS  (3)                                   ! Relative linear velocity of the current point on the current blade (point S) in the hub frame (body H)
REAL(ReKi)                   :: LinVelXO  (3)                                   ! Relative linear velocity of the tower-top / base plate             (point O) in the platform  (body X).
REAL(ReKi)                   :: LinVelXT  (3)                                   ! Relative linear velocity of the current point on the tower         (point T) in the platform  (body X).
REAL(ReKi)                   :: MomLPRot  (3)                                   ! The total moment on the low-speed shaft at point P caused by the rotor.
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

   ! variables that used to be in MODULE RtHndSd, but aren't used elsewhere:
REAL(ReKi)                   :: FKAero   (3)                                    ! The tail fin aerodynamic force acting at point K, the center-of-pressure of the tail fin.
REAL(ReKi)                   :: MAAero   (3)                                    ! The tail fin aerodynamic moment acting at point K, the center-of-pressure of the tail fin.
REAL(ReKi)                   :: FrcVGnRtt(3)                                    ! Portion of the force at the rotor-furl axis (point V   ) due to the structure that furls with the rotor, generator, and rotor associated with everything but the QD2T()'s.
REAL(ReKi)                   :: FrcWTailt(3)                                    ! Portion of the force at the  tail-furl axis (point W   ) due to the tail associated with everything but the QD2T()'s.
REAL(ReKi)                   :: FrcZAllt (3)                                    ! Portion of the force at platform reference  (point Z   ) due to everything associated with everything but the QD2T()'s.
REAL(ReKi)                   :: MomXAllt (3)                                    ! Portion of the moment at the platform   (body X) / platform reference                (point Z   ) due to everything associated with everything but the QD2T()'s.
REAL(ReKi)                   :: SolnVec    (p%NDOF)                             ! Solution vector found by solving the equations of motion
REAL(ReKi)                   :: AngAccEFt  (p%TwrNodes,3)                       ! Portion of the angular acceleration of tower element J                                               (body F) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.
REAL(ReKi)                   :: AngVelEF   (p%TwrNodes,3)                       ! Angular velocity of the current point on the tower                                (body F) in the inertia frame (body E for earth).
REAL(ReKi)                   :: LinVelET   (p%TwrNodes,3)                       ! Linear velocity of current point on the tower         (point T) in the inertia frame.
REAL(ReKi)                   :: LinVelESm2 (p%NumBl)                            ! The m2-component (closest to tip) of LinVelES.
REAL(ReKi)                   :: PAngVelEA  (p%NDOF,0:1,3)                       ! Partial angular velocity (and its 1st time derivative) of the tail                                                      (body A) in the inertia frame (body E for earth).
REAL(ReKi)                   :: PAngVelEF  (p%TwrNodes,p%NDOF,0:1,3)            ! Partial angular velocity (and its 1st time derivative) of tower element J                                               (body F) in the inertia frame (body E for earth).
REAL(ReKi)                   :: PAngVelEG  (p%NDOF,0:1,3)                       ! Partial angular velocity (and its 1st time derivative) of the generator                                                 (body G) in the inertia frame (body E for earth).
REAL(ReKi)                   :: PAngVelEH  (p%NDOF,0:1,3)                       ! Partial angular velocity (and its 1st time derivative) of the hub                                                       (body H) in the inertia frame (body E for earth).
REAL(ReKi)                   :: PAngVelEL  (p%NDOF,0:1,3)                       ! Partial angular velocity (and its 1st time derivative) of the low-speed shaft                                           (body L) in the inertia frame (body E for earth).
REAL(ReKi)                   :: PAngVelEM  (p%NumBl,p%TipNode,p%NDOF,0:1,3)     ! Partial angular velocity (and its 1st time derivative) of eleMent J of blade K                                          (body M) in the inertia frame (body E for earth).
REAL(ReKi)                   :: PAngVelEN  (p%NDOF,0:1,3)                       ! Partial angular velocity (and its 1st time derivative) of the nacelle                                                   (body N) in the inertia frame (body E for earth).
REAL(ReKi)                   :: PFrcVGnRt  (p%NDOF,3)                           ! Partial force at the rotor-furl axis (point V   ) due to the structure that furls with the rotor, generator, and rotor.
REAL(ReKi)                   :: PFrcWTail  (p%NDOF,3)                           ! Partial force at the  tail-furl axis (point W   ) due to the tail.
REAL(ReKi)                   :: PFrcZAll   (p%NDOF,3)                           ! Partial force at the platform reference (point Z) due to everything.
REAL(ReKi)                   :: PLinVelEC  (p%NDOF,0:1,3)                       ! Partial linear velocity (and its 1st time derivative) of the hub center of mass            (point C) in the inertia frame (body E for earth).
REAL(ReKi)                   :: PLinVelED  (p%NDOF,0:1,3)                       ! Partial linear velocity (and its 1st time derivative) of the center of mass of the structure that furls with the rotor (not including rotor) (point D) in the inertia frame (body E for earth).
REAL(ReKi)                   :: PLinVelEI  (p%NDOF,0:1,3)                       ! Partial linear velocity (and its 1st time derivative) of the tail boom center of mass                                                        (point I) in the inertia frame (body E for earth).
REAL(ReKi)                   :: PLinVelEJ  (p%NDOF,0:1,3)                       ! Partial linear velocity (and its 1st time derivative) of the tail fin  center of mass                                                        (point J) in the inertia frame (body E for earth).
REAL(ReKi)                   :: PLinVelEK  (p%NDOF,0:1,3)                       ! Partial linear velocity (and its 1st time derivative) of the tail fin  center of pressure                                                    (point K) in the inertia frame (body E for earth).
REAL(ReKi)                   :: PLinVelEP  (p%NDOF,0:1,3)                       ! Partial linear velocity (and its 1st time derivative) of the teeter pin                                                                      (point P) in the inertia frame (body E for earth).
REAL(ReKi)                   :: PLinVelEQ  (p%NDOF,0:1,3)                       ! Partial linear velocity (and its 1st time derivative) of the apex of rotation                                                                (point Q) in the inertia frame (body E for earth).
REAL(ReKi)                   :: PLinVelEU  (p%NDOF,0:1,3)                       ! Partial linear velocity (and its 1st time derivative) of the nacelle center of mass                                                          (point U) in the inertia frame (body E for earth).
REAL(ReKi)                   :: PLinVelEV  (p%NDOF,0:1,3)                       ! Partial linear velocity (and its 1st time derivative) of the selected point on the rotor-furl axis                                           (point V) in the inertia frame (body E for earth).
REAL(ReKi)                   :: PLinVelEW  (p%NDOF,0:1,3)                       ! Partial linear velocity (and its 1st time derivative) of the selected point on the  tail-furl axis                                           (point W) in the inertia frame (body E for earth).
REAL(ReKi)                   :: PLinVelEY  (p%NDOF,0:1,3)                       ! Partial linear velocity (and its 1st time derivative) of the platform mass center                                                            (point Y) in the inertia frame (body E for earth).
REAL(ReKi)                   :: PMomXAll   (p%NDOF,3)                           ! Partial moment at the platform   (body X) / platform reference                (point Z   ) due to the everything.
REAL(ReKi)                   :: rQS        (p%NumBl,p%TipNode,3)                ! Position vector from the apex of rotation (point Q   ) to a point on a blade (point S).
REAL(ReKi)                   :: AugMat     (p%NDOF,p%NAug)                      ! The augmented matrix used for the solution of the QD2T()s.

REAL(ReKi)                   :: GBoxTrq                                         ! Gearbox torque on the LSS side in N-m (calculated from inputs).

   ! local integer variables
   
INTEGER(4)                   :: I                                               ! Loops through some or all of the DOFs.
INTEGER(4)                   :: J                                               ! Loops through nodes / elements.
INTEGER(4)                   :: K                                               ! Loops through blades.
INTEGER(4)                   :: L                                               ! Generic index
INTEGER(4), SAVE             :: SgnPrvLSTQ = 1                                  ! The sign of the low-speed shaft torque from the previous call to RtHS().  This is calculated at the end of RtHS().  NOTE: The low-speed shaft torque is assumed to be positive at the beginning of the run!

INTEGER(IntKi)  :: ErrStat     ! Error status of the operation
CHARACTER(1024) :: ErrMsg      ! Error message if ErrStat /= ErrID_None



   ! Control the turbine's yaw and pitch, except during the first time step and
   !   only during a time-marching analysis (we can't call Control during the
   !   first time step since none of the output parameters needed for feedback
   !   of control measurements are computed until the end of the first time
   !   step):

! linking with ServoDyn 
!....................................
   
   ! ED outputs for SrvD:
   
y%Yaw      = x%QT( DOF_Yaw)
y%YawRate  = x%QDT(DOF_Yaw)
y%BlPitch  = OtherState%BlPitch
y%LSS_Spd  = x%QDT(DOF_GeAz)
y%HSS_Spd  = ABS(p%GBRatio)*x%QDT(DOF_GeAz)
y%RotSpeed = x%QDT(DOF_GeAz) + x%QDT(DOF_DrTr)

   ! map ED outputs to SrvD inputs:
u_SrvD%Yaw      = y%Yaw
u_SrvD%YawRate  = y%YawRate
u_SrvD%BlPitch  = y%BlPitch
u_SrvD%LSS_Spd  = y%LSS_Spd
u_SrvD%HSS_Spd  = y%HSS_Spd
u_SrvD%RotSpeed = y%RotSpeed 

IF ( t > 0.0_DbKi  )  THEN
   
   CALL Control( t, u_SrvD, p_SrvD, x_SrvD, xd_SrvD, z_SrvD, OtherState_SrvD, y_SrvD, ErrStat, ErrMsg, p, x, OtherState ) !bjj: note that OtherState%CoordSys%b1 hasn't been set yet when the simulation starts....
   
      ! map SrvD outputs to ED inputs:
   u%BlPitchCom = y_SrvD%BlPitchCom
   u%YawMom     = y_SrvD%YawMom
   
   
      ! ...
   OtherState%BlPitch = u%BlPitchCom
            
END IF

CALL DrvTrTrq( t, p_SrvD, u_SrvD, y_SrvD, ErrStat, ErrMsg  ) ! Compute generator and HSS-brake torque on LSS-side, GBoxTrq
IF (ErrStat /= ErrID_None) RETURN

!bjj most of the stuff inside is commented out, just the output mapping is left for now...
CALL SrvD_CalcOutput( t, u_SrvD, p_SrvD, x_SrvD, xd_SrvD, z_SrvD, OtherState_SrvD, y_SrvD, ErrStat, ErrMsg )

      ! map SrvD outputs to ED inputs:
u%GenTrq   = y_SrvD%GenTrq
u%HSSBrTrq = y_SrvD%HSSBrTrq

  


!....................................

   ! Initialize several variables to 0.0:

AugMat      = 0.0


LinAccECt   = 0.0
LinAccEDt   = 0.0
OtherState%RtHS%LinAccEIMUt = 0.0
LinAccEIt   = 0.0
LinAccEJt   = 0.0
LinAccEKt   = 0.0
OtherState%RtHS%LinAccEOt   = 0.0
LinAccEPt   = 0.0
LinAccEQt   = 0.0
OtherState%RtHS%LinAccESt   = 0.0
OtherState%RtHS%LinAccETt   = 0.0
LinAccEUt   = 0.0
LinAccEVt   = 0.0
LinAccEWt   = 0.0
LinAccEYt   = 0.0
OtherState%RtHS%LinAccEZt   = 0.0



   ! Let's define the coordinate systems that will be used throughout this routine:

CALL SetCoordSy( t, OtherState%CoordSys, OtherState%RtHS, OtherState%BlPitch, p, x, ErrStat, ErrMsg )

   !-------------------------------------------------------------------------------------------------
   ! Positions
   !-------------------------------------------------------------------------------------------------

   ! Define the position vectors between the various points on the wind turbine
   !   that are not dependent on the distributed tower or blade parameters:

OtherState%RtHS%rZ    = x%QT(DOF_Sg)* OtherState%CoordSys%z1 + x%QT(DOF_Hv)* OtherState%CoordSys%z2 - x%QT(DOF_Sw)* OtherState%CoordSys%z3                   ! Position vector from inertia frame origin to platform reference (point Z).
                rZY   =      p%rZYzt* OtherState%CoordSys%a2                                                                           ! Position vector from platform reference (point Z) to platform mass center (point Y).
                rZT0  =     p%rZT0zt* OtherState%CoordSys%a2                                                                           ! Position vector from platform reference (point Z) to tower base (point T(0))
OtherState%RtHS%rZO   = ( x%QT(DOF_TFA1) + x%QT(DOF_TFA2)                                             )*OtherState%CoordSys%a1 &       ! Position vector from platform reference (point Z) to tower-top / base plate (point O).
                      + ( p%RefTwrHt - 0.5*(       p%AxRedTFA(1,1,p%TTopNode)*x%QT(DOF_TFA1)*x%QT(DOF_TFA1) &
                                             +     p%AxRedTFA(2,2,p%TTopNode)*x%QT(DOF_TFA2)*x%QT(DOF_TFA2) &
                                             + 2.0*p%AxRedTFA(1,2,p%TTopNode)*x%QT(DOF_TFA1)*x%QT(DOF_TFA2) &
                                             +     p%AxRedTSS(1,1,p%TTopNode)*x%QT(DOF_TSS1)*x%QT(DOF_TSS1) &
                                             +     p%AxRedTSS(2,2,p%TTopNode)*x%QT(DOF_TSS2)*x%QT(DOF_TSS2) &
                                             + 2.0*p%AxRedTSS(1,2,p%TTopNode)*x%QT(DOF_TSS1)*x%QT(DOF_TSS2)   ) )*OtherState%CoordSys%a2 &
                       + ( x%QT(DOF_TSS1) + x%QT(DOF_TSS2)                                                 )*OtherState%CoordSys%a3
rOU   =    p%NacCMxn*OtherState%CoordSys%d1  +  p%NacCMzn*  OtherState%CoordSys%d2  -  p%NacCMyn  *OtherState%CoordSys%d3                          ! Position vector from tower-top / base plate (point O) to nacelle center of mass (point U).
rOV   =  p%RFrlPntxn*OtherState%CoordSys%d1  +  p%RFrlPntzn*OtherState%CoordSys%d2  -  p%RFrlPntyn*OtherState%CoordSys%d3                          ! Position vector from tower-top / base plate (point O) to specified point on rotor-furl axis (point V).
rVIMU =    p%rVIMUxn*OtherState%CoordSys%rf1 +  p%rVIMUzn  *OtherState%CoordSys%rf2 -   p%rVIMUyn *OtherState%CoordSys%rf3                         ! Position vector from specified point on rotor-furl axis (point V) to nacelle IMU (point IMU).
rVD   =      p%rVDxn*OtherState%CoordSys%rf1 +    p%rVDzn  *OtherState%CoordSys%rf2 -     p%rVDyn *OtherState%CoordSys%rf3                         ! Position vector from specified point on rotor-furl axis (point V) to center of mass of structure that furls with the rotor (not including rotor) (point D).
rVP   =      p%rVPxn*OtherState%CoordSys%rf1 +    p%rVPzn  *OtherState%CoordSys%rf2 -     p%rVPyn *OtherState%CoordSys%rf3 + p%OverHang*OtherState%CoordSys%c1  ! Position vector from specified point on rotor-furl axis (point V) to teeter pin (point P).
rPQ   =  -p%UndSling*OtherState%CoordSys%g1                                                                              ! Position vector from teeter pin (point P) to apex of rotation (point Q).
rQC   =      p%HubCM*OtherState%CoordSys%g1                                                                              ! Position vector from apex of rotation (point Q) to hub center of mass (point C).
rOW   =  p%TFrlPntxn*OtherState%CoordSys%d1  + p%TFrlPntzn* OtherState%CoordSys%d2 -  p%TFrlPntyn*OtherState%CoordSys%d3                          ! Position vector from tower-top / base plate (point O) to specified point on  tail-furl axis (point W).
rWI   =    p%rWIxn*OtherState%CoordSys%tf1 +       p%rWIzn* OtherState%CoordSys%tf2 -     p%rWIyn*OtherState%CoordSys%tf3                         ! Position vector from specified point on  tail-furl axis (point W) to tail boom center of mass     (point I).
rWJ   =    p%rWJxn*OtherState%CoordSys%tf1 +       p%rWJzn* OtherState%CoordSys%tf2 -     p%rWJyn*OtherState%CoordSys%tf3                         ! Position vector from specified point on  tail-furl axis (point W) to tail fin  center of mass     (point J).
rWK   =    p%rWKxn*OtherState%CoordSys%tf1 +       p%rWKzn* OtherState%CoordSys%tf2 -     p%rWKyn*OtherState%CoordSys%tf3                         ! Position vector from specified point on  tail-furl axis (point W) to tail fin  center of pressure (point K).
rPC   = rPQ + rQC                                                                                           ! Position vector from teeter pin (point P) to hub center of mass (point C).
OtherState%RtHS%rT0O  = OtherState%RtHS%rZO - rZT0                                                                                          ! Position vector from the tower base (point T(0)) to tower-top / base plate (point O).
OtherState%RtHS%rO    = OtherState%RtHS%rZ  + OtherState%RtHS%rZO                                                                                           ! Position vector from inertial frame origin to tower-top / base plate (point O).
rV    = OtherState%RtHS%rO  + rOV                                                                                           ! Position vector from inertial frame origin to specified point on rotor-furl axis (point V)
!rP    = OtherState%RtHS%rO  + rOV + rVP                                                                                    ! Position vector from inertial frame origin to teeter pin (point P).
rP    = rV  + rVP                                                                                           ! Position vector from inertial frame origin to teeter pin (point P).
rQ    = rP  + rPQ                                                                                           ! Position vector from inertial frame origin to apex of rotation (point Q).
rK    = OtherState%RtHS%rO  + rOW + rWK                                                                                     ! Position vector from inertial frame origin to tail fin center of pressure (point K).


DO K = 1,p%NumBl ! Loop through all blades



   ! Calculate the position vector of the tip:

   OtherState%RtHS%rS0S(K,p%TipNode,:) = (   p%TwistedSF(K,1,1,p%TipNode,0)*x%QT( DOF_BF(K,1) ) &  ! Position vector from the blade root (point S(0)) to the blade tip (point S(p%BldFlexL)).
                         + p%TwistedSF(K,1,2,p%TipNode,0)*x%QT( DOF_BF(K,2) ) &
                         + p%TwistedSF(K,1,3,p%TipNode,0)*x%QT( DOF_BE(K,1) )                          )*OtherState%CoordSys%j1(K,:) &
                     + (   p%TwistedSF(K,2,1,p%TipNode,0)*x%QT( DOF_BF(K,1) ) &
                         + p%TwistedSF(K,2,2,p%TipNode,0)*x%QT( DOF_BF(K,2) ) &
                         + p%TwistedSF(K,2,3,p%TipNode,0)*x%QT( DOF_BE(K,1) )                          )*OtherState%CoordSys%j2(K,:) &
                     + ( p%BldFlexL - 0.5* &
                         (       p%AxRedBld(K,1,1,p%TipNode)*x%QT( DOF_BF(K,1) )*x%QT( DOF_BF(K,1) ) &
                           +     p%AxRedBld(K,2,2,p%TipNode)*x%QT( DOF_BF(K,2) )*x%QT( DOF_BF(K,2) ) &
                           +     p%AxRedBld(K,3,3,p%TipNode)*x%QT( DOF_BE(K,1) )*x%QT( DOF_BE(K,1) ) &
                           + 2.0*p%AxRedBld(K,1,2,p%TipNode)*x%QT( DOF_BF(K,1) )*x%QT( DOF_BF(K,2) ) &
                           + 2.0*p%AxRedBld(K,2,3,p%TipNode)*x%QT( DOF_BF(K,2) )*x%QT( DOF_BE(K,1) ) &
                           + 2.0*p%AxRedBld(K,1,3,p%TipNode)*x%QT( DOF_BF(K,1) )*x%QT( DOF_BE(K,1) )   ) )*OtherState%CoordSys%j3(K,:)
   rQS (K,p%TipNode,:) = OtherState%RtHS%rS0S(K,p%TipNode,:) + p%HubRad*OtherState%CoordSys%j3(K,:)                  ! Position vector from apex of rotation (point Q) to the blade tip (point S(p%BldFlexL)).
   OtherState%RtHS%rS  (K,p%TipNode,:) = rQS (K,p%TipNode,:) + rQ                              ! Position vector from inertial frame origin      to the blade tip (point S(p%BldFlexL)).


   DO J = 1,p%BldNodes ! Loop through the blade nodes / elements


   ! Calculate the position vector of the current node:

      OtherState%RtHS%rS0S(K,J,:) = (   p%TwistedSF(K,1,1,J,0)*x%QT( DOF_BF(K,1) ) &  ! Position vector from the blade root (point S(0)) to the current node (point S(RNodes(J)).
                      + p%TwistedSF(K,1,2,J,0)*x%QT( DOF_BF(K,2) ) &
                      + p%TwistedSF(K,1,3,J,0)*x%QT( DOF_BE(K,1) )                          )*OtherState%CoordSys%j1(K,:) &
                  + (   p%TwistedSF(K,2,1,J,0)*x%QT( DOF_BF(K,1) ) &
                      + p%TwistedSF(K,2,2,J,0)*x%QT( DOF_BF(K,2) ) &
                      + p%TwistedSF(K,2,3,J,0)*x%QT( DOF_BE(K,1) )                          )*OtherState%CoordSys%j2(K,:) &
                  + ( p%RNodes(J) - 0.5* &
                      (       p%AxRedBld(K,1,1,J)*x%QT( DOF_BF(K,1) )*x%QT( DOF_BF(K,1) ) &
                        +     p%AxRedBld(K,2,2,J)*x%QT( DOF_BF(K,2) )*x%QT( DOF_BF(K,2) ) &
                        +     p%AxRedBld(K,3,3,J)*x%QT( DOF_BE(K,1) )*x%QT( DOF_BE(K,1) ) &
                        + 2.0*p%AxRedBld(K,1,2,J)*x%QT( DOF_BF(K,1) )*x%QT( DOF_BF(K,2) ) &
                        + 2.0*p%AxRedBld(K,2,3,J)*x%QT( DOF_BF(K,2) )*x%QT( DOF_BE(K,1) ) &
                        + 2.0*p%AxRedBld(K,1,3,J)*x%QT( DOF_BF(K,1) )*x%QT( DOF_BE(K,1) )   ) )*OtherState%CoordSys%j3(K,:)
      rQS (K,J,:) = OtherState%RtHS%rS0S(K,J,:) + p%HubRad*OtherState%CoordSys%j3(K,:)                  ! Position vector from apex of rotation (point Q) to the current node (point S(RNodes(J)).
      OtherState%RtHS%rS  (K,J,:) = rQS (K,J,:) + rQ                              ! Position vector from inertial frame origin      to the current node (point S(RNodes(J)).

      IF ( p_FAST%CompAero )  THEN   ! Calculate the blade element aerodynamic loads using AeroDyn.


   ! Calculate the aerodynamic pitching moment arm (i.e., the position vector
   !   from point S on the blade to the aerodynamic center of the element):

         rSAerCen = p%rSAerCenn1(K,J)*OtherState%CoordSys%n1(K,J,:) + p%rSAerCenn2(K,J)*OtherState%CoordSys%n2(K,J,:) !bjj: make rSAerCen a matrix? we recalculate it later


   ! Define positions USEd by AeroDyn.

         rPAerCen     = rPQ + rQS(K,J,:) + rSAerCen         ! Position vector from teeter pin (point P)  to blade analysis node aerodynamic center.
         rAerCen      =       OtherState%RtHS%rS (K,J,:) + rSAerCen         ! Position vector from inertial frame origin to blade analysis node aerodynamic center.

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

ADInterfaceComponents%Nacelle%Position(:)   = (/ OtherState%RtHS%rO(1), -1.*OtherState%RtHS%rO(3), OtherState%RtHS%rO(2) - p%PtfmRef /)

   ! Tower base position should be rT(0) instead of rZ, but AeroDyn needs this for
   ! the HubVDue2Yaw calculation:
ADInterfaceComponents%Tower%Position(:)     = (/ OtherState%RtHS%rZ(1), -1.*OtherState%RtHS%rZ(3), OtherState%RtHS%rZ(2) - p%PtfmRef /)


!y%HubPosition       = (/ rP(1),                 -1.*rP(3),                 rP(2)                 - p%PtfmRef /)
!y%RotorFurlPosition = (/ rV(1),                 -1.*rV(3),                 rV(2)                 - p%PtfmRef /)
!y%NacellePosition   = (/ OtherState%RtHS%rO(1), -1.*OtherState%RtHS%rO(3), OtherState%RtHS%rO(2) - p%PtfmRef /)
!y%TowerPosition     = (/ OtherState%RtHS%rZ(1), -1.*OtherState%RtHS%rZ(3), OtherState%RtHS%rZ(2) - p%PtfmRef /)

   !-------------------------------------------------------------------------------------------------
   ! Orientations - bjj: should this be moved to SetCoordSys ?
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

ADInterfaceComponents%Hub%Orientation(:,1)       =     (/ OtherState%CoordSys%e1(1), OtherState%CoordSys%e2(1), OtherState%CoordSys%e3(1) /)
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
   ! Angular and partial angular velocities
   !-------------------------------------------------------------------------------------------------

   ! Define the angular and partial angular velocities of all of the rigid
   !   bodies in the inertia frame:
   ! NOTE: PAngVelEN(I,D,:) = the Dth-derivative of the partial angular velocity
   !   of DOF I for body N in body E.

OtherState%RtHS%PAngVelEX(       :,0,:) = 0.0
OtherState%RtHS%PAngVelEX(DOF_R   ,0,:) =  OtherState%CoordSys%z1
OtherState%RtHS%PAngVelEX(DOF_P   ,0,:) = -OtherState%CoordSys%z3
OtherState%RtHS%PAngVelEX(DOF_Y   ,0,:) =  OtherState%CoordSys%z2
 OtherState%RtHS%AngVelEX               =                             x%QDT(DOF_R   )*OtherState%RtHS%PAngVelEX(DOF_R   ,0,:) &
                                                                    + x%QDT(DOF_P   )*OtherState%RtHS%PAngVelEX(DOF_P   ,0,:) &
                                                                    + x%QDT(DOF_Y   )*OtherState%RtHS%PAngVelEX(DOF_Y   ,0,:)
                 AngPosEX               =                             x%QT (DOF_R   )*OtherState%RtHS%PAngVelEX(DOF_R   ,0,:) &
                                                                    + x%QT (DOF_P   )*OtherState%RtHS%PAngVelEX(DOF_P   ,0,:) &
                                                                    + x%QT (DOF_Y   )*OtherState%RtHS%PAngVelEX(DOF_Y   ,0,:)

OtherState%RtHS%PAngVelEB(       :,0,:) = OtherState%RtHS%PAngVelEX(:,0,:)
OtherState%RtHS%PAngVelEB(DOF_TFA1,0,:) = -p%TwrFASF(1,p%TTopNode,1)*OtherState%CoordSys%a3
OtherState%RtHS%PAngVelEB(DOF_TSS1,0,:) =  p%TwrSSSF(1,p%TTopNode,1)*OtherState%CoordSys%a1
OtherState%RtHS%PAngVelEB(DOF_TFA2,0,:) = -p%TwrFASF(2,p%TTopNode,1)*OtherState%CoordSys%a3
OtherState%RtHS%PAngVelEB(DOF_TSS2,0,:) =  p%TwrSSSF(2,p%TTopNode,1)*OtherState%CoordSys%a1
 OtherState%RtHS%AngVelEB               =  OtherState%RtHS%AngVelEX + x%QDT(DOF_TFA1)*OtherState%RtHS%PAngVelEB(DOF_TFA1,0,:) &
                                                                    + x%QDT(DOF_TSS1)*OtherState%RtHS%PAngVelEB(DOF_TSS1,0,:) &
                                                                    + x%QDT(DOF_TFA2)*OtherState%RtHS%PAngVelEB(DOF_TFA2,0,:) &
                                                                    + x%QDT(DOF_TSS2)*OtherState%RtHS%PAngVelEB(DOF_TSS2,0,:)
 OtherState%RtHS%AngPosXB               =                             x%QT (DOF_TFA1)*OtherState%RtHS%PAngVelEB(DOF_TFA1,0,:) &
                                                                    + x%QT (DOF_TSS1)*OtherState%RtHS%PAngVelEB(DOF_TSS1,0,:) &
                                                                    + x%QT (DOF_TFA2)*OtherState%RtHS%PAngVelEB(DOF_TFA2,0,:) &
                                                                    + x%QT (DOF_TSS2)*OtherState%RtHS%PAngVelEB(DOF_TSS2,0,:)

                PAngVelEN(       :,0,:) = OtherState%RtHS%PAngVelEB(:,0,:)
                PAngVelEN(DOF_Yaw ,0,:) =  OtherState%CoordSys%d2
                 AngVelEN               =  OtherState%RtHS%AngVelEB + x%QDT(DOF_Yaw )*PAngVelEN(DOF_Yaw ,0,:)

OtherState%RtHS%PAngVelER(       :,0,:) = PAngVelEN(:,0,:)
OtherState%RtHS%PAngVelER(DOF_RFrl,0,:) = OtherState%CoordSys%rfa
 OtherState%RtHS%AngVelER               =  AngVelEN + x%QDT(DOF_RFrl)*OtherState%RtHS%PAngVelER(DOF_RFrl,0,:)

                PAngVelEL(       :,0,:) = OtherState%RtHS%PAngVelER(:,0,:)
                PAngVelEL(DOF_GeAz,0,:) =  OtherState%CoordSys%c1
                PAngVelEL(DOF_DrTr,0,:) =  OtherState%CoordSys%c1
                 AngVelEL               =  OtherState%RtHS%AngVelER + x%QDT(DOF_GeAz)*PAngVelEL(DOF_GeAz,0,:) &
                                                                    + x%QDT(DOF_DrTr)*PAngVelEL(DOF_DrTr,0,:)

                PAngVelEH(       :,0,:) = PAngVelEL(:,0,:)
                 AngVelEH               =  AngVelEL
IF ( p%NumBl == 2 )  THEN ! 2-blader
                PAngVelEH(DOF_Teet,0,:) = OtherState%CoordSys%f2
                    AngVelEH            =  AngVelEH + x%QDT(DOF_Teet)*PAngVelEH(DOF_Teet,0,:)
ENDIF

                PAngVelEG(       :,0,:) = OtherState%RtHS%PAngVelER(:,0,:)
                PAngVelEG(DOF_GeAz,0,:) = p%GBRatio*OtherState%CoordSys%c1
                 AngVelEG               =  OtherState%RtHS%AngVelER + x%QDT(DOF_GeAz)*PAngVelEG(DOF_GeAz,0,:)

                PAngVelEA(       :,0,:) = PAngVelEN(:,0,:)
                PAngVelEA(DOF_TFrl,0,:) = OtherState%CoordSys%tfa
                 AngVelEA               =  AngVelEN + x%QDT(DOF_TFrl)*PAngVelEA(DOF_TFrl,0,:)


   ! Note the hub rotational velocity should be AngVelEH instead AngVelEL, but AeroDyn (13.00.00)
   ! treats teeter deflections like blade deflections:

ADInterfaceComponents%Hub%RotationVel(:)       = (/ AngVelEL(1), -1.*AngVelEL(3), AngVelEL(2) /)
ADInterfaceComponents%RotorFurl%RotationVel(:) = (/ OtherState%RtHS%AngVelER(1), -1.*OtherState%RtHS%AngVelER(3), OtherState%RtHS%AngVelER(2) /)
ADInterfaceComponents%Nacelle%RotationVel(:)   = (/ AngVelEN(1), -1.*AngVelEN(3), AngVelEN(2) /)
ADInterfaceComponents%Tower%RotationVel(:)     = (/ OtherState%RtHS%AngVelEX(1), -1.*OtherState%RtHS%AngVelEX(3), OtherState%RtHS%AngVelEX(2) /)

   ! Define the 1st derivatives of the partial angular velocities of all
   !   of the rigid bodies in the inertia frame and the portion of the angular
   !   acceleration of the rigid bodies in the inertia frame associated with
   !   everything but the QD2T()'s:

OtherState%RtHS%PAngVelEX(       :,1,:) = 0.0
OtherState%RtHS%AngAccEXt               = 0.0

OtherState%RtHS%PAngVelEB(       :,1,:) =                 OtherState%RtHS%PAngVelEX(:,1,:)
OtherState%RtHS%PAngVelEB(DOF_TFA1,1,:) = CROSS_PRODUCT(  OtherState%RtHS%AngVelEX,                   OtherState%RtHS%PAngVelEB(DOF_TFA1,0,:) )
OtherState%RtHS%PAngVelEB(DOF_TSS1,1,:) = CROSS_PRODUCT(  OtherState%RtHS%AngVelEX,                   OtherState%RtHS%PAngVelEB(DOF_TSS1,0,:) )
OtherState%RtHS%PAngVelEB(DOF_TFA2,1,:) = CROSS_PRODUCT(  OtherState%RtHS%AngVelEX,                   OtherState%RtHS%PAngVelEB(DOF_TFA2,0,:) )
OtherState%RtHS%PAngVelEB(DOF_TSS2,1,:) = CROSS_PRODUCT(  OtherState%RtHS%AngVelEX,                   OtherState%RtHS%PAngVelEB(DOF_TSS2,0,:) )
OtherState%RtHS%AngAccEBt               =                 OtherState%RtHS%AngAccEXt + x%QDT(DOF_TFA1)*OtherState%RtHS%PAngVelEB(DOF_TFA1,1,:) &
                                                                                    + x%QDT(DOF_TSS1)*OtherState%RtHS%PAngVelEB(DOF_TSS1,1,:) &
                                                                                    + x%QDT(DOF_TFA2)*OtherState%RtHS%PAngVelEB(DOF_TFA2,1,:) &
                                                                                    + x%QDT(DOF_TSS2)*OtherState%RtHS%PAngVelEB(DOF_TSS2,1,:)

                PAngVelEN(       :,1,:) =                 OtherState%RtHS%PAngVelEB(:,1,:)
                PAngVelEN(DOF_Yaw ,1,:) = CROSS_PRODUCT(  OtherState%RtHS%AngVelEB,                   PAngVelEN(DOF_Yaw ,0,:) )
                AngAccENt               =                 OtherState%RtHS%AngAccEBt + x%QDT(DOF_Yaw )*PAngVelEN(DOF_Yaw ,1,:)

OtherState%RtHS%PAngVelER(       :,1,:) =                 PAngVelEN(:,1,:)
OtherState%RtHS%PAngVelER(DOF_RFrl,1,:) = CROSS_PRODUCT(  AngVelEN,                   OtherState%RtHS%PAngVelER(DOF_RFrl,0,:) )
OtherState%RtHS%AngAccERt               =                 AngAccENt + x%QDT(DOF_RFrl)*OtherState%RtHS%PAngVelER(DOF_RFrl,1,:)

                PAngVelEL(       :,1,:) =                 OtherState%RtHS%PAngVelER(:,1,:)
                PAngVelEL(DOF_GeAz,1,:) = CROSS_PRODUCT(   OtherState%RtHS%AngVelER,                   PAngVelEL(DOF_GeAz,0,:) )
                PAngVelEL(DOF_DrTr,1,:) = CROSS_PRODUCT(   OtherState%RtHS%AngVelER,                   PAngVelEL(DOF_DrTr,0,:) )
                AngAccELt               =                  OtherState%RtHS%AngAccERt + x%QDT(DOF_GeAz)*PAngVelEL(DOF_GeAz,1,:) &
                                                                                     + x%QDT(DOF_DrTr)*PAngVelEL(DOF_DrTr,1,:)

                PAngVelEH(       :,1,:) = PAngVelEL(:,1,:)
                AngAccEHt               =                  AngAccELt
IF ( p%NumBl == 2 )  THEN ! 2-blader
                PAngVelEH(DOF_Teet,1,:) = CROSS_PRODUCT(AngVelEH,                   PAngVelEH(DOF_Teet,0,:) )
                 AngAccEHt              =               AngAccEHt + x%QDT(DOF_Teet)*PAngVelEH(DOF_Teet,1,:)
ENDIF

                PAngVelEG(       :,1,:) = OtherState%RtHS%PAngVelER(:,1,:)
                PAngVelEG(DOF_GeAz,1,:) = CROSS_PRODUCT(   OtherState%RtHS%AngVelER,                   PAngVelEG(DOF_GeAz,0,:) )
                 AngAccEGt              =                   OtherState%RtHS%AngAccERt + x%QDT(DOF_GeAz)*PAngVelEG(DOF_GeAz,1,:)

                PAngVelEA(       :,1,:) = PAngVelEN(:,1,:)
                PAngVelEA(DOF_TFrl,1,:) = CROSS_PRODUCT(   AngVelEN,                   PAngVelEA(DOF_TFrl,0,:) )
                AngAccEAt               =                  AngAccENt + x%QDT(DOF_TFrl)*PAngVelEA(DOF_TFrl,1,:)



DO K = 1,p%NumBl ! Loop through all blades

   ! Define the partial angular velocities of the tip (body M(p%BldFlexL)) in the  inertia frame:
   ! NOTE: PAngVelEM(K,J,I,D,:) = the Dth-derivative of the partial angular velocity of DOF I for body M of blade K, element J in body E.

   PAngVelEM(K,p%TipNode,          :,0,:) = PAngVelEH(:,0,:)
   PAngVelEM(K,p%TipNode,DOF_BF(K,1),0,:) = - p%TwistedSF(K,2,1,p%TipNode,1)*OtherState%CoordSys%j1(K,:) &
                                            + p%TwistedSF(K,1,1,p%TipNode,1)*OtherState%CoordSys%j2(K,:)
   PAngVelEM(K,p%TipNode,DOF_BF(K,2),0,:) = - p%TwistedSF(K,2,2,p%TipNode,1)*OtherState%CoordSys%j1(K,:) &
                                            + p%TwistedSF(K,1,2,p%TipNode,1)*OtherState%CoordSys%j2(K,:)
   PAngVelEM(K,p%TipNode,DOF_BE(K,1),0,:) = - p%TwistedSF(K,2,3,p%TipNode,1)*OtherState%CoordSys%j1(K,:) &
                                            + p%TwistedSF(K,1,3,p%TipNode,1)*OtherState%CoordSys%j2(K,:)
!    AngVelHM(K,p%TipNode              ,:) =  AngVelEH + x%QDT(DOF_BF(K,1))*PAngVelEM(K,p%TipNode,DOF_BF(K,1),0,:) & ! Currently
!                                                    + x%QDT(DOF_BF(K,2))*PAngVelEM(K,p%TipNode,DOF_BF(K,2),0,:) & ! unused
!                                                    + x%QDT(DOF_BE(K,1))*PAngVelEM(K,p%TipNode,DOF_BE(K,1),0,:)   ! calculations
    OtherState%RtHS%AngPosHM(K,p%TipNode,:) =        x%QT (DOF_BF(K,1))*PAngVelEM(K,p%TipNode,DOF_BF(K,1),0,:) &
                                                   + x%QT (DOF_BF(K,2))*PAngVelEM(K,p%TipNode,DOF_BF(K,2),0,:) &
                                                   + x%QT (DOF_BE(K,1))*PAngVelEM(K,p%TipNode,DOF_BE(K,1),0,:)


   ! Define the 1st derivatives of the partial angular velocities of the tip
   !   (body M(p%BldFlexL)) in the inertia frame:

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
      PAngVelEM(K,J,DOF_BF(K,1),0,:) = - p%TwistedSF(K,2,1,J,1)*OtherState%CoordSys%j1(K,:) &
                                       + p%TwistedSF(K,1,1,J,1)*OtherState%CoordSys%j2(K,:)
      PAngVelEM(K,J,DOF_BF(K,2),0,:) = - p%TwistedSF(K,2,2,J,1)*OtherState%CoordSys%j1(K,:) &
                                       + p%TwistedSF(K,1,2,J,1)*OtherState%CoordSys%j2(K,:)
      PAngVelEM(K,J,DOF_BE(K,1),0,:) = - p%TwistedSF(K,2,3,J,1)*OtherState%CoordSys%j1(K,:) &
                                       + p%TwistedSF(K,1,3,J,1)*OtherState%CoordSys%j2(K,:)
!       AngVelHM(K,J              ,:) =  AngVelEH + x%QDT(DOF_BF(K,1))*PAngVelEM(K,J,DOF_BF(K,1),0,:) & ! Currently
!                                                 + x%QDT(DOF_BF(K,2))*PAngVelEM(K,J,DOF_BF(K,2),0,:) & ! unused
!                                                 + x%QDT(DOF_BE(K,1))*PAngVelEM(K,J,DOF_BE(K,1),0,:)   ! calculations
       OtherState%RtHS%AngPosHM(K,J              ,:) =             x%QT (DOF_BF(K,1))*PAngVelEM(K,J,DOF_BF(K,1),0,:) &
                                                 + x%QT (DOF_BF(K,2))*PAngVelEM(K,J,DOF_BF(K,2),0,:) &
                                                 + x%QT (DOF_BE(K,1))*PAngVelEM(K,J,DOF_BE(K,1),0,:)


   ! Define the 1st derivatives of the partial angular velocities of the current node (body M(RNodes(J))) in the inertia frame:

! NOTE: These are currently unused by the code, therefore, they need not
!       be calculated.  Thus, they are currently commented out.  If it
!       turns out that they are ever needed (i.e., if inertias of the
!       blade elements are ever added, etc...) simply uncomment out these computations:
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

EwXXrZY   = CROSS_PRODUCT( OtherState%RtHS%AngVelEX,                 rZY   ) !
EwXXrZO   = CROSS_PRODUCT( OtherState%RtHS%AngVelEX, OtherState%RtHS%rZO   ) !
EwNXrOU   = CROSS_PRODUCT(                 AngVelEN,                 rOU   ) !
EwNXrOV   = CROSS_PRODUCT(                 AngVelEN,                 rOV   ) !
EwRXrVD   = CROSS_PRODUCT( OtherState%RtHS%AngVelER,                 rVD   ) ! Cross products
EwRXrVIMU = CROSS_PRODUCT( OtherState%RtHS%AngVelER,                 rVIMU ) ! that are used
EwRXrVP   = CROSS_PRODUCT( OtherState%RtHS%AngVelER,                 rVP   ) ! in the following
EwHXrPQ   = CROSS_PRODUCT(                 AngVelEH,                 rPQ   ) ! DO...LOOPs
EwHXrQC   = CROSS_PRODUCT(                 AngVelEH,                 rQC   ) !
EwNXrOW   = CROSS_PRODUCT(                 AngVelEN,                 rOW   ) !
EwAXrWI   = CROSS_PRODUCT(                 AngVelEA,                 rWI   ) !
EwAXrWJ   = CROSS_PRODUCT(                 AngVelEA,                 rWJ   ) !
EwAXrWK   = CROSS_PRODUCT(                 AngVelEA,                 rWK   ) !


OtherState%RtHS%PLinVelEZ(       :,:,:) = 0.0
OtherState%RtHS%PLinVelEZ(DOF_Sg  ,0,:) =  OtherState%CoordSys%z1
OtherState%RtHS%PLinVelEZ(DOF_Sw  ,0,:) = -OtherState%CoordSys%z3
OtherState%RtHS%PLinVelEZ(DOF_Hv  ,0,:) =  OtherState%CoordSys%z2

 OtherState%RtHS%LinVelEZ               =   x%QDT(DOF_Sg  )*OtherState%RtHS%PLinVelEZ(DOF_Sg  ,0,:) &
                                          + x%QDT(DOF_Sw  )*OtherState%RtHS%PLinVelEZ(DOF_Sw  ,0,:) &
                                          + x%QDT(DOF_Hv  )*OtherState%RtHS%PLinVelEZ(DOF_Hv  ,0,:)


                PLinVelEY(       :,:,:) = OtherState%RtHS%PLinVelEZ(:,:,:)
DO I = 1,NPX   ! Loop through all DOFs associated with the angular motion of the platform (body X)

   TmpVec0              = CROSS_PRODUCT(            OtherState%RtHS%PAngVelEX(PX(I)   ,0,:),     rZY  )
   TmpVec1              = CROSS_PRODUCT(            OtherState%RtHS%PAngVelEX(PX(I)   ,0,:), EwXXrZY  )

   PLinVelEY(PX(I),0,:) = TmpVec0   +               PLinVelEY(PX(I)   ,0,:)
   PLinVelEY(PX(I),1,:) = TmpVec1   +               PLinVelEY(PX(I)   ,1,:)

    LinAccEYt           = LinAccEYt + x%QDT(PX(I) )*PLinVelEY(PX(I)   ,1,:)

ENDDO          ! I - all DOFs associated with the angular motion of the platform (body X)


OtherState%RtHS%PLinVelEO(       :,:,:) = OtherState%RtHS%PLinVelEZ(:,:,:)
OtherState%RtHS%PLinVelEO(DOF_TFA1,0,:) = OtherState%CoordSys%a1      - (   p%AxRedTFA(1,1,p%TTopNode)* x%QT(DOF_TFA1) &
                                               + p%AxRedTFA(1,2,p%TTopNode)* x%QT(DOF_TFA2)   )*OtherState%CoordSys%a2
OtherState%RtHS%PLinVelEO(DOF_TSS1,0,:) = OtherState%CoordSys%a3      - (   p%AxRedTSS(1,1,p%TTopNode)* x%QT(DOF_TSS1) &
                                               + p%AxRedTSS(1,2,p%TTopNode)* x%QT(DOF_TSS2)   )*OtherState%CoordSys%a2
OtherState%RtHS%PLinVelEO(DOF_TFA2,0,:) = OtherState%CoordSys%a1      - (   p%AxRedTFA(2,2,p%TTopNode)* x%QT(DOF_TFA2) &
                                               + p%AxRedTFA(1,2,p%TTopNode)* x%QT(DOF_TFA1)   )*OtherState%CoordSys%a2
OtherState%RtHS%PLinVelEO(DOF_TSS2,0,:) = OtherState%CoordSys%a3      - (   p%AxRedTSS(2,2,p%TTopNode)* x%QT(DOF_TSS2) &
                                               + p%AxRedTSS(1,2,p%TTopNode)* x%QT(DOF_TSS1)   )*OtherState%CoordSys%a2

TmpVec1 = CROSS_PRODUCT(   OtherState%RtHS%AngVelEX   , OtherState%RtHS%PLinVelEO(DOF_TFA1,0,:) )
TmpVec2 = CROSS_PRODUCT(   OtherState%RtHS%AngVelEX   , OtherState%RtHS%PLinVelEO(DOF_TSS1,0,:) )
TmpVec3 = CROSS_PRODUCT(   OtherState%RtHS%AngVelEX   , OtherState%RtHS%PLinVelEO(DOF_TFA2,0,:) )
TmpVec4 = CROSS_PRODUCT(   OtherState%RtHS%AngVelEX   , OtherState%RtHS%PLinVelEO(DOF_TSS2,0,:) )

OtherState%RtHS%PLinVelEO(DOF_TFA1,1,:) = TmpVec1 - (   p%AxRedTFA(1,1,p%TTopNode)*x%QDT(DOF_TFA1) &
                                      + p%AxRedTFA(1,2,p%TTopNode)*x%QDT(DOF_TFA2)   )*OtherState%CoordSys%a2
OtherState%RtHS%PLinVelEO(DOF_TSS1,1,:) = TmpVec2 - (   p%AxRedTSS(1,1,p%TTopNode)*x%QDT(DOF_TSS1) &
                                      + p%AxRedTSS(1,2,p%TTopNode)*x%QDT(DOF_TSS2)   )*OtherState%CoordSys%a2
OtherState%RtHS%PLinVelEO(DOF_TFA2,1,:) = TmpVec3 - (   p%AxRedTFA(2,2,p%TTopNode)*x%QDT(DOF_TFA2) &
                                      + p%AxRedTFA(1,2,p%TTopNode)*x%QDT(DOF_TFA1)   )*OtherState%CoordSys%a2
OtherState%RtHS%PLinVelEO(DOF_TSS2,1,:) = TmpVec4 - (   p%AxRedTSS(2,2,p%TTopNode)*x%QDT(DOF_TSS2) &
                                      + p%AxRedTSS(1,2,p%TTopNode)*x%QDT(DOF_TSS1)   )*OtherState%CoordSys%a2

 LinVelXO               =              x%QDT(DOF_TFA1)*OtherState%RtHS%PLinVelEO(DOF_TFA1,0,:) &
                                     + x%QDT(DOF_TSS1)*OtherState%RtHS%PLinVelEO(DOF_TSS1,0,:) &
                                     + x%QDT(DOF_TFA2)*OtherState%RtHS%PLinVelEO(DOF_TFA2,0,:) &
                                     + x%QDT(DOF_TSS2)*OtherState%RtHS%PLinVelEO(DOF_TSS2,0,:)
 OtherState%RtHS%LinAccEOt              =              x%QDT(DOF_TFA1)*OtherState%RtHS%PLinVelEO(DOF_TFA1,1,:) &
                                     + x%QDT(DOF_TSS1)*OtherState%RtHS%PLinVelEO(DOF_TSS1,1,:) &
                                     + x%QDT(DOF_TFA2)*OtherState%RtHS%PLinVelEO(DOF_TFA2,1,:) &
                                     + x%QDT(DOF_TSS2)*OtherState%RtHS%PLinVelEO(DOF_TSS2,1,:)

DO I = 1,NPX   ! Loop through all DOFs associated with the angular motion of the platform (body X)

   TmpVec0              = CROSS_PRODUCT(             OtherState%RtHS%PAngVelEX(PX(I)   ,0,:),     OtherState%RtHS%rZO                 )
   TmpVec1              = CROSS_PRODUCT(             OtherState%RtHS%PAngVelEX(PX(I)   ,0,:), EwXXrZO + LinVelXO      )

   OtherState%RtHS%PLinVelEO(PX(I),0,:) = TmpVec0    +               OtherState%RtHS%PLinVelEO(PX(I)   ,0,:)
   OtherState%RtHS%PLinVelEO(PX(I),1,:) = TmpVec1    +               OtherState%RtHS%PLinVelEO(PX(I)   ,1,:)

    OtherState%RtHS%LinAccEOt           =  OtherState%RtHS%LinAccEOt + x%QDT(PX(I) )*OtherState%RtHS%PLinVelEO(PX(I)   ,1,:)

ENDDO          ! I - all DOFs associated with the angular motion of the platform (body X)


PLinVelEU(       :,:,:) = OtherState%RtHS%PLinVelEO(:,:,:)
DO I = 1,NPN   ! Loop through all DOFs associated with the angular motion of the nacelle (body N)

   TmpVec0              = CROSS_PRODUCT(             PAngVelEN(PN(I)   ,0,:),     rOU                 )
   TmpVec1              = CROSS_PRODUCT(             PAngVelEN(PN(I)   ,0,:), EwNXrOU                 )
   TmpVec2              = CROSS_PRODUCT(             PAngVelEN(PN(I)   ,1,:),     rOU                 )

   PLinVelEU(PN(I),0,:) = TmpVec0    +               PLinVelEU(PN(I)   ,0,:)
   PLinVelEU(PN(I),1,:) = TmpVec1    + TmpVec2 +     PLinVelEU(PN(I)   ,1,:)

    LinAccEUt           =  LinAccEUt + x%QDT(PN(I) )*PLinVelEU(PN(I)   ,1,:)

ENDDO          ! I - all DOFs associated with the angular motion of the nacelle (body N)


PLinVelEV(       :,:,:) = OtherState%RtHS%PLinVelEO(:,:,:)
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

   TmpVec0              = CROSS_PRODUCT(             OtherState%RtHS%PAngVelER(PR(I)   ,0,:),     rVD                 )
   TmpVec1              = CROSS_PRODUCT(             OtherState%RtHS%PAngVelER(PR(I)   ,0,:), EwRXrVD                 )
   TmpVec2              = CROSS_PRODUCT(             OtherState%RtHS%PAngVelER(PR(I)   ,1,:),     rVD                 )

   PLinVelED(PR(I),0,:) = TmpVec0    +               PLinVelED(PR(I)   ,0,:)
   PLinVelED(PR(I),1,:) = TmpVec1    + TmpVec2 +     PLinVelED(PR(I)   ,1,:)

    LinAccEDt           =  LinAccEDt + x%QDT(PR(I) )*PLinVelED(PR(I)   ,1,:)

ENDDO          ! I - all DOFs associated with the angular motion of the structure that furls with the rotor (not including rotor) (body R)


OtherState%RtHS%PLinVelEIMU(     :,:,:) = PLinVelEV(:,:,:)
 OtherState%RtHS%LinVelEIMU             =  OtherState%RtHS%LinVelEZ
DO I = 1,NPR   ! Loop through all DOFs associated with the angular motion of the structure that furls with the rotor (not including rotor) (body R)

   TmpVec0                = CROSS_PRODUCT(              OtherState%RtHS%PAngVelER(PR(I)   ,0,:),     rVIMU               )
   TmpVec1                = CROSS_PRODUCT(              OtherState%RtHS%PAngVelER(PR(I)   ,0,:), EwRXrVIMU               )
   TmpVec2                = CROSS_PRODUCT(              OtherState%RtHS%PAngVelER(PR(I)   ,1,:),     rVIMU               )

   OtherState%RtHS%PLinVelEIMU(PR(I),0,:) = TmpVec0    +                OtherState%RtHS%PLinVelEIMU(PR(I) ,0,:)
   OtherState%RtHS%PLinVelEIMU(PR(I),1,:) = TmpVec1    + TmpVec2 +      OtherState%RtHS%PLinVelEIMU(PR(I) ,1,:)

    OtherState%RtHS%LinVelEIMU            =  OtherState%RtHS%LinVelEIMU  + x%QDT(PR(I) )*OtherState%RtHS%PLinVelEIMU(PR(I) ,0,:)
    OtherState%RtHS%LinAccEIMUt           =  OtherState%RtHS%LinAccEIMUt + x%QDT(PR(I) )*OtherState%RtHS%PLinVelEIMU(PR(I) ,1,:)

ENDDO          ! I - all DOFs associated with the angular motion of the structure that furls with the rotor (not including rotor) (body R)


PLinVelEP(       :,:,:) = PLinVelEV(:,:,:)
DO I = 1,NPR   ! Loop through all DOFs associated with the angular motion of the structure that furls with the rotor (not including rotor) (body R)

   TmpVec0              = CROSS_PRODUCT(             OtherState%RtHS%PAngVelER(PR(I)   ,0,:),     rVP                 )
   TmpVec1              = CROSS_PRODUCT(             OtherState%RtHS%PAngVelER(PR(I)   ,0,:), EwRXrVP                 )
   TmpVec2              = CROSS_PRODUCT(             OtherState%RtHS%PAngVelER(PR(I)   ,1,:),     rVP                 )

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
   !   blade tip (point S(p%BldFlexL)) in the inertia frame.  Also define the
   !   overall linear velocity of the blade tip in the inertia frame.  Also,
   !   define the portion of the linear acceleration of the blade tip in the
   !   inertia frame associated with everything but the QD2T()'s:

   EwHXrQS = CROSS_PRODUCT( AngVelEH, rQS(K,p%TipNode,:) )

   OtherState%RtHS%PLinVelES(K,p%TipNode,          :,:,:) = PLinVelEQ(:,:,:)
   OtherState%RtHS%PLinVelES(K,p%TipNode,DOF_BF(K,1),0,:) = p%TwistedSF(K,1,1,p%TipNode,0)                          *OtherState%CoordSys%j1(K,:) &
                                          + p%TwistedSF(K,2,1,p%TipNode,0)                          *OtherState%CoordSys%j2(K,:) &
                                          - (   p%AxRedBld(K,1,1,p%TipNode)*x%QT ( DOF_BF(K,1) ) &
                                              + p%AxRedBld(K,1,2,p%TipNode)*x%QT ( DOF_BF(K,2) ) &
                                              + p%AxRedBld(K,1,3,p%TipNode)*x%QT ( DOF_BE(K,1) )   )*OtherState%CoordSys%j3(K,:)
   OtherState%RtHS%PLinVelES(K,p%TipNode,DOF_BE(K,1),0,:) = p%TwistedSF(K,1,3,p%TipNode,0)                          *OtherState%CoordSys%j1(K,:) &
                                          + p%TwistedSF(K,2,3,p%TipNode,0)                          *OtherState%CoordSys%j2(K,:) &
                                          - (   p%AxRedBld(K,3,3,p%TipNode)*x%QT ( DOF_BE(K,1) ) &
                                              + p%AxRedBld(K,2,3,p%TipNode)*x%QT ( DOF_BF(K,2) ) &
                                              + p%AxRedBld(K,1,3,p%TipNode)*x%QT ( DOF_BF(K,1) )   )*OtherState%CoordSys%j3(K,:)
   OtherState%RtHS%PLinVelES(K,p%TipNode,DOF_BF(K,2),0,:) = p%TwistedSF(K,1,2,p%TipNode,0)                          *OtherState%CoordSys%j1(K,:) &
                                          + p%TwistedSF(K,2,2,p%TipNode,0)                          *OtherState%CoordSys%j2(K,:) &
                                          - (   p%AxRedBld(K,2,2,p%TipNode)*x%QT ( DOF_BF(K,2) ) &
                                              + p%AxRedBld(K,1,2,p%TipNode)*x%QT ( DOF_BF(K,1) ) &
                                              + p%AxRedBld(K,2,3,p%TipNode)*x%QT ( DOF_BE(K,1) )   )*OtherState%CoordSys%j3(K,:)

   TmpVec1 = CROSS_PRODUCT( AngVelEH, OtherState%RtHS%PLinVelES(K,p%TipNode,DOF_BF(K,1),0,:) )
   TmpVec2 = CROSS_PRODUCT( AngVelEH, OtherState%RtHS%PLinVelES(K,p%TipNode,DOF_BE(K,1),0,:) )
   TmpVec3 = CROSS_PRODUCT( AngVelEH, OtherState%RtHS%PLinVelES(K,p%TipNode,DOF_BF(K,2),0,:) )

   OtherState%RtHS%PLinVelES(K,p%TipNode,DOF_BF(K,1),1,:) = TmpVec1 &
                                        - (   p%AxRedBld(K,1,1,p%TipNode)*x%QDT( DOF_BF(K,1) ) &
                                            + p%AxRedBld(K,1,2,p%TipNode)*x%QDT( DOF_BF(K,2) ) &
                                            + p%AxRedBld(K,1,3,p%TipNode)*x%QDT( DOF_BE(K,1) )   )*OtherState%CoordSys%j3(K,:)
   OtherState%RtHS%PLinVelES(K,p%TipNode,DOF_BE(K,1),1,:) = TmpVec2 &
                                        - (   p%AxRedBld(K,3,3,p%TipNode)*x%QDT( DOF_BE(K,1) ) &
                                            + p%AxRedBld(K,2,3,p%TipNode)*x%QDT( DOF_BF(K,2) ) &
                                            + p%AxRedBld(K,1,3,p%TipNode)*x%QDT( DOF_BF(K,1) )   )*OtherState%CoordSys%j3(K,:)
   OtherState%RtHS%PLinVelES(K,p%TipNode,DOF_BF(K,2),1,:) = TmpVec3 &
                                        - (   p%AxRedBld(K,2,2,p%TipNode)*x%QDT( DOF_BF(K,2) ) &
                                            + p%AxRedBld(K,1,2,p%TipNode)*x%QDT( DOF_BF(K,1) ) &
                                            + p%AxRedBld(K,2,3,p%TipNode)*x%QDT( DOF_BE(K,1) )   )*OtherState%CoordSys%j3(K,:)

   LinVelHS                 = x%QDT( DOF_BF(K,1) )*OtherState%RtHS%PLinVelES(K,p%TipNode,DOF_BF(K,1),0,:) &
                            + x%QDT( DOF_BE(K,1) )*OtherState%RtHS%PLinVelES(K,p%TipNode,DOF_BE(K,1),0,:) &
                            + x%QDT( DOF_BF(K,2) )*OtherState%RtHS%PLinVelES(K,p%TipNode,DOF_BF(K,2),0,:)
   OtherState%RtHS%LinAccESt(K,p%TipNode,:) = x%QDT( DOF_BF(K,1) )*OtherState%RtHS%PLinVelES(K,p%TipNode,DOF_BF(K,1),1,:) &
                            + x%QDT( DOF_BE(K,1) )*OtherState%RtHS%PLinVelES(K,p%TipNode,DOF_BE(K,1),1,:) &
                            + x%QDT( DOF_BF(K,2) )*OtherState%RtHS%PLinVelES(K,p%TipNode,DOF_BF(K,2),1,:)

   LinVelES               = LinVelHS + OtherState%RtHS%LinVelEZ
   DO I = 1,p%NPH   ! Loop through all DOFs associated with the angular motion of the hub (body H)

      TmpVec0 = CROSS_PRODUCT( PAngVelEH(p%PH(I),0,:),     rQS(K,p%TipNode,:)            )
      TmpVec1 = CROSS_PRODUCT( PAngVelEH(p%PH(I),0,:), EwHXrQS              + LinVelHS   )
      TmpVec2 = CROSS_PRODUCT( PAngVelEH(p%PH(I),1,:),     rQS(K,p%TipNode,:)            )

      OtherState%RtHS%PLinVelES(K,p%TipNode,p%PH(I),0,:) = OtherState%RtHS%PLinVelES(K,p%TipNode,p%PH(I),0,:) + TmpVec0
      OtherState%RtHS%PLinVelES(K,p%TipNode,p%PH(I),1,:) = OtherState%RtHS%PLinVelES(K,p%TipNode,p%PH(I),1,:) + TmpVec1 + TmpVec2

      LinVelES                  = LinVelES                   + x%QDT(p%PH(I)    )*&
                                               OtherState%RtHS%PLinVelES(K,p%TipNode,p%PH(I),0,:)
      OtherState%RtHS%LinAccESt(K,p%TipNode, :) = OtherState%RtHS%LinAccESt(K,p%TipNode,  :) + x%QDT(p%PH(I)    )* &
                                               OtherState%RtHS%PLinVelES(K,p%TipNode,p%PH(I),1,:)

   ENDDO          ! I - all DOFs associated with the angular motion of the hub (body H)

!JASON: USE TipNode HERE INSTEAD OF BldNodes IF YOU ALLOCATE AND DEFINE n1, n2, n3, m1, m2, AND m3 TO USE TipNode.  THIS WILL REQUIRE THAT THE AERODYNAMIC AND STRUCTURAL TWISTS, AeroTwst() AND ThetaS(), BE KNOWN AT THE TIP!!!
   LinVelESm2(K) = DOT_PRODUCT( LinVelES, OtherState%CoordSys%m2(K,p%BldNodes,:) )


   DO J = 1,p%BldNodes ! Loop through the blade nodes / elements

   ! Define the partial linear velocities (and their 1st derivatives) of the
   !   current node (point S(RNodes(J))) in the inertia frame.  Also define
   !   the overall linear velocity of the current node in the inertia frame.
   !   Also, define the portion of the linear acceleration of the current node
   !   in the inertia frame associated with everything but the QD2T()'s:

      EwHXrQS = CROSS_PRODUCT(  AngVelEH, rQS(K,J,:) )

      OtherState%RtHS%PLinVelES(K,J,          :,:,:) = PLinVelEQ(:,:,:)
      OtherState%RtHS%PLinVelES(K,J,DOF_BF(K,1),0,:) = p%TwistedSF(K,1,1,J,0)                          *OtherState%CoordSys%j1(K,:) &
                                     + p%TwistedSF(K,2,1,J,0)                          *OtherState%CoordSys%j2(K,:) &
                                     - (   p%AxRedBld(K,1,1,J)*x%QT ( DOF_BF(K,1) ) &
                                         + p%AxRedBld(K,1,2,J)*x%QT ( DOF_BF(K,2) ) &
                                         + p%AxRedBld(K,1,3,J)*x%QT ( DOF_BE(K,1) )   )*OtherState%CoordSys%j3(K,:)
      OtherState%RtHS%PLinVelES(K,J,DOF_BE(K,1),0,:) = p%TwistedSF(K,1,3,J,0)                          *OtherState%CoordSys%j1(K,:) &
                                     + p%TwistedSF(K,2,3,J,0)                          *OtherState%CoordSys%j2(K,:) &
                                     - (   p%AxRedBld(K,3,3,J)*x%QT ( DOF_BE(K,1) ) &
                                         + p%AxRedBld(K,2,3,J)*x%QT ( DOF_BF(K,2) ) &
                                         + p%AxRedBld(K,1,3,J)*x%QT ( DOF_BF(K,1) )   )*OtherState%CoordSys%j3(K,:)
      OtherState%RtHS%PLinVelES(K,J,DOF_BF(K,2),0,:) = p%TwistedSF(K,1,2,J,0)                          *OtherState%CoordSys%j1(K,:) &
                                     + p%TwistedSF(K,2,2,J,0)                          *OtherState%CoordSys%j2(K,:) &
                                     - (   p%AxRedBld(K,2,2,J)*x%QT ( DOF_BF(K,2) ) &
                                         + p%AxRedBld(K,1,2,J)*x%QT ( DOF_BF(K,1) ) &
                                         + p%AxRedBld(K,2,3,J)*x%QT ( DOF_BE(K,1) )   )*OtherState%CoordSys%j3(K,:)

      TmpVec1 = CROSS_PRODUCT( AngVelEH, OtherState%RtHS%PLinVelES(K,J,DOF_BF(K,1),0,:) )
      TmpVec2 = CROSS_PRODUCT( AngVelEH, OtherState%RtHS%PLinVelES(K,J,DOF_BE(K,1),0,:) )
      TmpVec3 = CROSS_PRODUCT( AngVelEH, OtherState%RtHS%PLinVelES(K,J,DOF_BF(K,2),0,:) )

      OtherState%RtHS%PLinVelES(K,J,DOF_BF(K,1),1,:) = TmpVec1 &
                                     - (   p%AxRedBld(K,1,1,J)*x%QDT( DOF_BF(K,1) ) &
                                         + p%AxRedBld(K,1,2,J)*x%QDT( DOF_BF(K,2) ) &
                                         + p%AxRedBld(K,1,3,J)*x%QDT( DOF_BE(K,1) )   )*OtherState%CoordSys%j3(K,:)
      OtherState%RtHS%PLinVelES(K,J,DOF_BE(K,1),1,:) = TmpVec2 &
                                     - (   p%AxRedBld(K,3,3,J)*x%QDT( DOF_BE(K,1) ) &
                                         + p%AxRedBld(K,2,3,J)*x%QDT( DOF_BF(K,2) ) &
                                         + p%AxRedBld(K,1,3,J)*x%QDT( DOF_BF(K,1) )   )*OtherState%CoordSys%j3(K,:)
      OtherState%RtHS%PLinVelES(K,J,DOF_BF(K,2),1,:) = TmpVec3 &
                                     - (   p%AxRedBld(K,2,2,J)*x%QDT( DOF_BF(K,2) ) &
                                         + p%AxRedBld(K,1,2,J)*x%QDT( DOF_BF(K,1) ) &
                                         + p%AxRedBld(K,2,3,J)*x%QDT( DOF_BE(K,1) )   )*OtherState%CoordSys%j3(K,:)

      LinVelHS         = x%QDT( DOF_BF(K,1) )*OtherState%RtHS%PLinVelES(K,J,DOF_BF(K,1),0,:) &
                       + x%QDT( DOF_BE(K,1) )*OtherState%RtHS%PLinVelES(K,J,DOF_BE(K,1),0,:) &
                       + x%QDT( DOF_BF(K,2) )*OtherState%RtHS%PLinVelES(K,J,DOF_BF(K,2),0,:)
      OtherState%RtHS%LinAccESt(K,J,:) = x%QDT( DOF_BF(K,1) )*OtherState%RtHS%PLinVelES(K,J,DOF_BF(K,1),1,:) &
                       + x%QDT( DOF_BE(K,1) )*OtherState%RtHS%PLinVelES(K,J,DOF_BE(K,1),1,:) &
                       + x%QDT( DOF_BF(K,2) )*OtherState%RtHS%PLinVelES(K,J,DOF_BF(K,2),1,:)

      LinVelES         = LinVelHS + OtherState%RtHS%LinVelEZ
      DO I = 1,p%NPH   ! Loop through all DOFs associated with the angular motion of the hub (body H)

         TmpVec0 = CROSS_PRODUCT(   PAngVelEH(p%PH(I),0,:),     rQS(K,J,:)            )
         TmpVec1 = CROSS_PRODUCT(   PAngVelEH(p%PH(I),0,:), EwHXrQS        + LinVelHS )
         TmpVec2 = CROSS_PRODUCT(   PAngVelEH(p%PH(I),1,:),     rQS(K,J,:)            )

         OtherState%RtHS%PLinVelES(K,J,p%PH(I),0,:) = OtherState%RtHS%PLinVelES(K,J,p%PH(I),0,:) + TmpVec0
         OtherState%RtHS%PLinVelES(K,J,p%PH(I),1,:) = OtherState%RtHS%PLinVelES(K,J,p%PH(I),1,:) + TmpVec1 + TmpVec2

         LinVelES                 = LinVelES                 + x%QDT(p%PH(I))*OtherState%RtHS%PLinVelES(K,J,p%PH(I),0,:)
         OtherState%RtHS%LinAccESt(K,J,        :) = OtherState%RtHS%LinAccESt(K,J,        :) + x%QDT(p%PH(I))*OtherState%RtHS%PLinVelES(K,J,p%PH(I),1,:)

      END DO ! I - all DOFs associated with the angular motion of the hub (body H)

      ADAeroMarkers%Blade(J,K)%TranslationVel(:)= (/ LinVelES(1), -1.*LinVelES(3),  LinVelES(2)  /)  !AeroDyn's coordinates


   END DO !J = 1,p%BldNodes ! Loop through the blade nodes / elements

END DO !K = 1,p%NumBl




PLinVelEW(       :,:,:) = OtherState%RtHS%PLinVelEO(:,:,:)
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
 LinVelEK               =  OtherState%RtHS%LinVelEZ
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

OtherState%RtHS%PFrcPRot  = 0.0   ! Initialize these partial
OtherState%RtHS%PMomLPRot = 0.0   ! forces and moments to zero
DO I = 1,p%DOFs%NPCE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the hub center of mass (point C)

   TmpVec1 = -p%HubMass*PLinVelEC(p%DOFs%PCE(I),0,:)     ! The portion of PFrcPRot  associated with the HubMass
   TmpVec2 = CROSS_PRODUCT( rPC, TmpVec1 )      ! The portion of PMomLPRot associated with the HubMass

   OtherState%RtHS%PFrcPRot (p%DOFs%PCE(I),:) = TmpVec1

   OtherState%RtHS%PMomLPRot(p%DOFs%PCE(I),:) = TmpVec2 - p%Hubg1Iner*OtherState%CoordSys%g1*DOT_PRODUCT( OtherState%CoordSys%g1, PAngVelEH(p%DOFs%PCE(I),0,:) ) &
                                 - p%Hubg2Iner*OtherState%CoordSys%g2*DOT_PRODUCT( OtherState%CoordSys%g2, PAngVelEH(p%DOFs%PCE(I),0,:) )

ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the hub center of mass (point C)

TmpVec1 = -p%HubMass*( p%Gravity*OtherState%CoordSys%z2 + LinAccECt )                     ! The portion of FrcPRott  associated with the HubMass
TmpVec2 = CROSS_PRODUCT( rPC, TmpVec1 )                                    ! The portion of MomLPRott associated with the HubMass
TmpVec  = p%Hubg1Iner*OtherState%CoordSys%g1*DOT_PRODUCT( OtherState%CoordSys%g1, AngVelEH ) &     ! = ( Hub inertia dyadic ) dot ( angular velocity of hub in the inertia frame )
        + p%Hubg2Iner*OtherState%CoordSys%g2*DOT_PRODUCT( OtherState%CoordSys%g2, AngVelEH )
TmpVec3 = CROSS_PRODUCT( -AngVelEH, TmpVec )                               ! = ( -angular velocity of hub in the inertia frame ) cross ( TmpVec )

OtherState%RtHS%FrcPRott  = TmpVec1
OtherState%RtHS%MomLPRott = TmpVec2 + TmpVec3 - p%Hubg1Iner*OtherState%CoordSys%g1*DOT_PRODUCT( OtherState%CoordSys%g1, AngAccEHt ) &
                              - p%Hubg2Iner*OtherState%CoordSys%g2*DOT_PRODUCT( OtherState%CoordSys%g2, AngAccEHt )


   !-------------------------------------------------------------------------------------------------
   ! Call AeroDyn to calculate aerodynamic forces
   !-------------------------------------------------------------------------------------------------

IF ( p_FAST%CompAero ) ADAeroLoads = AD_CalculateLoads( REAL(t, ReKi), ADAeroMarkers, ADInterfaceComponents, ADIntrfaceOptions, ErrStat )

!y%RotSpeed = x%QDT(DOF_GeAz) + x%QDT(DOF_DrTr)
!u_SrvD%RotSpeed = y%RotSpeed   
!CALL TipBrake_CalcOutput( t, u_SrvD, p_SrvD, x_SrvD, xd_SrvD, z_SrvD, OtherState_SrvD, y_SrvD, ErrStat, ErrMsg )   


DO K = 1,p%NumBl ! Loop through all blades

   ! Calculate the position vector from the teeter pin to the blade root:

   rPS0 = rPQ + p%HubRad*OtherState%CoordSys%j3(K,:)   ! Position vector from teeter pin (point P) to blade root (point S(0)).


   ! Calculate the tip drag forces if necessary:

   IF ( p_FAST%CompAero )  THEN   ! Calculate the tip drag using the built-in model.

      OtherState%RtHS%FSTipDrag(K,:) = OtherState%CoordSys%m2(K,p%BldNodes,:)*SIGN( 0.5*p%AirDens*(LinVelESm2(K)**2)*y_SrvD%TBDrCon(K), -1.*LinVelESm2(K) )

   ELSE                    ! Wind turbine in vacuum, no aerodynamic forces.

      OtherState%RtHS%FSTipDrag(K,:) = 0.0

   ENDIF


   ! Initialize the partial forces and moments (including those associated
   !   with the QD2T()'s and those that are not) at the blade root (point S(0))
   !   using the tip brake effects:

   OtherState%RtHS%PFrcS0B(K,:,:) = 0.0 ! Initialize these partial
   OtherState%RtHS%PMomH0B(K,:,:) = 0.0 ! forces and moments to zero
   DO I = 1,p%DOFs%NPSE(K)  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of blade K

      TmpVec1 = -p%TipMass(K)*OtherState%RtHS%PLinVelES(K,p%TipNode,p%DOFs%PSE(K,I),0,:)  ! The portion of PFrcS0B associated with the tip brake
      TmpVec2 = CROSS_PRODUCT( OtherState%RtHS%rS0S(K,p%TipNode,:), TmpVec1 )                    ! The portion of PMomH0B associated with the tip brake

      OtherState%RtHS%PFrcS0B(K,p%DOFs%PSE(K,I),:) = TmpVec1

      OtherState%RtHS%PMomH0B(K,p%DOFs%PSE(K,I),:) = TmpVec2

   ENDDO             ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of blade K

   TmpVec1 = OtherState%RtHS%FSTipDrag(K,:) - p%TipMass(K)*( p%Gravity*OtherState%CoordSys%z2 + OtherState%RtHS%LinAccESt(K,p%TipNode,:) ) ! The portion of FrcS0Bt associated with the tip brake
   TmpVec2 = CROSS_PRODUCT(  OtherState%RtHS%rS0S(K,p%TipNode,:), TmpVec1 )                                 ! The portion of OtherState%RtHS%MomH0Bt associated with the tip brake

   OtherState%RtHS%FrcS0Bt(K,:) = TmpVec1

   OtherState%RtHS%MomH0Bt(K,:) = TmpVec2


   ! Initialize the portions of the mass matrix on and below the
   !   diagonal associated with purely blade DOFs (these portions can't
   !   be calculated using partial loads) using the tip mass effects.
   !   Also, initialize the portions of the forcing vector associated
   !   with purely blade DOFs (these portions can't be calculated using
   !   partial loads) using the tip mass effects:
   ! NOTE: The vector subscript array, PSBE(), used in the following loops must
   !       be sorted from smallest to largest DOF index in order for the loops
   !       to work to enter values only on and below the diagonal of AugMat():

   DO L = 1,p%DOFs%NPSBE(K)    ! Loop through all active (enabled) blade DOFs that contribute to the QD2T-related linear accelerations of the tip of blade K (point S(p%BldFlexL))
      DO I = L,p%DOFs%NPSBE(K) ! Loop through all active (enabled) blade DOFs greater than or equal to L
         AugMat(p%DOFs%PSBE(K,I),p%DOFs%PSBE(K,L)) = p%TipMass(K)*&
                                     DOT_PRODUCT( OtherState%RtHS%PLinVelES(K, p%TipNode, p%DOFs%PSBE(K,I),0,:), &   ! [C(q,t)]B
                                                  OtherState%RtHS%PLinVelES(K, p%TipNode, p%DOFs%PSBE(K,L),0,:)    )
      ENDDO             ! I - All active (enabled) blade DOFs greater than or equal to L
   ENDDO                ! L - All active (enabled) blade DOFs that contribute to the QD2T-related linear accelerations of the tip of blade K (point S(p%BldFlexL))
   DO I = 1,p%DOFs%NPSBE(K)    ! Loop through all active (enabled) blade DOFs that contribute to the QD2T-related linear accelerations of the tip of blade K (point S(p%BldFlexL))
         AugMat(p%DOFs%PSBE(K,I), p%NAug) = DOT_PRODUCT( OtherState%RtHS%PLinVelES(K,p%TipNode,p%DOFs%PSBE(K,I),0,:), &   ! {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB
                                                           TmpVec1                               ) ! NOTE: TmpVec1 is still the portion of FrcS0Bt associated with the tip brake
   ENDDO                ! I - All active (enabled) blade DOFs that contribute to the QD2T-related linear accelerations of the tip of blade K (point S(p%BldFlexL))



   DO J = 1,p%BldNodes ! Loop through the blade nodes / elements

   ! Calculate the normal and tangential aerodynamic forces and the aerodynamic
   !   pitching moment at the current element per unit span by calling AeroDyn,
   !   if necessary:

      IF ( p_FAST%CompAero )  THEN   ! Calculate the blade element aerodynamic loads using AeroDyn.


   ! Calculate the aerodynamic pitching moment arm (i.e., the position vector
   !   from point S on the blade to the aerodynamic center of the element):

         rSAerCen = p%rSAerCenn1(K,J)*OtherState%CoordSys%n1(K,J,:) + p%rSAerCenn2(K,J)*OtherState%CoordSys%n2(K,J,:)        ! bjj this is now re-calculated.


!JASON: WE SHOULD REALLY BE PASSING TO AERODYN THE LINEAR VELOCITIES OF THE AERODYNAMIC CENTER IN THE INERTIA FRAME, NOT SIMPLY THE LINEAR VELOCITIES OF POINT S.  IS THERE ANY WAY OF GETTING THIS VELOCITY?<--DO THIS, WHEN YOU ADD THE COUPLED MODE SHAPES!!!!

   ! Call AeroDyn through AeroCalc() and fill FSAero() and MMAero() with
   !   the resulting forces (AeroForces(:)):
   ! NOTE: AeroForces(1) = element normal     force per unit span in the  m1 direction (N/m).
   !       AeroForces(2) = element tangential force per unit span in the -m2 direction (N/m).
   !       AeroForces(3) = element pitching moment  per unit span in about the m3-axis (N-m/m).


         OtherState%RtHS%FSAero(K,J,:) = ADAeroLoads%Blade(J, K)%Force(1) * OtherState%CoordSys%te1(K,J,:) &
                       + ADAeroLoads%Blade(J, K)%Force(2) * OtherState%CoordSys%te2(K,J,:)

         OtherState%RtHS%MMAero(K,J,:) = CROSS_PRODUCT( rSAerCen, OtherState%RtHS%FSAero(K,J,:) )
         OtherState%RtHS%MMAero(K,J,:) = OtherState%RtHS%MMAero(K,J,:) + ADAeroLoads%Blade(J, K)%Moment(3) * OtherState%CoordSys%te3(K,J,:)


      ELSE                    ! Wind turbine in vacuum, no aerodynamic forces.

         OtherState%RtHS%FSAero(K,J,:) = 0.0
         OtherState%RtHS%MMAero(K,J,:) = 0.0

      ENDIF


   ! Calculate the mass of the current element

      ElmntMass = p%MassB(K,J)*p%DRNodes(J)   ! Mass of blade element J


   ! Integrate to find the partial forces and moments (including those associated
   !   with the QD2T()'s and those that are not) at the blade root (point S(0)):

      DO I = 1,p%DOFs%NPSE(K)  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of blade K

         TmpVec1 = -ElmntMass*OtherState%RtHS%PLinVelES(K,J,p%DOFs%PSE(K,I),0,:)   ! The portion of PFrcS0B associated with blade element J
         TmpVec2 = CROSS_PRODUCT( OtherState%RtHS%rS0S(K,J,:), TmpVec1 )    ! The portion of PMomH0B associated with blade element J

         OtherState%RtHS%PFrcS0B(K,p%DOFs%PSE(K,I),:) = OtherState%RtHS%PFrcS0B(K,p%DOFs%PSE(K,I),:) + TmpVec1

         OtherState%RtHS%PMomH0B(K,p%DOFs%PSE(K,I),:) = OtherState%RtHS%PMomH0B(K,p%DOFs%PSE(K,I),:) + TmpVec2

      ENDDO             ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of blade K

      TmpVec1 = OtherState%RtHS%FSAero(K,J,:)*p%DRNodes(J) - ElmntMass*( p%Gravity*OtherState%CoordSys%z2 + OtherState%RtHS%LinAccESt(K,J,:) ) ! The portion of FrcS0Bt associated with blade element J
      TmpVec2 = CROSS_PRODUCT( OtherState%RtHS%rS0S(K,J,:), TmpVec1 )                                  ! The portion of MomH0Bt associated with blade element J
      TmpVec3 = OtherState%RtHS%MMAero(K,J,:)*p%DRNodes(J)                                               ! The total external moment applied to blade element J

      OtherState%RtHS%FrcS0Bt(K,:) = OtherState%RtHS%FrcS0Bt(K,:) + TmpVec1

      OtherState%RtHS%MomH0Bt(K,:) = OtherState%RtHS%MomH0Bt(K,:) + TmpVec2 + TmpVec3


   ! Integrate to find the portions of the mass matrix on and below the
   !   diagonal associated with purely blade DOFs (these portions can't
   !   be calculated using partial loads).  Also, integrate to find the
   !   portions of the forcing vector associated with purely blade DOFs
   !   (these portions can't be calculated using partial loads):
   ! NOTE: The vector subscript array, PSBE(), used in the following loops must
   !       be sorted from smallest to largest DOF index in order for the loops
   !       to work to enter values only on and below the diagonal of AugMat():

      DO L = 1,p%DOFs%NPSBE(K)    ! Loop through all active (enabled) blade DOFs that contribute to the QD2T-related linear accelerations of the blade
         DO I = L,p%DOFs%NPSBE(K) ! Loop through all active (enabled) blade DOFs greater than or equal to L
            AugMat(p%DOFs%PSBE(K,I),p%DOFs%PSBE(K,L)) = AugMat(p%DOFs%PSBE(K,I),p%DOFs%PSBE(K,L)) + ElmntMass*&
                                          DOT_PRODUCT( OtherState%RtHS%PLinVelES(K,J,p%DOFs%PSBE(K,I),0,:), &           ! [C(q,t)]B
                                                       OtherState%RtHS%PLinVelES(K,J,p%DOFs%PSBE(K,L),0,:)   )
         ENDDO             ! I - All active (enabled) blade DOFs greater than or equal to L
      ENDDO                ! L - All active (enabled) blade DOFs that contribute to the QD2T-related linear accelerations of the blade
      DO I = 1,p%DOFs%NPSBE(K)    ! Loop through all active (enabled) blade DOFs that contribute to the QD2T-related linear accelerations of the blade
            AugMat(p%DOFs%PSBE(K,I), p%NAug) = AugMat(p%DOFs%PSBE(K,I),     p%NAug)                      & ! {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB
                                        + DOT_PRODUCT( OtherState%RtHS%PLinVelES(K,J,p%DOFs%PSBE(K,I),0,:), TmpVec1 ) & ! NOTE: TmpVec1 is still the portion of FrcS0Bt associated with blade element J
                                        + DOT_PRODUCT( PAngVelEM(K,J,p%DOFs%PSBE(K,I),0,:), TmpVec3 )   !       and TmpVec3 is still the total external moment applied to blade element J
      ENDDO                ! I - All active (enabled) blade DOFs that contribute to the QD2T-related linear accelerations of the blade


   ENDDO ! J - Blade nodes / elements



   ! Add the blade effects to the partial forces and moments (including
   !   those associated with the QD2T()'s and those that are not) at the
   !   teeter pin (point P):

   DO I = 1,p%DOFs%NPSE(K)  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of blade K

      TmpVec = CROSS_PRODUCT( rPS0, OtherState%RtHS%PFrcS0B(K,p%DOFs%PSE(K,I),:) ) ! The portion of PMomLPRot associated with PFrcS0B.

      OtherState%RtHS%PFrcPRot (p%DOFs%PSE(K,I),:) = OtherState%RtHS%PFrcPRot (p%DOFs%PSE(K,I),:) + OtherState%RtHS%PFrcS0B(K,p%DOFs%PSE(K,I),:)

      OtherState%RtHS%PMomLPRot(p%DOFs%PSE(K,I),:) = OtherState%RtHS%PMomLPRot(p%DOFs%PSE(K,I),:) + OtherState%RtHS%PMomH0B(K,p%DOFs%PSE(K,I),:)+TmpVec

   ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of blade K

   TmpVec = CROSS_PRODUCT( rPS0, OtherState%RtHS%FrcS0Bt(K,:) )       ! The portion of MomLPRott associated with FrcS0Bt.

   OtherState%RtHS%FrcPRott  = OtherState%RtHS%FrcPRott  + OtherState%RtHS%FrcS0Bt(K,:)

   OtherState%RtHS%MomLPRott = OtherState%RtHS%MomLPRott + OtherState%RtHS%MomH0Bt(K,:) + TmpVec



   ! Initialize the portions of the mass matrix below the diagonal associated
   !   with the teeter and pure blade DOFs using the partial loads at the
   !   teeter pin; only do this if necessary:

   IF ( ( p%NumBl == 2 ) .AND. ( p%DOF_Flag(DOF_Teet) ) )  THEN
      DO L = 1,p%DOFs%NPSBE(K) ! Loop through all active (enabled) blade DOFs that contribute to the QD2T-related linear accelerations of the blade
         AugMat(DOF_Teet,p%DOFs%PSBE(K,L)) = -DOT_PRODUCT( PAngVelEH(DOF_Teet,0,:), &
                                                            OtherState%RtHS%PMomLPRot(p%DOFs%PSBE(K,L),:) )  ! [C(q,t)]B
      ENDDO             ! L - All active (enabled) blade DOFs that contribute to the QD2T-related linear accelerations of the blade
   ENDIF



   ! If the associated DOFs are enabled, add the blade elasticity and damping
   !   forces to the forcing vector (these portions can't be calculated using
   !   partial loads):

   IF ( p%DOF_Flag(DOF_BF(K,1)) )  THEN
      AugMat(    DOF_BF(K,1),p%NAug) = AugMat(DOF_BF(K,1),p%NAug)      & !
                                   - p%KBF(K,1,1)*x%QT( DOF_BF(K,1)) &
                                   - p%KBF(K,1,2)*x%QT( DOF_BF(K,2)) &
                                   - p%CBF(K,1,1)*x%QDT(DOF_BF(K,1)) &
                                   - p%CBF(K,1,2)*x%QDT(DOF_BF(K,2))
   ENDIF
   IF ( p%DOF_Flag(DOF_BF(K,2)) )  THEN
      AugMat(    DOF_BF(K,2),p%NAug) = AugMat(DOF_BF(K,2),p%NAug)      & ! {-f(qd,q,t)}ElasticB + {-f(qd,q,t)}DampB
                                   - p%KBF(K,2,1)*x%QT( DOF_BF(K,1)) &
                                   - p%KBF(K,2,2)*x%QT( DOF_BF(K,2)) &
                                   - p%CBF(K,2,1)*x%QDT(DOF_BF(K,1)) &
                                   - p%CBF(K,2,2)*x%QDT(DOF_BF(K,2))
   ENDIF
   IF ( p%DOF_Flag(DOF_BE(K,1)) )  THEN
      AugMat(    DOF_BE(K,1),p%NAug) = AugMat(DOF_BE(K,1),p%NAug)      & !
                                   - p%KBE(K,1,1)*x%QT( DOF_BE(K,1)) &
                                   - p%CBE(K,1,1)*x%QDT(DOF_BE(K,1))
   ENDIF


ENDDO ! K - Blades



   ! Define the partial forces and moments (including those associated with
   !   the QD2T()'s and those that are not) at the specified point on the
   !   rotor-furl axis (point V) / nacelle (body N) using the structure that
   !   furls with the rotor, generator, and rotor effects.

PFrcVGnRt = OtherState%RtHS%PFrcPRot    ! Initialize these partial forces and
OtherState%RtHS%PMomNGnRt = OtherState%RtHS%PMomLPRot   ! moments using the rotor effects
DO I = 1,p%DOFs%NActvDOF ! Loop through all active (enabled) DOFs

   TmpVec = CROSS_PRODUCT( rVP, OtherState%RtHS%PFrcPRot(p%DOFs%SrtPS(I),:) )  ! The portion of PMomNGnRt associated with the PFrcPRot

   OtherState%RtHS%PMomNGnRt(p%DOFs%SrtPS(I),:) = OtherState%RtHS%PMomNGnRt(p%DOFs%SrtPS(I),:) + TmpVec

ENDDO             ! I - All active (enabled) DOFs
DO I = 1,p%DOFs%NPDE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the center of mass of the structure that furls with the rotor (not including rotor) (point D)

   TmpVec1 = -p%RFrlMass*PLinVelED(p%DOFs%PDE(I)  ,0,:)           ! The portion of PFrcVGnRt associated with the RFrlMass
   TmpVec2 = CROSS_PRODUCT( rVD,              TmpVec1 )  ! The portion of PMomNGnRt associated with the RFrlMass

   PFrcVGnRt(p%DOFs%PDE(I)  ,:) = PFrcVGnRt(p%DOFs%PDE(I)  ,:) + TmpVec1

   OtherState%RtHS%PMomNGnRt(p%DOFs%PDE(I)  ,:) = OtherState%RtHS%PMomNGnRt(p%DOFs%PDE(I)  ,:) + TmpVec2                                   &
                         - p%RrfaIner*OtherState%CoordSys%rfa*DOT_PRODUCT( OtherState%CoordSys%rfa, OtherState%RtHS%PAngVelER(p%DOFs%PDE(I)  ,0,:) ) &
                         -  p%GenIner*OtherState%CoordSys%c1 *DOT_PRODUCT( OtherState%CoordSys%c1 , PAngVelEG(p%DOFs%PDE(I)  ,0,:) )

ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the center of mass of the structure that furls with the rotor (not including rotor) (point D)
IF ( p%DOF_Flag(DOF_GeAz) )  THEN

   OtherState%RtHS%PMomNGnRt(DOF_GeAz,:) = OtherState%RtHS%PMomNGnRt(DOF_GeAz,:)                                             &           ! The previous loop (DO I = 1,NPDE) misses the DOF_GeAz-contribution to: ( Generator inertia dyadic ) dot ( partial angular velocity of the generator in the inertia frame )
                         -  p%GenIner*OtherState%CoordSys%c1 *DOT_PRODUCT( OtherState%CoordSys%c1 , PAngVelEG(DOF_GeAz,0,:) )     ! Thus, add this contribution if necessary.

ENDIF

TmpVec1 = -p%RFrlMass*( p%Gravity*OtherState%CoordSys%z2 + LinAccEDt )                    ! The portion of FrcVGnRtt associated with the RFrlMass
TmpVec2 = CROSS_PRODUCT( rVD      ,  TmpVec1 )                             ! The portion of MomNGnRtt associated with the RFrlMass
TmpVec3 = CROSS_PRODUCT( rVP      , OtherState%RtHS%FrcPRott )                             ! The portion of MomNGnRtt associated with the FrcPRott
TmpVec  = p%RrfaIner*OtherState%CoordSys%rfa*DOT_PRODUCT( OtherState%CoordSys%rfa, OtherState%RtHS%AngVelER )      ! = ( R inertia dyadic ) dot ( angular velocity of structure that furls with the rotor in the inertia frame )
TmpVec4 = CROSS_PRODUCT( -OtherState%RtHS%AngVelER, TmpVec )                               ! = ( -angular velocity of structure that furls with the rotor in the inertia frame ) cross ( TmpVec )
TmpVec  =  p%GenIner*OtherState%CoordSys%c1* DOT_PRODUCT( OtherState%CoordSys%c1 , AngVelEG )      ! = ( Generator inertia dyadic ) dot ( angular velocity of generator in the inertia frame )
TmpVec5 = CROSS_PRODUCT( -AngVelEG, TmpVec )                               ! = ( -angular velocity of generator in the inertia frame ) cross ( TmpVec )

FrcVGnRtt = OtherState%RtHS%FrcPRott  + TmpVec1

OtherState%RtHS%MomNGnRtt = OtherState%RtHS%MomLPRott + TmpVec2 + TmpVec3 + TmpVec4 + TmpVec5            &
          - p%RrfaIner*OtherState%CoordSys%rfa*DOT_PRODUCT( OtherState%CoordSys%rfa, OtherState%RtHS%AngAccERt ) &
          -  p%GenIner*OtherState%CoordSys%c1 *DOT_PRODUCT( OtherState%CoordSys%c1 , AngAccEGt )


   ! Let's compute the tail aerodynamic loads, if necessary:

   FKAero = 0.0
   MAAero = 0.0


   ! Define the partial forces and moments (including those associated with
   !   the QD2T()'s and those that are not) at the specified point on the
   !   tail-furl axis (point W) / nacelle (body N) using the tail effects.

PFrcWTail = 0.0   ! Initialize these partial
OtherState%RtHS%PMomNTail = 0.0   ! forces and moments to zero
DO I = 1,p%DOFs%NPIE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the tail boom center of mass (point I)

   TmpVec1 = -p%BoomMass*PLinVelEI(p%DOFs%PIE(I),0,:)    ! The portion of PFrcWTail associated with the BoomMass
   TmpVec2 = -p%TFinMass*PLinVelEJ(p%DOFs%PIE(I),0,:)    ! The portion of PFrcWTail associated with the TFinMass
   TmpVec3 = CROSS_PRODUCT( rWI, TmpVec1 )                      ! The portion of PMomNTail associated with the BoomMass
   TmpVec4 = CROSS_PRODUCT( rWJ, TmpVec2 )                      ! The portion of PMomNTail associated with the TFinMass

   PFrcWTail(p%DOFs%PIE(I),:) = TmpVec1 + TmpVec2

   OtherState%RtHS%PMomNTail(p%DOFs%PIE(I),:) = TmpVec3 + TmpVec4 &
                       - p%AtfaIner*OtherState%CoordSys%tfa*DOT_PRODUCT( OtherState%CoordSys%tfa, PAngVelEA(p%DOFs%PIE(I),0,:) )

ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the tail boom center of mass (point I)

TmpVec1 = -p%BoomMass*( p%Gravity*OtherState%CoordSys%z2 + LinAccEIt )                 ! The portion of FrcWTailt associated with the BoomMass
TmpVec2 = -p%TFinMass*( p%Gravity*OtherState%CoordSys%z2 + LinAccEJt )                 ! The portion of FrcWTailt associated with the TFinMass
TmpVec3 = CROSS_PRODUCT( rWI      , TmpVec1 )                           ! The portion of MomNTailt associated with the BoomMass
TmpVec4 = CROSS_PRODUCT( rWJ      , TmpVec2 )                           ! The portion of MomNTailt associated with the TFinMass
TmpVec  = p%AtfaIner*OtherState%CoordSys%tfa*DOT_PRODUCT( OtherState%CoordSys%tfa, AngVelEA )   ! = ( A inertia dyadic ) dot ( angular velocity of the tail in the inertia frame )
TmpVec5 = CROSS_PRODUCT( -AngVelEA, TmpVec  )                           ! = ( -angular velocity of the tail in the inertia frame ) cross ( TmpVec )
TmpVec  = CROSS_PRODUCT( rWK      , FKAero  )                           ! The portion of MomNTailt associated with FKAero

FrcWTailt = FKAero + TmpVec1 + TmpVec2

OtherState%RtHS%MomNTailt = MAAero + TmpVec3 + TmpVec4 + TmpVec5 + TmpVec &
          - p%AtfaIner*OtherState%CoordSys%tfa*DOT_PRODUCT( OtherState%CoordSys%tfa, AngAccEAt )



   ! Define the partial forces and moments (including those associated with
   !   the QD2T()'s and those that are not) at the yaw bearing (point O) /
   !   base plate (body B) using the nacelle, generator, rotor, and tail effects.

OtherState%RtHS%PFrcONcRt = PFrcVGnRt + PFrcWTail   ! Initialize these partial forces and moments using
OtherState%RtHS%PMomBNcRt = OtherState%RtHS%PMomNGnRt + OtherState%RtHS%PMomNTail   ! the rotor, rotor-furl, generator, and tail effects
DO I = 1,p%DOFs%NActvDOF ! Loop through all active (enabled) DOFs

   TmpVec = CROSS_PRODUCT( rOV, PFrcVGnRt(p%DOFs%SrtPS(I),:) ) ! The portion of PMomBNcRt associated with the PFrcVGnRt

   OtherState%RtHS%PMomBNcRt(p%DOFs%SrtPS(I),:) = OtherState%RtHS%PMomBNcRt(p%DOFs%SrtPS(I),:) + TmpVec

ENDDO             ! I - All active (enabled) DOFs
DO I = 1,p%DOFs%NPIE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the tail boom center of mass (point I)

   TmpVec = CROSS_PRODUCT( rOW, PFrcWTail(p%DOFs%PIE(I)  ,:) ) ! The portion of PMomBNcRt associated with the PFrcWTail

   OtherState%RtHS%PMomBNcRt(p%DOFs%PIE(I)  ,:) = OtherState%RtHS%PMomBNcRt(p%DOFs%PIE(I)  ,:) + TmpVec

ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the tail boom center of mass (point I)
DO I = 1,p%DOFs%NPUE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the nacelle center of mass (point U)

   TmpVec1 = -p%NacMass*PLinVelEU(p%DOFs%PUE(I),0,:)              ! The portion of PFrcONcRt associated with the NacMass
   TmpVec2 = CROSS_PRODUCT( rOU,               TmpVec1 ) ! The portion of PMomBNcRt associated with the NacMass

   OtherState%RtHS%PFrcONcRt(p%DOFs%PUE(I)  ,:) = OtherState%RtHS%PFrcONcRt(p%DOFs%PUE(I)  ,:) + TmpVec1

   OtherState%RtHS%PMomBNcRt(p%DOFs%PUE(I)  ,:) = OtherState%RtHS%PMomBNcRt(p%DOFs%PUE(I)  ,:) + TmpVec2 &
                         - p%Nacd2Iner*OtherState%CoordSys%d2*DOT_PRODUCT( OtherState%CoordSys%d2, PAngVelEN(p%DOFs%PUE(I),0,:) )

ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the nacelle center of mass (point U)

TmpVec1 = -p%NacMass*( p%Gravity*OtherState%CoordSys%z2 + LinAccEUt )                  ! The portion of FrcONcRtt associated with the NacMass
TmpVec2 = CROSS_PRODUCT( rOU      ,   TmpVec1 )                         ! The portion of MomBNcRtt associated with the NacMass
TmpVec3 = CROSS_PRODUCT( rOV      , FrcVGnRtt )                         ! The portion of MomBNcRtt associated with the FrcVGnRtt
TmpVec4 = CROSS_PRODUCT( rOW      , FrcWTailt )                         ! The portion of MomBNcRtt associated with the FrcWTailt
TmpVec  = p%Nacd2Iner*OtherState%CoordSys%d2*DOT_PRODUCT( OtherState%CoordSys%d2, AngVelEN )    ! = ( Nacelle inertia dyadic ) dot ( angular velocity of nacelle in the inertia frame )
TmpVec5 = CROSS_PRODUCT( -AngVelEN, TmpVec    )                         ! = ( -angular velocity of nacelle in the inertia frame ) cross ( TmpVec )

OtherState%RtHS%FrcONcRtt = FrcVGnRtt + FrcWTailt + TmpVec1

OtherState%RtHS%MomBNcRtt = OtherState%RtHS%MomNGnRtt + OtherState%RtHS%MomNTailt + TmpVec2 + TmpVec3 + TmpVec4 + TmpVec5 &
          - p%Nacd2Iner*OtherState%CoordSys%d2*DOT_PRODUCT( OtherState%CoordSys%d2, AngAccENt )



   ! Initialize the partial forces and moments (including those associated
   !   with the QD2T()'s and those that are not) at the tower base (point T(0))
   !   using everything but the tower:

OtherState%RtHS%PFrcT0Trb = OtherState%RtHS%PFrcONcRt   ! Initialize these partial forces and moments
OtherState%RtHS%PMomX0Trb = OtherState%RtHS%PMomBNcRt   ! using all of the effects above the yaw bearing
DO I = 1,p%DOFs%NActvDOF ! Loop through all active (enabled) DOFs

   TmpVec  = CROSS_PRODUCT(  OtherState%RtHS%rT0O, OtherState%RtHS%PFrcONcRt(p%DOFs%SrtPS(I),:) )   ! The portion of PMomX0Trb associated with the PFrcONcRt

   OtherState%RtHS%PMomX0Trb(p%DOFs%SrtPS(I),:) = OtherState%RtHS%PMomX0Trb(p%DOFs%SrtPS(I),:) + TmpVec

ENDDO             ! I - All active (enabled) DOFs
DO I = 1,p%DOFs%NPTE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the yaw bearing center of mass (point O)

   TmpVec1 = -p%YawBrMass*OtherState%RtHS%PLinVelEO(p%DOFs%PTE(I),0,:)               ! The portion of PFrcT0Trb associated with the YawBrMass
   TmpVec2 = CROSS_PRODUCT( OtherState%RtHS%rT0O,               TmpVec1 )   ! The portion of PMomX0Trb associated with the YawBrMass

   OtherState%RtHS%PFrcT0Trb(p%DOFs%PTE(I)  ,:) = OtherState%RtHS%PFrcT0Trb(p%DOFs%PTE(I)  ,:) + TmpVec1

   OtherState%RtHS%PMomX0Trb(p%DOFs%PTE(I)  ,:) = OtherState%RtHS%PMomX0Trb(p%DOFs%PTE(I)  ,:) + TmpVec2

ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the yaw bearing center of mass (point O)

TmpVec1 = -p%YawBrMass*( p%Gravity*OtherState%CoordSys%z2 + OtherState%RtHS%LinAccEOt ) ! The portion of FrcT0Trbt associated with the YawBrMass
TmpVec2 = CROSS_PRODUCT( OtherState%RtHS%rT0O,   TmpVec1 )               ! The portion of MomX0Trbt associated with the YawBrMass
TmpVec3 = CROSS_PRODUCT( OtherState%RtHS%rT0O, OtherState%RtHS%FrcONcRtt )               ! The portion of MomX0Trbt associated with the FrcONcRtt

OtherState%RtHS%FrcT0Trbt = OtherState%RtHS%FrcONcRtt + TmpVec1

OtherState%RtHS%MomX0Trbt = OtherState%RtHS%MomBNcRtt + TmpVec2 + TmpVec3



   ! Initialize the portions of the mass matrix on and below the diagonal
   !   associated with purely tower DOFs (these portions can't be calculated
   !   using partial loads) using the yaw bearing mass effects.
   !   Also, initialize the portions of the forcing vector associated with
   !   purely blade DOFs (these portions can't be calculated using partial
   !   loads) using the yaw bearing mass effects:
   ! NOTE: The vector subscript array, PTTE(), used in the following loops must
   !       be sorted from smallest to largest DOF index in order for the loops
   !       to work to enter values only on and below the diagonal of AugMat():

DO L = 1,p%DOFs%NPTTE    ! Loop through all active (enabled) tower DOFs that contribute to the QD2T-related linear accelerations of the yaw bearing (point O)
   DO I = L,p%DOFs%NPTTE ! Loop through all active (enabled) tower DOFs greater than or equal to L
      AugMat(p%DOFs%PTTE(I),p%DOFs%PTTE(L)) = p%YawBrMass*DOT_PRODUCT( OtherState%RtHS%PLinVelEO(p%DOFs%PTTE(I),0,:), &     ! [C(q,t)]T of YawBrMass
                                                   OtherState%RtHS%PLinVelEO(p%DOFs%PTTE(L),0,:)    )
   ENDDO          ! I - All active (enabled) tower DOFs greater than or equal to L
ENDDO             ! L - All active (enabled) tower DOFs that contribute to the QD2T-related linear accelerations of the yaw bearing (point O)
DO I = 1,p%DOFs%NPTTE    ! Loop through all active (enabled) tower DOFs that contribute to the QD2T-related linear accelerations of the yaw bearing (point O)
      AugMat(p%DOFs%PTTE(I),   p%NAug) =           DOT_PRODUCT( OtherState%RtHS%PLinVelEO(p%DOFs%PTTE(I),0,:), &     ! {-f(qd,q,t)}T + {-f(qd,q,t)}GravT of YawBrMass
                                                   TmpVec1                   )   ! NOTE: TmpVec1 is still the portion of FrcT0Trbt associated with YawBrMass
ENDDO             ! I - All active (enabled) tower DOFs that contribute to the QD2T-related linear accelerations of the yaw bearing (point O)

!----------------------------------------------------------------------------------------------------
! Get the tower element positions, velocities, and partial velocities
!----------------------------------------------------------------------------------------------------


DO J = 1,p%TwrNodes  ! Loop through the tower nodes / elements


   ! Calculate the position vector of the current node:

   OtherState%RtHS%rT0T(J,:) = ( p%TwrFASF(1,J,0)*x%QT(DOF_TFA1) + p%TwrFASF(2,J,0)*x%QT(DOF_TFA2)             )*OtherState%CoordSys%a1 &   ! Position vector from base of flexible portion of tower (point T(0)) to current node (point T(J)).
             + ( p%HNodes(J) - 0.5*(     p%AxRedTFA(1,1,J)*x%QT(DOF_TFA1)*x%QT(DOF_TFA1) &
                                   +     p%AxRedTFA(2,2,J)*x%QT(DOF_TFA2)*x%QT(DOF_TFA2) &
                                   + 2.0*p%AxRedTFA(1,2,J)*x%QT(DOF_TFA1)*x%QT(DOF_TFA2) &
                                   +     p%AxRedTSS(1,1,J)*x%QT(DOF_TSS1)*x%QT(DOF_TSS1) &
                                   +     p%AxRedTSS(2,2,J)*x%QT(DOF_TSS2)*x%QT(DOF_TSS2) &
                                   + 2.0*p%AxRedTSS(1,2,J)*x%QT(DOF_TSS1)*x%QT(DOF_TSS2)   ) )*OtherState%CoordSys%a2 &
             + ( p%TwrSSSF(1,J,0)*x%QT(DOF_TSS1) + p%TwrSSSF(2,J,0)*x%QT(DOF_TSS2)             )*OtherState%CoordSys%a3
   OtherState%RtHS%rZT (J,:) = rZT0 + OtherState%RtHS%rT0T(J,:)                                                                          ! Position vector from platform reference (point Z) to the current node (point T(HNodes(J)).


   OtherState%RtHS%rT(J,:)      = OtherState%RtHS%rZ   + OtherState%RtHS%rZT (J,:)                                                                       ! Position vector from inertial frame origin        to the current node (point T(HNodes(J)).


   ! Define the partial angular velocities (and their 1st derivatives) of the
   !   current node (body F(HNodes(J))  in the inertia frame.  Also define
   !   the overall angular velocity of the current node in the inertia frame.
   !   Also, define the portion of the angular acceleration of the current node
   !   in the inertia frame associated with everything but the QD2T()'s:

   ! NOTE: PAngVelEF(J,I,D,:) = the Dth-derivative of the partial angular velocity
   !   of DOF I for body F of element J in body E.

   PAngVelEF (J,       :,0,:) = OtherState%RtHS%PAngVelEX(:,0,:)
   PAngVelEF (J,DOF_TFA1,0,:) = -p%TwrFASF(1,J,1)*OtherState%CoordSys%a3
   PAngVelEF (J,DOF_TSS1,0,:) =  p%TwrSSSF(1,J,1)*OtherState%CoordSys%a1
   PAngVelEF (J,DOF_TFA2,0,:) = -p%TwrFASF(2,J,1)*OtherState%CoordSys%a3
   PAngVelEF (J,DOF_TSS2,0,:) =  p%TwrSSSF(2,J,1)*OtherState%CoordSys%a1

   PAngVelEF (J,       :,1,:) = OtherState%RtHS%PAngVelEX(:,1,:)
   PAngVelEF (J,DOF_TFA1,1,:) = CROSS_PRODUCT(  OtherState%RtHS%AngVelEX  ,                 PAngVelEF(J,DOF_TFA1,0,:) )
   PAngVelEF (J,DOF_TSS1,1,:) = CROSS_PRODUCT(  OtherState%RtHS%AngVelEX  ,                 PAngVelEF(J,DOF_TSS1,0,:) )
   PAngVelEF (J,DOF_TFA2,1,:) = CROSS_PRODUCT(  OtherState%RtHS%AngVelEX  ,                 PAngVelEF(J,DOF_TFA2,0,:) )
   PAngVelEF (J,DOF_TSS2,1,:) = CROSS_PRODUCT(  OtherState%RtHS%AngVelEX  ,                 PAngVelEF(J,DOF_TSS2,0,:) )


    AngVelEF (J,:)            =                 OtherState%RtHS%AngVelEX  + x%QDT(DOF_TFA1)*PAngVelEF(J,DOF_TFA1,0,:) &
                                                          + x%QDT(DOF_TSS1)*PAngVelEF(J,DOF_TSS1,0,:) &
                                                          + x%QDT(DOF_TFA2)*PAngVelEF(J,DOF_TFA2,0,:) &
                                                          + x%QDT(DOF_TSS2)*PAngVelEF(J,DOF_TSS2,0,:)

    OtherState%RtHS%AngPosXF (J,:)            =                             x%QT (DOF_TFA1)*PAngVelEF(J,DOF_TFA1,0,:) &
                                                          + x%QT (DOF_TSS1)*PAngVelEF(J,DOF_TSS1,0,:) &
                                                          + x%QT (DOF_TFA2)*PAngVelEF(J,DOF_TFA2,0,:) &
                                                          + x%QT (DOF_TSS2)*PAngVelEF(J,DOF_TSS2,0,:)
    OtherState%RtHS%AngPosEF (J,:)            =                 AngPosEX  + OtherState%RtHS%AngPosXF(J,:)
    AngAccEFt(J,:)            =                 OtherState%RtHS%AngAccEXt + x%QDT(DOF_TFA1)*PAngVelEF(J,DOF_TFA1,1,:) &
                                                          + x%QDT(DOF_TSS1)*PAngVelEF(J,DOF_TSS1,1,:) &
                                                          + x%QDT(DOF_TFA2)*PAngVelEF(J,DOF_TFA2,1,:) &
                                                          + x%QDT(DOF_TSS2)*PAngVelEF(J,DOF_TSS2,1,:)


   ! Define the partial linear velocities (and their 1st derivatives) of the
   !   current node (point T(HNodes(J))) in the inertia frame.  Also define
   !   the overall linear velocity of the current node in the inertia frame.
   !   Also, define the portion of the linear acceleration of the current node
   !   in the inertia frame associated with everything but the QD2T()'s:

   EwXXrZT                   = CROSS_PRODUCT(  OtherState%RtHS%AngVelEX, OtherState%RtHS%rZT(J,:) )

   OtherState%RtHS%PLinVelET(J,       :,:,:) = OtherState%RtHS%PLinVelEZ(:,:,:)
   OtherState%RtHS%PLinVelET(J,DOF_TFA1,0,:) = p%TwrFASF(1,J,0)*OtherState%CoordSys%a1 - (   p%AxRedTFA(1,1,J)* x%QT(DOF_TFA1) &
                                                              + p%AxRedTFA(1,2,J)* x%QT(DOF_TFA2)   )*OtherState%CoordSys%a2
   OtherState%RtHS%PLinVelET(J,DOF_TSS1,0,:) = p%TwrSSSF(1,J,0)*OtherState%CoordSys%a3 - (   p%AxRedTSS(1,1,J)* x%QT(DOF_TSS1) &
                                                              + p%AxRedTSS(1,2,J)* x%QT(DOF_TSS2)   )*OtherState%CoordSys%a2
   OtherState%RtHS%PLinVelET(J,DOF_TFA2,0,:) = p%TwrFASF(2,J,0)*OtherState%CoordSys%a1 - (   p%AxRedTFA(2,2,J)* x%QT(DOF_TFA2) &
                                                              + p%AxRedTFA(1,2,J)* x%QT(DOF_TFA1)   )*OtherState%CoordSys%a2
   OtherState%RtHS%PLinVelET(J,DOF_TSS2,0,:) = p%TwrSSSF(2,J,0)*OtherState%CoordSys%a3 - (   p%AxRedTSS(2,2,J)* x%QT(DOF_TSS2) &
                                                              + p%AxRedTSS(1,2,J)* x%QT(DOF_TSS1)   )*OtherState%CoordSys%a2

   TmpVec1                   = CROSS_PRODUCT( OtherState%RtHS%AngVelEX, OtherState%RtHS%PLinVelET(J,DOF_TFA1,0,:) )
   TmpVec2                   = CROSS_PRODUCT( OtherState%RtHS%AngVelEX, OtherState%RtHS%PLinVelET(J,DOF_TSS1,0,:) )
   TmpVec3                   = CROSS_PRODUCT( OtherState%RtHS%AngVelEX, OtherState%RtHS%PLinVelET(J,DOF_TFA2,0,:) )
   TmpVec4                   = CROSS_PRODUCT( OtherState%RtHS%AngVelEX, OtherState%RtHS%PLinVelET(J,DOF_TSS2,0,:) )

   OtherState%RtHS%PLinVelET(J,DOF_TFA1,1,:) = TmpVec1                    - (   p%AxRedTFA(1,1,J)*x%QDT(DOF_TFA1) &
                                                              + p%AxRedTFA(1,2,J)*x%QDT(DOF_TFA2)   )*OtherState%CoordSys%a2
   OtherState%RtHS%PLinVelET(J,DOF_TSS1,1,:) = TmpVec2                    - (   p%AxRedTSS(1,1,J)*x%QDT(DOF_TSS1) &
                                                              + p%AxRedTSS(1,2,J)*x%QDT(DOF_TSS2)   )*OtherState%CoordSys%a2
   OtherState%RtHS%PLinVelET(J,DOF_TFA2,1,:) = TmpVec3                    - (   p%AxRedTFA(2,2,J)*x%QDT(DOF_TFA2) &
                                                              + p%AxRedTFA(1,2,J)*x%QDT(DOF_TFA1)   )*OtherState%CoordSys%a2
   OtherState%RtHS%PLinVelET(J,DOF_TSS2,1,:) = TmpVec4                    - (   p%AxRedTSS(2,2,J)*x%QDT(DOF_TSS2) &
                                                              + p%AxRedTSS(1,2,J)*x%QDT(DOF_TSS1)   )*OtherState%CoordSys%a2

   LinVelXT       = x%QDT(DOF_TFA1)*OtherState%RtHS%PLinVelET(J,DOF_TFA1,0,:) &
                  + x%QDT(DOF_TSS1)*OtherState%RtHS%PLinVelET(J,DOF_TSS1,0,:) &
                  + x%QDT(DOF_TFA2)*OtherState%RtHS%PLinVelET(J,DOF_TFA2,0,:) &
                  + x%QDT(DOF_TSS2)*OtherState%RtHS%PLinVelET(J,DOF_TSS2,0,:)
   OtherState%RtHS%LinAccETt(J,:) = x%QDT(DOF_TFA1)*OtherState%RtHS%PLinVelET(J,DOF_TFA1,1,:) &
                  + x%QDT(DOF_TSS1)*OtherState%RtHS%PLinVelET(J,DOF_TSS1,1,:) &
                  + x%QDT(DOF_TFA2)*OtherState%RtHS%PLinVelET(J,DOF_TFA2,1,:) &
                  + x%QDT(DOF_TSS2)*OtherState%RtHS%PLinVelET(J,DOF_TSS2,1,:)

   LinVelET(J,:)  = LinVelXT + OtherState%RtHS%LinVelEZ
   DO I = 1,NPX   ! Loop through all DOFs associated with the angular motion of the platform (body X)

      TmpVec0   = CROSS_PRODUCT( OtherState%RtHS%PAngVelEX(PX(I),0,:),     OtherState%RtHS%rZT(J,:)            )
      TmpVec1   = CROSS_PRODUCT( OtherState%RtHS%PAngVelEX(PX(I),0,:), EwXXrZT      + LinVelXT )

      OtherState%RtHS%PLinVelET(J,PX(I),0,:) = OtherState%RtHS%PLinVelET(J,PX(I),0,:) + TmpVec0
      OtherState%RtHS%PLinVelET(J,PX(I),1,:) = OtherState%RtHS%PLinVelET(J,PX(I),1,:) + TmpVec1

      LinVelET( J,        :) = LinVelET( J,        :) + x%QDT(PX(I))*OtherState%RtHS%PLinVelET(J,PX(I),0,:)
      OtherState%RtHS%LinAccETt(J,        :) = OtherState%RtHS%LinAccETt(J,        :) + x%QDT(PX(I))*OtherState%RtHS%PLinVelET(J,PX(I),1,:)

   ENDDO          ! I - all DOFs associated with the angular motion of the platform (body X)


END DO ! J

!----------------------------------------------------------------------------------------------------
! Calculate hydrodynamic loads from HydroDyn
!----------------------------------------------------------------------------------------------------  
IF ( p_FAST%CompHydro ) THEN

      ! Set the markers required for HydroDyn
 
   IF ( HD_TwrNodes ) THEN
      
         ! Set the tower markers required for HydroDyn  (note this is for only the tower loading per unit length (not platform point source) !!!!!
         
      DO J = 1,p%TwrNodes  ! Loop through the tower nodes / elements
         HD_AllMarkers%Substructure(J)%Position       = (/ OtherState%RtHS%rT( J,1), -1.*OtherState%RtHS%rT( J,3),&
                                                           OtherState%RtHS%rT( J,2) - p%PtfmRef /)
         
         CALL SmllRotTrans( 'Tower', OtherState%RtHS%AngPosEF(J,1), -1.*OtherState%RtHS%AngPosEF(J,3), OtherState%RtHS%AngPosEF(J,2), &        
                                                                           HD_AllMarkers%Substructure(J)%Orientation, errstat=ErrStat, errmsg=ErrMsg )
         IF (ErrStat /= ErrID_None) CALL WrScr( TRIM(ErrMsg) )
                            
         HD_AllMarkers%Substructure(J)%TranslationVel = (/ LinVelET(J,1), -1.*LinVelET(J,3), LinVelET(J,2) /) 
         HD_AllMarkers%Substructure(J)%RotationVel    = (/ AngVelEF(J,1), -1.*AngVelEF(J,3), AngVelEF(J,2) /)
      END DO

   ELSE
   
         ! Set the platform markers required for HydroDyn (note this is for only the tower loading per unit length (not platform point source) !!!!!
   
      J = SIZE( HD_AllMarkers%Substructure, 1)
      
      HD_AllMarkers%Substructure(J)%Position      = (/ x%QT(DOF_Sg),x%QT(DOF_Sw),x%QT(DOF_Hv) /)      
      CALL SmllRotTrans( 'Platform',                   x%QT(DOF_R ),x%QT(DOF_P ),x%QT(DOF_Y ),HD_AllMarkers%Substructure(J)%Orientation, errstat=ErrStat, errmsg=ErrMsg )
         IF (ErrStat /= ErrID_None) CALL WrScr( TRIM(ErrMsg) )
                           
      HD_AllMarkers%Substructure(J)%TranslationVel= (/ x%QDT(DOF_Sg),x%QDT(DOF_Sw),x%QDT(DOF_Hv) /)  
      HD_AllMarkers%Substructure(J)%RotationVel   = (/ x%QDT(DOF_R ),x%QDT(DOF_P ),x%QDT(DOF_Y ) /)            
      
   END IF      
   
      ! Get the loads from HydroDyn

   CALL HD_CalculateLoads( t,  HD_AllMarkers,  HydroDyn_data, HD_AllLoads,  ErrStat )

   IF ( ErrStat /= 0 ) THEN
      CALL ProgAbort( ' Error calculating hydrodynamic loads in HydroDyn.' )
   END IF
   
END IF

!----------------------------------------------------------------------------------------------------
! Calculate tower loads (aerodynamic and hydrodynamic)
!----------------------------------------------------------------------------------------------------

DO J = 1,p%TwrNodes

   ! Calculate the aerodynamic forces and moments per unit length at the
   !   current tower element:
   ! NOTE: FTAero(J,:) = aerodynamic force per unit length acting on tower node J.
   ! NOTE: MFAero(J,:) = aerodynamic moment per unit length acting on tower element F at node J.

   IF ( p_FAST%CompAero )  THEN   ! Calculate the tower element aerodynamic loads using AeroDyn.

      OtherState%RtHS%FTAero(J,:) = 0.0    !JASON: ADD TOWER AERODYNAMIC LOAD CALCULATIONS HERE!!!
      OtherState%RtHS%MFAero(J,:) = 0.0    !JASON: ADD TOWER AERODYNAMIC LOAD CALCULATIONS HERE!!!

   ELSE                    ! Wind turbine in vacuum, no aerodynamic forces.

      OtherState%RtHS%FTAero(J,:) = 0.0
      OtherState%RtHS%MFAero(J,:) = 0.0

   ENDIF

   ! Let's compute the tower hydrodynamic loading; that is TwrAM(1:6,1:6) and
   !   TwrFt(1:6).


   CALL TwrLoading ( t, J, OtherState%RtHS%rT(   J,1), -OtherState%RtHS%rT(      J,3), ( OtherState%RtHS%rT(      J,2) - p%PtfmRef ), &
                                 OtherState%RtHS%AngPosEF(J,1), -OtherState%RtHS%AngPosEF(J,3), OtherState%RtHS%AngPosEF(J,2), &
                     LinVelET(J,1), -LinVelET(J,3),   LinVelET(J,2)            , AngVelEF(J,1), -AngVelEF(J,3), AngVelEF(J,2), &
                    p, u%TwrAM, u%TwrFT)




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

   OtherState%RtHS%PFTHydro(J,:,:) = 0.0
   OtherState%RtHS%PMFHydro(J,:,:) = 0.0
   DO I = 1,p%DOFs%NPTE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the tower

      OtherState%RtHS%PFTHydro(J,p%DOFs%PTE(I),:) = OtherState%CoordSys%z1*( - u%TwrAM(DOF_Sg,DOF_Sg)*OtherState%RtHS%PLinVelET(J,p%DOFs%PTE(I),0,1) &
                                           + u%TwrAM(DOF_Sg,DOF_Sw)*OtherState%RtHS%PLinVelET(J,p%DOFs%PTE(I),0,3) &
                                           - u%TwrAM(DOF_Sg,DOF_Hv)*OtherState%RtHS%PLinVelET(J,p%DOFs%PTE(I),0,2) &
                                           - u%TwrAM(DOF_Sg,DOF_R )*PAngVelEF(J,p%DOFs%PTE(I),0,1) &
                                           + u%TwrAM(DOF_Sg,DOF_P )*PAngVelEF(J,p%DOFs%PTE(I),0,3) &
                                           - u%TwrAM(DOF_Sg,DOF_Y )*PAngVelEF(J,p%DOFs%PTE(I),0,2)   ) &
                           - OtherState%CoordSys%z3*( - u%TwrAM(DOF_Sw,DOF_Sg)*OtherState%RtHS%PLinVelET(J,p%DOFs%PTE(I),0,1) &
                                           + u%TwrAM(DOF_Sw,DOF_Sw)*OtherState%RtHS%PLinVelET(J,p%DOFs%PTE(I),0,3) &
                                           - u%TwrAM(DOF_Sw,DOF_Hv)*OtherState%RtHS%PLinVelET(J,p%DOFs%PTE(I),0,2) &
                                           - u%TwrAM(DOF_Sw,DOF_R )*PAngVelEF(J,p%DOFs%PTE(I),0,1) &
                                           + u%TwrAM(DOF_Sw,DOF_P )*PAngVelEF(J,p%DOFs%PTE(I),0,3) &
                                           - u%TwrAM(DOF_Sw,DOF_Y )*PAngVelEF(J,p%DOFs%PTE(I),0,2)   ) &
                           + OtherState%CoordSys%z2*( - u%TwrAM(DOF_Hv,DOF_Sg)*OtherState%RtHS%PLinVelET(J,p%DOFs%PTE(I),0,1) &
                                           + u%TwrAM(DOF_Hv,DOF_Sw)*OtherState%RtHS%PLinVelET(J,p%DOFs%PTE(I),0,3) &
                                           - u%TwrAM(DOF_Hv,DOF_Hv)*OtherState%RtHS%PLinVelET(J,p%DOFs%PTE(I),0,2) &
                                           - u%TwrAM(DOF_Hv,DOF_R )*PAngVelEF(J,p%DOFs%PTE(I),0,1) &
                                           + u%TwrAM(DOF_Hv,DOF_P )*PAngVelEF(J,p%DOFs%PTE(I),0,3) &
                                           - u%TwrAM(DOF_Hv,DOF_Y )*PAngVelEF(J,p%DOFs%PTE(I),0,2)   )
      OtherState%RtHS%PMFHydro(J,p%DOFs%PTE(I),:) = OtherState%CoordSys%z1*( - u%TwrAM(DOF_R ,DOF_Sg)*OtherState%RtHS%PLinVelET(J,p%DOFs%PTE(I),0,1) &
                                           + u%TwrAM(DOF_R ,DOF_Sw)*OtherState%RtHS%PLinVelET(J,p%DOFs%PTE(I),0,3) &
                                           - u%TwrAM(DOF_R ,DOF_Hv)*OtherState%RtHS%PLinVelET(J,p%DOFs%PTE(I),0,2) &
                                           - u%TwrAM(DOF_R ,DOF_R )*PAngVelEF(J,p%DOFs%PTE(I),0,1) &
                                           + u%TwrAM(DOF_R ,DOF_P )*PAngVelEF(J,p%DOFs%PTE(I),0,3) &
                                           - u%TwrAM(DOF_R ,DOF_Y )*PAngVelEF(J,p%DOFs%PTE(I),0,2)   ) &
                           - OtherState%CoordSys%z3*( - u%TwrAM(DOF_P ,DOF_Sg)*OtherState%RtHS%PLinVelET(J,p%DOFs%PTE(I),0,1) &
                                           + u%TwrAM(DOF_P ,DOF_Sw)*OtherState%RtHS%PLinVelET(J,p%DOFs%PTE(I),0,3) &
                                           - u%TwrAM(DOF_P ,DOF_Hv)*OtherState%RtHS%PLinVelET(J,p%DOFs%PTE(I),0,2) &
                                           - u%TwrAM(DOF_P ,DOF_R )*PAngVelEF(J,p%DOFs%PTE(I),0,1) &
                                           + u%TwrAM(DOF_P ,DOF_P )*PAngVelEF(J,p%DOFs%PTE(I),0,3) &
                                           - u%TwrAM(DOF_P ,DOF_Y )*PAngVelEF(J,p%DOFs%PTE(I),0,2)   ) &
                           + OtherState%CoordSys%z2*( - u%TwrAM(DOF_Y ,DOF_Sg)*OtherState%RtHS%PLinVelET(J,p%DOFs%PTE(I),0,1) &
                                           + u%TwrAM(DOF_Y ,DOF_Sw)*OtherState%RtHS%PLinVelET(J,p%DOFs%PTE(I),0,3) &
                                           - u%TwrAM(DOF_Y ,DOF_Hv)*OtherState%RtHS%PLinVelET(J,p%DOFs%PTE(I),0,2) &
                                           - u%TwrAM(DOF_Y ,DOF_R )*PAngVelEF(J,p%DOFs%PTE(I),0,1) &
                                           + u%TwrAM(DOF_Y ,DOF_P )*PAngVelEF(J,p%DOFs%PTE(I),0,3) &
                                           - u%TwrAM(DOF_Y ,DOF_Y )*PAngVelEF(J,p%DOFs%PTE(I),0,2)   )

   ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the tower

   OtherState%RtHS%FTHydrot(J,:) = OtherState%CoordSys%z1*( u%TwrFt(DOF_Sg) - u%TwrAM(DOF_Sg,DOF_Sg)*OtherState%RtHS%LinAccETt(J,1) &
                                               + u%TwrAM(DOF_Sg,DOF_Sw)*OtherState%RtHS%LinAccETt(J,3) &
                                               - u%TwrAM(DOF_Sg,DOF_Hv)*OtherState%RtHS%LinAccETt(J,2) &
                                               - u%TwrAM(DOF_Sg,DOF_R )*AngAccEFt(J,1) &
                                               + u%TwrAM(DOF_Sg,DOF_P )*AngAccEFt(J,3) &
                                               - u%TwrAM(DOF_Sg,DOF_Y )*AngAccEFt(J,2)   ) &
                 - OtherState%CoordSys%z3*( u%TwrFt(DOF_Sw) - u%TwrAM(DOF_Sw,DOF_Sg)*OtherState%RtHS%LinAccETt(J,1) &
                                               + u%TwrAM(DOF_Sw,DOF_Sw)*OtherState%RtHS%LinAccETt(J,3) &
                                               - u%TwrAM(DOF_Sw,DOF_Hv)*OtherState%RtHS%LinAccETt(J,2) &
                                               - u%TwrAM(DOF_Sw,DOF_R )*AngAccEFt(J,1) &
                                               + u%TwrAM(DOF_Sw,DOF_P )*AngAccEFt(J,3) &
                                               - u%TwrAM(DOF_Sw,DOF_Y )*AngAccEFt(J,2)   ) &
                 + OtherState%CoordSys%z2*( u%TwrFt(DOF_Hv) - u%TwrAM(DOF_Hv,DOF_Sg)*OtherState%RtHS%LinAccETt(J,1) &
                                               + u%TwrAM(DOF_Hv,DOF_Sw)*OtherState%RtHS%LinAccETt(J,3) &
                                               - u%TwrAM(DOF_Hv,DOF_Hv)*OtherState%RtHS%LinAccETt(J,2) &
                                               - u%TwrAM(DOF_Hv,DOF_R )*AngAccEFt(J,1) &
                                               + u%TwrAM(DOF_Hv,DOF_P )*AngAccEFt(J,3) &
                                               - u%TwrAM(DOF_Hv,DOF_Y )*AngAccEFt(J,2)   )
   OtherState%RtHS%MFHydrot(J,:) = OtherState%CoordSys%z1*( u%TwrFt(DOF_R ) - u%TwrAM(DOF_R ,DOF_Sg)*OtherState%RtHS%LinAccETt(J,1) &
                                               + u%TwrAM(DOF_R ,DOF_Sw)*OtherState%RtHS%LinAccETt(J,3) &
                                               - u%TwrAM(DOF_R ,DOF_Hv)*OtherState%RtHS%LinAccETt(J,2) &
                                               - u%TwrAM(DOF_R ,DOF_R )*AngAccEFt(J,1) &
                                               + u%TwrAM(DOF_R ,DOF_P )*AngAccEFt(J,3) &
                                               - u%TwrAM(DOF_R ,DOF_Y )*AngAccEFt(J,2)   ) &
                 - OtherState%CoordSys%z3*( u%TwrFt(DOF_P ) - u%TwrAM(DOF_P ,DOF_Sg)*OtherState%RtHS%LinAccETt(J,1) &
                                               + u%TwrAM(DOF_P ,DOF_Sw)*OtherState%RtHS%LinAccETt(J,3) &
                                               - u%TwrAM(DOF_P ,DOF_Hv)*OtherState%RtHS%LinAccETt(J,2) &
                                               - u%TwrAM(DOF_P ,DOF_R )*AngAccEFt(J,1) &
                                               + u%TwrAM(DOF_P ,DOF_P )*AngAccEFt(J,3) &
                                               - u%TwrAM(DOF_P ,DOF_Y )*AngAccEFt(J,2)   ) &
                 + OtherState%CoordSys%z2*( u%TwrFt(DOF_Y ) - u%TwrAM(DOF_Y ,DOF_Sg)*OtherState%RtHS%LinAccETt(J,1) &
                                               + u%TwrAM(DOF_Y ,DOF_Sw)*OtherState%RtHS%LinAccETt(J,3) &
                                               - u%TwrAM(DOF_Y ,DOF_Hv)*OtherState%RtHS%LinAccETt(J,2) &
                                               - u%TwrAM(DOF_Y ,DOF_R )*AngAccEFt(J,1) &
                                               + u%TwrAM(DOF_Y ,DOF_P )*AngAccEFt(J,3) &
                                               - u%TwrAM(DOF_Y ,DOF_Y )*AngAccEFt(J,2)   )

   ! Calculate the mass of the current element:

   ElmntMass = p%MassT(J)*p%DHNodes(J)   ! Mass of tower element J


   ! Integrate to find the total partial forces and moments (including those
   !   associated with the QD2T()'s and those that are not) at the tower base
   !   (point T(0)):

   DO I = 1,p%DOFs%NPTE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the tower

      TmpVec1 = OtherState%RtHS%PFTHydro(J,p%DOFs%PTE(I),:)*p%DHNodes(J) &
              - ElmntMass*OtherState%RtHS%PLinVelET(J,p%DOFs%PTE(I),0,:)           ! The portion of PFrcT0Trb associated with tower element J
      TmpVec2 = CROSS_PRODUCT( OtherState%RtHS%rT0T(J,:), TmpVec1 )         ! The portion of PMomX0Trb associated with tower element J
      TmpVec3 = OtherState%RtHS%PMFHydro(J,p%DOFs%PTE(I),:)*p%DHNodes(J)             ! The added moment applied at tower element J

      OtherState%RtHS%PFrcT0Trb(p%DOFs%PTE(I),:) = OtherState%RtHS%PFrcT0Trb(p%DOFs%PTE(I),:) + TmpVec1

      OtherState%RtHS%PMomX0Trb(p%DOFs%PTE(I),:) = OtherState%RtHS%PMomX0Trb(p%DOFs%PTE(I),:) + TmpVec2 + TmpVec3

   ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the tower

   TmpVec1 = ( OtherState%RtHS%FTAero(J,:) + OtherState%RtHS%FTHydrot(J,:) )*p%DHNodes(J) &
           - ElmntMass*( p%Gravity*OtherState%CoordSys%z2 + OtherState%RtHS%LinAccETt(J,:) )   ! The portion of FrcT0Trbt associated with tower element J
   TmpVec2 = CROSS_PRODUCT( OtherState%RtHS%rT0T(J,:), TmpVec1 )                  ! The portion of MomX0Trbt associated with tower element J
   TmpVec3 = ( OtherState%RtHS%MFAero(J,:) + OtherState%RtHS%MFHydrot(J,:) )*p%DHNodes(J)           ! The external moment applied to tower element J

   OtherState%RtHS%FrcT0Trbt = OtherState%RtHS%FrcT0Trbt + TmpVec1

   OtherState%RtHS%MomX0Trbt = OtherState%RtHS%MomX0Trbt + TmpVec2 + TmpVec3


   ! Integrate to find the portions of the mass matrix on and below the
   !   diagonal associated with purely tower DOFs (these portions can't
   !   be calculated using partial loads).  Also, integrate to find the
   !   portions of the forcing vector associated with purely tower DOFs
   !   (these portions can't be calculated using partial loads).
   ! NOTE: The vector subscript array, PTTE(), used in the following loops must
   !       be sorted from smallest to largest DOF index in order for the loops
   !       to work to enter values only on and below the diagonal of AugMat():

   DO L = 1,p%DOFs%NPTTE    ! Loop through all active (enabled) tower DOFs that contribute to the QD2T-related linear accelerations of the tower
      DO I = L,p%DOFs%NPTTE ! Loop through all active (enabled) tower DOFs greater than or equal to L
         AugMat(p%DOFs%PTTE(I),p%DOFs%PTTE(L)) = AugMat(p%DOFs%PTTE(I),p%DOFs%PTTE(L))  &
                                 + ElmntMass *DOT_PRODUCT( OtherState%RtHS%PLinVelET(J,p%DOFs%PTTE(I),0,:),  &
                                                           OtherState%RtHS%PLinVelET(J,p%DOFs%PTTE(L),0,:) ) &  ! [C(q,t)]T + [C(q,t)]HydroT
                                 - p%DHNodes(J)*DOT_PRODUCT( OtherState%RtHS%PLinVelET(J,p%DOFs%PTTE(I),0,:),  &
                                                           OtherState%RtHS%PFTHydro (J,p%DOFs%PTTE(L),  :) ) &
                                 - p%DHNodes(J)*DOT_PRODUCT( PAngVelEF(J,p%DOFs%PTTE(I),0,:),  &
                                                           OtherState%RtHS%PMFHydro (J,p%DOFs%PTTE(L),  :) )
      ENDDO          ! I - All active (enabled) tower DOFs greater than or equal to L
   ENDDO             ! L - All active (enabled) tower DOFs that contribute to the QD2T-related linear accelerations of the tower
   DO I = 1,p%DOFs%NPTTE    ! Loop through all active (enabled) tower DOFs that contribute to the QD2T-related linear accelerations of the tower
         AugMat(p%DOFs%PTTE(I),   p%NAug) = AugMat(p%DOFs%PTTE(I),   p%NAug)                         &      ! {-f(qd,q,t)}T + {-f(qd,q,t)}GravT + {-f(qd,q,t)}AeroT + {-f(qd,q,t)}HydroT
                                 +            DOT_PRODUCT( OtherState%RtHS%PLinVelET(J,p%DOFs%PTTE(I),0,:), TmpVec1             ) &  ! NOTE: TmpVec1 is still the portion of FrcT0Trbt associated with tower element J
                                 +            DOT_PRODUCT( PAngVelEF(J,p%DOFs%PTTE(I),0,:), TmpVec3             )    !       and TmpVec3 is still the total external moment to tower element J
   ENDDO             ! I - All active (enabled) tower DOFs that contribute to the QD2T-related linear accelerations of the tower

ENDDO ! J - Tower nodes / elements


   ! If the associated DOFs are enabled, add the tower elasticity and damping
   !   forces to the forcing vector (these portions can't be calculated using
   !   partial loads):

IF ( p%DOF_Flag(DOF_TFA1) )  THEN
   AugMat(    DOF_TFA1,p%NAug) = AugMat(DOF_TFA1,p%NAug)                             &
                             - p%KTFA(1,1)*x%QT( DOF_TFA1) - p%KTFA(1,2)*x%QT( DOF_TFA2) &  !
                             - p%CTFA(1,1)*x%QDT(DOF_TFA1) - p%CTFA(1,2)*x%QDT(DOF_TFA2)
ENDIF
IF ( p%DOF_Flag(DOF_TSS1) )  THEN
   AugMat(    DOF_TSS1,p%NAug) = AugMat(DOF_TSS1,p%NAug)                             &
                             - p%KTSS(1,1)*x%QT( DOF_TSS1) - p%KTSS(1,2)*x%QT( DOF_TSS2) &  ! {-f(qd,q,t)}ElasticT + {-f(qd,q,t)}DampT
                             - p%CTSS(1,1)*x%QDT(DOF_TSS1) - p%CTSS(1,2)*x%QDT(DOF_TSS2)
ENDIF
IF ( p%DOF_Flag(DOF_TFA2) )  THEN
   AugMat(    DOF_TFA2,p%NAug) = AugMat(DOF_TFA2,p%NAug)                             &
                             - p%KTFA(2,1)*x%QT( DOF_TFA1) - p%KTFA(2,2)*x%QT( DOF_TFA2) &  !
                             - p%CTFA(2,1)*x%QDT(DOF_TFA1) - p%CTFA(2,2)*x%QDT(DOF_TFA2)
ENDIF
IF ( p%DOF_Flag(DOF_TSS2) )  THEN
   AugMat(    DOF_TSS2,p%NAug) = AugMat(DOF_TSS2,p%NAug)                             &
                             - p%KTSS(2,1)*x%QT( DOF_TSS1) - p%KTSS(2,2)*x%QT( DOF_TSS2) &  !
                             - p%CTSS(2,1)*x%QDT(DOF_TSS1) - p%CTSS(2,2)*x%QDT(DOF_TSS2)
ENDIF



   ! Let's compute the platform loading; that is PtfmAM(1:6,1:6), and
   !   PtfmFt(1:6):

CALL PtfmLoading( t, x, u%PtfmAM, u%PtfmFt )

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

OtherState%RtHS%PFZHydro = 0.0
OtherState%RtHS%PMXHydro = 0.0
DO I = 1,p%DOFs%NPYE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the platform center of mass (point Y)

   OtherState%RtHS%PFZHydro(p%DOFs%PYE(I),:) = - u%PtfmAM(DOF_Sg,p%DOFs%PYE(I))*OtherState%RtHS%PLinVelEZ(DOF_Sg,0,:) &
                                        - u%PtfmAM(DOF_Sw,p%DOFs%PYE(I))*OtherState%RtHS%PLinVelEZ(DOF_Sw,0,:) &
                                        - u%PtfmAM(DOF_Hv,p%DOFs%PYE(I))*OtherState%RtHS%PLinVelEZ(DOF_Hv,0,:)
   OtherState%RtHS%PMXHydro(p%DOFs%PYE(I),:) = - u%PtfmAM(DOF_R ,p%DOFs%PYE(I))*OtherState%RtHS%PAngVelEX(DOF_R ,0,:) &
                                        - u%PtfmAM(DOF_P ,p%DOFs%PYE(I))*OtherState%RtHS%PAngVelEX(DOF_P ,0,:) &
                                        - u%PtfmAM(DOF_Y ,p%DOFs%PYE(I))*OtherState%RtHS%PAngVelEX(DOF_Y ,0,:)

ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the platform center of mass (point Y)

OtherState%RtHS%FZHydrot = u%PtfmFt(DOF_Sg)*OtherState%RtHS%PLinVelEZ(DOF_Sg,0,:) &
         + u%PtfmFt(DOF_Sw)*OtherState%RtHS%PLinVelEZ(DOF_Sw,0,:) &
         + u%PtfmFt(DOF_Hv)*OtherState%RtHS%PLinVelEZ(DOF_Hv,0,:)
OtherState%RtHS%MXHydrot = u%PtfmFt(DOF_R )*OtherState%RtHS%PAngVelEX(DOF_R ,0,:) &
         + u%PtfmFt(DOF_P )*OtherState%RtHS%PAngVelEX(DOF_P ,0,:) &
         + u%PtfmFt(DOF_Y )*OtherState%RtHS%PAngVelEX(DOF_Y ,0,:)



   ! Define the partial forces and moments (including those associated with
   !   the QD2T()'s and those that are not) at the platform reference (point Z)
   !   / (body X) using the turbine and platform effects:

PFrcZAll = OtherState%RtHS%PFrcT0Trb ! Initialize these partial forces and moments
PMomXAll = OtherState%RtHS%PMomX0Trb ! using the effects from the wind turbine
DO I = 1,p%DOFs%NActvDOF ! Loop through all active (enabled) DOFs

   TmpVec = CROSS_PRODUCT( rZT0, OtherState%RtHS%PFrcT0Trb(p%DOFs%SrtPS(I),:) )   ! The portion of PMomXAll associated with the PFrcT0Trb

   PMomXAll(p%DOFs%SrtPS(I),:) = PMomXAll(p%DOFs%SrtPS(I),:) + TmpVec

ENDDO             ! I - All active (enabled) DOFs
DO I = 1,p%DOFs%NPYE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the platform center of mass (point Y)

   TmpVec1 = -p%PtfmMass*PLinVelEY(p%DOFs%PYE(I),0,:)                ! The portion of PFrcZAll associated with the PtfmMass
   TmpVec2 = CROSS_PRODUCT( rZY ,               TmpVec1 )   ! The portion of PMomXAll associated with the PtfmMass

   PFrcZAll(p%DOFs%PYE(I)  ,:) = PFrcZAll(p%DOFs%PYE(I)  ,:) + OtherState%RtHS%PFZHydro(p%DOFs%PYE(I),:) + TmpVec1

   PMomXAll(p%DOFs%PYE(I)  ,:) = PMomXAll(p%DOFs%PYE(I)  ,:) + OtherState%RtHS%PMXHydro(p%DOFs%PYE(I),:) + TmpVec2 &
                        - p%PtfmRIner*OtherState%CoordSys%a1*DOT_PRODUCT( OtherState%CoordSys%a1, OtherState%RtHS%PAngVelEX(p%DOFs%PYE(I),0,:) )   &
                        - p%PtfmYIner*OtherState%CoordSys%a2*DOT_PRODUCT( OtherState%CoordSys%a2, OtherState%RtHS%PAngVelEX(p%DOFs%PYE(I),0,:) )   &
                        - p%PtfmPIner*OtherState%CoordSys%a3*DOT_PRODUCT( OtherState%CoordSys%a3, OtherState%RtHS%PAngVelEX(p%DOFs%PYE(I),0,:) )

ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the platform center of mass (point Y)

TmpVec1 = -p%PtfmMass*( p%Gravity*OtherState%CoordSys%z2 + LinAccEYt  )                   ! The portion of FrcZAllt associated with the PtfmMass
TmpVec2 = CROSS_PRODUCT( rZY      ,   TmpVec1 )                            ! The portion of MomXAllt associated with the PtfmMass
TmpVec3 = CROSS_PRODUCT( rZT0     , OtherState%RtHS%FrcT0Trbt )                            ! The portion of MomXAllt associated with the FrcT0Trbt
TmpVec  = p%PtfmRIner*OtherState%CoordSys%a1*DOT_PRODUCT( OtherState%CoordSys%a1, OtherState%RtHS%AngVelEX  ) &    ! = ( Platform inertia dyadic ) dot ( angular velocity of platform in the inertia frame )
        + p%PtfmYIner*OtherState%CoordSys%a2*DOT_PRODUCT( OtherState%CoordSys%a2, OtherState%RtHS%AngVelEX  ) &
        + p%PtfmPIner*OtherState%CoordSys%a3*DOT_PRODUCT( OtherState%CoordSys%a3, OtherState%RtHS%AngVelEX  )
TmpVec4 = CROSS_PRODUCT( -OtherState%RtHS%AngVelEX,   TmpVec  )                            ! = ( -angular velocity of platform in the inertia frame ) cross ( TmpVec )

FrcZAllt = OtherState%RtHS%FrcT0Trbt + OtherState%RtHS%FZHydrot + TmpVec1

MomXAllt = OtherState%RtHS%MomX0Trbt + OtherState%RtHS%MXHydrot + TmpVec2 + TmpVec3 + TmpVec4



   ! Compute the moments from teeter springs and dampers, rotor-furl springs
   !   and dampers, tail-furl springs and dampers, and the generator and
   !   high-speed shaft brake torque:

CALL Teeter  ( t, p, OtherState%RtHS%TeetAng, OtherState%RtHS%TeetAngVel, TeetMom ) ! Compute moment from teeter     springs and dampers, TeetMom; NOTE: TeetMom will be zero for a 3-blader since TeetAng = TeetAngVel = 0
CALL RFurling( t, p, x%QT(DOF_RFrl),          x%QDT(DOF_RFrl),            RFrlMom ) ! Compute moment from rotor-furl springs and dampers, RFrlMom
CALL TFurling( t, p, x%QT(DOF_TFrl),          x%QDT(DOF_TFrl),            TFrlMom ) ! Compute moment from tail-furl  springs and dampers, TFrlMom



   ! Add the gearbox losses to total HSS torque and project to the LSS side of
   !   the gearbox.  The gearbox efficiency effects, however, are included in FAST.f90/RtHS().
GBoxTrq    = ( u%GenTrq + u%HSSBrTrq )*ABS(p%GBRatio)




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
   DO I = p%DOFs%Diag(DOF_Sg  ),p%DOFs%NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(p%DOFs%SrtPS(I),DOF_Sg  ) = -1.*DOT_PRODUCT( OtherState%RtHS%PLinVelEZ(DOF_Sg  ,0,:), PFrcZAll (p%DOFs%SrtPS(I),:) )    ! [C(q,t)]X + [C(q,t)]HydroX + [C(q,t)]T + [C(q,t)]HydroT + [C(q,t)]N + [C(q,t)]R + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_Sg  ,    p%NAug) =  DOT_PRODUCT( OtherState%RtHS%PLinVelEZ(DOF_Sg  ,0,:), FrcZAllt              )    ! {-f(qd,q,t)}X + {-f(qd,q,t)}HydroX + {-f(qd,q,t)}T + {-f(qd,q,t)}AeroT + {-f(qd,q,t)}HydroT + {-f(qd,q,t)}N + {-f(qd,q,t)}R + {-f(qd,q,t)}H + {-f(qd,q,t)}B + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}AeroA
ENDIF

IF ( p%DOF_Flag (DOF_Sw  ) )  THEN
   DO I = p%DOFs%Diag(DOF_Sw  ),p%DOFs%NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(p%DOFs%SrtPS(I),DOF_Sw  ) = -1.*DOT_PRODUCT( OtherState%RtHS%PLinVelEZ(DOF_Sw  ,0,:), PFrcZAll (p%DOFs%SrtPS(I),:) )    ! [C(q,t)]X + [C(q,t)]HydroX + [C(q,t)]T + [C(q,t)]HydroT + [C(q,t)]N + [C(q,t)]R + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_Sw  ,    p%NAug) =  DOT_PRODUCT( OtherState%RtHS%PLinVelEZ(DOF_Sw  ,0,:), FrcZAllt              )    ! {-f(qd,q,t)}X + {-f(qd,q,t)}HydroX + {-f(qd,q,t)}T + {-f(qd,q,t)}AeroT + {-f(qd,q,t)}HydroT + {-f(qd,q,t)}N + {-f(qd,q,t)}R + {-f(qd,q,t)}H + {-f(qd,q,t)}B + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}AeroA
ENDIF

IF ( p%DOF_Flag (DOF_Hv  ) )  THEN
   DO I = p%DOFs%Diag(DOF_Hv  ),p%DOFs%NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(p%DOFs%SrtPS(I),DOF_Hv  ) = -1.*DOT_PRODUCT( OtherState%RtHS%PLinVelEZ(DOF_Hv  ,0,:), PFrcZAll (p%DOFs%SrtPS(I),:) )    ! [C(q,t)]X + [C(q,t)]HydroX + [C(q,t)]T + [C(q,t)]HydroT + [C(q,t)]N + [C(q,t)]R + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_Hv  ,    p%NAug) =  DOT_PRODUCT( OtherState%RtHS%PLinVelEZ(DOF_Hv  ,0,:), FrcZAllt              )    ! {-f(qd,q,t)}X + {-f(qd,q,t)}GravX + {-f(qd,q,t)}HydroX + {-f(qd,q,t)}T + {-f(qd,q,t)}GravT + {-f(qd,q,t)}AeroT + {-f(qd,q,t)}HydroT + {-f(qd,q,t)}N + {-f(qd,q,t)}GravN + {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
ENDIF

IF ( p%DOF_Flag (DOF_R   ) )  THEN
   DO I = p%DOFs%Diag(DOF_R   ),p%DOFs%NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal

      AugMat(p%DOFs%SrtPS(I),DOF_R   ) = -1.*DOT_PRODUCT( OtherState%RtHS%PAngVelEX(DOF_R   ,0,:), PMomXAll (p%DOFs%SrtPS(I),:) )    ! [C(q,t)]X + [C(q,t)]HydroX + [C(q,t)]T + [C(q,t)]HydroT + [C(q,t)]N + [C(q,t)]R + [C(q,t)]G + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_R   ,    p%NAug) =  DOT_PRODUCT( OtherState%RtHS%PAngVelEX(DOF_R   ,0,:), MomXAllt              )    ! {-f(qd,q,t)}X + {-f(qd,q,t)}GravX + {-f(qd,q,t)}HydroX + {-f(qd,q,t)}T + {-f(qd,q,t)}GravT + {-f(qd,q,t)}AeroT + {-f(qd,q,t)}HydroT + {-f(qd,q,t)}N + {-f(qd,q,t)}GravN + {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}G + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
ENDIF

IF ( p%DOF_Flag (DOF_P   ) )  THEN
   DO I = p%DOFs%Diag(DOF_P   ),p%DOFs%NActvDOF    ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(p%DOFs%SrtPS(I),DOF_P   ) = -1.*DOT_PRODUCT( OtherState%RtHS%PAngVelEX(DOF_P  ,0,:), PMomXAll (p%DOFs%SrtPS(I),:) ) ! [C(q,t)]X + [C(q,t)]HydroX + [C(q,t)]T + [C(q,t)]HydroT + [C(q,t)]N + [C(q,t)]R + [C(q,t)]G + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
   ENDDO                                                             ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_P                   ,p%NAug  ) =     DOT_PRODUCT( OtherState%RtHS%PAngVelEX(DOF_P  ,0,:), MomXAllt              )                 ! {-f(qd,q,t)}X + {-f(qd,q,t)}GravX + {-f(qd,q,t)}HydroX + {-f(qd,q,t)}T + {-f(qd,q,t)}GravT + {-f(qd,q,t)}AeroT + {-f(qd,q,t)}HydroT + {-f(qd,q,t)}N + {-f(qd,q,t)}GravN + {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}G + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
END IF

IF ( p%DOF_Flag (DOF_Y   ) )  THEN
   DO I = p%DOFs%Diag(DOF_Y   ),p%DOFs%NActvDOF    ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(p%DOFs%SrtPS(I),DOF_Y   ) = -1.*DOT_PRODUCT( OtherState%RtHS%PAngVelEX(DOF_Y   ,0,:), PMomXAll (p%DOFs%SrtPS(I),:) )    ! [C(q,t)]X + [C(q,t)]HydroX + [C(q,t)]T + [C(q,t)]HydroT + [C(q,t)]N + [C(q,t)]R + [C(q,t)]G + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
   ENDDO                                                             ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_Y   ,    p%NAug) =     DOT_PRODUCT( OtherState%RtHS%PAngVelEX(DOF_Y   ,0,:), MomXAllt              )    ! {-f(qd,q,t)}X + {-f(qd,q,t)}GravX + {-f(qd,q,t)}HydroX + {-f(qd,q,t)}T + {-f(qd,q,t)}GravT + {-f(qd,q,t)}AeroT + {-f(qd,q,t)}HydroT + {-f(qd,q,t)}N + {-f(qd,q,t)}GravN + {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}G + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
ENDIF

IF ( p%DOF_Flag (DOF_TFA1) )  THEN
   DO I = p%DOFs%Diag(DOF_TFA1),p%DOFs%NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(p%DOFs%SrtPS(I),DOF_TFA1) = AugMat(p%DOFs%SrtPS(I),DOF_TFA1)                                  &
                                -  DOT_PRODUCT( OtherState%RtHS%PLinVelEO(DOF_TFA1,0,:), OtherState%RtHS%PFrcONcRt(p%DOFs%SrtPS(I),:) ) &  ! [C(q,t)]N + [C(q,t)]R + [C(q,t)]G + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
                                -  DOT_PRODUCT( OtherState%RtHS%PAngVelEB(DOF_TFA1,0,:), OtherState%RtHS%PMomBNcRt(p%DOFs%SrtPS(I),:) )
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_TFA1,    p%NAug) = AugMat(DOF_TFA1,    p%NAug)                                  &
                                +  DOT_PRODUCT( OtherState%RtHS%PLinVelEO(DOF_TFA1,0,:), OtherState%RtHS%FrcONcRtt             ) &  ! {-f(qd,q,t)}N + {-f(qd,q,t)}GravN + {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}G + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
                                +  DOT_PRODUCT( OtherState%RtHS%PAngVelEB(DOF_TFA1,0,:), OtherState%RtHS%MomBNcRtt             )
ENDIF

IF ( p%DOF_Flag (DOF_TSS1) )  THEN
   DO I = p%DOFs%Diag(DOF_TSS1),p%DOFs%NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(p%DOFs%SrtPS(I),DOF_TSS1) = AugMat(p%DOFs%SrtPS(I),DOF_TSS1)                                  &
                                -  DOT_PRODUCT( OtherState%RtHS%PLinVelEO(DOF_TSS1,0,:), OtherState%RtHS%PFrcONcRt(p%DOFs%SrtPS(I),:) ) &  ! [C(q,t)]N + [C(q,t)]R + [C(q,t)]G + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
                                -  DOT_PRODUCT( OtherState%RtHS%PAngVelEB(DOF_TSS1,0,:), OtherState%RtHS%PMomBNcRt(p%DOFs%SrtPS(I),:) )
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_TSS1,    p%NAug) = AugMat(DOF_TSS1,    p%NAug)                                  &
                                +  DOT_PRODUCT( OtherState%RtHS%PLinVelEO(DOF_TSS1,0,:), OtherState%RtHS%FrcONcRtt             ) &  ! {-f(qd,q,t)}N + {-f(qd,q,t)}GravN + {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}G + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
                                +  DOT_PRODUCT( OtherState%RtHS%PAngVelEB(DOF_TSS1,0,:), OtherState%RtHS%MomBNcRtt             )
ENDIF

IF ( p%DOF_Flag (DOF_TFA2) )  THEN
   DO I = p%DOFs%Diag(DOF_TFA2),p%DOFs%NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(p%DOFs%SrtPS(I),DOF_TFA2) = AugMat(p%DOFs%SrtPS(I),DOF_TFA2)                                  &
                                -  DOT_PRODUCT( OtherState%RtHS%PLinVelEO(DOF_TFA2,0,:), OtherState%RtHS%PFrcONcRt(p%DOFs%SrtPS(I),:) ) &  ! [C(q,t)]N + [C(q,t)]R + [C(q,t)]G + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
                                -  DOT_PRODUCT( OtherState%RtHS%PAngVelEB(DOF_TFA2,0,:), OtherState%RtHS%PMomBNcRt(p%DOFs%SrtPS(I),:) )
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_TFA2,    p%NAug) = AugMat(DOF_TFA2,    p%NAug)                                  &
                                +  DOT_PRODUCT( OtherState%RtHS%PLinVelEO(DOF_TFA2,0,:), OtherState%RtHS%FrcONcRtt             ) &  ! {-f(qd,q,t)}N + {-f(qd,q,t)}GravN + {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}G + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
                                +  DOT_PRODUCT( OtherState%RtHS%PAngVelEB(DOF_TFA2,0,:), OtherState%RtHS%MomBNcRtt             )
ENDIF

IF ( p%DOF_Flag (DOF_TSS2) )  THEN
   DO I = p%DOFs%Diag(DOF_TSS2),p%DOFs%NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(p%DOFs%SrtPS(I),DOF_TSS2) = AugMat(p%DOFs%SrtPS(I),DOF_TSS2)                                  &
                                -  DOT_PRODUCT( OtherState%RtHS%PLinVelEO(DOF_TSS2,0,:), OtherState%RtHS%PFrcONcRt(p%DOFs%SrtPS(I),:) ) &  ! [C(q,t)]N + [C(q,t)]R + [C(q,t)]G + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
                                -  DOT_PRODUCT( OtherState%RtHS%PAngVelEB(DOF_TSS2,0,:), OtherState%RtHS%PMomBNcRt(p%DOFs%SrtPS(I),:) )
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_TSS2,    p%NAug) = AugMat(DOF_TSS2,    p%NAug)                                  &
                                +  DOT_PRODUCT( OtherState%RtHS%PLinVelEO(DOF_TSS2,0,:), OtherState%RtHS%FrcONcRtt             ) &  ! {-f(qd,q,t)}N + {-f(qd,q,t)}GravN + {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}G + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
                                +  DOT_PRODUCT( OtherState%RtHS%PAngVelEB(DOF_TSS2,0,:), OtherState%RtHS%MomBNcRtt             )
ENDIF

IF ( p%DOF_Flag (DOF_Yaw ) )  THEN
   DO I = p%DOFs%Diag(DOF_Yaw ),p%DOFs%NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(p%DOFs%SrtPS(I),DOF_Yaw ) = -DOT_PRODUCT( PAngVelEN(DOF_Yaw ,0,:), OtherState%RtHS%PMomBNcRt(p%DOFs%SrtPS(I),:) )    ! [C(q,t)]N + [C(q,t)]R + [C(q,t)]G + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
      !YawMom = -  p%YawSpr *( x%QT (DOF_Yaw) - YawNeut     )                    &  ! + {-f(qd,q,t)}SpringYaw
      !         -  p%YawDamp*( x%QDT(DOF_Yaw) - YawRateNeut )                       ! + {-f(qd,q,t)}DampYaw; NOTE: The neutral yaw rate, YawRateNeut, defaults to zero.  It is only used for yaw control.
      AugMat(DOF_Yaw ,    p%NAug) =  DOT_PRODUCT( PAngVelEN(DOF_Yaw ,0,:), OtherState%RtHS%MomBNcRtt             ) &  ! {-f(qd,q,t)}N + {-f(qd,q,t)}GravN + {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}G + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
                                    + y_SrvD%YawMom
ENDIF

IF ( p%DOF_Flag (DOF_RFrl) )  THEN
   DO I = p%DOFs%Diag(DOF_RFrl),p%DOFs%NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(p%DOFs%SrtPS(I),DOF_RFrl) = -DOT_PRODUCT( OtherState%RtHS%PAngVelER(DOF_RFrl,0,:), OtherState%RtHS%PMomNGnRt(p%DOFs%SrtPS(I),:) )    ! [C(q,t)]R + [C(q,t)]G + [C(q,t)]H + [C(q,t)]B
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_RFrl,    p%NAug) =  DOT_PRODUCT( OtherState%RtHS%PAngVelER(DOF_RFrl,0,:), OtherState%RtHS%MomNGnRtt             ) &  ! {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}G + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB
                                +  RFrlMom                                                      ! + {-f(qd,q,t)}SpringRF + {-f(qd,q,t)}DampRF
ENDIF

TmpVec = p%GenIner*OtherState%CoordSys%c1*DOT_PRODUCT( OtherState%CoordSys%c1, PAngVelEG(DOF_GeAz,0,:) )  ! = ( generator inertia dyadic ) Dot ( partial angular velocity of G in E for DOF_GeAz )

IF ( p%DOF_Flag (DOF_GeAz) )  THEN
   DO I = p%DOFs%Diag(DOF_GeAz),p%DOFs%NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(p%DOFs%SrtPS(I),DOF_GeAz) = -DOT_PRODUCT( PAngVelEL(DOF_GeAz,0,:), OtherState%RtHS%PMomLPRot(p%DOFs%SrtPS(I),:) )    ! [C(q,t)]H + [C(q,t)]B
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_GeAz,    p%NAug) =  DOT_PRODUCT( PAngVelEL(DOF_GeAz,0,:), OtherState%RtHS%MomLPRott             ) &  ! {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB
                                -  GBoxTrq                                                      ! + {-f(qd,q,t)}Gen + {-f(qd,q,t)}Brake


   ! The previous loop (DO I = p%DOFs%Diag(DOF_GeAz),p%DOFs%NActvDOF) misses the
   !   generator inertia-contribution to the mass matrix and forcing function.
   !   Thus, add these in as well:


      AugMat(DOF_GeAz,DOF_GeAz) = AugMat(DOF_GeAz,DOF_GeAz)                                  &
                                +  DOT_PRODUCT( PAngVelEG(DOF_GeAz,0,:), TmpVec                )    ! [C(q,t)]G
      AugMat(DOF_GeAz,    p%NAug) = AugMat(DOF_GeAz,    p%NAug)                                  &
                                -  DOT_PRODUCT( AngAccEGt              , TmpVec                )    ! {-f(qd,q,t)}G


ENDIF

IF ( p%DOF_Flag (DOF_DrTr) )  THEN
   DO I = p%DOFs%Diag(DOF_DrTr),p%DOFs%NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(p%DOFs%SrtPS(I),DOF_DrTr) = -DOT_PRODUCT( PAngVelEL(DOF_DrTr,0,:), OtherState%RtHS%PMomLPRot(p%DOFs%SrtPS(I),:) )    ! [C(q,t)]H + [C(q,t)]B
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_DrTr,    p%NAug) =  DOT_PRODUCT( PAngVelEL(DOF_DrTr,0,:), OtherState%RtHS%MomLPRott             ) &  ! {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB
                                -  p%DTTorSpr*x%QT (DOF_DrTr)                                    &  ! + {-f(qd,q,t)}ElasticDrive
                                -  p%DTTorDmp*x%QDT(DOF_DrTr)                                       ! + {-f(qd,q,t)}DampDrive
ENDIF

IF ( p%DOF_Flag (DOF_TFrl) )  THEN
   ! The tail-furl DOF does not affect any DOF index larger than DOF_TFrl.  Therefore, there is no need to perform the loop: DO I = Diag(DOF_TFrl),NActvDOF
      AugMat(DOF_TFrl,DOF_TFrl) = -DOT_PRODUCT( PAngVelEA(DOF_TFrl,0,:), OtherState%RtHS%PMomNTail(DOF_TFrl,:) )    ! [C(q,t)]A
      AugMat(DOF_TFrl,    p%NAug) =  DOT_PRODUCT( PAngVelEA(DOF_TFrl,0,:), OtherState%RtHS%MomNTailt             ) &  ! {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
                                +  TFrlMom                                                      ! + {-f(qd,q,t)}SpringTF + {-f(qd,q,t)}DampTF
ENDIF

IF ( ( p%NumBl == 2 ) .AND. ( p%DOF_Flag(DOF_Teet) ) )  THEN
   ! The teeter DOF does not affect any DOF index larger than DOF_Teet.  Therefore, there is no need to perform the loop: DO I = Diag(DOF_Teet),NActvDOF
      AugMat(DOF_Teet,DOF_Teet) = -DOT_PRODUCT( PAngVelEH(DOF_Teet,0,:), OtherState%RtHS%PMomLPRot(DOF_Teet,:) )    ! [C(q,t)]H + [C(q,t)]B
      AugMat(DOF_Teet,    p%NAug) =  DOT_PRODUCT( PAngVelEH(DOF_Teet,0,:), OtherState%RtHS%MomLPRott             ) &  ! {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB
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

DO L = 2,p%DOFs%NActvDOF ! Loop through all active (enabled) DOFs above the diagonal (columns)
   DO I = 1,L-1   ! Loop through all active (enabled) DOFs above the diagonal (rows)
      AugMat(p%DOFs%SrtPS(I),p%DOFs%SrtPS(L)) = AugMat(p%DOFs%SrtPS(L),p%DOFs%SrtPS(I))
   ENDDO          ! I - All active (enabled) DOFs above the diagonal (rows)
ENDDO             ! L - All active (enabled) DOFs above the diagonal (columns)



   ! Let's add the gearbox friction terms to the mass matrix and forcing
   !   function.  These only effect the equation for the generator azimuth
   !   DOF.
   ! NOTE: the MASS MATRIX WILL NO LONGER BE SYMMETRIC after adding these
   !       terms, unless the gearbox efficiency, GBoxEff, was set to 100%:

OtherState%RtHS%GBoxEffFac  = p%GBoxEff**SgnPrvLSTQ      ! = GBoxEff if SgnPrvLSTQ = 1 OR 1/GBoxEff if SgnPrvLSTQ = -1
GBoxEffFac2 = ( 1.0/OtherState%RtHS%GBoxEffFac - 1.0 ) ! = ( 1 / GBoxEff^SgnPrvLSTQ - 1 )

DO I = 1,p%DOFs%NActvDOF ! Loop through all active (enabled) DOFs

   AugMat(DOF_GeAz,p%DOFs%SrtPS(I)) = AugMat(DOF_GeAz,p%DOFs%SrtPS(I)) &                                            ! NOTE: TmpVec is still = ( generator inertia dyadic ) Dot ( partial angular velocity of G in E for DOF_GeAz ) in the following equation
                             + GBoxEffFac2*  DOT_PRODUCT( PAngVelEG(p%DOFs%SrtPS(I),0,:), TmpVec )               ! [C(q,t)]GBFric

ENDDO             ! I - All active (enabled) DOFs

AugMat(   DOF_GeAz,    p%NAug) = AugMat(DOF_GeAz,    p%NAug) &                                            ! NOTE: TmpVec is still = ( generator inertia dyadic ) Dot ( partial angular velocity of G in E for DOF_GeAz ) in the following equation
                             - GBoxEffFac2*( DOT_PRODUCT( AngAccEGt              , TmpVec ) + GBoxTrq )   ! {-f(qd,q,t)}GBFric



IF ( PRESENT( AugMatOut ) ) THEN
   AugMatOut  = AugMat
END IF



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

CALL GaussElim( AugMat( p%DOFs%SrtPS    (1: p%DOFs%NActvDOF   ),     &
                        p%DOFs%SrtPSNAUG(1:(p%DOFs%NActvDOF+1)) ),   &
                                                     p%DOFs%NActvDOF, SolnVec, ErrStat, ErrMsg )
!IF ( ErrStat /= ErrID_None ) CALL WrScr( ' Message from RtHS: '//TRIM(ErrMsg) )



OtherState%QD2T = 0.0
DO I = 1,p%DOFs%NActvDOF ! Loop through all active (enabled) DOFs
   OtherState%QD2T(p%DOFs%SrtPS(I)) = SolnVec(I)
ENDDO             ! I - All active (enabled) DOFs



   ! Lets calculate the sign (+/-1) of the low-speed shaft torque for
   !   this time step and store it in SgnPrvLSTQ.  This will be used
   !   during the next call to RtHS.  MomLPRot is the moment on the
   !   low-speed shaft at the teeter pin caused by the rotor.

MomLPRot = OtherState%RtHS%MomLPRott ! Initialize MomLPRot using MomLPRott
DO I = 1,p%DOFs%NActvDOF ! Loop through all active (enabled) DOFs

   MomLPRot = MomLPRot + OtherState%RtHS%PMomLPRot(p%DOFs%SrtPS(I),:)*OtherState%QD2T(p%DOFs%SrtPS(I))  ! Add the moments associated with the accelerations of the DOFs

ENDDO             ! I - All active (enabled) DOFs

   ! MomLProt has now been found.  Now dot this with e1 to get the
   !   low-speed shaft torque and take the SIGN of the result:

SgnPrvLSTQ = NINT( SIGN( 1.0, DOT_PRODUCT( MomLPRot, OtherState%CoordSys%e1 ) ) )



RETURN

END SUBROUTINE RtHS
!----------------------------------------------------------------------------------------------------------------------------------  

!=======================================================================
SUBROUTINE Solver( t, n, p, x, y, OtherState, u, p_SrvD, y_SrvD, u_SrvD, OtherState_SrvD )


   ! Solver solves the equations of motion by marching in time using a
   !   predictor-corrector scheme.  Fourth order Runge-Kutta is used to
   !   get the first 4 points from the initial degrees of freedom and
   !   velocities.


IMPLICIT                        NONE


   ! Subroutine arguments (Passed variables):

REAL(DbKi), INTENT(IN) :: t
INTEGER(IntKi), INTENT(IN) :: n

TYPE(SrvD_ParameterType),    INTENT(IN)       :: p_SrvD                      ! The parameters of the ServoDyn module
TYPE(SrvD_OutputType),       INTENT(INOUT)    :: y_SrvD                      ! The outputs of the ServoDyn module
TYPE(SrvD_InputType),        INTENT(INOUT)    :: u_SrvD                      ! System inputs of the ServoDyn module 
TYPE(SrvD_OtherStateType),   INTENT(INOUT)    :: OtherState_SrvD             ! The ServoDyn "other" states


TYPE(ED_ParameterType),      INTENT(IN)       :: p                           ! The parameters of the structural dynamics module
TYPE(ED_ContinuousStateType),INTENT(INOUT)    :: x                           ! The structural dynamics module's continuous states
TYPE(ED_OtherStateType),     INTENT(INOUT)    :: OtherState                  ! The structural dynamics "other" states (including CoordSys coordinate systems)
TYPE(ED_OutputType),         INTENT(INOUT)    :: y                           ! System outputs of the structural dynamics module
TYPE(ED_InputType),          INTENT(INOUT)    :: u                           ! System inputs of the structural dynamics module 


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

REAL(ReKi)                   :: AugMat   (p%NDOF,p%NAug)                        ! The augmented matrix used for the solution of the QD2T()s.


IF ( n < 3 )  THEN   ! Use Runge-Kutta integration at the the start of the simulation (first 3 steps).


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

   x%QT  = OtherState%Q (:,OtherState%IC(1))
   x%QDT = OtherState%QD(:,OtherState%IC(1))

   CALL RtHS( t, p, x, OtherState, u, y, p_SrvD, y_SrvD, u_SrvD, OtherState_SrvD )

   ! Compute intermediate functions to estimate next Q and QD.

   DO I = 1,p%NDOF  ! Loop through all DOFs
      ZK1 (I) = p%DT*OtherState%QD  (I,OtherState%IC(1))
      ZK1D(I) = p%DT*OtherState%QD2T(I)

      x%QT  (I) = OtherState%Q (I,OtherState%IC(1)) + 0.5*ZK1 (I)
      x%QDT (I) = OtherState%QD(I,OtherState%IC(1)) + 0.5*ZK1D(I)
   ENDDO          ! I - All DOFs


   CALL RtHS( t, p, x, OtherState, u, y, p_SrvD, y_SrvD, u_SrvD, OtherState_SrvD )


   ! Repeat above steps for each ZK, ZKD:

   DO I = 1,p%NDOF  ! Loop through all DOFs
      ZK2 (I) = p%dt*( OtherState%QD  (I,OtherState%IC(1)) + 0.5*ZK1D(I) )
      ZK2D(I) = p%dt*  OtherState%QD2T(I)

      x%QT  (I) = OtherState%Q (I,OtherState%IC(1)) + 0.5*ZK2 (I)
      x%QDT (I) = OtherState%QD(I,OtherState%IC(1)) + 0.5*ZK2D(I)
   ENDDO          ! I - All DOFs


   CALL RtHS( t, p, x, OtherState, u, y, p_SrvD, y_SrvD, u_SrvD, OtherState_SrvD )


   DO I = 1,p%NDOF  ! Loop through all DOFs
      ZK3 (I) = p%dt*( OtherState%QD  (I,OtherState%IC(1)) + 0.5*ZK2D(I) )
      ZK3D(I) = p%dt*  OtherState%QD2T(I)

      x%QT  (I) = OtherState%Q (I,OtherState%IC(1)) + ZK3 (I)
      x%QDT (I) = OtherState%QD(I,OtherState%IC(1)) + ZK3D(I)
   ENDDO          ! I - All DOFs


   CALL RtHS( t, p, x, OtherState, u, y, p_SrvD, y_SrvD, u_SrvD, OtherState_SrvD )


   ! Compute best estimate for Q, QD at next time step using
   !   the intermediate functions (Runge-Kutta).
   ! IC(NMX) locates the i + 1 value of Q, QD.

   DO I = 1,p%NDOF  ! Loop through all DOFs
      ZK4 (I) = p%DT*( OtherState%QD  (I,OtherState%IC(1)) + ZK3D(I) )
      ZK4D(I) = p%DT*  OtherState%QD2T(I)

      OtherState%Q (I,OtherState%IC(NMX)) = OtherState%Q (I,OtherState%IC(1)) + ( ZK1 (I) + 2.0*ZK2 (I) + 2.0*ZK3 (I) + ZK4 (I) ) / 6.0
      OtherState%QD(I,OtherState%IC(NMX)) = OtherState%QD(I,OtherState%IC(1)) + ( ZK1D(I) + 2.0*ZK2D(I) + 2.0*ZK3D(I) + ZK4D(I) ) / 6.0
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
      OtherState%Q (I,OtherState%IC(NMX)) = OtherState%Q (I,OtherState%IC(1)) + p%DT24*( 55.0*OtherState%QD (I,OtherState%IC(1)) &
                                                                                       - 59.0*OtherState%QD (I,OtherState%IC(2)) &
                                                                                       + 37.0*OtherState%QD (I,OtherState%IC(3)) &
                                                                                       -  9.0*OtherState%QD (I,OtherState%IC(4)) )
      OtherState%QD(I,OtherState%IC(NMX)) = OtherState%QD(I,OtherState%IC(1)) + p%DT24*( 55.0*OtherState%QD2(I,OtherState%IC(1)) &
                                                                                       - 59.0*OtherState%QD2(I,OtherState%IC(2)) &
                                                                                       + 37.0*OtherState%QD2(I,OtherState%IC(3)) &
                                                                                       -  9.0*OtherState%QD2(I,OtherState%IC(4)) )
   ENDDO          ! I - All DOFs

   x%QT  = OtherState%Q (:,OtherState%IC(NMX))
   x%QDT = OtherState%QD(:,OtherState%IC(NMX))

   CALL RtHS( t, p, x, OtherState, u, y, p_SrvD, y_SrvD, u_SrvD, OtherState_SrvD, AugMat )
   

   OtherState%QD2(:,OtherState%IC(NMX)) = OtherState%QD2T


   ! Corrector (Adams-Moulton)

   ! Compute corrector from predictor value of Q, QD (IC(1)) and 3
   !   previous values of Q, QD, and QD2().  IC(1) = i, IC(2) = i-1,
   !   IC(3) = i-2 etc...

   DO I = 1,p%NDOF  ! Loop through all DOFs
      OtherState%Q (I,OtherState%IC(NMX)) = OtherState%Q (I,OtherState%IC(1)) + p%DT24*( 9.0*OtherState%QD (I,OtherState%IC(NMX)) &
                                                                                      + 19.0*OtherState%QD (I,OtherState%IC(1  )) &
                                                                                      -  5.0*OtherState%QD (I,OtherState%IC(2  )) &
                                                                                      +      OtherState%QD (I,OtherState%IC(3  )) )
      OtherState%QD(I,OtherState%IC(NMX)) = OtherState%QD(I,OtherState%IC(1)) + p%DT24*( 9.0*OtherState%QD2(I,OtherState%IC(NMX)) &
                                                                                      + 19.0*OtherState%QD2(I,OtherState%IC(1  )) &
                                                                                      -  5.0*OtherState%QD2(I,OtherState%IC(2  )) & 
                                                                                      +      OtherState%QD2(I,OtherState%IC(3  )) )
   ENDDO          ! I - All DOFs


    ! Make sure the HSS brake has not reversed the direction of the HSS:

   IF ( p%DOF_Flag(DOF_GeAz) .AND. ( t > p_SrvD%THSSBrDp ) )  CALL FixHSSBrTq ( 'Corrector', p, OtherState, AugMat, u%HSSBrTrq )


ENDIF


   ! Compute the final value of QD2T from the best estimates for Q and
   !   QD, last call to RtHS:

x%QT  = OtherState%Q (:,OtherState%IC(NMX))
x%QDT = OtherState%QD(:,OtherState%IC(NMX))

CALL RtHS( t, p, x, OtherState, u, y, p_SrvD, y_SrvD, u_SrvD, OtherState_SrvD, AugMat )

OtherState%QD2(:,OtherState%IC(NMX)) = OtherState%QD2T


   ! Update IC() index so IC(1) is the location of current Q values.

OtherState%IC = CSHIFT( OtherState%IC, -1 ) ! circular shift of all values to the right 



   ! Make sure the HSS brake will not reverse the direction of the HSS
   !   for the next time step.  Do this by computing the predicted value
   !   of QD(DOF_GeAz,IC(NMX)) as will be done during the next time step.
   ! Only do this after the first few time steps since it doesn't work
   !   for the Runga-Kutta integration scheme.

IF ( p%DOF_Flag(DOF_GeAz) .AND. ( t > p_SrvD%THSSBrDp ) .AND. ( n >= 3 ) )  THEN

   OtherState%QD(DOF_GeAz,OtherState%IC(NMX)) = OtherState%QD(DOF_GeAz,OtherState%IC(1)) &
                                                      + p%DT24*(   55.0*OtherState%QD2(DOF_GeAz,OtherState%IC(1)) &
                                                                 - 59.0*OtherState%QD2(DOF_GeAz,OtherState%IC(2)) &
                                                                 + 37.0*OtherState%QD2(DOF_GeAz,OtherState%IC(3)) &
                                                                 -  9.0*OtherState%QD2(DOF_GeAz,OtherState%IC(4))   )

   CALL FixHSSBrTq ( 'Predictor', p, OtherState, AugMat, u%HSSBrTrq )

ENDIF



RETURN
END SUBROUTINE Solver
!=======================================================================
SUBROUTINE TwrLoading ( t, JNode, X1 , X2 , X3 , X4 , X5 , X6 , &
                               XD1, XD2, XD3, XD4, XD5, XD6, p, TwrAM, TwrFt    )


   ! This routine computes the tower hydrodynamic loading; that is
   !   TwrAM(1:6,1:6) and TwrFt(1:6).


USE                             HydroDyn_Types


IMPLICIT                        NONE


   ! Passed Variables:
REAL(DbKi), INTENT(IN )      :: t
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

TYPE(ED_ParameterType), INTENT(IN)  :: p                                      ! Parameters of the structural dynamics module;  bjj: remove this in new framework

REAL(ReKi), INTENT(OUT )     :: TwrAM     (6,6)                                 ! Added mass matrix of the current tower element per unit length.
REAL(ReKi), INTENT(OUT )     :: TwrFt     (6)                                   ! The surge/xi (1), sway/yi (2), and heave/zi (3)-components of the portion of the tower force at the current tower element (point T) and the roll/xi (4), pitch/yi (5), and yaw/zi (6)-components of the portion of the tower moment acting at the current tower element (body F) / (point T) per unit length associated with everything but the QD2T()'s.


   ! Local variables:

!REAL(ReKi), PARAMETER        :: SymTol   = 9.999E-4                             ! Tolerance used to determine if matrix PtfmAM is symmetric.
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

TwrAM = 0.0_ReKi 
TwrFt = 0.0_ReKi

   ! Compute the tower hydrodynamic loading for the current tower node /
   !   element:



   IF ( p_FAST%CompUserTwrLd ) THEN  ! Are we getting additional loads from UserTwrLd?

         ! CALL the user-defined tower loading model:

      CALL UserTwrLd ( JNode, X, XD, t, p_FAST%DirRoot, TwrAM, TwrFt )

      ! Ensure that the tower element added mass matrix returned by UserTwrLd,
      !   TwrAM, is symmetric; Abort if necessary:
      IF (.NOT. IsSymmetric( TwrAM ) ) THEN
         CALL ProgAbort ( ' The user-defined tower element added mass matrix is unsymmetric.'// &
                          '  Make sure TwrAM returned by UserTwrLd() is symmetric.'               )
      END IF

   ELSE
      
   ! Set TwrAM and TwrFt to 0

      TwrAM = 0.0
      TwrFt = 0.0

   END IF
   


IF ( p_FAST%CompHydro .AND. HD_TwrNodes ) THEN 
   TwrAM      = TwrAM      + HD_AllLoads%Substructure(JNode)%AddedMass
   TwrFt(1:3) = TwrFt(1:3) + HD_AllLoads%Substructure(JNode)%Force
   TwrFt(4:6) = TwrFt(4:6) + HD_AllLoads%Substructure(JNode)%Moment

!   CALL MorisonTwrLd ( JNode, DiamT(JNode), CAT(JNode), CDT(JNode), X, XD, t, TwrAM, TwrFt )
END IF





RETURN
END SUBROUTINE TwrLoading
!=======================================================================
END MODULE FASTSubs
