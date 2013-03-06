SUBROUTINE GetFurl( InputFileData, p, UnEc )


   ! This routine reads in the FAST furling input parameters from
   !   FurlFile and validates the input.


USE                             General
USE                             InitCond
USE                             TailAero


IMPLICIT                        NONE

   ! Passed variables
TYPE(ED_InputFile),     INTENT(INOUT)    :: InputFileData                   ! Data stored in the module's input file
TYPE(ED_ParameterType), INTENT(INOUT)    :: p                               ! The module's parameters
INTEGER(IntKi),           INTENT(IN)       :: UnEc                            ! I/O unit for echo file. If present and > 0, write to UnEc


   ! Local variables:

INTEGER(4)                   :: IOS                                           ! I/O status returned from the read statement.
INTEGER                      :: I  ! loop counter


   ! Open the FAST furling input file:

CALL OpenFInpFile ( UnIn, FurlFile )


   ! Add a separator to the echo file if appropriate.

IF ( UnEc > 0 )  WRITE (UnEc,'(//,A,/)')  'Tail Fin Aerodynamics input data from file "'//TRIM( FurlFile )//'":'



!  -------------- HEADER -------------------------------------------------------
! BJJ: This file was read by the Structural Dynamics module, so we'll skip what 
! was already read. We're only going to read the tail fin aerodynamics, which 
! is a hack for now. 
! And we're not going to echo the first 69 lines, either (we already did). 

DO I = 1,69
   CALL ReadCom ( UnIn, FurlFile, 'tail fin aerodynamics'  )
END DO

!  -------------- TAIL FIN AERODYNAMICS ----------------------------------------


   ! Skip the comment line.

CALL ReadCom ( UnIn, FurlFile, 'tail fin aerodynamics', UnEc=UnEc  )


   ! TFinMod - Tail fin aerodynamics model switch.

CALL ReadVar ( UnIn, FurlFile, TFinMod, 'TFinMod', 'Tail fin aerodynamics model switch', UnEc=UnEc  )

IF ( ( TFinMod /= 0 ) .AND. ( TFinMod /= 1 ) .AND. ( TFinMod /= 2 ) )  CALL ProgAbort ( ' TFinMod must be 0, 1, or 2.' )


   ! TFinNFoil - Tail fin airfoil number.

CALL ReadVar ( UnIn, FurlFile, TFinNFoil, 'TFinNFoil', 'Tail fin airfoil number', UnEc=UnEc  )


   ! TFinArea - Tail fin planform area.

CALL ReadVar ( UnIn, FurlFile, TFinArea, 'TFinArea', 'Tail fin planform area', UnEc=UnEc  )

IF ( TFinArea < 0.0 )  CALL ProgAbort ( ' TFinArea must not be negative.' )


   ! SubAxInd - Subtract rotor axial induction?

CALL ReadVar ( UnIn, FurlFile, SubAxInd, 'SubAxInd', 'Subtract rotor axial induction?', UnEc=UnEc  )



   ! Close the FAST furling file:

CLOSE ( UnIn )



RETURN
END SUBROUTINE GetFurl
!=======================================================================

SUBROUTINE TFinAero( TFinCPxi, TFinCPyi, TFinCPzi, TFinCPVx, TFinCPVy, TFinCPVz, CoordSys, x, p )


   ! This routine computes the tail fin aerodynamic loads TFinKFx, TFinKFy,
   !   and TFinKMz.


   ! FAST Modules:
USE ElastoDyn_Types
USE ElastoDyn_Parameters
USE GlueCodeVars
   
USE                             General
USE                             SimCont
USE                             TailAero


   ! AeroDyn Modules

USE                             Rotor
USE                             AeroSubs
USE                             AeroDyn


IMPLICIT                        NONE


   ! Passed Variables:

REAL(ReKi), INTENT(IN )      :: TFinCPVx                                        ! Absolute Velocity of the tail center-of-pressure along tail fin chordline pointing toward tail fin trailing edge (m/s)
REAL(ReKi), INTENT(IN )      :: TFinCPVy                                        ! Absolute Velocity of the tail center-of-pressure normal to plane of tail fin pointing towards suction surface    (m/s)
REAL(ReKi), INTENT(IN )      :: TFinCPVz                                        ! Absolute Velocity of the tail center-of-pressure in plane of tail fin normal to chordline and nominally upward   (m/s)
REAL(ReKi), INTENT(IN )      :: TFinCPxi                                        ! Downwind distance from the inertial frame origin to the tail fin center-of-pressure (m)
REAL(ReKi), INTENT(IN )      :: TFinCPyi                                        ! Lateral  distance from the inertial frame origin to the tail fin center-of-pressure (m)
REAL(ReKi), INTENT(IN )      :: TFinCPzi                                        ! Vertical distance from the inertial frame origin to the tail fin center-of-pressure (m)

TYPE(ED_CoordSys), INTENT(IN)                :: CoordSys                      ! Coordinate systems of the structural dynamics module
TYPE(ED_ContinuousStateType),  INTENT(INOUT) :: x                             ! Continuous states of the structural dynamics module
TYPE(ED_ParameterType),        INTENT(IN)    :: p                             ! The parameters of the structural dynamics module

   ! Local variables:

REAL(ReKi)                   :: CTFinAOA                                        ! = COS( TFinAOA )
REAL(ReKi)                   :: STFinAOA                                        ! = SIN( TFinAOA )
REAL(ReKi)                   :: TFinQArea                                       ! (dynamic pressure)*(tail fin area) of the relative wind velocity
REAL(ReKi)                   :: TFinVrelx                                       ! Wind velocity at, and relative to, the tail fin center-of-pressure along tail fin chordline pointing toward tail fin trailing edge
REAL(ReKi)                   :: TFinVrely                                       ! Wind velocity at, and relative to, the tail fin center-of-pressure normal to plane of tail fin pointing towards suction surface
REAL(ReKi)                   :: TFinVrelz                                       ! Wind velocity at, and relative to, the tail fin center-of-pressure in plane of tail fin normal to chordline and nominally upward
REAL(ReKi)                   :: TFinWndVx                                       ! Tail fin wind velocity along tail fin chordline pointing toward tail fin trailing edge = Dot_Product( WindVelEK,  p1 )
REAL(ReKi)                   :: TFinWndVy                                       ! Tail fin wind velocity normal to plane of tail fin pointing towards suction surface    = Dot_Product( WindVelEK, -p3 )
REAL(ReKi)                   :: TFinWndVz                                       ! Tail fin wind velocity in plane of tail fin normal to chordline and nominally upward   = Dot_Product( WindVelEK,  p2 )
REAL(ReKi)                   :: WindVelEK(3)                                    ! Wind velocity at the tail fin center-of-pressure (point K) in the FAST ground (a) coordinate system

REAL(ReKi)                   :: TmpVar
INTEGER                      :: ErrStat


SELECT CASE ( TFinMod ) ! Which tail fin aerodynamics model are we using?

CASE ( 0 )              ! None!


   ! Do nothing here since TFinKFx, TFinKFy, and TFinKMz are all
   !   initialized to zero.


CASE ( 1 )              ! Standard (using inputs from the FAST furling input file).


   ! Compute wind velocity at tail fin center-of-pressure in AeroDyn ground
   !   coordinate system:

   WindVelEK(:) = AD_WindVelocityWithDisturbance( (/TFinCPxi,  TFinCPyi, TFinCPzi - p%PtfmRef /) )

      ! Convert wind velocity at tail fin center-of-pressure to FAST inertial coordinate system:

   TmpVar       = WindVelEK(2)
   WindVelEK(2) = WindVelEK(3)
   WindVelEK(3) = -1.0*TmpVar


   ! Decrease the wind velocity at tail fin center-of-pressure in the shaft
   !   direction (c1) by the average rotor axial induction if necessary:
!JASON: IS THERE ANY WAY OF DETERMINING WHETHER OR NOT THE TAIL FIN IS ACTUALLY IN THE WAKE?

   IF ( SubAxInd )  THEN
      WindVelEK = WindVelEK - AD_GetCurrentValue('AvgInfl',ErrStat)*CoordSys%c1
      IF (ErrStat /= 0) WindVelEK = 0.0
   END IF

   ! Convert the wind velocity at tail fin center-of-pressure to tail fin
   !   coordinate system:
   TFinWndVx    =      DOT_PRODUCT( WindVelEK, CoordSys%p1 )   ! Tail fin wind velocity along tail fin chordline pointing toward tail fin trailing edge
   TFinWndVy    = -1.0*DOT_PRODUCT( WindVelEK, CoordSys%p3 )   ! Tail fin wind velocity normal to plane of tail fin pointing towards suction surface
   TFinWndVz    =      DOT_PRODUCT( WindVelEK, CoordSys%p2 )   ! Tail fin wind velocity in plane of tail fin normal to chordline and nominally upward


   ! Compute the wind velocity relative to the tail fin at the tail fin
   !   center-of-pressure by subtracting out the tail fin CP velocity:

   TFinVrelx    = TFinWndVx - TFinCPVx ! Wind velocity at, and relative to, the tail fin center-of-pressure along tail fin chordline pointing toward tail fin trailing edge
   TFinVrely    = TFinWndVy - TFinCPVy ! Wind velocity at, and relative to, the tail fin center-of-pressure normal to plane of tail fin pointing towards suction surface
   TFinVrelz    = TFinWndVz - TFinCPVz ! Wind velocity at, and relative to, the tail fin center-of-pressure in plane of tail fin normal to chordline and nominally upward


   ! Compute the dynamic pressure of the relative wind velocity:

   TFinQ        = 0.5*p%AirDens*( TFinVrelx*TFinVrelx + TFinVrely*TFinVrely )
   TFinQArea    = TFinQ*TFinArea


   ! Compute the angle-of-attack between the relative wind velocity and the
   !   tail fin chordline as well as its sine and cosine:

   TFinAOA      = ATAN2( TFinVrely, TFinVrelx )
   CTFinAOA     = COS( TFinAOA )
   STFinAOA     = SIN( TFinAOA )


   ! Compute the lift, drag, and pitching moment coefficients:
   ! NOTE: The size of NFOIL equals the number of blade elements.  We need to
   !       fool the program into making it think that the tail fin is a blade
   !       element for this computation.

!JASON: THIS CALL DOES NOT TAKE INTO ACCOUNT MULTIPLE AIRFOIL TABLE INTERPOLATION BASED ON REYNOLDS NUMBER (OR OTHER PARAMETERS) BECAUSE I'M NOT SURE HOW THAT CODE WORKS.
   CALL CLCD ( TFinAOA, TFinCL, TFinCD, TFinCM, TFinNFoil, ErrStat )   ! Use the NFoil for blade element 1 to represent the tail fin airfoil


   ! Compute the resulting aerodynamic forces acting on the tail fin at its
   !   center-of-pressure:

   TFinKFx   = ( TFinCD*CTFinAOA - TFinCL*STFinAOA )*TFinQArea
   TFinKFy   = ( TFinCD*STFinAOA + TFinCL*CTFinAOA )*TFinQArea


CASE ( 2 )              ! User-defined tail fin aerodynamics model.


   CALL UserTFin ( x%QT(DOF_TFrl), x%QDT(DOF_TFrl), ZTime, DirRoot, &
                   TFinCPxi, TFinCPyi, ( TFinCPzi - p%PtfmRef ),    &
                   TFinCPVx, TFinCPVy,   TFinCPVz,                  &
                   TFinAOA , TFinQ   ,                              &
                   TFinCL  , TFinCD  ,                              &
                   TFinKFx , TFinKFy                                  )


ENDSELECT



RETURN
END SUBROUTINE TFinAero
!=======================================================================

MODULE TailAero


   ! This MODULE stores input variables for tail fin aerodynamics.


USE                             Precision


REAL(ReKi)                   :: SQRTTFinA = 0.0                                 ! = SQRT( TFinArea )
REAL(ReKi)                   :: TFinAOA   = 0.0                                 ! Angle-of-attack between the relative wind velocity and tail fin chordline
REAL(ReKi)                   :: TFinArea  = 0.0                                 ! Tail fin planform area.
REAL(ReKi)                   :: TFinCD    = 0.0                                 ! Tail fin drag            coefficient resulting from current TFinAOA
REAL(ReKi)                   :: TFinCL    = 0.0                                 ! Tail fin lift            coefficient resulting from current TFinAOA
REAL(ReKi)                   :: TFinCM    = 0.0                                 ! Tail fin pitching moment coefficient resulting from current TFinAOA
REAL(ReKi)                   :: TFinKFx   = 0.0                                 ! Aerodynamic force  at the tail fin center-of-pressure (point K) along tail fin chordline pointing toward tail fin trailing edge (N)
REAL(ReKi)                   :: TFinKFy   = 0.0                                 ! Aerodynamic force  at the tail fin center-of-pressure (point K) normal to plane of tail fin pointing towards suction surface    (N)
REAL(ReKi)                   :: TFinKMz   = 0.0                                 ! Aerodynamic moment at the tail fin center-of-pressure (point K) in plane of tail fin normal to chordline and nominally upward   (N-m)
REAL(ReKi)                   :: TFinQ     = 0.0                                 ! Dynamic pressure of the relative wind velocity

INTEGER(4)                   :: TFinMod   = 0                                   ! Tail fin aerodynamics model switch. (Initialized to zero b/c not all models read in FurlFile)
INTEGER(4)                   :: TFinNFoil = 1                                   ! Tail fin airfoil number. (iniated to first airfoil number)

LOGICAL                      :: SubAxInd  = .FALSE.                             ! Subtract average rotor axial induction when computing relative wind-inflow at tail fin?


END MODULE TailAero
!=======================================================================




! FROM FAST's RtHS routine:

   ! Let's compute the tail aerodynamic loads, if necessary:

IF ( CompAero )  THEN   ! Calculate the tail aerodynamic forces using AeroDyn.


   ! Compute TFinKFx, TFinKFy, and TFinKMz:

   CALL TFinAero( rK(1), -rK(3), rK(2),                  &
                  DOT_PRODUCT( LinVelEK,  OtherState%CoordSys%p1 ), &
                  DOT_PRODUCT( LinVelEK, -OtherState%CoordSys%p3 ), &
                  DOT_PRODUCT( LinVelEK,  OtherState%CoordSys%p2 ), OtherState%CoordSys, x, p )


   ! Vectorize these values into FKAero and MAAero:

   FKAero = TFinKFx*OtherState%CoordSys%p1 - TFinKFy*OtherState%CoordSys%p3
   MAAero = TFinKMz*OtherState%CoordSys%p2


ELSE                    ! Wind turbine in vacuum, no aerodynamic forces.


   FKAero = 0.0
   MAAero = 0.0


ENDIF

!=======================================================================

!   ! Make sure TFinNFoil is an existing airfoil number:
!
!IF ( ( TFinNFoil < 1 ) .OR. ( TFinNFoil > NumFoil ) )  &
!   CALL ProgAbort ( ' TFinNFoil must be between 1 and NumFoil (inclusive).' )
!
!
