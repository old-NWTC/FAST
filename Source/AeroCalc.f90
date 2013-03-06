!=======================================================================
SUBROUTINE Set_FAST_Params( p_ED )


   ! Set variables based on AeroDyn inputs
   ! Called at the start of the simulation to set up FAST variables based
   !   on AeroDyn parameters


   ! AeroDyn Modules:

USE                           Blade,      ONLY: C
USE                           AeroDyn
USE ElastoDyn_Types
USE GlueCodeVars
   
   ! FAST Modules:

USE                           Noise
USE                           SimCont !DT, TMax

USE                           AeroElem !, ONLY: NumADBldNodes, AD_AeroMarker


IMPLICIT                      NONE

   ! Passed variables:
TYPE(ED_ParameterType),  INTENT(INOUT)  :: p_ED                      ! The parameters of the structural dynamics module


   ! Local variables:
REAL(ReKi)                 :: AD_RefHt
REAL(ReKi)                 :: InpPosition(3)

INTEGER                    :: ErrStat
INTEGER(4)                 :: IELM
INTEGER(4)                 :: Sttus                                     ! Status returned from an allocation request.



!   ! Write data read in from ADFile into MODULEs used by FAST:

p_ED%AirDens  = AD_GetConstant('AirDensity', ErrStat)



  ! Let's compute the turbulence intensity and average wind speed for the
  !   turbulent inflow noise calculation:

IF ( CompNoise )  THEN  ! Yes, noise will be computed.
   InpPosition = (/ 0.0, 0.0, p_ED%FASTHH /)

   CALL Noise_CalcTI( REAL(0.0, ReKi), REAL(TMax, ReKi), REAL(DT, ReKi), InpPosition )

   KinViscosity = AD_GetConstant( 'KinVisc', ErrStat )      ! this variable stored in the Noise module.  The Noise module should be rewritten so that this is part of an initialization routine.
   AirDensity   = AD_GetConstant('AirDensity', ErrStat)
ENDIF


   ! Set up other parameters only if we need them

IF ( CompAero )  THEN

   ! Let's see if the hub-height in AeroDyn and FAST are within 10%:
   AD_RefHt = AD_GetConstant('RefHt', ErrStat)

   IF ( ABS( p_ED%FASTHH - AD_RefHt ) > 0.1*( p_ED%FASTHH ) )  THEN  !bjj: I believe that this should not be done in the future

      CALL ProgWarn( ' The FAST hub height ('//TRIM(Num2LStr( p_ED%FASTHH ))//') and AeroDyn input'// &
                    ' reference hub height ('//TRIM(Num2LStr(AD_RefHt))//') differ by more than 10%.' )
   ENDIF

ENDIF


RETURN
END SUBROUTINE Set_FAST_Params
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
