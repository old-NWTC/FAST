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