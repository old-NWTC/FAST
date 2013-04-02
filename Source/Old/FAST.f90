MODULE FASTSubs

   USE   NWTC_Library

   USE   ElastoDyn_Types
   USE   ElastoDyn_Parameters
   USE   ElastoDyn

   USE   ServoDyn_Types
   USE   ServoDyn

   USE   FAST_Types

   USE AeroDyn_Types

implicit none

CONTAINS
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

   CALL RtHS( t, p, x, OtherState, u, y, p_SrvD, y_SrvD, u_SrvD, OtherState_SrvD )


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

   IF ( p%DOF_Flag(DOF_GeAz) .AND. ( t > p_SrvD%THSSBrDp ) )  CALL FixHSSBrTq ( 'Corrector', p, OtherState, u%HSSBrTrq )


ENDIF


   ! Compute the final value of QD2T from the best estimates for Q and
   !   QD, last call to RtHS:

x%QT  = OtherState%Q (:,OtherState%IC(NMX))
x%QDT = OtherState%QD(:,OtherState%IC(NMX))

CALL RtHS( t, p, x, OtherState, u, y, p_SrvD, y_SrvD, u_SrvD, OtherState_SrvD )

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

   CALL FixHSSBrTq ( 'Predictor', p, OtherState, u%HSSBrTrq )

ENDIF



RETURN
END SUBROUTINE Solver
!=======================================================================

!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE FixHSSBrTq ( Integrator, p, OtherState, HSSBrTrq  )


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
OgnlGeAzRo = OtherState%AugMatOut(DOF_GeAz,:)


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

   OtherState%AugMatOut(p%DOFs%SrtPS(I),    p%NAug) = OtherState%AugMatOut(p%DOFs%SrtPS(I),p%NAug) - OtherState%AugMatOut(p%DOFs%SrtPS(I),DOF_GeAz)*RqdQD2GeAz  ! {{Fa}-[Cab]{Qb}}
   OtherState%AugMatOut(p%DOFs%SrtPS(I),DOF_GeAz) = 0.0                                                           ! [0]
   OtherState%AugMatOut(DOF_GeAz,p%DOFs%SrtPS(I)) = 0.0                                                           ! [0]

ENDDO             ! I - All active (enabled) DOFs

   OtherState%AugMatOut(DOF_GeAz,DOF_GeAz) = 1.0                                                           ! [I]{Qb}={Qb}
   OtherState%AugMatOut(DOF_GeAz,    p%NAug) = RqdQD2GeAz                                                    !


   ! Invert the matrix to solve for the new (updated) accelerations.  Like in
   !   RtHS(), the accelerations are returned by GaussElim() in the first NActvDOF
   !   elements of the solution vector, SolnVec().  These are transfered to the
   !   proper index locations of the acceleration vector QD2T() using the
   !   vector subscript array SrtPS(), after Gauss() has been called:
   ! NOTE: QD2T( SrtPS(1:NActvDOF) ) cannot be sent directly because arrays
   !   sections with vector subscripts must not be used in INTENT(OUT)
   !   arguments.

CALL GaussElim( OtherState%AugMatOut( p%DOFs%SrtPS    (1: p%DOFs%NActvDOF  )   ,         &
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

!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE RtHS( t, p, x, OtherState, u, y, p_SrvD, y_SrvD, u_SrvD, OtherState_SrvD )


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

REAL(ReKi)                   :: rAerCen   (3)                                   ! Position vector from inertial frame origin to current blade analysis node aerodynamic center.
REAL(ReKi)                   :: rPAerCen  (3)                                   ! Position vector from teeter pin (point P) to current blade analysis node aerodynamic center.
REAL(ReKi)                   :: rSAerCen  (3)                                   ! Position vector from a blade analysis node (point S) on the current blade to the aerodynamic center associated with the element.

   ! variables that used to be in MODULE RtHndSd, but aren't used elsewhere:
REAL(ReKi)                   :: SolnVec    (p%NDOF)                             ! Solution vector found by solving the equations of motion

REAL(ReKi)                   :: LinAccEO  (3)                                   ! Total linear acceleration of the base plate (point O) in the inertia frame (body E for earth).

   ! local integer variables

INTEGER(IntKi)               :: I                                               ! Loops through some or all of the DOFs.
INTEGER(IntKi)               :: J                                               ! Loops through nodes / elements.
INTEGER(IntKi)               :: K                                               ! Loops through blades.
INTEGER(IntKi)               :: L                                               ! Generic index
INTEGER(IntKi), PARAMETER    :: SgnPrvLSTQ = 1                                  ! The sign of the low-speed shaft torque from the previous call to RtHS().  This is calculated at the end of RtHS().  NOTE: The low-speed shaft torque is assumed to be positive at the beginning of the run!

INTEGER(IntKi)  :: ErrStat     ! Error status of the operation
CHARACTER(1024) :: ErrMsg      ! Error message if ErrStat /= ErrID_None



IF ( p_FAST%CompServo ) THEN
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

      ! Calculate tower-top acceleration (fore-aft mode only) in the tower-top system:

      LinAccEO = OtherState%RtHS%LinAccEOt
      DO I = 1,p%DOFs%NPTE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the yaw bearing center of mass (point O)
         LinAccEO = LinAccEO + OtherState%RtHS%PLinVelEO(p%DOFs%PTE(I),0,:)*OtherState%QD2T(p%DOFs%PTE(I))
      ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the yaw bearing center of mass (point O)

      u_SrvD%TwrAccel = DOT_PRODUCT( LinAccEO, OtherState%CoordSys%b1 )
   ELSE
      u_SrvD%TwrAccel = 0
   END IF

      ! Calculate SrvD outputs:

   CALL SrvD_CalcOutput( t, u_SrvD, p_SrvD, x_SrvD, xd_SrvD, z_SrvD, OtherState_SrvD, y_SrvD, ErrStat, ErrMsg )
   IF (ErrStat /= ErrID_None) RETURN

      ! Map SrvD outputs to ED inputs:
   u%GenTrq     = y_SrvD%GenTrq
   u%HSSBrTrq   = y_SrvD%HSSBrTrq
   u%BlPitchCom = y_SrvD%BlPitchCom
   u%YawMom     = y_SrvD%YawMom
!   u%TBDrCon    = y_SrvD%TBDrCon !array

   OtherState%BlPitch = u%BlPitchCom

END IF ! p_FAST%CompServo
!....................................

   ! Let's define the coordinate systems that will be used throughout this routine:

CALL SetCoordSy( t, OtherState%CoordSys, OtherState%RtHS, OtherState%BlPitch, p, x, ErrStat, ErrMsg )

CALL CalculatePositions(        p, x, OtherState%CoordSys, OtherState%RtHS ) ! calculate positions
CALL CalculateAngularPosVelAcc( p, x, OtherState%CoordSys, OtherState%RtHS ) ! calculate angular positions, velocities, and accelerations, including partial angular quantities
CALL CalculateLinearVelAcc(     p, x, OtherState%CoordSys, OtherState%RtHS ) ! calculate linear velocities and accelerations

IF ( p_FAST%CompAero )  THEN   ! Calculate the blade element aerodynamic loads using AeroDyn.

   !-------------------------------------------------------------------------------------------------
   ! Blade positions:
   !-------------------------------------------------------------------------------------------------
   DO K = 1,p%NumBl ! Loop through all blades
      DO J = 1,p%BldNodes ! Loop through the blade nodes / elements

      ! Calculate the aerodynamic pitching moment arm (i.e., the position vector from point S on the blade to the aerodynamic center of the element):

            rSAerCen = p%rSAerCenn1(K,J)*OtherState%CoordSys%n1(K,J,:) + p%rSAerCenn2(K,J)*OtherState%CoordSys%n2(K,J,:) !bjj: make rSAerCen a matrix? we recalculate it later


      ! Define positions USEd by AeroDyn.

            rPAerCen     = OtherState%RtHS%rPQ + OtherState%RtHS%rQS(K,J,:) + rSAerCen         ! Position vector from teeter pin (point P)  to blade analysis node aerodynamic center.
            rAerCen      =       OtherState%RtHS%rS (K,J,:) + rSAerCen         ! Position vector from inertial frame origin to blade analysis node aerodynamic center.

            ADAeroMarkers%Blade(J,K)%Position(1)      =     rAerCen(1)              ! = the distance from the undeflected tower centerline                                     to the current blade aerodynamic center in the xi ( z1) direction
            ADAeroMarkers%Blade(J,K)%Position(2)      = -1.*rAerCen(3)              ! = the distance from the undeflected tower centerline                                     to the current blade aerodynamic center in the yi (-z3) direction
            ADAeroMarkers%Blade(J,K)%Position(3)      =     rAerCen(2) - p%PtfmRef  ! = the distance from the nominal tower base position (i.e., the undeflected position of the tower base) to the current blade aerodynamic center in the zi ( z2) direction

      END DO !J = 1,p%BldNodes ! Loop through the blade nodes / elements
   END DO !K = 1,p%NumBl

   !JASON: WE SHOULD REALLY BE PASSING TO AERODYN THE LINEAR VELOCITIES OF THE AERODYNAMIC CENTER IN THE INERTIA FRAME, NOT SIMPLY THE LINEAR VELOCITIES OF POINT S.  IS THERE ANY WAY OF GETTING THIS VELOCITY?<--DO THIS, WHEN YOU ADD THE COUPLED MODE SHAPES!!!!


      ! the hub position should use rQ instead of rP, but the current version of AeroDyn treats
      ! teeter deflections like blade deflections:

   ADInterfaceComponents%Hub%Position  = (/ OtherState%RtHS%rP(1), -1.*OtherState%RtHS%rP(3), OtherState%RtHS%rP(2) - p%PtfmRef /)


      ! Rotor furl position should be rP instead of rV, but AeroDyn needs this for the
      ! HubVDue2Yaw calculation:

   ADInterfaceComponents%RotorFurl%Position(:) = (/ OtherState%RtHS%rV(1), -1.*OtherState%RtHS%rV(3), OtherState%RtHS%rV(2) - p%PtfmRef /)

   ADInterfaceComponents%Nacelle%Position(:)   = (/ OtherState%RtHS%rO(1), -1.*OtherState%RtHS%rO(3), OtherState%RtHS%rO(2) - p%PtfmRef /)

      ! Tower base position should be rT(0) instead of rZ, but AeroDyn needs this for
      ! the HubVDue2Yaw calculation:
   ADInterfaceComponents%Tower%Position(:)     = (/ OtherState%RtHS%rZ(1), -1.*OtherState%RtHS%rZ(3), OtherState%RtHS%rZ(2) - p%PtfmRef /)


   !y%HubPosition       = (/ OtherState%RtHS%rP(1),                 -1.*OtherState%RtHS%rP(3),                 OtherState%RtHS%rP(2)                 - p%PtfmRef /)
   !y%RotorFurlPosition = (/ OtherState%RtHS%rV(1),                 -1.*OtherState%RtHS%rV(3),                 OtherState%RtHS%rV(2)                 - p%PtfmRef /)
   !y%NacellePosition   = (/ OtherState%RtHS%rO(1), -1.*OtherState%RtHS%rO(3), OtherState%RtHS%rO(2) - p%PtfmRef /)
   !y%TowerPosition     = (/ OtherState%RtHS%rZ(1), -1.*OtherState%RtHS%rZ(3), OtherState%RtHS%rZ(2) - p%PtfmRef /)


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
   
   !-------------------------------------------------------------------------------------------------
   ! Call AeroDyn to calculate aerodynamic loads
   !-------------------------------------------------------------------------------------------------   
!JASON: WE SHOULD REALLY BE PASSING TO AERODYN THE LINEAR VELOCITIES OF THE AERODYNAMIC CENTER IN THE INERTIA FRAME, NOT SIMPLY THE LINEAR VELOCITIES OF POINT S.  IS THERE ANY WAY OF GETTING THIS VELOCITY?<--DO THIS, WHEN YOU ADD THE COUPLED MODE SHAPES!!!!

   ! NOTE: AeroBladeForce( 1) = element normal     force per unit span in the  m1 direction (N/m).
   !       AeroBladeForce( 2) = element tangential force per unit span in the -m2 direction (N/m).
   !       AeroBladeMoment(3) = element pitching moment  per unit span in about the m3-axis (N-m/m).
   
   ADAeroLoads = AD_CalculateLoads( REAL(t, ReKi), ADAeroMarkers, ADInterfaceComponents, ADIntrfaceOptions, ErrStat )
   
   !-------------------------------------------------------------------------------------------------
   ! convert AD outputs to ED inputs
   !-------------------------------------------------------------------------------------------------
   
   DO K = 1,p%NumBl ! Loop through all blades
      DO J = 1,p%BldNodes ! Loop through the blade nodes / elements  
         u%AeroBladeForce(:,J,K) = ADAeroLoads%Blade(J,K)%Force
         u%AeroBladeMoment(:,J,K) = ADAeroLoads%Blade(J,K)%Moment
      END DO !J
   END DO   !K
     
END IF  ! CompAero

CALL CalculateForcesMoments(    p, x, OtherState%CoordSys, u, OtherState%RtHS ) !calculate the forces and moments (requires AeroBladeForces and AeroBladeMoments)

!.....................................
!  TeetMom,  RFrlMom, TFrlMom
!.....................................
   
      ! Compute the moments from teeter springs and dampers, rotor-furl springs and dampers, tail-furl springs and dampers,
      !  and the generator and high-speed shaft brake torque:

   CALL Teeter  ( t, p, OtherState%RtHS%TeetAng, OtherState%RtHS%TeetAngVel, OtherState%RtHS%TeetMom ) ! Compute moment from teeter     springs and dampers, TeetMom; NOTE: TeetMom will be zero for a 3-blader since TeetAng = TeetAngVel = 0
   CALL RFurling( t, p, x%QT(DOF_RFrl),          x%QDT(DOF_RFrl),            OtherState%RtHS%RFrlMom ) ! Compute moment from rotor-furl springs and dampers, RFrlMom
   CALL TFurling( t, p, x%QT(DOF_TFrl),          x%QDT(DOF_TFrl),            OtherState%RtHS%TFrlMom ) ! Compute moment from tail-furl  springs and dampers, TFrlMom


!bjj: note OtherState%RtHS%GBoxEffFac needed in OtherState only to fix HSSBrTrq
OtherState%RtHS%GBoxEffFac  = p%GBoxEff**SgnPrvLSTQ      ! = GBoxEff if SgnPrvLSTQ = 1 OR 1/GBoxEff if SgnPrvLSTQ = -1

CALL FillAugMat( p, x, OtherState%CoordSys, u, OtherState%RtHS, OtherState%AugMat )



! make a copy for the routine that fixes the HSSBrTrq
OtherState%AugMatOut  = OtherState%AugMat




   ! Invert the matrix to solve for the accelerations.  The accelerations are
   !   returned by Gauss() in the first NActvDOF elements of the solution
   !   vector, SolnVec().  These are transfered to the proper index locations
   !   of the acceleration vector QD2T() using the vector subscript array
   !   SrtPS(), after Gauss() has been called:
   ! NOTE: QD2T( SrtPS(1:NActvDOF) ) cannot be sent directly because arrays
   !   sections with vector subscripts must not be used in INTENT(OUT) arguments.


CALL GaussElim( OtherState%AugMat( p%DOFs%SrtPS    (1: p%DOFs%NActvDOF   ),     &
                        p%DOFs%SrtPSNAUG(1:(p%DOFs%NActvDOF+1)) ),   &
                                                     p%DOFs%NActvDOF, SolnVec, ErrStat, ErrMsg )
!IF ( ErrStat /= ErrID_None ) CALL WrScr( ' Message from RtHS: '//TRIM(ErrMsg) )



OtherState%QD2T = 0.0
DO I = 1,p%DOFs%NActvDOF ! Loop through all active (enabled) DOFs
   OtherState%QD2T(p%DOFs%SrtPS(I)) = SolnVec(I)
ENDDO             ! I - All active (enabled) DOFs



   ! Let's calculate the sign (+/-1) of the low-speed shaft torque for this time step and store it in SgnPrvLSTQ.
   !  This will be used during the next call to RtHS (bjj: currently violates framework so we'll remove it).

!SgnPrvLSTQ = SignLSSTrq(p, OtherState)


RETURN

END SUBROUTINE RtHS
!----------------------------------------------------------------------------------------------------------------------------------

END MODULE FASTSubs
