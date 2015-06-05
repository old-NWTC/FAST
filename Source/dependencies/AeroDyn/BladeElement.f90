module BladeElement
   
   use AirfoilInfo_Types
   use UnsteadyAero
   use UnsteadyAero_Types
   
   implicit none
   
    integer(IntKi), public, parameter  :: SkewMod_Uncoupled  = 1      ! Uncoupled (no correction) [-]
    integer(IntKi), public, parameter  :: SkewMod_PittPeters = 2      ! Pitt/Peters [-]
    integer(IntKi), public, parameter  :: SkewMod_Coupled    = 3      ! Coupled [-]
   
   
   public :: BE_CalcCxCyCoefs
   public :: BE_CalcOutputs
   
   contains
   
   subroutine BE_CalcCxCyCoefs(phi, useAIDrag, useTIDrag, Cl, Cd, Cx, Cy)
      real(ReKi),             intent(in   ) :: phi
      logical,                intent(in   ) :: useAIDrag
      logical,                intent(in   ) :: useTIDrag
      real(ReKi),             intent(in   ) :: Cl
      real(ReKi),             intent(in   ) :: Cd
      real(ReKi),             intent(  out) :: Cx
      real(ReKi),             intent(  out) :: Cy
   
         !Locals
      real(ReKi)      cphi, sphi
   
      cphi = cos(phi)
      sphi = sin(phi)
   
            ! resolve into normal (x) and tangential (y) forces
       if (  useAIDrag ) then
           Cx = Cl*cphi + Cd*sphi
       else      
           Cx = Cl*cphi
       end if
    
       if (  useTIDrag ) then     
           Cy = Cl*sphi - Cd*cphi
       else     
           Cy = Cl*sphi
       end if
   end subroutine BE_CalcCxCyCoefs
   
   subroutine BE_CalcOutputs(AFInfo, UA_Flag, AOA, W, Re, p_UA, xd_UA, OtherState_UA, Cl, Cd, Cm, ErrStat, ErrMsg)     

      type(AFInfoType),             intent(in   ) :: AFInfo
      logical   ,                   intent(in   ) :: UA_Flag
      real(ReKi),                   intent(in   ) :: AOA            ! Angle of attack in radians
      real(ReKi),                   intent(in   ) :: W
      real(ReKi),                   intent(in   ) :: Re
      type(UA_ParameterType),       intent(in   ) :: p_UA           ! Parameters
      type(UA_DiscreteStateType),   intent(in   ) :: xd_UA          ! Discrete states at Time
      type(UA_OtherStateType),      intent(in   ) :: OtherState_UA  ! Other/optimization states
      real(ReKi),                   intent(  out) :: Cl
      real(ReKi),                   intent(  out) :: Cd
      real(ReKi),                   intent(  out) :: Cm
      integer(IntKi),               intent(  out) :: ErrStat     ! Error status of the operation
      character(*),                 intent(  out) :: ErrMsg      ! Error message if ErrStat /= ErrID_None
   
      real                            :: IntAFCoefs(4)                ! The interpolated airfoil coefficients.
      integer                         :: s1
      type(UA_InputType)              :: u_UA
      type(UA_OutputType)             :: y_UA          !
      
      ErrStat = ErrID_None
      ErrMsg  = ''
   
      ! TODO: Extend this to use the UnsteadyAero module to determine the Cl, Cd, Cm info, as needed.  We may need to be tracking whether this call is
      ! part of an UpdateStates action or a CalcOutput action, because the calculation chain differs for the two.
      if (UA_Flag) then
         u_UA%alpha = AOA   
         u_UA%Re    = Re
         u_UA%U     = W
         !bjj: this gets called element-by-element (not all at once). Are OtherState%iBladeNode and OtherState%iBlade set properly?
         call UA_CalcOutput(u_UA, p_UA, xd_UA, OtherState_UA, AFInfo, y_UA, errStat, errMsg )
         Cl         = y_UA%Cl
         Cd         = y_UA%Cd
         Cm         = y_UA%Cm
      else
            ! NOTE: we use Table(1) because the right now we can only interpolate with AOA and not Re or other variables.  If we had multiple tables stored
            ! for changes in other variables (Re, Mach #, etc) then then we would need to interpolate across tables.
            !
         s1 = size(AFInfo%Table(1)%Coefs,2)
   
         IntAFCoefs(1:s1) = CubicSplineInterpM( 1.0*real( AOA*R2D ) &
                                              , AFInfo%Table(1)%Alpha &
                                              , AFInfo%Table(1)%Coefs &
                                              , AFInfo%Table(1)%SplineCoefs &
                                              , ErrStat, ErrMsg )
   
         Cl = IntAFCoefs(1)
         Cd = IntAFCoefs(2)
         Cm = IntAFCoefs(3)
      end if
      
   end subroutine BE_CalcOutputs
   
end module BladeElement