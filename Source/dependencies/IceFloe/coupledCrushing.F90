!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2014  DNV KEMA Renewables, Inc.
!
!    This file is part of the IceFloe suite of subroutines
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
!************************************************************************

!**********************************************************************************************************************************
! File last committed: $Date: 2014-02-13 12:30:17 -0800 (Thu, 13 Feb 2014) $
! (File) Revision #: $Rev: 134 $
! URL: $HeadURL: http://sel1004.verit.dnv.com:8080/svn/LoadSimCtl_SurfaceIce/trunk/IceDyn_IntelFortran/IceDyn/source/IceFloe/coupledCrushing.F90 $
!**********************************************************************************************************************************!
 
!  Module for calculations of loads induced by ice crushing against vertical surfaces
!  Using a coupled model from Maattanen, M. (1998) "Numerical Model for Ice Induced 
!     vibration load lock-in and Synchronization".  Proceedings of the 14th Int. Symp.
!     on Ice, Potsdam, NY USA, vol 2, pp 923-930.

module iceCpldCrushing

   use iceFloeBase

   implicit none

   public

!  Some limits on strength curve
!  local minimum of stress rate to strength curve at stress rate = 1.3287MPa/s
!  failure mode is brittle so hold this constant for high stress rates
   real(ReKi), parameter   :: stressRateAtMin = 1.3287
!  this value does not have the area term in it, added later
   real(ReKi), parameter   :: localMinStrength = 2.00 + stressRateAtMin*(7.80       &
                            + stressRateAtMin*(-18.57 + stressRateAtMin*(13.00 - stressRateAtMin*2.91)))

contains

!  calculate the time series of ice loads using the ISO method
   subroutine initCpldCrushing (iceInput, myIceParams, iceLog)

      type(iceInputType), intent(in)            :: iceInput    ! Input parameters read from file
      type(IceFloe_ParameterType), intent(inout)     :: myIceParams
      type(iceFloe_LoggingType), intent(inout)   :: iceLog      ! structure with message and error logging variables
      type(inputParams)                         :: inParams    ! specific input parameter variable list

      integer(IntKi) :: err

!  initialize the common parmeters
      call initIceFloe(iceInput, inParams, myIceParams, iceLog)

      call logMessage(iceLog, newLine//' Setting coupled ice crushing input parameters ')

      call getIceInput(iceInput, 'refIceStrength', inParams%refIceStrength, iceLog, 0.5E6, 5.0E6)
      call logMessage(iceLog, ' Reference ice strength = '//TRIM(Num2LStr(inParams%refIceStrength))//' Pascals')

      call getIceInput(iceInput, 'minStrength', inParams%minStrength, iceLog, 0.0, 1.0E9)
      call logMessage(iceLog, ' Minimum ice strength from sress rate polynomial = '//        &
                              TRIM(Num2LStr(inParams%minStrength))//' Pascals')

      call getIceInput(iceInput, 'minStrengthNegVel', inParams%minStrengthNegVel, iceLog, 0.0, 1.0E9)
      call logMessage(iceLog, ' Minimum negative velocity ice strength = '//        &
                              TRIM(Num2LStr(inParams%minStrengthNegVel))//' Pascals')

!  Assign some input parameters to saved parameters
      myIceParams%iceVel      = inParams%iceVelocity
      myIceParams%minStrength = inParams%minStrength
      myIceParams%minStrengthNegVel = inParams%minStrengthNegVel/1.0E6
!  Precalc some things.
      myIceParams%crushArea   = inParams%twr%diam*inParams%iceThickness
!  Scale to MPa
      myIceParams%coeffStressRate = 8.0E-6*inParams%refIceStrength/PI/inParams%twr%diam
!  Need a negative stress rate below which strength = minStrengthNegVel
!  Also use this for a cubic transition between 0 stress rate and min stress rate      
!  Since equation is a quartic it is too complex to find roots. Fit region of interest with a quadratic instead
      myIceParams%minStressRate = (7.4998-sqrt(56.247+89.908*(1.9954-myIceParams%minStrengthNegVel    &
                                *sqrt(myIceParams%crushArea))))/44.954
!  Also need coefficients of cubic equation to transition strength from 0 to minStressRate                                
      myIceParams%C(1) = 7.8*myIceParams%minStressRate - 2.0*sqrt(myIceParams%crushArea)*myIceParams%minStrengthNegVel
      myIceParams%C(1) = (myIceParams%C(1) + 4.0)/sqrt(myIceParams%crushArea)/(myIceParams%minStressRate**3)
      myIceParams%C(2) = 3.0*sqrt(myIceParams%crushArea)*myIceParams%minStrengthNegVel - 15.6*myIceParams%minStressRate
      myIceParams%C(2) = (myIceParams%C(2) - 6.0)/sqrt(myIceParams%crushArea)/(myIceParams%minStressRate**2)
      myIceParams%C(3) = 7.8/sqrt(myIceParams%crushArea)
      myIceParams%C(4) = 2.0/sqrt(myIceParams%crushArea)

   end subroutine initCpldCrushing

!****************************************************************************
   function outputCpldCrushLoad (myIceParams, iceLog, inVels, time)  result(iceLoads)
      type(IceFloe_ParameterType), intent(in)      :: myIceParams
      type(iceFloe_LoggingType), intent(inout) :: iceLog   ! structure with message and error logging variables
      real(ReKi), intent(in)                  :: inVels(2,myIceParams%numLegs)
      real(DbKi), intent(in)                  :: time
      real(ReKi)                              :: iceLoads(6,myIceParams%numLegs)

      real(ReKi)     :: twrVelAngle    !  Angle of tower motion vector + ro x-axis towards y-axis
      real(ReKi)     :: velProjected   !  Velocity component projected onto direction of ice movement
      real(ReKi)     :: stressRate     !  MPa/sec
      real(ReKi)     :: strength       !  Instantaneous ice strength, Pascals
      integer(IntKi) :: nL             !  loop index for number of legs

      do nL = 1, myIceParams%numLegs
   !  get tower velocity in direction of ice floe flow
         twrVelAngle  = atan2(inVels(2,nL),inVels(1,nL)) 
         velProjected = sqrt(inVels(1,nL)**2 + inVels(2,nL)**2)*cos(twrVelAngle - myIceParams%iceDirection)
         stressRate = (myIceParams%iceVel - velProjected)*myIceParams%coeffStressRate

   !  This polynomial is specific to the Maattanen method and is thus hard coded
   !  Only applicable from a stress rate of zero to +1.3287MPa/sec
   !  In this range the failure mode is ductile crushing
   !  The ice load goes to a user specified minimum for negative stress rate (tower moving away from ice forming a gap)
         if (stressRate <= myIceParams%minStressRate) then
            strength = myIceParams%minStrengthNegVel
   !  Apply a cubic step to avoid sharp changes in load at minStrengthnegVel
         elseif (stressRate < 0.0) then  
            strength = myIceParams%C(4)+stressRate*(myIceParams%C(3)+stressRate*(myIceParams%C(2)+stressRate*myIceParams%C(1)))
         else
   !     The curve reaches a local minima at 1.3287MPa above which brittle failure dominates and strength is constant
   !     However it can fall below the user specifid minimum before that
            if (stressRate > stressRateAtMin) then
               strength = localMinStrength/sqrt(myIceParams%crushArea)
               strength = max(localMinStrength/sqrt(myIceParams%crushArea), 1.0E-6*myIceParams%minStrength)
            else
               strength = 2.00 + stressRate*(7.80 + stressRate*(-18.57 + stressRate*(13.00 - stressRate*2.91)))
               strength = max(strength/sqrt(myIceParams%crushArea), 1.0E-6*myIceParams%minStrength)
            endif
         endif         
         strength = 1.0E6*strength

   !  apply the load in the proper direction via x,y components
         iceLoads(:,nL) = myIceParams%ks(nL)*iceLoadDirection(strength*myIceParams%crushArea, myIceParams)
      enddo

   !  If the loads from multiple legs are applied as an "effective" load on one leg at the
   !  support center, then compute that effective load vector and put into the first load vector
   !  Note that only Fx, Fy, and Mz terms re-calculated.  All others remain zero
      if (myIceParams%numLegs > 1 .and. myIceParams%singleLoad)    &
         iceLoads(:,1) = iceLoadEquivalent(iceLoads, myIceParams)
      
   end function outputCpldCrushLoad

end module iceCpldCrushing
