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

!USE                           Noise
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



   ! Write data read in from ADFile into MODULEs used by FAST:

p_ED%AirDens  = AD_GetConstant('AirDensity', ErrStat)


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
