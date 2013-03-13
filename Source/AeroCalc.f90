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






RETURN
END SUBROUTINE Set_FAST_Params
!=======================================================================
