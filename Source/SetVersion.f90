!=======================================================================
SUBROUTINE SetVersion


   ! This routine sets the version number.  By doing it this way instead
   !   of the old way of initializing it in a module, we will no longer
   !   have to recompile everything every time we change versions.


USE                             General
USE                             NWTC_Library


IMPLICIT                        NONE


   ! Local Variables:

CHARACTER(6)                 :: Prcsn                                           ! String containing a description of the as-compiled precision.


ProgName = 'FAST'

ProgVer = '(v7.00.01, 20-Jan-2012)'



IF ( ReKi == 4 )  THEN     ! Single precision
   Prcsn = 'SINGLE'
ELSEIF ( ReKi == 8 )  THEN ! Double precision
   Prcsn = 'DOUBLE'
ELSE                       ! Unknown precision - it should be impossible to compile using a KIND that is not 4 or 8, but I'll put this check here just in case.
   Prcsn = 'UNKNWN'
ENDIF


IF ( Cmpl4SFun )  THEN     ! FAST has been compiled as an S-Function for Simulink

   ProgVer = TRIM(ProgVer)//'-Compiled as S-Function for Simulink'

ELSEIF( ReKi /= 4 )  THEN  ! Compiled using something other than single precision

   ProgVer = TRIM(ProgVer)//'-Compiled using '//Prcsn//' precision'

ENDIF



RETURN
END SUBROUTINE SetVersion
