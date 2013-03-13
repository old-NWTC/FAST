!=======================================================================
SUBROUTINE GetVersion(ProgVer)


   ! This routine sets the version number.  By doing it this way instead
   !   of the old way of initializing it in a module, we will no longer
   !   have to recompile everything every time we change versions.

   USE GlueCodeVars
   USE                             NWTC_Library


   IMPLICIT                        NONE


   ! Passed Variables:

   CHARACTER(1024), INTENT(OUT)  :: ProgVer                                           ! String containing a description of the as-compiled precision.


   
   ProgVer = TRIM(GetNVD(FAST_Ver))//' (compiled using '

   ! determine precision
      IF ( ReKi == SiKi )  THEN     ! Single precision
         ProgVer = TRIM(ProgVer)//'SINGLE'
      ELSEIF ( ReKi == R8Ki )  THEN ! Double precision
         ProgVer = TRIM(ProgVer)//'DOUBLE'
      ELSE                          ! Unknown precision
         ProgVer = TRIM(ProgVer)//'UNKNOWN'
      ENDIF

   ProgVer = TRIM(ProgVer)//' precision'


   ! determine if we've done some other modifications
      IF ( Cmpl4SFun )  THEN     ! FAST has been compiled as an S-Function for Simulink
         ProgVer = TRIM(ProgVer)//' as S-Function for Simulink'  
      ELSEIF ( Cmpl4LV )  THEN     ! FAST has been compiled as a DLL for Labview
         ProgVer = TRIM(ProgVer)//' as a DLL for Labview'
      ENDIF

      !IF ( OC3HywindMods ) THEN
      !   ProgVer = TRIM(ProgVer)//' with OC3 Hywind Modifications'
      !END IF
            
   ProgVer = TRIM(ProgVer)//')'
   

   RETURN
END SUBROUTINE GetVersion
