
IF "%COMPUTERNAME%"=="BJONKMAN-23080S" GOTO BJONKMAN-23080S
IF "%COMPUTERNAME%"=="MBUHL-20665S" GOTO MBUHL-20665S

REM --------------------------------------------------------------------------------------------------------------------------------
REM These paths are for Bonnie Jonkman and Marshall Buhl; other users should modify their own paths as necessary.

:BJONKMAN-23080S
ECHO Setting paths for Bonnie Jonkman.

SET Crunch=C:\Users\bjonkman\Documents\DATA\DesignCodes\postprocessors\Crunch\SVNdirectory\trunk\crunch_win32.exe

SET Registry=CALL Registry
SET REG_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\FAST\SVNdirectory\branches\FAST_Registry

SET FAST_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\FAST\SVNdirectory\branches\BJonkman\Source

SET NWTC_Lib_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\miscellaneous\nwtc_subs\SVNdirectory\trunk\source
SET NETLIB_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\miscellaneous\nwtc_subs\SVNdirectory\branches\NetLib\NWTC_source
SET ED_Loc=%FAST_Loc%
SET SrvD_Loc=%FAST_Loc%
SET AD_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\AeroDyn\SVNdirectory\trunk\Source
SET IfW_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\InflowWind\SVNdirectory\branches\modularization\Source
SET SD_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\SubDyn\SVNdirectory\branches\v1.00.00-rrd\Source

SET HD_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\HydroDyn\SVNdirectory\branches\HydroDyn_Modularization\Source
SET HD_Reg_Loc=%HD_Loc%\RegistryFiles

SET MAP_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\MAP\SVNdirectory\trunk\src\fortran
SET MAP_DLL=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\MAP\SVNdirectory\trunk\executable\MAP_win32.dll
SET MAP_Include_Lib=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\MAP\SVNdirectory\trunk\executable\MAP_win32.lib

ECHO BONNIE: FIX THE FEAM LOCATION!!!
SET FEAM_Loc=%FAST_Loc%\dependencies\FEAMooring

SET IceF_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\IceFloe\SVNDirectory\IceFloe\source

GOTO End

REM --------------------------------------------------------------------------------------------------------------------------------
:MBUHL-20665S
ECHO Setting paths for Marshall Buhl's laptop.

SET Registry=CALL Registry
SET Crunch=M:\CAEtools\Postprocessors\Crunch\trunk\crunch_win32.exe


SET REG_Loc=M:\CAETools\Miscellaneous\Registry\Source
SET FAST_Loc=M:\CAETools\Simulators\FAST\branches\BJonkman\Source

SET NWTC_Lib_Loc=M:\CAEtools\Miscellaneous\NWTC_Library\trunk\source
SET NETLIB_Loc=M:\CAEtools\Miscellaneous\NWTC_Library\branches\NetLib\NWTC_source
SET ED_Loc=M:\CAETools\Simulators\FAST\branches\BJonkman\Source
SET SrvD_Loc=M:\CAETools\Simulators\FAST\branches\BJonkman\Source
SET AD_Loc=M:\CAEtools\Simulators\AeroDyn\branches\Framework\Source
SET IfW_Loc=M:\CAEtools\Simulators\InflowWind\branches\modularization\Source
SET SD_Loc=M:\CAEtools\Simulators\SubDyn\branches\v0.4\Source

SET HD_Loc=M:\CAEtools\Simulators\HydroDyn\branches\HydroDyn_Modularization\Source
SET HD_Reg_Loc=%HD_Loc%\RegistryFiles

SET MAP_Loc=M:\CAEtools\Simulators\MAP\trunk\src\fortran
SET MAP_DLL=M:\CAEtools\Simulators\MAP\trunk\executable\MAP_win32.dll
SET MAP_Include_Lib=M:\CAEtools\Simulators\MAP\trunk\executable\MAP_win32.lib

GOTO End

REM --------------------------------------------------------------------------------------------------------------------------------

:End
