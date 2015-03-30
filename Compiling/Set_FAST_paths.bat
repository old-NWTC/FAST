
IF "%COMPUTERNAME%"=="BJONKMAN-23080S" GOTO BJONKMAN-23080S
IF "%COMPUTERNAME%"=="BJONKMAN-20916S" GOTO BJONKMAN-20916S
:: IF "%COMPUTERNAME%"=="MBUHL-20665S" GOTO MBUHL-20665S

REM --------------------------------------------------------------------------------------------------------------------------------
REM These paths are for Bonnie Jonkman; other users should modify their own paths as necessary.

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
SET TMD_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\TMD\SVNdirectory\Source

SET AD_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\AeroDyn\SVNdirectory\trunk\Source
rem SET AD_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\AeroDyn\SVNdirectory\branches\DWM_fromOct3\Source
SET DWM_Loc=%AD_Loc%\DWM

SET IfW_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\InflowWind\SVNdirectory\branches\modularization\Source
SET IfW_Reg_Loc=%IfW_Loc%\Registry

SET SD_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\SubDyn\SVNdirectory\trunk\Source

SET HD_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\HydroDyn\SVNdirectory\trunk\Source
SET HD_Reg_Loc=%HD_Loc%\RegistryFiles

SET MAP_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\MAP\SVNdirectory\trunk
SET MAP_Loc_R=%MAP_Loc%\registry
SET MAP_Loc_F=%MAP_Loc%\fortran_driver
SET MAP_DLL=%MAP_Loc%\bin\MAP_Win32.dll
SET MAP_DLL64=%MAP_Loc%\bin\MAP_x64.dll
SET MAP_Include_Lib=%MAP_Loc%\bin\MAP_Win32.lib

SET MD_LOC=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\MoorDyn\GitforSVN\branches\matt_current\source

rem ECHO BONNIE: FIX THE FEAM LOCATION!!!
rem SET FEAM_Loc=%FAST_Loc%\dependencies\FEAMooring
SET FEAM_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\FEAMooring\SVNdirectory\source\
SET FEAM_Reg_Loc=%FEAM_Loc%

SET IceF_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\IceFloe\SVNDirectory\IceFloe\source
SET IceF_RanLux_Loc=%IceF_Loc%\ranlux

SET IceD_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\IceDyn\SVNDirectory\source


GOTO End
REM --------------------------------------------------------------------------------------------------------------------------------
:BJONKMAN-20916S

SET REG_Loc=C:\Users\bjonkman\Documents\CAETools\FAST_Registry
SET Registry=%REG_Loc%\registry.exe

SET FAST_Loc=C:\Users\bjonkman\Documents\CAETools\FASTv8\Source


SET ED_Loc=%FAST_Loc%
SET SrvD_Loc=%FAST_Loc%

SET HD_Loc=C:\Users\bjonkman\Documents\CAETools\HydroDyn\Source
SET HD_Reg_Loc=%HD_Loc%\RegistryFiles

SET TMD_Loc=C:\Users\bjonkman\Documents\CAETools\TMD\Source
:: SET MAP_DLL=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\MAP\SVNdirectory\trunk\executable\MAP_win32.dll


REM --------------------------------------------------------------------------------------------------------------------------------
:End
