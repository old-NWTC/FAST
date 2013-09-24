
IF "%COMPUTERNAME%"=="BJONKMAN-23080S" GOTO BJONKMAN-23080S

REM ----------------------------------------------------------------------------
REM These paths are for Bonnie Jonkman; other users should modify their own paths as necessary

:BJONKMAN-23080S
ECHO Setting paths for Bonnie Jonkman.

SET Registry=CALL Registry
SET Crunch=C:\Users\bjonkman\Documents\DATA\DesignCodes\postprocessors\Crunch\SVNdirectory\trunk\crunch_win32.exe


SET REG_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\FAST\SVNdirectory\branches\CSC\FAST\Registry\Source
SET FAST_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\FAST\SVNdirectory\branches\BJonkman\Source

SET NWTC_Lib_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\miscellaneous\nwtc_subs\SVNdirectory\trunk\source
SET ED_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\FAST\SVNdirectory\branches\BJonkman\Source
SET SrvD_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\FAST\SVNdirectory\branches\BJonkman\Source
SET AD_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\AeroDyn\SVNdirectory\branches\Framework\Source
SET IfW_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\InflowWind\SVNdirectory\branches\modularization\Source
SET SD_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\SubDyn\SVNdirectory\branches\v0.4\Source

SET HD_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\HydroDyn\SVNdirectory\branches\HydroDyn_Modularization\Source
SET HD_Reg_Loc=%HD_Loc%\RegistryFiles

SET MAP_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\MAP\SVNdirectory\trunk\src\fortran_driver
SET MAP_DLL=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\MAP\SVNdirectory\trunk\executable\MAP.dll
SET MAP_Include_Lib=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\MAP\SVNdirectory\trunk\executable\map.lib


