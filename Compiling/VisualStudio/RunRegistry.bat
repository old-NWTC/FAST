@ECHO OFF

set lines=======================================================================
echo %lines%
IF "%1"=="" (
ECHO.
ECHO   The calling syntax for this script is
ECHO             RunRegistry ModuleName
ECHO.
GOTO Done
)


REM ----------------------------------------------------------------------------
REM ------------------------- LOCAL PATHS --------------------------------------
REM ----------------------------------------------------------------------------
REM -- USERS MAY EDIT THESE PATHS TO POINT TO FOLDERS ON THEIR LOCAL MACHINES. -
REM -- NOTE: do not use quotation marks around the path names!!!! --------------
REM ----------------------------------------------------------------------------
REM ----------------------------------------------------------------------------

SET Registry=..\..\bin\Registry_win32.exe
SET FAST_Loc=..\..\Source

SET NWTC_Lib_Loc=%FAST_Loc%\dependencies\NWTC_Library
SET ED_Loc=%FAST_Loc%\dependencies\ElastoDyn
SET SrvD_Loc=%FAST_Loc%\dependencies\ServoDyn
SET AD_Loc=%FAST_Loc%\dependencies\AeroDyn
SET DWM_Loc=%AD_Loc%
SET IfW_Loc=%FAST_Loc%\dependencies\InflowWind
SET HD_Loc=%FAST_Loc%\dependencies\HydroDyn
SET SD_Loc=%FAST_Loc%\dependencies\SubDyn
SET MAP_Loc=%FAST_Loc%\dependencies\MAP
SET FEAM_Loc=%FAST_Loc%\dependencies\FEAMooring
SET IceF_Loc=%FAST_Loc%\dependencies\IceFloe
SET IceD_Loc=%FAST_Loc%\dependencies\IceDyn

SET MAP_Include_Lib=%MAP_Loc%\map.lib
SET HD_Reg_Loc=%HD_Loc%
SET IfW_Reg_Loc=%IfW_Loc%
SET FEAM_Reg_Loc=%FEAM_Loc%

IF /I "%2"=="dev" CALL ..\Set_FAST_paths.bat

SET ModuleName=%1

GOTO %ModuleName%

REM ----------------------------------------------------------------------------
REM ---------------- RUN THE REGISTRY TO AUTO-GENERATE FILES -------------------
REM ----------------------------------------------------------------------------
:FAST
ECHO on
SET CURR_LOC=%FAST_Loc%
%REGISTRY% "%CURR_LOC%\FAST_Registry.txt" -I "%NWTC_Lib_Loc%" -I "%ED_Loc%" -I "%SrvD_Loc%" -I "%AD_Loc%" -I^
 "%IfW_Reg_Loc%" -I "%DWM_LOC%" -I "%SD_Loc%" -I "%HD_Reg_Loc%" -I "%MAP_Loc%" -I "%FEAM_Reg_Loc%"  -I "%IceF_Loc%" -I "%IceD_Loc%" -noextrap
	    
echo off		
GOTO checkError


:ElastoDyn
SET CURR_LOC=%ED_Loc%
%REGISTRY% "%CURR_LOC%\%ModuleName%_Registry.txt" -I "%NWTC_Lib_Loc%"
GOTO checkError


:ServoDyn
SET CURR_LOC=%SrvD_Loc%
%REGISTRY% "%CURR_LOC%\%ModuleName%_Registry.txt" -I "%NWTC_Lib_Loc%"
GOTO checkError


:InflowWind
:IfW_FFWind
:IfW_HHWind
SET CURR_LOC=%IfW_Loc%
%REGISTRY% "%IfW_Reg_Loc%\%ModuleName%.txt" -I "%NWTC_Lib_Loc%" -I "%IfW_Reg_Loc%"
GOTO checkError


:AeroDyn
SET CURR_LOC=%AD_Loc%
%REGISTRY% "%CURR_LOC%\Registry-AD.txt" -I "%NWTC_Lib_Loc%" -I "%IfW_Reg_Loc%" -I "%AD_Loc%" -I "%DWM_Loc%"
GOTO checkError

:DWM
SET CURR_LOC=%DWM_Loc%
%REGISTRY% "%CURR_LOC%\Registry-DWM.txt" -I "%NWTC_Lib_Loc%" -I "%IfW_Reg_Loc%"
GOTO checkError

:HydroDyn
:Current
:Waves
:Waves2
:SS_Radiation
:Conv_Radiation
:WAMIT
:WAMIT2
:Morison
SET CURR_LOC=%HD_Loc%
%REGISTRY% "%HD_Reg_Loc%\%ModuleName%.txt" -I "%NWTC_Lib_Loc%" -I "%HD_Reg_Loc%"
GOTO checkError


:SubDyn
SET CURR_LOC=%SD_Loc%
%REGISTRY% "%CURR_LOC%\%ModuleName%_Registry.txt" -I "%NWTC_Lib_Loc%"
GOTO checkError

:MAP
SET CURR_LOC=%MAP_Loc%
IF /I "%2"=="dev" (
%REGISTRY% "%CURR_LOC%\%ModuleName%_Registry.txt" -ccode -I "%NWTC_Lib_Loc%"
)
GOTO checkError

:FEAMooring
SET CURR_LOC=%FEAM_Loc%
%REGISTRY% "%FEAM_Reg_LOC%\FEAM_Registry.txt" -I "%NWTC_Lib_Loc%"
GOTO checkError

:IceFloe
SET CURR_LOC=%IceF_Loc%
%REGISTRY% "%CURR_LOC%\IceFloe_FASTRegistry.inp" -I "%NWTC_Lib_Loc%"
GOTO checkError


:IceDyn
SET CURR_LOC=%IceD_Loc%
%REGISTRY% "%CURR_LOC%\Registry_%ModuleName%.txt" -I "%NWTC_Lib_Loc%"
GOTO checkError


:checkError
ECHO.
IF %ERRORLEVEL% NEQ 0 (
ECHO Error running FAST Registry for %ModuleName%.
) ELSE (
ECHO Registry for %ModuleName% completed.
COPY /Y "%ModuleName%_Types.f90"   "%CURR_LOC%"
IF /I "%ModuleName%"=="MAP" COPY /Y "%ModuleName%_C_Types.f90" "%CURR_LOC%"
)




:end
REM ----------------------------------------------------------------------------
REM ------------------------- CLEAR MEMORY -------------------------------------
REM ----------------------------------------------------------------------------
ECHO. 


SET REGISTRY=

SET NWTC_Lib_Loc=
SET ED_Loc=
SET SrvD_Loc=
SET AD_Loc=
SET DWM_Loc=
SET IfW_Loc=
SET HD_Loc=
SET SD_Loc=
SET MAP_Loc=
SET FEAM_Loc=
SET IceF_Loc=
SET ID_Loc=
SET FAST_Loc=
SET MAP_Include_Lib=
SET HD_Reg_Loc=
SET IfW_Reg_Loc=
SET FEAM_Reg_Loc=

SET ModuleName=
SET CURR_LOC=
:Done
echo %lines%
set lines=