@ECHO OFF

REM The calling syntax for this script is
REM                  Compile_FAST [{32 | 64}] [-RegistryOnly]
REM
REM Add the "32" to the command line to compile FAST in 32-bit.
REM Add the "64" to the command line to compile FAST in 64-bit.
REM default is 32.

rem SET BITS=%1

SET RegOnly=1
IF /I "%1"=="-REGISTRYONLY" goto SetPaths
SET RegOnly=0

REM ----------------------------------------------------------------------------
REM                   set compiler internal variables
REM ----------------------------------------------------------------------------
REM    You can run this bat file from the IVF compiler's command prompt (and not
REM    do anything in this section). If you choose not to run from the IVF command
REM    prompt, you must call the compiler's script to set internal variables.
REM    TIP: Right click on the IVF Compiler's Command Prompt shortcut, click
REM    properties, and copy the target (without cmd.exe and/or its switches) here:

SET Lines=-----

IF "%INCLUDE%"=="" (
rem we haven't called the compiler, yet

IF /I "%1"=="64" ( ECHO %Lines% Using intel64 compiler %Lines%
CALL "C:\Program Files (x86)\Intel\Composer XE 2011 SP1\bin\ipsxe-comp-vars.bat" intel64 vs2010
SET BITS=64
) ELSE ( ECHO  %Lines% Using ia32 compiler %Lines%
call "C:\Program Files (x86)\Intel\Composer XE 2011 SP1\bin\ipsxe-comp-vars.bat" ia32 vs2010
SET BITS=32
)

) ELSE (
rem we have called the compiler before

IF /I "%1"=="64" ( %Lines% Using existing compiler settings: intel64 %Lines%
SET BITS=64
) ELSE ( ECHO  %Lines% Using existing compiler settings: ia32 %Lines%
SET BITS=32
)

)


:SetPaths
REM ----------------------------------------------------------------------------
REM ------------------------- LOCAL PATHS --------------------------------------
REM ----------------------------------------------------------------------------
REM -- USERS MAY EDIT THESE PATHS TO POINT TO FOLDERS ON THEIR LOCAL MACHINES. -
REM -- NOTE: do not use quotation marks around the path names!!!! --------------
REM ----------------------------------------------------------------------------
REM ----------------------------------------------------------------------------

rem SET Registry=CALL Registry
SET Registry=..\bin\Registry_win32.exe

SET FAST_Loc=..\Source

SET NWTC_Lib_Loc=%FAST_Loc%\dependencies\NWTC_Library
SET NETLIB_Loc=%FAST_Loc%\dependencies\NetLib
SET ED_Loc=%FAST_Loc%\dependencies\ElastoDyn
SET SrvD_Loc=%FAST_Loc%\dependencies\ServoDyn
SET AD_Loc=%FAST_Loc%\dependencies\AeroDyn
SET IfW_Loc=%FAST_Loc%\dependencies\InflowWind
SET HD_Loc=%FAST_Loc%\dependencies\HydroDyn
SET SD_Loc=%FAST_Loc%\dependencies\SubDyn
SET MAP_Loc=%FAST_Loc%\dependencies\MAP

SET MAP_Include_Lib=%MAP_Loc%\map_win32.lib
SET HD_Reg_Loc=%HD_Loc%

REM ----------------------------------------------------------------------------
REM The following script changes the above paths for Bonnie Jonkman; other users
REM    can ignore these lines
IF /I "%2"=="bjonkman" CALL Set_FAST_paths.bat
IF /I "%3"=="bjonkman" CALL Set_FAST_paths.bat
REM ----------------------------------------------------------------------------


REM ----------------------------------------------------------------------------
REM -------------------- LOCAL VARIABLES ---------------------------------------
REM ----------------------------------------------------------------------------

SET ROOT_NAME=FAST_iwin%BITS%
SET INTER_DIR=Obj_iwin%BITS%

:: /nologo /fpp /stand:f03 /Qdiag-disable:5268 /traceback /libs:static /threads /Qmkl:sequential /c
:: /Qmkl:sequential is for SubDyn's use of intel's math kernel library

SET COMPOPTS=/threads  /O2 /inline:speed /traceback /real_size:32 /fpp
rem SET LINKOPTS=/link /stack:64000000
SET LINKOPTS=/link %MAP_Include_Lib%

SET LINES=++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

:SourceFiles
REM ----------------------------------------------------------------------------
REM -------------------- LIST OF ALL SOURCE FILES ------------------------------
REM ----------------------------------------------------------------------------

SET NWTC_SOURCES=^
 "%NWTC_Lib_Loc%\SingPrec.f90"^
 "%NWTC_Lib_Loc%\NWTC_Base.f90" ^
 "%NWTC_Lib_Loc%\SysIVF.f90" ^
 "%NWTC_Lib_Loc%\NWTC_Library_Types.f90" ^
 "%NWTC_Lib_Loc%\NWTC_IO.f90" ^
 "%NWTC_Lib_Loc%\NWTC_Num.f90" ^
 "%NWTC_Lib_Loc%\ModMesh_Types.f90" ^
 "%NWTC_Lib_Loc%\ModMesh.f90" ^
 "%NWTC_Lib_Loc%\ModMesh_Mapping.f90" ^
 "%NWTC_Lib_Loc%\NWTC_Library.f90"

SET NETLIB_SOURCES=^
 "%NETLIB_Loc%\DLASRT2.f"^
 "%NETLIB_Loc%\SLASRT2.f"
 "%NETLIB_Loc%\NWTC_ScaLAPACK.f90"
 "%NETLIB_Loc%\NWTC_LAPACK.f90"

SET IfW_SOURCES=^
 "%IfW_Loc%\IFW_FFWind_Types.f90" ^
 "%IfW_Loc%\IFW_FFWind.f90" ^
 "%IfW_Loc%\IFW_HHWind_Types.f90"^
 "%IfW_Loc%\IFW_HHWind.f90" ^
 "%IfW_Loc%\InflowWind_Types.f90"^
 "%IfW_Loc%\InflowWind_Subs.f90"^
 "%IfW_Loc%\InflowWind.f90"


SET AD_SOURCES=^
 "%AD_Loc%\AeroDyn_Types.f90"^
 "%AD_Loc%\GenSubs.f90"^
 "%AD_Loc%\AeroSubs.f90"^
 "%AD_Loc%\AeroDyn.f90"


SET ED_SOURCES=^
 "%ED_Loc%\ElastoDyn_Types.f90" ^
 "%ED_Loc%\ElastoDyn.f90"


SET SrvD_SOURCES=^
 "%SrvD_Loc%\ServoDyn_Types.f90" ^
 "%SrvD_Loc%\BladedInterface.f90" ^
 "%SrvD_Loc%\ServoDyn.f90" ^
 "%SrvD_Loc%\PitchCntrl_ACH.f90" ^
 "%SrvD_Loc%\UserSubs.f90" ^
 "%SrvD_Loc%\UserVSCont_KP.f90"


SET HD_SOURCES=^
 "%HD_Loc%\fftpack.f" ^
 "%HD_Loc%\FFTMod.f90" ^
 "%HD_Loc%\SS_Radiation_Types.f90" ^
 "%HD_Loc%\SS_Radiation.f90" ^
 "%HD_Loc%\Waves_Types.f90" ^
 "%HD_Loc%\Waves.f90" ^
 "%HD_Loc%\Morison_Types.f90" ^
 "%HD_Loc%\Morison_Output.f90" ^
 "%HD_Loc%\Morison.f90" ^
 "%HD_Loc%\Current_Types.f90" ^
 "%HD_Loc%\Current.f90" ^
 "%HD_Loc%\Conv_Radiation_Types.f90" ^
 "%HD_Loc%\Conv_Radiation.f90" ^
 "%HD_Loc%\WAMIT_Types.f90" ^
 "%HD_Loc%\WAMIT_Output.f90" ^
 "%HD_Loc%\WAMIT.f90" ^
 "%HD_Loc%\HydroDyn_Types.f90" ^
 "%HD_Loc%\HydroDyn_Output.f90" ^
 "%HD_Loc%\HydroDyn_Input.f90" ^
 "%HD_Loc%\HydroDyn.f90"


SET SD_SOURCES=^
 "%SD_Loc%\qsort_c_module.f90" ^
 "%SD_Loc%\SubDyn_Types.f90" ^
 "%SD_Loc%\SD_FEM.f90" ^
 "%SD_Loc%\SubDyn_Output.f90" ^
 "%SD_Loc%\SubDyn.f90"


SET MAP_SOURCES=^
 "%MAP_Loc%\MAP_C_Types.f90" ^
 "%MAP_Loc%\MAP_Types.f90"   ^
 "%MAP_Loc%\MAP.f90"


SET FAST_SOURCES=^
 "%FAST_LOC%\FAST_Mods.f90" ^
 "%FAST_LOC%\FAST_IO.f90" ^
 "%FAST_LOC%\FAST_Prog.f90"


:RunRegistry
REM ----------------------------------------------------------------------------
REM ---------------- RUN THE REGISTRY TO AUTO-GENERATE FILES -------------------
REM ----------------------------------------------------------------------------
REM note that I'm changing directories, only to put the auto-generated files in their respective locations

ECHO %Lines%
ECHO Running the FAST Registry to auto-generate source files:
ECHO.

ECHO %Lines%
SET CURR_LOC=%ED_Loc%
CALL ::RunRegistry_fmt1 ElastoDyn


ECHO %Lines%
SET CURR_LOC=%SrvD_Loc%
CALL ::RunRegistry_fmt1 ServoDyn


ECHO %Lines%
SET CURR_LOC=%IfW_Loc%
CALL :RunRegistry_IfW IfW_FFWind
CALL :RunRegistry_IfW IfW_HHWind
CALL :RunRegistry_IfW InflowWind


ECHO %Lines%
SET CURR_LOC=%AD_Loc%
SET ModuleName=AeroDyn
%REGISTRY% "%CURR_LOC%\Registry-AD.txt" -I "%NWTC_Lib_Loc%" -I "%IfW_Loc%"
MOVE /Y "%ModuleName%_Types.f90" "%CURR_LOC%"



ECHO %Lines%
SET CURR_LOC=%HD_Loc%
CALL ::RunRegistry_HD  Current
CALL ::RunRegistry_HD  Waves
CALL ::RunRegistry_HD  SS_Radiation
CALL ::RunRegistry_HD  Conv_Radiation
CALL ::RunRegistry_HD  WAMIT
CALL ::RunRegistry_HD  Morison
CALL ::RunRegistry_HD  HydroDyn


ECHO %Lines%
SET CURR_LOC=%SD_Loc%
CALL ::RunRegistry_fmt1  SubDyn


ECHO %Lines%
rem CALL :setRegistryValues MAP
REM SET CURR_LOC=%MAP_Loc%
rem need the syntax for generating the c-to-fortran code...


IF %RegOnly%==1 goto end


:ivf
REM ----------------------------------------------------------------------------
REM ---------------- COMPILE WITH INTEL VISUAL FORTRAN -------------------------
REM ----------------------------------------------------------------------------

REM                           compile
rem goto link_now


:: we'll make this intermediate directory if it doesn't exist
if not exist %INTER_DIR% mkdir %INTER_DIR%

:: and we'll clean it out so we don't get extra files when we link
if exist %INTER_DIR%\*.mod DEL %INTER_DIR%\*.mod
if exist %INTER_DIR%\*.obj DEL %INTER_DIR%\*.obj


:: ECHO %Lines%
:: ECHO Compiling FAST, AeroDyn, ElastoDyn, ServoDyn, HydroDyn, InflowWind, SubDyn, MAP, and NWTC_Library routines to create %ROOT_NAME%.exe:

rem ifort %COMPOPTS% %NWTC_Files%  %LINKOPTS% /out:%ROOT_NAME%.exe
rem NOTE that I'm compiling the modules separately then linking them later. I split it up because the list of source files was getting too long ("Input line too long" error)

ECHO %Lines%
ECHO Compiling NWTC Library:
ifort %COMPOPTS% %NWTC_SOURCES% %NETLIB_SOURCE% /Qmkl:sequential  /c /object:%INTER_DIR%\ /module:%INTER_DIR%\
IF %ERRORLEVEL% NEQ 0 GOTO checkError

ECHO %Lines%
ECHO Compiling InflowWind:
ifort %COMPOPTS% %IfW_SOURCES%  /c /object:%INTER_DIR%\ /module:%INTER_DIR%\
IF %ERRORLEVEL% NEQ 0 GOTO checkError

ECHO %Lines%
ECHO Compiling AeroDyn:
ifort %COMPOPTS% %AD_SOURCES%   /c /object:%INTER_DIR%\ /module:%INTER_DIR%\
IF %ERRORLEVEL% NEQ 0 GOTO checkError


ECHO %Lines%
ECHO Compiling ElastoDyn:
ifort %COMPOPTS% %ED_SOURCES%   /c /object:%INTER_DIR%\ /module:%INTER_DIR%\
IF %ERRORLEVEL% NEQ 0 GOTO checkError


ECHO %Lines%
ECHO Compiling ServoDyn:
ifort %COMPOPTS% %SrvD_SOURCES% /c /object:%INTER_DIR%\ /module:%INTER_DIR%\
IF %ERRORLEVEL% NEQ 0 GOTO checkError


ECHO %Lines%
ECHO Compiling HydroDyn:
ifort %COMPOPTS% %HD_SOURCES%   /c /object:%INTER_DIR%\ /module:%INTER_DIR%\
IF %ERRORLEVEL% NEQ 0 GOTO checkError


ECHO %Lines%
ECHO Compiling SubDyn:
ifort %COMPOPTS% %SD_SOURCES%  /c /object:%INTER_DIR%\ /module:%INTER_DIR%\
IF %ERRORLEVEL% NEQ 0 GOTO checkError


ECHO %Lines%
ECHO Compiling MAP:
ifort %COMPOPTS% %MAP_SOURCES%  /c /object:%INTER_DIR%\ /module:%INTER_DIR%\
IF %ERRORLEVEL% NEQ 0 GOTO checkError


ECHO %Lines%
ECHO Compiling FAST glue code:
ifort %COMPOPTS% %FAST_SOURCES% /c /object:%INTER_DIR%\ /module:%INTER_DIR%\
IF %ERRORLEVEL% NEQ 0 GOTO checkError


:link_now
ECHO %Lines%
ECHO Linking compiled modules to form %ROOT_NAME%.exe:
ifort %INTER_DIR%\*.obj %LINKOPTS% /out:%ROOT_NAME%.exe

:checkError
ECHO %Lines%
IF %ERRORLEVEL% NEQ 0 (
ECHO Error creating %ROOT_NAME%.exe
) ELSE (
ECHO %ROOT_NAME%.exe was created.

)

GOTO END

REM ----------------------------------------------------------------------------
:: Some subroutine for the registry stuff:

:RunRegistry_HD
SET ModuleName=%1
%REGISTRY% %HD_Reg_Loc%\%ModuleName%.txt -I %NWTC_Lib_Loc% -I %HD_Reg_Loc%
MOVE /Y "%ModuleName%_Types.f90" "%CURR_LOC%"
EXIT /B

:RunRegistry_fmt1
SET ModuleName=%1
%REGISTRY% %CURR_LOC%\%ModuleName%_Registry.txt -I %NWTC_Lib_Loc%
MOVE /Y "%ModuleName%_Types.f90" "%CURR_LOC%"
EXIT /B

:RunRegistry_IfW
SET ModuleName=%1
%REGISTRY% "%CURR_LOC%\Reg-%ModuleName%.txt" -I "%NWTC_Lib_Loc%" -I "%IfW_Loc%"
MOVE /Y "%ModuleName%_Types.f90" "%CURR_LOC%"
EXIT /B

:end
REM ----------------------------------------------------------------------------
REM ------------------------- CLEAR MEMORY -------------------------------------
REM ----------------------------------------------------------------------------
ECHO. 


SET BITS=
set LINES=
SET ROOT_NAME=
SET REGISTRY=

SET NWTC_Lib_Loc=
SET ED_Loc=
SET SrvD_Loc=
SET AD_Loc=
SET IfW_Loc=
SET HD_Loc=
SET SD_Loc=
SET MAP_Loc=
SET FAST_Loc=
SET MAP_Include_Lib=
SET HD_Reg_Loc=

SET NWTC_SOURCES=
SET NETLIB_SOURCES=
SET IfW_SOURCES=
SET AD_SOURCES=
SET ED_SOURCES=
SET HD_SOURCES=
SET SrvD_SOURCES=
SET SD_SOURCES=
SET MAP_SOURCES=
SET FAST_SOURCES=

SET COMPOPTS=
SET LINKOPTS=

SET INTER_DIR=
SET CURR_LOC=
SET REGONLY=
SET MODULENAME=
:Done
EXIT /B