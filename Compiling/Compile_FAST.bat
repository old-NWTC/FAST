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
RegOnly=0
ECHO 'TEST'

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
SET ED_Loc=%FAST_Loc%\dependencies\ElastoDyn
SET SrvD_Loc=%FAST_Loc%\dependencies\ServoDyn
SET AD_Loc=%FAST_Loc%\dependencies\AeroDyn
SET IfW_Loc=%FAST_Loc%\dependencies\InflowWind
SET HD_Loc=%FAST_Loc%\dependencies\HydroDyn
SET SD_Loc=%FAST_Loc%\dependencies\SubDyn
SET MAP_Loc=%FAST_Loc%\dependencies\MAP

SET MAP_Include_Lib=%MAP_Loc%\map.lib
SET HD_Reg_Loc=%HD_Loc%

REM ----------------------------------------------------------------------------
REM The following script changes the above paths for Bonnie Jonkman; other users
REM    should modify the paths above
IF "%COMPUTERNAME%"=="BJONKMAN-23080S" CALL Set_FAST_paths.bat
REM ----------------------------------------------------------------------------


REM ----------------------------------------------------------------------------
REM -------------------- LOCAL VARIABLES ---------------------------------------
REM ----------------------------------------------------------------------------

SET ROOT_NAME=FAST_iwin%BITS%
SET INTER_DIR=Obj_iwin%BITS%

:: /nologo /fpp /stand:f03 /Qdiag-disable:5268 /traceback /libs:static /threads /Qmkl:sequential /c
:: /Qmkl:sequential is for SubDyn's use of intel's math kernel library

SET COMPOPTS=/threads  /O2 /inline:speed /traceback /real_size:32 /assume:byterecl /fpp
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
 "%HD_Loc%\HydroDyn_Output_Types.f90" ^
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
 "%SD_Loc%\DLASRT2.f90" ^
 "%SD_Loc%\KB07AD.f" ^
 "%SD_Loc%\METIS_NODEND_dummy.f" ^
 "%SD_Loc%\EA16AD.f" ^
 "%SD_Loc%\MA57D_dependency.f" ^
 "%SD_Loc%\MA57D.f" ^
 "%SD_Loc%\HSL_ZD11_double.f90" ^
 "%SD_Loc%\hsl_ma57_double.f90" ^
 "%SD_Loc%\EA16_INTERFACE.f90" ^
 "%SD_Loc%\HSL_M57_INTERFACE.f90" ^
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

SET CURRDIR=%CD%


ECHO %Lines%
ECHO ElastoDyn
CD /D %ED_Loc%
%REGISTRY% ElastoDyn_Registry.txt -I %NWTC_Lib_Loc%

ECHO %Lines%
ECHO ServoDyn
CD /D %SrvD_Loc%
%REGISTRY% ServoDyn_Registry.txt -I %NWTC_Lib_Loc%

ECHO %Lines%
ECHO InflowWind
CD /D %IfW_Loc%
%REGISTRY% Reg-IfW_FFWind.txt -I %NWTC_Lib_Loc%
%REGISTRY% Reg-IfW_HHWind.txt -I %NWTC_Lib_Loc%
%REGISTRY% Reg-InflowWind.txt -I %NWTC_Lib_Loc%

ECHO %Lines%
ECHO AeroDyn
CD /D %AD_Loc%
%REGISTRY% Registry-AD.txt -I %NWTC_Lib_Loc% -I %IfW_Loc%

ECHO %Lines%
ECHO HydroDyn
CD /D %HD_Loc%
%REGISTRY% %HD_Reg_Loc%\Current.txt -I %NWTC_Lib_Loc% -I %HD_Reg_Loc%
%REGISTRY% %HD_Reg_Loc%\Waves.txt -I %NWTC_Lib_Loc% -I %HD_Reg_Loc%
%REGISTRY% %HD_Reg_Loc%\SS_Radiation.txt -I %NWTC_Lib_Loc% -I %HD_Reg_Loc%
%REGISTRY% %HD_Reg_Loc%\Conv_Radiation.txt -I %NWTC_Lib_Loc% -I %HD_Reg_Loc%
%REGISTRY% %HD_Reg_Loc%\WAMIT.txt -I %NWTC_Lib_Loc% -I %HD_Reg_Loc%
%REGISTRY% %HD_Reg_Loc%\Morison.txt -I %NWTC_Lib_Loc% -I %HD_Reg_Loc%
%REGISTRY% %HD_Reg_Loc%\HydroDyn.txt -I %NWTC_Lib_Loc% -I %HD_Reg_Loc%

ECHO %Lines%
ECHO SubDyn
CD /D %SD_Loc%
%REGISTRY% "SubDyn_Registry.txt" -I %NWTC_Lib_Loc%

ECHO %Lines%
rem ECHO MAP
REM CD /D %MAP_Loc%
rem need the syntax for generating the c-to-fortran code...

CD %CURRDIR%

IF %RegOnly%==1 goto end

echo 'here'

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
ifort %COMPOPTS% %NWTC_SOURCES% /c /object:%INTER_DIR%\ /module:%INTER_DIR%\
IF %ERRORLEVEL% NEQ 0 GOTO checkError

ECHO %Lines%
ECHO Compiling Inflow Wind:
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
ifort %COMPOPTS% %SD_SOURCES%  /Qmkl:sequential /c /object:%INTER_DIR%\ /module:%INTER_DIR%\
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

:end
REM ----------------------------------------------------------------------------
REM ------------------------- CLEAR MEMORY -------------------------------------
REM ----------------------------------------------------------------------------
ECHO 


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
SET CURRDIR=
SET REGONLY=
:Done