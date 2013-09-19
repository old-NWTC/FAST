@ECHO OFF

REM The calling syntax for this script is
REM                  Compile_FAST [{32 | 64}]
REM
REM Add the "32" to the command line to compile FAST in 32-bit.
REM Add the "64" to the command line to compile FAST in 64-bit.
REM default is 32.

rem SET BITS=%1

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



REM ----------------------------------------------------------------------------
REM ------------------------- LOCAL PATHS --------------------------------------
REM ----------------------------------------------------------------------------
REM -- USERS WILL NEED TO EDIT THESE PATHS TO POINT TO FOLDERS ON THEIR LOCAL --
REM -- MACHINES.  NOTE: do not use quotation marks around the path names!!!! ---
REM ----------------------------------------------------------------------------
REM NWTC_Lib_Loc is the location of the NWTC subroutine library files
REM AD_Loc  is the location of the AeroDyn source files
REM IfW_Loc     is the location of the AeroDyn wind inflow source files
REM FAST_LOC     is the location of the FAST source files
REM ----------------------------------------------------------------------------

SET NWTC_Lib_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\miscellaneous\nwtc_subs\SVNdirectory\trunk\source
SET ED_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\FAST\SVNdirectory\branches\BJonkman\Source
SET SrvD_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\FAST\SVNdirectory\branches\BJonkman\Source
SET AD_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\AeroDyn\SVNdirectory\branches\Framework\Source
SET IfW_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\InflowWind\SVNdirectory\branches\modularization\Source
SET HD_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\HydroDyn\SVNdirectory\branches\HydroDyn_Modularization\Source
SET SD_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\SubDyn\SVNdirectory\branches\v0.4\Source
SET MAP_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\MAP\SVNdirectory\trunk\src\fortran_driver

SET HD_Reg_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\HydroDyn\SVNdirectory\branches\HydroDyn_Modularization\Source\RegistryFiles


SET FAST_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\FAST\SVNdirectory\branches\BJonkman\Source

SET MAP_Include_Lib=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\MAP\SVNdirectory\trunk\executable\map.lib

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

SET COMPOPTS=/threads  /O2 /inline:speed /traceback /Qzero /Qsave /real_size:32 /assume:byterecl /fpp
rem SET LINKOPTS=/link /stack:64000000
SET LINKOPTS=/link %MAP_Include_Lib%



REM ----------------------------------------------------------------------------
REM -------------------- LIST OF ALL SOURCE FILES ------------------------------
REM ----------------------------------------------------------------------------
:SourceFiles

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
 "%IfW_Loc%\IFW_InflowWind_Types.f90"^
 "%IfW_Loc%\IFW_InflowWind_Subs.f90"^
 "%IfW_Loc%\InflowWind.f90"


SET AD_SOURCES=^
 "%AD_Loc%\SharedTypes.f90"^
 "%AD_Loc%\AeroMods.f90"^
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


SET LINES=++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
:: ECHO %Lines%
:: ECHO Compiling FAST, AeroDyn, ElastoDyn, ServoDyn, HydroDyn, InflowWind, SubDyn, MAP, and NWTC_Library routines to create %ROOT_NAME%.exe:

rem ifort %COMPOPTS% %NWTC_Files%  %LINKOPTS% /out:%ROOT_NAME%.exe
rem NOTE that I'm compiling the modules separately then linking them later. I split it up because the list of source files was getting too long ("Input line too long" error)

ECHO %Lines%
ECHO Compiling NWTC Library:
ifort %COMPOPTS% %NWTC_SOURCES% /c /object:%INTER_DIR%\ /module:%INTER_DIR%\

ECHO %Lines%
ECHO Compiling Inflow Wind:
ifort %COMPOPTS% %IfW_SOURCES%  /c /object:%INTER_DIR%\ /module:%INTER_DIR%\

ECHO %Lines%
ECHO Compiling AeroDyn:
ifort %COMPOPTS% %AD_SOURCES%   /c /object:%INTER_DIR%\ /module:%INTER_DIR%\

ECHO %Lines%
ECHO Compiling ElastoDyn:
ifort %COMPOPTS% %ED_SOURCES%   /c /object:%INTER_DIR%\ /module:%INTER_DIR%\

ECHO %Lines%
ECHO Compiling ServoDyn:
ifort %COMPOPTS% %SrvD_SOURCES% /c /object:%INTER_DIR%\ /module:%INTER_DIR%\

ECHO %Lines%
ECHO Compiling HydroDyn:
ifort %COMPOPTS% %HD_SOURCES%   /c /object:%INTER_DIR%\ /module:%INTER_DIR%\

ECHO %Lines%
ECHO Compiling SubDyn:
ifort %COMPOPTS% %SD_SOURCES%  /Qmkl:sequential /c /object:%INTER_DIR%\ /module:%INTER_DIR%\

ECHO %Lines%
ECHO Compiling MAP:
ifort %COMPOPTS% %MAP_SOURCES%  /c /object:%INTER_DIR%\ /module:%INTER_DIR%\

ECHO %Lines%
ECHO Compiling FAST glue code:
ifort %COMPOPTS% %FAST_SOURCES% /c /object:%INTER_DIR%\ /module:%INTER_DIR%\

:link_now
ECHO %Lines%
ECHO Linking compiled modules to form %ROOT_NAME%.exe:
ifort %INTER_DIR%\*.obj %LINKOPTS% /out:%ROOT_NAME%.exe

ECHO %Lines%
IF %ERRORLEVEL% NEQ 0 (
ECHO Error creating %ROOT_NAME%.exe
) ELSE (
ECHO %ROOT_NAME%.exe was created.
)

REM ----------------------------------------------------------------------------
REM ------------------------- CLEAR MEMORY -------------------------------------
REM ----------------------------------------------------------------------------
:end
ECHO 


SET BITS=
set LINES=
SET ROOT_NAME=

SET NWTC_Lib_Loc=
SET ED_Loc=
SET SrvD_Loc=
SET AD_Loc=
SET IfW_Loc=
SET HD_Loc=
SET HD_Reg_Loc=
SET SD_Loc=
SET MAP_Loc=
SET FAST_Loc=
SET MAP_Include_Lib=

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

:Done