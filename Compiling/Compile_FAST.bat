@ECHO OFF

REM The calling syntax for this script is
REM                  Compile_FAST [dll]
REM
REM Add the "dll" to the command line to compile FAST for the Bladed-style dll.

REM ----------------------------------------------------------------------------
REM                   set compiler internal variables
REM ----------------------------------------------------------------------------
REM    You can run this bat file from the IVF compiler's command prompt (and not
REM    do anything in this section). If you choose not to run from the IVF command
REM    prompt, you must call the compiler's script to set internal variables.
REM    TIP: Right click on the IVF Compiler's Command Prompt shortcut, click
REM    properties, and copy the target (without cmd.exe and/or its switches) here:

rem IF "%INTEL_SHARED%"=="" CALL "C:\Program Files\Intel\Compiler\Fortran\10.1.024\IA32\Bin\IFORTVARS.bat"
rem CALL "C:\Program Files (x86)\Intel\ComposerXE-2011\bin\ipsxe-comp-vars.bat" ia32 vs2008
rem call "C:\Program Files (x86)\Intel\Composer XE 2011 SP1\bin\ipsxe-comp-vars.bat" ia32 vs2010
CALL "C:\Program Files (x86)\Intel\Composer XE 2011 SP1\bin\ipsxe-comp-vars.bat" intel64 vs2010


REM ----------------------------------------------------------------------------
REM -------------------- LOCAL VARIABLES ---------------------------------------
REM ----------------------------------------------------------------------------

SET ROOT_NAME=FAST_test

rem SET COMPOPTS=/threads  /O2 /inline:speed /traceback /Qzero /Qsave /real_size:32 /assume:byterecl /fpp /assume:realloc_lhs
SET COMPOPTS=/threads  /O2 /inline:speed /traceback /Qzero /Qsave /real_size:32 /assume:byterecl /fpp
rem SET LINKOPTS=/link /stack:64000000
SET LINKOPTS=/link


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
SET AD_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\FAST\SVNdirectory\branches\BJonkman\Source\ModulesNotConverted\AeroDyn
SET IfW_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\FAST\SVNdirectory\branches\BJonkman\Source\ModulesNotConverted\InflowWind
SET HD_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\HydroDyn\SVNdirectory\branches\HydroDyn_Modularization\Source
:: SET MAP_Loc

SET FAST_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\FAST\SVNdirectory\branches\BJonkman\Source

REM ----------------------------------------------------------------------------
REM The following script changes the above paths for Bonnie Jonkman; other users
REM    should modify the paths above
IF "%COMPUTERNAME%"=="BJONKMAN-23080S" CALL Set_FAST_paths.bat
REM ----------------------------------------------------------------------------


REM ----------------------------------------------------------------------------
REM -------------------- LIST OF ALL SOURCE FILES ------------------------------
REM ----------------------------------------------------------------------------
:SourceFiles

SET NWTC_Files=
SET NWTC_Files=%NWTC_Files%  "%NWTC_Lib_Loc%\SingPrec.f90"
SET NWTC_Files=%NWTC_Files%  "%NWTC_Lib_Loc%\SysIVF.f90"
SET NWTC_Files=%NWTC_Files%  "%NWTC_Lib_Loc%\NWTC_IO.f90"
SET NWTC_Files=%NWTC_Files%  "%NWTC_Lib_Loc%\NWTC_Num.f90"
SET NWTC_Files=%NWTC_Files%  "%NWTC_Lib_Loc%\ModMesh_Types.f90"
SET NWTC_Files=%NWTC_Files%  "%NWTC_Lib_Loc%\ModMesh.f90"
SET NWTC_Files=%NWTC_Files%  "%NWTC_Lib_Loc%\NWTC_Library.f90"


SET IfW_Files=
SET IfW_Files=%IfW_Files%  "%IfW_Loc%\SharedInflowDefs.f90"
SET IfW_Files=%IfW_Files%  "%IfW_Loc%\HHWind.f90"
SET IfW_Files=%IfW_Files%  "%IfW_Loc%\FFWind.f90"
SET IfW_Files=%IfW_Files%  "%IfW_Loc%\HAWCWind.f90"
SET IfW_Files=%IfW_Files%  "%IfW_Loc%\FDWind.f90"
SET IfW_Files=%IfW_Files%  "%IfW_Loc%\CTWind.f90"
SET IfW_Files=%IfW_Files%  "%IfW_Loc%\UserWind.f90"
SET IfW_Files=%IfW_Files%  "%IfW_Loc%\InflowWindMod.f90"


SET AD_Files=
SET AD_Files=%AD_Files%  "%AD_Loc%\SharedTypes.f90"
SET AD_Files=%AD_Files%  "%AD_Loc%\AeroMods.f90"
SET AD_Files=%AD_Files%  "%AD_Loc%\GenSubs.f90"
SET AD_Files=%AD_Files%  "%AD_Loc%\AeroSubs.f90"
SET AD_Files=%AD_Files%  "%AD_Loc%\AeroDyn.f90"


SET HD_Files=
SET HD_Files=%HD_Files%  "%HD_Loc%\fftpack.f"
SET HD_Files=%HD_Files%  "%HD_Loc%\FFTMod.f90"
SET HD_Files=%HD_Files%  "%HD_Loc%\SharedDataTypes.f90"
SET HD_Files=%HD_Files%  "%HD_Loc%\Waves.f90"
SET HD_Files=%HD_Files%  "%HD_Loc%\UserLine.f90"
SET HD_Files=%HD_Files%  "%HD_Loc%\FixedBottomSupportStructure.f90"
SET HD_Files=%HD_Files%  "%HD_Loc%\FloatingPlatform.f90"
SET HD_Files=%HD_Files%  "%HD_Loc%\Hydro_IO.f90"
SET HD_Files=%HD_Files%  "%HD_Loc%\HydroDyn.f90"


SET ED_Files=
SET ED_Files=%ED_Files%  "%ED_Loc%\ElastoDyn_Types.f90"
SET ED_Files=%ED_Files%  "%ED_Loc%\ElastoDyn.f90"


SET SrvD_Files=
SET SrvD_Files=%SrvD_Files%  "%SrvD_Loc%\ServoDyn_Types.f90"
SET SrvD_Files=%SrvD_Files%  "%SrvD_Loc%\Windows_defs_IVF.f90"
SET SrvD_Files=%SrvD_Files%  "%SrvD_Loc%\BladedInterface.f90"
SET SrvD_Files=%SrvD_Files%  "%SrvD_Loc%\ServoDyn.f90"
SET SrvD_Files=%SrvD_Files%  "%SrvD_Loc%\PitchCntrl_ACH.f90"
SET SrvD_Files=%SrvD_Files%  "%SrvD_Loc%\UserSubs.f90"
SET SrvD_Files=%SrvD_Files%  "%SrvD_Loc%\UserVSCont_KP.f90"


SET FAST_Files=
SET FAST_Files=%FAST_Files%  "%FAST_LOC%\FAST_Mods.f90"
SET FAST_Files=%FAST_Files%  "%FAST_LOC%\FAST_IO.f90"
SET FAST_Files=%FAST_Files%  "%FAST_LOC%\FAST_Prog.f90"



:ivf
REM ----------------------------------------------------------------------------
REM ---------------- COMPILE WITH INTEL VISUAL FORTRAN -------------------------
REM ----------------------------------------------------------------------------

REM                           compile

ECHO.
ECHO Compiling FAST, AeroDyn, ElastoDyn, ServoDyn, HydroDyn, InflowWind and NWTC_Library routines to create %ROOT_NAME%.exe:

rem ifort %COMPOPTS% %NWTC_Files%  %LINKOPTS% /out:%ROOT_NAME%.exe

ifort %COMPOPTS% %NWTC_Files% %IfW_Files% %AD_Files% %HD_Files% %ED_Files% %SrvD_Files% %FAST_Files% %LINKOPTS% /out:%ROOT_NAME%.exe

:end
REM ----------------------------------------------------------------------------
REM ------------------------- CLEAR MEMORY -------------------------------------
REM ------------- and delete all .mod and .obj files ---------------------------
REM ----------------------------------------------------------------------------
ECHO 

DEL *.mod
DEL *.obj

SET ROOT_NAME=
SET COPTS=

SET NWTC_Files=
SET IfW_Files=
SET AD_Files=
SET HD_Files=
SET ED_Files=
SET SrvD_Files=
SET FAST_Files=

SET NWTC_Lib_Loc=
SET IfW_Loc=
SET AD_Loc=
SET FAST_Loc=
SET ED_Loc=
SET HD_Loc=
SET SrvD_Loc=

SET COMPOPTS=
SET LINKOPTS=