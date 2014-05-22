@ECHO OFF
@ECHO.


REM  Set up environment variables.  You will probably have to change these.

@SET Compare=FC /T


@SET FAST=FAST_v7.02.00d-bjj_AeroDyn_v13.00.02a-bjj.exe
@SET FAST_DLL=FAST_v7.02.00d-bjj_AeroDyn_v13.00.02a-bjj_BladedDLLInterface.exe
@SET FAST_OC3=FAST_v7.02.00d-bjj_AeroDyn_v13.00.02a-bjj_BladedDLLInterface_OC3Hywind.exe


SET TEST18DIR=CasesWithDLL\NRELOffshrBsline5MW_Onshore
SET TEST19DIR=CasesWithDLL\NRELOffshrBsline5MW_Monopile_RF
SET TEST22DIR=CasesWithDLL\NRELOffshrBsline5MW_ITIBarge4
SET TEST23DIR=CasesWithDLL\NRELOffshrBsline5MW_Floating_TLP
SET TEST24DIR=CasesWithDLL\NRELOffshrBsline5MW_OC3Hywind


rem ***********************************************************************************************************************

@CALL :RunFAST 01
@CALL :RunFAST 02
@CALL :RunFAST 03
@CALL :RunFAST 04
@CALL :RunFAST 05
@CALL :RunFAST 06
@CALL :RunFAST 07
@CALL :RunFAST 08
@CALL :RunFAST 09
@CALL :RunFAST 10
@CALL :RunFAST 11
@CALL :RunFAST 12
@CALL :RunFAST 13

@CALL :RunFAST 15
@CALL :RunFAST 16
@CALL :RunFAST 17

SET CURRDIR=%Test18DIR%
@CALL :RunFASTDLL 18

SET CURRDIR=%Test19DIR%
@CALL :RunFASTDLL 19
@CALL :RunFASTDLL 19_noHD


:: @CALL :RunFASTDLL 20
:: @CALL :RunFASTDLL 21

SET CURRDIR=%Test22DIR%
@CALL :RunFASTDLL 22

SET CURRDIR=%Test23DIR%
@CALL :RunFASTDLL 23

SET CURRDIR=%Test24DIR%
@CALL :RunFASTDLL_OC3 24

:: @CALL :RunFASTDLL 25


GOTO END

rem ***********************************************************************************************************************
:RunFAST

SET Test=%1
SET CURRTEST=Test%1

echo -------------------------------------------------------------------
Echo Running Test %1: %CURRTEST%
echo -------------------------------------------------------------------

%FAST% %CURRTEST%.fst

IF ERRORLEVEL 1  GOTO ERROR

EXIT /B

rem ***********************************************************************************************************************
:RunFASTDLL

SET Test=%1
SET CURRTEST=Test%1

echo -------------------------------------------------------------------
Echo Running Test %1: %CURRTEST%
echo -------------------------------------------------------------------

IF /I "%2"=="DEBUG"  (
copy       "C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\FAST\IVF Projects\DISCON_dll\Release\DISCON_dll.dll" DISCON.DLL
) ELSE (
copy       %CURRDIR%\DISCON_win32.dll DISCON.DLL
)

%FAST_DLL% %CURRDIR%\%CURRTEST%.fst

IF ERRORLEVEL 1  GOTO ERROR

copy       %CURRDIR%\%CURRTEST%.out    %CURRTEST%.out
copy       %CURRDIR%\%CURRTEST%.outb   %CURRTEST%.outb
copy       %CURRDIR%\%CURRTEST%.fsm    %CURRTEST%.fsm
copy       %CURRDIR%\%CURRTEST%.elm    %CURRTEST%.elm
copy       %CURRDIR%\%CURRTEST%.opt    %CURRTEST%.opt


EXIT /B

rem ***********************************************************************************************************************
:RunFASTDLL_OC3

SET Test=%1
SET CURRTEST=Test%1

echo -------------------------------------------------------------------
Echo Running Test %1: %CURRTEST%
echo -------------------------------------------------------------------

copy       %CURRDIR%\DISCON_win32.dll DISCON.DLL
%FAST_OC3% %CURRDIR%\%CURRTEST%.fst

IF ERRORLEVEL 1  GOTO ERROR

copy       %CURRDIR%\%CURRTEST%.out    %CURRTEST%.out
copy       %CURRDIR%\%CURRTEST%.outb   %CURRTEST%.outb
copy       %CURRDIR%\%CURRTEST%.fsm    %CURRTEST%.fsm
copy       %CURRDIR%\%CURRTEST%.elm    %CURRTEST%.elm
copy       %CURRDIR%\%CURRTEST%.opt    %CURRTEST%.opt


EXIT /B
rem ***********************************************************************************************************************
:ERROR
:: Sets clears memory and stops the batch immediately
@echo ** An error has occurred in Test #%TEST% **
@echo ** An error has occurred in Test #%TEST% ** >> %CompareFile%

@call :end
@call :__ErrorExit 2> nul
EXIT /B

rem ***********************************************************************************************************************

:__ErrorExit
rem Creates a syntax error, stops immediately
()
EXIT /B
rem ***********************************************************************************************************************


:END

@SET CRUNCH=
@SET MATLAB=
@SET MBC_SOURCE=
@SET Compare=
@SET DateTime=
@SET Editor=
@SET FAST=
@SET FAST_DLL=
@SET FAST_OC3=
@SET TEST=


SET CURRTEST=
SET CURRDIR=

type Bell.txt
@echo Processing complete.


call Update.bat
