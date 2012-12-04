@echo off

SET Compare=FC
rem set FAST=Y:\Wind\WindWeb\public\jjonkman\FAST\FAST_v7.00.01a-bjj_AeroDyn_v13.00.00a-bjj_BladedDLLInterface.exe
SET FAST=..\..\..\FAST_forBladedDLL.exe
SET ADAMS=Call ADAMS08


SET Editor=NotePad.EXE

SET TEST01=NRELOffshrBsline5MW_Onshore
SET DASHES=---------------------------------------------------------------------------------------------
SET TESTDIR=..\..\TstFiles

echo FAST %TEST01%


rem --------------------------------------------------------------------
rem ------------- Run FAST ---------------------------------------------
rem --------------------------------------------------------------------

rem : running 32-bit FAST:
copy DISCON_win32.dll DISCON.dll

%FAST% %TEST01%.fst



IF ERRORLEVEL 1  GOTO ERROR
IF NOT EXIST %TEST01%.out  GOTO ERROR

IF EXIST CertTest.out  DEL CertTest.out
echo.                                                    >> CertTest.out
echo %TEST01%                                            >> CertTest.out
echo %DASHES%                                            >> CertTest.out
%Compare% %TEST01%.fsm %TESTDIR%\%TEST01%.fsm            >> CertTest.out
echo %DASHES%                                            >> CertTest.out
%Compare% %TEST01%.out %TESTDIR%\%TEST01%.out            >> CertTest.out
echo %DASHES%                                            >> CertTest.out
%Compare% %TEST01%.opt %TESTDIR%\%TEST01%.opt            >> CertTest.out

rem --------------------------------------------------------------------
rem ------------- Run ADAMS --------------------------------------------
rem --------------------------------------------------------------------

rem : running 64-bit ADAMS:
copy DISCON_win64.dll DISCON.dll


%ADAMS% %TEST01%_ADAMS DLL

IF NOT EXIST %TEST01%_ADAMS.plt  GOTO ERROR

echo %DASHES%                                            >> CertTest.out
%Compare% %TEST%_ADAMS.plt %TESTDIR%\%TEST01%_ADAMS.plt    >> CertTest.out

rem --------------------------------------------------------------------

%Editor% CertTest.out
GOTO END

rem --------------------------------------------------------------------
:ERROR
echo ** An error has occurred in Test01: %TEST01% **
rem --------------------------------------------------------------------

:END

@SET Compare=
@SET DASHES=
@SET Editor=
@SET FAST=
@SET ADAMS=
@SET TEST01=

