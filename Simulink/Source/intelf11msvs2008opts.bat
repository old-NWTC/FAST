@echo off
rem INTELF10MSVS2005OPTS.BAT
rem
rem    Compile and link options used for building MEX-files using the
rem    Intel® Fortran Compiler 11.1 with the Microsoft® Visual Studio®
rem    2008 linker.
rem
rem StorageVersion: 1.0
rem FortrankeyFileName: INTELF11MSVS2008OPTS.BAT
rem FortrankeyName: Intel Visual Fortran
rem FortrankeyManufacturer: Intel
rem FortrankeyVersion: 11.1
rem FortrankeyLanguage: Fortran
rem
rem    $Revision: 1.1.6.1 $  $Date: 2007/12/06 13:15:25 $
rem    $Revision: 5 Aug 2008, B. Jonkman (modified for VS71 instead of VS80)
rem    $Revision: 31 Mar 2010, B. Jonkman (modified for VS90)
rem
rem ********************************************************************
rem General parameters
rem ********************************************************************
REM BJJ: set MATLAB=%MATLAB%
set MATLAB=C:\Program Files\MATLAB\R2009a

rem set IFORT_COMPILER11=%IFORT_COMPILER11%
rem set VS90COMNTOOLS=%VS90COMNTOOLS%
rem set LINKERDIR=%VS90COMNTOOLS%\..\..

set IFORT_COMPILER11=C:\Program Files\Intel\Compiler\11.1\051
set VSINSTALLDIR=C:\Program Files\Microsoft Visual Studio 9.0\VC
set PATH=%IFORT_COMPILER11%\Bin\ia32;%VSINSTALLDIR%\Common7\IDE;%VCINSTALLDIR%\BIN;%VSINSTALLDIR%\Common7\Tools;%VSINSTALLDIR%\Common7\Tools\bin;%VCINSTALLDIR%\PlatformSDK\bin;%PATH%

set INCLUDE=%IFORT_COMPILER11%\Include;%IFORT_COMPILER11%\Include\IA32;%VCINSTALLDIR%\atlmfc\include;%VCINSTALLDIR%\include;%VCINSTALLDIR%\PlatformSDK\include;%INCLUDE%
set LIB=%IFORT_COMPILER11%\Lib\IA32;%VCINSTALLDIR%\atlmfc\lib;%VCINSTALLDIR%\lib;%VCINSTALLDIR%\PlatformSDK\lib;%MATLAB%\extern\lib\win32;%LIB%
set MW_TARGET_ARCH=win32


rem---------------- set up MKL stuff -----------------
set LIB=%IFORT_COMPILER11%\mkl\ia32\lib;%LIB%
set INCLUDE=%IFORT_COMPILER11%\mkl\include;%INCLUDE%
set PATH=%IFORT_COMPILER11%\mkl\ia32\bin;%PATH%
set LIBRARY_PATH=%IFORT_COMPILER11%\mkl\ia32\lib;%LIBRARY_PATH%
set CPATH=%IFORT_COMPILER11%\mkl\include;%CPATH%
set FPATH=%IFORT_COMPILER11%\mkl\include;%FPATH%
rem-------------------------------


rem ********************************************************************
rem Compiler parameters
rem ********************************************************************
set COMPILER=ifort
rem set COMPFLAGS= /Qprec "/I%MATLAB%/extern/include" -c -nologo -DMATLAB_MEX_FILE /fp:source
set COMPFLAGS= /assume:byterecl /traceback /Qsave /Qzero /real_size:64 /Qprec "/I%MATLAB%/extern/include" -c -nologo -DMATLAB_MEX_FILE /fp:source
rem set COMPFLAGS= /assume:byterecl /traceback /Qsave /Qprec /Qzero "/I%MATLAB%/extern/include" -c -nologo -DMATLAB_MEX_FILE /fp:source
set OPTIMFLAGS=-Ox -DNDEBUG
set DEBUGFLAGS=/Z7
set NAME_OBJECT=/Fo

rem ********************************************************************
rem Linker parameters
rem ********************************************************************
set LIBLOC=%MATLAB%\extern\lib\win32\microsoft
set LINKER=link
set LINKFLAGS=/DLL /EXPORT:MEXFUNCTION /LIBPATH:"%LIBLOC%" libmx.lib libmex.lib libmat.lib /implib:"%LIB_NAME%.x" /MAP:"%OUTDIR%%MEX_NAME%%MEX_EXT%.map" /NOLOGO /INCREMENTAL:NO
set LINKOPTIMFLAGS=
set LINKDEBUGFLAGS=/debug /PDB:"%OUTDIR%%MEX_NAME%%MEX_EXT%.pdb"
set LINK_FILE=
set LINK_LIB=
set NAME_OUTPUT=/out:"%OUTDIR%%MEX_NAME%%MEX_EXT%"
set RSP_FILE_INDICATOR=@

rem ********************************************************************
rem Resource compiler parameters
rem ********************************************************************
set RC_COMPILER=rc /fo "%OUTDIR%mexversion.res"
set RC_LINKER=

set POSTLINK_CMDS=del "%OUTDIR%%MEX_NAME%%MEX_EXT%.map"
set POSTLINK_CMDS1=del "%LIB_NAME%.x" "%LIB_NAME%.exp"
rem set POSTLINK_CMDS2=mt -outputresource:"%OUTDIR%%MEX_NAME%%MEX_EXT%";2 -manifest "%OUTDIR%%MEX_NAME%%MEX_EXT%.manifest"
set POSTLINK_CMDS3=del "%OUTDIR%%MEX_NAME%%MEX_EXT%.manifest" 
