# Compiling FAST v8.17.00a-bjj
Bonnie Jonkman

November 11, 2016

## Introduction
FAST is distributed with several options for compiling the source code. It contains Visual Studio project (XML) files and a makefile for gfortran. The FAST 
archive contains all of the source files necessary to compile, and the compiling tools are set up with the correct paths to those files. The tools also set up all the compiling 
options required to compile FAST.

Future versions will likely be distributed with cmake.

The compiling tools distributed with FAST were created primarily for Windows&reg; systems. However, the makefile can be used on non-Windows&reg; systems. Please see 
[Compiling and Running on Linux or Mac OS](#NonWindowsCompile)
at the end of this document for some suggestions.

We have developed these instructions with the following two compilers:
- Intel&reg; Visual Fortran Compiler XE 15.0.6.285
  
  Intel Math Kernel Library (MKL) 11.2 Update 4 (The MKL is used only for LAPACK routines.)
  
  Microsoft Visual Studio Community 2013, Version 12.0.31101.00 Update 4
- gfortran/gcc version 4.6.2
  mingw32
  
## Supported Compilers
We support gfortran/gcc and Intel Fortran compilers. The gcc compiler must be at least version 4.6.0. Intel Fortran should be version 2013 or later, though earlier versions can be supported with 
minor modifications to the source code.

## Compiling with Intel Fortran in Visual Studio
Open "FAST.sln" in the {FAST8}/Compiling/VisualStudio/FAST/ directory of the FAST v8 archive. This should open a Solution Explorer window that lists all of the source files and Registry input 
files for FAST v8. Most of the source files are in a Visual Studio project called "FASTlib". These source files are compiled as a library that is common to both the stand-alone executable 
(FAST.vfproj) and the FAST-Simulink interface (FAST_Library.vfproj) projects.

Choose either Debug or Release configuration from the Configuration manager (Debug is the default), and then select "Build Solution" from the "Build" menu. This will run the FAST Registry 
(when necessary) to produce the \*_Types.f90 files needed in the project(s) and then will compile and link the solution. When it has successfully completed, it will place an executable called 
"FAST_Win32.exe" (Release mode) or "FAST_Win32_debug.exe" (Debug mode) in the {FAST8}/bin directory. (Note that if you are using an older version of Intel Visual Fortran, the executable might 
be placed in different directories [i.e., Visual Studio's default output directories]. This is a result of differences in formats between versions of Intel Visual Fortran integrations with Visual Studio.).

Note that sometimes the Registry will create a \*_Types.f90 file that does not get compiled when building the solution. If that happens, you will need to select "Build Solution" a second time, 
so the \*_Types.f90 file will compile.

Note that the FASTlib project requires that you have the Intel&reg; Math Kernel Library (MKL) to access LAPACK routines. MKL comes with newer versions of Intel&reg; Visual Fortran (IVF), however some old 
versions of IVF required a separate license for MKL. If you have the Intel compiler without access to MKL, see the [LAPACK Libraries](#LAPACK) section of this document, and see instructions at this 
web site: http://icl.cs.utk.edu/lapack-for-windows/lapack/ for help in installing LAPACK and BLAS libraries for Release and Debug modes.

## Compiling with gfortran for Windows
The makefile in the {FAST8}/Compiling directory can be used to compile FAST v8 using gfortran. 
gfortran (gcc) for Windows will require you to download the LAPACK and BLAS binary files, which you can get here: http://icl.cs.utk.edu/lapack-for-windows/index.html. For 32-bit Windows, you will need these files:
- http://icl.cs.utk.edu/lapack-for-windows/libraries/VisualStudio/3.5.0/Dynamic-MINGW/Win32/liblapack.dll
- http://icl.cs.utk.edu/lapack-for-windows/libraries/VisualStudio/3.5.0/Dynamic-MINGW/Win32/liblapack.lib
- http://icl.cs.utk.edu/lapack-for-windows/libraries/VisualStudio/3.5.0/Dynamic-MINGW/Win32/libblas.dll
- http://icl.cs.utk.edu/lapack-for-windows/libraries/VisualStudio/3.5.0/Dynamic-MINGW/Win32/libblas.lib

When you use gfortran for Windows, you will need to make sure these .dll files are on your Windows PATH so that the executables will run. The .lib files are required for linking. 
[*edit: I've been told that gfortran v5.\* has some trouble linking with the above .lib files, but that it works to link with the .dll file instead.*]

To use the LAPACK and BLAS libraries in gfortran, use linking options `-llapack` and `-lblas`. (if you are having trouble, this web site may also provide some useful information: http://www.math.utah.edu/software/lapack.html) 


## Known compiling issues
- The version of gfortran must be at least 4.6.0. NWTC Library uses some quad-precision variables, which are not supported in earlier versions of gfortran.
- Intel Fortran version 10 and earlier does not provide the gamma() function. You can write your own gamma() function in the SysI\*.f90 files, or remove the calls to gamma() (e.g., set nwtc_gamma = 1) 
  and avoid using IceDyn and the wave spreading features of HydroDyn.

## Compiling and Linking Options
**The options required to compile and link FAST are already set in the compiling tools distributed with the code.** However, it is sometimes useful to modify the options for debugging or other purposes.
[Table 1](#Table1) lists several useful compiling options. The second column of the table indicates which options are required to compile FAST.

We also require some additional linking options:
- You must link with the MAP library. On Windows, this means adding MAP_Win32.lib (or MAP_x64.lib) to the linking command. On non-Windows machines, this means adding 
  libmap {version}.so to the linking command.
- Some parts of the code place large arrays on the stack, and we have found that on Win32 systems, we sometimes need to increase the stack reserve size. The command option is:

     ```/STACK:999999999``` (IVF)
	 
     ```-Wl,--stack=999999999``` (gfortran)
	 
  There is a 1-GB limit on stack size. If you go beyond that limit, you will see errors when you try to run the code. When using the OrcaFlex interface, you will need to ensure that 
  the stack reserve size is small enough to accommodate the number of threads being used in OrcaFlex. [*edit: I have modified many places where stack overflow occurred, so it is probably
  not necessary to increase the stack size any more.*]
- Also on some large models (e.g., OC4 Jacket), we can exceed the 2-GB Windows memory limit for 32-bit processes. We can extend this limit when running the application on 64-bit 
  architectures by adding the option to Enable Large Addresses.
  
    ```/LARGEADDRESSAWARE``` (IVF)
	
    ```-Wl,--large-address-aware``` (gfortran)

If you compile a 64-bit version of FAST, you can omit the ```/STACK``` and ```/LARGEADDRESSAWARE``` options.

###### <a name="Table1"></a>Table 1: Fortran Compiling/Linking Options

 | Category/Description                                           | Used in FAST?                  | Intel Fortran (Windows)   | Intel Fortran (Linux and Mac OS)   | GNU Fortran       |
 |----------------------------------------------------------------|--------------------------------|---------------------------|------------------------------------|-------------------|
  `Data options`                                                  |                                |                           |                                    |                   | 
  Define all default real (and complex) variables as 4 bytes long | Recommended (compiler default) | /real-size:32             | -real-size 32                      | (set by default)  | 
  Define all default real (and complex) variables as 8 bytes long |                                | /real-size:64             | -real-size 64                      | -fdefault-real-8  | 
  `Optimization options`                                          |                                |                           |                                    |                   | 
  Disable all optimizations (debug mode)                          | Recommended for Debug          | /Od                       | -O0                                | -O0               | 
  Enable optimizations for speed (release mode)                   | Recommended for Release        | /O2                       | -O2                                | -O2               | 
  Enable higher optimizations (may set other options; may not be appropriate for all codes) |      | /O3                       | -O3                                | -O3               | 
  `External libraries`                                            |                                |                           |                                    |                   | 
  Use optimized LAPACK routines                                   | Required                       | /Qmkl:sequential          | -mkl=sequential                    | -llapack -lblas   |
  `Debugging options`                                             |                                |                           |                                    |                   | 
  Provide source file traceback information when a severe error occurs at run time |               | /traceback                | -traceback                         | -fbacktrace       |
  Check array subscripts                                          |                                | /check:bounds             | -check bounds                      | -fcheck=bounds    |
  `Fortran Dialect Options`                                       |                                |                           |                                    |                   | 
  Produce warning/error for non-standard Fortran 2003 code        | Recommended                    | /stand:f03                | -std03                             | -std=f2003        |
  Allow free-format code to exceed 132 columns                    | Required                       | (allowed by default)      | (allowed by default)               | -ffree-line-length-none |
  `Other`                                                         |                                |                           |                                    |                   | 
  Display compiler version information                            |                                | /logo                     | -logo (or -V)                      | -v (or --version) | 
  Preprocess source files before compilation                      | Required                       | /fpp                      | -fpp                               | -x f95-cpp-input  |
  Create 32-bit code                      |  | (use appropriate Visual Studio configuration or call appropriate script prior to using compiler from command line) | | -m32              |
  Create 64-bit code                      |  | (use appropriate Visual Studio configuration or call appropriate script prior to using compiler from command line) | | -m64              |


## Double Precision
To compile FAST in double precision:
- Use the preprocessor definition DOUBLE_PRECISION (for the NWTC Subroutine Library). This can be done from the command line by adding `-DDOUBLE_PRECISION' to the compile options, 
  or using the appropriate drop-down box in the Visual Studio GUI.
- Some of the Third Party code we use also requires setting the default Real and Double KINDs to 8 and 16 bytes, respectively. (Default REAL must be the same 
  as ReKi and Default DOUBLE must be the same as DbKi.)
 

## <a name="LAPACK"></a>LAPACK Libraries
FAST v8 uses some LAPACK routines (http://www.netlib.org/lapack/index.html). We have made the decision to link with prebuilt libraries, which should include highly optimized 
versions of the Basic Linear Algebra Subprograms (BLAS).

These prebuilt libraries typically come installed on Linux and Mac operating systems. Use linking options `-llapack` and `-lblas` for gfortran. 

If you are using the Intel compiler, you can use the Intel&reg; Math Kernel Library. To activate MKL, you will need to set *Project -> {*project name*} Properties -> 
Configuration Properties -> Fortran -> Libraries -> Use Intel Math Kernel Library* to **Sequential (/Qmkl:sequential)**.
 

With this option set, you can compile and use any routine in the LAPACK libraries.
If you have an older version of the Intel compiler, you may not have access to MKL. In that case, please see the instructions at this web site: 
http://icl.cs.utk.edu/lapack-for-windows/lapack/ for help in installing LAPACK and BLAS libraries for Release and Debug modes.

## LAPACK in Double Precision
To facilitate compiling in double precision, we have created some wrapper routines in for the LAPACK and ScaLAPACK routines used in FAST and its modules.

*The NWTC wrapper routines for LAPACK have been written assuming that they are calling prebuilt libraries.* If you choose to compile with the LAPACK source files instead of 
using the prebuilt libraries (not recommended), you must make sure that default Real and Double KINDs for the LAPACK routines are 4 and 8 bytes, respectively (*even when 
compiling FAST in double precision*).


### <a name="NonWindowsCompile"></a>Compiling and Running on Linux or Mac OS
The makefile distributed in the {FAST8}/Compiling directory has been tested with Mac OS X and some Linux systems. Before compiling the FAST v8 executable, though, you will need to 
compile a few components.

#### FAST Registry
We have included the source code for the FAST Registry in the {FAST8}/Source/dependencies/Registry directory. That directory also contains a makefile that will generate the FAST Registry 
executable using gcc. In the Registry directory, perform the command 
````
make
````

This command will generate an executable named "registry.exe" in the {FAST8}/Source/dependencies directory.

#### MAP++ Library
The source files for the MAP++ library are not included in the FAST archive, but they are available on the MAP++ web site: https://nwtc.nrel.gov/MAP . The MAP++ archive contains a 
makefile (in its src directory) that can be used to generate the MAP++ library.

Running the ```make``` command should produce a .so file in the MAP++ src directory. At this writing, the file is called "libmap-1.20.10.so". When finished, copy libmap-1.20.10.so to the 
{FAST8}/bin directory, where the FAST makefile will look for the file.

When running FAST, the system must be able to load libmap-1.10.01.so. This can be accomplished in several ways, including putting it in the directory where you run FAST, copying 
it to /usr/lib, or changing DYLD_LIBRARY_PATH on Mac OS.

#### FAST Executable
Before running the FAST v8 makefile, make sure you have the FAST registry executable and MAP++ library compiled for your operating system. The FAST Registry executable file will 
be necessary only if you modify any of the FAST Registry input files or if you perform the command 
````
make superclean
````
for creating the FAST executable.

The makefile has been set up to link with the file ../bin/libmap-1.20.10.so. If this MAP++ library file has a different name, you will need to edit the makefile accordingly.

When the FAST Registry and MAP++ library have been generated, you can run the command
````
make
````
in the {FAST8}/Compiling directory. This should generate a file called FAST_glin32 in the {FAST8}/bin directory. If you have set the makefile to compile with 64-bit addresses, 
the file will be called FAST_glin64.

#### DISCON: Discrete Controllers in Dynamically Loaded Libraries
If you wish to run any FAST model that uses the DISCON dynamically loaded libraries (e.g., CertTests 18-26), you must compile the control algorithms as a dynamic library (shared object). 
The DISCON routine is an industry-standard interface (Gerrad Hassan's Bladed Interface). The source files for each of the controllers used in CertTests 18-26 are in the 
{FAST8}/CertTest/5MW_Baseline/ServoData/Source directory, and a makefile (makefile_DISCON_DLL) to compile them is in {FAST8}/Compiling. Running the command
````
make -f makefile_DISCON_DLL
````
in the Compiling directory will generate a file called DISCON_glin32.so in the {FAST8}/CertTest/5MW_Baseline/ServoData directory. This controller is used for Test 18 (and a few others). 
If you wish to compile a different source file, you will need to edit "makefile_DISCON_DLL" and modify variables **SOURCE_FILE** and **OUTPUT_NAME**.

When {FAST8}/CertTest/5MW_Baseline/ServoData/DISCON_glin32.so has been created, you must open the ServoDyn input file and tell it to read this library. Open the file 
{FAST8}/CertTest/5MW_Baseline/NRELOffshrBsline5MW_Onshore_ServoDyn.dat and change the input parameter "DLL_FileName" to "ServoData/DISCON_glin32.dll". 

When that is complete, you can run the FAST CertTest 18 model on Mac OS X or Linux. For example, in the {FAST8}/CertTest directory, type:
````
../bin/FAST_glin32 Test18.fst
````

Note: if you compile a 64-bit version of FAST (i.e., FAST_glin64), you also need to compile a 64-bit version of the DISCON\*.so file.


