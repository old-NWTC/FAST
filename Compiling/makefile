#================================================================================#
# This makefile created by B. Jonkman on 2-Apr-2013,                             #
# adapted from Crunch (M. Buhl on 25-Jan-2013).                                  #
# (c) 2013 National Renewable Energy Laboratory                                  #
#                                                                                #
# This makefile has been tested on Windows 7 with gfortran.                      #
# This makefile works with mingw32-make.exe.                                     #
#                                                                                #
# It was designed to be used with:                                               #
#     FAST                    (v8.01.00a-bjj, 20-Feb-2013)                       #
#     AeroDyn                 (v13.00.02a-bjj, 20-Feb-2013)                      #
#     InflowWind              (v1.01.00b-bjj, 10-Aug-2012)                       #
#     NWTC Subroutine Library (v1.07.00b-mlb, 10-Jan-2013)                       #
#                                                                                #
# Older versions of the source code may not work with this makefile.             #
#================================================================================#
# for debugging, use this make option:
# --warn-undefined-variables
#

# bjj: PLEASE NOTE THAT UNTIL WE RESOLVE ISSUES REGARDING THE USE OF INTEL'S MKL
#      THIS MAKEFILE WILL NOT WORK.


   # 32-bit or 64-bit?
BITS = 32
#BITS = 64


   # Location of source files for FAST, AeroDyn, InflowWind, and the NWTC Library.
   # You will probably need to change these for your system.


FAST_DIR     = ../Source

NWTC_Lib_DIR = $(FAST_DIR)/dependencies/NWTC_Library
NETLIB_DIR   = $(FAST_DIR)/dependencies/NetLib
ED_DIR       = $(FAST_DIR)/dependencies/ElastoDyn
SrvD_DIR     = $(FAST_DIR)/dependencies/ServoDyn
AD_DIR       = $(FAST_DIR)/dependencies/AeroDyn
IfW_DIR      = $(FAST_DIR)/dependencies/InflowWind
HD_DIR       = $(FAST_DIR)/dependencies/HydroDyn
SD_DIR       = $(FAST_DIR)/dependencies/SubDyn
MAP_DIR      = $(FAST_DIR)/dependencies/MAP

HD_DIR_Reg   = $(HD_DIR)
BIN_DIR      = ../bin


ifeq ($(OS),Windows_NT)
   Registry     = $(BIN_DIR)/Registry_win32.exe
   MAP_lib      = $(MAP_DIR)/MAP_win32.lib
   LAPACK_DIR   = C:/LAPACK/win32
else
   Registry     = registry.exe
   MAP_lib      = $(MAP_DIR)/MAP.a
endif


   # Name of compiler to use and flags to use.
FC      = gfortran
#FFLAGS  = -O2 -m$(BITS) -fbacktrace -ffree-line-length-none -x f95-cpp-input
FFLAGS  = -O2 -m$(BITS) -fbacktrace -ffree-line-length-none -x f95-cpp-input -fcheck=bounds -finit-local-zero -fno-automatic -C -Wconversion-extra -Wconversion

LDFLAGS = -O2 -m$(BITS) -fbacktrace -llapack -lblas -L$(LAPACK_DIR)

# other useful gfortran options:
#  -fdefault-real-8 -fcheck=bounds -std=f2003 -O0 -v -Wall
# http://gcc.gnu.org/onlinedocs/gfortran/Option-Index.html#Option-Index


   # Precision.

# Use "SingPrec" for single precision and "DoubPrec" for double precision. You may also need to change an option switch to make constants DP.
PREC = SingPrec



   # Destination and RootName for executable

OUTPUT_NAME = FAST
DEST_DIR    = .

   #==========================================================#
   # You should not need to change anything beyond this point #
   #==========================================================#

   # System-specific settings.

ifeq ($(OS),Windows_NT)
      # Windows
   DEL_CMD   = del
   EXE_EXT   = _gwin$(BITS).exe
   INTER_DIR = Obj_win$(BITS)
   MD_CMD    = @mkdir
   OBJ_EXT   = .obj
   PATH_SEP  = \\
   SYS_FILE  = SysGnuWin
else
      # Linux
   DEL_CMD   = rm -f
   EXE_EXT   = _glin$(BITS)
   INTER_DIR = Obj_lin$(BITS)
   MD_CMD    = @mkdir -p
   OBJ_EXT   = .o
   PATH_SEP  = /
   SYS_FILE  = SysGnuLinux
endif


   # Source files (by module)

LIB_SOURCES =           \
	$(PREC).f90          \
	NWTC_Base.f90        \
	$(SYS_FILE).f90      \
	NWTC_Library_Types.f90 \
	NWTC_IO.f90          \
	NWTC_Num.f90         \
	ModMesh_Types.f90    \
	ModMesh.f90          \
	ModMesh_Mapping.f90  \
	NWTC_Library.f90

IfW_SOURCES =            \
	IfW_FFWind_Types.f90  \
	IfW_FFWind.f90        \
	IfW_HHWind_Types.f90  \
	IfW_HHWind.f90        \
	InflowWind_Subs.f90   \
	InflowWind_Types.f90  \
	InflowWind.f90

AD_SOURCES =            \
	AeroDyn_Types.f90    \
	GenSubs.f90          \
	AeroSubs.f90         \
	AeroDyn.f90

HD_SOURCES =                 \
	FFTMod.f90                \
	SS_Radiation_Types.f90    \
	SS_Radiation.f90          \
	Waves_Types.f90           \
	Waves.f90                 \
	Current_Types.f90         \
	Current.f90               \
	Morison_Types.f90         \
	Morison_Output.f90        \
	Morison.f90               \
	Conv_Radiation_Types.f90  \
	Conv_Radiation.f90        \
	WAMIT_Types.f90           \
	WAMIT_Output.f90          \
	WAMIT.f90                 \
	HydroDyn_Output.f90       \
	HydroDyn_Types.f90        \
	HydroDyn_Input.f90        \
	HydroDyn.f90

MAP_SOURCES =              \
	MAP_C_Types.f90         \
	MAP_Types.f90           \
	MAP.f90

SD_SOURCES =               \
	qsort_c_module.f90      \
	SD_FEM.f90              \
	SubDyn_Types.f90        \
	SubDyn_Output.f90       \
	SubDyn.f90

NETLIB_SOURCES=            \
	NWTC_ScaLAPACK.f90      \
	NWTC_LAPACK.f90         \
	DLASRT2.f               \
	SLASRT2.f


ED_SOURCES =            \
	ElastoDyn_Types.f90  \
	ElastoDyn.f90

SrvD_SOURCES =          \
	ServoDyn_Types.f90   \
	ServoDyn.f90         \
	PitchCntrl_ACH.f90   \
	BladedInterface.f90  \
	UserSubs.f90         \
	UserVSCont_KP.f90

FAST_SOURCES =          \
	FAST_Mods.f90        \
	FAST_IO.f90          \
	FAST_Prog.f90

vpath %.f90 $(NWTC_Lib_DIR) $(AD_DIR) $(IfW_DIR) $(HD_DIR_Reg) $(HD_DIR) $(ED_DIR) $(SrvD_DIR) $(SD_DIR) $(MAP_DIR) $(FAST_DIR) $(NETLIB_DIR)
vpath %.f   $(HD_DIR) $(NETLIB_DIR)
vpath %.mod $(INTER_DIR)
vpath %.obj $(INTER_DIR)
vpath %.txt $(AD_DIR) $(IfW_DIR) $(HD_DIR_Reg) $(ED_DIR) $(SrvD_DIR)

NETLIB_OBJS   = NWTC_LAPACK.obj NWTC_ScaLAPACK.obj DLASRT2.obj SLASRT2.obj
LIB_OBJS      = $(LIB_SOURCES:.f90=.obj)
IfW_OBJS      = $(IfW_SOURCES:.f90=.obj)
AD_OBJS       = $(AD_SOURCES:.f90=.obj)
SrvD_OBJS     = $(SrvD_SOURCES:.f90=.obj)
ED_OBJS       = $(ED_SOURCES:.f90=.obj)
HD_OBJS       = $(HD_SOURCES:.f90=.obj) fftpack.obj
SD_OBJS       = $(SD_SOURCES:.f90=.obj)
MAP_OBJS      = $(MAP_SOURCES:.f90=.obj)
FAST_OBJS     = $(FAST_SOURCES:.f90=.obj)
ALL_OBJS      = $(LIB_OBJS) $(IfW_OBJS) $(AD_OBJS) $(SrvD_OBJS) $(ED_OBJS) $(HD_OBJS) $(SD_OBJS) $(FAST_OBJS) $(NETLIB_OBJS) $(MAP_OBJS)

   # Rule to do everything.

all:     default
default:  $(INTER_DIR) $(DEST_DIR)/$(OUTPUT_NAME)$(EXE_EXT)


   # General rules for compliling the files.

%.obj: %.f90
	$(FC) -I $(INTER_DIR) $(FFLAGS) -c $< -o $(INTER_DIR)/$@ -J $(INTER_DIR) -B $(INTER_DIR)

%.obj: %.f
	$(F77) -I $(INTER_DIR) $(FFLAGS) -c $< -o $(INTER_DIR)/$@ -J $(INTER_DIR) -B $(INTER_DIR)


   # General rules for creating _Types.f90 files from Registry files.

ServoDyn_Types.f90:
	$(Registry) ServoDyn_Registry.txt -I $(NWTC_Lib_DIR)

ElastoDyn_Types.f90:
	$(Registry) ElastoDyn_Registry.txt -I $(NWTC_Lib_DIR)

Current_Types.f90:
	$(Registry) Current.txt -I $(NWTC_Lib_DIR)

Waves_Types.f90:
	$(Registry) Waves.txt -I $(NWTC_Lib_DIR)

SS_Radiation_Types.f90:
	$(Registry) SS_Radiation.txt -I $(NWTC_Lib_DIR)

Conv_Radiation_Types.f90:
	$(Registry) Conv_Radiation.txt -I $(NWTC_Lib_DIR)

WAMIT_Types.f90:
	$(Registry) WAMIT.txt -I $(NWTC_Lib_DIR)

Morison_Types.f90:
	$(Registry) Morison.txt -I $(NWTC_Lib_DIR)

HydroDyn_Types.f90:
	$(Registry) HydroDyn.txt -I $(NWTC_Lib_DIR) -I $(HD_DIR)


AeroDyn_Types.f90:
	$(Registry) Registry-AD.txt -I $(NWTC_Lib_DIR) -I $(IfW_DIR)


IfW_HHWind_Types.f90:
	$(Registry) Reg-IfW_HHWind.txt -I $(NWTC_Lib_DIR) -I $(IfW_DIR)

IfW_FFWind_Types.f90:
	$(Registry) Reg-IfW_FFWind.txt -I $(NWTC_Lib_DIR) -I $(IfW_DIR)

InflowWind_Types.f90:
	$(Registry) Reg-InflowWind.txt -I $(NWTC_Lib_DIR) -I $(IfW_DIR)



   #  Dependency rules.
#NWTC Library dependency rules:
NWTC_Base.obj:           $(PREC).obj
$(SYS_FILE).obj:         NWTC_Base.obj
NWTC_Library_Types.obj:  $(SYS_FILE).obj
NWTC_IO.obj:             NWTC_Library_Types.obj
NWTC_Num.obj:            NWTC_IO.obj
ModMesh_Types.obj:       NWTC_Num.obj
ModMesh.obj:             ModMesh_Types.obj
ModMesh_Mapping.obj:     ModMesh.f90
NWTC_Library.obj:        ModMesh.obj  ModMesh_Mapping.obj

NWTC_LAPACK.obj:         NWTC_Base.obj
NWTC_ScaLAPACK.obj:      NWTC_Base.obj DLASRT2.obj SLASRT2.obj


#InflowWind dependency rules:
IfW_HHWind_Types.obj:    NWTC_Library.obj
IfW_HHWind.obj:          IfW_HHWind_Types.obj
IfW_FFWind_Types.obj:    NWTC_Library.obj
IfW_FFWind.obj:          IfW_FFWind_Types.obj
InflowWind_Types.obj:    NWTC_Library.obj  IfW_FFWind_Types.obj  IfW_HHWind_Types.obj
InflowWind_Subs.obj:     InflowWind_Types.obj IfW_FFWind.obj  IfW_HHWind.obj
InflowWind.obj:          InflowWind_Subs.obj


#AeroDyn dependency rules:
AeroDyn_Types.obj:       NWTC_Library.obj InflowWind_Types.obj
GenSubs.obj:             NWTC_Library.obj  AeroDyn_Types.obj
AeroSubs.obj:            NWTC_Library.obj  AeroDyn_Types.obj
AeroDyn.obj:             AeroDyn_Types.obj AeroSubs.obj  GenSubs.obj  InflowWind.obj


#HydroDyn dependency rules:
fftpack.obj:                NWTC_Library.obj
FFTMod.obj:                 NWTC_Library.obj  fftpack.obj

SS_Radiation_Types.obj:     NWTC_Library.obj
SS_Radiation.obj:           SS_Radiation_Types.obj

Waves_Types.obj:            NWTC_Library.obj
Waves.obj:                  Waves_Types.obj  FFTMod.obj

Current_Types.obj:          NWTC_Library.obj
Current.obj:                Current_Types.obj

Morison_Types.obj:          NWTC_Library.obj
Morison_Output.obj:         Morison_Types.obj Waves.obj
Morison.obj:                Morison_Types.obj Morison_Output.obj

Conv_Radiation_Types.obj:   NWTC_Library.obj
Conv_Radiation.obj:         Conv_Radiation_Types.obj

WAMIT_Types.obj:            NWTC_Library.obj Conv_Radiation_Types.obj SS_Radiation_Types.obj Waves_Types.obj
WAMIT.obj:                  WAMIT_Types.obj WAMIT_Output.obj Waves_Types.obj Conv_Radiation.obj SS_Radiation.obj FFTMod.obj

WAMIT_Output.obj:           WAMIT_Types.obj Waves.obj
HydroDyn_Output.obj:        HydroDyn_Types.obj Waves.obj

HydroDyn_Types.obj:         NWTC_Library.obj  Current_Types.obj  Waves_Types.obj  Conv_Radiation_Types.obj SS_Radiation_Types.obj WAMIT_Types.obj Morison_Types.obj
HydroDyn_Input.obj:         HydroDyn_Types.obj  Waves.obj Morison.obj
HydroDyn.obj:               HydroDyn_Types.obj  HydroDyn_Input.obj  HydroDyn_Output.obj  WAMIT.obj  Current.obj  Morison.obj


# SubDyn dependency rules:
SubDyn_Types.obj:          NWTC_Library.obj
SD_FEM.obj:                NWTC_Library.obj SubDyn_Types.obj qsort_c_module.obj
SubDyn_Output.obj:         SD_FEM.obj
SubDyn.obj:                SubDyn_Output.obj  SD_FEM.obj  qsort_c_module.obj NWTC_ScaLAPACK.obj NWTC_LAPACK.obj

# MAP dependency rules:
MAP_C_Types.obj:           NWTC_Library.obj
MAP_Types.obj:             NWTC_Library.obj MAP_C_Types.obj
MAP.obj:                   MAP_Types.obj MAP_C_Types.obj

# ElastoDyn dependency rules:
ElastoDyn_Types.obj:        NWTC_Library.obj  ElastoDyn_Registry.txt
ElastoDyn.obj:              NWTC_Library.obj  ElastoDyn_Types.obj

# ServoDyn dependency rules:
ServoDyn_Types.obj:         NWTC_Library.obj  ServoDyn_Registry.txt
BladedInterface.obj:        NWTC_Library.obj  ServoDyn_Types.obj
ServoDyn.obj:               NWTC_Library.obj  ServoDyn_Types.obj  PitchCntrl_ACH.obj  UserSubs.obj   UserVSCont_KP.obj  BladedInterface.obj
PitchCntrl_ACH.obj:         NWTC_Library.obj
UserSubs.obj:               NWTC_Library.obj
UserVSCont_KP.obj:          NWTC_Library.obj

# FAST dependency rules:
FAST_Mods.obj:              NWTC_Library.obj  AeroDyn.obj
FAST_IO.obj:                NWTC_Library.obj  FAST_Mods.obj  AeroDyn.obj  InflowWind.obj  ServoDyn.obj ElastoDyn.obj HydroDyn.obj NWTC_LAPACK.obj MAP.obj
FAST_Prog.obj:              NWTC_Library.obj  FAST_Mods.obj  FAST_IO.obj



#$(OUTPUT_NAME)$(EXE_EXT): Fast_Prog.obj

   # Make sure the destination directory for the intermediate files exist.

$(INTER_DIR):
	$(MD_CMD) $(INTER_DIR)


   # For linking FAST.

$(DEST_DIR)/$(OUTPUT_NAME)$(EXE_EXT): $(ALL_OBJS) | $(INTER_DIR)
	$(FC) $(LDFLAGS) -I $(INTER_DIR) -o $(DEST_DIR)/$(OUTPUT_NAME)$(EXE_EXT) \
	$(foreach src, $(ALL_OBJS), $(addprefix $(INTER_DIR)/,$(src))) $(MAP_lib)

   # Cleanup afterwards.

clean:
	$(DEL_CMD) $(INTER_DIR)$(PATH_SEP)*.mod $(INTER_DIR)$(PATH_SEP)*.obj

