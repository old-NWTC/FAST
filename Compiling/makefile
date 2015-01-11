#================================================================================#
# This makefile created by B. Jonkman on 2-Apr-2013,                             #
# adapted from Crunch (M. Buhl on 25-Jan-2013).                                  #
# (c) 2013-2014 National Renewable Energy Laboratory                             #
#                                                                                #
# This makefile has been tested on Windows 7 with gfortran.                      #
# This makefile works with mingw32-make.exe.                                     #
#                                                                                #
# It was designed to be used with:                                               #
#     FAST                    (v8.09.00a-bjj, 30-Sept-2014)                      #
#                                                                                #
# Older versions of the source code may not work with this makefile.             #
#================================================================================#

   # 32-bit or 64-bit?
BITS = 32
#BITS = 64


   # Location of source files for FAST, and its modules.

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
FEAM_DIR     = $(FAST_DIR)/dependencies/FEAMooring
IceF_DIR     = $(FAST_DIR)/dependencies/IceFloe
IceD_DIR     = $(FAST_DIR)/dependencies/IceDyn

DWM_DIR      = $(AD_DIR)
HD_DIR_Reg   = $(HD_DIR)
IfW_DIR_Reg  = $(IfW_DIR)
BIN_DIR      = ../bin

   # Names and locations of the Registry, MAP libraries, and instructions for linking with LAPACK.
   # You will probably need to change these for your system.

ifeq ($(OS),Windows_NT)
   Registry     = $(BIN_DIR)/Registry_win32.exe
   MAP_lib      = $(MAP_DIR)/MAP_win32.lib
   LAPACK_LINK  = -llapack -lblas -LC:/LAPACK/win32
else
   Registry     = registry.exe
   MAP_lib      = $(MAP_DIR)/MAP.so
   LAPACK_LINK  = -llapack -lblas
endif


   # Name of compiler to use and flags to use.
FC      = gfortran

FFLAGS  = -O2 -m$(BITS) -fbacktrace -ffree-line-length-none -x f95-cpp-input
LDFLAGS = -O2 -m$(BITS) -fbacktrace -Wl,--stack=999999999,--large-address-aware

#FFLAGS  = -O0 -m$(BITS) -fbacktrace -ffree-line-length-none -x f95-cpp-input -g -pg
#LDFLAGS = -O0 -m$(BITS) -fbacktrace -Wl,--stack=999999999,--large-address-aware -g -pg

# some useful gfortran options:
#  -DFPE_TRAP_ENABLED
#  -Wl,--large-address-aware   # not necessary when $(BITS) is 64 (64-bit target)
#  -Wl,--stack=999999999       # not necessary when $(BITS) is 64
#  -Wconversion-extra -Wconversion
#  -fdefault-real-8 -fcheck=bounds,do,mem,pointer -std=f2003 -O0 -v -Wall
#  -pg                          # generate debugging info for debugger
# http://gcc.gnu.org/onlinedocs/gfortran/Option-Index.html#Option-Index
#
# makefile:
# --warn-undefined-variables


   # Precision.

# Use "SingPrec" for single precision and "DoubPrec" for double precision. You may also need to change an option switch to make constants DP.
PREC = SingPrec



   # Destination and RootName for executable

OUTPUT_NAME = FAST
DEST_DIR    = ..\bin

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

NETLIB_SOURCES=            \
	NWTC_ScaLAPACK.f90      \
	NWTC_FFTPACK.f90        \
	NWTC_LAPACK.f90         \
	fftpack4.1.f            \
	dlasrt2.f               \
	slasrt2.f


IfW_SOURCES =            \
	IfW_FFWind_Types.f90  \
	IfW_FFWind.f90        \
	IfW_HHWind_Types.f90  \
	IfW_HHWind.f90        \
	InflowWind_Subs.f90   \
	InflowWind_Types.f90  \
	InflowWind.f90

AD_SOURCES =             \
	AeroDyn_Types.f90     \
	GenSubs.f90           \
	AeroSubs.f90          \
	AeroDyn.f90

DWM_SOURCES =            \
	DWM_Types.f90         \
	DWM.f90               \
	DWM_Wake_Sub_ver2.f90


HD_SOURCES =                 \
	SS_Radiation_Types.f90    \
	SS_Radiation.f90          \
	Waves2_Types.f90          \
	Waves2_Output.f90         \
	Waves2.f90                \
	Waves_Types.f90           \
	Waves.f90                 \
	Current_Types.f90         \
	Current.f90               \
	Morison_Types.f90         \
	Morison_Output.f90        \
	Morison.f90               \
	Conv_Radiation_Types.f90  \
	Conv_Radiation.f90        \
	WAMIT2_Types.f90          \
	WAMIT2_Output.f90         \
	WAMIT2.f90                \
	WAMIT_Types.f90           \
	WAMIT_Interp.f90          \
	WAMIT_Output.f90          \
	WAMIT.f90                 \
	HydroDyn_Output.f90       \
	HydroDyn_Types.f90        \
	HydroDyn_Input.f90        \
	HydroDyn.f90

MAP_SOURCES =              \
	MAP_Types.f90           \
	MAP.f90

FEAM_SOURCES =              \
	FEAMooring_Types.f90     \
	FEAM.f90


SD_SOURCES =               \
	qsort_c_module.f90      \
	SD_FEM.f90              \
	SubDyn_Types.f90        \
	SubDyn_Output.f90       \
	SubDyn.f90


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


IceF_SOURCES =              \
	IceFloe_Types.f90        \
	iceLog.F90               \
	coupledCrushing.F90      \
	crushingIEC.F90          \
	crushingISO.F90          \
	IceFlexBase.F90          \
	IceFlexIEC.f90           \
	IceFlexISO.f90           \
	IceFloeBase.F90          \
	iceInput.f90             \
	intermittentCrushing.F90 \
	lockInISO.F90            \
	randomCrushing.F90       \
	RANLUX.f90               \
	IceFloe.f90

IceD_SOURCES =              \
	IceDyn_Types.f90         \
	IceDyn.f90


FAST_SOURCES =           \
	FAST_Types.f90       \
	FAST_Mods.f90        \
	FAST_IO.f90          \
	FAST_Prog.f90

vpath %.f90 $(NWTC_Lib_DIR) $(AD_DIR) $(IfW_DIR) $(HD_DIR_Reg) $(HD_DIR) $(ED_DIR) $(SrvD_DIR) $(SD_DIR) $(MAP_DIR) $(FAST_DIR) $(NETLIB_DIR) $(FEAM_DIR) $(IceF_DIR) $(IceD_DIR) $(DWM_DIR)
vpath %.f   $(NETLIB_DIR)
vpath %.mod $(INTER_DIR)
vpath %.obj $(INTER_DIR)
vpath %.txt $(FAST_DIR) $(AD_DIR) $(IfW_DIR_Reg) $(HD_DIR_Reg) $(SD_DIR) $(ED_DIR) $(SrvD_DIR) $(FEAM_DIR) $(IceD_DIR) $(DWM_DIR)
vpath %.inp $(IceF_DIR)
#apparently vpath doesn't work as expected if I use %.F90 (is it not case sensitive???)
#vpath %.F90     $(IceF_DIR)
vpath %     $(IceF_DIR)


ALL_SOURCES = $(FAST_SOURCES) $(LIB_SOURCES) $(NETLIB_SOURCES) $(IfW_SOURCES) $(AD_SOURCES) $(SrvD_SOURCES) $(ED_SOURCES) $(HD_SOURCES) $(SD_SOURCES) $(MAP_SOURCES) $(FEAM_SOURCES) $(IceF_SOURCES) $(IceD_SOURCES) $(DWM_SOURCES)
tmp_objs1   = $(ALL_SOURCES:.f90=.obj)
tmp_objs2   = $(tmp_objs1:.F90=.obj)       #note the upper case here (from IceFloe)
ALL_OBJS    = $(tmp_objs2:.f=.obj)


   # Rule to do everything.

all:     default
# use this for "all" for debugging: print variables:
#all:     ; $(info $$IceF_SOURCES is [${IceF_SOURCES}]) echo debugging
#
default:  $(INTER_DIR) $(DEST_DIR)/$(OUTPUT_NAME)$(EXE_EXT)


   # General rules for compliling the files.

%.obj: %.f90
	$(FC) -I $(INTER_DIR) $(FFLAGS) -c $< -o $(INTER_DIR)/$@ -J $(INTER_DIR) -B $(INTER_DIR)

%.obj: %.F90
	$(FC) -I $(INTER_DIR) $(FFLAGS) -c $< -o $(INTER_DIR)/$@ -J $(INTER_DIR) -B $(INTER_DIR)

#bjj: what is $(F77)????
%.obj: %.f
	$(F77) -I $(INTER_DIR) $(FFLAGS) -c $< -o $(INTER_DIR)/$@ -J $(INTER_DIR) -B $(INTER_DIR)



   # General rules for creating _Types.f90 files from Registry files.

FAST_Types.f90:
	$(Registry) FAST_Registry.txt -I $(NWTC_Lib_DIR) -I $(ED_DIR) -I $(SrvD_DIR) -I $(SD_DIR) -I $(AD_DIR) \
	-I $(HD_DIR_Reg) -I $(IceF_DIR) -I $(IceD_DIR) -I $(MAP_DIR) -I $(FEAM_DIR) -I $(IfW_DIR_Reg) -I $(DWM_DIR) -noextrap


ServoDyn_Types.f90:
	$(Registry) ServoDyn_Registry.txt -I $(NWTC_Lib_DIR)


ElastoDyn_Types.f90:
	$(Registry) ElastoDyn_Registry.txt -I $(NWTC_Lib_DIR)


Current_Types.f90:
	$(Registry) Current.txt -I $(NWTC_Lib_DIR)

Waves_Types.f90:
	$(Registry) Waves.txt -I $(NWTC_Lib_DIR)

Waves2_Types.f90:
	$(Registry) Waves2.txt -I $(NWTC_Lib_DIR)

SS_Radiation_Types.f90:
	$(Registry) SS_Radiation.txt -I $(NWTC_Lib_DIR)

Conv_Radiation_Types.f90:
	$(Registry) Conv_Radiation.txt -I $(NWTC_Lib_DIR)

WAMIT_Types.f90:
	$(Registry) WAMIT.txt -I $(NWTC_Lib_DIR)

WAMIT2_Types.f90:
	$(Registry) WAMIT2.txt -I $(NWTC_Lib_DIR)

Morison_Types.f90:
	$(Registry) Morison.txt -I $(NWTC_Lib_DIR)

HydroDyn_Types.f90:
	$(Registry) HydroDyn.txt -I $(NWTC_Lib_DIR) -I $(HD_DIR_Reg)


AeroDyn_Types.f90:
	$(Registry) Registry-AD.txt -I $(NWTC_Lib_DIR) -I $(IfW_DIR_Reg) -I $(DWM_DIR)

DWM_Types.f90:
	$(Registry) Registry-DWM.txt -I $(NWTC_Lib_DIR) -I $(IfW_DIR_Reg)


IfW_HHWind_Types.f90:
	$(Registry) IfW_HHWind.txt -I $(NWTC_Lib_DIR) -I $(IfW_DIR_Reg)

IfW_FFWind_Types.f90:
	$(Registry) IfW_FFWind.txt -I $(NWTC_Lib_DIR) -I $(IfW_DIR_Reg)

InflowWind_Types.f90:
	$(Registry) InflowWind.txt -I $(NWTC_Lib_DIR) -I $(IfW_DIR_Reg)


SubDyn_Types.f90:
	$(Registry) SubDyn_Registry.txt -I $(NWTC_Lib_DIR)


FEAMooring_Types.f90:
	$(Registry) FEAM_Registry.txt -I $(NWTC_Lib_DIR)


IceFloe_Types.f90:
	$(Registry) IceFloe_FASTRegistry.inp -I $(NWTC_Lib_DIR)


IceDyn_Types.f90:
	$(Registry) Registry_IceDyn.txt -I $(NWTC_Lib_DIR)


   #  Dependency rules.
#NWTC Library dependency rules:
NWTC_Base.obj:              $(PREC).obj
$(SYS_FILE).obj:            NWTC_Base.obj
NWTC_Library_Types.obj:     $(SYS_FILE).obj
NWTC_IO.obj:                NWTC_Library_Types.obj
NWTC_Num.obj:               NWTC_IO.obj
ModMesh_Types.obj:          NWTC_Num.obj
ModMesh.obj:                ModMesh_Types.obj
ModMesh_Mapping.obj:        ModMesh.obj NWTC_LAPACK.obj
NWTC_Library.obj:           ModMesh.obj  ModMesh_Mapping.obj

NWTC_LAPACK.obj:            NWTC_Base.obj
NWTC_ScaLAPACK.obj:         NWTC_Base.obj dlasrt2.obj slasrt2.obj
NWTC_FFTPACK.obj:           NWTC_Library.obj fftpack4.1.obj
fftpack4.1.obj:             $(PREC).obj


#InflowWind dependency rules:
IfW_HHWind_Types.obj:       NWTC_Library.obj  IfW_HHWind.txt
IfW_HHWind.obj:             IfW_HHWind_Types.obj
IfW_FFWind_Types.obj:       NWTC_Library.obj  IfW_FFWind.txt
IfW_FFWind.obj:             IfW_FFWind_Types.obj
InflowWind_Types.obj:       NWTC_Library.obj  IfW_FFWind_Types.obj  IfW_HHWind_Types.obj  InflowWind.txt
InflowWind_Subs.obj:        InflowWind_Types.obj IfW_FFWind.obj  IfW_HHWind.obj
InflowWind.obj:             InflowWind_Subs.obj


#AeroDyn dependency rules:
AeroDyn_Types.obj:          NWTC_Library.obj  InflowWind_Types.obj  DWM_Types.obj  Registry-AD.txt
GenSubs.obj:                NWTC_Library.obj  AeroDyn_Types.obj
AeroSubs.obj:               NWTC_Library.obj  AeroDyn_Types.obj InflowWind.obj GenSubs.obj
AeroDyn.obj:                AeroDyn_Types.obj AeroSubs.obj  GenSubs.obj  InflowWind.obj DWM.obj DWM_Types.obj

#DWM dependency rules:
DWM_Types.obj:              NWTC_Library.obj  Registry-DWM.txt
DWM_Wake_Sub_ver2.obj:      NWTC_Library.obj DWM_Types.obj
DWM.obj:                    NWTC_Library.obj DWM_Types.obj DWM_Wake_Sub_ver2.obj


#HydroDyn dependency rules:
SS_Radiation_Types.obj:     NWTC_Library.obj  SS_Radiation.txt
SS_Radiation.obj:           SS_Radiation_Types.obj

Waves2_Types.obj:           NWTC_Library.obj  Waves2.txt
Waves2_Output.obj:          Waves2_Types.obj
Waves2.obj:                 NWTC_Library.obj  Waves2_Types.obj  NWTC_FFTPACK.obj  Waves2_Output.obj  Waves.obj

Waves_Types.obj:            NWTC_Library.obj  Waves.txt
Waves.obj:                  Waves_Types.obj   NWTC_FFTPACK.obj

Current_Types.obj:          NWTC_Library.obj  Current.txt
Current.obj:                Current_Types.obj

Morison_Types.obj:          NWTC_Library.obj  Morison.txt
Morison_Output.obj:         Morison_Types.obj Waves.obj
Morison.obj:                Morison_Types.obj Morison_Output.obj

Conv_Radiation_Types.obj:   NWTC_Library.obj  Conv_Radiation.txt
Conv_Radiation.obj:         Conv_Radiation_Types.obj

WAMIT2_Types.obj:           NWTC_Library.obj WAMIT2.txt
WAMIT2_Output.obj:          NWTC_Library.obj  WAMIT2_Types.obj
WAMIT2.obj:                 NWTC_Library.obj  WAMIT_Interp.obj  WAMIT2_Output.obj  NWTC_FFTPACK.obj  Waves.obj

WAMIT_Types.obj:            NWTC_Library.obj Conv_Radiation_Types.obj SS_Radiation_Types.obj Waves_Types.obj  WAMIT.txt
WAMIT.obj:                  WAMIT_Types.obj WAMIT_Output.obj Waves_Types.obj Conv_Radiation.obj SS_Radiation.obj NWTC_FFTPACK.obj WAMIT_Interp.obj

WAMIT_Interp.obj:           NWTC_Library.obj
WAMIT_Output.obj:           WAMIT_Types.obj Waves.obj
HydroDyn_Output.obj:        HydroDyn_Types.obj Waves.obj

HydroDyn_Types.obj:         NWTC_Library.obj  Current_Types.obj  Waves_Types.obj  Waves2_Types.obj  Conv_Radiation_Types.obj SS_Radiation_Types.obj WAMIT_Types.obj  WAMIT2_Types.obj  Morison_Types.obj  HydroDyn.txt
HydroDyn_Input.obj:         HydroDyn_Types.obj  Waves.obj Morison.obj  HydroDyn_Output.obj  WAMIT_Output.obj  Waves2_Output.obj  WAMIT2_Output.obj
HydroDyn.obj:               HydroDyn_Types.obj  HydroDyn_Input.obj  HydroDyn_Output.obj  WAMIT.obj  Current.obj  Morison.obj  Waves2.obj  WAMIT2.obj


# SubDyn dependency rules:
SubDyn_Types.obj:           NWTC_Library.obj    SubDyn_Registry.txt
SD_FEM.obj:                 NWTC_Library.obj SubDyn_Types.obj qsort_c_module.obj
SubDyn_Output.obj:          SD_FEM.obj
SubDyn.obj:                 SubDyn_Output.obj  SD_FEM.obj  qsort_c_module.obj NWTC_ScaLAPACK.obj NWTC_LAPACK.obj


# MAP dependency rules:
MAP_Types.obj:              NWTC_Library.obj
MAP.obj:                    NWTC_Library.obj  MAP_Types.obj


# FEAMooring dependency rules:
FEAMooring_Types.obj:       NWTC_Library.obj  FEAM_Registry.txt
FEAM.obj:                   NWTC_Library.obj  FEAMooring_Types.obj


# ElastoDyn dependency rules:
ElastoDyn_Types.obj:        NWTC_Library.obj  ElastoDyn_Registry.txt
ElastoDyn.obj:              NWTC_Library.obj  ElastoDyn_Types.obj  NWTC_LAPACK.obj


# ServoDyn dependency rules:
ServoDyn_Types.obj:         NWTC_Library.obj  ServoDyn_Registry.txt
BladedInterface.obj:        NWTC_Library.obj  ServoDyn_Types.obj
ServoDyn.obj:               NWTC_Library.obj  ServoDyn_Types.obj  PitchCntrl_ACH.obj  UserSubs.obj   UserVSCont_KP.obj  BladedInterface.obj
PitchCntrl_ACH.obj:         NWTC_Library.obj
UserSubs.obj:               NWTC_Library.obj
UserVSCont_KP.obj:          NWTC_Library.obj


# IceFloe dependency rules:
#RANLUX.obj:
iceLog.obj:                 NWTC_Library.obj
iceInput.obj:               iceLog.F90 NWTC_Library.obj
IceFloeBase.obj:            NWTC_Library.obj  iceInput.obj  RANLUX.obj IceFloe_Types.obj
coupledCrushing.obj:        IceFloeBase.obj
crushingIEC.obj:            IceFloeBase.obj
crushingISO.obj:            IceFloeBase.obj
intermittentCrushing.obj:   crushingISO.obj
lockInISO.obj:              crushingISO.obj
randomCrushing.obj:         crushingISO.obj
IceFlexBase.obj:            IceFloeBase.obj
IceFlexIEC.obj:             IceFlexBase.obj
IceFlexISO.obj:             IceFlexBase.obj
IceFloe_Types.obj:          NWTC_Library.obj IceFloe_FASTRegistry.inp
IceFloe.obj:                IceFloe_Types.obj iceLog.obj coupledCrushing.obj crushingIEC.obj crushingISO.obj IceFlexBase.obj IceFlexIEC.obj IceFlexISO.obj intermittentCrushing.obj lockInISO.obj randomCrushing.obj


# IceDyn dependency rules:
IceDyn_Types.obj:           NWTC_Library.obj Registry_IceDyn.txt
IceDyn.obj:                 NWTC_Library.obj IceDyn_Types.obj


# FAST dependency rules:
FAST_Types.obj:             NWTC_Library.obj  ElastoDyn_Types.obj  ServoDyn_Types.obj  AeroDyn_Types.obj  SubDyn_Types.obj  HydroDyn_Types.obj  MAP_Types.obj  FEAMooring_Types.obj  IceFloe_Types.obj  IceDyn_Types.obj FAST_Registry.txt
FAST_Mods.obj:              NWTC_Library.obj  FAST_Types.obj
FAST_IO.obj:                NWTC_Library.obj  FAST_Mods.obj  NWTC_LAPACK.obj  AeroDyn.obj  InflowWind.obj  ServoDyn.obj  ElastoDyn.obj  SubDyn.obj  HydroDyn.obj  MAP.obj  FEAM.obj  IceFloe.obj  IceDyn.obj
FAST_Prog.obj:              NWTC_Library.obj  FAST_Mods.obj  FAST_IO.obj



#$(OUTPUT_NAME)$(EXE_EXT): Fast_Prog.obj

   # Make sure the destination directory for the intermediate files exist.

$(INTER_DIR):
	$(MD_CMD) $(INTER_DIR)


   # For linking FAST.

$(DEST_DIR)/$(OUTPUT_NAME)$(EXE_EXT): $(ALL_OBJS) | $(INTER_DIR)
	$(FC) $(LDFLAGS) -I $(INTER_DIR) -o $(DEST_DIR)/$(OUTPUT_NAME)$(EXE_EXT) \
	$(foreach src, $(ALL_OBJS), $(addprefix $(INTER_DIR)/,$(src))) $(MAP_lib) $(LAPACK_LINK)

   # Cleanup afterwards.

clean:
	$(DEL_CMD) $(INTER_DIR)$(PATH_SEP)*.mod $(INTER_DIR)$(PATH_SEP)*.obj

#superclean:
#	$(DEL_CMD) $(INTER_DIR)$(PATH_SEP)*.mod $(INTER_DIR)$(PATH_SEP)*.obj
# also delete all the registry-generated ModuleName_Types.f90 files