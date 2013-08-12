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

   # 32-bit or 64-bit?
BITS = 32
#BITS = 64


   # Location of source files for FAST, AeroDyn, InflowWind, and the NWTC Library.
   # You will probably need to change these for your system.


ifeq ($(OS),Windows_NT)
   NWTC_Lib_DIR = C:/Users/bjonkman/Documents/DATA/DesignCodes/miscellaneous/nwtc_subs/SVNdirectory/trunk/source
   ED_DIR       = C:/Users/bjonkman/Documents/DATA/DesignCodes/simulators/FAST/SVNdirectory/branches/BJonkman/Source
   SrvD_DIR     = C:/Users/bjonkman/Documents/DATA/DesignCodes/simulators/FAST/SVNdirectory/branches/BJonkman/Source
   AD_DIR       = C:/Users/bjonkman/Documents/DATA/DesignCodes/simulators/FAST/SVNdirectory/branches/BJonkman/Source/ModulesNotConverted/AeroDyn
   IfW_DIR      = C:/Users/bjonkman/Documents/DATA/DesignCodes/simulators/FAST/SVNdirectory/branches/BJonkman/Source/ModulesNotConverted/InflowWInd
   HD_DIR       = C:/Users/bjonkman/Documents/DATA/DesignCodes/simulators/HydroDyn/SVNdirectory/branches/HydroDyn_Modularization/Source
   FAST_DIR     = C:/Users/bjonkman/Documents/DATA/DesignCodes/simulators/FAST/SVNdirectory/branches/BJonkman/Source

   Registry     = REGISTRY
else
   NWTC_Lib_DIR = $(HOME)/PC/CAEtools/miscellaneous/nwtc_subs/SVNdirectory/trunk/source
   ED_DIR       = $(HOME)/PC/CAEtools/simulators/FAST/SVNdirectory/branches/BJonkman/Source
   SrvD_DIR     = $(HOME)/PC/CAEtools/simulators/FAST/SVNdirectory/branches/BJonkman/Source
   AD_DIR       = $(HOME)/PC/CAEtools/simulators/FAST/SVNdirectory/branches/BJonkman/Source/ModulesNotConverted/AeroDyn
   IfW_DIR      = $(HOME)/PC/CAEtools/simulators/FAST/SVNdirectory/branches/BJonkman/Source/ModulesNotConverted/InflowWInd
   HD_DIR       = $(HOME)/PC/CAEtools/simulators/HydroDyn/SVNdirectory/branches/HydroDyn_Modularization/Source
   FAST_DIR     = $(HOME)/PC/CAEtools/simulators/FAST/SVNdirectory/branches/BJonkman/Source

   Registry     = registry.exe

endif


   # Name of compiler to use and flags to use.
FC      = gfortran
FFLAGS  = -O2 -m$(BITS) -fbacktrace -finit-local-zero -fno-automatic -ffree-line-length-none -x f95-cpp-input
LDFLAGS = -O2 -m$(BITS) -fbacktrace

# other useful gfortran options:
#  -fdefault-real-8 -fcheck=bounds -std=f2003 -O0 -v


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
	NWTC_IO.f90          \
	NWTC_Num.f90         \
	ModMesh_Types.f90    \
	ModMesh.f90          \
	ModMesh_Mapping.f90  \
	NWTC_Library.f90

IfW_SOURCES =           \
	SharedInflowDefs.f90 \
	HHWind.f90           \
	FFWind.f90           \
	HAWCWind.f90         \
	FDWind.f90           \
	CTWind.f90           \
	UserWind.f90         \
	InflowWindMod.f90

AD_SOURCES =            \
	SharedTypes.f90      \
	AeroMods.f90         \
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
	HydroDyn_Output_Types.f90 \
	HydroDyn_Output.f90       \
	HydroDyn_Types.f90        \
	HydroDyn_Input.f90        \
	HydroDyn.f90


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

vpath %.f90 $(NWTC_Lib_DIR) $(AD_DIR) $(IfW_DIR) $(HD_DIR) $(ED_DIR) $(SrvD_DIR) $(FAST_DIR)
vpath %.f   $(HD_DIR)
vpath %.mod $(INTER_DIR)
vpath %.obj $(INTER_DIR)
vpath %.txt $(AD_DIR) $(IfW_DIR) $(HD_DIR) $(ED_DIR) $(SrvD_DIR)

LIB_OBJS      = $(LIB_SOURCES:.f90=.obj)
IfW_OBJS      = $(IfW_SOURCES:.f90=.obj)
AD_OBJS       = $(AD_SOURCES:.f90=.obj)
SrvD_OBJS     = $(SrvD_SOURCES:.f90=.obj)
ED_OBJS       = $(ED_SOURCES:.f90=.obj)
HD_OBJS       = $(HD_SOURCES:.f90=.obj) fftpack.obj
FAST_OBJS     = $(FAST_SOURCES:.f90=.obj)
ALL_OBJS      = $(LIB_OBJS) $(IfW_OBJS) $(AD_OBJS) $(SrvD_OBJS) $(ED_OBJS) $(HD_OBJS) $(FAST_OBJS)

   # Rule to do everything.

all:     default
default:  $(INTER_DIR) $(DEST_DIR)/$(OUTPUT_NAME)$(EXE_EXT)


   # General rules for compliling the files.

%.obj: %.f90
	$(FC)  $(FFLAGS) -c $< -o $(INTER_DIR)/$@ -J $(INTER_DIR) -B $(INTER_DIR)

fftpack.obj: fftpack.f
	$(F77) $(FFLAGS) -c $< -o $(INTER_DIR)/$@ -J $(INTER_DIR) -B $(INTER_DIR)


   # General rules for creating _Types.f90 files from Registry files.

ServoDyn_Types.f90:
	$(REGISTRY) ServoDyn_Registry.txt

ElastoDyn_Types.f90:
	$(REGISTRY) ElastoDyn_Registry.txt



   #  Dependency rules.
#NWTC Library dependency rules:
NWTC_Base.obj:           $(PREC).obj
$(SYS_FILE).obj:         NWTC_Base.obj
NWTC_IO.obj:             $(SYS_FILE).obj
NWTC_Num.obj:            NWTC_IO.obj
ModMesh_Types.obj:       NWTC_Num.obj
ModMesh.obj:             ModMesh_Types.obj
ModMesh_Mapping.obj:     ModMesh.f90
NWTC_Library.obj:        ModMesh.obj  ModMesh_Mapping.obj

#InflowWind dependency rules:
SharedInflowDefs.f90:    NWTC_Library.obj
HHWind.obj:              SharedInflowDefs.obj
FFWind.obj:              SharedInflowDefs.obj
HAWCWind.obj:            SharedInflowDefs.obj
FDWind.obj:              SharedInflowDefs.obj
CTWind.obj:              SharedInflowDefs.obj
UserWind.obj:            SharedInflowDefs.obj
InflowWindMod.obj:       SharedInflowDefs.obj  HHWind.obj  FFWind.obj  HAWCWind.obj  FDWind.obj  CTWind.obj  UserWind.obj

#AeroDyn dependency rules:
AeroMods.obj:            NWTC_Library.obj
GenSubs.obj:             NWTC_Library.obj  AeroMods.obj
AeroSubs.obj:            NWTC_Library.obj  AeroMods.obj
AeroDyn.obj:             NWTC_Library.obj  AeroMods.obj  SharedTypes.obj  AeroSubs.obj  GenSubs.obj  InflowWindMod.obj


#HydroDyn dependency rules:
fftpack.obj:                NWTC_Library.obj
FFTMod.obj:                 NWTC_Library.obj  fftpack.obj

SS_Radiation_Types.obj:     NWTC_Library.obj
SS_Radiation.obj:           SS_Radiation_Types.obj

Waves_Types.obj:            NWTC_Library.obj
Waves.obj:                  Waves_Types.obj  FFTMod.obj

Current_Types.obj:          NWTC_Library.obj
Current.obj:                Current_Types.obj

Morison_Types.obj:          NWTC_Library.obj  HydroDyn_Output_Types.obj
Morison_Output.obj:         Morison_Types.obj HydroDyn_Output_Types.obj Waves.obj
Morison.obj:                Morison_Types.obj Morison_Output.obj

Conv_Radiation_Types.obj:   NWTC_Library.obj
Conv_Radiation.obj:         Conv_Radiation_Types.obj

WAMIT_Types.obj:            NWTC_Library.obj HydroDyn_Output_Types.obj Conv_Radiation_Types.obj SS_Radiation_Types.obj Waves_Types.obj
WAMIT.obj:                  WAMIT_Types.obj WAMIT_Output.obj Waves_Types.obj Conv_Radiation.obj SS_Radiation.obj FFTMod.obj

HydroDyn_Output_Types.obj:  NWTC_Library.obj
WAMIT_Output.obj:           WAMIT_Types.obj HydroDyn_Output_Types.obj Waves.obj
HydroDyn_Output.obj:        HydroDyn_Output_Types.obj HydroDyn_Types.obj Waves.obj

HydroDyn_Types.obj:         NWTC_Library.obj  HydroDyn_Output_Types.obj  Current_Types.obj  Waves_Types.obj  Conv_Radiation_Types.obj SS_Radiation_Types.obj WAMIT_Types.obj Morison_Types.obj
HydroDyn_Input.obj:         HydroDyn_Types.obj  Waves.obj Morison.obj
HydroDyn.obj:               HydroDyn_Types.obj  HydroDyn_Input.obj  HydroDyn_Output.obj  WAMIT.obj  Current.obj  Morison.obj


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
FAST_IO.obj:                NWTC_Library.obj  FAST_Mods.obj  AeroDyn.obj  InflowWindMod.obj  ServoDyn.obj ElastoDyn.obj HydroDyn.obj
FAST_Prog.obj:              NWTC_Library.obj  FAST_Mods.obj  FAST_IO.obj


# Files that depend on the registry



#$(OUTPUT_NAME)$(EXE_EXT): Fast_Prog.obj

   # Make sure the destination directory for the intermediate files exist.

$(INTER_DIR):
	$(MD_CMD) $(INTER_DIR)


   # For linking FAST.

$(DEST_DIR)/$(OUTPUT_NAME)$(EXE_EXT): $(ALL_OBJS) | $(INTER_DIR)
	$(FC) $(LDFLAGS) -I $(INTER_DIR) -o $(DEST_DIR)/$(OUTPUT_NAME)$(EXE_EXT) \
	$(foreach src, $(ALL_OBJS), $(addprefix $(INTER_DIR)/,$(src)))

   # Cleanup afterwards.

clean:
	$(DEL_CMD) $(INTER_DIR)$(PATH_SEP)*.mod $(INTER_DIR)$(PATH_SEP)*.obj

