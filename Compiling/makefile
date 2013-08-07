#================================================================================#
# This makefile created by B. Jonkman on 2-Apr-2013,                             #
# adapted from Crunch (M. Buhl on 25-Jan-2013).                                  #
# (c) 2013 National Renewable Energy Laboratory                                  #
#                                                                                #
# This makefile has been tested on Windows 7 with gfortran.                      #
# This makefile works with mingw32-make.exe.                                     #
#                                                                                #
# It was designed to be used with:                                               #
#     FAST                    (v7.02.00d-bjj, 20-Feb-2013)                       #
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
   HD_DIR       = C:/Users/bjonkman/Documents/DATA/DesignCodes/simulators/HydroDyn/SVNdirectory\branches\HydroDyn_Modularization\Source
   FAST_DIR     = C:/Users/bjonkman/Documents/DATA/DesignCodes/simulators/FAST/SVNdirectory/branches/BJonkman/Source

   Registry     = REGISTRY
else
   NWTC_Lib_DIR = $(HOME)/PC/CAEtools/miscellaneous/nwtc_subs/SVNdirectory/trunk/source
   ED_DIR       = $(HOME)/PC/CAEtools/simulators/FAST/SVNdirectory/branches/BJonkman/Source
   SrvD_DIR     = $(HOME)/PC/CAEtools/simulators/FAST/SVNdirectory/branches/BJonkman/Source
   AD_DIR       = $(HOME)/PC/CAEtools/simulators/FAST/SVNdirectory/branches/BJonkman/Source/ModulesNotConverted/AeroDyn
   IfW_DIR      = $(HOME)/PC/CAEtools/simulators/FAST/SVNdirectory/branches/BJonkman/Source/ModulesNotConverted/InflowWInd
   HD_DIR       = $(HOME)/PC/CAEtools/simulators/HydroDyn/SVNdirectory\branches\HydroDyn_Modularization\Source
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

HD_SOURCES =             \
	FFTMod.f90            \
	SharedDataTypes.f90   \
	Waves.f90             \
	UserLine.f90          \
	FixedBottomSupportStructure.f90  \
	FloatingPlatform.f90  \
	Hydro_IO.f90          \
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

   #  Dependency rules.
NWTC_Base.obj:           $(PREC).obj
$(SYS_FILE).obj:         NWTC_Base.obj
NWTC_IO.obj:             $(SYS_FILE).obj
NWTC_Num.obj:            NWTC_IO.obj
ModMesh_Types.obj:       NWTC_Num.obj
ModMesh.obj:             ModMesh_Types.obj
ModMesh_Mapping.obj:     ModMesh.f90
NWTC_Library.obj:        ModMesh.obj  ModMesh_Mapping.obj


HHWind.obj:                        NWTC_Library.obj  SharedInflowDefs.obj
FFWind.obj:                        NWTC_Library.obj  SharedInflowDefs.obj
HAWCWind.obj:                      NWTC_Library.obj  SharedInflowDefs.obj
FDWind.obj:                        NWTC_Library.obj  SharedInflowDefs.obj
CTWind.obj:                        NWTC_Library.obj  SharedInflowDefs.obj
UserWind.obj:                      NWTC_Library.obj  SharedInflowDefs.obj
InflowWindMod.obj:                 NWTC_Library.obj  SharedInflowDefs.obj  HHWind.obj  FFWind.obj  HAWCWind.obj  FDWind.obj  CTWind.obj  UserWind.obj

AeroMods.obj:                      NWTC_Library.obj
GenSubs.obj:                       NWTC_Library.obj  AeroMods.obj
AeroSubs.obj:                      NWTC_Library.obj  AeroMods.obj
AeroDyn.obj:                       NWTC_Library.obj  AeroMods.obj  SharedTypes.obj  AeroSubs.obj  GenSubs.obj  InflowWindMod.obj

fftpack.obj:                       NWTC_Library.obj
FFTMod.obj:                        NWTC_Library.obj  fftpack.obj
SharedDataTypes.obj:               NWTC_Library.obj
Waves.obj:                         NWTC_Library.obj  FFTMod.obj
UserLine.obj:                      NWTC_Library.obj
FixedBottomSupportStructure.obj:   NWTC_Library.obj  Waves.obj
FloatingPlatform.obj:              NWTC_Library.obj  UserLine.obj
Hydro_IO.obj:                      NWTC_Library.obj  SharedDataTypes.obj
HydroDyn.obj:                      NWTC_Library.obj  SharedDataTypes.obj FixedBottomSupportStructure.obj FloatingPlatform.obj Hydro_IO.obj Waves.obj

# ElastoDyn_Types.obj:               NWTC_Library.obj  ElastoDyn_Registry.txt
ElastoDyn_Types.obj:               NWTC_Library.obj
ElastoDyn.obj:                     NWTC_Library.obj  ElastoDyn_Types.obj

#ServoDyn_Types.f90:               ServoDyn_Registry.txt
ServoDyn_Types.obj:               NWTC_Library.obj
BladedInterface.obj:              NWTC_Library.obj  ServoDyn_Types.obj
ServoDyn.obj:                     NWTC_Library.obj  ServoDyn_Types.obj  PitchCntrl_ACH.obj  UserSubs.obj   UserVSCont_KP.obj  BladedInterface.obj
PitchCntrl_ACH.obj:               NWTC_Library.obj
UserSubs.obj:                     NWTC_Library.obj
UserVSCont_KP.obj:                NWTC_Library.obj


FAST_Mods.obj:                    NWTC_Library.obj  AeroDyn.obj HydroDyn.obj
FAST_IO.obj:                      NWTC_Library.obj  FAST_Mods.obj  AeroDyn.obj  InflowWindMod.obj  ServoDyn.obj ElastoDyn.obj HydroDyn.obj
FAST_Prog.obj:                    NWTC_Library.obj  FAST_Mods.obj  FAST_IO.obj



ServoDyn_Types.f90:
	$(REGISTRY) ServoDyn_Registry.txt

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

