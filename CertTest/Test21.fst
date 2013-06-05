------- FAST V8.01.* INPUT FILE ------------------------------------------------
NREL 5.0 MW Baseline Wind Turbine for Use in Offshore Analysis. Properties from Dutch Offshore Wind Energy Converter (DOWEC) 6MW Pre-Design (10046_009.pdf) and REpower 5M 5MW (5m_uk.pdf); Compatible with FAST v7.02.00
---------------------- SIMULATION CONTROL --------------------------------------
False         Echo            - Echo input data to <RootName>.ech (flag)
FATAL         AbortLevel      - Error level when simulation should abort {"WARNING", "SEVERE", "FATAL"} (string)
    60.0      TMax            - Total run time (s)
     0.005    DT              - Recommended module time step (s)
---------------------- FEATURE FLAGS -------------------------------------------
True          CompAero        - Compute aerodynamic forces (flag)
True          CompServo       - Compute servodynamics (flag)
True          CompHydro       - Compute hydrodynamics forces (flag)
False         CompSub         - Compute sub-structural dynamics (flag)
True          CompUserPtfmLd  - Compute additional platform loading {false: none, true: user-defined from routine UserPtfmLd} (flag)
False         CompUserTwrLd   - Compute additional tower loading {false: none, true: user-defined from routine UserTwrLd} (flag)
---------------------- INPUT FILES ---------------------------------------------
"5MW_Baseline\NRELOffshrBsline5MW_Floating_TLP_ElastoDyn.dat"  EDFile      - Name of file containing ElastoDyn input parameters (quoted string)
"5MW_Baseline\NRELOffshrBsline5MW_AeroDyn.ipt"                 ADFile      - Name of file containing AeroDyn input parameters (quoted string)
"5MW_Baseline\NRELOffshrBsline5MW_Floating_TLP_ServoDyn.dat"   SrvDFile    - Name of file containing ServoDyn input parameters (quoted string)
"5MW_Baseline\NRELOffshrBsline5MW_Floating_TLP_HydroDyn.dat"   HDFile      - Name of file containing HydroDyn input parameters (quoted string)
"unused"      SDFile      - Name of file containing SubDyn input parameters (quoted string)
---------------------- OUTPUT --------------------------------------------------
  True        SumPrint    - Print summary data to "<RootName>.sum" (flag)
          1   SttsTime    - Amount of time between screen status messages (sec)
      0.005   DT_Out      - Time step for tabular output (sec)
          0   TStart      - Time to begin tabular output (s)
          2   OutFileFmt  - Format for tabular (time-marching) output file(s) (1: text file [<RootName>.out], 2: binary file [<RootName>.outb], 3: both) (switch)
True          TabDelim    - Use tab delimiters in text tabular output file? (flag) {uses spaces if false}
"ES10.3E2"    OutFmt      - Format used for text tabular output (except time).  Resulting field should be 10 characters. (quoted string)
