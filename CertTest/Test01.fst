------- FAST V8.01.* INPUT FILE ------------------------------------------------
FAST certification Test #01: AWT-27CR2 with many DOFs with fixed yaw error and steady wind.
---------------------- SIMULATION CONTROL --------------------------------------
False         Echo            - Echo input data to <RootName>.ech (flag)
"FATAL"       AbortLevel      - Error level when simulation should abort {"WARNING", "SEVERE", "FATAL"} (string)
         20   TMax            - Total run time (s)
      0.004   DT              - Recommended module time step (s)
---------------------- FEATURE FLAGS -------------------------------------------
True          CompAero        - Compute aerodynamic forces (flag)
True          CompServo       - Compute servodynamics (flag)
False         CompHydro       - Compute hydrodynamics forces (flag)
False         CompSub         - Compute sub-structural dynamics (flag)
False         CompUserPtfmLd  - Compute additional platform loading {false: none, true: user-defined from routine UserPtfmLd} (flag)
False         CompUserTwrLd   - Compute additional tower loading {false: none, true: user-defined from routine UserTwrLd} (flag)
---------------------- INPUT FILES ---------------------------------------------
"AWT27/Test01_ElastoDyn.dat"    EDFile      - Name of file containing ElastoDyn input parameters (quoted string)
"AWT27/Test01_AD.ipt"    ADFile      - Name of file containing AeroDyn input parameters (quoted string)
"AWT27/Test01_ServoDyn.dat"    SrvDFile    - Name of file containing ServoDyn input parameters (quoted string)
"unused"      HDFile      - Name of file containing HydroDyn input parameters (quoted string)
"unused"      SDFile      - Name of file containing SubDyn input parameters (quoted string)
---------------------- OUTPUT --------------------------------------------------
True          SumPrint    - Print summary data to "<RootName>.sum" (flag)
          1   SttsTime    - Amount of time between screen status messages (sec)
       0.02   DT_Out      - Time step for tabular output (sec)
         10   TStart      - Time to begin tabular output (s)
          1   OutFileFmt  - Format for tabular (time-marching) output file(s) (1: text file [<RootName>.out], 2: binary file [<RootName>.outb], 3: both) (switch)
True          TabDelim    - Use tab delimiters in text tabular output file? (flag) {uses spaces if false}
"ES10.3E2"    OutFmt      - Format used for text tabular output (except time).  Resulting field should be 10 characters. (quoted string)
