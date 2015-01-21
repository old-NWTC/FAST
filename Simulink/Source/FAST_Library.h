// routines in FAST_Library_$(PlatformName).dll

extern void FAST_Sizes(char *InputFileName, int *AbortErrLev, int * NumOuts, double * dt, int *ErrStat, char *ErrMsg);
extern void FAST_Start(int *ErrStat, char *ErrMsg);
extern void FAST_Update(int *NumInputs_c, int *NumOutputs_c, double *InputAry, double *OutputAry, int *ErrStat, char *ErrMsg);
extern void FAST_End();

#define INTERFACE_STRING_LENGTH 1024

static int AbortErrLev = 4;      // abort error level; compare with NWTC Library
