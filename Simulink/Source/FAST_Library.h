// routines in FAST_Library_$(PlatformName).dll



extern void FAST_Sizes(char *InputFileName, int *AbortErrLev, int * NumOuts, double * dt, int *ErrStat, char *ErrMsg);
extern void FAST_Start(int *ErrStat, char *ErrMsg);
extern void FAST_Update(int *NumInputs_c, int *NumOutputs_c, double *InputAry, double *OutputAry, int *ErrStat, char *ErrMsg);
extern void FAST_End();

#define INTERFACE_STRING_LENGTH 1024

#define ErrID_None 0 
#define ErrID_Info 1 
#define ErrID_Warn 2 
#define ErrID_Severe 3 
#define ErrID_Fatal 4 

static int AbortErrLev = ErrID_Fatal;      // abort error level; compare with NWTC Library
