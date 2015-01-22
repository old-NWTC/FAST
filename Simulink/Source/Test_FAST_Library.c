
#include "FAST_Library.h"
#include "stdio.h"
#include <string.h>

static double dt;
static double InputAry[2];
static double OutputAry[2];
static int NumInputs = 2;
static int NumOutputs = 2;
static int ErrStat = 0;
static char ErrMsg[INTERFACE_STRING_LENGTH];        // make sure this is the same size as IntfStrLen in FAST_Library.f90
static char InputFileName[INTERFACE_STRING_LENGTH]; // make sure this is the same size as IntfStrLen in FAST_Library.f90

int checkError(const int ErrStat, const char * ErrMsg);

int 
main(int argc, char *argv[], char *env[])
{
   int n_t_global = 0;

      // initialization
   

   strcpy(InputFileName, "../../CertTest/Test01.fst");
   FAST_Sizes(InputFileName, &AbortErrLev, &NumOutputs, &dt, &ErrStat, ErrMsg);

   checkError(ErrStat, ErrMsg);

   FAST_Start(&ErrStat, ErrMsg);
   if (checkError(ErrStat, ErrMsg)) return 1;

   // update
   NumOutputs = 2;
   //NumInputs = 2;

   for (n_t_global = 0; n_t_global < 2; n_t_global++){
      FAST_Update(&NumInputs, &NumOutputs, &InputAry[0], &OutputAry[0], &ErrStat, ErrMsg);
      if (checkError(ErrStat, ErrMsg)) return 1;
   }
   
      // end
      
   FAST_End();



   return 0;
}

int
checkError(const int ErrStat, const char * ErrMsg){

   if (ErrStat != ErrID_None){
      fprintf(stderr, "%s\n", ErrMsg);

      if (ErrStat > AbortErrLev){
         FAST_End();
         return 1;
      }

   }

   return 0;

}
