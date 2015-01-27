
#include "FAST_Library.h"
#include "stdio.h"
#include <string.h>

static double dt;
static double TMax = 1.0;
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
   int i = 0;
   int j = 0;
   int k = 0;
   char ChannelNames[CHANNEL_LENGTH*MAXIMUM_OUTPUTS + 1];
  // char OutList[MAXIMUM_OUTPUTS][CHANNEL_LENGTH + 1];
   char OutList[CHANNEL_LENGTH + 1];

      // initialization
   


   strcpy(InputFileName, "../../CertTest/Test01.fst");
   FAST_Sizes(&TMax, InputFileName, &AbortErrLev, &NumOutputs, &dt, &ErrStat, ErrMsg, ChannelNames);

   checkError(ErrStat, ErrMsg);

   fprintf(stderr, "DT = %f;", dt);
   sprintf(ErrMsg, "DT = %f;", dt);
   fprintf(stderr, "%s\n", ErrMsg);


   for (i = 0; i < NumOutputs; i++){
      j = CHANNEL_LENGTH - 1;
      while (ChannelNames[i*CHANNEL_LENGTH + j] == ' '){
         j--;
      }
      strncpy(&OutList[0], &ChannelNames[i*CHANNEL_LENGTH], j + 1);
      OutList[j + 1] = '\0';

      fprintf(stderr, "%d %s\n", i, OutList);

      /*chrAry = mxCreateString(OutList);
      mxSetCell(pm, i, chrAry);
      mxDestroyArray(chrAry);*/
   }



   /*
   // put the names of the output channels in a variable called "OutList" in the base matlab workspace
   for (i = 0; i < NumOutputs; i++){
      strncpy(&OutList[i][0], &ChannelNames[i*CHANNEL_LENGTH], CHANNEL_LENGTH);
      OutList[i][CHANNEL_LENGTH] = '\0'; // null terminator
      for (j = CHANNEL_LENGTH - 1; j > 0; j--){
         if (OutList[i][j] == ' ') OutList[i][j] = '\0'; // null terminator
      }

      fprintf(stderr, "%d %s\n", i, OutList[i]);
   }
   */



   /*
   for (i = 0; i < NumOutputs; i++){
      //strncpy(&OutList[i][0], &ChannelNames[i*CHANNEL_LENGTH], CHANNEL_LENGTH);
      OutList[i][CHANNEL_LENGTH] = '\0'; // null terminator
      for (j = CHANNEL_LENGTH - 1; j >= 0; j--){ // remove trailing spaces (not sure this is necessary)
         if (ChannelNames[i*CHANNEL_LENGTH + j] == ' ') {
            OutList[i][j] = '\0'; // null terminator
         }
         else{
            for (k = j; k >= 0; k--){
               OutList[i][k] = ChannelNames[i*CHANNEL_LENGTH + k];
            }
            break;
         }
      }
   }

   for (i = 0; i < NumOutputs; i++){
      fprintf(stderr, "%s\n", OutList[i]);
   }
   */

   FAST_Start(&NumOutputs, &OutputAry[0], &ErrStat, ErrMsg);
   if (checkError(ErrStat, ErrMsg)) return 1;

   // update
   for (n_t_global = 0; n_t_global < 2 ; n_t_global++){
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
