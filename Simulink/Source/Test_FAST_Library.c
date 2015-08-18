
#include "FAST_Library.h"
#include "stdio.h"
#include <string.h>

static double dt;
static double TMax = 1.0;
static double InputAry[NumFixedInputs];
static double OutputAry[MAXIMUM_OUTPUTS];
static int NumInputs = 10;
static int NumOutputs = 28;
static int ErrStat = 0;
static char ErrMsg[INTERFACE_STRING_LENGTH];        // make sure this is the same size as IntfStrLen in FAST_Library.f90
static char InputFileName[INTERFACE_STRING_LENGTH]; // make sure this is the same size as IntfStrLen in FAST_Library.f90
static char CheckpointFileRoot[INTERFACE_STRING_LENGTH]; // make sure this is the same size as IntfStrLen in FAST_Library.f90

int checkError(const int ErrStat, const char * ErrMsg);

int 
main(int argc, char *argv[], char *env[])
{
   int n_t_global = -2;
   int i = 0;
   int j = 0;
   int k = 0;
   int n_t_global_start = 0;
   int n_checkpoint = -1;
   char ChannelNames[CHANNEL_LENGTH*MAXIMUM_OUTPUTS + 1];
   double InitInputAry[MAXInitINPUTS];

  // char OutList[MAXIMUM_OUTPUTS][CHANNEL_LENGTH + 1];
   char OutList[CHANNEL_LENGTH + 1];

   if (0){ // restart from checkpoint file

      /* ******************************
      restart
      ********************************* */
      /* note that this will set n_t_global inside the FAST library; so the loop below would need to be changed. I can easily return the
      current value of n_t_global if you need it. */
      strcpy(CheckpointFileRoot, "../../CertTest/Test18.1200");
      FAST_Restart(CheckpointFileRoot, &AbortErrLev, &NumOutputs, &dt, &ErrStat, ErrMsg);
      if (checkError(ErrStat, ErrMsg)) return 1;

      //n_t_global_start = 1200;  // we could return this from the checkpoint file, too
   }
   else{
      /* ******************************
      initialization
      ********************************* */
      InitInputAry[0] = 1.0;
      InitInputAry[1] = 0.0;

      // this calls the Init() routines of each module
      strcpy(InputFileName, "../../CertTest/Test18.fst");
      FAST_Sizes(&TMax, InitInputAry, InputFileName, &AbortErrLev, &NumOutputs, &dt, &ErrStat, ErrMsg, ChannelNames);
      if (checkError(ErrStat, ErrMsg)) return 1;


      // prepare to pass the channel names to Simulink (wouldn't do this in OpenFOAM):
      sprintf(ErrMsg, "DT = %f;", dt);
      for (i = 0; i < NumOutputs; i++){
         j = CHANNEL_LENGTH - 1;
         while (ChannelNames[i*CHANNEL_LENGTH + j] == ' '){
            j--;
         }
         strncpy(&OutList[0], &ChannelNames[i*CHANNEL_LENGTH], j + 1);
         OutList[j + 1] = '\0';

         fprintf(stderr, "%d %s\n", i, OutList);
      }

      // this does the first CalcOutput() [at t=0]
      FAST_Start(&NumInputs, &NumOutputs, InputAry, OutputAry, &ErrStat, ErrMsg);
      if (checkError(ErrStat, ErrMsg)) return 1;

   }

   /* ******************************
   call FAST once per time step
   ********************************* */

   for (n_t_global = n_t_global_start; n_t_global < 20; n_t_global++){

      /* ******************************
      if you want to create a checkpoint file:
      ********************************* */
      if (n_t_global == n_checkpoint){
         sprintf(CheckpointFileRoot, "../../CertTest/Test18.%d", n_t_global);
         FAST_CreateCheckpoint(CheckpointFileRoot, &ErrStat, ErrMsg);
         checkError(ErrStat, ErrMsg);
      }


      /* ******************************
      set inputs from this code and call FAST:
      ********************************* */
      //InputAry[ 9] = 50.0;
      //InputAry[10] = 0.0;
      //InputAry[11] = 0.0;

      // this advances the states, calls CalcOutput, and solves for next inputs. Predictor-corrector loop is imbeded here:
      FAST_Update(&NumInputs, &NumOutputs, InputAry, OutputAry, &ErrStat, ErrMsg);
      if (checkError(ErrStat, ErrMsg)) return 1;
   }
   
   /* ******************************
   End the program
   ********************************* */

   FAST_End();


   return 0;
}

int
checkError(const int ErrStat, const char * ErrMsg){

   if (ErrStat != ErrID_None){
      fprintf(stderr, "%s\n", ErrMsg);

      if (ErrStat >= AbortErrLev){
         FAST_End();
         return 1;
      }

   }

   return 0;

}
