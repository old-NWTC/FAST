#define MAP_CHECKERR() IF(MAP_ERROR_CHECKER(message_from_MAP,status_from_MAP,ErrMsg,ErrStat)) RETURN
#define MAP_CHECKERRQ(string) IF(MAP_ERROR(ErrMsg,ErrStat,string)) RETURN

MODULE MAP
  
  USE MAP_Types
  USE NWTC_Library

  IMPLICIT NONE
  
  PRIVATE
 
  PUBLIC :: MAP_Init
  PUBLIC :: MAP_UpdateStates
  PUBLIC :: MAP_CalcOutput
  PUBLIC :: MAP_End
  PUBLIC :: MAP_ERROR_CHECKER


  ! ==========   initalize_map_base   ======     <----------------------------------------------------------+
  ! Initializes states                                                                           !          |
  INTERFACE                                                                                      !          |
     SUBROUTINE MAP_Initialize_Base(FC_u      , &                                                !          |
                                    FC_p      , &                                                !          |
                                    FC_x      , &                                                !          |
                                    FC_z      , &                                                !          |
                                    FC_O      , &                                                !          |
                                    FC_y      , &                                                !          |
                                    FC_InitOut) &                                                !          |
                                    bind(C,name='map_initialize_msqs_base')                      !          |
       IMPORT                                                                                    !          |
       IMPLICIT NONE                                                                             !          |
       TYPE(MAP_InitOutputType_C)      :: FC_InitOut                                             !          |
       TYPE(MAP_InputType_C)           :: FC_u                                                   !          |
       TYPE(MAP_ParameterType_C)       :: FC_p                                                   !          |
       TYPE(MAP_ContinuousStateType_C) :: FC_x                                                   !          |
       TYPE(MAP_DiscreteStateType_C)   :: FC_xd                                                  !          |
       TYPE(MAP_ConstraintStateType_C) :: FC_z                                                   !          |
       TYPE(MAP_OtherStateType_C)      :: FC_O                                                   !          |
       TYPE(MAP_OutputType_C)          :: FC_y                                                   !          |
     END SUBROUTINE MAP_Initialize_Base                                                          !          |
  END INTERFACE                                                                                  !   -------+
  !==========================================================================================================

  
  ! ==========   MAP_GetHdrString   ======        <---------------------------------------------------------+
  !                                                                                              !          |
  ! Get the string information (label) of all the outputs MAP is providing the FAST glue code    !          | 
  INTERFACE                                                                                      !          | 
     SUBROUTINE MAP_Get_Header_String(FC_int, FC_string, FC_other) &                             !          | 
          BIND(C,name='map_get_header_string')                                                   !          | 
       IMPORT                                                                                    !          | 
       IMPLICIT NONE                                                                             !          | 
       INTEGER(KIND=C_INT)            :: FC_int                                                  !          | 
       TYPE(MAP_OtherStateType_C)     :: FC_other                                                !          | 
       TYPE(C_PTR), DIMENSION(FC_int) :: FC_string                                               !          | 
     END SUBROUTINE MAP_Get_Header_String                                                        !          | 
  END INTERFACE                                                                                  !   -------+
  !==========================================================================================================
  
  
  ! ==========   MAP_ModifyHdrUntString   ======     <------------------------------------------------------+
  !                                                                                              !          |
  ! Gets the units of all the outputs MAP is providing to the FAST glue code                     !          | 
  INTERFACE                                                                                      !          | 
     SUBROUTINE MAP_Get_Unit_String(FC_int, FC_string, FC_other) &                               !          | 
          BIND(C,name='map_get_unit_string')                                                     !          | 
       IMPORT                                                                                    !          | 
       IMPLICIT NONE                                                                             !          | 
       INTEGER(KIND=C_INT)            :: FC_int                                                  !          | 
       TYPE(MAP_OtherStateType_C)     :: FC_other                                                !          | 
       TYPE(C_PTR), DIMENSION(FC_int) :: FC_string                                               !          | 
     END SUBROUTINE MAP_Get_Unit_String                                                          !          | 
  END INTERFACE                                                                                  !   -------+
  !==========================================================================================================
  

  ! ==========   MAP_SetGravity   ======     <--------------------------------------------------------------+
  !                                                                                              !          |
  ! Calls C function "MAPCALL_SetGravity(MAP_InitInputType)" in MAP_FortranBinding.cpp.          !          |
  ! The idea is to use the gravity constant as defined by FAST, rather than inputing             !          |
  !   something indenpendent of it. Numerical errors can generate is g (in units of [Nm/s^2]     !          |
  !   is not consistent among modules.                                                           !          |
  INTERFACE                                                                                      !          |
     SUBROUTINE MAP_set_gravity(interf, val) bind(C,name='map_set_gravity')                      !          |
       IMPORT                                                                                    !          |
       IMPLICIT NONE                                                                             !          |
       TYPE(MAP_ParameterType_C) :: interf                                                       !          |
       REAL(C_DOUBLE), VALUE     :: val                                                          !          |
     END SUBROUTINE MAP_set_gravity                                                              !          |
  END INTERFACE                                                                                  !   -------+
  !==========================================================================================================
  
  
  ! ==========   MAP_SetDepth   ======     <----------------------------------------------------------------+
  !                                                                                              !          |
  ! Calls C function "MAPCALL_SetDepth(MAP_InitInputType)" in MAP_FortranBinding.cpp.            !          |
  ! The idea is to use the gravity constant as defined by FAST, rather than inputing             !          |
  !   something indenpendent of it. Numerical errors can generate is g (in units of [Nm/s^2]     !          |
  !   is not consistent among modules.                                                           !          |
  INTERFACE                                                                                      !          |
     SUBROUTINE MAP_set_depth(interf, val) BIND(C,name='map_set_sea_depth')                      !          |
       IMPORT                                                                                    !          |
       IMPLICIT NONE                                                                             !          |
       TYPE(MAP_ParameterType_C) :: interf                                                       !          |
       REAL(C_DOUBLE), VALUE     :: val                                                          !          |
     END SUBROUTINE MAP_set_depth                                                                !          |
  END INTERFACE                                                                                  !   -------+
  !==========================================================================================================
  
  
  ! ==========   MAP_set_summary_file_name   ======     <---------------------------------------------------+
  !                                                                                              !          |
  ! Calls C function "MAPCALL_SetSummaryFilename(MAP_InitInputType)" in MAP_FortranBinding.cpp.  !          |
  INTERFACE                                                                                      !          |
     SUBROUTINE MAP_set_summary_file_name(interf,msg,err) &                                      !          |
          BIND(C,name='map_set_summary_file_name')                                               !          |
       IMPORT                                                                                    !          |
       IMPLICIT NONE                                                                             !          |
       TYPE( MAP_InitInputType_C ) interf                                                        !          |
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(INOUT) :: msg                                !          |
       INTEGER(KIND=C_INT) :: err                                                                !          |
     END SUBROUTINE MAP_set_summary_file_name                                                    !          |
  END INTERFACE                                                                                  !   -------+
  !==========================================================================================================

  
  ! ==========   MAP_SetDensity   ======     <--------------------------------------------------------------+
  !                                                                                              !          |
  ! Calls C function "MAPCALL_SetDensity(MAP_InitInputType)" in MAP_FortranBinding.cpp.          !          |
  ! Sets the density of seawater [kg/m^3] according to what is being used in HydroDyn/FAST       !          |
  INTERFACE                                                                                      !          |
     SUBROUTINE MAP_set_density(interf, val) BIND(C,name='map_set_sea_density')                  !          |
       IMPORT                                                                                    !          |
       IMPLICIT NONE                                                                             !          |
       TYPE(MAP_ParameterType_C) :: interf                                                       !          |
       REAL(C_DOUBLE), VALUE     :: val                                                          !          |
     END SUBROUTINE MAP_set_density                                                              !          |
  END INTERFACE                                                                                  !   -------+
  !==========================================================================================================
  
  
  ! ==========   MAP_SetCableLibraryData   ======     <-----------------------------------------------------+
  !                                                                                              !          |
  ! Calls C function "MAPCALL_SetCableLibaryData(MAP_InitInputType)" in MAP_FortranBinding.cpp.  !          |
  ! Pases strings from the "LINE DICTIONARY" porition of the MPA input file to the C++           !          |
  !   data structure.                                                                            !          |
  INTERFACE                                                                                      !          |
     SUBROUTINE MAP_SetCableLibraryData(interf) BIND(C,name='map_add_cable_library_input_text')  !          |
       IMPORT                                                                                    !          |
       IMPLICIT NONE                                                                             !          |
       TYPE(MAP_InitInputType_C) :: interf                                                       !          |
     END SUBROUTINE MAP_SetCableLibraryData                                                      !          |
  END INTERFACE                                                                                  !   -------+
  !==========================================================================================================
   
   
  ! ==========   MAP_SetNodeData   ======     <-------------------------------------------------------------+
  !                                                                                              !          |
  ! Calls C function "MAPCALL_SetNodeData(MAP_InitInputType)" in MAP_FortranBinding.cpp.         !          |
  ! Pases strings from the "NOE PROPERTIES" porition of the MPA input file to the C++            !          |
  !   data structure.                                                                            !          |
  INTERFACE                                                                                      !          |
     SUBROUTINE MAP_SetNodeData(interf) BIND(C,name='map_add_node_input_text')                   !          |
       IMPORT                                                                                    !          |
       IMPLICIT NONE                                                                             !          |
       TYPE(MAP_InitInputType_C) :: interf                                                       !          |
     END SUBROUTINE MAP_SetNodeData                                                              !          |
  END INTERFACE                                                                                  !   -------+
  !==========================================================================================================
  
  
  ! ==========   MAP_SetElementData   ======     <----------------------------------------------------------+
  !                                                                                              !          |
  ! Calls C function "MAP_SetElementData(MAP_InitInputType)" in MAP_FortranBinding.cpp.          !          |
  ! Pases strings from the "ELEMENT PROPERTIES" porition of the MPA input file to the C++        !          |
  !   data structure.                                                                            !          |
  INTERFACE                                                                                      !          |
     SUBROUTINE MAP_SetElementData(interf) BIND(C,name='map_add_line_input_text')                !          |
       IMPORT                                                                                    !          |
       IMPLICIT NONE                                                                             !          |
       TYPE( MAP_InitInputType_C ) :: interf                                                     !          |
     END SUBROUTINE MAP_SetElementData                                                           !          |
  END INTERFACE                                                                                  !   -------+
  !==========================================================================================================
  
  
  ! ==========   MAP_SetSolverOption   ======     <---------------------------------------------------------+
  !                                                                                              !          |
  ! Calls C function "MAPCALL_SetCableLibaryData(MAP_InitInputType)" in MAP_FortranBinding.cpp.  !          |
  ! Pases strings from the "SOLVER OPTIONS" porition of the MPA input file to the C++            !          |
  !   data structure.                                                                            !          |
  INTERFACE                                                                                      !          |
     SUBROUTINE MAP_SetSolverOptions(interf) BIND(C,name='map_add_options_input_text')           !          |
       IMPORT                                                                                    !          |
       IMPLICIT NONE                                                                             !          |
       TYPE(MAP_InitInputType_C) :: interf                                                       !          |
     END SUBROUTINE MAP_SetSolverOptions                                                         !          |
  END INTERFACE                                                                                  !   -------+
  !==========================================================================================================


  ! ==========   MSQS_Init   ======     <-------------------------------------------------------------------+
  !                                                                                              !          |
  ! Calls C function "MAPCALL_MSQS_Init(...)" in MAP_FortranBinding.cpp.                         !          |
  ! Initializes the model                                                                        !          |
  INTERFACE                                                                                      !          |
     SUBROUTINE MSQS_Init(FC_InitInp, &                                                          !          |
                          FC_u      , &                                                          !          |
                          FC_p      , &                                                          !          |
                          FC_x      , &                                                          !          |
                          FC_xd     , &                                                          !          |
                          FC_z      , &                                                          !          |
                          FC_O      , &                                                          !          |
                          FC_y      , &                                                          !          |
                          FC_InitOut, &                                                          !          |
                          err       , &                                                          !          |
                          msg)        &                                                          !          |
                          BIND(C,name='map_init')                                                !          |
       IMPORT                                                                                    !          |
       IMPLICIT NONE                                                                             !          |
       INTEGER(KIND=C_INT)                     :: err                                            !          |
       CHARACTER(KIND=C_CHAR), DIMENSION(1024) :: msg                                            !          |
       TYPE(MAP_InitInputType_C)               :: FC_InitInp                                     !          |
       TYPE(MAP_InitOutputType_C)              :: FC_InitOut                                     !          |
       TYPE(MAP_InputType_C)                   :: FC_u                                           !          |
       TYPE(MAP_ParameterType_C)               :: FC_p                                           !          |
       TYPE(MAP_ContinuousStateType_C)         :: FC_x                                           !          |
       TYPE(MAP_DiscreteStateType_C)           :: FC_xd                                          !          |
       TYPE(MAP_ConstraintStateType_C)         :: FC_z                                           !          |
       TYPE(MAP_OtherStateType_C)              :: FC_O                                           !          |
       TYPE(MAP_OutputType_C)                  :: FC_y                                           !          |
     END SUBROUTINE MSQS_Init                                                                    !          |
  END INTERFACE                                                                                  !   -------+
  !==========================================================================================================
   
  
  ! ==========   MSQS_UpdateStates   ======     <-----------------------------------------------------------+
  !                                                                                              !          |
  ! Calls C function "mapcall_msqs_update_states(...)" in MAP_FortranBinding.cpp.                !          |
  ! Calculates the new fairlead forces based on an updated fairlead displacement                 !          |
  INTERFACE                                                                                      !          |
     SUBROUTINE MSQS_UpdateStates(time , &                                                       !          |
                                  n    , &                                                       !          |
                                  FC_u , &                                                       !          |
                                  FC_p , &                                                       !          |
                                  FC_x , &                                                       !          |
                                  FC_xd, &                                                       !          |
                                  FC_z , &                                                       !          |
                                  FC_O , &                                                       !          |
                                  err  , &                                                       !          |
                                  msg)   &                                                       !          |
                                  BIND(C,name='map_update_states')                               !          |
       IMPORT                                                                                    !          |
       IMPLICIT NONE                                                                             !          |
       REAL(KIND=C_FLOAT), VALUE               :: time                                           !          |
       INTEGER(KIND=C_INT), VALUE              :: n                                              !          |
       INTEGER(KIND=C_INT)                     :: err                                            !          |
       CHARACTER(KIND=C_CHAR), DIMENSION(1024) :: msg                                            !          |
       TYPE(MAP_InputType_C)                   :: FC_u                                           !          |
       TYPE(MAP_ParameterType_C)               :: FC_p                                           !          |
       TYPE(MAP_ContinuousStateType_C)         :: FC_x                                           !          |
       TYPE(MAP_DiscreteStateType_C)           :: FC_xd                                          !          |
       TYPE(MAP_ConstraintStateType_C)         :: FC_z                                           !          |
       TYPE(MAP_OtherStateType_C)              :: FC_O                                           !          |
     END SUBROUTINE MSQS_UpdateStates                                                            !          |
  END INTERFACE                                                                                  !   -------+
  !==========================================================================================================
   
   
  ! ==========   MSQS_CalcOutput   ======     <-------------------------------------------------------------+
  !                                                                                              !          |
  ! Calls C function "MAPCALL_MSQS_CalcOutput(...)" in MAP_FortranBinding.cpp.                   !          |
  ! Calculates the outputs                                                                       !          |
  INTERFACE                                                                                      !          |
     SUBROUTINE map_calc_output( time  , &                                                       !          |
                                 FC_u  , &                                                       !          |
                                 FC_p  , &                                                       !          |
                                 FC_x  , &                                                       !          |
                                 FC_xd , &                                                       !          |
                                 FC_z  , &                                                       !          |
                                 FC_O  , &                                                       !          |
                                 FC_y  , &                                                       !          |
                                 err   , &                                                       !          |
                                 msg )   &                                                       !          |
                                BIND(C,name='map_calc_output')                                   !          |
       IMPORT                                                                                    !          |
       IMPLICIT NONE                                                                             !          |
       REAL(KIND=C_FLOAT) , VALUE :: time                                                        !          |
       INTEGER(KIND=C_INT) :: err                                                                !          |
       CHARACTER(KIND=C_CHAR), DIMENSION(1024) :: msg                                            !          |
       TYPE(MAP_InputType_C)                   :: FC_u                                           !          |
       TYPE(MAP_ParameterType_C)               :: FC_p                                           !          |
       TYPE(MAP_ContinuousStateType_C)         :: FC_x                                           !          |
       TYPE(MAP_DiscreteStateType_C)           :: FC_xd                                          !          |
       TYPE(MAP_ConstraintStateType_C)         :: FC_z                                           !          |
       TYPE(MAP_OtherStateType_C)              :: FC_O                                           !          |
       TYPE(MAP_OutputType_C)                  :: FC_y                                           !          |
     END SUBROUTINE map_calc_output !MSQS_CalcOutput                                                              !          |
  END INTERFACE                                                                                  !   -------+
  !==========================================================================================================
  
  
  ! ==========   MSQS_End   ======     <--------------------------------------------------------------------+
  !                                                                                              !          |
  ! Calls C function "MAPCALL_MSQS_Init(...)" in MAP_FortranBinding.cpp.                         !          |
  ! Initializes the model                                                                        !          |
  INTERFACE                                                                                      !          |
     SUBROUTINE MSQS_End( FC_u       , &                                                         !          |
                          FC_p       , &                                                         !          |
                          FC_x       , &                                                         !          |
                          FC_xd      , &                                                         !          |
                          FC_z       , &                                                         !          |
                          FC_O       , &                                                         !          |
                          FC_y       , &                                                         !          |
                          err        , &                                                         !          |
                          msg )        &                                                         !          |
                         BIND(C,name='map_end')                                                  !          |
       IMPORT                                                                                    !          |
       IMPLICIT NONE                                                                             !          |
       INTEGER(KIND=C_INT) :: err                                                                !          |
       CHARACTER(KIND=C_CHAR),DIMENSION(1024) :: msg                                             !          |
       TYPE( MAP_InputType_C ) FC_u                                                              !          |
       TYPE( MAP_ParameterType_C ) FC_p                                                          !          |
       TYPE( MAP_ContinuousStateType_C ) FC_x                                                    !          |
       TYPE( MAP_DiscreteStateType_C ) FC_xd                                                     !          |
       TYPE( MAP_ConstraintStateType_C ) FC_z                                                    !          |
       TYPE( MAP_OtherStateType_C ) FC_O                                                         !          |
       TYPE( MAP_OutputType_C ) FC_y                                                             !          |
     END SUBROUTINE MSQS_End                                                                     !          |
  END INTERFACE                                                                                  !   -------+
  !==========================================================================================================

CONTAINS


  !==========   MAP_Init   ======     <----------------------------------------------------------------------+
  SUBROUTINE MAP_Init( InitInp, u, p, x, xd, z, other, y, Interval, InitOut, ErrStat, ErrMsg )    
    IMPLICIT NONE
    TYPE( MAP_InitInputType ),       INTENT(INOUT)  :: InitInp     ! INTENT(IN  ) : Input data for initialization routine
    TYPE( MAP_InputType ),           INTENT(  OUT)  :: u           ! INTENT(  OUT) : An initial guess for the input; input mesh must be defined
    TYPE( MAP_ParameterType ),       INTENT(  OUT)  :: p           ! INTENT(  OUT) : Parameters
    TYPE( MAP_ContinuousStateType ), INTENT(  OUT)  :: x           ! INTENT(  OUT) : Initial continuous states
    TYPE( MAP_DiscreteStateType ),   INTENT(  OUT)  :: xd          ! INTENT(  OUT) : Initial discrete states
    TYPE( MAP_ConstraintStateType ), INTENT(  OUT)  :: z           ! INTENT(  OUT) : Initial guess of the constraint states
    TYPE( MAP_OtherStateType ),      INTENT(  OUT)  :: other       ! INTENT(  OUT) : Initial other/optimization states
    TYPE( MAP_OutputType ),          INTENT(  OUT)  :: y           ! INTENT(  OUT) : Initial system outputs (outputs are not calculated; only the output mesh is initialized)
    REAL(DbKi),                      INTENT(INOUT)  :: Interval    ! Coupling interval in seconds: the rate that Output is the actual coupling interval 
    TYPE( MAP_InitOutputType ),      INTENT(INOUT)  :: InitOut     ! Output for initialization routine
    INTEGER(IntKi),                  INTENT(  OUT)  :: ErrStat     ! Error status of the operation
    CHARACTER(*),                    INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None

    ! Local variables
    INTEGER(KIND=C_INT)                             :: status_from_MAP 
    CHARACTER(KIND=C_CHAR), DIMENSION(1024)         :: message_from_MAP
    
    INTEGER(IntKi)                                  :: i
    INTEGER(IntKi)                                  :: n
    REAL(ReKi)                                      :: Pos(3)
    INTEGER(IntKi)                                  :: NumNodes

    ErrStat = ErrID_None
    ErrMsg  = "" 
 
    status_from_MAP = 0
    message_from_MAP =  " "
    i = 0
    N = 0
    NumNodes = 0
    p%dt = interval 
    p%C_obj%dt = p%dt

    CALL NWTC_Init( )  ! Initialize the NWTC Subroutine Library     
    CALL MAP_InitInput_Initialize(InitInp%C_obj%object, message_from_MAP, status_from_MAP); MAP_CHECKERR() ! Call the constructor for each MAP class to create and instance of each C++ object        
    CALL MAP_Other_Initialize(other%C_obj%object, message_from_MAP, status_from_MAP); MAP_CHECKERR() 
    CALL MAP_Initialize_Base(u%C_obj, p%C_obj, x%C_obj, z%C_obj, other%C_obj, y%C_obj, InitOut%C_obj)

    ! Set the environmental properties:
    !   depth           = water depth [m]
    !   gravity         = the acceleration due to gravity [N] -or- [kg*m/s^2]
    !   sea_density     = density of sea water [kg/m^3]
    !   coupled_to_FAST = flag letting MAP know it is coupled to FAST. MAP won't create the output file when .TRUE.
    InitInp%C_Obj%gravity           =  InitInp%gravity
    InitInp%C_Obj%sea_density       =  InitInp%sea_density
    InitInp%C_Obj%depth             = -InitInp%depth  !BJJ: Why is this the negative? I have to put a negative in the glue code, too. Let's get rid of both of them
    
    N = LEN_TRIM(InitInp%summary_file_name)
    DO i = 1,N
       InitInp%C_Obj%summary_file_name(i) = InitInp%summary_file_name(i:i)  
    END DO !bjj: add C_NULL_CHAR???
   
    ! Set the gravity constant, water depth, and sea density in MAP.
    ! This calls functions in MAP_FortranBinding.cpp
    CALL MAP_set_gravity(p%C_obj, InitInp%C_Obj%gravity)
    CALL MAP_set_depth(p%C_obj, InitInp%C_Obj%depth)
    CALL MAP_set_density(p%C_obj, InitInp%C_Obj%sea_density)
    CALL MAP_set_summary_file_name(InitInp%C_obj, message_from_map, status_from_MAP); MAP_CHECKERR()

    ! Read the MAP input file, and pass the arguments to the C++ sructures. 
    ! @note : this call the following C function in MAP_FortranBinding.cpp
    CALL map_read_input_file_contents(InitInp%file_name , InitInp, ErrStat)
    MAP_CHECKERRQ("MAP_ERROR[FORT]: cannot read the MAP input file.")

    ! This binds MSQS_Init function in C++ with Fortran
    CALL MSQS_Init(InitInp%C_obj   , &
                   u%C_obj         , &
                   p%C_obj         , &
                   x%C_obj         , &
                   xd%C_obj        , &
                   z%C_obj         , &
                   other%C_obj     , &
                   y%C_obj         , &
                   InitOut%C_obj   , &
                   status_from_MAP , &
                   message_from_MAP )
    MAP_CHECKERR()

    ! ==========   MAP F2C (literally, Fortran to C) conversion   ===========================================
    ! Now call the C2FC_ routines for the INTENT(  OUT) C objects
    !  MAP Inputs
    !  MAP Constraint State
    !  MAP Other State
    !  MAP Output State
    ! =======================================================================================================  
    CALL MAP_C2F_ConstrState_Array_Allocation(z, ErrStat, ErrMsg); MAP_CHECKERRQ("MAP_ERROR[FORT]: FAST/MAP C2F constraint state conversion error.")
    CALL MAP_C2F_Output_Array_Allocation(y, ErrStat, ErrMsg); MAP_CHECKERRQ("MAP_ERROR[FORT]: FAST/MAP C2F output state conversion error.")
    CALL MAP_C2F_OtherState_Array_Allocation(other, ErrStat, ErrMsg); MAP_CHECKERRQ("MAP_ERROR[FORT]: FAST/MAP C2F other state conversion error.")
    CALL MAP_C2F_Input_Array_Allocation(u, ErrStat, ErrMsg); MAP_CHECKERRQ("MAP_ERROR[FORT]: FAST/MAP C2F input state conversion error.")
    CALL MAP_Get_Output_Headers(InitOut, other)
    
    !==========   MAP Mesh initialization   ======     <--------------------------+               
    ! get header information for the FAST output file                  !          | 
    NumNodes = u%C_obj%X_Len                                           !          |    
    ! Create the input mesh                                            !          |
    CALL MeshCreate(BlankMesh=u%PtFairDisplacement ,IOS= COMPONENT_INPUT, NNodes=NumNodes, TranslationDisp=.TRUE.,ErrStat=ErrStat, ErrMess=ErrMsg)
    IF(ErrStat/=ErrID_None) CALL WrScr(TRIM(ErrMsg))                   !          |
                                                                       !          |
    DO i = 1,NumNodes                                                  !          |
       Pos(1) = u%X(i)                                                 !          |
       Pos(2) = u%Y(i)                                                 !          |
       Pos(3) = u%Z(i)                                                 !          |
                                                                       !          |
       CALL MeshPositionNode(u%PtFairDisplacement,i,Pos,ErrStat,ErrMsg)!          |
       IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))             !          |
                                                                       !          |
       CALL MeshConstructElement(u%PtFairDisplacement, ELEMENT_POINT, ErrStat, ErrMsg, i)
       IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))             !          |
    END DO                                                             !          |
                                                                       !          |
    CALL MeshCommit ( u%PtFairDisplacement, ErrStat, ErrMsg )          !          |
    IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))                !          |
                                                                       !          |
    ! now, copy the input PtFairDisplacement to output                 !          |
    ! PtFairleadLoad to complete this                                  !          |
    CALL MeshCopy ( SrcMesh  = u%PtFairDisplacement , &                !          |
                    DestMesh = y%PtFairleadLoad     , &                !          |
                    CtrlCode = MESH_SIBLING         , &                !          |
                    IOS      = COMPONENT_OUTPUT     , &                !          |
                    Force    = .TRUE.               , &                !          |
                    ErrStat  = ErrStat              , &                !          |
                    ErrMess  = ErrMsg                 )                !          |
    IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))                !          |
                                                                       !          |
    ! End mesh initialization                                          !   -------+
    !==============================================================================
         
    ! Program version
    N = LEN(InitOut%version)
    DO i=1,N
       IF (InitOut%C_obj%version(i).EQ.C_NULL_CHAR) THEN
          InitOut%version(i:N) = ' '
          EXIT
       ELSE
          InitOut%version(i:i)  = InitOut%C_obj%version(i)
       END IF
    END DO
    
    ! Program compiling data
    N = LEN(InitOut%compilingData)
    DO i=1,N
       IF (InitOut%C_obj%compilingData(i).EQ.C_NULL_CHAR) THEN
          InitOut%compilingData(i:N) = ' '
          EXIT
       ELSE
          InitOut%compilingData(i:i)  = InitOut%C_obj%compilingData(i)
       END IF
    END DO
    
    InitOut%Ver = ProgDesc('MAP++',TRIM(InitOut%version),TRIM(InitOut%compilingData))
!IF (ALLOCATED(InitOut%WriteOutputHdr)) WRITE(*,*) InitOut%WriteOutputHdr ! @bonnie : this is artificial. Remove.
!IF (ALLOCATED(InitOut%WriteOutputUnt)) WRITE(*,*) InitOut%WriteOutputUnt ! @bonnie : this is artificial. Remove.

   IF ( ALLOCATED(InitOut%WriteOutputHdr) ) THEN
      ALLOCATE( y%WriteOutput(SIZE(InitOut%WriteOutputHdr)), STAT=n)
      IF (N/=0) CALL SetErrStat(ErrID_Fatal, 'Failed to allocate y%WriteOutput',ErrStat, ErrMsg, 'MAP_Init')
   END IF
  
  END SUBROUTINE MAP_Init                                                                        !   -------+
  !==========================================================================================================


  !==========   MAP_UpdateStates   ======     <-------------------------------------------------------------+
  SUBROUTINE MAP_UpdateStates( t, n, u, utimes, p, x, xd, z, O, ErrStat, ErrMsg)    
    REAL(DbKi)                      , INTENT(IN   ) :: t
    INTEGER(IntKi)                  , INTENT(IN   ) :: n
    REAL(DbKi)                      , INTENT(IN   ) :: utimes(:)
    TYPE( MAP_InputType )           , INTENT(INOUT) :: u(:)       ! INTENT(IN   )
    TYPE( MAP_ParameterType )       , INTENT(INOUT) :: p          ! INTENT(IN   )
    TYPE( MAP_ContinuousStateType ) , INTENT(INOUT) :: x          ! INTENT(INOUT)
    TYPE( MAP_DiscreteStateType )   , INTENT(INOUT) :: xd         ! INTENT(INOUT)
    TYPE( MAP_ConstraintStateType ) , INTENT(INOUT) :: z          ! INTENT(INOUT)
    TYPE( MAP_OtherStateType )      , INTENT(INOUT) :: O          ! INTENT(INOUT)
    INTEGER(IntKi)                  , INTENT(  OUT) :: ErrStat    ! Error status of the operation
    CHARACTER(*)                    , INTENT(  OUT) :: ErrMsg     ! Error message if ErrStat /= ErrID_None
  
    ! Local variables
    INTEGER(KIND=C_INT)                             :: status_from_MAP = 0
    CHARACTER(KIND=C_CHAR), DIMENSION(1024)         :: message_from_MAP = ' '
    ! CHARACTER(KIND=C_CHAR,len=1024)                 :: message_from_MAP = ""//CHAR(0)
    REAL(KIND=C_FLOAT)                              :: time = 0
    INTEGER(KIND=C_INT)                             :: interval = 0
    INTEGER(IntKi)                                  :: i=0  
    TYPE(MAP_InputType)                             :: u_interp    ! Inputs at t
    
    ! create space for arrays/meshes in u_interp
    CALL MAP_CopyInput(u(1), u_interp, MESH_NEWCOPY, ErrStat, ErrMsg)          
    CALL MAP_Input_ExtrapInterp(u, utimes, u_interp, t+p%dt, ErrStat, ErrMsg)
    !CALL CheckError(ErrStat2,ErrMsg2)
    !IF ( ErrStat >= AbortErrLev ) RETURN
    
    ! set the time and coupling interval to something readable by MAP (using KIND=C_INT/C_FLOAT instead
    ! of the native IntKi/DbKi format in FAST)
    time = t
    interval = n
    
    ! Copy the mesh input to the MAP C types
    ! @marco: the Position field is fixed in the initialization routine. TranslationDisp is the displacement from the original position.
    !         if you need the absolute position, add them: u_interp%PtFairDisplacement(1)%TranslationDisp(1,i) + u_interp%PtFairleadDisplacement(1)%Pos
    ! Copy the mesh input to the MAP C types
    DO i = 1,u_interp%PtFairDisplacement%NNodes
       u_interp%X(i) = u_interp%PtFairDisplacement%Position(1,i) + u_interp%PtFairDisplacement%TranslationDisp(1,i)
       u_interp%Y(i) = u_interp%PtFairDisplacement%Position(2,i) + u_interp%PtFairDisplacement%TranslationDisp(2,i)
       u_interp%Z(i) = u_interp%PtFairDisplacement%Position(3,i) + u_interp%PtFairDisplacement%TranslationDisp(3,i)
    END DO
    
    ! @bonnie: remove. This is just to test MAP by using fake fairlead displacements
    ! write (*,*) u_interp%X(1)!u(1)%x(1)

    CALL copy_inputs_for_c(u_interp, ErrStat, ErrMsg)

    ! @bonnie: If we pass in u(1) as an argument into MSQS_UpdateStates, then the program behaves as expected. It does not seem 
    !          that u_interp is setup in a way to be handled by the C code -> this is why the states are not updated when 
    !          UpdateStates is called. I think there is a discrepancy here because the arrays are allocated/deallocation in C. 
    !          I think MAP_Input_ExtrapInterp neglected this detail? If so, this might also explain the memory leaks.     
    CALL MSQS_UpdateStates( time            , &
                            interval        , & 
                            u_interp%C_obj  , &
                            p%C_obj         , &
                            x%C_obj         , &
                            xd%C_obj        , &
                            z%C_obj         , &
                            O%C_obj         , &
                            status_from_MAP , &
                            message_from_MAP  )
    MAP_CHECKERR()
    
    ! delete the temporary input arrays/meshes 
    CALL deallocate_primitives_for_c(u_interp, ErrStat, ErrMsg)
    CALL MAP_DestroyInput(u_interp, ErrStat, ErrMsg)        
  END SUBROUTINE MAP_UpdateStates                                                                !   -------+
  !==========================================================================================================
  
   
  !==========   MAP_CalcOutput   ======     <---------------------------------------------------------------+  
  SUBROUTINE MAP_CalcOutput( t, u, p, x, xd, z, O, y, ErrStat, ErrMsg )    
    REAL(DbKi)                      , INTENT(IN   ) :: t
    TYPE( MAP_InputType )           , INTENT(INOUT) :: u       ! INTENT(IN   )
    TYPE( MAP_ParameterType )       , INTENT(INOUT) :: p       ! INTENT(IN   )
    TYPE( MAP_ContinuousStateType ) , INTENT(INOUT) :: x       ! INTENT(IN   )
    TYPE( MAP_DiscreteStateType )   , INTENT(INOUT) :: xd      ! INTENT(IN   )
    TYPE( MAP_ConstraintStateType ) , INTENT(INOUT) :: z       ! INTENT(IN   )
    TYPE( MAP_OtherStateType )      , INTENT(INOUT) :: O       ! INTENT(INOUT)
    TYPE( MAP_OutputType )          , INTENT(INOUT) :: y       ! INTENT(INOUT)
    INTEGER(IntKi)                  , INTENT(  OUT) :: ErrStat
    CHARACTER(*)                    , INTENT(  OUT) :: ErrMsg 
  
    ! Local variables
    INTEGER(KIND=C_INT)                             :: status_from_MAP
    CHARACTER(KIND=C_CHAR), DIMENSION(1024)         :: message_from_MAP = ' '
    REAL(KIND=C_FLOAT)                              :: time = 0
    integer                                         :: i
  
    time = t
    
    DO i = 1,u%PtFairDisplacement%NNodes
       u%X(i) = u%PtFairDisplacement%Position(1,i) + u%PtFairDisplacement%TranslationDisp(1,i)
       u%Y(i) = u%PtFairDisplacement%Position(2,i) + u%PtFairDisplacement%TranslationDisp(2,i)
       u%Z(i) = u%PtFairDisplacement%Position(3,i) + u%PtFairDisplacement%TranslationDisp(3,i)
    END DO
  
    CALL map_calc_output(time            , & 
                         u%C_obj         , &
                         p%C_obj         , &
                         x%C_obj         , &
                         xd%C_obj        , &
                         z%C_obj         , &
                         O%C_obj         , &
                         y%C_obj         , &
                         status_from_MAP , &
                         message_from_MAP ) 
    MAP_CHECKERR()
  
  !BJJ: Why doesn't this work? 

   IF (ALLOCATED(y%WriteOutput) .AND. ASSOCIATED(y%WrtOutput) ) y%WriteOutput = REAL( y%WrtOutput, ReKi ) 
   ! IF (ALLOCATED(y%WriteOutput)) WRITE(*,*) y%WriteOutput ! @bonnie : remove
  !  WRITE(*,*) y%wrtOutput ! @bonnie : remove
  !  write(*,*)
    ! Copy the MAP C output types to the native Fortran mesh output types
    DO i = 1,y%ptFairleadLoad%NNodes
       y%ptFairleadLoad%Force(1,i) = -y%FX(i) 
       y%ptFairleadLoad%Force(2,i) = -y%FY(i)
       y%ptFairleadLoad%Force(3,i) = -y%FZ(i)
    END DO  
  END SUBROUTINE MAP_CalcOutput                                                                  !   -------+
  !==========================================================================================================


  !==========   MAP_End   ======     <----------------------------------------------------------------------+
  SUBROUTINE MAP_End(u, p, x, xd, z, other, y, ErrStat , ErrMsg)                           
    TYPE( MAP_InputType ) ,           INTENT(INOUT) :: u                                 
    TYPE( MAP_ParameterType ) ,       INTENT(INOUT) :: p                                 
    TYPE( MAP_ContinuousStateType ) , INTENT(INOUT) :: x                                 
    TYPE( MAP_DiscreteStateType ) ,   INTENT(INOUT) :: xd                                
    TYPE( MAP_ConstraintStateType ) , INTENT(INOUT) :: z                                 
    TYPE( MAP_OtherStateType ) ,      INTENT(INOUT) :: other                                 
    TYPE( MAP_OutputType ) ,          INTENT(INOUT) :: y                                 
    INTEGER(IntKi),                   INTENT(  OUT) :: ErrStat                           
    CHARACTER(*),                     INTENT(  OUT) :: ErrMsg                            

    ! Locals
    INTEGER(KIND=C_INT)                             :: status_from_MAP=0                 
    CHARACTER(KIND=C_CHAR), DIMENSION(1024)         :: message_from_MAP = ' '
    INTEGER(IntKi)                                  :: i=0 
                                                                                         
    ErrStat = ErrID_None                                                                 
    ErrMsg  = ""                                                                             

    CALL MSQS_End( u%C_obj         , &                                                   
                   p%C_obj         , &                                                   
                   x%C_obj         , &                                                   
                   xd%C_obj        , &                                                   
                   z%C_obj         , &                                                   
                   other%C_obj     , &                                                   
                   y%C_obj         , &                                                   
                   status_from_MAP , &                                                   
                   message_from_MAP  )                                                    
    MAP_CHECKERR()

    ! bjj: we need to nullify Fortran pointers that were associated with C_F_POINTER in the Init routine:
    ! 
        
    IF (.NOT. C_ASSOCIATED( u%C_obj%x              ) ) NULLIFY( u%x              )
    IF (.NOT. C_ASSOCIATED( u%C_obj%y              ) ) NULLIFY( u%y              )
    IF (.NOT. C_ASSOCIATED( u%C_obj%z              ) ) NULLIFY( u%z              )
                                                                                 
    IF (.NOT. C_ASSOCIATED( z%C_obj%H              ) ) NULLIFY( z%H              )
    IF (.NOT. C_ASSOCIATED( z%C_obj%V              ) ) NULLIFY( z%V              )
    IF (.NOT. C_ASSOCIATED( z%C_obj%x              ) ) NULLIFY( z%x              )
    IF (.NOT. C_ASSOCIATED( z%C_obj%y              ) ) NULLIFY( z%y              )
    IF (.NOT. C_ASSOCIATED( z%C_obj%z              ) ) NULLIFY( z%z              )
                                                                                 
    IF (.NOT. C_ASSOCIATED( y%C_obj%Fx             ) ) NULLIFY( y%Fx             )
    IF (.NOT. C_ASSOCIATED( y%C_obj%Fy             ) ) NULLIFY( y%Fy             )
    IF (.NOT. C_ASSOCIATED( y%C_obj%Fz             ) ) NULLIFY( y%Fz             )
    IF (.NOT. C_ASSOCIATED( y%C_obj%wrtOutput      ) ) NULLIFY( y%wrtOutput      )    
    
    IF (.NOT. C_ASSOCIATED( other%C_obj%x          ) ) NULLIFY( other%x          )
    IF (.NOT. C_ASSOCIATED( other%C_obj%y          ) ) NULLIFY( other%y          )
    IF (.NOT. C_ASSOCIATED( other%C_obj%z          ) ) NULLIFY( other%z          )
    IF (.NOT. C_ASSOCIATED( other%C_obj%Fx_connect ) ) NULLIFY( other%Fx_connect )
    IF (.NOT. C_ASSOCIATED( other%C_obj%Fy_connect ) ) NULLIFY( other%Fy_connect )
    IF (.NOT. C_ASSOCIATED( other%C_obj%Fz_connect ) ) NULLIFY( other%Fz_connect )
    IF (.NOT. C_ASSOCIATED( other%C_obj%Fx_anchor  ) ) NULLIFY( other%Fx_anchor  )
    IF (.NOT. C_ASSOCIATED( other%C_obj%Fy_anchor  ) ) NULLIFY( other%Fy_anchor  )
    IF (.NOT. C_ASSOCIATED( other%C_obj%Fz_anchor  ) ) NULLIFY( other%Fz_anchor  )
    

    
    
    
    ! Destroy Fortran MAP types
    ! Anything allocated in C should be destroyed in C. Calling these functions only destroys mesh types. 
    CALL MAP_DestroyInput(u, ErrStat, ErrMsg)
    CALL MAP_DestroyParam(p , ErrStat, ErrMsg)
    CALL MAP_DestroyContState(x, ErrStat, ErrMsg)
    CALL MAP_DestroyDiscState(xd, ErrStat, ErrMsg)
    CALL MAP_DestroyConstrState(z, ErrStat, ErrMsg)
    CALL MAP_DestroyOtherState(other, ErrStat, ErrMsg)
    CALL MAP_DestroyOutput(y, ErrStat, ErrMsg)
  END SUBROUTINE MAP_End                                                                         !   -------+
  !==========================================================================================================


 ! ==========   MAP_ReadInputFileContents   ======     <---------------------------------------------------+
 !                                                                                              !          |
  ! Reads the MAP input files. Assumes the MAP input file is formated as demonstrated with the 
  !   MAP distruction archives. Any changes to the format, and this read function may fail.    
  SUBROUTINE map_read_input_file_contents(file, InitInp, ErrStat)                              
    TYPE(MAP_InitInputType), INTENT(INOUT) :: InitInp                     
    CHARACTER(255) ,         INTENT(IN   ) :: file                        
    INTEGER(IntKi),          INTENT(  OUT) :: ErrStat                     
    INTEGER                                :: success                     
    INTEGER                                :: index_begn=1                
    INTEGER                                :: index_cabl=0                
    INTEGER                                :: index_node=0                
    INTEGER                                :: index_elem=0                
    INTEGER                                :: index_optn=0                
    INTEGER                                :: i = 0
    INTEGER                                :: N = 0
    CHARACTER(255)                         :: line
   
    INTEGER                                :: Un
    CHARACTER(1024)                        :: ErrMsg
                                                                                                 
    ErrStat = ErrID_None  
    
    ! Open the MAP input file
    Un = -1  
    CALL GetNewUnit( Un, ErrStat, ErrMsg )
    CALL OpenFInpFile ( Un, file, ErrStat, ErrMsg )
    IF ( ErrStat >= AbortErrLev )RETURN

    ! Read the contents of the MAP input file                                     
    DO                                          
       READ( Un , '(A)' , IOSTAT=success ) line ! read one line of the MAP input file
                                                                   
      ! we are no longer reading the MAP input file if we          
      !   reached the end                                          
      IF ( success.NE.0 ) EXIT                                     
                                                                   
      ! populate the cable library parameter                       
      IF ( index_begn.EQ.1 ) THEN                                  
         index_cabl = index_cabl + 1                               
         IF ( index_cabl.GE.4 ) THEN                               
             IF ( line(1:1).EQ."-" ) THEN                    
               index_begn=2                                        
            ELSE                                                   
                N = LEN_TRIM(line)
                DO i = 1,N
                   InitInp%C_obj%library_input_str(i) = line(i:i)   
                END DO
                InitInp%C_obj%library_input_str(N+1) = ' ' 
                InitInp%C_obj%library_input_str(N+2) = C_NULL_CHAR
               CALL MAP_SetCableLibraryData(InitInp%C_obj)         
            END IF                                                 
         END IF                                                    
      END IF                                                       
                                                                   
                                                                   
      ! populate the node parameter                                
      IF ( index_begn.EQ.2 ) THEN                                  
         index_node = index_node + 1                               
         IF ( index_node.GE.4 ) THEN                               
             IF ( line(1:1).EQ."-" ) THEN          
               index_begn=3                                        
            ELSE                                                   
                N = LEN_TRIM(line)
                DO i = 1,N
                   InitInp%C_obj%node_input_str(i) = line(i:i)   
                END DO
                InitInp%C_obj%node_input_str(N+1) = ' '
                InitInp%C_obj%node_input_str(N+2) = C_NULL_CHAR
               CALL MAP_SetNodeData(InitInp%C_obj)                 
            END IF                                                 
         END IF                                                    
      END IF                                                       
                                                                   
                                                                   
      ! populate the element parameter                             
      IF ( index_begn.EQ.3 ) THEN                                  
         index_elem = index_elem + 1                               
         IF ( index_elem.GE.4 ) THEN                               
             IF ( line(1:1).EQ."-" ) THEN               
               index_begn=4                                        
            ELSE                                                   
                N = LEN_TRIM(line)
                DO i = 1,N
                   InitInp%C_obj%line_input_str(i) = line(i:i)   
                END DO
                InitInp%C_obj%line_input_str(N+1) = ' '
                InitInp%C_obj%line_input_str(N+2) = C_NULL_CHAR
                CALL MAP_SetElementData(InitInp%C_obj)             
            END IF                                                 
         END IF                                                    
      END IF                                                       
                                                                   
                                                                   
      ! populate the solver options                                
      IF ( index_begn.EQ.4 ) THEN                                  
         index_optn = index_optn + 1                               
         IF ( index_optn.GE.4 ) THEN                               
             IF ( line(1:1).NE."!" )  THEN              
                N = LEN_TRIM(line)
                DO i = 1,N
                   InitInp%C_obj%option_input_str(i) = line(i:i)   
                END DO
                InitInp%C_obj%option_input_str(N+1) = ' '
                InitInp%C_obj%option_input_str(N+2) = C_NULL_CHAR
               CALL MAP_SetSolverOptions(InitInp%C_obj)            
            END IF                                                 
         END IF                                                    
      END IF                                                       
   END DO                                                          
                                                                               
    ! Close the MAP input file                                                                   !          |
    CLOSE( Un )                                                                
  END SUBROUTINE map_read_input_file_contents                                                    !   -------+
 !==========================================================================================================


  ! ==========   MAP_ERROR   ======     <-------------------------------------------------------------------+
  !                                                                                              !          |
  ! this is different from MAP_ERROR_CHECKER. MAP_ERROR check internal fortran errors, whereas
  ! the former checks errors in the MAP DLL.
  LOGICAL FUNCTION MAP_ERROR(ErrMsg, ErrStat, string)
    CHARACTER(1024), INTENT(INOUT) :: ErrMsg 
    INTEGER(IntKi),  INTENT(INOUT) :: ErrStat 
    CHARACTER(*),    INTENT(IN   ) :: string    

    MAP_ERROR = .FALSE.

    IF (ErrStat.NE.ErrID_None) THEN
       ErrMsg = TRIM(ErrMsg)//string
       MAP_ERROR = .TRUE.
    END IF
  END FUNCTION MAP_ERROR                                                                         !   -------+
  !==========================================================================================================

   
  ! ==========   MAP_ERROR_CHECKER   ======     <-----------------------------------------------------------+
  !                                                                                              !          |
  ! A convenient way to convert C-character arrays into a fortran string. The return argustment 
  ! is a logical: False if program is safe; True if program fails in the MAP DLL 
  LOGICAL FUNCTION MAP_ERROR_CHECKER(msg, stat, ErrMsg, ErrStat)
    CHARACTER(KIND=C_CHAR), DIMENSION(1024), INTENT(INOUT) :: msg
    INTEGER(KIND=C_INT),                     INTENT(INOUT) :: stat
    CHARACTER(1024),                         INTENT(  OUT) :: ErrMsg 
    INTEGER(IntKi),                          INTENT(  OUT) :: ErrStat    
    INTEGER                                                :: i = 0                                             

    MAP_ERROR_CHECKER = .FALSE. ! default warning; does not throw a RETURN
    ErrStat = ErrID_None
    ErrMsg  = ""
    
    IF (stat.NE.0) THEN
       DO i = 1,1024 ! convert c-character array to a fortran character array
          IF(msg(i).NE.C_NULL_CHAR) THEN
             ErrMsg(i:i) = msg(i)
          ELSE
             EXIT
          END IF
       END DO

       IF(stat.EQ.1) THEN ! assign warning levels
          ErrStat = ErrID_Warn
          CALL WrScr(ErrMsg)         
       ELSE ! only the case of a fatal warning returns true; throws a RETURN
          ErrStat = ErrID_Fatal
          MAP_ERROR_CHECKER = .TRUE.
       END IF
    END IF
  END FUNCTION MAP_ERROR_CHECKER                                                                 !   -------+
  !==========================================================================================================

 
  ! ==========   MAP_C2F_OtherState_Array_Allocation   ======     <-----------------------------------------+
  !                                                                                              !          |
  ! Get te output header for the FAST text file so something like this is printed:
  !  l[1]     h[1]     psi[1]   alpha[1] alpha_a[1]
  !   [m]     [m]     [m]            [m]        [m]   
  SUBROUTINE MAP_Get_Output_Headers(InitOut, Other) 
    TYPE(MAP_InitOutputType), INTENT(INOUT)  :: InitOut     ! Output for initialization routine
    TYPE(MAP_OtherStateType), INTENT(INOUT)  :: Other   

    ! Locals
    INTEGER :: i = 0
    INTEGER(C_INT)                                  :: numHeaderStr = 0
    CHARACTER(16),DIMENSION(:), ALLOCATABLE, TARGET :: strHdrArray ! Hopefully none of the headers are more than 16 characters long
    TYPE(C_PTR), DIMENSION(:), ALLOCATABLE          :: strHdrPtrs
    CHARACTER(15),DIMENSION(:), ALLOCATABLE, TARGET :: strUntArray ! Hopefully none of the headers are more than 15 characters long
    TYPE(C_PTR), DIMENSION(:), ALLOCATABLE          :: strUntPtrs

    !==========   MAP_InitInpInputType   ======     <--------------------------+
    ! get header information for the FAST output file               
    
    numHeaderStr = InitOut%C_obj%writeOutputHdr_Len    
    ALLOCATE(strHdrArray(numHeaderStr+1))           
    ALLOCATE(strHdrPtrs (numHeaderStr+1))           
    ALLOCATE(strUntArray(numHeaderStr))             
    ALLOCATE(strUntPtrs (numHeaderStr))             
    ALLOCATE(InitOut%WriteOutputHdr(numHeaderStr))  
    ALLOCATE(InitOut%WriteOutputUnt(numHeaderStr))
    
    DO i = 1, numHeaderStr                       
       strHdrArray(i) = "None"//C_NULL_CHAR     
       strUntArray(i) = "None"//C_NULL_CHAR     
       strHdrPtrs(i) = C_LOC(strHdrArray(i))  
       strUntPtrs(i) = C_LOC(strUntArray(i))  
    END DO                                       
    
    CALL MAP_Get_Header_String(numHeaderStr, strHdrPtrs, Other%C_obj)
    CALL MAP_Get_Unit_String(numHeaderStr, strUntPtrs, Other%C_obj)
    
    DO i = 1, numHeaderStr                        
       InitOut%WriteOutputHdr(i) = strHdrArray(i) 
       CALL RemoveNullChar( InitOut%WriteOutputHdr(i) ) 
       InitOut%WriteOutputUnt(i) = strUntArray(i) 
       CALL RemoveNullChar( InitOut%WriteOutputUnt(i) ) 
    END DO                   
                             
    DEALLOCATE(strHdrArray)
    DEALLOCATE(strHdrPtrs)
    DEALLOCATE(strUntArray)
    DEALLOCATE(strUntPtrs)
  END SUBROUTINE MAP_Get_Output_Headers                                                          !   -------+
  !==========================================================================================================


  ! ==========   MAP_C2F_OtherState_Array_Allocation   ======     <-----------------------------------------+
  !                                                                                              !          |
  ! MAP_OtherStateType (defined in MAP_Types.f90 and MAP_Types.h) :
  ! Done just once for other states.
  SUBROUTINE MAP_C2F_OtherState_Array_Allocation(other, ErrStat, ErrMsg) 
    ! Passed arguments
    TYPE( MAP_OtherStateType ), INTENT(INOUT)  :: other
    INTEGER(IntKi),             INTENT(INOUT)  :: ErrStat     ! Error status of the operation
    CHARACTER(*),               INTENT(INOUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None

    CALL C_F_POINTER(other%C_obj%x, other%x, (/other%C_obj%x_Len/))  
    CALL C_F_POINTER(other%C_obj%y, other%y, (/other%C_obj%y_Len/))  
    CALL C_F_POINTER(other%C_obj%z, other%z, (/other%C_obj%z_Len/))  
    CALL C_F_POINTER(other%C_obj%Fx_connect, other%Fx_connect, (/other%C_obj%Fx_connect_Len/))  
    CALL C_F_POINTER(other%C_obj%Fy_connect, other%Fy_connect, (/other%C_obj%Fy_connect_Len/))  
    CALL C_F_POINTER(other%C_obj%Fz_connect, other%Fz_connect, (/other%C_obj%Fz_connect_Len/))  
    CALL C_F_POINTER(other%C_obj%Fx_anchor, other%Fx_anchor, (/other%C_obj%Fx_anchor_Len/))  
    CALL C_F_POINTER(other%C_obj%Fy_anchor, other%Fy_anchor, (/other%C_obj%Fy_anchor_Len/))  
    CALL C_F_POINTER(other%C_obj%Fz_anchor, other%Fz_anchor, (/other%C_obj%Fz_anchor_Len/))  
    ErrStat = ErrID_None
  END SUBROUTINE MAP_C2F_OtherState_Array_Allocation                                             !   -------+
  !==========================================================================================================


  ! ==========   MAP_C2F_Output_Array_Allocation   ======     <---------------------------------------------+
  !                                                                                              !          |
  ! MAP_OutputType (defined in MAP_Types.f90 and MAP_Types.h) :
  ! Done just once for output states.
  SUBROUTINE MAP_C2F_Output_Array_Allocation(y, ErrStat, ErrMsg) 
    ! Passed arguments
    TYPE( MAP_OutputType ), INTENT(INOUT)  :: y
    INTEGER(IntKi),         INTENT(INOUT)  :: ErrStat     ! Error status of the operation
    CHARACTER(*),           INTENT(INOUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None

    CALL C_F_POINTER(y%C_obj%Fx, y%Fx, (/y%C_obj%Fx_Len/))  
    CALL C_F_POINTER(y%C_obj%Fy, y%Fy, (/y%C_obj%Fy_Len/))  
    CALL C_F_POINTER(y%C_obj%Fz, y%Fz, (/y%C_obj%Fz_Len/))  
    CALL C_F_POINTER(y%C_obj%wrtOutput, y%wrtOutput, (/y%C_obj%wrtOutput_Len/))  
    ErrStat = ErrID_None
  END SUBROUTINE MAP_C2F_Output_Array_Allocation                                                 !   -------+
  !==========================================================================================================


  ! ==========   MAP_C2F_ConstrState_Array_Allocation   ======     <----------------------------------------+
  !                                                                                              !          |
  ! MAP_ConstrStateType (defined in MAP_Types.f90 and MAP_Types.h) :
  ! Done just once for constraint states.  
  SUBROUTINE MAP_C2F_ConstrState_Array_Allocation(z, ErrStat, ErrMsg) 
    ! Passed arguments
    TYPE( MAP_ConstraintStateType ), INTENT(INOUT)  :: z
    INTEGER(IntKi),                  INTENT(INOUT)  :: ErrStat     ! Error status of the operation
    CHARACTER(*),                    INTENT(INOUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None

    CALL C_F_POINTER(z%C_obj%H, z%H, (/z%C_obj%H_Len/))  
    CALL C_F_POINTER(z%C_obj%V, z%V, (/z%C_obj%V_Len/))  
    CALL C_F_POINTER(z%C_obj%x, z%x, (/z%C_obj%x_Len/))  
    CALL C_F_POINTER(z%C_obj%y, z%y, (/z%C_obj%y_Len/))  
    CALL C_F_POINTER(z%C_obj%z, z%z, (/z%C_obj%z_Len/))  
    ErrStat = ErrID_None
  END SUBROUTINE MAP_C2F_ConstrState_Array_Allocation                                            !   -------+
  !==========================================================================================================


  ! ==========   MAP_C2F_Input_Array_Allocation   ======     <----------------------------------------------+
  !                                                                                              !          |
  ! MAP_InputType (defined in MAP_Types.f90 and MAP_Types.h) :
  ! Done just once for input states.  
  SUBROUTINE MAP_C2F_Input_Array_Allocation(u, ErrStat, ErrMsg) 
    ! Passed arguments
    TYPE( MAP_InputType ), INTENT(INOUT)  :: u
    INTEGER(IntKi),        INTENT(INOUT)  :: ErrStat     ! Error status of the operation
    CHARACTER(*),          INTENT(INOUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None

    CALL C_F_POINTER(u%C_obj%x, u%x, (/u%C_obj%x_Len/))  
    CALL C_F_POINTER(u%C_obj%y, u%y, (/u%C_obj%y_Len/))  
    CALL C_F_POINTER(u%C_obj%z, u%z, (/u%C_obj%z_Len/))  
    ErrStat = ErrID_None
  END SUBROUTINE MAP_C2F_Input_Array_Allocation                                                  !   -------+
  !==========================================================================================================


  ! ==========   copy_input_for_c   ======     <------------------------------------------------------------+
  !                                                                                              !          |
  ! We need to copy the inputs and set the pointer before entering the DLL. Referencing to the inputs
  ! change when interp/extrap routines are executed. 
  SUBROUTINE copy_inputs_for_c(u, ErrStat, ErrMsg)
    TYPE(MAP_InputType), INTENT(INOUT)  :: u
    INTEGER(IntKi),      INTENT(INOUT)  :: ErrStat     ! Error status of the operation
    CHARACTER(*),        INTENT(INOUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None

    u%c_obj%x = C_LOC(u%x(1))
    u%c_obj%x_Len = SIZE(u%x)
    u%c_obj%y = C_LOC(u%y(1))
    u%c_obj%y_Len = SIZE(u%y)
    u%c_obj%z = C_LOC(u%z(1))
    u%c_obj%z_Len = SIZE(u%z)    
  END SUBROUTINE copy_inputs_for_c                                                               !   -------+
  !==========================================================================================================


  ! ==========   deallocate_primitives_for_c   ======     <-------------------------------------------------+
  !                                                                                              !          |
  ! Delete the above stuff
  SUBROUTINE deallocate_primitives_for_c(u, ErrStat, ErrMsg)
    TYPE(MAP_InputType), INTENT(INOUT)  :: u
    INTEGER(IntKi),      INTENT(INOUT)  :: ErrStat     ! Error status of the operation
    CHARACTER(*),        INTENT(INOUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None

    u%c_obj%x = C_NULL_ptr    
    u%c_obj%y = C_NULL_ptr    
    u%c_obj%z = C_NULL_ptr    
    DEALLOCATE(u%x)
    DEALLOCATE(u%y)
    DEALLOCATE(u%z)
  END SUBROUTINE deallocate_primitives_for_c                                                     !   -------+
  !==========================================================================================================


END MODULE MAP
