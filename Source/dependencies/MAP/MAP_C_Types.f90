!!STARTOFREGISTRYGENERATEDFILE 'MAP_C_Types.f90'
!!
!! WARNING This file is generated automatically by the FAST registry
!! Do not edit.  Your changes to this file will be lost.
!!
MODULE MAP_C_Types

  USE , INTRINSIC :: ISO_C_Binding
  IMPLICIT NONE

  PRIVATE

  !==========   MAP C++ Object Pointers   ===
  ! Fortran types that will be binded with C++. 
  ! These types are pointers to C++ objects.    
  !                                             
  ! Initialization Input States                 
  TYPE , BIND(C) :: MAP_InitInput_C         
     PRIVATE                                    
     TYPE(C_ptr) :: object = C_NULL_ptr         
  END TYPE MAP_InitInput_C                  
                                                
  ! Other States                                
  TYPE , BIND(C) :: MAP_OtherState_C        
     PRIVATE                                    
     TYPE(C_ptr) :: object = C_NULL_ptr         
  END TYPE MAP_OtherState_C                 
  !==============================================


  !==========   MAP Object Constructor/Destructor   ==========
  !                                                               
  INTERFACE ! BEGIN: Interface to external C functions            
                                                                  
     !==========   MAP C++ Object Constructor/Destructor   ===
     !                                                            
     ! Initalize Initialization Input object                      
     FUNCTION C_Create_MAP_InitInput(msg,err) RESULT( this ) &
          !NAME = the C function called inside ' extern "C" '   
          BIND( C , NAME="MAP_InitInput_Create" )           
       IMPORT                                                     
       TYPE(C_ptr) :: this                                        
       CHARACTER(KIND=C_CHAR), DIMENSION(1024) :: msg             
       INTEGER(KIND=C_INT) :: err                                 
     END FUNCTION C_Create_MAP_InitInput                      
                                                                  
     ! Delete input object                                        
     SUBROUTINE C_Delete_MAP_InitInput( this ) &              
          !NAME = the C function called inside ' extern "C" '   
          BIND( C , NAME="MAP_InitInput_Delete" )           
       IMPORT                                                     
       TYPE(C_ptr), VALUE :: this                                 
     END SUBROUTINE C_Delete_MAP_InitInput                    
     !============================================================
                                                                  
                                                                  
     !==========   MAP C++ Object Constructor/Destructor   ===
     !                                                            
     ! Initalize input object                                     
     FUNCTION C_Create_MAP_Other(msg,err) RESULT( this ) &    
          !NAME = the C function called inside ' extern "C" '   
          BIND( C , NAME="MAP_OtherState_Create" )          
       IMPORT                                                     
       TYPE(C_ptr) :: this                                        
       CHARACTER(KIND=C_CHAR), DIMENSION(1024) :: msg             
       INTEGER(KIND=C_INT) :: err                                 
     END FUNCTION C_Create_MAP_Other                          
                                                                  
     ! Delete input object                                        
     SUBROUTINE C_Delete_MAP_Other( this ) &                  
          !NAME = the C function called inside ' extern "C" '   
          BIND( C , NAME="MAP_OtherState_Delete" )          
       IMPORT                                                     
       TYPE(C_ptr), VALUE :: this                                 
     END SUBROUTINE C_Delete_MAP_Other                        
     !============================================================
  END INTERFACE ! END: Interface to external C functions          
  !===============================================================


  !==========   MAP C++ Object Interface   ========
  !
  ! Input initalize interface                         
  INTERFACE MAP_InitInput_Initialize              
     MODULE PROCEDURE MAP_InitInput_Create        
  END INTERFACE MAP_InitInput_Initialize          
                                                      
  ! Input destructor interface                        
  INTERFACE MAP_InitInput_Destroy                 
     MODULE PROCEDURE MAP_InitInput_Delete        
  END INTERFACE MAP_InitInput_Destroy             
                                                      
                                                      
  ! Input initalize interface                         
  INTERFACE MAP_Other_Initialize                  
     MODULE PROCEDURE MAP_OtherState_Create       
  END INTERFACE MAP_Other_Initialize              
  ! Input Destructor interface                        
  INTERFACE MAP_Other_Destroy                     
     MODULE PROCEDURE MAP_Other_Delete            
  END INTERFACE MAP_Other_Destroy                 
  !====================================================


  PUBLIC :: MAP_InitInput_C    , &
       MAP_OtherState_C        , &
       MAP_InitInput_Initialize, & 
       MAP_Other_Initialize    , &
       MAP_InitInput_Destroy   , &
       MAP_Other_Destroy        

CONTAINS

  !==========   MAP C++ Object Interface   ====== 
  !
  ! Initialization Input type construction
  SUBROUTINE MAP_InitInput_Create( this,msg,err )
    TYPE( MAP_InitInput_C ), INTENT( OUT ) :: this
    CHARACTER(KIND=C_CHAR), DIMENSION(1024) :: msg
    INTEGER(KIND=C_INT) :: err
    this%object = C_Create_MAP_InitInput(msg,err)
  END SUBROUTINE MAP_InitInput_Create
  ! Initlialization Input type destruction
  SUBROUTINE MAP_InitInput_Delete(this)
    TYPE( MAP_InitInput_C ), INTENT(INOUT) :: this
    CALL C_Delete_MAP_InitInput( this%object )
    this%object = C_NULL_ptr
  END SUBROUTINE MAP_InitInput_Delete

  ! Other type initialization
  SUBROUTINE MAP_OtherState_Create( this,msg,err )
    TYPE( MAP_OtherState_C ), INTENT( OUT ) :: this
    CHARACTER(KIND=C_CHAR), DIMENSION(1024) :: msg
    INTEGER(KIND=C_INT) :: err
    this%object = C_Create_MAP_Other(msg,err)
  END SUBROUTINE MAP_OtherState_Create
  ! Other type destruction
  SUBROUTINE MAP_Other_Delete(this)
    TYPE( MAP_OtherState_C ), INTENT(INOUT) :: this
    CALL C_Delete_MAP_Other( this%object )
    this%object = C_NULL_ptr
  END SUBROUTINE MAP_Other_Delete
  !=======================================================

END MODULE MAP_C_Types
