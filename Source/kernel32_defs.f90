MODULE loadLib_defs

   USE               Precision   !from NWTC Library
   USE, INTRINSIC :: ISO_C_BINDING, ONLY:  C_FUNPTR, C_INTPTR_T, C_CHAR
   
   IMPLICIT NONE 

   
      ! define parameters
   INTEGER, PARAMETER :: POINTER_LEN = C_INTPTR_T
   INTEGER, PARAMETER :: TRUE   = 1
   INTEGER, PARAMETER :: LPVOID = POINTER_LEN   ! INTEGER(4)
   INTEGER, PARAMETER :: HANDLE = POINTER_LEN   ! INTEGER(4/8)
   INTEGER, PARAMETER :: BOOL   = B1Ki          ! INTEGER(4)
       

   INTERFACE 
      FUNCTION LoadLibrary(lpFileName) BIND(C,NAME='LoadLibraryA')
         IMPORT
         CHARACTER(KIND=C_CHAR) :: lpFileName(*) 
         !GCC$ ATTRIBUTES STDCALL :: LoadLibrary 
         INTEGER(C_INTPTR_T) :: LoadLibrary 
      END FUNCTION LoadLibrary 
   END INTERFACE

   INTERFACE 
      FUNCTION GetProcAddress(hModule, lpProcName) BIND(C, NAME='GetProcAddress')
         IMPORT
         IMPLICIT NONE
         !GCC$ ATTRIBUTES STDCALL :: GetProcAddress
         INTEGER(LPVOID)        :: GetProcAddress
!BJJ: ABOVE WAS TYPE(C_FUNPTR) :: GetProcAddress         
         INTEGER(HANDLE), VALUE :: hModule
         CHARACTER(KIND=C_CHAR) :: lpProcName(*)
      END FUNCTION GetProcAddress      
   END INTERFACE

   INTERFACE 
      FUNCTION FreeLibrary(hLibModule) BIND(C, NAME='FreeLibrary')
         import  !get the definition of BOOL from above
         IMPLICIT NONE
         INTEGER(BOOL)   :: FreeLibrary ! BOOL
         !GCC$ ATTRIBUTES STDCALL :: FreeLibrary
         INTEGER(HANDLE) :: hLibModule ! HMODULE hLibModule
      END FUNCTION
   END INTERFACE    
       
   
END MODULE loadLib_defs