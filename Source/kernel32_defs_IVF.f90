MODULE loadLib_defs

   USE               IFWINTY,  ONLY : BOOL, HANDLE, LPVOID, TRUE, POINTER_LEN
   USE               kernel32, ONLY : LoadLibrary, FreeLibrary, GetProcAddress
   USE, INTRINSIC :: ISO_C_BINDING, ONLY:  C_FUNPTR, C_INTPTR_T, C_CHAR

   
   
!   integer, parameter :: POINTER_LEN = INT_PTR_KIND() ! 4 for WIN32 systems
!                                                      ! 8 for WIN64 systems  
!   integer, parameter :: TRUE = 1
!   integer, parameter :: LPVOID = POINTER_LEN   ! INTEGER(4)
!   integer, parameter :: HANDLE = POINTER_LEN   ! INTEGER(4/8)
!   integer, parameter :: BOOL = 4             ! INTEGER(4)

!..........................................
! values from IVF's kernel32.f90:
!
!INTERFACE 
!FUNCTION LoadLibrary( lpLibFileName)
!import
!  integer(HANDLE) :: LoadLibrary ! HMODULE
!    !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'LoadLibraryA' :: LoadLibrary
!!DEC$ ATTRIBUTES REFERENCE, ALLOW_NULL :: lpLibFileName
!  character*(*) lpLibFileName ! LPCSTR lpLibFileName
! END FUNCTION
!END INTERFACE
!
!INTERFACE 
!FUNCTION FreeLibrary( &
!        hLibModule)
!import
!  integer(BOOL) :: FreeLibrary ! BOOL
!    !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'FreeLibrary' :: FreeLibrary
!  integer(HANDLE) hLibModule ! HMODULE hLibModule
! END FUNCTION
!END INTERFACE    
!    
!INTERFACE GetProcAddress
!FUNCTION GetProcAddress_G1( &
!        hModule, &
!        lpProcName)
!import
!    integer(LPVOID) :: GetProcAddress_G1 ! FARPROC
!    !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'GetProcAddress' :: GetProcAddress_G1
!  integer(HANDLE) hModule ! HMODULE hModule
!!DEC$ ATTRIBUTES REFERENCE, ALLOW_NULL :: lpProcName
!  character*(*) lpProcName ! LPCSTR lpProcName
! END FUNCTION


!FUNCTION GetProcAddress_G2( &
!        hModule, &
!        lpProcName)
!import
!    integer(LPVOID) :: GetProcAddress_G2 ! FARPROC
!    !DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'GetProcAddress' :: GetProcAddress_G2
!  integer(HANDLE) hModule ! HMODULE hModule
!  integer(LPVOID) lpProcName ! LPCSTR lpProcName
! END FUNCTION
!END INTERFACE   
   
END MODULE loadLib_defs