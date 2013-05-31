MODULE loadLib_defs

   USE               IFWINTY,  ONLY : BOOL, HANDLE, LPVOID, FALSE, POINTER_LEN
   USE               kernel32, ONLY : LoadLibrary, FreeLibrary, GetProcAddress
   USE               NWTC_Library, ONLY: IntKi, ErrID_None, ErrID_Fatal,Num2LStr,BITS_IN_ADDR
   
   USE, INTRINSIC :: ISO_C_BINDING

   CHARACTER(*), PARAMETER :: OS_Desc = 'IVF for Windows'
   
   TYPE DLL_Type
      
      INTEGER(HANDLE)           :: FileAddr                                        ! The address of file FileName.         (RETURN value from LoadLibrary in kernel32.f90)
      INTEGER(LPVOID)           :: ProcAddr                                        ! The address of procedure ProcName.    (RETURN value from GetProcAddress in kernel32.f90)
            
      CHARACTER(1024)           :: FileName                                        ! The name of the DLL file including the full path to the current working directory.
      CHARACTER(1024)           :: ProcName                                        ! The name of the procedure in the DLL that will be called.      
      
   END TYPE DLL_Type   
   
   
CONTAINS
!==================================================================================================================================      
SUBROUTINE LoadDynamicLib ( DLL, ErrStat, ErrMsg )

      ! This SUBROUTINE is used to load the DLL.

      ! Passed Variables:

   TYPE (DLL_Type),           INTENT(INOUT)  :: DLL         ! The DLL to be loaded.
   INTEGER(IntKi),            INTENT(  OUT)  :: ErrStat     ! Error status of the operation
   CHARACTER(*),              INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None

   
   ErrStat = ErrID_None
   ErrMsg = ''

   
      ! Load the DLL and get the file address:

   DLL%FileAddr = LoadLibrary( TRIM(DLL%FileName)//C_NULL_CHAR )  !the "C_NULL_CHAR" converts the Fortran string to a C-type string (i.e., adds //CHAR(0) to the end)
   IF ( DLL%FileAddr == INT(0,HANDLE) ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = 'The dynamic library '//TRIM(DLL%FileName)//' could not be loaded. Check that the file '// &
                'exists in the specified location and that it is compiled for '//TRIM(Num2LStr(BITS_IN_ADDR))//'-bit systems.'
      RETURN
   END IF
  

      ! Get the procedure address:

   DLL%ProcAddr = GetProcAddress( DLL%FileAddr, TRIM(DLL%ProcName)//C_NULL_CHAR )  !the "C_NULL_CHAR" converts the Fortran string to a C-type string (i.e., adds //CHAR(0) to the end)

   IF ( DLL%ProcAddr == INT(0,C_INTPTR_T) ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = 'The procedure '//TRIM(DLL%ProcName)//' in file '//TRIM(DLL%FileName)//' could not be loaded.'
      RETURN
   END IF
     
   
   RETURN
END SUBROUTINE LoadDynamicLib
!==================================================================================================================================
SUBROUTINE FreeDynamicLib ( DLL, ErrStat, ErrMsg )

      ! This SUBROUTINE is used to free the DLL.

      ! Passed Variables:

   TYPE (DLL_Type),           INTENT(INOUT)  :: DLL         ! The DLL to be freed.
   INTEGER(IntKi),            INTENT(  OUT)  :: ErrStat     ! Error status of the operation
   CHARACTER(*),              INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None

      ! Local variable: 
   INTEGER(BOOL)                             :: Success     ! Whether or not the call to FreeLibrary was successful
   
      ! Free the DLL:

   Success = FreeLibrary( DLL%FileAddr ) !If the function succeeds, the return value is nonzero. If the function fails, the return value is zero. 
      
   IF ( Success == FALSE ) THEN !BJJ: note that this is the Windows BOOL type so TRUE isn't the same as the Fortran LOGICAL .FALSE.
      ErrStat = ErrID_Fatal
      ErrMsg  = 'The dynamic library could not be freed.'
      RETURN
   ELSE
      ErrStat = ErrID_None
      ErrMsg = ''         
   END IF

   RETURN
END SUBROUTINE FreeDynamicLib
!==================================================================================================================================   
   
   
   
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
