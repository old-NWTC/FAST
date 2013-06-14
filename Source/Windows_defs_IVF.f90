!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2013  National Renewable Energy Laboratory
!
!    This file is part of FAST's Controls and Electrical Drive Module, "ServoDyn".
!
!    ServoDyn is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as
!    published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
!    of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License along with ServoDyn.
!    If not, see <http://www.gnu.org/licenses/>.
!
!**********************************************************************************************************************************
MODULE loadLib_defs

   USE               IFWINTY,  ONLY : BOOL, HANDLE, LPVOID, FALSE, POINTER_LEN
   USE               kernel32, ONLY : LoadLibrary, FreeLibrary, GetProcAddress
   USE               NWTC_Library, ONLY: IntKi, ErrID_None, ErrID_Fatal,Num2LStr,BITS_IN_ADDR
   
   USE, INTRINSIC :: ISO_C_BINDING

   CHARACTER(*), PARAMETER :: OS_Desc = 'IVF for Windows'
   
   
   TYPE DLL_Type

      INTEGER(C_INTPTR_T)       :: FileAddr                                        ! The address of file FileName.         (RETURN value from LoadLibrary )
      TYPE(C_FUNPTR)            :: ProcAddr                                        ! The address of procedure ProcName.    (RETURN value from GetProcAddress)

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


      ! local variables
   INTEGER(HANDLE)                           :: FileAddr    ! The address of file FileName.         (RETURN value from LoadLibrary in kernel32.f90)
   INTEGER(LPVOID)                           :: ProcAddr    ! The address of procedure ProcName.    (RETURN value from GetProcAddress in kernel32.f90)

   
   ErrStat = ErrID_None
   ErrMsg = ''

   
      ! Load the DLL and get the file address:

   FileAddr = LoadLibrary( TRIM(DLL%FileName)//C_NULL_CHAR )  !the "C_NULL_CHAR" converts the Fortran string to a C-type string (i.e., adds //CHAR(0) to the end)   
   DLL%FileAddr = TRANSFER(FileAddr, DLL%FileAddr)             !convert INTEGER(HANDLE) to INTEGER(C_INTPTR_T) [used only for compatibility with gfortran]
   
   IF ( DLL%FileAddr == INT(0,C_INTPTR_T) ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = 'The dynamic library '//TRIM(DLL%FileName)//' could not be loaded. Check that the file '// &
                'exists in the specified location and that it is compiled for '//TRIM(Num2LStr(BITS_IN_ADDR))//'-bit systems.'
      RETURN
   END IF
  

      ! Get the procedure address:

   ProcAddr = GetProcAddress( DLL%FileAddr, TRIM(DLL%ProcName)//C_NULL_CHAR )  !the "C_NULL_CHAR" converts the Fortran string to a C-type string (i.e., adds //CHAR(0) to the end)
   DLL%ProcAddr = TRANSFER(ProcAddr, DLL%ProcAddr)  !convert INTEGER(LPVOID) to INTEGER(C_FUNPTR) [used only for compatibility with gfortran]
     
   IF(.NOT. C_ASSOCIATED(DLL%ProcAddr)) THEN
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
   INTEGER(HANDLE)                           :: FileAddr    ! The address of file FileName.  (RETURN value from LoadLibrary in kernel32.f90)
   INTEGER(BOOL)                             :: Success     ! Whether or not the call to FreeLibrary was successful

   
   
   FileAddr = TRANSFER(DLL%FileAddr, FileAddr) !convert INTEGER(C_INTPTR_T) to INTEGER(HANDLE) [used only for compatibility with gfortran]
   
      ! Free the DLL:

   Success = FreeLibrary( FileAddr ) !If the function succeeds, the return value is nonzero. If the function fails, the return value is zero. 
      
   IF ( Success == FALSE ) THEN !BJJ: note that this is the Windows BOOL type so FALSE isn't the same as the Fortran LOGICAL .FALSE.
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
