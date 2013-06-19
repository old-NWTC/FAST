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

   USE, INTRINSIC :: ISO_C_BINDING
   USE               NWTC_Library, ONLY: IntKi, ErrID_None, ErrID_Fatal,Num2LStr,BITS_IN_ADDR
   
   IMPLICIT NONE 
         
   TYPE DLL_Type

      INTEGER(C_INTPTR_T)       :: FileAddr                                        ! The address of file FileName.         (RETURN value from LoadLibrary )
      TYPE(C_FUNPTR)            :: ProcAddr                                        ! The address of procedure ProcName.    (RETURN value from GetProcAddress)

      CHARACTER(1024)           :: FileName                                        ! The name of the DLL file including the full path to the current working directory.
      CHARACTER(1024)           :: ProcName                                        ! The name of the procedure in the DLL that will be called.      
      
   END TYPE DLL_Type   
      
!...........................
!bjj: I have been unable to find a solution that works with both IVF and gfortran...  
!bjj: note that "Intel Fortran does not support use of STDCALL with BIND(C) at this time"
!     See this link: http://software.intel.com/en-us/articles/replacing-intel-fortran-attributes-with-c-interoperability-features   
!bjj: Until this is fixed, Intel uses kernel32.f90 definitions instead of the interface below:
!...........................
   
      
CONTAINS   
!==================================================================================================================================             
SUBROUTINE LoadDynamicLib ( DLL, ErrStat, ErrMsg )

      ! This SUBROUTINE is used to load the DLL.

      ! Passed Variables:

   TYPE (DLL_Type),           INTENT(INOUT)  :: DLL         ! The DLL to be loaded.
   INTEGER(IntKi),            INTENT(  OUT)  :: ErrStat     ! Error status of the operation
   CHARACTER(*),              INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None

   INTERFACE  ! Definitions of Windows API routines
   
      FUNCTION LoadLibrary(lpFileName) BIND(C,NAME='LoadLibraryA')
         !DEC$ ATTRIBUTES STDCALL :: LoadLibrary
         USE, INTRINSIC :: ISO_C_BINDING
         IMPLICIT NONE
         !GCC$ ATTRIBUTES STDCALL :: LoadLibrary
         INTEGER(C_INTPTR_T)        :: LoadLibrary 
         CHARACTER(KIND=C_CHAR)     :: lpFileName(*) 
      END FUNCTION LoadLibrary 

      FUNCTION GetProcAddress(hModule, lpProcName) BIND(C, NAME='GetProcAddress')
         !DEC$ ATTRIBUTES STDCALL :: GetProcAddress
         USE, INTRINSIC :: ISO_C_BINDING
         IMPLICIT NONE
         !GCC$ ATTRIBUTES STDCALL :: GetProcAddress
         TYPE(C_FUNPTR)             :: GetProcAddress       
         INTEGER(C_INTPTR_T),VALUE  :: hModule
         CHARACTER(KIND=C_CHAR)     :: lpProcName(*)
      END FUNCTION GetProcAddress      
      
   END INTERFACE    
   
   
   
   ErrStat = ErrID_None
   ErrMsg = ''

   
      ! Load the DLL and get the file address:

   DLL%FileAddr = LoadLibrary( TRIM(DLL%FileName)//C_NULL_CHAR )  !the "C_NULL_CHAR" converts the Fortran string to a C-type string (i.e., adds //CHAR(0) to the end)
   IF ( DLL%FileAddr == INT(0,C_INTPTR_T) ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = 'The dynamic library '//TRIM(DLL%FileName)//' could not be loaded. Check that the file '// &
                'exists in the specified location and that it is compiled for '//TRIM(Num2LStr(BITS_IN_ADDR))//'-bit systems.'
      RETURN
   END IF
  

      ! Get the procedure address:

   DLL%ProcAddr = GetProcAddress( DLL%FileAddr, TRIM(DLL%ProcName)//C_NULL_CHAR )  !the "C_NULL_CHAR" converts the Fortran string to a C-type string (i.e., adds //CHAR(0) to the end)

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
   INTEGER(C_INT)                            :: Success     ! Whether or not the call to FreeLibrary was successful
   INTEGER(C_INT), PARAMETER                 :: FALSE  = 0

   INTERFACE  ! Definitions of Windows API routines
   
      FUNCTION FreeLibrary(hLibModule) BIND(C, NAME='FreeLibrary')
         !DEC$ ATTRIBUTES STDCALL :: FreeLibrary
         USE, INTRINSIC :: ISO_C_BINDING
         IMPLICIT NONE
         !GCC$ ATTRIBUTES STDCALL :: FreeLibrary
         INTEGER(C_INT)             :: FreeLibrary ! BOOL
         INTEGER(C_INTPTR_T),VALUE  :: hLibModule ! HMODULE hLibModule
      END FUNCTION    
      
   END INTERFACE    
   
   
      ! Free the DLL:

   Success = FreeLibrary( DLL%FileAddr ) !If the function succeeds, the return value is nonzero. If the function fails, the return value is zero. 
      
   IF ( Success == FALSE ) THEN !BJJ: note that this isn't the same as the Fortran LOGICAL .FALSE.
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
   
   
END MODULE loadLib_defs
