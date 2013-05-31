
MODULE loadLib_defs
!BJJ: this is not tested
!based partially on http://www.stefanocottafavi.com/?p=360

   USE, INTRINSIC :: ISO_C_BINDING
   USE               NWTC_Library, ONLY: IntKi, ErrID_None, ErrID_Fatal,Num2LStr,BITS_IN_ADDR

       
   IMPLICIT NONE 
      
      ! define parameters       
   CHARACTER(*), PARAMETER :: OS_Desc = 'gfortran for Linux'
      
   TYPE DLL_Type  ! type that stores appropriate info for the dynamic library

      INTEGER(C_PTR)            :: FileAddr                                        ! The address of file FileName.         (RETURN value from dlopen )
      TYPE(C_FUNPTR)            :: ProcAddr                                        ! The address of procedure ProcName.    (RETURN value from dlSym)

      CHARACTER(1024)           :: FileName                                        ! The name of the DLL file including the full path to the current working directory.
      CHARACTER(1024)           :: ProcName                                        ! The name of the procedure in the DLL that will be called.      
      
   END TYPE DLL_Type      
   
   
   INTERFACE !linux API routines
      !bjj see http://linux.die.net/man/3/dlopen
   
      FUNCTION dlOpen(filename,mode) BIND(C,NAME="dlopen")
      ! void *dlopen(const char *filename, int mode);
         USE ISO_C_BINDING
         IMPLICIT NONE
         TYPE(C_PTR)                   :: dlOpen
         CHARACTER(C_CHAR), INTENT(IN) :: filename(*)
         INTEGER(C_INT), VALUE         :: mode
      END FUNCTION

      FUNCTION dlSym(handle,name) BIND(C,NAME="dlsym")
      ! void *dlsym(void *handle, const char *name);
         USE ISO_C_BINDING
         IMPLICIT NONE
         TYPE(C_FUNPTR)                :: dlSym ! A function pointer
         TYPE(C_PTR), VALUE            :: handle
         CHARACTER(C_CHAR), INTENT(IN) :: name(*)
      END FUNCTION

      FUNCTION dlClose(handle) BIND(C,NAME="dlclose")
      ! int dlclose(void *handle);
         USE ISO_C_BINDING
         IMPLICIT NONE
         INTEGER(C_INT)       :: dlClose
         TYPE(C_PTR), VALUE   :: handle
      END FUNCTION
  
   END INTERFACE
  
      
CONTAINS   
!==================================================================================================================================             
SUBROUTINE LoadDynamicLib ( DLL, ErrStat, ErrMsg )

      ! This SUBROUTINE is used to load the DLL.

      ! Passed Variables:

   TYPE (DLL_Type),           INTENT(INOUT)  :: DLL         ! The DLL to be loaded.
   INTEGER(IntKi),            INTENT(  OUT)  :: ErrStat     ! Error status of the operation
   CHARACTER(*),              INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None

!bjj: these aren't vetted or tested ...
   INTEGER(C_INT), PARAMETER :: RTLD_LAZY=1            ! "Perform lazy binding. Only resolve symbols as the code that references them is executed. If the symbol is never referenced, then it is never resolved. (Lazy binding is only performed for function references; references to variables are always immediately bound when the library is loaded.) "
   INTEGER(C_INT), PARAMETER :: RTLD_NOW=2             ! "If this value is specified, or the environment variable LD_BIND_NOW is set to a nonempty string, all undefined symbols in the library are resolved before dlopen() returns. If this cannot be done, an error is returned."
   INTEGER(C_INT), PARAMETER :: RTLD_GLOBAL=256        ! "The symbols defined by this library will be made available for symbol resolution of subsequently loaded libraries"
   INTEGER(C_INT), PARAMETER :: RTLD_LOCAL=0           ! "This is the converse of RTLD_GLOBAL, and the default if neither flag is specified. Symbols defined in this library are not made available to resolve references in subsequently loaded libraries."
      
      
   ErrStat = ErrID_None
   ErrMsg = ''

  
      ! Load the DLL and get the file address:

   DLL%FileAddr = dlOpen( TRIM(DLL%FileName)//C_NULL_CHAR, RTLD_LAZY )  !the "C_NULL_CHAR" converts the Fortran string to a C-type string (i.e., adds //CHAR(0) to the end)
   
   IF( .NOT. C_ASSOCIATED(DLL%FileAddr) ) THEN
      ErrStat = ErrID_Fatal
      ErrMsg  = 'The dynamic library '//TRIM(DLL%FileName)//' could not be loaded. Check that the file '// &
                'exists in the specified location and that it is compiled for '//TRIM(Num2LStr(BITS_IN_ADDR))//'-bit systems.'
      RETURN
   END IF
  

      ! Get the procedure address:

   DLL%ProcAddr = dlSym( DLL%FileAddr, TRIM(DLL%ProcName)//C_NULL_CHAR )  !the "C_NULL_CHAR" converts the Fortran string to a C-type string (i.e., adds //CHAR(0) to the end)

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
   INTEGER(C_INT)                            :: Success     ! Whether or not the call to dlClose was successful
   INTEGER(C_INT), PARAMETER                 :: TRUE  = 0

      ! Close the library:

   Success = dlClose( DLL%FileAddr ) !The function dlclose() returns 0 on success, and nonzero on error. 
      
   IF ( Success /= TRUE ) THEN !bjj: note that this is not the same as LOGICAL .TRUE.
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
