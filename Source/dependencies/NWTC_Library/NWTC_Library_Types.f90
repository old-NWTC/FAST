!STARTOFREGISTRYGENERATEDFILE './NWTC_Library_Types.f90'
!jm JM 20130819
!jm This add types and routines that were generated by running the registry on Registry_NWTC_Library.txt 
!jm with usefroms changed to typedef on a one-time-only basis and then copied into this routine.
!jm If the types change, a new version of this file will need to be regenerated.
!jm Notes
!jm 1. "USE NWTC_Libary" is manually commented out, below, and replaced with USE SysSubs
!jm to avoid a circular USE association with NWTC_IO, which uses this module.
!jm 2. Every routine after NWTC_Library_UnPackfileinfotype is commented out (or you can remove the lines)
!**********************************************************************************************************************************
! File last committed: $Date: 2013-09-21 22:37:32 -0600 (Sat, 21 Sep 2013) $
! (File) Revision #: $Rev: 175 $
! URL: $HeadURL: https://windsvn.nrel.gov/NWTC_Library/trunk/source/NWTC_Library_Types.f90 $
!**********************************************************************************************************************************

!
!*********************************************************************************************************************************
! NWTC_Library_Types
!.................................................................................................................................
! LICENSING
! Copyright (C) 2012, 2013 National Renewable Energy Laboratory
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!     http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.
!
!
! W A R N I N G : This file was automatically generated from the FAST registry.  Changes made to this file may be lost.
!
!*********************************************************************************************************************************
MODULE NWTC_Library_Types
! This module contains all of the user-defined types needed in NWTC_Library. It also contains copy, destroy, pack, and
! unpack routines associated with each defined data type. This code will be generated by the FAST Registry.
!---------------------------------------------------------------------------------------------------------------------------------
!jm USE NWTC_Library
USE SysSubs ! see comment at the top of this routine
IMPLICIT NONE
! =========  ProgDesc  =======
  TYPE, PUBLIC :: ProgDesc
    CHARACTER(99)  :: Name 
    CHARACTER(99)  :: Ver 
    CHARACTER(24)  :: Date 
  END TYPE ProgDesc
! =======================
! =========  FASTdataType  =======
  TYPE, PUBLIC :: FASTdataType
    CHARACTER(1024)  :: File 
    CHARACTER(1024)  :: Descr 
    INTEGER(IntKi)  :: NumChans 
    INTEGER(IntKi)  :: NumRecs 
    REAL(DbKi)  :: TimeStep 
    CHARACTER(20) , DIMENSION(:), ALLOCATABLE  :: ChanNames 
    CHARACTER(20) , DIMENSION(:), ALLOCATABLE  :: ChanUnits 
    REAL(ReKi) , DIMENSION(:,:), ALLOCATABLE  :: Data 
  END TYPE FASTdataType
! =======================
! =========  OutParmType  =======
  TYPE, PUBLIC :: OutParmType
    INTEGER(IntKi)  :: Indx 
    CHARACTER(10)  :: Name 
    CHARACTER(10)  :: Units 
    INTEGER(IntKi)  :: SignM 
  END TYPE OutParmType
! =======================
! =========  FileInfoType  =======
  TYPE, PUBLIC :: FileInfoType
    INTEGER(IntKi)  :: NumLines 
    INTEGER(IntKi)  :: NumFiles                      ! The total number of files recursively read.

    INTEGER(IntKi) , DIMENSION(:), ALLOCATABLE  :: FileLine 
    INTEGER(IntKi) , DIMENSION(:), ALLOCATABLE  :: FileIndx
    CHARACTER(1024), ALLOCATABLE                :: FileList  (:)  ! The list of the unique files whose contents are stored in this structure.
    CHARACTER(512) , DIMENSION(:), ALLOCATABLE  :: Lines 
  END TYPE FileInfoType
! =======================
CONTAINS
 SUBROUTINE NWTC_Library_Copyprogdesc( SrcprogdescData, DstprogdescData, CtrlCode, ErrStat, ErrMsg )
  TYPE(progdesc), INTENT(INOUT) :: SrcprogdescData
  TYPE(progdesc), INTENT(INOUT) :: DstprogdescData
  INTEGER(IntKi),  INTENT(IN   ) :: CtrlCode
  INTEGER(IntKi),  INTENT(  OUT) :: ErrStat
  CHARACTER(*),    INTENT(  OUT) :: ErrMsg
! Local 
  INTEGER(IntKi)                 :: i,i1,i2,i3,i4,i5,j,k
! 
  ErrStat = ErrID_None
  ErrMsg  = ""
  DstprogdescData%Name = SrcprogdescData%Name
  DstprogdescData%Ver = SrcprogdescData%Ver
  DstprogdescData%Date = SrcprogdescData%Date
 END SUBROUTINE NWTC_Library_Copyprogdesc

 SUBROUTINE NWTC_Library_Destroyprogdesc( progdescData, ErrStat, ErrMsg )
  TYPE(progdesc), INTENT(INOUT) :: progdescData
  INTEGER(IntKi),  INTENT(  OUT) :: ErrStat
  CHARACTER(*),    INTENT(  OUT) :: ErrMsg
  INTEGER(IntKi)                 :: i, i1, i2, i3, i4, i5 
! 
  ErrStat = ErrID_None
  ErrMsg  = ""
 END SUBROUTINE NWTC_Library_Destroyprogdesc

 SUBROUTINE NWTC_Library_Packprogdesc( ReKiBuf, DbKiBuf, IntKiBuf, Indata, ErrStat, ErrMsg, SizeOnly )
  REAL(ReKi),       ALLOCATABLE, INTENT(  OUT) :: ReKiBuf(:)
  REAL(DbKi),       ALLOCATABLE, INTENT(  OUT) :: DbKiBuf(:)
  INTEGER(IntKi),   ALLOCATABLE, INTENT(  OUT) :: IntKiBuf(:)
  TYPE(progdesc),  INTENT(INOUT) :: InData
  INTEGER(IntKi),   INTENT(  OUT) :: ErrStat
  CHARACTER(*),     INTENT(  OUT) :: ErrMsg
  LOGICAL,OPTIONAL, INTENT(IN   ) :: SizeOnly
    ! Local variables
  INTEGER(IntKi)                 :: Re_BufSz
  INTEGER(IntKi)                 :: Re_Xferred
  INTEGER(IntKi)                 :: Re_CurrSz
  INTEGER(IntKi)                 :: Db_BufSz
  INTEGER(IntKi)                 :: Db_Xferred
  INTEGER(IntKi)                 :: Db_CurrSz
  INTEGER(IntKi)                 :: Int_BufSz
  INTEGER(IntKi)                 :: Int_Xferred
  INTEGER(IntKi)                 :: Int_CurrSz
  INTEGER(IntKi)                 :: i,i1,i2,i3,i4,i5     
  LOGICAL                        :: OnlySize ! if present and true, do not pack, just allocate buffers
 ! buffers to store meshes, if any
  OnlySize = .FALSE.
  IF ( PRESENT(SizeOnly) ) THEN
    OnlySize = SizeOnly
  ENDIF
    !
  ErrStat = ErrID_None
  ErrMsg  = ""
  Re_Xferred  = 1
  Db_Xferred  = 1
  Int_Xferred  = 1
  Re_BufSz  = 0
  Db_BufSz  = 0
  Int_BufSz  = 0
  IF ( Re_BufSz  .GT. 0 ) ALLOCATE( ReKiBuf(  Re_BufSz  ) )
  IF ( Db_BufSz  .GT. 0 ) ALLOCATE( DbKiBuf(  Db_BufSz  ) )
  IF ( Int_BufSz .GT. 0 ) ALLOCATE( IntKiBuf( Int_BufSz ) )
 END SUBROUTINE NWTC_Library_Packprogdesc

 SUBROUTINE NWTC_Library_UnPackprogdesc( ReKiBuf, DbKiBuf, IntKiBuf, Outdata, ErrStat, ErrMsg )
  REAL(ReKi),      ALLOCATABLE, INTENT(IN   ) :: ReKiBuf(:)
  REAL(DbKi),      ALLOCATABLE, INTENT(IN   ) :: DbKiBuf(:)
  INTEGER(IntKi),  ALLOCATABLE, INTENT(IN   ) :: IntKiBuf(:)
  TYPE(progdesc), INTENT(INOUT) :: OutData
  INTEGER(IntKi),  INTENT(  OUT) :: ErrStat
  CHARACTER(*),    INTENT(  OUT) :: ErrMsg
    ! Local variables
  INTEGER(IntKi)                 :: Re_BufSz
  INTEGER(IntKi)                 :: Re_Xferred
  INTEGER(IntKi)                 :: Re_CurrSz
  INTEGER(IntKi)                 :: Db_BufSz
  INTEGER(IntKi)                 :: Db_Xferred
  INTEGER(IntKi)                 :: Db_CurrSz
  INTEGER(IntKi)                 :: Int_BufSz
  INTEGER(IntKi)                 :: Int_Xferred
  INTEGER(IntKi)                 :: Int_CurrSz
  INTEGER(IntKi)                 :: i, i1, i2, i3, i4, i5
  LOGICAL, ALLOCATABLE           :: mask1(:)
  LOGICAL, ALLOCATABLE           :: mask2(:,:)
  LOGICAL, ALLOCATABLE           :: mask3(:,:,:)
  LOGICAL, ALLOCATABLE           :: mask4(:,:,:,:)
  LOGICAL, ALLOCATABLE           :: mask5(:,:,:,:,:)
 ! buffers to store meshes, if any
    !
  ErrStat = ErrID_None
  ErrMsg  = ""
  Re_Xferred  = 1
  Db_Xferred  = 1
  Int_Xferred  = 1
  Re_BufSz  = 0
  Db_BufSz  = 0
  Int_BufSz  = 0
  Re_Xferred   = Re_Xferred-1
  Db_Xferred   = Db_Xferred-1
  Int_Xferred  = Int_Xferred-1
 END SUBROUTINE NWTC_Library_UnPackprogdesc

 SUBROUTINE NWTC_Library_Copyfastdatatype( SrcfastdatatypeData, DstfastdatatypeData, CtrlCode, ErrStat, ErrMsg )
  TYPE(fastdatatype), INTENT(INOUT) :: SrcfastdatatypeData
  TYPE(fastdatatype), INTENT(INOUT) :: DstfastdatatypeData
  INTEGER(IntKi),  INTENT(IN   ) :: CtrlCode
  INTEGER(IntKi),  INTENT(  OUT) :: ErrStat
  CHARACTER(*),    INTENT(  OUT) :: ErrMsg
! Local 
  INTEGER(IntKi)                 :: i,i1,i2,i3,i4,i5,j,k
! 
  ErrStat = ErrID_None
  ErrMsg  = ""
  DstfastdatatypeData%File = SrcfastdatatypeData%File
  DstfastdatatypeData%Descr = SrcfastdatatypeData%Descr
  DstfastdatatypeData%NumChans = SrcfastdatatypeData%NumChans
  DstfastdatatypeData%NumRecs = SrcfastdatatypeData%NumRecs
  DstfastdatatypeData%TimeStep = SrcfastdatatypeData%TimeStep
IF ( ALLOCATED( SrcfastdatatypeData%ChanNames ) ) THEN
  i1 = SIZE(SrcfastdatatypeData%ChanNames,1)
  IF (.NOT.ALLOCATED(DstfastdatatypeData%ChanNames)) ALLOCATE(DstfastdatatypeData%ChanNames(i1))
  DstfastdatatypeData%ChanNames = SrcfastdatatypeData%ChanNames
ENDIF
IF ( ALLOCATED( SrcfastdatatypeData%ChanUnits ) ) THEN
  i1 = SIZE(SrcfastdatatypeData%ChanUnits,1)
  IF (.NOT.ALLOCATED(DstfastdatatypeData%ChanUnits)) ALLOCATE(DstfastdatatypeData%ChanUnits(i1))
  DstfastdatatypeData%ChanUnits = SrcfastdatatypeData%ChanUnits
ENDIF
IF ( ALLOCATED( SrcfastdatatypeData%Data ) ) THEN
  i1 = SIZE(SrcfastdatatypeData%Data,1)
  i2 = SIZE(SrcfastdatatypeData%Data,2)
  IF (.NOT.ALLOCATED(DstfastdatatypeData%Data)) ALLOCATE(DstfastdatatypeData%Data(i1,i2))
  DstfastdatatypeData%Data = SrcfastdatatypeData%Data
ENDIF
 END SUBROUTINE NWTC_Library_Copyfastdatatype

 SUBROUTINE NWTC_Library_Destroyfastdatatype( fastdatatypeData, ErrStat, ErrMsg )
  TYPE(fastdatatype), INTENT(INOUT) :: fastdatatypeData
  INTEGER(IntKi),  INTENT(  OUT) :: ErrStat
  CHARACTER(*),    INTENT(  OUT) :: ErrMsg
  INTEGER(IntKi)                 :: i, i1, i2, i3, i4, i5 
! 
  ErrStat = ErrID_None
  ErrMsg  = ""
  IF ( ALLOCATED(fastdatatypeData%ChanNames) ) DEALLOCATE(fastdatatypeData%ChanNames)
  IF ( ALLOCATED(fastdatatypeData%ChanUnits) ) DEALLOCATE(fastdatatypeData%ChanUnits)
  IF ( ALLOCATED(fastdatatypeData%Data) ) DEALLOCATE(fastdatatypeData%Data)
 END SUBROUTINE NWTC_Library_Destroyfastdatatype

 SUBROUTINE NWTC_Library_Packfastdatatype( ReKiBuf, DbKiBuf, IntKiBuf, Indata, ErrStat, ErrMsg, SizeOnly )
  REAL(ReKi),       ALLOCATABLE, INTENT(  OUT) :: ReKiBuf(:)
  REAL(DbKi),       ALLOCATABLE, INTENT(  OUT) :: DbKiBuf(:)
  INTEGER(IntKi),   ALLOCATABLE, INTENT(  OUT) :: IntKiBuf(:)
  TYPE(fastdatatype),  INTENT(INOUT) :: InData
  INTEGER(IntKi),   INTENT(  OUT) :: ErrStat
  CHARACTER(*),     INTENT(  OUT) :: ErrMsg
  LOGICAL,OPTIONAL, INTENT(IN   ) :: SizeOnly
    ! Local variables
  INTEGER(IntKi)                 :: Re_BufSz
  INTEGER(IntKi)                 :: Re_Xferred
  INTEGER(IntKi)                 :: Re_CurrSz
  INTEGER(IntKi)                 :: Db_BufSz
  INTEGER(IntKi)                 :: Db_Xferred
  INTEGER(IntKi)                 :: Db_CurrSz
  INTEGER(IntKi)                 :: Int_BufSz
  INTEGER(IntKi)                 :: Int_Xferred
  INTEGER(IntKi)                 :: Int_CurrSz
  INTEGER(IntKi)                 :: i,i1,i2,i3,i4,i5     
  LOGICAL                        :: OnlySize ! if present and true, do not pack, just allocate buffers
 ! buffers to store meshes, if any
  OnlySize = .FALSE.
  IF ( PRESENT(SizeOnly) ) THEN
    OnlySize = SizeOnly
  ENDIF
    !
  ErrStat = ErrID_None
  ErrMsg  = ""
  Re_Xferred  = 1
  Db_Xferred  = 1
  Int_Xferred  = 1
  Re_BufSz  = 0
  Db_BufSz  = 0
  Int_BufSz  = 0
  Int_BufSz  = Int_BufSz  + 1  ! NumChans
  Int_BufSz  = Int_BufSz  + 1  ! NumRecs
  Db_BufSz   = Db_BufSz   + 1  ! TimeStep
  Re_BufSz    = Re_BufSz    + SIZE( InData%Data )  ! Data 
  IF ( Re_BufSz  .GT. 0 ) ALLOCATE( ReKiBuf(  Re_BufSz  ) )
  IF ( Db_BufSz  .GT. 0 ) ALLOCATE( DbKiBuf(  Db_BufSz  ) )
  IF ( Int_BufSz .GT. 0 ) ALLOCATE( IntKiBuf( Int_BufSz ) )
  IF ( .NOT. OnlySize ) IntKiBuf ( Int_Xferred:Int_Xferred+(1)-1 ) = (InData%NumChans )
  Int_Xferred   = Int_Xferred   + 1
  IF ( .NOT. OnlySize ) IntKiBuf ( Int_Xferred:Int_Xferred+(1)-1 ) = (InData%NumRecs )
  Int_Xferred   = Int_Xferred   + 1
  IF ( .NOT. OnlySize ) DbKiBuf ( Db_Xferred:Db_Xferred+(1)-1 ) =  (InData%TimeStep )
  Db_Xferred   = Db_Xferred   + 1
  IF ( ALLOCATED(InData%Data) ) THEN
    IF ( .NOT. OnlySize ) ReKiBuf ( Re_Xferred:Re_Xferred+(SIZE(InData%Data))-1 ) =  PACK(InData%Data ,.TRUE.)
    Re_Xferred   = Re_Xferred   + SIZE(InData%Data)
  ENDIF
 END SUBROUTINE NWTC_Library_Packfastdatatype

 SUBROUTINE NWTC_Library_UnPackfastdatatype( ReKiBuf, DbKiBuf, IntKiBuf, Outdata, ErrStat, ErrMsg )
  REAL(ReKi),      ALLOCATABLE, INTENT(IN   ) :: ReKiBuf(:)
  REAL(DbKi),      ALLOCATABLE, INTENT(IN   ) :: DbKiBuf(:)
  INTEGER(IntKi),  ALLOCATABLE, INTENT(IN   ) :: IntKiBuf(:)
  TYPE(fastdatatype), INTENT(INOUT) :: OutData
  INTEGER(IntKi),  INTENT(  OUT) :: ErrStat
  CHARACTER(*),    INTENT(  OUT) :: ErrMsg
    ! Local variables
  INTEGER(IntKi)                 :: Re_BufSz
  INTEGER(IntKi)                 :: Re_Xferred
  INTEGER(IntKi)                 :: Re_CurrSz
  INTEGER(IntKi)                 :: Db_BufSz
  INTEGER(IntKi)                 :: Db_Xferred
  INTEGER(IntKi)                 :: Db_CurrSz
  INTEGER(IntKi)                 :: Int_BufSz
  INTEGER(IntKi)                 :: Int_Xferred
  INTEGER(IntKi)                 :: Int_CurrSz
  INTEGER(IntKi)                 :: i, i1, i2, i3, i4, i5
  LOGICAL, ALLOCATABLE           :: mask1(:)
  LOGICAL, ALLOCATABLE           :: mask2(:,:)
  LOGICAL, ALLOCATABLE           :: mask3(:,:,:)
  LOGICAL, ALLOCATABLE           :: mask4(:,:,:,:)
  LOGICAL, ALLOCATABLE           :: mask5(:,:,:,:,:)
 ! buffers to store meshes, if any
    !
  ErrStat = ErrID_None
  ErrMsg  = ""
  Re_Xferred  = 1
  Db_Xferred  = 1
  Int_Xferred  = 1
  Re_BufSz  = 0
  Db_BufSz  = 0
  Int_BufSz  = 0
  OutData%NumChans = IntKiBuf ( Int_Xferred )
  Int_Xferred   = Int_Xferred   + 1
  OutData%NumRecs = IntKiBuf ( Int_Xferred )
  Int_Xferred   = Int_Xferred   + 1
  OutData%TimeStep = DbKiBuf ( Db_Xferred )
  Db_Xferred   = Db_Xferred   + 1
  IF ( ALLOCATED(OutData%Data) ) THEN
  ALLOCATE(mask2(SIZE(OutData%Data,1),SIZE(OutData%Data,2))); mask2 = .TRUE.
    OutData%Data = UNPACK(ReKiBuf( Re_Xferred:Re_Xferred+(SIZE(OutData%Data))-1 ),mask2,OutData%Data)
  DEALLOCATE(mask2)
    Re_Xferred   = Re_Xferred   + SIZE(OutData%Data)
  ENDIF
  Re_Xferred   = Re_Xferred-1
  Db_Xferred   = Db_Xferred-1
  Int_Xferred  = Int_Xferred-1
 END SUBROUTINE NWTC_Library_UnPackfastdatatype

 SUBROUTINE NWTC_Library_Copyoutparmtype( SrcoutparmtypeData, DstoutparmtypeData, CtrlCode, ErrStat, ErrMsg )
  TYPE(outparmtype), INTENT(INOUT) :: SrcoutparmtypeData
  TYPE(outparmtype), INTENT(INOUT) :: DstoutparmtypeData
  INTEGER(IntKi),  INTENT(IN   ) :: CtrlCode
  INTEGER(IntKi),  INTENT(  OUT) :: ErrStat
  CHARACTER(*),    INTENT(  OUT) :: ErrMsg
! Local 
  INTEGER(IntKi)                 :: i,i1,i2,i3,i4,i5,j,k
! 
  ErrStat = ErrID_None
  ErrMsg  = ""
  DstoutparmtypeData%Indx = SrcoutparmtypeData%Indx
  DstoutparmtypeData%Name = SrcoutparmtypeData%Name
  DstoutparmtypeData%Units = SrcoutparmtypeData%Units
  DstoutparmtypeData%SignM = SrcoutparmtypeData%SignM
 END SUBROUTINE NWTC_Library_Copyoutparmtype

 SUBROUTINE NWTC_Library_Destroyoutparmtype( outparmtypeData, ErrStat, ErrMsg )
  TYPE(outparmtype), INTENT(INOUT) :: outparmtypeData
  INTEGER(IntKi),  INTENT(  OUT) :: ErrStat
  CHARACTER(*),    INTENT(  OUT) :: ErrMsg
  INTEGER(IntKi)                 :: i, i1, i2, i3, i4, i5 
! 
  ErrStat = ErrID_None
  ErrMsg  = ""
 END SUBROUTINE NWTC_Library_Destroyoutparmtype

 SUBROUTINE NWTC_Library_Packoutparmtype( ReKiBuf, DbKiBuf, IntKiBuf, Indata, ErrStat, ErrMsg, SizeOnly )
  REAL(ReKi),       ALLOCATABLE, INTENT(  OUT) :: ReKiBuf(:)
  REAL(DbKi),       ALLOCATABLE, INTENT(  OUT) :: DbKiBuf(:)
  INTEGER(IntKi),   ALLOCATABLE, INTENT(  OUT) :: IntKiBuf(:)
  TYPE(outparmtype),  INTENT(INOUT) :: InData
  INTEGER(IntKi),   INTENT(  OUT) :: ErrStat
  CHARACTER(*),     INTENT(  OUT) :: ErrMsg
  LOGICAL,OPTIONAL, INTENT(IN   ) :: SizeOnly
    ! Local variables
  INTEGER(IntKi)                 :: Re_BufSz
  INTEGER(IntKi)                 :: Re_Xferred
  INTEGER(IntKi)                 :: Re_CurrSz
  INTEGER(IntKi)                 :: Db_BufSz
  INTEGER(IntKi)                 :: Db_Xferred
  INTEGER(IntKi)                 :: Db_CurrSz
  INTEGER(IntKi)                 :: Int_BufSz
  INTEGER(IntKi)                 :: Int_Xferred
  INTEGER(IntKi)                 :: Int_CurrSz
  INTEGER(IntKi)                 :: i,i1,i2,i3,i4,i5     
  LOGICAL                        :: OnlySize ! if present and true, do not pack, just allocate buffers
 ! buffers to store meshes, if any
  OnlySize = .FALSE.
  IF ( PRESENT(SizeOnly) ) THEN
    OnlySize = SizeOnly
  ENDIF
    !
  ErrStat = ErrID_None
  ErrMsg  = ""
  Re_Xferred  = 1
  Db_Xferred  = 1
  Int_Xferred  = 1
  Re_BufSz  = 0
  Db_BufSz  = 0
  Int_BufSz  = 0
  Int_BufSz  = Int_BufSz  + 1  ! Indx
  Int_BufSz  = Int_BufSz  + 1  ! SignM
  IF ( Re_BufSz  .GT. 0 ) ALLOCATE( ReKiBuf(  Re_BufSz  ) )
  IF ( Db_BufSz  .GT. 0 ) ALLOCATE( DbKiBuf(  Db_BufSz  ) )
  IF ( Int_BufSz .GT. 0 ) ALLOCATE( IntKiBuf( Int_BufSz ) )
  IF ( .NOT. OnlySize ) IntKiBuf ( Int_Xferred:Int_Xferred+(1)-1 ) = (InData%Indx )
  Int_Xferred   = Int_Xferred   + 1
  IF ( .NOT. OnlySize ) IntKiBuf ( Int_Xferred:Int_Xferred+(1)-1 ) = (InData%SignM )
  Int_Xferred   = Int_Xferred   + 1
 END SUBROUTINE NWTC_Library_Packoutparmtype

 SUBROUTINE NWTC_Library_UnPackoutparmtype( ReKiBuf, DbKiBuf, IntKiBuf, Outdata, ErrStat, ErrMsg )
  REAL(ReKi),      ALLOCATABLE, INTENT(IN   ) :: ReKiBuf(:)
  REAL(DbKi),      ALLOCATABLE, INTENT(IN   ) :: DbKiBuf(:)
  INTEGER(IntKi),  ALLOCATABLE, INTENT(IN   ) :: IntKiBuf(:)
  TYPE(outparmtype), INTENT(INOUT) :: OutData
  INTEGER(IntKi),  INTENT(  OUT) :: ErrStat
  CHARACTER(*),    INTENT(  OUT) :: ErrMsg
    ! Local variables
  INTEGER(IntKi)                 :: Re_BufSz
  INTEGER(IntKi)                 :: Re_Xferred
  INTEGER(IntKi)                 :: Re_CurrSz
  INTEGER(IntKi)                 :: Db_BufSz
  INTEGER(IntKi)                 :: Db_Xferred
  INTEGER(IntKi)                 :: Db_CurrSz
  INTEGER(IntKi)                 :: Int_BufSz
  INTEGER(IntKi)                 :: Int_Xferred
  INTEGER(IntKi)                 :: Int_CurrSz
  INTEGER(IntKi)                 :: i, i1, i2, i3, i4, i5
  LOGICAL, ALLOCATABLE           :: mask1(:)
  LOGICAL, ALLOCATABLE           :: mask2(:,:)
  LOGICAL, ALLOCATABLE           :: mask3(:,:,:)
  LOGICAL, ALLOCATABLE           :: mask4(:,:,:,:)
  LOGICAL, ALLOCATABLE           :: mask5(:,:,:,:,:)
 ! buffers to store meshes, if any
    !
  ErrStat = ErrID_None
  ErrMsg  = ""
  Re_Xferred  = 1
  Db_Xferred  = 1
  Int_Xferred  = 1
  Re_BufSz  = 0
  Db_BufSz  = 0
  Int_BufSz  = 0
  OutData%Indx = IntKiBuf ( Int_Xferred )
  Int_Xferred   = Int_Xferred   + 1
  OutData%SignM = IntKiBuf ( Int_Xferred )
  Int_Xferred   = Int_Xferred   + 1
  Re_Xferred   = Re_Xferred-1
  Db_Xferred   = Db_Xferred-1
  Int_Xferred  = Int_Xferred-1
 END SUBROUTINE NWTC_Library_UnPackoutparmtype

 SUBROUTINE NWTC_Library_Copyfileinfotype( SrcfileinfotypeData, DstfileinfotypeData, CtrlCode, ErrStat, ErrMsg )
  TYPE(fileinfotype), INTENT(INOUT) :: SrcfileinfotypeData
  TYPE(fileinfotype), INTENT(INOUT) :: DstfileinfotypeData
  INTEGER(IntKi),  INTENT(IN   ) :: CtrlCode
  INTEGER(IntKi),  INTENT(  OUT) :: ErrStat
  CHARACTER(*),    INTENT(  OUT) :: ErrMsg
! Local 
  INTEGER(IntKi)                 :: i,i1,i2,i3,i4,i5,j,k
! 
  ErrStat = ErrID_None
  ErrMsg  = ""
  DstfileinfotypeData%NumLines = SrcfileinfotypeData%NumLines
IF ( ALLOCATED( SrcfileinfotypeData%FileLine ) ) THEN
  i1 = SIZE(SrcfileinfotypeData%FileLine,1)
  IF (.NOT.ALLOCATED(DstfileinfotypeData%FileLine)) ALLOCATE(DstfileinfotypeData%FileLine(i1))
  DstfileinfotypeData%FileLine = SrcfileinfotypeData%FileLine
ENDIF
IF ( ALLOCATED( SrcfileinfotypeData%Lines ) ) THEN
  i1 = SIZE(SrcfileinfotypeData%Lines,1)
  IF (.NOT.ALLOCATED(DstfileinfotypeData%Lines)) ALLOCATE(DstfileinfotypeData%Lines(i1))
  DstfileinfotypeData%Lines = SrcfileinfotypeData%Lines
ENDIF
 END SUBROUTINE NWTC_Library_Copyfileinfotype

 SUBROUTINE NWTC_Library_Destroyfileinfotype( fileinfotypeData, ErrStat, ErrMsg )
  TYPE(fileinfotype), INTENT(INOUT) :: fileinfotypeData
  INTEGER(IntKi),  INTENT(  OUT) :: ErrStat
  CHARACTER(*),    INTENT(  OUT) :: ErrMsg
  INTEGER(IntKi)                 :: i, i1, i2, i3, i4, i5 
! 
  ErrStat = ErrID_None
  ErrMsg  = ""
  IF ( ALLOCATED(fileinfotypeData%FileLine) ) DEALLOCATE(fileinfotypeData%FileLine)
  IF ( ALLOCATED(fileinfotypeData%Lines) ) DEALLOCATE(fileinfotypeData%Lines)
 END SUBROUTINE NWTC_Library_Destroyfileinfotype

 SUBROUTINE NWTC_Library_Packfileinfotype( ReKiBuf, DbKiBuf, IntKiBuf, Indata, ErrStat, ErrMsg, SizeOnly )
  REAL(ReKi),       ALLOCATABLE, INTENT(  OUT) :: ReKiBuf(:)
  REAL(DbKi),       ALLOCATABLE, INTENT(  OUT) :: DbKiBuf(:)
  INTEGER(IntKi),   ALLOCATABLE, INTENT(  OUT) :: IntKiBuf(:)
  TYPE(fileinfotype),  INTENT(INOUT) :: InData
  INTEGER(IntKi),   INTENT(  OUT) :: ErrStat
  CHARACTER(*),     INTENT(  OUT) :: ErrMsg
  LOGICAL,OPTIONAL, INTENT(IN   ) :: SizeOnly
    ! Local variables
  INTEGER(IntKi)                 :: Re_BufSz
  INTEGER(IntKi)                 :: Re_Xferred
  INTEGER(IntKi)                 :: Re_CurrSz
  INTEGER(IntKi)                 :: Db_BufSz
  INTEGER(IntKi)                 :: Db_Xferred
  INTEGER(IntKi)                 :: Db_CurrSz
  INTEGER(IntKi)                 :: Int_BufSz
  INTEGER(IntKi)                 :: Int_Xferred
  INTEGER(IntKi)                 :: Int_CurrSz
  INTEGER(IntKi)                 :: i,i1,i2,i3,i4,i5     
  LOGICAL                        :: OnlySize ! if present and true, do not pack, just allocate buffers
 ! buffers to store meshes, if any
  OnlySize = .FALSE.
  IF ( PRESENT(SizeOnly) ) THEN
    OnlySize = SizeOnly
  ENDIF
    !
  ErrStat = ErrID_None
  ErrMsg  = ""
  Re_Xferred  = 1
  Db_Xferred  = 1
  Int_Xferred  = 1
  Re_BufSz  = 0
  Db_BufSz  = 0
  Int_BufSz  = 0
  Int_BufSz  = Int_BufSz  + 1  ! NumLines
  Int_BufSz   = Int_BufSz   + SIZE( InData%FileLine )  ! FileLine 
  IF ( Re_BufSz  .GT. 0 ) ALLOCATE( ReKiBuf(  Re_BufSz  ) )
  IF ( Db_BufSz  .GT. 0 ) ALLOCATE( DbKiBuf(  Db_BufSz  ) )
  IF ( Int_BufSz .GT. 0 ) ALLOCATE( IntKiBuf( Int_BufSz ) )
  IF ( .NOT. OnlySize ) IntKiBuf ( Int_Xferred:Int_Xferred+(1)-1 ) = (InData%NumLines )
  Int_Xferred   = Int_Xferred   + 1
  IF ( ALLOCATED(InData%FileLine) ) THEN
    IF ( .NOT. OnlySize ) IntKiBuf ( Int_Xferred:Int_Xferred+(SIZE(InData%FileLine))-1 ) = PACK(InData%FileLine ,.TRUE.)
    Int_Xferred   = Int_Xferred   + SIZE(InData%FileLine)
  ENDIF
 END SUBROUTINE NWTC_Library_Packfileinfotype

 SUBROUTINE NWTC_Library_UnPackfileinfotype( ReKiBuf, DbKiBuf, IntKiBuf, Outdata, ErrStat, ErrMsg )
  REAL(ReKi),      ALLOCATABLE, INTENT(IN   ) :: ReKiBuf(:)
  REAL(DbKi),      ALLOCATABLE, INTENT(IN   ) :: DbKiBuf(:)
  INTEGER(IntKi),  ALLOCATABLE, INTENT(IN   ) :: IntKiBuf(:)
  TYPE(fileinfotype), INTENT(INOUT) :: OutData
  INTEGER(IntKi),  INTENT(  OUT) :: ErrStat
  CHARACTER(*),    INTENT(  OUT) :: ErrMsg
    ! Local variables
  INTEGER(IntKi)                 :: Re_BufSz
  INTEGER(IntKi)                 :: Re_Xferred
  INTEGER(IntKi)                 :: Re_CurrSz
  INTEGER(IntKi)                 :: Db_BufSz
  INTEGER(IntKi)                 :: Db_Xferred
  INTEGER(IntKi)                 :: Db_CurrSz
  INTEGER(IntKi)                 :: Int_BufSz
  INTEGER(IntKi)                 :: Int_Xferred
  INTEGER(IntKi)                 :: Int_CurrSz
  INTEGER(IntKi)                 :: i, i1, i2, i3, i4, i5
  LOGICAL, ALLOCATABLE           :: mask1(:)
  LOGICAL, ALLOCATABLE           :: mask2(:,:)
  LOGICAL, ALLOCATABLE           :: mask3(:,:,:)
  LOGICAL, ALLOCATABLE           :: mask4(:,:,:,:)
  LOGICAL, ALLOCATABLE           :: mask5(:,:,:,:,:)
 ! buffers to store meshes, if any
    !
  ErrStat = ErrID_None
  ErrMsg  = ""
  Re_Xferred  = 1
  Db_Xferred  = 1
  Int_Xferred  = 1
  Re_BufSz  = 0
  Db_BufSz  = 0
  Int_BufSz  = 0
  OutData%NumLines = IntKiBuf ( Int_Xferred )
  Int_Xferred   = Int_Xferred   + 1
  IF ( ALLOCATED(OutData%FileLine) ) THEN
  ALLOCATE(mask1(SIZE(OutData%FileLine,1))); mask1 = .TRUE.
    OutData%FileLine = UNPACK(IntKiBuf( Int_Xferred:Re_Xferred+(SIZE(OutData%FileLine))-1 ),mask1,OutData%FileLine)
  DEALLOCATE(mask1)
    Int_Xferred   = Int_Xferred   + SIZE(OutData%FileLine)
  ENDIF
  Re_Xferred   = Re_Xferred-1
  Db_Xferred   = Db_Xferred-1
  Int_Xferred  = Int_Xferred-1
 END SUBROUTINE NWTC_Library_UnPackfileinfotype

!bjj: deleted remaining (unnecessary) subroutines...

END MODULE NWTC_Library_Types
!ENDOFREGISTRYGENERATEDFILE
