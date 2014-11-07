
!Crown Copyright 2014 AWE.
!
! This file is part of Bookleaf.
!
! Bookleaf is free software: you can redistribute it and/or modify it under
! the terms of the GNU General Public License as published by the
! Free Software Foundation, either version 3 of the License, or (at your option)
! any later version.
!
! Bookleaf is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
! FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
! details.
!
! You should have received a copy of the GNU General Public License along with
! Bookleaf. If not, see http://www.gnu.org/licenses/.


MODULE silo_mod

  IMPLICIT NONE

  PUBLIC  :: write_silo_dump

CONTAINS

  SUBROUTINE write_silo_dump(cDirName)

    USE kinds_mod,   ONLY: ink
    USE integers_mod,ONLY: nMat,nEl,nNod
    USE pointers_mod,ONLY: iElNod,iElMat,ndx,ndy
    USE error_mod,   ONLY: halt
    USE paradef_mod, ONLY: NprocW,MprocW,rankW
    USE utils_f_mod, ONLY: utils_mkdir_f,utils_ln_f,UTILS_SUCCESS
    INCLUDE "silo.inc"

    ! Argument list
    CHARACTER(LEN=*),INTENT(IN)                  :: cDirName
    ! Local
    INTEGER(KIND=ink)                            :: iErr,ii,iFileID,    &
&                                                   iOptionID,jj,kk
    INTEGER(KIND=ink),  PARAMETER                :: SLEN=80,NSHAPE=1
    CHARACTER(LEN=4)                             :: cNum
    CHARACTER(LEN=SLEN)                          :: cString
    CHARACTER(LEN=SLEN),PARAMETER                :: cDataName='domain', &
&                                                   cHeaderFile='Top.silo'
    CHARACTER(LEN=SLEN),DIMENSION(:),ALLOCATABLE :: cMat
    INTEGER(KIND=ink),  DIMENSION(:),ALLOCATABLE :: iMat,lMat,iNodeList
    CHARACTER(LEN=SLEN),DIMENSION(NprocW)        :: cName
    INTEGER(KIND=ink),  DIMENSION(NprocW)        :: iName,iType
    INTEGER(KIND=ink),  DIMENSION(NSHAPE)        :: iShapeType,         &
&                                                   iShapeSize,         &
&                                                   iShapeCount

    ! make directory
    iErr=utils_mkdir_f(TRIM(cDirName))
    IF (iErr.NE.UTILS_SUCCESS) CALL halt("ERROR: failed to make silo "  &
&    //"directory",0)

    ! remove deprecated warning messages
    iErr=DBSetDepWarn(0)

    ! header file on Master
    IF (MProcW) THEN
      ! open
      cString=TRIM(cDirName)//'/'//TRIM(cHeaderFile)
      iErr=DBCreate(TRIM(cString),LEN_TRIM(cString),DB_CLOBBER,DB_LOCAL,&
&                   'SILO file from Bookleaf',23,DB_HDF5,iFileID)
      IF (iFileID.EQ.-1) CALL halt("ERROR: failed to create silo file",0)
      ! write mesh
      DO ii=1,NProcW
        WRITE(cNum,'(i4)') ii+999_ink
        cString=TRIM(cDirName)//'/'//TRIM(cDataName)//cNum(2:4)//'.silo'
        cName(ii)=TRIM(cString)//':MESH'
        iName(ii)=LEN_TRIM(cName(ii))
        iType(ii)=DB_UCDMESH
      ENDDO
      iErr=DBPutMMesh(iFileID,'MMESH',5,nProcW,cName,iName,iType,       &
&                     DB_F77NULL,ii)
      ! write material
      ALLOCATE(iMat(nMat),cMat(nMat),lMat(nMat),STAT=iErr)
      IF (iErr.NE.0) CALL halt("ERROR: failed to allocate material for "&
&      //"silo",0)
      DO ii=1,nMat
        iMat(ii)=ii
        WRITE(cNum,'(i4)') ii+1000_ink
        cMat(ii)='material '//cNum(2:4)
        lMat(ii)=LEN_TRIM(cMat(ii))
      ENDDO
      iErr=DBSet2DStrLen(SLEN)
      iErr=DBMkOptList(4,iOptionID)
      iErr=DBAddIOpt(iOptionID,DBOPT_NMATNOS,nMat)
      iErr=DBAddIOpt(iOptionID,DBOPT_MATNOS,iMat)
      iErr=DBAddCAOpt(iOptionID,DBOPT_MATNAMES,nMat,cMat,lMat)
      iErr=DBAddCOpt(iOptionID,DBOPT_MMESH_NAME,'MMESH',5)
      DO ii=1,NProcW
        WRITE(cNum,'(i4)') ii+999_ink
        cString=TRIM(cDirName)//'/'//TRIM(cDataName)//cNum(2:4)//'.silo'
        cName(ii)=TRIM(cString)//':MATERIAL'
        iName(ii)=LEN_TRIM(cName(ii))
      ENDDO
      iErr=DBPutMMat(iFileID,'MMATERIAL',9,nProcW,cName,iName,          &
&                    iOptionID,ii)
      iErr=DBFreeOptList(iOptionID)
      ! write variables
      ! close header file
      iErr=DBClose(iFileID)
    ENDIF

    ! data file on each PE
    WRITE(cNum,'(i4)') rankW+1000_ink
    cString=TRIM(cDirName)//'/'//TRIM(cDataName)//cNum(2:4)//'.silo'
    iErr=DBCreate(TRIM(cString),LEN_TRIM(cString),DB_CLOBBER,DB_LOCAL,  &
&                 'SILO file from bookleaf',23,DB_HDF5,iFileID)
    IF (iFileID.EQ.-1) CALL halt("ERROR: failed to create silo data "   &
&    //"file",0)
    ! write mesh
    iErr=DBMkOptList(3,iOptionID)
    cString(1:1)='X'
    cString(2:2)='Y'
    iErr=DBAddCOpt(iOptionID,DBOPT_XLABEL,cString(1:1),1)
    iErr=DBAddCOpt(iOptionID,DBOPT_YLABEL,cString(2:2),1)
    iErr=DBAddIOpt(iOptionID,DBOPT_COORDSYS,DB_CARTESIAN)
    iShapeType(1)=DB_ZONETYPE_QUAD
    iShapeSize(1)=4
    iShapeCount(1)=nEl
    ALLOCATE(iNodeList(iShapeSize(1)*nEl),STAT=iErr)
    IF (iErr.NE.0) CALL halt("ERROR: failed to allocate connectivity "  &
&    //"for silo mesh",0)
    kk=1_ink
    DO ii=1,nEl
      DO jj=1,4
        iNodeList(kk)=iElNod(jj,ii)
        kk=kk+1_ink
      ENDDO
    ENDDO
    iErr=DBPutZl2(iFileID,'NodeList',8,nEl,2,iNodelist,4*nEl,1,0,0,     &
&                 iShapeType,iShapeSize,iShapeCount,nShape,DB_F77NULL,ii)
    iErr=DBPutUM(iFileID,'MESH',4,2,ndx(1:nNod),ndy(1:nNod),DB_F77NULL, &
&                cString(1:1),1,cString(2:2),1,DB_F77NULL,0,DB_DOUBLE,  &
&                nNod,nEl,'NodeList',8,DB_F77NULL,0,iOptionID,ii)
    iErr=DBFreeOptList(iOptionID)
    DEALLOCATE(iNodeList,STAT=iErr)
    IF (iErr.NE.0) CALL halt("ERROR: failed to deallocate connectivity "&
&    //"for silo mesh",0)
    ! write material
    iErr=DBSet2DStrLen(SLEN)
    iErr=DBMkOptList(2,iOptionID)
    iErr=DBAddIOpt(iOptionID,DBOPT_ORIGIN,0)
    iErr=DBAddCAOpt(iOptionID,DBOPT_MATNAMES,nMat,cMat,lMat)
    iErr=DBPutMat(iFileID,'MATERIAL',8,'MESH',4,nMat,iMat,iElMat(1:nEl),&
&                 [nEl],1,DB_F77NULL,DB_F77NULL,DB_F77NULL,DB_F77NULL,0,&
&                 DB_DOUBLE,iOptionID,ii)
    iErr=DBFreeOptList(iOptionID)
    DEALLOCATE(iMat,lMat,cMat,STAT=iErr)
    IF (iErr.NE.0) CALL halt("ERROR: failed to deallocate material for "&
&    //"silo",0)
    ! write variables
    ! close data file
    iErr=DBClose(iFileID)

    ! create symbolic link
    iErr=utils_ln_f(TRIM(cDirName)//'/'//TRIM(cHeaderFile),             &
&                   TRIM(cDirName)//'.silo')
    IF (iErr.NE.UTILS_SUCCESS) CALL halt("ERROR: failed to create silo "&
&    //"link",0)

  END SUBROUTINE write_silo_dump

END MODULE silo_mod  
