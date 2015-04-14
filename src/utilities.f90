
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


MODULE utilities_mod

  IMPLICIT NONE

  PUBLIC  :: convupper,findstr,gather,getconn,getsconn,corrconn,sort,   &
&            arth
  PRIVATE :: quicksort,iquicksort,indexi,indexr

  INTERFACE sort
    MODULE PROCEDURE quicksort,iquicksort
  END INTERFACE sort

CONTAINS

  SUBROUTINE convupper(str)

    USE kinds_mod,ONLY: ink

    ! Argument list
    CHARACTER(LEN=*),INTENT(INOUT) :: str
    ! Local
    INTEGER(KIND=ink)              :: ii,ic

    DO ii=1,LEN(str)
      ic=IACHAR(str(ii:ii))
      IF ((ic.GE.97_ink).AND.(ic.LE.122)) THEN
        ic=ic-32_ink
        str(ii:ii)=ACHAR(ic)
      ENDIF
    ENDDO

  END SUBROUTINE convupper

  FUNCTION findstr(str,iunit) RESULT(zflag)

    USE kinds_mod,     ONLY: ink,lok
    USE parameters_mod,ONLY: LN

    ! Argument list
    CHARACTER(LEN=*), INTENT(IN) :: str
    INTEGER(KIND=ink),INTENT(IN) :: iunit
    ! Result
    LOGICAL(KIND=lok)            :: zflag
    ! Local
    INTEGER(KIND=ink)            :: il,ierr
    CHARACTER(LEN=LN)            :: sline
    CHARACTER(LEN=LEN(str))      :: stru

    ! Initialise
    zflag=.FALSE._lok
    ! search for string
    il=LEN(str)
    stru=str
    CALL convupper(stru)
    REWIND(UNIT=iunit)
    l1:DO
      READ(UNIT=iunit,FMT=*,IOSTAT=ierr) sline
      IF (ierr.LT.0_ink) RETURN
      sline=TRIM(ADJUSTL(sline))
      IF (sline(1:1).EQ.'&') THEN
        CALL convupper(sline(2:il+1))
        IF (stru(1:il).EQ.sline(2:il+1)) THEN
          zflag=.TRUE._lok
          RETURN
        ENDIF
      ENDIF
    ENDDO l1

  END FUNCTION findstr

  SUBROUTINE gather(n1,n2,n3,indx,aa,bb)

    USE kinds_mod,ONLY: ink,rlk

    ! Argument list
    INTEGER(KIND=ink),                 INTENT(IN)  :: n1,n2,n3
    INTEGER(KIND=ink),DIMENSION(n1,n2),INTENT(IN)  :: indx
    REAL(KIND=rlk),   DIMENSION(n3),   INTENT(IN)  :: aa
    REAL(KIND=rlk),   DIMENSION(n1,n2),INTENT(OUT) :: bb
    ! Local
    INTEGER(KIND=ink)                              :: ii,jj

    DO jj=1,n1
      DO ii=1,n2
        bb(jj,ii)=aa(indx(jj,ii))
      ENDDO
    ENDDO

  END SUBROUTINE gather

  PURE FUNCTION getconn(nCell,nFace,e2v) RESULT(e2e)

    ! Internal
    USE kinds_mod,ONLY: ink

    ! argument list
    INTEGER(KIND=ink),                       INTENT(IN) :: nCell,nFace
    INTEGER(KIND=ink),DIMENSION(nFace,nCell),INTENT(IN) :: e2v
    ! result
    INTEGER(KIND=ink),DIMENSION(nFace,nCell)            :: e2e
    ! local
    INTEGER(KIND=ink)                                   :: i1,i2,i3,i4, &
&                                                          i5,i6,nSz
    INTEGER(KIND=ink),DIMENSION(nCell*nFace)            :: iConn,iUind, &
&                                                          iWork1,iWork2

    ! initialise
    e2e=0_ink

    ! set loop size
    nSz=nFace*nCell

    ! set work and initial connectivity
    DO i1=1,nCell
      i2=nCell+i1
      i3=nCell+i2
      i4=nCell+i3
      iWork1(i1)=e2v(1,i1)
      iWork2(i1)=e2v(2,i1)
      iWork1(i2)=e2v(2,i1)
      iWork2(i2)=e2v(3,i1)
      iWork1(i3)=e2v(3,i1)
      iWork2(i3)=e2v(4,i1)
      iWork1(i4)=e2v(4,i1)
      iWork2(i4)=e2v(1,i1)
      iConn(i1)=i1
      iConn(i2)=i1
      iConn(i3)=i1
      iConn(i4)=i1
    ENDDO

    ! set unique index
    i4=MAXVAL(e2v)
    DO i1=1,nSz
      i5=iWork1(i1)
      i6=iWork2(i1)
      i2=MAX(i5,i6)
      i3=MIN(i5,i6)-1_ink
      iUind(i1)=i3*i4+i2
    ENDDO

    ! sort unique index
    iWork1=sort(iUind)
    IF (iWork1(1).EQ.-HUGE(1_ink)) THEN
      e2e(1,1)=-HUGE(1_ink)
      RETURN
    ENDIF

    ! find matches
    i2=0_ink
    DO i1=1,nSz-1
      i3=iWork1(i1)
      i4=iWork1(i1+1)
      IF (iUind(i3).EQ.iUind(i4)) THEN
        i2=i2+1_ink
        iWork2(i2)=i1
      ENDIF
    ENDDO

    ! insert matches into connectivity table
    iUind=arth(1_ink,1_ink,nSz)
    iUind=iUind(iWork1)
    iConn=iConn(iWork1)
    iWork1=0_ink
    DO i1=1,i2
      i3=iWork2(i1)
      i4=i3+1_ink
      iWork1(iUind(i3))=iConn(i4)
      iWork1(iUind(i4))=iConn(i3)
    ENDDO

    ! copy to result
    i4=0_ink
    DO i1=1,nFace
      DO i3=1,nCell
        i4=i4+1_ink
        e2e(i1,i3)=iWork1(i4)
      ENDDO
    ENDDO

  END FUNCTION getconn

  PURE FUNCTION getsconn(nCell,nFace,e2e) RESULT(e2s)

    ! Internal
    USE kinds_mod,ONLY: ink

    ! argument list
    INTEGER(KIND=ink),                       INTENT(IN) :: nCell,nFace
    INTEGER(KIND=ink),DIMENSION(nFace,nCell),INTENT(IN) :: e2e
    ! result
    INTEGER(KIND=ink),DIMENSION(nFace,nCell)            :: e2s
    ! local
    INTEGER(KIND=ink)                                   :: iel,i1,i2,   &
&                                                          ineigh
    INTEGER(KIND=ink),DIMENSION(0:nCell)                :: istore

    ! initialise
    e2s=0_ink

    ! set side connectivity
    DO iel=1,nCell
      DO i1=1,nFace
        ineigh=e2e(i1,iel)
        IF (ineigh.GT.0_ink) THEN
          DO i2=1,nFace
            istore(e2e(i2,ineigh))=i2
          ENDDO
          e2s(i1,iel)=istore(iel)
        ENDIF
      ENDDO
    ENDDO

  END FUNCTION getsconn

  SUBROUTINE corrconn(nCell,nFace,e2e,e2s)

    USE kinds_mod,ONLY: ink

    ! Argument list
    INTEGER(KIND=ink),                       INTENT(IN)    :: nCell,nFace
    INTEGER(KIND=ink),DIMENSION(nFace,nCell),INTENT(INOUT) :: e2e,e2s
    ! Local
    INTEGER(KIND=ink)                                      :: iel,i1,i2

    DO iel=1,nCell
      DO i1=1,nFace/2
        i2=i1+2_ink
        IF (e2e(i1,iel).EQ.0_ink) THEN
          e2e(i1,iel)=iel
          e2s(i1,iel)=i2
        ENDIF
        IF (e2e(i2,iel).EQ.0_ink) THEN
          e2e(i2,iel)=iel
          e2s(i2,iel)=i1
        ENDIF
      ENDDO
    ENDDO

  END SUBROUTINE corrconn

  PURE FUNCTION quicksort(arr) RESULT(slave)

    USE kinds_mod,ONLY: ink,rlk

!   Argument list
    REAL(KIND=rlk),   DIMENSION(:),INTENT(in) :: arr
!   Result    
    INTEGER(KIND=ink),DIMENSION(SIZE(arr))    :: slave
!   Local
    INTEGER(KIND=ink),DIMENSION(SIZE(arr))    :: index1

    index1=indexr(arr)
    IF (index1(1).EQ.-HUGE(1_ink)) THEN
      slave(1)=-HUGE(1_ink)
      RETURN
    ENDIF
    slave=arth(1_ink,1_ink,SIZE(arr))
    slave=slave(index1)

  END FUNCTION quicksort

  PURE FUNCTION iquicksort(arr) RESULT(slave)

    USE kinds_mod,ONLY: ink

!   Argument list
    INTEGER(KIND=ink),DIMENSION(:),INTENT(in) :: arr
!   Result    
    INTEGER(KIND=ink),DIMENSION(SIZE(arr))    :: slave
!   Local
    INTEGER(KIND=ink),DIMENSION(SIZE(arr))    :: index1

    index1=indexi(arr)
    IF (index1(1).EQ.-HUGE(1_ink)) THEN
      slave(1)=-HUGE(1_ink)
      RETURN
    ENDIF
    slave=arth(1_ink,1_ink,SIZE(arr))
    slave=slave(index1)

  END FUNCTION iquicksort

  PURE FUNCTION arth(first,increment,n) RESULT(rRes)

    USE kinds_mod,ONLY:ink

    ! Argument list
    INTEGER(KIND=ink),INTENT(in) :: first,increment,n
    ! Result
    INTEGER(KIND=ink),DIMENSION(n) :: rRes
    ! Local
    INTEGER(KIND=ink) :: k,k2,temp
    INTEGER(KIND=ink),PARAMETER :: npar_arth=16_ink,npar2_arth=8_ink

    IF (n.gt.0_ink) rRes(1)=first
    IF (n.le.npar_arth) THEN
      DO k=2,n
        rRes(k)=rRes(k-1)+increment
      ENDDO
    ELSE
      DO k=2,npar2_arth
        rRes(k)=rRes(k-1)+increment
      ENDDO
      temp=increment*npar2_arth
      k=npar2_arth
      DO
        IF (k.ge.n) EXIT
        k2=k+k
        rRes(k+1:MIN(k2,n))=temp+rRes(1:MIN(k,n-k))
        temp=temp+temp
        k=k2
      ENDDO
    ENDIF     

  END FUNCTION arth

  PURE FUNCTION indexr(arr) RESULT(index1)

    USE kinds_mod,ONLY: ink,rlk

!   Argument list
    REAL(KIND=rlk),   DIMENSION(:),        INTENT(in) :: arr
!   Result    
    INTEGER(KIND=ink),DIMENSION(SIZE(arr)),TARGET     :: index1
!   Local
    REAL(KIND=rlk) :: a
    INTEGER(KIND=ink),PARAMETER         :: nn=7_ink,nstack=500_ink
    INTEGER(KIND=ink) :: n,k,i,j,indext,jstack,l,r,sw,swap
    INTEGER(KIND=ink),POINTER           :: p1,p2,p3
    INTEGER(KIND=ink),DIMENSION(nstack) :: istack

    n=SIZE(arr)
    index1=arth(1_ink,1_ink,n)
    jstack=0_ink
    l=1_ink
    r=n
    p1=>NULL()
    p2=>NULL()
    p3=>NULL()
    DO
      IF (r-l.lt.nn) THEN
        DO j=l+1,r
          indext=index1(j)
          a=arr(indext)
          DO i=j-1,l,-1
            IF (arr(index1(i)).le.a) EXIT
            index1(i+1)=index1(i)
          ENDDO
          index1(i+1)=indext
        ENDDO
        IF (jstack.eq.0_ink) RETURN
        r=istack(jstack)
        l=istack(jstack-1)
        jstack=jstack-2_ink
      ELSE
        k=(l+r)/2_ink
        swap=index1(k)
        index1(k)=index1(l+1)
        index1(l+1)=swap
        p1=>index1(l)
        p2=>index1(r)
        p3=>index1(l+1)
        IF (arr(p2).lt.arr(p1)) THEN
          sw=p1
          p1=p2
          p2=sw
        ENDIF
        IF (arr(p2).lt.arr(p3)) THEN
          sw=p3
          p3=p2
          p2=sw
        ENDIF
        IF (arr(p3).lt.arr(p1)) THEN
          sw=p1
          p1=p3
          p3=sw
        ENDIF
        i=l+1_ink
        j=r
        indext=index1(l+1)
        a=arr(indext)
        DO
          DO
            i=i+1_ink
            IF (arr(index1(i)).ge.a) EXIT
          ENDDO
          DO
            j=j-1_ink
            IF (arr(index1(j)).le.a) EXIT
          ENDDO
          IF (j.lt.i) EXIT
          swap=index1(i)
          index1(i)=index1(j)
          index1(j)=swap
        ENDDO
        index1(l+1)=index1(j)
        index1(j)=indext
        jstack=jstack+2_ink
        IF (jstack.gt.nstack) THEN
          index1(1)=-HUGE(1_ink)
          RETURN
        ENDIF
        IF (r-i+1.ge.j-1) THEN
          istack(jstack)=r
          istack(jstack-1)=i
          r=j-1_ink
        ELSE
          istack(jstack)=j-1_ink
          istack(jstack-1)=l
          l=i
        ENDIF
      ENDIF
    ENDDO

  END FUNCTION indexr

  PURE FUNCTION indexi(arr) RESULT(index1)

    USE kinds_mod,ONLY: ink

!   Argument list
    INTEGER(KIND=ink),DIMENSION(:),        INTENT(in) :: arr
!   Result    
    INTEGER(KIND=ink),DIMENSION(SIZE(arr)),TARGET     :: index1
!   Local
    INTEGER(KIND=ink) :: a
    INTEGER(KIND=ink),PARAMETER         :: nn=7_ink,nstack=500_ink
    INTEGER(KIND=ink) :: n,k,i,j,indext,jstack,l,r,sw,swap
    INTEGER(KIND=ink),POINTER           :: p1,p2,p3
    INTEGER(KIND=ink),DIMENSION(nstack) :: istack

    n=SIZE(arr)
    index1=arth(1_ink,1_ink,n)
    jstack=0_ink
    l=1_ink
    r=n
    p1=>NULL()
    p2=>NULL()
    p3=>NULL()
    DO
      IF (r-l.lt.nn) THEN
        DO j=l+1,r
          indext=index1(j)
          a=arr(indext)
          DO i=j-1,l,-1
            IF (arr(index1(i)).le.a) EXIT
            index1(i+1)=index1(i)
          ENDDO
          index1(i+1)=indext
        ENDDO
        IF (jstack.eq.0_ink) RETURN
        r=istack(jstack)
        l=istack(jstack-1)
        jstack=jstack-2_ink
      ELSE
        k=(l+r)/2_ink
        swap=index1(k)
        index1(k)=index1(l+1)
        index1(l+1)=swap
        p1=>index1(l)
        p2=>index1(r)
        p3=>index1(l+1)
        IF (arr(p2).lt.arr(p1)) THEN
          sw=p1
          p1=p2
          p2=sw
        ENDIF
        IF (arr(p2).lt.arr(p3)) THEN
          sw=p3
          p3=p2
          p2=sw
        ENDIF
        IF (arr(p3).lt.arr(p1)) THEN
          sw=p1
          p1=p3
          p3=sw
        ENDIF
        i=l+1_ink
        j=r
        indext=index1(l+1)
        a=arr(indext)
        DO
          DO
            i=i+1_ink
            IF (arr(index1(i)).ge.a) EXIT
          ENDDO
          DO
            j=j-1_ink
            IF (arr(index1(j)).le.a) EXIT
          ENDDO
          IF (j.lt.i) EXIT
          swap=index1(i)
          index1(i)=index1(j)
          index1(j)=swap
        ENDDO
        index1(l+1)=index1(j)
        index1(j)=indext
        jstack=jstack+2_ink
        IF (jstack.gt.nstack) THEN
          index1(1)=-HUGE(1_ink)
          RETURN
        ENDIF
        IF (r-i+1.ge.j-1) THEN
          istack(jstack)=r
          istack(jstack-1)=i
          r=j-1_ink
        ELSE
          istack(jstack)=j-1_ink
          istack(jstack-1)=l
          l=i
        ENDIF
      ENDIF
    ENDDO

  END FUNCTION indexi

END MODULE utilities_mod  
