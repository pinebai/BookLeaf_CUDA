
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

MODULE geometry_mod

  IMPLICIT NONE

  PUBLIC  :: dlm,dln,getgeom
  PRIVATE :: denom,distpp,distpl

CONTAINS

  PURE FUNCTION dlm(nshape,elx,ely) RESULT(res)

    USE kinds_mod,   ONLY: rlk,ink

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN) :: nshape
    REAL(KIND=rlk),DIMENSION(nshape),INTENT(IN) :: elx,ely
    ! Result
    REAL(KIND=rlk),DIMENSION(nshape)            :: res
    ! Local
    REAL(KIND=rlk)                              :: x1,x2,y1,y2

    x1=elx(1)+elx(2)
    x2=elx(3)+elx(4)
    y1=ely(1)+ely(2)
    y2=ely(3)+ely(4)
    x1=0.5_rlk*(x1-x2)
    y1=0.5_rlk*(y1-y2)
    res(1)=x1*x1+y1*y1
    x1=elx(3)+elx(2)
    x2=elx(1)+elx(4)
    y1=ely(3)+ely(2)
    y2=ely(1)+ely(4)
    x1=0.5_rlk*(x1-x2)
    y1=0.5_rlk*(y1-y2)
    res(2)=x1*x1+y1*y1
    res(3)=res(1)
    res(4)=res(2)

  END FUNCTION dlm

  PURE FUNCTION dln(nshape,elx,ely) RESULT(res)

    USE kinds_mod,   ONLY: rlk,ink
    USE reals_mod,   ONLY: zcut

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN) :: nshape
    REAL(KIND=rlk),DIMENSION(nshape),INTENT(IN) :: elx,ely
    ! Result
    REAL(KIND=rlk),DIMENSION(nshape)            :: res
    ! Local
    REAL(KIND=rlk)                              :: w1

    w1=denom(elx(3),ely(3),elx(4),ely(4))
    IF (w1.LT.zcut) THEN
      res(1)=distpp(elx(1),ely(1),elx(2),ely(2),elx(3),ely(3))
    ELSE
      res(1)=distpl(elx(1),ely(1),elx(2),ely(2),elx(3),ely(3),elx(4),   &
&                   ely(4))/w1
    ENDIF
    w1=denom(elx(4),ely(4),elx(1),ely(1))
    IF (w1.LT.zcut) THEN
      res(2)=distpp(elx(2),ely(2),elx(3),ely(3),elx(4),ely(4))
    ELSE
      res(2)=distpl(elx(2),ely(2),elx(3),ely(3),elx(4),ely(4),elx(1),   &
&                   ely(1))/w1
    ENDIF
    w1=denom(elx(1),ely(1),elx(2),ely(2))
    IF (w1.LT.zcut) THEN
      res(3)=distpp(elx(3),ely(3),elx(4),ely(4),elx(1),ely(1))
    ELSE
      res(3)=distpl(elx(3),ely(3),elx(4),ely(4),elx(1),ely(1),elx(2),   &
&                   ely(2))/w1
    ENDIF
    w1=denom(elx(2),ely(2),elx(3),ely(3))
    IF (w1.LT.zcut) THEN
      res(4)=distpp(elx(4),ely(4),elx(1),ely(1),elx(2),ely(2))
    ELSE
      res(4)=distpl(elx(4),ely(4),elx(1),ely(1),elx(2),ely(2),elx(3),   &
&                   ely(3))/w1
    ENDIF

  END FUNCTION dln

  SUBROUTINE getgeom(nshape,nel,nnod,ndx,ndy,elx,ely)

    USE kinds_mod,     ONLY: ink,rlk
    USE integers_mod,  ONLY: nel1,nnod1
    USE utilities_mod, ONLY: gather
    USE pointers_mod,  ONLY: a1,a2,a3,b1,b2,b3,elvol,cnwt,ielnod
    USE error_mod,     ONLY: halt
    USE parameters_mod,ONLY: ONEBYNINE
    USE timing_stats,  ONLY: bookleaf_times
    USE TYPH_util_mod, ONLY: get_time

    ! Argument list
    INTEGER(KIND=ink),                    INTENT(IN)  :: nshape,nel,nnod
    REAL(KIND=rlk),DIMENSION(nnod1),      INTENT(IN)  :: ndx,ndy
    REAL(KIND=rlk),DIMENSION(nshape,nel1),INTENT(OUT) :: elx,ely
    ! Local
    INTEGER(KIND=ink)                                 :: iel,ierr
    REAL(KIND=rlk)                                    :: t0,t1

    ! Timer
    t0=get_time()

    ! Gather position to element
    CALL gather(nshape,nel,nnod,ielnod(1,1),ndx(1),elx(1,1))
    CALL gather(nshape,nel,nnod,ielnod(1,1),ndy(1),ely(1,1))

    ! Calculate volume and iso-parametric terms
    DO iel=1,nel
      a1(iel)=0.25_rlk*(-elx(1,iel)+elx(2,iel)+elx(3,iel)-elx(4,iel))
      a2(iel)=0.25_rlk*( elx(1,iel)-elx(2,iel)+elx(3,iel)-elx(4,iel))
      a3(iel)=0.25_rlk*(-elx(1,iel)-elx(2,iel)+elx(3,iel)+elx(4,iel))
      b1(iel)=0.25_rlk*(-ely(1,iel)+ely(2,iel)+ely(3,iel)-ely(4,iel))
      b2(iel)=0.25_rlk*( ely(1,iel)-ely(2,iel)+ely(3,iel)-ely(4,iel))
      b3(iel)=0.25_rlk*(-ely(1,iel)-ely(2,iel)+ely(3,iel)+ely(4,iel))
      cnwt(1,iel)=ONEBYNINE*                                            &
&                 ((3.0_rlk*b3(iel)-b2(iel))*(3.0_rlk*a1(iel)-a2(iel))  &
&                 -(3.0_rlk*a3(iel)-a2(iel))*(3.0_rlk*b1(iel)-b2(iel)))
      cnwt(2,iel)=ONEBYNINE*                                            &
&                 ((3.0_rlk*b3(iel)+b2(iel))*(3.0_rlk*a1(iel)-a2(iel))  &
&                 -(3.0_rlk*a3(iel)+a2(iel))*(3.0_rlk*b1(iel)-b2(iel)))
      cnwt(3,iel)=ONEBYNINE*                                            &
&                 ((3.0_rlk*b3(iel)+b2(iel))*(3.0_rlk*a1(iel)+a2(iel))  &
                  -(3.0_rlk*a3(iel)+a2(iel))*(3.0_rlk*b1(iel)+b2(iel)))
      cnwt(4,iel)=ONEBYNINE*                                            &
&                 ((3.0_rlk*b3(iel)-b2(iel))*(3.0_rlk*a1(iel)+a2(iel))  &
                  -(3.0_rlk*a3(iel)-a2(iel))*(3.0_rlk*b1(iel)+b2(iel)))
      elvol(iel)=4.0_rlk*(a1(iel)*b3(iel)-a3(iel)*b1(iel))
    ENDDO
    ierr=0_ink
    ierr=MINVAL(MINLOC(elvol(1:nel),MASK=(elvol(1:nel).LT.0.0_rlk)))
    IF (ierr.NE.0_ink) CALL halt("ERROR: cell volume < 0",1)

    ! Timing data
    t1=get_time()
    t1=t1-t0
    bookleaf_times%time_in_geom=bookleaf_times%time_in_geom+t1

  END SUBROUTINE getgeom

  PURE FUNCTION denom(x1,y1,x2,y2)

    USE kinds_mod,ONLY: rlk

    ! Argument list
    REAL(KIND=rlk),INTENT(IN) :: x1,y1,x2,y2
    ! Result
    REAL(KIND=rlk)            :: denom
    ! Local
    REAL(KIND=rlk)            :: w1,w2

    w1=y1-y2
    w2=x1-x2
    denom=w1*w1+w2*w2

  END FUNCTION denom

  PURE FUNCTION distpp(x3,y3,x4,y4,x1,y1)

    USE kinds_mod,ONLY: rlk

    ! Argument list
    REAL(KIND=rlk),INTENT(IN) :: x3,y3,x4,y4,x1,y1
    ! Result
    REAL(KIND=rlk)            :: distpp
    ! Local
    REAL(KIND=rlk)            :: w1,w2

    w1=0.5_rlk*(x3+x4)-x1
    w2=0.5_rlk*(y3+y4)-y1
    distpp=w1*w1+w2*w2

  END FUNCTION distpp  

  PURE FUNCTION distpl(x3,y3,x4,y4,x1,y1,x2,y2)

    USE kinds_mod,ONLY: rlk

    ! Argument list
    REAL(KIND=rlk),INTENT(IN) :: x3,y3,x4,y4,x1,y1,x2,y2
    ! Result
    REAL(KIND=rlk)            :: distpl

    distpl=0.5_rlk*(y1-y2)*(x3+x4)+0.5_rlk*(y3+y4)*(x2-x1)+y2*x1-y1*x2
    distpl=distpl*distpl

  END FUNCTION distpl

END MODULE geometry_mod
