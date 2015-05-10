
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

MODULE ale_getfvol_mod

  USE kinds_mod,ONLY: ink,rlk

  IMPLICIT NONE

  PRIVATE :: fvol
  PUBLIC  :: alegetfvol

CONTAINS

  SUBROUTINE alegetfvol(nshape,nnod,nel,dt,cut,indstatus,ielnd,ndx,ndy, &
&                       ndux,ndvy,rdelv)

    USE kinds_mod,    ONLY: ink,rlk
    USE logicals_mod, ONLY: zeul
    USE timers_mod,   ONLY: bookleaf_times
    USE typh_util_mod,ONLY: get_time

    ! Argument list
    INTEGER(KIND=ink),                      INTENT(IN)    :: nshape,    &
&                                                            nnod,nel
    REAL(KIND=rlk),                         INTENT(IN)    :: dt,cut
    INTEGER(KIND=ink),DIMENSION(nnod),      INTENT(IN)    :: indstatus
    INTEGER(KIND=ink),DIMENSION(nshape,nel),INTENT(IN)    :: ielnd
    REAL(KIND=rlk),   DIMENSION(nnod),      INTENT(INOUT) :: ndx,ndy,   &
&                                                            ndux,ndvy
    REAL(KIND=rlk),   DIMENSION(nshape,nel),INTENT(OUT)   :: rdelv
    ! Local
    INTEGER(KIND=ink) :: iNd
    REAL(KIND=rlk)    :: t0,t1

    ! Timer
    t0=get_time()

    ! calculate mesh velocity
    IF (zeul) THEN
      ndux=-ndux
      ndvy=-ndvy
    ELSE
    ENDIF

    ! construct new position
    DO iNd=1,nNod
      ndux(iNd)=ndx(iNd)+dt*ndux(iNd)
      ndvy(iNd)=ndy(iNd)+dt*ndvy(iNd)
    ENDDO

    ! construct flux volumes
    CALL fvol(nshape,nnod,nel,cut,ielnd(1,1),ndx(1),ndy(1),ndux(1),     &
&             ndvy(1),rDelV(1,1)) 

    ! update position
    DO iNd=1,nNod
      ndx(iNd)=ndux(iNd)
      ndy(iNd)=ndvy(iNd)
    ENDDO

    ! Timing data
    t1=get_time()
    t1=t1-t0
    bookleaf_times%time_in_alegetfvol=bookleaf_times%time_in_alegetfvol+&
&                                     t1

  END SUBROUTINE alegetfvol

  SUBROUTINE fvol(nshape,nnod,nel,cut,ielnd,ndx0,ndy0,ndx1,ndy1,rdelv)

    USE kinds_mod,ONLY: ink,rlk

    ! Argument list
    INTEGER(KIND=ink),                      INTENT(IN)  :: nshape,nnod, &
&                                                          nel
    REAL(KIND=rlk),                         INTENT(IN)  :: cut
    INTEGER(KIND=ink),DIMENSION(nshape,nel),INTENT(IN)  :: ielnd
    REAL(KIND=rlk),   DIMENSION(nnod),      INTENT(IN)  :: ndx0,ndy0,   &
&                                                          ndx1,ndy1
    REAL(KIND=rlk),   DIMENSION(nshape,nel),INTENT(OUT) :: rdelv
    ! Local
    INTEGER(KIND=ink) :: iel,jj,jp,n1,n2
    REAL(KIND=rlk)    :: x1,x2,x3,x4,y1,y2,y3,y4,a1,a3,b1,b3

    ! initialise
    rdelv=0.0_rlk

    ! construct volumes
    DO iel=1,nel
      DO jj=1,nshape
        jp=MOD(jj,nshape)+1_ink
        n1=ielnd(jj,iel)
        n2=ielnd(jp,iel)
        x1=ndx0(n1)
        x2=ndx0(n2)
        y1=ndy0(n1)
        y2=ndy0(n2)
        x3=ndx1(n2)
        x4=ndx1(n1)
        y3=ndy1(n2)
        y4=ndy1(n1)
        a1=0.25_rlk*(-x1+x2+x3-x4)
        a3=0.25_rlk*(-x1-x2+x3+x4)
        b1=0.25_rlk*(-y1+y2+y3-y4)
        b3=0.25_rlk*(-y1-y2+y3+y4)
        rdelv(jj,iel)=4.0_rlk*(a1*b3-a3*b1)
        IF (rdelv(jj,iel).LT.cut) rdelv(jj,iel)=0.0_rlk
      ENDDO
    ENDDO

    END SUBROUTINE fvol

END MODULE ale_getfvol_mod
