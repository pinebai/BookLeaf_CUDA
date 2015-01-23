
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

  SUBROUTINE alegetfvol(nshape,nnod,nel,dt,indstatus,ndx,ndy,ndux,ndvy, &
&                       rdelv)

    USE kinds_mod,   ONLY: ink,rlk
    USE logicals_mod,ONLY: zeul

    ! Argument list
    INTEGER(KIND=ink),                      INTENT(IN)    :: nshape,    &
&                                                            nnod,nel
    REAL(KIND=rlk),                         INTENT(IN)    :: dt
    INTEGER(KIND=ink),DIMENSION(nnod),      INTENT(IN)    :: indstatus
    REAL(KIND=rlk),   DIMENSION(nnod),      INTENT(INOUT) :: ndx,ndy,   &
&                                                            ndux,ndvy
    REAL(KIND=rlk),   DIMENSION(nshape,nel),INTENT(OUT)   :: rdelv
    ! Local
    INTEGER(KIND=ink) :: iNd

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
    CALL fvol(nshape,nnod,nel,ndx(1),ndy(1),ndux(1),ndvy(1),rDelV(1,1)) 

    ! update position
    DO iNd=1,nNod
      ndx(iNd)=ndux(iNd)
      ndy(iNd)=ndvy(iNd)
    ENDDO

  END SUBROUTINE alegetfvol

  SUBROUTINE fvol(nshape,nnod,nel,ndx0,ndy0,ndx1,ndy1,rdelv)

    ! Argument list
    INTEGER(KIND=ink),                      INTENT(IN)  :: nshape,nnod, &
&                                                          nel
    REAL(KIND=rlk),   DIMENSION(nnod),      INTENT(IN)  :: ndx0,ndy0,   &
&                                                          ndx1,ndy1
    REAL(KIND=rlk),   DIMENSION(nshape,nel),INTENT(OUT) :: rdelv

    ! initialise
    rdelv=0.0_rlk

    ! construct volumes
    END SUBROUTINE fvol

END MODULE ale_getfvol_mod
