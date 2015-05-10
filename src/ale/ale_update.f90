
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

MODULE ale_update_mod

  IMPLICIT NONE

  PUBLIC :: aleupdate

CONTAINS

  SUBROUTINE aleupdate(nshape,nel,nnod,ndx,ndy,elx,ely,elmass,rho,pre,  &
&                      ein,csqrd,ielmat)    

    USE kinds_mod,    ONLY: ink,rlk
    USE pointers_mod, ONLY: elvol
    USE geometry_mod, ONLY: getgeom
    USE getpc_mod,    ONLY: getpc
    USE timing_mod,   ONLY: timer=>bookleaf_times
    USE typh_util_mod,ONLY: get_time

    ! Argument list
    INTEGER(KIND=ink),                      INTENT(IN)  :: nshape,nel,  &
&                                                          nnod
    REAL(KIND=rlk),   DIMENSION(nnod),      INTENT(IN)  :: ndx,ndy
    REAL(KIND=rlk),   DIMENSION(nshape,nel),INTENT(OUT) :: elx,ely
    REAL(KIND=rlk),   DIMENSION(nel),       INTENT(IN)  :: elmass,ein
    REAL(KIND=rlk),   DIMENSION(nel),       INTENT(OUT) :: rho,pre,csqrd
    INTEGER(KIND=ink),DIMENSION(nel),       INTENT(IN)  :: ielmat
    ! Local
    INTEGER(KIND=ink) :: iel
    REAL(KIND=rlk)    :: t0,t1

    ! Timer
    t0=get_time()

    ! update geometry
    CALL getgeom(nshape,nel,nnod,ndx,ndy,elx,ely,timer%time_in_getgeoma)

    ! update density to be consistent with geometry
    DO iel=1,nel
      rho(iel)=elmass(iel)/elvol(iel)
    ENDDO

    ! update EoS
    CALL getpc(nel,ielmat(1),rho(1),ein(1),pre(1),csqrd(1),             &
&              timer%time_in_getpca)

    ! Timing data
    t1=get_time()
    t1=t1-t0
    timer%time_in_aleupdate=timer%time_in_aleupdate+t1

  END SUBROUTINE aleupdate

END MODULE ale_update_mod
