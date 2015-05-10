
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

MODULE ale_getmesh_mod

  IMPLICIT NONE

  PUBLIC :: alegetmesh

CONTAINS

  SUBROUTINE alegetmesh(nnod,indstatus)

    USE kinds_mod,    ONLY: ink,rlk
    USE logicals_mod, ONLY: zeul
    USE timing_mod,   ONLY: bookleaf_times
    USE typh_util_mod,ONLY: get_time

    ! Argument list
    INTEGER(KIND=ink),                INTENT(IN)  :: nnod
    INTEGER(KIND=ink),DIMENSION(nnod),INTENT(OUT) :: indstatus
    ! Local
    INTEGER(KIND=ink) :: inod
    REAL(KIND=rlk)    :: t0,t1

    ! Timer
    t0=get_time()

    ! Select mesh to be moved
    IF (zeul) THEN
      DO inod=1,nnod
        indstatus(inod)=2_ink
      ENDDO
    ELSE
    ENDIF

    ! Timing data
    t1=get_time()
    t1=t1-t0
    bookleaf_times%time_in_alegetmesh=bookleaf_times%time_in_alegetmesh+&
&                                     t1

  END SUBROUTINE alegetmesh

END MODULE ale_getmesh_mod
