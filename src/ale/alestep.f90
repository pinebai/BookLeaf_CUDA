
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

MODULE alestep_mod

  IMPLICIT NONE

  PUBLIC :: alestep

CONTAINS

  SUBROUTINE alestep(dt)

    USE kinds_mod,      ONLY: rlk
    USE ale_getmesh_mod,ONLY: alegetmesh
    USE ale_getvel_mod, ONLY: alegetvel
    USE ale_advect_mod, ONLY: aleadvect
    USE ale_update_mod, ONLY: aleupdate
    USE scratch_mod,    ONLY: ndum=>rscratch14,ndvm=>rscratch15

    ! Argument list
    REAL(KIND=rlk),INTENT(IN) :: dt

    ! select mesh to be moved
    CALL alegetmesh(indstatus)

    ! calculate mesh velocity
    CALL alegetvel(ndum(1),ndvm(1))

    ! advect independent variables
    CALL aleadvect()

    ! update dependent variables
    CALL aleupdate()

  END SUBROUTINE alestep

END MODULE alestep_mod
