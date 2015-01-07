
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

MODULE ale_getvel_mod

  IMPLICIT NONE

  PUBLIC :: alegetvel

CONTAINS

  SUBROUTINE alegetvel(nnod,ndum,ndvm)

    ! Argument list
    INTEGER(KIND=ink),                INTENT(IN)    :: nnod
    REAL(KIND=rlk),   DIMENSION(nnod),INTENT(INOUT) :: ndum,ndvm

    IF (zeul) THEN
    ELSE
    ENDIF

  END SUBROUTINE alegetvel

END MODULE ale_getvel_mod
