
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


SUBROUTINE modify()

  USE kinds_mod,   ONLY: ink,rlk
  USE pointers_mod,ONLY: ndx,ndy,ndu,ndv,indtype
  USE integers_mod,ONLY: nnod1

  IMPLICIT NONE

  INTEGER(KIND=ink) :: inod
  REAL(KIND=rlk)    :: w1,w2,w3

  DO inod=1,nnod1
    IF (indtype(inod).NE.-3_ink) THEN
      w1=ndx(inod)
      w2=ndy(inod)
      w3=1.0_rlk/SQRT(w1*w1+w2*w2)
      ndu(inod)=-w1*w3
      ndv(inod)=-w2*w3
    ENDIF
  ENDDO

END SUBROUTINE modify

SUBROUTINE mesh_modify()
END SUBROUTINE mesh_modify

