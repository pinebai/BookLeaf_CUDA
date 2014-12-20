
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
  USE integers_mod,ONLY: nel
  USE paradef_mod, ONLY: e_loc_glob,zparallel
  USE pointers_mod,ONLY: ein,ielmat,rho,pre,csqrd,elvol
  USE getpc_mod,   ONLY: getpc

  IMPLICIT NONE

  INTEGER(KIND=ink) :: ii,iel

  DO ii=1,nel
    IF (zparallel) THEN
      iel=e_loc_glob(ii)
    ELSE
      iel=ii
    ENDIF
    IF (iel.EQ.1_ink) THEN
      ein(ii)=0.4935932_rlk/(2.0_rlk*elvol(ii))
      CALL getpc(nel,ielmat(ii),rho(ii),ein(ii),pre(ii),csqrd(ii))
      EXIT
    ENDIF
  ENDDO

END SUBROUTINE modify
