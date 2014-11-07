
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

SUBROUTINE mesh_modify()
  USE kinds_mod,   ONLY: ink,rlk
  USE integers_mod,ONLY: nel,nmat,nreg
  USE pointers_mod,ONLY: ielmat,ielreg,indtype
  USE mesh_mod,    ONLY: reg
  IMPLICIT NONE
  INTEGER(KIND=ink) :: ii,no_l,no_k
  INTEGER(KIND=ink) :: l1,l2,k1,k2,kk,ll

  nmat=2_ink
  nreg=2_ink
  ielmat((nel/2_ink)+1_ink:nel) = 2_ink
  ielreg((nel/2_ink)+1_ink:nel) = 2_ink

  no_l=reg(1)%dim(1)+1_ink
  no_k=reg(1)%dim(2)+1_ink
  l1=1_ink
  l2=no_l
  k1=no_k/2_ink +1_ink
  k2=no_k-1_ink

  ii=no_l*(no_k/2_ink)
  DO kk=k1,k2
    DO ll=l1,l2
      ii=ii+1
      IF (ll.NE.1 .AND. ll.NE.no_l) THEN
        indtype(ii)=2
      ENDIF
    ENDDO
  ENDDO
END SUBROUTINE mesh_modify

SUBROUTINE modify()
END SUBROUTINE modify

