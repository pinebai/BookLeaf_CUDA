
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
  USE pointers_mod,ONLY: ndx,ndy,ielmat,ielnd,rho,pre,ein,elvol,cnwt,   &
&                        elmass,cnmass,spmass
  USE integers_mod,ONLY: nel,nnod
  USE reals_mod,   ONLY: eos_param
  USE logicals_mod,ONLY: zsp

  ! Local
  INTEGER(KIND=ink) :: inod,iel,ii,n1,n2,n3,n4
  REAL(KIND=rlk)    :: x1,x2,x3,x4,y1,y2,y3,y4,w1,w2,w3,w4,xmid

  ! find mid-point
  x1=ndx(1)
  x2=x1
  DO inod=1,nnod
    IF (ndx(inod).LT.x1) x1=ndx(inod)
    IF (ndx(inod).GT.x2) x2=ndx(inod)
  ENDDO
  xmid=0.5_rlk*(x1+x2)

  ! reset variables
  DO iel=1,nel
    x1=ndx(ielnd(1,iel))
    x2=ndx(ielnd(2,iel))
    x3=ndx(ielnd(3,iel))
    x4=ndx(ielnd(4,iel))
    IF ((0.25_rlk*(x1+x2+x3+x4)).LT.xmid) THEN
      ielmat(iel)=1_ink
      rho(iel)=1.0_rlk
      pre(iel)=1.0_rlk
    ELSE
      ielmat(iel)=2_ink
      rho(iel)=0.125_rlk
      pre(iel)=0.1_rlk
    ENDIF
    ein(iel)=pre(iel)/(rho(iel)*(eos_param(1,ielmat(iel))-1.0_rlk))
    elmass(iel)=rho(iel)*elvol(iel)
    cnmass(1:4,iel)=rho(iel)*cnwt(1:4,iel)
  ENDDO

  ! reset subzonal pressure mass
  IF (zsp) THEN
    DO iel=1,nel
      n1=ielnd(1,iel)
      n2=ielnd(2,iel)
      n3=ielnd(3,iel)
      n4=ielnd(4,iel)
      x3=0.25_rlk*(ndx(n1)+ndx(n2)+ndx(n3)+ndx(n4))
      y3=0.25_rlk*(ndy(n1)+ndy(n2)+ndy(n3)+ndy(n4))
      DO inod=1,4
        x1=ndx(ielnd(inod,iel))
        y1=ndy(ielnd(inod,iel))
        ii=MOD(inod,4)+1_ink
        x2=0.5_rlk*(x1+ndx(ielnd(ii,iel)))
        y2=0.5_rlk*(y1+ndy(ielnd(ii,iel)))
        ii=MOD(inod+2,4)+1_ink
        x4=0.5_rlk*(x1+ndx(ielnd(ii,iel)))
        y4=0.5_rlk*(y1+ndy(ielnd(ii,iel)))
        w1=-x1+x2+x3-x4
        w2=-x1-x2+x3+x4
        w3=-y1+y2+y3-y4
        w4=-y1-y2+y3+y4
        spmass(inod,iel)=rho(iel)*(w1*w4-w2*w3)
      ENDDO
    ENDDO
  ENDIF
  
END SUBROUTINE modify

