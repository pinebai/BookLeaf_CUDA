
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

  USE kinds_mod,   ONLY: ink,rlk,lok
  USE integers_mod,ONLY: nel1,nnod1,nshape
  USE pointers_mod,ONLY: ndx,ndy,ielnod,ndu

  IMPLICIT NONE

  INTEGER(KIND=ink)                 :: iel,inod,ii
  REAL(KIND=rlk),   PARAMETER       :: TOL=1.0e-6_rlk,DX1=0.01_rlk,     &
&                                      DY1=0.01_rlk
  REAL(KIND=rlk)                    :: pi,x0,y0,w1,w2
  LOGICAL(KIND=lok),DIMENSION(nnod1):: zchanged

  ! Alter mesh positions
  pi=4.0_rlk*ATAN(1.0_rlk)
  zchanged(:)=.FALSE._lok
  DO iel=1,nel1
    DO ii=1,nshape
      inod=ielnod(ii,iel)
      IF (.NOT.zchanged(inod)) THEN
        x0=ndx(inod)
        y0=ndy(inod)
        w1=100.0_rlk*x0+TOL
        w2=100.0_rlk*y0+TOL
        ndx(inod)=w1*DX1+(10.0_rlk-w2)*DY1*SIN(pi*w1*0.01_rlk)
        zchanged(inod)=.TRUE._lok
      ENDIF
    ENDDO
  ENDDO

  ! Set left hand boundary velocity
  w1=ndx(1)
  DO inod=2,nnod1
    IF (ndx(inod).LT.w1) w1=ndx(inod)
  ENDDO
  w1=w1+TOL
  DO iel=1,nel1
    DO ii=1,nshape
      inod=ielnod(ii,iel)
      IF (ndx(inod).LE.w1) ndu(inod)=1.0_rlk
    ENDDO
  ENDDO

END SUBROUTINE modify
