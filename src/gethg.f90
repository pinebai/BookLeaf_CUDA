
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

MODULE gethg_mod

  IMPLICIT NONE

  PUBLIC :: gethg

CONTAINS

  SUBROUTINE gethg(nshape,nel,dt,rho,elu,elv,elfx,elfy)

    USE kinds_mod,    ONLY: ink,rlk
    USE reals_mod,    ONLY: kappareg
    USE pointers_mod, ONLY: ielreg,area=>elvol
    USE timing_mod,   ONLY: bookleaf_times
    USE TYPH_util_mod,ONLY: get_time

    ! Argument list
    INTEGER(KIND=ink),                   INTENT(IN)    :: nel,nshape
    REAL(KIND=rlk),                      INTENT(IN)    :: dt
    REAL(KIND=rlk),DIMENSION(nel),       INTENT(IN)    :: rho
    REAL(KIND=rlk),DIMENSION(nshape,nel),INTENT(IN)    :: elu,elv
    REAL(KIND=rlk),DIMENSION(nshape,nel),INTENT(INOUT) :: elfx,elfy
    ! Local
    INTEGER(KIND=ink)                                  :: iel,ireg
    REAL(KIND=rlk)                                     :: w1,w2,w3,t0,t1

    ! Timer
    t0=get_time()

    !# Missing code here that can't be merged
    ! Add hourglass restoring force
    DO iel=1,nel
      w2=elu(1,iel)-elu(2,iel)+elu(3,iel)-elu(4,iel)
      w3=elv(1,iel)-elv(2,iel)+elv(3,iel)-elv(4,iel)
      ireg=ielreg(iel)
      w1=-kappareg(ireg)*rho(iel)*area(iel)/dt
      w2=w1*w2
      w3=w1*w3
      elfx(1,iel)=elfx(1,iel)+w2
      elfx(2,iel)=elfx(2,iel)-w2
      elfx(3,iel)=elfx(3,iel)+w2
      elfx(4,iel)=elfx(4,iel)-w2
      elfy(1,iel)=elfy(1,iel)+w3
      elfy(2,iel)=elfy(2,iel)-w3
      elfy(3,iel)=elfy(3,iel)+w3
      elfy(4,iel)=elfy(4,iel)-w3
    ENDDO

    ! Timing data
    t1=get_time()
    t1=t1-t0
    bookleaf_times%time_in_gethg=bookleaf_times%time_in_gethg+t1

  END SUBROUTINE gethg

END MODULE gethg_mod
