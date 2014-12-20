
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

MODULE getforce_mod

  IMPLICIT NONE

  PUBLIC :: getforce

CONTAINS

  SUBROUTINE getforce(nshape,nel,dt,elx,ely,elu,elv,elfx,elfy,pre,rho,  &
&                     zflag)

    USE kinds_mod,    ONLY: ink,rlk,lok
    USE logicals_mod, ONLY: zhg,zsp
    USE pointers_mod, ONLY: a1,a3,b1,b3,qx,qy
    USE gethg_mod,    ONLY: gethg
    USE getsp_mod,    ONLY: getsp
    USE timing_mod,   ONLY: bookleaf_times
    USE TYPH_util_mod,ONLY: get_time

    ! Argument list
    INTEGER(KIND=ink),                   INTENT(IN)  :: nshape,nel
    REAL(KIND=rlk),                      INTENT(IN)  :: dt
    REAL(KIND=rlk),DIMENSION(nshape,nel),INTENT(IN)  :: elx,ely,elu,elv
    REAL(KIND=rlk),DIMENSION(nshape,nel),INTENT(OUT) :: elfx,elfy
    REAL(KIND=rlk),DIMENSION(nel),       INTENT(IN)  :: pre,rho
    LOGICAL(KIND=lok),                   INTENT(IN)  :: zflag
    ! Local
    INTEGER(KIND=ink),PARAMETER                      :: istrip=48_ink
    INTEGER(KIND=ink)                                :: i0,i1,jj,jp,iel
    REAL(KIND=rlk)                                   :: w1,t0,t1

    ! Timer
    t0=get_time()

    ! Pressure force
    DO i0=1,nel,istrip
      i1=MIN(nel,i0+istrip-1)
      DO iel=i0,i1
        w1=pre(iel)
        elfx(1,iel)=w1*(-b3(iel)+b1(iel))
        elfx(2,iel)=w1*( b3(iel)+b1(iel))
        elfx(3,iel)=w1*( b3(iel)-b1(iel))
        elfx(4,iel)=w1*(-b3(iel)-b1(iel))
        elfy(1,iel)=w1*( a3(iel)-a1(iel))
        elfy(2,iel)=w1*(-a3(iel)-a1(iel))
        elfy(3,iel)=w1*(-a3(iel)+a1(iel))
        elfy(4,iel)=w1*( a3(iel)+a1(iel))
      ENDDO
    ENDDO

    ! Artificial viscosity force
    !# Missing code here that can't be merged
    DO i0=1,nel,istrip
      i1=MIN(nel,i0+istrip-1)
      DO iel=i0,i1
        DO jj=1,nshape
          jp=jj+1_ink
          IF (jp.GT.4_ink) jp=1_ink
          elfx(jj,iel)=elfx(jj,iel)+qx(jj,iel)
          elfx(jp,iel)=elfx(jp,iel)-qx(jj,iel)
          elfy(jj,iel)=elfy(jj,iel)+qy(jj,iel)
          elfy(jp,iel)=elfy(jp,iel)-qy(jj,iel)
        ENDDO
      ENDDO
    ENDDO

    ! Subzonal pressure force
    IF (zsp) CALL getsp(nshape,nel,rho,elx,ely,elfx,elfy)

    !# Missing code here that can't be merged
    IF (zflag) THEN
      !# Missing code here that can't be merged
      ! Anti-hourglass force
      IF (zhg) CALL gethg(nshape,nel,dt,rho,elu,elv,elfx,elfy)
    ENDIF

    ! Timing data
    t1=get_time()
    t1=t1-t0
    bookleaf_times%time_in_getfrc=bookleaf_times%time_in_getfrc+t1

  END SUBROUTINE getforce

END MODULE getforce_mod  
