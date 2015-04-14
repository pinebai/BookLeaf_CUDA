
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

MODULE getpc_mod

  IMPLICIT NONE

  PUBLIC :: getpc

CONTAINS

  SUBROUTINE getpc(nel,ielmat,rho,ein,pre,csqrd)

    USE kinds_mod,    ONLY: ink,rlk
    USE eos_mod,      ONLY: getpre,getcc
    USE timing_mod,   ONLY: bookleaf_times
    USE TYPH_util_mod,ONLY: get_time

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)  :: nel
    INTEGER(KIND=ink),DIMENSION(nel),INTENT(IN)  :: ielmat
    REAL(KIND=rlk),   DIMENSION(nel),INTENT(IN)  :: rho,ein
    REAL(KIND=rlk),   DIMENSION(nel),INTENT(OUT) :: pre,csqrd
    ! Local
    INTEGER(KIND=ink)                            :: iel
    REAL(KIND=rlk)                               :: t0,t1

    ! Timer
    t0=get_time()

    !# Missing code here that can't be merged

    ! update pressure and sound speed
    DO iel=1,nel
      pre(iel)=getpre(ielmat(iel),rho(iel),ein(iel))
      csqrd(iel)=getcc(ielmat(iel),rho(iel),ein(iel))
    ENDDO
    ! correct negative sound speeds
    csqrd=MERGE(csqrd,0.0_rlk,csqrd.GT.0.0_rlk)

    !# Missing code here that can't be merged

    ! Timing data
    t1=get_time()
    t1=t1-t0
    bookleaf_times%time_in_eos=bookleaf_times%time_in_eos+t1

  END SUBROUTINE getpc

END MODULE getpc_mod  
