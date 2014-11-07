
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

MODULE eos_mod

  IMPLICIT NONE

  PUBLIC :: getpre,getcc

CONTAINS

  PURE FUNCTION getpre(im,rr,ie)

    USE kinds_mod,      ONLY: ink,rlk
    USE integers_mod,   ONLY: eos_type
    USE reals_mod,      ONLY: eos_param,pcut

    INTEGER(KIND=ink),INTENT(IN) :: im
    REAL(KIND=rlk),   INTENT(IN) :: rr
    REAL(KIND=rlk),   INTENT(IN) :: ie
    ! Result
    REAL(KIND=rlk) :: getpre
    ! Local
    REAL(KIND=rlk) :: t1,t2,t3,t4,t5

    SELECT CASE(eos_type(im))
      CASE(0_ink) ! VOID
        getpre=eos_param(1,im)
      CASE(1_ink) ! IDEAL GAS
        getpre=ie*rr*(eos_param(1,im)-1.0_rlk)
      CASE(2_ink) ! TAIT
        t1=rr/eos_param(3,im)
        getpre=eos_param(1,im)*(t1**eos_param(2,im)-1.0_rlk)
        getpre=MAX(getpre,eos_param(4,im))
      CASE(3_ink) ! JWL
        t1=eos_param(4,im)*eos_param(6,im)/rr
        t2=eos_param(5,im)*eos_param(6,im)/rr
        t3=eos_param(1,im)*rr*ie
        t4=(1.0_rlk-eos_param(1,im)/t1)*eos_param(2,im)*EXP(-t1)
        t5=(1.0_rlk-eos_param(1,im)/t2)*eos_param(3,im)*EXP(-t2)
        getpre=t3+t4+t5
      CASE DEFAULT
        getpre=-1_rlk
    END SELECT
    IF (ABS(getpre).LT.pcut) getpre=0.0_rlk

  END FUNCTION getpre

  PURE FUNCTION getcc(im,rr,ie)

    USE kinds_mod,   ONLY: ink,rlk
    USE integers_mod,ONLY: eos_type
    USE reals_mod,   ONLY: eos_param

    INTEGER(KIND=ink),INTENT(IN) :: im
    REAL(KIND=rlk),   INTENT(IN) :: rr
    REAL(KIND=rlk),   INTENT(IN) :: ie
    ! Result
    REAL(KIND=rlk) :: getcc
    ! Local
    REAL(KIND=rlk) :: t1,t2,t3,t4,t5

    SELECT CASE(eos_type(im))
      CASE(0_ink) ! VOID
        getcc=1.0e-6_rlk
      CASE(1_ink) ! IDEAL GAS
        getcc=eos_param(1,im)*(eos_param(1,im)-1.0_rlk)*ie
      CASE(2_ink) ! TAIT
        t1=rr/eos_param(3,im)
        t2=eos_param(2,im)-1.0_rlk
        getcc=(eos_param(1,im)*eos_param(2,im))/eos_param(3,im)
        getcc=getcc*t1**t2
      CASE(3_ink) ! JWL
        t1=eos_param(6,im)/rr
        t2=getpre(im,rr,ie)
        t3=eos_param(4,im)*t1
        t4=eos_param(1,im)/eos_param(4,im)+eos_param(1,im)*t1-t3*t1
        t4=t4*eos_param(2,im)*EXP(-t3)
        t3=eos_param(5,im)*t1
        t5=eos_param(1,im)/eos_param(5,im)+eos_param(1,im)*t1-t3*t1
        t5=t5*eos_param(3,im)*EXP(-t3)
        getcc=eos_param(1,im)*t2/rr+eos_param(1,im)*ie-t4-t5
      CASE DEFAULT
        getcc=-1_rlk
    END SELECT

  END FUNCTION getcc

END MODULE eos_mod
