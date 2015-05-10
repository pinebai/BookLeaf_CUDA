
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

MODULE alestep_mod

  IMPLICIT NONE

  PUBLIC :: alestep

CONTAINS

  SUBROUTINE alestep(nstep,dt)

    USE kinds_mod,      ONLY: rlk,ink
    USE integers_mod,   ONLY: nnod,nel,nshape,nsz,adv_type
    USE reals_mod,      ONLY: zerocut,dencut
    USE error_mod,      ONLY: halt
    USE ale_getmesh_mod,ONLY: alegetmesh
    USE ale_getfvol_mod,ONLY: alegetfvol
    USE ale_advect_mod, ONLY: aleadvect
    USE ale_update_mod, ONLY: aleupdate
    USE pointers_mod,   ONLY: ndx,ndy,elx,ely,elmass,rho,pre,ein,csqrd, &
&                             elvol,ielmat,cnwt,cnmass,ielel,ielsd,     &
&                             ielnd,indtype
    USE scratch_mod,    ONLY: store1=>rscratch11,store2=>rscratch12,    &
&                             store3=>rscratch13,store4=>rscratch14,    &
&                             store5=>rscratch15,store6=>rscratch16,    &
&                             cnms=>rscratch21,rDelV=>rscratch22,       &
&                             rDelM=>rscratch23,rFlux=>rscratch24,      &
&                             rwork1=>rscratch25,rwork2=>rscratch26,    &
&                             rwork3=>rscratch27,indstatus=>iscratch11, &
&                             zactive=>zscratch11
    USE timing_mod,     ONLY: bookleaf_times
    USE typh_util_mod,  ONLY: get_time

    ! Argument list
    INTEGER(KIND=ink),INTENT(IN) :: nstep
    REAL(KIND=rlk),   INTENT(IN) :: dt
    ! Local
    INTEGER(KIND=ink) :: ii,i1,i2,i3
    REAL(KIND=rlk)    :: t0,t1

    ! Timer
    t0=get_time()

    ! select mesh to be moved
    CALL alegetmesh(nnod,indstatus(1))

    ! calculate flux volume
    CALL alegetfvol(nshape,nnod,nel,dt,zerocut,indstatus(1),ielnd(1,1), &
&                   ndx(1),ndy(1),store4(1),store5(1),rDelV(1,1))

    ! advect independent variables
    SELECT CASE(adv_type)
      CASE(1_ink)
        CALL aleadvect(1_ink,2_ink,nshape,nel,nnod,nsz,ielel(1,1),      &
&                      ielsd(1,1),ielnd(1,1),indstatus(1),indtype(1),   &
&                      dencut,zerocut,store5(1),store6(1),store1(1),    &
&                      store2(1),store3(1),store4(1),elvol(1),elmass(1),&
&                      rho(1),cnwt(1,1),cnmass(1,1),rDelV(1,1),         &
&                      rDelM(1,1),rwork3(1,1),rFlux(1,1),rwork1(1,1),   &
&                      rwork2(1,1),zactive(1))
      CASE(2_ink)
        ii=MOD(nstep+1,2)
        i1=1_ink+ii
        i2=2_ink-ii
        i3=i2-i1
        DO ii=i1,i2,i3
          CALL aleadvect(ii,ii,nshape,nel,nnod,nsz,ielel(1,1),          &
&                        ielsd(1,1),ielnd(1,1),indstatus(1),indtype(1), &
&                        dencut,zerocut,store5(1),store6(1),store1(1),  &
&                        store2(1),store3(1),store4(1),elvol(1),        &
&                        elmass(1),rho(1),cnwt(1,1),cnms(1,1),          &
&                        rDelV(1,1),rDelM(1,1),rwork3(1,1),rFlux(1,1),  &
&                        rwork1(1,1),rwork2(1,1),zactive(1))
        ENDDO
      CASE DEFAULT
        CALL halt("ERROR: unrecognised adv_type",0)
    END SELECT

    ! update dependent variables
    CALL aleupdate(nshape,nel,nnod,ndx(1),ndy(1),elx(1,1),ely(1,1),     &
&                  elmass(1),rho(1),pre(1),ein(1),csqrd(1),ielmat(1))

    ! Timing data
    t1=get_time()
    t1=t1-t0
    bookleaf_times%time_in_alestep=bookleaf_times%time_in_alestep+t1

  END SUBROUTINE alestep

END MODULE alestep_mod
