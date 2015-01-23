
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
    USE integers_mod,   ONLY: nnod,nel,nshape
    USE error_mod,      ONLY: halt
    USE ale_getmesh_mod,ONLY: alegetmesh
    USE ale_getfvol_mod,ONLY: alegetfvol
    USE ale_advect_mod, ONLY: aleadvect
    USE ale_update_mod, ONLY: aleupdate
    USE pointers_mod,   ONLY: ndx,ndy,elx,ely,elmass,rho,pre,ein,csqrd, &
&                             ielmat,cnwt,ielel,ielsd
    USE scratch_mod,    ONLY: storev=>rscratch11,storem=>rscratch12,    &
&                             storer=>rscratch13,ndumcut=>rscratch14,   &
&                             ndvmcut=>rscratch15,cnms=>rscratch21,     &
&                             rDelV=>rscratch22,rDelM=>rscratch23,      &
&                             rFlux=>rscratch24,rwork1=>rscratch25,     &
&                             rwork2=>rscratch26,indstatus=>iscratch11

    ! Argument list
    INTEGER(KIND=ink),INTENT(IN) :: nstep
    REAL(KIND=rlk),   INTENT(IN) :: dt
    ! Local
    INTEGER(KIND=ink) :: ii,i1,i2,i3

    ! select mesh to be moved
    CALL alegetmesh(nnod,indstatus(1))

    ! calculate flux volume
    CALL alegetfvol(nshape,nnod,nel,dt,indstatus(1),ndx(1),ndy(1),      &
&                   ndumcut(1),ndvmcut(1),rDelV(1,1))

    ! advect independent variables
!    SELECT CASE(iadv_type)
!      CASE(1_ink)
        CALL aleadvect(1_ink,2_ink,nshape,nel,nnod,storev(1),storem(1), &
&                      storer(1),elmass(1),rho(1),ndumcut(1),ndvmcut(1),&
&                      cnwt(1,1),cnms(1,1),rDelV(1,1),rDelM(1,1),       &
&                      rFlux(1,1),ielel(1,1),ielsd(1,1),rwork1(1,1),    &
&                      rwork2(1,1))
!      CASE(2_ink)
        ii=MOD(nstep+1,2)
        i1=1_ink+ii
        i2=2_ink-ii
        i3=i2-i1
        DO ii=i1,i2,i3
          CALL aleadvect(ii,ii,nshape,nel,nnod,storev(1),storem(1),     &
&                        storer(1),elmass(1),rho(1),ndumcut(1),         &
&                        ndvmcut(1),cnwt(1,1),cnms(1,1),rDelV(1,1),     &
&                        rDelM(1,1),rFlux(1,1),ielel(1,1),ielsd(1,1),   &
&                        rwork1(1,1),rwork2(1,1))
        ENDDO
!      CASE DEFAULT
        CALL halt("ERROR: unrecognised iadv_type",0)
!    END SELECT

    ! update dependent variables
    CALL aleupdate(nshape,nel,nnod,ndx(1),ndy(1),elx(1,1),ely(1,1),     &
&                  elmass(1),rho(1),pre(1),ein(1),csqrd(1),ielmat(1))

  END SUBROUTINE alestep

END MODULE alestep_mod
