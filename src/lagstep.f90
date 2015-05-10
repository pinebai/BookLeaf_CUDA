
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

MODULE lagstep_mod

  IMPLICIT NONE

  PUBLIC :: lagstep

CONTAINS

  SUBROUTINE lagstep(dt)

    USE kinds_mod,    ONLY: rlk,ink,lok
    USE integers_mod, ONLY: nel,nnod,nshape,nel1,nnod1
    USE geometry_mod, ONLY: getgeom
    USE pointers_mod, ONLY: rho,elmass,elvol,ielmat,ein,pre,csqrd,      &
&                           ndx,ndy,elx,ely,ndu,ndv,ielnd
    USE getacc_mod,   ONLY: getacc
    USE getq_mod,     ONLY: getq
    USE getpc_mod,    ONLY: getpc
    USE getforce_mod, ONLY: getforce
    USE getein_mod,   ONLY: getein
    USE utilities_mod,ONLY: gather
    USE scratch_mod,  ONLY: elu=>rscratch21,elv=>rscratch22,            &
&                           elfx=>rscratch23,elfy=>rscratch24,          &
&                           rho05=>rscratch11,ein05=>rscratch12,        &
&                           pre05=>rscratch13,ndxu=>rscratch14,         &
&                           ndyv=>rscratch15,dx=>rscratch25,            &
&                           dy=>rscratch26,scratch=>rscratch27
    USE timing_mod,   ONLY: timer=>bookleaf_times
    USE TYPH_util_mod,ONLY: get_time

    ! Argument list
    REAL(KIND=rlk),INTENT(IN) :: dt
    ! Local
    INTEGER(KIND=ink)         :: iel,inod
    REAL(KIND=rlk)            :: dt05, t0, t1

    ! Timer
    t0=get_time()

    ! ##############
    ! Predictor
    ! ##############
    dt05=0.5_rlk*dt
    CALL gather(nshape,nel,nnod,ielnd(1,1),ndu(1),elu(1,1))
    CALL gather(nshape,nel,nnod,ielnd(1,1),ndv(1),elv(1,1))
    ! Artificial viscosity
    CALL getq(nshape,nel,elx(1,1),ely(1,1),elu(1,1),elv(1,1),rho(1),    &
&             pre(1),dx,dy,elfx,elfy,scratch)
    ! Force
    CALL getforce(nshape,nel,dt05,elx(1,1),ely(1,1),elu(1,1),elv(1,1),  &
&                 elfx(1,1),elfy(1,1),pre(1),rho(1),.FALSE._lok)
    ! Half step positions
    DO inod=1,nnod
      ndxu(inod)=ndx(inod)+dt05*ndu(inod)
      ndyv(inod)=ndy(inod)+dt05*ndv(inod)
    ENDDO
    !# Missing code here that can't be merged
    ! Update geometry and iso-parametric terms
    CALL getgeom(nshape,nel,nnod,ndxu(1),ndyv(1),elx(1,1),ely(1,1),     &
&                timer%time_in_getgeoml)
    !# Missing code here that can't be merged
    ! Half step density
    DO iel=1,nel
      rho05(iel)=elmass(iel)/elvol(iel)
    ENDDO
    ! Half step internal energy
    CALL getein(nshape,nel,ein05(1),dt05,elfx(1,1),elfy(1,1),elu(1,1),  &
&               elv(1,1))
    !# Missing code here that can't be merged
    ! Half step pressure
    CALL getpc(nel,ielmat(1),rho05(1),ein05(1),pre05(1),csqrd(1),       &
&              timer%time_in_getpcl)
    !# Missing code here that can't be merged

    ! ###############
    ! Corrector
    ! ###############
    ! Artificial viscosity
    CALL getq(nshape,nel,elx(1,1),ely(1,1),elu(1,1),elv(1,1),rho05(1),  &
&             pre05(1),dx,dy,elfx,elfy,scratch)    
    ! Force
    CALL getforce(nshape,nel,dt,elx(1,1),ely(1,1),elu(1,1),elv(1,1),    &
&                 elfx(1,1),elfy(1,1),pre05(1),rho05(1),.TRUE._lok)
    ! Acceleration
    CALL getacc(nshape,nel,nel1,nnod,nnod1,elfx(1,1),elfy(1,1),ndxu(1), &
&               ndyv(1),elu(1,1),elv(1,1),rho05(1),dt05,dt)
    ! Update geometry and iso-parametric terms
    CALL getgeom(nshape,nel,nnod,ndx(1),ndy(1),elx(1,1),ely(1,1),       &
&                timer%time_in_getgeoml)
    !# Missing code here that can't be merged
    ! Full step density
    DO iel=1,nel
      rho(iel)=elmass(iel)/elvol(iel)
    ENDDO
    ! full step internal energy update
    CALL getein(nshape,nel,ein(1),dt,elfx(1,1),elfy(1,1),elu(1,1),      &
&               elv(1,1))
    !# Missing code here that can't be merged
    ! Full step pressure
    CALL getpc(nel,ielmat(1),rho(1),ein(1),pre(1),csqrd(1),             &
&              timer%time_in_getpcl)
    !# Missing code here that can't be merged

    ! Timing data
    t1=get_time()
    t1=t1-t0
    timer%time_in_lag=timer%time_in_lag+t1

  END SUBROUTINE lagstep

END MODULE lagstep_mod
