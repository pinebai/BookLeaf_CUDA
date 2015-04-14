
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

MODULE getdt_mod

  IMPLICIT NONE

  PUBLIC :: getdt

CONTAINS

  SUBROUTINE getdt(dt)

    USE kinds_mod,       ONLY: rlk,ink
    USE reals_mod,       ONLY: ccut,zcut,cfl_sf,div_sf,dt_g,dt_min,     &
&                              dt_max
    USE integers_mod,    ONLY: nel,nshape,nnod,idtel
    USE logicals_mod,    ONLY: zdtnotreg,zmidlength
    USE paradef_mod,     ONLY: CommS,NProcW,zparallel
    USE pointers_mod,    ONLY: ielreg,rho,qq,csqrd,elx,ely,a1,a3,b1,b3, &
&                              ielnd,elvol,ndu,ndv
    USE scratch_mod,     ONLY: rscratch11,elu=>rscratch21,elv=>rscratch22
    USE geometry_mod,    ONLY: dlm,dln
    USE error_mod,       ONLY: halt
    USE utilities_mod,   ONLY: gather
    USE timing_mod,      ONLY: bookleaf_times
    USE TYPH_util_mod,   ONLY: get_time
    USE TYPH_Collect_mod,ONLY: TYPH_Gather

    ! Argument list
    REAL(KIND=rlk),INTENT(INOUT)         :: dt
    ! Local
    INTEGER(KIND=ink)                    :: iel,ireg,ii,ierr
    REAL(KIND=rlk)                       :: w1,w2,dt_cfl,dt_div,t0,t1,t2,t3
    REAL(KIND=rlk),DIMENSION(0:NprocW-1) :: dtt

    ! Timer
    t0=get_time()

    idtel=0
    ! CFL
    DO iel=1,nel
      ireg=ielreg(iel)
      IF (zdtnotreg(ireg)) THEN
        rscratch11(iel)=dt_max
      ELSE
        w1=MAX(rho(iel),zcut)
        w2=MAX(ccut,csqrd(iel))+2.0_rlk*qq(iel)/w1
        IF (zmidlength(ireg)) THEN
          w1=MINVAL(dlm(nshape,elx(:,iel),ely(:,iel)))
        ELSE
          w1=MINVAL(dln(nshape,elx(:,iel),ely(:,iel)))
        ENDIF
        rscratch11(iel)=w1/w2
      ENDIF
    ENDDO
    ii=1_ink
    DO iel=2,nel
      IF (rscratch11(iel).LT.rscratch11(ii)) ii=iel
    ENDDO
    w1=rscratch11(ii)
    IF (w1.LT.0.0_rlk) CALL halt("ERROR: dt_cfl < 0",1)
    dt_cfl=cfl_sf*SQRT(w1)
    idtel=ii
    ! Divergence
    w2=TINY(1.0_rlk)
    CALL gather(nshape,nel,nnod,ielnd(1,1),ndu(1),elv(1,1))
    CALL gather(nshape,nel,nnod,ielnd(1,1),ndv(1),elu(1,1))
    DO iel=1,nel
      w1=elu(1,iel)*(-b3(iel)+b1(iel))+elv(1,iel)*( a3(iel)-a1(iel))+   &
&        elu(2,iel)*( b3(iel)+b1(iel))+elv(2,iel)*(-a3(iel)-a1(iel))+   &
&        elu(3,iel)*( b3(iel)-b1(iel))+elv(3,iel)*(-a3(iel)+a1(iel))+   &
&        elu(4,iel)*(-b3(iel)-b1(iel))+elv(4,iel)*( a3(iel)+a1(iel))
      w1=ABS(w1)/elvol(iel)
      IF (w1.GT.w2) w2=w1
    ENDDO
    dt_div=div_sf/w2

    !# Missing code here that can't be merged

    ! Find smallest timestep
    dt=MIN(dt_cfl,dt_div,dt_g*dt,dt_max)
    IF (zparallel) THEN
      t2=get_time()
      ierr=TYPH_Gather(dt,dtt,comm=CommS)
      t3=get_time()
      t3=t3-t2
      bookleaf_times%time_in_colls=bookleaf_times%time_in_colls+t3
      dt=MINVAL(dtt)
    ENDIF

    ! Check minimum
    IF (dt.LT.dt_min) CALL halt("ERROR: dt < dt_min",1,.true.)

    ! Timing data
    t1=get_time()
    t1=t1-t0
    bookleaf_times%time_in_getdt=bookleaf_times%time_in_getdt+t1

  END SUBROUTINE getdt

END MODULE getdt_mod
