
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
    USE reals_mod,       ONLY: ccut,zcut,cfl_sf,div_sf,ale_sf,dt_g,     &
&                              dt_min,dt_max,zerocut
    USE integers_mod,    ONLY: nel,nshape,nnod,idtel,idtreg,comms,nprocw
    USE logicals_mod,    ONLY: zdtnotreg,zmidlength,zparallel,zeul,zale,&
&                              zaleon
    USE strings_mod,     ONLY: sdt
    USE pointers_mod,    ONLY: ielreg,rho,qq,csqrd,elx,ely,a1,a3,b1,b3, &
&                              ielnd,elvol,ndu,ndv,iellocglob
    USE scratch_mod,     ONLY: rscratch11,rscratch12,elu=>rscratch21,   &
&                              elv=>rscratch22
    USE geometry_mod,    ONLY: dlm,dln
    USE error_mod,       ONLY: halt
    USE utilities_mod,   ONLY: gather
    USE timing_mod,      ONLY: bookleaf_times
    USE TYPH_util_mod,   ONLY: get_time
    USE TYPH_Collect_mod,ONLY: TYPH_Gather

    ! Argument list
    REAL(KIND=rlk),INTENT(INOUT)               :: dt
    ! Local
    INTEGER(KIND=ink)                          :: iel,ireg,ip,ii
    INTEGER(KIND=ink),DIMENSION(1)             :: iloc
    INTEGER(KIND=ink),PARAMETER                :: NDT=5_ink
    INTEGER(KIND=ink),DIMENSION(NDT)           :: idt
    INTEGER(KIND=ink),DIMENSION(3,0:NProcW-1)  :: idtt
    REAL(KIND=rlk)                             :: w1,w2,t0,t1,t2,t3
    REAL(KIND=rlk),   DIMENSION(NDT)           :: rdt
    REAL(KIND=rlk),   DIMENSION(0:NprocW-1)    :: dtt
    REAL(KIND=rlk),   DIMENSION(1)             :: dtm
    CHARACTER(LEN=8), DIMENSION(NDT),PARAMETER :: sdtt=['     CFL',     &
&                                                       '     DIV',     &
&                                                       '     ALE',     &
&                                                       '  GROWTH',     &
&                                                       ' MAXIMUM']

    ! Timer
    t0=get_time()

    ! Initialise
    rdt=HUGE(1.0_rlk)

    ! CFL
    DO iel=1,nel
      ireg=ielreg(iel)
      IF (zdtnotreg(ireg)) THEN
        rscratch11(iel)=dt_max
        rscratch12(iel)=TINY(1.0_rlk)
      ELSE
        w1=MAX(rho(iel),zcut)
        w2=MAX(ccut,csqrd(iel))+2.0_rlk*qq(iel)/w1
        IF (zmidlength(ireg)) THEN
          w1=MINVAL(dlm(nshape,elx(:,iel),ely(:,iel)))
        ELSE
          w1=MINVAL(dln(nshape,elx(:,iel),ely(:,iel)))
        ENDIF
        rscratch11(iel)=w1/w2
        rscratch12(iel)=w1
      ENDIF
    ENDDO
    ii=1_ink
    DO iel=2,nel
      IF (rscratch11(iel).LT.rscratch11(ii)) ii=iel
    ENDDO
    w1=rscratch11(ii)
    IF (w1.LT.0.0_rlk) CALL halt("ERROR: dt_cfl < 0",1)
    rdt(1)=cfl_sf*SQRT(w1)
    idt(1)=ii
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
      IF (w1.GT.w2) THEN
        w2=w1
        ii=iel
      ENDIF
    ENDDO
    rdt(2)=div_sf/w2
    idt(2)=ii
    ! ALE
    IF (zale.AND.zaleon) THEN
      w2=HUGE(1.0_rlk)
      IF (zeul) THEN
        DO iel=1,nel
          w1=MAX(elu(1,iel)*elu(1,iel)+elv(1,iel)*elv(1,iel),           &
&                elu(2,iel)*elu(2,iel)+elv(2,iel)*elv(2,iel),           &
&                elu(3,iel)*elu(3,iel)+elv(3,iel)*elv(3,iel),           &
&                elu(4,iel)*elu(4,iel)+elv(4,iel)*elv(4,iel))
          w1=rscratch12(iel)/MAX(w1,zerocut)
          IF (w1.LT.w2) THEN
            w2=w1
            ii=iel
          ENDIF
        ENDDO
      ELSE
        ! Other options
      ENDIF
      rdt(3)=ale_sf*SQRT(w2)
      idt(3)=ii
    ENDIF
    ! Growth
    rdt(4)=dt_g*dt
    idt(4)=-1_ink
    ! Maximum
    rdt(5)=dt_max
    idt(5)=-1_ink

    !# Missing code here that can't be merged

    ! Find smallest timestep, store info
    ii=MINVAL(MINLOC(rdt))
    dt=rdt(ii)
    idtel=idt(ii)
    IF (idtel.GT.0_ink) THEN
      idtreg=ielreg(idtel)
    ELSE
      idtreg=-1_ink
    ENDIF
    IF (zparallel) THEN
      idt(1)=idtel
      idt(2)=idtreg
      idt(3)=ii
      t1=get_time()
      ip=Typh_Gather(dt,dtt,comm=CommS)
      t2=get_time()
      t3=t2-t1
      ii=MINVAL(MINLOC(dtt))-1_ink  ! minloc converts (0:n) index to (1:n+1)
      dt=dtt(ii)
      t1=get_time()
      ip=Typh_Gather(idt(1:3),idtt,comm=CommS)
      t2=get_time()
      t3=t3+t2-t1
      ! global time controlling cell 
      IF (idtt(1,ii).LE.0_ink) THEN
        idtel=-1_ink
      ELSE
        idtel=iellocglob(idtt(1,ii))
      ENDIF
      idtreg=idtt(2,ii)
      ii=idtt(3,ii)
      bookleaf_times%time_in_colls=bookleaf_times%time_in_colls+t3
    ENDIF
    sdt=sdtt(ii)

    ! Check minimum
    IF (dt.LT.dt_min) CALL halt("ERROR: dt < dt_min",1,.true.)

    ! Timing data
    t1=get_time()
    t1=t1-t0
    bookleaf_times%time_in_getdt=bookleaf_times%time_in_getdt+t1

  END SUBROUTINE getdt

END MODULE getdt_mod
