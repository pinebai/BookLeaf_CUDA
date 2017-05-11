
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

SUBROUTINE hydro()

  use cudafor
  USE kinds_mod,    ONLY: ink,rlk, lok
  USE integers_mod, ONLY: nel,nshape,nstep,idtel,idtreg
  USE logicals_mod, ONLY: zale,zaleon,zmprocw, zparallel
  USE strings_mod,  ONLY: sdt
  USE reals_mod,    ONLY: time,time_end,dt_initial,time_alemin,         &
&                         time_alemax, pmeritreg, kappareg
  USE getdt_mod,    ONLY: getdt, getdt_host
  USE lagstep_mod,  ONLY: lagstep
  USE alestep_mod,  ONLY: alestep
  USE timing_mod,   ONLY: bookleaf_times
  USE Typh_util_mod,ONLY: get_time
  USE reals_mod,    ONLY: pmeritreg

  USE pointers_mod, ONLY: rho,elmass,elvol,ielmat,ein,pre,csqrd,      &
&                           ndx,ndy,elx,ely,ndu,ndv,ielnd, ielreg,    &
&                           spmass, cnwt,cnmass, ielsort1
  USE pointers_mod, ONLY: ielel,ielsd,indtype,qq,qx,qy, a1, a2, a3, b1,b2, b3
  USE scratch_mod,  ONLY: elu=>rscratch21,elv=>rscratch22,            &
&                           elfx=>rscratch23,elfy=>rscratch24,          &
&                           rho05=>rscratch11,ein05=>rscratch12,        &
&                           pre05=>rscratch13,ndxu=>rscratch14,         &
&                           ndyv=>rscratch15,dx=>rscratch25,            &
&                           dy=>rscratch26,scratch=>rscratch27
  USE pointers_mod,   ONLY: ielsort2
  USE scratch_mod,    ONLY: store6=>rscratch16,elvv=>rscratch28,        &
&                           indstatus=>iscratch11,zactive=>zscratch11
    USE integers_mod,   ONLY: eos_type
    USE reals_mod,      ONLY: eos_param,pcut
    USE parameters_mod, ONLY: LI


    USE logicals_mod,    ONLY: zdtnotreg,zmidlength
    USE pointers_mod,    ONLY: iellocglob
  IMPLICIT NONE

  ! Local
  REAL(KIND=rlk) :: dt,t0,t1,t2,grind

  CHARACTER(LEN=4) :: str

  ! Timer

  !device data
    INTEGER(KIND=ink), allocatable, device             :: d_ielnd(:,:),d_ielel(:,:), &
&                                                         d_ielsd(:,:),d_indtype(:), &
&                                                         d_ielreg(:), d_ielmat(:),  &
&                                                         d_eos_type(:), d_ielsort1(:), &
&                                                         d_iellocglob(:), d_ielsort2(:), &
&                                                         d_indstatus(:)

    REAL(KIND=rlk),allocatable,dimension(:),device     :: d_qq, d_csqrd, d_a1, d_a2, d_a3, d_b1,d_b2, d_b3, d_pre, d_pre05

    REAL(KIND=rlk),allocatable,dimension(:,:),device   :: d_qx, d_qy

    REAL(KIND=rlk),DIMENSION(:,:),allocatable,device   :: d_elx,d_ely,d_elu,d_elv, d_cnwt, d_cnmass
    REAL(KIND=rlk),DIMENSION(:,:),allocatable,device   :: d_dx,d_dy,d_du,d_dv,d_scratch, d_spmass, d_eos_param, &
&                                                         d_elvv
    REAL(KIND=rlk),DIMENSION(:),allocatable, device    :: d_rho, d_rho05,d_ndu, d_ndv, d_pmeritreg, d_elvol, &
&                                                         d_kappareg, d_ndx, d_ndy, d_ndxu, d_ndyv, d_elmass, &
&                                                         d_ein05, d_ein, d_store6

    logical(kind=lok), dimension(:), allocatable, device :: d_zdtnotreg, d_zmidlength, d_zactive
    bookleaf_times%time_hydro=get_time()


  ! initialise
  nstep=0_ink
  dt=dt_initial
  idtel=0_ink
  idtreg=0_ink
  sdt=' INITIAL'

	allocate(d_elu(nshape, nel))
	allocate(d_elv(nshape, nel))
	allocate(d_elx(nshape, nel))
    allocate(d_ely(nshape, nel))
    allocate(d_rho(nel))
    allocate(d_rho05(nel))
    allocate(d_eos_param(6,LI))
    allocate(d_zdtnotreg(LI))
    allocate(d_zmidlength(LI))


    !allocate(d_zactive(size(zactive)))
    !allocate(d_store6(size(store6)))
    !allocate(d_indstatus(size(indstatus)))
    !allocate(d_elvv(size(elvv)/size(elvv(0,:)),size(elvv(0,:))))

    if(size(iellocglob)>0) allocate(d_iellocglob(size(iellocglob)))

   allocate(d_qq(size(qq)))
   allocate(d_qx(size(qx)/size(qx(0,:)),size(qx(0,:))))
   allocate(d_qy(size(qy)/size(qy(0,:)),size(qy(0,:))))
   allocate(d_du(size(elfx)/size(elfx(0,:)),size(elfx(0,:))))
   allocate(d_dv(size(elfy)/size(elfy(0,:)),size(elfy(0,:))))
   allocate(d_dx(size(dx)/size(dx(0,:)),size(dx(0,:))))
   allocate(d_dy(size(dy)/size(dy(0,:)),size(dy(0,:))))
   allocate(d_scratch(size(scratch)/size(scratch(0,:)), size(scratch(0,:))))
   allocate(d_ielel(size(ielel)/size(ielel(0,:)),size(ielel(0,:))))
   allocate(d_ielnd(size(ielnd)/size(ielnd(0,:)),size(ielnd(0,:))))
   allocate(d_ielsd(size(ielsd)/size(ielsd(0,:)),size(ielsd(0,:))))
   allocate(d_cnwt(size(cnwt)/size(cnwt(0,:)),size(cnwt(0,:))))
   allocate(d_cnmass(size(cnmass)/size(cnmass(0,:)),size(cnmass(0,:))))
   allocate(d_indtype(size(indtype)))
   allocate(d_csqrd(size(csqrd)))
   allocate(d_ndu(size(ndu)))
   allocate(d_ndv(size(ndv)))
   allocate(d_ndx(size(ndx)))
   allocate(d_ndy(size(ndy)))
   allocate(d_a1(size(a1)))
   allocate(d_a2(size(a2)))
   allocate(d_a3(size(a3)))
   allocate(d_b1(size(b1)))
   allocate(d_b2(size(b2)))
   allocate(d_b3(size(b3)))
   allocate(d_pre(size(pre)))
   allocate(d_pre05(size(pre05)))
   allocate(d_ielreg(size(ielreg)))
   allocate(d_pmeritreg(size(pmeritreg)))
   allocate(d_elvol(size(elvol)))
   allocate(d_kappareg(size(kappareg)))
   allocate(d_ndxu(size(ndxu)))
   allocate(d_ndyv(size(ndyv)))
   allocate(d_elmass(size(elmass)))
   allocate(d_ein05(size(ein05)))
   allocate(d_ein(size(ein)))
   allocate(d_ielmat(size(ielmat)))
   allocate(d_eos_type(size(eos_type)))
   allocate(d_ielsort1(size(ielsort1)))
   !allocate(d_spmass(size(spmass)/size(spmass(0,:)), size(spmass(0,:))))

   !d_spmass = spmass

   !d_elvol = elvol
   d_ielreg = ielreg
   d_pmeritreg = pmeritreg
   d_kappareg = kappareg   
   d_elx = elx
   d_ely = ely
   d_rho = rho
   d_ielel = ielel
   d_ielnd = ielnd
   d_ielsd = ielsd
   d_indtype = indtype
   d_csqrd = csqrd
   d_ndu = ndu
   d_ndv = ndv
   d_a1 = a1
   d_a2 = a2
   d_a3 = a3
   d_b1 = b1
   d_b2 = b2
   d_b3 = b3
   d_pre = pre
   d_pre05 = pre05
   d_ndu = ndu
   d_ndv = ndv
   d_ndx = ndx
   d_ndy = ndy
   d_elmass = elmass
   d_rho05 = rho05
   d_ein = ein
   d_ein05 = ein05
   d_ielmat = ielmat
   d_eos_param = eos_param
   d_eos_type = eos_type
   d_cnmass = cnmass
   d_ielsort1 = ielsort1
   d_elvol = elvol
   !d_elu = elu
   !d_elv = elv
   d_zdtnotreg = zdtnotreg
   d_zmidlength = zmidlength
   !d_qq = qq
  l1:DO
    t0=get_time()
    ! increment step
    nstep=nstep+1_ink


    ! calculate timestep

    if(zparallel) then
        IF (nstep.GT.1_ink) CALL getdt(dt)
        d_elu = elu
        d_elv = elv
        d_rho05 = rho05
        d_ein05 = ein05
    else
        IF (nstep.GT.1_ink) CALL getdt_host(dt, d_zdtnotreg, d_zmidlength, d_ielreg, d_rho, d_qq, d_csqrd, d_elx, d_ely,&
&                               d_a1, d_a3, d_b1, d_b3, d_ielnd, d_elvol, d_ndu, d_ndv, d_iellocglob, d_rho05, &
&                               d_ein05, d_elu, d_elv)
    endif
    
    time=time+dt
    !# Missing code here that can't be merged
    ! update time
    !# Code here that can't be taken out
    ! lagrangian step
    call  lagstep(dt, d_elu, d_elv, d_elx, d_ely, d_rho, d_rho05,d_qq, d_qx, d_qy, d_du, d_dv, d_dx, &
&    d_dy, d_scratch, d_ielel, d_ielnd, d_ielsd, d_indtype, d_csqrd, d_ndu, d_ndv, d_a1, d_a3, d_b1, &
&    d_b3, d_pre,d_pre05, d_ielreg, d_pmeritreg, d_spmass, d_elvol, d_kappareg, d_ndx, d_ndy, d_ndxu, d_ndyv,&
&    d_a2, d_b2, d_cnwt, d_elmass, d_ein05, d_ein, d_ielmat, d_eos_type, d_eos_param, d_cnmass, d_ielsort1)


    ! ale step
    IF (zale) THEN
      zaleon=(time.GE.time_alemin).AND.(time.LE.time_alemax)
      print *, 'called'
      IF (zaleon) CALL alestep(nstep,dt)
    ENDIF
    !# Missing code here that can't be merged
    t1=get_time()
    t2=t1-t0
    grind=t2*1.0e6_rlk/nel
    IF (zmprocw) THEN
      WRITE(6,'(" step=",i7,"  el=",i9,"  reg=",i3,"  dt=",1pe16.9,'    &
&      //'"  time=",1pe16.9,"  grind=",1pe8.1,"  timer=",1pe16.9," s",' &
&      //'2X,a8)') nstep,idtel,idtreg,dt,time,grind,t2,sdt
    ENDIF
    ! IO Timing data
    t2=get_time()
    t2=t2-t1

    if(zparallel) then
        qq = d_qq
        qx = d_qx
        qy = d_qy
        elfx = d_du
        elfy = d_dv
        dx = d_dx
        dy = d_dy
        elu = d_elu
        elv = d_elv
        scratch = d_scratch
        ndxu = d_ndxu
        ndyv = d_ndyv
        elx = d_elx
        ely = d_ely
        a1 = d_a1
        a2 = d_a2
        a3 = d_a3
        b1 = d_b1
        b2 = d_b2
        b3 = d_b3
        cnwt = d_cnwt
        elvol = d_elvol
        rho05 = d_rho05
        rho = d_rho
        ein05 = d_ein05
        pre05 = d_pre05
        pre = d_pre
        pre05 = d_pre05
        csqrd = d_csqrd
        ndx = d_ndx
        ndy = d_ndy
        ndu = d_ndu
        ndv = d_ndv
        ein = d_ein
    endif
    bookleaf_times%time_step_io=bookleaf_times%time_step_io+t2
    ! test for end of calculation
    IF (time.GE.time_end) EXIT l1
    !# test for resources
  ENDDO l1

    qq = d_qq
    qx = d_qx
    qy = d_qy
    elfx = d_du
    elfy = d_dv
    dx = d_dx
    dy = d_dy
    elu = d_elu
    elv = d_elv
    scratch = d_scratch
    ndxu = d_ndxu
    ndyv = d_ndyv
    elx = d_elx
    ely = d_ely
    a1 = d_a1
    a2 = d_a2
    a3 = d_a3
    b1 = d_b1
    b2 = d_b2
    b3 = d_b3
    cnwt = d_cnwt
    elvol = d_elvol
    rho05 = d_rho05
    rho = d_rho
    ein05 = d_ein05
    pre05 = d_pre05
    pre = d_pre
    pre05 = d_pre05
    csqrd = d_csqrd
    ndx = d_ndx
    ndy = d_ndy
    ndu = d_ndu
    ndv = d_ndv
    ein = d_ein
    ! deallocate(d_elu)
    ! deallocate(d_elv)!(nshape, nel))
    ! deallocate(d_elx)!(nshape, nel))
    ! deallocate(d_ely)!(nshape, nel))
    ! deallocate(d_rho)!(nel))
    ! deallocate(d_rho05)!(nel))
    ! deallocate(d_eos_param)!(6,LI))
    ! deallocate(d_zdtnotreg)!(LI))
    ! deallocate(d_zmidlength)!(LI))
    !
    !
    ! if(size(iellocglob)>0) deallocate(d_iellocglob)!(size(iellocglob)))
    !
    !deallocate(d_qq)!(size(qq)))
    !deallocate(d_qx)!(size(qx)/size(qx(0,:)),size(qx(0,:))))
    !deallocate(d_qy)!(size(qy)/size(qy(0,:)),size(qy(0,:))))
    !deallocate(d_du)!(size(elfx)/size(elfx(0,:)),size(elfx(0,:))))
    !deallocate(d_dv)!(size(elfy)/size(elfy(0,:)),size(elfy(0,:))))
    !deallocate(d_dx)!(size(dx)/size(dx(0,:)),size(dx(0,:))))
    !deallocate(d_dy)!(size(dy)/size(dy(0,:)),size(dy(0,:))))
    !deallocate(d_scratch)!(size(scratch)/size(scratch(0,:)), size(scratch(0,:))))
    !deallocate(d_ielel)!(size(ielel)/size(ielel(0,:)),size(ielel(0,:))))
    !deallocate(d_ielnd)!(size(ielnd)/size(ielnd(0,:)),size(ielnd(0,:))))
    !deallocate(d_ielsd)!(size(ielsd)/size(ielsd(0,:)),size(ielsd(0,:))))
    !deallocate(d_cnwt)!(size(cnwt)/size(cnwt(0,:)),size(cnwt(0,:))))
    !deallocate(d_cnmass)!(size(cnmass)/size(cnmass(0,:)),size(cnmass(0,:))))
    !deallocate(d_indtype)!(size(indtype)))
    !deallocate(d_csqrd)!(size(csqrd)))
    !deallocate(d_ndu)!(size(ndu)))
    !deallocate(d_ndv)!(size(ndv)))
    !deallocate(d_ndx)!(size(ndx)))
    !deallocate(d_ndy)!(size(ndy)))
    !deallocate(d_a1)!(size(a1)))
    !deallocate(d_a2)!(size(a2)))
    !deallocate(d_a3)!(size(a3)))
    !deallocate(d_b1)!(size(b1)))
    !deallocate(d_b2)!(size(b2)))
    !deallocate(d_b3)!(size(b3)))
    !deallocate(d_pre)!(size(pre)))
    !deallocate(d_pre05)!(size(pre05)))
    !deallocate(d_ielreg)!(size(ielreg)))
    !deallocate(d_pmeritreg)!(size(pmeritreg)))
    !deallocate(d_elvol)!(size(elvol)))
    !deallocate(d_kappareg)!(size(kappareg)))
    !deallocate(d_ndxu)!(size(ndxu)))
    !deallocate(d_ndyv)!(size(ndyv)))
    !deallocate(d_elmass)!(size(elmass)))
    !deallocate(d_ein05)!(size(ein05)))
    !deallocate(d_ein)!(size(ein)))
    !deallocate(d_ielmat)!(size(ielmat)))
    !deallocate(d_eos_type)!(size(eos_type)))
    !deallocate(d_ielsort1)!(size(ielsort1)))
	
END SUBROUTINE hydro
