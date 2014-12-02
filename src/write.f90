
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


MODULE write_mod

  USE kinds_mod,       ONLY: ink,rlk
  USE timing_stats,    ONLY: bookleaf_times
  USE TYPH_Collect_mod,ONLY: TYPH_Reduce,TYPH_OP_SUM,TYPH_OP_MIN,TYPH_OP_MAX
  USE TYPH_util_mod,   ONLY: get_time
  IMPLICIT NONE
  REAL(KIND=rlk)            :: t0,t1,t2,t3

  PUBLIC :: write_sprint

CONTAINS

  SUBROUTINE write_sprint()

    USE integers_mod,  ONLY: nel,nreg,nshape,nstep
    USE reals_mod,     ONLY: time,dencut
    USE paradef_mod,   ONLY: CommS,MProcW,zparallel
    USE pointers_mod,  ONLY: ielnod,ndx,ndy,ielmat,ein,pre,rho,csqrd,   &
&                            ndu,ndv,ielreg,elvol,cnmass,cnwt,elmass

    INTEGER(KIND=ink)                :: iel,ireg,ii,inod,ierr
    REAL(KIND=rlk)                   :: tot_mass,tot_pre,tot_rho,tot_ie,&
&                                       tot_vol,tot_ke,tot_mom_u,       &
&                                       tot_mom_v,c1,w2,w3,w4
    REAL(KIND=rlk),DIMENSION(1:nreg) :: reg_vol,reg_mass,reg_ke,reg_dmn,&
&                                       reg_dmx,reg_ie,reg_pre,reg_pmx, &
&                                       reg_pmn,reg_vol_gl,reg_mass_gl, &
&                                       reg_ke_gl,reg_dmn_gl,reg_dmx_gl,&
&                                       reg_pmn_gl,reg_pmx_gl,reg_ie_gl,&
&                                       reg_pre_gl,reg_rho_gl

    t0 = get_time()

    IF (MProcW) THEN
      WRITE(6,*) ' '
      WRITE(6,'(a11,i7,a8,f14.7)')'  Step no. ',nstep,' Time = ',time
    ENDIF

    ! Initialise arrays
    reg_vol=0.0_rlk
    reg_ie=0.0_rlk
    reg_pre=0.0_rlk
    reg_pmx=-HUGE(1.0_rlk)
    reg_pmn=HUGE(1.0_rlk)
    reg_mass=0.0_rlk
    reg_ke=0.0_rlk
    reg_dmx=-HUGE(1.0_rlk)
    reg_dmn=HUGE(1.0_rlk)

    ! Initialise momentum
    tot_mom_u=0.0_rlk
    tot_mom_v=0.0_rlk

    DO iel=1,nel
      ! Info
      ireg=ABS(ielreg(iel))
      ! Condition
      c1=dencut*elvol(iel)
      ! Scatter element contributions to region
      reg_vol(ireg)=reg_vol(ireg)+elvol(iel)
      IF (elmass(iel).GT.c1) THEN
        w2=elmass(iel)
        reg_mass(ireg)=reg_mass(ireg)+w2
        w3=ein(iel)
        w3=w3*w2
        reg_ie(ireg)=reg_ie(ireg)+w3
        w4=pre(iel)
        w3=w2*w4
        reg_pre(ireg)=reg_pre(ireg)+w3
        IF (w4.GT.reg_pmx(ireg)) reg_pmx(ireg)=w4
        IF (w4.LT.reg_pmn(ireg)) reg_pmn(ireg)=w4
        w4=rho(iel)
        IF (w4.GT.reg_dmx(ireg)) reg_dmx(ireg)=w4
        IF (w4.LT.reg_dmn(ireg)) reg_dmn(ireg)=w4
      ENDIF
      DO ii=1,nshape
        inod=ielnod(ii,iel)
        w2=ndu(inod)
        w3=ndv(inod)
        IF (elmass(iel).GT.c1) THEN
          reg_ke(ireg)=0.5_rlk*cnmass(ii,iel)*(w2*w2+w3*w3)+reg_ke(ireg)
        ENDIF
        w4=rho(iel)*cnwt(ii,iel)
        tot_mom_u=tot_mom_u+w2*w4
        tot_mom_v=tot_mom_v+w3*w4
      ENDDO
    ENDDO

    IF (MProcW) THEN
      WRITE(6,*) ' '
      WRITE(6,*) ' Table 1: Hydro region'
      WRITE(6,*) ' '
      WRITE(6,1001) 'reg','mat','vol','mass','tot ie','tot ke','press',   &
&                   'min press','max press','dens','min dens','max dens'
    ENDIF

    IF (zparallel) THEN
      t2 = get_time()
      ierr=TYPH_Reduce(reg_vol, RVal=reg_vol_gl, Op=TYPH_OP_SUM,Comm=CommS)
      ierr=TYPH_Reduce(reg_mass,RVal=reg_mass_gl,Op=TYPH_OP_SUM,Comm=CommS)
      ierr=TYPH_Reduce(reg_ie,  RVal=reg_ie_gl,  Op=TYPH_OP_SUM,Comm=CommS)
      ierr=TYPH_Reduce(reg_ke,  RVal=reg_ke_gl,  Op=TYPH_OP_SUM,Comm=CommS)
      ierr=TYPH_Reduce(reg_pre, RVal=reg_pre_gl, Op=TYPH_OP_SUM,Comm=CommS)
      ierr=TYPH_Reduce(reg_pmn, RVal=reg_pmn_gl, Op=TYPH_OP_MIN,Comm=CommS)
      ierr=TYPH_Reduce(reg_pmx, RVal=reg_pmx_gl, Op=TYPH_OP_MAX,Comm=CommS)
      ierr=TYPH_Reduce(reg_dmn, RVal=reg_dmn_gl, Op=TYPH_OP_MIN,Comm=CommS)
      ierr=TYPH_Reduce(reg_dmx, RVal=reg_dmx_gl, Op=TYPH_OP_MAX,Comm=CommS)
      t3 = get_time()
      t3=t3-t2
      bookleaf_times%time_in_colls=bookleaf_times%time_in_colls+t3
    ELSE
      reg_vol_gl  = reg_vol
      reg_mass_gl = reg_mass
      reg_ie_gl   = reg_ie
      reg_ke_gl   = reg_ke
      reg_pre_gl  = reg_pre
      reg_pmn_gl  = reg_pmn
      reg_pmx_gl  = reg_pmx
      reg_dmn_gl  = reg_dmn
      reg_dmx_gl  = reg_dmx
    ENDIF

    ! Calculate averages, totals and print table
    tot_vol =0.0_rlk
    tot_mass=0.0_rlk
    tot_ie  =0.0_rlk
    tot_ke  =0.0_rlk
    tot_pre =0.0_rlk
    DO ii=1,nreg
      IF (reg_vol_gl(ii).GT.0.0_rlk) THEN
        reg_rho_gl(ii)=reg_mass_gl(ii)/reg_vol_gl(ii)
      ENDIF
      IF (reg_mass_gl(ii).GT.(dencut*reg_vol_gl(ii))) THEN
        reg_pre_gl(ii)=reg_pre_gl(ii)/reg_mass_gl(ii)
      ENDIF
      tot_vol =tot_vol +reg_vol_gl(ii)
      tot_mass=tot_mass+reg_mass_gl(ii)
      tot_ie  =tot_ie  +reg_ie_gl(ii)
      tot_ke  =tot_ke  +reg_ke_gl(ii)
      tot_pre =tot_pre +reg_pre_gl(ii)*reg_mass_gl(ii)
      IF (MProcW) THEN
        WRITE(6,1002) ii,-999,reg_vol_gl(ii),reg_mass_gl(ii),             &
&                     reg_ie_gl(ii),reg_ke_gl(ii),reg_pre_gl(ii),         &
&                     reg_pmn_gl(ii),reg_pmx_gl(ii),reg_rho_gl(ii),       &
&                     reg_dmn_gl(ii),reg_dmx_gl(ii)
      ENDIF
    ENDDO
    tot_rho=tot_mass/tot_vol
    tot_pre=tot_pre /tot_mass
    IF (MProcW) THEN
      WRITE(6,*) ' '
      WRITE(6,1006) tot_vol,tot_mass,tot_ie,tot_ke,tot_pre,tot_rho
    ENDIF

    ! Print totals
    IF (MProcW) THEN
      WRITE(6,1007) tot_ie,tot_ke,tot_ie+tot_ke
    ENDIF

    ! Timing data
    t1 = get_time()
    t1=t1-t0
    bookleaf_times%time_in_io=bookleaf_times%time_in_io+t1

    ! Formats
 1001 FORMAT(2(3X,a3),9X,a3,8X,a4,6X,a6,6X,a6,7X,a5,3X,a9,3X,a9,8X,a4,  &
&            4X,a8,4X,a8)
 1002 FORMAT(2(1X,i5),1p10e12.4)
 1006 FORMAT(' Total ',5X,1p5e12.4,24X,1pe12.4,24X,/)
 1007 FORMAT('           total energy',/,' internal ',1pe12.4,          &
&             /,' kinetic  ',1pe12.4,/,' total    ',1pe12.4,/)

  END SUBROUTINE write_sprint

END MODULE write_mod
