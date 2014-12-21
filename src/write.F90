
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

  IMPLICIT NONE

  PUBLIC :: write_sprint,write_iprint,write_lprint

CONTAINS

  SUBROUTINE write_sprint()

    USE kinds_mod,       ONLY: ink,rlk
    USE integers_mod,    ONLY: nel,nreg,nshape,nstep
    USE reals_mod,       ONLY: time,dencut
    USE timing_mod,      ONLY: bookleaf_times
    USE typh_collect_mod,ONLY: typh_reduce,TYPH_OP_SUM,TYPH_OP_MIN,     &
&                              TYPH_OP_MAX
    USE typh_util_mod,   ONLY: get_time
    USE paradef_mod,     ONLY: CommS,MProcW
    USE pointers_mod,    ONLY: ielnod,ein,pre,rho,elvol,elmass,cnmass,  &
&                              ndu,ndv,ielreg,cnwt

    INTEGER(KIND=ink)                :: iel,ireg,ii,inod,ierr
    REAL(KIND=rlk)                   :: tot_mass,tot_pre,tot_rho,tot_ie,&
&                                       tot_vol,tot_ke,tot_mom_u,t0,t1, &
&                                       tot_mom_v,c1,w2,w3,w4,t2,t3
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

    t2=get_time()
    ierr=TYPH_Reduce(reg_vol, RVal=reg_vol_gl, Op=TYPH_OP_SUM,Comm=CommS)
    ierr=TYPH_Reduce(reg_mass,RVal=reg_mass_gl,Op=TYPH_OP_SUM,Comm=CommS)
    ierr=TYPH_Reduce(reg_ie,  RVal=reg_ie_gl,  Op=TYPH_OP_SUM,Comm=CommS)
    ierr=TYPH_Reduce(reg_ke,  RVal=reg_ke_gl,  Op=TYPH_OP_SUM,Comm=CommS)
    ierr=TYPH_Reduce(reg_pre, RVal=reg_pre_gl, Op=TYPH_OP_SUM,Comm=CommS)
    ierr=TYPH_Reduce(reg_pmn, RVal=reg_pmn_gl, Op=TYPH_OP_MIN,Comm=CommS)
    ierr=TYPH_Reduce(reg_pmx, RVal=reg_pmx_gl, Op=TYPH_OP_MAX,Comm=CommS)
    ierr=TYPH_Reduce(reg_dmn, RVal=reg_dmn_gl, Op=TYPH_OP_MIN,Comm=CommS)
    ierr=TYPH_Reduce(reg_dmx, RVal=reg_dmx_gl, Op=TYPH_OP_MAX,Comm=CommS)
    t3=get_time()
    t3=t3-t2
    bookleaf_times%time_in_colls=bookleaf_times%time_in_colls+t3

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

  SUBROUTINE write_iprint(reg)

    USE kinds_mod,   ONLY: ink
    USE integers_mod,ONLY: nreg,nmat,eos_type
    USE reals_mod,   ONLY: time_start,time_end,dt_initial,dt_min,dt_max,&
&                          dt_g,cfl_sf,div_sf,cq1,cq2,kappaall,kappareg,&
&                          pmeritall,pmeritreg,zcut,zerocut,dencut,     &
&                          accut,pcut,ccut,eos_param,mat_rho,mat_ein
    USE logicals_mod,ONLY: zdtnotreg,zmidlength
    USE strings_mod, ONLY: sfile
    USE paradef_mod, ONLY: MprocW,NprocW,Nthread
    USE mesh_mod,    ONLY: mesh_print,regions

    ! Argument list
    TYPE(regions),DIMENSION(:),INTENT(IN) :: reg
    ! Local
    INTEGER(KIND=ink) :: ii
    CHARACTER(LEN=8)  :: dat
    CHARACTER(LEN=10) :: tim
    CHARACTER(LEN=5)  :: zon,str1,str2

    CALL DATE_AND_TIME(DATE=dat,TIME=tim,ZONE=zon)

    IF (MprocW) THEN
      PRINT*,'########################################################',&
&      '################'
      PRINT*,' Input file:       ',TRIM(ADJUSTL(sfile))
      WRITE(6,'(a30,a12,a10)') '  Time stamp:       '//dat(7:8)//'/'//  &
&      dat(5:6)//'/'//dat(1:4),' at '//tim(1:2)//':'//tim(3:4)//':'//   &
&      tim(5:6),' '//zon(1:5)//' GMT'
      PRINT*,'########################################################',&
&      '################'
      PRINT*,'PRE-PROCESSING OPTIONS'
#ifdef NOMPI
      PRINT*,' No MPI routines included'
#else
      PRINT*,' MPI parallelism included'
      WRITE(6,'(a14,i5,a10)') '  Running on: ',NprocW,' MPI tasks'
#endif
#ifdef NOOMP
      PRINT*,' No OpenMP threads used'
#else
      WRITE(6,'(a9,i5,a16)') '  Using: ',Nthread,' Open MP threads'
#endif
#ifdef MOD
      PRINT*,' Additional problem specific initialisation used'
#else
      PRINT*,' No additional problem specific initialisation used'
#endif
#ifdef SILO
      PRINT*,' SILO visualisation dumps written out'
#else
      PRINT*,' No SILO visualisation dumps available'
#endif
      PRINT*,'########################################################',&
&      '################'
      PRINT*,'CONTROL OPTIONS'
      WRITE(6,'(a63,e10.4)')'  Time at which calculation starts:       '&
&      //'          time_start ',time_start
      WRITE(6,'(a63,e10.4)')'  Time at which calculation ends:         '&
&      //'            time_end ',time_end
      WRITE(6,'(a63,e10.4)')'  Initial timestep:                       '&
&      //'          dt_initial ',dt_initial
      WRITE(6,'(a63,e10.4)')'  Minimum allowed timestep:               '&
&      //'              dt_min ',dt_min
      WRITE(6,'(a63,e10.4)')'  Maximum allowed timestep:               '&
&      //'              dt_max ',dt_max
      WRITE(6,'(a63,e10.4)')'  Timestep growth factor:                 '&
&      //'                dt_g ',dt_g
      WRITE(6,'(a63,e10.4)')'  Divergence safety factor:               '&
&      //'              div_sf ',div_sf
      WRITE(6,'(a63,e10.4)')'  CFL safety factor:                      '&
&      //'              cfl_sf ',cfl_sf
      WRITE(6,'(a63)')      '  Mid-length or projection for CFL length '&
&      //'scale:    zmidlength '
      WRITE(6,'(a63)')      '  Exclude region from CFL calculation:    '&
&      //'           zdtnotreg '
      WRITE(6,'(a63,e10.4)')'  Linear artificial viscosity coefficient:'&
&      //'                 cq1 ',cq1
      WRITE(6,'(a63,e10.4)')'  Quadratic artificial viscosity coefficie'&
&      //'nt:              cq2 ',cq2
      WRITE(6,'(a63,e10.4)')'  Hourglass filter coefficient:           '&
&      //'            kappaall ',kappaall
      WRITE(6,'(a63)')      '  Regional hourglass filter coefficient:  '&
&      //'            kappareg '
      WRITE(6,'(a63,e10.4)')'  Sub-zonal pressure coefficient:         '&
&      //'           pmeritall ',pmeritall
      WRITE(6,'(a63)')      '  Regional sub-zonal pressure coefficient:'&
&      //'           pmeritreg '
      PRINT*,' '
      WRITE(6,'(a5,4a11)') '  reg',' mid-length','  CFL calc.',         &
&      '      kappa','     pmerit'
      DO ii=1,nreg
        IF (zmidlength(ii)) THEN
          str1=' TRUE'
        ELSE
          str1='FALSE'
        ENDIF
        IF (zdtnotreg(ii)) THEN
          str2='FALSE'
        ELSE
          str2=' TRUE'
        ENDIF
        WRITE(6,'(i5,2(6X,a5),2(1X,e10.4))')ii,str1,str2,kappareg(ii),  &
&        pmeritreg(ii)
      ENDDO
      PRINT*,' '
      PRINT*,'########################################################',&
&      '################'
      PRINT*,'MESHING OPTIONS'
      CALL mesh_print(reg)
      PRINT*,'########################################################',&
&      '################'
      PRINT*,'EOS OPTIONS'
      DO ii=1,nmat
        WRITE(6,'(a12,i3)') '  Material: ',ii
        SELECT CASE(eos_type(ii))
          CASE(0)
            WRITE(6,'(a63,a10)') '   EOS:                              '&
&            //'                 eos_type ','      VOID'
            WRITE(6,'(a58,i3,a2,e10.4)') '   Void pressure (p0):       '&
&            //'                 eos_param(1,',ii,') ',eos_param(1,ii)
          CASE(1)
            WRITE(6,'(a63,a10)') '   EOS:                              '&
&            //'                 eos_type ',' IDEAL GAS'
            WRITE(6,'(a58,i3,a2,f10.6)') '   Ideal gas gamma:          '&
&            //'                 eos_param(1,',ii,') ',eos_param(1,ii)
          CASE(2)
            WRITE(6,'(a63,a10)') '   EOS:                              '&
&            //'                 eos_type ','      TAIT'
            WRITE(6,'(a58,i3,a2,f10.6)') '   Tait a:                   '&
&            //'                 eos_param(1,',ii,') ',eos_param(1,ii)
            WRITE(6,'(a58,i3,a2,f10.6)') '   Tait b:                   '&
&            //'                 eos_param(2,',ii,') ',eos_param(2,ii)
            WRITE(6,'(a58,i3,a2,f10.6)') '   Tait rho0:                '&
&            //'                 eos_param(3,',ii,') ',eos_param(3,ii)
          CASE(3)
            WRITE(6,'(a63,a10)') '   EOS:                              '&
&            //'                 eos_type ','       JWL'
            WRITE(6,'(a58,i3,a2,f10.6)') '   JWL omega:                '&
&            //'                 eos_param(1,',ii,') ',eos_param(1,ii)
            WRITE(6,'(a58,i3,a2,f10.6)') '   JWL a:                    '&
&            //'                 eos_param(2,',ii,') ',eos_param(2,ii)
            WRITE(6,'(a58,i3,a2,f10.6)') '   JWL b:                    '&
&            //'                 eos_param(3,',ii,') ',eos_param(3,ii)
            WRITE(6,'(a58,i3,a2,f10.6)') '   JWL r1:                   '&
&            //'                 eos_param(4,',ii,') ',eos_param(4,ii)
            WRITE(6,'(a58,i3,a2,f10.6)') '   JWL r2:                   '&
&            //'                 eos_param(5,',ii,') ',eos_param(5,ii)
            WRITE(6,'(a58,i3,a2,f10.6)') '   JWL rho0:                 '&
&            //'                 eos_param(6,',ii,') ',eos_param(6,ii)
        END SELECT
        WRITE(6,'(a63,e10.4)') '   Density:                            '&
&        //'                mat_rho ',mat_rho(ii)
        WRITE(6,'(a63,e10.4)') '   Internal energy:                    '&
&        //'                mat_ein ',mat_ein(ii)
      ENDDO
      PRINT*,'########################################################',&
&      '################'
      PRINT*,'CUT-OFF OPTIONS'
      WRITE(6,'(a63,e10.4)')'  Rounding precision cut-off:             '&
&      //'                zcut ',zcut
      WRITE(6,'(a63,e10.4)')'  Underflow cut-off:                      '&
&      //'             zerocut ',zerocut
      WRITE(6,'(a63,e10.4)')'  Acceleration cut-off:                   '&
&      //'               accut ',accut
      WRITE(6,'(a63,e10.4)')'  Density cut-off:                        '&
&      //'              dencut ',dencut
      WRITE(6,'(a63,e10.4)')'  Pressure cut-off:                       '&
&      //'                pcut ',pcut
      WRITE(6,'(a63,e10.4)')'  Sound speed cut-off:                    '&
&      //'                ccut ',ccut
      PRINT*,'########################################################',&
&      '################'
    ENDIF

  END SUBROUTINE write_iprint

  SUBROUTINE write_lprint(str,iunit)

    USE kinds_mod,   ONLY: ink
    USE integers_mod,ONLY: nel,nnod
    USE pointers_mod,ONLY: rho,pre,ein,elvol,elmass,qx,qy,indtype,ndx,  &
&                          ndy,ndu,ndv

    ! Argument list
    CHARACTER(LEN=*), INTENT(IN) :: str
    INTEGER(KIND=ink),INTENT(IN) :: iunit
    ! Local
    INTEGER(KIND=ink)            :: ii

    WRITE(iunit,*) TRIM(str)
    WRITE(iunit,*) ' '
    WRITE(iunit,'(a6,5(1X,a12))')'    el','         rho','           P',&
&    '          IE','           V','           M'
    DO ii=1,nel
      WRITE(iunit,'(i6,5(1X,e12.4))') ii,rho(ii),pre(ii),ein(ii),       &
&      elvol(ii),elmass(ii)
    ENDDO
    WRITE(iunit,*) ' '
    WRITE(iunit,'(a6,8(1X,a12))')'    el','       qx(1)','       qx(2)',&
&    '       qx(3)','       qx(4)','       qy(1)','       qy(2)',       &
&    '       qy(3)','       qy(4)'
    DO ii=1,nel
      WRITE(iunit,'(i6,8(1X,e12.4))') ii,qx(1,ii),qx(2,ii),qx(3,ii),    &
&      qx(4,ii),qy(1,ii),qy(2,ii),qy(3,ii),qy(4,ii)
    ENDDO
    WRITE(iunit,*) ' '
    WRITE(iunit,'(2(1X,a6),4(1X,a12))')'  node','  type','           X',&
&    '           Y','           U','           V'
    DO ii=1,nnod
      WRITE(iunit,'(2(1X,i6),4(1X,e12.4))') ii,indtype(ii),ndx(ii),     &
&      ndy(ii),ndu(ii),ndv(ii)
    ENDDO

  END SUBROUTINE write_lprint

END MODULE write_mod
