
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

SUBROUTINE init_memory()

  USE kinds_mod,   ONLY: ink
  USE integers_mod,ONLY: nshape,nel1,nnod1,nsz
  USE logicals_mod,ONLY: zsp,zale
  USE error_mod,   ONLY: halt
  USE paradef_mod, ONLY: ielsort1
  USE pointers_mod,ONLY: ielreg,ielmat,ielnd,rho,qq,csqrd,pre,ein,cnwt, &
&                        elmass,elvol,ndu,ndv,a1,a2,a3,b1,b2,b3,ndx,ndy,&
&                        indtype,ielel,cnmass,elx,ely,qx,qy,spmass,ielsd
  USE scratch_mod, ONLY: rscratch21,rscratch22,rscratch23,rscratch24,   &
&                        rscratch25,rscratch26,rscratch27,rscratch11,   &
&                        rscratch12,rscratch13,rscratch14,rscratch15,   &
&                        rscratch16,iscratch11,zscratch11

  IMPLICIT NONE

  ! Local
  INTEGER(KIND=ink) :: ierr

  ALLOCATE(ielreg(1:nel1),ielmat(1:nel1),ielnd(nshape,1:nel1),          &
&          rho(1:nel1),qq(1:nel1),csqrd(1:nel1),pre(1:nel1),ein(1:nel1),&
&          elmass(1:nel1),elvol(1:nel1),ndu(1:nnod1),ndv(1:nnod1),      &
&          a1(1:nel1),a2(1:nel1),a3(1:nel1),b1(1:nel1),b2(1:nel1),      &
&          b3(1:nel1),ndx(1:nnod1),ndy(1:nnod1),indtype(1:nnod1),       &
&          cnwt(nshape,1:nel1),cnmass(nshape,1:nel1),elx(nshape,1:nel1),&
&          ely(nshape,1:nel1),qx(nshape,1:nel1),qy(nshape,1:nel1),      &
&          ielel(nshape,1:nel1),ielsd(nshape,1:nel1),ielsort1(nel1),    &
&          STAT=ierr)
  IF (ierr.NE.0_ink) CALL halt("ERROR: failed to allocate memory",0)
  nsz=MAX(nel1,nnod1)
  ALLOCATE(rscratch11(1:nsz),rscratch12(1:nsz),rscratch13(1:nsz),       &
&          rscratch14(1:nsz),rscratch15(1:nsz),rscratch21(nshape,1:nsz),&
&          rscratch22(nshape,1:nsz),rscratch23(nshape,1:nsz),           &
&          rscratch24(nshape,1:nsz),rscratch25(nshape,1:nsz),           &
&          rscratch26(nshape,1:nsz),rscratch27(nshape,1:nsz),STAT=ierr)
  IF (ierr.NE.0_ink) CALL halt("ERROR: failed to allocate memory",0)
  IF (zsp) THEN
    ALLOCATE(spmass(nshape,1:nel1),STAT=ierr)
    IF (ierr.NE.0_ink) CALL halt("ERROR: failed to allocate memory",0)
  ENDIF
  IF (zale) THEN
    ALLOCATE(rscratch16(1:nsz),iscratch11(1:nsz),zscratch11(1:nsz),     &
&    STAT=ierr)
    IF (ierr.NE.0_ink) CALL halt("ERROR: failed to allocate memory",0)
  ENDIF

END SUBROUTINE init_memory

SUBROUTINE init()

  USE kinds_mod,    ONLY: ink,rlk
  USE integers_mod, ONLY: nshape,nel,nnod,nel1
  USE logicals_mod, ONLY: zsp
  USE reals_mod,    ONLY: time,time_start,mat_rho,mat_ein
  USE pointers_mod, ONLY: ielmat,rho,ein,elmass,elvol,qq,qx,qy,pre,     &
&                         csqrd,ndx,ndy,elx,ely,ielel,ielnd,ielsd,cnwt, &
&                         cnmass,spmass,indtype
  USE geometry_mod, ONLY: getgeom
  USE getpc_mod,    ONLY: getpc
  USE utilities_mod,ONLY: getconn,getsconn,corrconn

  IMPLICIT NONE

  ! Local
  INTEGER(KIND=ink)                       :: iel,imat,ii,jj,j1,j2
  INTEGER(KIND=ink),DIMENSION(0:nshape-1) :: nodes
  REAL(KIND=rlk)                          :: x1,x2,x3,x4,y1,y2,y3,y4,w1,&
&                                            w2,w3,w4

  ! initialise time
  time=time_start

  ! initialise geometry
  CALL getgeom(nshape,nel,nnod,ndx(1),ndy(1),elx(1,1),ely(1,1))

  ! initialise density, energy and mass
  DO iel=1,nel
    imat=ielmat(iel)
    rho(iel)=mat_rho(imat)
    ein(iel)=mat_ein(imat)
    elmass(iel)=rho(iel)*elvol(iel)
    cnmass(1:nshape,iel)=rho(iel)*cnwt(1:nshape,iel)
  ENDDO

  ! initialise subzonal pressure mass
  IF (zsp) THEN
    DO iel=1,nel
      x3=0.25_rlk*(elx(1,iel)+elx(2,iel)+elx(3,iel)+elx(4,iel))
      y3=0.25_rlk*(ely(1,iel)+ely(2,iel)+ely(3,iel)+ely(4,iel))
      DO j1=1,nshape
        x1=elx(j1,iel)
        y1=ely(j1,iel)
        j2=MOD(j1,nshape)+1_ink
        x2=0.5_rlk*(x1+elx(j2,iel))
        y2=0.5_rlk*(y1+ely(j2,iel))
        j2=MOD(j1+2,nshape)+1_ink
        x4=0.5_rlk*(x1+elx(j2,iel))
        y4=0.5_rlk*(y1+ely(j2,iel))
        !# Axi-symmetric alternative
        w1=0.25_rlk*(-x1+x2+x3-x4)
        w2=0.25_rlk*(-x1-x2+x3+x4)
        w3=0.25_rlk*(-y1+y2+y3-y4)
        w4=0.25_rlk*(-y1-y2+y3+y4)
        spmass(j1,iel)=4.0_rlk*rho(iel)*(w1*w4-w2*w3)
      ENDDO
    ENDDO
  ENDIF

  ! initialise pressure and sound speed
  CALL getpc(nel,ielmat(1),rho(1),ein(1),pre(1),csqrd(1))

  ! initialise artifical viscosity
  qq=0.0_rlk
  qx=0.0_rlk
  qy=0.0_rlk

  ! initialise connectivity
  ielel(1:,1:nel1)=getconn(nel1,nshape,ielnd(1:,1:nel1))
  ielsd(1:,1:nel1)=getsconn(nel1,nshape,ielel(1:,1:nel1))
  CALL corrconn(nel1,nshape,ielel(1:,1:nel1),ielsd(1:,1:nel1))

  ! initialise node type
  DO iel=1,nel1
    nodes(0:nshape-1)=ielnd(1:nshape,iel)
    IF (COUNT(indtype(nodes).LT.0_ink).EQ.3_ink) THEN
      l1:DO ii=0,nshape-1
        IF (indtype(nodes(ii)).GT.0_ink) EXIT l1
      ENDDO l1
      ii=MOD(ii+2_ink,nshape)
      jj=nodes(ii)
      IF (jj.LE.nnod) THEN
        j1=nodes(MOD(ii+1_ink,nshape))
        j2=nodes(MOD(ii+3_ink,nshape))
        IF (((indtype(j1).EQ.-2_ink).AND.(indtype(j2).EQ.-1_ink)).OR.     &
&           ((indtype(j2).EQ.-2_ink).AND.(indtype(j1).EQ.-1_ink))) THEN
          indtype(jj)=-3_ink
        ENDIF
      ENDIF
    ENDIF
  ENDDO

END SUBROUTINE init

SUBROUTINE init_comm()

  USE kinds_mod,    ONLY: ink
  USE integers_mod, ONLY: nel1
  USE paradef_mod,  ONLY: e_loc_glob,ielsort1
  USE utilities_mod,ONLY: sort
  USE error_mod,    ONLY: halt

  IMPLICIT NONE

  ielsort1(1:nel1)=sort(e_loc_glob(1:nel1))
  if (ielsort1(1).eq.-HUGE(1_ink)) then
    call halt("ERROR: sort failed for ielsort1",0)
  endif

END SUBROUTINE init_comm

SUBROUTINE init_defaults()

  USE kinds_mod,   ONLY: rlk,lok,ink
  USE strings_mod, ONLY: sfile
  USE integers_mod,ONLY: eos_type,max_seg,max_subseg
  USE reals_mod,   ONLY: time_start,time_end,dt_initial,dt_g,dt_min,    &
&                        dt_max,cfl_sf,div_sf,ccut,zcut,zerocut,pcut,   &
&                        eos_param,dencut,accut,cq1,cq2,kappaall,       &
&                        kappareg,pmeritall,pmeritreg
  USE logicals_mod,ONLY: zdtnotreg,zmidlength

  IMPLICIT NONE

  ! file defaults
  sfile='control'
  ! time defaults
  time_start=0.0_rlk
  time_end  =1.0_rlk
  dt_initial=1.0e-5_rlk
  dt_g      =1.02_rlk
  dt_min    =1.0e-8_rlk
  dt_max    =1.0e-1_rlk
  cfl_sf    =0.5_rlk
  div_sf    =0.25_rlk
  ! dt options
  zdtnotreg(:) =.FALSE._lok
  zmidlength(:)=.FALSE._lok
  ! cutoffs
  zcut=1.0e-8_rlk
  ccut=1.0e-6_rlk
  zerocut=1.0e-40_rlk
  pcut=1.0e-8_rlk
  dencut=1.0e-6_rlk
  accut=1.0e-6_rlk
  ! eos
  eos_type(:)=1_ink
  eos_param(1,:)=1.4_rlk
  eos_param(2:,:)=0.0_rlk
  ! artificial viscosity
  cq1=0.5_rlk
  cq2=0.75_rlk
  ! hourglass control
  kappaall=0.0_rlk
  kappareg(:)=0.0_rlk
  pmeritall=0.0_rlk
  pmeritreg(:)=0.0_rlk
  ! meshgen
  max_seg=50_ink
  max_subseg=5_ink
  ! ale

END SUBROUTINE init_defaults

SUBROUTINE init_parallel()

  USE kinds_mod,     ONLY: ink,lok
  USE paradef_mod,   ONLY: rankW,MProcW,NProcW,CommS,CommW,zparallel
  USE TYPH_util_mod, ONLY: TYPH_Init,TYPH_Get_Size,TYPH_Get_Rank,set_comm

  IMPLICIT NONE

  ! Local
  INTEGER(KIND=ink) :: ierr

  ierr=TYPH_Init()   
  ierr=TYPH_Get_Size(NProcW)
  zparallel=.FALSE._lok
  IF (NProcW.GT.1_ink) zparallel=.TRUE._lok
  ierr=TYPH_Get_Rank(RankW)
  MProcW=.FALSE._lok
  IF (RankW.EQ.0_ink) MProcW=.TRUE._lok
  ierr=set_comm(CommW)
  ierr=set_comm(CommS)

END SUBROUTINE init_parallel

SUBROUTINE init_parameters()

  USE kinds_mod,   ONLY: rlk,ink,lok
  USE reals_mod,   ONLY: kappaall,kappareg,pmeritall,pmeritreg,time_end,&
&                        time_start,time_alemin,time_alemax
  USE logicals_mod,ONLY: zhg,zsp,zale,zeul
  USE integers_mod,ONLY: nreg,nshape

  IMPLICIT NONE

  ! hourglass filter
  kappareg=MERGE(kappareg,kappaall,kappareg.GT.0.0_rlk)
  zhg=ANY(kappareg(1:nreg).GT.0.0_rlk)
  ! subzonal pressures
  pmeritreg=MERGE(pmeritreg,pmeritall,pmeritreg.GT.0.0_rlk)
  zsp=ANY(pmeritreg(1:nreg).GT.0.0_rlk)
  ! geometry
  nshape=4_ink
  ! ale
  zale=(time_alemin.LT.time_end).AND.(time_alemax.GT.time_start).AND.   &
&      (time_alemax.GT.time_alemin)
  IF (zale) THEN
!    zeul=
  ELSE
    zeul=.FALSE._lok
  ENDIF

END SUBROUTINE init_parameters  
