
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

MODULE kinds_mod

  INTEGER,PARAMETER :: ink=4,rlk=8,lok=4

END MODULE kinds_mod

MODULE parameters_mod

  USE kinds_mod,ONLY: ink,rlk

  INTEGER(KIND=ink),PARAMETER :: LN       =80_ink
  INTEGER(KIND=ink),PARAMETER :: LI       =100_ink
  REAL(KIND=rlk),   PARAMETER :: ONEBYNINE=1.0_rlk/9.0_rlk
  REAL(KIND=rlk),   PARAMETER :: PI       =3.1415926535897932385_rlk
  REAL(KIND=rlk),   PARAMETER :: TWO_PI   =6.2831853071795864770_rlk

END MODULE parameters_mod

MODULE integers_mod

  USE kinds_mod,     ONLY: ink
  USE parameters_mod,ONLY: LI

  ! dimensions
  INTEGER(KIND=ink)                  :: nel,nnod
  ! dimensions including ghosts
  INTEGER(KIND=ink)                  :: nel1,nnod1
  INTEGER(KIND=ink)                  :: nel2,nnod2
  ! sizes
  INTEGER(KIND=ink)                  :: nshape,nsz,nmat,nreg
  ! timestep
  INTEGER(KIND=ink)                  :: nstep,idtel,idtreg
  ! mesh
  INTEGER(KIND=ink)                  :: max_seg,max_subseg
  ! eos
  INTEGER(KIND=ink),DIMENSION(LI)    :: eos_type
  ! ale
  INTEGER(KIND=ink)                  :: adv_type,npatch
  INTEGER(KIND=ink),DIMENSION(LI)    :: patch_type,patch_motion,        &
&                                       patch_ntrigger
  INTEGER(KIND=ink),DIMENSION(LI,LI) :: patch_trigger
  ! parallelism
  INTEGER(KIND=ink)                  :: nprocw,nprocs,nprocr,commw,     &
&                                       comms,commr,rankw,ranks,rankr,  &
&                                       nthread

END MODULE integers_mod

MODULE reals_mod

  USE kinds_mod,     ONLY: rlk
  USE parameters_mod,ONLY: LI

  ! time 
  REAL(KIND=rlk)                 :: time,time_start,time_end,dt_min,    &
&                                   dt_initial,dt_max,cfl_sf,div_sf,    &
&                                   ale_sf,dt_g
  ! cut-off
  REAL(KIND=rlk)                 :: ccut,zcut,zerocut,pcut,dencut,accut
  ! q
  REAL(KIND=rlk)                 :: cq1,cq2
  ! eos
  REAL(KIND=rlk),DIMENSION(LI)   :: mat_rho,mat_ein
  REAL(KIND=rlk),DIMENSION(6,LI) :: eos_param
  ! hourglass
  REAL(KIND=rlk)                 :: kappaall,pmeritall
  REAL(KIND=rlk),DIMENSION(LI)   :: kappareg,pmeritreg
  ! ale
  REAL(KIND=rlk)                 :: time_alemin,time_alemax
  REAL(KIND=rlk),DIMENSION(LI)   :: patch_ontime,patch_offtime,         &
&                                   patch_minvel,patch_maxvel,patch_om

END MODULE reals_mod  

MODULE strings_mod

  USE parameters_mod,ONLY: LN

  CHARACTER(LEN=LN) :: sfile
  CHARACTER(LEN=8)  :: sdt 

END MODULE strings_mod 

MODULE logicals_mod

  USE kinds_mod,     ONLY: lok
  USE parameters_mod,ONLY: LI

  LOGICAL(KIND=lok)               :: zhg,zsp,zale,zaleon,zeul,zparallel,&
&                                    zmprocw,zmprocs,zmprocr
  LOGICAL(KIND=lok),DIMENSION(LI) :: zdtnotreg,zmidlength

END MODULE logicals_mod

MODULE pointers_mod

  USE kinds_mod,ONLY: ink,rlk

  INTEGER(KIND=ink),DIMENSION(:),  ALLOCATABLE        :: ielreg,ielmat, &
&                                                        indtype,       &
&                                                        ielsort1,      &
&                                                        iellocglob,    &
&                                                        indlocglob
  INTEGER(KIND=ink),DIMENSION(:,:),ALLOCATABLE        :: ielel,ielsd,   &
&                                                        ielownerproc,  &
&                                                        indownerproc
  INTEGER(KIND=ink),DIMENSION(:,:),ALLOCATABLE,TARGET :: ielnd
  REAL(KIND=rlk),   DIMENSION(:),  ALLOCATABLE        :: rho,qq,csqrd,  &
&                                                        pre,ein,elmass,&
&                                                        elvol,a1,a2,a3,&
&                                                        b1,b2,b3,ndx,  &
&                                                        ndy,ndu,ndv
  REAL(KIND=rlk),   DIMENSION(:,:),ALLOCATABLE        :: elx,ely,cnwt,  &
&                                                        qx,qy,spmass,  &
&                                                        cnmass

END MODULE pointers_mod

MODULE scratch_mod

  USE kinds_mod,ONLY: rlk,ink,lok

  REAL(KIND=rlk),   DIMENSION(:),  ALLOCATABLE,TARGET :: rscratch11,    &
&                                                        rscratch12,    &
&                                                        rscratch13,    &
&                                                        rscratch14,    &
&                                                        rscratch15,    &
&                                                        rscratch16
  REAL(KIND=rlk),   DIMENSION(:,:),ALLOCATABLE,TARGET :: rscratch21,    &
&                                                        rscratch22,    &
&                                                        rscratch23,    &
&                                                        rscratch24,    &
&                                                        rscratch25,    &
&                                                        rscratch26,    &
&                                                        rscratch27
  INTEGER(KIND=ink),DIMENSION(:),  ALLOCATABLE,TARGET :: iscratch11
  LOGICAL(KIND=lok),DIMENSION(:),  ALLOCATABLE,TARGET :: zscratch11

END MODULE scratch_mod

MODULE timing_mod

  USE kinds_mod, ONLY: rlk

  TYPE time_stats
     REAL(KIND=rlk) :: time_start
     REAL(KIND=rlk) :: time_end
     REAL(KIND=rlk) :: time_end_main
     REAL(KIND=rlk) :: time_total
     REAL(KIND=rlk) :: time_end_init
     REAL(KIND=rlk) :: time_hydro
     REAL(KIND=rlk) :: time_in_lag
     REAL(KIND=rlk) :: time_in_getdt
     REAL(KIND=rlk) :: time_in_io
     REAL(KIND=rlk) :: time_step_io
     REAL(KIND=rlk) :: time_in_getq
     REAL(KIND=rlk) :: time_in_gethg
     REAL(KIND=rlk) :: time_in_getsp
     REAL(KIND=rlk) :: time_in_getacc
     REAL(KIND=rlk) :: time_in_getfrc
     REAL(KIND=rlk) :: time_in_getein
     REAL(KIND=rlk) :: time_in_getpca
     REAL(KIND=rlk) :: time_in_getpci
     REAL(KIND=rlk) :: time_in_getpcl
     REAL(KIND=rlk) :: time_in_getgeoma
     REAL(KIND=rlk) :: time_in_getgeomi
     REAL(KIND=rlk) :: time_in_getgeoml
     REAL(KIND=rlk) :: time_in_comreg
     REAL(KIND=rlk) :: time_in_comms
     REAL(KIND=rlk) :: time_in_colls
     REAL(KIND=rlk) :: time_in_mshgen
     REAL(KIND=rlk) :: time_in_mshprt
     REAL(KIND=rlk) :: time_in_alestep
     REAL(KIND=rlk) :: time_in_alegetmesh
     REAL(KIND=rlk) :: time_in_alegetfvol
     REAL(KIND=rlk) :: time_in_aleadvect
     REAL(KIND=rlk) :: time_in_aleadvect_el
     REAL(KIND=rlk) :: time_in_update_el_basis
     REAL(KIND=rlk) :: time_in_update_el_var
     REAL(KIND=rlk) :: time_in_aleadvect_nd
     REAL(KIND=rlk) :: time_in_update_nd_basis
     REAL(KIND=rlk) :: time_in_update_nd_var
     REAL(KIND=rlk) :: time_in_aleupdate
  END TYPE time_stats
  TYPE(time_stats) :: bookleaf_times

END MODULE timing_mod
