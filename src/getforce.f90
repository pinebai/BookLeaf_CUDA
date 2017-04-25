
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


module getforce_kernel_mod

contains
    attributes(global) subroutine  pressure_force_kernel(d_du, d_dv, d_a1, d_a3, d_b1, d_b3, d_pre, nel)
    implicit none
    integer,parameter::ink=4, rlk=8
    REAL(KIND=rlk),dimension(:)     :: d_a1, d_a3, d_b1, d_b3, d_pre
    REAL(KIND=rlk),DIMENSION(:,:)   :: d_du,d_dv
    integer(kind=ink), value :: nel
    real(kind=rlk)::w1
    integer::idx
    real(kind=rlk)::a1, a3, b1, b3

    idx = threadIdx%x + (blockIdx%x-1)*blockDim%x
    if (idx<=nel) then
        w1=d_pre(idx)
        a1 = d_a1(idx)
        a3 = d_a3(idx)
        b1 = d_b1(idx)
        b3 = d_b3(idx)

        d_du(1,idx)=w1*(-b3+b1)
        d_du(2,idx)=w1*( b3+b1)
        d_du(3,idx)=w1*( b3-b1)
        d_du(4,idx)=w1*(-b3-b1)
        d_dv(1,idx)=w1*( a3-a1)
        d_dv(2,idx)=w1*(-a3-a1)
        d_dv(3,idx)=w1*(-a3+a1)
        d_dv(4,idx)=w1*( a3+a1)
    endif
    end subroutine pressure_force_kernel


    attributes(global) subroutine  artificial_viscosity_force_kernel(d_du, d_dv, d_qx, d_qy, nel)
    implicit none
    integer,parameter::ink=4, rlk=8
    REAL(KIND=rlk),DIMENSION(:,:)   :: d_du,d_dv, d_qx, d_qy
    integer(kind=ink), value :: nel
    integer::idx
    real(kind=rlk)::qx1, qx2, qx3,qx4, qy1, qy2, qy3, qy4
    real(kind=rlk)::elfx1, elfx2, elfx3, elfx4, elfy1, elfy2, elfy3, elfy4

    idx = threadIdx%x + (blockIdx%x-1)*blockDim%x
    if (idx<=nel) then
        qx1 = d_qx(1, idx)
        qx2 = d_qx(2, idx)
        qx3 = d_qx(3, idx)
        qx4 = d_qx(4, idx)

        qy1 = d_qy(1, idx)
        qy2 = d_qy(2, idx)
        qy3 = d_qy(3, idx)
        qy4 = d_qy(4, idx)

        d_du(1,idx) = d_du(1,idx)+qx1
        d_du(2,idx) = d_du(2,idx)-qx1
        d_dv(1,idx) = d_dv(1,idx)+qy1
        d_dv(2,idx) = d_dv(2,idx)-qy1

        d_du(2,idx) = d_du(2,idx)+qx2
        d_du(3,idx) = d_du(3,idx)-qx2
        d_dv(2,idx) = d_dv(2,idx)+qy2
        d_dv(3,idx) = d_dv(3,idx)-qy2

        d_du(3,idx) = d_du(3,idx)+qx3
        d_du(4,idx) = d_du(4,idx)-qx3
        d_dv(3,idx) = d_dv(3,idx)+qy3
        d_dv(4,idx) = d_dv(4,idx)-qy3

        d_du(4,idx) = d_du(4,idx)+qx4
        d_du(1,idx) = d_du(1,idx)-qx4
        d_dv(4,idx) = d_dv(4,idx)+qy4
        d_dv(1,idx) = d_dv(1,idx)-qy4
   endif
    end subroutine artificial_viscosity_force_kernel
end module getforce_kernel_mod


MODULE getforce_mod

  IMPLICIT NONE

  PUBLIC :: getforce

CONTAINS

  SUBROUTINE getforce_host(nshape,nel,dt, zflag, d_elx, d_ely, d_elu, d_elv, d_du,d_dv, & 
&                     d_a1, d_a3, d_b1, d_b3, d_pre, d_qx, d_qy,&
&                     d_pmeritreg, d_ielreg, d_csqrd, d_spmass, d_rho, d_elvol, d_kappareg)


    use cudafor
    use getforce_kernel_mod
    USE kinds_mod,    ONLY: ink,rlk,lok
    USE logicals_mod, ONLY: zhg,zsp
    USE pointers_mod, ONLY: a1,a3,b1,b3,qx,qy
    USE gethg_mod,    ONLY: gethg, gethg_host
    USE getsp_mod,    ONLY: getsp, getsp_host
    USE timing_mod,   ONLY: bookleaf_times
    USE TYPH_util_mod,ONLY: get_time

    ! Argument list
    INTEGER(KIND=ink),                   INTENT(IN)  :: nshape,nel
    REAL(KIND=rlk),                      INTENT(IN)  :: dt
    !REAL(KIND=rlk),DIMENSION(nshape,nel),INTENT(IN)  :: elx,ely,elu,elv

    !REAL(KIND=rlk),DIMENSION(nshape,nel),INTENT(OUT) :: elfx,elfy
    !REAL(KIND=rlk),DIMENSION(nel),       INTENT(IN)  :: pre,rho
    LOGICAL(KIND=lok),                   INTENT(IN)  :: zflag
    ! Local
    INTEGER(KIND=ink),PARAMETER                      :: istrip=48_ink
    INTEGER(KIND=ink)                                :: i0,i1,jj,jp,iel
    REAL(KIND=rlk)                                   :: w1,t0,t1

    !device 
    REAL(KIND=rlk),DIMENSION(:,:),allocatable,device   :: d_elx,d_ely,d_elu,d_elv
    REAL(KIND=rlk),allocatable,dimension(:),device     :: d_qq, d_csqrd, d_a1, d_a3, d_b1, d_b3, d_pre
    REAL(KIND=rlk),allocatable,dimension(:,:),device   :: d_qx, d_qy
    REAL(KIND=rlk),DIMENSION(:,:),allocatable,device   :: d_dx,d_dy,d_du,d_dv,d_scratch
    integer(kind=ink), dimension(:), device            :: d_ielreg
    REAL(KIND=rlk),DIMENSION(:), device                :: d_rho, d_kappareg, d_elvol
    REAL(KIND=rlk),DIMENSION(:), device                :: d_pmeritreg
    real(kind=rlk), DIMENSION(:,:), device             :: d_spmass

    integer::block_num, thread_num, istat
    thread_num = 128
    block_num = ceiling(real(nel)/thread_num)
    ! Timer
    t0=get_time()

    ! Pressure force

    call pressure_force_kernel<<<block_num, thread_num>>>(d_du, d_dv, d_a1, d_a3, d_b1, d_b3, d_pre, nel)
    call artificial_viscosity_force_kernel<<<block_num, thread_num>>>(d_du, d_dv, d_qx, d_qy, nel)

    ! Subzonal pressure force
    IF (zsp) CALL getsp_host(nshape,nel,d_rho,d_elx,d_ely,d_du,d_dv, d_pmeritreg, d_ielreg, d_csqrd, d_spmass)
    !getsp(nshape,nel,rho,elx,ely,elfx,elfy)

    !# Missing code here that can't be merged
    IF (zflag) THEN
      !# Missing code here that can't be merged
      ! Anti-hourglass force
      IF (zhg) CALL gethg_host(nshape,nel,dt,d_rho,d_elu,d_elv,d_du,d_dv, d_kappareg, d_elvol, d_ielreg)
    ENDIF

    ! Timing data
    t1=get_time()
    t1=t1-t0
    bookleaf_times%time_in_getfrc=bookleaf_times%time_in_getfrc+t1

  END SUBROUTINE getforce_host

  SUBROUTINE getforce(nshape,nel,dt,elx,ely,elu,elv,elfx,elfy,pre,rho,  &
&                     zflag)

    USE kinds_mod,    ONLY: ink,rlk,lok
    USE logicals_mod, ONLY: zhg,zsp
    USE pointers_mod, ONLY: a1,a3,b1,b3,qx,qy
    USE gethg_mod,    ONLY: gethg
    USE getsp_mod,    ONLY: getsp
    USE timing_mod,   ONLY: bookleaf_times
    USE TYPH_util_mod,ONLY: get_time

    ! Argument list
    INTEGER(KIND=ink),                   INTENT(IN)  :: nshape,nel
    REAL(KIND=rlk),                      INTENT(IN)  :: dt
    REAL(KIND=rlk),DIMENSION(nshape,nel),INTENT(IN)  :: elx,ely,elu,elv
    REAL(KIND=rlk),DIMENSION(nshape,nel),INTENT(OUT) :: elfx,elfy
    REAL(KIND=rlk),DIMENSION(nel),       INTENT(IN)  :: pre,rho
    LOGICAL(KIND=lok),                   INTENT(IN)  :: zflag
    ! Local
    INTEGER(KIND=ink),PARAMETER                      :: istrip=48_ink
    INTEGER(KIND=ink)                                :: i0,i1,jj,jp,iel
    REAL(KIND=rlk)                                   :: w1,t0,t1

    ! Timer
    t0=get_time()

    ! Pressure force
    DO i0=1,nel,istrip
      i1=MIN(nel,i0+istrip-1)
      DO iel=i0,i1
        w1=pre(iel)
        elfx(1,iel)=w1*(-b3(iel)+b1(iel))
        elfx(2,iel)=w1*( b3(iel)+b1(iel))
        elfx(3,iel)=w1*( b3(iel)-b1(iel))
        elfx(4,iel)=w1*(-b3(iel)-b1(iel))
        elfy(1,iel)=w1*( a3(iel)-a1(iel))
        elfy(2,iel)=w1*(-a3(iel)-a1(iel))
        elfy(3,iel)=w1*(-a3(iel)+a1(iel))
        elfy(4,iel)=w1*( a3(iel)+a1(iel))
      ENDDO
    ENDDO

    ! Artificial viscosity force
    !# Missing code here that can't be merged
    DO i0=1,nel,istrip
      i1=MIN(nel,i0+istrip-1)
      DO iel=i0,i1
        DO jj=1,nshape
          jp=jj+1_ink
          IF (jp.GT.4_ink) jp=1_ink
          elfx(jj,iel)=elfx(jj,iel)+qx(jj,iel)
          elfx(jp,iel)=elfx(jp,iel)-qx(jj,iel)
          elfy(jj,iel)=elfy(jj,iel)+qy(jj,iel)
          elfy(jp,iel)=elfy(jp,iel)-qy(jj,iel)
        ENDDO
      ENDDO
    ENDDO

    ! Subzonal pressure force
    IF (zsp) CALL getsp(nshape,nel,rho,elx,ely,elfx,elfy)

    !# Missing code here that can't be merged
    IF (zflag) THEN
      !# Missing code here that can't be merged
      ! Anti-hourglass force
      IF (zhg) CALL gethg(nshape,nel,dt,rho,elu,elv,elfx,elfy)
    ENDIF

    ! Timing data
    t1=get_time()
    t1=t1-t0
    bookleaf_times%time_in_getfrc=bookleaf_times%time_in_getfrc+t1

  END SUBROUTINE getforce
END MODULE getforce_mod  
