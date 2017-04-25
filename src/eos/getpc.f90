
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

module getpc_mod_kernel
    implicit none
    contains 
        attributes(device) subroutine merge_kernel(nel, csqrd)
        use kinds_mod,      only: ink, rlk
        INTEGER(KIND=ink),value,       INTENT(IN)      :: nel
        REAL(KIND=rlk),   DIMENSION(:),INTENT(OUT)     :: csqrd
        integer::idx

        idx = threadIdx%x + (blockIdx%x-1)*blockDim%x

        if(idx<=nel) then
            if(csqrd(idx)<0) csqrd(idx) = 0.0_rlk
        endif
        end subroutine merge_kernel

        attributes(global) subroutine getpc_kernel(nel, ielmat, rho, ein, pre, csqrd, eos_type, eos_param, pcut)
        use kinds_mod,      only: ink, rlk
        use eos_mod,        only: getpre_kernel, getcc_kernel

        INTEGER(KIND=ink),value,       INTENT(IN)      :: nel
        real(kind=rlk), value                         :: pcut
        INTEGER(KIND=ink),DIMENSION(:),INTENT(IN)     :: ielmat, eos_type
        REAL(KIND=rlk),   DIMENSION(:),INTENT(IN)     :: rho,ein
        REAL(KIND=rlk),   DIMENSION(:,:),INTENT(IN)   :: eos_param
        REAL(KIND=rlk),   DIMENSION(:),INTENT(OUT)    :: pre,csqrd

        !local
        integer::idx

        idx = threadIdx%x + (blockIdx%x-1)*blockDim%x
        if(idx<=nel) then
            pre(idx)=getpre_kernel(ielmat(idx),rho(idx),ein(idx), eos_type, eos_param, pcut)
            csqrd(idx)=getcc_kernel(ielmat(idx),rho(idx),ein(idx), eos_type, eos_param, pcut)
        endif
        
        !csqrd=MERGE(csqrd,0.0_rlk,csqrd.GT.0.0_rlk)

        end subroutine getpc_kernel
end module getpc_mod_kernel

MODULE getpc_mod

  IMPLICIT NONE

  PUBLIC :: getpc, getpc_host

CONTAINS

  SUBROUTINE getpc_host(nel,ielmat,rho,ein,pre,csqrd,eos_type, eos_param, timer)

    USE kinds_mod,    ONLY: ink,rlk
    use getpc_mod_kernel
    !USE eos_mod,      ONLY: getpre,getcc
    USE reals_mod,      ONLY: pcut
    USE TYPH_util_mod,ONLY: get_time

    ! Argument list
    INTEGER(KIND=ink),             INTENT(IN)            :: nel
    INTEGER(KIND=ink),DIMENSION(:),INTENT(IN), device    :: ielmat, eos_type
    REAL(KIND=rlk),   DIMENSION(:),INTENT(IN), device    :: rho,ein
    REAL(KIND=rlk),   DIMENSION(:),INTENT(OUT),device    :: pre,csqrd
    REAL(KIND=rlk),   DIMENSION(:,:),INTENT(IN),device   :: eos_param
    REAL(KIND=rlk),                  INTENT(INOUT) :: timer
    ! Local
    INTEGER(KIND=ink)                            :: thread_num, block_num
    REAL(KIND=rlk)                               :: t0,t1

    ! Timer
    t0=get_time()

    thread_num = 128
    block_num = ceiling(real(nel)/thread_num)
    !# Missing code here that can't be merged
    call getpc_kernel<<<block_num, thread_num>>>(nel,ielmat,rho,ein, pre, csqrd, eos_type, eos_param, pcut)
    call merge_kernel<<<block_num, thread_num>>>(nel, csqrd)
    ! update pressure and sound speed
    ! correct negative sound speeds

    !# Missing code here that can't be merged

    ! Timing data
    t1=get_time()
    t1=t1-t0
    timer=timer+t1

  END SUBROUTINE getpc_host

  SUBROUTINE getpc(nel,ielmat,rho,ein,pre,csqrd,timer)

    USE kinds_mod,    ONLY: ink,rlk
    USE eos_mod,      ONLY: getpre,getcc
    USE TYPH_util_mod,ONLY: get_time

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)    :: nel
    INTEGER(KIND=ink),DIMENSION(nel),INTENT(IN)    :: ielmat
    REAL(KIND=rlk),   DIMENSION(nel),INTENT(IN)    :: rho,ein
    REAL(KIND=rlk),   DIMENSION(nel),INTENT(OUT)   :: pre,csqrd
    REAL(KIND=rlk),                  INTENT(INOUT) :: timer
    ! Local
    INTEGER(KIND=ink)                            :: iel
    REAL(KIND=rlk)                               :: t0,t1

    ! Timer
    t0=get_time()

    !# Missing code here that can't be merged

    ! update pressure and sound speed
    DO iel=1,nel
      pre(iel)=getpre(ielmat(iel),rho(iel),ein(iel))
      csqrd(iel)=getcc(ielmat(iel),rho(iel),ein(iel))
    ENDDO
    ! correct negative sound speeds
    csqrd=MERGE(csqrd,0.0_rlk,csqrd.GT.0.0_rlk)

    !# Missing code here that can't be merged

    ! Timing data
    t1=get_time()
    t1=t1-t0
    timer=timer+t1

  END SUBROUTINE getpc
END MODULE getpc_mod  
