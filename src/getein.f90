
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

module getein_kernel_mod
    implicit none
    contains
        attributes(global) subroutine getein_kernel(nshape, nel, ein_out,ein, dt, elfx, elfy, elu, elv, elmass, zerocut)
        implicit none
            integer,parameter::ink=4, rlk=8
            integer(kind=ink), value :: nshape, nel
            real(kind=rlk), dimension(:)  :: ein_out,ein, elmass
            real(kind=rlk), value :: dt, zerocut
            real(kind=rlk), dimension(:,:) :: elfx, elfy, elu, elv

            INTEGER                                :: iel
            REAL(KIND=rlk)                                   :: w1,t0,t1
            
            iel = threadIdx%x + (blockIdx%x-1)*blockDim%x

            if(iel<=nel) then
                w1=elfx(1,iel)*elu(1,iel)+elfy(1,iel)*elv(1,iel)+                 &
&                  elfx(2,iel)*elu(2,iel)+elfy(2,iel)*elv(2,iel)+                 &
&                  elfx(3,iel)*elu(3,iel)+elfy(3,iel)*elv(3,iel)+                 &
&                  elfx(4,iel)*elu(4,iel)+elfy(4,iel)*elv(4,iel)
      !# Missing code here that can't be merged
                w1=-w1/MAX(elmass(iel),zerocut)
                ein_out(iel)=ein(iel)+w1*dt
            endif

        end subroutine getein_kernel
end module getein_kernel_mod

MODULE getein_mod

  IMPLICIT NONE

  PUBLIC :: getein

CONTAINS

  SUBROUTINE getein_host(nshape,nel,ein_out,dt,elfx,elfy,elu,elv, elmass, ein)

    use cudafor
    use getein_kernel_mod
    USE kinds_mod,    ONLY: ink,rlk
    !USE pointers_mod, ONLY: elmass,ein
    USE reals_mod,    ONLY: zerocut
    USE timing_mod,   ONLY: bookleaf_times
    USE TYPH_util_mod,ONLY: get_time

    ! Argument list
    INTEGER(KIND=ink),                   INTENT(IN)  :: nshape,nel
    REAL(KIND=rlk),                      INTENT(IN)  :: dt
    !REAL(KIND=rlk),DIMENSION(nel),       INTENT(OUT) :: ein_out
    !REAL(KIND=rlk),DIMENSION(nshape,nel),INTENT(IN)  :: elfx,elfy,elu,  &
!&                                                       elv
    real(kind=rlk), dimension(:),allocatable, device  :: ein_out,ein, elmass
    real(kind=rlk), dimension(:,:), allocatable, device :: elfx, elfy, elu, elv

    ! Local
    !INTEGER(KIND=ink)                                :: iel
    REAL(KIND=rlk)                                   :: t0,t1

    integer::thread_num, block_num
    ! Timer

    ! ##############
    ! Predictor
    ! ##############
    thread_num = 128
    block_num = ceiling(real(nel)/thread_num)
    ! Timer
    t0=get_time()

    !# Missing code here that can't be merged

    ! FdS internal energy update

    call getein_kernel<<<block_num, thread_num>>>(nshape, nel, ein_out,ein, dt, elfx, elfy, elu, elv, elmass, zerocut)
    !# Missing code here that can't be merged

    ! Timing data
    t1=get_time()
    t1=t1-t0
    bookleaf_times%time_in_getein=bookleaf_times%time_in_getein+t1

  END SUBROUTINE getein_host

SUBROUTINE getein(nshape,nel,ein_out,dt,elfx,elfy,elu,elv)

    USE kinds_mod,    ONLY: ink,rlk
    USE pointers_mod, ONLY: elmass,ein
    USE reals_mod,    ONLY: zerocut
    USE timing_mod,   ONLY: bookleaf_times
    USE TYPH_util_mod,ONLY: get_time

    ! Argument list
    INTEGER(KIND=ink),                   INTENT(IN)  :: nshape,nel
    REAL(KIND=rlk),DIMENSION(nel),       INTENT(OUT) :: ein_out
    REAL(KIND=rlk),                      INTENT(IN)  :: dt
    REAL(KIND=rlk),DIMENSION(nshape,nel),INTENT(IN)  :: elfx,elfy,elu,  &
&                                                       elv
    ! Local
    INTEGER(KIND=ink)                                :: iel
    REAL(KIND=rlk)                                   :: w1,t0,t1

    ! Timer
    t0=get_time()

    !# Missing code here that can't be merged

    ! FdS internal energy update
    DO iel=1,nel
      w1=elfx(1,iel)*elu(1,iel)+elfy(1,iel)*elv(1,iel)+                 &
&        elfx(2,iel)*elu(2,iel)+elfy(2,iel)*elv(2,iel)+                 &
&        elfx(3,iel)*elu(3,iel)+elfy(3,iel)*elv(3,iel)+                 &
&        elfx(4,iel)*elu(4,iel)+elfy(4,iel)*elv(4,iel)
      !# Missing code here that can't be merged
      w1=-w1/MAX(elmass(iel),zerocut)
      ein_out(iel)=ein(iel)+w1*dt
    ENDDO

    !# Missing code here that can't be merged

    ! Timing data
    t1=get_time()
    t1=t1-t0
    bookleaf_times%time_in_getein=bookleaf_times%time_in_getein+t1

  END SUBROUTINE getein

END MODULE getein_mod
