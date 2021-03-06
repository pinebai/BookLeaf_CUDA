
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

module getsp_kernel_mod
    implicit none
    contains
    attributes(global) subroutine getsp_kernel(nshape, nel, rho, elx, ely, elfx, elfy, ielreg, csqrd, spmass,&
&                                       pmeritreg)
        integer,parameter::ink=4, rlk=8
            !device data
            integer(KIND=ink),DIMENSION(:)       :: ielreg
            REAL(KIND=rlk),DIMENSION(:)        :: rho, csqrd
            REAL(KIND=rlk),DIMENSION(:,:),INTENT(IN)    :: elx,ely
            REAL(KIND=rlk),DIMENSION(:,:),INTENT(INOUT) :: elfx,elfy
            REAL(KIND=rlk),DIMENSION(:)   :: pmeritreg
            real(kind=rlk), DIMENSION(:,:)::            spmass

            !local data
            INTEGER(KIND=ink),                   value         :: nshape,nel
            INTEGER(KIND=ink)                                  :: ireg,j1,j2
            REAL(KIND=rlk)                                     :: w1,w2,w3,w4,  &
&                                                                 w5,w6,x1,x2,  &
&                                                                 x3,x4,y1,y2,  &
&                                                                 y3,y4,t0,t1
            REAL(KIND=rlk),DIMENSION(nshape,nshape)            :: lfx,lfy
            integer:: idx

            idx = threadIdx%x + (blockIdx%x-1)*blockDim%x

      ! info
      ireg=ABS(ielreg(idx))
      w1=pmeritreg(ireg)
      ! centroid
      x3=0.25_rlk*(elx(1,idx)+elx(2,idx)+elx(3,idx)+elx(4,idx))
      y3=0.25_rlk*(ely(1,idx)+ely(2,idx)+ely(3,idx)+ely(4,idx))
      ! initialise local force
      lfx=0.0_rlk
      lfy=0.0_rlk
      ! loop over sub-elements
      DO j1=1,nshape
        ! construct sub-volumes
        x1=elx(j1,idx)
        y1=ely(j1,idx)
        j2=MOD(j1,nshape)+1_ink
        x2=0.5_rlk*(x1+elx(j2,idx))
        y2=0.5_rlk*(y1+ely(j2,idx))
        j2=MOD(j1+2,nshape)+1_ink
        x4=0.5_rlk*(x1+elx(j2,idx))
        y4=0.5_rlk*(y1+ely(j2,idx))
        !# Missing code here that can't be merged
        w3=0.25_rlk*(-x1+x2+x3-x4)
        w4=0.25_rlk*(-x1-x2+x3+x4)
        w5=0.25_rlk*(-y1+y2+y3-y4)
        w6=0.25_rlk*(-y1-y2+y3+y4)
        w2=4.0_rlk*(w3*w6-w4*w5)
        ! calculate change in pressure
        w2=spmass(j1,idx)/w2
        w2=w2-rho(idx)
        w2=csqrd(idx)*w2
        ! add to forces
        lfx(j1,1)=w2*( w5-w6)
        lfx(j1,2)=w2*( w5+w6)
        lfx(j1,3)=w2*(-w5+w6)
        lfx(j1,4)=w2*(-w5-w6)
        lfy(j1,1)=w2*(-w3+w4)
        lfy(j1,2)=w2*(-w3-w4)
        lfy(j1,3)=w2*( w3-w4)
        lfy(j1,4)=w2*( w3+w4)
      ENDDO
      ! distribute forces
      w2=0.5_rlk*(lfx(1,4)+lfx(4,2))
      w3=0.5_rlk*(lfx(1,2)+lfx(2,4))
      w4=0.5_rlk*(lfx(2,2)+lfx(3,4))
      w5=0.5_rlk*(lfx(4,4)+lfx(3,2))
      w6=0.25_rlk*(lfx(1,3)+lfx(2,3)+lfx(3,3)+lfx(4,3))
      elfx(1,idx)=elfx(1,idx)+w1*(lfx(1,1)+w2+w3+w6)
      elfx(2,idx)=elfx(2,idx)+w1*(lfx(2,1)+w4+w3+w6)
      elfx(3,idx)=elfx(3,idx)+w1*(lfx(3,1)+w4+w5+w6)
      elfx(4,idx)=elfx(4,idx)+w1*(lfx(4,1)+w2+w5+w6)
      w2=0.5_rlk*(lfy(1,4)+lfy(4,2))
      w3=0.5_rlk*(lfy(1,2)+lfy(2,4))
      w4=0.5_rlk*(lfy(2,2)+lfy(3,4))
      w5=0.5_rlk*(lfy(4,4)+lfy(3,2))
      w6=0.25_rlk*(lfy(1,3)+lfy(2,3)+lfy(3,3)+lfy(4,3))
      elfy(1,idx)=elfy(1,idx)+w1*(lfy(1,1)+w2+w3+w6)
      elfy(2,idx)=elfy(2,idx)+w1*(lfy(2,1)+w4+w3+w6)
      elfy(3,idx)=elfy(3,idx)+w1*(lfy(3,1)+w4+w5+w6)
      elfy(4,idx)=elfy(4,idx)+w1*(lfy(4,1)+w2+w5+w6)

    end subroutine getsp_kernel
end module getsp_kernel_mod

MODULE getsp_mod

  IMPLICIT NONE

  PUBLIC :: getsp

CONTAINS

  SUBROUTINE getsp_host(nshape,nel,rho,elx,ely,elfx,elfy, pmeritreg, ielreg, csqrd, d_spmass)

    use cudafor
    use getsp_kernel_mod
    USE kinds_mod,    ONLY: ink,rlk
    USE timing_mod,   ONLY: bookleaf_times
    use pointers_mod, only: spmass
    USE TYPH_util_mod,ONLY: get_time

    ! Argument list
    integer(KIND=ink),DIMENSION(:), device        :: ielreg
    REAL(KIND=rlk),DIMENSION(:),device        :: rho, csqrd
    REAL(KIND=rlk),DIMENSION(:,:), device    :: elx,ely
    REAL(KIND=rlk),DIMENSION(:,:),device :: elfx,elfy
    REAL(KIND=rlk),DIMENSION(:),device   :: pmeritreg
    real(kind=rlk), DIMENSION(:,:), device::            d_spmass
    INTEGER(KIND=ink),                   INTENT(IN)    :: nshape,nel

    ! Local
    INTEGER(KIND=ink)                                  :: iel,ireg,j1,j2
    REAL(KIND=rlk)                                     :: w1,w2,w3,w4,  &
&                                                         w5,w6,x1,x2,  &
&                                                         x3,x4,y1,y2,  &
&                                                         y3,y4,t0,t1
    REAL(KIND=rlk),DIMENSION(nshape,nshape)            :: lfx,lfy
    integer::block_num, thread_num, istat
    thread_num = 128
    block_num = ceiling(real(nel)/thread_num)

    ! Timer
    t0=get_time()
    print *, 'called   here', size(spmass)
    call getsp_kernel<<<block_num, thread_num>>>(nshape, nel, rho, elx, ely, elfx, elfy, ielreg, csqrd, d_spmass,&
&                                       pmeritreg)

    !# Missing code here that can't be merged

    ! Timing data
    t1=get_time()
    t1=t1-t0
    bookleaf_times%time_in_getsp=bookleaf_times%time_in_getsp+t1

  END SUBROUTINE getsp_host

  SUBROUTINE getsp(nshape,nel,rho,elx,ely,elfx,elfy)

      USE kinds_mod,    ONLY: ink,rlk
      USE reals_mod,    ONLY: pmeritreg
      USE pointers_mod, ONLY: ielreg,csqrd,spmass
      USE timing_mod,   ONLY: bookleaf_times
      USE TYPH_util_mod,ONLY: get_time

      ! Argument list
      INTEGER(KIND=ink),                   INTENT(IN)    :: nshape,nel
      REAL(KIND=rlk),DIMENSION(nel),       INTENT(IN)    :: rho
      REAL(KIND=rlk),DIMENSION(nshape,nel),INTENT(IN)    :: elx,ely
      REAL(KIND=rlk),DIMENSION(nshape,nel),INTENT(INOUT) :: elfx,elfy
      ! Local
      INTEGER(KIND=ink)                                  :: iel,ireg,j1,j2
      REAL(KIND=rlk)                                     :: w1,w2,w3,w4,  &
  &                                                         w5,w6,x1,x2,  &
  &                                                         x3,x4,y1,y2,  &
  &                                                         y3,y4,t0,t1
      REAL(KIND=rlk),DIMENSION(nshape,nshape)            :: lfx,lfy

      ! Timer
      t0=get_time()

      !# Missing code here that can't be merged
      DO iel=1,nel
        ! info
        ireg=ABS(ielreg(iel))
        w1=pmeritreg(ireg)
        ! centroid
        x3=0.25_rlk*(elx(1,iel)+elx(2,iel)+elx(3,iel)+elx(4,iel))
        y3=0.25_rlk*(ely(1,iel)+ely(2,iel)+ely(3,iel)+ely(4,iel))
        ! initialise local force
        lfx=0.0_rlk
        lfy=0.0_rlk
        ! loop over sub-elements
        DO j1=1,nshape
          ! construct sub-volumes
          x1=elx(j1,iel)
          y1=ely(j1,iel)
          j2=MOD(j1,nshape)+1_ink
          x2=0.5_rlk*(x1+elx(j2,iel))
          y2=0.5_rlk*(y1+ely(j2,iel))
          j2=MOD(j1+2,nshape)+1_ink
          x4=0.5_rlk*(x1+elx(j2,iel))
          y4=0.5_rlk*(y1+ely(j2,iel))
          !# Missing code here that can't be merged
          w3=0.25_rlk*(-x1+x2+x3-x4)
          w4=0.25_rlk*(-x1-x2+x3+x4)
          w5=0.25_rlk*(-y1+y2+y3-y4)
          w6=0.25_rlk*(-y1-y2+y3+y4)
          w2=4.0_rlk*(w3*w6-w4*w5)
          ! calculate change in pressure
          w2=spmass(j1,iel)/w2
          w2=w2-rho(iel)
          w2=csqrd(iel)*w2
          ! add to forces
          lfx(j1,1)=w2*( w5-w6)
          lfx(j1,2)=w2*( w5+w6)
          lfx(j1,3)=w2*(-w5+w6)
          lfx(j1,4)=w2*(-w5-w6)
          lfy(j1,1)=w2*(-w3+w4)
          lfy(j1,2)=w2*(-w3-w4)
          lfy(j1,3)=w2*( w3-w4)
          lfy(j1,4)=w2*( w3+w4)
        ENDDO
        ! distribute forces
        w2=0.5_rlk*(lfx(1,4)+lfx(4,2))
        w3=0.5_rlk*(lfx(1,2)+lfx(2,4))
        w4=0.5_rlk*(lfx(2,2)+lfx(3,4))
        w5=0.5_rlk*(lfx(4,4)+lfx(3,2))
        w6=0.25_rlk*(lfx(1,3)+lfx(2,3)+lfx(3,3)+lfx(4,3))
        elfx(1,iel)=elfx(1,iel)+w1*(lfx(1,1)+w2+w3+w6)
        elfx(2,iel)=elfx(2,iel)+w1*(lfx(2,1)+w4+w3+w6)
        elfx(3,iel)=elfx(3,iel)+w1*(lfx(3,1)+w4+w5+w6)
        elfx(4,iel)=elfx(4,iel)+w1*(lfx(4,1)+w2+w5+w6)
        w2=0.5_rlk*(lfy(1,4)+lfy(4,2))
        w3=0.5_rlk*(lfy(1,2)+lfy(2,4))
        w4=0.5_rlk*(lfy(2,2)+lfy(3,4))
        w5=0.5_rlk*(lfy(4,4)+lfy(3,2))
        w6=0.25_rlk*(lfy(1,3)+lfy(2,3)+lfy(3,3)+lfy(4,3))
        elfy(1,iel)=elfy(1,iel)+w1*(lfy(1,1)+w2+w3+w6)
        elfy(2,iel)=elfy(2,iel)+w1*(lfy(2,1)+w4+w3+w6)
        elfy(3,iel)=elfy(3,iel)+w1*(lfy(3,1)+w4+w5+w6)
        elfy(4,iel)=elfy(4,iel)+w1*(lfy(4,1)+w2+w5+w6)
      ENDDO

      ! Timing data
      t1=get_time()
      t1=t1-t0
      bookleaf_times%time_in_getsp=bookleaf_times%time_in_getsp+t1

    END SUBROUTINE getsp

END MODULE getsp_mod
