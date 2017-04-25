
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

module getacc_mod_kernel
    implicit none
    contains
        attributes(global) subroutine construct_force(nes, zparallel, ielsort1, ielnd, &
&                                                     cnmass, ndmass, rho, cnwt, ndarea, &
&                                                     ndub, ndvb, cnfx, cnfy,jj, zerocut)

    integer,parameter::ink=4, rlk=8, lok=4

    INTEGER(KIND=ink), value,            INTENT(IN)  :: nes, jj
    integer(kind=ink), dimension(:,:)                :: ielnd
    integer(kind=ink), dimension(:)                  :: ielsort1
    REAL(KIND=rlk),DIMENSION(:,:),INTENT(IN)         :: cnfx,cnfy
    REAL(KIND=rlk),DIMENSION(:), INTENT(OUT)         :: ndub,ndvb
    REAL(KIND=rlk),DIMENSION(:),  INTENT(IN)         :: rho
    real(kind=rlk), dimension(:,:)                   :: cnmass,cnwt
    real(kind=rlk), value                            :: zerocut
    LOGICAL(KIND=lok), value                         :: zparallel 
    ! Local
    INTEGER(KIND=ink)                                :: ii,iel,inod
    REAL(KIND=rlk),DIMENSION(:)                    :: ndmass,ndarea
    integer :: idx

    idx = threadIdx%x + (blockIdx%x-1)*blockDim%x

    if (idx<=nes) then
        IF (zparallel) THEN
          iel=ielsort1(idx)
        ELSE
          iel=idx
        ENDIF
        inod=ielnd(jj,iel)
        IF (cnmass(jj,iel).GT.zerocut) THEN
          ndmass(inod)=ndmass(inod)+cnmass(jj,iel)
        ELSE
          ii=jj-1_ink
          IF (ii.EQ.0_ink) ii=4_ink
          IF (cnmass(ii,iel).GT.zerocut) THEN
            ndmass(inod)=ndmass(inod)+cnmass(ii,iel)
          ELSE
            ndmass(inod)=ndmass(inod)+rho(iel)*cnwt(jj,iel)
          ENDIF
        ENDIF
        ndarea(inod)=ndarea(inod)+cnwt(jj,iel)
        ndub(inod)=ndub(inod)+cnfx(jj,iel)
        ndvb(inod)=ndvb(inod)+cnfy(jj,iel)
    endif
    end subroutine construct_force

    attributes(global) subroutine calculate_acceleration_kernle(nnod, dencut, ndarea, ndmass, ndub, ndvb,zerocut)
    integer,parameter::ink=4, rlk=8

    INTEGER(KIND=ink), value,            INTENT(IN)  :: nnod
    REAL(KIND=rlk),DIMENSION(:), INTENT(OUT)         :: ndub,ndvb
    REAL(KIND=rlk),DIMENSION(:)                      :: ndmass,ndarea
    real(kind=rlk), value                            :: zerocut, dencut
    REAL(KIND=rlk)                                   :: w1
    ! Local
    INTEGER(KIND=ink)                                :: idx

    idx = threadIdx%x + (blockIdx%x-1)*blockDim%x
    if(idx<=nnod) then
      w1=dencut*ndarea(idx)
      IF (ndmass(idx).GT.w1) THEN
        ndub(idx)=ndub(idx)/ndmass(idx)
        ndvb(idx)=ndvb(idx)/ndmass(idx)
      ELSE
        ndub(idx)=0.0_rlk
        ndvb(idx)=0.0_rlk
        ndmass(idx)=MAX(zerocut,w1)
      ENDIF
    endif 
    end subroutine calculate_acceleration_kernle

    attributes(global) subroutine boundary_kernel(nnod, accut, indtype, ndub, ndvb)
    integer,parameter::ink=4, rlk=8

    INTEGER(KIND=ink),value,             INTENT(IN)  :: nnod
    real(kind=rlk), value                            :: accut
    integer(kind=ink), dimension(:)                  :: indtype
    REAL(KIND=rlk),DIMENSION(:), INTENT(OUT)         :: ndub,ndvb
    ! Local
    REAL(KIND=rlk)                                   :: w1,w2
    integer::idx

    idx = threadIdx%x + (blockIdx%x-1)*blockDim%x
    w1=accut*accut
    if(idx<=nnod) then 
      SELECT CASE(indtype(idx))
        CASE DEFAULT
        CASE(-1_ink)
          ndub(idx)=0.0_rlk
        CASE(-2_ink)
          ndvb(idx)=0.0_rlk
        CASE(-3_ink)
          ndub(idx)=0.0_rlk
          ndvb(idx)=0.0_rlk
      END SELECT
      w2=ndub(idx)*ndub(idx)+ndvb(idx)*ndvb(idx)
      IF (w2.LT.w1) THEN
        ndub(idx)=0.0_rlk
        ndvb(idx)=0.0_rlk
      ENDIF
    end if
    end subroutine boundary_kernel


    attributes(global) subroutine average_velocity_kernel(nnod, ndu, ndv, ndub, ndvb, dt, dt05)
    integer,parameter::ink=4, rlk=8

    INTEGER(KIND=ink),value,             INTENT(IN)  :: nnod
    real(kind=rlk), value                            :: dt, dt05
    REAL(KIND=rlk),DIMENSION(:)                      :: ndub,ndvb, ndu, ndv
    ! Local
    REAL(KIND=rlk)                                   :: w1,w2
    integer::idx

    ! Calculate average velocity
    idx = threadIdx%x + (blockIdx%x-1)*blockDim%x
    if(idx<=nnod) then
      w1=ndu(idx)
      w2=ndv(idx)
      ndu(idx)=w1+dt*ndub(idx)
      ndv(idx)=w2+dt*ndvb(idx)
      ndub(idx)=w1+dt05*ndub(idx)
      ndvb(idx)=w2+dt05*ndvb(idx)
    endif
    end subroutine average_velocity_kernel

    attributes(global) subroutine  update_position_kernel(nnod, ndx,ndy, ndub, ndvb, dt)

    integer,parameter::ink=4, rlk=8

    INTEGER(KIND=ink),value,             INTENT(IN)  :: nnod
    real(kind=rlk), value                            :: dt
    REAL(KIND=rlk),DIMENSION(:)                      :: ndub,ndvb, ndx, ndy
    ! Local
    integer::idx

    idx = threadIdx%x + (blockIdx%x-1)*blockDim%x
    if(idx<=nnod) then
      ndx(idx)=ndx(idx)+dt*ndub(idx)
      ndy(idx)=ndy(idx)+dt*ndvb(idx)
    endif
    end subroutine update_position_kernel

end module getacc_mod_kernel

MODULE getacc_mod

  IMPLICIT NONE

  PUBLIC :: getacc

CONTAINS
SUBROUTINE getacc(nshape,nel,nes,nnod,nns,cnfx,cnfy,ndub,ndvb,elu,elv,&
&                   rho,dt05,dt)

    USE kinds_mod,    ONLY: ink,rlk
    USE reals_mod,    ONLY: zerocut,dencut,accut
    USE logicals_mod, ONLY: zparallel
    USE comms_mod,    ONLY: HALFSTEP,exchange
    USE pointers_mod, ONLY: ielnd,cnmass,cnwt,indtype,ndu,ndv,ndx,ndy,  &
&                           ielsort1
    USE utilities_mod,ONLY: gather
    USE timing_mod,   ONLY: bookleaf_times
    USE TYPH_util_mod,ONLY: get_time

    ! Argument list
    INTEGER(KIND=ink),                   INTENT(IN)  :: nshape,nel,nnod,&
&                                                       nes,nns
    REAL(KIND=rlk),DIMENSION(nshape,nes),INTENT(IN)  :: cnfx,cnfy
    REAL(KIND=rlk),DIMENSION(nns),       INTENT(OUT) :: ndub,ndvb
    REAL(KIND=rlk),DIMENSION(nshape,nes),INTENT(OUT) :: elu,elv
    REAL(KIND=rlk),DIMENSION(nes),       INTENT(IN)  :: rho
    REAL(KIND=rlk),                      INTENT(IN)  :: dt05,dt
    ! Local
    INTEGER(KIND=ink)                                :: ii,jj,kk,iel,   &
&                                                       inod
    REAL(KIND=rlk)                                   :: w1,w2,t0,t1
    REAL(KIND=rlk),DIMENSION(nns)                    :: ndmass,ndarea

    ! Timer
    t0=get_time()

    ! MPI parallelism
    IF (zparallel) THEN
      call exchange(HALFSTEP)
    ENDIF

    ! Construct nodal mass and scatter force to nodes
    ndmass=0.0_rlk
    ndarea=0.0_rlk
    ndub=0.0_rlk
    ndvb=0.0_rlk
    DO jj=1,4
      DO kk=1,nes
        IF (zparallel) THEN
          iel=ielsort1(kk)
        ELSE
          iel=kk
        ENDIF
        inod=ielnd(jj,iel)
        IF (cnmass(jj,iel).GT.zerocut) THEN
          ndmass(inod)=ndmass(inod)+cnmass(jj,iel)
        ELSE
          ii=jj-1_ink
          IF (ii.EQ.0_ink) ii=4_ink
          IF (cnmass(ii,iel).GT.zerocut) THEN
            ndmass(inod)=ndmass(inod)+cnmass(ii,iel)
          ELSE
            ndmass(inod)=ndmass(inod)+rho(iel)*cnwt(jj,iel)
          ENDIF
        ENDIF
        ndarea(inod)=ndarea(inod)+cnwt(jj,iel)
        ndub(inod)=ndub(inod)+cnfx(jj,iel)
        ndvb(inod)=ndvb(inod)+cnfy(jj,iel)
      ENDDO
    ENDDO
    ! Calculate acceleration
    DO inod=1,nnod
      w1=dencut*ndarea(inod)
      IF (ndmass(inod).GT.w1) THEN
        ndub(inod)=ndub(inod)/ndmass(inod)
        ndvb(inod)=ndvb(inod)/ndmass(inod)
      ELSE
        ndub(inod)=0.0_rlk
        ndvb(inod)=0.0_rlk
        ndmass(inod)=MAX(zerocut,w1)
      ENDIF
    ENDDO
    
    !# Missing code here that can't be merged
    ! Boundary conditions
    w1=accut*accut
    DO inod=1,nnod
      SELECT CASE(indtype(inod))
        CASE DEFAULT
        CASE(-1_ink)
          ndub(inod)=0.0_rlk
        CASE(-2_ink)
          ndvb(inod)=0.0_rlk
        CASE(-3_ink)
          ndub(inod)=0.0_rlk
          ndvb(inod)=0.0_rlk
      END SELECT
      w2=ndub(inod)*ndub(inod)+ndvb(inod)*ndvb(inod)
      IF (w2.LT.w1) THEN
        ndub(inod)=0.0_rlk
        ndvb(inod)=0.0_rlk
      ENDIF
    ENDDO
    !# Missing code here that can't be merged
    ! Calculate average velocity
    DO inod=1,nnod
      w1=ndu(inod)
      w2=ndv(inod)
      ndu(inod)=w1+dt*ndub(inod)
      ndv(inod)=w2+dt*ndvb(inod)
      ndub(inod)=w1+dt05*ndub(inod)
      ndvb(inod)=w2+dt05*ndvb(inod)
    ENDDO
    CALL gather(nshape,nel,nnod,ielnd(1,1),ndub(1),elu(1,1))
    CALL gather(nshape,nel,nnod,ielnd(1,1),ndvb(1),elv(1,1))
    !# Missing code here that can't be merged
    ! Update position
    DO inod=1,nnod
      ndx(inod)=ndx(inod)+dt*ndub(inod)
      ndy(inod)=ndy(inod)+dt*ndvb(inod)
    ENDDO
    !# Missing code here that can't be merged

    ! Timing data
    t1=get_time()
    t1=t1-t0
    bookleaf_times%time_in_getacc=bookleaf_times%time_in_getacc+t1

  END SUBROUTINE getacc



  SUBROUTINE getacc_host(nshape,nel,nes,nnod,nns,cnfx,cnfy,ndub,ndvb,elu,elv,&
&                   rho,dt05,dt, ielnd,cnmass,cnwt,indtype,ndu,ndv,ndx,ndy,  &
&                   ielsort1)

    use getacc_mod_kernel
    USE kinds_mod,    ONLY: ink,rlk
    USE reals_mod,    ONLY: zerocut,dencut,accut
    USE logicals_mod, ONLY: zparallel
    USE comms_mod,    ONLY: HALFSTEP,exchange
!    USE pointers_mod, ONLY: ielnd,cnmass,cnwt,indtype,ndu,ndv,ndx,ndy,  &
!&                           ielsort1
    USE utilities_mod,ONLY: gather, gather_kernel
    USE timing_mod,   ONLY: bookleaf_times
    USE TYPH_util_mod,ONLY: get_time

    ! Argument list
    INTEGER(KIND=ink),                   INTENT(IN)  :: nshape,nel,nnod,&
&                                                       nes,nns
    integer(kind=ink), dimension(:,:), device        :: ielnd
    integer(kind=ink), dimension(:), device        :: indtype,ielsort1
    REAL(KIND=rlk),DIMENSION(:,:),INTENT(IN),device  :: cnfx,cnfy
    REAL(KIND=rlk),DIMENSION(:), INTENT(OUT), device :: ndub,ndvb
    REAL(KIND=rlk),DIMENSION(:,:),INTENT(OUT),device :: elu,elv
    REAL(KIND=rlk),DIMENSION(:),  INTENT(IN),device  :: rho
    REAL(KIND=rlk),                      INTENT(IN)  :: dt05,dt
    real(kind=rlk), dimension(:,:), device           :: cnmass,cnwt
    real(kind=rlk), dimension(:), device             :: ndu,ndv,ndx,ndy
    ! Local
    INTEGER(KIND=ink)                                :: ii,jj,kk,iel,   &
&                                                       inod
    REAL(KIND=rlk)                                   :: w1,w2,t0,t1
    REAL(KIND=rlk),DIMENSION(:),allocatable, device            :: ndmass,ndarea
    integer::thread_num, block_num

    allocate(ndmass(nns))
    allocate(ndarea(nns))

    thread_num = 128
    block_num = ceiling(real(nel)/thread_num)

    ! Timer
    t0=get_time()

    ! MPI parallelism
    IF (zparallel) THEN
      call exchange(HALFSTEP)
    ENDIF

    ! Construct nodal mass and scatter force to nodes
    ndmass=0.0_rlk
    ndarea=0.0_rlk
    ndub=0.0_rlk
    ndvb=0.0_rlk

    block_num = ceiling(real(nes)/thread_num)
    call construct_force<<<block_num, thread_num>>>(nes, zparallel, ielsort1, ielnd, &
&                                                   cnmass, ndmass, rho, cnwt, ndarea, &
&                                                   ndub, ndvb, cnfx, cnfy, 1, zerocut)
    call construct_force<<<block_num, thread_num>>>(nes, zparallel, ielsort1, ielnd, &
&                                                   cnmass, ndmass, rho, cnwt, ndarea, &
&                                                   ndub, ndvb, cnfx, cnfy, 2, zerocut)
    call construct_force<<<block_num, thread_num>>>(nes, zparallel, ielsort1, ielnd, &
&                                                   cnmass, ndmass, rho, cnwt, ndarea, &
&                                                   ndub, ndvb, cnfx, cnfy, 3, zerocut)
    call construct_force<<<block_num, thread_num>>>(nes, zparallel, ielsort1, ielnd, &
&                                                   cnmass, ndmass, rho, cnwt, ndarea, &
&                                                   ndub, ndvb, cnfx, cnfy, 4, zerocut)
    ! Calculate acceleration
    block_num = ceiling(real(nnod)/thread_num)
    call calculate_acceleration_kernle<<<block_num, thread_num>>>(nnod, dencut, ndarea, ndmass, ndub, ndvb,zerocut)
    
    !# Missing code here that can't be merged
    ! Boundary conditions

    call boundary_kernel<<<block_num, thread_num>>>(nnod, accut, indtype, ndub, ndvb)


    call average_velocity_kernel<<<block_num, thread_num>>>(nnod, ndu, ndv, ndub, ndvb, dt, dt05)
    !# Missing code here that can't be merged
    block_num = ceiling(real(nel)/thread_num)

    CALL gather_kernel<<<block_num,thread_num>>>(nshape,nel,nnod,ielnd,ndub,elu)
    CALL gather_kernel<<<block_num, thread_num>>>(nshape,nel,nnod,ielnd,ndvb,elv)
    !# Missing code here that can't be merged
    ! Update position
    !# Missing code here that can't be merged
    block_num = ceiling(real(nnod)/thread_num)
    call update_position_kernel<<<block_num, thread_num>>>(nnod, ndx,ndy, ndub, ndvb, dt)

    ! Timing data
    t1=get_time()
    t1=t1-t0
    bookleaf_times%time_in_getacc=bookleaf_times%time_in_getacc+t1

  END SUBROUTINE getacc_host

END MODULE getacc_mod
