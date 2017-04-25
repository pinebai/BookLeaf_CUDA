
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

module getq_kernel
contains
    attributes(global) subroutine init_kernel_zero(d_qq, d_qx, d_qy, nel)
        implicit none
        integer,parameter::ink=4, rlk=8
        REAL(KIND=rlk), dimension(:) :: d_qq
        REAL(KIND=rlk), dimension(:,:):: d_qx, d_qy
        integer::idx 
        INTEGER(KIND=ink), value::nel

        idx = threadIdx%x + (blockIdx%x-1)*blockDim%x
		        
        if(idx<=nel) then
            d_qq(idx)=0.0_rlk
            d_qx(1,idx)=0.0_rlk
            d_qx(2,idx)=0.0_rlk
            d_qx(3,idx)=0.0_rlk
            d_qx(4,idx)=0.0_rlk
            d_qy(1,idx)=0.0_rlk
            d_qy(2,idx)=0.0_rlk
            d_qy(3,idx)=0.0_rlk
            d_qy(4,idx)=0.0_rlk
        end if
    end subroutine init_kernel_zero


    attributes(global) subroutine init_kernel_array(d_du, d_dv, d_dx, d_dy, d_elu, d_elv, d_elx,d_ely, nel)
        implicit none
        integer,parameter::ink=4, rlk=8
        REAL(KIND=rlk),DIMENSION(:,:):: d_elx,d_ely,d_elu,d_elv
        REAL(KIND=rlk),DIMENSION(:,:):: d_dx,d_dy,d_du,d_dv  

        integer::idx 
        INTEGER(KIND=ink), value::nel

        idx = threadIdx%x + (blockIdx%x-1)*blockDim%x
        !nel = size(d_qq)
        
        if(idx <= nel) then
            d_du(1,idx)=d_elu(2,idx)-d_elu(1,idx)
            d_du(2,idx)=d_elu(3,idx)-d_elu(2,idx)
            d_du(3,idx)=d_elu(4,idx)-d_elu(3,idx)
            d_du(4,idx)=d_elu(1,idx)-d_elu(4,idx)
            d_dv(1,idx)=d_elv(2,idx)-d_elv(1,idx)
            d_dv(2,idx)=d_elv(3,idx)-d_elv(2,idx)
            d_dv(3,idx)=d_elv(4,idx)-d_elv(3,idx)
            d_dv(4,idx)=d_elv(1,idx)-d_elv(4,idx)
            d_dx(1,idx)=d_elx(2,idx)-d_elx(1,idx)
            d_dx(2,idx)=d_elx(3,idx)-d_elx(2,idx)
            d_dx(3,idx)=d_elx(4,idx)-d_elx(3,idx)
            d_dx(4,idx)=d_elx(1,idx)-d_elx(4,idx)
            d_dy(1,idx)=d_ely(2,idx)-d_ely(1,idx)
            d_dy(2,idx)=d_ely(3,idx)-d_ely(2,idx)
            d_dy(3,idx)=d_ely(4,idx)-d_ely(3,idx)
            d_dy(4,idx)=d_ely(1,idx)-d_ely(4,idx)
        endif
    end subroutine init_kernel_array

    attributes(global) subroutine compute_edge_kernel(d_ielel, d_iside, d_du, d_dv, d_dx, d_dy, &
&                                                     d_is1,d_is2, d_ielsd, d_nshape, d_scratch, d_zerocut, d_nel)
    implicit none
    integer, parameter::ink=4, rlk=8

		!parameters
		integer(kind=ink):: d_ielnd(:,:),d_ielel(:,:),d_ielsd(:,:)
		integer(kind=ink), value::d_iside, d_nshape,d_is1, d_is2
		integer(kind=ink), value,intent(in)::d_nel
		real(kind=rlk), dimension(:,:)::d_du, d_dv, d_dx, d_dy, d_scratch
        real(kind=rlk), value::d_zerocut
		!local variable
		integer(kind=ink):: d_in1, d_in2, d_ins 
		real(kind=rlk):: d_w1, d_w2, d_w3, d_w4, d_den, d_uhat, d_vhat, d_xhat, d_yhat 
		integer::idx 
		
        idx = threadIdx%x + (blockIdx%x-1)*blockDim%x

        !if(idx == 1) then
        
        !print *, 'i kernel', d_iside, d_nel, d_is1, d_is2, d_zerocut
        !endif
		if(idx<=d_nel) then
			d_in1=d_ielel(d_iside,idx)
			d_in2=d_ielel(d_iside+2,idx)
			! edge 1
			d_w1=d_du(d_is1,idx)
			d_w2=d_dv(d_is1,idx)
			d_w3=d_dx(d_is1,idx)
			d_w4=d_dy(d_is1,idx)
			d_den=sqrt(d_w1*d_w1+d_w2*d_w2)
			d_den=1.0_rlk/MAX(d_den,d_zerocut)
			d_uhat=d_w1*d_den
			d_vhat=d_w2*d_den
			d_den=SQRT(d_w3*d_w3+d_w4*d_w4)
			d_den=1.0_rlk/MAX(d_den,d_zerocut)
			d_xhat=d_w3*d_den
			d_yhat=d_w4*d_den
			d_den=d_w3*d_xhat+d_w4*d_yhat
			d_w1=(d_w1*d_uhat+d_w2*d_vhat)/SIGN(MAX(ABS(d_den),d_zerocut),d_den)
			d_w1=1.0_rlk/SIGN(MAX(ABS(d_w1),d_zerocut),d_w1)
			d_ins=d_ielsd(d_iside,idx)
			d_ins=MOD(d_ins,d_nshape)+1_ink
			d_den=d_dx(d_ins,d_in1)*d_xhat+d_dy(d_ins,d_in1)*d_yhat
			d_w2=(d_du(d_ins,d_in1)*d_uhat+d_dv(d_ins,d_in1)*d_vhat)/                        &
&       	SIGN(MAX(ABS(d_den),d_zerocut),d_den)
			d_scratch(1,idx)=d_w2*d_w1
			d_ins=d_ielsd(d_iside+2_ink,idx)
			d_ins=MOD(d_ins+2_ink,d_nshape)+1_ink
			d_den=d_dx(d_ins,d_in2)*d_xhat+d_dy(d_ins,d_in2)*d_yhat
			d_w3=(d_du(d_ins,d_in2)*d_uhat+d_dv(d_ins,d_in2)*d_vhat)/                        &
&       	SIGN(MAX(ABS(d_den),d_zerocut),d_den)
			d_scratch(2,idx)=d_w3*d_w1 
			! edge 2
			d_w1=d_du(d_is2,idx)
			d_w2=d_dv(d_is2,idx)
			d_w3=d_dx(d_is2,idx)
			d_w4=d_dy(d_is2,idx)
			d_den=SQRT(d_w1*d_w1+d_w2*d_w2)
			d_den=1.0_rlk/MAX(d_den,d_zerocut)
			d_uhat=d_w1*d_den
			d_vhat=d_w2*d_den
			d_den=SQRT(real(d_w3*d_w3+d_w4*d_w4))
			d_den=1.0_rlk/MAX(d_den,d_zerocut)
			d_xhat=d_w3*d_den
			d_yhat=d_w4*d_den
			d_den=d_w3*d_xhat+d_w4*d_yhat
			d_w1=(d_w1*d_uhat+d_w2*d_vhat)/SIGN(MAX(ABS(d_den),d_zerocut),d_den)
			d_w1=1.0_rlk/SIGN(MAX(ABS(d_w1),d_zerocut),d_w1)
			d_ins=d_ielsd(d_iside,idx)
			d_ins=MOD(d_ins+2_ink,d_nshape)+1_ink
			d_den=d_dx(d_ins,d_in1)*d_xhat+d_dy(d_ins,d_in1)*d_yhat
			d_w2=(d_du(d_ins,d_in1)*d_uhat+d_dv(d_ins,d_in1)*d_vhat)/                         &
&   		SIGN(MAX(ABS(d_den),d_zerocut),d_den)
			d_scratch(3,idx)=d_w2*d_w1
			d_ins=d_ielsd(d_iside+2_ink,idx)
			d_ins=MOD(d_ins,d_nshape)+1_ink
			d_den=d_dx(d_ins,d_in2)*d_xhat+d_dy(d_ins,d_in2)*d_yhat
			d_w3=(d_du(d_ins,d_in2)*d_uhat+d_dv(d_ins,d_in2)*d_vhat)/                         &
&   		SIGN(MAX(ABS(d_den),d_zerocut),d_den)
    		d_scratch(4,idx)=d_w3*d_w1
		endif
    end subroutine compute_edge_kernel


    attributes(global) subroutine compute_bc_kernel(d_ielel, d_ielnd, d_iside, d_indtype, d_scratch, &
    &                                               d_nshape, d_nel, d_ins)
    implicit none
    integer,parameter::ink=4, rlk=8
    !device parameter
		integer(kind=ink):: d_ielnd(:,:),d_ielel(:,:), d_indtype(:)
		integer(kind=ink), value::d_iside, d_nshape,d_ins
		integer(kind=ink), value,intent(in)::d_nel
		real(kind=rlk), dimension(:,:):: d_scratch
		!local variable
		integer(kind=ink):: d_in1, d_in2, d_ic1, d_ic2
		integer::idx 
    
        idx = threadIdx%x + (blockIdx%x-1)*blockDim%x
        if(idx<=d_nel) then
            d_in1=d_ielel(d_iside,idx)
            d_in2=d_ielel(d_ins,idx)
            IF (d_in1.EQ.idx) THEN
                d_ic1=d_ielnd(d_iside,idx)
                d_ic2=d_ielnd(MOD(d_iside,d_nshape)+1_ink,idx)
                IF (((d_indtype(d_ic1).LT.0_ink).AND.(d_indtype(d_ic2).LT.0_ink)).AND.&
&                  (d_in2.NE.idx)) THEN
                   d_scratch(1,idx)=1.0_rlk
                   d_scratch(3,idx)=1.0_rlk
                ELSE
                   d_scratch(1,idx)=0.0_rlk
                   d_scratch(3,idx)=0.0_rlk
                ENDIF
            ENDIF
            
            IF (d_in2.EQ.idx) THEN
                d_ic1=d_ielnd(d_ins,idx)
                d_ic2=d_ielnd(MOD(d_ins,d_nshape)+1_ink,idx)
                IF (((d_indtype(d_ic1).LT.0_ink).AND.(d_indtype(d_ic2).LT.0_ink)).AND.&
&                  (d_in1.NE.idx)) THEN
                   d_scratch(2,idx)=1.0_rlk
                   d_scratch(4,idx)=1.0_rlk
                ELSE
                   d_scratch(2,idx)=0.0_rlk
                   d_scratch(4,idx)=0.0_rlk
               ENDIF
            ENDIF
        endif
    end subroutine compute_bc_kernel


    attributes(global) subroutine apply_limits_kernel(d_csqrd, d_du, d_dv, d_qx, d_qy, d_scratch, &
    &                                                 d_cq1, d_cq2,d_is1, d_is2, d_rho, d_nel)

        implicit none
        integer, parameter::ink=4, rlk=8

		!parameters
		integer(kind=ink), value, intent(in)::d_is1, d_is2
		integer(kind=ink), value,intent(in)::d_nel
        REAL(KIND=rlk), DIMENSION(:),intent(in)   :: d_rho
		real(kind=rlk), dimension(:,:)::d_du, d_dv, d_scratch
        REAL(KIND=rlk), dimension(:,:):: d_qx, d_qy
		
        real(kind=rlk), value, intent(in)::d_cq1, d_cq2
        REAL(KIND=rlk),dimension(:) :: d_csqrd
		!local variable
		real(kind=rlk):: d_w1, d_w2, d_w3, d_w4
		integer::idx 
		
        idx = threadIdx%x + (blockIdx%x-1)*blockDim%x

        if(idx<=d_nel) then
	        d_w1=d_cq1*SQRT(d_csqrd(idx))
	        d_w2=d_scratch(1,idx)
	        d_w3=d_scratch(2,idx)
	        d_w2=MIN(0.5_rlk*(d_w2+d_w3),2.0_rlk*d_w2,2.0_rlk*d_w3,1.0_rlk)
	        d_w2=MAX(0.0_rlk,d_w2)
	        d_w3=d_du(d_is1,idx)
	        d_w4=d_dv(d_is1,idx)
	        d_w3=SQRT(d_w3*d_w3+d_w4*d_w4)
	        d_w3=(1.0_rlk-d_w2)*d_rho(idx)*(d_w1+d_cq2*d_w3)
	        d_qx(d_is1,idx)=d_w3
	        d_qy(d_is1,idx)=d_w3
			d_w2=d_scratch(3,idx)
	        d_w3=d_scratch(4,idx)
	        d_w2=MIN(0.5_rlk*(d_w2+d_w3),2.0_rlk*d_w2,2.0_rlk*d_w3,1.0_rlk)
	        d_w2=MAX(0.0_rlk,d_w2)
	        d_w3=d_du(d_is2,idx)
	        d_w4=d_dv(d_is2,idx)
	        d_w3=SQRT(d_w3*d_w3+d_w4*d_w4)
	        d_w3=(1.0_rlk-d_w2)*d_rho(idx)*(d_w1+d_cq2*d_w3)
	        d_qx(d_is2,idx)=d_w3
	        d_qy(d_is2,idx)=d_w3
        endif
    end subroutine apply_limits_kernel

    attributes(global) subroutine compute_final_q_kernel(d_elx, d_ely, d_elu, d_elv, d_qx, d_qy, d_qq, d_zerocut, &
&                                                        d_nel, d_iside, d_ins)
    implicit none
    integer, parameter::ink=4, rlk=8
    real(kind=rlk), dimension(:,:), intent(in):: d_elx, d_ely, d_elu, d_elv
    REAL(KIND=rlk), dimension(:,:):: d_qx, d_qy
    REAL(KIND=rlk), dimension(:) :: d_qq
	integer(kind=ink), value::d_iside, d_ins
	integer(kind=ink), value,intent(in)::d_nel
    real(kind=rlk), value::d_zerocut

    !local
	real(kind=rlk):: d_w1, d_w2, d_w3, d_w4, d_w5, d_w6,d_w7,d_w8, d_den, d_uhat, d_vhat, d_xhat, d_yhat 
    integer::idx
    
    idx = threadIdx%x + (blockIdx%x-1)*blockDim%x

    if(idx<=d_nel) then
        d_w1=d_elx(d_iside,idx)
        d_w2=d_elx(d_ins,idx)
        d_w3=0.5_rlk*(d_w1+d_w2)
        d_w1=d_w2-d_w1
        d_w2=0.25_rlk*(d_elx(1,idx)+d_elx(2,idx)+d_elx(3,idx)+d_elx(4,idx))
        d_w4=d_ely(d_iside,idx)
        d_w5=d_ely(d_ins,idx)
        d_w6=0.5_rlk*(d_w4+d_w5)
        d_w4=d_w5-d_w4
        d_w5=0.25_rlk*(d_ely(1,idx)+d_ely(2,idx)+d_ely(3,idx)+d_ely(4,idx))
        d_w7=SQRT((d_w2-d_w3)*(d_w2-d_w3)+(d_w5-d_w6)*(d_w5-d_w6))
        d_w8=SQRT(d_w1*d_w1+d_w4*d_w4)
        d_den=1.0_rlk/d_w7
        d_xhat=(d_w5-d_w6)*d_den
        d_yhat=(d_w3-d_w2)*d_den
        d_den=1.0_rlk/d_w8
        d_w1=d_w1*d_den
        d_w2=d_w4*d_den
        d_w3=d_xhat*d_w1+d_yhat*d_w2
        d_den=-SIGN(1.0_rlk,d_w3)*d_w7
        d_xhat=d_xhat*d_den
        d_yhat=d_yhat*d_den
        d_uhat=d_elu(d_ins,idx)-d_elu(d_iside,idx)
        d_vhat=d_elv(d_ins,idx)-d_elv(d_iside,idx)
        d_w5=SQRT((d_uhat*d_uhat)+(d_vhat*d_vhat))
        d_w6=d_uhat*d_xhat+d_vhat*d_yhat
        d_den=d_w6/MAX(d_w5,d_zerocut)
        d_qx(d_iside,idx)=d_qx(d_iside,idx)*d_uhat*d_den
        d_qy(d_iside,idx)=d_qy(d_iside,idx)*d_vhat*d_den
        IF ((d_w5.LE.d_zerocut).OR.(d_w6.LE.d_zerocut).OR.(d_w7.LE.d_zerocut).OR.   &
&           (d_w8.LE.d_zerocut)) THEN
          d_qx(d_iside,idx)=0.0_rlk
          d_qy(d_iside,idx)=0.0_rlk
        ENDIF
        d_qq(idx)=d_qq(idx)+0.25_rlk*SQRT(d_qx(d_iside,idx)*d_qx(d_iside,idx)+      &
&               d_qy(d_iside,idx)*d_qy(d_iside,idx))
    endif
    end subroutine compute_final_q_kernel
end module

MODULE getq_mod

  IMPLICIT NONE

  PUBLIC :: getq

CONTAINS
  SUBROUTINE getq_host(nshape,nel, d_elu, d_elv, d_elx, d_ely, d_rho, d_qq, d_qx, d_qy, d_du, d_dv, d_dx, d_dy, &
& d_scratch, d_ielel, d_ielnd, d_ielsd, d_indtype, d_csqrd)!, rho, elv, elu, ely, elx, du, dv, dx, dy, scratch)
    use cudafor
    use cublas
    use getq_kernel
    USE kinds_mod,    ONLY: ink,rlk
    USE reals_mod,    ONLY: zerocut,cq1,cq2
    USE logicals_mod, ONLY: zparallel
    USE comms_mod,    ONLY: exchange,VISCOSITY
    USE pointers_mod, ONLY: ielnd,ielel,ielsd,indtype,qq,qx,qy,csqrd
    USE timing_mod,   ONLY: bookleaf_times
    USE typh_util_mod,ONLY: get_time

    ! Argument list
    INTEGER(KIND=ink),                   INTENT(IN)    :: nshape,nel
    !REAL(KIND=rlk),DIMENSION(nshape,nel),INTENT(IN)    :: elx,ely,elu,  &
!&                                                         elv
    !REAL(KIND=rlk),DIMENSION(nel),       INTENT(IN)    :: rho
    !REAL(KIND=rlk),DIMENSION(:,:)                      :: dx,dy,du,dv,  &
!&                                                         scratch
    ! Local
    INTEGER(KIND=ink)                                  :: iel,iside,in1,&
&                                                         in2,is1,is2,  &
&                                                         ins,ic1,ic2
    REAL(KIND=rlk)                                     :: xhat,yhat,t0, &
&                                                         uhat,vhat,den,&
&                                                         w1,w2,w3,w4,  &
&                                                         w5,w6,w7,w8,t1
    
    ! Device Argument list 
    INTEGER(KIND=ink), allocatable, device             :: d_ielnd(:,:),d_ielel(:,:), &
&                                                         d_ielsd(:,:),d_indtype(:)

    REAL(KIND=rlk),dimension(:),allocatable, device     :: d_qq, d_csqrd

    REAL(KIND=rlk),dimension(:,:),allocatable, device   :: d_qx, d_qy

    REAL(KIND=rlk),DIMENSION(:,:),allocatable, device        :: d_elx,d_ely,d_elu,d_elv
    REAL(KIND=rlk),DIMENSION(:,:),allocatable, device   :: d_dx,d_dy,d_du,d_dv,d_scratch
    REAL(KIND=rlk),DIMENSION(:), allocatable, device                 :: d_rho
    integer::block_num, thread_num, istat
    ! Local
    ! Timer
    t0=get_time()

    !# Missing code here that can't be merged

    thread_num = 128
    block_num = ceiling(real(nel)/128)
    !print *, 'block num', block_num
    call init_kernel_zero<<<block_num, thread_num>>>(d_qq, d_qx, d_qy, nel)
    call init_kernel_array<<<block_num, thread_num>>>(d_du, d_dv, d_dx, d_dy, d_elu, d_elv, d_elx,d_ely, nel)
	 
    ! gradient construction

    ! MPI parallelism
    IF (zparallel) THEN
      CALL exchange(VISCOSITY)
    ENDIF

    ! Christiensen monotonic limit
    DO iside=1,nshape/2_ink
      is1=MOD(iside+2_ink,nshape)+1_ink
      is2=iside+1_ink
      
      !print *, 'o kernel', iside, nel, is1, is2, zerocut
      call compute_edge_kernel<<<block_num,thread_num>>>(d_ielel, iside, d_du, d_dv, d_dx, d_dy, &
&                                                     is1,is2, d_ielsd, nshape, d_scratch, zerocut, nel)

      ins=iside+2_ink
      call compute_bc_kernel<<<block_num, thread_num>>>(d_ielel, d_ielnd, iside, d_indtype, d_scratch, &
    &                                                   nshape, nel, ins)
      call apply_limits_kernel<<<block_num, thread_num>>>(d_csqrd, d_du, d_dv, d_qx, d_qy, d_scratch, &
    &                                                 cq1, cq2,is1, is2, d_rho, nel)
	ENDDO
	
      ! BC
    ! Final Q calculation
    DO iside=1,nshape
      ins=MOD(iside,nshape)+1_ink
      call compute_final_q_kernel<<<block_num, thread_num>>>(d_elx, d_ely, d_elu, d_elv, d_qx, d_qy, d_qq,&
&                                                            zerocut, nel, iside, ins)
    ENDDO
    
    istat =  cudaThreadSynchronize()

    ! Timing data
    t1=get_time()
    t1=t1-t0
    bookleaf_times%time_in_getq=bookleaf_times%time_in_getq+t1

  END SUBROUTINE getq_host

  SUBROUTINE getq(nshape,nel,elx,ely,elu,elv,rho,pre,dx,dy,du,dv,       &
&                 scratch)

    USE kinds_mod,    ONLY: ink,rlk
    USE reals_mod,    ONLY: zerocut,cq1,cq2
    USE logicals_mod, ONLY: zparallel
    USE comms_mod,    ONLY: exchange,VISCOSITY
    USE pointers_mod, ONLY: ielnd,ielel,ielsd,indtype,qq,qx,qy,csqrd
    USE timing_mod,   ONLY: bookleaf_times
    USE typh_util_mod,ONLY: get_time

    ! Argument list
    INTEGER(KIND=ink),                   INTENT(IN)    :: nshape,nel
    REAL(KIND=rlk),DIMENSION(nshape,nel),INTENT(IN)    :: elx,ely,elu,  &
&                                                         elv
    REAL(KIND=rlk),DIMENSION(nel),       INTENT(IN)    :: rho,pre
    REAL(KIND=rlk),DIMENSION(:,:),       INTENT(INOUT) :: dx,dy,du,dv,  &
&                                                         scratch
    ! Local
    INTEGER(KIND=ink)                                  :: iel,iside,in1,&
&                                                         in2,is1,is2,  &
&                                                         ins,ic1,ic2
    REAL(KIND=rlk)                                     :: xhat,yhat,t0, &
&                                                         uhat,vhat,den,&
&                                                         w1,w2,w3,w4,  &
&                                                         w5,w6,w7,w8,t1
    ! Timer
    t0=get_time()

    !# Missing code here that can't be merged

    ! initialisation
    DO iel=1,nel
      qq(iel)=0.0_rlk
      qx(1,iel)=0.0_rlk
      qx(2,iel)=0.0_rlk
      qx(3,iel)=0.0_rlk
      qx(4,iel)=0.0_rlk
      qy(1,iel)=0.0_rlk
      qy(2,iel)=0.0_rlk
      qy(3,iel)=0.0_rlk
      qy(4,iel)=0.0_rlk
    ENDDO

    ! gradient construction
    DO iel=1,nel
      du(1,iel)=elu(2,iel)-elu(1,iel)
      du(2,iel)=elu(3,iel)-elu(2,iel)
      du(3,iel)=elu(4,iel)-elu(3,iel)
      du(4,iel)=elu(1,iel)-elu(4,iel)
      dv(1,iel)=elv(2,iel)-elv(1,iel)
      dv(2,iel)=elv(3,iel)-elv(2,iel)
      dv(3,iel)=elv(4,iel)-elv(3,iel)
      dv(4,iel)=elv(1,iel)-elv(4,iel)
      dx(1,iel)=elx(2,iel)-elx(1,iel)
      dx(2,iel)=elx(3,iel)-elx(2,iel)
      dx(3,iel)=elx(4,iel)-elx(3,iel)
      dx(4,iel)=elx(1,iel)-elx(4,iel)
      dy(1,iel)=ely(2,iel)-ely(1,iel)
      dy(2,iel)=ely(3,iel)-ely(2,iel)
      dy(3,iel)=ely(4,iel)-ely(3,iel)
      dy(4,iel)=ely(1,iel)-ely(4,iel)
    ENDDO

    ! MPI parallelism
    IF (zparallel) THEN
      CALL exchange(VISCOSITY)
    ENDIF

    ! Christiensen monotonic limit
    DO iside=1,nshape/2_ink
      is1=MOD(iside+2_ink,nshape)+1_ink
      is2=iside+1_ink
      DO iel=1,nel
        ! connectivity
        in1=ielel(iside,iel)
        in2=ielel(iside+2,iel)
        ! edge 1
        w1=du(is1,iel)
        w2=dv(is1,iel)
        w3=dx(is1,iel)
        w4=dy(is1,iel)
        den=SQRT(w1*w1+w2*w2)
        den=1.0_rlk/MAX(den,zerocut)
        uhat=w1*den
        vhat=w2*den
        den=SQRT(w3*w3+w4*w4)
        den=1.0_rlk/MAX(den,zerocut)
        xhat=w3*den
        yhat=w4*den
        den=w3*xhat+w4*yhat
        w1=(w1*uhat+w2*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        w1=1.0_rlk/SIGN(MAX(ABS(w1),zerocut),w1)
        ins=ielsd(iside,iel)
        ins=MOD(ins,nshape)+1_ink
        den=dx(ins,in1)*xhat+dy(ins,in1)*yhat
        w2=(du(ins,in1)*uhat+dv(ins,in1)*vhat)/                        &
&        SIGN(MAX(ABS(den),zerocut),den)
        scratch(1,iel)=w2*w1
        ins=ielsd(iside+2_ink,iel)
        ins=MOD(ins+2_ink,nshape)+1_ink
        den=dx(ins,in2)*xhat+dy(ins,in2)*yhat
        w3=(du(ins,in2)*uhat+dv(ins,in2)*vhat)/                        &
&        SIGN(MAX(ABS(den),zerocut),den)
        scratch(2,iel)=w3*w1
        ! edge 2
        w1=du(is2,iel)
        w2=dv(is2,iel)
        w3=dx(is2,iel)
        w4=dy(is2,iel)
        den=SQRT(w1*w1+w2*w2)
        den=1.0_rlk/MAX(den,zerocut)
        uhat=w1*den
        vhat=w2*den
        den=SQRT(w3*w3+w4*w4)
        den=1.0_rlk/MAX(den,zerocut)
        xhat=w3*den
        yhat=w4*den
        den=w3*xhat+w4*yhat
        w1=(w1*uhat+w2*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        w1=1.0_rlk/SIGN(MAX(ABS(w1),zerocut),w1)
        ins=ielsd(iside,iel)
        ins=MOD(ins+2_ink,nshape)+1_ink
        den=dx(ins,in1)*xhat+dy(ins,in1)*yhat
        w2=(du(ins,in1)*uhat+dv(ins,in1)*vhat)/                         &
&        SIGN(MAX(ABS(den),zerocut),den)
        scratch(3,iel)=w2*w1
        ins=ielsd(iside+2_ink,iel)
        ins=MOD(ins,nshape)+1_ink
        den=dx(ins,in2)*xhat+dy(ins,in2)*yhat
        w3=(du(ins,in2)*uhat+dv(ins,in2)*vhat)/                         &
&        SIGN(MAX(ABS(den),zerocut),den)
        scratch(4,iel)=w3*w1
      ENDDO
      ! BC
      ins=iside+2_ink
      DO iel=1,nel
        in1=ielel(iside,iel)
        in2=ielel(ins,iel)
        IF (in1.EQ.iel) THEN
          ic1=ielnd(iside,iel)
          ic2=ielnd(MOD(iside,nshape)+1_ink,iel)
          IF (((indtype(ic1).LT.0_ink).AND.(indtype(ic2).LT.0_ink)).AND.&
&          (in2.NE.iel)) THEN
            scratch(1,iel)=1.0_rlk
            scratch(3,iel)=1.0_rlk
          ELSE
            scratch(1,iel)=0.0_rlk
            scratch(3,iel)=0.0_rlk
          ENDIF
        ENDIF
        IF (in2.EQ.iel) THEN
          ic1=ielnd(ins,iel)
          ic2=ielnd(MOD(ins,nshape)+1_ink,iel)
          IF (((indtype(ic1).LT.0_ink).AND.(indtype(ic2).LT.0_ink)).AND.&
&          (in1.NE.iel)) THEN
            scratch(2,iel)=1.0_rlk
            scratch(4,iel)=1.0_rlk
          ELSE
            scratch(2,iel)=0.0_rlk
            scratch(4,iel)=0.0_rlk
          ENDIF
        ENDIF
      ENDDO
      ! Apply limiter
      DO iel=1,nel
        w1=cq1*SQRT(csqrd(iel))
        w2=scratch(1,iel)
        w3=scratch(2,iel)
        w2=MIN(0.5_rlk*(w2+w3),2.0_rlk*w2,2.0_rlk*w3,1.0_rlk)
        w2=MAX(0.0_rlk,w2)
        w3=du(is1,iel)
        w4=dv(is1,iel)
        w3=SQRT(w3*w3+w4*w4)
        w3=(1.0_rlk-w2)*rho(iel)*(w1+cq2*w3)
        qx(is1,iel)=w3
        qy(is1,iel)=w3
        w2=scratch(3,iel)
        w3=scratch(4,iel)
        w2=MIN(0.5_rlk*(w2+w3),2.0_rlk*w2,2.0_rlk*w3,1.0_rlk)
        w2=MAX(0.0_rlk,w2)
        w3=du(is2,iel)
        w4=dv(is2,iel)
        w3=SQRT(w3*w3+w4*w4)
        w3=(1.0_rlk-w2)*rho(iel)*(w1+cq2*w3)
        qx(is2,iel)=w3
        qy(is2,iel)=w3
      ENDDO
    ENDDO

    ! Final Q calculation
    DO iside=1,nshape
      ins=MOD(iside,nshape)+1_ink
      DO iel=1,nel
        w1=elx(iside,iel)
        w2=elx(ins,iel)
        w3=0.5_rlk*(w1+w2)
        w1=w2-w1
        w2=0.25_rlk*(elx(1,iel)+elx(2,iel)+elx(3,iel)+elx(4,iel))
        w4=ely(iside,iel)
        w5=ely(ins,iel)
        w6=0.5_rlk*(w4+w5)
        w4=w5-w4
        w5=0.25_rlk*(ely(1,iel)+ely(2,iel)+ely(3,iel)+ely(4,iel))
        w7=SQRT((w2-w3)*(w2-w3)+(w5-w6)*(w5-w6))
        w8=SQRT(w1*w1+w4*w4)
        den=1.0_rlk/w7
        xhat=(w5-w6)*den
        yhat=(w3-w2)*den
        den=1.0_rlk/w8
        w1=w1*den
        w2=w4*den
        w3=xhat*w1+yhat*w2
        den=-SIGN(1.0_rlk,w3)*w7
        xhat=xhat*den
        yhat=yhat*den
        uhat=elu(ins,iel)-elu(iside,iel)
        vhat=elv(ins,iel)-elv(iside,iel)
        w5=SQRT((uhat*uhat)+(vhat*vhat))
        w6=uhat*xhat+vhat*yhat
        den=w6/MAX(w5,zerocut)
        qx(iside,iel)=qx(iside,iel)*uhat*den
        qy(iside,iel)=qy(iside,iel)*vhat*den
        IF ((w5.LE.zerocut).OR.(w6.LE.zerocut).OR.(w7.LE.zerocut).OR.   &
&           (w8.LE.zerocut)) THEN
          qx(iside,iel)=0.0_rlk
          qy(iside,iel)=0.0_rlk
        ENDIF
        qq(iel)=qq(iel)+0.25_rlk*SQRT(qx(iside,iel)*qx(iside,iel)+      &
&               qy(iside,iel)*qy(iside,iel))
      ENDDO
    ENDDO

    ! Timing data
    t1=get_time()
    t1=t1-t0
    bookleaf_times%time_in_getq=bookleaf_times%time_in_getq+t1

  END SUBROUTINE getq
END MODULE getq_mod
