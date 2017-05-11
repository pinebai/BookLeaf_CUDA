
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
module lagstep_kernel_mod
implicit none
contains
    
  attributes(global) subroutine half_step_kernel(ndxu, ndyv, ndx, ndy, ndu, ndv, nnod, dt05)
    implicit none
    integer,parameter::ink=4, rlk=8
    real(kind=rlk), dimension(:)::ndu, ndv, ndx, ndy, ndxu, ndyv
    real(kind=rlk), value :: dt05
    integer(kind=ink), value:: nnod
    integer::idx

    idx = threadIdx%x + (blockIdx%x-1)*blockDim%x
    
    if(idx<=nnod) then
      ndxu(idx)=ndx(idx)+dt05*ndu(idx)
      ndyv(idx)=ndy(idx)+dt05*ndv(idx)
    endif
  end subroutine half_step_kernel

  attributes(global) subroutine half_step_density_kernel(rho, elmass, elvol, nel)
    implicit none
    integer,parameter::ink=4, rlk=8
    REAL(KIND=rlk),DIMENSION(:) :: rho, elvol, elmass
    integer(kind=ink), value:: nel
    integer::idx
    idx = threadIdx%x + (blockIdx%x-1)*blockDim%x
    
    if(idx<=nel) then
      rho(idx)=elmass(idx)/elvol(idx)
    endif
end subroutine half_step_density_kernel

end module lagstep_kernel_mod

MODULE lagstep_mod

  IMPLICIT NONE

  PUBLIC :: lagstep

CONTAINS


  SUBROUTINE lagstep(dt, d_elu, d_elv, d_elx, d_ely, d_rho, d_rho05,d_qq, d_qx, d_qy, d_du, d_dv, d_dx, &
&       d_dy, d_scratch, d_ielel, d_ielnd, d_ielsd, d_indtype, d_csqrd, d_ndu, d_ndv, d_a1, d_a3, d_b1, &
&       d_b3, d_pre,d_pre05, d_ielreg, d_pmeritreg, d_spmass, d_elvol, d_kappareg, d_ndx, d_ndy, d_ndxu, d_ndyv,&
&       d_a2, d_b2, d_cnwt, d_elmass, d_ein05, d_ein, d_ielmat, d_eos_type, d_eos_param, d_cnmass, d_ielsort1)

    use cudafor
    use lagstep_kernel_mod
    USE kinds_mod,    ONLY: rlk,ink,lok
    USE integers_mod, ONLY: nel,nnod,nshape,nel1,nnod1
    USE geometry_mod, ONLY: getgeom, getgeom_host
    USE pointers_mod, ONLY: ielel,ielsd,indtype,qq,qx,qy, a1, a3, b1, b3, a2, b2
    USE getacc_mod,   ONLY: getacc, getacc_host
    USE getq_mod,     ONLY: getq, getq_host
    USE getpc_mod,    ONLY: getpc, getpc_host
    USE getforce_mod, ONLY: getforce, getforce_host
    USE getein_mod,   ONLY: getein, getein_host
    USE utilities_mod,ONLY: gather, gather_kernel
    USE timing_mod,   ONLY: timer=>bookleaf_times
    USE TYPH_util_mod,ONLY: get_time

    USE scratch_mod,  ONLY: elfx=>rscratch23,elfy=>rscratch24,          &
&                           dx=>rscratch25, dy=>rscratch26, rho05=>rscratch11

    ! Argument list
    REAL(KIND=rlk),INTENT(IN) :: dt
    ! Local
    INTEGER(KIND=ink)         :: iel,inod
    REAL(KIND=rlk)            :: dt05, t0, t1

    !device data

    INTEGER(KIND=ink),allocatable, device   :: d_ielnd(:,:),d_ielel(:,:), &
&                                              d_ielsd(:,:),d_indtype(:), d_ielreg(:), &
&                                              d_ielmat(:), d_eos_type(:), d_ielsort1(:)

    REAL(KIND=rlk),dimension(:),allocatable, device  :: d_qq, d_csqrd, d_a1,d_a2, d_a3, d_b1, d_b2, d_b3, d_pre, d_pre05

    REAL(KIND=rlk),dimension(:,:),allocatable, device   :: d_qx, d_qy

    REAL(KIND=rlk),DIMENSION(:,:),allocatable, device        :: d_elx,d_ely,d_elu,d_elv,d_cnwt, d_eos_param, d_cnmass
    REAL(KIND=rlk),DIMENSION(:,:),allocatable, device   :: d_dx,d_dy,d_du,d_dv,d_scratch, d_spmass
    REAL(KIND=rlk),DIMENSION(:), allocatable, device    :: d_rho, d_rho05, d_ndu, d_ndv, d_pmeritreg,&
&                                                  d_kappareg, d_elvol, d_ndx, d_ndy, d_ndxu, d_ndyv, &
&                                                  d_elmass, d_ein05, d_ein

    integer::thread_num, block_num
    ! Timer
    t0=get_time()

    ! ##############
    ! Predictor
    ! ##############
    dt05=0.5_rlk*dt
    thread_num = 128
    block_num = ceiling(real(nel)/thread_num)
    !print *, nel, block_num



    call gather_kernel<<<block_num, thread_num>>>(nshape,nel,nnod,d_ielnd,d_ndu,d_elu)
    call gather_kernel<<<block_num, thread_num>>>(nshape,nel,nnod,d_ielnd,d_ndv,d_elv)
	
    CALL getq_host(nshape,nel, d_elu, d_elv, d_elx, d_ely, d_rho, d_qq, d_qx, d_qy, d_du, d_dv, d_dx, d_dy, &
& d_scratch, d_ielel, d_ielnd, d_ielsd, d_indtype, d_csqrd, elfx, elfy,dx, dy)

	    ! Force
	    CALL getforce_host(nshape,nel,dt05, .FALSE._lok, d_elx, d_ely, d_elu, d_elv, &
	&                 d_du, d_dv, d_a1, d_a3, d_b1, d_b3, d_pre, d_qx, d_qy, d_pmeritreg, d_ielreg, d_csqrd, &
	&                 d_spmass, d_rho, d_elvol, d_kappareg)

	    ! Half step positions		
	    thread_num = 128
	    block_num = ceiling(real(nnod)/thread_num)
	    call half_step_kernel<<<block_num, thread_num>>>(d_ndxu, d_ndyv, d_ndx, d_ndy, d_ndu, d_ndv, nnod, dt05)

	    !# Missing code here that can't be merged
	    ! Update geometry and iso-parametric terms
    	call getgeom_host(nshape,nel,nnod,d_ndxu,d_ndyv,d_elx,d_ely,d_a1, d_a2, d_a3, d_b1, d_b2, d_b3, d_elvol,&
&                d_cnwt, d_ielnd, timer%time_in_getgeoml)

	    !# Missing code here that can't be merged
	    ! Half step density
		
	    block_num = ceiling(real(nel)/thread_num)
	    call half_step_density_kernel<<<block_num, thread_num>>>(d_rho05, d_elmass, d_elvol, nel)

	    ! Half step internal energy

    	CALL getein_host(nshape,nel,d_ein05,dt05,d_du, d_dv,d_elu,  &
&               d_elv, d_elmass, d_ein)


	    !# Missing code here that can't be merged
	    ! Half step pressure

    	CALL getpc_host(nel,d_ielmat,d_rho05,d_ein05,d_pre05, d_csqrd, d_eos_type, d_eos_param, &
&              timer%time_in_getpcl)
	    !# Missing code here that can't be merged
	    ! ###############
	    ! Corrector
	    ! ###############
	    ! Artificial viscosity
    	CALL getq_host(nshape,nel, d_elu, d_elv, d_elx, d_ely, d_rho05, d_qq, d_qx, d_qy, d_du, d_dv, d_dx, d_dy, &
&              d_scratch, d_ielel, d_ielnd, d_ielsd, d_indtype, d_csqrd, elfx, elfy, dx, dy)

	    ! Force

    	CALL getforce_host(nshape,nel,dt05,.FALSE._lok, d_elx, d_ely, d_elu, d_elv, &
&                 d_du, d_dv, d_a1, d_a3, d_b1, d_b3, d_pre05, d_qx, d_qy, d_pmeritreg, d_ielreg, d_csqrd, &
&                 d_spmass, d_rho05, d_elvol, d_kappareg)



	    ! Acceleration

    	CALL getacc_host(nshape,nel,nel1,nnod,nnod1,d_du,d_dv,d_ndxu, &
&               	d_ndyv,d_elu, d_elv,d_rho05,dt05,dt, d_ielnd,d_cnmass, &
&               	d_cnwt,d_indtype,d_ndu,d_ndv,d_ndx,d_ndy, d_ielsort1, &
&                   elfx(1,1), elfy(1,1), rho05(1))


	    ! Update geometry and iso-parametric terms

    	call getgeom_host(nshape,nel,nnod,d_ndx,d_ndy,d_elx,d_ely,d_a1, d_a2, d_a3, d_b1, d_b2, d_b3, d_elvol,&
&                d_cnwt, d_ielnd, timer%time_in_getgeoml)



	    !# Missing code here that can't be merged
	    ! Full step density

	    
		call half_step_density_kernel<<<block_num, thread_num>>>(d_rho, d_elmass, d_elvol, nel)
		
		
	    ! full step internal energy update

    	CALL getein_host(nshape,nel,d_ein,dt,d_du, d_dv, d_elu,  &
&               d_elv, d_elmass, d_ein)

	    !# Missing code here that can't be merged
	    ! Full step pressure

    CALL getpc_host(nel,d_ielmat,d_rho,d_ein,d_pre, d_csqrd, d_eos_type, d_eos_param, &
&              timer%time_in_getpcl)

    t1=get_time()
    t1=t1-t0
    timer%time_in_lag=timer%time_in_lag+t1


  END SUBROUTINE lagstep

END MODULE lagstep_mod
