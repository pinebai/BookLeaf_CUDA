
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
module getdt_mod_kernel

implicit none
contains
	
	subroutine test_value_match(input1, input2)
        integer,parameter::ink=4, rlk=8, lok=4
        real(kind=rlk) :: input1, input2
		integer(kind=ink):: i
				
	        if(input1 == input2) then
		        print *, "test passed"
			else
				call halt("ERROR: value not match", 1, .true.)
	        endif
	end subroutine test_value_match
		
	subroutine test_1d_array(input1, input2, n)
        integer,parameter::ink=4, rlk=8, lok=4
        real(kind=rlk) :: input1(:), input2(:)
        integer(kind=ink), value ::n
		integer(kind=ink):: i
				
	    do i= 1, n
	        !if(abs(input1(i)-input2(i)) >0.00000000000005) then
	        if(input1(i) /= input2(i)) then
                print *, 'location ', i, input1(1), input2(i)
				call halt("ERROR: array value not match", i,.true.)
	        endif
	    enddo
        print *, "test passed"
	end subroutine test_1d_array 
	
	subroutine test_2d_array(input1, input2, x, y)
        integer,parameter::ink=4, rlk=8, lok=4
        real(kind=rlk) :: input1(:,:), input2(:,:)
        integer(kind=ink), value ::x, y
		integer(kind=ink):: i,j
				
	    do i= 1, x
			do j=1, y
	        if(input1(i,j) .ne. input2(i,j)) then
	            print *, "2d array test failed", i, j
	        endif
			enddo
	    enddo
	end subroutine test_2d_array 
	
    
	attributes(global) subroutine get_min_loc(input,rscratch11,  min_w, loc, n, rdt, idt, sf_value, ridx)
        integer,parameter::ink=4, rlk=8, lok=4
        real(kind=rlk) :: input(:), rscratch11(:), rdt(:)
        real(kind=rlk) :: min_w
        integer(kind=ink) :: loc, idt(:)
        integer :: idx, tid,index, i, istat
        integer(kind=ink), value ::n
        real(kind=rlk),value :: sf_value
        integer, value :: ridx

        
        idx = blockDim%x * (blockIdx%x - 1) + threadIdx%x
        min_w = input(1)
        if(idx<=n) then
            if(rscratch11(idx) == min_w) then
                istat = atomicmin(loc,idx)
            endif
        endif
        if(idx==1) then
            rdt(ridx)=sf_value*SQRT(min_w)
            idt(ridx)=loc
        endif
    end subroutine get_min_loc

    attributes(global) subroutine get_max_loc(input,w1, max_w, loc, n, rdt, idt, div_sf)
        integer,parameter::ink=4, rlk=8, lok=4
        real(kind=rlk) :: input(:), rdt(:), w1(:)
        real(kind=rlk) :: max_w
        integer(kind=ink) :: loc, idt(:)
        integer :: idx, tid,index, i, istat
        integer(kind=ink), value ::n
        real(kind=rlk),value :: div_sf

        idx = blockDim%x * (blockIdx%x - 1) + threadIdx%x
        max_w = input(1)
        if(idx<=n) then
            if(w1(idx) == max_w) then
                istat = atomicmin(loc,idx)
            endif
        endif
        if(idx==1) then
            rdt(2)=div_sf/max_w
            idt(2)=loc
        endif
    end subroutine get_max_loc

    subroutine reduction(input, output, n, flag, block_num, thread_num)
    implicit none
        integer,parameter::ink=4, rlk=8, lok=4
        real(kind=rlk), device :: input(:), output(:)
        integer :: grid, nz
        integer, value ::n
        integer, value ::flag, block_num, thread_num

        call reduction_kernel<<<block_num, thread_num>>>(input, output,n, flag)
        nz = block_num
        grid = ceiling(real(block_num)/thread_num)
        do while(grid >1)
            call reduction_kernel<<<grid, thread_num>>>(output, output, n, flag)
            nz = grid
            grid = ceiling(real(grid)/thread_num)
        enddo
        call reduction_kernel<<<grid, thread_num>>>(output, output, n, flag)
    end subroutine reduction

    attributes(global) subroutine reduction_kernel(input, output, n, flag)
    implicit none
        integer,parameter::ink=4, rlk=8, lok=4
        real(kind=rlk) :: input(:), output(:)
        integer :: idx, tid,index, i
        integer, value ::n
        real(kind=rlk), shared:: s_data(128)
        integer, value ::flag

        tid = threadIdx%x
        idx = blockDim%x * (blockIdx%x - 1) + threadIdx%x
        i=blockDim%x/2
        if(idx<=n) then
            s_data(tid) = input(idx)
            !if(flag == 1) output(idx) = TINY(1.0_rlk)
            !if(flag == 0) output(idx) = HUGE(1.0_rlk) 
            call syncthreads()
        endif

        do while(i>0)
            if(tid<=i .and. idx+i<=n) then
                if(flag == 0) s_data(tid) = min(s_data(tid), s_data(tid+i))
                if(flag == 1) s_data(tid) = max(s_data(tid), s_data(tid+i))
            endif
            i = i/2
        enddo
            call syncthreads()

        if(tid==1) then
            output(blockIdx%x) = s_data(1)
        endif
    end subroutine reduction_kernel
	
    attributes(global) subroutine cfl_kernel(nel,nshape, ielreg, zdtnotreg, rscratch11, rscratch12, rho, zcut, ccut, &
&                                    csqrd, qq, zmidlength,elx, ely, dt_max, cfl_sf, minr, loc) 

    USE geometry_mod,    ONLY: dlm_kernel, dln_kernel
    integer,parameter::ink=4, rlk=8, lok=4
    INTEGER(KIND=ink),PARAMETER                :: NDT=5_ink
    real(kind=rlk), value           ::dt_max, zcut, ccut, cfl_sf
    integer(kind=ink), value        ::nel, nshape
    real(kind=rlk)                  :: minr

    INTEGER(KIND=ink)              :: ielreg(:)
    REAL(KIND=rlk),dimension(:)     :: qq, csqrd, rho, rscratch11, rscratch12
    REAL(KIND=rlk),DIMENSION(:,:)   :: elx,ely
    REAL(KIND=rlk)   :: elx1,elx2, elx3, elx4,ely1, ely2, ely3, ely4, x1,x2, y1, y2, res(nshape)

    logical(kind=lok), dimension(:) :: zdtnotreg, zmidlength
    ! Argument list
    ! Local
    INTEGER(KIND=ink)                          :: ireg, iel, loc
    REAL(KIND=rlk)                             :: w1,w2, w3, tmp, maxw

    iel = threadIdx%x + (blockIdx%x-1)*blockDim%x

    if(iel<=nel) then
        ireg=ielreg(iel)
        IF (zdtnotreg(ireg)) THEN
            rscratch11(iel)=dt_max
            rscratch12(iel)=TINY(1.0_rlk)
        ELSE
            w1=MAX(rho(iel),zcut)
            w2=MAX(ccut,csqrd(iel))+2.0_rlk*qq(iel)/w1
            elx1 = elx(1, iel)
            elx2 = elx(2, iel)
            elx3 = elx(3, iel)
            elx4 = elx(4, iel)

            ely1 = ely(1, iel)
            ely2 = ely(2, iel)
            ely3 = ely(3, iel)
            ely4 = ely(4, iel)
            
            IF (zmidlength(ireg)) THEN
              !w1=minval(dlm_kernel(nshape,elx1, elx2, elx3, elx4, ely1, ely2, ely3, ely4))
                x1=elx1+elx2
                x2=elx3+elx4
                y1=ely1+ely2
                y2=ely3+ely4
                x1=0.5_rlk*(x1-x2)
                y1=0.5_rlk*(y1-y2)
                w1=x1*x1+y1*y1

                x1=elx3+elx2
                x2=elx1+elx4
                y1=ely3+ely2
                y2=ely1+ely4
                x1=0.5_rlk*(x1-x2)
                y1=0.5_rlk*(y1-y2)
                w3=x1*x1+y1*y1
                if(w1 > w3) w1 = w3
            ELSE
                w1 = (ely3-ely4)*(ely3-ely4)+(elx3-elx4)*(elx3-elx4)
                IF (w1.LT.zcut) THEN
                     maxw = (0.5_rlk*(elx1+elx2)-elx3)*(0.5_rlk*(elx1+elx2)-elx3) + &
      &              (0.5_rlk*(ely1+ely2)-ely3)*(0.5_rlk*(ely1+ely2)-ely3) 
                ELSE
                    tmp=0.5_rlk*((ely3-ely4)*(elx1+elx2))+0.5_rlk*((ely1+ely2)*(elx4-elx3))+(ely4*elx3-ely3*elx4)
                    tmp = tmp*tmp
                    maxw = tmp/w1
                ENDIF

                w1 = (ely4-ely1)*(ely4-ely1)+(elx4-elx1)*(elx4-elx1)
                IF (w1.LT.zcut) THEN
                    w3 = (0.5_rlk*(elx2+elx3)-elx4)*(0.5_rlk*(elx2+elx3)-elx4) + &
      &              (0.5_rlk*(ely2+ely3)-ely4)*(0.5_rlk*(ely2+ely3)-ely4)
                ELSE
                  tmp=0.5_rlk*((ely4-ely1)*(elx2+elx3))+0.5_rlk*((ely2+ely3)*(elx1-elx4))+(ely1*elx4-ely4*elx1)
                  tmp = tmp*tmp
                  w3 = tmp/w1
                ENDIF
                maxw = min(w3, maxw)

                w1 = (ely1-ely2)*(ely1-ely2)+(elx1-elx2)*(elx1-elx2)
                IF (w1.LT.zcut) THEN
                    w3= (0.5_rlk*(elx3+elx4)-elx1)*(0.5_rlk*(elx3+elx4)-elx1) + &
      &              (0.5_rlk*(ely3+ely4)-ely1)*(0.5_rlk*(ely3+ely4)-ely1)
                ELSE
                  tmp=0.5_rlk*((ely1-ely2)*(elx3+elx4))+0.5_rlk*((ely3+ely4)*(elx2-elx1))+(ely2*elx1-ely1*elx2)
                  tmp = tmp*tmp
                  w3 = tmp/w1
                ENDIF
                maxw = min(w3, maxw)

                w1 = (ely2-ely3)*(ely2-ely3)+(elx2-elx3)*(elx2-elx3)
                IF (w1.LT.zcut) THEN
                    w3 =  (0.5_rlk*(elx4+elx1)-elx2)*(0.5_rlk*(elx4+elx1)-elx2) + &
      &              (0.5_rlk*(ely4+ely1)-ely2)*(0.5_rlk*(ely4+ely1)-ely2)
                ELSE
                  tmp=0.5_rlk*((ely2-ely3)*(elx4+elx1))+0.5_rlk*((ely4+ely1)*(elx3-elx2))+(ely3*elx2-ely2*elx3)
                  tmp = tmp*tmp
                  w3 = tmp/w1
                ENDIF
                maxw = min(w3, maxw)
                w1=maxw
            ENDIF
            rscratch11(iel)=w1/w2
            rscratch12(iel)=w1
        ENDIF
    endif
    end subroutine cfl_kernel

    attributes(global) subroutine divergence_kernel(nel, elvol,elu, elv, a1, a3, b1, b3, output)

    integer,parameter::ink=4, rlk=8, lok=4
    integer(kind=ink), value        ::nel
    INTEGER(KIND=ink),PARAMETER                :: NDT=5_ink

    REAL(KIND=rlk),dimension(:)    :: a1, a3, b1,b3, output

    REAL(KIND=rlk),DIMENSION(:,:)  :: elu,elv
    REAL(KIND=rlk),DIMENSION(:)   ::  elvol
    ! Argument list
    ! Local
    INTEGER(KIND=ink)                          :: iel, ii, loc
    REAL(KIND=rlk)                             :: w1,w2, max_val
    

    iel = threadIdx%x + (blockIdx%x-1)*blockDim%x
    if(iel<=nel) then 
      output(iel)=TINY(1.0_rlk)
      w1=elu(1,iel)*(-b3(iel)+b1(iel))+elv(1,iel)*( a3(iel)-a1(iel))+   &
&        elu(2,iel)*( b3(iel)+b1(iel))+elv(2,iel)*(-a3(iel)-a1(iel))+   &
&        elu(3,iel)*( b3(iel)-b1(iel))+elv(3,iel)*(-a3(iel)+a1(iel))+   &
&        elu(4,iel)*(-b3(iel)-b1(iel))+elv(4,iel)*( a3(iel)+a1(iel))
      w1=ABS(w1)/elvol(iel)
      if(w1 .gt. output(iel)) then
        output(iel) = w1
      endif
    endif
    end subroutine divergence_kernel

    attributes(global) subroutine ale_kernel(nel, elu, elv, rscratch12, zerocut, ale_sf, output)
        integer,PARAMETER                           ::ink=4, rlk=8, lok=4
        INTEGER(KIND=ink),PARAMETER                :: NDT=5_ink
        integer(kind=ink), value                    ::nel
        real(kind=rlk), value                       :: ale_sf, zerocut

        REAL(KIND=rlk),DIMENSION(:,:)               :: elu,elv
        REAL(KIND=rlk),DIMENSION(:)                 :: rscratch12, output
        ! Argument list
        ! Local
        INTEGER(KIND=ink)                          :: iel, ii, loc
        REAL(KIND=rlk)                             :: w1,w2, minw
    

        iel = threadIdx%x + (blockIdx%x-1)*blockDim%x
        if(iel<=nel) then
          output(iel) = HUGE(1.0_rlk)
          w1=MAX(elu(1,iel)*elu(1,iel)+elv(1,iel)*elv(1,iel),           &
&                elu(2,iel)*elu(2,iel)+elv(2,iel)*elv(2,iel),           &
&                elu(3,iel)*elu(3,iel)+elv(3,iel)*elv(3,iel),           &
&                elu(4,iel)*elu(4,iel)+elv(4,iel)*elv(4,iel))
          w1=rscratch12(iel)/MAX(w1,zerocut)
          if(w1<output(iel)) output(iel) = w1
      endif
    end subroutine ale_kernel
end module getdt_mod_kernel

MODULE getdt_mod

  IMPLICIT NONE

  PUBLIC :: getdt

CONTAINS
    subroutine getdt_host(dt, d_zdtnotreg, d_zmidlength, d_ielreg, d_rho, d_qq, d_csqrd, d_elx, d_ely,&
&                               d_a1, d_a3, d_b1, d_b3, d_ielnd, d_elvol, d_ndu, d_ndv, d_iellocglob, d_rho05, &
&                               d_ein05, d_elu, d_elv)

    use cudafor
    use getdt_mod_kernel
    USE kinds_mod,       ONLY: rlk,ink, lok
    USE reals_mod,       ONLY: ccut,zcut,cfl_sf,div_sf,ale_sf,dt_g,     &
&                              dt_min,dt_max,zerocut !all variables
    USE integers_mod,    ONLY: nel,nshape,nnod,idtel,idtreg,comms,nprocw
    USE logicals_mod,    ONLY: zdtnotreg,zmidlength,zparallel,zeul,zale,&
&                              zaleon
    USE strings_mod,     ONLY: sdt
    USE pointers_mod,    ONLY: ielreg,rho,qq,csqrd,elx,ely,a1,a3,b1,b3, &
&                              ielnd,elvol,ndu,ndv,iellocglob
    USE scratch_mod,     ONLY: rscratch11,rscratch12,elu=>rscratch21,   &
&                              elv=>rscratch22

    USE geometry_mod,    ONLY: dlm,dln,dlm_kernel, dln_kernel
    USE error_mod,       ONLY: halt
    USE utilities_mod,   ONLY: gather_kernel, gather
    USE timing_mod,      ONLY: bookleaf_times
    USE TYPH_util_mod,   ONLY: get_time
    use pointers_mod,    only: iellocglob
    USE TYPH_Collect_mod,ONLY: TYPH_Gather

    INTEGER(KIND=ink), allocatable, device             :: d_ielnd(:,:), d_ielreg(:), d_iellocglob(:)

    REAL(KIND=rlk),allocatable,dimension(:),device     :: d_qq, d_csqrd, d_a1, d_a3, d_b1,d_b3

    REAL(KIND=rlk),DIMENSION(:,:),allocatable,device   :: d_elx,d_ely,d_elu,d_elv
    REAL(KIND=rlk),DIMENSION(:),allocatable, device    :: d_rho, d_rho05,d_ndu, d_ndv, d_elvol,d_ein05
	
    REAL(KIND=rlk),DIMENSION(:), allocatable   :: h_rho05,h_elvol,h_ein05
    REAL(KIND=rlk),DIMENSION(nshape,nel)   :: h_elu,h_elv

    REAL(KIND=rlk),DIMENSION(:), device, allocatable    :: d_tmp_array

    logical(kind=lok), dimension(:), allocatable, device :: d_zdtnotreg, d_zmidlength
    ! Argument list
    REAL(KIND=rlk),INTENT(INOUT)               :: dt
    ! Local
    INTEGER(KIND=ink)                          :: iel,ireg,ip,ii
    INTEGER(KIND=ink),DIMENSION(1)             :: iloc
    INTEGER(KIND=ink),PARAMETER                :: NDT=5_ink
    INTEGER(KIND=ink),DIMENSION(NDT)           :: idt
    INTEGER(KIND=ink),DIMENSION(NDT),device    :: d_idt
    INTEGER(KIND=ink),DIMENSION(3,0:NProcW-1)  :: idtt
    REAL(KIND=rlk), device, allocatable        :: d_w1, w1_array(:)
    REAL(KIND=rlk)        ::  hw1_array(nel), hww(nel)
    integer(KIND=ink), device,allocatable      :: loc
    REAL(KIND=rlk)                             :: h_w1,w2, w1
    integer(KIND=ink)                          :: h_loc
    real(kind=rlk)                             :: t0,t1,t2,t3
    REAL(KIND=rlk),   DIMENSION(NDT)           :: rdt
    REAL(KIND=rlk),   DIMENSION(NDT),device    :: d_rdt
    REAL(KIND=rlk),   DIMENSION(0:NprocW-1)    :: dtt
    REAL(KIND=rlk),   DIMENSION(1)             :: dtm
    CHARACTER(LEN=8), DIMENSION(NDT),PARAMETER :: sdtt=['     CFL',     &
&                                                       '     DIV',     &
&                                                       '     ALE',     &
&                                                       '  GROWTH',     &
&                                                       ' MAXIMUM']

    integer::thread_num, block_num, nz, grid
    integer :: istat

    thread_num = 128
    block_num = ceiling(real(nel)/thread_num)

    rdt=HUGE(1.0_rlk)
    d_rdt=HUGE(1.0_rlk)
    allocate(d_w1)
	allocate(loc)
    allocate(d_tmp_array(nel))
    allocate(w1_array(nel))

    !CFL
    t0=get_time()
    call cfl_kernel<<<block_num, thread_num>>>(nel,nshape, d_ielreg, d_zdtnotreg, d_rho05, d_ein05, d_rho, zcut, &
&                               ccut, d_csqrd, d_qq, d_zmidlength, d_elx, d_ely, dt_max, cfl_sf,d_w1, loc) 
    call reduction(d_rho05, d_tmp_array, nel, 0, block_num, thread_num)
    loc = nel+1
    call get_min_loc<<<block_num, thread_num>>>(d_tmp_array, d_rho05, d_w1, loc, nel, d_rdt, d_idt, cfl_sf, 1)
    
    CALL gather_kernel<<<block_num, thread_num>>>(nshape,nel,nnod,d_ielnd,d_ndu,d_elv)
    CALL gather_kernel<<<block_num, thread_num>>>(nshape,nel,nnod,d_ielnd,d_ndv,d_elu)

    call divergence_kernel<<<block_num, thread_num>>>(nel, d_elvol, d_elu, d_elv, d_a1, d_a3, d_b1, d_b3, w1_array)
    d_tmp_array = TINY(1.0_rlk)
    call reduction(w1_array, d_tmp_array, nel, 1, block_num, thread_num)
    call get_max_loc<<<block_num, thread_num>>>(d_tmp_array,w1_array, d_w1, loc, nel, d_rdt, d_idt, div_sf)

       ! ALE
       IF (zale.AND.zaleon) THEN
             print *, "in ale"
         w2=HUGE(1.0_rlk)
         IF (zeul) THEN
             print *, "in ale"
             call ale_kernel<<<block_num, thread_num>>>(nel, d_elu, d_elv, d_ein05, zerocut, ale_sf, w1_array)
             d_tmp_array = HUGE(1.0_rlk)
             call reduction(d_ein05, d_tmp_array, nel, 0, block_num, thread_num)
             loc = nel+1
             call get_min_loc<<<block_num, thread_num>>>(d_tmp_array, d_ein05, d_w1, loc, nel, d_rdt, d_idt, ale_sf, 3)
             !w2 = d_w1
             !ii = loc
             !hw1_array = w1_array
             !do iel=1, nel
             !IF (hw1_array(iel).LT.w2) THEN
             !  w2=hw1_array(iel)
             !  ii=iel
             !ENDIF
             !ENDDO
         ENDIF
         
        !print *, "after ale"
        !rdt(3)=ale_sf*SQRT(w2)
         !idt(3)=ii
       ENDIF
       ! Growth
       rdt = d_rdt
       idt = d_idt
       rdt(4)=dt_g*dt
       idt(4)=-1_ink
       ! Maximum
       rdt(5)=dt_max
       idt(5)=-1_ink

       !# Missing code here that can't be merged

       ! Find smallest timestep, store info
       ii=MINVAL(MINLOC(rdt))
       dt=rdt(ii)
       idtel=idt(ii)
       IF (idtel.GT.0_ink) THEN
         idtreg=ielreg(idtel)
       ELSE
         idtreg=-1_ink
       ENDIF
       !print *, 'size of dtt', size(dtt), NprocW
       IF (zparallel) THEN
         idt(1)=idtel
         idt(2)=idtreg
         idt(3)=ii
         t1=get_time()
         ip=Typh_Gather(dt,dtt,comm=CommS)
         t2=get_time()
         t3=t2-t1
         ii=MINVAL(MINLOC(dtt))-1_ink  ! minloc converts (0:n) index to (1:n+1)
         dt=dtt(ii)
         !print *, 'dt is ', dt
         t1=get_time()
         ip=Typh_Gather(idt(1:3),idtt,comm=CommS)
         !print *, 'idt ', idt(1:3)
         !print *, 'idtt', idtt
         t2=get_time()
         t3=t3+t2-t1
         ! global time controlling cell 
         IF (idtt(1,ii).LE.0_ink) THEN
           idtel=-1_ink
         ELSE
           idtel=iellocglob(idtt(1,ii))
         ENDIF
         idtreg=idtt(2,ii)
         ii=idtt(3,ii)
         bookleaf_times%time_in_colls=bookleaf_times%time_in_colls+t3
       ENDIF
       sdt=sdtt(ii)

       ! Check minimum
       IF (dt.LT.dt_min) CALL halt("ERROR: dt < dt_min",1,.true.)
       t1=get_time()
       t1=t1-t0
       bookleaf_times%time_in_getdt=bookleaf_times%time_in_getdt+t1

       ! Timing data
end subroutine getdt_host


  SUBROUTINE getdt(dt)

    USE kinds_mod,       ONLY: rlk,ink
    USE reals_mod,       ONLY: ccut,zcut,cfl_sf,div_sf,ale_sf,dt_g,     &
&                              dt_min,dt_max,zerocut !all variables
    USE integers_mod,    ONLY: nel,nshape,nnod,idtel,idtreg,comms,nprocw
    USE logicals_mod,    ONLY: zdtnotreg,zmidlength,zparallel,zeul,zale,&
&                              zaleon
    USE strings_mod,     ONLY: sdt
    USE pointers_mod,    ONLY: ielreg,rho,qq,csqrd,elx,ely,a1,a3,b1,b3, &
&                              ielnd,elvol,ndu,ndv,iellocglob
    USE scratch_mod,     ONLY: rscratch11,rscratch12,elu=>rscratch21,   &
&                              elv=>rscratch22
    USE geometry_mod,    ONLY: dlm,dln
    USE error_mod,       ONLY: halt
    USE utilities_mod,   ONLY: gather
    USE timing_mod,      ONLY: bookleaf_times
    USE TYPH_util_mod,   ONLY: get_time
    USE TYPH_Collect_mod,ONLY: TYPH_Gather

    ! Argument list
    REAL(KIND=rlk),INTENT(INOUT)               :: dt
    ! Local
    INTEGER(KIND=ink)                          :: iel,ireg,ip,ii
    INTEGER(KIND=ink),DIMENSION(1)             :: iloc
    INTEGER(KIND=ink),PARAMETER                :: NDT=5_ink
    INTEGER(KIND=ink),DIMENSION(NDT)           :: idt
    INTEGER(KIND=ink),DIMENSION(3,0:NProcW-1)  :: idtt
    REAL(KIND=rlk)                             :: w1,w2,t0,t1,t2,t3
    REAL(KIND=rlk),   DIMENSION(NDT)           :: rdt
    REAL(KIND=rlk),   DIMENSION(0:NprocW-1)    :: dtt
    REAL(KIND=rlk),   DIMENSION(1)             :: dtm
    CHARACTER(LEN=8), DIMENSION(NDT),PARAMETER :: sdtt=['     CFL',     &
&                                                       '     DIV',     &
&                                                       '     ALE',     &
&                                                       '  GROWTH',     &
&                                                       ' MAXIMUM']

    ! Timer
    t0=get_time()

    ! Initialise
    rdt=HUGE(1.0_rlk)

    ! CFL
    DO iel=1,nel
      ireg=ielreg(iel)
      IF (zdtnotreg(ireg)) THEN
        rscratch11(iel)=dt_max
        rscratch12(iel)=TINY(1.0_rlk)
      ELSE
        w1=MAX(rho(iel),zcut)
        w2=MAX(ccut,csqrd(iel))+2.0_rlk*qq(iel)/w1
        IF (zmidlength(ireg)) THEN
          w1=MINVAL(dlm(nshape,elx(:,iel),ely(:,iel)))
        ELSE
          w1=MINVAL(dln(nshape,elx(:,iel),ely(:,iel), iel))
        ENDIF
        rscratch11(iel)=w1/w2
        rscratch12(iel)=w1
      ENDIF
    ENDDO
    ii=1_ink
    DO iel=2,nel
      IF (rscratch11(iel).LT.rscratch11(ii)) ii=iel
    ENDDO
    w1=rscratch11(ii)
    IF (w1.LT.0.0_rlk) CALL halt("ERROR: dt_cfl < 0",1)
    rdt(1)=cfl_sf*SQRT(w1)
    idt(1)=ii
    ! Divergence
    w2=TINY(1.0_rlk)
    CALL gather(nshape,nel,nnod,ielnd(1,1),ndu(1),elv(1,1))
    CALL gather(nshape,nel,nnod,ielnd(1,1),ndv(1),elu(1,1))
    DO iel=1,nel
      w1=elu(1,iel)*(-b3(iel)+b1(iel))+elv(1,iel)*( a3(iel)-a1(iel))+   &
&        elu(2,iel)*( b3(iel)+b1(iel))+elv(2,iel)*(-a3(iel)-a1(iel))+   &
&        elu(3,iel)*( b3(iel)-b1(iel))+elv(3,iel)*(-a3(iel)+a1(iel))+   &
&        elu(4,iel)*(-b3(iel)-b1(iel))+elv(4,iel)*( a3(iel)+a1(iel))
      w1=ABS(w1)/elvol(iel)
      IF (w1.GT.w2) THEN
        w2=w1
        ii=iel
      ENDIF
    ENDDO
    rdt(2)=div_sf/w2
    idt(2)=ii
    ! ALE
    IF (zale.AND.zaleon) THEN
          print *, "in ale"
      w2=HUGE(1.0_rlk)
      IF (zeul) THEN
        DO iel=1,nel
          w1=MAX(elu(1,iel)*elu(1,iel)+elv(1,iel)*elv(1,iel),           &
&                elu(2,iel)*elu(2,iel)+elv(2,iel)*elv(2,iel),           &
&                elu(3,iel)*elu(3,iel)+elv(3,iel)*elv(3,iel),           &
&                elu(4,iel)*elu(4,iel)+elv(4,iel)*elv(4,iel))
          w1=rscratch12(iel)/MAX(w1,zerocut)
          IF (w1.LT.w2) THEN
            w2=w1
            ii=iel
          ENDIF
        ENDDO
      ELSE
        ! Other options
      ENDIF
      print *, 'after ale'
      rdt(3)=ale_sf*SQRT(w2)
      idt(3)=ii
    ENDIF
    ! Growth
    rdt(4)=dt_g*dt
    idt(4)=-1_ink
    ! Maximum
    rdt(5)=dt_max
    idt(5)=-1_ink

    !# Missing code here that can't be merged

    ! Find smallest timestep, store info
    ii=MINVAL(MINLOC(rdt))
       if(ii==3) print *, "undefined 3"
    dt=rdt(ii)
    idtel=idt(ii)
    IF (idtel.GT.0_ink) THEN
      idtreg=ielreg(idtel)
    ELSE
      idtreg=-1_ink
    ENDIF
    IF (zparallel) THEN
      idt(1)=idtel
      idt(2)=idtreg
      idt(3)=ii
      t1=get_time()
      ip=Typh_Gather(dt,dtt,comm=CommS)
      t2=get_time()
      t3=t2-t1
      ii=MINVAL(MINLOC(dtt))-1_ink  ! minloc converts (0:n) index to (1:n+1)
      dt=dtt(ii)
      t1=get_time()
      ip=Typh_Gather(idt(1:3),idtt,comm=CommS)
      t2=get_time()
      t3=t3+t2-t1
      ! global time controlling cell 
      IF (idtt(1,ii).LE.0_ink) THEN
        idtel=-1_ink
      ELSE
        idtel=iellocglob(idtt(1,ii))
      ENDIF
      idtreg=idtt(2,ii)
      ii=idtt(3,ii)
      bookleaf_times%time_in_colls=bookleaf_times%time_in_colls+t3
    ENDIF
    sdt=sdtt(ii)

    ! Check minimum
    IF (dt.LT.dt_min) CALL halt("ERROR: dt < dt_min",1,.true.)

    ! Timing data
    t1=get_time()
    t1=t1-t0
    bookleaf_times%time_in_getdt=bookleaf_times%time_in_getdt+t1

  END SUBROUTINE getdt

END MODULE getdt_mod
