
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
MODULE geometry_mod

  IMPLICIT NONE

  PUBLIC  :: dlm,dln,getgeom, dlm_kernel, dln_kernel
  PRIVATE :: denom,distpp,distpl, denom_kernel, distpp_kernel, distpl_kernel

CONTAINS

  attributes(device) function dlm_kernel(nshape, elx1,elx2, elx3, elx4, ely1, ely2,ely3,ely4) result(res)
    integer,parameter::ink=4, rlk=8
    INTEGER(KIND=ink) :: nshape
    REAL(KIND=rlk),INTENT(IN) :: elx1,elx2, elx3, elx4,ely1, ely2, ely3, ely4
    ! Result
    REAL(KIND=rlk),DIMENSION(nshape)            :: res
    REAL(KIND=rlk)            :: res_value
    ! Local
    REAL(KIND=rlk)                              :: x1,x2,y1,y2
    integer:: i
    
    !elx ely can be put in shared memory here

    x1=elx1+elx1
    x2=elx3+elx4
    y1=ely1+ely2
    y2=ely3+ely4
    x1=0.5_rlk*(x1-x2)
    y1=0.5_rlk*(y1-y2)
    res(1)=x1*x1+y1*y1
    x1=elx3+elx3
    x2=elx1+elx4
    y1=ely3+ely2
    y2=ely1+ely4
    x1=0.5_rlk*(x1-x2)
    y1=0.5_rlk*(y1-y2)
    res(2)=x1*x1+y1*y1
    res(3)=res(1)
    res(4)=res(2)

    !res_value = res(1)
    !do i=2, nshape
    !    if(res_value > res(i) ) res_value = res(i)
    !enddo
  end function dlm_kernel

  attributes(device) pure function denom_kernel(x1,y1,x2, y2) 

    use cudafor
    integer,parameter:: rlk=8
    ! Argument list
    REAL(KIND=rlk),value,INTENT(IN) :: x1,y1,x2,y2
    ! Result
    REAL(KIND=rlk)            :: denom_kernel
    ! Local
    REAL(KIND=rlk)            :: w1,w2

    w1=y1-y2
    w2=x1-x2
    denom_kernel=w1*w1+w2*w2
  end function denom_kernel

  attributes(device) pure function dln_kernel(nshape, elx1d,elx2d, elx3d, elx4d, ely1d, ely2d,ely3d,ely4d, zcut) result(res)

    use cudafor
    integer,parameter::ink=4, rlk=8

    ! Argument list
    INTEGER(KIND=ink) :: nshape
    REAL(KIND=rlk),INTENT(IN) :: elx1d,elx2d, elx3d, elx4d,ely1d, ely2d, ely3d, ely4d
    REAL(KIND=rlk) :: elx1,elx2, elx3, elx4,ely1, ely2, ely3, ely4, tmp
    real(kind=rlk), value,           intent(in) :: zcut
    ! Result
    REAL(KIND=rlk),DIMENSION(nshape)            :: res
    REAL(KIND=rlk)            :: res_value
    ! Local
    REAL(KIND=rlk)                              :: w1
    integer:: i

    elx1 = elx1d
    elx2 = elx2d
    elx3 = elx3d
    elx4 = elx4d
    ely1 = ely1d
    ely2 = ely2d
    ely3 = ely3d
    ely4 = ely4d

    !w1=denom_kernel(elx3,ely3,elx4,ely4)
    w1 = (ely3-ely4)*(ely3-ely4)+(elx3-elx4)*(elx3-elx4)
    IF (w1.LT.zcut) THEN
      res(1) = (0.5_rlk*(elx1+elx2)-elx3)*(0.5_rlk*(elx1+elx2)-elx3) + &
&              (0.5_rlk*(ely1+ely2)-ely3)*(0.5_rlk*(ely1+ely2)-ely3) 
!      res(1)=distpp_kernel(elx1,ely1,elx2,ely2,elx3,ely3)
    ELSE
!      res(1)=distpl_kernel(elx1,ely1,elx2,ely2,elx3,ely3,elx4,   &
!&                   ely4)/w1
      tmp=0.5_rlk*(ely3-ely4)*(elx1+elx2)+0.5_rlk*(ely1+ely2)*(elx4-elx3)+ely4*elx3-ely3*elx4
      tmp = tmp*tmp
      res(1) = tmp/w1
    ENDIF
    !w1=denom_kernel(elx4,ely4,elx1,ely1)
    w1 = (ely4-ely1)*(ely4-ely1)+(elx4-elx1)*(elx4-elx1)
    IF (w1.LT.zcut) THEN
      !res(2)=distpp_kernel(elx2,ely2,elx3,ely3,elx4,ely4)
      res(2) = (0.5_rlk*(elx2+elx3)-elx4)*(0.5_rlk*(elx2+elx3)-elx4) + &
&              (0.5_rlk*(ely2+ely3)-ely4)*(0.5_rlk*(ely2+ely3)-ely4)
    ELSE
!      res(2)=distpl_kernel(elx2,ely2,elx3,ely3,elx4,ely4,elx1,   &
!&                   ely1)/w1
      tmp=0.5_rlk*(ely4-ely1)*(elx2+elx3)+0.5_rlk*(ely2+ely3)*(elx1-elx4)+ely1*elx4-ely4*elx1
      tmp = tmp*tmp
      res(2) = tmp/w1
    ENDIF
    !w1=denom_kernel(elx1,ely1,elx2,ely2)
    w1 = (ely1-ely2)*(ely1-ely2)+(elx1-elx2)*(elx1-elx2)
    IF (w1.LT.zcut) THEN
      !res(3)=distpp_kernel(elx3,ely3,elx4,ely4,elx1,ely1)
      res(3) = (0.5_rlk*(elx3+elx4)-elx1)*(0.5_rlk*(elx3+elx4)-elx1) + &
&              (0.5_rlk*(ely3+ely4)-ely1)*(0.5_rlk*(ely3+ely4)-ely1)
    ELSE
!      res(3)=distpl_kernel(elx3,ely3,elx4,ely4,elx1,ely1,elx2,   &
!&                   ely2)/w1
      tmp=0.5_rlk*(ely1-ely2)*(elx3+elx4)+0.5_rlk*(ely3+ely4)*(elx2-elx1)+ely2*elx1-ely1*elx2
      tmp = tmp*tmp
      res(3) = tmp/w1
    ENDIF
    !w1=denom_kernel(elx2,ely2,elx3,ely3)
    w1 = (ely2-ely3)*(ely2-ely3)+(elx2-elx3)*(elx2-elx3)
    IF (w1.LT.zcut) THEN
      !res(4)=distpp_kernel(elx4,ely4,elx1,ely1,elx2,ely2)
      res(4) = (0.5_rlk*(elx4+elx1)-elx2)*(0.5_rlk*(elx4+elx1)-elx2) + &
&              (0.5_rlk*(ely4+ely1)-ely2)*(0.5_rlk*(ely4+ely1)-ely2)
    ELSE
      !res(4)=distpl_kernel(elx4,ely4,elx1,ely1,elx2,ely2,elx3,   &
!&                   ely3)/w1
      tmp=0.5_rlk*(ely2-ely3)*(elx4+elx1)+0.5_rlk*(ely4+ely1)*(elx3-elx2)+ely3*elx2-ely2*elx3
      tmp = tmp*tmp
      res(4) = tmp/w1
    ENDIF

    !res_value = res(1)
    !do i=2, 4
    !    if(res_value > res(i) ) res_value = res(i)
    !enddo
end function dln_kernel

  PURE FUNCTION dlm(nshape,elx,ely) RESULT(res)

    USE kinds_mod,   ONLY: rlk,ink

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN) :: nshape
    REAL(KIND=rlk),DIMENSION(nshape),INTENT(IN) :: elx,ely
    ! Result
    REAL(KIND=rlk),DIMENSION(nshape)            :: res
    ! Local
    REAL(KIND=rlk)                              :: x1,x2,y1,y2

    x1=elx(1)+elx(2)
    x2=elx(3)+elx(4)
    y1=ely(1)+ely(2)
    y2=ely(3)+ely(4)
    x1=0.5_rlk*(x1-x2)
    y1=0.5_rlk*(y1-y2)
    res(1)=x1*x1+y1*y1
    x1=elx(3)+elx(2)
    x2=elx(1)+elx(4)
    y1=ely(3)+ely(2)
    y2=ely(1)+ely(4)
    x1=0.5_rlk*(x1-x2)
    y1=0.5_rlk*(y1-y2)
    res(2)=x1*x1+y1*y1
    res(3)=res(1)
    res(4)=res(2)

  END FUNCTION dlm

  PURE FUNCTION dln(nshape,elx,ely) RESULT(res)

    USE kinds_mod,   ONLY: rlk,ink
    USE reals_mod,   ONLY: zcut

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN) :: nshape
    REAL(KIND=rlk),DIMENSION(nshape),INTENT(IN) :: elx,ely
    ! Result
    REAL(KIND=rlk),DIMENSION(nshape)            :: res
    ! Local
    REAL(KIND=rlk)                              :: w1


    w1=denom(elx(3),ely(3),elx(4),ely(4))
    IF (w1.LT.zcut) THEN
      res(1)=distpp(elx(1),ely(1),elx(2),ely(2),elx(3),ely(3))
    ELSE
      res(1)=distpl(elx(1),ely(1),elx(2),ely(2),elx(3),ely(3),elx(4),   &
&                   ely(4))/w1
    ENDIF
    w1=denom(elx(4),ely(4),elx(1),ely(1))
    IF (w1.LT.zcut) THEN
      res(2)=distpp(elx(2),ely(2),elx(3),ely(3),elx(4),ely(4))
    ELSE
      res(2)=distpl(elx(2),ely(2),elx(3),ely(3),elx(4),ely(4),elx(1),   &
&                   ely(1))/w1
    ENDIF
    w1=denom(elx(1),ely(1),elx(2),ely(2))
    IF (w1.LT.zcut) THEN
      res(3)=distpp(elx(3),ely(3),elx(4),ely(4),elx(1),ely(1))
    ELSE
      res(3)=distpl(elx(3),ely(3),elx(4),ely(4),elx(1),ely(1),elx(2),   &
&                   ely(2))/w1
    ENDIF
    w1=denom(elx(2),ely(2),elx(3),ely(3))
    IF (w1.LT.zcut) THEN
      res(4)=distpp(elx(4),ely(4),elx(1),ely(1),elx(2),ely(2))
    ELSE
      res(4)=distpl(elx(4),ely(4),elx(1),ely(1),elx(2),ely(2),elx(3),   &
&                   ely(3))/w1
    ENDIF

  END FUNCTION dln

  attributes(global) subroutine getgeom_err_kenel(nel,elvol)

    integer,parameter::ink=4, rlk=8
    INTEGER(KIND=ink),  value, INTENT(IN)               :: nel
    INTEGER(KIND=ink)               :: ierr
    REAL(KIND=rlk),DIMENSION(:)   :: elvol, tmp, MASK
    INTEGER           :: iel

    iel = threadIdx%x + (blockIdx%x-1)*blockDim%x
  end subroutine getgeom_err_kenel

  attributes(global) subroutine getgeom_kenel(nel,elx,ely, a1, a2, a3, b1, b2, b3, elvol, cnwt)

    use cudafor
    implicit none
    integer,parameter::ink=4, rlk=8
    REAL(KIND=rlk),   PARAMETER :: ONEBYNINE=1.0_rlk/9.0_rlk

    INTEGER(KIND=ink),  value, INTENT(IN)               :: nel
    REAL(KIND=rlk),dimension(:)    :: a1,a2, a3, b1, b2, b3 
    REAL(KIND=rlk),DIMENSION(:,:)  :: elx,ely,cnwt

    REAL(KIND=rlk),DIMENSION(:)   :: elvol
    INTEGER(KIND=ink)            :: ielnd(:,:)

    
    !REAL(KIND=rlk),DIMENSION(nnod),       INTENT(IN)   :: ndx,ndy
    !REAL(KIND=rlk),DIMENSION(nshape,nel), INTENT(OUT)  :: elx,ely
    ! Local
    INTEGER           :: iel

    iel = threadIdx%x + (blockIdx%x-1)*blockDim%x

    if(iel<=nel) then
      a1(iel)=0.25_rlk*(-elx(1,iel)+elx(2,iel)+elx(3,iel)-elx(4,iel))
      a2(iel)=0.25_rlk*( elx(1,iel)-elx(2,iel)+elx(3,iel)-elx(4,iel))
      a3(iel)=0.25_rlk*(-elx(1,iel)-elx(2,iel)+elx(3,iel)+elx(4,iel))
      b1(iel)=0.25_rlk*(-ely(1,iel)+ely(2,iel)+ely(3,iel)-ely(4,iel))
      b2(iel)=0.25_rlk*( ely(1,iel)-ely(2,iel)+ely(3,iel)-ely(4,iel))
      b3(iel)=0.25_rlk*(-ely(1,iel)-ely(2,iel)+ely(3,iel)+ely(4,iel))
      cnwt(1,iel)=ONEBYNINE*                                            &
&                 ((3.0_rlk*b3(iel)-b2(iel))*(3.0_rlk*a1(iel)-a2(iel))  &
&                 -(3.0_rlk*a3(iel)-a2(iel))*(3.0_rlk*b1(iel)-b2(iel)))
      cnwt(2,iel)=ONEBYNINE*                                            &
&                 ((3.0_rlk*b3(iel)+b2(iel))*(3.0_rlk*a1(iel)-a2(iel))  &
&                 -(3.0_rlk*a3(iel)+a2(iel))*(3.0_rlk*b1(iel)-b2(iel)))
      cnwt(3,iel)=ONEBYNINE*                                            &
&                 ((3.0_rlk*b3(iel)+b2(iel))*(3.0_rlk*a1(iel)+a2(iel))  &
                  -(3.0_rlk*a3(iel)+a2(iel))*(3.0_rlk*b1(iel)+b2(iel)))
      cnwt(4,iel)=ONEBYNINE*                                            &
&                 ((3.0_rlk*b3(iel)-b2(iel))*(3.0_rlk*a1(iel)+a2(iel))  &
                  -(3.0_rlk*a3(iel)-a2(iel))*(3.0_rlk*b1(iel)+b2(iel)))
      elvol(iel)=4.0_rlk*(a1(iel)*b3(iel)-a3(iel)*b1(iel))
    endif

  end subroutine getgeom_kenel

 SUBROUTINE getgeom(nshape,nel,nnod,ndx,ndy,elx,ely,timer)

    USE kinds_mod,     ONLY: ink,rlk
    USE utilities_mod, ONLY: gather
    USE pointers_mod,  ONLY: a1,a2,a3,b1,b2,b3,elvol,cnwt,ielnd
    USE error_mod,     ONLY: halt
    USE parameters_mod,ONLY: ONEBYNINE
    USE TYPH_util_mod, ONLY: get_time

    ! Argument list
    INTEGER(KIND=ink),                    INTENT(IN)   :: nshape,nel,   &
&                                                         nnod
    REAL(KIND=rlk),DIMENSION(nnod),       INTENT(IN)   :: ndx,ndy
    REAL(KIND=rlk),DIMENSION(nshape,nel), INTENT(OUT)  :: elx,ely
    REAL(KIND=rlk),                       INTENT(INOUT):: timer
    ! Local
    INTEGER(KIND=ink)                                  :: iel,ierr
    REAL(KIND=rlk)                                     :: t0,t1

    ! Timer
    t0=get_time()

    ! Gather position to element
    CALL gather(nshape,nel,nnod,ielnd(1,1),ndx(1),elx(1,1))
    CALL gather(nshape,nel,nnod,ielnd(1,1),ndy(1),ely(1,1))

    ! Calculate volume and iso-parametric terms
    DO iel=1,nel
      a1(iel)=0.25_rlk*(-elx(1,iel)+elx(2,iel)+elx(3,iel)-elx(4,iel))
      a2(iel)=0.25_rlk*( elx(1,iel)-elx(2,iel)+elx(3,iel)-elx(4,iel))
      a3(iel)=0.25_rlk*(-elx(1,iel)-elx(2,iel)+elx(3,iel)+elx(4,iel))
      b1(iel)=0.25_rlk*(-ely(1,iel)+ely(2,iel)+ely(3,iel)-ely(4,iel))
      b2(iel)=0.25_rlk*( ely(1,iel)-ely(2,iel)+ely(3,iel)-ely(4,iel))
      b3(iel)=0.25_rlk*(-ely(1,iel)-ely(2,iel)+ely(3,iel)+ely(4,iel))
      cnwt(1,iel)=ONEBYNINE*                                            &
&                 ((3.0_rlk*b3(iel)-b2(iel))*(3.0_rlk*a1(iel)-a2(iel))  &
&                 -(3.0_rlk*a3(iel)-a2(iel))*(3.0_rlk*b1(iel)-b2(iel)))
      cnwt(2,iel)=ONEBYNINE*                                            &
&                 ((3.0_rlk*b3(iel)+b2(iel))*(3.0_rlk*a1(iel)-a2(iel))  &
&                 -(3.0_rlk*a3(iel)+a2(iel))*(3.0_rlk*b1(iel)-b2(iel)))
      cnwt(3,iel)=ONEBYNINE*                                            &
&                 ((3.0_rlk*b3(iel)+b2(iel))*(3.0_rlk*a1(iel)+a2(iel))  &
                  -(3.0_rlk*a3(iel)+a2(iel))*(3.0_rlk*b1(iel)+b2(iel)))
      cnwt(4,iel)=ONEBYNINE*                                            &
&                 ((3.0_rlk*b3(iel)-b2(iel))*(3.0_rlk*a1(iel)+a2(iel))  &
                  -(3.0_rlk*a3(iel)-a2(iel))*(3.0_rlk*b1(iel)+b2(iel)))
      elvol(iel)=4.0_rlk*(a1(iel)*b3(iel)-a3(iel)*b1(iel))
    ENDDO
    IF (ANY(elvol(1:nel).LT.0.0_rlk)) THEN
      ierr=MINVAL(MINLOC(elvol(1:nel),MASK=(elvol(1:nel).LT.0.0_rlk))-1_ink)
      IF (ierr.NE.0_ink) CALL halt("ERROR: cell volume < 0",1)
    ENDIF

    ! Timing data
    t1=get_time()
    t1=t1-t0
    timer=timer+t1

  END SUBROUTINE getgeom

  SUBROUTINE getgeom_host(nshape,nel,nnod,d_ndx,d_ndy,d_elx,d_ely,d_a1, d_a2, d_a3, d_b1, d_b2, d_b3,&
&                    d_elvol, d_cnwt, d_ielnd, timer)

    use cudafor
    USE kinds_mod,     ONLY: ink,rlk
    USE utilities_mod, ONLY: gather, gather_kernel
    USE pointers_mod,  ONLY: elvol !a1,a2,a3,b1,b2,b3,elvol,cnwt,ielnd
    USE error_mod,     ONLY: halt
    USE parameters_mod,ONLY: ONEBYNINE
    USE TYPH_util_mod, ONLY: get_time

    ! Argument list
    INTEGER(KIND=ink),                    INTENT(IN)   :: nshape,nel, nnod
    REAL(KIND=rlk),dimension(:),allocatable, device     :: d_a1,d_a2, d_a3, d_b1, d_b2, d_b3 
    REAL(KIND=rlk),DIMENSION(:,:),allocatable, device   :: d_elx,d_ely,d_cnwt

    REAL(KIND=rlk),DIMENSION(:), allocatable, device    :: d_elvol, d_ndx, d_ndy
    INTEGER(KIND=ink),allocatable, device             :: d_ielnd(:,:)

    
    !REAL(KIND=rlk),DIMENSION(nnod),       INTENT(IN)   :: ndx,ndy
    !REAL(KIND=rlk),DIMENSION(nshape,nel), INTENT(OUT)  :: elx,ely
    REAL(KIND=rlk),                       INTENT(INOUT):: timer
    ! Local
    INTEGER(KIND=ink)                                  :: iel,ierr
    REAL(KIND=rlk)                                     :: t0,t1
    integer::thread_num, block_num

    ! Timer
    t0=get_time()

    ! Gather position to element
    thread_num = 128
    block_num = ceiling(real(nel)/thread_num)

    CALL gather_kernel<<<block_num, thread_num>>>(nshape,nel,nnod,d_ielnd, d_ndx,d_elx)
    CALL gather_kernel<<<block_num, thread_num>>>(nshape,nel,nnod,d_ielnd,d_ndy,d_ely)

    call getgeom_kenel<<<block_num, thread_num>>>(nel,d_elx,d_ely, d_a1, d_a2, d_a3, d_b1, d_b2, d_b3, d_elvol, d_cnwt)
    !elvol = d_elvol

    ! Calculate volume and iso-parametric terms
    !IF (ANY(elvol(1:nel).LT.0.0_rlk)) THEN
    !  print *, 'here ',size(MINLOC(elvol(1:nel),MASK=(elvol(1:nel).LT.0.0_rlk))-1_ink)
    !  ierr=MINVAL(MINLOC(elvol(1:nel),MASK=(elvol(1:nel).LT.0.0_rlk))-1_ink)
    !  IF (ierr.NE.0_ink) CALL halt("ERROR: cell volume < 0",1)
    !ENDIF

    ! Timing data
    t1=get_time()
    t1=t1-t0
    timer=timer+t1

  END SUBROUTINE getgeom_host


  PURE FUNCTION denom(x1,y1,x2,y2)

    USE kinds_mod,ONLY: rlk

    ! Argument list
    REAL(KIND=rlk),INTENT(IN) :: x1,y1,x2,y2
    ! Result
    REAL(KIND=rlk)            :: denom
    ! Local
    REAL(KIND=rlk)            :: w1,w2

    w1=y1-y2
    w2=x1-x2
    denom=w1*w1+w2*w2

  END FUNCTION denom

  attributes(device) pure function distpp_kernel(x3,y3,x4,y4,x1,y1)

    integer,parameter:: rlk=8

    ! Argument list
    REAL(KIND=rlk),value,INTENT(IN) :: x3,y3,x4,y4,x1,y1
    ! Result
    REAL(KIND=rlk)            :: distpp_kernel
    ! Local
    REAL(KIND=rlk)            :: w1,w2

    w1=0.5_rlk*(x3+x4)-x1
    w2=0.5_rlk*(y3+y4)-y1
    distpp_kernel=w1*w1+w2*w2

  end function distpp_kernel  

  PURE FUNCTION distpp(x3,y3,x4,y4,x1,y1)

    USE kinds_mod,ONLY: rlk

    ! Argument list
    REAL(KIND=rlk),INTENT(IN) :: x3,y3,x4,y4,x1,y1
    ! Result
    REAL(KIND=rlk)            :: distpp
    ! Local
    REAL(KIND=rlk)            :: w1,w2

    w1=0.5_rlk*(x3+x4)-x1
    w2=0.5_rlk*(y3+y4)-y1
    distpp=w1*w1+w2*w2

  END FUNCTION distpp  

  attributes(device) PURE FUNCTION distpl_kernel(x3,y3,x4,y4,x1,y1,x2,y2)

    integer,parameter:: rlk=8

    ! Argument list
    REAL(KIND=rlk),value, INTENT(IN) :: x3,y3,x4,y4,x1,y1,x2,y2
    ! Result
    REAL(KIND=rlk)            :: distpl_kernel

    distpl_kernel=0.5_rlk*(y1-y2)*(x3+x4)+0.5_rlk*(y3+y4)*(x2-x1)+y2*x1-y1*x2
    distpl_kernel=distpl_kernel*distpl_kernel

  END FUNCTION distpl_kernel

  PURE FUNCTION distpl(x3,y3,x4,y4,x1,y1,x2,y2)

    USE kinds_mod,ONLY: rlk

    ! Argument list
    REAL(KIND=rlk),INTENT(IN) :: x3,y3,x4,y4,x1,y1,x2,y2
    ! Result
    REAL(KIND=rlk)            :: distpl

    distpl=0.5_rlk*(y1-y2)*(x3+x4)+0.5_rlk*(y3+y4)*(x2-x1)+y2*x1-y1*x2
    distpl=distpl*distpl

  END FUNCTION distpl

END MODULE geometry_mod
