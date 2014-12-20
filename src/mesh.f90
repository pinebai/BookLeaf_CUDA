
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


!#######################################################################
! Mesh generator
!#######################################################################

MODULE mesh_mod

  USE kinds_mod,      ONLY: ink,lok,rlk

  IMPLICIT NONE

  TYPE segs
    INTEGER(KIND=ink)                            :: seg_no
    INTEGER(KIND=ink)                            :: seg_typ
    INTEGER(KIND=ink)                            :: bc
    REAL(KIND=rlk),               DIMENSION(1:8) :: point
  END TYPE segs
  TYPE sides
    INTEGER(KIND=ink)                            :: no_seg
    TYPE(segs),       POINTER,    DIMENSION(:)   :: seg
  END TYPE sides
  TYPE regions
    INTEGER(KIND=ink),            DIMENSION(1:2) :: dim
    INTEGER(KIND=ink)                            :: typ
    INTEGER(KIND=ink)                            :: no_it
    INTEGER(KIND=ink)                            :: mat
    INTEGER(KIND=ink)                            :: vel_typ
    REAL(KIND=rlk),               DIMENSION(1:3) :: vel
    LOGICAL(KIND=lok),POINTER,    DIMENSION(:,:) :: merged
    REAL(KIND=rlk)                               :: tol
    REAL(KIND=rlk)                               :: om
    REAL(KIND=rlk),   POINTER,    DIMENSION(:,:) :: rr
    REAL(KIND=rlk),   POINTER,    DIMENSION(:,:) :: ss
    INTEGER(KIND=ink),POINTER,    DIMENSION(:,:) :: bc
    REAL(KIND=rlk),               DIMENSION(1:8) :: r_wgt
    REAL(KIND=rlk),               DIMENSION(1:8) :: s_wgt
    REAL(KIND=rlk),               DIMENSION(1:16):: wgt
    TYPE(sides),                  DIMENSION(1:4) :: side
  END TYPE regions

  PRIVATE :: check_input,reg_mesh,line,arc,segs,sides
  PUBLIC  :: mesh_gen,mesh_transfer,mesh_print,regions

CONTAINS

  SUBROUTINE mesh_gen(reg)
    
    USE integers_mod,ONLY: nel,nel1,nnod,nnod1

    ! Argument list
    TYPE(regions),    DIMENSION(:),  ALLOCATABLE   :: reg

    ! read control file
    CALL read_input(reg)

    ! check user input
    CALL check_input(reg)

    ! Generate region meshes
    CALL reg_mesh(reg)

    ! Until decomposition, copy size
    nel1=nel
    nnod1=nnod

  END SUBROUTINE mesh_gen

  SUBROUTINE read_input(reg)

    USE parameters_mod, ONLY: MNS=>MAX_NAMELIST_SIZE,LI
    USE strings_mod,    ONLY: sfile
    USE error_mod,      ONLY: halt
    USE integers_mod,   ONLY: nreg,nmat,max_seg,max_subseg
    USE utilities_mod,  ONLY: findstr

    ! Argument list
    TYPE(regions), DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: reg
    ! Local
    INTEGER(KIND=ink)           :: i1,i2,i3,ii,ij,ierr
    INTEGER(KIND=ink),PARAMETER :: IUNIT=30_ink
    CHARACTER(LEN=LI)           :: str
    LOGICAL(KIND=lok)           :: zflag
    ! defaults
    INTEGER(KIND=ink),PARAMETER :: reg_side_default=0_ink,              &
&                                  mesh_dimension_default=0_ink,        &
&                                  vel_typ_default=0_ink,               &
&                                  mat_default=0_ink
    CHARACTER(LEN=4), PARAMETER :: reg_type_default='    '
    CHARACTER(LEN=5), PARAMETER :: seg_type_default='     ',            &
&                                  seg_bc_default='FREE'
    REAL(KIND=rlk),   PARAMETER :: reg_tol_default=1.0e-12_rlk,         &
&                                  reg_om_default=1.0_rlk,              &
&                                  vel_default=0.0_rlk,                 &
&                                  seg_default=0.0_rlk
    ! input
    INTEGER(KIND=ink),DIMENSION(MNS,4,MNS) :: region_side
    INTEGER(KIND=ink),DIMENSION(MNS,2)     :: region_dim
    INTEGER(KIND=ink),DIMENSION(MNS)       :: region_material,          &
&                                             region_vel_typ
    REAL(KIND=rlk),   DIMENSION(MNS,3)     :: region_vel
    REAL(KIND=rlk),   DIMENSION(MNS)       :: region_om,region_tol
    CHARACTER(LEN=4), DIMENSION(MNS)       :: region_typ
    REAL(KIND=rlk),   DIMENSION(MNS,8)     :: segment_pos
    CHARACTER(LEN=5), DIMENSION(MNS)       :: segment_typ,segment_bc

    ! NB. easier as a derived type, valid Fortran that some compilers
    !     don't support
    NAMELIST /MESH/ region_side,region_dim,region_material,             &
&                   region_vel_typ,region_vel,region_om,region_tol,     &
&                   region_typ,segment_pos,segment_typ,segment_bc

    IF (max_seg.GT.MNS) THEN
      CALL halt('ERROR: max_seg > MAX_NAMELIST_SIZE',0)
    ENDIF
    IF (max_subseg-1.GT.MNS) THEN
      CALL halt('ERROR: max_subseg > MAX_NAMELIST_SIZE',0)
    ENDIF

    ! set defaults
    DO ii=1,nreg
      region_typ(ii)=reg_type_default
      region_dim(ii,:)=mesh_dimension_default
      DO ij=1,4
        region_side(ii,ij,:)=reg_side_default
      ENDDO
      region_tol(ii)=reg_tol_default
      region_om(ii)=reg_om_default
      region_vel_typ(ii)=vel_typ_default
      region_vel(ii,:)=vel_default
      region_material(ii)=mat_default
    ENDDO
    DO ii=1,max_seg
      segment_typ(ii)=seg_type_default
      segment_bc(ii)=seg_bc_default
      segment_pos(ii,:)=seg_default
    ENDDO

    ! Open control file
    OPEN(UNIT=IUNIT,FILE=sfile,ACTION='read',STATUS='old',                &
&      FORM='formatted',IOSTAT=ierr,IOMSG=str)
    IF (ierr.NE.0_ink) CALL halt("ERROR: "//TRIM(str),0)

    ! Find mesh
    zflag=findstr('mesh',IUNIT)
    IF (zflag) THEN
      REWIND(UNIT=IUNIT)
      READ(UNIT=IUNIT,NML=MESH,IOSTAT=ierr,IOMSG=str)
      IF (ierr.NE.0_ink) CALL halt("ERROR: "//TRIM(str),0)
    ENDIF
    CLOSE(IUNIT)

    ! transfer mesh input to pass to mesh generator
    ALLOCATE(reg(1:nreg))
    DO i1=1,nreg
      SELECT CASE(region_typ(i1))
        CASE('LIN1')
          reg(i1)%typ=1_ink
        CASE('LIN2')
          reg(i1)%typ=2_ink
        CASE('EQUI')
          reg(i1)%typ=3_ink
        CASE('USER')
          reg(i1)%typ=4_ink
        CASE DEFAULT
          CALL halt('ERROR: unrecognised region type',0)
      END SELECT
      reg(i1)%dim(1)=region_dim(i1,2)
      reg(i1)%dim(2)=region_dim(i1,1)
      IF (reg(i1)%dim(1).LE.0_ink) THEN
        CALL halt('ERROR: region L dimension <= 0',0)
      ENDIF
      IF (reg(i1)%dim(2).LE.0_ink) THEN
        CALL halt('ERROR: region K dimension <=0',0)
      ENDIF
      reg(i1)%tol=region_tol(i1)
      IF (reg(i1)%tol.LT.0.0_rlk) THEN
        CALL halt('ERROR: reg%tol < 0.0',0)
      ENDIF
      reg(i1)%om=region_om(i1)
      IF (reg(i1)%om.LT.0.0_rlk) THEN
        CALL halt('ERROR: reg%om < 0.0',0)
      ENDIF
      reg(i1)%mat=region_material(i1)
      IF ((reg(i1)%mat.LE.0_ink).OR.(reg(i1)%mat.GT.nmat)) THEN
        CALL halt('ERROR: reg%mat <= 0 or > nmat',0)
      ENDIF
      SELECT CASE(region_vel_typ(i1))
        CASE(0_ink)
          ! pick up from defaults
        CASE(1_ink)
          reg(i1)%vel_typ=1_ink
          reg(i1)%vel(1)=region_vel(i1,1)
          reg(i1)%vel(2)=region_vel(i1,2)
          reg(i1)%vel(3)=0.0_rlk
        CASE(2_ink)
          reg(i1)%vel_typ=2_ink
          reg(i1)%vel(1)=region_vel(i1,1)
          reg(i1)%vel(2)=region_vel(i1,2)
          reg(i1)%vel(3)=region_vel(i1,3)
        CASE DEFAULT
          CALL halt('ERROR: unrecognised region_vel_typ',0)
      END SELECT
      DO i2=1,4
        ii=region_side(i1,i2,1)
        reg(i1)%side(i2)%no_seg=ii
        IF (ii.LT.1_ink) CALL halt('ERROR: no_seg < 1',0)
        IF (ii.GT.max_subseg) THEN
          CALL halt('ERROR: no_seg > max_subseg',0)
        ENDIF
        ALLOCATE(reg(i1)%side(i2)%seg(1:ii))
        DO i3=1,ii
          ij=region_side(i1,i2,i3+1)
          reg(i1)%side(i2)%seg(i3)%seg_no=ij
          IF (ij.LT.1_ink) CALL halt('ERROR: seg_no < 1',0)
          IF (ij.GT.max_seg) CALL halt('ERROR: seg_no > max_seg',0)
          ! For some reason gfortran 4.2.0 or 4.4.0 produce warning 
          ! if the following is coded using SELECT CASE
          IF (TRIM(ADJUSTL(segment_typ(ij))).EQ.'LINE') THEN
            reg(i1)%side(i2)%seg(i3)%seg_typ=1_ink
          ELSEIF (TRIM(ADJUSTL(segment_typ(ij))).EQ.'ARC_C') THEN
            reg(i1)%side(i2)%seg(i3)%seg_typ=2_ink
          ELSEIF (TRIM(ADJUSTL(segment_typ(ij))).EQ.'ARC_A') THEN
            reg(i1)%side(i2)%seg(i3)%seg_typ=3_ink
          ELSEIF (TRIM(ADJUSTL(segment_typ(ij))).EQ.'POINT') THEN
            reg(i1)%side(i2)%seg(i3)%seg_typ=4_ink
          ELSEIF (TRIM(ADJUSTL(segment_typ(ij))).EQ.'LINK')  THEN
            reg(i1)%side(i2)%seg(i3)%seg_typ=5_ink
          ELSE
            CALL halt('ERROR: unrecognised segment type',0)
          ENDIF
          reg(i1)%side(i2)%seg(i3)%point(1:8)=segment_pos(ij,1:8)
          ! For some reason gfortran 4.2.0 or 4.4.0 produce warning 
          ! if the following is coded using SELECT CASE
          IF (TRIM(ADJUSTL(segment_bc(ij))).EQ.'SLIPX') THEN
            reg(i1)%side(i2)%seg(i3)%bc=1_ink
          ELSEIF (TRIM(ADJUSTL(segment_bc(ij))).EQ.'SLIPY') THEN
            reg(i1)%side(i2)%seg(i3)%bc=2_ink
          ELSEIF (TRIM(ADJUSTL(segment_bc(ij))).EQ.'WALL')  THEN
            reg(i1)%side(i2)%seg(i3)%bc=3_ink
          ELSEIF (TRIM(ADJUSTL(segment_bc(ij))).EQ.'TRANS') THEN
            reg(i1)%side(i2)%seg(i3)%bc=6_ink
          ELSEIF (TRIM(ADJUSTL(segment_bc(ij))).EQ.'OPEN')  THEN
            reg(i1)%side(i2)%seg(i3)%bc=7_ink
          ELSEIF (TRIM(ADJUSTL(segment_bc(ij))).EQ.'FREE')  THEN
            reg(i1)%side(i2)%seg(i3)%bc=8
          ELSE
            CALL halt('ERROR: unrecognised segment boundary condition',0)
          ENDIF
        ENDDO
      ENDDO
    ENDDO

  END SUBROUTINE read_input

  SUBROUTINE check_input(reg)

    USE integers_mod,ONLY: nreg
    USE error_mod,   ONLY: halt

    ! Argument list
    TYPE(regions),DIMENSION(:),INTENT(INOUT) :: reg
    ! Local
    INTEGER(KIND=ink) :: ii,no_seg1,no_seg2,swap,ireg,ind
    REAL(KIND=rlk)    :: l1,l2,k1,k2,l3,k3,l4,k4

    IF (nreg.LE.0) THEN
      CALL halt('ERROR: nreg <= 0',0)
    ENDIF
    DO ireg=1,nreg
      DO ind=1,4
        IF (reg(ireg)%side(ind)%no_seg.GT.1_ink) THEN
          IF (ANY(reg(ireg)%side(ind)%seg(:)%seg_typ.EQ.4_ink)) THEN
            CALL halt('ERROR: POINT as part of multi-segment '  &
&                             //'side',0)
          ENDIF
          ! Check segments connected
          l1=reg(ireg)%side(ind)%seg(1)%point(3)
          k1=reg(ireg)%side(ind)%seg(1)%point(4)
          l2=reg(ireg)%side(ind)%seg(2)%point(1)
          k2=reg(ireg)%side(ind)%seg(2)%point(2)
          IF ((l1.NE.l2).OR.(k1.NE.k2)) THEN
            ! try swapping segment 2
            l3=reg(ireg)%side(ind)%seg(2)%point(3)
            k3=reg(ireg)%side(ind)%seg(2)%point(4)
            IF ((l1.NE.l3).OR.(k1.NE.k3)) THEN
              ! try swapping segment 1
              l4=reg(ireg)%side(ind)%seg(1)%point(1)
              k4=reg(ireg)%side(ind)%seg(1)%point(2)
              IF ((l4.NE.l2).OR.(k4.NE.k2)) THEN
                ! try swapping both
                IF ((l4.NE.l3).OR.(k4.NE.k3)) THEN
                  CALL halt('ERROR: failed to form continuous ' &
&                                   //'side',0)
                ELSE
                  reg(ireg)%side(ind)%seg(1)%point(1)=l1
                  reg(ireg)%side(ind)%seg(1)%point(2)=k1
                  reg(ireg)%side(ind)%seg(1)%point(3)=l4
                  reg(ireg)%side(ind)%seg(1)%point(4)=k4
                  reg(ireg)%side(ind)%seg(2)%point(1)=l3
                  reg(ireg)%side(ind)%seg(2)%point(2)=k3
                  reg(ireg)%side(ind)%seg(2)%point(3)=l2
                  reg(ireg)%side(ind)%seg(2)%point(4)=k2
                ENDIF
              ELSE
                reg(ireg)%side(ind)%seg(1)%point(1)=l1
                reg(ireg)%side(ind)%seg(1)%point(2)=k1
                reg(ireg)%side(ind)%seg(1)%point(3)=l4
                reg(ireg)%side(ind)%seg(1)%point(4)=k4
              ENDIF
            ELSE
              reg(ireg)%side(ind)%seg(2)%point(1)=l3
              reg(ireg)%side(ind)%seg(2)%point(2)=k3
              reg(ireg)%side(ind)%seg(2)%point(3)=l2
              reg(ireg)%side(ind)%seg(2)%point(4)=k2
            ENDIF
          ENDIF
          IF (reg(ireg)%side(ind)%no_seg.GT.2_ink) THEN
            DO ii=2,reg(ireg)%side(ind)%no_seg-1_ink
              l1=reg(ireg)%side(ind)%seg(ii)%point(3)
              k1=reg(ireg)%side(ind)%seg(ii)%point(4)
              l2=reg(ireg)%side(ind)%seg(ii+1)%point(1)
              k2=reg(ireg)%side(ind)%seg(ii+1)%point(2)
              IF ((l1.NE.l2).OR.(k1.NE.k2)) THEN
                ! try swapping segment ii+1
                l3=reg(ireg)%side(ind)%seg(ii+1)%point(3)
                k3=reg(ireg)%side(ind)%seg(ii+1)%point(4)
                IF ((l1.NE.l3).OR.(k1.NE.k3)) THEN
                  CALL halt('ERROR: failed to form continuous ' &
&                                   //'side',0)
                ELSE
                  reg(ireg)%side(ind)%seg(ii+1)%point(1)=l3
                  reg(ireg)%side(ind)%seg(ii+1)%point(2)=k3
                  reg(ireg)%side(ind)%seg(ii+1)%point(3)=l2
                  reg(ireg)%side(ind)%seg(ii+1)%point(4)=k2
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDIF
      ENDDO
      ! Check sides form closed polygon
      no_seg1=reg(ireg)%side(1)%no_seg
      l1=reg(ireg)%side(1)%seg(no_seg1)%point(3)
      k1=reg(ireg)%side(1)%seg(no_seg1)%point(4)
      l2=reg(ireg)%side(2)%seg(1)%point(1)
      k2=reg(ireg)%side(2)%seg(1)%point(2)
      IF ((l1.NE.l2).OR.(k1.NE.k2)) THEN
        ! try swapping side 2
        no_seg2=reg(ireg)%side(2)%no_seg
        l3=reg(ireg)%side(2)%seg(no_seg2)%point(3)
        k3=reg(ireg)%side(2)%seg(no_seg2)%point(4)
        IF ((l1.NE.l3).OR.(k1.NE.k3)) THEN
          ! try swapping side 1
          l4=reg(ireg)%side(1)%seg(1)%point(1)
          k4=reg(ireg)%side(1)%seg(1)%point(2)
          IF ((l4.NE.l2).OR.(k4.NE.k2)) THEN
            ! try swapping both
            IF ((l4.NE.l3).OR.(k4.NE.k3)) THEN
              CALL halt('ERROR: region not closed',0)
            ELSE
              IF (no_seg1.EQ.1_ink) THEN
                reg(ireg)%side(1)%seg(1)%point(1)=l1
                reg(ireg)%side(1)%seg(1)%point(2)=k1
                reg(ireg)%side(1)%seg(1)%point(3)=l4
                reg(ireg)%side(1)%seg(1)%point(4)=k4
              ELSE
                DO ii=1,no_seg1
                  swap=reg(ireg)%side(1)%seg(ii)%point(1)
                  reg(ireg)%side(1)%seg(ii)%point(1)=                   &
&                  reg(ireg)%side(1)%seg(ii)%point(3)
                  reg(ireg)%side(1)%seg(ii)%point(3)=swap
                  swap=reg(ireg)%side(1)%seg(ii)%point(2)
                  reg(ireg)%side(1)%seg(ii)%point(2)=                   &
&                  reg(ireg)%side(1)%seg(ii)%point(4)
                  reg(ireg)%side(1)%seg(ii)%point(4)=swap
                ENDDO
              ENDIF
              IF (no_seg2.EQ.1_ink) THEN
                reg(ireg)%side(2)%seg(1)%point(1)=l3
                reg(ireg)%side(2)%seg(1)%point(2)=k3
                reg(ireg)%side(2)%seg(1)%point(3)=l2
                reg(ireg)%side(2)%seg(1)%point(4)=k2
              ELSE
                DO ii=1,no_seg2
                  swap=reg(ireg)%side(2)%seg(ii)%point(1)
                  reg(ireg)%side(2)%seg(ii)%point(1)=                   &
&                  reg(ireg)%side(2)%seg(ii)%point(3)
                  reg(ireg)%side(2)%seg(ii)%point(3)=swap
                  swap=reg(ireg)%side(2)%seg(ii)%point(2)
                  reg(ireg)%side(2)%seg(ii)%point(2)=                   &
&                  reg(ireg)%side(2)%seg(ii)%point(4)
                  reg(ireg)%side(2)%seg(ii)%point(4)=swap
                ENDDO
              ENDIF
            ENDIF
          ELSE
            IF (no_seg1.EQ.1_ink) THEN
              reg(ireg)%side(1)%seg(1)%point(1)=l1
              reg(ireg)%side(1)%seg(1)%point(2)=k1
              reg(ireg)%side(1)%seg(1)%point(3)=l4
              reg(ireg)%side(1)%seg(1)%point(4)=k4
            ELSE
              DO ii=1,no_seg1
                swap=reg(ireg)%side(1)%seg(ii)%point(1)
                reg(ireg)%side(1)%seg(ii)%point(1)=                     &
&                reg(ireg)%side(1)%seg(ii)%point(3)
                reg(ireg)%side(1)%seg(ii)%point(3)=swap
                swap=reg(ireg)%side(1)%seg(ii)%point(2)
                reg(ireg)%side(1)%seg(ii)%point(2)=                     &
&                reg(ireg)%side(1)%seg(ii)%point(4)
                reg(ireg)%side(1)%seg(ii)%point(4)=swap
              ENDDO
            ENDIF
          ENDIF
        ELSE
          IF (no_seg2.EQ.1_ink) THEN
            reg(ireg)%side(2)%seg(1)%point(1)=l3
            reg(ireg)%side(2)%seg(1)%point(2)=k3
            reg(ireg)%side(2)%seg(1)%point(3)=l2
            reg(ireg)%side(2)%seg(1)%point(4)=k2
          ELSE
            DO ii=1,no_seg2
              swap=reg(ireg)%side(2)%seg(ii)%point(1)
              reg(ireg)%side(2)%seg(ii)%point(1)=                       &
&              reg(ireg)%side(2)%seg(ii)%point(3)
              reg(ireg)%side(2)%seg(ii)%point(3)=swap
              swap=reg(ireg)%side(2)%seg(ii)%point(2)
              reg(ireg)%side(2)%seg(ii)%point(2)=                       &
&              reg(ireg)%side(2)%seg(ii)%point(4)
              reg(ireg)%side(2)%seg(ii)%point(4)=swap
            ENDDO
          ENDIF
        ENDIF
      ENDIF
      DO ind=2,3
        no_seg1=reg(ireg)%side(ind)%no_seg
        l1=reg(ireg)%side(ind)%seg(no_seg1)%point(3)
        k1=reg(ireg)%side(ind)%seg(no_seg1)%point(4)
        l2=reg(ireg)%side(ind+1)%seg(1)%point(1)
        k2=reg(ireg)%side(ind+1)%seg(1)%point(2)
        IF ((l1.NE.l2).OR.(k1.NE.k2)) THEN
          ! try swapping second segment
          no_seg2=reg(ireg)%side(ind+1)%no_seg
          l3=reg(ireg)%side(ind+1)%seg(no_seg2)%point(3)
          k3=reg(ireg)%side(ind+1)%seg(no_seg2)%point(4)
          IF ((l1.NE.l3).OR.(k1.NE.k3)) THEN
            CALL halt('ERROR: region not closed',0)
          ELSE
            IF (no_seg2.EQ.1_ink) THEN
              reg(ireg)%side(ind+1)%seg(1)%point(1)=l3
              reg(ireg)%side(ind+1)%seg(1)%point(2)=k3
              reg(ireg)%side(ind+1)%seg(1)%point(3)=l2
              reg(ireg)%side(ind+1)%seg(1)%point(4)=k2
            ELSE
              DO ii=1,no_seg2
                swap=reg(ireg)%side(ind+1)%seg(ii)%point(1)
                reg(ireg)%side(ind+1)%seg(ii)%point(1)=                 &
&                reg(ireg)%side(ind+1)%seg(ii)%point(3)
                reg(ireg)%side(ind+1)%seg(ii)%point(3)=swap
                swap=reg(ireg)%side(ind+1)%seg(ii)%point(2)
                reg(ireg)%side(ind+1)%seg(ii)%point(2)=                 &
&                reg(ireg)%side(ind+1)%seg(ii)%point(4)
                reg(ireg)%side(ind+1)%seg(ii)%point(4)=swap
              ENDDO
            ENDIF
          ENDIF
        ENDIF
      ENDDO
      no_seg1=reg(ireg)%side(4)%no_seg
      l1=reg(ireg)%side(4)%seg(no_seg1)%point(3)
      k1=reg(ireg)%side(4)%seg(no_seg1)%point(4)
      l2=reg(ireg)%side(1)%seg(1)%point(1)
      k2=reg(ireg)%side(1)%seg(1)%point(2)
      IF ((l1.NE.l2).OR.(k1.NE.k2)) THEN
        CALL halt('ERROR: region not closed',0)
      ENDIF
    ENDDO

  END SUBROUTINE check_input

  SUBROUTINE reg_mesh(reg)

    USE integers_mod, ONLY: nreg,nel,nnod
    USE reals_mod,    ONLY: zerocut
    USE error_mod,    ONLY: halt

    ! Argument list
    TYPE(regions),DIMENSION(:),ALLOCATABLE,INTENT(INOUT) :: reg
    ! Local
    INTEGER(KIND=ink) :: no_l,no_k,ll,kk,lm,lp,km,kp,no_seg,no_it,      &
&                        max_no_it,ireg,ind,i1,i2,l1,l2,k1,k2
    REAL(KIND=rlk)    :: tl0,tl1,tk0,tk1,wl0,wl1,wk0,wk1,ww,fac,dr,ds,  &
&                        tol,om,r1,r2,r3,r4,r5,r6,r7,r8,s1,s2,s3,s4,s5, &
&                        s6,s7,s8,alph,beta,gamm,zeta,s_psi,s_phi,r_psi,&
&                        r_phi
    LOGICAL(KIND=lok) :: exit_status,ep

    DO ireg=1,nreg
      no_l=reg(ireg)%dim(1)+1_ink
      no_k=reg(ireg)%dim(2)+1_ink
      ALLOCATE(reg(ireg)%rr(1:no_l,1:no_k))
      ALLOCATE(reg(ireg)%ss(1:no_l,1:no_k))
      ALLOCATE(reg(ireg)%bc(1:no_l,1:no_k))
      reg(ireg)%rr(1:no_l,1:no_k)=0.0_rlk
      reg(ireg)%ss(1:no_l,1:no_k)=0.0_rlk
      reg(ireg)%bc(1:no_l,1:no_k)=0_ink
      ALLOCATE(reg(ireg)%merged(1:no_l,1:no_k))
      reg(ireg)%merged(1:no_l,1:no_k)=.FALSE._lok
      ! set weights
      reg(ireg)%r_wgt(1:8)=0.0_rlk
      reg(ireg)%s_wgt(1:8)=0.0_rlk
      SELECT CASE(reg(ireg)%typ)
        CASE(1_ink)
          reg(ireg)%r_wgt(2)=1.0_rlk
          reg(ireg)%r_wgt(7)=1.0_rlk
          reg(ireg)%s_wgt(2)=1.0_rlk
          reg(ireg)%s_wgt(7)=1.0_rlk
        CASE(2_ink)
          reg(ireg)%r_wgt(4)=1.0_rlk
          reg(ireg)%r_wgt(5)=1.0_rlk
          reg(ireg)%s_wgt(4)=1.0_rlk
          reg(ireg)%s_wgt(5)=1.0_rlk
        CASE(4_ink)
          reg(ireg)%r_wgt(1:8)=reg(ireg)%wgt(1:8)
          IF (ANY(reg(ireg)%wgt(9:16).NE.0.0_rlk)) THEN
            tl0=SUM(ABS(reg(ireg)%r_wgt(1:8)))
            IF (ABS(tl0).GT.zerocut) THEN
              reg(ireg)%s_wgt(1:8)=reg(ireg)%wgt(9:16)
            ENDIF
          ELSE
            reg(ireg)%s_wgt(1:8)=reg(ireg)%r_wgt(1:8)
          ENDIF
      END SELECT
      tl0=SUM(reg(ireg)%r_wgt(1:8))
      tl1=SUM(reg(ireg)%s_wgt(1:8))
      IF ((ABS(tl0).LT.zerocut).OR.(ABS(tl1).LT.zerocut)) THEN
        CALL halt('ERROR: ill defined user weights in reg_mesh',0)
      ELSE
        DO ind=1,8
          reg(ireg)%r_wgt(ind)=reg(ireg)%r_wgt(ind)/tl0
          reg(ireg)%s_wgt(ind)=reg(ireg)%s_wgt(ind)/tl1
        ENDDO
      ENDIF
      ! set equipotential flag
      ep=.FALSE._lok
      IF (reg(ireg)%typ.EQ.3_ink) ep=.TRUE._lok
      ! calculate points on region boundary
      DO ind=1,4
        no_seg=reg(ireg)%side(ind)%no_seg
        SELECT CASE(ind)
          CASE(1)
            l1=1_ink
            k1=1_ink
            l2=1_ink
            k2=no_k
          CASE(2)
            l1=1_ink
            k1=no_k
            l2=no_l
            k2=no_k
          CASE(3)
            l1=no_l
            k1=no_k
            l2=no_l
            k2=1_ink
          CASE(4)
            l1=no_l
            k1=1_ink
            l2=1_ink
            k2=1_ink
        END SELECT
        IF (no_seg.EQ.1_ink) THEN
          SELECT CASE(reg(ireg)%side(ind)%seg(1)%seg_typ)
            CASE(1_ink,5_ink)
              CALL line(ireg=ireg,i_seg=1_ink,ind=ind,l1=l1,l2=l2,k1=k1,&
&                       k2=k2,reg=reg)
            CASE(2_ink,3_ink)
              CALL arc(ireg=ireg,i_seg=1_ink,ind=ind,l1=l1,l2=l2,k1=k1, &
&                      k2=k2,reg=reg)
            CASE(4_ink)
              CALL halt('ERROR: side_type is POINT but DJN off',0)
          END SELECT
        ELSE
        ENDIF
      ENDDO
      ! calculate initial guess at interior points
      l1=2_ink
      k1=2_ink
      l2=no_l-1_ink
      k2=no_k-1_ink
      tl0=0.25_rlk
      tl1=0.25_rlk
      tk0=0.25_rlk
      tk1=0.25_rlk
      fac=1.0_rlk
      IF (.not.ep) THEN
        tl0=reg(ireg)%r_wgt(7)
        tl1=reg(ireg)%r_wgt(2)
        tk0=reg(ireg)%r_wgt(4)
        tk1=reg(ireg)%r_wgt(5)
        fac=tl0+tl1+tk0+tk1
        IF (ABS(fac).LE.zerocut) fac=1.0_rlk
      ENDIF
      tl0=2.0_rlk*tl0/fac
      tl1=2.0_rlk*tl1/fac
      tk0=2.0_rlk*tk0/fac
      tk1=2.0_rlk*tk1/fac
      DO kk=k1,k2
        DO ll=l1,l2
          ww=REAL(l2-l1,rlk)+2.0_rlk
          wl0=tl0*(REAL(l2-ll,rlk)+1.0_rlk)/ww
          wl1=tl1*(REAL(ll-l1,rlk)+1.0_rlk)/ww
          ww=REAL(k2-k1,rlk)+2.0_rlk
          wk0=tk0*(REAL(k2-kk,rlk)+1.0_rlk)/ww
          wk1=tk1*(REAL(kk-k1,rlk)+1.0_rlk)/ww
          reg(ireg)%rr(ll,kk)=wk0*reg(ireg)%rr(ll,k1-1)+                &
&                             wk1*reg(ireg)%rr(ll,k2+1)+                &
&                             wl0*reg(ireg)%rr(l1-1,kk)+                &
&                             wl1*reg(ireg)%rr(l2+1,kk)
          reg(ireg)%ss(ll,kk)=wk0*reg(ireg)%ss(ll,k1-1)+                &
&                             wk1*reg(ireg)%ss(ll,k2+1)+                &
&                             wl0*reg(ireg)%ss(l1-1,kk)+                &
&                             wl1*reg(ireg)%ss(l2+1,kk)
        ENDDO
      ENDDO
      ! calculate interior points
      no_it=0_ink
      tol=reg(ireg)%tol
      om=reg(ireg)%om
      max_no_it=8_ink*no_l*no_k
      IF (ep) THEN
        ! to ensure r1-8,s1-8 initialised, not strictly necessary but keeps
        ! gcc based compilers happy.
        r1=0.0_rlk
        r2=0.0_rlk
        r3=0.0_rlk
        r4=0.0_rlk
        r5=0.0_rlk
        r6=0.0_rlk
        r7=0.0_rlk
        r8=0.0_rlk
        s1=0.0_rlk
        s2=0.0_rlk
        s3=0.0_rlk
        s4=0.0_rlk
        s5=0.0_rlk
        s6=0.0_rlk
        s7=0.0_rlk
        s8=0.0_rlk
      ELSE
        r1=reg(ireg)%r_wgt(1)
        r2=reg(ireg)%r_wgt(2)
        r3=reg(ireg)%r_wgt(3)
        r4=reg(ireg)%r_wgt(4)
        r5=reg(ireg)%r_wgt(5)
        r6=reg(ireg)%r_wgt(6)
        r7=reg(ireg)%r_wgt(7)
        r8=reg(ireg)%r_wgt(8)
        s1=reg(ireg)%s_wgt(1)
        s2=reg(ireg)%s_wgt(2)
        s3=reg(ireg)%s_wgt(3)
        s4=reg(ireg)%s_wgt(4)
        s5=reg(ireg)%s_wgt(5)
        s6=reg(ireg)%s_wgt(6)
        s7=reg(ireg)%s_wgt(7)
        s8=reg(ireg)%s_wgt(8)
      ENDIF
      mesh_loop:DO
        no_it=no_it+1_ink
        exit_status=.TRUE._lok
        DO kk=k1,k2
          DO ll=l1,l2
            lm=ll-1_ink
            lp=ll+1_ink
            km=kk-1_ink
            kp=kk+1_ink
            IF (ep) THEN
              r_psi=0.5_rlk*(reg(ireg)%rr(ll,kp)-reg(ireg)%rr(ll,km))
              s_psi=0.5_rlk*(reg(ireg)%ss(ll,kp)-reg(ireg)%ss(ll,km))
              r_phi=0.5_rlk*(reg(ireg)%rr(lp,kk)-reg(ireg)%rr(lm,kk))
              s_phi=0.5_rlk*(reg(ireg)%ss(lp,kk)-reg(ireg)%ss(lm,kk))
              zeta=s_psi*r_phi-s_phi*r_psi
              IF (ABS(zeta).LT.zerocut) CYCLE
              alph=s_psi*s_psi+r_psi*r_psi
              beta=s_phi*s_psi+r_phi*r_psi
              gamm=s_phi*s_phi+r_phi*r_phi
              r1=0.5_rlk*beta
              r2=alph
              r3=-r1
              r4=gamm
              r5=r4
              r6=r3
              r7=r2
              r8=r1
              fac=r1+r2+r3+r4+r5+r6+r7+r8
              IF (fac.LT.1.0e-24_rlk) THEN
                fac=fac*1.0e14_rlk
                r1=r1*1.0e14_rlk
                r2=r2*1.0e14_rlk
                r3=r3*1.0e14_rlk
                r4=r4*1.0e14_rlk
                r5=r5*1.0e14_rlk
                r6=r6*1.0e14_rlk
                r7=r7*1.0e14_rlk
                r8=r8*1.0e14_rlk
              ENDIF
              r1=r1/fac
              r2=r2/fac
              r3=r3/fac
              r4=r4/fac
              r5=r5/fac
              r6=r6/fac
              r7=r7/fac
              r8=r8/fac
              s1=r1
              s2=r2
              s3=r3
              s4=r4
              s5=r5
              s6=r6
              s7=r7
              s8=r8
            ENDIF
            dr=r1*reg(ireg)%rr(lp,km)+r2*reg(ireg)%rr(lp,kk)+           &
&              r3*reg(ireg)%rr(lp,kp)+r4*reg(ireg)%rr(ll,km)+           &
&              r5*reg(ireg)%rr(ll,kp)+r6*reg(ireg)%rr(lm,km)+           &
&              r7*reg(ireg)%rr(lm,kk)+r8*reg(ireg)%rr(lm,kp)-           &
&              reg(ireg)%rr(ll,kk)
            ds=s1*reg(ireg)%ss(lp,km)+s2*reg(ireg)%ss(lp,kk)+           &
&              s3*reg(ireg)%ss(lp,kp)+s4*reg(ireg)%ss(ll,km)+           &
&              s5*reg(ireg)%ss(ll,kp)+s6*reg(ireg)%ss(lm,km)+           &
&              s7*reg(ireg)%ss(lm,kk)+s8*reg(ireg)%ss(lm,kp)-           &
&              reg(ireg)%ss(ll,kk)
            IF ((ABS(dr).GT.tol).OR.(ABS(ds).GT.tol)) THEN
              exit_status=.FALSE._lok
              reg(ireg)%rr(ll,kk)=reg(ireg)%rr(ll,kk)+om*dr
              reg(ireg)%ss(ll,kk)=reg(ireg)%ss(ll,kk)+om*ds
            ENDIF
          ENDDO
        ENDDO
        IF (no_it.GT.max_no_it) THEN
          WRITE(6,'(a28,a54,i3)') ' WARNING: maximum number of ',       &
&          'iterations exceeded whilst generating mesh for region ',ireg
          EXIT mesh_loop
        ENDIF
        IF (exit_status) THEN
          reg(ireg)%no_it=no_it
          EXIT mesh_loop
        ENDIF
      ENDDO mesh_loop
    ENDDO

    ! calculate sizes
    nel=0_ink
    nnod=0_ink
    DO ireg=1,nreg
      nel=nel+reg(ireg)%dim(1)*reg(ireg)%dim(2)
      ! set corner BC
      IF (.NOT.reg(ireg)%merged(1,1)) THEN
        IF (((reg(ireg)%bc(2,1).EQ.1_ink).AND.                          &
&            (reg(ireg)%bc(1,2).EQ.2_ink)).OR.                          &
&           ((reg(ireg)%bc(2,1).EQ.2_ink).AND.                          &
&            (reg(ireg)%bc(1,2).EQ.1_ink))) THEN
          reg(ireg)%bc(1,1)=3_ink
        ENDIF
        IF (((reg(ireg)%bc(2,1).EQ.6_ink).AND.                          &
&            (reg(ireg)%bc(1,2).EQ.1_ink)).OR.                          &
&           ((reg(ireg)%bc(2,1).EQ.1_ink).AND.                          &
&            (reg(ireg)%bc(1,2).EQ.6_ink))) THEN
          reg(ireg)%bc(1,1)=4_ink
        ENDIF
        IF (((reg(ireg)%bc(2,1).EQ.6_ink).AND.                          &
&            (reg(ireg)%bc(1,2).EQ.2_ink)).OR.                          &
&           ((reg(ireg)%bc(2,1).EQ.2_ink).AND.                          &
&            (reg(ireg)%bc(1,2).EQ.6_ink))) THEN
          reg(ireg)%bc(1,1)=5_ink
        ENDIF
        IF (((reg(ireg)%bc(2,1).EQ.8_ink).AND.                          &
&            (reg(ireg)%bc(1,2).EQ.1_ink)).OR.                          &
&           ((reg(ireg)%bc(2,1).EQ.1_ink).AND.                          &
&            (reg(ireg)%bc(1,2).EQ.8_ink))) THEN
          reg(ireg)%bc(1,1)=1_ink
        ENDIF
        IF (((reg(ireg)%bc(2,1).EQ.8_ink).AND.                          &
&            (reg(ireg)%bc(1,2).EQ.2_ink)).OR.                          &
&           ((reg(ireg)%bc(2,1).EQ.2_ink).AND.                          &
&            (reg(ireg)%bc(1,2).EQ.8_ink))) THEN
          reg(ireg)%bc(1,1)=2_ink
        ENDIF
        IF (((reg(ireg)%bc(2,1).EQ.8_ink).AND.                          &
&            (reg(ireg)%bc(1,2).EQ.3_ink)).OR.                          &
&           ((reg(ireg)%bc(2,1).EQ.3_ink).AND.                          &
&            (reg(ireg)%bc(1,2).EQ.8_ink))) THEN
          reg(ireg)%bc(1,1)=3_ink
        ENDIF
        IF (((reg(ireg)%bc(2,1).EQ.6_ink).AND.                          &
&            (reg(ireg)%bc(1,2).EQ.3_ink)).OR.                          &
&           ((reg(ireg)%bc(2,1).EQ.3_ink).AND.                          &
&            (reg(ireg)%bc(1,2).EQ.6_ink))) THEN
          CALL halt('ERROR: inconsistent BC at corner',0)
        ENDIF
      ENDIF
      i1=reg(ireg)%dim(2)
      i2=i1+1_ink
      IF (.NOT.reg(ireg)%merged(1,i2)) THEN
        IF (((reg(ireg)%bc(2,i2).EQ.1_ink).AND.                         &
&            (reg(ireg)%bc(1,i1).EQ.2_ink)).OR.                         &
&           ((reg(ireg)%bc(2,i2).EQ.2_ink).AND.                         &
&            (reg(ireg)%bc(1,i1).EQ.1_ink))) THEN
          reg(ireg)%bc(1,i2)=3_ink
        ENDIF
        IF (((reg(ireg)%bc(2,i2).EQ.6_ink).AND.                         &
&            (reg(ireg)%bc(1,i1).EQ.1_ink)).OR.                         &
&           ((reg(ireg)%bc(2,i2).EQ.1_ink).AND.                         &
&            (reg(ireg)%bc(1,i1).EQ.6_ink))) THEN
          reg(ireg)%bc(1,i2)=4_ink
        ENDIF
        IF (((reg(ireg)%bc(2,i2).EQ.6_ink).AND.                         &
&            (reg(ireg)%bc(1,i1).EQ.2_ink)).OR.                         &
&           ((reg(ireg)%bc(2,i2).EQ.2_ink).AND.                         &
&            (reg(ireg)%bc(1,i1).EQ.6_ink))) THEN
          reg(ireg)%bc(1,i2)=5_ink
        ENDIF
        IF (((reg(ireg)%bc(2,i2).EQ.8_ink).AND.                         &
&            (reg(ireg)%bc(1,i1).EQ.1_ink)).OR.                         &
&           ((reg(ireg)%bc(2,i2).EQ.1_ink).AND.                         &
&            (reg(ireg)%bc(1,i1).EQ.8_ink))) THEN
          reg(ireg)%bc(1,i2)=1_ink
        ENDIF
        IF (((reg(ireg)%bc(2,i2).EQ.8_ink).AND.                         &
&            (reg(ireg)%bc(1,i1).EQ.2_ink)).OR.                         &
&           ((reg(ireg)%bc(2,i2).EQ.2_ink).AND.                         &
&            (reg(ireg)%bc(1,i1).EQ.8_ink))) THEN
          reg(ireg)%bc(1,i2)=2_ink
        ENDIF
        IF (((reg(ireg)%bc(2,i2).EQ.8_ink).AND.                         &
&            (reg(ireg)%bc(1,i1).EQ.3_ink)).OR.                         &
&           ((reg(ireg)%bc(2,i2).EQ.3_ink).AND.                         &
&            (reg(ireg)%bc(1,i1).EQ.8_ink))) THEN
          reg(ireg)%bc(1,i2)=3_ink
        ENDIF
        IF (((reg(ireg)%bc(2,i2).EQ.6_ink).AND.                         &
&            (reg(ireg)%bc(1,i1).EQ.3_ink)).OR.                         &
&           ((reg(ireg)%bc(2,i2).EQ.3_ink).AND.                         &
&            (reg(ireg)%bc(1,i1).EQ.6_ink))) THEN
          CALL halt('ERROR: inconsistent BC at corner',0)
        ENDIF
      ENDIF
      i1=reg(ireg)%dim(1)
      i2=i1+1_ink
      IF (.NOT.reg(ireg)%merged(i2,1)) THEN
        IF (((reg(ireg)%bc(i1,1).EQ.1_ink).AND.                         &
&            (reg(ireg)%bc(i2,2).EQ.2_ink)).OR.                         &
&           ((reg(ireg)%bc(i1,1).EQ.2_ink).AND.                         &
&            (reg(ireg)%bc(i2,2).EQ.1_ink))) THEN
          reg(ireg)%bc(i2,1)=3_ink
        ENDIF
        IF (((reg(ireg)%bc(i1,1).EQ.6_ink).AND.                         &
&            (reg(ireg)%bc(i2,2).EQ.1_ink)).OR.                         &
&           ((reg(ireg)%bc(i1,1).EQ.1_ink).AND.                         &
&            (reg(ireg)%bc(i2,2).EQ.6_ink))) THEN
          reg(ireg)%bc(i2,1)=4_ink
        ENDIF
        IF (((reg(ireg)%bc(i1,1).EQ.6_ink).AND.                         &
&            (reg(ireg)%bc(i2,2).EQ.2_ink)).OR.                         &
&           ((reg(ireg)%bc(i1,1).EQ.2_ink).AND.                         &
&            (reg(ireg)%bc(i2,2).EQ.6_ink))) THEN
          reg(ireg)%bc(i2,1)=5_ink
        ENDIF
        IF (((reg(ireg)%bc(i1,1).EQ.8_ink).AND.                         &
&            (reg(ireg)%bc(i2,2).EQ.1_ink)).OR.                         &
&           ((reg(ireg)%bc(i1,1).EQ.1_ink).AND.                         &
&            (reg(ireg)%bc(i2,2).EQ.8_ink))) THEN
          reg(ireg)%bc(i2,1)=1_ink
        ENDIF
        IF (((reg(ireg)%bc(i1,1).EQ.8_ink).AND.                         &
&            (reg(ireg)%bc(i2,2).EQ.2_ink)).OR.                         &
&           ((reg(ireg)%bc(i1,1).EQ.2_ink).AND.                         &
&            (reg(ireg)%bc(i2,2).EQ.8_ink))) THEN
          reg(ireg)%bc(i2,1)=2_ink
        ENDIF
        IF (((reg(ireg)%bc(i1,1).EQ.8_ink).AND.                         &
&            (reg(ireg)%bc(i2,2).EQ.3_ink)).OR.                         &
&           ((reg(ireg)%bc(i1,1).EQ.3_ink).AND.                         &
&            (reg(ireg)%bc(i2,2).EQ.8_ink))) THEN
          reg(ireg)%bc(i2,1)=3_ink
        ENDIF
        IF (((reg(ireg)%bc(i1,1).EQ.6_ink).AND.                         &
&            (reg(ireg)%bc(i2,2).EQ.3_ink)).OR.                         &
&           ((reg(ireg)%bc(i1,1).EQ.3_ink).AND.                         &
&            (reg(ireg)%bc(i2,2).EQ.6_ink))) THEN
          CALL halt('ERROR: inconsistent BC at corner',0)
        ENDIF
      ENDIF
      i1=reg(ireg)%dim(1)+1_ink
      i2=reg(ireg)%dim(2)+1_ink
      IF (.NOT.reg(ireg)%merged(i1,i2)) THEN
        IF (((reg(ireg)%bc(i1-1,i2).EQ.1_ink).AND.                      &
&            (reg(ireg)%bc(i1,i2-1).EQ.2_ink)).OR.                      &
&           ((reg(ireg)%bc(i1-1,i2).EQ.2_ink).AND.                      &
&            (reg(ireg)%bc(i1,i2-1).EQ.1_ink))) THEN
          reg(ireg)%bc(i1,i2)=3_ink
        ENDIF
        IF (((reg(ireg)%bc(i1-1,i2).EQ.6_ink).AND.                      &
&            (reg(ireg)%bc(i1,i2-1).EQ.1_ink)).OR.                      &
&           ((reg(ireg)%bc(i1-1,i2).EQ.1_ink).AND.                      &
&            (reg(ireg)%bc(i1,i2-1).EQ.6_ink))) THEN
          reg(ireg)%bc(i1,i2)=4_ink
        ENDIF
        IF (((reg(ireg)%bc(i1-1,i2).EQ.6_ink).AND.                      &
&            (reg(ireg)%bc(i1,i2-1).EQ.2_ink)).OR.                      &
&           ((reg(ireg)%bc(i1-1,i2).EQ.2_ink).AND.                      &
&            (reg(ireg)%bc(i1,i2-1).EQ.6_ink))) THEN
          reg(ireg)%bc(i1,i2)=5_ink
        ENDIF
        IF (((reg(ireg)%bc(i1-1,i2).EQ.8_ink).AND.                      &
&            (reg(ireg)%bc(i1,i2-1).EQ.1_ink)).OR.                      &
&           ((reg(ireg)%bc(i1-1,i2).EQ.1_ink).AND.                      &
&            (reg(ireg)%bc(i1,i2-1).EQ.8_ink))) THEN
          reg(ireg)%bc(i1,i2)=1_ink
        ENDIF
        IF (((reg(ireg)%bc(i1-1,i2).EQ.8_ink).AND.                      &
&            (reg(ireg)%bc(i1,i2-1).EQ.2_ink)).OR.                      &
&           ((reg(ireg)%bc(i1-1,i2).EQ.2_ink).AND.                      &
&            (reg(ireg)%bc(i1,i2-1).EQ.8_ink))) THEN
          reg(ireg)%bc(i1,i2)=2_ink
        ENDIF
        IF (((reg(ireg)%bc(i1-1,i2).EQ.8_ink).AND.                      &
&            (reg(ireg)%bc(i1,i2-1).EQ.3_ink)).OR.                      &
&           ((reg(ireg)%bc(i1-1,i2).EQ.3_ink).AND.                      &
&            (reg(ireg)%bc(i1,i2-1).EQ.8_ink))) THEN
          reg(ireg)%bc(i1,i2)=3_ink
        ENDIF
        IF (((reg(ireg)%bc(i1-1,i2).EQ.6_ink).AND.                      &
&            (reg(ireg)%bc(i1,i2-1).EQ.3_ink)).OR.                      &
&           ((reg(ireg)%bc(i1-1,i2).EQ.3_ink).AND.                      &
&            (reg(ireg)%bc(i1,i2-1).EQ.6_ink))) THEN
          CALL halt('ERROR: inconsistent BC at corner',0)
        ENDIF
      ENDIF
      DO ll=1,reg(ireg)%dim(1)+1_ink
        DO kk=1,reg(ireg)%dim(2)+1_ink
          IF (.NOT.reg(ireg)%merged(ll,kk)) THEN
            nnod=nnod+1_ink
          ENDIF
        ENDDO
      ENDDO
    ENDDO

  END SUBROUTINE reg_mesh

  SUBROUTINE mesh_transfer(reg)

    USE integers_mod,ONLY: nreg,nel,nnod
    USE pointers_mod,ONLY: ndx,ndy,ielnod,ielreg,indtype,ielmat,ndu,ndv
    USE error_mod,   ONLY: halt

    ! Argument list
    TYPE(regions),DIMENSION(:),ALLOCATABLE,INTENT(INOUT) :: reg
    ! Local
    INTEGER(KIND=ink)                 :: ll,kk,nod_count,ele_count,i1,  &
&                                        i2,ireg,iele,inod,ii,l1,l2,k1, &
&                                        k2
    INTEGER(KIND=ink),DIMENSION(nnod) :: istore
    REAL(KIND=rlk)                    :: r1,r2,r3,s1,s2,s3,x1,y1,w1

    nod_count=0_ink
    ele_count=0_ink
    DO ireg=1,nreg
      l1=1_ink
      l2=reg(ireg)%dim(1)+1_ink
      k1=1_ink
      k2=reg(ireg)%dim(2)+1_ink
      ! coords + node type
      DO kk=k1,k2
        DO ll=l1,l2
          IF (ll.EQ.l1) THEN
            IF (reg(ireg)%merged(ll,kk)) THEN
            ELSE
              nod_count=nod_count+1_ink
              ndx(nod_count)=reg(ireg)%ss(ll,kk)
              ndy(nod_count)=reg(ireg)%rr(ll,kk)
              i1=reg(ireg)%bc(ll,kk)
              IF (i1.GT.0_ink) THEN
                indtype(nod_count)=-i1
                istore(nod_count)=ireg
              ELSE
                CALL halt('ERROR: undefined BC at region edge',0)
              ENDIF
            ENDIF
          ELSEIF (ll.EQ.l2) THEN
            IF (reg(ireg)%merged(ll,kk)) THEN
            ELSE
              nod_count=nod_count+1_ink
              ndx(nod_count)=reg(ireg)%ss(ll,kk)
              ndy(nod_count)=reg(ireg)%rr(ll,kk)
              i1=reg(ireg)%bc(ll,kk)
              IF (i1.GT.0_ink) THEN
                indtype(nod_count)=-i1
                istore(nod_count)=ireg
              ELSE
                CALL halt('ERROR: undefined BC at region edge',0)
              ENDIF
            ENDIF
          ELSEIF (kk.EQ.k1) THEN
            IF (reg(ireg)%merged(ll,kk)) THEN
            ELSE
              nod_count=nod_count+1_ink
              ndx(nod_count)=reg(ireg)%ss(ll,kk)
              ndy(nod_count)=reg(ireg)%rr(ll,kk)
              i1=reg(ireg)%bc(ll,kk)
              IF (i1.GT.0_ink) THEN
                indtype(nod_count)=-i1
                istore(nod_count)=ireg
              ELSE
                CALL halt('ERROR: undefined BC at region edge',0)
              ENDIF
            ENDIF
          ELSEIF (kk.EQ.k2) THEN
            IF (reg(ireg)%merged(ll,kk)) THEN
            ELSE
              nod_count=nod_count+1_ink
              ndx(nod_count)=reg(ireg)%ss(ll,kk)
              ndy(nod_count)=reg(ireg)%rr(ll,kk)
              i1=reg(ireg)%bc(ll,kk)
              IF (i1.GT.0_ink) THEN
                indtype(nod_count)=-i1
                istore(nod_count)=ireg
              ELSE
                CALL halt('ERROR: undefined BC at region edge',0)
              ENDIF
            ENDIF
          ELSE 
            nod_count=nod_count+1_ink
            ndx(nod_count)=reg(ireg)%ss(ll,kk)
            ndy(nod_count)=reg(ireg)%rr(ll,kk)
            indtype(nod_count)=ireg
          ENDIF
        ENDDO
      ENDDO  

      ! connectivity
      r1=reg(ireg)%rr(2,1)-reg(ireg)%rr(1,1)
      r2=reg(ireg)%rr(1,2)-reg(ireg)%rr(1,1)
      r3=reg(ireg)%rr(2,2)-reg(ireg)%rr(1,1)
      s1=reg(ireg)%ss(2,1)-reg(ireg)%ss(1,1)
      s2=reg(ireg)%ss(1,2)-reg(ireg)%ss(1,1)
      s3=reg(ireg)%ss(2,2)-reg(ireg)%ss(1,1)
      IF (((r1*s2-r2*s1).GT.0.0_rlk).OR.((r1*s3-r3*s1).GT.0.0_rlk).OR.  &
&         ((r3*s2-r2*s3).GT.0.0_rlk)) THEN
        i1=4_ink
        i2=2_ink
      ELSE
        i1=2_ink
        i2=4_ink
      ENDIF
      DO kk=k1,k2-1
        DO ll=l1,l2-1
          ele_count=ele_count+1_ink
          ielnod(1,ele_count)= ll       +(kk-1_ink)*l2
          ielnod(3,ele_count)=(ll+1_ink)+ kk       *l2
          ielnod(i1,ele_count)=ielnod(1,ele_count)+1_ink
          ielnod(i2,ele_count)=ielnod(3,ele_count)-1_ink
          ielreg(ele_count)=ireg
        ENDDO 
      ENDDO
      ! destroy most of reg
      DEALLOCATE(reg(ireg)%rr)
      DEALLOCATE(reg(ireg)%ss)
      DEALLOCATE(reg(ireg)%merged)
      DEALLOCATE(reg(ireg)%bc)
    ENDDO

    ! material no.
    DO iele=1,nel
      ireg=ielreg(iele)
      ielmat(iele)=reg(ireg)%mat
    ENDDO

    ! velocities
    DO inod=1,nnod
      ireg=indtype(inod)
      IF (ireg.GT.0_ink) THEN      ! internal node
        SELECT CASE(reg(ireg)%vel_typ)
          CASE(0_ink) ! default
            ndu(inod)=0.0_rlk
            ndv(inod)=0.0_rlk
          CASE(1_ink) ! cartesian
            ndu(inod)=reg(ireg)%vel(1)
            ndv(inod)=reg(ireg)%vel(2)
          CASE(2_ink) ! radial
            x1=ndx(inod)
            y1=ndy(inod)
            w1=SQRT((x1-reg(ireg)%vel(2))**2_ink+                       &
&                   (y1-reg(ireg)%vel(3))**2_ink)
            ndu(inod)=reg(ireg)%vel(1)*x1/w1
            ndv(inod)=reg(ireg)%vel(1)*y1/w1
        END SELECT
      ELSE                         ! region boundary
        SELECT CASE(ireg)
          CASE(-1_ink) ! u FIXED, v FREE
            ndu(inod)=0.0_rlk
            i1=istore(inod)
            SELECT CASE(reg(i1)%vel_typ)
              CASE(0_ink) ! default
                ndv(inod)=0.0_rlk
              CASE(1_ink) ! cartesian
                ndv(inod)=reg(i1)%vel(2)
              CASE(2_ink) ! radial
                y1=ndy(inod)
                w1=SQRT((ndx(inod)-reg(i1)%vel(2))**2_ink+              &
&                       (y1       -reg(i1)%vel(3))**2_ink) 
                ndv(inod)=reg(i1)%vel(1)*y1/w1
            END SELECT
          CASE(-2_ink) ! u FREE, v FIXED
            ndv(inod)=0.0_rlk
            i1=istore(inod)
            SELECT CASE(reg(i1)%vel_typ)
              CASE(0_ink) ! default
                ndu(inod)=0.0_rlk
              CASE(1_ink) ! cartesian
                ndu(inod)=reg(i1)%vel(1)
              CASE(2_ink) ! radial
                x1=ndx(inod)
                w1=SQRT((x1       -reg(i1)%vel(2))**2_ink+              &
&                       (ndy(inod)-reg(i1)%vel(3))**2_ink)
                ndu(inod)=reg(i1)%vel(1)*x1/w1
            END SELECT
          CASE(-3_ink) ! WALL
            ndu(inod)=0.0_rlk
            ndv(inod)=0.0_rlk
          CASE DEFAULT
            ! reset 
            i1=istore(inod)
            indtype(inod)=i1
            SELECT CASE(reg(i1)%vel_typ)
              CASE(0_ink) ! default
                ndu(inod)=0.0_rlk
                ndv(inod)=0.0_rlk
              CASE(1_ink) ! cartesian
                ndu(inod)=reg(i1)%vel(1)
                ndv(inod)=reg(i1)%vel(2)
              CASE(2_ink) ! radial
                x1=ndx(inod)
                y1=ndy(inod)
                w1=SQRT((x1-reg(i1)%vel(2))**2_ink+                     &
&                       (y1-reg(i1)%vel(3))**2_ink)
                ndu(inod)=reg(i1)%vel(1)*x1/w1
                ndv(inod)=reg(i1)%vel(1)*y1/w1
            END SELECT
        END SELECT
      ENDIF
    ENDDO

    ! destroy rest of reg
    DO ii=1,nreg
      DO i1=1,4
        DEALLOCATE(reg(ii)%side(i1)%seg)
      ENDDO
    ENDDO
    DEALLOCATE(reg)

  END SUBROUTINE mesh_transfer

  SUBROUTINE mesh_print(reg)

    USE kinds_mod,   ONLY: ink
    USE integers_mod,ONLY: nreg,nmat,nel,nnod,max_seg,max_subseg

    ! Argument list
    TYPE(regions),DIMENSION(:),INTENT(IN) :: reg
    ! Local
    INTEGER(KIND=ink) :: ii,is,it

    WRITE(6,'(a63,i10)') '  Number of regions:                         '&
&    //'             nreg ',nreg
    WRITE(6,'(a63,i10)') '  Number of materials:                       '&
&    //'             nmat ',nmat
    WRITE(6,'(a63,i10)') '  Total number of elements:                  '&
&    //'                  ',nel
    WRITE(6,'(a63,i10)') '  Total number of nodes:                     '&
&    //'                  ',nnod
    WRITE(6,'(a63,i10)') '  Maximum number of segments:                '&
&    //'          max_seg ',max_seg
    WRITE(6,'(a63,i10)') '  Maximum number of sub-segments:            '&
&    //'       max_subseg ',max_subseg
    DO ii=1,nreg
      WRITE(6,'(a10,i3)') '  Region: ',ii
      WRITE(6,'(a63,i10)') '   Material number:                        '&
&      //'    region_material ',reg(ii)%mat
      SELECT CASE(reg(ii)%typ)
        CASE(1_ink)
          WRITE(6,'(a63,a10)') '   Region type:                        '&
&          //'             region_typ ','      LIN1'
        CASE(2_ink)
          WRITE(6,'(a63,a10)') '   Region type:                        '&
&          //'             region_typ ','      LIN2'
        CASE(3_ink)
          WRITE(6,'(a63,a10)') '   Region type:                        '&
&          //'             region_typ ','      EQUI'
        CASE(4_ink)
          WRITE(6,'(a63,a10)') '   Region type:                        '&
&          //'             region_typ ','      USER'
      END SELECT
      WRITE(6,'(a63,1X,i4,a1,i4)') '   Region dimension:               '&
&      //'                 region_dim ',reg(ii)%dim(2),'x',             &
&      reg(ii)%dim(1)
      WRITE(6,'(a63,e10.4)') '   Convergence tolerance:                '&
&      //'           region_tol ',reg(ii)%tol
      WRITE(6,'(a63,f10.6)') '   Convergence scaling factor:           '&
&      //'            region_om ',reg(ii)%om
      WRITE(6,'(a63,i10)')   '   Number of iterations:                 '&
&      //'                      ',reg(ii)%no_it
      SELECT CASE(reg(ii)%vel_typ)
        CASE(1_ink)
          WRITE(6,'(a63,i10)') '   Cartesian region velocity:          '&
&          //'         region_vel_typ ',reg(ii)%vel_typ
          WRITE(6,'(a63,e10.4)') '   X velocity:                       '&
&          //'            region_vel(1) ',reg(ii)%vel(1)
          WRITE(6,'(a63,e10.4)') '   Y velocity:                       '&
&          //'            region_vel(2) ',reg(ii)%vel(2)
        CASE(2_ink)
          WRITE(6,'(a63,i10)') '   Radial region velocity:             '&
&          //'         region_vel_typ ',reg(ii)%vel_typ
          WRITE(6,'(a63,e10.4)') '   Velocity:                         '&
&          //'            region_vel(1) ',reg(ii)%vel(1)
          WRITE(6,'(a63,e10.4)') '   X centre:                         '&
&          //'            region_vel(2) ',reg(ii)%vel(2)
          WRITE(6,'(a63,e10.4)') '   Y centre:                         '&
&          //'            region_vel(3) ',reg(ii)%vel(3) 
      END SELECT
      DO is=1,4
        WRITE(6,'(a9,i1,a53,i10)') '   Side: ',is,' number of segments:'&
&        //'                                 ',reg(ii)%side(is)%no_seg
        DO it=1,reg(ii)%side(is)%no_seg
          SELECT CASE(reg(ii)%side(is)%seg(it)%seg_typ)
            CASE(1_ink)
              WRITE(6,'(a20,i3,a40,a10)') '    Segment number: ',       &
&              reg(ii)%side(is)%seg(it)%seg_no,' segment type:         '&
&               //'                ','      LINE'
              WRITE(6,'(a11,e11.5,a1,e11.5,a6,e11.5,a1,e11.5,a1)')      &
&              '    From  (',reg(ii)%side(is)%seg(it)%point(1),',',     &
&              reg(ii)%side(is)%seg(it)%point(2),') to (',              &
&              reg(ii)%side(is)%seg(it)%point(3),',',                   &
&              reg(ii)%side(is)%seg(it)%point(4),')'
            CASE(2_ink)
              WRITE(6,'(a20,i3,a40,a10)') '    Segment number: ',       &
&              reg(ii)%side(is)%seg(it)%seg_no,' segment type:         '&
&               //'                ','     ARC_C'
              WRITE(6,'(a11,e11.5,a1,e11.5,a6,e11.5,a1,e11.5,a1)')      &
&              '    From  (',                                           &
&              reg(ii)%side(is)%seg(it)%point(1),',',                   &
&              reg(ii)%side(is)%seg(it)%point(2),') to (',              &
&              reg(ii)%side(is)%seg(it)%point(3),',',                   &
&              reg(ii)%side(is)%seg(it)%point(4),')'
              WRITE(6,'(a11,e11.5,a1,e11.5,a1)') '    About (',         &
&              reg(ii)%side(is)%seg(it)%point(5),',',                   &
&              reg(ii)%side(is)%seg(it)%point(6),')'
            CASE(3_ink)
              WRITE(6,'(a20,i3,a40,a10)') '    Segment number: ',       &
&              reg(ii)%side(is)%seg(it)%seg_no,' segment type:         '&
&               //'                ','     ARC_A'
              WRITE(6,'(a11,e11.5,a1,e11.5,a6,e11.5,a1,e11.5,a1)')      &
&              '    From  (',                                           &
&              reg(ii)%side(is)%seg(it)%point(1),',',                   &
&              reg(ii)%side(is)%seg(it)%point(2),') to (',              &
&              reg(ii)%side(is)%seg(it)%point(3),',',                   &
&              reg(ii)%side(is)%seg(it)%point(4),')'
              WRITE(6,'(a11,e11.5,a1,e11.5,a1)') '    About (',         &
&              reg(ii)%side(is)%seg(it)%point(5),',',                   &
&              reg(ii)%side(is)%seg(it)%point(6),')'
            CASE(4_ink)
              WRITE(6,'(a20,i3,a40,a10)') '    Segment number: ',       &
&              reg(ii)%side(is)%seg(it)%seg_no,' segment type:         '&
&               //'                ','     POINT'
              WRITE(6,'(a11,e11.5,a1,e11.5,a1)') '    At    (',         &
&              reg(ii)%side(is)%seg(it)%point(1),',',                   &
&              reg(ii)%side(is)%seg(it)%point(2),')'
            CASE(5_ink)
              WRITE(6,'(a20,i3,a40,a10)') '    Segment number: ',       &
&              reg(ii)%side(is)%seg(it)%seg_no,' segment type:         '&
&               //'                ','      LINK'
          END SELECT  
          SELECT CASE(reg(ii)%side(is)%seg(it)%bc)
            CASE(1_ink)
              WRITE(6,'(a63,a10)') '    Segment boundary condition:    '&
&              //'                            ','     SLIPX'
            CASE(2_ink)
              WRITE(6,'(a63,a10)') '    Segment boundary condition:    '&
&              //'                            ','     SLIPY'
            CASE(3_ink)
              WRITE(6,'(a63,a10)') '    Segment boundary condition:    '&
&              //'                            ','      WALL'
            CASE(8_ink)
              WRITE(6,'(a63,a10)') '    Segment boundary condition:    '&
&              //'                            ','      FREE'
          END SELECT
        ENDDO
      ENDDO
    ENDDO

  END SUBROUTINE mesh_print

  SUBROUTINE line(ireg,i_seg,ind,l1,l2,k1,k2,reg)

    USE reals_mod,ONLY: zerocut
    USE error_mod,ONLY: halt

    ! Argument list
    TYPE(regions),DIMENSION(:),INTENT(IN) :: reg
    INTEGER(KIND=ink),         INTENT(IN) :: ireg,i_seg,ind,l1,l2,k1,k2
    ! Local
    INTEGER(KIND=ink) :: lmin,lmax,il,lm,lp,ll,li,l3,l4
    INTEGER(KIND=ink) :: kmin,kmax,ik,km,kp,kk,ki,k3,k4
    REAL(KIND=rlk)    :: r1,r2,s1,s2,w1,w2,fac,dr,ds,tol,om,dd
    LOGICAL(KIND=lok) :: weighted,exit_status

    s1=reg(ireg)%side(ind)%seg(i_seg)%point(1)
    r1=reg(ireg)%side(ind)%seg(i_seg)%point(2)
    s2=reg(ireg)%side(ind)%seg(i_seg)%point(3)
    r2=reg(ireg)%side(ind)%seg(i_seg)%point(4)
    w1=reg(ireg)%side(ind)%seg(i_seg)%point(5)
    w2=reg(ireg)%side(ind)%seg(i_seg)%point(6)

    kmin=MIN(k1,k2)
    kmax=MAX(k1,k2)
    lmin=MIN(l1,l2)
    lmax=MAX(l1,l2)
    dr=r2-r1
    ds=s2-s1
    il=MAX(1,lmax-lmin)
    ik=MAX(1,kmax-kmin)

    fac=w1+w2
    weighted=.FALSE._lok
    IF (fac.GT.zerocut) THEN
      weighted=.TRUE._lok
      w1=w1/fac
      w2=w2/fac
      IF (reg(ireg)%typ.EQ.4_ink) THEN
        CALL halt('ERROR: consistency between user weights '    &
&                         //'and line weights not yet coded',0)   
      ENDIF
    ENDIF
    tol=reg(ireg)%tol
    om=reg(ireg)%om
    IF (lmin.EQ.lmax) THEN
      dd=REAL(ik,rlk)
      DO kk=kmin,kmax
        ki=kk
        IF (kmin.EQ.k2) ki=kmax+kmin-kk
        reg(ireg)%rr(l1,ki)=r1+REAL(kk-kmin,rlk)*dr/dd
        reg(ireg)%ss(l1,ki)=s1+REAL(kk-kmin,rlk)*ds/dd
        reg(ireg)%bc(l1,ki)=reg(ireg)%side(ind)%seg(i_seg)%bc
      ENDDO
      IF (weighted) THEN
        k3=kmin+1_ink
        k4=kmax-1_ink
        iter_loop_k:DO
          exit_status=.TRUE._lok
          DO kk=k3,k4
            km=kk-1_ink
            kp=kk+1_ink
            dr=w1*reg(ireg)%rr(l1,km)+w2*reg(ireg)%rr(l1,kp)-           &
&              reg(ireg)%rr(l1,kk)
            ds=w1*reg(ireg)%ss(l1,km)+w2*reg(ireg)%ss(l1,kp)-           &
&              reg(ireg)%ss(l1,kk)
            IF ((ABS(dr).GT.tol).OR.(ABS(ds).GT.tol)) THEN
              exit_status=.FALSE._lok
            ENDIF
            reg(ireg)%rr(l1,kk)=reg(ireg)%rr(l1,kk)+om*dr
            reg(ireg)%ss(l1,kk)=reg(ireg)%ss(l1,kk)+om*ds
          ENDDO
          IF (exit_status) EXIT iter_loop_k
        ENDDO iter_loop_k
      ENDIF
    ELSE
      dd=REAL(il,rlk)
      DO ll=lmin,lmax
        li=ll
        IF (lmin.EQ.l2) li=lmax+lmin-ll
        reg(ireg)%rr(li,k1)=r1+REAL(ll-lmin,rlk)*dr/dd
        reg(ireg)%ss(li,k1)=s1+REAL(ll-lmin,rlk)*ds/dd
        reg(ireg)%bc(li,k1)=reg(ireg)%side(ind)%seg(i_seg)%bc
      ENDDO
      IF (weighted) THEN
        l3=lmin+1_ink
        l4=lmax-1_ink
        iter_loop_l:DO
          exit_status=.TRUE._lok
          DO ll=l3,l4
            lm=ll-1_ink
            lp=ll+1_ink
            dr=w1*reg(ireg)%rr(lm,k1)+w2*reg(ireg)%rr(lp,k1)-         &
&              reg(ireg)%rr(ll,k1)
            ds=w1*reg(ireg)%ss(lm,k1)+w2*reg(ireg)%ss(lp,k1)-         &
&              reg(ireg)%ss(ll,k1)
            IF ((ABS(dr).GT.tol).OR.(ABS(ds).GT.tol)) THEN
              exit_status=.FALSE._lok
            ENDIF
            reg(ireg)%rr(ll,k1)=reg(ireg)%rr(ll,k1)+om*dr
            reg(ireg)%ss(ll,k1)=reg(ireg)%ss(ll,k1)+om*ds
          ENDDO
          IF (exit_status) EXIT iter_loop_l
        ENDDO iter_loop_l
      ENDIF
    ENDIF

  END SUBROUTINE line

  SUBROUTINE arc(ireg,i_seg,ind,l1,l2,k1,k2,reg)

    USE reals_mod,     ONLY: zerocut
    USE parameters_mod,ONLY: two_pi,pi

    ! Argument list
    TYPE(regions),DIMENSION(:),INTENT(IN) :: reg
    INTEGER(KIND=ink),         INTENT(IN) :: ireg,i_seg,ind,l1,l2,k1,k2
    ! Local
    INTEGER(KIND=ink) :: lmin,lmax,ll,il,li,l3,l4,lm,lp
    INTEGER(KIND=ink) :: kmin,kmax,kk,ik,ki,k3,k4,km,kp
    REAL(KIND=rlk)    :: r0,r1,r2,s0,s1,s2,fac,d1,d2,theta,theta1
    REAL(KIND=rlk)    :: theta2,dtheta,w1,w2,w3,w4,dd,dl,tol,om
    LOGICAL(KIND=lok) :: fl,weighted,exit_status

    IF (reg(ireg)%side(ind)%seg(i_seg)%seg_typ.EQ.2_ink) THEN
      s1=reg(ireg)%side(ind)%seg(i_seg)%point(3)
      r1=reg(ireg)%side(ind)%seg(i_seg)%point(4)
      s2=reg(ireg)%side(ind)%seg(i_seg)%point(1)
      r2=reg(ireg)%side(ind)%seg(i_seg)%point(2)
      fl=.TRUE._lok
    ELSE
      s1=reg(ireg)%side(ind)%seg(i_seg)%point(1)
      r1=reg(ireg)%side(ind)%seg(i_seg)%point(2)
      s2=reg(ireg)%side(ind)%seg(i_seg)%point(3)
      r2=reg(ireg)%side(ind)%seg(i_seg)%point(4)
      fl=.FALSE._lok
    ENDIF
    r0=reg(ireg)%side(ind)%seg(i_seg)%point(5)
    s0=reg(ireg)%side(ind)%seg(i_seg)%point(6)
    w1=reg(ireg)%side(ind)%seg(i_seg)%point(7)
    w2=reg(ireg)%side(ind)%seg(i_seg)%point(8)

    kmin=MIN(k1,k2)
    kmax=MAX(k1,k2)
    lmin=MIN(l1,l2)
    lmax=MAX(l1,l2)

    fac=w1+w2    
    weighted=.FALSE._lok
    IF (fac.GT.zerocut) THEN
      w1=w1/fac
      w2=w2/fac
      weighted=.TRUE._lok
    ENDIF
    dd=s1-s0
    w3=r1-r0
    IF (ABS(w3).LT.zerocut) THEN
      d2=zerocut
    ELSE
      d2=w3
    ENDIF
    theta1=ATAN(dd/d2)
    IF (d2.LT.0.0_rlk) THEN
      theta1=theta1+pi
    ELSEIF (dd.LT.0.0_rlk) THEN
      theta1=theta1+two_pi
    ENDIF
    d1=s2-s0
    w4=r2-r0
    IF (ABS(w4).LT.zerocut) THEN
      d2=zerocut
    ELSE
      d2=w4
    ENDIF
    theta2=ATAN(d1/d2)
    IF (d2.LT.0.0_rlk) THEN
      theta2=theta2+pi
    ELSEIF (d1.LT.0.0_rlk) THEN
      theta2=theta2+two_pi
    ENDIF
    IF (theta2.GT.theta1) theta2=theta2-two_pi
    d2=SQRT(w4*w4+d1*d1)
    d1=SQRT(w3*w3+dd*dd)
    tol=reg(ireg)%tol
    om=reg(ireg)%om
    IF (lmin.EQ.lmax) THEN
      ik=kmax-kmin
      dd=REAL(ik,rlk)
      dtheta=theta1-theta2
      DO kk=kmin,kmax
        ki=kk
        IF (kmin.EQ.k2) ki=kmin+kmax-kk
        theta=theta1-REAL(kk-kmin,rlk)*dtheta/dd
        w3=-1.0_rlk/dd
        dl=(d1-d2)/dtheta
        dl=d1+dl*(theta-theta1)
        reg(ireg)%rr(l1,ki)=r0+dl*COS(theta)
        reg(ireg)%ss(l1,ki)=s0+dl*SIN(theta)
        reg(ireg)%bc(l1,ki)=reg(ireg)%side(ind)%seg(i_seg)%bc
        IF (reg(ireg)%ss(l1,ki).LT.0.0_rlk) THEN
          w3=theta-w3+ASIN(-s0/dl)
          theta=ASIN(-s0/dl)
        ENDIF
      ENDDO
      reg(ireg)%rr(l1,k1)=r1
      reg(ireg)%rr(l2,k2)=r2
      reg(ireg)%ss(l1,k1)=s1
      reg(ireg)%ss(l2,k2)=s2
      IF (weighted) THEN
        k3=kmin+1_ink
        k4=kmax-1_ink
        DO kk=kmin,kmax
          ki=kk
          IF (kmin.EQ.k2) ki=kmax+kmin-kk
          reg(ireg)%rr(l1,ki)=theta1-REAL(kk-kmin,rlk)*dtheta/dd
        ENDDO
        iter_k:DO
          exit_status=.TRUE._lok
          DO kk=k3,k4
            km=kk-1_ink
            kp=kk+1_ink
            d1=w1*reg(ireg)%rr(l1,km)+w2*reg(ireg)%rr(l1,kp)-           &
&              reg(ireg)%rr(l1,kk)
            IF (ABS(d1).GT.tol) exit_status=.FALSE._lok
            reg(ireg)%rr(l1,kk)=reg(ireg)%rr(l1,kk)+d1
          ENDDO
          IF (exit_status) EXIT iter_k
        ENDDO iter_k
        DO kk=k3,k4
          theta=reg(ireg)%rr(l1,kk)
          dl=(d1-d2)/dtheta
          dl=d1+dl*(theta-theta1)
          reg(ireg)%rr(l1,kk)=r0+dl*COS(theta)
          reg(ireg)%ss(l1,kk)=r0+dl*SIN(theta)
        ENDDO
      ENDIF
    ELSE
      il=lmax-lmin
      dd=REAL(il,rlk)
      dtheta=theta1-theta2
      DO ll=lmin,lmax
        li=ll
        IF (lmin.EQ.l2) li=lmax+lmin-ll
        theta=theta1-REAL(ll-lmin,rlk)*dtheta/dd
        w3=-1.0_rlk/dd
        dl=(d1-d2)/dtheta
        dl=d1+dl*(theta-theta1)
        reg(ireg)%rr(li,k1)=r0+dl*COS(theta)
        reg(ireg)%ss(li,k1)=s0+dl*SIN(theta)
        reg(ireg)%bc(li,k1)=reg(ireg)%side(ind)%seg(i_seg)%bc
        IF (reg(ireg)%ss(li,k1).LT.0.0_rlk) THEN
          w3=theta-w3+ASIN(-s0/dl)
          theta=ASIN(-s0/dl)
        ENDIF
      ENDDO
      reg(ireg)%rr(l1,k1)=r1
      reg(ireg)%rr(l2,k2)=r2
      reg(ireg)%ss(l1,k1)=s1
      reg(ireg)%ss(l2,k2)=s2
      IF (weighted) THEN
        l3=lmin+1_ink
        l4=lmax-1_ink
        DO ll=lmin,lmax
          li=ll
          IF (lmin.EQ.l2) li=lmax+lmin-ll
          reg(ireg)%rr(li,k1)=theta1-REAL(ll-lmin,rlk)*dtheta/dd
        ENDDO
        iter_l:DO
          exit_status=.TRUE._lok
          DO ll=l3,l4
            lm=ll-1_ink
            lp=ll+1_ink
            d1=w1*reg(ireg)%rr(lm,k1)+w2*reg(ireg)%rr(lp,k1)-           &
&              reg(ireg)%rr(ll,k1)
            IF (ABS(d1).GT.tol) exit_status=.FALSE._lok
            reg(ireg)%rr(ll,k1)=reg(ireg)%rr(ll,k1)+d1
          ENDDO
          IF (exit_status) EXIT iter_l
        ENDDO iter_l
        DO ll=l3,l4
          theta=reg(ireg)%rr(ll,k1)
          dl=(d1-d2)/dtheta
          dl=d1+dl*(theta-theta1)
          reg(ireg)%rr(ll,k1)=r0+dl*COS(theta)
          reg(ireg)%ss(ll,k1)=r0+dl*SIN(theta)
        ENDDO
      ENDIF
    ENDIF

    IF (fl) THEN
      IF (l1.NE.l2) THEN
        DO ll=0,(lmax-lmin)/2
          li=lmin+ll
          il=lmax-ll
          w3=reg(ireg)%rr(li,k1)
          w4=reg(ireg)%ss(li,k1)
          reg(ireg)%rr(li,k1)=reg(ireg)%rr(il,k1)
          reg(ireg)%ss(li,k1)=reg(ireg)%ss(il,k1)
          reg(ireg)%rr(il,k1)=w3
          reg(ireg)%ss(il,k1)=w4
        ENDDO
      ELSE
        DO kk=0,(kmax-kmin)/2
          ki=kmin+kk
          ik=kmax-kk
          w3=reg(ireg)%rr(l1,ki)
          w4=reg(ireg)%ss(l1,ki)
          reg(ireg)%rr(l1,ki)=reg(ireg)%rr(l1,ik)
          reg(ireg)%ss(l1,ki)=reg(ireg)%ss(l1,ik)
          reg(ireg)%rr(l1,ik)=w3
          reg(ireg)%ss(l1,ik)=w4
        ENDDO
      ENDIF
    ENDIF

  END SUBROUTINE arc

END MODULE mesh_mod
