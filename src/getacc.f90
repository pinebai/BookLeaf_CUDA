
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

MODULE getacc_mod

  IMPLICIT NONE

  PUBLIC :: getacc

CONTAINS

  SUBROUTINE getacc(nshape,nel,nes,nnod,nns,cnfx,cnfy,ndub,ndvb,elu,elv,&
&                   rho,dt05,dt)

    USE kinds_mod,    ONLY: ink,rlk
    USE reals_mod,    ONLY: zerocut,dencut,accut
    USE comms_mod,    ONLY: HALFSTEP,exchange
    USE paradef_mod,  ONLY: zparallel,ielsort1
    USE pointers_mod, ONLY: ielnd,cnmass,cnwt,indtype,ndu,ndv,ndx,ndy
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

END MODULE getacc_mod
