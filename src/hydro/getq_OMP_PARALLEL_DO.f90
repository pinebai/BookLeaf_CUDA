
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

MODULE getq_mod

  IMPLICIT NONE

  PUBLIC :: getq

CONTAINS

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
!$OMP PARALLEL DO
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
!$OMP END PARALLEL DO

    ! gradient construction
!$OMP PARALLEL DO
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
!$OMP END PARALLEL DO

    ! MPI parallelism
    IF (zparallel) THEN
      CALL exchange(VISCOSITY)
    ENDIF

    ! Christiensen monotonic limit
    DO iside=1,nshape/2_ink
      is1=MOD(iside+2_ink,nshape)+1_ink
      is2=iside+1_ink

!$OMP PARALLEL DO PRIVATE(in1,in2,w1,w2,w3,w4,w5,w6,w7,w8,den,xhat,yhat,uhat,vhat,ins)
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
!$OMP END PARALLEL DO

      ! BC
      ins=iside+2_ink
!$OMP PARALLEL DO PRIVATE(in1,in2,ic1,ic2)
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
!$OMP END PARALLEL DO

      ! Apply limiter
!$OMP PARALLEL DO PRIVATE(w1,w2,w3,w4)
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
!$OMP END PARALLEL DO
    ENDDO

    ! Final Q calculation
    DO iside=1,nshape
      ins=MOD(iside,nshape)+1_ink
!$OMP PARALLEL DO PRIVATE(w1,w2,w3,w4,w5,w6,w7,w8,den,xhat,yhat,uhat,vhat)
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
!$OMP END PARALLEL DO
    ENDDO

    ! Timing data
    t1=get_time()
    t1=t1-t0
    bookleaf_times%time_in_getq=bookleaf_times%time_in_getq+t1

  END SUBROUTINE getq

END MODULE getq_mod
