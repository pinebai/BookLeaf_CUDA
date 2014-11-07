
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

  USE scratch_mod,   ONLY: dub=>rscratch16,dul=>rscratch17,           &
&                          dut=>rscratch18,dur=>rscratch19,           &
&                          dvb=>rscratch110,dvl=>rscratch111,         &
&                          dvt=>rscratch112,dvr=>rscratch113,         &
&                          dxb=>rscratch114,dxl=>rscratch115,         &
&                          dxt=>rscratch116,dxr=>rscratch117,         &
&                          dyb=>rscratch118,dyl=>rscratch119,         &
&                          dyt=>rscratch120,dyr=>rscratch121
  IMPLICIT NONE

  PUBLIC :: getq

CONTAINS

  SUBROUTINE getq(nshape,nel,elx,ely,elu,elv,rho,pre)

    USE kinds_mod,    ONLY: ink,rlk
    USE integers_mod, ONLY: nel1
    USE reals_mod,    ONLY: zerocut,cq1,cq2
    USE comms_mod,    ONLY: exchange,VISCOSITY
    USE paradef_mod,  ONLY: zparallel
    USE pointers_mod, ONLY: a1,a3,b1,b3,ielnod,ielel,indtype,qq,qx,qy,   &
&                           csqrd
    USE timing_stats, ONLY: bookleaf_times
    USE TYPH_util_mod,ONLY: get_time

    ! Argument list
    INTEGER(KIND=ink),                    INTENT(IN) :: nshape,nel
    REAL(KIND=rlk),DIMENSION(nshape,nel1),INTENT(IN) :: elx,ely,elu,elv
    REAL(KIND=rlk),DIMENSION(nel1),       INTENT(IN) :: rho,pre
    ! Local
    INTEGER(KIND=ink)                                :: iel,e1,e2,e3,e4, &
&                                                       n1,n2,n3,n4,jj,ierr
    REAL(KIND=rlk)                                   :: w1,w2,w3,w4,w5,  &
&                                                       w6,w7,w8,den,    &
&                                                       uhat,vhat,xhat,  &
&                                                       yhat,rl,rr,rb,rt,&
&                                                       t0,t1
    REAL(KIND=rlk),   DIMENSION(nel1)                :: phib,phit,phil,  &
&                                                       phir

    t0 = get_time()

    !# Missing code here that can't be merged
    ! initialisation
    qq=0.0_rlk
    qx=0.0_rlk
    qy=0.0_rlk
    phib=0.0_rlk
    phit=0.0_rlk
    phil=0.0_rlk
    phir=0.0_rlk
    !# Missing code here that can't be merged
    ! gradient construction, geometry checked in getgeom
    l1:DO iel=1,nel
      dub(iel)=elu(2,iel)-elu(1,iel)
      dul(iel)=elu(1,iel)-elu(4,iel) 
      dut(iel)=elu(4,iel)-elu(3,iel) 
      dur(iel)=elu(3,iel)-elu(2,iel) 
      dvb(iel)=elv(2,iel)-elv(1,iel) 
      dvl(iel)=elv(1,iel)-elv(4,iel) 
      dvt(iel)=elv(4,iel)-elv(3,iel) 
      dvr(iel)=elv(3,iel)-elv(2,iel) 
      dxb(iel)=elx(2,iel)-elx(1,iel) 
      dxl(iel)=elx(1,iel)-elx(4,iel) 
      dxt(iel)=elx(4,iel)-elx(3,iel) 
      dxr(iel)=elx(3,iel)-elx(2,iel) 
      dyb(iel)=ely(2,iel)-ely(1,iel) 
      dyl(iel)=ely(1,iel)-ely(4,iel) 
      dyt(iel)=ely(4,iel)-ely(3,iel) 
      dyr(iel)=ely(3,iel)-ely(2,iel) 
    ENDDO l1

    IF (zparallel) THEN
      call exchange(VISCOSITY)
    ENDIF

    ! Christiensen monotonic limit
    DO iel=1,nel 
      e1=ielel(1,iel) 
      e2=ielel(2,iel) 
      e3=ielel(3,iel) 
      e4=ielel(4,iel) 
      n1=ielnod(1,iel) 
      n2=ielnod(2,iel) 
      n3=ielnod(3,iel) 
      n4=ielnod(4,iel) 
      ! initialise
      w1=0.0_rlk
      w2=0.0_rlk
      w3=0.0_rlk
      w4=0.0_rlk
      w5=0.0_rlk
      w6=0.0_rlk
      w7=0.0_rlk
      w8=0.0_rlk 
      ! Neighbour 1
      IF (e1.NE.0_ink) THEN 
        den=SQRT(dul(iel)*dul(iel)+dvl(iel)*dvl(iel)) 
        uhat=dul(iel)/MAX(den,zerocut) 
        vhat=dvl(iel)/MAX(den,zerocut) 
        den=SQRT(dxl(iel)*dxl(iel)+dyl(iel)*dyl(iel)) 
        xhat=dxl(iel)/MAX(den,zerocut) 
        yhat=dyl(iel)/MAX(den,zerocut) 
        den=dxl(iel)*xhat+dyl(iel)*yhat
        rl=(dul(iel)*uhat+dvl(iel)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        den=dxl(e1)*xhat+dyl(e1)*yhat      
        w1=(dul(e1)*uhat+dvl(e1)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        w1=w1/SIGN(MAX(ABS(rl),zerocut),rl)
        den=SQRT(dur(iel)*dur(iel)+dvr(iel)*dvr(iel)) 
        uhat=dur(iel)/MAX(den,zerocut) 
        vhat=dvr(iel)/MAX(den,zerocut) 
        den=SQRT(dxr(iel)*dxr(iel)+dyr(iel)*dyr(iel)) 
        xhat=dxr(iel)/MAX(den,zerocut) 
        yhat=dyr(iel)/MAX(den,zerocut) 
        den=dxr(iel)*xhat+dyr(iel)*yhat
        rr=(dur(iel)*uhat+dvr(iel)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        den=dxr(e1)*xhat+dyr(e1)*yhat
        w2=(dur(e1)*uhat+dvr(e1)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        w2=w2/SIGN(MAX(ABS(rr),zerocut),rr)
      ELSE  
        IF (((indtype(n1).LT.0_ink).AND.(indtype(n2).LT.0_ink)).AND.    &
&           (e3.NE.0_ink)) THEN   
        !# Missing code here that can't be merged
          w1=1.0_rlk
          w2=1.0_rlk 
        ELSE 
          w1=0.0_rlk
          w2=0.0_rlk
        ENDIF 
      ENDIF 
      ! Neighbour 2
      IF (e2.NE.0_ink) THEN 
        den=SQRT(dub(iel)*dub(iel)+dvb(iel)*dvb(iel)) 
        uhat=dub(iel)/MAX(den,zerocut)
        vhat=dvb(iel)/MAX(den,zerocut) 
        den=SQRT(dxb(iel)*dxb(iel)+dyb(iel)*dyb(iel)) 
        xhat=dxb(iel)/MAX(den,zerocut) 
        yhat=dyb(iel)/MAX(den,zerocut)
        den=dxb(iel)*xhat+dyb(iel)*yhat
        rb=(dub(iel)*uhat+dvb(iel)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        den=dxb(e2)*xhat+dyb(e2)*yhat
        w3=(dub(e2)*uhat+dvb(e2)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        w3=w3/SIGN(MAX(ABS(rb),zerocut),rb)
        den=SQRT(dut(iel)*dut(iel)+dvt(iel)*dvt(iel)) 
        uhat=dut(iel)/MAX(den,zerocut) 
        vhat=dvt(iel)/MAX(den,zerocut) 
        den=SQRT(dxt(iel)*dxt(iel)+dyt(iel)*dyt(iel)) 
        xhat=dxt(iel)/MAX(den,zerocut) 
        yhat=dyt(iel)/MAX(den,zerocut)
        den=dxt(iel)*xhat+dyt(iel)*yhat
        rt=(dut(iel)*uhat+dvt(iel)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        den=dxt(e2)*xhat+dyt(e2)*yhat
        w4=(dut(e2)*uhat+dvt(e2)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        w4=w4/SIGN(MAX(ABS(rt),zerocut),rt) 
      ELSE  
        IF (((indtype(n2).LT.0_ink).AND.(indtype(n3).LT.0_ink)).AND.    &
&           (e4.NE.0_ink)) THEN 
        !# Missing code here that can't be merged
          w3=1.0_rlk
          w4=1.0_rlk
        ELSE 
          w3=0.0_rlk
          w4=0.0_rlk
        ENDIF 
      ENDIF 
      ! Neighbour 3
      IF (e3.NE.0_ink) THEN 
        den=SQRT(dul(iel)*dul(iel)+dvl(iel)*dvl(iel)) 
        uhat=dul(iel)/MAX(den,zerocut) 
        vhat=dvl(iel)/MAX(den,zerocut) 
        den=SQRT(dxl(iel)*dxl(iel)+dyl(iel)*dyl(iel)) 
        xhat=dxl(iel)/MAX(den,zerocut) 
        yhat=dyl(iel)/MAX(den,zerocut) 
        den=dxl(iel)*xhat+dyl(iel)*yhat
        rl=(dul(iel)*uhat+dvl(iel)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        den=dxl(e3)*xhat+dyl(e3)*yhat
        w5=(dul(e3)*uhat+dvl(e3)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        w5=w5/SIGN(MAX(ABS(rl),zerocut),rl) 
        den=SQRT(dur(iel)*dur(iel)+dvr(iel)*dvr(iel)) 
        uhat=dur(iel)/MAX(den,zerocut) 
        vhat=dvr(iel)/MAX(den,zerocut) 
        den=SQRT(dxr(iel)*dxr(iel)+dyr(iel)*dyr(iel)) 
        xhat=dxr(iel)/MAX(den,zerocut) 
        yhat=dyr(iel)/MAX(den,zerocut) 
        den=dxr(iel)*xhat+dyr(iel)*yhat
        rr=(dur(iel)*uhat+dvr(iel)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        den=dxr(e3)*xhat+dyr(e3)*yhat
        w6=(dur(e3)*uhat+dvr(e3)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        w6=w6/SIGN(MAX(ABS(rr),zerocut),rr) 
      ELSE  
        IF (((indtype(n3).LT.0_ink).AND.(indtype(n4).LT.0_ink)).AND.    &
&           (e1.NE.0_ink)) THEN 
        !# Missing code here that can't be merged
          w5=1.0_rlk
          w6=1.0_rlk
        ELSE 
          w5=0.0_rlk
          w6=0.0_rlk
        ENDIF 
      ENDIF 
      ! Neighbour 4
      IF (e4.NE.0_ink) THEN 
        den=SQRT(dub(iel)*dub(iel)+dvb(iel)*dvb(iel)) 
        uhat=dub(iel)/MAX(den,zerocut)
        vhat=dvb(iel)/MAX(den,zerocut)
        den=SQRT(dxb(iel)*dxb(iel)+dyb(iel)*dyb(iel)) 
        xhat=dxb(iel)/MAX(den,zerocut)
        yhat=dyb(iel)/MAX(den,zerocut)
        den=dxb(iel)*xhat+dyb(iel)*yhat
        rb=(dub(iel)*uhat+dvb(iel)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        den=dxb(e4)*xhat+dyb(e4)*yhat 
        w7=(dub(e4)*uhat+dvb(e4)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        w7=w7/SIGN(MAX(ABS(rb),zerocut),rb) 
        den=SQRT(dut(iel)*dut(iel)+dvt(iel)*dvt(iel)) 
        uhat=dut(iel)/MAX(den,zerocut) 
        vhat=dvt(iel)/MAX(den,zerocut) 
        den=SQRT(dxt(iel)*dxt(iel)+dyt(iel)*dyt(iel)) 
        xhat=dxt(iel)/MAX(den,zerocut) 
        yhat=dyt(iel)/MAX(den,zerocut)
        den=dxt(iel)*xhat+dyt(iel)*yhat
        rt=(dut(iel)*uhat+dvt(iel)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        den=dxt(e4)*xhat+dyt(e4)*yhat
        w8=(dut(e4)*uhat+dvt(e4)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        w8=w8/SIGN(MAX(ABS(rt),zerocut),rt)
      ELSE  
        IF (((indtype(n4).LT.0_ink).AND.(indtype(n1).LT.0_ink)).AND.    &
&           (e2.NE.0_ink)) THEN 
        !# Missing code here that can't be merged
          w7=1.0_rlk 
          w8=1.0_rlk
        ELSE 
          w7=0.0_rlk
          w8=0.0_rlk
        ENDIF 
      ENDIF 
      ! Calculate limiters 
      phib(iel)=MAX(0.0_rlk,MIN(0.5_rlk*(w7+w3),2.0_rlk*w7,2.0_rlk*w3,1.0_rlk)) 
      phit(iel)=MAX(0.0_rlk,MIN(0.5_rlk*(w8+w4),2.0_rlk*w8,2.0_rlk*w4,1.0_rlk)) 
      phil(iel)=MAX(0.0_rlk,MIN(0.5_rlk*(w5+w1),2.0_rlk*w5,2.0_rlk*w1,1.0_rlk)) 
      phir(iel)=MAX(0.0_rlk,MIN(0.5_rlk*(w6+w2),2.0_rlk*w6,2.0_rlk*w2,1.0_rlk)) 
    ENDDO 
    ! Construct artificial viscosity
    DO iel=1,nel 
      rr=SQRT(csqrd(iel))
      den=SQRT(dub(iel)*dub(iel)+dvb(iel)*dvb(iel))
      qx(1,iel)=(1.0_rlk-phib(iel))*rho(iel)*(cq1*rr+cq2*den)
      qy(1,iel)=(1.0_rlk-phib(iel))*rho(iel)*(cq1*rr+cq2*den) 
      den=SQRT(dut(iel)*dut(iel)+dvt(iel)*dvt(iel)) 
      qx(3,iel)=(1.0_rlk-phit(iel))*rho(iel)*(cq1*rr+cq2*den)
      qy(3,iel)=(1.0_rlk-phit(iel))*rho(iel)*(cq1*rr+cq2*den)
      den=SQRT(dul(iel)*dul(iel)+dvl(iel)*dvl(iel))
      qx(4,iel)=(1.0_rlk-phil(iel))*rho(iel)*(cq1*rr+cq2*den)
      qy(4,iel)=(1.0_rlk-phil(iel))*rho(iel)*(cq1*rr+cq2*den)
      den=SQRT(dur(iel)*dur(iel)+dvr(iel)*dvr(iel))
      qx(2,iel)=(1.0_rlk-phir(iel))*rho(iel)*(cq1*rr+cq2*den)
      qy(2,iel)=(1.0_rlk-phir(iel))*rho(iel)*(cq1*rr+cq2*den)
      w4=0.25_rlk*(elx(1,iel)+elx(2,iel)+elx(3,iel)+elx(4,iel))
      w8=0.25_rlk*(ely(1,iel)+ely(2,iel)+ely(3,iel)+ely(4,iel))
      DO jj=1,4
        n1=jj
        n2=jj+1_ink
        IF (n2.GT.4_ink) n2=1_ink
        w1=elx(n1,iel)
        w5=ely(n1,iel)
        w2=elx(n2,iel)
        w6=ely(n2,iel)
        w3=0.5_rlk*(w2+w1)
        w7=0.5_rlk*(w6+w5)
        den=SQRT((w4-w3)*(w4-w3)+(w8-w7)*(w8-w7))
        IF (den.GT.zerocut) THEN
          xhat=(w8-w7)/den
          yhat=(w3-w4)/den
          rb=SQRT((w2-w1)*(w2-w1)+(w6-w5)*(w6-w5))
          IF (rb.GT.zerocut) THEN
            uhat=(w2-w1)/rb
            vhat=(w6-w5)/rb
            rr=xhat*uhat+yhat*vhat
            rr=-SIGN(1.0_rlk,rr)
            xhat=xhat*den*rr
            yhat=yhat*den*rr
            uhat=elu(n2,iel)-elu(n1,iel)
            vhat=elv(n2,iel)-elv(n1,iel)
            rt=SQRT((uhat*uhat)+(vhat*vhat))
            IF (rt.GT.zerocut) THEN
              rl=uhat*xhat+vhat*yhat
              IF (rl.GT.0.0_rlk) THEN
                qx(jj,iel)=qx(jj,iel)*rl*uhat/rt
                qy(jj,iel)=qy(jj,iel)*rl*vhat/rt
              ELSE
                qx(jj,iel)=0.0_rlk
                qy(jj,iel)=0.0_rlk
              ENDIF
            ELSE
              qx(jj,iel)=0.0_rlk
              qy(jj,iel)=0.0_rlk
            ENDIF
          ELSE
            qx(jj,iel)=0.0_rlk
            qy(jj,iel)=0.0_rlk
          ENDIF
        ELSE
          qx(jj,iel)=0.0_rlk
          qy(jj,iel)=0.0_rlk
        ENDIF
      ENDDO
      qq(iel)=0.25_rlk*(SQRT(qx(1,iel)*qx(1,iel)+qy(1,iel)*qy(1,iel))   &
                       +SQRT(qx(2,iel)*qx(2,iel)+qy(2,iel)*qy(2,iel))   &
                       +SQRT(qx(3,iel)*qx(3,iel)+qy(3,iel)*qy(3,iel))   &
                       +SQRT(qx(4,iel)*qx(4,iel)+qy(4,iel)*qy(4,iel)))
    ENDDO

    ! Timing data
    t1 = get_time()
    t1=t1-t0
    bookleaf_times%time_in_getq=bookleaf_times%time_in_getq+t1

  END SUBROUTINE getq

END MODULE getq_mod
