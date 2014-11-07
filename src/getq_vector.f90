
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
&                                                       n1,n2,n3,n4,ierr
    REAL(KIND=rlk),  DIMENSION(nel)                  :: w1,w2,w3,w4,w5,w6,w7,w8
    REAL(KIND=rlk)                                   :: w1_s,w2_s,w3_s,w4_s,w5_s, &
&                                                       w6_s, w7_s, w8_s, den,    &
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
    DO iel=1,nel 
      e1=ielel(1,iel) 
 
        den=SQRT(dul(iel)*dul(iel)+dvl(iel)*dvl(iel)) 
        uhat=dul(iel)/MAX(den,zerocut) 
        vhat=dvl(iel)/MAX(den,zerocut) 
        den=SQRT(dxl(iel)*dxl(iel)+dyl(iel)*dyl(iel)) 
        xhat=dxl(iel)/MAX(den,zerocut) 
        yhat=dyl(iel)/MAX(den,zerocut) 
        den=dxl(iel)*xhat+dyl(iel)*yhat
        rl=(dul(iel)*uhat+dvl(iel)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        den=dxl(e1)*xhat+dyl(e1)*yhat      
        w1(iel)=(dul(e1)*uhat+dvl(e1)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        w1(iel)=w1(iel)/SIGN(MAX(ABS(rl),zerocut),rl)
        den=SQRT(dur(iel)*dur(iel)+dvr(iel)*dvr(iel)) 
        uhat=dur(iel)/MAX(den,zerocut) 
        vhat=dvr(iel)/MAX(den,zerocut) 
        den=SQRT(dxr(iel)*dxr(iel)+dyr(iel)*dyr(iel)) 
        xhat=dxr(iel)/MAX(den,zerocut) 
        yhat=dyr(iel)/MAX(den,zerocut) 
        den=dxr(iel)*xhat+dyr(iel)*yhat
        rr=(dur(iel)*uhat+dvr(iel)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        den=dxr(e1)*xhat+dyr(e1)*yhat
        w2(iel)=(dur(e1)*uhat+dvr(e1)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        w2(iel)=w2(iel)/SIGN(MAX(ABS(rr),zerocut),rr)

    ENDDO
    DO iel=1,nel
      e1=ielel(1,iel)

      IF (e1.EQ.0_ink) THEN

      e3=ielel(3,iel)

      n1=ielnod(1,iel)
      n2=ielnod(2,iel)

        IF (((indtype(n1).LT.0_ink).AND.(indtype(n2).LT.0_ink)).AND.    &
&           (e3.NE.0_ink)) THEN   
        !# Missing code here that can't be merged
          w1(iel)=1.0_rlk
          w2(iel)=1.0_rlk 
        ELSE 
          w1(iel)=0.0_rlk
          w2(iel)=0.0_rlk
        ENDIF 
      ENDIF 

    ENDDO

    ! Neighbour 2
    DO iel=1,nel

      e2=ielel(2,iel)
 

        den=SQRT(dub(iel)*dub(iel)+dvb(iel)*dvb(iel)) 
        uhat=dub(iel)/MAX(den,zerocut)
        vhat=dvb(iel)/MAX(den,zerocut) 
        den=SQRT(dxb(iel)*dxb(iel)+dyb(iel)*dyb(iel)) 
        xhat=dxb(iel)/MAX(den,zerocut) 
        yhat=dyb(iel)/MAX(den,zerocut)
        den=dxb(iel)*xhat+dyb(iel)*yhat
        rb=(dub(iel)*uhat+dvb(iel)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        den=dxb(e2)*xhat+dyb(e2)*yhat
        w3(iel)=(dub(e2)*uhat+dvb(e2)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        w3(iel)=w3(iel)/SIGN(MAX(ABS(rb),zerocut),rb)
        den=SQRT(dut(iel)*dut(iel)+dvt(iel)*dvt(iel)) 
        uhat=dut(iel)/MAX(den,zerocut) 
        vhat=dvt(iel)/MAX(den,zerocut) 
        den=SQRT(dxt(iel)*dxt(iel)+dyt(iel)*dyt(iel)) 
        xhat=dxt(iel)/MAX(den,zerocut) 
        yhat=dyt(iel)/MAX(den,zerocut)
        den=dxt(iel)*xhat+dyt(iel)*yhat
        rt=(dut(iel)*uhat+dvt(iel)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        den=dxt(e2)*xhat+dyt(e2)*yhat
        w4(iel)=(dut(e2)*uhat+dvt(e2)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        w4(iel)=w4(iel)/SIGN(MAX(ABS(rt),zerocut),rt) 

     ENDDO
     DO iel=1,nel

       e2=ielel(2,iel)

       IF (e2.EQ.0_ink) THEN

       e4=ielel(4,iel)

       n2=ielnod(2,iel)
       n3=ielnod(3,iel)


       IF (((indtype(n2).LT.0_ink).AND.(indtype(n3).LT.0_ink)).AND.    &
&           (e4.NE.0_ink)) THEN 
        !# Missing code here that can't be merged
          w3(iel)=1.0_rlk
          w4(iel)=1.0_rlk
        ELSE 
          w3(iel)=0.0_rlk
          w4(iel)=0.0_rlk
        ENDIF 
      ENDIF 

    ENDDO

    ! Neighbour 3
    DO iel=1,nel 

      e3=ielel(3,iel) 

        den=SQRT(dul(iel)*dul(iel)+dvl(iel)*dvl(iel)) 
        uhat=dul(iel)/MAX(den,zerocut) 
        vhat=dvl(iel)/MAX(den,zerocut) 
        den=SQRT(dxl(iel)*dxl(iel)+dyl(iel)*dyl(iel)) 
        xhat=dxl(iel)/MAX(den,zerocut) 
        yhat=dyl(iel)/MAX(den,zerocut) 
        den=dxl(iel)*xhat+dyl(iel)*yhat
        rl=(dul(iel)*uhat+dvl(iel)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        den=dxl(e3)*xhat+dyl(e3)*yhat
        w5(iel)=(dul(e3)*uhat+dvl(e3)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        w5(iel)=w5(iel)/SIGN(MAX(ABS(rl),zerocut),rl) 
        den=SQRT(dur(iel)*dur(iel)+dvr(iel)*dvr(iel)) 
        uhat=dur(iel)/MAX(den,zerocut) 
        vhat=dvr(iel)/MAX(den,zerocut) 
        den=SQRT(dxr(iel)*dxr(iel)+dyr(iel)*dyr(iel)) 
        xhat=dxr(iel)/MAX(den,zerocut) 
        yhat=dyr(iel)/MAX(den,zerocut) 
        den=dxr(iel)*xhat+dyr(iel)*yhat
        rr=(dur(iel)*uhat+dvr(iel)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        den=dxr(e3)*xhat+dyr(e3)*yhat
        w6(iel)=(dur(e3)*uhat+dvr(e3)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        w6(iel)=w6(iel)/SIGN(MAX(ABS(rr),zerocut),rr) 

      enddo

      do iel=1,nel
      e3=ielel(3,iel)
      IF (e3.EQ.0_ink) THEN
        e1=ielel(1,iel)

        n3=ielnod(3,iel)
        n4=ielnod(4,iel)

        IF (((indtype(n3).LT.0_ink).AND.(indtype(n4).LT.0_ink)).AND.    &
&           (e1.NE.0_ink)) THEN 
        !# Missing code here that can't be merged
          w5(iel)=1.0_rlk
          w6(iel)=1.0_rlk
        ELSE 
          w5(iel)=0.0_rlk
          w6(iel)=0.0_rlk
        ENDIF 
      ENDIF 


    enddo


   ! Neighbour 4

    DO iel=1,nel 

      e4=ielel(4,iel) 

        den=SQRT(dub(iel)*dub(iel)+dvb(iel)*dvb(iel)) 
        uhat=dub(iel)/MAX(den,zerocut)
        vhat=dvb(iel)/MAX(den,zerocut)
        den=SQRT(dxb(iel)*dxb(iel)+dyb(iel)*dyb(iel)) 
        xhat=dxb(iel)/MAX(den,zerocut)
        yhat=dyb(iel)/MAX(den,zerocut)
        den=dxb(iel)*xhat+dyb(iel)*yhat
        rb=(dub(iel)*uhat+dvb(iel)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        den=dxb(e4)*xhat+dyb(e4)*yhat 
        w7(iel)=(dub(e4)*uhat+dvb(e4)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        w7(iel)=w7(iel)/SIGN(MAX(ABS(rb),zerocut),rb) 
        den=SQRT(dut(iel)*dut(iel)+dvt(iel)*dvt(iel)) 
        uhat=dut(iel)/MAX(den,zerocut) 
        vhat=dvt(iel)/MAX(den,zerocut) 
        den=SQRT(dxt(iel)*dxt(iel)+dyt(iel)*dyt(iel)) 
        xhat=dxt(iel)/MAX(den,zerocut) 
        yhat=dyt(iel)/MAX(den,zerocut)
        den=dxt(iel)*xhat+dyt(iel)*yhat
        rt=(dut(iel)*uhat+dvt(iel)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        den=dxt(e4)*xhat+dyt(e4)*yhat
        w8(iel)=(dut(e4)*uhat+dvt(e4)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        w8(iel)=w8(iel)/SIGN(MAX(ABS(rt),zerocut),rt)

    ENDDO

    DO iel=1,nel

      e4=ielel(4,iel)

      IF (e4.EQ.0_ink) THEN
       e2=ielel(2,iel)       

       n1=ielnod(1,iel)
       n4=ielnod(4,iel)

       IF (((indtype(n4).LT.0_ink).AND.(indtype(n1).LT.0_ink)).AND.    &
&           (e2.NE.0_ink)) THEN 
        !# Missing code here that can't be merged
          w7(iel)=1.0_rlk 
          w8(iel)=1.0_rlk
        ELSE 
          w7(iel)=0.0_rlk
          w8(iel)=0.0_rlk
        ENDIF 
      ENDIF 

     end DO


      ! Calculate limiters 
    DO iel=1,nel 
      phib(iel)=MAX(0.0_rlk,MIN(0.5_rlk*(w7(iel)+w3(iel)),2.0_rlk*w7(iel),2.0_rlk*w3(iel),1.0_rlk)) 
      phit(iel)=MAX(0.0_rlk,MIN(0.5_rlk*(w8(iel)+w4(iel)),2.0_rlk*w8(iel),2.0_rlk*w4(iel),1.0_rlk)) 
      phil(iel)=MAX(0.0_rlk,MIN(0.5_rlk*(w5(iel)+w1(iel)),2.0_rlk*w5(iel),2.0_rlk*w1(iel),1.0_rlk)) 
      phir(iel)=MAX(0.0_rlk,MIN(0.5_rlk*(w6(iel)+w2(iel)),2.0_rlk*w6(iel),2.0_rlk*w2(iel),1.0_rlk)) 
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
      w4_s=0.25_rlk*(elx(1,iel)+elx(2,iel)+elx(3,iel)+elx(4,iel))
      w8_s=0.25_rlk*(ely(1,iel)+ely(2,iel)+ely(3,iel)+ely(4,iel))

      ! For n1 = 1, n2 =2
        n1=1_ink
        n2=2_ink
        w1_s=elx(n1,iel)
        w5_s=ely(n1,iel)
        w2_s=elx(n2,iel)
        w6_s=ely(n2,iel)
        w3_s=0.5_rlk*(w2_s+w1_s)
        w7_s=0.5_rlk*(w6_s+w5_s)
        den=SQRT((w4_s-w3_s)*(w4_s-w3_s)+(w8_s-w7_s)*(w8_s-w7_s))
        xhat=(w8_s-w7_s)/den
        yhat=(w3_s-w4_s)/den
        rb=SQRT((w2_s-w1_s)*(w2_s-w1_s)+(w6_s-w5_s)*(w6_s-w5_s))
        uhat=(w2_s-w1_s)/rb
        vhat=(w6_s-w5_s)/rb
        rr=xhat*uhat+yhat*vhat
        rr=-SIGN(1.0_rlk,rr)
        xhat=xhat*den*rr
        yhat=yhat*den*rr
        uhat=elu(n2,iel)-elu(n1,iel)
        vhat=elv(n2,iel)-elv(n1,iel)
        rt=SQRT((uhat*uhat)+(vhat*vhat))
        rl=uhat*xhat+vhat*yhat
        qx(1,iel)=qx(1,iel)*rl*uhat/rt
        qy(1,iel)=qy(1,iel)*rl*vhat/rt

        ! Cut off checks
        IF ( (den.LE.zerocut)  ) THEN
              qx(1,iel)=0.0_rlk
              qy(1,iel)=0.0_rlk
        ENDIF

        IF ( (rb.LE.zerocut)  ) THEN
              qx(1,iel)=0.0_rlk
              qy(1,iel)=0.0_rlk
        ENDIF

        IF ( (rt.LE.zerocut)  ) THEN
              qx(1,iel)=0.0_rlk
              qy(1,iel)=0.0_rlk
        ENDIF

        IF ( (rl.LE.zerocut)  ) THEN
              qx(1,iel)=0.0_rlk
              qy(1,iel)=0.0_rlk
        ENDIF

      ! For n1 = 2, n2 = 3
        n1=2_ink
        n2=3_ink
        w1_s=elx(n1,iel)
        w5_s=ely(n1,iel)
        w2_s=elx(n2,iel)
        w6_s=ely(n2,iel)
        w3_s=0.5_rlk*(w2_s+w1_s)
        w7_s=0.5_rlk*(w6_s+w5_s)
        den=SQRT((w4_s-w3_s)*(w4_s-w3_s)+(w8_s-w7_s)*(w8_s-w7_s))
        xhat=(w8_s-w7_s)/den
        yhat=(w3_s-w4_s)/den
        rb=SQRT((w2_s-w1_s)*(w2_s-w1_s)+(w6_s-w5_s)*(w6_s-w5_s))
        uhat=(w2_s-w1_s)/rb
        vhat=(w6_s-w5_s)/rb
        rr=xhat*uhat+yhat*vhat
        rr=-SIGN(1.0_rlk,rr)
        xhat=xhat*den*rr
        yhat=yhat*den*rr
        uhat=elu(n2,iel)-elu(n1,iel)
        vhat=elv(n2,iel)-elv(n1,iel)
        rt=SQRT((uhat*uhat)+(vhat*vhat))
        rl=uhat*xhat+vhat*yhat
        qx(2,iel)=qx(2,iel)*rl*uhat/rt
        qy(2,iel)=qy(2,iel)*rl*vhat/rt

        ! Cut off checks
        IF ( (den.LE.zerocut)  ) THEN
              qx(2,iel)=0.0_rlk
              qy(2,iel)=0.0_rlk
        ENDIF

        IF ( (rb.LE.zerocut)  ) THEN
              qx(2,iel)=0.0_rlk
              qy(2,iel)=0.0_rlk
        ENDIF

        IF ( (rt.LE.zerocut)  ) THEN
              qx(2,iel)=0.0_rlk
              qy(2,iel)=0.0_rlk
        ENDIF

        IF ( (rl.LE.zerocut)  ) THEN
              qx(2,iel)=0.0_rlk
              qy(2,iel)=0.0_rlk
        ENDIF

     ! For n1 = 3, n2 = 4
        n1=3_ink
        n2=4_ink
        w1_s=elx(n1,iel)
        w5_s=ely(n1,iel)
        w2_s=elx(n2,iel)
        w6_s=ely(n2,iel)
        w3_s=0.5_rlk*(w2_s+w1_s)
        w7_s=0.5_rlk*(w6_s+w5_s)
        den=SQRT((w4_s-w3_s)*(w4_s-w3_s)+(w8_s-w7_s)*(w8_s-w7_s))
        xhat=(w8_s-w7_s)/den
        yhat=(w3_s-w4_s)/den
        rb=SQRT((w2_s-w1_s)*(w2_s-w1_s)+(w6_s-w5_s)*(w6_s-w5_s))
        uhat=(w2_s-w1_s)/rb
        vhat=(w6_s-w5_s)/rb
        rr=xhat*uhat+yhat*vhat
        rr=-SIGN(1.0_rlk,rr)
        xhat=xhat*den*rr
        yhat=yhat*den*rr
        uhat=elu(n2,iel)-elu(n1,iel)
        vhat=elv(n2,iel)-elv(n1,iel)
        rt=SQRT((uhat*uhat)+(vhat*vhat))
        rl=uhat*xhat+vhat*yhat
        qx(3,iel)=qx(3,iel)*rl*uhat/rt
        qy(3,iel)=qy(3,iel)*rl*vhat/rt

        ! Cut off checks
        IF ( (den.LE.zerocut)  ) THEN
              qx(3,iel)=0.0_rlk
              qy(3,iel)=0.0_rlk
        ENDIF

        IF ( (rb.LE.zerocut)  ) THEN
              qx(3,iel)=0.0_rlk
              qy(3,iel)=0.0_rlk
        ENDIF

        IF ( (rt.LE.zerocut)  ) THEN
              qx(3,iel)=0.0_rlk
              qy(3,iel)=0.0_rlk
        ENDIF

        IF ( (rl.LE.zerocut)  ) THEN
              qx(3,iel)=0.0_rlk
              qy(3,iel)=0.0_rlk
        ENDIF


     ! For n1 = 4, n2 = 1
        n1=4_ink
        n2=1_ink
        w1_s=elx(n1,iel)
        w5_s=ely(n1,iel)
        w2_s=elx(n2,iel)
        w6_s=ely(n2,iel)
        w3_s=0.5_rlk*(w2_s+w1_s)
        w7_s=0.5_rlk*(w6_s+w5_s)
        den=SQRT((w4_s-w3_s)*(w4_s-w3_s)+(w8_s-w7_s)*(w8_s-w7_s))
        xhat=(w8_s-w7_s)/den
        yhat=(w3_s-w4_s)/den
        rb=SQRT((w2_s-w1_s)*(w2_s-w1_s)+(w6_s-w5_s)*(w6_s-w5_s))
        uhat=(w2_s-w1_s)/rb
        vhat=(w6_s-w5_s)/rb
        rr=xhat*uhat+yhat*vhat
        rr=-SIGN(1.0_rlk,rr)
        xhat=xhat*den*rr
        yhat=yhat*den*rr
        uhat=elu(n2,iel)-elu(n1,iel)
        vhat=elv(n2,iel)-elv(n1,iel)
        rt=SQRT((uhat*uhat)+(vhat*vhat))
        rl=uhat*xhat+vhat*yhat
        qx(4,iel)=qx(4,iel)*rl*uhat/rt
        qy(4,iel)=qy(4,iel)*rl*vhat/rt

        ! Cut off checks
        IF ( (den.LE.zerocut)  ) THEN
              qx(4,iel)=0.0_rlk
              qy(4,iel)=0.0_rlk
        ENDIF

        IF ( (rb.LE.zerocut)  ) THEN
              qx(4,iel)=0.0_rlk
              qy(4,iel)=0.0_rlk
        ENDIF

        IF ( (rt.LE.zerocut)  ) THEN
              qx(4,iel)=0.0_rlk
              qy(4,iel)=0.0_rlk
        ENDIF

        IF ( (rl.LE.zerocut)  ) THEN
              qx(4,iel)=0.0_rlk
              qy(4,iel)=0.0_rlk
        ENDIF

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
