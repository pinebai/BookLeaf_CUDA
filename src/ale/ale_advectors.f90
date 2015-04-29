
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

MODULE ale_advectors_mod

  USE kinds_mod,ONLY: ink,rlk,lok

  IMPLICIT NONE

  PUBLIC :: flux_c1_VL,flux_n1_VL,sum_flux,update_c1,update_n1

CONTAINS

  SUBROUTINE flux_c1_VL(iD1,iD2,iShape,iLSize,iASize,iElEl,iElSd,       &
&                       rCorner,rDel,rVar,rFlux)

    ! Argument list
    INTEGER(KIND=ink),                         INTENT(IN)  :: iD1,iD2,  &
&                                                             iShape,   &
&                                                             iLSize,   &
&                                                             iASize
    INTEGER(KIND=ink),DIMENSION(iShape,iASize),INTENT(IN)  :: iElEl,    &
&                                                             iElSd
    REAL(KIND=rlk),   DIMENSION(iShape,iASize),INTENT(IN)  :: rCorner,  &
&                                                             rDel
    REAL(KIND=rlk),   DIMENSION(iASize),       INTENT(IN)  :: rVar
    REAL(KIND=rlk),   DIMENSION(iShape,iASize),INTENT(OUT) :: rFlux
    ! Local
    INTEGER(KIND=ink) :: i1,i2,j1,j2,iEl,iE1,iE2
    REAL(KIND=rlk)    :: r1,r2,r3,r4,w1,w2,w3,w4,w5,w6,w7,w8,rV,rGrad

    ! initialise
    rFlux=0.0_rlk

    ! construct flux
    DO i1=iD1,iD2
      i2=i1+2_ink
      DO iEl=1,iLSize
        iE2=iElEl(i2,iEl)
        j2=iElSd(i2,iEl)
        j1=MOD(i2,iShape)+1_ink
        r3=rCorner(i2,iEl)+rCorner(j1,iEl)
        j1=MOD(j2,iShape)+1_ink
        w5=r3+rCorner(j2,iE2)+rCorner(j1,iE2)
        iE1=iElEl(i1,iEl)
        j2=iElSd(i1,iEl)
        j1=i1+1_ink
        r4=rCorner(i1,iEl)+rCorner(j1,iEl)
        j1=MOD(j2,iShape)+1_ink
        w6=r4+rCorner(j2,iE1)+rCorner(j1,iE1)
        rV=rVar(iEl)
        r1=rDel(i1,iEl)
        r2=rDel(i2,iEl)
        w1=rV-rVar(iE2)
        w2=rVar(iE1)-rV
        w3=ABS(w1)
        w4=ABS(w2)
        w7=SIGN(1.0_rlk,w2)
        w8=(w4*w6*w6+w3*w5*w5)/(w5*w6*(w5+w6))
        rGrad=w7*MIN(ABS(w8),w3/w5,w4/w6)
        IF (w1*w2.LE.0.0_rlk) rGrad=0.0_rlk
        r1=r1*(rV+rGrad*(r3-0.5_rlk*r1))
        r2=r2*(rV-rGrad*(r4-0.5_rlk*r2))
        rFlux(i1,iel)=r1
        rFlux(i2,iel)=r2
      ENDDO
    ENDDO

  END SUBROUTINE flux_c1_VL  

  SUBROUTINE flux_n1_VL(iShape,iLSize,iASize,iElEl,iElSd,rCorner,rDel,  &
&                       rVar,rFlux)

    ! Argument list
    INTEGER(KIND=ink),                         INTENT(IN)  :: iShape,   &
&                                                             iLSize,   &
&                                                             iASize
    INTEGER(KIND=ink),DIMENSION(iShape,iASize),INTENT(IN)  :: iElEl,    &
&                                                             iElSd
    REAL(KIND=rlk),   DIMENSION(iShape,iASize),INTENT(IN)  :: rCorner,  &
&                                                             rDel,rVar
    REAL(KIND=rlk),   DIMENSION(iShape,iASize),INTENT(OUT) :: rFlux
    ! Local
    INTEGER(KIND=ink) :: iFaceL,iFaceR,iCorner,iEl,iElL,iElR,iSdL,iSdR, &
&                        iLNdL,iLNdR,iLNNdL,iLNNdR,ii
    REAL(KIND=rlk)    :: w1,w2,w3,w4,w5,w6,w7,w8,rD,rGrad,rV

    ! initialise
    rFlux=0.0_rlk

    ! construct flux
    DO iFaceL=1,2
      iFaceR=iFaceL+2_ink
      DO iCorner=1,2
        iLNdL=iFaceL+iCorner-1_ink
        iLNdR=MOD(iFaceR-iCorner+1,iShape)+1_ink
        DO iEl=1,iLSize
          rD=0.0_rlk
          iElL=iElEl(iFaceL,iEl)
          iElR=iElEl(iFaceR,iEl)
          iSdL=iElSd(iFaceL,iEl)
          iSdR=iElSd(iFaceR,iEl)
          ii=iFaceL+2_ink*(iCorner-1_ink)
          IF (rDel(ii,iEl).GT.0.0_rlk) THEN
            iLNNdL=MOD(iSdL+iCorner,iShape)+1_ink
            iLNNdR=MOD(iSdL-iCorner+1,iShape)+1_ink
            rV=rVar(iLndL,iEl)
            rD=rCorner(iLNdL,iEl)-0.5_rlk*rDel(ii,iEl)
            w5=rCorner(iLNdL,iEl)+rCorner(iLNdR,iEl)
            w6=rCorner(iLNNdL,iElL)+rCorner(iLNNdR,iElL)
            w1=rV-rVar(iLNNdL,iElL)
            w2=rVar(iLNdR,iEl)-rV
            w3=ABS(w1)
            w4=ABS(w2)
            w7=SIGN(1.0_rlk,w2)
            w8=(w4*w6*w6+w3*w5*w5)/(w5*w6*(w5+w6))
            rGrad=w7*MIN(ABS(w8),w3/w5,w4/w6)
            IF (w1*w2.LE.0.0_rlk) rGrad=0.0_rlk
            rD=rDel(ii,iel)*(rV+rGrad*rD)
          ENDIF
          IF (rDel(ii,iEl).LT.0.0_rlk) THEN
            iLNNdL=MOD(iSdR+iCorner-2,iShape)+1_ink
            iLNNdR=MODULO(iSdR-iCorner-1,iShape)+1_ink
            rV=rVar(iLNdR,iEl)
            rD=rCorner(iLNdR,iEl)+0.5_rlk*rDel(ii,iEl)
            w5=rCorner(iLNdL,iEl)+rCorner(iLNdR,iEl)
            w6=rCorner(iLNNdL,iElR)+rCorner(iLNNdR,iElR)
            w1=rV-rVar(iLNdL,iEl)
            w2=rVar(iLNNdR,iElR)-rV
            w3=ABS(w1)
            w4=ABS(w2)
            w7=SIGN(1.0_rlk,w2)
            w8=(w4*w6*w6+w3*w5*w5)/(w5*w6*(w5+w6))
            rGrad=-w7*MIN(ABS(w8),w3/w5,w4/w6)
            IF (w1*w2.LE.0.0_rlk) rGrad=0.0_rlk
            rD=rDel(ii,iel)*(rV+rGrad*rD)
          ENDIF
          rFlux(iLNdL,iEl)=rFlux(iLNdL,iEl)-rD
          rFlux(iLNdR,iEl)=rFlux(iLNdR,iEl)+rD
        ENDDO
      ENDDO
    ENDDO

  END SUBROUTINE flux_n1_VL

  SUBROUTINE update_c1(iD1,iD2,iShape,iLSize,iASize,iElEl,iElSd,rBase0, &
&                      rBase1,rCutOff,rFlux,rTotFlux,rVar)

    ! Argument list
    INTEGER(KIND=ink),                         INTENT(IN)    :: iD1,iD2,&
&                                                               iShape, &
&                                                               iLSize, &
&                                                               iASize
    INTEGER(KIND=ink),DIMENSION(iShape,iASize),INTENT(IN)    :: iElEl,  &
&                                                               iElSd
    REAL(KIND=rlk),   DIMENSION(iASize),       INTENT(IN)    :: rBase0, &
&                                                               rBase1, &
&                                                               rCutOff
    REAL(KIND=rlk),   DIMENSION(iShape,iASize),INTENT(IN)    :: rFlux
    REAL(KIND=rlk),   DIMENSION(iASize),       INTENT(OUT)   :: rTotFlux
    REAL(KIND=rlk),   DIMENSION(iASize),       INTENT(INOUT) :: rVar
    ! Local
    INTEGER(KIND=ink) :: iEl

    ! calculate total flux
    CALL sum_flux(iD1,iD2,iShape,iLSize,iASize,iElEl,iElSd,rFlux,       &
&                 rTotFlux)

    ! update variable  
    DO iEl=1,iLSize
      IF (rBase1(iEl).GT.rCutOff(iEl)) THEN
        rVar(iEl)=(rVar(iEl)*rBase0(iEl)+rTotFlux(iEl))/rBase1(iEl)
      ENDIF
    ENDDO

  END SUBROUTINE update_c1  

  SUBROUTINE update_n1(iShape,iUSize,iCSize,iNSize,iElNd,rBase0,rBase1, &
&                      rCut,zActive,rFlux,rTotFlux,rVar)

    ! Argument list
    INTEGER(KIND=ink),                         INTENT(IN)    :: iShape, &
&                                                               iUSize, &
&                                                               iCSize, &
&                                                               iNSize
    INTEGER(KIND=ink),DIMENSION(iShape,iCSize),INTENT(IN)    :: iElNd
    REAL(KIND=rlk),   DIMENSION(iNSize),       INTENT(IN)    :: rBase0, &
&                                                               rBase1, &
&                                                               rCut
    LOGICAL(KIND=lok),DIMENSION(iNSize),       INTENT(IN)    :: zActive
    REAL(KIND=rlk),   DIMENSION(iShape,iCSize),INTENT(IN)    :: rFlux
    REAL(KIND=rlk),   DIMENSION(iNSize),       INTENT(OUT)   :: rTotFlux
    REAL(KIND=rlk),   DIMENSION(iNSize),       INTENT(INOUT) :: rVar
    ! Local
    INTEGER(KIND=ink) :: iEl,iNd,ii

    ! construct total flux
    rTotFlux=0.0_rlk
    DO iEl=1,iCSize
      DO ii=1,iShape
        iNd=iElNd(ii,iEl)
        rTotFlux(iNd)=rTotFlux(iNd)+rFlux(ii,iEl)
      ENDDO
    ENDDO

    ! update variable
    DO iNd=1,iUSize
      IF (zActive(iNd).AND.(rBase1(iNd).GT.rCut(iNd))) THEN
        rVar(iNd)=(rVar(iNd)*rBase0(iNd)+rTotFlux(iNd))/rBase1(iNd)
      ENDIF
    ENDDO

  END SUBROUTINE update_n1

  SUBROUTINE sum_flux(iD1,iD2,iShape,iLSize,iASize,iElEl,iElSd,rFlux,   &
&                     rTotFlux)

    ! Argument list
    INTEGER(KIND=ink),                         INTENT(IN)    :: iD1,iD2,&
&                                                               iShape, &
&                                                               iLSize, &
&                                                               iASize
    INTEGER(KIND=ink),DIMENSION(iShape,iASize),INTENT(IN)    :: iElEl,  &
&                                                               iElSd
    REAL(KIND=rlk),   DIMENSION(iShape,iASize),INTENT(IN)    :: rFlux
    REAL(KIND=rlk),   DIMENSION(iASize),       INTENT(OUT)   :: rTotFlux
    ! Local
    INTEGER(KIND=ink) :: i1,i2,j1,j2,iEl,iE1,iE2
    REAL(KIND=rlk)    :: w1,w2

    rTotFlux=0.0_rlk
    DO i1=iD1,iD2
      i2=i1+2_ink
      DO iEl=1,iLSize
        iE1=iElEl(i1,iEl)
        iE2=iElEl(i2,iEl)
        j1=iElSd(i1,iEl)
        j2=iElSd(i2,iEl)
        w1=rFlux(j1,iE1)
        w2=rFlux(j2,iE2)
        IF (iE1.EQ.iEl) w1=0.0_rlk
        IF (iE2.EQ.iEl) w2=0.0_rlk
        rTotFlux(iEl)=rTotFlux(iEl)-rFlux(i1,iEl)-rFlux(i2,iEl)+w1+w2
      ENDDO
    ENDDO

  END SUBROUTINE sum_flux

END MODULE ale_advectors_mod
