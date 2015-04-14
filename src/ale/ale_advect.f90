
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

MODULE ale_advect_mod

  USE kinds_mod,ONLY: ink,rlk,lok

  IMPLICIT NONE

  PRIVATE :: update_e_basis,update_e_var,update_n_basis,update_n_var,   &
&            aleadvect_e,aleadvect_n  
  PUBLIC  :: aleadvect

CONTAINS

  SUBROUTINE aleadvect(id1,id2,nshape,nel,nnod,nsz,ielel,ielsd,ielnd,   &
&                      indstatus,indtype,dencut,cut,cutv,cutm,elv0ndm1, &
&                      elm0ndm0,elr0ndv0,ndv1,elv1,elm1,elr1,cnv0,cnm1, &
&                      dfv,dfm,cnm0,flux,work1,work2,zactive)

    ! Argument list
    INTEGER(KIND=ink),                      INTENT(IN)    :: id1,id2,   &
&                                                            nshape,nsz,&
&                                                            nel,nnod
    REAL(KIND=rlk),                         INTENT(IN)    :: dencut,cut
    INTEGER(KIND=ink),DIMENSION(nshape,nel),INTENT(IN)    :: ielel,     &
&                                                            ielsd,     &
&                                                            ielnd
    INTEGER(KIND=ink),DIMENSION(nnod),      INTENT(IN)    :: indstatus, &
&                                                            indtype
    REAL(KIND=rlk),   DIMENSION(nsz),       INTENT(OUT)   :: cutv,cutm, &
&                                                            elv0ndm1,  &
&                                                            elm0ndm0,  &
&                                                            elr0ndv0,  &
&                                                            ndv1
    REAL(KIND=rlk),   DIMENSION(nel),       INTENT(INOUT) :: elv1,elm1, &
&                                                            elr1
    REAL(KIND=rlk),   DIMENSION(nshape,nel),INTENT(INOUT) :: cnv0,cnm1, &
&                                                            dfv,dfm
    REAL(KIND=rlk),   DIMENSION(nshape,nel),INTENT(OUT)   :: cnm0,flux, &
&                                                            work1,work2
    LOGICAL(KIND=lok),DIMENSION(nnod),      INTENT(OUT)   :: zactive

    ! Advect element quantities
    CALL aleadvect_e(id1,id2,nshape,nel,elv0ndm1(1),elm0ndm0(1),        &
&                    elr0ndv0(1),elv1(1),elm1(1),elr1(1),cutv(1),       &
&                    cutm(1),cnv0(1,1),cnm1(1,1),dfv(1,1),dfm(1,1),     &
&                    flux(1,1),ielel(1,1),ielsd(1,1),work1(1,1),        &
&                    work2(1,1))

    ! Advect nodal quantities
    CALL aleadvect_n(id1,id2,nshape,nel,nnod,nsz,ielel(1,1),ielsd(1,1), &
&                    ielnd(1,1),indstatus(1),indtype(1),dencut,cut,     &
&                    cutv(1),cutm(1),elr0ndv0(1),ndv1(1),elm0ndm0(1),   &
&                    elv0ndm1(1),elv1(1),cnv0(1,1),cnm0(1,1),cnm1(1,1), &
&                    dfv(1,1),dfm(1,1),work1(1,1),work2(1,1),flux(1,1), &
&                    zactive(1))

  END SUBROUTINE aleadvect

  SUBROUTINE aleadvect_e(id1,id2,nshape,nel,elvpr,elmpr,elrpr,elv,elm,  &
&                        elr,cutv,cutm,cnv,cnm,delv,delm,flux,ielel,    &
&                        ielsd,work1,work2)

    ! Argument list
    INTEGER(KIND=ink),                      INTENT(IN)    :: id1,id2,   &
&                                                            nel,nshape
    INTEGER(KIND=ink),DIMENSION(nshape,nel),INTENT(IN)    :: ielel,ielsd
    REAL(KIND=rlk),   DIMENSION(nel),       INTENT(OUT)   :: elvpr,     &
&                                                            cutv,cutm, &
&                                                            elrpr,     &
&                                                            elmpr
    REAL(KIND=rlk),   DIMENSION(nel),       INTENT(INOUT) :: elv,elm,elr
    REAL(KIND=rlk),   DIMENSION(nshape,nel),INTENT(IN)    :: delv,cnv,  &
&                                                            cnm
    REAL(KIND=rlk),   DIMENSION(nshape,nel),INTENT(OUT)   :: delm,flux, &
&                                                            work1,work2

    ! update element basis variables
    CALL update_e_basis(id1,id2,nshape,nel,elvpr(1),elmpr(1),elrpr(1),  &
&                       elv(1),elm(1),elr(1),cutv(1),cutm(1),cnv(1,1),  &
&                       cnm(1,1),delv(1,1),delm(1,1),ielel(1,1),        &
&                       ielsd(1,1),work1(1,1),work2(1,1))

    ! update element independent variables
    CALL update_e_var(id1,id2,nshape,nel,ielel(1,1),ielsd(1,1),elvpr(1),&
&                     elmpr(1),elv(1),elm(1),cutv(1),cutm(1),cnv(1,1),  &
&                     cnm(1,1),delv(1,1),delm(1,1),flux(1,1),work1(1,1))

  END SUBROUTINE aleadvect_e

  SUBROUTINE aleadvect_n(id1,id2,nshape,nel,nnod,nsz,ielel,ielsd,ielnd, &
&                        indstatus,indtype,dencut,cut,cutv,cutm,ndv0,   &
&                        ndv1,ndm0,elv0ndm1,elv1,cnv0,cnm0,cnm1,dfv,dfm,&
&                        dcv,dcm,flux,zactive)

    ! Argument list
    INTEGER(KIND=ink),                      INTENT(IN)    :: nshape,nel,&
&                                                            nnod,nsz,  &
&                                                            id1,id2
    REAL(KIND=rlk),                         INTENT(IN)    :: dencut,cut
    INTEGER(KIND=ink),DIMENSION(nshape,nel),INTENT(IN)    :: ielel,     &
&                                                            ielsd,     &
&                                                            ielnd    
    INTEGER(KIND=ink),DIMENSION(nnod),      INTENT(IN)    :: indstatus, &
&                                                            indtype
    REAL(KIND=rlk),   DIMENSION(nel),       INTENT(IN)    :: elv1
    REAL(KIND=rlk),   DIMENSION(nnod),      INTENT(OUT)   :: ndv0,ndm0, &
&                                                            ndv1,cutv, &
&                                                            cutm
    REAL(KIND=rlk),   DIMENSION(nshape,nel),INTENT(IN)    :: cnv0
    REAL(KIND=rlk),   DIMENSION(nshape,nel),INTENT(INOUT) :: dfv,dfm,   &
&                                                            cnm1    
    REAL(KIND=rlk),   DIMENSION(nshape,nel),INTENT(OUT)   :: dcv,dcm,   &
&                                                            cnm0,flux
    REAL(KIND=rlk),   DIMENSION(nsz),       INTENT(INOUT) :: elv0ndm1
    LOGICAL(KIND=lok),DIMENSION(nnod),      INTENT(OUT)   :: zactive

    ! update nodal basis variables
    CALL update_n_basis(id1,id2,nshape,nel,nnod,nsz,dencut,cut,         &
&                       ielel(1,1),ielsd(1,1),ielnd(1,1),dfv(1,1),      &
&                       dfm(1,1),dcv(1,1),dcm(1,1),cnm0(1,1),cnm1(1,1), &
&                       cutv(1),cutm(1),ndv0(1),ndv1(1),ndm0(1),        &
&                       elv0ndm1(1),elv1(1),flux(1,1))

    ! update nodal independent variables
    CALL update_n_var(nshape,nel,nnod,ielel(1,1),ielsd(1,1),ielnd(1,1), &
&                     indstatus(1),indtype(1),ndv0(1),ndm0(1),ndv1(1),  &
&                     elv0ndm1(1),cutv(1),cutm(1),cnv0(1,1),cnm0(1,1),  &
&                     dcv(1,1),dcm(1,1),flux(1,1),dfv(1,1),dfm(1,1),    &
&                     zactive(1))

  END SUBROUTINE aleadvect_n

  SUBROUTINE update_e_basis(id1,id2,nshape,nel,elvpr,elmpr,elrpr,elv,   &
&                           elm,elr,cutv,cutm,cnv,cnm,delv,delm,ielel,  &
&                           ielsd,totv,totm)

    USE reals_mod,        ONLY: dencut,zerocut
    USE ale_advectors_mod,ONLY: flux_c1_VL,sum_flux

    ! Argument list
    INTEGER(KIND=ink),                      INTENT(IN)    :: id1,id2,   &
&                                                            nshape,nel
    INTEGER(KIND=ink),DIMENSION(nshape,nel),INTENT(IN)    :: ielel,ielsd
    REAL(KIND=rlk),   DIMENSION(nel),       INTENT(OUT)   :: cutv,cutm, &
&                                                            elvpr,totv,&
&                                                            elmpr,totm,&
&                                                            elrpr
    REAL(KIND=rlk),   DIMENSION(nel),       INTENT(INOUT) :: elv,elm,elr
    REAL(KIND=rlk),   DIMENSION(nshape,nel),INTENT(IN)    :: delv,cnv,  &
&                                                            cnm
    REAL(KIND=rlk),   DIMENSION(nshape,nel),INTENT(OUT)   :: delm
    ! Local
    INTEGER(KIND=ink) :: iel
  
    ! calculate total volume flux
    CALL sum_flux(id1,id2,nshape,nel,nel,ielel(1,1),ielsd(1,1),         &
&                 delv(1,1),totv(1))

    ! construct mass flux
    CALL flux_c1_VL(id1,id2,nshape,nel,nel,ielel(1,1),ielsd(1,1),       &
&                   cnv(1,1),delv(1,1),elr(1),delm(1,1))

    ! calculate total mass flux
    CALL sum_flux(id1,id2,nshape,nel,nel,ielel(1,1),ielsd(1,1),         &
&                 delm(1,1),totm(1))

    ! update
    DO iel=1,nel
      ! store basis variables
      elvpr(iel)=elv(iel)
      elmpr(iel)=elm(iel)
      elrpr(iel)=elr(iel)
      ! construct cut-off's
      cutv(iel)=zerocut
      cutm(iel)=elvpr(iel)*dencut
      ! volume
      elv(iel)=elv(iel)+totv(iel)
      ! mass
      elm(iel)=elm(iel)+totm(iel)
      ! density
      elr(iel)=elm(iel)/elv(iel)
    ENDDO

  END SUBROUTINE update_e_basis

  SUBROUTINE update_e_var(id1,id2,nshape,nel,ielel,ielsd,elvpr,elmpr,   &
&                         elv,elm,cutv,cutm,cnv,cnm,delv,delm,flux,     &
&                         tflux)

    USE ale_advectors_mod,ONLY: flux_c1_VL,update_c1
    USE pointers_mod,     ONLY: ein

    ! Argument list
    INTEGER(KIND=ink),                      INTENT(IN)  :: id1,id2,nel, &
&                                                          nshape
    INTEGER(KIND=ink),DIMENSION(nshape,nel),INTENT(IN)  :: ielel,ielsd
    REAL(KIND=rlk),   DIMENSION(nel),       INTENT(IN)  :: elvpr,elmpr, &
&                                                          elv,elm,cutv,&
&                                                          cutm
    REAL(KIND=rlk),   DIMENSION(nshape,nel),INTENT(IN)  :: cnv,cnm,delv,&
&                                                          delm
    REAL(KIND=rlk),   DIMENSION(nshape,nel),INTENT(OUT) :: flux
    REAL(KIND=rlk),   DIMENSION(nel),       INTENT(OUT) :: tflux

    ! internal energy (mass weighted)
    CALL flux_c1_VL(id1,id2,nshape,nel,nel,ielel(1,1),ielsd(1,1),       &
&                   cnm(1,1),delm(1,1),ein(1),flux(1,1))
    CALL update_c1(id1,id2,nshape,nel,nel,ielel(1,1),ielsd(1,1),        &
&                  elmpr(1),elm(1),cutm(1),flux(1,1),tflux(1),ein(1))

  END SUBROUTINE update_e_var

  SUBROUTINE update_n_basis(id1,id2,nshape,nel,nnod,nsz,dencut,cut,     &
&                           ielel,ielsd,ielnd,delv,delm,dndv,dndm,cnm0, &
&                           cnm1,cutv,cutm,ndv0,ndv1,ndm0,elv0ndm1,elv1,&
&                           flux)

    ! Argument list
    INTEGER(KIND=ink),                      INTENT(IN)    :: id1,id2,   &
&                                                            nel,nnod,  &
&                                                            nsz,nshape
    REAL(KIND=rlk),                         INTENT(IN)    :: dencut,cut
    INTEGER(KIND=ink),DIMENSION(nshape,nel),INTENT(IN)    :: ielel,     &
&                                                            ielsd,     &
&                                                            ielnd
    REAL(KIND=rlk),   DIMENSION(nel),       INTENT(IN)    :: elv1
    REAL(KIND=rlk),   DIMENSION(nshape,nel),INTENT(IN)    :: delv,delm
    REAL(KIND=rlk),   DIMENSION(nshape,nel),INTENT(OUT)   :: dndv,dndm, &
&                                                            cnm0,flux
    REAL(KIND=rlk),   DIMENSION(nnod),      INTENT(OUT)   :: ndv0,ndv1, &
&                                                            ndm0,cutv, &
&                                                            cutm
    REAL(KIND=rlk),   DIMENSION(nsz),       INTENT(INOUT) :: elv0ndm1
    REAL(KIND=rlk),   DIMENSION(nshape,nel),INTENT(INOUT) :: cnm1
    ! Local
    INTEGER(KIND=ink) :: ind,iel,i1,i2,ie1,ie2,is1,is2
    REAL(KIND=rlk)    :: w1,w2,w3

    ! initialise
    DO ind=1,nnod
      ndv0(ind)=0.0_rlk
      ndv1(ind)=0.0_rlk
      ndm0(ind)=0.0_rlk
    ENDDO

    ! construct pre/post nodal volumes and pre nodal/corner mass
    DO iel=1,nel
      w1=0.25_rlk*elv0ndm1(iel)
      w2=0.25_rlk*elv1(iel)
      w3=cnm1(1,iel)
      cnm0(1,iel)=w3
      ind=ielnd(1,iel)
      ndv0(ind)=ndv0(ind)+w1
      ndv1(ind)=ndv1(ind)+w2
      ndm0(ind)=ndm0(ind)+w3
      w3=cnm1(2,iel)
      cnm0(2,iel)=w3
      ind=ielnd(2,iel)
      ndv0(ind)=ndv0(ind)+w1
      ndv1(ind)=ndv1(ind)+w2
      ndm0(ind)=ndm0(ind)+w3
      w3=cnm1(3,iel)
      cnm0(3,iel)=w3
      ind=ielnd(3,iel)
      ndv0(ind)=ndv0(ind)+w1
      ndv1(ind)=ndv1(ind)+w2
      ndm0(ind)=ndm0(ind)+w3
      w3=cnm1(4,iel)
      cnm0(4,iel)=w3
      ind=ielnd(4,iel)
      ndv0(ind)=ndv0(ind)+w1
      ndv1(ind)=ndv1(ind)+w2
      ndm0(ind)=ndm0(ind)+w3
    ENDDO

    ! construct volume and mass flux
    flux=0.0_rlk
    DO i1=id1,id2
      i2=i1+2_ink
      DO iel=1,nel
        ie1=ielel(i1,iel)
        ie2=ielel(i2,iel)
        is1=ielsd(i1,iel)
        is2=ielsd(i2,iel)
        w1=delv(is1,ie1)-delv(i1,iel)
        w2=delv(is2,ie2)-delv(i2,iel)
        w3=0.25_rlk*(w1-w2)
        dndv(i1,iel)=w3
        dndv(i2,iel)=w3
        w1=delm(is1,ie1)-delm(i1,iel)
        w2=delm(is2,ie2)-delm(i2,iel)
        w3=0.25_rlk*(w1-w2)
        dndm(i1,iel)=w3
        dndm(i2,iel)=w3
        w3=0.25_rlk*(w1+w2)
        flux(1,iel)=flux(1,iel)+w3
        flux(2,iel)=flux(2,iel)+w3
        flux(3,iel)=flux(3,iel)+w3
        flux(4,iel)=flux(4,iel)+w3
      ENDDO
    ENDDO

    ! construct post nodal/corner mass
    DO ind=1,nnod
      elv0ndm1(ind)=ndm0(ind)
    ENDDO
    DO iel=1,nel
      cnm1(1,iel)=cnm1(1,iel)+flux(1,iel)
      cnm1(2,iel)=cnm1(2,iel)+flux(2,iel)
      ind=ielnd(1,iel)
      elv0ndm1(ind)=elv0ndm1(ind)+flux(1,iel)
      ind=ielnd(2,iel)
      elv0ndm1(ind)=elv0ndm1(ind)+flux(2,iel)
    ENDDO

    ! construct cut-offs
    DO ind=1,nnod
      cutv(ind)=cut
      cutm(ind)=dencut*ndv0(ind)
    ENDDO

  END SUBROUTINE update_n_basis

  SUBROUTINE update_n_var(nshape,nel,nnod,ielel,ielsd,ielnd,indstatus,  &
&                         indtype,ndv0,ndm0,ndv1,ndm1,cutv,cutm,cnv,cnm,&
&                         delv,delm,flux,eluv,tflux,zactive)

    USE ale_advectors_mod,ONLY: flux_n1_VL,update_n1
    USE pointers_mod,     ONLY: ndu,ndv
    USE utilities_mod,    ONLY: gather

    ! Argument list
    INTEGER(KIND=ink),                      INTENT(IN)  :: nshape,nel,  &
&                                                          nnod
    INTEGER(KIND=ink),DIMENSION(nshape,nel),INTENT(IN)  :: ielel,ielsd, &
&                                                          ielnd
    INTEGER(KIND=ink),DIMENSION(nnod),      INTENT(IN)  :: indstatus,   &
&                                                          indtype
    REAL(KIND=rlk),   DIMENSION(nnod),      INTENT(IN)  :: ndv0,ndm0,   &
&                                                          ndv1,ndm1,   &
&                                                          cutv,cutm
    REAL(KIND=rlk),   DIMENSION(nshape,nel),INTENT(IN)  :: cnv,cnm,delv,&
&                                                          delm
    REAL(KIND=rlk),   DIMENSION(nshape,nel),INTENT(OUT) :: flux,eluv
    REAL(KIND=rlk),   DIMENSION(nnod),      INTENT(OUT) :: tflux
    LOGICAL(KIND=lok),DIMENSION(nnod),      INTENT(OUT) :: zactive
    ! Local
    INTEGER(KIND=ink) :: ind

    ! momentum (mass weighted)
    CALL gather(nshape,nel,nnod,ielnd(1,1),ndu(1),eluv(1,1))
    DO ind=1,nnod
      IF ((indstatus(ind).GT.0_ink).AND.(indtype(ind).NE.-1_ink).AND.   &
&         (indtype(ind).NE.-3_ink)) THEN
        zactive(ind)=.TRUE._lok
      ELSE
        zactive(ind)=.FALSE._lok
      ENDIF
    ENDDO
    CALL flux_n1_VL(nshape,nel,nel,ielel(1,1),ielsd(1,1),cnm(1,1),      &
&                   delm(1,1),eluv(1,1),flux(1,1))
    CALL update_n1(nshape,nel,nel,nnod,ielnd(1,1),ndm0(1),ndm1(1),      &
&                  cutm(1),zactive(1),flux(1,1),tflux(1),ndu(1))
    CALL gather(nshape,nel,nnod,ielnd(1,1),ndv(1),eluv(1,1))
    DO ind=1,nnod
      IF ((indstatus(ind).GT.0_ink).AND.(indtype(ind).NE.-2_ink).AND.   &
&         (indtype(ind).NE.-3_ink)) THEN
        zactive(ind)=.TRUE._lok
      ELSE
        zactive(ind)=.FALSE._lok
      ENDIF
    ENDDO
    CALL flux_n1_VL(nshape,nel,nel,ielel(1,1),ielsd(1,1),cnm(1,1),      &
&                   delm(1,1),eluv(1,1),flux(1,1))
    CALL update_n1(nshape,nel,nel,nnod,ielnd(1,1),ndm0(1),ndm1(1),      &
&                  cutm(1),zactive(1),flux(1,1),tflux(1),ndv(1))

  END SUBROUTINE update_n_var

END MODULE ale_advect_mod
