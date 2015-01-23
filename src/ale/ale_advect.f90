
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

  USE kinds_mod,ONLY: ink,rlk

  IMPLICIT NONE

  PRIVATE :: update_e_basis,update_e_var,update_n_basis,update_n_var
  PUBLIC  :: aleadvect

CONTAINS

  SUBROUTINE aleadvect(id1,id2,nshape,nel,nnod,elvpr,elmpr,elrpr,elm,   &
&                      elr,cutv,cutm,cnv,cnm,delv,delm,flux,ielel,ielsd,&
&                      work1,work2)

    USE pointers_mod,ONLY: elv=>elvol

    ! Argument list
    INTEGER(KIND=ink),                      INTENT(IN)    :: id1,id2,   &
&                                                            nnod,nel,  &
&                                                            nshape
    INTEGER(KIND=ink),DIMENSION(nshape,nel),INTENT(IN)    :: ielel,ielsd
    REAL(KIND=rlk),   DIMENSION(nel),       INTENT(OUT)   :: elvpr,     &
&                                                            cutv,cutm, &
&                                                            elrpr,     &
&                                                            elmpr
    REAL(KIND=rlk),   DIMENSION(nel),       INTENT(INOUT) :: elm,elr
    REAL(KIND=rlk),   DIMENSION(nshape,nel),INTENT(IN)    :: delv,cnv
    REAL(KIND=rlk),   DIMENSION(nshape,nel),INTENT(OUT)   :: delm,cnm,  &
&                                                            flux,work1,&
&                                                            work2

    ! update element basis variables
    CALL update_e_basis(id1,id2,nshape,nel,nnod,elvpr,elmpr,elrpr,elv,  &
&                       elm,elr,cutv,cutm,cnv,cnm,delv,delm,ielel,ielsd,&
&                       work1,work2)

    ! update element independent variables
    CALL update_e_var()

    ! update nodal basis variables
    CALL update_n_basis()

    ! update nodal independent variables
    CALL update_n_var()

  END SUBROUTINE aleadvect

  SUBROUTINE update_e_basis(id1,id2,nshape,nel,nnod,elvpr,elmpr,elrpr,  &
&                           elv,elm,elr,cutv,cutm,cnv,cnm,delv,delm,    &
&                           ielel,ielsd,totv,totm)

    USE reals_mod,        ONLY: dencut,zerocut
    USE ale_advectors_mod,ONLY: flux_c1_VL,sum_flux

    ! Argument list
    INTEGER(KIND=ink),                      INTENT(IN)    :: id1,id2,   &
&                                                            nshape,nel,&
&                                                            nnod
    INTEGER(KIND=ink),DIMENSION(nshape,nel),INTENT(IN)    :: ielel,ielsd
    REAL(KIND=rlk),   DIMENSION(nel),       INTENT(OUT)   :: cutv,cutm, &
&                                                            elvpr,totv,&
&                                                            elmpr,totm,&
&                                                            elrpr
    REAL(KIND=rlk),   DIMENSION(nel),       INTENT(INOUT) :: elv,elm,elr
    REAL(KIND=rlk),   DIMENSION(nshape,nel),INTENT(IN)    :: delv,cnv
    REAL(KIND=rlk),   DIMENSION(nshape,nel),INTENT(OUT)   :: delm,cnm
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

  SUBROUTINE update_e_var()
  END SUBROUTINE update_e_var

  SUBROUTINE update_n_basis()
  END SUBROUTINE update_n_basis

  SUBROUTINE update_n_var()
  END SUBROUTINE update_n_var

END MODULE ale_advect_mod
