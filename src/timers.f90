
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


MODULE timers_mod

  USE kinds_mod,    ONLY: rlk
  USE timing_mod,   ONLY: bookleaf_times
  USE TYPH_util_mod,ONLY: get_time

  IMPLICIT NONE

CONTAINS

  SUBROUTINE start_timers

    bookleaf_times%time_start    = get_time()
    bookleaf_times%time_end      = 0.0_rlk
    bookleaf_times%time_end_main = 0.0_rlk
    bookleaf_times%time_total    = 0.0_rlk
    bookleaf_times%time_hydro    = 0.0_rlk
    bookleaf_times%time_end_init = 0.0_rlk  ! time at start of hydro
    bookleaf_times%time_in_lag   = 0.0_rlk
    bookleaf_times%time_in_io    = 0.0_rlk
    bookleaf_times%time_step_io  = 0.0_rlk
    bookleaf_times%time_in_getq  = 0.0_rlk
    bookleaf_times%time_in_getdt = 0.0_rlk
    bookleaf_times%time_in_gethg = 0.0_rlk
    bookleaf_times%time_in_getsp = 0.0_rlk
    bookleaf_times%time_in_getacc= 0.0_rlk
    bookleaf_times%time_in_getfrc= 0.0_rlk
    bookleaf_times%time_in_getein= 0.0_rlk
    bookleaf_times%time_in_eos   = 0.0_rlk
    bookleaf_times%time_in_geom  = 0.0_rlk
    bookleaf_times%time_in_comreg= 0.0_rlk
    bookleaf_times%time_in_comms = 0.0_rlk
    bookleaf_times%time_in_colls = 0.0_rlk

  END SUBROUTINE start_timers

  SUBROUTINE end_timers

    bookleaf_times%time_end   = get_time()
    bookleaf_times%time_total = bookleaf_times%time_end-bookleaf_times%time_start

  END SUBROUTINE end_timers

  SUBROUTINE print_timers

    USE paradef_mod, ONLY: MProcW,zparallel

    ! local
    REAL(KIND=rlk)               :: fac,w1,w2,w3,w4,w5
    CHARACTER(LEN=25),PARAMETER  :: ft='(a28,1X,e13.6,1X,f7.3,a2)'

    IF (MProcW) THEN
      fac=100.0_rlk/bookleaf_times%time_total
      w1=MAX(0.0_rlk,bookleaf_times%time_end_main-bookleaf_times%time_hydro)
      w2=bookleaf_times%time_end_init-bookleaf_times%time_start
      WRITE(6,*) ' '
      WRITE(6,ft) ' time in initialisation     ',w2,w2*fac,' %'
      IF (zparallel) THEN
        w3=bookleaf_times%time_in_comreg
        WRITE(6,ft) '   time in register         ',w3,w3*fac,' %'
      ENDIF
      WRITE(6,ft) ' time in main loop          ',w1,w1*fac,' %'
      w3=bookleaf_times%time_in_getdt
      WRITE(6,ft) '   time in getdt            ',w3,w3*fac,' %'
      w3=bookleaf_times%time_step_io
      WRITE(6,ft) '   time in step IO          ',w3,w3*fac,' %'
      w3=bookleaf_times%time_in_lag
      WRITE(6,ft) '   time in lagstep          ',w3,w3*fac,' %'
      w3=bookleaf_times%time_in_eos
      WRITE(6,ft) '     time in eos            ',w3,w3*fac,' %'
      w3=bookleaf_times%time_in_getacc
      WRITE(6,ft) '     time in getacc         ',w3,w3*fac,' %'
      w3=bookleaf_times%time_in_getq
      WRITE(6,ft) '     time in getq           ',w3,w3*fac,' %'
      w3=bookleaf_times%time_in_geom
      WRITE(6,ft) '     time in getgeom        ',w3,w3*fac,' %'
      w3=bookleaf_times%time_in_getein
      WRITE(6,ft) '     time in getein         ',w3,w3*fac,' %'
      w3=bookleaf_times%time_in_getfrc
      WRITE(6,ft) '     time in getforce       ',w3,w3*fac,' %'
      w3=bookleaf_times%time_in_gethg
      WRITE(6,ft) '       time in gethg        ',w3,w3*fac,' %'
      w3=bookleaf_times%time_in_getsp
      WRITE(6,ft) '       time in getsp        ',w3,w3*fac,' %'
      IF (zparallel) THEN
        w3=bookleaf_times%time_in_comms
        WRITE(6,ft) ' time in mpi exchange     ',w3,w3*fac,' %'
        w3=bookleaf_times%time_in_colls
        WRITE(6,ft) ' time in mpi collectives  ',w3,w3*fac,' %'
      ENDIF
      w4=bookleaf_times%time_in_io
      WRITE(6,ft) ' time in output dumps       ',w4,w4*fac,' %'
      w5=bookleaf_times%time_total-w1-w2-w4
      WRITE(6,ft) ' remaining time             ',w5,w5*fac,' %'
      WRITE(6,*) ' '
      WRITE(6,'(a13,e13.6,a2)') ' Run time =  ',bookleaf_times%time_total,' s'
      WRITE(6,*) ' '
    ENDIF

  END SUBROUTINE print_timers  

END MODULE timers_mod
