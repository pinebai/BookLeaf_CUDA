
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


PROGRAM main

  USE comms_mod,    ONLY: register
  USE error_mod,    ONLY: halt
  USE paradef_mod,  ONLY: zparallel,MProcW
  USE timing_stats, ONLY: bookleaf_times
  USE timers_mod,   ONLY: start_timers
  USE TYPH_util_mod,ONLY: get_time
  USE write_mod,    ONLY: write_sprint
  USE mesh_mod,     ONLY: mesh_gen,mesh_transfer
#ifdef SILO
  USE silo_mod,     ONLY: write_silo_dump
#endif

  IMPLICIT NONE

  INTEGER :: ierr

! ###################
! Parallelism
! ###################

  CALL init_parallel()

! ###################
! TIMERS
! ###################

! start timers
  CALL start_timers()

! ###################
! BANNER
! ###################

! welcome banner
  IF (MprocW) THEN
    CALL banner()
  ENDIF

! ###################
! DEFAULTS
! ###################

! initialise input
  CALL init_defaults()

! ###################
! INPUT
! ###################

! read command line
  CALL read_command()

! read input files
  CALL read_files()

! generate mesh from input
  CALL mesh_gen()

! ###################
! INITIALISATION
! ###################

! setup run parameters from input
  CALL init_parameters()

! setup memory
  CALL init_memory()

  ! Transfer mesh onto solution arrays, populate connectivity arrays
  CALL mesh_transfer()

! register comms
  IF (zparallel) THEN
    call register()
  ENDIF

! main initialisation
  CALL init()

! initialise parallel misdirection  
  IF (zparallel) THEN
    CALL init_comm()
  ENDIF

! problem specific modifications
#ifdef MOD_P
  CALL modify()
#endif

  bookleaf_times%time_end_init = get_time()

! print initial totals
  CALL write_sprint()

! Dump initial Silo file
#ifdef SILO
  CALL write_silo_dump("initial_dump")
#endif

! ###################
! SOLVER
! ###################

! hydrodynamics
  CALL hydro()

! ###################
! FINISH
! ###################

  CALL halt("End time reached, terminating cleanly",1,zend=.true.)

END PROGRAM main
