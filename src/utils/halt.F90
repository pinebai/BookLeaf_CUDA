
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

SUBROUTINE halt(smessage,iout,zend)

  USE kinds_mod,    ONLY: lok,ink
  USE logicals_mod, ONLY: zmprocw
  USE timers_mod,   ONLY: end_timers,print_timers
  USE TYPH_util_mod,ONLY: get_time
  USE timing_mod,   ONLY: bookleaf_times
  USE write_mod,    ONLY: write_sprint
#ifdef SILO
  USE silo_mod,     ONLY: write_silo_dump
#endif
#ifdef TIO
  USE TyphonIO_mod, ONLY: write_tio_dump
#endif
  USE Typhon,       ONLY: TYPH_kill,TYPH_Abort

  IMPLICIT NONE

  ! Argument list
  CHARACTER(LEN=*), INTENT(IN)          :: smessage ! error message
  INTEGER(KIND=ink),INTENT(IN)          :: iout     ! output vis.
  LOGICAL(KIND=lok),INTENT(IN),OPTIONAL :: zend     ! end time reached
  ! Local
  INTEGER                               :: ierr
  LOGICAL(KIND=lok)                     :: zfin

  ! spacer
  IF (zmprocw) THEN
    WRITE(6,'(a132)')' #############################################' &
&              //'##################################################' &
&              //'####################################'
  ENDIF

  ! reached end of calculation
  zfin=.FALSE._lok
  IF (PRESENT(zend)) THEN
    zfin=zend
  ENDIF

  ! echo message
  IF ((zfin.AND.zmprocw).OR.(.NOT.zfin)) THEN
    WRITE(6,'(2x,a)') smessage
  ENDIF

  ! Timing data
  bookleaf_times%time_end_main=get_time()

  IF (iout.EQ.1_ink) THEN
#ifdef SILO
    ! Dump Silo file
    CALL write_silo_dump("final_dump")
#endif
#ifdef TIO
  CALL write_tio_dump("final_dump.h5",.NOT.zfin)
#endif
  
  ENDIF

  IF (zfin) THEN
    ! print final totals
    CALL write_sprint()
  ENDIF

  ! halt timers
  CALL end_timers()

  ! print timers
  CALL print_timers()

  ! spacer
  IF (zmprocw) THEN
    WRITE(6,'(a132)')' #############################################' &
&              //'##################################################' &
&              //'####################################'
  ENDIF
  ! end program

  IF (zfin) THEN
    ierr=TYPH_Kill(FinalizeMPI=.true.)
  ELSE
    ierr=TYPH_Abort(-1)
  ENDIF

  STOP

END SUBROUTINE halt
