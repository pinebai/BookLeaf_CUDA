
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

SUBROUTINE hydro()

  USE kinds_mod,    ONLY: ink,rlk
  USE integers_mod, ONLY: nel,nstep,idtel,idtreg
  USE logicals_mod, ONLY: zale,zaleon,zmprocw
  USE strings_mod,  ONLY: sdt
  USE reals_mod,    ONLY: time,time_end,dt_initial,time_alemin,         &
&                         time_alemax
  USE getdt_mod,    ONLY: getdt
  USE lagstep_mod,  ONLY: lagstep
  USE alestep_mod,  ONLY: alestep
  USE timing_mod,   ONLY: bookleaf_times
  USE Typh_util_mod,ONLY: get_time

  IMPLICIT NONE

  ! Local
  REAL(KIND=rlk) :: dt,t0,t1,t2,grind

  ! Timer
  bookleaf_times%time_hydro=get_time()

  ! initialise
  nstep=0_ink
  dt=dt_initial
  idtel=0_ink
  idtreg=0_ink
  sdt=' INITIAL'

  l1:DO
    t0=get_time()
    ! increment step
    nstep=nstep+1_ink
    ! calculate timestep
    IF (nstep.GT.1_ink) CALL getdt(dt)
    !# Missing code here that can't be merged
    ! update time
    time=time+dt
    !# Code here that can't be taken out
    ! lagrangian step
    CALL lagstep(dt)
    ! ale step
    IF (zale) THEN
      zaleon=(time.GE.time_alemin).AND.(time.LE.time_alemax)
      IF (zaleon) CALL alestep(nstep,dt)
    ENDIF
    !# Missing code here that can't be merged
    t1=get_time()
    t2=t1-t0
    grind=t2*1.0e6_rlk/nel
    IF (zmprocw) THEN
      WRITE(6,'(" step=",i7,"  el=",i9,"  reg=",i3,"  dt=",1pe16.9,'    &
&      //'"  time=",1pe16.9,"  grind=",1pe8.1,"  timer=",1pe16.9," s",' &
&      //'2X,a8)') nstep,idtel,idtreg,dt,time,grind,t2,sdt
    ENDIF
    ! IO Timing data
    t2=get_time()
    t2=t2-t1
    bookleaf_times%time_step_io=bookleaf_times%time_step_io+t2
    ! test for end of calculation
    IF (time.GE.time_end) EXIT l1
    !# test for resources
  ENDDO l1

END SUBROUTINE hydro
