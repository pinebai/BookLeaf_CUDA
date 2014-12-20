
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

SUBROUTINE read_command()

  USE kinds_mod,     ONLY: ink
  USE strings_mod,   ONLY: sfile
  USE error_mod,     ONLY: halt
  USE parameters_mod,ONLY: LN
  USE paradef_mod,   ONLY: MProcW
  USE utilities_mod, ONLY: convupper

  IMPLICIT NONE

  ! Local
  INTEGER(KIND=ink) :: nargs,ii,il,ires
  CHARACTER(LEN=LN) :: str

  nargs=COMMAND_ARGUMENT_COUNT()
  IF (nargs.LE.0_ink) RETURN
  l1:DO ii=1,nargs
    CALL GET_COMMAND_ARGUMENT(NUMBER=ii,VALUE=str,LENGTH=il,STATUS=ires)
    IF (ires.NE.0_ink) CALL halt("ERROR: Get_Command_Argument failed",0)
    IF (il.LE.5_ink) CYCLE l1
    CALL convupper(str(1:5))
    IF (str(1:5).EQ.'FILE=') THEN
      sfile=' '
      IF ((il-5_ink).GT.LN) CALL halt("ERROR: file string too long",0)
      sfile=TRIM(str(6:il))
    ENDIF
  ENDDO l1

END SUBROUTINE read_command

SUBROUTINE read_files()

  USE kinds_mod,     ONLY: ink,lok,rlk
  USE strings_mod,   ONLY: sfile
  USE integers_mod,  ONLY: eos_type,nreg,nmat,max_seg,max_subseg
  USE reals_mod,     ONLY: time_start,time_end,dt_initial,dt_g,dt_min,  &
&                          dt_max,cfl_sf,div_sf,eos_param,mat_rho,      &
&                          mat_ein,ccut,zcut,zerocut,pcut,dencut,accut, &
&                          cq1,cq2,kappaall,kappareg,pmeritall,pmeritreg
  USE logicals_mod,  ONLY: zdtnotreg,zmidlength
  USE parameters_mod,ONLY: LI
  USE error_mod,     ONLY: halt
  USE utilities_mod, ONLY: findstr

  IMPLICIT NONE

  ! Local
  INTEGER(KIND=ink),PARAMETER :: IUNIT=30_ink
  INTEGER(KIND=ink)           :: ierr
  CHARACTER(LEN=LI)           :: str
  LOGICAL(KIND=lok)           :: zflag

  NAMELIST /PROBSIZE/ nreg,nmat,max_seg,max_subseg
  NAMELIST /CONTROL/ time_start,time_end,dt_initial,dt_g,dt_min,dt_max, &
&  cfl_sf,div_sf,zdtnotreg,zmidlength,cq1,cq2,kappaall,kappareg,        &
&  pmeritall,pmeritreg
  NAMELIST /EOS/ eos_type,eos_param,mat_rho,mat_ein
  NAMELIST /CUTOFF/ ccut,zcut,zerocut,pcut,dencut,accut

  ! Open control file
  OPEN(UNIT=IUNIT,FILE=sfile,ACTION='read',STATUS='old',                &
&      FORM='formatted',IOSTAT=ierr,IOMSG=str)
  IF (ierr.NE.0_ink) CALL halt("ERROR: "//TRIM(str),0)

  ! Find problem size
  zflag=findstr('probsize',IUNIT)
  IF (zflag) THEN
    REWIND(UNIT=IUNIT)
    READ(UNIT=IUNIT,NML=PROBSIZE,IOSTAT=ierr,IOMSG=str)
    IF (ierr.NE.0_ink) CALL halt("ERROR: "//TRIM(str),0)
  ENDIF

  ! Find control
  zflag=findstr('control',IUNIT)
  IF (zflag) THEN
    REWIND(UNIT=IUNIT)
    READ(UNIT=IUNIT,NML=CONTROL,IOSTAT=ierr,IOMSG=str)
    IF (ierr.NE.0_ink) CALL halt("ERROR: "//TRIM(str),0)
  ENDIF

  ! Find eos
  zflag=findstr('eos',IUNIT)
  IF (zflag) THEN
    REWIND(UNIT=IUNIT)
    READ(UNIT=IUNIT,NML=EOS,IOSTAT=ierr,IOMSG=str)
    IF (ierr.NE.0_ink) CALL halt("ERROR: "//TRIM(str),0)
  ENDIF

  ! Find cutoff
  zflag=findstr('cutoff',IUNIT)
  IF (zflag) THEN
    REWIND(UNIT=IUNIT)
    READ(UNIT=IUNIT,NML=CUTOFF,IOSTAT=ierr,IOMSG=str)
    IF (ierr.NE.0_ink) CALL halt("ERROR: "//TRIM(str),0)
  ENDIF

  IF (nreg.LE.0_rlk) CALL halt('ERROR: nreg < 0',0)
  IF (nmat.LE.0_rlk) CALL halt('ERROR: nmat < 0',0)

  ! close control file
  CLOSE(UNIT=IUNIT)

END SUBROUTINE read_files

