
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


MODULE comms_mod

  USE Typhon
  USE timing_stats, ONLY: bookleaf_times
  USE TYPH_util_mod,ONLY: get_time

  IMPLICIT NONE

  INTEGER(KIND=TSIZEK) :: VISCOSITY,HALFSTEP
  INTEGER(KIND=TSIZEK) :: key_comm_cells,key_comm_nodes
  INTEGER(KIND=TSIZEK) :: cnmassID,cnwtID,elfxID,elfyID,rho05ID,     &
&                         dubID,dulID,dutID,durID,dvbID,dvlID,dvtID, &
&                         dvrID,dxbID,dxlID,dxtID,dxrID,dybID,dylID, &
&                         dytID,dyrID

  PRIVATE
  PUBLIC :: register,exchange,VISCOSITY,HALFSTEP

CONTAINS

  SUBROUTINE register()

    USE kinds_mod,     ONLY: rlk,ink,lok
    USE integers_mod,  ONLY: nel,nnod,nshape,nel1,nnod1
    USE paradef_mod,   ONLY: e_owner_proc,           &
&                            n_owner_proc,e_loc_glob,n_loc_glob
    USE pointers_mod,  ONLY: cnmass,cnwt,ielnod
    USE error_mod,     ONLY: halt
    USE scratch_mod,   ONLY: dub=>rscratch16,dul=>rscratch17,     &
&                            dut=>rscratch18,dur=>rscratch19,     &
&                            dvb=>rscratch110,dvl=>rscratch111,   &
&                            dvt=>rscratch112,dvr=>rscratch113,   &
&                            dxb=>rscratch114,dxl=>rscratch115,   &
&                            dxt=>rscratch116,dxr=>rscratch117,   &
&                            dyb=>rscratch118,dyl=>rscratch119,   &
&                            dyt=>rscratch120,dyr=>rscratch121,   &
&                            elfx=>rscratch23,elfy=>rscratch24,   &
&                            rho05=>rscratch11
    USE timing_stats,  ONLY: bookleaf_times
    USE TYPH_util_mod, ONLY: get_time
    ! Local
    INTEGER(KIND=ink)    :: iel,inod,ierr
    INTEGER(KIND=TSIZEK) :: WHOLEMESH
    INTEGER(KIND=TSIZEK) :: nglayer = 1_TSIZEK
    REAL(KIND=rlk)       :: t0, t1

    integer(kind=TSIZEK), dimension(:),   pointer :: nel_tot  => NULL()
    integer(kind=TSIZEK), dimension(:),   pointer :: nnod_tot => NULL()
    integer(kind=TSIZEK), dimension(:,:), pointer :: conn => NULL()

    t0 = get_time()

    ierr = TYPH_Start_Register()

!   Partition Info
    ALLOCATE(nel_tot(0:nglayer),nnod_tot(0:nglayer),STAT=ierr)
    IF (ierr.NE.0_ink) CALL halt("ERROR: failed to allocate T3 memory",0)

    nel_tot(0)  = nel
    nnod_tot(0) = nnod
    nel_tot(1)  = nel1
    nnod_tot(1) = nnod1
    conn => ielnod(:,1:)

    ierr = TYPH_Set_Partition_Info(WHOLEMESH,4_TSIZEK,nglayer, &
&                                  nel_tot,nnod_tot,           &
&                                  e_owner_proc,n_owner_proc,  &
&                                  e_loc_glob,n_loc_glob,conn)
    DEALLOCATE(nel_tot,nnod_tot,e_owner_proc,n_owner_proc)

!   Keys - which cells go to which procs
    ierr = TYPH_Create_Key_Set(key_comm_cells,TYPH_KTYPE_CELL,    &
&                              1_TSIZEK,1_TSIZEK,WHOLEMESH)

!   Phases
    ierr = TYPH_Add_Phase(VISCOSITY,"Viscosity",TYPH_GHOSTS_ONE,  &
                          TYPH_PURE,KeySetID = key_comm_cells)
    ierr = TYPH_Add_Phase(HALFSTEP,"Half Step",TYPH_GHOSTS_ONE,   &
                          TYPH_PURE,KeySetID = key_comm_cells)

!   2D Quants
    ierr = TYPH_Add_Quant(cnmassID,"cnmass",TYPH_GHOSTS_ONE,      &
                          TYPH_REAL,TYPH_CENTRE_CELL,             &
                          TYPH_PURE,Dims=[nshape,TYPH_MESH_DIM])
    ierr = TYPH_Add_Quant(cnwtID,"cnwt",TYPH_GHOSTS_ONE,          &
                          TYPH_REAL,TYPH_CENTRE_CELL,             &
                          TYPH_PURE,Dims=[nshape,TYPH_MESH_DIM])
    ierr = TYPH_Add_Quant(elfxID,"elfx",TYPH_GHOSTS_ONE,          &
                          TYPH_REAL,TYPH_CENTRE_CELL,             &
                          TYPH_PURE,Dims=[nshape,TYPH_MESH_DIM])
    ierr = TYPH_Add_Quant(elfyID,"elfy",TYPH_GHOSTS_ONE,          &
                          TYPH_REAL,TYPH_CENTRE_CELL,             &
                          TYPH_PURE,Dims=[nshape,TYPH_MESH_DIM])

!   1D Quants
    ierr = TYPH_Add_Quant(rho05ID,"rho05",TYPH_GHOSTS_ONE,        &
                          TYPH_REAL,TYPH_CENTRE_CELL,TYPH_PURE)

    ierr = TYPH_Add_Quant(dubID,"dub",TYPH_GHOSTS_ONE,            &
                          TYPH_REAL,TYPH_CENTRE_CELL,TYPH_PURE)
    ierr = TYPH_Add_Quant(dulID,"dul",TYPH_GHOSTS_ONE,            &
                          TYPH_REAL,TYPH_CENTRE_CELL,TYPH_PURE)
    ierr = TYPH_Add_Quant(dutID,"dut",TYPH_GHOSTS_ONE,            &
                          TYPH_REAL,TYPH_CENTRE_CELL,TYPH_PURE)
    ierr = TYPH_Add_Quant(durID,"dur",TYPH_GHOSTS_ONE,            &
                           TYPH_REAL,TYPH_CENTRE_CELL,TYPH_PURE)

    ierr = TYPH_Add_Quant(dvbID,"dvb",TYPH_GHOSTS_ONE,            &
                          TYPH_REAL,TYPH_CENTRE_CELL,TYPH_PURE)
    ierr = TYPH_Add_Quant(dvlID,"dvl",TYPH_GHOSTS_ONE,            &
                          TYPH_REAL,TYPH_CENTRE_CELL,TYPH_PURE)
    ierr = TYPH_Add_Quant(dvtID,"dvt",TYPH_GHOSTS_ONE,            &
                          TYPH_REAL,TYPH_CENTRE_CELL,TYPH_PURE)
    ierr = TYPH_Add_Quant(dvrID,"dvr",TYPH_GHOSTS_ONE,            &
                          TYPH_REAL,TYPH_CENTRE_CELL,TYPH_PURE)

    ierr = TYPH_Add_Quant(dxbID,"dxb",TYPH_GHOSTS_ONE,            &
                          TYPH_REAL,TYPH_CENTRE_CELL,TYPH_PURE)
    ierr = TYPH_Add_Quant(dxlID,"dxl",TYPH_GHOSTS_ONE,            &
                          TYPH_REAL,TYPH_CENTRE_CELL,TYPH_PURE)
    ierr = TYPH_Add_Quant(dxtID,"dxt",TYPH_GHOSTS_ONE,            &
                          TYPH_REAL,TYPH_CENTRE_CELL,TYPH_PURE)
    ierr = TYPH_Add_Quant(dxrID,"dxr",TYPH_GHOSTS_ONE,            &
                          TYPH_REAL,TYPH_CENTRE_CELL,TYPH_PURE)

    ierr = TYPH_Add_Quant(dybID,"dyb",TYPH_GHOSTS_ONE,            &
                          TYPH_REAL,TYPH_CENTRE_CELL,TYPH_PURE)
    ierr = TYPH_Add_Quant(dylID,"dyl",TYPH_GHOSTS_ONE,            &
                          TYPH_REAL,TYPH_CENTRE_CELL,TYPH_PURE)
    ierr = TYPH_Add_Quant(dytID,"dyt",TYPH_GHOSTS_ONE,            &
                          TYPH_REAL,TYPH_CENTRE_CELL,TYPH_PURE)
    ierr = TYPH_Add_Quant(dyrID,"dyr",TYPH_GHOSTS_ONE,            &
                          TYPH_REAL,TYPH_CENTRE_CELL,TYPH_PURE)

    ierr = TYPH_Add_Quant_to_Phase(HALFSTEP,cnmassID)
    ierr = TYPH_Add_Quant_to_Phase(HALFSTEP,cnwtID)
    ierr = TYPH_Add_Quant_to_Phase(HALFSTEP,elfxID)
    ierr = TYPH_Add_Quant_to_Phase(HALFSTEP,elfyID)
    ierr = TYPH_Add_Quant_to_Phase(HALFSTEP,rho05ID)

    ierr = TYPH_Add_Quant_to_Phase(VISCOSITY,dubID)
    ierr = TYPH_Add_Quant_to_Phase(VISCOSITY,dulID)
    ierr = TYPH_Add_Quant_to_Phase(VISCOSITY,dutID)
    ierr = TYPH_Add_Quant_to_Phase(VISCOSITY,durID)

    ierr = TYPH_Add_Quant_to_Phase(VISCOSITY,dvbID)
    ierr = TYPH_Add_Quant_to_Phase(VISCOSITY,dvlID)
    ierr = TYPH_Add_Quant_to_Phase(VISCOSITY,dvtID)
    ierr = TYPH_Add_Quant_to_Phase(VISCOSITY,dvrID)

    ierr = TYPH_Add_Quant_to_Phase(VISCOSITY,dxbID)
    ierr = TYPH_Add_Quant_to_Phase(VISCOSITY,dxlID)
    ierr = TYPH_Add_Quant_to_Phase(VISCOSITY,dxtID)
    ierr = TYPH_Add_Quant_to_Phase(VISCOSITY,dxrID)

    ierr = TYPH_Add_Quant_to_Phase(VISCOSITY,dybID)
    ierr = TYPH_Add_Quant_to_Phase(VISCOSITY,dylID)
    ierr = TYPH_Add_Quant_to_Phase(VISCOSITY,dytID)
    ierr = TYPH_Add_Quant_to_Phase(VISCOSITY,dyrID)

    ierr = TYPH_Set_Quant_Address(cnmassID,cnmass)
    ierr = TYPH_Set_Quant_Address(cnwtID,cnwt)
    ierr = TYPH_Set_Quant_Address(elfxID,elfx)
    ierr = TYPH_Set_Quant_Address(elfyID,elfy)
    ierr = TYPH_Set_Quant_Address(rho05ID,rho05)

    ierr = TYPH_Set_Quant_Address(dubID,dub)
    ierr = TYPH_Set_Quant_Address(dulID,dul)
    ierr = TYPH_Set_Quant_Address(dutID,dut)
    ierr = TYPH_Set_Quant_Address(durID,dur)

    ierr = TYPH_Set_Quant_Address(dvbID,dvb)
    ierr = TYPH_Set_Quant_Address(dvlID,dvl)
    ierr = TYPH_Set_Quant_Address(dvtID,dvt)
    ierr = TYPH_Set_Quant_Address(dvrID,dvr)

    ierr = TYPH_Set_Quant_Address(dxbID,dxb)
    ierr = TYPH_Set_Quant_Address(dxlID,dxl)
    ierr = TYPH_Set_Quant_Address(dxtID,dxt)
    ierr = TYPH_Set_Quant_Address(dxrID,dxr)

    ierr = TYPH_Set_Quant_Address(dybID,dyb)
    ierr = TYPH_Set_Quant_Address(dylID,dyl)
    ierr = TYPH_Set_Quant_Address(dytID,dyt)
    ierr = TYPH_Set_Quant_Address(dyrID,dyr)

    ierr = Typh_Finish_Register()

    ! Timing data
    t1 = get_time()
    t1=t1-t0
    bookleaf_times%time_in_comreg=bookleaf_times%time_in_comreg+t1

  END SUBROUTINE register

  SUBROUTINE exchange(comm_phase)

    USE kinds_mod,     ONLY: rlk,ink,lok

    INTEGER(KIND=TSIZEK), INTENT(IN) :: comm_phase
    INTEGER(KIND=TSIZEK)             :: ierr
    REAL(KIND=rlk)       :: t0, t1

    t0 = get_time()

    ierr=TYPH_Start_Exch(comm_phase)
    ierr=TYPH_Finish_Exch(comm_phase)

    ! Timing data
    t1 = get_time()
    t1=t1-t0
    bookleaf_times%time_in_comms=bookleaf_times%time_in_comms+t1

  END SUBROUTINE exchange

END MODULE comms_mod
