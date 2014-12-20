
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

  IMPLICIT NONE

  INTEGER(KIND=TSIZEK) :: VISCOSITY,HALFSTEP
  INTEGER(KIND=TSIZEK) :: key_comm_cells,key_comm_nodes
  INTEGER(KIND=TSIZEK) :: cnmassID,cnwtID,elfxID,elfyID,rho05ID,duID,   &
&                         dvID,dxID,dyID

  PRIVATE
  PUBLIC :: register,exchange,rcb,VISCOSITY,HALFSTEP

CONTAINS

  SUBROUTINE register()

    USE kinds_mod,    ONLY: rlk,ink
    USE integers_mod, ONLY: nel,nnod,nshape,nel1,nnod1
    USE paradef_mod,  ONLY: e_owner_proc,n_owner_proc,e_loc_glob,      &
&                           n_loc_glob
    USE pointers_mod, ONLY: cnmass,cnwt,ielnod
    USE error_mod,    ONLY: halt
    USE scratch_mod,  ONLY: rscratch11,rscratch23,rscratch24,rscratch25,&
&                           rscratch26
    USE timing_mod,   ONLY: bookleaf_times
    USE TYPH_util_mod,ONLY: get_time

    ! Local
    INTEGER(KIND=ink)                           :: ierr
    INTEGER(KIND=TSIZEK)                        :: WHOLEMESH
    INTEGER(KIND=TSIZEK),PARAMETER              :: nglayer=1_TSIZEK
    REAL(KIND=rlk)                              :: t0,t1
    INTEGER(KIND=TSIZEK),DIMENSION(:),  POINTER :: nel_tot
    INTEGER(KIND=TSIZEK),DIMENSION(:),  POINTER :: nnod_tot
    INTEGER(KIND=TSIZEK),DIMENSION(:,:),POINTER :: conn

    t0 = get_time()

    ! Initialise
    NULLIFY(nel_tot,nnod_tot,conn)
    ierr = TYPH_Start_Register()

    ! Partition Info
    ALLOCATE(nel_tot(0:nglayer),nnod_tot(0:nglayer),STAT=ierr)
    IF (ierr.NE.0_ink) THEN
      CALL halt("ERROR: failed to allocate T3 memory",0)
    ENDIF
    nel_tot(0)=nel
    nnod_tot(0)=nnod
    nel_tot(1)=nel1
    nnod_tot(1)=nnod1
    conn=>ielnod(:,1:)
    ierr=TYPH_Set_Partition_Info(WHOLEMESH,4_TSIZEK,nglayer,nel_tot,    &
&                                nnod_tot,e_owner_proc,n_owner_proc,    &
&                                e_loc_glob,n_loc_glob,conn)
    DEALLOCATE(nel_tot,nnod_tot,e_owner_proc,n_owner_proc)

    ! Keys - which cells go to which procs
    ierr=TYPH_Create_Key_Set(key_comm_cells,TYPH_KTYPE_CELL,1_TSIZEK,   &
&                            1_TSIZEK,WHOLEMESH)

    ! Phases
    ierr=TYPH_Add_Phase(VISCOSITY,"Viscosity",TYPH_GHOSTS_ONE,          &
&                       TYPH_PURE,KeySetID=key_comm_cells)
    ierr=TYPH_Add_Phase(HALFSTEP,"Half Step",TYPH_GHOSTS_ONE,           &
&                       TYPH_PURE,KeySetID=key_comm_cells)

    ! 2D Quants
    ierr=TYPH_Add_Quant(cnmassID,"cnmass",TYPH_GHOSTS_ONE,TYPH_REAL,    &
&                       TYPH_CENTRE_CELL,TYPH_PURE,                     &
&                       Dims=[nshape,TYPH_MESH_DIM])
    ierr=TYPH_Add_Quant(cnwtID,"cnwt",TYPH_GHOSTS_ONE,TYPH_REAL,        &
&                       TYPH_CENTRE_CELL,TYPH_PURE,                     &
&                       Dims=[nshape,TYPH_MESH_DIM])
    ierr=TYPH_Add_Quant(elfxID,"elfx",TYPH_GHOSTS_ONE,TYPH_REAL,        &
&                       TYPH_CENTRE_CELL,TYPH_PURE,                     &
&                       Dims=[nshape,TYPH_MESH_DIM])
    ierr=TYPH_Add_Quant(elfyID,"elfy",TYPH_GHOSTS_ONE,TYPH_REAL,        &
&                       TYPH_CENTRE_CELL,TYPH_PURE,                     &
&                       Dims=[nshape,TYPH_MESH_DIM])
    ierr=TYPH_Add_Quant(duID,"du",TYPH_GHOSTS_ONE,TYPH_REAL,            &
&                       TYPH_CENTRE_CELL,TYPH_PURE,                     &
&                       Dims=[nshape,TYPH_MESH_DIM])
    ierr=TYPH_Add_Quant(dvID,"dv",TYPH_GHOSTS_ONE,TYPH_REAL,            &
&                       TYPH_CENTRE_CELL,TYPH_PURE,                     &
&                       Dims=[nshape,TYPH_MESH_DIM])
    ierr=TYPH_Add_Quant(dxID,"dx",TYPH_GHOSTS_ONE,TYPH_REAL,            &
&                       TYPH_CENTRE_CELL,TYPH_PURE,                     &
&                       Dims=[nshape,TYPH_MESH_DIM])
    ierr=TYPH_Add_Quant(dyID,"dy",TYPH_GHOSTS_ONE,TYPH_REAL,            &
&                       TYPH_CENTRE_CELL,TYPH_PURE,                     &
&                       Dims=[nshape,TYPH_MESH_DIM])

    ! 1D Quants
    ierr=TYPH_Add_Quant(rho05ID,"rho05",TYPH_GHOSTS_ONE,TYPH_REAL,      &
&                       TYPH_CENTRE_CELL,TYPH_PURE)

    ! Attach quantities to phase                      
    ierr=TYPH_Add_Quant_to_Phase(HALFSTEP,cnmassID)
    ierr=TYPH_Add_Quant_to_Phase(HALFSTEP,cnwtID)
    ierr=TYPH_Add_Quant_to_Phase(HALFSTEP,elfxID)
    ierr=TYPH_Add_Quant_to_Phase(HALFSTEP,elfyID)
    ierr=TYPH_Add_Quant_to_Phase(HALFSTEP,rho05ID)
    ierr=TYPH_Add_Quant_to_Phase(VISCOSITY,duID)
    ierr=TYPH_Add_Quant_to_Phase(VISCOSITY,dvID)
    ierr=TYPH_Add_Quant_to_Phase(VISCOSITY,dxID)
    ierr=TYPH_Add_Quant_to_Phase(VISCOSITY,dyID)

    ! Set addresses
    ierr=TYPH_Set_Quant_Address(cnmassID,cnmass)
    ierr=TYPH_Set_Quant_Address(cnwtID,cnwt)
    ierr=TYPH_Set_Quant_Address(elfxID,rscratch23)
    ierr=TYPH_Set_Quant_Address(elfyID,rscratch24)
    ierr=TYPH_Set_Quant_Address(rho05ID,rscratch11)
    ierr=TYPH_Set_Quant_Address(duID,rscratch23)
    ierr=TYPH_Set_Quant_Address(dvID,rscratch24)
    ierr=TYPH_Set_Quant_Address(dxID,rscratch25)
    ierr=TYPH_Set_Quant_Address(dyID,rscratch26)

    ! Finish
    ierr=Typh_Finish_Register()

    ! Timing data
    t1=get_time()
    t1=t1-t0
    bookleaf_times%time_in_comreg=bookleaf_times%time_in_comreg+t1

  END SUBROUTINE register

  SUBROUTINE exchange(comm_phase)

    USE kinds_mod,    ONLY: rlk
    USE typh_util_mod,ONLY: get_time
    USE timing_mod,   ONLY: bookleaf_times

    INTEGER(KIND=TSIZEK), INTENT(IN) :: comm_phase
    INTEGER(KIND=TSIZEK)             :: ierr
    REAL(KIND=rlk)                   :: t0,t1

    t0 = get_time()

    ! exchange
    ierr=TYPH_Start_Exch(comm_phase)
    ierr=TYPH_Finish_Exch(comm_phase)

    ! Timing data
    t1 = get_time()
    t1=t1-t0
    bookleaf_times%time_in_comms=bookleaf_times%time_in_comms+t1

  END SUBROUTINE exchange

  RECURSIVE SUBROUTINE rcb(nl,nk,npartl,nparth,ipart,icolour)

    USE kinds_mod,ONLY: ink

    ! Argument list
    INTEGER(KIND=ink),                 INTENT(IN)    :: nl,nk,npartl,   &
&                                                       nparth
    INTEGER(KIND=ink),                 INTENT(INOUT) :: ipart
    INTEGER(KIND=ink),DIMENSION(nl,nk),INTENT(INOUT) :: icolour
    ! Local
    INTEGER(KIND=ink)                                :: nmid,npartmid,  &
&                                                       npart    

    ! calculate the number of remaining partitions
    npart=nparth-npartl+1_ink

    ! finish
    IF (npart.EQ.1_ink) THEN
      ipart=ipart+1_ink
      icolour(1:nl,1:nk)=ipart
      RETURN 
    ENDIF

    ! set colour
    npartmid=npartl+npart/2_ink
    IF (nl.GT.nk) THEN
      nmid=nl/2_ink
      IF ((npartmid-npartl).GT.0_ink) THEN
        CALL rcb(nmid,nk,npartl,npartmid-1_ink,ipart,icolour(1:nmid,1:nk))
      ENDIF
      IF ((nparth-npartmid+1_ink).GT.0_ink) THEN
        CALL rcb(nl-nmid,nk,npartmid,nparth,ipart,icolour(nmid+1_ink:nl,1:nk))
      ENDIF
    ELSE
      nmid=nk/2_ink
      IF ((npartmid-npartl).GT.0_ink) THEN
        CALL rcb(nl,nmid,npartl,npartmid-1_ink,ipart,icolour(1:nl,1:nmid))
      ENDIF
      IF ((nparth-npartmid+1_ink).GT.0_ink) THEN
        CALL rcb(nl,nk-nmid,npartmid,nparth,ipart,icolour(1:nl,nmid+1_ink:nk))
      ENDIF
    ENDIF

  END SUBROUTINE rcb

END MODULE comms_mod
