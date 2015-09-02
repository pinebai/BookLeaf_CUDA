
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
  USE kinds_mod,    ONLY: ink

  IMPLICIT NONE

  INTEGER(KIND=ink)    :: neltot,nnodtot
  INTEGER(KIND=TSIZEK) :: VISCOSITY,HALFSTEP,ADV_EXCH_EL,ADV_EXCH_ND
  INTEGER(KIND=TSIZEK) :: key_comm_cells,key_comm_nodes
  INTEGER(KIND=TSIZEK) :: cnmassID,cnwtID,elfxID,elfyID,rho05ID,duID,  &
&                         dvID,dxID,dyID,dfvID,dfmID,einID,rhoID,      &
&                         eluvID,elvvID,elv0ID,elvolID
  INTEGER(KIND=ink),DIMENSION(:),ALLOCATABLE   :: nodproc,nnodiel
  INTEGER(KIND=ink), PARAMETER                 :: NGSTLAY=2_ink

  PRIVATE
  PUBLIC :: register,exchange,VISCOSITY,HALFSTEP,ADV_EXCH_EL,ADV_EXCH_ND
#ifndef NOMPI
  PUBLIC :: partition_mesh
#endif

CONTAINS

  SUBROUTINE register()

    USE kinds_mod,    ONLY: ink,rlk
    USE integers_mod, ONLY: nel,nnod,nshape,nel1,nnod1,nel2,nnod2
    USE pointers_mod, ONLY: cnmass,cnwt,ein,elvol,ielnd,ielownerproc,  &
&                           iellocglob,indownerproc,indlocglob,rho
    USE error_mod,    ONLY: halt
    USE scratch_mod,  ONLY: rscratch11,rscratch21,rscratch22,          &
&                           rscratch23,rscratch24,rscratch25,          &
&                           rscratch26,rscratch27,rscratch28
    USE timing_mod,   ONLY: bookleaf_times
    USE TYPH_util_mod,ONLY: get_time

    ! Local
    INTEGER(KIND=ink)                           :: ierr
    INTEGER(KIND=TSIZEK)                        :: WHOLEMESH
    REAL(KIND=rlk)                              :: t0,t1
    INTEGER(KIND=TSIZEK),DIMENSION(:),  POINTER :: nel_tot
    INTEGER(KIND=TSIZEK),DIMENSION(:),  POINTER :: nnod_tot
    INTEGER(KIND=TSIZEK),DIMENSION(:,:),POINTER :: conn

    t0 = get_time()

    ! Initialise
    NULLIFY(nel_tot,nnod_tot,conn)
    ierr = TYPH_Start_Register()

    ! Partition Info
    ALLOCATE(nel_tot(0:NGSTLAY),nnod_tot(0:NGSTLAY),STAT=ierr)
    IF (ierr.NE.0_ink) THEN
      CALL halt("ERROR: failed to allocate T3 memory",0)
    ENDIF
    nel_tot(0)=nel
    nnod_tot(0)=nnod
    nel_tot(1)=nel1
    nnod_tot(1)=nnod1
    nel_tot(2)=nel2
    nnod_tot(2)=nnod2
    conn=>ielnd(:,1:)
    ierr=TYPH_Set_Partition_Info(WHOLEMESH,4_TSIZEK,NGSTLAY,nel_tot,    &
&                                nnod_tot,ielownerproc,indownerproc,    &
&                                iellocglob,indlocglob,conn)
    DEALLOCATE(nel_tot,nnod_tot,ielownerproc,indownerproc)

    ! Keys - which cells go to which procs
    ierr=TYPH_Create_Key_Set(key_comm_cells,TYPH_KTYPE_CELL,1_TSIZEK,   &
&                            2_TSIZEK,WHOLEMESH)

    ! Phases
    ierr=TYPH_Add_Phase(VISCOSITY,"Viscosity",TYPH_GHOSTS_ONE,          &
&                       TYPH_PURE,KeySetID=key_comm_cells)
    ierr=TYPH_Add_Phase(HALFSTEP,"Half Step",TYPH_GHOSTS_ONE,           &
&                       TYPH_PURE,KeySetID=key_comm_cells)
    ierr=TYPH_Add_Phase(ADV_EXCH_EL,"Pre Ele Adv Exchange",             &
&                       TYPH_GHOSTS_TWO,TYPH_PURE,                      &
&                       KeySetID=key_comm_cells)
    ierr=TYPH_Add_Phase(ADV_EXCH_ND,"Pre Nod Adv Exchange",             &
&                       TYPH_GHOSTS_TWO,TYPH_PURE,                      &
&                       KeySetID=key_comm_cells)

    ! 2D Quants
    ierr=TYPH_Add_Quant(cnmassID,"cnmass",TYPH_GHOSTS_TWO,TYPH_REAL,    &
&                       TYPH_CENTRE_CELL,TYPH_PURE,                     &
&                       Dims=[nshape,TYPH_MESH_DIM])
    ierr=TYPH_Add_Quant(cnwtID,"cnwt",TYPH_GHOSTS_TWO,TYPH_REAL,        &
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
    ierr=TYPH_Add_Quant(dfvID,"dfv",TYPH_GHOSTS_TWO,TYPH_REAL,          &
&                       TYPH_CENTRE_CELL,TYPH_PURE,                     &
&                       Dims=[nshape,TYPH_MESH_DIM])
    ierr=TYPH_Add_Quant(dfmID,"dfm",TYPH_GHOSTS_TWO,TYPH_REAL,          &
&                       TYPH_CENTRE_CELL,TYPH_PURE,                     &
&                       Dims=[nshape,TYPH_MESH_DIM])
    ierr=TYPH_Add_Quant(eluvID,"eluv",TYPH_GHOSTS_TWO,TYPH_REAL,        &
&                       TYPH_CENTRE_CELL,TYPH_PURE,                     &
&                       Dims=[nshape,TYPH_MESH_DIM])
    ierr=TYPH_Add_Quant(elvvID,"elvv",TYPH_GHOSTS_TWO,TYPH_REAL,        &
&                       TYPH_CENTRE_CELL,TYPH_PURE,                     &
&                       Dims=[nshape,TYPH_MESH_DIM])

    ! 1D Quants
    ierr=TYPH_Add_Quant(rho05ID,"rho05",TYPH_GHOSTS_ONE,TYPH_REAL,      &
&                       TYPH_CENTRE_CELL,TYPH_PURE)
    ierr=TYPH_Add_Quant(einID,"ein",TYPH_GHOSTS_TWO,TYPH_REAL,          &
&                       TYPH_CENTRE_CELL,TYPH_PURE)
    ierr=TYPH_Add_Quant(rhoID,"rho",TYPH_GHOSTS_TWO,TYPH_REAL,          &
&                       TYPH_CENTRE_CELL,TYPH_PURE)
    ierr=TYPH_Add_Quant(elv0ID,"elv0",TYPH_GHOSTS_TWO,TYPH_REAL,        &
&                       TYPH_CENTRE_CELL,TYPH_PURE)
    ierr=TYPH_Add_Quant(elvolID,"elvol",TYPH_GHOSTS_TWO,TYPH_REAL,      &
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
    ierr=TYPH_Add_Quant_to_Phase(ADV_EXCH_EL,dfvID)
    ierr=TYPH_Add_Quant_to_Phase(ADV_EXCH_EL,einID)
    ierr=TYPH_Add_Quant_to_Phase(ADV_EXCH_EL,rhoID)
    ierr=TYPH_Add_Quant_to_Phase(ADV_EXCH_EL,cnwtID)
    ierr=TYPH_Add_Quant_to_Phase(ADV_EXCH_EL,cnmassID)

    ierr=TYPH_Add_Quant_to_Phase(ADV_EXCH_ND,dfvID)
    ierr=TYPH_Add_Quant_to_Phase(ADV_EXCH_ND,dfmID)
    ierr=TYPH_Add_Quant_to_Phase(ADV_EXCH_ND,eluvID)
    ierr=TYPH_Add_Quant_to_Phase(ADV_EXCH_ND,elvvID)
    ierr=TYPH_Add_Quant_to_Phase(ADV_EXCH_ND,elv0ID)
    ierr=TYPH_Add_Quant_to_Phase(ADV_EXCH_ND,elvolID)
    ierr=TYPH_Add_Quant_to_Phase(ADV_EXCH_ND,cnmassID)

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
    ierr=TYPH_Set_Quant_Address(dfvID,rscratch21)
    ierr=TYPH_Set_Quant_Address(dfmID,rscratch22)
    ierr=TYPH_Set_Quant_Address(einID,ein)
    ierr=TYPH_Set_Quant_Address(rhoID,rho)
    ierr=TYPH_Set_Quant_Address(eluvID,rscratch27)
    ierr=TYPH_Set_Quant_Address(elvvID,rscratch28)
    ierr=TYPH_Set_Quant_Address(elv0ID,rscratch11)
    ierr=TYPH_Set_Quant_Address(elvolID,elvol)

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

#ifndef NOMPI
  SUBROUTINE partition_mesh(nl,nk,nprocW)
    USE kinds_mod,    ONLY: ink,rlk
    USE integers_mod, ONLY: nel,nel1,nnod,nnod1,nnod2
    USE timing_mod,   ONLY: bookleaf_times
    USE TYPH_util_mod,ONLY: get_time
    INTEGER(KIND=ink), INTENT(INOUT) :: nl,nk,nprocW
    INTEGER(KIND=ink) :: npartl,nparth,ipart,neltot,nnodtot
    INTEGER(KIND=ink),DIMENSION(nl,nk) :: icolour
    REAL(KIND=rlk)                     :: t0,t1

    t0 = get_time()

    npartl=0_ink
    nparth=nprocW-1
    ipart=-1_ink
    icolour=-1_ink
    CALL rcb((nl-1_ink),(nk-1_ink),npartl,nparth,ipart,icolour)
    CALL partition((nl-1_ink),(nk-1_ink),icolour,nprocW)
    CALL transfer_partition(nel,nel1,nnod,nnod1,nnod2)

    ! Timing data
    t1 = get_time()
    t1=t1-t0
    bookleaf_times%time_in_mshprt=bookleaf_times%time_in_mshprt+t1

  END SUBROUTINE partition_mesh

  RECURSIVE SUBROUTINE rcb(nl,nk,npartl,nparth,ipart,icolour)

    USE kinds_mod,    ONLY: ink
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

  SUBROUTINE partition(nl,nk,icolour,nprocW)

    USE kinds_mod,    ONLY: ink,lok
    USE integers_mod, ONLY: nel,nnod,nshape,nel1,nel2,     &
&                           nnod1,nnod2,rankw,commS
    USE logicals_mod, ONLY: zparallel
    USE pointers_mod, ONLY: ielnd,ielownerproc,iellocglob, &
&                           indownerproc,indlocglob
    USE error_mod,    ONLY: halt

    INTEGER(KIND=ink),                 INTENT(IN):: nl,nk,nprocW
    INTEGER(KIND=ink),DIMENSION(nl,nk),INTENT(IN):: icolour
    ! local
    INTEGER(KIND=ink)                            :: iproc,ii,jj,k,kk,igst,node,nn,np
    INTEGER(KIND=ink)                            :: iel,inod,iown,ierr
    INTEGER(KIND=ink)                            :: nel_proc,nnod_proc
    INTEGER(KIND=ink)                            :: nelavg,nellow,nelhigh
    INTEGER(KIND=ink)                            :: nnodavg,nnodlow,nnodhigh
    INTEGER(KIND=ink)                            :: inodp,nnodp,nnodbdy,iowner
    LOGICAL(KIND=lok),DIMENSION(:),  ALLOCATABLE :: el_on_proc,nod_on_proc
    LOGICAL(KIND=lok),DIMENSION(:),  ALLOCATABLE :: el_tmp,nod_tmp
    INTEGER(KIND=ink),DIMENSION(:),  ALLOCATABLE :: nel_on_proc,nnod_on_proc,nnod0_on_proc
    INTEGER(KIND=ink),DIMENSION(:),  ALLOCATABLE :: nnodproc,nodowner,nod_glob_loc
    INTEGER(KIND=ink),DIMENSION(:),  ALLOCATABLE :: ielproc,ielpar
    INTEGER(KIND=ink),DIMENSION(:),  ALLOCATABLE :: nelghost,nnodghost
    INTEGER(KIND=ink),DIMENSION(:,:),ALLOCATABLE :: pariel,parnod,nodiel,nodbdy,ielndg
    INTEGER(KIND=ink),DIMENSION(3,0:NprocW-1)    :: n_on_proc,n_on_proc_t

!   convert rcb output to 1D array
    ALLOCATE(ielpar(nl*nk))
    k=0
    DO jj=1,nk
      DO ii=1,nl
        k=k+1
        ielpar(k)=icolour(ii,jj) 
      ENDDO
    ENDDO

    ! calculate sizes on each proc
    neltot = nel
    nnodtot= nnod

    ALLOCATE(nod_on_proc(nnodtot))     ! node is on this proc?
    ALLOCATE(nnod_on_proc(0:NProcW-1)) ! number of nodes on proc
    ALLOCATE(nnod0_on_proc(0:NProcW-1)) ! number of non-ghost nodes on proc
    ALLOCATE(nod_tmp(nnodtot))         ! scratch
    ALLOCATE(el_on_proc(neltot))       ! element is on this proc?
    ALLOCATE(nel_on_proc(0:NProcW-1))  ! number of elements on proc
    ALLOCATE(el_tmp(neltot))           ! scratch
    ALLOCATE(pariel(0:NProcW-1,neltot))  ! global el  id across procs
    ALLOCATE(parnod(0:NProcW-1,nnodtot)) ! global nod id across procs 
    ALLOCATE(nodproc(nnodtot))         ! processor owning node
    ALLOCATE(nnodiel(nnodtot))         ! element to node connectivity

    ! copy global ielnd. will reuse ielnd for local data
    ALLOCATE(ielndg(nshape,neltot))
    ielndg=ielnd(:,1:neltot)
    DEALLOCATE(ielnd)

    ! calculate parllel loop extents
    nelavg=neltot/NProcW
    nnodavg=nnodtot/NProcW
    IF (zparallel) THEN
      nellow=(rankW*nelavg)+1
      nnodlow=(rankW*nnodavg)+1
      IF (rankW.NE.NprocW-1) THEN
        nelhigh=(rankW*nelavg)+nelavg
        nnodhigh=(rankW*nnodavg)+nnodavg
      ELSE
        nelhigh=neltot
        nnodhigh=nnodtot
      ENDIF
    ELSE
      nellow=1_ink
      nelhigh=neltot
      nnodlow=1_ink
      nnodhigh=nnodtot
    ENDIF

    ! calculate node to element connectivity
    ALLOCATE(nodiel(nnodtot,nshape))
    nnodiel=0_ink
    nodiel =0_ink
    DO iel=1,neltot
      DO k=1,nshape
        node=ielndg(k,iel)
        nnodiel(node)=nnodiel(node)+1_ink
        nodiel(node,nnodiel(node))=iel
      ENDDO
    ENDDO

    nnod_on_proc =0_ink
    nnod0_on_proc=0_ink
    nel_on_proc  =0_ink

    DO iproc=0,NProcW-1
      el_on_proc  =.FALSE.
      nod_on_proc =.FALSE.
      el_tmp      =.FALSE.
      nod_tmp     =.FALSE.
      DO iel=1,neltot
        IF (ielpar(iel)==iproc) THEN
          el_on_proc(iel)=.TRUE.
          nel_on_proc(iproc)=nel_on_proc(iproc)+1_ink
          pariel(iproc,nel_on_proc(iproc))=iel
          DO k=1,nshape
            node=ielndg(k,iel)
            nod_on_proc(node)=.TRUE.
          ENDDO
        ENDIF
      ENDDO
      DO inod=nnodlow,nnodhigh
        IF (nod_on_proc(inod)) THEN
          nnod_on_proc(iproc)=nnod_on_proc(iproc)+1_ink
        ENDIF
      ENDDO

      IF (rankW==iproc) THEN
        nel =nel_on_proc(iproc)
      ENDIF
      nnod0_on_proc(iproc)=nnod_on_proc(iproc)

    ! now redo count, but include ghosts
      nnod_on_proc(iproc)=0_ink
      nel_on_proc(iproc) =0_ink
      el_tmp=el_on_proc
      DO igst=1,NGSTLAY  ! two layers of ghosts
        DO iel=nellow,nelhigh
          IF (el_tmp(iel)) CYCLE ! is existing real or prev level ghost
          DO k=1,nshape
            node=ielndg(k,iel)
            IF (nod_on_proc(node)) THEN
              ! if any surrounding node is on proc element must be on proc
              el_tmp(iel)=.TRUE.
              EXIT
            ENDIF
          ENDDO
        ENDDO
        DO iel=nellow,nelhigh
          IF (el_tmp(iel)) THEN
            DO k=1,nshape
              node=ielndg(k,iel)
              ! connected node must be on proc
              nod_tmp(node)=.TRUE.
            ENDDO
          ENDIF
        ENDDO
        el_on_proc =el_tmp
        nod_on_proc=nod_tmp
      ENDDO

      DO iel=nellow,nelhigh
        IF (el_on_proc(iel)) THEN
          nel_on_proc(iproc)=nel_on_proc(iproc)+1_ink
        ENDIF
      ENDDO
      DO inod=nnodlow,nnodhigh
        IF (nod_on_proc(inod)) THEN
          nnod_on_proc(iproc)=nnod_on_proc(iproc)+1_ink
        ENDIF
      ENDDO
    ENDDO

    IF (zparallel) THEN
      n_on_proc(1,:)=nel_on_proc
      n_on_proc(2,:)=nnod_on_proc
      n_on_proc(3,:)=nnod0_on_proc
      ierr=TYPH_Reduce(n_on_proc,RVal=n_on_proc_t,Op=TYPH_OP_SUM,Comm=CommS)
      nel_on_proc=n_on_proc_t(1,:)
      nnod_on_proc=n_on_proc_t(2,:)
      nnod0_on_proc=n_on_proc_t(3,:)
    ENDIF
    nnod=nnod0_on_proc(rankW)

    DEALLOCATE(nod_on_proc,nod_tmp,nnod0_on_proc)
    DEALLOCATE(el_on_proc,el_tmp)

    ALLOCATE(nnodproc(0:NprocW-1),nodbdy(nshape,nnodtot),nodowner(nnodtot))

    nnodbdy=0_ink
    nodbdy =-1_ink  ! partitioner sanity checker
    nodproc=0_ink   ! processor that node is on (negative = boundary node, value=owner+/-1)
    nnodproc=0_ink
    nodowner=-1_ink  ! holds node equivalent of ielpar

    DO iproc=0,NProcW-1
      DO ii=1,neltot
        IF (ielpar(ii)==iproc) THEN
          shapeloop: DO k=1,nshape
            node=ielndg(k,ii)
            np=nodproc(node)
            ! has this node been registered on another partition (ie on a boundary?)
            IF (np.NE.0_ink.AND.np.NE.iproc+1) THEN  ! shared (boundary) node
              IF (iproc.LT.(ABS(nodproc(node))-1)) THEN
                iown=-(iproc+1_ink)
              ELSE
                iown=-ABS(nodproc(node))
              ENDIF
              nodproc(node)=iown ! lowest proc owns this boundary node
              np=nodproc(node)
              ! Check that it hasn't already been registered
              DO nn=1,nnodproc(iproc)
                IF (parnod(iproc,nn)==node) CYCLE shapeloop
              ENDDO
              ! if not add it to the stack
              nnodproc(iproc)=nnodproc(iproc)+1_ink
              parnod(iproc,nnodproc(iproc))=node
              nnodbdy=nnodbdy+1_ink
!             check if it is on more than nshape procs
              IF (nodbdy(nshape,nnodbdy).NE.-1_ink) THEN
                PRINT*,"ERROR: Node",node," on more than nshape procs"
                CALL halt("Error in partitioner",1,zend=.true.)
              ENDIF
              DO kk=1,nshape
                IF (nodbdy(kk,nnodbdy)==-1_ink) THEN
                  nodbdy(kk,nnodbdy)=iproc
                  EXIT
                ENDIF
              ENDDO
              ! if not on a boundary it is only only iproc for now
            ELSEIF (np==0_ink) THEN
              nodproc(node)=iproc+1
              nnodproc(iproc)=nnodproc(iproc)+1_ink
              parnod(iproc,nnodproc(iproc))=node
            ENDIF
            ! if nodowner already has an entry assume lowest iproc owns node. Else:
            IF (.NOT.(nodowner(node).NE.-1_ink .AND. nodowner(node).NE.iproc)) THEN
              nodowner(node)=iproc
            ENDIF
          ENDDO shapeloop
        ENDIF
      ENDDO
    ENDDO

    DEALLOCATE(nodbdy)

    ALLOCATE(nelghost(NGSTLAY))
    ALLOCATE(nnodghost(NGSTLAY))
    nelghost =0_ink
    nnodghost=0_ink
    nel_proc=nel_on_proc(rankW)
    nnod_proc=nnod_on_proc(rankW)
    CALL get_ghosts(nshape,rankW,nelghost,nnodghost, &
&                   ielpar,nnodproc,nel_proc,   &
&                   nnod_proc,pariel,parnod,nodiel,ielndg)

    nel1=nelghost(1)
    nel2=nelghost(2)
    nnod1=nnodghost(1)
    nnod2=nnodghost(2)
    DEALLOCATE(nelghost,nnodghost)
    DEALLOCATE(nodiel,nnodiel,nodproc)

    ! New local connectivity
    ALLOCATE(ielnd(nshape,1:nel2))
    ALLOCATE(nod_glob_loc(nnodtot))
    ielnd=0_ink
    nod_glob_loc=0_ink
    DO ii=1,nnod2
      nod_glob_loc(parnod(rankW,ii))=ii
    ENDDO
    DO k=1,nshape
      DO ii=1,nel2
        iel=pariel(rankW,ii)
        node=ielndg(k,iel)
        jj=nod_glob_loc(node)
        IF (jj==0) CYCLE
        ielnd(k,ii)=jj
      ENDDO
    ENDDO
    DEALLOCATE(nod_glob_loc,ielndg)

    ! ownership of elements and nodes for comms and local to global mapping

    ALLOCATE(ielownerproc(2,nel2))
    ALLOCATE(iellocglob(nel2))
    ielownerproc=-2000000_ink
    iellocglob=-2000000_ink
    DO iel=1,nel2
      iellocglob(iel)=pariel(rankW,iel)
      iowner=ielpar(iellocglob(iel))
      ielownerproc(1,iel)=iowner
      IF (iowner==rankW) THEN
        ielownerproc(2,iel)=iel
      ELSE
        DO jj=1,neltot
          IF (pariel(iowner,jj)==iellocglob(iel)) THEN
            ielownerproc(2,iel)=jj
            EXIT
          ENDIF
        ENDDO
      ENDIF
    ENDDO

    ALLOCATE(indownerproc(2,nnod2))
    ALLOCATE(indlocglob(nnod2))
    indownerproc=-2000000_ink
    indlocglob=-2000000_ink
    DO inod=1,nnod2
      indlocglob(inod)=parnod(rankW,inod)
      iowner=nodowner(indlocglob(inod))
      indownerproc(1,inod)=iowner
      IF (iowner==rankW) THEN
        indownerproc(2,inod)=inod
      ELSE
        DO jj=1,nnodtot
          IF (parnod(iowner,jj)==indlocglob(inod)) THEN
            indownerproc(2,inod)=jj
            EXIT
          ENDIF
        ENDDO
      ENDIF
    ENDDO

    DEALLOCATE(ielpar,pariel,nodowner,parnod,nel_on_proc,nnod_on_proc)

  END SUBROUTINE partition 

  SUBROUTINE get_ghosts(nshape,iproc,nelghost,nnodghost,ielpar, &
                        nnodproc,nel_proc,nnod_proc,pariel,parnod,nodiel,ielnd)
    USE kinds_mod,    ONLY: ink,lok
    USE integers_mod, ONLY: nel,nnod,rankW
    ! Find ghosts for this proc
    INTEGER(KIND=ink), INTENT(IN) :: nshape,iproc,nel_proc,nnod_proc
    INTEGER(KIND=ink),DIMENSION(:),              INTENT(IN)   :: ielpar
    INTEGER(KIND=ink),DIMENSION(:),              INTENT(IN)   :: nnodproc
    INTEGER(KIND=ink),DIMENSION(:),  ALLOCATABLE,INTENT(INOUT):: nelghost,nnodghost
    INTEGER(KIND=ink),DIMENSION(:,:),ALLOCATABLE,INTENT(INOUT):: pariel,parnod,nodiel
    INTEGER(KIND=ink),DIMENSION(:,:),ALLOCATABLE,INTENT(IN)   :: ielnd

    INTEGER(KIND=ink) :: ii,iel,jj,k,kk,node,nelprev,nnodprev,nelbnd
    INTEGER(KIND=ink),DIMENSION(:),ALLOCATABLE :: ielprocbdy
    LOGICAL(KIND=lok),DIMENSION(:),ALLOCATABLE :: zelproc, znodproc
    LOGICAL(KIND=lok),DIMENSION(:,:),ALLOCATABLE :: zelghost,znodghost

    ALLOCATE(ielprocbdy(neltot))
    ALLOCATE(zelproc(neltot))
    ALLOCATE(znodproc(nnodtot))
    ALLOCATE(zelghost(0:NGSTLAY-1,neltot))
    ALLOCATE(znodghost(0:NGSTLAY-1,nnodtot))

    ielprocbdy=0_ink
    zelproc  =.FALSE.
    znodproc =.FALSE.
    zelghost =.FALSE.
    znodghost=.FALSE.

    ! find all elements at the processor boundary and put into ielprocbdy
    ielprocbdy=0_ink
    DO iel=1,neltot
      IF (ielpar(iel)==iproc) THEN
        zelproc(iel)=.TRUE.
        DO k=1,nshape
          node=ielnd(k,iel)
          znodproc(node)=.TRUE.
        ENDDO
      ENDIF
    ENDDO

    kk=0_ink

    el_loop: DO ii=1,nel
      iel=pariel(iproc,ii)
      DO k=1,nshape
        node=ielnd(k,iel)
        IF (nodproc(node).LT.0) THEN
          ! check if this element is already stored
          DO jj=1,kk
            IF (ielprocbdy(kk)==iel) CYCLE el_loop
          ENDDO
          kk=kk+1
          ielprocbdy(kk)=iel
        ENDIF
      ENDDO
    ENDDO el_loop
    nelprev=nel
    nnodprev=nnod
    nelbnd=kk
    call get_ghost_layer(0,nel_proc,nnod_proc,nshape,nelprev,nnodprev,nelbnd,ielprocbdy,zelghost, &
&                        znodghost,parnod,pariel,nodiel,nnodproc,ielnd,ielpar,znodproc)
    nelghost(1)=nelprev
    nnodghost(1)=nnodprev
    call get_ghost_layer(1,nel_proc,nnod_proc,nshape,nelprev,nnodprev,nelbnd,ielprocbdy,zelghost, &
&                        znodghost,parnod,pariel,nodiel,nnodproc,ielnd,ielpar,znodproc)
    nelghost(2)=nelprev
    nnodghost(2)=nnodprev

    DEALLOCATE(zelghost,znodghost,ielprocbdy)

  END SUBROUTINE get_ghosts

  SUBROUTINE get_ghost_layer(ilayer,nel_proc,nnod_proc,nshape,nelprev,nnodprev,    &
&                            nelbnd,iboundlist,zelghost,znodghost, &
                             parnod,pariel,nodiel,nnodproc,ielnd,ielpar,znodproc)
    USE kinds_mod,    ONLY: ink,lok
    USE integers_mod, ONLY: nel,rankW

    INTEGER(KIND=ink),                 INTENT(IN)    :: ilayer,nel_proc,nnod_proc,nshape
    INTEGER(KIND=ink),                 INTENT(INOUT) :: nelprev  ! total no els so far
    INTEGER(KIND=ink),                 INTENT(OUT)   :: nnodprev  ! total no nodes so far
    INTEGER(KIND=ink),                 INTENT(INOUT) :: nelbnd    ! no. boundary els
    INTEGER(KIND=ink), DIMENSION(:),ALLOCATABLE,INTENT(INOUT) :: iboundlist
    LOGICAL(KIND=lok), DIMENSION(0:,:), INTENT(INOUT) :: zelghost
    LOGICAL(KIND=lok), DIMENSION(0:,:), INTENT(INOUT) :: znodghost
    INTEGER(KIND=ink),DIMENSION(0:,:),  INTENT(INOUT) :: parnod,pariel
    INTEGER(KIND=ink),DIMENSION(:,:),   INTENT(IN)    :: nodiel,ielnd
    INTEGER(KIND=ink),DIMENSION(0:),    INTENT(IN)    :: nnodproc
    INTEGER(KIND=ink),DIMENSION(:),     INTENT(IN)    :: ielpar
    LOGICAL(KIND=lok), DIMENSION(:),    INTENT(IN)    :: znodproc

    INTEGER(KIND=ink) :: ii,iel,ielg,inodg,jj,k,kk,kkk,nnn,node
    INTEGER(KIND=ink), DIMENSION(:), ALLOCATABLE :: ibndelnod  ! list of boundary cell nodes
    INTEGER(KIND=ink), DIMENSION(:), ALLOCATABLE :: ibndelnod1 ! packed list of ibndelnod
    INTEGER(KIND=ink), DIMENSION(:), ALLOCATABLE :: ghostel,ghostnod

    ! create connectivity list based on boundary elements
    ALLOCATE(ibndelnod(nelbnd*4_ink))
    ibndelnod=RESHAPE(ielnd(:,iboundlist(1:nelbnd)),(/nelbnd*4_ink/))
    DO ii=1,nelbnd*4_ink
      IF (ibndelnod(ii)==0_ink) CYCLE
      DO jj=ii+1,nelbnd*4_ink
        ! zero duplicates
        IF (ibndelnod(jj) == ibndelnod(ii)) ibndelnod(jj)=0_ink
      ENDDO
    ENDDO
    ! Now pack into scratch array
    ALLOCATE(ibndelnod1(COUNT(ibndelnod.NE.0_ink)))
    ibndelnod1=PACK(ibndelnod,ibndelnod.NE.0_ink)
    ! find ghost elements
    DO jj=1,SIZE(ibndelnod1)
      node=ibndelnod1(jj)
      midloop: DO ii=1,nnodiel(node)
        iel=nodiel(node,ii)
        DO kk=0,ilayer
          IF ((ielpar(iel)==rankW).OR.zelghost(kk,iel)) CYCLE midloop
        ENDDO
        zelghost(ilayer,iel)=.TRUE.
      ENDDO midloop
    ENDDO
    DEALLOCATE(ibndelnod,ibndelnod1)

    kkk=0_ink
    nnn=0_ink

    ALLOCATE(ghostel(nel_proc),ghostnod(nnod_proc))

    DO iel=1,neltot
      IF (zelghost(ilayer,iel)) THEN
        ! elements
        kkk=kkk+1_ink
        ghostel(kkk)=iel
        ielg=nelprev+kkk
        pariel(rankW,ielg)=iel
        ! nodes        
        nodloop: DO k=1,nshape
          node=ielnd(k,iel)
          DO jj=0,ilayer
            IF (znodproc(node).OR.znodghost(jj,node)) CYCLE nodloop
          ENDDO
          nnn=nnn+1_ink
          ghostnod(nnn)=node
          inodg=nnodprev+nnn
          parnod(rankW,inodg)=node
          znodghost(ilayer,node)=.TRUE.
        ENDDO nodloop
      ENDIF
    ENDDO

    nelbnd=kkk
    nelprev=kkk+nelprev
    nnodprev=nnn+nnodprev
    IF (ALLOCATED(iboundlist)) DEALLOCATE(iboundlist)
    ALLOCATE(iboundlist(kkk))
    iboundlist=0_ink
    DO iel=1,kkk
      iboundlist(iel)=ghostel(iel)
    ENDDO
    DEALLOCATE(ghostel)

  END SUBROUTINE get_ghost_layer

  SUBROUTINE transfer_partition(nel,nel1,nnod,nnod1,nnod2)
    USE kinds_mod,   ONLY: ink,rlk
    USE pointers_mod,ONLY: ndx,ndy,ielreg,indtype,ielmat,ndu,ndv, &
&                          iellocglob,indlocglob

    INTEGER(KIND=ink), INTENT(IN) :: nel,nel1,nnod,nnod1,nnod2
    ! local
    INTEGER(KIND=ink) :: ii,iig
    REAL(KIND=rlk),    DIMENSION(:), ALLOCATABLE :: gndx,gndy,gndu,gndv
    INTEGER(KIND=ink), DIMENSION(:), ALLOCATABLE :: gielreg,gindtype,gielmat

    ! copy global data into temproaries then reallocate global arrays to local
    ALLOCATE(gndx(1:nnodtot),gndy(1:nnodtot),gndu(1:nnodtot),gndv(1:nnodtot))
    ALLOCATE(gindtype(1:nnodtot),gielreg(1:neltot),gielmat(1:neltot))
    gndx=ndx
    gndy=ndy
    gndu=ndu
    gndv=ndv
    gielreg=ielreg
    gielmat=ielmat
    gindtype=indtype

    DEALLOCATE(ndx,ndy,ielreg,indtype,ielmat,ndu,ndv)
    ALLOCATE(ndx(1:nnod1),ndy(1:nnod1),ndu(1:nnod1),ndv(1:nnod1))
    ALLOCATE(indtype(1:nnod2),ielmat(1:nel1),ielreg(1:nel1))

    DO ii=1,nnod
      DO iig=1,nnodtot
        IF (indlocglob(ii)==iig) THEN
          ndx(ii)=gndx(iig)
          ndy(ii)=gndy(iig)
          ndu(ii)=gndu(iig)
          ndv(ii)=gndv(iig)
          indtype(ii)=gindtype(iig)
        ENDIF
      ENDDO
    ENDDO

    DO ii=1,nel
      DO iig=1,neltot
        IF (iellocglob(ii)==iig) THEN
          ielmat(ii) =gielmat(iig)
          ielreg(ii) =gielreg(iig)
        ENDIF
      ENDDO
    ENDDO

    DEALLOCATE(gndx,gndy,gndu,gndv,gielreg,gindtype,gielmat)

  END SUBROUTINE transfer_partition
#endif

END MODULE comms_mod
















