
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
  USE kinds_mod,    ONLY: rlk,ink,lok

  IMPLICIT NONE

  INTEGER(KIND=ink)    :: neltot,nnodtot
  INTEGER(KIND=TSIZEK) :: VISCOSITY,HALFSTEP
  INTEGER(KIND=TSIZEK) :: key_comm_cells,key_comm_nodes
  INTEGER(KIND=TSIZEK) :: cnmassID,cnwtID,elfxID,elfyID,rho05ID,duID,  &
&                         dvID,dxID,dyID
  INTEGER(KIND=ink),DIMENSION(:),ALLOCATABLE   :: nodproc,nnodiel
  INTEGER(KIND=ink), PARAMETER                 :: NGSTLAY=2_ink

  PRIVATE
  PUBLIC :: register,exchange,rcb,VISCOSITY,HALFSTEP,partition_mesh

CONTAINS

  SUBROUTINE register()

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

  SUBROUTINE partition_mesh(nl,nk,nprocW)
    USE integers_mod, ONLY: nel,nel1,nnod,nnod1
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
    CALL transfer_partition(nel,nel1,nnod,nnod1)

    ! Timing data
    t1 = get_time()
    t1=t1-t0
    bookleaf_times%time_in_mshprt=bookleaf_times%time_in_mshprt+t1

  END SUBROUTINE partition_mesh

  RECURSIVE SUBROUTINE rcb(nl,nk,npartl,nparth,ipart,icolour)

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

    USE integers_mod, ONLY: nel,nnod,nshape,nel1,nnod1
    USE paradef_mod,  ONLY: e_owner_proc,n_owner_proc,e_loc_glob,      &
&                           n_loc_glob,rankW,MprocW,zparallel
    USE pointers_mod, ONLY: ielnod
    USE error_mod,    ONLY: halt
#ifndef NOMPI
    USE mpi
#endif

    INTEGER(KIND=ink),              INTENT(IN)   :: nl,nk,nprocW
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
    INTEGER(KIND=ink),DIMENSION(:,:),ALLOCATABLE :: pariel,parnod,nodiel,nodbdy,ielnodg
    INTEGER(KIND=ink),DIMENSION(3,0:NprocW-1)    :: n_on_proc,n_on_proc_t

!   NEED TO CONVERT COLOUR to local cell ID

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

    ! copy global ielnod. will reuse ielnod for local data
    ALLOCATE(ielnodg(nshape,neltot))
    ielnodg=ielnod(:,1:neltot)
    DEALLOCATE(ielnod)

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

    ! calculate element to node connectivity
    ALLOCATE(nodiel(nnodtot,nshape))
    ! now calculate connectivity
    nnodiel=0_ink
    nodiel =0_ink
    DO iel=1,neltot
      DO k=1,nshape
        node=ielnodg(k,iel)
        nnodiel(node)=nnodiel(node)+1_ink
        nodiel(node,nnodiel(node))=iel
      ENDDO
    ENDDO

    nnod_on_proc =0_ink
    nnod0_on_proc =0_ink
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
            node=ielnodg(k,iel)
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
            node=ielnodg(k,iel)
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
              node=ielnodg(k,iel)
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

#ifndef NOMPI
    IF (zparallel) THEN
      n_on_proc(1,:)=nel_on_proc
      n_on_proc(2,:)=nnod_on_proc
      n_on_proc(3,:)=nnod0_on_proc
      CALL MPI_ALLREDUCE(n_on_proc,n_on_proc_t,3*NprocW,MPI_INTEGER, &
                         MPI_SUM,MPI_COMM_WORLD,ierr)
      nel_on_proc=n_on_proc_t(1,:)
      nnod_on_proc=n_on_proc_t(2,:)
      nnod0_on_proc=n_on_proc_t(3,:)
    ENDIF
#endif
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
            node=ielnodg(k,ii)
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
&                   nnod_proc,pariel,parnod,nodiel,ielnodg)

    nel1=nelghost(1)
!    nel2=nelghost(2)
    nnod1=nnodghost(1)
!    nnod2=nnodghost(2)
    DEALLOCATE(nelghost,nnodghost)
    DEALLOCATE(nodiel,nnodiel,nodproc)

    ! New local connectivity
    ALLOCATE(ielnod(nshape,0:nel1))
    ALLOCATE(nod_glob_loc(nnodtot))
    ielnod=0_ink
    nod_glob_loc=0_ink
    DO ii=1,nnod1
      nod_glob_loc(parnod(rankW,ii))=ii
    ENDDO
    DO k=1,nshape
      DO ii=1,nel1
        iel=pariel(rankW,ii)
        node=ielnodg(k,iel)
        jj=nod_glob_loc(node)
        IF (jj==0) CYCLE
        ielnod(k,ii)=jj
      ENDDO
    ENDDO
    DEALLOCATE(nod_glob_loc,ielnodg)

    ! ownership of elements and nodes for comms and local to global mapping

    ALLOCATE(e_owner_proc(2,nel1))
    ALLOCATE(e_loc_glob(nel1))
    e_owner_proc=-2000000_ink
    e_loc_glob=-2000000_ink
    DO iel=1,nel1
      e_loc_glob(iel)=pariel(rankW,iel)
      iowner=ielpar(e_loc_glob(iel))
      e_owner_proc(1,iel)=iowner
      IF (iowner==rankW) THEN
        e_owner_proc(2,iel)=iel
      ELSE
        DO jj=1,neltot
          IF (pariel(iowner,jj)==e_loc_glob(iel)) THEN
            e_owner_proc(2,iel)=jj
            EXIT
          ENDIF
        ENDDO
      ENDIF
    ENDDO

    ALLOCATE(n_owner_proc(2,nnod1))
    ALLOCATE(n_loc_glob(nnod1))
    n_owner_proc=-2000000_ink
    n_loc_glob=-2000000_ink
    DO inod=1,nnod1
      n_loc_glob(inod)=parnod(rankW,inod)
      iowner=nodowner(n_loc_glob(inod))
      n_owner_proc(1,inod)=iowner
      IF (iowner==rankW) THEN
        n_owner_proc(2,inod)=inod
      ELSE
        DO jj=1,nnodtot
          IF (parnod(iowner,jj)==n_loc_glob(inod)) THEN
            n_owner_proc(2,inod)=jj
            EXIT
          ENDIF
        ENDDO
      ENDIF
    ENDDO

    DEALLOCATE(ielpar,pariel,nodowner,parnod,nel_on_proc,nnod_on_proc)

  END SUBROUTINE partition 

  SUBROUTINE get_ghosts(nshape,iproc,nelghost,nnodghost,ielpar, &
                        nnodproc,nel_proc,nnod_proc,pariel,parnod,nodiel,ielnod)
    USE integers_mod, ONLY: nel,nnod
    USE paradef_mod,  ONLY: rankW
    ! Find ghosts for this proc
    INTEGER(KIND=ink), INTENT(IN) :: nshape,iproc,nel_proc,nnod_proc
    INTEGER(KIND=ink),DIMENSION(:),              INTENT(IN)   :: ielpar
    INTEGER(KIND=ink),DIMENSION(:),              INTENT(IN)   :: nnodproc
    INTEGER(KIND=ink),DIMENSION(:),  ALLOCATABLE,INTENT(INOUT):: nelghost,nnodghost
    INTEGER(KIND=ink),DIMENSION(:,:),ALLOCATABLE,INTENT(INOUT):: pariel,parnod,nodiel
    INTEGER(KIND=ink),DIMENSION(:,:),ALLOCATABLE,INTENT(IN)   :: ielnod

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
          node=ielnod(k,iel)
          znodproc(node)=.TRUE.
        ENDDO
      ENDIF
    ENDDO

    kk=0_ink

    el_loop: DO ii=1,nel
      iel=pariel(iproc,ii)
      DO k=1,nshape
        node=ielnod(k,iel)
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
&                        znodghost,parnod,pariel,nodiel,nnodproc,ielnod,ielpar,znodproc)
    nelghost(1)=nelprev
    nnodghost(1)=nnodprev
    call get_ghost_layer(1,nel_proc,nnod_proc,nshape,nelprev,nnodprev,nelbnd,ielprocbdy,zelghost, &
&                        znodghost,parnod,pariel,nodiel,nnodproc,ielnod,ielpar,znodproc)
    nelghost(2)=nelprev
    nnodghost(2)=nnodprev

    DEALLOCATE(zelghost,znodghost,ielprocbdy)

  END SUBROUTINE get_ghosts

  SUBROUTINE get_ghost_layer(ilayer,nel_proc,nnod_proc,nshape,nelprev,nnodprev,    &
&                            nelbnd,iboundlist,zelghost,znodghost, &
                             parnod,pariel,nodiel,nnodproc,ielnod,ielpar,znodproc)
    USE integers_mod, ONLY: nel
    USE paradef_mod,  ONLY: rankW

    INTEGER(KIND=ink),                 INTENT(IN)    :: ilayer,nel_proc,nnod_proc,nshape
    INTEGER(KIND=ink),                 INTENT(INOUT) :: nelprev  ! total no els so far
    INTEGER(KIND=ink),                 INTENT(OUT)   :: nnodprev  ! total no nodes so far
    INTEGER(KIND=ink),                 INTENT(INOUT) :: nelbnd    ! no. boundary els
    INTEGER(KIND=ink), DIMENSION(:),ALLOCATABLE,INTENT(INOUT) :: iboundlist
    LOGICAL(KIND=lok), DIMENSION(0:,:), INTENT(INOUT) :: zelghost
    LOGICAL(KIND=lok), DIMENSION(0:,:), INTENT(INOUT) :: znodghost
    INTEGER(KIND=ink),DIMENSION(0:,:),  INTENT(INOUT) :: parnod,pariel
    INTEGER(KIND=ink),DIMENSION(:,:),   INTENT(IN)    :: nodiel,ielnod
    INTEGER(KIND=ink),DIMENSION(0:),    INTENT(IN)    :: nnodproc
    INTEGER(KIND=ink),DIMENSION(:),     INTENT(IN)    :: ielpar
    LOGICAL(KIND=lok), DIMENSION(:),    INTENT(IN)    :: znodproc

    INTEGER(KIND=ink) :: ii,iel,ielg,inodg,jj,k,kk,kkk,nnn,node
    INTEGER(KIND=ink), DIMENSION(:), ALLOCATABLE :: ibndelnod  ! list of boundary cell nodes
    INTEGER(KIND=ink), DIMENSION(:), ALLOCATABLE :: ibndelnod1 ! packed list of ibndelnod
    INTEGER(KIND=ink), DIMENSION(:), ALLOCATABLE :: ghostel,ghostnod

    ! create connectivity list based on boundary elements
    ALLOCATE(ibndelnod(nelbnd*4_ink))
    ibndelnod=RESHAPE(ielnod(:,iboundlist(1:nelbnd)),(/nelbnd*4_ink/))
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
          node=ielnod(k,iel)
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

  SUBROUTINE transfer_partition(nel,nel1,nnod,nnod1)
    USE paradef_mod,  ONLY: e_loc_glob,n_loc_glob
    USE pointers_mod,ONLY: ndx,ndy,ielreg,indtype,ielmat,ndu,ndv

    INTEGER(KIND=ink), INTENT(IN) :: nel,nel1,nnod,nnod1
    ! local
    INTEGER(KIND=ink) :: ii,iig
    REAL(KIND=rlk),    DIMENSION(:), ALLOCATABLE :: gndx,gndy,gndu,gndv
    INTEGER(KIND=ink), DIMENSION(:), ALLOCATABLE :: gielreg,gindtype,gielmat

    ! copy global data into temproaries then reallocate global arrays to local
    ALLOCATE(gndx(0:nnodtot),gndy(0:nnodtot),gndu(0:nnodtot),gndv(0:nnodtot))
    ALLOCATE(gindtype(0:nnodtot),gielreg(0:neltot),gielmat(0:neltot))
    gndx=ndx
    gndy=ndy
    gndu=ndu
    gndv=ndv
    gielreg=ielreg
    gielmat=ielmat
    gindtype=indtype

    DEALLOCATE(ndx,ndy,ielreg,indtype,ielmat,ndu,ndv)
    ALLOCATE(ndx(0:nnod1),ndy(0:nnod1),ndu(0:nnod1),ndv(0:nnod1))
    ALLOCATE(indtype(0:nnod1),ielmat(0:nel1),ielreg(0:nel1))

    DO ii=1,nnod
      DO iig=1,nnodtot
        IF (n_loc_glob(ii)==iig) THEN
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
        IF (e_loc_glob(ii)==iig) THEN
          ielmat(ii) =gielmat(iig)
          ielreg(ii) =gielreg(iig)
        ENDIF
      ENDDO
    ENDDO

    DEALLOCATE(gndx,gndy,gndu,gndv,gielreg,gindtype,gielmat)

  END SUBROUTINE transfer_partition

END MODULE comms_mod
















