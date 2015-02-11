
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
  INTEGER(KIND=ink),DIMENSION(:),ALLOCATABLE   :: nodproc,nnodiel

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

  SUBROUTINE partition_mesh(nl,nk)
    INTEGER(KIND=ink), INTENT(IN) :: nl,nk
    INTEGER(KIND=ink) :: npartl,nparth,ipart
    INTEGER(KIND=ink),DIMENSION(nl,nk) :: icolour

    npartl=0_ink
    npartl=NProcW-1
    ipart=-1_ink
    icolour=-1_ink
    CALL rcb(nl,nk,npartl,nparth,ipart,icolour)
    CALL partition(nl,nk,icolour)
    CALL transfer_partition(nel1,nnod1)
  END SUBROUTINE partition_mesh

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

  SUBROUTINE partition(nl,nk,icolour)

    USE paradef_mod,  ONLY: e_owner_proc,n_owner_proc,e_loc_glob,      &
&                           n_loc_glob
    USE pointers_mod, ONLY: ielnod
    INTEGER(KIND=ink),                 INTENT(IN):: nl,nk
    INTEGER(KIND=ink),DIMENSION(nl,nk),INTENT(IN):: icolour
    ! local
    INTEGER(KIND=ink), PARAMETER                 :: NGSTLAY=2_ink
    INTEGER(KIND=ink)                            :: iproc,ii,k,igst,node,np
    INTEGER(KIND=ink)                            :: iel,inod
    INTEGER(KIND=ink)                            :: inodp,nnodp
    LOGICAL(KIND=lok),DIMENSION(:),  ALLOCATABLE :: el_on_proc,nod_on_proc
    LOGICAL(KIND=lok),DIMENSION(:),  ALLOCATABLE :: el_tmp,nod_tmp
    INTEGER(KIND=ink),DIMENSION(:),  ALLOCATABLE :: nel_on_proc,nnod_on_proc
    INTEGER(KIND=ink),DIMENSION(:),  ALLOCATABLE :: ielproc,inodproc,ielpar
    INTEGER(KIND=ink),DIMENSION(:,:),ALLOCATABLE :: pariel,nodiel,nodbdy
    LOGICAL(KIND=lok),DIMENSION(:,:),ALLOCATABLE :: nelghost,nnodghost
    INTEGER(KIND=ink),DIMENSION(:,:),POINTER     :: ielnodg=>NULL()  ! global connectivity

!   NEED TO CONVERT COLOUR to local cell ID

    ALLOCATE(ielpar(nl*nk))
    k=0
    DO ii=1,nl
      DO jj=1,nk
        k=k+1
        ielpar(k)=icolour(ii,jj) 
      ENDDO
    ENDDO

    ! calculate sizes on each proc
    nel_tot = nel
    nnod_tot= nnod

    ALLOCATE(nod_on_proc(nnod_tot))     ! node is on this proc?
    ALLOCATE(nnod_on_proc(0:NProcW))    ! number of nodes on proc
    ALLOCATE(nod_tmp(nnod_tot))         ! scratch
    ALLOCATE(el_on_proc(nel_tot))       ! element is on this proc?
    ALLOCATE(nel_on_proc(0:NProcW))     ! number of elements on proc
    ALLOCATE(el_tmp(nel_tot))           ! scratch
    ALLOCATE(pariel(0:NProcW,nel_tot))  ! global el  id across procs
    ALLOCATE(parnod(0:NProcW,nnod_tot)) ! global nod id across procs 
    ALLOCATE(nodproc(nnod_tot))         ! processor owning node
    ALLOCATE(nnodiel(nnod_tot))         ! element to node connectivity

    ! point to global ielnod. Will reuse ielnod itself for local data
    ielnodg=>ielnod
    NULLIFY(ielnod)

    ! calculate element to node connectivity
!     nnodiel=0_ink
!     ! first, get sizes for allocation
!     DO ii=1,nel_tot
!       DO k=1,nshape
!         node=ielnodg(k,ii)
!         nnodiel(node)=nnodiel(node)+1_ink
!       ENDDO
!     ENDDO
!     ALLOCATE(nodiel(nnod_tot,MAXVAL(nnodiel)))  ! nnodiel = nshape?
    ALLOCATE(nodiel(nnod_tot,nshape))  ! nnodiel = nshape?
    ! now calculate connectivity
    nnodiel=0_ink
    DO ii=1,nel_tot
      DO k=1,nshape
        node=ielnodg(k,ii)
        nnodiel(node)=nnodiel(node)+1_ink
        nodiel(node,nnodiel(node))=ii
      ENDDO
    ENDDO
        
    nnod_on_proc=0_ink
    nel_on_proc =0_ink

    DO iproc=0,NProcW-1
      el_on_proc  =.FALSE.
      nod_on_proc =.FALSE.
      el_tmp      =.FALSE.
      nod_tmp     =.FALSE.
      DO ii=1,nel_tot
        IF (ielpar(ii)==iproc) THEN
          el_on_proc(ii)=.TRUE.
          nel_on_proc(iproc)=nel_on_proc(iproc)+1_ink
          pariel(iproc,nel_on_proc(iproc)=ii
          DO k=1,nshape
            node=ielnodg(k,ii)
            nod_on_proc(iproc)=.TRUE.
          ENDDO
        ENDIF
      ENDDO
      DO ii=1,nnod_tot
        IF (nod_on_proc(iproc)) THEN
          nnod_on_proc(iproc)=nnod_on_proc(iproc)+1_ink
        ENDIF
      ENDDO
      IF (rankW==iproc) THEN
        nel  = nel_on_proc(iproc)
        nnod = nnod_on_proc(iproc)
      ENDIF
      IF (MProcW) THEN
        WRITE(6,'(3(A8,I8))') "   Rank=",iproc,"    nel=", nel, "   nnod=",nnod
      ENDIF

    ! now redo count, but include ghosts
      nnod_on_proc=0_ink
      nel_on_proc =0_ink

      DO igst=1,NGSTLAY  ! two layers of ghosts
        DO ii=1,nel_tot
          DO k=1,nshape
            node=ielnodg(k,ii)
            IF (nod_on_proc(iproc)) THEN
              el_tmp(ii)=.TRUE.
            ENDIF
          ENDDO
        ENDDO
        DO ii=1,nel_tot
          IF (el_tmp(ii)) THEN
            DO k=1,nshape
              node=ielnodg(k,ii)
              nod_tmp(ii)=.TRUE.
            ENDDO
          ENDIF
        ENDDO
        nod_on_proc=nod_tmp
        el_on_proc =el_tmp
      ENDDO

      DO ii=1,nel_tot
        IF (el_on_proc(ii)) THEN
          nel_on_proc(iproc)=nel_on_proc(iproc)+1_ink
        ENDIF
      ENDDO
      DO ii=1,nnod_tot
        IF (nod_on_proc(ii)) THEN
          nnod_on_proc(iproc)=nnod_on_proc(iproc)+1_ink
        ENDIF
      ENDDO
      IF (MProcW) THEN
        WRITE(6,'(3(A8,I8))') "   Rank=",iproc,"  nel+g=", nel_on_proc, " nnod+g=",nnod_on_proc
      ENDIF
    ENDDO
    ! Max no. elements and nodes
    maxel  = MAX(nel_on_proc)
    maxnod = MAX(nnod_on_proc)

    DEALLOCATE(nod_on_proc,nod_tmp)
    DEALLOCATE(el_on_proc,el_tmp)        

    ALLOCATE(nnodproc(0:NprocW-1),nodbdy(nshape,nnod_tot),nodpart3(nnod_tot))

    nnodbody=0_ink
    nodbdy  =-1_ink  ! partitioner sanity checker
    nodproc =0_ink
    nodpart3(node)=-1_ink  ! holds node equivalent of ielpar

    DO iproc=0,NProcW-1
      DO ii=1,nel_tot
        DO k=1,nshape
          node=ielnodg(k,ii)
          IF (ielpar(ii)==iproc) THEN
            np=nodproc(node)
            ! has this node been registered on another partition (ie on a boundary?)
            IF (np.NE.0_ink.AND.np.NE.iproc+1) THEN
              nodproc(node)=-ABS(iproc+1)
              np=nodproc(node)
              ! Check that it hasn't already been registered
              DO nn=1,nnodproc(iproc-1)
                IF (parnod(iproc,nn)==node) CYCLE
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
              DO k=1,nshape
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
            ! if nodpart3 already has an entry assume lowest iproc owns node. Else:
            IF (.NOT.(nodpart3(node).NE.-1_ink .AND. nodpart3(node).NE.iproc)) THEN
              nodpart3(node)=iproc  
            ENDIF
          ENDIF
        ENDDO
      ENDDO
    ENDDO

    DEALLOCATE(nodbdy)

    ALLOCATE(nelghost(NGSTLAY))
    ALLOCATE(nnodghost(NGSTLAY))
    DO iproc=0,NProcW-1
      nel_proc=nel_on_proc(iproc)
      nnod_proc=nnod_on_proc(iproc)
      ALLOCATE(ielproc(nel_proc))
      ALLOCATE(inodproc(nnod_proc))
      ielproc =pariel(iproc,1:nel_proc)
      inodproc=parnod(iproc,1:nnod_proc)
      CALL get_ghosts(iproc,nelghost,nnodghost,ielproc,inodproc)
      DEALLOCATE(ielproc,inodproc)
    ENDDO

    nel1=nelghost(1)+nel
!    nel2=nelghost(2)+nel1
    nnod1=nnodghost(1)+nnod
!    nnod2=nnodghost(2)+nnod1
    DEALLOCATE(nelghost,nnodghost)
    DEALLOCATE(nodiel,nnodiel,nodproc)

    ! New local connectivity
    ALLOCATE(ielnod(nshape,nel_on_proc))
    ALLOCATE(nod_glob_loc(nnod_tot))
    nod_glob_loc=0_ink
    DO ii=1,nnod_on_proc(rankW)
      nod_glob_loc(parnod(rankW,ii))=ii
    ENDDO
    DO k=1,nshape
      DO ii=1,nel_on_proc(rankW)
        iel=pariel(rankW,ii)
        node=ielnodg(k,iel)
        jj=nod_glob_loc(node)
        IF (jj==0) CYCLE
        ielnod(k,ii)=jj
      ENDDO
    ENDDO
    DEALLOCATE(nod_glob_loc,ielnodg)

    ! ownership of elements and nodes for comms and local to global mapping

    ALLOCATE(e_owner_proc(2,nel_on_proc(rankW)))
    ALLOCATE(e_loc_glob(nel_on_proc(rankW)))
    DO iel=1,nel_on_proc(rankW)
      e_loc_glob(iel)=pariel(rankW,iel)
      iowner=ielpar(e_loc_glob(iel))
      e_owner_proc(1,iel)=iowner
      IF (iowner==rankW) THEN
        e_owner_proc(2,iel)=iel
      ELSE
        DO jj=1,nel_on_proc(iowner)
          IF (pariel(iowner,jj)==e_loc_glob(iel)) THEN
            e_owner_proc(2,iel)=jj
          ENDIF
        ENDDO
      ENDIF
    ENDDO

    ALLOCATE(n_owner_proc(2,nnod_on_proc(rankW)))
    ALLOCATE(n_loc_glob(nnod_on_proc(rankW)))
    DO inod=1,nnod_on_proc(rankW)
      n_loc_glob(inod)=parnod(rankW,inod)
      iowner=nodpart3(n_loc_glob(inod))
      n_owner_proc(1,inod)=iowner
      IF (iowner==rankW) THEN
        n_owner_proc(2,inod)=iel
      ELSE
        DO jj=1,nnod_on_proc(iowner)
          IF (parnod(iowner,jj)==n_loc_glob(inod)) THEN
            n_owner_proc(2,inod)=jj
          ENDIF
        ENDDO
      ENDIF
    ENDDO

    DEALLOCATE(ielpar,pariel,nodpart3,parnod,nel_on_proc,nnod_on_proc)

  END SUBROUTINE partition 

  SUBROUTINE get_ghosts(iproc,nelghost,nnodghost,ielpar,ielproc,inodproc)
    ! Find ghosts for this proc
    INTEGER(KIND=ink), INTENT(IN) :: iproc
    INTEGER(KIND=ink),DIMENSION(:),             INTENT(IN)  :: ielproc,inodproc,ielpar
    INTEGER(KIND=ink),DIMENSION(:),ALLOCATABLE, INTENT(OUT) :: nelghost,nnodghost

    INTEGER(KIND=ink) :: ii,iel,jj,k,kk,node,nel_proc,nnod_proc
    LOGICAL(KIND=lok),DIMENSION(:),ALLOCATABLE :: zelproc, znodproc
    LOGICAL(KIND=lok),DIMENSION(:),ALLOCATABLE :: zelghost,znodghost

    ALLOCATE(zelproc(nel_tot))
    ALLOCATE(znodproc(nnod_tot))
    nel_proc=nel_on_proc(iproc)
    nnod_proc=nnod_on_proc(iproc)
    ALLOCATE(zelghost(nel_proc))
    ALLOCATE(znodghost(nnod_proc))

    ielprocbdy=0_ink
    zelproc  =.FALSE.
    znodproc =.FALSE.
    zelghost =.FALSE.
    znodghost=.FALSE.

    ! find all elements at the processor boundary and put into ielprocbdy
    ielprocbdy=0_ink
    DO iel=1,nel_tot
      IF (ielpar(iel)==iproc) THEN
        zelghost(0_ink,iel)=.TRUE.   ! zeroth level element. needed later
        zelproc(iel)=.TRUE.
        DO k=1,nshape
          node=ielnod(k,iel)
          znodghost(0_ink,iel)=.TRUE.   ! zeroth level node. needed later
          znodproc(node)=.TRUE.
        ENDDO
      ENDIF
    ENDDO

    kk=0_ink

    el_loop: DO ii=1,nel_proc
      iel=ielproc(ii)
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
    nelprev=kk
    call get_ghost_layer(1,nelprev,nnodprev,ielprocbdy,zelghost,znodghost)
    nelghost(1,iproc)=nelprev
    nnodghost(iproc)=nnodprev
    call get_ghost_layer(2,nelprev,nnodprev,ielprocbdy,zelghost,znodghost)
    nelghost(2,iproc)=nelprev
    nnodghost(2,iproc)=nnodprev

    DEALLOCATE(ielproc,inodproc,zelghost,znodghost,ielprocbdy)

  END SUBROUTINE get_ghosts  

  SUBROUTINE get_ghost_layer(ilayer,nelprev,nnodprev,iboundlist,zelghost,znodghost)
    INTEGER(KIND=ink),                INTENT(IN)    :: ilayer
    INTEGER(KIND=ink),                INTENT(INOUT) :: nelprev
    INTEGER(KIND=ink),                INTENT(OUT)   :: nnodprev
    INTEGER(KIND=ink), DIMENSION(:),  INTENT(IN)    :: iboundlist
    LOGICAL(KIND=lok), DIMENSION(:,:),INTENT(INOUT) :: zelghosts
    LOGICAL(KIND=lok), DIMENSION(:,:),INTENT(INOUT) :: znodghosts

    ! create connectivity list based on boundary elements
    ALLOCATE(ibndelnod(nelprev*4_ink)
    ibndelnod=RESHAPE(ielnod(:,iboundlist(1:nelprev)),[nelprev*4_ink])
    DO ii=1,nelprev*4_ink
      IF (ibndelnod(ii)==0_ink) CYCLE
      DO jj=ii+1,nelprev*4_ink
        ! zero duplicates
        IF (ibndelnod(jj) == ibndelnod(ii)) ibndelnod(jj)=0_ink
      ENDDO
    ENDDO
    ! Now pack into scratch array
    ALLOCATE(ibndelnod1(COUNT(ibndelnod.NE.0_ink)))
    ibndelnod1=PACK(ibndelnod,ibndelnod.NE.0_ink)
    ! find ghost elements
    DO iel=1,SIZE(ibndelnod1)
      node=ibndelnod1(iel)
      midloop: DO ii=1,nnodiel(node)
        jj=nodiel(node,ii)
        DO kk=0,ilayer
          IF (zelghost(kk,node) CYCLE midloop
        ENDDO
        zelghost(ilayer,ii)=.TRUE.
      ENDDO midloop
    ENDDO
    DEALLOCATE(ibndelnod,ibndelnod1)

    kkk=0_ink
    nnn=0_ink

    DO iel=1,nel_proc
      IF (zelghost(ilayer,iel)) THEN
        ! elements
        kkk=kkk+1_ink
        ghostel(iproc,kkk)=iel
        ilghostel(iproc,kkk)=1_ink
        ielg=nelpar(iproc)+kkk
        pariel(iproc,ielg)=iel
        ! nodes        
        nodloop DO k=1,nshape
          node=ielnod(k,iel)
          DO jj=1,ilayer
            IF (znodghost(jj,node)) CYCLE nodloop
            nnn=nnn+1_ink
            ghostnod(iproc,nnn)=node
            ilghostnod(iproc,nnn)=1_ink
            inodg=nnodproc(iproc)+nnn
            parnod(iproc,inodg)=node
            znodghost(ilayer,node)=.TRUE.
          ENDDO
        ENDDO nodloop
      ENDIF
    ENDDO

    nelprev=kkk
    nnodprev=nnn
    DEALLOCATE(iboundlist)
    ALLOCATE(iboundlist(nelprev))
    iboundlist=0_ink
    DO iel=1,nelprev
      iboundlist(iel)=ghostel(iproc,iel)
    ENDDO

  END SUBROUTINE get_ghost_layer

  SUBROUTINE transfer_partition(nel1,nnod1)
    USE paradef_mod,  ONLY: e_loc_glob,n_loc_glob
    USE pointers_mod,ONLY: ndx,ndy,ielreg,indtype,ielmat,ndu,ndv

    INTEGER(KIND=ink), INTENT(IN) :: nel1,nnod1
    ! local
    INTEGER(KIND=ink) :: ii,iig
    REAL(KIND=rlk),    DIMENSION(:), POINTER :: pndx,pndy,pndu,pndv
    INTEGER(KIND=ink), DIMENSION(:), POINTER :: pielreg,pindtype,pielmat

    NULLIFY(pndx,pndy,pndu,pndv,pielreg,pindtype,pielmat)

    ! Set pointers to global data then reallocate global arrays to local
    pndx=>ndx
    pndy=>ndy
    pndu=>ndu
    pndv=>ndv
    pielreg=>ielreg
    pindtype=>indtype
    pielmat=>ielmat
    DEALLOCATE(ndx,ndy,ielreg,indtype,ielmat,ndu,ndv)

    ALLOCATE(ndx(nnod1),ndy(nnod1),ndu(nnod1),ndv(nnod1))
    ALLOCATE(ielmat(nel1),ielreg(nel1),indtype(nel1))

    DO ii=1,nnod1
      DO iig=1,nnod_tot
        IF (n_loc_glob(ii)==iig) THEN
          ndx(ii)=pndx(iig)
          ndy(ii)=pndy(iig)
          ndu(ii)=pndu(iig)
          ndv(ii)=pndv(iig)
        ENDIF
      ENDDO
    ENDDO

    DO ii=1,nel1
      DO iig=1,nel_tot
        IF (e_loc_glob(ii)==iig) THEN
          ielmat(ii) =pielmat(iig)
          ielreg(ii) =pielreg(iig)
          indtype(ii)=pindtype(iig)
        ENDIF
      ENDDO
    ENDDO

    DEALLOCATE(pndx,pndy,pielreg,pindtype,pielmat,pndu,pndv)

  END SUBROUTINE transfer_partition

END MODULE comms_mod
















