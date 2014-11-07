
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

module TYPH_Types_mod
  
  use MPI
  
  implicit none
  
  public
  
  integer, parameter :: TYPHK            = 4
  integer, parameter :: TERRK            = 4
  integer, parameter :: TSIZEK           = 4
  integer, parameter :: TDOUBLEK         = 8         ! From timing routines
  integer, parameter :: TMEMK            = MPI_ADDRESS_KIND
  
  integer, parameter :: MPIK = 4         ! Kind for integers used in normal MPI arguments
  
  
  integer, parameter :: REALK            = 8 ! Parameterised kind of application's reals
  integer, parameter :: INTK             = 4 ! Parameterised kind of application's integers
  integer, parameter :: LOGK             = 4 ! Parameterised kind of application's logicals
  
  
  integer, parameter :: TYPH_STRLEN      = 64      ! Length of strings in external API
  integer, parameter :: SUBNAME_LEN      = 30      ! Length of routine name strings
  
  integer(kind=TYPHK), parameter :: TYPH_NULL  = -1      ! Widely-used NULL value - must be -ve
  integer(kind=TYPHK), parameter :: TYPH_ALL   = -999    ! Widely-used special value - must be -ve
  
  
  integer(kind=TYPHK), parameter :: TYPH_SIZEK       = 0       ! Not made app-public
  integer(kind=TYPHK), parameter :: TYPH_REAL        = 1
  integer(kind=TYPHK), parameter :: TYPH_INTEGER     = 2
  integer(kind=TYPHK), parameter :: TYPH_LOGICAL     = 3
  integer(kind=TYPHK), parameter :: TYPH_CHARACTER   = 4       ! Not made app-public
  integer(kind=TYPHK), parameter :: TYPH_STRING      = 5       ! Not made app-public
  integer(kind=TYPHK), parameter :: TYPH_TYPHK       = 6       ! Not made app-public
  integer(kind=TYPHK), parameter :: TYPH_MEMK        = 7       ! Not made app-public
  integer(kind=TYPHK), parameter :: TYPH_MPIK        = 8       ! Not made app-public

  integer(kind=TYPHK), parameter :: TYPH_AUXID_NONE   = -9099


  integer(kind=TYPHK), parameter :: TYPH_V3_SEND = 1001
  integer(kind=TYPHK), parameter :: TYPH_V3_RECV = 1002

  integer(kind=TYPHK), parameter :: TYPH_GHOSTS_ZERO  = 0
  integer(kind=TYPHK), parameter :: TYPH_GHOSTS_ONE   = 1
  integer(kind=TYPHK), parameter :: TYPH_GHOSTS_TWO   = 2
  integer(kind=TYPHK), parameter :: TYPH_GHOSTS_THREE = 3

  integer(kind=TYPHK), parameter :: TYPH_SHAPE_QUAD4  = 4

  integer(kind=TYPHK), parameter :: TYPH_MESH_DIM     = -88

  integer(kind=TMEMK), parameter :: TYPH_NULL_ADDR = -999

  integer(kind=TYPHK), parameter :: TY_MEM_ALLOC   = 1111
  integer(kind=TYPHK), parameter :: TY_MEM_DEALLOC = 1112
  !
  ! Error classes:
  !
  integer(kind=TERRK), public, parameter :: ERR_USER = 101    ! User error
  integer(kind=TERRK), public, parameter :: ERR_MEM  = 102    ! Memory error
  integer(kind=TERRK), public, parameter :: ERR_MPI  = 103    ! MPI error
  integer(kind=TERRK), public, parameter :: ERR_INT  = 104    ! Internal error
  integer(kind=TERRK), public, parameter :: ERR_APP  = 105    ! For when application calls abort

  !
  ! Error codes:
  !
  integer(kind=TERRK), public, parameter :: TYPH_SUCCESS            =    0
  integer(kind=TERRK), public, parameter :: TYPH_FAIL               =   -1
  integer(kind=TERRK), public, parameter :: TYPH_ERR_USER           = -101
  integer(kind=TERRK), public, parameter :: TYPH_ERR_MEM            = -102
  integer(kind=TERRK), public, parameter :: TYPH_ERR_MPI            = -103
  integer(kind=TERRK), public, parameter :: TYPH_ERR_INT            = -104
  integer(kind=TERRK), public, parameter :: TYPH_ERR_APP            = -105
  integer(kind=TERRK), public, parameter :: TYPH_ERR_UNINITIALISED  = -110
  integer(kind=TERRK), public, parameter :: TYPH_ERR_INVALID_ARG    = -111
  integer(kind=TERRK), public, parameter :: TYPH_ERR_MISSING_ARG    = -112
  integer(kind=TERRK), public, parameter :: TYPH_ERR_INVALID_OP     = -113
  integer(kind=TERRK), public, parameter :: TYPH_ERR_UNKNOWN_MODE   = -114

! Ideally these should be defined in V3 directory
! Intel compiler cannot cope with class(*), pointer.
  type :: KeyLL_tp
    integer(kind=TSIZEK)                        :: layer   ! ghost layer that key belongs to
    integer(kind=MPIK)                          :: proc    ! proc to send or receive from
    integer(kind=TSIZEK), dimension(:), pointer :: list   => null()  ! list of elements or nodes
    integer(kind=TSIZEK)                        :: nlist             ! size of element/node list
!   Optional block lengths to variable amounts of data to be communicated per cell/node
    integer(kind=TSIZEK), dimension(:), pointer :: blockLens
    type (KeyLL_tp), pointer                    :: next   => null()  ! next key in linked list
    type (KeyLL_tp), pointer                    :: prev   => null()  ! previous key in linked list
    type (KeyLL_tp), pointer                    :: parent => null()  ! key this key is derived from
  end type KeyLL_tp


  type Key_Set_tp
    integer(kind=TYPHK)        :: centring    ! Type of variable centring (NODE/CELL)
    integer(kind=TSIZEK)       :: AuxID       ! Type of Auxiliary data
    integer(kind=TSIZEK)       :: stride      ! no. parts object is divided into (for 2D etc arrays)
    integer(kind=TSIZEK)       :: lmin        ! smallest ghost layer index
    integer(kind=TSIZEK)       :: lmax        ! largest ghost layer index
    integer(kind=TSIZEK)       :: partitionID ! ID of Partition to use with Key
    integer(kind=TSIZEK)       :: nSend       ! No. Send keys in set
    integer(kind=TSIZEK)       :: nRecv       ! No. Recv keys in set
!   character(len=TYPH_STRLEN) :: name        ! key set name
    type (KeyLL_tp), pointer   :: send_keys => null()   ! linked list of send keys
    type (KeyLL_tp), pointer   :: recv_keys => null()   ! linked list of recv keys
  end type Key_Set_tp

  type :: V3_comm_Quant_tp
    integer(kind=TSIZEK)         :: quantID
    integer(kind=TSIZEK)         :: receiveQuantID
    integer(kind=TSIZEK)         :: KeySetID
    integer(kind=TSIZEK)         :: ghostsMin  ! Default Ghost layer range
    integer(kind=TSIZEK)         :: ghostsMax  ! Default Ghost layer range
    integer(kind=TSIZEK)         :: quantSize
    integer(kind=TSIZEK)         :: nrepeat
    integer(kind=TSIZEK)         :: stride
    integer(kind=MPIK),  pointer :: oldMpiType => null()
    integer(kind=MPIK),  pointer :: newMpiType => null()
    type(Key_Set_tp),    pointer :: keySet     => null() ! KeySet for this Quant
    type(V3_comm_Quant_tp), pointer :: next    => null()
  end type V3_comm_Quant_tp

  type V3_SchedulePart_tp
    integer(kind=MPIK)           :: new_mpi_tp
    integer(kind=TSIZEK)         :: quantSize
    integer(kind=TSIZEK)         :: nrepeat
    integer(kind=TSIZEK)         :: stride
    type(KeyLL_tp), pointer      :: key        => null()  ! offset array and length
    integer(kind=MPIK), pointer  :: old_mpi_tp => null()
    integer(kind=TMEMK), pointer :: address    => null()  ! Start address of item
  end type V3_SchedulePart_tp

  type V3_Schedule_tp
     integer(kind=TSIZEK)                            :: nsend      ! no. procs to send to
     integer(kind=MPIK),   dimension(:), allocatable :: send_proc   ! procs to send to
     integer(kind=MPIK),   dimension(:), allocatable :: mpi_send_tp ! MPI types of send data
!    MPI send requests
     integer(kind=TSIZEK), dimension(:), pointer     :: send_requests => null()
     integer(kind=TSIZEK), dimension(:), allocatable :: send_nparts
!    Number of parts sent to each proc
     integer(kind=TSIZEK), dimension(:), allocatable :: send_start
!    Start index in parts array for send to proc i

     integer(kind=TSIZEK)                            :: nrecv      ! no. procs to recv from
     integer(kind=MPIK),   dimension(:), allocatable :: recv_proc   ! procs to recv from
     integer(kind=MPIK),   dimension(:), allocatable :: mpi_recv_tp ! MPI types of recv data
!    MPI recv requests
     integer(kind=TSIZEK), dimension(:), pointer     :: recv_requests => null()
     integer(kind=TSIZEK), dimension(:), allocatable :: recv_nparts
!    Number of parts received from each proc
     integer(kind=TSIZEK), dimension(:), allocatable :: recv_start
!    Start index in parts array for recv from proc i
     type(V3_SchedulePart_tp),dimension(:), pointer  :: parts  => null()
!    Array of all parts to/from all procs for this proc

  end type V3_Schedule_tp

end module TYPH_Types_mod
  
! EOF

