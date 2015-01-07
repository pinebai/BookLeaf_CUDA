
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

module TYPH_util_mod
  
  use TYPH_Types_mod

  implicit none

  public
  private :: mInitMP
  private :: mInitMPtypes
  private :: mNewMP
  private :: mMP
  private :: mMPtable
  
  ! The global internal MPI communication properties.
  ! These are given null, meaningless default values that get set during Typhon initialization
  ! - this is to try and stop users using them beforehand.
  
  
  type :: MPtypes_tp
    integer(kind=MPIK) :: real        = MPI_DATATYPE_NULL
    integer(kind=MPIK) :: integer     = MPI_DATATYPE_NULL
    integer(kind=MPIK) :: logical     = MPI_DATATYPE_NULL
    integer(kind=MPIK) :: character   = MPI_DATATYPE_NULL
    integer(kind=MPIK) :: mem         = MPI_DATATYPE_NULL
    integer(kind=MPIK) :: size        = MPI_DATATYPE_NULL
    integer(kind=MPIK) :: mpi         = MPI_DATATYPE_NULL
    integer(kind=MPIK) :: integerpad         ! Keeps derived type on 8-byte bound
  end type MPtypes_tp
  
  
  type :: MP_tp
    integer(kind=MPIK) :: comm        = MPI_COMM_NULL
    integer(kind=MPIK) :: info        = MPI_INFO_NULL
    integer(kind=MPIK) :: size        = 0
    integer(kind=MPIK) :: rank        = MPI_PROC_NULL
    integer(kind=MPIK) :: minrank     = MPI_PROC_NULL
    integer(kind=MPIK) :: maxrank     = MPI_PROC_NULL
    integer(kind=MPIK) :: masterrank  = MPI_PROC_NULL
    integer(kind=MPIK) :: error       = MPI_SUCCESS
    logical            :: ismaster    = .false.
    logical            :: initialised = .false.
    logical            :: finalised   = .false.
    logical            :: logicalpad                       ! Keeps derived type on 8-byte boundary
  end type MP_tp
  
  
  type (MP_tp),      target, save :: mMP
  type (MPtypes_tp), target, save :: mMPtypes

  type (MP_tp),      pointer :: ty_MP => null()            ! Used for declaring automatic arrays

  integer, parameter :: pMP_tp_size = 48     ! in bytes

  type (MP_tp), dimension(:), pointer, save :: mMPtable
  integer(kind=TSIZEK),                save :: mMPtablesize  = 0
  integer(kind=TSIZEK),                save :: mMPtabletally = 0

  abstract interface
    integer(kind=TERRK) function killfunc()
      use :: TYPH_Types_mod
    end function killfunc
  end interface

  integer(kind=TSIZEK), parameter :: pKillSize = 16
  integer(kind=TSIZEK) :: mNKills = 0

  type :: mKillStore_tp
    procedure(killfunc), pointer, nopass :: kfunc
  end type mKillStore_tp

  type (mKillStore_tp), dimension(pKillSize), save :: mKillStore

contains
  
  integer(kind=TERRK) function TYPH_Init( Comm )

    implicit none
    integer(kind=MPIK), intent(in), optional :: Comm

    type (MP_tp), pointer :: iMP       ! MPI gubbins type
    integer(kind=TERRK)   :: irc       ! Internal return code

    iMP => ty_GetMP()        ! Default MPI gubbins

    irc = ty_ErrorCheck((.not. iMP%initialised), "TYPH_Init", ERR_USER, TYPH_ERR_INVALID_OP,  &
         &  "Already initialised")

    ! Initialize the MPI communication.
    ! (- initializes MPI with communicator if needed, gets No. processes, task IDs, etc).

    if (present(Comm)) then
      irc = ty_CommsInit(iMP, Comm)
    else
      irc = ty_CommsInit(iMP)
    end if

    iMP%initialised = .true.

    TYPH_Init = irc
    
  end function TYPH_Init
  
  integer(kind=TERRK) function TYPH_Kill( FinalizeMPI )

    implicit none
    logical,            intent(in), optional :: FinalizeMPI

    logical               :: iFinalizeMPI        ! Local value of the optional flag.
    type (MP_tp), pointer :: iMP

    integer(kind=TSIZEK) :: ik     ! Loop over kill table
    integer(kind=TERRK)  :: irc    ! Internal return code

    iMP => ty_GetMP()
    irc = ty_ErrorCheck(iMP%initialised, "TYPH_Kill", ERR_USER, TYPH_ERR_UNINITIALISED,  &
         &  "Not initialised")

    ! Perform a barrier, so that all processes are okay and ready to exit Typhon cleanly.
    irc = ty_CommsBarrier(iMP)
    
    ! Call all registered kill routines for components - do in reverse order, for cleanliness
    do ik = mNKills, 1, -1
      irc = mKillStore(ik)%kfunc()
    end do
!   zero mNKills as if we reinit it will increment and be too big
    mNKills = 0

    !
    ! Kill the MPI communication as needed, finalizing MPI if specified
    ! 
    if (present(FinalizeMPI)) then
      iFinalizeMPI = FinalizeMPI
    else
      iFinalizeMPI = .true.
    end if

    if (iFinalizeMPI) then
      irc = ty_CommsKill( iMP, iFinalizeMPI )
    end if

    iMP%initialised = .false.

    TYPH_Kill = irc

  end function TYPH_Kill

  integer(kind=TERRK) function TYPH_Abort(Code)

    implicit none
    integer(kind=TYPHK), intent(in) :: Code
    integer(kind=TERRK) :: irc

    irc = ty_CommsAbort( ty_GetMP(), Code )   ! Actually does the abort!

    TYPH_Abort = irc
    stop  ! REALLY, REALLY shouldn't get here

  end function TYPH_Abort

  integer(kind=TERRK) function TYPH_Get_Size( Size )
    
    implicit none
    integer(kind=TSIZEK),          intent(out) :: Size
    
    type (MP_tp), pointer :: iMP
    
    iMP => ty_GetMP()
    Size = int(iMP%size, TSIZEK)
    TYPH_Get_Size = TYPH_SUCCESS
    
  end function TYPH_Get_Size
  
  integer(kind=TERRK) function TYPH_Get_Rank( Rank )
    
    implicit none
    
    integer(kind=TSIZEK),          intent(out) :: Rank
    type (MP_tp), pointer :: iMP
    
    iMP => ty_GetMP()
    Rank = int(iMP%rank, TSIZEK)
    TYPH_Get_Rank = TYPH_SUCCESS
    
  end function TYPH_Get_Rank
  
  logical function TYPH_IsMaster()
    
    implicit none
    
    type (MP_tp), pointer :: iMP
    iMP => ty_GetMP()
    TYPH_IsMaster = iMP%ismaster
    
  end function TYPH_IsMaster
  
  integer(kind=TERRK) function TYPH_Barrier()

    implicit none
    type (MP_tp), pointer :: iMP
    integer(kind=TERRK) :: irc
    
    iMP => ty_GetMP()
    irc = ty_CommsBarrier(iMP)
    TYPH_Barrier = irc

  end function TYPH_Barrier
 
  function ty_GetMP()

    implicit none
    type (MP_tp), pointer :: ty_GetMP

    ty_GetMP => mMP

  end function ty_GetMP
  
  integer(kind=TERRK) function ty_CommsAbort( aMP, aCode )
    
    implicit none
    type (MP_tp), pointer          :: aMP
    integer(kind=MPIK), intent(in) :: aCode
    
    integer(kind=MPIK)   :: iMPIerr        ! MPI error code
    
    aMP%finalised = .true.
    !
    !  This is a terminal abort
    !  So if supporting multiple communitors, make sure they all get free-ed and then abort
    !
    if (aMP%initialised) then
      call MPI_ABORT(aMP%comm, aCode, iMPIerr)
    end if

    ty_CommsAbort = 0

  end function ty_CommsAbort
  
  integer(kind=TERRK) function ty_CommsInit( aMP, aComm )
    
    implicit none
    
    type (MP_tp), pointer                    :: aMP        ! intent(in)
    integer(kind=MPIK), intent(in), optional :: aComm      ! Communicator to base Typhon's on
    
    logical             :: iFlag          ! Flag for whether
    integer(kind=MPIK)  :: iComm          ! MPI communicator
    integer(kind=MPIK)  :: iMPIerr        ! MPI error code
    integer(kind=TERRK) :: irc            ! Internal return code
    
    ! Initialize MPI communication, gets No. processes, task IDs, etc.
    
    call MPI_INITIALIZED(iFlag, iMPIerr)
    irc = ty_ErrorCheck((iMPIerr == MPI_SUCCESS), "ty_CommsInit", ERR_MPI, TYPH_ERR_MPI,  &
         &  "MPI_INITIALIZED failure")
    
    if (.not. iFlag) then
      call MPI_INIT(iMPIerr)
      irc = ty_ErrorCheck((iMPIerr == MPI_SUCCESS), "ty_CommsInit", ERR_MPI, TYPH_ERR_MPI,  &
           &  "MPI_INIT failure")
    end if
    
    if (present(aComm)) then
      iComm = aComm
    else
      iComm = MPI_COMM_WORLD   ! default greatness
    end if

    irc = mInitMP( aMP, iComm )
    irc = mInitMPtypes()
    
    ty_MP => aMP
    
    ty_CommsInit = 0
    
  end function ty_CommsInit
  
  integer(kind=TERRK) function ty_KillInsert( aKillFunc )

    implicit none

    procedure(killfunc) :: aKillFunc

    mNKills = mNKills + 1

    if (mNKills > pKillSize) then
      ty_KillInsert = -1
    end if

    mKillStore(mNKills)%kfunc => aKillFunc
    ty_KillInsert = 0

  end function ty_KillInsert

  integer(kind=TERRK) function ty_CommsKill( aMP, aFinalize )
    
    implicit none
    
    type (MP_tp), pointer                    :: aMP           ! intent(in)
    logical,            intent(in)           :: aFinalize     ! If .true. finalize MPI
    
    integer(kind=MPIK)  :: iMPIerr       ! MPI error code
    integer(kind=TERRK) :: irc           ! Internal return code
    
    
    if (.not. aMP%initialised) then
      ty_CommsKill = TYPH_ERR_UNINITIALISED
      return
    end if
    
    if (aFinalize) then
      aMP%finalised = .true.
      call MPI_FINALIZE(iMPIerr)
      irc = ty_ErrorCheck((iMPIerr == MPI_SUCCESS), "ty_CommsKill", ERR_MPI, TYPH_ERR_MPI,  &
           &  "MPI_FINALIZE failure")
    end if
    
    ty_CommsKill = 0
    
  end function ty_CommsKill
  
  integer(kind=TERRK) function ty_CommsBarrier( aMP )
    
    implicit none
    
    type (MP_tp), pointer :: aMP         ! intent(in)
    
    integer(kind=MPIK)  :: iMPIerr       ! MPI error code
    integer(kind=TERRK) :: irc           ! Internal return code
    
    
    call MPI_BARRIER(aMP%comm, iMPIerr)
    irc = ty_ErrorCheck((iMPIerr == MPI_SUCCESS), "ty_CommsBarrier", ERR_MPI, TYPH_ERR_MPI,  &
         &  "MPI_BARRIER failure")
    
    ty_CommsBarrier = 0
    
  end function ty_CommsBarrier
  
  integer(kind=TERRK) function mInitMP( aMP, aComm )
    
    implicit none
    
    type (MP_tp), pointer          :: aMP        ! intent(in)
    integer(kind=MPIK), intent(in) :: aComm      ! Communicator to base Typhon's on
    
    integer(kind=TERRK) :: irc            ! Internal return code
    integer(kind=MPIK)  :: iMPIerr
    
    
    if (aComm/=MPI_COMM_NULL) then
      call MPI_COMM_DUP(aComm,     aMP%comm, iMPIerr)
      irc = ty_ErrorCheck((iMPIerr == MPI_SUCCESS), "mInitMP", ERR_MPI, TYPH_ERR_MPI,  &
           &  "MPI_COMM_DUP failure")

      call MPI_COMM_SIZE(aMP%comm, aMP%size, iMPIerr)
      irc = ty_ErrorCheck((iMPIerr == MPI_SUCCESS), "mInitMP", ERR_MPI, TYPH_ERR_MPI,  &
           &  "MPI_COMM_SIZE failure")

      call MPI_COMM_RANK(aMP%comm, aMP%rank, iMPIerr)
      irc = ty_ErrorCheck((iMPIerr == MPI_SUCCESS), "mInitMP", ERR_MPI, TYPH_ERR_MPI,  &
           &  "MPI_COMM_RANK failure")
    
      aMP%minrank    = 0
      aMP%maxrank    = aMP%size - 1
      aMP%masterrank = aMP%minrank
      aMP%ismaster   = (aMP%rank == aMP%masterrank)
    else
      aMP%comm       = MPI_COMM_NULL
      aMP%size       = 0
      aMP%rank       = MPI_PROC_NULL
      aMP%minrank    = 0
      aMP%maxrank    = 0
      aMP%masterrank = 0
      aMP%ismaster   = .false.
   endif

    aMP%initialised = .true.
    aMP%finalised   = .false.
    
    mInitMP = 0
    
  end function mInitMP
  
  integer(kind=TERRK) function mInitMPtypes( )
    
    implicit none
    
    integer(kind=MPIK) :: iSize
    integer(kind=MPIK) :: iMPIerr
    
    
    ! Construct MPI datatypes to match Fortran90 parameterized types.
    
    ! MPI_SIZEOF is specific to F90 MPI and simply returns size in bytes of intrinsics F90 types.
    ! Can't be bothered to check iMPIerr for sizes here!
    
    call MPI_SIZEOF(0.0_REALK, iSize, iMPIerr)
    call MPI_TYPE_MATCH_SIZE(MPI_TYPECLASS_REAL, iSize, mMPtypes%real, iMPIerr)
    
    call MPI_SIZEOF(0_INTK, iSize, iMPIerr)
    call MPI_TYPE_MATCH_SIZE(MPI_TYPECLASS_INTEGER, iSize, mMPtypes%integer, iMPIerr)
    
    mMPtypes%logical   = MPI_LOGICAL
    mMPtypes%character = MPI_CHARACTER
    
    call MPI_SIZEOF(0_TMEMK, iSize, iMPIerr)
    call MPI_TYPE_MATCH_SIZE(MPI_TYPECLASS_INTEGER, iSize, mMPtypes%mem, iMPIerr)
    
    call MPI_SIZEOF(0_TSIZEK, iSize, iMPIerr)
    call MPI_TYPE_MATCH_SIZE(MPI_TYPECLASS_INTEGER, iSize, mMPtypes%size, iMPIerr)
    
    call MPI_SIZEOF(0_MPIK, iSize, iMPIerr)
    call MPI_TYPE_MATCH_SIZE(MPI_TYPECLASS_INTEGER, iSize, mMPtypes%mpi, iMPIerr)
    
    mInitMPtypes = 0
    
  end function mInitMPtypes
  
  integer(kind=TERRK) function mNewMP( aComm )

    implicit none
    integer(kind=MPIK),   intent(in)  :: aComm         ! MPI communicator
    
    integer(kind=TSIZEK), parameter :: pDefaultSize  = 5
    integer(kind=TSIZEK), parameter :: pDefaultDelta = 5
    integer(kind=TSIZEK)            :: iTempSize     = 5
    
    type(MP_tp), dimension(:), pointer :: iTempMPtable
    type(MP_tp), pointer :: iMP
    integer              :: iAllocStat                            ! Allocation status
    integer(kind=TERRK)  :: irc                                   ! Internal return code
    integer(kind=TSIZEK) :: ii
    
    if (mMPtablesize == 0) then
      ! alloc if needs be
      allocate(mMPtable(pDefaultSize), stat=iAllocStat )
      mMPtablesize = pDefaultSize
    else if (mMPtablesize == mMPtabletally) then
      ! realloc if needs be
      iTempSize = mMPtablesize + pDefaultDelta
      allocate(iTempMPtable(iTempSize), stat=iAllocStat)
      iTempMPtable(1:mMPtablesize) = mMPtable(1:mMPtablesize)

      deallocate(mMPtable, stat=iAllocStat)
      nullify(mMPtable)
      mMPtable => iTempMPtable
      nullify(iTempMPtable)

      mMPtablesize = iTempsize
    end if

    ! find space in table
    do ii = 1, mMPtablesize
      if (.not. mMPtable(ii)%initialised) then
        exit
      end if
    end do

    iMP => mMPtable(1)
    mMPtabletally = mMPtabletally + 1

    irc = mInitMP( iMP, aComm )

    mNewMP = 0
   
  end function mNewMP
  
  integer(kind=TERRK) function ty_ErrorCheck( aCondition, aRoutine, aClass, aCode, aString )

    implicit none

    logical,             intent(in) :: aCondition   ! if .false. then there's an error
    character(len=*),    intent(in) :: aRoutine     ! Calling routine name
    integer(kind=TYPHK), intent(in) :: aClass       ! Class of error, using ERR_*
    integer(kind=TYPHK), intent(in) :: aCode        ! Abort code to use
    character(len=*),    intent(in) :: aString      ! Error description string

    ty_ErrorCheck = 0

    if (aCondition) then
      return
    end if
    ! If here, then the check failed and there's an error
    ty_ErrorCheck = aCode
    call halt(aString)

  end function ty_ErrorCheck

  integer(kind=TERRK) function ty_MemCheck(aStat, aAorD, aVar)
    
    implicit none
    
    integer,             intent(in) :: aStat      ! Default integer = allocate/deallocate istat
    integer(kind=TERRK), intent(in) :: aAorD      ! Flag for allocation or deallocation
    character(len=*),    intent(in) :: aVar       ! Variable name
    
    character(len=64)   :: ierrstr
    
    ty_MemCheck = 0

    if (aStat /= 0) then
      select case (aAorD)
      case (TY_MEM_ALLOC)
        write(ierrstr,*) "Memory allocation error:", aStat, aVar
        call halt(ierrstr)
      case (TY_MEM_DEALLOC)
        write(ierrstr,*) "Memory deallocation error:", aStat, aVar
        call halt(ierrstr)
      case default
        ! Um, shouldn't get here
      end select
    end if

  end function ty_MemCheck

  real(kind=REALK) function get_time()

    use mpi

    get_time = MPI_WTIME()

  end function get_time

  integer(kind=TERRK) function set_info(pinfo)

    use mpi
    implicit none

    integer(kind=TSIZEK), intent(inout) :: pinfo

    pinfo    = MPI_INFO_NULL
    set_info = 0

  end function set_info

  integer(kind=TERRK) function set_comm(comm)

    use mpi
    use kinds_mod,only: ink
    implicit none
    integer(kind=ink), intent(inout) :: comm

    comm     = MPI_COMM_WORLD
    set_comm = 0

  end function set_comm

  integer(kind=TERRK) function set_comm_self(comm)

    use mpi
    use kinds_mod,only: ink
    implicit none
    integer(kind=ink), intent(inout) :: comm

    comm          = MPI_COMM_SELF
    set_comm_self = 0

  end function set_comm_self

end module TYPH_util_mod

! EOF
