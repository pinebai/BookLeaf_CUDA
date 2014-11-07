
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
  use kinds_mod,only: rlk

  implicit none

  public  :: TYPH_Init,TYPH_kill,TYPH_abort,TYPH_get_size,TYPH_get_rank,&
&            TYPH_get_comm,get_time,set_comm

contains

  integer(kind=TERRK) function TYPH_Init( Comm )

    integer(kind=MPIK),intent(in),optional :: Comm

    TYPH_Init=0

  end function TYPH_Init

  integer(kind=TERRK) function TYPH_Kill( FinalizeMPI )

    logical,intent(in),optional :: FinalizeMPI

    TYPH_Kill=0

  end function Typh_Kill

  integer(kind=TERRK) function TYPH_Abort(Code)

    integer(kind=TYPHK),intent(in) :: Code

    TYPH_Abort=0

  end function TYPH_Abort

  integer(kind=TERRK) function TYPH_get_size( Size )

    integer(kind=TSIZEK),intent(out) :: Size

    Size=1_TSIZEK
    TYPH_get_size=0

  end function TYPH_get_size

  integer(kind=TERRK) function TYPH_get_rank( Rank )

    integer(kind=TSIZEK),intent(out) :: Rank

    Rank=0_TSIZEK
    TYPH_get_rank=0

  end function TYPH_get_rank

  integer(kind=TERRK) function TYPH_get_comm( )

    TYPH_get_comm=0

  end function TYPH_get_comm

  real(kind=RLK) function get_time()



    call CPU_TIME(get_time)

  end function get_time

  integer(kind=TERRK) function ty_ErrorCheck( aCondition, aRoutine, aClass, aCode, aString )

    implicit none

    logical,             intent(in) :: aCondition   ! if .false. then there's an error
    character(len=*),    intent(in) :: aRoutine     ! Calling routine name
    integer(kind=TYPHK), intent(in) :: aClass       ! Class of error, using ERR_*
    integer(kind=TYPHK), intent(in) :: aCode        ! Abort code to use
    character(len=*),    intent(in) :: aString      ! Error description string

    integer(kind=TERRK) :: irc

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
    integer(kind=TERRK) :: irc        ! Internal return code
    
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

  integer(kind=TERRK) function set_info(pinfo)

    use kinds_mod,only: ink
    implicit none
    integer(kind=ink), intent(inout) :: pinfo

    pinfo    = -1
    set_info = 0

  end function set_info

  integer(kind=TERRK) function set_comm(comm)

    use kinds_mod,only: ink
    implicit none
    integer(kind=ink), intent(inout) :: comm

    comm     = -1
    set_comm = 0

  end function set_comm

  integer(kind=TERRK) function set_comm_self(comm)

    use kinds_mod,only: ink
    implicit none
    integer(kind=ink), intent(inout) :: comm

    comm          = -1
    set_comm_self = 0

  end function set_comm_self

end module TYPH_util_mod
