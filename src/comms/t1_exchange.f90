
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

module TYPH_Exchange_mod

  USE TYPH_Types_mod

  implicit none

contains

  integer(kind=TERRK) function TYPH_Start_Exch(PhaseID, NumGhosts)

    integer(kind=TSIZEK), intent(in)            :: PhaseID
    integer(kind=TSIZEK), intent(in), optional  :: NumGhosts

    TYPH_Start_Exch = 0

  end function TYPH_Start_Exch


  integer(kind=TERRK) function TYPH_Finish_Exch(PhaseID)

    integer(kind=TSIZEK), intent(in) :: PhaseID

    TYPH_Finish_Exch = 0

  end function TYPH_Finish_Exch

end module TYPH_Exchange_mod

! EOF
