
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

module TYPH_Key_mod

  use TYPH_Types_mod

  implicit none

  integer(kind=TYPHK), parameter :: TYPH_KTYPE_CELL        = 1

contains

  integer(kind=TERRK) function TYPH_Create_Key_Set(ID, KType, Lmin, Lmax, PartitionID)

    implicit none

    integer(kind=TSIZEK), intent(out) :: ID           !    KeySet index
    integer(kind=TYPHK),  intent(in)  :: KType        !    Key Type (Node/Cell/Cell Corner)
    integer(kind=TSIZEK), intent(in)  :: Lmin         !    smallest ghost layer index
    integer(kind=TSIZEK), intent(in)  :: Lmax         !    largest ghost layer index
    integer(kind=TSIZEK), intent(in)  :: PartitionID  !    ID of Partition to use with Key

    ID = 1
    TYPH_Create_Key_Set = 0

  end function TYPH_Create_Key_Set

end module TYPH_Key_mod

! EOF
