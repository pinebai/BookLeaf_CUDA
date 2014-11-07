
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

module TYPH_Decomposition_mod

  USE TYPH_Types_mod

  implicit none

  public

contains

  integer(kind=TERRK) function TYPH_Set_Partition_Info( ID, El_Shape, N_Layers, N_El_tot,    &
                                                        N_Nod_tot,El_To_Proc, Nod_To_Proc,   &
                                                        El_Loc_To_Glob, Nod_Loc_To_Glob,     &
                                                        Connectivity, Name, Group )

    implicit none

    integer(kind=TSIZEK),                 intent(out)          :: ID
    integer(kind=TSIZEK),                 intent(in)           :: El_Shape
    integer(kind=TSIZEK),                 intent(in)           :: N_Layers
    integer(kind=TSIZEK), dimension(0:),  intent(in)           :: N_El_tot
    integer(kind=TSIZEK), dimension(0:),  intent(in)           :: N_Nod_tot
    integer(kind=TSIZEK), dimension(:,:), intent(in)           :: El_To_Proc
    integer(kind=TSIZEK), dimension(:,:), intent(in)           :: Nod_To_Proc
    integer(kind=TSIZEK), dimension(:),   intent(in)           :: El_Loc_To_Glob
    integer(kind=TSIZEK), dimension(:),   intent(in)           :: Nod_Loc_To_Glob
    integer(kind=TSIZEK), dimension(:,:), intent(in), optional :: Connectivity
    character(len=*),                     intent(in), optional :: Name
    integer(kind=TSIZEK),                 intent(in), optional :: Group

    ID = 1
    TYPH_Set_Partition_Info = 0
    return

  end function TYPH_Set_Partition_Info

end module TYPH_Decomposition_mod

! EOF
