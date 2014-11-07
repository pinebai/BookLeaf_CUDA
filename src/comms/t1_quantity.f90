
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

module TYPH_Quant_mod

  use TYPH_Register_mod
  use TYPH_Types_mod
  use TYPH_util_mod, only: ty_ErrorCheck,ty_MemCheck
  implicit none

  interface TYPH_Set_Quant_Address
    module procedure                          &
      mUpdateQuant_R8_1D, mUpdateQuant_R8_2D, &
      mUpdateQuant_I4_1D, mUpdateQuant_I4_2D
  end interface

contains

  integer(kind=TERRK) function TYPH_Add_Quant_To_Phase(PhaseID, QuantID,    &
                      ReceiveQuantID, KeySetID, GhostsMin, GhostsMax)


    implicit none


    integer(kind=TSIZEK), intent(in)           :: PhaseID
    integer(kind=TSIZEK), intent(in)           :: QuantID
    integer(kind=TSIZEK), intent(in), optional :: ReceiveQuantID
    integer(kind=TSIZEK), intent(in), optional :: KeySetID
    integer(kind=TSIZEK), intent(in), optional :: GhostsMin
    integer(kind=TSIZEK), intent(in), optional :: GhostsMax

    TYPH_Add_Quant_To_Phase = 0

  end function TYPH_Add_Quant_To_Phase


  integer(kind=TERRK) function mUpdateQuant_R8_1D(QuantID, PQuant)

    implicit none

    integer(kind=TSIZEK),                     intent(in) :: QuantID
    real(kind=REALK),dimension(:),allocatable,intent(in) :: PQuant

    mUpdateQuant_R8_1D = 0

  end function mUpdateQuant_R8_1D


  integer(kind=TERRK) function mUpdateQuant_R8_2D(QuantID, PQuant)

    implicit none
    integer(kind=TSIZEK),                       intent(in) :: QuantID
    real(kind=REALK),dimension(:,:),allocatable,intent(in) :: PQuant

    mUpdateQuant_R8_2D = 0

  end function mUpdateQuant_R8_2D

  integer(kind=TERRK) function mUpdateQuant_I4_1D(QuantID, PQuant)

    implicit none

    integer(kind=TSIZEK),                        intent(in) :: QuantID
    integer(kind=INTK), dimension(:),allocatable,intent(in) :: PQuant

    mUpdateQuant_I4_1D = 0

  end function mUpdateQuant_I4_1D

  integer(kind=TERRK) function mUpdateQuant_I4_2D(QuantID, PQuant)

    implicit none

    integer(kind=TSIZEK),                         intent(in) :: QuantID
    integer(kind=INTK),dimension(:,:),allocatable,intent(in) :: PQuant

    mUpdateQuant_I4_2D = 0

  end function mUpdateQuant_I4_2D

end module TYPH_Quant_mod


! EOF


