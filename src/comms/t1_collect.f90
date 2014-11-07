
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

module TYPH_Collect_mod
  
  USE TYPH_Types_mod

  implicit none
  
  private
  
  public :: TYPH_Gather, TYPH_Reduce
  
  integer(kind=TYPHK), public, parameter :: TYPH_OP_SUM  = 1001    ! No significance to these values
  integer(kind=TYPHK), public, parameter :: TYPH_OP_PROD = 1002    ! - just chose them to be a bit
  integer(kind=TYPHK), public, parameter :: TYPH_OP_MAX  = 1003    !   different to avoid possible
  integer(kind=TYPHK), public, parameter :: TYPH_OP_MIN  = 1004    !   conflicts elsewhere
  integer(kind=TYPHK), public, parameter :: TYPH_OP_OR   = 1011
  integer(kind=TYPHK), public, parameter :: TYPH_OP_XOR  = 1012
  integer(kind=TYPHK), public, parameter :: TYPH_OP_AND  = 1013

  interface TYPH_Reduce
    module procedure mReduce1D_Real
    module procedure mReduce1D_Int
  end interface
  
  interface TYPH_Gather
    module procedure mAllGather0D_Real
    module procedure mAllGather1D_Int
  end interface

contains
  
  integer(kind=TERRK) function mAllGather0D_Real(Val, GVal, Comm) result(fres)

    implicit none

    real(kind=REALK),    intent(in)                 :: Val
    real(kind=REALK),    dimension(0:),intent(inout) :: GVal     ! intent(out)
    integer(kind=TSIZEK), intent(in)                :: Comm

    integer(kind=TERRK)        :: irc

    GVal(0) = Val

    fres = irc

  end function mAllGather0D_Real

  integer(kind=TERRK) function mAllGather1D_Int(Val, GVal, Comm)  result(fres)
    
    implicit none

    integer(kind=INTK),  dimension(:),   intent(in)    :: Val
    integer(kind=INTK),  dimension(:,0:),intent(inout) :: GVal          ! intent(out)
    integer(kind=TSIZEK),                intent(in)    :: Comm
    integer(kind=TERRK) :: irc        ! Internal return code
    
    GVal(:,0) = Val(:)

    fres = irc
   
  end function mAllGather1D_Int
  
  integer(kind=TERRK) function mReduce1D_Real(Val, RVal, Op, Comm) result(fres)
    
    implicit none

    real(kind=REALK),    dimension(:), intent(in)  :: Val
    real(kind=REALK),    dimension(:), intent(out) :: RVal
    integer(kind=TYPHK),               intent(in)  :: Op
    integer(kind=TSIZEK),              intent(in)  :: Comm
    
    integer(kind=MPIK)    :: iMPIop       ! MPI reduction operation
    integer(kind=TERRK)   :: irc          ! Internal return code

    RVal = Val
    fres = irc
    
  end function mReduce1D_Real


  integer(kind=TERRK) function mReduce1D_Int(Val, RVal, Op, Comm)  result(fres)
    
    implicit none

    integer(kind=INTK),  dimension(:), intent(in)  :: Val
    integer(kind=INTK),  dimension(:), intent(out) :: RVal
    integer(kind=TYPHK),               intent(in)  :: Op
    integer(kind=TSIZEK),              intent(in)  :: Comm

    integer(kind=MPIK)         :: iMPIop                          ! MPI reduction operation
    integer(kind=TERRK)        :: irc                             ! Internal return code

    RVal = Val
    fres = irc
    
  end function mReduce1D_Int

end module TYPH_Collect_mod


! EOF


