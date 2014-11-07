
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

  implicit none
  
  public
  
  integer, parameter :: TYPHK  = 4
  integer, parameter :: TERRK  = 4
  integer, parameter :: TSIZEK = 4
  integer, parameter :: MPIK   = 4 
  integer, parameter :: REALK  = 8
  integer, parameter :: INTK   = 4
  integer(kind=TYPHK), parameter :: TYPH_REAL        = 1
  integer(kind=TYPHK), parameter :: TY_MEM_ALLOC   = 1111
  integer(kind=TYPHK), parameter :: TY_MEM_DEALLOC = 1112
  integer(kind=TYPHK), parameter :: TYPH_GHOSTS_ONE   = 1
  integer(kind=TYPHK), parameter :: TYPH_MESH_DIM     = -88

end module TYPH_Types_mod
  
! EOF

