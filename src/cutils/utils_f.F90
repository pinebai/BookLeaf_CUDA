
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

module utils_f_mod

  implicit none

  integer,parameter,public :: UTILS_SUCCESS=0

  public :: UTILS_mkdir_f,UTILS_ln_f

contains

  integer function UTILS_mkdir_f(path)
    
    implicit none
    
    character(len=*), intent(in) :: path
    integer :: err
    
    call utils_mkdir_c(path, len_trim(path), err)
    UTILS_mkdir_f = err
    
  end function UTILS_mkdir_f
  
  integer function UTILS_ln_f(path, link_path)
    
    implicit none
    
    character(len=*), intent(in) :: path, link_path
    integer :: err
    
    call utils_ln_c(path, len_trim(path), link_path, len_trim(link_path), err)
    UTILS_ln_f = err
    
  end function UTILS_ln_f
  
end module utils_f_mod 
