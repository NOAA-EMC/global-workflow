!***********************************************************************
!*                   GNU General Public License                        *
!* This file is a part of fvGFS.                                       *
!*                                                                     *
!* fvGFS is free software; you can redistribute it and/or modify it    *
!* and are expected to follow the terms of the GNU General Public      *
!* License as published by the Free Software Foundation; either        *
!* version 2 of the License, or (at your option) any later version.    *
!*                                                                     *
!* fvGFS is distributed in the hope that it will be useful, but        *
!* WITHOUT ANY WARRANTY; without even the implied warranty of          *
!* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
!* General Public License for more details.                            *
!*                                                                     *
!* For the full text of the GNU General Public License,                *
!* write to: Free Software Foundation, Inc.,                           *
!*           675 Mass Ave, Cambridge, MA 02139, USA.                   *
!* or see:   http://www.gnu.org/licenses/gpl.html                      *
!***********************************************************************
subroutine _SUBNAME_(errortype, errormsg1, array1, errormsg2, array2, errormsg3)
  integer,            intent(in) :: errortype
  _ARRAY1TYPE_, dimension(:), intent(in) :: array1
  _ARRAY2TYPE_, dimension(:), intent(in) :: array2
  character(len=*),      intent(in) :: errormsg1, errormsg2
  character(len=*),   intent(in), optional :: errormsg3
  character(len=512) :: string

  string = errormsg1//trim(array_to_char(array1)) 
  string = trim(string)//errormsg2//trim(array_to_char(array2)) 
  if(present(errormsg3)) string = trim(string)//errormsg3
  call mpp_error_basic( errortype, trim(string))

end subroutine _SUBNAME_
