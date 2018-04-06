!***********************************************************************
!*                   GNU Lesser General Public License                 
!*
!* This file is part of the FV3 dynamical core.
!*
!* The FV3 dynamical core is free software: you can redistribute it 
!* and/or modify it under the terms of the
!* GNU Lesser General Public License as published by the
!* Free Software Foundation, either version 3 of the License, or 
!* (at your option) any later version.
!*
!* The FV3 dynamical core is distributed in the hope that it will be 
!* useful, but WITHOUT ANYWARRANTY; without even the implied warranty 
!* of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
!* See the GNU General Public License for more details.
!*
!* You should have received a copy of the GNU Lesser General Public
!* License along with the FV3 dynamical core.  
!* If not, see <http://www.gnu.org/licenses/>.
!***********************************************************************

!>@brief The module 'sim_nc' is a netcdf file reader.
!>@details The code is necessary to circumvent issues with the FMS
!! 'read_data' utility, which opens too many files and uses excessive
!! memory.
!>@author Shian-Jiann Lin

module sim_nc_mod

! This is S-J Lin's private netcdf file reader
! This code is needed because FMS utility (read_data) led to too much
! memory usage and too many files openned. Perhaps lower-level FMS IO
! calls should be used instead.

#if defined(OLD_PT_TO_T) || defined(OLD_COS_SG)
#error
#error Compile time options -DOLD_PT_TO_T and -DOLD_COS_SG are no longer supported. Please remove them from your XML.
#error
#endif

 use mpp_mod,     only: mpp_error, FATAL

 implicit none
#include <netcdf.inc>

 private
 public  open_ncfile, close_ncfile, get_ncdim1, get_var1_double, get_var2_double,   &
         get_var3_real, get_var3_double, get_var3_r4, get_var2_real, get_var2_r4,   &
         handle_err, check_var, get_var1_real, get_var_att_double, &
         check_var_exists 

 contains

      subroutine open_ncfile( iflnm, ncid )
      character(len=*), intent(in)::  iflnm
      integer, intent(out)::      ncid
      integer::  status

      status = nf_open (iflnm, NF_NOWRITE, ncid)
      if (status .ne. NF_NOERR) call handle_err('nf_open',status)


      end subroutine open_ncfile


      subroutine close_ncfile( ncid )
      integer, intent(in)::    ncid
      integer::  status

      status = nf_close (ncid)
      if (status .ne. NF_NOERR) call handle_err('nf_close',status)


      end subroutine close_ncfile


      subroutine get_ncdim1( ncid, var1_name, im )
      integer, intent(in):: ncid
      character(len=*), intent(in)::  var1_name
      integer, intent(out):: im
      integer::  status, var1id

      status = nf_inq_dimid (ncid, var1_name, var1id)
      if (status .ne. NF_NOERR) call handle_err('dimid '//var1_name,status)

      status = nf_inq_dimlen (ncid, var1id, im)
      if (status .ne. NF_NOERR) call handle_err('dimid '//var1_name,status)

      end subroutine get_ncdim1

!>@brief The 'get_var' subroutines read in variables from netcdf files
      subroutine get_var1_double( ncid, var1_name, im, var1, var_exist )
      integer, intent(in):: ncid
      character(len=*), intent(in)::  var1_name
      integer, intent(in):: im
      logical, intent(out), optional:: var_exist
      real(kind=8), intent(out):: var1(im)
      integer::  status, var1id

      status = nf_inq_varid (ncid, var1_name, var1id)
      if (status .ne. NF_NOERR) then
!         call handle_err('varid '//var1_name,status)
          if(present(var_exist) ) var_exist = .false.
      else
          status = nf_get_var_double (ncid, var1id, var1)
          if (status .ne. NF_NOERR) call handle_err('varid '//var1_name,status)
          if(present(var_exist) ) var_exist = .true.
      endif


      end subroutine get_var1_double


! 4-byte data:
      subroutine get_var1_real( ncid, var1_name, im, var1, var_exist )
      integer, intent(in):: ncid
      character(len=*), intent(in)::  var1_name
      integer, intent(in):: im
      logical, intent(out), optional:: var_exist
      real(kind=4), intent(out):: var1(im)
      integer::  status, var1id

      status = nf_inq_varid (ncid, var1_name, var1id)
      if (status .ne. NF_NOERR) then
!         call handle_err(status)
          if(present(var_exist) ) var_exist = .false.
      else
          status = nf_get_var_real (ncid, var1id, var1)
          if (status .ne. NF_NOERR) call handle_err('get_var1_real1 '//var1_name,status)
          if(present(var_exist) ) var_exist = .true.
      endif


      end subroutine get_var1_real

      subroutine get_var2_real( ncid, var_name, im, jm, var2 )
      integer, intent(in):: ncid
      character(len=*), intent(in)::  var_name
      integer, intent(in):: im, jm
      real(kind=4), intent(out):: var2(im)

      integer::  status, var1id

      status = nf_inq_varid (ncid, var_name, var1id)
      if (status .ne. NF_NOERR) call handle_err('get_var2_real varid '//var_name,status)

      status = nf_get_var_real (ncid, var1id, var2)
      if (status .ne. NF_NOERR) call handle_err('get_var2_real get_var'//var_name,status)

      end subroutine get_var2_real

      subroutine get_var2_r4( ncid, var2_name, is,ie, js,je, var2, time_slice )
      integer, intent(in):: ncid
      character(len=*), intent(in)::  var2_name
      integer, intent(in):: is, ie, js, je
      real(kind=4), intent(out):: var2(is:ie,js:je)
      integer, intent(in), optional :: time_slice
!
      real(kind=4), dimension(1) :: time
      integer, dimension(3):: start, nreco
      integer:: status, var2id

      status = nf_inq_varid (ncid, var2_name, var2id)
      if (status .ne. NF_NOERR) call handle_err('get_var2_r4 varid'//var2_name,status)

      start(1) = is; start(2) = js; start(3) = 1
      if ( present(time_slice) ) then
         start(3) = time_slice
      end if

      nreco(1) = ie - is + 1
      nreco(2) = je - js + 1
      nreco(3) = 1

      status = nf_get_vara_real(ncid, var2id, start, nreco, var2)
      if (status .ne. NF_NOERR) call handle_err('get_var2_r4 get_vara_real'//var2_name,status)

      end subroutine get_var2_r4

      subroutine get_var2_double( ncid, var2_name, im, jm, var2 )
      integer, intent(in):: ncid
      character(len=*), intent(in)::  var2_name
      integer, intent(in):: im, jm
      real(kind=8), intent(out):: var2(im,jm)

      integer::  status, var2id

      status = nf_inq_varid (ncid, var2_name, var2id)
      if (status .ne. NF_NOERR) call handle_err('get_var2_double varid'//var2_name,status)

      status = nf_get_var_double (ncid, var2id, var2)
      if (status .ne. NF_NOERR) call handle_err('get_var2_double get_var_double'//var2_name,status)


      end subroutine get_var2_double


      subroutine get_var3_double( ncid, var3_name, im, jm, km, var3 )
      integer, intent(in):: ncid
      character(len=*), intent(in)::  var3_name
      integer, intent(in):: im, jm, km
      real(kind=8), intent(out):: var3(im,jm,km)

      integer::  status, var3id

      status = nf_inq_varid (ncid, var3_name, var3id)

      if (status .ne. NF_NOERR) &
          call handle_err('get_var3_double varid '//var3_name,status)

      status = nf_get_var_double (ncid, var3id, var3)
      if (status .ne. NF_NOERR) &
          call handle_err('get_var3_double get_vara_double '//var3_name,status)

      end subroutine get_var3_double

      subroutine get_var3_real( ncid, var3_name, im, jm, km, var3 )
      integer, intent(in):: ncid
      character(len=*), intent(in)::  var3_name
      integer, intent(in):: im, jm, km
      real(kind=4), intent(out):: var3(im,jm,km)

      integer::  status, var3id

      status = nf_inq_varid (ncid, var3_name, var3id)

      if (status .ne. NF_NOERR) &
                call handle_err('get_var3_real varid '//var3_name,status)
      status = nf_get_var_real (ncid, var3id, var3)

      if (status .ne. NF_NOERR) &
                call handle_err('get_var3_real get_var_real '//var3_name,status)

      end subroutine get_var3_real


      subroutine check_var_exists(ncid, var_name, status)
      integer, intent(in):: ncid
      integer, intent(inout) :: status
      character(len=*), intent(in)::  var_name
      integer:: varid
      status = nf_inq_varid (ncid, var_name, varid)
      end subroutine check_var_exists

      subroutine get_var3_r4( ncid, var3_name, is,ie, js,je, ks,ke, var3, time_slice )
      integer, intent(in):: ncid
      character(len=*), intent(in)::  var3_name
      integer, intent(in):: is, ie, js, je, ks,ke
      real(kind=4), intent(out):: var3(is:ie,js:je,ks:ke)
      integer, intent(in), optional :: time_slice
!
      real(kind=4), dimension(1) :: time
      integer, dimension(4):: start, nreco
      integer:: status, var3id

      status = nf_inq_varid (ncid, var3_name, var3id)
      if (status .ne. NF_NOERR) call handle_err('get_var3_r4 varid '//var3_name,status)

      start(1) = is; start(2) = js; start(3) = ks; start(4) = 1
      if ( present(time_slice) ) then
         start(4) = time_slice
      end if

      nreco(1) = ie - is + 1 
      nreco(2) = je - js + 1
      nreco(3) = ke - ks + 1
      nreco(4) = 1

      status = nf_get_vara_real(ncid, var3id, start, nreco, var3)
      if (status .ne. NF_NOERR) call handle_err('get_var3_r4 get_vara_real '//var3_name,status)

      end subroutine get_var3_r4


      subroutine get_var4_real( ncid, var4_name, im, jm, km, nt, var4 )
      implicit         none
#include <netcdf.inc>
      integer, intent(in):: ncid
      character*(*), intent(in)::  var4_name
      integer, intent(in):: im, jm, km, nt
      real*4:: wk4(im,jm,km,4)
      real*4, intent(out):: var4(im,jm)
      integer::  status, var4id
      integer:: start(4), icount(4) 
      integer:: i,j

      start(1) = 1
      start(2) = 1
      start(3) = 1
      start(4) = nt

      icount(1) = im    ! all range
      icount(2) = jm    ! all range
      icount(3) = km    ! all range
      icount(4) = 1     ! one time level at a time

!     write(*,*) nt, 'Within get_var4_double: ', var4_name

      status = nf_inq_varid (ncid, var4_name, var4id)
!     write(*,*) '#1', status, ncid, var4id

      status = nf_get_vara_real(ncid, var4id, start, icount, var4)
!     status = nf_get_vara_real(ncid, var4id, start, icount, wk4)
!     write(*,*) '#2', status, ncid, var4id

      do j=1,jm
      do i=1,im
!        var4(i,j) = wk4(i,j,1,nt)
      enddo
      enddo

      if (status .ne. NF_NOERR) call handle_err('get_var4_r4 get_vara_real '//var4_name,status)

      end subroutine get_var4_real


      subroutine get_var4_double( ncid, var4_name, im, jm, km, nt, var4 )
      integer, intent(in):: ncid
      character(len=*), intent(in)::  var4_name
      integer, intent(in):: im, jm, km, nt
      real(kind=8), intent(out):: var4(im,jm,km,1)
      integer::  status, var4id
!
      integer:: start(4), icount(4) 

      start(1) = 1
      start(2) = 1
      start(3) = 1
      start(4) = nt

      icount(1) = im    ! all range
      icount(2) = jm    ! all range
      icount(3) = km    ! all range
      icount(4) = 1     ! one time level at a time

      status = nf_inq_varid (ncid, var4_name, var4id)
      status = nf_get_vara_double(ncid, var4id, start, icount, var4)

      if (status .ne. NF_NOERR) call handle_err('get_var4_double get_vara_double '//var4_name,status)

      end subroutine get_var4_double
!------------------------------------------------------------------------

      subroutine get_real3( ncid, var4_name, im, jm, nt, var4 )
! This is for multi-time-level 2D var
      integer, intent(in):: ncid
      character(len=*), intent(in)::  var4_name
      integer, intent(in):: im, jm, nt
      real(kind=4), intent(out):: var4(im,jm)
      integer::  status, var4id
      integer:: start(3), icount(3)
      integer:: i,j

      start(1) = 1
      start(2) = 1
      start(3) = nt

      icount(1) = im
      icount(2) = jm
      icount(3) = 1

      status = nf_inq_varid (ncid, var4_name, var4id)
      status = nf_get_vara_real(ncid, var4id, start, icount, var4)

      if (status .ne. NF_NOERR) &
         call handle_err('get_real3 get_vara_real '//var4_name,status)

      end subroutine get_real3
!------------------------------------------------------------------------

      logical function check_var( ncid, var3_name)
      integer, intent(in):: ncid
      character(len=*), intent(in)::  var3_name

      integer::  status, var3id

      status = nf_inq_varid (ncid, var3_name, var3id)
      check_var = (status == NF_NOERR) 

      end function check_var

      subroutine get_var_att_str(ncid, var_name, att_name, att)
      implicit none
#include <netcdf.inc>
      integer, intent(in):: ncid
      character*(*), intent(in)::  var_name, att_name
      character*(*), intent(out)::  att

      integer::  status, varid

      status = nf_inq_varid (ncid, var_name, varid)
      status = nf_get_att_text(ncid, varid, att_name, att)

      if (status .ne. NF_NOERR) call handle_err('get_var_att_str '//var_name,status)

      end subroutine get_var_att_str

      subroutine get_var_att_double(ncid, var_name, att_name, value)
      implicit none
#include <netcdf.inc>
      integer, intent(in):: ncid
      character*(*), intent(in)::  var_name, att_name
      real(kind=8), intent(out)::  value

      integer::  status, varid

      status = nf_inq_varid (ncid, var_name, varid)
      status = nf_get_att(ncid, varid, att_name, value)

      if (status .ne. NF_NOERR) call handle_err('get_var_att_double '//var_name,status)

      end subroutine get_var_att_double


      subroutine handle_err(idstr,status)
      integer          status
      character(len=500) :: errstr
      character(len=*) :: idstr

      if (status .ne. nf_noerr) then
         write(errstr,*) 'Error in handle_err: ',trim(idstr)//' ',NF_STRERROR(STATUS)
         call mpp_error(FATAL,errstr)
      endif

      end subroutine handle_err

!>@brief The subroutine 'calendar' computes the current GMT.
   subroutine calendar(year, month, day, hour)
      integer, intent(inout) :: year              ! year
      integer, intent(inout) :: month             ! month
      integer, intent(inout) :: day               ! day
      integer, intent(inout) :: hour
!
! Local variables
!
      integer irem4,irem100
      integer mdays(12)                           !< number day of month 
      data mdays /31,28,31,30,31,30,31,31,30,31,30,31/
!**** consider leap year
!
      irem4    = mod( year, 4 )
      irem100  = mod( year, 100 )
      if( irem4 == 0 .and. irem100 /= 0) mdays(2) = 29
!
      if( hour >= 24 ) then
        day    = day + 1
        hour   = hour - 24
      end if

      if( day > mdays(month) ) then
        day    = day - mdays(month)
        month  = month + 1
      end if
      if( month > 12 ) then
        year   = year + 1
        month  = 1
      end if

  end subroutine calendar

end module sim_nc_mod
