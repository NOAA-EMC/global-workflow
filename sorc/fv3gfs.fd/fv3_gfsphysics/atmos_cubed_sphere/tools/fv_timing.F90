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
      module fv_timing_mod

      use mpp_mod, only: mpp_error, FATAL
#if defined(SPMD)
      use fv_mp_mod, only: is_master, mp_reduce_max
#endif
!
! ... Use system etime() function for timing
!
      implicit none

      integer, private      :: nblks
      parameter  (nblks   = 100)

      character(len=20), private :: blkname(nblks)

      integer , private      :: tblk

#if defined(SPMD)
      real(kind=8) , external       :: MPI_Wtime
#endif
      real , private       :: etime
      real(kind=8) , private       :: totim
      real , private       :: tarray(2)
      type tms
           private
           real (kind=8) :: usr, sys
      end type tms


      type (tms), private   :: accum(nblks), last(nblks)

      real , private       :: us_tmp1(nblks,2)
      real , private       :: us_tmp2(nblks,2)

      logical, private :: module_initialized = .false.

!---- version number -----
      character(len=128) :: version = '$Id$'
      character(len=128) :: tagname = '$Name$'

      contains
         subroutine timing_init
!
! init
!
         implicit none

         integer  :: C, R, M
         real (kind=8) :: wclk

         integer  n

         if ( module_initialized ) return

         tblk=0
         do n = 1, nblks
            accum(n)%usr = 0.
            accum(n)%sys = 0.
            last(n)%usr  = 0.
            last(n)%sys  = 0.
         end do
!
! ... To reduce the overhead for the first call
!
#if defined(SPMD)
    wclk = MPI_Wtime() 
    totim = wclk
#else
#   if defined( IRIX64 ) || ( defined FFC )
         totim = etime(tarray)
#   else
         CALL SYSTEM_CLOCK(Count=C, Count_Rate=R, Count_Max=M)
         wclk =  REAL(C) / REAL(R)
         totim = wclk
#   endif
#endif

         module_initialized = .true.
         end subroutine timing_init


         subroutine timing_on(blk_name)
!
! timing_on
!

         implicit none

         character(len=*) :: blk_name



         character(len=20) :: UC_blk_name
         character(len=20) ::  ctmp 
         integer i
         integer iblk

         integer :: C, R, M
         real (kind=8)  :: wclk

         integer ierr

         if ( .not. module_initialized ) then
            call timing_init()
         end if

         UC_blk_name = blk_name

         call upper(UC_blk_name,len_trim(UC_blk_name))
!c         ctmp=UC_blk_name(:len_trim(UC_blk_name))
         ctmp=trim(UC_blk_name)

!         write(*,*) 'timing_on ', ctmp
         iblk=0
         do i=1, tblk
            if ( ctmp .EQ. blkname(i) ) then
               iblk =i
            endif
         enddo
      
         if ( iblk .eq. 0 ) then
            tblk=tblk+1
            iblk=tblk
            call upper(UC_blk_name,len_trim(UC_blk_name))
!C            blkname(iblk)=UC_blk_name(:len_trim(UC_blk_name))
            blkname(iblk)=trim(UC_blk_name)

        endif

#if defined(SPMD)
        wclk = MPI_Wtime()
        last(iblk)%usr = wclk
        last(iblk)%sys = 0.0
#else
# if defined( IRIX64 ) || ( defined FFC )
        totim = etime(tarray)
        last(iblk)%usr = tarray(1)
        last(iblk)%sys = tarray(2)
# else
        CALL SYSTEM_CLOCK(Count=C, Count_Rate=R, Count_Max=M)
        wclk = REAL(C) / REAL(R)
        last(iblk)%usr = wclk
        last(iblk)%sys = 0.0
# endif
#endif  

        end subroutine timing_on


        subroutine timing_off(blk_name)
!
! Timing_off
!

        implicit none
        character(len=*) :: blk_name

        character(len=20) :: UC_blk_name
        character(len=20) :: ctmp
        integer i

        integer  :: C, R, M
        real (kind=8)   :: wclk

        integer  iblk

        UC_blk_name = blk_name

        call upper(UC_blk_name,len_trim(UC_blk_name))
!v        ctmp=UC_blk_name(:len_trim(UC_blk_name))
        ctmp=trim(UC_blk_name)

        iblk=0
        do i=1, tblk
           if ( ctmp .EQ. blkname(i) ) then
              iblk =i
           endif
        enddo
      
!         write(*,*) 'timing_off ', ctmp, tblk, tblk
        if ( iblk .eq. 0 ) then
            call mpp_error(FATAL,'fv_timing_mod: timing_off called before timing_on for: '//trim(blk_name))
!           write(*,*) 'stop in timing off in ', ctmp
!           stop 
        endif

#if defined(SPMD)
        wclk = MPI_Wtime()
        accum(iblk)%usr = accum(iblk)%usr + wclk - last(iblk)%usr
        accum(iblk)%sys = 0.0
        last(iblk)%usr  = wclk
        last(iblk)%sys  = 0.0
#else
# if defined( IRIX64 ) || ( defined FFC ) 
        totim = etime(tarray)
        accum(iblk)%usr = accum(iblk)%usr +           &
                        tarray(1) - last(iblk)%usr
        accum(iblk)%sys = accum(iblk)%sys +           &
                        tarray(2) - last(iblk)%sys
        last(iblk)%usr = tarray(1)
        last(iblk)%sys = tarray(2)
# else
        CALL SYSTEM_CLOCK(Count=C, Count_Rate=R, Count_Max=M)
        wclk = REAL(C) / REAL(R)
        accum(iblk)%usr = accum(iblk)%usr + wclk - last(iblk)%usr
        accum(iblk)%sys = 0.0
        last(iblk)%usr  = wclk
        last(iblk)%sys  = 0.0
# endif
#endif
        end subroutine timing_off


        subroutine timing_clear()
        integer  n
          do n = 1, nblks
             accum(n)%usr = 0
             accum(n)%sys = 0
          enddo
        end subroutine timing_clear


        subroutine timing_prt(gid)
!
! Timing_prt
!
        implicit none
        integer  gid
        integer  n

        type (tms)   :: others, tmp(nblks)
        real         :: tmpmax

#if defined( SPMD )
        do n = 1, nblks                   !will clean these later
           tmpmax = accum(n)%usr
           call mp_reduce_max(tmpmax)
           tmp(n)%usr = tmpmax
           tmpmax = accum(n)%sys
           call mp_reduce_max(tmpmax)
           tmp(n)%sys = tmpmax
        enddo
        if ( is_master() ) then
#else
        do n = 1, nblks
           tmp(n)%usr = accum(n)%usr
           tmp(n)%sys = accum(n)%sys
        enddo
#endif

        print *
        print *,                                  &
        '  -----------------------------------------------------'
        print *,                                  &
        '     Block                    User time  System Time   Total Time   GID '
        print *,                                  &
        '  -----------------------------------------------------'

        do n = 1, tblk
           print '(3x,a20,2x,3(1x,f12.4), 2x, I6)', blkname(n),     &
               tmp(n)%usr, tmp(n)%sys, tmp(n)%usr + tmp(n)%sys, gid
        end do


        print *
#if defined( SPMD )
        endif ! masterproc
#endif

        end subroutine timing_prt

      subroutine upper(string,length)

!***********************************************************************
!
!     upper.f - change lower case letter to upper case letter          *
!                                                                      *
!     George Lai Tue Jun 28 16:37:00 1994                              *
!                                                                      *
!***********************************************************************

      implicit         none

!      character string(length)
!      character(len=20) string
!      character, dimension(length) :: string
!      character (len=*), intent(inout) ::  string
!      character (len=*) ::  string
!      character (len=1), intent(inout) ::  string(20)
!ok      character (len=20), intent(inout) ::  string
      character (len=*), intent(inout) ::  string
      character char1
      integer,   intent(in)    ::  length
      integer i
      integer a, z, dist
      a = ichar('a')
      z = ichar('z')
      dist = ichar('A') - a

      do i = 1,length
        char1=string(i:i)
        if (ichar(char1) .ge. a .and.       &
            ichar(char1) .le. z) then
          string(i:i) = char(ichar(char1)+dist)
        endif
      end do

      return
      end subroutine upper

      end module fv_timing_mod
