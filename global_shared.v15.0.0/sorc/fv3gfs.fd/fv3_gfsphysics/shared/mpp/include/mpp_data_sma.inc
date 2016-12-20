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

! -*-f90-*-
! $Id$

!----------------------------------------------------------------!
! The following data is used in mpp_mod and its components       !
!----------------------------------------------------------------!
real(DOUBLE_KIND)                       :: mpp_stack(1)
integer                                 :: mpp_from_pe
integer(LONG_KIND), dimension(0:MAXPES) :: status, remote_data_loc
integer :: sync(SHMEM_REDUCE_SYNC_SIZE+SHMEM_BCAST_SYNC_SIZE+SHMEM_BARRIER_SYNC_SIZE)


!we call shpalloc in mpp_init() to ensure all these are remotely accessible
!on PVP where shpalloc doesn't exist, module variables are automatically
!guaranteed to be remotely accessible
pointer( ptr_stack, mpp_stack )
pointer( ptr_sync, sync )
pointer( ptr_status, status )
pointer( ptr_from, mpp_from_pe )
pointer( ptr_remote, remote_data_loc )


!--- these are some dummy data
integer, parameter :: stat = -999

!-------------------------------------------------------------------!
! The following data is used in mpp_domains_mod and its components  !
!-------------------------------------------------------------------!
real(DOUBLE_KIND) :: mpp_domains_stack(1)
real(DOUBLE_KIND) :: mpp_domains_stack_nonblock(1)
pointer( ptr_domains_stack, mpp_domains_stack )  
pointer( ptr_domains_stack_nonblock, mpp_domains_stack_nonblock )  
