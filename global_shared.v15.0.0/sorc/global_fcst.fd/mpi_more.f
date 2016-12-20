      subroutine glob_abort(ie,s,rc)
      implicit none
      include 'mpif.h'
      integer rc,ie,ierr
      character*(*) s
      if (ie.ne.0) then
        print*,'glob_abort: '//s//' ie,rc:',ie,rc
        if (rc.eq.0) return
        call mpi_abort(mpi_comm_world,rc,ierr)
      end if
      return
      end
c
c***********************************************************************
c
