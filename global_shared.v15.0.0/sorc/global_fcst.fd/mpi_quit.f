      subroutine mpi_quit(iret)
      use mpi_def
      implicit none
!
      integer iret
 
      write(0,*) 'call stop mpi_quit ',iret

      if (comp_task) then
        call mpi_abort(mc_comp,iret,info)
      else
        call mpi_abort(mc_io,iret,info)
      endif
 
      return
      end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
