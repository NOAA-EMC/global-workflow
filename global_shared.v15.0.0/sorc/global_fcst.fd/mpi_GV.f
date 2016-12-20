      subroutine mpi_gathe4(src,lenin,itype,agg,lenin2,itype2,iroot
     &,                     icomm,ier)
      use mpi
      implicit none
      real*4 src(*),agg(*)
      integer istatu(mpi_status_size),ier,nrank,iroot,itype2,itype,
     &        icomm,lenin,lenin2,iend,ioff,k,ktag,isize

      call mpi_comm_size(icomm,isize,ier)
      call mpi_comm_rank(icomm,nrank,ier)
      if(nrank == iroot) then
        do k=0,isize-1
          ioff = 1 + (k*lenin)
          ktag = k
          if(k .ne. iroot) call
     &        mpi_recv(agg(ioff),lenin,itype,k,ktag,icomm,istatu,ier)
          iend =ioff + lenin - 1
          if(k .eq. iroot) agg(ioff:ioff+lenin-1)=src(1:lenin)
        end do
      else
        call mpi_send(src,lenin,itype,iroot,nrank,icomm,ier)
      endif
      return
      end
      subroutine mpi_gathe8(src,lenin,itype,agg,lenin2,itype2,iroot
     &,                     icomm,ier)
      use mpi
      implicit none
      real*8 src(*),agg(*)
      integer istatu(mpi_status_size),ier,nrank,iroot,itype2,itype,
     &        icomm,lenin,lenin2,iend,ioff,k,ktag,isize

      call mpi_comm_size(icomm,isize,ier)
      call mpi_comm_rank(icomm,nrank,ier)
      if(nrank == iroot) then
        do k=0,isize-1
          ioff = 1 + (k*lenin)
          ktag = k
          if(k .ne. iroot) call
     &        mpi_recv(agg(ioff),lenin,itype,k,ktag,icomm,istatu,ier)
          iend =ioff + lenin - 1
          if(k .eq. iroot) agg(ioff:ioff+lenin-1)=src(1:lenin)
        end do
      else
        call mpi_send(src,lenin,itype,iroot,nrank,icomm,ier)
      endif
      return
      end
