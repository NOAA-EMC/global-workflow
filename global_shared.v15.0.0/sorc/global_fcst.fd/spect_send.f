      subroutine spect_send  (nsig,ioproc,fhour,idate,
     x           geshem,
     &           global_lats_r,lonsperlar,
     &           trieo_ls_node,
     x           trieo_ls_nodes_buf)
!    x           trieo_ls_nodes_buf,xgf)
cc
c  get node local state array data in trieo_ls_node  off
c  the compute nodes.  currently done with a gather
c  to a single i/o node.   the full state is in
c  trieo_ls_nodes_buf on the i/o task at the end of the gather
c  but it still has to be assembled into contiguous
c  records.  that is done offline on the i/o task.
c
cc
      use resol_def
      use layout1
      use mpi_def
c
      implicit none
cc
      integer              nsig,ioproc
      real(kind=8) t1,t2,rtc
!     real(kind=8) t3,t4,t5,t6,ta,tb
c
      real(kind=kind_evod) fhour
cc
      integer              idate(4)
cc
cc
      real (kind=kind_io8) geshem(lonr,lats_node_r)
!!
cc
cc
      integer              ierr,j,k,l,lenrec,n
!     integer              ierr,j,k,l,lenrec,locl,n,node
cc
cc
!     real(kind=kind_evod) xgf
cc
cc
      integer              global_lats_r(latr)
      integer                 lonsperlar(latr)
cc
      integer  il,i
cc
cc
cc
       real(kind=kind_mpi)   trieo_ls_node
     1(len_trie_ls_max+len_trio_ls_max, 2, 3*levs+1*levh+1)
c
      real(kind=kind_mpi)trieo_ls_nodes_buf
     1 (len_trie_ls_max+len_trio_ls_max, 2, 3*levs+1*levh+1,nodes,1)
cc
!     integer kmsk0(lonr,lats_node_r)
cc
      real(kind=kind_io4) z(lnt2)
!     real(kind=kind_io8) buffo(lonr,lats_node_r)
!     real(kind=kind_io4) buff2(lonr,latr)
!     integer len_trie_ls_nod, len_trio_ls_nod
 
c   all fields over each node's subset of the domain are present
c   in trieo_ls_node.    gather these together
cc
      lenrec = (len_trie_ls_max+len_trio_ls_max) * 2 * (3*levs+1*levh+1)
c  lenrec is state buffer size on each node.   full state is gathered below
cc
      t1=rtc()
      call mpi_gather( trieo_ls_node , lenrec, mpi_r_mpi,
     x                 trieo_ls_nodes_buf(1,1,1,1,1), lenrec, mpi_r_mpi,
     x                 ioproc, mpi_comm_all, ierr)
      t2=rtc()
cc
c      deallocate ( trieo_ls_node  )
cc
c        the compute tasks have finished state assembly at  this point.
c   after this is done all tasks will have to assemble one grid  to be written to
c   the sigma file.
c
c  assemble a full grid from the subset (subdomains) on the nodes.
c  the ioproc gets the grid but does not write it out yet.
cc
!     kmsk0=0
!     t1=rtc()
!     call uninterpred(2,kmsk0,buffo,geshem,global_lats_r,lonsperlar)
!     t2=rtc()
!     call unsplit2d(ioproc,buff2,buffo,global_lats_r)
!     t3=rtc()
!     if (me.eq.ioproc) then
!     buff_grid=buff2
!     endif
!  end grid assembly
!        t4=rtc()
      return
      end
 
 
