      module tiles_cc
      
      use atm_cc

      implicit none

      integer process_rank_tiles,stat(mpi_status_size),
     >master_rank_tiles,ierr
      integer,dimension(:),allocatable:: ipt_lats_node_rs,lats_node_rs
      logical init /.true./, master

      save

      end module tiles_cc
c
c***********************************************************************
c
      subroutine initialize_tiling

      use tiles_cc

      implicit none

      integer i
c

      if (.not.init) return

      comp=process_rank_local.lt.tiles_nprocs

      master=component_master_rank_local.eq.process_rank_local

      if (master .and. .not.comp) then
        call glob_abort(1,'am: initialize_tiling: comp is .false. '//
     >  'for coupling master process',1)
      end if

      if (comp) then

        call mpi_comm_size(comm_tiles,i,ierr)
        call glob_abort(ierr,'am: mpi_comm_size(comm_tiles,...) '//
     >  'failure in coupling proc. in initialize_tiling',ierr)
        call glob_abort(i-tiles_nprocs,'am: size of comm_tiles.ne. '//
     >  'tiles_nprocs in initialize_tiling',i-tiles_nprocs)
        
        call mpi_comm_rank(comm_tiles,process_rank_tiles,ierr)
        if (master) master_rank_tiles=process_rank_tiles
        call mpi_bcast(master_rank_tiles,1,
     >  mpi_integer,master_rank_tiles,comm_tiles,ierr)

        allocate(ipt_lats_node_rs(0:tiles_nprocs-1))
        call mpi_gather(ipt_lats_node_r,1,mpi_integer,
     >  ipt_lats_node_rs,1,mpi_integer,
     >  master_rank_tiles,comm_tiles,ierr)
        call glob_abort(ierr,
     >  'am: mpi_gather(ipt_lats_node_r,...) failure',ierr)

        allocate(lats_node_rs(0:tiles_nprocs-1))
        call mpi_gather(lats_node_r,1,mpi_integer,
     >  lats_node_rs,1,mpi_integer,
     >  master_rank_tiles,comm_tiles,ierr)
        call glob_abort(ierr,
     >  'am: mpi_gather(lats_node_r,...) failure',ierr)

      end if

      init=.false.

      call atm_announce('initialize_tiling: exiting',2)

      end
c
c***********************************************************************
c
      subroutine assemble_cc(x,xl)

c***********************************************************************

      use tiles_cc

      implicit none
!!
      real(kind=kind_real) x(lonr,latr)
      real (kind=kind_real) xl(lonr,lats_node_r)
      real(kind=kind_real) tmp(lonr,latr)
      integer ipt_lats_node_rl
      integer lats_nodes_rl
      integer maxfld,nproct
      integer proc,j,lat,msgtag,i
      integer ifldu/0/
      save ifldu
      integer illen
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      call initialize_tiling

      if (.not.comp) return
!!
      maxfld=50
      ifldu=ifldu+1
!!
      nproct=tiles_nprocs

      if (.not.master) then
c
c         sending the data
c         ----------------
        msgtag=1000+(process_rank_tiles+1)*nproct*maxfld+ifldu
        call mpi_send(xl,lats_node_r*lonr,mpi_kind_real,
     >                  master_rank_tiles,
     &                  msgtag,comm_tiles,ierr)
      else
!!
        x = 0.0               ! added by moorthi on 2005111700
        do j=1,lats_node_r
          lat=global_lats_r(ipt_lats_node_r-1+j)
          do i=1,lonr
            x(i,lat)=xl(i,j)
          enddo
        enddo
        do proc=0,nproct-1
          if (proc.eq.master_rank_tiles) cycle
          msgtag=1000+(proc+1)*nproct*maxfld+ifldu
          illen=lats_node_rs(proc)
          call mpi_recv(tmp,illen*lonr,mpi_kind_real,proc,
     &                msgtag,comm_tiles,stat,ierr)
          ipt_lats_node_rl=ipt_lats_node_rs(proc)
          lats_nodes_rl=illen
          do j=1,lats_nodes_rl
            lat=global_lats_r(ipt_lats_node_rl-1+j)
            do i=1,lonr
              x(i,lat)=tmp(i,j)
            enddo
          enddo
        enddo
!!
      endif
!!
      return
      end
c
c***********************************************************************
c
      subroutine disassemble_cc(x,xl)

c***********************************************************************

      use tiles_cc

      implicit none

      real(kind=kind_real) x(lonr,latr),xl(lonr,lats_node_r)

      real(kind=kind_real) tmp(lonr,latr)
      integer ipt_lats_node_rl
      integer lats_nodes_rl
      integer maxfld,nproct
      integer proc,j,lat,msgtag,i
      integer ifldu/0/
      save ifldu
      integer illen
c

      call initialize_tiling

      if (.not.comp) return
!!
      xl=0.
      maxfld=50
      ifldu=ifldu+1
!!
      nproct=tiles_nprocs

      if (.not.master) then
c
c         receiving the data
c         ----------------
        msgtag=1111+(process_rank_tiles+1)*nproct*maxfld+ifldu
        call mpi_recv(xl,lats_node_r*lonr,mpi_kind_real,
     >                  master_rank_tiles,
     &                  msgtag,comm_tiles,stat,ierr)
      else
!!
        do j=1,lats_node_r
          lat=global_lats_r(ipt_lats_node_r-1+j)
          do i=1,lonr
            xl(i,j)=x(i,lat)
          enddo
        enddo
        do proc=0,nproct-1
          if (proc.eq.master_rank_tiles) cycle
          ipt_lats_node_rl=ipt_lats_node_rs(proc)
          illen=lats_node_rs(proc)
          lats_nodes_rl=illen
          do j=1,lats_nodes_rl
            lat=global_lats_r(ipt_lats_node_rl-1+j)
            do i=1,lonr
              tmp(i,j)=x(i,lat)
            enddo
          enddo
          msgtag=1111+(proc+1)*nproct*maxfld+ifldu
          call mpi_send(tmp,illen*lonr,mpi_kind_real,proc,
     &                msgtag,comm_tiles,ierr)
        enddo
!!
      endif
!!
      return
      end
c
c***********************************************************************
c
      subroutine interpred_cc(iord,kmsk,f,fi)
      use atm_cc
      implicit none
      integer,intent(in):: iord
      integer,intent(in):: kmsk(lonr,latd)
      real(kind=kind_real), intent(in):: f(lonr,latd)
      real(kind=kind_real), intent(out):: fi(lonr,latd)
c

      call interpred(iord,kmsk,f,fi,global_lats_r,lonsperlar)

      return
      end
c
c**********************************************************************
c
      subroutine uninterpred_cc(iord,kmsk,fi,f)
      use atm_cc
      implicit none
      integer,intent(in):: iord
      integer,intent(in):: kmsk(lonr,latd)
      real(kind=kind_real), intent(in):: fi(lonr,latd)
      real(kind=kind_real), intent(out):: f(lonr,latd)

      call uninterpred(iord,kmsk,f,fi,global_lats_r,lonsperlar)

      return
      end
c
c**********************************************************************
c
