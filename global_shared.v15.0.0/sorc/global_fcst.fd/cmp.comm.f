      module cmp_comm

      use machine , only : kind_real, kind_integer
      implicit none

! mpi variables
      include 'mpif.h'
 
      integer coupler_id /0/   ! this is coupler's id, used to address
                               ! coupler. this is a default value,
                               ! possibly to be redefined later
!
!     make coupler's id 0 if it is active (i.e. communnicating with
! the component.) otherwise, make it a negative integer; in this case,
! the component is standalone.
!

      integer ibuffer_size
      parameter (ibuffer_size=10)
      integer coupler_rank,my_id,comm_local,
     >component_master_rank_global,process_rank_global,
     >component_master_rank_local,process_rank_local,
     >component_nprocs,flexlev,ibuffer(ibuffer_size)

!     integer kind_real,kind_integer,mpi_kind_real,
!    >kind_alt_real,mpi_kind_alt_real
!     parameter (kind_real=8,kind_integer=4)

      integer mpi_kind_real, mpi_kind_alt_real
!     integer kind_alt_real
!     parameter (kind_alt_real=12-kind_real)
!
!       kind_integer must be number of bytes equal to number of bytes
!     implied by mpi_integer mpi constant; all integers sent/received
!     are of this kind. no value other than 4 is anticipated as of now
!       kind_real is type of real data to communicate. the corresponding
!     mpi data type variable mpi_kind_real is assigned in cmp_init.
!       kind_alt_real is alternative type of real data to communicate. 
!     the corresponding mpi data type variable mpi_kind_alt_real is
!     assigned in cmp_init. (as of 4/21/04, it is not used. it is
!     planned to be used in subroutines cmp_alt_send and cmp_alt_recv,
!     to be written)

      save

      end module cmp_comm
!
!***********************************************************************
!
      subroutine cmp_init(id,flex)
!                         in  in
!
!     this subroutine must be called by every component right upon
!     calling mpi_init. it assigns a value to the component communicator
!     comm_local (which is a global variable in module cmp), to be 
!     thereafter used by the component in place of
!     mpi_comm_world wherever it is used by the component's
!     standalone version. besides, it stores the component's id,
!     the process's ranks, and the "flexibility level" (flex) requested
!     by the component in glob. variables. (the latter parameter affects
!     the mode of communications; for its description, see cmp_send and
!     cmp_recv.) finally, it starts handshaking with coupler, receiving
!     the unique (global, i.e. in mpi_comm_world) coupler process 
!     rank coupler_rank from coupler
                                        ! ibuffer may include additional
                                        ! info to be received
!
      use cmp_comm

      implicit none

      integer id,flex
      integer ierr,color,key,status(mpi_status_size),tag,dummy(2),
     >nprocs_global
!cpl mom4
      integer, dimension(2) :: ibuf

      character*10 s
!

!        determine mpi send/receive types according to prescribed
!        types for arrays to be communicated
!
      if (kind_real == 8) then
        mpi_kind_real     = mpi_real8
        mpi_kind_alt_real = mpi_real4
      else if (kind_real == 4) then
        mpi_kind_real     = mpi_real4
        mpi_kind_alt_real = mpi_real8
      else
        write(s,'(i0)') kind_real
        call glob_abort(1,
     >  'cmp_init: illegal value of kind_real='//s,1)
      end if
      if (kind_integer .ne. 4) then
        write(s,'(i0)') kind_integer
        call glob_abort(1,
     >  'cmp_init: illegal value of kind_integer='//s,1)
      end if

!        store the component's id
!
      my_id = id

!        store the component's "flexibility level"
!
      flexlev = flex

!        assign a value to the component communicator
!        comm_local, to be thereafter used by the component in place of
!        mpi_comm_world wherever it is used by the component's
!        standalone version
!
      color = id
      key   = 1
!           print*,'cmp_init: to call mpi_comm_split, color=',color
      call mpi_comm_split(mpi_comm_world,color,key,comm_local,ierr)
      call glob_abort(ierr,'cmp_init: error in mpi_comm_split',1)

!        store the process's global and local ranks
!
!           print*,'cmp_init: to call mpi_comm_rank for global rank'
      call mpi_comm_rank(mpi_comm_world,process_rank_global,ierr)
      call glob_abort(ierr,
     >'cmp_init: error in mpi_comm_rank(mpi_comm_world...)',1)
!           print*,'cmp_init: to call mpi_comm_rank for local rank'
      call mpi_comm_rank(comm_local,process_rank_local,ierr)
      call glob_abort(ierr,
     >'cmp_init: error in mpi_comm_rank(comm_local...)',1)

!        store component_nprocs - component's number of processes;
!        calculate global number number of processes;
!        determine whether it is standalone mode and if it is, make
!        coupler's id negative and return
!
      call mpi_comm_size(comm_local,component_nprocs,ierr)
      call mpi_comm_size(mpi_comm_world,nprocs_global,ierr)
      if (component_nprocs.eq.nprocs_global) then
        if(process_rank_local.eq.0) print*,'cmp_init: standalone mode'
        coupler_id=-1
        return
      end if

!        start handshaking with coupler (all processes):
!        receive the unique (global, i.e. in mpi_comm_world) coupler 
!        process rank coupler_rank from coupler
!
!     tag=coupler_id+23456
      tag=1
!           print*,'cmp_init: to call mpi_recv'
      call mpi_recv(ibuf,size(ibuf),mpi_integer,mpi_any_source,tag,
     >mpi_comm_world,status,ierr)
      call glob_abort(ierr,'cmp_init: error in mpi_recv',1)
      coupler_rank=ibuf(2)
      if (ibuf(1).ne.coupler_id) then
        print*,'cmp_init: stopped, rcvd ibuffer(1) value is not c id: ',
     >  ibuffer(1)
        call mpi_abort(mpi_comm_world,2,ierr)
      end if

!        inform coupler that this components exists and is active
!
      ibuf(1)=id
      ibuf(2)=process_rank_global
      call mpi_gather(ibuf,2,mpi_integer,dummy,2,mpi_integer,
     >coupler_rank,mpi_comm_world,ierr)

!
      print*,
     >'cmp_init: ranks: process local, global, coupler; coupler_id: ',
     >process_rank_local,process_rank_global,coupler_rank,coupler_id

      return
      end
!
!***********************************************************************
!
      subroutine cmp_intro(master_rank_local)
!                                in
!       this routine must be called by all component's processes
!       which must all know the local rank of component's master
!       process (master_rank_local)
!          alternatively, subroutine cmp_intro_m can be called
!      from component's master process only, and subroutine cmp_intro_s
!      from all other processes. in this case, the local rank of
!      component's master process will be determined and broadcast
!      automatically

      use cmp_comm

      implicit none
 
      integer master_rank_local,ierr,ibuf(2),tag
!

!     print*,'cmp_intro: entered ',master_rank_local,process_rank_local
!    >,coupler_rank

      component_master_rank_local=master_rank_local

      if (coupler_id.lt.0) return    !   <- standalone mode

!        if this process is the component's master process,
!        complete handshaking with coupler:
!        "register", i.e. send component master process global rank 
!        to coupler. also, send the requested "flexibility level".
!        (sending component's id (in ibuf(1)) is for double-check only.)
!
      if (process_rank_local.eq.master_rank_local) then
        component_master_rank_global=process_rank_global
        ibuf(1)=my_id  ! redundant, sent for control only
        ibuf(2)=process_rank_global
!       ibuf(3)=flexlev
        tag=my_id-1
            print*,'cmp_intro: to call mpi_send ',process_rank_local,
     >      process_rank_global,'tag=',tag,'coupler=',coupler_rank,
     >      'ibuf=',ibuf(1),ibuf(2)
        call mpi_send(ibuf,2,mpi_integer,coupler_rank,tag,
     >  mpi_comm_world,ierr)
        if (ierr.ne.0) then
          print*,'cmp_intro: error in mpi_send, process ',
     >    process_rank_global
          call mpi_abort(mpi_comm_world,2,ierr)
        end if
      end if
            print*,'cmp_intro: returning ',process_rank_local,
     >      process_rank_global,coupler_rank
      return
      end
!
!***********************************************************************
!
      subroutine cmp_intro_m
!
!      this routine must be called by component's master process (only),
!      if cmp_intro is not called (see comments in cmp_intro)

      use cmp_comm

      implicit none
 
      integer ierr,ibuf(3),tag,i
!

!     print*,'cmp_intro_m: entered, process_rank_local=',
!    >process_rank_local

      component_master_rank_local=process_rank_local
      component_master_rank_global=process_rank_global

      tag=abs(my_id)+12345
      do i=0,component_nprocs-1
        if (i.ne.component_master_rank_local) then
          ibuf(1)=component_master_rank_local
          ibuf(2)=component_master_rank_global
          call mpi_send(ibuf,2,mpi_integer,i,tag,comm_local,ierr)
          if (ierr.ne.0) then
            print*,'cmp_intro_m: error in 1st mpi_send, i=',i
            call mpi_abort(mpi_comm_world,2,ierr)
          end if
        end if
      end do

      if (coupler_id.lt.0) return    !   <- standalone mode

!        complete handshaking with coupler:
!        "register", i.e. send component master process global rank 
!        to coupler. also, send the requested "flexibility level".
!        (sending component's id (in ibuf(1)) is for double-check only.)
!
      tag=my_id+54321
      ibuf(1)=my_id  ! redundant, sent for control only
      ibuf(2)=process_rank_global
      ibuf(3)=flexlev
!         print*,'cmp_intro_m: to call mpi_send ',process_rank_local,
!    >    process_rank_global
      call mpi_send(ibuf,3,mpi_integer,coupler_rank,tag,
     >mpi_comm_world,ierr)
      if (ierr.ne.0) then
        print*,'cmp_intro_m: error in mpi_send, process ',
     >  process_rank_global
        call mpi_abort(mpi_comm_world,2,ierr)
      end if
!         print*,'cmp_intro_m: returning ',process_rank_local,
!    >    process_rank_global
      return
      end
!
!***********************************************************************
!
      subroutine cmp_intro_s
!
!      this routine must be called by all component's processes other
!      than master process,
!      if cmp_intro is not called (see comments in cmp_intro)

      use cmp_comm

      implicit none
 
      integer ierr,ibuf(3),tag,status(mpi_status_size)
!

!     print*,'cmp_intro_s: entered, process_rank_local=',
!    >process_rank_local

      tag=abs(my_id)+12345
      call mpi_recv(ibuf,2,mpi_integer,mpi_any_source,tag,
     >comm_local,status,ierr)
      if (ierr.ne.0) then
        print*,'cmp_intro_s: error in mpi_recv ',process_rank_local
        call mpi_abort(mpi_comm_world,2,ierr)
      end if
      component_master_rank_local=ibuf(1)
      component_master_rank_global=ibuf(2)
c wtf?      do i=0,component_nprocs-1
c wtf?        if (i.ne.component_master_rank_local) then
c wtf?          ibuf(1)=component_master_rank_local
c wtf?          ibuf(2)=component_master_rank_global
c wtf?          call mpi_send(ibuf,2,mpi_integer,i,tag,comm_local,ierr)
c wtf?        end if
c wtf?      end do

!         print*,'cmp_intro_s: returning ',process_rank_local,
!    >    process_rank_global,component_master_rank_local,
!    >    component_master_rank_global
      return
      end
!
!***********************************************************************
!
      subroutine cmp_send(f,n)
!
      use cmp_comm

      implicit none
 
      integer n,ierr,tag
      real(kind=kind_real) f(n)
!
      if (coupler_id.lt.0) return    !   <- standalone mode

!           call cmp_dbg_cr(6,'cmp_send: entered')

      if (process_rank_local.ne.component_master_rank_local) then
        if (flexlev.eq.0) then
!         with "flexibility level" flexlev=0, only component master 
!         process is supposed to call this subroutine.
          print '("*** cmp_send: process_rank_local=",i4,"  ***"/
     >    "*** and component_master_rank_local=",i4," differ:  ***"/
     >    "*** stopped ***")',
     >    process_rank_local,component_master_rank_local
          call mpi_abort(mpi_comm_world,2,ierr)
        else if (flexlev.eq.1) then
!         with "flexibility level" flexlev=1, any component process is 
!         allowed to call this subroutine but only the component
!         master process can actually send data (so the
!         others just make a dummy call), as the coupler process only 
!         receives data from the component master process.
          return
        else if (flexlev.ne.2 .and. flexlev.ne.3) then
          print '("*** cmp_send: illegal value of flexlev",i9/
     >    "*** stopped")',flexlev
          call mpi_abort(mpi_comm_world,2,ierr)
        end if
!         with "flexibility level" flexlev=2 or flexlev=3, any 
!         component process is allowed to actually send data.
!         [in this case, the coupler process (in cpl_recv) receives 
!         from mpi_any_source rather than component_master_rank_global,
!         and it is only identification by  tag  which enables coupler
!         to receive the data from the right source.]
!         but in any case only one component process may actually be
!         engaged in a particular exchange of data with coupler.
      end if

      tag=my_id-1

      call mpi_send(f,n,mpi_kind_real,coupler_rank,tag,
     >mpi_comm_world,ierr)
      call glob_abort(ierr,'cmp_send: error in mpi_send',1)

!           call cmp_dbg_cr(6,'cmp_send: exiting')
      return
      end
!
!***********************************************************************
!
      subroutine cmp_integer_send(f,n)
!
      use cmp_comm

      implicit none
 
      integer n,ierr,tag
      integer f(n)
!
      if (coupler_id.lt.0) return    !   <- standalone mode

!           print*,'cmp_integer_send: entered with n=',n,' f=',f,
!    >      '; my_id=',my_id,'coupler_rank=',coupler_rank

      if (process_rank_local.ne.component_master_rank_local) then
        if (flexlev.eq.0) then
!         with "flexibility level" flexlev=0, only component master 
!         process is supposed to call this subroutine.
          print '("*** cmp_send: process_rank_local=",i4,"  ***"/
     >    "*** and component_master_rank_local=",i4," differ:  ***"/
     >    "*** stopped ***")',
     >    process_rank_local,component_master_rank_local
          call mpi_abort(mpi_comm_world,2,ierr)
        else if (flexlev.eq.1) then
!         with "flexibility level" flexlev=1, any component process is 
!         allowed to call this subroutine but only the component
!         master process can actually send data (so the
!         others just make a dummy call), as the coupler process only 
!         receives data from the component master process.
          return
        else if (flexlev.ne.2 .and. flexlev.ne.3) then
          print '("*** cmp_send: illegal value of flexlev",i9/
     >    "*** stopped")',flexlev
          call mpi_abort(mpi_comm_world,2,ierr)
        end if
!         with "flexibility level" flexlev=2 or flexlev=3, any 
!         component process is allowed to actually send data.
!         [in this case, the coupler process (in cpl_recv) receives 
!         from mpi_any_source rather than component_master_rank_global,
!         and it is only identification by  tag  which enables coupler
!         to receive the data from the right source.]
!         but in any case only one component process may actually be
!         engaged in a particular exchange of data with coupler.
      end if

      tag=my_id-1
!           print*,'cmp_integer_send: to call mpi_send; f=',
!    >      f,' n=',n,' coupler_rank=',coupler_rank,' tag=',tag
      call mpi_send(f,n,mpi_integer,coupler_rank,tag,
     >mpi_comm_world,ierr)
      call glob_abort(ierr,'cmp_integer_send: error in mpi_send',1)
            print*,'cmp_integer_send: to return'

      return
      end
!
!***********************************************************************
!
      subroutine cmp_recv(f,n)
!
      use cmp_comm

      implicit none
 
      integer n,ierr,tag,ibuf(3),status(mpi_status_size)
      real(kind=kind_real) f(n)
!
!     print *,' am: in cmp_recv n=',n,' process_rank_local=',
!    &process_rank_local,' component_master_rank_local=',
!    &component_master_rank_local

      if (coupler_id.lt.0) return    !   <- standalone mode

!           call cmp_dbg_cr(6,'cmp_recv: entered')

      if (process_rank_local.ne.component_master_rank_local) then

        if (flexlev.eq.0) then

!         with "flexibility level" flexlev=0, only component master 
!         process is supposed to call this subroutine.

          print '("*** cmp_recv: process_rank_local=",i4,"  ***"/
     >    "*** and component_master_rank_local=",i4," differ:  ***"/
     >    "*** stopped ***")',
     >    process_rank_local,component_master_rank_local
          call mpi_abort(mpi_comm_world,2,ierr)

        else if (flexlev.eq.1 .or. flexlev.eq.2) then

!         with "flexibility level" flexlev=1 or flexlev=2, any 
!         component process is allowed to call this subroutine but 
!         only the component master process is supposed to actually 
!         receive data (so the others just make a dummy call), as
!         the coupler process only sends data to the component master
!         process.

          return

        else if (flexlev.eq.3) then

!         with "flexibility level" flexlev=3, any component process
!         may actually receive data.
!         [in this case, the coupler process (in cpl_send) first
!         receives the component process global rank 
!         (process_rank_global) from this subroutine, the source being
!         mpi_any_source, so it is only identification by  tag  which 
!         enables coupler to receive process_rank_global from the right
!         source. upon the receipt, the coupler process (in cpl_send)
!         sends the data to this component process, rather than to 
!         the component master process as is the case with lower 
!         "flexibility levels".]
!         but in any case only one component process may actually be
!         engaged in a particular exchange of data with coupler.

          ibuf(1)=my_id
          ibuf(2)=process_rank_global
          tag=my_id
          call mpi_send(ibuf,2,mpi_integer,coupler_rank,tag,
     >    mpi_comm_world,ierr)
          if (ierr.ne.0) stop 'cmp_recv: error in mpi_send'

        else

          print '("*** cmp_recv: illegal value of flexlev",i9/
     >    "*** stopped")',flexlev
          call mpi_abort(mpi_comm_world,2,ierr)

        end if

      end if

      tag=my_id-1

      call mpi_recv(f,n,mpi_kind_real,coupler_rank,tag,
     >mpi_comm_world,status,ierr)
      call glob_abort(ierr,'cmp_recv: error in mpi_recv',1)

!           call cmp_dbg_cr(6,'cmp_recv: exiting')

      return
      end
!
!***********************************************************************
!
      subroutine cmp_announce(nunit,s)
!
      use cmp_comm

      implicit none

      character*(*) s
 
      integer nunit,ierr
!

      if (process_rank_local.eq.component_master_rank_local) then
        write(nunit,*) trim(s)
      else if (flexlev.eq.0) then

!         with "flexibility level" flexlev=0, only component master 
!         process is supposed to call this subroutine.

          print '("*** cmp_announce: process_rank_local=",i4,"  ***"/
     >    "*** and component_master_rank_local=",i4," differ:  ***"/
     >    "*** stopped ***")',
     >    process_rank_local,component_master_rank_local
          call mpi_abort(mpi_comm_world,2,ierr)

      end if

      return
      end
!
!***********************************************************************
!
      subroutine cmp_stdout(s)
!
!     use cmp_comm, only: coupler_id,process_rank_global
        ! <- these values may not have the right value by this moment,
        ! as this routine may be called before cmp_init  - 02/23/05

      implicit none

      character*(*) s
      integer ios
      character*4 mess
c

! -> for debugging:
      open(12345,
     >file='/nfsuser/g01/wx20ds/c/cmp.stdout',
     >form='formatted',status='old',iostat=ios)
      if (ios.eq.0) then
        read(12345,*) mess
        if (mess.eq.'mess') then
!         print*,'cmp_stdout: unit 6 left alone, process ',
!    >    process_rank_global
        ! <- process_rank_global may be undefined by this moment, as
        !    this routine may be called before cmp_init  - 02/23/05
          return
        end if
        close(12345)
      end if
! <- for debugging

!     if (coupler_id.lt.0) return    ! nothing is to occur if there is
                                     ! no communication with coupler,
                                     ! i.e. if component is standalone
        ! <- coupler_id may not have the right value by this moment,
        ! as this routine may be called before cmp_init  - 02/23/05

      if (len_trim(s).eq.0) return

      close(6)
      
      open(6,file=trim(s),form='formatted',status='unknown')

      print*,'cmp_stdout: unit 6 closed, reopened as '//trim(s)

      return
      end
!
!***********************************************************************
!
      subroutine cmp_dbg_cr(nunit,s)
!
!       debugging routine: mainly, prints coupler_rank
!
      use cmp_comm

      implicit none

      character*(*) s
      integer nunit

      integer ncall/0/,ncallmax/5000/
      save
!

      if (s(5:6).eq.'m:') then
        if (process_rank_local .ne. component_master_rank_local) return
      end if

      if (ncall.ge.ncallmax) return
      ncall=ncall+1

      write(nunit,*) process_rank_global,ncall,s//' coupler_rank=',
     >coupler_rank

! the following assumes that coupler_rank must be =0, comment out if
! this is not the case
      call glob_abort(coupler_rank,
     >'cmp_dbg_cr: coupler_rank!=0, aborting',1)

      return
      end
