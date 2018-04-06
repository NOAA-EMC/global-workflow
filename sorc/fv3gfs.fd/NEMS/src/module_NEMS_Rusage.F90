module module_NEMS_Rusage
  ! --------------------------------------------------------------------
  ! Resource usage monitoring tools
  ! --------------------------------------------------------------------
  ! Calculates resource usage differences between two calls.
  ! Writes out a report in nemsusage.xml
  !
  ! HISTORY:
  !  2016-11 - Trahan - creator
  ! --------------------------------------------------------------------

  ! Compiler note: iso_c_binding is an intrinsic fortran module.  If
  ! your compiler does not understand "intrinsic" or cannot find the
  ! "iso_c_binding" module, then that is a bug in your compiler.
  use, intrinsic :: iso_c_binding ! has to be here due to type definitions
  implicit none
  private

  public :: NEMS_Rusage
  public :: NEMS_Rusage_Start
  public :: NEMS_Rusage_Stop
  public :: NEMS_Rusage_Report
  public :: NEMS_Rusage_Is_Valid

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Compiler note: NEMS_Rusage is a Fortran class with member
  ! functions.  If your compiler gives an error in the procedure
  ! declarations or the class(NEMS_Rusage) declarations later in this
  ! module, then that is a bug in your compiler's Fortran 2003 support.
  type NEMS_Rusage
     private
     ! Note that all member variables have names that begin with m_
     ! and are private.  All member procedures are public.
     logical :: m_valid=.false. ! .false. means do not use this object
     integer(kind=c_int64_t) :: m_start_sec=-1 ! NEMS start in seconds since reference time
     integer(kind=c_int64_t) :: m_start_nsec=-1 ! NEMS start nanosecond portion
     integer(kind=c_int64_t) :: m_end_sec=-1 ! NEMS end in seconds since reference time
     integer(kind=c_int64_t) :: m_end_nsec=-1 ! NEMS end nanosecond portion
     integer :: m_comm_world=-1 ! Global communicator for NEMS
     integer :: m_comm_name=-1 ! Per-host communicator
     integer :: m_comm_hosts=-1 ! Communicator used by ranks 0 of each comm_name group
     integer :: m_comm_size_world=-1 ! Size of comm_world
     integer :: m_comm_size_hosts=-1 ! Size of comm_hosts, valid iff m_nodemaseter
     integer :: m_comm_size_name=-1 ! Size of comm_name
     integer :: m_rank_world=-1 ! Rank in comm_world
     logical :: m_master=.false. ! Am I rank 0 on comm_world?
     logical :: m_nodemaster=.false. ! Am I rank 0 on comm_name?
     character(len=:), pointer :: m_procname=>NULL()
   contains
     procedure, public :: start => NEMS_Rusage_Start
     procedure, public :: stop => NEMS_Rusage_Stop
     procedure, public :: report => NEMS_Rusage_Report
     procedure, public :: is_valid => NEMS_Rusage_Is_Valid
  end type NEMS_Rusage

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  interface
     function nems_c_crc32(buffer,length,error) bind(c)
       use iso_c_binding
       implicit none
       integer(kind=c_int32_t), value :: length
       integer(kind=c_int32_t) :: error
       character(kind=c_char) :: buffer(*)
       integer(kind=c_int32_t) :: nems_c_crc32
     end function nems_c_crc32
  end interface

  interface
     subroutine nems_c_timer(sec,nsec,error) bind(c)
       use iso_c_binding
       implicit none
       integer(kind=c_int64_t) :: sec   ! seconds since reference time
       integer(kind=c_int64_t) :: nsec  ! nanosecond portion
       integer(kind=c_int32_t) :: error ! 0 on success
     end subroutine nems_c_timer
  end interface

  interface
     subroutine nems_c_usage(utime,stime,maxrss,inblock, &
                             outblock,error)                bind(c)
       use iso_c_binding
       implicit none
       integer(kind=c_int64_t) :: maxrss,inblock,outblock
       real(kind=c_double) :: utime,stime
       integer(kind=c_int32_t) :: error ! 0 on success
     end subroutine nems_c_usage
  end interface


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
contains
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  logical function NEMS_Rusage_Is_Valid(ru)
    implicit none
    class(NEMS_Rusage), intent(inout) :: ru
    NEMS_Rusage_Is_Valid=ru%m_valid
  end function NEMS_Rusage_Is_Valid

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine NEMS_Rusage_Report(ru,ierr,unit)
    implicit none
    class(NEMS_Rusage), intent(inout) :: ru
    integer, intent(inout) :: Ierr
    integer, optional, intent(in) :: unit

    integer :: xmlunit
    integer(kind=c_int64_t) :: secdiff,nsecdiff
    double precision :: timediff
    double precision, parameter :: nano=1e-9

    real(kind=c_double) :: utime,stime
    integer(kind=c_Int64_t) :: maxrss,inblock,outblock

    double precision :: dmaxrss, dinblock, doutblock

    ierr=-999

    if(.not.ru%m_valid) then
       ierr=10
       return
    endif

    call nems_c_usage(utime,stime,maxrss,inblock,outblock,ierr)
    if(ierr/=0) return

    if(present(unit)) then
       xmlunit=unit
       open(file='nemsusage.xml',unit=unit,form='formatted')
    else
       open(file='nemsusage.xml',newunit=xmlunit,form='formatted')
    endif

    dmaxrss=dble(maxrss)/1024
    dinblock=dble(inblock)
    doutblock=dble(outblock)

    secdiff=ru%m_end_sec-ru%m_start_sec
    nsecdiff=ru%m_end_nsec-ru%m_start_nsec
    if(nsecdiff<0) then
       nsecdiff=1000000000-nsecdiff
       secdiff=secdiff-1
    endif
    timediff=dble(secdiff) + nano*dble(nsecdiff)
    if(ru%m_master) then
       write(xmlunit,'(A)') '<?xml version="1.0"?>'
       write(xmlunit,'(A)') '<nemsrusage>'
    endif
    call report_world_max  (ru,'walltime','sec',timediff,xmlunit,ierr,&
                            'total runtime of NEMS, excluding MPI setup time')
    call report_world_range(ru,'systime', 'percent',100*stime/(stime+utime),xmlunit,ierr,&
                            'maximum system time used on any one rank')
    call report_world_range(ru,'usertime','percent',100*utime/(stime+utime),xmlunit,ierr,&
                            'percent of time in user space any one rank')
    call report_by_rank(ru,'memory','MiB',dmaxrss,xmlunit,ierr,&
                        'maximum resident set size')
    call report_nodesum_range(ru,'memory','MiB',dmaxrss,xmlunit,ierr,&
                             'maximum resident set size')
    call report_nodesum_range(ru,'blocksread','blocks',dinblock,xmlunit,ierr,&
                              'number of blocks read')
    call report_nodesum_range(ru,'blockswritten','blocks',doutblock,xmlunit,ierr,&
                              'number of blocks written')
    if(ru%m_master) then
       write(xmlunit,'(A)') '</nemsrusage>'
    endif
  end subroutine NEMS_Rusage_Report

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine report_world_max(ru,what,units,localstat,xmlunit,ierr,descr)
    use mpi
    implicit none
    class(NEMS_Rusage), intent(inout) :: ru
    integer, intent(inout) :: Ierr
    integer, intent(in) :: xmlunit
    character(len=*), intent(in) :: what, units,descr
    double precision, intent(in) :: localstat
    double precision :: maxstat
    double precision :: sendbuf
    sendbuf=localstat
    call MPI_Allreduce(sendbuf,maxstat,1,MPI_DOUBLE_PRECISION,MPI_MAX,&
                       ru%m_comm_world,ierr)
    if(ru%m_master) then
       write(xmlunit,20) trim(what)
       write(xmlunit,30) trim(units)
       write(xmlunit,31) trim(descr)
       write(xmlunit,10) 'max',maxstat
       write(xmlunit,21) trim(what)
    endif
20  format('  <',A,'>')
21  format('  </',A,'>')
30  format('    <units>',A,'</units>')
31  format('    <descr>',A,'</descr>')
10  format('    <stat type="',A,'" by="rank">',F0.9,'</stat>')
  end subroutine report_world_max

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine report_world_range(ru,what,units,localstat,xmlunit,ierr,descr)
    use mpi
    implicit none
    class(NEMS_Rusage), intent(inout) :: ru
    integer, intent(inout) :: Ierr
    integer, intent(in) :: xmlunit
    character(len=*), intent(in) :: what, units,descr
    double precision, intent(in) :: localstat
    double precision :: sumstat,maxstat,minstat
    double precision :: sendbuf
    sendbuf=localstat
    call MPI_Allreduce(sendbuf,sumstat,1,MPI_DOUBLE_PRECISION,MPI_SUM,&
                       ru%m_comm_world,ierr)
    call MPI_Allreduce(sendbuf,minstat,1,MPI_DOUBLE_PRECISION,MPI_MIN,&
                       ru%m_comm_world,ierr)
    call MPI_Allreduce(sendbuf,maxstat,1,MPI_DOUBLE_PRECISION,MPI_MAX,&
                       ru%m_comm_world,ierr)
    if(ru%m_master) then
       write(xmlunit,20) trim(what)
       write(xmlunit,30) trim(units)
       write(xmlunit,31) trim(descr)
       write(xmlunit,10) 'min',minstat
       write(xmlunit,10) 'max',maxstat
       write(xmlunit,10) 'avg',sumstat/ru%m_comm_size_world
       write(xmlunit,21) trim(what)
    endif
20  format('  <',A,'>')
21  format('  </',A,'>')
30  format('    <units>',A,'</units>')
31  format('    <descr>',A,'</descr>')
10  format('    <stat type="',A,'" by="rank">',F0.9,'</stat>')
  end subroutine report_world_range

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine report_by_rank(ru,what,units,localstat,xmlunit,ierr,descr)
    use iso_c_binding
    use mpi
    implicit none
    class(NEMS_Rusage), intent(inout) :: ru
    integer, intent(inout) :: ierr
    integer, intent(in) :: xmlunit
    character(len=*), intent(in) :: what, units,descr
    double precision, intent(in) :: localstat

    ! Locals:
    integer :: localrank ! mpi_comm_world rank of local rank
    integer :: namesize ! length of processor's name
    integer :: size ! sizes required for temporary buffer
    integer :: ihost ! host rank within comm_hosts
    integer :: arank ! index of rank within allranks
    integer :: irank ! index of rank within one host
    integer :: i ! contains proof that P=NP
    double precision, allocatable :: hoststat(:) ! stat for all ranks in a node
    double precision, allocatable :: allstats(:) ! stat for all ranks
    integer, allocatable :: hostranks(:) ! ranks in world order for one host
    integer, allocatable :: allranks(:) ! ranks sorted by host, then world order
    integer, allocatable :: namesizes(:) ! size of names of each host
    integer, allocatable :: namedispl(:) ! mpi gatherv displacements for names
    integer, allocatable :: statsizes(:) ! number of ranks per host
    integer, allocatable :: statdispl(:) ! mpi gatherv displacements for ranks
    character(len=:), allocatable :: allnames ! all host names concatinated
    
    if(ru%m_nodemaster) then
       allocate(hoststat(ru%m_comm_size_name))
       allocate(hostranks(ru%m_comm_size_name))
    else
       allocate(hoststat(1))
       allocate(hostranks(1))
    endif
    if(ru%m_master) then
       allocate(allstats(ru%m_comm_size_world))
       allocate(allranks(ru%m_comm_size_world))
    else
       allocate(allstats(1))
       allocate(allranks(1))
    endif

    ! Gather ranks and stats within one node:
    call MPI_Gather(localstat,1,MPI_DOUBLE_PRECISION, &
                    hoststat,1,MPI_DOUBLE_PRECISION, &
                    0,ru%m_comm_name,ierr)
       if(ierr/=0) goto 1000
    call MPI_Gather(ru%m_rank_world,1,MPI_INTEGER, &
                    hostranks,1,MPI_INTEGER, &
                    0,ru%m_comm_name,ierr)
       if(ierr/=0) goto 1000

    ! Gather names across nodes:
    if(ru%m_nodemaster) then
       allocate(namesizes(ru%m_comm_size_hosts))
       allocate(namedispl(ru%m_comm_size_hosts+1))
       namesize=len(ru%m_procname)
       call MPI_Allgather(namesize,1,MPI_INTEGER,&
                          namesizes,1,MPI_INTEGER,&
                          ru%m_comm_hosts,ierr)
       if(ierr/=0) goto 1000
       namedispl(1)=0
       do i=1,ru%m_comm_size_hosts
          namedispl(i+1)=namedispl(i)+namesizes(i)
       enddo
       flush(6)
       if(ru%m_master) then
          size=namedispl(ru%m_comm_size_hosts+1)
          allocate(character(len=size) :: allnames)
       else
          allocate(character(len=1) :: allnames)
       endif
       call MPI_Gatherv(ru%m_procname,namesize,MPI_CHARACTER,&
                        allnames,namesizes,namedispl,MPI_CHARACTER,&
                        0,ru%m_comm_hosts,ierr)
       if(ierr/=0) goto 1000
    endif

    ! Gather ranks and stats sorted by nodes:
    if(ru%m_nodemaster) then
       allocate(statsizes(ru%m_comm_size_hosts))
       allocate(statdispl(ru%m_comm_size_hosts+1))
       call MPI_Allgather(ru%m_comm_size_name,1,MPI_INTEGER,&
                          statsizes,1,MPI_INTEGER,&
                          ru%m_comm_hosts,ierr)
       if(ierr/=0) goto 1000
       statdispl(1)=0
       do i=1,ru%m_comm_size_hosts
          statdispl(i+1)=statdispl(i)+statsizes(i)
       enddo
       call MPI_Gatherv(hoststat,ru%m_comm_size_name,MPI_DOUBLE_PRECISION,&
                        allstats,statsizes,statdispl,MPI_DOUBLE_PRECISION,&
                        0,ru%m_comm_hosts,ierr)
       call MPI_Gatherv(hostranks,ru%m_comm_size_name,MPI_INTEGER,&
                        allranks,statsizes,statdispl,MPI_INTEGER,&
                        0,ru%m_comm_hosts,ierr)
       if(ierr/=0) goto 1000
    endif

    ! Master writes data:
    if(ru%m_master) then
10     format('  <',A,'>')
       write(xmlunit,10) trim(what)
20     format('    <units>',A,'</units>')
       write(xmlunit,20) trim(units)
30     format('    <descr>',A,'</descr>')
       write(xmlunit,30) trim(descr)
40     format('    <values>')
       write(xmlunit,40)
       arank=0
       do ihost=1,ru%m_comm_size_hosts
50        format('      <node name="',A,'">')
          write(xmlunit,50) trim(allnames(namedispl(ihost)+1:namedispl(ihost+1)))
          do irank=1,statsizes(ihost)
             arank=arank+1
60           format('        <rank in_world="',I0,'">',F0.9,'</rank>')
             write(xmlunit,60) allranks(arank),allstats(arank)
          enddo
70        format('      </node>')
          write(xmlunit,70)
       enddo
80     format('    </values>')
       write(xmlunit,80)
90     format('  </',A,'>')
       write(xmlunit,90) trim(what)
    endif

    1000 continue ! cleanup block

    ! These deallocates are not needed in standard-conforming fortran,
    ! but are placed here to work around compiler bugs:
    if(allocated(hoststat)) deallocate(hoststat)
    if(allocated(hostranks)) deallocate(hostranks)
    if(allocated(allstats)) deallocate(allstats)
    if(allocated(allranks)) deallocate(allranks)
    if(allocated(allnames)) deallocate(allnames)
    if(allocated(statsizes)) deallocate(statsizes)
    if(allocated(statdispl)) deallocate(statdispl)
  end subroutine report_by_rank

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine report_nodesum_range(ru,what,units,localstat,xmlunit,ierr,descr)
    use mpi
    implicit none
    class(NEMS_Rusage), intent(inout) :: ru
    integer, intent(inout) :: Ierr
    integer, intent(in) :: xmlunit
    character(len=*), intent(in) :: what, units,descr
    double precision, intent(in) :: localstat
    double precision :: sumstat,maxstat,minstat,avgstat
    double precision :: sendbuf, nodesum, sendbuf3(3), recvbuf3(3)
    character*100 message

    ! Sum within each node:
    sendbuf=localstat
    call MPI_Reduce(sendbuf,nodesum,1,MPI_DOUBLE_PRECISION,MPI_SUM,&
                    0,ru%m_comm_name,ierr)

    if(ru%m_nodemaster) then
       ! Get stats across all nodes using rank 0 of each node:
       sendbuf=nodesum
       call MPI_Allreduce(sendbuf,sumstat,1,MPI_DOUBLE_PRECISION,MPI_SUM,&
                          ru%m_comm_hosts,ierr)
       call MPI_Allreduce(sendbuf,minstat,1,MPI_DOUBLE_PRECISION,MPI_MIN,&
                          ru%m_comm_hosts,ierr)
       call MPI_Allreduce(sendbuf,maxstat,1,MPI_DOUBLE_PRECISION,MPI_MAX,&
                          ru%m_comm_hosts,ierr)

       ! Prepare the sendbuf for broadcasting to the rest of the
       ! ranks.  Note that we divide by the number of hosts here since
       ! the other ranks do not know the number of hosts.
       avgstat=sumstat/ru%m_comm_size_hosts
       sendbuf3=(/ avgstat, minstat, maxstat /)
    endif

    ! Broadcast results from rank 0 of each node to the rest of the
    ! node's ranks:

    call MPI_Bcast(sendbuf3,3,MPI_DOUBLE_PRECISION,0,&
                   ru%m_comm_name,ierr)
    avgstat=sendbuf3(1)
    minstat=sendbuf3(2)
    maxstat=sendbuf3(3)

    if(ru%m_master) then
       write(xmlunit,20) trim(what)
       write(xmlunit,30) trim(units)
       write(xmlunit,31) trim(descr)
       write(xmlunit,10) 'min',minstat
       write(xmlunit,10) 'max',maxstat
       write(xmlunit,10) 'avg',avgstat
       write(xmlunit,21) trim(what)
    endif
20  format('  <',A,'>')
21  format('  </',A,'>')
30  format('    <units>',A,'</units>')
31  format('    <descr>',A,'</descr>')
10  format('    <stat type="',A,'" by="nodesum">',F0.9,'</stat>')
  end subroutine report_nodesum_range

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine NEMS_Rusage_Stop(ru,ierr)
    implicit none
    class(NEMS_Rusage), intent(inout) :: ru
    integer, intent(inout) :: ierr
    ierr=-999
    if(.not.ru%m_valid) then
       ierr=10
       return
    endif
    call rusage_time(ru,ru%m_end_sec,ru%m_end_nsec,ierr)
  end subroutine NEMS_Rusage_Stop

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine NEMS_Rusage_Start(ru,comm_world,procname,procname_len,ierr)
    use mpi
    implicit none
    class(NEMS_Rusage), intent(inout) :: ru
    integer, intent(in) :: comm_world ! Global communicator for NEMS
    integer, intent(in) :: procname_len
    integer, intent(inout) :: ierr ! 0 = success
    character(len=procname_len), intent(in) :: procname

    if(ru%m_valid .and. associated(ru%m_procname)) then
       deallocate(ru%m_procname)
       nullify(ru%m_procname)
    endif

    ru%m_valid=.true.

    ! Compiler note: the below line is the correct syntax for
    ! allocating a string.  If your compiler gives an error here, then
    ! that is a bug in your compiler.
    allocate(character(len=procname_len) :: ru%m_procname)
    ru%m_procname=procname

    call init_rusage_comms(ru,comm_world,procname,procname_len,ierr)
    if(ierr/=0) then
       deallocate(ru%m_procname)
       nullify(ru%m_procname)
       ru%m_valid=.false.
       return
    endif

    call rusage_time(ru,ru%m_start_sec,ru%m_start_nsec,ierr)
    if(ierr/=0) then
       deallocate(ru%m_procname)
       nullify(ru%m_procname)
       ru%m_valid=.false.
       return
    endif
  end subroutine NEMS_Rusage_Start

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine rusage_time(ru,sec,nsec,ierr)
    use iso_c_binding
    use mpi
    implicit none
    class(NEMS_Rusage), intent(inout) :: ru
    integer(kind=c_int64_t),intent(inout) :: sec,nsec
    integer, intent(inout) :: ierr
    ierr=-999
    call MPI_Barrier(ru%m_comm_world,ierr)
    if(ierr/=0) return
    call nems_c_timer(sec,nsec,ierr)
  end subroutine rusage_time

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine init_rusage_comms(ru,comm_world,procname,procname_len,ierr)
    use mpi
    implicit none
    class(NEMS_Rusage), intent(inout) :: ru
    integer, intent(in) :: comm_world ! Global communicator for NEMS
    integer, intent(in) :: procname_len
    integer, intent(inout) :: ierr ! 0 = success
    character(len=procname_len), intent(in) :: procname
    logical :: match
    integer :: rank

    ru%m_comm_world=comm_world

    ierr=-999

    call color_by_hash(procname(1:procname_len),procname_len,comm_world,ru%m_comm_name,ru%m_comm_hosts,ru%m_rank_world,ierr)
    if(ierr/=0) then
       ierr=1
    endif

    call MPI_Comm_size(ru%m_comm_name,ru%m_comm_size_name,ierr)
    call MPI_Comm_size(ru%m_comm_hosts,ru%m_comm_size_hosts,ierr)
    call MPI_Comm_size(ru%m_comm_world,ru%m_comm_size_world,ierr)
    call MPI_Comm_rank(ru%m_comm_world,ru%m_rank_world,ierr)
    ru%m_master = ru%m_rank_world==0
    call MPI_Comm_rank(ru%m_comm_name,rank,ierr)
    ru%m_nodemaster = rank==0

    call check_names(procname(1:procname_len),ru%m_comm_name,ru%m_comm_hosts,match,ierr)
    if(ierr/=0) then
       ierr=2
    endif
    
    if(.not.match) then
       ierr=3
       return
    endif
  end subroutine init_rusage_comms

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine check_names(name,comm_name,comm_hosts,match,ierr)
    !! Checks to see if all names match within each comm_names group.
    !! Sets match=.true. if all names match, or .false. if any groups
    !! have more than one name.
    use mpi
    implicit none
    integer, intent(inout) :: ierr
    character(len=*) :: name
    integer, intent(in) :: comm_name,comm_hosts
    logical, intent(out) :: match
    integer :: rank
    integer :: error
    integer :: local_match, name_match, all_names_match
    character(len=MPI_MAX_PROCESSOR_NAME) :: sendbuf

    ierr=-999
    match=.false.

    call MPI_Comm_rank(comm_name,rank,error)
    if(error/=0) then
       ierr=1
       return
    endif

    sendbuf=name
    call MPI_Bcast(sendbuf,MPI_MAX_PROCESSOR_NAME,MPI_CHARACTER, &
         0,comm_name,error)
    if(error/=0) then
       ierr=2
       return
    endif

    ! The local_match is 0 if this rank thinks the names match.
    local_match=1
    if(trim(name)==trim(sendbuf)) then
       local_match=0
    else
       !call msg('MISMATCH: "'//trim(name)//'" /= "'//trim(sendbuf)//'"')
    end if

    !call MPI_Allreduce(local_match,all_names_match,1,MPI_INTEGER,MPI_MAX,&
    !                MPI_COMM_WORLD,error)

    ! Now all ranks for this host gather the local_match value into
    ! name_match:
    call MPI_Allreduce(local_match,name_match,1,MPI_INTEGER,MPI_MAX,&
                       comm_name,error)
    if(error/=0) then
       ierr=3
       return
    endif

    ! Master ranks of each name gather that information for all ranks
    ! into all_names_match, which will be 1 if any name mismatched on
    ! any rank.
    if(rank==0) then
       call MPI_Allreduce(name_match,all_names_match,1,MPI_INTEGER,MPI_MAX,&
                          comm_hosts,error)
       if(error/=0) then
          ierr=4
          return
       endif
    endif

    ! Broadcast all_names_match across all ranks in this name:
    call MPI_Bcast(all_names_match,1,MPI_INTEGER,0,comm_name,ierr)
    if(error/=0) then
       ierr=5
       return
    endif

    match = all_names_match==0
    ierr=0
  end subroutine check_names

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine color_by_hash(name,namelen,commin,comm_name,comm_hosts,rank_world,ierr)
    !! Groups processors into ranks by name.  
    !!
    !! Each group in comm_name will have the same name in each rank.
    !! The first rank of each communicator will be in the same group
    !! of comm_hosts.  This is done using a CRC-based hash function
    !! and MPI_Comm_split for better scalability.  The hash function
    !! is in freebsd_crc32.c and is the CRC algorithm used by the
    !! freebsd kernel.  Hash collisions are detected via check_names.
    !! If one is detected, some "salt" is added to the name to change
    !! the hash value, and the process is repeated until success (or
    !! 26 failures).
    use mpi
    use iso_c_binding
    implicit none
    character(len=namelen), intent(in) :: name
    integer, intent(inout) :: ierr
    integer, intent(in) :: namelen ! length of input buffer
    integer, intent(in) :: rank_world ! used only for mpi_comm_split key
    integer, intent(in) :: commin ! global communicator
    integer, intent(inout) :: comm_name ! host-specific communicator
    integer, intent(inout) :: comm_hosts ! for ranks 0 in comm_name, inter-host communicator

    character(len=100) :: message

    integer :: itry ! hash collision counter
    integer :: i ! character loop index
    integer :: error ! MPI error number

    character(kind=c_char) :: c_name1(namelen+1) ! name plus salt in c datatype
    integer(kind=c_int32_t) :: c_crc32c ! integer hash in c datatype
    integer(kind=c_int32_t) :: c_length ! namelen+1 in c datatype
    integer(kind=c_int32_t) :: c_error ! error indicator in c datatype
    integer :: hash ! Fortran version of c_crc32c
    integer :: is_rank_0 ! 1 if rank 0 within comm_name, else 0
    integer :: rank ! Rank within comm_name
    logical :: match ! did the check_names report all names match?

    ! Salt:
    character(len=26), parameter :: ctry = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    ierr=-999

    do i=1,namelen
       c_name1(i+1)=name(i:i)
    enddo

    match=.false.
    hashtries: do itry=1,len(ctry)
       c_name1(1) = ctry(itry:itry)
       c_length=namelen+1
       c_error=-999
       c_crc32c=nems_c_crc32(c_name1,c_length,c_error)
       if(c_error/=0) then
          ! Should never get here.  This indicates the name is
          ! empty or beyond 2**31-3 bytes.
          ierr=1
          return
       endif
       hash=c_crc32c

       call MPI_Comm_Split(commin,hash,rank_world,comm_name,error)
       if(error/=0) then
          ierr=2
          return ! comm split failed
       endif
       call MPI_Comm_rank(comm_name,rank,error)
       if(error/=0) then
          ierr=3
          return
       endif

       is_rank_0=0
       if(rank==0) is_rank_0=1

       call MPI_Comm_Split(commin,is_rank_0,rank_world,comm_hosts,error)
       if(error/=0) then
          ierr=4
          return ! comm split failed
       endif

       call check_names(name,comm_name,comm_hosts,match,error)
       if(error/=0) then
          ierr=3
          return
       endif

       if(match) exit hashtries
       ! We get here on hash collisions.  That means the 
    end do hashtries

    if(.not.match) then
       ! Gave up after too many hash collisions.
       ierr=4
       return
    endif

    ierr=0
  end subroutine color_by_hash


end module module_NEMS_Rusage
