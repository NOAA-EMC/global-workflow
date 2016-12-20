module mpeu_util
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    module mpeu_util
!   prgmmr:      j guo <jguo@nasa.gov>
!      org:      NASA/GSFC, Global Modeling and Assimilation Office, 610.1
!     date:      2010-03-17
!
! abstract: - utilities for runtime messaging, etc.
!
! program history log:
!   2010-03-17  j guo   - added this document block
!   2010-05-30  todling - add some real dirty mimic of i90's table read 
!   2010-87-19  todling - remove reference to abor1
!   2013-05-14  guo     - added getarec(), for not-commented text buffer.
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! !DESCRIPTION:
!
!   This module provides some basic utilities with generic interfaces.
!   These generic interfaces support limited argument type-kind-ranks,
!   such as default INTERGER, default REAL and DOUBLE PRECISION, etc..
!   The combination of these argument type-kinds, is efficient to most
!   user applications, without additional KIND specifications.  The
!   matching(or mismatching) of the type-kinds in this module to user
!   specific calls is left to the compiler to handle.  Therefore, this
!   implementation is designed NOT having to explicitly use user
!   defined KIND parameters.
!
!   To be compiled with -Dsys`uname -s` if necessary.
!

#define USE_MPI
#ifdef USE_MPI
#define USE_MPI_ABORT
#endif

#define INCLUDE_MPOUT

! module interface:

#if NCEP_ENV
      use kinds, only: IK => i_kind     ! default INTEGER kind
      use kinds, only: SP => r_single   ! default REAL kind
      use kinds, only: DP => r_double   ! DOUBLE PRECISION kind
#endif
      implicit none
      private    ! except

      public :: die, perr, warn, tell, assert_
      public :: luavail
      public :: stdin, stdout, stderr
      public :: strTemplate
      public :: getarec                 ! (lu,line,ier[,nrec][,commchar])
      public :: GetTableSize,GetTable
      public :: GetIndex                ! get index in array given user entry

        ! a stable sorting tool.  See <Use indexed sorting> section below
      public :: IndexSet                ! setup an index array
      public :: IndexSort               ! (stable)sort through the index array

#ifdef INCLUDE_MPOUT
      public :: stdout_open
      public :: stdout_close
#endif

    integer,parameter :: STDIN = 5
    integer,parameter :: STDOUT= 6
#ifdef sysHP_UX
    integer,parameter :: STDERR= 7
#else
    integer,parameter :: STDERR= 0
#endif
    interface luavail; module procedure luavail_; end interface

    interface die; module procedure &
      die_bul_, &
      die_chr_, &
      die_int_, &
      die_vint_, &
      die_flt_, &
      die_dbl_, &
      die2_,    &
      die_; end interface
    interface perr; module procedure &
      perr_bul_, &
      perr_chr_, &
      perr_int_, &
      perr_vint_, &
      perr_flt_, &
      perr_dbl_, &
      perr_; end interface
    interface warn; module procedure &
      warn_bul_, &
      warn_chr_, &
      warn_int_, &
      warn_vint_, &
      warn_flt_, &
      warn_dbl_, &
      warn_; end interface
    interface tell; module procedure &
      tell_bul_, &
      tell_chr_, &
      tell_int_, &
      tell_vint_, &
      tell_flt_, &
      tell_dbl_, &
      tell_; end interface

    interface mprint; module procedure &
      mprint_bul_, &
      mprint_chr_, &
      mprint_int_, &
      mprint_vint_, &
      mprint_flt_, &
      mprint_dbl_, &
      mprint_; end interface

    interface IndexSet; module procedure &
      set_; end interface

    interface IndexSort; module procedure &
      iSort_, & ! by an INTEGER key
      rSort_, & ! by a REAL key
      dSort_, & ! by a DOUBLE PRECISION key
      cSort_    ! by a CHARACTER(len=*) key
      end interface

    interface GetTableSize; module procedure &
      get_table_size_; end interface
    interface GetTable; module procedure &
      get_table_; end interface

    interface GetIndex; module procedure &
      getindex_; end interface

    interface getarec; module procedure getarec_; end interface


! !REVISION HISTORY:
!       19Aug09 - Jing Guo <Jing.Guo@nasa.gov>
!               - modified format, abort() message and interface, etc.
!       19Feb09 - Jing Guo <Jing.Guo@nasa.gov>
!               - Implemented for GSI to avoid the dependency on
!                 GMAO_mpeu/.
!               . Selected from mksi/satinfo_util.F90.
!               . Added StrTemplate() from m_StrTemplate.F90.  Format
!                 class is no limited to GrADS like.
!               . Some new %-keywords are added (%i, %j, %k, %l).
!               . Modified dropdead_() to use GSI abor1().
!
!       02May07 - Jing Guo <guo@gmao.gsfc.nasa.gov>
!               - initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname="mpeu_util"

#ifndef NCEP_ENV
  integer,parameter :: Ix=1
  real*4,parameter :: R4=1.E+0
  real*8,parameter :: R8=1.D+0

  integer,parameter :: IK=kind(Ix)      ! kind of default INTEGER
  integer,parameter :: SP=kind(R4)      ! kind of default REAL
  integer,parameter :: DP=kind(R8)      ! kind of DOUBLE PRECISION real
#endif

  integer,parameter :: PAGESIZE_=64
  integer,parameter :: MAX_LUNIT=1024

  integer,save :: mype=-1
  integer,save :: npes=-1

#ifdef INCLUDE_MPOUT
  type node
    private
    character(len=1),dimension(:),pointer:: name
    type(node),pointer:: next => null()
  end type node
  integer:: topsize_=-1

  type(node),pointer:: filelist => null()
#endif

!-----------------------------------------------------------------------
! Use indexed sorting
!
!       ...
!       integer,intent(in) :: N
!       type Observations
!         real:: prs,lon,lat
!         integer:: kt,ks,kx
!       end type Observations
!       type(Observations), dimension(N), intent(inout) :: obs
!
!       integer, dimension(size(obs)) :: indx      ! automatic array
!
!                       ! This tool allows incremental sorting.
!                       ! An incremental sorting can only be done
!                       ! with a stable-sorting algorithm, and
!                       ! in a reversed sequence.
!
!       call IndexSet(indx)
!       call IndexSort(indx(:),obs(:)%prs,descend=.true.)
!       call IndexSort(indx(:),obs(:)%lon,descend=.false.)
!       call IndexSort(indx(:),obs(:)%lat,descend=.false.)
!       call IndexSort(indx(:),obs(:)%kt,descend=.false.)
!       call IndexSort(indx(:),obs(:)%ks,descend=.false.)
!       call IndexSort(indx(:),obs(:)%kx,descend=.false.)
!
!       ! Sorting
!       obs(1:N) = obs( indx(1:N) )
!       ...
!       ! Unsorting (restore the original order)
!       obs( (indx(1:N) ) = obs(1:N)
!_______________________________________________________________________

contains
#ifdef INCLUDE_MPOUT

subroutine push_(name)
  implicit none
  character(len=*),intent(in) :: name

  type(node),pointer:: hold
  integer:: ic

  hold => filelist
  allocate(filelist)
  filelist%next => hold

  topsize_=len_trim(name)
  allocate(filelist%name(topsize_))
    do ic=1,topsize_
      filelist%name(ic)=name(ic:ic)
    enddo
end subroutine push_

subroutine pop_(stat)
  implicit none
  integer,intent(out) :: stat
  character(len=*),parameter:: myname_=myname//'.pop_'
  type(node),pointer:: hold

  stat=0
  if(topsize_<0 .or. .not.associated(filelist)) then
    call perr(myname_,'_filelist_ stack underflow')
    call perr(myname_,'topsize_ =',topsize_)
    call perr(myname_,'associated(fileist) =',associated(filelist))
    stat=-1
    return
  endif

  hold => filelist%next
  deallocate(filelist%name)
  deallocate(filelist)
  filelist => hold

  topsize_=-1
  if(associated(filelist)) topsize_=size(filelist%name)
end subroutine pop_

function top_()
  implicit none
  character(len=max(topsize_,0)):: top_
  integer:: ic
  if(topsize_==0) return
  do ic=1,topsize_
    top_(ic:ic) = filelist%name(ic)
  enddo
end function top_

subroutine stdout_open(prefix,rewind,stat,mask,comm)
  use mpeu_mpif, only : MPI_comm_world
!! -- This routine redirects local STDOUT to a file, named as
!!    "<prefix>.<pid>", where "<pid>" is the local PE rank in
!!    a leading-zero-filling format, determined by the size of
!!    the communicator, and started from at least "i3.3".
!!

!! -- call stdout_open(prefix='stdout',rewind=.true.,mask=3,comm=mycomm,stat=ier)
!!      rewind: default is append
!!      mask:   apply only if (mask>=0 .and. (iPE && mask)) == 0
!!              mask=-1, none
!!              mask=0, all, default
!!              mask=1, PE #0, 2, 4, 6, 8, every another PE
!!              mask=2, PE #0, 1, 4, 5, etc.
!!              mask=3, PE #0, 4, 8, 12, etc.
!!              mask=4, PE #0:3, not #4:7
!!              mask=12, PE #0:3, not #4:15

!! -- call stdout_close(stat=ier)
  implicit none
  character(len=*),optional,intent(in) :: prefix
  logical,optional,intent(in) :: rewind
  integer,optional,intent(out) :: stat
  integer,optional,intent(in) :: mask
  integer,optional,intent(in) :: comm

  logical:: yes_prefix_

  yes_prefix_=.false.
  if(present(prefix)) yes_prefix_=prefix/=""
  if(yes_prefix_) then
    call stdout_open__( prefix ,rewind=rewind,stat=stat,mask=mask,comm=comm)
  else
    call stdout_open__('stdout',rewind=rewind,stat=stat,mask=mask,comm=comm)
  endif
end subroutine stdout_open

subroutine stdout_open__(prefix,rewind,stat,mask,comm)
  implicit none
  character(len=*),intent(in) :: prefix
  logical,optional,intent(in) :: rewind
  integer,optional,intent(out) :: stat
  integer,optional,intent(in) :: mask
  integer,optional,intent(in) :: comm

  integer:: ier,ipe,npe,i
  character(len=len(prefix)+9) :: fname
  character(len=*),parameter:: myname_=myname//'::stdout_open'
  character(len=len('append')) :: position
  character(len=8) :: cmype_

  if(mype<0) call mype_get_(mype,npes,trim(myname_))

  call mype_get_(ipe,npe,trim(myname_),comm=comm)
  if(present(mask)) then
    !!!! while _ipe_ of _comm_ is used to check against mask, mype
    !!!! is always used to define the local file suffix, since it is
    !!!! used to identify output streams after runs.

    if(and(mask,ipe)/=0) return
  endif

  select case(npes)
  case(0:999)
    write(cmype_,'(i3.3)') mype
  case(1000:9999)
    write(cmype_,'(i4.4)') mype
  case(10000:99999)
    write(cmype_,'(i5.5)') mype
  case(100000:999999)
    write(cmype_,'(i6.6)') mype
  case default
    write(cmype_,'(i8.8)') mype
  endselect

  position='append'      ! this is default
  if(present(rewind)) then
    if(rewind) position='rewind'
  endif

  i=len_trim(prefix)
  if(prefix(i:i)=='/'.or.prefix(i:i)=='.') then
    fname=trim(prefix)//trim(cmype_)
  else
    fname=trim(prefix)//'.'//trim(cmype_)
  endif

  call close_if_(top_(),stat=ier)
  if(ier/=0) then
     call perr(myname_,'close_if_(STDOUT), STDOUT =',stdout)
     call perr(myname_,'close_if_(STDOUT), iostat =',ier)
     if(.not.present(stat)) call die(myname_)
     stat=ier
     return
  endif
  call mprint(stdout,myname_,'switching STDOUT to file "'//trim(fname)//'"')
  call push_(fname)
  open(stdout,file=fname,position=position,iostat=ier,action='write')
  if(ier/=0) then
    call perr(myname_,'open(STDOUT,"'//trim(fname)//'"), STDOUT =',stdout)
    call perr(myname_,'open(STDOUT,"'//trim(fname)//'"), iostat =',ier)
    if(.not.present(stat)) call die(myname_)
    stat=ier
    return
  endif
end subroutine stdout_open__

subroutine stdout_close(stat)
!! -- this routine ends redirected local STDOUT.
  implicit none
  integer,optional,intent(out):: stat

  character(len=*),parameter:: myname_=myname//'::stdout_close'
  integer:: ier

  if(present(stat)) stat=0
  call mprint(stdout,myname_,'switching back to default STDOUT')
  close(stdout,iostat=ier)
  if(ier/=0) then
    call perr(myname_,'close(STDOUT), stdout =',stdout)
    call perr(myname_,'close(STDOUT), iostat =',ier)
    if(.not.present(stat)) call die(myname_)
    stat=ier
    return
  endif
  call pop_(stat=ier)
  if(ier/=0) then
    call perr(myname_,'pop_(), stat =',ier)
    if(.not.present(stat)) call die(myname_)
    stat=ier
    return
  endif
  call open_if_(top_(),stat=ier)
  if(ier/=0) then
    call perr(myname_,'open_if_(STDOUT,"'//trim(top_())//'"), STDOUT =',stdout)
    call perr(myname_,'open_if_(STDOUT,"'//trim(top_())//'"), iostat =',ier)
    if(.not.present(stat)) call die(myname_)
    stat=ier
    return
  endif
end subroutine stdout_close

subroutine open_if_(fname,stat)
  implicit none
  character(len=*),intent(in):: fname
  integer,optional,intent(out) :: stat
  character(len=*),parameter:: myname_=myname//'.open_if_'
  integer:: ier
  if(present(stat)) stat=0
  if(fname=="") return
  open(stdout,file=fname,position='append',iostat=ier,action='write')
  if(ier/=0) then
    call perr(myname_,'open(STDOUT,"'//trim(fname)//'"), STDOUT =',stdout)
    call perr(myname_,'open(STDOUT,"'//trim(fname)//'"), iostat =',ier)
    if(.not.present(stat)) call die(myname_)
    stat=ier
    return
  endif
end subroutine open_if_

subroutine close_if_(fname,stat)
  implicit none
  character(len=*),intent(in):: fname
  integer,optional,intent(out) :: stat
  character(len=*),parameter:: myname_=myname//'.close_if_'
  integer:: ier
  if(present(stat)) stat=0
  if(fname=="") return
  close(stdout,iostat=ier)
  if(ier/=0) then
    call perr(myname_,'close(STDOUT,"'//trim(fname)//'"), STDOUT =',stdout)
    call perr(myname_,'close(STDOUT,"'//trim(fname)//'"), iostat =',ier)
    if(.not.present(stat)) call die(myname_)
    stat=ier
    return
  endif
end subroutine close_if_

#ifdef _NEW_CODE_
!! need to send outputs to variables.
!! need to set return code (stat=).
subroutine ls_(files)       ! show information? or just inquire(exists(file))
  call system("ls "//files)
end subroutine ls_
subroutine rm_(files)     ! delete, open();close(status='delete')
  call system("rm "//files)
end subroutine rm_
subroutine mkdir_(dir,mode,parents)
  call system("mkdir "//files)
end subroutine mkdir_
subroutine size_(file)    ! faster access?
  call system("wc -c "//files)
end subroutine size_
#endif
#endif

function myid_(who)
  implicit none
  character(len=*),intent(in) :: who
  character(len=10+len(who)) :: myid_

  character(len=10) :: fmtid
  if(mype<0) call mype_get_(mype,npes,trim(who)//'>myid_')
  select case(npes)
  case(0:999)
    fmtid='(i3.3,2a)'
  case(1000:9999)
    fmtid='(i4.4,2a)'
  case(10000:99999)
    fmtid='(i5.5,2a)'
  case(100000:999999)
    fmtid='(i6.6,2a)'
  case default
    fmtid='(i8.8,2a)'
  endselect

  if(who/="") then
    write(myid_,fmtid) mype,']',trim(who)
    myid_='['//adjustl(myid_)
  else
    write(myid_,fmtid) mype
    myid_=adjustl(myid_)
  endif
end function myid_

subroutine mype_get_(mype,npes,who,comm)
  use mpeu_mpif, only : MPI_comm_world
  implicit none
  integer,intent(out) :: mype
  integer,intent(out) :: npes
  character(len=*),intent(in) :: who
  integer,optional,intent(in) :: comm

  integer :: ier
  integer :: mycomm
  mycomm=MPI_comm_world
  if(present(comm)) mycomm=comm
  mype=0;npes=1
#ifdef USE_MPI
  call MPI_comm_rank(mycomm,mype,ier)
    if(ier/=0) then
      write(stderr,'(3a)') &
        trim(who),'>mype_get_(): >>> ERROR <<< ','MPI_comm_rank(), ier= ',ier
      if(stderr==stdout) return
      write(stdout,'(3a)') &
        trim(who),'>mype_get_(): >>> ERROR <<< ','MPI_comm_rank(), ier= ',ier
      call dropdead_()
    endif
  call MPI_comm_size(mycomm,npes,ier)
    if(ier/=0) then
      write(stderr,'(3a)') &
        trim(who),'>mype_get_(): >>> ERROR <<< ','MPI_comm_size(), ier= ',ier
      if(stderr==stdout) return
      write(stdout,'(3a)') &
        trim(who),'>mype_get_(): >>> ERROR <<< ','MPI_comm_size(), ier= ',ier
      call dropdead_()
    endif
#endif
end subroutine mype_get_

function luavail_() result(lu)
  implicit none
  integer :: lu

  character(len=*),parameter :: myname_=myname//'::luavail_'
  integer ios
  logical inuse

  lu=-1
  ios=0
  inuse=.true.

  do while(ios==0.and.inuse)
    lu=lu+1

! Test #1, reserved units

    inuse = lu==stdout .or. lu==stdin .or. lu==stderr

#ifdef sysSunOS
! Reserved units under SunOS
    inuse = lu==100 .or. lu==101 .or. lu==102
#endif

! Test #2, in-use

    if(.not.inuse) inquire(unit=lu,opened=inuse,iostat=ios)

    if(lu >= MAX_LUNIT) ios=-1
  end do
  if(ios/=0) lu=-1
end function luavail_

subroutine mprint_(lu,who,what)
  implicit none
  integer,intent(in) :: lu
  character(len=*),intent(in) :: who,what
  write(lu,'(3a)') &
    trim(myid_(who)),'(): ',trim(what)
end subroutine mprint_
subroutine mprint_chr_(lu,who,what,val)
  implicit none
  integer,intent(in) :: lu
  character(len=*),intent(in) :: who,what,val
  write(lu,'(3a)',advance='no') &
    trim(myid_(who)),'(): ',trim(what)
  write(lu,'(1x,3a)') '"',trim(val),'"'
end subroutine mprint_chr_
subroutine mprint_bul_(lu,who,what,val)
  implicit none
  integer,intent(in) :: lu
  character(len=*),intent(in) :: who,what
  logical,intent(in) :: val
  write(lu,'(3a)',advance='no') &
    trim(myid_(who)),'(): ',trim(what)
  write(lu,'(1x,l1)') val
end subroutine mprint_bul_
subroutine mprint_int_(lu,who,what,val,base)
  implicit none
  integer,intent(in) :: lu
  character(len=*),intent(in) :: who,what
  integer,intent(in) :: val
  character(len=*),optional,intent(in) :: base
        ! base is either z(hex), o(oct), b(binary), default(decimal)
  write(lu,'(3a)',advance='no') &
    trim(myid_(who)),'(): ',trim(what)
  if(present(base)) then
    select case(base)
    case('z','Z')
      write(lu,'(1x,z8.8)') val
    case('o','O')
      write(lu,'(1x,o11.11)') val
    case('b','B')
      write(lu,'(1x,b32.32)') val
    case default
      write(lu,'(1x,i0)') val
    endselect
  else
    write(lu,'(1x,i0)') val
  endif
end subroutine mprint_int_
subroutine mprint_vint_(lu,who,what,vals,base,format,sum)
  implicit none
  integer,intent(in) :: lu
  character(len=*),intent(in) :: who,what
  integer,dimension(:),intent(in) :: vals
  character(len=*),optional,intent(in) :: base
  character(len=*),optional,intent(in) :: format
  logical,optional,intent(in) :: sum
  integer:: i
  write(lu,'(3a)',advance='no') &
    trim(myid_(who)),'(): ',trim(what)
  if(present(sum)) then
    if(sum) then
      if(present(format)) then
        write(lu,fmt=format,advance='no') sumof_(vals(:))
      else
        write(lu,'(1x,i0)',advance='no') sumof_(vals(:))
      endif
    endif
  endif
  do i=1,size(vals)
  if(present(base)) then
    select case(base)
    case('z','Z')
      write(lu,'(1x,z8.8)',advance='no') vals(i)
    case('o','O')
      write(lu,'(1x,o11.11)',advance='no') vals(i)
    case('b','B')
      write(lu,'(1x,b32.32)',advance='no') vals(i)
    case default
      if(present(format)) then
        write(lu,fmt=format,advance='no') vals(i)
      else
        write(lu,'(1x,i0)',advance='no') vals(i)
      endif
    endselect
  else
    if(present(format)) then
      write(lu,fmt=format,advance='no') vals(i)
    else
      write(lu,'(1x,i0)',advance='no') vals(i)
    endif
  endif
  enddo
  write(lu,'()',advance='yes')
end subroutine mprint_vint_
function sumof_(vals)
  implicit none
  integer,dimension(:),intent(in) :: vals
  integer:: sumof_
  sumof_=sum(vals(:))
end function sumof_
subroutine mprint_flt_(lu,who,what,val)
  implicit none
  integer,intent(in) :: lu
  character(len=*),intent(in) :: who,what
  real(SP),intent(in) :: val
  write(lu,'(3a)',advance='no') &
    trim(myid_(who)),'(): ',trim(what)
  write(lu,'(1x,g15.7)') val
end subroutine mprint_flt_
subroutine mprint_dbl_(lu,who,what,val)
  implicit none
  integer,intent(in) :: lu
  character(len=*),intent(in) :: who,what
  real(DP),intent(in) :: val
  write(lu,'(3a)',advance='no') &
    trim(myid_(who)),'(): ',trim(what)
  write(lu,'(1x,g23.15)') val
end subroutine mprint_dbl_
subroutine mprint_err_(lu,who,what,errm)
  implicit none
  integer,intent(in) :: lu
  character(len=*),intent(in) :: who,what,errm
  write(lu,'(3a)',advance='no') &
    trim(myid_(who)),'(): >>> ERROR <<< ',trim(what)
  write(lu,'(1x,3a)') '"',trim(errm),'"'
end subroutine mprint_err_

subroutine perr_(who,what)
  implicit none
  character(len=*),intent(in) :: who,what
  call mprint(stderr,who,'>>> ERROR <<< '//trim(what))
  if(stderr==stdout) return
  call mprint(stdout,who,'>>> ERROR <<< '//trim(what))
end subroutine perr_

subroutine perr_chr_(who,what,val)
  implicit none
  character(len=*),intent(in) :: who,what,val
  call mprint(stderr,who,'>>> ERROR <<< '//trim(what),val)
  if(stderr==stdout) return
  call mprint(stdout,who,'>>> ERROR <<< '//trim(what),val)
end subroutine perr_chr_
subroutine perr_bul_(who,what,val)
  implicit none
  character(len=*),intent(in) :: who,what
  logical,intent(in) :: val
  call mprint(stderr,who,'>>> ERROR <<< '//trim(what),val)
  if(stderr==stdout) return
  call mprint(stdout,who,'>>> ERROR <<< '//trim(what),val)
end subroutine perr_bul_
subroutine perr_int_(who,what,val,base)
  implicit none
  character(len=*),intent(in) :: who,what
  integer,intent(in) :: val
  character(len=*),optional,intent(in) :: base
  call mprint(stderr,who,'>>> ERROR <<< '//trim(what),val,base=base)
  if(stderr==stdout) return
  call mprint(stdout,who,'>>> ERROR <<< '//trim(what),val,base=base)
end subroutine perr_int_
subroutine perr_vint_(who,what,vals,base,format,sum)
  implicit none
  character(len=*),intent(in) :: who,what
  integer,dimension(:),intent(in) :: vals
  character(len=*),optional,intent(in) :: base
  character(len=*),optional,intent(in) :: format
  logical,optional,intent(in) :: sum
  call mprint(stderr,who,'>>> ERROR <<< '//trim(what),vals,base=base,format=format,sum=sum)
  if(stderr==stdout) return
  call mprint(stdout,who,'>>> ERROR <<< '//trim(what),vals,base=base,format=format,sum=sum)
end subroutine perr_vint_
subroutine perr_flt_(who,what,val)
  implicit none
  character(len=*),intent(in) :: who,what
  real(SP),intent(in) :: val
  call mprint(stderr,who,'>>> ERROR <<< '//trim(what),val)
  if(stderr==stdout) return
  call mprint(stdout,who,'>>> ERROR <<< '//trim(what),val)
end subroutine perr_flt_
subroutine perr_dbl_(who,what,val)
  implicit none
  character(len=*),intent(in) :: who,what
  real(DP),intent(in) :: val
  call mprint(stderr,who,'>>> ERROR <<< '//trim(what),val)
  if(stderr==stdout) return
  call mprint(stdout,who,'>>> ERROR <<< '//trim(what),val)
end subroutine perr_dbl_

subroutine warn_(who,what)
  implicit none
  character(len=*),intent(in) :: who,what
  call mprint(stdout,who,'>>> WARNING <<< '//trim(what))
end subroutine warn_
subroutine warn_chr_(who,what,val)
  implicit none
  character(len=*),intent(in) :: who,what,val
  call mprint(stdout,who,'>>> WARNING <<< '//trim(what),val)
end subroutine warn_chr_
subroutine warn_bul_(who,what,val)
  implicit none
  character(len=*),intent(in) :: who,what
  logical,intent(in) :: val
  call mprint(stdout,who,'>>> WARNING <<< '//trim(what),val)
end subroutine warn_bul_
subroutine warn_int_(who,what,val,base)
  implicit none
  character(len=*),intent(in) :: who,what
  integer,intent(in) :: val
  character(len=*),optional,intent(in) :: base
  call mprint(stdout,who,'>>> WARNING <<< '//trim(what),val,base=base)
end subroutine warn_int_
subroutine warn_vint_(who,what,vals,base,format,sum)
  implicit none
  character(len=*),intent(in) :: who,what
  integer,dimension(:),intent(in) :: vals
  character(len=*),optional,intent(in) :: base
  character(len=*),optional,intent(in) :: format
  logical,optional,intent(in) :: sum
  call mprint(stdout,who,'>>> WARNING <<< '//trim(what),vals,base=base,format=format,sum=sum)
end subroutine warn_vint_
subroutine warn_flt_(who,what,val)
  implicit none
  character(len=*),intent(in) :: who,what
  real(SP),intent(in) :: val
  call mprint(stdout,who,'>>> WARNING <<< '//trim(what),val)
end subroutine warn_flt_
subroutine warn_dbl_(who,what,val)
  implicit none
  character(len=*),intent(in) :: who,what
  real(DP),intent(in) :: val
  call mprint(stdout,who,'>>> WARNING <<< '//trim(what),val)
end subroutine warn_dbl_

subroutine tell_(who,what)
  implicit none
  character(len=*),intent(in) :: who,what
  call mprint(stdout,who,what)
end subroutine tell_
subroutine tell_chr_(who,what,val)
  implicit none
  character(len=*),intent(in) :: who,what,val
  call mprint(stdout,who,what,val)
end subroutine tell_chr_
subroutine tell_bul_(who,what,val)
  implicit none
  character(len=*),intent(in) :: who,what
  logical,intent(in) :: val
  call mprint(stdout,who,what,val)
end subroutine tell_bul_
subroutine tell_int_(who,what,val,base)
  implicit none
  character(len=*),intent(in) :: who,what
  integer,intent(in) :: val
  character(len=*),optional,intent(in) :: base
  call mprint(stdout,who,what,val,base)
end subroutine tell_int_
subroutine tell_vint_(who,what,vals,base,format,sum)
  implicit none
  character(len=*),intent(in) :: who,what
  integer,dimension(:),intent(in) :: vals
  character(len=*),optional,intent(in) :: base
  character(len=*),optional,intent(in) :: format
  logical,optional,intent(in) :: sum
  call mprint(stdout,who,what,vals,base=base,format=format,sum=sum)
end subroutine tell_vint_
subroutine tell_flt_(who,what,val)
  implicit none
  character(len=*),intent(in) :: who,what
  real(SP),intent(in) :: val
  call mprint(stdout,who,what,val)
end subroutine tell_flt_
subroutine tell_dbl_(who,what,val)
  implicit none
  character(len=*),intent(in) :: who,what
  real(DP),intent(in) :: val
  call mprint(stdout,who,what,val)
end subroutine tell_dbl_

subroutine dropdead_()
#ifdef USE_MPI_ABORT
  use mpeu_mpif, only: mpi_comm_world
#endif
  implicit none
  integer:: ier
  integer,parameter:: myer=2

  character(len=08):: cdate
  character(len=10):: ctime
  character(len=05):: czone

  call date_and_time(date=cdate,time=ctime,zone=czone)
  call mprint(stdout,'dropdead','at '//cdate//':'//ctime//'(z'//czone//'00)')
  call mprint(stderr,'dropdead','at '//cdate//':'//ctime//'(z'//czone//'00)')

#ifdef USE_MPI_ABORT
  call mpi_abort(mpi_comm_world,myer,ier)
#else
  call exit(myer)
#endif
end subroutine dropdead_

subroutine die_(who)
  implicit none
  character(len=*),intent(in) :: who
  call perr_(who,'terminated')
  call dropdead_()
end subroutine die_
subroutine die2_(who,what)
  implicit none
  character(len=*),intent(in) :: who,what
  call perr_(who,what)
  call dropdead_()
end subroutine die2_
subroutine die_chr_(who,what,val)
  implicit none
  character(len=*),intent(in) :: who,what,val
  call perr_chr_(who,what,val)
  call dropdead_()
end subroutine die_chr_
subroutine die_bul_(who,what,val)
  implicit none
  character(len=*),intent(in) :: who,what
  logical,intent(in) :: val
  call perr_bul_(who,what,val)
  call dropdead_()
end subroutine die_bul_
subroutine die_int_(who,what,val,base)
  implicit none
  character(len=*),intent(in) :: who,what
  integer,intent(in) :: val
  character(len=*),optional,intent(in) :: base
  call perr_int_(who,what,val,base=base)
  call dropdead_()
end subroutine die_int_
subroutine die_vint_(who,what,vals,base,format,sum)
  implicit none
  character(len=*),intent(in) :: who,what
  integer,dimension(:),intent(in) :: vals
  character(len=*),optional,intent(in) :: base
  character(len=*),optional,intent(in) :: format
  logical,optional,intent(in) :: sum
  call perr_vint_(who,what,vals,base=base,format=format,sum=sum)
  call dropdead_()
end subroutine die_vint_
subroutine die_flt_(who,what,val)
  implicit none
  character(len=*),intent(in) :: who,what
  real(SP),intent(in) :: val
  call perr_flt_(who,what,val)
  call dropdead_()
end subroutine die_flt_
subroutine die_dbl_(who,what,val)
  implicit none
  character(len=*),intent(in) :: who,what
  real(DP),intent(in) :: val
  call perr_dbl_(who,what,val)
  call dropdead_()
end subroutine die_dbl_
subroutine assert_(str,from,line)
  implicit none
  character(len=*),intent(in) :: str    ! a message of assert_()
  character(len=*),intent(in) :: from   ! where assert_() is invoked.
  integer,         intent(in) :: line   ! where assert_() is invoked.
  character(len=*),parameter :: myname_='ASSERT_'
  call perr(myname_,'failed',str)
  call perr(myname_,'file =',from)
  call perr(myname_,'line #',line)
  call die(myname_)
end subroutine assert_
subroutine assert_GE_(m,n,who,str)
  implicit none
  integer,intent(in) :: m,n
  character(len=*),intent(in) :: who   ! where assert_GE_() is invoked.
  character(len=*),intent(in) :: str   ! a message of assert_GE_()
  character(len=*),parameter :: myname_='ASSERT_GE_'
  if(.not.(m>=n)) then
    call perr(myname_,'test failed',str)
    call perr(myname_,'operand 1 = ',m)
    call perr(myname_,'operand 2 = ',n)
    call die(myname_,who)
  endif
end subroutine assert_GE_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: strTemplate - expanding a format template to a string
!
! !DESCRIPTION:
!
!   A template resolver formatting a string with string, time, and
!   dimensioin variables.  The format descriptors are similar to those
!   used in the GrADS, with some extensions.
!
!       "${E}"  substitute environment variable ${E}
!       "%y4"   substitute with a 4 digit year
!       "%y2"   a 2 digit year
!       "%m1"   a 1 or 2 digit month
!       "%m2"   a 2 digit month
!       "%mc"   a 3 letter month in lower cases
!       "%Mc"   a 3 letter month with a leading letter in upper case
!       "%MC"   a 3 letter month in upper cases
!       "%d1"   a 1 or 2 digit day
!       "%d2"   a 2 digit day
!       "%h1"   a 1 or 2 digit hour
!       "%h2"   a 2 digit hour
!       "%h3"   a 3 digit hour (?)
!       "%n2"   a 2 digit minute
!       "%s"    a string variable
!       "%i"    dims(1) of dims=(/im,jm,km,lm/)
!       "%j"    dims(2) of dims=(/im,jm,km,lm/)
!       "%k"    dims(3) of dims=(/im,jm,km,lm/)
!       "%l"    dims(4) of dims=(/im,jm,km,lm/)
!       "%%"    a "%"
!
! !INTERFACE:

    subroutine strTemplate(str,tmpl,nymd,nhms,dims,xid,stat)
      !! use m_stdio, only : stderr
      !! use m_die,   only : die,perr
      implicit none

      character(len=*),intent(out) :: str       ! the output

      character(len=*),intent(in ) :: tmpl      ! a "format"

      integer,intent(in ),optional :: nymd
                        ! yyyymmdd, substituting "%y4", "%y2", "%m1",
                        ! "%m2", "%mc", "%Mc', and "%MC"

      integer,intent(in ),optional :: nhms
                        ! hhmmss, substituting "%h1", "%h2", "%h3",
                        ! and "%n2"

      integer,dimension(:),intent(in ),optional :: dims
                        ! integers, substituing "%i", "%j", "%k", "%l"

      character(len=*),intent(in ),optional :: xid
                        ! a string substituting a "%s".  Trailing
                        ! spaces will be ignored

      integer,intent(out),optional :: stat
                        ! error code

! !REVISION HISTORY:
!       18Feb09 - Jing Guo <Jing.Guo@nasa.gov>
!               - implemented for GSI to cut the m_StrTemplate
!                 dependency on GMAO_mpeu.
!               - Extended the "%-keyword" with "%i", "%j", "%k", and
!                 "%l" to support dims=(/./) option.
!       19Dec06 - Jing Guo <guo@gmao.gsfc.nasa.gov>
!               - Merged changes between 1.1.2.6 and 1.1.2.9 to 1.2,
!                 including a fix at bug nymd==0 and environment
!                 variable ($env or ${env}) support if getenv() is
!                 available from the system.
!       01Jun99 - Jing Guo <guo@dao.gsfc.nasa.gov>
!               - initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::strTemplate'

  character(len=3),parameter,dimension(12) :: mon_lc = (/              &
       'jan','feb','mar','apr','may','jun',                            &
       'jul','aug','sep','oct','nov','dec'/)

  character(len=3),parameter,dimension(12) :: mon_wd = (/     &
       'Jan','Feb','Mar','Apr','May','Jun',                   &
       'Jul','Aug','Sep','Oct','Nov','Dec'/)

  character(len=3),parameter,dimension(12) :: mon_uc = (/     &
       'JAN','FEB','MAR','APR','MAY','JUN',                   &
       'JUL','AUG','SEP','OCT','NOV','DEC'/)


  integer :: iy4,iy2,imo,idy
  integer :: ihr,imn
  integer :: i,i1,i2,m,k
  integer :: ln_tmpl,ln_str
  integer :: istp,kstp
  integer :: ier

  character(len=1) :: c0,c1,c2
  character(len=8) :: sbuf
!________________________________________
! Determine iyr, imo, and idy
  iy4=-1
  iy2=-1
  imo=-1
  idy=-1
  if(present(nymd)) then
    if(nymd <= 0) then
        call perr(myname_,'nymd <= 0',nymd)
        if(.not.present(stat)) call die(myname_)
        stat=1
        return
    endif

    i=nymd
    iy4=i/10000
    iy2=mod(iy4,100)
      i=mod(i,10000)
    imo=i/100
      i=mod(i,100)
    idy=i
  endif
!________________________________________
! Determine ihr and imn
  ihr=-1
  imn=-1
  if(present(nhms)) then
    if(nhms < 0) then
        call perr(myname_,'nhms < 0',nhms)
        if(.not.present(stat)) call die(myname_)
        stat=1
        return
    endif

    i=nhms
    ihr=i/10000
    i=mod(i,10000)
    imn=i/100
  endif
!________________________________________

  ln_tmpl=len_trim(tmpl)        ! size of the format template
  ln_str =len(str)              ! size of the output string
!________________________________________

  if(present(stat)) stat=0

str=""

i=0; istp=1
k=1; kstp=1

do while( i+istp <= ln_tmpl )   ! A loop over all tokens in (tmpl)

  if(k>ln_Str) exit             ! truncate the output here.

  i=i+istp
  c0=tmpl(i:i)

  select case(c0)
  case ("$")
    call genv_(tmpl,ln_tmpl,i,istp,str,ln_str,k,ier)
    if(ier/=0) then
      call perr(myname_,'genv_("'//tmpl(i:ln_tmpl)//'"',ier)
      if(.not.present(stat)) call die(myname_)
      stat=1
      return
    endif

  case ("%")
!________________________________________

    c1=""
    i1=i+1
    if(i1 <= ln_Tmpl) c1=tmpl(i1:i1)
!________________________________________

    select case(c1)

    case("s")
      if(.not.present(xid)) then
        write(stderr,'(2a)') myname_,              &
                   ': optional argument expected, "xid="'
        if(.not.present(stat)) call die(myname_)
        stat=1
        return
      endif

      istp=2
      m=min(k+len_trim(xid)-1,ln_str)
      str(k:m)=xid
      k=m+1
      cycle

    case("i":"l")                      ! from "i" to "l", (i,j,k,l)
      if(.not.present(dims)) then
        write(stderr,'(2a)') myname_,      &
           ': optional argument expected, "dims=(/./)"'
        if(.not.present(stat)) call die(myname_)
        stat=1
        return
      endif

      m=ichar(c1)-ichar("i")+1                    ! m=1,2,3,4 for i,j,k,l

      if(m>size(dims)) then
        write(stderr,'(2a)') myname_,            &
            ': additional "dims=(/./)" element expected'
        write(stderr,'(2a,i4)') myname_,': size(dims) = ',size(dims)
        write(stderr,'(2a,2a)') myname_,': %-keyword  = "%',c1,'"'
        if(.not.present(stat)) call die(myname_)
        stat=1
        return
      endif
      ! If m<size(dims), any extra dims elements are ignored

      write(sbuf,'(i8)') dims(m)
      sbuf=adjustl(sbuf)           ! adjust left all digits

      istp=2                       ! size of the "%" keyword

      m=min(k+len_trim(sbuf)-1,ln_str)
      str(k:m)=sbuf
      k=m+1
      cycle

    case("%","$")

      istp=2
      str(k:k)=c1
      k=k+1              ! kstp=1
      cycle

    case default

      c2=""
      i2=i+2
      if(i2 <= ln_Tmpl) c2=tmpl(i2:i2)
!________________________________________

      select case(c1//c2)

      case("y4","y2","m1","m2","mc","Mc","MC","d1","d2")
        if(.not.present(nymd)) then
           write(stderr,'(2a)') myname_,        &
                ': optional argument expected, "nymd="'
          if(.not.present(stat)) call die(myname_)
          stat=1
          return
        endif
        istp=3

      case("h1","h2","h3","n2")
        if(.not.present(nhms)) then
           write(stderr,'(2a)') myname_,          &
              ': optional argument expected, "nhms="'
           if(.not.present(stat)) call die(myname_)
           stat=1
           return
        endif
        istp=3

      case default

        write(stderr,'(4a)') myname_,     &
            ': invalid template entry, "',trim(tmpl(i:)),'"'
        if(.not.present(stat)) call die(myname_)
        stat=2
        return

      end select          ! case(c1//c2)
    end select            ! case(c1)
!________________________________________

    select case(c1)

    case("y")
      select case(c2)
      case("2")
        write(sbuf,'(i2.2)') iy2
        kstp=2
      case("4")
        write(sbuf,'(i4.4)') iy4
        kstp=4
      case default
        write(stderr,'(4a)') myname_,                  &
             ': invalid template entry, "',trim(tmpl(i:)),'"'
        if(.not.present(stat)) call die(myname_)
        stat=2
        return
      end select

    case("m")
      select case(c2)
      case("1")
        if(imo < 10) then
          write(sbuf,'(i1)') imo
          kstp=1
        else
          write(sbuf,'(i2)') imo
          kstp=2
        endif
      case("2")
        write(sbuf,'(i2.2)') imo
        kstp=2
      case("c")
        sbuf=mon_lc(imo)
        kstp=3
      case default
        write(stderr,'(4a)') myname_,           &
           ': invalid template entry, "',trim(tmpl(i:)),'"'
        if(.not.present(stat)) call die(myname_)
        stat=2
        return
      end select

    case("M")
      select case(c2)
      case("c")
        sbuf=mon_wd(imo)
        kstp=3
      case("C")
        sbuf=mon_uc(imo)
        kstp=3
      case default
        write(stderr,'(4a)') myname_,          &
          ': invalid template entry, "',trim(tmpl(i:)),'"'
        if(.not.present(stat)) call die(myname_)
        stat=2
        return
      end select

    case("d")
      select case(c2)
      case("1")
        if(idy < 10) then
          write(sbuf,'(i1)') idy
          kstp=1
        else
          write(sbuf,'(i2)') idy
          kstp=2
        endif
      case("2")
        write(sbuf,'(i2.2)') idy
        kstp=2
      case default
        write(stderr,'(4a)') myname_,           &
          ': invalid template entry, "',trim(tmpl(i:)),'"'
        if(.not.present(stat)) call die(myname_)
        stat=2
        return
      end select

    case("h")
      select case(c2)
      case("1")
        if(ihr < 10) then
          write(sbuf,'(i1)') ihr
          kstp=1
        else
          write(sbuf,'(i2)') ihr
          kstp=2
        endif
      case("2")
        write(sbuf,'(i2.2)') ihr
        kstp=2
      case("3")
        write(sbuf,'(i3.3)') ihr
        kstp=3
      case default
        write(stderr,'(4a)') myname_,       &
          ': invalid template entry, "',trim(tmpl(i:)),'"'
        if(.not.present(stat)) call die(myname_)
        stat=2
        return
      end select

    case("n")
      select case(c2)
      case("2")
        write(sbuf,'(i2.2)') imn
        kstp=2
      case default
        write(stderr,'(4a)') myname_,         &
          ': invalid template entry, "',trim(tmpl(i:)),'"'
        if(.not.present(stat)) call die(myname_)
        stat=2
        return
      end select

    case default
      write(stderr,'(4a)') myname_,            &
           ': invalid template entry, "',trim(tmpl(i:)),'"'
      if(.not.present(stat)) call die(myname_)
      stat=2
      return
    end select                ! case(c1)

    m=min(k+kstp-1,ln_Str)
    str(k:m)=sbuf
    k=m+1

  case default

    istp=1
    str(k:k)=tmpl(i:i)
    k=k+1

  end select     ! case(c0)
end do

contains
subroutine genv_(tmpl,lnt,i,istp,str,lns,k,ier)
  implicit none
  character(len=*),intent(in) :: tmpl
  integer,intent(in)  :: lnt
  integer,intent(in)  :: i
  integer,intent(out) :: istp
  character(len=*),intent(inout) :: str
  integer         ,intent(in)    :: lns
  integer         ,intent(inout) :: k
  integer,intent(out) :: ier

  integer :: j,jb,je
  integer :: l,m
  logical :: bracket,more
  character(len=256) :: env

  j=i+1         ! skip "$"
  ier=0

  if(j>lnt) then
    ier=1
    return
  endif

  bracket = tmpl(j:j)=='{'
  if(bracket) j=j+1

! There is at least one a letter (including "_") to start a
! variable name

  select case(tmpl(j:j))
  case ("A":"Z","a":"z","_")
  case default
    ier=2
    return
  end select

  jb=j
  je=j

  if(bracket) then

    more=.true.
    do while(more)
      select case(tmpl(j:j))
      case ("A":"Z","a":"z","_","0":"9")
        je=j
        j=j+1
      case ("}")           ! End if "}" or eos
        j=j+1
        exit
      case default
        ier=3
        return
      end select
      more=j<=lnt
    enddo

  else

    more=.true.
    do while(more)
      select case(tmpl(j:j))
      case ("A":"Z","a":"z","_","0":"9")
        je=j
        j=j+1
      case default
        exit
      end select
      more=j<=lnt
    enddo
  endif

  istp=j-i

  call getenv(tmpl(jb:je),env)
  l=len_trim(env)
  m=min(k+l-1,lns)
  str(k:m)=env
  k=m+1

end subroutine genv_
end subroutine strTemplate

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: set_ - Initialize an array of data location indices
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine set_(indx)
      implicit none
      integer, dimension(:), intent(out) :: indx          ! indices

! !REVISION HISTORY:
!       15Mar00 - Jing Guo
!             . Modified the interface, by removing the explicit size
!       09Sep97 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!       04Jan99 - Jing Guo <guo@thunder> - revised prolog format
!EOP ___________________________________________________________________

  integer :: i

  do i=1,size(indx)
    indx(i)=i
  end do

end subroutine set_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: iSort_ - A stable merge index sorting of INTs.
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine iSort_(indx,keys,descend)
      implicit none

      integer, dimension(:), intent(inout) :: indx
      integer, dimension(:), intent(in) :: keys
      logical, optional, intent(in)  :: descend

! !REVISION HISTORY:
!       15Mar00- Jing Guo
!             . Modified the interface, by removing the explicit size
!       02Feb99 - Jing Guo <guo@thunder> - Added if(present(stat)) ...
!       04Jan99 - Jing Guo <guo@thunder> - revised the prolog
!       09Sep97 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP ___________________________________________________________________

  logical :: dsnd
  integer, dimension(size(indx)) :: mtmp
  integer :: n

  character(len=*),parameter :: myname_=myname//'::iSort_'

  n=size(indx)
  dsnd=.false.
  if(present(descend)) dsnd=descend

  call MergeSort_()
contains
subroutine MergeSort_()
  implicit none
  integer :: mstep,lstep
  integer :: lb,lm,le

  mstep=1
  do while(mstep < n)
    lstep=mstep*2

    lb=1
    do while(lb < n)
      lm=lb+mstep
      le=min(lm-1+mstep,n)

      call merge_(lb,lm,le)
      indx(lb:le)=mtmp(lb:le)
      lb=le+1
    end do

    mstep=lstep
  end do
end subroutine MergeSort_

subroutine merge_(lb,lm,le)
  integer,intent(in) :: lb,lm,le
  integer :: l1,l2,l

  l1=lb
  l2=lm
  do l=lb,le
    if(l2>le) then
      mtmp(l)=indx(l1)
      l1=l1+1
    elseif(l1>=lm) then
      mtmp(l)=indx(l2)
      l2=l2+1
    else
      if(dsnd) then
        if(keys(indx(l1)) >= keys(indx(l2))) then
          mtmp(l)=indx(l1)
          l1=l1+1
        else
          mtmp(l)=indx(l2)
          l2=l2+1
        endif
      else
        if(keys(indx(l1)) <= keys(indx(l2))) then
          mtmp(l)=indx(l1)
          l1=l1+1
        else
          mtmp(l)=indx(l2)
          l2=l2+1
        endif
      endif
    endif
  end do
end subroutine merge_

end subroutine iSort_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: rSort_ - A stable merge index sorting REALs.
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine rSort_(indx,keys,descend)
      implicit none

      integer, dimension(:), intent(inout) :: indx
      real(SP),dimension(:), intent(in) :: keys
      logical, optional, intent(in)  :: descend

! !REVISION HISTORY:
!       15Mar00 - Jing Guo
!            . Modified the interface, by removing the explicit size
!       02Feb99 - Jing Guo <guo@thunder> - Added if(present(stat)) ...
!       04Jan99 - Jing Guo <guo@thunder> - revised the prolog
!       09Sep97 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP ___________________________________________________________________

  logical :: dsnd
  integer, dimension(size(indx)) :: mtmp
  integer :: n

  character(len=*),parameter :: myname_=myname//'::rSort_'

  n=size(indx)
  dsnd=.false.
  if(present(descend)) dsnd=descend

  call MergeSort_()
contains
subroutine MergeSort_()
  implicit none
  integer :: mstep,lstep
  integer :: lb,lm,le

  mstep=1
  do while(mstep < n)
    lstep=mstep*2

    lb=1
    do while(lb < n)
      lm=lb+mstep
      le=min(lm-1+mstep,n)

      call merge_(lb,lm,le)
      indx(lb:le)=mtmp(lb:le)
      lb=le+1
    end do

    mstep=lstep
  end do
end subroutine MergeSort_

subroutine merge_(lb,lm,le)
  integer,intent(in) :: lb,lm,le
  integer :: l1,l2,l

  l1=lb
  l2=lm
  do l=lb,le
    if(l2>le) then
      mtmp(l)=indx(l1)
      l1=l1+1
    elseif(l1>=lm) then
      mtmp(l)=indx(l2)
      l2=l2+1
    else
      if(dsnd) then
        if(keys(indx(l1)) >= keys(indx(l2))) then
          mtmp(l)=indx(l1)
          l1=l1+1
        else
          mtmp(l)=indx(l2)
          l2=l2+1
        endif
      else
        if(keys(indx(l1)) <= keys(indx(l2))) then
          mtmp(l)=indx(l1)
          l1=l1+1
        else
          mtmp(l)=indx(l2)
          l2=l2+1
        endif
      endif
    endif
  end do
end subroutine merge_

end subroutine rSort_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: dSort_ - A stable merge index sorting DOUBLEs.
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine dSort_(indx,keys,descend)
      implicit none

      integer, dimension(:), intent(inout) :: indx
      real(DP), dimension(:), intent(in) :: keys
      logical, optional, intent(in)  :: descend

! !REVISION HISTORY:
!       15Mar00 - Jing Guo
!             . Modified the interface, by removing the explicit size
!       02Feb99 - Jing Guo <guo@thunder> - Added if(present(stat)) ...
!       04Jan99 - Jing Guo <guo@thunder> - revised the prolog
!       09Sep97 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP ___________________________________________________________________

  logical :: dsnd
  integer, dimension(size(indx)) :: mtmp
  integer :: n

  character(len=*),parameter :: myname_=myname//'::dSort_'

  n=size(indx)
  dsnd=.false.
  if(present(descend)) dsnd=descend

  call MergeSort_()
contains
subroutine MergeSort_()
  implicit none
  integer :: mstep,lstep
  integer :: lb,lm,le

  mstep=1
  do while(mstep < n)
    lstep=mstep*2

    lb=1
    do while(lb < n)
      lm=lb+mstep
      le=min(lm-1+mstep,n)

      call merge_(lb,lm,le)
      indx(lb:le)=mtmp(lb:le)
      lb=le+1
    end do

    mstep=lstep
  end do
end subroutine MergeSort_

subroutine merge_(lb,lm,le)
  integer,intent(in) :: lb,lm,le
  integer :: l1,l2,l

  l1=lb
  l2=lm
  do l=lb,le
    if(l2>le) then
      mtmp(l)=indx(l1)
      l1=l1+1
    elseif(l1>=lm) then
      mtmp(l)=indx(l2)
      l2=l2+1
    else
      if(dsnd) then
        if(keys(indx(l1)) >= keys(indx(l2))) then
          mtmp(l)=indx(l1)
          l1=l1+1
        else
          mtmp(l)=indx(l2)
          l2=l2+1
        endif
      else
        if(keys(indx(l1)) <= keys(indx(l2))) then
          mtmp(l)=indx(l1)
          l1=l1+1
        else
          mtmp(l)=indx(l2)
          l2=l2+1
        endif
      endif
    endif
  end do
end subroutine merge_

end subroutine dSort_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: cSort_ - A stable merge index sorting of CHAR(*)s.
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine cSort_(indx,keys,descend)
      implicit none

      integer, dimension(:), intent(inout) :: indx
      character(len=*), dimension(:), intent(in) :: keys
      logical, optional, intent(in)  :: descend

! !REVISION HISTORY:
!       15Mar00 - Jing Guo
!            . Modified the interface, by removing the explicit size
!       02Feb99 - Jing Guo <guo@thunder> - Added if(present(stat)) ...
!       04Jan99 - Jing Guo <guo@thunder> - revised the prolog
!       09Sep97 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP ___________________________________________________________________

  logical :: dsnd
  integer, dimension(size(indx)) :: mtmp
  integer :: n

  character(len=*),parameter :: myname_=myname//'::cSort_'

  n=size(indx)
  dsnd=.false.
  if(present(descend)) dsnd=descend

  call MergeSort_()
contains
subroutine MergeSort_()
  implicit none
  integer :: mstep,lstep
  integer :: lb,lm,le

  mstep=1
  do while(mstep < n)
    lstep=mstep*2

    lb=1
    do while(lb < n)
      lm=lb+mstep
      le=min(lm-1+mstep,n)

      call merge_(lb,lm,le)
      indx(lb:le)=mtmp(lb:le)
      lb=le+1
    end do

    mstep=lstep
  end do
end subroutine MergeSort_

subroutine merge_(lb,lm,le)
  integer,intent(in) :: lb,lm,le
  integer :: l1,l2,l

  l1=lb
  l2=lm
  do l=lb,le
    if(l2>le) then
      mtmp(l)=indx(l1)
      l1=l1+1
    elseif(l1>=lm) then
      mtmp(l)=indx(l2)
      l2=l2+1
    else
      if(dsnd) then
        if(keys(indx(l1)) >= keys(indx(l2))) then
          mtmp(l)=indx(l1)
          l1=l1+1
        else
          mtmp(l)=indx(l2)
          l2=l2+1
        endif
      else
        if(keys(indx(l1)) <= keys(indx(l2))) then
          mtmp(l)=indx(l1)
          l1=l1+1
        else
          mtmp(l)=indx(l2)
          l2=l2+1
        endif
      endif
    endif
  end do
end subroutine merge_

end subroutine cSort_

! RTodling: The quickest and dirtiest version of i90 I can some up with
subroutine get_table_size_(tname,lu,ntotal,nactual)
implicit none
integer,intent(in) :: lu
character(len=*)::tname
integer,intent(out) :: ntotal
integer,intent(out) :: nactual
integer(IK) ier,ln,ios,n,ncomment
character(len=256)::buf

! Scan file for desired table first
! and get size of table
ncomment=0
n=0
rewind(lu)
done_scan: do
  read(lu,*,iostat=ier) buf
  if(ier/=0) exit
  if(trim(buf)==''.or.buf(1:1)=='#'.or.buf(1:1)=='!') cycle ! ignore comments outside table
  ln=len(trim(tname))
  if(index(buf(1:ln),trim(tname))>0) then ! found wanted table
     n=0
     table_scan: do  ! start reading table
        line_scan: do ! start reading line
           n=n+1
           read(lu,'(a)',advance='no',eor=998,iostat=ios) buf ! read next line, save contents
        enddo line_scan ! finished reading line
998 continue
        if(buf(1:2)=='::') exit  ! end of table
        if(buf(1:1)=='#'.or.buf(1:1)=='!') ncomment=ncomment+1
     enddo table_scan
     exit ! finished reading table
  endif
enddo done_scan
ntotal=n
nactual=max(0,n-ncomment-1)

end subroutine get_table_size_

subroutine get_table_(tname,lu,ntot,nact,utable)
implicit none
integer,intent(in) :: lu,ntot,nact
character(len=*),intent(in):: tname
character(len=*),intent(inout):: utable(nact)

character(len=256)::buf
integer(IK) ier,ln,i,n,ios

! Now get contents
n=ntot
rewind(lu)
done_read: do
  read(lu,*,iostat=ier) buf
  if(ier/=0) exit
  if(trim(buf)==''.or.buf(1:1)=='#'.or.buf(1:1)=='!') cycle ! ignore comments outside table
  ln=len(trim(tname))
  if(index(buf(1:ln),trim(tname))>0) then ! found wanted table
     i=0
     table: do  ! start reading table
        line: do ! start reading line
           read(lu,'(a)',advance='no',eor=999,iostat=ios) buf ! read next line, save contents
        enddo line ! finished reading line
999 continue
        if(buf(1:2)=='::') exit  ! end of table
           if(buf(1:1)=='#'.or.buf(1:1)=='!') then
              ! ignore
           else
              if(i>nact) then
                 write(6,*) 'error reading table'  
                stop
              endif
              i=i+1
              utable(i)=trim(buf)
           endif
     enddo table
     exit ! finished reading table
  endif
enddo done_read
end subroutine get_table_

integer(IK) function getindex_(varnames,usrname)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    getid_
!   prgmmr: todling         org: gmao                date:
!
! abstract:
!
! program history log:
!   2010-05-28  todling - initial code
!
!   input argument list:
!    varnames - array w/ variable names (e.g., cvars3d, or nrf_var)
!    usrname  - name of desired control variable
!
!   output argument list:
!     getindex_ - variable index in varnames (control variable name array)
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  character(len=*),intent(in) :: varnames(:)
  character(len=*),intent(in) :: usrname
  integer(IK) i
  getindex_=-1
  do i=1,size(varnames)
     if(trim(usrname)==trim(varnames(i))) then
        getindex_=i
        exit
     endif
  enddo
end function getindex_

subroutine getarec_(lu,line,ier,nrec,commchar)
  implicit none
  integer,intent(in) :: lu
  character(len=*),intent(out) :: line
  integer,intent(out) :: ier
  integer,optional,intent(out) :: nrec ! count of record readings
  character(len=*),optional,intent(in) :: commchar ! set of comment chars

character(len=1) :: c
character(len=*),parameter :: SPC=achar(32),TAB=achar(09)
character(len=*),parameter :: NUL=achar(00),COM='#'

if(present(nrec)) nrec=0

    ! Read records until a line of non-blank and any non-comment text.
    ! A pure commont text record is a record with non-block content
    ! lead by a "#".
  read(lu,'(a)',iostat=ier) line
  do while(ier==0)
    if(present(nrec)) nrec=nrec+1
    c=leadchar_(line)
    if(present(commchar)) then
      if(c/=SPC .and. c/=TAB .and. index(commchar,c)/=1) exit
    else
      if(c/=SPC .and. c/=TAB .and. c/=COM) exit
    endif
    read(lu,'(a)',iostat=ier) line
  enddo
contains
function leadchar_(line) result(c)
  implicit none
  character(len=*),intent(in) :: line
  character(len=1) :: c
  integer :: i,l
  i=0
  l=len(line)
  c=SPC
  do while(i<l)
    i=i+1
    if(line(i:i)==SPC .or. line(i:i)==TAB) cycle
    c=line(i:i)
    return
  enddo
end function leadchar_
end subroutine getarec_
end module mpeu_util
