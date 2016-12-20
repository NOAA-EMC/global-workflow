module m_dtime
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    module background
!   prgmmr:      j guo <jguo@nasa.gov>
!      org:      NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:      2010-03-22
!
! abstract: background bin tester
!
! program history log:
!   2010-03-22  jing    - added this document block
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

!#define VERBOSE
#define ZERODIFFTEST

! module interface:

  use kinds, only: i_kind,r_kind
  implicit none; private

  public :: dtime_setup
  public :: dtime_check
  public :: dtime_show

!
! Revision history:
!   2010-03-22  jing    - added this document block
!   2009-08-19  jing    - created to support multi-pass observation "setup"
!                         processes.  This module is used by all observation
!                         "setup" routines to check if a given data (specified
!                         by argument _dtime_) is in the given analysis window
!                        (in_anybin) and in the given background bin (in_curbin).
!! Usage:
!!      call dtime_setup()
!!      call dtime_check(dtime,in_curbin,in_anybin)
!!      call dtime_show('setupt','t',i_t_ob_type)

!! This implementation is not thread-safe, because of these...
  integer(i_kind),save:: nm,nl,nr,nt    ! counts of in-time, early, late, and total
  real   (r_kind),save:: am,al,ar,at    ! means of in-time, early, late, and total

  character(len=*),parameter :: myname='m_dtime'
contains

subroutine dtime_setup()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:     subroutine dtime_setup
!   prgmmr:      j guo <jguo@nasa.gov>
!      org:      NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:      2010-03-22
!
! abstract: 
!
! program history log:
!   2010-03-22  jing    - added this document block
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

! subroutine interface:

  use kinds, only: r_kind,i_kind
  implicit none
  character(len=*),parameter :: myname_=myname//'::setup'

  nm=0; am=0._r_kind
  nl=0; al=0._r_kind
  nr=0; ar=0._r_kind
  nt=0; at=0._r_kind
end subroutine dtime_setup

subroutine dtime_check(dtime, in_curbin,in_anybin)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    subroutine dtime_check
!   prgmmr:      jing  <jguo@nasa.gov>
!      org:      NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:      2010-03-22
!
! abstract: determine of dtime is in current bin and/or in any bin
!
! program history log:
!   2010-03-22  jing    - added this document block
!   2010-04-20  jing    - redefined in_curbin conditions for special cases.
!                       - removed extrap_intime.
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

! subroutine interface:

  use kinds, only: r_kind,i_kind
  use guess_grids, only: nfldsig    , hrdifsig
  use guess_grids, only: nfldsig_all, hrdifsig_all
  implicit none
  real(r_kind),intent(in):: dtime
  logical,intent(out) :: in_curbin      ! in current bins
  logical,intent(out) :: in_anybin      ! in any bin
  character(len=*),parameter :: myname_=myname//'::check'

 ! for simple bookkeeping
  nt=nt+1
  at=at+(dtime-at)/nt

  in_curbin = (dtime>hrdifsig(1) .and. dtime<=hrdifsig(nfldsig))
#ifdef ZERODIFFTEST
  if(hrdifsig(1)==hrdifsig_all(1)) in_curbin = in_curbin .or. dtime<=hrdifsig(1)
  if(hrdifsig(nfldsig)==hrdifsig_all(nfldsig_all)) in_curbin = in_curbin .or. dtime>hrdifsig(nfldsig)
  in_anybin = .true.
#else
  in_curbin = in_curbin .or. nfldsig_all==1
  in_anybin = in_curbin .or. &
      (dtime>hrdifsig_all(1) .and. dtime<=hrdifsig_all(nfldsig_all))
#endif


  if(in_curbin) then
    nm=nm+1
    am=am+(dtime-am)/nm
    return
  endif
  if(in_anybin) return

  if(dtime <= hrdifsig_all(1)) then
    nl=nl+1
    al=al+(dtime-al)/nl
    return
  endif

  if(dtime > hrdifsig_all(nfldsig_all)) then
    nr=nr+1
    ar=ar+(dtime-ar)/nr
    return
  endif
end subroutine dtime_check

subroutine dtime_show(who,what,it)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    subroutine dtime_show
!   prgmmr:      j guo <jguo@nasa.gov>
!      org:      NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:      2010-03-22
!
! abstract: - show bin counters
!
! program history log:
!   2010-03-22  jing    - added this document block
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

! subroutine interface:

  use kinds, only: r_kind,i_kind
  use mpeu_util, only: tell
  implicit none
  character(len=*),intent(in) :: who
  character(len=*),intent(in) :: what
  integer(i_kind),intent(in):: it
  character(len=*),parameter :: myname_=myname//'::show'

#ifdef VERBOSE
#ifndef OLDCODE
  character(len=80):: bufr
  write(bufr,'(i4,4(i8,f10.5))') it,nt,at,nl,al,nm,am,nr,ar
  call tell(who,what//' '//trim(bufr))

#else
  call tell(who,what//', iobs_type=',it)
  call tell(who,what//', nt=',nt)
  call tell(who,what//', at=',at)

  call tell(who,what//', nm=',nm)
  call tell(who,what//', am=',am)
  call tell(who,what//', nl=',nl)
  call tell(who,what//', al=',al)
  call tell(who,what//', nr=',nr)
  call tell(who,what//', ar=',ar)
#endif
#endif
end subroutine dtime_show
end module m_dtime
