module coinfo 
!$$$ module documentation block
!           .      .    .                                       .
! module:   coinfo
!   prgmmr: treadon     org: np23                date: 2004-04-10
!
! abstract:  This module contains variables and routines related
!            to the assimilation of co observations (presently,
!            satellite based co observations)
!
! program history log:
!   2004-04-10  treadon - original code
!   2004-05-13  kleist  - original documentation
!   2004-06-16  treadon - update documentation
!   2004-12-22  treadon - rename logical "idiag_ozone" to "diag_ozone"
!   2005-09-28  derber  - Modify for new ozinfo file, add var qc parameters
!   2006-02-03  derber  - modify for new obs control and obs count
!   2007-06-29  Zhou    - change total number of ozone enteries (jpch_oz) from
!                         53 (version 6 SBUV/2) to 67 (version 8 SBUV/2)
!   2010-04-01  tangborn - created coinfo.f90 from ozinfo.f90
!
! Subroutines Included:
!   sub init_co       - set co related variables to defaults
!   sub ozinfo_read   - read in co info
!
! Functions Included:
!
! Variable Definitions:
!   def diag_co     - logical to turn off or on the diagnostic co file (true=on)
!   def jpch_co        - number of (levels+1) * number of satellites
!   def mype_co        - task id for writing out radiance diagnostics
!   def pob_co         - pressure level of observation (hPa)
!   def gross_co       - gross error limit
!   def error_co       - co observation error (VMR or Total column?)
!   def nusis_co       - sensor/intrument/satellite id (MOPITT #?) 
!   def nulev          - integer level of co observation
!   def iuse_co        - integer flag to control usage of co data (-1=don't use, 1=use)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_co
  public :: coinfo_read
! set passed variables to pubic
  public :: jpch_co,diag_co,nusis_co,iuse_co,b_co,pg_co,gross_co
  public :: error_co,pob_co,mype_co,nulev
  public :: ihave_co

  logical diag_co
  integer(i_kind) mype_co,jpch_co
  real(r_kind),allocatable,dimension(:)::pob_co,gross_co,error_co,pg_co,b_co
  integer(i_kind),allocatable,dimension(:):: nulev,iuse_co
  character(len=20),allocatable,dimension(:):: nusis_co

  logical :: ihave_co  ! when .t., will go after CO info

contains
  
  subroutine init_co
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_co     initialize parameters for co data
!     prgmmr:    treadon     org: np23                date: 2004-04-10
!
! abstract:  This routine sets default values for variables used in 
!            the co processing routines
!
! program history log:
!   2004-04-10  treadon
!   2004-06-16  treadon, documentation
!   2005-07-28  treadon - increase jpch_oz from 52 to 53 (add omi data)
!   2010-04-01  tangborn- start work on co version
!   2010-05-29  todling - check chem-bundle for presence of CO
!
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use mpimod, only: npe              ! contains the number of mpi tasks, variable "npe"
    use gsi_chemguess_mod, only: gsi_chemguess_get
    implicit none
    integer(i_kind) :: ico,ier

    jpch_co = 0                        ! number of enteries read from coinfo 
    diag_co = .false.                  ! default is to generate co diagnostic file
    mype_co = max(0,npe-6)             ! mpi task to write co summary report
    call gsi_chemguess_get ('var::co', ico, ier )
    ihave_co=(ico>0)                  ! .t. when CO present in state-vector

  end subroutine init_co
  

  subroutine coinfo_read
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    coinfo_read      read co information file
!     prgmmr:    treadon     org: np23                date: 2004-04-10
!
! abstract:  This routine reads the co information file, global_coinfo.txt
!
! program history log:
!   2004-04-10  treadon
!   2004-06-16  treadon, documentation
!   2005-10-11  treadon - change ozinfo read to free format
!   2008-04-29  safford - rm redundant use
!   2010-04-01  tangborn - start on co version
!   2010-05-29  todling - add ihave-co check; update interface
!
!   input argument list:
!     mype - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use mpimod, only: mype
    use obsmod, only: iout_co
    implicit none

    character(len=1):: cflg
    character(len=120) crecord
    integer(i_kind) lunin,j,k,istat,nlines
    data lunin / 47 /

!   If CO not present in Chem-Bundle, just return
    if(.not.ihave_co) return

!   Determine number of entries in co information file
    open(lunin,file='coinfo',form='formatted')
    j=0
    nlines=0
    read1:  do 
       read(lunin,100,iostat=istat,end=123) cflg,crecord
       if (istat /= 0) exit
       nlines=nlines+1
       if (cflg == '!') cycle
       j=j+1
    end do read1
123 continue
    if (istat>0) then
       write(6,*)'COINFO_READ:  ***ERROR*** error reading coinfo, istat=',istat
       close(lunin)
       write(6,*)'COINFO_READ:  stop program execution'
       call stop2(79)
    endif
    jpch_co = j


!   Allocate arrays to hold co information
    allocate(nusis_co(jpch_co),nulev(jpch_co),iuse_co(jpch_co), &
         pob_co(jpch_co),gross_co(jpch_co),error_co(jpch_co), &
         pg_co(jpch_co),b_co(jpch_co))


!   All mpi tasks open and read co information file.
!   Task mype_co writes information to co runtime file
  
    if (mype==mype_co) then
       open(iout_co)
       write(iout_co,*)'COINFO_READ:  jpch_co=',jpch_co
    endif
    rewind(lunin)
    j=0
    do k=1,nlines
       read(lunin,100) cflg,crecord
       if (cflg == '!') cycle
       j=j+1
       read(crecord,*) nusis_co(j),&
            nulev(j),iuse_co(j),pob_co(j),gross_co(j),error_co(j), &
            b_co(j),pg_co(j)
       if (mype==mype_co) write(iout_co,130) j,nusis_co(j),nulev(j),&
               iuse_co(j),pob_co(j),gross_co(j),error_co(j),b_co(j), &
               pg_co(j)
    end do
    close(lunin)
    if (mype==mype_co) close(iout_co)

100 format(a1,a120)
130 format(i3,1x,a20,' lev = ',i4,' use = ',i2,' pob = ',f9.3,&
         ' gross = ',f7.3,' error = ',f7.3,' b_co = ',f7.3,' pg_co = ',f7.3)


!   Successful read, return to calling routine
    return
  end subroutine coinfo_read
  
end module coinfo
