module ozinfo 
!$$$ module documentation block
!           .      .    .                                       .
! module:   ozinfo
!   prgmmr: treadon     org: np23                date: 2004-04-10
!
! abstract:  This module contains variables and routines related
!            to the assimilation of ozone observations (presently,
!            satellite based ozone observations)
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
!
! Subroutines Included:
!   sub init_oz       - set ozone related variables to defaults
!   sub ozinfo_read   - read in ozone info
!
! Functions Included:
!
! Variable Definitions:
!   def diag_ozone     - logical to turn off or on the diagnostic ozone file (true=on)
!   def jpch_oz        - number of (levels+1) * number of satellites
!   def mype_oz        - task id for writing out radiance diagnostics
!   def pob_oz         - pressure level of observation (hPa)
!   def gross_oz       - gross error limit
!   def error_oz       - observation error
!   def nusis_oz       - sensor/intrument/satellite id (14=NOAA-14, 15=NOAA-15, 16=NOAA-16, etc)
!   def nulev          - integer level of ozone observation
!   def iuse_oz        - integer flag to control usage of ozone data (-1=don't use, 1=use)
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
  public :: init_oz
  public :: ozinfo_read
! set passed variables to pubic
  public :: jpch_oz,diag_ozone,nusis_oz,iuse_oz,b_oz,pg_oz,gross_oz
  public :: error_oz,pob_oz,mype_oz,nulev
  public :: ihave_oz

  logical diag_ozone
  integer(i_kind) mype_oz,jpch_oz
  real(r_kind),allocatable,dimension(:)::pob_oz,gross_oz,error_oz,pg_oz,b_oz
  integer(i_kind),allocatable,dimension(:):: nulev,iuse_oz
  character(len=20),allocatable,dimension(:):: nusis_oz

  logical :: ihave_oz

contains
  
  subroutine init_oz
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_oz     initialize parameters for ozone data
!     prgmmr:    treadon     org: np23                date: 2004-04-10
!
! abstract:  This routine sets default values for variables used in 
!            the ozone processing routines
!
! program history log:
!   2004-04-10  treadon
!   2004-06-16  treadon, documentation
!   2005-07-28  treadon - increase jpch_oz from 52 to 53 (add omi data)
!   2010-05-29  todling - check existence in state vector (viz., guess)
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
    use state_vectors, only: svars3d
    use mpeu_util, only: getindex
    implicit none

    jpch_oz = 0                        ! number of enteries read from ozinfo
    diag_ozone = .true.                ! default is to generate ozone diagnostic file
    mype_oz     = max(0,npe-6)         ! mpi task to write ozone summary report
    ihave_oz=(getindex(svars3d,'oz')>0)! .t. when OZ present in state-vector

  end subroutine init_oz
  

  subroutine ozinfo_read
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ozinfo_read      read ozone information file
!     prgmmr:    treadon     org: np23                date: 2004-04-10
!
! abstract:  This routine reads the ozone information file, global_ozinfo.txt
!
! program history log:
!   2004-04-10  treadon
!   2004-06-16  treadon, documentation
!   2005-10-11  treadon - change ozinfo read to free format
!   2008-04-29  safford - rm redundant use
!   2010-05-29  todling - update interface
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
    use mpimod, only: mype
    use obsmod, only: iout_oz
    implicit none

    character(len=1):: cflg
    character(len=120) crecord
    integer(i_kind) lunin,j,k,istat,nlines
    data lunin / 47 /


!   Determine number of entries in ozone information file
    open(lunin,file='ozinfo',form='formatted')
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
       write(6,*)'OZINFO_READ:  ***ERROR*** error reading ozinfo, istat=',istat
       close(lunin)
       write(6,*)'OZINFO_READ:  stop program execution'
       call stop2(79)
    endif
    jpch_oz = j
    if(jpch_oz == 0)then
      close(lunin)
      return
    end if

!   Allocate arrays to hold ozone information
    allocate(nusis_oz(jpch_oz),nulev(jpch_oz),iuse_oz(jpch_oz), &
         pob_oz(jpch_oz),gross_oz(jpch_oz),error_oz(jpch_oz), &
         pg_oz(jpch_oz),b_oz(jpch_oz))


!   All mpi tasks open and read ozone information file.
!   Task mype_oz writes information to ozone runtime file
  
    if (mype==mype_oz) then
       open(iout_oz)
       write(iout_oz,110) jpch_oz
110    format('OZINFO_READ:  jpch_oz=',1x,i6)
    endif
    rewind(lunin)
    j=0
    do k=1,nlines
       read(lunin,100) cflg,crecord
       if (cflg == '!') cycle
       j=j+1
       read(crecord,*) nusis_oz(j),&
            nulev(j),iuse_oz(j),pob_oz(j),gross_oz(j),error_oz(j), &
            b_oz(j),pg_oz(j)
       if (mype==mype_oz) write(iout_oz,130) j,nusis_oz(j),nulev(j),&
               iuse_oz(j),pob_oz(j),gross_oz(j),error_oz(j),b_oz(j), &
               pg_oz(j)
    end do
    close(lunin)
    if (mype==mype_oz) close(iout_oz)

100 format(a1,a120)
130 format(i3,1x,a20,' lev = ',i4,' use = ',i2,' pob = ',f9.3,&
         ' gross = ',f7.3,' error = ',f7.3,' b_oz = ',f7.3,' pg_oz = ',f7.3)


!   Successful read, return to calling routine
    return
  end subroutine ozinfo_read
  
end module ozinfo
