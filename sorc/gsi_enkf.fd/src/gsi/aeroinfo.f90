module aeroinfo 
!$$$   module documentation block
!                .      .    .                                       .
! module:    aeroinfo
!   prgmmr: HCHuang          org: np23                date: 2009-08-10
!
! abstract:  This module contains variables and routines related
!            to the assimilation of aerosol observations (presently,
!            satellite based aerosol AOD observations)
!
! program history log:
!   2009-08-10  hchuang - original code modified from ozinfo
!   2010-10-20  hclin   - modified for total aod in channels
!
! Subroutines Included:
!   sub init_aero       - set aerosol related variables to defaults
!   sub init_aero_vars  - initialize aerosol related variables
!   sub final_aero_vars - finalize aerosol related variables
!   sub aeroinfo_read   - read in aerosol info
!
! Functions Included:
!
! Variable Definitions:
!   def diag_aero        - logical to turn off or on the diagnostic aerosol file (true=on)
!   def jpch_aero        - number of (nchannels) * number of satellites
!   def mype_aero        - task id for writing out radiance diagnostics
!   def pob_aero         - pressure level of observation (hPa)
!   def gross_aero       - gross error limit
!   def error_aero       - observation error
!   def nusis_aero       - sensor/intrument/satellite id (14=NOAA-14, 15=NOAA-15, 16=NOAA-16, etc)
!   def nuchan_aero      - integer channel of aerosol observation
!   def iuse_aero        - integer flag to control usage of aerosol data (-1=don't use, 1=use)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only:r_kind,i_kind
  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_aero
  public :: init_aero_vars
  public :: final_aero_vars
  public :: aeroinfo_read
! set passed variables to pubic
  public :: jpch_aero,diag_aero,nusis_aero,iuse_aero,b_aero,pg_aero,gross_aero
  public :: error_aero,pob_aero,mype_aero
  public :: nuchan_aero
  public :: aerojacnames,aerojacindxs,nsigaerojac

  logical diag_aero
  integer(i_kind) mype_aero,jpch_aero
  real(r_kind),allocatable,dimension(:)::pob_aero,gross_aero,error_aero,b_aero,pg_aero
  integer(i_kind),allocatable,dimension(:):: nuchan_aero,iuse_aero
  character(len=20),allocatable,dimension(:):: nusis_aero

  integer(i_kind) :: nsigaerojac
  character(len=20),allocatable,dimension(:):: aerojacnames
  integer(i_kind),  allocatable,dimension(:):: aerojacindxs

contains
  
  subroutine init_aero
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_aero     initialize parameters for aerosol data
!     prgmmr:    HCHuang     org: np23                date: 2009-08-10
!
! abstract:  This routine sets default values for variables used in 
!            the aerosol processing routines
!
! program history log:
!   2009-08-10  HCHuang  original code modified from init_oz
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

    jpch_aero = 0               ! number of enteries read from aeroinfo
    diag_aero = .true.          ! default is to generate aerosol diagnostic file
    mype_aero = 0               ! mpi task to write aerosol summary report

  end subroutine init_aero
  
  subroutine init_aero_vars
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    init_aero_vars
!
!   prgrmmr:     hclin      org: ncar/mmm                date: 2011-09-20
!
! abstract:  This routine sets parameters used in the aod
!            assimilation.  The parameters below depend on values
!            which may be altered by the SETUP namelist.
!
! program history log:
!   2011-09-20  hclin - modified from init_rad_vars for aod
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use mpimod, only: mype
    use gsi_chemguess_mod, only: gsi_chemguess_get
    use gridmod, only: nsig
    implicit none

    integer(i_kind) ii,isum,nvarjac,n_aerosols_crtm,ndim,ier
    integer(i_kind) ip25
    integer(i_kind),allocatable,dimension(:)::aux
    character(len=20),allocatable,dimension(:)::aerosols_names

!   inquire about variables in guess
    call gsi_chemguess_get ( 'dim', ndim, ier )

!   inquire number of aerosols to participate in CRTM calculations
    ip25 = -1

    call gsi_chemguess_get ( 'var::p25', ip25, ier)

    if (ip25 > 0) then
       call gsi_chemguess_get ( 'aerosols_4crtm::3d', n_aerosols_crtm, ier )
    else
       call gsi_chemguess_get ( 'aerosols_4crtm_jac::3d', n_aerosols_crtm, ier )
    endif

    if (n_aerosols_crtm>0) then
       allocate(aerosols_names(n_aerosols_crtm))

       if (ip25 > 0) then
          call gsi_chemguess_get ( 'aerosols_4crtm::3d', aerosols_names, ier )
          nvarjac=n_aerosols_crtm+1  !plus p25 whose jacobian is derived from dust1 and dust2
       else
          call gsi_chemguess_get ( 'aerosols_4crtm_jac::3d', aerosols_names, ier )
          nvarjac=n_aerosols_crtm
       endif

       allocate(aerojacnames(nvarjac))
       allocate(aerojacindxs(nvarjac))
       allocate(aux(nvarjac))
       do ii=1,n_aerosols_crtm
          aerojacnames(ii) = trim(aerosols_names(ii))
          aerojacindxs(ii) = nsig
       enddo
       if ( ip25 > 0 ) then
          aerojacnames(nvarjac) = 'p25'
          aerojacindxs(nvarjac) = nsig
       endif
!      Determine initial pointer location for each var in the Jacobian
       nsigaerojac = 0
       if(size(aerojacnames)>0) then
          nsigaerojac = sum(aerojacindxs)
          isum=0
          do ii=2,nvarjac
             isum=isum+aerojacindxs(ii-1)
             aux(ii) = isum
          enddo
          aux(1) = 0
          aerojacindxs = aux
       endif
       deallocate(aerosols_names,aux)
       if(mype==0) then
          write(6,*) 'Vars in Aero-Jacobian (dims)'
          write(6,*) '--------------------------'
          do ii=1,nvarjac
             write(6,*) aerojacnames(ii), aerojacindxs(ii)
          end do
       endif
    endif

    return
  end subroutine init_aero_vars

  subroutine final_aero_vars
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    final_aero_vars
!
!   prgrmmr:     hclin     org: ncar/mmm                date: 2011-09-20
!
! abstract:  This routine finalizes this package
!
! program history log:
!   2011-09-20   hclin - modified from final_rad_vars for aod
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

    implicit none

    if (allocated(aerojacindxs)) deallocate(aerojacindxs)
    if (allocated(aerojacnames)) deallocate(aerojacnames)

    return
  end subroutine final_aero_vars

  subroutine aeroinfo_read
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    aeroinfo_read      read aerosol information file
!     prgmmr:    HCHuang     org: np23                date: 2009-08-10
!
! abstract:  This routine reads the aerosol information file, aeroinfo -> global_aeroinfo.txt
!
! program history log:
!   2009-08-10  HCHuang  original code modified from ozinfo_read
!   2010-07-10  Todling  mype from mpimod
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
    use obsmod, only: iout_aero
    implicit none

    character(len=1)   :: cflg
    character(len=120) :: crecord
    character(len=80)  :: fname = 'aeroinfo'
    integer(i_kind)    :: j,k,istat,nlines
    integer(i_kind)    :: lunin = 47
    logical            :: lexist


!   Check the status of input file 
    inquire(file=trim(fname),exist=lexist)
    if ( lexist ) then

!      Determine number of entries in aerosol information file
       open(lunin,file=trim(fname),form='formatted')
       j=0
       nlines=0
       read1:  do 
          read(lunin,100,iostat=istat,end=123) cflg,crecord
          if (istat /= 0) exit
          nlines=nlines+1
          if (cflg == '!') cycle
          j=j+1
       end do read1
123    continue
       if (istat>0) then
          write(6,*)'AEROINFO_READ:  ***ERROR*** error reading ',trim(fname),' istat=',istat
          close(lunin)
          write(6,*)'AEROINFO_READ:  stop program execution'
          call stop2(79)
       endif
       jpch_aero = j


!      Allocate arrays to hold aerosol information
       allocate(nusis_aero(jpch_aero),nuchan_aero(jpch_aero),iuse_aero(jpch_aero), &
            pob_aero(jpch_aero),gross_aero(jpch_aero),error_aero(jpch_aero), &
            b_aero(jpch_aero),pg_aero(jpch_aero))


!      All mpi tasks open and read aerosol information file.
!      Task mype_aero writes information to aerosol runtime file
  
       if (mype==mype_aero) then
          open(iout_aero)
          write(iout_aero,110) jpch_aero
110       format('AEROINFO_READ:  jpch_aero=',1x,i6)
       endif
       rewind(lunin)
       j=0
       do k=1,nlines
          read(lunin,100) cflg,crecord
          if (cflg == '!') cycle
          j=j+1
          read(crecord,*) nusis_aero(j),&
               nuchan_aero(j),iuse_aero(j),pob_aero(j),gross_aero(j),error_aero(j), &
               b_aero(j),pg_aero(j)
          if (mype==mype_aero) write(iout_aero,130) j,nusis_aero(j),nuchan_aero(j),&
                  iuse_aero(j),pob_aero(j),gross_aero(j),error_aero(j),b_aero(j), &
                  pg_aero(j)
       end do
       close(lunin)
       if (mype==mype_aero) close(iout_aero)

100 format(a1,a120)
130 format(i3,1x,a20,' lev = ',i4,' use = ',i2,' pob = ',f9.3,&
         ' gross = ',f7.3,' error = ',f7.3,' b_aero = ',f7.3,' pg_aero = ',f7.3)


!      Successful read, return to calling routine
     else
!       File does not exist, write warning message to unit 6 to alert users
        if (mype==mype_aero) then
           write(6,*)'AEROINFO_READ:  ***WARNING*** FILE ',trim(fname),' does not exist'
           write(6,*)'AEROINFO_READ:  jpch_aero=',jpch_aero
        endif
     end if

     return
  end subroutine aeroinfo_read
  
end module aeroinfo
