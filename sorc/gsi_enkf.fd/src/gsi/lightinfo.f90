module lightinfo
!$$$ module documentation block
!           .      .    .                                       .
!   module: lightinfo
!   prgmmr: Apodaca     org: CSU/CIRA            date: 2015-08-11
! abstract: This module contains variables related to the
!           direct assimilation of lightning observations
!           (e.g. GOES-16 GLM).
!
! program history log:
!     
!           .       .       .       .       .       .       .       .
!   2016-05-03  Apodaca - updates regarding GLM observation errors 
!
! Subroutines Included:
!   sub init_light         - initialize lightning related variables to defaults
!   sub lightinfo_read     - read in lightning information 



! Variable Definitions
!   def diag_light  - flag to toggle the creation of a lightning diagnostic file
!   def nlighttype  - maximum number of lightning data types
!   def mype_light  - task id for writing out lightning diagnostics
!   def deltiml     - model timestep
!   def loberr      - lightning observation error
!   def gross_light - gross error for lightning obs      
!   def glermax     - gross error parameter - max error      
!   def glermin     - gross error parameter - min error      
!   def b_light     - b value for variational QC      
!   def pg_light    - pg value for variational QC      
!   def nulight     - satellite/instrument                
!   def iuse_light  - use to turn off lightning data
!
! attributes:
!   language: Fortran 90 or higher
!   machine:  
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_light
  public :: lightinfo_read
! set passed variables to public
  public :: nulight,nlighttype,pg_light,b_light,diag_light,iuse_light
  public :: glermin,glermax,gross_light,mype_light
  public :: loberr 
  character(len=80)  :: fname = 'lightinfo'
  logical diag_light
  integer(i_kind) nlighttype,mype_light
  real(r_kind) deltiml
  real(r_kind),allocatable,dimension(:)::loberr,gross_light,b_light,pg_light
  real(r_kind),allocatable,dimension(:)::glermin,glermax
  integer(i_kind),allocatable,dimension(:)::iuse_light
  character(len=20),allocatable,dimension(:)::nulight 
contains
  
  subroutine init_light
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_light
!     prgmmr:    apodaca    org: CSU/CIRA             date: 2015-08-11
!
! abstract:  set defaults for variables used in lightning  
!            assimilation routines
!
! program history log:
!   2016-05-03  apodaca, updates regarding GLM observation errors
!   2016-06-13  apodaca, documentation
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
    use constants, only: r3600,one
    implicit none

    real(r_kind),parameter:: r1200=1200.0_r_kind

    nlighttype = 0       ! number of entries read from lightinfo, 
    deltiml    = r1200   ! model timestep
    diag_light =.true.   ! flag to toggle creation of lightning diagnostic file
    mype_light = 0       ! task to print light info. Note that mype_light 
                      ! MUST equal mype_rad (see radinfo.f90) in order for 
                      ! statspcp.f90 to print out the correct information          
  end subroutine init_light

  subroutine lightinfo_read
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lightinfo_read
!     prgmmr:    apodaca     org: CSU/CIRA                date: 2015-08-14
!
! abstract:  read text file containing information (satellite/instrument id, 
!            observation type, error, usage flags) for lightning observations.  
!
! program history log:
!   2015-08-14  apodaca - original code based on pcpinfo and aeroinfo
!   2016-05-03  apodaca - updates regarding GLM observation errors
!   input argument list:
!      mype - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use mpimod, only: mype
    use constants, only: zero
    use obsmod, only: iout_light
    implicit none

! Declare local varianbes
    logical lexist
    character(len=1):: cflg
    character(len=120) crecord
    integer(i_kind) lunin,j,k,istat,nlines
    
    data lunin / 47 /
    
   
    !   Check the status of input file
 
    inquire(file=trim(fname),exist=lexist)
 
    if ( lexist ) then
 
!-----------------------------------------------------------
! Determine number of entries in light information file
!-----------------------------------------------------------

       open(lunin,file='lightinfo',form='formatted')
 

       j=0
       nlines=0
       read1:  do
          read(lunin,100,iostat=istat,end=120) cflg,crecord
          if (istat /= 0) exit
          nlines=nlines+1
          if (cflg == '!') cycle
          j=j+1
       end do read1
   120 continue

       if (istat>0) then
          write(6,*)'LIGHTINFO_READ:  ***ERROR*** error reading lightinfo, istat=',istat
          close(lunin)
          write(6,*)'LIGHTINFO_READ:  stop program execution'
          call stop2(79)
       endif
       nlighttype=j


    ! Allocate arrays to hold lightning information
       allocate(nulight(nlighttype),iuse_light(nlighttype),loberr(nlighttype), gross_light(nlighttype), &
             glermin(nlighttype),glermax(nlighttype),b_light(nlighttype),pg_light(nlighttype))


    ! All mpi tasks open and read lightinfo information file.
    ! Task mype_light writes information to light runtime file
 
       if (mype==mype_light) then
          open(iout_light)
          write(iout_light,110) nlighttype
110       format('LIGHTINFO_READ:  nlighttype=',1x,i6)
       endif
       rewind(lunin)

!----------------------------------------------------------
! READ INFO FILE
!----------------------------------------------------------
       j=0
       do k=1,nlines
          read(lunin,100)  cflg,crecord
          if (cflg == '!') cycle
          j=j+1
          read(crecord,*) nulight(j),iuse_light(j),loberr(j),&
               gross_light(j),glermin(j),glermax(j),b_light(j),pg_light(j)
          if (mype==mype_light)  write(iout_light,130) nulight(j),&
              iuse_light(j),loberr(j),gross_light(j),glermax(j),&
              glermin(j),b_light(j),pg_light(j)
       end do

       close(lunin)
       if (mype==mype_light) close(iout_light)

100    format(a1,a120)
130    format(a20,' iuse_light = ',i2, ' err = ',&
            f7.3,' gross = ',f7.3,' glermax = ',f7.3,' glermin = ',f7.3, ' b_light = ',f7.3, ' pg_light = ',f7.3)

        ! Successful read, return to calling routine
    
    else    
        ! File does not exist, write warning message to alert users
!  For many usages light data is not important.  Write line to output.
!      if (mype==mype_light) then
!         open(iout_light)
!         write(iout_light,*)'LIGHTINFO_READ:  ***WARNING*** FILE ',trim(fname),'does not exist'
          if(mype==mype_light)write(6,*)'LIGHTINFO_READ:  FILE ',trim(fname),'does not exist'
!         close(iout_light)
!      endif
    end if

    return
  end subroutine lightinfo_read

  
end module lightinfo
