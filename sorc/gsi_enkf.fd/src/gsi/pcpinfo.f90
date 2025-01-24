module pcpinfo
!$$$ module documentation block
!           .      .    .                                       .
! module:   pcpinfo
!   prgmmr: treadon     org: np23                date: 2003-09-25
!
! abstract: This moduce contains variables pertinent to
!           assimilation of precipitation rates
!
! program history log:
!   2004-05-13  kleist, documentation
!   2004-06-15  treadon, reformat documentation
!   2004-12-22  treadon - rename logical "idiag_pcp" to "diag_pcp"
!   2005-09-28  derber - modify pcpinfo input and add qc input
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-04-27  derber - remove jppfp
!   2007-01-19  treadon - remove tinym1_obs since no longer used
!   2008-04-29  safford - rm unused uses
!   2009-01-23  todling - place back tinym1_obs since need for ltlint option
!
! Subroutines Included:
!   sub init_pcp          - initialize pcp related variables to defaults
!   sub pcpinfo_read      - read in pcp info and biases
!   sub pcpinfo_write     - write out pcp biases
!   sub create_pcp_random - generate random number for precip. assimilation
!   sub destroy_pcp_random- deallocate random number array
!
! Variable Definitions
!   def diag_pcp    - flag to toggle creation of precipitation diagnostic file
!   def npredp      - number of predictors in precipitation bias correction
!   def npcptype    - maximum number of precipitation data types
!   def mype_pcp    - task id for writing out pcp diagnostics
!   def deltim      - model timestep
!   def dtphys      - relaxation time scale for convection
!   def tiny_obs    - used to check whether or not to include pcp forcing
!   def tinym1_obs  - small number (tiny_obs) minus one
!   def varchp      - precipitation rate observation error
!   def gross_pcp   - gross error for precip obs      
!   def b_pcp       - b value for variational QC      
!   def pg_pcp      - pg value for variational QC      
!   def predxp      - precipitation rate bias correction coefficients
!   def xkt2d       - random numbers used in SASCNV cloud top selection
!   def nupcp       - satellite/instrument                
!   def iusep       - use to turn off pcp data
!   def ibias       - pcp bias flag, used for IO
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
  public :: init_pcp
  public :: pcpinfo_read
  public :: pcpinfo_write
  public :: create_pcp_random
  public :: destroy_pcp_random
! set passed variables to public
  public :: npcptype,npredp,tinym1_obs,pg_pcp,b_pcp,diag_pcp,iusep
  public :: nupcp,deltim,dtphys,tiny_obs,predxp,gross_pcp,ibias
  public :: xkt2d,varchp

  logical diag_pcp
  integer(i_kind) npredp,npcptype,mype_pcp
  real(r_kind) deltim,dtphys
  real(r_kind) tinym1_obs,tiny_obs
  real(r_kind),allocatable,dimension(:):: varchp,gross_pcp,b_pcp,pg_pcp
  real(r_kind),allocatable,dimension(:,:):: predxp ,xkt2d
  integer(i_kind),allocatable,dimension(:):: iusep,ibias
  character(len=20),allocatable,dimension(:):: nupcp
  
contains
  
  subroutine init_pcp
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_pcp
!     prgmmr:    treadon     org: np23                date: 2003-09-25
!
! abstract:  set defaults for variables used in precipitation rate 
!            assimilation routines
!
! program history log:
!   2003-09-25  treadon
!   2004-05-13  treadon, documentation
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

    npredp    = 6             ! number of predictors in precipitation bias correction
    npcptype  = 0             ! number of entries read from pcpinfo
    deltim    = r1200         ! model timestep
    dtphys    = r3600         ! relaxation time scale for convection
    diag_pcp =.true.          ! flag to toggle creation of precipitation diagnostic file
    mype_pcp  = 0             ! task to print pcp info to.  Note that mype_pcp MUST equal
                              !    mype_rad (see radinfo.f90) in order for statspcp.f90
                              !    to print out the correct information          
    tiny_obs = 1.e-9_r_kind   ! "small" observation
    tinym1_obs = tiny_obs - one
  end subroutine init_pcp

  subroutine pcpinfo_read
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    pcpinfo_read
!     prgmmr:    treadon     org: np23                date: 2003-09-25
!
! abstract:  read text file containing information (satellite id, error, 
!            usage flags) for precipitation rate observations.  This
!            routine also reads (optional) precipitation rate bias 
!            coefficients
!
! program history log:
!   2003-09-25  treadon
!   2004-05-13  treadon, documentation
!   2004-08-04  treadon - add only on use declarations; add intent in/out
!   2005-10-11  treadon - change pcpinfo read to free format
!   2008-04-29  safford - rm redundant uses
!   2008-10-10  derber  - flip indices for predxp
!   2010-05-29  todling - interface consistent w/ similar routines
!
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
    use obsmod, only: iout_pcp,use_limit
    implicit none

! Declare local varianbes
    logical lexist
    character(len=1):: cflg
    character(len=120) crecord
    integer(i_kind) lunin,i,j,k,istat,nlines,iusept
    character(len=20) :: nupcpt
    real(r_kind),dimension(npredp):: predrp

    data lunin / 48 /
    
!   Determine number of entries in pcp information file
    open(lunin,file='pcpinfo',form='formatted')
    j=0
    nlines=0
    read1:  do
       read(lunin,100,iostat=istat,end=120) cflg,crecord
       if (istat /= 0) exit
       nlines=nlines+1
       if (cflg == '!') cycle
       read(crecord,*) nupcpt,iusept
       if (iusept < use_limit) cycle
       j=j+1
    end do read1
120 continue
    if (istat>0) then
       write(6,*)'PCPINFO_READ:  ***ERROR*** error reading pcpinfo, istat=',istat
       close(lunin)
       write(6,*)'PCPINFO_READ:  stop program execution'
       call stop2(79)
    endif
    npcptype=j
    if(npcptype == 0)then
      close(lunin)
      return
    end if


!   Allocate arrays to hold pcp information
    allocate(nupcp(npcptype),iusep(npcptype),ibias(npcptype), &
         varchp(npcptype),gross_pcp(npcptype),b_pcp(npcptype),pg_pcp(npcptype))


!   All mpi tasks open and read pcpinfo information file.
!   Task mype_pcp writes information to pcp runtime file
    
    if (mype==mype_pcp) then
       open(iout_pcp)
       write(iout_pcp,110) npcptype
110    format('PCPINFO_READ:  npcptype=',1x,i6)
    endif
    rewind(lunin)
    j=0
    do k=1,nlines
       read(lunin,100)  cflg,crecord
       if (cflg == '!') cycle
       read(crecord,*) nupcpt,iusept
       if (mype==mype_pcp .and. iusept < use_limit) write(iout_pcp, *) &
                'line ignored due to use flag ',cflg,nupcpt,iusept
       if (iusept < use_limit) cycle
       j=j+1
       read(crecord,*) nupcp(j),iusep(j),ibias(j),&
            varchp(j),gross_pcp(j),b_pcp(j),pg_pcp(j)

       if (mype==mype_pcp)  write(iout_pcp,130) nupcp(j),&
            iusep(j),ibias(j),varchp(j),gross_pcp(j),b_pcp(j),pg_pcp(j)

    end do
    close(lunin)

100 format(a1,a120)
130 format(a20,' iusep = ',i2,   ' ibias = ',i2,' var   = ',&
         f7.3,' gross = ',f7.3,' b_pcp = ',f7.3, ' pg_pcp = ',f7.3)

    
    allocate(predxp(npredp,npcptype))
    do j=1,npcptype
       do i=1,npredp
          predxp(i,j)=zero
       end do
    end do

    inquire(file='pcpbias_in',exist=lexist)
    if (lexist) then
       open(lunin,file='pcpbias_in',form='formatted')
       if(mype==mype_pcp) &
            write(iout_pcp,*)'PCPINFO_READ:  read pcpbias coefs with npredp=',npredp
       read2: do
          read(lunin,'(I5,10f12.6)') i,(predrp(j),j=1,npredp)
          if (istat /= 0) exit
          do j=1,npredp
             predxp(j,i)=predrp(j)
          end do
          if(mype==mype_pcp) write(iout_pcp,140) i,(predxp(j,i),j=1,npredp)
       end do read2
140    format(1x,'npcptype=',i3,10f12.6)
       close(lunin)
    else
       if (mype==mype_pcp) write(6,*)'PCPINFO_READ:  no pcpbias file.  set predxp=0.0'
    endif
    if (mype==mype_pcp) close(iout_pcp)
    
    return
  end subroutine pcpinfo_read

  subroutine pcpinfo_write
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    pcpinfo_write
!     prgmmr:    treadon     org: np23                date: 2003-09-25
!
! abstract:  write precipitation rate bias correction coefficients
!
! program history log:
!   2003-09-25  treadon
!   2004-05-13  treadon, documentation
!   2008-10-10  derber  - flip indices for predxp
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
    implicit none
    integer(i_kind) iobcof,ityp,ip

    iobcof=52
    open(iobcof,file='pcpbias_out',form='formatted')
    rewind iobcof
    do ityp=1,npcptype
       write(iobcof,'(I5,10f12.6)') ityp,(predxp(ip,ityp),ip=1,npredp)
    end do
    close(iobcof)
    return
  end subroutine pcpinfo_write

  subroutine create_pcp_random(iadate,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_pcp_random
!     prgmmr:    treadon     org: np23                date: 2003-09-25
!
! abstract:  generate random numbers for cloud selction application
!            in GFS convective parameterization (SASCNV)
!
! program history log:
!   2003-09-25  treadon
!   2004-05-13  treadon, documentation
!   2004-12-03  treadon - replace mpe_iscatterv (IBM extension) with
!                         standard mpi_scatterv
!   2005-12-12  treadon - remove IBM specific call to random_seed(generator)
!   2006-01-10  treadon - move myper inside routine
!   2013-10-25  todling - reposition ltosi and others to commvars
!
!   input argument list:
!      iadate - analysis date (year, month, day, hour, minute)
!      mype   - mpi task id 
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use gridmod, only: ijn_s,displs_s,itotsub,&
       lat2,lon2,nlat,nlon
    use general_commvars_mod, only: ltosj_s,ltosi_s
    use mpimod, only: mpi_comm_world,ierror,mpi_rtype,npe
    use mersenne_twister, only: random_setseed, random_number
    implicit none

! Declare passed variables
    integer(i_kind)             ,intent(in   ) :: mype
    integer(i_kind),dimension(5),intent(in   ) :: iadate    

! Declare local variables
    integer(i_kind) i,j,k,mm1,myper,iseed
    
    real(r_kind) rseed
    real(r_kind),allocatable,dimension(:):: rwork,rgrid1
    real(r_kind),allocatable,dimension(:,:):: rgrid2

! Compute random number for precipitation forward model.  
    mm1=mype+1
    allocate(xkt2d(lat2,lon2))
    allocate(rwork(itotsub))
    myper=npe-1
    if (mype==myper) then
       rseed = 1e6_r_kind*iadate(1) + 1e4_r_kind*iadate(2) + 1e2_r_kind*iadate(3) + iadate(4)
       iseed=rseed
       write(6,*)'CREATE_PCP_RANDOM:  iseed=',iseed
       call random_setseed(iseed)
       allocate(rgrid1(nlat*nlon),rgrid2(nlat,nlon))
       call random_number(rgrid1)
       k=0
       do j=1,nlon
          do i=1,nlat
             k=k+1
             rgrid2(i,j)=rgrid1(k)
          end do
       end do
       do k=1,itotsub
          i=ltosi_s(k); j=ltosj_s(k)
          rwork(k)=rgrid2(i,j)
       end do
       deallocate(rgrid1,rgrid2)
    endif
    call mpi_scatterv(rwork,ijn_s,displs_s,mpi_rtype,xkt2d,ijn_s(mm1),&
         mpi_rtype,myper,mpi_comm_world,ierror)
    deallocate(rwork)
    return
  end subroutine create_pcp_random


  subroutine destroy_pcp_random
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_pcp_random
!     prgmmr:    treadon     org: np23                date: 2003-09-25
!
! abstract:  deallocate array to contain random numbers for SASCNV
!
! program history log:
!   2003-09-25  treadon
!   2004-05-13  treadon, documentation
!   2013-10-24  todling, dealloc a number of arrays
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
     implicit none

     if(allocated(nupcp))     deallocate(nupcp)
     if(allocated(iusep))     deallocate(iusep)
     if(allocated(ibias))     deallocate(ibias)
     if(allocated(varchp))    deallocate(varchp)
     if(allocated(gross_pcp)) deallocate(gross_pcp)
     if(allocated(b_pcp))     deallocate(b_pcp)
     if(allocated(pg_pcp))    deallocate(pg_pcp)
     deallocate(xkt2d)
     return
  end subroutine destroy_pcp_random
  
end module pcpinfo
