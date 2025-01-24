module convinfo
!$$$ module documentation block
!           .      .    .                                       .
! module:   convinfo
!   prgmmr: derber          org: np2                date: 2005-02-08
!
! abstract:  This module contains variables and routines related
!            to the assimilation of conventional observations 
!
! program history log:
!   2005-02-08  derber  - original code - consolidated from read routines
!   2006-04-20  kistler  - extensions for thinning and bias corrections
!   2006-06-29  kistler  - ithin_conv,rmesh_conv moved added to convinfo file entry
!   2007-11-03       su  - add pmesh_conv 
!   2009-01-22  todling - add convinfo_initialized
!   2010-09-10  pagowski - add pm2_5
!   2013-08-20  s.liu - add reflectivity
!   2013-11-20     su - add ptime_conv as time dimension,and pmot_conv as
!                           parameter for the option to keep thinned data as
!                           monitored
!   2016-03-02  s.liu/carley - remove use_reflectivity and use i_gsdcldanal_type
!   2019-05-23     su - add ibeta and ikapa for new VQC parameters 

!
! Subroutines Included:
!   sub init_convinfo    - initialize conventional obs related variables
!   sub convinfo_read    - allocate arrays for and read in conventional info and bias coefs
!   sub convinfo_destroy - destroy conventional info arrays and bias coef vectors
!
!
! Variable Definitions:
!   def nconvtype      - number of input conventional types
!   def ictype         - observation type
!   def icsubtype      - observation subtype                           
!   def icuse          - use flag                                        
!   def ctwind         - time window (absolute value)            
!   def ncnumgrp       - cross validation parameter - number of groups
!   def ncgroup        - cross validation parameter - group to remove from data use
!   def ncmiter        - cross validation parameter - external iteration to introduce removed data
!   def cgross         - gross error parameter - gross error
!   def cermax         - gross error parameter - max error
!   def cermin         - gross error parameter - min error
!   def cvar_b         - variational quality control parameter -  b parameter
!   def cvar_pg        - variational quality control parameter -  pg parameter
!   def ithin_conv     - 0, no thinning, 1 - thinning
!   def rmesh_conv     - size of thinning mesh (km)
!   def pmesh_conv     - size of vertical thinning mesh 
!   def pmot_conv      - option to keep thinned data out
!   def ptime_conv     - option to add time dimension
!   def ibeta          -new VQC parameter
!   def ikapa          -new VQC parameter

!
!
!                        count,max # of coefs
!   def index_sub      - index to count subtypes of a type and the position in the bufr error table 

! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ enddocumentation block

  use kinds, only: r_kind,i_kind
  use constants, only: zero,one
  use obsmod, only: use_limit
  implicit none

! set default as private
  private
! set subroutines as public
  public :: init_convinfo
  public :: convinfo_read
  public :: ihave_pm2_5
  public :: convinfo_destroy
! set passed variables as public
  public :: icsubtype,ioctype,nconvtype,ictype,diag_conv,icuse

  public :: ncgroup,ncnumgrp,ncmiter,ctwind,cermax,pmesh_conv,rmesh_conv,ithin_conv,cvar_b,cvar_pg,pmot_conv,ptime_conv
  public :: cermin,cgross
  public :: use_prepb_satwnd
  public :: index_sub
  public :: id_drifter
  public :: id_ship
  public :: ec_amv_qc
  public :: ibeta,ikapa      ! for new variational QC 


  logical diag_conv
  logical :: ihave_pm2_5
  logical :: use_prepb_satwnd
  logical :: id_drifter
  logical :: id_ship
  logical :: ec_amv_qc=.true.
  integer(i_kind) nconvtype,mype_conv
  real(r_kind),allocatable,dimension(:)::ctwind,cgross,cermax,cermin,cvar_b,cvar_pg, &
           rmesh_conv,pmesh_conv,pmot_conv,ptime_conv
  integer(i_kind),allocatable,dimension(:):: ncmiter,ncgroup,ncnumgrp,icuse,ictype,icsubtype,&
           ithin_conv,index_sub,ibeta,ikapa
  character(len=16),allocatable,dimension(:)::ioctype

  logical,save :: convinfo_initialized=.false.

contains

  subroutine init_convinfo
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    init_convinfo --- Initialize parameters for conventional obs
!
!   prgrmmr:     kistler      org: np23                date: 2006-04-20
!
! abstract:      This routine sets default values for conventional obs
!
! program history log:
!   2008-06-04  safford -- add subprogram doc block
!   2008-09-05  lueken -- merged ed's changes into q1fy09 code
!   2011-08-27  todling -- add knob to allow using prepbufr SATWND
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

    implicit none

    diag_conv = .true.    ! .true.=generate conv obs diagnostic file
    mype_conv = 0         ! mpi task to collect and print conv obs use information 
    use_prepb_satwnd=.false.  ! allow use of satwind stored in prepbufr file
    id_drifter=.false.        ! modify KX of drifting buoys
    id_ship=.false.           ! modify KX of ships

    call init_pm2_5

  end subroutine init_convinfo

  subroutine convinfo_read
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    convinfo_read      read conventional information file
!     prgmmr:    derber    org: np2                date: 2006-02-08
!
! abstract:  This routine reads the conventional information file
!
! program history log:
!   2006-02-08  derber 
!   2006-04-20  kistler - extended to read conv biases
!   2008-06-04  safford - rm unused vars
!   2008-09-05  lueken - merged ed's changes into q1fy09 code
!   2009-01-22  todling - protect against non-initialized destroy call
!   2010-05-29  todling - interface consistent w/ similar routines
!   2014-07-10  carley  - add check to bypass blank lines in convinfo file
!   2023-05-09  s.vetra-carvalho - extended iotype to be 8 characters long to
!   allow for more descriptive observations names
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
    use gsi_io, only: verbose
    implicit none
    
    character(len=1)cflg
    character(len=8) iotype
    character(len=140) crecord
    integer(i_kind) lunin,i,nc,ier,istat
    integer(i_kind) nlines
    integer(i_kind) ictypet,icsubtypet,icuset
    integer(i_kind) idum
    logical print_verbose

    print_verbose=.false.
    if(verbose)print_verbose=.true.
    lunin = 47
    open(lunin,file='convinfo',form='formatted')
    rewind(lunin)
    nconvtype=0
    nlines=0
    read1: do
       cflg=' '
       iotype='        '
       read(lunin,1030,iostat=istat,end=1130)cflg,iotype,crecord
1030   format(a1,a8,1x,a140)
       if (istat /= 0) exit
       nlines=nlines+1
       if(cflg == '!')cycle
       if (cflg==' '.and.iotype=='        ') then
         if(print_verbose)write(6,*) 'Encountered a blank line in convinfo file at line number: ',nlines,' skipping!'
         cycle
       end if
       read(crecord,*)ictypet,icsubtypet,icuset
       if (icuset < use_limit) cycle
       nconvtype=nconvtype+1
    enddo read1
1130 continue
    if (istat>0) then
       write(6,*)'CONVINFO_READ:  ***ERROR*** error reading convinfo, istat=',istat
       close(lunin)
       write(6,*)'CONVINFO_READ:  stop program execution'
       call stop2(79)
    endif

    if(nconvtype == 0) then
       write(6,*) 'CONVINFO_READ: NO CONVENTIONAL DATA USED'
       return
    endif
  
    allocate(ctwind(nconvtype),cgross(nconvtype),cermax(nconvtype),cermin(nconvtype), &
             cvar_b(nconvtype),cvar_pg(nconvtype),ncmiter(nconvtype),ncgroup(nconvtype), &
             ncnumgrp(nconvtype),icuse(nconvtype),ictype(nconvtype),icsubtype(nconvtype), &
             ioctype(nconvtype), index_sub(nconvtype),& 
             ithin_conv(nconvtype),rmesh_conv(nconvtype),pmesh_conv(nconvtype),&
             pmot_conv(nconvtype),ptime_conv(nconvtype),ibeta(nconvtype),ikapa(nconvtype),  &
             stat=ier )
    if ( ier /= 0 )  then
       write(6,*) 'CONVINFO_READ: allocate 1 failed' 
       call stop2(48)
    endif
    do i=1,nconvtype
       ithin_conv(i)=0                ! 0=no thinning
       rmesh_conv(i)=99999.0_r_kind
       pmesh_conv(i)=zero
       index_sub(i)=2
       pmot_conv(i)=zero
       ptime_conv(i)=zero
       ibeta(i)=0
       ikapa(i)=0
    enddo
    nc=0

    rewind(lunin)

    do i=1,nlines
       cflg=' '
       iotype='        '
       read(lunin,1030)cflg,iotype,crecord
       if (cflg==' '.and.iotype=='        ') then
         if(print_verbose)write(6,*) 'Encountered a blank line in convinfo file at line number: ',i,' skipping!'
         cycle
       end if
       if(cflg == '!')cycle
       read(crecord,*)ictypet,icsubtypet,icuset
       if(icuset < use_limit)then
         if (mype==0) write(6, *) 'line ignored in convinfo due to use flag ',&
                cflg,iotype,ictypet,icsubtypet,icuset
         cycle
       end if
       nc=nc+1
       ioctype(nc)=iotype
           !otype   type isub iuse twindow numgrp ngroup nmiter gross ermax ermin var_b var_pg ithin rmesh pmesh npred pmot ptime
           !ps       120    0    1     3.0      0      0      0   5.0   3.0   1.0  10.0  0.000 0 99999.    5
           !ioctype(nc),
           !  ictype(nc),
           !     icsubtype(nc),
           !              icuse(nc),
           !                     ctwind(nc),
           !                         ncnumgrp(nc),

       read(crecord,*,iostat=istat)ictype(nc),icsubtype(nc),icuse(nc),ctwind(nc),ncnumgrp(nc), &
          ncgroup(nc),ncmiter(nc),cgross(nc),cermax(nc),cermin(nc),cvar_b(nc),cvar_pg(nc), &
          ithin_conv(nc),rmesh_conv(nc),pmesh_conv(nc),idum,pmot_conv(nc),ptime_conv(nc),ibeta(nc),ikapa(nc)
       if(istat /=0) then
         read(crecord,*,iostat=istat)ictype(nc),icsubtype(nc),icuse(nc),ctwind(nc),ncnumgrp(nc), &
          ncgroup(nc),ncmiter(nc),cgross(nc),cermax(nc),cermin(nc),cvar_b(nc),cvar_pg(nc), &
          ithin_conv(nc),rmesh_conv(nc),pmesh_conv(nc),idum,pmot_conv(nc),ptime_conv(nc)
       endif
          if(nc >=2 )then
            if(trim(ioctype(nc))==trim(ioctype(nc-1)) .and. ictype(nc)==ictype(nc-1)) then
               index_sub(nc)=index_sub(nc-1)+1
            endif
          endif
       if(print_verbose .and. mype == 0)write(6,1031)ioctype(nc),ictype(nc),icsubtype(nc),icuse(nc),ctwind(nc),ncnumgrp(nc), &
          ncgroup(nc),ncmiter(nc),cgross(nc),cermax(nc),cermin(nc),cvar_b(nc),cvar_pg(nc), &
          ithin_conv(nc),rmesh_conv(nc),pmesh_conv(nc),idum,pmot_conv(nc),ptime_conv(nc),index_sub(nc),ibeta(nc),ikapa(nc)
1031   format('READ_CONVINFO: ',a8,1x,i3,1x,i4,1x,i2,1x,g13.6,1x,3(I3,1x),5g13.6,i5,2g13.6,i5,2g13.6,3i5)
    enddo

    close(lunin)
    convinfo_initialized=.true.
    
    return
  end subroutine convinfo_read


  subroutine convinfo_destroy
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    convinfo_destroy      destroy conventional information file
!     prgmmr:    derber    org: np2                date: 2006-02-08
!
! abstract:  This routine destroys arrays from convinfo file
!
! program history log:
!   2006-02-08  derber 
!   2006-04-20  kistler - extended to read conv biases
!   2009-01-22  todling - protect against non-initialized destroy call
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

    integer(i_kind) ier

    if(.not.convinfo_initialized) return
    deallocate(ctwind,cgross,cermax,cermin, &
             cvar_b,cvar_pg,ncmiter,ncgroup, &
             ncnumgrp,icuse,ictype,icsubtype, &
             ioctype,index_sub, & 
             ithin_conv,rmesh_conv,pmesh_conv, &
             pmot_conv,ptime_conv,ibeta,ikapa, &
             stat=ier )
    if ( ier /= 0 )  then
       write(6,*) 'CONVINFO_DESTROY: deallocate  failed' 
       call stop2(48)
    endif

    return
  end subroutine convinfo_destroy

  subroutine init_pm2_5
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_pm2_5     initialize parameters for pm2_5 data
!     prgmmr:    pagowski                      date: 2010-12-14
!
! abstract:  This routine sets default values for variables used in 
!            the pm2_5 processing routines
!
! program history log:
!   2010-10-06  pagowski - check chem-bundle for presence of pm2_5
!   based on  coinfo.f90
!   2010-05-29  todling - check chem-bundle for presence of CO
 

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
    use gsi_chemguess_mod, only: gsi_chemguess_get
    implicit none
    integer(i_kind) :: ipm2_5,ier

    call gsi_chemguess_get ('var::pm2_5', ipm2_5, ier )
    ihave_pm2_5=(ipm2_5 > 0)                  ! .t. when pm2_5 present in state-vector

  end subroutine init_pm2_5

end module convinfo

