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

!
! Subroutines Included:
!   sub init_convinfo    - initialize conventional obs related variables
!   sub convinfo_read    - allocate arrays for and read in conventional info and bias coefs
!   sub convinfo_write   -
!   sub conv_bias_print  - write out conventional obs bias coefs
!   sub convinfo_destroy - destroy conventional info arrays and bias coef vectors
!
!
! Variable Definitions:
!   def nconvtype      - number of input conventional types
!   def nconvtype_ps   - number of input conventional type ps
!   def nconvtype_t    - number of input conventional type t
!   def nconvtype_spd  - number of input conventional type spd
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
!
!
!   def predx_conv     - conv obs bias correction coefficients: t,uv,q,ps,spd,sst,pw,pm2_5,pm10
!                        count,max # of coefs
!   def npred_conv_max - maximum number of conv ob bias correction coefs 
!   def npred_conv     - conv ob bias coef count
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
  public :: convinfo_write
  public :: conv_bias_print
  public :: convinfo_destroy
! set passed variables as public
  public :: icsubtype,ioctype,nconvtype,ictype,diag_conv,icuse,conv_bias_spd,conv_bias_t,stndev_conv_ps
  public :: stndev_conv_spd,stndev_conv_t,id_bias_ps,npred_conv_max,id_bias_t,conv_bias_ps,id_bias_spd
  public :: conv_bias_pm2_5,id_bias_pm2_5,ihave_pm2_5
  public :: conv_bias_pm10,id_bias_pm10

  public :: ncgroup,ncnumgrp,ncmiter,ctwind,cermax,pmesh_conv,rmesh_conv,ithin_conv,cvar_b,cvar_pg,pmot_conv,ptime_conv
  public :: cermin,cgross
  public :: use_prepb_satwnd
  public :: index_sub

  logical diag_conv
  logical :: ihave_pm2_5
  logical :: use_prepb_satwnd
  integer(i_kind) nconvtype,mype_conv
  real(r_kind),allocatable,dimension(:)::ctwind,cgross,cermax,cermin,cvar_b,cvar_pg, &
		                          rmesh_conv,pmesh_conv,stndev_conv,pmot_conv,ptime_conv
  integer(i_kind),allocatable,dimension(:):: ncmiter,ncgroup,ncnumgrp,icuse,ictype,icsubtype,&
                                             ithin_conv,npred_conv,index_sub
  character(len=16),allocatable,dimension(:)::ioctype

  real(r_kind),allocatable,dimension(:,:) :: predx_conv
  integer(i_kind)  npred_conv_max
  integer(i_kind)  nconvtype_ps,nconvtype_t,nconvtype_spd
  integer(i_kind)  id_bias_ps,id_bias_t,id_bias_spd,id_bias_pm2_5,id_bias_pm10
  real(r_kind)     conv_bias_ps,conv_bias_t,conv_bias_spd, &
       conv_bias_pm2_5,conv_bias_pm10, &
       stndev_conv_ps,stndev_conv_t,stndev_conv_spd

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

    npred_conv_max=0      ! max of all conv bias predictors 
    nconvtype_ps  =0
    nconvtype_t   =0
    nconvtype_spd =0
    stndev_conv_t =one
    stndev_conv_ps =one
    stndev_conv_spd =one

    id_bias_ps = 0            ! prepbufr id to have conv_bias added for testing 
    id_bias_t  = 0            ! prepbufr id to have conv_bias added for testing 
    id_bias_spd= 120          ! prepbufr id to have conv_bias added for testing 

    id_bias_pm2_5= 0 
    id_bias_pm10= 0

    conv_bias_ps = zero       ! magnitude of ps bias(mb)
    conv_bias_t  = zero       ! magnitude of t  bias(deg K)
    conv_bias_spd= zero       ! magnitude of spd bias(m/sec)
				
    conv_bias_pm2_5= zero
    conv_bias_pm10= zero

    use_prepb_satwnd=.false.  ! allow use of satwind stored in prepbufr file

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
    implicit none
    
    character(len=1)cflg
    character(len=16) cob
    character(len=11) bias_file_in/'convbias_in'/
    character(len=7) iotype
    character(len=120) crecord
    integer(i_kind) lunin,i,n,nc,ier,istat
    integer(i_kind) iunit,iob,isub,np,nlines
    integer(i_kind) ictypet,icsubtypet,icuset

    lunin = 47
    open(lunin,file='convinfo',form='formatted')
    rewind(lunin)
    nconvtype=0
    nlines=0
    read1: do
       cflg=' '
       iotype='       '
       read(lunin,1030,iostat=istat,end=1130)cflg,iotype,crecord
1030   format(a1,a7,2x,a120)
       if (istat /= 0) exit
       nlines=nlines+1
       if(cflg == '!')cycle
       if (cflg==' '.and.iotype=='       ') then
         write(6,*) 'Encountered a blank line in convinfo file at line number: ',nlines,' skipping!'
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
             npred_conv(nconvtype),pmot_conv(nconvtype),ptime_conv(nconvtype),  &
             stndev_conv(nconvtype), &
             stat=ier )
    if ( ier /= 0 )  then
       write(6,*) 'CONVINFO_READ: allocate 1 failed' 
       call stop2(48)
    endif
    do i=1,nconvtype
       ithin_conv(i)=0                ! 0=no thinning
       npred_conv(i)=0                ! number of bias predictors
       rmesh_conv(i)=99999.0_r_kind
       pmesh_conv(i)=zero
       stndev_conv(i)=one
       index_sub(i)=2
       pmot_conv(i)=zero
       ptime_conv(i)=zero
    enddo
    nc=0

    if(nconvtype*npred_conv_max>0) then
       allocate(predx_conv (nconvtype,npred_conv_max))
       predx_conv=zero
    endif

    rewind(lunin)

! open convbias.in file
    if (npred_conv_max > 0 ) then
       iunit=49
       open(iunit,file=bias_file_in,form='formatted',iostat=ier)
       if (ier /= 0) then  
          write(6,*) 'CONVINFO_READ: open error = ',ier,' for ',bias_file_in
          call stop2(48)
       endif
    endif

    do i=1,nlines
       cflg=' '
       iotype='       '
       read(lunin,1030)cflg,iotype,crecord
       if (cflg==' '.and.iotype=='       ') then
         write(6,*) 'Encountered a blank line in convinfo file at line number: ',i,' skipping!'
         cycle
       end if
       if(cflg == '!')cycle
       read(crecord,*)ictypet,icsubtypet,icuset
       if (mype==0 .and. icuset < use_limit) write(6, *) &
                'line ignored in convinfo due to use flag ',cflg,iotype,ictypet,icsubtypet,icuset
       if(icuset < use_limit)cycle
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

       read(crecord,*)ictype(nc),icsubtype(nc),icuse(nc),ctwind(nc),ncnumgrp(nc), &
          ncgroup(nc),ncmiter(nc),cgross(nc),cermax(nc),cermin(nc),cvar_b(nc),cvar_pg(nc), &
          ithin_conv(nc),rmesh_conv(nc),pmesh_conv(nc),npred_conv(nc),pmot_conv(nc),ptime_conv(nc)
          if(nc >=2 )then
            if(trim(ioctype(nc))==trim(ioctype(nc-1)) .and. ictype(nc)==ictype(nc-1)) then
               index_sub(nc)=index_sub(nc-1)+1
            endif
          endif
       if(mype == 0)write(6,1031)ioctype(nc),ictype(nc),icsubtype(nc),icuse(nc),ctwind(nc),ncnumgrp(nc), &
          ncgroup(nc),ncmiter(nc),cgross(nc),cermax(nc),cermin(nc),cvar_b(nc),cvar_pg(nc), &
          ithin_conv(nc),rmesh_conv(nc),pmesh_conv(nc),npred_conv(nc),pmot_conv(nc),ptime_conv(nc),index_sub(nc)
1031   format('READ_CONVINFO: ',a7,1x,i3,1x,i4,1x,i2,1x,g13.6,1x,3(I3,1x),5g13.6,i5,2g13.6,i5,2g13.6,i5)
       if (npred_conv_max > 0 ) then
          read(iunit,*,iostat=ier) cob,iob,isub,np,(predx_conv(nc,n),n=1,np)
          if (ier /= 0 ) then
             write(6,*) 'CONVINFO_READ:,i/o error ',iunit,' reading convinfo file',ier
             call stop2(48)
          endif
          if ( trim(cob) /= trim(ioctype(nc)) .or. &
             iob /= ictype(nc) .or. &		
             isub /= icsubtype(nc) .or. &
             np /= npred_conv(nc)) then
             write(6,*) 'CONVINFO_READ: convbias.in mismatch: ',& 
                 nc,ioctype(nc),ictype(nc),icsubtype(nc),npred_conv(nc),cob,iob,isub,np
             call stop2(48)
          endif
          stndev_conv(nc)=one
          select case (cob) 
             case('ps')
                nconvtype_ps=nconvtype_ps+1
                stndev_conv(nc)=stndev_conv_ps
             case('t')
                nconvtype_t=nconvtype_t+1
                stndev_conv(nc)=stndev_conv_t
             case('spd')
                nconvtype_spd=nconvtype_spd+1
                stndev_conv(nc)=stndev_conv_spd
          end select
       endif
    enddo

    if (npred_conv_max > 0) call conv_bias_print
	
    close(lunin)
    convinfo_initialized=.true.
    
    return
  end subroutine convinfo_read


  subroutine convinfo_write
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    convinfo_write
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-06-04  safford -- add subprogram doc block, rm unused vars
!   2008-09-05  lueken -- merged ed's changes into q1fy09 code
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

    implicit none

    integer(i_kind) np,n,nc,ier
    integer (i_kind) iunit

    iunit=53
    open(iunit,file='convbias_out',form='formatted')
    rewind iunit
    do nc=1,nconvtype
       np=npred_conv(nc)
       write(iunit,*,iostat=ier) ioctype(nc),ictype(nc),icsubtype(nc), np, (predx_conv(nc,n),n=1,np)
       if (ier /= 0) then
          write(6,*) 'CONVINFO_WRITE:,i/o error ',iunit,' writing convbias_out file '
          call stop2(48)
       endif
    enddo
    call conv_bias_print
    return

  end subroutine convinfo_write


  subroutine conv_bias_print
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    conv_bias_print
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-06-04  safford -- add subprogram doc block
!   2008-09-05  lueken -- merged ed's changes into q1fy09 code
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
    implicit none

    integer (i_kind) nc,np

    do nc=1,nconvtype
       if (trim(ioctype(nc)) == 'ps') then
          np=npred_conv(nc)
       endif
       if (trim(ioctype(nc)) == 't') then
          np=npred_conv(nc)
       endif
    enddo
  end subroutine conv_bias_print


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
             npred_conv,pmot_conv,ptime_conv, &
             stndev_conv, &
             stat=ier )
    if ( ier /= 0 )  then
       write(6,*) 'CONVINFO_DESTROY: deallocate  failed' 
       call stop2(48)
    endif
    if(allocated(predx_conv)) then
       deallocate(predx_conv ,stat=ier)
       if ( ier /= 0 )  then
          write(6,*) 'CONVINFO_DESTROY: deallocate predx_conv  failed' 
          call stop2(48)
       endif
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
    use mpimod, only: npe              ! contains the number of mpi tasks, variable "npe"
    use gsi_chemguess_mod, only: gsi_chemguess_get
    implicit none
    integer(i_kind) :: ipm2_5,ier

    call gsi_chemguess_get ('var::pm2_5', ipm2_5, ier )
    ihave_pm2_5=(ipm2_5 > 0)                  ! .t. when pm2_5 present in state-vector

  end subroutine init_pm2_5

end module convinfo

