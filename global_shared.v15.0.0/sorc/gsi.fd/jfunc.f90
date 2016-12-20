module jfunc
!$$$   module documentation block
!                .      .    .                                       .
! module:    jfunc
!   prgmmr: treadon          org: np23                date: 2003-11-24
!
! abstract: module containing variables used in inner loop minimzation
!           NOTE: it is ok for jfunc to depend on setting related to B,
!                 but not the other way around.
!
! program history log:
!   2003-11-24  treadon
!   2004-05-18  kleist, documentation
!   2004-10-06  kleist, create separate control vector for u,v
!   2004-10-15  parrish, add outer iteration variable for nonlinear qc
!   2004-12-23  treadon - add logical flags first and last
!   2005-02-23  wu - add qoption, dqdt,dqdrh,dqdp and varq for norm RH
!   2005-03-28  wu - replace mlath with mlat             
!   2005-06-03  parrish - add logical switch_on_derivatives
!   2005-09-29  kleist - add pointers for time derivatives
!   2005-11-21  kleist - expand time deriv pointers for tracer tendencies
!   2005-11-29  derber - fix bug in restart
!   2006-02-02  treadon - remove prsi_oz (use ges_prsi array)
!   2006-08-30  zhang,b - add bias correction control parameter
!   2007-03-13  derber - remove qsinv2 and add rhgues
!   2007-04-13  tremolet - use control vectors
!   2008-05-22  guo, j - merge GMAO MERRA changes with NCEP 2008-04-22
!                      - defer GMAO diurnal bias correction changes.
!   2008-12-01  todling - bring in Tremolet's changes
!   2009-06-01  pondeca,sato - add tsensible initialization. used in 2dvar mode only
!   2009-06-01  pondeca - add lgschmidt initalization. this variable controls the B-norm
!                         re-orthogonalization of the gradx vectors in 2dvar mode
!   2010-02-20  parrish - add change to get correct nval_len when using hybrid ensemble with dual resolution.
!   2010-02-20  zhu     - add nrf_levb and nrf_leve
!   2010-03-23  derber  - remove rhgues (not used)
!   2010-03-25  zhu     - add pointer_state
!   2010-05-12  todling - use gsi_bundle for state vector; remove pointer_state
!   2010-05-12  todling - replace some existing subdomain pointers w/ nsubwhalo/nsubnhalo
!                       - declare all variables coming from use statements
!   2010-05-20  todling - move nrf_levb and nrf_leve to control_vector where they belong
!   2010-08-15  gu/todling - add pseudo-q2 options
!   2013-05-20  zhu     - add ntclen for aircraft temperature bias correction aircraft_t_bc=.true. 
!                         or aircraft_t_bc_pof=.true.
!   2013-10-30  jung    - added logical clip_supersaturation
!   2013-12-10  zhu     - add variables varcw and cwoption
!   2014-03-19  pondeca - add factw10m
!   2014-05-07  pondeca - add facthowv
!   2014-06-18  carley/zhu - add lcbas and tcamt
!   2015-07-10  pondeca - add factcldch
!
! Subroutines Included:
!   sub init_jfunc           - set defaults for cost function variables
!   sub create_jfunc         - allocate cost function arrays 
!   sub destroy_jfunc        - deallocate cost function arrays
!   anav_info                - control variables information
!   sub read_guess_solution  - read guess solution
!   sub write_guess_solution - write guess solution
!   sub strip2               - strip off halo from subdomain arrays
!   sub set_pointer          - set location indices for components of vectors
!
! remarks: variable definitions below
!   def first      - logical flag = .true. on first outer iteration
!   def last       - logical flag = .true. following last outer iteration
!   def switch_on_derivatives - .t. = generate horizontal derivatives
!   def iout_iter  - output file number for iteration information
!   def miter      - number of outer iterations
!   def qoption    - option of q analysis variable; 1:q/qsatg 2:norm RH
!   def iguess     - flag for guess solution
!   def biascor    - background error bias correction coefficient 
!   def bcoption   - 0=ibc (no bias correction to bkg); 1= sbc(original implementation)
!   def diurnalbc  - 1= diurnal bias; 0= persistent bias
!   def niter      - number of inner interations (for each other iter.)
!   def niter_no_qc- number of inner interations without nonlinear qc (for each outer iter.)
!   def jiter      - outer iteration counter
!   def jiterstart - first outloop iteration number
!   def jiterend   - last outloop iteration number
!   def iter       - do loop iteration integer
!   def nclen      - length of control (x,y) vectors
!   def diag_precon- logical, if true do  preconditioning
!   def step_start - initial stepsize
!   def nvals_levs - number of 2d (x/y) state-vector variables
!   def nvals_len  - number of 2d state-vector variables * subdomain size (with buffer)
!   def nval_levs  - number of 2d (x/y) control-vector variables
!   def nval_len   - number of 2d control-vector variables * subdomain size (with buffer)
!   def l_foto     - option for foto
!   def print_diag_pcg - option for turning on GMAO diagnostics in pcgsoi
!   def tsensible  - option to use sensible temperature as the control variable. applicable
!                    to the 2dvar mode only
!   def lgschmidt  - option to re-biorthogonalyze the gradx and grady vectors during the
!                    inner iteration using the modified gram-schmidt method. useful for
!                    estimating the analysis error via the projection method. 
!
!   def ntracer    - total number of tracer variables
!   def nrft       - total number of time tendencies for upper level control variables
!   def nrft_      - order of time tendencies for 3d control variables
!   def R_option   - Option to use variable correlation length for lcbas based on data
!                    density - follows Hayden and Purser (1995) (twodvar_regional only)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use control_vectors, only: nc2d,nc3d,mvars
  use control_vectors, only: nrf,nrf_3d,nrf_var
  use control_vectors, only: allocate_cv,deallocate_cv
  use control_vectors, only: control_vector
  use control_vectors, only: assignment(=)
  use control_vectors, only: setup_control_vectors
  use control_vectors, only: cvars3d
  use state_vectors, only: setup_state_vectors
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundle
  use mpeu_util, only: getindex
  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_jfunc
  public :: create_jfunc
  public :: destroy_jfunc
  public :: read_guess_solution
  public :: write_guess_solution
  public :: strip2
  public :: set_pointer
  public :: set_sqrt_2dsize
! set passed variables to public
  public :: nrclen,npclen,nsclen,ntclen,qoption,nval_lenz,tendsflag,tsensible,cwoption,varcw
  public :: switch_on_derivatives,jiterend,jiterstart,jiter,iter,niter,miter
  public :: diurnalbc,bcoption,biascor,nval2d,dhat_dt,xhat_dt,l_foto,xhatsave,first
  public :: factqmax,factqmin,clip_supersaturation,last,yhatsave,nvals_len,nval_levs,iout_iter,nclen
  public :: niter_no_qc,print_diag_pcg,lgschmidt,penorig,gnormorig,iguess
  public :: factg,factv,factp,factl,R_option,factw10m,facthowv,factcldch,diag_precon,step_start
  public :: pseudo_q2
  public :: varq

  logical first,last,switch_on_derivatives,tendsflag,l_foto,print_diag_pcg,tsensible,lgschmidt,diag_precon
  logical clip_supersaturation,R_option
  logical pseudo_q2
  integer(i_kind) iout_iter,miter,iguess,nclen,qoption,cwoption
  integer(i_kind) jiter,jiterstart,jiterend,iter
  integer(i_kind) nvals_len,nvals_levs
  integer(i_kind) nval_len,nval_lenz,nval_levs
  integer(i_kind) nclen1,nclen2,nrclen,nsclen,npclen,ntclen
  integer(i_kind) nval2d,nclenz

  integer(i_kind),dimension(0:50):: niter,niter_no_qc
  real(r_kind) factqmax,factqmin,gnormorig,penorig,biascor,diurnalbc,factg,factv,factp,factl, & 
               factw10m,facthowv,factcldch,step_start
  integer(i_kind) bcoption
  real(r_kind),allocatable,dimension(:,:):: varq
  real(r_kind),allocatable,dimension(:,:):: varcw
  type(control_vector),save :: xhatsave,yhatsave
  type(gsi_bundle),save :: xhat_dt,dhat_dt

contains

  subroutine init_jfunc
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_jfunc
!   prgmmr: treadon          org: np23               date:  2003-11-24
!
! abstract: initialize cost function variables to defaults
!
! program history log:
!   2003-11-24  treadon
!   2004-05-18  kleist, documentation
!   2004-12-23  treadon - initialize first and last
!   2005-06-03  parrish - initialize switch_on_derivatives
!   2005-10-27  kleist  - initialize tendency flag
!   2006-08-30  zhang,b - initialize bias correction scheme
!   2008-05-12  safford - rm unused uses
!   2010-08-15  gu/todling - add pseudo-q2 options
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
    use constants, only: zero, one
    implicit none
    integer(i_kind) i

    first = .true.
    last  = .false.
    switch_on_derivatives=.false.
    tendsflag=.false.
    l_foto=.false.
    print_diag_pcg=.false.
    tsensible=.false.
    lgschmidt=.false.
    diag_precon=.false.
    step_start=1.e-4_r_kind
    R_option=.false.

    factqmin=zero
    factqmax=zero
    clip_supersaturation=.false.
    factg=zero
    factv=zero
    factp=zero
    factl=zero
    factw10m=zero
    facthowv=zero
    factcldch=zero
    iout_iter=220
    miter=1
    qoption=1
    cwoption=0
    pseudo_q2=.false.
    do i=0,50
       niter(i)=0
       niter_no_qc(i)=1000000
    end do
    jiterstart=1
    jiterend=1
    jiter=jiterstart
    biascor=-one        ! bias multiplicative coefficient
    diurnalbc=0         ! 1= diurnal bias; 0= persistent bias
    bcoption=1          ! 0=ibc; 1=sbc
    nclen=1
    nclenz=1

    penorig=zero
    gnormorig=zero

! iguess = -1  do not use guess file
! iguess =  0  write only guess file
! iguess =  1  read and write guess file
! iguess =  2  read only guess file

    iguess=1

    return
  end subroutine init_jfunc


  subroutine create_jfunc
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_jfunc
!   prgmmr: treadon          org: np23               date:  2003-11-24
!
! abstract: allocate memory for cost function variables
!
! program history log:
!   2003-11-24  treadon
!   2004-05-18  kleist, documentation
!   2004-07-28  treadon - simplify subroutine argument list
!   2005-03-28  wu - replace mlath with mlat, modify dim of varq 
!   2005-06-15  treadon - remove "use guess_grids"
!   2008-05-12  safford - rm unused uses
!   2013-10-25  todling - revisit variable initialization
!   2013-11-12  lueken - revisit logic around cwgues
!   2014-02-03  todling - CV length and B-dims here (no longer in observer)
!
!   input argument list:
!    mlat
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use constants, only: zero
    use gridmod, only: lat2,lon2,nsig,regional
    use m_berror_stats, only: berror_get_dims
    use m_berror_stats_reg, only: berror_get_dims_reg
    implicit none

    integer(i_kind) j,k
    integer(i_kind) msig,mlat,mlon 

!   Set length of control vector and other control vector constants
    call set_pointer

!   Allocate arrays used in minimization
    if(.not.regional)then                    ! If global, use msig, mlat, and mlon
       call berror_get_dims(msig,mlat,mlon)
    else                                     ! If regional, use msig and mlat only
       call berror_get_dims_reg(msig,mlat)
    endif

    call allocate_cv(xhatsave)
    call allocate_cv(yhatsave)
    xhatsave=zero
    yhatsave=zero

    if (getindex(cvars3d,'q')>0) then
        allocate(varq(1:mlat,1:nsig))
        do k=1,nsig
          do j=1,mlat
             varq(j,k)=zero
          end do
       end do
    endif

    allocate(varcw(1:mlat,1:nsig))
    do k=1,nsig
       do j=1,mlat
          varcw(j,k)=zero
       end do
    end do

    return
  end subroutine create_jfunc
    
  subroutine destroy_jfunc
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_jfunc
!   prgmmr: treadon          org: np23               date:  2003-11-24
!
! abstract: deallocate memory from cost function variables
!
! program history log:
!   2003-11-24  treadon
!   2004-05-18  kleist, documentation
!   2013-10-25  todling, revisit deallocs
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

    call deallocate_cv(xhatsave)
    call deallocate_cv(yhatsave)
    if(allocated(varq)) deallocate(varq)
    if(allocated(varcw)) deallocate(varcw)

    return
  end subroutine destroy_jfunc

  subroutine read_guess_solution(dirx,diry,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_guess_solution
!   prgmmr: treadon          org: np23               date:  2003-11-24
!
! abstract: read in guess solution
!
! program history log:
!   2003-11-24  treadon
!   2004-05-18  kleist, documentation
!   2005-05-05  treadon - read guess solution from 4-byte reals
!   2008-05-12  safford - rm unused uses and vars
!   2013-10-25  todling - reposition ltosi and others to commvars
!
!   input argument list:
!     mype   - mpi task id
!
!   output argument list:
!     dirx,diry
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use kinds, only: r_single
    use mpimod, only: ierror, mpi_comm_world,mpi_real4
    use gridmod, only: nlat,nlon,nsig,itotsub,&
         displs_s,ijn_s,latlon11,iglobal
    use general_commvars_mod, only: ltosi_s,ltosj_s
    use obsmod, only: iadate
    implicit none

    integer(i_kind)     ,intent(in   ) :: mype
    type(control_vector),intent(  out) :: dirx,diry

    integer(i_kind) i,k,mm1,myper,kk,i1,i2
    integer(i_kind) nlatg,nlong,nsigg
    integer(i_kind),dimension(5):: iadateg
    real(r_single),dimension(max(iglobal,itotsub)):: fieldx,fieldy
    real(r_single),dimension(nlat,nlon):: xhatsave_g,yhatsave_g
    real(r_single),dimension(nclen):: xhatsave_r4,yhatsave_r4
    
    jiterstart = 1
    mm1=mype+1
    myper=0

! Open unit to guess solution.  Read header.  If no header, file is
! empty and exit routine
    open(12,file='gesfile_in',form='unformatted')
    iadateg=0
    nlatg=0
    nlong=0
    nsigg=0
    read(12,end=1234)iadateg,nlatg,nlong,nsigg
    if(iadate(1) == iadateg(1) .and. iadate(2) == iadate(2) .and. &
       iadate(3) == iadateg(3) .and. iadate(4) == iadateg(4) .and. &
       iadate(5) == iadateg(5) .and. nlat == nlatg .and. &
       nlon == nlong .and. nsig == nsigg) then
       if(mype == 0) write(6,*)'READ_GUESS_SOLUTION:  read guess solution for ',&
                     iadateg,nlatg,nlong,nsigg
       jiterstart=0
         
! Let all tasks read gesfile_in to pick up bias correction (second read)

! Loop to read input guess fields.  After reading in each field & level,
! scatter the grid to the appropriate location in the xhat and yhatsave
! arrays.
       do k=1,nval_levs
          read(12,end=1236) xhatsave_g,yhatsave_g
          do kk=1,itotsub
             i1=ltosi_s(kk); i2=ltosj_s(kk)
             fieldx(kk)=xhatsave_g(i1,i2)
             fieldy(kk)=yhatsave_g(i1,i2)
          end do
          i=(k-1)*latlon11 + 1
          call mpi_scatterv(fieldx,ijn_s,displs_s,mpi_real4,&
                   xhatsave_r4(i),ijn_s(mm1),mpi_real4,myper,mpi_comm_world,ierror)
          call mpi_scatterv(fieldy,ijn_s,displs_s,mpi_real4,&
                   yhatsave_r4(i),ijn_s(mm1),mpi_real4,myper,mpi_comm_world,ierror)
       end do  !end do over nval_levs

!      Read radiance and precipitation bias correction terms
       read(12,end=1236) (xhatsave_r4(i),i=nclen1+1,nclen),(yhatsave_r4(i),i=nclen1+1,nclen)
       do i=1,nclen
          dirx%values(i)=real(xhatsave_r4(i),r_kind)
          diry%values(i)=real(yhatsave_r4(i),r_kind)
       end do

    else
       if(mype == 0) then
          write(6,*) 'READ_GUESS_SOLUTION:  INCOMPATABLE GUESS FILE, gesfile_in'
          write(6,*) 'READ_GUESS_SOLUTION:  iguess,iadate,iadateg=',iguess,iadate,iadateg
          write(6,*) 'READ_GUESS_SOLUTION:  nlat,nlatg,nlon,nlong,nsig,nsigg=',&
                      nlat,nlatg,nlon,nlong,nsig,nsigg
       end if
    endif
    close(12)
    return

! The guess file is empty.  Do not return an error code but print a message to
! standard out.
1234 continue
    if(mype == 0) then
       write(6,*) 'READ_GUESS_SOLUTION:  NO GUESS FILE, gesfile_in'
       write(6,*) 'READ_GUESS_SOLUTION:  iguess,iadate,iadateg=',iguess,iadate,iadateg
       write(6,*) 'READ_GUESS_SOLUTION:  nlat,nlatg,nlon,nlong,nsig,nsigg=',&
                   nlat,nlatg,nlon,nlong,nsig,nsigg
    end if
    close(12)
    return

! Error contition reading level or bias correction data.  Set error flag and
! return to the calling program.
1236 continue
    if (mype==0) write(6,*) 'READ_GUESS_SOLUTION:  ERROR in reading guess'
    close(12)
    call stop2(76)

    return
  end subroutine read_guess_solution
  
  subroutine write_guess_solution(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    write_guess_solution
!   prgmmr: treadon          org: np23               date:  2003-11-24
!
! abstract: write out guess solution (not from spectral forecast)
!
! program history log:
!   2003-11-24  treadon
!   2004-05-18  kleist, documentation
!   2005-03-10  treadon - remove iadate from calling list, access via obsmod
!   2005-05-05  treadon - write guess solution using 4-byte reals
!   2008-05-12  safford - rm unused uses
!   2008-12-13  todling - strip2 called w/ consistent interface
!   2013-10-25  todling - reposition ltosi and others to commvars
!
!   input argument list:
!     mype   - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use kinds, only: r_single
    use mpimod, only: ierror, mpi_comm_world, mpi_real4
    use gridmod, only: ijn,latlon11,displs_g,nsig,&
         nlat,nlon,lat1,lon1,itotsub,iglobal
    use general_commvars_mod, only: ltosj,ltosi 
    use obsmod, only: iadate
    use constants, only: zero
    implicit none

    integer(i_kind),intent(in   ) :: mype

    integer(i_kind) i,j,k,mm1,mypew,kk,i1,i2,ie,is
    real(r_single),dimension(lat1,lon1,2):: field
    real(r_single),dimension(max(iglobal,itotsub)):: fieldx,fieldy
    real(r_single),dimension(nlat,nlon):: xhatsave_g,yhatsave_g
    real(r_single),dimension(nrclen):: xhatsave4,yhatsave4

    mm1=mype+1
    mypew=0
    
! Write header record to output file
    if (mype==mypew) then
       open(51,file='gesfile_out',form='unformatted')
       write(51) iadate,nlat,nlon,nsig
    endif

! Loop over levels.  Gather guess solution and write to output
    do k=1,nval_levs
       ie=(k-1)*latlon11 + 1
       is=ie+latlon11
       call strip2(xhatsave%values(ie:is),yhatsave%values(ie:is),field)
       call mpi_gatherv(field(1,1,1),ijn(mm1),mpi_real4,&
            fieldx,ijn,displs_g,mpi_real4,mypew,&
            mpi_comm_world,ierror)
       call mpi_gatherv(field(1,1,2),ijn(mm1),mpi_real4,&
            fieldy,ijn,displs_g,mpi_real4,mypew,&
            mpi_comm_world,ierror)

! Transfer to global arrays
       do j=1,nlon
          do i=1,nlat
             xhatsave_g(i,j)=zero
             yhatsave_g(i,j)=zero
          end do
       end do
       do kk=1,iglobal
          i1=ltosi(kk); i2=ltosj(kk)
          xhatsave_g(i1,i2)=fieldx(kk)
          yhatsave_g(i1,i2)=fieldy(kk)
       end do

! Write level record
       if (mype==mypew) write(51) xhatsave_g,yhatsave_g
    end do  !end do over nval_levs

! Write radiance and precipitation bias correction terms to output file
    if (mype==mypew) then
       do i=1,nrclen
          xhatsave4(i)=xhatsave%values(nclen1+i)
          yhatsave4(i)=yhatsave%values(nclen1+i)
       end do
       write(51) (xhatsave4(i),i=1,nrclen),(yhatsave4(i),i=1,nrclen)
       close(51)
       write(6,*)'WRITE_GUESS_SOLUTION:  write guess solution for ',&
                  iadate,nlat,nlon,nsig
    endif

    return
  end subroutine write_guess_solution

    subroutine strip2(field_in1,field_in2,field_out)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    strip2
!   prgmmr: treadon          org: np23                date: 2003-11-24
!
! abstract: strip off halo from two subdomain arrays & combine into
!           single output array
!
! program history log:
!   2003-11-24  treadon
!   2004-05-18  kleist, documentation
!   2008-05-12  safford - rm unused uses
!
!   input argument list:
!     field_in1 - subdomain field one with halo
!     field_in2 - subdomain field two with halo
!
!   output argument list:
!     field_out - combined subdomain fields with halo stripped
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use kinds, only: r_single
    use gridmod, only: lat1,lon1,lat2,lon2
    implicit none

    real(r_single),dimension(lat1,lon1,2),intent(  out) :: field_out
    real(r_kind)  ,dimension(lat2,lon2)  ,intent(in   ) :: field_in1,field_in2

    integer(i_kind) i,j,jp1


    do j=1,lon1
       jp1 = j+1
       do i=1,lat1
          field_out(i,j,1)=field_in1(i+1,jp1)
          field_out(i,j,2)=field_in2(i+1,jp1)
       end do
    end do

    return
  end subroutine strip2

  subroutine set_pointer
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    set_pointer
!   prgmmr: treadon          org: np23                date: 2004-07-28
!
! abstract: Set length of control vector and other control 
!           vector constants
!
! program history log:
!   2004-07-28  treadon
!   2006-04-21  kleist - include pointers for more time tendency arrays
!   2008-12-04  todling - increase number of 3d fields from 6 to 8 
!   2009-09-16  parrish - add hybrid_ensemble connection in call to setup_control_vectors
!   2010-03-01  zhu     - add nrf_levb and nrf_leve, generalize nval_levs
!                       - generalize vector starting points such as nvpsm, nst2, and others
!   2010-05-23  todling - remove pointers such as nvpsm, nst2, and others (intro on 10/03/01)
!                       - move nrf_levb and nrf_leve to anberror where they are needed
!   2010-05-29  todling - generalized count for number of levels in state variables
!   2013-10-22  todling - revisit level count in view of changes to bundle
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
    use gridmod, only: lat1,lon1,latlon11,latlon1n,nsig,lat2,lon2
    use gridmod, only: nnnn1o,regional,nlat,nlon
    use radinfo, only: npred,jpch_rad
    use pcpinfo, only: npredp,npcptype
    use aircraftinfo, only: npredt,ntail,aircraft_t_bc_pof,aircraft_t_bc
    use state_vectors, only: ns3d,ns2d,levels
    use constants, only : max_varname_length
    use gsi_4dvar, only: nsubwin, lsqrtb
    use bias_predictors, only: setup_predictors
    use hybrid_ensemble_parameters, only: l_hyb_ens,n_ens,generate_ens,grd_ens,nval_lenz_en
    implicit none

    integer(i_kind) n_ensz,nval_lenz_tot,nval_lenz_enz

    nvals_levs=ns2d+sum(levels)
    nvals_len=nvals_levs*latlon11

    nval_levs=max(0,nc3d)*nsig+max(0,nc2d)
    nval_len=nval_levs*latlon11
    if(l_hyb_ens) then
       nval_len=nval_len+n_ens*nsig*grd_ens%latlon11
    end if
    nsclen=npred*jpch_rad
    npclen=npredp*npcptype
    if (aircraft_t_bc_pof .or. aircraft_t_bc) then 
       ntclen=npredt*ntail
    else
       ntclen=0
    end if
    nclen=nsubwin*nval_len+nsclen+npclen+ntclen
    nrclen=nsclen+npclen+ntclen
    nclen1=nclen-nrclen
    nclen2=nclen1+nsclen
  
    n_ensz=0
    nval_lenz_enz=0
    if(lsqrtb.or.(l_hyb_ens.and.generate_ens)) then
       if(regional) then
          nval2d=nlat*nlon*3
       else
          call set_sqrt_2dsize(nval2d)
       end if
       nval_lenz=nval2d*nnnn1o
       nval_lenz_tot=nval_lenz
       if(lsqrtb.and.l_hyb_ens) then
          n_ensz=n_ens
          nval_lenz_enz=nval_lenz_en
          nval_lenz_tot=nval_lenz+n_ensz*nval_lenz_enz
       endif
       nclenz=nsubwin*nval_lenz_tot+nsclen+npclen+ntclen
    else
       nval2d=latlon11
    end if

    if (lsqrtb) then
       CALL setup_control_vectors(nsig,lat2,lon2,latlon11,latlon1n, &
                                  nsclen,npclen,ntclen,nclenz,nsubwin,nval_lenz,lsqrtb,n_ensz, &
                                  nval_lenz_enz)
    else
       CALL setup_control_vectors(nsig,lat2,lon2,latlon11,latlon1n, &
                                  nsclen,npclen,ntclen,nclen,nsubwin,nval_len,lsqrtb,n_ens, &
                                  nval_lenz_enz)
    endif
    CALL setup_predictors(nrclen,nsclen,npclen,ntclen)
    CALL setup_state_vectors(latlon11,latlon1n,nvals_len,lat2,lon2,nsig)

  end subroutine set_pointer

  subroutine set_sqrt_2dsize(ndim2d)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    set_sqrt_2dsize
!   prgmmr: todling          org: np23                date: 2011-09-05
!
! abstract: Calculates size of 2d-component of control vector in sqrt-B
!           case. This being an independent call allows using ckgcov
!           within context of B-precond.
!
! program history log:
!   2011-09-05  todling - move as independent piece out of set_pointer
!
!   input argument list:
!
!   output argument list:
!     ndim2d - size of 2d component of control vector in sqrt-B case
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
  use kinds, only: i_kind
  use gridmod, only: nlat,nlon
  implicit none
  integer(i_kind),intent(out):: ndim2d
  integer(i_kind) nx,ny,mr,nr,nf
!           following lifted from subroutine create_berror_vars in module berror.f90
!            inserted because create_berror_vars called after this routine
     nx=nlon*3/2
     nx=nx/2*2
     ny=nlat*8/9
     ny=ny/2*2
     if(mod(nlat,2)/=0)ny=ny+1
     mr=0
     nr=nlat/4
     nf=nr
     ndim2d=(ny*nx + 2*(2*nf+1)*(2*nf+1))*3
  end subroutine set_sqrt_2dsize

end module jfunc
