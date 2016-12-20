module berror
!$$$   module documentation block
!                .      .    .                                       .
! module:    berror    contains information pertaining to the background error
!   prgmmr: kleist           org: np20                date: 2004-01-01
!
! abstract: contains information pertaining to the background error
!
! program history log:
!   2004-01-01  kleist
!   2004-11-03  treadon - add horizontal scale weighting factors
!   2004-11-16  treadon - add longitude dimension to variance (dssv) array
!   2004-11-22  derber  - modify horizontal table definition
!   2005-01-22  parrish - clean up code by using module balmod
!   2005-02-23  wu      - add subroutine set_nrh_var
!   2005-05-24  pondeca - accommodate 2dvar only surface analysis option
!                         in set_predictors_var and create_berror_vars_reg
!   2005-06-06  wu - initialize logical fstat
!   2005-10-06  wu - set ozmz values in regional mode
!   2005-11-16  wgu - set nmix=nr+1+(ny-nlat)/2 to make sure
!               nmix+nrmxb=nr in routine smoothrf no matter what
!               number nlat is
!   2005-11-16  wgu - set ny to be odd number if nlat is odd number
!               to support GMAO grid 
!   2005-11-29  derber -  remove set_ozone_var (included in prewgt_reg and prewgt)
!   2006-01-09  derber - remove set_nrh_var and move capability to compute_derived
!   2006-04-21  kleist - add capability to perturb background error parameters 
!   2006-11-30  todling - add fpsproj to control full nsig projection onto ps
!   2007-03-13  derber - add qvar3d array to allow qoption=2 to work similar to others
!   2007-07-03  kleist - add variables for flow-dependent background error variances
!   2010-04-25  zhu - add handling of option newpc4pred for new pre-conditioning of predictors
!   2010-06-01  todling - revist as,tsfc_sdv to allow alloc depending on size of CVec
!   2011-04-07  todling - move newpc4pred to radinfo
!   2012-10-09  Gu - add fut2ps to project unbalanced temp to surface pressure in static B modeling
!   2013-05-27  zhu - add background error variances for aircraft temperature bias correction coefficients
!   2013-10-02  zhu - add reset_predictors_var
!
! subroutines included:
!   sub init_berror         - initialize background error related variables
!   sub create_berror_vars  - allocate global background error related variables
!   sub destroy_berror_vars - deallocate global background error 
!                                    related variables
!   sub set_predictor_var   - set background variances for bias correction coefs
!   sub init_rftable        - load global/global pointers and tables for 
!                                    recursive filters
!   sub initable            - initialize tables/pointers for recursive filters
!   sub create_berror_vars_reg - allocate regional background error 
!                                    related variables
!   sub destroy_berror_vars_reg - deallocate regional background error 
!                                    related variables
!
! Variable Definitions:
!   def norh      - order of interpolations in smoother
!   def ndeg      - degree of smoothing
!   def nta       - first dimension of table (granularity of smoothing coef)
!   def nx        - number longitudes for rf patches
!   def ny        - number latitudes for rf patches
!   def mr        - subdomain dimension for patches
!   def nr        - subdomain dimension for patches
!   def nf        - subdomain dimension for patches
!   def nlath     - half the number of latitudes
!   def ii        - array that point to the x location in table for smoothing
!   def jj        - array that point to the y location in table for smoothing
!   def ii1       - array that point to the x location in table for smoothing
!                 - for northern pole patch
!   def jj1       - array that point to the y location in table for smoothing
!                 - for northern pole patch
!   def ii2       - array that point to the x location in table for smoothing
!                 - for southern pole patch
!   def jj2       - array that point to the y location in table for smoothing
!                 - for southern pole patch
!   def aw        - array that point to the x location in table for smoothing
!   def bw        - factor in background error calculation
!   def as        - normalized scale factor for background error (see namelist setup)
!   def vs        - scale factor for background error vertical scales
!   def be        - smoother coefficients
!   def bl        - blending coefficients for lat/lon boundaries
!   def bl2       - blending coefficients lat/lon boundaries
!   def varprd    - variance for predictors
!   def vprecond  - additional preconditioner for radiance bc predictor coeffients
!   def tsfc_sdv  - standard deviation for land (1) and ice (2) surface temperature
!   def wmask     - not used, function of land-sea mask
!   def table     - table of coeffients for smoothing
!   def slw       - horizontal scale info for 1st patch
!   def slw1      - horizontal scale info for 2nd patch
!   def slw2      - horizontal scale info for 3rd patch
!   def wtaxs     - weights for polar-cascade interpolation
!   def wtxrs     - weights for the step, hybrid to polar grid
!   def inaxs     - index for polar-cascade interpolation
!   def inxrs     - index for polar-cascade interpolation
!   def dssv      - vertical smoother coefficients including variances
!   def qvar3d    - 3d q variance for qoption =2
!   def dssvs     - variances (lat,lon) for surface variables
!   def alv       - vertical smoother coefficients
!   def hzscl     - scale factor for background error horizontal scales
!   def hswgt     - empirical weights to apply to each horizontal scale
!   def pert_berr - logical for turning on background error parameter perturbations
!   def pert_berr_fct - scaling factor for randum numbers for berror perturbations 
!   def bkgv_flowdep  - logical to turn on flow-dependence to background error variances
!   def bkgv_rewgtfct - scaling factor to reweight flow-dependent background error variances
!   def fpsproj   - controls full nsig projection onto surface pressure
!   def bkgv_write- logical to turn on/off generation of binary file with reweighted variances
!   def adjustozvar - adjust ozone variance in stratosphere based on guess field
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use control_vectors, only: nc3d,nvars,mvars
  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_berror
  public :: pcinfo
  public :: create_berror_vars
  public :: destroy_berror_vars
  public :: set_predictors_var
  public :: reset_predictors_var
  public :: init_rftable
  public :: initable
  public :: create_berror_vars_reg
  public :: destroy_berror_vars_reg
! set passed variables to public
  public :: qvar3d,nr,nf,varprd,fpsproj,bkgv_flowdep,fut2ps
  public :: ndx,ndy,ndx2,nmix,nymx,nfg,nfnf,norm,nxem
  public :: vprecond
  public :: adjustozvar
  public :: dssvs,dssv,bkgv_write,bkgv_rewgtfct,hswgt
  public :: hzscl,bw,pert_berr_fct,pert_berr,ndeg,norh,vs
  public :: bl,bl2,be,slw2,slw1,slw,mr,inaxs,wtxrs,wtaxs,nx,ny
  public :: inxrs,jj1,ii2,jj2,ii,jj,ii1,table,alv,nhscrf
  public :: cwcoveqqcov

  integer(i_kind) norh,ndeg,nta,nlath
  integer(i_kind) nx,ny,mr,nr,nf,ndx,ndy,ndx2,nmix,nymx,norm,nxem,nfg,nfnf
  integer(i_kind),allocatable,dimension(:,:):: inaxs,inxrs
  integer(i_kind),allocatable,dimension(:,:,:,:):: ii,jj,ii1,jj1,ii2,jj2
  integer(i_kind) nhscrf

  real(r_kind) bw,vs
  real(r_kind),dimension(1:3):: hzscl,hswgt

  real(r_kind),allocatable,dimension(:):: be,bl,bl2,varprd,vprecond
  real(r_kind),allocatable,dimension(:,:):: table,&
       slw,slw1,slw2
  real(r_kind),allocatable,dimension(:,:,:):: dssvs
  real(r_kind),allocatable,dimension(:,:,:):: wtaxs,wtxrs,qvar3d
  real(r_kind),allocatable,dimension(:,:,:,:):: alv,dssv

  logical pert_berr,bkgv_flowdep,bkgv_write,adjustozvar
  real(r_kind) pert_berr_fct,bkgv_rewgtfct

  logical,save :: fpsproj
  logical,save :: fut2ps
  logical,save :: cwcoveqqcov

contains

  subroutine init_berror
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_berror    initializes constants for the background error
!   prgmmr: kleist           org: np20                date: 2004-01-01
!
! abstract: intializes constants for the background error
!
! program history log:
!   2004-01-01  kleist
!   2004-11-03  treadon - add default definition for horizontal scale weighting factors
!   2005-06-06  wu - add logical fstat
!   2006-11-30  todling - add logical fpsproj
!   2012-05-14  wargan - add adjustozvar
!   2013-10-26  todling - remove initialization of as and tsfc_sdv (see control_vector)
!   2014-01-05  todling - knob to allow cw-cov to be overwritten w/ q-cov
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use constants, only:  zero,one,three
    use balmod, only: fstat
    implicit none
    integer(i_kind) i

    fstat = .false.
    pert_berr = .false.
    bkgv_flowdep = .false.
    adjustozvar = .false.
    bkgv_write = .false.
    pert_berr_fct = zero
    bkgv_rewgtfct = zero
    norh=2
    ndeg=4
    nta=50000
    nlath=48
    nhscrf=3

    bw=zero

    fpsproj = .true.
    fut2ps = .false.
    cwcoveqqcov=.true.

    do i=1,3
       hzscl(i)=one
       hswgt(i)=one/three
    end do
    vs=one/1.5_r_kind

  return
  end subroutine init_berror


  subroutine create_berror_vars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_berror_vars    create arrays for global background error
!   prgmmr: kleist           org: np20                date: 2004-01-01
!
! abstract: creates arrays for global background error
!
! program history log:
!   2004-01-01  kleist
!   2004-07-28  treadon - remove subroutine argument list to --> use modules
!   2004-11-16  treadon - add longitude dimension to array dssv
!   2008-10-24  zhu     - use nrf3,nvars & dssvs,remove dssvt
!                       - change the order of dssv's dimensions
!   2010-04-27  zhu     - add vprecond for new preconditioner of predictors
!   2010-06-01  todling - aas/atsfc_sdv now alloc/ble and initialized here
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use balmod, only: llmin,llmax
  use gridmod, only: nlat,nlon,lat2,lon2,nsig,nnnn1o
  use jfunc, only: nrclen,nclen,diag_precon
  use constants, only: zero,one
  implicit none
  
  llmin=1
  llmax=nlat

! Grid constant to transform to 3 pieces

  nx=nlon*3/2  
  nx=nx/2*2
  ny=nlat*8/9
  ny=ny/2*2
  if(mod(nlat,2)/=0)ny=ny+1
  mr=0
  nr=nlat/4
  nf=nr
  nlath=nlat/2
  if(mod(nlat,2)/=0) nlath=nlath+1
  ndx=(nx-nlon)/2
  ndy=(nlat-ny)/2
  ndx2=2*ndx
  nmix=nr+1-ndy
  nymx=ny-nmix
  norm=norh*2-1
  nxem=nlon/8-1
  nfg=2*nf+1
  nfnf=nfg*nfg

  allocate(wtaxs(0:norh*2-1,nf,0:(nlon/8)-1), &
           wtxrs(0:norh*2-1,0:(nlon/8)-1,mr:nr), &
           be(ndeg), &
           bl(nx-nlon), &
           bl2(nr+1+(ny-nlat)/2), &
           qvar3d(lat2,lon2,nsig))
  if(nc3d>0)then
     allocate(alv(lat2,ndeg,nsig,nc3d),&
              dssv(lat2,lon2,nsig,nc3d))
  endif
  if(nvars-nc3d>0)then
     allocate(dssvs(lat2,lon2,nvars-nc3d))
     dssvs = zero
  endif
  allocate(varprd(nrclen))
  if(diag_precon)allocate(vprecond(nclen))
  allocate(inaxs(nf,nlon/8),inxrs(nlon/8,mr:nr) )

  allocate(slw(ny*nx,nnnn1o),&
           slw1((2*nf+1)*(2*nf+1),nnnn1o),&
           slw2((2*nf+1)*(2*nf+1),nnnn1o))
  allocate(ii(ny,nx,nhscrf,nnnn1o),jj(ny,nx,nhscrf,nnnn1o),&
           ii1(2*nf+1,2*nf+1,nhscrf,nnnn1o),jj1(2*nf+1,2*nf+1,nhscrf,nnnn1o),&
           ii2(2*nf+1,2*nf+1,nhscrf,nnnn1o),jj2(2*nf+1,2*nf+1,nhscrf,nnnn1o))

  return
 end subroutine create_berror_vars


  subroutine destroy_berror_vars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_berror_vars  deallocates global background error arrays
!   prgmmr: kleist           org: np20                date: 2004-01-01
!
! abstract: deallocates global background error arrays
!
! program history log:
!   2004-01-01  kleist
!   2005-03-03  treadon - add implicit none
!   2008-10-24  zhu     - remove dssvt (dssvs includes dssvt)
!   2010-06-01  todling - dealloc as/tsfc_sdv; protect dealloc of some variables
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use jfunc, only: diag_precon
    implicit none
    if(allocated(table)) deallocate(table)
    deallocate(wtaxs)
    deallocate(wtxrs)
    deallocate(be,bl,bl2)
!_RT    if(allocated(qvar3d)) deallocate(qvar3d) ! _RTod somehow this makes GSI crash!when only single var in CV/SV
    deallocate(inaxs,inxrs)
    deallocate(varprd)
    if(allocated(alv))   deallocate(alv)
    if(allocated(dssv))  deallocate(dssv)
    if(allocated(dssvs)) deallocate(dssvs)
    if(diag_precon)deallocate(vprecond)
    deallocate(slw,slw1,slw2)
    deallocate(ii,jj,ii1,jj1,ii2,jj2)

    return
  end subroutine destroy_berror_vars


  subroutine set_predictors_var
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    set_predictors_var sets variances for bias correction predictors
!   prgmmr: kleist           org: np20                date: 2004-01-01
!
! abstract: sets variances for bias correction predictors
!
! program history log:
!   2004-01-01  kleist
!   2004-07-28  treadon - remove subroutine calling list, pass through module
!   2005-05-24  pondeca - take into consideration that nrclen=0 for 2dvar only
!                         surface analysis option
!   2010-04-30  zhu     - add handling of newpc4pred, set varprd based on diagonal
!                         info of analysis error covariance
!   2012-04-14  whitaker - variance can be specified in setup namelist.
!   2013-05-27  zhu - add background error variances for aircraft temperature bias correction coefficients
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use constants, only:  zero,one,two,one_tenth,r10
    use radinfo, only: ostats,varA,jpch_rad,npred,inew_rad,newpc4pred,biaspredvar
    use aircraftinfo, only: aircraft_t_bc_pof,aircraft_t_bc,biaspredt,ntail,npredt,ostats_t,rstats_t,varA_t
    use gridmod, only: twodvar_regional
    use jfunc, only: nrclen, ntclen
    implicit none

    integer(i_kind) i,j,ii
    real(r_kind) stndev
    real(r_kind) obs_count
    logical new_tail
    
    stndev = one/biaspredvar
    do i=1,max(1,nrclen)
       varprd(i)=stndev
    end do

    if ((aircraft_t_bc_pof .or. aircraft_t_bc) .and. ntclen>0) then 
       do i=nrclen-ntclen+1,max(1,nrclen)
          varprd(i)=one/biaspredt
       end do
    end if

!   set variances for bias predictor coeff. based on diagonal info
!   of previous analysis error variance
    if (.not. twodvar_regional .and. newpc4pred) then
       ii=0
       do i=1,jpch_rad
          do j=1,npred
             ii=ii+1
             if (inew_rad(i)) then
                varprd(ii)=10000.0_r_kind
             else
                if (ostats(i)<=20.0_r_kind) then 
                   varA(j,i)=two*varA(j,i)+1.0e-6_r_kind
                   varprd(ii)=varA(j,i)
                else
                   varprd(ii)=1.1_r_kind*varA(j,i)+1.0e-6_r_kind
                end if
                if (varprd(ii)>r10) varprd(ii)=r10
                if (varA(j,i)>10000.0_r_kind) varA(j,i)=10000.0_r_kind
             end if
          end do
       end do

       if ((aircraft_t_bc_pof .or. aircraft_t_bc) .and. ntclen>0) then
          ii=nrclen-ntclen
          do i=1,ntail
             do j=1,npredt
                ii=ii+1

                if (aircraft_t_bc_pof) then 
                   obs_count = ostats_t(j,i)
                   new_tail = varA_t(j,i)==zero
                end if
                if (aircraft_t_bc) then 
                   obs_count = ostats_t(1,i)
                   new_tail = .true.
                   if (any(varA_t(:,i)/=zero)) new_tail = .false.
                end if

                if (new_tail) then
                   varprd(ii)=one_tenth
                   if (aircraft_t_bc .and. j==2) varprd(ii)=1.0e-3_r_kind
                   if (aircraft_t_bc .and. j==3) varprd(ii)=1.0e-4_r_kind
                else
                   if (obs_count<=3.0_r_kind) then
                      if (aircraft_t_bc .and. j==2) then
                         varA_t(j,i)=1.05_r_kind*varA_t(j,i)+1.0e-5_r_kind
                      else if (aircraft_t_bc .and. j==3) then
                         varA_t(j,i)=1.05_r_kind*varA_t(j,i)+1.0e-6_r_kind
                      else
                         varA_t(j,i)=1.05_r_kind*varA_t(j,i)+1.0e-4_r_kind
                      end if
                      varprd(ii)=varA_t(j,i)
                   else
                      if (aircraft_t_bc .and. j==2) then
                         varprd(ii)=1.005_r_kind*varA_t(j,i)+1.0e-5_r_kind
                      else if (aircraft_t_bc .and. j==3) then
                         varprd(ii)=1.005_r_kind*varA_t(j,i)+1.0e-6_r_kind
                      else
                         varprd(ii)=1.005_r_kind*varA_t(j,i)+1.0e-4_r_kind
                      end if
                   end if
                   if (varprd(ii)>one) varprd(ii)=one
                   if (varA_t(j,i)>one) varA_t(j,i)=one
                   if (aircraft_t_bc .and. j>1) then
                      if (varprd(ii)>one_tenth) varprd(ii)=one_tenth
                      if (varA_t(j,i)>one_tenth) varA_t(j,i)=one_tenth
                   end if
                end if
             end do
          end do
       end if
    end if

    return
  end subroutine set_predictors_var


  subroutine reset_predictors_var
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    reset_predictors_var sets variances for bias correction predictors
!   prgmmr: yanqiu           org: np20                date: 2013-10-01
!
! abstract: resets variances for bias correction predictors
!
! program history log:
!   output argument list:
!   2013-10-01  zhu
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use constants, only:  one,one_tenth
    use radinfo, only: newpc4pred,jpch_rad,npred,ostats,inew_rad,iuse_rad
    use aircraftinfo, only: aircraft_t_bc_pof,aircraft_t_bc,biaspredt,ntail,npredt,ostats_t
    use gridmod, only: twodvar_regional
    use jfunc, only: nrclen, ntclen
    implicit none

    integer(i_kind) i,j,ii,obs_count
    real(r_kind) stndev
    
    stndev = one/biaspredt

!   reset variances for bias predictor coeff. based on current data count
    if (.not. twodvar_regional .and. newpc4pred) then
       ii=0
       do i=1,jpch_rad
          do j=1,npred
             ii=ii+1
             if (.not.inew_rad(i) .and. iuse_rad(i)>0 .and. ostats(i)<=20.0_r_kind) then
                varprd(ii)=1.0e-6_r_kind
             end if
          end do
       end do

       if ((aircraft_t_bc_pof .or. aircraft_t_bc) .and. ntclen>0) then
          ii=nrclen-ntclen
          do i=1,ntail
             do j=1,npredt
                ii=ii+1

                if (aircraft_t_bc_pof) obs_count = ostats_t(j,i)
                if (aircraft_t_bc) obs_count = ostats_t(1,i)

                if (obs_count<=3.0_r_kind .and. varprd(ii)>stndev) then
                   varprd(ii)=stndev
                   if (aircraft_t_bc .and. j==2) varprd(ii)=one_tenth*stndev
                   if (aircraft_t_bc .and. j==3) varprd(ii)=one_tenth*one_tenth*stndev
                end if
             end do
          end do
       end if
    end if

    return
  end subroutine reset_predictors_var

  subroutine pcinfo
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    pcinfo    Set up additional preconditioning information
!   prgmmr: zhu           org: np20                date: 2009-11-29
!
! abstract: Set additional preconditioning based on the
!           analysis error covariance from current analysis cycle
!
! program history log:
!   2009-11-29   zhu
!   2016-01-07   derber - apply the same preconditioning formula (for obs_count>3 or 20) 
!                         to the cases with obs_count<=3 or 20 as well
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use kinds, only: r_kind,i_kind
    use radinfo, only: ostats,rstats,varA,jpch_rad,npred,newpc4pred
    use aircraftinfo, only: aircraft_t_bc_pof,aircraft_t_bc,ntail,npredt,ostats_t,rstats_t,varA_t
    use jfunc, only: nclen,nrclen,diag_precon,step_start,ntclen
    use constants, only:  zero,one,tiny_r_kind
    implicit none

!   Declare local variables
    integer(i_kind) i,j,ii,jj,obs_count
    integer(i_kind) nclen1
    real(r_kind) lfact


!   Set up L=inverse(B)*M for preconditioning purpose
!   Only diagonal elememts are considered

!   set a coeff. factor for variances of control variables
    if(diag_precon)then
      lfact=step_start
      vprecond=lfact

      if(newpc4pred)then
!       for radiance bias predictor coeff.
        nclen1=nclen-nrclen
        ii=0
        do i=1,jpch_rad
           do j=1,npred
              ii=ii+1
              if (ostats(i)>zero) vprecond(nclen1+ii)=one/(one+rstats(j,i)*varprd(ii))
              if (ostats(i)>20.0_r_kind) then
                 if (rstats(j,i)>zero) then
                    varA(j,i)=one/(one/varprd(ii)+rstats(j,i))
                 else
                    varA(j,i)=10000.0_r_kind
                 end if
              end if
           end do
        end do

!       for aircraft temperature bias predictor coeff.
        if ((aircraft_t_bc_pof .or. aircraft_t_bc) .and. ntclen>0) then
          nclen1=nclen-ntclen
          ii=0
          jj=nrclen-ntclen
          do i=1,ntail
             do j=1,npredt
                ii=ii+1
                jj=jj+1

                if (aircraft_t_bc_pof) obs_count = ostats_t(j,i)
                if (aircraft_t_bc) obs_count = ostats_t(1,i)

                if (obs_count>zero) vprecond(nclen1+ii)=one/(one+rstats_t(j,i)*varprd(jj))
                if (obs_count>3.0_r_kind) then
                   varA_t(j,i)=one/(one/varprd(jj)+rstats_t(j,i))
                end if
             end do
          end do
        end if
      end if
    end if
    return

  end subroutine pcinfo

  subroutine init_rftable(mype,rate,nnn,sli,sli1,sli2)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_rftable    initializes constants for the background error
!   prgmmr: kleist           org: np20                date: 2004-01-01
!
! abstract: intializes constants for the global background error
!
! program history log:
!   2004-01-01  kleist
!   2004-11-22  derber - modify to do both global and regional, make table
!                        reproducible with different number of processors and
!                        save only those elements of table which are used
!   2005-06-10  devenyi/treadon - remove mype from call rfdparv
!   2008-06-05  safford - rm unused uses
!   2009-03-09  derber  - modify to make arrays smaller
!
!   input argument list:
!     mype
!     rate
!     nnn
!     sli      - horizontal scale info for 1st patch
!     sli1     - horizontal scale info for 2nd patch (optional)
!     sli2     - horizontal scale info for 3rd patch (optional)
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use gridmod, only:  regional
    use constants, only: zero,one
    implicit none

    integer(i_kind)                                               ,intent(in   ) :: nnn,mype
    real(r_kind),dimension(ndeg)                                  ,intent(in   ) :: rate
    real(r_kind),dimension(ny*nx,2,nnn)                           ,intent(in   ) :: sli
    real(r_kind),optional,dimension((2*nf+1)*(2*nf+1),2,nnn),intent(in   ) :: sli1,sli2

    real(r_kind),parameter:: tin = 0.2e-3_r_kind

    integer(i_kind) i,j,k,n,nynx
    integer(i_kind) ihwlb
    integer(i_kind) ntax,iloc
    
    real(r_kind):: hwlmax,hwlmin,hwlb,hwle,wni2,hzsmax,hzsmin
    real(r_kind),parameter:: r999         = 999.0_r_kind
    real(r_kind),allocatable,dimension(:):: dsh
    logical,allocatable,dimension(:):: iuse
    integer(i_kind),allocatable,dimension(:):: ipoint


    nynx=ny*nx
    if(.not. regional)then
       if(.not. present(sli1) .or. .not. present(sli2))then
          write(6,*)'INIT_RFTABLE:  ***ERROR*** sli1 or sli2 not present'
          call stop2(34)
       end if
    end if

! Determine lower/upper bounds on scales
    hwlmax=-r999
    hwlmin=r999
    do k=1,nnn

       do j=1,nynx
          hwlmax=max(hwlmax,sli(j,1,k),sli(j,2,k))
          hwlmin=min(hwlmin,sli(j,1,k),sli(j,2,k))
!         hwlmax=max(hwlmax,sli(j,1,k))
!         hwlmin=min(hwlmin,sli(j,1,k))
!         hwlmax=max(hwlmax,sli(j,2,k))
!         hwlmin=min(hwlmin,sli(j,2,k))
       end do
       if(.not. regional)then
          do j=1,nfnf
             hwlmax=max(hwlmax,sli1(j,1,k),sli2(j,1,k),sli1(j,2,k),sli2(j,2,k))
             hwlmin=min(hwlmin,sli1(j,1,k),sli2(j,1,k),sli1(j,2,k),sli2(j,2,k))
!            hwlmax=max(hwlmax,sli1(j,1,k))
!            hwlmin=min(hwlmin,sli1(j,1,k))
!            hwlmax=max(hwlmax,sli2(j,1,k))
!            hwlmin=min(hwlmin,sli2(j,1,k))
!            hwlmax=max(hwlmax,sli1(j,2,k))
!            hwlmin=min(hwlmin,sli1(j,2,k))
!            hwlmax=max(hwlmax,sli2(j,2,k))
!            hwlmin=min(hwlmin,sli2(j,2,k))
          end do
       end if
    enddo

! factor from multi-Gaussian RF
!   write(6,*)'INIT_RFTABLE:  hwlmax...=',hwlmax,hwlmin,&
!        hzscl(1),hzscl(2),hzscl(3),mype,nynx,nnn
    hzsmax=-r999
    hzsmin=r999
    do j=1,nhscrf
       hzsmax=max(hzsmax,hzscl(j))
       hzsmin=min(hzsmin,hzscl(j))
    end do

    hwlmax=hwlmax*hzsmax
    hwlmin=hwlmin*hzsmin

! setup smoother coef and scale
    if (nnn>0 .and. (hwlmax<=zero .or. hwlmin<=zero)) then
       write(6,*)'INIT_RFTABLE:  ***ERROR*** illegal value for min,max scale.',&
            '  hwlmin,hwlmax=',hwlmin,hwlmax,mype
       call stop2(41)
    endif


    hwlb=one/hwlmax
    hwle=one/hwlmin

    ihwlb=hwlb/tin
    hwlb=ihwlb*tin
!   tin=(hwle-hwlb)/float(nta-1)
    ntax=(hwle-hwlb)/tin+2
!   write(6,*)'INIT_RFTABLE:  tin ',ntax,ihwlb,tin,hwlb,hwle

    allocate(iuse(ntax))

    iuse=.false.
    wni2=one/tin
    do k=1,nnn
       do n=1,2
          do i=1,nynx
             do j=1,nhscrf
                iloc=min(ntax,nint(one-ihwlb+wni2/(hzscl(j)*sli(i,n,k))))
                iloc=max(iloc,1)
                iuse(iloc)=.true.
             enddo
          enddo

          if(.not. regional)then
             do i=1,nfnf
                do j=1,nhscrf
                   iloc=min(ntax,nint(one-ihwlb+wni2/(hzscl(j)*sli1(i,n,k))))
                   iloc=max(iloc,1)
                   iuse(iloc)=.true.
                   iloc=min(ntax,nint(one-ihwlb+wni2/(hzscl(j)*sli2(i,n,k))))
                   iloc=max(iloc,1)
                   iuse(iloc)=.true.
                enddo
             enddo
          end if

       enddo
    enddo
    nta=0
    allocate(dsh(ntax),ipoint(ntax))
    ipoint=0
    do i=1,ntax
       if(iuse(i))then
          nta=nta+1
          ipoint(i)=nta
          dsh(nta)=one/(float(i-1+ihwlb)*tin)
       end if
    end do
!   write(6,*)'INIT_RFTABLE:  ntax,nta = ',ntax,nta

!   Loop over number of sigma levels per task    
    do k=1,nnn

!      Load pointers into table array
       do j=1,nhscrf

          call initable(ny,nx,sli(1,1,k),ntax,ihwlb,&
             ii(1,1,j,k),jj(1,1,j,k),hzscl(j),tin,ipoint)

          if(.not. regional)then
             call initable(nfg,nfg,sli1(1,1,k),ntax,ihwlb,&
              ii1(1,1,j,k),jj1(1,1,j,k),hzscl(j),tin,ipoint)

             call initable(nfg,nfg,sli2(1,1,k),ntax,ihwlb,&
              ii2(1,1,j,k),jj2(1,1,j,k),hzscl(j),tin,ipoint)
          end if

       end do

!   End of loop over number of sigma levels per mpi task       
    end do

    deallocate(iuse,ipoint)

    allocate(table(nta,ndeg)) 

    call rfdparv(dsh,rate,table,nta,ndeg)

    deallocate(dsh)

    return
  end subroutine init_rftable

  
  subroutine initable(nxdim,nydim,sli,ntax,ihwlb,iix,jjx,factor,tin,ipoint)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    initable    initializes pointers to table for background error
!   prgmmr: kleist           org: np20                date: 2004-01-01
!
! abstract: initializes pointers to table for global and regional background error
!
! program history log:
!   2004-01-01  kleist
!   2004-11-22  derber  - modify to be consistent with init_rftable
!   2008-06-05  safford - rm unused var
!
!   input argument list:
!     nxdim    - 1st dimension of arrays
!     nydim    - 2nd dimension of arrays
!     sli      - horizontal scale info
!     ntax     - number of possible entries in table array
!     ihwlb    - minimum of possible coefficient values
!     factor   - horizontal scale factor
!     tin      - table interval
!     ipoint   - pointer at table locations
!
!   output argument list:
!     iix      - first index pointer array to table
!     jjx      - second index pointer array to table
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use constants, only: one
    implicit none

    integer(i_kind)                       ,intent(in   ) :: nydim,nxdim,ntax
    real(r_kind)                          ,intent(in   ) :: factor,tin
    real(r_kind),dimension(nxdim,nydim,2) ,intent(in   ) :: sli
    integer(i_kind),dimension(ntax)       ,intent(in   ) :: ipoint
    integer(i_kind)                       ,intent(in   ) :: ihwlb
    integer(i_kind),dimension(nxdim,nydim),intent(  out) :: iix,jjx

    integer(i_kind) iy,ix,iloc
    real(r_kind) wni2

!   Load pointers for table array
    wni2=one/tin
    do iy=1,nydim
       do ix=1,nxdim
          iloc=min(ntax, max(1, nint(one-ihwlb+wni2/(sli(ix,iy,1)*factor))))
          iix(ix,iy)=ipoint(iloc)
          iloc=min(ntax, max(1, nint(one-ihwlb+wni2/(sli(ix,iy,2)*factor))))
          jjx(ix,iy)=ipoint(iloc)
       enddo
    enddo

    return
  end subroutine initable


  subroutine create_berror_vars_reg
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_berror_vars_reg  create arrays for reg background error
!   prgmmr: kleist           org: np20                date: 2004-01-01
!
! abstract: creates arrays for regional background error
!
! program history log:
!   2004-01-01  kleist
!   2004-07-28  treadon - simplify subroutine calling list
!   2004-11-16  treadon - add longitude dimension to array dssv
!   2005-05-24  pondeca - take into consideration that npred=npredp=0
!                         for 2dvar only surface analysis option
!   2005-06-23  middlecoff/treadon - iniitalize mr,nr,nf
!   2009-01-04  todling - remove mype
!   2010-03-05  zhu - use nrf3 and nvars,remove dssvt
!                   - change lat dimension of dssv and dssvs
!                   - change the order of dssv's dimensions
!   2010-10-22  zhu - allocate vprecond for new preconditioner of predictors
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use constants, only: zero
    use balmod, only: llmin,llmax
    use gridmod, only: nlat,nlon,nsig,nnnn1o,lat2,lon2
    use jfunc, only: nrclen,nclen,diag_precon
    implicit none
    
    nx=nlon
    ny=nlat
    mr=1
    nr=1
    nf=nr
    
!   Grid constant for background error

    allocate(be(ndeg), &
         qvar3d(lat2,lon2,nsig))
    if(nc3d>0)then
       allocate(alv(llmin:llmax,ndeg,nsig,nc3d), &
            dssv(lat2,lon2,nsig,nc3d))
       if(nvars-nc3d>0)then
          allocate(dssvs(lat2,lon2,nvars-nc3d))
          dssvs = zero
       endif
    endif
    
    allocate(varprd(max(1,nrclen) ) )     
    if(diag_precon)allocate(vprecond(nclen))

    allocate(slw(ny*nx,nnnn1o) )
    allocate(ii(ny,nx,3,nnnn1o),jj(ny,nx,3,nnnn1o) )
    
    
    return
  end subroutine create_berror_vars_reg


  subroutine destroy_berror_vars_reg
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_berror_vars_reg  deallocate reg background error arrays
!   prgmmr: kleist           org: np20                date: 2004-01-01
!
! abstract: deallocates regional background error arrays
!
! program history log:
!   2004-01-01  kleist
!   2005-03-03  treadon - add implicit none
!   2010-03-09  zhu     - remove dssvt
!   2010-10-22  zhu - deallocate vprecond for new preconditioner of predictors
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use jfunc, only:diag_precon
    implicit none

    deallocate(be,qvar3d)
    if(allocated(table)) deallocate(table)
    if(allocated(alv)) deallocate(alv)
    if(allocated(dssv)) deallocate(dssv)
    if(allocated(dssvs)) deallocate(dssvs)
    deallocate(ii,jj)
    deallocate(slw)
    deallocate(varprd)
    if(diag_precon)deallocate(vprecond)

    return
  end subroutine destroy_berror_vars_reg

end module berror
