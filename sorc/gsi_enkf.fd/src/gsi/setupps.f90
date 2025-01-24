module ps_setup
  implicit none
  private
  public:: setup
        interface setup; module procedure setupps; end interface

contains
subroutine setupps(obsLL,odiagLL,lunin,mype,bwork,awork,nele,nobs,is,conv_diagsave)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setupps     compute rhs of oi for surface pressure
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract: For surface pressure observations, this routine
!              a) reads obs assigned to given mpi task (geographic region),
!              b) simulates obs from guess,
!              c) apply some quality control to obs,
!              d) load weight and innovation arrays used in minimization
!              e) collects statistics for runtime diagnostic output
!              f) writes additional diagnostic information to output file
!
! program history log:
!   1990-10-06  parrish
!   1998-04-10  weiyu yang
!   1999-03-01  wu - ozone processing moved into setuprhs from setupoz
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-17  treadon - update documentation
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-06  parrish - increase size of pwork array for nonlinear qc
!   2004-11-22  derber - remove weight, add logical for boundary point
!   2004-12-22  treadon - move logical conv_diagsave from obsmod to argument list
!   2005-03-02  dee - remove garbage from diagnostic file
!   2005-03-09  parrish - nonlinear qc change to account for inflated obs error
!   2005-07-27  derber  - add print of monitoring and reject data
!   2005-09-12  derber  - rewrite and incorporate prep routine   
!   2005-09-28  derber  - combine with prep,spr,remove tran and clean up
!   2005-10-14  derber  - input grid location and fix regional lat/lon
!   2005-10-21  su   - modified variational quality control and diagnose output
!   2005-11-03  treadon - correct error in ilone,ilate data array indices
!   2005-11-22  wu      - add option to perturb conventional obs
!   2005-11-29  derber - remove psfcg and use ges_lnps instead
!   2006-01-31  todling - storing wgt/wgtlim in diag file instead of wgt only
!   2006-02-02  treadon - rename lnprsl as ges_lnprsl
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-03-21  treadon - modify optional perturbation to observation
!   2006-05-30  su,derber,treadon - modify diagnostic output
!   2006-06-06  su - move to wgtlim to constants module
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - modify handling of multiple data at same location
!   2006-07-31  kleist - change analysis variable to ps (cb) instead of lnps
!   2006-08-28      su - fix a bug in variational qc
!   2007-03-09      su - modify obs perturbation 
!   2007-03-19  tremolet - binning of observations
!   2007-06-05  tremolet - add observation diagnostics structure
!   2007-08-28      su - modify the error used in gross check
!   2008-03-24      wu - oberror tuning and perturb obs
!   2008-05-23  safford - rm unused vars and uses
!   2008-12-03  todling - changed handle of tail%time
!   2009-02-06  pondeca - for each observation site, add the following to the
!                         diagnostic file: local terrain height, dominate surface
!                         type, station provider name and station subprovider name
!   2009-08-19  guo     - changed for multi-pass setup with dtime_check().
!   2011-05-06  Su      - modify the observation gross check error
!   2011-08-09  pondeca - correct bug in qcgross use
!   2013-01-26  parrish - change grdcrd to grdcrd1, intrp2a to intrp2a11,
!                                  tintrp2a to tintrp2a1, tintrp2a11,
!                                  tintrp3 to tintrp31 (so debug compile works on WCOSS)
!   2013-10-19  todling - metguess now holds background 
!   2014-01-28  todling - write sensitivity slot indicator (ioff) to header of diagfile
!   2014-04-12       su - add non linear qc from Purser's scheme
!   2014-12-30  derber - Modify for possibility of not using obsdiag
!   2015-10-01  guo   - full res obvsr: index to allow redistribution of obsdiags
!   2015-12-21  yang    - Parrish's correction to the previous code in new varqc.
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!   2016-06-24  guo     - fixed the default value of obsdiags(:,:)%tail%luse to luse(i)
!                       . removed (%dlat,%dlon) debris.
!   2016-11-29  shlyaeva - save linearized H(x) for EnKF
!   2016-12-09  mccarty - add netcdf_diag capability
!   2017-02-09  guo     - Remove m_alloc, n_alloc.
!                       . Remove my_node with corrected typecast().
!   2017-03-31  Hu      -  addd option l_closeobs to use closest obs to analysis
!                                     time in analysis
!   2019-09-20  Su      -  remove current VQC part and add subroutine call on VQC and add new VQC option
!   2021-10-xx  pondeca/morris/zhao - added observation provider/subprovider
!                         information in diagonostic file, which is used
!                         in offline observation quality control program (AutoObsQC) 
!                         for 3D-RTMA (if l_obsprvdiag is true).
!
!
!   input argument list:
!     lunin    - unit from which to read observations
!     mype     - mpi task id
!     nele     - number of data elements per observation
!     nobs     - number of observations
!
!   output argument list:
!     bwork    - array containing information about obs-ges statistics
!     awork    - array containing information for data counts and gross checks
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use mpeu_util, only: die,perr,getindex
  use state_vectors, only: svars2d, levels, ns3d
  use kinds, only: r_kind,r_single,r_double,i_kind
  use m_obsdiagNode, only: obs_diag
  use m_obsdiagNode, only: obs_diags
  use m_obsdiagNode, only: obsdiagLList_nextNode
  use m_obsdiagNode, only: obsdiagNode_set
  use m_obsdiagNode, only: obsdiagNode_get
  use m_obsdiagNode, only: obsdiagNode_assert

  use obsmod, only: rmiss_single,perturb_obs,oberror_tune,&
                    lobsdiagsave,nobskeep,lobsdiag_allocated,&
                    time_offset,lobsdiag_forenkf,ianldate
  use m_obsNode, only: obsNode
  use m_psNode, only: psNode
  use m_psNode, only: psNode_appendto
  use m_obsLList, only: obsLList
  use obsmod, only: luse_obsdiag
  use gsi_4dvar, only: nobs_bins,hr_obsbin,min_offset
  use oneobmod, only: magoberr,maginnov,oneobtest
  use obsmod, only: netcdf_diag, binary_diag, dirname
  use obsmod, only: l_obsprvdiag
  use nc_diag_write_mod, only: nc_diag_init, nc_diag_header, nc_diag_metadata, &
       nc_diag_write, nc_diag_data2d, nc_diag_metadata_to_single
  use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_get_dim, nc_diag_read_close
  use gridmod, only: nsig,get_ij,twodvar_regional
  use constants, only: zero,one_tenth,one,half,pi,g_over_rd, &
             huge_r_kind,tiny_r_kind,two,huge_single, &
             r1000,wgtlim,tiny_single,r10,three
  use jfunc, only: jiter,last,jiterstart,miter
  use qcmod, only: dfact,dfact1,npres_print,vqc,nvqc
  use guess_grids, only: hrdifsig,ges_lnprsl,nfldsig,ntguessig
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype,icsubtype
  use convinfo, only: ibeta,ikapa 

  use m_dtime, only: dtime_setup, dtime_check

  use hdraobmod, only: nhdps,hdpslist
  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle
  use sparsearr, only: sparr2, new, size, writearray, fullarray
  use rapidrefresh_cldsurf_mod, only: l_closeobs

  implicit none

! Declare passed variables
  type(obsLList ),target,dimension(:),intent(in):: obsLL
  type(obs_diags),target,dimension(:),intent(in):: odiagLL

  real(r_kind),dimension(100+7*nsig)               ,intent(inout) :: awork
  real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout) :: bwork
  integer(i_kind)                                  ,intent(in   ) :: lunin,mype,nele,nobs
  integer(i_kind)                                  ,intent(in   ) :: is     ! ndat index
  logical                                          ,intent(in   ) :: conv_diagsave

! Declare local parameters
  character(len=*),parameter:: myname='setupps'
  real(r_kind),parameter:: r0_7=0.7_r_kind

! Declare external calls for code analysis
  external:: intrp2a
  external:: tintrp2a1
  external:: tintrp3
  external:: grdcrd1
  external:: stop2

! Declare local variables
  real(r_double) rstation_id
  real(r_kind) tges,tges2,drbx,pob,pges,psges,psges2,dlat,dlon,dtime,var_jb
  real(r_kind) rdelz,rdp,halfpi,obserror,obserrlm,drdp,residual,ratio
  real(r_kind) errinv_input,errinv_adjst,errinv_final
  real(r_kind) err_input,err_adjst,err_final,tfact
  real(r_kind) zsges,pgesorig,rwgt
  real(r_kind) r0_005,r0_2,r2_5,tmin,tmax,half_tlapse
  real(r_kind) ratio_errors,error,dhgt,ddiff,dtemp
  real(r_kind) val2,ress,ressw2,val,valqc
  real(r_kind) cg_t,cvar,wgt,rat_err2,qcgross
  real(r_kind),dimension(nobs):: dup
  real(r_kind),dimension(nsig):: prsltmp
  real(r_kind),dimension(nele,nobs):: data
  real(r_single),allocatable,dimension(:,:)::rdiagbuf

  integer(i_kind) ier,ilon,ilat,ipres,ihgt,itemp,id,itime,ikx,iqc,iptrb,ijb
  integer(i_kind) ier2,iuse,ilate,ilone,istnelv,idomsfc,izz,iprvd,isprvd
  integer(i_kind) ikxx,nn,ibin,ioff,ioff0
  integer(i_kind) i,j,nchar,nreal,ii,jj,k,l,mm1
  integer(i_kind) itype,isubtype 
  integer(i_kind) ibb,ikk,idddd

  logical,dimension(nobs):: luse,muse
  integer(i_kind),dimension(nobs):: ioid ! initial (pre-distribution) obs ID
  logical proceed
 
  character(8) station_id
  character(8),allocatable,dimension(:):: cdiagbuf
  character(8),allocatable,dimension(:):: cprvstg,csprvstg
  character(8) c_prvstg,c_sprvstg
  real(r_double) r_prvstg,r_sprvstg
  real(r_kind) :: hr_offset

  logical:: in_curbin, in_anybin, save_jacobian
  type(psNode),pointer:: my_head
  type(obs_diag ),pointer:: my_diag
  type(obs_diags),pointer:: my_diagLL

  equivalence(rstation_id,station_id)
  equivalence(r_prvstg,c_prvstg)
  equivalence(r_sprvstg,c_sprvstg)
  
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_ps
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_z
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_tv

  type(sparr2) :: dhx_dx
  integer(i_kind) :: ps_ind, nnz, nind

  type(obsLList),pointer,dimension(:):: pshead
  pshead => obsLL(:)

  save_jacobian = conv_diagsave .and. jiter==jiterstart .and. lobsdiag_forenkf

!*******************************************************************************
! Read observations in work arrays.

  read(lunin)data,luse,ioid

!        index information for data array (see reading routine)
  ier=1       ! index of obs error 
  ilon=2      ! index of grid relative obs location (x)
  ilat=3      ! index of grid relative obs location (y)
  ipres=4     ! index of pressure
  ihgt=5      ! index of surface height
  itemp=6     ! index of surface temperature observation
  id=7        ! index of station id
  itime=8     ! index of observation time in data array
  ikxx=9      ! index of ob type
  iqc=10      ! index of quality mark
  ier2=11     ! index of original-original obs error ratio
  iuse=12     ! index of use parameter
  idomsfc=13  ! index of dominant surface type
  ilone=14    ! index of longitude (degrees)
  ilate=15    ! index of latitude (degrees)
  istnelv=16  ! index of station elevation (m)
  izz=17      ! index of surface height
  iprvd=18    ! index of observation provider
  isprvd=19   ! index of observation subprovider
  ijb=20      ! index of non linear qc parameter
  iptrb=21    ! index of ps perturbation

! Declare local constants
  halfpi = half*pi
  r0_005 = 0.005_r_kind
  r0_2=0.2_r_kind
  r2_5=2.5_r_kind
  tmin=150.0_r_kind
  tmax=350.0_r_kind
  half_tlapse=0.00325_r_kind  ! half of 6.5K/1km
  mm1=mype+1
  var_jb=zero

! Check to see if required guess fields are available
  call check_vars_(proceed)
  if(.not.proceed) return  ! not all vars available, simply return

! If require guess vars available, extract from bundle ...
  call init_vars_

!  Check to see if observation should be used or monitored
!  muse = true  then used

  do i=1,nobs
     muse(i)=nint(data(iuse,i)) <= jiter .and. nint(data(iqc,i)) < 8
  end do
!  If HD raobs available move prepbufr version to monitor
  if(nhdps > 0)then
     do i=1,nobs
        ikx=nint(data(ikxx,i))
        itype=ictype(ikx)
        if(itype == 120) then
           rstation_id     = data(id,i)
           read(station_id,'(i5,3x)',err=1200) idddd
           stn_loop:do j=1,nhdps
             if(idddd == hdpslist(j))then
                data(iuse,i)=108._r_kind
                muse(i) = .false.
                exit stn_loop
             end if
           end do stn_loop
        end if
1200    continue
     end do
  end if

  hr_offset=min_offset/60.0_r_kind
!  Check for duplicate observations at same location
  dup=one
  do k=1,nobs
     do l=k+1,nobs
        if(data(ilat,k) == data(ilat,l) .and. &
           data(ilon,k) == data(ilon,l) .and. &
           data(ier,k) < r1000 .and. data(ier,l) < r1000 .and. &
           muse(k) .and. muse(l))then
           if(l_closeobs) then
              if(abs(data(itime,k)-hr_offset)<abs(data(itime,l)-hr_offset)) then
                  muse(l)=.false.
              else
                  muse(k)=.false.
              endif
           else
              tfact=min(one,abs(data(itime,k)-data(itime,l))/dfact1)
              dup(k)=dup(k)+one-tfact*tfact*(one-dfact)
              dup(l)=dup(l)+one-tfact*tfact*(one-dfact)
           endif
        end if
     end do
  end do


! If requested, save select data for output to diagnostic file

  if(conv_diagsave)then
     nchar=1
     ioff0=20
     nreal=ioff0
     if (lobsdiagsave) nreal=nreal+4*miter+1
     if (twodvar_regional .or. l_obsprvdiag) then
       nreal=nreal+2                           !account for idomsfc,izz
       allocate(cprvstg(nobs),csprvstg(nobs))  !obs provider info
     endif
     if (save_jacobian) then
       nnz   = 1                   ! number of non-zero elements in dH(x)/dx profile
       nind   = 1
       call new(dhx_dx, nnz, nind)
       nreal = nreal + size(dhx_dx)
     endif
     allocate(cdiagbuf(nobs),rdiagbuf(nreal,nobs))
     ii=0
     if(netcdf_diag) call init_netcdf_diag_
  end if

  call dtime_setup()
  do i = 1,nobs
     dtime=data(itime,i)
     call dtime_check(dtime, in_curbin, in_anybin)
     if(.not.in_anybin) cycle

     if(in_curbin) then
        ikx=nint(data(ikxx,i))
        itype=ictype(ikx) 
        isubtype=icsubtype(ikx)
!  reduce observation error for buoy surface pressure observations
        if( itype ==180 .and. isubtype == 0) then
           data(ier2,i)=data(ier2,i)*0.7_r_kind
           data(ier,i)=data(ier,i)*0.7_r_kind
        endif
        error=data(ier2,i)
        dlon=data(ilon,i)
        dlat=data(ilat,i)
        pob=data(ipres,i)
 
        dhgt=data(ihgt,i)
        dtemp=data(itemp,i)
        var_jb=data(ijb,i)
     endif
 
!    Link observation to appropriate observation bin
     if (nobs_bins>1) then
        ibin = NINT( dtime/hr_obsbin ) + 1
     else
        ibin = 1
     endif
     IF (ibin<1.OR.ibin>nobs_bins) write(6,*)mype,'Error nobs_bins,ibin= ',nobs_bins,ibin

     if (luse_obsdiag) my_diagLL => odiagLL(ibin)

!    Link obs to diagnostics structure
     if (luse_obsdiag) then
        my_diag => obsdiagLList_nextNode(my_diagLL      ,&
                create = .not.lobsdiag_allocated        ,&
                   idv = is             ,&
                   iob = ioid(i)        ,&
                   ich = 1              ,&
                  elat = data(ilate,i)  ,&
                  elon = data(ilone,i)  ,&
                  luse = luse(i)        ,&
                 miter = miter          )

        if (.not.associated(my_diag)) call die(myname, &
                'obsdiagLList_nextNode(), create =', .not.lobsdiag_allocated)
     endif

     if(.not.in_curbin) cycle

!    Load obs error into local variable
     obserror = max(cermin(ikx)*one_tenth,&
          min(cermax(ikx)*one_tenth,data(ier,i)))

! Get guess sfc hght at obs location

     call intrp2a11(ges_z(1,1,ntguessig),zsges,dlat,dlon,mype)

! Interpolate to get log(ps) and log(pres) at mid-layers
! at obs location/time

     call tintrp2a11(ges_ps,psges,dlat,dlon,dtime,hrdifsig,&
        mype,nfldsig)
     call tintrp2a1(ges_lnprsl,prsltmp,dlat,dlon,dtime,hrdifsig,&
        nsig,mype,nfldsig)

! Convert pressure to grid coordinates

     pgesorig = psges

! Take log for vertical interpolation
     psges = log(psges)
     call grdcrd1(psges,prsltmp,nsig,-1)

! Get guess temperature at observation location and surface

     call tintrp31(ges_tv,tges,dlat,dlon,psges,dtime, &
          hrdifsig,mype,nfldsig)

! Adjust observation error and obs value due to differences in surface height

     rdelz=dhgt-zsges
     if(dtemp > tmin .and. dtemp < tmax) then

!  Case of observed surface temperature

        drbx = half*abs(tges-dtemp)+r0_2+r0_005*abs(rdelz)
        tges = half*(tges+dtemp)
     else

!  No observed temperature 
        psges2=log(data(ipres,i))
        call grdcrd1(psges2,prsltmp,nsig,-1)
        call tintrp31(ges_tv,tges2,dlat,dlon,psges2,dtime, &
             hrdifsig,mype,nfldsig)
 
        drbx = half*abs(tges-tges2)+r2_5+r0_005*abs(rdelz)
        tges = half*(tges+tges2)
 
! Extrapolate surface temperature below ground at 6.5 k/km
! note only extrapolating .5dz, if no surface temp available.

        if(rdelz < zero)then
           tges=tges-half_tlapse*rdelz
           drbx=drbx-half_tlapse*rdelz
        end if

     end if

! Adjust guess hydrostatically 

     rdp = g_over_rd*rdelz/tges

! Subtract off dlnp correction, then convert to pressure (cb)
     pges = exp(log(pgesorig) - rdp)

     if (save_jacobian) then
        ps_ind = getindex(svars2d,'ps')
        if (ps_ind < 0) then
           print *, 'Error: no variable ps in state vector. Exiting.'
           call stop2(1300)
        endif

        dhx_dx%st_ind(1) = sum(levels(1:ns3d)) + ps_ind
        dhx_dx%end_ind(1) = sum(levels(1:ns3d)) + ps_ind
        dhx_dx%val(1) = one
     endif

! observational error adjustment 

     drdp=zero
     if (.not.twodvar_regional) then
       drdp = pges*(g_over_rd*abs(rdelz)*drbx/(tges**2))
     endif

!  find adjustment to observational error (in terms of ratio)
     ratio_errors=error/(data(ier,i)+drdp)
     error=one/error

! Compute innovations
     ddiff=pob-pges  ! in cb

! Oberror Tuning and Perturb Obs
     if(muse(i)) then
        if(oberror_tune )then
           if( jiter > jiterstart ) then
              ddiff=ddiff+data(iptrb,i)/error/ratio_errors
           endif
        else if(perturb_obs )then
           ddiff=ddiff+data(iptrb,i)/error/ratio_errors
        endif
     endif

!    Gross check using innovation normalized by error

     obserror = min(r10/max(ratio_errors*error,tiny_r_kind),huge_r_kind)
     obserrlm = max(cermin(ikx),min(cermax(ikx),obserror))
     residual = abs(r10*ddiff)
     ratio    = residual/obserrlm

! modify gross check limit for quality mark=3
     if(data(iqc,i) == three ) then
        qcgross=r0_7*cgross(ikx)
     else
        qcgross=cgross(ikx)
     endif

     if (ratio > qcgross .or. ratio_errors < tiny_r_kind) then
        if (luse(i)) awork(6) = awork(6)+one
        error = zero
        ratio_errors = zero
     else
        ratio_errors = ratio_errors/sqrt(dup(i))
     end if

     if (ratio_errors*error <= tiny_r_kind) muse(i)=.false.

! If requested, setup for single obs test.

     if (oneobtest) then
        maginnov=one_tenth*maginnov
        magoberr=one_tenth*magoberr
        ddiff=maginnov
        error=one/magoberr
        ratio_errors=one
        muse(i) = .true.
     endif

     if (nobskeep>0.and.luse_obsdiag) call obsdiagNode_get(my_diag, jiter=nobskeep, muse=muse(i))

! Compute penalty terms, and accumulate statistics.

     val      = error*ddiff
     if(nvqc .and. ibeta(ikx) >0  ) ratio_errors=0.8_r_kind*ratio_errors
     if(luse(i))then

!    Compute penalty terms (linear & nonlinear qc).
        val2     = val*val
        if(vqc) then
           cg_t=cvar_b(ikx)
           cvar=cvar_pg(ikx)
        else
           cg_t=zero
           cvar=zero
        endif
        if(nvqc) then
           ibb=ibeta(ikx)
           ikk=ikapa(ikx)
        else
           ibb=0
           ikk=0
        endif

        call vqc_setup(val,ratio_errors,error,cvar,cg_t,ibb,ikk,&
                      var_jb,rat_err2,wgt,valqc)
        rwgt = wgt/wgtlim
        if (muse(i)) then
!       Accumulate statistics for obs used belonging to this task        
           if(rwgt < one) awork(21) = awork(21)+one
           awork(4)=awork(4)+val2*rat_err2
           awork(5)=awork(5)+one
           awork(22)=awork(22)+valqc
           nn=1
        else
       
!       rejected obs 
           nn=2
!       monitored obs 
           if(ratio_errors*error >=tiny_r_kind)nn=3
        end if

!     Accumulate statistics for each ob type

        ress   = ddiff*r10
        ressw2 = ress*ress
        bwork(1,ikx,1,nn)  = bwork(1,ikx,1,nn)+one              ! count
        bwork(1,ikx,2,nn)  = bwork(1,ikx,2,nn)+ress             ! (o-g)
        bwork(1,ikx,3,nn)  = bwork(1,ikx,3,nn)+ressw2           ! (o-g)**2
        bwork(1,ikx,4,nn)  = bwork(1,ikx,4,nn)+val2*rat_err2    ! penalty
        bwork(1,ikx,5,nn)  = bwork(1,ikx,5,nn)+valqc            ! nonlin qc penalty
 
     end if

     if (luse_obsdiag) then
        call obsdiagNode_set(my_diag,wgtjo=(error*ratio_errors)**2, &
                jiter=jiter,muse=muse(i),nldepart=ddiff)
     endif


     if (.not. last .and. muse(i)) then
!    If obs is "acceptable", load array with obs info for use
!    in inner loop minimization (int* and stp* routines)
!    if no minimization (inner loop), do not load arrays

        allocate(my_head)
        call psNode_appendto(my_head,pshead(ibin))

        my_head%idv = is
        my_head%iob = ioid(i)
        my_head%elat= data(ilate,i)
        my_head%elon= data(ilone,i)

!       Set (i,j) indices of guess gridpoint that bound obs location
        call get_ij(mm1,dlat,dlon,my_head%ij,my_head%wij)

        my_head%res      = ddiff
        my_head%err2     = error**2
        my_head%raterr2  = ratio_errors**2     
        my_head%time     = dtime
        my_head%b        = cvar_b(ikx)
        my_head%pg       = cvar_pg(ikx)
        my_head%jb       = var_jb
        my_head%ib      = ibeta(ikx)
        my_head%ik      = ikapa(ikx)
        my_head%luse     = luse(i)
        if(oberror_tune) then
           my_head%kx    = ikx        ! data type for oberror tuning
           my_head%ppertb= data(iptrb,i)/error/ratio_errors ! obs perturbation
        endif

        if (luse_obsdiag) then
           call obsdiagNode_assert(my_diag,my_head%idv,my_head%iob,1,myname,'my_diag:my_head')
           my_head%diags => my_diag
        endif

        my_head => null()

     endif

! Save obs and simulated surface pressure data for diagnostic output

     if(conv_diagsave .and. luse(i))then
        ii=ii+1
        rstation_id     = data(id,i)
        pob      = pob*r10
        pges     = pges*r10
        pgesorig = pgesorig*r10

        err_input = data(ier2,i)*r10   ! r10 converts cb to mb
        err_adjst = data(ier,i)*r10
        if (ratio_errors*error/r10>tiny_r_kind) then
           err_final = r10/(ratio_errors*error)
        else
           err_final = huge_single
        endif

        errinv_input = huge_single
        errinv_adjst = huge_single
        errinv_final = huge_single
        if (err_input>tiny_single) errinv_input = one/err_input
        if (err_adjst>tiny_single) errinv_adjst = one/err_adjst
        if (err_final>tiny_single) errinv_final = one/err_final

        if(binary_diag) call contents_binary_diag_(my_diag)
        if(netcdf_diag) call contents_netcdf_diag_(my_diag)

     end if

! End of loop over observations
  end do

! Release memory of local guess arrays
  call final_vars_

! Write information to diagnostic file

  if(conv_diagsave)then
     if(netcdf_diag) call nc_diag_write
     if(binary_diag .and. ii>0)then
        write(7)' ps',nchar,nreal,ii,mype,ioff0
        write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)

        if (twodvar_regional .or. l_obsprvdiag) then
           write(7)cprvstg(1:ii),csprvstg(1:ii)
           deallocate(cprvstg,csprvstg)
        endif
     end if
     deallocate(cdiagbuf,rdiagbuf)
  end if
  
! End of routine
  return 

  contains

  subroutine check_vars_ (proceed)
  logical,intent(inout) :: proceed
  integer(i_kind) ivar, istatus
! Check to see if required guess fields are available
  call gsi_metguess_get ('var::ps', ivar, istatus )
  proceed=ivar>0
  call gsi_metguess_get ('var::z' , ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::tv', ivar, istatus )
  proceed=proceed.and.ivar>0
  end subroutine check_vars_ 

  subroutine init_vars_

  real(r_kind),dimension(:,:  ),pointer:: rank2
  real(r_kind),dimension(:,:,:),pointer:: rank3
  character(len=5) :: varname
  integer(i_kind) ifld, istatus

! If require guess vars available, extract from bundle ...
  if(size(gsi_metguess_bundle)==nfldsig) then
!    get ps ...
     varname='ps'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_ps))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_ps(size(rank2,1),size(rank2,2),nfldsig))
         ges_ps(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_ps(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get z ...
     varname='z'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_z))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_z(size(rank2,1),size(rank2,2),nfldsig))
         ges_z(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_z(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get tv ...
     varname='tv'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_tv))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_tv(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_tv(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_tv(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
  else
     write(6,*) trim(myname), ': inconsistent vector sizes (nfldsig,size(metguess_bundle) ',&
                 nfldsig,size(gsi_metguess_bundle)
     call stop2(999)
  endif
  end subroutine init_vars_

  subroutine init_netcdf_diag_
  character(len=80) string
  character(len=128) diag_conv_file
  integer(i_kind) ncd_fileid,ncd_nobs
  logical append_diag
  logical,parameter::verbose=.false.
     write(string,900) jiter
900  format('conv_ps_',i2.2,'.nc4')
     diag_conv_file=trim(dirname) // trim(string)

     inquire(file=diag_conv_file, exist=append_diag)

     if (append_diag) then
        call nc_diag_read_init(diag_conv_file,ncd_fileid)
        ncd_nobs = nc_diag_read_get_dim(ncd_fileid,'nobs')
        call nc_diag_read_close(diag_conv_file)

        if (ncd_nobs > 0) then
           if(verbose) print *,'file ' // trim(diag_conv_file) // ' exists.  Appending.  nobs,mype=',ncd_nobs,mype
        else
           if(verbose) print *,'file ' // trim(diag_conv_file) // ' exists but contains no obs.  Not appending. nobs,mype=',ncd_nobs,mype
           append_diag = .false. ! if there are no obs in existing file, then do not try to append
        endif
     end if

     call nc_diag_init(diag_conv_file, append=append_diag)

     if (.not. append_diag) then ! don't write headers on append - the module will break?
        call nc_diag_header("date_time",ianldate )
        if (save_jacobian) then
          call nc_diag_header("jac_nnz", nnz)
          call nc_diag_header("jac_nind", nind)
        endif
     endif

  end subroutine init_netcdf_diag_
  subroutine contents_binary_diag_(odiag)
  type(obs_diag),pointer,intent(in):: odiag
        cdiagbuf(ii)    = station_id         ! station id

        rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
        rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype
        rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
        rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
        rdiagbuf(5,ii)  = data(istnelv,i)    ! station elevation (meters)
        rdiagbuf(6,ii)  = data(ipres,i)*r10  ! observation pressure (hPa)
        rdiagbuf(7,ii)  = dhgt               ! observation height (meters)
        rdiagbuf(8,ii)  = dtime-time_offset  ! obs time (hours relative to analysis time)

        rdiagbuf(9,ii)  = data(iqc,i)        ! input prepbufr qc or event mark
        rdiagbuf(10,ii) = var_jb             ! non linear qc parameter
        rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
        if(muse(i)) then
           rdiagbuf(12,ii) = one             ! analysis usage flag (1=use, -1=not used)
        else
           rdiagbuf(12,ii) = -one                    
        endif

        rdiagbuf(13,ii) = rwgt               ! nonlinear qc relative weight
        rdiagbuf(14,ii) = errinv_input       ! prepbufr inverse obs error (hPa**-1)
        rdiagbuf(15,ii) = errinv_adjst       ! read_prepbufr inverse obs error (hPa**-1)
        rdiagbuf(16,ii) = errinv_final       ! final inverse observation error (hPa**-1)

        rdiagbuf(17,ii) = pob                ! surface pressure observation (hPa)
        rdiagbuf(18,ii) = pob-pges           ! obs-ges used in analysis (coverted to hPa)
        rdiagbuf(19,ii) = pob-pgesorig       ! obs-ges w/o adjustment to guess surface pressure (hPa)
        rdiagbuf(20,ii) = 1.e+10_r_single    ! spread (filled in by EnKF)

        ioff=ioff0

       if (lobsdiagsave) then
           do jj=1,miter
              ioff=ioff+1
              if (odiag%muse(jj)) then
                 rdiagbuf(ioff,ii) = one
              else
                 rdiagbuf(ioff,ii) = -one
              endif
           enddo
           do jj=1,miter+1
              ioff=ioff+1
              rdiagbuf(ioff,ii) = odiag%nldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = odiag%tldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = odiag%obssen(jj)
           enddo
       endif

        if (twodvar_regional .or. l_obsprvdiag) then
           ioff = ioff + 1
           rdiagbuf(ioff,ii) = data(idomsfc,i) ! dominate surface type
           ioff = ioff + 1
           rdiagbuf(ioff,ii) = data(izz,i)     ! model terrain at ob location
           r_prvstg            = data(iprvd,i)
           cprvstg(ii)         = c_prvstg        ! provider name
           r_sprvstg           = data(isprvd,i)
           csprvstg(ii)        = c_sprvstg       ! subprovider name
        endif
        if (save_jacobian) then
           call writearray(dhx_dx, rdiagbuf(ioff+1:nreal, ii))
           ioff = ioff + size(dhx_dx)
        endif

  end subroutine contents_binary_diag_
  subroutine contents_netcdf_diag_(odiag)
  type(obs_diag),pointer,intent(in):: odiag
! Observation class
  character(7),parameter     :: obsclass = '     ps'
  real(r_kind),dimension(miter) :: obsdiag_iuse
           call nc_diag_metadata("Station_ID",              station_id             )
           call nc_diag_metadata("Observation_Class",       obsclass               )
           call nc_diag_metadata("Observation_Type",        ictype(ikx)            )
           call nc_diag_metadata("Observation_Subtype",     icsubtype(ikx)         )
           !Replace direct calls to nc_diag_metadata with the screening subroutine
           call nc_diag_metadata_to_single("Latitude",      data(ilate,i)          )
           call nc_diag_metadata_to_single("Longitude",     data(ilone,i)          )
           call nc_diag_metadata_to_single("Station_Elevation",data(istnelv,i)     )
           call nc_diag_metadata_to_single("Pressure",      data(ipres,i),r10,'*'  )
           call nc_diag_metadata_to_single("Height",        dhgt                   )
           call nc_diag_metadata_to_single("Time",          dtime,time_offset,'-'  )
           call nc_diag_metadata_to_single("Prep_QC_Mark",  data(iqc,i)            )
           call nc_diag_metadata_to_single("Prep_Use_Flag", data(iuse,i)           )
           call nc_diag_metadata_to_single("Nonlinear_QC_Var_Jb",var_jb            )
           call nc_diag_metadata_to_single("Nonlinear_QC_Rel_Wgt",rwgt             )
           if(muse(i)) then
              call nc_diag_metadata("Analysis_Use_Flag",    1.0_r_single           )
           else
              call nc_diag_metadata("Analysis_Use_Flag",   -1.0_r_single           )
           endif

           call nc_diag_metadata_to_single("Errinv_Input",  errinv_input           )
           call nc_diag_metadata_to_single("Errinv_Adjust", errinv_adjst           )
           call nc_diag_metadata_to_single("Errinv_Final",  errinv_final           )

           call nc_diag_metadata_to_single("Observation",   pob                    )
           call nc_diag_metadata_to_single("Obs_Minus_Forecast_adjusted",pob,pges,'-')
           call nc_diag_metadata_to_single("Obs_Minus_Forecast_unadjusted",pob,pgesorig,'-')

          if (lobsdiagsave) then

              do jj=1,miter
                 if (odiag%muse(jj)) then
                       obsdiag_iuse(jj) =  one
                 else
                       obsdiag_iuse(jj) = -one
                 endif
              enddo
   
              call nc_diag_data2d("ObsDiagSave_iuse",     obsdiag_iuse                             )
              call nc_diag_data2d("ObsDiagSave_nldepart", odiag%nldepart )
              call nc_diag_data2d("ObsDiagSave_tldepart", odiag%tldepart )
              call nc_diag_data2d("ObsDiagSave_obssen",   odiag%obssen   )             
          endif
   
           if (twodvar_regional .or. l_obsprvdiag) then
              call nc_diag_metadata("Dominant_Sfc_Type", data(idomsfc,i)              )
              call nc_diag_metadata("Model_Terrain",     data(izz,i)                  )
              r_prvstg            = data(iprvd,i)
              call nc_diag_metadata("Provider_Name",     c_prvstg                     )    
              r_sprvstg           = data(isprvd,i)
              call nc_diag_metadata("Subprovider_Name",  c_sprvstg                    )
           endif

           if (save_jacobian) then
             call nc_diag_data2d("Observation_Operator_Jacobian_stind", dhx_dx%st_ind)
             call nc_diag_data2d("Observation_Operator_Jacobian_endind", dhx_dx%end_ind)
             call nc_diag_data2d("Observation_Operator_Jacobian_val", real(dhx_dx%val,r_single))
           endif

  end subroutine contents_netcdf_diag_

  subroutine final_vars_
    if(allocated(ges_tv)) deallocate(ges_tv)
    if(allocated(ges_z )) deallocate(ges_z )
    if(allocated(ges_ps)) deallocate(ges_ps)
  end subroutine final_vars_

end subroutine setupps
end module ps_setup
