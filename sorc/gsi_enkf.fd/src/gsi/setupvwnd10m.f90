module vwnd10m_setup
  implicit none
  private
  public:: setup
        interface setup; module procedure setupvwnd10m; end interface

contains
subroutine setupvwnd10m(obsLL,odiagLL,lunin,mype,bwork,awork,nele,nobs,is,conv_diagsave)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setupvwnd10m    compute rhs for conventional 10 m vwind 
!   prgmmr: pondeca           org: np23                date: 2016-03-07
!
! abstract: For 10-m uwind observations
!              a) reads obs assigned to given mpi task (geographic region),
!              b) simulates obs from guess,
!              c) apply some quality control to obs,
!              d) load weight and innovation arrays used in minimization
!              e) collects statistics for runtime diagnostic output
!              f) writes additional diagnostic information to output file
!
! program history log:
!   2016-03-07  pondeca
!   2016-06-24  guo     - fixed the default value of obsdiags(:,:)%tail%luse to luse(i)
!                       . removed (%dlat,%dlon) debris.
!   2016-10-07  pondeca - if(.not.proceed) advance through input file first
!                         before retuning to setuprhsall.f90
!   2017-02-09  guo     - Remove m_alloc, n_alloc.
!                       . Remove my_node with corrected typecast().
!   2017-03-15  Yang    - modify code to use polymorphic code.
!   2017-09-28  todling - add netcdf_diag capability; hidden as contained code
!   2018-01-08  pondeca - addd option l_closeobs to use closest obs to analysis
!                                     time in analysis
!   2020-05-04  wu   - no rotate_wind for fv3_regional
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
  use mpeu_util, only: die,perr
  use kinds, only: r_kind,r_single,r_double,i_kind

  use guess_grids, only: hrdifsig,nfldsig,ges_lnprsl, &
               sfcmod_gfs,sfcmod_mm5,comp_fact10
  use m_obsdiagNode, only: obs_diag
  use m_obsdiagNode, only: obs_diags
  use m_obsdiagNode, only: obsdiagLList_nextNode
  use m_obsdiagNode, only: obsdiagNode_set
  use m_obsdiagNode, only: obsdiagNode_get
  use m_obsdiagNode, only: obsdiagNode_assert

  use obsmod, only: rmiss_single,&
                    lobsdiagsave,nobskeep,lobsdiag_allocated,time_offset,bmiss
  use m_obsNode    , only: obsNode
  use m_vwnd10mNode, only: vwnd10mNode
  use m_vwnd10mNode, only: vwnd10mNode_appendto
  use m_obsLList   , only: obsLList
  use obsmod, only: luse_obsdiag
  use obsmod, only: netcdf_diag, binary_diag, dirname, ianldate
  use nc_diag_write_mod, only: nc_diag_init, nc_diag_header, nc_diag_metadata, &
       nc_diag_write, nc_diag_data2d
  use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_get_dim, nc_diag_read_close
  use gsi_4dvar, only: nobs_bins,hr_obsbin,min_offset
  use oneobmod, only: magoberr,maginnov,oneobtest

  use gridmod, only: nsig
  use gridmod, only: get_ij,twodvar_regional,regional,rotate_wind_xy2ll,pt_ll,&
            fv3_regional
  use constants, only: zero,tiny_r_kind,one,one_tenth,half,wgtlim,rd,grav,&
            two,cg_term,three,four,five,ten,huge_single,r1000,r3600,&
            grav_ratio,flattening,grav,deg2rad,grav_equator,somigliana, &
            semi_major_axis
  use jfunc, only: jiter,last,miter
  use qcmod, only: dfact,dfact1,npres_print,qc_satwnds
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype
  use convinfo, only: icsubtype
  use m_dtime, only: dtime_setup, dtime_check
  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle
  use rapidrefresh_cldsurf_mod, only: l_closeobs
  implicit none

! Declare passed variables
  type(obsLList ),target,dimension(:),intent(in):: obsLL
  type(obs_diags),target,dimension(:),intent(in):: odiagLL

  logical                                          ,intent(in   ) :: conv_diagsave
  integer(i_kind)                                  ,intent(in   ) :: lunin,mype,nele,nobs
  real(r_kind),dimension(100+7*nsig)               ,intent(inout) :: awork
  real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout) :: bwork
  integer(i_kind)                                  ,intent(in   ) :: is	! ndat index

! Declare external calls for code analysis
  external:: tintrp2a1,tintrp2a11
  external:: stop2

! Declare local parameters
  real(r_kind),parameter:: r0_7=0.7_r_kind
  real(r_kind),parameter:: r6=6.0_r_kind
  real(r_kind),parameter:: r20=20.0_r_kind
  real(r_kind),parameter:: r360=360.0_r_kind
  real(r_kind),parameter:: r0_1_bmiss=one_tenth*bmiss
  character(len=*),parameter:: myname='setupvwnd10m'

! Declare local variables
  
  integer(i_kind) num_bad_ikx

  real(r_double) rstation_id

  real(r_kind) spdges,dlat,dlon,ddiff,dtime,error,prsln2,r0_001,thirty
  real(r_kind) scale,val2,rsig,rsigp,ratio,ressw2,ress,residual,dudiff,dvdiff
  real(r_kind) obserrlm,obserror,val,valqc,dx10,rlow,rhgh,drpx,prsfc
  real(r_kind) term,rwgt
  real(r_kind) cg_vwnd10m,wgross,wnotgross,wgt,arg,exp_arg,rat_err2,qcgross
  real(r_kind) presw,factw,dpres,sfcchk,ugesin,vgesin,dpressave
  real(r_kind) qcu,qcv
  real(r_kind) ratio_errors,tfact,wflate,psges,goverrd,spdob
  real(r_kind) uob,vob,spdb
  real(r_kind) dudiff_opp, dvdiff_opp, vecdiff, vecdiff_opp
  real(r_kind) ascat_vec
  real(r_kind) errinv_input,errinv_adjst,errinv_final
  real(r_kind) err_input,err_adjst,err_final,skint,sfcr
  real(r_kind) uob_reg,vob_reg,uob_e,vob_e,dlon_e,uges_e,vges_e,dudiff_e,dvdiff_e
  real(r_kind),dimension(nobs):: dup
  real(r_kind),dimension(nsig)::prsltmp,tges
  real(r_kind) wdirob,wdirgesin,wdirdiffmax
  real(r_kind),dimension(nele,nobs):: data
  real(r_single),allocatable,dimension(:,:)::rdiagbuf


  integer(i_kind) ier,ier2,ilon,ilat,ihgt,iuob,ivob,ipres,id,itime,ikx,iqc
  integer(i_kind) iuse,ilate,ilone,ielev,izz,iprvd,isprvd
  integer(i_kind) i,nchar,nreal,k,ii,ikxx,nn,isli,ibin,ioff,ioff0,jj,itype
  integer(i_kind) l,mm1
  integer(i_kind) idomsfc,iskint,iff10,isfcr
  
  logical,dimension(nobs):: luse,muse
  integer(i_kind),dimension(nobs):: ioid ! initial (pre-distribution) obs ID
  logical lowlevelsat
  logical proceed

  character(8) station_id
  character(8),allocatable,dimension(:):: cdiagbuf
  character(8),allocatable,dimension(:):: cprvstg,csprvstg
  character(8) c_prvstg,c_sprvstg
  real(r_double) r_prvstg,r_sprvstg

  logical:: in_curbin, in_anybin
  type(vwnd10mNode), pointer:: my_head
  type(obs_diag   ), pointer:: my_diag
  type(obs_diags  ), pointer:: my_diagLL
  real(r_kind) :: hr_offset



  equivalence(rstation_id,station_id)
  equivalence(r_prvstg,c_prvstg)
  equivalence(r_sprvstg,c_sprvstg)
  
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_ps
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_z         !will probably need at some point
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_uwnd10m
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_vwnd10m
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_tv
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_wspd10m

  type(obsLList),pointer,dimension(:):: vwnd10mhead
  vwnd10mhead => obsLL(:)

! Check to see if required guess fields are available
  call check_vars_(proceed)
  if(.not.proceed) then
     read(lunin)data,luse   !advance through input file
     return  ! not all vars available, simply return
  endif

! If require guess vars available, extract from bundle ...
  call init_vars_

!*********************************************************************************
! Read and reformat observations in work arrays.
  spdb=zero

  read(lunin)data,luse,ioid

!  index information for data array (see reading routine)
  ier=1       ! index of obs error
  ilon=2      ! index of grid relative obs location (x)
  ilat=3      ! index of grid relative obs location (y)
  ipres=4     ! index of pressure
  ihgt=5      ! index of observation elevation
  iuob=6      ! index of u observation
  ivob=7      ! index of v observation
  id=8        ! index of station id
  itime=9     ! index of observation time in data array
  ikxx=10     ! index of ob type
  ielev=11    ! index of station elevation (m)
  iqc=12      ! index of quality mark
  ier2=13     ! index of original-original obs error ratio
  iuse=14     ! index of use parameter
  idomsfc=15  ! index of dominant surface type
  iskint=16   ! index of surface skin temperature
  iff10=17    ! index of 10 meter wind factor
  isfcr=18    ! index of surface roughness
  ilone=19    ! index of longitude (degrees)
  ilate=20    ! index of latitude (degrees)
  izz=21      ! index of surface height
  iprvd=22    ! index of provider
  isprvd=23   ! index of subprovider

  mm1=mype+1
  scale=one
  rsig=nsig
  thirty = 30.0_r_kind
  r0_001=0.001_r_kind
  rsigp=rsig+one
  goverrd=grav/rd

  do i=1,nobs
     muse(i)=nint(data(iuse,i)) <= jiter .and. nint(data(iqc,i)) < 8
  end do

! Check for missing data
  if (.not. oneobtest) then
  do i=1,nobs
    if (data(iuob,i) > r0_1_bmiss .or. data(ivob,i) > r0_1_bmiss)  then
       muse(i)=.false.
       data(iuob,i)=rmiss_single   ! for diag output
       data(ivob,i)=rmiss_single   ! for diag output
    end if
  end do
  end if

! Check for duplicate observations at same location
  hr_offset=min_offset/60.0_r_kind
  dup=one
  do k=1,nobs
     do l=k+1,nobs
        if(data(ilat,k) == data(ilat,l) .and.  &
           data(ilon,k) == data(ilon,l) .and.  &
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
     ii=0
     nchar=1
     ioff0=23
     nreal=ioff0
     if (lobsdiagsave) nreal=nreal+4*miter+1
     if (twodvar_regional) then; nreal=nreal+2; allocate(cprvstg(nobs),csprvstg(nobs)); endif
     if (binary_diag) allocate(cdiagbuf(nobs),rdiagbuf(nreal,nobs))
     if (netcdf_diag) call init_netcdf_diag_
  end if

  call dtime_setup()
  do i=1,nobs
     dtime=data(itime,i)
     call dtime_check(dtime, in_curbin, in_anybin)
     if(.not.in_anybin) cycle

     if(in_curbin) then
        dlat=data(ilat,i)
        dlon=data(ilon,i)

        ikx  = nint(data(ikxx,i))
        if(ikx < 1 .or. ikx > nconvtype) then
           num_bad_ikx=num_bad_ikx+1
           if(num_bad_ikx<=10) write(6,*)' in setupvwnd10m, bad ikx, ikx,i,nconvtype=',ikx,i,nconvtype
           cycle
        end if

        error=data(ier2,i)
        isli=data(idomsfc,i)
     endif

!    Link observation to appropriate observation bin
     if (nobs_bins>1) then
        ibin = NINT( dtime/hr_obsbin ) + 1
     else
        ibin = 1
     endif
     IF (ibin<1.OR.ibin>nobs_bins) write(6,*)mype,'Error nobs_bins,ibin= ',nobs_bins,ibin

     if(luse_obsdiag) my_diagLL => odiagLL(ibin)

!    Link obs to diagnostics structure
     if(luse_obsdiag)then
        my_diag => obsdiagLList_nextNode(my_diagLL      ,&
                create = .not.lobsdiag_allocated        ,&
                   idv = is             ,&
                   iob = ioid(i)        ,&
                   ich = 1              ,&
                  elat = data(ilate,i)  ,&
                  elon = data(ilone,i)  ,&
                  luse = luse(i)        ,&
                 miter = miter          )

        if(.not.associated(my_diag)) call die(myname,   &
                'obsdiagLList_nextNode(), create =',.not.lobsdiag_allocated)
     end if

     if(.not.in_curbin) cycle

!    Load observation error and values into local variables
     uob = data(iuob,i)
     vob = data(ivob,i)
     spdob=sqrt(uob*uob+vob*vob)
     call tintrp2a11(ges_ps,psges,dlat,dlon,dtime,hrdifsig,&
          mype,nfldsig)
     call tintrp2a1(ges_lnprsl,prsltmp,dlat,dlon,dtime,hrdifsig,&
          nsig,mype,nfldsig)

! Interpolate to get wspd10m at obs location/time
     call tintrp2a11(ges_wspd10m,spdges,dlat,dlon,dtime,hrdifsig,&
          mype,nfldsig)

     itype=ictype(ikx)

!    Process observations with reported pressure
        dpres = data(ipres,i)
        presw = ten*exp(dpres)
        dpres = dpres-log(psges)
        drpx=zero
      
        prsfc=psges
        prsln2=log(exp(prsltmp(1))/prsfc)
        dpressave=dpres

!       Put obs pressure in correct units to get grid coord. number
        dpres=log(exp(dpres)*prsfc)
        call grdcrd1(dpres,prsltmp(1),nsig,-1)
 
!       Interpolate guess u and v to observation location and time.
 
        call tintrp2a11(ges_uwnd10m,ugesin,dlat,dlon,dtime,hrdifsig,&
             mype,nfldsig)
        call tintrp2a11(ges_vwnd10m,vgesin,dlat,dlon,dtime,hrdifsig,&
             mype,nfldsig)

        if(dpressave <= prsln2)then
           factw=one
        else
           factw = data(iff10,i)
           if(sfcmod_gfs .or. sfcmod_mm5) then
              sfcr = data(isfcr,i)
              skint = data(iskint,i)
              call comp_fact10(dlat,dlon,dtime,skint,sfcr,isli,mype,factw)
           end if
 
           call tintrp2a1(ges_tv,tges,dlat,dlon,dtime,hrdifsig,&
              nsig,mype,nfldsig)
!          Apply 10-meter wind reduction factor to guess winds
           dx10=-goverrd*ten/tges(1)
           if (dpressave < dx10)then
              term=(prsln2-dpressave)/(prsln2-dx10)
              factw=one-term+factw*term
           end if
           ugesin=factw*ugesin   
           vgesin=factw*vgesin
 
        end if
       
!       Get approx k value of sfc by using surface pressure
        sfcchk=log(psges)
        call grdcrd1(sfcchk,prsltmp(1),nsig,-1)

!    Checks based on observation location relative to model surface and top
     rlow=max(sfcchk-dpres,zero)
     rhgh=max(dpres-r0_001-rsigp,zero)
     if(luse(i))then
        awork(1) = awork(1) + one
        if(rlow/=zero) awork(2) = awork(2) + one
        if(rhgh/=zero) awork(3) = awork(3) + one
     end if
 
!    Adjust observation error
     wflate=zero
     if (ictype(ikx)==288 .or. ictype(ikx)==295) then
       if (spdob<one .and. spdges >=ten ) wflate=four*data(ier,i) ! Tyndall/Horel type QC
     endif

     ratio_errors=error/(data(ier,i)+drpx+wflate+1.0e6_r_kind*rhgh+four*rlow)

!    Invert observation error
     error=one/error

!    Check to see if observation below model surface or above model top.
!    If so, don't use observation
     if (dpres > rsig )then
        if( regional .and. presw > pt_ll )then
           dpres=rsig
        else
           ratio_errors=zero
        endif
     endif

!    Compute innovations
     lowlevelsat=itype==242.or.itype==243.or.itype==245.or.itype==246.or. &
                 itype==247.or.itype==250.or.itype==251.or.itype==252.or. &
                 itype==253.or.itype==254.or.itype==257.or.itype==258.or. &
                 itype==259.or.itype==241
     if (lowlevelsat .and. twodvar_regional) then
         call windfactor(presw,factw)
         data(iuob,i)=factw*data(iuob,i)
         data(ivob,i)=factw*data(ivob,i)
         uob = data(iuob,i)
         vob = data(ivob,i)
     endif
     dudiff=uob-ugesin
     dvdiff=vob-vgesin
     spdb=sqrt(uob**2+vob**2)-sqrt(ugesin**2+vgesin**2)

     ddiff=dvdiff

     if ( qc_satwnds ) then
        if(itype >=240 .and. itype <=260) then
           if( presw >950.0_r_kind) error =zero    !  screen data beloww 950mb
        endif
        if( itype == 246 .or. itype == 250 .or. itype == 254 )   then     !  water vapor cloud top
           if(presw >399.0_r_kind) error=zero
        endif
        if(itype ==258 .and. presw >600.0_r_kind) error=zero
        if(itype ==259 .and. presw >600.0_r_kind) error=zero
     endif ! qc_satwnds

!    QC WindSAT winds
     if (itype==289) then
        qcu = r6
        qcv = r6
        if ( spdob > r20 .or. &          ! high wind speed check
             abs(dudiff) > qcu  .or. &   ! u component check
             abs(dvdiff) > qcv ) then    ! v component check
           error = zero
        endif
     endif

!    QC ASCAT winds
     if (itype==290) then
        qcu = five
        qcv = five
!       Compute innovations for opposite vectors
        dudiff_opp = -uob - ugesin
        dvdiff_opp = -vob - vgesin
        vecdiff = sqrt(dudiff**2 + dvdiff**2)
        vecdiff_opp = sqrt(dudiff_opp**2 + dvdiff_opp**2)
        ascat_vec = sqrt((dudiff**2 + dvdiff**2)/spdob**2)

        if ( abs(dudiff) > qcu  .or. &       ! u component check
             abs(dvdiff) > qcv  .or. &       ! v component check
             vecdiff > vecdiff_opp ) then    ! ambiguity check

           error = zero
        endif
     endif

!    If requested, setup for single obs test.
     if (oneobtest) then
        ddiff=maginnov
        error=one/magoberr
        ratio_errors=one
     endif

!    Gross check using innovation normalized by error
     obserror = one/max(ratio_errors*error,tiny_r_kind)
     obserrlm = max(cermin(ikx),min(cermax(ikx),obserror))
     residual = abs(ddiff)

!    it's probably more robust to evalute gross-error in
!    terms of magnitude of full-vector difference

!!   if ( abs(ugesin)>zero .or. abs(vgesin)>zero ) then
!!      ugesin_scaled=(ugesin/sqrt(ugesin**2+vgesin**2))*spdges
!!      vgesin_scaled=(vgesin/sqrt(ugesin**2+vgesin**2))*spdges
!!      residual = sqrt((uob-ugesin_scaled)**2+(vob-vgesin_scaled)**2)
!!    else
!!      residual = sqrt(dudiff**2+dvdiff**2)
!!   endif

!!   residual = sqrt(dudiff**2+dvdiff**2)
     ratio    = residual/obserrlm

!!   modify cgross depending on the quality mark, qcmark=3, cgross=0.7*cgross
!!   apply asymetric gross check for satellite winds
     qcgross=cgross(ikx)
     if(data(iqc,i) == three) qcgross=r0_7*cgross(ikx)

     if(spdb <0 )then
        if(itype ==244) then   ! AVHRR, use same as MODIS
          qcgross=r0_7*cgross(ikx)
        endif
        if(itype >=257 .and. itype <=259 ) then
          qcgross=r0_7*cgross(ikx)
        endif
     endif

     if (ratio> qcgross .or. ratio_errors < tiny_r_kind) then
        if (luse(i)) awork(6) = awork(6)+one
        error = zero
        ratio_errors=zero
     else
        ratio_errors =ratio_errors/sqrt(dup(i))
     end if

     if (lowlevelsat .and. twodvar_regional) then
        if (data(idomsfc,i) /= 0 .and. data(idomsfc,i) /= 3 ) then
           error = zero
           ratio_errors = zero
        endif
     endif

     if (twodvar_regional) then
        if (lowlevelsat .or. itype==289 .or. itype==290) then
            wdirdiffmax=45._r_kind
          else
           wdirdiffmax=100000._r_kind
        endif
        if (spdob > zero .and. (spdob-spdb) > zero) then
           call getwdir(uob,vob,wdirob)
           call getwdir(ugesin,vgesin,wdirgesin)
           if ( min(abs(wdirob-wdirgesin),abs(wdirob-wdirgesin+r360), &
                          abs(wdirob-wdirgesin-r360)) > wdirdiffmax ) then
               error = zero
               ratio_errors = zero
           endif
        endif
     endif

     if (ratio_errors*error <=tiny_r_kind) muse(i)=.false.

     if (nobskeep>0 .and. luse_obsdiag) call obsdiagNode_get(my_diag, jiter=nobskeep, muse=muse(i))

!    Compute penalty terms (linear & nonlinear qc).
     val      = error*ddiff
     if(luse(i))then
        val2     = val*val
        exp_arg  = -half*val2
        rat_err2 = ratio_errors**2
        if (cvar_pg(ikx) > tiny_r_kind .and. error > tiny_r_kind) then
           arg  = exp(exp_arg)
           wnotgross= one-cvar_pg(ikx)
           cg_vwnd10m=cvar_b(ikx)
           wgross = cg_term*cvar_pg(ikx)/(cg_vwnd10m*wnotgross)
           term = log((arg+wgross)/(one+wgross))
           wgt  = one-wgross/(arg+wgross)
           rwgt = wgt/wgtlim
        else
           term = exp_arg
           wgt  = wgtlim
           rwgt = wgt/wgtlim
        endif
        valqc = -two*rat_err2*term

!       Accumulate statistics for obs belonging to this task
        if (muse(i)) then
           if(rwgt < one) awork(21) = awork(21)+one
           awork(4)=awork(4)+val2*rat_err2
           awork(5)=awork(5)+one
           awork(22)=awork(22)+valqc
        end if
        ress   = ddiff*scale
        ressw2 = ress*ress
        val2   = val*val
        rat_err2 = ratio_errors**2
        nn=1
        if (.not. muse(i)) then
           nn=2
           if(ratio_errors*error >=tiny_r_kind)nn=3
        end if
        if (abs(data(ivob,i)-rmiss_single) >=tiny_r_kind) then
           bwork(1,ikx,1,nn)  = bwork(1,ikx,1,nn)+one           ! count
           bwork(1,ikx,2,nn)  = bwork(1,ikx,2,nn)+ress          ! (o-g)
           bwork(1,ikx,3,nn)  = bwork(1,ikx,3,nn)+ressw2        ! (o-g)**2
           bwork(1,ikx,4,nn)  = bwork(1,ikx,4,nn)+val2*rat_err2 ! penalty
           bwork(1,ikx,5,nn)  = bwork(1,ikx,5,nn)+valqc         ! nonlin qc penalty
        end if

     endif

     if(luse_obsdiag)then
        call obsdiagNode_set(my_diag, wgtjo=(error*ratio_errors)**2, &
           jiter=jiter, muse=muse(i), nldepart=ddiff)
     end if

!    If obs is "acceptable", load array with obs info for use
!    in inner loop minimization (int* and stp* routines)
     if (.not. last .and. muse(i)) then
        allocate(my_head)
        call vwnd10mNode_appendto(my_head,vwnd10mhead(ibin))

        my_head%idv = is
        my_head%iob = ioid(i)
        my_head%elat= data(ilate,i)
        my_head%elon= data(ilone,i)

!       Set (i,j) indices of guess gridpoint that bound obs location
        call get_ij(mm1,dlat,dlon,my_head%ij,my_head%wij)



        my_head%res     = ddiff
        my_head%err2    = error**2
        my_head%raterr2 = ratio_errors**2    
        my_head%time    = dtime
        my_head%b       = cvar_b(ikx)
        my_head%pg      = cvar_pg(ikx)
        my_head%luse    = luse(i)

        if(luse_obsdiag)then
          call obsdiagNode_assert(my_diag, my_head%idv,my_head%iob,1, myname,'my_diag:my_head')
          my_head%diags => my_diag
        end if
        my_head => null ()
     endif


!    Save stuff for diagnostic output
     if(conv_diagsave .and. luse(i))then
        ii=ii+1
        rstation_id = data(id,i)
        err_input   = data(ier2,i)
        err_adjst   = data(ier,i)
        if (ratio_errors*error>tiny_r_kind) then
           err_final = one/(ratio_errors*error)
        else
           err_final = huge_single
        endif
 
        errinv_input = huge_single
        errinv_adjst = huge_single
        errinv_final = huge_single
        if (err_input>tiny_r_kind) errinv_input = one/err_input
        if (err_adjst>tiny_r_kind) errinv_adjst = one/err_adjst
        if (err_final>tiny_r_kind) errinv_final = one/err_final

        if (binary_diag) call contents_binary_diag_(my_diag)
        if (netcdf_diag) call contents_netcdf_diag_(my_diag)

     end if


  end do

! Release memory of local guess arrays
  call final_vars_

! Write information to diagnostic file
  if(conv_diagsave .and. ii>0)then
     if(netcdf_diag) call nc_diag_write
     if(binary_diag) then
       write(7)'uwn',nchar,nreal,ii,mype,ioff0
       write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
       deallocate(cdiagbuf,rdiagbuf)

       if (twodvar_regional) then
          write(7)cprvstg(1:ii),csprvstg(1:ii)
          deallocate(cprvstg,csprvstg)
       endif
     endif
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
  call gsi_metguess_get ('var::uwnd10m', ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::vwnd10m', ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::wspd10m', ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::tv', ivar, istatus )
  proceed=proceed.and.ivar>0
  end subroutine check_vars_ 

  subroutine init_vars_

  real(r_kind),dimension(:,:  ),pointer:: rank2=>NULL()
  real(r_kind),dimension(:,:,:),pointer:: rank3=>NULL()
  character(len=10) :: varname
  integer(i_kind) ifld, istatus

! If require guess vars available, extract from bundle ...
  if(size(gsi_metguess_bundle)==nfldsig) then
!    get uwnd10m ...
     varname='uwnd10m'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_uwnd10m))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_uwnd10m(size(rank2,1),size(rank2,2),nfldsig))
         ges_uwnd10m(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_uwnd10m(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get vwnd10m ...
     varname='vwnd10m'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_vwnd10m))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_vwnd10m(size(rank2,1),size(rank2,2),nfldsig))
         ges_vwnd10m(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_vwnd10m(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get wspd10m ...
     varname='wspd10m'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_wspd10m))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_wspd10m(size(rank2,1),size(rank2,2),nfldsig))
         ges_wspd10m(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_wspd10m(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
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
900  format('conv_v10m_',i2.2,'.nc4')
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
     endif
  end subroutine init_netcdf_diag_
  subroutine contents_binary_diag_(odiag)
  type(obs_diag),pointer,intent(in):: odiag
        cdiagbuf(ii)    = station_id         ! station id
        rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
        rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype
 
        rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
        rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
        rdiagbuf(5,ii)  = data(ielev,i)    ! station elevation (meters)
        rdiagbuf(6,ii)  = presw              ! observation pressure (hPa)
        rdiagbuf(7,ii)  = data(ihgt,i)       ! observation height (meters)
        rdiagbuf(8,ii)  = dtime-time_offset  ! obs time (hours relative to analysis time)

        rdiagbuf(9,ii)  = data(iqc,i)        ! input prepbufr qc or event mark
        rdiagbuf(10,ii) = rmiss_single       ! setup qc or event mark
        rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
        if(muse(i)) then
           rdiagbuf(12,ii) = one             ! analysis usage flag (1=use, -1=not used)
        else
           rdiagbuf(12,ii) = -one
        endif

        rdiagbuf(13,ii) = rwgt               ! nonlinear qc relative weight
        rdiagbuf(14,ii) = errinv_input       ! prepbufr inverse obs error (ms**-1)
        rdiagbuf(15,ii) = errinv_adjst       ! read_prepbufr inverse obs error (ms**-1)
        rdiagbuf(16,ii) = errinv_final       ! final inverse observation error (ms**-1)
 
        rdiagbuf(17,ii) = data(ivob,i)       ! 10m vwind observation (ms**-1)
        rdiagbuf(18,ii) = ddiff              ! obs-ges used in analysis (ms**-1)
        rdiagbuf(19,ii) = data(ivob,i)-vgesin! obs-ges w/o bias correction (ms**-1) (future slot)

        rdiagbuf(20,ii) = data(iuob,i)       ! 10m vwind observation (ms**-1)
        rdiagbuf(21,ii) = dudiff             ! uob-ges (ms**-1)
        rdiagbuf(22,ii) = data(iuob,i)-ugesin! uob-ges w/o bias correction (ms**-1) (future slot)
 
        if(regional .and. .not. fv3_regional) then

!           replace positions 17-22 with earth relative wind component information

           uob_reg=data(iuob,i)
           vob_reg=data(ivob,i)
           dlon_e=data(ilone,i)*deg2rad
           call rotate_wind_xy2ll(uob_reg,vob_reg,uob_e,vob_e,dlon_e,dlon,dlat)
           call rotate_wind_xy2ll(ugesin,vgesin,uges_e,vges_e,dlon_e,dlon,dlat)
           call rotate_wind_xy2ll(ddiff,dvdiff,dudiff_e,dvdiff_e,dlon_e,dlon,dlat)
           rdiagbuf(17,ii) = uob_e         ! earth relative u wind component observation (m/s)
           rdiagbuf(18,ii) = dudiff_e      ! earth relative u obs-ges used in analysis (m/s)
           rdiagbuf(19,ii) = uob_e-uges_e  ! earth relative u obs-ges w/o bias correction (m/s) (future slot)

           rdiagbuf(20,ii) = vob_e         ! earth relative v wind component observation (m/s)
           rdiagbuf(21,ii) = dvdiff_e      ! earth relative v obs-ges used in analysis (m/s)
           rdiagbuf(22,ii) = vob_e-vges_e  ! earth relative v obs-ges w/o bias correction (m/s) (future slot)
        end if

        rdiagbuf(23,ii) = factw              ! 10m wind reduction factor

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

        if (twodvar_regional) then
           rdiagbuf(ioff+1,ii) = data(idomsfc,i)    ! dominant surface type
           rdiagbuf(ioff+2,ii) = data(izz,i)        ! model terrain at ob location
           r_prvstg        = data(iprvd,i)
           cprvstg(ii)     = c_prvstg               ! provider name
           r_sprvstg       = data(isprvd,i)
           csprvstg(ii)    = c_sprvstg              ! subprovider name
        endif
 
  end subroutine contents_binary_diag_
  subroutine contents_netcdf_diag_(odiag)
  type(obs_diag),pointer,intent(in):: odiag
! Observation class
  character(7),parameter     :: obsclass = 'vwnd10m'
  real(r_kind),dimension(miter) :: obsdiag_iuse

        call nc_diag_metadata("Station_ID",              station_id             )
        call nc_diag_metadata("Observation_Class",       obsclass               )
        call nc_diag_metadata("Observation_Type",        ictype(ikx)            )
        call nc_diag_metadata("Observation_Subtype",     icsubtype(ikx)         )

        call nc_diag_metadata("Latitude",                data(ilate,i)          )
        call nc_diag_metadata("Longitude",               data(ilone,i)          )
        call nc_diag_metadata("Station_Elevation",       data(ielev,i)          )
        call nc_diag_metadata("Pressure",                presw                  )
        call nc_diag_metadata("Height",                  data(ihgt,i)           )
        call nc_diag_metadata("Time",                    dtime-time_offset      )
        call nc_diag_metadata("Prep_QC_Mark",            data(iqc,i)            )
        call nc_diag_metadata("Setup_QC_Mark",           bmiss                  ) 
        call nc_diag_metadata("Prep_Use_Flag",           data(iuse,i)           )

        if(muse(i)) then
           call nc_diag_metadata("Analysis_Use_Flag",     one                   )
        else
           call nc_diag_metadata("Analysis_Use_Flag",    -one                   )
        endif

        call nc_diag_metadata("Nonlinear_QC_Rel_Wgt",    rwgt                   )
        call nc_diag_metadata("Errinv_Input",            errinv_input           )
        call nc_diag_metadata("Errinv_Adjust",           errinv_adjst           )
        call nc_diag_metadata("Errinv_Final",            errinv_final           )
 
        call nc_diag_metadata("u_Observation", data(ivob,i)                     )
        call nc_diag_metadata("u_Obs_Minus_Forecast_adjusted", ddiff            )
        call nc_diag_metadata("u_Obs_Minus_Forecast_unadjusted",data(ivob,i)-vgesin)

        call nc_diag_metadata("v_Observation", data(iuob,i)                     )
        call nc_diag_metadata("v_Obs_Minus_Forecast_adjusted", dudiff           )
        call nc_diag_metadata("v_Obs_Minus_Forecast_unadjusted", data(iuob,i)-ugesin)

        if(regional .and. .not. fv3_regional) then

!           replace positions 17-22 with earth relative wind component information

           uob_reg=data(iuob,i)
           vob_reg=data(ivob,i)
           dlon_e=data(ilone,i)*deg2rad
           call rotate_wind_xy2ll(uob_reg,vob_reg,uob_e,vob_e,dlon_e,dlon,dlat)
           call rotate_wind_xy2ll(ugesin,vgesin,uges_e,vges_e,dlon_e,dlon,dlat)
           call rotate_wind_xy2ll(ddiff,dvdiff,dudiff_e,dvdiff_e,dlon_e,dlon,dlat)
           rdiagbuf(17,ii) = uob_e         ! earth relative u wind component observation (m/s)
           rdiagbuf(18,ii) = dudiff_e      ! earth relative u obs-ges used in analysis (m/s)
           rdiagbuf(19,ii) = uob_e-uges_e  ! earth relative u obs-ges w/o bias correction (m/s) (future slot)

           rdiagbuf(20,ii) = vob_e         ! earth relative v wind component observation (m/s)
           rdiagbuf(21,ii) = dvdiff_e      ! earth relative v obs-ges used in analysis (m/s)
           rdiagbuf(22,ii) = vob_e-vges_e  ! earth relative v obs-ges w/o bias correction (m/s) (future slot)
           call nc_diag_metadata("u_Observation", uob_e                            )
           call nc_diag_metadata("u_Obs_Minus_Forecast_adjusted", dudiff_e         )
           call nc_diag_metadata("u_Obs_Minus_Forecast_unadjusted",uob_e-uges_e    )

           call nc_diag_metadata("v_Observation", vob_e                            )
           call nc_diag_metadata("v_Obs_Minus_Forecast_adjusted", dvdiff_e         )
           call nc_diag_metadata("v_Obs_Minus_Forecast_unadjusted", vob_e-vges_e   )
        end if

        call nc_diag_metadata("Wind_Reduction_Factor_at_10m", factw                )

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
           call nc_diag_data2d("ObsDiagSave_iuse",     obsdiag_iuse)
           call nc_diag_data2d("ObsDiagSave_nldepart", odiag%nldepart )
           call nc_diag_data2d("ObsDiagSave_tldepart", odiag%tldepart )
           call nc_diag_data2d("ObsDiagSave_obssen",   odiag%obssen   )
        endif

        if (twodvar_regional) then
           call nc_diag_metadata("Dominant_Sfc_Type", data(idomsfc,i))
           call nc_diag_metadata("Model_Terrain",     data(izz,i))
           r_prvstg        = data(iprvd,i)
           cprvstg(ii)     = c_prvstg               ! provider name
           r_sprvstg       = data(isprvd,i)
           csprvstg(ii)    = c_sprvstg              ! subprovider name
        endif
  
  end subroutine contents_netcdf_diag_

  subroutine final_vars_
    if(allocated(ges_z   )) deallocate(ges_z   )
    if(allocated(ges_ps  )) deallocate(ges_ps  )
    if(allocated(ges_tv  )) deallocate(ges_tv  )
    if(allocated(ges_uwnd10m)) deallocate(ges_uwnd10m)
    if(allocated(ges_vwnd10m)) deallocate(ges_vwnd10m)
    if(allocated(ges_wspd10m)) deallocate(ges_wspd10m)
  end subroutine final_vars_

end subroutine setupvwnd10m
end module vwnd10m_setup
