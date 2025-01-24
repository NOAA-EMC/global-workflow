module swcp_setup
  implicit none
  private
  public:: setup
        interface setup; module procedure setupswcp; end interface

contains
subroutine setupswcp(obsLL,odiagLL,lunin,mype,bwork,awork,nele,nobs,is,conv_diagsave)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setupswcp     compute rhs of oi for solid-water condensate path
!   prgmmr: Ting-Chi Wu        org: CIRA/CSU                date: 2017-06-28
!
! abstract:  For solid-water condensate path (swcp), this routine
!              a) reads obs assigned to given mpi task (geographic region),
!              b) simulates obs from guess,
!              c) apply some quality control to obs,
!              d) load weight and innovation arrays used in minimization
!              e) collects statistics for runtime diagnostic output
!              f) writes additional diagnostic information to output file
!
! program history log:
!   2017-06-28  Ting-Chi Wu - mimic the structure in setuppw.f90 and setupbend.f90 
!                           - setupswcp.f90 includes 2 operator options
!                             1) when l_wcp_cwm = .false.: 
!                                operator = f(T,P,q)
!                             2) when l_wcp_cwm = .true. and CWM partition6: 
!                                operator = f(qi,qs,qg,qh) partition6
!   2018-05-10  guo     - Remove m_alloc, n_alloc.
!                       . Remove my_node with corrected typecast().
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
  use kinds, only: r_kind,r_single,r_double,i_kind
  use guess_grids, only: ges_prsi,ges_prsl,ges_tsen,hrdifsig,nfldsig
  use gridmod, only: lat2,lon2,nsig,get_ij,latlon11
  use m_obsdiagNode, only: obs_diag
  use m_obsdiagNode, only: obs_diags
  use m_obsdiagNode, only: obsdiagLList_nextNode
  use m_obsdiagNode, only: obsdiagNode_set
  use m_obsdiagNode, only: obsdiagNode_get
  use m_obsdiagNode, only: obsdiagNode_assert

  use obsmod, only: rmiss_single,lobsdiag_forenkf,ianldate,&
                    lobsdiagsave,nobskeep,lobsdiag_allocated,time_offset
  use obsmod, only: l_wcp_cwm
  use m_obsNode, only: obsNode
  use m_swcpNode, only: swcpNode
  use m_swcpNode, only: swcpNode_appendto
  use m_obsLList, only: obsLList
  use obsmod, only: luse_obsdiag
  use gsi_4dvar, only: nobs_bins,hr_obsbin

  use obsmod, only: netcdf_diag, binary_diag, dirname
  use nc_diag_write_mod, only: nc_diag_init, nc_diag_header,nc_diag_metadata, &
       nc_diag_write, nc_diag_data2d, nc_diag_metadata_to_single
  use nc_diag_read_mod, only: nc_diag_read_init,nc_diag_read_get_dim,nc_diag_read_close
  use state_vectors, only: svars3d, levels

  use constants, only: zero,one,tpwcon,r1000,r10, &
       tiny_r_kind,three,half,two,cg_term,huge_single,&
       wgtlim, ttp, tmix, psatk, xa, xai, xb, xbi
  use jfunc, only: jiter,last,jiterstart,miter
  use qcmod, only: dfact,dfact1,npres_print
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype
  use convinfo, only: icsubtype
  use m_dtime, only: dtime_setup, dtime_check
  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle
  use gfs_stratosphere, only: use_gfs_stratosphere, nsig_save

  use sparsearr, only: sparr2, new, size, writearray, fullarray

  implicit none

! Declare passed variables
  type(obsLList ),target,dimension(:),intent(in):: obsLL
  type(obs_diags),target,dimension(:),intent(in):: odiagLL

  logical                                          ,intent(in   ) :: conv_diagsave
  integer(i_kind)                                  ,intent(in   ) :: lunin,mype,nele,nobs
  real(r_kind),dimension(100+7*nsig)               ,intent(inout) :: awork
  real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout) :: bwork
  integer(i_kind)                                  ,intent(in   ) :: is ! ndat index

! Declare local parameter
  character(len=*),parameter:: myname='setupswcp'

! Declare external calls for code analysis
  external:: tintrp2a1,tintrp2a11
  external:: stop2

! Declare local variables
  real(r_double) rstation_id
  real(r_kind):: swcpges,grsmlt,dlat,dlon,dtime,obserror, &
       obserrlm,residual,ratio,dswcp
  real(r_kind) error,ddiff
  real(r_kind) ressw2,ress,scale,val2,val,valqc
  real(r_kind) rat_err2,exp_arg,term,ratio_errors,rwgt
  real(r_kind) cg_swcp,wgross,wnotgross,wgt,arg
  real(r_kind) errinv_input,errinv_adjst,errinv_final
  real(r_kind) err_input,err_adjst,err_final,tfact
  real(r_kind),dimension(nobs)::dup
  real(r_kind),dimension(nele,nobs):: data
  real(r_single),allocatable,dimension(:,:)::rdiagbuf

  integer(i_kind) ikxx,nn,istat,ibin,ioff,ioff0
  integer(i_kind) i,nchar,nreal,k,j,jj,ii,l,mm1
  integer(i_kind) ier,ilon,ilat,iswcp,id,itime,ikx,iswcpmax,iqc
  integer(i_kind) ier2,iuse,ilate,ilone,istnelv,iobshgt,iobsprs

  logical,dimension(nobs):: luse,muse
  integer(i_kind),dimension(nobs):: ioid ! initial (pre-distribution) obs ID
  logical proceed
  
  type(sparr2) :: dhx_dx
  integer(i_kind) :: qi_ind, nind, nnz

  character(8) station_id
  character(8),allocatable,dimension(:):: cdiagbuf

  logical:: in_curbin, in_anybin,save_jacobian
  type(swcpNode),pointer:: my_head
  type(obs_diag),pointer:: my_diag
  type(obs_diags),pointer:: my_diagLL

  equivalence(rstation_id,station_id)
  integer(i_kind),dimension(4) :: swcp_ij
  integer(i_kind) :: nsig_top

  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_q
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_qi
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_qs
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_qg
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_qh
  real(r_kind),dimension(lat2,lon2,nfldsig)::ges_swcp
  real(r_kind),dimension(nsig+1):: piges
  real(r_kind),dimension(nsig):: qges, plges, tges
  real(r_kind),dimension(nsig):: trges, wges, dwdt
  real(r_kind),dimension(nsig):: esges, eslges, esiges
  real(r_kind),dimension(nsig):: desdt, desldt, desidt
  real(r_kind),dimension(nsig):: dssqdq, dssqdt, dssqdp
  real(r_kind),dimension(nsig):: qvges, qvsges, ssqges
  real(r_kind),dimension(nsig):: qiges, qsges, qgges, qhges
  real(r_kind) :: tupper, tlower, tcenter
  real(r_kind),dimension(lat2,lon2,nsig,nfldsig)::qv, esi, esl, es, qvsi, ssqvi
  real(r_kind),dimension(lat2,lon2,nsig,nfldsig)::ges_tr, ges_w

  type(obsLList),pointer,dimension(:):: swcphead
  swcphead => obsLL(:)

  save_jacobian = conv_diagsave .and. jiter==jiterstart .and. lobsdiag_forenkf

  grsmlt=three  ! multiplier factor for gross check
  mm1=mype+1
  scale=one

! Check to see if required guess fields are available
  call check_vars_(proceed)
  if(.not.proceed) return  ! not all vars available, simply return

! If require guess vars available, extract from bundle ...
  call init_vars_

!******************************************************************************
! Read and reformat observations in work arrays.

!=============================================================================================================
! Operator for swcp (solid-water content path w.r.t ice forward model)

  if (use_gfs_stratosphere) then
    nsig_top = nsig_save 
  else
    nsig_top = nsig
  endif

  tupper = ttp
  tlower = tmix

  if (.not.l_wcp_cwm) then
    esi = zero; esl = zero; es = zero
    qvsi = zero; ssqvi = zero
    ges_swcp = zero

    tcenter = 0.5_r_kind * (tupper + tlower)
    ges_tr = ttp / ges_tsen
    ges_w = 0.5_r_kind * (one + tanh((ges_tsen-tcenter)/((tupper-tlower)/4._r_kind))) ! hyperbolic tangent
    esl = psatk * (ges_tr**xa) * exp(xb*(one-ges_tr))
    esi = psatk * (ges_tr**xai) * exp(xbi*(one-ges_tr))
    es = ges_w * esl + (one-ges_w) * esi 


    do jj=1,nfldsig
      ! gues_q is acquired through gsi_bundlegetpointer in the init_vars_ call
      qv(:,:,:,jj) = ges_q(:,:,:,jj) / (one - ges_q(:,:,:,jj)) ! kg/kg

      do k=1,nsig
        do j=1,lon2
          do i=1,lat2
            if (ges_tsen(i,j,k,jj) < tupper .and. k <= nsig_top ) then
              qvsi(i,j,k,jj) = 0.622_r_kind * es(i,j,k,jj) / (ges_prsl(i,j,k,jj)-es(i,j,k,jj)) ! ges_prsl in cbar
              ssqvi(i,j,k,jj) = qv(i,j,k,jj) - qvsi(i,j,k,jj) ! kg/kg
              if (ssqvi(i,j,k,jj) < zero) ssqvi(i,j,k,jj) = zero
              ges_swcp(i,j,jj) = ges_swcp(i,j,jj) + ssqvi(i,j,k,jj) * &
                                 tpwcon*r10*(ges_prsi(i,j,k,jj)-ges_prsi(i,j,k+1,jj)) ! kg/m^2
            endif
          end do
        end do
      end do
    end do

  else

    ! l_wcp_cwm = T and partition6: ql, qi, qr, qs, qg, and qh'
    ges_swcp = zero
      
    do jj=1,nfldsig
      do k=1,nsig
        do j=1,lon2
          do i=1,lat2
            if (ges_tsen(i,j,k,jj) < tupper .and. k <= nsig_top ) then
              ges_swcp(i,j,jj) = ges_swcp(i,j,jj) + &
                               (ges_qi(i,j,k,jj)+ges_qs(i,j,k,jj)+ges_qg(i,j,k,jj)+ges_qh(i,j,k,jj)) * &
                               tpwcon*r10*(ges_prsi(i,j,k,jj)-ges_prsi(i,j,k+1,jj)) ! kg/m^2
            endif
          enddo
        enddo
      enddo
    enddo

  endif ! l_wcp_cwm  

!=============================================================================================================


  read(lunin)data,luse,ioid

!        index information for data array (see reading routine)
  ier=1       ! index of obs error
  ilon=2      ! index of grid relative obs location (x)
  ilat=3      ! index of grid relative obs location (y)
  iswcp = 4   ! index of swcp observations
  id=5        ! index of station id
  itime=6     ! index of observation time in data array
  ikxx=7      ! index of ob type
  iswcpmax=8  ! index of swcp max error
  iqc=9       ! index of quality mark
  ier2=10     ! index of original-original obs error ratio
  iuse=11     ! index of use parameter
  ilone=12    ! index of longitude (degrees)
  ilate=13    ! index of latitude (degrees)
  istnelv=14  ! index of station elevation (m)
  iobsprs=15  ! index of observation pressure (hPa)
  iobshgt=16  ! index of observation height (m)

  do i=1,nobs
     muse(i)=nint(data(11,i)) <= jiter .and. nint(data(iqc,i)) < 8
  end do

  dup=one
  do k=1,nobs
     do l=k+1,nobs
        if(data(ilat,k) == data(ilat,l) .and.  &
           data(ilon,k) == data(ilon,l) .and. &
           data(ier,k) < r1000 .and. data(ier,l) < r1000 .and. &
           muse(k) .and. muse(l)) then
           tfact=min(one,abs(data(itime,k)-data(itime,l))/dfact1)
           dup(k)=dup(k)+one-tfact*tfact*(one-dfact)
           dup(l)=dup(l)+one-tfact*tfact*(one-dfact)
        end if
     end do
  end do

! If requested, save select data for output to diagnostic file
  if(conv_diagsave)then
     nchar=1
     ioff0=20
     nreal=ioff0
     if (lobsdiagsave) nreal=nreal+4*miter+1
     if (save_jacobian) then
        nnz   = nsig                   ! number of non-zero elements in dH(x)/dx profile  
        nind   = 1
        call new(dhx_dx, nnz, nind)
        nreal = nreal + size(dhx_dx)
     endif
     allocate(cdiagbuf(nobs),rdiagbuf(nreal,nobs))
     ii=0
     if(netcdf_diag) call init_netcdf_diag_
  end if


! Prepare total precipitable water data
  call dtime_setup()
  do i=1,nobs
     dtime=data(itime,i)
     call dtime_check(dtime, in_curbin, in_anybin)
     if(.not.in_anybin) cycle

     if(in_curbin) then
        dlat=data(ilat,i)
        dlon=data(ilon,i)
 

        dswcp=data(iswcp,i)
        ikx = nint(data(ikxx,i))
        error=data(ier2,i)

        ratio_errors=error/data(ier,i)
        error=one/error
     endif ! (in_curbin)

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

        if(.not.associated(my_diag)) call die(myname, &
                'obsdiagLList_nextNode(), create =',.not.lobsdiag_allocated)
     endif

     if(.not.in_curbin) cycle
 
!=============================================================================================================
! Interpolate ges_* to obs location

     ! Interpolate model swcp to obs location
     call tintrp2a11(ges_swcp,swcpges,dlat,dlon,dtime, &
        hrdifsig,mype,nfldsig)

     ! Interpolate pressure at interface values to obs location
     call tintrp2a1(ges_prsi,piges,dlat,dlon,dtime, &
         hrdifsig,nsig+1,mype,nfldsig)

     if (.not.l_wcp_cwm) then
       call tintrp2a1(ges_prsl,plges,dlat,dlon,dtime, &
           hrdifsig,nsig,mype,nfldsig)
       call tintrp2a1(ges_tsen,tges,dlat,dlon,dtime, &
           hrdifsig,nsig,mype,nfldsig)
       call tintrp2a1(ges_q,qges,dlat,dlon,dtime, &
           hrdifsig,nsig,mype,nfldsig)
     else
       call tintrp2a1(ges_tsen,tges,dlat,dlon,dtime, &
           hrdifsig,nsig,mype,nfldsig)
       call tintrp2a1(ges_qi,qiges,dlat,dlon,dtime, &
           hrdifsig,nsig,mype,nfldsig)
       call tintrp2a1(ges_qs,qsges,dlat,dlon,dtime, &
           hrdifsig,nsig,mype,nfldsig)
       call tintrp2a1(ges_qg,qgges,dlat,dlon,dtime, &
           hrdifsig,nsig,mype,nfldsig)
       call tintrp2a1(ges_qh,qhges,dlat,dlon,dtime, &
           hrdifsig,nsig,mype,nfldsig)
     endif

     if (save_jacobian) then
        qi_ind = getindex(svars3d, 'qi')
        if (qi_ind < 0) then
           print *, 'Error: no variable q in state vector. Exiting.'
           call stop2(1300)
        endif

        dhx_dx%st_ind(1)  = 1    + sum(levels(1:qi_ind-1))
        dhx_dx%end_ind(1) = nsig + sum(levels(1:qi_ind-1))

        do k = 1, nsig
           dhx_dx%val(k) = tpwcon*r10*(piges(k)-piges(k+1))
        enddo
     endif

!=============================================================================================================
         
     ! Compute innovation
     ddiff = dswcp - swcpges

     !if (l_limit_swcp_innov) then
     !   ! Limit size of swcp innovation to a percent of the background value
     !   ddiff = sign(min(abs(ddiff),max_innov_pct*swcpges),ddiff)
     !end if

!    Gross checks using innovation

     residual = abs(ddiff)
     if (residual>grsmlt*data(iswcpmax,i)) then
        error = zero
        ratio_errors=zero
        if (luse(i)) awork(7) = awork(7)+one
     end if
     obserror = one/max(ratio_errors*error,tiny_r_kind)
     obserrlm = max(cermin(ikx),min(cermax(ikx),obserror))
     ratio    = residual/obserrlm
     if (ratio> cgross(ikx) .or. ratio_errors < tiny_r_kind) then
        if (luse(i)) awork(6) = awork(6)+one
        error = zero
        ratio_errors=zero
     else
        ratio_errors=ratio_errors/sqrt(dup(i))
     end if
     if (ratio_errors*error <= tiny_r_kind) muse(i)=.false.
     if (nobskeep>0.and.luse_obsdiag) call obsdiagNode_get(my_diag, jiter=nobskeep, muse=muse(i))

     val      = error*ddiff

     if(luse(i))then
!    Compute penalty terms (linear & nonlinear qc).
        val2     = val*val
        exp_arg  = -half*val2
        rat_err2 = ratio_errors**2
        if (cvar_pg(ikx) > tiny_r_kind .and. error > tiny_r_kind) then
           arg  = exp(exp_arg)
           wnotgross= one-cvar_pg(ikx)
           cg_swcp=cvar_b(ikx)
           wgross = cg_term*cvar_pg(ikx)/(cg_swcp*wnotgross)
           term = log((arg+wgross)/(one+wgross))
           wgt  = one-wgross/(arg+wgross)
           rwgt = wgt/wgtlim
        else
           term = exp_arg
           wgt  = wgtlim
           rwgt = wgt/wgtlim
        endif
        valqc = -two*rat_err2*term

! Accumulate statistics as a function of observation type.
        ress  = ddiff*scale
        ressw2= ress*ress
        val2  = val*val
        rat_err2 = ratio_errors**2
!       Accumulate statistics for obs belonging to this task
        if (muse(i) ) then
           if(rwgt < one) awork(21) = awork(21)+one
           awork(5) = awork(5)+val2*rat_err2
           awork(4) = awork(4)+one
           awork(22)=awork(22)+valqc
           nn=1
        else
           nn=2
           if(ratio_errors*error >=tiny_r_kind)nn=3
        end if
        bwork(1,ikx,1,nn)  = bwork(1,ikx,1,nn)+one             ! count
        bwork(1,ikx,2,nn)  = bwork(1,ikx,2,nn)+ress            ! (o-g)
        bwork(1,ikx,3,nn)  = bwork(1,ikx,3,nn)+ressw2          ! (o-g)**2
        bwork(1,ikx,4,nn)  = bwork(1,ikx,4,nn)+val2*rat_err2   ! penalty
        bwork(1,ikx,5,nn)  = bwork(1,ikx,5,nn)+valqc           ! nonlin qc penalty
        
     end if

     if (luse_obsdiag) then
        call obsdiagNode_set(my_diag, wgtjo=(error*ratio_errors)**2, &
           jiter=jiter, muse=muse(i), nldepart=ddiff)
     endif

!    If obs is "acceptable", load array with obs info for use
!    in inner loop minimization (int* and stp* routines)
     if ( .not. last .and. muse(i)) then

        allocate(my_head)
        call swcpNode_appendto(my_head,swcphead(ibin))

        my_head%idv = is
        my_head%iob = ioid(i)
        my_head%elat= data(ilate,i)
        my_head%elon= data(ilone,i)

        allocate(my_head%ij(4, nsig), &
                 my_head%jac_t(nsig  ), &
                 my_head%jac_p(nsig+1), &
                 my_head%jac_q(nsig  ), &
                 my_head%jac_qi(nsig  ), &
                 my_head%jac_qs(nsig  ), &
                 my_head%jac_qg(nsig  ), &
                 my_head%jac_qh(nsig  ), stat=istat)
        if (istat/=0) write(6,*)'MAKECOBS:  allocate error for swcphead, istat=',istat


!       Set (i,j) indices of guess gridpoint that bound obs location
        call get_ij(mm1,dlat,dlon,swcp_ij,my_head%wij)

        my_head%res    = ddiff
        my_head%err2   = error**2
        my_head%raterr2= ratio_errors**2  
        my_head%time   = dtime
        my_head%b      = cvar_b(ikx)
        my_head%pg     = cvar_pg(ikx)
        my_head%luse   = luse(i)

        my_head%jac_t(:)=zero
        my_head%jac_p(:)=zero
        my_head%jac_q(:)=zero
        my_head%jac_qi(:)=zero
        my_head%jac_qs(:)=zero
        my_head%jac_qg(:)=zero
        my_head%jac_qh(:)=zero

!=============================================================================================================
! Calculate Jacobians for swcp

        eslges=zero; esiges=zero; esges=zero;
        desldt=zero; desidt=zero; desdt=zero; dwdt=zero
        dssqdq=zero; dssqdt=zero; dssqdp=zero

        do k=1,nsig

          my_head%ij(1,k)=swcp_ij(1)+(k-1)*latlon11
          my_head%ij(2,k)=swcp_ij(2)+(k-1)*latlon11
          my_head%ij(3,k)=swcp_ij(3)+(k-1)*latlon11
          my_head%ij(4,k)=swcp_ij(4)+(k-1)*latlon11

          if (.not.l_wcp_cwm) then
            
            qvges(k) = qges(k)/(one-qges(k)) ! kg/kg
            trges(k) = ttp/tges(k)
            wges(k) = 0.5_r_kind*(one+tanh((tges(k)-tcenter)/((tupper-tlower)/4._r_kind))) ! hyperbolic tangent
 
            if ( tges(k) < tupper .and. k <= nsig_top ) then
              !psat is in Pa; psatk is in cbar 
              eslges(k) = psatk*(trges(k)**xa)*exp(xb*(one-trges(k))) ! cbar
              esiges(k) = psatk*(trges(k)**xai)*exp(xbi*(one-trges(k))) !cbar
              esges(k) = wges(k) * eslges(k) + (one-wges(k)) * esiges(k) ! cbar           
              qvsges(k) = 0.622_r_kind*esges(k)/(plges(k)-esges(k)) ! kg/kg
              ssqges(k) = qvges(k)-qvsges(k)
              if ( ssqges(k) < zero ) ssqges(k)=zero
                !jacobian 
                desldt(k) = eslges(k)*(-xa/tges(k)) + eslges(k)*xb*ttp/(tges(k)**2)
                desidt(k) = esiges(k)*(-xai/tges(k)) + esiges(k)*xbi*ttp/(tges(k)**2)
                ! hyperbolic tangent
                dwdt(k) = 0.5_r_kind*(one/cosh((tges(k)-tcenter)/((tupper-tlower)/4._r_kind))**2)*(4._r_kind/(tupper-tlower)) 
                desdt(k) = dwdt(k)*eslges(k) + wges(k)*desldt(k) &
                         + (-dwdt(K))*esiges(k) + (one-wges(k))*desidt(k)
 
                dssqdt(k) = -0.622_r_kind* ( desdt(k)/(plges(k)-esges(k)) &
                          + esges(k)*desdt(k)/((plges(k)-esges(k))**2) )
                dssqdq(k) = one/(one-qges(k)) + qges(k)/((one-qges(k))**2)
                dssqdp(k) = 0.622_r_kind*esges(k)/(plges(k)-esges(k))**2

                my_head%jac_t(k)=dssqdt(k)*(tpwcon*r10*(piges(k)-piges(k+1))) 
                my_head%jac_p(k)=dssqdp(k)*(tpwcon*r10*(piges(k)-piges(k+1))) 
                my_head%jac_q(k)=dssqdq(k)*(tpwcon*r10*(piges(k)-piges(k+1))) 
            endif
    
          else

            if ( tges(k) < tupper .and. k <= nsig_top ) then
              my_head%jac_qi(k)=tpwcon*r10*(piges(k)-piges(k+1)) 
              my_head%jac_qs(k)=tpwcon*r10*(piges(k)-piges(k+1)) 
              my_head%jac_qg(k)=tpwcon*r10*(piges(k)-piges(k+1)) 
              my_head%jac_qh(k)=tpwcon*r10*(piges(k)-piges(k+1)) 
            endif
 
          endif
        end do

        my_head%jac_p(nsig+1) = zero
!=============================================================================================================

        if (luse_obsdiag) then
           call obsdiagNode_assert(my_diag, my_head%idv, my_head%iob, 1, myname, 'my_diag:my_head')
           my_head%diags => my_diag
        endif

        my_head => null()
     endif


!    Save select output for diagnostic file
     if(conv_diagsave .and. luse(i))then
        ii=ii+1
        rstation_id     = data(id,i)
        err_input = data(ier2,i)
        err_adjst = data(ier,i)
        if (ratio_errors*error>tiny_r_kind) then
           err_final = one/(ratio_errors*error)
        else
           err_final = huge_single
        endif

        errinv_input = huge_single
        errinv_adjst = huge_single
        errinv_final = huge_single
        if (err_input>tiny_r_kind) errinv_input=one/err_input
        if (err_adjst>tiny_r_kind) errinv_adjst=one/err_adjst
        if (err_final>tiny_r_kind) errinv_final=one/err_final

        ioff=ioff0

        if(binary_diag) call contents_binary_diag_(my_diag)
        if(netcdf_diag) call contents_netcdf_diag_(my_diag)
     end if


  end do

! Release memory of local guess arrays
  call final_vars_

! Write information to diagnostic file
  if(conv_diagsave)then
     if(netcdf_diag) call nc_diag_write
     if(binary_diag.and. ii>0)then
        write(7)'swc',nchar,nreal,ii,mype,ioff0
        write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
        deallocate(cdiagbuf,rdiagbuf)
     end if
  end if

! End of routine

  return
  contains

  subroutine check_vars_ (proceed)
  use obsmod, only: l_wcp_cwm
  logical,intent(inout) :: proceed
  integer(i_kind) ivar, istatus
! Check to see if required guess fields are available
  if (.not.l_wcp_cwm) then

    call gsi_metguess_get ('var::q', ivar, istatus )
    proceed=ivar>0

  else

    call gsi_metguess_get ('var::qi' , ivar, istatus )
    proceed=ivar>0
    call gsi_metguess_get ('var::qs', ivar, istatus )
    proceed=proceed.and.ivar>0
    call gsi_metguess_get ('var::qg', ivar, istatus )
    proceed=proceed.and.ivar>0
    call gsi_metguess_get ('var::qh', ivar, istatus )
    proceed=proceed.and.ivar>0

  endif ! l_wcp_cwm
  end subroutine check_vars_ 

  subroutine init_vars_
  use obsmod, only: l_wcp_cwm
  real(r_kind),dimension(:,:,:),pointer:: rank3=>NULL()
  character(len=5) :: varname
  integer(i_kind) ifld, istatus

! If require guess vars available, extract from bundle ...
  if(size(gsi_metguess_bundle)==nfldsig) then

    if (.not.l_wcp_cwm) then

      ! get q ...
      varname='q'
      call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
      if (istatus==0) then
        if(allocated(ges_q))then
          write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
          call stop2(999)
        endif
        allocate(ges_q(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
          ges_q(:,:,:,1)=rank3
          do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_q(:,:,:,ifld)=rank3
          enddo
      else
        write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
        call stop2(999)
      endif

    else

      ! get qi ...
      varname='qi'
      call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
      if (istatus==0) then
        if(allocated(ges_qi))then
          write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
          call stop2(999)
        endif
        allocate(ges_qi(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
        ges_qi(:,:,:,1)=rank3
        do ifld=2,nfldsig
          call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
          ges_qi(:,:,:,ifld)=rank3
        enddo
      else
        write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
        call stop2(999)
      endif
      ! get qs ...
      varname='qs'
      call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
      if (istatus==0) then
        if(allocated(ges_qs))then
          write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
          call stop2(999)
        endif
        allocate(ges_qs(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
        ges_qs(:,:,:,1)=rank3
        do ifld=2,nfldsig
          call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
          ges_qs(:,:,:,ifld)=rank3
        enddo
      else
        write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
        call stop2(999)
      endif
      ! get qg ...
      varname='qg'
      call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
      if (istatus==0) then
        if(allocated(ges_qg))then
          write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
          call stop2(999)
        endif
        allocate(ges_qg(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
        ges_qg(:,:,:,1)=rank3
        do ifld=2,nfldsig
          call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
          ges_qg(:,:,:,ifld)=rank3
        enddo
      else
        write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
        call stop2(999)
      endif
      ! get qh ...
      varname='qh'
      call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
      if (istatus==0) then
        if(allocated(ges_qh))then
          write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
          call stop2(999)
        endif
        allocate(ges_qh(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
        ges_qh(:,:,:,1)=rank3
        do ifld=2,nfldsig
          call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
          ges_qh(:,:,:,ifld)=rank3
        enddo
      else
        write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
        call stop2(999)
      endif

    endif ! l_wcp_cwm

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

! open netcdf diag file
     write(string,900) jiter
900  format('conv_swcp_',i2.2,'.nc4')
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
     rdiagbuf(6,ii)  = data(iobsprs,i)    ! observation pressure (hPa)
     rdiagbuf(7,ii)  = data(iobshgt,i)    ! observation height (meters)
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
     rdiagbuf(14,ii) = errinv_input       ! prepbufr inverse obs error
     rdiagbuf(15,ii) = errinv_adjst       ! read_prepbufr inverse obs error
     rdiagbuf(16,ii) = errinv_final       ! final inverse observation error

     rdiagbuf(17,ii) = dswcp              ! solid-water content path obs (kg/m**2)
     rdiagbuf(18,ii) = ddiff              ! obs-ges used in analysis (kg/m**2)
     rdiagbuf(19,ii) = dswcp-swcpges          ! obs-ges w/o bias correction (kg/m**2) (future slot)
     rdiagbuf(20,ii) = 1.e10_r_single     ! spread (filled in by EnKF)

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

     if (save_jacobian) then
        call writearray(dhx_dx, rdiagbuf(ioff+1:nreal,ii))
        ioff = ioff + size(dhx_dx)
     endif

  end subroutine contents_binary_diag_

  subroutine contents_netcdf_diag_(odiag)
  type(obs_diag),pointer,intent(in):: odiag
! Observation class
  character(7),parameter     :: obsclass = '   swcp'
  real(r_kind),parameter::     missing = -9.99e9_r_kind

  real(r_kind),dimension(miter) :: obsdiag_iuse

    call nc_diag_metadata("Station_ID",              station_id             )
    call nc_diag_metadata("Observation_Class",       obsclass               )
    call nc_diag_metadata("Observation_Type",        ictype(ikx)            )
    call nc_diag_metadata("Observation_Subtype",     icsubtype(ikx)         )
    call nc_diag_metadata_to_single("Latitude",data(ilate,i)      )
    call nc_diag_metadata_to_single("Longitude",data(ilone,i)      )
    call nc_diag_metadata_to_single("Station_Elevation",data(istnelv,i)  )
    call nc_diag_metadata_to_single("Pressure",data(iobsprs,i)  )
    call nc_diag_metadata_to_single("Height",data(iobshgt,i)  )
    call nc_diag_metadata_to_single("Time",dtime,time_offset,'-')
    call nc_diag_metadata_to_single("Prep_QC_Mark",data(iqc,i)        )
    call nc_diag_metadata("Setup_QC_Mark",rmiss_single       )
    call nc_diag_metadata_to_single("Prep_Use_Flag",data(iuse,i)       )
    if(muse(i)) then
       call nc_diag_metadata("Analysis_Use_Flag",    sngl(one)              )
    else
       call nc_diag_metadata("Analysis_Use_Flag",    sngl(-one)             )
    endif

    call nc_diag_metadata_to_single("Nonlinear_QC_Rel_Wgt",rwgt               )
    call nc_diag_metadata_to_single("Errinv_Input",errinv_input       )
    call nc_diag_metadata_to_single("Errinv_Adjust",errinv_adjst       )
    call nc_diag_metadata_to_single("Errinv_Final",errinv_final       )
    call nc_diag_metadata_to_single("Observation",dswcp              )
    call nc_diag_metadata_to_single("Obs_Minus_Forecast_adjusted",ddiff        )
    call nc_diag_metadata_to_single("Obs_Minus_Forecast_unadjusted", dswcp,swcpges,'-')

    if (lobsdiagsave) then
       do jj=1,miter
          if (odiag%muse(jj)) then
             obsdiag_iuse(jj) = one
          else
             obsdiag_iuse(jj) = -one
          endif
       enddo

       call nc_diag_data2d("ObsDiagSave_iuse",     obsdiag_iuse                             )
       call nc_diag_data2d("ObsDiagSave_nldepart", odiag%nldepart )
       call nc_diag_data2d("ObsDiagSave_tldepart", odiag%tldepart )
       call nc_diag_data2d("ObsDiagSave_obssen",   odiag%obssen   )              
    endif

    if (save_jacobian) then
       call nc_diag_data2d("Observation_Operator_Jacobian_stind", dhx_dx%st_ind)
       call nc_diag_data2d("Observation_Operator_Jacobian_endind", dhx_dx%end_ind)
       call nc_diag_data2d("Observation_Operator_Jacobian_val", real(dhx_dx%val,r_single))
    endif

  end subroutine contents_netcdf_diag_

  subroutine final_vars_
    if(allocated(ges_q )) deallocate(ges_q )
    if(allocated(ges_qi)) deallocate(ges_qi)
    if(allocated(ges_qs)) deallocate(ges_qs)
    if(allocated(ges_qg)) deallocate(ges_qg)
    if(allocated(ges_qh)) deallocate(ges_qh)
  end subroutine final_vars_

end subroutine setupswcp
end module swcp_setup
