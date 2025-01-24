subroutine anbkerror(grady)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    anbkerror apply anisotropic background error covariance  
!   prgmmr: parrish          org: np22                date: 2005-02-03
!
! abstract: apply regional anisotropic background error.
!
! program history log:
!   2005-02-08  parrish
!   2005-04-29  parrish - replace coarse2fine with fgrid2agrid;
!                         remove ansmoothrf_reg_d
!   2006-11-30  todling - add fpsproj as arg to (t)balance routine(s)
!   2008-10-10  derber - add strong constraint to background error
!   2008-12-29  todling - update interface to strong_bk/bk_ad
!   2009-04-13  derber - move strong_bk into balance
!   2009-07-01  sato - update for global mode
!   2010-05-05  todling - update to use gsi_bundle
!   2010-06-22  todling - update to better handle bundle pointers
!   2010-06-29  lueken - replaced tv with t in call to gsi_bundlegetpointer
!   2010-08-19  lueken - add only to module use
!   2012-10-09  Gu - add fut2ps as arg to (t)balance routine(s)
!   2013-05-23  zhu    - add ntclen for aircraft temperature bias correction
!   2014-02-07  pondeca - update to handle motley variables. rename p_st to p_sf
!   2014-02-14  pondeca - update to handle optional separation of sf and vp control variables
!                         into land-only and water-only parts
!   2015-05-02  parrish - add subroutine ansmoothrf_reg_sub2slab_option, and
!                         parameter rtma_bkerr_sub2slab.
!                         rtma_bkerr_sub2slab = F, then use ansmoothrf_reg_subdomain_option
!                         rtma_bkerr_sub2slab = T, then use ansmoothrf_reg_sub2slab_option
!                         This allows dual resolution for the anisotropic recursive filter, which
!                         currently can only be used with full horizontal domain (slab) storage.
!   2015-07-02  pondeca - update slab mode option to work with any number of control variables
!
!   input argument list:
!     grady    - input field  
!
!   output
!     grady    - background structure * grady 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2
  use jfunc, only: nsclen,npclen,ntclen
  use balmod, only: balance,tbalance,ke_vp,bvk
  use berror, only: varprd,fpsproj,fut2ps
  use constants, only: zero
  use control_vectors, only: control_vector,assignment(=)
  use control_vectors, only: mvars,nrf,nrf_var,nrf_3d
  use gsi_4dvar, only: nsubwin
  use timermod, only: timer_ini,timer_fnl
  use gsi_bundlemod, only: gsi_bundlegetpointer,gsi_bundlemerge,gsi_bundle,gsi_bundledup,gsi_bundledestroy
  implicit none

! Declare passed variables
  type(control_vector),intent(inout) :: grady

! Declare local variables
  integer(i_kind) i,j,k,ii,istatus
  real(r_kind),dimension(:,:,:),pointer::p_t  =>NULL()
  real(r_kind),dimension(:,:,:),pointer::p_sf =>NULL()
  real(r_kind),dimension(:,:,:),pointer::p_vp =>NULL()
  real(r_kind),dimension(:,:  ),pointer::p_ps =>NULL()
  real(r_kind),dimension(:,:,:),pointer::p_sfwter =>NULL()
  real(r_kind),dimension(:,:,:),pointer::p_vpwter =>NULL()
  real(r_kind),pointer::rank2a(:,:)  =>NULL()
  real(r_kind),pointer::rank2b(:,:)  =>NULL()
  real(r_kind),pointer::rank3a(:,:,:)=>NULL()
  real(r_kind),pointer::rank3b(:,:,:)=>NULL()
  logical do_balance, do_balancewter
  integer(i_kind), parameter :: myvars = 6
  integer(i_kind) :: ipnts(myvars)
  type(gsi_bundle) :: mbundle
  character(len=6), parameter :: myvnames(myvars) = (/  &
       'sf    ', 'vp    ', 'ps    ', 't     ', 'sfwter', 'vpwter'/)

! Initialize timer
  call timer_ini('anbkerror')

! Since each internal vector [step(jj)] of grad has the same structure, pointers
! are the same independent of the subwindow jj
call gsi_bundlegetpointer (grady%step(1),myvnames,ipnts,istatus)

! Define what to do depending on what's in CV and SV
do_balance=ipnts(1)>0.and.ipnts(2)>0.and.ipnts(3)>0.and.ipnts(4)>0
do_balancewter=ipnts(5)>0.and.ipnts(6)>0

! Loop on control steps
  do ii=1,nsubwin

!    Create temporary bundle which merges grady%step(ii) with grady%motley(ii)
     if(mvars>0) then
        call gsi_bundlemerge(mbundle,grady%step(ii),grady%motley(ii),' add motley to step',istatus)
     else
        call gsi_bundledup(grady%step(ii),mbundle,' copy of step ',istatus)
     end if

!    Transpose of balance equation
     if(do_balancewter) then 
        call gsi_bundlegetpointer (mbundle,'sfwter',p_sfwter,  istatus)
        call gsi_bundlegetpointer (mbundle,'vpwter',p_vpwter,  istatus)

       !Adjoint of contribution to velocity potential from streamfunction.
        do k=1,ke_vp
           do j=1,lon2
              do i=1,lat2
                 p_sfwter(i,j,k)=p_sfwter(i,j,k)+bvk(i,j,k)*p_vpwter(i,j,k)
              end do
           end do
        end do
     endif

     if(do_balance) then 
        call gsi_bundlegetpointer (mbundle,'sf',p_sf,  istatus)
        call gsi_bundlegetpointer (mbundle,'vp',p_vp,  istatus)
        call gsi_bundlegetpointer (mbundle,'ps',p_ps,  istatus)
        call gsi_bundlegetpointer (mbundle,'t ',p_t,   istatus)
        call gsi_bundlegetpointer (mbundle,'sfwter',p_sfwter,  istatus)
        call gsi_bundlegetpointer (mbundle,'vpwter',p_vpwter,  istatus)
        call tbalance(p_t,p_ps,p_sf,p_vp,fpsproj,fut2ps)
     endif

!    Apply variances, as well as vertical & horizontal parts of background error
     call anbkgcov(mbundle)

!    Balance equation
     if(do_balance) call balance(p_t,p_ps,p_sf,p_vp,fpsproj,fut2ps)

     if(do_balancewter) then 
       do k=1,ke_vp
          do j=1,lon2
             do i=1,lat2
                p_vpwter(i,j,k)=p_vpwter(i,j,k)+bvk(i,j,k)*p_sfwter(i,j,k)
             end do
          end do
       end do
     endif

!    Transfer step part of mbundle back to grady%step(ii)
     do i=1,nrf
        if(nrf_3d(i)) then
           call gsi_bundlegetpointer(mbundle,trim(nrf_var(i)),rank3a,istatus)
           call gsi_bundlegetpointer(grady%step(ii),trim(nrf_var(i)),rank3b,istatus)
           rank3b=rank3a
        else
           call gsi_bundlegetpointer(mbundle,trim(nrf_var(i)),rank2a,istatus)
           call gsi_bundlegetpointer(grady%step(ii),trim(nrf_var(i)),rank2b,istatus)
           rank2b=rank2a
        end if
     end do

!    Transfer motley part of mbundle back to grady%motley(ii)
     do i=1,mvars
        call gsi_bundlegetpointer(mbundle,trim(nrf_var(nrf+i)),rank2a,istatus)
        call gsi_bundlegetpointer(grady%motley(ii),trim(nrf_var(nrf+i)),rank2b,istatus)
        rank2b=rank2a
     end do
  end do

! clean work space
  call gsi_bundledestroy(mbundle,istatus)
  if(istatus/=0) then
     write(6,*) ' in anbkerror: trouble destroying work mbundle'
     call stop2(999)
  endif

! Take care of background error for bias correction terms
  do i=1,nsclen
     grady%predr(i)=grady%predr(i)*varprd(i)
  end do
  do i=1,npclen
     grady%predp(i)=grady%predp(i)*varprd(nsclen+i)
  end do
  if(ntclen>0)then
     do i=1,ntclen
        grady%predt(i)=grady%predt(i)*varprd(nsclen+npclen+i)
     end do
  end if

! Finalize timer
  call timer_fnl('anbkerror')

end subroutine anbkerror


subroutine anbkgcov(bundle)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    anbkgcov    apply anisotropic background error covar
!   prgmmr: parrish        org: np22                date: 2005-02-14
!
! abstract: apply regional anisotropic background error covariance
!
! program history log:
!   2005-02-14  parrish
!   2009-07-01  sato - update for global mode
!   2010-05-20  todling - update fit interface to sug2grid/grid2sub (bundle)
!   2010-06-22  todling - update interface (remove cwmr since it's in bunlde)
!   2010-06-29  lueken - added if(ipnts(2)>0) to second call of anbkgvar
!   2011-02-22  zhu - replace the argument list of ansmoothrf_reg_subdomain_option by a bundle
!   2012-06-25  parrish - replace sub2grid and grid2sub calls with general_sub2grid, general_grid2sub.
!                 NOTE:  This will not work with sst and the motley variables stl,sti.  However
!                        this is not currently used in this version of RTMA.
!   2014-03-19  pondeca - add wspd10m
!   2014-04-10  pondeca - add td2m,mxtm,mitm,pmsl
!   2014-05-07  pondeca - add howv
!   2016-05-03  pondeca - add uwnd10m, vwnd10m
!
!   input argument list:
!     t        - t on subdomain
!     p        - p surface pressure on subdomain
!     q        - q on subdomain
!     oz       - ozone on subdomain
!     skint    - skin temperature on subdomain
!     sst      - sea surface temperature on subdomain
!     stl      - land surface temperature on subdomain
!     sti      - ice surface temperature on subdomain
!     gust     - 10-m gust on subdomain
!     vis      - surface visibility on subdomain
!     wspd10m  - 10m-wind speed on subdomain
!     td2m     - td on subdomain
!     mxtm     - daily maximum temperature
!     mitm     - daily  minimum temperature
!     pmsl     - pressure at mean sea level
!     howv     - significant wave height
!     tcamt    - total cloud amount
!     lcbas    - lowest cloud base height
!     cldch    - cloud ceiling height
!     uwnd10m  - 10m-uwind on subdomain
!     vwnd10m  - 10m-vwind on subdomain
!     pswter   - water surface pressure on subdomain
!     twter    - water 2m-temperature on subdomain
!     qwter    - water 2m-specific humidity on subdomain
!     gustwter - water 10m-gust on subdomain
!     wspd10mwter  - water 10m-wind speed on subdomain
!     td2mwter - water td on subdomain
!     mxtmwter - water daily maximum temperature
!     mitmwter - water daily minimum temperature
!     uwnd10mwter  - water 10m-uwind on subdomain
!     vwnd10mwter  - water 10m-vwind on subdomain
!
!   output argument list:
!                 all after smoothing, combining scales
!     t        - t on subdomain
!     p        - p surface pressure on subdomain
!     q        - q on subdomain
!     oz       - ozone on subdomain
!     skint    - skin temperature on subdomain
!     sst      - sea surface temperature on subdomain
!     stl      - land surface temperature on subdomain
!     sti      - ice surface temperature on subdomain
!     gust     - 10-m gust on subdomain
!     vis      - surface visibility on subdomain
!     wspd10m  - 10m-wind speed on subdomain
!     td2m     - td on subdomain
!     mxtm     - daily maximum temperature
!     mitm     - daily minimum temperature
!     pmsl     - pressure at mean sea level
!     howv     - significant wave height
!     tcamt    - total cloud amount
!     lcbas    - lowest cloud base height
!     cldch    - cloud ceiling height
!     uwnd10m  - 10m-uwind on subdomain
!     vwnd10m  - 10m-vwind on subdomain
!     pswter   - water surface pressure on subdomain
!     twter    - water 2m-temperature on subdomain
!     qwter    - water 2m-specific humidity on subdomain
!     gustwter - water 10m-gust on subdomain
!     wspd10mwter  - water 10m-wind speed on subdomain
!     td2mwter - water td on subdomain
!     mxtmwter - water daily maximum temperature
!     mitmwter - water daily minimum temperature
!     uwnd10mwter  - water 10m-uwind on subdomain
!     vwnd10mwter  - water 10m-vwind on subdomain
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2,twodvar_regional
  use anberror, only: rtma_subdomain_option,nsmooth, nsmooth_shapiro,rtma_bkerr_sub2slab
  use constants, only: zero
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use general_sub2grid_mod, only: general_sub2grid,general_grid2sub
  implicit none

! Passed Variables
  type(gsi_bundle),                 intent(inout) :: bundle

! Local Variables

  integer(i_kind) n,istatus
  integer(i_kind) i_sst,i_stl,i_sti,i_ps,i_t,i_q,i_gust,i_wspd10m, &
                  i_td2m,i_mxtm,i_mitm,i_uwnd10m,i_vwnd10m, & 
                  i_pswter,i_twter,i_qwter,i_gustwter,i_wspd10mwter, &
                  i_td2mwter,i_mxtmwter,i_mitmwter,i_uwnd10mwter,i_vwnd10mwter                 

  real(r_kind),dimension(lat2,lon2) :: skint,sst,stl,sti
  real(r_kind),dimension(lat2,lon2) :: field,fld,fldwter

  real(r_kind),dimension(:,:),   pointer :: ptrsst=>NULL()
  real(r_kind),dimension(:,:),   pointer :: ptrstl=>NULL()
  real(r_kind),dimension(:,:),   pointer :: ptrsti=>NULL()
  real(r_kind),dimension(:,:),   pointer :: ptrps=>NULL()
  real(r_kind),dimension(:,:,:), pointer :: ptrt=>NULL()
  real(r_kind),dimension(:,:,:), pointer :: ptrq=>NULL()
  real(r_kind),dimension(:,:),   pointer :: ptrgust=>NULL()
  real(r_kind),dimension(:,:),   pointer :: ptrwspd10m=>NULL()
  real(r_kind),dimension(:,:),   pointer :: ptrtd2m=>NULL()
  real(r_kind),dimension(:,:),   pointer :: ptrmxtm=>NULL()
  real(r_kind),dimension(:,:),   pointer :: ptrmitm=>NULL()
  real(r_kind),dimension(:,:),   pointer :: ptruwnd10m=>NULL()
  real(r_kind),dimension(:,:),   pointer :: ptrvwnd10m=>NULL()
  real(r_kind),dimension(:,:,:), pointer :: ptr3d=>NULL()
  real(r_kind),dimension(:,:),   pointer :: ptrpswter=>NULL()
  real(r_kind),dimension(:,:),   pointer :: ptrtwter=>NULL()
  real(r_kind),dimension(:,:),   pointer :: ptrqwter=>NULL()
  real(r_kind),dimension(:,:),   pointer :: ptrgustwter=>NULL()
  real(r_kind),dimension(:,:),   pointer :: ptrwspd10mwter=>NULL()
  real(r_kind),dimension(:,:),   pointer :: ptrtd2mwter=>NULL()
  real(r_kind),dimension(:,:),   pointer :: ptrmxtmwter=>NULL()
  real(r_kind),dimension(:,:),   pointer :: ptrmitmwter=>NULL()
  real(r_kind),dimension(:,:),   pointer :: ptruwnd10mwter=>NULL()
  real(r_kind),dimension(:,:),   pointer :: ptrvwnd10mwter=>NULL()


! Perform simple vertical smoothing while fields are in sudomain mode.
! The accompanying smoothing in the horizontal is performed inside the
! recursive filter. Motivation: Reduce possible high frequency noise in
! the analysis that would arise from the use of a "non-blending" RF algorithm.

  do n=1,bundle%n3d
     call gsi_bundlegetpointer ( bundle,bundle%r3(n)%shortname,ptr3d,istatus )
     call vert_smther(ptr3d,nsmooth,nsmooth_shapiro)
  end do

! Break up skin temp into components

! First get pointers
  !SST
  call gsi_bundlegetpointer (bundle, 'sst', i_sst,  istatus)
  call gsi_bundlegetpointer (bundle, 'stl', i_stl,  istatus)
  call gsi_bundlegetpointer (bundle, 'sti', i_sti,  istatus)
  if (i_sst>0) call gsi_bundlegetpointer (bundle, 'sst', ptrsst, istatus)
  if (i_stl>0) call gsi_bundlegetpointer (bundle, 'stl', ptrstl, istatus)
  if (i_sti>0) call gsi_bundlegetpointer (bundle, 'sti', ptrsti, istatus)

  if(twodvar_regional) then
     !SURFACE PRESSURE
     call gsi_bundlegetpointer (bundle, 'ps',     i_ps,      istatus)
     call gsi_bundlegetpointer (bundle, 'pswter', i_pswter,  istatus)
     if (i_ps>0)     call gsi_bundlegetpointer (bundle, 'ps',     ptrps,     istatus)
     if (i_pswter>0) call gsi_bundlegetpointer (bundle, 'pswter', ptrpswter, istatus)

     !TEMPERATURE
     call gsi_bundlegetpointer (bundle, 't',     i_t,      istatus)
     call gsi_bundlegetpointer (bundle, 'twter', i_twter,  istatus)
     if (i_t>0)     call gsi_bundlegetpointer (bundle, 't',     ptrt,     istatus)
     if (i_twter>0) call gsi_bundlegetpointer (bundle, 'twter', ptrtwter, istatus)

     !SPECIFIC HUMIDITY
     call gsi_bundlegetpointer (bundle, 'q',     i_q,      istatus)
     call gsi_bundlegetpointer (bundle, 'qwter', i_qwter,  istatus)
     if (i_q>0)     call gsi_bundlegetpointer (bundle, 'q',     ptrq,     istatus)
     if (i_qwter>0) call gsi_bundlegetpointer (bundle, 'qwter', ptrqwter, istatus)

     !WIND GUST
     call gsi_bundlegetpointer (bundle, 'gust',     i_gust,      istatus)
     call gsi_bundlegetpointer (bundle, 'gustwter', i_gustwter,  istatus)
     if (i_gust>0)     call gsi_bundlegetpointer (bundle, 'gust',     ptrgust,     istatus)
     if (i_gustwter>0) call gsi_bundlegetpointer (bundle, 'gustwter', ptrgustwter, istatus)

     !10-m WIND SPEED
     call gsi_bundlegetpointer (bundle, 'wspd10m',     i_wspd10m,      istatus)
     call gsi_bundlegetpointer (bundle, 'wspd10mwter', i_wspd10mwter,  istatus)
     if (i_wspd10m>0)     call gsi_bundlegetpointer (bundle, 'wspd10m',     ptrwspd10m,     istatus)
     if (i_wspd10mwter>0) call gsi_bundlegetpointer (bundle, 'wspd10mwter', ptrwspd10mwter, istatus)

     !2-m DEW POINT
     call gsi_bundlegetpointer (bundle, 'td2m',     i_td2m,      istatus)
     call gsi_bundlegetpointer (bundle, 'td2mwter', i_td2mwter,  istatus)
     if (i_td2m>0)     call gsi_bundlegetpointer (bundle, 'td2m',     ptrtd2m,     istatus)
     if (i_td2mwter>0) call gsi_bundlegetpointer (bundle, 'td2mwter', ptrtd2mwter, istatus)

     !MAXIMUM TEMPERATURE
     call gsi_bundlegetpointer (bundle, 'mxtm',     i_mxtm,      istatus)
     call gsi_bundlegetpointer (bundle, 'mxtmwter', i_mxtmwter,  istatus)
     if (i_mxtm>0)     call gsi_bundlegetpointer (bundle, 'mxtm',     ptrmxtm,     istatus)
     if (i_mxtmwter>0) call gsi_bundlegetpointer (bundle, 'mxtmwter', ptrmxtmwter, istatus)

     !MINIMUM TEMPERATURE
     call gsi_bundlegetpointer (bundle, 'mitm',     i_mitm,      istatus)
     call gsi_bundlegetpointer (bundle, 'mitmwter', i_mitmwter,  istatus)
     if (i_mitm>0)     call gsi_bundlegetpointer (bundle, 'mitm',     ptrmitm,     istatus)
     if (i_mitmwter>0) call gsi_bundlegetpointer (bundle, 'mitmwter', ptrmitmwter, istatus)

     !10-m UWIND
     call gsi_bundlegetpointer (bundle, 'uwnd10m',     i_uwnd10m,      istatus)
     call gsi_bundlegetpointer (bundle, 'uwnd10mwter', i_uwnd10mwter,  istatus)
     if (i_uwnd10m>0)     call gsi_bundlegetpointer (bundle, 'uwnd10m',     ptruwnd10m,     istatus)
     if (i_uwnd10mwter>0) call gsi_bundlegetpointer (bundle, 'uwnd10mwter', ptruwnd10mwter, istatus)

     !10-m VWIND
     call gsi_bundlegetpointer (bundle, 'vwnd10m',     i_vwnd10m,      istatus)
     call gsi_bundlegetpointer (bundle, 'vwnd10mwter', i_vwnd10mwter,  istatus)
     if (i_vwnd10m>0)     call gsi_bundlegetpointer (bundle, 'vwnd10m',     ptrvwnd10m,     istatus)
     if (i_vwnd10mwter>0) call gsi_bundlegetpointer (bundle, 'vwnd10mwter', ptrvwnd10mwter, istatus)
  endif

! Break up skin temp

  if(i_sst>0 .and. i_stl>0 .and. i_sti>0) then
    stl=zero
    sst=zero
    sti=zero
    skint=ptrsst

    call anbkgvar(skint,sst,stl,sti,0)

    ptrsst=sst
    ptrstl=stl
    ptrsti=sti
  endif

  if(twodvar_regional) then
    if(i_ps>0.and.i_pswter>0) then
      fld=zero
      fldwter=zero
      field=ptrps

      call anbkgvar_lw(field,fld,fldwter,0)
  
      ptrps=fld
      ptrpswter=fldwter
    endif

    if(i_t>0.and.i_twter>0) then
      fld=zero
      fldwter=zero
      field(:,:)=ptrt(:,:,1)

      call anbkgvar_lw(field,fld,fldwter,0)

      ptrt(:,:,1)=fld(:,:)
      ptrtwter=fldwter
    endif

    if(i_q>0.and.i_qwter>0) then
      fld=zero
      fldwter=zero
      field(:,:)=ptrq(:,:,1)

      call anbkgvar_lw(field,fld,fldwter,0)

      ptrq(:,:,1)=fld(:,:)
      ptrqwter=fldwter
    endif

    if(i_gust>0.and.i_gustwter>0) then
      fld=zero
      fldwter=zero
      field=ptrgust

      call anbkgvar_lw(field,fld,fldwter,0)

      ptrgust=fld
      ptrgustwter=fldwter
    endif

    if(i_wspd10m>0.and.i_wspd10mwter>0) then
      fld=zero
      fldwter=zero
      field=ptrwspd10m

      call anbkgvar_lw(field,fld,fldwter,0)

      ptrwspd10m=fld
      ptrwspd10mwter=fldwter
    endif

    if(i_td2m>0.and.i_td2mwter>0) then
      fld=zero
      fldwter=zero
      field=ptrtd2m

      call anbkgvar_lw(field,fld,fldwter,0)

      ptrtd2m=fld
      ptrtd2mwter=fldwter
    endif


    if(i_mxtm>0.and.i_mxtmwter>0) then
      fld=zero
      fldwter=zero
      field=ptrmxtm

      call anbkgvar_lw(field,fld,fldwter,0)

      ptrmxtm=fld
      ptrmxtmwter=fldwter
    endif

    if(i_mitm>0.and.i_mitmwter>0) then
      fld=zero
      fldwter=zero
      field=ptrmitm

      call anbkgvar_lw(field,fld,fldwter,0)

      ptrmitm=fld
      ptrmitmwter=fldwter
    endif

    if(i_uwnd10m>0.and.i_uwnd10mwter>0) then
      fld=zero
      fldwter=zero
      field=ptruwnd10m

      call anbkgvar_lw(field,fld,fldwter,0)

      ptruwnd10m=fld
      ptruwnd10mwter=fldwter
    endif

    if(i_vwnd10m>0.and.i_vwnd10mwter>0) then
      fld=zero
      fldwter=zero
      field=ptrvwnd10m

      call anbkgvar_lw(field,fld,fldwter,0)

      ptrvwnd10m=fld
      ptrvwnd10mwter=fldwter
    endif
  endif

! Apply auto-covariance 
  if(rtma_subdomain_option) then 
      if(rtma_bkerr_sub2slab) then
         call ansmoothrf_reg_sub2slab_option(bundle)
      else
         call ansmoothrf_reg_subdomain_option(bundle)
      end if
  else
     call ansmoothrf(bundle)
  end if

!Adjoint of simple vertical smoothing
  do n=bundle%n3d,1,-1
     call gsi_bundlegetpointer ( bundle,bundle%r3(n)%shortname,ptr3d,istatus )
     call tvert_smther(ptr3d,nsmooth,nsmooth_shapiro)
  end do

  if(twodvar_regional) then
    if(i_vwnd10m>0.and.i_vwnd10mwter>0) then
      fld=ptrvwnd10m
      fldwter=ptrvwnd10mwter
      field=zero

      call anbkgvar_lw(field,fld,fldwter,1)

      ptrvwnd10m=field
!     ignore content of remaining arrays
    endif

    if(i_uwnd10m>0.and.i_uwnd10mwter>0) then
      fld=ptruwnd10m
      fldwter=ptruwnd10mwter
      field=zero

      call anbkgvar_lw(field,fld,fldwter,1)

      ptruwnd10m=field
!     ignore content of remaining arrays
    endif

    if(i_mitm>0.and.i_mitmwter>0) then
      fld=ptrmitm
      fldwter=ptrmitmwter
      field=zero

      call anbkgvar_lw(field,fld,fldwter,1)

      ptrmitm=field
!     ignore content of remaining arrays
    endif

    if(i_mxtm>0.and.i_mxtmwter>0) then
      fld=ptrmxtm
      fldwter=ptrmxtmwter
      field=zero

      call anbkgvar_lw(field,fld,fldwter,1)

      ptrmxtm=field
!     ignore content of remaining arrays
    endif

    if(i_td2m>0.and.i_td2mwter>0) then
      fld=ptrtd2m
      fldwter=ptrtd2mwter
      field=zero

      call anbkgvar_lw(field,fld,fldwter,1)

      ptrtd2m=field
!     ignore content of remaining arrays
    endif

    if(i_wspd10m>0.and.i_wspd10mwter>0) then
      fld=ptrwspd10m
      fldwter=ptrwspd10mwter
      field=zero

      call anbkgvar_lw(field,fld,fldwter,1)

      ptrwspd10m=field
!     ignore content of remaining arrays
    endif

    if(i_gust>0.and.i_gustwter>0) then
      fld=ptrgust
      fldwter=ptrgustwter
      field=zero

      call anbkgvar_lw(field,fld,fldwter,1)

      ptrgust=field
!     ignore content of remaining arrays
    endif

    if(i_q>0.and.i_qwter>0) then
      fld(:,:)=ptrq(:,:,1)
      fldwter=ptrqwter
      field=zero

      call anbkgvar_lw(field,fld,fldwter,1)

      ptrq(:,:,1)=field(:,:)
!     ignore content of remaining arrays
    endif

    if(i_t>0.and.i_twter>0) then
      fld(:,:)=ptrt(:,:,1)
      fldwter=ptrtwter
      field(:,:)=zero

      call anbkgvar_lw(field,fld,fldwter,1)

      ptrt(:,:,1)=field(:,:)
!     ignore content of remaining arrays
    endif

    if(i_ps>0.and.i_pswter>0) then
      fld=ptrps
      fldwter=ptrpswter
      field=zero

      call anbkgvar_lw(field,fld,fldwter,1)
  
      ptrps=field
!     ignore content of remaining arrays
    endif
  endif

!==> combine sst,stl, and sti into skin temperature field
  if(i_sst>0 .and. i_stl>0 .and. i_sti>0) then
    sst=ptrsst
    stl=ptrstl
    sti=ptrsti
    skint=zero

    call anbkgvar(skint,sst,stl,sti,1)

    ptrsst=skint
!   ignore contents of remaining arrays
  endif

end subroutine anbkgcov


subroutine anbkgvar(skint,sst,stl,sti,iflg)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    anbkgvar_reg      manipulate skin temp
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract: manipulate skin temp <--> sst,sfc temp, and ice temp fields
!
! program history log:
!   2005-01-22  parrish
!   2008-06-05  safford - rm unused uses
!   2012-06-25  parrish - remove _i_kind from integer constants
!
!   input argument list:
!     skint    - skin temperature grid values
!     sst      - sst grid values
!     stl      - land surface temperature grid values
!     sti      - snow/ice covered surface temperature grid values
!     iflg     - flag for skin temperature manipulation
!                0: skint --> sst,stl,sti
!                1: sst,stl,sti --> skint
!
!   output argument list:
!     skint    - skin temperature grid values
!     sst      - sst grid values
!     stl      - land surface temperature grid values
!     sti      - snow/ice covered surface temperature grid values
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2
  use guess_grids, only: isli2
  implicit none

! Declare passed variables
  integer(i_kind)                  ,intent(in   ) :: iflg
  real(r_kind),dimension(lat2,lon2),intent(inout) :: skint,sst,stl,sti

! Declare local variables
  integer(i_kind) i,j

       do j=1,lon2
          do i=1,lat2
             if(iflg == 0) then
! Break skin temperature into components
!          If land point
                if(isli2(i,j) == 1) then
                   stl(i,j)=skint(i,j)
!          If ice
                else if(isli2(i,j) == 2) then
                   sti(i,j)=skint(i,j)
!          Else treat as a water point
                else
                   sst(i,j)=skint(i,j)
                end if

             else if (iflg==1) then
! Combine sst,stl, and sti into skin temperature field
!          Land point, load land sfc t into skint
                if(isli2(i,j) == 1) then
                   skint(i,j)=stl(i,j)
!          Ice, load ice temp into skint
                else if(isli2(i,j) == 2) then
                   skint(i,j)=sti(i,j)
!          Treat as a water point, load sst into skint
                else
                   skint(i,j)=sst(i,j)
                end if
             end if
          end do
       end do

  return
end subroutine anbkgvar

subroutine anbkgvar_lw_original(field,fld,fldwter,iflg)
!$$$  subprogram documentation block
!                .      .    .                                       
! subprogram:    anbkgvar_lw land/water field manipulation
!   prgmmr: pondeca          org: np22                date: 2013-04-02
!
! abstract: manipulate field  <--> fld,fldwter
!           (based on anbkgvar)
!
! program history log:
!   2013-04-02 pondeca
!
!   input argument list:
!     field   - field grid values
!     fld     - field grid values over land
!     fldwter - field grid values over water
!     iflg    - flag for field manipulation
!               0: field --> fld,fldwter
!               1: fld,fldwter --> field
!
!   output argument list:
!     field   - field grid values
!     fld     - field grid values over land
!     fldwter - field grid values over water
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2
  use guess_grids, only: isli2
  implicit none

! Declare passed variables
  integer(i_kind)                  ,intent(in   ) :: iflg
  real(r_kind),dimension(lat2,lon2),intent(inout) :: field, &
                                                     fld,fldwter

! Declare local variables
  integer(i_kind) i,j

       do j=1,lon2
          do i=1,lat2
             if(iflg == 0) then
! Break field into components
!          If land point
                if(isli2(i,j) == 1) then
                   fld(i,j)=field(i,j)

!          Else treat as a water point
                else
                   fldwter(i,j)=field(i,j)
                end if

             else if (iflg==1) then
! Combine fld,fldwter into field
!          Land point
                if(isli2(i,j) == 1) then
                   field(i,j)=fld(i,j)
!          Treat as a water point,
                else
                   field(i,j)=fldwter(i,j)
                end if
             end if
          end do
       end do

  return
end subroutine anbkgvar_lw_original

subroutine anbkgvar_lw(field,fld,fldwter,iflg)
!$$$  subprogram documentation block
!                .      .    .                                       
! subprogram:    anbkgvar_lw land/water field manipulation
!   prgmmr: pondeca          org: np22                date: 2013-04-02
!
! abstract: manipulate field  <--> fld,fldwter
!           (based on anbkgvar)
!
! program history log:
!   2013-04-02 pondeca
!
!   input argument list:
!     field   - field grid values
!     fld     - field grid values over land
!     fldwter - field grid values over water
!     iflg    - flag for field manipulation
!               0: field --> fld,fldwter
!               1: fld,fldwter --> field
!
!   output argument list:
!     field   - field grid values
!     fld     - field grid values over land
!     fldwter - field grid values over water
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2,region_lat,region_lon, &
                     nlon_regional,nlat_regional,istart,jstart
  use guess_grids, only: isli2
  use mpimod, only: mype
  use constants, only: rad2deg
  implicit none

! Declare passed variables
  integer(i_kind)                  ,intent(in   ) :: iflg
  real(r_kind),dimension(lat2,lon2),intent(inout) :: field, &
                                                     fld,fldwter

! Declare local variables
  integer(i_kind) i,j
  integer(i_kind) iglob,jglob
  integer(i_kind) mm1
  real(r_kind) flon,flat
  logical glerlarea

! Declare local parameters
!    Great Lakes
  real(r_kind),parameter::flon1=-93._r_kind
  real(r_kind),parameter::flon2=-75._r_kind
  real(r_kind),parameter::flat1=40.5_r_kind
  real(r_kind),parameter::flat2=49.5_r_kind

!    Great Salt Lake
  real(r_kind),parameter::slon1=-113._r_kind
  real(r_kind),parameter::slon2=-112._r_kind
  real(r_kind),parameter::slat1=40.6_r_kind
  real(r_kind),parameter::slat2=41.7_r_kind

  mm1=mype+1

       do j=1,lon2
          jglob=max(1,min(j+jstart(mm1)-2,nlon_regional))
          do i=1,lat2
             iglob=max(1,min(i+istart(mm1)-2,nlat_regional)) 
             flat=region_lat(iglob,jglob)*rad2deg
             flon=region_lon(iglob,jglob)*rad2deg
             glerlarea=(flat>=flat1.and.flat<=flat2).and.(flon>=flon1.and.flon<=flon2)
             glerlarea=glerlarea.or.((flat>=slat1.and.flat<=slat2).and.(flon>=slon1.and.flon<=slon2))

             if(iflg == 0) then
! Break field into components
!          If land point
                if(isli2(i,j) == 1 .or. .not.glerlarea) then
                   fld(i,j)=field(i,j)

!          Else treat as a water point
                else
                   fldwter(i,j)=field(i,j)
                end if

             else if (iflg==1) then
! Combine fld,fldwter into field
!          Land point
                if(isli2(i,j) == 1 .or. .not.glerlarea) then
                   field(i,j)=fld(i,j)
!          Treat as a water point,
                else
                   field(i,j)=fldwter(i,j)
                end if
             end if
          end do
       end do

  return
end subroutine anbkgvar_lw

subroutine ansmoothrf(cstate)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ansmoothrf  anisotropic rf for regional mode
!   prgmmr: parrish          org: np22                date: 2005-02-14
!
! abstract: apply anisotropic rf for regional mode
!
! program history log:
!   2005-02-14  parrish
!   2008-12-04  sato - update for global mode
!   2009-01-02  todling - get mype from mpimod directly
!   2015-07-01  pondeca - rewrite to handle motley variables
!
!   input argument list:
!     cstate     - bundle containing horizontal fields to be smoothed
!
!   output argument list:
!     cstate     - bundle containing smoothed horizontal fields
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind,r_single
  use anberror, only: indices,indices_p,ngauss,pf2aP1, &
                      filter_all,filter_p2,filter_p3
  use patch2grid_mod, only: patch2grid, tpatch2grid
  use mpimod, only:  npe
  use constants, only: zero
  use gridmod, only: nlat,nlon,lat2,lon2,nsig,regional
  use control_vectors, only: nvars,nrf,nrf_var,nrf_3d
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use general_commvars_mod, only: s2g_raf
  use general_sub2grid_mod, only: general_sub2grid,general_grid2sub
  use fgrid2agrid_mod, only: fgrid2agrid,tfgrid2agrid
  use raflib, only: raf4_ad,raf4
  use raflib, only: raf4_ad_wrap,raf4_wrap
  implicit none

! Declare passed variables
  type(gsi_bundle),intent(inout) :: cstate

! Declare local variables
  integer(i_kind):: ips,ipe,jps,jpe,kps,kpe,kds,kde
  integer(i_kind):: p_ips,p_ipe,p_jps,p_jpe,p_kps,p_kpe
  integer(i_kind) i,igauss,j,k,kk,n,istatus
  real(r_kind),allocatable:: fields(:,:,:,:)
  real(r_kind),allocatable:: work(:,:,:,:)
  real(r_kind),allocatable:: worka(:,:,:)
  real(r_single),allocatable:: workb(:,:,:,:)
  real(r_kind),allocatable,dimension(:,:,:)  :: workanp,workasp
  real(r_single),allocatable,dimension(:,:,:,:):: workbnp,workbsp
  real(r_kind),pointer::rank2(:,:)
  real(r_kind),pointer::rank3(:,:,:)

! Convert from subdomain to full horizontal field distributed among processors
  kds=1
  kde=s2g_raf%num_fields
  kps=s2g_raf%kbegin_loc
  kpe=s2g_raf%kend_loc
  ips=indices%ips
  ipe=indices%ipe
  jps=indices%jps
  jpe=indices%jpe

  p_kps=s2g_raf%kbegin_loc  !indices_p%kps
  p_kpe=s2g_raf%kend_loc    !indices_p%kpe
  p_ips=indices_p%ips
  p_ipe=indices_p%ipe
  p_jps=indices_p%jps
  p_jpe=indices_p%jpe


               !WHY THE "1" IN THE ALLOCATE STATEMENTS? / MPondeca
  allocate(fields(1,lat2,lon2,kds:kde))             !MUST DEALLOCATE / MPondeca
  allocate(work(1,nlat,nlon,kps:max(kps,kpe)))   !MUST DEALLOCATE / MPondeca

  allocate(worka(ips:ipe,jps:jpe,kps:max(kps,kpe)))
  allocate(workb(ngauss,ips:ipe,jps:jpe,kps:max(kps,kpe)))

  if(.not.regional) then
     allocate(workanp(p_ips:p_ipe, p_jps:p_jpe, p_kps:p_kpe))
     allocate(workasp(p_ips:p_ipe ,p_jps:p_jpe ,p_kps:p_kpe))

     allocate(workbnp(ngauss, p_ips:p_ipe, p_jps:p_jpe, p_kps:p_kpe))
     allocate(workbsp(ngauss, p_ips:p_ipe, p_jps:p_jpe, p_kps:p_kpe ))
  end if

  fields=zero
  kk=0
  do n=1,nvars
     if (n<=nrf .and. nrf_3d(min(n,nrf))) then
        call gsi_bundlegetpointer (cstate,trim(nrf_var(n)),rank3,istatus)
        if(istatus==0) then
           do k=1,nsig
              kk=kk+1
              do j=1,lon2
                 do i=1,lat2
                    fields(1,i,j,kk)=rank3(i,j,k)
                 end do
              end do
           end do
        endif
     else        !2d flds including motley flds
        call gsi_bundlegetpointer (cstate,trim(nrf_var(n)),rank2,istatus)
        if(istatus==0) then
           kk=kk+1
           do j=1,lon2
              do i=1,lat2
                 fields(1,i,j,kk)=rank2(i,j)
              end do
           end do
        endif
     endif
  end do

! Convert from subdomain to full horizontal fields distributed among processors

  call general_sub2grid(s2g_raf,fields,work)

!  adjoint of coarse to fine grid
  do k=kps,kpe
     if(regional) then
        call tfgrid2agrid(pf2aP1,work(1,1,1,k),worka(ips,jps,k))
     else
        call tpatch2grid(work(1,1,1,k), &
                         worka  (ips,jps,k),   &
                         workanp(p_ips,p_jps,k), &
                         workasp(p_ips,p_jps,k))
     end if
  end do

!  transfer coarse grid fields to ngauss copies
  do k=kps,kpe
     do j=jps,jpe
        do i=ips,ipe
           do igauss=1,ngauss
              workb(igauss,i,j,k)=worka(i,j,k)
           end do
        end do
     end do
  end do

  if(.not.regional) then
     do k=p_kps,p_kpe
        do j=p_jps,p_jpe
           do i=p_ips,p_ipe
              do igauss=1,ngauss
                 workbnp(igauss,i,j,k)=workanp(i,j,k)
                 workbsp(igauss,i,j,k)=workasp(i,j,k)
              end do
           end do
        end do
     end do
  end if

!   apply recursive filter

!  call raf4_wrap(   workb,filter_all,ngauss,indices,npe)  /MPondeca
!  call raf4_ad_wrap(workb,filter_all,ngauss,indices,npe)  /MPondeca

  call raf4 (workb,filter_all,ngauss,ips,ipe,jps,jpe,ips,ipe,jps,jpe,kps,kpe,npe)
  call raf4_ad(workb,filter_all,ngauss,ips,ipe,jps,jpe,ips,ipe,jps,jpe,kps,kpe,npe)


  if(.not.regional) then
     call raf4_wrap(   workbnp,filter_p2,ngauss,indices_p,npe)   !WORK ON THIS / MPondeca
     call raf4_ad_wrap(workbnp,filter_p2,ngauss,indices_p,npe)   !WORK ON THIS / MPondeca 
     call raf4_wrap(   workbsp,filter_p3,ngauss,indices_p,npe)   !WORK ON THIS / MPondeca
     call raf4_ad_wrap(workbsp,filter_p3,ngauss,indices_p,npe)   !WORK ON THIS / MPondeca
  end if

!  add together ngauss copies
  worka=zero
  do k=kps,kpe
     do j=jps,jpe
        do i=ips,ipe
           do igauss=1,ngauss
              worka(i,j,k)=worka(i,j,k)+workb(igauss,i,j,k)
           end do
        end do
     end do
  end do
  deallocate(workb)

  if(.not.regional) then
     workanp=zero
     workasp=zero
     do k=p_kps,p_kpe
        do j=p_jps,p_jpe
           do i=p_ips,p_ipe
              do igauss=1,ngauss
                 workanp(i,j,k)=workanp(i,j,k)+workbnp(igauss,i,j,k)
                 workasp(i,j,k)=workasp(i,j,k)+workbsp(igauss,i,j,k)
              end do
           end do
        end do
     end do
  end if

!  coarse to fine grid
  do k=kps,kpe
     if(regional) then
        call fgrid2agrid(pf2aP1,worka(ips,jps,k),work(1,1,1,k))
     else
        call patch2grid(work(1,1,1,k), &
                        worka  (ips,jps,k),&
                        workanp(p_ips,p_jps,k),&
                        workasp(p_ips,p_jps,k))
     end if
  end do
  deallocate(worka)

  call general_grid2sub(s2g_raf,work,fields)
  deallocate(work)

  if(.not.regional) then
     deallocate(workanp)
     deallocate(workbnp)
     deallocate(workasp)
     deallocate(workbsp)
  end if

!   transfer from work back to bundle

  kk=0
  do n=1,nvars
     if (n<=nrf .and. nrf_3d(min(n,nrf))) then
        call gsi_bundlegetpointer (cstate,trim(nrf_var(n)),rank3,istatus)
        if(istatus==0) then
           do k=1,nsig
              kk=kk+1
              do j=1,lon2
                 do i=1,lat2
                    rank3(i,j,k)=fields(1,i,j,kk)
                 end do
              end do
           end do
        endif
     else        !2d flds including motley flds
        call gsi_bundlegetpointer (cstate,trim(nrf_var(n)),rank2,istatus)
        if(istatus==0) then
           kk=kk+1
           do j=1,lon2
              do i=1,lat2
                 rank2(i,j)=fields(1,i,j,kk)
              end do
           end do
        endif
     endif
  end do
  deallocate(fields)

end subroutine ansmoothrf

subroutine vert_smther(g,nsmooth,nsmooth_shapiro)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    vert_smther
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-15  lueken - added subprogram doc block
!
!   input argument list:
!    nsmooth
!    nsmooth_shapiro
!    g
!
!   output argument list:
!    g
!
! Notes:  nsmooth > 0 ==> apply 1-2-1 smoother
!         nsmooth_shapiro 0 ==> apply second moment preserving
!                               "shapiro smoother"
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use constants, only: quarter,half
  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig
  implicit none


! Declare passed variables
  integer(i_kind)                             ,intent(in   ) :: nsmooth,nsmooth_shapiro
  real(r_kind),dimension(1:lat2,1:lon2,1:nsig),intent(inout) :: g

! Declare local variables
  integer(i_kind) i,j,l,k,kp,km,kp3,km3
  real(r_kind), allocatable:: gaux(:)

  if (nsig==1)return

  allocate(gaux(1:nsig))

  if (nsmooth > 0 ) then
     do i=1,lat2
        do j=1,lon2
           do l=1,nsmooth
              gaux(1:nsig)=g(i,j,1:nsig)
              do k=1,nsig
                 kp=min(k+1,nsig) ; km=max(1,k-1)
                 g(i,j,k)=quarter*(gaux(kp)+gaux(km))+half*gaux(k)
              enddo
           enddo
        enddo
     enddo
  endif

  if (nsmooth_shapiro > 0 .and. nsmooth <= 0) then
     do i=1,lat2
        do j=1,lon2
           do l=1,nsmooth_shapiro
              gaux(1:nsig)=g(i,j,1:nsig)
              do k=1,nsig
                 kp=min(k+1,nsig) ; km=max(1,k-1)
                 kp3=min(k+3,nsig) ; km3=max(1,k-3)
                 g(i,j,k)=.28125_r_kind*(gaux(kp)+gaux(km))+half*gaux(k)-.03125_r_kind*(gaux(kp3)+gaux(km3))
              enddo
           enddo
        enddo
     enddo
  endif
  deallocate(gaux)

  return
end subroutine vert_smther


subroutine tvert_smther(g,nsmooth,nsmooth_shapiro)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tvert_smther
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-15  lueken - added subprogram doc block
!
!   input argument list:
!    nsmooth
!    nsmooth_shapiro
!    g
!
!   output argument list:
!    g
!
! Notes:  nsmooth > 0 ==>  1-2-1 smoother
!         nsmooth_shapiro 0 ==>  second moment preserving
!                               "shapiro smoother"
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use constants, only: zero,quarter,half
  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig
  implicit none


! Declare passed variables
  integer(i_kind)                             ,intent(in   ) :: nsmooth,nsmooth_shapiro
  real(r_kind),dimension(1:lat2,1:lon2,1:nsig),intent(inout) :: g

! Declare local variables
  integer(i_kind) i,j,l,k,kp,km,kp3,km3
  real(r_kind), allocatable:: gaux(:)

  if (nsig==1)return

  allocate(gaux(1:nsig))

  if (nsmooth > 0 ) then
     do i=1,lat2
        do j=1,lon2
           do l=1,nsmooth
              gaux(1:nsig)=zero
              do k=1,nsig
                 kp=min(k+1,nsig) ; km=max(1,k-1)
                 gaux(k)=gaux(k)+half*g(i,j,k)
                 gaux(km)=gaux(km)+quarter*g(i,j,k)
                 gaux(kp)=gaux(kp)+quarter*g(i,j,k)
              enddo
              g(i,j,1:nsig)=gaux(1:nsig)
           enddo
        enddo
     enddo
  endif

  if (nsmooth_shapiro > 0 .and. nsmooth <= 0) then
     do i=1,lat2
        do j=1,lon2
           do l=1,nsmooth_shapiro
              gaux(1:nsig)=zero
              do k=1,nsig
                 kp=min(k+1,nsig) ; km=max(1,k-1)
                 kp3=min(k+3,nsig) ; km3=max(1,k-3)
                 gaux(km3)=gaux(km3)-.03125_r_kind*g(i,j,k)
                 gaux(kp3)=gaux(kp3)-.03125_r_kind*g(i,j,k)
                 gaux(k)=gaux(k)+half*g(i,j,k)
                 gaux(km)=gaux(km)+.28125_r_kind*g(i,j,k)
                 gaux(kp)=gaux(kp)+.28125_r_kind*g(i,j,k)
              enddo
              g(i,j,1:nsig)=gaux(1:nsig)
           enddo
        enddo
     enddo
  endif
  deallocate(gaux)

  return
end subroutine tvert_smther

subroutine ansmoothrf_reg_sub2slab_option(cstate)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ansmoothrf_reg_subdomain_option  anisotropic rf for regional mode
!   prgmmr: parrish          org: np22                date: 2015-05-01
!
! abstract: apply anisotropic rf for regional mode (input subdomains but slab for filter)
!
! program history log:
!   2005-02-14  parrish
!   2011-02-22  zhu - use cstate to replace argument list such as p,t,q,vp,st 
!   2014-02-07  pondeca - update to include motley variables as well in 2d set of filtered variables
!   2015-05-02  parrish - make copy of ansmoothrf_reg_subdomain_option and
!                            modify to allow conversion from subdomains to slabs.
!
!   input argument list:
!     t,p,q,oz,st,stl,sti,cwmr,st,vp   -  fields to be smoothed
!
!   output argument list:
!     t,p,q,oz,st,stl,sti,cwmr,st,vp   -  smoothed fields
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind,r_single
  use anberror, only: filter_all,ngauss
  use anberror, only: pf2aP1
  use mpimod, only: npe
  use constants, only: zero,zero_single
  use gridmod, only: lat2,lon2,nsig
  use raflib, only: raf4_ad,raf4
  use control_vectors, only: nvars,nrf,nrf_var,nrf_3d
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use general_commvars_mod, only: s2g_raf
  use general_sub2grid_mod, only: general_sub2grid,general_grid2sub
  use fgrid2agrid_mod, only: fgrid2agrid,tfgrid2agrid
  implicit none

! Declare passed variables
  type(gsi_bundle),intent(inout) :: cstate

! Declare local variables
  integer(i_kind) i,igauss,j,k,kk,n,istatus
  real(r_kind),allocatable:: worka(:,:,:,:)
  real(r_single),allocatable:: workb(:,:,:,:)
  real(r_kind),pointer::rank2(:,:)
  real(r_kind),pointer::rank3(:,:,:)
  real(r_kind),allocatable::slaba(:,:,:,:)
  real(r_kind),allocatable::slabb(:,:,:)

  integer(i_kind):: kds,kde,kps,kpe,nlatf,nlonf,nlat,nlon

  kds=1
  kde=s2g_raf%num_fields
  kps=s2g_raf%kbegin_loc
  kpe=s2g_raf%kend_loc
  nlat=pf2ap1%nlata
  nlon=pf2ap1%nlona
  nlatf=pf2ap1%nlatf
  nlonf=pf2ap1%nlonf

  allocate(worka(1,lat2,lon2,kds:kde))
  allocate(slaba(1,nlat,nlon,kps:max(kps,kpe)))
  allocate(slabb(nlatf,nlonf,kps:max(kps,kpe)))
  allocate(workb(ngauss,nlatf,nlonf,kps:max(kps,kpe)))

!   transfer from bundle to worka

  worka=zero
  kk=0
  do n=1,nvars
     if (n<=nrf .and. nrf_3d(min(n,nrf))) then
        call gsi_bundlegetpointer (cstate,trim(nrf_var(n)),rank3,istatus)
        if(istatus==0) then
           do k=1,nsig
              kk=kk+1
              do j=1,lon2
                 do i=1,lat2
                    worka(1,i,j,kk)=rank3(i,j,k)
                 end do
              end do
           end do
        endif
     else        !2d flds including motley flds
        call gsi_bundlegetpointer (cstate,trim(nrf_var(n)),rank2,istatus)
        if(istatus==0) then
           kk=kk+1
           do j=1,lon2
              do i=1,lat2
                 worka(1,i,j,kk)=rank2(i,j)
              end do
           end do
        endif
     endif
  end do

!   transfer from subdomains to slabs

  call general_sub2grid(s2g_raf,worka,slaba)

!  adjoint of coarse to fine grid interpolation:

  do k=kps,kpe
     call tfgrid2agrid(pf2aP1,slaba(1,1,1,k),slabb(1,1,k))
  end do

!  transfer to ngauss copies

  do k=kps,kpe
     do j=1,nlonf
        do i=1,nlatf
           do igauss=1,ngauss
              workb(igauss,i,j,k)=slabb(i,j,k)
           end do
        end do
     end do
  end do

!   apply recursive filter

  call raf4   (workb,filter_all,ngauss,1,nlatf,1,nlonf,1,nlatf,1,nlonf,kps,kpe,npe)
  call raf4_ad(workb,filter_all,ngauss,1,nlatf,1,nlonf,1,nlatf,1,nlonf,kps,kpe,npe)

!  add together ngauss copies

  slabb=zero
  do k=kps,kpe
     do j=1,nlonf
        do i=1,nlatf
           do igauss=1,ngauss
              slabb(i,j,k)=workb(igauss,i,j,k)+slabb(i,j,k)
           end do
        end do
     end do
  end do
  deallocate(workb)

!  coarse to fine grid interpolation:

  do k=kps,kpe
     call fgrid2agrid(pf2aP1,slabb(1,1,k),slaba(1,1,1,k))
  end do
  deallocate(slabb)

  call general_grid2sub(s2g_raf,slaba,worka)
  deallocate(slaba)

!   transfer from worka back to bundle

  kk=0
  do n=1,nvars
     if (n<=nrf .and. nrf_3d(min(n,nrf))) then
        call gsi_bundlegetpointer (cstate,trim(nrf_var(n)),rank3,istatus)
        if(istatus==0) then
           do k=1,nsig
              kk=kk+1
              do j=1,lon2
                 do i=1,lat2
                    rank3(i,j,k)=worka(1,i,j,kk)
                 end do
              end do
           end do
        endif
     else        !2d flds including motley flds
        call gsi_bundlegetpointer (cstate,trim(nrf_var(n)),rank2,istatus)
        if(istatus==0) then
           kk=kk+1
           do j=1,lon2
              do i=1,lat2
                 rank2(i,j)=worka(1,i,j,kk)
              end do
           end do
        endif
     endif
  end do
  deallocate(worka)

end subroutine ansmoothrf_reg_sub2slab_option

subroutine ansmoothrf_reg_subdomain_option(cstate)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ansmoothrf_reg_subdomain_option  anisotropic rf for regional mode
!   prgmmr: parrish          org: np22                date: 2005-02-14
!
! abstract: apply anisotropic rf for regional mode (using subdomains instead of slabs)
!              NOTE: only works if filter grid is same as analysis grid
!
! program history log:
!   2005-02-14  parrish
!   2011-02-22  zhu - use cstate to replace argument list such as p,t,q,vp,st 
!   2014-02-07  pondeca - update to include motley variables as well in 2d set of filtered variables
!
!   input argument list:
!     t,p,q,oz,st,stl,sti,cwmr,st,vp   -  fields to be smoothed
!
!   output argument list:
!     t,p,q,oz,st,stl,sti,cwmr,st,vp   -  smoothed fields
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind,r_single
  use anberror, only: indices, filter_all,ngauss,halo_update_reg
  use mpimod, only: mype,npe
  use constants, only: zero,zero_single
  use gridmod, only: istart,jstart,nsig
  use raflib, only: raf4_ad_wrap,raf4_wrap
  use control_vectors, only: nvars,nrf,nrf_var,nrf_3d
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none

! Declare passed variables
  type(gsi_bundle),intent(inout) :: cstate

! Declare local variables
  integer(i_kind) i,igauss,iloc,j,jloc,k,kk,mm1,n,istatus
  real(r_single),dimension(ngauss, &
                           indices%ips:indices%ipe,&
                           indices%jps:indices%jpe,&
                           indices%kps:indices%kpe):: workb
  real(r_kind),pointer::rank2(:,:)
  real(r_kind),pointer::rank3(:,:,:)

  integer(i_kind):: ids,ide,jds,jde,kds,kde,ips,ipe,jps,jpe,kps,kpe

  ids=indices%ids; ide=indices%ide
  jds=indices%jds; jde=indices%jde
  kds=indices%kds; kde=indices%kde
  ips=indices%ips; ipe=indices%ipe
  jps=indices%jps; jpe=indices%jpe
  kps=indices%kps; kpe=indices%kpe

  mm1=mype+1

  workb=zero_single

!  transfer variables to ngauss copies
  kk=0
  do n=1,nvars
     if (n<=nrf .and. nrf_3d(min(n,nrf))) then
        call gsi_bundlegetpointer (cstate,trim(nrf_var(n)),rank3,istatus)
        if(istatus==0) then
           do k=1,nsig
              kk=kk+1
              do j=jps,jpe
                 jloc=j-jstart(mm1)+2
                 do i=ips,ipe
                    iloc=i-istart(mm1)+2
                    do igauss=1,ngauss
                       workb(igauss,i,j,kk)=rank3(iloc,jloc,k)
                    end do
                 end do
              end do
           end do
        endif
     else        !2d flds including motley flds
        call gsi_bundlegetpointer (cstate,trim(nrf_var(n)),rank2,istatus)
        if(istatus==0) then
           kk=kk+1
           do j=jps,jpe
              jloc=j-jstart(mm1)+2
              do i=ips,ipe
                 iloc=i-istart(mm1)+2
                 do igauss=1,ngauss
                    workb(igauss,i,j,kk)=rank2(iloc,jloc)
                 end do 
              end do
           end do
        endif
     endif
  end do

!   apply recursive filter

  call raf4_wrap(workb,filter_all,ngauss,indices,npe)
  call raf4_ad_wrap(workb,filter_all,ngauss,indices,npe)

!  add together ngauss copies
  kk=0
  do n=1,nvars
     if (n<=nrf .and. nrf_3d(min(n,nrf))) then
        call gsi_bundlegetpointer (cstate,trim(nrf_var(n)),rank3,istatus)
        if(istatus==0) then
           do k=1,nsig
              kk=kk+1
              do j=jps,jpe
                 jloc=j-jstart(mm1)+2
                 do i=ips,ipe
                    iloc=i-istart(mm1)+2
                    rank3(iloc,jloc,k)=zero
                    do igauss=1,ngauss
                       rank3(iloc,jloc,k)=rank3(iloc,jloc,k)+workb(igauss,i,j,kk)
                    end do
                 end do
              end do
           end do
           call halo_update_reg(rank3,nsig)
        endif
     else
        call gsi_bundlegetpointer (cstate,trim(nrf_var(n)),rank2,istatus)
        if(istatus==0) then
           kk=kk+1
           do j=jps,jpe
              jloc=j-jstart(mm1)+2
              do i=ips,ipe
                 iloc=i-istart(mm1)+2
                 rank2(iloc,jloc)=zero
                 do igauss=1,ngauss
                    rank2(iloc,jloc)=rank2(iloc,jloc)+workb(igauss,i,j,kk)
                 end do
              end do
           end do
           call halo_update_reg(rank2,1)
        endif
     endif
  end do

end subroutine ansmoothrf_reg_subdomain_option
