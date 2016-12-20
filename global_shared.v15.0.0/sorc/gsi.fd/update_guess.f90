subroutine update_guess(sval,sbias)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    update_guess          add analysis increment to guess
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract:  This routine adds the analysis increment from the inner 
!            loop to the guess.  For certain variables, a change in
!            in units or represenation is made.
!
!            For ozone, a change of units is made from the units used
!            in the minimization to those used in the guess.  Stream 
!            function and velocity potential are converted into 
!            vorticity and divergence, the guess variables.
!
!            If the guess bias correction is turned on (biascor>=0.0),
!            then use the analysis increment to adjust the bias 
!            correction fields 
!
! program history log:
!   1990-10-06  parrish - original code
!   1994-02-02  parrish
!   1997-12-03  yang,w. - original mpi code
!   1999-06-28  yang w. - second structure mpp version
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   1999-12-07  wu      - grid version
!   2003-10-31  kleist  - add hybrid and sigma vertical coordinate
!   2003-12-22  derber  
!   2004-01-15  parrish - unified grid version (regional mode added)
!   2004-05-15  treadon - remove spectral output, leave updated guess in grid space
!   2004-06-15  treadon - reformat documenation
!   2004-07-28  treadon - add only to module use, add intent in/out
!   2004-08-27  treadon - use splib routines for grid <---> spectral transforms
!   2004-10-15  kleist  - fix sign error in laplacian computation, use separate
!               u,v work vector to update ges u,v
!   2005-02-23  wu - obtain q from normalized rh (only active when qoption=2)
!   2005-03-25  treadon - replace spectral sf,vp --> vor,div conversion with
!                         compact differencing of u,v --> vor,div
!   2005-03-28  treadon - combine hopers.f90 and update_ggrid.f90 into single
!                         routine, update_guess.f90
!   2005-06-27  guo     - support for interface to GMAO gridded fields
!   2005-09-28  parrish - fix bug in zeroing of regional xhat(noz) & xhat(ncw) arrays
!   2005-12-01  guo     - replaced reshape() in ggDivo() through a pass-
!			  by-reference interface pbr_ggDivo().
!   2005-12-09  guo     - remove GMAO divr-vort computation code.  Use
!                         unified NCEP compact_diff procedures.
!   2006-02-02  treadon - replace prsi_oz with ges_prsi
!   2006-03-27  treadon - bug fix:  use xhat_q, not xhat(nq), to update bias_q
!   2006-04-05  treadon - add update to skin temperature and bias correction
!   2006-06-08  zhang,b - change "biascor>0" to "biascor>=0" for debug purposes
!   2006-06-10  zhang,b - add update_bias and m_gsiBiases reference
!   2006-07-28  derber  - include sensible temperature update
!   2006-07-31  kleist  - change to ps instead of ln(ps)
!   2006-12-13  todling - add brute-force lower bound for positive quantities
!   2007-04-13  tremolet - use state vectors
!   2007-04-24  tremolet - 4dvar version
!   2007-07-05  todling - moved vor/div calculation elsewhere
!   2007-05-30  h.liu - remove ozone units conversion
!   2008-10-10  derber  - flip indices for predx and predxp
!   2009-01-28  todling - remove reference to original GMAO interface
!   2009-07-08  pondeca - add logical 'tsensible' for use with 2dvar only
!   2010-04-30  wu - setup for regional ozone analysis
!   2010-05-13  todling - update to use gsi_bundle
!   2010-06-01  todling - skip upd when pointer not defined
!   2010-06-02  todling - bug in upd of chem
!   2010-11-02  ting - replace loop index k in nfldsfc loop with it (bug fix)
!   2011-02-25  zhu  - add gust,vis,pblh
!   2010-05-01  todling - add support for generalized guess (use met-guess)
!                       - cwmr now in met-guess
!   2011-06-29  todling - no explict reference to internal bundle arrays
!   2011-09-20  hclin   - enforce non-negative aerosol fields
!   2011-11-01  eliu    - generalize met-guess updates for global/regional
!   2011-10-01  Hu      - GSD limitation of Q over ocean
!   2013-05-23  zhu     - add update for aircraft temperature bias correction coefficients
!   2013-10-19  todling - metguess now holds background (considerable shuffling)
!   2013-10-30  jung    - remove supersaturation
!   2014-02-12  Hu      - Adjust 2m Q based on 1st level moisture analysis increment  
!   2014-02-15  kim     - revisit various options of cloud-related updates
!   2014-04-13  todling - replace update bias code w/ call to routine in bias_predictors
!   2014-05-07  pondeca - constrain significant wave height (howv) to be >=0
!   2014-06-16  carley/zhu - add tcamt and lcbas
!   2014-06-17  carley  - remove setting nguess=0 when use_reflectivity==true
!   2014-11-28  zhu     - move update of cw to compute_derived when cw is not 
!                         state variable for all-sky radiance assimilation
!   2015-07-10  pondeca  - add cldch
!   2016-04-28  eliu    - revise update for cloud water 
!
!   input argument list:
!    sval
!    sbias
!
!   output argument list:
!    sval
!    sbias
!
!   comments:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use mpimod, only: mype
  use constants, only: zero,one,fv,max_varname_length,qmin,qcmin,tgmin,&
                       r100,one_tenth
  use jfunc, only: iout_iter,biascor,tsensible,clip_supersaturation
  use gridmod, only: lat2,lon2,nsig,&
       regional,twodvar_regional,regional_ozone
  use guess_grids, only: ges_tsen,ges_qsat,&
       nfldsig,hrdifsig,hrdifsfc,nfldsfc,dsfct
  use state_vectors, only: svars3d,svars2d
  use xhat_vordivmod, only: xhat_vor,xhat_div
  use gsi_4dvar, only: nobs_bins, hr_obsbin
  use radinfo, only: npred,jpch_rad,predx
  use pcpinfo, only: npredp,npcptype,predxp
  use aircraftinfo, only: aircraft_t_bc_pof,aircraft_t_bc,npredt,predt,ntail
  use m_gsiBiases,only : bias_hour, update_bias
  use bias_predictors, only: predictors,update_bias_preds
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_bundlemod, only: gsi_bundlegetvar
  use gsi_metguess_mod, only: gsi_metguess_bundle
  use gsi_metguess_mod, only: gsi_metguess_get
  use gsi_chemguess_mod, only: gsi_chemguess_bundle
  use gsi_chemguess_mod, only: gsi_chemguess_get
  use mpeu_util, only: getindex
  use rapidrefresh_cldsurf_mod, only: l_gsd_limit_ocean_q,l_gsd_soilTQ_nudge
  use gsd_update_mod, only: gsd_limit_ocean_q,gsd_update_soil_tq,&
       gsd_update_th2,gsd_update_q2

  implicit none

! Declare passed variables
  type(gsi_bundle), intent(inout) :: sval(nobs_bins)
  type(predictors), intent(inout) :: sbias

! Declare local variables
  character(len=*),parameter::myname='update_guess'
  character(max_varname_length),allocatable,dimension(:) :: gases
  character(max_varname_length),allocatable,dimension(:) :: guess
  character(max_varname_length),allocatable,dimension(:) :: cloud
  integer(i_kind) i,j,k,it,ij,ii,ic,id,ngases,nguess,istatus,ier
  integer(i_kind) is_t,is_q,is_oz,is_cw,is_sst
  integer(i_kind) icloud,ncloud
  integer(i_kind) idq
  real(r_kind) :: zt
  real(r_kind),pointer,dimension(:,:  ) :: ptr2dinc =>NULL()
  real(r_kind),pointer,dimension(:,:  ) :: ptr2dges =>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ptr3dinc =>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ptr3dges =>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: p_q      =>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: p_tv     =>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ptr3daux =>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ges_ql   =>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ges_qi   =>NULL()

  real(r_kind),dimension(lat2,lon2)     :: tinc_1st,qinc_1st

!*******************************************************************************
! In 3dvar, nobs_bins=1 is smaller than nfldsig. This subroutine is
! written in a way that is more efficient in that case but might not
! be the best in 4dvar.

! Get required pointers and abort if not found (RTod: needs revision)
  call gsi_bundlegetpointer(sval(1),'tv', is_t,  istatus)
  call gsi_bundlegetpointer(sval(1),'q',  is_q,  istatus)
  call gsi_bundlegetpointer(sval(1),'oz', is_oz, istatus)
  call gsi_bundlegetpointer(sval(1),'cw', is_cw, istatus)
  call gsi_bundlegetpointer(sval(1),'sst',is_sst,istatus)

! Inquire about guess fields
  call gsi_metguess_get('dim',nguess,istatus)
  if (nguess>0) then
     allocate(guess(nguess))
     call gsi_metguess_get('gsinames',guess,istatus)
  endif

! Inquire about clouds
  call gsi_metguess_get('clouds::3d',ncloud,istatus)
  if (ncloud>0) then
     allocate(cloud(ncloud))
     call gsi_metguess_get('clouds::3d',cloud,istatus)
  endif

! Inquire about chemistry fields
  call gsi_chemguess_get('dim',ngases,istatus)
  if (ngases>0) then
     allocate(gases(ngases))
     call gsi_chemguess_get('gsinames',gases,istatus)
  endif

! Initialize local arrays
  if (regional) then
     if(is_cw>0)then
        do ii=1,nobs_bins
           call gsi_bundlegetpointer (sval(ii),'cw',ptr3dinc,istatus)
           do k=1,nsig
              do j=1,lon2
                 do i=1,lat2
                    ptr3dinc(i,j,k)=zero
                 end do
              end do
           end do
        end do
     endif
     if(.not.regional_ozone) then
        if(is_oz>0)then
           do ii=1,nobs_bins
              call gsi_bundlegetpointer (sval(ii),'oz',ptr3dinc,istatus)
              do k=1,nsig
                 do j=1,lon2
                    do i=1,lat2
                       ptr3dinc(i,j,k)=zero
                    end do
                 end do
              end do
           end do
        endif
     endif
  endif

! Add increment to background
  do it=1,nfldsig
     if (nobs_bins>1) then
        zt = hrdifsig(it)
        ii = NINT(zt/hr_obsbin)+1
     else
        ii = 1
     endif
     call gsi_bundlegetpointer (sval(ii),'q' ,p_q ,istatus)
     call gsi_bundlegetpointer (sval(ii),'tv',p_tv,istatus)
! GSD modification for moisture
     if(is_q>0) then
        if(l_gsd_limit_ocean_q) then
           call gsd_limit_ocean_q(p_q)
        endif
     endif

!    Update extra met-guess fields
     do ic=1,nguess
        id=getindex(svars3d,guess(ic))
        if (id>0) then  ! Case when met_guess and state vars map one-to-one, take care of them together 
           call gsi_bundlegetpointer (sval(ii),               guess(ic),ptr3dinc,istatus)
           call gsi_bundlegetpointer (gsi_metguess_bundle(it),guess(ic),ptr3dges,istatus)
           if (trim(guess(ic))=='q') then
               call upd_positive_fldr3_(ptr3dges,ptr3dinc, qmin)
               if(clip_supersaturation) ptr3dges(:,:,:) = min(ptr3dges(:,:,:),ges_qsat(:,:,:,it))
               cycle
           endif
           if (trim(guess(ic))=='oz') then
               call upd_positive_fldr3_(ptr3dges,ptr3dinc,tgmin)
               cycle
           endif
           if (trim(guess(ic))=='tv') then
              cycle ! updating tv is trick since it relates to tsen and therefore q
                    ! since we don't know which comes first in met-guess, we
                    ! must postpone updating tv after all other met-guess fields
           endif
           icloud=getindex(cloud,guess(ic))
           if(icloud>0) then
              ptr3dges = max(ptr3dges+ptr3dinc,qcmin)
              cycle
           else  
              ptr3dges = ptr3dges + ptr3dinc
              cycle
           endif
        else  ! Case when met_guess and state vars do not map one-to-one 
           if (trim(guess(ic))=='div') then
               call gsi_bundlegetpointer (gsi_metguess_bundle(it),guess(ic),ptr3dges,istatus)
               ptr3dges = ptr3dges + xhat_div(:,:,:,ii)
               cycle
           endif
           if (trim(guess(ic))=='vor') then
               call gsi_bundlegetpointer (gsi_metguess_bundle(it),guess(ic),ptr3dges,istatus)
               ptr3dges = ptr3dges + xhat_vor(:,:,:,ii)
               cycle
           endif
        endif
        id=getindex(svars2d,guess(ic))
        if (id>0) then
           call gsi_bundlegetpointer (sval(ii),               guess(ic),ptr2dinc,istatus)
           call gsi_bundlegetpointer (gsi_metguess_bundle(it),guess(ic),ptr2dges,istatus)
           ptr2dges = ptr2dges + ptr2dinc
           if (trim(guess(ic))=='gust')  ptr2dges = max(ptr2dges,zero)
           if (trim(guess(ic))=='vis')   ptr2dges = max(min(ptr2dges,20000.0_r_kind),one_tenth)
           if (trim(guess(ic))=='wspd10m') ptr2dges = max(ptr2dges,zero)
           if (trim(guess(ic))=='pblh')  ptr2dges = max(ptr2dges,zero)
           if (trim(guess(ic))=='howv')  ptr2dges = max(ptr2dges,zero)
           if (trim(guess(ic))=='tcamt') ptr2dges = max(min(ptr2dges,r100),zero) !Cannot have > 100% or < 0% cloud amount
           if (trim(guess(ic))=='lcbas') ptr2dges = max(min(ptr2dges,20000.0_r_kind),one_tenth)
           if (trim(guess(ic))=='cldch') ptr2dges = max(min(ptr2dges,20000.0_r_kind),one_tenth)
           cycle
        endif
     enddo
     if (getindex(svars3d,'ql')>0 .and. getindex(svars3d,'qi')>0) then
        ier=0
        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'cw',ptr3dges,istatus) ; ier=istatus                                                              
        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ql',ges_ql,  istatus) ; ier=ier+istatus                                        
        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qi',ges_qi,  istatus) ; ier=ier+istatus                                
        if (ier==0) then
           ptr3dges = ges_ql + ges_qi
        endif
     endif
!    At this point, handle the Tv exception since by now Q has been updated 
!    NOTE 1: This exceptions is unnecessary: all we need to do is put tsens in the
!    state-vector instead of tv (but this will require changes elsewhere).
!    For now we keep the exception code in place
!    NOTE 2: the following assumes tv has same name in met-guess and increment vectors
     id=getindex(svars3d,'tv')
     if (id>0) then
        call gsi_bundlegetpointer (sval(ii),               'tv',ptr3dinc,istatus)
        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'tv',ptr3dges,istatus)
        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'q ',ptr3daux,idq)
        if (.not.twodvar_regional .or. .not.tsensible) then
!           TV analyzed; Tsens diagnosed
            ptr3dges = ptr3dges + ptr3dinc
            if(idq==0) ges_tsen(:,:,:,it) = ptr3dges/(one+fv*ptr3daux)
        else
!           Tsens analyzed; Tv diagnosed
            ges_tsen(:,:,:,it) = ges_tsen(:,:,:,it) + ptr3dinc
            if(idq==0) ptr3dges = ges_tsen(:,:,:,it)*(one+fv*ptr3daux)
        endif
     endif
!    Update trace gases
     do ic=1,ngases
        id=getindex(svars3d,gases(ic))
        if (id>0) then
           call gsi_bundlegetpointer (sval(ii),                gases(ic),ptr3dinc,istatus)
           call gsi_bundlegetpointer (gsi_chemguess_bundle(it),gases(ic),ptr3dges,istatus)
           call upd_positive_fldr3_(ptr3dges,ptr3dinc,tgmin)
           cycle
        endif
        id=getindex(svars2d,gases(ic))
        if (id>0) then
           call gsi_bundlegetpointer (sval(ii),                gases(ic),ptr2dinc,istatus)
           call gsi_bundlegetpointer (gsi_chemguess_bundle(it),gases(ic),ptr2dges,istatus)
           call upd_positive_fldr2_(ptr2dges,ptr2dinc,tgmin)
           cycle
        endif
     enddo
! update surface and soil    
     if (l_gsd_soilTQ_nudge ) then
        if(is_q>0) then
           do j=1,lon2
              do i=1,lat2
                  qinc_1st(i,j)=p_q(i,j,1)
              end do
           end do
        endif
        if(is_t > 0) then
           do j=1,lon2
              do i=1,lat2
                  tinc_1st(i,j)=p_tv(i,j,1)
              end do
           end do
        endif
        call  gsd_update_soil_tq(tinc_1st,is_t,qinc_1st,is_q)
     endif  ! l_gsd_soilTQ_nudge
     if (l_gsd_soilTQ_nudge .and. is_t>0) then
        do j=1,lon2
           do i=1,lat2
              tinc_1st(i,j)=p_tv(i,j,1)
           end do
        end do
        call  gsd_update_th2(tinc_1st)
     endif ! l_gsd_th2_adjust
     if (l_gsd_soilTQ_nudge .and. is_q>0) then
        do j=1,lon2
           do i=1,lat2
              qinc_1st(i,j)=p_q(i,j,1)
           end do
        end do
        call  gsd_update_q2(qinc_1st)
     endif ! l_gsd_q2_adjust

  end do

  if(ngases>0) deallocate(gases)
  if(ncloud>0) deallocate(cloud)
  if(nguess>0) deallocate(guess)

  if(is_sst>0) then
     do it=1,nfldsfc
        if (nobs_bins>1) then
           zt = hrdifsfc(it)
           ii = NINT(zt/hr_obsbin)+1
        else
           ii = 1
        endif
        ij=0
        call gsi_bundlegetpointer (sval(ii),'sst',ptr2dinc,istatus)
        do j=1,lon2
           do i=1,lat2
              dsfct(i,j,it)=dsfct(i,j,it)+ptr2dinc(i,j)
           end do
        end do
     end do
  endif

  if (regional_ozone) then
     if(is_oz>0) then
        do it=1,nfldsig
           call gsi_bundlegetpointer (gsi_metguess_bundle(it),'oz',ptr3dges,istatus)
           if(istatus/=0) cycle
           do k=1,nsig
              do j=1,lon2
                 do i=1,lat2  ! this protect should not be needed (RTodling)
                    if(ptr3dges(i,j,k)<zero) ptr3dges(i,j,k) = 1.e-10_r_kind
                 enddo
              enddo
           enddo
        enddo
     endif
  endif

! If requested, update background bias correction
  if (biascor >= zero) then
     if (mype==0) write(iout_iter,*) &
        'UPDATE_GUESS:  update background bias correction.  biascor=',biascor

!    Update bias correction field

     call update_bias(sval(1),xhat_div(:,:,:,1),xhat_vor(:,:,:,1),hour=bias_hour)

  endif

 
! Update bias correction coefficients.
  call update_bias_preds (twodvar_regional,sbias)

  if(mype==0) write(6,*) trim(myname), ': successfully complete'
  return
  contains
  subroutine upd_positive_fldr2_(ges,xinc,threshold)
  real(r_kind) :: threshold
  real(r_kind),pointer :: ges(:,:)
  real(r_kind),pointer :: xinc(:,:)
  do j=1,lon2
     do i=1,lat2
        ges(i,j) = max(ges(i,j)+ xinc(i,j),threshold)
     end do
  end do
  end subroutine upd_positive_fldr2_
  subroutine upd_positive_fldr3_(ges,xinc,threshold)
  real(r_kind) :: threshold
  real(r_kind),pointer :: ges(:,:,:)
  real(r_kind),pointer :: xinc(:,:,:)
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           ges(i,j,k) = max(ges(i,j,k)+ xinc(i,j,k),threshold)
        end do
     end do
  end do
  end subroutine upd_positive_fldr3_
end subroutine update_guess
