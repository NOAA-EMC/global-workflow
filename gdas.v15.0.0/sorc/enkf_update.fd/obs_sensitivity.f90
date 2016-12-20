module obs_sensitivity
!$$$ module documentation block
!           .      .    .                                       .
! module:   obs_sensitivity
!   prgmmr: tremolet
!
! abstract: Contains variables and routines for computation of
!           forecast sensitivity to observations.
!
! program history log:
!   2007-06-26 tremolet
!   2007-07-19 tremolet - increment sensitivity to observations
!   2009-08-07 lueken   - updated documentation
!   2010-04-30 tangborn - add pointer to carbon monoxide
!   2010-05-27 todling  - remove all user-specific TL-related references
!   2010-07-16 todling  - add reference to aero and aerol
!   2010-08-19 lueken   - add only to module use;no machine code, so use .f90
!   2011-03-29 todling  - add reference to pm2_5
!   2012-04-15 todling  - add reference to gust, vis, pblh
!   2015-07-10 pondeca  - add reference to wspd10m, td2m ,mxtm ,mitm ,pmsl,
!                         howv ,tcamt, lcbas, cldch
!
! Subroutines Included:
!   init_fc_sens  - Initialize computations
!
! Variable Definitions:
!   fcsens - forecast sensitivity gradient
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
! ------------------------------------------------------------------------------
use kinds, only: r_kind,i_kind,r_quad
use constants, only: zero, zero_quad, two
use gsi_4dvar, only: nobs_bins, l4dvar, lsqrtb, nsubwin
use jfunc, only: jiter, miter, niter, iter
use obsmod, only: cobstype, nobs_type, obsdiags, obsptr, obscounts, &
                  i_ps_ob_type, i_t_ob_type, i_w_ob_type, i_q_ob_type, &
                  i_spd_ob_type, i_srw_ob_type, i_rw_ob_type, i_dw_ob_type, &
                  i_sst_ob_type, i_pw_ob_type, i_pcp_ob_type, i_oz_ob_type, &
                  i_o3l_ob_type, i_gps_ob_type, i_rad_ob_type, i_tcp_ob_type, &
                  i_lag_ob_type, i_colvk_ob_type, i_aero_ob_type, i_aerol_ob_type, &
                  i_pm2_5_ob_type, i_gust_ob_type, i_vis_ob_type, i_pblh_ob_type, & 
                  i_wspd10m_ob_type, i_td2m_ob_type, i_mxtm_ob_type, i_mitm_ob_type, & 
                  i_pmsl_ob_type, i_howv_ob_type, i_tcamt_ob_type, i_lcbas_ob_type, & 
                  i_cldch_ob_type,i_pm10_ob_type
use mpimod, only: mype
use control_vectors, only: control_vector,allocate_cv,read_cv,deallocate_cv, &
    dot_product,assignment(=)
use state_vectors, only: allocate_state,deallocate_state
use gsi_bundlemod, only: assignment(=)
use gsi_bundlemod, only: gsi_bundle
use bias_predictors, only: predictors,allocate_preds,deallocate_preds, &
    assignment(=)
use mpl_allreducemod, only: mpl_allreduce
use gsi_4dcouplermod, only: gsi_4dcoupler_getpert
use hybrid_ensemble_parameters,only : l_hyb_ens,ntlevs_ens
! ------------------------------------------------------------------------------
implicit none
save
private
public lobsensfc,lobsensjb,lobsensincr,lobsensadj,&
       lobsensmin,iobsconv,llancdone,lsensrecompute, &
       fcsens, sensincr, &
       init_obsens, init_fc_sens, save_fc_sens, dot_prod_obs

logical lobsensfc,lobsensjb,lobsensincr, &
        lobsensadj,lobsensmin,llancdone,lsensrecompute
integer(i_kind) :: iobsconv

! ------------------------------------------------------------------------------
type(control_vector) :: fcsens
real(r_kind), allocatable :: sensincr(:,:,:)
character(len=5) :: cobtype(nobs_type)
! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------
subroutine init_obsens
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_obsens
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-07  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
implicit none

lobsensfc=.false.
lobsensjb=.false.
lobsensincr=.false.
lobsensadj=.false.
lobsensmin=.false.
lsensrecompute=.false.
llancdone=.false.
iobsconv=0

end subroutine init_obsens
! ------------------------------------------------------------------------------
subroutine init_fc_sens
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_fc_sens
!   prgmmr:      tremolet
!
! abstract: Read forecast sensitivity gradient
!
! program history log:
!   2007-06-26  tremolet - initial code
!   2009-08-07  lueken - added subprogram doc block
!   2010-05-27  todling - gsi_4dcoupler; remove dependence on GMAO specifics
!   2012-05-22  todling - update interface to getpert
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

character(len=12) :: clfile
type(gsi_bundle) :: fcgrad(nsubwin)
type(gsi_bundle) :: eval(ntlevs_ens)
type(predictors) :: zbias
type(control_vector) :: xwork
real(r_kind) :: zjx
integer(i_kind) :: ii
character(len=80),allocatable,dimension(:)::fname

if (mype==0) then
   write(6,*)'init_fc_sens: lobsensincr,lobsensfc,lobsensjb=', &
                            lobsensincr,lobsensfc,lobsensjb
   write(6,*)'init_fc_sens: lobsensadj,lobsensmin,iobsconv=', &
                            lobsensadj,lobsensmin,iobsconv
   write(6,*)'init_fc_sens: lsensrecompute=',lsensrecompute
endif

call allocate_cv(fcsens)
fcsens=zero

if (lobsensadj.and.lobsensmin) then
   write(6,*)'init_fc_sens: unknown method',lobsensadj,lobsensmin
   call stop2(155)
end if

if (iobsconv>=2) then
   allocate(sensincr(nobs_bins,nobs_type,niter(jiter)))
else
   allocate(sensincr(nobs_bins,nobs_type,1))
endif
sensincr=zero

! Initialize fcsens
if (lobsensfc) then
   if (lobsensincr) then
      clfile='xhatsave.ZZZ'
      write(clfile(10:12),'(I3.3)') jiter
      call read_cv(fcsens,clfile)
      if (jiter>1) then
         clfile='xhatsave.ZZZ'
         write(clfile(10:12),'(I3.3)') jiter-1
         call allocate_cv(xwork)
         call read_cv(xwork,clfile)
         do ii=1,fcsens%lencv
            fcsens%values(ii) = fcsens%values(ii) - xwork%values(ii)
         end do
         call deallocate_cv(xwork)
      endif
   else
      if (jiter==miter) then
         if (lobsensjb) then
            clfile='xhatsave.ZZZ'
            write(clfile(10:12),'(I3.3)') miter
            call read_cv(fcsens,clfile)
         else
!           read and convert output of GCM adjoint
            allocate(fname(nsubwin))
            fname='NULL'
            do ii=1,nsubwin
               call allocate_state(fcgrad(ii))
            end do
            call allocate_preds(zbias)
            zbias=zero
            call gsi_4dcoupler_getpert(fcgrad,nsubwin,'adm',fname)
            if (lsqrtb) then
               call control2model_ad(fcgrad,zbias,fcsens)
            else
               if (l_hyb_ens) then
                  do ii=1,ntlevs_ens
                     call allocate_state(eval(ii))
                  end do
                  eval(1)=fcgrad(1)
                  fcgrad(1)=zero
                  call ensctl2state_ad(eval,fcgrad(1),fcsens)
                  call control2state_ad(fcgrad,zbias,fcsens)
                  do ii=1,ntlevs_ens
                     call deallocate_state(eval(ii))
                  end do
               else
                  call control2state_ad(fcgrad,zbias,fcsens)
               end if
            endif
            do ii=1,nsubwin
               call deallocate_state(fcgrad(ii))
            end do
            call deallocate_preds(zbias)
            deallocate(fname)
         endif
      else
!        read gradient from outer loop jiter+1
         clfile='fgsens.ZZZ'
         WRITE(clfile(8:10),'(I3.3)') jiter+1
         call read_cv(fcsens,clfile)
      endif
   endif
   zjx=dot_product(fcsens,fcsens)
   if (mype==0) write(6,888)'init_fc_sens: Norm fcsens=',sqrt(zjx)
endif
888 format(A,3(1X,ES25.18))

! Define short name for obs types
cobtype( i_ps_ob_type)   ="spr  "
cobtype(  i_t_ob_type)   ="tem  "
cobtype(  i_w_ob_type)   ="uv   "
cobtype(  i_q_ob_type)   ="hum  "
cobtype(i_spd_ob_type)   ="spd  "
cobtype(i_srw_ob_type)   ="srw  "
cobtype( i_rw_ob_type)   ="rw   "
cobtype( i_dw_ob_type)   ="dw   "
cobtype(i_sst_ob_type)   ="sst  "
cobtype( i_pw_ob_type)   ="pw   "
cobtype(i_pcp_ob_type)   ="pcp  "
cobtype( i_oz_ob_type)   ="oz   "
cobtype(i_o3l_ob_type)   ="o3l  "
cobtype(i_gps_ob_type)   ="gps  "
cobtype(i_rad_ob_type)   ="rad  "
cobtype(i_tcp_ob_type)   ="tcp  "
cobtype(i_lag_ob_type)   ="lag  "
cobtype(i_colvk_ob_type) ="colvk"
cobtype(i_aero_ob_type)  ="aero "
cobtype(i_aerol_ob_type) ="aerol"
cobtype(i_pm2_5_ob_type) ="pm2_5"
cobtype(i_pm10_ob_type)  ="pm10 "
cobtype(i_gust_ob_type)  ="gust "
cobtype(i_vis_ob_type)   ="vis  "
cobtype(i_pblh_ob_type)  ="pblh "
cobtype(i_wspd10m_ob_type) ="w10m"
cobtype(i_td2m_ob_type)  ="td2m "
cobtype(i_mxtm_ob_type)  ="mxtm "
cobtype(i_mitm_ob_type)  ="mitm "
cobtype(i_pmsl_ob_type)  ="pmsl "
cobtype(i_howv_ob_type)  ="howv "
cobtype(i_tcamt_ob_type) ="tcamt "
cobtype(i_lcbas_ob_type) ="lcbas "
cobtype(i_cldch_ob_type) ="cldch "

return
end subroutine init_fc_sens
! ------------------------------------------------------------------------------
subroutine save_fc_sens
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    save_fc_sens
!   prgmmr:      tremolet
!
! abstract: Compute and save forecast sensitivity to observations
!
! program history log:
!   2007-06-26  tremolet - initial code
!   2009-08-07  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
implicit none

real(r_kind) :: zz
integer(i_kind) :: ii,jj,kk

! Save statistics
if (mype==0) then

!  Full stats
   do jj=1,nobs_type
      write(6,'(A,2X,I3,2X,A)')'Obs types:',jj,cobtype(jj)
   enddo
   write(6,'(A,2X,I4)')'Obs bins:',nobs_bins
   write(6,*)'Obs Count Begin'
   if (.not.allocated(obscounts)) then
      write(6,*)'save_fc_sens: obscounts not allocated'
      call stop2(156)
   end if
   do jj=1,nobs_type
      write(6,'((1X,I12))')(obscounts(jj,ii),ii=1,nobs_bins)
   enddo
   write(6,*)'Obs Count End'

   write(6,*)'Obs Impact Begin'
   do kk=1,SIZE(sensincr,3)
      if (SIZE(sensincr,3)==1) then
         write(6,'(A,I4)')'Obs Impact iteration= ',niter(jiter)
      else
         write(6,'(A,I4)')'Obs Impact iteration= ',kk
      endif
      do jj=1,nobs_type
         write(6,'((1X,ES12.5))')(sensincr(ii,jj,kk),ii=1,nobs_bins)
      enddo
   enddo
   write(6,*)'Obs Impact End'

   kk=SIZE(sensincr,3)
!  Summary by obs type
   do jj=1,nobs_type
      zz=zero
      do ii=1,nobs_bins
         zz=zz+sensincr(ii,jj,kk)
      enddo
      if (zz/=zero) write(6,'(A,2X,A3,2X,ES12.5)')'Obs Impact type',cobtype(jj),zz
   enddo

!  Summary by obs bins
   do ii=1,nobs_bins
      zz=zero
      do jj=1,nobs_type
         zz=zz+sensincr(ii,jj,kk)
      enddo
      if (zz/=zero) write(6,'(A,2X,I3,2X,ES12.5)')'Obs Impact bin',ii,zz
   enddo

endif

deallocate(sensincr)
call deallocate_cv(fcsens)

return
end subroutine save_fc_sens
! ------------------------------------------------------------------------------
real(r_kind) function dot_prod_obs()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_fc_sens
!   prgmmr:      tremolet
!
! abstract: Computes scalar product in observation space
!           (based on evaljo)
!
! program history log:
!   2007-06-27  tremolet
!   2009-01-18  todling - carry summations in quad precision
!   2009-08-07  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
implicit none

integer(i_kind) :: ii,jj,ij,it
real(r_quad)    :: zzz
real(r_quad)    :: zprods(nobs_type*nobs_bins)
! ----------------------------------------------------------

zprods(:)=zero_quad

ij=0
do ii=1,nobs_bins
   do jj=1,nobs_type
      ij=ij+1

      obsptr => obsdiags(jj,ii)%head
      do while (associated(obsptr))
         if (obsptr%luse.and.obsptr%muse(jiter)) then
            zprods(ij) = zprods(ij) + obsptr%nldepart(jiter) * obsptr%obssen(jiter)
         endif
         obsptr => obsptr%next
      enddo

   enddo
enddo

! Gather contributions
call mpl_allreduce(nobs_type*nobs_bins,qpvals=zprods)

! Save intermediate values
it=-1
if (iobsconv>=2) then
   if (iter>=1.and.iter<=niter(jiter)) it=iter
else
   it=1
endif

if (it>0) then
   ij=0
   do ii=1,nobs_bins
      do jj=1,nobs_type
         ij=ij+1
         sensincr(ii,jj,it)=zprods(ij)
      enddo
   enddo
endif

! Sum
zzz=zero_quad

ij=0
do ii=1,nobs_bins
   do jj=1,nobs_type
      ij=ij+1
      zzz=zzz+zprods(ij)
   enddo
enddo

dot_prod_obs=zzz

return
end function dot_prod_obs
! ------------------------------------------------------------------------------
end module obs_sensitivity
