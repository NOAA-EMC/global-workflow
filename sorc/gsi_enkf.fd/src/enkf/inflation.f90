module inflation
!$$$  module documentation block
!
! module: inflation           inflate posterior ensemble perturbations
!                             by an factor proportional to the amount
!                             the ensemble spread is reduced by the assimilation
!                             of observations.
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract:  posterior ensemble inflation. Contains two components.
!
! 1) relaxation-to-prior spread (RTPS)  posterior ensemble multiplicative inflation.
!  The amount of inflation is given at each analysis grid point by:
!
!  r = analpertwt*((stdev_prior-stdev_posterior)/stdev_posterior) + 1 
!
!  where stdev_prior is the prior ensemble standard deviation,
!  stdev_posterior is posterior ensemble standard deviation
!  (before inflation), and r is inflation factor applied to each ensemble
!  member deviation from the ensemble mean.
!  if analpertwt=1, ensemble inflated so posterior standard deviation same as prior.
!  if analpertwt=0, there is no inflation.
!  analpertwt is a namelist parameter defined in module params.
!  The inflation factor can be different for each variable, level and horizontal
!  grid point and will in general be larger where observations are dense.
!
!  if the smoothing parameter (smoothparm) > 0, the estimated inflation
!  factor is smoothed using a Gaussian spectral filter with an efolding
!  scale of smoothparm.
!
!  The minimum and maximum values allowed can be controlled by the
!  namelist parameters covinflatemin and covinflatemax.
!
! 2) relaxation-to-prior perturbation inflation (RTPP)
!
!  xa_pert = (1-analpertwt_rtpp)*xa_pert + analpertwt_rtpp*xb_pert
!
!  analpertwt_rtpp is a namelist parameter defined in module params.
!  if =1, then analysis perturbations are re-set to background perturbations
!  if between 0 and 1, analysis perts are linear combination of analysis
!  and background perts.
!
!
! Public Subroutines:
!  inflate_ens: apply inflation to the ensemble perturbations after
!   the EnKF analysis step.
!
! Public Variables: None
!   
! Modules Used: mpisetup, params, kinds, covlocal, controlvec, gridinfo, loadal
!
! program history log:
!   2009-02-23:  Initial version.
!   2016-05-02:  shlyaeva: Modification for reading state vector from table
!   2016-11-29:  shlyaeva: Modification for using control vector (control and state
!                used to be the same) and the "chunks" come from loadbal 
!   2017-05-12: Johnson, Y. Wang and X. Wang - Add height-dependent inflation,
!                                              POC:xuguang.wang@ou.edu
! attributes:
!   language: f95
!
!$$$

use mpimod, only: mpi_comm_world
use mpisetup, only: mpi_real4,mpi_sum,mpi_comm_io,mpi_in_place,numproc,nproc,&
                mpi_integer,mpi_wtime,mpi_status,mpi_real8,mpi_max,mpi_realkind,&
                mpi_min

use params, only: analpertwtnh,analpertwtsh,analpertwttr,nanals,nlevs,&
                  analpertwtnh_rtpp,analpertwtsh_rtpp,analpertwttr_rtpp,&
                  latbound, delat, datapath, covinflatemax, save_inflation, &
                  covinflatemin, nlons, nlats, smoothparm, nbackgrounds,&
                  covinflatenh,covinflatesh,covinflatetr,lnsigcovinfcutoff,taperanalperts
use kinds, only: r_single, i_kind
use mpeu_util, only: getindex
use constants, only: one, zero, rad2deg, deg2rad
use covlocal, only: latval, taper
use controlvec, only: ncdim, cvars3d, cvars2d, nc3d, nc2d, clevels, index_pres
! note: vars2d_landonly currently only defined for gridio_gfs, but smoothing only coded for gfs.
use gridinfo, only: latsgrd, logp, npts, nlevs_pres, vars2d_landonly, taper_vert
use loadbal, only: indxproc, numptsperproc, npts_max, anal_chunk, anal_chunk_prior
use smooth_mod, only: smooth

implicit none

private
public :: inflate_ens

contains

subroutine inflate_ens()

integer(i_kind),parameter :: ndiag = 3
!  Currently 3 diagnostic areas (ndiag =3)
!  Area 1 northern hemisphere
!  Area 2 southern hemisphere
!  Area 3 tropics

real(r_single) sprdmin, sprdmax, sprdmaxall, &
  sprdminall, deglat,analpertwt,analpertwt_rtpp, fsprd, asprd
real(r_single),dimension(ndiag) :: sumcoslat,suma,suma2,sumi,sumf,sumitot,sumatot, &
     sumcoslattot,suma2tot,sumftot
real(r_single) fnanalsml,coslat
integer(i_kind) i,k,nlev,nn,iunit,ierr,nb,nnlvl,ps_ind, this_ind, ind
integer(i_kind), dimension(8) :: soil_index
character(len=500) filename
real(r_single), allocatable, dimension(:,:) :: tmp_chunk2,covinfglobal,store_presmooth
real(r_single) r

fnanalsml = one/(real(nanals-1,r_single))

if (analpertwtnh_rtpp > 1.e-5_r_single .and. &
    analpertwtnh_rtpp > 1.e-5_r_single .and. &
    analpertwttr_rtpp > 1.e-5_r_single) then
if (nproc == 0) print *,'performing RTPP inflation...'
nbloop: do nb=1,nbackgrounds ! loop over time levels in background
! First perform RTPP ensemble inflation,
! as first described in:
! Zhang, F., C. Snyder, and J. Sun, 2004: Tests of an ensemble 
! Kalman Filter for convective-scale data assim-imilation:
! Impact of initial estimate and observations. 
! Mon. Wea. Rev., 132, 1238-1253. 
do nn=1,ncdim
 do i=1,numptsperproc(nproc+1)
   deglat = rad2deg*latsgrd(indxproc(nproc+1,i))
   ! coefficent can be different in NH, TR, SH.
   analpertwt_rtpp = &
     latval(deglat,analpertwtnh_rtpp,analpertwttr_rtpp,analpertwtsh_rtpp)
   anal_chunk(:,i,nn,nb) = analpertwt_rtpp*anal_chunk_prior(:,i,nn,nb) +&
     (one-analpertwt_rtpp)*anal_chunk(:,i,nn,nb)
 end do
end do
end do nbloop ! end loop over time levels in background
endif

! if no RTPS inflation desired, return
if (abs(analpertwtnh) < 1.e-5_r_single .and. &
    abs(analpertwttr) < 1.e-5_r_single .and. &
    abs(analpertwtsh) < 1.e-5_r_single) return

if (nproc == 0) print *,'performing RTPS inflation...'

! now perform RTPS inflation
nbloop2: do nb=1,nbackgrounds ! loop over time levels in background

! adaptive posterior inflation based upon ratio of posterior to prior spread.
allocate(tmp_chunk2(npts_max,ncdim))
tmp_chunk2 = covinflatemin

! compute inflation.
sumf = zero
suma = zero
sumcoslat = zero
sprdmax = -9.9e31_r_single
sprdmin = 9.9e31_r_single
ps_ind  = getindex(cvars2d, 'ps')  ! Ps (2D)

do nn=1,ncdim
 do i=1,numptsperproc(nproc+1)
   deglat = rad2deg*latsgrd(indxproc(nproc+1,i))

   ! compute stdev of prior and posterior.
   asprd = sum(anal_chunk(:,i,nn,nb)**2)*fnanalsml  
   fsprd = sum(anal_chunk_prior(:,i,nn,nb)**2)*fnanalsml

   ! inflation proportional to posterior stdev reduction
   ! if analpertwt=1, ensemble inflated so posterior stdev same as prior.
   ! if analpertwt=0, no inflation.
   ! coefficent can be different in NH, TR, SH.
   analpertwt = latval(deglat,analpertwtnh,analpertwttr,analpertwtsh)
   fsprd = max(fsprd,tiny(fsprd))
   asprd = max(asprd,tiny(asprd))

   ! area mean surface pressure posterior and prior spread.
   ! (this diagnostic only makes sense for grids that are regular in longitude)
   if (ps_ind > 0 .and. nn == clevels(nc3d) + ps_ind) then 
      coslat=cos(latsgrd(indxproc(nproc+1,i)))
      if (fsprd > sprdmax) sprdmax = fsprd
      if (fsprd < sprdmin) sprdmin = fsprd
      if (deglat > latbound) then 
         suma(1) = suma(1) + asprd*coslat
         sumf(1) = sumf(1) + fsprd*coslat
         sumcoslat(1) = sumcoslat(1) + coslat
      else if (deglat < -latbound) then
         suma(2) = suma(2) + asprd*coslat
         sumf(2) = sumf(2) + fsprd*coslat
         sumcoslat(2) = sumcoslat(2) + coslat
      else
         suma(3) = suma(3) + asprd*coslat
         sumf(3) = sumf(3) + fsprd*coslat
         sumcoslat(3) = sumcoslat(3) + coslat
      end if
   end if

   fsprd = sqrt(fsprd); asprd = sqrt(asprd)
   ! clip values to avoid NaNs.
   asprd = max(asprd,tiny(asprd))
   fsprd = max(fsprd,tiny(fsprd))
   tmp_chunk2(i,nn) = analpertwt*((fsprd-asprd)/asprd) + 1.0

   if ( nn == ncdim ) then
       nnlvl=nlevs_pres
   else
       nnlvl=nn - nn/nlevs*nlevs
   end if
   if( nnlvl == 0 ) nnlvl = nlevs
   
   r=abs((logp(indxproc(nproc+1,i),nnlvl)-logp(indxproc(nproc+1,i),nlevs_pres))/lnsigcovinfcutoff)
   if ( r > 0.75_r_single ) then
     r=1.0_r_single
   endif
   
   tmp_chunk2(i,nn) = tmp_chunk2(i,nn) + &
             taper(r)*latval(deglat,covinflatenh,covinflatetr,covinflatesh)
   ! min/max inflation set by covinflatemin/covinflatemax.
   tmp_chunk2(i,nn) = max(covinflatemin,min(tmp_chunk2(i,nn),covinflatemax))

 end do
end do

iunit = 88 ! inflation output file (used if save_inflation=.true.)
filename = trim(adjustl(datapath))//"covinflate.dat"
if (smoothparm .gt. zero) then
   ! inflation smoothing.
   ! (warning: this requires a lot of memory)
   allocate(covinfglobal(npts,ncdim))
   covinfglobal=zero
   do i=1,numptsperproc(nproc+1)
      covinfglobal(indxproc(nproc+1,i),:) = tmp_chunk2(i,:)
   end do
   !call mpi_allreduce(mpi_in_place,covinfglobal,npts*ncdim,mpi_real4,mpi_sum,mpi_comm_world,ierr)
   do nn=1,ncdim
     call mpi_allreduce(mpi_in_place,covinfglobal(1,nn),npts,mpi_real4,mpi_sum,mpi_comm_world,ierr)
   enddo
   ! do not apply smoothing to soil temp. or soil moisture (not globally defined)

   ind = 0
   do i = 1,8
       this_ind = getindex(cvars2d, vars2d_landonly(i))
       if (this_ind>0) then
            ind=ind+1
            soil_index(ind)=this_ind
       endif
   enddo

   if (ind>0) then
        allocate(store_presmooth(npts,ind))
        do i = 1, ind
           store_presmooth(:,i) = covinfglobal(:,clevels(nc3d)+soil_index(i))
        enddo
   endif

   call smooth(covinfglobal)

   if (ind>0) then
        do i = 1, ind
           covinfglobal(:,clevels(nc3d) + soil_index(i))  = store_presmooth(:,i)
        enddo
        deallocate(store_presmooth)
   endif

   where (covinfglobal < covinflatemin) covinfglobal = covinflatemin
   where (covinfglobal > covinflatemax) covinfglobal = covinflatemax
   do i=1,numptsperproc(nproc+1)
      tmp_chunk2(i,:) = covinfglobal(indxproc(nproc+1,i),:)
   end do
   if(nproc == 0)then
      do i=1,nc3d
        print *,'min/max ',cvars3d(i),' inflation = ',minval(covinfglobal(:,(i-1)*nlevs+1:i*nlevs)),maxval(covinfglobal(:,(i-1)*nlevs+1:i*nlevs))
      enddo
      do i=1,nc2d
        print *,'min/max ',cvars2d(i),' inflation = ',minval(covinfglobal(:,nc3d*nlevs+i)),maxval(covinfglobal(:,nc3d*nlevs+i))
      enddo
      ! write out inflation.
      if (save_inflation) then
         open(iunit,form='unformatted',file=filename,access='direct',recl=npts*ncdim*4)
         write(iunit,rec=1) covinfglobal 
         close(iunit)
      endif
   end if
   deallocate(covinfglobal)
else if (save_inflation) then
   allocate(covinfglobal(npts,ncdim))
   covinfglobal=zero
   do i=1,numptsperproc(nproc+1)
      covinfglobal(indxproc(nproc+1,i),:) = tmp_chunk2(i,:)
   end do
   do nn=1,ncdim
     call mpi_allreduce(mpi_in_place,covinfglobal(1,nn),npts,mpi_real4,mpi_sum,mpi_comm_world,ierr)
   enddo
   if (nproc == 0) then
      open(iunit,form='unformatted',file=filename,access='direct',recl=npts*ncdim*4)
      write(iunit,rec=1) covinfglobal 
      close(iunit)
      deallocate(covinfglobal)
   endif
end if

suma2 = zero
sumi = zero

! apply inflation.
do nn=1,ncdim
 nlev = index_pres(nn) ! vertical index for i'th control variable
 if (nlev == nlevs+1) nlev=-1 ! 2d field
 do i=1,numptsperproc(nproc+1)

   ! inflate posterior perturbations.
   anal_chunk(:,i,nn,nb) = tmp_chunk2(i,nn)*anal_chunk(:,i,nn,nb)

   ! optionally 'deflate' perturbations to reduce spread near top of model
   if (taperanalperts .and. nlev > 0) then
      anal_chunk(:,i,nn,nb) = taper_vert(nlev)*anal_chunk(:,i,nn,nb)
   endif

   ! area mean surface pressure posterior spread, inflation.
   ! (this diagnostic only makes sense for grids that are regular in longitude)
   if (ps_ind > 0 .and. nn == clevels(nc3d) + ps_ind) then 
      coslat=cos(latsgrd(indxproc(nproc+1,i)))
      deglat = rad2deg*latsgrd(indxproc(nproc+1,i))
      if (deglat > latbound) then 
         suma2(1) = suma2(1) + &
         sum(anal_chunk(:,i,nn,nb)**2)*coslat*fnanalsml
         sumi(1) = sumi(1) + tmp_chunk2(i,nn)*coslat
      else if (deglat < -latbound) then
         suma2(2) = suma2(2) + &
         sum(anal_chunk(:,i,nn,nb)**2)*coslat*fnanalsml
         sumi(2) = sumi(2) + tmp_chunk2(i,nn)*coslat
      else
         suma2(3) = suma2(3) + &
         sum(anal_chunk(:,i,nn,nb)**2)*coslat*fnanalsml
         sumi(3) = sumi(3) + tmp_chunk2(i,nn)*coslat
      end if
   end if

 end do
end do

deallocate(tmp_chunk2)

! collect statistics of area mean inflation, posterior and prior standard deviation for ps.
call mpi_reduce(sprdmin,sprdminall,1,mpi_real4,mpi_min,0,mpi_comm_world,ierr)
call mpi_reduce(sprdmax,sprdmaxall,1,mpi_real4,mpi_max,0,mpi_comm_world,ierr)
call mpi_reduce(sumf,sumftot,ndiag,mpi_real4,mpi_sum,0,mpi_comm_world,ierr)
call mpi_reduce(sumi,sumitot,ndiag,mpi_real4,mpi_sum,0,mpi_comm_world,ierr)
call mpi_reduce(suma,sumatot,ndiag,mpi_real4,mpi_sum,0,mpi_comm_world,ierr)
call mpi_reduce(suma2,suma2tot,ndiag,mpi_real4,mpi_sum,0,mpi_comm_world,ierr)
call mpi_reduce(sumcoslat,sumcoslattot,ndiag,mpi_real4,mpi_sum,0,mpi_comm_world,ierr)
if (nproc == 0 .and. ps_ind > 0) then
   print *,'inflation stats, time level: ',nb
   print *,'---------------------------------'
   do i=1,size (sumcoslattot) 
     if (sumcoslattot(i) .gt. tiny(sumcoslattot(i))) then
        sumftot(i) = sqrt(sumftot(i)/sumcoslattot(i))
        sumatot(i) = sqrt(sumatot(i)/sumcoslattot(i))
        suma2tot(i) = sqrt(suma2tot(i)/sumcoslattot(i))
        sumitot(i) = sumitot(i)/sumcoslattot(i)
     else
        sumftot(i) =0_r_single
        sumatot(i) =0_r_single
        suma2tot(i)=0_r_single
        sumitot(i) =0_r_single 
     endif
   enddo
   print *,'global ps prior std. dev min/max = ',sqrt(sprdminall),sqrt(sprdmaxall)
! NH first.
   if (sumcoslattot(1) .gt. tiny(sumcoslattot(1))) then
   print *,'NH mean ps prior standard deviation = ',sumftot(1)
   print *,'NH mean ps posterior standard deviation (before inflation)= ',sumatot(1)
   print *,'NH mean ps posterior standard deviation (after inflation) = ',suma2tot(1)
   print *,'NH mean ps inflation = ',sumitot(1)
   endif

! now SH.
   if (sumcoslattot(2) .gt. tiny(sumcoslattot(2))) then
   print *,'SH mean ps prior standard deviation = ',sumftot(2)
   print *,'SH mean ps posterior standard deviation (before inflation)= ',sumatot(2)
   print *,'SH mean ps posterior standard deviation (after inflation) = ',suma2tot(2)
   print *,'SH mean ps inflation = ',sumitot(2)
   endif
! now tropics.
   if (sumcoslattot(3) .gt. tiny(sumcoslattot(3))) then
   print *,'TR mean ps prior standard deviation = ',sumftot(3)
   print *,'TR mean ps posterior standard deviation (before inflation)= ',sumatot(3)
   print *,'TR mean ps posterior standard deviation (after inflation) = ',suma2tot(3)
   print *,'TR mean ps inflation = ',sumitot(3)
   endif
end if

end do nbloop2 ! end loop over time levels in background


end subroutine inflate_ens

end module inflation
