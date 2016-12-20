module adjtest_obs
!$$$ module documentation block
!           .      .    .                                       .
! module:   adjtest_obs
!  prgmmr: Syed RH Rizvi, NCAR/NESL/MMM/DAS
!
! abstract: Performs adjoint test for linear observation operator
!
! program history log:
!   2012-09-14  Rizvi, NCAR/NESL/MMM/DAS - initial code
!
! subroutines included:
!   sub adtest_obs
!   sub get_lhs
!
! variable definition:
!
! attributes:
!   language: f90
!   machine: NCAR/IBM
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use gsi_4dvar, only: lsqrtb, nsubwin, nobs_bins
  use obsmod, only: yobs, obs_handle, &
           t_ob_type,q_ob_type,w_ob_type,ps_ob_type,pw_ob_type, spd_ob_type, &
           rw_ob_type, dw_ob_type, srw_ob_type, tcp_ob_type, sst_ob_type, &
           oz_ob_type, o3l_ob_type, gps_ob_type, colvk_ob_type, pm2_5_ob_type, rad_ob_type, &
           pm10_ob_type, aero_ob_type, &
           gust_ob_type,vis_ob_type,pblh_ob_type, & 
           wspd10m_ob_type,td2m_ob_type,mxtm_ob_type,mitm_ob_type,pmsl_ob_type, & 
           howv_ob_type,tcamt_ob_type,lcbas_ob_type,cldch_ob_type

  use jfunc, only: jiter
  use constants, only: zero, two
  use mpimod, only: npe, ierror,mpi_comm_world, mpi_sum,mpi_integer4,mpi_rtype,mype
  use control_vectors, only: control_vector,allocate_cv,random_cv, deallocate_cv,assignment(=)
  use state_vectors, only: allocate_state,deallocate_state,prt_state_norms,dot_product
  use gsi_bundlemod, only : gsi_bundle,assignment(=)
  use bias_predictors, only: predictors,allocate_preds,deallocate_preds, assignment(=)
  use intallmod, only: intall

  implicit none
  private
  public adtest_obs

contains

subroutine adtest_obs
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    adtest_obs
!  prgmmr: Syed RH Rizvi, NCAR/NESL/MMM/DAS
!
! abstract: Performs adjoint test for linear observation operator
!
! program history log:
!
!   2012-09-14  Rizvi, NCAR/NESL/MMM/DAS - initial code
!
!   input argument list:
!    xhat
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine: NCAR/IBM
!
!$$$ end documentation block

  implicit none

! Declare local variables  
  type(control_vector) :: xtest1
  type(gsi_bundle) :: mval(nsubwin)
  type(gsi_bundle) :: stest1(nobs_bins)
  type(gsi_bundle) :: rval(nobs_bins)
  type(predictors) :: sbias1
  type(predictors) :: rbias1
  integer(i_kind)  :: ii,idig
  real(r_kind) :: adj_lhs, adj_rhs, adj_lhs_tmp, adj_rhs_tmp
  real(r_kind) :: adj_rhs_tsen_tmp, adj_rhs_tsen
  real(r_kind) :: zz1, zz2, zz3

! ----------------------------------------------------------------------
  if (mype==0) write(6,*)'ADTEST_OBS starting for jiter= ',jiter
! Allocate local variables
  call allocate_cv(xtest1)

  do ii=1,nsubwin
     call allocate_state(mval(ii))
  end do

  do ii=1,nobs_bins 
     call allocate_state(stest1(ii))
  end do
  call allocate_preds(sbias1)

! Initialize control space vectors
  call random_cv(xtest1)

  do ii=1,nsubwin   
     mval(ii)=zero
  enddo
  sbias1=zero
  lsqrtb = .false.
  call control2state(xtest1,mval,sbias1)

  do ii=1,nobs_bins
     stest1(ii)=mval(1)    
  enddo

  do ii = 1, nobs_bins
     call prt_state_norms(stest1(ii),'Initial input')
  end do

! Run obs adj test

  do ii=1,nobs_bins 
     call allocate_state(rval(ii))
  end do
  call allocate_preds(rbias1)


  call intall(stest1,sbias1,rval,rbias1)

  do ii = 1, nobs_bins
     call prt_state_norms(rval(ii),'After Adjoint')
  end do

 
  adj_rhs_tmp =0._r_kind
  adj_rhs     =0._r_kind
  adj_rhs_tsen=0._r_kind
  adj_rhs_tsen_tmp=0._r_kind

  adj_rhs = dot_product(stest1,rval)
  if(mype == 0) write(6,*)'Final obs_adj_RHS= ',adj_rhs
! Now compute LHS
  adj_lhs_tmp = 0._r_kind
  adj_lhs     = 0._r_kind

  do ii=1,nobs_bins
     call get_lhs(yobs(ii), adj_lhs_tmp)
  end do ! loop over nobs_bins

  call mpi_allreduce(adj_lhs_tmp,adj_lhs,1,mpi_rtype,mpi_sum,mpi_comm_world,ierror)
  if(mype == 0) write(6,*)'Final obs_adj_LHS= ',adj_lhs

! Diagnostics
  zz1 = adj_rhs
  zz2 = adj_lhs
!
  if ( abs(zz1+zz2) > sqrt(tiny(zz3)) ) then
     zz3=two*abs(zz1-zz2)/(zz1+zz2)
  else
     zz3=abs(zz1-zz2)
  endif
  idig= int(-log(zz3+tiny(zz3))/log(10.0_r_kind))


  if (mype==0) then
     write(6,'(A)')' ADTEST_OBS             0.123456789012345678'
     write(6,'(A,ES25.18)')' ADTEST_OBS <F*F.Y,X>= ',zz1
     write(6,'(A,ES25.18)')' ADTEST_OBS <F.Y,F.Y>= ',zz2
     write(6,'(A,i3,   A)')' ADTEST_OBS ',idig,' digits are identical'
     write(6,'(A,ES25.18)')' ADTEST_OBS rel. err.= ',zz3
     write(6,'(A,ES25.18)')' ADTEST_OBS mach.eps = ',epsilon(zz3)
  endif

! Release local variables
  call deallocate_cv(xtest1)
  do ii=1,nsubwin
     call deallocate_state(stest1(ii))
  enddo
  call deallocate_preds(sbias1)
! ----------------------------------------------------------------------

  if (mype==0) write(6,*)'ADTEST_OBS finished'
  return
end subroutine adtest_obs
! ----------------------------------------------------------------------
subroutine get_lhs(yobs, lhs) 
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_lhs     
!  prgmmr: Syed RH Rizvi, NCAR/NESL/MMM/DAS
!
! abstract: Computed lhs of the adjoint equation for linear observation operator
!
! program history log:
!
!   2012-09-14  Rizvi, NCAR/NESL/MMM/DAS - initial code
!
!   input argument list:
!    yobs  
!
!   output argument list:
!   lhs
!
! attributes:
!   language: f90
!   machine: NCAR/IBM
!
!$$$ end documentation block

  implicit none

! Declare passed variables
  type(obs_handle), intent(in   ) :: yobs
  real(r_kind), intent (inout)    :: lhs
  type(t_ob_type),     pointer  :: tptr     ! 1. temperature
  type(q_ob_type),     pointer  :: qptr     ! 2. moisture
  type(w_ob_type),     pointer  :: wptr     ! 3. Wind (u and v componenets)
  type(ps_ob_type),    pointer  :: psptr    ! 4. Surface pressure
  type(pw_ob_type),    pointer  :: pwptr    ! 5. Total precipitable water
  type(spd_ob_type),   pointer  :: spdptr   ! 6. Wind speed
  type(rw_ob_type),    pointer  :: rwptr    ! 7. radar winds
  type(srw_ob_type),   pointer  :: srwptr   ! 8. Superobs radar wind
  type(dw_ob_type),    pointer  :: dwptr    ! 9. Lidare winds
  type(tcp_ob_type),   pointer  :: tcpptr   ! 10. Tropical bogus cental pressure 
  type(sst_ob_type),   pointer  :: sstptr   ! 11. Sea surface temperature
  type(oz_ob_type),    pointer  :: ozptr    ! 12. Ozone
  type(o3l_ob_type),   pointer  :: o3lptr   ! 12. Ozone
  type(gps_ob_type),   pointer  :: gpsptr   ! 13. GPS 
  type(colvk_ob_type), pointer  :: colvkptr ! 14. CO 
  type(pm2_5_ob_type), pointer  :: pm2_5ptr ! 15. pm2_5  
  type(rad_ob_type),   pointer  :: radptr   ! 16. Radiance 
  type(vis_ob_type),   pointer  :: visptr   ! 17. Conventional visibility
  type(pblh_ob_type),  pointer  :: pblhptr  ! 18. Conventional pbl height
  type(gust_ob_type),  pointer  :: gustptr  ! 19. Conventional wind gust 
  type(wspd10m_ob_type),pointer :: wspd10mptr ! 20. Conventional wind speed
  type(td2m_ob_type),  pointer :: td2mptr   ! 21. Conventional 2m dew point
  type(mxtm_ob_type),  pointer :: mxtmptr   ! 22. Conventional maxT
  type(mitm_ob_type),  pointer :: mitmptr   ! 23. Conventional minT
  type(pmsl_ob_type),  pointer :: pmslptr   ! 24. Conventional pressure at MSL
  type(howv_ob_type),  pointer :: howvptr   ! 25. Conventional significant wave height
  type(tcamt_ob_type), pointer :: tcamtptr  ! 26. Conventional total cloud amount
  type(lcbas_ob_type), pointer :: lcbasptr  ! 27. Conventional lowest cloud base
  type(cldch_ob_type), pointer :: cldchptr  ! 28. Conventional cloud ceiling height
  type(pm10_ob_type),  pointer  :: pm10ptr  ! 29. pm10
  type(aero_ob_type),  pointer  :: aeroptr  ! 30. aero aod


! ----------------------------------------------------------------------
  integer (i_kind) :: nob, nobs
  integer (i_kind) :: k
!
!--------------------------------------------------------------------------
! Do t obs
  nob = 0
!--------------------------------------------------------------------------
  tptr => yobs%t   
  do while (associated(tptr))

     if (tptr%luse) then
        lhs = lhs + tptr%diags%tldepart(jiter) * tptr%diags%tldepart(jiter)
        nob = nob + 1
     end if
     tptr => tptr%llpoint
  end do
  call mpi_allreduce(nob,nobs,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)

!--------------------------------------------------------------------------
! Do q obs
  nob = 0
!--------------------------------------------------------------------------
  qptr => yobs%q   
  do while (associated(qptr))

     if (qptr%luse) then
        lhs = lhs + qptr%diags%tldepart(jiter) * qptr%diags%tldepart(jiter)
        nob = nob + 1
     end if
     qptr => qptr%llpoint
  end do
  call mpi_allreduce(nob,nobs,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)

!--------------------------------------------------------------------------
! Do w obs
  nob = 0
!--------------------------------------------------------------------------
  wptr => yobs%w   
  do while (associated(wptr))

     if (wptr%luse) then
        lhs = lhs + wptr%diagu%tldepart(jiter) * wptr%diagu%tldepart(jiter)
        lhs = lhs + wptr%diagv%tldepart(jiter) * wptr%diagv%tldepart(jiter)
        nob = nob + 1
     end if
     wptr => wptr%llpoint
  end do
  call mpi_allreduce(nob,nobs,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)

!--------------------------------------------------------------------------
! Do ps obs
  nob = 0
!--------------------------------------------------------------------------
  psptr => yobs%ps  
  do while (associated(psptr))

     if (psptr%luse) then
        lhs = lhs + psptr%diags%tldepart(jiter) * psptr%diags%tldepart(jiter)
        nob = nob + 1
     end if
     psptr => psptr%llpoint
  end do
  call mpi_allreduce(nob,nobs,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)
!--------------------------------------------------------------------------
! Do pw obs
  nob = 0
!--------------------------------------------------------------------------
  pwptr => yobs%pw  
  do while (associated(pwptr))

     if (pwptr%luse) then
        lhs = lhs + pwptr%diags%tldepart(jiter) * pwptr%diags%tldepart(jiter)
        nob = nob + 1
     end if
     pwptr => pwptr%llpoint
  end do
  call mpi_allreduce(nob,nobs,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)
!--------------------------------------------------------------------------
! Do spd obs
  nob = 0
!--------------------------------------------------------------------------
  spdptr => yobs%spd 
  do while (associated(spdptr))

     if (spdptr%luse) then
        lhs = lhs + spdptr%diags%tldepart(jiter) * spdptr%diags%tldepart(jiter)
        nob = nob + 1
     end if
     spdptr => spdptr%llpoint
  end do
  call mpi_allreduce(nob,nobs,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)

!--------------------------------------------------------------------------
! Do radar wind (rw) obs
  nob = 0
!-------------------------------------------------------------------------
  rwptr => yobs%rw
  do while (associated(rwptr))

     if (rwptr%luse) then
        lhs = lhs + rwptr%diags%tldepart(jiter) * rwptr%diags%tldepart(jiter)
        nob = nob + 1
     end if
     rwptr => rwptr%llpoint
  end do
  call mpi_allreduce(nob,nobs,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)

!--------------------------------------------------------------------------
! Do doppler wind (dw) obs
  nob = 0
!-------------------------------------------------------------------------
  dwptr => yobs%dw
  do while (associated(dwptr))

     if (dwptr%luse) then
        lhs = lhs + dwptr%diags%tldepart(jiter) * dwptr%diags%tldepart(jiter)
        nob = nob + 1
     end if
     dwptr => dwptr%llpoint
  end do
  call mpi_allreduce(nob,nobs,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)
!  if(nobs > 0 .and. mype ==0) write(6,*)' for jiter= ',jiter,' got dw-obs ',nobs

!--------------------------------------------------------------------------
! Do superobs radar wind (srw) obs
  nob = 0
!-------------------------------------------------------------------------
  srwptr => yobs%srw
  do while (associated(srwptr))

     if (srwptr%luse) then
        lhs = lhs + srwptr%diagu%tldepart(jiter) * srwptr%diagu%tldepart(jiter)
        lhs = lhs + srwptr%diagv%tldepart(jiter) * srwptr%diagv%tldepart(jiter)
        nob = nob + 1
     end if
     srwptr => srwptr%llpoint
  end do
  call mpi_allreduce(nob,nobs,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)
!  if(nobs > 0 .and. mype ==0) write(6,*)' for jiter= ',jiter,' got srw-obs ',nobs
!--------------------------------------------------------------------------
! Do storm central pressure (tcp) obs
  nob = 0
!-------------------------------------------------------------------------
  tcpptr => yobs%tcp
  do while (associated(tcpptr))

     if (tcpptr%luse) then
        lhs = lhs + tcpptr%diags%tldepart(jiter) * tcpptr%diags%tldepart(jiter)
        nob = nob + 1
     end if
     tcpptr => tcpptr%llpoint
  end do
  call mpi_allreduce(nob,nobs,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)

!--------------------------------------------------------------------------
! Do sst obs
  nob = 0
!-------------------------------------------------------------------------
  sstptr => yobs%sst
  do while (associated(sstptr))

     if (sstptr%luse) then
        lhs = lhs + sstptr%diags%tldepart(jiter) * sstptr%diags%tldepart(jiter)
        nob = nob + 1
     end if
     sstptr => sstptr%llpoint
  end do
  call mpi_allreduce(nob,nobs,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)

!--------------------------------------------------------------------------
! Do oz obs
  nob = 0
!-------------------------------------------------------------------------
  ozptr => yobs%oz
  do while (associated(ozptr))

     if (ozptr%luse) then
        do k = 1, ozptr%nloz
           lhs = lhs + ozptr%diags(k)%ptr%tldepart(jiter) * ozptr%diags(k)%ptr%tldepart(jiter)
           nob = nob + 1
        end do ! k-loop
     end if
     ozptr => ozptr%llpoint
   end do
  call mpi_allreduce(nob,nobs,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)
!  if(nobs > 0 .and. mype ==0) write(6,*)' for jiter= ',jiter,' got oz-obs ',nobs

!--------------------------------------------------------------------------
! Do o3l obs
  nob = 0
!-------------------------------------------------------------------------
  o3lptr => yobs%o3l
  do while (associated(o3lptr))

     if (o3lptr%luse) then
        lhs = lhs + o3lptr%diags%tldepart(jiter) * o3lptr%diags%tldepart(jiter)
        nob = nob + 1
     end if
     o3lptr => o3lptr%llpoint
  end do
  call mpi_allreduce(nob,nobs,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)
!  if(nobs > 0 .and. mype ==0) write(6,*)' for jiter= ',jiter,' got o3l-obs ',nobs

!--------------------------------------------------------------------------
! Do gps obs
  nob = 0
!-------------------------------------------------------------------------
  gpsptr => yobs%gps
  do while (associated(gpsptr))

     if (gpsptr%luse) then
        lhs = lhs + gpsptr%diags%tldepart(jiter) * gpsptr%diags%tldepart(jiter)
        nob = nob + 1
     end if
     gpsptr => gpsptr%llpoint
  end do
  call mpi_allreduce(nob,nobs,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)
!  if(nobs > 0 .and. mype ==0) write(6,*)' for jiter= ',jiter,' got gps-obs ',nobs

!--------------------------------------------------------------------------
! Do co obs
  nob = 0
!-------------------------------------------------------------------------
  colvkptr => yobs%colvk
  do while (associated(colvkptr))

     if (colvkptr%luse) then
        do k = 1, colvkptr%nlco
           lhs = lhs + colvkptr%diags(k)%ptr%tldepart(jiter) * colvkptr%diags(k)%ptr%tldepart(jiter)
           nob = nob + 1
        end do ! k-loop
     end if
     colvkptr => colvkptr%llpoint
  end do
  call mpi_allreduce(nob,nobs,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)
!  if(nobs > 0 .and. mype ==0) write(6,*)' for jiter= ',jiter,' got co-obs ',nobs

!--------------------------------------------------------------------------
! Do pm2_5 obs
  nob = 0
!-------------------------------------------------------------------------
  pm2_5ptr => yobs%pm2_5
  do while (associated(pm2_5ptr))

     if (pm2_5ptr%luse) then
        lhs = lhs + pm2_5ptr%diags%tldepart(jiter) * pm2_5ptr%diags%tldepart(jiter)
        nob = nob + 1
     end if
     pm2_5ptr => pm2_5ptr%llpoint
  end do
  call mpi_allreduce(nob,nobs,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)
!  if(nobs > 0 .and. mype ==0) write(6,*)' for jiter= ',jiter,' got pm2_5-obs ',nobs

!--------------------------------------------------------------------------
! Do rad obs
  nob = 0
!-------------------------------------------------------------------------
  radptr => yobs%rad
  do while (associated(radptr))

     if (radptr%luse) then
        do k = 1, radptr%nchan
           lhs = lhs + radptr%diags(k)%ptr%tldepart(jiter) * radptr%diags(k)%ptr%tldepart(jiter)
           nob = nob + 1
        end do ! k-loop
     end if
     radptr => radptr%llpoint
  end do
  call mpi_allreduce(nob,nobs,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)

!--------------------------------------------------------------------------
! Do visibility obs
  nob = 0
!-------------------------------------------------------------------------
  visptr => yobs%vis
  do while (associated(visptr))

     if (visptr%luse) then
        lhs = lhs + visptr%diags%tldepart(jiter) * visptr%diags%tldepart(jiter)
        nob = nob + 1
     end if
     visptr => visptr%llpoint
  end do
  call mpi_allreduce(nob,nobs,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)

!--------------------------------------------------------------------------
! Do pbl height obs
  nob = 0
!-------------------------------------------------------------------------
  pblhptr => yobs%pblh
  do while (associated(pblhptr))

     if (pblhptr%luse) then
        lhs = lhs + pblhptr%diags%tldepart(jiter) * pblhptr%diags%tldepart(jiter)
        nob = nob + 1
     end if
     pblhptr => pblhptr%llpoint
  end do
  call mpi_allreduce(nob,nobs,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)

!--------------------------------------------------------------------------
! Do wind gust  obs
  nob = 0
!-------------------------------------------------------------------------
  gustptr => yobs%gust
  do while (associated(gustptr))

     if (gustptr%luse) then
        lhs = lhs + gustptr%diags%tldepart(jiter) * gustptr%diags%tldepart(jiter)
        nob = nob + 1
     end if
     gustptr => gustptr%llpoint
  end do
  call mpi_allreduce(nob,nobs,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)

!--------------------------------------------------------------------------
! Do wspd10m obs
  nob = 0
!-------------------------------------------------------------------------
  wspd10mptr => yobs%wspd10m
  do while (associated(wspd10mptr))
     if (wspd10mptr%luse) then
        lhs = lhs + wspd10mptr%diags%tldepart(jiter) * wspd10mptr%diags%tldepart(jiter)
        nob = nob + 1
     end if
     wspd10mptr => wspd10mptr%llpoint
  end do
  call mpi_allreduce(nob,nobs,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)
!--------------------------------------------------------------------------
! Do pm10 obs
  nob = 0
!-------------------------------------------------------------------------
  pm10ptr => yobs%pm10
  do while (associated(pm10ptr))

     if (pm10ptr%luse) then
        lhs = lhs + pm10ptr%diags%tldepart(jiter) * pm10ptr%diags%tldepart(jiter)
        nob = nob + 1
     end if
     pm10ptr => pm10ptr%llpoint
  end do
  call mpi_allreduce(nob,nobs,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)

!--------------------------------------------------------------------------
! Do aero aod obs
  nob = 0
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
  aeroptr => yobs%aero
  do while (associated(aeroptr))

     if (aeroptr%luse) then
        DO k = 1,aeroptr%nlaero
           lhs = lhs + aeroptr%diags(k)%ptr%tldepart(jiter) * aeroptr%diags(k)%ptr%tldepart(jiter)
           nob = nob + 1
        enddo
     end if
     aeroptr => aeroptr%llpoint
  end do
  call mpi_allreduce(nob,nobs,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)
!--------------------------------------------------------------------------
! Do td2m obs
  nob = 0
!-------------------------------------------------------------------------
  td2mptr => yobs%td2m
  do while (associated(td2mptr))

     if (td2mptr%luse) then
        lhs = lhs + td2mptr%diags%tldepart(jiter) * td2mptr%diags%tldepart(jiter)
        nob = nob + 1
     end if
     td2mptr => td2mptr%llpoint
  end do
  call mpi_allreduce(nob,nobs,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)

!--------------------------------------------------------------------------
! Do mxtm obs
  nob = 0
!-------------------------------------------------------------------------
  mxtmptr => yobs%mxtm
  do while (associated(mxtmptr))

     if (mxtmptr%luse) then
        lhs = lhs + mxtmptr%diags%tldepart(jiter) * mxtmptr%diags%tldepart(jiter)
        nob = nob + 1
     end if
     mxtmptr => mxtmptr%llpoint
  end do
  call mpi_allreduce(nob,nobs,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)

!--------------------------------------------------------------------------
! Do mitm obs
  nob = 0
!-------------------------------------------------------------------------
  mitmptr => yobs%mitm
  do while (associated(mitmptr))

     if (mitmptr%luse) then
        lhs = lhs + mitmptr%diags%tldepart(jiter) * mitmptr%diags%tldepart(jiter)
        nob = nob + 1
     end if
     mitmptr => mitmptr%llpoint
  end do
  call mpi_allreduce(nob,nobs,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)

!--------------------------------------------------------------------------
! Do pmsl obs
  nob = 0
!-------------------------------------------------------------------------
  pmslptr => yobs%pmsl
  do while (associated(pmslptr))

     if (pmslptr%luse) then
        lhs = lhs + pmslptr%diags%tldepart(jiter) * pmslptr%diags%tldepart(jiter)
        nob = nob + 1
     end if
     pmslptr => pmslptr%llpoint
  end do
  call mpi_allreduce(nob,nobs,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)

!--------------------------------------------------------------------------
! Do howv obs
  nob = 0
!-------------------------------------------------------------------------
  howvptr => yobs%howv
  do while (associated(howvptr))

     if (howvptr%luse) then
        lhs = lhs + howvptr%diags%tldepart(jiter) * howvptr%diags%tldepart(jiter)
        nob = nob + 1
     end if
     howvptr => howvptr%llpoint
  end do
  call mpi_allreduce(nob,nobs,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)

!--------------------------------------------------------------------------
! Do tcamt obs
  nob = 0
!-------------------------------------------------------------------------
  tcamtptr => yobs%tcamt
  do while (associated(tcamtptr))

     if (tcamtptr%luse) then
        lhs = lhs + tcamtptr%diags%tldepart(jiter) * tcamtptr%diags%tldepart(jiter)
        nob = nob + 1
     end if
     tcamtptr => tcamtptr%llpoint
  end do
  call mpi_allreduce(nob,nobs,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)

!--------------------------------------------------------------------------
! Do lcbas obs
  nob = 0
!-------------------------------------------------------------------------
  lcbasptr => yobs%lcbas
  do while (associated(lcbasptr))

     if (lcbasptr%luse) then
        lhs = lhs + lcbasptr%diags%tldepart(jiter) * lcbasptr%diags%tldepart(jiter)
        nob = nob + 1
     end if
     lcbasptr => lcbasptr%llpoint
  end do
  call mpi_allreduce(nob,nobs,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)

!--------------------------------------------------------------------------
! Do cldch obs
  nob = 0
!-------------------------------------------------------------------------
  cldchptr => yobs%cldch
  do while (associated(cldchptr))

     if (cldchptr%luse) then
        lhs = lhs + cldchptr%diags%tldepart(jiter) * cldchptr%diags%tldepart(jiter)
        nob = nob + 1
     end if
     cldchptr => cldchptr%llpoint
  end do
  call mpi_allreduce(nob,nobs,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)

!--------------------------------------------------------------------------
  return
end subroutine get_lhs     

! ----------------------------------------------------------------------
end module adjtest_obs
