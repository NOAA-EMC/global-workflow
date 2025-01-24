subroutine compute_qvar3d

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    compute_qvar3d
!   prgmmr: zhu               org: np22                date: 2010-03-15
!
! abstract: compute rhgues and qvar3d
!
! program history log:
! 2010-03-15 zhu - extracted out from compute_derived
! 2010-04-10 parrish - make rhgues local, since removed from jfunc by derber (no longer used)
! 2010-05-28 todling - obtain variable id's on the fly (add getindex)
! 2011-08-17 zhu  - add handling of dssv(:,:,:,nrf3_cw) for regional when total condensate is control variable 
! 2011-11-01 eliu - add qmin 
! 2012-02-08 kleist  - add computation of ges_qsat over nfldsig bins
! 2013-10-19 todling - metguess now holds background
! 2013-10-25 todling - reposition ltosi and others to commvars
! 2013-10-30 jung - check and clip supersaturation
! 2012-12-15 zhu  - add two cwoption options for both global and regional
! 2014-06-15 zhu  - new background error variance of cw in the regional applications 
!                   for all-sky radiance assimilation (cwoption3)
! 2015-01-04 zhu  - apply the background error variance of cw cwoption3 to the global
! 2015-09-10 zhu  - use centralized cloud_names_fwd and n_clouds_fwd in the assignments of cloud 
!                   variances (either cw or individual hydrometerors) for all-sky radiance assimilation
!                 - remove cwoption1
!
!   input argument list:
!
!   output argument list:
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_single
  use berror, only: dssv
  use jfunc, only: varq,qoption,varcw,cwoption,clip_supersaturation,superfact
  use derivsmod, only: qsatg,qgues
  use control_vectors, only: cvars3d
  use gridmod, only: lat2,lon2,nsig
  use constants, only: zero,one,fv,r100,qmin
  use guess_grids, only: fact_tv,ntguessig,nfldsig,ges_tsen,ges_prsl,ges_qsat
  use mpeu_util, only: getindex
  use gsi_metguess_mod,  only: gsi_metguess_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use general_sub2grid_mod, only: general_sub2grid,general_grid2sub
  use radiance_mod, only: icloud_cv,n_clouds_fwd,cloud_names_fwd
  use obsmod, only: l_wcp_cwm

  implicit none

! Declare local variables
  logical ice
  integer(i_kind) :: i,j,k,it,n,np,iderivative,nrf3_q,nrf3_cw
  integer(i_kind) :: ic,nrf3_var
  real(r_kind) d,dn1,dn2
  real(r_kind),allocatable,dimension(:,:,:):: rhgues

  integer(i_kind):: istatus,ier,ier6
  real(r_kind):: cwtmp
  real(r_kind),pointer,dimension(:,:,:):: ges_var=>NULL()
  real(r_kind),pointer,dimension(:,:,:):: ges_ql=>NULL()
  real(r_kind),pointer,dimension(:,:,:):: ges_qi=>NULL()
  real(r_kind),pointer,dimension(:,:,:):: ges_qr=>NULL()
  real(r_kind),pointer,dimension(:,:,:):: ges_qs=>NULL()
  real(r_kind),pointer,dimension(:,:,:):: ges_qg=>NULL()
  real(r_kind),pointer,dimension(:,:,:):: ges_qh=>NULL()
  real(r_kind),pointer,dimension(:,:,:):: ges_q =>NULL()
  integer(i_kind):: maxvarq1


  nrf3_q=getindex(cvars3d,'q')
  nrf3_cw=getindex(cvars3d,'cw')

! Calculate qsat independently of presence of q in guess
  iderivative = 0
  ice=.true.
  do it=1,nfldsig
     call genqsat(ges_qsat(1,1,1,it),ges_tsen(1,1,1,it),ges_prsl(1,1,1,it),lat2,lon2, &
                  nsig,ice,iderivative)
  enddo

! If q in guess, check/fix q limits
  ier=0
  do it=1,nfldsig
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'q',ges_q, ier)
     if(ier/=0) exit
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
! Limit q to be >= qmin
              ges_q(i,j,k)=max(ges_q(i,j,k),qmin)
! Limit q to be <= ges_qsat
              if(clip_supersaturation) ges_q(i,j,k)=min(ges_q(i,j,k),superfact*ges_qsat(i,j,k,it))
           end do
        end do
     end do
  end do
  if(ier/=0) return

  call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'q',ges_q, ier)

! Load guess q.  Initialize saturation array to guess.
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           qgues(i,j,k)=ges_q(i,j,k) ! q guess
           qsatg(i,j,k)=ges_q(i,j,k) ! q guess
           fact_tv(i,j,k)=one/(one+fv*qsatg(i,j,k))      ! factor for tv to tsen conversion
        end do
     end do
  end do

! Compute saturation specific humidity.  Set up normalization factor
! for limq routines (1/qs*2)
  if(qoption == 1)then
      iderivative = 1
  else
      iderivative = 2
  end if
  ice=.true.
  call genqsat(qsatg,ges_tsen(1,1,1,ntguessig),ges_prsl(1,1,1,ntguessig),lat2,lon2,nsig,ice,iderivative)

  allocate(rhgues(lat2,lon2,nsig))
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           rhgues(i,j,k)=qgues(i,j,k)/qsatg(i,j,k)
        end do
     end do
  end do

  if (qoption==2) then
     maxvarq1=min(size(varq,1),25)
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              d=20.0_r_kind*rhgues(i,j,k) + one
              n=int(d)
              np=n+1
              dn2=d-real(n,r_kind)
              dn1=one-dn2
              n=min0(max(1,n),maxvarq1)
              np=min0(max(1,np),maxvarq1)
              dssv(i,j,k,nrf3_q)=(varq(n,k)*dn1 + varq(np,k)*dn2)*dssv(i,j,k,nrf3_q)
           end do
        end do
     end do
  end if

  deallocate(rhgues)

  if (.not. icloud_cv) return
  if (nrf3_cw>0) then 

     call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'ql',ges_ql,istatus);ier=istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'qi',ges_qi,istatus);ier=ier+istatus
     if (ier/=0) return
     call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'qr',ges_qr,istatus);ier6=istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'qs',ges_qs,istatus);ier6=ier6+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'qg',ges_qg,istatus);ier6=ier6+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'qh',ges_qh,istatus);ier6=ier6+istatus
     if (l_wcp_cwm .and. ier6/=0) return

     if (cwoption==3) then
        do k = 1,nsig
           do j = 1,lon2
              do i = 1,lat2
                 if (ges_prsl(i,j,k,ntguessig)<15.0_r_kind) then
                    dssv(i,j,k,nrf3_cw)=zero
                 else
                    cwtmp=ges_ql(i,j,k)+ges_qi(i,j,k)
                    if (l_wcp_cwm .and. ier6==0) then
                       cwtmp=cwtmp &
                           +ges_qr(i,j,k)+ges_qs(i,j,k) &
                           +ges_qg(i,j,k)+ges_qh(i,j,k)
                    endif
                    if (cwtmp<1.0e-10_r_kind) cwtmp=1.0e-10_r_kind
                    dn1=0.05_r_kind*cwtmp
                    dssv(i,j,k,nrf3_cw)=dn1*dssv(i,j,k,nrf3_cw)
                 end if
              end do
           end do
        end do
     end if
     if (cwoption==2) then
        do k = 1,nsig
           do j = 1,lon2
              do i = 1,lat2
                 cwtmp=ges_ql(i,j,k)+ges_qi(i,j,k)
                 if (l_wcp_cwm .and. ier6==0) then
                    cwtmp=cwtmp &
                        +ges_qr(i,j,k)+ges_qs(i,j,k) &
                        +ges_qg(i,j,k)+ges_qh(i,j,k)
                 endif
                 if (cwtmp<1.0e-10_r_kind) cwtmp=1.0e-10_r_kind
                 d=-2.0_r_kind*log(cwtmp) + one
                 n=int(d)
                 np=n+1
                 dn2=d-real(n,r_kind)
                 dn1=one-dn2
                 n=min0(max(1,n),30)
                 np=min0(max(1,np),30)
                 dssv(i,j,k,nrf3_cw)=(varcw(n,k)*dn1 + varcw(np,k)*dn2)*dssv(i,j,k,nrf3_cw)
              end do
           end do
        end do
     end if ! end of cw

! for individual hydrometeors
  else 
     if (cwoption/=3) return
     do n=1,size(cvars3d)
        do ic=1,n_clouds_fwd
           if(trim(cvars3d(n))==trim(cloud_names_fwd(ic))) then
              nrf3_var=n
              call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig), &
                             trim(cloud_names_fwd(ic)),ges_var,istatus)
              if (istatus/=0) return

              do k = 1,nsig
                 do j = 1,lon2
                    do i = 1,lat2
                       if (ges_prsl(i,j,k,ntguessig)<15.0_r_kind) then
                          dssv(i,j,k,nrf3_var)=zero
                       else
                          cwtmp=ges_var(i,j,k)
                          if (ges_var(i,j,k)<1.0e-10_r_kind) cwtmp=1.0e-10_r_kind
                          dn1=0.05_r_kind*cwtmp
                          dssv(i,j,k,nrf3_var)=dn1*dssv(i,j,k,nrf3_var)
                       end if
                    end do
                 end do
              end do

           end if
        end do 
     end do
  end if ! end of nrf3_cw

end subroutine compute_qvar3d

