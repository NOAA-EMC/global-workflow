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
  use jfunc, only: varq,qoption,varcw,cwoption,clip_supersaturation
  use derivsmod, only: qsatg,qgues
  use control_vectors, only: cvars3d
  use gridmod, only: lat2,lon2,nsig,lat1,lon1,nlat,nlon,regional
  use constants, only: zero,one,fv,r100,qmin
  use guess_grids, only: fact_tv,ntguessig,nfldsig,ges_tsen,ges_prsl,ges_qsat
  use mpeu_util, only: getindex
  use mpimod, only: mype
  use gsi_metguess_mod,  only: gsi_metguess_get,gsi_metguess_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use general_commvars_mod, only: g3
  use general_sub2grid_mod, only: general_sub2grid,general_grid2sub

  implicit none

! Declare local variables
  logical ice
  integer(i_kind) :: i,j,k,it,ii,n,np,iderivative,nrf3_q,nrf3_cw
  real(r_kind) d,dn1,dn2
  real(r_kind),allocatable,dimension(:,:,:):: rhgues

  integer(i_kind):: istatus,ier,n_actual_clouds
  integer(i_kind),dimension(nlat,nsig):: ntmp
  real(r_kind):: cwtmp
  real(r_kind),dimension(nlat,nsig):: work_cw
  real(r_kind),dimension(nlat,nsig):: cw_avg
  real(r_kind),dimension(lat2*lon2*nsig):: cw_tmp
  real(r_kind),dimension(g3%inner_vars,nlat,nlon,g3%kbegin_loc:g3%kend_alloc):: work
  real(r_kind),pointer,dimension(:,:,:):: ges_ql=>NULL()
  real(r_kind),pointer,dimension(:,:,:):: ges_qi=>NULL()
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
              if(clip_supersaturation) ges_q(i,j,k)=min(ges_q(i,j,k),ges_qsat(i,j,k,it))
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
              dn2=d-float(n)
              dn1=one-dn2
              n=min0(max(1,n),maxvarq1)
              np=min0(max(1,np),maxvarq1)
              dssv(i,j,k,nrf3_q)=(varq(n,k)*dn1 + varq(np,k)*dn2)*dssv(i,j,k,nrf3_q)
           end do
        end do
     end do
  end if

  deallocate(rhgues)

  if (nrf3_cw>0) then 
     call gsi_metguess_get('clouds_4crtm_jac::3d',n_actual_clouds,ier)
     if (n_actual_clouds<=0) return

     call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'ql',ges_ql,istatus);ier=istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'qi',ges_qi,istatus);ier=ier+istatus
     if (ier/=0) return

     if (cwoption==1) then 
!       compute mean at each vertical level for each latitude
        ii=0
        do k=1,nsig
           do j=1,lon2
              do i=1,lat2
                 ii=ii+1
                 cw_tmp(ii)=ges_ql(i,j,k)+ges_qi(i,j,k)
              end do
           end do
        end do
        call general_sub2grid(g3,cw_tmp,work)

        ntmp=0
        work_cw=zero
!       cw_avg =1.0e-12_r_kind
        cw_avg =zero
        do k=g3%kbegin_loc,g3%kend_alloc
           do i = 2,nlat-1
              do j=1,nlon
                 if (work(1,i,j,k)>1.0e-10_r_kind) then
                    work_cw(i,k)=work_cw(i,k)+work(1,i,j,k)
                    ntmp(i,k)=ntmp(i,k)+1
                 end if
              end do

              if (ntmp(i,k)>10) then
                 cw_avg(i,k)=max(work_cw(i,k)/float(ntmp(i,k)),1.0e-10_r_kind)
              end if
           end do
        end do

        do k=g3%kbegin_loc,g3%kend_alloc
           do i = 1,nlat
              do j=1,nlon
                 work(1,i,j,k)=0.2_r_kind*cw_avg(i,k)
              end do
           end do
        end do
        call general_grid2sub(g3,work,cw_tmp)

!       apply the coefficients to dssv of cw
        ii=0
        do k=1,nsig
           do j=1,lon2
              do i=1,lat2
                 ii=ii+1
                 dssv(i,j,k,nrf3_cw)=cw_tmp(ii)*dssv(i,j,k,nrf3_cw)
              end do
           end do
        end do
     end if ! end of cwoption==1

     if (cwoption==3) then
        do k = 1,nsig
           do j = 1,lon2
              do i = 1,lat2
                 if (ges_prsl(i,j,k,ntguessig)<15.0_r_kind) then
                    dssv(i,j,k,nrf3_cw)=zero
                 else
                    cwtmp=ges_ql(i,j,k)+ges_qi(i,j,k)
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
                 if (cwtmp<1.0e-10_r_kind) cwtmp=1.0e-10_r_kind
                 d=-2.0_r_kind*log(cwtmp) + one
                 n=int(d)
                 np=n+1
                 dn2=d-float(n)
                 dn1=one-dn2
                 n=min0(max(1,n),30)
                 np=min0(max(1,np),30)
                 dssv(i,j,k,nrf3_cw)=(varcw(n,k)*dn1 + varcw(np,k)*dn2)*dssv(i,j,k,nrf3_cw)
              end do
           end do
        end do
     end if ! end of cwoption==3

  end if ! end of nrf3_cw


end subroutine compute_qvar3d

