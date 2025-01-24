module ens_spread_mod
implicit none
public
contains
subroutine ens_spread_dualres_regional(en_bar)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ens_spread_dualres_regional
!   prgmmr: mizzi            org: ncar/mmm            date: 2010-08-11
!
! abstract:
!
!
! program history log:
!   2010-08-11  parrish, initial documentation
!   2011-04-05  parrish - add pseudo-bundle capability
!   2011-08-31  todling - revisit en_perts (single-prec) in light of extended bundle
!
!   input argument list:
!     en_bar - ensemble mean
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
!
  use kinds, only: r_single,r_kind,i_kind
  use hybrid_ensemble_parameters, only: n_ens,grd_ens,grd_anl,p_e2a,uv_hyb_ens, &
                                        regional_ensemble_option
  use hybrid_ensemble_parameters, only: en_perts,nelen
  use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info,general_sube2suba
  use constants, only:  zero,two,half,one
  use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
  use gsi_bundlemod, only: gsi_bundlecreate
  use gsi_bundlemod, only: gsi_grid
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_bundlemod, only: gsi_bundledestroy
  use gsi_bundlemod, only: gsi_gridcreate
  implicit none

  type(gsi_bundle),OPTIONAL,intent(in):: en_bar

  type(gsi_bundle):: sube,suba
  type(gsi_grid):: grid_ens,grid_anl
  real(r_kind) sp_norm,sig_norm_sq_inv
  type(sub2grid_info)::se,sa
  integer(i_kind) k

  integer(i_kind) i,n,ic3
  logical regional
  integer(i_kind) num_fields,inner_vars,istat,istatus
  logical,allocatable::vector(:)
  real(r_kind),pointer,dimension(:,:,:):: st,vp,tv,rh,oz,cw
  real(r_kind),pointer,dimension(:,:):: ps
  real(r_kind),dimension(grd_anl%lat2,grd_anl%lon2,grd_anl%nsig),target::dum3
  real(r_kind),dimension(grd_anl%lat2,grd_anl%lon2),target::dum2

!      create simple regular grid
        call gsi_gridcreate(grid_anl,grd_anl%lat2,grd_anl%lon2,grd_anl%nsig)
        call gsi_gridcreate(grid_ens,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig)

!      create two internal bundles, one on analysis grid and one on ensemble grid

       call gsi_bundlecreate (suba,grid_anl,'ensemble work',istatus, &
                                 names2d=cvars2d,names3d=cvars3d,bundle_kind=r_kind)
       if(istatus/=0) then
          write(6,*)' in ens_spread_dualres_regional: trouble creating bundle_anl bundle'
          call stop2(999)
       endif
       call gsi_bundlecreate (sube,grid_ens,'ensemble work ens',istatus, &
                                 names2d=cvars2d,names3d=cvars3d,bundle_kind=r_kind)
       if(istatus/=0) then
          write(6,*)' ens_spread_dualres_regional: trouble creating bundle_ens bundle'
          call stop2(999)
       endif

  sp_norm=(one/real(n_ens,r_kind))

  sube%values=zero
!

  if(regional_ensemble_option == 1)then
     print *,'global ensemble'
     sig_norm_sq_inv=n_ens-one

     do n=1,n_ens
        do i=1,nelen
           sube%values(i)=sube%values(i) &
             +en_perts(n,1,1)%valuesr4(i)*en_perts(n,1,1)%valuesr4(i)
        end do
     end do

     do i=1,nelen
       sube%values(i) = sqrt(sp_norm*sig_norm_sq_inv*sube%values(i))
     end do
  else
     do n=1,n_ens
        do i=1,nelen
           sube%values(i)=sube%values(i) &
             +(en_perts(n,1,1)%valuesr4(i)-en_bar%values(i))*(en_perts(n,1,1)%valuesr4(i)-en_bar%values(i))
        end do
     end do
 
     do i=1,nelen
       sube%values(i) = sqrt(sp_norm*sube%values(i))
     end do
  end if

  if(grd_ens%latlon1n == grd_anl%latlon1n) then
     do i=1,nelen
        suba%values(i)=sube%values(i)
     end do
  else
     inner_vars=1
     num_fields=max(0,nc3d)*grd_ens%nsig+max(0,nc2d)
     allocate(vector(num_fields))
     vector=.false.
     do ic3=1,nc3d
        if(trim(cvars3d(ic3))=='sf'.or.trim(cvars3d(ic3))=='vp') then
           do k=1,grd_ens%nsig
              vector((ic3-1)*grd_ens%nsig+k)=uv_hyb_ens
           end do
        end if
     end do
     call general_sub2grid_create_info(se,inner_vars,grd_ens%nlat,grd_ens%nlon,grd_ens%nsig,num_fields, &
                                       regional,vector)
     call general_sub2grid_create_info(sa,inner_vars,grd_anl%nlat,grd_anl%nlon,grd_anl%nsig,num_fields, &
                                       regional,vector)
     deallocate(vector)
     call general_sube2suba(se,sa,p_e2a,sube%values,suba%values,regional)
  end if

  dum2=zero
  dum3=zero
  call gsi_bundlegetpointer(suba,'sf',st,istat)
  if(istat/=0) then
     write(6,*)' no sf pointer in ens_spread_dualres, point st at dum3 array'
     st => dum3
  end if
  call gsi_bundlegetpointer(suba,'vp',vp,istat)
  if(istat/=0) then
     write(6,*)' no vp pointer in ens_spread_dualres, point vp at dum3 array'
     vp => dum3
  end if
  call gsi_bundlegetpointer(suba,'t',tv,istat)
  if(istat/=0) then
     write(6,*)' no t pointer in ens_spread_dualres, point tv at dum3 array'
     tv => dum3
  end if
  call gsi_bundlegetpointer(suba,'q',rh,istat)
  if(istat/=0) then
     write(6,*)' no q pointer in ens_spread_dualres, point rh at dum3 array'
     rh => dum3
  end if
  call gsi_bundlegetpointer(suba,'oz',oz,istat)
  if(istat/=0) then
     write(6,*)' no oz pointer in ens_spread_dualres, point oz at dum3 array'
     oz => dum3
  end if
  call gsi_bundlegetpointer(suba,'cw',cw,istat)
  if(istat/=0) then
     write(6,*)' no cw pointer in ens_spread_dualres, point cw at dum3 array'
     cw => dum3
  end if
  call gsi_bundlegetpointer(suba,'ps',ps,istat)
  if(istat/=0) then
     write(6,*)' no ps pointer in ens_spread_dualres, point ps at dum2 array'
     ps => dum2
  end if

  call write_spread_dualres(st,vp,tv,rh,oz,cw,ps)

  return
end subroutine ens_spread_dualres_regional

end module ens_spread_mod
