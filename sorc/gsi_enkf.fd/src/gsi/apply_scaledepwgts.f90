!$$$  program documentation block
! 
!  program history:
!
!  2018-03-28    T. Lei and D. Kleist - consoliated and added codes 
!                                       for the scale dependent scale localization scheme
!
!$$$ end documentation block

function fwgtofwvlen (rvlft,rvrgt,rcons,rlen,rinput)
!$$$  subprogram documentation block
! 
! subprogram: fwgtofwvlen
!  
! abstract: Calculation of spectral filter functions
!
!$$$ end documentation block

    use kinds, only: r_kind
    implicit none

    real(r_kind),intent(in)  :: rvlft,rvrgt,rcons,rlen,rinput
    real(r_kind)  :: fwgtofwvlen
    real(r_kind)  :: rlen1,rtem1,rconshalf

    if(rinput > rvlft .and. rinput < rvrgt) then
       fwgtofwvlen=rcons
    else
       rlen1=rlen/10.0_r_kind ! rlen corresponds to a (-5,5) region
       rconshalf=0.5_r_kind*rcons
       rtem1=min(abs(rinput-rvlft),abs(rinput-rvrgt))
       fwgtofwvlen=rconshalf*(1.0_r_kind+tanh(5.0_r_kind-rtem1/rlen1))
    endif
       
end function fwgtofwvlen
!                .      .    .                                       .
subroutine init_mult_spc_wgts(jcap_in)
!$$$  subprogram documentation block
! 
! subprogram: init_mult_spc_wgts
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use constants, only: zero,half,one,rearth,pi,tiny_r_kind
  use mpimod, only: mype
  use hybrid_ensemble_parameters, only: nsclgrp
  use hybrid_ensemble_parameters, only: spc_multwgt,spcwgt_params,r_ensloccov4scl
  implicit none

  integer(i_kind),intent(in   ) :: jcap_in

  integer(i_kind) i,l,ks
  integer(i_kind) ig
  real(r_kind) :: rwv0,rtem1,rtem2
  real(r_kind) :: fwgtofwvlen
  real(r_kind) :: totwvlength
  real(r_kind),dimension(0:jcap_in,nsclgrp) :: spcwgt
  logical :: l_sum_spc_weights

  ! Spectral scale decomposition is differernt between SDL-cross and SDL-nocross
  if( r_ensloccov4scl < tiny_r_kind )then
     l_sum_spc_weights = .false.
  else
     l_sum_spc_weights = .true.
  end if

  spcwgt(0,1)=one
  do ig=2,nsclgrp
    spcwgt(0,ig)=zero
  end do


  rwv0=2.0_r_kind*pi*rearth*0.001_r_kind
  do i=1,jcap_in
     totwvlength= rwv0/real(i)                   
     rtem1=zero
     do ig=1,nsclgrp
        if(ig /= 2) then
           spcwgt(i,ig)=fwgtofwvlen(spcwgt_params(1,ig),spcwgt_params(2,ig),&
                                    spcwgt_params(3,ig),spcwgt_params(4,ig),totwvlength)
           spcwgt(i,ig)=min(max(spcwgt(i,ig),zero),one)
           if(l_sum_spc_weights) then
              rtem1=rtem1+spcwgt(i,ig)
           else
              rtem1=rtem1+spcwgt(i,ig)*spcwgt(i,ig)
           endif
        endif
     enddo
     rtem2 =1.0_r_kind - rtem1
     if(rtem2 >= zero) then 
 
        if(l_sum_spc_weights) then
           spcwgt(i,2)=rtem2 
        else
           spcwgt(i,2)=sqrt(rtem2)
        endif
     else
        if(mype == 0)write(6,*) ' rtem2 < zero ',i,rtem2,(spcwgt(i,ig),ig=1,nsclgrp)
        spcwgt(i,2)=zero
     endif
  enddo
!! Code borrowed from spvar in splib

  spc_multwgt = zero
  do ig=1,nsclgrp
    do i=0,jcap_in
       ks=2*i
       spc_multwgt(ks+1,ig)=spcwgt(i,ig)
    end do
    do i=0,jcap_in
       do l=MAX(1,i-jcap_in),MIN(i,jcap_in)
          ks=l*(2*jcap_in+1-l)+2*i
          spc_multwgt(ks+1,ig) = spcwgt(i,ig)
          spc_multwgt(ks+2,ig) = spcwgt(i,ig)
       end do
    end do
  end do

  
  return
end subroutine init_mult_spc_wgts
subroutine destroy_mult_spc_wgts
!$$$  subprogram documentation block
!
! subprogram: destroy_mult_spc_wgts
!
!$$$ end documentation block

  use hybrid_ensemble_parameters, only: spc_multwgt,spcwgt_params
  implicit none

  deallocate(spc_multwgt,spcwgt_params)

  return
end subroutine destroy_mult_spc_wgts


subroutine apply_scaledepwgts(m,grd_in,sp_in)  
!
!  Program history log:
!   2017-03-30  J. Kay, X. Wang - copied from Kleist's apply_scaledepwgts and
!                                 add the calculation of scale-dependent weighting for mixed resolution ensemble  
!                                 POC: xuguang.wang@ou.edu
!
  use constants, only:  one
  use control_vectors, only: control_vector
  use kinds, only: r_kind,i_kind,r_single
  use gsi_bundlemod, only: gsi_bundle
  use general_sub2grid_mod, only: general_sub2grid,general_grid2sub   
  use general_specmod, only: spec_vars
  use general_sub2grid_mod, only: sub2grid_info
  use hybrid_ensemble_parameters, only: spc_multwgt,en_perts,nsclgrp,n_ens
  implicit none

! Declare passed variables
  integer,intent(in) :: m
  type(spec_vars),intent (in):: sp_in
  type(sub2grid_info),intent(in)::grd_in

! Declare local variables
  integer(i_kind) kk,ig,n,ig2,i,j

  real(r_single),dimension(grd_in%nlat,grd_in%nlon,grd_in%nlevs_alloc,nsclgrp) :: hwork2
  real(r_kind),dimension(grd_in%nlat,grd_in%nlon)      :: work
  real(r_kind),dimension(sp_in%nc,grd_in%nlevs_alloc):: spc1
  real(r_kind),dimension(sp_in%nc):: spc2
 
  do n=1,n_ens
!    Get from subdomains to full grid
     call general_sub2grid(grd_in,en_perts(n,1,m)%valuesr4(:),hwork2(:,:,:,1))

!$omp parallel do schedule(static,1) private(i,j,kk,work)
     do kk=1,grd_in%nlevs_loc
        do j=1,grd_in%nlon
           do i=1,grd_in%nlat
              work(i,j)=hwork2(i,j,kk,1)
           end do
        end do
!    Transform from physical space to spectral space   
        call general_g2s0(grd_in,sp_in,spc1(1,kk),work)

     end do
!$omp parallel do schedule(static,1) private(kk,ig,ig2,i,j,work,spc2)
     do ig2=1,nsclgrp*grd_in%nlevs_loc
        ig=(ig2-1)/grd_in%nlevs_loc+1
        kk=ig2-(ig-1)*grd_in%nlevs_loc

        do i=1,sp_in%nc
           spc2(i)=spc1(i,kk)*spc_multwgt(i,ig)
        end do
!    Apply spectral weights
!    Transform back to physical space
        call general_s2g0(grd_in,sp_in,spc2,work)

        do j=1,grd_in%nlon
           do i=1,grd_in%nlat
              hwork2(i,j,kk,ig)=work(i,j)
           end do
        end do
     end do
     do ig=1,nsclgrp

!    Transfer work back to subdomains
        call general_grid2sub(grd_in,hwork2(:,:,:,ig),en_perts(n,ig,m)%valuesr4(:))    
     end do
  end do

  return
end subroutine apply_scaledepwgts
