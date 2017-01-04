module module_stochastic_physics
use fv_mp_mod, only : is_master,commglobal
use fv_treat_da_inc_mod, only : remap_coef
use stochy_internal_state_mod
use fv_arrays_mod,only: fv_atmos_type
use stochy_data_mod, only : nshum,rpattern_shum,init_stochdata,rpattern_sppt,nsppt
use get_stochy_pattern_mod,only : get_random_pattern_fv3,s2c,id1,id2,jdc
use stochy_resol_def , only : latg,lonf
use stochy_gg_def,only : colrad_a
use stochy_namelist_def
use physcons, only: con_pi
use spectral_layout,only:me
use mpp_mod
use ESMF
use MPI

implicit none
private
public:: init_stochastic_physics,run_stochastic_physics
real(kind=ESMF_KIND_R8),allocatable :: vfact_shum(:),vfact_sppt(:)
type(stochy_internal_state) :: gis_stochy

contains

subroutine init_stochastic_physics(Atm,dt_atmos,iret)
type(fv_atmos_type), target :: Atm
real,intent(in) :: dt_atmos
integer,intent(out)      ::iret
real*8 :: PRSI(Atm%npz+1),PRSL(Atm%npz),sl(Atm%npz),rad2deg,dx
real,allocatable :: gg_lats(:),gg_lons(:)
integer :: k,kflip,latghf,nodes
character*2::proc
iret=0 

! replace
rad2deg=180.0/con_pi
me=mpp_pe()
nodes=mpp_npes()
gis_stochy%me=me
gis_stochy%nodes=nodes
print*,'calling init_stochdata',dt_atmos,is_master()
call init_stochdata(gis_stochy, Atm%npz,dt_atmos)
   do k=1,Atm%npz
      sl(k)= 0.5*(Atm%ak(k)/101300.+Atm%bk(k)+Atm%ak(k+1)/101300.0+Atm%bk(k+1)) ! si are now sigmas
      if(is_master())print*,'sl(k)',k,sl(k),Atm%ak(k),Atm%bk(k)
   enddo
if (do_sppt) then
   allocate(vfact_sppt(Atm%npz))
   do k=1,Atm%npz
      if (sl(k) .lt. sppt_sigtop1 .and. sl(k) .gt. sppt_sigtop2) then
         vfact_sppt(k) = (sl(k)-sppt_sigtop2)/(sppt_sigtop1-sppt_sigtop2)
      else if (sl(k) .lt. sppt_sigtop2) then
          vfact_sppt(k) = 0.0
      else
          vfact_sppt(k) = 1.0
      endif
      if (is_master())  print *,'sppt vert profile',k,sl(k),vfact_sppt(k)
   enddo
   if (sppt_sfclimit) then
       vfact_sppt(Atm%npz-1)=vfact_sppt(Atm%npz-1)*0.5
       vfact_sppt(Atm%npz)=vfact_sppt(Atm%npz-1)*0.5
   endif
endif
if (do_shum) then
   allocate(vfact_shum(Atm%npz))
   do k=1,Atm%npz
      vfact_shum(k) = exp((sl(k)-1.)/shum_sigefold)
      if (is_master())  print *,'shum vert profile',k,sl(k),vfact_shum(k)
   enddo
endif
allocate(s2c(Atm%bd%is:Atm%bd%ie,Atm%bd%js:Atm%bd%je,4))
allocate(id1(Atm%bd%is:Atm%bd%ie,Atm%bd%js:Atm%bd%je))
allocate(id2(Atm%bd%is:Atm%bd%ie,Atm%bd%js:Atm%bd%je))
allocate(jdc(Atm%bd%is:Atm%bd%ie,Atm%bd%js:Atm%bd%je))
	
! get interpolation weights
! define gaussian grid lats and lons
latghf=latg/2
allocate(gg_lats(latg),gg_lons(lonf))
do k=1,latghf
   gg_lats(k)=-1.0*colrad_a(latghf-k+1)!*rad2deg
   gg_lats(latg-k+1)=-1*gg_lats(k)
enddo
dx=360.0/lonf
do k=1,lonf
  gg_lons(k)=dx*(k-1)/rad2deg
enddo
!   write(proc,FMT='(I2.2)') mpp_pe()
!   open(74,file='grid_proc_'//proc)
!   write(74,*) 'calling remap_coef',Atm%bd%is,Atm%bd%ie,Atm%bd%js,Atm%bd%je
!   write(74,*) 'gg_dims',lonf,latg
!   write(74,*) 'gg_lats',gg_lats
!   write(74,*) 'gg_grid',gg_lons(1),gg_lons(lonf),gg_lats(1),gg_lats(latg)
!   write(74,*) 'fv_lons', Atm%gridstruct%agrid(Atm%bd%is,Atm%bd%js,1),  &
!                           Atm%gridstruct%agrid(Atm%bd%ie,Atm%bd%js,1),  &
!                           Atm%gridstruct%agrid(Atm%bd%is,Atm%bd%je,1),  &
!                           Atm%gridstruct%agrid(Atm%bd%ie,Atm%bd%je,1) 
!   write(74,*) 'fv_lats',Atm%gridstruct%agrid(Atm%bd%is,Atm%bd%js,2),  &
!                           Atm%gridstruct%agrid(Atm%bd%ie,Atm%bd%js,2),  &
!                           Atm%gridstruct%agrid(Atm%bd%is,Atm%bd%je,2),  &
!                           Atm%gridstruct%agrid(Atm%bd%ie,Atm%bd%je,2) 
!   close(74)
call remap_coef(Atm%bd%is,Atm%bd%ie,Atm%bd%js,Atm%bd%je,Atm%bd%isd,Atm%bd%ied,Atm%bd%jsd,Atm%bd%jed, &
        lonf, latg, gg_lons, gg_lats, id1, id2, jdc, s2c, &
        Atm%gridstruct%agrid)


end subroutine init_stochastic_physics

subroutine run_stochastic_physics(Atm)
         
type(fv_atmos_type), target :: Atm
real(ESMF_KIND_R8) :: tmp_rndm(Atm%bd%is:Atm%bd%ie,Atm%bd%js:Atm%bd%je)
integer :: k,null_arr(5)
integer j,ierr,i

null_arr(:)=0
if (do_sppt) then
   call get_random_pattern_fv3(rpattern_sppt,nsppt,&
      gis_stochy%ls_node,gis_stochy%ls_nodes,gis_stochy%max_ls_nodes,&
      gis_stochy%lats_nodes_a,gis_stochy%global_lats_a,gis_stochy%lonsperlat,&
      gis_stochy%plnev_a,gis_stochy%plnod_a,null_arr,Atm%bd%is,Atm%bd%ie,Atm%bd%js,Atm%bd%je,&
   tmp_rndm(:,:),Atm)
   if (sppt_logit) tmp_rndm(:,:) = (2./(1.+exp(tmp_rndm(:,:))))-1.
   do k = 1, Atm%npz
      Atm%sppt_wts(Atm%bd%is:Atm%bd%ie,Atm%bd%js:Atm%bd%je,k)=tmp_rndm(Atm%bd%is:Atm%bd%ie,Atm%bd%js:Atm%bd%je)*vfact_sppt(k)+1.0
   enddo
endif
if (do_shum) then
   call get_random_pattern_fv3(rpattern_shum,nshum,&
      gis_stochy%ls_node,gis_stochy%ls_nodes,gis_stochy%max_ls_nodes,&
      gis_stochy%lats_nodes_a,gis_stochy%global_lats_a,gis_stochy%lonsperlat,&
      gis_stochy%plnev_a,gis_stochy%plnod_a,null_arr,Atm%bd%is,Atm%bd%ie,Atm%bd%js,Atm%bd%je,&
      tmp_rndm(:,:),Atm)
   do k = 1, Atm%npz
      Atm%shum_wts(Atm%bd%is:Atm%bd%ie,Atm%bd%js:Atm%bd%je,k)=tmp_rndm(Atm%bd%is:Atm%bd%ie,Atm%bd%js:Atm%bd%je)*vfact_shum(k)
   enddo
!   print*,'shum_wts=',Atm%shum_wts(Atm%bd%is,Atm%bd%js,1),Atm%shum_wts(Atm%bd%is,Atm%bd%js,Atm%npz)
endif

end subroutine run_stochastic_physics

end module module_stochastic_physics
