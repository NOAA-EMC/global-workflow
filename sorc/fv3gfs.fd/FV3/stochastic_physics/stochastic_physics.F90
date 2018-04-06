
subroutine init_stochastic_physics(Model,Init_parm)
use fv_mp_mod, only : is_master
use stochy_internal_state_mod
use stochy_data_mod, only : nshum,rpattern_shum,init_stochdata,rpattern_sppt,nsppt,rpattern_skeb,nskeb,gg_lats,gg_lons,&
                            rad2deg,INTTYP,wlon,rnlat,gis_stochy,vfact_skeb,vfact_sppt,vfact_shum,skeb_vpts,skeb_vwts,sl
use get_stochy_pattern_mod,only : get_random_pattern_fv3_vect
use stochy_resol_def , only : latg,lonf,skeblevs
use stochy_gg_def,only : colrad_a
use stochy_namelist_def
use physcons, only: con_pi
use spectral_layout,only:me
use mpp_mod
use MPI
use GFS_typedefs,       only: GFS_control_type, GFS_init_type

implicit none
type(GFS_control_type),   intent(inout) :: Model
type(GFS_init_type),      intent(in) :: Init_parm
real*8 :: PRSI(Model%levs),PRSL(Model%levs),dx
real, allocatable :: skeb_vloc(:)
integer :: k,kflip,latghf,nodes,blk,k2
character*2::proc

! replace
rad2deg=180.0/con_pi
INTTYP=0 ! bilinear interpolation
me=mpp_pe()
nodes=mpp_npes()
gis_stochy%me=me
gis_stochy%nodes=nodes
call init_stochdata(Model%levs,Model%dtp,Model%input_nml_file,Model%fn_nml,Init_parm%nlunit)
! check to see decomposition
Model%do_sppt=do_sppt
Model%use_zmtnblck=use_zmtnblck
Model%do_shum=do_shum
Model%do_skeb=do_skeb
Model%skeb_npass=skeb_npass
if (do_sppt.EQ. .false. .AND. do_shum.EQ. .false..AND.do_skeb.EQ..false.) return
allocate(sl(Model%levs))
do k=1,Model%levs
   sl(k)= 0.5*(Init_parm%ak(k)/101300.+Init_parm%bk(k)+Init_parm%ak(k+1)/101300.0+Init_parm%bk(k+1)) ! si are now sigmas
!   if(is_master())print*,'sl(k)',k,sl(k),Init_parm%ak(k),Init_parm%bk(k)
enddo
if (do_sppt) then
   allocate(vfact_sppt(Model%levs))
   do k=1,Model%levs
      if (sl(k) .lt. sppt_sigtop1 .and. sl(k) .gt. sppt_sigtop2) then
         vfact_sppt(k) = (sl(k)-sppt_sigtop2)/(sppt_sigtop1-sppt_sigtop2)
      else if (sl(k) .lt. sppt_sigtop2) then
          vfact_sppt(k) = 0.0
      else
          vfact_sppt(k) = 1.0
      endif
   enddo
   if (sppt_sfclimit) then
       vfact_sppt(2)=vfact_sppt(3)*0.5
       vfact_sppt(1)=0.0
   endif
   if (is_master()) then
      do k=1,MOdel%levs
         print *,'sppt vert profile',k,sl(k),vfact_sppt(k)
      enddo
   endif
endif
if (do_skeb) then
   !print*,'allocating skeb stuff',skeblevs
   allocate(vfact_skeb(Model%levs))
   allocate(skeb_vloc(skeblevs)) ! local
   allocate(skeb_vwts(Model%levs,2)) ! save for later
   allocate(skeb_vpts(Model%levs,2)) ! save for later
   do k=1,Model%levs
      if (sl(k) .lt. skeb_sigtop1 .and. sl(k) .gt. skeb_sigtop2) then
         vfact_skeb(k) = (sl(k)-skeb_sigtop2)/(skeb_sigtop1-skeb_sigtop2)
      else if (sl(k) .lt. skeb_sigtop2) then
          vfact_skeb(k) = 0.0
      else
          vfact_skeb(k) = 1.0
      endif
      if (is_master())  print *,'skeb vert profile',k,sl(k),vfact_skeb(k) 
   enddo
! calculate vertical interpolation weights
   do k=1,skeblevs
      skeb_vloc(k)=sl(1)-real(k-1)/real(skeblevs-1.0)*(sl(1)-sl(Model%levs))
   enddo
! surface
skeb_vwts(1,2)=0
skeb_vpts(1,1)=1
! top
skeb_vwts(Model%levs,2)=1
skeb_vpts(Model%levs,1)=skeblevs-2
! internal
DO k=2,Model%levs-1
   DO k2=1,skeblevs-1
      IF (sl(k) .LE. skeb_vloc(k2) .AND. sl(k) .GT. skeb_vloc(k2+1)) THEN
        skeb_vpts(k,1)=k2
        skeb_vwts(k,2)=(skeb_vloc(k2)-sl(k))/(skeb_vloc(k2)-skeb_vloc(k2+1))
      ENDIF
   ENDDO 
ENDDO  
deallocate(skeb_vloc)
if (is_master()) then
DO k=1,Model%levs
   print*,'skeb vpts ',skeb_vpts(k,1),skeb_vwts(k,2)
ENDDO
endif
skeb_vwts(:,1)=1.0-skeb_vwts(:,2)
skeb_vpts(:,2)=skeb_vpts(:,1)+1.0
endif

if (do_shum) then
   allocate(vfact_shum(Model%levs))
   do k=1,Model%levs
      vfact_shum(k) = exp((sl(k)-1.)/shum_sigefold)
      if (sl(k).LT. 2*shum_sigefold) then
         vfact_shum(k)=0.0
      endif
      if (is_master())  print *,'shum vert profile',k,sl(k),vfact_shum(k)
   enddo
endif
! get interpolation weights
! define gaussian grid lats and lons
latghf=latg/2
!print *,'define interp weights',latghf,lonf
!print *,allocated(gg_lats),allocated(gg_lons)
allocate(gg_lats(latg))
!print *,'aloocated lats'
allocate(gg_lons(lonf))
!print *,'aloocated lons'
do k=1,latghf
   gg_lats(k)=-1.0*colrad_a(latghf-k+1)*rad2deg
   gg_lats(latg-k+1)=-1*gg_lats(k)
enddo
dx=360.0/lonf
!print*,'dx=',dx
do k=1,lonf
  gg_lons(k)=dx*(k-1)
enddo
WLON=gg_lons(1)-(gg_lons(2)-gg_lons(1))
RNLAT=gg_lats(1)*2-gg_lats(2)

!print *,'done with init_stochastic_physics'

end subroutine init_stochastic_physics

subroutine run_stochastic_physics(nblks,Model,Grid,Coupling)
use stochy_internal_state_mod
use stochy_data_mod, only : nshum,rpattern_shum,rpattern_sppt,nsppt,rpattern_skeb,nskeb,gg_lats,gg_lons,&
                            rad2deg,INTTYP,wlon,rnlat,gis_stochy,vfact_sppt,vfact_shum,vfact_skeb
use get_stochy_pattern_mod,only : get_random_pattern_fv3,get_random_pattern_fv3_vect,dump_patterns
use stochy_resol_def , only : latg,lonf
use stochy_namelist_def
use spectral_layout,only:me
use mpp_mod
use MPI
use GFS_typedefs,       only: GFS_control_type, GFS_grid_type,GFS_Coupling_type
implicit none
integer,intent(in) :: nblks
type(GFS_control_type),   intent(in) :: Model
type(GFS_grid_type),      intent(in) :: Grid(nblks)
type(GFS_coupling_type),  intent(inout) :: Coupling(nblks)
         
real,allocatable :: tmp_wts(:,:),tmpu_wts(:,:,:),tmpv_wts(:,:,:)
!D-grid
integer :: k
integer j,ierr,i
integer :: blk
character*120 :: sfile
character*6   :: STRFH
if (do_sppt.EQ. .false. .AND. do_shum.EQ. .false. .and. do_skeb.EQ. .false.) return
! check to see if it is time to write out random patterns
if (Model%phour .EQ. fhstoch) then
   write(STRFH,FMT='(I6.6)') nint(Model%fhour)
   sfile='stoch_out.F'//trim(STRFH)
   call dump_patterns(sfile)
endif
allocate(tmp_wts(nblks,Model%isc:Model%isc+Model%nx-1))
allocate(tmpu_wts(nblks,Model%isc:Model%isc+Model%nx-1,Model%levs))
allocate(tmpv_wts(nblks,Model%isc:Model%isc+Model%nx-1,Model%levs))
if (do_sppt) then
   call get_random_pattern_fv3(rpattern_sppt,nsppt,gis_stochy,Model,Grid,nblks,tmp_wts)
   DO blk=1,nblks
      DO k=1,Model%levs
         Coupling(blk)%sppt_wts(:,k)=tmp_wts(blk,:)*vfact_sppt(k)
      ENDDO
      if (sppt_logit) Coupling(blk)%sppt_wts(:,:) = (2./(1.+exp(Coupling(blk)%sppt_wts(:,:))))-1.
       Coupling(blk)%sppt_wts(:,:)= Coupling(blk)%sppt_wts(:,:)+1.0
   ENDDO
endif
if (do_shum) then
   call get_random_pattern_fv3(rpattern_shum,nshum,gis_stochy,Model,Grid,nblks,tmp_wts)
   DO blk=1,nblks
      DO k=1,Model%levs
         Coupling(blk)%shum_wts(:,k)=tmp_wts(blk,:)*vfact_shum(k)
      ENDDO
   ENDDO
endif
if (do_skeb) then
   call get_random_pattern_fv3_vect(rpattern_skeb,nskeb,gis_stochy,Model,Grid,nblks,tmpu_wts,tmpv_wts)
   DO blk=1,nblks
         DO k=1,Model%levs
         Coupling(blk)%skebu_wts(:,k)=tmpu_wts(blk,:,k)*vfact_skeb(k)
         Coupling(blk)%skebv_wts(:,k)=tmpv_wts(blk,:,k)*vfact_skeb(k)
      ENDDO
   ENDDO
endif
deallocate(tmp_wts)
deallocate(tmpu_wts)
deallocate(tmpv_wts)

end subroutine run_stochastic_physics
