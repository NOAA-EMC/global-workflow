module get_stochy_pattern_mod
 use stochy_resol_def
 use spectral_layout
 use stochy_namelist_def
 use stochy_data_mod
 use stochy_gg_def
 use stochy_patterngenerator
 use stochy_internal_state_mod
 use fv_mp_mod, only : mp_reduce_sum,is_master
 use fv_arrays_mod,      only: fv_atmos_type
 use GFS_typedefs,       only: GFS_control_type, GFS_grid_type
 use mersenne_twister, only: random_seed
 implicit none
 private

 public  get_random_pattern_fv3,get_random_pattern_fv3_vect
 public dump_patterns
 logical :: first_call=.true.
#include "mpif.h"
 contains

subroutine get_random_pattern_fv3(rpattern,npatterns,&
           gis_stochy,Model,Grid,nblks,pattern_2d)

! generate a random pattern for stochastic physics
 implicit none
 type(random_pattern), intent(inout) :: rpattern(npatterns)
 type(stochy_internal_state), target :: gis_stochy
 type(GFS_control_type),   intent(in) :: Model
 type(GFS_grid_type),      intent(in) :: Grid(nblks)
 integer,intent(in)::   npatterns,nblks

 integer i,j,l,lat,ierr,n,nn,k,nt
 real(kind=kind_dbl_prec), dimension(lonf,gis_stochy%lats_node_a,1):: wrk2d
 
 integer :: num2d
! logical lprint

 real(kind=kind_dbl_prec), allocatable, dimension(:,:) :: workg
 real (kind=kind_dbl_prec)   glolal(lonf,gis_stochy%lats_node_a)
 integer kmsk0(lonf,gis_stochy%lats_node_a),len
 real(kind=kind_dbl_prec) :: globalvar,globalvar0
 real(kind=kind_dbl_prec) :: pattern_2d(nblks,Model%isc:Model%isc+Model%nx-1)
 real(kind=kind_dbl_prec) :: pattern_1d(Model%isc:Model%isc+Model%nx)
 real(kind=kind_dbl_prec), allocatable, dimension(:,:) :: rslmsk
 real(kind=kind_dbl_prec), allocatable, dimension(:) :: slmask,tlats,tlons
 integer :: inttyp,blk
 
 character*2 proc
 kmsk0 = 0
 glolal = 0.
 do n=1,npatterns
    call patterngenerator_advance(rpattern(n),1)
    call scalarspect_to_gaugrid(                       &
         rpattern(n)%spec_e,rpattern(n)%spec_o,wrk2d,&
         gis_stochy%ls_node,gis_stochy%ls_nodes,gis_stochy%max_ls_nodes,&
         gis_stochy%lats_nodes_a,gis_stochy%global_lats_a,gis_stochy%lonsperlat,&
         gis_stochy%plnev_a,gis_stochy%plnod_a,1)
    glolal = glolal + wrk2d(:,:,1)
 enddo

 allocate(workg(lonf,latg))
 workg = 0.
  do j=1,gis_stochy%lats_node_a
     lat=gis_stochy%global_lats_a(ipt_lats_node_a-1+j)
     do i=1,lonf
        workg(i,lat) = glolal(i,j)
     enddo
  enddo
 
   call mp_reduce_sum(workg,lonf,latg)

! interpolate to cube grid
    
   allocate(rslmsk(lonf,latg))
   do blk=1,nblks
      len=size(Grid(blk)%xlat,1)
      allocate(SLMASK(len))
      allocate(tlats(len))
      allocate(tlons(len))
      tlats=Grid(blk)%xlat*rad2deg
      tlons=Grid(blk)%xlon*rad2deg
      call la2ga(workg,lonf,latg,gg_lons,gg_lats,wlon,rnlat,inttyp,&
                 pattern_1d,len,.false.,rslmsk,slmask,&
                 tlats,tlons,me)
      pattern_2d(blk,:)=pattern_1d(:)
      deallocate(SLMASK)
      deallocate(tlats)
      deallocate(tlons)
   enddo
   deallocate(rslmsk)
   deallocate(workg)
 
   
end subroutine get_random_pattern_fv3 

subroutine get_random_pattern_fv3_vect(rpattern,npatterns,&
           gis_stochy,Model,Grid,nblks,upattern_3d,vpattern_3d)

! generate a random pattern for stochastic physics
 implicit none
 type(GFS_control_type),   intent(in) :: Model
 type(GFS_grid_type),      intent(in) :: Grid(nblks)
 type(stochy_internal_state), target :: gis_stochy
 type(random_pattern), intent(inout) :: rpattern(npatterns)

 real(kind=kind_evod), dimension(len_trie_ls,2,1) ::  vrtspec_e,divspec_e
 real(kind=kind_evod), dimension(len_trio_ls,2,1) ::  vrtspec_o,divspec_o
 integer::   npatterns,nblks,blk,len

 real(kind=kind_dbl_prec) :: upattern_3d(nblks,Model%isc:Model%isc+Model%nx-1,levs)
 real(kind=kind_dbl_prec) :: vpattern_3d(nblks,Model%isc:Model%isc+Model%nx-1,levs)
 real(kind=kind_dbl_prec) :: pattern_1d(Model%isc:Model%isc+Model%nx)
 real(kind=kind_dbl_prec), allocatable, dimension(:,:) :: rslmsk
 real(kind=kind_dbl_prec), allocatable, dimension(:) :: slmask,tlats,tlons
 integer i,j,l,lat,ierr,n,nn,k,nt
 real(kind_dbl_prec), dimension(lonf,gis_stochy%lats_node_a,1):: wrk2du,wrk2dv

 integer :: num2d
! logical lprint

 real, allocatable, dimension(:,:) :: workgu,workgv
integer kmsk0(lonf,gis_stochy%lats_node_a),i1,i2,j1
 real(kind=kind_dbl_prec) :: globalvar,globalvar0
 kmsk0 = 0
 allocate(workgu(lonf,latg))
 allocate(workgv(lonf,latg))
 allocate(rslmsk(lonf,latg))
 if (first_call) then
    allocate(skebu_save(nblks,Model%isc:Model%isc+Model%nx-1,skeblevs))
    allocate(skebv_save(nblks,Model%isc:Model%isc+Model%nx-1,skeblevs))
    do k=1,skeblevs-1
       workgu = 0.
       workgv = 0.
       do n=1,npatterns
          call patterngenerator_advance(rpattern(n),1)
      !   ke norm (convert streamfunction forcing to vorticity forcing)
          divspec_e = 0; divspec_o = 0.
          do nn=1,2
             vrtspec_e(:,nn,1) = gis_stochy%kenorm_e*rpattern(n)%spec_e(:,nn,1)
             vrtspec_o(:,nn,1) = gis_stochy%kenorm_o*rpattern(n)%spec_o(:,nn,1)
          enddo
        ! convert to winds
          call vrtdivspect_to_uvgrid(&
                 divspec_e,divspec_o,vrtspec_e,vrtspec_o,&
                 wrk2du,wrk2dv,&
                 gis_stochy%ls_node,gis_stochy%ls_nodes,gis_stochy%max_ls_nodes,&
                 gis_stochy%lats_nodes_a,gis_stochy%global_lats_a,gis_stochy%lonsperlat,&
                 gis_stochy%epsedn,gis_stochy%epsodn,gis_stochy%snnp1ev,gis_stochy%snnp1od,&
                 gis_stochy%plnev_a,gis_stochy%plnod_a,1)
       do i=1,lonf
          do j=1,gis_stochy%lats_node_a
             lat=gis_stochy%global_lats_a(ipt_lats_node_a-1+j)
             workgu(i,lat) = workgu(i,lat) + wrk2du(i,j,1)
             workgv(i,lat) = workgv(i,lat) + wrk2dv(i,j,1)
          enddo
       enddo
    enddo
    call mp_reduce_sum(workgu,lonf,latg)
    call mp_reduce_sum(workgv,lonf,latg)
! interpolate to cube grid
    do blk=1,nblks
       len=size(Grid(blk)%xlat,1)
       allocate(SLMASK(len))
       allocate(tlats(len))
       allocate(tlons(len))
       tlats=Grid(blk)%xlat*rad2deg
       tlons=Grid(blk)%xlon*rad2deg
       call la2ga(workgu,lonf,latg,gg_lons,gg_lats,wlon,rnlat,inttyp,&
                  pattern_1d,len,.false.,rslmsk,slmask,&
                  tlats,tlons,me)
       skebu_save(blk,:,k)=pattern_1d(:)
       call la2ga(workgv,lonf,latg,gg_lons,gg_lats,wlon,rnlat,inttyp,&
                  pattern_1d,len,.false.,rslmsk,slmask,&
                  tlats,tlons,me)
       skebv_save(blk,:,k)=-1*pattern_1d(:)
       deallocate(SLMASK)
       deallocate(tlats)
       deallocate(tlons)
    enddo
  enddo
    first_call=.false.
 else
! shift patterns
   do k=1,skeblevs-1
      skebu_save(:,:,k)=skebu_save(:,:,k+1)
      skebv_save(:,:,k)=skebv_save(:,:,k+1)
   enddo
 endif
 
! get pattern for last level 
 workgu = 0.
 workgv = 0.
 do n=1,npatterns
    call patterngenerator_advance(rpattern(n),1)
!   ke norm (convert streamfunction forcing to vorticity forcing)
    divspec_e = 0; divspec_o = 0.
    do nn=1,2
       vrtspec_e(:,nn,1) = gis_stochy%kenorm_e*rpattern(n)%spec_e(:,nn,1)
       vrtspec_o(:,nn,1) = gis_stochy%kenorm_o*rpattern(n)%spec_o(:,nn,1)
    enddo
  ! convert to winds
    call vrtdivspect_to_uvgrid(&
           divspec_e,divspec_o,vrtspec_e,vrtspec_o,&
           wrk2du,wrk2dv,&
           gis_stochy%ls_node,gis_stochy%ls_nodes,gis_stochy%max_ls_nodes,&
           gis_stochy%lats_nodes_a,gis_stochy%global_lats_a,gis_stochy%lonsperlat,&
           gis_stochy%epsedn,gis_stochy%epsodn,gis_stochy%snnp1ev,gis_stochy%snnp1od,&
           gis_stochy%plnev_a,gis_stochy%plnod_a,1)
    do i=1,lonf
       do j=1,gis_stochy%lats_node_a
          lat=gis_stochy%global_lats_a(ipt_lats_node_a-1+j)
          workgu(i,lat) = workgu(i,lat) + wrk2du(i,j,1)
          workgv(i,lat) = workgv(i,lat) + wrk2dv(i,j,1)
       enddo
    enddo
 enddo
 call mp_reduce_sum(workgu,lonf,latg)
 call mp_reduce_sum(workgv,lonf,latg)
! interpolate to cube grid
 do blk=1,nblks
    len=size(Grid(blk)%xlat,1)
    allocate(SLMASK(len))
    allocate(tlats(len))
    allocate(tlons(len))
    tlats=Grid(blk)%xlat*rad2deg
    tlons=Grid(blk)%xlon*rad2deg
    call la2ga(workgu,lonf,latg,gg_lons,gg_lats,wlon,rnlat,inttyp,&
               pattern_1d,len,.false.,rslmsk,slmask,&
               tlats,tlons,me)
    skebu_save(blk,:,k)=pattern_1d(:)
    call la2ga(workgv,lonf,latg,gg_lons,gg_lats,wlon,rnlat,inttyp,&
               pattern_1d,len,.false.,rslmsk,slmask,&
               tlats,tlons,me)
    skebv_save(blk,:,k)=-1*pattern_1d(:)
    deallocate(SLMASK)
    deallocate(tlats)
    deallocate(tlons)
  enddo
  deallocate(rslmsk)
  deallocate(workgu)
  deallocate(workgv)
! interpolate in the vertical  ! consider moving to cubed sphere side,  more memory, but less interpolations
 do k=1,Model%levs
    do blk=1,nblks
       upattern_3d(blk,:,k) = skeb_vwts(k,1)*skebu_save(blk,:,skeb_vpts(k,1))+skeb_vwts(k,2)*skebu_save(blk,:,skeb_vpts(k,2))
       vpattern_3d(blk,:,k) = skeb_vwts(k,1)*skebv_save(blk,:,skeb_vpts(k,1))+skeb_vwts(k,2)*skebv_save(blk,:,skeb_vpts(k,2))
    enddo
 enddo
 
end subroutine get_random_pattern_fv3_vect

subroutine scalarspect_to_gaugrid(&
           trie_ls,trio_ls,datag,&
           ls_node,ls_nodes,max_ls_nodes,&
           lats_nodes_a,global_lats_a,lonsperlat,&
           plnev_a,plnod_a,nlevs)


      implicit none
      real(kind=kind_dbl_prec), intent(in) :: trie_ls(len_trie_ls,2,nlevs)
      real(kind=kind_dbl_prec), intent(in) :: trio_ls(len_trio_ls,2,nlevs)
      real(kind=kind_dbl_prec),  intent(out) :: datag(lonf,lats_node_a,nlevs)
      integer, intent(in) :: ls_node(ls_dim,3),ls_nodes(ls_dim,nodes),&
        nlevs,max_ls_nodes(nodes),lats_nodes_a(nodes),global_lats_a(latg),lonsperlat(latg)
      real(kind=kind_dbl_prec),intent(in) :: plnev_a(len_trie_ls,latg2),plnod_a(len_trio_ls,latg2)
! local vars
      real(kind=kind_dbl_prec) for_gr_a_1(lon_dim_a,nlevs,lats_dim_a)
      real(kind=kind_dbl_prec) for_gr_a_2(lonf,nlevs,lats_dim_a)
      integer              i,j,k
      integer              l,lan,lat
      integer              lons_lat

      call sumfln_stochy(trie_ls,&
                  trio_ls,&
                  lat1s_a,&
                  plnev_a,plnod_a,&
                  nlevs,ls_node,latg2,&
                  lats_dim_a,nlevs,for_gr_a_1,&
                  ls_nodes,max_ls_nodes,&
                  lats_nodes_a,global_lats_a,&
                  lats_node_a,ipt_lats_node_a,&
                  lonsperlat,lon_dim_a,latg,0)

      do lan=1,lats_node_a
         lat = global_lats_a(ipt_lats_node_a-1+lan)
         lons_lat = lonsperlat(lat)
         CALL FOUR_TO_GRID(for_gr_a_1(1,1,lan),for_gr_a_2(1,1,lan),&
                           lon_dim_a,lonf,lons_lat,nlevs)
      enddo  

      datag = 0.
      do lan=1,lats_node_a
        lat      = global_lats_a(ipt_lats_node_a-1+lan)
        lons_lat = lonsperlat(lat)
        do k=1,nlevs
          do i=1,lons_lat
            datag(i,lan,k) = for_gr_a_2(i,k,lan)
          enddo
        enddo
      enddo

      return
      end subroutine scalarspect_to_gaugrid

subroutine dump_patterns(sfile)
    implicit none
    character*120 :: sfile
    integer :: stochlun,k,n
    stochlun=99
    if (is_master()) then
       if (nsppt > 0 .OR. nshum > 0 .OR. nskeb > 0) then
          OPEN(stochlun,file=sfile,form='unformatted')
          print*,'open ',sfile,' for output'
       endif
    endif
    if (nsppt > 0) then
       do n=1,nsppt 
       call write_pattern(rpattern_sppt(n),1,stochlun)
       enddo
    endif
    if (nshum > 0) then
       do n=1,nshum
       call write_pattern(rpattern_shum(n),1,stochlun)
       enddo
    endif
    if (nskeb > 0) then
       do n=1,nskeb
       do k=1,skeblevs
          call write_pattern(rpattern_skeb(n),k,stochlun)
       enddo
       enddo
    endif
    close(stochlun)
 end subroutine dump_patterns
 subroutine write_pattern(rpattern,lev,lunptn)
   implicit none
   type(random_pattern), intent(inout) :: rpattern
   integer, intent(in) :: lunptn,lev
   real(kind_dbl_prec), allocatable  :: pattern2d(:)
   integer nm,nn,ierr,arrlen,isize
   integer,allocatable :: isave(:)
   arrlen=2*ndimspec

   allocate(pattern2d(arrlen))
   pattern2d=0.0
   ! fill in apprpriate pieces of array
   !print*,'before collection...',me,maxval(rpattern%spec_e),maxval(rpattern%spec_o) &
   ! ,minval(rpattern%spec_e),minval(rpattern%spec_o)
   do nn=1,len_trie_ls
      nm = rpattern%idx_e(nn)
      if (nm == 0) cycle
      pattern2d(nm)          = rpattern%spec_e(nn,1,lev)
      pattern2d(ndimspec+nm) = rpattern%spec_e(nn,2,lev)
   enddo
   do nn=1,len_trio_ls
      nm = rpattern%idx_o(nn)
      if (nm == 0) cycle
      pattern2d(nm)          = rpattern%spec_o(nn,1,lev)
      pattern2d(ndimspec+nm) = rpattern%spec_o(nn,2,lev)
   enddo
   call mp_reduce_sum(pattern2d,arrlen)
  !  write only on root process
   if (is_master()) then
      print*,'writing out random pattern (min/max/size)',&
      minval(pattern2d),maxval(pattern2d),size(pattern2d)
      !print*,'max/min pattern=',maxval(pattern2d),minval(pattern2d)
      write(lunptn) ntrunc
      call random_seed(size=isize) ! get seed size
      allocate(isave(isize)) ! get seed
      call random_seed(get=isave,stat=rpattern%rstate) ! write seed
      write(lunptn) isave
      write(lunptn) pattern2d
   endif
   deallocate(pattern2d)
 end subroutine write_pattern
 subroutine vrtdivspect_to_uvgrid(&
           trie_di,trio_di,trie_ze,trio_ze,&
           uug,vvg,&
           ls_node,ls_nodes,max_ls_nodes,&
           lats_nodes_a,global_lats_a,lonsperlar,&
           epsedn,epsodn,snnp1ev,snnp1od,plnev_a,plnod_a,nlevs)

      implicit none
      real(kind=kind_dbl_prec), intent(in) :: trie_di(len_trie_ls,2,nlevs)
      real(kind=kind_dbl_prec), intent(in) :: trio_di(len_trio_ls,2,nlevs)
      real(kind=kind_dbl_prec), intent(in) :: trie_ze(len_trie_ls,2,nlevs)
      real(kind=kind_dbl_prec), intent(in) :: trio_ze(len_trio_ls,2,nlevs)
      real(kind=kind_dbl_prec),  intent(out) :: uug(lonf,lats_node_a,nlevs)
      real(kind=kind_dbl_prec),  intent(out) :: vvg(lonf,lats_node_a,nlevs)
      integer, intent(in) :: ls_node(ls_dim,3),ls_nodes(ls_dim,nodes),&
        nlevs,max_ls_nodes(nodes),lats_nodes_a(nodes),global_lats_a(latg),lonsperlar(latg)
      real(kind=kind_dbl_prec),intent(in) ::  epsedn(len_trie_ls),&
       epsodn(len_trio_ls),snnp1ev(len_trie_ls),snnp1od(len_trio_ls),&
       plnev_a(len_trie_ls,latg2),plnod_a(len_trio_ls,latg2)
! local vars
      real(kind=kind_dbl_prec) trie_ls(len_trie_ls,2,2*nlevs)
      real(kind=kind_dbl_prec) trio_ls(len_trio_ls,2,2*nlevs)
      real(kind=kind_dbl_prec) for_gr_a_1(lon_dim_a,2*nlevs,lats_dim_a)
      real(kind=kind_dbl_prec) for_gr_a_2(lonf,2*nlevs,lats_dim_a)
      integer              i,j,k
      integer              l,lan,lat
      integer              lons_lat
      real (kind=kind_dbl_prec) tx1

      do k=1,nlevs
        call dezouv_stochy(trie_di(1,1,k),       trio_ze(1,1,k),&
                    trie_ls(1,1,k), trio_ls(1,1,nlevs+k),&
                    epsedn,epsodn,snnp1ev,snnp1od,ls_node)
        call dozeuv_stochy(trio_di(1,1,k),       trie_ze(1,1,k),&
                    trio_ls(1,1,k), trie_ls(1,1,nlevs+k),&
                    epsedn,epsodn,snnp1ev,snnp1od,ls_node)
      enddo

      call sumfln_stochy(trie_ls,&
                  trio_ls,&
                  lat1s_a,&
                  plnev_a,plnod_a,&
                  2*nlevs,ls_node,latg2,&
                  lats_dim_a,2*nlevs,for_gr_a_1,&
                  ls_nodes,max_ls_nodes,&
                  lats_nodes_a,global_lats_a,&
                  lats_node_a,ipt_lats_node_a,&
                  lonsperlar,lon_dim_a,latg,0)

      do lan=1,lats_node_a
         lat = global_lats_a(ipt_lats_node_a-1+lan)
         lons_lat = lonsperlar(lat)
         CALL FOUR_TO_GRID(for_gr_a_1(1,1,lan),for_gr_a_2(1,1,lan),&
                           lon_dim_a,lonf,lons_lat,2*nlevs)
      enddo  

      uug = 0.; vvg = 0.
      do lan=1,lats_node_a
        lat      = global_lats_a(ipt_lats_node_a-1+lan)
        lons_lat = lonsperlar(lat)
        tx1      = 1. / coslat_a(lat)
        do k=1,nlevs
          do i=1,lons_lat
            uug(i,lan,k) = for_gr_a_2(i,k,lan) * tx1
            vvg(i,lan,k) = for_gr_a_2(i,nlevs+k,lan) * tx1
          enddo
        enddo
      enddo

      return
 end subroutine vrtdivspect_to_uvgrid
end module get_stochy_pattern_mod
