module get_stochy_pattern_mod
 use stochy_resol_def
 use spectral_layout
 use stochy_namelist_def
 use stochy_data_mod
 use stochy_gg_def
 use stochy_patterngenerator
 use fv_mp_mod
 use mpp_mod
!#use mpp_mod
 use fv_arrays_mod,      only: fv_atmos_type
 implicit none
 private

 public  get_random_pattern_fv3
 public dump_patterns,restore_patterns
 real,public,allocatable, dimension(:,:,:):: s2c
 integer,public,allocatable, dimension(:,:):: id1, id2, jdc
#include "mpif.h"
 contains

subroutine get_random_pattern_fv3(rpattern,npatterns,&
           ls_node,ls_nodes,max_ls_nodes,&
           lats_nodes_a,global_lats_a,lonsperlat,&
           plnev_a,plnod_a,vfilt,is,ie,js,je,&
           pattern_2d,Atm)
! generate a random pattern for stochastic physics
 implicit none
  type(random_pattern), intent(inout) :: rpattern(npatterns)

 integer, intent(in) :: ls_node(ls_dim,3),ls_nodes(ls_dim,nodes),&
   max_ls_nodes(nodes),lats_nodes_a(nodes),global_lats_a(latg),lonsperlat(latg),&
   npatterns,vfilt(5),is,ie,js,je
 real(kind=kind_dbl_prec),intent(in) :: &
    plnev_a(len_trie_ls,latg2),plnod_a(len_trio_ls,latg2)

 real(kind=kind_dbl_prec),intent(out) :: pattern_2d(is:ie,js:je)
 integer i,j,l,lat,ierr,n,nn,k,nt
 real(kind_dbl_prec), dimension(lonf,lats_node_a,1):: wrk2d
 real(kind_dbl_prec), dimension(lonf,lats_node_a):: wrk2db
 type(fv_atmos_type), target :: Atm
 integer :: num2d
! logical lprint

 real(kind_dbl_prec), allocatable, dimension(:,:) :: workg,workl
 real (kind=kind_dbl_prec)   glolal(lonf,lats_node_a)
 integer kmsk0(lonf,lats_node_a),i1,i2,j1
 real(kind=kind_dbl_prec) :: globalvar,globalvar0
 character*2 proc
 kmsk0 = 0
 wrk2db = 0.
 do n=1,npatterns
    !print*,'in get random patttern',minval(rpattern(n)%spec_e),maxval(rpattern(n)%spec_e)
    call patterngenerator_advance(rpattern(n),1)
    call scalarspect_to_gaugrid(&
         rpattern(n)%spec_e,rpattern(n)%spec_o,wrk2d,&
         ls_node,ls_nodes,max_ls_nodes,&
         lats_nodes_a,global_lats_a,lonsperlat,&
         plnev_a,plnod_a,1,Atm)
    wrk2db = wrk2db + wrk2d(:,:,1)
 enddo
 allocate(workl(lonf,latg))
 allocate(workg(lonf,latg))
 workl = 0.
 workg = 0.
 CALL uninterpred_stochy(2,kmsk0,glolal,wrk2db(:,:),& ! TBD  fix for a 3-d pattern
                 global_lats_a,lonsperlat)
  do j=1,lats_node_a
     lat=global_lats_a(ipt_lats_node_a-1+j)
     do i=1,lonf
        workl(i,lat) = glolal(i,j)
     enddo
  enddo
  call mpi_allreduce(workl,workg,lonf*latg,mpi_real8,mpi_sum,commglobal,ierr)
!  if (is_master())then
!   open(72,file='shum.gau',form='unformatted',access='append')
!   write(72) real(workg,kind=4)
!   close(72)
!  endif
! interpolation
   pattern_2d(:,:)=0.0
   do j=js,je
      do i=is,ie
         i1 = id1(i,j)
         i2 = id2(i,j)
         j1 = jdc(i,j)
         pattern_2d(i,j) = s2c(i,j,1)*workg(i1,j1  ) + s2c(i,j,2)*workg(i2,j1  )+&
                           s2c(i,j,3)*workg(i2,j1+1) + s2c(i,j,4)*workg(i1,j1+1)
      enddo
   enddo
   deallocate(workg)
   !print*,'after inter',maxval(pattern_2d),minval(pattern_2d)
   !print*,'after inter',mpp_pe(),is,ie,js,je
!   write(proc,FMT='(I2.2)') mpp_pe()
!   open(73,file='shum_proc_'//proc,form='unformatted',access='append')
!   write(73) real(pattern_2d,kind=4)
!   close(73)
   
end subroutine get_random_pattern_fv3 

subroutine scalarspect_to_gaugrid(&
           trie_ls,trio_ls,datag,&
           ls_node,ls_nodes,max_ls_nodes,&
           lats_nodes_a,global_lats_a,lonsperlat,&
           plnev_a,plnod_a,nlevs,Atm)


      implicit none
      real(kind=kind_dbl_prec), intent(in) :: trie_ls(len_trie_ls,2,nlevs)
      real(kind=kind_dbl_prec), intent(in) :: trio_ls(len_trio_ls,2,nlevs)
      real(kind=kind_dbl_prec),  intent(out) :: datag(lonf,lats_node_a,nlevs)
      integer, intent(in) :: ls_node(ls_dim,3),ls_nodes(ls_dim,nodes),&
        nlevs,max_ls_nodes(nodes),lats_nodes_a(nodes),global_lats_a(latg),lonsperlat(latg)
      real(kind=kind_dbl_prec),intent(in) :: plnev_a(len_trie_ls,latg2),plnod_a(len_trio_ls,latg2)
      type(fv_atmos_type), target :: Atm
! local vars
      real(kind=kind_dbl_prec) for_gr_a_1(lon_dim_a,nlevs,lats_dim_a)
      real(kind=kind_dbl_prec) for_gr_a_2(lonf,nlevs,lats_dim_a)
      integer              i,j,k
      integer              l,lan,lat
      integer              lons_lat
      !print*,'len_trie',maxval(trie_ls)
      call sumfln_stochy(trie_ls,&
                  trio_ls,&
                  lat1s_a,&
                  plnev_a,plnod_a,&
                  nlevs,ls_node,latg2,&
                  lats_dim_a,nlevs,for_gr_a_1,&
                  ls_nodes,max_ls_nodes,&
                  lats_nodes_a,global_lats_a,&
                  lats_node_a,ipt_lats_node_a,&
                  lonsperlat,lon_dim_a,latg,0,Atm)

      do lan=1,lats_node_a
         lat = global_lats_a(ipt_lats_node_a-1+lan)
         lons_lat = lonsperlat(lat)
         CALL FOUR_TO_GRID(for_gr_a_1(1,1,lan),for_gr_a_2(1,1,lan),&
                           lon_dim_a,lonf,lons_lat,nlevs)
      enddo  
      !print*,'for_gr ',maxval(for_gr_a_2)
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
      !print*,'datag',maxval(datag)

      return
      end subroutine scalarspect_to_gaugrid

subroutine dump_patterns(sfile)
    implicit none
    character*9 :: sfile
    integer :: stochlun,k,n
    stochlun=99
    if (is_master()) then
       if (nsppt > 0 .OR. nvc > 0 .OR. nshum > 0 .OR. nskeb > 0) then
          OPEN(stochlun,file=sfile,form='unformatted')
          print*,'open ',sfile,' for output'
       endif
    endif
    if (nsppt > 0) then
       do n=1,nsppt 
       call write_pattern(rpattern_sppt(n),1,stochlun)
       enddo
    endif
    if (nvc > 0) then
       do n=1,nvc 
       call write_pattern(rpattern_vc(n),1,stochlun)
       enddo
    endif
    if (nshum > 0) then
       do n=1,nshum
       call write_pattern(rpattern_shum(n),1,stochlun)
       enddo
    endif
    if (nskeb > 0) then
       do n=1,nskeb
       do k=1,levs
          call write_pattern(rpattern_skeb(n),k,stochlun)
       enddo
       enddo
    endif
    close(stochlun)
 end subroutine dump_patterns
 subroutine restore_patterns(sfile)
    implicit none
    character*9 :: sfile
    integer :: stochlun,k,n
    stochlun=99
    if (is_master()) then
       if (nsppt > 0 .OR. nvc > 0 .OR. nshum > 0 .OR. nskeb > 0) then
          OPEN(stochlun,file=sfile,form='unformatted',status='old')
       endif
    endif
    if (nsppt > 0) then
       do n=1,nsppt
       call read_pattern(rpattern_sppt(n),1,stochlun)
       enddo
    endif
    if (nvc > 0) then
       do n=1,nvc
       call read_pattern(rpattern_vc(n),1,stochlun)
       enddo
    endif
    if (nshum > 0) then
       do n=1,nshum
       call read_pattern(rpattern_shum(n),1,stochlun)
       enddo
    endif
    if (nskeb > 0) then
       do n=1,nskeb
       do k=1,levs
          call read_pattern(rpattern_skeb(n),k,stochlun)
       enddo
       enddo
    endif
    close(stochlun)
 end subroutine restore_patterns
subroutine read_pattern(rpattern,k,lunptn)
   implicit none
   type(random_pattern), intent(inout) :: rpattern
   integer, intent(in) :: lunptn,k
   real(kind_dbl_prec),allocatable  :: pattern2d(:)
   integer nm,nn,ierr

   allocate(pattern2d(2*ndimspec))

   ! read only on root process, and send to all tasks
   if (is_master()) then
      read(lunptn) pattern2d
      print*,'reading in random pattern (min/max/size)',minval(pattern2d),maxval(pattern2d),size(pattern2d)
   endif
   do nn=1,2*ndimspec
      call mp_bcst(pattern2d(nn))
   enddo
   ! subset
   do nn=1,len_trie_ls
      nm = rpattern%idx_e(nn)
      if (nm == 0) cycle
      rpattern%spec_e(nn,1,k) = pattern2d(nm)
      rpattern%spec_e(nn,2,k) = pattern2d(ndimspec+nm)
   enddo
   do nn=1,len_trio_ls
      nm = rpattern%idx_o(nn)
      if (nm == 0) cycle
      rpattern%spec_o(nn,1,k) = pattern2d(nm)
      rpattern%spec_o(nn,2,k) = pattern2d(ndimspec+nm)
   enddo
   !print*,'after scatter...',me,maxval(rpattern%spec_e),maxval(rpattern%spec_o) &
   ! ,minval(rpattern%spec_e),minval(rpattern%spec_o)
   deallocate(pattern2d)
 end subroutine read_pattern

 subroutine write_pattern(rpattern,lev,lunptn)
   implicit none
   type(random_pattern), intent(inout) :: rpattern
   integer, intent(in) :: lunptn,lev
   real(kind_dbl_prec), allocatable  :: pattern2d(:),pattern2d_out(:)
   integer nm,nn,ierr

   allocate(pattern2d(2*ndimspec))
   allocate(pattern2d_out(2*ndimspec))
   pattern2d=0.0
   pattern2d_out=0.0
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
   call mpi_reduce(pattern2d,pattern2d_out,2*ndimspec,&
                   kind_ior,kind_ior,masterproc,commglobal,ierr)
  !  write only on root process
   if (is_master()) then
      print*,'writing out random pattern (min/max/size)',&
      minval(pattern2d_out),maxval(pattern2d_out),size(pattern2d_out)
      !print*,'max/min pattern=',maxval(pattern2d),minval(pattern2d)
      write(lunptn) pattern2d_out
   endif
   deallocate(pattern2d)
   deallocate(pattern2d_out)
 end subroutine write_pattern
 subroutine vrtdivspect_to_uvgrid(&
           trie_di,trio_di,trie_ze,trio_ze,&
           uug,vvg,&
           ls_node,ls_nodes,max_ls_nodes,&
           lats_nodes_a,global_lats_a,lonsperlar,&
           epsedn,epsodn,snnp1ev,snnp1od,plnev_a,plnod_a,nlevs,Atm)

      implicit none
      real(kind=kind_dbl_prec), intent(in) :: trie_di(len_trie_ls,2,nlevs)
      real(kind=kind_dbl_prec), intent(in) :: trio_di(len_trio_ls,2,nlevs)
      real(kind=kind_dbl_prec), intent(in) :: trie_ze(len_trie_ls,2,nlevs)
      real(kind=kind_dbl_prec), intent(in) :: trio_ze(len_trio_ls,2,nlevs)
      real(kind=kind_dbl_prec),  intent(out) :: uug(lonf,lats_node_a,nlevs)
      real(kind=kind_dbl_prec),  intent(out) :: vvg(lonf,lats_node_a,nlevs)
      type(fv_atmos_type), target :: Atm
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
                  lats_node_a,ipt_lats_node_a,lon_dim_a,&
                  lonsperlar,lon_dim_a,latg,0,Atm)

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
