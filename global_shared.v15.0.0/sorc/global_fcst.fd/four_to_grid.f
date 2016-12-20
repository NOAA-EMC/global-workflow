      subroutine four_to_grid(syn_gr_a_1,syn_gr_a_2,
     &  lon_dim_coef,lon_dim_grid,lons_lat,lot)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      use machine , only : kind_dbl_prec,kind_evod
      implicit none
!!
      real(kind=kind_evod)     syn_gr_a_1(lon_dim_coef,lot)
      real(kind=kind_evod)     syn_gr_a_2(lon_dim_grid,lot)
      integer                  lon_dim_coef
      integer                  lon_dim_grid
      integer                  lons_lat
      integer                  lot
!________________________________________________________
      real(kind=kind_dbl_prec) aux1crs(42002)
      real(kind=kind_evod)     scale_ibm
      integer                  ibmsign
      integer                  init
      integer                  lot_thread
      integer                  num_threads
      integer                  nvar_thread_max
      integer                  nvar_1
      integer                  nvar_2
      integer                  thread
      integer                  num_parthds
!________________________________________________________
      num_threads=min(num_parthds(),lot)
 
      nvar_thread_max=(lot+num_threads-1)/num_threads
 
      if ( kind_evod .eq. 8 ) then !------------------------------------
!$omp parallel do shared(syn_gr_a_1,syn_gr_a_2,lons_lat)
!$omp+shared(lon_dim_coef,lon_dim_grid)
!$omp+shared(lot,num_threads,nvar_thread_max)
!$omp+shared(ibmsign,scale_ibm)
!$omp+private(thread,nvar_1,nvar_2,lot_thread,init,aux1crs)
 
         do thread=1,num_threads   ! start of thread loop ..............
            nvar_1=(thread-1)*nvar_thread_max+1
            nvar_2=min(nvar_1+nvar_thread_max-1,lot)
            lot_thread=nvar_2 - nvar_1 +1

            init=1
            ibmsign=-1
            scale_ibm=1.0d0
            call dcrft(init,
     x              syn_gr_a_1(1,nvar_1)   ,lon_dim_coef/2,
     x              syn_gr_a_2(1,nvar_1)   ,lon_dim_grid,
     x              lons_lat,lot_thread,ibmsign,scale_ibm,
     x              aux1crs,22000,
     x              aux1crs(22001),20000)
            init=0
            call dcrft(init,
     x              syn_gr_a_1(1,nvar_1)   ,lon_dim_coef/2,
     x              syn_gr_a_2(1,nvar_1)   ,lon_dim_grid,
     x              lons_lat,lot_thread,ibmsign,scale_ibm,
     x              aux1crs,22000,
     x              aux1crs(22001),20000)
 
         enddo  ! fin thread loop ......................................
      else !------------------------------------------------------------
!$omp parallel do shared(syn_gr_a_1,syn_gr_a_2,lons_lat)
!$omp+shared(lon_dim_coef,lon_dim_grid)
!$omp+shared(lot,num_threads,nvar_thread_max)
!$omp+shared(ibmsign,scale_ibm)
!$omp+private(thread,nvar_1,nvar_2,lot_thread,init,aux1crs)
 
         do thread=1,num_threads   ! start of thread loop ..............
            nvar_1=(thread-1)*nvar_thread_max+1
            nvar_2=min(nvar_1+nvar_thread_max-1,lot)
            lot_thread=nvar_2 - nvar_1 +1
 
            init=1
            ibmsign=-1
            scale_ibm=1.0d0
            call scrft(init,
     x              syn_gr_a_1(1,nvar_1)   ,lon_dim_coef/2,
     x              syn_gr_a_2(1,nvar_1)   ,lon_dim_grid,
     x              lons_lat,lot_thread,ibmsign,scale_ibm,
     x              aux1crs,22000,
     x              aux1crs(22001),20000,
     x              aux1crs(22001),0)
            init=0
            call scrft(init,
     x              syn_gr_a_1(1,nvar_1)   ,lon_dim_coef/2,
     x              syn_gr_a_2(1,nvar_1)   ,lon_dim_grid,
     x              lons_lat,lot_thread,ibmsign,scale_ibm,
     x              aux1crs,22000,
     x              aux1crs(22001),20000,
     x              aux1crs(22001),0)
 
         enddo  ! fin thread loop ......................................
      endif !-----------------------------------------------------------
!!
      return
      end
      subroutine grid_to_four(anl_gr_a_2,anl_gr_a_1,
     &  lon_dim_grid,lon_dim_coef,lons_lat,lot)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      use machine , only : kind_dbl_prec,kind_evod
      implicit none
!!
      real(kind=kind_evod)     anl_gr_a_2(lon_dim_grid,lot)
      real(kind=kind_evod)     anl_gr_a_1(lon_dim_coef,lot)
      integer                  lon_dim_grid
      integer                  lon_dim_coef
      integer                  lons_lat
      integer                  lot
!________________________________________________________
      real(kind=kind_dbl_prec) aux1crs(42002)
      real(kind=kind_evod)     scale_ibm,rone
      integer                  ibmsign
      integer                  init
      integer                  lot_thread
      integer                  num_threads
      integer                  nvar_thread_max
      integer                  nvar_1,nvar_2
      integer                  thread
      integer                  num_parthds
!________________________________________________________
      num_threads=min(num_parthds(),lot)
 
      nvar_thread_max=(lot+num_threads-1)/num_threads
 
      if ( kind_evod .eq. 8 ) then !------------------------------------
!$omp parallel do shared(anl_gr_a_1,anl_gr_a_2,lons_lat)
!$omp+shared(lon_dim_coef,lon_dim_grid)
!$omp+shared(lot,num_threads,nvar_thread_max)
!$omp+shared(ibmsign,scale_ibm,rone)
!$omp+private(thread,nvar_1,nvar_2,lot_thread,init,aux1crs)
 
         do thread=1,num_threads   ! start of thread loop ..............
            nvar_1=(thread-1)*nvar_thread_max+1
            nvar_2=min(nvar_1+nvar_thread_max-1,lot)
            lot_thread=nvar_2 - nvar_1 +1
 
            init=1
            ibmsign=1
            rone=1.0d0
            scale_ibm=rone/lons_lat
            call drcft(init,
     x              anl_gr_a_2(1,nvar_1),   lon_dim_grid,
     x              anl_gr_a_1(1,nvar_1),   lon_dim_coef/2,
     x              lons_lat,lot_thread,ibmsign,scale_ibm,
     x              aux1crs,22000,
     x              aux1crs(22001),20000)
            init=0
            call drcft(init,
     x              anl_gr_a_2(1,nvar_1),   lon_dim_grid,
     x              anl_gr_a_1(1,nvar_1),   lon_dim_coef/2,
     x              lons_lat,lot_thread,ibmsign,scale_ibm,
     x              aux1crs,22000,
     x              aux1crs(22001),20000)
 
         enddo  ! fin thread loop ......................................
      else !------------------------------------------------------------
!$omp parallel do shared(anl_gr_a_1,anl_gr_a_2,lons_lat)
!$omp+shared(lon_dim_coef,lon_dim_grid)
!$omp+shared(lot,num_threads,nvar_thread_max)
!$omp+shared(ibmsign,scale_ibm,rone)
!$omp+private(thread,nvar_1,nvar_2,lot_thread,init,aux1crs)
 
         do thread=1,num_threads   ! start of thread loop ..............
            nvar_1=(thread-1)*nvar_thread_max+1
            nvar_2=min(nvar_1+nvar_thread_max-1,lot)
            lot_thread=nvar_2 - nvar_1 +1
 
            init=1
            ibmsign=1
            rone=1.0d0
            scale_ibm=rone/lons_lat
            call srcft(init,
     x              anl_gr_a_2(1,nvar_1),   lon_dim_grid,
     x              anl_gr_a_1(1,nvar_1),   lon_dim_coef/2,
     x              lons_lat,lot_thread,ibmsign,scale_ibm,
     x              aux1crs,22000,
     x              aux1crs(22001),20000,
     x              aux1crs(22001),0)
            init=0
            call srcft(init,
     x              anl_gr_a_2(1,nvar_1),   lon_dim_grid,
     x              anl_gr_a_1(1,nvar_1),   lon_dim_coef/2,
     x              lons_lat,lot_thread,ibmsign,scale_ibm,
     x              aux1crs,22000,
     x              aux1crs(22001),20000,
     x              aux1crs(22001),0)
 
         enddo  ! fin thread loop ......................................
      endif !-----------------------------------------------------------
!!
      return
      end
