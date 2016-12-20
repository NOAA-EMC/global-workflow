      subroutine four2grid_thread(syn_gr_a_1,syn_gr_a_2,
     &  lon_dim,lons_lat,lonl,lot,lan,me)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      use machine
      implicit none
!!
      integer              init,lot,lonl
      integer              lan,me
      integer              ibmsign
      real(kind=kind_dbl_prec) aux1crs(42002)
      real(kind=kind_evod) scale_ibm
      integer              lon_dim,lons_lat
      real(kind=kind_evod) syn_gr_a_1(lonl*lot)
      real(kind=kind_evod) syn_gr_a_2(lonl*lot)
!________________________________________________________
      integer              num_threads
      integer              nvar_thread_max
      integer              nvar_1,nvar_2
      integer              thread
      integer              lot_thread
      integer              indbeg
!________________________________________________________
      num_threads=min(num_parthds(),lot)
 
      nvar_thread_max=(lot+num_threads-1)/num_threads
 
!100   format('thread=',i2,2x,
!    & ' lot=',i3,2x,' indbeg=',i6,2x,' lan=',i2,2x,' me_fou=',i3,
!    & ' lon_dim=',i3,2x,' lons_lat=',i3)
!200   format('thread=',i2,2x,' nvar_1=',i6,2x,' nvar_2=',i6,2x
!    &,' lot=',i3,2x,' indbeg=',i6,2x,' lan=',i2,2x,' me_fou=',i3,
!    & ' lon_dim=',i3,2x,' lons_lat=',i3)
      if ( kind_evod .eq. 8 ) then !------------------------------------
!$omp parallel do shared(syn_gr_a_1,syn_gr_a_2,lon_dim,lons_lat)
!$omp+shared(lot,num_threads,nvar_thread_max)
!$omp+private(thread,nvar_1,nvar_2,lot_thread,indbeg,aux1crs)
!$omp+private(ibmsign,scale_ibm,init)
 
         do thread=1,num_threads   ! start of thread loop ..............
            nvar_1=(thread-1)*nvar_thread_max+1
            nvar_2=min(nvar_1+nvar_thread_max-1,lot)
            lot_thread=nvar_2 - nvar_1 +1
         indbeg=1+lon_dim*(nvar_1-1)
!sela    print 200,thread,nvar_1,nvar_2,lot_thread,indbeg,lan,me,lon_dim
!sela& ,lons_lat
!sela    print 100,thread,lot_thread,indbeg,lan,me,lon_dim
!sela& ,lons_lat
 
         init=1
         ibmsign=-1
         scale_ibm=1.0d0
         call dcrft(init,
     x              syn_gr_a_1(indbeg)   ,lon_dim/2,
     x              syn_gr_a_2(indbeg)   ,lon_dim,
     x              lons_lat,lot_thread,ibmsign,scale_ibm,
     x              aux1crs,22000,
     x              aux1crs(22001),20000)
         init=0
         call dcrft(init,
     x              syn_gr_a_1(indbeg)   ,lon_dim/2,
     x              syn_gr_a_2(indbeg)   ,lon_dim,
     x              lons_lat,lot_thread,ibmsign,scale_ibm,
     x              aux1crs,22000,
     x              aux1crs(22001),20000)
 
      enddo  ! fin thread loop .........................................
      else !------------------------------------------------------------
!$omp parallel do shared(syn_gr_a_1,syn_gr_a_2,lon_dim,lons_lat)
!$omp+shared(lot,num_threads,nvar_thread_max)
!$omp+private(thread,nvar_1,nvar_2,lot_thread,indbeg,aux1crs)
!$omp+private(ibmsign,scale_ibm,init)
 
         do thread=1,num_threads   ! start of thread loop ..............
            nvar_1=(thread-1)*nvar_thread_max+1
            nvar_2=min(nvar_1+nvar_thread_max-1,lot)
            lot_thread=nvar_2 - nvar_1 +1
         indbeg=1+lon_dim*(nvar_1-1)
!sela    print 200,thread,nvar_1,nvar_2,lot_thread,indbeg,lan,me,lon_dim
!sela& ,lons_lat
!sela    print 100,thread,lot_thread,indbeg,lan,me,lon_dim
!sela& ,lons_lat
 
         init=1
         ibmsign=-1
         scale_ibm=1.0d0
         call scrft(init,
     x              syn_gr_a_1(indbeg)   ,lon_dim/2,
     x              syn_gr_a_2(indbeg)   ,lon_dim,
     x              lons_lat,lot_thread,ibmsign,scale_ibm,
     x              aux1crs,22000,
     x              aux1crs(22001),20000,
     x              aux1crs(22001),0)
         init=0
         call scrft(init,
     x              syn_gr_a_1(indbeg)   ,lon_dim/2,
     x              syn_gr_a_2(indbeg)   ,lon_dim,
     x              lons_lat,lot_thread,ibmsign,scale_ibm,
     x              aux1crs,22000,
     x              aux1crs(22001),20000,
     x              aux1crs(22001),0)
 
      enddo  ! fin thread loop .........................................
      endif !-----------------------------------------------------------
!!
      return
      end
      subroutine grid2four_thread(anl_gr_a_2,anl_gr_a_1,
     &                     lon_dim,lons_lat,lonl,lot)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      use machine
      implicit none
!!
      integer              init,lot,lonl
      integer              ibmsign
      real(kind=kind_dbl_prec) aux1crs(42002)
      real(kind=kind_evod) scale_ibm,rone
      integer              lon_dim,lons_lat
      real(kind=kind_evod) anl_gr_a_1(lonl*lot)
      real(kind=kind_evod) anl_gr_a_2(lonl*lot)
!________________________________________________________
      integer              num_threads
      integer              nvar_thread_max
      integer              nvar_1,nvar_2
      integer              thread
      integer              lot_thread
      integer              indbeg
!________________________________________________________
      num_threads=min(num_parthds(),lot)
 
      nvar_thread_max=(lot+num_threads-1)/num_threads
      if ( kind_evod .eq. 8 ) then !------------------------------------
!$omp parallel do shared(anl_gr_a_1,anl_gr_a_2,lon_dim,lons_lat)
!$omp+shared(lot,num_threads,nvar_thread_max)
!$omp+shared(ibmsign,scale_ibm,rone)
!$omp+private(thread,nvar_1,nvar_2,lot_thread,indbeg,init,aux1crs)
 
         do thread=1,num_threads   ! start of thread loop ..............
            nvar_1=(thread-1)*nvar_thread_max+1
            nvar_2=min(nvar_1+nvar_thread_max-1,lot)
            lot_thread=nvar_2 - nvar_1 +1
 
         indbeg=1+lon_dim*(nvar_1-1)
 
         init=1
         ibmsign=1
         rone=1.0d0
         scale_ibm=rone/lons_lat
         call drcft(init,
     x              anl_gr_a_2(indbeg),   lon_dim,
     x              anl_gr_a_1(indbeg),   lon_dim/2,
     x              lons_lat,lot_thread,ibmsign,scale_ibm,
     x              aux1crs,22000,
     x              aux1crs(22001),20000)
         init=0
         call drcft(init,
     x              anl_gr_a_2(indbeg),   lon_dim,
     x              anl_gr_a_1(indbeg),   lon_dim/2,
     x              lons_lat,lot_thread,ibmsign,scale_ibm,
     x              aux1crs,22000,
     x              aux1crs(22001),20000)
 
      enddo  ! fin thread loop .........................................
      else !------------------------------------------------------------
!$omp parallel do shared(anl_gr_a_1,anl_gr_a_2,lon_dim,lons_lat)
!$omp+shared(lot,num_threads,nvar_thread_max)
!$omp+shared(ibmsign,scale_ibm,rone)
!$omp+private(thread,nvar_1,nvar_2,lot_thread,indbeg,init,aux1crs)
 
         do thread=1,num_threads   ! start of thread loop ..............
            nvar_1=(thread-1)*nvar_thread_max+1
            nvar_2=min(nvar_1+nvar_thread_max-1,lot)
            lot_thread=nvar_2 - nvar_1 +1
 
         indbeg=1+lon_dim*(nvar_1-1)
 
         init=1
         ibmsign=1
         rone=1.0d0
         scale_ibm=rone/lons_lat
         call srcft(init,
     x              anl_gr_a_2(indbeg),   lon_dim,
     x              anl_gr_a_1(indbeg),   lon_dim/2,
     x              lons_lat,lot_thread,ibmsign,scale_ibm,
     x              aux1crs,22000,
     x              aux1crs(22001),20000,
     x              aux1crs(22001),0)
         init=0
         call srcft(init,
     x              anl_gr_a_2(indbeg),   lon_dim,
     x              anl_gr_a_1(indbeg),   lon_dim/2,
     x              lons_lat,lot_thread,ibmsign,scale_ibm,
     x              aux1crs,22000,
     x              aux1crs(22001),20000,
     x              aux1crs(22001),0)
 
      enddo  ! fin thread loop .........................................
      endif !-----------------------------------------------------------
!!
      return
      end
