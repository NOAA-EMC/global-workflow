!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine four2grid_thread(syn_gr_a_1,syn_gr_a_2,
     &                            lon_dim,lons_lat,lonl,lot,lan,me)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
      use machine
      use resol_def
      implicit none
!!
      integer              lon_dim,lot
      real(kind=kind_evod) syn_gr_a_1(lon_dim,lot)
!!
      real(kind=kind_evod) syn_gr_a_2(lon_dim,lot)
!!

      integer              lons_lat
      integer              lonl
      integer              lan
      integer              me
!!
      integer              i
      integer              n
!!
      real(kind=kind_evod) table(44002)
!!
!!!!  print 200,lon_dim,lons_lat,lonl,lot,lan,me
  200 format (' four2grid_thread  fftpack',
     &        ' lon_dim=',i4,
     &        ' lons_lat=',i4,
     &        ' lonl=',i4,
     &        ' lot=',i4,
     &        ' lan=',i4,
     &        ' me=',i3)
!!
      call rffti(lons_lat,table)
!!
      do n=1,lot
         syn_gr_a_2(1,n)=syn_gr_a_1(1,n)
         do i=3,lons_lat+1
            syn_gr_a_2(i-1,n)=syn_gr_a_1(i,n)
         enddo
         call rfftb(lons_lat,syn_gr_a_2(1,n),table)
      enddo
!!
      return
      end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine grid2four_thread(anl_gr_a_2,anl_gr_a_1,
     &                            lon_dim,lons_lat,lonl,lot)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
      use machine
      use resol_def
      implicit none
!!
      integer lon_dim,lot
      real(kind=kind_evod) anl_gr_a_1(lon_dim,lot)
!!
      real(kind=kind_evod) anl_gr_a_2(lon_dim,lot)
!!
      integer              lons_lat
      integer              lonl
!!
      integer              i
      integer              n
!!
      real(kind=kind_evod) cons_0
!!
      real(kind=kind_evod) r_lons_lat
!!
      real(kind=kind_evod) table(44002)
!!
      real(kind=kind_evod) y(lons_lat)
!!
!!!!  print 200,lon_dim,lons_lat,lonl,lot
  200 format (' grid2four_thread  fftpack',
     &        ' lon_dim=',i4,
     &        ' lons_lat=',i4,
     &        ' lonl=',i4,
     &        ' lot=',i4)
!!
      cons_0=0
!!
      call rffti(lons_lat,table)
!!
      r_lons_lat=lons_lat
!!
      do n=1,lot
         do i=1,lons_lat
            y(i)=anl_gr_a_2(i,n)
         enddo
         call rfftf(lons_lat,y,table)
         anl_gr_a_1(1,n)=y(1)/r_lons_lat
         anl_gr_a_1(2,n)=cons_0
         do i=3,lons_lat+1
            anl_gr_a_1(i,n)=y(i-1)/r_lons_lat
         enddo
         anl_gr_a_1(lons_lat+2,n)=cons_0
      enddo
!!
      return
      end
