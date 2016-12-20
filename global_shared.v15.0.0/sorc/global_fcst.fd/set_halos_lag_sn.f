      subroutine set_halos(grid_h_2,
     &                     lats_nodes_h,global_lats_h_sn,
     &                     lonsperlat,
     &                     lon_dim_h,xhalo,yhalo,
     &                     lot,lot_dim)
!
!    subroutine set_halos sets x and y halos
!    in this version, latitudes of grid_h_2 are south to north.
!    for real array grid_h_2
!    dimensioned:   grid_h_2(lon_dim_h,lot_dim,lats_dim_h)
!    integer lats_nodes_h(nodes) -   number of latitudes for each processor.
!    integer global_lats_h_ns(latg+2*yhalo*nodes) -
!                        - latitude numbers for all processors, north to south.
!    integer lonsperlat(latg) - number of longitudes for each latitude number.
!    integer xhalo - length of east-west halo.
!    integer yhalo - length of north-south halo.
!    integer lot -   number of sets of data; lot .le. lot_dim
!
      use machine    , only : kind_evod
      use resol_def  , only : latg,lonf
      use layout1    , only : me,nodes
      use mpi_def    , only : mc_comp,mpi_r_def,stat
      use layout_lag , only : ipt_lats_node_h,lats_dim_h,lats_node_h
!
      implicit none
!
      integer xhalo, yhalo, lot, lot_dim
      real(kind=kind_evod) grid_h_2(lon_dim_h,lot_dim,lats_dim_h)
!
      integer lats_nodes_h(nodes)
      integer, dimension(latg+2*yhalo*nodes) ::  global_lats_h_sn
      integer lonsperlat(latg)
      integer lon_dim_h
!
      integer i, i1, i2, ierr, iprint, j, jj, k, lan, lan_n, lat, n 
     &,       lons_lat, num_recv, num_send, lotlon_dim_h, itagme
!mjr  integer lon_dim
!
      integer, dimension (2*yhalo*nodes) ::  req_recv, req_send
!
!     integer, parameter :: itaga=2**5, itagb=2**4
!     integer, parameter :: itaga=2**15, itagb=2**20
!     integer, parameter :: itaga=2**21, itagb=2**20
      integer :: itagb, itaga
!     integer i_count
!     save    i_count
!     data    i_count / 0 /
!             i_count = i_count + 1
!
      itagb = latg
      itaga = nodes*itagb
      if ( xhalo == 0 .and. yhalo == 0 ) return
!
      do lan=1+yhalo,lats_node_h-yhalo

        lat = global_lats_h_sn(ipt_lats_node_h-1+lan)
!mjr    lon_dim = lon_dims_h_grid(lan)
        lons_lat = lonsperlat(lat)

        do k=1,lot

!         initialize xhalo west endpoints

          i2 = 0
          do i=xhalo,1,-1
            grid_h_2(i,k,lan) = grid_h_2(xhalo+lons_lat-i2,k,lan)
            i2 = i2 + 1
          enddo

!         initialize xhalo east endpoints

          do i=1,xhalo+(lon_dim_h-lonf-2*xhalo)
            grid_h_2(xhalo+i+lons_lat,k,lan) = grid_h_2(xhalo+i,k,lan)
          enddo
        enddo

      enddo                  !     fin initialization of xhalo endpoints

      if(nodes == 1)return
!
!cmr---------------------------------------------------
! 
!     each processor has lats_node_h latitudes:
!     yhalo south halo latitudes;
!     lats_node_h-2*yhalo non-halo latitudes;
!     yhalo north halo latitudes;
! 
!     copy or receive for south and north halos;
!     processor sets values for its own halo latitudes

      lotlon_dim_h = lot * lon_dim_h                    ! number of elements

      num_recv = 0                                      ! set number receive count to zero
      do k=1,2                                          ! k=1 south halo ;  k=2 north halo 
                                                        !     ----------        ----------
        do j=1,yhalo                                    ! increment south to north in halo
          if (k == 1) lan = j                           ! lan is pointer in processor's lats
          if (k == 2) lan = lats_node_h-yhalo+j
          lat = global_lats_h_sn(ipt_lats_node_h-1+lan) ! lat is latitude number from 1 to latg
                                                        ! lon_dim is dimension of longitude circle
          i = 0                                         ! set pointer to zero for all latitudes
                                                        !                    (halo and non-halo)
          do n=1,nodes                                  ! increment through all comp tasks
            i = i + yhalo                               ! skip south halo
            itagme = k*itaga + (n-1)*itagb
            do lan_n=1+yhalo,lats_nodes_h(n)-yhalo      ! non-halo lats
              i = i + 1                                 ! increment pointer for all latitudes
              if (lat == global_lats_h_sn(i)) then
                if (n-1 == me) then                     ! values are in same processor
                  grid_h_2(:,:,lan  ) = grid_h_2(:,:,lan_n)
                else                                    ! values are in different processors
                  num_recv = num_recv + 1               ! increment number receive count
                  call mpi_irecv(                       ! nonblocking receive operation
     &                grid_h_2(1,1,lan),                ! receive buffer
     &                lotlon_dim_h,                     ! number of elements
     &                mpi_r_def,                        ! data type
     &                n-1,                              ! rank of source process
!moo &                1*10**8+(n-1)*10**4+lat,          ! message tag
     &                itagme+lat,                       ! message tag
     &                mc_comp,                          ! communicator handle
     &                req_recv(num_recv),               ! communication request
     &                ierr)                             ! out: return code
                endif
                go to 12345
              endif
            enddo
            i = i + yhalo                               !  skip north halo
          enddo
12345     continue
        enddo
      enddo
!
!  each processor sends values of its own non-halo latitudes to other processors' halo latitudes
!
      num_send = 0                                      !  set number send count to zero
      i = 0                                             !  set pointer ito zero for all latitudes
                                                        !                      (halo and non-halo) 
      do n=1,nodes                                      !  increment through all comp tasks
        do k=1,2                                        !  k=1 south halo and k=2 north halo 
                                                        !      ----------        ----------
          itagme = k*itaga + me*itagb

          do j=1,yhalo                                  !  increment south to north in halo
            i = i + 1                                   !  increment pointer for all latitudes
            do lan=1+yhalo,lats_node_h-yhalo            !  non-halo lats
                                                        !  lat is latitude number from 1 to latg
              lat = global_lats_h_sn(ipt_lats_node_h-1+lan)
              if (lat == global_lats_h_sn(i)) then
                if (n-1 /= me) then
                                                        !  lon_dim is dimension of longitude circle
!mjr              lon_dim = lon_dims_h_grid(lan)
                  num_send = num_send + 1               !  increment number send count
                  call mpi_isend(                       !  nonblocking send operation
     &                grid_h_2(1,1,lan),                !  send buffer
!mjr &                lot*lon_dim,                      !  number of elements
     &                lotlon_dim_h,                     !  number of elements
     &                mpi_r_def,                        !  data type
     &                n-1,                              !  rank of destination process
!moo &                1*10**8+me*10**4+lat,             !  message tag
     &                itagme+lat,                       !  message tag
     &                mc_comp,                          !  communicator handle
     &                req_send(num_send),               !  communication request
     &                ierr)                             !  out: return code
                endif
                go to 23456
              endif
            enddo
23456       continue
          enddo
          if (k == 1) i = i + lats_nodes_h(n)-2*yhalo   !  skip non-halo
        enddo
      enddo
!
!mr  print*,' num_recv=',num_recv,' num_send=',num_send
!mr  call mpi_barrier (mc_comp,ierr)
!
      do j=1,num_recv     !  increment through number receive count
                          !  waits for nonblocking grid_h_2 receive operation to complete
        call mpi_wait(
     &       req_recv(j), !  communication request to wait for
     &       stat,        !  out: status object
     &       ierr)        !  out: return code
      enddo
!
      do j=1,num_send     !  increment through number send count
                          !  waits for nonblocking grid_h_2 send operationto complete
        call mpi_wait(
     &       req_send(j), !  communication request to wait for
     &       stat,        !  out: status object
     &       ierr)        !  out: return code
      enddo
!
      return
      end
