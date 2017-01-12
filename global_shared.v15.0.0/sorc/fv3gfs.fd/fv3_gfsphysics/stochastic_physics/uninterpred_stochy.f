      subroutine uninterpred_stochy(iord,kmsk,f,fi,global_lats_a,
     &                              lonsperlat)
!!
      use stochy_resol_def,   ONLY: latg, lonf
      use spectral_layout,     ONLY: lats_node_a, ipt_lats_node_a
      use machine
      implicit none
!!
      integer              global_lats_a(latg)
      integer,intent(in):: iord
      integer,intent(in):: kmsk(lonf,lats_node_a)
      integer,intent(in):: lonsperlat(latg)
      real(kind=kind_dbl_prec),intent(out):: f(lonf,lats_node_a)
      real(kind=kind_dbl_prec),intent(in) :: fi(lonf,lats_node_a)
      integer j,lons,lat
!!
!$omp parallel do private(j,lat,lons)
      do j=1,lats_node_a
        lat  = global_lats_a(ipt_lats_node_a-1+j)
        lons = lonsperlat(lat)
        if(lons /= lonf) then
          call intlon(iord,1,1,lons,lonf, kmsk(1,j),fi(1,j),f(1,j))
        else
          f(:,j) = fi(:,j)
        endif
      enddo
      end subroutine

