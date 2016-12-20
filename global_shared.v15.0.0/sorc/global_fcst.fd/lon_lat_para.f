      subroutine lonlat_para(global_lats_r,xlon,xlat,lonsperlar)
!
c***********************************************************************
!
      use machine , only : kind_rad,kind_phys

      use resol_def
      use layout1
      use gg_def
      use physcons, pi => con_pi
      implicit none
      integer i,j,lat
      integer                 lonsperlar(latr)
      real (kind=kind_rad) tpi,hpi,bphi
      parameter (tpi=2.e0*pi,hpi=0.5e0*pi)
      integer              global_lats_r(latr)
      real (kind=kind_rad) xlon(lonr,lats_node_r)
      real (kind=kind_rad) xlat(lonr,lats_node_r)
!
      xlon=0.
      xlat=0.
 
      do j=1,lats_node_r
        lat = global_lats_r(ipt_lats_node_r-1+j)
        bphi = tpi/lonsperlar(lat)
        if (lat.le.latr2) then
          do i=1,lonsperlar(lat)
            xlon(i,j) = (i-1) * bphi
            xlat(i,j) = hpi - colrad_r(lat)
          enddo
        else
          do i=1,lonsperlar(lat)
            xlon(i,j) =  (i-1) * bphi
            xlat(i,j) = colrad_r(lat)-hpi
          enddo
        endif
      enddo
 
      return
      end
 
