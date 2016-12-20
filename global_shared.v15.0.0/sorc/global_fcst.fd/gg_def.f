      module gg_def
!     use resol_def
      use machine

      implicit none
      save
      real(kind=kind_evod) ,allocatable ::  colrad_a(:),wgt_a(:),
     . wgtcs_a(:),rcs2_a(:),sinlat_a(:),colrad_r(:),wgt_r(:),
     . wgtcs_r(:),rcs2_r(:),sinlat_r(:),coslat_r(:)
      end module gg_def
