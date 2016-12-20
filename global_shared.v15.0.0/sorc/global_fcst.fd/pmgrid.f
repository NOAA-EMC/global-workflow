      module pmgrid
      save
      integer
     $     plon,    ! number of longitudes
     $     plev,    ! number of vertical levels
     $     plat,    ! number of latitudes
     $     plevp,   ! plev + 1
!mjr $     nxpt,    ! no.of points outside active domain for interpolant
!mjr $     jintmx,  ! number of extra latitudes in polar region
     $     plond,   ! slt extended domain longitude
     $     platd,   ! slt extended domain lat.
     $     plevd,   ! fold plev,pcnst indices into one
     $     pgls 
      real mprec,   ! machine precision to restrict dep
     &     pi
      real, allocatable ::  wgt(:)
      logical quamon
      end       module pmgrid
