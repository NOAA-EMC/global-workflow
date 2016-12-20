      module slgshr
      save

      real ra                  ! reciprocal of radius of earth meters
      real, allocatable ::
     &     lammp(:,:,:),! trajectory mid-point long. coordinate
     &     phimp(:,:,:),! trajectory mid-point lat   coordinate
     &     sigmp(:,:,:),! trajectory mid-point sigma coordinate
     &     phi(:),            ! latitude  coordinates of model grid
     &     dphi(:),           ! latitudinal grid increments
     &     lbasdy(:,:,:),     ! basis functions for lat deriv est.
     &     lbasiy(:,:,:),     ! basis functions for lagrange interp
     &     dlam(:), rdlam(:), rdlam6(:), lam(:,:), dphii(:)
      integer, allocatable :: nlonex(:)
      end module slgshr

