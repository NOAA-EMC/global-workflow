      module date_def
!     use resol_def
      use machine
      implicit none
      save
      integer idate(4)
      real(kind=kind_evod) fhour,shour,thour,z00
      real(kind=kind_evod) ,allocatable :: spdmax(:)

      end module date_def
