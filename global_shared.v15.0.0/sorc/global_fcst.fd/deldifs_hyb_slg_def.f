      module deldifs_hyb_slg_def
      use machine
      implicit none
      save
      real(kind=kind_evod),allocatable :: dne_hyb(:),dno_hyb(:)
      real(kind=kind_evod),allocatable :: sl_hyb(:),sf_hyb(:)
      real(kind=kind_evod),allocatable :: rtrd_hyb(:),rthk_hyb(:)
      integer jdel
      real(kind=kind_evod) rtnp
      end module deldifs_hyb_slg_def
