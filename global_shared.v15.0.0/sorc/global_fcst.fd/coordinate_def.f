      module coordinate_def
!     use resol_def
      use machine
      implicit none
      save
       real(kind=kind_evod) , allocatable ::
     . ak5(:),bk5(:),ck5(:),ck(:),dbk(:),bkl(:),   		! hmhj
     . amhyb(:,:),bmhyb(:,:),svhyb(:),tor_hyb(:),
     . d_hyb_m(:,:,:),thref(:),dm205_hyb(:,:,:)			! hmhj
       real(kind=kind_evod) vertcoord_id,eps_si			! hmhj

!
      real(kind=kind_evod) , allocatable :: vcoord(:,:)
      integer nvcoord, idsl, idvc, idvm
!
!sela for semilag version.
       real(kind=kind_evod) , allocatable ::
     . yecm(:,:),tecm(:,:),sv_ecm(:),
     . am_slg(:,:),bm_slg(:,:),sv_slg(:),tor_slg(:),
     . d_slg_m(:,:,:),y_ecm(:,:),t_ecm(:,:)
!
      end module coordinate_def
