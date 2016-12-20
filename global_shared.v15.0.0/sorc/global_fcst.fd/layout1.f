      module layout1
!     use resol_def
      implicit none
      save
cc
      integer           nodes, nodes_comp,
     x                  me,
     x                  ls_dim,
     x                  ls_max_node,
     x                  lats_dim_a,
     x                  lats_dim_r,
!    x                  lats_dim_ext,
     x                  lats_node_a,
     x                  lats_node_a_max,
!    x                  lats_node_ext,
     x                  lats_node_r,
     x                  lats_node_r_max,
     x                  ipt_lats_node_a,
!    x                  ipt_lats_node_ext,
     x                  ipt_lats_node_r,
     x                  len_trie_ls,
     x                  len_trio_ls,
     x                  len_trie_ls_max,
     x                  len_trio_ls_max,
     x                  lon_dim_a,
     x                  lon_dim_r,
     x                  me_l_0
cc
      integer ,allocatable :: lat1s_a(:),lat1s_r(:)
     &                       ,lon_dims_r(:)
!mjr .  lon_dims_a(:),lon_dims_ext(:),lon_dims_r(:)
!mjr .  lon_dims_a(:),                lon_dims_r(:)

!hmhj ndsl
      integer   lonfull,lonhalf,lonpart,lonlenmax,mylonlen
      integer   latfull,lathalf,latpart,latlenmax,mylatlen

      integer, allocatable :: lonstr(:),lonlen(:)
      integer, allocatable :: latstr(:),latlen(:)
      real, allocatable :: cosglat(:)
      real, allocatable :: gglat(:),gglati(:),gslati(:),ggfact(:,:)
      real, allocatable :: gglon(:),ggloni(:)
      real                 dslon, dslat, scale

      integer, allocatable :: i00_reg2red(:,:),i00_red2reg(:,:)
      integer, allocatable :: i11_reg2red(:,:),i11_red2reg(:,:)
      real,    allocatable :: h00_reg2red(:,:),h00_red2reg(:,:)
      real,    allocatable :: h01_reg2red(:,:),h01_red2reg(:,:)
      real,    allocatable :: h10_reg2red(:,:),h10_red2reg(:,:)
      real,    allocatable :: h11_reg2red(:,:),h11_red2reg(:,:)

      end module layout1
