      module spectral_layout
      implicit none
!
! program log:
! 20161011   philip pegion :  make stochastic pattern generator standalone
!

      integer           nodes, nodes_comp,nodes_io,
     &                  me,lon_dim_a,
     &                  ls_dim,
     &                  ls_max_node,
     &                  lats_dim_a,
     &                  lats_node_a,
     &                  lats_node_a_max,
     &                  ipt_lats_node_a,
     &                  len_trie_ls,
     &                  len_trio_ls,
     &                  len_trie_ls_max,
     &                  len_trio_ls_max,
     &                  me_l_0,

     &                  lats_dim_ext,
     &                  lats_node_ext,
     &                  ipt_lats_node_ext
!
      INTEGER ,ALLOCATABLE :: lat1s_a(:), lon_dims_a(:),lon_dims_ext(:)

      end module spectral_layout
