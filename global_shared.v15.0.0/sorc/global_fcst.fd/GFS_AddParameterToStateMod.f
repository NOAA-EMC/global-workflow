!----------------------------------------------------------------------
! !module: gfs_addparametertostatemod
!        --- add required parameters to the gfs esmf export state
!            for the ensemble coupler to do the spectral transform
!            for the stochastic perturbation scheme, the second step.
!
! !description: add all required parameters to the gfs esmf export state.
!
! !revision history:
!
!  may      2007     weiyu yang initial code.
!
!
! !interface:
!

 module gfs_addparametertostatemod

 use esmf_mod

 use resol_def
 use layout1
 use mpi_def
 use gfs_internalstate_esmfmod

 implicit none

 real(kind = kind_evod), dimension(:), pointer   :: work1a, work1b, work1c, work1d
 integer,                dimension(:), pointer   :: work3
 type(esmf_logical)                              :: lslag_1


 contains

 subroutine addparametertostate(state, int_state, rc)

 type(esmf_state),                 intent(inout) :: state
 type(gfs_internalstate), pointer, intent(in)    :: int_state
 integer, optional,                intent(out)   :: rc


 integer                                         :: i, j, i1
 integer                                         :: dimg
 integer                                         :: rc1, rcfinal
!integer                                         :: xxxxx

 logical first
 data first /.true./
 save first

 rc1     = esmf_success
 rcfinal = esmf_success

! one by one add the parameters to the gfs esmf export state.
!------------------------------------------------------------
 call esmf_attributeset(state, 'jcap', jcap, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "add jcap to the gfs export state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when adding jcap to the gfs export state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeset(state, 'ls_dim', ls_dim, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "add ls_dim to the gfs export state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when adding ls_dim to the gfs export state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeset(state, 'ls_max_node', ls_max_node, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "add ls_max_node to the gfs export state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when adding ls_max_node to the gfs export state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeset(state, 'mpi_r_mpi', mpi_r_mpi, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "add mpi_r_mpi to the gfs export state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when adding mpi_r_mpi to the gfs export state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeset(state, 'levs', levs, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "add levs to the gfs export state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when adding levs to the gfs export state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeset(state, 'levh', levh, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "add levh to the gfs export state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when adding levh to the gfs export state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeset(state, 'latr', latr, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "add latr to the gfs export state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when adding latr to the gfs export state, rc =', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeset(state, 'nodes_comp', nodes, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "add nodes_comp to the gfs export state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when adding nodes_comp to the gfs export state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeset(state, 'me_comp', me, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "add me_comp to the gfs export state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when adding me_comp to the gfs export state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeset(state, 'mc_comp', mc_comp, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "add mc_comp to the gfs export state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when adding mc_comp to the gfs export state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeset(state, 'latl2', latr2, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "add latl2 to the gfs export state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when adding latl2 to the gfs export state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeset(state, 'nvars', lots, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "add nvars to the gfs export state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when adding nvars to the gfs export state, rc = ', rc1
         rc1 = esmf_success
     end if

 if(int_state%lslag) then
     lslag_1 = esmf_true
 else
     lslag_1 = esmf_false
 end if

 call esmf_attributeset(state, 'lslag', lslag_1, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "add lslag to the gfs export state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when adding lslag to the gfs export state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeset(state, 'latdims', lats_dim_r, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "add latdims to the gfs export state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when adding latdims to the gfs export state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeset(state, 'lat1s', jcap + 1, lat1s_r, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "add lat1s to the gfs export state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when adding lat1s to the gfs export state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeset(state, 'latl', latr, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "add latl to the gfs export state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when adding latl to the gfs export state, rc = ', rc1
         rc1 = esmf_success
     end if

 dimg = 0

 call esmf_attributeset(state, 'lats_node', lats_node_r, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "add lats_node to the gfs export state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when adding lats_node to the gfs export state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeset(state, 'ipt_lats_node', ipt_lats_node_r, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "add ipt_lats_node to the gfs export state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when adding ipt_lats_node to the gfs export state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeset(state, 'lon_dims', latr, lon_dims_r, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "add lon_dims to the gfs export state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when adding lon_dims to the gfs export state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeset(state, 'lons_lat', latr, int_state%lonsperlar, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "add lons_lat to the gfs export state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when adding lons_lat to the gfs export state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeset(state, 'londi', lonrx, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "add londi to the gfs export state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when adding londi to the gfs export state, rc = ', rc1
         rc1 = esmf_success
     end if


 if(first) allocate(work1a(len_trie_ls * latg2))

 i1 = 0
 do j = 1, latg2
     do i = 1, len_trie_ls
         i1 = i1 + 1
         work1a(i1) = int_state%plnev_r(i, j)
     end do
 end do

 call esmf_attributeset(state, 'plnev', len_trie_ls * latg2, work1a, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "add plnev to the gfs export state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when adding plnev to the gfs export state, rc = ', rc1
         rc1 = esmf_success
     end if
 deallocate(work1a)

 if(first) allocate(work1b(len_trio_ls * latg2))

 i1 = 0
 do j = 1, latg2
     do i = 1, len_trio_ls
         i1 = i1 + 1
         work1b(i1) = int_state%plnod_r(i, j)
     end do
 end do

 call esmf_attributeset(state, 'plnod', len_trio_ls * latg2, work1b, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "add plnod to the gfs export state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when adding plnod to the gfs export state, rc = ', rc1
         rc1 = esmf_success
     end if
 deallocate(work1b)

 if(first) allocate(work1c(len_trie_ls * latg2))

 i1 = 0
 do j = 1, latg2
     do i = 1, len_trie_ls
         i1 = i1 + 1
         work1c(i1) = int_state%plnew_r(i, j)
     end do
 end do

 call esmf_attributeset(state, 'plnew', len_trie_ls * latg2, work1c, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "add plnew to the gfs export state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when adding plnew to the gfs export state, rc = ', rc1
         rc1 = esmf_success
     end if
 deallocate(work1c)

 if(first) allocate(work1d(len_trio_ls * latg2))

 i1 = 0
 do j = 1, latg2
     do i = 1, len_trio_ls
         i1 = i1 + 1
         work1d(i1) = int_state%plnow_r(i, j)
     end do
 end do

 call esmf_attributeset(state, 'plnow', len_trio_ls * latg2, work1d, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "add plnow to the gfs export state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when adding plnow to the gfs export state, rc = ', rc1
         rc1 = esmf_success
     end if
 deallocate(work1d)

 call esmf_attributeset(state, 'ls_node', ls_dim * 3, int_state%ls_node, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "add ls_node to the gfs export state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when adding ls_node to the gfs export state, rc = ', rc1
         rc1 = esmf_success
     end if

 if(first) allocate(work3(ls_dim * nodes))
 
 i1 = 0
 do j = 1, nodes
     do i = 1, ls_dim
         i1 = i1 + 1
         work3(i1) = int_state%ls_nodes(i, j)
     end do
 end do

 call esmf_attributeset(state, 'ls_nodes', ls_dim * nodes, work3, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "add ls_nodes to the gfs export state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when adding ls_nodes to the gfs export state, rc = ', rc1
         rc1 = esmf_success
     end if
 deallocate(work3)

 call esmf_attributeset(state, 'max_ls_nodes', nodes, int_state%max_ls_nodes, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "add max_ls_nodes to the gfs export state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when adding max_ls_nodes to the gfs export state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeset(state, 'lats_nodes', nodes, int_state%lats_nodes_r, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "add lats_nodes to the gfs export state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when adding lats_nodes to the gfs export state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeset(state, 'global_lats', latr + dimg, int_state%global_lats_r, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "add global_lats to the gfs export state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when adding global_lats to the gfs export state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeset(state, 'epsedn', len_trie_ls, int_state%epsedn, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "set epsedn to the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when setting epsedn to the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeset(state, 'epsodn', len_trio_ls, int_state%epsodn, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "set epsodn to the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when setting epsodn to the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeset(state, 'epse', len_trie_ls, int_state%epse, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "set epse to the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when setting epse to the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeset(state, 'epso', len_trio_ls, int_state%epso, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "set epso to the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when setting epso to the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeset(state, 'snnp1ev', len_trie_ls, int_state%snnp1ev, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "set snnp1ev to the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when setting snnp1ev to the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeset(state, 'snnp1od', len_trio_ls,int_state%snnp1od, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "set snnp1od to the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when setting snnp1od to the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if
 
 if(first) first = .false.

 if(rcfinal /= esmf_success) then
     print*, "fail: gfs_addparametertostatemod.f"
!else
!    print*, "pass: gfs_addparametertostatemod.f"
 end if

 if(present(rc)) then
     rc = rcfinal
 end if

 end subroutine addparametertostate

 end module gfs_addparametertostatemod
