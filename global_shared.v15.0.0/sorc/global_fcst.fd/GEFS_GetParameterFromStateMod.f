!----------------------------------------------------------------------
! !module: gefs_getparameterfromstatemod
!        --- get required parameters from the gefs coupler esmf import state
!            for the ensemble coupler to do the spectral transform
!            for the stochastic perturbation scheme, the second step.
!
! !description: get all required parameters from the gefs cpl esmf import state.
!
! !revision history:
!
!  may      2007     weiyu yang initial code.
!
!
! !interface:
!

 module gefs_getparameterfromstatemod

 use esmf_mod
 use gefs_cpl_internalstate_esmfmod

 implicit none

 contains

 subroutine gefs_getparameterfromstate(state, int_state, rc)

 type(esmf_state),                      intent(inout) :: state
 type(gefs_cpl_internalstate), pointer, intent(inout) :: int_state
 integer, optional,                     intent(out)   :: rc

 real(kind = kind_evod), dimension(:), pointer        :: work1
 integer,                dimension(:), pointer        :: work3

 integer                                              :: i, j, i1
 integer                                              :: rc1, rcfinal

 rc1     = esmf_success
 rcfinal = esmf_success

! one by one get the parameters from the gfs esmf export state.
!--------------------------------------------------------------
 call esmf_attributeget(state, 'jcap', int_state%jcap, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get jcap from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting jcap from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeget(state, 'ls_dim', int_state%ls_dim, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get ls_dim from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting ls_dim from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeget(state, 'ls_max_node', int_state%ls_max_node, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get ls_max_node from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting ls_max_node from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeget(state, 'mpi_r_mpi', int_state%mpi_r_mpi, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get mpi_r_mpi from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting mpi_r_mpi from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeget(state, 'levs', int_state%levs, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get levs from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting levs from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeget(state, 'levh', int_state%levh, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get levh from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting levh from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeget(state, 'latr', int_state%latr, rc = rc1)
print*,'in gefs_getpara,latr=',int_state%latr
     if(esmf_logmsgfoundallocerror(rc1, "get latr from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting latr from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeget(state, 'nodes_comp', int_state%nodes_comp, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get nodes_comp from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting nodes_comp from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeget(state, 'me_comp', int_state%me_comp, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get me_comp from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting me_comp from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeget(state, 'mc_comp', int_state%mc_comp, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get mc_comp from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting mc_comp from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeget(state, 'latl2', int_state%latl2, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get latl2 from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting latl2 from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeget(state, 'nvars', int_state%nvars, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get nvars from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting nvars from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeget(state, 'lslag', int_state%lslag_1, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get lslag from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting lslag from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 if(int_state%lslag_1 == esmf_true) then
     int_state%lslag = .true.
 else
     int_state%lslag = .false.
 end if

 call esmf_attributeget(state, 'latdims', int_state%latdims, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get latdims from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting latdims from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 nullify(int_state%lat1s)
 allocate(int_state%lat1s(0 : int_state%jcap))

 call esmf_attributeget(state, 'lat1s', int_state%jcap + 1, int_state%lat1s, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get lat1s from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting lat1s from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeget(state, 'latl', int_state%latl, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get latl from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting latl from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 int_state%dimg = 0

 call esmf_attributeget(state, 'lats_node', int_state%lats_node, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get lats_node from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting lats_node from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeget(state, 'ipt_lats_node', int_state%ipt_lats_node, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get ipt_lats_node from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting ipt_lats_node from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 nullify(int_state%lon_dims)
 allocate(int_state%lon_dims(int_state%latr))
 
 call esmf_attributeget(state, 'lon_dims', int_state%latr, int_state%lon_dims, rc = rc1)
print*,'in gefs_getpara,lon_dims=',int_state%lon_dims(1:5),int_state%lon_dims(50:55)
     if(esmf_logmsgfoundallocerror(rc1, "get lon_dims from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting lon_dims from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 nullify(int_state%lons_lat)
 allocate(int_state%lons_lat(int_state%latl))

 call esmf_attributeget(state, 'lons_lat', int_state%latl, int_state%lons_lat, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get lons_lat from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting lons_lat from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 call esmf_attributeget(state, 'londi', int_state%londi, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get londi from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting londi from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 allocate(work1(int_state%trie_ls_size * int_state%latl2))
 nullify (int_state%plnev)
 allocate(int_state%plnev(int_state%trie_ls_size, int_state%latl2))

 call esmf_attributeget(state, 'plnev', int_state%trie_ls_size * int_state%latl2, &
     work1, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get plnev from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting plnev from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 i1 = 0
 do j = 1, int_state%latl2
     do i = 1, int_state%trie_ls_size
         i1 = i1 + 1
         int_state%plnev(i, j) = work1(i1)
     end do
 end do

 deallocate(work1)

 allocate(work1(int_state%trio_ls_size * int_state%latl2))
 nullify (int_state%plnod)
 allocate(int_state%plnod(int_state%trio_ls_size, int_state%latl2))

 call esmf_attributeget(state, 'plnod', int_state%trio_ls_size * int_state%latl2, &
     work1, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get plnod from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting plnod from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 i1 = 0
 do j = 1, int_state%latl2
     do i = 1, int_state%trio_ls_size
         i1 = i1 + 1
         int_state%plnod(i, j) = work1(i1)
     end do
 end do

 deallocate(work1)

 allocate(work1(int_state%trie_ls_size * int_state%latl2))
 nullify (int_state%plnew)
 allocate(int_state%plnew(int_state%trie_ls_size, int_state%latl2))

 call esmf_attributeget(state, 'plnew', int_state%trie_ls_size * int_state%latl2, &
     work1, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get plnew from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting plnew from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 i1 = 0
 do j = 1, int_state%latl2
     do i = 1, int_state%trie_ls_size
         i1 = i1 + 1
         int_state%plnew(i, j) = work1(i1)
     end do
 end do

 deallocate(work1)

 allocate(work1(int_state%trio_ls_size * int_state%latl2))
 nullify (int_state%plnow)
 allocate(int_state%plnow(int_state%trio_ls_size, int_state%latl2))

 call esmf_attributeget(state, 'plnow', int_state%trio_ls_size * int_state%latl2, &
     work1, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get plnow from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting plnow from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 i1 = 0
 do j = 1, int_state%latl2
     do i = 1, int_state%trio_ls_size
         i1 = i1 + 1
         int_state%plnow(i, j) = work1(i1)
     end do
 end do

 deallocate(work1)

 allocate(work3(int_state%ls_dim * 3))
 nullify (int_state%ls_node)
 allocate(int_state%ls_node(int_state%ls_dim, 3))

 call esmf_attributeget(state, 'ls_node', int_state%ls_dim * 3, work3, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get ls_node from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting ls_node from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 i1 = 0
 do j = 1, 3
     do i = 1, int_state%ls_dim
         i1 = i1 + 1
         int_state%ls_node(i, j) = work3(i1)
     end do
 end do

 deallocate(work3)


 allocate(work3(int_state%ls_dim * int_state%nodes_comp))
 nullify (int_state%ls_nodes)
 allocate(int_state%ls_nodes(int_state%ls_dim, int_state%nodes_comp))
 
 call esmf_attributeget(state, 'ls_nodes', int_state%ls_dim * int_state%nodes_comp, &
     work3, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get ls_nodes from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting ls_nodes from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 i1 = 0
 do j = 1, int_state%nodes_comp
     do i = 1, int_state%ls_dim
         i1 = i1 + 1
         int_state%ls_nodes(i, j) = work3(i1)
     end do
 end do

 deallocate(work3)

 nullify (int_state%max_ls_nodes)
 allocate(int_state%max_ls_nodes(int_state%nodes_comp))

 call esmf_attributeget(state, 'max_ls_nodes', int_state%nodes_comp, &
     int_state%max_ls_nodes, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get max_ls_nodes from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting max_ls_nodes from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 nullify (int_state%lats_nodes)
 allocate(int_state%lats_nodes(int_state%nodes_comp))

 call esmf_attributeget(state, 'lats_nodes', int_state%nodes_comp, &
     int_state%lats_nodes, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get lats_nodes from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting lats_nodes from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 nullify (int_state%global_lats)
 allocate(int_state%global_lats(int_state%latl + int_state%dimg))

 call esmf_attributeget(state, 'global_lats', int_state%latl + int_state%dimg, &
     int_state%global_lats, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get global_lats from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting global_lats from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 nullify (int_state%epsedn)
 allocate(int_state%epsedn(int_state%trie_ls_size))

 call esmf_attributeget(state, 'epsedn', int_state%trie_ls_size, &
     int_state%epsedn, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get epsedn from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting epsedn from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 nullify (int_state%epsodn)
 allocate(int_state%epsodn(int_state%trio_ls_size))

 call esmf_attributeget(state, 'epsodn', int_state%trio_ls_size, &
     int_state%epsodn, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get epsodn from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting epsodn from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 nullify (int_state%epse)
 allocate(int_state%epse(int_state%trie_ls_size))

 call esmf_attributeget(state, 'epse', int_state%trie_ls_size, &
     int_state%epse, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get epse from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting epse from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 nullify (int_state%epso)
 allocate(int_state%epso(int_state%trio_ls_size))

 call esmf_attributeget(state, 'epso', int_state%trio_ls_size, &
     int_state%epso, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get epso from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting epso from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 nullify (int_state%snnp1ev)
 allocate(int_state%snnp1ev(int_state%trie_ls_size))

 call esmf_attributeget(state, 'snnp1ev', int_state%trie_ls_size, &
     int_state%snnp1ev, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get snnp1ev from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting snnp1ev from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 nullify (int_state%snnp1od)
 allocate(int_state%snnp1od(int_state%trio_ls_size))

 call esmf_attributeget(state, 'snnp1od', int_state%trio_ls_size, &
     int_state%snnp1od, rc = rc1)

     if(esmf_logmsgfoundallocerror(rc1, "get snnp1od from the gefs cpl import state.")) then
         rcfinal = esmf_failure
         print*, 'error happened when getting snnp1od from the gefs cpl import state, rc = ', rc1
         rc1 = esmf_success
     end if

 if(rcfinal /= esmf_success) then
     print*, "fail: gefs_getparameterfromstatemod.f"
!else
!    print*, "pass: gefs_getparameterfromstatemod.f"
 end if

 if(present(rc)) then
     rc = rcfinal
 end if

 end subroutine gefs_getparameterfromstate

 end module gefs_getparameterfromstatemod

