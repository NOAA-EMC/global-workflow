 subroutine gettotalmember_ensembleruntime(total_member, &
                                           ens_sps,      &
                                           pe_member,    &
                                           hh_increase,  &
                                           hh_start,     &
                                           hh_final,     &
                                           rc)

 use esmf_mod

 implicit none

 integer,                        intent(out) :: total_member
 integer, dimension(50),         intent(out) :: pe_member
 integer,                        intent(out) :: hh_increase
 integer,                        intent(out) :: hh_start
 integer,                        intent(out) :: hh_final
 integer,                        intent(out) :: rc
 logical,                        intent(out) :: ens_sps
 type(esmf_vm)                               :: vm
 type(esmf_config)                           :: cf
 character(esmf_maxstr)                      :: cf_fname
 character(12)                               :: pelab
 integer                                     :: tasks
 integer                                     :: i

 call esmf_vmgetglobal(vm, rc = rc)
 call esmf_vmget(vm, petcount = tasks, rc = rc)

 rc       = esmf_success
 cf       = esmf_configcreate(rc = rc)
 cf_fname = 'gfs_namelist.rc'

 call esmf_configloadfile(cf, cf_fname, rc = rc)

 call esmf_configgetattribute(cf,                      &
                              total_member,            &
                              label = 'total_member:', &
                              rc    = rc)
 call esmf_configgetattribute(cf,                      &
                              ens_sps,                 &
                              label = 'ens_sps:',      &
                              rc    = rc)

 pe_member = 0
 do i = 1, total_member
   write(pelab,'("pe_member",i2.2,":")') i
   call esmf_configgetattribute(cf, pe_member(i), label = pelab, rc = rc)
   if (pe_member(i) == 0) pe_member(i) = tasks / total_member
 enddo

 call esmf_configgetattribute(cf,                      &
                              hh_increase,             &
                              label = 'hh_increase:',  &
                              rc    = rc)

 call esmf_configgetattribute(cf,                      &
                              hh_start,                &
                              label = 'hh_start:',     &
                              rc    = rc)

 call esmf_configgetattribute(cf,                      &
                              hh_final,                &
                              label = 'hh_final:',     &
                              rc    = rc)

 call esmf_configdestroy(cf, rc = rc)

 end subroutine gettotalmember_ensembleruntime



 subroutine setgcstatenames(gridcompname, &
                            cplcompname,  &
                            impstatename, &
                            expstatename, &
                            impgefsname,  &
                            expgefsname,  &
                            total_member)

 use esmf_mod

 implicit none

 integer,                                         intent(in)  :: total_member
 character(esmf_maxstr), dimension(total_member), intent(out) :: gridcompname
 character(esmf_maxstr),                          intent(out) :: cplcompname
 character(esmf_maxstr),                          intent(out) :: impstatename
 character(esmf_maxstr),                          intent(out) :: expstatename
 character(esmf_maxstr),                          intent(out) :: impgefsname
 character(esmf_maxstr),                          intent(out) :: expgefsname

 integer :: i

 write(cplcompname,  1000)
 write(impstatename, 2000)
 write(expstatename, 3000)
 write(impgefsname,  4000)
 write(expgefsname,  5000)

 do i = 1, total_member
     write(gridcompname(i), 6000) i
 end do

1000 format('gfs coupler grid component name')
2000 format('gfs import state')
3000 format('gfs export state')
4000 format('gefs import state')
5000 format('gefs export state')
6000 format('gfs grid component', i2.2)

 end subroutine setgcstatenames



 subroutine gridcompcreate(vm, gcgfs, cplgefs, gridcompname, cplcompname, cf,   &
                           total_member, pe_member, member_id, ens_sps, liope, ens_comp_pe, rc)

 use esmf_mod

 implicit none

 integer, intent(in) :: total_member, pe_member(total_member)
!
 type(esmf_vm),                                   intent(inout) :: vm    ! the esmf virtual machine.
 type(esmf_config),                               intent(inout) :: cf    ! esmf config
 type(esmf_gridcomp),    dimension(total_member), intent(out)   :: gcgfs
 type(esmf_cplcomp),                              intent(out)   :: cplgefs
 character(esmf_maxstr), dimension(total_member), intent(in)    :: gridcompname
 character(esmf_maxstr),                          intent(in)    :: cplcompname
 logical,                                         intent(in)    :: ens_sps
 logical,                                         intent(in)    :: liope
 logical,                                         intent(out)   :: ens_comp_pe
 integer,                                         intent(out)   :: rc, member_id

 integer, pointer :: petlist(:, :), petlist_cpl(:)
 integer          :: npe, me, i, pe_max, npe_cpl

 rc = esmf_success

!  get the number of provided tasks.
!-----------------------------------
 call esmf_vmget(vm, petcount = npe, localpet = me, rc = rc)

!
 pe_max = 1
 do i=1,total_member
  pe_max = max(pe_max,pe_member(i))
 enddo

!  set up the pet list.
!----------------------
 allocate(petlist(pe_max, total_member))
 allocate(petlist_cpl(npe))

! create the the pet list and the local communicator for each ensemble member.
!-----------------------------------------------------------------------------
 call setup_member_communicator(petlist, petlist_cpl, total_member, pe_member,  &
                                npe, pe_max, member_id, me, liope,              &
                                ens_comp_pe, npe_cpl)
 do i = 1, total_member
    gcgfs(i) = esmf_gridcompcreate (                                         &
                                 name         = gridcompname(i),             &
                                 gridcomptype = esmf_atm,                    &
                                 petlist      = petlist(1:pe_member(i), i),  &
                                 config       = cf,                          &
                                 rc           = rc)
 end do

 if(ens_sps) then
    cplgefs = esmf_cplcompcreate(name         = cplcompname,                 &
                                 petlist      = petlist_cpl(1:npe_cpl),      &
                                 rc           = rc)
 end if
end subroutine gridcompcreate


subroutine setup_member_communicator(petlist, petlist_cpl, total_member, pe_member,  &
                                     npe, pe_max, member_id, me, liope,              &
                                     ens_comp_pe, npe_cpl)
!
implicit none

 integer, intent(in) :: total_member, pe_max, pe_member(total_member)
 integer, dimension(pe_max, total_member), intent(out) :: petlist
 integer, dimension(npe),                  intent(out) :: petlist_cpl
 integer,                                  intent(out) :: member_id, npe_cpl
 integer,                                  intent(in)  :: me, npe
 logical,                                  intent(in)  :: liope
 logical,                                  intent(out) :: ens_comp_pe

 integer :: i, j, i1, i2, i3, i4

! this part is for [(comp_tasks + i/o task) * total_member]. weiyu.
!------------------------------------------------------------------
 i1 = 0
 i2 = 0
 ens_comp_pe = .false.
 do j = 1, total_member
     do i = 1, pe_member(j)
         if(liope) then
             if(i /= pe_member(j)) then
                 if(me == i1) ens_comp_pe = .true.
                 i2 = i2 + 1
                 petlist_cpl(i2) = i1
             end if
         else
             if(me == i1) ens_comp_pe = .true.
             i2 = i2 + 1
             petlist_cpl(i2) = i1
         end if
         petlist(i, j) = i1
         if(me == i1) then
             member_id = j
         end if
         i1 = i1+1
     end do
 end do
 npe_cpl = i2
!------------------------------------------------------------------





! this part is for [comp_tasks * total_member + i/o task * total_member]. weiyu.
! assume all pe_member are same for every ensemble member.
!-------------------------------------------------------------------------------
! ens_comp_pe = .false.
! i1 = 0
! i2 = 0
! i2 = 0
! i4 = total_member * (pe_member(1) - 1)
! do j = 1, total_member
!     if(liope) then
!         i3 = pe_member(j) - 1
!     else
!         i3 = pe_member(j)
!     end if
!     do i = 1, i3
!         i2 = i2 + 1
!         petlist_cpl(i2) = i1
!         petlist(i, j) = i1
!         if(me == i1) then
!             member_id = j
!             ens_comp_pe = .true.
!         end if
!         i1 = i1+1
!     end do
!
!     if(liope) then
!         petlist(pe_member(j), j) = i4
!         if(me == i4) then
!             member_id = j
!         end if
!         i4 = i4 + 1
!     end if
! end do
! npe_cpl = i2

!if(me==0) then
!     do j = 1, total_member
!         print*,'in set up, j, petlist=',j, petlist(:,j)
!     end do
!     print*,'in set up, pe_member=', pe_member(1:total_member)
!     print*,'in set up, liope=', liope
!     print*,'in set up, petlist_cpl=', petlist_cpl
!     print*,'in set up, me, member_id, ens_comp_pe, npe_cpl=', &
!         me, member_id, ens_comp_pe, npe_cpl
!end if
 end subroutine setup_member_communicator
