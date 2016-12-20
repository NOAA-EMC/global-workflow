 subroutine gefs_bcst_global(var, peid, rc)

!----------------------------------------------------------------------
! subroutine bcst_global
!
! this subroutine broadcasts the inputted variable var to all pes and all
! ensemble members.  and output contains all related index parameters.
!
! description: vm broadcast tool software.
!
! revision history:
!
!  setpember 2007     weiyu yang initial code.
!
!
! interface:
!   
!   var    -- inputted single variable.
!   peid   -- pe id of var.
!   vm     -- the sttp coupler esmf vm.
!
 use esmf_mod
 use machine

 real(kind = kind_evod)                    :: var 
 real(esmf_kind_r8), dimension(:), pointer :: var_work 
 integer                                   :: peid
 type(esmf_vm)                             :: vm
 integer                                   :: rc

 rc = esmf_success

 call esmf_vmgetcurrent(vm, rc = rc)

 if(esmf_logmsgfounderror(rc, 'vmgetcurrent error')) then
     print*, 'error happened when getting the current vm, peid, rc=', &
          peid, rc
 end if

 allocate(var_work(1))

 var_work(1) = var

 call esmf_vmbroadcast(vm, var_work, 1, peid, rc = rc) 

 if(esmf_logmsgfounderror(rc, 'vm broadcast error')) then
     print*, 'error happened when vm broadcasting, peid, rc=', &
          peid, rc
 end if

 var = var_work(1)

 deallocate(var_work)

 end subroutine gefs_bcst_global





 subroutine gefs_bcst_global_i4(var, peid, rc)

!----------------------------------------------------------------------
! subroutine bcst_global_i4
!
! this subroutine broadcasts the inputted variable var to all pes and all
! ensemble members.  and output contains all related index parameters.
!
! description: vm broadcast tool software.
!
! revision history:
!
!  setpember 2007     weiyu yang initial code.
!
!
! interface:
!   
!   var    -- inputted single variable.
!   peid   -- pe id of var.
!   vm     -- the sttp coupler esmf vm.
!
 use esmf_mod
 use machine

 integer                                   :: var 
 real(esmf_kind_i4), dimension(:), pointer :: var_work 
 integer                                   :: peid
 type(esmf_vm)                             :: vm
 integer                                   :: rc

 rc = esmf_success

 call esmf_vmgetcurrent(vm, rc = rc)

 if(esmf_logmsgfounderror(rc, 'vmgetcurrent error')) then
     print*, 'error happened when getting the current vm, peid, rc=', &
          peid, rc
 end if

 allocate(var_work(1))

 var_work(1) = var

 call esmf_vmbroadcast(vm, var_work, 1, peid, rc = rc) 

 if(esmf_logmsgfounderror(rc, 'vm broadcast error')) then
     print*, 'error happened when vm broadcasting, peid, rc=', &
          peid, rc
 end if

 var = var_work(1)

 deallocate(var_work)

 end subroutine gefs_bcst_global_i4
