 subroutine gefs_sto_per_scheme_step2(int_state, zeros03, uses1, griduse,       &
                 qadjust, rs_global, jul_day,  kemax, nreg,slat1, slat2, rc)
!
!dhou, 10/17/2007  added arguments 7 and 8, i.e. kemax(nreg) and nreg for regional rescaling
!dhou, 10/17/2007  added argument 6 i.e. rs_global for global rescaling factor 
!dhou  09/11/2007  added arguments
!zeros03, 0=no, 1=yes; zero out arrays gz, dpdlam, dpdphi, uln, and vln as well as state 3
!s1use, 0=zero-out, 1=no-change, 2=replaced with s2, 3=replace s1 and s2 with 0.5(s1+s2).
!           for arrays related to state 1,zem to qm.
!griduse, 1=s1, 2=s2; convert the state s1/s2 from spectral to grid and  and back to spec.
!qadjust, 0=no, 1=yes; adjust the q array so that q>=0 everywhere.

! this subroutine is used to compute the second step of the
! stochastic perturbation scheme, in which it carries out the spectral 
! transform computation into the gaussian grid space arrays, then
! computes the second step of the stochastic perturbation scheme that
! considering the local weighting influences.
!---------------------------------------------------------------------

! !revision history:
!
!  may 2007       weiyu yang initial code for wave-grid conversion for model state .
!  nov 2007       dingchen hou ddopted the code for global/regional rescaling as well as conversion, for model state
!                 or its perturbation.    
!-----------------------------------------

 use esmf_mod
 use gefs_cpl_internalstate_esmfmod
 use machine,  only: kind_evod, kind_phys, kind_rad
 use physcons, only : pi => con_pi, fv => con_fvirt, rerth => con_rerth

!dhou 04/16/2012 moved this line for zeus (following gw?) 
 implicit none
 include 'mpif.h'

 type(gefs_cpl_internalstate), intent(inout) :: int_state
 integer,                      intent(out)   :: rc
 integer                                     :: zeros03,uses1,griduse,qadjust,lshift
 integer,                      intent(in)    :: jul_day
 real(kind = kind_evod)                      :: rs_global,rs
 integer,                      intent(in )   :: nreg
 real(kind = kind_evod)                      :: parm1,parm2,parm3
 integer                                     :: ireg,k500
 real(kind = kind_evod)                      :: kemax(nreg)
 real(kind = kind_evod)                      :: ker(3,15)
 real(kind = kind_evod)                      :: slat1,slat2

 type(esmf_vm)                               :: vm_esmf
 integer                                     :: i, j, k, l, mem
 integer                                     :: ksd, ksplam, kspphi
 integer                                     :: ksq, ksr,    kst
 integer                                     :: ksu, ksv,    ksz
 integer                                     :: kso, ksc
 integer                                     :: lan, lat,    lon_dim
 integer                                     :: lon_lat

 real(kind = kind_evod)                      :: atem, keavg

 real(kind = kind_evod)                      :: vorm, divm, tm
 real(kind = kind_evod)                      :: qm,   ozm,  clwm
 real(kind = kind_evod)                      :: um,   vm
 real(kind = kind_evod)                      :: psm,  dpdlamm, dpdphim

 real(kind = kind_evod)                      :: vors, divs, ts
 real(kind = kind_evod)                      :: qs,   ozs,  clws 
 real(kind = kind_evod)                      :: us,   vs
 real(kind = kind_evod)                      :: pss,  dpdlams, dpdphis

 real(kind = kind_rad)                       :: qmin
!real(kind = kind_phys)                      :: fv
!real(kind = kind_phys)                      :: rerth


 integer                                     :: rc1
 integer                                     :: rcfinal

 parameter(qmin  = 1.0e-10)
!parameter(fv    = 4.6150e+2 / 2.8705e+2 - 1.0)
!parameter(rerth = 6.3712e+6)

 rc1     = esmf_success
 rcfinal = esmf_success

 call esmf_vmgetcurrent(vm_esmf, rc=rc1)
 ksz     = 0 * int_state%levs + 0 * int_state%levh + 1
 ksd     = 1 * int_state%levs + 0 * int_state%levh + 1
 kst     = 2 * int_state%levs + 0 * int_state%levh + 1
 ksr     = 3 * int_state%levs + 0 * int_state%levh + 1
 kso     = 4 * int_state%levs + 0 * int_state%levh + 1
 ksc     = 5 * int_state%levs + 0 * int_state%levh + 1
 ksq     = 3 * int_state%levs + 1 * int_state%levh + 1
 ksplam  = 3 * int_state%levs + 1 * int_state%levh + 2
 kspphi  = 3 * int_state%levs + 1 * int_state%levh + 3
 ksu     = 3 * int_state%levs + 1 * int_state%levh + 4
 ksv     = 4 * int_state%levs + 1 * int_state%levh + 4

 if (.not.associated(int_state%trie_ls))                                               & 
 allocate(int_state%trie_ls(int_state%trie_ls_size, 2, int_state%trieo_vertical_size))
 if (.not.associated(int_state%trio_ls))                                               & 
 allocate(int_state%trio_ls(int_state%trio_ls_size, 2, int_state%trieo_vertical_size))

!split trieo into trie and trio arrays
 k = 1
 do l = 1, int_state%trieo_vertical_size
     do j = 1, 2
         do i = 1, int_state%trie_ls_size
             int_state%trie_ls( i, j, l) = int_state%trieo_ls_max(k)
             k = k + 1
         end do
         do i = 1, int_state%trio_ls_size
             int_state%trio_ls( i, j, l) = int_state%trieo_ls_max(k)
             k = k + 1
         end do
     end do
 end do
   
!print *, "in the ensemble coupler, step2, gz ",                  &
!int_state%trio_ls(1, 1, int_state%p_gz),int_state%trie_ls(1, 1, int_state%p_gz),int_state%p_gz
!print *, "in the ensemble coupler, step2, gz ",                  &
!int_state%trio_ls(1, 2, int_state%p_gz),int_state%trie_ls(1, 2, int_state%p_gz),int_state%p_gz

! zero out s0 arrays --- arrays not related to the model state
if (zeros03 .eq. 1) then
! state 0, gs array
 do l = int_state%p_gz, int_state%p_gz  
     do j = 1, 2
         do i = 1, int_state%trie_ls_size
             int_state%trie_ls( i, j, l) = 0.0
         end do
         do i = 1, int_state%trio_ls_size
             int_state%trio_ls( i, j, l) = 0.0 
         end do
     end do
 end do
! state 0, arrays dpdlam, dpdphi, uln and vln
 do l = int_state%p_dlam, int_state%p_w - 1 
     do j = 1, 2
         do i = 1, int_state%trie_ls_size
             int_state%trie_ls( i, j, l) = 0.0
         end do
         do i = 1, int_state%trio_ls_size
             int_state%trio_ls( i, j, l) = 0.0 
         end do
     end do
 end do
! state 3
 do l = int_state%p_w, int_state%p_zq  
     do j = 1, 2
         do i = 1, int_state%trie_ls_size
             int_state%trie_ls( i, j, l) = 0.0
         end do
         do i = 1, int_state%trio_ls_size
             int_state%trio_ls( i, j, l) = 0.0 
         end do
     end do
 end do
endif

! zero out s1 arrays  (model state 1, for t-1)
if (uses1 .lt. 0 .or. uses1 .gt. 3 ) then
  print *, 'invalid value of uses1=', uses1, 'forced stop, check step2'
  stop
endif
if (uses1 .eq. 0) then
 do l = int_state%p_zem, int_state%p_qm  
     do j = 1, 2
         do i = 1, int_state%trie_ls_size
             int_state%trie_ls( i, j, l) = 0.0
         end do
         do i = 1, int_state%trio_ls_size
             int_state%trio_ls( i, j, l) = 0.0 
         end do
     end do
 end do
endif

!dhou 09/06/2007 shift the arrays to process the t-1 time level instead of t level

if (griduse .lt. 1 .or. griduse .gt. 2 ) then
  print *, 'invalid value of griduse=', griduse, 'forced stop, check step2'
  stop
endif
if (griduse .eq. 1 ) then
 lshift = int_state%p_q - int_state%p_qm  
 do l = int_state%p_ze, int_state%p_q 
     do j = 1, 2
         do i = 1, int_state%trie_ls_size
             atem = int_state%trie_ls( i, j, l) 
             int_state%trie_ls( i, j, l) = int_state%trie_ls(i, j, l-lshift)
             int_state%trie_ls(i, j, l-lshift) = atem
         end do
         do i = 1, int_state%trio_ls_size
             atem = int_state%trio_ls( i, j, l) 
             int_state%trio_ls( i, j, l) = int_state%trio_ls(i, j, l-lshift)
             int_state%trio_ls(i, j, l-lshift) = atem
         end do
     end do
 end do
endif

!start wave-grid conversion
 do k = 1, int_state%levs
     call gefs_dezouv(int_state%trie_ls(1, 1, int_state%p_di  + k - 1), &
                      int_state%trio_ls(1, 1, int_state%p_ze  + k - 1), &
                      int_state%trie_ls(1, 1, int_state%p_uln + k - 1), &
                      int_state%trio_ls(1, 1, int_state%p_vln + k - 1), &
                      int_state%epsedn,       int_state%epsodn,         &
                      int_state%snnp1ev,      int_state%snnp1od,        &
                      int_state%ls_node,      int_state%jcap,           &
                      int_state%trie_ls_size, int_state%trio_ls_size,   &
                      int_state%ls_dim,       int_state%ls_max_node,    &
                      rerth)

     call gefs_dozeuv(int_state%trio_ls(1, 1, int_state%p_di  + k - 1), &
                      int_state%trie_ls(1, 1, int_state%p_ze  + k - 1), &
                      int_state%trio_ls(1, 1, int_state%p_uln + k - 1), &
                      int_state%trie_ls(1, 1, int_state%p_vln + k - 1), &
                      int_state%epsedn,       int_state%epsodn,         &
                      int_state%snnp1ev,      int_state%snnp1od,        &
                      int_state%ls_node,      int_state%jcap,           &
                      int_state%trie_ls_size, int_state%trio_ls_size,   &
                      int_state%ls_dim,       int_state%ls_max_node,    &
                      rerth)
 end do

 int_state%dimg = 0

 if (.not.associated(int_state%four_gr1))                                               & 
 allocate(int_state%four_gr1(int_state%londi * int_state%nvars, int_state%latdims))
 if (.not.associated(int_state%four_gr2))                                               & 
 allocate(int_state%four_gr2(int_state%londi * int_state%nvars, int_state%latdims))

 call gefs_sumfln(int_state%trie_ls(1, 1, int_state%p_ze),             &
                  int_state%trio_ls(1, 1, int_state%p_ze),             &
                  int_state%lat1s,                                     &
                  int_state%plnev, int_state%plnod,                    &
                  int_state%nvars, int_state%ls_node, int_state%latl2, &
                  int_state%lslag, int_state%latdims, int_state%nvars, &
                  int_state%four_gr1,                                  &
                  int_state%ls_nodes,     int_state%max_ls_nodes,      &
                  int_state%lats_nodes,   int_state%global_lats,       &
                  int_state%lats_node,    int_state%ipt_lats_node,     &
                  int_state%lon_dims,     int_state%dimg,              &
                  int_state%lons_lat,     int_state%londi,             &
                  int_state%latl,         int_state%jcap,              &
                  int_state%trie_ls_size, int_state%trio_ls_size,      &
                  int_state%ls_dim,       int_state%ls_max_node,       &
                  int_state%mpi_r_mpi,    int_state%levs,              &
                  int_state%levh,         int_state%latr,              &
                  int_state%nodes_comp,   int_state%me_comp,           &
                  int_state%mc_comp)
!finished wave-grid conversion

!convert the grid array (from gr1 to gr2) for zonal processing---a number of latitude on each task
 do lan = 1, int_state%lats_node
     lat     = int_state%global_lats(int_state%ipt_lats_node - 1 + lan)
     lon_dim = int_state%lon_dims(lan)
     lon_lat = int_state%lons_lat(lat)

     call gefs_four2grid_thread(int_state%four_gr1(1, lan), int_state%four_gr2(1, lan), &
                           lon_dim, lon_lat, int_state%londi, int_state%nvars,     &
                           lan, int_state%me_comp)

! adjust for q<0 grid points.
! if do spectral transform back to the spectral space, the results will be 
! changed and cannot be identical to the original spectral t and q fields.
!-------------------------------------------------------------------------
if (qadjust .eq. 1) then
     do k = 1, int_state%levs
         do j = 1, lon_lat
             if(int_state%four_gr2(j + (ksr - 1) * lon_dim + lon_dim * (k - 1), lan) <= 0.0) &
                int_state%four_gr2(j + (ksr - 1) * lon_dim + lon_dim * (k - 1), lan) = qmin

             int_state%four_gr2(j + (kst - 1) * lon_dim + lon_dim * (k - 1), lan)=           &
                 int_state%four_gr2(j + (kst - 1) * lon_dim + lon_dim * (k - 1), lan) /      &
                 (1.0 + fv * int_state%four_gr2(j + (ksr - 1) * lon_dim + lon_dim * (k - 1), lan))
         end do
     end do
endif

! converting ln(ps * 0.1) to (ps * 0.1).
!----------------------------------------------------------------------
     do j = 1, lon_lat
          int_state%four_gr2(j + (ksq - 1) * lon_dim, lan) &
              = exp(int_state%four_gr2(j + (ksq - 1) * lon_dim, lan))
     end do
 end do

! put the gaussian fileds into each control variable arrays.
! first, allocate all control variable arrays if not done yet
!-----------------------------------------------------------
 if (.not.associated(int_state%vor))                                            & 
 allocate(int_state%vor   (int_state%londi, int_state%levs, int_state%latdims))
 if (.not.associated(int_state%div))                                            & 
 allocate(int_state%div   (int_state%londi, int_state%levs, int_state%latdims))
 if (.not.associated(int_state%t))                                            & 
 allocate(int_state%t     (int_state%londi, int_state%levs, int_state%latdims))
 if (.not.associated(int_state%q))                                            & 
 allocate(int_state%q     (int_state%londi, int_state%levs, int_state%latdims))
 if (.not.associated(int_state%oz))                                            & 
 allocate(int_state%oz    (int_state%londi, int_state%levs, int_state%latdims))
 if (.not.associated(int_state%clw))                                            & 
 allocate(int_state%clw   (int_state%londi, int_state%levs, int_state%latdims))
 if (.not.associated(int_state%u))                                            & 
 allocate(int_state%u     (int_state%londi, int_state%levs, int_state%latdims))
 if (.not.associated(int_state%v))                                            & 
 allocate(int_state%v     (int_state%londi, int_state%levs, int_state%latdims))
 if (.not.associated(int_state%ps))                                            & 
 allocate(int_state%ps    (int_state%londi, int_state%latdims))
 if (.not.associated(int_state%dpdlam))                                            & 
 allocate(int_state%dpdlam(int_state%londi, int_state%latdims))
 if (.not.associated(int_state%dpdphi))                                            & 
 allocate(int_state%dpdphi(int_state%londi, int_state%latdims))

! set up the maximum and minumum initial values for max/min etc.
!-----------------------------------------------
 vorm    = -1e20
 divm    = -1e20
 tm      = -1e20
 qm      = -1e20
 ozm     = -1e20
 clwm    = -1e20
 um      = -1e20
 vm      = -1e20
 psm     = -1e20
 dpdlamm = -1e20
 dpdphim = -1e20

 vors    =  1e20
 divs    =  1e20
 ts      =  1e20
 qs      =  1e20
 ozs     =  1e20
 clws    =  1e20
 us      =  1e20
 vs      =  1e20
 pss     =  1e20
 dpdlams =  1e20
 dpdphis =  1e20

 do j = 1, int_state%lats_node

! lat is the real global latitue index number (1 for n pole).
! lon_lat is the number of longitudes at the "lat" latitude. note that used grid is a reduce gaussian grid.
!--------------------------------------------------------
     lat     = int_state%global_lats(int_state%ipt_lats_node - 1 + j)
     lon_lat = int_state%lons_lat(lat)

     do k = 1, int_state%levs
         do i = 1, lon_lat
             lon_dim                = int_state%lon_dims(j)
             int_state%vor(i, k, j) = int_state%four_gr2(i + (ksz + k - 2) * lon_dim, j)
             int_state%div(i, k, j) = int_state%four_gr2(i + (ksd + k - 2) * lon_dim, j)
             int_state%t  (i, k, j) = int_state%four_gr2(i + (kst + k - 2) * lon_dim, j)
             int_state%q  (i, k, j) = int_state%four_gr2(i + (ksr + k - 2) * lon_dim, j)
             int_state%oz (i, k, j) = int_state%four_gr2(i + (kso + k - 2) * lon_dim, j)
             int_state%clw(i, k, j) = int_state%four_gr2(i + (ksc + k - 2) * lon_dim, j)
             int_state%u  (i, k, j) = int_state%four_gr2(i + (ksu + k - 2) * lon_dim, j)
             int_state%v  (i, k, j) = int_state%four_gr2(i + (ksv + k - 2) * lon_dim, j)

! calculate the maximum and minumum of each variable fields.
!-----------------------------------------------------------
             vorm = max(vorm, int_state%vor(i, k, j))
             divm = max(divm, int_state%div(i, k, j))
             tm   = max(tm,   int_state%t  (i, k, j))
             qm   = max(qm,   int_state%q  (i, k, j))
             ozm  = max(ozm,  int_state%oz (i, k, j))
             clwm = max(clwm, int_state%clw(i, k, j))
             um   = max(um,   int_state%u  (i, k, j))
             vm   = max(vm,   int_state%v  (i, k, j))

             vors = min(vors, int_state%vor(i, k, j))
             divs = min(divs, int_state%div(i, k, j))
             ts   = min(ts,   int_state%t  (i, k, j))
             qs   = min(qs,   int_state%q  (i, k, j))
             ozs  = min(ozs,  int_state%oz (i, k, j))
             clws = min(clws, int_state%clw(i, k, j))
             us   = min(us,   int_state%u  (i, k, j))
             vs   = min(vs,   int_state%v  (i, k, j))
         end do
     end do

     do i = 1, lon_lat
         lon_dim                = int_state%lon_dims(j)
         int_state%ps    (i, j) = int_state%four_gr2(i + (ksq     - 1) * lon_dim, j)
         int_state%dpdlam(i, j) = int_state%four_gr2(i + (ksplam  - 1) * lon_dim, j)
         int_state%dpdphi(i, j) = int_state%four_gr2(i + (kspphi  - 1) * lon_dim, j)

! calculate the maximum and minumum of each variable fields.
!-----------------------------------------------------------
         psm     = max(psm,     int_state%ps    (i, j))
         dpdlamm = max(dpdlamm, int_state%dpdlam(i, j))
         dpdphim = max(dpdphim, int_state%dpdphi(i, j))

         pss     = min(pss,     int_state%ps    (i, j))
         dpdlams = min(dpdlams, int_state%dpdlam(i, j))
         dpdphis = min(dpdphis, int_state%dpdphi(i, j))
     end do
 end do
 
!print*,'in the ensemble coupler, step2, maximum and minumum vor    = ', vorm, vors
!print*,'in the ensemble coupler, step2, maximum and minumum div    = ', divm, divs
!print*,'in the ensemble coupler, step2, maximum and minumum t      = ', tm,   ts  
!print*,'in the ensemble coupler, step2, maximum and minumum q      = ', qm,   qs  
!print*,'in the ensemble coupler, step2, maximum and minumum oz     = ', ozm,  ozs 
!print*,'in the ensemble coupler, step2, maximum and minumum clw    = ', clwm, clws
!print*,'in the ensemble coupler, step2, maximum and minumum u      = ', um,   us  
!print*,'in the ensemble coupler, step2, maximum and minumum v      = ', vm,   vs  
!print*,'in the ensemble coupler, step2, maximum and minumum ps     = ', psm,  pss 
!print*,'in the ensemble coupler, step2, maximum and minumum dpdlam = ', dpdlamm, dpdlams
!print*,'in the ensemble coupler, step2, maximum and minumum dpdphi = ', dpdphim, dpdphis

! now we get the gaussian grid fields and can use them for reginal processing.
!--------------------------------------------------------------
 print *, "rs_global=",rs_global
 if (abs(rs_global).gt.0.999.and.abs(rs_global).lt.1.001.and.griduse.eq.2) then
!  calculating regional rescaling factors fro kinetic energy at 500hpa

 if (.not.associated(int_state%ke_work))                                            & 
 allocate(int_state%ke_work(int_state%latmax, int_state%total_member))
 int_state%ke_work = 0.0

!print*,'in the ensemble coupler, bcst, latdims/total_member=', int_state%latdims, int_state%total_member
!print*,'in the ensemble coupler, bcst, me/member_id=', int_state%me, int_state%member_id

!identify the 500hpa level
 if (int_state%levs .eq. 28) then
  k500=13 
 endif
 if (int_state%levs .eq. 64) then
  k500=25 
 endif

!calculate 500hpa kineitic  energy ke.
 do j = 1, int_state%lats_node
     lat     = int_state%global_lats(int_state%ipt_lats_node - 1 + j)
     lon_lat = int_state%lons_lat(lat)
     keavg = 0.0
     do i = 1, lon_lat
        keavg = keavg + int_state%u(i, k500, j) * int_state%u(i, k500, j)        &
                      + int_state%v(i, k500, j) * int_state%v(i, k500, j)
     end do
        keavg = 0.5 * keavg / float(lon_lat)
        int_state%ke_work(lat, int_state%member_id(int_state%mm1)) = keavg
 end do

! start global broadcast 
! broadcasting the average ke over this latitude to other tasks
! print*,'in the ensemble coupler, start bcst, # of cpus', int_state%nodes
 do k = 1, int_state%nodes    ! each task
  do j = 1, int_state%lats_node_global(k)    !each latitude
   call gefs_bcst_global(int_state%ke_work(int_state%lats_global(j,k), int_state%member_id(k)), k-1, rc1)
!  call esmf_vmbarrier(vm_esmf, rc=rc1)
  end do
 end do
! print*,'in the ensemble coupler, finished bcst, '

! print out ke_work
 if(int_state%me == 00) then
   print *,'in the ensemble coupler, after bcst, test ke_work'
  do k = 1, int_state%latmax
   write (*,'(1x,i4,f6.3,21f8.3)')  k,int_state%slat_work(k),(int_state%ke_work(k,l),l=1,int_state%total_member)
  end do
 endif

!do k = 1, int_state%latmax
!do l = 1, int_state%total_member
! if(int_state%me == 00) then
!  print*,'in the ensemble coupler, bcst, k/l/keavg=', k,l,int_state%ke_work(k,l),int_state%slat_work(k) 
! endif
!end do
!end do

!calculating regiobnal re-scaling factors from the ke_work array.
!print *, 'dhhhtest', rs_global, griduse, kemax
!if (abs(rs_global).gt.0.999.and.abs(rs_global).lt.1.001.and.griduse.eq.2) then
   call get_scaling_factors(int_state%ke_work,int_state%latmax,int_state%total_member,int_state%slat_work,   &
                            kemax,ker,int_state%factor1_work,nreg,slat1,slat2)
   if(int_state%me == 35) then
    print*,'in the ensemble coupler, bcst, itest ker', int_state%cpl_run_calling_number
    do k = 1, nreg
     write (*,'(1x,a4,i4,15f8.3)')  'ker ',k,(ker(k,l),l=1,int_state%total_member) 
    end do
    print*,'in the ensemble coupler, bcst, itest faw factor1_work',int_state%cpl_run_calling_number
    do k = 1, nreg
     write (*,'(1x,a4,i4,15f8.3)')  'faw ',k,(int_state%factor1_work(k,l),l=1,int_state%total_member)
    end do
   endif
!endif

!   do k = 1, nreg
!   do l = 1, int_state%total_member
!    if(int_state%me == 00) then
!     print*,'after calling get_scaling_factors region/member/kemax/ker/factor=',  &
!          k,l,kemax(k),ker(k,l),int_state%factor1_work(k,l)
!    endif
!   end do
!   end do

 deallocate(int_state%ke_work)

 endif !! (abs(rs_global).gt.0.999.and.abs(rs_global).lt.1.001.and.griduse.eq.2) 
!end of the if_block of 500hpa ke base rescaling factor calculation

! do the rescaling for all model variables except ps.
!------------------------------------------------------------------------------
! print *, "rs_global=",rs_global

 do j = 1, int_state%lats_node
   lat     = int_state%global_lats(int_state%ipt_lats_node - 1 + j)
   lon_lat = int_state%lons_lat(lat)
! assigning rescaling factor, 3-belt or latitude-dependent
   if (abs(rs_global).gt.0.999.and.abs(rs_global).lt.1.001.and.griduse.eq.2) then 
     if (int_state%slat_work(lat).ge.slat1) then
      ireg = 1
     elseif (int_state%slat_work(lat).ge.slat2) then
      ireg = 2
     else
      ireg = 3
     endif
! dhou 12/10/2007, ke based, 3-belt rescaling
     rs = rs_global * int_state%factor1_work(ireg,int_state%member_id(int_state%mm1))
   else
! dhou 12/10/2007, latitude-julian_day based rescaling
!    rs = rs_global * ( 1.0 + int_state%parm3(1) * asin(int_state%slat_work(lat)) * &
!        2.0/pi * cos( (jul_day-1)*pi/182.0 ) )    ! linear function of latitude
     rs = rs_global * ( 1.0 + int_state%parm3(1) * int_state%slat_work(lat) *       &
         cos( (jul_day-1)*pi/182.0 ) )  ! linear function of sin(latitude)
!    if(int_state%me == 00) then
!      print *,'rs=', j,lat,int_state%slat_work(lat),rs,jul_day
!    endif
!
!         dhou 09/05/2013, increase rescaling factor in nh
   if(int_state%me == 00) then
     print *,'adh:rs=', j,lat,int_state%slat_work(lat),rs,jul_day
   endif 
    if (int_state%parm3_i(6) > 0) then
   parm1=int_state%parm3(7)
   parm2=int_state%parm3(8)
   parm3=int_state%parm3(9)
   if (int_state%slat_work(lat) > parm1 .and. int_state%slat_work(lat) < parm2) then
   rs=rs*(1.0 + (int_state%slat_work(lat) - parm1) / (parm2-parm1) * parm3 )
   endif
   if (int_state%slat_work(lat) >= parm2) then
   rs=rs*(1.0 + parm3)
   endif
   if(int_state%me == 00) then
     print *,'bdh:rs=', j,lat,int_state%slat_work(lat),rs,jul_day
   endif 
    endif 
   endif

! applying the rescaling factor
     do k = 1, int_state%levs
       do i = 1, lon_lat
        lon_dim                = int_state%lon_dims(j)
        int_state%four_gr2(i + (ksz + k - 2) * lon_dim, j) = int_state%vor(i, k, j)*rs
        int_state%four_gr2(i + (ksd + k - 2) * lon_dim, j) = int_state%div(i, k, j)*rs
        int_state%four_gr2(i + (kst + k - 2) * lon_dim, j) = int_state%t  (i, k, j)*rs
        int_state%four_gr2(i + (ksr + k - 2) * lon_dim, j) = int_state%q  (i, k, j)*rs
        int_state%four_gr2(i + (kso + k - 2) * lon_dim, j) = int_state%oz (i, k, j)*rs
        int_state%four_gr2(i + (ksc + k - 2) * lon_dim, j) = int_state%clw(i, k, j)*rs
        int_state%four_gr2(i + (ksu + k - 2) * lon_dim, j) = int_state%u  (i, k, j)*rs
        int_state%four_gr2(i + (ksv + k - 2) * lon_dim, j) = int_state%v  (i, k, j)*rs
       end do
     end do

     do i = 1, lon_lat
       lon_dim                = int_state%lon_dims(j)
       int_state%four_gr2(i + (ksplam  - 1) * lon_dim, j) = int_state%dpdlam(i, j)*rs
       int_state%four_gr2(i + (kspphi  - 1) * lon_dim, j) = int_state%dpdphi(i, j)*rs
       int_state%four_gr2(i + (ksq     - 1) * lon_dim, j) = log(int_state%ps(i, j))*rs
! nb: for surface pressure, convert to ln(ps * 0.1) before rescaling 
     end do
 end do   !(j = 1, int_state%lats_node)

! re-scaling done!
! transforming back from the gaussian grid fields to the spectral space fields.
! and do the global rescaling for all model variables except ps.
!------------------------------------------------------------------------------
 do lan = 1, int_state%lats_node
     lat     = int_state%global_lats(int_state%ipt_lats_node - 1 + lan)
     lon_dim = int_state%lon_dims(lan)
     lon_lat = int_state%lons_lat(lat)

     call gefs_grid2four_thread(int_state%four_gr2(1, lan), int_state%four_gr1(1, lan), &
                           lon_dim, lon_lat, int_state%londi, int_state%nvars)
 end do
 
 call gefs_four2fln(int_state%lslag,        int_state%latdims,    int_state%nvars,         &
                    int_state%nvars,        int_state%four_gr1,   int_state%ls_nodes,      &
                    int_state%max_ls_nodes, int_state%lats_nodes, int_state%global_lats,   &
                    int_state%lon_dims,     int_state%lats_node,  int_state%ipt_lats_node, &
                    int_state%dimg,         int_state%lat1s,      int_state%londi,         &
                    int_state%latl,         int_state%latl2,                               &
                    int_state%trie_ls(1, 1, int_state%p_ze),                               &
                    int_state%trio_ls(1, 1, int_state%p_ze),                               &
                    int_state%plnew,        int_state%plnow,      int_state%ls_node,       &
                    int_state%jcap,         int_state%jintmx,                              &
                    int_state%trie_ls_size, int_state%trio_ls_size,                        &
                    int_state%ls_dim,       int_state%ls_max_node,                         &
                    int_state%mpi_r_mpi,    int_state%levs,       int_state%latr,         &
                    int_state%nodes_comp,   int_state%me_comp,    int_state%mc_comp)

 do k = 1, int_state%levs
     call gefs_uveodz(int_state%trie_ls(1, 1, int_state%p_ze  + k - 1), &
                      int_state%trio_ls(1, 1, int_state%p_di  + k - 1), &
                      int_state%trie_ls(1, 1, int_state%p_uln + k - 1), &
                      int_state%trio_ls(1, 1, int_state%p_vln + k - 1), &
                      int_state%epse,         int_state%epso,           &
                      int_state%ls_node,      int_state%jcap,           &
                      int_state%trie_ls_size, int_state%trio_ls_size,   &
                      int_state%ls_dim,       int_state%ls_max_node,    &
                      rerth)

     call gefs_uvoedz(int_state%trio_ls(1, 1, int_state%p_ze  + k - 1), &
                      int_state%trie_ls(1, 1, int_state%p_di  + k - 1), &
                      int_state%trio_ls(1, 1, int_state%p_uln + k - 1), &
                      int_state%trie_ls(1, 1, int_state%p_vln + k - 1), &
                      int_state%epse,         int_state%epso,           &
                      int_state%ls_node,      int_state%jcap,           &
                      int_state%trie_ls_size, int_state%trio_ls_size,   &
                      int_state%ls_dim,       int_state%ls_max_node,    &
                      rerth)
 end do

!print *, "in the ensemble coupler, step2, dpdlam ",                  &
!int_state%trio_ls(1, 1, int_state%p_dlam),int_state%trie_ls(1, 1, int_state%p_dlam)
!print *, "in the ensemble coupler, step2, dpdphi ",                  &
!int_state%trio_ls(1, 1, int_state%p_dphi),int_state%trie_ls(1, 1, int_state%p_dphi)

!dhou 09/06/2007 shift the arrays back to original position if it is shifted
if (griduse .eq. 1 ) then
 lshift = int_state%p_q - int_state%p_qm  
 do l = int_state%p_ze, int_state%p_q
     do j = 1, 2
         do i = 1, int_state%trie_ls_size
             atem = int_state%trie_ls(i, j, l-lshift) 
             int_state%trie_ls( i, j, l-lshift) = int_state%trie_ls( i, j, l)
             int_state%trie_ls( i, j, l) = atem
         end do
         do i = 1, int_state%trio_ls_size
             atem = int_state%trio_ls(i, j, l-lshift)
             int_state%trio_ls( i, j, l-lshift) = int_state%trio_ls( i, j, l)
             int_state%trio_ls( i, j, l) = atem
         end do
     end do
 end do
endif

! replace state s2 with 0.5*(s1+s2) or replace state s1 with s2
if (uses1 .eq. 3 ) then
 lshift = int_state%p_q - int_state%p_qm  
 do l = int_state%p_ze, int_state%p_q
     do j = 1, 2
         do i = 1, int_state%trie_ls_size
              int_state%trie_ls(i, j, l) =                                     &   
         0.5*(int_state%trie_ls(i, j, l-lshift) + int_state%trie_ls(i, j, l) )
         end do
         do i = 1, int_state%trio_ls_size
              int_state%trio_ls(i, j, l) =                                     &   
         0.5*(int_state%trio_ls(i, j, l-lshift) + int_state%trio_ls(i, j, l) )
         end do
     end do
 end do
elseif (uses1 .eq. 2 ) then
 lshift = int_state%p_q - int_state%p_qm  
 do l = int_state%p_ze, int_state%p_q
     do j = 1, 2
         do i = 1, int_state%trie_ls_size
             int_state%trie_ls( i, j, l-lshift) = int_state%trie_ls( i, j, l)
         end do
         do i = 1, int_state%trio_ls_size
             int_state%trio_ls( i, j, l-lshift) = int_state%trio_ls( i, j, l)
         end do
     end do
 end do
endif

!dhou, 09/06/2007  save the trio-ls and trie_ls back to trieo_ls_max, so
!                   that the change (if any) can be memorized. 
 k = 1
 do l = 1, int_state%trieo_vertical_size
     do j = 1, 2
         do i = 1, int_state%trie_ls_size
             int_state%trieo_ls_max(k) = int_state%trie_ls(i, j, l) 
             k = k + 1
         end do
         do i = 1, int_state%trio_ls_size
             int_state%trieo_ls_max(k) = int_state%trio_ls(i, j, l) 
             k = k + 1
         end do
     end do
 end do
   
!print *, "in the ensemble coupler, step2, dpdlam ",                  &
!int_state%trio_ls(1, 1, int_state%p_dlam),int_state%trie_ls(1, 1, int_state%p_dlam)
!print *, "in the ensemble coupler, step2, dpdphi ",                  &
!int_state%trio_ls(1, 1, int_state%p_dphi),int_state%trie_ls(1, 1, int_state%p_dphi)
!print *, "in the ensemble coupler, step2, zzzzz ",                  &
!int_state%trio_ls(1, 1, int_state%p_dlam),int_state%trie_ls(1, 4, int_state%p_ze),int_state%trio_ls(1, 5, int_state%p_ze)
!print *, "in the ensemble coupler, step2, zzzdd ",                  &
!int_state%trio_ls(1, 1, int_state%p_dlam),int_state%trie_ls(1, 4, int_state%p_di),int_state%trio_ls(1, 5, int_state%p_di)
!print *, "in the ensemble coupler, step2, zzzuu ",                  &
!int_state%trio_ls(1, 1, int_state%p_dlam),int_state%trie_ls(1, 4, int_state%p_uln),int_state%trio_ls(1, 5, int_state%p_uln)
!print *, "in the ensemble coupler, step2, zzzvv ",                  &
!int_state%trio_ls(1, 1, int_state%p_dlam),int_state%trie_ls(1, 4, int_state%p_vln),int_state%trio_ls(1, 5, int_state%p_vln)

 if(rcfinal /= esmf_success) then
     print*, "fail: gefs_sto_per_scheme_step2_m."
!else
!    print*, "pass: gefs_sto_per_scheme_step2_m."
 end if

!if(present(rc)) then
     rc = rcfinal
!end if

 end subroutine gefs_sto_per_scheme_step2

 subroutine get_scaling_factors(ke,nlat,nmember,slat,kemax,ker,factor1,nregion,slat1,slat2)
 use machine,  only: kind_evod
 integer nlat,nmember,nregion
 real(kind = kind_evod)                      :: ke(nlat,nmember)
 real(kind = kind_evod)                      :: factor1(nregion,nmember) 
 real(kind = kind_evod)                      :: slat(nlat),slat1,slat2 
 real(kind = kind_evod)                      :: kemax(nregion) 
 real(kind = kind_evod)                      :: ker(nregion,nmember) 
 real(kind = kind_evod)                      :: weight(3) 
 real(kind = kind_evod)                      :: coslat
 integer i,j,k

 do k=1,nmember
  do j=1,nregion
    ker(j,k)=0.0
  enddo
 enddo
 do k=1,nmember-1
  do j=1,nregion
    weight(j)=0.0
  enddo 
  do j=1,nlat
   coslat=sqrt(1-slat(j)**2)
   if (slat(j).gt.slat1) then
    ker(1,k)=ker(1,k)+ke(j,k)*coslat
    weight(1)=weight(1)+coslat
   elseif (slat(j).gt.slat2) then
    ker(2,k)=ker(2,k)+ke(j,k)*coslat
    weight(2)=weight(2)+coslat
   else
    ker(3,k)=ker(3,k)+ke(j,k)*coslat
    weight(3)=weight(3)+coslat
   endif
  enddo
  do j=1,nregion
    if (weight(j).lt.1.0e-5) then
     print *, 'weight=0 in get_scaling_factors, forced to stop! for region/member',j,k
     stop
    endif
    ker(j,k)=ker(j,k)/weight(j)
  enddo 
  do j=1,nregion
   if (ker(j,k).gt.kemax(j)) then
    factor1(j,k)=kemax(j)/ker(j,k)
   else
    factor1(j,k)=1.0
   endif
  enddo
 enddo

 return
 end
