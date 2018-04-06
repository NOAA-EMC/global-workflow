
!
! !module: stochy_internal_state_mod 
!                         --- internal state definition of the
!                             gridded component of the spectral random patterns
!
! !description:  define the spectral internal state used to
!                                             create the internal state.
!---------------------------------------------------------------------------
! !revision history:
!
!  Oct 11 2016     P Pegion port of gfs_dynamics_interal_state
!
! !interface:
!
      
      module stochy_internal_state_mod

!!uses:
!------
      use spectral_layout
      use stochy_gg_def
      use stochy_resol_def
 

      implicit none
      private

! -----------------------------------------------
      type,public::stochy_internal_state		! start type define
! -----------------------------------------------

      integer                   :: me, nodes
      integer                   :: lnt2_s, llgg_s
      integer                   :: lnt2
      integer                   :: grib_inp

!
      integer nxpt,nypt,jintmx
      integer lonf,latg,lats_node_a_max

      integer npe_single_member

      character(16)                     ::  cfhour1
!jws
      integer                           ::  num_file
      character(32)        ,allocatable ::  filename_base(:)
      integer                           ::  ipt_lats_node_a
      integer                           ::  lats_node_a
!jwe

      integer                           ::  nblck,kdt
!      real                              ::  deltim

      integer              ,allocatable ::      lonsperlat (:)
      integer              ,allocatable ::      ls_node    (:)
      integer              ,allocatable ::      ls_nodes   (:, :)
      integer              ,allocatable ::  max_ls_nodes   (:)

      integer              ,allocatable ::  lats_nodes_a   (:)
      integer              ,allocatable ::  global_lats_a  (:)
      integer              ,allocatable ::  lats_nodes_ext (:)
      integer              ,allocatable ::  global_lats_ext(:)
      integer              ,allocatable ::  global_lats_h  (:)
      integer                           :: xhalo,yhalo

      integer              ,allocatable ::  lats_nodes_a_fix (:)

      real(kind=kind_dbl_prec) ,allocatable ::        epse  (:)
      real(kind=kind_dbl_prec) ,allocatable ::        epso  (:)
      real(kind=kind_dbl_prec) ,allocatable ::        epsedn(:)
      real(kind=kind_dbl_prec) ,allocatable ::        epsodn(:)
      real(kind=kind_dbl_prec) ,allocatable ::        kenorm_e(:)
      real(kind=kind_dbl_prec) ,allocatable ::        kenorm_o(:)

      real(kind=kind_dbl_prec) ,allocatable ::       snnp1ev(:)
      real(kind=kind_dbl_prec) ,allocatable ::       snnp1od(:)

      real(kind=kind_dbl_prec) ,allocatable ::       plnev_a(:,:)
      real(kind=kind_dbl_prec) ,allocatable ::       plnod_a(:,:)
      real(kind=kind_dbl_prec) ,allocatable ::       pddev_a(:,:)
      real(kind=kind_dbl_prec) ,allocatable ::       pddod_a(:,:)
      real(kind=kind_dbl_prec) ,allocatable ::       plnew_a(:,:)
      real(kind=kind_dbl_prec) ,allocatable ::       plnow_a(:,:)


      real(kind=kind_dbl_prec) ,allocatable ::       trie_ls(:,:,:)
      real(kind=kind_dbl_prec) ,allocatable ::       trio_ls(:,:,:)

      INTEGER                               :: TRIEO_TOTAL_SIZE
      INTEGER, ALLOCATABLE, DIMENSION(:)    :: TRIE_LS_SIZE
      INTEGER, ALLOCATABLE, DIMENSION(:)    :: TRIO_LS_SIZE
      INTEGER, ALLOCATABLE, DIMENSION(:)    :: TRIEO_LS_SIZE
      INTEGER, ALLOCATABLE, DIMENSION(:)    :: LS_MAX_NODE_GLOBAL
      INTEGER, ALLOCATABLE, DIMENSION(:, :) :: LS_NODE_GLOBAL


!

!!
      integer              init,jcount,jpt,node,ibmsign,lon_dim,ilat

      real(kind=kind_dbl_prec) colat1, rone, rlons_lat, scale_ibm

      integer              lotls,lotgr,lots,lots_slg,lotd,lota,lotp

      integer              ibrad,ifges,ihour,ini,j,jdt,ksout,maxstp
      integer              mdt,idt,timetot,timer,time0
      integer              mods,n1,n2,ndgf,ndgi,nfiles,nflps
      integer              n1hyb, n2hyb,nlunit
      integer              nges,ngpken,niter,nnmod,nradf,nradr
      integer              nsfcf,nsfci,nsfcs,nsigi,nsigs,nstep
      integer              nznlf,nznli,nznls,id,iret,nsout,ndfi

      integer              ierr,iprint,k,l,locl,n
      integer              lan,lat
      integer              spectral_loop


      integer ikey,nrank_all,kcolor

      real(kind=kind_dbl_prec) cons0p5,cons1200,cons3600,cons0
     
!
! -----------------------------------------------------
      end type stochy_internal_state		! end type define
! -----------------------------------------------------

! this state is supported by c pointer not f90 pointer, thus
! need this wrap.
!-----------------------------------------------------------
      type stochy_wrap		! begin type define
          type (stochy_internal_state), pointer :: int_state
      end type stochy_wrap	! end type define

      end module stochy_internal_state_mod
