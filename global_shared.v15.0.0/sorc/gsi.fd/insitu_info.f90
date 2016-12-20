module insitu_info
!$$$  
!                .      .    .                                       .
! module:  insitu_info          
!   prgmmr: Xu Li          org: np22                date: 2008-04-22
!
! abstract:  This  module classify the depth & instrument dependent
!            moored buoy and ships observations
!
!
! program history log:
!   ??????    li      - intial version
!   10Jul2011 todling - careful about existence of info-text file
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: i_kind,r_kind
  implicit none

! Declare parameters
  integer(i_kind),parameter:: n_comps = 3
  integer(i_kind),parameter:: n_scripps = 40
  integer(i_kind),parameter:: n_triton = 70
  integer(i_kind),parameter:: n_3mdiscus = 153
  integer(i_kind),parameter:: lunship = 11
  integer(i_kind),save     :: n_ship = 2011

! Declare variables
  integer(i_kind) :: i
  character(len=10) :: filename
  character(len=5),allocatable,dimension(:):: cid_mbuoy
  type, public :: shipvar
     character(len=10), allocatable, dimension(:) :: id
     real(r_kind), allocatable, dimension(:) :: depth
     character(len=5), allocatable, dimension(:) :: sensor
  end type
  type(shipvar):: ship

 contains

 subroutine mbuoy_info(mype)
!**************************************************************************
!
! assign the depth dependent moored buoy station ID
!

  integer(i_kind), intent(in) :: mype
  allocate(cid_mbuoy(n_3mdiscus))
!
! initialize cid
!
  cid_mbuoy = '     '
!
! COMPS moored buoy (depth = 1.2m)
!
  cid_mbuoy( 1) = '42022'
  cid_mbuoy( 2) = '42023'
  cid_mbuoy( 3) = '42024'
!
! SCRIPPS moored buoy (depth = 0.45m)
!
  cid_mbuoy( 4) = '31201'
  cid_mbuoy( 5) = '41112'
  cid_mbuoy( 6) = '41113'
  cid_mbuoy( 7) = '41114'
  cid_mbuoy( 8) = '42099'
  cid_mbuoy( 9) = '46211'
  cid_mbuoy(10) = '46212'
  cid_mbuoy(11) = '46213'
  cid_mbuoy(12) = '46214'
  cid_mbuoy(13) = '46215'
  cid_mbuoy(14) = '46216'
  cid_mbuoy(15) = '46217'
  cid_mbuoy(16) = '46218'
  cid_mbuoy(17) = '46219'
  cid_mbuoy(18) = '46220'
  cid_mbuoy(19) = '46221'
  cid_mbuoy(10) = '46222'
  cid_mbuoy(21) = '46223'
  cid_mbuoy(22) = '46224'
  cid_mbuoy(23) = '46225'
  cid_mbuoy(24) = '46226'
  cid_mbuoy(25) = '46227'
  cid_mbuoy(26) = '46228'
  cid_mbuoy(27) = '46229'
  cid_mbuoy(28) = '46230'
  cid_mbuoy(29) = '46231'
  cid_mbuoy(30) = '46232'
  cid_mbuoy(31) = '46233'
  cid_mbuoy(32) = '46234'
  cid_mbuoy(33) = '46235'
  cid_mbuoy(34) = '46236'
  cid_mbuoy(35) = '46237'
  cid_mbuoy(36) = '46238'
  cid_mbuoy(37) = '51201'
  cid_mbuoy(38) = '51202'
  cid_mbuoy(39) = '51203'
  cid_mbuoy(40) = '52200'
!
! TRITON buoys (depth = 1.5m)
!
  cid_mbuoy(41) = '52071'
  cid_mbuoy(42) = '52072'
  cid_mbuoy(43) = '52073'
  cid_mbuoy(44) = '52074'
  cid_mbuoy(45) = '52075'
  cid_mbuoy(46) = '52076'
  cid_mbuoy(47) = '52077'
  cid_mbuoy(48) = '52078'
  cid_mbuoy(49) = '52079'
  cid_mbuoy(50) = '52080'
  cid_mbuoy(51) = '52081'
  cid_mbuoy(52) = '52082'
  cid_mbuoy(53) = '52083'
  cid_mbuoy(54) = '52084'
  cid_mbuoy(55) = '52085'
  cid_mbuoy(56) = '52086'
  cid_mbuoy(57) = '52087'
  cid_mbuoy(58) = '52088'
  cid_mbuoy(59) = '53056'
  cid_mbuoy(60) = '53057'
  cid_mbuoy(61) = '52043'
  cid_mbuoy(62) = '52044'
  cid_mbuoy(63) = '52045'
  cid_mbuoy(64) = '52046'
!
! NDBC 3-meter buoy (depth = 0.6m)
!
  cid_mbuoy(71) = '41004' 
  cid_mbuoy(72) = '41008' 
  cid_mbuoy(73) = '41012' 
  cid_mbuoy(74) = '41013' 
  cid_mbuoy(75) = '41025' 
  cid_mbuoy(76) = '41035' 
  cid_mbuoy(77) = '41036' 
  cid_mbuoy(78) = '42007' 
  cid_mbuoy(79) = '42019' 
  cid_mbuoy(80) = '42020' 
  cid_mbuoy(81) = '42035' 
  cid_mbuoy(82) = '42036' 
  cid_mbuoy(83) = '42039' 
  cid_mbuoy(84) = '42040' 
  cid_mbuoy(85) = '44007' 
  cid_mbuoy(86) = '44008' 
  cid_mbuoy(87) = '44009' 
  cid_mbuoy(88) = '44013' 
  cid_mbuoy(89) = '44014' 
  cid_mbuoy(90) = '44017' 
  cid_mbuoy(91) = '44018' 
  cid_mbuoy(92) = '44025' 
  cid_mbuoy(93) = '44027' 
  cid_mbuoy(94) = '45001' 
  cid_mbuoy(95) = '45002' 
  cid_mbuoy(96) = '45003' 
  cid_mbuoy(97) = '45004' 
  cid_mbuoy(98) = '45005' 
  cid_mbuoy(99) = '45006' 
  cid_mbuoy(100) = '45007' 
  cid_mbuoy(101) = '45008' 
  cid_mbuoy(102) = '45012' 
  cid_mbuoy(103) = '46011' 
  cid_mbuoy(104) = '46012' 
  cid_mbuoy(105) = '46013' 
  cid_mbuoy(106) = '46014' 
  cid_mbuoy(107) = '46015' 
  cid_mbuoy(108) = '46022' 
  cid_mbuoy(109) = '46025' 
  cid_mbuoy(110) = '46026' 
  cid_mbuoy(111) = '46027' 
  cid_mbuoy(112) = '46028' 
  cid_mbuoy(113) = '46029' 
  cid_mbuoy(114) = '46042' 
  cid_mbuoy(115) = '46047' 
  cid_mbuoy(116) = '46050' 
  cid_mbuoy(117) = '46053' 
  cid_mbuoy(118) = '46060' 
  cid_mbuoy(119) = '46063' 
  cid_mbuoy(120) = '46069' 
  cid_mbuoy(121) = '46081' 
  cid_mbuoy(122) = '46086' 
  cid_mbuoy(123) = '46087' 
  cid_mbuoy(124) = '46088' 
  cid_mbuoy(125) = '46089' 
  cid_mbuoy(126) = '51001' 
  cid_mbuoy(127) = '51028' 
!
! Canadian 3-meter buoy (depth = 0.6m)
!
  cid_mbuoy(128) = '44258' 
  cid_mbuoy(129) = '45132' 
  cid_mbuoy(130) = '45135' 
  cid_mbuoy(131) = '45136' 
  cid_mbuoy(132) = '45137' 
  cid_mbuoy(133) = '45138' 
  cid_mbuoy(134) = '45143' 
  cid_mbuoy(135) = '45144' 
  cid_mbuoy(136) = '45145' 
  cid_mbuoy(137) = '46131' 
  cid_mbuoy(138) = '46132' 
  cid_mbuoy(139) = '46134' 
  cid_mbuoy(140) = '46145' 
  cid_mbuoy(141) = '46146' 
  cid_mbuoy(142) = '46147' 
  cid_mbuoy(143) = '46181' 
  cid_mbuoy(144) = '46183' 
  cid_mbuoy(145) = '46185' 
  cid_mbuoy(146) = '46204' 
  cid_mbuoy(147) = '46205' 
  cid_mbuoy(148) = '46206' 
  cid_mbuoy(149) = '46207' 
  cid_mbuoy(150) = '46208' 
!
! MBARI moored buoy (depth = 0.6m)
!
  cid_mbuoy(151) = '46091'
  cid_mbuoy(152) = '46092'
  cid_mbuoy(153) = '46093'

  if(mype == 0) write(6,*) ' in mbuoy_info,n_comps = ',n_comps,' n_scripps = ',n_scripps, &
                              ' n_triton = ',n_triton,' n_3mdiscus = ',n_3mdiscus
 end subroutine mbuoy_info

 subroutine read_ship_info(mype)

!
!  read ship info from an external file to determine the depth and instrument
!
   integer(i_kind), intent(in) :: mype

   integer(i_kind) ios
   logical iexist

   filename='insituinfo'
   inquire(file=trim(filename),exist=iexist)
   if(iexist) then
      open(lunship,file=filename,form='formatted',iostat=ios)
      allocate (ship%id(n_ship),ship%depth(n_ship),ship%sensor(n_ship))
      if(ios==0) then
         do i = 1, n_ship
            read(lunship,'(a10,f6.1,1x,a5)') ship%id(i),ship%depth(i),ship%sensor(i)
         enddo
      endif
   else 
      n_ship=0
      allocate (ship%id(n_ship),ship%depth(n_ship),ship%sensor(n_ship))
   endif
  
   if(mype == 0) write(6,*) ' in read_ship_info, n_ship = ', n_ship
 end subroutine read_ship_info
end module insitu_info
