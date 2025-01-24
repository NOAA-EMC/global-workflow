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
!   20190115  li      - add to handle mbuoyb
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
  character(len=7),allocatable,dimension(:):: cid_mbuoyb
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

  if(mype == 0) write(6,1000) n_comps,n_scripps,n_triton,n_3mdiscus
1000 format(' in mbuoy_info,n_comps = ',i10,' n_scripps = ',i10, &
        ' n_triton = ',i10,' n_3mdiscus = ',i10)
 end subroutine mbuoy_info

 subroutine mbuoyb_info(mype)
!**************************************************************************
!
! assign the depth dependent moored buoyb station ID
!

  integer(i_kind), intent(in) :: mype
  allocate(cid_mbuoyb(n_3mdiscus))
!
! initialize cid
!
  cid_mbuoyb = '     '
!
! COMPS moored buoy (depth = 1.2m)
!
  cid_mbuoyb( 1) = '4200022'
  cid_mbuoyb( 2) = '4200023'
  cid_mbuoyb( 3) = '4200024'
!
! SCRIPPS moored buoy (depth = 0.45m)
!
  cid_mbuoyb( 4) = '3100201'
  cid_mbuoyb( 5) = '4100112'
  cid_mbuoyb( 6) = '4100113'
  cid_mbuoyb( 7) = '4100114'
  cid_mbuoyb( 8) = '4200099'
  cid_mbuoyb( 9) = '4600211'
  cid_mbuoyb(10) = '4600212'
  cid_mbuoyb(11) = '4600213'
  cid_mbuoyb(12) = '4600214'
  cid_mbuoyb(13) = '4600215'
  cid_mbuoyb(14) = '4600216'
  cid_mbuoyb(15) = '4600217'
  cid_mbuoyb(16) = '4600218'
  cid_mbuoyb(17) = '4600219'
  cid_mbuoyb(18) = '4600220'
  cid_mbuoyb(19) = '4600221'
  cid_mbuoyb(10) = '4600222'
  cid_mbuoyb(21) = '4600223'
  cid_mbuoyb(22) = '4600224'
  cid_mbuoyb(23) = '4600225'
  cid_mbuoyb(24) = '4600226'
  cid_mbuoyb(25) = '4600227'
  cid_mbuoyb(26) = '4600228'
  cid_mbuoyb(27) = '4600229'
  cid_mbuoyb(28) = '4600230'
  cid_mbuoyb(29) = '4600231'
  cid_mbuoyb(30) = '4600232'
  cid_mbuoyb(31) = '4600233'
  cid_mbuoyb(32) = '4600234'
  cid_mbuoyb(33) = '4600235'
  cid_mbuoyb(34) = '4600236'
  cid_mbuoyb(35) = '4600237'
  cid_mbuoyb(36) = '4600238'
  cid_mbuoyb(37) = '5100201'
  cid_mbuoyb(38) = '5100202'
  cid_mbuoyb(39) = '5100203'
  cid_mbuoyb(40) = '5200200'
!
! TRITON buoys (depth = 1.5m)
!
  cid_mbuoyb(41) = '5200071'
  cid_mbuoyb(42) = '5200072'
  cid_mbuoyb(43) = '5200073'
  cid_mbuoyb(44) = '5200074'
  cid_mbuoyb(45) = '5200075'
  cid_mbuoyb(46) = '5200076'
  cid_mbuoyb(47) = '5200077'
  cid_mbuoyb(48) = '5200078'
  cid_mbuoyb(49) = '5200079'
  cid_mbuoyb(50) = '5200080'
  cid_mbuoyb(51) = '5200081'
  cid_mbuoyb(52) = '5200082'
  cid_mbuoyb(53) = '5200083'
  cid_mbuoyb(54) = '5200084'
  cid_mbuoyb(55) = '5200085'
  cid_mbuoyb(56) = '5200086'
  cid_mbuoyb(57) = '5200087'
  cid_mbuoyb(58) = '5200088'
  cid_mbuoyb(59) = '5300056'
  cid_mbuoyb(60) = '5300057'
  cid_mbuoyb(61) = '5200043'
  cid_mbuoyb(62) = '5200044'
  cid_mbuoyb(63) = '5200045'
  cid_mbuoyb(64) = '5200046'
!
! NDBC 3-meter buoy (depth = 0.6m)
!
  cid_mbuoyb(71) = '4100004' 
  cid_mbuoyb(72) = '4100008' 
  cid_mbuoyb(73) = '4100012' 
  cid_mbuoyb(74) = '4100013' 
  cid_mbuoyb(75) = '4100025' 
  cid_mbuoyb(76) = '4100035' 
  cid_mbuoyb(77) = '4100036' 
  cid_mbuoyb(78) = '4200007' 
  cid_mbuoyb(79) = '4200019' 
  cid_mbuoyb(80) = '4200020' 
  cid_mbuoyb(81) = '4200035' 
  cid_mbuoyb(82) = '4200036' 
  cid_mbuoyb(83) = '4200039' 
  cid_mbuoyb(84) = '4200040' 
  cid_mbuoyb(85) = '4400007' 
  cid_mbuoyb(86) = '4400008' 
  cid_mbuoyb(87) = '4400009' 
  cid_mbuoyb(88) = '4400013' 
  cid_mbuoyb(89) = '4400014' 
  cid_mbuoyb(90) = '4400017' 
  cid_mbuoyb(91) = '4400018' 
  cid_mbuoyb(92) = '4400025' 
  cid_mbuoyb(93) = '4400027' 
  cid_mbuoyb(94) = '4500001' 
  cid_mbuoyb(95) = '4500002' 
  cid_mbuoyb(96) = '4500003' 
  cid_mbuoyb(97) = '4500004' 
  cid_mbuoyb(98) = '4500005' 
  cid_mbuoyb(99) = '4500006' 
  cid_mbuoyb(100) = '4500007' 
  cid_mbuoyb(101) = '4500008' 
  cid_mbuoyb(102) = '4500012' 
  cid_mbuoyb(103) = '4600011' 
  cid_mbuoyb(104) = '4600012' 
  cid_mbuoyb(105) = '4600013' 
  cid_mbuoyb(106) = '4600014' 
  cid_mbuoyb(107) = '4600015' 
  cid_mbuoyb(108) = '4600022' 
  cid_mbuoyb(109) = '4600025' 
  cid_mbuoyb(110) = '4600026' 
  cid_mbuoyb(111) = '4600027' 
  cid_mbuoyb(112) = '4600028' 
  cid_mbuoyb(113) = '4600029' 
  cid_mbuoyb(114) = '4600042' 
  cid_mbuoyb(115) = '4600047' 
  cid_mbuoyb(116) = '4600050' 
  cid_mbuoyb(117) = '4600053' 
  cid_mbuoyb(118) = '4600060' 
  cid_mbuoyb(119) = '4600063' 
  cid_mbuoyb(120) = '4600069' 
  cid_mbuoyb(121) = '4600081' 
  cid_mbuoyb(122) = '4600086' 
  cid_mbuoyb(123) = '4600087' 
  cid_mbuoyb(124) = '4600088' 
  cid_mbuoyb(125) = '4600089' 
  cid_mbuoyb(126) = '5100001' 
  cid_mbuoyb(127) = '5100028' 
!
! Canadian 3-meter buoy (depth = 0.6m)
!
  cid_mbuoyb(128) = '4400258' 
  cid_mbuoyb(129) = '4500132' 
  cid_mbuoyb(130) = '4500135' 
  cid_mbuoyb(131) = '4500136' 
  cid_mbuoyb(132) = '4500137' 
  cid_mbuoyb(133) = '4500138' 
  cid_mbuoyb(134) = '4500143' 
  cid_mbuoyb(135) = '4500144' 
  cid_mbuoyb(136) = '4500145' 
  cid_mbuoyb(137) = '4600131' 
  cid_mbuoyb(138) = '4600132' 
  cid_mbuoyb(139) = '4600134' 
  cid_mbuoyb(140) = '4600145' 
  cid_mbuoyb(141) = '4600146' 
  cid_mbuoyb(142) = '4600147' 
  cid_mbuoyb(143) = '4600181' 
  cid_mbuoyb(144) = '4600183' 
  cid_mbuoyb(145) = '4600185' 
  cid_mbuoyb(146) = '4600204' 
  cid_mbuoyb(147) = '4600205' 
  cid_mbuoyb(148) = '4600206' 
  cid_mbuoyb(149) = '4600207' 
  cid_mbuoyb(150) = '4600208' 
!
! MBARI moored buoy (depth = 0.6m)
!
  cid_mbuoyb(151) = '4600091'
  cid_mbuoyb(152) = '4600092'
  cid_mbuoyb(153) = '4600093'

  if(mype == 0) write(6,1000) n_comps,n_scripps,n_triton,n_3mdiscus
1000 format(' in mbuoyb_info,n_comps = ',i10,' n_scripps = ',i10, &
        ' n_triton = ',i10,' n_3mdiscus = ',i10)
 end subroutine mbuoyb_info

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
