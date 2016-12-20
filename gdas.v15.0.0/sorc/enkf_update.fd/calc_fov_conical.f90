 module calc_fov_conical
!$$$   module documentation block
!                .      .    .                                       .
! module:    calc_fov_conical
!
! abstract: contains all routines necessary for fov calculations
!           for conical scanners.
!
! program history log:
!   2008-11-03  kleespies/gayno   initial version
!   2009-12-10  keesspies - new ssmi/s instruments
!   2011-09-13  gayno - improve module's error handling
!
! subroutines included:
!   sub fov_ellipse_conical    - calc lat/lon of fov polygon
!   sub fovconicalanglessizes  - compute cross/along track angles/fov sizes
!   sub instrument_init        - initialize variables for a specific instrument
!   sub inside_fov_conical     - determine antenna power at pt within fov
!
! Variable Definitions:
!   def  alongtrackangle       - along track angle
!   def  alongtrackfovsize     - along track fov size
!   def  crosstrackangle       - cross track angle
!   def  crosstrackfovsize     - cross track fov size
!   def  rmax                  - major axis of ellipse
!   def  rmin                  - minor axis of ellipse
!   def  eccen                 - fov eccentricity
!   def  maxinstr              - maximum number of instruments
!   def  npoly                 - number of vertices in the fov polygon
!   def  radius                - radius of earth in km
!   def  nchan                 - number of channels
!   def  instrumentrange       - instruments processed by this code
!   def  fovangle              - fov angle
!   def  ssmiscoef_XX          - coefficients to calc returned power
!                                for each ssmis satellite
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

 use kinds, only : i_kind, r_kind
 use calc_fov_crosstrk, only : npoly
 implicit none

 private

 public instrument_init
 public inside_fov_conical
 public fov_ellipse_conical

 integer(i_kind), parameter, private :: nchan = 24
 integer(i_kind), parameter, private :: maxinstr = 5
 integer(i_kind), parameter :: instrumentrange(2) = (/26, 30/)

 real(r_kind), parameter, private    :: radius   = 6371.22_r_kind

 real(r_kind), private :: alongtrackangle(nchan)
 real(r_kind), private :: crosstrackangle(nchan)
 real(r_kind), private :: alongtrackfovsize(nchan)
 real(r_kind), private :: crosstrackfovsize(nchan)
 real(r_kind), private :: rmax(nchan)
 real(r_kind), private :: rmin(nchan)
 real(r_kind), private :: eccen(nchan)
 real(r_kind), private :: psi(npoly)

! These SSMIS values are the average of the 0 and 90 deg cuts as determined by
! plot_test_conical_fov.f90
 real(r_kind), private, dimension(nchan,instrumentrange(1):instrumentrange(2))   &
                                  :: fovangle   =  reshape ( (/                  &
 			 .7950_r_kind, .7950_r_kind, .8350_r_kind, .8000_r_kind, & 
                         .7725_r_kind, .7000_r_kind, .7150_r_kind, .4050_r_kind, &
			 .3650_r_kind, .3650_r_kind, .3650_r_kind,1.9450_r_kind, &
			2.0975_r_kind,1.8550_r_kind,1.2100_r_kind,1.2275_r_kind, &
                         .4125_r_kind, .4000_r_kind, .6600_r_kind, .6925_r_kind, &
			 .6925_r_kind, .6925_r_kind, .6925_r_kind, .6925_r_kind, &
                        0.7900_r_kind,0.7800_r_kind,0.7850_r_kind,0.7900_r_kind, &
                        0.7750_r_kind,0.7150_r_kind,0.7000_r_kind,0.4225_r_kind, &
			0.3825_r_kind,0.3825_r_kind,0.3825_r_kind,1.9300_r_kind, &
                        1.9125_r_kind,1.8550_r_kind,1.2025_r_kind,1.2150_r_kind, &
                        0.4175_r_kind,0.4100_r_kind,0.6775_r_kind,0.6950_r_kind, &
			0.6950_r_kind,0.6950_r_kind,0.6950_r_kind,0.6950_r_kind, &
                        0.7775_r_kind,0.7825_r_kind,0.7675_r_kind,0.7750_r_kind, &
                        0.8674_r_kind,0.7125_r_kind,0.7050_r_kind,0.3975_r_kind, &
			0.4625_r_kind,0.4625_r_kind,0.4625_r_kind,1.9100_r_kind, &
                        1.9450_r_kind,1.8920_r_kind,1.2120_r_kind,1.1950_r_kind, &
                        0.4050_r_kind,0.4175_r_kind,0.6800_r_kind,0.7025_r_kind, &
			0.7025_r_kind,0.7025_r_kind,0.7025_r_kind,0.7025_r_kind, &
                        0.8400_r_kind,0.8125_r_kind,0.8225_r_kind,0.8075_r_kind, &
                        0.8375_r_kind,0.7025_r_kind,0.6950_r_kind,0.3875_r_kind, &
			0.4425_r_kind,0.4425_r_kind,0.4425_r_kind,1.8975_r_kind, &
			1.9075_r_kind,1.8700_r_kind,1.2300_r_kind,1.1950_r_kind, &
                        0.4150_r_kind,0.4125_r_kind,0.7025_r_kind,0.6550_r_kind, &
			0.6550_r_kind,0.6550_r_kind,0.6550_r_kind,0.6550_r_kind, &
			0.8075_r_kind,0.7900_r_kind,0.7725_r_kind,0.7625_r_kind, &
                        0.8000_r_kind,0.7050_r_kind,0.6975_r_kind,0.4275_r_kind, &
			0.3750_r_kind,0.3750_r_kind,0.3750_r_kind,1.8725_r_kind, &
			1.8750_r_kind,1.8525_r_kind,1.1400_r_kind,1.1875_r_kind, &
                        0.4050_r_kind,0.4200_r_kind,0.7050_r_kind,0.7225_r_kind, &
			0.7225_r_kind,0.7225_r_kind,0.7225_r_kind,0.7225_r_kind/) , (/nchan,maxinstr/) )

 real(r_kind), private, pointer, dimension(:,:,:) :: ssmiscoeff 

 real(r_kind), private, target, dimension(0:7,2,nchan) :: ssmiscoeff_26 = reshape( (/    &
   0.0000000e+000_r_kind,  -2.0421422e+000_r_kind,  -1.8475670e+001_r_kind,   5.8127385e-001_r_kind, &
  -7.0556941e+000_r_kind,  -1.2992365e+001_r_kind,  -6.5335264e+000_r_kind,  -1.4934616e+000_r_kind, &
   0.0000000e+000_r_kind,   1.0068195e+000_r_kind,  -1.8621782e+001_r_kind,   7.0600611e-001_r_kind, &
  -6.4058743e+000_r_kind,  -1.6801881e+000_r_kind,   1.6044432e-001_r_kind,   1.9570413e-001_r_kind, &
   0.0000000e+000_r_kind,  -2.0343354e+000_r_kind,  -1.8820206e+001_r_kind,  -2.0464630e-001_r_kind, &
  -5.8437357e+000_r_kind,  -1.2724338e+001_r_kind,  -7.5160422e+000_r_kind,  -1.7063006e+000_r_kind, &
   0.0000000e+000_r_kind,   1.2394649e+000_r_kind,  -1.7794115e+001_r_kind,   7.1650553e-001_r_kind, &
  -1.0076857e+001_r_kind,  -8.8283193e-001_r_kind,   4.1311064e+000_r_kind,   3.2674277e-001_r_kind, &
   0.0000000e+000_r_kind,   1.1069076e+000_r_kind,  -1.6729525e+001_r_kind,   3.2541008e+000_r_kind, &
  -4.3188162e+000_r_kind,  -1.2228760e+001_r_kind,  -5.4997191e+000_r_kind,  -6.4083195e-001_r_kind, &
   0.0000000e+000_r_kind,   9.1011721e-001_r_kind,  -1.5002439e+001_r_kind,   8.3682829e-001_r_kind, &
  -1.4865303e+001_r_kind,  -1.4518541e+000_r_kind,   6.3205481e+000_r_kind,   8.9516824e-001_r_kind, &
   0.0000000e+000_r_kind,  -2.0301795e+000_r_kind,  -1.7520290e+001_r_kind,  -2.0359952e+000_r_kind, &
  -1.5126715e+001_r_kind,  -1.7495092e+001_r_kind,  -2.6604106e+000_r_kind,   1.6044024e+000_r_kind, &
   0.0000000e+000_r_kind,   1.1353749e+000_r_kind,  -1.7554970e+001_r_kind,   3.7382832e+000_r_kind, &
  -1.1544557e+001_r_kind,  -7.0454316e+000_r_kind,   3.4665370e+000_r_kind,   4.7528372e+000_r_kind, &
   0.0000000e+000_r_kind,  -2.2901790e+000_r_kind,  -2.0729614e+001_r_kind,  -1.5420046e+000_r_kind, &
  -3.1637704e+000_r_kind,  -1.1189982e+001_r_kind,  -9.5094662e+000_r_kind,  -2.7242188e+000_r_kind, &
   0.0000000e+000_r_kind,   3.6689255e-001_r_kind,  -1.9364031e+001_r_kind,   2.6905188e-001_r_kind, &
  -7.8406730e+000_r_kind,  -4.4513118e-001_r_kind,   3.1332321e+000_r_kind,   7.8229588e-001_r_kind, &
   0.0000000e+000_r_kind,   1.0386219e+000_r_kind,  -2.4594749e+001_r_kind,   1.5004185e+000_r_kind, &
  -6.0441451e+000_r_kind,   1.0094608e+001_r_kind,  -7.5647199e-001_r_kind,  -1.0276575e+000_r_kind, &
   0.0000000e+000_r_kind,  -6.0515773e-001_r_kind,  -2.5771051e+001_r_kind,   3.1947966e+000_r_kind, &
   8.7908926e+000_r_kind,  -3.4560359e+000_r_kind,  -2.3648663e+000_r_kind,   5.7246488e-001_r_kind, &
   0.0000000e+000_r_kind,   1.0671118e+000_r_kind,  -2.2004122e+001_r_kind,   2.3684590e+000_r_kind, &
  -1.4672252e+001_r_kind,   1.4386868e+001_r_kind,   3.5601823e+000_r_kind,  -4.0438247e+000_r_kind, &
   0.0000000e+000_r_kind,  -1.5223384e+000_r_kind,  -2.3667862e+001_r_kind,  -3.1768622e+000_r_kind, &
  -1.1866217e+001_r_kind,   1.4418871e+000_r_kind,   4.0313168e+000_r_kind,  -2.6718433e+000_r_kind, &
   0.0000000e+000_r_kind,   1.0933807e+000_r_kind,  -4.5624649e+001_r_kind,   3.1849602e+001_r_kind, &
  -3.4993677e+002_r_kind,  -5.8365466e+002_r_kind,   8.3752588e+002_r_kind,   1.3366458e+003_r_kind, &
   0.0000000e+000_r_kind,  -3.6622078e+000_r_kind,  -1.0634037e+002_r_kind,  -7.1558281e+001_r_kind, &
   1.2876265e+002_r_kind,   3.2541953e+002_r_kind,  -2.1310993e+001_r_kind,  -3.3278876e+002_r_kind, &
   0.0000000e+000_r_kind,  -8.1931009e+000_r_kind,  -9.6183205e+001_r_kind,   8.3611893e+001_r_kind, &
  -2.9098847e+000_r_kind,  -4.4306622e+002_r_kind,   1.5092833e+002_r_kind,   1.2897216e+003_r_kind, &
   0.0000000e+000_r_kind,   3.5744536e+000_r_kind,  -9.2921333e+001_r_kind,  -1.2623575e+002_r_kind, &
   9.1476303e+001_r_kind,   4.5832971e+002_r_kind,   2.5282922e+000_r_kind,  -4.5760828e+002_r_kind, &
   0.0000000e+000_r_kind,  -8.1931009e+000_r_kind,  -9.6183205e+001_r_kind,   8.3611893e+001_r_kind, &
  -2.9098847e+000_r_kind,  -4.4306622e+002_r_kind,   1.5092833e+002_r_kind,   1.2897216e+003_r_kind, &
   0.0000000e+000_r_kind,   3.5744536e+000_r_kind,  -9.2921333e+001_r_kind,  -1.2623575e+002_r_kind, &
   9.1476303e+001_r_kind,   4.5832971e+002_r_kind,   2.5282922e+000_r_kind,  -4.5760828e+002_r_kind, &
   0.0000000e+000_r_kind,  -8.1931009e+000_r_kind,  -9.6183205e+001_r_kind,   8.3611893e+001_r_kind, &
  -2.9098847e+000_r_kind,  -4.4306622e+002_r_kind,   1.5092833e+002_r_kind,   1.2897216e+003_r_kind, &
   0.0000000e+000_r_kind,   3.5744536e+000_r_kind,  -9.2921333e+001_r_kind,  -1.2623575e+002_r_kind, &
   9.1476303e+001_r_kind,   4.5832971e+002_r_kind,   2.5282922e+000_r_kind,  -4.5760828e+002_r_kind, &
   0.0000000e+000_r_kind,   3.0958306e-002_r_kind,  -3.1592178e+000_r_kind,   3.8233291e-002_r_kind, &
  -2.7496846e-002_r_kind,   4.5768835e-002_r_kind,  -4.2374048e-002_r_kind,   1.0417381e-002_r_kind, &
   0.0000000e+000_r_kind,   2.0846614e-001_r_kind,  -2.9599710e+000_r_kind,  -1.3269909e-001_r_kind, &
  -2.0445679e-001_r_kind,   7.5261474e-002_r_kind,   4.2130649e-003_r_kind,  -1.0547909e-002_r_kind, &
   0.0000000e+000_r_kind,  -1.5602320e-001_r_kind,  -1.9116079e+000_r_kind,   1.3854431e-001_r_kind, &
  -7.3373616e-001_r_kind,   1.4142011e-001_r_kind,   7.3796161e-002_r_kind,  -1.8763458e-002_r_kind, &
   0.0000000e+000_r_kind,   9.2653804e-002_r_kind,  -2.5388041e+000_r_kind,  -1.3895155e-001_r_kind, &
  -3.0783531e-001_r_kind,   7.3903047e-002_r_kind,   1.3743081e-002_r_kind,  -1.1820830e-002_r_kind, &
   0.0000000e+000_r_kind,   8.8804968e-002_r_kind,  -3.5472960e+000_r_kind,   1.4374045e-001_r_kind, &
   1.1865481e-002_r_kind,   4.3875087e-002_r_kind,  -4.2952936e-002_r_kind,   9.1003580e-003_r_kind, &
   0.0000000e+000_r_kind,   3.1733221e-001_r_kind,  -3.3926344e+000_r_kind,  -1.0685296e-001_r_kind, &
  -8.9582920e-002_r_kind,   6.8896666e-002_r_kind,   4.4185176e-004_r_kind,  -7.2034262e-003_r_kind, &
   0.0000000e+000_r_kind,  -7.8170049e-001_r_kind,  -7.8873696e+000_r_kind,  -1.1268061e+000_r_kind, &
  -6.9391781e-001_r_kind,  -4.0826172e-001_r_kind,  -4.1230079e-002_r_kind,   1.5049034e-002_r_kind, &
   0.0000000e+000_r_kind,   5.3912395e-001_r_kind,  -8.6529970e+000_r_kind,  -2.8816953e-001_r_kind, &
  -2.9525390e-001_r_kind,   1.7211132e-001_r_kind,   4.6021707e-002_r_kind,  -7.0339635e-002_r_kind, &
   0.0000000e+000_r_kind,   3.6837742e-001_r_kind,  -7.5171447e+000_r_kind,  -5.0336015e-001_r_kind, &
  -3.6997813e-001_r_kind,  -4.9953908e-001_r_kind,  -1.2228214e-001_r_kind,   3.7737708e-003_r_kind, &
   0.0000000e+000_r_kind,  -3.5959744e-001_r_kind,  -8.2851095e+000_r_kind,  -2.6963434e-001_r_kind, &
  -3.4499434e-001_r_kind,   1.7260285e-001_r_kind,   5.2084908e-002_r_kind,  -5.8956187e-002_r_kind, &
   0.0000000e+000_r_kind,  -2.3420448e+000_r_kind,  -6.0383373e+001_r_kind,  -9.0748375e+001_r_kind, &
  -1.7785561e+002_r_kind,   8.3703729e+002_r_kind,   2.4984094e+002_r_kind,  -1.8838197e+003_r_kind, &
   0.0000000e+000_r_kind,   4.0578790e+000_r_kind,  -8.4118553e+001_r_kind,   5.9846531e+001_r_kind, &
   1.6410830e+001_r_kind,  -3.5211914e+002_r_kind,  -4.9207938e+002_r_kind,   9.3371954e+002_r_kind, &
   0.0000000e+000_r_kind,  -7.4266438e+000_r_kind,  -7.8480431e+001_r_kind,   2.0624901e+001_r_kind, &
  -4.3978077e+001_r_kind,  -2.6610355e+002_r_kind,  -1.6364496e+002_r_kind,   1.0066548e+003_r_kind, &
   0.0000000e+000_r_kind,   1.6968371e+000_r_kind,  -7.1473190e+001_r_kind,   8.2124586e+000_r_kind, &
  -6.8828499e+001_r_kind,  -4.9714859e+001_r_kind,  -3.5048290e+001_r_kind,   7.4115654e+001_r_kind, &
   0.0000000e+000_r_kind,   2.7627389e+000_r_kind,  -2.6941221e+001_r_kind,   1.0671624e+000_r_kind, &
  -1.8025070e+001_r_kind,   2.4485531e+001_r_kind,   3.2902256e-001_r_kind,  -4.8242216e+000_r_kind, &
   0.0000000e+000_r_kind,   1.3322572e-002_r_kind,  -2.7592098e+001_r_kind,  -3.7608089e+000_r_kind, &
  -7.5215788e+000_r_kind,   3.8176088e+000_r_kind,   1.9556910e+000_r_kind,  -2.9116702e+000_r_kind, &
   0.0000000e+000_r_kind,   2.4362673e-001_r_kind,  -2.3671696e+001_r_kind,   1.9867196e+000_r_kind, &
  -1.5356319e+001_r_kind,   1.9979763e+001_r_kind,  -1.5665739e+000_r_kind,  -2.4290712e+000_r_kind, &
   0.0000000e+000_r_kind,  -1.3031545e+000_r_kind,  -2.4348703e+001_r_kind,  -2.8804038e+000_r_kind, &
  -1.2756769e+001_r_kind,   1.9352788e+000_r_kind,   5.8430328e+000_r_kind,  -1.0966378e+000_r_kind, &
   0.0000000e+000_r_kind,   2.4362673e-001_r_kind,  -2.3671696e+001_r_kind,   1.9867196e+000_r_kind, &
  -1.5356319e+001_r_kind,   1.9979763e+001_r_kind,  -1.5665739e+000_r_kind,  -2.4290712e+000_r_kind, &
   0.0000000e+000_r_kind,  -1.3031545e+000_r_kind,  -2.4348703e+001_r_kind,  -2.8804038e+000_r_kind, &
  -1.2756769e+001_r_kind,   1.9352788e+000_r_kind,   5.8430328e+000_r_kind,  -1.0966378e+000_r_kind, &
   0.0000000e+000_r_kind,   2.4362673e-001_r_kind,  -2.3671696e+001_r_kind,   1.9867196e+000_r_kind, &
  -1.5356319e+001_r_kind,   1.9979763e+001_r_kind,  -1.5665739e+000_r_kind,  -2.4290712e+000_r_kind, &
   0.0000000e+000_r_kind,  -1.3031545e+000_r_kind,  -2.4348703e+001_r_kind,  -2.8804038e+000_r_kind, &
  -1.2756769e+001_r_kind,   1.9352788e+000_r_kind,   5.8430328e+000_r_kind,  -1.0966378e+000_r_kind, &
   0.0000000e+000_r_kind,   2.4362673e-001_r_kind,  -2.3671696e+001_r_kind,   1.9867196e+000_r_kind, &
  -1.5356319e+001_r_kind,   1.9979763e+001_r_kind,  -1.5665739e+000_r_kind,  -2.4290712e+000_r_kind, &
   0.0000000e+000_r_kind,  -1.3031545e+000_r_kind,  -2.4348703e+001_r_kind,  -2.8804038e+000_r_kind, &
  -1.2756769e+001_r_kind,   1.9352788e+000_r_kind,   5.8430328e+000_r_kind,  -1.0966378e+000_r_kind, &
   0.0000000e+000_r_kind,   2.4362673e-001_r_kind,  -2.3671696e+001_r_kind,   1.9867196e+000_r_kind, &
  -1.5356319e+001_r_kind,   1.9979763e+001_r_kind,  -1.5665739e+000_r_kind,  -2.4290712e+000_r_kind, &
   0.0000000e+000_r_kind,  -1.3031545e+000_r_kind,  -2.4348703e+001_r_kind,  -2.8804038e+000_r_kind, &
  -1.2756769e+001_r_kind,   1.9352788e+000_r_kind,   5.8430328e+000_r_kind,  -1.0966378e+000_r_kind/), &
      (/ 8,2,nchan/) )
 
 real(r_kind), private, target, dimension(0:7,2,nchan) :: ssmiscoeff_27 = reshape( (/  &
   0.0000000e+000_r_kind,  -1.3646689e-001_r_kind,  -1.7491594e+001_r_kind,  -1.4631690e+000_r_kind, & 
  -7.5684819e+000_r_kind,  -7.6566257e+000_r_kind,  -6.6404605e-001_r_kind,   3.2459021e-001_r_kind, &
   0.0000000e+000_r_kind,   7.6307529e-001_r_kind,  -1.9646463e+001_r_kind,   2.0191465e-001_r_kind, &
  -6.4181051e+000_r_kind,   1.4203938e+000_r_kind,   1.3550677e+000_r_kind,   5.3587806e-001_r_kind, &
   0.0000000e+000_r_kind,  -7.9532194e-001_r_kind,  -1.9742596e+001_r_kind,  -6.2726033e-001_r_kind, &
  -2.0304644e+000_r_kind,  -7.4279966e+000_r_kind,  -8.0835075e+000_r_kind,  -3.7774084e+000_r_kind, &
   0.0000000e+000_r_kind,   4.6107411e-001_r_kind,  -1.9117405e+001_r_kind,   1.2998869e+000_r_kind, &
  -7.6979017e+000_r_kind,  -2.0893183e+000_r_kind,   2.0465825e+000_r_kind,   2.8866854e+000_r_kind, &
   0.0000000e+000_r_kind,  -7.1036977e-001_r_kind,  -1.9012058e+001_r_kind,  -7.0765209e-001_r_kind, &
  -5.7102566e+000_r_kind,  -9.1604614e+000_r_kind,  -4.3605328e+000_r_kind,  -1.3313843e+000_r_kind, &
   0.0000000e+000_r_kind,   3.1243098e-001_r_kind,  -1.8528467e+001_r_kind,   1.9728154e+000_r_kind, &
  -8.2097015e+000_r_kind,  -3.0031221e+000_r_kind,   2.4967330e+000_r_kind,   2.4759190e+000_r_kind, &
   0.0000000e+000_r_kind,  -6.2438941e-001_r_kind,  -2.0402948e+001_r_kind,  -1.5155400e+000_r_kind, &
  -5.1162057e+000_r_kind,  -9.3205929e+000_r_kind,  -5.5283504e+000_r_kind,  -1.7135507e+000_r_kind, &
   0.0000000e+000_r_kind,   7.6732087e-001_r_kind,  -1.5912438e+001_r_kind,  -2.5489950e-001_r_kind, &
  -1.4004007e+001_r_kind,   2.5863705e+000_r_kind,   5.9506993e+000_r_kind,  -7.9862869e-001_r_kind, &
   0.0000000e+000_r_kind,  -7.6770180e-001_r_kind,  -2.1217270e+001_r_kind,  -1.2007523e+000_r_kind, &
  -4.4100690e+000_r_kind,  -1.0881293e+001_r_kind,  -6.7093534e+000_r_kind,  -1.7030925e+000_r_kind, &
   0.0000000e+000_r_kind,   2.9133058e-001_r_kind,  -1.7666773e+001_r_kind,   9.4997086e-002_r_kind, &
  -1.0238405e+001_r_kind,   2.9632556e+000_r_kind,   4.4156651e+000_r_kind,  -1.4097084e+000_r_kind, &
   0.0000000e+000_r_kind,   2.2738552e+000_r_kind,  -2.1972227e+001_r_kind,   8.8293689e-001_r_kind, &
  -1.3470187e+001_r_kind,   2.0145281e+001_r_kind,  -3.9182999e+000_r_kind,  -2.2199464e+000_r_kind, &
   0.0000000e+000_r_kind,   2.2542253e+000_r_kind,  -2.4955254e+001_r_kind,   5.7870060e-001_r_kind, &
  -2.7696946e+000_r_kind,   2.6180410e+000_r_kind,  -5.5245268e-001_r_kind,  -1.8563029e-001_r_kind, &
   0.0000000e+000_r_kind,  -1.7110617e+000_r_kind,  -2.6025618e+001_r_kind,   7.6529276e-001_r_kind, &
   2.1126766e+000_r_kind,   1.1818262e+001_r_kind,  -1.7023336e+001_r_kind,   8.0484543e+000_r_kind, &
   0.0000000e+000_r_kind,  -1.5494124e+000_r_kind,  -2.3606911e+001_r_kind,  -5.7789793e+000_r_kind, &
  -8.2072773e+000_r_kind,   8.6932020e+000_r_kind,   2.1572309e+000_r_kind,  -6.1711001e+000_r_kind, &
   0.0000000e+000_r_kind,   1.9367024e+000_r_kind,  -7.1905418e+001_r_kind,   1.8186728e+002_r_kind, &
  -1.0284077e+002_r_kind,  -1.3280175e+003_r_kind,   2.2840768e+002_r_kind,   2.0377109e+003_r_kind, &
   0.0000000e+000_r_kind,   2.6490505e+000_r_kind,  -1.0113955e+002_r_kind,   2.4378333e+002_r_kind, &
   2.1187120e+002_r_kind,  -1.8063729e+003_r_kind,  -4.5114478e+002_r_kind,   3.7692583e+003_r_kind, &
   0.0000000e+000_r_kind,   7.8743153e+000_r_kind,  -9.7176628e+001_r_kind,  -3.6019478e+000_r_kind, &
   6.4251077e-001_r_kind,  -2.5934351e+002_r_kind,   1.8840852e+002_r_kind,   4.6089346e+002_r_kind, &
   0.0000000e+000_r_kind,   3.7559617e+000_r_kind,  -7.2472275e+001_r_kind,   2.2125927e+001_r_kind, &
  -1.6764737e+002_r_kind,  -4.5280774e+002_r_kind,   5.1139508e+002_r_kind,   1.0117816e+003_r_kind, &
   0.0000000e+000_r_kind,   7.8743153e+000_r_kind,  -9.7176628e+001_r_kind,  -3.6019478e+000_r_kind, &
   6.4251077e-001_r_kind,  -2.5934351e+002_r_kind,   1.8840852e+002_r_kind,   4.6089346e+002_r_kind, &
   0.0000000e+000_r_kind,   3.7559617e+000_r_kind,  -7.2472275e+001_r_kind,   2.2125927e+001_r_kind, &
  -1.6764737e+002_r_kind,  -4.5280774e+002_r_kind,   5.1139508e+002_r_kind,   1.0117816e+003_r_kind, &
   0.0000000e+000_r_kind,   7.8743153e+000_r_kind,  -9.7176628e+001_r_kind,  -3.6019478e+000_r_kind, &
   6.4251077e-001_r_kind,  -2.5934351e+002_r_kind,   1.8840852e+002_r_kind,   4.6089346e+002_r_kind, &
   0.0000000e+000_r_kind,   3.7559617e+000_r_kind,  -7.2472275e+001_r_kind,   2.2125927e+001_r_kind, &
  -1.6764737e+002_r_kind,  -4.5280774e+002_r_kind,   5.1139508e+002_r_kind,   1.0117816e+003_r_kind, &
   0.0000000e+000_r_kind,   1.8266505e-001_r_kind,  -3.3461845e+000_r_kind,   5.3636897e-002_r_kind, &
   4.2429578e-002_r_kind,   4.3490700e-002_r_kind,  -5.3845055e-002_r_kind,   1.2972679e-002_r_kind, &
   0.0000000e+000_r_kind,   1.8891554e-001_r_kind,  -2.9229119e+000_r_kind,  -6.3320272e-002_r_kind, &
  -2.3943205e-001_r_kind,   4.6603668e-002_r_kind,   1.2278236e-002_r_kind,  -6.1170841e-003_r_kind, &
   0.0000000e+000_r_kind,   1.9350249e-001_r_kind,  -3.2152145e+000_r_kind,   3.1346075e-002_r_kind, &
  -2.8084269e-002_r_kind,   5.7417762e-002_r_kind,  -3.9081186e-002_r_kind,   9.0511842e-003_r_kind, &
   0.0000000e+000_r_kind,   1.5680872e-001_r_kind,  -3.2230101e+000_r_kind,   4.4403723e-003_r_kind, &
  -1.3255835e-001_r_kind,   1.1291061e-002_r_kind,  -8.9034922e-003_r_kind,  -4.2807843e-004_r_kind, &
   0.0000000e+000_r_kind,   3.6579531e-001_r_kind,  -3.4859917e+000_r_kind,   1.2930833e-001_r_kind, &
  -5.8087360e-002_r_kind,   7.1415007e-002_r_kind,  -3.8046870e-002_r_kind,   7.5299572e-003_r_kind, &
   0.0000000e+000_r_kind,   6.3857608e-002_r_kind,  -3.4254475e+000_r_kind,   7.3168203e-002_r_kind, &
  -9.7979978e-002_r_kind,   3.8355000e-003_r_kind,  -3.0666450e-003_r_kind,  -2.6641181e-004_r_kind, &
   0.0000000e+000_r_kind,   6.0623950e-001_r_kind,  -8.4134035e+000_r_kind,  -3.6488310e-001_r_kind, &
   3.9826405e-001_r_kind,  -3.2095316e-001_r_kind,  -2.7543458e-001_r_kind,  -5.2250728e-002_r_kind, &
   0.0000000e+000_r_kind,   7.4535280e-001_r_kind,  -8.1254025e+000_r_kind,  -1.8645565e-001_r_kind, &
  -7.9415834e-001_r_kind,   1.1349876e-001_r_kind,   1.2804832e-001_r_kind,  -3.3321097e-002_r_kind, &
   0.0000000e+000_r_kind,   4.1089869e-001_r_kind,  -7.8023496e+000_r_kind,  -6.3169289e-001_r_kind, &
  -3.8937669e-002_r_kind,  -2.4086182e-001_r_kind,  -2.4392936e-001_r_kind,  -7.5236328e-002_r_kind, &
   0.0000000e+000_r_kind,   4.6313271e-001_r_kind,  -8.4078159e+000_r_kind,  -1.1982609e-001_r_kind, &
  -4.3372706e-001_r_kind,  -4.8598129e-002_r_kind,   4.7060594e-002_r_kind,   2.4245718e-002_r_kind, &
   0.0000000e+000_r_kind,  -6.4666977e+000_r_kind,  -6.6049683e+001_r_kind,   6.0914874e+000_r_kind, &
  -6.2315540e+001_r_kind,  -2.9783438e+001_r_kind,  -1.1772702e+002_r_kind,   4.5344077e+002_r_kind, &
   0.0000000e+000_r_kind,  -2.8644288e+000_r_kind,  -6.8392441e+001_r_kind,  -2.3870790e+000_r_kind, &
  -1.3848622e+002_r_kind,  -1.8050613e+002_r_kind,   1.3202374e+002_r_kind,   1.3246623e+002_r_kind, &
   0.0000000e+000_r_kind,   7.0280061e+000_r_kind,  -7.6730682e+001_r_kind,   9.5418425e+000_r_kind, &
  -4.4003113e+001_r_kind,   1.6520891e+002_r_kind,  -5.2304480e+002_r_kind,   5.2930609e+002_r_kind, &
   0.0000000e+000_r_kind,  -1.3385438e+000_r_kind,  -7.3685303e+001_r_kind,  -3.2390606e+001_r_kind, &
  -1.6800714e+001_r_kind,   1.9815450e+002_r_kind,  -2.7483936e+002_r_kind,  -9.0628497e+002_r_kind, &
   0.0000000e+000_r_kind,  -1.4215437e-001_r_kind,  -2.7673775e+001_r_kind,   2.2236590e+000_r_kind, &
   3.4517300e+000_r_kind,   6.7635159e+000_r_kind,  -1.3662134e+001_r_kind,   5.8717651e+000_r_kind, &
   0.0000000e+000_r_kind,  -2.2137783e+000_r_kind,  -2.3649857e+001_r_kind,  -4.5962830e+000_r_kind, &
  -2.3325146e+001_r_kind,   9.7882700e+000_r_kind,   1.8784193e+001_r_kind,  -4.0483398e+000_r_kind, &
   0.0000000e+000_r_kind,  -1.9054600e+000_r_kind,  -2.4767467e+001_r_kind,   2.2741683e+000_r_kind, &
   2.1102891e+000_r_kind,   5.7827330e+000_r_kind,  -1.3349512e+001_r_kind,   7.7343783e+000_r_kind, &
   0.0000000e+000_r_kind,   2.2701132e+000_r_kind,  -2.4232327e+001_r_kind,   3.1316319e+000_r_kind, &
  -1.8675348e+001_r_kind,  -3.3524268e+000_r_kind,   1.3261509e+001_r_kind,   2.3453029e-001_r_kind, &
   0.0000000e+000_r_kind,  -1.9054600e+000_r_kind,  -2.4767467e+001_r_kind,   2.2741683e+000_r_kind, &
   2.1102891e+000_r_kind,   5.7827330e+000_r_kind,  -1.3349512e+001_r_kind,   7.7343783e+000_r_kind, &
   0.0000000e+000_r_kind,   2.2701132e+000_r_kind,  -2.4232327e+001_r_kind,   3.1316319e+000_r_kind, &
  -1.8675348e+001_r_kind,  -3.3524268e+000_r_kind,   1.3261509e+001_r_kind,   2.3453029e-001_r_kind, &
   0.0000000e+000_r_kind,  -1.9054600e+000_r_kind,  -2.4767467e+001_r_kind,   2.2741683e+000_r_kind, &
   2.1102891e+000_r_kind,   5.7827330e+000_r_kind,  -1.3349512e+001_r_kind,   7.7343783e+000_r_kind, &
   0.0000000e+000_r_kind,   2.2701132e+000_r_kind,  -2.4232327e+001_r_kind,   3.1316319e+000_r_kind, &
  -1.8675348e+001_r_kind,  -3.3524268e+000_r_kind,   1.3261509e+001_r_kind,   2.3453029e-001_r_kind, &
   0.0000000e+000_r_kind,  -1.9054600e+000_r_kind,  -2.4767467e+001_r_kind,   2.2741683e+000_r_kind, &
   2.1102891e+000_r_kind,   5.7827330e+000_r_kind,  -1.3349512e+001_r_kind,   7.7343783e+000_r_kind, &
   0.0000000e+000_r_kind,   2.2701132e+000_r_kind,  -2.4232327e+001_r_kind,   3.1316319e+000_r_kind, &
  -1.8675348e+001_r_kind,  -3.3524268e+000_r_kind,   1.3261509e+001_r_kind,   2.3453029e-001_r_kind, &
   0.0000000e+000_r_kind,  -1.9054600e+000_r_kind,  -2.4767467e+001_r_kind,   2.2741683e+000_r_kind, &
   2.1102891e+000_r_kind,   5.7827330e+000_r_kind,  -1.3349512e+001_r_kind,   7.7343783e+000_r_kind, &
   0.0000000e+000_r_kind,   2.2701132e+000_r_kind,  -2.4232327e+001_r_kind,   3.1316319e+000_r_kind, &
  -1.8675348e+001_r_kind,  -3.3524268e+000_r_kind,   1.3261509e+001_r_kind,   2.3453029e-001_r_kind/), & 
      (/ 8,2,nchan/) )

 real(r_kind), private, target, dimension(0:7,2,nchan) :: ssmiscoeff_28 = reshape( (/ &
   0.0000000e+000_r_kind,   2.0570794e-001_r_kind,  -1.9492340e+001_r_kind,  -7.6119816e-001_r_kind, & 
  -4.2826099e+000_r_kind,  -6.4894390e+000_r_kind,  -2.7921066e+000_r_kind,  -1.0449085e+000_r_kind, &
   0.0000000e+000_r_kind,   7.9341185e-001_r_kind,  -1.8895180e+001_r_kind,   5.6425661e-001_r_kind, &
  -6.8573856e+000_r_kind,  -2.7626008e-001_r_kind,   1.0872771e+000_r_kind,   3.7025112e-001_r_kind, &
   0.0000000e+000_r_kind,   2.8792736e-001_r_kind,  -1.8585211e+001_r_kind,   1.3555159e-001_r_kind, &
  -8.4083433e+000_r_kind,  -1.1403633e+001_r_kind,  -1.2586716e+000_r_kind,   1.1006272e+000_r_kind, &
   0.0000000e+000_r_kind,   4.5986116e-002_r_kind,  -1.8444597e+001_r_kind,   8.3667862e-001_r_kind, &
  -8.4577694e+000_r_kind,  -4.4495186e-001_r_kind,   1.8835176e+000_r_kind,  -8.1300542e-002_r_kind, &
   0.0000000e+000_r_kind,  -2.1122069e+000_r_kind,  -2.0122660e+001_r_kind,  -2.3027046e+000_r_kind, &
  -6.0809941e+000_r_kind,  -9.9340944e+000_r_kind,  -7.0323119e+000_r_kind,  -2.5341692e+000_r_kind, &
   0.0000000e+000_r_kind,   6.1376721e-001_r_kind,  -1.9993145e+001_r_kind,   1.0280871e+000_r_kind, &
  -8.6834974e+000_r_kind,  -2.3503625e+000_r_kind,   2.1337285e+000_r_kind,   6.3782203e-001_r_kind, &
   0.0000000e+000_r_kind,  -1.9532768e+000_r_kind,  -1.9621267e+001_r_kind,  -3.9438715e+000_r_kind, &
  -1.1919080e+001_r_kind,  -9.1981249e+000_r_kind,   1.3615044e+000_r_kind,   1.6593552e+000_r_kind, &
   0.0000000e+000_r_kind,   1.0395137e+000_r_kind,  -1.8802746e+001_r_kind,   1.9742522e+000_r_kind, &
  -1.2060226e+001_r_kind,  -3.7792008e+000_r_kind,   5.3513527e+000_r_kind,   1.6459979e+000_r_kind, &
   0.0000000e+000_r_kind,  -5.0596690e-001_r_kind,  -7.1553793e+000_r_kind,  -8.3692141e+000_r_kind, &
  -6.1301308e+001_r_kind,  -2.5097546e+001_r_kind,   4.8093712e+001_r_kind,   2.9619022e+001_r_kind, &
   0.0000000e+000_r_kind,   1.8852498e-002_r_kind,  -1.4736298e+001_r_kind,   1.5810119e+000_r_kind, &
  -1.2297031e+001_r_kind,  -1.5597023e+000_r_kind,   3.8528342e+000_r_kind,   3.5590562e-001_r_kind, &
   0.0000000e+000_r_kind,   1.4893959e+000_r_kind,  -2.3249767e+001_r_kind,   1.1899576e+000_r_kind, &
  -1.1915029e+001_r_kind,   1.5538939e+001_r_kind,   1.8791051e+000_r_kind,  -4.0227718e+000_r_kind, &
   0.0000000e+000_r_kind,  -1.3560814e+000_r_kind,  -2.2592892e+001_r_kind,  -2.7395062e+000_r_kind, &
  -1.0760350e+001_r_kind,   4.4008365e+000_r_kind,   3.4309959e+000_r_kind,  -5.3188500e+000_r_kind, &
   0.0000000e+000_r_kind,  -3.9845034e-001_r_kind,  -2.4144131e+001_r_kind,  -2.0474164e-001_r_kind, &
  -9.3550129e+000_r_kind,   1.6425165e+001_r_kind,  -7.1929663e-001_r_kind,  -3.1228340e+000_r_kind, &
   0.0000000e+000_r_kind,   5.6258380e-001_r_kind,  -2.2156683e+001_r_kind,  -1.3846273e+000_r_kind, &
  -1.2714151e+001_r_kind,   1.1731935e+000_r_kind,   5.4358530e+000_r_kind,  -2.9065890e+000_r_kind, &
   0.0000000e+000_r_kind,   5.4270353e+000_r_kind,  -1.0884671e+002_r_kind,   1.5316464e+002_r_kind, &
   5.3795416e+002_r_kind,  -1.3436436e+003_r_kind,  -2.3434673e+003_r_kind,   4.3166475e+003_r_kind, &
   0.0000000e+000_r_kind,  -4.0115494e-001_r_kind,  -9.1217171e+001_r_kind,  -9.5021843e+001_r_kind, &
  -2.4285288e+000_r_kind,   1.7251961e+002_r_kind,   2.3338638e+002_r_kind,   1.6270474e+002_r_kind, &
   0.0000000e+000_r_kind,   2.9441638e+000_r_kind,  -7.7502502e+001_r_kind,   1.1973157e+002_r_kind, &
   1.8658537e+002_r_kind,  -5.2266168e+002_r_kind,  -1.6395461e+003_r_kind,   2.0197260e+003_r_kind, &
   0.0000000e+000_r_kind,   2.3879592e+000_r_kind,  -4.8609634e+001_r_kind,   5.6553886e+001_r_kind, &
  -2.9753879e+002_r_kind,   2.6479669e+002_r_kind,   1.0089089e+003_r_kind,  -3.1226721e+003_r_kind, &
   0.0000000e+000_r_kind,   2.9441638e+000_r_kind,  -7.7502502e+001_r_kind,   1.1973157e+002_r_kind, &
   1.8658537e+002_r_kind,  -5.2266168e+002_r_kind,  -1.6395461e+003_r_kind,   2.0197260e+003_r_kind, &
   0.0000000e+000_r_kind,   2.3879592e+000_r_kind,  -4.8609634e+001_r_kind,   5.6553886e+001_r_kind, &
  -2.9753879e+002_r_kind,   2.6479669e+002_r_kind,   1.0089089e+003_r_kind,  -3.1226721e+003_r_kind, &
   0.0000000e+000_r_kind,   2.9441638e+000_r_kind,  -7.7502502e+001_r_kind,   1.1973157e+002_r_kind, &
   1.8658537e+002_r_kind,  -5.2266168e+002_r_kind,  -1.6395461e+003_r_kind,   2.0197260e+003_r_kind, &
   0.0000000e+000_r_kind,   2.3879592e+000_r_kind,  -4.8609634e+001_r_kind,   5.6553886e+001_r_kind, &
  -2.9753879e+002_r_kind,   2.6479669e+002_r_kind,   1.0089089e+003_r_kind,  -3.1226721e+003_r_kind, &
   0.0000000e+000_r_kind,  -2.3165885e-001_r_kind,  -3.3532021e+000_r_kind,   4.5839932e-002_r_kind, &
   6.0010750e-002_r_kind,   4.2515807e-002_r_kind,  -4.5826130e-002_r_kind,   9.9699562e-003_r_kind, &
   0.0000000e+000_r_kind,   3.4466746e-001_r_kind,  -3.1282275e+000_r_kind,  -1.0812374e-001_r_kind, &
  -1.6708182e-001_r_kind,   5.4002166e-002_r_kind,   4.7064982e-003_r_kind,  -7.9766633e-003_r_kind, &
   0.0000000e+000_r_kind,   3.6336908e-001_r_kind,  -3.2328796e+000_r_kind,   3.6219481e-002_r_kind, &
   2.0642634e-002_r_kind,   5.4954067e-002_r_kind,  -4.9871225e-002_r_kind,   1.0791362e-002_r_kind, &
   0.0000000e+000_r_kind,   3.0349955e-001_r_kind,  -2.9475000e+000_r_kind,  -8.3187059e-002_r_kind, &
  -2.3077631e-001_r_kind,   5.0809365e-002_r_kind,   1.1117456e-002_r_kind,  -7.3062922e-003_r_kind, &
   0.0000000e+000_r_kind,  -1.2583630e-001_r_kind,  -3.5378649e+000_r_kind,   2.2263686e-001_r_kind, &
  -8.7968864e-002_r_kind,   4.0010985e-002_r_kind,  -2.0845870e-002_r_kind,   5.1851445e-003_r_kind, &
   0.0000000e+000_r_kind,  -1.4947593e-001_r_kind,  -2.9632766e+000_r_kind,  -7.3179863e-002_r_kind, &
  -2.0202492e-001_r_kind,   3.0318748e-002_r_kind,   9.5973583e-003_r_kind,  -4.1733296e-003_r_kind, &
   0.0000000e+000_r_kind,  -8.3948714e-001_r_kind,  -8.0054512e+000_r_kind,  -9.3238717e-001_r_kind, &
  -2.4132523e-001_r_kind,  -2.9901129e-001_r_kind,  -2.1427314e-001_r_kind,  -4.9018297e-002_r_kind, &
   0.0000000e+000_r_kind,  -7.1775484e-001_r_kind,  -8.4051352e+000_r_kind,   2.4517737e-002_r_kind, &
  -7.7322978e-001_r_kind,  -1.3713906e-002_r_kind,   1.2749098e-001_r_kind,   4.5805253e-002_r_kind, &
   0.0000000e+000_r_kind,  -3.4207308e-001_r_kind,  -8.3024530e+000_r_kind,  -7.7922302e-001_r_kind, &
   7.1535856e-002_r_kind,  -1.9902948e-001_r_kind,  -2.2501387e-001_r_kind,  -5.9000120e-002_r_kind, &
   0.0000000e+000_r_kind,  -1.9112661e-001_r_kind,  -8.5674524e+000_r_kind,  -5.0346788e-002_r_kind, &
  -5.9935790e-001_r_kind,   4.7692221e-002_r_kind,   1.1758236e-001_r_kind,   1.4390506e-002_r_kind, &
   0.0000000e+000_r_kind,  -6.5184350e+000_r_kind,  -7.2203987e+001_r_kind,   5.3044048e+001_r_kind, &
  -1.0914716e+001_r_kind,  -2.5077600e+002_r_kind,  -1.9828600e+002_r_kind,   7.4696576e+002_r_kind, &
   0.0000000e+000_r_kind,  -7.4638619e+000_r_kind,  -8.1932907e+001_r_kind,  -1.1777075e+001_r_kind, &
   6.5498346e-001_r_kind,   2.3458975e+001_r_kind,  -3.4077051e+002_r_kind,  -1.6807849e+002_r_kind, &
   0.0000000e+000_r_kind,   6.5740843e+000_r_kind,  -7.6522293e+001_r_kind,   3.0943171e+001_r_kind, &
  -7.2000725e+001_r_kind,   1.4366963e+002_r_kind,  -7.3938210e+001_r_kind,  -1.0095850e+002_r_kind, &
   0.0000000e+000_r_kind,  -4.5244980e+000_r_kind,  -6.7658073e+001_r_kind,   2.8068810e+001_r_kind, &
  -7.4450378e+001_r_kind,  -3.1507153e+002_r_kind,  -6.0539570e+001_r_kind,   7.0202527e+002_r_kind, &
   0.0000000e+000_r_kind,  -1.4768767e+000_r_kind,  -2.5380819e+001_r_kind,   5.7130628e+000_r_kind, &
  -9.7443876e+000_r_kind,   7.5264950e+000_r_kind,  -3.5491523e-001_r_kind,   9.5710915e-001_r_kind, &
   0.0000000e+000_r_kind,  -2.0779033e+000_r_kind,  -2.5249126e+001_r_kind,  -2.3347569e+000_r_kind, &
  -8.9244280e+000_r_kind,   2.0736899e+000_r_kind,   4.1227179e+000_r_kind,  -2.2944684e+000_r_kind, &
   0.0000000e+000_r_kind,  -1.9373071e+000_r_kind,  -2.4036428e+001_r_kind,  -3.9531195e-001_r_kind, &
  -3.8098814e+000_r_kind,   1.0168166e+001_r_kind,  -3.6215985e+000_r_kind,   1.0253588e+000_r_kind, &
   0.0000000e+000_r_kind,  -2.0423832e+000_r_kind,  -2.4063471e+001_r_kind,   6.1321461e-001_r_kind, &
  -9.7925587e+000_r_kind,  -6.7504501e+000_r_kind,   1.2449081e+000_r_kind,   2.3537226e+000_r_kind, &
   0.0000000e+000_r_kind,  -1.9373071e+000_r_kind,  -2.4036428e+001_r_kind,  -3.9531195e-001_r_kind, &
  -3.8098814e+000_r_kind,   1.0168166e+001_r_kind,  -3.6215985e+000_r_kind,   1.0253588e+000_r_kind, &
   0.0000000e+000_r_kind,  -2.0423832e+000_r_kind,  -2.4063471e+001_r_kind,   6.1321461e-001_r_kind, &
  -9.7925587e+000_r_kind,  -6.7504501e+000_r_kind,   1.2449081e+000_r_kind,   2.3537226e+000_r_kind, &
   0.0000000e+000_r_kind,  -1.9373071e+000_r_kind,  -2.4036428e+001_r_kind,  -3.9531195e-001_r_kind, &
  -3.8098814e+000_r_kind,   1.0168166e+001_r_kind,  -3.6215985e+000_r_kind,   1.0253588e+000_r_kind, &
   0.0000000e+000_r_kind,  -2.0423832e+000_r_kind,  -2.4063471e+001_r_kind,   6.1321461e-001_r_kind, &
  -9.7925587e+000_r_kind,  -6.7504501e+000_r_kind,   1.2449081e+000_r_kind,   2.3537226e+000_r_kind, &
   0.0000000e+000_r_kind,  -1.9373071e+000_r_kind,  -2.4036428e+001_r_kind,  -3.9531195e-001_r_kind, &
  -3.8098814e+000_r_kind,   1.0168166e+001_r_kind,  -3.6215985e+000_r_kind,   1.0253588e+000_r_kind, &
   0.0000000e+000_r_kind,  -2.0423832e+000_r_kind,  -2.4063471e+001_r_kind,   6.1321461e-001_r_kind, &
  -9.7925587e+000_r_kind,  -6.7504501e+000_r_kind,   1.2449081e+000_r_kind,   2.3537226e+000_r_kind, &
   0.0000000e+000_r_kind,  -1.9373071e+000_r_kind,  -2.4036428e+001_r_kind,  -3.9531195e-001_r_kind, &
  -3.8098814e+000_r_kind,   1.0168166e+001_r_kind,  -3.6215985e+000_r_kind,   1.0253588e+000_r_kind, &
   0.0000000e+000_r_kind,  -2.0423832e+000_r_kind,  -2.4063471e+001_r_kind,   6.1321461e-001_r_kind, &
  -9.7925587e+000_r_kind,  -6.7504501e+000_r_kind,   1.2449081e+000_r_kind,   2.3537226e+000_r_kind/),& 
      (/ 8,2,nchan/) )

 real(r_kind), private, target, dimension(0:7,2,nchan) :: ssmiscoeff_29 = reshape( (/  &
   0.0000000e+000_r_kind,   1.5625407e+000_r_kind,  -1.6123783e+001_r_kind,  -9.1493206e+000_r_kind, & 
  -1.2375475e+001_r_kind,   1.9653361e+000_r_kind,   8.6359787e+000_r_kind,   2.3022599e+000_r_kind, &
   0.0000000e+000_r_kind,  -6.6403961e-001_r_kind,  -1.3765058e+001_r_kind,  -1.9172590e+000_r_kind, &
  -1.7816914e+001_r_kind,   3.9072070e-001_r_kind,   7.7860289e+000_r_kind,   7.8557414e-001_r_kind, &
   0.0000000e+000_r_kind,  -5.6254543e-002_r_kind,  -1.7999596e+001_r_kind,  -3.9189792e-001_r_kind, &
  -3.4482403e+000_r_kind,  -8.1249981e+000_r_kind,  -6.7380219e+000_r_kind,  -2.7040765e+000_r_kind, &
   0.0000000e+000_r_kind,  -6.6692841e-001_r_kind,  -1.6735210e+001_r_kind,  -6.7410064e-001_r_kind, &
  -9.1972437e+000_r_kind,  -1.8858464e+000_r_kind,   4.3322029e+000_r_kind,   1.5235697e+000_r_kind, &
   0.0000000e+000_r_kind,  -5.5174303e-001_r_kind,  -1.5763796e+001_r_kind,   2.3525839e+000_r_kind, &
  -1.2896632e+001_r_kind,  -1.8897469e+001_r_kind,  -3.4085879e+000_r_kind,   1.7145292e+000_r_kind, &
   0.0000000e+000_r_kind,  -5.5089825e-001_r_kind,  -1.6215494e+001_r_kind,  -2.9813105e-001_r_kind, &
  -1.2104578e+001_r_kind,  -1.0353115e+000_r_kind,   4.5084162e+000_r_kind,   1.4481387e+000_r_kind, &
   0.0000000e+000_r_kind,  -6.9536425e-002_r_kind,  -1.5814633e+001_r_kind,   2.4723272e-001_r_kind, &
  -1.0932647e+001_r_kind,  -1.3912515e+001_r_kind,  -1.9026325e+000_r_kind,   1.4365892e+000_r_kind, &
   0.0000000e+000_r_kind,  -8.1589860e-001_r_kind,  -1.7951654e+001_r_kind,  -1.1003120e+000_r_kind, &
  -1.3073632e+001_r_kind,   5.1298106e-001_r_kind,   4.0509553e+000_r_kind,   1.5236962e+000_r_kind, &
   0.0000000e+000_r_kind,  -9.1628246e-002_r_kind,  -1.5311787e+001_r_kind,   1.8295782e+000_r_kind, &
  -1.2783432e+001_r_kind,  -1.5435957e+001_r_kind,  -1.2408618e+000_r_kind,   1.8449930e+000_r_kind, &
   0.0000000e+000_r_kind,  -3.2023370e-001_r_kind,  -1.4794636e+001_r_kind,  -1.0388253e+000_r_kind, &
  -1.3135794e+001_r_kind,   8.2332176e-001_r_kind,   5.8204508e+000_r_kind,   5.6739199e-001_r_kind, &
   0.0000000e+000_r_kind,  -1.7746508e+000_r_kind,  -2.4223608e+001_r_kind,   1.3940347e+000_r_kind, &
  -3.3743076e+000_r_kind,   6.6819482e+000_r_kind,  -5.9035969e+000_r_kind,   4.8589239e+000_r_kind, &
   0.0000000e+000_r_kind,  -2.4095900e+000_r_kind,  -2.4074108e+001_r_kind,   6.6819078e-001_r_kind, &
  -6.4097409e+000_r_kind,  -1.3507084e+000_r_kind,   2.7213280e+000_r_kind,   2.5155761e+000_r_kind, &
   0.0000000e+000_r_kind,   1.9547707e+000_r_kind,  -2.5778395e+001_r_kind,   5.0604951e-001_r_kind, &
  -1.7787642e+000_r_kind,   1.8542128e+001_r_kind,  -2.3181459e+001_r_kind,   9.7702017e+000_r_kind, &
   0.0000000e+000_r_kind,  -1.3597628e+000_r_kind,  -2.4529604e+001_r_kind,  -6.3877082e-001_r_kind, &
  -8.6301928e+000_r_kind,   2.6526305e-001_r_kind,   5.1637397e+000_r_kind,   1.4656678e+000_r_kind, &
   0.0000000e+000_r_kind,   6.0032139e+000_r_kind,  -7.6777519e+001_r_kind,   3.9894840e+001_r_kind, &
  -1.2294738e+002_r_kind,  -3.1476166e+002_r_kind,   4.5004855e+002_r_kind,   4.1952277e+002_r_kind, &
   0.0000000e+000_r_kind,   7.1063489e-001_r_kind,  -9.2623924e+001_r_kind,  -8.3987915e+001_r_kind, &
   2.4781717e+001_r_kind,   3.8257230e+002_r_kind,   1.5585898e+002_r_kind,  -4.6345865e+002_r_kind, &
   0.0000000e+000_r_kind,  -7.7745061e+000_r_kind,  -5.1609882e+001_r_kind,   1.7463708e+002_r_kind, &
  -1.7688531e+002_r_kind,  -1.1080892e+003_r_kind,   2.9901968e+002_r_kind,   2.1341060e+003_r_kind, &
   0.0000000e+000_r_kind,   4.0031023e+000_r_kind,  -6.5797638e+001_r_kind,   1.6090117e+001_r_kind, &
   1.9881977e+001_r_kind,  -9.2731514e+000_r_kind,  -4.1151913e+002_r_kind,  -3.8807162e+002_r_kind, &
   0.0000000e+000_r_kind,  -7.7745061e+000_r_kind,  -5.1609882e+001_r_kind,   1.7463708e+002_r_kind, &
  -1.7688531e+002_r_kind,  -1.1080892e+003_r_kind,   2.9901968e+002_r_kind,   2.1341060e+003_r_kind, &
   0.0000000e+000_r_kind,   4.0031023e+000_r_kind,  -6.5797638e+001_r_kind,   1.6090117e+001_r_kind, &
   1.9881977e+001_r_kind,  -9.2731514e+000_r_kind,  -4.1151913e+002_r_kind,  -3.8807162e+002_r_kind, &
   0.0000000e+000_r_kind,  -7.7745061e+000_r_kind,  -5.1609882e+001_r_kind,   1.7463708e+002_r_kind, &
  -1.7688531e+002_r_kind,  -1.1080892e+003_r_kind,   2.9901968e+002_r_kind,   2.1341060e+003_r_kind, &
   0.0000000e+000_r_kind,   4.0031023e+000_r_kind,  -6.5797638e+001_r_kind,   1.6090117e+001_r_kind, &
   1.9881977e+001_r_kind,  -9.2731514e+000_r_kind,  -4.1151913e+002_r_kind,  -3.8807162e+002_r_kind, &
   0.0000000e+000_r_kind,  -7.8594452e-003_r_kind,  -3.4471641e+000_r_kind,   8.0819257e-002_r_kind, &
   9.0075269e-002_r_kind,   3.2504857e-002_r_kind,  -5.9909221e-002_r_kind,   1.4444345e-002_r_kind, &
   0.0000000e+000_r_kind,   5.4501977e-002_r_kind,  -3.1469109e+000_r_kind,  -1.7849508e-001_r_kind, &
  -1.6899924e-001_r_kind,   7.5798497e-002_r_kind,   4.3266434e-003_r_kind,  -1.1419394e-002_r_kind, &
   0.0000000e+000_r_kind,   5.8959834e-002_r_kind,  -3.3812485e+000_r_kind,   3.6676552e-002_r_kind, &
   6.1499011e-002_r_kind,   4.5787528e-002_r_kind,  -5.7126217e-002_r_kind,   1.3349920e-002_r_kind, &
   0.0000000e+000_r_kind,  -5.3862277e-002_r_kind,  -3.1149063e+000_r_kind,  -1.7908591e-001_r_kind, &
  -1.6015162e-001_r_kind,   7.6263897e-002_r_kind,   4.0943678e-003_r_kind,  -1.0471013e-002_r_kind, &
   0.0000000e+000_r_kind,   3.5132575e-001_r_kind,  -3.6589427e+000_r_kind,   1.1757551e-001_r_kind, &
   3.4198239e-002_r_kind,   6.2035419e-002_r_kind,  -4.8968330e-002_r_kind,   9.7700218e-003_r_kind, &
   0.0000000e+000_r_kind,   3.5774279e-001_r_kind,  -3.1648154e+000_r_kind,   1.4719218e-001_r_kind, &
  -1.9367284e-001_r_kind,   1.3059931e-002_r_kind,   8.9753121e-003_r_kind,  -3.1883763e-003_r_kind, &
   0.0000000e+000_r_kind,   9.2929967e-002_r_kind,  -7.8499112e+000_r_kind,  -5.8482140e-001_r_kind, &
  -1.3937727e-001_r_kind,  -3.6829272e-001_r_kind,  -2.4559858e-001_r_kind,  -5.7163361e-002_r_kind, &
   0.0000000e+000_r_kind,  -4.7696391e-001_r_kind,  -7.7833419e+000_r_kind,  -3.9853480e-001_r_kind, &
  -9.3758696e-001_r_kind,  -1.7017341e-003_r_kind,   1.4658263e-001_r_kind,  -1.3759389e-002_r_kind, &
   0.0000000e+000_r_kind,   6.3316137e-001_r_kind,  -8.0104923e+000_r_kind,  -6.3296270e-001_r_kind, &
   1.5075909e-001_r_kind,  -2.6595381e-001_r_kind,  -2.5882784e-001_r_kind,  -5.6811526e-002_r_kind, &
   0.0000000e+000_r_kind,   1.5558870e-002_r_kind,  -8.8178129e+000_r_kind,   7.0906274e-002_r_kind, &
  -2.7876469e-001_r_kind,  -2.5698286e-001_r_kind,   1.9218726e-002_r_kind,   5.4331992e-002_r_kind, &
   0.0000000e+000_r_kind,   4.1348686e+000_r_kind,  -7.8823051e+001_r_kind,   8.5124825e+001_r_kind, &
  -5.4667458e+001_r_kind,  -3.2198987e+002_r_kind,   2.4643106e+002_r_kind,   1.8443771e+002_r_kind, &
   0.0000000e+000_r_kind,   7.3553162e+000_r_kind,  -7.7426323e+001_r_kind,   3.4344803e+001_r_kind, &
  -9.0672569e+000_r_kind,  -1.0366821e+002_r_kind,  -2.2883960e+002_r_kind,   2.8478043e+002_r_kind, &
   0.0000000e+000_r_kind,  -4.1509781e+000_r_kind,  -7.0806435e+001_r_kind,   2.1327454e+001_r_kind, &
  -6.8019737e+001_r_kind,   1.4720393e+001_r_kind,   1.9501047e+002_r_kind,  -2.3744902e+002_r_kind, &
   0.0000000e+000_r_kind,   3.8904622e+000_r_kind,  -7.4227524e+001_r_kind,   1.6073383e+001_r_kind, &
   1.7793552e+001_r_kind,  -1.7465906e+001_r_kind,  -4.6764459e+002_r_kind,   5.8414502e+002_r_kind, &
   0.0000000e+000_r_kind,   1.0158999e+000_r_kind,  -2.3931587e+001_r_kind,  -3.5625136e-001_r_kind, &
  -2.0001745e+000_r_kind,   1.0046896e+001_r_kind,  -9.3339024e+000_r_kind,   4.1566882e+000_r_kind, &
   0.0000000e+000_r_kind,   4.3787226e-001_r_kind,  -2.3817133e+001_r_kind,   7.7674234e-001_r_kind, &
  -1.3957298e+001_r_kind,  -4.7761259e+000_r_kind,   1.2175179e+001_r_kind,   3.8461826e+000_r_kind, &
   0.0000000e+000_r_kind,  -2.4366511e-002_r_kind,  -2.5216434e+001_r_kind,   4.5976925e-001_r_kind, &
  -7.7533526e+000_r_kind,   1.6154577e+001_r_kind,  -7.4004397e+000_r_kind,   1.9803473e+000_r_kind, &
   0.0000000e+000_r_kind,  -2.2456892e+000_r_kind,  -3.1012749e+001_r_kind,  -4.8632174e+000_r_kind, &
  -6.1604791e+000_r_kind,   9.9734573e+000_r_kind,   1.3498210e+000_r_kind,  -3.8913133e+000_r_kind, &
   0.0000000e+000_r_kind,  -2.4366511e-002_r_kind,  -2.5216434e+001_r_kind,   4.5976925e-001_r_kind, &
  -7.7533526e+000_r_kind,   1.6154577e+001_r_kind,  -7.4004397e+000_r_kind,   1.9803473e+000_r_kind, &
   0.0000000e+000_r_kind,  -2.2456892e+000_r_kind,  -3.1012749e+001_r_kind,  -4.8632174e+000_r_kind, &
  -6.1604791e+000_r_kind,   9.9734573e+000_r_kind,   1.3498210e+000_r_kind,  -3.8913133e+000_r_kind, &
   0.0000000e+000_r_kind,  -2.4366511e-002_r_kind,  -2.5216434e+001_r_kind,   4.5976925e-001_r_kind, &
  -7.7533526e+000_r_kind,   1.6154577e+001_r_kind,  -7.4004397e+000_r_kind,   1.9803473e+000_r_kind, &
   0.0000000e+000_r_kind,  -2.2456892e+000_r_kind,  -3.1012749e+001_r_kind,  -4.8632174e+000_r_kind, &
  -6.1604791e+000_r_kind,   9.9734573e+000_r_kind,   1.3498210e+000_r_kind,  -3.8913133e+000_r_kind, &
   0.0000000e+000_r_kind,  -2.4366511e-002_r_kind,  -2.5216434e+001_r_kind,   4.5976925e-001_r_kind, &
  -7.7533526e+000_r_kind,   1.6154577e+001_r_kind,  -7.4004397e+000_r_kind,   1.9803473e+000_r_kind, &
   0.0000000e+000_r_kind,  -2.2456892e+000_r_kind,  -3.1012749e+001_r_kind,  -4.8632174e+000_r_kind, &
  -6.1604791e+000_r_kind,   9.9734573e+000_r_kind,   1.3498210e+000_r_kind,  -3.8913133e+000_r_kind, &
   0.0000000e+000_r_kind,  -2.4366511e-002_r_kind,  -2.5216434e+001_r_kind,   4.5976925e-001_r_kind, &
  -7.7533526e+000_r_kind,   1.6154577e+001_r_kind,  -7.4004397e+000_r_kind,   1.9803473e+000_r_kind, &
   0.0000000e+000_r_kind,  -2.2456892e+000_r_kind,  -3.1012749e+001_r_kind,  -4.8632174e+000_r_kind, &
  -6.1604791e+000_r_kind,   9.9734573e+000_r_kind,   1.3498210e+000_r_kind,  -3.8913133e+000_r_kind/), & 
      (/ 8,2,nchan/) )

 real(r_kind), private, target, dimension(0:7,2,nchan) :: ssmiscoeff_30 = reshape( (/  &
   0.0000000e+000_r_kind,  -8.1242698e-001_r_kind,  -1.7322411e+001_r_kind,  -1.4913555e+000_r_kind, & 
  -7.5479631e+000_r_kind,  -5.2311492e+000_r_kind,   7.5847095e-001_r_kind,   2.1852940e-001_r_kind, &
   0.0000000e+000_r_kind,  -5.0213617e-001_r_kind,  -1.7320137e+001_r_kind,  -3.5626882e-001_r_kind, &
  -1.0421036e+001_r_kind,  -1.0414523e+000_r_kind,   3.8325624e+000_r_kind,   5.6047857e-001_r_kind, &
   0.0000000e+000_r_kind,   9.1732121e-001_r_kind,  -1.8073399e+001_r_kind,  -4.0555561e-001_r_kind, &
  -1.4234181e+001_r_kind,  -1.1354250e+001_r_kind,   9.4643373e+000_r_kind,   6.7748699e+000_r_kind, &
   0.0000000e+000_r_kind,  -2.2143494e-001_r_kind,  -1.7848644e+001_r_kind,   1.5019023e+000_r_kind, &
  -7.9237504e+000_r_kind,  -2.1583858e+000_r_kind,   2.9565239e+000_r_kind,   9.3898833e-001_r_kind, &
   0.0000000e+000_r_kind,   6.2198943e-001_r_kind,  -1.8866920e+001_r_kind,   1.0232400e-002_r_kind, &
  -1.0987863e+001_r_kind,  -1.1172478e+001_r_kind,   5.6161885e+000_r_kind,   4.8539929e+000_r_kind, &
   0.0000000e+000_r_kind,   1.7685266e-001_r_kind,  -1.9272566e+001_r_kind,   3.3714971e-001_r_kind, &
  -7.0522461e+000_r_kind,  -1.6394613e+000_r_kind,   2.8404305e+000_r_kind,   8.3983219e-001_r_kind, &
   0.0000000e+000_r_kind,  -9.9236149e-001_r_kind,  -2.1220371e+001_r_kind,  -1.7408177e+000_r_kind, &
  -4.4365578e+000_r_kind,  -8.7910299e+000_r_kind,  -2.6262834e+000_r_kind,   2.0165244e-001_r_kind, &
   0.0000000e+000_r_kind,  -6.0894823e-001_r_kind,  -1.9529301e+001_r_kind,  -1.8224642e+000_r_kind, &
  -8.2119188e+000_r_kind,   1.6370267e+000_r_kind,   3.5837822e+000_r_kind,  -1.0699166e+000_r_kind, &
   0.0000000e+000_r_kind,  -5.9376007e-001_r_kind,  -1.7813452e+001_r_kind,  -9.7909898e-001_r_kind, &
  -1.1710135e+001_r_kind,  -1.0005310e+001_r_kind,   1.6678929e+000_r_kind,   1.6424536e+000_r_kind, &
   0.0000000e+000_r_kind,   6.6379562e-002_r_kind,  -1.7637888e+001_r_kind,  -8.4791547e-001_r_kind, &
  -7.1286364e+000_r_kind,   2.0018690e+000_r_kind,   2.9259231e+000_r_kind,  -1.2473146e+000_r_kind, &
   0.0000000e+000_r_kind,  -5.7577997e-001_r_kind,  -2.3462358e+001_r_kind,   3.0251467e+000_r_kind, &
  -5.8962140e+000_r_kind,   4.0891199e+000_r_kind,   6.5121198e+000_r_kind,  -4.4468627e+000_r_kind, &
   0.0000000e+000_r_kind,   9.9014723e-001_r_kind,  -2.3427225e+001_r_kind,   4.3571511e-001_r_kind, &
  -1.4011609e+001_r_kind,  -1.3213108e+000_r_kind,   7.6995211e+000_r_kind,  -2.1625619e+000_r_kind, &
   0.0000000e+000_r_kind,   1.1848265e+000_r_kind,  -2.2836988e+001_r_kind,   1.8503007e+000_r_kind, &
  -1.2766649e+001_r_kind,   8.5218239e+000_r_kind,   4.5473623e+000_r_kind,  -1.9612073e+000_r_kind, &
   0.0000000e+000_r_kind,   9.6428937e-001_r_kind,  -2.4980568e+001_r_kind,  -3.4923217e-001_r_kind, &
  -9.8359289e+000_r_kind,  -3.5861421e+000_r_kind,   6.0737405e+000_r_kind,   2.0032719e-001_r_kind, &
   0.0000000e+000_r_kind,  -8.4320527e-001_r_kind,  -6.7140419e+001_r_kind,  -4.8543148e+001_r_kind, &
  -2.5773932e+002_r_kind,   1.5596658e+002_r_kind,   7.0775281e+002_r_kind,  -3.6785297e+001_r_kind, &
   0.0000000e+000_r_kind,  -2.6547649e+000_r_kind,  -5.2738731e+001_r_kind,  -5.3957340e+001_r_kind, &
  -3.0973065e+002_r_kind,  -1.9356750e+001_r_kind,   6.6924072e+002_r_kind,   4.0972095e+002_r_kind, &
   0.0000000e+000_r_kind,  -3.7919652e+000_r_kind,  -7.4432121e+001_r_kind,   1.0177740e+002_r_kind, &
  -1.2210255e+001_r_kind,  -4.1836511e+002_r_kind,  -2.7696204e+002_r_kind,  -1.5293272e+002_r_kind, &
   0.0000000e+000_r_kind,  -8.4154472e+000_r_kind,  -1.1623577e+002_r_kind,   3.6768167e+000_r_kind, &
   1.8412891e+002_r_kind,   4.2939056e+001_r_kind,  -9.5675323e+001_r_kind,  -4.1348026e+001_r_kind, &
   0.0000000e+000_r_kind,  -3.7919652e+000_r_kind,  -7.4432121e+001_r_kind,   1.0177740e+002_r_kind, &
  -1.2210255e+001_r_kind,  -4.1836511e+002_r_kind,  -2.7696204e+002_r_kind,  -1.5293272e+002_r_kind, &
   0.0000000e+000_r_kind,  -8.4154472e+000_r_kind,  -1.1623577e+002_r_kind,   3.6768167e+000_r_kind, &
   1.8412891e+002_r_kind,   4.2939056e+001_r_kind,  -9.5675323e+001_r_kind,  -4.1348026e+001_r_kind, &
   0.0000000e+000_r_kind,  -3.7919652e+000_r_kind,  -7.4432121e+001_r_kind,   1.0177740e+002_r_kind, &
  -1.2210255e+001_r_kind,  -4.1836511e+002_r_kind,  -2.7696204e+002_r_kind,  -1.5293272e+002_r_kind, &
   0.0000000e+000_r_kind,  -8.4154472e+000_r_kind,  -1.1623577e+002_r_kind,   3.6768167e+000_r_kind, &
   1.8412891e+002_r_kind,   4.2939056e+001_r_kind,  -9.5675323e+001_r_kind,  -4.1348026e+001_r_kind, &
   0.0000000e+000_r_kind,   2.3349223e-001_r_kind,  -3.3928640e+000_r_kind,   7.3811524e-002_r_kind, &
   5.7615761e-002_r_kind,   5.0451934e-002_r_kind,  -5.2907690e-002_r_kind,   1.1161046e-002_r_kind, &
   0.0000000e+000_r_kind,   1.6616182e-001_r_kind,  -3.4010665e+000_r_kind,  -9.6719041e-002_r_kind, &
  -1.3302240e-001_r_kind,   5.2781180e-002_r_kind,  -3.0623950e-004_r_kind,  -9.9937636e-003_r_kind, &
   0.0000000e+000_r_kind,   7.8117646e-002_r_kind,  -3.3993268e+000_r_kind,   3.0030102e-002_r_kind, &
   4.5503613e-002_r_kind,   4.8739538e-002_r_kind,  -5.1880572e-002_r_kind,   1.1812583e-002_r_kind, &
   0.0000000e+000_r_kind,  -6.2565304e-002_r_kind,  -3.4176610e+000_r_kind,  -2.0383033e-001_r_kind, &
  -6.0506601e-002_r_kind,   6.6073805e-002_r_kind,  -1.5484643e-002_r_kind,  -8.7743290e-003_r_kind, &
   0.0000000e+000_r_kind,   1.7220975e-001_r_kind,  -3.5378897e+000_r_kind,   1.0902610e-001_r_kind, &
  -1.1534608e-001_r_kind,   1.0459975e-001_r_kind,  -3.7106849e-002_r_kind,   5.4422715e-003_r_kind, &
   0.0000000e+000_r_kind,  -4.8829676e-003_r_kind,  -3.1576529e+000_r_kind,  -1.6720854e-001_r_kind, &
  -2.9642692e-001_r_kind,   6.1122209e-002_r_kind,   2.0650864e-002_r_kind,  -6.1765374e-003_r_kind, &
   0.0000000e+000_r_kind,   1.3717458e+000_r_kind,  -1.1060697e+001_r_kind,  -4.6913085e+000_r_kind, &
   1.3176364e+000_r_kind,   2.7809887e+000_r_kind,  -9.3083180e-002_r_kind,  -4.3128756e-001_r_kind, &
   0.0000000e+000_r_kind,   1.2089583e-001_r_kind,  -7.6801124e+000_r_kind,  -1.7641781e-001_r_kind, &
  -1.1694665e+000_r_kind,   2.3553216e-001_r_kind,   2.8315729e-001_r_kind,  -7.1349286e-002_r_kind, &
   0.0000000e+000_r_kind,  -3.7924060e-001_r_kind,  -7.9739203e+000_r_kind,  -3.1737602e-001_r_kind, &
   1.5923525e-001_r_kind,  -6.7117053e-001_r_kind,  -4.8460048e-001_r_kind,  -8.6309299e-002_r_kind, &
   0.0000000e+000_r_kind,   8.6717665e-001_r_kind,  -9.1479797e+000_r_kind,   5.7676911e-001_r_kind, &
  -8.2519203e-001_r_kind,  -1.3055032e-001_r_kind,   1.6377680e-001_r_kind,   2.4141574e-002_r_kind, &
   0.0000000e+000_r_kind,   3.1188061e+000_r_kind,  -7.9309235e+001_r_kind,   1.2149082e+001_r_kind, &
   4.8229881e+001_r_kind,  -2.3845295e+001_r_kind,  -6.2838306e+002_r_kind,   8.5685492e+002_r_kind, &
   0.0000000e+000_r_kind,  -5.1661599e-001_r_kind,  -7.0389023e+001_r_kind,  -2.8435934e+001_r_kind, &
  -9.4271751e+001_r_kind,   8.4401657e+001_r_kind,  -1.4321292e+002_r_kind,  -3.0814734e+002_r_kind, &
   0.0000000e+000_r_kind,   3.1969938e+000_r_kind,  -6.3910507e+001_r_kind,   1.6119511e+001_r_kind, &
  -2.1956174e+002_r_kind,   1.7886533e+002_r_kind,   6.1882263e+002_r_kind,  -7.4026147e+002_r_kind, &
   0.0000000e+000_r_kind,  -1.4528100e+000_r_kind,  -6.8755867e+001_r_kind,  -2.1990011e+001_r_kind, &
  -2.2600664e+001_r_kind,   1.4443349e+002_r_kind,  -3.4695981e+002_r_kind,  -8.6315863e+002_r_kind, &
   0.0000000e+000_r_kind,  -3.1687707e-001_r_kind,  -1.9885412e+001_r_kind,   6.7765403e+000_r_kind, &
  -1.6002058e+001_r_kind,   7.0442166e+000_r_kind,   6.8716435e+000_r_kind,  -4.3698077e+000_r_kind, &
   0.0000000e+000_r_kind,   6.5448666e-001_r_kind,  -2.7958408e+001_r_kind,  -3.5078766e+000_r_kind, &
  -6.6805452e-001_r_kind,   5.0201545e+000_r_kind,  -5.3602285e+000_r_kind,  -8.3190079e+000_r_kind, &
   0.0000000e+000_r_kind,  -4.4566658e-001_r_kind,  -2.1961597e+001_r_kind,   3.6832643e+000_r_kind, &
  -1.3226493e+001_r_kind,   7.3017807e+000_r_kind,   1.1002332e+001_r_kind,  -7.0395536e+000_r_kind, &
   0.0000000e+000_r_kind,  -2.0096771e-002_r_kind,  -2.2113859e+001_r_kind,  -4.5402646e-001_r_kind, &
  -1.2636704e+001_r_kind,  -8.3116474e+000_r_kind,   4.1869698e+000_r_kind,   3.5938828e+000_r_kind, &
   0.0000000e+000_r_kind,  -4.4566658e-001_r_kind,  -2.1961597e+001_r_kind,   3.6832643e+000_r_kind, &
  -1.3226493e+001_r_kind,   7.3017807e+000_r_kind,   1.1002332e+001_r_kind,  -7.0395536e+000_r_kind, &
   0.0000000e+000_r_kind,  -2.0096771e-002_r_kind,  -2.2113859e+001_r_kind,  -4.5402646e-001_r_kind, &
  -1.2636704e+001_r_kind,  -8.3116474e+000_r_kind,   4.1869698e+000_r_kind,   3.5938828e+000_r_kind, &
   0.0000000e+000_r_kind,  -4.4566658e-001_r_kind,  -2.1961597e+001_r_kind,   3.6832643e+000_r_kind, &
  -1.3226493e+001_r_kind,   7.3017807e+000_r_kind,   1.1002332e+001_r_kind,  -7.0395536e+000_r_kind, &
   0.0000000e+000_r_kind,  -2.0096771e-002_r_kind,  -2.2113859e+001_r_kind,  -4.5402646e-001_r_kind, &
  -1.2636704e+001_r_kind,  -8.3116474e+000_r_kind,   4.1869698e+000_r_kind,   3.5938828e+000_r_kind, &
   0.0000000e+000_r_kind,  -4.4566658e-001_r_kind,  -2.1961597e+001_r_kind,   3.6832643e+000_r_kind, &
  -1.3226493e+001_r_kind,   7.3017807e+000_r_kind,   1.1002332e+001_r_kind,  -7.0395536e+000_r_kind, &
   0.0000000e+000_r_kind,  -2.0096771e-002_r_kind,  -2.2113859e+001_r_kind,  -4.5402646e-001_r_kind, &
  -1.2636704e+001_r_kind,  -8.3116474e+000_r_kind,   4.1869698e+000_r_kind,   3.5938828e+000_r_kind, &
   0.0000000e+000_r_kind,  -4.4566658e-001_r_kind,  -2.1961597e+001_r_kind,   3.6832643e+000_r_kind, &
  -1.3226493e+001_r_kind,   7.3017807e+000_r_kind,   1.1002332e+001_r_kind,  -7.0395536e+000_r_kind, &
   0.0000000e+000_r_kind,  -2.0096771e-002_r_kind,  -2.2113859e+001_r_kind,  -4.5402646e-001_r_kind, &
  -1.2636704e+001_r_kind,  -8.3116474e+000_r_kind,   4.1869698e+000_r_kind,   3.5938828e+000_r_kind/), &
      (/ 8,2,nchan/) )

 contains
subroutine instrument_init(instr,satid,expansion,valid)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    instrument_init          initalize instrument fields
!
!   prgmmr: kleespies           org: nesdis              date: 2008-08-09
!
! abstract: initialize variables required by this module.
!   
! program history log:
!   2008-08-09  kleespies
!   2008-11-06  gayno - modified for gsi software standards
!   2009-08-21  kleespies - additional instruments 
!   2011-09-13  gayno - improve error handling.  pass back
!                       error flag (variable valid) when
!                       inputs are incorrect.
!
! input argument list:
!   instr        - instrument number
!                  25 - Default circular fov
!                  26 - F16 SSMIS - FM2
!                  27 - F17 SSMIS - FM1
!                  28 - FM3 SSMIS
!                  29 - FM4 SSMIS
!                  30 - FM5 SSMIS
!   satid        - satellite id
!   expansion    - expansion factor.  must be 1.0 for accurate rendering, 
!                  > 1.0 makes bigger ellipses, < 1.0 makes smaller ellipses.
!                  do not make bigger than 3.0.
!  
! output argument list:
!   valid        - set to false when inputs are incorrect.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

 use calc_fov_crosstrk, only : get_sat_height
 use constants, only  : pi, one, two, half, zero

 implicit none

! Declare passed variables.
 character(len=*), intent(in   ) :: satid
 integer(i_kind) , intent(in   ) :: instr
 real(r_kind)    , intent(in   ) :: expansion

 logical :: valid
! Declare local variables.
 integer(i_kind)                    :: i, jchan, minstr
 real(r_kind)                       :: ata, cta, atf, ctf, ratio, height

 valid=.true.

 minstr = instr
 if(instr == 25) minstr = 26 ! use dmsp 16 for default circular fov size
  
 if ((minstr < instrumentrange(1)) .or. (minstr > instrumentrange(2))) then 
    write(6,*) "INSTRUMENT_INIT: INSTRUMENT NUMBER OF: ",minstr," OUT OF RANGE." 
    write(6,*) "VALID VALUES ARE: ", instrumentrange(1)," TO ",instrumentrange(2)
    valid=.false.
    return
 endif

 nullify(ssmiscoeff)
 select case (minstr)
    case(26)
       ssmiscoeff=>ssmiscoeff_26
    case(27)
       ssmiscoeff=>ssmiscoeff_27
    case(28)
       ssmiscoeff=>ssmiscoeff_28
    case(29)
       ssmiscoeff=>ssmiscoeff_29
    case(30)
       ssmiscoeff=>ssmiscoeff_30
    case default
       write(6,*) "INSTRUMENT_INIT: NO SSMIS COEFFICIENTS FOR SATELLITE ",minstr
       valid=.false.
       return
 end select

! fov is polygon.
 do i = 1 , npoly
    psi(i) = two*pi*(i-1)/(npoly-1)  ! will connect npoly points
 enddo

 call get_sat_height(satid, height, valid)
 if(.not.valid) return

 do jchan = 1 , nchan
! For accurate representation of fov, this computation should go with
! the height from the 1B
    call fovconicalanglessizes(minstr,jchan,height,ata,cta,atf,ctf)
    alongtrackangle(jchan)   = ata
    crosstrackangle(jchan)   = cta
    alongtrackfovsize(jchan) = atf
    crosstrackfovsize(jchan) = ctf
    rmax(jchan) = half*crosstrackangle(jchan)* expansion ! remember, these are semiaxes
    rmin(jchan) = half*alongtrackangle(jchan)* expansion
    ratio = rmin(jchan)**2/rmax(jchan)**2
    if(ratio > one) ratio = one  !  this takes care of some precision issues
    eccen(jchan) = sqrt(one - ratio)
 enddo

 if(instr == 25) eccen = zero  ! default circular fov

 return

end subroutine instrument_init
subroutine fovconicalanglessizes(instr,chan,height,alongtrackangle, &
                                 crosstrackangle,alongtrackfovsize, &
                                 crosstrackfovsize)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fovconicalanglessizes    calc conical angle size
!
!   prgmmr: kleespies           org: nesdis              date: 2008-08-09
!
! abstract: computes the cross track and along track angles of a 
!   conical instrument FOV as viewed from the center of the earth;
!   and cross track and along track FOV size in km. presumes a
!   spherical earth.
!   
! program history log:
!   2008-08-09  kleespies
!   2008-11-06  gayno - modified for gsi software standards
!   2009-09-21  Kleespies - added instruments
!
! input argument list:
!   instr        - insturment number
!                  25 - Default circular fov
!                  26 - F16 SSMIS - FM2
!                  27 - F17 SSMIS - FM1
!                  28 - FM3 SSMIS
!                  29 - FM4 SSMIS
!                  30 - FM5 SSMIS
!   chan         - channel number (ghz)
!                       1  - 50.3 v
!                       2  - 52.8 v
!                       3  - 53.596 v
!                       4  - 54.4 v
!                       5  - 55.5 v
!                       6  - 57.29 rcp
!                       7  - 59.4 rcp
!                       8  - 150 h
!                       9  - 183.31+-6.6 h
!                       10 - 183.31+-3 h
!                       11 - 183.31+-1 h
!                       12 - 19.35 h
!                       13 - 19.35 v
!                       14 - 22.235 v
!                       15 - 37 h
!                       16 - 37 v
!                       17 - 91.655 v
!                       18 - 91.655 h
!                       19 - 63.283248+-0.285271 rcp
!                       20 - 60.792668+-0.357892 rcp
!                       21 - 60.792668+-0.357892+-0.002 rcp
!                       22 - 60.792668+-0.357892+-0.0055 rcp
!                       23 - 60.792668+-0.357892+-0.016  rcp
!                       24 - 60.792668+-0.357892+-0.05 rcp
!   height       - satellite height in km
!  
! output argument list:
!   alongtrackangle       - along track angle
!   alongtrackfovsize     - along track fov size
!   crosstrackangle       - cross track angle
!   crosstrackfovsize     - cross track fov size
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

 use constants, only : half, two, deg2rad, rad2deg, zero

 implicit none

! Declare passed variables.
 integer(i_kind), intent(in   ) :: instr
 integer(i_kind), intent(in   ) :: chan
 real(r_kind)   , intent(in   ) :: height
 real(r_kind)   , intent(  out) :: alongtrackangle,crosstrackangle
 real(r_kind)   , intent(  out) :: alongtrackfovsize,crosstrackfovsize

! Declare local parameters.
 real(r_kind), dimension(instrumentrange(1):instrumentrange(2)) &
                                  :: conicalangle = (/45._r_kind,45._r_kind,45._r_kind,45._r_kind,45._r_kind/)

! Declare local variables.
 real(r_kind)                     :: nadirangle
 real(r_kind)                     :: nadirangle_m
 real(r_kind)                     :: nadirangle_p
 real(r_kind)                     :: compzacenter
 real(r_kind)                     :: compza_m
 real(r_kind)                     :: compza_p
 real(r_kind)                     :: distancetofov

! Nadir angles of center and crosstrack extremes of fov
 nadirangle   = conicalangle(instr)
 nadirangle_m = nadirangle - fovangle(chan,instr)*half 
 nadirangle_p = nadirangle + fovangle(chan,instr)*half 

! Complement of zenith angle for center and crosstrack extremes of fov
 compzacenter = 180._r_kind-asin((radius+height)/radius * sin(nadirangle  /rad2deg))*rad2deg
 compza_m     = 180._r_kind-asin((radius+height)/radius * sin(nadirangle_m/rad2deg))*rad2deg
 compza_p     = 180._r_kind-asin((radius+height)/radius * sin(nadirangle_p/rad2deg))*rad2deg

! cross track angle of the fov as viewed from center of the earth
 crosstrackangle = abs(nadirangle_p + compza_p - nadirangle_m - compza_m)

! cross track fov size in km
 crosstrackfovsize = abs(crosstrackangle*deg2rad*radius)

! distance from satellite to the center of the fov in km
 distancetofov = (radius+height)* &
                  sin( (180._r_kind-nadirangle-compzacenter)/rad2deg)/sin((compzacenter)/rad2deg)
 if(distancetofov < zero) distancetofov = height ! for nadir fov
  
! along track fov size in km. the following is an approximation, but it is close. 
! it underestimates the FOV by a smidge.
 alongtrackfovsize = two*distancetofov*tan(fovangle(chan,instr)*half/rad2deg)

! along track angle of the fov as viewed from center of the earth
 alongtrackangle = rad2deg*alongtrackfovsize/(radius)

end subroutine fovconicalanglessizes
subroutine fov_ellipse_conical(ichan,satellite_azimuth,lat,lon,elats,elons)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fov_ellipse_conical     computes fov ellipses
!
!   prgmmr: kleespies           org: nesdis              date: 2008-08-09
!
! abstract: computes fov ellipses in latitude/longitude coordinates of
!   a conically scanning instrument. 
!   
! program history log:
!   2008-08-09  kleespies
!   2008-11-06  gayno - modified for gsi software standards
!
! input argument list:
!   ichan             - channel number (ghz)
!                       1  - 50.3 v
!                       2  - 52.8 v
!                       3  - 53.596 v
!                       4  - 54.4 v
!                       5  - 55.5 v
!                       6  - 57.29 rcp
!                       7  - 59.4 rcp
!                       8  - 150 h
!                       9  - 183.31+-6.6 h
!                       10 - 183.31+-3 h
!                       11 - 183.31+-1 h
!                       12 - 19.35 h
!                       13 - 19.35 v
!                       14 - 22.235 v
!                       15 - 37 h
!                       16 - 37 v
!                       17 - 91.655 v
!                       18 - 91.655 h
!                       19 - 63.283248+-0.285271 rcp
!                       20 - 60.792668+-0.357892 rcp
!                       21 - 60.792668+-0.357892+-0.002 rcp
!                       22 - 60.792668+-0.357892+-0.0055 rcp
!                       23 - 60.792668+-0.357892+-0.016  rcp
!                       24 - 60.792668+-0.357892+-0.05 rcp
!   satellite_azimuth - satellite azimuth angle (degrees)  
!   lat               - latitude of fov center 
!   lon               - longitude of fov center
!  
! output argument list:
!   elats             - ellipse latitudes  centered about lat,lon
!   elons             - ellipse longitudes centered about lat,lon
!
! remarks:
!
!  There are several engineering checks to handle things like 
!  arcsin( x>1.0 or x<-1.0).  These things can happen because POES navigation
!  uses an oblate spheroid earth, while here we are using a spherical earth,
!  so there is a small inconsistency in computing arc angles.
!
!  No provisions are made for spacecraft roll-pitch-yaw errors, which
!  are presumed to be small (SC attitude is usually held to within 0.1 deg)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

 use constants, only : one, rad2deg

 implicit none

! Declare passed variables
 integer(i_kind), intent(in   ) :: ichan
 real(r_kind)   , intent(in   ) :: lat
 real(r_kind)   , intent(in   ) :: lon
 real(r_kind)   , intent(in   ) :: satellite_azimuth
 real(r_kind)   , intent(  out) :: elats(npoly)
 real(r_kind)   , intent(  out) :: elons(npoly)

! Declare local variables
 integer(i_kind)                :: i ! loop counters
 real(r_kind)                   :: pos_ang ! rotation angle of the ellipse
 real(r_kind), dimension(npoly) :: psip
 real(r_kind), dimension(npoly) :: r
 real(r_kind), dimension(npoly) :: cosc
 real(r_kind), dimension(npoly) :: c
 real(r_kind), dimension(npoly) :: sinb
 real(r_kind), dimension(npoly) :: b

 pos_ang = satellite_azimuth 
 psip    = psi + pos_ang/rad2deg 
 r       = rmax(ichan) * sqrt( (one - eccen(ichan)**2)/(one - eccen(ichan)**2 *cos(psi)**2) )
 cosc    = cos((90._r_kind-lat)/rad2deg)*cos(r/rad2deg) + &
           sin((90._r_kind-lat)/rad2deg)*sin(r/rad2deg)*cos(psip)
 c       = acos(cosc)*rad2deg

 elats(1:npoly) = 90._r_kind - c
 sinb = sin(r/rad2deg)*sin(psip)/sin(c/rad2deg)

! handle numeric imprecision 
 do i = 1 , npoly
    if(sinb(i) >  one) sinb(i) =  one
    if(sinb(i) < -one) sinb(i) = -one
 enddo

 b = asin(sinb)*rad2deg
 elons(1:npoly) = lon + b
 
 return
end subroutine fov_ellipse_conical
subroutine inside_fov_conical(instr,ichan,satellite_azimuth,lat,lon, &
                              testlat,testlon,expansion,inside)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    inside_fov_conical     inside satellite fov
!
!   prgmmr: kleespies           org: nesdis              date: 2008-08-09
!
! abstract: determines whether a test point is inside a fov
!   for a conically scanning instrument
!   
! program history log:
!   2008-08-09  kleespies
!   2008-11-06  gayno - modified for gsi software standards
!   2011-09-13  gayno - move initialization of ssmiscoef to
!                       routine instrument_init
!
! input argument list:
!   ichan             - channel number (ghz)
!                       1  - 50.3 v
!                       2  - 52.8 v
!                       3  - 53.596 v
!                       4  - 54.4 v
!                       5  - 55.5 v
!                       6  - 57.29 rcp
!                       7  - 59.4 rcp
!                       8  - 150 h
!                       9  - 183.31+-6.6 h
!                       10 - 183.31+-3 h
!                       11 - 183.31+-1 h
!                       12 - 19.35 h
!                       13 - 19.35 v
!                       14 - 22.235 v
!                       15 - 37 h
!                       16 - 37 v
!                       17 - 91.655 v
!                       18 - 91.655 h
!                       19 - 63.283248+-0.285271 rcp
!                       20 - 60.792668+-0.357892 rcp
!                       21 - 60.792668+-0.357892+-0.002 rcp
!                       22 - 60.792668+-0.357892+-0.0055 rcp
!                       23 - 60.792668+-0.357892+-0.016  rcp
!                       24 - 60.792668+-0.357892+-0.05 rcp
!   instr             - instrument number
!                       25 - Default circular fov
!                       26 - F16 SSMIS - FM2
!                       27 - F17 SSMIS - FM1
!                       28 - FM3 SSMIS
!                       29 - FM4 SSMIS
!                       30 - FM5 SSMIS
!   expansion         - expansion factor.  must be 1.0 for accurate rendering, 
!                       > 1.0 makes bigger ellipses, < 1.0 makes smaller ellipses.
!   satellite_azimuth - satellite azimuth angle (degrees)  
!   lat               - latitude of fov center 
!   lon               - longitude of fov center
!   testlat           - latitude of test point 
!   testlon           - longitude of test point
!  
! output argument list:
!   inside            - 0.0-1.0 relative antenna power if [testLat,testLon] is 
!                       inside the FOV, 0 if outside.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

 use constants, only : one, half, two, zero, deg2rad, rad2deg, pi, one_tenth

 implicit none

! Declare passed variables
 integer(i_kind), intent(in   ) :: instr             
 integer(i_kind), intent(in   ) :: ichan             
 real(r_kind)   , intent(in   ) :: satellite_azimuth 
 real(r_kind)   , intent(in   ) :: lat               
 real(r_kind)   , intent(in   ) :: lon               
 real(r_kind)   , intent(in   ) :: testlat           
 real(r_kind)   , intent(in   ) :: testlon           
 real(r_kind)   , intent(in   ) :: expansion         
 real(r_kind)   , intent(  out) :: inside         

! Declare local parameters
 real(r_kind), parameter  :: r1 = one ! Equatorial radius. Work in angular distance, 
                                      ! not km (otherwise r1=6371)
 real(r_kind), parameter  :: r2 = r1  ! assume spherical earth (otherwise r2 = polar radius)

! Declare local variables
 real(r_kind)  :: satellite_azimuth_rot ! relative azimuth from NOAA level 1B file
 real(r_kind)  :: dellon ! longitude difference from fov center to test location
 real(r_kind)  :: dellat ! latitude  difference from fov center to test location

! These two are what gets compared to determine if test location is within fov
 real(r_kind)  :: d      ! angular distance from fov center to test location
 real(r_kind)  :: r      ! angular distance from fov center to ellipse along d

 real(r_kind)  :: psi    ! angle from 3:00 on fov to line from fov center to test location
 real(r_kind)  :: psip   ! angle from latitude parallel through fov center
                         ! to line from fov center to test location
 
! These are the flat earth variables
 real(r_kind)  :: distance_north  ! north angular distance from fov center to test location
 real(r_kind)  :: distance_east   ! east  angular distance from fov center to test location
 real(r_kind)  :: bearing_to_test ! same as psip
 real(r_kind)  :: bearing_to_test_deg ! in degrees
 real(r_kind)  :: fovanglesize 
 real(r_kind)  :: x,y,px,py,p,rat
 
 integer(i_kind) :: minstr        ! storage for instrument number permitting instr 25 default circular

  minstr = instr
  if(instr == 25) minstr = 26 ! use dmsp 16 for default circular fov size

! Get satellite az to where we want it. 1st, convert +- to 0-360
  satellite_azimuth_rot = satellite_azimuth
  if(satellite_azimuth_rot < zero) satellite_azimuth_rot = 360.0_r_kind + satellite_azimuth_rot
! 2nd, shift rotation direction
  satellite_azimuth_rot = mod((450.0_r_kind-satellite_azimuth_rot),360.0_r_kind)

  dellat = (testlat - lat)*deg2rad
  dellon = testlon - lon
  if (dellon > 180._r_kind) dellon = dellon - 360._r_kind
  dellon = dellon*deg2rad

! Distance north and east in degrees
  distance_north =  r1*dellat
  distance_east  =  r2*cos(lat*deg2rad)*dellon

! Angle to the test point
  bearing_to_test = mod(atan2(distance_north,distance_east),two*pi)
  bearing_to_test_deg = bearing_to_test*rad2deg ! convert to degrees

! This is the arc distance to the test point
  d=two*asin(sqrt((sin(dellat/two))**2 +     & 
                 cos(testlat*deg2rad)*cos(lat*deg2rad)*(sin(dellon/two))**2))
  d = d*rad2deg  ! convert to degrees

  psip = bearing_to_test_deg

  psi = (psip  - satellite_azimuth_rot)
  psi = psi*deg2rad ! convert to radians

! r is the angular distance from the ichan center to the edge of the ellipse in degrees
  r = rmax(ichan)*sqrt( (one - eccen(ichan)**2)/(one - eccen(ichan)**2 *cos(psi)**2) )
 
  inside = zero


  if (d<r) then

     fovanglesize = fovangle(ichan,minstr)
     rat = d / r * expansion * fovanglesize * half

     x = rat * cos(psi)
     y = rat * sin(psi)

     px = ssmiscoeff(0,1,ichan) + ssmiscoeff(1,1,ichan)*x    + ssmiscoeff(2,1,ichan)*x**2 & 
                                + ssmiscoeff(3,1,ichan)*x**3 + ssmiscoeff(4,1,ichan)*x**4 &
                                + ssmiscoeff(5,1,ichan)*x**5 + ssmiscoeff(6,1,ichan)*x**6 &
                                + ssmiscoeff(7,1,ichan)*x**7   

     py = ssmiscoeff(0,2,ichan) + ssmiscoeff(1,2,ichan)*y    + ssmiscoeff(2,2,ichan)*y**2 &
                                + ssmiscoeff(3,2,ichan)*y**3 + ssmiscoeff(4,2,ichan)*y**4 &
                                + ssmiscoeff(5,2,ichan)*y**5 + ssmiscoeff(6,2,ichan)*y**6 &
	                        + ssmiscoeff(7,2,ichan)*y**7  

     p = -(px+py) ! power in dB (positive)

   ! convert to fraction of max power

     p = 10._r_kind**(-p*one_tenth)

     inside = p  
     if(inside > one) inside = one
   
  endif

  return
 end subroutine inside_fov_conical
 end module calc_fov_conical
