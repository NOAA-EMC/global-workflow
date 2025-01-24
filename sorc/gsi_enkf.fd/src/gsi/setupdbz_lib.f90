! History log

! 2017-05-12 Johnson, Y. Wang and X. Wang - define reflectivity opeator and its adjoint for WSM6 scheme, POC: xuguang.wang@ou.edu

module setupdbz_lib
public ::  hx_dart,jqr_dart,jqs_dart,jqg_dart
contains
subroutine hx_dart(qrgesin0,qggesin0,qsgesin0,rhogesin,tempgesin,rDBZ,debugging)
  use kinds, only: r_kind,r_double,i_kind
  use obsmod, only: static_gsi_nopcp_dbz
implicit none
real(r_kind) :: qrgesin0,qsgesin0,qggesin0
real(r_kind) :: qrgesin,qsgesin,qggesin,rhogesin,tempgesin,rDBZ
real(r_kind) :: zqr,zqg,zqs
logical :: debugging
real(r_kind) :: param_r,param_dry_g,param_wet_g,param_dry_s,param_wet_s
real(r_kind) ::n0r,n0s,n0g,rhor,rhos,rhog,dielectric,pi

 qrgesin=qrgesin0
 qsgesin=qsgesin0
 qggesin=qggesin0


pi=3.14159_r_kind
dielectric=0.224_r_kind
n0r=8e6_r_kind
n0s=3e6_r_kind !(2e6) !*exp(0.12*(min(273.15,tempgesin)-273.15)) !this is n0s in WSM6 paper, dif. from DART constant of 3e6
n0g=4e6_r_kind
rhos=100_r_kind
rhor=1000_r_kind
rhog=500_r_kind !this is rhog in WSM6 paper, dif. from DART 400

param_r=(7.2e20_r_kind)/(((pi*rhor)**1.75_r_kind)*(n0r**0.75_r_kind))
param_dry_g=dielectric*(rhog/rhor)*(rhog/rhor)*(7.2e20_r_kind)/(((pi*rhog)**1.75_r_kind)*(n0g**0.75_r_kind))
param_wet_g=(7.2e20_r_kind)/((((pi*rhog)**1.75_r_kind)*(n0g**0.75_r_kind))**0.95_r_kind)
param_wet_s=(7.2e20_r_kind)/(((pi*rhos)**1.75_r_kind)*(n0s**0.75_r_kind))
param_dry_s=dielectric*(rhos/rhor)*(rhos/rhor)*param_wet_s


zqr=param_r*((rhogesin*qrgesin)**1.75_r_kind)
if (tempgesin < 273.15_r_kind) then
  zqr=0_r_kind
  zqg=param_dry_g*((rhogesin*qggesin)**1.75_r_kind)
  zqs=param_dry_s*((rhogesin*qsgesin)**1.75_r_kind)
else if(tempgesin < 278.15_r_kind) then
  zqg=param_wet_g*((rhogesin*qggesin)**1.6675_r_kind)
  zqs=param_wet_s*((rhogesin*qsgesin)**1.75_r_kind)
else
  zqg=0_r_kind
  zqs=0_r_kind
endif
rDBZ=zqr+zqg+zqs
if (rdBZ > 1.0e-3_r_kind) then
  rdBZ=10_r_kind*log10(rdBZ)
else
  rdBZ=-30_r_kind
endif
if(rdBZ<static_gsi_nopcp_dbz) rdBZ=static_gsi_nopcp_dbz !notice, static_gsi_nopcp_dbz should be larger than -30

if(debugging) print *, "ZQR=",zqr,zqs,zqg,tempgesin

end subroutine hx_dart



subroutine jqr_dart(qrgesin0,qsgesin0,qggesin0,rhogesin,tempgesin,jqr)
  use kinds, only: r_kind,r_double,i_kind
implicit none
real(r_kind) :: qrgesin0,qsgesin0,qggesin0
real(r_kind) :: qrgesin,rhogesin,tempgesin,jqr
real(r_kind) :: Ze,zqr,zqg,zqs,qsgesin,qggesin

real(r_kind) :: param_r,param_dry_g,param_wet_g,param_dry_s,param_wet_s
real(r_kind) ::n0r,n0s,n0g,rhor,rhos,rhog,dielectric,pi,thisqrgesin
 qrgesin=qrgesin0
 qsgesin=qsgesin0
 qggesin=qggesin0

pi=3.14159_r_kind
dielectric=0.224_r_kind
n0r=8e6_r_kind
n0s=3e6_r_kind !(2e6) !*exp(0.12*(min(273.15,tempgesin)-273.15)) !this is n0s in WSM6 paper, dif. from DART constant of 3e6
n0g=4e6_r_kind
rhos=100_r_kind
rhor=1000_r_kind
rhog=500_r_kind !this is rhog in WSM6 paper, dif. from DART 400

param_r=(7.2e20_r_kind)/(((pi*rhor)**1.75_r_kind)*(n0r**0.75_r_kind))
param_dry_g=dielectric*(rhog/rhor)*(rhog/rhor)*(7.2e20_r_kind)/(((pi*rhog)**1.75_r_kind)*(n0g**0.75_r_kind))
param_wet_g=(7.2e20_r_kind)/((((pi*rhog)**1.75_r_kind)*(n0g**0.75_r_kind))**0.95_r_kind)
param_wet_s=(7.2e20_r_kind)/(((pi*rhos)**1.75_r_kind)*(n0s**0.75_r_kind))
param_dry_s=dielectric*(rhos/rhor)*(rhos/rhor)*param_wet_s

thisqrgesin=qrgesin
!calculate actual reflectivity
zqr=param_r*((rhogesin*qrgesin)**1.75_r_kind)
if (tempgesin < 273.15_r_kind) then
  zqr=0_r_kind
  thisqrgesin=0_r_kind
  zqg=param_dry_g*((rhogesin*qggesin)**1.75_r_kind)
  zqs=param_dry_s*((rhogesin*qsgesin)**1.75_r_kind)
else if (tempgesin < 278.15_r_kind) then
  zqg=param_wet_g*((rhogesin*qggesin)**1.6675_r_kind)
  zqs=param_wet_s*((rhogesin*qsgesin)**1.75_r_kind)
else
  zqg=0_r_kind
  zqs=0_r_kind
endif

Ze = zqr+zqg+zqs 

if (tempgesin >= 273.15_r_kind) then
  jqr=(10_r_kind*param_r*(rhogesin**1.75_r_kind)*1.75_r_kind*(thisqrgesin**0.75_r_kind))/(log(10.0_r_kind)*Ze)
else
  jqr=0.0_r_kind
endif 

end subroutine jqr_dart

subroutine jqs_dart(qrgesin0,qsgesin0,qggesin0,rhogesin,tempgesin,jqs)
  use kinds, only: r_kind,r_double,i_kind
implicit none
real(r_kind) :: qsgesin0,qggesin0,qrgesin0
real(r_kind) :: qsgesin,rhogesin,tempgesin,jqs
real(r_kind) :: Ze,qrgesin,qggesin,zqr,zqs,zqg

real(r_kind) :: param_r,param_dry_g,param_wet_g,param_dry_s,param_wet_s
real(r_kind) ::n0r,n0s,n0g,rhor,rhos,rhog,dielectric,pi,thisqsgesin
 qrgesin=qrgesin0
 qsgesin=qsgesin0
 qggesin=qggesin0


pi=3.14159_r_kind
dielectric=0.224_r_kind
n0r=8e6_r_kind
n0s=3e6_r_kind !(2e6) !*exp(0.12*(min(273.15,tempgesin)-273.15)) !this is n0s in WSM6 paper, dif. from DART constant of 3e6
n0g=4e6_r_kind !values taken from jung et al 2008/lfo83
rhos=100_r_kind
rhor=1000_r_kind
rhog=500_r_kind !this is rhog in WSM6 paper, dif. from DART 400

param_r=(7.2e20_r_kind)/(((pi*rhor)**1.75_r_kind)*(n0r**0.75_r_kind))
param_dry_g=dielectric*(rhog/rhor)*(rhog/rhor)*(7.2e20_r_kind)/(((pi*rhog)**1.75_r_kind)*(n0g**0.75_r_kind))
param_wet_g=(7.2e20_r_kind)/((((pi*rhog)**1.75_r_kind)*(n0g**0.75_r_kind))**0.95_r_kind)
param_wet_s=(7.2e20_r_kind)/(((pi*rhos)**1.75_r_kind)*(n0s**0.75_r_kind))
param_dry_s=dielectric*(rhos/rhor)*(rhos/rhor)*param_wet_s

thisqsgesin=qsgesin
!calculate actual reflectivity
zqr=param_r*((rhogesin*qrgesin)**1.75_r_kind)
if (tempgesin < 273.15_r_kind) then
  zqr=0_r_kind
  zqg=param_dry_g*((rhogesin*qggesin)**1.75_r_kind)
  zqs=param_dry_s*((rhogesin*qsgesin)**1.75_r_kind)
else if (tempgesin < 278.15_r_kind) then
  zqg=param_wet_g*((rhogesin*qggesin)**1.6675_r_kind)
  zqs=param_wet_s*((rhogesin*qsgesin)**1.75_r_kind)
else
  zqg=0_r_kind
  zqs=0_r_kind
  thisqsgesin=0.0_r_kind
endif

Ze = zqr+zqg+zqs 
if (tempgesin < 273.15_r_kind) then
  jqs=(10_r_kind*param_dry_s*(rhogesin**1.75_r_kind)*1.75_r_kind*(thisqsgesin**0.75_r_kind))/(log(10.0_r_kind)*Ze)
else
  jqs=(10_r_kind*param_wet_s*(rhogesin**1.75_r_kind)*1.75_r_kind*(thisqsgesin**0.75_r_kind))/(log(10.0_r_kind)*Ze)
endif

end subroutine jqs_dart

subroutine jqg_dart(qrgesin0,qsgesin0,qggesin0,rhogesin,tempgesin,jqg)
  use kinds, only: r_kind,r_double,i_kind
implicit none
real(r_kind) :: qggesin0,qsgesin0,qrgesin0
real(r_kind) :: qggesin,rhogesin,tempgesin,jqg
real(r_kind) :: Ze,qrgesin,qsgesin,zqr,zqs,zqg,thisqggesin

real(r_kind) :: param_r,param_dry_g,param_wet_g,param_dry_s,param_wet_s
real(r_kind) ::n0r,n0s,n0g,rhor,rhos,rhog,dielectric,pi
 qrgesin=qrgesin0
 qsgesin=qsgesin0
 qggesin=qggesin0


pi=3.14159_r_kind
dielectric=0.224_r_kind
n0r=8e6_r_kind
n0s=3e6_r_kind !(2e6) !*exp(0.12*(min(273.15,tempgesin)-273.15)) !this is n0s in WSM6 paper, dif. from DART constant of 3e6
n0g=4e6_r_kind
rhos=100_r_kind
rhor=1000_r_kind
rhog=500_r_kind !this is rhog in WSM6 paper, dif. from DART 400

param_r=(7.2e20_r_kind)/(((pi*rhor)**1.75_r_kind)*(n0r**0.75_r_kind))
param_dry_g=dielectric*(rhog/rhor)*(rhog/rhor)*(7.2e20_r_kind)/(((pi*rhog)**1.75_r_kind)*(n0g**0.75_r_kind))
param_wet_g=(7.2e20_r_kind)/((((pi*rhog)**1.75_r_kind)*(n0g**0.75_r_kind))**0.95_r_kind)
param_wet_s=(7.2e20_r_kind)/(((pi*rhos)**1.75_r_kind)*(n0s**0.75_r_kind))
param_dry_s=dielectric*(rhos/rhor)*(rhos/rhor)*param_wet_s

thisqggesin=qggesin
!calculate actual reflectivity
zqr=param_r*((rhogesin*qrgesin)**1.75_r_kind)
if (tempgesin < 273.15_r_kind) then
  zqr=0_r_kind
  zqg=param_dry_g*((rhogesin*qggesin)**1.75_r_kind)
  zqs=param_dry_s*((rhogesin*qsgesin)**1.75_r_kind)
else if (tempgesin < 278.15_r_kind) then
  zqg=param_wet_g*((rhogesin*qggesin)**1.6675_r_kind)
  zqs=param_wet_s*((rhogesin*qsgesin)**1.75_r_kind)
else
  zqg=0_r_kind
  zqs=0_r_kind
  thisqggesin=0.0_r_kind
endif

Ze = zqr+zqg+zqs 

if (tempgesin < 273.15_r_kind) then
  jqg=(10_r_kind*param_dry_g*(rhogesin**1.75_r_kind)*1.75_r_kind*(thisqggesin**0.75_r_kind))/(log(10.0_r_kind)*Ze)
else
  jqg=(10_r_kind*param_wet_g*(rhogesin**1.6675_r_kind)*1.6675_r_kind*(thisqggesin**0.6675_r_kind))/(log(10.0_r_kind)*Ze)
endif
end subroutine jqg_dart

!hydrometeor first guess values are in g/kg but note that equations use kg/kg
subroutine hx_gaostensrud2012(qrgesin,qggesin,qsgesin,rhogesin,tempgesin,rDBZ)
  use kinds, only: r_kind,r_double,i_kind
implicit none
real(r_kind) :: qrgesin,qsgesin,qggesin,rhogesin,tempgesin,rDBZ
real(r_kind) :: zqr,zqg,zqs

zqr=(3.63e9_r_kind)*((rhogesin*qrgesin)**1.75_r_kind)
zqg=(4.33e10_r_kind)*((rhogesin*qggesin)**1.75_r_kind)
if(tempgesin < 273.15_r_kind) then 
  zqs=(9.8e8_r_kind)*((rhogesin*qsgesin)**1.75_r_kind)
else
  zqs=(4.26e11_r_kind)*((rhogesin*qsgesin)**1.75_r_kind)
endif
rDBZ=zqr+zqg+zqs
if (rdBZ > 1_r_kind) then
  rdBZ=10_r_kind*log10(rdBZ)
else
  rdBZ=0_r_kind
endif


!reflectivity threshold for no-precip:
if (rdBZ < 5_r_kind) rdBZ=5_r_kind

end subroutine hx_gaostensrud2012
end module setupdbz_lib
