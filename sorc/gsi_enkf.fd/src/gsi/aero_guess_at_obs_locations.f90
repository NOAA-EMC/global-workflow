SUBROUTINE aero_guess_at_obs_locations(&
     &obstime,data_s,nchanl,nreal,nsig,n_aerosols,&
     &aero,aero_names)
  ! from M. Pagowski, added to this branch by C. Martin - 2/21/2019

  USE kinds, ONLY: r_kind,i_kind
  USE gsi_bundlemod, ONLY: gsi_bundlegetpointer
  USE gsi_chemguess_mod, ONLY: gsi_chemguess_bundle   
  USE gsi_chemguess_mod, ONLY: gsi_chemguess_get
  USE gridmod, ONLY: istart,jstart,nlon,nlat,lon1
  USE constants, ONLY: max_varname_length, zero, one
  USE mpimod, ONLY: mype
  USE guess_grids, ONLY: hrdifsig,nfldsig

  IMPLICIT NONE

! Declare passed variables

  INTEGER(i_kind), INTENT(in   ) :: nchanl,nreal,nsig, n_aerosols
  REAL(r_kind), INTENT(in   ) :: obstime

  REAL(r_kind),DIMENSION(nreal+nchanl), INTENT(in   ) ::data_s
  CHARACTER(len=max_varname_length), DIMENSION(n_aerosols), INTENT(in   ) :: aero_names

  REAL(r_kind),DIMENSION(nsig,n_aerosols), INTENT(  out) :: aero


  INTEGER(i_kind):: j,k,m1,ix,ix1,ixp,iy,iy1,iyp,ii
  INTEGER(i_kind):: itsig,itsigp

  REAL(r_kind):: w00,w01,w10,w11,dx,dy
  REAL(r_kind):: delx,dely,delx1,dely1,dtsig,dtsigp

  INTEGER(i_kind):: ilon, ilat, ier

  REAL(r_kind),POINTER,DIMENSION(:,:,:)::aeroges_itsig =>NULL()
  REAL(r_kind),POINTER,DIMENSION(:,:,:)::aeroges_itsigp=>NULL()

  m1=mype+1

  ilon      = 3  ! index of grid relative obs location (x)
  ilat      = 4  ! index of grid relative obs location (y)

  dx  = data_s(ilat)                 ! grid relative latitude
  dy  = data_s(ilon)                 ! grid relative longitude

! Set spatial interpolation indices and weights
  ix1=dx
  ix1=MAX(1,MIN(ix1,nlat))
  delx=dx-ix1
  delx=MAX(zero,MIN(delx,one))
  ix=ix1-istart(m1)+2
  ixp=ix+1
  IF(ix1==nlat) THEN
     ixp=ix
  END IF
  delx1=one-delx

  iy1=dy
  dely=dy-iy1
  iy=iy1-jstart(m1)+2
  IF(iy<1) THEN
     iy1=iy1+nlon
     iy=iy1-jstart(m1)+2
  END IF
  IF(iy>lon1+1) THEN
     iy1=iy1-nlon
     iy=iy1-jstart(m1)+2
  END IF
  iyp=iy+1
  dely1=one-dely

  w00=delx1*dely1; w10=delx*dely1; w01=delx1*dely; w11=delx*dely


! Get time interpolation factors for sigma files
  IF(obstime > hrdifsig(1) .AND. obstime < hrdifsig(nfldsig))THEN
     DO j=1,nfldsig-1
        IF(obstime > hrdifsig(j) .AND. obstime <= hrdifsig(j+1))THEN
           itsig=j
           itsigp=j+1
           dtsig=((hrdifsig(j+1)-obstime)/(hrdifsig(j+1)-hrdifsig(j)))
        END IF
     END DO
  ELSE IF(obstime <=hrdifsig(1))THEN
     itsig=1
     itsigp=1
     dtsig=one
  ELSE
     itsig=nfldsig
     itsigp=nfldsig
     dtsig=one
  END IF
  dtsigp=one-dtsig

  ier=0

  IF(n_aerosols>0)THEN
     IF(SIZE(gsi_chemguess_bundle)==1) THEN
        DO ii=1,n_aerosols
           CALL gsi_bundlegetpointer(gsi_chemguess_bundle(1),aero_names(ii),aeroges_itsig ,ier) 
           DO k=1,nsig
              aero(k,ii) =(aeroges_itsig(ix ,iy ,k)*w00+ &
                   aeroges_itsig(ixp,iy ,k)*w10+ &
                   aeroges_itsig(ix ,iyp,k)*w01+ &
                   aeroges_itsig(ixp,iyp,k)*w11)
           END DO
        ENDDO
     ELSE
        DO ii=1,n_aerosols
           CALL gsi_bundlegetpointer(gsi_chemguess_bundle(itsig ),aero_names(ii),aeroges_itsig ,ier) 
           CALL gsi_bundlegetpointer(gsi_chemguess_bundle(itsigp),aero_names(ii),aeroges_itsigp,ier) 
           DO k=1,nsig
              aero(k,ii) =(aeroges_itsig (ix ,iy ,k)*w00+ &
                   aeroges_itsig (ixp,iy ,k)*w10+ &
                   aeroges_itsig (ix ,iyp,k)*w01+ &
                   aeroges_itsig (ixp,iyp,k)*w11)*dtsig + &
                   (aeroges_itsigp(ix ,iy ,k)*w00+ &
                   aeroges_itsigp(ixp,iy ,k)*w10+ &
                   aeroges_itsigp(ix ,iyp,k)*w01+ &
                   aeroges_itsigp(ixp,iyp,k)*w11)*dtsigp
           END DO
        ENDDO
     ENDIF

  ENDIF

END SUBROUTINE aero_guess_at_obs_locations
