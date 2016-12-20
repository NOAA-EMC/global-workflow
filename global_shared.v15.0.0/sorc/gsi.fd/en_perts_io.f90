subroutine en_perts_get_from_save
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    en_perts_get_from_save  get content of en_perts from saved
!                                        files on each subdomain.
!   prgmmr: Hu               org: np22                date: 2015-01-22
!
! abstract: get en_perts from save.
!
!
! program history log:
!   2014-01-22  Hu     , initial documentation
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!
!$$$ end documentation block

  use gridmod, only: nlon,nlat,lat2,lon2,nsig
  use hybrid_ensemble_parameters, only: en_perts,ps_bar,nelen
  use hybrid_ensemble_parameters, only: n_ens,grd_ens,grd_anl
  use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use mpimod, only: ierror,mype
  use kinds, only: r_kind,i_kind,r_single
  use constants, only: max_varname_length
  use gsi_bundlemod, only: GSI_BundleGetPointer
  use gsi_metguess_mod, only: GSI_MetGuess_Bundle
  use mpeu_util, only: die
  implicit none

  real(r_single),pointer,dimension(:,:,:):: w3
  real(r_single),pointer,dimension(:,:):: w2

  integer(i_kind) n
  character(255) filename
  character(len=max_varname_length) varname
  integer(i_kind) istatus

  integer(i_kind) ic3,ic2
  integer(i_kind) iunit,nn

  iunit=20
  write(filename,'(a,I4.4)') 'saved_en_perts.pe',mype
  open(iunit,file=trim(filename),form='unformatted')
  do n=1,n_ens
!
     read(iunit) nn
     if(nn /= n) then
        write(6,*)' error in ensemble number. read in ',nn,' looking for ',n
        call stop2(999)
     endif
     read(iunit) ps_bar(:,:,1)
!
     do ic3=1,nc3d

        call gsi_bundlegetpointer(en_perts(n,1),trim(cvars3d(ic3)),w3,istatus)
        if(istatus/=0) then
           write(6,*)' error retrieving pointer to ',trim(cvars3d(ic3)),' for ensemble member ',n
           call stop2(999)
        end if

        read(iunit) varname
        if(trim(varname) == trim(cvars3d(ic3))) then
           read(iunit) w3
        else
           write(*,*) 'error match field: read in ',trim(varname), &
                      ' in cvars3d',trim(cvars3d(ic3))
        endif
     end do

     do ic2=1,nc2d

        call gsi_bundlegetpointer(en_perts(n,1),trim(cvars2d(ic2)),w2,istatus)
        if(istatus/=0) then
           write(6,*)' error retrieving pointer to ',trim(cvars2d(ic2)),' for ensemble member ',n
           call stop2(999)
        end if

        read(iunit) varname
        if(trim(varname) == trim(cvars2d(ic2))) then
           read(iunit) w2
        else
           write(*,*) 'error match field: read in ',trim(varname), &
                      ' in cvars2d',trim(cvars2d(ic2))
        endif
     end do
  end do

  close(iunit)
  return

end subroutine en_perts_get_from_save

subroutine en_perts_save
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    en_perts_save  save content in en_perts to a file.
!   prgmmr: Hu               org: np22                date: 2015-01-22
!
! abstract: save en_perts.
!
!
! program history log:
!   2014-01-22  Hu     , initial documentation
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!
!$$$ end documentation block

  use gridmod, only: nlon,nlat,lat2,lon2,nsig
  use hybrid_ensemble_parameters, only: en_perts,ps_bar,nelen
  use hybrid_ensemble_parameters, only: n_ens,grd_ens,grd_anl
  use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use mpimod, only: ierror,mype
  use kinds, only: r_kind,i_kind,r_single
  use gsi_bundlemod, only: GSI_BundleGetPointer
  use gsi_metguess_mod, only: GSI_MetGuess_Bundle
  use mpeu_util, only: die
  implicit none

  real(r_single),pointer,dimension(:,:,:):: w3
  real(r_single),pointer,dimension(:,:):: w2

  integer(i_kind) n
  character(255) filename
  integer(i_kind) istatus

  integer(i_kind) ic3,ic2
  integer(i_kind) iunit

  iunit=20
  write(filename,'(a,I4.4)') 'saved_en_perts.pe',mype
  if(mype==0) write(*,*) 'save en_perts as ', trim(filename)
  open(iunit,file=trim(filename),form='unformatted')
  do n=1,n_ens
!
     write(iunit) n
     write(iunit) ps_bar(:,:,1)
!
     do ic3=1,nc3d

        call gsi_bundlegetpointer(en_perts(n,1),trim(cvars3d(ic3)),w3,istatus)
        if(istatus/=0) then
           write(6,*)' error retrieving pointer to ',trim(cvars3d(ic3)),' for ensemble member ',n
           call stop2(999)
        end if

        write(iunit) cvars3d(ic3)
        write(iunit) w3

     end do
     do ic2=1,nc2d

        call gsi_bundlegetpointer(en_perts(n,1),trim(cvars2d(ic2)),w2,istatus)
        if(istatus/=0) then
           write(6,*)' error retrieving pointer to ',trim(cvars2d(ic2)),' for ensemble member ',n
           call stop2(999)
        end if

        write(iunit) cvars2d(ic2)
        write(iunit) w2

     end do
  end do

  close(iunit)
  return

end subroutine en_perts_save
