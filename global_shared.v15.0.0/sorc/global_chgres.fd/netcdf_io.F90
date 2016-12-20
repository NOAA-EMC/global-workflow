module netcdf_io

public :: write_gfs_head, write_gfs_data, write_gfs_data2, write_sfc_head 
private :: netcdf_err

contains
! write the GFSHEADO into a single control file 'sfc_ctrl.nc'
subroutine write_gfs_head(outfile,version, fhour,idate,nrec,latb,lonb,levs, &
        jcap,itrun,iorder,irealf,igen,latf,lonf,latr,lonr,ntrac,icen2,iens, &
        idpp,idsl,idvc,idvm,idvt,idrun,idusr,pdryini,ncldt,ixgr,nvcoord,    &
        idrt,recname,reclevtyp,reclev,vcoord,NTRACM,Cpi,Ri)

    implicit none
    character(len=*),intent(in)            :: outfile
    integer,intent(in) :: version
    real(kind=4),intent(in)   :: fhour,pdryini
    integer,intent(in) :: idate(4),iens(2)
    integer,intent(in) :: nrec,latb,lonb,levs,&
      jcap,itrun,iorder,irealf,igen,latf,lonf,latr,lonr,ntrac,icen2,&
      idpp,idsl,idvc,idvm,idvt,idrun,idusr,ncldt,ixgr,nvcoord
    integer,intent(in) :: idrt
    real(kind=4),intent(in)   :: vcoord(levs+1,nvcoord)
    character(len=*), intent(in)          :: recname(nrec)
    character(len=*), intent(in)          :: reclevtyp(nrec)
    integer,intent(in) :: reclev(nrec)
    integer,intent(in) :: ntracm
    real(kind=4), intent(in)   :: Cpi(ntracm+1)
    real(kind=4), intent(in)   :: Ri(ntracm+1)
    real(kind=4)  :: tmp(levs+1,nvcoord)
    integer  :: error, ncid, dim_string, dim_ntrac, dim_nrec, dim_nvcoord, dim_levsp
    integer  :: dim_four, dim_two, dim_ntracmp
    integer  :: id_fhour, id_pdryini, id_idate, id_iens, id_nrec, id_version
    integer  :: id_latb, id_lonb, id_levs, id_jcap, id_itrun, id_iorder
    integer  :: id_irealf, id_igen, id_latf, id_lonf, id_latr, id_lonr
    integer  :: id_ntrac, id_icen2, id_idpp, id_idsl, id_idvc, id_idvm
    integer  :: id_idvt, id_idrun, id_idusr, id_ncldt, id_ixgr, id_reclev
    integer  :: id_nvcoord, id_idrt, id_vcoord, id_recname, id_reclevtyp, id_Cpi, id_Ri
    integer  :: start(4), nwrite(4), n
    integer  :: header_buffer_val = 16384
    integer  :: fsize=65536, inital = 0
    integer  :: strlen=255
    include "netcdf.inc"

    !--- open the file
    error = NF__CREATE(outfile, IOR(NF_NETCDF4,NF_CLASSIC_MODEL), inital, fsize, ncid)
    call netcdf_err(error, 'Creating file '//trim(outfile) )

!    print*, "size of cpi = ", size(cpi(:)), size(ri(:))
!    print*, "levs=", levs, nvcoord
!    print*, "size of vcoord = ",size(vcoord,1), size(vcoord,2)

    !--- define dimension
    error = nf_def_dim(ncid, 'string', strlen, dim_string)
    call netcdf_err(error, 'define dimension string for file='//trim(outfile) )
    error = nf_def_dim(ncid, 'four', 4, dim_four)
    call netcdf_err(error, 'define dimension four for file='//trim(outfile) )
    error = nf_def_dim(ncid, 'two', 2, dim_two)
    call netcdf_err(error, 'define dimension two for file='//trim(outfile) )
    error = nf_def_dim(ncid, 'ntracmp', ntracm+1, dim_ntracmp)
    call netcdf_err(error, 'define dimension ntracmp for file='//trim(outfile) )
    error = nf_def_dim(ncid, 'nvcoord', nvcoord, dim_nvcoord)
    call netcdf_err(error, 'define dimension nvcoord for file='//trim(outfile) )
    error = nf_def_dim(ncid, 'nrec', nrec, dim_nrec)
    call netcdf_err(error, 'define dimension nrec for file='//trim(outfile) )
    error = nf_def_dim(ncid, 'levsp', levs+1, dim_levsp)
    call netcdf_err(error, 'define dimension levsp for file='//trim(outfile) )


    !--- define field
    error = nf_def_var(ncid, 'version', NF_INT, 0, (/0/), id_version)
    call netcdf_err(error, 'define var version for file='//trim(outfile) )
    error = nf_def_var(ncid, 'fhour', NF_FLOAT, 0, (/0/), id_fhour)
    call netcdf_err(error, 'define var fhour for file='//trim(outfile) )
    error = nf_def_var(ncid, 'pdryini', NF_FLOAT, 0, (/0/), id_pdryini)
    call netcdf_err(error, 'define var pdryini for file='//trim(outfile) )
    error = nf_def_var(ncid, 'idate', NF_INT, 1, (/dim_four/), id_idate)
    call netcdf_err(error, 'define var idate for file='//trim(outfile) )
    error = nf_def_var(ncid, 'iens', NF_INT, 1, (/dim_two/), id_iens)
    call netcdf_err(error, 'define var iens for file='//trim(outfile) )

!    error = nf_def_var(ncid, 'nrec', NF_INT, 0, (/0/), id_nrec)
!    call netcdf_err(error, 'define var nrec for file='//trim(outfile) )
    error = nf_def_var(ncid, 'latb', NF_INT, 0, (/0/), id_latb)
    call netcdf_err(error, 'define var latb for file='//trim(outfile) )
    error = nf_def_var(ncid, 'lonb', NF_INT, 0, (/0/), id_lonb)
    call netcdf_err(error, 'define var lonb for file='//trim(outfile) )
    error = nf_def_var(ncid, 'levs', NF_INT, 0, (/0/), id_levs)
    call netcdf_err(error, 'define var levs for file='//trim(outfile) )
    error = nf_def_var(ncid, 'jcap', NF_INT, 0, (/0/), id_jcap)
    call netcdf_err(error, 'define var jcap for file='//trim(outfile) )
    error = nf_def_var(ncid, 'itrun', NF_INT, 0, (/0/), id_itrun)
    call netcdf_err(error, 'define var itrun for file='//trim(outfile) )
    error = nf_def_var(ncid, 'iorder', NF_INT, 0, (/0/), id_iorder)
    call netcdf_err(error, 'define var iorder for file='//trim(outfile) )
    error = nf_def_var(ncid, 'irealf', NF_INT, 0, (/0/), id_irealf)
    call netcdf_err(error, 'define var irealf for file='//trim(outfile) )
    error = nf_def_var(ncid, 'igen', NF_INT, 0, (/0/), id_igen)
    call netcdf_err(error, 'define var igen for file='//trim(outfile) )
    error = nf_def_var(ncid, 'latf', NF_INT, 0, (/0/), id_latf)
    call netcdf_err(error, 'define var latf for file='//trim(outfile) )
    error = nf_def_var(ncid, 'lonf', NF_INT, 0, (/0/), id_lonf)
    call netcdf_err(error, 'define var lonf for file='//trim(outfile) )
    error = nf_def_var(ncid, 'latr', NF_INT, 0, (/0/), id_latr)
    call netcdf_err(error, 'define var latr for file='//trim(outfile) )
    error = nf_def_var(ncid, 'lonr', NF_INT, 0, (/0/), id_lonr)
    call netcdf_err(error, 'define var lonr for file='//trim(outfile) )
    error = nf_def_var(ncid, 'ntrac', NF_INT, 0, (/0/), id_ntrac)
    call netcdf_err(error, 'define var ntrac for file='//trim(outfile) )
    error = nf_def_var(ncid, 'icen2', NF_INT, 0, (/0/), id_icen2)
    call netcdf_err(error, 'define var icen2 for file='//trim(outfile) )
    error = nf_def_var(ncid, 'idpp', NF_INT, 0, (/0/), id_idpp)
    call netcdf_err(error, 'define var idpp for file='//trim(outfile) )
    error = nf_def_var(ncid, 'idsl', NF_INT, 0, (/0/), id_idsl)
    call netcdf_err(error, 'define var idsl for file='//trim(outfile) )
    error = nf_def_var(ncid, 'idvc', NF_INT, 0, (/0/), id_idvc)
    call netcdf_err(error, 'define var idvc for file='//trim(outfile) )
    error = nf_def_var(ncid, 'idvm', NF_INT, 0, (/0/), id_idvm)
    call netcdf_err(error, 'define var idvm for file='//trim(outfile) )
    error = nf_def_var(ncid, 'idvt', NF_INT, 0, (/0/), id_idvt)
    call netcdf_err(error, 'define var idvt for file='//trim(outfile) )
    error = nf_def_var(ncid, 'idrun', NF_INT, 0, (/0/), id_idrun)
    call netcdf_err(error, 'define var idrun for file='//trim(outfile) )
    error = nf_def_var(ncid, 'idusr', NF_INT, 0, (/0/), id_idusr)
    call netcdf_err(error, 'define var idusr for file='//trim(outfile) )
    error = nf_def_var(ncid, 'ncldt', NF_INT, 0, (/0/), id_ncldt)
    call netcdf_err(error, 'define var ncldt for file='//trim(outfile) )
    error = nf_def_var(ncid, 'ixgr', NF_INT, 0, (/0/), id_ixgr)
    call netcdf_err(error, 'define var ixgr for file='//trim(outfile) )
!    error = nf_def_var(ncid, 'nvcoord', NF_INT, 0, (/0/), id_nvcoord)
!    call netcdf_err(error, 'define var nvcoord for file='//trim(outfile) )
    error = nf_def_var(ncid, 'idrt', NF_INT, 0, (/0/), id_idrt)
    call netcdf_err(error, 'define var idrt for file='//trim(outfile) )
    error = nf_def_var(ncid, 'recname', NF_CHAR, 2, (/dim_string,dim_nrec/), id_recname)
    call netcdf_err(error, 'define var recname for file='//trim(outfile) )
    error = nf_def_var(ncid, 'reclevtyp', NF_CHAR, 2, (/dim_string,dim_nrec/), id_reclevtyp)
    call netcdf_err(error, 'define var reclevtyp for file='//trim(outfile) )
    error = nf_def_var(ncid, 'reclev', NF_INT, 1, (/dim_nrec/), id_reclev)
    call netcdf_err(error, 'define var reclev for file='//trim(outfile) )
    error = nf_def_var(ncid, 'vcoord', NF_FLOAT, 2, (/dim_levsp, dim_nvcoord/), id_vcoord)
    call netcdf_err(error, 'define var vcoord for file='//trim(outfile) )   
    error = nf_def_var(ncid, 'Cpi', NF_FLOAT, 1, (/dim_ntracmp/), id_Cpi)
    call netcdf_err(error, 'define var Cpi for file='//trim(outfile) )
    error = nf_def_var(ncid, 'Ri', NF_FLOAT, 1, (/dim_ntracmp/), id_Ri)
    call netcdf_err(error, 'define var Ri for file='//trim(outfile) )



    error = nf__enddef(ncid, header_buffer_val,4,0,4)
    call netcdf_err(error, 'end meta define for file='//trim(outfile) )


    !--- write out data
    error = nf_put_var_int( ncid, id_version, version)
    call netcdf_err(error, 'write var version for file='//trim(outfile) )
    error = nf_put_var_real( ncid, id_fhour, fhour)
    call netcdf_err(error, 'write var fhour for file='//trim(outfile) )
    error = nf_put_var_real( ncid, id_pdryini, pdryini)
    call netcdf_err(error, 'write var pdryini for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_idate, idate )
    call netcdf_err(error, 'write var idate for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_iens, iens )
    call netcdf_err(error, 'write var iens for file='//trim(outfile) )

!    error = nf_put_var_int( ncid, id_nrec, nrec)
!    call netcdf_err(error, 'write var nrec for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_latb, latb)
    call netcdf_err(error, 'write var latb for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_lonb, lonb)
    call netcdf_err(error, 'write var lonb for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_levs, levs)
    call netcdf_err(error, 'write var levs for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_jcap, jcap)
    call netcdf_err(error, 'write var jcap for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_itrun, itrun)
    call netcdf_err(error, 'write var itrun for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_iorder, iorder)
    call netcdf_err(error, 'write var iorder for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_irealf, irealf)
    call netcdf_err(error, 'write var irealf for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_igen, igen)
    call netcdf_err(error, 'write var igen for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_latf, latf)
    call netcdf_err(error, 'write var latf for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_lonf, lonf)
    call netcdf_err(error, 'write var lonf for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_latr, latr)
    call netcdf_err(error, 'write var latr for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_lonr, lonr)
    call netcdf_err(error, 'write var lonr for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_ntrac, ntrac)
    call netcdf_err(error, 'write var ntrac for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_icen2, icen2)
    call netcdf_err(error, 'write var icen2 for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_idpp, idpp)
    call netcdf_err(error, 'write var idpp for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_idsl, idsl)
    call netcdf_err(error, 'write var idsl for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_idvc, idvc)
    call netcdf_err(error, 'write var idvc for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_idvm, idvm)
    call netcdf_err(error, 'write var idvm for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_idvt, idvt)
    call netcdf_err(error, 'write var idvt for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_idrun, idrun)
    call netcdf_err(error, 'write var idrun for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_idusr, idusr)
    call netcdf_err(error, 'write var idusr for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_ncldt, ncldt)
    call netcdf_err(error, 'write var ncldt for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_ixgr, ixgr)
    call netcdf_err(error, 'write var ixgr for file='//trim(outfile) )
!    error = nf_put_var_int( ncid, id_nvcoord, nvcoord)
!    call netcdf_err(error, 'write var nvcoord for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_idrt, idrt)
    call netcdf_err(error, 'write var idrt for file='//trim(outfile) )
    start = 1
    nwrite = 1
    do n = 1, nrec
      start(2) = n
      nwrite(1) = len_trim(recname(n))
      error = nf_put_vara_text( ncid, id_recname, start, nwrite, trim(recname(n)))
      call netcdf_err(error, 'write var recname for file='//trim(outfile) )
      nwrite(1) = len_trim(reclevtyp(n))
      error = nf_put_vara_text( ncid, id_reclevtyp, start, nwrite, trim(reclevtyp(n)))
      call netcdf_err(error, 'write var reclevtyp for file='//trim(outfile) )
    enddo
    error = nf_put_var_int( ncid, id_reclev, reclev)
    call netcdf_err(error, 'write var reclev for file='//trim(outfile) )

    tmp(1:levs+1,:) = vcoord(levs+1:1:-1,:)
    error = nf_put_var_real( ncid, id_vcoord, tmp)
    call netcdf_err(error, 'write var vcoord for file='//trim(outfile) )

    error = nf_put_var_real( ncid, id_Cpi, cpi)
    call netcdf_err(error, 'write var Cpi for file='//trim(outfile) )
    error = nf_put_var_real( ncid, id_Ri, Ri)
    call netcdf_err(error, 'write var Ri for file='//trim(outfile) )


    error = nf_close(ncid)
    call netcdf_err(error, 'close file='//trim(outfile) )

  end subroutine write_gfs_head

  subroutine write_gfs_data(ni,nj,nk,nq,ps,w,zh,q,write_grid,lon,lat,outfile)    
    implicit none
    integer,         intent(in) :: ni, nj, nk, nq
    real(kind=4),    intent(in) :: ps(ni,nj)
    real(kind=4),    intent(in) :: w(ni,nj,nk)
    real(kind=4),    intent(in) :: zh(ni,nj,nk+1)
    real(kind=4),    intent(in) :: q(ni,nj,nk,nq)
    logical,         intent(in) :: write_grid
    real(kind=4),    intent(in) :: lon(ni,nj), lat(ni,nj)
    character(len=*),intent(in) :: outfile

    real(kind=4) :: tmp(ni,nj,nk,1)
    real(kind=4) :: tmp2(ni,nj,nk+1,1)
    integer  :: error, ncid, dim_lon, dim_lat, dim_lev, dim_ntracer
    integer  :: id_ps, id_t, id_w
    integer  :: id_sphum, id_o3mr, id_clwmr, id_zh, dim_levp
    integer  :: id_lon, id_lat, l
    integer  :: header_buffer_val = 16384
    integer  :: fsize=65536, inital = 0
    integer  :: strlen=255
    include "netcdf.inc"

    !--- open the file
    error = NF__CREATE(outfile, IOR(NF_NETCDF4,NF_CLASSIC_MODEL), inital, fsize, ncid)
    call netcdf_err(error, 'Creating file '//trim(outfile) )
    !--- define dimesion
    error = nf_def_dim(ncid, 'lon', ni, dim_lon)
    call netcdf_err(error, 'define dimension lon for file='//trim(outfile) )
    error = nf_def_dim(ncid, 'lat', nj, dim_lat)
    call netcdf_err(error, 'define dimension lat for file='//trim(outfile) )
    error = nf_def_dim(ncid, 'lev', nk, dim_lev)
    call netcdf_err(error, 'define dimension lev for file='//trim(outfile) )
    error = nf_def_dim(ncid, 'levp', nk+1, dim_levp)
        call netcdf_err(error, 'define dimension levp for file='//trim(outfile) )
    error = nf_def_dim(ncid, 'ntracer', nq, dim_ntracer)
    call netcdf_err(error, 'define dimension ntracer for file='//trim(outfile) )

    !--- define field
    if(write_grid) then 
      error = nf_def_var(ncid, 'lon', NF_FLOAT, 1, (/dim_lon/), id_lon)
      call netcdf_err(error, 'define var lon for file='//trim(outfile) )
      error = nf_put_att_text(ncid, id_lon, "cartesian_axis", 1, "X")
      call netcdf_err(error, 'put att cartesian_axis for lon for file='//trim(outfile) )
      error = nf_def_var(ncid, 'lat', NF_FLOAT, 1, (/dim_lat/), id_lat)
      call netcdf_err(error, 'define var lat for file='//trim(outfile) )
      error = nf_put_att_text(ncid, id_lat, "cartesian_axis", 1, "Y")
      call netcdf_err(error, 'put att cartesian_axis for lat for file='//trim(outfile) )
    endif
 
    error = nf_def_var(ncid, 'ps', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_ps)
    call netcdf_err(error, 'define var ps for file='//trim(outfile) )
    error = nf_def_var(ncid, 'w', NF_FLOAT, 3, (/dim_lon,dim_lat,dim_lev/), id_w)
    call netcdf_err(error, 'define var w for file='//trim(outfile) )
    error = nf_def_var(ncid, 'zh', NF_FLOAT, 3, (/dim_lon,dim_lat,dim_levp/),id_zh)
    call netcdf_err(error, 'define var zh for file='//trim(outfile) )

    error = nf_def_var(ncid, 'sphum', NF_FLOAT, 3, (/dim_lon,dim_lat,dim_lev/), id_sphum)
    call netcdf_err(error, 'define var w for file='//trim(outfile) )
    error = nf_def_var(ncid, 'o3mr', NF_FLOAT, 3, (/dim_lon,dim_lat,dim_lev/), id_o3mr)
    call netcdf_err(error, 'define var w for file='//trim(outfile) )
    error = nf_def_var(ncid, 'liq_wat', NF_FLOAT, 3, (/dim_lon,dim_lat,dim_lev/), id_clwmr)
    call netcdf_err(error, 'define var w for file='//trim(outfile) )

    error = nf__enddef(ncid, header_buffer_val,4,0,4)
    call netcdf_err(error, 'end meta define for file='//trim(outfile) )

    !--- write data
    if(write_grid) then
      error = nf_put_var_real( ncid, id_lon, lon(:,1))
      call netcdf_err(error, 'write var lon for file='//trim(outfile) )
      error = nf_put_var_real( ncid, id_lat, lat(1,:))
      call netcdf_err(error, 'write var lat for file='//trim(outfile) )
    endif
    error = nf_put_var_real( ncid, id_ps, ps)
    call netcdf_err(error, 'write var ps for file='//trim(outfile) )
    tmp(:,:,1:nk,1) = w(:,:,nk:1:-1)
    error = nf_put_var_real( ncid, id_w, tmp(:,:,:,1))
    call netcdf_err(error, 'write var w for file='//trim(outfile) )
    tmp2(:,:,1:nk+1,1) = zh(:,:,nk+1:1:-1)
    error = nf_put_var_real( ncid, id_zh, tmp2(:,:,:,1))
    call netcdf_err(error, 'write var zh for file='//trim(outfile) )
    tmp(:,:,1:nk,1) = q(:,:,nk:1:-1,1)
    error = nf_put_var_real( ncid, id_sphum, tmp(:,:,:,1))
    call netcdf_err(error, 'write var sphum for file='//trim(outfile) )
    tmp(:,:,1:nk,1) = q(:,:,nk:1:-1,2)
    error = nf_put_var_real( ncid, id_o3mr, tmp(:,:,:,1))
    call netcdf_err(error, 'write var o3mr for file='//trim(outfile) )
    tmp(:,:,1:nk,1) = q(:,:,nk:1:-1,3)
    error = nf_put_var_real( ncid, id_clwmr, tmp(:,:,:,1))
    call netcdf_err(error, 'write var clwmr for file='//trim(outfile) )

    error = nf_close(ncid)
    call netcdf_err(error, 'close file='//trim(outfile) )
  end subroutine write_gfs_data

!  subroutine write_gfs_data2(ni,nj,nk,nq,zs,ps,p,dp,t,u,v,w,zh,rh,q,write_grid,lon,lat,outfile, &
  subroutine write_gfs_data2(ni,nj,nk,nq,outfile,ps,var3d,var_name,write_data,write_grid,header,&
                             close,lon,lat)
    implicit none
    integer,         intent(in) :: ni, nj, nk, nq
    character(len=*),intent(in) :: outfile
    real(kind=4),    intent(in), optional :: ps(ni,nj)
    real(kind=4),    intent(in), optional :: var3d(ni,nj,nk)
    character(len=*),intent(in), optional :: var_name
    logical,         intent(in), optional :: write_data
    logical,         intent(in), optional :: write_grid
    logical,         intent(in), optional :: header
    logical,         intent(in), optional :: close
    real(kind=4),    intent(in), optional :: lon(ni,nj), lat(ni,nj)

    real(kind=4) :: tmp(ni,nj,nk)
    integer, save :: error, ncid, dim_lon, dim_lat, dim_lev
    integer, save :: id_ps, id_t, id_w
    integer, save :: id_sphum, id_o3mr, id_clwmr, id_zh, dim_levp
    integer, save :: dim_lonp,dim_latp,id_u_w,id_v_w,id_u_s,id_v_s
    integer, save :: id_lon, id_lat, l
    integer  :: dim_ntracer
    integer  :: header_buffer_val = 16384
    integer  :: fsize=65536, inital = 0
    integer  :: strlen=255
    include "netcdf.inc"

    if (present(header)) then
    if (header) then
      !--- open the file
      error = NF__CREATE(outfile, IOR(NF_NETCDF4,NF_CLASSIC_MODEL), inital, fsize, ncid)
      call netcdf_err(error, 'Creating file '//trim(outfile) )
      !--- define dimesion
      error = nf_def_dim(ncid, 'lon', ni, dim_lon)
      call netcdf_err(error, 'define dimension lon for file='//trim(outfile) )
      error = nf_def_dim(ncid, 'lat', nj, dim_lat)
      call netcdf_err(error, 'define dimension lat for file='//trim(outfile) )
      error = nf_def_dim(ncid, 'lonp', ni+1, dim_lonp)
      call netcdf_err(error, 'define dimension lon for file='//trim(outfile) )
      error = nf_def_dim(ncid, 'latp', nj+1, dim_latp)
      call netcdf_err(error, 'define dimension lat for file='//trim(outfile) )

      error = nf_def_dim(ncid, 'lev', nk, dim_lev)
      call netcdf_err(error, 'define dimension lev for file='//trim(outfile) )
      error = nf_def_dim(ncid, 'levp', nk+1, dim_levp)
      call netcdf_err(error, 'define dimension levp for file='//trim(outfile) )

      error = nf_def_dim(ncid, 'ntracer', nq, dim_ntracer)
      call netcdf_err(error, 'define dimension ntracer for file='//trim(outfile) )

      !--- define grid
      error = nf_def_var(ncid, 'lon', NF_FLOAT, 1, (/dim_lon/), id_lon)
      call netcdf_err(error, 'define var lon for file='//trim(outfile) )
      error = nf_put_att_text(ncid, id_lon, "cartesian_axis", 1, "X")
      call netcdf_err(error, 'put att cartesian_axis for lon for file='//trim(outfile) )
      error = nf_def_var(ncid, 'lat', NF_FLOAT, 1, (/dim_lat/), id_lat)
      call netcdf_err(error, 'define var lat for file='//trim(outfile) )
      error = nf_put_att_text(ncid, id_lat, "cartesian_axis", 1, "Y")
      call netcdf_err(error, 'put att cartesian_axis for lat for file='//trim(outfile) )
 
      !--- define ps
      error = nf_def_var(ncid, 'ps', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_ps)
      call netcdf_err(error, 'define var ps for file='//trim(outfile) )
      !--- define w
      error = nf_def_var(ncid, 'w', NF_FLOAT, 3, (/dim_lon,dim_lat,dim_lev/), id_w)
      call netcdf_err(error, 'define var w for file='//trim(outfile) )
      !--- define zh
      error = nf_def_var(ncid, 'zh', NF_FLOAT, 3, (/dim_lon,dim_lat,dim_levp/),id_zh)
      call netcdf_err(error, 'define var zh for file='//trim(outfile) )

      !--- define tracers
      error = nf_def_var(ncid, 'sphum', NF_FLOAT, 3, (/dim_lon,dim_lat,dim_lev/), id_sphum)
      call netcdf_err(error, 'define var w for file='//trim(outfile) )
      error = nf_def_var(ncid, 'o3mr', NF_FLOAT, 3, (/dim_lon,dim_lat,dim_lev/), id_o3mr)
      call netcdf_err(error, 'define var w for file='//trim(outfile) )
      error = nf_def_var(ncid, 'liq_wat', NF_FLOAT, 3, (/dim_lon,dim_lat,dim_lev/), id_clwmr)
      call netcdf_err(error, 'define var w for file='//trim(outfile) )

      !--- define west winds
      error = nf_def_var(ncid, 'u_w', NF_FLOAT, 3, (/dim_lonp,dim_lat,dim_lev/), id_u_w)
      call netcdf_err(error, 'define var u_w for file='//trim(outfile) )
      error = nf_def_var(ncid, 'v_w', NF_FLOAT, 3, (/dim_lonp,dim_lat,dim_lev/), id_v_w)
      call netcdf_err(error, 'define var v_w for file='//trim(outfile) )

      !--- define north winds
      error = nf_def_var(ncid, 'u_s', NF_FLOAT, 3, (/dim_lon,dim_latp,dim_lev/), id_u_s)
      call netcdf_err(error, 'define var u_s for file='//trim(outfile) )
      error = nf_def_var(ncid, 'v_s', NF_FLOAT, 3, (/dim_lon,dim_latp,dim_lev/), id_v_s)
      call netcdf_err(error, 'define var v_s for file='//trim(outfile) )

      !--- add buffer space to metadata header
      error = nf__enddef(ncid, header_buffer_val,4,0,4)
      call netcdf_err(error, 'end meta define for file='//trim(outfile) )
    endif
    endif

    !--- write grid data
    if (present(write_grid)) then
    if (write_grid) then
      error = nf_put_var_real( ncid, id_lon, lon(:,1))
      call netcdf_err(error, 'write var lon for file='//trim(outfile) )
      error = nf_put_var_real( ncid, id_lat, lat(1,:))
      call netcdf_err(error, 'write var lat for file='//trim(outfile) )
    endif
    endif

    if (present(write_data)) then
    if (write_data) then
      !--- write ps
      if (trim(var_name) == 'ps') then
        error = nf_put_var_real( ncid, id_ps, ps)
        call netcdf_err(error, 'write var ps for file='//trim(outfile) )
      endif

      !--- write w
      if (trim(var_name) == 'w') then
        tmp(:,:,1:nk) = var3d(:,:,nk:1:-1)
        error = nf_put_var_real( ncid, id_w, tmp(:,:,:))
        call netcdf_err(error, 'write var w for file='//trim(outfile) )
      endif

      !--- write zh
      if (trim(var_name) == 'zh') then
        tmp(:,:,1:nk) = var3d(:,:,nk:1:-1)
        error = nf_put_var_real( ncid, id_zh, tmp(:,:,:))
        call netcdf_err(error, 'write var zh for file='//trim(outfile) )
      endif

      !--- write tracers
      if (trim(var_name) == 'sphum') then
        tmp(:,:,1:nk) = var3d(:,:,nk:1:-1)
        error = nf_put_var_real( ncid, id_sphum, tmp(:,:,:))
        call netcdf_err(error, 'write var sphum for file='//trim(outfile) )
      endif
      if (trim(var_name) == 'o3mr') then
        tmp(:,:,1:nk) = var3d(:,:,nk:1:-1)
        error = nf_put_var_real( ncid, id_o3mr, tmp(:,:,:))
        call netcdf_err(error, 'write var o3mr for file='//trim(outfile) )
      endif
      if (trim(var_name) == 'clwmr') then
        tmp(:,:,1:nk) = var3d(:,:,nk:1:-1)
        error = nf_put_var_real( ncid, id_clwmr, tmp(:,:,:))
        call netcdf_err(error, 'write var clwmr for file='//trim(outfile) )
      endif

      !--- write east winds
      if (trim(var_name) == 'u_e') then
        tmp(:,:,1:nk) = var3d(:,:,nk:1:-1)
        error = nf_put_var_real( ncid, id_u_w, tmp(:,:,:))
        call netcdf_err(error, 'write var u_w for file='//trim(outfile) )
      endif
      if (trim(var_name) == 'v_e') then
        tmp(:,:,1:nk) = var3d(:,:,nk:1:-1)
        error = nf_put_var_real( ncid, id_v_w, tmp(:,:,:))
        call netcdf_err(error, 'write var v_w for file='//trim(outfile) )
      endif

      !--- write north winds
      if (trim(var_name) == 'u_n') then
        tmp(:,:,1:nk) = var3d(:,:,nk:1:-1)
        error = nf_put_var_real( ncid, id_u_s, tmp(:,:,:))
        call netcdf_err(error, 'write var u_s for file='//trim(outfile) )
      endif
      if (trim(var_name) == 'v_n') then
        tmp(:,:,1:nk) = var3d(:,:,nk:1:-1)
        error = nf_put_var_real( ncid, id_v_s, tmp(:,:,:))
        call netcdf_err(error, 'write var v_s for file='//trim(outfile) )
      endif
    endif
    endif

    if (present(close)) then
    if (close) then
      !--- close the file
      error = nf_close(ncid)
      call netcdf_err(error, 'close file='//trim(outfile) )
    endif
    endif

  end subroutine write_gfs_data2




 subroutine write_sfc_head(outfile,version,lsoil,fhour,idate,lonb,latb,irealf)
    implicit none
    character(len=*),intent(in) :: outfile
    integer,         intent(in) :: version, lsoil, lonb, latb, irealf, idate(4)
    real(kind=4),    intent(in) :: fhour


    integer  :: error, ncid, dim_four, dim_string
    integer  :: id_cgfs,id_csfc,id_version,id_nhead,id_ndata,id_nresv
    integer  :: id_fhour,id_idate,id_lonb,id_latb,id_lsoil,id_irealf
    integer  :: header_buffer_val = 16384
    integer  :: fsize=65536, inital = 0
    integer  :: strlen=255
    include "netcdf.inc"

    if(version .ne. 200509 .and.  version .ne. 200501) then
       print*, "version must be either 200509 or 200501"
       call ABORT()
    endif  

    !--- open the file
    error = NF__CREATE(outfile, IOR(NF_NETCDF4,NF_CLASSIC_MODEL), inital, fsize, ncid)
    call netcdf_err(error, 'Creating file '//trim(outfile) )


    !--- define dimension
    error = nf_def_dim(ncid, 'string', 3, dim_string)
    call netcdf_err(error, 'define dimension string for file='//trim(outfile) )
    error = nf_def_dim(ncid, 'four', 4, dim_four)
    call netcdf_err(error, 'define dimension four for file='//trim(outfile) )

    !--- define field
    error = nf_def_var(ncid, 'cgfs', NF_CHAR, 1, (/dim_string/), id_cgfs)
    call netcdf_err(error, 'define var cgfs for file='//trim(outfile) )
    error = nf_def_var(ncid, 'csfc', NF_CHAR, 1, (/dim_string/), id_csfc)
    call netcdf_err(error, 'define var csfc for file='//trim(outfile) )
    error = nf_def_var(ncid, 'version', NF_INT, 0, (/0/), id_version)
    call netcdf_err(error, 'define var version for file='//trim(outfile) )
    error = nf_def_var(ncid, 'nhead', NF_INT, 0, (/0/), id_nhead)
    call netcdf_err(error, 'define var nhead for file='//trim(outfile) )
    error = nf_def_var(ncid, 'ndata', NF_INT, 0, (/0/), id_ndata)
    call netcdf_err(error, 'define var ndata for file='//trim(outfile) )
    error = nf_def_var(ncid, 'nresv', NF_INT, 0, (/0/), id_nresv)
    call netcdf_err(error, 'define var nresv for file='//trim(outfile) )
    error = nf_def_var(ncid, 'fhour', NF_FLOAT, 0, (/0/), id_fhour)
    call netcdf_err(error, 'define var fhour for file='//trim(outfile) )
    error = nf_def_var(ncid, 'idate', NF_INT, 1, (/dim_four/), id_idate)
    call netcdf_err(error, 'define var idate for file='//trim(outfile) )
    error = nf_def_var(ncid, 'lonb', NF_INT, 0, (/0/), id_lonb)
    call netcdf_err(error, 'define var lonb for file='//trim(outfile) )
    error = nf_def_var(ncid, 'latb', NF_INT, 0, (/0/), id_latb)
    call netcdf_err(error, 'define var latb for file='//trim(outfile) )
    error = nf_def_var(ncid, 'lsoil', NF_INT, 0, (/0/), id_lsoil)
    call netcdf_err(error, 'define var lsoil for file='//trim(outfile) )
    error = nf_def_var(ncid, 'irealf', NF_INT, 0, (/0/), id_irealf)
    call netcdf_err(error, 'define var irealf for file='//trim(outfile) )

    error = nf__enddef(ncid, header_buffer_val,4,0,4)
    call netcdf_err(error, 'end meta define for file='//trim(outfile) )

    !--- write out data
    error = nf_put_var_text( ncid, id_cgfs, "GFS")
    call netcdf_err(error, 'write var cgfs for file='//trim(outfile) )
    error = nf_put_var_text( ncid, id_csfc, "SFC")
    call netcdf_err(error, 'write var csfc for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_version, version)
    call netcdf_err(error, 'write var version for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_nhead, 5)
    call netcdf_err(error, 'write var nhead for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_ndata, 29+3*lsoil)
    call netcdf_err(error, 'write var ndata for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_nresv, 0)
    call netcdf_err(error, 'write var nresv for file='//trim(outfile) )
    error = nf_put_var_real( ncid, id_fhour, fhour)
    call netcdf_err(error, 'write var fhour for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_idate, idate)
    call netcdf_err(error, 'write var idate for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_lonb, lonb)
    call netcdf_err(error, 'write var lonb for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_latb, latb)
    call netcdf_err(error, 'write var latb for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_lsoil, lsoil)
    call netcdf_err(error, 'write var lsoil for file='//trim(outfile) )
    error = nf_put_var_int( ncid, id_irealf, irealf)
    call netcdf_err(error, 'write var irealf for file='//trim(outfile) )

  end subroutine write_sfc_head


  subroutine netcdf_err( err, string )
      integer, intent(in) :: err
      character(len=*), intent(in) :: string
      character(len=256) :: errmsg
      include "netcdf.inc"

      if( err.EQ.NF_NOERR )return
      errmsg = NF_STRERROR(err)
      print*, trim(string), ': ', trim(errmsg)
      call ABORT()

      return
  end subroutine netcdf_err

end module netcdf_io
