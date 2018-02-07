!-------------------------------------------------------------------------------
! write out data in netcdf format
  subroutine write_netcdf(im, jm, slm, land_frac, oro, orf, hprime, ntiles, tile, geolon, geolat, lon, lat)
    implicit none
    integer, intent(in):: im, jm, ntiles, tile
    real, intent(in) :: lon(im), lat(jm)
    real, intent(in), dimension(im,jm)  :: slm, oro, orf, geolon, geolat, land_frac
    real, intent(in), dimension(im,jm,14):: hprime
    character(len=128) :: outfile
    integer            :: error, ncid, i
    integer            :: header_buffer_val = 16384      
    integer            :: fsize=65536, inital = 0  
    integer            :: dim1, dim2
    integer            :: dim_lon, dim_lat
    integer            :: id_lon,id_lat,id_geolon,id_geolat
    integer            :: id_slmsk,id_orog_raw,id_orog_filt,id_land_frac
    integer            :: id_stddev,id_convex
    integer            :: id_oa1,id_oa2,id_oa3,id_oa4
    integer            :: id_ol1,id_ol2,id_ol3,id_ol4
    integer            :: id_theta,id_gamma,id_sigma,id_elvmax
    include "netcdf.inc"

    if(ntiles > 1) then
      write(outfile, '(a,i4.4,a)') 'out.oro.tile', tile, '.nc'
    else
      outfile = "out.oro.nc"
    endif

    dim1=size(lon,1)
    dim2=size(lat,1)
    write(6,*) ' netcdf dims are: ',dim1, dim2
      
    !--- open the file
    error = NF__CREATE(outfile, IOR(NF_NETCDF4,NF_CLASSIC_MODEL), inital, fsize, ncid)
    call netcdf_err(error, 'Creating file '//trim(outfile) )
    !--- define dimension
    error = nf_def_dim(ncid, 'lon', dim1, dim_lon)
    call netcdf_err(error, 'define dimension lon for file='//trim(outfile) )
    error = nf_def_dim(ncid, 'lat', dim2, dim_lat)
    call netcdf_err(error, 'define dimension lat for file='//trim(outfile) )  

    !--- define field
    error = nf_def_var(ncid, 'lon', NF_FLOAT, 1, (/dim_lon/), id_lon)
    call netcdf_err(error, 'define var lon for file='//trim(outfile) )
    error = nf_put_att_text(ncid, id_lon, "cartesian_axis", 1, "X")
    call netcdf_err(error, 'put att cartesian_axis for lon for file='//trim(outfile) )
    error = nf_def_var(ncid, 'lat', NF_FLOAT, 1, (/dim_lat/), id_lat)
    call netcdf_err(error, 'define var lat for file='//trim(outfile) )
    error = nf_put_att_text(ncid, id_lat, "cartesian_axis", 1, "Y")
    call netcdf_err(error, 'put att cartesian_axis for lat for file='//trim(outfile) )

    error = nf_def_var(ncid, 'geolon', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_geolon)
    call netcdf_err(error, 'define var geolon for file='//trim(outfile) )
    error = nf_def_var(ncid, 'geolat', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_geolat)
    call netcdf_err(error, 'define var geolat for file='//trim(outfile) )
!---slmsk
    error = nf_def_var(ncid, 'slmsk', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_slmsk)
    call netcdf_err(error, 'define var slmsk for file='//trim(outfile) )
!--- land_frac
    error = nf_def_var(ncid, 'land_frac', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_land_frac)
    call netcdf_err(error, 'define var land_frac for file='//trim(outfile) )

!---orography - raw
    error = nf_def_var(ncid, 'orog_raw', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_orog_raw)
    call netcdf_err(error, 'define var orog_raw for file='//trim(outfile) )
!---orography - filtered
    error = nf_def_var(ncid, 'orog_filt', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_orog_filt)
    call netcdf_err(error, 'define var orog_filt for file='//trim(outfile) )
!---stddev
    error = nf_def_var(ncid, 'stddev', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_stddev)
    call netcdf_err(error, 'define var stddev for file='//trim(outfile) )
!---convexity
    error = nf_def_var(ncid, 'convexity', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_convex)
    call netcdf_err(error, 'define var convexity for file='//trim(outfile) )      
!---oa1 -> oa4
    error = nf_def_var(ncid, 'oa1', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_oa1)
    call netcdf_err(error, 'define var oa1 for file='//trim(outfile) )
    error = nf_def_var(ncid, 'oa2', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_oa2)
    call netcdf_err(error, 'define var oa2 for file='//trim(outfile) )
    error = nf_def_var(ncid, 'oa3', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_oa3)
    call netcdf_err(error, 'define var oa3 for file='//trim(outfile) )
    error = nf_def_var(ncid, 'oa4', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_oa4)
    call netcdf_err(error, 'define var oa4 for file='//trim(outfile) )
!---ol1 -> ol4
    error = nf_def_var(ncid, 'ol1', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_ol1)
    call netcdf_err(error, 'define var ol1 for file='//trim(outfile) )
    error = nf_def_var(ncid, 'ol2', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_ol2)
    call netcdf_err(error, 'define var ol2 for file='//trim(outfile) )
    error = nf_def_var(ncid, 'ol3', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_ol3)
    call netcdf_err(error, 'define var ol3 for file='//trim(outfile) )
    error = nf_def_var(ncid, 'ol4', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_ol4)
    call netcdf_err(error, 'define var ol4 for file='//trim(outfile) )
!---theta gamma sigma elvmax
    error = nf_def_var(ncid, 'theta', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_theta)
    call netcdf_err(error, 'define var theta for file='//trim(outfile) )
    error = nf_def_var(ncid, 'gamma', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_gamma)
    call netcdf_err(error, 'define var gamma for file='//trim(outfile) )
    error = nf_def_var(ncid, 'sigma', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_sigma)
    call netcdf_err(error, 'define var sigma for file='//trim(outfile) )
    error = nf_def_var(ncid, 'elvmax', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_elvmax)
    call netcdf_err(error, 'define var elvmax for file='//trim(outfile) )

    error = nf__enddef(ncid, header_buffer_val,4,0,4)
    call netcdf_err(error, 'end meta define for file='//trim(outfile) )
      
    !--- write out data
    error = nf_put_var_double( ncid, id_lon, lon(:))
    call netcdf_err(error, 'write var lon for file='//trim(outfile) )
    error = nf_put_var_double( ncid, id_lat, lat(:))
    call netcdf_err(error, 'write var lat for file='//trim(outfile) )
    error = nf_put_var_double( ncid, id_geolon, geolon(:dim1,:dim2))
    call netcdf_err(error, 'write var geolon for file='//trim(outfile) )
    error = nf_put_var_double( ncid, id_geolat, geolat(:dim1,:dim2))
    call netcdf_err(error, 'write var geolat for file='//trim(outfile) )

    error = nf_put_var_double( ncid, id_slmsk, slm(:dim1,:dim2))
    call netcdf_err(error, 'write var slmsk for file='//trim(outfile) )
    error = nf_put_var_double( ncid, id_land_frac, land_frac(:dim1,:dim2))
    call netcdf_err(error, 'write var land_frac for file='//trim(outfile) )

    error = nf_put_var_double( ncid, id_orog_raw, oro(:dim1,:dim2))
    call netcdf_err(error, 'write var orog_raw for file='//trim(outfile) )
    error = nf_put_var_double( ncid, id_orog_filt, orf(:dim1,:dim2))
    call netcdf_err(error, 'write var orog_filt for file='//trim(outfile) )

    error = nf_put_var_double( ncid, id_stddev, hprime(:dim1,:dim2,1))
    call netcdf_err(error, 'write var stddev for file='//trim(outfile) )
    error = nf_put_var_double( ncid, id_convex, hprime(:dim1,:dim2,2))
    call netcdf_err(error, 'write var convex for file='//trim(outfile) )

    error = nf_put_var_double( ncid, id_oa1, hprime(:dim1,:dim2,3))
    call netcdf_err(error, 'write var oa1 for file='//trim(outfile) )
    error = nf_put_var_double( ncid, id_oa2, hprime(:dim1,:dim2,4))
    call netcdf_err(error, 'write var oa2 for file='//trim(outfile) )
    error = nf_put_var_double( ncid, id_oa3, hprime(:dim1,:dim2,5))
    call netcdf_err(error, 'write var oa3 for file='//trim(outfile) )
    error = nf_put_var_double( ncid, id_oa4, hprime(:dim1,:dim2,6))
    call netcdf_err(error, 'write var oa4 for file='//trim(outfile) )

    error = nf_put_var_double( ncid, id_ol1, hprime(:dim1,:dim2,7))
    call netcdf_err(error, 'write var ol1 for file='//trim(outfile) )
    error = nf_put_var_double( ncid, id_ol2, hprime(:dim1,:dim2,8))
    call netcdf_err(error, 'write var ol2 for file='//trim(outfile) )
    error = nf_put_var_double( ncid, id_ol3, hprime(:dim1,:dim2,9))
    call netcdf_err(error, 'write var ol3 for file='//trim(outfile) )
    error = nf_put_var_double( ncid, id_ol4, hprime(:dim1,:dim2,10))
    call netcdf_err(error, 'write var ol4 for file='//trim(outfile) )

    error = nf_put_var_double( ncid, id_theta, hprime(:dim1,:dim2,11))
    call netcdf_err(error, 'write var theta for file='//trim(outfile) )
    error = nf_put_var_double( ncid, id_gamma, hprime(:dim1,:dim2,12))
    call netcdf_err(error, 'write var gamma for file='//trim(outfile) )
    error = nf_put_var_double( ncid, id_sigma, hprime(:dim1,:dim2,13))
    call netcdf_err(error, 'write var sigma for file='//trim(outfile) )
    error = nf_put_var_double( ncid, id_elvmax, hprime(:dim1,:dim2,14))
    call netcdf_err(error, 'write var elvmax for file='//trim(outfile) )

    error = nf_close(ncid) 
    call netcdf_err(error, 'close file='//trim(outfile) )  
      
  end subroutine

!-------------------------------------------------------------------------------
  subroutine netcdf_err( err, string )
      integer, intent(in) :: err
      character(len=*), intent(in) :: string
      character(len=256) :: errmsg
      include "netcdf.inc"

      if( err.EQ.NF_NOERR )return
      errmsg = NF_STRERROR(err)
      print*, trim(string), ': ', trim(errmsg)

      return
    end subroutine netcdf_err
