GSM Modification Instructions
-----------------------------

### How to add new variable to sigf file:

1. Go to `~/src/atmos/gfs/dyn` directory

2. Search the new variable, 

    a. If it is in a module, add that module in subroutine
    POINT_DYNAMICS_OUTPUT_GFS in gfs_dynamics_output.f

    b. If it is not in any module, add the new variable in dynamice
    internal state, so that it can be passed into
    POINT_DYNAMICS_OUTPUT_GFS

      1. declare the variable in gfs_dynamics_internal_state_mod.f

      2. allocate the variable in gfs_dynamics_initialize_mod.f

    c. If the new variable is a 2D or 3D field in grid_gr, do nothing
    (grid_gr has already be declared and allocated)

3. In gfs_dynamics_output.f, add the variable name to the
   corresponding output list, set the pointer in subroutine
   POINT_DYNAMICS_OUTPUT_GFS for the new variable. (see example below)

4.  In gfs_dynamics_initialize_mod.f, increase the dimension of the
   output full grid buffer, `buff_mult_pieceg`, `ngrids_gg`,
   `ngrids_gg=ngrids_gg+1(2d)`, or `ngrids_gg=ngrids_gg+fld_levs(3d)`

5. If the new variable is a 2D or 3D array, pass it into
   wrtout_dynamics, add the variable to subroutine grid_collect. In
   grid_collect.f interpolate the field into full grid field and save the
   data in buff_mult_pieceg.

With these changes, a field (2d or3d array) or an attribute(1d
int, real, log) will be added into the sigma field bundle in import
write state, and it will then be written out in write grid component.
Eg, to add dpdt(pressure tendency: ptend) in sigf file:

1. dpdt is in grid_gr
2. in gfs_dynamics_output.f, in DYN_INT_STATE_3D_R_ADIAB, add

        ,'ptend       ', 'OGFS_SIG  ', 'levs      ' &

before tracer "spfh". If adding a new tracer, add that tracer after
"clwmr".  Notice in POINT_DYNAMICS_OUTPUT_GFS, the pointer for 3d
real array output is set to buff_mult_pieceg

        R_3D(1)%NAME=>buff_mult_pieceg

we will add the output field ptend in buff_mult_pieceg

3. in subroutine wrtout_dynamics in wrtout_dynamics.f, get dpdt from
`grid_gr(:,:,g_dpdt:gdpdt+levs-1)`, and pass dpdt to grid_collect

        !
         do k=1,levs
           do i=1,lons_lat
             dpdt(i,lan,k) = grid_gr(i+jlonf,g_dpdt-1+k)
           enddo
         enddo
         call grid_collect (zsg,psg,uug,vvg,ttg,rqg,dpg,dpdt,
        &          global_lats_a,lonsperlat)

4. in  gfs_dynamics_initialize_mod.f, 

        ngrids_gg=ngrids_gg+levs

5. in grid_collect.f, interpolate the field from reduced grid to full
grid, and add this field in buff_mult_pieceg before `tracersi(rqg)`
start:

       !
        do k=1,levs
          buffi(:,:) = dpdt(:,:,k)
          CALL uninterpreg(1,kmsk,buffo,buffi,global_lats_a,lonsperlat,
       & buff_mult_pieceg(1,1,2+5*levs+k) )
         enddo

Same procedure should be done with 2D arrays.

### How to add new variable to sfcf or flxf file:

1. Go to `~/src/atmos/gfs/phys` directory

2. Search the new variable,

    a. if it is in a module, add that module in subroutine
    POINT_PHYSICS_OUTPUT_GFS in gfs_physics_output.f

    b. if it is not in any module, and if the new variable is a scalar
    or a 1D array, add it in physics internal state, so it can be
    passed into POINT_PHYSICS_OUTPUT

    c. if it is not in any module, and the new variable is 2D sfc or
    flx field, in gfs_physics_sfc_flx_mod.f.

      1. for sfc field,add the new variable in data type
      Sfc_Var_Data,
        
      2. for flx field, add the new variable in Flx_Var_Data allocate
      the new field in gfs_physics_sfc_flx_set_mod.f,if flx field,
      initalize the field in subroutine flx_init

3. In gfs_physics_output.f, add the variable name to the corresponding
output list. 'OGFS_FLX' in the list is for flx file, 'OGFS_SFC' is for
sfc file, 'OGFS_PHY' is for both files. The field name could be
`"field_name"//"_"//"statistics property"`. SET THE Pointer in
subroutine POINT_PHYSICS_OUTPUT_GFS for the new variable.

4. If the new variable is a 2D or 3D, sfc or flx array:

    a. increase dimension of sfc or flx output file buffer. In
    gfs_physics_initialize_mod.f, increase ngrids_sfcc(total number of
    sfc fields), or ngrids_sfc2d(total number of 2D sfc fields),or
    ngrids_sfc3d(total 2D fields for all 3D sfc fields),or
    ngrids_flx(total 2D flx fields)

    b. for sfc field, in subroutine sfc_collect in wrtout_physics.f,
    interpolate the field to full grid field, put the output full grd
    field in buff_mult_piecea2d for 2D array, and put into
    buff_mult_piecea3d for 3D array.

    c. for flx field, in subroutine wrtflx_a in wrtout_physics.f,
    interpolate the field to full grid field, put the output full grd
    field in buff_mult_piecef.

With these changes, a field (2d or 3d array) or an attribute(1d
int, real, or log) will be added into the sfc or flx field bundle in import
write state, and it will then be written out in write grid component.

Eg, to add sunshine durationtime (sunsd) in flxf file:

1. in gfs_physics_sfc_flx_mod.f, in TYPE Flx_Var_Data, add:

        real(kind=kind_phys),pointer:: suntim(:,:)=>null()

   allocate this array in gfs_physics_sfc_flx_set_mod.f, in allication
   of xlf field, add:

        flx_fld%suntim  (dim1,dim2), &

2. in gfs_physics_output.f, in array PHY_INT_STATE_2D_R_FLX,add:

        ,'sunsd_acc       ', 'OGFS_FLX        ', 'sfc             ' &

3. add new field in output buffer buff_mult_piecef:

    a. change dimension of buff_mult_piecef in gfs_physics_initialize_mod.f:

            ngrids_flx  = 66+43+6

    b. in wrtflx_a in wrtout_physics.f, interpolate the suntim to full
    grid fields and save it in buff_mult_piecef

            !
            !    accumulated sunshine time
            !
                  glolal  = flx_fld%suntim
                  ngrid2d = ngrid2d+1
                  CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar,
            &     buff_mult_piecef(1,1,ngrid2d))
            !     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
            !    x '107)Accumulated sunshine duration (sec)'
            !
            !    end sunshine time

