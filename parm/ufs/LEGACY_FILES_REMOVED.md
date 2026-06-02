# Legacy Static File Removal Record

This document records the legacy static configuration file variants that were removed
as part of the templated-model-configs migration (Requirement 6.3).

Each category of files has been replaced by a single parameterized Jinja2 template
located under `dev/parm/ufs/`.

## field_table_* → dev/parm/ufs/fv3/field_table.j2

The following 18 static field_table variants were deleted:

- `parm/ufs/fv3/field_table_gfdl`
- `parm/ufs/fv3/field_table_gfdl_progsigma`
- `parm/ufs/fv3/field_table_gfdl_satmedmf`
- `parm/ufs/fv3/field_table_gfdl_satmedmf_progsigma`
- `parm/ufs/fv3/field_table_thompson`
- `parm/ufs/fv3/field_table_thompson_aero_tke`
- `parm/ufs/fv3/field_table_thompson_aero_tke_progsigma`
- `parm/ufs/fv3/field_table_thompson_noaero_tke`
- `parm/ufs/fv3/field_table_thompson_noaero_tke_progsigma`
- `parm/ufs/fv3/field_table_thompson_satmedmf`
- `parm/ufs/fv3/field_table_wsm6`
- `parm/ufs/fv3/field_table_wsm6_progsigma`
- `parm/ufs/fv3/field_table_wsm6_satmedmf`
- `parm/ufs/fv3/field_table_wsm6_satmedmf_progsigma`
- `parm/ufs/fv3/field_table_zhaocarr`
- `parm/ufs/fv3/field_table_zhaocarr_progsigma`
- `parm/ufs/fv3/field_table_zhaocarr_satmedmf`
- `parm/ufs/fv3/field_table_zhaocarr_satmedmf_progsigma`

## ufs.configure.*.IN → dev/parm/ufs/ufs.configure.j2

No `ufs.configure.*.IN` files were present in this branch (already removed or
never existed in this development branch). The replacement template is in place.

## ExtData.* → dev/parm/ufs/gocart/ExtData.j2

The following 4 static ExtData variants were deleted:

- `parm/ufs/gocart/ExtData.qfed`
- `parm/ufs/gocart/ExtData.gbbepx`
- `parm/ufs/gocart/ExtData.none`
- `parm/ufs/gocart/ExtData.other`

## diag_table variants → dev/parm/ufs/fv3/diag_table.j2

The following 4 static diag_table variants were deleted:

- `parm/ufs/fv3/diag_table_aod`
- `parm/ufs/fv3/diag_table_da`
- `parm/ufs/fv3/diag_table_da_orig`
- `parm/ufs/fv3/diag_table.aero`

## Coupled-Model `.IN` Files → dev/parm/ufs/{ocean,ice,wave,post}/*.j2

The following coupled-model template files were removed from the `ufs_templates`
array in `sorc/link_workflow.sh` and are no longer linked from
`sorc/ufs_model.fd/tests/parm/`. They are replaced by Jinja2 templates rendered
at deployment time (coupled-model-configs spec, Requirements 11.7, 14.1, 14.2, 14.3).

- `parm/ufs/MOM_input_025.IN` → `dev/parm/ufs/ocean/MOM_input.j2`
- `parm/ufs/MOM_input_050.IN` → `dev/parm/ufs/ocean/MOM_input.j2`
- `parm/ufs/MOM_input_100.IN` → `dev/parm/ufs/ocean/MOM_input.j2`
- `parm/ufs/MOM_input_500.IN` → `dev/parm/ufs/ocean/MOM_input.j2`
- `parm/ufs/MOM6_data_table.IN` → `dev/parm/ufs/ocean/MOM6_data_table.j2`
- `parm/ufs/ice_in.IN` → `dev/parm/ufs/ice/ice_in.j2`
- `parm/ufs/ww3_shel.nml.IN` → `dev/parm/ufs/wave/ww3_shel.nml.j2`
- `parm/ufs/input_global_nest.nml.IN` → `dev/parm/ufs/fv3/input_global_nest.nml.j2`
- `parm/ufs/post_itag_gfs` → `dev/parm/ufs/post/post_itag.j2`
- `parm/ufs/post_itag_gcafs` → `dev/parm/ufs/post/post_itag.j2`
