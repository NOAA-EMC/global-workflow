# Global workflow comparison tools
A collection of tools to compare two different global workflow experiments for bitwise identicality. 

## Disclaimer

These tools are still a work-in-progress. Use at your own risk. There is no guarantee every relevant file will be compared (but feel free to make a pull request adding more).

# Usage

## Quick start
### To compare two UFS run directories
```
./diff_UFS_rundir.sh dirA dirB
```
Where `dirA` and `dirB` are the two UFS run directories.


### To compare two ROTDIRs
```
./diff_ROTDIR.sh dirA dirB
```
Where `dirA` and `dirB` are the two cycle directories (`.../gfs.YYYYMMDD/HH/`)

OR

```
./diff_ROTDIR.sh comroot cdate expA expB
```

Where:
- `comroot` is the root of your rotdirs (the portion of path the experiments share)
- `cdate` is the datetime of the cycle in YYYMMDDHH format
- `expA` and `expB` are the experiment names ($PSLOT) of each experiment

## Description

There are currently two tools included in this package:
* `diff_UFS_rundir.sh` will compare two UFS run directories (must have retained them by setting `KEEPDATA` to `NO` in config.base)
* `diff_ROTDIR.sh` will compare entire ROTDIRs

Both scripts work similarly. You will need two experiments to compare. Typically this means a "baseline" experiment using the current develop and whatever feature you are working on. Experiments need to be for the same cycle and use all the same settings, otherwise there is no chance of them matching. Except for specific text files, file lists are constructed by globbing the first experiment directory, so if the second experiment contains files that would otherwise be included, they will be skipped.

There are three classes of files compared:
- Text files, by simple posix diff
- GRiB2 files, using correaltion from `wgrib2`
- NetCDF files, using NetCDF Operators (nco)

Text and grib2 files are processed first and complete quickly. NetCDF processing is currently a lot slower.

Any variables listed in the coordinates.lst file will be ignored when comparing NetCDFs. This is because coordinate variables are not differenced, so when iterating through the variables of the difference they will be non-zero.

## Output

Output will appear like this:
```
=== <filename> ===
<comparison info>

```

For text files, it will be the ouput of posix diff, which is just an empty string when identical:
```
...

=== field_table ===


=== input.nml ===
310,313c310,313
<   FNGLAC   = '/scratch2/NCEPDEV/ensemble/save/Walter.Kolczynski/global-workflow/develop/fix/fix_am/global_glacier.2x2.grb'
<   FNMXIC   = '/scratch2/NCEPDEV/ensemble/save/Walter.Kolczynski/global-workflow/develop/fix/fix_am/global_maxice.2x2.grb'
<   FNTSFC   = '/scratch2/NCEPDEV/ensemble/save/Walter.Kolczynski/global-workflow/develop/fix/fix_am/RTGSST.1982.2012.monthly.clim.grb'
<   FNSNOC   = '/scratch2/NCEPDEV/ensemble/save/Walter.Kolczynski/global-workflow/develop/fix/fix_am/global_snoclim.1.875.grb'
---
>   FNGLAC   = '/scratch2/NCEPDEV/ensemble/save/Walter.Kolczynski/global-workflow/add_preamble/fix/fix_am/global_glacier.2x2.grb'
>   FNMXIC   = '/scratch2/NCEPDEV/ensemble/save/Walter.Kolczynski/global-workflow/add_preamble/fix/fix_am/global_maxice.2x2.grb'
>   FNTSFC   = '/scratch2/NCEPDEV/ensemble/save/Walter.Kolczynski/global-workflow/add_preamble/fix/fix_am/RTGSST.1982.2012.monthly.clim.grb'
>   FNSNOC   = '/scratch2/NCEPDEV/ensemble/save/Walter.Kolczynski/global-workflow/add_preamble/fix/fix_am/global_snoclim.1.875.grb'

...
```
(Text diffs have two extra blank line to separate the output.)

Grib files will look like this if they are identical:
```
=== GFSFLX.GrbF00 ===
All fields are identical!
=== GFSFLX.GrbF03 ===
All fields are identical!
=== GFSFLX.GrbF06 ===
All fields are identical!
=== GFSFLX.GrbF09 ===
All fields are identical!
=== GFSFLX.GrbF12 ===
All fields are identical!

...

```

And NetCDFs will look like this:
```
=== atmf000.nc ===
0 differences found
=== atmf003.nc ===
0 differences found
=== atmf006.nc ===
0 differences found
=== atmf009.nc ===
0 differences found

...
```

If any variables in a grib or NetCDF do not match, they will be listed instead.
