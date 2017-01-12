from netCDF4 import Dataset
nc = Dataset('fv3_zeroincrement.nc','a')
for varname in nc.variables:
    if varname[-3:] == 'inc':
       print varname
       var = nc[varname]
       var[:] = 0
nc.close()
