#!/usr/bin/env python
# calcanl_gcafs.py
# script to run executables to produce netCDF analysis
# on GCAFS gaussian grid for downstream users
# based on calcanl_gfs.py
import os
import datetime
from wxflow import FileHandler
from netCDF4 import Dataset, num2date
import numpy as np

python2fortran_bool = {True: '.true.', False: '.false.'}


# function to calculate analysis from two increment files and background
def calcanl_gcafs(RunDir, ComOut, APrefix):
    print('calcanl_gcafs beginning at: ', datetime.datetime.utcnow())
    # add aerosol increments to background aerosol fields
    # add meteorological increments to background meteorological fields

    # analysis increment is already assumed to be
    # at the GCAFS C384 equivalent Gaussian resolution

    # define path variables
    inc_file = os.path.join(RunDir, 'siginc.nc')
    anl_file = os.path.join(RunDir, 'siganl')
    ges_file = os.path.join(RunDir, 'sigf06')

    # add meteorological increments to background meteorological fields
    metvars = [['spfh', 'sphum'],
               ['tmp', 'T'],
               ['ugrd', 'u'],
               ['vgrd', 'v'],
               ['dpres', 'delp'],
               ['delz', 'delz'],
               ['o3mr', 'o3mr'],
               ['clwmr', 'liq_wat']]

    with Dataset(inc_file, mode='r') as incfile, Dataset(ges_file, mode='r') as gesfile, Dataset(anl_file, mode='a') as anlfile:
        # loop over meteorological variables and add increments to background
        for ioname, incname in metvars:
            print(f"Adding increment to background for variable: {ioname}")
            bkg = gesfile.variables[ioname][:]
            increment = incfile.variables[incname + '_inc'][:]
            anl = bkg + np.flip(increment, axis=1)

            anlfile.variables[ioname][:] = anl[:]

        # handle pressfc as a special case
        print("Adding increment to background for variable: pressfc")
        # read bk attribute and compute ps_inc from delp_inc
        bk = gesfile.ncattrs()
        if 'bk' in bk:
            bk = gesfile.getncattr('bk')
        else:
            # try to find bk as a variable if not an attribute
            bk = gesfile.variables['bk'][:]

        pressfc = gesfile.variables['pressfc'][:]
        delp_inc = incfile.variables['delp_inc'][:]

        # compute surface pressure increment
        ps_inc = delp_inc[-1] / (bk[-1] - bk[-2])

        # add increment to background surface pressure
        pressfc_anl = pressfc + np.flip(ps_inc, axis=0)
        anlfile.variables['pressfc'][:] = pressfc_anl[:]

    # add aerosol increments to background aerosol fields
    aerovars = [['so4', 'mass_fraction_of_sulfate_in_air'],
                ['bc1', 'mass_fraction_of_hydrophobic_black_carbon_in_air'],
                ['bc2', 'mass_fraction_of_hydrophilic_black_carbon_in_air'],
                ['oc1', 'mass_fraction_of_hydrophobic_organic_carbon_in_air'],
                ['oc2', 'mass_fraction_of_hydrophilic_organic_carbon_in_air'],
                ['dust1', 'mass_fraction_of_dust001_in_air'],
                ['dust2', 'mass_fraction_of_dust002_in_air'],
                ['dust3', 'mass_fraction_of_dust003_in_air'],
                ['dust4', 'mass_fraction_of_dust004_in_air'],
                ['dust5', 'mass_fraction_of_dust005_in_air'],
                ['seas1', ''],                                   # no seas1 increment
                ['seas2', 'mass_fraction_of_sea_salt001_in_air'],
                ['seas3', 'mass_fraction_of_sea_salt002_in_air'],
                ['seas4', 'mass_fraction_of_sea_salt003_in_air'],
                ['seas5', 'mass_fraction_of_sea_salt004_in_air']
                ]

    inc_file = os.path.join(RunDir, 'aeroinc.nc')
    with Dataset(inc_file, mode='r') as incfile, Dataset(ges_file, mode='r') as gesfile, Dataset(anl_file, mode='a') as anlfile:
        for ioname, incname in aerovars:
            print(f"Adding increment to background for variable: {ioname}")
            bkg = gesfile.variables[ioname][:]
            # no seas1 increment
            if ioname == 'seas1':
                anl = bkg
            else:
                increment = incfile.variables[incname][:]
                # reordering the dimensions of increment (latitude, longitude, levels) to macth background (time, levs, lat, lon)
                increment_reshape = np.transpose(increment, (2, 0, 1))
                anl = bkg + increment_reshape[np.newaxis, :, :, :]

            anlfile.variables[ioname][:] = anl[:]
        # update time (from 6 to 0) and time units in anlfile so UPP can create anl variables
        time = gesfile.variables['time']
        time_val = time[:]
        time_units = time.units
        time_calendar = getattr(time, "calendar", "standard")
        cycle_time = num2date(time_val, units=time_units, calendar=time_calendar)
        time_units_new = f"hours since {cycle_time[0]}"
        anlfile.variables['time'][:] = 0.0
        anlfile.variables['time'].setncattr("units", time_units_new)

    print('calcanl_gcafs successfully completed at: ', datetime.datetime.utcnow())


# run the function if this script is called from the command line
if __name__ == '__main__':
    ComOut = os.getenv('COMOUT_ATMOS_ANALYSIS', './')
    APrefix = os.getenv('APREFIX', '')
    RunDir = os.getenv('DATA', './')

    calcanl_gcafs(RunDir, ComOut, APrefix)
