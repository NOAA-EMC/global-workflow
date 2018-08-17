# Create MPMD jobs
import os
#
def write_poejob(poedir, nc, type, var, var_levels):
    poejob_filename = os.path.join(poedir,'poejob'+str(nc)+'.sh')
    if os.path.exists(poejob_filename):
        os.remove(poejob_filename)
    poejob_file = open(poejob_filename, 'w')
    #write environment variables
    for key in os.environ.keys():
        if key != 'A__z':
            poejob_file.write('export '+key+'="'+os.environ[key]+'"\n')
    poejob_file.write('export poejobnum="'+str(nc)+'"\n')
    poejob_file.write('export type="'+type+'"\n')
    poejob_file.write('export VAR="'+var+'"\n')
    if type == 'sfc':
       var_level, var_grblvl = var_levels.split(';')
       poejob_file.write('export VAR_LEVELS="'+var_level+'"\n')
       poejob_file.write('export VAR_GRBLVL_NUM="'+var_grblvl+'"\n')
    else:
       poejob_file.write('export VAR_LEVELS="'+var_levels+'"\n')
    #write METplus command
    metplushome = os.environ.get('metplushome')
    metplusconfig = os.environ.get('metplusconfig')
    metplusver = os.environ.get('METPLUSver')
    machine = os.environ.get('machine')
    rundir_g2g1 = os.environ.get('rundir_g2g1')
    exp = os.environ.get('exp')
    VDATE = os.environ.get('VDATE')
    if type == 'pres':
        poejob_file.write(metplushome+'/ush/master_metplus.py -c '+metplusconfig+'/metplus_config/METplus-'+metplusver+'/mpmd_confs/grid2grid_pres_step1a.conf -c '+metplusconfig+'/machine_config/machine.'+machine+'\n')
    elif type == 'anom':
        if var == 'HGT':
            poejob_file.write(metplushome+'/ush/master_metplus.py -c '+metplusconfig+'/metplus_config/METplus-'+metplusver+'/mpmd_confs/grid2grid_anom_step1b.conf -c '+metplusconfig+'/machine_config/machine.'+machine+'\n')
        else:
            poejob_file.write(metplushome+'/ush/master_metplus.py -c '+metplusconfig+'/metplus_config/METplus-'+metplusver+'/mpmd_confs/grid2grid_anom_step1a.conf -c '+metplusconfig+'/machine_config/machine.'+machine+'\n')
    elif type == 'sfc':
        poejob_file.write(metplushome+'/ush/master_metplus.py -c '+metplusconfig+'/metplus_config/METplus-'+metplusver+'/mpmd_confs/grid2grid_sfc_step1a.conf -c '+metplusconfig+'/machine_config/machine.'+machine+'\n')
    poejob_file.write('mv '+rundir_g2g1+'/make_met_data/'+type+'/'+exp+'/poejob'+str(nc)+'/'+VDATE+'00/grid_stat/* '+rundir_g2g1+'/make_met_data/'+type+'/'+exp+'/'+VDATE+'00/grid_stat/. \n')
    poejob_file.write('rm -r '+rundir_g2g1+'/make_met_data/'+type+'/'+exp+'/poejob'+str(nc))
#
type_list = os.environ.get('typelist').split(' ')
poedir = os.environ.get('poedir')
#make sure starting with a clean directory
if os.path.exists(poedir):
    for root, dirs, files in os.walk(poedir):
        for file in files:
            os.remove(os.path.join(poedir, file))
os.rmdir(poedir)
os.makedirs(poedir)
nc = 0
for type in type_list:
    print(type)
    if type == 'pres':
        var_list = [ 'HGT', 'UGRD', 'VGRD', 'TMP', 'O3MR' ]
        for var in var_list:
            if var == 'O3MR':
                var_levels = 'P100, P70, P50, P30, P20, P10'
            else:    
                var_levels = 'P1000, P925, P850, P700, P500, P400, P300, P250, P200, P150, P100, P50, P20, P10' 
            nc+=1
            print(str(nc)+' '+var)
            write_poejob(poedir, nc, type, var, var_levels)
    elif type == 'anom':
        var_list = [ 'HGT', 'UGRD', 'VGRD', 'TMP', 'PRMSL' ]
        for var in var_list:
            if var[0] == 'H':
                var_levels = 'P1000, P700, P500, P250'
            elif var[0] == 'P':
                var_levels = 'Z0'
            else:
                var_levels = 'P850, P500, P250'
            nc+=1
            print(str(nc)+' '+var)
            write_poejob(poedir, nc, type, var, var_levels)
    elif type == 'sfc':
        var_list = [ 'TMP:Z2;105', 'RH:Z2;105', 'SPFH:Z2;105', 'HPBL:L0;01', 'PRES:Z0;01', 'PRMSL:Z0;102', 'TMP:Z0;01', 'UGRD:Z10;105', 'VGRD:Z10;105', 'TSOIL:Z0-10;112', 'SOILW:Z0-10;112', 'WEASD:Z0;01', 'CAPE:Z0;01', 'CWAT:L0;200', 'PWAT:L0;200', 'TMP:L0;07', 'HGT:L0;07', 'TOZNE:L0;200' ]
        for var_info in var_list:
            nc+=1
            print(str(nc)+' '+var_info)
            var, var_level_info = var_info.split(':')
            write_poejob(poedir, nc, type, var, var_level_info)
print(' ')
print('NC COUNT: '+str(nc))
