# Create MPMD jobs
import os
#
def write_poejob_pb2nc(poedir, nc, type, vhr):
    poejob_filename = os.path.join(poedir,'poejob'+str(nc)+'.sh')
    if os.path.exists(poejob_filename):
        os.remove(poejob_filename)
    poejob_file = open(poejob_filename, 'w')
    #write environment variables
    for key in os.environ.keys():
        if key != 'A__z' and key != 'BASH_FUNC_module%%':
            poejob_file.write('export '+key+'="'+os.environ[key]+'"\n')
    poejob_file.write('export poejobnum="'+str(nc)+'"\n')
    poejob_file.write('export vhr="'+str(vhr)+'"\n')
    #write METplus command
    metplushome = os.environ.get('metplushome')
    metplusconfig = os.environ.get('metplusconfig')
    metplusver = os.environ.get('METPLUSver')
    machine = os.environ.get('machine')
    poejob_file.write(metplushome+'/ush/master_metplus.py -c '+metplusconfig+'/metplus_config/METplus-'+metplusver+'/mpmd_confs/grid2obs_'+type+'_step1a.conf -c '+metplusconfig+'/machine_config/machine.'+machine+'\n')
#
def write_poejob_pointstat(poedir, nc, type, vhr, var, var_levels):
    poejob_filename = os.path.join(poedir,'poejob'+str(nc)+'.sh')
    if os.path.exists(poejob_filename):
        os.remove(poejob_filename)
    poejob_file = open(poejob_filename, 'w')
    #write environment variables
    for key in os.environ.keys():
        if key != 'A__z'  and key != 'BASH_FUNC_module%%':
            poejob_file.write('export '+key+'="'+os.environ[key]+'"\n')
    poejob_file.write('export poejobnum="'+str(nc)+'"\n')
    poejob_file.write('export vhr_start="'+str(vhr)+'"\n')
    poejob_file.write('export vhr_end="'+str(int(vhr)+1).zfill(2)+'"\n')
    poejob_file.write('export VAR="'+var+'"\n')
    #write METplus command
    metplushome = os.environ.get('metplushome')
    metplusconfig = os.environ.get('metplusconfig')
    metplusver = os.environ.get('METPLUSver')
    machine = os.environ.get('machine')
    if type == 'upper_air':
        poejob_file.write('export VAR_LEVELS="'+var_levels+'"\n')
        poejob_file.write(metplushome+'/ush/master_metplus.py -c '+metplusconfig+'/metplus_config/METplus-'+metplusver+'/mpmd_confs/grid2obs_'+type+'_step1b.conf -c '+metplusconfig+'/machine_config/machine.'+machine+'\n')
        poejob_file.write('rename ${point_stat_upper_air_dir}/poejob${poejobnum}/point_stat ${point_stat_upper_air_dir}/poejob${poejobnum}/point_stat_${VAR} ${point_stat_upper_air_dir}/poejob${poejobnum}/point_stat*'+'\n')
        poejob_file.write('mv ${point_stat_upper_air_dir}/poejob${poejobnum}/* ${point_stat_upper_air_dir}/.'+'\n')
        poejob_file.write('rm -r ${point_stat_upper_air_dir}/poejob${poejobnum}') 
    elif type == 'conus_sfc': 
        var_level, var_grblvl = var_levels.split(';')
        poejob_file.write('export VAR_LEVELS="'+var_level+'"\n')
        poejob_file.write('export VAR_GRBLVL_NUM_DESC="GRIB_lvl_typ = '+var_grblvl+'"\n')
        poejob_file.write(metplushome+'/ush/master_metplus.py -c '+metplusconfig+'/metplus_config/METplus-'+metplusver+'/mpmd_confs/grid2obs_'+type+'_step1b.conf -c '+metplusconfig+'/machine_config/machine.'+machine+'\n')
        poejob_file.write('rename ${point_stat_conus_sfc_dir}/poejob${poejobnum}/point_stat ${point_stat_conus_sfc_dir}/poejob${poejobnum}/point_stat_${VAR}${VAR_LEVELS} ${point_stat_conus_sfc_dir}/poejob${poejobnum}/point_stat*'+'\n')
        poejob_file.write('mv ${point_stat_conus_sfc_dir}/poejob${poejobnum}/* ${point_stat_conus_sfc_dir}/.'+'\n')
        poejob_file.write('rm -r ${point_stat_conus_sfc_dir}/poejob${poejobnum}')
#
poedir = os.environ.get('poedir')
g2o_process = os.environ.get('g2o_process')
type_list = os.environ.get('typelist').split(' ')
#
if g2o_process == 'pb2nc':
     print('Creating pb2nc poejobs')
     poedir_pb2nc = os.path.join(poedir,g2o_process)
     #make sure starting with a clean directory
     if os.path.exists(poedir_pb2nc):
         for root, dirs, files in os.walk(poedir_pb2nc):
             for file in files:
                 os.remove(os.path.join(poedir_pb2nc, file))
         os.rmdir(poedir_pb2nc)
     os.makedirs(poedir_pb2nc)
     nc = 0
     for type in type_list:
         print(type)
         if type == 'upper_air':
             vhr_upper_air_start = int(os.environ.get('vhr_upper_air_start'))
             vhr_upper_air_end = int(os.environ.get('vhr_upper_air_end'))
             vhr_upper_air_inc = int(os.environ.get('vhr_upper_air_inc'))
             vhr_upper_air_now = vhr_upper_air_start
             while vhr_upper_air_now <= vhr_upper_air_end:
                 nc+=1
                 vhr_upper_air = str(vhr_upper_air_now).zfill(2)
                 print(str(nc)+' '+vhr_upper_air+'Z')
                 write_poejob_pb2nc(poedir_pb2nc, nc, type, vhr_upper_air)
                 vhr_upper_air_now+=vhr_upper_air_inc
         elif type == 'conus_sfc': 
             vhr_conus_sfc_start = int(os.environ.get('vhr_conus_sfc_start'))
             vhr_conus_sfc_end = int(os.environ.get('vhr_conus_sfc_end'))
             vhr_conus_sfc_inc = int(os.environ.get('vhr_conus_sfc_inc'))
             vhr_conus_sfc_now = vhr_conus_sfc_start
             while vhr_conus_sfc_now <= vhr_conus_sfc_end:
                 nc+=1
                 vhr_conus_sfc = str(vhr_conus_sfc_now).zfill(2)
                 print(str(nc)+' '+vhr_conus_sfc+'Z')
                 write_poejob_pb2nc(poedir_pb2nc, nc, type, vhr_conus_sfc)
                 vhr_conus_sfc_now+=vhr_conus_sfc_inc
elif g2o_process == 'point_stat':
     print('Creating point_stat poejobs')
     poedir_pointstat = os.path.join(poedir,g2o_process)
     #make sure starting with a clean directory
     if os.path.exists(poedir_pointstat):
         for root, dirs, files in os.walk(poedir_pointstat):
             for file in files:
                 os.remove(os.path.join(poedir_pointstat, file))
         os.rmdir(poedir_pointstat)
     os.makedirs(poedir_pointstat)
     nc = 0
     for type in type_list:
         if type == 'upper_air':
             var_list = [ 'TMP', 'RH', 'UGRD', 'VGRD', 'HGT']
             for var in var_list:
                 if var == 'RH':
                    var_levels = 'P1000, P925, P850, P700, P500, P400, P300'
                 else:
                    var_levels = 'P1000, P925, P850, P700, P500, P400, P300, P250, P200, P150, P100, P50'
                 vhr_upper_air_start = int(os.environ.get('vhr_upper_air_start'))
                 vhr_upper_air_end = int(os.environ.get('vhr_upper_air_end'))
                 vhr_upper_air_inc = int(os.environ.get('vhr_upper_air_inc'))
                 vhr_upper_air_now = vhr_upper_air_start
                 while vhr_upper_air_now <= vhr_upper_air_end:
                    nc+=1
                    vhr_upper_air = str(vhr_upper_air_now).zfill(2)
                    print(str(nc)+' '+var+' '+vhr_upper_air+'Z')
                    write_poejob_pointstat(poedir_pointstat, nc, type, vhr_upper_air, var, var_levels)
                    vhr_upper_air_now+=vhr_upper_air_inc
         elif type == 'conus_sfc':
             var_list = [ 'TMP:Z2;105', 'RH:Z2;105', 'DPT:Z2;105', 'UGRD:Z10;105', 'VGRD:Z10;105', 'TCDC:L0;200', 'PRMSL:Z0;102' ]
             for var_info in var_list:
                 var, var_level_info = var_info.split(':')
                 vhr_conus_sfc_start = int(os.environ.get('vhr_conus_sfc_start'))
                 vhr_conus_sfc_end = int(os.environ.get('vhr_conus_sfc_end'))
                 vhr_conus_sfc_inc = int(os.environ.get('vhr_conus_sfc_inc'))
                 vhr_conus_sfc_now = vhr_conus_sfc_start
                 while vhr_conus_sfc_now <= vhr_conus_sfc_end:
                     nc+=1
                     vhr_conus_sfc = str(vhr_conus_sfc_now).zfill(2)
                     print(str(nc)+' '+var_info+' '+vhr_conus_sfc+'Z')
                     write_poejob_pointstat(poedir_pointstat, nc, type, vhr_conus_sfc, var, var_level_info)
                     vhr_conus_sfc_now+=vhr_conus_sfc_inc
