#!/usr/bin/env python
# calcanl_gfs.py
# cory.r.martin@noaa.gov
# 2019-10-11
# script to run executables to produce netCDF analysis
# on GFS gaussian grid for downstream users
import os
import shutil
import subprocess
import sys
import gsi_utils
from collections import OrderedDict
import datetime

# function to calculate analysis from a given increment file and background
def calcanl_gfs(DoIAU, l4DEnsVar, Write4Danl, ComOut, APrefix, ASuffix, 
                FixDir, atmges_ens_mean, RunDir, NThreads, NEMSGet, IAUHrs, 
                ExecCMD, ExecCMDMPI, ExecAnl, ExecChgresGes, ExecChgresInc, Cdump):
  print('calcanl_gfs beginning at: ',datetime.datetime.utcnow())

  IAUHH = IAUHrs
  if Cdump == "gfs":
    IAUHH = list(map(int,'6'))
  else:
    IAUHH = IAUHrs
  
  ######## copy and link files
  if DoIAU and l4DEnsVar and Write4Danl:
    for fh in IAUHH:
      if fh == 6:
        # for full res analysis
        CalcAnlDir = RunDir+'/calcanl_'+format(fh, '02')
        if not os.path.exists(CalcAnlDir):
          os.makedirs(CalcAnlDir)
        shutil.copy(ExecAnl, CalcAnlDir+'/calc_anl.x')
        gsi_utils.link_file(RunDir+'/siginc.nc', CalcAnlDir+'/siginc.nc.06')
        gsi_utils.link_file(RunDir+'/sigf06', CalcAnlDir+'/ges.06')
        gsi_utils.link_file(RunDir+'/siganl', CalcAnlDir+'/anl.06')
        shutil.copy(ExecChgresInc, CalcAnlDir+'/chgres_inc.x')
        # for ensemble res analysis
        CalcAnlDir = RunDir+'/calcanl_ensres_'+format(fh, '02')
        if not os.path.exists(CalcAnlDir):
          os.makedirs(CalcAnlDir)
        shutil.copy(ExecAnl, CalcAnlDir+'/calc_anl.x')
        gsi_utils.link_file(RunDir+'/siginc.nc', CalcAnlDir+'/siginc.nc.06')
        gsi_utils.link_file(ComOut+'/'+APrefix+'atmanl.ensres'+ASuffix, CalcAnlDir+'/anl.ensres.06')
        gsi_utils.link_file(RunDir+'/sigf06', CalcAnlDir+'/ges.06')
        shutil.copy(ExecChgresGes, CalcAnlDir+'/chgres_ges.x')
        
      else:
        if os.path.isfile('sigi'+format(fh, '02')+'.nc'):
          # for full res analysis
          CalcAnlDir = RunDir+'/calcanl_'+format(fh, '02')
          CalcAnlDir6 = RunDir+'/calcanl_'+format(6, '02')
          if not os.path.exists(CalcAnlDir):
            os.makedirs(CalcAnlDir)
          if not os.path.exists(CalcAnlDir6):
            os.makedirs(CalcAnlDir6)
          gsi_utils.link_file(ComOut+'/'+APrefix+'atma'+format(fh, '03')+ASuffix, CalcAnlDir6+'/anl.'+format(fh, '02'))
          gsi_utils.link_file(RunDir+'/siga'+format(fh, '02'), CalcAnlDir6+'/anl.'+format(fh, '02'))
          gsi_utils.link_file(RunDir+'/sigi'+format(fh, '02')+'.nc', CalcAnlDir+'/siginc.nc.'+format(fh, '02'))
          gsi_utils.link_file(CalcAnlDir6+'/inc.fullres.'+format(fh, '02'),CalcAnlDir+'/inc.fullres.'+format(fh, '02'))
          gsi_utils.link_file(RunDir+'/sigf'+format(fh, '02'), CalcAnlDir6+'/ges.'+format(fh, '02'))
          shutil.copy(ExecChgresInc, CalcAnlDir+'/chgres_inc.x')
          # for ensemble res analysis
          CalcAnlDir = RunDir+'/calcanl_ensres_'+format(fh, '02')
          CalcAnlDir6 = RunDir+'/calcanl_ensres_'+format(6, '02')
          if not os.path.exists(CalcAnlDir):
            os.makedirs(CalcAnlDir)
          if not os.path.exists(CalcAnlDir6):
            os.makedirs(CalcAnlDir6)
          gsi_utils.link_file(ComOut+'/'+APrefix+'atma'+format(fh, '03')+'.ensres'+ASuffix, CalcAnlDir6+'/anl.ensres.'+format(fh, '02'))
          gsi_utils.link_file(RunDir+'/sigi'+format(fh, '02')+'.nc', CalcAnlDir6+'/siginc.nc.'+format(fh, '02'))
          gsi_utils.link_file(RunDir+'/sigf'+format(fh, '02'), CalcAnlDir+'/ges.'+format(fh, '02'))
          gsi_utils.link_file(CalcAnlDir6+'/ges.ensres.'+format(fh, '02'),CalcAnlDir+'/ges.ensres.'+format(fh, '02'))
          shutil.copy(ExecChgresGes, CalcAnlDir+'/chgres_ges.x')
          
            
  else:
    # for full res analysis
    CalcAnlDir = RunDir+'/calcanl_'+format(6, '02')
    if not os.path.exists(CalcAnlDir):
      os.makedirs(CalcAnlDir)
    shutil.copy(ExecAnl, CalcAnlDir+'/calc_anl.x')
    gsi_utils.link_file(RunDir+'/siginc.nc', CalcAnlDir+'/siginc.nc.06')
    gsi_utils.link_file(RunDir+'/sigf06', CalcAnlDir+'/ges.06')
    gsi_utils.link_file(RunDir+'/siganl', CalcAnlDir+'/anl.06')
    shutil.copy(ExecChgresInc, CalcAnlDir+'/chgres_inc.x')
    # for ensemble res analysis
    CalcAnlDir = RunDir+'/calcanl_ensres_'+format(6, '02')
    if not os.path.exists(CalcAnlDir):
      os.makedirs(CalcAnlDir)
    shutil.copy(ExecAnl, CalcAnlDir+'/calc_anl.x')
    gsi_utils.link_file(RunDir+'/siginc.nc', CalcAnlDir+'/siginc.nc.06')
    gsi_utils.link_file(ComOut+'/'+APrefix+'atmanl.ensres'+ASuffix, CalcAnlDir+'/anl.ensres.06')
    gsi_utils.link_file(RunDir+'/sigf06', CalcAnlDir+'/ges.06')
    shutil.copy(ExecChgresGes, CalcAnlDir+'/chgres_ges.x')

  # determine if the analysis is to be written in netCDF or NEMSIO
  if ASuffix == ".nc":
     nemsanl = ".false."
  else:
     nemsanl = ".true."

  ######## get dimension information from background and increment files
  AnlDims = gsi_utils.get_ncdims('siginc.nc')
  if ASuffix == ".nc":
    GesDims = gsi_utils.get_ncdims('sigf06') 
  else:
    GesDims = gsi_utils.get_nemsdims('sigf06',NEMSGet)

  levs = AnlDims['lev']
  LonA = AnlDims['lon']
  LatA = AnlDims['lat']
  LonB = GesDims['grid_xt']
  LatB = GesDims['grid_yt']

  # vertical coordinate info
  levs2 = levs + 1
  siglevel = FixDir+'/global_hyblev.l'+str(levs2)+'.txt'

  ####### determine how many forecast hours to process
  nFH=0
  for fh in IAUHH:
    # first check to see if increment file exists
    CalcAnlDir = RunDir+'/calcanl_'+format(fh, '02')
    if (os.path.isfile(CalcAnlDir+'/siginc.nc.'+format(fh, '02'))):
      print('will process increment file: '+CalcAnlDir+'/siginc.nc.'+format(fh, '02'))
      nFH+=1
    else:
      print('Increment file: '+CalcAnlDir+'/siginc.nc.'+format(fh, '02')+' does not exist. Skipping.')
  
  sys.stdout.flush()
  ######## need to gather information about runtime environment
  ExecCMD = ExecCMD.replace("$ncmd","1")
  os.environ['OMP_NUM_THREADS'] = str(NThreads)
  os.environ['ncmd'] = str(nFH)
  ExecCMDMPI1 = ExecCMDMPI.replace("$ncmd",str(1))
  ExecCMDMPI = ExecCMDMPI.replace("$ncmd",str(nFH))
  ExecCMDLevs = ExecCMDMPI.replace("$ncmd",str(levs))
  ExecCMDMPI10 = ExecCMDMPI.replace("$ncmd",str(10))

  # are we using mpirun with lsf, srun, or aprun with Cray?
  launcher = ExecCMDMPI.split(' ')[0]
  if launcher == 'mpirun':
    hostfile = os.getenv('LSB_DJOB_HOSTFILE','')
    with open(hostfile) as f:
      hosts_tmp = f.readlines()
    hosts_tmp = [x.strip() for x in hosts_tmp]
    hosts = []
    [hosts.append(x) for x in hosts_tmp if x not in hosts]
    nhosts = len(hosts)
    ExecCMDMPI_host = 'mpirun -np '+str(nFH)+' --hostfile hosts' 
    tasks = int(os.getenv('LSB_DJOB_NUMPROC',1))
    if levs > tasks:
      ExecCMDMPILevs_host = 'mpirun -np '+str(tasks)+' --hostfile hosts'
      ExecCMDMPILevs_nohost = 'mpirun -np '+str(tasks)
    else:
      ExecCMDMPILevs_host = 'mpirun -np '+str(levs)+' --hostfile hosts'
      ExecCMDMPILevs_nohost = 'mpirun -np '+str(levs)
    ExecCMDMPI1_host = 'mpirun -np 1 --hostfile hosts' 
    ExecCMDMPI10_host = 'mpirun -np 10 --hostfile hosts' 
  elif launcher == 'srun':
    nodes = os.getenv('SLURM_JOB_NODELIST','')
    hosts_tmp = subprocess.check_output('scontrol show hostnames '+nodes, shell=True) 
    if (sys.version_info > (3, 0)):
        hosts_tmp = hosts_tmp.decode('utf-8')
        hosts_tmp = str(hosts_tmp).splitlines()
        hosts_tmp = [x.strip() for x in hosts_tmp]
    else:
        hosts_tmp = hosts_tmp.strip()
        hosts_tmp = str(hosts_tmp).splitlines()
        hosts_tmp = [x.strip() for x in hosts_tmp]
    hosts = []
    [hosts.append(x) for x in hosts_tmp if x not in hosts]
    nhosts = len(hosts)
    ExecCMDMPI_host = 'srun -n '+str(nFH)+' --verbose --export=ALL -c 1 --distribution=arbitrary --cpu-bind=cores'
    # need to account for when fewer than LEVS tasks are available
    tasks = int(os.getenv('SLURM_NPROCS',1))
    if levs > tasks:
      ExecCMDMPILevs_host = 'srun -n '+str(tasks)+' --verbose --export=ALL -c 1 --distribution=arbitrary --cpu-bind=cores'
      ExecCMDMPILevs_nohost = 'srun -n '+str(tasks)+' --verbose --export=ALL'
    else:
      ExecCMDMPILevs_host = 'srun -n '+str(levs)+' --verbose --export=ALL -c 1 --distribution=arbitrary --cpu-bind=cores'
      ExecCMDMPILevs_nohost = 'srun -n '+str(levs)+' --verbose --export=ALL'
    ExecCMDMPI1_host = 'srun -n 1 --verbose --export=ALL -c 1 --distribution=arbitrary --cpu-bind=cores'
    ExecCMDMPI10_host = 'srun -n 10 --verbose --export=ALL -c 1 --distribution=arbitrary --cpu-bind=cores'
  elif launcher == 'aprun':
    hostfile = os.getenv('LSB_DJOB_HOSTFILE','')
    with open(hostfile) as f:
      hosts_tmp = f.readlines()
    hosts_tmp = [x.strip() for x in hosts_tmp]
    hosts = []
    [hosts.append(x) for x in hosts_tmp if x not in hosts]
    nhosts = len(hosts)
    ExecCMDMPI_host = 'aprun -l hosts -d '+str(NThreads)+' -n '+str(nFH)
    ExecCMDMPILevs_host = 'aprun -l hosts -d '+str(NThreads)+' -n '+str(levs)
    ExecCMDMPILevs_nohost = 'aprun -d '+str(NThreads)+' -n '+str(levs)
    ExecCMDMPI1_host = 'aprun -l hosts -d '+str(NThreads)+' -n 1'
    ExecCMDMPI10_host = 'aprun -l hosts -d '+str(NThreads)+' -n 10'
  else:
    print('unknown MPI launcher. Failure.')
    sys.exit(1)

  ####### generate the full resolution analysis
  interp_jobs = []
  ihost = 0
  ### interpolate increment to full background resolution
  for fh in IAUHH:
    # first check to see if increment file exists
    CalcAnlDir = RunDir+'/calcanl_'+format(fh, '02')
    if (os.path.isfile(CalcAnlDir+'/siginc.nc.'+format(fh, '02'))):
      # set up the namelist
      namelist = OrderedDict()
      namelist["setup"] = {"lon_out": LonB,
                           "lat_out": LatB,
                           "lev": levs,
                           "infile": "'siginc.nc."+format(fh, '02')+"'",
                           "outfile": "'inc.fullres."+format(fh, '02')+"'",
                         }
      gsi_utils.write_nml(namelist, CalcAnlDir+'/fort.43')

      if ihost >= nhosts:
        ihost = 0
      with open(CalcAnlDir+'/hosts', 'w') as hostfile:
          hostfile.write(hosts[ihost]+'\n')
          if launcher == 'srun': # need to write host per task not per node for slurm
            for a in range(0,9): # need 9 more of the same host for the 10 tasks for chgres_inc
              hostfile.write(hosts[ihost]+'\n')
      if launcher == 'srun':
        os.environ['SLURM_HOSTFILE'] = CalcAnlDir+'/hosts'
      print('interp_inc', fh, namelist)
      job = subprocess.Popen(ExecCMDMPI10_host+' '+CalcAnlDir+'/chgres_inc.x', shell=True, cwd=CalcAnlDir)
      interp_jobs.append(job)
      print(ExecCMDMPI10_host+' '+CalcAnlDir+'/chgres_inc.x submitted on '+hosts[ihost])
      ihost+=1

  sys.stdout.flush()
  exit_codes = [p.wait() for p in interp_jobs]
  for ec in exit_codes:
    if ec != 0:
      print('Error with chgres_inc.x, exit code='+str(ec))
      print(locals())
      sys.exit(ec)

  #### generate analysis from interpolated increment
  CalcAnlDir6 = RunDir+'/calcanl_'+format(6, '02')
  # set up the namelist
  namelist = OrderedDict()
  namelist["setup"] =  {"datapath": "'./'",
                        "analysis_filename": "'anl'",
                        "firstguess_filename": "'ges'",
                        "increment_filename": "'inc.fullres'",
                        "fhr": 6,
                       }
  
  gsi_utils.write_nml(namelist, CalcAnlDir6+'/calc_analysis.nml')

  # run the executable
  if ihost >= nhosts-1:
    ihost = 0
  if launcher == 'srun':
    del os.environ['SLURM_HOSTFILE']
  print('fullres_calc_anl', namelist)
  fullres_anl_job = subprocess.Popen(ExecCMDMPILevs_nohost+' '+CalcAnlDir6+'/calc_anl.x', shell=True, cwd=CalcAnlDir6)
  print(ExecCMDMPILevs_nohost+' '+CalcAnlDir6+'/calc_anl.x submitted')

  sys.stdout.flush()
  exit_fullres = fullres_anl_job.wait()
  sys.stdout.flush()
  if exit_fullres != 0:
    print('Error with calc_analysis.x for deterministic resolution, exit code='+str(exit_fullres))
    print(locals())
    sys.exit(exit_fullres)


  ######## run chgres to get background on ensemble resolution
  if Cdump == "gdas":
    chgres_jobs = []
    for fh in IAUHH:
      # first check to see if guess file exists
      CalcAnlDir = RunDir+'/calcanl_ensres_'+format(fh, '02')
      if (os.path.isfile(CalcAnlDir+'/ges.'+format(fh, '02'))):
        # set up the namelist
        namelist = OrderedDict()
        namelist["chgres_setup"] =  {"i_output": str(LonA),
                                     "j_output": str(LatA),
                                     "input_file": "'ges."+format(fh, '02')+"'",
                                     "output_file": "'ges.ensres."+format(fh, '02')+"'",
                                     "terrain_file": "'"+atmges_ens_mean+"'",
                                     "vcoord_file": "'"+siglevel+"'",
                                   }
      
        gsi_utils.write_nml(namelist, CalcAnlDir+'/chgres_nc_gauss.nml')
    
        # run the executable
        if ihost >= nhosts-1:
          ihost = 0
        with open(CalcAnlDir+'/hosts', 'w') as hostfile:
          hostfile.write(hosts[ihost]+'\n')
        if launcher == 'srun':
          os.environ['SLURM_HOSTFILE'] = CalcAnlDir+'/hosts'
        print('chgres_nc_gauss', fh, namelist)
        job = subprocess.Popen(ExecCMDMPI1_host+' '+CalcAnlDir+'/chgres_ges.x', shell=True, cwd=CalcAnlDir)
        chgres_jobs.append(job)
        print(ExecCMDMPI1_host+' '+CalcAnlDir+'/chgres_ges.x submitted on '+hosts[ihost])
        ihost+=1

    sys.stdout.flush()
    exit_codes = [p.wait() for p in chgres_jobs]
    for ec in exit_codes:
      if ec != 0:
        print('Error with chgres_ges.x, exit code='+str(ec))
        print(locals())
        sys.exit(ec)

    sys.stdout.flush()
    ######## generate ensres analysis from interpolated background
    if launcher == 'srun':
      del os.environ['SLURM_HOSTFILE']
    for fh in IAUHH:
      CalcAnlDir6 = RunDir+'/calcanl_ensres_'+format(6, '02')
      # set up the namelist
      namelist = OrderedDict()
      namelist["setup"] =  {"datapath": "'./'",
                            "analysis_filename": "'anl.ensres'",
                            "firstguess_filename": "'ges.ensres'",
                            "increment_filename": "'siginc.nc'",
                            "fhr": fh,
                          }

  
      gsi_utils.write_nml(namelist, CalcAnlDir6+'/calc_analysis.nml')

      # run the executable
      if ihost > nhosts-1:
        ihost = 0
      print('ensres_calc_anl', namelist)
      ensres_anl_job = subprocess.Popen(ExecCMDMPILevs_nohost+' '+CalcAnlDir6+'/calc_anl.x', shell=True, cwd=CalcAnlDir6)
      print(ExecCMDMPILevs_nohost+' '+CalcAnlDir6+'/calc_anl.x submitted')

      sys.stdout.flush()
      ####### check on analysis steps
      exit_ensres = ensres_anl_job.wait()
      if exit_ensres != 0:
        print('Error with calc_analysis.x for ensemble resolution, exit code='+str(exit_ensres))
        print(locals())
        sys.exit(exit_ensres)

  print('calcanl_gfs successfully completed at: ',datetime.datetime.utcnow())
  print(locals())

# run the function if this script is called from the command line
if __name__ == '__main__':
  DoIAU = gsi_utils.isTrue(os.getenv('DOIAU', 'NO'))
  l4DEnsVar = gsi_utils.isTrue(os.getenv('l4densvar', 'NO'))
  Write4Danl = gsi_utils.isTrue(os.getenv('lwrite4danl', 'NO'))
  ComOut = os.getenv('COMOUT', './')
  APrefix = os.getenv('APREFIX', '')
  ASuffix= os.getenv('ASUFFIX', '')
  NThreads = os.getenv('NTHREADS_CHGRES', 1)
  FixDir = os.getenv('FIXgsm', './')
  atmges_ens_mean = os.getenv('ATMGES_ENSMEAN', './atmges_ensmean')
  RunDir = os.getenv('DATA', './')
  ExecCMD = os.getenv('APRUN_CALCANL', '')
  ExecCMDMPI = os.getenv('APRUN_CALCINC', '')
  ExecAnl = os.getenv('CALCANLEXEC', './calc_analysis.x')
  ExecChgresGes = os.getenv('CHGRESNCEXEC', './chgres_nc_gauss.exe')
  ExecChgresInc = os.getenv('CHGRESINCEXEC', './chgres_increment.exe')
  NEMSGet = os.getenv('NEMSIOGET','nemsio_get')
  IAUHrs = list(map(int,os.getenv('IAUFHRS','6').split(',')))
  Cdump = os.getenv('CDUMP', 'gdas')

  print(locals())
  calcanl_gfs(DoIAU, l4DEnsVar, Write4Danl, ComOut, APrefix, ASuffix, 
              FixDir, atmges_ens_mean, RunDir, NThreads, NEMSGet, IAUHrs, 
              ExecCMD, ExecCMDMPI, ExecAnl, ExecChgresGes, ExecChgresInc,
              Cdump)
