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
                ComIn_Ges, GPrefix, GSuffix,
                FixDir, atmges_ens_mean, RunDir, NThreads, NEMSGet, IAUHrs,
                ExecCMD, ExecCMDMPI, ExecAnl, ExecChgresInc, Cdump):
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
                    gsi_utils.make_dir(CalcAnlDir)
                gsi_utils.copy_file(ExecAnl, CalcAnlDir+'/calc_anl.x')
                gsi_utils.link_file(RunDir+'/siginc.nc', CalcAnlDir+'/siginc.nc.06')
                gsi_utils.link_file(RunDir+'/sigf06', CalcAnlDir+'/ges.06')
                gsi_utils.link_file(RunDir+'/siganl', CalcAnlDir+'/anl.06')
                gsi_utils.copy_file(ExecChgresInc, CalcAnlDir+'/chgres_inc.x')
                # for ensemble res analysis
                if Cdump == "gdas":
                    CalcAnlDir = RunDir+'/calcanl_ensres_'+format(fh, '02')
                    if not os.path.exists(CalcAnlDir):
                        gsi_utils.make_dir(CalcAnlDir)
                    gsi_utils.copy_file(ExecAnl, CalcAnlDir+'/calc_anl.x')
                    gsi_utils.link_file(RunDir+'/siginc.nc', CalcAnlDir+'/siginc.nc.06')
                    gsi_utils.link_file(ComOut+'/'+APrefix+'atmanl.ensres'+ASuffix, CalcAnlDir+'/anl.ensres.06')
                    gsi_utils.link_file(ComIn_Ges+'/'+GPrefix+'atmf006.ensres'+GSuffix, CalcAnlDir+'/ges.ensres.06')
                    gsi_utils.link_file(RunDir+'/sigf06', CalcAnlDir+'/ges.06')
            else:
                if os.path.isfile('sigi'+format(fh, '02')+'.nc'):
                    # for full res analysis
                    CalcAnlDir = RunDir+'/calcanl_'+format(fh, '02')
                    CalcAnlDir6 = RunDir+'/calcanl_'+format(6, '02')
                    if not os.path.exists(CalcAnlDir):
                        gsi_utils.make_dir(CalcAnlDir)
                    if not os.path.exists(CalcAnlDir6):
                        gsi_utils.make_dir(CalcAnlDir6)
                    gsi_utils.link_file(ComOut+'/'+APrefix+'atma'+format(fh, '03')+ASuffix,
                                        CalcAnlDir6+'/anl.'+format(fh, '02'))
                    gsi_utils.link_file(RunDir+'/siga'+format(fh, '02'),
                                        CalcAnlDir6+'/anl.'+format(fh, '02'))
                    gsi_utils.link_file(RunDir+'/sigi'+format(fh, '02')+'.nc',
                                        CalcAnlDir+'/siginc.nc.'+format(fh, '02'))
                    gsi_utils.link_file(CalcAnlDir6+'/inc.fullres.'+format(fh, '02'),
                                        CalcAnlDir+'/inc.fullres.'+format(fh, '02'))
                    gsi_utils.link_file(RunDir+'/sigf'+format(fh, '02'),
                                        CalcAnlDir6+'/ges.'+format(fh, '02'))
                    gsi_utils.link_file(RunDir+'/sigf'+format(fh, '02'),
                                        CalcAnlDir+'/ges.'+format(fh, '02'))
                    gsi_utils.copy_file(ExecChgresInc, CalcAnlDir+'/chgres_inc.x')
                    # for ensemble res analysis
                    CalcAnlDir = RunDir+'/calcanl_ensres_'+format(fh, '02')
                    CalcAnlDir6 = RunDir+'/calcanl_ensres_'+format(6, '02')
                    if not os.path.exists(CalcAnlDir):
                        gsi_utils.make_dir(CalcAnlDir)
                    if not os.path.exists(CalcAnlDir6):
                        gsi_utils.make_dir(CalcAnlDir6)
                    gsi_utils.link_file(ComOut+'/'+APrefix+'atma'+format(fh, '03')+'.ensres'+ASuffix,
                                        CalcAnlDir6+'/anl.ensres.'+format(fh, '02'))
                    gsi_utils.link_file(RunDir+'/sigi'+format(fh, '02')+'.nc',
                                        CalcAnlDir6+'/siginc.nc.'+format(fh, '02'))
                    gsi_utils.link_file(ComIn_Ges+'/'+GPrefix+'atmf'+format(fh, '03')+'.ensres'+GSuffix,
                                        CalcAnlDir6+'/ges.ensres.'+format(fh, '02'))


    else:
        # for full res analysis
        CalcAnlDir = RunDir+'/calcanl_'+format(6, '02')
        if not os.path.exists(CalcAnlDir):
            gsi_utils.make_dir(CalcAnlDir)
        gsi_utils.copy_file(ExecAnl, CalcAnlDir+'/calc_anl.x')
        gsi_utils.link_file(RunDir+'/siginc.nc', CalcAnlDir+'/siginc.nc.06')
        gsi_utils.link_file(RunDir+'/sigf06', CalcAnlDir+'/ges.06')
        gsi_utils.link_file(RunDir+'/siganl', CalcAnlDir+'/anl.06')
        gsi_utils.copy_file(ExecChgresInc, CalcAnlDir+'/chgres_inc.x')
        # for ensemble res analysis
        CalcAnlDir = RunDir+'/calcanl_ensres_'+format(6, '02')
        if not os.path.exists(CalcAnlDir):
            gsi_utils.make_dir(CalcAnlDir)
        gsi_utils.copy_file(ExecAnl, CalcAnlDir+'/calc_anl.x')
        gsi_utils.link_file(RunDir+'/siginc.nc', CalcAnlDir+'/siginc.nc.06')
        gsi_utils.link_file(ComOut+'/'+APrefix+'atmanl.ensres'+ASuffix, CalcAnlDir+'/anl.ensres.06')
        gsi_utils.link_file(ComIn_Ges+'/'+GPrefix+'atmf006.ensres'+GSuffix, CalcAnlDir+'/ges.ensres.06')

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
    elif launcher == 'mpiexec':
        hostfile = os.getenv('PBS_NODEFILE','')
        with open(hostfile) as f:
            hosts_tmp = f.readlines()
        hosts_tmp = [x.strip() for x in hosts_tmp]
        hosts = []
        [hosts.append(x) for x in hosts_tmp if x not in hosts]
        nhosts = len(hosts)
        ExecCMDMPI_host = 'mpiexec -l -n '+str(nFH)
        tasks = int(os.getenv('ntasks',1))
        print('nhosts,tasks=', nhosts, tasks)
        if levs > tasks:
            ExecCMDMPILevs_host = 'mpiexec -l -n '+str(tasks)
            ExecCMDMPILevs_nohost = 'mpiexec -l -n '+str(tasks)
        else:
            ExecCMDMPILevs_host = 'mpiexec -l -n '+str(levs)
            ExecCMDMPILevs_nohost = 'mpiexec -l -n '+str(levs)
        ExecCMDMPI1_host = 'mpiexec -l -n 1 --cpu-bind depth --depth '+str(NThreads)
        ExecCMDMPI10_host = 'mpiexec -l -n 10 --cpu-bind depth --depth '+str(NThreads)
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
    ihost = 0
    ### interpolate increment to full background resolution
    for fh in IAUHH:
        # first check to see if increment file exists
        CalcAnlDir = RunDir+'/calcanl_'+format(fh, '02')
        if (os.path.isfile(CalcAnlDir+'/siginc.nc.'+format(fh, '02'))):
            print('Interpolating increment for f'+format(fh, '03'))
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
                    # For xjet, each instance of chgres_inc must run on two nodes each
                    if os.getenv('SLURM_JOB_PARTITION','') == 'xjet':
                        for a in range(0,4):
                            hostfile.write(hosts[ihost]+'\n')
                        ihost+=1
                        for a in range(0,5):
                            hostfile.write(hosts[ihost]+'\n')
                    for a in range(0,9): # need 9 more of the same host for the 10 tasks for chgres_inc
                        hostfile.write(hosts[ihost]+'\n')
            if launcher == 'srun':
                os.environ['SLURM_HOSTFILE'] = CalcAnlDir+'/hosts'
            print('interp_inc', fh, namelist)
            job = subprocess.Popen(ExecCMDMPI10_host+' '+CalcAnlDir+'/chgres_inc.x', shell=True, cwd=CalcAnlDir)
            print(ExecCMDMPI10_host+' '+CalcAnlDir+'/chgres_inc.x submitted on '+hosts[ihost])
            sys.stdout.flush()
            ec = job.wait()
            if ec != 0:
                print('Error with chgres_inc.x at forecast hour: f'+format(fh, '03'))
                print('Error with chgres_inc.x, exit code='+str(ec))
                print(locals())
                sys.exit(ec)
            ihost+=1
        else:
            print('f'+format(fh, '03')+' is in $IAUFHRS but increment file is missing. Skipping.')

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


    ######## compute determinstic analysis on ensemble resolution
    if Cdump == "gdas":
        chgres_jobs = []
        for fh in IAUHH:
            # first check to see if guess file exists
            CalcAnlDir6 = RunDir+'/calcanl_ensres_06'
            print(CalcAnlDir6+'/ges.ensres.'+format(fh, '02'))
            if (os.path.isfile(CalcAnlDir6+'/ges.ensres.'+format(fh, '02'))):
                print('Calculating analysis on ensemble resolution for f'+format(fh, '03'))
                ######## generate ensres analysis from interpolated background
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
            else:
                print('f'+format(fh, '03')+' is in $IAUFHRS but ensemble resolution guess file is missing. Skipping.')

    print('calcanl_gfs successfully completed at: ',datetime.datetime.utcnow())
    print(locals())

# run the function if this script is called from the command line
if __name__ == '__main__':
    DoIAU = gsi_utils.isTrue(os.getenv('DOIAU', 'NO'))
    l4DEnsVar = gsi_utils.isTrue(os.getenv('l4densvar', 'NO'))
    Write4Danl = gsi_utils.isTrue(os.getenv('lwrite4danl', 'NO'))
    ComIn_Ges = os.getenv('COMIN_GES', './')
    GPrefix = os.getenv('GPREFIX', './')
    GSuffix = os.getenv('GSUFFIX', './')
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
    ExecChgresInc = os.getenv('CHGRESINCEXEC', './interp_inc.x')
    NEMSGet = os.getenv('NEMSIOGET','nemsio_get')
    IAUHrs = list(map(int,os.getenv('IAUFHRS','6').split(',')))
    Cdump = os.getenv('CDUMP', 'gdas')

    print(locals())
    calcanl_gfs(DoIAU, l4DEnsVar, Write4Danl, ComOut, APrefix, ASuffix,
                ComIn_Ges, GPrefix, GSuffix,
                FixDir, atmges_ens_mean, RunDir, NThreads, NEMSGet, IAUHrs,
                ExecCMD, ExecCMDMPI, ExecAnl, ExecChgresInc,
                Cdump)
