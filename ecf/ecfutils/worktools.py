#! /usr/bin/env python3
f'This python module requires python 3.6 or newer'

import logging, os, io, sys, datetime, glob, shutil, subprocess, re, itertools, collections
from collections import OrderedDict
from copy import copy
from getopt import getopt
from contextlib import suppress
logger=logging.getLogger('crow.model.fv3gfs')

YAML_DIRS_TO_COPY={ 'schema':'schema',
                    'defaults':'defaults',
                    'config':'config',
                    'runtime':'runtime' } # important: no ending /
YAML_FILES_TO_COPY={ '_expdir_main.yaml': '_main.yaml',
                     'user.yaml': 'user.yaml' }

try:
    import crow
except ImportError as ie:
    thisdir=os.path.dirname(os.path.abspath(__file__))
    topdir=os.path.realpath(os.path.join(thisdir,"../.."))
    sys.path.append(topdir)
    del thisdir, topdir

level=logging.WARNING
if os.environ.get('WORKTOOLS_VERBOSE','NO') == 'YES':
    level=logging.INFO
logging.basicConfig(stream=sys.stderr,level=level)

import crow.tools, crow.config
from crow.metascheduler import to_ecflow, to_rocoto
from crow.config import from_dir, Suite, from_file, to_yaml
from crow.tools import Clock

ECFNETS_INCLUDE = "/ecf/ecfnets/include"
SIX_HOURS = datetime.timedelta(seconds=6*3600)

def loudly_make_dir_if_missing(dirname):
    if dirname and not os.path.exists(dirname):
        logger.info(f'{dirname}: make directory')
        os.makedirs(dirname)

def loudly_make_symlink(src,tgt):
    logger.debug(f'{src}: symlink {tgt}')
    with suppress(FileNotFoundError): os.unlink(tgt)
    if not os.path.exists(src):
        logger.warning(f'{src}: link target does not exist')
    os.symlink(src,tgt)

def make_parent_dir(filename):
    loudly_make_dir_if_missing(os.path.dirname(filename))

def find_available_platforms(platdir):
    available={}
    for matching_file in glob.glob(f'{platdir}/*.yaml'):
        logger.info(f'{matching_file}: check this platform...')
        plat=from_file('user.yaml',matching_file)
        if not 'platform' in plat or \
           not 'detect' in plat.platform or \
           not 'name' in plat.platform:
            logger.warning(f'{matching_file}: does not contain a '
                           '"platform" map with "detect" and "name"')
        if plat.platform.detect:
            logger.info(f'{matching_file}: platform {plat.platform.name} matches')
            available[plat.platform.name]=plat
    return available

def select_platform(requested_platform,valid_platforms):
    if not requested_platform and len(valid_platforms)>1:
        logger.error('More than one platform is available: '
                     +(', '.join(valid_platforms.keys())))
        logger.error('Pick one with -p option')
        exit(1)
    elif requested_platform in valid_platforms:
        platdoc=valid_platforms[requested_platform]
    elif requested_platform:
        logger.error(f'Invalid platform {requested_platform}.')
        logger.error('Available platforms: '+
                     ', '.join(valid_platforms.keys()))
        exit(1)
    else: # choose first platform
        platdoc=next(iter(valid_platforms.values()))
    platdoc.platform.Evaluate=True
    crow.config.evaluate_immediates(platdoc.platform)
    return platdoc

def create_COMROT(conf):
    if not 'IC_CDUMP' in conf.settings or not conf.settings.IC_CDUMP:
        logger.info('IC_CDUMP not specified; will assume scripts will provide their own ICs.')
        print('conf.settings.IC_CDUMP: not set; will assume scripts will provide their own ICs.')
        return
    cdump = conf.settings.IC_CDUMP
    icsdir = conf.places.ICSDIR
    comrot = conf.places.ROTDIR
    resens = conf.fv3_enkf_settings.CASE[1:]
    resdet = conf.fv3_gfs_settings.CASE[1:]
    idate = conf.settings.SDATE
    detdir = f'{cdump}.{idate:%Y%m%d}/{idate:%H}'
    nens = conf.data_assimilation.NMEM_ENKF
    enkfdir = f'enkf.{cdump}.{idate:%Y%m%d}/{idate:%H}'
    idatestr = f'{idate:%Y%m%d%H}'

    print(f'Input conditions from model: {cdump.upper()}')
    print(f'Copy input conditions from: {icsdir}')
    logger.info(f'Input conditions: {icsdir}')

    loudly_make_dir_if_missing(os.path.join(comrot,enkfdir))
    loudly_make_dir_if_missing(os.path.join(comrot, detdir))

    print(f'Copy input conditions to: {comrot}')
    logger.info(f'Workflow COM root: {comrot}')

    # Link ensemble member initial conditions
    for i in range(1, nens + 1):
        memdir=os.path.join(comrot,enkfdir,f'mem{i:03d}')
        loudly_make_dir_if_missing(memdir)
        src=os.path.join(icsdir, idatestr, f'C{resens}',f'mem{i:03d}','INPUT')
        tgt=os.path.join(comrot, enkfdir, f'mem{i:03d}', 'INPUT')
        loudly_make_symlink(src,tgt)

    # Link deterministic initial conditions
    src=os.path.join(icsdir, idatestr, f'C{resdet}', 'control', 'INPUT')
    tgt=os.path.join(comrot, detdir, 'INPUT')
    loudly_make_symlink(src,tgt)

    # Link bias correction and radiance diagnostics files
    for fname in ['abias', 'abias_pc', 'abias_air', 'radstat']:
        file=f'{cdump}.t{idate:%H}z.{fname}'
        src=os.path.join(icsdir, idatestr, file)
        tgt=os.path.join(comrot, detdir, file)
        loudly_make_symlink(src,tgt)

def find_case_yaml_file_for(case_name):
    for case_file in [ case_name,f"{case_name}.yaml",f"cases/{case_name}",
                       f"cases/{case_name}.yaml","/" ]:
        if os.path.exists(case_file) and case_file!='/':
            return case_file
    if case_file == "/":
        logger.error(f"{case_name}: no such case; pick one from in cases/")
        exit(1)

def read_yaml_suite(dir):
    logger.info(f'{dir}: read yaml files specified in _main.yaml')
    conf=from_dir(dir)
    assert(conf.suite._path)
    for scope_name in conf.validate_me:
        crow.config.validate(conf[scope_name])
    suite=Suite(conf.suite)
    assert(suite.viewed._path)
    return conf,suite

def make_config_files_in_expdir(doc,expdir):
    for key in doc.keys():
        if not key.startswith('config_'): continue
        value=doc[key]
        if not isinstance(value,collections.Mapping): continue
        if not 'filename' in value or not 'content' in value:
            logger.warning(f'{key}: config files require "filename" and "content" entries.')
        filename=os.path.join(expdir,str(value.filename))
        content=str(value.content)
        logger.info(f'{filename}: write')
        with open(filename,'wt') as fd:
            fd.write(content)

def make_yaml_files_in_expdir(srcdir,case_name,experiment_name,platdoc,force):
    logger.info(f'{srcdir}: get yaml files from here')
    logger.info(f'{case_name}: use this case')

    case_file=find_case_yaml_file_for(case_name)
    platform_yaml=to_yaml(platdoc)

    names={ 'names': { 'experiment':experiment_name,
                       'case':case_name } }
    names_yaml=to_yaml(names)

    # Get the configuration from the source directory:
    with io.StringIO() as fd:
        fd.write(platform_yaml)
        fd.write('\n\n')
        fd.write(names_yaml)
        fd.write('\n\n')
        crow.config.follow_main(fd,srcdir)
        fd.write('\n\n')
        with open(case_file,'rt') as cfd:
            fd.write(cfd.read())
        config_contents=fd.getvalue()
    config=crow.config.from_string(config_contents)
    workflow_file=os.path.join(srcdir,config.places.workflow_file)
    tgtdir=config.places.EXPDIR
    rotdir=config.places.ROTDIR

    logger.info(f'{rotdir}: COM files will be here')
    logger.info(f'{tgtdir}: send yaml files to here')

    gud=True
    if os.path.exists(tgtdir):
        gud=False
        logger.warning(f'{tgtdir}: already exists!')
    if os.path.exists(rotdir):
        gud=False
        logger.warning(f'{rotdir}: already exists!')
    if not gud and not force:
        logger.error('Target directories already exist.')
        logger.error('I will not start a workflow unless you do -f.')
        logger.critical('Use -f to force this workflow to start, but we aware that config, initial COM, and yaml files will be overwritten.  Other files will remain unmodified.')
        exit(1)
    elif not gud:
        logger.warning('Target directories already exist.')
        logger.warning('Received -f, so I will start anyway.')
        logger.warning('Will overwrite config, initial COM, and yaml files.')
        logger.warning('All other files will remain unmodified.')

    del config

    if not os.path.exists(tgtdir):
        logger.info(f'{tgtdir}: make directory')
        os.makedirs(tgtdir)

    logger.info(f'{tgtdir}/names.yaml: write experiment name and case name')
    with open(f'{tgtdir}/names.yaml','wt') as fd:
        fd.write(names_yaml)

    logger.info(f'{tgtdir}/platform.yaml: write platform logic')
    with open(f'{tgtdir}/platform.yaml','wt') as fd:
        fd.write(platform_yaml)

    logger.info(f'{case_file}: use this case file')
    shutil.copy2(case_file,os.path.join(tgtdir,'case.yaml'))

    logger.info(f'{workflow_file}: use this workflow file')
    shutil.copy2(workflow_file,os.path.join(tgtdir,'workflow.yaml'))

    for srcfile,tgtbase in itertools.chain(
            iter(YAML_DIRS_TO_COPY.items()),
            iter(YAML_FILES_TO_COPY.items())):
        tgtfile=os.path.join(tgtdir,tgtbase)
        if os.path.isdir(srcfile):
            logger.info(f'{srcfile}: copy yaml directory tree to {tgtfile}')
            if os.path.exists(tgtfile):
                logger.info(f'{tgtfile}: delete directory')
                shutil.rmtree(tgtfile)
            shutil.copytree(srcfile,tgtfile)
        else:
            logger.info(f'{srcfile}: copy yaml file to {tgtfile}')
            shutil.copyfile(srcfile,tgtfile)
        del tgtfile

    # Deal with the static files:
    for srcfile in glob.glob(f'{srcdir}/static/*.yaml'):
        logger.info(f'{srcfile}: read file')
        doc=from_file(srcfile)
        tgtfile=os.path.join(tgtdir,"static_"+os.path.basename(srcfile))
        yaml=to_yaml(doc)
        logger.info(f'{tgtfile}: generate file')
        with open(tgtfile,'wt') as fd:
            fd.write('# This file is automatically generated from:\n')
            fd.write(f'#    {srcfile}')
            fd.write('# Changes to this file may be overwritten.\n\n')
            fd.write(yaml)
        del doc,tgtfile

    logger.info(f'{tgtdir}: yaml files created here')
    return tgtdir

def make_clocks_for_cycle_range(suite,first_cycle,last_cycle,surrounding_cycles):
    suite_clock=copy(suite.Clock)
    logger.info(f'cycles to write:   {first_cycle:%Ft%T} - {last_cycle:%Ft%T}')
    suite.ecFlow.write_cycles = Clock(
        start=first_cycle,end=last_cycle,step=SIX_HOURS)
    first_analyzed=max(suite_clock.start,first_cycle-surrounding_cycles*SIX_HOURS)
    last_analyzed=min(suite_clock.end,last_cycle+surrounding_cycles*SIX_HOURS)
    logger.info(f'cycles to analyze: {first_analyzed:%Ft%T} - {last_analyzed:%Ft%T}')
    suite.ecFlow.analyze_cycles=Clock(
        start=first_analyzed,end=last_analyzed,step=SIX_HOURS)

def generate_ecflow_suite_in_memory(suite,first_cycle,last_cycle,surrounding_cycles):
    logger.info(f'make suite for cycles: {first_cycle:%Ft%T} - {last_cycle:%Ft%T}')
    make_clocks_for_cycle_range(suite,first_cycle,last_cycle,surrounding_cycles)
    suite_defs, ecf_files = to_ecflow(suite)
    return suite_defs, ecf_files

def write_ecflow_suite_to_disk(targetdir, suite_defs, ecf_files):
    written_suite_defs=OrderedDict()
    logger.info(f'{targetdir}: write suite here')
    for deffile in suite_defs.keys():
        defname = suite_defs[deffile]['name']
        defcontents = suite_defs[deffile]['def']
        filename=os.path.realpath(os.path.join(targetdir,'defs',deffile))
        make_parent_dir(filename)
        logger.info(f'{defname}: {filename}: write suite definition')
        with open(os.path.join(targetdir,filename),'wt') as fd:
            fd.write(defcontents)
        written_suite_defs[defname]=filename
        for setname in ecf_files:
            logger.info(f'{defname}: write ecf file set {setname}')
            for filename in ecf_files[setname]:
                full_fn=os.path.realpath(os.path.join(targetdir,defname,filename)+'.ecf')
                logger.debug(f'{defname}: {setname}: write ecf file {full_fn}')
                make_parent_dir(full_fn)
                with open(full_fn,'wt') as fd:
                    fd.write(ecf_files[setname][filename])
    return written_suite_defs

def get_target_dir_and_check_ecflow_env():
    ECF_HOME=os.environ.get('ECF_HOME',None)

    if not ECF_HOME:
        logger.error('Set $ECF_HOME to location where your ecflow files should reside.')
        return None
    elif not os.environ.get('ECF_PORT',None):
        logger.error('Set $ECF_PORT to the port number of your ecflow server.')
        return None
    elif not os.path.isdir(ECF_HOME):
        logger.error('Directory $ECF_HOME={ECF_HOME} does not exist.  You need to set up your account for ecflow before you can run any ecflow workflows.')
        return None
    
    for file in [ 'head.h', 'tail.h', 'envir-xc40.h' ]:
        yourfile=os.path.join(ECF_HOME,file)
        if not os.path.exists(yourfile):
            logger.warning(f'{yourfile}: does not exist.  I will get one for you.')
            os.symlink(os.path.join(ECFNETS_INCLUDE,file),yourfile)
        else:
            logger.info(f'{yourfile}: exists.')
        
    return ECF_HOME

def create_new_ecflow_workflow(suite,surrounding_cycles=2):
    ECF_HOME=get_target_dir_and_check_ecflow_env()
    if not ECF_HOME: return None,None,None,None
    first_cycle=suite.Clock.start
    last_cycle=min(suite.Clock.end,first_cycle+suite.Clock.step*2)
    suite_defs, ecf_files = generate_ecflow_suite_in_memory(
        suite,first_cycle,last_cycle,surrounding_cycles)
    suite_def_files = write_ecflow_suite_to_disk(
        ECF_HOME,suite_defs,ecf_files)
    return ECF_HOME, suite_def_files, first_cycle, last_cycle

def update_existing_ecflow_workflow(suite,first_cycle,last_cycle,
                                    surrounding_cycles=2):
    ECF_HOME=get_target_dir_and_check_ecflow_env()
    suite_defs, ecf_files = generate_ecflow_suite_in_memory(
        suite,first_cycle,last_cycle,surrounding_cycles)
    suite_def_files = write_ecflow_suite_to_disk(
        ECF_HOME,suite_defs,ecf_files)
    return ECF_HOME, suite_def_files

def load_ecflow_suites(ECF_HOME,suite_def_files):
    logger.info(f'{ECF_HOME}: load suites: '
                f'{", ".join(suite_def_files.keys())}')
    with crow.tools.chdir(ECF_HOME):
        for file in suite_def_files.values():
            cmd=f'ecflow_client --load {file}'
            logger.info(cmd)
            subprocess.run(cmd,check=False,shell=True)

def begin_ecflow_suites(ECF_HOME,suite_def_files):
    logger.info(f'{ECF_HOME}: begin suites: '
                f'{", ".join(suite_def_files.keys())}')
    with crow.tools.chdir(ECF_HOME):
        for suite in suite_def_files.keys():
            cmd=f'ecflow_client --begin {suite}'
            logger.info(cmd)
            subprocess.run(cmd,check=False,shell=True)

def make_rocoto_xml(suite,filename):
    with open(filename,'wt') as fd:
        logger.info(f'{filename}: create Rocoto XML document')
        fd.write(to_rocoto(suite))
    print(f'{filename}: Rocoto XML document created here.')
    
########################################################################

# These functions are called directly from scripts, and can be thought
# of as "main programs."

def remake_ecflow_files_for_cycles(
        yamldir,first_cycle_str,last_cycle_str,
        surrounding_cycles=2):
    ECF_HOME=get_target_dir_and_check_ecflow_env()
    conf,suite=read_yaml_suite(yamldir)
    loudly_make_dir_if_missing(f'{conf.places.ROTDIR}/log')

    first_cycle=datetime.datetime.strptime(first_cycle_str,'%Y%m%d%H')
    first_cycle=max(suite.Clock.start,first_cycle)

    last_cycle=datetime.datetime.strptime(last_cycle_str,'%Y%m%d%H')
    last_cycle=max(first_cycle,min(suite.Clock.end,last_cycle))

    suite_defs, ecf_files = generate_ecflow_suite_in_memory(
        suite,first_cycle,last_cycle,surrounding_cycles)
    written_suite_defs = write_ecflow_suite_to_disk(
        ECF_HOME, suite_defs, ecf_files)
    print(f'''Suite definition files and ecf files have been written to:

  {ECF_HOME}

If all you wanted to do was update the ecf files, then you're done.

If you want to update the suite (cycle) definitions, or add suites
(cycles), you will need to call ecflow_client's --load, --begin,
--replace, or --delete commands.''')

def create_and_load_ecflow_workflow(yamldir,surrounding_cycles=2,begin=False):
    conf,suite=read_yaml_suite(yamldir)
    loudly_make_dir_if_missing(f'{conf.places.ROTDIR}/log')
    ECF_HOME, suite_def_files, first_cycle, last_cycle = \
        create_new_ecflow_workflow(suite,surrounding_cycles)
    if not ECF_HOME:
        logger.error('Could not create workflow files.  See prior errors for details.')
        return False
    load_ecflow_suites(ECF_HOME,suite_def_files)
    if begin:
        begin_ecflow_suites(ECF_HOME,suite_def_files)
        
def add_cycles_to_running_ecflow_workflow_at(
        yamldir,first_cycle_str,last_cycle_str,surrounding_cycles=2): 
    conf,suite=read_yaml_suite(yamldir)
    first_cycle=datetime.datetime.strptime(first_cycle_str,'%Y%m%d%H')
    last_cycle=datetime.datetime.strptime(last_cycle_str,'%Y%m%d%H')
    ECF_HOME, suite_def_files = update_existing_ecflow_workflow(
        suite,first_cycle,last_cycle,surrounding_cycles)
    load_ecflow_suites(ECF_HOME,suite_def_files)    
    begin_ecflow_suites(ECF_HOME,suite_def_files)    

def make_rocoto_xml_for(yamldir):
    conf,suite=read_yaml_suite(yamldir)
    assert(suite.viewed._path)
    loudly_make_dir_if_missing(f'{conf.places.ROTDIR}/log')
    make_rocoto_xml(suite,f'{yamldir}/workflow.xml')

def setup_case_usage(why=None):
    print('fixme: usage message here')
    exit(1)

def setup_case(command_line_arguments):
    options,positionals=getopt(command_line_arguments,'vfp:')
    options=dict(options)

    force='-f' in options

    if '-v' in options:
        logger.setLevel(logging.INFO)

    if len(positionals)!=2:
        setup_case_usage('expected two positional arguments')

    case_name=positionals[0]
    experiment_name=positionals[1]
    if not re.match('^[A-Za-z][A-Za-z0-9_]*$',experiment_name):
        logger.error('{experiment_name}: experiment names must be '
                     'alphanumeric and start with a letter.')

    if not os.path.exists('user.yaml'):
        logger.error('You did not create user.yaml!')
        logger.error('Copy user.yaml.default to user.yaml and edit.')
        exit(1)

    requested_platform=options.get('-p',None)
    valid_platforms=find_available_platforms("platforms/")
    platdoc=select_platform(requested_platform,valid_platforms)
    logger.info(f'{platdoc.platform.name}: selected this platform.')

    EXPDIR = make_yaml_files_in_expdir(
        os.path.abspath('.'),case_name,experiment_name,platdoc,force)

    doc=from_dir(EXPDIR,validation_stage='setup')
    make_config_files_in_expdir(doc,EXPDIR)

    create_COMROT(doc)

    print()
    print(f'Case "{case_name}" is set up under experiment name "{experiment_name}" with:')
    print()
    print(f'  YAML files:     {EXPDIR}')
    print(f'  Config files:   {EXPDIR}')
    print(f'  COM directory:  {doc.places.ROTDIR}')
    print()
    print('Now you should make a workflow:')
    print()
    print(f'  Rocoto: ./make_rocoto_xml_for.sh {EXPDIR}')
    print(f'  ecFlow: ./load_ecflow_workflow.sh {EXPDIR}')
    print()
