#! /usr/bin/env python3
f'This script requires python 3.6 or later'

import os, sys, logging, glob, io, getopt, re
from collections.abc import Sequence

sys.path.append(os.path.abspath(os.path.join(
    os.path.dirname(__file__),'../../..')))

from create_comrot import create_COMROT
import crow.config, crow.metascheduler, crow.dataflow
from crow.config import Platform, follow_main

logger=logging.getLogger("setup_case")

def epicfail(why):
    logger.error(why)
    sys.exit(1)
    
def main():
    ( optval, args ) = getopt.getopt(sys.argv[1:],"v",["verbose","force"])
    options=dict(optval)
    level=logging.INFO
    if '-v' in options or '--verbose' in options:
        level=logging.DEBUG
    logging.basicConfig(stream=sys.stderr,level=level)
    force="--force" in options

    if len(args)!=2:
        sys.stderr.write("Format: setup_case.py [-v] [--force] case-name experiment-name\n")
        exit(1)

    case=args[0]
    experiment_name=args[1]

    logger.info(f"read case {case}")
    conf=read_contents(case)
    conf.experiment_name=experiment_name
    crow.config.validate(conf.case)
    logger.info("Remove platforms from configuration.")
    for key in list(conf.keys()):
        if isinstance(conf[key],Platform) and key!='platform':
            del conf[key]
    
    EXPDIR=conf.places.EXPDIR
    logger.info(f'Run directory: {EXPDIR}')
    config_yaml=os.path.join(EXPDIR,'config.yaml')
    dataflow_db=os.path.join(EXPDIR,'dataflow.db')
            
    try:
        os.makedirs(EXPDIR)
    except FileExistsError:
        logger.warning(f'{EXPDIR}: exists')
        if not force:
            logger.error(f'{EXPDIR}: already exists.  Delete or use --force.')
            sys.exit(1)
        logger.warning(f'--force given; will replace config.yaml without '
                       'deleting directory')
    
    if 'IC_CDUMP' in conf.case and 'IC_DIR' in conf.case:
        create_COMROT(conf)
    
    chosen_workflow=conf.case.workflow
    conf.workflow=conf[chosen_workflow]

    suite=crow.config.Suite(conf[chosen_workflow])
    doc=crow.config.document_root(suite)

    expname=conf.case.experiment_name
    logger.info(f'Experiment name: {expname}')
    
    logger.info(f'Generate suite definition')
    rocoto_xml=crow.metascheduler.to_rocoto(suite)
    logger.info(f'Prepare cached YAML')
    yaml=crow.config.to_yaml(doc)
    
    logger.info(f'Write the config file: {config_yaml}')
    with open(config_yaml,'wt') as fd:
        fd.write(yaml)
    
    if not os.path.exists(dataflow_db):
        logger.info(f'Write the dataflow sqlite3 file: {dataflow_db}')
        df=crow.dataflow.from_suite(suite,dataflow_db)
        #Uncomment to dump database to stdout: df.dump(sys.stdout)

    rocoto_xml_file=os.path.join(EXPDIR,f'{expname}.xml')
    logger.info(f'Rocoto XML file: {rocoto_xml_file}')
    with open(rocoto_xml_file,'wt') as fd:
        fd.write(rocoto_xml)
    logger.info('Workflow XML file is generated.')
    logger.info('Use Rocoto to execute this workflow.')

if __name__ == "__main__":
    main()
