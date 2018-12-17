#! /usr/bin/env python3.6

import os, sys, logging

try:
    import crow.config
except ModuleNotFoundError:
    there=os.path.abspath(os.path.join(os.path.dirname(__file__),'../..'))
    sys.path.append(there)
    import crow.config
from crow.config import Platform

logging.basicConfig(stream=sys.stderr,level=logging.INFO,
   format='%(module)s:%(lineno)d: %(levelname)8s: %(message)s')
logger=logging.getLogger('setup_expt')

if len(sys.argv)<3:
    logger.error('Format: test_sections.py /path/to/config.yaml sections')
    exit(1)

conf=crow.config.from_file(sys.argv[1])

for var in sys.argv[2:]:
    val=eval(var,{},conf)
    crow.config.validate(val)
    print(f'{var} = {val!r}')
