#!/usr/bin/env python3
from variable_name_converter import GlobalToNetConverter, NetToGlobalConverter

REPO_ROOT = '/scratch3/NCEPDEV/global/Anton.Fernando/global-workflow/'
JOBS_PATH = 'dev/jobs/'

GlobalToNetConverter().convert(REPO_ROOT, JOBS_PATH, 'all')
#NetToGlobalConverter().convert(REPO_ROOT, JOBS_PATH, 'all')
