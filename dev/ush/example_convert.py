#!/usr/bin/env python3
from pathlib import Path
from variable_name_converter import GlobalToNetConverter, NetToGlobalConverter

REPO_ROOT = '/scratch3/NCEPDEV/global/Anton.Fernando/global-workflow'
TARGET_PATH = 'dev/jobs'

# Convert HOMEglobal -> HOMEgfs
#GlobalToNetConverter().convert(REPO_ROOT, TARGET_PATH, 'gfs')

# Revert HOMEgfs -> HOMEglobal
NetToGlobalConverter().convert(REPO_ROOT, TARGET_PATH, 'gfs')
