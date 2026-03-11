#!/usr/bin/env python3
from pathlib import Path
from variable_name_converter import GlobalToNetConverter, NetToGlobalConverter

REPO_ROOT = Path(__file__).resolve().parents[2]
JOBS_PATH = 'dev/'

# Convert HOMEglobal -> HOMEglobal
#GlobalToNetConverter().convert(REPO_ROOT, JOBS_PATH, 'gfs')

# Revert HOMEglobal -> HOMEglobal
NetToGlobalConverter().convert(REPO_ROOT, JOBS_PATH, 'gfs')
