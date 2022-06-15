import workflow_utils as wfu

from configuration import Configuration
from applications import AppConfig
from workflow_xml import RocotoXML

expdir = '/Users/rmahajan/scratch/gwWork/EXPDIR/test-workflow'
#mode = 'forecast-only'
mode = 'cycled'

cfg = Configuration(expdir)

_base = cfg.parse_config('config.base')
wfu.check_expdir(expdir, _base['EXPDIR'])

app_config = AppConfig(mode, cfg)
xml = RocotoXML(app_config)
xml.write('text.xml')