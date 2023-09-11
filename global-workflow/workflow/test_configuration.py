import sys
from wxflow import Configuration


expdir = sys.argv[1]

cfg = Configuration(expdir)

print(f'experiment dir: {cfg.config_dir}')

print('configuration files ...')
line_separator = '\n'  # \escapes are not allowed inside f-strings
print(f'{line_separator.join(cfg.config_files)}')

print(f'config.base: {cfg.find_config("config.base")}')

print('*' * 80)
print('config.base ...')
base = cfg.parse_config('config.base')
cfg.print_config('config.base')
print(type(base))
print(base.HOMEgfs)

print('*' * 80)
print('config.anal...')
cfg.print_config(['config.base', 'config.anal'])


print('*' * 80)
print('config.efcs ...')
configs = ['config.base', 'config.fcst', 'config.efcs']
cfg.print_config(configs)
