from hosts import Host

print(f'supported hosts are: {", ".join(Host.SUPPORTED_HOSTS)}')

print(f'host detected as: {Host.detect}')
print(f'scheduler on host: {Host().scheduler}')

print('initializing host ...')
host = Host()

print(f'hostname: {host.machine}')

print(f'scheduler on host: {host.scheduler}')

print('host information ...')
line_separator = '\n'  # \escapes are not allowed inside f-strings
print(f'{line_separator.join(f"{key}: {host.info[key]}" for key in host.info.keys())}')
