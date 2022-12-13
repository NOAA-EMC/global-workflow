#!/usr/bin/env python3

import os
from pathlib import Path

from pygw.yaml_file import YAMLFile


__all__ = ['Host']


class Host:
    """
    Gather Host specific information.
    """

    SUPPORTED_HOSTS = ['HERA', 'ORION', 'JET',
                       'WCOSS2', 'S4', 'CONTAINER']

    def __init__(self, host=None):

        detected_host = self.detect()

        if host is not None and host != detected_host:
            raise ValueError(f'detected host: "{detected_host}" does not match host: "{host}"')

        self.machine = detected_host
        self.info = self._get_info
        self.scheduler = self.info['SCHEDULER']

    @classmethod
    def detect(cls):

        machine = 'NOTFOUND'
        container = os.getenv('SINGULARITY_NAME', None)

        if os.path.exists('/scratch1/NCEPDEV'):
            machine = 'HERA'
        elif os.path.exists('/work/noaa'):
            machine = 'ORION'
        elif os.path.exists('/lfs4/HFIP'):
            machine = 'JET'
        elif os.path.exists('/lfs/f1'):
            machine = 'WCOSS2'
        elif os.path.exists('/data/prod'):
            machine = 'S4'
        elif container is not None:
            machine = 'CONTAINER'

        if machine not in Host.SUPPORTED_HOSTS:
            raise NotImplementedError(f'This machine is not a supported host.\n' +
                                      'Currently supported hosts are:\n' +
                                      f'{" | ".join(Host.SUPPORTED_HOSTS)}')

        return machine

    @property
    def _get_info(self) -> dict:

        hostfile = Path(os.path.join(os.path.dirname(__file__), f'hosts/{self.machine.lower()}.yaml'))
        try:
            info = YAMLFile(path=hostfile)
        except FileNotFoundError:
            raise FileNotFoundError(f'{hostfile} does not exist!')
        except IOError:
            raise IOError(f'Unable to read from {hostfile}')
        except Exception:
            raise Exception(f'unable to get information for {self.machine}')

        return info
