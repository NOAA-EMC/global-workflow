#!/usr/bin/env python3

import os
import socket
from pathlib import Path

from wxflow import YAMLFile


__all__ = ['Host']


class Host:
    """
    Gather Host specific information.
    """

    SUPPORTED_HOSTS = ['HERA', 'ORION', 'JET', 'HERCULES',
                       'WCOSS2', 'S4', 'CONTAINER', 'GAEA',
                       'AWSPW', 'AZUREPW', 'GOOGLEPW']

    def __init__(self, host=None):

        detected_host = self.detect()

        if host is not None and host != detected_host:
            raise ValueError(
                f'detected host: "{detected_host}" does not match host: "{host}"')

        self.machine = detected_host
        self.info = self._get_info
        self.scheduler = self.info['SCHEDULER']

    @classmethod
    def detect(cls):

        machine = 'NOTFOUND'
        container = os.getenv('SINGULARITY_NAME', None)
        pw_csp = os.getenv('PW_CSP', None)

        if os.path.exists('/scratch1/NCEPDEV'):
            machine = 'HERA'
        elif os.path.exists('/work/noaa'):
            machine = socket.gethostname().split("-", 1)[0].upper()
        elif os.path.exists('/lfs5/HFIP'):
            machine = 'JET'
        elif os.path.exists('/lfs/f1'):
            machine = 'WCOSS2'
        elif os.path.exists('/data/prod'):
            machine = 'S4'
        elif os.path.exists('/gpfs/f5'):
            machine = 'GAEA'
        elif container is not None:
            machine = 'CONTAINER'
        elif pw_csp is not None:
            if pw_csp.lower() not in ['azure', 'aws', 'google']:
                raise ValueError(
                    f'NOAA cloud service provider "{pw_csp}" is not supported.')
            machine = f"{pw_csp.upper()}PW"

        if machine not in Host.SUPPORTED_HOSTS:
            raise NotImplementedError(f'This machine is not a supported host.\n' +
                                      'Currently supported hosts are:\n' +
                                      f'{" | ".join(Host.SUPPORTED_HOSTS)}')

        return machine

    @property
    def _get_info(self) -> dict:

        hostfile = Path(os.path.join(os.path.dirname(__file__),
                        f'hosts/{self.machine.lower()}.yaml'))
        try:
            info = YAMLFile(path=hostfile)
        except FileNotFoundError:
            raise FileNotFoundError(f'{hostfile} does not exist!')
        except IOError:
            raise IOError(f'Unable to read from {hostfile}')
        except Exception:
            raise Exception(f'unable to get information for {self.machine}')

        return info
