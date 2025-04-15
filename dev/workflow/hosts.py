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

    SUPPORTED_HOSTS = ['HERA', 'ORION', 'HERCULES', 'WCOSS2', 'CONTAINER',
                       'GAEAC5', 'GAEAC6', 'AWSPW', 'AZUREPW', 'GOOGLEPW']

    def __init__(self, host=None):

        if host is not None and host not in Host.SUPPORTED_HOSTS:
            raise NotImplementedError(f'{host} is not a supported host.\n' +
                                      'Currently supported hosts are:\n' +
                                      f'{" | ".join(Host.SUPPORTED_HOSTS)}')

        # Detect the host if not provided
        detected_host = self.detect() if host is None else host

        if host is not None and host != detected_host:
            raise ValueError(
                f'detected host: "{detected_host}" does not match provided host: "{host}"')

        self.machine = detected_host
        self.info = self._get_info
        self.scheduler = self.info['SCHEDULER']

    @classmethod
    def detect(cls):

        machine = os.getenv('MACHINE_ID', 'UNKNOWN')
        pw_csp = os.getenv('PW_CSP', 'UNKNOWN')
        container = os.getenv('SINGULARITY_NAME', None)

        # Detect the machine since MACHINE_ID is set,
        # Additionaly, if PW_CSP is set, then the machine is a cloud machine
        if machine != 'UNKNOWN':
            if pw_csp != 'UNKNOWN':
                machine = f"{pw_csp.upper()}PW"
            return machine

        # Detect the machine since MACHINE_ID is not set
        if os.path.exists('/scratch1/NCEPDEV'):
            machine = 'HERA'
        elif os.path.exists('/work/noaa'):
            machine = socket.gethostname().split("-", 1)[0].upper()
        elif os.path.exists('/lfs/f1'):
            machine = 'WCOSS2'
        elif os.path.exists('/gpfs/f5'):
            machine = 'GAEAC5'
        elif os.path.exists('/gpfs/f6'):
            machine = 'GAEAC6'
        elif container is not None:
            machine = 'CONTAINER'
        elif pw_csp is not None:
            if pw_csp.lower() not in ['azure', 'aws', 'google']:
                raise ValueError(
                    f'cloud service provider "{pw_csp}" is not supported.')
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
