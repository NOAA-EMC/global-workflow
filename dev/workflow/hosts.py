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

    SUPPORTED_HOSTS = ['HERA', 'URSA', 'ORION', 'HERCULES', 'WCOSS2', 'CONTAINER',
                       'GAEAC5', 'GAEAC6', 'AWSPW', 'AZUREPW', 'GOOGLEPW']

    def __init__(self, host=None):

        if host is not None:
            if host not in Host.SUPPORTED_HOSTS:
                raise NotImplementedError(f'{host} is not a supported host.\n' +
                                          'Currently supported hosts are:\n' +
                                          f'{" | ".join(Host.SUPPORTED_HOSTS)}')
            self.machine = host
        # Otherwise, detect the host.
        else:
            # Detect the host if not provided
            self.detect()

        self.info = self._get_info
        self.scheduler = self.info['SCHEDULER']

    def __str__(self) -> str:
        # The string representation of the Host object is the name of the machine
        return f"{self.machine}"

    def detect(self) -> None:
        # Detect the machine name and store in self.machine

        self.machine = None

        machine_id = os.getenv('MACHINE_ID', 'UNKNOWN')
        pw_csp = os.getenv('PW_CSP', 'UNKNOWN')
        container = os.getenv('SINGULARITY_NAME', None)

        # Detect the machine since MACHINE_ID is set,
        # Additionaly, if PW_CSP is set, then the machine is a cloud machine
        if machine_id != 'UNKNOWN':
            if pw_csp != 'UNKNOWN':
                self.machine = f"{pw_csp.upper()}PW"
            else:
                self.machine = f"{machine_id.upper()}"
            return

        # Detect the machine since MACHINE_ID is not set
        if os.path.exists('/scratch3/NCEPDEV'):
            # Hera or Ursa
            # Open the mountinfo file and check if /home is mounted to "home_ursa" or "home_hera"
            # NOTE: the github runners do not have a /home directory, so self.machine will be unset
            with open('/proc/self/mountinfo') as f:
                for line in f:
                    fields = line.strip().split()
                    mount_point = fields[4]
                    if mount_point == "/apps":
                        mount_source = fields[9]
                        if "ursa" in mount_source.lower():
                            self.machine = "URSA"
                        elif "hera" in mount_source.lower():
                            self.machine = "HERA"

            # TODO: When Hera is no longer used, remove this check and switch to Ursa.
            # Check if this is the GitHub runner
            if self.machine != 'HERA' and self.machine != 'URSA':
                machine = socket.gethostname().upper()
                print(f'Detected host {machine}; assuming this is a GitHub runner.')
                self.machine = 'HERA'

        elif os.path.exists('/work/noaa'):
            # Orion or Hercules
            self.machine = socket.gethostname().split("-", 1)[0].upper()
        elif os.path.exists('/lfs/f1'):
            self.machine = 'WCOSS2'
        elif os.path.exists('/gpfs/f5'):
            self.machine = 'GAEAC5'
        elif os.path.exists('/gpfs/f6'):
            self.machine = 'GAEAC6'
        elif container is not None:
            self.machine = 'CONTAINER'
        elif pw_csp != "UNKNOWN":
            if pw_csp.lower() not in ['azure', 'aws', 'google']:
                raise ValueError(
                    f'cloud service provider "{pw_csp}" is not supported.')
            self.machine = f"{pw_csp.upper()}PW"

        if self.machine is not None:
            print(f"Detected host as '{self.machine}'")
            return

        # If we are here, we have not been able to detect a valid host
        raise NotImplementedError('Unable to detect a supported host.\n' +
                                  'Currently supported hosts are:\n' +
                                  f'{" | ".join(Host.SUPPORTED_HOSTS)}')

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
            raise Exception(f'unable to get information for {self}')

        return info
