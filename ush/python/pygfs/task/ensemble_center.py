#!/usr/bin/env python3

import datetime
from logging import getLogger
import netCDF4 as nc
from pprint import pformat
import os
from pygfs.jedi import Jedi
from wxflow import (AttrDict, FileHandler, Task, Executable,
                    add_to_datetime, to_fv3time, to_timedelta,
                    parse_j2yaml, save_as_yaml,
                    logit)

logger = getLogger(__name__.split('.')[-1])


class EnsembleCenter(Task):
    """
    Class for JEDI-based ensemble increment recentering
    """
    @logit(logger, name="EnsembleCenter")
    def __init__(self, config):
        """Constructor diagnostic atmospheric ensemble increment recentering

        This method will construct an ensemble increment recentering task
        This includes:
        - extending the task_config attribute AttrDict to include parameters required for this task
        - instantiate the Jedi attribute object

        Parameters
        ----------
        config: Dict
            dictionary object containing task configuration

        Returns
        ----------
        None
        """
        super().__init__(config)

        _res = int(self.task_config.CASE[1:])
        _res_anl = int(self.task_config.CASE_ANL[1:])
        _window_begin = add_to_datetime(self.task_config.current_cycle, -to_timedelta(f"{self.task_config.assim_freq}H") / 2)

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'npx_ges': _res + 1,
                'npy_ges': _res + 1,
                'npz_ges': self.task_config.LEVS - 1,
                'npz': self.task_config.LEVS - 1,
                'npx_anl': _res_anl + 1,
                'npy_anl': _res_anl + 1,
                'npz_anl': self.task_config.LEVS - 1,
                'ATM_WINDOW_BEGIN': _window_begin,
                'ATM_WINDOW_LENGTH': f"PT{self.task_config.assim_freq}H",
                'APREFIX': f"gdas.t{self.task_config.cyc:02d}z.",
                'GPREFIX': f"gdas.t{self.task_config.previous_cycle.hour:02d}z.",
            }
        )

        # Extend task_config with local_dict
        self.task_config = AttrDict(**self.task_config, **local_dict)

        # Create dictionary of Jedi objects
        expected_keys = ['ecen']
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.JEDI_CONFIG_YAML, self.task_config, expected_keys)

    @logit(logger)
    def initialize(self) -> None:
        """Initialize the ensemble increment recentering task

        This method will initialize the ensemble increment recentering task.
        This includes:
        - initializing the JEDI recentering application
        - creating working directories for each forecast hour
        - staging backgrounds and increments

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # Initialize ensemble recentering if running GDAS
        if self.task_config.RUN == 'gdas':
            # Initialize JEDI ensemble increment recentering application
            logger.info(f"Initializing JEDI recentering application")
            self.jedi_dict['ecen'].initialize(self.task_config)

            # Stage fix files
            logger.info(f"Staging JEDI fix files from {self.task_config.JEDI_FIX_YAML}")
            jedi_fix_dict = parse_j2yaml(self.task_config.JEDI_FIX_YAML, self.task_config)
            FileHandler(jedi_fix_dict).sync()
            logger.debug(f"JEDI fix files:\n{pformat(jedi_fix_dict)}")

        # Stage background and increment files
        logger.info(f"Staging background and increment files from {self.task_config.JEDI_BKG_INC_YAML}")
        fh_dict = parse_j2yaml(self.task_config.JEDI_BKG_INC_YAML, self.task_config)
        FileHandler(fh_dict).sync()
        logger.debug(f"JEDI background and increment files:\n{pformat(fh_dict)}")

    @logit(logger)
    def execute(self) -> None:
        """Run JEDI executable

        This method will run the JEDI executable for the ensemble increment recentering

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # Compute correction increment for ensemble recentering
        if self.task_config.RUN == 'gdas':
            self.jedi_dict['ecen'].execute()

        # Compute analyses
        for fh in self.task_config.IAUFHRS:
           add_increment(f"atmi{format(fh, '03')}.nc",
                         f"atma{format(fh, '03')}.nc")

           if self.task_config.DO_AERO_ANL:
               add_increment(f"aeroi{format(fh, '03')}.nc",
                             f"atma{format(fh, '03')}.nc")

           if self.task_config.DO_JEDISNOWDA:
               add_increment(f"asnowi{format(fh, '03')}.nc",
                             f"sfca{format(fh, '03')}.nc")

    @logit(logger)
    def finalize(self) -> None:
        """Finalize the ensemble increment recentering task

        This method will finalize the ensemble increment recentering task.
        This includes:
        - Move increment files to the comrot directory

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        fh_dict = {'copy': []}
        anal_time = os.system('echo $(date)')
        with open('loganl.txt', 'w') as file:
            file.write(f"{self.task_config.rCDUMP} {self.task_config.PDY}{self.task_config.cyc} atmanl and sfcanl done at {anal_time}")
        fh_dict['copy'].append(['loganl.txt', f"{self.task_config.COM_ATMOS_ANALYSIS}/{self.task_config.APREFIX}loganl.txt"])

        # Copy files to comrot
        if self.task_config.RUN == 'gdas':
            inc_prefix = f"{self.task_config.COM_ATMOS_ANALYSIS_ENSSTAT}/enkf{self.task_config.APREFIX}"
            for fh in self.task_config.IAUFHRS:
                if fh == 6:
                    for itile in range(6):
                        fh_dict['copy'].append([f"{self.task_config.DATA}/cubed_sphere_grid_catmi{format(fh, '03')}.tile{itile+1}.nc",
                                                f"{inc_prefix}cubed_sphere_grid_catminc.tile{itile+1}.nc"])
                    else:
                        for itile in range(6):
                            fh_dict['copy'].append([f"{self.task_config.DATA}/cubed_sphere_grid_catmi{format(fh, '03')}.tile{itile+1}.nc",
                                                    f"{inc_prefix}cubed_sphere_grid_catmi{format(fh, '03')}.tile{itile+1}.nc"])
        # Copy analyses to comrot
        for fh in self.task_config.IAUFHRS:
            if fh == 6:
                fh_dict['copy'].append([f"{self.task_config.DATA}/atma{format(fh, '03')}.nc",
                                        f"{self.task_config.COM_ATMOS_ANALYSIS}/{self.task_config.APREFIX}atmanl.nc"])
                fh_dict['copy'].append([f"{self.task_config.DATA}/sfca{format(fh, '03')}.nc",
                                        f"{self.task_config.COM_ATMOS_ANALYSIS}/{self.task_config.APREFIX}sfcanl.nc"])
            else:
                fh_dict['copy'].append([f"{self.task_config.DATA}/atma{format(fh, '03')}.nc",
                                        f"{self.task_config.COM_ATMOS_ANALYSIS}/{self.task_config.APREFIX}atma{format(fh, '03')}.nc"])
                fh_dict['copy'].append([f"{self.task_config.DATA}/sfca{format(fh, '03')}.nc",
                                        f"{self.task_config.COM_ATMOS_ANALYSIS}/{self.task_config.APREFIX}sfca{format(fh, '03')}.nc"])

        # Call FileHandler
        FileHandler(fh_dict).sync()

@logit(logger)
def add_increment(fn_incr: str, fn_bkg: str) -> None:
    try:
        with nc.Dataset(fn_incr, 'r') as nc_incr:
            with nc.Dataset(fn_bkg, 'r+') as nc_bkg:
                for var in nc_incr.variables:
                    if len(nc_incr[var].dimensions) == 3 or len(nc_incr[var].dimensions) == 4:
                        var_incr = nc_incr[var][:]
                        var_bkg = nc_bkg[var][:]

                        nc_bkg[var][:] = var_bkg + var_incr

    except Exception as e:
        logger.error(f"Error occurred with message {e}")
        raise
