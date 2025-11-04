#!/usr/bin/env python3

from datetime import datetime
from logging import getLogger
import netCDF4 as nc
import os
from pygfs.jedi import Jedi
from pygfs.task.analysis import Analysis
from typing import Dict, Any
from wxflow import AttrDict, FileHandler, to_fv3time, parse_j2yaml, logit

logger = getLogger(__name__.split('.')[-1])


class FV3AnalysisCalc(Analysis):
    """
    Class for analysis calculation
    """
    @logit(logger, name="FV3AnalysisCalc")
    def __init__(self, config: Dict[str, Any]):
        """Constructor for analysis calculation task

        This method will construct an analysis calculation
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
        _res_anl = int(self.task_config['CASE_ANL'][1:])

        # Create a local dictionary that is repeatedly used across this class
        self.task_config.update(AttrDict(
            {
                'npx_ges': _res + 1,
                'npy_ges': _res + 1,
                'npz_ges': self.task_config.LEVS - 1,
                'npx_anl': _res_anl + 1,
                'npy_anl': _res_anl + 1,
                'npz_anl': self.task_config.LEVS - 1,
                'npz': self.task_config.LEVS - 1,
            }
        ))

        # Extend task_config with content of config yaml for this task
        self.task_config.update(parse_j2yaml(self.task_config.TASK_CONFIG_YAML, self.task_config))

        # Create dictionary of Jedi objects
        expected_keys = ['atm_addincrement']
        if self.task_config.DO_AERO_ANL:
            expected_keys.append('aero_addincrement')
        if self.task_config.DO_JEDISNOWDA:
            expected_keys.append('snow_addincrement')
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.jedi_config, self.task_config, expected_keys)

    @logit(logger)
    def initialize(self) -> None:
        """Initialize the analysis calculation task

        This method will initialize the analysis calculation task.
        This includes:
        - stage input files from COM and create output directories
        - initialize JEDI applications

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # Stage files from COM
        logger.info(f"Staging files from COM")
        FileHandler(self.task_config.data_in).sync()

        # Initialize GDASApp JEDI addincrement application
        logger.info(f"Initializing GDASApp JEDI addincrement applications")
        self.jedi_dict['atm_addincrement'].initialize(self.task_config)
        if self.task_config.DO_AERO_ANL:
            self.jedi_dict['aero_addincrement'].initialize(self.task_config)
        if self.task_config.DO_JEDISNOWDA:
            self.jedi_dict['snow_addincrement'].initialize(self.task_config)

    @logit(logger)
    def execute(self) -> None:
        """Compute analyses

        This method will execute the analysis calculation task. This includes:
        - Running the addincrement applications to compute the analysis variables
          and interpolate to the Gaussian grid
        - Inserting the resulting increments into the Gaussian UFS history files to obtain
          analysis files

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # Convert cubed sphere increments to Gaussian grid
        self.jedi_dict['atm_addincrement'].execute()
        if self.task_config.DO_AERO_ANL:
            self.jedi_dict['aero_addincrement'].execute()
        if self.task_config.DO_JEDISNOWDA:
            self.jedi_dict['snow_addincrement'].execute()

        # Loop through forecast hours
        auxgrid_time_str = to_fv3time(self.task_config.current_cycle).replace('.', '_') + 'z'

        # Atmosphere
        logger.info(f"Inserting analysis variables into atmospheric analysis file")
        insert_analysis_variables(self.task_config.current_cycle,
                                  f"atmanl.{auxgrid_time_str}.nc4",
                                  f"{self.task_config.GPREFIX}atmf006.nc")

        # Aerosols
        if self.task_config.DO_AERO_ANL:
            logger.info(f"Inserting analysis variables into aerosol analysis file")
            insert_analysis_variables(self.task_config.current_cycle,
                                      f"aeroanl.{auxgrid_time_str}.nc4",
                                      f"{self.task_config.GPREFIX}atmf006.nc")

        # Snow
        if self.task_config.DO_JEDISNOWDA:
            logger.info(f"Inserting analysis variables into snow analysis file")
            insert_analysis_variables(self.task_config.current_cycle,
                                      f"snowanl.{auxgrid_time_str}.nc4",
                                      f"{self.task_config.GPREFIX}sfcf006.nc")

    @logit(logger)
    def finalize(self) -> None:
        """Finalize the analysis calculation task

        This method will finalize the analysis calculation task.
        This includes:
        - write analysis log file
        - save output files and YAMLs to COM

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # Write analysis log file
        formatted_date = datetime.now().strftime("%a %b %d %H:%M:%S %Z%Y")
        log_file = os.path.join(self.task_config.COMOUT_ATMOS_ANALYSIS, f"{self.task_config.RUN}.t{self.task_config.cyc}z.done.txt")
        message = f"{self.task_config.rCDUMP} {self.task_config.PDY}{self.task_config.cyc} atmanl and sfcanl done at {formatted_date}"
        with open(log_file, "w") as file:
            file.write(f"{message}\n")

        # Save files from COM
        logger.info(f"Saving files to COM")
        FileHandler(self.task_config.data_out).sync()


@logit(logger)
def insert_analysis_variables(valid_time, fn_anl: str, fn_bkg: str) -> None:
    """Insert analysis variable into Gaussian history file

    This function will open the analysis and UFS Gaussian history files and
    insert the anlaysis variables in the Gaussian history file.
    Thus, the history file becomes an analysis file suitable to be read
    by UPP.

    Parameters
    ----------
    valid_time: datetime
        datetime object time in which analysis is valid
    fn_anl: str
        path of analysis file
    fn_bkg: str
        path of history file

    Returns
    ----------
    None
    """

    try:
        with nc.Dataset(fn_anl, 'r') as nc_anl, nc.Dataset(fn_bkg, 'r+') as nc_bkg:
            # Change the units of the time coordinate since the units from the UFS history
            # file will break UPP
            time_var = nc_bkg.variables['time']
            time_var.units = valid_time.strftime('hours since %Y-%m-%dT%H:%M:%S')
            time_var[:] = 0.

            # Insert analysis variables into history file
            for var in nc_anl.variables:
                if len(nc_anl[var].dimensions) == 3 or len(nc_anl[var].dimensions) == 4:
                    var_anl = nc_anl[var][:]
                    var_bkg = nc_bkg[var][:]

                    nc_bkg[var][:] = var_anl

    except Exception as e:
        logger.error(f"Error occurred with message {e}")
        raise
