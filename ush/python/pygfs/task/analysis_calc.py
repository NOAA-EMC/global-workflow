#!/usr/bin/env python3

from logging import getLogger
import netCDF4 as nc
from pprint import pformat
from pygfs.jedi import Jedi
from wxflow import (AttrDict, FileHandler, Task,
                    parse_j2yaml,
                    to_timedelta, add_to_datetime, to_fv3time,
                    logit)

logger = getLogger(__name__.split('.')[-1])


class AnalysisCalc(Task):
    """
    Class for analysis calculation
    """
    @logit(logger, name="AnalysisCalc")
    def __init__(self, config):
        """Constructor for analysis calculation task

        This method will construct an analysis calculation
        This includes:
        - extending the task_config attribute AttrDict to include parameters required for this task

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
                'ATM_WINDOW_LENGTH': f"PT{self.task_config.assim_freq}H",
                'ATM_WINDOW_BEGIN': _window_begin,
                'APREFIX': f"gdas.t{self.task_config.cyc:02d}z.",
                'GPREFIX': f"gdas.t{self.task_config.previous_cycle.hour:02d}z.",
            }
        )

        # Extend task_config with local_dict
        self.task_config = AttrDict(**self.task_config, **local_dict)

        # Create dictionary of Jedi objects
        expected_keys = ['atm_convertstate']
        if self.task_config.DO_AERO_ANL:
            expected_keys.append('aero_convertstate')
        if self.task_config.DO_JEDISNOWDA:
            expected_keys.append('snow_convertstate')
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.JEDI_CONFIG_YAML, self.task_config, expected_keys)

    @logit(logger)
    def initialize(self) -> None:
        """Initialize the analysis calculation task

        This method will initialize the analysis calculation task.
        This includes:
        - staging backgrounds and increments

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # Initialize JEDI ensemble increment recentering application
        logger.info(f"Initializing JEDI convertstate applications")
        self.jedi_dict['atm_convertstate'].initialize(self.task_config)
        if self.task_config.DO_AERO_ANL:
            self.jedi_dict['aero_convertstate'].initialize(self.task_config)
        if self.task_config.DO_JEDISNOWDA:
            self.jedi_dict['snow_convertstate'].initialize(self.task_config)

        # Stage fix files
        logger.info(f"Staging JEDI fix files from {self.task_config.JEDI_FIX_YAML}")
        jedi_fix_dict = parse_j2yaml(self.task_config.JEDI_FIX_YAML, self.task_config)
        FileHandler(jedi_fix_dict).sync()
        logger.debug(f"JEDI fix files:\n{pformat(jedi_fix_dict)}")

        # Stage background and increment files
        logger.info(f"Staging background and increment files from {self.task_config.JEDI_BKG_INC_YAML}")
        fh_dict = parse_j2yaml(self.task_config.JEDI_BKG_INC_YAML, self.task_config)
        FileHandler(fh_dict).sync()
        logger.debug(f"Background and increment files:\n{pformat(fh_dict)}")

    @logit(logger)
    def execute(self) -> None:
        """Compute analyses

        This method will add increments to backgrounds to generate the analyses

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # Convert cubed sphere increments to Gaussian grid
        self.jedi_dict['atm_convertstate'].execute()
        if self.task_config.DO_AERO_ANL:
            self.jedi_dict['aero_convertstate'].execute()
        if self.task_config.DO_JEDISNOWDA:
            self.jedi_dict['snow_convertstate'].execute()

        # Loop through forecast hours
        for fh in self.task_config.IAUFHRS:
           hr = format(fh, '03')
           valid_time = add_to_datetime(self.task_config.current_cycle, to_timedelta(hr))
           auxgrid_time_str = to_fv3time(valid_time).replace('.', '_') + 'z'

           # Atmosphere
           add_increment(valid_time,
                         f"atmi{hr}.{auxgrid_time_str}.nc4",
                         f"atma{hr}.nc")

           # Aerosols
           if self.task_config.DO_AERO_ANL:
               add_increment(valid_time,
                             f"aeroi{hr}.{auxgrid_time_str}.nc4",
                             f"atma{hr}.nc")

           # Snow
           if self.task_config.DO_JEDISNOWDA:
               add_increment(valid_time,
                             f"snowi{hr}.{auxgrid_time_str}.nc4",
                             f"sfca{hr}.nc")

    @logit(logger)
    def finalize(self) -> None:
        """Finalize the analysis calculation task

        This method will finalize the analysis calculation task.
        This includes:
        - Move analysis files to the comrot directory

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # Copy analyses to comrot
        fh_dict = {'copy': []}
        anl_prefix = f"{self.task_config.COM_ATMOS_ANALYSIS}/{self.task_config.APREFIX}"
        for fh in self.task_config.IAUFHRS:
            hr = format(fh, '03')
            if fh == 6:
                fh_dict['copy'].append([f"{self.task_config.DATA}/atma{hr}.nc",
                                        f"{anl_prefix}atmanl.nc"])
                fh_dict['copy'].append([f"{self.task_config.DATA}/sfca{hr}.nc",
                                        f"{anl_prefix}sfcanl.nc"])
            else:
                fh_dict['copy'].append([f"{self.task_config.DATA}/atma{hr}.nc",
                                        f"{anl_prefix}atma{hr}.nc"])
                fh_dict['copy'].append([f"{self.task_config.DATA}/sfca{hr}.nc",
                                        f"{anl_prefix}sfca{hr}.nc"])

        # Call FileHandler
        FileHandler(fh_dict).sync()

@logit(logger)
def add_increment(valid_time, fn_incr: str, fn_bkg: str) -> None:
    """Add increment to backgrounds

    This function will open background and increment files and add
    increment variables to the corresponding variables in the background
    file. Thus, the background file becomes and analysis file.

    Parameters
    ----------
    valid_time: datetime
        datetime object time in which analysis is valid
    fn_incr: str
        path of increment file
    fn_bkg: str
        path of background file

    Returns
    ----------
    None
    """

    try:
        with nc.Dataset(fn_incr, 'r') as nc_incr, nc.Dataset(fn_bkg, 'r+') as nc_bkg:
            # Change the units of the time coordinate since the units from the UFS history
            # file will break UPP
            time_var = nc_bkg.variables['time']
            time_var.units = valid_time.strftime('hours since %Y-%m-%dT%H:%M:%S')
            time_var[:] = 0.

            # Add increment variables to corresponding background variables
            for var in nc_incr.variables:
                if len(nc_incr[var].dimensions) == 3 or len(nc_incr[var].dimensions) == 4:
                    var_incr = nc_incr[var][:]
                    var_bkg = nc_bkg[var][:]

                    nc_bkg[var][:] = var_bkg + var_incr

    except Exception as e:
        logger.error(f"Error occurred with message {e}")
        raise
