#!/usr/bin/env python3

from logging import getLogger
import netCDF4 as nc
from pprint import pformat
from pygfs.jedi import Jedi
from wxflow import (AttrDict, FileHandler, Task,
                    parse_j2yaml,
                    to_timedelta, add_to_datetime, to_fv3time, to_isotime,
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

        _iau_times_iso = []
        for hour in self.task_config.IAUFHRS:
            _iau_times_iso.append(to_isotime(_window_begin + to_timedelta(f"{str(hour)}H") - to_timedelta(f"{self.task_config.assim_freq}H") / 2))

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
                'APREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'GPREFIX': f"gdas.t{self.task_config.previous_cycle.hour:02d}z.",
                'iau_times_iso': _iau_times_iso
            }
        )

        # Extend task_config with local_dict
        self.task_config = AttrDict(**self.task_config, **local_dict)

        # Create dictionary of Jedi objects
        expected_keys = ['atm_add_increments']
        if self.task_config.DO_AERO_ANL:
            expected_keys.append('aero_add_increments')
        if self.task_config.DO_JEDISNOWDA:
            expected_keys.append('snow_add_increments')
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.JEDI_CONFIG_YAML, self.task_config, expected_keys)

    @logit(logger)
    def initialize(self) -> None:
        """Initialize the analysis calculation task

        This method will initialize the analysis calculation task.
        This includes:
        - initializing the JEDI add_increments application
        - staging JEDI fix files
        - staging backgrounds and increments

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # Initialize GDASApp JEDI add_increments application
        logger.info(f"Initializing GDASApp JEDI add_increments applications")
        self.jedi_dict['atm_add_increments'].initialize(self.task_config)
        if self.task_config.DO_AERO_ANL:
            self.jedi_dict['aero_add_increments'].initialize(self.task_config)
        if self.task_config.DO_JEDISNOWDA:
            self.jedi_dict['snow_add_increments'].initialize(self.task_config)

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

        This method will execute the analysis calculation task. This includes:
        - Running the add_increments applications to compute the analysis variables
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
        self.jedi_dict['atm_add_increments'].execute()
        if self.task_config.DO_AERO_ANL:
            self.jedi_dict['aero_add_increments'].execute()
        if self.task_config.DO_JEDISNOWDA:
            self.jedi_dict['snow_add_increments'].execute()

        # Loop through forecast hours
        for fh in self.task_config.IAUFHRS:
            hr = format(fh, '03')
            valid_time = add_to_datetime(self.task_config.current_cycle, to_timedelta(hr))
            auxgrid_time_str = to_fv3time(valid_time).replace('.', '_') + 'z'

            # Atmosphere
            logger.info(f"Adding atmospheric increment to background for forecast hour {hr}")
            insert_analysis_variables(valid_time,
                                      f"atma{hr}.{auxgrid_time_str}.nc4",
                                      f"{self.task_config.GPREFIX}atmf{hr}.nc")

            # Aerosols
            if self.task_config.DO_AERO_ANL:
                logger.info(f"Adding aerosol increment to background for forecast hour {hr}")
                insert_analysis_variables(valid_time,
                                          f"aeroa{hr}.{auxgrid_time_str}.nc4",
                                          f"{self.task_config.GPREFIX}atmf{hr}.nc")

            # Snow
            if self.task_config.DO_JEDISNOWDA:
                logger.info(f"Adding snow increment to background for forecast hour {hr}")
                insert_analysis_variables(valid_time,
                                          f"snowa{hr}.{auxgrid_time_str}.nc4",
                                          f"{self.task_config.GPREFIX}sfcf{hr}.nc")

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
        src_prefix = f"{self.task_config.DATA}/{self.task_config.GPREFIX}"
        dest_prefix = f"{self.task_config.COMOUT_ATMOS_ANALYSIS}/{self.task_config.APREFIX}"
        for fh in self.task_config.IAUFHRS:
            hr = format(fh, '03')
            if fh == 6:
                fh_dict['copy'].append([f"{src_prefix}atmf{hr}.nc",
                                        f"{dest_prefix}atmanl.nc"])
                fh_dict['copy'].append([f"{src_prefix}sfcf{hr}.nc",
                                        f"{dest_prefix}sfcanl.nc"])
            else:
                fh_dict['copy'].append([f"{src_prefix}atmf{hr}.nc",
                                        f"{dest_prefix}atma{hr}.nc"])
                fh_dict['copy'].append([f"{src_prefix}sfcf{hr}.nc",
                                        f"{dest_prefix}sfca{hr}.nc"])

        # Call FileHandler
        FileHandler(fh_dict).sync()


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
