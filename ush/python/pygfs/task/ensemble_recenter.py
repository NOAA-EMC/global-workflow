#!/usr/bin/env python3

from datetime import timedelta
from logging import getLogger
import os
from pprint import pformat
from pygfs.jedi import Jedi
from wxflow import (AttrDict, FileHandler, Task, Executable, Template, TemplateConstants,
                    add_to_datetime, to_timedelta, to_isotime, to_YMD,
                    parse_j2yaml,
                    logit)

logger = getLogger(__name__.split('.')[-1])


class EnsembleRecenter(Task):
    """
    Class for JEDI-based ensemble increment recentering
    """
    @logit(logger, name="EnsembleRecenter")
    def __init__(self, config):
        """Constructor for atmospheric ensemble increment recentering task

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
                'APREFIX': f"{self.task_config.RUN.replace('enkf', '')}.t{self.task_config.cyc:02d}z.",
                'APREFIX_ENS': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'GPREFIX': f"gdas.t{self.task_config.previous_cycle.hour:02d}z.",
                'GPREFIX_ENS': f"enkfgdas.t{self.task_config.previous_cycle.hour:02d}z.",
                'iau_times_iso': _iau_times_iso
            }
        )

        # Extend task_config with local_dict
        self.task_config = AttrDict(**self.task_config, **local_dict)

        # Create dictionary of Jedi objects
        expected_keys = ['correction_increment', 'ensemble_recenter']
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.JEDI_CONFIG_YAML, self.task_config, expected_keys)

    @logit(logger)
    def initialize(self) -> None:
        """Initialize the ensemble increment recentering task

        This method will initialize the ensemble increment recentering task.
        This includes:
        - initializing the JEDI recentering application
        - staging JEDI fix files
        - staging backgrounds and increments

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # Initialize JEDI ensemble increment recentering application
        logger.info(f"Initializing JEDI ensemble recentering applications")
        self.jedi_dict['correction_increment'].initialize(self.task_config)
        self.jedi_dict['ensemble_recenter'].initialize(self.task_config)

        # Stage fix files
        logger.info(f"Staging JEDI fix files from {self.task_config.STAGE_JEDI_FIX_YAML}")
        jedi_fix_dict = parse_j2yaml(self.task_config.STAGE_JEDI_FIX_YAML, self.task_config)
        FileHandler(jedi_fix_dict).sync()
        logger.debug(f"JEDI fix files:\n{pformat(jedi_fix_dict)}")

        # Stage background and increment files
        logger.info(f"Staging background and increment files from {self.task_config.STAGE_YAML}")
        fh_dict = parse_j2yaml(self.task_config.STAGE_YAML, self.task_config)
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
        self.jedi_dict['correction_increment'].execute()

        # Recenter increments
        self.jedi_dict['ensemble_recenter'].execute()

    @logit(logger)
    def finalize(self) -> None:
        """Finalize the ensemble increment recentering task

        This method will finalize the ensemble increment recentering task.
        This includes:
        - Move correction increment files to the comrot directory

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        fh_dict = {'copy': []}

        # create template dictionaries
        template_inc = self.task_config.COM_ATMOS_ANALYSIS_TMPL
        tmpl_inc_dict = {
            'ROTDIR': self.task_config.ROTDIR,
            'RUN': self.task_config.RUN,
            'YMD': to_YMD(self.task_config.current_cycle),
            'HH': self.task_config.current_cycle.strftime('%H')
        }

        # Copy increments to COM
        for imem in range(1, self.task_config.NMEM_ENS + 1):
            memchar = f"mem{imem:03d}"
            tmpl_inc_dict['MEMDIR'] = memchar
            incdir = Template.substitute_structure(template_inc, TemplateConstants.DOLLAR_CURLY_BRACE, tmpl_inc_dict.get)
            for fh in self.task_config.IAUFHRS:
                hr = format(fh, '03')
                for itile in range(6):
                    src = os.path.join(self.task_config.DATA, memchar,
                                       f"{self.task_config.APREFIX_ENS}cubed_sphere_grid_ratmi{hr}.tile{itile+1}.nc")
                    if fh == 6:
                        dest = os.path.join(incdir,
                                            f"{self.task_config.APREFIX_ENS}cubed_sphere_grid_ratminc.tile{itile+1}.nc")
                    else:
                        dest = incdir
                    fh_dict['copy'].append([src, dest])

        # Copy YAMLs to COM
        for app_name in self.jedi_dict.keys():
            src = os.path.join(self.task_config.DATA,
                               f"{app_name}.yaml")
            dest = os.path.join(self.task_config.COMOUT_CONF,
                                f"{self.task_config.APREFIX_ENS}{app_name}.yaml")
            fh_dict['copy'].append([src, dest])

        # Sync file handler
        FileHandler(fh_dict).sync()
