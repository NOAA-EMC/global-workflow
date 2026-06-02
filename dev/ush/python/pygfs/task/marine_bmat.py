#!/usr/bin/env python3

import os
import glob
from logging import getLogger
from pygfs.task.analysis import Analysis
from wxflow import (AttrDict, FileHandler, Executable,
                    add_to_datetime, to_timedelta, to_isotime,
                    chdir,
                    parse_j2yaml, parse_j2tmpl, save_as_yaml,
                    logit)

from pygfs.jedi import Jedi

logger = getLogger(__name__.split('.')[-1])


class MarineBMat(Analysis):
    """
    Class for global marine B-matrix tasks.
    """
    def __init__(self, config):
        """Constructor for marine B-matrix task

        This method will construct the marine B-matrix task object
        This includes:
        - extending the task_config AttrDict to include parameters required for this task
        - loading the task configuration YAML
        - instantiating the Jedi attribute objects

        Parameters
        ----------
        config: Dict
            dictionary object containing task configuration

        Returns
        ----------
        None
        """
        super().__init__(config)

        _calc_scale_exec = os.path.join(self.task_config.HOMEglobal, 'ush', 'python', 'soca', 'calc_scales.py')

        # compute the relative path from self.task_config.DATA to self.task_config.DATAenspert
        _enspert_relpath = os.path.relpath(self.task_config.DATAens, self.task_config.DATA)

        # Create a local dictionary that is repeatedly used across this class
        self.task_config.update(AttrDict(
            {
                'PARMmarine': os.path.join(self.task_config.PARMglobal, 'gdas', 'marine'),
                'CALC_SCALE_EXEC': _calc_scale_exec,
                'ENSPERT_RELPATH': _enspert_relpath,
                'CALC_SCALE_EXEC': _calc_scale_exec,
            }
        ))

        # Extend task_config with content of config yaml for this task
        self.task_config.update(parse_j2yaml(self.task_config.TASK_CONFIG_YAML, self.task_config))

        # Create dictionary of Jedi objects
        expected_keys = ['gridgen', 'soca_diagb', 'soca_parameters_diffusion_vt', 'soca_setcorscales',
                         'soca_parameters_diffusion_hz', 'soca_ensb', 'soca_ensweights', 'soca_chgres']
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.jedi_config, self.task_config, expected_keys)

    @logit(logger)
    def initialize(self) -> None:
        """Initialize a global B-matrix

        This method will initialize a global B-Matrix.
        This includes:
        - staging input files from COM and create output directories
        - initializing input namelists for MOM6
        - initializing the soca_vtscales Python script
        - initializing the JEDI applications

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # stage files from COM
        logger.info(f"Staging files from COM and creating input/output directories")
        FileHandler(self.task_config.data_in).sync()

        # prepare the deterministic MOM6 input.nml
        parse_j2tmpl(os.path.join(self.task_config.PARMmarine, 'mom_input.nml.j2'),
                     self.task_config,
                     output_file="mom_input.nml")

        # prepare the input.nml for the analysis geometry
        parse_j2tmpl(os.path.join(self.task_config.PARMmarine, 'mom_input_anlgeom.nml.j2'),
                     self.task_config,
                     output_file="./anl_geom/mom_input.nml")

        # initialize vtscales python script
        vtscales_config = self.jedi_dict['soca_parameters_diffusion_vt'].render_jcb_template('soca_vtscales')
        save_as_yaml(vtscales_config, os.path.join(self.task_config.DATA, 'soca_vtscales.yaml'))

        # initialize JEDI applications
        self.jedi_dict['gridgen'].initialize()
        self.jedi_dict['soca_diagb'].initialize()
        self.jedi_dict['soca_chgres'].initialize()
        self.jedi_dict['soca_parameters_diffusion_vt'].initialize()
        self.jedi_dict['soca_setcorscales'].initialize()
        self.jedi_dict['soca_parameters_diffusion_hz'].initialize()
        if self.task_config.DOHYBVAR_OCN == "YES" or self.task_config.NMEM_ENS >= 2:
            self.jedi_dict['soca_ensb'].initialize()
            self.jedi_dict['soca_ensweights'].initialize()

    @logit(logger)
    def execute(self) -> None:
        """Generate the full B-matrix

        This method will generate the full B-matrix according to the configuration.
        This includes:
        - running all JEDI application and Python scripts required to generate the B-matrix

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # soca grid generation
        self.jedi_dict['gridgen'].execute()

        # variance partitioning
        self.jedi_dict['soca_diagb'].execute()

        # Interpolate f009 bkg to analysis geometry
        self.jedi_dict['soca_chgres'].execute()

        # horizontal diffusion
        self.jedi_dict['soca_setcorscales'].execute()
        self.jedi_dict['soca_parameters_diffusion_hz'].execute()

        # vertical diffusion
        exec_cmd = Executable("python")
        exec_name = self.task_config.CALC_SCALE_EXEC
        exec_cmd.add_default_arg(exec_name)
        exec_cmd.add_default_arg('soca_vtscales.yaml')
        self.run(exec_cmd)

        self.jedi_dict['soca_parameters_diffusion_vt'].execute()

        # hybrid EnVAR case
        if self.task_config.DOHYBVAR_OCN == "YES" or self.task_config.NMEM_ENS >= 2:
            self.jedi_dict['soca_ensb'].execute()
            self.jedi_dict['soca_ensweights'].execute()

    @logit(logger)
    def finalize(self) -> None:
        """Finalize the global B-matrix job

        This method will finalize the global B-matrix job.
        This includes:
        - saving output files and YAMLs to COM

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # save files from COM
        logger.info(f"Saving files to COM")
        FileHandler(self.task_config.data_out).sync()
