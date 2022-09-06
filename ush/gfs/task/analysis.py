from gfs.task.base import Task
import logging
import os
import shutil

class Analysis(Task):
    """
    Parent class for global analysis tasks
    """
    def __init__(self, config):
        super().__init__(config)
        # for now config is assumed to be os.environ
        self.fv3jedi_fix = config['FV3JEDI_FIX']

    def initialize(self):
        super().initialize()

    def execute(self):
        super().execute()

    def finalize(self):
        super().finalize()

    def stage_obs(self, filedict):
        logging.info('Staging observations')
        self.stage(filedict)
        logging.info('Finished staging observations')

    def stage_fix(self, filedict):
        logging.info('Staging fix files')
        self.stage(filedict)
        logging.info('Finished staging fix files')

    def stage_berror(self, filedict):
        logging.info('Only using identity B for now... no staging performed')

    def stage(self, filedict):
        for src, dest in filedict.items():
            logging.info(f'Copying {src} to {dest}')
            if not os.path.exists(os.path.dirname(dest)):
                os.makedirs(os.path.dirname(dest))
            if os.path.exists(dest):
                os.remove(dest)
            shutil.copyfile(src, dest)

class AerosolAnalysis(Analysis):
    """
    Class for global aerosol analysis tasks
    """
    def __init__(self, config):
        super().__init__(config)

    def initialize(self):
        super().initialize()
        fix_dict = self.get_fix_file_dict()
        self.stage_fix(fix_dict)
        crtm_fix_dict = self.get_crtm_coeff_dict()
        self.stage_crtm(crtm_fix_dict)
        self.stage_berror({})
        obs_dict = self.get_obs_dict()
        self.stage_obs(obs_dict)
        bkg_dict = self.get_bkg_dict()
        self.stage_bkg(bkg_dict)

    def execute(self):
        super().execute()

    def finalize(self):
        super().finalize()

    def stage_crtm(self, filedict):
        logging.info('Staging CRTM coefficient files')
        self.stage(filedict)
        logging.info('Finished staging CRTM coefficient files')

    def get_crtm_coeff_dict(self):
        coeff_file_dict = {
            os.path.join(self.fv3jedi_fix,
                         'crtm', '2.3.0',
                         'AerosolCoeff.bin'): os.path.join(self.datadir,
                                                           'crtm',
                                                           'AerosolCoeff.bin'),
            os.path.join(self.fv3jedi_fix,
                         'crtm', '2.3.0',
                         'CloudCoeff.bin'): os.path.join(self.datadir,
                                                           'crtm',
                                                           'CloudCoeff.bin'),
            # Note: Ideally we would only copy files for platforms used
            # but since it is just 2 VIIRS platforms, just copy them both regardless
            # We will fix this when there is a solution for ATM mode
            os.path.join(self.fv3jedi_fix,
                         'crtm', '2.3.0',
                         'v.viirs-m_npp.SpcCoeff.bin'): os.path.join(self.datadir,
                                                                     'crtm',
                                                                     'v.viirs-m_npp.SpcCoeff.bin'),
            os.path.join(self.fv3jedi_fix,
                         'crtm', '2.3.0',
                         'v.viirs-m_npp.TauCoeff.bin'): os.path.join(self.datadir,
                                                                     'crtm',
                                                                     'v.viirs-m_npp.TauCoeff.bin'),
            os.path.join(self.fv3jedi_fix,
                         'crtm', '2.3.0',
                         'v.viirs-m_j01.SpcCoeff.bin'): os.path.join(self.datadir,
                                                                     'crtm',
                                                                     'v.viirs-m_j01.SpcCoeff.bin'),
            os.path.join(self.fv3jedi_fix,
                         'crtm', '2.3.0',
                         'v.viirs-m_j01.TauCoeff.bin'): os.path.join(self.datadir,
                                                                     'crtm',
                                                                     'v.viirs-m_j01.TauCoeff.bin'),
            # do we need other files? NPOESS/FASTEM6/USGS/etc.? Test to find out?
            }
        return coeff_file_dict


    def get_fix_file_dict(self):
        fix_file_dict = {
            os.path.join(self.fv3jedi_fix,
                         'fv3jedi', 'fv3files',
                         f'akbk{self.nlevs}.nc4'): os.path.join(self.datadir,
                                                                'fv3jedi',
                                                                'akbk.nc4'),
            os.path.join(self.fv3jedi_fix,
                         'fv3jedi', 'fv3files',
                         'fmsmpp.nml'): os.path.join(self.datadir,
                                                     'fv3jedi',
                                                     'fmsmpp.nml'),
            os.path.join(self.fv3jedi_fix,
                         'fv3jedi', 'fv3files',
                         'field_table_gfdl'): os.path.join(self.datadir,
                                                           'fv3jedi',
                                                           'field_table'),
            os.path.join(self.fv3jedi_fix,
                         'fv3jedi', 'fieldmetadata',
                         'gfs-aerosol.yaml'): os.path.join(self.datadir,
                                                           'fv3jedi',
                                                           'field_table'),
            }
        return fix_file_dict
