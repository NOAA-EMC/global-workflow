from pygfs.task.analysis import Analysis
import copy
import glob
import gzip
import netCDF4 as nc
import tarfile
import ufsda # temporary until this is in workflow
import yaml


class AerosolAnalysis(Analysis):
    """
    Class for global aerosol analysis tasks
    """
    def __init__(self, config):
        super().__init__(config)
        self.yamltemplate = config['AEROVARYAML']
        self.taskname = f'{self.cdump}aeroanl'

    def initialize(self):
        super().initialize()
        # logging.info('Initializing global aerosol analysis')
        # fix_dict = self.get_fix_file_dict()
        # self.stage_fix(fix_dict)
        # crtm_fix_dict = self.get_crtm_coeff_dict()
        # self.stage_crtm(crtm_fix_dict)
        # if os.environ.get('STATICB_TYPE', 'bump_aero') in ['bump_aero']:
        #     self.stage_berror(self.get_berror_dict())
        # bkg_dict = self.get_bkg_dict()
        # self.stage_bkg(bkg_dict)
        # yaml_config = {
        #     'BKG_DIR': 'bkg',
        #     'OBS_DIR': 'obs',
        #     'DIAG_DIR': 'diags',
        #     'CRTM_COEFF_DIR': 'crtm',
        #     'OBS_PREFIX': os.environ['OPREFIX'],
        #     'fv3jedi_staticb_aero_dir': 'berror',
        #     'fv3jedi_fix_dir': 'fv3jedi',
        #     'fv3jedi_fieldmetadata_dir': 'fv3jedi',
        #     'OBS_DATE': os.environ['CDATE'],
        #     'ANL_DIR': 'anl',
        #     'INTERP_METHOD': 'barycentric',
        #     # for now making the below equal to eachother
        #     'AERO_WINDOW_LENGTH': '$(ATM_WINDOW_LENGTH)',
        #     'AERO_WINDOW_BEGIN': '$(ATM_WINDOW_BEGIN)',
        #     'window_begin': '$(ATM_WINDOW_BEGIN)',
        #     'layout_x': os.environ['layout_x'],
        #     'layout_y': os.environ['layout_y'],
        #     }
        # self.generate_yaml(yaml_config)
        # self.stage_exe()
        # # need output dir for diags and anl
        # newdirs = [
        #     os.path.join(self.datadir, 'anl'),
        #     os.path.join(self.datadir, 'diags'),
        #     ]
        # for newdir in newdirs:
        #     if not os.path.exists(newdir):
        #         os.makedirs(newdir)
        #         logging.info(f'Creating directory {newdir}')


    def execute(self):
        super().execute()

    def finalize(self):
        super().finalize()
        logging.info('Finalizing global aerosol analysis')
        #---- tar up diags
        # path of output tar statfile
        aerostat = os.path.join(os.environ['COMOUTaero'], f"{os.environ['APREFIX']}aerostat")
        # get list of diag files to put in tarball
        diags = glob.glob(os.path.join(self.datadir, 'diags', 'diag*nc4'))
        # gzip the files first
        for diagfile in diags:
            logging.info(f'Compressing {diagfile} using gzip')
            with open(diagfile, 'rb') as f_in, gzip.open(f"{diagfile}.gz", 'wb') as f_out:
                f_out.writelines(f_in)
        # open tar file for writing
        archive = tarfile.open(aerostat, "w")
        for diagfile in diags:
            archive.add(f"{diagfile}.gz")
        archive.close()
        logging.info(f'Wrote diags to {aerostat}')
        # copy full YAML from executable to ROTDIR
        src = os.path.join(self.datadir, f'{self.taskname}_{self.cdate}.yaml')
        dest = os.path.join(os.environ['COMOUTaero'], f"{os.environ['APREFIX']}aeroanl.yaml")
        if os.path.exists(dest):
            logging.info(f'{dest} already exists, removing it!')
            os.remove(dest)
        shutil.copy(src, dest)
        logging.info(f'Copied YAML file from {src} to {dest}')
        #---- NOTE below is 'temporary', eventually we will not be using FMS RESTART formatted files
        #---- all of the rest of this method will need to be changed but requires model and JEDI changes
        #---- copy RESTART fv_tracer files for future reference
        cdate_fv3 = dt.datetime.strptime(self.cdate, '%Y%m%d%H').strftime('%Y%m%d.%H%M%S')
        comin_ges = os.environ['COMIN_GES']
        # NOTE that while 'chem' is the $componenet, the aerosol fields are with the 'atmos' tracers
        comin_ges_atm = comin_ges.replace('chem', 'atmos')
        fms_bkg_file_template = os.path.join(comin_ges_atm, 'RESTART', f'{cdate_fv3}.fv_tracer.res.tile1.nc')
        for itile in range(1,7):
            bkg_path = fms_bkg_file_template.replace('tile1', f'tile{itile}')
            dest = os.path.join(os.environ['COMOUTaero'], f'aeroges.{os.path.basename(bkg_path)}')
            if os.path.exists(dest):
                os.remove(dest)
            logging.info(f'Copying RESTART {bkg_path} to archive aerosol background')
            shutil.copy(bkg_path, dest)

        #---- add increments to RESTART files
        self.add_fms_cube_sphere_increments()
        #---- move increments to ROTDIR
        fms_inc_file_template = os.path.join(self.datadir, 'anl', f'aeroinc.{cdate_fv3}.fv_tracer.res.tile1.nc')
        for itile in range(1,7):
            inc_path = fms_inc_file_template.replace('tile1', f'tile{itile}')
            dest = os.path.join(os.environ['COMOUTaero'], os.path.basename(inc_path))
            if os.path.exists(dest):
                os.remove(dest)
            logging.info(f'Copying aerosol FMS cube sphere increment to {dest}')
            shutil.copy(bkg_path, dest)

    def add_fms_cube_sphere_increments(self):
        logging.info('Adding increments to RESTART files')
        # only need the fv_tracer files
        cdate_fv3 = dt.datetime.strptime(self.cdate, '%Y%m%d%H').strftime('%Y%m%d.%H%M%S')
        fms_inc_file_template = os.path.join(self.datadir, 'anl', f'aeroinc.{cdate_fv3}.fv_tracer.res.tile1.nc')
        comin_ges = os.environ['COMIN_GES']
        # NOTE that while 'chem' is the $componenet, the aerosol fields are with the 'atmos' tracers
        comin_ges_atm = comin_ges.replace('chem', 'atmos')
        fms_bkg_file_template = os.path.join(comin_ges_atm, 'RESTART', f'{cdate_fv3}.fv_tracer.res.tile1.nc')
        incvars = ['dust1', 'dust2', 'dust3', 'dust4', 'dust5',
                   'seas1', 'seas2', 'seas3', 'seas4',
                   'so4', 'oc1', 'oc2', 'bc1', 'bc2']
        for itile in range(1,7):
            inc_path = fms_inc_file_template.replace('tile1', f'tile{itile}')
            bkg_path = fms_bkg_file_template.replace('tile1', f'tile{itile}')
            with nc.Dataset(inc_path, mode='r') as incfile:
                logging.info(f'Opening increment file {inc_path}')
                with nc.Dataset(bkg_path, mode='a') as rstfile:
                    logging.info(f'Opening RESTART file {bkg_path}')
                    for vname in incvars:
                        logging.info(f'...Adding increment for {vname}')
                        increment = incfile.variables[vname][:]
                        bkg = rstfile.variables[vname][:]
                        anl = bkg + increment
                        rstfile.variables[vname][:] = anl[:]
                        rstfile.variables[vname].delncattr('checksum') # remove the checksum so fv3 does not complain

    def stage_crtm(self, filedict):
        logging.info('Staging CRTM coefficient files')
        self.stage(filedict)
        logging.info('Finished staging CRTM coefficient files')

    def stage_exe(self):
        logging.info('Staging FV3-JEDI executable')
        src = os.environ['JEDIVAREXE']
        dest = os.path.join(self.datadir, os.path.basename(src))
        if os.path.exists(dest):
            os.remove(dest)
        os.symlink(src, dest)
        logging.info(f'Linking {src} to {dest}')
        logging.info('Finished staging FV3-JEDI executable')

    def get_bkg_dict(self):
        """
        Return dict of src/dest pairs for model backgrounds
        """
        # NOTE for now this is FV3 RESTART files and just assumed to be fh006
        comin_ges = os.environ['COMIN_GES']
        # NOTE that while 'chem' is the $componenet, the aerosol fields are with the 'atmos' tracers
        comin_ges_atm = comin_ges.replace('chem', 'atmos')
        rst_dir = os.path.join(comin_ges_atm, 'RESTART') # for now, option later?
        # date variable string format
        cdate_fv3 = dt.datetime.strptime(self.cdate, '%Y%m%d%H').strftime('%Y%m%d.%H%M%S')
        # get FV3 RESTART files, this will be a lot simpler when using history files
        ntiles = 6 # global
        # aerosol DA only needs core/tracer
        bkg_dict = {}
        basename = f'{cdate_fv3}.coupler.res'
        bkg_dict[os.path.join(rst_dir, basename)] = os.path.join(self.datadir, 'bkg', basename)
        for t in range(1,ntiles+1):
            basename = f'{cdate_fv3}.fv_core.res.tile{t}.nc'
            bkg_dict[os.path.join(rst_dir, basename)] = os.path.join(self.datadir, 'bkg', basename)
            basename = f'{cdate_fv3}.fv_tracer.res.tile{t}.nc'
            bkg_dict[os.path.join(rst_dir, basename)] = os.path.join(self.datadir, 'bkg', basename)
        return bkg_dict

    def get_berror_dict(self):
        """
        Return dict of src/dest pairs for berror
        """

        ntiles = 6 # global
        # aerosol static-B needs nicas, cor_rh, cor_rv and stddev files.
        b_dir = self.berror_dir
        berror_dict = {}
        berror_dict[os.path.join(b_dir, '20160630.000000.cor_rh.coupler.res')] = os.path.join(self.datadir, 'berror', '20160630.000000.cor_rh.coupler.res')
        berror_dict[os.path.join(b_dir, '20160630.000000.cor_rv.coupler.res')] = os.path.join(self.datadir, 'berror', '20160630.000000.cor_rv.coupler.res')
        berror_dict[os.path.join(b_dir, '20160630.000000.stddev.coupler.res')] = os.path.join(self.datadir, 'berror', '20160630.000000.stddev.coupler.res')
        for t in range(1,ntiles+1):
            berror_dict[os.path.join(b_dir, f'20160630.000000.cor_rh.fv_tracer.res.tile{t}.nc')] = os.path.join(self.datadir, 'berror', f'20160630.000000.cor_rh.fv_tracer.res.tile{t}.nc')
            berror_dict[os.path.join(b_dir, f'20160630.000000.cor_rv.fv_tracer.res.tile{t}.nc')] = os.path.join(self.datadir, 'berror', f'20160630.000000.cor_rv.fv_tracer.res.tile{t}.nc')
            berror_dict[os.path.join(b_dir, f'20160630.000000.stddev.fv_tracer.res.tile{t}.nc')] = os.path.join(self.datadir, 'berror', f'20160630.000000.stddev.fv_tracer.res.tile{t}.nc')
        nproc = ntiles * int(os.environ['layout_x']) * int(os.environ['layout_y'])
        for t in range(1,nproc+1):
            berror_dict[os.path.join(b_dir, f'nicas_aero_nicas_local_{nproc:06}-{t:06}.nc')] = os.path.join(self.datadir, 'berror', f'nicas_aero_nicas_local_{nproc:06}-{t:06}.nc')
        return berror_dict

    def get_crtm_coeff_dict(self):
        coeff_file_dict = {
            os.path.join(self.fv3jedi_fix,
                         'crtm', crtmver,
                         'AerosolCoeff.bin'): os.path.join(self.datadir,
                                                           'crtm',
                                                           'AerosolCoeff.bin'),
            os.path.join(self.fv3jedi_fix,
                         'crtm', crtmver,
                         'CloudCoeff.bin'): os.path.join(self.datadir,
                                                           'crtm',
                                                           'CloudCoeff.bin'),
            # Note: Ideally we would only copy files for platforms used
            # but since it is just 2 VIIRS platforms, just copy them both regardless
            # We will fix this when there is a solution for ATM mode
            os.path.join(self.fv3jedi_fix,
                         'crtm', crtmver,
                         'v.viirs-m_npp.SpcCoeff.bin'): os.path.join(self.datadir,
                                                                     'crtm',
                                                                     'v.viirs-m_npp.SpcCoeff.bin'),
            os.path.join(self.fv3jedi_fix,
                         'crtm', crtmver,
                         'v.viirs-m_npp.TauCoeff.bin'): os.path.join(self.datadir,
                                                                     'crtm',
                                                                     'v.viirs-m_npp.TauCoeff.bin'),
            os.path.join(self.fv3jedi_fix,
                         'crtm', crtmver,
                         'v.viirs-m_j1.SpcCoeff.bin'): os.path.join(self.datadir,
                                                                    'crtm',
                                                                    'v.viirs-m_j1.SpcCoeff.bin'),
            os.path.join(self.fv3jedi_fix,
                         'crtm', crtmver,
                         'v.viirs-m_j1.TauCoeff.bin'): os.path.join(self.datadir,
                                                                    'crtm',
                                                                    'v.viirs-m_j1.TauCoeff.bin'),
            os.path.join(self.fv3jedi_fix,
                         'crtm', crtmver,
                         'NPOESS.VISice.EmisCoeff.bin'): os.path.join(self.datadir,
                                                                      'crtm',
                                                                      'NPOESS.VISice.EmisCoeff.bin'),
            os.path.join(self.fv3jedi_fix,
                         'crtm', crtmver,
                         'NPOESS.VISland.EmisCoeff.bin'): os.path.join(self.datadir,
                                                                      'crtm',
                                                                      'NPOESS.VISland.EmisCoeff.bin'),
            os.path.join(self.fv3jedi_fix,
                         'crtm', crtmver,
                         'NPOESS.VISsnow.EmisCoeff.bin'): os.path.join(self.datadir,
                                                                      'crtm',
                                                                      'NPOESS.VISsnow.EmisCoeff.bin'),
            os.path.join(self.fv3jedi_fix,
                         'crtm', crtmver,
                         'NPOESS.VISwater.EmisCoeff.bin'): os.path.join(self.datadir,
                                                                      'crtm',
                                                                      'NPOESS.VISwater.EmisCoeff.bin'),
            }
        return coeff_file_dict


    def get_fix_file_dict(self):
        fix_file_dict = {
            os.path.join(self.fv3jedi_fix,
                         'fv3jedi', 'fv3files',
                         f'akbk{self.nlayers}.nc4'): os.path.join(self.datadir,
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
                                                           'gfs-restart.yaml'),
            }
        return fix_file_dict
