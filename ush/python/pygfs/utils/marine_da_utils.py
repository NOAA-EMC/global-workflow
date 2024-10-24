from datetime import datetime, timedelta
import dateutil.parser as dparser
import os
from netCDF4 import Dataset
from logging import getLogger
import yaml

from wxflow import (FileHandler,
                    logit,
                    WorkflowException,
                    AttrDict,
                    parse_j2yaml,
                    Executable,
                    save_as_yaml,
                    jinja)

logger = getLogger(__name__.split('.')[-1])


@logit(logger)
def run(exec_cmd: Executable) -> None:
    """Run the executable command
    """
    logger.info(f"Executing {exec_cmd}")
    try:
        logger.debug(f"Executing {exec_cmd}")
        exec_cmd()
    except OSError:
        raise OSError(f"FATAL ERROR: Failed to execute {exec_cmd}")
    except Exception:
        raise WorkflowException(f"FATAL ERROR: Error occurred during execution of {exec_cmd}")


@logit(logger)
def link_executable(task_config: AttrDict, exe_name: str) -> None:
    """Link the executable to the DATA directory
    """
    logger.info(f"Link executable {exe_name}")
    logger.warn("WARNING: Linking is not permitted per EE2.")
    exe_src = os.path.join(task_config.EXECgfs, exe_name)
    exe_dest = os.path.join(task_config.DATA, exe_name)
    if os.path.exists(exe_dest):
        os.remove(exe_dest)
    os.symlink(exe_src, exe_dest)


@logit(logger)
def prep_input_nml(task_config: AttrDict) -> None:
    """Prepare the mom_input.nml file
    """
    # stage input.nml.j2
    mom_input_nml_tmpl_src = os.path.join(task_config.PARMsoca, 'fms', 'input.nml.j2')
    mom_input_nml_tmpl = os.path.join(task_config.DATA, 'mom_input.nml.tmpl')
    FileHandler({'copy': [[mom_input_nml_tmpl_src, mom_input_nml_tmpl]]}).sync()

    # swap date and stacksize
    date_init = [int(s) for s in task_config.MARINE_WINDOW_END.strftime('%Y,%m,%d,%H,%M,%S').split(',')]
    input_nml_config = {'domain_stack_size': task_config.DOMAIN_STACK_SIZE,
                        'date_init': date_init}
    jinja_input_nml = jinja.Jinja(mom_input_nml_tmpl, input_nml_config)
    jinja_input_nml.save('mom_input.nml')


@logit(logger)
def stage_ens_mem(task_config: AttrDict) -> None:
    """ Copy the ensemble members to the DATA directory
    Copy the ensemble members to the DATA directory and reformat the CICE history files
    """
    # Copy the ensemble members to the DATA directory
    logger.info("---------------- Stage ensemble members")
    ensbkgconf = AttrDict(task_config)
    ensbkgconf.RUN = task_config.GDUMP_ENS
    logger.debug(f"{jinja.Jinja(task_config.MARINE_ENSDA_STAGE_BKG_YAML_TMPL, ensbkgconf).render}")
    letkf_stage_list = parse_j2yaml(task_config.MARINE_ENSDA_STAGE_BKG_YAML_TMPL, ensbkgconf)
    logger.info(f"{letkf_stage_list}")
    FileHandler(letkf_stage_list).sync()


@logit(logger)
def test_hist_date(histfile: str, ref_date: datetime) -> None:
    """
    Check that the date in the MOM6 history file is the expected one for the cycle.
    TODO: Implement the same for seaice
    """

    ncf = Dataset(histfile, 'r')
    hist_date = dparser.parse(ncf.variables['time'].units, fuzzy=True) + timedelta(hours=int(ncf.variables['time'][0]))
    ncf.close()
    logger.info(f"*** history file date: {hist_date} expected date: {ref_date}")

    if hist_date != ref_date:
        raise ValueError(f"FATAL ERROR: Inconsistent bkg date'")


@logit(logger)
def gen_bkg_list(bkg_path: str, window_begin=' ', yaml_name='bkg.yaml', ice_rst=False) -> None:
    """
    Generate a YAML of the list of backgrounds for the pseudo model
    """

    # Pseudo model parameters (time step, start date)
    # TODO: make this a parameter
    dt_pseudo = 3
    bkg_date = window_begin

    # Construct list of background file names
    cyc = str(os.getenv('cyc')).zfill(2)
    gcyc = str((int(cyc) - 6) % 24).zfill(2)  # previous cycle
    fcst_hrs = list(range(6, 10, dt_pseudo))
    files = []
    for fcst_hr in fcst_hrs:
        files.append(os.path.join(bkg_path, f"ocean.bkg.f{str(fcst_hr).zfill(3)}.nc"))

    # Identify the ocean background that will be used for the  vertical coordinate remapping
    ocn_filename_ic = './INPUT/MOM.res.nc'
    test_hist_date(ocn_filename_ic, bkg_date)  # assert date of the history file is correct

    # Copy/process backgrounds and generate background yaml list
    bkg_list = []
    for bkg in files:
        logger.info(f"****************** bkg: {bkg}")
        # assert validity of the ocean bkg date, remove basename
        bkg_date = bkg_date + timedelta(hours=dt_pseudo)
        test_hist_date(bkg, bkg_date)
        ocn_filename = os.path.splitext(os.path.basename(bkg))[0] + '.nc'

        # prepare the seaice background, aggregate if the backgrounds are CICE restarts
        ice_filename = ocn_filename.replace("ocean", "ice")

        # prepare list of ocean and ice bkg to be copied to RUNDIR
        bkg_dict = {'date': bkg_date.strftime('%Y-%m-%dT%H:%M:%SZ'),
                    'basename': './bkg/',
                    'ocn_filename': ocn_filename,
                    'ice_filename': ice_filename,
                    'read_from_file': 1}

        bkg_list.append(bkg_dict)

    # save pseudo model yaml configuration
    save_as_yaml(bkg_list, yaml_name)


@logit(logger)
def clean_empty_obsspaces(config, target, app='var'):
    """
    Remove obs spaces that point to non-existent file and save
    """

    # obs space dictionary depth is dependent on the application
    if app == 'var':
        obs_spaces = config['cost function']['observations']['observers']
    else:
        raise ValueError(f"FATAL ERROR: obs space cleaning not implemented for {app}")

    # remove obs spaces that point to a non existant file
    cleaned_obs_spaces = []
    for obs_space in obs_spaces:
        fname = obs_space['obs space']['obsdatain']['engine']['obsfile']
        if os.path.isfile(fname):
            cleaned_obs_spaces.append(obs_space)
        else:
            logger.info(f"WARNING: {fname} does not exist, removing obs space")

    # update obs spaces
    config['cost function']['observations']['observers'] = cleaned_obs_spaces

    # save cleaned yaml
    save_as_yaml(config, target)


@logit(logger)
def get_mom6_levels(ocnres: str) -> int:
    """
    Temporary function that returns the number of vertical levels in MOM6 given the horizontal resolution.
    This is requiered by the diffusion saber block that now makes use of oops::util::FieldSetHelpers::writeFieldSet
    and requires the number of levels in the configuration. I have been told this will be changed in the future.

    Parameters
    -----------
    ocnres: str
        Input resolution for ocean in str format. e.g. '500', '100', '050', '025'

   Returns
   -------
   nlev: int
       number of levels in the ocean model given an input resolution
    """

    # Currently implemented resolutions
    ocnres_to_nlev = {
        '500': 25,
        '100': 75,
        '050': 75,
        '025': 75
    }
    try:
        nlev = ocnres_to_nlev.get(ocnres)
    except KeyError:
        raise KeyError("FATAL ERROR: Invalid ocnres value. Aborting.")

    return nlev
