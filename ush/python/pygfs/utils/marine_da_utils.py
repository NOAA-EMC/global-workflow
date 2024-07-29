import f90nml
import os
from logging import getLogger
import xarray as xr

from wxflow import (FileHandler,
                    logit,
                    WorkflowException,
                    AttrDict,
                    parse_j2yaml,
                    Executable,
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
        raise OSError(f"Failed to execute {exec_cmd}")
    except Exception:
        raise WorkflowException(f"An error occured during execution of {exec_cmd}")


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
    """Prepare the input.nml file
       TODO: Use jinja2 instead of f90nml
    """
    # stage input.nml
    mom_input_nml_tmpl_src = os.path.join(task_config.HOMEgdas, 'parm', 'soca', 'fms', 'input.nml')
    mom_input_nml_tmpl = os.path.join(task_config.DATA, 'mom_input.nml.tmpl')
    FileHandler({'copy': [[mom_input_nml_tmpl_src, mom_input_nml_tmpl]]}).sync()

    # swap date and stacksize
    domain_stack_size = task_config.DOMAIN_STACK_SIZE
    ymdhms = [int(s) for s in task_config.MARINE_WINDOW_END.strftime('%Y,%m,%d,%H,%M,%S').split(',')]
    with open(mom_input_nml_tmpl, 'r') as nml_file:
        nml = f90nml.read(nml_file)
        nml['ocean_solo_nml']['date_init'] = ymdhms
        nml['fms_nml']['domains_stack_size'] = int(domain_stack_size)
        nml.write('mom_input.nml')


@logit(logger)
def cice_hist2fms(input_filename: str, output_filename: str) -> None:
    """ Reformat the CICE history file so it can be read by SOCA/FMS
    Simple reformatting utility to allow soca/fms to read the CICE history files
    """

    # open the CICE history file
    ds = xr.open_dataset(input_filename)

    if 'aicen' in ds.variables and 'hicen' in ds.variables and 'hsnon' in ds.variables:
        logger.info(f"*** Already reformatted, skipping.")
        return

    # rename the dimensions to xaxis_1 and yaxis_1
    ds = ds.rename({'ni': 'xaxis_1', 'nj': 'yaxis_1'})

    # rename the variables
    ds = ds.rename({'aice_h': 'aicen', 'hi_h': 'hicen', 'hs_h': 'hsnon'})

    # Save the new netCDF file
    ds.to_netcdf(output_filename, mode='w')


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
