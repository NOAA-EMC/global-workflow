from logging import getLogger
from wxflow import Executable, WorkflowException, save_as_yaml, parse_j2yaml
from os.path import join

logger = getLogger(__name__.split('.')[-1])


def run_nc2ioda(task_config: dict, obs_space: str, context: dict) -> int:
    """
    Executes the nc2ioda conversion process using a Jinja2 template and a YAML configuration.

    Args:
        task_config (dict): Configuration dictionary containing paths and settings for the task.
        obs_space (str): Observation space identifier used to generate file paths.
        context (dict): Context dictionary with variables to render the Jinja2 template.

    Returns:
        int: Returns 0 upon successful execution. Logs errors and warnings for failures.
    """
    jinja_template = join(task_config['HOMEglobal'], "parm", "chem", "nc2ioda.yaml.j2")
    yaml_config = parse_j2yaml(jinja_template, context)
    nc2ioda_yaml = join(task_config['DATA'], obs_space, f"{obs_space}_nc2ioda.yaml")
    save_as_yaml(yaml_config, nc2ioda_yaml)

    # Run the ioda converter
    nc2ioda_exe = join(task_config['HOMEglobal'], 'exec', 'gdas_obsprovider2ioda.x')
    exe = Executable(nc2ioda_exe)
    exe.add_default_arg([nc2ioda_yaml])
    try:
        logger.debug(f"Executing {exe}")
        exe(cwd=task_config['DATA'])
    except OSError:
        logger.exception(f"Failed to execute {exe}")
        raise
    except Exception as err:
        logger.exception(f"An error occurred during execution of {exe}")
        raise WorkflowException(f"An error occurred during execution of {exe}") from err
