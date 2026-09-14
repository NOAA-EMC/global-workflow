#!/usr/bin/env python3
# exglobal_prep_emissions.py
# This script creates a emissions object
# which perform the pre-processing for aerosol emissions
import os

from wxflow import Logger, cast_strdict_as_dtypedict, WorkflowException
from pygfs import ChemFireEmissions, NEXUSEmissions


# Initialize root logger
logger = Logger(
    level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)


def _check_catchem_emissions(config) -> None:
    """Validate the CATChem emissions configuration set.

    CATChem reads the anthropogenic and fire emission fields from ChemInput
    (the NEXUS_DIAG.* and FIRE_EMIS_*.nc netCDF files produced below) and maps
    them onto model tracers through its own runtime emissions YAML, staged as
    CATChem_emission.yml by CATCHEM_rc().  Confirm the selected emissions set
    exists so that an unknown CATCHEM_CONFIG fails loudly here rather than at
    forecast time.

    Parameters
    ----------
    config : dict
        Task configuration from the environment.

    Returns
    -------
    None

    Raises
    ------
    WorkflowException
        If the CATChem emissions YAML for the selected config is missing.
    """
    catchem_config = config.get('CATCHEM_CONFIG', None)
    catchem_config_dir = config.get('CATCHEM_CONFIG_DIR', None)
    if catchem_config is None or catchem_config_dir is None:
        raise WorkflowException(
            "CHEM_MODEL is 'catchem' but CATCHEM_CONFIG/CATCHEM_CONFIG_DIR "
            "are not set; check config.aero sourcing")

    emis_yaml = os.path.join(
        catchem_config_dir, f"CATChem_emissions_{catchem_config}.yaml")
    if not os.path.isfile(emis_yaml):
        raise WorkflowException(
            f"CATChem emissions configuration not found: {emis_yaml} "
            f"(CATCHEM_CONFIG='{catchem_config}')")

    logger.info(
        f"CATChem emissions: species-to-tracer mapping delegated to {emis_yaml}; "
        "NEXUS/fire preprocessing still produces the ChemInput netCDF fields")


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # When running with the CATChem chemistry model, the emission species-to-
    # tracer mapping lives in the staged CATChem emissions YAML rather than the
    # NEXUS/HEMCO .rc set.  Validate that set up front; the underlying NEXUS and
    # fire emissions still produce the ChemInput fields CATChem reads.
    if config.get('CHEM_MODEL', 'none') == 'catchem':
        _check_catchem_emissions(config)

    nxsemis = NEXUSEmissions(config.copy())
    # Instantiate the emissions pre-processing task
    fireemis = ChemFireEmissions(config.copy())
    fireemis.initialize()
    fireemis.configure()
    fireemis.execute()
    fireemis.finalize()

    nxsemis.initialize()
    nxsemis.configure()
    nxsemis.execute()
    nxsemis.finalize()
