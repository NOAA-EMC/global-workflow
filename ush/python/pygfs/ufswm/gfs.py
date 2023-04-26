import os
import logging
from pprint import pformat

from pygw.attrdict import AttrDict
from pygw.yaml_file import parse_yamltmpl
from pygw.logger import logit
from pygfs.ufswm.ufs import UFS

logger = logging.getLogger(__name__.split(".")[-1])


class GFS(UFS):
    @logit(logger, name="GFS")
    def __init__(self, config):

        super().__init__("GFS", config, HOMEufs=config["HOMEgfs"])

        # Start putting fixed properties of the GFS in the ufs_model container
        self.ufs_model.ntiles = 6

        # Determine coupled/uncoupled from config and define as appropriate
        self.build_ufs_model()

        # Is this an ensemble member?
        self.member = self._config.get("MEMBER", None)

    @logit(logger)
    def build_ufs_model(self) -> None:
        """
        Description
        -----------
        This method extracts configuration directly from `self._config` and maps it to `self.ufs_model`
        TODO: This method could be broken up into smaller methods for each component, but maintain this as the entry point

        Parameters
        ----------
        None
        Returns
        -------
        None
        """

        self._get_fix_info()  # Get the fix directory info
        self._get_res_info()  # Get the model geometry etc.

        # Make some static decisions (these will be based on data soon!)
        self.ufs_model.warm_start = self.is_warm_start()
        self.ufs_model.do_iau = False

        # Get model dates relevant for this run
        self._get_date_info()

        logger.debug(f"UFS-weather-model configuration:\n{pformat(self.ufs_model)}")

    @logit(logger)
    def _get_fix_info(self) -> None:
        self.ufs_model.fix = AttrDict()
        self.ufs_model.fix.FIX_aer = os.path.join(self.ufs_model.HOMEufs, "fix", "aer")
        self.ufs_model.fix.FIX_am = os.path.join(self.ufs_model.HOMEufs, "fix", "am")
        self.ufs_model.fix.FIX_lut = os.path.join(self.ufs_model.HOMEufs, "fix", "lut")
        self.ufs_model.fix.FIX_orog = os.path.join(self.ufs_model.HOMEufs, "fix", "orog")
        self.ufs_model.fix.FIX_ugwd = os.path.join(self.ufs_model.HOMEufs, "fix", "ugwd")

    @logit(logger)
    def _get_res_info(self) -> None:
        # TODO: break this into smaller methods for atmos, ocean, etc.
        self.ufs_model.atm_res = self._config.get("CASE", "C96")
        self.ufs_model.atm_levs = self._config.get("LEVS", 127)
        self.ufs_model.ocn_res = str(self._config.get("OCNRES", 100))

    @logit(logger)
    def get_start_info(self) -> None:

        # These will be replaced with data driven decisions after COM reorg PR goes in
        self.ufs_model.warm_start = self.is_warm_start()
        self.ufs_model.do_iau = False

        self._get_date_info()

    @logit(logger)
    def is_warm_start(self) -> bool:
        """
        Description
        -----------
        This method determines if the current run is a warm start or a cold start

        Returns
        -------
        is_warm_start: bool
            Returns True if this is a warm start, False otherwise
        """
        # TODO: This method will be replaced with a data driven decision after the COM reorg PR goes in
        return False

    @logit(logger)
    def is_a_rerun(self) -> bool:
        """
        Description
        -----------
        This method determines if the current run is a continuation of a previous run

        Returns
        -------
        is_a_rerun: bool
            Returns True if this is a rerun, False otherwise
        """
        # A rerun is still a warm start, but not with IAU
        self.ufs_model.do_iau = False

        # TODO:
        #   This method will be replaced with a data driven decision after the COM reorg PR goes in
        return False

    @logit(logger)
    def _get_date_info(self) -> None:
        """
        Description
        -----------
        This method determines the attributes listed below for the UFS-weather-model
        based on the configuration

        The following attributes are set in self.ufs_model:
            start_date : datetime.datetime
            current_date : datetime.datetime
            iau_offset : int
            fhrot : int

        Parameters
        ----------
        None

        Returns
        -------
        None
        """

        if self.ufs_model.warm_start:  # warm start
            if self.is_a_rerun():  # This is a rerun restart from an intermediate failure point
                # TODO: To be implemented
                raise NotImplementedError("Rerun restarts are not yet implemented!")
            else:  # This is a warm start from the beginning
                if self.ufs_model.do_iau:
                    start_date = self.ufs_model.previous_cycle
                    current_date = self.ufs_model.current_cycle
                    iau_offset = self._config.IAU_OFFSET
                    fhrot = iau_offset / 2
                else:
                    start_date = self.ufs_model.current_cycle
                    current_date = self.ufs_model.current_cycle
                    iau_offset = 0
                    fhrot = 0
        else:  # cold start
            start_date = self.ufs_model.current_cycle
            current_date = self.ufs_model.current_cycle
            iau_offset = 0
            fhrot = 0

        self.ufs_model.start_date = start_date
        self.ufs_model.current_date = current_date
        self.ufs_model.iau_offset = iau_offset
        self.ufs_model.fhrot = fhrot

        logger.debug("Returning from _get_date_info() with:")
        for key in ["start_date", "current_date", "iau_offset", "fhrot"]:
            logger.debug(f"\tself.ufs_model.{key} = {getattr(self.ufs_model, key)}")

    @logit(logger)
    def prepare_DATA(self) -> None:
        """
        Prepare the DATA directory for the GFS
        Reads the `stage` section of UFS_CONFIG_FILE and creates the necessary directories
        """

        localconf = AttrDict()

        localconf.DATA = self._config.DATA

        data = parse_yamltmpl(self.ufs_model.yaml_config, localconf)
        self.stage(data.stage)

    @logit(logger)
    def stage_fix(self) -> None:
        """
        Stage the fix files for the GFS
        Reads the `fix` section of UFS_CONFIG_FILE and stages the files into DATA/
        """

        localconf = AttrDict()
        localconf.HOMEgfs = self._config.HOMEgfs
        localconf.DATA = self._config.DATA

        # All the various fix directories
        for key, value in self.ufs_model.fix.items():
            localconf[key] = value

        # various resolution stuff that is needed
        localconf.atm_res = self.ufs_model.atm_res
        localconf.ocn_res = self.ufs_model.ocn_res

        data = parse_yamltmpl(self.ufs_model.yaml_config, localconf)
        self.stage(data.fix)

    @logit(logger)
    def stage_tables(self, table, target) -> None:

        run = self._config.get("RUN", "gdas")

        localconf = AttrDict()
        localconf.HOMEgfs = self._config.HOMEgfs
        data = parse_yamltmpl(self.ufs_model.yaml_config, localconf).get(table)
        tables = data.get(run, data.get("default"))

        # Loop over the tables and concatenate into the target
        with open(target, "w") as fh:
            for tt in tables:
                with open(tt, "r") as fih:
                    fh.write(fih.read())

    @logit(logger)
    def generate_diag_table(self) -> None:
        """
        Generate the diag_table file from the template
        """

        localconf = AttrDict()
        localconf.SYEAR = self.ufs_model.start_date.year
        localconf.SMONTH = self.ufs_model.start_date.month
        localconf.SDAY = self.ufs_model.start_date.day
        localconf.SHOUR = self.ufs_model.start_date.hour

        diag_table = os.path.join(self._config.DATA, "diag_table")
        self.parse_ufs_templates(diag_table + ".tmpl", diag_table, localconf)

    @logit(logger)
    def prepare_input_nml(self: UFS) -> AttrDict:
        """
        Description
        -----------

        Prepare `input.nml`.

        Returns
        -------

        cfg: AttrDict

            A Python dictionary containing the configuration
            attributes relevant for `input.nml`.

        """

        # Deine the configuration variables required to build the
        # `input.nml`` file.
        cfg = AttrDict()

        # TODO: Optional variables which may be set via `schema`.
        nml_var_dict = {"CCPP_SUITE": self._config.CCPP_SUITE,
                        "MAX_OUTPUT_FIELDS": 300,
                        "INPES": self._config.layout_x,
                        "JNPES": self._config.layout_y,
                        "NPX": self._config.npx,
                        "NPY": self._config.npy,
                        "NPZ": self._config.npz,
                        "MAKE_NH": self._config.make_nh,
                        "NA_INIT": self._config.na_init,
                        "DNATS": self._config.dnats,
                        "EXTERNAL_IC": self._config.external_ic,
                        "NGGPS_IC": self._config.nggps_ic,
                        "MOUNTAIN": self._config.mountain,
                        "WARM_START": self._config.warm_start,
                        "READ_INCREMENT": self._config.read_increment,
                        "RES_LATLON_DYNAMICS": self._config.res_latlon_dynamics,
                        "NPZP": self._config.NLEVS,
                        "FHZERO": self._config.FHZER,
                        "LDIAG3D": False,  # TODO: default value
                        "QDIAG3D": False,  # TODO: default value
                        "FHCYC": self._config.FHCYC,
                        "IAER": self._config.IAER,
                        "IOVR": 3,  # TODO: default value
                        "LHEATSTRG": False,  # TODO: default value
                        "LSEASPRAY": True,  # TODO: default value
                        "RANDOM_CLDS": True,  # TODO: default value
                        "CNVCLD": True,  # TODO: default value
                        "IMFSHALCNV": 2,  # TODO: default value
                        "IMFDEEPCNV": 2,  # TODO: default value
                        "RAS": False,  # TODO: default value
                        "CDMBWD": [3.5, 0.25],  # TODO: default value
                        "CPLCHM": False,  # TODO: default value
                        "CPLWAV": False,  # TODO: default value
                        "CPLWAV2ATM": False,  # TODO: default value
                        "DO_SPPT": False,  # TODO: default value
                        "DO_SHUM": False,  # TODO: default value
                        "DO_SKEB": False,  # TODO: default value
                        "N_VAR_LNDP": 0,  # TODO: default value
                        "FSCAV_AERO": "*0.0",  # TODO: default value
                        "DO_RRTMG": False,  # TODO: default value
                        "DOGP_CLDOPTICS_LUT": False,  # TODO: default value
                        "DOGP_LWSCAT": False,  # TODO: default value
                        "PROGSIGMA": True,  # TODO: default value
                        "FNALBC": self._config.FNALBC,
                        "FNVETC": self._config.FNVETC,
                        "FNSOTC": self._config.FNSOTC,
                        "FNSMCC_control": self._config.FNSMCC,
                        "FNMSKH_control": self._config.FNMSKH,
                        "FNABSC": self._config.FNABSC,
                        "STOCHINI": None,  # TODO: This currently does not have a default value.
                        "SKEB": self._config.SKEB,
                        "SHUM": self._config.SHUM,
                        "SPPT": self._config.SPPT,
                        "LNDP_TYPE": self._config.lndp_type,
                        "LNDP_MODEL_TYPE": None,  # TODO: This currently does not have a default value.
                        "LNDP_VAR_LIST": self._config.lndp_var_list,
                        "LNDP_PRT_LIST": self._config.lndp_prt_list
                        }

        for (key, value) in nml_var_dict.items():
            setattr(cfg, key, value)

        return cfg

    @logit(logger)
    def prepare_model_configure(self) -> None:
        """
        Prepare model_configure related attributes etc.
        """
        self.mdl_config()

    @logit(logger)
    def prepare_nems_configure(self: UFS) -> AttrDict:
        """
        Description
        -----------

        Prepare `nems.configure`.

        Returns
        -------

        cfg: AttrDict

            A Python dictionary containing the configuration
            attributes relevant for `nems.configure`.

        """

        # Define the configuration variables required to build the
        # `nems.configure``.
        cfg = AttrDict()

        # TODO: Populate this dictionary as configurations are added.
        nems_var_dict = {
            "atm_model": "fv3",
            "atm_omp_num_threads": self._config.ATMTHREADS,
            "atm_petlist_bounds": self._config.ATMPETS,
        }

        for (key, value) in nems_var_dict.items():
            setattr(cfg, key, value)

        return cfg
