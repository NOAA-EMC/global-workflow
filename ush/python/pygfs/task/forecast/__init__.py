
from dataclasses import dataclass

from task import Task

# ----


@dataclass
class Forecast(Task):

    # This class is the base-class for all forecast models; the reason
    # and intention for creating such a base-class is two-fold:

    # - After discussions with Rahul Mahajan regarding long-term
    #   modifications to the global-workflow, including the
    #   implementation of other UFS-based forecast systems (e.g.,
    #   HAFS), maintaining a single class that can handle the common
    #   necessesities (i.e., the build of model_configure,
    #   nems.configure, etc., from templates) will improve the
    #   efficiency of the introduction of new systems.

    # - Having a single base-class will reduce the size of the modules
    #   dedicated to the respective forecast related applications.

    # Methods to be included in this class:

    # - build model_configure from templates, specific to the type of
    #   forecast (i.e., forecast-only, S2S, S2SW, DATM, etc.,).

    # - build input.nml from templates, specific to the type of
    #   forecast; this includes MOM6, CICE, and WW3 when applicable.

    # - build nems.configure from templates, specific to the type of
    #   forecast.

    # - build diag, data, and other necessary tables as required by
    #   the respective forecast applications.

    # - build the directory tree as required by the respective
    #   forecast applications.

    # - Link both input and fixed files as required by the respective
    #   forecast applications.

    # Additional capabilities of this class:

    # - Check that a given forecast model is supported (e.g., GFS,
    #   GDAS, etc.,); this can be accomplished either via a
    #   schema-type interface or by evaluating a base-class Python
    #   dictionary.

    # - This class will use methods, functions, and classes beneath
    #   the pygw package; additions to pygw may/will become necessary
    #   and development evolves.
