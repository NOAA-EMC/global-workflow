
from task.forecast import Forecast

# ----


class GFS(Forecast):

    # This class it the base-class for the GFS forecast-type
    # applications. Similar to the base-class `Forecast`, the
    # intention of this task is both brevity and longer-term UFS-based
    # forecast type applications.

    # Methods to be included in this class:

    # - constructor: this method handles the instantation of the GFS
    #   class; this method should also contain means to determine
    #   whether configuration requests are supported (i.e., is DATM a
    #   valid configuration, is coupling currently supported, etc.,).

    def __init__(self: Forecast):

        super().__init__(config_obj)

        # This may be best defined by a YAML-formatted file input to
        # the respective `global-workflow/scripts` application.

    # - initialize: this method handles all directory tree
    #   constuction, configuration file constructions, file linking,
    #   etc., ; it will be run in the `service` queue of RDHPCS
    #   platforms.

    def initialize(self: Forecast) -> None:
        pass

    # - run: this method simply launches the forecast model
    #   executable; it will be run in the `batch` queue for the
    #   corresponding RDHPCS platforms.

    def run(self: Forecast) -> None:
        pass

    # - finalize: this method will deliver products for any and all
    #   downstream applications, as function of UFS experiment type,
    #   and clean-up as necessary; if will be run in the `service`
    #   queue of the respective RDHPCS platforms.

    def finalize(self: Forecast) -> None:
        pass
