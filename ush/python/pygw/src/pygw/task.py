class Task:
    """
    Base class for all tasks
    """

    def __init__(self, config, *args, **kwargs):
        """
        Every task needs a config.
        Additional arguments (or key-value arguments) can be provided.

        Parameters
        ----------
        config : Dict
                 dictionary object containing task configuration

        *args : tuple
                Additional arguments to `Task`

        **kwargs : dict, optional
                   Extra keyword arguments to `Task`
        """

        # Store the config and arguments as attributes of the object
        self.config = config

        for arg in args:
            setattr(self, str(arg), arg)

        for key, value in kwargs.items():
            setattr(self, key, value)

    def initialize(self):
        pass

    def configure(self):
        pass

    def execute(self):
        pass

    def finalize(self):
        pass

    def clean(self):
        pass
