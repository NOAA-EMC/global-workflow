import sys


__all__ = ['Factory']


class Factory:
    """
    General Purpose Object Factory (Factory) to create all kinds of objects.
    It provides methods to register a Builder and create concrete object
    instances based on key value.
    It also provides methods to check if a Builder is registered as well as
    all the registered builders in the Factory.
    """

    def __init__(self, name: str):
        """
        Initialize an empty {name}Factory with no Builders


        Parameters
        ----------
        name : Name of factory
        """
        self._name = f'{name}Factory'
        self._builders = {}

        # Register {name}Factory as importable from uwtools.factory
        me = sys.modules[__name__]
        if not hasattr(me, self._name):
            setattr(me, self._name, self)
        else:
            raise AttributeError(f"{self._name} is already an importable object from {me}")

    def register(self, key: str, builder: object):
        """
        Register a new builder in the Factory

        Parameters
        ----------
        key: str
            Name of the builder

        Returns
        -------
        object: The class that will serve as the builder for this key
        """
        if self.is_registered(key):
            print(f'{key} is already a registered Builder in {self._name}')
            return
        self._builders[key] = builder

    def create(self, key: str, *args, **kwargs):
        """
        Instantiate a registered Builder

        Parameters
        ----------
        key: str
            Name of the builder to use
        args : tuple
            Arguments to pass to the builder
        kwargs : dict
            Keyword arguments to pass to the builder
        """
        if not self.is_registered(key):
            raise KeyError(
                f"{key} is not a registered builder in {self._name}.\n" +
                "Available builders are:\n" +
                f"{', '.join(self._builders.keys())}")

        return self._builders[key](*args, **kwargs)

    def destroy(self, key: str):
        """
        Retire a registered builder from the Factory
        Note: This will not delete the instance if it was created, just that
        this Builder will no longer be able to work in the Factory

        Parameters
        ----------
        key : str
            Name of builder to unregister
        """
        try:
            del self._builders[key]
        except KeyError:
            print(f'{key} is not a registered Builder in {self._name}')

    @property
    def registered(self):
        """
        Return a set of all registered builders in the Factory

        Returns
        -------
        set : All registered builders

        """
        return set(self._builders.keys())

    def is_registered(self, key: str):
        """
        Return True/False if a builder is registered in the Factory

        Parameters
        ----------
        key : Name of builder to check

        Returns
        -------
        bool : if builder is registered in the Factory
        """
        return key in self._builders.keys()

    @classmethod
    def get_factory(cls, name: str):
        """
        Retrieve the named factory

        Parameters
        ----------
        name : Name of factory

        Returns
        -------
        Factory : Factory designated by the specified name

        """
        me = sys.modules[__name__]
        try:
            return getattr(me, name)
        except AttributeError:
            raise AttributeError(f"{name} is not a Factory in {me}")
