import io
import os
import sys
import jinja2
from markupsafe import Markup
from pathlib import Path
from typing import Dict

from .timetools import strftime, to_YMDH, to_YMD, to_fv3time, to_isotime, to_julian

__all__ = ['Jinja']


@jinja2.pass_eval_context
class SilentUndefined(jinja2.Undefined):
    """
    Description
    -----------
    A Jinja2 undefined that does not raise an error when it is used in a
    template. Instead, it returns the template back when the variable is not found
    This class is not to be used outside of this file
    Its purpose is to return the template instead of an empty string
    Presently, it also does not return the filter applied to the variable.
    This will be added later when a use case for it presents itself.
    """
    def __str__(self):
        return "{{ " + self._undefined_name + " }}"

    def __add__(self, other):
        return str(self) + other

    def __radd__(self, other):
        return other + str(self)

    def __mod__(self, other):
        return str(self) % other

    def __call__(self, *args, **kwargs):
        return Markup("{{ " + self._undefined_name + " }}")


class Jinja:
    """
    Description
    -----------
    A wrapper around jinja2 to render templates
    """

    def __init__(self, template_path_or_string: str, data: Dict, allow_missing: bool = True):
        """
        Description
        -----------
        Given a path to a (jinja2) template and a data object, substitute the
        template file with data.
        Allow for retaining missing or undefined variables.
        Parameters
        ----------
        template_path_or_string : str
            Path to the template file or a templated string
        data : dict
            Data to be substituted into the template
        allow_missing : bool
            If True, allow for missing or undefined variables
        """

        self.data = data
        self.undefined = SilentUndefined if allow_missing else jinja2.StrictUndefined

        if os.path.isfile(template_path_or_string):
            self.template_type = 'file'
            self.template_path = Path(template_path_or_string)
        else:
            self.template_type = 'stream'
            self.template_stream = template_path_or_string

    @property
    def render(self, data: Dict = None) -> str:
        """
        Description
        -----------
        Render the Jinja2 template with the data
        Parameters
        ----------
        data: dict (optional)
        Additional data to be used in the template
        Not implemented yet.  Placed here for future use
        Returns
        -------
        rendered: str
        Rendered template into text
        """

        render_map = {'stream': self._render_stream,
                      'file': self._render_file}
        return render_map[self.template_type]()

    def get_set_env(self, loader: jinja2.BaseLoader, filters: Dict[str, callable] = None) -> jinja2.Environment:
        """
        Description
        -----------
        Define the environment for the jinja2 template
        Any number of filters can be added here.
        Optionally, a dictionary of external filters can be passed in

        Currently, the following filters are defined:
        strftime: convert a datetime object to a string with a user defined format
        to_isotime: convert a datetime object to an ISO 8601 string
        to_fv3time: convert a datetime object to a FV3 time string
        to_YMDH: convert a datetime object to a YYYYMMDDHH string
        to_YMD: convert a datetime object to a YYYYMMDD string
        to_julian: convert a datetime object to a julian day
        to_f90bool: convert a boolean to a fortran boolean
        getenv: read variable from enviornment if defined, else UNDEFINED

        Parameters
        ----------
        loader: jinja2.BaseLoader
            An instance of class jinja2.BaseLoader
        filters: Dict[str, callable] (optional)
            A dictionary of filters to be added to the environment

        Returns
        -------
        env: jinja2.Environment
        """

        env = jinja2.Environment(loader=loader, undefined=self.undefined)
        env.filters["strftime"] = lambda dt, fmt: strftime(dt, fmt)
        env.filters["to_isotime"] = lambda dt: to_isotime(dt) if not isinstance(dt, SilentUndefined) else dt
        env.filters["to_fv3time"] = lambda dt: to_fv3time(dt) if not isinstance(dt, SilentUndefined) else dt
        env.filters["to_YMDH"] = lambda dt: to_YMDH(dt) if not isinstance(dt, SilentUndefined) else dt
        env.filters["to_YMD"] = lambda dt: to_YMD(dt) if not isinstance(dt, SilentUndefined) else dt
        env.filters["to_julian"] = lambda dt: to_julian(dt) if not isinstance(dt, SilentUndefined) else dt
        env.filters["to_f90bool"] = lambda bool: ".true." if bool else ".false."
        env.filters['getenv'] = lambda name, default='UNDEFINED': os.environ.get(name, default)

        # Add any additional filters
        if filters is not None:
            for filter_name, filter_func in filters.items():
                env.filters[filter_name] = filter_func

        return env

    @staticmethod
    def add_filter_env(env: jinja2.Environment, filter_name: str, filter_func: callable):
        """
        Description
        -----------
        Add a custom filter to the jinja2 environment
        Not implemented yet.  Placed here for future use
        Parameters
        ----------
        env: jinja2.Environment
            Active jinja2 environment
        filter_name: str
            name of the filter
        filter_func: callable
            function that will be called
        Returns
        -------
        env: jinja2.Environment
            Active jinja2 environment with the new filter added
        """

        env.filters[filter_name] = filter_func

        return env

    def _render_stream(self, filters: Dict[str, callable] = None):
        loader = jinja2.BaseLoader()
        env = self.get_set_env(loader, filters)
        template = env.from_string(self.template_stream)
        return self._render_template(template)

    def _render_file(self, data: Dict = None, filters: Dict[str, callable] = None):
        template_dir = self.template_path.parent
        template_file = self.template_path.relative_to(template_dir)

        loader = jinja2.FileSystemLoader(template_dir)
        env = self.get_set_env(loader, filters)
        template = env.get_template(str(template_file))
        return self._render_template(template)

    def _render_template(self, template: jinja2.Template):
        """
        Description
        -----------
        Render a jinja2 template object
        Parameters
        ----------
        template: jinja2.Template

        Returns
        -------
        rendered: str
        """
        try:
            rendered = template.render(**self.data)
        except jinja2.UndefinedError as ee:
            raise Exception(f"Undefined variable in Jinja2 template\n{ee}")

        return rendered

    def _render(self, template_name: str, loader: jinja2.BaseLoader) -> str:
        """
        Description
        -----------
        Internal method to render a jinja2 template
        Parameters
        ----------
        template_name: str
        loader: jinja2.BaseLoader
        Returns
        -------
        rendered: str
        rendered template
        """
        env = jinja2.Environment(loader=loader, undefined=self.undefined)
        template = env.get_template(template_name)
        try:
            rendered = template.render(**self.data)
        except jinja2.UndefinedError as ee:
            raise Exception(f"Undefined variable in Jinja2 template\n{ee}")

        return rendered

    def save(self, output_file: str) -> None:
        """
        Description
        -----------
        Render and save the output to a file
        Parameters
        ----------
        output_file: str
        Path to the output file
        Returns
        -------
        None
        """
        with open(output_file, 'wb') as fh:
            fh.write(self.render.encode("utf-8"))

    def dump(self) -> None:
        """
        Description
        -----------
        Render and dump the output to stdout
        Returns
        -------
        None
        """
        io.TextIOWrapper(sys.stdout.buffer,
                         encoding="utf-8").write(self.render)
