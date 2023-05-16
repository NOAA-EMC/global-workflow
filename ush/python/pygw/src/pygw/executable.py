import os
import shlex
import subprocess
import sys
from typing import Any, Optional, Union, List

__all__ = ["Executable", "which", "CommandNotFoundError"]


class Executable:
    """
    Class representing a program that can be run on the command line.

    Example:
    --------

    >>> from pygw.executable import Executable
    >>> cmd = Executable('srun')  # Lets say we need to run command e.g. "srun"
    >>> cmd.add_default_arg('my_exec.x')  # Lets say we need to run the executable "my_exec.x"
    >>> cmd.add_default_arg('my_arg.yaml')  # Lets say we need to pass an argument to this executable e.g. "my_arg.yaml"
    >>> cmd.add_default_env('OMP_NUM_THREADS', 4)  # Lets say we want to run w/ 4 threads in the environment
    >>> cmd(output='stdout', error='stderr')  # Run the command and capture the stdout and stderr in files named similarly.

    `cmd` line above will translate to:

    $ export OMP_NUM_THREADS=4
    $ srun my_exec.x my_arg.yaml 1>&stdout 2>&stderr

    References
    ----------
    .. [1] "spack.util.executable.py", https://github.com/spack/spack/blob/develop/lib/spack/spack/util/executable.py
    """

    def __init__(self, name: str):
        """
        Construct an executable object.

        Parameters
        ----------
        name : str
               name of the executable to run
        """
        self.exe = shlex.split(str(name))
        self.default_env = {}
        self.returncode = None

        if not self.exe:
            raise ProcessError(f"Cannot construct executable for '{name}'")

    def add_default_arg(self, arg: Union[str, List]) -> None:
        """
        Add a default argument to the command.
        Parameters
        ----------
        arg : str
              argument to the executable
        """
        if isinstance(arg, list):
            self.exe.extend(arg)
        else:
            self.exe.append(arg)

    def add_default_env(self, key: str, value: Any) -> None:
        """
        Set an environment variable when the command is run.

        Parameters:
        ----------
        key : str
              The environment variable to set
        value : Any
                The value to set it to
        """
        self.default_env[key] = str(value)

    @property
    def command(self) -> str:
        """
        The command-line string.

        Returns:
        --------
        str : The executable and default arguments
        """
        return " ".join(self.exe)

    @property
    def name(self) -> str:
        """
        The executable name.

        Returns:
        --------
        str : The basename of the executable
        """
        return os.path.basename(self.path)

    @property
    def path(self) -> str:
        """
        The path to the executable.

        Returns:
        --------
        str : The path to the executable
        """
        return self.exe[0]

    def __call__(self, *args, **kwargs):
        """
        Run this executable in a subprocess.

        Parameters:
        -----------
        *args (str): Command-line arguments to the executable to run

        Keyword Arguments:
        ------------------
        _dump_env : Dict
            Dict to be set to the environment actually
            used (envisaged for testing purposes only)
        env : Dict
            The environment with which to run the executable
        fail_on_error : bool
            Raise an exception if the subprocess returns
            an error. Default is True. The return code is available as
            ``exe.returncode``
        ignore_errors : int or List
            A list of error codes to ignore.
            If these codes are returned, this process will not raise
            an exception even if ``fail_on_error`` is set to ``True``
        input :
            Where to read stdin from
        output :
            Where to send stdout
        error :
            Where to send stderr

        Accepted values for input, output, and error:

        * python streams, e.g. open Python file objects, or ``os.devnull``
        * filenames, which will be automatically opened for writing
        * ``str``, as in the Python string type. If you set these to ``str``,
          output and error will be written to pipes and returned as a string.
          If both ``output`` and ``error`` are set to ``str``, then one string
          is returned containing output concatenated with error. Not valid
          for ``input``
        * ``str.split``, as in the ``split`` method of the Python string type.
          Behaves the same as ``str``, except that value is also written to
          ``stdout`` or ``stderr``.

        By default, the subprocess inherits the parent's file descriptors.

        """
        # Environment
        env_arg = kwargs.get("env", None)

        # Setup default environment
        env = os.environ.copy() if env_arg is None else {}
        env.update(self.default_env)

        # Apply env argument
        if env_arg:
            env.update(env_arg)

        if "_dump_env" in kwargs:
            kwargs["_dump_env"].clear()
            kwargs["_dump_env"].update(env)

        fail_on_error = kwargs.pop("fail_on_error", True)
        ignore_errors = kwargs.pop("ignore_errors", ())

        # If they just want to ignore one error code, make it a tuple.
        if isinstance(ignore_errors, int):
            ignore_errors = (ignore_errors,)

        output = kwargs.pop("output", None)
        error = kwargs.pop("error", None)
        input = kwargs.pop("input", None)

        if input is str:
            raise ValueError("Cannot use `str` as input stream.")

        def streamify(arg, mode):
            if isinstance(arg, str):
                return open(arg, mode), True
            elif arg in (str, str.split):
                return subprocess.PIPE, False
            else:
                return arg, False

        istream, close_istream = streamify(input, "r")
        ostream, close_ostream = streamify(output, "w")
        estream, close_estream = streamify(error, "w")

        cmd = self.exe + list(args)

        escaped_cmd = ["'%s'" % arg.replace("'", "'\"'\"'") for arg in cmd]
        cmd_line_string = " ".join(escaped_cmd)

        proc = None  # initialize to avoid lint warning
        try:
            proc = subprocess.Popen(cmd, stdin=istream, stderr=estream, stdout=ostream, env=env, close_fds=False)
            out, err = proc.communicate()

            result = None
            if output in (str, str.split) or error in (str, str.split):
                result = ""
                if output in (str, str.split):
                    outstr = str(out.decode("utf-8"))
                    result += outstr
                    if output is str.split:
                        sys.stdout.write(outstr)
                if error in (str, str.split):
                    errstr = str(err.decode("utf-8"))
                    result += errstr
                    if error is str.split:
                        sys.stderr.write(errstr)

            rc = self.returncode = proc.returncode
            if fail_on_error and rc != 0 and (rc not in ignore_errors):
                long_msg = cmd_line_string
                if result:
                    # If the output is not captured in the result, it will have
                    # been stored either in the specified files (e.g. if
                    # 'output' specifies a file) or written to the parent's
                    # stdout/stderr (e.g. if 'output' is not specified)
                    long_msg += "\n" + result

                raise ProcessError(f"Command exited with status {proc.returncode}:", long_msg)

            return result

        except OSError as e:
            raise ProcessError(f"{self.exe[0]}: {e.strerror}", f"Command: {cmd_line_string}")

        except subprocess.CalledProcessError as e:
            if fail_on_error:
                raise ProcessError(
                    str(e),
                    f"\nExit status {proc.returncode} when invoking command: {cmd_line_string}",
                )

        finally:
            if close_ostream:
                ostream.close()
            if close_estream:
                estream.close()
            if close_istream:
                istream.close()

    def __eq__(self, other):
        return hasattr(other, "exe") and self.exe == other.exe

    def __neq__(self, other):
        return not (self == other)

    def __hash__(self):
        return hash((type(self),) + tuple(self.exe))

    def __repr__(self):
        return f"<exe: {self.exe}>"

    def __str__(self):
        return " ".join(self.exe)


def which_string(*args, **kwargs) -> str:
    """
    Like ``which()``, but return a string instead of an ``Executable``.

    If given multiple executables, returns the string of the first one that is found.
    If no executables are found, returns None.

    Parameters:
    -----------
    *args : str
        One or more executables to search for

    Keyword Arguments:
    ------------------
    path : str or List
        The path to search. Defaults to ``PATH``
    required : bool
        If set to True, raise an error if executable not found

    Returns:
    --------
    str :
        The first executable that is found in the path
    """
    path = kwargs.get("path", os.environ.get("PATH", ""))
    required = kwargs.get("required", False)

    if isinstance(path, str):
        path = path.split(os.pathsep)

    for name in args:
        for candidate_name in [name]:
            if os.path.sep in candidate_name:
                exe = os.path.abspath(candidate_name)
                if os.path.isfile(exe) and os.access(exe, os.X_OK):
                    return exe
            else:
                for directory in path:
                    exe = os.path.join(directory, candidate_name)
                    if os.path.isfile(exe) and os.access(exe, os.X_OK):
                        return exe

    if required:
        raise CommandNotFoundError(f"'{args[0]}' is required. Make sure it is in your PATH.")

    return None


def which(*args, **kwargs) -> Optional[Executable]:
    """
    Finds an executable in the PATH like command-line which.

    If given multiple executables, returns the first one that is found.
    If no executables are found, returns None.

    Parameters:
    -----------
    *args : str
        One or more executables to search for

    Keyword Arguments:
    ------------------
    path : str or List
        The path to search. Defaults to ``PATH``
    required : bool
        If set to True, raise an error if executable not found

    Returns:
    --------
    Executable: The first executable that is found in the path
    """
    exe = which_string(*args, **kwargs)
    return Executable(shlex.quote(exe)) if exe else None


class ProcessError(Exception):
    """
    ProcessErrors are raised when Executables exit with an error code.
    """
    def __init__(self, short_msg, long_msg=None):
        self.short_msg = short_msg
        self.long_msg = long_msg
        message = short_msg + '\n' + long_msg if long_msg else short_msg
        super().__init__(message)


class CommandNotFoundError(OSError):
    """
    Raised when ``which()`` cannot find a required executable.
    """
