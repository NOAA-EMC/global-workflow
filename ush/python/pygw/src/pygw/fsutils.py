import os
import errno
import shutil
import contextlib

__all__ = ['mkdir', 'mkdir_p', 'rmdir', 'chdir', 'rm_p', 'cp_dict']


def mkdir_p(path):
    try:
        os.makedirs(path)
    except OSError as exc:
        if exc.errno == errno.EEXIST and os.path.isdir(path):
            pass
        else:
            raise OSError(f"unable to create directory at {path}")


mkdir = mkdir_p


def rmdir(dir_path):
    try:
        shutil.rmtree(dir_path)
    except OSError as exc:
        raise OSError(f"unable to remove {dir_path}")


@contextlib.contextmanager
def chdir(path):
    cwd = os.getcwd()
    try:
        os.chdir(path)
        yield
    finally:
        print(f"WARNING: Unable to chdir({path})")  # TODO: use logging
        os.chdir(cwd)


def rm_p(path):
    try:
        os.unlink(path)
    except OSError as exc:
        if exc.errno == errno.ENOENT:
            pass
        else:
            raise OSError(f"unable to remove {path}")


def cp_dict(filedict, skip_missing=False):
    for src, dest in filedict.items():
        destdir = os.path.dirname(dest)
        if not os.path.exists(destdir):
            print(f'{destdir} does not exist, creating directory.')
            os.makedirs(destdir)
        if os.path.exists(dest):
            os.remove(dest)
        if skip_missing:
            if not os.path.exists(src):
                print(f'{src} does not exist. Will not copy.')
                continue
        print(f'Copying {src} to {dest}')
        shutil.copyfile(src, dest)