"""
Abstracts the config directory.

Example:
c = ConfigManager(pathlib.Path("../parm/config"))

print(c)
>>> anal, analcalc, analdiag, arch, awips, base.emc.dyn, base.nco.static, earc, ecen, echgres, ediag, efcs, eobs, epos, esfc, eupd, fcst, fv3.emc.dyn, fv3.nco.static, fv3ic, gempak, getic, gldas, metp, nsst, post, postsnd, prep, prepbufr, resources.emc.dyn, resources.nco.static, vrfy, wafs, wafsblending, wafsblending0p25, wafsgcip, wafsgrib2, wafsgrib20p25, wave, waveawipsbulls, waveawipsgridded, wavegempak, waveinit, wavepostbndpnt, wavepostbndpntbll, wavepostpnt, wavepostsbs, waveprep

print(c.eupd.path)
>>> /home/rlong/Sandbox/temp/global-workflow/parm/config/config.eupd

print(c.eupd.name)
>>> eupd

print(c.eupd.content)
>>> ...prints content of eupd


c.eupd.write_to(pathlib.Path("./eupd.sh"))
>>> ...writes content of eupd to file at `./eupd.sh"

"""

import glob
import pathlib
import itertools
from typing import List


class ConfigFile:
    """represents a config file"""

    def __init__(self, name, path):
        self.name = name
        self.path = path

    def __repr__(self):
        return f"<{self.__class__.__name__} {self.name}:{self.path} />"

    def __str__(self):
        return f"{self.name}: {self.path}"

    @property
    def content(self) -> List[str]:
        """return the config file contents"""
        with open(self.path, "r", encoding="utf-8") as _file:
            return _file.readlines()

    def write_to(self, output_path: pathlib.Path) -> None:
        """writes the content of a config file to `output_path`"""
        with open(output_path, "w", encoding="utf-8") as _file:
            _file.writelines(self.content)


class ConfigManager:
    """interface to manage config files"""

    def __init__(self, config_files_root: pathlib.Path):
        self.config_files_root = config_files_root.resolve()

    def __iter__(self):
        return iter(self.keys)

    def __len__(self):
        return len(self.keys)

    def __repr__(self):
        return f"<{self.__class__.__name__} {len(self.map)}/>"

    def __str__(self):
        return f"{', '.join(self.keys)}"

    def __getitem__(self, __name: str):
        return list(filter(lambda x: x.name == __name, self.map))[0]

    def __getattr__(self, __name: str):
        return self.__getitem__(__name)

    @property
    def map(self):
        """returns list of ConfigFile"""
        return [
            ConfigFile(config_file.name.split(".", 1)[1], config_file)
            for config_file in files_by_path(
                pathlib.Path(self.config_files_root), ["*"]
            )
        ]

    @property
    def keys(self):
        """returns unique list of config names"""
        return sorted({x.name for x in self.map})

    @property
    def paths(self):
        """returns unique list of config paths"""
        return sorted({x.path for x in self.map})


def files_by_path(target: pathlib.Path, extensions: List[str]) -> List[pathlib.Path]:
    """returns a list of files"""
    return [
        pathlib.Path(_file).resolve()
        for _file in itertools.chain(
            *[
                glob.glob(f"{target.resolve()}/**/*.{x}", recursive=True)
                for x in extensions
            ],
        )
        if not any([pathlib.Path(_file).is_dir()])
    ]


if __name__ == "__main__":
    pass
