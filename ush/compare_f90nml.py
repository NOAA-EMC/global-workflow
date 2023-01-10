#!/usr/bin/env python3

import json
from typing import Dict
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
try:
    import f90nml
except (ImportError, ModuleNotFoundError):
    raise ModuleNotFoundError(f"f90nml not found on this system, ABORT!")


def get_dict_from_nml(filename: str) -> Dict:
    """
    Read a F90 namelist and convert to a dictionary.
    This method uses json to convert OrderedDictionary into regular dictionary
    """
    return json.loads(json.dumps(f90nml.read(filename).todict()))


def compare_dicts(dict1: Dict, dict2: Dict, path: str = "") -> None:
    """
    Compare 2 dictionaries.
    This is done by looping over keys in dictionary 1 and searching for them
    in dictionary 2.
    If a matching key is found, the values are compared.
    If a matching key is not found, it is set to as UNDEFINED.
    Note: A reverse match is not performed in this method.  For reverse matching, use the -r option in the main driver.
    Note: This is a recursive method to handle nested dictionaries.
    """

    result = dict()
    for kk in dict1.keys():
        if kk in dict2.keys():
            if type(dict1[kk]) is dict:
                compare_dicts(dict1[kk], dict2[kk],
                              f"{path} -> {kk}" if path else kk)
            else:
                if dict1[kk] != dict2[kk]:
                    if path not in result:
                        result[path] = dict()
                    result[path][kk] = [dict1[kk], dict2[kk]]
        else:
            if path:
                if path not in result:
                    result[path] = dict()
                result[path][kk] = [dict1[kk], 'UNDEFINED']
            else:
                if kk not in result:
                    result[kk] = dict()
                    result[kk][kk] = ['UNDEFINED']

    def _print_diffs(diffs: Dict) -> None:
        """
        Print the differences between the two dictionaries to stdout
        """
        for path in diffs.keys():
            print(f"{path}:")
            max_len = len(max(diffs[path], key=len))
            for kk in diffs[path].keys():
                items = diffs[path][kk]
                print(
                    f"{kk:>{max_len+2}} : {' | '.join(map(str, diffs[path][kk]))}")

    _print_diffs(result)


if __name__ == "__main__":

    parser = ArgumentParser(
        description=("Compare two Fortran namelists and display differences"),
        formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('-n', '--namelists', help='name of two namelists to compare (namelist1 - namelist2)',
                        type=str, nargs=2,
                        metavar=('namelist1', 'namelist2'), required=True)
    parser.add_argument('-r', '--reverse', help='reverse diff (namelist2 - namelist1)',
                        action='store_true', required=False)
    args = parser.parse_args()

    nml1, nml2 = args.namelists
    if args.reverse:
        nml2, nml1 = nml1, nml2

    dict1 = get_dict_from_nml(nml1)
    dict2 = get_dict_from_nml(nml2)

    msg = f"comparing: {nml1} | {nml2}"
    print(msg)
    print("-" * len(msg))
    compare_dicts(dict1, dict2)
