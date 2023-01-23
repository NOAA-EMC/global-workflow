#!/usr/bin/env python3

import json
import f90nml
from typing import Dict
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter


def get_dict_from_nml(filename: str) -> Dict:
    """
    Read a F90 namelist and convert to a dictionary.
    This method uses json to convert OrderedDictionary into regular dictionary
    Parameters
    ----------
    filename: str
              Name of the F90 namelist
    Returns
    -------
    dictionary: Dict
                F90 namelist returned as a dictionary
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
    Parameters
    ----------
    dict1: Dict
           First dictionary
    dict2: Dict
           Second dictionary
    path:  str (optional)
           default: ""
           key (if nested dictionary)
    Returns
    -------
    None
    """

    result = dict()
    for kk in dict1.keys():  # Loop over all keys of first dictionary
        if kk in dict2.keys():  # kk is present in dict2
            if isinstance(dict1[kk], dict):  # nested dictionary, go deeper
                compare_dicts(dict1[kk], dict2[kk], path=kk)
            else:
                if dict1[kk] != dict2[kk]:
                    if path not in result:
                        result[path] = dict()
                    result[path][kk] = [dict1[kk], dict2[kk]]
        else:  # kk is *not* present in dict2
            tt = path if path else kk
            if tt not in result:
                result[tt] = dict()
            result[tt][kk] = [dict1[kk], 'UNDEFINED']

    def _print_diffs(diff_dict: Dict) -> None:
        """
        Print the differences between the two dictionaries to stdout
        Parameters
        ----------
        diff_dict: Dict
                   Dictionary containing differences
        Returns
        -------
        None
        """
        for path in diff_dict.keys():
            print(f"{path}:")
            max_len = len(max(diff_dict[path], key=len))
            for kk in diff_dict[path].keys():
                items = diff_dict[path][kk]
                print(
                    f"{kk:>{max_len+2}} : {' | '.join(map(str, diff_dict[path][kk]))}")

    _print_diffs(result)


if __name__ == "__main__":

    parser = ArgumentParser(
        description=("Compare two Fortran namelists and display differences (left_namelist - right_namelist)"),
        formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('left_namelist', type=str, help="Left namelist to compare")
    parser.add_argument('right_namelist', type=str, help="Right namelist to compare")
    parser.add_argument('-r', '--reverse', help='reverse diff (right_namelist - left_namelist)',
                        action='store_true', required=False)
    args = parser.parse_args()

    nml1, nml2 = args.left_namelist, args.right_namelist
    if args.reverse:
        nml2, nml1 = nml1, nml2

    dict1 = get_dict_from_nml(nml1)
    dict2 = get_dict_from_nml(nml2)

    msg = f"comparing: {nml1} | {nml2}"
    print(msg)
    print("-" * len(msg))
    compare_dicts(dict1, dict2)
