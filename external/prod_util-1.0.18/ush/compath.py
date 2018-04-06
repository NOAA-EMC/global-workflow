#!/usr/bin/python

# Author:  Kit Menlove <kit.menlove@noaa.gov>
# Purpose: Given the relative path of a COM directory, return the corresponding absolute
#          path. For COMIN paths, a search is performed in the following order:
#              1. the COMPATH variable in an envir-insensitive manner
#              2. the COM path list in an envir-sensitive manner
#              3. the production paths - success if only one match is found
#          For COMOUT directories ('-o' flag), the COMROOT variable is prepended to the
#          provided relative COM path.
# Usage:   compath [-o] [-e envir] [-v] relpath
#              where relpath may contain $NET, $NET/$envir, $NET/$envir/$RUN, or
#              $NET/$envir/$RUN.$PDY.
# Input:   parm/comroot_${envir}.list files, where $envir is retrieved from one of the
#          following (in order of decreasing precedence):
#              1. the command line '-e' or '--envir' switch
#              2. the input path (e.g. gfs/test/gfs.20160301 would use the
#                 comroot_test.list file)
#              3. the $envir environment variable
#              4. "prod"

from __future__ import print_function
from os import path, environ, getenv, system
from sys import exit, stderr
import re

def err_exit(msg):
    if __name__ == "__main__":
        system('err_exit "[compath] ' + msg + '"')
    else:
        print(msg, file=stderr)
    exit(1)

if path.exists('/gpfs/gp2'):
    com_aliases = {'/com': '/gpfs/gp1/nco/ops/com', '/gpfs/?p1/nco/ops/com': '/gpfs/gp1/nco/ops/com', '/com2': '/gpfs/gp2/nco/ops/com', '/gpfs/?p2/nco/ops/com': '/gpfs/gp2/nco/ops/com', '/comhps': '/gpfs/hps/nco/ops/com'}
elif path.exists('/gpfs/tp2'):
    com_aliases = {'/com': '/gpfs/tp1/nco/ops/com', '/gpfs/?p1/nco/ops/com': '/gpfs/tp1/nco/ops/com', '/com2': '/gpfs/tp2/nco/ops/com', '/gpfs/?p2/nco/ops/com': '/gpfs/tp2/nco/ops/com', '/comhps': '/gpfs/hps/nco/ops/com'}
else:
    err_exit('Unable to find /gpfs/?p2, are you on WCOSS?')

parse_compath = re.compile(r'/(?P<NET>[\w-]+)(?:/(?P<envir>prod|para|test)(?:/(?P<RUN>[\w-]+?)(?:\.(?P<PDY>2\d(?:\d\d){1,4}))?)?)?$')

# Function that returns a dictionary containing the NET, envir, RUN, and PDY parts of dirpath
def getparts(dirpath):
    match_result = parse_compath.search(dirpath)
    if match_result:
        dirdict = match_result.groupdict()
        dirdict['path'] = dirpath
        return dirdict
    else:
        print("WARNING: A member of the COM path list (" + dirpath + ") is not formatted correctly", file=stderr)
        return {'path': dirpath}

# Function to search for a path in a list of paths.  First, the full length will
# be searched. Then, increasingly shorter paths (with the rightmost segment removed,
# delimited by slashes and periods) will be searched until nothing is left.
def findpath(relpath_parts, dirlist_parts, envir_sensitive=True):
    pathpart_names = ('NET', 'envir', 'RUN', 'PDY')
    pathpart_count = 0
    for part_name in pathpart_names:
        if relpath_parts[part_name]:
            pathpart_count += 1
        else:
            # If the pathpart is not defined for the relpath, remove all directories
            # from the dirlist where it is defined
            dirlist_parts[:] = [dir_parts for dir_parts in dirlist_parts if not dir_parts.get(part_name)]

    # Start looking at all of the significant path parts, then take one away, etc...
    for num_pathparts in range(pathpart_count, 0, -1):
        if relpath_parts[pathpart_names[num_pathparts-1]] == None:
            continue
        # Check each absolute path for a match
        for dir_parts in dirlist_parts:
            foundmatch = True
            for part in pathpart_names[0:num_pathparts]:
                # If the path part is defined in the relative path but not the absolute path, skip it for now
                if dir_parts.get(part) == None:
                    foundmatch = False
                    break
                # If the path part is defined in both the relative and absolute path but they don't match,
                # remove the absolute path from the list
                elif relpath_parts[part] != dir_parts[part] and (part != 'envir' or envir_sensitive):
                    foundmatch = False
                    dir_parts.clear()
                    break
            if foundmatch:
                match_pathparts = [ dir_parts['path'] ]
                for part in pathpart_names[num_pathparts:pathpart_count]:
                    match_pathparts.append('.' if part == 'PDY' else '/')
                    match_pathparts.append(relpath_parts[part])
                if relpath_parts['tail']:
                    match_pathparts.append(relpath_parts['tail'])
                return ''.join(match_pathparts)

def get_compath(relpath, envir=None, out=False, verbose=False):
    """!Returns the absolute path of the production COM directory represented
    by the provided relative path.

    @param relpath: The relative path of the COM directory desired.
    @param envir:   Environment of the COM paths list to use (default: $envir
                    environment variable).
    @param out:     Whether to return the location for a COMOUT (out=True) or
                    COMIN (out=False) directory.
                    COMOUT directories will always point to the current system.
    @param verbose: Whether to print the source of the returned COMROOT path to
                    stderr.
    @returns        A string containing the absolute path of the desired
                    production COM directory.
    """

    match_result = re.match(r'/?(?:com/)?(?P<NET>[\w-]+)(?:/(?P<envir>prod|para|test)(?:/(?P<RUN>[\w-]+?)(?:\.(?P<PDY>2\d(?:\d\d){1,4}))?)?)?(?P<tail>/)?$', relpath)
    if match_result:
        relpath_parts = match_result.groupdict()
        if envir == None:
            envir = relpath_parts['envir'] if relpath_parts['envir'] else getenv('envir', 'prod')
    else:
        err_exit('The relative COM path provided (' + relpath + ') is not formatted correctly.')

    foundpath = None

    # If we are looking for a COMOUT path, use the COMROOT variable
    if out:
        try:
            foundpath = environ['COMROOT'] + '/' + relpath
            if verbose and foundpath:
                print("COMOUT path found using $COMROOT environment variable", file=stderr)
        except KeyError:
            err_exit('\$COMROOT is not defined. Please define it or load the prod_envir module.')

    # Search the COMPATH environment variable for an appropriate match
    # The matching done in this case is envir-insensitive, meaning that a relpath
    # will match with directories in the dirlist from a different environment
    if not foundpath:
        compath_var = getenv('COMPATH')
        if compath_var:
            # Split COMPATH by colons and commas
            var_dirlist = [ s.strip().rstrip('/') for s in re.split(r':|,', compath_var) ]
            var_dirlist_parts = map(getparts, var_dirlist)
            foundpath = findpath(relpath_parts, var_dirlist_parts, envir_sensitive=False)
            if verbose and foundpath:
                print("COMIN path found in $COMPATH environment variable", file=stderr)
        else:
            foundpath = None

    # Search the compaths list file for an appropriate match
    if not foundpath:
        try:
            compaths_filename = "{0}/parm/compaths_{1}.list".format(environ['UTILROOT'], envir)
        except KeyError:
            err_exit('\$UTILROOT is not defined. Please load the prod_util module.')
        try:
            with open(compaths_filename, 'r') as compaths_list:
                # Remove blank and commented-out lines
                file_dirlist = filter(lambda line: line and not line.startswith('#'),
                                      (line.strip().rstrip('/') for line in compaths_list))
            file_dirlist_parts = map(getparts, file_dirlist)
            foundpath = findpath(relpath_parts, file_dirlist_parts)
            if verbose and foundpath:
                print("COMIN path found in", compaths_filename, file=stderr)
        except IOError as err:
            print("WARNING: Could not find the", envir, "COM paths list at", err.filename, file=stderr)

    # Search the available COM directories.  If only one path is found, return it.
    if not foundpath:
        possible_comroots = list()
        for dir in set(com_aliases.values()):
            if path.exists(dir + '/' + relpath):
                possible_comroots.append(dir)
        if len(possible_comroots) == 1:
            foundpath = possible_comroots[0] + '/' + relpath
            if verbose and foundpath:
                print("COMIN path found searching through the system COM paths", file=stderr)

    if foundpath:
        # Replace the matching alias (if an alias was used in the found path) in
        # com_aliases with its corresponding full path
        pattern = re.compile("^(?:%s)" % '|'.join(map(re.escape, com_aliases.keys())))
        foundpath = pattern.sub(lambda x: com_aliases[x.group()], foundpath, 1)

        # Print the absolute COM path
        return foundpath
    else:
        err_exit('Could not find ' + relpath)

if __name__ == "__main__":
    import argparse
    # Parse command line arguments
    parser = argparse.ArgumentParser(description='Given the relative path of a COM directory, return the corresponding absolute path according to where the data is located.')
    parser.add_argument('-o', '--out', action='store_true', help='Return a COMOUT directory')
    parser.add_argument('-e', '--envir', metavar='envir', choices=('prod', 'para', 'test'), help='Environment of the COM paths list to use (default: $envir environment variable)')
    parser.add_argument('-v', '--verbose', action='store_true', help='Print the source of the returned COMROOT path to stderr')
    parser.add_argument('path', metavar='relpath', help='Relative path of the COM directory to be located ($NET/$envir/$RUN.$PDY)')
    args = parser.parse_args()

    # Parse the relative path provided as input
    print(get_compath(args.path.strip(), args.envir, args.out, args.verbose))

