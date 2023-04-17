#!/usr/bin/env python3

import filecmp
import os
import sys
from pathlib import Path

from pygw.logger import Logger
logger = Logger(level="INFO", _format='%(levelname)-5s %(message)s', colored_log=False)

from pygw.executable import Executable
from pygw.executable import which
import re


def get_args():
    import argparse
    import json
    parser = argparse.ArgumentParser()
    parser.add_argument('--cmp_dirs', nargs=2, metavar=('ROTDIR_baseline', 'ROTDIR_testrun'), help='compare COMROT foloders')
    parser.add_argument('-n', '--nameID', dest="nameID", help='tag name for compare (used in output filename)')
    parser.add_argument('-v', '--verbose', help='enhanced detailed reporting and unpacking compressed files ', action='store_true', default=False)
    args = parser.parse_args()
    if args.cmp_dirs is not None:
        for dirs in args.cmp_dirs:
            if not Path(dirs).is_dir():
                logger.critical(f'directory {dirs} does not exsist')
                sys.exit(-1)
    return args


def compare(folder1, folder2):
    return _recursive_dircmp(folder1, folder2)


def count_nonid_corr(test_string: str):
    pattern = re.compile(r"(\d+:\d+:)(?P<var>.*):rpn_corr=(?P<corr>.*)")
    matches = [m.groupdict() for m in pattern.finditer(test_string)]

    count = 0
    for match in matches:
        if float(match['corr']) != 1.0:
            count = count + 1
    return count


def _recursive_dircmp(folder1, folder2):

    ignore_list = ['INPUT', 'RESTART', 'logs']
    comparison = filecmp.dircmp(folder1, folder2, ignore=ignore_list)
    data = {
        'left': [r'{}/{}'.format(folder1, i) for i in comparison.left_only],
        'right': [r'{}/{}'.format(folder2, i) for i in comparison.right_only],
        'both': [r'{}/{}'.format(folder1, i) for i in comparison.common_files],
    }

    for datalist in data.values():
        datalist.sort()

    if comparison.common_dirs:
        for folder in comparison.common_dirs:
            sub_folder1 = os.path.join(folder1, folder)
            sub_folder2 = os.path.join(folder2, folder)
            sub_report = _recursive_dircmp(sub_folder1, sub_folder2)

            for key, value in sub_report.items():
                data[key] += value

    return data


def tarcmp(tar_file_one, tar_file_two):

    import hashlib
    import tarfile

    tar1 = tarfile.open(tar_file_one, mode="r")
    tar2 = tarfile.open(tar_file_two, mode="r")
    chunk_size = 100 * 1024

    for member1, member2 in list(zip(tar1, tar2)):
        if not member1.isfile():
            continue

        store_digests = {}

        f1 = tar1.extractfile(member1)
        h1 = hashlib.new('md5')
        data1 = f1.read(chunk_size)
        f2 = tar2.extractfile(member1)
        h2 = hashlib.new('md5')
        data2 = f2.read(chunk_size)

        while data1:
            h1.update(data1)
            data1 = f1.read(chunk_size)
        while data2:
            h2.update(data2)
            data2 = f2.read(chunk_size)

        if h1.hexdigest() != h2.hexdigest():
            return False

    return True


def tarcmp_verbose(tar_file_one, tar_file_two):

    import hashlib
    import tarfile

    comp_tars = []
    files_md5 = {}
    comp_tars.append(tar_file_one)
    comp_tars.append(tar_file_two)
    diff_members = []

    for tar_file in comp_tars:

        tar = tarfile.open(tar_file, mode="r")

        chunk_size = 100 * 1024
        store_digests = {}
        files_md5[tar_file] = {}

        for member in tar:
            if not member.isfile():
                continue
            f = tar.extractfile(member)
            h = hashlib.new('md5')
            data = f.read(chunk_size)
            while data:
                h.update(data)
                data = f.read(chunk_size)
            files_md5[tar_file][member.name] = h.hexdigest()

    if len(files_md5[tar_file_one]) != len(files_md5[tar_file_two]):
        return diff_members

    for member in files_md5[tar_file_one]:
        if files_md5[tar_file_one][member] != files_md5[tar_file_two][member]:
            diff_members.append(member)
    return diff_members


def netcdfver(filename):
    import codecs
    with open(filename, 'rb') as f:
        eight = f.read(8)
        if len(eight) < 4:
            return None
        four = eight[0:4]
        if four == b'CDF\x01':
            return "CDF1"
        elif four == b'CDF\x02':
            return "CDF2"
        elif eight == b'\x89\x48\x44\x46\x0d\x0a\x1a\x0a':
            return "HDF5"
    return None


def check_diff_files(dcmp, ignore_list):

    import tarfile

    global diff_file
    global verbose
    global files_compared
    global total_num_diff_files

    cwd = os.getcwd()

    if len(dcmp.common_dirs) != 0:
        logger.info(f'checking directories: {" ".join(dcmp.common_dirs)}')

    if len(dcmp.diff_files) != 0 and verbose:
        number_netcdf_files = len([s for s in dcmp.diff_files if '.nc' in s])
        logger.info(f'checking {len(dcmp.diff_files)} differing files of which {number_netcdf_files} are NetCDF and some may be tar files')

    num_netcdf_differing_files = 0
    num_netcdf_differing_files_onlyheader = 0
    num_tar_differing_files = 0
    num_identified_tar_files = 0
    num_grib_differing_files = 0
    num_identified_grib_files = 0
    num_differing_files = 0

    for name in dcmp.diff_files:

        if any(item in name for item in ignore_list):
            continue

        file1 = os.path.join(dcmp.left, name)
        file2 = os.path.join(dcmp.right, name)
        file1_shortpath = '/' + dcmp.left.replace(cwd, '').lstrip('/')
        file2_shortpath = '/' + dcmp.right.replace(cwd, '').lstrip('/')
        in_dir = f'in directories {file1_shortpath} and {file2_shortpath}\n'

        if '.nc' in name:
            net_cdf_type = netcdfver(file1)
            if net_cdf_type is not None:
                if verbose:
                    netcdf_diff_output = NCCMP("--threads=4", "--data", file1, file2)
                else:
                    netcdf_diff_output = NCCMP("--diff-count=3", "--threads=4", "--data", file1, file2)
                if netcdf_diff_output is None:
                    diff_file.write(f'NetCDF file {name} of type: {net_cdf_type} differs only in the header ' + in_dir)
                    num_netcdf_differing_files_onlyheader += 1
                else:
                    diff_file.write(f'NetCDF file {name} of type: {net_cdf_type} differs ' + in_dir)
                    num_netcdf_differing_files += 1
        elif tarfile.is_tarfile(file1):
            num_identified_tar_files += 1
            if verbose:
                diff_tar_members = tarcmp_verbose(file1, file2)
                if len(diff_tar_members) != 0:
                    for tar_file in diff_tar_members:
                        diff_file.write(f'tar member file {tar_file} differs in tar file {name} '
                                        f'from directories {file1_shortpath} and {file2_shortpath}\n')
            if not tarcmp(file1, file2):
                diff_file.write(f'tar file {name} differs ' + in_dir)
                num_tar_differing_files += 1
        elif any([x in name for x in ["grib2", "grb2", "flux"]]):
            num_identified_grib_files += 1
            grib2_diff_output = WGRIB2(file1, "-var", "-rpn", "sto_1", "-import_grib", file2, "-rpn", "rcl_1:print_corr", output=str)
            count = count_nonid_corr(grib2_diff_output)
            if count != 0:
                diff_file.write(f'grib file {name} differs with {count} uncorrelated vars' + in_dir)
                num_grib_differing_files += 1
        else:
            diff_file.write(f'file {name} differs' + in_dir)
            num_differing_files += 1
        diff_file.flush()
    if num_netcdf_differing_files != 0:
        logger.info(f'{num_netcdf_differing_files} NetCDF files differed')
    if num_tar_differing_files != 0:
        logger.info(f'{num_tar_differing_files} tar files differed')
    if num_differing_files != 0:
        logger.info(f'{num_differing_files} files differed that was not NetCDF nor tar files')
    files_compared += len(dcmp.common_files)
    total_num_diff_files += num_differing_files + num_tar_differing_files +                      \
                            num_netcdf_differing_files + num_netcdf_differing_files_onlyheader + \
                            num_grib_differing_files
    if verbose:
        if num_netcdf_differing_files == 0 and num_tar_differing_files == 0 and num_differing_files == 0 and len(dcmp.diff_files) != 0:
            if num_identified_tar_files == len(dcmp.diff_files):
                logger.info(f'all of the {len(dcmp.diff_files)} potentially differed files where actually non-differing tar files')
            elif len(dcmp.diff_files) == num_netcdf_differing_files_onlyheader:
                logger.info(f'all of the {len(dcmp.diff_files)} potentially differed files where actually non-differing NetCDF files (only headers differed)')
            else:
                logger.info(f'of the {len(dcmp.diff_files)} potentially differed {num_netcdf_differing_files} NetCDF differed {num_tar_differing_files} '
                            f'tar files differed, and {num_differing_files} differed that where not NetCDF or tar')

    for sub_dcmp in dcmp.subdirs.values():
        check_diff_files(sub_dcmp, ignore_list)


if __name__ == '__main__':

    import datetime
    import time

    global files_compared
    files_compared = 0
    global total_num_diff_files
    total_num_diff_files = 0

    if which("nccmp") is None:
        logger.critical('The NCO nccmp utility was not found')
        sys.exit(-1)
    if which("wgrib2") is None:
        logger.critical('The wgrib2 utility was not found')
        sys.exit(-1)

    NCCMP = Executable("nccmp")
    WGRIB2 = Executable("wgrib2")

    args = get_args()

    process_time = time.process_time()

    global verbose
    verbose = args.verbose

    folder1 = os.path.realpath(args.cmp_dirs[0])
    folder2 = os.path.realpath(args.cmp_dirs[1])
    if args.nameID:
        now_date_time = ''
        nameID = args.nameID
        diff_file_name = f'diff_file_list_{nameID}.lst'
    else:
        now_date_time = datetime.datetime.now().strftime('%d-%m-%Y-H%H')
        nameID = ''
        diff_file_name = f'diff_file_list_{now_date_time}.lst'
    diff_file_number = 0
    while os.path.exists(diff_file_name):
        diff_file_number += 1
        diff_file_name = f'diff_file_list_{nameID}{now_date_time}({str(diff_file_number)}).lst'

    for folder in (folder1, folder2):
        if not os.path.isdir(folder):
            logger.critical(f'directory {folder} does not exist')
            sys.exit(-1)

    CR = '\n'
    logger.info(f'{CR}comparing folders:{CR}   {folder1}{CR}   {folder2}{CR}')

    logger.info('checking for matching file counts in directories')
    match_pass = True
    results = compare(folder1, folder2)
    left_right = ('left', 'right')
    diff_file = open(diff_file_name, 'w')
    for each_side in left_right:
        if each_side == 'left':
            foldera = folder1
            folderb = folder2
        else:
            folderb = folder1
            foldera = folder2
        num_missmatched_files = len(results[each_side])
        if num_missmatched_files != 0:
            if verbose:
                basename_a = os.path.basename(foldera)
                basename_b = os.path.basename(folderb)
                diff_file.write(f'{num_missmatched_files} files found in:{CR}    {baseame_a} that are not in{CR}    {basename_b)}:')
                for file in results[each_side]:
                    diff_file.write(file)
            logger.info(f'{num_missmatched_files} files found in {os.path.basename(foldera)} that are not in {os.path.basename(folderb)}')
            match_pass = False
    if match_pass:
        logger.info(f"Both directorires: {os.path.basename(foldera)} and {os.path.basename(folderb)} match with "
                    f"{len(results['both'])} distinct files and directories")
    else:
        logger.info(f"Total number of distinct files and directories found in both: {len(results['both'])}")

    logger.info('checking for file differences...')
    ignore_list = ['INPUT', 'RESTART', 'logs']
    compare_files = filecmp.dircmp(folder1, folder2, ignore=ignore_list)
    ignore_list = ["log", "pathname", "prep_ran"]
    check_diff_files(compare_files, ignore_list)
    logger.info(f'Total number of files common to both experiments: {files_compared} of which {total_num_diff_files} differed')
    elapsed_time = time.process_time() - process_time
    logger.info(f'Results written to file: {diff_file_name}')
    logger.info(f'comparing fv3gfs output directories completed. Time to process({elapsed_time:4.4f} seconds)')
    diff_file.close()
