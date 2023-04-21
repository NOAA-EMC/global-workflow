#!/usr/bin/env python3

import filecmp
import collections
import os,sys
from pathlib import Path

from pygw.logger import Logger
logger = Logger(level='DEBUG', colored_log=True)

from pygw.executable import Executable

def get_args():
    import argparse
    import json
    parser = argparse.ArgumentParser()
    #group  = parser.add_mutually_exclusive_group(required=True)
    parser.add_argument('--cmp_dirs',nargs=2,metavar=('ROTDIR_baseline','ROTDIR_testrun'),help='compare COMROT foloders')
    parser.add_argument('--cmp_dirs_joblevel', nargs=1, metavar=('file_list.yml'), help='use stored job level file list when comparing ROTDIRs')
    parser.add_argument('--cmp_jobs',nargs=3,metavar=('job_name','ROTDIR','file_list.yml'),help='compare files at the job level (uses file_list.yml to track)')
    parser.add_argument('-n','--nameID',dest="nameID",help='tag name for compare (used in output filename)')
    parser.add_argument('-vt','--verbose_tar', help='include names of differing files witin tar files', action='store_true',default=False)
    args = parser.parse_args()
    if args.cmp_dirs is not None:
        for dirs in args.cmp_dirs:
            if not Path(dirs).is_dir():
                logger.critical('directory %s does not exsist'%dirs)
                sys.exit(-1)
    return args


def compare(folder1, folder2 ):
    return _recursive_dircmp(folder1, folder2)

def _recursive_dircmp(folder1, folder2 ):

    comparison = filecmp.dircmp(folder1, folder2)
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

    tar1 = tarfile.open( tar_file_one, mode="r" )
    tar2 = tarfile.open( tar_file_two, mode="r" )
    chunk_size = 100*1024

    for member1,member2 in list(zip(tar1, tar2)):
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
    comp_tars.append( tar_file_one )
    comp_tars.append( tar_file_two )
    diff_members = []

    for tar_file in comp_tars:

        tar = tarfile.open( tar_file, mode="r" )

        chunk_size = 100*1024
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
#    Returns one of three strings based on the NetCDF version of the
#    given file, or returns None if the file is not NetCDF:
#     *  "CDF1" = NetCDF classic format
#     *  "CDF2" = NetCDF 64-bit offset format
#     *  "HDF5" = HDF5 file, and hence possibly a NetCDF4 file.
#     *  None   = Not NetCDF and not HDF5
    import codecs
    with open(filename,'rb') as f:
        eight=f.read(8)
        if len(eight)<4:
            return None
        four=eight[0:4]
        if four==b'CDF\x01':
            return "CDF1"
        elif four==b'CDF\x02':
            return "CDF2"
        elif eight==b'\x89\x48\x44\x46\x0d\x0a\x1a\x0a':
            return "HDF5"
    return None


def print_diff_files(dcmp):

    import tarfile
    import subprocess
    from subprocess import run

    global diff_file; global cwd; global verbose
    global fixed_dir_experment_name
    if len(dcmp.common_dirs) != 0:
        logger.info('checking directories: %s'%' '.join(dcmp.common_dirs))
    if len( dcmp.diff_files ) == 0 and len(dcmp.common_files) != 0:
        logger.info('out of %d common files no differences found'%len(dcmp.common_files))
    file1_shortpath = '/'+dcmp.left.replace(cwd,'').replace(fixed_dir_experment_name,'').lstrip('/')
    if verbose:
        logger.info('checked in directory %s'%(file1_shortpath))
    if len( dcmp.diff_files) != 0 and verbose:
        number_netcdf_files = len([s for s in dcmp.diff_files if '.nc' in s])
        logger.info('checking %d differing files of which %d are NetCDF and some may be tar files'%(len(dcmp.diff_files),number_netcdf_files))
    num_netcdf_differing_files = 0
    num_netcdf_differing_files_onlyheader = 0
    num_tar_differing_files = 0
    num_identified_tar_files = 0
    num_differing_files = 0
    for name in dcmp.diff_files:
        file1 = os.path.join(dcmp.left,name); file2 = os.path.join(dcmp.right,name)
        file1_shortpath = '/'+dcmp.left.replace(cwd,'').replace(fixed_dir_experment_name,'').lstrip('/')
        file2_shortpath = '/'+dcmp.right.replace(cwd,'').replace(fixed_dir_experment_name,'').lstrip('/')
        if '.nc' in name:
            net_cdf_type = netcdfver(file1)
            if net_cdf_type is not None:
                if verbose:
                    netcdf_diff_output = NCCMP("--threads=4", "--data", file1, file2)
                else:
                    netcdf_diff_output = NCCMP("--diff-count=3", "--threads=4", "--data", file1, file2)
                if netcdf_diff_output is None:    
                #if len(netcdf_diff_output) == 0:
                    diff_file.write('NetCDF file %s of type: %s differs only in the header in directories %s and %s\n'%(name,net_cdf_type,file1_shortpath,file2_shortpath))
                    num_netcdf_differing_files_onlyheader += 1
                else: 
                    diff_file.write( 'NetCDF file %s of type: %s differs %s in directories %s and %s\n'%(name,net_cdf_type,netcdf_diff_output,file1_shortpath,file2_shortpath))
                    num_netcdf_differing_files += 1
        elif tarfile.is_tarfile(file1):
            num_identified_tar_files += 1
            if verbose:
                diff_tar_members = tarcmp_verbose( file1, file2 )
                if len(diff_tar_members) != 0:
                    for tar_file in diff_tar_members:
                        diff_file.write('tar member file %s differs in tar file %s from directories %s and %s\n' % (tar_file, name, file1_shortpath, file2_shortpath))
            if not tarcmp( file1, file2 ):
                diff_file.write('tar file %s differs in directories %s and %s\n' % (name, file1_shortpath, file2_shortpath))
                num_tar_differing_files += 1
        else:
            diff_file.write('file %s differs in directories %s and %s\n'% (name, file1_shortpath, file2_shortpath))
            num_differing_files += 1
        diff_file.flush()
    if num_netcdf_differing_files != 0:
        logger.info('%d NetCDF files differed'%num_netcdf_differing_files)
    if num_tar_differing_files != 0:
        logger.info('%d tar files differed'%num_tar_differing_files)
    if num_differing_files != 0:
        logger.info('%d files differed that was not NetCDF nor tar files'%num_differing_files)
    if verbose:
        if num_netcdf_differing_files == 0 and num_tar_differing_files == 0 and num_differing_files == 0 and len(dcmp.diff_files) != 0:
            if num_identified_tar_files == len(dcmp.diff_files):
                logger.info('all of the %d potentially differeing files where acctually non-differing tar files'%len(dcmp.diff_files))
            elif len(dcmp.diff_files) == num_netcdf_differing_files_onlyheader:
                logger.info('all of the %d potentially differeing files where acctually non-differing NetCDF files (only headers differed)'%len(dcmp.diff_files))
            else:
                logger.info('of the %d potentially differeing %d NetCDF differed %d tar files differedl, and %d differed that where not NetCDF or tar'%(len(dcmp.diff_files),num_netcdf_differing_files,num_tar_differing_files,num_differing_files))

    for sub_dcmp in dcmp.subdirs.values():
        print_diff_files(sub_dcmp)

def capture_files_dir( input_dir ):

    #current_file_list = collections.defaultdict(list)
    current_file_list = []
    for path, subdirs, files in os.walk(input_dir):
        for name in files:
            current_file_list.append( os.path.join(path, name) )
    return current_file_list

if __name__ == '__main__':

    import datetime
    import time
    import yaml

    fixed_dir_experment_name = 'fv3gfs_regression_experments'

    NCCMP = Executable("nccmp")
    print(NCCMP("--help"))

    args = get_args()

    process_time = time.process_time()

    verbose = args.verbose_tar
    file_dic_list = collections.defaultdict(list)

    if args.cmp_jobs is not None:

        job_name = args.cmp_jobs[0]
        ROTDIR =  args.cmp_jobs[1]
        ROTDIR_Path = Path( args.cmp_jobs[1] )
        if not ROTDIR_Path.is_dir():
            logger.critical('ROTDIR %s is not a direcy')
            sys.exit(-1)
        yaml_files_filename =  os.path.realpath( args.cmp_jobs[2] )
        logger.info('determining job level files for job %s in file %s'%(job_name, os.path.basename(yaml_files_filename)))
        file_list_current = capture_files_dir( ROTDIR )
        yaml_files_filename_Path = Path(yaml_files_filename)
        if yaml_files_filename_Path.is_file():
            yaml_files_fptr = open(  yaml_files_filename )
            file_dic_list = yaml.load( yaml_files_fptr  )
            yaml_files_fptr.close()

        if 'prior_ROTDIR' in file_dic_list:
            result = []
            for file in file_list_current:
                if file not in file_dic_list['prior_ROTDIR']:
                    result.append(file)
            file_dic_list[job_name] = result
        else:
            file_dic_list[job_name] = file_list_current
            
        file_dic_list['prior_ROTDIR'] = file_list_current
        logger.info('write out file %s'%yaml_files_filename )
        with open(yaml_files_filename, 'w') as outfile:
            yaml.dump(file_dic_list, outfile, default_flow_style=False)

    if  args.cmp_dirs is None:
        logger.info( 'compare_folders script is being used to capture job level files only and is quitting')
        sys.exit(0)

    folder1 = os.path.realpath( args.cmp_dirs[0] )
    folder2 = os.path.realpath( args.cmp_dirs[1] )

    if args.nameID:
        now_date_time  = ''; nameID = args.nameID
        diff_file_name = 'diff_file_list_%s.lst'%nameID
    else:
        now_date_time  = datetime.datetime.now().strftime('%d-%m-%Y-H%H')
        nameID = ''
        diff_file_name = 'diff_file_list_%s.lst'%now_date_time
    diff_file_number = 0
    while os.path.exists(diff_file_name):
        diff_file_number += 1
        diff_file_name = 'diff_file_list_%s%s(%s).lst'%(nameID,now_date_time,str(diff_file_number))

    for folder in (folder1,folder2):
        if not os.path.isdir(folder):
            logger.critical('directory %s does not exsist'%folder)
            sys.exit(-1)

    cwd = os.getcwd()

    logger.info('comparing folders:\n   %s\n   %s'%(folder1,folder2))
    logger.info('checking for matching file counts in directories')

    results = compare(folder1, folder2)
    left_right = ('left','right')
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
                diff_file.write('%d files found in %s that are not in %s:'%(num_missmatched_files,os.path.basename(foldera),os.path.basename(folderb)))
                for file in results[each_side]:
                    diff_file.write('   %s'%file)
            logger.info('%d files found in %s that are not in %s:'%(num_missmatched_files,os.path.basename(foldera),os.path.basename(folderb)))
    logger.info('checking for file differences...')
    egnore_file_list = ['*.log','INPUT','RESTART','logs']
    compare_files = filecmp.dircmp(folder1, folder2, egnore_file_list)
    diff_file = open( diff_file_name, 'w')
    print_diff_files( compare_files )
    elapsed_time = time.process_time() - process_time 
    logger.info('comparing fv3gfs output directories completed. Time to process(%.4f seconds)'%elapsed_time)
    diff_file.close()
