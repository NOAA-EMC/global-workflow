'''
Program Name: gfs_parallel_output_verification_cfp.py
Developer: Lin Gan
Description: This python script dynamically create cfp job, submit
Input Environment Variable:
  GAN_MPI_RANK_CT - MPI RANK count from the input CMD file
  LIN_MPI_RANK_CMD_DATA - The DATA directory for CMD process
'''

import os
import sys
from pathlib import Path

print("BEGIN: " + os.path.basename(__file__))

# Read in environment variables
gan_mpi_rank_ct = os.environ['GAN_MPI_RANK_CT']
lin_mpi_rank_cmd_data = os.environ['LIN_MPI_RANK_CMD_DATA']
cfp_cmd_file_name = os.path.join(lin_mpi_rank_cmd_data + '/differ_netcdf_nccmp.sh')
job_card_filename = "parallel_comparison_cfp.sh"
# Exception Handling - Ensure DATA directory exist
if not os.path.isfile(cfp_cmd_file_name):
    sys.exit(f"TERMINATING: The file '{cfp_cmd_file_name}' was not found.")

cfp_node_ct = int(gan_mpi_rank_ct) // 256
if cfp_node_ct < 1:
    cfp_node_ct = 1
cfp_proc = cfp_node_ct * 256
# Create job card
os.chdir(lin_mpi_rank_cmd_data)
current_dir = Path.cwd()
print("Current Python DATA is " + str(current_dir))
print("Writing job card to " + str(job_card_filename))
with open(job_card_filename, 'w') as job_card:
    job_card.write('#!/bin/sh\n')
    job_card.write('#PBS -q debug\n')
    job_card.write('#PBS -A GFS-DEV\n')
    job_card.write('#PBS -V \n')
    job_card.write('#PBS -N parallel_comparison_cfp\n')
    job_card.write('#PBS -j oe \n')
    job_card.write('#PBS -l walltime=00:30:00\n')
    job_card.write('#PBS -l debug=true\n')
    job_card.write('#PBS -l hyper=true\n')
    job_card.write('#PBS -l place=vscatter:excl,select=' + str(cfp_node_ct) + ':ncpus=128:mpiprocs=256:mem=500GB\n')
    job_card.write('\n')
    job_card.write('set -x \n')
    job_card.write('module load intel craype cray-mpich cray-pals cfp netcdf nccmp\n')
    job_card.write('module list   \n')
    job_card.write('cd ' + lin_mpi_rank_cmd_data + '\n')
    job_card.write('\n')
    job_card.write('mpiexec -n ' + str(cfp_proc) + ' -ppn 256 --cpu-bind thread cfp differ_netcdf_nccmp.sh')
    job_card.write('\n')
    job_card.write('status=$? \n')
    job_card.write('if [ $status -ne 0 ]; then \n')
    job_card.write('  exit $status \n')
    job_card.write('fi \n')

# Submit job card
print("Submitting " + job_card_filename)
os.system('qsub ' + job_card_filename)

print("END: " + os.path.basename(__file__))
