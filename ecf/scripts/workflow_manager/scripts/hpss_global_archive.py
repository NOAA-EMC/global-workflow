'''
Program Name: hpss_global_archive.py
Developer: Lin Gan
Description: This python script dynamically create a transfer job, submit, and  upload file to hpss
Input Environment Variable:
  machine - Currently support WCOSS2
  NET - parallel env
  RUN - parallel env
  HOMEgfs - parallel env
  EXPDIR - parallel env
  QUEUE - parallel env
  ACCOUNT - parallel env
  nproc - wcoss2 default
'''

import os

print("BEGIN: "+os.path.basename(__file__))

# Read in environment variables
machine = os.environ['machine']
NET = os.environ['NET']
RUN = os.environ['RUN']
JOB_LOG_DIR = os.environ['JOB_LOG_DIR']
QUEUE_ARCH = os.environ['QUEUE_ARCH']
ACCOUNT = os.environ['ACCOUNT']
ARCH_LIST = os.environ['ARCH_LIST']
SOURCE_DIR = os.environ['SOURCE_DIR']
HPSS_TARGET_DIR = os.environ['HPSS_TARGET_DIR']
TRANSFER_TARGET_FILE = os.environ['TRANSFER_TARGET_FILE']
PDY = os.environ['PDY']
CYC = os.environ['cyc']

# Create job card directory and file name
if not 'TRANSFER_TARGET_FILE_2D' in os.environ:
    TRANSFER_TARGET_FILE_2D = TRANSFER_TARGET_FILE
else:
    TRANSFER_TARGET_FILE_2D = os.environ['TRANSFER_TARGET_FILE_2D']
if not os.path.exists(JOB_LOG_DIR):
    os.makedirs(JOB_LOG_DIR)
job_name = RUN+'_HPSS_ARCHIVE_'+TRANSFER_TARGET_FILE+'_'+PDY+CYC
job_card_filename = os.path.join(JOB_LOG_DIR,job_name+'.sh')
job_output_filename = os.path.join(JOB_LOG_DIR,job_name+'.out')
if os.path.exists(job_output_filename):
    os.remove(job_output_filename)

# Create job card
print("Writing job card to "+job_card_filename)
with open(job_card_filename, 'w') as job_card:
    if machine == 'WCOSS2':
        job_card.write('#!/bin/sh\n')
        job_card.write('#PBS -q '+QUEUE_ARCH+'\n')
        job_card.write('#PBS -A '+ACCOUNT+'\n')
        job_card.write('#PBS -V \n')
        job_card.write('#PBS -N '+job_name+'\n')
        job_card.write('#PBS -o '+job_output_filename+'\n')
        job_card.write('#PBS -e '+job_output_filename+'\n')
        job_card.write('#PBS -l walltime=05:55:00\n')
        job_card.write('#PBS -l debug=true\n')
        job_card.write('#PBS -l place=vscatter,select=1:ncpus=1:mem=30GB')
        job_card.write('\n')
        job_card.write('set -x \n')
        job_card.write('\n')
        job_card.write('#### '+HPSS_TARGET_DIR+'/'+TRANSFER_TARGET_FILE_2D+'.tar '+ARCH_LIST+'/'+TRANSFER_TARGET_FILE+'.txt'  ' \n')
        job_card.write('cd '+SOURCE_DIR+'\n')
        job_card.write('\n')
        job_card.write('htar -P -cvf '+HPSS_TARGET_DIR+'/'+TRANSFER_TARGET_FILE_2D+'.tar `cat '+ARCH_LIST+'/'+TRANSFER_TARGET_FILE+'.txt`')
        job_card.write('\n')
        job_card.write('status=$? \n')
        job_card.write('if [ $status -ne 0 ]; then \n')
        job_card.write('  exit $status \n')
        job_card.write('fi \n')

# Submit job card
print("Submitting "+job_card_filename+" to "+QUEUE_ARCH)
print("Output sent to "+job_output_filename)
if machine == 'WCOSS2':
    os.system('qsub '+job_card_filename)

print("END: "+os.path.basename(__file__))
