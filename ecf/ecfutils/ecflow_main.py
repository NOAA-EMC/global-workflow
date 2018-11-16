'''
Created on Jul 11, 2018

@author: jiankuang
'''

import sys, os;

sys.path.append(os.getcwd() + "/CROW")

os.environ['ECF_HOME'] = os.getcwd()
os.environ['ECF_ROOT'] = os.getcwd()
os.environ['ECF_HOST'] = "ldecflow1"
os.environ['ECF_PORT'] = "32065"

#os.remove('head.h')
#os.remove('tail.h')
#os.remove("envir-xc40.h")

import worktools; 

# Edit your writting directory and comment out the following line:
# output = '/Users/jiankuang/Documents/Eclipse_workspace//expdir/casetest1'

if __name__ == '__main__':
    print(os.getcwd())
    worktools.make_ecflow_files_for_cycles(output,'2015112500','2015112506')
