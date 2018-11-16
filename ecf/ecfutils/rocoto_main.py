'''
Created on Jul 11, 2018

@author: jiankuang
'''

import sys, os;

sys.path.append("/Users/jiankuang/Documents/Eclipse_workspace/ecfutils_007/CROW")

import worktools ; 
if __name__ == '__main__':
    worktools.make_rocoto_xml_for(os.getcwd() + '/expdir/casetest1')