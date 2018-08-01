#!/usr/bin/env python
#
##@namespace rocoto_viewer
# @brief A Curses based terminal viewer to interact and display the status of a Rocoto Workflow in real time.
#
# @anchor rocoto_viewer
## This Python script allows users to see and interact with a running Rocoto Workflow in real time.
# \image html pythonCurses.jpeg "Rocoto Viewer for Displaying Real-time Status of Workflow" 
#
# To launch this viewer simply give it the database and the XML files being used by the \b Rocoto system for your experiment:
#
#      rocoto_viewer.py -w my_gfs-workflow.xml -d my_database.db
#
# The script is located in the directory para/exp/rocoto/rocotoviewers/rocotoviewer_curses/rocoto_viewer.py
# The view will continuously update every four minutes and reflect the current status of your workflow.  You may use your mouse or arrow keys to select a particular task and view its status details by pressing the key \p c as indicated as \b \<c\> (which runs \b rocotocheck) or perform a \b rocotorewind by pressing \b \<r\> to restart the workflow at that point. Running \b rocotorewind causes the state information of that task to be cleared from the database and resubmits the job to the scheduler.
#
# Tasks marked with the \b \< symbol are \b metatasks and can be expanded by highlight that task with the mouse, and then clicking on the \b \< symbol which then changes to \b \> . You can then click on the \b \> symbol to collapse it again. Alternatively, you can select the 'x' to expand and collapse metatasks when selected.
#
##@cond ROCOTO_VIEWER_CURSES

from __future__ import division 

import curses

import os, sys, getpass, getopt, signal, tempfile
from os.path import basename
import subprocess
from math import *

from  __builtin__ import any as b_any
from os.path import realpath, normpath, dirname, getsize
from io import StringIO
from itertools import groupby
from time import time
from multiprocessing import Process, Queue
import time as std_time
from datetime import datetime, timedelta
import uuid
import shutil

import sqlite3,datetime,collections
import xml.etree.ElementTree as ET
import cPickle

try:
    from dateutil.relativedelta import relativedelta
except ImportError:
    #print 'dateutil which uses relativedelta to increment monthly (used by UGCS) is not supported with this version of python.  Use Anaconda the native version in /user/bin'
    #sys.exit(1)
    pass

# Global Variables
database_file_agmented = None
use_performance_metrics = False
default_column_length = 125
stat_read_time_delay = 3*60
temp_workflow_file = ''
header_string = ''
format_string = "jobid slots submit_time start_time cpu_used run_time delimiter=';'"

ccs_html='''
<html>

<head>
<META HTTP-EQUIV="refresh" CONTENT="180">
<style type="text/css">

thead { font-weight:bold; }

red { background-color:red }
blue { background-color:lightblue }
green { background-color:lightgreen }
yellow { background-color:yellow }

td, th {
  border: 2px solid #999;
  padding: 0.5rem;
}

table {
  border-collapse: collapse;
}

</style>
</head>
'''
bottom_message_scroll = '<c>heck <b>oot <r>ewind <R>un (->) Next Cycle (<-) Previous Cycle <u>p <d>own <h>elp <Q>uit'
bottom_message        = '<c>heck <b>oot <r>ewind <R>un (->) Next Cycle (<-) Previous Cycle <h>elp <Q>uit'

#Global Variables
#================
list_tasks = False
html_output = False
html_output_file = None
rzdm_path = ''
only_check_point = False
save_checkfile_path = None
use_multiprocessing = True
get_user = getpass.getuser()

screen_resized = False
debug = None

mlines = 0
mcols = 0

def sigwinch_handler(signum, frame):
    global screen_resized
    global mlines
    global mcols
    term_size = subprocess.Popen(['stty', 'size'], stdout=subprocess.PIPE)
    try:
        get_term_size, err = term_size.communicate()
    except:
        return
    mlines,mcols = map(int,get_term_size.split())
    screen_resized = True

def usage(message=None):
    curses.endwin()
    print>>sys.stderr, '''
Usage: rocoto_status_viewer.py  -w workflow.xml -d database.db [--listtasks]\n                                                               [--html=filename.html]\n                                                               [--perfmetrics={True,False}]

Mandatory arguments:
  -w workflow.xml
  -d database.db
Optional arguments:
  --listtasks             --- print out a list of all tasks
  --html=filename.html    --- creates an HTML document of status
  --perfmetrics=True      --- turn on/off extra columns for performance metrics 
  --help                  --- print this usage message'''

    if message is not None:
        print>>sys.stderr,'\n'+str(message).rstrip()+'\n'
    sys.exit(-1)

def augment_SQLite3(filename):

    connection=sqlite3.connect(filename)
    c=connection.cursor()
    #qinfo=c.execute("DROP TABLE IF EXISTS jobs_augment;")
    qinfo=c.execute("PRAGMA table_info(jobs_augment)").fetchall()
    if any('qtime' in element for element in qinfo):
        c.close()
        return 'is_already_augmented'
    else:
        sql_create_augment_table = "CREATE TABLE jobs_augment AS SELECT * FROM jobs;"
        q=c.execute(sql_create_augment_table)
        q=c.execute("alter table jobs_augment add column qtime integer;")
        q=c.execute("alter table jobs_augment add column cputime integer;")
        q=c.execute("alter table jobs_augment add column runtime integer;")
        q=c.execute("alter table jobs_augment add column slots integer;")
        connection.commit()

    c.close()
    database_file = filename
    return 'now_augmented'

def isSQLite3(filename):
    from produtil.fileop import check_file
    from produtil.fileop import deliver_file
    if not check_file(filename):
        return False
    if getsize(filename) < 100:
        return False
    with open(filename, 'rb') as fd:
        header = fd.read(100)
        fd.close()
    if not header[:16] == 'SQLite format 3\x00':
        return False
    else:
        return True

def isRocotoWorkflow(filename):
    from produtil.fileop import check_file
    if not check_file(filename):
        return False
    with open(filename, 'r') as input:
        for line in input:
            if 'DOCTYPE workflow' in line:
                input.close()
                return True
    return False


def load_produtil_pythonpath():

    try:
        import produtil.cluster
        return True
    except ImportError:
        pass

    PRODUTIL = collections.defaultdict(list) 
    PRODUTIL['theia'] = '/scratch4/NCEPDEV/global/save/glopara/svn/nceplibs/produtil/trunk/ush'
    PRODUTIL['luna']  = '/gpfs/hps3/emc/global/noscrub/emc.glopara/svn/nceplibs/produtil/trunk/ush'
    PRODUTIL['tide']  = '/gpfs/td1/emc/global/save/emc.glopara/svn/nceplibs/produtil/trunk/ush'
    PRODUTIL['gyre']  = '/gpfs/gd1/emc/global/save/emc.glopara/svn/nceplibs/produtil/trunk/ush'
    try_clusters = ('theia','luna','tide','gyre')

    for cluster in try_clusters:
        sys.path.append(PRODUTIL[cluster])
        try:
            import produtil.cluster
            return True
        except ImportError:
            pass
    return False

def get_arguments():
    from produtil.fileop import check_file
    short_opts = "w:d:f:"
    long_opts  = ["checkfile=","workfolw=","database=","html=","listtasks","onlycheckpoint","help","perfmetrics="]
    try:
        opts, args = getopt.getopt(sys.argv[1:], short_opts, long_opts)
    except getopt.GetoptError as err:
        print str(err)
        print
        usage('SCRIPT IS ABORTING DUE TO UNRECOGNIZED ARGUMENT')

    global save_checkfile_path
    global use_performance_metrics
    workflow_file = None
    database_file = None
    perfmetrics_on = None
    for k, v in opts:
        if  k in ('-w', '--workflow'):
            workflow_file = v
        elif k in ('-d','--database'):
            database_file = v
        elif k in ('-f','--checkfile'):
            save_checkfile_path = v
        elif k in ('--perfmetrics'):
            perfmetrics_on = v
        elif k in ('--listtasks'):
            global list_tasks
            list_tasks = True
        elif k in ('--onlycheckpoint'):
            global only_check_point
            only_check_point = True
        elif k in ('--html'):
            global html_output
            global rzdm_path
            global send_html_to_rzdm
            send_html_to_rzdm = True
            rzdm_path = v
            html_output = True
        elif k in ('--help'):
            usage('')
        else:
            pass
            #usage('OPTION NOT REGOGNIZED')

    if perfmetrics_on is None:
        use_performance_metrics = False
    elif perfmetrics_on.lower() == 'true':
        use_performance_metrics = True
    elif perfmetrics_on.lower() == 'false':
        use_performance_metrics = False
    elif perfmetrics_on is not None:
                usage('perfmetrics must be either set to true or false (e.g. --perfmetrics=True')
        
    send_html_to_rzdm = False
    if len(rzdm_path) != 0:
        if ':' not in rzdm_path or '@' not in rzdm_path:
            print 'No user name or path found for sending html directory to server, no files will be sent to rzdm'
            print 'Creating html folder in: %s'%rzdm_path
        else:
            send_html_to_rzdm = True

    if list_tasks and workflow_file is None:
                usage('In order to list tasks you must supply the XML worflow-file')

    if only_check_point and (workflow_file is None or database_file is None or save_checkfile_path is None):
        usage('To use the check point output you must specify the workflow, data base, and the specific name of the checkpoint file')

    if (not list_tasks) and (workflow_file is None or database_file is None):
            usage('Booth database-file and workflow-file must be specified')

    if (not list_tasks) and (workflow_file is not None and database_file is not None):
        #debug.write('database_file_agmented: '+database_file_agmented+'\n')
        if not isSQLite3( database_file ):
            usage('%s is not SQLite3 database file'%database_file)
        if not isRocotoWorkflow( workflow_file ):
            usage('%s is not an Rocoto XML file'%workflow_file)

    #global use_multiprocessing
    #if getsize(database_file) < 104857600:
    #   use_multiprocessing = True
    #else:
    #   use_multiprocessing = True

    return (workflow_file,database_file )


def get_entity_values( workflow_file ):

    entity_values = collections.defaultdict(list)
    with open( workflow_file, 'rw' ) as f:
        for line in f:
            split_line = line.split()
            if ']>' in line:
                break
            if 'ENTITY' in line:
                if 'SYSTEM' in line:
                    value =  split_line[3]
                else:
                    value =  split_line[2]
                entity_values[ split_line[1] ] = value[:-1].replace('"','')
    return entity_values

def timedelta_total_seconds(timedelta):
    return (
        timedelta.microseconds + 0.0 +
        (timedelta.seconds + timedelta.days * 24 * 3600) * 10 ** 6) / 10 ** 6

def get_aug_perf_values( username ):
    from produtil.run import run,runstr, batchexe
    global html_ouput
    global format_keys
    cmd = batchexe('which') ['bjobs']
    try:
        which_bjobs = runstr(cmd).strip()
    except Exception,e:
        return None
    bjobs = collections.defaultdict(dict)
    aug_perf = collections.defaultdict(dict)
    cmd = batchexe( which_bjobs )['-a','-o',format_string,'-u',username]
    bjobs_line = runstr(cmd)
    if 'No job found' in bjobs_line:
        return None
    bjobs_lines = bjobs_line.split('\n')
    for l,line in enumerate(bjobs_lines):
        split_line = line.split(';')
        if l == 0:
            format_keys = split_line
            continue
        for i, value in enumerate(split_line):
            if i == 0:
                key = value 
            else:
                if format_keys[i] in ('RUN_TIME','CPU_USED'):
                    value_list = value.split()
                    if len(value_list) > 1:
                        value = value_list[0]
                bjobs[key][format_keys[i]] = value
    sub_time_string = ''
    year = str(datetime.datetime.now().year)+' '
    sub_time = None
    bstart_time = None
    for jobid,keys in bjobs.iteritems():
        #debug.write(jobid+'\n')
        for key in keys:
            #debug.write('   '+key+":"+bjobs[jobid][key]+'\n')
            try:
                int_key = int(bjobs[jobid][key].strip())
                str_key = str(int_key)
            except:
                str_key = bjobs[jobid][key].strip()

            if key == 'SUBMIT_TIME':
                sub_time_string =  str_key
                try:
                    sub_time = datetime.datetime.strptime( year+sub_time_string, '%Y %b %d %H:%M' )
                except:
                    sub_time = None
                continue
            elif key == 'START_TIME':
                bstart_time_string =  str_key
                try:
                    bstart_time = datetime.datetime.strptime( year+bstart_time_string, '%Y %b %d %H:%M' )
                except:
                    bstart_time = None
                continue
            elif key == 'RUN_TIME':
                aug_perf[jobid]['runtime'] = str_key
            elif key == 'CPU_USED':
                aug_perf[jobid]['cputime'] = str_key
            elif key == 'SLOTS':
                aug_perf[jobid]['slots'] = str_key

        if bstart_time_string == sub_time_string:
            aug_perf[jobid]['qtime'] = '0'
        elif sub_time is not None and bstart_time is None :
            try:
                aug_perf[jobid]['qtime'] = str(int(( datetime.datetime.now() - sub_time ).total_seconds()))
            except AttributeError:
                aug_perf[jobid]['qtime'] = str(int(timedelta_total_seconds( datetime.datetime.now() - sub_time )))

        elif sub_time is not None and bstart_time is not None:
            try:
                aug_perf[jobid]['qtime'] = str(int((bstart_time - sub_time).total_seconds()))
            except AttributeError:
                aug_perf[jobid]['qtime'] = str(int(timedelta_total_seconds(bstart_time - sub_time)))
        else:
            aug_perf[jobid]['qtime'] = '-'

    return aug_perf

def help_screen( screen ):

    max_row = 25
    box_cols = 60
    box = curses.newwin( max_row, box_cols  , 5, 32 )
    box.box()
    box.border(0)
    box.addstr( 0 , 23, '<q> when done', curses.A_BOLD )
    helpstr= [ '<c>heck  : run rocotocheck  on selected task(s)',
               '<b>oot   : run rocotoboot   on selected task(s)', 
               '<r>ewind : run rocotorewind on selected task(s)',
               '<R>un    : run rocotorun    on selected task(s)',
               ' ',
               '(->) Next Cycle      <d>own (or) Page-dwn to scroll',
               '(<-) Previous Cycle  <u>own (or) Page-up  to scroll ',
               ' ',
               '<Shift> + Arrow Up     to selected multiple tasks',
               '<Shift> + Arrow Down   for using with rocoto utils',
               'Double-Click  or  <x>  to expand/collapse metatasks',
               ' ',
               '<ENTER> Selects a task for list or opens meta-task list',
               ' ',
               '        When a meta-task list is open for selection:',
               '        Double-Click (or)  <s>  to select the begining',
               '        of a range for selection and repeate to complete',
               '        the desired selected list.',
               '',
               '<l>oads and renews status data (no rocotorun)',
               '<F>inds the last cycle with a running task',
               '<U>nloads and clears all previously seleted tasks',
               '<f>makes a symlink of log file of highlited task']

    for i in range(0,len(helpstr)):
        box.addstr( 1+i ,2, helpstr[i] )
    x = screen.getch()
    while x != ord('q'):
        x = screen.getch()
        box.refresh()
        
def list_selector( screen, selected_strings, strings ):

    global screen_resized
    global mlines
    global mcols
    global highlightText
    global highlightSelectedText
    global normalText

    def define_box():
    
        if len( strings ) < mlines:
            max_row = len( strings )
        else:
            max_row =  mlines - 12 
        max_mcols = max(18,len(max( strings, key=len )))
        if max_mcols + 8 < mcols:
            box_cols = max_mcols + 8
        else:
            box_cols = mcols - 3
        box = curses.newwin( max_row + 6, box_cols  , 4, 5 )
        box.box()
        box.border(0)

        return box, max_row, box_cols

    strings_selected = selected_strings
    string_ctr_selected = ''

    box, max_row, box_cols = define_box()
    row_num = len( strings )
    pages = int( ceil( row_num / max_row ) )
    position = 1
    page = 1
    for i in range( 1, max_row+1 ):
        if row_num == 0:
            box.addstr( 1, 1, "There aren't strings", highlightText )
        else:
            print_string = '  '+strings[ i - 1 ]+' '
            if (i == position):
                box.addstr( i+1, 2, print_string, highlightText )
            else:
                box.addstr( i+1, 2, print_string, normalText )
            if i == row_num:
                break

    screen_resized = False

    range_selected = False
    string_ctr_selected_prior = ''

    x = screen.getch()
    while x != ord('q'):

        if screen_resized:

            screen_resized = False
            curses.resizeterm(mlines, mcols)
            screen.refresh()
            box.clear()
            box.erase()

            box, max_row, box_cols = define_box()

            box.border( 0 )
            box.refresh()

        if x in ( curses.KEY_SF, curses.KEY_DOWN):
            if x == curses.KEY_SF:
                string_selected = strings[ position - 1 ]
                if string_selected in strings_selected:
                    string_ctr_selected = ''
                    try:
                        if len(strings_selected) > 0:
                            strings_selected.remove( string_selected )
                    except ValueError:
                        pass
                else:
                    strings_selected.append( string_selected )
            if page == 1:
                if position < i:
                    position = position + 1
                else:
                    if pages > 1:
                        page = page + 1
                        position = 1 + ( max_row * ( page - 1 ) )
            elif page == pages:
                if position < row_num:
                    position = position + 1
            else:
                if position < max_row + ( max_row * ( page - 1 ) ):
                    position = position + 1
                else:
                    box.erase()
                    box.border(0)
                    page = page + 1
                    position = 1 + ( max_row * ( page - 1 ) )
        if x in ( curses.KEY_SR, curses.KEY_UP):
            if x == curses.KEY_SR:
                string_selected = strings[ position - 1 ]
                if string_selected in strings_selected:
                    try:
                        if len(strings_selected) > 0:
                            strings_selected.remove( string_selected )
                    except ValueError:
                        pass
                else:
                    strings_selected.append( string_selected )
            if page == 1:
                if position > 1:
                    position = position - 1
            else:
                if position > ( 1 + ( max_row * ( page - 1 ) ) ):
                    position = position - 1
                else:
                    box.erase()
                    box.border(0)
                    page = page - 1
                    position = max_row + ( max_row * ( page - 1 ) )

        if x == curses.KEY_PPAGE:
            box.erase()
            box.border( 0 )
            if page > 1:
                page = page - 1
                position = 1 + ( max_row * ( page - 1 ) )

        if x == curses.KEY_NPAGE:
            box.erase()
            box.border( 0 )
            #screen.refresh()
            if page < pages:
                page = page + 1
                position = ( 1 + ( max_row * ( page - 1 ) ) )

        if x in ( curses.KEY_MOUSE, ord('s') ):
            mouse_id, mouse_x, mouse_y, mouse_z, button_state = (0,0,0,0,0)
            index_prior_selected = 0
            if x == curses.KEY_MOUSE:
                mouse_id, mouse_x, mouse_y, mouse_z, button_state = curses.getmouse()
                box.erase()
                box.border( 0 )
                pos = mouse_y-5
                if page == 1:
                    position = pos
                else:
                    position = max_row*(page-1)+pos

            if x == ord('s') or (button_state & curses.BUTTON1_DOUBLE_CLICKED):
                string_ctr_selected = strings[ position - 1 ]
                if range_selected:
                    range_selected = False
                    string_ctr_selected = ''
                    if string_ctr_selected != string_ctr_selected_prior:
                        index_prior_selected = strings.index(string_ctr_selected_prior)
                        if position < index_prior_selected:
                            first = position-1
                            last  = index_prior_selected+1
                        else:
                            first = index_prior_selected
                            last  = position
                        for i in range( first, last ):
                            if  strings[i] in strings_selected:
                                strings_selected.remove(strings[i])
                            else:
                                strings_selected.append( strings[i] )
                    string_ctr_selected_prior = ''
                else:
                    range_selected = True
                    string_ctr_selected_prior = string_ctr_selected

        if x in (curses.KEY_ENTER, 10, 13) and row_num != 0:
            box.border( 0 )
            string_selected = strings[ position - 1 ]
            if string_ctr_selected_prior == string_selected:
                string_ctr_selected_prior = ''
                range_selected = False
            if string_selected in strings_selected:
                try:
                    if len(strings_selected) > 0:
                        strings_selected.remove( string_selected )
                except ValueError:
                    pass
            else:
                strings_selected.append( string_selected )

        if x == ord('U'):
            for each_sting in strings:
                if each_sting in strings_selected:
                    if len(strings_selected) > 0:
                        strings_selected.remove(each_sting)
            
        for i in range( 1 + ( max_row * ( page - 1 ) ), max_row + 1 + ( max_row * ( page - 1 ) ) ):
            if row_num == 0:
                box.addstr( 1, 1, "There aren't strings",  highlightText )
            else:
                if strings[ i - 1 ] == string_ctr_selected_prior:
                    string_print = '* '+strings[ i - 1 ]+' ' 
                else:
                    string_print = '  '+strings[ i - 1 ]+' ' 

                start_pos =  i - ( max_row * ( page - 1 ) ) + 1
                if ( i + ( max_row * ( page - 1 ) ) == position + ( max_row * ( page - 1 ) ) ):
                    box.addstr( start_pos, 2, string_print, highlightText )
                else:
                    box.addstr( start_pos, 2, string_print, normalText )
                if strings[ i - 1 ] in strings_selected:
                    box.addstr( start_pos, 2, string_print[:1] )
                    box.addstr( start_pos, 4, string_print[2:-1],  highlightSelectedText | curses.A_DIM )
                if i == row_num:
                    break

        box.addstr( max_row+3 , 2, 'Select with <ENTER> or' )
        box.addstr( max_row+4 , 2, '<SHIFT> + <UP/DOWN>' )
        box.addstr( 0 , 7, '<q> when done', curses.A_BOLD )
        box.refresh()
        x = screen.getch()

    return strings_selected


def get_rocoto_check(params, queue_check):
    from produtil.run import run,runstr, batchexe, exe
    workflow_file, database_file, task, cycle, process = params
    cmd=batchexe('rocotocheck')['-v',10,'-w',workflow_file,'-d',database_file,'-c',cycle,'-t',task]
    check=runstr(cmd)
    if check is None:
        curses.endwin()
        print 'rcotocheck falied: %d'%stat
        sys.exit(-1)
    queue_check.put(check)

def rocoto_boot(params):
    from produtil.run import run,runstr, batchexe, exe
    workflow_file, database_file, cycle, metatask_list, task_list = params
    run( exe('yes') | exe('head')['-1']  > '.yes.txt')
    if len(task_list) == 0 and len(metatask_list) != 0:
        cmd=batchexe('rocotoboot')['--workflow', workflow_file,'--database',database_file,'--cycles',cycle,'--metatasks', metatask_list] < '.yes.txt'
    elif len(task_list) != 0 and len(metatask_list) == 0:
        cmd=batchexe('rocotoboot')['--workflow', workflow_file,'--database',database_file,'--cycles',cycle,'--tasks', task_list ] < '.yes.txt'
    elif len(task_list) != 0 and len(metatask_list) != 0:
        cmd=batchexe('rocotoboot')['--workflow', workflow_file,'--database',database_file,'--cycles',cycle,'--tasks', task_list, '--metatasks', metatask_list ] < '.yes.txt'
    else:
        return 'Warning: No metatasks or tasks where selected when rocotboot was called'
    stat=runstr(cmd)
    if stat is None:
        display_results( 'rcotoboot falied!!','')
    return stat

def rocoto_rewind(params):
    from produtil.run import run,runstr, batchexe
    workflow_file, database_file, cycle, process = params
    cmd=batchexe('rocotorewind')['-w',workflow_file,'-d',database_file,'-c',cycle,process]
    stat=runstr(cmd)
    if stat is None:
        display_results('rcotorewind falied!!','')
    return stat

def rocoto_run(params):
    from produtil.run import run,runstr, batchexe
    workflow_file, database_file = params
    cmd=batchexe('rocotorun')['-w',workflow_file,'-d',database_file]
    stat=runstr(cmd )
    stat = ''
    if stat is None:
        curses.endwin()
        print 'rcotorun falied: %d'%stat
        sys.exit(-1)
    return stat

def get_tasklist(workflow_file):
    import produtil.run, produtil.numerics
    tasks_ordered = []
    metatask_list = collections.defaultdict(list)
    tree = ET.parse(workflow_file)
    root = tree.getroot()
    cycledef_group_cycles = collections.defaultdict(list)
    if list_tasks:
        curses.endwin() 
        print
    cycle_noname = 'default_cycle' 
    for child in root:
        if child.tag == 'cycledef':
            if len(child.attrib) != 0:
                cycle_def_name = child.attrib['group']
            else:
                cycle_def_name = cycle_noname
            cycle_string = child.text.split()

            ucgs_is_cron = None
            if PACKAGE.lower() == 'ugcs':
                start_cycle = produtil.numerics.to_datetime ( entity_values['SDATE'] )
                end_cycle   = produtil.numerics.to_datetime ( entity_values['EDATE'] )
                #inc_cycle   = produtil.numerics.to_timedelta( entity_values['INC_MONTHS'] )
                #NOTE: this is for the special case when cycle for every month
                inc_cycle   = int(entity_values['INC_MONTHS'])
                if inc_cycle == 0:
                    inc_cycle   = produtil.numerics.to_timedelta( cycle_string[2] )
                    ucgs_is_cron = False
                else:
                    ucgs_is_cron = True
                only_once_ugcs = True
            else:
                start_cycle = produtil.numerics.to_datetime ( cycle_string[0] )
                end_cycle   = produtil.numerics.to_datetime ( cycle_string[1] )
                inc_cycle   = produtil.numerics.to_timedelta( cycle_string[2] )

            while  start_cycle <= end_cycle:
                cycledef_group_cycles[cycle_def_name].append(start_cycle.strftime("%Y%m%d%H%M"))
                if PACKAGE.lower() == 'ugcs' and ucgs_is_cron:
                    try:
                        start_cycle = start_cycle + relativedelta(months=+inc_cycle)
                    except AttributeError:
                        curses.endwin()
                        print;print
                        print 'dateutil which uses relativedelta to increment monthly (used by UGCS) is not supported with this version of python.\nUse Anaconda the native version in /user/bin'
                        sys.exit(-1)
                else:
                    start_cycle = start_cycle + inc_cycle
                #if list_tasks:
                #print 'cycledef=%s number of cycles %s inc: %s'%(cycle_def_name, len(cycledef_group_cycles[cycle_def_name]),inc_cycle)
                #print 'contails cycles',cycledef_group_cycles[cycle_def_name]
        if child.tag == 'task':
            task_name = child.attrib['name']
            log_file = child.find('join').find('cyclestr').text.replace( '@Y@m@d@H','CYCLE' )
            #if len(log_file) != 0:
            #   print 'LOG: %s %s'%( task_name, log_file )
            if 'cycledefs' in child.attrib:
                task_cycledefs = child.attrib['cycledefs']
                #if list_tasks:
                #   print 'task_cycledefs:',task_cycledefs
            else:
                task_cycledefs = cycle_noname
            if list_tasks:
                print task_name,task_cycledefs
                #dependancies = child.getiterator('dependency')
                #for dependency in dependancies:
                #   for them in dependency.getchildren():
                #       print them.attrib
            tasks_ordered.append((task_name,task_cycledefs,log_file))
        elif child.tag == 'metatask':
            all_metatasks_iterator = child.getiterator('metatask')
            all_vars = dict() ; all_tasks = []
            for i,metatasks in enumerate(all_metatasks_iterator):
                metatask_name = 'NO_NAME'
                try:
                    metatask_name = metatasks.attrib['name']
                except:
                    pass
                if list_tasks:
                    print ' '*i+'metatask:',metatask_name
                all_vars_list = metatasks.findall('var')
                all_tasks_list = metatasks.findall('task')
                for var in all_vars_list:
                    var_list_values = var.text.split()
                    #print ' '+'  '*i+'(%d) var name:'%i,var.attrib['name'],var_list_values
                    all_vars[var.attrib['name']] = var_list_values
                for task in all_tasks_list:
                    task_name = task.attrib['name']
                    task_log = task.find('join').find('cyclestr').text.replace( '@Y@m@d@H','CYCLE' )
                    #if len(task_log) != 0:
                    #   print 'LOG: %s %s'%( task_name, task_log)
                    #print ' '+'  '*i+'(%d) task name:'%i,task.attrib['name']
                    if 'cycledefs' in task.attrib:
                        task_cycledefs = task.attrib['cycledefs']
                        #if list_tasks:
                        #   print 'task_cycledefs (meta):',task_cycledefs
                    else:
                        task_cycledefs = cycle_noname
                    all_tasks.append((task_name,task_cycledefs,task_log))
                add_task = []
                for task_name in all_tasks:
                    first_task_resolved = False
                    first_task_resolved_name = ''
                    add_task[:] = []
                    add_task.append(task_name)
                    for name,vars in all_vars.iteritems():
                        replace_var = '#'+name+'#'
                        #print 'TASK_NAME: %s | %s'%(task_name,replace_var)
                        for each_task_name in add_task:
                        #for each_task_name in all_tasks:
                            if replace_var in each_task_name[0]:
                                for var in vars:
                                    new_task_name = each_task_name[0].replace(replace_var, var)
                                    new_task_log = each_task_name[2].replace(replace_var, var)
                                    add_task.append((new_task_name,each_task_name[1],new_task_log))
                        for task in add_task:
                            if '#' not in task[0]:
                                if task[0] not in [ j[0] for j in tasks_ordered]:
                                    tasks_ordered.append(task)
                                    if not  first_task_resolved:
                                        first_task_resolved = True
                                        first_task_resolved_name = task[0]
                                        if metatask_name == 'NO_NAME':
                                            metatask_list[task[0]].append(task[0])
                                        else:
                                            metatask_list[task[0]].append(metatask_name)
                                        metatask_list[task[0]].append(task[0])
                                    else:
                                        metatask_list[first_task_resolved_name].append(task[0])
                                    if list_tasks:
                                        print ' '+' '*i+task[0],task[1],'LOG:',task[2]

    # Default expantion of metatasks True = collapsed
    #for metatask,metatasks in metatask_list.iteritems():
    #   metatask_list[metatask].append(True)

    return tasks_ordered,metatask_list,cycledef_group_cycles

def get_rocoto_stat(params, queue_stat):
    workflow_file, database_file, tasks_ordered, metatask_list, cycledef_group_cycles = params

    global temp_workflow_file
    global database_file_agmented
    if len(tasks_ordered) == 0 or len(metatask_list) == 0 or len(cycledef_group_cycles) == 0 or list_tasks:
        tasks_ordered, metatask_list,cycledef_group_cycles  = get_tasklist(temp_workflow_file)

    if use_performance_metrics:
        aug_perf = get_aug_perf_values(get_user)
    else:
        aug_perf = None

    info=collections.defaultdict(list)
    cycles=set()

    connection=sqlite3.connect(database_file)
    c=connection.cursor()

    if use_performance_metrics:
        q=c.execute("DROP TABLE IF EXISTS jobs_augment_tmp;")
        sql_create_augment_table = "CREATE TABLE jobs_augment_tmp AS SELECT * FROM jobs;"
        q=c.execute(sql_create_augment_table)
        q=c.execute("alter table jobs_augment_tmp add column qtime integer;")
        q=c.execute("alter table jobs_augment_tmp add column cputime integer;")
        q=c.execute("alter table jobs_augment_tmp add column runtime integer;")
        q=c.execute("alter table jobs_augment_tmp add column slots integer;")

        sq_command = ''
        column_updates = ('qtime','cputime','runtime','slots')
        sqlite_merge_command = "%s=(SELECT jobs_augment.%s FROM jobs_augment WHERE jobs_augment.id=jobs_augment_tmp.id)"
        for column in column_updates:
            sq_command += sqlite_merge_command%(column,column)+','
        sq_command=';'.join(sq_command.rsplit(',', 1))
        sq_command = 'UPDATE jobs_augment_tmp SET '+sq_command
        q=c.execute(sq_command)

        sq_command = 'UPDATE jobs_augment_tmp SET '
        sqlite_update_command = "%s = '%s' WHERE jobs_augment_tmp.jobid = %s"
        #debug.write('WRITING TO DATABASE'+'\n')
        for perf_jobid,perf_values in aug_perf.iteritems():
            for name,each_value in perf_values.iteritems():
                 q=c.execute(sq_command+sqlite_update_command%(name,each_value,perf_jobid))
                #debug.write('SQL: '+sq_command+sqlite_update_command%(name,each_value,perf_jobid+'\n'))

        qinfo=c.execute("DROP TABLE IF EXISTS jobs_augment;")
        qinfo=c.execute("ALTER TABLE jobs_augment_tmp RENAME TO jobs_augment;")

    cycledifitions = []
    q=c.execute('SELECT id, groupname, cycledef FROM cycledef')
    for row in q:
        (theid, groupname, cycledef) = row
        cycledifitions.append( (theid, groupname, cycledef) )
        
    cycle_done_stat = dict()
    q=c.execute('SELECT id,cycle,done FROM cycles')
    for row in q:
        (theid,cycle,done)=row
        cycles.add(cycle)
        cycle_done_stat[cycle]=done

    if use_performance_metrics:
        q=c.execute('SELECT id,jobid,taskname,cycle,state,exit_status,duration,tries,qtime,cputime,runtime,slots FROM jobs_augment')
    else:
        q=c.execute('SELECT id,jobid,taskname,cycle,state,exit_status,duration,tries FROM jobs')
        
    q_get = []
    entered_jobids = []
    last_task_index = 0
    for row in q:
        row = tuple('-' if x is None else x for x in row)
        if use_performance_metrics:
            (theid, jobid,taskname,cycle,state,exit_status,duration,tries,qtime,cputime,runtime,slots)=row
        else:
            (theid, jobid,taskname,cycle,state,exit_status,duration,tries,)=row
        if jobid in entered_jobids:
            continue
        else:
            if taskname in tasks_ordered:
                task_index = [x[0] for x in task_ordered].index(taskname)
                #task_index = tasks_ordered.index(taskname)
                last_task_index = task_index
            else:
                task_index = last_task_index
            
            if use_performance_metrics:
                q_get.append( (theid,jobid,task_index,taskname,cycle,state,exit_status,duration,tries,qtime,cputime,runtime,slots) )
            else:
                q_get.append( (theid,jobid,task_index,taskname,cycle,state,exit_status,duration,tries) )
        entered_jobids.append(jobid)

    q_get.sort( key=lambda x: x[2] )

    connection.commit()
    c.close()

    for row in q_get:
        if use_performance_metrics:
            (theid,jobid,task_order,taskname,cycle,state,exit_status,duration,tries,qtime,cputime,runtime,slots)=row
        else:
            (theid,jobid,task_order,taskname,cycle,state,exit_status,duration,tries)=row
        if jobid != '-':
            if use_performance_metrics:
                line = '%s %s %s %s %s %s %s %s %s %s %s'%(datetime.datetime.fromtimestamp(cycle).strftime('%Y%m%d%H%M'),taskname,str(jobid),str(state),str(exit_status),str(tries),str(duration).split('.')[0],str(slots),str(qtime),str(cputime).split('.')[0],str(runtime))
            else:
                line = '%s %s %s %s %s %s %s'%(datetime.datetime.fromtimestamp(cycle).strftime('%Y%m%d%H%M'),taskname,str(jobid),str(state),str(exit_status),str(tries),str(duration).split('.')[0])
            #debug.write('LINE: '+line+'\n')
            info[cycle].append(line)

    for every_cycle in cycles:
        if len(info[every_cycle]) == 0:
            info[every_cycle].append('place holder')

    new_info=collections.defaultdict(list)
    job_ids = []
    job_id = ''
    for each_cycle,lines_in_cycle in info.iteritems():
        for task in tasks_ordered:
            skip_task = False
            for each_line in lines_in_cycle:
                if task[0] == each_line.split()[1]:
                #if task[0]+' ' in each_line:
                    job_id = each_line.split()[2]
                    if job_id in job_ids:
                        break
                    cycle_string = datetime.datetime.fromtimestamp(each_cycle).strftime('%Y%m%d%H%M')
                    #print 'TESTB:', len(task), task[0],task[1]
                    cycledefs = task[1].split(',')
                    if len(cycledefs) > 1:
                        #print 'Checking if %s for %s is in a gfs cycle:'%(task[0],cycle_string)
                        for each_cycledef in cycledefs:
                            #print 'group:', each_cycledef, cycledef_group_cycles[each_cycledef]
                            if cycle_string in cycledef_group_cycles[each_cycledef]:
                                #print 'Found:', task[0],'with cycle',cycle_string
                                new_info[each_cycle].append(each_line)
                                job_ids.append(job_id)
                                skip_task = True
                                break
                    elif cycle_string in cycledef_group_cycles[task[1]]:
                        new_info[each_cycle].append(each_line)
                        job_ids.append(job_id)
                        skip_task = True
                        break
            if skip_task:
                continue    
            line = datetime.datetime.fromtimestamp(each_cycle).strftime('%Y%m%d%H%M')+' '*7+task[0]+' - - - - -'
            cycle_string = datetime.datetime.fromtimestamp(each_cycle).strftime('%Y%m%d%H%M')
            cycledefs = task[1].split(',')
            if len(cycledefs) > 1:
                for each_cycledef in cycledefs:
                    if cycle_string in cycledef_group_cycles[each_cycledef]:
                        new_info[each_cycle].append(line)
                        skip_task = True
                        break
            elif cycle_string in cycledef_group_cycles[task[1]]:
                new_info[each_cycle].append(line)
                skip_task = True
            if skip_task:
                continue

    rocoto_stat = []
    for cycle in sorted(cycles):
        if len(new_info[cycle]) != 0:
            rocoto_stat.append(new_info[cycle])

    if save_checkfile_path is not None:
        stat_update_time = str(datetime.datetime.now()).rsplit(':',1)[0]
        with open(save_checkfile_path, 'w') as savefile:
            rocoto_data_and_time = (rocoto_stat, tasks_ordered, metatask_list,cycledef_group_cycles, stat_update_time)
            cPickle.dump(rocoto_data_and_time, savefile)
        if only_check_point:
            sys.exit(0)

    if use_multiprocessing:
        queue_stat.put((rocoto_stat, tasks_ordered, metatask_list, cycledef_group_cycles))
    else:
        return (rocoto_stat, tasks_ordered, metatask_list, cycledef_group_cycles)
            

def display_results(results,screen,params):
    from produtil.fileop import check_file
    results_lines = results.split('\n')
    num_lines,num_columns = (len(results_lines)+3,len(max(results_lines, key=len))+1)
    pad_pos = 0
    force_load_stat = False
    global mlines
    global mcols
    while True:
        screen.clear()
        screen.refresh()
        results_pad = curses.newpad(num_lines,num_columns)
        for results_line in results_lines:
            results_pad.addstr(results_line+'\n')
        results_pad.refresh( pad_pos, 0, 0,0, mlines-3,mcols-1)
        extra_1 = extra_2 = ''
        if pad_pos < num_lines-mlines-2 or pad_pos > 0:
            extra_1 = '<Page Up>/<Page Down> Scroll'
        if len(params) != 0:
            extra_2 = '<s>ave results to a file'
        screen.addstr(mlines-1,0,'<ENTER> Return %s %s'%(extra_1,extra_2),curses.A_BOLD)
        event = screen.getch()
        if event == curses.KEY_RESIZE:
            screen.refresh()
        elif event in ( curses.KEY_PPAGE, ord('u') ):
            if pad_pos < num_lines-mlines-2:
                pad_pos += 1
        elif event in ( curses.KEY_NPAGE, ord('d') ):
            if pad_pos != 0:
                pad_pos -= 1
        elif event == curses.KEY_ENTER or event == 10:
            screen.clear()
            break
        elif event == ord('s'):
            strg = []
            strg.append(PSLOT)
            for i in range(2,5):
                try:
                    if ' ' not in basename(params[i]):
                        strg.append(basename(params[i]).split('.')[0])
                except:
                    pass
                if len(strg) == 0:
                    strg = 'rocotoviewer_outout_file'
                save_results_file = '_'.join(strg)+'.txt'
            inc_int = 0
            while check_file(save_results_file):
                if '(%d)'%inc_int in save_results_file:
                    save_results_file = save_results_file.replace('(%d)'%inc_int,'(%d)'%(inc_int+1))
                    inc_int += 1
                else:
                    save_results_file = basename(save_results_file.split('.')[0])+'(%d)'%inc_int+'.txt'
            out_file = open(save_results_file,'w')
            out_file.write(results)
            out_file.close()
            screen.addstr(mlines-1,0,'Saved file %s'%save_results_file+' '*10)
            screen.refresh()
            std_time.sleep(0.5)

    return

def main(screen):

    global mlines
    global mcols
    global default_column_length
    global use_multiprocessing
    global highlightText
    global highlightSelectedText
    global normalText
    global PSLOT
    global PACKAGE
    global entity_values

    event = 10

    if not sys.stdin.isatty():
        if screen != 'dummy':
            print 'There seems to be a problem with the curses init'
            sys.exit(-1)
        else:
            mlines = 100
    else:
        mlines, mcols = screen.getmaxyx()

    #global debug
    #PWD = os.getcwd()
    #debug = open(PWD+'/debug.log','a',0)

    (workflow_file,database_file) = get_arguments()

    if not load_produtil_pythonpath():
        curses.endwin()
        print '\n\nCRITICAL ERROR: The produtil package could not be loaded from your system'
        sys.exit(-1)

    if html_output:
        if sys.stdin.isatty():
            curses.endwin()
        print '\nPreparing to write out an html folder'
        use_multiprocessing = False

    import produtil.run, produtil.numerics
    from produtil.run import run,runstr, batchexe
    from produtil.fileop import check_file, makedirs, deliver_file, remove_file, make_symlinks_in
    from produtil.prog import shbackslash

    header_string = '   CYCLE        TASK                JOBID     STATE          EXIT   TRIES   DURATION'
    header_string_under = '========(updated:tttttttttttttttt)========== PSLOT: pslot ==========================='

    global use_performance_metrics
    aug_perf = collections.defaultdict(dict)
    if use_performance_metrics:
        result = augment_SQLite3( database_file )
        aug_perf = get_aug_perf_values(get_user)
        header_string += '  SLOTS   QTIME    CPU    RUN\n'
        header_string_under += '=============================\n'
        header_string += header_string_under
        default_column_length = 122
    else:
        aug_perf = None
        header_string = header_string+'\n'+header_string_under+'\n'
        default_column_length = 91

    html_output_dir = None
    entity_values = get_entity_values( workflow_file )
    workflow_name = 'gfs_workflow'
    if 'ROTDIR' in entity_values:
        ROTDIR = entity_values['ROTDIR']
    else:
        ROTDIR = 'no_rotdir'
    if 'PSLOT' in entity_values:
        PSLOT = entity_values['PSLOT']
    else:
        PSLOT = 'no_name'
    if 'PACKAGE' in entity_values:
        PACKAGE = entity_values['PACKAGE']
        if  PACKAGE == 'ugcs':
            workflow_name = 'ugcs_workflow'
        if  PACKAGE == 'gfs':
            workflow_name = 'gfs_workflow'
    else:
        PACKAGE = 'none'
    if 'EXPDIR' in entity_values:
        EXPDIR = entity_values['EXPDIR']
    else:
        EXPDIR = '.'
    
    if html_output:
        html_ptr = None
        if not send_html_to_rzdm and len(rzdm_path) != 0:
            html_output_dir = shbackslash(rzdm_path)
        else:
            html_output_dir = shbackslash('%s/pr%s'%(workflow_name,PSLOT))
        print 'writing html to directory:',html_output_dir
        html_output_file = shbackslash( html_output_dir+'/index.html' )
        html_header_line = '<table>\n<thead><tr><td>CYCLE</td><td>TASK</td><td>JOBID</td><td>STATE</td><td>EXIT</td><td>TRIES</td><td>DURATION</td>'
        if use_performance_metrics:
            html_header_line = html_header_line+'<td>SLOTS</td><td>QTIME</td><td>CPU</td><td>RUN</td>'+'</tr></thead>\n<tbody>'
        else:
            html_header_line = html_header_line+'</tr></thead>\n<tbody>'
        print 'Generating html folder html: %s ...'%html_output_file
        cmd = batchexe('rm') ['-Rf', html_output_dir ]
        stat=runstr(cmd)
        makedirs( html_output_dir ) 
        html_ptr = open(html_output_file,'w')
        html_ptr.write(ccs_html)
        break_file = False
        stat_update_time = str(datetime.datetime.now()).rsplit(':',1)[0]
        html_discribe_line  = '\n<table>\n<thead>\n<tr><td><a href="index_exp.html">Expand</a></td><td>Refreshed: %s</td><td>PSLOT: %s</td></tr>\n'%(stat_update_time,PSLOT)
        html_discribe_line += '<tr><td colspan="2">ROTDIR: %s</td><td><a href="../%s_perf_%s.pdf">Turn Around Times</a></td></tr>\n</thead>\n</table>\n<br>\n'%(workflow_name,ROTDIR,PSLOT)
        html_discribe_line += html_header_line
        html_ptr.write( html_discribe_line )
    else:
        curses.start_color()
        curses.use_default_colors()
        screen.refresh()
        curses.mousemask(1)
        curses.noecho()
        for i in range(0, curses.COLORS):
            curses.init_pair(i + 1, i,curses.COLOR_BLACK)
            if i == 4:
                curses.init_pair(i + 1, i,curses.COLOR_WHITE)
        curses.init_pair(8, 0, -1)

        curses.mousemask(curses.ALL_MOUSE_EVENTS)
        #curses.init_pair(6,curses.COLOR_BLACK, curses.COLOR_CYAN)
        highlightText = curses.A_STANDOUT
        highlightSelectedText = curses.color_pair(5)
        normalText = curses.A_NORMAL

        cmd = batchexe('which') ['rocotorun']
        try:
            which_rocoto = runstr(cmd).strip()
        except Exception,e:
            curses.endwin()
            print '\n\nCRITICAL ERROR: rocotorun is not in your path, user "module load rocoto"'
            sys.exit(0)

    os.environ['TZ']='UTC'
    std_time.tzset()

    #stdout_buff = StringIO()
    #stderr_buff = StringIO()
    #sys.stdout = stdout_buff
    #sys.stderr = stderr_buff

    HOME = os.environ['HOME']
    rocoto_temp = HOME+'/.rocoto/tmp'
    makedirs( rocoto_temp )

    global temp_workflow_file
    workflow_basename = basename(workflow_file)+'.'
    temp_file= tempfile.NamedTemporaryFile(prefix=workflow_basename, dir=rocoto_temp, delete=False)
    temp_workflow_file = temp_file.name
    old =  open(workflow_file)
    temp = []
    for line in old:
        if '&ENV_VARS;' not in line:
            temp.append(line)

    for line in temp:
        temp_file.write(line)

    temp_file.close()
    old.close()

    tasks_ordered = []
    metatask_list = collections.defaultdict(list)
    cycledef_group_cycles = collections.defaultdict(list)

    queue_stat = Queue()
    queue_check = Queue()

    if only_check_point:
        curses.endwin()
        sys.stdout = os.fdopen(0,'w',0)
        print 'Creating check point file ...'
        params = (workflow_file, database_file, tasks_ordered, metatask_list, cycledef_group_cycles )
        get_rocoto_stat( params, queue_stat )

    stat_update_time = ''
    params_check = ''
    header = None

    process_get_rocoto_stat = None
    process_get_rocoto_check = None

    cycle = 0
    if html_output:
        mlines = 100 
        mcols = 125 
    if not html_output and mcols < default_column_length:
        curses.endwin()
        print
        print 'Your terminal is only %d characters must be at least %d to display workflow status'%(mcols,default_column_length)
        sys.exit(-1)
    if not html_output:
        screen.refresh()
    rocoto_stat_params = ''
    rocoto_stat_params_tmp = ''
    step = 0.0 ; i = 0
    dots = ('.    ','..   ','...  ','.... ','.....',' ....','  ...','    .')
    dot_stat = 0 ; dot_check = 0 
    current_time = time()
    meta_tasklist = collections.defaultdict(list)

    if save_checkfile_path is not None and check_file(save_checkfile_path):
        with open(save_checkfile_path) as savefile:
            rocoto_data_and_time = cPickle.load(savefile)
            rocoto_stat, tasks_ordered, metatask_list,cycledef_group_cycles, stat_update_time = rocoto_data_and_time
            start_time = time() - stat_read_time_delay - 10
            header = header_string
            header = header.replace('t'*16,stat_update_time)
            if PACKAGE.lower() == 'ugcs':
                header = header.replace(' PSLOT: pslot ','==== UGCS ====')
            elif PSLOT.lower() == 'no_name':
                header = header.replace(' PSLOT: pslot ','==============')
                reduce_header_size = 0
            else:
                header = header.replace(' PSLOT: pslot ','==== UGCS ====')
                reduce_header_size = 0
            if reduce_header_size > 0:
                header = header[:-reduce_header_size]
                header = header[reduce_header_size:]
    if list_tasks:
        params = (workflow_file, database_file, tasks_ordered, metatask_list, cycledef_group_cycles )
        get_rocoto_stat( params, Queue() )
        curses.endwin()
        sys.stdout = os.fdopen(0,'w',0)
        sys.exit(0)


    if save_checkfile_path is None or (save_checkfile_path is not None and not check_file(save_checkfile_path)):
        params = (workflow_file, database_file, tasks_ordered, metatask_list,cycledef_group_cycles)
        if use_multiprocessing:
            process_get_rocoto_stat = Process( target=get_rocoto_stat, args=[params, queue_stat] )
            process_get_rocoto_stat.start()
            screen.addstr(mlines-2,0,'No checkpoint file, must get rocoto stats please wait',curses.A_BOLD)
            screen.addstr(mlines-1,0,'Running rocotostat ',curses.A_BOLD)
        else:
            (rocoto_stat, tasks_ordered, metatask_list,cycledef_group_cycles) = get_rocoto_stat( params, Queue() )
            header = header_string
            stat_update_time = str(datetime.datetime.now()).rsplit(':',1)[0]
            header = header.replace('t'*16,stat_update_time)
            if PSLOT.lower() == 'no_name':
                header = header.replace(' PSLOT: pslot ','==============')
                reduce_header_size = 0
            elif PACKAGE.lower() == 'ugcs':
                header = header.replace(' PSLOT: pslot ','==== UGCS ====')
                reduce_header_size = 0
            else:
                header = header.replace('pslot',PSLOT)
                reduce_header_size = int((len(PSLOT)-len('PSLOT'))/2)
            if reduce_header_size > 0:
                header = header[:-reduce_header_size]
                header = header[reduce_header_size:]
            
        while use_multiprocessing:
            if  mcols < default_column_length:
                curses.endwin()
                print
                print 'Your terminal is only %d characters must be at least %d to display workflow status'%(mcols,default_column_length)
                sys.exit(-1)
            step += 0.001
            if step > 100:
                step = 0.0
                i = (0 if i == len(dots)-1 else i+1 )
                curses.curs_set(0)
                screen.addstr(mlines-1,19,dots[i],curses.A_BOLD)
                screen.refresh()
            try:
                rocoto_stat_params = queue_stat.get_nowait()
            except:
                pass
            if len(rocoto_stat_params) != 0:
                (rocoto_stat, tasks_ordered, metatask_list,cycledef_group_cycles) = rocoto_stat_params
                if use_multiprocessing:
                    process_get_rocoto_stat.join()
                    process_get_rocoto_stat.terminate()
                stat_update_time = str(datetime.datetime.now()).rsplit(':',1)[0]
                header = header_string
                header = header.replace('t'*16,stat_update_time)
                if PSLOT.lower() == 'no_name':
                    header = header.replace(' PSLOT: pslot ','==============')
                    reduce_header_size = 0
                elif PACKAGE.lower() == 'ugcs':
                    header = header.replace(' PSLOT: pslot ','==== UGCS ====')
                    reduce_header_size = 0
                else:
                    header = header.replace('pslot',PSLOT)
                    reduce_header_size = int((len(PSLOT)-len('PSLOT'))/2)
                if reduce_header_size > 0:
                    header = header[:-reduce_header_size]
                    header = header[reduce_header_size:]
                break

        start_time = time()

    num_cycle = len(rocoto_stat)
    time_to_load = (time()- current_time)/60.0

    pad_pos = 0
    update_pad = True
    task = 0 ; execute_task = '' ; execute_cycle = ''
    loading_stat = False
    loading_check = False
    find_next = 0
    check_task = '' ; check_cycle = ''
    rocoto_check = ''
    break_twice = False
    search_string = ''

    meta_tasks = []
    metatasks_state_cycle = []
    metatasks_state_string_cycle = []

    metatask_list_copy = collections.defaultdict(list)
    metatask_name =  collections.defaultdict(list)
    for each_metatask in metatask_list:
        metatask_name[each_metatask] = metatask_list[each_metatask][0]
        del metatask_list[each_metatask][0]

    tasks_in_cycle = []
    for each_cycle in rocoto_stat:
        list_of_tasks_per_cycle = []
        meta_tasks_in_cycle = []
        for each_line in each_cycle:
            line_has_metatask = False
            for check_metatask, check_metatask_list in metatask_list.iteritems():
                if check_metatask in each_line:
                    meta_tasks_in_cycle.append( (check_metatask, True, check_metatask_list ) )
                    line_has_metatask = True
                    continue
                else:
                    for every_meta_task in check_metatask_list:
                        each_element_in_line = each_line.split()
                        if every_meta_task != check_metatask:
                            for item in each_element_in_line:
                                if every_meta_task == item:
                                    meta_tasks_in_cycle.append((every_meta_task, False, check_metatask) )
                                    line_has_metatask = True
            if not line_has_metatask:
                if '---' not in each_line.split()[1]:
                    list_of_tasks_per_cycle.append(each_line.split()[1])
                meta_tasks_in_cycle.append(('False',False,'False'))

        tasks_in_cycle.append(list_of_tasks_per_cycle)

        meta_tasks_state = dict()
        meta_tasks_state_string = dict()
        for check_metatask, check_metatask_list in metatask_list.iteritems():
            meta_tasks_state[check_metatask] = True
            meta_tasks_state_string[check_metatask] = ''
        meta_tasks_state['False'] = False
                    
        meta_tasks.append(meta_tasks_in_cycle)
        metatasks_state_cycle.append(meta_tasks_state)
        metatasks_state_string_cycle.append(meta_tasks_state_string)

    update_metatask_state_status_message = True
    '''
# This lists each metatask and its elements
# for the first cycle for code edification
    curses.endwin()
    print
    for each_metatask in meta_tasks[0]:
        if each_metatask[1]:
            print metatask_name[each_metatask[2][0]]
            for task in each_metatask[2]:
                print '',task
    sys.exit(0)
    '''

    metatask_list_per_cycle = []
    metatask_list_by_name = collections.defaultdict(dict)
    for each_cycle in meta_tasks:
        list_of_metatasks_in_cycle = []
        for each_metatask in each_cycle:
            if each_metatask[1]:
                tasks_in_metatask_list = []
                for task in each_metatask[2]:
                    tasks_in_metatask_list.append( task )
                metatask_list_by_name[ metatask_name[each_metatask[2][0]] ] = tasks_in_metatask_list
                list_of_metatasks_in_cycle.append( metatask_name[each_metatask[2][0]] )
        metatask_list_per_cycle.append(list_of_metatasks_in_cycle)

    found = False
    end_found = False
    found_cycle = 0
    found_end_cycle = 0
    for find_cycle in range(0,len(rocoto_stat)):
        for lines in rocoto_stat[find_cycle]:
            if not found and any(x in lines for x in ['RUNNING', 'QUEUED']):
                found = True
                found_cycle = find_cycle
            if found and not any(x in lines for x in ['RUNNING', 'QUEUED']):
                end_found = True
                found_end_cycle = find_cycle
                break

    get_number_of_stats = 0
    if found:
        cycle = found_cycle
    else:
        get_number_of_stats = 2
        if len(rocoto_stat) > 2:
            cycle = len(rocoto_stat) - 2
        else: cycle = 0

    if html_output:
        if cycle > 2:
            cycle -= 2
        html_start_cycle = cycle

        html_output_firstpass = True
        #debug.write('num cycles: %s\n'%str(len(rocoto_stat)))
        while True:
            num_columns = default_column_length
            mlines = 90; mcols = 125
            if header is None:
                header = ' '
            if update_pad is True:
                #debug.write('cycle: %s\n'%str(cycle))
                num_lines = len(rocoto_stat[cycle])
                #debug.write('len rocoto_stat[cycle]: %s\n'%str(num_lines))
                line_correction = 0
                for count_meta_tasks in meta_tasks[cycle]:
                    if count_meta_tasks[1] and  metatasks_state_cycle[cycle][ count_meta_tasks[0] ]:
                        line_correction += len(count_meta_tasks[2]) - 1
                num_lines -= line_correction
                update_pad = False
                line_number = -1
                colapsed_metatask = False
                for line_num,line in enumerate(rocoto_stat[cycle]):
                    columns = line.split()
                    count_columns = line.split(' ')
                    spaces = []
                    for c,sub_group in groupby(count_columns):
                        if c != '': continue
                        spaces.append(' '*len(list(sub_group)))
                    spaces.append('')
                    text_color = {'SUCCEEDED':3,'QUEUED':4,'DEAD':2,'FAILED':2,'RUNNING':6}
                    skip_task = False

                    if not meta_tasks[cycle][line_num][1] and  metatasks_state_cycle[cycle][ meta_tasks[cycle][line_num][2] ]   :
                        skip_task = True
                    else:
                        line_number +=1
                    html_line = '<tr>'
                    if use_performance_metrics and len(columns) == 7:
                        for i in range(0,4):
                            columns.append('-')
                    for i,column in enumerate(columns):
                        if skip_task: continue
                        if not use_performance_metrics and i > 7: continue
                        execute_cycle = columns[0]
                        if i == 0:
                            if meta_tasks[cycle][line_num][1]:
                                if metatasks_state_cycle[cycle][columns[1]]:
                                    colapsed_metatask = True
                                    if update_metatask_state_status_message or len(metatasks_state_string_cycle[cycle][ columns[1] ])==0:
                                        get_state_list = []
                                        total_numer_of_tasks = len(meta_tasks[cycle][line_num][2])
                                        for check_metatask_line in rocoto_stat[cycle]:
                                            split_check_metatask_line = check_metatask_line.split()
                                            for each_metatask in meta_tasks[cycle][line_num][2]:
                                                    if each_metatask == split_check_metatask_line[1]:
                                                        get_state_list.append(split_check_metatask_line[3])
                                        metatask_state = columns[3]
                                        if 'SUCCEEDED' in get_state_list:
                                            metatask_state = '(%d/%d) SUCCEEDED'%(get_state_list.count('SUCCEEDED'),total_numer_of_tasks)
                                        if 'QUEUED' in get_state_list:
                                            metatask_state = '(%d/%d) QUEUED'%(get_state_list.count('QUEUED'),total_numer_of_tasks)
                                        if 'RUNNING' in get_state_list:
                                            metatask_state = '(%d/%d) RUNNING'%(get_state_list.count('RUNNING'),total_numer_of_tasks)
                                        if 'DEAD' in get_state_list:
                                            metatask_state = '(%d/%d) DEAD'%(get_state_list.count('DEAD'),total_numer_of_tasks)
                                        metatasks_state_string_cycle[cycle][ columns[1] ] = metatask_state  
                            html_line += '<td>'+column+'</td>'
                        elif i == 1:
                            save_column = column
                            if colapsed_metatask:
                                colapsed_metatask = False
                                column = metatask_name[column]
                            display_column = (column if len(column) < 40 else column[:40])
                            if line_number == task:
                                execute_task = save_column
                            if html_output:
                                log_file = ''
                                for find_task in tasks_ordered:
                                    if find_task[0] == column:
                                        log_file = find_task[2].replace('CYCLE', execute_cycle[:-2] )
                                if check_file(shbackslash( log_file )):
                                    deliver_file( log_file, html_output_dir )
                                    log_file_base = os.path.basename(log_file)
                                    html_line +=  '<td><a href="%s">'%log_file_base+display_column+'</a></td>'
                                else:       
                                    html_line +=  '<td>'+display_column+'</td>'
                        elif i == 2:
                            if len(column) > 7:
                                column = column[:7]
                            html_line +=  '<td>'+column+'</td>'
                        elif i == 3:
                            if  meta_tasks[cycle][line_num][1] and len(metatasks_state_string_cycle[cycle][ columns[1] ].split())!=1 and metatasks_state_cycle[cycle][columns[1]]:
                                column = metatasks_state_string_cycle[cycle][ columns[1] ]
                                if len(column)>15:
                                    if column.split()[1] == 'SUCCEEDED':
                                        html_line +=  '<td><green>'+column[:15]+'</green></td>'
                                    elif column.split()[1] == 'QUEUED':
                                        html_line +=  '<td><yellow>'+column[:15]+'</yellow></td>'
                                    elif column.split()[1] in('DEAD','FAILED'):
                                        html_line +=  '<td><red>'+column[:15]+'</red></td>'
                                    elif column.split()[1] == 'RUNNING':
                                        html_line +=  '<td><blue>'+column[:15]+'</blue></td>'
                                    else:
                                        html_line +=  '<td>'+column[:15]+'</td>'
                                else:
                                    if column.split()[1] == 'SUCCEEDED':
                                        html_line +=  '<td><green>'+column+'</green></td>'
                                    elif column.split()[1] == 'QUEUED':
                                        html_line +=  '<td><yellow>'+column+'</yellow></td>'
                                    elif column.split()[1] in('DEAD','FAILED'):
                                        html_line +=  '<td><red>'+column+'</red></td>'
                                    elif column.split()[1] == 'RUNNING':
                                        html_line +=  '<td><blue>'+column+'</blue></td>'
                                    else:
                                        html_line +=  '<td>'+column+'</td>'
                            elif column in text_color:
                                if column == 'SUCCEEDED':
                                    html_line +=  '<td><green>'+column+'</green></td>'
                                elif column == 'QUEUED':
                                    html_line +=  '<td><yellow>'+column+'</yellow></td>'
                                elif column in('DEAD','FAILED'):
                                    html_line +=  '<td><red>'+column+'</red></td>'
                                elif column == 'RUNNING':
                                    html_line +=  '<td><blue>'+column+'</blue></td>'
                                else:
                                    html_line +=  '<td>'+column+'</td>'
                            else:
                                html_line +=  '<td>'+column+'</td>'
                        else:
                            if len(column)<6:
                                html_line +=  '<td>'+column+'</td>'
                            else:
                                html_line +=  '<td>'+column+'</td>'
                    if not skip_task:
                        html_line += '</tr>\n'
                        html_ptr.write(html_line)

            update_metatask_state_status_message = False

            found_still_running = False
            cycle += 1
            update_pad = True
            for find_cycle in range(cycle,len(rocoto_stat)):
                for lines in rocoto_stat[find_cycle]:
                    if 'RUNNING' in lines:
                        found_still_running = True
                        break
                        break
            if  get_number_of_stats >= 0:
                found_still_running = True
            if cycle < len(rocoto_stat) or found_still_running:
                html_line = '</table>\n'
                html_line += '\n<br>\n\n'
                html_line += html_header_line
                html_ptr.write(html_line)
                get_number_of_stats -= 1
            else:   
                html_line = '</tbody>\n'
                html_line += '</table>\n'
                html_line += '</html>\n'
                html_ptr.write(html_line)
                html_ptr.close()
                if html_output_firstpass:
                    for meta_cycle in range(0,len(rocoto_stat)):
                        for execute_task in metatasks_state_cycle[meta_cycle]:
                            metatasks_state_cycle[meta_cycle][execute_task] = False
                    html_output_file = shbackslash( html_output_dir+'/index_exp.html' )
                    html_ptr = open(html_output_file,'w')
                    html_ptr.write(ccs_html)
                    stat_update_time = str(datetime.datetime.now()).rsplit(':',1)[0]
                    html_discribe_line  = '\n<table>\n<thead>\n<tr><td><a href="index.html">Collapse</a></td><td>Refreshed: %s</td><td>PSLOT: %s</td></tr>\n'%(stat_update_time,PSLOT)
                    html_discribe_line += '<tr><td colspan="2">ROTDIR: %s</td><td><a href="../%s_perf_%s.pdf">Turn Around Times</a></td></tr>\n</thead>\n</table>\n<br>\n'%(workflow_name,ROTDIR,PSLOT)
                    html_discribe_line += html_header_line
                    html_ptr.write( html_discribe_line )
                    html_output_firstpass = False
                    #cycle = html_start_cycle
            if not html_output_firstpass:
                if send_html_to_rzdm:
                    print 'sending html files to rzdm using rsync ...'
                    cmd=batchexe('rsync')['-avzr','--delete', html_output_dir, rzdm_path]
                    stat=runstr(cmd)
                    if stat is None:
                        print 'warning rsync to %s failed'%html_output_dir
                        sys.exit(-1)
                    else:
                        print 'done'
                sys.exit(0)
    else:

        # Main Curses Screen Loop 
        # Write to curses screen when HTML is not outputted
        highlight_CYCLE = False
        highlight_WORKFLOW = False
        get_execute_task_track = False
        screen.clear()
        global screen_resized
        selected_tasks = collections.defaultdict(list)
        selected_meta_tasks = collections.defaultdict(list)
        execute_metatask = None
        colapsed_metatask = None
        task = 0
        while True:
            if not check_file(workflow_file) or not check_file(database_file):
                curses.endwin()
                print;print
                print 'rocoto_viwer quit because the Rocoto database or XML file used by this session when missing'
                sys.exit(-1)
            job_id = None
            curses.noecho()
            num_columns = default_column_length
            if header is None:
                header = ' '
            if highlight_WORKFLOW:
                header_split = header.split('\n')
                screen.addstr(0,0,header_split[0]+'\n')
                screen.addstr(header_split[1],curses.A_STANDOUT)
            else:
                screen.addstr(0,0,header)
            if update_pad is True:
                num_lines = len(rocoto_stat[cycle])
                line_correction = 0
                for count_meta_tasks in meta_tasks[cycle]:
                    if count_meta_tasks[1] and  metatasks_state_cycle[cycle][ count_meta_tasks[0] ]:
                        line_correction += len(count_meta_tasks[2]) - 1
                num_lines -= line_correction
                update_pad = False
                if mlines > num_lines:
                    pad = curses.newpad(mlines ,num_columns)
                else:   
                    pad = curses.newpad(num_lines+1 ,num_columns)
                line_number = -1
                for line_num,line in enumerate(rocoto_stat[cycle]):
                    #debug.write('DISPLAY LINE: '+line+'\n')
                    colapsed_metatask = False
                    columns = line.split()
                    count_columns = line.split(' ')
                    spaces = []
                    for c,sub_group in groupby(count_columns):
                        if c != '': continue
                        spaces.append(' '*len(list(sub_group)))
                    spaces.append('')
                    text_color = {'SUCCEEDED':3,'QUEUED':4,'DEAD':2,'FAILED':2,'RUNNING':6}
                    skip_task = False

                    if not meta_tasks[cycle][line_num][1] and  metatasks_state_cycle[cycle][ meta_tasks[cycle][line_num][2] ]   :
                        skip_task = True
                    else:
                        line_number +=1
                    if use_performance_metrics and len(columns) == 7:
                        for i in range(0,4):
                            columns.append('-')
                    for i,column in enumerate(columns):
                        if skip_task: continue
                        if not use_performance_metrics and i > 7: continue
                        execute_cycle = columns[0]
                        if i == 0:
                            if meta_tasks[cycle][line_num][1]:
                                if metatasks_state_cycle[cycle][columns[1]]:
                                    if highlight_CYCLE:
                                        pad.addstr(column, curses.A_STANDOUT)
                                    else:
                                        pad.addstr(column)
                                    pad.addstr(' < ')
                                    colapsed_metatask = True
                                    if update_metatask_state_status_message or len(metatasks_state_string_cycle[cycle][ columns[1] ])==0:
                                        get_state_list = []
                                        total_numer_of_tasks = len(meta_tasks[cycle][line_num][2])
                                        for check_metatask_line in rocoto_stat[cycle]:
                                            split_check_metatask_line = check_metatask_line.split()
                                            for each_metatask in meta_tasks[cycle][line_num][2]:
                                                    if each_metatask == split_check_metatask_line[1]:
                                                        get_state_list.append(split_check_metatask_line[3])
                                        red_override = False
                                        metatask_state = columns[3]
                                        if 'SUCCEEDED' in get_state_list:
                                            metatask_state = '(%d/%d) SUCCEEDED'%(get_state_list.count('SUCCEEDED'),total_numer_of_tasks)
                                        if 'QUEUED' in get_state_list:
                                            metatask_state = '(%d/%d) QUEUED'%(get_state_list.count('QUEUED'),total_numer_of_tasks)
                                        if 'RUNNING' in get_state_list:
                                            metatask_state = '(%d/%d) RUNNING'%(get_state_list.count('RUNNING'),total_numer_of_tasks)
                                        if 'FAILED' in get_state_list:
                                            metatask_state = '(%d/%d) FAILED'%(get_state_list.count('FAILED'),total_numer_of_tasks)
                                            red_override = True
                                        if 'DEAD' in get_state_list:
                                            red_override = True
                                            metatask_state = '(%d/%d) DEAD'%(get_state_list.count('DEAD'),total_numer_of_tasks)
                                        metatasks_state_string_cycle[cycle][ columns[1] ] = metatask_state  
                                else:
                                    if highlight_CYCLE:
                                        pad.addstr(column, curses.A_STANDOUT)
                                    else:
                                        pad.addstr(column)
                                    pad.addstr(' > ')
                            else:
                                if highlight_CYCLE:
                                    pad.addstr(column,curses.A_STANDOUT)
                                    pad.addstr('   ')
                                else:
                                    pad.addstr(column+'   ')
                        elif i == 1:
                            save_column = column
                            if colapsed_metatask:
                                column = metatask_name[column]
                            display_column = (column if len(column) < 19 else column[:19])
                            if line_number == task and not highlight_CYCLE and not highlight_WORKFLOW :
                                pad.addstr(display_column,curses.A_STANDOUT)
                                execute_task_track = save_column
                                if colapsed_metatask:
                                    execute_metatask_check = True
                                    execute_metatask = column
                                    metatask_list_of_selected_metatask = meta_tasks[cycle][line_num][2]
                                else:
                                    execute_metatask_check = False
                                    execute_metatask = None
                                    metatask_list_of_selected_metatask = None
                                execute_task = column
                            else:
                                #if column in metatask_list_by_name[metatask_name[column]]:
                                #   display_column = ' '+display_column
                                if column in selected_tasks[execute_cycle]:
                                    pad.addstr(display_column, highlightSelectedText )
                                elif column in selected_meta_tasks[execute_cycle]:
                                    pad.addstr(display_column, highlightSelectedText )
                                else:
                                    pad.addstr(display_column)
                            pad.addstr(' '*(21-len(display_column)))
                        elif i == 2:
                            job_id = column.strip()
                            if len(job_id) > 9:
                                job_id = job_id[:9]
                            if job_id == '-':
                                pad.addstr(job_id+' '*9)
                            else:
                                pad.addstr(job_id+' '*(10-len(job_id)))
                        elif i == 3:
                            if  meta_tasks[cycle][line_num][1] and len(metatasks_state_string_cycle[cycle][ columns[1] ].split())!=1 and metatasks_state_cycle[cycle][columns[1]]:
                                column = metatasks_state_string_cycle[cycle][ columns[1] ]
                                if red_override:
                                    the_text_color = 2
                                else:
                                    the_text_color = text_color[column.split()[1]]

                                if len(column) >= 15:
                                    pad.addstr( column[:15],curses.color_pair(the_text_color)|curses.A_STANDOUT)
                                    column = column[:15]
                                else:
                                    pad.addstr( column,curses.color_pair(the_text_color)|curses.A_STANDOUT)
                            elif column in text_color:
                                pad.addstr(column, curses.color_pair(text_color[column])|curses.A_STANDOUT)
                            else:
                                pad.addstr(column)
                            pad.addstr(' '*(16-len(column)),curses.color_pair(8))
                        elif i in (4,5,6,7,8,9,10):
                            if len(column) < 8:
                                pad.addstr(column+' '*(8-len(column)))
                            else:
                                pad.addstr(column.strip()+'  ')

                    if not skip_task:
                        pad.addstr('\n')

            update_metatask_state_status_message = False
            pad.refresh( pad_pos, 0, 2,0,  mlines-4,mcols)

            entire_workflow = 'Hit <ENTER> to open cycle based information page (implementation pending)' 
            entire_cycle = '********* The ENTIRE CYCLE has been selected for an action **********'

            try:
                if highlight_WORKFLOW:
                    screen.addstr(mlines-2,0,entire_workflow,curses.A_BOLD)
                else:
                    screen.addstr(mlines-2,0,' '*len(entire_workflow))
                if highlight_CYCLE:
                    screen.addstr(mlines-2,0,entire_cycle,curses.A_BOLD)
                elif not highlight_WORKFLOW:
                    screen.addstr(mlines-2,0,' '*len(entire_cycle))
                if pad_pos < num_lines-mlines+4 or pad_pos > 0:
                    screen.addstr(mlines-1,0,' '*len(bottom_message_scroll))
                    screen.addstr(mlines-1,0,bottom_message_scroll,curses.A_BOLD)
                else:
                    screen.addstr(mlines-1,0,' '*len(bottom_message_scroll))
                    screen.addstr(mlines-1,0,bottom_message,curses.A_BOLD)
            except:
                std_time.sleep(1)
                pass

            if num_columns > mcols:
                curses.endwin()
                print   
                print 'Your terminal is only %s characters must be at least %s to display workflow status'%(str(mcols),str(num_columns))
                sys.exit(-1)

            if loading_stat:
                dot_stat = (0 if dot_stat == len(dots)-1 else dot_stat+1 )
                screen.addstr(mlines-2,0,'Running rocotostat ')
                screen.addstr(mlines-2,20,dots[dot_stat])
                try:
                    rocoto_stat_tmp = queue_stat.get_nowait()
                except:
                    rocoto_stat_tmp = ''
                if len(rocoto_stat_tmp) != 0:
                    (rocoto_stat, tasks_ordered, metatask_list,cycledef_group_cycles) = rocoto_stat_tmp
                    process_get_rocoto_stat.join()
                    process_get_rocoto_stat.terminate()
                    update_pad = True
                    loading_stat = False
                    rocoto_stat_tmp = ''
                    stat_update_time = str(datetime.datetime.now()).rsplit(':',1)[0]
                    header = header_string
                    header = header.replace('t'*16,stat_update_time)
                    header = header.replace('pslot',PSLOT)
                    reduce_header_size = int((len(PSLOT)-len('PSLOT'))/2)
                    if reduce_header_size > 0:
                        header = header[:-reduce_header_size]
                        header = header[reduce_header_size:]
                    screen.addstr(mlines-2,0,'Updated new rocotostatus: %s'%stat_update_time+' '*48)
                    screen.refresh()
                    std_time.sleep(0.5)
                    screen.addstr(mlines-2,0,' '*100)
                    screen.refresh()
        
            if loading_check:
                if  time() - current_check_time > 5:
                    dot_check = (0 if dot_check == len(dots)-1 else dot_check+1 )
                    loc = (0 if not loading_stat else 27)       
                    screen.addstr(mlines-2,loc,'Running rocotocheck ')
                    screen.addstr(mlines-2,loc+20,dots[dot_check])
                try:
                    rocoto_check = queue_check.get_nowait()
                except:
                    pass
                if len(rocoto_check) != 0:
                    process_get_rocoto_check.join()
                    process_get_rocoto_check.terminate()
                    loading_check = False
                    if  time() - current_check_time > 5:
                        event = screen.getch()
                        time_inc = 0.0
                        while event != curses.KEY_ENTER and event != 10:
                            message_string = 'rocotocheck for %s %s is ready for vieweing'%(params_check[2],params_check[3])
                            message_string = (message_string if len(message_string) < mcols  else message_string[:mcols-1])
                            time_inc += 1
                            if time_inc > 4:
                                screen.addstr(mlines-2,0, message_string)
                                screen.addstr(mlines-2,len(message_string),'             ')
                                time_inc = 0.0
                            else:
                                screen.addstr(mlines-2,0,message_string)
                                screen.addstr(mlines-2,len(message_string),'  <Hit Enter>',curses.A_BOLD)
                                event = screen.getch()
                    display_results(rocoto_check,screen,params_check)
                    rocoto_check = ''
                    
            curses.curs_set(0)
            curses.halfdelay(2)
            screen.keypad(1)
            event = screen.getch()

            if event in (curses.KEY_LEFT, curses.KEY_RIGHT):
                highlight_CYCLE = False
                highlight_WORKFLOW = False
                if event == curses.KEY_LEFT:
                    pad_pos = 0
                    #debug.write('KEY_LEFT %s\n'%pad_pos)
                    if cycle - 1 >= 0:
                        cycle -= 1
                elif event == curses.KEY_RIGHT:
                    pad_pos = 0
                    #debug.write('KEY_RIGHT %s\n'%pad_pos)
                    if cycle + 1 < num_cycle:
                        cycle += 1
                num_lines = len(rocoto_stat[cycle])
                line_correction = 0
                for count_meta_tasks in meta_tasks[cycle]:
                    if count_meta_tasks[1] and  metatasks_state_cycle[cycle][ count_meta_tasks[0] ]:
                        line_correction += len(count_meta_tasks[2])-1
                num_lines -= line_correction
                if task > num_lines-1:
                    task = num_lines-1
                update_pad = True
            if event == ord('Q'):
                break
            if get_execute_task_track:
                get_execute_task_track = False
                if execute_task_track in metatasks_state_cycle[cycle]:
                    metatasks_state_cycle[cycle][execute_task_track] = not metatasks_state_cycle[cycle][execute_task_track]
                    update_metatask_state_status_message = True
                    update_pad = True
            if event == curses.KEY_MOUSE:
                mouse_id, mouse_x, mouse_y, mouse_z, button_state = curses.getmouse()
                task_mouse_pos = pad_pos+mouse_y-2
                if task_mouse_pos >= 0 and task_mouse_pos < num_lines:
                    task = task_mouse_pos
                    update_pad = True
                if button_state & curses.BUTTON1_DOUBLE_CLICKED and mouse_x in range(12,15):
                    get_execute_task_track = True
            if event == ord('x'):
                if execute_task_track in metatasks_state_cycle[cycle]:
                    metatasks_state_cycle[cycle][execute_task_track] = not metatasks_state_cycle[cycle][execute_task_track]
                    update_metatask_state_status_message = True
                    update_pad = True
            if screen_resized:
                screen.erase()
                screen.refresh()
                update_pad = True
                task = pad_pos
                screen_resized = False
                curses.resizeterm(mlines, mcols)
                #debug.write('SCREEN RESIZED %s (%d,%d)\n'%(pad_pos,mlines,mcols))
                if  mcols < default_column_length:
                    curses.endwin()
                    print
                    print 'Your terminal is only %d characters must be at least %d to display workflow status'%(mcols,default_column_length)
                    sys.exit(-1)
            elif event in ( curses.KEY_NPAGE, ord('d') ):
                highlight_CYCLE = False
                highlight_WORKFLOW = False
                if pad_pos + mlines < num_lines-mlines+5:
                    pad_pos += mlines - 5
                    task += mlines - 5
                else:
                    pad_pos = num_lines-mlines+5
                    task = num_lines-1
                update_pad = True
            elif event in ( curses.KEY_PPAGE, ord('u') ):
                highlight_CYCLE = False
                highlight_WORKFLOW = False
                if pad_pos != 0:
                    if pad_pos - mlines > 0:
                        pad_pos -= mlines - 5
                        if task > pad_pos+mlines-6:
                            task -= mlines - 5
                    else:
                        pad_pos = 0
                        task = 0
                update_pad = True
            elif event in (curses.KEY_UP, curses.KEY_SR):
                if task == 0:
                    if highlight_CYCLE:
                        highlight_CYCLE = False
                        highlight_WORKFLOW = True
                    if not highlight_WORKFLOW: 
                        highlight_CYCLE = True
                if task != pad_pos:
                    update_pad = True
                    task -= 1
                elif pad_pos != 0:
                    pad_pos -= 1
                    task -= 1
                if event == curses.KEY_SR:
                    if execute_metatask_check:
                        if execute_metatask in selected_meta_tasks[execute_cycle]:
                            if len(selected_meta_tasks[execute_cycle]) > 0:
                                selected_meta_tasks[execute_cycle].remove(execute_metatask)
                        else:
                            selected_meta_tasks[execute_cycle].append(execute_metatask)
                    else:
                        if execute_task in selected_tasks[execute_cycle]:
                            if len(selected_tasks[execute_cycle]) > 0:
                                selected_tasks[execute_cycle].remove(execute_task)
                        else:
                            selected_tasks[execute_cycle].append(execute_task)
                update_pad = True
            elif event in ( curses.KEY_DOWN, curses.KEY_SF ):
                if highlight_CYCLE or highlight_WORKFLOW:
                    task = -1 
                highlight_CYCLE = False
                highlight_WORKFLOW = False
                if task != num_lines-1 and task < pad_pos+mlines-6:
                    task += 1
                elif pad_pos < num_lines-mlines+5:
                    pad_pos += 1
                    task += 1
                if event == curses.KEY_SF:
                    if execute_metatask_check:
                        if execute_metatask in selected_meta_tasks[execute_cycle]:
                            if len(selected_meta_tasks[execute_cycle]):
                                selected_meta_tasks[execute_cycle].remove(execute_metatask)
                        else:
                            selected_meta_tasks[execute_cycle].append(execute_metatask)
                    else:
                        if execute_task in selected_tasks[execute_cycle]:
                            if len(selected_tasks[execute_cycle]) > 0:
                                selected_tasks[execute_cycle].remove(execute_task)
                        else:
                            selected_tasks[execute_cycle].append(execute_task)
                update_pad = True
            elif event == ord('c'):
                if loading_check == True:
                    screen.addstr(mlines-2,0,'rocotocheck is all reading running                    ')
                    screen.refresh()
                    std_time.sleep(0.5)
                    screen.addstr(mlines-2,0,' '*100)
                    screen.refresh()
                else:
                    loc = (0 if not loading_stat else 27)       
                    screen.addstr(mlines-2,loc,'Running rocotocheck ')
                    screen.refresh()
                    params_check = (workflow_file, database_file, execute_task, execute_cycle, 'check')
                    process_get_rocoto_check = Process( target=get_rocoto_check, args=[params_check, queue_check] )
                    process_get_rocoto_check.start()
                    loading_check = True
                    current_check_time = time()
            elif event == ord('f'):
                log_file = ''
                for find_task in tasks_ordered:
                    if find_task[0] == execute_task:
                        log_file = find_task[2].replace('CYCLE', execute_cycle[:-2] )
                        if check_file(log_file):
                            links = []
                            links.append(log_file)
                            try:
                                make_symlinks_in(links,EXPDIR,force=True)
                            except:
                                pass
            elif event in (curses.KEY_ENTER, 10, 13):

                if execute_metatask_check:
                    selected_tasks[execute_cycle] = list_selector( screen, selected_tasks[execute_cycle], metatask_list_of_selected_metatask )
                    screen.erase()
                else:
                    if execute_task in selected_tasks[execute_cycle]:
                        if len(selected_tasks[execute_cycle]) > 0:
                            selected_tasks[execute_cycle].remove(execute_task)
                    else:
                        selected_tasks[execute_cycle].append(execute_task)
                
            elif event == ord('r'):
                screen.clear()
                process = ''
                if highlight_CYCLE:
                    screen.addstr('Are you sure you want to rewind all the tasks in the cycle %s by running:\n\n'%execute_cycle)
                    process = '-a'
                #highlight_WORKFLOW = False
                elif execute_metatask_check and len(selected_tasks[execute_cycle]) == 0:
                    for tasks in metatask_list_of_selected_metatask:
                        process += '-t ' + tasks+' '
                    screen.addstr('Are you sure you want to rewind all the tasks in the metatask (%s) by running:\n\n'%execute_task)
                elif len(selected_tasks[execute_cycle]) != 0 or len(selected_meta_tasks[execute_cycle]) != 0:
                    if  len(selected_tasks[execute_cycle]) != 0:
                        selected_tasks_string = ''
                        screen.addstr('Selected tasks:\n\n')
                        for tasks in selected_tasks[execute_cycle]:
                            selected_tasks_string += tasks+'\t'
                            process += '-t ' + tasks+' '
                        screen.addstr(selected_tasks_string+'\n\n')
                    if  len(selected_meta_tasks[execute_cycle]) != 0:
                        selected_tasks_string = ''
                        screen.addstr('Selected %d entire meta-tasks and their tasks:\n\n'%len( selected_meta_tasks[execute_cycle]))
                        for meta_task_selected in selected_meta_tasks[execute_cycle]:
                            for tasks in metatask_list_by_name[meta_task_selected]:
                                selected_tasks_string += tasks+'\t'
                                process += '-t ' + tasks+' '
                            screen.addstr(selected_tasks_string+'\n\n')
                    screen.addstr('\nAre you sure you want to rewind all these seleted tasks by running:\n\n')
                elif len(selected_tasks[execute_cycle]) == 0:
                    process = '-t '+ execute_task
                    screen.addstr('Are you sure you want to rewind the single task %s by running:\n\n'%execute_task)
                screen.addstr('rocotorewind -c %s -d %s -w %s %s\n\n'%(execute_cycle,basename(database_file),basename(workflow_file),process))
                screen.addstr('Enter: <Y>es or <N>o',curses.A_BOLD)
                while True:
                    event = screen.getch()
                    if event == ord('y') or event == ord('Y'):
                        params = (workflow_file, database_file, execute_cycle,process)
                        results = rocoto_rewind(params)
                        results_params = ('','','rewind',execute_cycle,'tasks')
                        try:
                            display_results(results,screen,results_params)
                        except:
                            screen.addstr('\n\nRewind of this job was successful but displaying of the stdout failed\n')
                            screen.addstr('Output has been written out to the file rocotorewind_output.log\n')
                            screen.addstr('Press <ENTER> to continue')
                            with open('rocotorewind_output.log','a') as rocotorewind_logfile:
                                rocotorewind_logfile.write('\n\n'+results)
                            while True:
                                event = screen.getch()
                                if event in (curses.KEY_ENTER, 10, 13):
                                    break
                        selected_tasks[execute_cycle] = [] 
                        break
                    elif event == ord('n') or event == ord('N'):
                        break
                screen.clear()
                update_pad = True
            elif event == ord('U'):
                selected_tasks[execute_cycle] = []
                selected_meta_tasks[execute_cycle] = []
                update_pad = True
            elif event == ord('b'):
                process = ''
                screen.clear()
                list_meta_tasks = ''
                list_of_tasks = ''
                boot_task_list = '' ; tasks_to_boot = []
                boot_metatask_list = '' ; metatasks_to_boot = []
                if highlight_CYCLE:
                    screen.addstr('You have selected to boot the entire cycle %s:\n\n'%execute_cycle,curses.A_BOLD)
                    metatasks_to_boot = metatask_list_per_cycle[cycle] 
                    tasks_to_boot = tasks_in_cycle[cycle]
                elif len(selected_tasks[execute_cycle]) != 0 or len(selected_meta_tasks[execute_cycle]) != 0:
                    screen.addstr('You have a list selected tasks and/or metatasks to boot:\n\n',curses.A_BOLD)
                    metatasks_to_boot = selected_tasks[execute_cycle]
                    tasks_to_boot = selected_tasks[execute_cycle]
                elif execute_metatask_check:
                    screen.addstr('Are you sure you want boot the entire meta task %s by running:\n\n'%execute_metatask)
                    metatasks_to_boot.append(execute_metatask)
                elif len(selected_tasks[execute_cycle]) == 0:
                    tasks_to_boot.append(execute_task)
                    screen.addstr('Are you sure you want boot the task %s by running rocotoboot with:'%execute_task)
                else:
                    update_pad = True
                    continue

                if len(metatasks_to_boot) > 0:
                    list_meta_tasks = '   '
                    screen.addstr('Metatasks selected in cycle:\n\n',curses.A_BOLD)
                    for meta_task in metatasks_to_boot:
                        list_meta_tasks += meta_task+' '
                        boot_metatask_list += meta_task+','
                    boot_metatask_list = boot_metatask_list[:-1]
                    screen.addstr( list_meta_tasks )
                if len(tasks_to_boot) > 0:
                    list_of_tasks = '   '
                    screen.addstr('\n\nTasks selected in cycle:\n\n',curses.A_BOLD)
                    for a_task in tasks_to_boot:
                        list_of_tasks += a_task+' '
                        boot_task_list += a_task+','
                    boot_task_list = boot_task_list[:-1]
                    screen.addstr( list_of_tasks )

                screen.addstr('\n\nAre you sure you want to boot all the tasks and/or metatasks in the cycle %s by running:\n\n'%execute_cycle,curses.A_BOLD)
                if len(boot_metatask_list) != 0:
                    list_meta_tasks = '--metatasks '+"'"+boot_metatask_list+"'"
                if len(boot_task_list) != 0:
                    list_of_tasks = ' --tasks '+"'"+boot_task_list+"'"
                screen.addstr('rocotoboot -d %s -w %s %s\n\n'%(basename(database_file),basename(workflow_file),list_meta_tasks+list_of_tasks))
                screen.addstr('Enter: <Y>es or <N>o',curses.A_BOLD)

                while True:
                    event = screen.getch()
                    if event == ord('y') or event == ord('Y'):
                        params = (workflow_file, database_file, execute_cycle, boot_metatask_list, boot_task_list)
                        results = rocoto_boot(params)
                        display_results(results,screen,('','',execute_cycle,'rocotoboot_output'))
                        break
                    elif event == ord('n') or event == ord('N'):
                        break
                screen.clear()
                update_pad = True
            elif event == ord('R'):
                screen.addstr(mlines-2,0,'Running rocotorun and rocotostat ...'+' '*60,curses.A_BOLD)
                params = (workflow_file, database_file)
                rocoto_run(params)
                update_pad = True
                screen.clear()
                if loading_stat == True:
                    screen.addstr(mlines-2,0,'rocotostat is all reading running'+' '*60)
                    screen.refresh()
                    std_time.sleep(0.5)
                else:
                    start_time = 0
            elif event == ord('/'):
                curses.echo()
                find_next = 1
                screen.addstr(mlines-3,0,' '*100)
                screen.refresh()
                screen.addstr(mlines-3,0,'/')
                screen.refresh()
                search_string = screen.getstr(mlines-3,1,50)
                break_twice = False
                screen.addstr(mlines-3,0,' '*100)
                screen.refresh()
                for every_cycle in range(0,len(rocoto_stat)):
                    for line_number,line in enumerate(rocoto_stat[every_cycle]):
                        if search_string in line:
                            task = line_number
                            if num_lines < mlines:
                                pad_pos = 0
                            else:
                                pad_pos = task
                            update_pad = True
                            cycle = every_cycle
                            break_twice = True
                            break
                    if break_twice:
                        screen.clear()
                        break
                    else:
                        find_next = 1
            elif (event == ord('n') or event == ord('N')) and len(search_string) != 0:
                if event == ord('n'):
                    find_next += 1
                else:
                    if find_next - 1 >= 1:
                        find_next -= 1
                found_next = 0
                break_twice = False
                for every_cycle in range(0,len(rocoto_stat)):
                    for line_number,line in enumerate(rocoto_stat[every_cycle]):
                        if search_string in line:
                            found_next += 1
                            if find_next == found_next:
                                task = line_number
                                if num_lines < mlines:
                                    pad_pos = 0
                                else:
                                    pad_pos = task
                                update_pad = True
                                cycle = every_cycle
                                break_twice = True
                                break
                        if break_twice:
                            screen.clear()
                            break
                if not break_twice:
                    find_next = 1

            elif event == ord('F'):
                for find_cycle in range(0,len(rocoto_stat)):
                    for lines in rocoto_stat[find_cycle]:
                        if 'RUNNING' in line:
                            break
                            break
                if find_cycle > 1:
                    cycle = find_cycle - 2
                    update_pad = True
            elif event == ord('l'):
                start_time -= stat_read_time_delay
            elif event == ord('h'):
                update_pad = True
                help_screen(screen)
                screen.clear()
            current_time = time()
            diff = current_time - start_time
            if diff > stat_read_time_delay and not loading_stat:
                start_time = current_time
                if not use_multiprocessing:
                    params = (workflow_file, database_file, tasks_ordered, metatask_list,cycledef_group_cycles)
                    (rocoto_stat, tasks_ordered, metatask_list,cycledef_group_cycles) = get_rocoto_stat( params, Queue() )
                    stat_update_time = str(datetime.datetime.now()).rsplit(':',1)[0]
                    header = header_string
                    header = header.replace('t'*16,stat_update_time)
                    header = header.replace('pslot',PSLOT)
                    reduce_header_size = int((len(PSLOT)-len('PSLOT'))/2)
                    if reduce_header_size > 0:
                        header = header[:-reduce_header_size]
                        header = header[reduce_header_size:]
                    update_pad = True
                    screen.clear()
                else:
                    loading_stat = True
                    screen.addstr(mlines-2,0,'Running rocotostat                                        ')
                    params = (workflow_file, database_file, tasks_ordered, metatask_list,cycledef_group_cycles)
                    process_get_rocoto_stat = Process( target=get_rocoto_stat, args=[params, queue_stat] )
                    process_get_rocoto_stat.start()

        if use_multiprocessing:
            if  process_get_rocoto_stat is not None:
                if process_get_rocoto_stat.is_alive():
                    process_get_rocoto_stat.terminate()
            if process_get_rocoto_check is not None:
                if process_get_rocoto_check.is_alive():
                    process_get_rocoto_check.terminate()

        #debug.close()

if __name__ == '__main__':
    if not load_produtil_pythonpath():
        print '\n\nCRITICAL ERROR: The produtil package could not be loaded from your system'
        sys.exit(-1)
    from produtil.fileop import remove_file
    try:
        signal.signal(signal.SIGWINCH, sigwinch_handler)
        sys.stdout = sys.__stdout__
        sys.stderr = sys.__stderr__
        if sys.stdin.isatty():
            curses.wrapper(main)
        else:
            screen = 'dummy'
            main(screen)
        remove_file(temp_workflow_file)
    except KeyboardInterrupt:
        print "Got KeyboardInterrupt exception. Exiting..."
        sys.exit(-1)
