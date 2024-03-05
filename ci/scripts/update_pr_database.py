#!/usr/bin/env python3

import os,sys

from github import Github
from wxflow import Executable, which, SQLiteDB
from workflow.hosts import Host

from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter, REMAINDER, ZERO_OR_MORE

def full_path(string):
    if os.path.isfile(string) or os.path.isdir(os.path.dirname(string)):
        return os.path.abspath(string)
    else:
        raise NotADirectoryError(string)
    
def create(db):
    db.connect()
    db.create_table('pr_list', ['pr INTEGER PRIMARY KEY', 'state TEXT', 'status TEXT', 'reset_id INTEGER', 'cases TEXT'])
    db.disconnect()   

def add_pr(db, pr):
    db.connect()
    rows = db.fetch_data('pr_list')
    for row in rows:
        if str(row[0]) == str(args.add_pr[0]):
            print(f"pr {row[0]} already is in list: nothing added")
            sys.exit(0)

    entities = (args.add_pr[0], 'Open', 'Ready', 0, 'ci_repo')
    db.insert_data('pr_list', entities)
    db.disconnect()

def update_pr(db, pr):        
    if len(args.update_pr) < 2:
        print(f"update_pr must have at least one vaule to update")
        sys.exit(0)
    pr = args.update_pr[0]

    db.connect()
    db.update_data('pr_list', pr, args.update_pr[1:])
    db.disconnect()

def remove_pr(db, pr):
    db.connect()
    db.remove_column('pr_list', args.remove_pr[0])
    db.disconnect()

def display(db):
    db.connect()
    rows = db.fetch_data('pr_list')
    if len(args.display) == 1:
        for row in rows:
            if int(args.display[0]) == int(row[0]):
                print(' '.join(map(str, row)))
        else:
            for row in rows:
                print(' '.join(map(str, row)))
    db.disconnect()


def input_args():

    description = """Arguments for creating and updating db file for pr states
    """

    parser = ArgumentParser(description=description,
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('--dbfile', help='SQLite3 database file with PR list', type=full_path)
    parser.add_argument('--create', help='create sqlite file for pr list status', action='store_true', required=False)
    parser.add_argument('--add_pr', nargs=1, metavar='PR', help='add new pr to list (defults to: Open,Ready)', required=False)
    parser.add_argument('--remove_pr', nargs=1, metavar='PR', help='removes pr from list', required=False)
    parser.add_argument('--update_pr', nargs=REMAINDER, metavar=('pr', 'state', 'status', 'reset_id', 'cases'),
                        help='updates state and status of a given pr', required=False)
    parser.add_argument('--display', nargs='*', help='output pr table', required=False)

    args = parser.parse_args()
    return args

if __name__ == '__main__':

    args = input_args()

    if not args.create:
        if not os.path.isfile(args.dbfile):
            print(f'Error: {args.dbfile} does not exsist')
            sys.exit(-1)

    ci_database = SQLiteDB(args.dbfile)

    if args.create:
	create(ci_database)
    if args.add_pr:
        add_pr(ci_database, args.add_pr[0])
    if args.update_pr:
        update_pr(ci_database, args.update_pr[0])
    if remove_pr:
	remove_pr(ci_database, args.remove_pr[0])
    if display:
        display(ci_database)

    sys.exit(0)

    gh_cli = which('gh')
    gh_cli.add_default_arg(['auth', 'status','--show-token'])
    gh_access_token=gh_cli(output=str, error=str).split('\n')[3].split(': ')[1]
    gh = Github(gh_access_token)

    #repo = g.get_repo("TerrenceMcGuinness-NOAA/global-workflow")
    repo = gh.get_repo("NOAA-EMC/global-workflow")
    pulls = repo.get_pulls(state='open')

    host = Host()
    pr_list = []
    for pull in pulls:
        labels = pull.get_labels()
        ci_labels = [s for s in labels if 'CI' in s.name]
        for label in ci_labels:
            if host.machine.capitalize() in label.name:
                pr_list.append(pull)
                print(f"With label: {label.name} the PR {pull.number} is added to list")


    print(f"PR list: {pr_list}")
