#!/usr/bin/env python3

import sys
import os
from wxflow import SQLiteDB
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter, REMAINDER, ZERO_OR_MORE


def full_path(string):
    if os.path.isfile(string) or os.path.isdir(os.path.dirname(string)):
        return os.path.abspath(string)
    else:
        raise NotADirectoryError(string)


def create(db):
    """
    Create a new database.

    Parameters
    ----------
    db : SQLiteDB
        The database to create.
    """
    db.create_table('pr_list', ['pr INTEGER PRIMARY KEY UNIQUE', 'state TEXT', 'status TEXT', 'reset_id INTEGER', 'cases TEXT'])


def add_pr(db, pr):
    """
    Add a pull request to the database.

    Parameters
    ----------
    ci_database : SQLiteDB
        The database to add the pull request to.
    pr : str
        The pull request to add.
    """
    rows = db.fetch_data('pr_list')
    entities = (args.add_pr[0], 'Open', 'Ready', 0, 'ci_repo')
    try:
        db.insert_data('pr_list', entities)
    except (SQLiteDB.IntegrityError) as e:
        if str(e) == "PRIMARY KEY must be unique":
            print(f"pr {entities[0]} already is in list: nothing added")


def update_pr(db, args):
    """
    Update a pull request in the database.

    Parameters
    ----------
    ci_database : SQLiteDB
        The database to update the pull request in.
    args : argparse.Namespace
        The command line arguments.
    """
    if len(args.update_pr) < 2:
        print(f"update_pr must have at least one vaule to update")
        sys.exit(0)

    update_list = ['state', 'status', 'reset_id', 'cases']
    for value in args.update_pr[1:]:
        update = update_list.pop(0)
        db.update_data('pr_list', update, value, 'pr', args.update_pr[0])


def remove_pr(db, args):
    """
    Remove a pull request from the database.

    Parameters
    ----------
    ci_database : SQLiteDB
        The database to remove the pull request from.
    pr : str
        The pull request to remove.
    """
    db.remove_column('pr_list', args.remove_pr[0])


def display(db, args):
    """
    Display the database.

    Parameters
    ----------
    ci_database : SQLiteDB
        The database to display.
    args : argparse.Namespace
        The command line arguments.

    Returns
    -------
    list
        The rows of the database.
    """
    values = []
    if len(args.display) == 1:
        rows = db.fetch_data('pr_list', ['pr', 'state', 'status', 'reset_id', 'cases'], f'pr = {args.display[0]}')
    else:
        rows = db.fetch_data('pr_list', ['pr', 'state', 'status', 'reset_id', 'cases'])
    for row in rows:
        values.append(' '.join(map(str, row)))

    return values


def input_args():
    """
    Parse command line arguments.

    Returns
    -------
    argparse.Namespace
        The parsed command line arguments.
    """

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
    ci_database.connect()

    if args.create:
        create(ci_database)
    if args.add_pr:
        add_pr(ci_database, args.add_pr[0])
    if args.update_pr:
        update_pr(ci_database, args)
    if args.remove_pr:
        remove_pr(ci_database, args.remove_pr[0])
    if args.display is not None:
        for rows in display(ci_database, args):
            print(rows)

    ci_database.disconnect()
