#!/usr/bin/env python3

import sys
import os
from wxflow import SQLiteDB, SQLiteDBError
from githubpr import GitHubPR
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter, REMAINDER


def full_path(string):
    """
    full_path Get the absolute path of a file or directory.
    Parameters
    ----------
    string : str
        The relative path of the file or directory.
    Returns
    -------
    str
        The absolute path of the file or directory.
    Raises
    ------
    NotADirectoryError
        If the provided string does not represent a valid file or directory.
    """

    if os.path.isfile(string) or os.path.isdir(os.path.dirname(string)):
        return os.path.abspath(string)
    else:
        raise NotADirectoryError(string)


def create_table(db: SQLiteDB):
    """
    Create a new table in a database.

    Parameters
    ----------
    db : SQLiteDB
        The database to create.
    """
    db.create_table('pr_list', ['pr INTEGER PRIMARY KEY UNIQUE', 'state TEXT', 'status TEXT', 'reset_id INTEGER', 'cases TEXT'])


def add_pr(db: SQLiteDB, pr: str) -> bool:
    """
    Add a pull request to the database.

    Parameters
    ----------
    ci_database : SQLiteDB
        The database to add the pull request to.
    pr : str
        The pull request to add.
    """
    entities = (pr, 'Open', 'Ready', 0, 'ci_repo')
    try:
        db.insert_data('pr_list', entities)
        return True
    except (SQLiteDBError.IntegrityError) as e:
        if 'unique' in str(e).lower():
            print(f"pr {pr} already is in list: nothing added")
            return False


def update_pr(db: SQLiteDB, args):
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


def display_db(db, display) -> list:
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
    if len(display) == 1:
        rows = db.fetch_data('pr_list', ['pr', 'state', 'status', 'reset_id', 'cases'], f"pr = '{display[0]}'")
    if len(display) == 2:
        rows = db.fetch_data('pr_list', ['pr'], f"state = '{display[0]}' AND status = '{display[1]}'")
    if len(display) == 0:
        rows = db.fetch_data('pr_list', ['pr', 'state', 'status', 'reset_id', 'cases'])
    for row in rows:
        values.append(' '.join(map(str, row)))

    return values


def update_database(db: SQLiteDB) -> list:
    """
    Update the database from the GitHub PRs
    - only PRs from host machine are added to the database
    - if the PR is already in the database it its added to the kill list

    Parameters
    ----------
    ci_database : SQLiteDB
        The database to update.

    Returns
    -------
    list
        The kill list of pull requests.
    """
    gh = GitHubPR()
    pr_ready_list, pr_kill_list = gh.get_open_pr_list()
    for pr in pr_ready_list:
        if not add_pr(db, str(pr)):
            if pr not in pr_kill_list:
                pr_kill_list.append(pr)
    pr_kill_list = list(set(pr_kill_list))
    return pr_kill_list


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
    parser.add_argument('--list', nargs=2, metavar=('state', 'status'), required=False)
    parser.add_argument('--update_database', help='use labels from Open GitHub PRs to update database state and produces a kill list',
                         action='store_true', required=False)
    args = parser.parse_args()
    return args


if __name__ == '__main__':

    args = input_args()

    if not args.create_table:
        if not os.path.isfile(args.dbfile):
            print(f'Error: {args.dbfile} does not exsist')
            sys.exit(-1)

    ci_database = SQLiteDB(args.dbfile)
    ci_database.connect()

    if args.create:
        create_table(ci_database)
    if args.add_pr:
        add_pr(ci_database, args.add_pr[0])
    if args.update_pr:
        update_pr(ci_database, args)
    if args.remove_pr:
        ci_database.remove_data('pr_list', 'PR', args.remove_pr[0])
    if args.display is not None:
        for rows in display_db(ci_database, args.display):
            print(rows)
    if args.list:
        for rows in display_db(ci_database, [args.list[0], args.list[1]]):
            print(rows, end=' ')
        print()
    if args.update_database:
        pr_kill_list = update_database(ci_database)
        for pr in pr_kill_list:
            print(pr, end=' ')
        print()

    ci_database.disconnect()
