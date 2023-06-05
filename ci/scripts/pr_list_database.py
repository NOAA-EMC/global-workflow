#!/usr/bin/env python3

import sys,os
from pathlib import Path
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter, REMAINDER, ZERO_OR_MORE
import sqlite3

def dir_path(string):
    if os.path.isfile(string):
        return os.path.abspath(string)
    else:
        raise NotADirectoryError(string)


def sql_connection(filename: os.path) -> sqlite3.Connection:
    """
    Returns an Sqlite3 Cursor object from a given path to a sqlite3 database file

    Parameters
    ----------
    filename : Path
        Full path to a sqlite3 database file

    Returns
    -------
    sqlite3.Connection
        Sqlite3 Connection object for updating table

    """
    try:
        return sqlite3.connect(filename)
    except sqlite3.Error:
        print(sqlite3.Error)
        sys.exit(-1)


def sql_table(obj: sqlite3.Cursor) -> None:
    """
    Creates the initial sqlite3 table for PR states and status

    Parameters
    ----------
    obj : sqlite3.Cursor
         Cursor object for Sqlite3

    """

    obj.execute("CREATE TABLE processing(pr integer PRIMARY KEY, state text, status text, reset_id integer, cases text)")


def sql_insert(obj: sqlite3.Cursor, entities: list) -> None:
    """
    Inserts a new row in sqlite3 table with PR, state, and status

    Parameters
    ----------
    obj : sqlite3.Cursor
        Cursor object for Sqlite3
    entities : list
        A list of four string values that go into sqlite table (pr, state, status, reset_id, cases)
            pr: pull request number
            state: The new value for the state (Open, Closed)
            status: The new value for the status (Ready, Running, Failed)
            reset_id: The value for number of times reset_id to Ready
            cases: String containing case selection information

    """

    obj.execute('INSERT INTO processing(pr, state, status, reset_id, cases) VALUES(?, ?, ?, ?, ?)', entities)


def sql_update(obj: sqlite3.Cursor, pr: str, updates: dict) -> None:
    """Updates table for a given pr with new values for state and status

    Parameters
    ----------
    obj : sqlite.sql_connection
        sqlite3 Cursor Object
    pr : str
        The given pr number to update in the table
    updates : dict
        Dictionary of values to update for a given PR to include by postion
        state, The new value for the state (Open, Closed)
        status, The new value for the status (Ready, Running, Failed)
        reset_id, The value for number of times reset_id to Ready
        cases, Information regarding which cases are used (i.e. self PR)

    """

    update_list = ['state', 'status', 'reset_id', 'cases']
    rows = sql_fetch(obj)
    for value in updates:
        update = update_list.pop(0)
        obj.execute(f'UPDATE processing SET "{update}" = "{value}" WHERE pr = {pr}')


def sql_fetch(obj: sqlite3.Cursor) -> list:
    """ Gets list of all rows in table

    Parameters
    ----------
    obj : sqlite.sql_connection
        sqlite3 Cursor Object

    """

    obj.execute('SELECT * FROM processing')
    return obj.fetchall()


def sql_remove(obj: sqlite3.Cursor, pr: str) -> None:
    """ Removes the row from table with given pr number

    Parameters
    ----------
    obj : sqlite.sql_connection
        sqlite3 Connection Object
    pr : str
        pr number acting as key for removing the row with in it

    """

    obj.execute(f'DELETE FROM processing WHERE pr = {pr}').rowcount


def input_args():

    description = """Arguments for creating and updating db file for pr states
    """

    parser = ArgumentParser(description=description,
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('--sbfile', help='SQLite3 database file with PR list',type=dir_path)
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

    con = sql_connection(args.sbfile)
    obj = con.cursor()

    if args.create:
        sql_table(obj)

    if args.add_pr:
        rows = sql_fetch(obj)
        for row in rows:
            if str(row[0]) == str(args.add_pr[0]):
                print(f"pr {row[0]} already is in list: nothing added")
                sys.exit(0)

        entities = (args.add_pr[0], 'Open', 'Ready', 0, 'ci_repo')
        sql_insert(obj, entities)

    if args.update_pr:
        if len(args.update_pr) < 2:
            print(f"update_pr must have at least one vaule to update")
            sys.exit(0)
        pr = args.update_pr[0]

        sql_update(obj, pr, args.update_pr[1:])

    if args.remove_pr:
        sql_remove(obj, args.remove_pr[0])

    if args.display is not None:
        rows = sql_fetch(obj)
        if len(args.display) == 1:
            for row in rows:
                if int(args.display[0]) == int(row[0]):
                    print(' '.join(map(str, row)))
        else:
            for row in rows:
                print(' '.join(map(str, row)))

    con.commit()
    con.close()
