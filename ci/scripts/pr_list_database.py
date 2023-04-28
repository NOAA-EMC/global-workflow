#!/usr/bin/env python3

import sys
from pathlib import Path
import sqlite3
from sqlite3 import Error
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

def sql_connection(filename):
    try:
        con = sqlite3.connect(Path(filename))
        return con
    except Error:
        print(Error)
        sys.exit(-1)

def sql_table(obj):
    obj.execute( "CREATE TABLE processing(pr integer PRIMARY KEY, state text, status text)")


def sql_insert(obj, entities):
    obj.execute('INSERT INTO processing(pr, state, status ) VALUES(?, ?, ?)', entities)   

def sql_update(obj,pr,state,status):
    obj.execute(f'UPDATE processing SET state = "{state}", status = "{status}" WHERE pr = {pr}')


def sql_fetch(obj):
    obj.execute('SELECT * FROM processing')
    rows = obj.fetchall()
    return rows

def sql_remove(obj,pr):
    obj.execute(f'DELETE FROM processing WHERE pr = {pr}').rowcount



def input_args():

    description = """Arguemtns for creating and updating db file for pr states
    """

    parser = ArgumentParser(description=description,
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('sbfile', help='file of sqlite3 for pr list database ', type=str)
    parser.add_argument('--create', help='create sqlite file for pr list status', type=str, required=False)
    parser.add_argument('--add_pr', help='add new pr to list (defults to: Open,Ready)', type=int, required=False)
    parser.add_argument('--remove_pr', help='removes pr from list', type=int, required=False)
    parser.add_argument('--update_pr', nargs=3, metavar=('pr','state','status'), help='updates state and status of a given pr', required=False)
    parser.add_argument('--display', help='output pr table', action='store_true', required=False)


    args = parser.parse_args()
    return args


if __name__ == '__main__':

    args = input_args()
    con = sql_connection(args.sbfile)
    obj = con.cursor()

    if args.create:
        sql_table(obj)

    if args.add_pr:
        entities = (args.add_pr, 'Open', 'Ready')
        sql_insert(obj, entities)

    if args.update_pr:
        pr=args.update_pr[0]
        state=args.update_pr[1]
        status=args.update_pr[2]
        sql_update(obj,pr,state,status)

    if args.remove_pr:
        sql_remove(obj,args.remove_pr)

    if args.display:
        rows=sql_fetch(obj)
        for row in rows:
            print(' '.join(map(str,row)))

    con.commit()
    con.close()
