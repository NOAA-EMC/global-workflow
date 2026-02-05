from logging import getLogger
import sqlite3
from datetime import datetime, timedelta
from wxflow.sqlitedb import SQLiteDB
from wxflow import FileHandler
from os.path import basename, join

logger = getLogger(__name__.split('.')[-1])


class BaseDatabase(SQLiteDB):
    """Base class for managing different types of file-based databases."""

    def __init__(self, db_name: str, base_dir: str) -> None:
        """
        Initialize the database.

        :param db_name: Name of the SQLite database.
        :param base_dir: Directory containing observation files.
        """
        super().__init__(db_name)
        self.base_dir = base_dir
        self.create_database()

    def create_database(self):
        """Create the SQLite database. Must be implemented by subclasses."""
        raise NotImplementedError("Subclasses must implement create_database method")

    def get_connection(self):
        """Return the database connection."""
        return self.connection

    def parse_filename(self):
        """Parse a filename and extract relevant metadata. Must be implemented by subclasses."""
        raise NotImplementedError("Subclasses must implement parse_filename method")

    def ingest_files(self):
        """Scan the directory for new observation files and insert them into the database."""
        raise NotImplementedError("Subclasses must implement ingest_files method")

    def insert_record(self, query: str, params: tuple) -> None:
        """Insert a record into the database."""
        self.connect()
        cursor = self.connection.cursor()
        try:
            cursor.execute(query, params)
            self.connection.commit()
        except sqlite3.IntegrityError:
            pass  # Skip duplicates
        finally:
            self.disconnect()

    def insert_records(self, query: str, params_list: list[tuple]) -> None:
        """
        Insert multiple records into the database.

        :param query: SQL query for inserting records.
        :param params_list: List of tuples containing the parameters for each record.
        """
        self.connect()
        cursor = self.connection.cursor()
        try:
            cursor.executemany(query, params_list)
            self.connection.commit()
        except sqlite3.IntegrityError:
            pass  # Skip duplicates
        finally:
            self.disconnect()

    def execute_query(self, query: str, params: tuple = None) -> list:
        """Execute a query and return the results."""
        self.connect()
        cursor = self.connection.cursor()
        cursor.execute(query, params or [])
        results = cursor.fetchall()
        self.disconnect()
        return results

    def get_valid_files(self,
                        window_begin: datetime,
                        window_end: datetime,
                        dst_dir: str,
                        instrument: str = None,
                        satellite: str = None,
                        obs_type: str = None,
                        check_receipt: str = "none") -> list:
        """
        Retrieve and copy to dst_dir a list of observation files within a specified time window, possibly filtered by instrument,
        satellite, and observation type. The check_receipt parameter can be 'gdas', 'gfs', or 'none'. If 'gdas' or
        'gfs' is specified, files are further filtered based on their receipt time to ensure they meet the
        required delay criteria.

        :param window_begin: Start of the time window (datetime object).
        :param window_end: End of the time window (datetime object).
        :param dst_dir: Destination directory where valid files will be copied.
        :param instrument: (Optional) Filter by instrument name.
        :param satellite: (Optional) Filter by satellite name.
        :param obs_type: (Optional) Filter by observation type.
        :param check_receipt: (Optional) Specify receipt time check ('gdas', 'gfs', or 'none').
        :return: List of valid observation file paths in the destination directory.
        """

        query = """
        SELECT filename FROM obs_files
        WHERE obs_time BETWEEN ? AND ?
        """
        minutes_behind_realtime = {'gdas': 160, 'gfs': 20}
        params = [window_begin, window_end]

        if instrument:
            query += " AND instrument = ?"
            params.append(instrument)
        if satellite:
            query += " AND satellite = ?"
            params.append(satellite)
        if obs_type:
            query += " AND obs_type = ?"
            params.append(obs_type)

        results = self.execute_query(query, tuple(params))
        valid_files = []
        for row in results:
            filename = row[0]
            if check_receipt in ["gdas", "gfs"]:
                query = "SELECT receipt_time FROM obs_files WHERE filename = ?"
                receipt_time = self.execute_query(query, (filename,))[0][0]
                receipt_time = datetime.strptime(receipt_time, "%Y-%m-%d %H:%M:%S.%f")
                if receipt_time <= window_end - timedelta(minutes=minutes_behind_realtime[check_receipt]):
                    continue

            valid_files.append(filename)

        # Copy files to the destination directory
        dst_files = []
        if len(valid_files) > 0:
            src_dst_obs_list = []  # list of [src_file, dst_file]
            for src_file in valid_files:
                dst_file = join(dst_dir, f"{basename(src_file)}")
                dst_files.append(dst_file)
                src_dst_obs_list.append([src_file, dst_file])
            FileHandler({'mkdir': [dst_dir]}).sync()
            FileHandler({'copy': src_dst_obs_list}).sync()

        return dst_files
