import os
import glob
from logging import getLogger
from datetime import datetime, timedelta
from pygfs.obsprep.obsdb import BaseDatabase

logger = getLogger(__name__.split('.')[-1])


class RADSDatabase(BaseDatabase):
    """Class to manage an observation file database for data assimilation."""

    def __init__(self, db_name="rads.db",
                 dcom_dir=None,
                 obs_dir="wgrdbul/adt"):
        if dcom_dir is None:
            dcom_dir = os.environ.get("DCOMROOT")
            if dcom_dir is None:
                raise KeyError("DCOMROOT environment variable is not set")
        base_dir = os.path.join(dcom_dir, '*', obs_dir)
        super().__init__(db_name, base_dir)

    def create_database(self):
        """
        Create the SQLite database and observation files table.

        This method initializes the database with a table named `obs_files` to store metadata
        about observation files. The table contains the following columns:

        - `id`: A unique identifier for each record (auto-incremented primary key).
        - `filename`: The full path to the observation file (must be unique).
        - `obs_time`: The timestamp of the observation, extracted from the filename.
        - `receipt_time`: The timestamp when the file was added to the `dcom` directory.
        - `satellite`: The satellite from which the observation was collected (e.g., 3a, 3b, j3, ...).

        The table is created if it does not already exist.
        """
        query = """
        CREATE TABLE IF NOT EXISTS obs_files (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            filename TEXT UNIQUE,
            obs_time TIMESTAMP,
            receipt_time TIMESTAMP,
            satellite TEXT
        )
        """
        self.execute_query(query)

    def parse_filename(self, filename):
        """Extract metadata from filenames matching the expected pattern."""
        parts = os.path.basename(filename).replace('.', '_').split('_')
        if len(parts) == 5 and parts[0] == 'rads' and parts[1] == 'adt' and parts[3].isdigit():
            obs_time = datetime.strptime(parts[3], "%Y%j") + timedelta(hours=12)
            satellite = parts[2]
            receipt_time = datetime.fromtimestamp(os.path.getctime(filename))
            return filename, obs_time, receipt_time, satellite
        return None

    def ingest_files(self):
        """Scan the directory for new RADS observation files and insert them into the database."""
        obs_files = glob.glob(os.path.join(self.base_dir, "*.nc"))
        logger.info(f"Found {len(obs_files)} new files to ingest")

        records_to_insert = []
        for file in obs_files:
            parsed_data = self.parse_filename(file)
            if parsed_data:
                records_to_insert.append(parsed_data)

        if records_to_insert:
            query = """
                INSERT INTO obs_files (filename, obs_time, receipt_time, satellite)
                    VALUES (?, ?, ?, ?)
            """
            try:
                self.insert_records(query, records_to_insert)
                logger.info(f"Successfully ingested {len(records_to_insert)} files into the database")
            except Exception as e:
                logger.error(f"Failed to insert records: {e}")
