import os
import glob
from logging import getLogger
from datetime import datetime
from pygfs.obsprep.obsdb import BaseDatabase

logger = getLogger(__name__.split('.')[-1])


class SmapDatabase(BaseDatabase):
    """Class to manage an observation file database for data assimilation."""

    def __init__(self, db_name="smap.db",
                 dcom_dir="/lfs/h1/ops/prod/dcom/",
                 obs_dir="wtxtbul/satSSS/SMAP"):
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
        - `satellite`: The satellite from which the observation was collected (e.g., GW1).

        The table is created if it does not already exist.
        """
        query = """
        CREATE TABLE IF NOT EXISTS obs_files (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            filename TEXT UNIQUE,
            obs_time TIMESTAMP,
            receipt_time TIMESTAMP,
            satellite TEXT,
            obs_type TEXT
        )
        """
        self.execute_query(query)

    def parse_filename(self, filename):
        # Pattern: SMAP_L2B_SSS_NRT_54047_A_20250315T011742.h5
        basename = os.path.basename(filename)
        parts = basename.split('_')

        # Pre-check: Must match SMAP_L2B_SSS_NRT structure
        if not basename.startswith("SMAP_L2B_SSS_NRT") or len(parts) < 7:
            logger.debug(f"Skipping non-SMAP_L2B_SSS_NRT file: {filename}")
            return None

        try:
            satellite = "SMAP"
            obs_type = "sss_smap_l2"
            timestamp_with_ext = parts[6]
            timestamp_str = os.path.splitext(timestamp_with_ext)[0]
            obs_time = datetime.strptime(timestamp_str, "%Y%m%dT%H%M%S")
            receipt_time = datetime.fromtimestamp(os.path.getctime(filename))
            return filename, obs_time, receipt_time, satellite, obs_type

        except Exception as e:
            logger.debug(f"Error parsing filename {filename}: {e}")
            return None

    def ingest_files(self):
        """Scan the directory for new observation files and insert them into the database."""
        obs_files = glob.glob(os.path.join(self.base_dir, "*.h5"))
        logger.info(f"Found {len(obs_files)} new files to ingest")

        records_to_insert = []
        for file in obs_files:
            parsed_data = self.parse_filename(file)
            if parsed_data:
                records_to_insert.append(parsed_data)
            else:
                logger.debug(f"Skipped (unparseable): {os.path.basename(file)}")

        if records_to_insert:
            query = """
                INSERT INTO obs_files (filename, obs_time, receipt_time, satellite, obs_type)
                VALUES (?, ?, ?, ?, ?)
            """
            try:
                self.insert_records(query, records_to_insert)
                logger.info(f"Successfully ingested {len(records_to_insert)} files into the database")
            except Exception as e:
                logger.error(f"Failed to insert records: {e}")
