import os
import glob
from logging import getLogger
from datetime import datetime
from pygfs.obsprep.obsdb import BaseDatabase

logger = getLogger(__name__.split('.')[-1])


class NesdisJpssrrDatabase(BaseDatabase):
    """Class to manage an observation file database for data assimilation."""

    def __init__(self, db_name="nesdis_jpssrr.db",
                 dcom_dir="/lfs/h1/ops/prod/dcom/",
                 obs_dir="wgrdbul/IST"):

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
            satellite TEXT
        )
        """
        self.execute_query(query)

    def parse_filename(self, filename):
        """Extract metadata from filenames matching the JPSSRR-TYPE-SEAICE pattern
        JRR-IceConcentration_v3r3_j01_s202506010136113_e202506010137358_c202506010226221.nc
        JRR-IceConcentration_v3r3_n21_s202506010136118_e202506010137347_c202506010235083.nc
        JRR-IceConcentration_v3r3_npp_s202506010136106_e202506010137348_c202506010258132.nc
        """
        basename = os.path.basename(filename)
        parts = basename.split('_')
        try:
            if len(parts) >= 4 and parts[0] == "JRR-IceConcentration":
                obs_time = datetime.strptime(parts[3][1:15], "%Y%m%d%H%M%S")
                receipt_time = datetime.fromtimestamp(os.path.getctime(filename))
                satellite = parts[2]
                return filename, obs_time, receipt_time, satellite
        except ValueError:
            return None

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
