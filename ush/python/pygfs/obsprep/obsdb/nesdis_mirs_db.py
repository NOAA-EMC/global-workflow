import os
import glob
from logging import getLogger
from datetime import datetime
from pyobsforge.obsdb import BaseDatabase

logger = getLogger(__name__.split('.')[-1])


class NesdisMirsDatabase(BaseDatabase):
    """Class to manage an observation file database for data assimilation."""

    def __init__(self, obs_dirs, db_name="nesdis_mirs.db", dcom_dir="/lfs/h1/ops/prod/dcom/"):
        base_dirs = [os.path.join(dcom_dir, '*', obs_dir) for obs_dir in obs_dirs]
        super().__init__(db_name=db_name, base_dir=base_dirs)

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
        - `obs_type`: The type of observation (e.g., SEAICE)

        The table is created if it does not already exist.
        """
        query = """
        CREATE TABLE IF NOT EXISTS obs_files (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            filename TEXT UNIQUE,
            obs_time TIMESTAMP,
            receipt_time TIMESTAMP,
            instrument TEXT,
            satellite TEXT,
            obs_type TEXT
        )
        """
        self.execute_query(query)

    def parse_filename(self, filename):
        """Extract metadata from filenames matching the MIRS-TYPE-SEAICE pattern
        NPR-MIRS-IMG_v11r9_ma1_s202504300706550_e202504300756360_c202504300838450.nc
        NPR-MIRS-IMG_v11r9_n20_s202504300858350_e202504300859066_c202504300933000.nc
        NPR-MIRS-IMG_v11r9_n21_s202504300858324_e202504300859040_c202504300935130.nc
        NPR-MIRS-IMG_v11r9_npp_s202504300858336_e202504300859053_c202504300916400.nc
        NPR-MIRS-IMG_v11r9_gpm_s202504300848270_e202504300853250_c202504300912100.nc
        """
        try:
            fname = os.path.basename(filename)
            parts = fname.split("_")

            if len(parts) < 6 or not parts[3].startswith("s") or not parts[5].startswith("c"):
                logger.debug(f"Unexpected filename format: {fname}")
                return None

            instrument = parts[0].split("-")[1]
            satellite = parts[2]
            obs_type = {
                "ma1": "icec_amsu_ma1_l2",
                "n20": "icec_atms_n20_l2",
                "n21": "icec_atms_n21_l2",
                "npp": "icec_atms_npp_l2",
                "gpm": "icec_gmi_gpm_l2"
            }.get(satellite.lower(), None)

            if obs_type is None:
                logger.debug(f"Unrecognized satellite: {satellite}")
                return None

            obs_time = datetime.strptime(parts[3][1:15], "%Y%m%d%H%M%S")
            receipt_time = datetime.fromtimestamp(os.path.getctime(filename))
            return filename, obs_time, receipt_time, instrument, satellite, obs_type

        except Exception as e:
            logger.error(f"Failed to parse {filename}: {e}")
            return None

    def ingest_files(self):
        """Scan the directory for new NESDIS MIRS observation files and insert them into the database."""
        obs_files = []
        for base in self.base_dir:
            matched = glob.glob(os.path.join(base, "*.nc"))
            obs_files.extend(matched)

        logger.info(f"Found {len(obs_files)} new files to ingest")

        records_to_insert = []
        for file in obs_files:
            parsed_data = self.parse_filename(file)
            if parsed_data:
                records_to_insert.append(parsed_data)
            else:
                logger.warning(f"Skipped (unparseable): {os.path.basename(file)}")

        if records_to_insert:
            query = """
                INSERT INTO obs_files (filename, obs_time, receipt_time, instrument, satellite, obs_type)
                VALUES (?, ?, ?, ?, ?, ?)
            """
            try:
                self.insert_records(query, records_to_insert)
                logger.info(f"Successfully ingested {len(records_to_insert)} files into the database")
            except Exception as e:
                logger.error(f"Failed to insert records: {e}")
