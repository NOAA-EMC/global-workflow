"""
Factory for creating Rocoto XML generators.

This module provides a factory that creates appropriate Rocoto XML generators
for different types of workflows (GFS, GEFS, SFS, GCAFS).
"""
from wxflow import Factory
from rocoto.gfs_cycled_xml import GFSCycledRocotoXML
from rocoto.gfs_forecast_only_xml import GFSForecastOnlyRocotoXML
from rocoto.gefs_xml import GEFSRocotoXML
from rocoto.sfs_xml import SFSRocotoXML
from rocoto.gcafs_xml import GCAFSForecastOnlyRocotoXML
from rocoto.gcafs_cycled_xml import GCAFSCycledRocotoXML

# Create a factory for Rocoto XML generator objects
rocoto_xml_factory = Factory('RocotoXML')

# Register XML generators for different workflow types
rocoto_xml_factory.register('gfs_cycled', GFSCycledRocotoXML)
rocoto_xml_factory.register('gfs_forecast-only', GFSForecastOnlyRocotoXML)
rocoto_xml_factory.register('gefs_forecast-only', GEFSRocotoXML)
rocoto_xml_factory.register('sfs_forecast-only', SFSRocotoXML)
rocoto_xml_factory.register('gcafs_cycled', GCAFSCycledRocotoXML)
rocoto_xml_factory.register('gcafs_forecast-only', GCAFSForecastOnlyRocotoXML)
