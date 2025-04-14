from wxflow import Factory
from rocoto.gfs_cycled_xml import GFSCycledRocotoXML
from rocoto.gfs_forecast_only_xml import GFSForecastOnlyRocotoXML
from rocoto.gefs_xml import GEFSRocotoXML
from rocoto.sfs_xml import SFSRocotoXML

rocoto_xml_factory = Factory('RocotoXML')
rocoto_xml_factory.register('gfs_cycled', GFSCycledRocotoXML)
rocoto_xml_factory.register('gfs_forecast-only', GFSForecastOnlyRocotoXML)
rocoto_xml_factory.register('gefs_forecast-only', GEFSRocotoXML)
rocoto_xml_factory.register('sfs_forecast-only', SFSRocotoXML)
