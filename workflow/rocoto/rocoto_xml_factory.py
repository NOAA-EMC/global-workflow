from pygw.factory import Factory
from rocoto.gfs_cycled_xml import GFSCycledRocotoXML
from rocoto.gfs_forecast_only_xml import GFSForecastOnlyRocotoXML
from rocoto.gefs_xml import GEFSRocotoXML


class RocotoXMLFactory(Factory):

    def __init__(self, name: str):
        super().__init__(name)


rocoto_xml_factory = RocotoXMLFactory('RocotoXML')
rocoto_xml_factory.register('gfs_cycled', GFSCycledRocotoXML)
rocoto_xml_factory.register('gfs_forecast-only', GFSForecastOnlyRocotoXML)
rocoto_xml_factory.register('gefs_forecast-only', GEFSRocotoXML)
