from wxflow import Factory
from rocoto.gfs_tasks import GFSTasks
from rocoto.gefs_tasks import GEFSTasks
from rocoto.sfs_tasks import SFSTasks


tasks_factory = Factory('Tasks')
tasks_factory.register('gfs', GFSTasks)
tasks_factory.register('gefs', GEFSTasks)
tasks_factory.register('sfs', SFSTasks)
