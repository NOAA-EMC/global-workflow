from pygw.factory import Factory
from rocoto.gfs_tasks import GFSTasks
from rocoto.gefs_tasks import GEFSTasks


class TasksFactory(Factory):

	def __init__(self, name: str):
		super().__init__(name)


tasks_factory = TasksFactory('Tasks')
tasks_factory.register('gfs', GFSTasks)
tasks_factory.register('gefs', GEFSTasks)
