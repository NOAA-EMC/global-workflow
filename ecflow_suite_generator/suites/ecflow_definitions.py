#!/usr/bin/env python3

import sys
import json

try:
    from ecflow import (Defs, Suite, Family, Variable, Task)
    import ecflow
except ImportError as err:
    raiseException("Error: Could not import ecflow module: %s" % err)

class ecFlowDefinition:

    def __init__(self,ecflowconfig,envconfig,arguments):
        self.verifyConstruction(ecflowconfig,arguments.suite,arguments.model)
        self.baseconfig = ecflowconfig
        self.suite = arguments.suite
        self.model = arguments.model
        self.DEFS = Defs()
        self.prodSuite = self.add_suite(ecflowconfig,arguments.suite)
        self.includePostprocess = arguments.postprocess

    def verifyConstruction(self,baseconfig,suite,model):
        try:
            if suite is None:
                raise Exception("suite is not defined")
            if model is None:
                raise Exception("Model is not defined")
        except Exception as e:
            self.raiseException(e)

        try:
            if suite not in baseconfig['suites']:
                raise Exception("Suite %s does not exist in the configuration."
                    % suite)
        except Exception as e:
            self.raiseException(e)

    def raiseException(self,e):
        print(e)
        sys.exit(1)

    def display(self):
        print(self.DEFS)

    def full_suite(self):
        self.process_definition_header()
        if self.model == 'all':
            for model in self.baseconfig['models']:
                self.prodSuite += self.process_node(self.baseconfig['models'][model],model)
        else:
            self.prodSuite += self.process_node(self.baseconfig['models'][self.model],self.model)
        self.DEFS.add(self.prodSuite)


    def forecast_only(self):
        self.process_definition_header()
        forecast_definition = self.baseconfig['models'][self.model]['forecast']
        self.prodSuite += self.process_node(forecast_definition)
        self.DEFS.add(self.prodSuite)

    def process_definition_header(self):
        if 'externs' in self.baseconfig['suites'][self.suite].keys():
            for extern in self.baseconfig['suites'][self.suite]['externs']:
                self.DEFS.add_extern(extern)

    def process_node(self,node,name=None):
        ecFlowNodes = {
            'task': self.add_task,
            'family': self.add_family,
            'task_loop': self.add_task_loop,
            'post': self.add_postprocessing,

        }
        return ecFlowNodes.get(node['type'], self.invalid_node)(node,name)

    def invalid_node():
        raise Exception("Node is not definied")

    def add_suite(self, configuration, suite):
        new_suite = Suite("prod%s" % suite)

        # Baseline edits
        edits = ecflow.Edit(configuration['suites']['edits'])
        new_suite += edits

        # Setup sutite specific edits
        if 'edits' in configuration['suites'][suite].keys():
            edits = ecflow.Edit(configuration['suites'][suite]['edits'])
            new_suite += edits

        return new_suite

    def add_task(self, node, name):
        if 'script' in node.keys():
            task = Task(node['script'])
        else:
            task = Task(name)

        # Check for Triggers
        if 'triggers' in node.keys():
            triggerString = ''
            for trigger in node['triggers']:
                if node['triggers'].index(trigger) == len(node['triggers'])-1:
                    triggerString += "%s" % trigger
                else:
                    triggerString += "%s and" % trigger
            task += ecflow.Trigger(triggerString)

        # Check for Events
        eventid = 1
        if 'events' in node.keys():
            for event in node['events']:
                task += ecflow.Event(eventid,"%s" % event)
                eventid += 1

        for key in node.keys():
            if (
                type(node[key]) is dict and
                'type' in node[key].keys() and
                node[key]['type'] == 'event_loop'
                ):
                    initial_count = node[key]['initial_count'] if 'initial_count' in node[key].keys() else 000
                    final_count = int(node[key]['total_events'])+1
                    script = node[key]['script']
                    for event_counter in range(initial_count,final_count,1):
                        task += ecflow.Event(eventid,"%s%03d" % (script,event_counter))
                        eventid += 1

        # Check for Edits
        if 'edits' in node.keys():
            task += ecflow.Edit(node['edits'])

        # Check for time parameters
        if 'time' in node.keys():
            task += ecflow.Time(node['time'])

        return task


    def add_family(self, node, name):
        family = Family(name)

        # Check for Edits and add them in.
        if 'edits' in node.keys():
            family += ecflow.Edit(node['edits'])

        # Since Families have sub families or other add on possibilities, this
        # section uses the "type" in the YAML file to identify it is an add on
        # for the family.
        addOns = {
            'family': self.add_family,
            'post': self.add_postprocessing,
            'task': self.add_task,
        }
        for key in node.keys():
            if (
                type(node[key]) is dict and
                'type' in node[key].keys() and
                node[key]['type'] in addOns.keys()
            ):
                family += addOns.get(node[key]['type'], self.invalid_node)(node[key],key)

        for key in node.keys():
            if (
                type(node[key]) is dict and
                'type' in node[key].keys() and
                node[key]['type'] == 'task_loop'

            ):
                task_array = self.add_task_loop(node[key],key)
                family += task_array

        # Check for a Forecast
        if 'forecast' in node.keys():
            forecast_definition = node['forecast']
            family += self.process_node(forecast_definition)

        return family

    def add_task_loop(self, node, name):
        script = node['script']
        final_count = int(node['total_events'])+1
        task_array = []
        task_initial_count = node['initial_count'] if 'initial_count' in node.keys() else 000

        def process_task_loop_edit(edit_name,edit_props,task_counter):
            initial_count = edit_props['initial_count'] if 'initial_count' in edit_props.keys() else 000
            if 'increment' in edit_props.keys():
                adjusted_counter = task_counter * edit_props['increment'] + initial_count
            else:
                adjusted_counter = task_counter + initial_count

            if 'count_prefix' in edit_props.keys():
                counter = "%s%03d" % (edit_props['count_prefix'], adjusted_counter)
            else:
                counter = "%03d" % adjusted_counter
            if 'name' in edit_props.keys():
                edit_dict = {edit_props['name']:counter}
            else:
                edit_dict = {edit_name:counter}
            return ecflow.Edit(edit_dict)

        def process_task_loop_trigger(trigger_name,trigger_props,task_counter):
            initial_count = trigger_props['initial_count'] if 'initial_count' in trigger_props.keys() else 000
            if 'increment' in trigger_props.keys():
                adjusted_counter = task_counter * trigger_props['increment'] + initial_count
            else:
                adjusted_counter = task_counter + initial_count

            triggerString = ''

            for trigger in trigger_props['triggers']:
                adjusted_trigger = trigger.replace("&counter","%03d" % adjusted_counter)
                if trigger_props['triggers'].index(trigger) == len(trigger_props['triggers'])-1:
                    triggerString += "%s" % adjusted_trigger
                else:
                    triggerString += "%s and" % adjusted_trigger

            return ecflow.Trigger(triggerString)

        for task_counter in range(0,final_count,1):
            if 'increment' in node.keys():
                adjusted_counter = task_counter * node['increment'] + task_initial_count
                task = Task("%s%03d" % (script,adjusted_counter))
            else:
                adjusted_counter = task_counter + task_initial_count
                task = Task("%s%03d" % (script,adjusted_counter))

            if 'looping_parameters' in node.keys():
                loops = node['looping_parameters']
                for param in node['looping_parameters']:
                    if loops[param]['type'] == 'edit':
                        task += process_task_loop_edit(param,loops[param],task_counter)
                    if loops[param]['type'] == 'trigger':
                        task += process_task_loop_trigger(param,loops[param],task_counter)

            task_array.append(task)

        return task_array

    def add_postprocessing(self, node, name):
        if self.includePostprocess:
            return self.add_family(node,'post_processing')
        else:
            return None
