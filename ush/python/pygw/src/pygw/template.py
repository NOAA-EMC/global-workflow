import re
import os
import copy
from collections import namedtuple
from collections.abc import Sequence

# Template imported with permission from jcsda/solo

__all__ = ['Template', 'TemplateConstants']


class TemplateConstants:
    DOLLAR_CURLY_BRACE = '${}'
    DOLLAR_PARENTHESES = '$()'
    DOUBLE_CURLY_BRACES = '{{}}'
    AT_SQUARE_BRACES = '@[]'
    AT_ANGLE_BRACKETS = '@<>'

    SubPair = namedtuple('SubPair', ['regex', 'slice'])


class Template:

    """
        Utility for substituting variables in a template. The template can be the contents of a whole file
        as a string (substitute_string) or in a complex dictionary (substitute_structure).
        substitutions define different type of variables with a regex and a slice:
        - the regex is supposed to find the whole variable, e.g, $(variable)
        - the slice indicate how to slice the value returned by the regex to have the variable name, in the
          case of $(variable), the slice is 2, -1 to remove $( and ).
        You can easily add new type of variables following those rules.

        Please note that the regexes allow for at least one nested variable and the code is able to handle it.
        It means that $($(variable)) will be processed correctly but the substitutions will need more than one
        pass.

        If you have a file that is deeper than just a simple dictionary of has lists in it, you can use the method
        build_index to create a dictionary that will have all the options from deeper levels (list, dicts).
        You can then pass index.get as an argument to any method you use.
        If you use substitute_with_dependencies, this is done automatically.
    """

    substitutions = {
        TemplateConstants.DOLLAR_CURLY_BRACE: TemplateConstants.SubPair(re.compile(r'\${.*?}+'), slice(2, -1)),
        TemplateConstants.DOLLAR_PARENTHESES: TemplateConstants.SubPair(re.compile(r'\$\(.*?\)+'), slice(2, -1)),
        TemplateConstants.DOUBLE_CURLY_BRACES: TemplateConstants.SubPair(re.compile(r'{{.*?}}+'), slice(2, -2)),
        TemplateConstants.AT_SQUARE_BRACES: TemplateConstants.SubPair(re.compile(r'@\[.*?\]+'), slice(2, -1)),
        TemplateConstants.AT_ANGLE_BRACKETS: TemplateConstants.SubPair(
            re.compile(r'@\<.*?\>+'), slice(2, -1))
    }

    @classmethod
    def find_variables(cls, variable_to_substitute: str, var_type: str):
        pair = cls.substitutions[var_type]
        return [x[pair.slice] for x in re.findall(pair.regex, variable_to_substitute)]

    @classmethod
    def substitute_string(cls, variable_to_substitute, var_type: str, get_value):
        """
            Substitutes variables under the form var_type (e.g. DOLLAR_CURLY_BRACE), looks for a value returned
            by function get_value and if found, substitutes the variable. Convert floats and int to string
            before substitution. If the value in the dictionary is a complex type, just assign it instead
            of substituting.
                get_value is a function that returns the value to substitute:
                signature: get_value(variable_name).
                If substituting from a dictionary my_dict, pass my_dict.get
        """
        pair = cls.substitutions[var_type]
        if isinstance(variable_to_substitute, str):
            variable_names = re.findall(pair.regex, variable_to_substitute)
            for variable in variable_names:
                var = variable[pair.slice]
                v = get_value(var)
                if v is not None:
                    if not is_single_type_or_string(v):
                        if len(variable_names) == 1:
                            # v could be a list or a dictionary (complex structure and not a string).
                            # If there is one variable that is the whole
                            # string, we can safely replace, otherwise do nothing.
                            if variable_to_substitute.replace(variable_names[0][pair.slice], '') == var_type:
                                variable_to_substitute = v
                    else:
                        if isinstance(v, float) or isinstance(v, int):
                            v = str(v)
                        if isinstance(v, str):
                            variable_to_substitute = variable_to_substitute.replace(
                                variable, v)
                        else:
                            variable_to_substitute = v
                else:
                    more = re.search(pair.regex, var)
                    if more is not None:
                        new_value = cls.substitute_string(
                            var, var_type, get_value)
                        variable_to_substitute = variable_to_substitute.replace(
                            var, new_value)
        return variable_to_substitute

    @classmethod
    def substitute_structure(cls, structure_to_substitute, var_type: str, get_value):
        """
            Traverses a dictionary and substitutes variables in fields, lists
            and nested dictionaries.
        """
        if isinstance(structure_to_substitute, dict):
            for key, item in structure_to_substitute.items():
                structure_to_substitute[key] = cls.substitute_structure(
                    item, var_type, get_value)
        elif is_sequence_and_not_string(structure_to_substitute):
            for i, item in enumerate(structure_to_substitute):
                structure_to_substitute[i] = cls.substitute_structure(
                    item, var_type, get_value)
        else:
            structure_to_substitute = cls.substitute_string(structure_to_substitute, var_type,
                                                            get_value)
        return structure_to_substitute

    @classmethod
    def substitute_structure_from_environment(cls, structure_to_substitute):
        return cls.substitute_structure(structure_to_substitute, TemplateConstants.DOLLAR_CURLY_BRACE, os.environ.get)

    @classmethod
    def substitute_with_dependencies(cls, dictionary, keys, var_type: str, shallow_precedence=True, excluded=()):
        """
            Given a dictionary with a complex (deep) structure, we want to substitute variables,
            using keys, another dictionary that may also have a deep structure (dictionary and keys
            can be the same dictionary if you want to substitute in place).
            We create an index based on keys (see build_index) and substitute values in dictionary
            using index. If variables may refer to other variables, more than one pass of substitution
            may be needed, so we substitute until there is no more change in dictionary (convergence).
        """
        all_variables = cls.build_index(keys, excluded, shallow_precedence)
        previous = {}
        while dictionary != previous:
            previous = copy.deepcopy(dictionary)
            dictionary = cls.substitute_structure(
                dictionary, var_type, all_variables.get)
        return dictionary

    @classmethod
    def build_index(cls, dictionary, excluded=None, shallow_precedence=True):
        """
            Builds an index of all keys with their values, going deep into the dictionary. The index
            if a flat structure (dictionary).
            If the same key name is present more than once in the structure, we want to
            either prioritise the values that are near the root of the tree (shallow_precedence=True)
            or values that are near the leaves (shallow_precedence=False). We don't anticipate use
            cases where the "nearest variable" should be used, but this could constitute a future
            improvement.
        """
        def build(structure, variables):
            if isinstance(structure, dict):
                for k, i in structure.items():
                    if ((k not in variables) or (k in variables and not shallow_precedence)) and k not in excluded:
                        variables[k] = i
                        build(i, variables)
            elif is_sequence_and_not_string(structure):
                for v in structure:
                    build(v, variables)
        var = {}
        if excluded is None:
            excluded = set()
        build(dictionary, var)
        return var


# These used to be in basic.py, and have been copied here till they are needed elsewhere.


def is_sequence_and_not_string(a):
    return isinstance(a, Sequence) and not isinstance(a, str)


def is_single_type(s):
    try:
        len(s)
    except TypeError:
        return True
    else:
        return False


def is_single_type_or_string(s):
    if isinstance(s, str):
        return True
    try:
        len(s)
    except TypeError:
        return True
    else:
        return False
