#!/usr/bin/env python
# ------------------------------------------------------------------ #
# GENERATE-SIMS.py
# ------------------------------------------------------------------ #
# Generates one Lisp file for every point in the hyperparameter
# space. The simulations can then be run in parallel on multi-core
# architectures
# ------------------------------------------------------------------ #

import numpy as np
import sys

class ParamRange():
    def __init__(self, name, start, end, step):
        if self.is_param_name(name)        \
           and self.is_param_value(start)  \
           and self.is_param_value(end)    \
           and self.is_param_value(step)   \
           and float(end) > float(start):

            self.name = name
            self.start = float(start)
            self.end = float(end)
            self.step = float(step)

        else:
            pass
        

    def is_param_name(self, string):
        if isinstance(string, str) \
           and len(string) > 1    \
           and string[0:1] is ":" :

            return True
        else:
            return False


    def is_param_value(self, string):
        try: 
            float(string)
            return True
        except ValueError:
            return False


    def expand(self):
        """Returns the range as a list"""
        return list(np.arange(self.start, self.end, self.step))


def load_params(filename="params.txt"):
    """Loads the param specification and creates the hyperspace"""
    fin = open(filename, 'r')
    params = []
    for i in fin.readlines():
        # Remove comments that start with '#';
        var = i
        if "#" in var:
            var = var[0:var.index("#")]
        var = var.split()
        var = [x.strip() for x in var]

        candidate = ParamRange(var[0], var[1], var[2], var[3])
        if candidate is None:
            print("Error in line: ``%s''', no parameter created")
        else:
            params.append(candidate)
    return [x for x in params if x is not None]

def HyperPoint():
    """Hyperpoint in parameter space"""
    def __init__(self, parameters, values):
        self.internal = zip(parameters, values)

    def lisp_representation(self):
        

def HyperSpace():
    """Hyper parameter space"""
    def __init__(self, lst):
        self.params = lst

    def get_points(self):
        """Returns this hyperspace as a list of hyperpoints"""
        pass

    # Here we should include a function to chop
    # the space into N subspaces, cut somehow.
    # Or we should include a function to "cut across"
    # N possible dimensions, simulating across all
    # the others
    #
    # e.g. - cut_across([param1, param2, ..., paramN])
    #  --> N smaller hyperspaces
    def cut_across(param_list):
        """Returns a series of hyperspaces across the values of given params"""
        pass

if __name__ == "__main__":
    params = load_params(sys.argv[1])
    for p in params:
        print("%s: %s" % (p.name, p.expand()))
        
