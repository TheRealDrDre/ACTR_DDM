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

class HyperPoint():
    """Hyperpoint in parameter space"""
    def __init__(self, parameters, values):
        self._internal = dict(zip(parameters, values))

    def __repr__(self):
        return self.lisp_representation()

    def __str__(self):
        return self.__repr__()
    
    def lisp_representation(self):
        """Returns a string representing the HP in Lisp notation"""
        string = "("
        params = list(self._internal.keys())
        params.sort()
        for k in params[:-1]:
            v = self._internal[k]  # The value
            string += "(%s %.3f) " % (k, v)
        
        string += "(%s %.3f))" % (params[-1], self._internal[params[-1]])
        return string

    
def cmbn(lst1, lst2):
    res = []
    for a in lst1:
        for b in lst2:
            partial = []
            if isinstance(a, list) and isinstance(b, list):
                partial = a + b

            elif isinstance(a, list) and not isinstance(b, list):
                partial = a + [b]

            elif not isinstance(a, list) and isinstance(b, list):
                partial = [a] + b

            elif not isinstance(a, list) and not isinstance(b, list):
                partial = [a, b]
            
            res.append(partial)
    return res

def combinations(lst):
    """Returns the permiutations of all the lists in LST"""
    if len(lst) > 0:
        res = lst[0]
        for axis in lst[1:]:
            res = cmbn(res, axis)
        return res
    else:
        return []
            
    
class HyperSpace():
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
    
    def divide_into(n):
        """Attempts to devide the parameter space into N subspaces"""
        pass
    
if __name__ == "__main__":
    params = load_params(sys.argv[1])
    for p in params:
        print("%s: %s" % (p.name, p.expand()))
        
