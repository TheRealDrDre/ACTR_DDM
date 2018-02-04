#!/usr/bin/env python
# ------------------------------------------------------------------ #
# GENERATE-SIMS.py
# ------------------------------------------------------------------ #
# Generates one Lisp file for every point in the hyperparameter
# space. The simulations can then be run in parallel on multi-core
# architectures
# ------------------------------------------------------------------ #

import numpy as np

class ParamRange():
    def __init__(self, name, start, end, step):
        if is_param_name(name) and \\
           is_param_value(start) and \\
           is_param_value(end) and \\
           is_param_value(step):

            self.name = name
            self.start = start
            self.end = end
            self.step = step

        else:
            pass
        

    def is_param_name(self, string):
        if and isintance(string, str) \\
           and len(string) > 1 \\
           and string[0:1] is ":" :

            return True
        else:
            return False


    def is_param_value(self, string):
        try: 
            float(s)
            return True
        except ValueError:
            return False


    def as_list(self):
        """Returns the range as a list"""
        
    
def load_params(filename="params.txt"):
    """Loads the param specification and creates the hyperspace"""
    fin = open(filename, 'r')
    params = []
    for i in fin.xreadlines():
        # Remove comments that start with '#';
        var = i
        if "#" in var:
            var = var[0:var.index("#")]
        var = var.split()
        var = [x.strip() for x in var]

        params.append(ParamRange(var))
    return [x for x in params if x is not None]

        
    

