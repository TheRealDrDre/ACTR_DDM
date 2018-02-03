#!/usr/bin/env python
# ------------------------------------------------------------------ #
# GENERATE-SIMS.py
# ------------------------------------------------------------------ #
# Generates one Lisp file for every point in the hyperparameter
# space. The simulations can then be run in parallel on multi-core
# architectures
# ------------------------------------------------------------------ #

def load_params(filename="params.txt"):
    """Loads the param specification and creates the hyperspace"""
    fin = open(filename, 'r')
    params = []
    for i in fin.xreadlines():
        # Remove comments that start with '#';
        # Trim the string;
        # Tokenize it;
        # Check if tokens are a parameter range:
        #   Parameter range:
        #   1. Parameter name, string that starts with ":"
        #   2. Parameter starting point, number
        #   3. Parameter end point, number (END < START)
        #   4. Parameter step, number
        pass
    

