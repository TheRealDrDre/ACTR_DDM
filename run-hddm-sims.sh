#!/bin/bash

for d in `ls -d simulations01_redux_*`; do 
    nice ${d}/runmodel.py &
done
