#!/bin/bash


i=0

for k in {0..15..1}; do
    if [ ! -d simulations01_redux_${k} ]; then
	mkdir simulations01_redux_${k}
    fi
done

for f in `ls /projects/actr/models/ACTR_DDM/simulations01_redux/*.txt`; do
    
    n=$((i/46))
    cp $f simulations01_redux_${n}
    i=$((i+1))
done
