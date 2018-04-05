

# each file is the simulation for one point in parameter

# have v, a and t depend on the stimulus

#just a very rough initial example of applying HDDM to ACT-R RT results

import os
import hddm
import matplotlib.pyplot as plt

#folder = '/home/ausmanpa/Documents/gp/ACTR_DDM/simulations/'
folder = '/projects/actr/models/ACTR_DDM/simulations01_redux/'
files = os.listdir(folder)
folderIdx = files.index('DDM_results')
files.pop(folderIdx)


#listdir will list the results folder, remove it

for f in files:
    
    hddmData = hddm.load_csv(folder+f)
    
    #create model object where v, a, and t all depend on the difficulty
    model = hddm.HDDM(hddmData, depends_on={'v':'stim', 'a':'stim', 't':'stim'})
    
    #find a good starting point to help with convergence - but, seems to run into a warning/error
    model.find_starting_values()

    #draw 2000 samples, discard 20 as burn-in
    model.sample(2000, burn=20, dbname = 'traces.db', db='pickle')
    
    tempF = f.split('.txt')[0]
    
    fname = folder + 'DDM_results/' + tempF    
    
    model.save(fname)
    
    del(hddmData, model, tempF, fname)
    
    
    
#model.plot_posteriors(['v','a','t'])
