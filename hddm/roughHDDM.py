#just a very rough initial example of applying HDDM to ACT-R RT results

import hddm
import matplotlib.pyplot as plt

#folder = '/home/ausmanpa/Documents/gp/ACTR_DDM/simulations/'
folder = '/actr/models/ACTR_DDM/simulations/'
fname = 'test.csv'

data = hddm.load_csv(folder+fname)

#the column names need to be adjusted (there are spaces in front of the actual name)
colNames = data.columns
hddmData = data.rename(index=str, columns={'idx':'subj_idx',
                                colNames[1]:colNames[1][1:],
                                colNames[2]:colNames[2][1:],
                                colNames[3]:colNames[3][1:],
                                colNames[4]:colNames[4][1:],
                                colNames[5]:colNames[5][1:]})

print(hddmData)

#let's plot correct and error RT distributions first
rtDistData = hddm.utils.flip_errors(hddmData)
fig = plt.figure()
ax = fig.add_subplot(111, xlabel='RT', ylabel='count', title='RT distributions')
for i, subj_data in rtDistData.groupby('subj_idx'):
    subj_data.rt.hist(bins=20, histtype='step', ax=ax)
    

#create model object where drift-rate v depends on the difficulty
model = hddm.HDDM(hddmData, depends_on={'v':'stim'})

#find a good starting point to help with convergence - but, seems to run into a warning/error
model.find_starting_values()

#draw 2000 samples, discard 20 as burn-in
model.sample(2000, burn=20)

#############################################################################################################

#if we instead wanted to use the gelman-rubin statistic to check convergence, we should run multiple models:
#this would take awhile!
models=[]
for i in range(5):
    m = hddm.HDDM(hddmData, depends_on={'v':'stim'})
    m.find_starting_values()
    m.sample(1000,burn=10)
    models.append(m)
    
hddm.analyze.gelman_rubin(models)

#############################################################################################################

#generate stats on the model results
stats = model.gen_stats()

#there are a lot of stats, so let's just look at a few
stats[stats.index.isin(['v( easy)','v( difficult)','a','t'])]

#let's plot the posteriors for these parameters
model.plot_posteriors(['v','a','t']) #'v' will plot for both v( easy) and v( difficult)

#this plots individual subject RT distributions on top of predictive likelihood
#we have a lot of "subjects" so it's unreadable - will have to figure out argument to restrict the number of subjs plotted
model.plot_posterior_predictive(figsize=(14, 10))

#let's look at the posteriors of v for the two conditions
v_Easy = model.nodes_db.node[['v( easy)']]
hddm.analyze.plot_posterior_nodes([v_Easy[0]])

v_Diff = model.nodes_db.node[['v( difficult)']]
hddm.analyze.plot_posterior_nodes([v_Diff[0]])

hddm.analyze.plot_posterior_nodes([v_Easy[0],v_Diff[0]])
plt.xlabel('drift-rate')
plt.ylabel('Posterior probability')
plt.title('Posterior of drift-rate group means')

#what's the posterior probability that the Easy drift rate is larger than the Difficult drift rate?
print('P(v_Easy > v_Diff) =', (v_Easy[0].trace() > v_Diff[0].trace()).mean())

#what's the deviance information criterion (DIC; lower is better) of the model?
print('DIC =',model.dic)










