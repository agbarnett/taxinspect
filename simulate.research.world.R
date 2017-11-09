# simulate.research.world.R
# simulate the research world
# replicate http://rsos.royalsocietypublishing.org/content/3/9/160384 
# Oct 2017
library(reshape2)
library(doBy)
library(plyr)
library(ggplot2) # used by false positive
source('false.positive.R')
source('perform.communicate.R')
source('initialise.labs.R')
source('initialise.labs.random.R')
source('birth.and.death.R')
source('taxinspect.R')
source('tackle.R')

## set up all parameters
b = 0.1 # base rate probability that novel hypothesis are true
e_0 = 75 # usually 75
r_0 = 0
W_0 = 0.8
initial_random = F
r_i = 0
eta = 0.2
VN = 1
VRpos = 0.5
VRneg = 0.5
mu_e = 0.01
mu_r = 0
mu_W = 0
birth.death = 10
sigma_e = 1
sigma_r = 0.01
sigma_W = 0.01
tax.audit = F # T for most
increase_e_audited = 1 # 5 for most
increase_e_networked = 0.5 # 5 for most
FP.threshold = 0.67 # 0.67 for most
n.papers.min = 50 # 50 for most
n.papers.per.auditor = 10
auditor.salary = 105 # in USD $1000
audit = 200 # audit frequency (in time); 70 baseline
audit.error = 0 # 0 for most, range [0,1); -99 = random kill
mistake.baseline = 0.25 # probability of mistake when there is a false positive

# set up times
max.time = 800000 # maximum time examined
n.results = 100 # number of results to keep
collect = max.time / n.results # frequency of data collection for plotting

# 1a) initialise labs
if(initial_random == F){labs = initialise.labs(r_0=r_0, e_0=e_0, W_0=W_0)} # Figure 3/4
if(initial_random == T){labs = initialise.labs.random(r_0=r_0, e_0=e_0, W_0=W_0)} # 
# 1b) initialise hypothesis as true or false
truth <<- rbinom(n=100*max.time, size=1, prob=b) # novel hypothesis are true (global variable)
h <<- 0 # index for hypothesis (global variable)
cost <<-0 # cost of audits (global variable)
naudit <<-0 # number of papers audited
removed <<- NULL # to store data on removed labs
total.FP <<-0 # total number of false positives
total.mistake <<-0 # total number of mistakes

## loop through time
#all.past = NULL # need to store all past results for new hypothesis for confirming replications
for (t in 1:max.time){

# 2) Run a cycle of research
init = F
if (t < 5) {init = T} # no replication in early times (allow hypotheses to build up)
labs = perform.communicate(frame=labs, initial = init, r_i=r_i, eta=eta, VN=VN, VRneg=VRneg, VRpos=VRpos, mistake.baseline=mistake.baseline)
labs$t = t
# calculate totals over 100 labs for later costs
Mistakes = sum(labs$mistake, na.rm=T)
FPs = sum(labs$FP, na.rm=T)
total.FP <<- total.FP + FPs
total.mistake <<- total.mistake + Mistakes

# 3) birth and death (and mutation)
labs = birth.and.death(labs, mu_e=mu_e, mu_r=mu_r, mu_W=mu_W, sigma_e=sigma_e, sigma_W=sigma_W, sigma_r=sigma_r, birth.death=birth.death, birth.only=F) 

# 4) audit (only start after 100 research cycles)
if(tax.audit == T & (t > 100) & (t %% audit == 0)){
  labs = taxinspect(labs, mu_e=mu_e, mu_r=mu_r, mu_W=mu_W,
    increase_e_audited = increase_e_audited, increase_e_networked = increase_e_networked, FP.threshold = FP.threshold, n.papers.min = n.papers.min,
    n.papers.per.auditor = n.papers.per.auditor, auditor.salary = auditor.salary, sim=run.number,
    audit.error = audit.error)
}

# keep occasional record of labs over time (also collect time 1)
if( t==1 | (t %% collect == 0) ){
  cat('t=', t,'\n', sep='')
  fnum = t/collect # file number
  if(t==1){fnum = 0}
  filename = paste(fint, run.number, '.', fnum ,'.RData', sep='')
  save(labs, file=filename)
}

} # end of time loop

# calculate total papers
total.papers = max(labs$h, na.rm=T)
# save all parameters
sim.parms = data.frame(b=b, e_0=e_0, r_0=r_0, W_0=W_0, initial_random=initial_random,
                       max.time=max.time, r_i=r_i, eta=eta,
                       VN=VN, VRneg=VRneg, VRpos=VRpos,
                       mu_e=mu_e, mu_r=mu_r, mu_W=mu_W, 
                       sigma_e=sigma_e, sigma_r=sigma_r, sigma_W=sigma_W, birth.death=birth.death,
    tax.audit=tax.audit, increase_e_audited = increase_e_audited, increase_e_networked = increase_e_networked,
	FP.threshold = FP.threshold, n.papers.min = n.papers.min,
    n.papers.per.auditor = n.papers.per.auditor, auditor.salary = auditor.salary,
audit.error = audit.error, # added Oct 2017
                       cost=cost, audit=audit, naudit=naudit, total.papers=total.papers,
					  mistake.baseline = mistake.baseline) # added Oct 2017

# save meta data
outfile = paste('meta.data.', run.number, '.RData', sep='')
save(sim.parms, file=outfile)

# save data on removed labs
#outfile = paste('removed.', run.number, '.RData', sep='')
#save(removed, file=outfile)

# save data on total number of false positives and mistakes
outfile = paste('totals.', run.number, '.RData', sep='')
save(t, total.FP, total.mistake, total.papers, file=outfile)
