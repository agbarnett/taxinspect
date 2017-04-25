# simulate.research.world.R
# simulate the research world
# replicate http://rsos.royalsocietypublishing.org/content/3/9/160384 
# Jan 2017
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
e_0 = 75 
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
tax.audit = T
increase_e = 5
FP.threshold = 0.67
n.papers.min = 25 # 50 for most
n.papers.per.auditor = 10
auditor.salary = 77
audit = 80 # audit frequency (in time)

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

## loop through time
#all.past = NULL # need to store all past results for new hypothesis for confirming replications
for (t in 1:max.time){

# 2) Run a cycle of research
init = F
if (t < 5) {init = T} # no replication in early times
labs = perform.communicate(frame=labs, initial = init, r_i=r_i, eta=eta, VN=VN, VRneg=VRneg, VRpos=VRpos)
labs$t = t

#to.record = subset(labs, replicate==0, select=c('i','h','pos.obs'))
#all.past = rbind(all.past, to.record)

# 3) birth and death (and mutation)
labs = birth.and.death(labs, mu_e=mu_e, mu_r=mu_r, mu_W=mu_W, sigma_e=sigma_e, sigma_W=sigma_W, sigma_r=sigma_r, birth.death=birth.death) 

# 4) audit (only start after 100)
if(tax.audit == T & (t > 100) & (t %% audit == 0)){
  labs = taxinspect(labs, mu_e=mu_e, mu_r=mu_r, mu_W=mu_W,
    increase_e = increase_e, FP.threshold = FP.threshold, n.papers.min = n.papers.min,
    n.papers.per.auditor = n.papers.per.auditor, auditor.salary = auditor.salary)
}

# keep occasional record of labs over time
if(t %% collect == 0){
  cat('t=', t,'\n', sep='')
  filename = paste(fint, run.number, '.', t/collect ,'.RData', sep='')
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
    tax.audit=tax.audit, increase_e = increase_e, FP.threshold = FP.threshold, n.papers.min = n.papers.min,
    n.papers.per.auditor = n.papers.per.auditor, auditor.salary = auditor.salary,
                       cost=cost, audit=audit, naudit=naudit, total.papers=total.papers)

# save meta data
outfile = paste('Yetax.', run.number, '.RData', sep='')
save(sim.parms, file=outfile)
