# birth.and.death.R
# birth and death of lab (plus mutation of new labs)
# Jan 2017

birth.and.death = function(
  frame, # input data
  birth.death = 10, # number of labs sampled for death and birth events
  mu_W = 0.01, # probability of mutation for Power
  mu_e = 0.01, # probability of mutation for effort
  mu_r = 0.01, # probability of mutation for replication rate 
  sigma_r = 0.01, # standard deviation of replication rate
  sigma_e = 1, # standard deviation of effort 
  sigma_W = 0.01, # standard deviation of Power
  birth.only = F # just complete the birth cycle (TRUE/FALSE)
){
  
## death  
  if(birth.only != T){ # skip if it's just a birth
# a) randomly select a subset of labs
index = sample(1:nrow(frame), size=birth.death, replace=F) 
# b) then find the oldest
oldest = subset(frame[index,], age==max(frame[index,]$age))$i # find the oldest
if(length(oldest)>1){oldest = sample(oldest, size=1)}
if(length(oldest)!=1){cat('error, oldest lab for removal not found.\n')}
frame = subset(frame, i!= oldest) # remove lab from population
  }
  
## birth
# a) randomly select a subset ...
index = sample(1:nrow(frame), size=birth.death, replace=F) 
# b) ... then find one with higest payoff
high = subset(frame[index,], payoff==max(frame[index,]$payoff))$i # find the highest payoff
if(length(high) > 1){high = sample(high, size=1)} # if ties then select one
new.lab = frame[frame$i==high, ]
new.lab$age = new.lab$payoff = 0 # reset age and pay-off
new.lab$parent = new.lab$i # new lab's parent lab
new.lab$i = new.lab$i + runif(n=1) # new lab number
new.lab$cum.papers = 0 # cumulative papers
new.lab$cum.FP = 0 # cumulative FP numbers
new.lab$audit = 0 # audit status
new.lab$new = new.lab$h = new.lab$true = new.lab$replicate = new.lab$pos.obs = new.lab$FP = NA # blank carried over results that are not applicable
# c) mutate
mutate.power = rbinom(size=1, n=1, prob=mu_W)
mutate.effort = rbinom(size=1, n=1, prob=mu_e)
mutate.replication = rbinom(size=1, n=1, prob=mu_r)
if(mutate.power==1) {
  new.lab$W_i = new.lab$W_i + rnorm(n=1, mean=0, sd=sigma_W)
  new.lab$W_i = min(max(new.lab$W_i, 0), 1) # can't go outside limits
}
if(mutate.effort==1) {
  new.lab$e_i = new.lab$e_i + rnorm(n=1, mean=0, sd=sigma_e)
  new.lab$e_i = min(max(new.lab$e_i, 1), 100) # can't go outside limits
}
if(mutate.replication==1) {
  new.lab$r_i = new.lab$r_i + rnorm(n=1, mean=0, sd=sigma_r)
  new.lab$r_i = min(max(new.lab$e_i, 0), 1) # can't go outside limits
}

# concatenate together surviving labs and new lab
frame = rbind(frame, new.lab)

# temporary, output if less than 100 labs
if(nrow(frame) < 100){
   outfile = paste('error', rnorm(1), '.RData', sep='')
   save(frame, new.lab, oldest, high, file=outfile)
}

return(frame)
}
