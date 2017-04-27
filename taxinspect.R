# taxinspect.R
# simulate single lab being audited
# Jan 2017
# variables to keep in kill data
svars = c('t','i','parent','age','e_i','cum.papers','cum.FP','payoff','alpha_i','FP')

taxinspect = function(
  frame, # input data
  mu_W = 0.01, # probability of mutation for Power
  mu_e = 0.01, # probability of mutation for effort
  mu_r = 0.01, # probability of mutation for replication rate 
  increase_e = 5, # increase in effort post-audit
  FP.threshold = 0.67, # threshold for false positive (kill labs above this limit), default is upper third
  n.papers.min = 50, # minimum number of papers needed to audit
  n.papers.per.auditor = 10, # number of papers per auditor
  auditor.salary = 77, # salary of auditor (USD $10K)
  sim = NA # simulation number
){

  eligible = subset(frame, cum.papers >= n.papers.min & audit==0) # select labs with more than minimum number of papers that have not been audited
  not.eligible = subset(frame, cum.papers < n.papers.min | audit==1) # store these
  if(nrow(not.eligible) < 100){ # only if there are labs eligible
  rand = sample(1:nrow(eligible), size = 1)
  select = eligible[rand, ] # randomly select one lab for auditing
  naudit <<- naudit + select$cum.papers # number of papers audited
  cost <<- cost + (auditor.salary / 12) * round(select$cum.papers / n.papers.per.auditor)  # audit costs
  
# kill lab if ...
  # ... effort is too low?
  #  kill = select$effort < 75
  # ... replication rate is too low?
# ... false positive rate is too high
  threshold = quantile(frame$cum.FP / frame$cum.papers, FP.threshold, na.rm=T) # time-relative threshold of poor performers (upper tail) based on false positives
  threshold = max(0.05, threshold) # limit threshold at FP prob of 0.05 (go no lower)
  audit.error = 0 # option to add measurement error (peer reviewer error) here to select$cum.FP
  #audit.error = rpois(select$cum.papers, lambda)
  kill = ( (select$cum.FP + audit.error) / select$cum.papers) > threshold
  if(kill == T){
    e1 = nrow(eligible)
    eligible = subset(eligible, i != select$i) # remove lab
    e2 = nrow(eligible)
    if(e1==e2){cat('kill did not work, select$i=',select$i,'\n')}
    frame = rbind(eligible, not.eligible) # add back old labs
    # extra birth if there's been a death  
    frame = birth.and.death(frame, birth.only=T, mu_e=mu_e, mu_W=mu_W, mu_r=mu_r)
    ## store information on kills
    kfile = paste('kills.', sim, '.RData', sep='')
    d = length(dir(pattern=kfile))
    if(d==0){
      kills = subset(select, select=svars)
      save(kills, file=kfile)
    }
    if(d>0){
      load(kfile)
      kills = rbind(kills, subset(select, select=svars))
      save(kills, file=kfile)
    }
  }
  if(kill == F){
    eligible = subset(eligible, i != select$i) # remove lab (temporary)
    select$e_i = select$e_i + increase_e # increase effort in audited lab (assume audit triggers better future behaviour)
    select$e_i[select$e_i>100] = 100 # stop at 100
    select$audit = 1 # flag for audit
    frame = rbind(eligible, not.eligible, select) # concatenate all labs
  }
  # error flagging:
 if(nrow(frame) != 100){
    cat('Not 100 labs after kill or not, number=',nrow(frame),'\n')
    print(select)
    cat('kill=', kill, '\n')
    cat('eligible=', nrow(eligible), '\n')
    cat('not eligible=', nrow(not.eligible), '\n')
    cat('frame:')
    print(frame)
  }

# flow on effect to lab's children (even without kill)
  children = subset(frame, parent == select$i) # labs with this parent
  if(nrow(children)>0){
    not.children = subset(frame, parent != select$i)
    children$e_i = children$e_i + increase_e # increase effort
    children$e_i[children$e_i>100] = 100 # stop at 100
    frame = rbind(children, not.children) # re-combine labs
  }

  # flow on effect to lab's parent
  parent = subset(frame, i == select$parent)
  if(nrow(parent)>0){
    not.parent = subset(frame, i != select$parent)
    parent$e_i = parent$e_i + increase_e # increase effort
    parent$e_i[parent$e_i>100] = 100 # stop at 100
    frame = rbind(parent, not.parent) # re-combine labs
  }
  
  if(nrow(frame) != 100){
    cat('Not 100 labs, number=',nrow(frame),'\n')
    print(select)
    cat('kill=', kill, '\n')
    cat('parent=', nrow(parent), '\n')
    cat('children=', nrow(children), '\n')
    cat('not children=', nrow(not.children), '\n')
    cat('eligible=', nrow(eligible), '\n')
    cat('not eligible=', nrow(not.eligible), '\n')
  }
  } # end of nrow(not.eligible)
return(frame)
}
