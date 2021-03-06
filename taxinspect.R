# taxinspect.FP.R
# simulate single lab being audited; version based on using false positives
# Oct 2017
# variables to keep in kill data:
svars = c('t','i','parent','age','e_i','cum.papers','cum.mistakes','payoff','alpha_i','FP')

taxinspect = function(
  frame, # input data
  mu_W = 0.01, # probability of mutation for Power
  mu_e = 0.01, # probability of mutation for effort
  mu_r = 0.01, # probability of mutation for replication rate 
  increase_e_audited = 5, # increase in effort by audited labs post-audit
  increase_e_networked = 5, # increase in effort by networked labs post-audit
  FP.threshold = 0.67, # threshold for false positive (kill labs above this limit), default is upper third
  n.papers.min = 50, # minimum number of papers needed to audit
  n.papers.per.auditor = 10, # number of papers per auditor
  auditor.salary = 77, # salary of auditor (USD $10K)
  audit.error = 0, # peer review error of auditors range = [0,1)
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
# ... rate of mistakes is too high
  threshold = quantile(frame$cum.mistakes / frame$cum.papers, FP.threshold, na.rm=T) # time-relative threshold of poor performers (upper tail) based on false positives
  threshold = max(0.05, threshold) # limit threshold at 0.05 of distribution (go no lower)
  if(audit.error > 0){
  	audit.sigma = (select$cum.mistakes*audit.error) / 1.96 # as a proportion of false positives
  	aerror = round(rnorm(n=1, mean=0, sd=audit.sigma)) # plus/minus; centred on zero
  	kill = ( (select$cum.mistakes + aerror) / select$cum.papers ) > threshold
  }
  if(audit.error == 0){
  	kill = ( select$cum.mistakes / select$cum.papers ) > threshold
  }
  if(audit.error == -99){
  	  kill = runif(1) > 0.5 # totally random kill with 50% probability (negative control)
  }
  
  if(kill == T){
    e1 = nrow(eligible)
    rem = subset(eligible, i==select$i, select=c(i,age,audit)) # store to show below
    rem$tax = T
    eligible = subset(eligible, i != select$i) # remove lab
    e2 = nrow(eligible)
    if(e1==e2){cat('kill did not work, select$i=',select$i,'\n')}
    frame = rbind(eligible, not.eligible) # add back old labs
    # extra birth as there's been a death  
    frame = birth.and.death(frame, birth.only=T, mu_e=mu_e, mu_W=mu_W, mu_r=mu_r)
    ## store information on kills and added labs
    kill.store = F
    if(kill.store==T){
       kfile = paste('kills.', sim, '.RData', sep='')
       d = length(dir(pattern=kfile))
       if(d==0){ # first kill
         kill = subset(select, select=svars); kill$kill=T # killed lab
         new = subset(frame[100,], select=svars); new$kill=F # new lab to replace killed lab
         kills = rbind(kill, new)
         save(kills, file=kfile)
       }
       if(d>0){ # concatenate
         load(kfile)
         kill = subset(select, select=svars); kill$kill=T
         new = subset(frame[100,], select=svars); new$kill=F
         kills = rbind(kills, kill, new)
         save(kills, file=kfile)
       }
    }
    if(runif(1)<0.1){removed <<- rbind(removed, rem)} # only record 10%
  }
  if(kill == F){
    eligible = subset(eligible, i != select$i) # remove lab (temporary)
    select$e_i = select$e_i + increase_e_audited # increase effort in audited lab (assume audit triggers better future behaviour)
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
    children$e_i = children$e_i + increase_e_networked # increase effort
    children$e_i[children$e_i>100] = 100 # stop at 100
    frame = rbind(children, not.children) # re-combine labs
  }

  # flow on effect to lab's parent
  parent = subset(frame, i == select$parent)
  if(nrow(parent)>0){
    not.parent = subset(frame, i != select$parent)
    parent$e_i = parent$e_i + increase_e_networked # increase effort
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
