# perform.communicate.R
# function to perform and communicate research
# Oct 2017

perform.communicate = 
  function(frame, # input frame
           r_i = 0, # replication rate
           eta = 0.2, # "constant reflecting the extent to which increased effort lowers the lab’s rate of producing new research"
           VN = 1,	# pay-off for publishing positive novel result
           VRpos = 0.5,	# pay-off for publishing positive replication
           VRneg = 0.5,	# pay-off for publishing negative replication	
           VOpos = 0.1, # pay-off for having novel result replicated
           VOneg = -100, # pay-off for failing to have novel result replicated
           initial = F, # flag for world starting where there are not enough papers to replicate
	   mistake.baseline = 0.5 # baseline probability of mistake
  ){
    n = nrow(frame)
    # a) start new work
    frame$h.e_i = tackle(frame, eta) # probability of tackling new hypothesis
    frame$new = rbinom(n=n, size=1, prob=frame$h.e_i)
    # increase cumulative number of papers per lab (will need to move below if it becomes dependent on probability of communicating)
    frame$cum.papers = frame$cum.papers + frame$new
    # select papers
    store = subset(frame, new==0) # store records for those not working this round
    if(nrow(store)>0){ # will be zero when effort hits zero
      store$h = store$replicate = store$true = store$pos.obs = store$FP = NA # blank key stats
    }
    frame = subset(frame, new==1) # update records for those working
    n.work = nrow(frame)
    # b) Q: is this an attempt to replicate an existing study?
    frame$replicate = rbinom(n=n.work, size=1, prob=r_i) # supported at least once, i.e., replicate yes/no
    if(initial==T){frame$replicate = 0} # in early worlds labs only tackle new hypotheses
    hindex = (h+1):(h+sum(frame$replicate==0)) # indices for new hypothesis
    # c) is hypothesis true? 
    # select from existing set for new studies
    frame$true[frame$replicate==0] = truth[hindex]
    frame$h[frame$replicate==0] = hindex # record the hypothesis number
    # select truth from previous study for replication
    n.rep = sum(frame$replicate)
    if(n.rep>0){
      past = sample(1:h, size=n.rep, replace=F) # randomly sample from previously tested hypothesis
      frame$true[frame$replicate==1] = truth[past]
      frame$h[frame$replicate==1] = past # record the hypothesis number
    }
    h <<- h + sum(frame$replicate==0) # add new hypotheses to index (global)
    # d) finding positive result
    frame$alpha_i = alpha_i(W_i = frame$W_i, e_i = frame$e_i) # false positive rate
    frame$pos.prob = (frame$W_i*frame$true) + (frame$alpha_i*(1-frame$true)) # probability of positive result conditional on truth
    frame$pos.obs = rbinom(size=1, n=n.work, prob=frame$pos.prob) # hypothesis gives positive result
    # calculate false positive
    frame$FP = as.numeric(frame$true==0) * as.numeric(frame$pos.obs==1) # not true but positive result observed
    frame$cum.FP = frame$cum.FP + frame$FP # cumulative false positive
    # e) cumulative pay-off, conditional on whether it is a replication
    frame$payoff = frame$payoff + (VN*(1-frame$replicate)*frame$pos.obs) + (VRpos*frame$pos.obs*frame$replicate) + (VRneg*(1-frame$pos.obs)*frame$replicate)
    # to do?: replication payoff
    # f) add binomial mistake (Oct 2017)
    frame$mistake = rbinom(size=1, n=n.work, prob=mistake.baseline) # random mistake
    frame$mistake = frame$mistake * frame$FP # only possible where FP error made
    frame$cum.mistakes = frame$cum.mistakes + frame$mistake # cumulative mistakes
    # add back non-working labs (if any)
    if(nrow(store) > 0) {frame = rbind.fill(frame, store)}
    # increase lab age
    frame$age = frame$age + 1
    # end and return
    return(frame)
  }
