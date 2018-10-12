# initialise.labs.R
# function to initialise the labs
# Jan 2017

# see Table 1 from paper (global model parameters)
initialise.labs = 
  function(n = 100, # number of labs
           W_0 = 0.8, # lab characteristic power [0,1]
           e_0 = 75, # effort, higher scores mean better labs [1,100]
           r_0 = c(0, 0.01, 0.2, 0.5) # replication rate
  ){
    frame = data.frame(i=1:n, age=0, e_i=e_0, W_i=W_0, cum.papers=0, cum.FP=0, cum.mistakes=0, audit=0)
    frame$r_i = sample(r_0, replace=T, size=n)
    frame$parent = 0 # parent lab (start as all original)
    frame$payoff = 0 # start at zero
    # end and return
    return(frame)
  }
