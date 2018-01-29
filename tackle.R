# tackle.R
# function to generate probability that lab takes on a new hypothesis
# Jan 2017

tackle = 
  function(frame, # input frame
           eta = 0.2 # "constant reflecting the extent to which increased effort lowers the lab's rate of producing new research"
  ){
    h.e_i = 1 - (eta * log10(frame$e_i)) # probability of tackling new hypothesis
    return(h.e_i)
  }
