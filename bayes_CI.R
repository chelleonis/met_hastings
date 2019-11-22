#bayesian credible interval

#returns lower, upper interval 
bayes_CI <- function(mh_output, alpha) {

  hi <- quantile(mh_output, probs = c(alpha/2,1-alpha/2))
  
  return(hi)
}