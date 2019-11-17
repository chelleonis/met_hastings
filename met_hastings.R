#Metropolis-Hastings w/ and w/o burn-in(?)
# 
#1.) starting theta(0)
#2.) at iteration t, draw theta* from jumping distr: Jt(theta*|theta(t-1))
#3.) compute acceptance ratio r = p(theta|y) / Jt(theta* | theta(t-1)) /
# p(theta(t-1)|y / Jt(theta(t-1)|theta*)
#4.) Accept theta* as theta(t) w/ prob min(r,1), 
# if it isn't accepted, theta(t) = theta(t-1)
#5.) repeat 2-4 M times to get M draws, with burn-in / thinning

#functions to add
#bayesian confidence interval
#plotting


#start with gamma, from normal jumping  , then expand
met_hastings <- function(nsims, start, burn_in, cand.sd, shape, rate) {
  #step 1, starting theta
  theta_current <- start
  draws <- rep(NA,nsims) #pre-allocate xd
  #steps 2-4 in update_theta
  #step 5, repeat for preset number of draws
  for (i in 1:nsims) {
   draws[i] <- theta_current <- update_theta(theta_current, 
                                             shape = shape,rate = rate,cand.sd)
  }
  return(draws[(burn_in + 1):nsims])
}

#example:
#mh.draws <- mh.gamma(10000, start = 1, burnin = 1000, cand.sd = 2,shape = 1.7, rate = 4.4)

#current update_theta is normal jumping with sampling from gamma
update_theta <- function(theta_cur, shape, rate, cand.sd) {
  #step 2, draw from jumping distribution (default: normal)
  theta_star <- rnorm(1, mean = theta_cur, sd = cand.sd)
  #step 3, compute acceptance ratio (can do log)
  accept_ratio <- dgamma(theta_star, shape = shape, rate = rate)/
    dgamma(theta_cur, shape = shape, rate = rate)
  #step 4 acceptance rule
  if (runif(1) <= accept_ratio) {
    return(theta_star)
  }
    else {
      return(theta_cur)
    } 
}
