#'Metropolis-Hastings Algorithmn (non-adaptive)
#'Random Walk
#1.) starting theta(0)
#2.) at iteration t, draw theta* from jumping distr: Jt(theta*|theta(t-1))
#3.) compute acceptance ratio r = p(theta|y) / Jt(theta* | theta(t-1)) /
# p(theta(t-1)|y / Jt(theta(t-1)|theta*)
#4.) Accept theta* as theta(t) w/ prob min(r,1), 
# if it isn't accepted, theta(t) = theta(t-1)
#5.) repeat 2-4 M times to get M draws, with burn-in / thinning

#jumping distributions:
#normal (default)
#beta 

#sampling distributions:
#gamma
#beta
#normal (default)
#custom(?)

#nsim - how many trials
#start - initial estimate (idk how to derive)
#burn in - how many trials to throw away (default 0)
#jump - what is your jump distr, default = normal
#distr - options: "normal", "gamma", "beta", "logistic", "other"
#for other, provide your own likelhood
#params - params for your likelhood
#likelihood - if choose other, provide a function for your likelhood
met_hastings <- function(nsims, start = 1, burn_in, jump = "normal", jparams = 1,
                         distr = "normal", dparams = 1, likelihood = NULL) {
  #step 1, starting theta
  theta_current <- start
  draws <- rep(NA,nsims) #pre-allocate xd
  #steps 2-4 in update_theta
  #step 5, repeat for preset number of draws
  for (i in 1:nsims) {
   draws[i] <- theta_current <- update_theta(theta_current, jump, jparams,  
                distr, dparams)
  }
  return(draws[(burn_in + 1):nsims])
}
#returns vector of your distribution draws

#example:
#draws <- met_hastings(10000, start = 1, burn_in = 1000,
#   jparams = 2,distr = "gamma",c(1.7,4.4))

#current update_theta is normal jumping with sampling from gamma
update_theta <- function(theta_cur, jump = "normal", jparams,
                         distr = "normal", dparams) {
  #step 2, draw from jumping distribution (default: normal)
  theta_star <- draw_jump(theta_cur,jump,jparams) #oh no no need to fix
  #step 3, compute acceptance (WHEN YOU CAN DO LOG DO LOG)
  accept_ratio <- calc_accept(theta_star, theta_cur, distr,dparams)
  #step 4 acceptance rule
  if (runif(1) <= accept_ratio) {
    return(theta_star)
  }
    else {
      return(theta_cur)
    } 
}


