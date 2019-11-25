#'Metropolis-Hastings Algorithmn Random Walk (non-adaptive)
#'
#'1.) Choose a starting \[\theta^(0)\] \\
#'2.) At iteration t, draw \[\theta^*\] from jumping distribution: 
#' \[J_t(\theta^* | \theta^(t-1)) \] \\
#'3.) compute acceptance ratio r = p(theta|y) / Jt(theta* | theta(t-1)) /
#' p(theta(t-1)|y / Jt(theta(t-1)|theta*) \\
#'4.) Accept theta* as theta(t) w/ prob min(r,1), 
#' if it isn't accepted, theta(t) = theta(t-1) \\
#'5.) repeat 2-4 M times to get M draws, thinning out \\


#'@param nsims - how many trials
#'@param start - initial estimate
#'@param burn_in - how many trials to throw away, default = 0
#'@param jump - what is your jump distribution, default = normal
#'options: normal, beta
#'@param jparams - options for chosen jumping distr, default for normal: variance = 1
#'@param distr - what distribution you are sampling from
#' options: "normal", "gamma", "beta", "logistic", "custom". 
#' If other is selected, user should provide a likelihood function (see example) and parameters
#'@param dparams - params for your sampling distribution or likelihood
#'@param likelihood - if choose other, provide a function for your likelhood
#'
met_hastings <- function(nsims, start = 1, burn_in = 0, jump = "normal", jparams = 1,
                         distr = "normal", dparams = 1, likelihood = NULL) {
  #step 1, starting theta
  theta_current <- start
  draws <- rep(NA,nsims) #pre-allocate xd
  #steps 2-4 in update_theta
  #step 5, repeat for preset number of draws
  for (i in 1:nsims) {
   draws[i] <- theta_current <- update_theta(theta_current, jump, jparams,  
                distr, dparams, likelihood)
  }
  return(draws[(burn_in + 1):nsims])
}
#' @return Vector of your post burn-in draws

#' @examples
#' draws <- met_hastings(10000, start = 1, burn_in = 1000,
#'   jparams = 2,distr = "gamma",c(1.7,4.4))
#'   
#' draws_2 <- met_hastings(nsims = 100000, start = 1, burn_in = 1000, jump = "beta", 
#'    jparams = c(2,3), distr = "binomial", dparams = c(100,0.05))
#' 
#' Slightly more complicated usage, draw distribution represents a "t-distribution"
#' 
#' nn_lk <- function(tstar,params) {
#   lkld <- 0.5*rnorm(1,tstar,params[1]) + 0.5*rnorm(1,0,params[2])
#' }
#' 
#' draws_3 <- met_hastings(nsims = 10000, start = 1, burn_in = 2500, 
#' distr = "custom", dparams = c(1,100) , likelihood = nn_lk)
#' 
#'
#'    

update_theta <- function(theta_cur, jump = "normal", jparams,
                         distr = "normal", dparams, likelihood = NULL) {
  #step 2, draw from jumping distribution
  theta_star <- draw_jump(theta_cur,jump,jparams)
  #step 3, compute acceptance
  accept_ratio <- calc_accept(theta_star, theta_cur, distr,dparams, lk = likelihood)
  #step 4 acceptance rule
  if (runif(1) <= accept_ratio) {
    return(theta_star)
  }
    else {
      return(theta_cur)
    } 
}

#benchmark against MH_Adaptive

