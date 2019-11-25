#'Metropolis-Hastings Algorithmn Random Walk (non-adaptive)
#'
#' The algorithmn works as follows: \cr
#'1.) Choose a starting \eqn{\theta(0)} \cr
#'2.) At iteration t, draw \eqn{\theta(*)} from jumping distribution: 
#' \eqn{J_t(\theta(*) | \theta(t-1)} \cr
#'3.) Compute acceptance ratio \eqn{r = ( p(\theta|y) / J_t(\theta(*) | \theta(t-1) ) /
#' ( p(\theta(t-1) | y) / J_t(\theta(t-1) | \theta(*) ) )} \cr
#'4.) Accept \eqn{\theta(*)} as \eqn{\theta(t)} w/ probability: min(r,1), 
#' if it isn't accepted, \eqn{\theta(t) = \theta(t-1)} \cr
#'5.) repeat steps 2-4 nsim times \cr


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
#' @return Vector of your post burn-in draws
#' @examples 
#' draws_1 <- met_hastings(10000, start = 1, burn_in = 1000,
#'   jparams = 2,distr = "gamma",c(1.7,4.4))
#'   
#' draws_2 <- met_hastings(nsims = 100000, start = 1, burn_in = 1000, jump = "beta", 
#'    jparams = c(2,3), distr = "binomial", dparams = c(100,0.05))
#' 
#' Slightly more complicated usage, draw distribution represents a "t-distribution"
#' 
#' nn_lk <- function(tstar,params) {
#'   lkld <- 0.5*rnorm(1,tstar,params[1]) + 0.5*rnorm(1,0,params[2])
#' }
#' 
#' draws_3 <- met_hastings(nsims = 10000, start = 1, burn_in = 2500, 
#' distr = "custom", dparams = c(1,100) , likelihood = nn_lk)
#' 
#'@examples benchmark against MHAdaptive
#'Note: Since there are no comparable rpackages, equivalence is unfortunately not 
#'testable. To show that the code is fast for its intended use, a crude performance
#'check is done as follows: 
#' \dontrun{
#' MHadaptive example
#' install.packages(MHadaptive)
#' library(MHadaptive) 
#' li_reg<-function(pars,data)
#' {
#'   a<-pars[1] #intercept
#'   b<-pars[2] #slope
#'   sd_e<-pars[3] #error (residuals)
#'  if(sd_e<=0){return(NaN)}
#'   pred <- a + b * data[,1]
#'   log_likelihood<-sum( dnorm(data[,2],pred,sd_e, log=TRUE) )
#'   prior<- prior_reg(pars)
#'   return(log_likelihood + prior)
#' }
#' 
#' prior_reg<-function(pars)
#' {
#'   a<-pars[1] #intercept
#'   b<-pars[2] #slope
#'   epsilon<-pars[3] #error
#'   prior_a<-dnorm(a,0,100,log=TRUE) ## non-informative (flat) priors on all
#'   prior_b<-dnorm(b,0,100,log=TRUE) ## parameters.
#'   prior_epsilon<-dgamma(epsilon,1,1/100,log=TRUE)
#'   return(prior_a + prior_b + prior_epsilon)
#' }
#' simulate data
#' x<-runif(30,5,15)
#' y<-x+rnorm(30,0,5)
#' d<-cbind(x,y)
#'
#' system.time(Metro_Hastings(li_func=li_reg,pars=c(0,1,1),
#'                            par_names=c('a','b','epsilon'),data=d))
#' system.time(met_hastings(nsims = 100000, start = 1, burn_in = 2500,
#'                          distr = "custom", dparams = c(1,100) , likelihood = nn_lk))
#' }

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



