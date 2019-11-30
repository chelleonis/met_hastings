#' Functions generating draws and calculating the acceptance ratio
#' @usage 
#' To structure your jumping or distribution parameters, look at how the function 
#' e.g. for a normal distribution, jparams = (your standard deviation)
#' If using a custom structure, the likelihood parameters should be passed to dparams
#' and the likelihood function should include input for theta
#' See example #3 in the main function for assisstence in structuring your likelihood function
#' @export
draw_jump <- function(theta_cur, jump = "normal",jparams = 1) {
  if(jump == "normal") {
    theta_star <- rnorm(1, mean = theta_cur, sd = jparams) #figure out this param
  }
  else if (jump == "beta") { 
    theta_star <- rbeta(1, jparams[1], jparams[2]) #need to add params thingy
  }
  else if (jump == "mvn") {
    theta_star <- rnorm(n,mean = theta_cur, sd = jparams) #same thing
  }
  else {
    theta_star <- rnorm(1, mean = theta_cur, sd = jparams) #correct sd
  }
}

calc_accept <- function(tstar, tcur,distr, dparams = 1, lk = NULL) {
  if(distr == "normal") {
    accept <- dnorm(tstar, mean = tcur, sd = dparams) /
      dnorm(tcur, mean = tcur, sd = dparams)
  }
  else if (distr == "beta") {
    accept <- dbeta(tstar, dparams[1],dparams[2]) /
      dbeta(tcur, dparams[1],dparams[2]) #need to add params thingy
  }
  else if (distr == "binomial") {
    accept <- dbinom(floor(tstar), dparams[1], dparams[2]) /
      dbinom(floor(tcur), dparams[1], dparams[2])
  }
  else if (distr == "mvn") { #fix 
    theta_star <- dnorm(n,mean = theta_cur, sd = cand.sd) #same thing
  }
  else if (distr == "gamma") {
    accept <- dgamma(tstar, dparams[1],dparams[2]) /
      dgamma(tcur, dparams[1],dparams[2])
  }
  else if(distr == "custom") {
    accept <- lk(tstar,dparams) / lk(tcur,dparams)
  }
  else { #default to norm
    accept <- dnorm(tstar, mean = tcur, sd = dparams) /
      dnorm(tcur, mean = tcur, sd = dparams) #correct sd
  }
  
  
  
}