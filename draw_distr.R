draw_jump <- function() {
  if(jump == "normal") {
    theta_star <- rnorm(1, mean = theta_cur, sd = cand.sd) #figure out this param
  }
  else if (jump == "beta") {
    theta_star <- rbeta(1, params, params) #need to add params thingy
  }
  else if (jump == "mvn") {
    theta_star <- norm(n,mean = theta_cur, sd = cand.sd) #same thing
  }
  else {
    theta_star <- rnorm(1, mean = theta_cur, sd = cand.sd) #correct sd
  }
}

calc_accept <- function(tstar, tcur, params, distr) {
  if(distr == "normal") {
    accept <- dnorm(tstar, mean = theta_cur, sd = cand.sd) /
      dnorm(tcur, mean = theta_cur, sd = cand.sd)
  }
  else if (distr == "beta") {
    accept <- dbeta(tstar, mean = theta_cur, sd = cand.sd) /
      dbeta(tcur, mean = theta_cur, sd = cand.sd) #need to add params thingy
  }
  else if (distr == "mvn") { #fix 
    theta_star <- norm(n,mean = theta_cur, sd = cand.sd) #same thing
  }
  else if (distr == "gamma") {
    accept <- dgamma(tstar, mean = theta_cur, sd = cand.sd) /
      dgamma(tcur, mean = theta_cur, sd = cand.sd)
  }
  else { #default to norm
    accept <- dnorm(tstar, mean = theta_cur, sd = cand.sd) /
      dnorm(tcur, mean = theta_cur, sd = cand.sd) #correct sd
  }
  
  
  
}