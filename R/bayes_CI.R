#' Creation of Bayesian Credible Interval from MH Samples
#' @param mh_output - vector output from met_hastings function
#' @param alpha - value of bounds, calculated as alpha, 1-alpha/2
#' @return Lower and upper bounds of the alpha interval
#' @export
#' @examples 
#'   
#' 95% credible interval for our draws in example #1
#' 
#' out <- met_hastings(10000, start = 1, burn_in = 1000,
#'   jparams = 2,distr = "gamma",c(1.7,4.4))
#' bayes_CI(out, 0.1)

bayes_CI <- function(mh_output, alpha) {
  hi <- apply(mh_output,2,quantile,probs = c(alpha/2,1-alpha/2))
  return(hi)
}