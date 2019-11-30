# met_hastings
Intuitive Metropolis Hastings Simulation package for bios625

The Metropolis-Hastings algorithmn is a Markov chain Monte Carlo method commonly used in bayesian statistics to obtain a probability distribution for the posterior distirbution when it is hard 

For more information on the Metropolis-Hastings algorithmn it is highly recommended to read chapter 10 of Hoff, A First Course in Bayesian Statistical Methods and the wikipedia page.

For example, conjugate-priors like the exponential-gamma family have easy to derive posteriors ( posterior ~ Gamma(alpha + n, Beta + sum xi) ). But in cases like the logistic or poisson regression model, the typical prior class of the multivariate norm is unavaible due to restrictions on beta. Traditional methods of derviation are inefficient for non-trivial cases; Thus, numerical methods like M-H are necessary.

Most current packages (MHadaptive,mcmc, bayesianTools, MetropolisHastings, metrop) offer generalized and comprehensive coverage for performing this MCMC method, but are slightly overwhelming for those just getting into these methods. This package aims to ease the transition into use of aforementioned packages by mimicing what an introductory bayesian class would use as a homework excercise, in the hope that the building blocks of those packages are demystified come time to use them.

To install and launch the package, follow the code below:
```r 
install.packages("devtools")
library(devtools)
install_github("chelleonis/met_hastings", build_vignettes = TRUE, force = TRUE)
library(mhsim)
```

To build the vignette, 

```r
browseVignettes("mhsim")
```
