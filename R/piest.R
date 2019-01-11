
#' @title Estimate the value of pi
#' @description The package is used for estimating pi.
#' @param n the number of simulations
#' @return value of estimate and it's error
#' @examples
#' piest(10^6)
#' @export
piest<- function(n){
  #
  # Obtains the estimate of pi and its standard
  #
  # n is the number of simulations
  samp<- 4*sqrt(1-runif(n)^2)
  est<- mean(samp)
  se<- sqrt(var(samp)/n)
  list(est=est,se=se)
}

