
#' @title EM algorithm
#' @description Estimating the parameters of a mixed normal model using R
#' @param x data
#' @param theta0  unknown parameter
#' @return estimated value
#' @examples
#' x<- rnorm(100,mean=2,sd=2)
#' theta0<- c(1,1,2,3,0.5)
#' mixnormal(x,theta0)
#' @export
mixnormal <- function(x,theta0){
  part1 <- (1-theta0[5])*dnorm(x,theta0[1],theta0[3])
  part2 <- theta0[5]*dnorm(x,theta0[2],theta0[4])
  gam <- part1/(part1+part2)
  denom1 <- sum(1-gam)
  denom2 <- sum(gam)
  mu1 <- sum((1-gam)*x)/denom1
  sig1 <- sqrt(sum((1-gam)*((x-mu1)^2))/denom1)
  mu2 <- sum(gam*x)/denom2
  sig2 <- sqrt(sum(gam*((x-mu2)^2))/denom2)
  p <- mean(gam)
  mixnormal <- c(mu1,mu2,sig1,sig2,p)
  mixnormal
}
