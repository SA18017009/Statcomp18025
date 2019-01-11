
#' @title bootstrap test
#' @description test the mean  using bootstrap
#' @param x data
#' @param theta0 null value of the mean
#' @param b  the number of bootstrap resamples
#' @return origtest pvalue teststatall
#' @examples
#' bootstrap(rnorm(200,mean=0.9,sd=6),0.6,50)
#' @export
bootstrap<- function(x,theta0,b){
  #
  # x=sample
  # theta0 is the null value of the mean
  # b is the number of bootstrap resamples
  #
  # origtest contains the value of the test statistics
  #    for original sample
  # pvalue is the bootstrap p-value
  # teststatall contains the b bootstrap tests
  #
  n<- length(x)
  v<- mean(x)
  z<- x-median(x)+theta0
  counter<- 0
  teststatall<- rep(0,b)
  for(i in 1:b){xstar<- sample(z,n,replace=T)
  vstar<- mean(xstar)
  if(vstar>=v){counter<- counter+1}
  teststatall[i]<- vstar}
  pvalue<- counter/b
  list(origtest=v,pvalue=pvalue,teststatall=teststatall)}
