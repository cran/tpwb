#' Maximum likelihood estimation (MLE) for the three-parameter Weibull distribution.
#'
#' This function for estimating parameter of the three-parameter Weibull distribution.

#' @param x        vector of quantiles.
#' @param shape    shape parameter, where \eqn{\beta > 0}.
#' @param scale    scale parameter, where \eqn{\alpha > 0}.
#' @param location location parameter, where \eqn{\delta \ge 0}.
#'
#' @return the estimated shape, scale and location values of the three-parameter Weibull distribution.
#' @export
#'
#' @note the result of this function may produce a Warning message, but not effect to the estimated parameter.
#'
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, volume 1, chapter 21. Wiley, New York.
#'
#' @examples
#' x<- rtpwb(1000,2,3,1) #n=1000 large sample
#' mlewb(x,2,3,1)
#' x<- rtpwb(50,2,3,1) #n=50 medium sample
#' mlewb(x,2,3,1)
#' x<- rtpwb(10,2,3,1) #n=10 small sample
#' mlewb(x,2,3,1)
mlewb <- function(x,shape,scale,location){
  negll<- function(par){
    -sum(dtpwb(x,shape=par[1],scale=par[2],location=par[3],log=TRUE))
  }
  objmle<- nlminb(start = c(shape,scale,location),negll)
  mle.shape <- objmle$par[1]
  mle.scale <- objmle$par[2]
  mle.location <- objmle$par[3]
  return(c(mle.shape,mle.scale,mle.location))
}
