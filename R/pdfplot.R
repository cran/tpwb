#' Probability density function plot of the three-parameter Weibull distribution
#'
#' Probability density function plot of the three-parameter Weibull distribution with specified \code{shape}, \code{scale} and \code{location}.
#'
#' @param x     vector of quantiles
#' @param shape shape parameter (\eqn{\beta}) of the three-parameter Weibull distribution, where \eqn{\beta >0}.
#' @param scale scale parameter (\eqn{\alpha}) of the three-parameter Weibull distribution, where \eqn{\alpha > 0}.
#' @param location location parameter (\eqn{\delta}) of the three-parameter Weibull distribution, where \eqn{\delta \ge 0}.
#'
#' @return Probability density function plot of the three-parameter Weibull distribution.
#' @export
#'
#' @references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, volume 1, chapter 21. Wiley, New York.
#'
#' @examples
#' x <- rtpwb(100,1.5,2,1)
#' pdfplot(x,1.5,2,1)
#'
pdfplot <-function(x,shape,scale,location){
  beta <- shape; alpha <- scale; delta <- location
  x <- x[x>=delta]
  xs <- sort(x)
  fx <- dtpwb(xs,shape = beta, scale = alpha, location = delta )
  plot(x=xs,y=fx,type = "b",xlab="",ylab="")
  title(main = "PDF of the three-parameter Weibull distribution",
        xlab="t or x",ylab="f(t) or f(x)")
  sshape <- beta
  sscale <- alpha
  slocation <- delta
  txtshape <- paste("shape=",sshape)
  txtscale <- paste("scale=",sscale)
  txtlocation <- paste("location=",slocation)
  leg.txt <- c(txtshape,txtscale,txtlocation)
  legend("topright",legend = leg.txt, pch = 1,title = "parameters")
}
