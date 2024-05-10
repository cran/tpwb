#' The three-parameter Weibull distribution(tpwb)
#'
#' @description Density, distribution function, quantile function, and random generation function
#' for the three-parameter Weibull distribution with \code{shape}, \code{scale} and \code{location}
#'
#' @param x,q vector of quantiles.
#' @param p   vector of probabilities
#' @param n   number of observations. If \code{length(n) > 1}, the length is taken to be the number required.
#' @param shape  shape parameter, where \eqn{\beta > 0}.
#' @param scale  scale parameter, where \eqn{\alpha > 0}.
#' @param location location parameter, where \eqn{\delta \ge 0}.
#' @param log,log.p   logical; (default = \code{FALSE}), if \code{TRUE}, then probabilities are given as \code{log(p)}.
#' @param lower.tail  logical; if \code{TRUE} (default), probabilities are \eqn{P[X \le x]}, otherwise, \eqn{P[X > x]}.
#'
#' @import graphics
#' @import stats
#'
#' @note If location parameter, \eqn{\delta = 0} , it reduced to the two-parameter Weibull distribution.
#'
#'@references Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) Continuous Univariate Distributions, volume 1, chapter 21. Wiley, New York.
#'
#' @return
#' \code{dtpwb} gives the density,
#' \code{ptpwb} gives the distribution function,
#' \code{qtpwb} gives the quantile function,
#' and \code{rtpwb} generates random samples.
#'
#' @name tpwb
#' @examples
#'
NULL

#' @export
#' @rdname tpwb
#' @examples
#' x <- rtpwb(20,1.5,3,1)
#' dtpwb(x,1.5,3,1)
#' dtpwb(x,1.5,3,1,log=TRUE)
#'
dtpwb <- function(x, shape, scale, location=1, log = FALSE){
  beta<- shape; alpha <- scale; delta <- location
  xs <- x[x>=delta]
  fx <- (beta/(alpha^beta))*((xs-delta)^(beta-1))*exp(-((xs-delta)/alpha)^beta)
  if (log==TRUE)
    return (log(fx))
  else
    return(fx)
}

#' @export
#' @rdname tpwb
#' @examples
#' q <- rtpwb(20,1.5,3,1)
#' ptpwb(q,1.5,3,1 )
#' ptpwb(q,1.5,3,1, lower.tail = FALSE)
#'
ptpwb <- function(q, shape, scale,location=1, lower.tail = TRUE, log.p = FALSE){
  beta<- shape; alpha <- scale; delta <- location
  xs <- q[q>=delta]
  cdf <- 1-exp(-((xs-delta)/alpha)^beta)
  if(lower.tail==TRUE)
    p <- cdf
  else
    p <- 1-cdf
  if (log.p==TRUE)
    return (log(p))
  else
    return(p)
}

#' @export
#' @rdname tpwb
#' @examples
#' q <- rtpwb(20,1.5,3,1); q
#' p<- ptpwb(q,1.5,3,1 ); p
#' qtpwb(p,1.5,3,1)
#'
qtpwb <- function(p, shape, scale, location = 1, lower.tail = TRUE, log.p = FALSE){
  beta<- shape; alpha <- scale; delta <- location
  if (log.p==TRUE)
    p <- exp(p)
  if (lower.tail == FALSE)
    p <- 1 - p
  x <- delta + alpha * (-log(1 - p))^(1/beta)
  return(x)
}

#' @export
#' @rdname tpwb
#' @examples
#' rtpwb(5, 1.5, 3, 0) # the same as rweibull(5,1.5,3)
#' rtpwb(25,0.5, 2, 1)
#'
rtpwb <- function(n, shape, scale, location = 1){
  beta<- shape; alpha <- scale; delta <- location
  u<- runif(n)
  if (beta <= 0){
    stop(paste("beta must be larger than 0!", "\n"))
  }
  # x based on the inverse transform method : F(x)=u => x=(inv[F(u)])
  # where u=runif(n,0,1)
  x <- delta + alpha*(-log(1 - u))^(1/beta)
  return(x)
}
