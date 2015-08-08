#' Arbitrary moment
#'
#' This function returns order-th weighted sample moment.
#' @param x is a numeric vector.
#' @param p is a numeric vector of weight of x elements
#' @param order is order of moment.
#' @param center is logical; when TRUE, moment is around mean and
#' when FALSE, around zero
#' @keywords moment
#' @export
#' @examples
#' n <- 10
#' x <- runif(n)
#' p <- runif(n)
#' out <- ry_momentProb(x,p,oder=2,center=TRUE)
#' mob.fx <- ry_moebius(zs,ws)
#' print(mob.fx(zs))
#' print(ws)
ry_momentProb<-function (x, p,order = 1, center = FALSE) 
{
 if(center)
  x <- x - momentProb(x,p,order=1,center=FALSE)
 sum(x^order*p)
}


#'
#' This function returns order-th sample moment.
#' @param x is a numeric vector.
#' @param order is order of moment.
#' @param center is logical; when TRUE, moment is around mean
#' and when FALSE, around zero
#' @keywords complex function, conformal transformation
#' @export
#' @examples
#' n <- 10
#' x <- runif(n)
#' out2 <- ry_momentX(x,order=2,center=TRUE)


momentX<-function (x, order = 1, center = FALSE) 
{
 momentProb(x,p=rep(1,length(x))/length(x),order=order,center=center) 
}