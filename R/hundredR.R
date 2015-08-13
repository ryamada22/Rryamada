#' hundredR
#'
#' This function is a toy.
#' @keywords hoge
#' @export
#' @examples
#' hundredR()
#' @useDynLib Rryamada

hundredR <- function(){
  result <- .C("hundred",package="Rryamada",x=100.0)
	return(result$x)
}
