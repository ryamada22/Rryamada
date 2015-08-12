#' hundredR
#'
#' This function is a toy.
#' @keywords hoge
#' @export
#' @examples
#' hundredR()

hundredR <- function(){
  result <- .C("hundred",x=100.0)
	return(result$x)
}
