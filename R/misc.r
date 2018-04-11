#------------------------------------------ sigdigs ------------------------------------------
#' set significant digits without rounding higher numbers
#'
#' This function sets significant digits without rounding any numbers
#' @param x a numerical vector
#' @param sdig a single number defining the number of significant digits
#' @export
sigdigs <- function(x,sdig=3){
  om <- floor(log10(abs(x)))
  dp <- sdig-om-1
  dp <- ifelse(dp<0 | is.na(dp) | is.infinite(dp),0,dp)
  sprintf(paste("%.",dp,"f", sep=""), x)
}
