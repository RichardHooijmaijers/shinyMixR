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
#------------------------------------------ getwidg ------------------------------------------
#' get list of widgets
#'
#' This function gets list of widgets to include in run_shinymixr
#' @export
getwidg <- function(){
  w <- c("widget_overview.r","widget_edit.r","widget_run.r","widget_pars.r","widget_gofplot.r","widget_fitplot.r",
       "widget_scripts.r","widget_results.r","widget_settings.r")
  list(widgets=paste0(system.file("widgets",package="shinyMixR"),"/",w),init=paste0(system.file("widgets",package="shinyMixR"),"/widget_init.r"))
}
