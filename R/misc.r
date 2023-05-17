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

#------------------------------------------ myalert ------------------------------------------
#' wrapper function for sweetalert in shinywidgets
#'
#' This function gets list of widgets to include in run_shinymixr
#' @export
myalert <- function(text,type,...){
  shinyWidgets::sendSweetAlert(text = text,type = type,
    showClass=list(backdrop='swal2-noanimation',popup=''),width="30%",
    padding="1em",title=NULL,...)
    # ,icon=''
}

#------------------------------------------ theme_shinyMixR ------------------------------------------
#' theme for ggplot output in the shinyMixR package
#'
#' This function provides a custom theme for ggplot output
#' @param fontsize numeric with the default fontsize passed through to theme
#' @export
theme_shinyMixR <- function(fontsize=12){
  ret <- theme_bw(base_size = fontsize) + 
    theme(panel.border     = element_rect(color="grey30", size=0.75),
          panel.grid.major = element_line(color="grey70",size=.25,linetype = "dotted"),
          panel.grid.minor = element_blank(),
          axis.ticks       = element_line(color="grey50"))
  return(ret)
}