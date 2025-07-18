#------------------------------------------ update_inits ------------------------------------------
#' Update initial estimates from a final model run
#'
#' This function update the initial estimates from a model using the final estimates
#' from a model result file. Currently this function assumes all models were submitted
#' using shinyMixR opposed to vanilla nlmixr
#'
#' @param mod character with the entire model function included
#' @param res character with the path to the model result RDS which holds the final estimated
#' @param out character with the path for the updated model to save
#'
#' @export
#' @return nothing will be returned the function saves the updated model to disk
#' @author Richard Hooijmaijers
#' @examples
#'
#' \dontrun{
#'   update_inits(readLines("run2.r"),"shinyMixR/run2.res.rds","run3.r")
#' }
update_inits <- function(mod,res,out){
  eval(parse(text=mod))
  modnm <- ls()[!ls()%in%c("mod","res","out")]
  ores  <- readRDS(res)

  # Get parameters from original model (adapted to use direct interface; easier to obtain IIV as well)
  eomod  <- get(modnm)
  eomod2 <- nlmixr2est::nlmixr2(eomod)
  opar   <- eomod2$iniDf[eomod2$iniDf$fix==FALSE,c("name","est")]
  opar   <- setNames(signif(opar$est,4),opar$name)

  # Get parameters from final estimates
  rpar  <- ores$ui$iniDf
  rpar  <- rpar[rpar$fix==FALSE,c("name","est")]
  rpar  <- setNames(signif(rpar$est,4),rpar$name)

  # Update the model with the inits (perform intersect to only update parameters that allign)
  apar    <- rpar[intersect(names(rpar),names(opar))]
  outt    <- eomod  %>% rxode2::ini(apar)
  outt    <- deparse(outt$fun)
  outt[1] <- paste(tools::file_path_sans_ext(basename(out)),"<-",outt[1])
  writeLines(outt,out)
}
