#------------------------------------------ adpt_meta ------------------------------------------
#' Adapt meta information inside a nlmixr UIF
#'
#' regular expressions are used to search for meta data inside a model
#' file. This meta data is then updated with the provided new values
#'
#' @param mdl character with the name of the model to adapt
#' @param newvals list with characteristics/meta data to adapt
#'
#' @export
#' @return character vector with model including the adapted meta data
#' @author Richard Hooijmaijers
#' @examples
#'
#' \dontrun{
#'   adpt_meta("model.r",newvals=list(imp=4,ref="run 1"))
#' }
adpt_meta <- function(mdl,newvals){
  fenv <- environment()
  mdlt <- readLines(mdl)
  lapply(1:length(newvals),function(x){
    taga <- ifelse(names(newvals)[x]%in%c("desc","data","ref","est"),"[\"|'].*[\"|']","[[:digit:]]")
    sstr <- paste0(names(newvals)[x],".*",taga)
    gstr <- regexpr(sstr,mdlt)
    # only adapt first occurence [1]
    gstr <- c(which(gstr>1)[1],gstr[which(gstr>1)][1],attr(gstr,'match.length')[which(gstr>1)[1]])
    if(!any(is.na(gstr))){
      ret <- paste(names(newvals)[x],"=",ifelse(names(newvals)[x]=="imp",newvals[x],paste0("\"",newvals[x],"\"")))
      mdlt[gstr[1]] <- gsub(substr(mdlt[gstr[1]],gstr[2],gstr[3]+gstr[2]-1),ret,mdlt[gstr[1]])
    }
    assign("mdlt",mdlt,envir = fenv)
  })
  return(mdlt)
}
