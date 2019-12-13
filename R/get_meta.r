#------------------------------------------ get_meta ------------------------------------------
#' Get the meta data out of a model function
#'
#' This function gets only the meta data from a function
#'
#' @param mdl character with the path of the model function
#'
#' @export
#' @examples
#'
#' \dontrun{
#'   get_meta("run1.r")
#' }
get_meta <- function(mdl){
  # get the body of the model function as character
  mdlt <- readLines(mdl)
  mdlt <- eval(parse(text=readLines(mdl)))
  mdlt <- lapply(body(mdlt),as.character)

  # Get the meta data out of the character (first entry is assignment, second is lhs, third is rhs)
  metd <- lapply(mdlt,function(x){if(grepl("=|<-",x[1]) && grepl("data|desc|ref|imp|est|control",x[2])) return(x)})
  metd[sapply(metd, is.null)] <- NULL
  names(metd) <- sapply(metd,"[[",2)

  lapply(metd,function(x){
    if(x[2]=="imp") {
      as.numeric(x[3])
    }else if(x[2]=="control"){
      eval(parse(text=x[3]))
    }else{
      x[3]
    }
  })
}
