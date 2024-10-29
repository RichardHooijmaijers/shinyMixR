#------------------------------------------ incr_mdl ------------------------------------------
#' Increments a model name
#'
#' A model name is incremented either by incrementing numerical or alpha numerical.
#' Furthermore it is possible to check the existence of the incremented model
#' and take this into account.
#'
#' @param mod character with the model name
#' @param checkloc character with the location to check for existence of a file
#'
#' @export
#' @return character with the incremented name
#' @author Richard Hooijmaijers
#' @examples
#'
#'  incr_mdl("run01.r")
incr_mdl <- function(mod,checkloc=NULL){

  name_func <- function(mdl=mod){
    mod2 <- substring(mdl,1,tail(gregexpr("\\.",mdl)[[1]],1)-1) # delete extension!
    if(suppressWarnings(is.na(as.numeric(substring(mod2,nchar(mod2)))))){
      # In case of character
      if(toupper(substring(mod2,nchar(mod2)))=="Z"){
        paste0(mod2,"a")
      }else{
        firstp <- substring(mod2,1,nchar(mod2)-1)
        lastp  <- c(letters,LETTERS)[grep(substring(mod2,nchar(mod2)),c(letters,LETTERS)) + 1]
        ret    <- paste0(firstp,lastp)
      }
    }else{
      # In case of numeric
      nfmt <- regexpr("[[:digit:]]*$",mod2)
      num  <- substring(mod2,nfmt[1],attr(nfmt,"match.length")+nfmt[1])
      num  <- formatC(as.numeric(num)+1,width=attr(nfmt,"match.length"),flag="0")
      ret  <- paste0(substring(mod2,1,nfmt[1]-1),num)
    }
    paste0(ret,substring(mdl,tail(gregexpr("\\.",mdl)[[1]],1)))
  }
  # If there is a checkloc the function is repeated until a non existing filename is present (or after 500 repeats)
  if(!is.null(checkloc)){
    ret   <- name_func()
    maxtr <- 500
    tryn  <- 1
    while(file.exists(paste0(checkloc,"/",ret))){
      ret  <- name_func(mdl=ret)
      tryn <- tryn+1
      if(tryn>maxtr) break
    }
    ret
  }else{
     name_func()
  }
}
