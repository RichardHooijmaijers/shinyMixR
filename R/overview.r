#------------------------------------------ overview ------------------------------------------
#' Creates model overview
#'
#' Create an overview of the models within a project. This overview includes the meta data
#' of the models and if results are available, also the objective function and run-times
#'
#' @param ... additional arguments passed to \code{\link{get_proj}}
#'
#' @export
#' @return a data frame is returned with the overview
#' @author Richard Hooijmaijers
#' @examples
#'
#' \dontrun{
#'  overview()
#' }
overview <- function(...){
  obj  <- get_proj(...)
  mdln <- names(obj)[names(obj)!="meta"]
  res1 <- lapply(mdln, function(x){
    if(class(obj[[x]]$modeleval)=="try-error"){
      c(NA,"","","","")
    }else{
      meta <- obj[[x]]$modeleval$meta
      c(ifelse(is.null(meta$imp),NA,meta$imp), ifelse(is.null(meta$desc),"",meta$desc),ifelse(is.null(meta$ref),"",meta$ref),
        ifelse(is.null(meta$data),"",meta$data),ifelse(is.null(meta$est),"",meta$est))
    }
  })
  res2 <- lapply(obj[mdln], function(x){
    if(!is.null(x$results)) c(round(x$results$OBJF,3),round(x$results$tottime,3)) else c("","")
  })
  res <- data.frame(cbind(mdln,do.call(rbind,res1),do.call(rbind,res2)),stringsAsFactors = FALSE)
  if(nrow(res)!=0){
    names(res) <- c("models","importance","description","ref","data","method","OBJF","runtime")
    #res$dOBJF  <- sapply(res$models,function(x) round(as.numeric(res$OBJF[which(res$models==x)]) - as.numeric(res$OBJF[which(res$models==res$ref[res$models==x])]),4))
    dOBJF      <- lapply(res$models,function(x) round(as.numeric(res$OBJF[which(res$models==x)]) - as.numeric(res$OBJF[which(res$models==res$ref[res$models==x])]),4))
    res$dOBJF  <- sapply(dOBJF,function(x) ifelse(length(x)==0,NA,x))
    res        <- res[,c("models","importance","description","ref","data","method","OBJF","dOBJF","runtime")]
  }
  res
}
