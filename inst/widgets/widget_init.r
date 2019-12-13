# Currently only add the project object when initializing the app
#cat("time now is",difftime(Sys.time(),strt,units="secs"),"\n")
#cat("starting up took",Sys.time()-strt,"\n")
assign("proj_obj",get_proj(),pos = .GlobalEnv)
#cat("loading proj_obj took",Sys.time()-strt,"\n")
# Clean progress files on startup
try(unlink(list.files(paste0("shinyMixR/temp"),pattern=".*prog\\.txt$",full.names = TRUE)))
try(unlink("shinyMixR/temp/scriptres.out"))
try(dir.create("shinyMixR/temp",recursive=TRUE))
try(file.create("shinyMixR/temp/scriptres.out"))
