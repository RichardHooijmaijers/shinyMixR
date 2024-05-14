# This function installs all necessary R packages and performs some simple tests to see
# if packages can be loaded and models can be correctly submitted
install_fun <- function(pkg=TRUE,test=TRUE){
  sect    <- function(txt) paste(paste0(rep("-",40),collapse = ""),txt,paste0(rep("-",40),collapse = ""))
  cat("Start installation function...\n")
  if(pkg){
    cat("Installing all necessary R packages (this could take a while)...\n")
    ins1   <- system2("Rscript", "-e \"install.packages('nlmixr2',quiet=TRUE, dependencies = TRUE,repos='https://cloud.r-project.org')\"", stdout=TRUE, stderr=TRUE)
    ins2   <- system2("Rscript", "-e \"install.packages('xpose.nlmixr2',quiet=TRUE, dependencies = TRUE,repos='https://cloud.r-project.org')\"", stdout=TRUE, stderr=TRUE)
    ins3   <- system2("Rscript", "-e \"devtools::install_github('richardhooijmaijers/shinyMixR', repos='https://cloud.r-project.org')\"", stdout=TRUE, stderr=TRUE)
    ins4   <- system2("Rscript", "-e \"install.packages('GGally',quiet=TRUE, dependencies = TRUE,repos='https://cloud.r-project.org')\"", stdout=TRUE, stderr=TRUE)
    pkgres <- c(sect("R package installation - nlmixr2"),ins1,sect("R package installation - xpose.nlmixr2"),ins2,
                sect("R package installation - shinyMixR"),ins3,sect("R package installation - other"),ins4)
  }
  if(test){
    cat("Testing the installation (this could take a while)...\n")
    tst1 <- suppressWarnings(system2("Rscript", "-e \"library(nlmixr2)\"", stdout=TRUE, stderr=TRUE))
    tst2 <- suppressWarnings(system2("Rscript", "-e \"library(shinyMixR)\"", stdout=TRUE, stderr=TRUE))
    try({
      library(shinyMixR)
      library(nlmixr2)
      owd <- getwd()
      setwd(tempdir())
      create_proj()
      run_nmx("run1",get_proj()) 
      # wait max 5 minutes obtain the results (start reading after 5 seconds)
      Sys.sleep(5)
      for(i in 1:300){
        chk <- try(readLines(paste0(tempdir(),"/shinyMixR/temp/run1.prog.txt")),silent=TRUE)
        if("try-error"%in%class(chk)) break
        if(any(grepl("run finished!",chk))) break
        Sys.sleep(1)
      }
      setwd(owd)  
      tst3 <- chk
    },silent=TRUE)
    if(is.null(tst3) || length(tst3)==0) tst3 <- "failed"
    tstres <- c(sect("Loading libraries - nlmixr2"),tst1,sect("Loading libraries - shinyMixR"),tst2,
                sect("Running models"),tst3)
  }
  allres <- paste("Test resulst for",Sys.info()['user'],"\n")
  if("pkgres"%in%ls())  allres <- c(allres,pkgres)
  if("tstres"%in%ls())  allres <- c(allres,tstres)
  allres <- c(allres,"\n\n",capture.output(sessionInfo()))
  writeLines(allres,paste0(path.expand('~'),"/InstallationResults.log"))
  
  cat(paste0("Done! Results for the test can be found here: ",paste0(path.expand('~'),"/InstallationResults.log"),
             "\nPlease mail this file to r.hooijmaijers@lapp.nl\n"))
  
}
