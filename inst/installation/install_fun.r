# This function installs all necessary R packages and performs some simple tests to see
# if packages can be loaded and models can be correctly submitted
install_fun <- function(pkg=TRUE,test=TRUE){
  sect    <- function(txt) paste(paste0(rep("-",40),collapse = ""),txt,paste0(rep("-",40),collapse = ""))
  message("------------- Start installation function (Make sure all R sessions are closed!)...\n")
  if(pkg){
    message("------------- Installing all necessary R packages (this could take a while, please ignore warnings here)...\n")
    # Install regular pacakges first, then delete all possible nlmixr2 related packages (and reinstall clean version 3.0.0)
    ins1 <- try(install.packages('devtools', dependencies = TRUE,repos='https://cloud.r-project.org', quiet=TRUE), silent=TRUE)
    ins2 <- try(install.packages('xpose.nlmixr2', dependencies = FALSE, repos='https://cloud.r-project.org', quiet=TRUE), silent=TRUE)
    ins3 <- try(devtools::install_github('richardhooijmaijers/shinyMixR', repos='https://cloud.r-project.org', quiet=TRUE), silent=TRUE)
    
    rempck <- c("nlmixr2","lbfgsb3c","n1qn1c","cpp11armadillo","lotri","PreciseSums","dparser-R","rxode2","nlmixr2est","dparser")
    try(suppressMessages(remove.packages(rempck)),silent = TRUE)
    
    ins4 <- try(devtools::install_version("nlmixr2est", version = "3.0.0", repos = "https://cloud.r-project.org", upgrade="always", quiet = TRUE), silent=TRUE)
    ins5 <- try(devtools::install_version("rxode2", version = "3.0.0", repos = "https://cloud.r-project.org", upgrade="always", quiet = TRUE), silent=TRUE)
    ins6 <- try(devtools::install_version("nlmixr2", version = "3.0.0", repos = "https://cloud.r-project.org", upgrade="never", quiet = TRUE), silent=TRUE)
    ins7 <- try(install.packages('n1qn1c', dependencies = FALSE, repos='https://cloud.r-project.org', quiet=TRUE), silent=TRUE)
    ins8 <- try(install.packages('dparser', dependencies = FALSE,repos='https://cloud.r-project.org', quiet=TRUE), silent=TRUE)
    
    pkgres <- c(sect("R package installation - devtools"),ins1, sect("R package installation - xpose.nlmixr2"), ins2,
                sect("R package installation - shinyMixR"),ins3, sect("R package installation - nlmixr2est"),ins4,
                sect("R package installation - rxode2"),ins5, sect("R package installation - nlmixr2"),ins6,
                sect("R package installation - n1qn1c"),ins7, sect("R package installation - dparser"),ins8)
  }
  if(test){
    message("------------- Testing the installation (this could take a while)...\n")
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
      #print(list.files(recursive=TRUE))
      #print(readLines(paste0(tempdir(),"/shinyMixR/temp/run1.prog.txt")))
      setwd(owd)  
      tst3 <- chk
    },silent=TRUE)
    if(!exists("tst3") || is.null(tst3) || length(tst3)==0) tst3 <- "failed"
    tstres <- c(sect("Loading libraries - nlmixr2"),tst1,sect("Loading libraries - shinyMixR"),tst2,
                sect("Running models"),tst3)
  }
  allres <- paste("------------- Test resulst for",Sys.info()['user'],"(please ignore warnings below)\n")
  if("pkgres"%in%ls())  allres <- c(allres,pkgres)
  if("tstres"%in%ls())  allres <- c(allres,tstres)
  allres <- c(allres,"\n\n",capture.output(sessionInfo()))
  writeLines(allres,paste0(path.expand('~'),"/InstallationResults.log"))
  
  message(paste0("------------- Done! Results for the test can be found here: ",paste0(path.expand('~'),"/InstallationResults.log"),
                 "\nPlease mail this file to r.hooijmaijers@lapp.nl (please ignore warnings below) \n\n\n\n\n"))
  
}