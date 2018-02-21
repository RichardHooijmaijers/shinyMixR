library(nlmixr)
options(keep.source = TRUE)
source('/Users/richard/Documents/Rpackages/shinyMixR/V0.1.1/shinyMixR/vignettes/models/run2.r')

modres <- nlmixr(run2,theo_sd,est="saem",control=)
saveRDS(modres,file="./shinyMixR/run2.res.rds")
saveRDS(list(OBJF=modres$objective,partbl=modres$par.fixed,omega=modres$omega,tottime=rowSums(modres$time)),file="./shinyMixR/run2.ressum.rds")
