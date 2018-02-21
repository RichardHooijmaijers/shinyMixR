library(nlmixr)
options(keep.source = TRUE)
source('/Users/richard/Documents/Rpackages/shinyMixR/V0.1.1/shinyMixR/vignettes/models/run1.r')

modres <- nlmixr(run1,theo_sd,est="saem",control=)
saveRDS(modres,file="./shinyMixR/run1.res.rds")
saveRDS(list(OBJF=modres$objective,partbl=modres$par.fixed,omega=modres$omega,tottime=rowSums(modres$time)),file="./shinyMixR/run1.ressum.rds")
