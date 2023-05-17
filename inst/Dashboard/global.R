# Loading libraries
library(shiny)     # Used as it is a shiny app
library(bs4Dash)   # Used instead of shinydashboard to provide additional options
library(fresh)     # Used to easily change css in bs4Dash
library(ggplot2)   # Used for plotting
library(shinyMixR) # Used for everything else

# Check and load nlmixr(2)
if("nlmixr2" %in% rownames(installed.packages())){
  library(nlmixr2)
}else if("nlmixr" %in% rownames(installed.packages())){
  library(nlmixr)
}else{
  cat("you need either the 'nlmixr' or 'nlmixr2' package to run models\n")
}

# Create theme for dashboard
newtheme <- create_theme(
  theme = "darkly", # theme has no effect, at least within bs4Dash
  bs4dash_font(size_base = "0.9rem"),
  bs4dash_status(primary = "#3c8dbc")
)