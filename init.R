my_packages = c("pacman","crosstalk","aws.s3","readxl","leaflet.extras","reactable","reactablefmtr","shinyjs","leaflegend","leaflet","shinyWidgets","htmltools","janitor","fresh","tidyverse","shiny","DT","shinythemes","bslib","waiter","stringr","paletteer","highcharter","readr","bs4Dash","shinydashboard","shinydashboardPlus","reshape2")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))