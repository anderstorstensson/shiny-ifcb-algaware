# AlgAware Shiny Application
# This file is the entry point when running via shinyAppDir()

library(algaware)
library(shiny)
library(bslib)
library(DT)

source("ui.R", local = TRUE)
source("server.R", local = TRUE)

shinyApp(ui, server)
