library(shiny)
library(knitr)
library(ggplot2)
library(dplyr)
library(tidyr)

source("my_ui.R")
source("my_server.R")

shinyApp(ui= ui, server = server)