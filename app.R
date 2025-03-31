
library(vroom)
library(etm)
library(ggplot2)
library(dplyr)
library(colourpicker)
library(forestploter)
library(survival)
library(tidyverse)
library(matrixStats)
library(splines)
library(table1)
library(shinyjs)


source("functions.R") 
source("interface.R")

shinyApp(ui, server)
