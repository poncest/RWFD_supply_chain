# tests/testthat/setup.R

library(testthat)
library(shiny)
library(dplyr)
library(here)

# Source server file using here package
source(here("modules", "cost_analysis", "cost_analysis_server.R"))