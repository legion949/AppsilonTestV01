

# Global options
options(encoding = "utf-8")
options(shiny.maxRequestSize = 500*1024^2)

# Libreries
# library("shiny.semantic")
library("shiny")
library("leaflet")
library("DT")
library("geosphere")
library("lubridate")
library("nnet")
library("leaflet")
library("sp")
library("dplyr")

library(shiny)
library(shinyjs) 
library(shinydashboard) 
library(tableHTML) 

# Functions
source("functions.R")


# Web Page and Comments
web_page <- read.csv(file ="002_data/ShipsPages.csv", sep = ",", dec = ".", header = T,
                     stringsAsFactors = F, encoding = "utf-8")


# Example Details
the_columns <- c("Ship Type", "Order Type", "Ship")
example_details <- matrix(NA, 3, length(the_columns))
colnames(example_details) <- the_columns
example_details[1,] <-  c("Cargo (Cod: 7)", "Ship Name (Id)", "THURKUS (122562)")
example_details[2,] <-  c("Tug (Cod: 3)", "Ship Name (Id)", "SAR 111261105 (73528)")
example_details[3,] <-  c("Unspecified (Cod: 0)", "Ship Name (Id)", "VTS HEL (23788)")

