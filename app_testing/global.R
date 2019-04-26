##############################################
# Code author: erin r stearns
# Code objective: us counties geodisparities shiny mock up global script
# Date: 4.25.2019
#############################################


######################################################################################################
# -------------------------------------- set up ---------------------------------------------------- #
######################################################################################################
library(shiny)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(rgdal)
library(sf)
library(ggplot2)
library(ggvis)
library(shinydashboard)
library(dplyr)
library(fontawesome)
require(raster)
require(gstat)

# -------------------------------------- load data ------------------------------------------------- 
#load spatial data
geodata <- readRDS('app/data/sf_acs5_2007_2017_w2010counties_v.Rds')
#transform spatial data to wgs84
geodata <- st_transform(geodata, crs = 4326)

#load aspatial data
adata <- readRDS('app/data/acs5_2007_2017_fin.Rds')

# -------------------------------------- app inputs defined ----------------------------------------
#state choices
state_names <- as.character(unique(adata$state_name))