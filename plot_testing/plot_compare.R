##############################################
# Code author: erin r stearns
# Code objective: testing ggplot vs plotly stand-alone performance
# Date: 4.25.2019
#############################################

rm(list = ls())

######################################################################################################
# -------------------------------------- set up ------------------------------------------------------
######################################################################################################
library(sf)
library(rgdal)
library(plotly)
library(ggplot2)

######################################################################################################
# -------------------------------------- load data ---------------------------------------------------
######################################################################################################
#load spatial data
geodata <- readRDS('app/data/sf_acs5_2007_2017_w2010counties_v.Rds')
#transform spatial data to wgs84
geodata <- st_transform(geodata, crs = 4326)

#load aspatial data
adata <- readRDS('app/data/acs5_2007_2017_fin.Rds')

######################################################################################################
# -------------------------------------- wrangle & set args ------------------------------------------
######################################################################################################
vizdata <- adata
univar <- "median_income"
xvar <- "poverty_fem_rate"
yvar <- "edu_collegeplus"
colorvar <- "perc_black"

######################################################################################################
# -------------------------------------- ggplot ------------------------------------------------------
######################################################################################################

#univariate density
gguni <- ggplot((vizdata), aes_string(univar)) +
            geom_density(alpha = 0.2, fill = "blue") +
            geom_density(fill = "red") +
            theme(legend.position = "none")


#bivariate scatter
ggbiv <- ggplot((vizdata),aes_string((xvar), (yvar), colour = (colorvar))) +
            geom_point(alpha = 0.5, size = 1.5) +
            geom_line(stat = "smooth", method = "lm", alpha = 0.3, colour = "red") +
            scale_colour_distiller(palette = "YlOrRd", direction = 1) +
            theme(legend.position = "none")
