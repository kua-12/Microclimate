### does the effect vary with season and MAP?

rm(list = ls())
#script to analyze the forest structure data from the moisture microclimate project

library(readr)
library(geodata)
library(terra)
library(climatol)
library(tidyverse)
library(lme4)
library(lmerTest)


dat <- read_csv("updated_dataset_july2023_withRR.csv")

#step 1: get MAP and MAT data for all sites. Some sites reported it, for those that didn't, get from worldclim
prec_monthly <- worldclim_global(var = 'prec', path = tempdir(), res = 10)

prec_total <- sum(prec_monthly)

temp_monthly <- worldclim_global(var = 'tavg', path = tempdir(), res = 10)
temp_avg <- mean(temp_monthly)

head(dat)

## need a function to loop over the map values, if NA, get the values from worldclim to fill in

getPrecFn <- function(row){
  if(is.na(dat[row,'map_mm'])){
    out <- terra::extract(prec_total, matrix(c(dat$Long_Coordinates[row], dat$Lat_Coordinates[row]), ncol=2))
  } else {
    out <- as.numeric(dat[row,'map_mm'])
  }
  return(out)
}

getTempFn <- function(row){
  if(is.na(dat[row,'mat_deg_c'])){
    out <- terra::extract(temp_avg, matrix(c(dat$Long_Coordinates[row], dat$Lat_Coordinates[row]), ncol=2))
  } else {
    out <- as.numeric(dat[row,'mat_deg_c'])
  }
  return(out)
}


prec_complete <- sapply(1:nrow(dat), getPrecFn)
dat$prec_complete <- do.call(c, prec_complete)

temp_complete <- sapply(1:nrow(dat), getTempFn)
dat$temp_complete <- do.call(c, temp_complete)

plot(dat$rr ~ dat$prec_complete)
plot(dat$rr ~ dat$temp_complete)

climMod <- lmer(rr ~ prec_complete + temp_complete + (1|unique_id), dat= dat)
summary(climMod)

###seems like average macroclimate has no effect... 

##next check whether season has an effect. Growing season/not, as well as monthly precip vs avg monthly precip


