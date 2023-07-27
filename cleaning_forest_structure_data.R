rm(list = ls())
#script to analyze the forest structure data from the moisture microclimate project

library(readr)
dat <- read_csv("updated_dataset_july2023_withRR.csv")



##cleaning the structure data:
#1. stem density and stand density are the same, so combine these columns and use same units
#first make sure they're not duplicated
standDens <- !is.na(dat$stand_density)
stemDens <- !is.na(dat$stem_density)
stand_stem_dens <- standDens + stemDens
table(stand_stem_dens)
dat[stand_stem_dens == 2,]

#one study (study 24) is from a mediterrannean forest and has stems/ha and indviduals/ha. I will use the individuals per ha value for that one
dat$stem_density[dat$unique_id == 24] <- NA

#combine stand and stem density
dat <- dat %>% 
  rowwise() %>%
  mutate(forest_density = ifelse(sum(stem_density, stand_density, na.rm = T) > 0, sum(stem_density, stand_density, na.rm = T), NA)) %>%
  ungroup() %>%
  mutate(forest_density_units = 'stems/ha', stem_density = NULL, stand_density = NULL)
  

#2. What's the deal with "forest dbh" as a structure variable?
hasDBH <- dat[!is.na(dat$forest_dbh),]
unique(hasDBH$unique_id)  #go check these studies - what is this variable?

#these seem to be taken from descriptions of study forests.
#for example, study 24 says: Mean canopy height is 50 m, ages are up to 420 years old, and mean DBH ranges from 35 cm (red fir) to 53 cm
#so, I don't think we can use this variable

#3. create a variable that indicates whether or not there's forest structure data, and also what the variable(s) are
#here's a list of all the structure variables
str_vars <- c('canopy_height', 'forest_basal_area', 'canopy_cover', 'LAI', 'forest_density')

dat <- dat %>% 
  mutate(has_structure = rowSums(is.na(dat[,str_vars])) < 5)

#1310 observations out of 2350 have some structure variable


#Going variable by variable, check for correlations with response ratio and the variable

#basal area
basal_area <- dat %>%
  filter(!is.na(forest_basal_area))

#first check/fix the units
basal_area %>% filter(forest_basal_area_units == 'm2')

#study 70 not suitable - BA measure is individual tree.. remove study 70
basal_area <- basal_area %>% filter(unique_id != 70)

plot(basal_area$rr ~ basal_area$forest_basal_area)

basal_area$BA2 <- basal_area$forest_basal_area^2

library(lme4)
library(lmerTest)
BAmod <- lmer(rr ~ forest_basal_area + (1|unique_id), data = basal_area)
summary(BAmod)

##suggests marginally positive effect of BA, but not significant

##stem density
dens <- dat %>% filter(!is.na(forest_density))
dens$stem.dens.z <- (dens$forest_density - mean(dens$forest_density))/sd(dens$forest_density)

densMod <- lmer(rr ~ stem.dens.z + (1|unique_id), data = dens)



#canopy Height

height <-dat %>% filter(!is.na(canopy_height))

hist(height$canopy_height)
unique(height$canopy_height)
##there are only 5 studies and 13 observations with height data... so leave out.

#canopy cover
cover <- dat %>% filter(!is.na(canopy_cover))

hist(cover$canopy_cover)
unique(cover$unique_id)
unique(cover$canopy_cover)
unique(cover$canopy_cover_units)

coverMod <- lmer(rr ~ canopy_cover + (1|unique_id), data = cover)
summary(coverMod)

plot(cover$canopy_cover, cover$rr) ##a few values with really low cover... what's up with these??

#LAI
LAI <- dat %>% filter(!is.na(LAI))

hist(LAI$LAI)
unique(LAI$unique_id)
unique(LAI$LAI)

LAImod <- lmer(rr ~ LAI + (1|unique_id), data = LAI)
summary(LAImod)


