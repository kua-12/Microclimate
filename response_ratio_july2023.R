#this is a simple script to calculate the response ratio, add it to the dataset, and fit a simple mixed effects model to calculate the average RR accounting for the multiple observations from studies

library(tidyverse)
library(lme4)
library(readr)

microclim <- read_csv("updated_dataset_july2023.csv")
microclim <- microclim %>% mutate(datapoint.num = 1:nrow(microclim))

microclim_tidy <- microclim %>%
  select(unique_id, datapoint.num, moisture_type_consistent, moisture_units, moisture_open, moisture_forest) %>%
  filter(moisture_units %in% c('cm^3/cm^3', '%', 'm^3/m^3', 'm^3/ m^3', "mm/mm", "g kg -1", 'mm/cm')) %>%
  mutate(rr = log(moisture_forest/moisture_open))

##choose the measurements with "proportion units"

hist(microclim_tidy$rr)
direction <- microclim_tidy$rr > 0 ## 1 = positive, 0 = negative
table(direction)

studyMeans <- microclim_tidy %>% group_by(unique_id) %>% summarise(meanRR = mean(rr, na.rm = T))
direction <- studyMeans$meanRR > 0 ## 1 = positive, 0 = negative
table(direction)

ggplot(studyMeans, aes(x = meanRR)) + 
  geom_histogram(breaks = seq(-2, 2, by = 0.2), fill = 'lightgrey', color = 'darkgrey') + 
  geom_vline(aes(xintercept = 0), linetype = 'dashed') + 
  xlab('log(response ratio)') + 
  theme_classic()

microclim <- microclim %>% left_join(microclim_tidy)

write.csv(microclim, "updated_dataset_july2023_withRR.csv")


### fit a very simple LMER to predict response ratio with a random effect for study
library(lme4)

simp_mod <- with(microclim_tidy, lmer(rr ~ 1|unique_id))
summary(simp_mod)
