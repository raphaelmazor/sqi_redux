# SQI model recalibration script
# January 5, 2021
# Heili Lowman

# All of the below code has been taken from Marcus' original script found here: https://github.com/SCCWRP/SQI_Doc/blob/master/R/dat_proc.R

# Load packages
library(tidyverse)
library(sf)
library(randomForest)

# create sqi mods ---------------------------------------------------------

# Load in dataset from "sqi_dataset_creation_script.R".
# Adding in a mock conductivity column for now since it's not yet in there.
sqidat <- sqidat %>%
  mutate(Cond = 1707)

# get calibration/validation datasets
set.seed(500)

mydf.t<- sqidat %>% group_by(bio_fp)
# want both passing and failing sites in calibration and validation datasets

my.sites <- unique(mydf.t[, c('masterid', 'bio_fp')])

sites.cal <- sample_frac(my.sites, 0.75, replace = F) %>% 
  group_by('bio_fp')

mydf.t <- mydf.t %>% 
  mutate(SiteSet = ifelse(masterid %in% sites.cal$masterid, 'Cal', 'Val')) %>% 
  select(masterid, yr, SiteSet)

sqidat <- sqidat %>% 
  left_join(mydf.t, by = c('masterid', 'yr', 'bio_fp'))

# separate cal, val data
caldat <- sqidat %>% 
  filter(SiteSet %in% 'Cal')

valdat <- sqidat %>% 
  filter(SiteSet %in% 'Val')

# models, glm
# the following models use the same variables as those in Beck et al., 2019
# bio_fp is pass/fail
# 'logit' makes it logistic regression

# pChem
wqglm <- glm(bio_fp ~ log10(0.1 + tn) + log10(0.01 + tp) + Cond,
  family = binomial('logit'), data = caldat)

# pHab
habglm <- glm(bio_fp ~ hy + pct_safn + xcmg, 
  family = binomial('logit'), data = caldat)

# get SQI model results from combined data ----------------------------------

# add predicted pChem and pHab values to sqidat
sqidat2 <- sqidat %>%
  mutate(pChem = predict(wqglm, newdata = sqidat, type = "response"),
    pHab = predict(habglm, newdata = sqidat, type = "response"))

# End of script.