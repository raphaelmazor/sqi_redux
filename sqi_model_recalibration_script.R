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
my.sites <- unique(mydf.t[, c('comid', 'bio_fp')])
# Marcus did by MasterID but I'm switching to comid
sites.cal <- sample_frac(my.sites, 0.75, replace = F) %>% 
  group_by('bio_fp')
mydf.t <- mydf.t %>% 
  mutate(SiteSet = ifelse(comid %in% sites.cal$comid, 'Cal', 'Val')) %>% 
  select(comid, yr, SiteSet)
sqidat <- sqidat %>% 
  left_join(mydf.t, by = c('comid', 'yr', 'bio_fp'))

# separate cal, val data
caldat <- sqidat %>% 
  filter(SiteSet %in% 'Cal')
valdat <- sqidat %>% 
  filter(SiteSet %in% 'Val')

# models, glm
# pChem
wqglm <- glm(bio_fp ~ log10(0.1 + tn) + log10(0.01 + tp) + Cond,
  family = binomial('logit'), data = caldat)
wqglm <- step(wqglm)

# pHab
vif_func(caldat[, c('bs', 'hy', 'ps', 'ev_flowhab', 'h_aqhab', 'h_subnat', 'pct_safn', 'xcmg')], thresh = 3)
habglm <- glm(bio_fp ~ hy + ps + ev_flowhab + h_aqhab + pct_safn + xcmg, 
  family = binomial('logit'), data = caldat)
habglm <- step(habglm)

# save to package 
save(wqglm, file = '../SQI/data/wqglm.RData', compress = 'xz')
save(habglm, file = '../SQI/data/habglm.RData', compress = 'xz')

# get SQI model results from combined data ----------------------------------

# Not entirely sure what this is doing since it's pulling from the SQI package.
sqidat2 <- sqidat %>% 
  rename(
    CSCI = csci, 
    ASCI = d_asci
  ) %>% 
  sqi %>% 
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = prj) 

# add predicted pChem and pHab values to sqidat
sqidat <- sqidat %>%
  mutate(pChem = predict(wqglm, data.frame(
      bio_fp = sqidat$bio_fp, 
      tn = sqidat$tn, 
      tp = sqidat$tp,
      Cond = sqidat$Cond)),
    pHab = predict(habglm, data.frame(
      hy = sqidat$hy, 
      pct_safn = sqidat$pct_safn, 
      xcmg = sqidat$xcmg)))

# End of script.