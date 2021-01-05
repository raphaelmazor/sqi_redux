# SQI model recalibration script
# January 5, 2021
# Heili Lowman

# All of the below code has been taken from Marcus' original script found here: https://github.com/SCCWRP/SQI_Doc/blob/master/R/dat_proc.R

# create sqi mods ---------------------------------------------------------

data(sqidatinp)

# lookup table of bio BCG class, corresponding score, and combined categorical score
xwalk <- read.csv('raw/scoring_xwalkrc.csv', stringsAsFactors = F)

# create bio categories for fail/pass combos
# for BCG, CSCI 2, 3, 4, 5, 6 is 1.03, 0.83, 0.63, 0.33, ASCI 2, 3, 4, 5, 6 is 1.23, 0.97, 0.67, 0.3
# for reference dist thresholds, CSCI li, pa, la, vla is 0.92, 0.79, 0.63, ASCI li, pa, la, vla is 0.93, 0.83, 0.7 
sqidat <- sqidatinp %>%  
  mutate(
    CSCI_rc = cut(csci_mean, breaks = c(-Inf, 0.63, 0.79, 0.92, Inf), labels = c('vla', 'la', 'pa', 'li')), 
    CSCI_rc = as.character(CSCI_rc),
    ASCI_rc = cut(asci_mean, breaks = c(-Inf, 0.70, 0.83, 0.93, Inf), labels = c('vla', 'la', 'pa', 'li')),
    ASCI_rc = as.character(ASCI_rc)
  ) %>% 
  left_join(xwalk, by = c('CSCI_rc', 'ASCI_rc')) %>% 
  select(-CSCI_score, -ASCI_score) %>%
  mutate(
    bio_fp = ifelse(Bio_BPJ < 0, 1, 0)
  ) %>% 
  ungroup

# get calibration/validation datasets
set.seed(500)
mydf.t<- sqidat %>% group_by(bio_fp)
my.sites <- unique(mydf.t[, c('MasterID', 'bio_fp')])
sites.cal <- sample_frac(my.sites, 0.75, replace = F) %>% 
  group_by('bio_fp')
mydf.t <- mydf.t %>% 
  mutate(
    SiteSet = ifelse(MasterID %in% sites.cal$MasterID, 'Cal', 'Val')
  ) %>% 
  select(MasterID, yr, SiteSet)
sqidat <- sqidat %>% 
  left_join(mydf.t, by = c('MasterID', 'yr', 'bio_fp'))

# separate cal, val data
caldat <- sqidat %>% 
  filter(SiteSet %in% 'Cal')
valdat <- sqidat %>% 
  filter(SiteSet %in% 'Val')

# models, glm
# tmp <- caldat %>% 
#   mutate(
#     TN = log10(0.1 + TN), 
#     TP = log10(0.01 + TP)
#     )
# vif_func(tmp[, c('TN', 'TP', 'Cond')], thresh = 3)
wqglm <- glm(bio_fp ~ log10(0.1 + TN) + log10(0.01 + TP) + Cond,
  family = binomial('logit'), data = caldat)
wqglm <- step(wqglm)
vif_func(caldat[, c('bs', 'hy', 'ps', 'Ev_FlowHab', 'H_AqHab', 'H_SubNat', 'PCT_SAFN', 'XCMG')], thresh = 3)
habglm <- glm(bio_fp ~ hy + ps + Ev_FlowHab + H_AqHab + PCT_SAFN + XCMG, family = binomial('logit'), data = caldat)
habglm <- step(habglm)

# save to package 
save(wqglm, file = '../SQI/data/wqglm.RData', compress = 'xz')
save(habglm, file = '../SQI/data/habglm.RData', compress = 'xz')

# sample data for package
sampdat <- sqidat %>%
  select(MasterID, yr, csci_mean, asci_mean, IPI, PCT_SAFN, H_AqHab, H_SubNat, Ev_FlowHab, XCMG, IPI, blc, bs, hy, ps, indexscore_cram, Cond, TN, TP, SiteSet) %>%
  rename(
    CSCI = csci_mean,
    ASCI = asci_mean
  )

save(sampdat, file = '../SQI/data/sampdat.RData', compress = 'xz')

# get SQI model results from combined data ----------------------------------

sqidat <- sqidat %>% 
  rename(
    CSCI = csci_mean, 
    ASCI = asci_mean
  ) %>% 
  sqi %>% 
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = prj) 

save(sqidat, file = 'data/sqidat.RData', compress = 'xz')
save(sqidat, file = '../SQI_shiny/data/sqidat.RData', compress = 'xz')