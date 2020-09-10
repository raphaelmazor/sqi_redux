# SQI Recalibration Script re: Chemistry (N/P)
# Creator: Heili Lowman
# Date: September 4th
# contributor: Jhen Cabasal

#### Packages ####

# Attach necessary packages.
library(tidyverse)

#### Data ####

# Load in necessary SMC datasets.
tbl_chem <- read_csv("tbl_chemistryresults_NP.csv")
#swamp_tax <- read_csv("swamp_taxonomysampleinfo.csv")

#### Tidy ####

# The tbl_chem dataset has been pulled from the SMC database and represents data that is submitted directly to SCCWRP by participating partners. There has been no filter imposed on this data except to pull only data using the following identifiers in the analytename column: "Nitrogen,Total", "Nitrate + Nitrite as N", "Nitrogen, Total Kjeldahl", "Ammonia as N", "Nitrate as N", "Nitrite as N", "OrthoPhosphate as P", and "Phosphorus as P".

# The following code will trim it down further to generate a dataset with actual values of interest.

chem_clean <- tbl_chem %>% # Takes the original dataset.
  filter(!stationcode %in% c("000NONPJ", "LABQA", "FBLANK")) %>% # Removes non-sample values.
  filter(sampletypecode == "Grab") %>% # Includes grab samples only.
  filter(matrixname == "samplewater") %>% # Includes samples of water only.
  mutate(result_ed = ifelse(result < 0, 0, result)) # Creates a new column named result_ed in which all samples that were effectively reported as negative values instead are reported as 0. These values are not missing, just below the detection limit.

# Paused here (9/4/2020), because data appears only to cover 2017-2020.

# End of script.