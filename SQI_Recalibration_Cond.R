# SQI Recalibration Script re: Conductivity
# Creator: Heili Lowman
# Date: September 14th
# contributor: Jhen Cabasal, Megan Mirkhanian

#### Packages ####

# Attach necessary packages.
library(tidyverse)

#### Data ####

# Load in necessary SMC datasets.
tbl_chem_cond <- read_csv("tbl_chemistryresults_cond.csv")
legacy_tbl_chem_cond <- read_csv("legacy_tbl_chemistryresults_cond.csv")
tbl_phab_cond <- read_csv("tbl_phab_cond.csv")

#### Tidy Data  ####

# The tbl_chem dataset has been pulled from the SMC database and represents data that is submitted directly to SCCWRP by participating partners. The legacy_tbl_chem dataset has also been pulled from the SMC database, but these are values collected prior to the institution of the new data checker/portal, so these were transferred from the old Access database. The tbl_phab dataset has been pulled from the SMC database and represents data that has either been submitted by participating partners or is from the previous database version - either way, it has all been passed through the current checker system prior to being placed in this table.

#There has been no filter imposed on these data except to pull only data using the following identifiers in the analytename column:
# for phab data "SpecificConductivity", "Oxygen, Dissolved", "Temperature", "pH", "Alkalinity as CaCO3", "Turbidity", and "Salinity".
# for chemistryresults data "SpecificConductivity", "ElectricalConductivity", "pH", "Alkalinity as CaCO3", and "Turbidity".

# The following code will trim them down further to generate datasets with actual values of interest - we only want "SpecificConductivity" and "ElectricalConductivity" for this particular review/script.

cond_chem_clean <- tbl_chem_cond %>% # Takes the original dataset.
  filter(analytename == "SpecificConductivity" | analytename == "ElectricalConductivity") %>% # Includes only conductivity measures.
  filter(!stationcode %in% c("000NONPJ", "LABQA", "FBLANK")) %>% # Removes non-sample values.
  filter(sampletypecode == "Grab") %>% # Includes grab samples only.
  filter(matrixname == "samplewater") %>% # Includes samples of water only.
  filter(fieldreplicate == 1) %>% # Includes only first field replicates.
  filter(labreplicate == 1) %>% # Includes only first lab replicates.
  mutate(result_ed = ifelse(result < 0, 0, result)) # Creates a new column named result_ed in which all samples that were effectively reported as negative values instead are reported as 0. These values are not missing, just below the detection limit.

cond_chem_clean_legacy <- legacy_tbl_chem_cond %>% # Same actions as above.
  filter(analytename == "SpecificConductivity" | analytename == "ElectricalConductivity") %>%
  filter(!stationcode %in% c("000NONPJ", "LABQA", "FBLANK")) %>% 
  filter(sampletypecode == "Grab") %>% 
  filter(matrixname == "samplewater") %>% 
  filter(fieldreplicate == 1) %>% 
  filter(labreplicate == 1) %>% 
  mutate(result_ed = ifelse(result < 0, 0, result))

cond_phab_clean <- tbl_phab_cond %>% # Same actions as above.
  filter(analytename == "SpecificConductivity" | analytename == "ElectricalConductivity") %>% 
  filter(!stationcode %in% c("000NONPJ", "LABQA", "FBLANK")) %>% 
  filter(matrixname == "samplewater") %>% 
  filter(fieldreplicate == 1) %>%
  mutate(result_ed = ifelse(result < 0, 0, result))

# How many conductivity records from tbl_chemistryresults? 11
# How many conductivity records from legacy_tbl_chemistryresults? 149
# How many conductivity records from tbl_phab? 880

#### Missingness Exploration ####

# Creating new datasets that explicitly records unique sampling events for conductivity measures.

# tbl_chemistryresults_cond

ccc <- cond_chem_clean %>%
  select(stationcode, sampledate, analytename) %>%
  rename(C1 = analytename)

# legacy_tbl_chemistryresults_cond

cccl <- cond_chem_clean_legacy %>%
  select(stationcode, sampledate, analytename) %>%
  rename(C2 = analytename)

# legacy_tbl_chemistryresults_cond

cpc <- cond_phab_clean %>%
  select(stationcode, sampledate, analytename) %>%
  rename(C3 = analytename)

# Join the above datasets together to examine for duplicates.

cond_all_1 <- full_join(ccc, cccl) # partial join of chemistry data.

cond_all <- full_join(cond_all_1, cpc) # partial join of phab data.

# Only 1 of the 11 records in the ccc dataset is a unique record.
# Only 64 of the 149 records in the cccl dataset is a unique record.
# The remaining 880 of the cpc dataset brings the total # of unique records to 945.

# End of script.