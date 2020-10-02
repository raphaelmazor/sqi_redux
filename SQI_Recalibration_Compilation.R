# SQI Recalibration Script re: Data Compilation
# Creator: Heili Lowman
# Date: October 2nd

# The following script

#### Packages ####

# Attach necessary packages.
library(tidyverse)
library(lubridate)

#### Data Import ####

# Load in necessary SMC datasets.
# Sites crosswalk
lu_stations_prob <- read_csv("lu_stations_prob.csv")
# Nutrients (N/P)
tbl_chem <- read_csv("tbl_chemistryresults_NP.csv")
legacy_tbl_chem <- read_csv("legacy_tbl_chemistryresults_NP.csv")
# Conductivity 
tbl_chem_cond <- read_csv("tbl_chemistryresults_cond.csv")
legacy_tbl_chem_cond <- read_csv("legacy_tbl_chemistryresults_cond.csv")
tbl_phab_cond <- read_csv("tbl_phab_cond.csv")

#### Tidy Data  ####

# See other scripts in this project to learn more about dataset origins and reasoning for below tidying steps.

# Nutrients (N/P)

chem_clean <- tbl_chem %>% # Takes the original dataset.
  filter(!stationcode %in% c("000NONPJ", "LABQA", "FBLANK")) %>% # Removes non-sample values.
  filter(sampletypecode == "Grab") %>% # Includes grab samples only.
  filter(matrixname == "samplewater") %>% # Includes samples of water only.
  filter(fieldreplicate == 1) %>% # Includes only first field replicates.
  filter(labreplicate == 1) # Includes only first lab replicates.

chem_clean_legacy <- legacy_tbl_chem %>% # Same actions as above.
  filter(!stationcode %in% c("000NONPJ", "LABQA", "FBLANK")) %>% 
  filter(sampletypecode == "Grab") %>% 
  filter(matrixname == "samplewater") %>%
  filter(fieldreplicate == 1) %>% 
  filter(labreplicate == 1)

# Conductivity

cond_chem_clean <- tbl_chem_cond %>% # Takes the original dataset.
  filter(analytename == "SpecificConductivity" | analytename == "ElectricalConductivity") %>% # Includes only conductivity measures.
  filter(!stationcode %in% c("000NONPJ", "LABQA", "FBLANK")) %>% # Removes non-sample values.
  filter(sampletypecode == "Grab") %>% # Includes grab samples only.
  filter(matrixname == "samplewater") %>% # Includes samples of water only.
  filter(fieldreplicate == 1) %>% # Includes only first field replicates.
  filter(labreplicate == 1) # Includes only first lab replicates.

cond_chem_clean_legacy <- legacy_tbl_chem_cond %>% # Same actions as above.
  filter(analytename == "SpecificConductivity" | analytename == "ElectricalConductivity") %>%
  filter(!stationcode %in% c("000NONPJ", "LABQA", "FBLANK")) %>% 
  filter(sampletypecode == "Grab") %>% 
  filter(matrixname == "samplewater") %>% 
  filter(fieldreplicate == 1) %>% 
  filter(labreplicate == 1) 

cond_phab_clean <- tbl_phab_cond %>% # Same actions as above.
  filter(analytename == "SpecificConductivity" | analytename == "ElectricalConductivity") %>% 
  filter(!stationcode %in% c("000NONPJ", "LABQA", "FBLANK")) %>% 
  filter(matrixname == "samplewater") %>% 
  filter(fieldreplicate == 1) 

#### Binding Data by Parameter  ####

# Nutrients (N/P)

# Pivot_wider the tidied dataset.
chem_clean2 <- chem_clean %>%
  select(stationcode, sampledate, analytename, result) %>%
  pivot_wider(names_from = analytename, values_from = result) %>% # generating new columns by analyte
  relocate("Phosphorus as P", .after = last_col()) %>%
  relocate("OrthoPhosphate as P", .after = last_col()) %>%
  mutate(Date = as.Date(sampledate)) %>%
  mutate(Year = year(Date))

# Join with lu_stations.
chem_clean_match <- inner_join(chem_clean2, lu_stations_prob,
                                by = c("stationcode" = "stationid")) # join data + sites (133)

# Create a new column to join with the remaining data.
chem_clean_match2 <- chem_clean_match %>%
  mutate(masterid_Year = paste(masterid, Year, sep="_"))

chem_clean_anti <- anti_join(chem_clean2, lu_stations_prob,
                                by = c("stationcode" = "stationid")) # listing of dropped sites (8)  

# pivot
chem_clean_legacy2 <- chem_clean_legacy %>%
  select(stationcode, sampledate, analytename, result) %>%
  pivot_wider(names_from = analytename, values_from = result, values_fn = mean) %>% # summarizing the handful of duplicates
  relocate("Phosphorus as P", .after = last_col()) %>%
  relocate("OrthoPhosphate as P", .after = last_col()) %>%
  mutate(Date = as.Date(sampledate)) %>%
  mutate(Year = year(Date))

# join
chem_clean_legacy_match <- inner_join(chem_clean_legacy2, lu_stations_prob,
                                        by = c("stationcode" = "stationid")) # join data + sites (661)

# new column
chem_clean_legacy_match2 <- chem_clean_legacy_match %>%
  mutate(masterid_Year = paste(masterid, Year, sep="_"))

chem_clean_legacy_anti <- anti_join(chem_clean_legacy2, lu_stations_prob,
                                      by = c("stationcode" = "stationid")) # listing of dropped sites (131)

# Combine datasets by masterid_Year.
chem_full <- full_join(chem_clean_match2, chem_clean_legacy_match2) # full nutrients dataset (794)

# Conductivity

# Pivot wider.
cond_chem_clean2 <- cond_chem_clean %>%
  select(stationcode, sampledate, analytename, result) %>%
  pivot_wider(names_from = analytename, values_from = result) %>% # generating new columns by analyte
  mutate(Date = as.Date(sampledate)) %>%
  mutate(Year = year(Date))

cond_chem_clean_legacy2 <- cond_chem_clean_legacy %>%
  select(stationcode, sampledate, analytename, result) %>%
  pivot_wider(names_from = analytename, values_from = result) %>% # generating new columns by analyte
  mutate(Date = as.Date(sampledate)) %>%
  mutate(Year = year(Date))

cond_phab_clean2 <- cond_phab_clean %>%
  select(stationcode, sampledate, analytename, result) %>%
  pivot_wider(names_from = analytename, values_from = result) %>% # generating new columns by analyte
  mutate(Date = as.Date(sampledate)) %>%
  mutate(Year = year(Date))

# Join with lu_stations & create masterid_Year column.
cond_chem_clean_match <- inner_join(cond_chem_clean2, lu_stations_prob,
  by = c("stationcode" = "stationid")) %>% # join data + sites (11, none dropped)
  mutate(masterid_Year = paste(masterid, Year, sep="_"))

cond_chem_clean_legacy_match <- inner_join(cond_chem_clean_legacy2, lu_stations_prob,
  by = c("stationcode" = "stationid")) %>% # join data + sites (137, 12 dropped)
  mutate(masterid_Year = paste(masterid, Year, sep="_"))

cond_phab_clean_match <- inner_join(cond_phab_clean2, lu_stations_prob,
  by = c("stationcode" = "stationid")) %>% # join data + sites (737, 143 dropped)
  mutate(masterid_Year = paste(masterid, Year, sep="_"))

# Join datasets.
cond_part <- full_join(cond_chem_clean_match, cond_chem_clean_legacy_match) # partial cond dataset (148)

cond_full <- full_join(cond_part, cond_phab_clean_match) # full cond dataset (885)

#### Compiling All Datasets ####

# Joining chemistry datasets (nutrients, 794 records + conductivity, 885 records = 1,126 records in total).
chemistry_full <- full_join(chem_full, cond_full)

# Making a slightly condensed dataset that's a little easier to look at.
chemistry_full_trim <- chemistry_full %>%
  select("masterid", "sampledate", "masterid_Year", "Nitrogen,Total", "Nitrate + Nitrite as N", "Nitrogen, Total Kjeldahl", "Ammonia as N", "Nitrate as N", "Nitrite as N", "Nitrate as N03", "Nitrogen-Organic", "Phosphorus as P", "OrthoPhosphate as P", "SpecificConductivity", "ElectricalConductivity")

# 245/1126 records missing conductivity. 353/1126 records missing phosphorus as P (good proxy for nutrients in general).

# End of script.