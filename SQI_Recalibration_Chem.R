# SQI Recalibration Script re: Chemistry (N/P)
# Creator: Heili Lowman
# Date: September 4th
# contributors: Jhen Cabasal, Megan Mirkhanian, Raphael Mazor
# Another comment. Heili was here.

#### Packages ####

# Attach necessary packages.
library(tidyverse)
library(lubridate)

#### Data Import ####

# Load in necessary SMC datasets.
tbl_chem <- read_csv("tbl_chemistryresults_NP.csv")
legacy_tbl_chem <- read_csv("legacy_tbl_chemistryresults_NP.csv")
#swamp_tax <- read_csv("swamp_taxonomysampleinfo.csv")

#### Tidy Data  ####

# The tbl_chem dataset has been pulled from the SMC database and represents data that is submitted directly to SCCWRP by participating partners. The legacy_tbl_chem dataset has also been pulled from the SMC database, but these are values collected prior to the institution of the new data checker/portal, so these were transferred from the old Access database.

#There has been no filter imposed on these data except to pull only data using the following identifiers in the analytename column: "Nitrogen,Total", "Nitrate + Nitrite as N", "Nitrogen, Total Kjeldahl", "Ammonia as N", "Nitrate as N", "Nitrate as N03", "Nitrite as N", "Nitrogen-Organic", "OrthoPhosphate as P", and "Phosphorus as P".

# The following code will trim them down further to generate datasets with actual values of interest.

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

# counts of N & P records from both datasets

# How many P records? 282
chem_clean_P <- chem_clean %>%
  filter(analytename == "Phosphorus as P" | analytename == "OrthoPhosphate as P") %>%
  filter(fieldreplicate == 1 & labreplicate == 1)

# Legacy? 1344
chem_clean_lP <- chem_clean_legacy %>%
  filter(analytename == "Phosphorus as P" | analytename == "OrthoPhosphate as P") %>%
  filter(fieldreplicate == 1 & labreplicate == 1)

# How many N records? 628
chem_clean_N <- chem_clean %>%
  filter(analytename != "Phosphorus as P" & analytename != "OrthoPhosphate as P") %>%
  filter(fieldreplicate == 1 & labreplicate == 1)

# Legacy? 3482
chem_clean_lN <- chem_clean_legacy %>%
  filter(analytename != "Phosphorus as P" & analytename != "OrthoPhosphate as P") %>%
  filter(fieldreplicate == 1 & labreplicate == 1)

#### Visualize ####

# Rough bar plots to see how the data is delineated by analyte.
chem_clean_plot <- chem_clean %>%
  ggplot(aes(x = analytename)) +
  geom_bar(aes(fill = analytename)) +
  labs(x = "Analyte",
    y = "Record Count") +
  theme_classic() +
  theme(legend.position = "none")

chem_clean_plot

chem_clean_lplot <- chem_clean_legacy %>%
  ggplot(aes(x = analytename)) +
  geom_bar(aes(fill = analytename)) +
  labs(x = "Analyte",
    y = "Legacy Record Count") +
  theme_classic() +
  theme(legend.position = "none")

chem_clean_lplot

#### Missingness Exploration####

# tbl_chemistryresults

# Adding a new dataset that explicitly records which analytes were inputted on which dates (this will also give us an idea as to unique submissions).

cc_P <- chem_clean_P %>%
  filter(analytename == "Phosphorus as P") %>%
  select(stationcode, sampledate, analytename) %>%
  rename(P1 = analytename)

cc_OP <- chem_clean_P %>%
  filter(analytename == "OrthoPhosphate as P") %>%
  select(stationcode, sampledate, analytename) %>%
  rename(P2 = analytename)

cc_all_P <- full_join(cc_P, cc_OP) # full join of phosphorus data (field and lab replicates #1 only).

cc_TN <- chem_clean_N %>%
  filter(analytename == "Nitrogen,Total") %>%
  select(stationcode, sampledate, analytename) %>%
  rename(N1 = analytename)

cc_TKN <- chem_clean_N %>%
  filter(analytename == "Nitrogen, Total Kjeldahl") %>%
  select(stationcode, sampledate, analytename) %>%
  rename(N2 = analytename)

cc_all_N1 <- full_join(cc_TN, cc_TKN) # partial join of nitrogen data

cc_NN <- chem_clean_N %>%
  filter(analytename == "Nitrate + Nitrite as N") %>%
  select(stationcode, sampledate, analytename) %>%
  rename(N3 = analytename)

cc_all_N2 <- full_join(cc_all_N1, cc_NN) # partial join of nitrogen data

cc_NO3 <- chem_clean_N %>%
  filter(analytename == "Nitrate as N") %>%
  select(stationcode, sampledate, analytename) %>%
  rename(N4 = analytename)

cc_all_N3 <- full_join(cc_all_N2, cc_NO3) # partial join of nitrogen data

cc_NO2 <- chem_clean_N %>%
  filter(analytename == "Nitrite as N") %>%
  select(stationcode, sampledate, analytename) %>%
  rename(N5 = analytename)

cc_all_N4 <- full_join(cc_all_N3, cc_NO2) # partial join of nitrogen data

cc_NH4 <- chem_clean_N %>%
  filter(analytename == "Ammonia as N") %>%
  select(stationcode, sampledate, analytename) %>%
  rename(N6 = analytename)

cc_all_N <- full_join(cc_all_N4, cc_NH4) # full join of nitrogen data

cc_all <- full_join(cc_all_P, cc_all_N) # full join of nutrient data

# 141 unique records
# P & N for 141 (100%)
# Total P for 141 (100%)
# Total N for 140 (99%) 
# - Nitrogen,Total for 73 (52%)
# - Nitrogen, Total Kjeldahl; Nitrate + Nitrite as N for 4 (3%)
# - Nitrogen, Total Kjeldahl; Nitrate as N; Nitrite as N for 63 (45%)

# legacy_tbl_chemistryresults

# Adding new dataset that records which analytes were inputted on which dates.

ccl_P <- chem_clean_lP %>%
  filter(analytename == "Phosphorus as P") %>%
  select(stationcode, sampledate, analytename) %>%
  rename(P1 = analytename)

ccl_OP <- chem_clean_lP %>%
  filter(analytename == "OrthoPhosphate as P") %>%
  select(stationcode, sampledate, analytename) %>%
  rename(P2 = analytename)

ccl_all_P <- full_join(ccl_P, ccl_OP) # full join of phosphorus data (field and lab replicates #1 only).

ccl_TN <- chem_clean_lN %>%
  filter(analytename == "Nitrogen,Total") %>%
  select(stationcode, sampledate, analytename) %>%
  rename(N1 = analytename)

ccl_TKN <- chem_clean_lN %>%
  filter(analytename == "Nitrogen, Total Kjeldahl") %>%
  select(stationcode, sampledate, analytename) %>%
  rename(N2 = analytename)

ccl_all_N1 <- full_join(ccl_TN, ccl_TKN) # partial join of nitrogen data

ccl_NN <- chem_clean_lN %>%
  filter(analytename == "Nitrate + Nitrite as N") %>%
  select(stationcode, sampledate, analytename) %>%
  rename(N3 = analytename)

ccl_all_N2 <- full_join(ccl_all_N1, ccl_NN) # partial join of nitrogen data

ccl_NO3 <- chem_clean_lN %>%
  filter(analytename == "Nitrate as N") %>%
  select(stationcode, sampledate, analytename) %>%
  rename(N4 = analytename)

ccl_all_N3 <- full_join(ccl_all_N2, ccl_NO3) # partial join of nitrogen data

ccl_NO2 <- chem_clean_lN %>%
  filter(analytename == "Nitrite as N") %>%
  select(stationcode, sampledate, analytename) %>%
  rename(N5 = analytename)

ccl_all_N4 <- full_join(ccl_all_N3, ccl_NO2) # partial join of nitrogen data

ccl_NH4 <- chem_clean_lN %>%
  filter(analytename == "Ammonia as N") %>%
  select(stationcode, sampledate, analytename) %>%
  rename(N6 = analytename)

ccl_all_N5 <- full_join(ccl_all_N4, ccl_NH4) # partial join of nitrogen data

ccl_N03 <- chem_clean_lN %>%
  filter(analytename == "Nitrate as N03") %>%
  select(stationcode, sampledate, analytename) %>%
  rename(N7 = analytename)

ccl_all_N6 <- full_join(ccl_all_N5, ccl_N03) # partial join of nitrogen data

ccl_ON <- chem_clean_lN %>%
  filter(analytename == "Nitrogen-Organic") %>%
  select(stationcode, sampledate, analytename) %>%
  rename(N8 = analytename)

ccl_all_N <- full_join(ccl_all_N6, ccl_ON) # full join of nitrogen data

ccl_all <- full_join(ccl_all_P, ccl_all_N) # full join of nutrient data

# 1923 unique records in the legacy table
# P & N for 1783 (93%)
# Total P for 1797 (93%)
# Total N for 1833 (95%) 

# I used various iterations of the code below to count how many instances of each of these combinations happened:
test <- ccl_all %>% 
  filter(P1 == "Phosphorus as P") %>%
  filter(N1 == "Nitrogen,Total" | 
      N2 == "Nitrogen, Total Kjeldahl" & N4 == "Nitrate as N" & N5 == "Nitrite as N" |
      N2 == "Nitrogen, Total Kjeldahl" & N7 == "Nitrate as N03" & N5 == "Nitrite as N" |
      N2 == "Nitrogen, Total Kjeldahl" & N3 == "Nitrate + Nitrite as N")

# - Nitrogen,Total for 1529 (80%)
# - Nitrogen, Total Kjeldahl; Nitrate + Nitrite as N for 935 (49%)
# - Nitrogen, Total Kjeldahl; Nitrate as N; Nitrite as N for 1512 (79%)
# - Nitrogen, Total Kjeldahl; Nitrate as N03; Nitrite as N for 0 (0%)

# Jhen's script 

#### Identifying Duplicates ####

# Create a listing of the duplicate records between the tbl_chem and legacy_tbl_chem datasets.

cc <- chem_clean %>%
  select(stationcode, sampledate, analytename, result) %>% # selects specific columns
  mutate(Date = as.Date(sampledate)) %>% # creates new column
  mutate(Year = year(Date)) %>% # creates new column
  mutate(stationcode_Year = paste(stationcode, Year, sep="_")) # creates new column, combining station code & Year

ccl <- chem_clean_legacy %>% # same steps as above
  select(stationcode, sampledate, analytename, result) %>% 
  mutate(Date = as.Date(sampledate)) %>% 
  mutate(Year = year(Date)) %>%
  mutate(stationcode_Year = paste(stationcode, Year, sep="_"))
  
chem_full <- full_join(cc, ccl) %>% # join "cleaner" versions of chem_clean & chem_clean_legacy
  pivot_wider(names_from = "analytename", values_from = "result", values_fn = length) %>% # made separate analyte columns
  distinct(stationcode_Year, .keep_all = TRUE) %>% 
  relocate("Phosphorus as P", .after = last_col()) %>% # reorganized column order
  select(-c("Ammonia as N", "OrthoPhosphate as P", "Nitrogen-Organic")) # removed analytes not used as a measure of TN/TP

chem_dups <- full_join(cc, ccl) %>% # same code as above
  pivot_wider(names_from = "analytename", values_from = "result", values_fn = length) %>%
  distinct(stationcode_Year, .keep_all = TRUE) %>% 
  relocate("Phosphorus as P", .after = last_col()) %>%
  select(-c("Ammonia as N", "OrthoPhosphate as P", "Nitrogen-Organic")) %>%
  filter_at(vars(-stationcode, sampledate, Date, Year, stationcode_Year), any_vars(. == 2)) # filters out duplicate entries

# End of script. 10/2/2020