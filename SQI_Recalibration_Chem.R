# SQI Recalibration Script re: Chemistry (N/P)
# Creator: Heili Lowman
# Date: September 4th
# contributor: Jhen Cabasal, Megan Mirkhanian

#### Packages ####

# Attach necessary packages.
library(tidyverse)

#### Data Import ####

# Load in necessary SMC datasets.
tbl_chem <- read_csv("tbl_chemistryresults_NP.csv")
legacy_tbl_chem <- read_csv("legacy_tbl_chemistryresults_NP.csv")
#swamp_tax <- read_csv("swamp_taxonomysampleinfo.csv")

#### Tidy Data ####

# The tbl_chem dataset has been pulled from the SMC database and represents data that is submitted directly to SCCWRP by participating partners. The legacy_tbl_chem dataset has also been pulled from the SMC database, but these are values collected prior to the institution of the new data checker/portal, so these were transferred from the old Access database.

#There has been no filter imposed on these data except to pull only data using the following identifiers in the analytename column: "Nitrogen,Total", "Nitrate + Nitrite as N", "Nitrogen, Total Kjeldahl", "Ammonia as N", "Nitrate as N", "Nitrate as N03", "Nitrite as N", "Nitrogen-Organic", "OrthoPhosphate as P", and "Phosphorus as P".

# The following code will trim them down further to generate datasets with actual values of interest.

chem_clean <- tbl_chem %>% # Takes the original dataset.
  filter(!stationcode %in% c("000NONPJ", "LABQA", "FBLANK")) %>% # Removes non-sample values.
  filter(sampletypecode == "Grab") %>% # Includes grab samples only.
  filter(matrixname == "samplewater") %>% # Includes samples of water only.
  mutate(result_ed = ifelse(result < 0, 0, result)) # Creates a new column named result_ed in which all samples that were effectively reported as negative values instead are reported as 0. These values are not missing, just below the detection limit.

chem_clean_legacy <- legacy_tbl_chem %>% # Same actions as above.
  filter(!stationcode %in% c("000NONPJ", "LABQA", "FBLANK")) %>% 
  filter(sampletypecode == "Grab") %>% 
  filter(matrixname == "samplewater") %>% 
  mutate(result_ed = ifelse(result < 0, 0, result))

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

#### Separate datasets for analytes ####

# Goal: Create separate datasets for each analytename in the chem_clean dataset where their results are "-88".

missing_chem <- chem_clean %>% # takes dataset with values of interest
  filter(result == "-88") # only looking at results listed with "-88", that are presumed to be 'missing data'
# 282 entries

# using analytename identifies to sort filter_chem in 8 separate tables

total_N <- missing_chem %>% 
  filter(analytename == "Nitrogen,Total") # creating a table with "Nitrogen,Total" in analytename column
# 12 entries

nitrate_nitrite <- missing_chem %>% 
  filter(analytename == "Nitrate + Nitrite as N") # creating a table with "Nitrate + Nitrite as N" in analytename column
# 24 entries

total_Kjeldahl <- missing_chem %>% 
  filter(analytename == "Nitrogen, Total Kjeldahl") # creating a table with "Nitrogen, Total Kjeldahl" in analytename column
# 23 entries

ammonia_N <- missing_chem %>% 
  filter(analytename == "Ammonia as N") # creating a table with "Ammonia as N" in analytename column
# 48 entries

nitrate_N <- missing_chem %>% 
  filter(analytename == "Nitrate as N") # creating a table with "Nitrate as N" in analytename column
# 36 entries

nitrite_N <- missing_chem %>% 
  filter(analytename == "Nitrite as N") # creating a table with "Nitrite as N" in analytename column
# 86 entries

orthophospate_P <- missing_chem %>% 
  filter(analytename == "OrthoPhosphate as P") # creating a table with "OrthoPhosphate as P" in analytename column
# 42 entries

phosphorus_P <- missing_chem %>% 
  filter(analytename == "Phosphorus as P") # creating a table with "Phosphorus as P" in analytename column
# 11 entries

# paused here, by Jhen (9/15/2020)