# SQI Recalibration Script
# Creator: Raphael Mazor
# Date: August 27th

#### Packages ####

library(tidyverse)
library(DBI) # needed to connect to data.dfbase
library(dbplyr) # needed to connect to data.dfbase
library(RPostgreSQL) # needed to connect to our data.dfbase
library(rstudioapi) # just so we can type the password as we run the script, so it is not written in the clear
library(tidyverse)
library(lubridate)
library(skimr)

####Create the connection####

# con is short for connection
# Create connection to the data.dfbase while hiding security credentials.
con <- DBI::dbConnect(
  PostgreSQL(),
  host = showPrompt("username", "Please enter the hostname of IP address of the database"),
  dbname = showPrompt("dbname", "Please enter the name of the database"),
  user = showPrompt("username", "Please enter the username for the database"),
  askForPassword()
)

####Create the SMC tables####

lustations.df<- dbGetQuery(con, ' 
                      SELECT * FROM
                      sde.lu_stations
                            ')

gis.df<-dbGetQuery(con, ' 
                      SELECT * FROM
                      sde.tblgismetrics
                            ')
chan.df<-dbGetQuery(con, ' 
                      SELECT * FROM
                      sde.unified_channelengineering
                            ')


#Chemistry

cond.df<-dbGetQuery(con, ' 
                      SELECT * FROM
                      sde.analysis_combined_specificconductivity
                            ')
cond.df2 <- cond.df %>%  filter(analytename=="SpecificConductivity")

nutrients.df<-  dbGetQuery(con, ' 
                      SELECT * FROM
                      sde.analysis_chem_nutrients
                            ')

#Biology

csci_core.df<- dbGetQuery(con, '
                     SELECT * FROM
                     sde.analysis_csci_core')
# asci.df<- dbGetQuery(con, '
#                      SELECT * FROM
#                      sde.analysis_asci') 


#Physical habitat

ipi.df<- dbGetQuery(con, '
                     SELECT * FROM
                     sde.analysis_phab_ipi')

cram.df<- dbGetQuery(con, '
                     SELECT * FROM
                     sde.tblcramindexandattributescores')
#####

#Prepare DFs for merging
#They should all have a masterid, and a sampleyear. One column per analyte
#chem
cond.df_premerge<-cond.df2 %>% #has masterid, needs sample year
  transmute(masterid=masterid,
            stationcode=stationcode, 
            sampledate=ymd(sampledate),
            sample_year=year(sampledate),
            fieldreplicate=fieldreplicate,
            # labreplicate=case_when(labreplicate>1~labreplicate, T~1),
            labreplicate=labreplicate, 
            SpCond_uScm=result) %>%
  filter(!is.na(masterid))
cond.df_premerge$labreplicate[is.na(cond.df_premerge$labreplicate)]<-1

head(nutrients.df)
nutrients.df_premerge<-
  lustations.df %>% select(masterid, stationcode=stationid) %>%
  right_join(
    nutrients.df %>%
      filter(stationcode!="000NONPJ") %>%
      transmute(#masterid=masterid,
        stationcode=stationcode, 
        sampledate=sampledate %>% 
          substr(1,10) %>%
          ymd(),
        sample_year=year(sampledate),
        fieldreplicate=fieldreplicate,
        # labreplicate=case_when(labreplicate>1~labreplicate, T~1),
        labreplicate=labreplicate, 
        TN_mgL=total_n_mgl,
        TP_mgL=total_p_mgl)
  )%>%
  filter(!is.na(masterid))

# nutrients.df_premerge$fieldreplicate[is.na(nutrients.df_premerge$fieldreplicate)]

chem_premerge<-inner_join(cond.df_premerge %>%
                            filter(fieldreplicate==1 & labreplicate==1), 
                          nutrients.df_premerge %>% 
                            select(-stationcode) %>%
                            filter(fieldreplicate==1 & labreplicate==1)
                            )
#hab

# End of script.
