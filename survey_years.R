######### Find CHIS Survey Years ################

##### packages #####
library(readxl)     # read excel files
library(janitor)    # clean up data
library(tidyverse)  # tidyverse packages

##### chis #####

# load data
chis <- read_csv("data/CHIS_cbs_sample_dates.csv")

# get table w/ sites + years

chis_yrs <- chis %>%
  mutate(sample_yr = lubridate::year(sample_date)) %>%
  select(intertidal_sitename, island, sample_yr) %>%
  distinct()

View(chis_yrs)

##### cabr #####

# load data

cabr <- read_excel("data/neya_suresh_kumar_CABR_data_20210316.xlsx", 
                sheet = "point_contact_full_sample")

cabr_yrs <- cabr %>%
  select(intertidal_sitename, year) %>%
  distinct()
