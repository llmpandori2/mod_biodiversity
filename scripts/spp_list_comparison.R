#################################################
# Title: Species Lists for CABR & CHIS
# Purpose: adapted from clean-up code
# Author: LP
# Created: 2/18/22
# Last edited: 2/18/22
##################################################

##### presets #####

# folder to save things 
save_folder <- './figures/spp_list_comparison_figs/'

# folder to look for cabr data 
cabr_data_folder <- '../cabr_ri/data/MEDN_RI_DB_FA21/'

# folder to look for chis data 
chis_data_folder <-'./data/CHIS_Jan22/'

##### goals #####
# overarching goal: create species list for MEDN modified biodiversity SOP
# Steps: 
  # 1 - get list of spp for each year for CHIS and CABR photoplots (target)
  # 2 - determine if there are spp in other survey types (lottia, timed search, transect) missing
  # 3 - generate heatmap of when spp show up over time at each park unit
  # 4 - export final list for SOP as a table

##### packages #####
library(readxl)     # read excel files
library(janitor)    # clean up data
library(viridis)    # color palette
library(DT)         # table HTML widget
library(tidyverse)  # tidyverse packages

##### load data #####

### CABR
# owl limpet data
lim_density <- read_excel(paste0(cabr_data_folder, 'Limpet_density_by_plot_size.xlsx'))
lim_measure <- read_excel(paste0(cabr_data_folder, 'Limpet_measurements.xlsx'))

# transect data
transect <- read_excel(paste0(cabr_data_folder, 'Line_transect_summary.xlsx'))

# target data
cabr_target <- read_excel(paste0(cabr_data_folder, 'Photoplot_summary_by_plot.xlsx'))

# timed search
timed_search <- read_excel(paste0(cabr_data_folder, 'TimedSearch_plot_counts.xlsx'))

### CHIS

# target data
chis_target <- read.table(paste0(chis_data_folder, 'qsummarizer_TGT_SppN_Whitaker.txt'), 
                         header = T, sep = ',', fileEncoding = 'UTF-8') 

##### get spp over time for cabr and chis #####

spp_time_matrix <- function(dataset, save_name){
  
  # get number of points per year for each spp
                # format all colnames in snake case
  pts_per_yr <- clean_names(dataset) |>
    group_by(survey_year, species_code, scientific_name) |>
    summarize(n = sum(n)) |>
    ungroup()
  
  # save all years html widget
  saveWidget(pts_per_yr %>%
               select(-species_code) %>%
               pivot_wider(names_from = survey_year, values_from = n) %>%
               datatable(), 
             paste(save_folder, save_name,'_widgetall.html', sep = ''))
  
  # save since 2010 html widget
  saveWidget(pts_per_yr %>%
               select(-species_code) %>%
               filter(survey_year >= 2010) %>%
               pivot_wider(names_from = survey_year, values_from = n) %>%
               datatable(), 
             paste(save_folder, save_name,'_widget2010.html', sep = ''))
  
  return(pts_per_yr)
}

# consolidate results, add ID column
all_list <- rbind(mutate(spp_time_matrix(cabr_target, 'cabr'), id = 'cabr'), mutate(spp_time_matrix(chis_target, 'chis'), id = 'chis'))

# clean up environment
remove(chis_target, cabr_target, spp_time_matrix)

##### consolidate lists #####

# get lists where values for since 2010 are non-zero
list2 <- all_list %>%
  filter(survey_year >= 2010 & 
           n > 0) %>%
  # remove ID columns
  select(species_code, scientific_name) %>%
  # get distinct values
  distinct()

# export
write.csv(list2, './data/spp_list_output.csv')
