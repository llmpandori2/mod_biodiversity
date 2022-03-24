#################################################
# Title: Rough Vis - Mod Biodiversity CABR
# Purpose: 
# Author: LP
# Created: 3/23/22
# Last edited: 2/22/22
##################################################

##### packages #####

library(readxl)
library(viridis)
library(tidyverse)

##### load data #####

mod <- read_excel("data/mod_biodiversity_sp22.xlsx")

# fix entry errors
mod <- mod %>%
  # there's a "NO" instead of "BO" for substrate
  mutate(substrate = if_else(substrate == 'NO', 'BO', substrate))

##### tidy data #####

# get unique list of spp and see where we can lump them
spplist <- tibble(orig_code = c(mod$layer1, mod$layer2, mod$layer3, mod$layer4, mod$layer5)) %>% distinct()

# make a list of things that can be condensed
# into OR
or_list <- c('PLO', 'CEN', 'LAU', 'GEL', 'C', 'CL', 'MAZ', 'CAU', 'SS', 'MA', 'GAS', 'CS', 'CA', 'OSM', 'MEL')
# as inverts
# list of condensed things
invert_list <- c('LI', 'PH', 'MEX', 'LO', 'TF', 'CH', 'OI', 'TG')

# use "or" list to modify columns
mod <- mod %>%
  # if layer 2+ is R or SA, remove it
  mutate(across(layer2:layer5, ~ na_if(.x, 'R'))) %>%
  mutate(across(layer2:layer5, ~ na_if(.x, 'SA'))) %>%
  # if taxa names are on OR list, change to "OR"
  mutate(across(layer1:layer5, ~ if_else(.x %in% or_list, 'OR', .x))) %>%
  # if taxa names are on invert list, change to "OI" 
  mutate(across(layer1:layer5, ~ if_else(.x %in% invert_list, 'OI', .x))) %>%
  # remove any layers that are all NAs
  janitor::remove_empty(which = 'cols')

remove(invert_list, or_list, spplist)

##### stacked bar chart of substrate types #####

# 3 bars - zone 1, zone 3 flat (transects 0-21), zone 3 boulders (transects 24-30)

bar_data <- mod %>%
  mutate(site2 = case_when(site == 'cabr3' & transect >= 24 ~ 'CABR 3B - Boulder',
                           site == 'cabr3' & transect < 24 ~ 'CABR 3A - Flat',
                           TRUE ~ 'CABR 1')) %>%
  group_by(site2, substrate) %>%
  tally() %>%
  # remove "NA" substrate where transects were underwater
  ungroup() %>%
  filter(!is.na(substrate)) %>%
  # get total sum
  group_by(site2) %>%
  mutate(sumn = sum(n)) %>%
  # calculate proportion of each habitat
  ungroup() %>%
  mutate(prop = n/sumn)

# plot
ggplot(data = bar_data,
       mapping = aes(x = site2, y = prop, group = substrate, fill = substrate)) + 
  geom_col() + 
  scale_fill_viridis(discrete = TRUE, labels = c('Bench', 'Boulder', 'Cobble', 'Sand', 'Tidepool'),
                     name = 'Substrate') + 
  xlab('Site') + 
  ylab('Substrate proportion') + 
  ggtitle('Substrate types across CABR sites') + 
  theme_classic() + 
  theme(text = element_text(size = 12, color = 'black'))

ggsave('./figures/substrate_props_sp22.png')

remove(bar_data)
  
##### "heatmap" of cover types #####

# make function, apply across sites

heat_fn <- function(sitename) {
  
# get data for site of interest
dat <- filter(mod, site == sitename)

# tidy data + calculate point #'s to pivot longer
dat2 <- dat %>%
  # fill in blanks w/ layer prior
  # note - no layer 5 as of writing this code
  mutate(layer2 = if_else(is.na(layer2), layer1, layer2),
         layer3 = if_else(is.na(layer3), layer2, layer3),
         layer4 = if_else(is.na(layer4), layer3, layer4),
         # get point # instead of place on transect
         pt = m/m[1]) %>%
  # pivot layers longer
  pivot_longer(layer1:layer4, names_to = 'layers', values_to = 'cover_type') %>%
  # give different #'s to layers
  mutate(pt = case_when(layers == 'layer1' ~ pt,
                        layers == 'layer2' ~ pt + 0.25,
                        layers == 'layer3' ~ pt + 0.5,
                        layers == 'layer4' ~ pt + 0.75)) %>%
  # make matrix for heatmap
  select(transect, cover_type, pt)
  
# make heatmap with cover type as fill

ggplot(data = dat2,
       mapping = aes(x = transect, y = pt, fill = fct_lump_n(cover_type, 10))) +
  geom_tile() + 
  scale_fill_viridis(discrete = TRUE, name = 'Cover type', option = 'D', na.value = 'gray80') + 
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) + 
  xlab('Transect (0-30m)') + 
  ylab('Distance along transect (points)') + 
  ggtitle(paste('MCBS Cover Types at', sitename)) + 
  theme_classic() + 
  theme(text = element_text(size = 12, color = 'black'))

ggsave(paste0('./figures/mcbs_sp22_', sitename, '.png'))
  
}

heat_fn('cabr1')
heat_fn('cabr3')



