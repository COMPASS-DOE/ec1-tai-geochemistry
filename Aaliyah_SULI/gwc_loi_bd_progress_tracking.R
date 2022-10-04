## This script brings in the three mostly complete soil physicochemical properties
## datasets we've already got a jump on for EC1 that will hopefully be completed
## during Aaliyah's SULI term. I've started an outline of how I have been visualizing
## what samples are collected and what samples need to be done, but please update
## as you see fit!
## 
## Raw data folder: https://drive.google.com/drive/folders/1UK_PYovVvHZM8gZ8qKjsPHqG77onTcEw
##
## 2022-06-01
## Peter Regier and Aaliyah Hameed
##
# ######### #
# ######### #

# 1. Setup ---------------------------------------------------------------------

## load packages
require(pacman)
p_load(tidyverse, ## Keep things tidy
       googlesheets4, ## read_sheet()
       googledrive) ##

## set up paths for each dataset
bd_path <- "https://docs.google.com/spreadsheets/d/1R00De3XmShwcaVdSuqeO-IaL_sdF200n14hKjmbdFM4/edit#gid=0"
gwc_loi_path <- "https://docs.google.com/spreadsheets/d/1Osig5zxzW3l9z_1Bb0zNW2tfTdJ60hsh78qMvjgclQE/edit"

## Define constants (we are not defining a maximum value for GWC)
gwc_min = 0
loi_min = 0
loi_max = 100

# 2. Read in GWC ---------------------------------------------------------------

## Read in data from Google sheet
gwc_loi_raw <- read_sheet(gwc_loi_path)

## The formula for GWC is [(wt_crucible + wt_moist) - (wt_crucible + wt_dry)] / 
## [(crucible + dry) - (crucible)]
gwc_processed <- gwc_loi_raw %>% 
  separate(sample_id, c("campaign", "kit_id", "transect_location")) %>% 
  mutate(gwc_perc = ((wt_crucible_moist_g - wt_crucible_dry_g) / (wt_crucible_dry_g - wt_crucible_g))* 100) %>% 
  dplyr::select(campaign, kit_id, transect_location, gwc_perc)

gwc_clean <- gwc_processed %>% 
  mutate(gwc_perc = round(gwc_perc, 0)) %>% 
  mutate(gwc_flag = ifelse(gwc_perc < gwc_min, "outside range", NA)) 

# 3. Read in LOI ---------------------------------------------------------------

loi_processed <- gwc_loi_raw %>% 
  separate(sample_id, c("campaign", "kit_id", "transect_location")) %>% 
  mutate(loi_perc = ((wt_crucible_dry_g - wt_crucible_combusted_g) / (wt_crucible_dry_g - wt_crucible_g)) * 100) %>% 
  dplyr::select(campaign, kit_id, transect_location, loi_perc)

loi_clean <- loi_processed %>% 
  mutate(loi_perc = round(loi_perc, 0)) %>% 
  mutate(loi_flag = ifelse(loi_perc < loi_min | loi_perc > loi_max, "outside range", NA)) 


# 4. Read in BD ----------------------------------------------------------------

## Read in data from Google sheet
bd_raw <- read_sheet(bd_path)

## Calculate bulk density using GWC to calculate dry weight
## two 2.5" ring lids weigh 16.9g, with 5cm diameter and 5.1cm height. Thus, the 
## volume is pi * (d/2)^2 * headspace_cm. Dry weight is calculated from GWC 
## based on the formula of (field_moist / ((GWC / 100) + 1)) from KP
bd_processed <- bd_raw %>% 
  mutate(campaign = "EC1",
         kit_id = str_match(sample_id, "K0\\d\\d")[,1], 
         transect_location = str_to_title(str_match(sample_id, "[:alpha:]{6,10}")), 
         wt_soil_fm_g = wt_ring_soil_lids_g - (wt_ring_g + 17), 
         ring_diameter = ifelse(wt_ring_g > 150, 8, 5), 
         total_volume = ifelse(wt_ring_g > 150, 250, 100), 
         headspace_volume = pi * (ring_diameter/2)^2 * height_headspace_cm, 
         volume_soil_cm3 = total_volume - headspace_volume) %>% 
  left_join(gwc_clean %>% select(-campaign), by = c("kit_id", "transect_location")) %>% 
  mutate(wt_soil_dry_g = wt_soil_fm_g / ((gwc_perc / 100) + 1),
         bulk_density_g_cm3 = wt_soil_dry_g / volume_soil_cm3) %>% 
  dplyr::select(campaign, kit_id, transect_location, bulk_density_g_cm3) %>% 
  mutate(bulk_density_g_cm3 = round(bulk_density_g_cm3, 2))

bd_clean <- bd_processed %>% 
  mutate(bulk_density_flag = ifelse(bulk_density_g_cm3 < 0 | bulk_density_g_cm3 > 2, "outside range", NA))

# 5. Read in list of samples collected -----------------------------------------

## Set the directory where metadata are stored
metadata_directory = "https://drive.google.com/drive/folders/1IQUq_sD-Jama7ajaZl1zW_9zlWfyCohn"

## Next, list all files in the directory, filter out the Kit Level file and snag the file name
metadata_file <- drive_ls(metadata_directory) %>% 
  filter(grepl("KitLevel", name)) %>% 
  pull(name)

## Now download that file to your local (an annoying googledrive requirement) 
drive_download(metadata_file, overwrite = T)

## Finally, read those data in and format so we can set up a list of all samples received
metadata <- read_csv(metadata_file) %>% 
  mutate(kit_id = paste0("K", kit_id)) %>%
  select(kit_id, samples_collected) %>% 
  mutate(Sediment = ifelse(str_detect(samples_collected, "Sediment"), T, F), 
         Wetland = ifelse(str_detect(samples_collected, "Wetland"), T, F), 
         Transition = ifelse(str_detect(samples_collected, "Transition"), T, F), 
         Upland = ifelse(str_detect(samples_collected, "Upland"), T, F)) %>% 
  pivot_longer(cols = c(Sediment, Wetland, Transition, Upland), 
               names_to = "transect_location", values_to = "collected") 


# 6. Bring it all together -----------------------------------------------------

df <- full_join(metadata %>% select(-samples_collected), gwc_clean %>% select(-campaign, -gwc_flag), by = c("kit_id", "transect_location")) %>% 
  full_join(loi_clean %>% select(-campaign, -loi_flag), by = c("kit_id", "transect_location")) %>% 
  full_join(bd_clean %>% select(-campaign, -bulk_density_flag), by = c("kit_id", "transect_location")) %>% 
  filter(collected == TRUE) %>% ## remove samples that weren't collected
  filter(!is.na(transect_location)) ## gets rid of two weird lines without locations


## One example graph I find helpful. Every square that's shaded gray is a sample
## that exists, but that we don't have a GWC value for (i.e., needs to be run)

ggplot(df, aes(kit_id, transect_location, fill = gwc_perc)) + 
  geom_tile() + 
  scale_fill_viridis_c() + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(x = "Kit ID", y = "Transect Location", fill = "GWC (%)", 
       title = "Gray = Sample to be run, white = Sample not received")

## I find this hard to look at in R, so let's save it as an image
ggsave("220602_GWC_progress.png", width = 10, height = 3)


## If you prefer to look at samples that need to be run, you can use filter.
## For instance, if I want to know which samples need to be still be run for GWC, 
## I would do this:

## First, save the samples that need to be run to 'gwc_samples_to_run'
gwc_samples_to_run <- df %>% #for the full dataset, 
  filter(is.na(gwc_perc)) #filter to return only GWC rows that don't have a value

## Now, print all rows of 'gwc_samples_to_run'
print(gwc_samples_to_run, n = nrow(gwc_samples_to_run))

## You can even save this as a spreadsheet so you can open in Excel
write_csv(gwc_samples_to_run, "gwc_samples_to_run.csv")

## It's great experience to play around with how you would like to visualize what 
##samples need to be done. Two options that might be helpful: 

## Tables: https://rfortherestofus.com/2019/11/how-to-make-beautiful-tables-in-r/
## Bar charts: https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html


## df contains information for GWC, LOI, & BD so another graph can be made for 
## these by following step 6, lets start with LOI -Aaliyah
ggplot(df, aes(kit_id, transect_location, fill = loi_perc)) + 
  geom_tile() + 
  scale_fill_viridis_c() + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(x = "Kit ID", y = "Transect Location", fill = "LOI (%)", 
       title = "Gray = Sample to be run, white = Sample not received")

## instead of running the ggsave command just right click the graph and download 
## it as a png -Aaliyah

loi_samples_to_run <- df %>% #for the full dataset, 
  filter(is.na(loi_perc)) #filter to return only LOI rows that don't have a value

## Now, print all rows of 'loi_samples_to_run'
print(loi_samples_to_run, n = nrow(loi_samples_to_run))

## You can even save this as a spreadsheet so you can open in Excel. I saved 
## both a visual copy and the spreadsheet -Aaliyah
write_csv(loi_samples_to_run, "loi_samples_to_run.csv")

## Lastly, lets repeat these steps for BD -Aaliyah
ggplot(df, aes(kit_id, transect_location, fill = bulk_density_g_cm3)) + 
  geom_tile() + 
  scale_fill_viridis_c() + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(x = "Kit ID", y = "Transect Location", fill = "BD (g/cm3)", 
       title = "Gray = Sample to be run, white = Sample not received")

bd_samples_to_run <- df %>% #for the full dataset, 
  filter(is.na(bulk_density_g_cm3)) #filter to return only BD rows that don't have a value



