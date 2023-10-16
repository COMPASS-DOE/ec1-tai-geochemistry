
# 1. Setup ---------------------------------------------------------------------
cat("Setup")

# load packages
require(pacman)
pacman::p_load(cowsay, tidyverse, googlesheets4, googledrive, purrr, stringr)

## Welcome
say("Welcome to EXCHANGE!", by = "random")

## Define analyte
var <- "sample inventory"

## URL for data
L1directory <- "https://drive.google.com/drive/folders/1yhukHvW4kCp6mN2jvcqmtq3XA5niKVR3"

# 2. Import data ---------------------------------------------------------------
cat("Importing", var, "data...")

files <-
  drive_ls(L1directory, recursive = TRUE) %>%
  filter(grepl("L1", name))

## b. Download files to local (don't worry, we'll delete em in a sec)
lapply(files$id, drive_download, overwrite = TRUE)

## BETWEEN THESE STEPS I DELETE BY HAND THE FTICRMS AND CDOM FILES

paths <- list.files("./", pattern = "soil|water", full.names = TRUE)

lapply(paths, read_delim) -> t
purrr::reduce(t, dplyr::full_join, by = c("campaign", "kit_id", "transect_location")) -> test

file.remove(c(files$name))  

tai_palette <- c("upland" = "#508C67","transition" = "#73A93A","wetland" = "#7F4D18", "water" = "#67B7C6")

analytes <- c("doc_mgC_L", "ph_water", "ph_soil", "carbon_weight_perc", "nitrogen_weight_perc","tdn_mgN_L")

test %>% 
  select(-contains("flag"), -volume_filtered_ml, -filters_used, -total_filter_mass_g) %>% 
  rename(ph_water = ph.y, ph_soil = ph.x) %>% 
  pivot_longer(-c("campaign", "kit_id", "transect_location"), values_to = "value", names_to = "analyte") %>% na.omit() %>% 
  filter(analyte %in% analytes) %>% 
  mutate(transect_location = fct_relevel(transect_location, "upland", "transition", "wetland", "water"),
         analyte = case_when(analyte == "ph_water" ~ "Water pH",
                             analyte == "tdn_mgN_L" ~ "Total Dissolved N (mgN/L)",
                             analyte == "doc_mgC_L" ~ "Dissolved Organic Carbon (mgC/L)",
                             analyte == "ph_soil" ~ "Soil pH",
                             analyte == "carbon_weight_perc" ~ "Soil Carbon Weight (%)",
                             analyte == "nitrogen_weight_perc" ~ "Soil Nitrogen Weight (%)"),
         analyte = fct_relevel(analyte, "Water pH", "Total Dissolved N (mgN/L)", 
                               "Dissolved Organic Carbon (mgC/L)", "Soil pH", 
                               "Soil Carbon Weight (%)", "Soil Nitrogen Weight (%)")) -> test2

test2 %>% 
  ggplot(aes(x = transect_location, y = value, fill = transect_location)) + 
ggdist::stat_halfeye(
  adjust = .5, 
  width = .6, 
  .width = 0, 
  justification = -.2, 
  point_colour = NA
) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA
  ) +
  # geom_point(
  #   alpha = .5
  # ) +
  coord_cartesian(xlim = c(1.2, NA), clip = "off") + facet_wrap(~analyte, scales = "free") +
  theme_minimal() + scale_fill_manual(values = tai_palette) + 
  labs(y = "Analyte Value", x = "Transect Location", fill = "Transect Location") +
  theme(text = element_text(size = 40)) -> g
  
ggsave("pi-meeting.jpg", plot = g, width = 40, height = 20)

