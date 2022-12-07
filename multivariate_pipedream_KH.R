
## This script is (hopefully) a light introduction to multivariate statistics!
## The goal of these analyses is to understand how different measurements relate
## to each other. The simplest relationships to find and explain is a linear
## relationship, which we observe as correlation between two variables. Our first
## step will be exploring individual relationships and also correlation matrices
## to look for any useful relationships. If you find an interesting story, you 
## may not need anything more complicated than correlations. If you want to dig 
## deeper, I'd suggest looking into principal component analysis (esp using
## ggbiplot) - I can walk making and interpreting if you want. Also, multiple
## linear regression, which is a really powerful tool, but also much more complicated
## to do right. 
##
## 2022-10-26
## Khadijah Homolka
## Adapted largely from Aaliyah Hameed and Peter Regier
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load useful packages
require(pacman)
p_load(tidyverse, ## keep things tidy and load ggplot2
       googledrive, ## get data from Gdrive
       ggpubr, #stat_compare_means()
       corrplot, ## create correlation matrices easily
       changepoint,
       strucchange,
       cowplot) ## plot_grid() to put put multiple plots into one plot


## Set two paths for the two folders with data we want
data_path <- "https://drive.google.com/drive/folders/1r3sP7tvhG2btZACvxZUdtrtSiLVGUUGp"
metadata_path <- "https://drive.google.com/drive/folders/1IQUq_sD-Jama7ajaZl1zW_9zlWfyCohn"


## Next, create two lists of file names we'll use to download files
data_filenames <- drive_ls(data_path) %>% pull(name)
metadata_filenames <- drive_ls(metadata_path) %>% pull(name) # I'm not using
## the metadata files in this script, but you will likely find them useful at 
## some point, so adding this in pre-emptively

## set a better graph theme


## Set common columns
#common_cols <- c("kit_id", "transect_location")

# 2. Import data ---------------------------------------------------------------

## Now, download those files to your local

#path <- "/Users/homo761/Library/CloudStorage/OneDrive-PNNL/Desktop/"

##brute force trying to load in datasets...

##Next, you will have to load in tidyverse
library(tidyverse)

soilparty = readRDS("./soils_data_merged_withmeta.rds") 
#You should have a dataframe 212 obs. of 35 variables pop into your environment 

## Load in useful datasets
#bulk_density <- read_csv(paste0(path, "EC1_Soil_BulkDensity_L0B_20220714.csv"))
#gwc <- read_csv(paste0("EC1_Soil_GWC_L0B_20220601.csv"))
#loi <- read_csv(paste0("EC1_Soil_LOI_L0B_20220601.csv"))
#tctn <- read_csv(paste0("EC1_Soil_TCTN_L0B_20220608.csv"))
#soil_ph <- read_csv(paste0("EC1_Soil_pH_L0B_20220531.csv"))leaving out for now due to 
## small sample size

##df <- full_join(bulk_density %>% select(common_cols, bulk_density_g_cm3), 
              #  gwc %>% select(common_cols, gwc_perc), 
              #  by = c(common_cols)) %>% 
## full_join(loi %>% select(common_cols, loi_perc), 
           # by = c(common_cols)) %>% 
##  full_join(tctn %>% select(common_cols, tn_perc, tc_perc), 
           # by = c(common_cols))


# 3. Individual correlation plots -----------------------------------------------

## First, let's look at the correlation between two factors. Let's do GWC and LOI
## which have a pretty strong relationship. My first step is to plot them so 
## I can look and see visually if there is a correlation: 
ggplot(soilparty, aes(gwc_perc, loi_perc)) + ## select data and variables
  geom_point() + ## add points
  geom_smooth(method = "lm") ## add best fit line

## Now, because we have different sample types, we can divide our plot above into
## 4 different plots to see if that better explains the relationship between
## gwc and loi: 
ggplot(soilparty, aes(gwc_perc, loi_perc)) +
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~transect_location) ## create 'facets' for each transect_location
## So, that's really helpful, because we see that there are different slopes 
## based on different transect_locations, where the first plot doesn't really
## show us that. 

## A different, more confusing way to look at this could be
## to plot all 4 on the same plot: 
ggplot(soilparty, aes(gwc_perc, loi_perc, color = transect_location)) +
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm", se = F) +
  ggtitle("Correlation Between Loss on Ignition and Gravimetric Water Content ") +
  labs( y = "LOI (%)", x = "GWC (%)", color = 'Transect Location') +
  theme_gray() +
  stat_regline_equation(aes(label = ..adj.rr.label..), label.x.npc = 'center', 
                        label.y.npc = 'bottom')


## This is actually kinda helpful in this case, because it clearly shows that
## as we move from sediment to upland, the slope generally increases, which is 
## a useful finding we can use to help interpret our data. In this case, GWC
## is analogous to the amount of water, and LOI is analogous to the amount of 
## organic carbon, so while higher saturation correlates to higher organic carbon,
## smaller increases in saturation in the upland lead to larger increases in 
## organic carbon. This is the kind of finding we are looking for: correlations
## that explain relationships between soil properties which help us understand 
## how the ecosystem works.

## These next graphs don't include numbers for sediment, so lets remove those 
#df_no_sed <- df %>% filter(transect_location != "Sediment")

## Lets make another correlation for BD + GWC and BD + LOI

ggplot(soilparty, aes(gwc_perc, bulk_density_g_cm3, color = transect_location)) +
  geom_point(alpha = 0.25) + 
  geom_smooth(se = F) +
  ggtitle("Correlation Between Bulk Density and Gravimetric Water Content") +
  labs( y = "Bulk Density (g/cm3)", x = "GWC (%)", color = 'Transect Location') 


ggplot(soilparty, aes(loi_perc, bulk_density_g_cm3, color = transect_location)) +
  geom_point(alpha = 0.25) + 
  geom_smooth(se = F) +
  ggtitle("Correlation Between Bulk Density and Loss on Ignition ") +
  labs( y = "Bulk Density (g/cm3)", x = "LOI (%)", color = 'Transect Location') 

## Then a set of graphs relating TN/TC to LOI/GWC
ggplot(soilparty, aes(loi_perc, tn_perc, color = transect_location)) +
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm", se = F) +
  ggtitle("Correlation Between Total Nitrogen and Loss on Ignition") +
  labs( y = "Total Nitrogen (%)", x = "LOI (%)", color = 'Transect Location') +
  stat_regline_equation(label.x = 25, aes(label = ..adj.rr.label..)) ## Adds R^2

ggplot(soilparty, aes(loi_perc, tc_perc, color = transect_location)) +
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm", se = F) +
  ggtitle("Correlation Between Total Carbon and Loss on Ignition") +
  labs( y = "Total Carbon (%)", x = "LOI (%)", color = 'Transect Location') +
  stat_regline_equation(label.x = 25, aes(label = ..adj.rr.label..))

## The GWC vs TC/TN correlations aren't as strong as the ones btwn LOI and TC/TN

#### AGU PLOTS ####
agucolors = c("#056009","#8E7941","#021677")

##### Positive Relationships #####
TNGWC = soilparty %>%
  filter(transect_location %in% c("Upland", "Wetland", "Transition")) %>%
  ggplot(aes(gwc_perc, tn_perc, color = transect_location)) +
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm", se = F) +
  scale_color_manual(values=agucolors) +
  ggtitle("Correlation Between Total Nitrogen and Gravimetric Water Content") +
  labs( y = "Total Nitrogen (%)", x = "GWC (%)", color = 'Transect Location') +
  stat_regline_equation(label.x = 400, aes(label = ..adj.rr.label..)) + 
  cowplot::theme_cowplot()

print(TNGWC)

cowplot::save_plot("./TN_GWC.png", TNGWC, dpi=300)


TCGWC = soilparty %>%
  filter(transect_location %in% c("Upland", "Wetland", "Transition")) %>%
  ggplot(aes(gwc_perc, tc_perc, color = transect_location)) +
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm", se = F) +
  scale_color_manual(values=agucolors) +
  ggtitle("Correlation Between Total Carbon and Gravimetric Water Content") +
  labs( y = "Total Carbon (%)", x = "GWC (%)", color = 'Transect Location') +
  stat_regline_equation(label.x = 400, aes(label = ..adj.rr.label..)) + 
  cowplot::theme_cowplot()

print(TCGWC)

cowplot::save_plot("./TC_GWC.png", TCGWC, dpi=300)

#### Negative Relationships ####
TNBD = soilparty %>%
  filter(transect_location %in% c("Upland", "Wetland", "Transition")) %>%
  ggplot(aes(bulk_density_g_cm3, tn_perc, color = transect_location)) +
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm", se = F) +
  scale_color_manual(values=agucolors) +
  ggtitle("Correlation Between Total Nitrogen and Bulk Density") +
  labs( y = "Total Nitrogen (%)", x = "Bulk Density (g/cm3)", color = 'Transect Location') +
  stat_regline_equation(label.x = 1, aes(label = ..adj.rr.label..)) + 
  cowplot::theme_cowplot()

print(TNBD)

cowplot::save_plot("./TN_BD.png", TNBD, dpi=300)


TCBD = soilparty %>%
  filter(transect_location %in% c("Upland", "Wetland", "Transition")) %>%
  ggplot(aes(bulk_density_g_cm3, tc_perc, color = transect_location)) +
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm", se = F) +
  scale_color_manual(values=agucolors) +
  ggtitle("Correlation Between Total Carbon and Bulk Density") +
  labs( y = "Total Carbon (%)", x = "Bulk Density (g/cm3)", color = 'Transect Location') +
  stat_regline_equation(label.x = 1, aes(label = ..adj.rr.label..)) + 
  cowplot::theme_cowplot()

print(TCBD)

cowplot::save_plot("./TC_BD.png", TCBD, dpi=300)

#####################
ggplot(soilparty, aes(gwc_perc, tc_perc, color = transect_location)) +
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm", se = F) +
  ggtitle("Correlation Between Total Carbon and Gravimetric Water Content") +
  labs( y = "Total Carbon (%)", x = "GWC (%)", color = 'Transect Location') +
  stat_regline_equation(label.x = 400, aes(label = ..adj.rr.label..))


ggplot(soilparty, aes(bulk_density_g_cm3, tc_perc, color = transect_location)) +
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm", se = F) +
  ggtitle("Correlation Between Total Carbon and Bulk Density") +
  labs( y = "Total Carbon (%)", x = "BD (g/cm3)", color = 'Transect Location') +
  stat_regline_equation(aes(label= ..adj.rr.label..))


ggplot(soilparty, aes(bulk_density_g_cm3, tn_perc, color = transect_location)) +
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm",se = F) +
  ggtitle("Correlation Between Total Nitrogen and Bulk Density") +
  labs( y = "Total Nitrogen (%)", x = "BD (g/cm3)", color = 'Transect Location') 

# 4. Correlation matrices ------------------------------------------------------

## Individual correlations are great to explore a relationship you already think
## is interesting, but would take a long time if you have many variables like we
## do, which is where correlation matrices excel. We can take as many continuous
## (won't work on factor or character columns) variables as we want and see how 
## they all correlate to each other. Since we saw that transect location is 
## useful, but transect_location is also a factor, we need to convert it to a 
## numeric value. Don't worry how this code works: 

df <- soilparty %>% 
  mutate(location_numeric = case_when(transect_location == "Sediment" ~ 1, 
                                      transect_location == "Wetland" ~ 2, 
                                      transect_location == "Transition" ~ 3, 
                                      transect_location == "Upland" ~ 4))

## Now let's prepare our data for correlations
df_cor <- df %>% 
  select(where(is.numeric)) %>% ## only select numeric variables
  drop_na() %>%
  select(-location_numeric)## correlations fail if there are missing values (NAs) so drop em

## Time to make our first correlation matrix!
cor(df_cor, method="spearman")
## Interpreting this: Each value is the correlation coefficient between two 
## variables. You can see for the first row in the first column, there's a 
## correlation of 1. That's because bulk_density perfectly correlates with itself.
## Values closer to 1 mean strong positive correlations (when one variable increases, 
## the other increases). Values closer to -1 mean stronger negative correlations 
## (when one variable increases, the other decreases).

## An easier way to visualize this is with corrplot()

library("wesanderson")
pal <- wes_palette("Darjeeling2",21, type = "continuous")


matrix <- cor(df_cor) %>% 
  corrplot(addCoef.col = 'black', col = COL2("BrBG"), tl.srt = 45, tl.col = 'black', 
           type = 'lower', shade.col = c("blue","tan")) 

## Interpretation: We can see that bulk density is negatively correlated to
## both GWC and LOI. We can also see that GWC and LOI are positively correlated,
## which we knew from the plots in the last section. We don't see strong
## correlations between transect location and any of the variables, except maybe
## for GWC.


## Since we saw much stronger correlations between GWC and LOI when separating
## by transect_location, let's do the corrplot again, but just for wetland
## soils: 
df %>% 
  filter(transect_location == "Wetland") %>% 
  select(where(is.numeric)) %>%
  drop_na() %>% 
  cor() %>% 
  corrplot()
## Note that since there's only one level for location_numeric, we don't get
## correlations. But now, we see a strong relationships between LOI and GWC. 

## BD has (-) relationship with GWC, LOI, TN, and TC whereas we can see (+) 
## correlation btwn all of the other variables with one another -Aaliyah


# 5. Correlation stats ---------------------------------------------------------

## Everything above is visual in that we don't know the exact numbers for any of
## these relationships. But we can get those pretty painlessly in the following 
## way. First, let's make a linear model relating LOI and GWC
lm(loi_perc~gwc_perc, data = df)

## This gives us two coefficients, which are b and m from y = mx + b. But we really
## want to know how well it fits (R^2): 
summary(lm(loi_perc~gwc_perc, data = df))
## Down at the bottom, we see Adjusted R-squared = 0.4578, so R2 = 0.46. That's 
## decent, though an R2 > 0.6 would be more notable. One other important piece of
## info: the p-value down at the bottom is very very very low. Lower p-values mean
## a more significant R2, so that's good news.

## Now, since we know that we have stronger relationships when separating by 
## transect_location, let's do that (tidyverse to the rescue!)
summary(lm(loi_perc~gwc_perc, data = df %>% filter(transect_location == "Wetland")))

## R2 = 0.85! Much stronger! That's a great correlation and something we are very
## confident can be interpreted. 


# 6. Statistics to compare groups ----------------------------------------------

## One last thing: we often want to know compare mean values between different
## groups within the dataset. We'll use some ggpubr magic: 

## First, we need to tell ggplot what groups we want to compare
## Set comparisons for stats
compare_transect <- list( #c("Sediment", "Wetland"), #c("Sediment", "Transition"), 
  c("Sediment", "Upland"), c("Wetland", "Transition"), 
  c("Wetland", "Upland"), c("Transition", "Upland")) 
## removed comparision btwn sed/wet & sed/tran because
# insignificant


## make the plot
ggplot(df, aes(transect_location, gwc_perc, fill = transect_location)) + 
  geom_boxplot() + ## add boxplots
  geom_jitter(alpha=0.15) +
  scale_x_discrete(limits= c("Sediment", "Wetland", "Transition", "Upland")) +
  stat_compare_means(aes(label = ..p.signif..), comparisons = compare_transect) +
  labs(y = 'GWC (%)', x = 'Transect Location', fill = 'Transect Location') +
  ggtitle("Significance Between Gravimetric Water Content (GWC) Across Locations")

## Here, ns means not significant, or a p-value >= 0.05. The stars mean that
## there is a significant difference (p<0.05) between the groups.


