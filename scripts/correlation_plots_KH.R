## 1. Load in packages ---------------------------------------------------------
require(pacman)
p_load(tidyverse, ## keep things tidy and load ggplot2
       googledrive, ## get data from Gdrive
       ggpubr, #stat_compare_means()
       corrplot, ## create correlation matrices easily
       changepoint,
       strucchange,
       cowplot) ## plot_grid() to put put multiple plots into one plot

## 2. Read in data--------------------------------------------------------------

soilparty = readRDS("./soils_data_merged_withmeta.rds")

# 3. Correlation matrices ------------------------------------------------------

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
                                      transect_location == "Upland" ~ 4)) %>%
  mutate(region_numeric = case_when(region == "Chesapeake Bay" ~ 1, 
                                    region == "Great Lakes" ~ 2))
         
## Now let's prepare our data for correlations
df_cor <- df %>% 
  select(where(is.numeric)) %>% ## only select numeric variables
  drop_na() ## correlations fail if there are missing values (NAs) so drop em

## Time to make our first correlation matrix!
cor(df_cor)
## Interpreting this: Each value is the correlation coefficient between two 
## variables. You can see for the first row in the first column, there's a 
## correlation of 1. That's because bulk_density perfectly correlates with itself.
## Values closer to 1 mean strong positive correlations (when one variable increases, 
## the other increases). Values closer to -1 mean stronger negative correlations 
## (when one variable increases, the other decreases).

## An easier way to visualize this is with corrplot()
matrix <- cor(df_cor) %>% 
  corrplot(addCoef.col = 'black', col = COL2('PiYG'), tl.srt = 45, tl.col = 'black', 
           type = 'lower') 


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

## I'm trying to get tc, tn and gwc correlations by region and transect location...
df_cor %>% 
  select(gwc_perc, tc_perc, tn_perc, location_numeric, region_numeric) %>% 
  select(where(is.numeric)) %>%
  drop_na() %>% 
  cor() %>% 
  corrplot()


## Note that since there's only one level for location_numeric, we don't get
## correlations. But now, we see a strong relationships between LOI and GWC. 

## BD has (-) relationship with GWC, LOI, TN, and TC whereas we can see (+) 
## correlation btwn all of the other variables with one another -Aaliyah


##Let's try to look at the linear relationships between, tc, tn, and gwc 
## by region and transect location 

## Here is with the TC/TN ratio
df_ratio <- df %>% 
  mutate(tc_tn_ratio = tc_perc/tn_perc) 
  

ggplot(df_ratio, aes(gwc_perc, tc_tn_ratio)) +
  geom_point() + 
  geom_smooth(method = "lm", se=F) + 
  facet_wrap(~transect_location)

ggplot(df_ratio, aes(gwc_perc, tc_tn_ratio)) +
  geom_point() + 
  geom_smooth(method = "lm", se=F) + 
  facet_wrap(~region)

ggplot(df_ratio, aes(gwc_perc, tc_tn_ratio, color = region)) +
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm", se = F) +
  ggtitle("Correlation Between TC/TN Ratio and Gravimetric Water Content ") +
  labs( y = "TC/TN Ratio (%)", x = "GWC (%)", color = 'Region') +
  theme_gray() +
  stat_regline_equation(aes(label = ..adj.rr.label..), label.x.npc = 'center', 
                        label.y.npc = 'bottom')

## Just with TC and GWC by region and transect location

ggplot(df_ratio, aes(gwc_perc, tc_perc)) +
  geom_point() + 
  geom_smooth(method = "lm", se=F) + 
  facet_wrap(~transect_location)

ggplot(df_ratio, aes(gwc_perc, tc_perc)) +
  geom_point() + 
  geom_smooth(method = "lm", se=F) + 
  facet_wrap(~region)

ggplot(df_ratio, aes(gwc_perc, tc_perc, color = region)) +
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm", se = F) +
  ggtitle("Correlation Between Total Carbon and Gravimetric Water Content ") +
  labs( y = "TC (%)", x = "GWC (%)", color = 'Region') +
  theme_gray() +
  stat_regline_equation(aes(label = ..adj.rr.label..), label.x.npc = 'center', 
                        label.y.npc = 'bottom')

ggplot(df_ratio, aes(gwc_perc, tc_perc, color = transect_location)) +
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm", se = F) +
  ggtitle("Correlation Between Total Carbon and Gravimetric Water Content ") +
  labs( y = "TC (%)", x = "GWC (%)", color = 'Transect Location') +
  theme_gray() +
  stat_regline_equation(aes(label = ..adj.rr.label..), label.x.npc = 'center', 
                        label.y.npc = 'bottom')

## TN and GWC by region and transect location 

ggplot(df_ratio, aes(gwc_perc, tn_perc)) +
  geom_point() + 
  geom_smooth(method = "lm", se=F) + 
  facet_wrap(~transect_location)

ggplot(df_ratio, aes(gwc_perc, tn_perc)) +
  geom_point() + 
  geom_smooth(method = "lm", se=F) + 
  facet_wrap(~region)

ggplot(df_ratio, aes(gwc_perc, tn_perc, color = region)) +
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm", se = F) +
  ggtitle("Correlation Between Total Nitrogen and Gravimetric Water Content ") +
  labs( y = "TC (%)", x = "GWC (%)", color = 'Region') +
  theme_gray() +
  stat_regline_equation(aes(label = ..adj.rr.label..), label.x.npc = 'center', 
                        label.y.npc = 'bottom')

ggplot(df_ratio, aes(gwc_perc, tn_perc, color = transect_location)) +
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm", se = F) +
  ggtitle("Correlation Between Total Nitrogen and Gravimetric Water Content ") +
  labs( y = "TC (%)", x = "GWC (%)", color = 'Transect Location') +
  theme_gray() +
  stat_regline_equation(aes(label = ..adj.rr.label..), label.x.npc = 'center', 
                        label.y.npc = 'bottom')

## There are a lot of NA's in soilparty and the subsequent dataframes. Trying 
## to find them here. 

new_DF<-soilparty[is.na(soilparty$region),]
## to save a new dataframe as a file:
    ## save(new_DF, file="NAcolumns.Rda") 
