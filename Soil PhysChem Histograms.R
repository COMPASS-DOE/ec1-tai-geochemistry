phys_chem <- soils_data_foralex %>%
  select("kit_id", "gwc_perc","bulk_density_g_cm3", "specific_conductance_us_cm", "loi_perc", "transect_location", "region")

#Histogram for GWC with mean - non-normal distribution
phys_chem %>% ggplot(aes(x = gwc_perc))+
  geom_histogram(aes(y = ..density..), bins = 20, color="black", fill="cyan4")+
  geom_vline(aes(xintercept=mean(gwc_perc, na.rm=T)), color="red", linetype="dashed", size=1)+
  geom_density()

#Histogram for BD with mean - normal-ish distribution
phys_chem %>% ggplot(aes(x = bulk_density_g_cm3))+
  geom_histogram(aes(y = ..density..),bins=20, color="black", fill="cyan4")+
  geom_vline(aes(xintercept=mean(bulk_density_g_cm3, na.rm=T)), color="red", linetype="dashed", size=1)+
  geom_density()

#Histogram for SC with mean - non-normal distribution
phys_chem %>% ggplot(aes(x = specific_conductance_us_cm))+
  geom_histogram(aes(y = ..density..),bins=20, color="black", fill="cyan4")+
  geom_vline(aes(xintercept=mean(specific_conductance_us_cm, na.rm=T)), color="red", linetype="dashed", size=1)+
  geom_density()

#Box plot for GWC vs Transect
qplot(data=phys_chem,x = transect_location, y = gwc_perc, fill=transect_location,geom = "boxplot")+
  geom_jitter()

#Box plot for BD vs Transect
qplot(data=phys_chem,x = transect_location, y = bulk_density_g_cm3, fill=transect_location,geom = "boxplot")+
  geom_jitter()

#Box plot for LOI vs Transect
qplot(data=phys_chem,x = transect_location, y = loi_perc, fill=transect_location,geom = "boxplot")+
  geom_jitter()

#Box plot for SC vs Region
qplot(data=phys_chem,x = region, y = specific_conductance_us_cm, fill=region,geom = "boxplot")+
  geom_jitter()

#Box plot for GWC vs BD by Transect
qplot(interaction(gwc_perc,bulk_density_g_cm3),transect_location,data=phys_chem, fill=transect_location,geom = "boxplot")+
  geom_jitter()

#Box plot distributions by transect and region but the scaling is all jacked up
ggboxplot(phys_chem, x = "gwc_perc", y = "bulk_density_g_cm3",  color = "transect_location", palette = c("red", "black","blue"), facet.by = "region")

#Anova test 
df$gwc_perc = factor(df$gwc_perc)
df$bulk_density_g_cm3 = factor(df$bulk_density_g_cm3)

phys_chem %>% select(transect_location, bulk_density_g_cm3, gwc_perc) %>%
  anova_test(transect_location ~bulk_density_g_cm3*gwc_perc)
