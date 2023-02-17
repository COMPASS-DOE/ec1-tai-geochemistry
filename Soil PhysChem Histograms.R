ggplot(soils_data_foralex, aes(x = gwc_perc)) +
geom_density(alpha = 0.5) + 
labs(x = "GWC (%)", y = "Frequency of % TC", fill = "GWC (%)") +
xlim(5, 30) -> minerals_density
plot(minerals_density)

phys_chem <- soils_data_foralex %>%
  select("kit_id", "gwc_perc","bulk_density_g_cm3", "specific_conductance_us_cm", "loi_perc", "transect_location", "region")

#Histogram for GWC - non-normal distribution
phys_chem %>% ggplot(aes(x = gwc_perc))+
  geom_histogram(bins=20, color="black", fill="cyan4")

#Histogram for BD - non-normal distribution
phys_chem %>% ggplot(aes(x = bulk_density_g_cm3))+
  geom_histogram(bins=20, color="black", fill="cyan4")

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

phys_chem %>% group_by(gwc_perc, bulk_density_g_cm3, transect_location) %>%
  summarize(mean_gwc = mean(gwc_perc))

phys_chem %>% group_by(gwc_perc,bulk_density_g_cm3)