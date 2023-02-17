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

#LOI histogram
phys_chem %>% ggplot(aes(x = loi_perc))+
  geom_histogram(aes(y = ..density..),bins=20, color="black", fill="cyan4")+
  geom_vline(aes(xintercept=mean(loi_perc, na.rm=T)), color="red", linetype="dashed", size=1)+
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

##trying the box plot facet with Allison's code for BD
phys_chem %>% ggplot(aes(x = transect_location, y = bulk_density_g_cm3))+
  geom_jitter(width = 0.1)+
  facet_wrap(~region)+
  labs(x = "",
       y = "Bulk density, g/cm3")

## facet plot for GWC
phys_chem %>% ggplot(aes(x = transect_location, y = gwc_perc))+
  geom_jitter(width = 0.1)+
  facet_wrap(~region)+
  labs(x = "",
       y = "Gravimetric Water Content (%)")

## facet plot for SC
phys_chem %>% ggplot(aes(x = transect_location, y = specific_conductance_us_cm))+
  geom_jitter(width = 0.1)+
  facet_wrap(~region)+
  labs(x = "",
       y = "Specific Conductance, us cm")

##facet plot for LOI
phys_chem %>% ggplot(aes(x = transect_location, y = loi_perc))+
  geom_jitter(width = 0.1)+
  facet_wrap(~region)+
  labs(x = "",
       y = "Loss On Ignition (%)")

##ANOVA
phys_chem$transect_location <- as.factor(phys_chem$transect_location)
anova(lm(gwc_perc ~ phys_chem$transect_location, phys_chem))


##AOV instead of ANOVA for GWC and Transect Location
aov_gwc<- aov(gwc_perc ~ transect_location, data = phys_chem)  
summary.aov(aov_gwc)
Tukey_gwc <-TukeyHSD(aov_gwc, conf.level = 0.95) #running ad-hoc Tukey test to see what is driving the difference
print(Tukey_gwc)

##trying to plot Tukey results on box plot 
multcompBoxplot(formula = gwc_perc ~ transect_location,
  data = phys_chem,
  horizontal = TRUE,
  compFn = "TukeyHSD")



