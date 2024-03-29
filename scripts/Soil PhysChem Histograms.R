##Loading packages 
library(tidyverse)
library(ggplot2)
library(multcomp)

#Loading Data 
soils_data_foralex <- readRDS("~/GitHub/ec1-tai-geochemistry/soils_data_foralex.rds")

#Creating Dataframe 
phys_chem <- soils_data_foralex %>%
  mutate(transect_location_factor=factor(transect_location,levels=c("Wetland", "Transition", "Upland")),
         region_factor=factor(region, levels=c("Chesapeake Bay", "Great Lakes")),
         region_factor= stringr::str_replace_all(region_factor, pattern = "Chesapeake Bay", replacement = "Mid-Atlantic")) %>%
  dplyr::select(kit_id, gwc_perc,bulk_density_g_cm3, specific_conductance_us_cm, loi_perc, transect_location, region, transect_location_factor, region_factor)


#----------------------------------------Histograms--------------------------------------------------------------------------

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

#------------------------------------------------Box Plots by Transect------------------------------------------------------------------

#Box plot for GWC vs Transect 
qplot(data=phys_chem,x = transect_location_factor, y = gwc_perc, fill=transect_location,geom = "boxplot")+
  geom_jitter()+
  xlab("Transect Location")+ylab("Graviemtric Water Content (%)")+
  scale_fill_manual(values=c("#056009","#8E7941","#021677"))+
  theme_bw()+
  theme(legend.position = "none",panel.background = element_blank())

#Box plot for GWC vs Region
qplot(data=phys_chem,x = region_factor, y = gwc_perc, fill=region ,geom = "boxplot")+
  geom_jitter()+
  xlab("Region")+ylab("Graviemtric Water Content (%)")+
  scale_fill_manual(values=wesanderson::wes_palette("Darjeeling2"))+
  theme_bw()+
  theme(legend.position = "none",panel.background = element_blank())

#Box plot for BD vs Transect
qplot(data=phys_chem,x = transect_location_factor, y = bulk_density_g_cm3, fill=transect_location,geom = "boxplot")+
  geom_jitter()+
  xlab("Transect Location")+ylab("Bulk Density (g/cm3)")+
  scale_fill_manual(values=c("#056009","#8E7941","#021677"))+
  theme_bw()+
  theme(legend.position = "none",panel.background = element_blank())

#Box plot for LOI vs Transect
qplot(data=phys_chem,x = transect_location_factor, y = loi_perc, fill=transect_location,geom = "boxplot")+
  geom_jitter()+
  xlab("Transect Location")+ylab("Loss-On-Ignition (%)")+
  scale_fill_manual(values=c("#056009","#8E7941","#021677"))+
  theme_bw()+
  theme(legend.position = "none",panel.background = element_blank())

#--------------------------------Box Plot By Region---------------------------------------------------------------------------------------

#Box plot for BD vs Region
qplot(data=phys_chem,x = region_factor, y = bulk_density_g_cm3, fill=region,geom = "boxplot")+
  geom_jitter()+
  xlab("Region")+ylab("Bulk Density (g/cm3)")+
  scale_fill_manual(values=c("#056009","#8E7941","#021677"))+
  theme_bw()+
  theme(legend.position = "none",panel.background = element_blank())

#Box plot for SC vs Region
qplot(data=phys_chem,x = region, y = specific_conductance_us_cm, fill=region,geom = "boxplot")+
  geom_jitter()+
  xlab("Region")+ylab("Specific Conductance (us cm)")+
  scale_fill_manual(values=c("#056009","#8E7941","#021677"))+
  theme_bw()+
  theme(legend.position = "none",panel.background = element_blank())

#Box plot for LOI vs Region 
qplot(data=phys_chem,x = region, y = loi_perc, fill=region,geom = "boxplot")+
  geom_jitter()+
  xlab("Region")+ylab("Loss On Ignition (%)")+
  scale_fill_manual(values=c("#056009","#8E7941","#021677"))+
  theme_bw()+
  theme(legend.position = "none",panel.background = element_blank())

#Box plot for GWC vs Region 
qplot(data=phys_chem,x = region, y = gwc_perc, fill=region,geom = "boxplot")+
  geom_jitter()+
  xlab("Region")+ylab("Gravimetric Water Content (%)")+
  scale_fill_manual(values=c("#056009","#8E7941","#021677"))+
  theme_bw()+
  theme(legend.position = "none",panel.background = element_blank())

#------------------------------------------Facet Graphs------------------------------------------------------------------------

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

##ANOVA - using the anova function, learned this way from stackexchange 
phys_chem$transect_location <- as.factor(phys_chem$transect_location)
anova(lm(gwc_perc ~ phys_chem$transect_location, phys_chem))



## Learning ANOVA from Allison using aov instead of anova 
#------------------------------------------------AOV for GWC and Transect---------------------------------------------------------------------
##AOV for GWC and Transect Location
aov_gwc<- aov(gwc_perc ~ transect_location_factor, data = phys_chem)  
summary.aov(aov_gwc)
Tukey_gwc <-TukeyHSD(aov_gwc, conf.level = 0.95) #running ad-hoc Tukey test to see what is driving the difference
print(Tukey_gwc)

##trying to plot Tukey results on box plot 
tukey <- glht(aov_gwc, linfct=mcp(transect_location_factor="Tukey"))
cld(tukey)

#---------------------------------------------AOV for BD and Transect------------------------------------------------------------------------

##AOV for BD and Transect Location
aov_bd<- aov(bulk_density_g_cm3 ~ transect_location_factor, data = phys_chem)  
summary.aov(aov_bd)
Tukey_bd <-TukeyHSD(aov_bd, conf.level = 0.95) #running ad-hoc Tukey test to see what is driving the difference
print(Tukey_bd)
tukey <- glht(aov_bd, linfct=mcp(transect_location_factor="Tukey"))
cld(tukey)

#------------------------------------------------AOV for LOI and Transect---------------------------------------------------------------------

##AOV for LOI and Transect Location
aov_loi<- aov(loi_perc ~ transect_location_factor, data = phys_chem)  
summary.aov(aov_loi)
Tukey_loi <-TukeyHSD(aov_loi, conf.level = 0.95) #running ad-hoc Tukey test to see what is driving the difference
print(Tukey_loi)
tukey <- glht(aov_loi, linfct=mcp(transect_location_factor="Tukey"))
cld(tukey)

#---------------------------------------------AOV for BD and Region---------------------------------------------------

aov_bd2<- aov(bulk_density_g_cm3 ~ region_factor, data = phys_chem)  
summary.aov(aov_bd2)
Tukey_bd2 <-TukeyHSD(aov_bd2, conf.level = 0.95) #running ad-hoc Tukey test to see what is driving the difference
print(Tukey_bd2)
tukey <- glht(aov_bd2, linfct=mcp(region_factor="Tukey"))
cld(tukey)

#-----------------------------------------AOV for LOI and Region-------------------------------------------------------

aov_loi2<- aov(loi_perc ~ region_factor, data = phys_chem)  
summary.aov(aov_loi2)
Tukey_loi2 <-TukeyHSD(aov_loi2, conf.level = 0.95) #running ad-hoc Tukey test to see what is driving the difference
print(Tukey_loi2)
tukey <- glht(aov_loi2, linfct=mcp(region_factor="Tukey"))
cld(tukey)


#---------------------------------------------AOV for GWC and Region---------------------------------------------------

aov_gwc2<- aov(gwc_perc ~ region_factor, data = phys_chem)  
summary.aov(aov_gwc2)
Tukey_gwc2 <-TukeyHSD(aov_gwc2, conf.level = 0.95) #running ad-hoc Tukey test to see what is driving the difference
print(Tukey_gwc2)
tukey <- glht(aov_gwc2, linfct=mcp(region_factor="Tukey"))
cld(tukey)
