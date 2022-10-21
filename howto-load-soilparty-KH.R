##Notes on how to check your working directory and setup the soilparty dataset to do fun stats on 
##It's good to get in the habit of checking what working directory you are in. This will effect 
## whether your code works or not, or cause you to be saving stuff in the wrong directory and then be very sad 

##First, open the tai-geochemistry-ec1.Rproj first from the Github, then double check your working directory

##This command will return what working directory you are currently in 
getwd()  

##Next, you will have to load in tidyverse
library(tidyverse)

##Then, run the following line of code 
soilparty = readRDS("./soils_data_merged_withmeta.rds") 
#You should have a dataframe 212 obs. of 35 variables pop into your environment 