## Everyone has their own way of coding, and their favorite ways to do things. 
## One of the great things about R is that you can code however you want, as long
## as it works! There are of course "best practices" but mostly whatever works for
## you is what is important. This script is designed to introduce you to the 
## anatomy of R code, and how I like to set my scripts up.

## First, this is a comment. Comments are anything that follows a '#' character, 
## and R doesn't try to evaluate (run) comments.


## R runs on libraries, the standard way to install a library is
install.packages("ggplot2")

## I do something different, that I think is simpler, and uses the 'pacman' library

## First, install/load pacman using the 'require' function
require(pacman)

## Now, you can use the p_load function (from the pacman library) to load a library. installs and loads
#it's like a shortcut
p_load(ggplot2)

## You can also do this for multiple packages
p_load(ggplot2, dplyr)

## Those are the basics, I'm going to write a full (simple) script below to illustrate
## how I like to set things up. Like I said above, you can write code however you
## like, and there are all sorts of cool tips and tricks out there, if you ever
## get stuck, you can always ask me, but Google will be your best friend as you
## learn to code and troubleshoot, places like stackexchange have answers to 
## virtually any R code question you can think up. google is your bff

# ------------------------------------------------------------------------------


## This script is an introduction to some basic R code that will be useful for 
## understanding the data you'll be collecting, and for completing the 
## research you'll present in your SULI deliverables.
## I start my codes off with a header (composed of comments) that explains what
## what this code is supposed to do. 
##
## Peter Regier 
## 2022-05-31
##
# ########## #
# ########## #

# 1. Setup environment ---------------------------------------------------------

## Load pacman
require(pacman)

## Load useful packages
p_load(tidyverse, # Contains many different tidy packages, including ggplot2 for 
       #graphing and dplyr for tidy data wrangling
       googlesheets4) # lets us read data directly from Google sheets


# 2. Read in some data ---------------------------------------------------------

## Let's read in some EXCHANGE data we've already collected so we can play with 
## it: 

## First, set the path where data is (it's a URL in this case)
loi_path <- "https://docs.google.com/spreadsheets/d/1Osig5zxzW3l9z_1Bb0zNW2tfTdJ60hsh78qMvjgclQE/edit#gid=0"

loi_path2 <- "/Users/homo761/Library/CloudStorage/OneDrive-PNNL/Documents/R/EC1/Gravimetric_water_and_LOI - Sheet1.csv"

#R likes having csv files vs xcl files because it can read them more easily
## Second, read data in to the R environment
## The first time you run this, you will need to jump through some hoops. 
# <- or = allows you to name and assign data
loi_raw <- read_csv(loi_path2)

## Now let's look at our dataset, which is stored as a 'tibble'

## You can look at the first several rows just by calling the object
loi_raw

## You can look at all the column names as
colnames(loi_raw)


# 3. Let's clean up our dataset ------------------------------------------------

## There are some columns here we need and some we do. To select which columns 
## you want to keep, you can use select

## First, to figure out how select works and what arguments it takes, run this:
?select
## you can put a ? in front of any function and it will open help to tell you
## what it does and how to properly use it

## One other thing to know is '%>%' which is called a pipe operator, because it
## pipes data from one function to the next. enter data name %>% and then what 
# you want to change/add/edit etc..

## For instance, if I wanted to select just one column from our data I would do this: 
loi_raw %>% 
  select(gwc)

## Or, what if you wanted to remove a single column? Just put '-' (minus) in front
loi_raw %>% 
  select(-date_started)

## This is where we're going to get a little fancy, but don't worry about how
## this works for now, you won't need to do this often: 
loi_raw %>% 
  separate(sample_id, into = c("campaign", "kit_id", "transect_location"))

## allows for seperation of data within data, for XCHANGE data is named after 
## campaign, kit_id, and transect_loaction

## All the operations above haven't been changing our data, just making subsets
## that we don't save.  To save to a new object, you use '<-'

## If I wanted to create a new dataset with only a couple columns, using
## the separate call above, I'd do this: 

loi_subset <- loi_raw %>% 
  separate(sample_id, into = c("campaign", "kit_id", "transect_location")) %>% 
  select("kit_id", "transect_location", "gwc", "loi")

## Look at the new dataset, which is now simpler and has all the things we need 
## to make some sweet plots
loi_subset2 <- loi_subset %>%
  mutate(gwc = as.numeric(str_replace_all(gwc, "%", ""))) %>%
mutate(loi = as.numeric(str_replace_all(loi, "%", ""))) 

## Let's make a plot where we look at how gwc and loi relate
ggplot(data = loi_raw, aes(x = gwc, y = loi)) + 
  geom_point()

## ggplot is how you plot in tidyverse, plot is how you plot in R data is 
## numerical, logical, or character and change be changed using as. command
## aes = aesthetica and allows you to change how the graph looks like axis, title,
## point size etc. and geom_point makes a point graph

## That's fine, but we have more info we can add, like transect_location.
ggplot(data = loi_subset, aes(x = gwc, y = loi, color = transect_location)) + 
  geom_point()

## Now, let's change the axis labels to be more descriptive
ggplot(data = loi_subset2, aes(x = gwc, y = loi, color = transect_location)) + 
  geom_point() + 
  labs(x = "Gravimetric water content (%)", y = "Loss on ignition (%)")

## Next, let's make things look nicer. Here I'm going to plot all the points
## twice: the first layer are all the same color and bigger, the second layer
## are smaller and colored based on transect_location
ggplot(data = loi_subset2, aes(x = gwc, y = loi)) + 
  geom_point(size = 3) + 
  geom_point(aes(color = transect_location), size = 2) + 
  labs(x = "Gravimetric water content (%)", y = "Loss on ignition (%)")
  
# A couple final tweaks to change the aesthetics to something I like better 
ggplot(data = loi_subset2, aes(x = gwc, y = loi)) + 
  geom_point(size = 3, alpha = 0.5) + 
  geom_point(aes(color = transect_location), size = 2, alpha = 0.6) + 
  labs(x = "Gravimetric Water Content (%)", y = "Loss on Ignition (%)") +
  theme_bw()

##  when in doubt google it out! swirl stat can help!

