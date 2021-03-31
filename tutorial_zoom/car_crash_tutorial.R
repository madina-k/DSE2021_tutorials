# Seminar Data Science for Economics Spring 2021



# PRELIMINARIES -----------------------------------------
# Install packages that are needed today
# install.packages(c("tidyverse", "sf", "maps", "ggmap", "osmdata", "viridis"))



# At first we just need tidyverse packages. Read more about all the packages that are included in the tidyverse at https://www.tidyverse.org/packages/

library(tidyverse)
# tidyverse_update()


# The goals for today:
## Learn how to provide descriptive statistics with dplyr functions
## Merge data, including geo-spatial merge
## Visualize geo-spatial data


# GETTING THE DATA  -----------------------------------------
# I downloaded the FARS data on all fatal traffic accidents that happened in 2018 in the U.S. from https://www.nhtsa.gov/filebrowser/download/280571
# I saved two csv files for convinience into the data folder of this tutorial
# You can find the full information about each variable in each dataset by looking in the User's Manual https://crashstats.nhtsa.dot.gov/Api/Public/ViewPublication/812827

# Read crash-level data
fars_crash <- read_csv("./tutorial_zoom/data/fars2018/accident.CSV")

# Read person-level data
fars_person <- read_csv("./tutorial_zoom/data/fars2018/Person.CSV")

# I use a relative path. I can do this since I created a project with the root directory in the main folder that contains all the tutorials.

# MERGE CRASH AND PERSON DATASETS ---------------------------------------

glimpse(fars_crash)

glimpse(fars_person)

intersect(names(fars_crash), names(fars_person)) # Get the columns in common between the two datasets

# Left join silently merges based on columns that appear in both datasets.
fars <- left_join(fars_crash, fars_person)

nrow(fars) == nrow(fars_person) # It has the same number of obs as fars_person

# Look at this: the join operation repeated the case-level information for each person. Hence, fars dataset is now at person-level resolution
fars %>% select(ST_CASE, LONGITUD, LATITUDE, PER_NO, SEXNAME)

# To return back fars dataset to case-level resolution but without loosing information about each person, we can nest personal information about each crash participant
fars <- fars %>%
  group_by_at(vars(names(fars_crash))) %>%
  nest() %>%
  ungroup()

# Why group_by_at(vars(.))?
# Why ungroup at the end?

nrow(fars) == nrow(fars_crash) # Now it has the same number of obs as fars_crash


# fars is different from fars_crash by only one column
setdiff(names(fars), names(fars_crash))

# let's rename the data column into something more meaningful
fars <- fars %>%
  rename(person_data = data)

names(fars)
# now the last column is called person_data


# INSPECTING THE DATA -------------------------------------

class(fars)
# fars is a dataframe but it is also a tibble. Apparently, read_csv() function from readr package automatically loaded the data in a tibble class.

names(fars)
# fars has 91 variables which contain information about circumstances of each crash and 1 column containing dataframes with personal information  for people involved in the crash


fars %>% glimpse()
# person_data column is in fact a list. We can store it inside fars table because it is a tibble

# Now let's inspect person_data column
fars %>% select(person_data) %>%  head()
fars$person_data[[1]] %>%  glimpse()


# SUMMARY STATISTICS  --------------------------------

# N of crashes
fars %>% count() # Correct
fars_crash %>% count() # Correct
fars_person %>% count() # Incorrect (fars_person is at the person-level resolution)

# N obs per light conditions LGT_CONDNAME
fars %>% count(LGT_CONDNAME) # Correct
count(fars, LGT_CONDNAME) # Correct (equivalent without the pipe %>%  operator)

fars %>% group_by(LGT_CONDNAME) %>% count() # Correct
fars %>% group_by(LGT_CONDNAME) %>% summarise(n=n()) # Correct





# N obs for accidents that involve pedestrians vs. those that do not
# Use PEDS column which records how many pedestrians are involved in the crash

# Which of the following is correct [Poll]
fars %>% summarise(sum(PEDS)) # ?
fars %>% count(PEDS>0) # ?
fars %>% group_by(PEDS>0) %>% summarise(n = n()) # ?
fars_person %>% count(PEDS>0) # ?


# Count N obs by number of pedestrians involved in the crash
fars %>% count(PEDS)

# Count N obs with missing coordinates
fars %>% count(is.na(LONGITUD))
fars %>% count(is.na(LATITUDE))

# It seems like there are no missing coordinates, but if you look in the manual, you realize that missing Longitude and Latitude are coded with values like 77.7777000, 88.8888000, 99.9999000 for Latitude and 777.777000, 888.888000, 999.999000 for Longitude.

# PRO TIP: ALWAYS READ THE CODEBOOK

fars %>%
  count(LONGITUD>777)

fars %>%
  count(LATITUDE>77)

# FATALS is a variable that records number of fatalities (deaths)
# Calculate average number of dead per accidents with and without pedestrians

# Please write your suggestions in the chat
fars %>%
  group_by(________) %>%
  summarise(________)

# N obs per CITYNAME.

fars %>% count(CITYNAME, sort=TRUE)

fars %>%
  filter(CITYNAME != "NOT APPLICABLE", CITYNAME != "Not Reported") %>%
  count(CITYNAME, sort=TRUE)

# Question for you: Can you find which city names are repeated across different states?


# CREATING COLUMNS: MUTATE + CASE_WHEN  -------------------------------------
# Create a column which stores information on the type of the accident
# VE_FORMS counts how many vehicles were involved in the collision
# PEDS counts how many pedestrians were involved in the crash

fars <- fars %>%
  mutate(
    crash_type = case_when(
      VE_FORMS == 1 & PEDS != 0 ~ "car_to_pedestrian",
      VE_FORMS  > 1 & PEDS == 0 ~ "car_to_car",
      VE_FORMS == 1 & PEDS == 0 ~ "car_to_obstacle",
      VE_FORMS  > 1 & PEDS != 0 ~ "multi_crash"
    ))

fars %>%  count(crash_type)

# CREATING COLUMNS: MUTATE + NESTED DATA  -------------------------------------
# Let's create an indicator if accident involves  at least one death of a Black person

# Remember that we have person-level information nested within person_data column
fars %>%  select(person_data)

# Inspect: Look at the RACENAME of people involved in the first crash in our fars data
# Write your answer in the chat
fars$_____________________

# Get a TRUE/FALSE answer to whether there are any Black people involved in the first crash
"Black" %in% fars$_____________________

# Create a new variable called black_dead which stores TRUE/FALSE indicators on whether the crash involves one or more Black people

fars <- fars %>%
  mutate(black_dead = map_lgl(.x = person_data,~"Black" %in% .x$RACENAME))

# What does map_lgl() do different from just map()?
# What does .x do?

# See how many accidents involve Blacks
fars %>%  count(black_dead)




# SUBSETTING THE DATA  --------------------------------

# We want to keep only the accidents that happened in Chicago (Cook county, Illinois) and keep information on longitude-latitude, number of drunk drivers, light conditions, as well as newly created crash_type and black_dead variables.

fars_ch <- fars %>%
  filter(COUNTYNAME == "COOK (31)", STATENAME == "Illinois") %>%
  select(LONGITUD, LATITUDE, DRUNK_DR, LGT_CONDNAME, crash_type, black_dead)

fars_ch %>%  head()

# GEOPLOTTING THE DATA ------------------------------------

# sf (Simple Features) for tibbles with coordinates https://github.com/rstudio/cheatsheets/blob/master/sf.pdf

library(sf)

# a collection of maps
library(maps)

# Notice that maps package has masked map function from purrr
# We can restore it to purrr
map <- purrr::map

# a library to load and plot maps
library(ggmap)

# open street maps
library(osmdata)


# Converting to sf object ------------------------------------------------
# Converting fars dataframe/tibble into an `sf` (simple features) object. An `sf-object` is a table, just like a `data.frame`, but with an additional column called `geometry` which stores the geo-spatial attributes of each observation -- points or polygons (areas). The best part of it, is that we can apply our common tidyverse functions  an `sf` object as if it was a dataframe. For example, we can `filter()`, `mutate()`, `gather()`, etc. This is a great thing, which allows us working with geo-referenced data in a smooth fashion.

fars_sf <- fars %>%
  filter(LONGITUD < 77.7, LATITUDE <777.7) %>% # Keep only obs with present coordinates
  st_as_sf(coords = c("LONGITUD", "LATITUDE"), crs = "+proj=longlat +datum=WGS84 +no_defs", remove = FALSE)

# fars_sf has this new column called geometry
fars_sf %>% select(LONGITUD, LATITUDE, geometry) %>% head()

# and we can immediately plot our data using geom_sf() layer within ggplot
fars_sf %>%  ggplot() + geom_sf()


# Getting shapefiles for the U.S. ----------------------------------------

# This code fetches the map for the U.S. provided by maps package
usa <-  st_as_sf(map('usa', plot = FALSE, fill = TRUE))
# What is wrong?








usa <-  st_as_sf(maps::map('usa', plot = FALSE, fill = TRUE))


# Plotting the crash data together with the U.S. map
ggplot() + geom_sf(data = usa) +
  geom_sf(data=fars_sf, alpha = 0.01, size = 0.5) +
  coord_sf(xlim = c(-130, -60),
           ylim = c(20, 50))

# What do alpha = 0.01, size = 0.5 do?
# Guess what coord_sf() do?


# Convert fars_ch into an SF object -----------------------------------------
# Now you need to convert fars_ch into an sf object
fars_ch_sf <- (Complete the code)

# How would you complete the code?

# Getting maps from open street maps ---------------------------------------
ch_map <- get_map(getbb("Chicago"), maptype = "toner-background")

# Plot ch_map
ggmap(ch_map)

#Please be careful with downloading maps or data from osm. Doing constantly repeated queries or bulk downloads violate their terms of use, and you might be blocked by their server. See the terms [here](https://operations.osmfoundation.org/policies/tiles/).
# Hence, to avoid being blocked, do not forget to save the loaded maps or data on your hard drive.

# Saving objects as rds ---------------------------------------
# Save the map from OSM as an rds file
write_rds(ch_map, "./tutorial_zoom/data/maps/ch_map.rds")

# What is an rds object?


# Load the map
ch_map <- read_rds("./tutorial_zoom/data/maps/ch_map.rds")

# Plot OSM map + crashes -------------------------------------

# Plot the ch_map together with the crash data
ggmap(ch_map) +
  geom_sf(
    aes(color = as.factor(crash_type),
        fill = as.factor(crash_type),
        shape = as.factor(crash_type)),
    data = fars_ch_sf,
    inherit.aes = FALSE,
    size = 1.5)


# GEOSPATIAL MERGE ---------------------------------------

# Read Census shapefiles and data ------------------------

# Read census tract map, which I already pre-loaded and saved as an rds file in maps folder
shp_tract_sf <- read_rds("./tutorial_zoom/data/maps/tracts_sf.rds")
shp_tract_sf %>% ggplot() + geom_sf()
glimpse(shp_tract_sf)

# Read census tract statistics
demo_census_tract <- read_rds("./tutorial_zoom/data/maps/tracts_demography.rds")
glimpse(demo_census_tract)

# Note that both the shapefile and the census statistics data include the "GISJOIN" variable. This is the identifier we will use to join the two objects. We use `left_join(x,y)`, which tells R to "return all rows from x, and all columns from x and y. Rows in x with no match in y will have NA values in the new columns."

# We want to keep all rows of the `shp_tract_sf`.  Notice how we tell R which variable to join by with `by = "GISJOIN"`.

censusdata_sf <-  left_join(shp_tract_sf, demo_census_tract, by = "GISJOIN")

glimpse(censusdata_sf)

# Plot censusdata_sf -------------------------------------------------------

# Now you can plot the newly merged `censusdata_sf` to see the census tracts by the share of white residents `tract_black_s`.

# Plot the map of census tracts  colored by the share of black residents `tract_black_s`

ggplot() + geom_sf(aes(fill=tract_black_s), data = censusdata_sf )

# Let's add better coloring scheme
library(viridis)

last_plot() + scale_fill_viridis()
# I like how viridis colors go through more colors, allowing us to distinguish finer variations in the numeric values

# Plot censusdata_sf: use gather() and facet_grid() --------------------------------

# The code below shows how to use `gather()` in combination with `ggplot()` to be able to `facet_grid()` combining two (or more if you want) variables in one plot.

censusdata_sf %>%
  select(tract_black_s, tract_white_s, geometry) %>%
  gather(key, value, -geometry) %>%
  ggplot() +
  geom_sf(aes(fill=value) ) +
  facet_grid(.~key) +
  scale_fill_viridis()

# What does gather() do?
# What does facet_grid() do?

# Final plot -------------------------------------------------

# Plot the census tracts by the share of Black residents and on top of it another layer with the traffic accidents colored by presence of Black fatalities

ggplot() +
  geom_sf(aes(fill = tract_black_s), data = censusdata_sf ) +
  scale_fill_viridis() +
  geom_sf(aes(color = as.factor(black_dead)),
          data=fars_ch_sf) +
  scale_colour_manual(values=cbPalette)

# The green dots on top of the yellow-greenish background do not look very clear

# let's use our own palette of colors
cbPalette <- c("#E69F00", "#990000", "#56B4E9")

# also, let's increase the size of the dots, and add proper labels
ggplot() +
  geom_sf(aes(fill = tract_black_s), data = censusdata_sf ) +
  scale_fill_viridis() +
  geom_sf(aes(color = as.factor(black_dead)),
          data=fars_ch_sf,
          size = 2,
          alpha = 0.7) +
  scale_colour_manual(values=cbPalette) +
  labs(
    fill = "Share of black residents",
    colour= "Black fatalities",
    title = "Car crashes in 2018 Cook County (IL)")

# save the plot
dir.create("./tutorial_zoom/output")
ggsave("./tutorial_zoom/output/crashes_chicago.png", scale = 0.8)


# Spatially join fars and censusdata_sf -------------------------------
# Finally, let's combine our crash data with the data from census

glimpse(censusdata_sf)

fars_ch_demo <- st_join(fars_ch_sf, censusdata_sf[ , 19:ncol(censusdata_sf)], left = TRUE)

glimpse(fars_ch_demo)


# Run a regression ---------------------------------------------

lm(black_dead ~ tract_black_s, data = fars_ch_demo) %>% summary()



