if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               terra,
               tidyterra,
               mapview,
               stars,
               here)

# read/export raster data -------------------------------------------------

## read geotiff
(spr_ex <- rast(here("data/spr_example.tif")))


## export geotiff
writeRaster(spr_ex,
            filename = here("data/spr_elev.tif"),
            overwrite = TRUE)

## mapping
ggplot() +
  geom_spatraster(data = spr_ex)

## mapview functions
star_ex <- st_as_stars(spr_ex)
class(spr_ex)
class(star_ex)


mapview(star_ex)



# raster data type --------------------------------------------------------


v_elev <- values(spr_ex)
head(v_elev, 10)

na.omit(v_elev) %>% 
  mean()


## extract data from given location
## xy specifies longitude/latitude
xy <- cbind(6.0000, 50.0000)
extract(spr_ex, xy)


## xy can be multiple sites
df_point <- tibble(lon = c(6, 5.9), lat = c(50, 49.96))
df_point

extract(spr_ex, 
        y = df_point)

## discrete raster
(spr_for <- rast(here("data/spr_forest_nc.tif")))

ggplot() +
  geom_spatraster(data = spr_for)

unique(spr_for)
v_binary <- values(spr_for)


mean(v_binary)

## discrete, coded values
spr_land <- rast(here("data/spr_land_reclass.tif"))

unique(spr_land)

## UNCG Sullivan building coords
sull <- cbind(-79.8063, 36.0701)

## ID the land use type for Sullivan building
terra::extract(spr_land,
               sull)
## answer: 1100 -- urban


# reclass -----------------------------------------------------------------

##create a conversion matrix
cm <- cbind(c(0, 1001, 1010, 1100),
      c(0, 1, 0, 0))

cm


## reclass
spr_bin <- classify(spr_land,
         rcl = cm)
unique(spr_bin)

v_bin <- values(spr_bin)
mean(v_bin)



# exercise ----------------------------------------------------------------

#1. Read geoTIFF file
# Load the raster file spr_prec_ncne.tif from the data folder using the terra::rast() function.
# Assign the result to a new object named spr_prec_ncne.

spr_prec_ncne <- terra::rast(here("data/spr_prec_ncne.tif"))

#2. Inspect raster properties
# In your own words, describe the following based on the output:
# Number of rows and columns (i.e., the raster dimensions)
# Resolution (size of each cell in degrees)
# Spatial extent (minimum and maximum coordinates)
# Coordinate Reference System
# Minimum and maximum precipitation values

(spr_prec_ncne)

# there are 162 rows and 532 columns
# the resolution is 0.0083333 degrees for both the longitude and latitude
# for the longitude, the min coord is -79.89181 and the max coord is -75.45847. 
# for latitide, the min coord is 35.24153 and the max coord is 36.59153
# the CRS is geodectic and is WGS 84
# the min precipitation is 1063.1 and the max precip is 1501.5

#3. Visualize the raster
# Use ggplot2 with tidyterra::geom_spatraster() to create a basic map of the precipitation raster.

ggplot() +
  geom_spatraster(data = spr_prec_ncne)


#4. Extract values

# Read the fish sampling site data from data/sf_finsync_nc.rds and assign it to sf_site.
# 
# st_coordinates(sf_site) will extract coordinates from the sf object as a dataframe. Assign this data frame to df_xy.
# 
# Using terra::extract() function with inputs spr_land and df_xy, identify land use type at each sampling site. Assign the result to df_land.
# 
# Identify the most common land use type at these sampling sites.

sf_site <- readRDS(here("data/sf_finsync_nc.rds"))

df_xy <- st_coordinates(sf_site)

df_land <- terra::extract(spr_land,
                          y = df_xy)
df_land %>% 
  group_by(df_land$`PROBAV_LC100_global_v3.0.1_2015-base_Discrete-Classification-map_EPSG-4326`) %>% 
  summarize(count = n())

table(df_land)

# most common land use type = 1001 -- forest

#5. Reclassify
# Reclassify the values of spr_land to have binary entries urban = 1 and non-urban = 0. After reclassification, assign the layer to spr_urban.
# 
# Calculate the proportion of urban land use in NC.

urban <- cbind(c(0, 1001, 1010, 1100),
               c(0, 0, 0, 1))


df_land_reclass <- classify(spr_land,
                            rcl = urban)
unique(df_land_reclass)

land_bin <- values(df_land_reclass)
mean(land_bin)

#mean urban land use = 3.2%

