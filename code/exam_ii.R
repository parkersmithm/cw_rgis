# Exam II
# Submitted by: Maya Parker-Smith
# Date: November 4, 2025

# By submitting this exam on time, you will obtain 55 points
# 15 questions in total, with each worth 3 points
# Points will be awarded if your code produces the expected result(s)

if (!require(pacman)) install.packages("pacman")
library(pacman)

# call packages -----------------------------------------------------------

# Execute the following lines of code to call packages
p_load(tidyverse,
       sf,
       terra,
       exactextractr,
       tidyterra)

# To answer the following questions, use the data below:
df_site <- read_csv("data/data_finsync_nc.csv") %>% 
  distinct(site_id, 
           lon, 
           lat)

sf_nc_county <- readRDS("data/sf_nc_county.rds")

# vector data analysis ----------------------------------------------------

# Q1. 
# `df_site` currently has no coordinate reference system (CRS). 
# Convert it to an `sf` object and assign the WGS 84 CRS (EPSG: 4326). 
# Save the resulting object as `sf_site`.
sf_site <- df_site %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326)

# Q2.
# From `sf_nc_county`, select only the county polygons of the following counties: 
#   "guilford", "randolph", "davidson", and "forsyth". 
# Save the result as `sf_four`.
sf_four <- sf_nc_county %>% 
  filter(county %in% c("guilford", 
                       "randolph", 
                       "davidson", 
                       "forsyth"))

# Q3. 
# Perform a spatial join to identify sites in `sf_site` that fall within 
#   the four selected counties stored in `sf_four`. 
# Remove any rows without a `county` value and save the result as `sf_site_four`.

sf_site_four <- st_join(x = sf_site,
                        y = sf_four) %>% 
  na.omit()


# Q4.
# Create a map showing the four selected counties (`sf_four`) 
#   and the sampling sites (`sf_site_four`) overlaid on the same plot. 

ggplot() +
  geom_sf(data = sf_four) +
  geom_sf(data = sf_site_four)
  

# Q5.
# Calculate the pairwise distances among all sites in `sf_site_four`
#   with the appropriate CRS, UTM Zone 17N (EPSG: 32617) 
#   so that distances are measured in meters. 
# Then, find the maximum distance among all site pairs.

sf_site_four_distance <- sf_site_four %>%
  st_transform(crs = 32617) %>%
  st_distance()



max(sf_site_four_distance)

# ENTER YOUR ANSWER HERE: 71724.58 m


# raster data analysis ----------------------------------------------------

# Q6. 
# The raster file "spr_land_reclass.tif" in the "data" folder 
#   contains reclassified land-cover data, 
#   where pixel values represent land-cover types as follows:
#   1001 = forest
#   1010 = crop
#   1100 = urban
#   0 = other
# 
# Load this raster as `spr_land` and display the unique land-cover codes it contains.

spr_land <- rast(here("data/spr_land_reclass.tif"))

unique(spr_land$code)


# Q7. 
# Reclassify the raster `spr_land` to create a new raster object `spr_crop` 
#   that highlights only cropland areas. 
# Use the following reclassification rules:
#   1001 = 0 (forest)
#   1010 = 1 (crop)
#   1100 = 0 (urban)
#   0 = 0 (other)

crop_class <- cbind(c(1001, 1010, 1100, 0),
                    c(0, 1, 0, 0))

spr_crop <- classify(spr_land,
                     rcl = crop_class)

# Q8. 
# Crop the cropland raster (`spr_crop`) to the extent of the four selected counties 
# (`sf_four`; "guilford", "randolph", "davidson", and "forsyth")
# Save the resulting cropped raster as `spr_crop_four`.

spr_crop_four <- crop(x = spr_crop,
                      y = sf_four)

# Q9. 
# Create a map showing the cropped cropland raster (`spr_crop_four`) 
#   overlaid with the four counties (`sf_four`). 
# Use a semi-transparent overlay for the counties.

ggplot() +
  geom_spatraster(data = spr_crop_four) +
  geom_sf(data = sf_four,
          color = "black",
          alpha = 0.20)

# Q10. Calculate the proportion of cropland pixels within the four counties 
#   from the cropped raster (`spr_crop_four`). 
# Since cropland pixels are coded as 1 and others as 0, the mean gives the proportion.
#

crop_val <- values(spr_crop_four)
mean(crop_val)

# ENTER YOUR ANSWER HERE: 0.077
# (round your answer to third decimal places, e.g., 0.021)


# raster-vector interaction -----------------------------------------------

# Q11. 
# The raster file "spr_tmp_nc.tif" in the "data" folder contains 
#   annual mean temperature (°C) data for North Carolina. 
# Load this raster and extract the temperature values 
#   at each sampling site in `sf_site`. 
# Then, identify how many sites have temperature values greater than 16°C.
#

spr_tmp_nc <- rast(here("data/spr_tmp_nc.tif"))

sf_tmp_nc_site <- extract(x = spr_tmp_nc,
                          y = sf_site,
                          bind = TRUE) %>%
  st_as_sf()

nrow(sf_tmp_nc_site[sf_tmp_nc_site$temperature > 16, ])

# ENTER YOUR ANSWER HERE: 24



# Q12. Create 3-km buffers around each site in `sf_site_four` (see Q3). 
# Be sure to first transform the coordinate reference system to UTM Zone 17N (EPSG: 32617) 
# so that the buffer distance is measured in meters.

sf_site_dist <- st_transform(sf_site_four,
                             crs = 32617)

sf_buff_proj <- sf_site_dist %>% 
  st_buffer(dist = 3000)

# Q13. Project the cropped cropland raster (`spr_crop_four`) 
# to the same UTM coordinate reference system (EPSG: 32617). 
# Use an appropriate re-sampling method in light of the raster data type.
spr_crop_proj <- project(x = spr_crop_four,
                              y = "EPSG:32617",
                              method = "near")


# Q14. Create a map displaying the projected cropland raster (`spr_crop_proj`) 
# with 3-km site buffers (`sf_buff_proj`) overlaid.

ggplot() +
  geom_spatraster(data = spr_crop_proj) +
  geom_sf(data = sf_buff_proj,
          alpha = 0.25)



# Q15. Calculate the proportion of cropland within each 3-km site buffer. 
# Store the result as `df_crop_frac`, and identify the `site_id` 
# with the highest cropland fraction.


df_crop_frac <- exact_extract(x = spr_crop_proj,
                              y = sf_buff_proj,
                              fun = "mean",
                              append_cols = TRUE,
                              progress = FALSE) %>%
  as_tibble() %>%
  rename(crop_count = mean)

df_crop_frac %>% 
  arrange(desc(crop_count))

## Site_id with highest cropland fraction: "finsync_nrs_nc-10113" (a Guilford county site)

                         