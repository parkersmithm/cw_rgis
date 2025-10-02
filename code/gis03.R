if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse,
               sf,
               mapview,
               here)


# spatial join ------------------------------------------------------------

##point vector
sf_site <- readRDS("data/sf_finsync_nc.rds")


##polygon vector
sf_nc_county <- readRDS("data/sf_nc_county.rds")

## st_join() evaluates two geometry layers
sf_site_join <- st_join(x = sf_site,
        y = sf_nc_county)

##check how it works
sf_site_join

sf_one <- sf_site %>% 
  slice(1)

mapview(sf_nc_county) + mapview(sf_one)


sf_site_guilford <- sf_site_join %>% 
  filter(county == "guilford")


sf_nc_guilford <- sf_nc_county %>% 
  filter(county == "guilford")



sf_str_guilford <- readRDS(here("data/sf_stream_gi.rds"))

ggplot() +
  geom_sf(data = sf_nc_guilford) +
  geom_sf(data = sf_str_guilford,
          color = "steelblue") +
  geom_sf(data = sf_site_guilford,
          color = "salmon")


## Exercise
# count the number of sites in each county
# identify the county that has the most sites
# function "n()" may be useful to count rows

df_n <- sf_site_join %>% 
  as_tibble() %>%
  group_by(county) %>% 
  summarize(count = n()) %>%
  arrange(desc(count))

##Exercise
# sf_nc_county -- this is a "geospatial" object
# df_n - the number of sites by county
# combine them with left_join()
sf_nc_n <- left_join(sf_nc_county, df_n, by="county") %>% 
  mutate(count = ifelse(is.na(count), 
                        0, 
                        count))


## mapping
ggplot() +
  geom_sf(data = sf_nc_n,
          aes(fill = count))



# geometric analysis ------------------------------------------------------

#length calculation
## change CRS first!
sf_str_proj <- st_transform(sf_str_guilford, 
             crs = 32617)

v_str_l <- st_length(sf_str_proj)
head(v_str_l)


sf_str_w_len <- sf_str_guilford %>% 
  mutate(length = as.numeric(v_str_l))


ggplot() +
  geom_sf(data = sf_str_w_len, 
          aes(color = length))

# area calculation
sf_nc_county_proj <- st_transform(sf_nc_county, 
                                  crs = 32617)

v_area<- st_area(sf_nc_county_proj)

sf_nc_county_w_area <- sf_nc_county %>% 
  mutate(area = as.numeric(v_area))


ggplot() +
  geom_sf(data = sf_nc_county_w_area,
          aes(fill = area))


#Exercises
#1
# Load sf_quakes.rds from the data folder using readRDS(). This file should have been created through the exercise in Chapter 2.
# Assign the loaded object to sf_quakes.
# Read (a polygon for New Zealand) with readRDS(data/sf_nz.rds) and assign the resulting object to sf_nz.
# Visualize the point and polygon layers with mapview(sf_nz) + mapview(sf_quakes).
# Perform a spatial join between the quake object sf_quakes and the New Zealand polgyon object sf_nz.
# Assign the resulting joined object to sf_quakes_join.
# In sf_quakes_join, earthquake events that occurred outside New Zealand should have NA in the fid column. Use drop_na(sf_quakes, fid) to remove quakes that occurred outside New Zealand, and assign the resulting object to sf_quakes_nz.
# Count the number of earthquake events in New Zealand by counting the number of rows in sf_quakes_nz. Use nrow() function to count.
sf_quakes <- readRDS("data/sf_quakes.rds")

sf_nz <- readRDS("data/sf_nz.rds")

mapview(sf_nz) + mapview(sf_quakes)

sf_quakes_join <- st_join(x = sf_quakes,
                          y = sf_nz)
head(sf_quakes_join)

sf_quakes_nz <- drop_na(sf_quakes_join, fid)

nrow(sf_quakes_nz)
#Answer = 3

#2
# Using sf_site_join, calculate the number of survey sites in each county. Use dplyr::group_by() to group by county.
# Then use dplyr::summarize() to create a new column n containing the count of survey sites. Assign the resulting object to sf_n_site.

sf_n_site <- sf_site_join %>% 
  group_by(county) %>%
  summarize(n = n())


#3
# From sf_n_site, retain only counties with more than 10 survey sites using dplyr::filter().
# Assign the subsetted object to sf_n10.

sf_n10 <- sf_n_site %>% 
  filter(n>10)

#4
# Create a layered (stacked) map showing both sf_n_site and sf_n10. Use geom_sf() function.
# Plot sf_n_site polygons in grey to show all counties with at least one site.
# Overlay sf_n10 polygons in salmon to highlight counties with more than three sites.

ggplot() +
  geom_sf(data = sf_nc_county) +
  geom_sf(data = sf_n_site,
          color = "grey") +
  geom_sf(data = sf_n10,
          color = "salmon") +
  theme_bw()
  