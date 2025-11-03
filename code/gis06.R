
# Vector-raster interactions ----------------------------------------------


if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               sf,
               here,
               terra,
               tidyterra,
               exactextractr)

## finsync survey site
sf_site <- readRDS(here("data/sf_finsync_nc.rds"))

## county polygons
sf_nc_county <- readRDS(here("data/sf_nc_county.rds"))

## precipitation raster
spr_prec_nc <- rast(here("data/spr_prec_nc.tif"))



# pointwise extraction ----------------------------------------------------

# Let’s perform point-wise extraction of precipitation values at survey site 
# locations. To explore the spatial patterns and get a sense of the data, we 
# will first visualize the raster precipitation data using 
# tidyterra::geom_spatraster() and overlay the survey sites 
# using ggplot2::geom_sf().

ggplot() +
  geom_spatraster(data = spr_prec_nc) +
  geom_sf(data = sf_site) +
  scale_fill_viridis_c() + # change color palette for raster
  theme_bw()



# To extract raster values at the survey sites, we will use the 
# terra::extract() function. This function retrieves the values of one or 
# more raster layers at the locations specified by spatial vector data, 
# such as points or polygons. In our case, it will return the precipitation 
# value at each survey site location.
# 
# Since the output of terra::extract() with bind = TRUE is a class of 
# SpatVector that includes geometry but is not yet an sf object, we convert it 
# back to an sf object using sf::st_as_sf() (using %>%, these steps can be 
#                                            done in one step).


(sf_site_prec <- extract(x = spr_prec_nc, 
                         y = sf_site,
                         bind = TRUE) %>%
  st_as_sf())



ggplot() +
  geom_sf(data=sf_nc_county,
          fill="grey") +
  geom_sf(data = sf_site_prec,
          aes(color = precipitation)) +
  scale_color_viridis_c() +
  theme_bw()




# zonal statistics --------------------------------------------------------

## transform the sf_site to 32617; use st_transform

## transform spr_prec_nc to 32617; use project()

sf_county_nc_proj <- sf_nc_county %>% st_transform(crs=32617)


spr_prec_nc_proj <- project(x = spr_prec_nc,
                             y = "EPSG:32617",
                             method = "bilinear")

spr_prec_nc_proj

# With both layers now in a common projected CRS, 
# we can proceed with zonal statistics, ensuring that area-weighted calculations 
# are geometrically accurate.
# 
# In the code below, exact_extract() is used to calculate the mean precipitation 
# within each county polygon (sf_nc_county_proj) from the reprojected raster 
# spr_prec_nc_proj. The argument fun = "mean" specifies that we want to compute 
# the simple mean of all raster cell values that overlap each polygon. 
# The append_cols = TRUE option ensures that all original attributes from 
# the sf_nc_county_proj object are retained in the output, which makes it 
# easier to link the results back to the spatial features.

df_prec_county <- exact_extract(x = spr_prec_nc_proj,
                                y = sf_county_nc_proj,
                                fun = "mean",
                                append_cols = TRUE) %>% 
  as_tibble() %>% 
  rename(precipitation = mean)

#looking at variation instead of mean
df_prec_county_sd <- exact_extract(x = spr_prec_nc_proj,
                                y = sf_county_nc_proj,
                                fun = "stdev",
                                append_cols = TRUE) %>% 
  as_tibble()


sf_nc_county_prec <- left_join(sf_nc_county,
                               df_prec_county, 
                               by = "county")



ggplot() +
  geom_sf(data = sf_nc_county_prec,
          aes(fill = precipitation)) +
  scale_fill_viridis_c()




# buffer analysis ---------------------------------------------------------

#transform crs
sf_site_proj <- sf_site %>% 
  st_transform(crs = 32617)


#create buffers around the points

sf_site_buff_proj <- sf_site_proj %>%
  st_buffer(dist=10000) #unit is meters

ggplot() +
  geom_sf(data = sf_county_nc_proj) +
  geom_sf(data = sf_site_buff_proj) +
  geom_sf(data = sf_site_proj)



## get mean precipitation for each site buffer

## link these values to site layer

## map the precipitation value at each site

buff_prec_nc_proj <- exact_extract(x = spr_prec_nc_proj,
                                   y = sf_site_buff_proj,
                                   fun = "mean",
                                   append_cols = TRUE) %>%
  as_tibble() %>%
  rename(precipitation = mean)

sf_site_prec_buff <- sf_site %>%
  left_join(buff_prec_nc_proj, by = "site_id")


ggplot() + 
  geom_sf(data = sf_county_nc_proj) +
  geom_sf(data = sf_site_prec_buff, 
          aes(color = precipitation)) + 
  scale_color_viridis_c()

## identify top three precipitation sites
sf_site_prec_buff %>% 
  arrange(desc(precipitation)) %>%
  slice(1:3)

