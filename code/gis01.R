## Intro to Coordinate Systems
if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               sf,
               mapview,
               here)
##Read fish data
df_fish <- read_csv(here::here("data/data_finsync_nc.csv"))


#create map coordinates dta
sf_site <- df_fish %>% 
  distinct(site_id, 
           lon, 
           lat) %>% 
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326) ## crs = 4326 is most common when simply mapping, but will not provide correct distances due to the 2D nature

df_fish
sf_site

##View map
mapview(sf_site, 
        legend = FALSE)

##export the data
saveRDS(sf_site, 
        file = here::here("data/sf_finsync_nc.rds"))


# conversion from geodectic to projected ----------------------------------

sf_ft_wgs <- sf_site %>% 
  slice(c(1, 2))

sf_ft_utm <- sf_ft_wgs %>% 
  st_transform(crs = 32617)

mapview(sf_ft_wgs, 
        legend = FALSE)

st_distance(sf_ft_utm)



# Exercises ---------------------------------------------------------------

#1
##load quakes data
df_quakes <- as.tibble(quakes)

#print quakes to see latitude and longitude
print(df_quakes)

#2
#convert to an sf object
sf_quakes <- df_quakes %>% 
  st_as_sf(coords = c("long", "lat"),
           crs = 4326)
#view map
mapview(sf_quakes,
        legend = FALSE)

#3
#select first 2 sites
sf_ft_quakes <- sf_quakes %>% 
  slice(c(1,2))

#transform to utm
sf_ft_quakes_proj <- sf_ft_quakes %>%
  st_transform(crs = 32760)


mapview(sf_ft_quakes,
        legend = FALSE)

#calculate distance
st_distance(sf_ft_quakes_proj)
#distance = ~66km


#4
#save file
saveRDS(sf_quakes, 
        file = here::here("data/sf_quakes.rds"))


