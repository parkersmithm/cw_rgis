## ctl+shift+n makes a new rscript

if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse,
               sf,
               mapview,
               here)


# read/export vector data --------------------------------------------------

# read a shapefile (e.g., ESRI Shapefile format)
# `quiet = TRUE` just for cleaner output
sf_nc_county <- st_read(dsn = here("data/nc.shp"),
                         quiet = TRUE)

#save as shape file
st_write(sf_nc_county,
         dsn = here("data/sf_nc_county.shp"),
         append = FALSE)

#save as geopackage
st_write(sf_nc_county,
         dsn = here("data/sf_nc_county.gpkg"),
         append = FALSE)

#save as rds
saveRDS(sf_nc_county,
         file = here("data/sf_nc_county.rds"))

#read rds
sf_nc_county <- readRDS(file = here("data/sf_nc_county.rds"))


##point data 
sf_site <- readRDS(file = here("data/sf_finsync_nc.rds"))


mapview(sf_site,
        col.regions = "black", # point's fill color
        legend = FALSE) # disable legend

##take first 10 sites
sf_site10<- sf_site %>% slice(1:10)

mapview(sf_site10,
        col.regions= "black",
        legend = FALSE)

#line data
sf_str <- readRDS(file = here("data/sf_stream_gi.rds"))

mapview(sf_str,
        color = "steelblue", # line's color
        legend = FALSE) # disable legend


##take first 10 sites
sf_str10 <- sf_str %>% slice(1:10)

mapview(sf_str10,
        color = "darkblue",
        legend = FALSE)

#polygon
mapview(sf_nc_county,
        color="white",
        col.regions="orchid",
        legend= FALSE)


#highlight guilford county
sf_nc_gi <- sf_nc_county %>% 
  filter(county == "guilford")

mapview(sf_nc_gi,
        col.regions = "chartreuse4",
        legend = FALSE)


#map with ggplot
ggplot() +
  geom_sf(data = sf_nc_county) +
  geom_sf(data = sf_str) +
  geom_sf(data = sf_site) ##Not a good map :(

## a little better
ggplot() +
  geom_sf(data = sf_nc_gi) +
  geom_sf(data = sf_str)



# Exercises ---------------------------------------------------------------

#1.Read stream line data for Ashe county 
##Load the stream line data file sf_stream_as.rds located in the data folder. Use readRDS().
 sf_str_as <- readRDS(file = here("data/sf_stream_as.rds"))


#2. Check coordinate reference systems (CRS) (ref: Section 3.2.2)
##Print objects tocheck the CRS for both sf_str_as and sf_nc_county.
##Determine whether they share the same CRS.
print(sf_str_as)
print(sf_nc_county)

##CRS for sf_str_as: WGS 84
##CRS for sf_nc_county: WGS 84

###geodectic CRS for both is WGS 84

#3. Export to GeoPackage format (ref: Section 3.2.2)
##Save sf_str_as as a GeoPackage file named sf_stream_as.gpkg in the data folder. Use sf::st_write().

###SKIP not working?
# st_write(sf_str_as, 
#          dsn = "data/sf_stream_as.gpkg",
#          append = FALSE)


#4. Map streams and county boundaries (ref: Section 3.2.6)
##Create a map displaying both: North Carolina county boundaries from sf_nc_county, 
##and Ashe County stream lines from sf_str_as. Use ggplot2::ggplot() with multiple geom_sf() layers.

ggplot() +
  geom_sf(data = sf_nc_county) +
  geom_sf(data = sf_str_as) ##not a good map again :(

#5. Subset county layer to Ashe county and remap (ref: Section 3.2.5 and Section 3.2.6)
##Subset the sf_nc_county object to include only Ashe County. Use dplyr::filter() for subsetting. Assign the result to sf_nc_as.
##Then, recreate the map showing only sf_nc_as and sf_str_as
sf_nc_as <- sf_nc_county %>%
  filter(county == "ashe")

ggplot() +
  geom_sf(data = sf_nc_as) +
  geom_sf(data = sf_str_as, color = "dodgerblue")

