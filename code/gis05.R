if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               terra,
               tidyterra,
               mapview,
               stars,
               here)

# crop --------------------------------------------------------------------

(spr_prec <- rast(here("data/spr_prec_us.tif")))

# ggplot() +
#   geom_raster(data=spr_spec)

#ext returns extent of the layer
ext(spr_prec)


# crop function, direct entry lat/lon
# order matters c(xmin, xmax, ymin, ymax)
spr_crop <- crop(x = spr_prec,
     y = c(-80, -75, 34, 37))

ext(spr_crop)

# check coverage visually
sf_nc_county <- readRDS(here("data/sf_nc_county.rds"))


ggplot() +
  geom_spatraster(data = spr_crop) +
  geom_sf(data = sf_nc_county, 
          alpha = 0.25)


# use vector layer as a mask layer
# no need to enter raw lat/lon values directly
# 
spr_prec_nc <- crop(x = spr_prec, 
                    y = sf_nc_county)


ggplot() +
  geom_spatraster(data = spr_prec_nc) +
  geom_sf(data = sf_nc_county, 
          alpha = 0.25)




# merge -------------------------------------------------------------------


spr_nw <- rast(here("data/spr_prec_ncnw.tif")) # Northwest NC
spr_ne <- rast(here("data/spr_prec_ncne.tif")) # Northeast NC
spr_sw <- rast(here("data/spr_prec_ncsw.tif")) # Southwest NC
spr_se <- rast(here("data/spr_prec_ncse.tif")) # Southeast NC


ggplot() +
  geom_spatraster(data = spr_nw) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)

# merge two raster datasets into one 
spr_n <- merge(spr_nw, spr_ne)

ggplot() +
  geom_spatraster(data = spr_n) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)

#compare the extent 
ext(spr_nw)
ext(spr_n)


## merging more than 2 raster layers
# 1st step: create a list of raster layers
list_spr <- list(spr_ne,
                 spr_nw,
                 spr_se,
                 spr_sw)

spr_col <- sprc(list_spr)

spr_merge <- merge(spr_col)


## check output after merging

ggplot() +
  geom_spatraster(data = spr_merge) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)


writeRaster(spr_merge, 
            filename = here("data/spr_prec_nc.tif"),
            overwrite = TRUE)



# stack -------------------------------------------------------------------

spr_prec_rast <- rast(here("data/spr_prec_nc.tif"))
spr_tmp_nc <- rast(here("data/spr_tmp_nc.tif"))


spr_pt_nc <- c(spr_prec_nc,
               spr_tmp_nc)

print(spr_pt_nc)

## access each layer separately 
spr_pt_nc$precipitation
spr_pt_nc$temperature



# reproject ---------------------------------------------------------------

print(spr_prec_nc)


#reprojection for raster
spr_prec_nc_proj <- project(x = spr_prec_nc,
                            y = "EPSG:32617",
                            method = "bilinear")




# Exercise ----------------------------------------------------------------


#1. 
# Load four regional temperature tiles: 
# spr_tmp_ncnw.tif (Northwest), 
# spr_tmp_ncne.tif (Northeast), 
# spr_tmp_ncsw.tif (Southwest), 
# spr_tmp_ncse.tif (Southeast). 
# Assign them to separate objects (object names of your choice).
# Merge all four using:
#   A list of rasters.
# terra::sprc() to create a SpatRasterCollection.
# merge() to combine all tiles into spr_merge.
# Plot spr_merge over sf_nc_county to confirm coverage. 
# Use ggplot2::ggplot() with geom_spatraster() and geom_sf() 
# (Tip: Use alpha = 0.25 in geom_sf() to make the polygons transparent.)

spr_tmp_ncnw <- rast(here("data/spr_tmp_ncnw.tif"))
spr_tmp_ncne <- rast(here("data/spr_tmp_ncne.tif"))
spr_tmp_ncsw <- rast(here("data/spr_tmp_ncsw.tif"))
spr_tmp_ncse <- rast(here("data/spr_tmp_ncse.tif"))

spr_tmp_list <- list(spr_tmp_ncnw,
                     spr_tmp_ncne,
                     spr_tmp_ncsw,
                     spr_tmp_ncse)

spr_tmp_col <- sprc(spr_tmp_list)


spr_tmp_merge <- merge(spr_tmp_col)

ggplot() +
  geom_spatraster(data = spr_tmp_merge) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)

#2. 
# Crop raster to a defined extent (ref: Section 4.3.1)
# Select county camden from sf_nc_county (from data/sf_nc_county.rds) and assign it to sf_camden.
# Inspect its spatial extent (sf_camden) using terra::ext().
# Crop spr_merge to the extent of the camden county using sf_camden (use terra::crop())
# Assign the cropped raster to spr_tmp_camden.
# Plot spr_tmp_camden over sf_camden to confirm coverage. 
# Use ggplot2::ggplot() with geom_spatraster() and geom_sf() 
# (Tip: Use alpha = 0.25 in geom_sf() to make the polygons transparent.)


sf_camden <- sf_nc_county %>% 
  filter(county == "camden")

ext(sf_camden)

spr_tmp_camden <- crop(x = spr_tmp_merge,
                       y = sf_camden)

ggplot() +
  geom_spatraster(data = spr_tmp_camden) +
  geom_sf(data = sf_camden,
          alpha = 0.25)


#3.
# Reproject raster and explore resampling (ref: Section 4.3.4)
# 
# Reproject spr_tmp_camden to a projected CRS: UTM Zone 18N (EPSG:32618). 
# Use terra::project() with y = "EPSG:32618". 
# Given the data type of the temperature layer, choose an appropriate method of 
# resampling (either method = "near" or method = "bilinear").
# 
# Assign to spr_tmp_camden_proj.
# 
# Inspect the new CRS and resolution by printing the projected object.


spr_tmp_camden_proj <- project(x = spr_tmp_camden,
                                y = "EPSG:32617",
                                method = "bilinear")

print(spr_tmp_camden_proj)

