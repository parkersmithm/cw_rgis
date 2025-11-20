if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               ggeffects,
               sf,
               terra,
               tidyterra,
               exactextractr,
               mapview,
               here)

(df_finsync <- read_csv(here("data/data_finsync_nc.csv")))


df_finsync %>% 
  mutate(presence = 1) %>% # all recorded species are "presence" = 1
  pivot_wider(id_cols = c(site_id, lon, lat),
              names_from = latin,
              values_from = presence)



# Convert data to species richness ----------------------------------------

df_finsync_rich <- df_finsync %>% 
  group_by(site_id, lon, lat) %>%
  summarize(sprich = sum(presence)) %>%
  ungroup()



# Link to environmental data (human footprint index) ----------------------

# get site coordinates
sf_rich_coords <- df_finsync_rich %>% 
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326)

# load human footprint data
hfi <- rast(here("data/spr_hfp2022.tif"))

# combine richness data set with human footprint dataset
rich_hfi_coords <- extract(x = hfi,
                           y = rich_coords,
                           bind = TRUE) %>% 
  st_as_sf()


df_hfi_rich <- as_tibble(rich_hfi_coords) %>% 
    select(-geometry)




# Visualize with ggplot ---------------------------------------------------

ggplot() +
  geom_spatraster(data = hfi) +
  geom_sf(data = sf_rich_coords, aes(color = sprich)) +
  scale_color_continuous() +
  scale_fill_wiki_c() +
  theme_bw() #it's still ugly:(



# Graph species richness against human footprint index --------------------

df_hfi_rich %>%
  ggplot(aes(x = hfp2022,
             y = sprich)) +
  geom_point() +
  theme_bw() +
  labs(x = "Human Footprint Index",
       y = "Species Richness",
       title = "Species Richness by Human Footprint Index in North Carolina 2022")



# Data analysis -----------------------------------------------------------

# Transform data
df_hfi_rich_trans <- df_hfi_rich %>% mutate(sprich = log(sprich), 
                                          hfi = log(hfp2022))

df_hfi_rich_trans %>%
  ggplot(aes(x = hfi,
             y = sprich)) +
  geom_point() +
  theme_bw() +
  labs(x = "Human Footprint Index",
       y = "Species Richness",
       title = "Species Richness by Human Footprint Index in North Carolina 2022")
## after transforming the data, it is still not normal - will use untransformed data for model


m1 <- glm(sprich ~ hfp2022,
          data = df_hfi_rich,
          family = "gaussian")#the data doesn't follow a Gaussian distribution, but it doesn't match anything else sooo.....
summary(m1)

# prediction

df_hfi_pred <- ggpredict(m1,
                     terms = "hfp2022 [all]")

ggplot() +
  geom_point(data = df_hfi_rich,
             aes(x = hfp2022,
                 y = sprich)) +
  geom_line(data = df_hfi_pred,
            aes(x = x,
                y = predicted)) +
  geom_ribbon(data = df_hfi_pred,
              aes(x = x,
                  ymin = conf.low,
                  ymax = conf.high),
              fill = "grey",
              alpha = 0.2) +
  labs(x = "Human Footprint Index",
       y = "Species Richness",
       title = "Species Richness by Human Footprint Index in North Carolina 2022") +
  theme_bw()

