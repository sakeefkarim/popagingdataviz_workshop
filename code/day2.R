
# PRELIMINARIES ----------------------------------------------------------------

library(systemfonts)
library(tidyverse)


# Mapping in ggplot2
library(sf)
library(sp)
library(terra)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggthemes)
library(tidygeocoder)
library(geojsonsf)
library(tidycensus)
library(cancensus)
library(mapcan)
library(usmap)
library(usmapdata)
library(mapview)
library(leaflet)
library(leafpop)

# Animations

library(plotly)
library(gganimate)
library(ggiraph)

# Misc

library(patchwork)
library(see)
library(scales)
library(ggrepel)

# LOADING THE DATA -------------------------------------------------------------


load("./data/day2.RData")

# BASIC MAPS -------------------------------------------------------------------

# A Map of the World -----------------------------------------------------------


ne_countries(scale = "medium",
             returnclass = "sf") %>% 
  filter(!name == "Antarctica") %>% 
  ggplot() + 
  geom_sf() +
  theme_map(base_family = "IBM Plex Sans") +
  labs(title = "A Map of the World")

ne_countries(scale = "medium",
             returnclass = "sf") %>% 
  filter(!name == "Antarctica") %>% 
  ggplot() + 
  geom_sf() +
# Changing the coordinate system
# Here, the popular Robinson projection:
  coord_sf(crs = st_crs("ESRI:53030")) +
# More projections can be found here:
# https://en.wikipedia.org/wiki/List_of_map_projections
# Search for details here: https://epsg.io/?q=
# Example: orthographic, north pole
#  coord_sf(crs = st_crs("ESRI:102035")) +
  theme_map(base_family = "IBM Plex Sans") +
  labs(title = "A Map of the World")


# Grid of Maps (Plots) ---------------------------------------------------------

map_robinson <- ne_countries(scale = "medium",
                             returnclass = "sf") %>% 
                filter(!name == "Antarctica") %>% 
                ggplot() + 
                geom_sf(fill = "red") +
                coord_sf(crs = st_crs("ESRI:53030")) +
                theme_light(base_family = "IBM Plex Sans")  +
                theme(text = element_blank())

map_orthographic <- ne_countries(scale = "medium",
                             returnclass = "sf") %>% 
                    filter(!name == "Antarctica") %>% 
                    ggplot() + 
                    geom_sf(fill = "lightseagreen") +
                    coord_sf(crs = st_crs("ESRI:102035")) 


map_robinson + map_orthographic + plot_layout(nrow = 2)


# INTRODUCING STATS ------------------------------------------------------------

map_data %>% 
  filter(!name == "Antarctica",
         year == 2015) %>% 
  ggplot() + 
  geom_sf(colour = "white",
          linewidth = 0.1,
          mapping = aes(fill = ntile(fertility_rate, 100))) +
  scale_fill_gradient2(low = muted("pink"), 
                       high = muted("red")) +
  coord_sf(crs = st_crs("ESRI:53030")) +
  theme_map(base_family = "IBM Plex Sans") +
  labs(title = "A Map of the World",
       fill = "Fertility Rate in 2015 (Percentile)") +
  theme(legend.position = "bottom",
        legend.justification = "center") +
  guides(fill = guide_colourbar(title.position = "top"))
                                

# Quick Exercise ---------------------------------------------------------------

# Play around with some of the indicators in the data. These include, but are
# not limited to ...:

# year, age_dependency (old age dependency), fertility_rate, death_rate,
# net_migration, foreign_share

# Create some new maps using some of the skills you picked up in Day 1:

# Scroll down for an example ...








map_data %>% 
  filter(!name == "Antarctica") %>% 
  drop_na(year) %>% 
  ggplot() + 
  geom_sf(colour = "white",
          linewidth = 0.05,
          mapping = aes(fill = death_rate)) +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  facet_wrap(~year, ncol = 2) +
  coord_sf(crs = st_crs("ESRI:53030")) +
  theme_map(base_family = "Inconsolata") +
  labs(title = "Mortality Around the World",
       subtitle = "1990 to 2015",
       fill = "Death Rate (per 1000)") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold"),
        strip.text = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = "right") +
  guides(fill = guide_colourbar(title.position = "top"))


# Zooming in on Canada ---------------------------------------------------------

province_territories_2017 %>% 
  ggplot(., aes(x = long, y = lat, group = group)) +
                  geom_polygon(mapping = aes(fill = population),
                               colour = "white",
                               linewidth = 0.2) +
  coord_sf() +
  theme_map(base_family = "IBM Plex Sans") +
  theme(legend.position = "top") +
  scale_fill_viridis_c(labels = function(x) paste((x)/1000000, "mil"),
                       direction = -1) +
  labs(fill = "Canadian Population in 2017") +
  guides(fill = guide_colorbar(title.position = "bottom")) +
  theme(legend.key.width = unit(0.85, "cm"))
                               

# Zooming in on the US ---------------------------------------------------------

us_vote %>% ggplot() +
            facet_wrap(~year, nrow = 2) +
            geom_polygon(colour = "white",
                         linewidth = 0.1,
                         mapping = aes(x = x, y = y,
                                       fill = ntile(republican, 100),
                                       group = group)) +
            coord_sf() +
            theme_map(base_family = "Inconsolata") +
            scale_fill_continuous_tableau(palette = "Red",
                                          labels = function(x) paste0(x, "%")) +
            labs(fill = "Trump Share (Percentile)") +
            theme(legend.position = "right") +
            guides(fill = guide_colorbar(title.position = "top"))
            

# Zooming in even more/geocoding -----------------------------------------------

# Creating some data:

locations <- tibble(site = c("FSU Student Union",
                             "McGill University"),
                    address = c("75 N Woodward Avenue, Tallahassee",
                                "McGill University")) %>% 
                    geocode(address = address, 
                            method = "arcgis")


# Talahassee (Point) -----------------------------------------------------------

tallahassee_shp %>% 
  ggplot() +
  geom_sf()

# Here, we mark the FSU Student Union using geom_point()

tallahassee_shp %>% 
  ggplot() +
  geom_sf() +
  geom_point(data = locations %>% slice(1),
             mapping = aes(x = long, y = lat)) +
  geom_label_repel(data = locations %>% slice(1),
                   mapping = aes(x = long, y = lat,
                                 label = site))


tallahassee_shp %>% 
  ggplot() +
  geom_sf(fill = "#782F40", colour = "white") +
  geom_point(colour = "#CEB888",
             size = 3.5,
             data = locations %>% slice(1),
             mapping = aes(x = long, y = lat)) +
  geom_label_repel(family = "IBM Plex Sans",
                   fill = "#CEB888",
                   colour = "white",
                   nudge_y = 0.02,
                   data = locations %>% slice(1),
                   mapping = aes(x = long, y = lat,
                                 label = site)) +
  theme_void(base_family = "IBM PLex Sans") +
  labs(title = "Tallahassee, Florida") 


# Montreal (Exercise) ----------------------------------------------------------

mtl <- canada_cd %>% ggplot() +
                     geom_sf(colour = "grey")  + theme_void() +
                     coord_sf(xlim = c(-75, -73),
                              ylim = c(45, 47))


# How can we highlight McGill University (see above for some guidance ---
# or scroll down for the answer!)




mtl +
geom_point(colour = "#ed1b2f",
           size = 3.5,
           data = locations %>% slice(2),
           mapping = aes(x = long, y = lat)) +
geom_label_repel(family = "IBM Plex Sans",
                 fill = "#ed1b2f",
                 colour = "white",
                 nudge_y = 0.15,
                 data = locations %>% slice(2),
                 mapping = aes(x = long, y = lat,
                               label = site))


# TIDYCENSUS -------------------------------------------------------------------

census_api_key("YOUR CENSUS KEY")

# Fetching data from the American Community Survey

# Variables in 2021:

acs_2021 <- load_variables(2021, "acs5")


# Searching for ... non-white share:

acs_2021 %>% filter(concept == "RACE",
                    str_detect(label, "Whit|Tota"))


# Leon/Duval County  -----------------------------------------------------------

leon_county <- get_acs(state = "FL",
                       county = "Leon County",
                       geography = "tract",
                       variables = "B02001_002",
                       summary_var = "B02001_001",
                       geometry = TRUE,
                       year = 2021)  %>% mutate(non_white_share = 1 - 
                                                estimate/summary_est)


duval_county <- get_acs(state = "FL",
                        county = "Duval County",
                        geography = "tract",
                        variables = "B02001_002",
                        summary_var = "B02001_001",
                        geometry = TRUE,
                        year = 2021)  %>% mutate(non_white_share = 1 - 
                                                 estimate/summary_est)

leon_county %>% ggplot() +
                   geom_sf(mapping = aes(fill = non_white_share), 
                           colour = "grey") +
                   theme_map(base_family = "IBM Plex Sans") +
                   scale_fill_gradient2_tableau() +
                   labs(title = "Leon County") 



duval_county %>% ggplot() +
                 geom_sf(mapping = aes(fill = non_white_share), colour = "grey") +
                 theme_map(base_family = "Inconsolata") +
                 scale_fill_gradient2_tableau() +
                 labs(title = "Duval County") +
                 theme(legend.position = "bottom")


# Can you adjust/beautify these plots? Spend 5-10 minutes exploring different
# possibilities. 

# CANCENSUS --------------------------------------------------------------------

set_cancensus_api_key('YOUR KEY', install = TRUE)


mtl <- list_census_regions(dataset = "CA21") %>% filter(name == "Montréal") %>% 
       slice(1) %>% pull(1)

# Interactively find variable(s) of interest:

explore_census_vectors(dataset = "CA21")

# Want to explore total non-white share in MTL:

mtl_data <- get_census(dataset = "CA21",
                       regions = list(CMA = mtl),
                       vectors = "v_CA21_4875",
                       level = "CT",
                       geo_format = "sf",
                       labels = "short")

mtl_data <- mtl_data %>% mutate(vm_share = v_CA21_4875/Population)

mtl_data %>% ggplot() +
             geom_sf(mapping = aes(fill = vm_share),
                     colour = "white",
                     linewidth = 0.01) +
             theme_map(base_family = "Inconsolata") + 
             scale_fill_viridis_c(option = "inferno") +
             labs(title = "Grand Montréal",
                  subtitle = "Data from the 2021 Canadian Census",
                  fill = "Visible Minority Share") +
             theme(legend.position = "top")

# Feel free to adjust the plot to your liking --- and find other indicators to
# visualize using cancensus OR tidycensus.


# INTERACTIVE MAPS -------------------------------------------------------------

# mapview ----------------------------------------------------------------------

mapView(tallahassee_shp, color = "white", 
        col.regions = "#446f85",
        layer.name = "Tallahassee")

fsu <- tibble(site = "Florida State University",
              address = "600 W College Ave, Tallahassee") %>%  
       geocode(address = address)

fsu_sf <- st_as_sf(fsu, coords = c("long", "lat"),
                   crs = 4326)

mapView(fsu_sf, color = "white", 
        col.regions = "#782F40",
        layer.name = "Florida State University",
        popup = popupIframe("https://www.youtube.com/embed/dB2VUuTm7MU?si=yZ2Y654rs-aDMZn4",
                           width = 300, height = 300))

# Leaflet ----------------------------------------------------------------------


# MORE INTERACTIVITY -----------------------------------------------------------


# plotly -----------------------------------------------------------------------


# gganimate --------------------------------------------------------------------








