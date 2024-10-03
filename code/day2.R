# ------------------------------------------------------------------------------

# Day 2: Maps and Interactivity
# PopAging DataViz
# Sakeef M. Karim
# Companion Script File
# CC-BY-SA 4.0

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

# Using rnaturalearth to generate a map of the world in seconds:

ne_countries(scale = "medium",
             returnclass = "sf") %>%
# Removing Antarctica from the map ... 
  filter(!name == "Antarctica") %>% 
  ggplot() + 
# The workhorse geom to produce maps -- but not our only option:
  geom_sf() +
  theme_map(base_family = "IBM Plex Sans") +
  labs(title = "A Map of the World")

# We can easily adjust or experiment with different map projections:

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

# Here's a fun way to sneak in a new package --- patchwork. Thanks to patchwork
# (and packages like ggpubr), we can easily combine plots:

# Here's the popular Robinson projection:

map_robinson <- ne_countries(scale = "medium",
                             returnclass = "sf") %>% 
                filter(!name == "Antarctica") %>% 
                ggplot() + 
                # Adjusting the colour of "countries" on the map:
                geom_sf(fill = "red") +
                coord_sf(crs = st_crs("ESRI:53030")) +
                theme_light(base_family = "IBM Plex Sans")  +
                theme(text = element_blank())

# An orthographic projection:

map_orthographic <- ne_countries(scale = "medium",
                             returnclass = "sf") %>% 
                    filter(!name == "Antarctica") %>% 
                    ggplot() + 
                    geom_sf(fill = "lightseagreen") +
                    coord_sf(crs = st_crs("ESRI:102035")) 


# Now, we easily combine plots by simply using "+" 

map_robinson + map_orthographic + 
              # Adjusts the layout of the plot -- here, two rows (in lieu of 1):
               plot_layout(nrow = 2)


# INTRODUCING STATS ------------------------------------------------------------

# Like any other geom, we can fill in our maps based on some variables 
# embedded in our input data frame:

# Get a sense of the data:

map_data %>% 
  filter(!name == "Antarctica",
         # For siplicity, isolating 2015:
         year == 2015) %>% 
  ggplot() + 
  geom_sf(colour = "white",
          linewidth = 0.1,
          # Filling in data as a function of TFR (percentiles):
          mapping = aes(fill = ntile(fertility_rate, 100))) +
  # Creating our own gradient scale:
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

# Data comes from the mapcan package:

province_territories_2017 %>% 
  ggplot(., aes(x = long, y = lat, group = group)) +
  # Notice something?
  geom_polygon(mapping = aes(fill = population),
                             colour = "white",
                              linewidth = 0.2) +
  # Why'd we switch things up?
  coord_sf() +
  theme_map(base_family = "IBM Plex Sans") +
  theme(legend.position = "top") +
  scale_fill_viridis_c(labels = function(x) paste((x)/1000000, "mil"),
                       direction = -1) +
  labs(fill = "Canadian Population in 2017") +
  guides(fill = guide_colorbar(title.position = "bottom")) +
  theme(legend.key.width = unit(0.85, "cm"))
                               

# Zooming in on the US ---------------------------------------------------------

# US election data:

us_vote %>% ggplot() +
            facet_wrap(~year, nrow = 2) +
            geom_polygon(colour = "white",
                         linewidth = 0.1,
                         mapping = aes(x = x, y = y,
                                       fill = ntile(republican, 100),
                                       group = group)) +
            coord_sf() +
            theme_minimal(base_family = "Inconsolata") +
            # From the ggthemes package:
            scale_fill_continuous_tableau(palette = "Red",
                                          labels = function(x) paste0(x, "%")) +
            labs(fill = "Trump Share (Percentile)") +
            theme(panel.grid = element_blank(),
                  axis.text = element_blank(),
                  axis.title = element_blank(),
                  legend.position = "bottom", 
                  strip.text = element_text(size = 11)) +
            guides(fill = guide_legend(title.position = "top"))            

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
  # Notice us isolating the first row via the slice function:
  geom_point(data = locations %>% slice(1),
             mapping = aes(x = long, y = lat)) +
  geom_label_repel(data = locations %>% slice(1),
                   mapping = aes(x = long, y = lat,
                                 label = site))


# Adding some FSU-specific colours:

tallahassee_shp %>% 
  ggplot() +
  # Adding FSU colours:
  geom_sf(fill = "#782F40", colour = "white") +
  geom_point(colour = "#CEB888",
             size = 3.5,
             data = locations %>% slice(1),
             mapping = aes(x = long, y = lat)) +
  geom_label_repel(family = "Inconsolata",
                   fill = "#CEB888",
                   colour = "white",
                   size = 4.5,
                   nudge_y = 0.02,
                   data = locations %>% slice(1),
                   mapping = aes(x = long, y = lat,
                                 label = site)) +
  theme_void(base_family = "Inconsolata") +
  labs(title = "Tallahassee, Florida") 


# Montreal (Exercise) ----------------------------------------------------------

# canada_cd from Monica Alexander ...

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
                           colour = "white") +
                   theme_map(base_family = "IBM Plex Sans") +
                   scale_fill_viridis_c(guide = guide_legend(),
                                        labels = function(x) paste0(x * 100, "%")) +
                   labs(fill = "Non-White Share", 
                        title = "Leon County") +
                  theme(legend.position = "bottom",
                        plot.title = element_text(size = 14),
                        legend.title = element_text(size = 12),
                        legend.text = element_text(size = 11))



duval_county %>% ggplot() +
                 geom_sf(mapping = aes(fill = non_white_share), 
                         colour = "white") +
                 theme_map(base_family = "Inconsolata") +
                 scale_fill_viridis_c(option = "magma",
                                      labels = function(x) paste0(x * 100, "%")) +
                 labs(title = "Duval County",
                      fill = "Non-White Share") +
                 theme(legend.position = "bottom",
                       plot.title = element_text(size = 14),
                       legend.title = element_text(size = 12),
                       legend.text = element_text(size = 11))

# Can you adjust/beautify these plots? Spend 5-10 minutes exploring different
# possibilities. 

# CANCENSUS --------------------------------------------------------------------

# Montreal  --------------------------------------------------------------------

set_cancensus_api_key('YOUR KEY', install = TRUE)

# Interactively find variable(s) of interest:

explore_census_vectors(dataset = "CA16")

# Want to explore total non-white share in MTL ...

# Extracting MTL's census code:

mtl <- list_census_regions(dataset = "CA21") %>% filter(name == "Montréal") %>% 
  slice(1) %>% pull(1)

# Returning data for visible minority share ...

mtl_data <- get_census(dataset = "CA21",
                       regions = list(CMA = mtl),
                       vectors = "v_CA21_4875",
                       level = "CT",
                       geo_format = "sf",
                       labels = "short")

# Generating measure of VM share:

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

# leaflet ----------------------------------------------------------------------

# Where workshop participants were born  ---------------------------------------

born_data

continent_palette <- colorFactor(palette = c("dodgerblue", "red", 
                                             "orange", "black", "purple"),
                                 domain = born_data$continent)


leaflet(born_data) %>%
  # addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addProviderTiles(providers$CartoDB.Voyager) %>% 
  addCircleMarkers(lng = born_data$long, 
                   lat = born_data$lat,
                   fillOpacity = 0.5,
                   weight = 10,
                   radius = ~ sqrt(n) * 8,
                   color = ~continent_palette(continent),
                   stroke = FALSE) %>% 
  # setView(lng= 90.4152, lat=23.8041, zoom = 4)


# mapview ----------------------------------------------------------------------

mapView(tallahassee_shp, color = "white", 
        col.regions = "#446f85",
        layer.name = "Tallahassee")

fsu <- tibble(site = "Florida State University",
              address = "600 W College Ave, Tallahassee") %>%  
       geocode(address = address)

fsu_sf <- st_as_sf(fsu, coords = c("long", "lat"),
                   # WGS84 projection:
                   crs = 4326)

mapView(fsu_sf, color = "white", 
        col.regions = "#782F40",
        layer.name = "Florida State University",
        # Size of point:
        cex = 25,
        popup = popupIframe("https://www.youtube.com/embed/dB2VUuTm7MU?si=yZ2Y654rs-aDMZn4",
                           width = 300, height = 300))

# popupIframe function comes from
# https://rdrr.io/github/r-spatial/leafpop/src/R/graph.R


# MORE INTERACTIVITY -----------------------------------------------------------


# plotly -----------------------------------------------------------------------

old_age_dependency  <- ggplot(data = select_countries,
                              aes(x = year, y = age_dependency, 
                                 colour = country)) +
                       geom_line() +
                       scale_colour_colorblind() +
                       theme_bw(base_family = "Inconsolata") +
                       labs(colour = "", x = "", y = "Old Age Dependency")
           

select_countries_modified <- select_countries %>% 
                             # Creating a text "tooltip" for our "hover label"
                             mutate(tooltip = paste(" Country:", 
                                                    country, "<br>", 
                                                    "Year:", year, "<br>",
                                                    "Old Age Depndency:",
                                                     round(age_dependency, 2)))

old_age_dependency_new  <- ggplot(data = select_countries_modified,
                                  aes(x = year, y = age_dependency, 
                                      text = tooltip,
                                      group = country,
                                      colour = country)) +
                           geom_line() +
                           scale_colour_colorblind() +
                           theme_bw(base_family = "Inconsolata") +
                           labs(colour = "", x = "", y = "Old Age Dependency") 

# Adjusting the "hover label" and legend:

ggplotly(old_age_dependency_new,
         tooltip = "text") %>% 
  
# Adjusting the "hover label" and legend:
        # Changes font of tooltip:
layout(hoverlabel = list(font = list(family = "Inconsolata")),
       # Changes legend location
       legend = list(x = 0,
                     y = 1,
                     traceorder = "normal"))

# htmltools::save_html(STORED_OBJECT_HERE, "interactive_plot.html")

# Use the select_countries data frame + plotly to easily produce some 
# interactive plots of your own (in the next 10-15 mintutes or so).



old_age_dependency  <- ggplot(data = select_countries,
                              aes(x = year, y = age_dependency, 
                                  colour = country)) +
  geom_line() +
  scale_colour_colorblind() +
  theme_bw(base_family = "Inconsolata") +
  labs(colour = "", x = "", y = "Old Age Dependency")

old_age_dependency + transition_time(year)




