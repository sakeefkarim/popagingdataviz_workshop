# ------------------------------------------------------------------------------

# Day 1: Introduction to ggplot2
# PopAging DataViz
# Sakeef M. Karim
# Companion Script File
# CC-BY-SA 4.0

# PRELIMINARIES ----------------------------------------------------------------

library(systemfonts)
library(tidyverse)

# Engines behind the data frames:

library(palmerpenguins)
library(gapminder)
library(demography)
library(covdata)
library(cansim)
library(WDI)
library(LexisPlotR)

# Themes, Colour Schemes

library(ggthemes)
library(hrbrthemes)
library(see)
library(paletteer)
library(gglgbtq)
library(colorspace)

# Additional packages, geoms, tools

library(lemon)
library(skimr)
library(lattice)
library(ggrepel)
library(ggridges)
library(ggtext)
library(ggdist)
library(LexisPlotR)

# LOADING THE DATA -------------------------------------------------------------

load("./data/day1.RData")

# PRELIMINARY EXAMPLE ----------------------------------------------------------

data(penguins)

skim(penguins)

# Three histograms -------------------------------------------------------------

# Base R:

hist(penguins$bill_depth_mm)

# lattice

histogram(~bill_depth_mm, data = penguins)

# ggplot2

ggplot(data = penguins, mapping = aes(x = bill_depth_mm)) + 
  geom_histogram()

# Questions

# 1. What are the differences between the three histograms/their underlying code?


# 2. What other questions can we ask (or visually explore) using `penguins`?
  

# THE GRAMMAR OF ggplot2 -------------------------------------------------------

# AESTHETICS -------------------------------------------------------------------

?aes

skim(gapminder)


# First example: gapminder

# No mapping aesthetics:

ggplot(data = gapminder)

# + positional aesthetics:

ggplot(data = gapminder,
       mapping = aes(x = year, y = lifeExp))


# The code for this workshop is verbose by design, but you do not *have* to 
# spell out `mapping` and `data` each time you generate a plot. For instance, 
# you can produce the graphic by running:
  
ggplot(gapminder,
       aes(year, lifeExp))


# GEOMS ------------------------------------------------------------------------

# All the base geoms:

ls(pattern = '^geom_', env = as.environment('package:ggplot2'))

# Scatterplots -----------------------------------------------------------------

# Focus: relationship between the log of GDP per capita and life expectancy
# in gapminder. 

# To make matters easier, we'll home-in on the latest year included 
# in `gapminder` (2007). 

ggplot(# Note that we're subsetting the data within the ggplot function:
  data = gapminder %>% filter(year == max(year)),
  # Here, we're mapping variables in our data to
  # the 'x' and 'y' positions in our plot space:
  mapping = aes(x = log(gdpPercap), y = lifeExp)) +
  geom_point(# Adjusts the colour of the points:
    colour = "#008080",
    # Adjusts the size of the points:
    size = 1,
    # Adjusts the transparency of the points:
    alpha = 0.5)

# In the plot above, we've _added_ a geometric object (points or circles) to our
# graphic by including the `+ geom_point()` argument. 

# We can now tune or modify the aesthetic attributes of our `geom_point()` layer. 

# For instance, we can adjust `colour` within our global `aes` function to
# ensure that points are shaded pursuant to the `continent` variable in our data:
  
ggplot(data = gapminder %>% filter(year == max(year)),
       mapping = aes(x = log(gdpPercap), y = lifeExp,
                     # Sets colour globally --- mapping it to the
                     # `continent` variable in the data.
                     colour = continent)) +
  geom_point( # Adjusts the size of the points:
    size = 3,
    # Adjusts the transparency of the points:
    alpha = 0.5)


# We can also systematically adjust the *size* of our points. Below,
# the size of the points corresponds to a country's population in 2007 (logged).

ggplot(data = gapminder %>% filter(year == max(year)),
       mapping = aes(x = log(gdpPercap), y = lifeExp,
                     colour = continent,
                     # Sets "size" globally -- mapping it to the
                     # population variable in the data.
                     size = log(pop))) +
geom_point(# Adjusts the transparency of the points:
            alpha = 0.5)


# Question

# How can we adjust the `shape` of the points in our plot to ensure that they 
# vary as a function of our `continent` variable?
  
# Scroll down for the answer:





# Answer:


ggplot(data = gapminder %>% filter(year == max(year)),
       mapping = aes(x = log(gdpPercap), y = lifeExp,
                     colour = continent,
                     # Include the shape attribute in your aes() call:
                     shape = continent,
                     size = log(pop))) +
  geom_point(# Adjusts the transparency of the points:
    alpha = 0.5)


# Line Plots -------------------------------------------------------------------

# Let's play around with some of the other data we have at our disposal ---
# specifically, data from `select_countries`.

# Here's an overview of the data frame:

select_countries

skim(select_countries)

# Let's say we want to visualize how old-age dependency (`age_dependency`) has
# evolved over time for each of the nation-states (Canada, the United States,
# Germany, Japan) featured in our data. 

# To kick things off, let's produce a rudimentary plot that makes use of the `
# geom_line()` function.

ggplot(data = select_countries,
       aes(x = year, y = age_dependency,
           # This ensures that we produce unique lines for each country.
           group = country)) +
  geom_line(colour = "dodgerblue",
            linetype = "dashed")


# Here's what would happen if we didn't modify the `group` aesthetic.

# We've produced four unique trajectories, but we don't know what these 
# trajectories mean.

# What can we do about it?
  
# Scroll down for the answer!










# Answer

ggplot(data = select_countries,
       aes(x = year, y = age_dependency, 
           # Sets colour to country:
           colour = country)) +
  geom_line(linetype = "dashed")


# We can also ensure that our `linetype`s vary as a function of the `country`
# variable: 
  
ggplot(data = select_countries,
       aes(x = year, y = age_dependency, 
           colour = country,
           # Sets linetype to country as well:
           linetype = country)) +
  geom_line()


# Question 

# How can we adjust the `linewidth` of the trajectories in our plot to ensure 
# that they vary as a function of our `fertility_rate` variable? 

# For this exercise, do not include a `linetype` argument in your `aes()` call.
# Scroll down for the answer.










# Answer

ggplot(data = select_countries,
       aes(x = year, y = age_dependency, 
           colour = country,
           # Ensures that the width of the line varies by TFR:
           linewidth = fertility_rate)) +
geom_line()

# Bar Plots --------------------------------------------------------------------

# To produce a few basic bar plots in `ggplot2`, let's use the 
# `select_countries_sex` data frame. 

# Here's a look at the data frame in question.

select_countries_sex

skim(select_countries_sex)

# For now, let's begin by producing a bar plot that highlights sex differences 
# in life expectancy in Canada, the United States, Germany and Japan in the 
# year 2020.

ggplot(data = select_countries_sex %>% filter(year == max(year)),
       mapping = aes(x = country, y = life_expectancy,
                     # To produce different quantities along 
                     # the lines of sex:
                     group = sex)) +
  geom_col(# To ensure that bars are placed
    # side-by-side --- and not stacked!
    position = "stack", 
    colour = "white") 

?position_

# Here's what would happen if our `position` argument was left alone.

ggplot(select_countries_sex %>%
       filter(year ==
              max(year)),
       aes(x = country, 
           y = life_expectancy,
           group = sex)) +
geom_col(colour = "white") 


# Now, let's clarify what the two bars (per country) actually represent. 

# How can we do this in a straightforward manner? 

# A simple approach is shown below: here, we simply change the `fill` of our 
# bars so they correspond to the `sex` variable in our input data:
  
ggplot(data = select_countries_sex %>% filter(year == max(year)),
       mapping = aes(x = country,
                     y = life_expectancy,
                     # Ensuring that the colour inside the bars
                     #  (the "fill") varies by sex:
                     fill = sex)) +
  geom_col(position = "dodge", 
           colour = "white")

# Let's add a bit more complexity. To do so, we'll produce a graph that:

# 1. Reproduces the bar plot from above --- which illustrates sex differences in
#    life expectancy across four countries in the year 2020.

# 2. Displays the *distribution* of life expectancy for females/males across 
#    these 4 countries in the last half century (1970-2020). 

# Adding geoms -----------------------------------------------------------------

ggplot(data = select_countries_sex %>% filter(year == max(year)),
       mapping = aes(x = country, y = life_expectancy,
                     fill = sex)) +
geom_col(# Allows for more fine-grained control of
         # the space between bars:
         position = position_dodge(width = 0.9), 
         colour = "white") +
geom_boxplot(linewidth = 0.3,
             width = 0.35,
             # Space between boxplots constrained to
             # be equal to space between bars:
             position = position_dodge(width = 0.9),
             # Using original data frame that
             # has not been subsetted.
             data = select_countries_sex)

ggplot(data = select_countries_sex %>% 
         filter(year == max(year)) %>% 
         # Rearranging the order of the discrete 
         # y-axis labels using forcats functions:
         mutate(country = 
                  fct_rev(fct_relevel(country, 
                                      "Canada",
                                      "United States",
                                      "Germany"))),
       mapping = aes(# Note the inversion of the x and y axes:
         x = country,
         y = life_expectancy,
         fill = sex)) +
  geom_col(position = "dodge", 
           colour = "white") 


# What do you notice about the arguments within the `geom_boxplot()` function?

# STATISTICAL TRANSFORMATIONS --------------------------------------------------

# *Most* of the plots covered thus far feature an explicit mapping of variables
# to visual. However, many of the `geom`s available in `ggplot2` feature 
# statistical transformations of the inputs to ease interpretation of the 
# relationships between variables in our data. 

# For our purposes, we'll focus on a couple of statistical transformations and a
# associated functions. To this end, we'll work with some new `geom`s that are 
# powered by `stat_*` functions under the hood.

# Smoothed Conditional Means ---------------------------------------------------

# We'll begin by visualizing how the fertility rate has evolved over time in 
# Canada, the United States, Germany and Japan. 

ggplot(data = select_countries,
       mapping = aes(x = year, 
                     y = fertility_rate, 
                     colour = country)) +
geom_line()

# The plot below uses `geom_smooth()` to simplify the same trends we encountered 
# above. 

# The function is powered by `stat_smooth()` under the hood, which (by default) 
# uses local polynomial regressions or general additive models.

ggplot(data = select_countries,
       mapping = aes(x = year, y = fertility_rate, 
                     colour = country)) +
geom_smooth(mapping = # Adjusts hue of the confidence intervals:
                     aes(fill = country),
            alpha = 0.3)

?geom_smooth

# We can adjust how we're "smoothing" our data by toggling the `method` 
# argument within `geom_smooth()`. 

# Here's how to generate estimates from a linear model (in lieu of the default
# non-parametric approaches),

ggplot(data = select_countries,
      aes(x = year,
          y = fertility_rate, 
          colour = country)) +
geom_smooth(aes(fill = country),
            method = "lm",
            alpha = 0.5)

# Density of observations ------------------------------------------------------

ggplot(data = gapminder %>% 
              # Zeroing in on latest year and removing Oceania
              # which has only two observations:
              filter(year == max(year)),
       mapping = aes(x = lifeExp,
                     # Ensuring the "fill" (colour inside the distribution) 
                     # and "colour" (the line) have the same attributes:
                     colour = continent)) +
geom_freqpoly(mapping = aes(y = after_stat(density)),
              binwidth = 5)

?stat_density

# 
# gapminder %>% 
#   filter(year == max(year), 
#          !continent == "Oceania") %>% 
#   ggplot(aes(x = lifeExp,
#              colour = continent)) +
#   geom_freqpoly(binwidth = 5)

ggplot(data = gapminder %>% 
              # Zeroing in on latest year and removing Oceania
              # which has only two observations:
              filter(year == max(year),
                     !continent == "Oceania"),
       mapping = aes(x = lifeExp,
                     # Ensuring the "fill" (colour inside the distribution) 
                     # and "colour" (the line) have the same attributes:
                     colour = continent,
                     fill = continent)) +
geom_density(alpha = 0.5)

# An aside: statistical summaries 

ggplot(data = gapminder %>% 
         # Zeroing in on latest year and removing Oceania
         # which has only two observations:
         filter(year == max(year)), 
       aes(x = continent, y = lifeExp)) +
  geom_point(colour = "lightgrey") + 
  stat_summary(fun.data = "mean_cl_boot", 
               colour = "skyblue", 
               linewidth = 2,
               size = 1)

# SCALES -----------------------------------------------------------------------

# Pruning axes -----------------------------------------------------------------

ggplot(data = select_countries,
       mapping = aes(x = year, 
                     y = fertility_rate, 
                     colour = country)) +
geom_smooth(mapping = # Adjusts hue of the confidence intervals:
                     aes(fill = country),
            alpha = 0.5) +
scale_x_continuous(# Prunes the x-axis by setting new limits:
                   # 2000-2020 instead of 1970-2020
                   limits = c(2000, 2020),
                   # How is this range sliced up? 
                   # Here, 2000 to 2020 in increments of 5 (years):
                   breaks = seq(2000, 2020, by = 5)) 

# Modifying linetype aesthetics ------------------------------------------------

ggplot(data = select_countries,
       mapping = aes(x = year, 
                     y = fertility_rate, 
                     linetype = fertility_rate,
                     colour = country)) +
geom_smooth(mapping = 
            aes(fill = country,
            alpha = 0.5)  +
scale_x_continuous(limits = c(2000, 2020),
                   breaks = seq(2000, 2020, by = 5)) +
geom_hline(mapping = aes(yintercept = 2.1, 
                         linetype = "Replacement Level Fertility")) +
scale_linetype_manual(# Removing the legend title for linetypes:
                      name = "",
                      # To control legends, breaks need to be linked
                      # to values:
                      breaks = "Replacement Level Fertility",
                      values = "dashed")


# Modifying colour/fill of geometric layers ------------------------------------

ggplot(data = select_countries,
       mapping = aes(x = year, y = fertility_rate, 
                     colour = country)) +
geom_smooth(mapping = aes(fill = country),
            alpha = 0.5)  +
scale_x_continuous(limits = c(2000, 2020),
                   breaks = seq(2000, 2020, by = 5)) +
geom_hline(mapping = aes(yintercept = 2.1, 
                         linetype = "Replacement Level Fertility")) +
scale_linetype_manual(name = "",
                      breaks = "Replacement Level Fertility",
                      values = "dashed") +
# Using the "Dark 2" palette from the inbuilt
# colour_brewer() family of functions:
scale_colour_brewer(palette = "Dark2") +
scale_fill_brewer(palette = "Dark2")



# Working with "dates" ---------------------------------------------------------

mobility_covdata

# Data can be piped in as a part of a longer code sequence:
mobility_covdata %>% 
# Isolating data from Jacksonville:
filter(str_detect(city, "Jack")) %>% 
ggplot(., aes(x = date, 
              y = score, 
              colour = transportation_type, 
              fill = transportation_type)) +
geom_smooth()



mobility_covdata %>% 
filter(str_detect(city, "Jack")) %>% 
ggplot(., aes(x = date, 
              y = score, 
              colour = transportation_type, 
              fill = transportation_type)) +
geom_smooth() +
# Using the inbuilt viridis functions to adjust colour/fill aesthetics:
scale_colour_viridis_d(option = "inferno") +
scale_fill_viridis_d(option = "inferno") +
# Modifying how dates are displayed on the plot:
scale_x_date(# Breaks between dates:
             date_breaks = "2 months",
             # Date format --- run ?strptime for more information:
             date_labels = "%D")

?strptime


# COORDS -----------------------------------------------------------------------

ggplot(data = select_countries_sex %>%  
              filter(country == "Canada"),
       mapping = aes(x = year, 
                     y = life_expectancy, 
                     fill = sex, 
                     colour = sex))  +
  geom_polygon(alpha = 0.5)


ggplot(data = select_countries_sex %>%  filter(country == "Canada"),
       mapping = aes(x = year, 
                     y = life_expectancy, 
                     fill = sex, 
                     colour = sex))  +
  geom_polygon(alpha = 0.5) +
  scale_x_continuous(limits = c(1970, 2020),
                    # Flipping the labels to ensure
                    # that plot is read "clockwise"
                     labels = c("2020", "1980", 
                                "1990", "2000", 
                                "2010", "1970")) +
  coord_polar()

# An aside: pie charts ---------------------------------------------------------

# Pie charts are popular, but should generally be avoided 
# (https://kieranhealy.org/blog/archives/2017/04/06/saying-no-to-pie/)

# That said, you may have to make one at some point. With that in mind, 
#here's a very simple example ... but beware: this snippet includes code 
# we have yet to cover.

select_countries_sex %>%
  filter(year == 1980, country == "Germany") %>% 
  mutate(label = paste0(round(pop_share), "%")) %>% 
  ggplot(.,
         mapping = aes(x = "", y = pop_share, fill = sex)) +
  geom_bar(stat = "identity") + 
  coord_polar(theta = "y") +
  theme_void() +
  geom_text(mapping = aes(label = label),
            colour = "white",
            position = position_stack(vjust = 0.5)) +
  labs(title = "Sex Distribution in Germany (1980)") +
  scale_fill_brewer(palette = "Set1")

# FACETS -----------------------------------------------------------------------

# facet_wrap() -----------------------------------------------------------------

mobility_covdata %>% 
  ggplot(., aes(x = date, 
                y = score, 
                colour = transportation_type, 
                fill = transportation_type)) +
  geom_smooth() +
  scale_colour_viridis_d(option = "inferno") +
  scale_fill_viridis_d(option = "inferno") +
  scale_x_date(date_breaks = "2 months",
               date_labels = "%D") +
  # Creating small multiples of the data:
  # Here, we're conditioning on city/generating two rows of
  # facets (or panels):
  facet_wrap(~city, nrow = 2)

ggplot(data = select_countries_sex,
       mapping = aes(x = year, y = life_expectancy, 
                     fill = sex, 
                     colour = sex))  +
  geom_polygon(alpha = 0.5) +
  facet_wrap(~country, nrow = 1) +
  scale_x_continuous(limits = c(1970, 2020),
                     labels = c("2020", "1980", "1990",
                                "2000", "2010", "1970")) +
  # Greyscale colour/fill themes:
  scale_fill_grey() +
  scale_colour_grey() +
  coord_polar() 

# facet_grid() -----------------------------------------------------------------

select_countries %>% 
  # Reorienting data (to long/"tidy" format):
  pivot_longer(!c(country, year),
               names_to = "indicator",
               values_to = "value") %>% 
  ggplot(., aes(x = year, y = value, 
                colour = country, fill = country)) +
  geom_smooth(alpha = 0.5) +
  scale_x_continuous(breaks = seq(1970, 2020, by = 25)) +
  # Creating a grid of small multiples (row ~ column):
  facet_grid(indicator ~ country, 
             # Ensures that both panels can have their own
             # x/y limits:
             scale = "free") +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")


# ADJUSTING LABELS -------------------------------------------------------------

ggplot(gapminder %>% filter(year == max(year) |
                            year == min(year)),
       aes(x = log(gdpPercap), y = lifeExp))  +
  facet_wrap(~year, nrow = 2) +
  geom_point(aes(colour = continent, size = pop), alpha = 0.65)  +
  geom_smooth(colour = "black", alpha = 0.35,
              method = "lm",
              linewidth = 0.5) +
  labs(# Editing x-axis title:
    x = "Log of Per Capita GDP", 
    # Editing y-axis title:
    y = "Life Expectancy in Years", 
    # Removing legend title for the colour aesthetic:
    colour = "",
    # Changing legend title for the size aesthetic:
    size = "Population") +
  # Using functions within scales function to clean up labels ---
  # in this case, simply adding a "+" sign
  scale_size_continuous(labels = 
                          scales::comma_format(suffix = " +")) 
?comma_format

# ADJUSTING THEMES -------------------------------------------------------------

# Zeroing in on the first and last year in the gapminder df:
gapminder %>% filter(year == max(year) |
                       year == min(year)) %>% 
  ggplot(aes(x = log(gdpPercap), y = lifeExp))   +
  facet_wrap(~year) +
  geom_point(aes(colour = continent, size = pop), alpha = 0.65)  +
  geom_smooth(colour = "black", alpha = 0.35,
              method = "lm",
              linewidth = 0.5) +
  labs(title = "Relationship Between GDP and Life Expectancy",
       subtitle = "Over 50 Years Apart",
       x = "Log of Per Capita GDP",
       y = "Life Expectancy in Years", 
       colour = "",
       size = "Population") +
  scale_size_continuous(labels = 
                          function(x) paste(x/1000000, "mil")) +
  scale_colour_brewer(palette = "Dark2") +
  # Using theme_bw() to modify default "look" of the plot; using the
  # IBM Plex Sans plot:
  theme_bw(base_family = "IBM Plex Sans") + 
  theme(# Ensuring that the plot title is in boldface:
    plot.title = element_text(face = "bold"),
    # Changing the colour of the subtitle:
    plot.subtitle = element_text(colour = "grey45"),
    # Adding space to the right of the y-axis title 
    # (pushing text away from the plot panel):
    axis.title.y = element_text(margin = margin(r = 15)),
    # Adding space to the top of the x-axis title:
    axis.title.x = element_text(margin = margin(t = 15)),
    # Removing minor gridlines not linked to axis labels:
    panel.grid.minor = element_blank(),
    # Placing legend on the bottom of the plot:
    legend.position = "bottom",
    # Increasing the size of the legend keys:
    legend.key.size = unit(1, "cm"),
    # Arranging multiple legends vertically (more than one row):
    legend.box = "vertical")



# ADJUSTING GUIDES -------------------------------------------------------------

ggplot(gapminder %>% filter(year == max(year) |
                              year == min(year)), 
       aes(x = log(gdpPercap), y = lifeExp))   +
  facet_wrap(~year) +
  geom_point(aes(colour = continent, 
                 # Sizing plots based on log of population:
                 size = log(pop)), 
             alpha = 0.65)  +
  geom_smooth(colour = "black", 
              alpha = 0.35,
              method = "lm",
              linewidth = 0.5) +
  labs(title = "Relationship Between GDP and Life Expectancy",
       subtitle = "Over 50 Years Apart",
       x = "Log of Per Capita GDP", 
       y = "Life Expectancy in Years", colour = "",
       size = "Log of Population") +
  scale_size_binned(# Range of plot sizes:
    range = c(0.1, 3.5),
    labels = function(x) paste(x, "+")) +
  scale_colour_brewer(palette = "Dark2") +
  theme_bw(base_family = "IBM Plex Sans") + 
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(colour = "grey45"),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 15)),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(1, "cm"),
        legend.box = "vertical") +
  guides(size = guide_bins(# Push legend title to the bottom:
    title.position = "bottom",
    # Centring legend title.
    title.hjust = 0.5)) +
  guides(# Rearranging order of legends; colour now appears first.
    colour = guide_legend(order = 1,
                          # Overriding aes - all keys are 
                          # at size = 5.
                          override.aes = list(size = 8))) 


# ADDITIONAL GEOMS -------------------------------------------------------------

# ggridges ---------------------------------------------------------------------

ggplot(gapminder %>% 
         filter(year == max(year),
                !continent == "Oceania"), 
       aes(x = lifeExp, y = fct_rev(continent),
           fill = continent,
           colour = continent)) +
  geom_density_ridges(alpha = 0.35,
                      jittered_points = TRUE) +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme_ridges() +
  labs(x = "Life Expectancy in 2007", y = "") +
  theme(text = element_text(family = "IBM Plex Sans"),
        legend.position = "none") +
  # Removes all padding around y-axis:
  scale_y_discrete(expand = c(0, 0)) +
  # Removes all padding around x-axis:
  scale_x_continuous(expand = c(0, 0)) +
  # Allows plotting outside of the plot margin/panel:
  coord_cartesian() 

# geom_repel() -----------------------------------------------------------------

select_countries %>% 
  # Zooming on 1970 and 2020
  filter(year %in% c(1970, 2020)) %>% 
  # Creating a label variable:
  mutate(label = paste(as.character(round(age_dependency, 1)),
                       "per 100")) %>% 
  ggplot(., aes(x = as_factor(year),
                y = age_dependency, 
                group = country, 
                label = label,
                colour = country, 
                fill = country)) +
  facet_rep_wrap(~country, 
                 nrow = 4, 
                 # Repeats axis text for each facet (via lemon):
                 repeat.tick.labels = TRUE) + 
  geom_point(size = 3) + 
  geom_line() +
  # Adding labels to the points:
  geom_label_repel(segment.color = "grey85",
                   colour = "white",
                   # Moving label down:
                   nudge_y = -10, 
                   show.legend = FALSE,
                   size = 4.5,
                   family = "IBM Plex Sans") +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Rising Old Age Dependency",
       subtitle = "In the Last Half Century",
       x = "", 
       y = "Old Age Dependency Ratio",
       caption = "Old Age Dependency =\nRatio of Elderly Population (64+) to Working Age Population (15-64)") +
  theme_classic(base_family = "IBM Plex Sans") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(colour = "grey50"),
        axis.title.y = element_text(size = 12, margin = margin(r = 15)),
        strip.text = element_text(size = 12),
        plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 10))


# MORE THEMES, SCHEMES ---------------------------------------------------------

# Colour blind friendly themes -------------------------------------------------

select_countries %>% 
  pivot_longer(!c(country, year),
               names_to = "indicator",
               values_to = "value") %>% 
  mutate(indicator = ifelse(str_detect(indicator, "age_"), 
                            "Old Age Dependency",
                            "Fertility Rate")) %>% 
  ggplot(., aes(x = year, y = value, 
                colour = country, fill = country)) +
  geom_smooth(alpha = 0.5) +
  scale_x_continuous(breaks = seq(1970, 2020, by = 25)) +
  facet_grid(fct_rev(indicator) ~ country, 
             scales = "free") +
  labs(x = "", y = "") +
  # Colour/fill themes that should be visible to individuals with
  # colour blindness:
  scale_colour_colorblind() +
  scale_fill_colorblind() +
  # Classic theme from the "see" package:
  theme_classic(base_family = "Inconsolata") +
  theme(strip.text.y = element_text(angle = 0),
        panel.spacing = unit(1, "cm"),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(colour = "grey50"),
        axis.title.y = element_text(size = 12, margin = margin(r = 15)),
        strip.text = element_text(size = 12),
        plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 10))

# gglgbtq ----------------------------------------------------------------------

ggplot(gapminder %>% 
         filter(year == max(year),
                !continent == "Oceania"), 
       aes(x = lifeExp, y = fct_rev(continent),
           fill = continent,
           colour = continent)) +
  geom_density_ridges(alpha = 0.35,
                      jittered_points = TRUE)  +
  scale_colour_manual(values = palette_lgbtq("rainbow")) +
  scale_fill_manual(values = palette_lgbtq("rainbow")) +
  theme_ridges() +
  labs(x = "Life Expectancy in 2007", y = "") +
  theme(text = element_text(family = "IBM Plex Sans"),
        legend.position = "none") +
  # Removes all padding around y-axis:
  scale_y_discrete(expand = c(0, 0)) +
  # Removes all padding around x-axis:
  scale_x_continuous(expand = c(0, 0)) +
  # Allows plotting outside of the plot margin/panel:
  coord_cartesian(clip = "off") 

# For more palettes, run:

# paletteer::palettes_c_names

# OR

# paletteer::palettes_d_names


# VISUALIZATIONS FOR POPULATION RESEARCH ---------------------------------------

# Heatmaps  --------------------------------------------------------------------

# Reproducing Kieran Healy's mortality heat map (https://kieranhealy.org/prints/mortality-v/)

fr.mort %>% 
  filter(!Group == "total", !Age > 100) %>% 
  ggplot(., aes(x = Year, y = Age, fill = ntile(Mortality, 100))) +
  facet_wrap(~paste0(str_to_title(Group),"s"), nrow = 2) +
  geom_raster() +
  # scale_fill_viridis_c(option = "magma", direction = -1) +
  scale_fill_continuous_sequential(palette = "SunsetDark") +
  scale_x_continuous(breaks = seq(1816, 2006, by = 20)) +
  theme_modern_rc(base_family = "IBM Plex Sans") +
  labs(title = "Mortality in France",
       subtitle = "1816-2006",
       x = "", fill = "Death Rate (Percentile)") +
  guides(fill = guide_legend(nrow = 1, 
                             title.position = "top", 
                             label.position = "bottom")) +
  theme(plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 15),
        axis.title.y = element_text(size = 13, 
                                    margin = margin(r = 15)),
        legend.text = element_text(size = 12),
        strip.text = element_text(colour = "white"),
        panel.grid.minor = element_blank(),
        legend.key = element_rect(colour = "white", linewidth = 1.1),
        legend.justification = "top")

aus.fert


# Lexis Diagrams ---------------------------------------------------------------

lexis_grid(year_start = 1990,
           year_end = 2020, 
           age_start = 15, age_end = 64) %>% 
  lexis_age(age = 25,
            fill = "skyblue") %>% 
  lexis_year(year = 2015, fill = "pink") %>% 
  lexis_cohort(cohort = 1995, fill = "black") %>%
  lexis_lifeline(birth = "1995-11-07", 
                 lwd = 1, colour = "lightseagreen") +
  theme_grey(base_family = "IBM Plex Sans") +
  theme(panel.grid.minor = element_blank()) +
  coord_cartesian(xlim = c(as.Date("2000-01-01"), 
                           as.Date("2019-01-01")),
                  ylim = c(15, 64)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(breaks = "1 years",
               date_labels = "%Y",
               guide = guide_axis(n.dodge = 2)) +
  annotate(geom = "richtext",
           label = "1980 Cohort",
           x = as.Date("2017-12-01"),
           y = 34,
           family = "IBM Plex Sans",
           fontface = "bold",
           fill = "black",
           colour = "white")


# Population Pyramids ----------------------------------------------------------

can_binned_age %>% 
  filter(year == 2021) %>% 
  mutate(share = ifelse(sex == "Males", -share, share)) %>% 
  ggplot(., aes(x = share,
                y = age_group,
                colour = sex,
                fill = sex)) +
  geom_col(alpha = 0.7, 
           colour = "white") +
  theme_modern(base_family = "Inconsolata") + 
  labs(fill = "",  colour = "",
       x = "Share of Canadian Population in 2021",
       y = "Age Group") +
  scale_fill_manual(values = c("#C47376", "#73c4c1")) +
  scale_colour_manual(values = c("#C47376", "#73c4c1")) +
  scale_x_continuous(labels = function(x) {paste0(abs(x), "%")}) +
  theme(axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10),
                                    angle = 0),
        legend.position = "top",
        legend.text = element_text(size = 13))
