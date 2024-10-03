# ------------------------------------------------------------------------------

# Day 3: Visualizing Statistical Quantities
# PopAging DataViz
# Sakeef M. Karim
# Companion Script File
# CC-BY-SA 4.0

# PRELIMINARIES ----------------------------------------------------------------

library(tidyverse)
library(systemfonts)


# Packages for estimating models, visualizing results:

library(estimatr)
library(ggeffects)
library(marginaleffects)
library(modelsummary)
library(effects)

# Misc:

library(gtsummary)
library(gt)
library(ggthemes)
library(hrbrthemes)
library(gssr)
library(panelr)
library(skimr)


# LOADING THE DATA -------------------------------------------------------------

load("./data/day3.RData")

# FITTING MODELS, VISUALIZING RESULTS ------------------------------------------

# Basic Linear Model -----------------------------------------------------------

mod_gapminder_1 <- lm(lifeExp ~ log_pop + log_gdpPercap + continent,
                      data = gapminder_2007)


# Quick Tables

tbl_regression(mod_gapminder_1)

modelsummary(mod_gapminder_1)

# Tidying Results --------------------------------------------------------------

mod_gapminder_1 %>% tidy()

# Quick, Manual Coefficient Plot -----------------------------------------------

mod_gapminder_1 %>% tidy(conf.int = TRUE) %>% 
                    # Zeroing in on variables of interest:
                    filter(str_detect(term, "log")) %>% 
                    ggplot() +
                    geom_pointrange(mapping = aes(x = estimate,
                                                  y = term,
                                                  xmin = conf.low,
                                                  xmax = conf.high)) +
  geom_vline(xintercept = 0,
             linetype = "dotted")
                    
# Spend 10 minutes beautifying this basic visualization

# modelplot (via modelsummary) -------------------------------------------------

modelplot(mod_gapminder_1) 

# Customizing the plot

modelplot(mod_gapminder_1,
          # Omitting specific variables (e.g., controls, fixed effects etc.)
          coef_omit = "Interc|contin",
          # 99% confidence intervals:
          conf_level = 0.99) +
geom_vline(xintercept = 0,
           linetype = "dotted") +
    # Shading in "pointranges" based on significance level:
aes(color = ifelse(p.value < 0.01, "Significant", "Not significant")) +
scale_colour_manual(values = c("grey", "red"))

# A more "complicated" model --- but not necessarily a good one:
    
wage_panelr %>% skim()

mod_wages_1 <- lm_robust(lwage ~ years_worked + female +
                         black + blue_collar + south + union + 
                         manufacturing + wave,
                         data = wage_panelr,
                         clusters = id,
                         # Produces robust clustered SEs (via the Delta method, 
                         # as in Stata):
                         se_type = "stata") 
  
# Labels for coefficients:

wages_labels <- c("manufacturing1" = "Manufacturing",
                  "union1" = 'Union Member', 
                  "south1" = "Southerner",
                  "black1" = "Black",
                  "female1" = "Female",
                  "years_worked" = "Years in Workforce",
                  "blue_collar1" = "Blue Collar Worker")
    

mod_wages_1 %>% modelplot(conf_level = 0.99,
                          #String search:
                          coef_omit = 'Interc|wave',
                          colour = "red",
                         # Renaming coefficients:
                          coef_map = wages_labels) +
              theme_bw(base_family = "Inconsolata") +
              # Adding reference point for "non-significant" finding:
              geom_vline(xintercept = 0, 
                         linetype = "dashed") +
              labs(x = "Coefficient Estimates",
                   title = "Predicting the Log of Wages",
                   subtitle = "With 99% Confidence Intervals") 


# Additional control:

mod_wages_2 <- lm_robust(lwage ~ years_worked + years_education + female + black +
                         blue_collar + south + union + manufacturing + wave,
                         data = wage_panelr,
                         clusters = id,
                         se_type = "stata") 

# Adjusting our labels:

wages_labels <- c(wages_labels, "years_education" = "Education (Years)")

mod_wages_2 %>% modelplot(conf_level = 0.99,
                          coef_omit = 'Interc|wave',
                          colour = "red",
                          coef_map = wages_labels) +
                theme_bw(base_family = "Inconsolata") +
                geom_vline(xintercept = 0, 
                           linetype = "dashed") +
                labs(x = "Coefficient Estimates",
                     title = "Predicting the Log of Wages",
                     subtitle = "With 99% Confidence Intervals") 

# Comparing models

wage_models <- list("Base Model" = mod_wages_1,
                    "Full Model" = mod_wages_2)

wage_models %>% modelplot(coef_omit = 'Interc|wave|edu',
                          size = 1,
                          linewidth = 1,
                          coef_map = wages_labels) +
                # Adjusting colour scale:
                scale_colour_brewer(palette = "Set2") +
                # From ggthemes
                theme_wsj(base_family = "Inconsolata") +
                theme(legend.title = element_blank()) +
                geom_vline(xintercept = 0,
                           linetype = "dashed")
  
# Using facets

rev(wage_models) %>% modelplot(coef_omit = 'Interc|wave|edu',
                               # Matches arguments for geom_pointrange()
                               size = 1,
                               linewidth = 1,
                               coef_map = wages_labels,
                               colour = "skyblue",
                               facet = TRUE) +
  theme_wsj(base_family = "Inconsolata") +
  theme(legend.title = element_blank(),
        strip.text.y = element_text(angle = 0)) +
  geom_vline(xintercept = 0,
             linetype = "dashed")


# ADJUSTED PREDICTIONS ---------------------------------------------------------

# ggeffects --------------------------------------------------------------------

# Fitting new model with interaction term:

mod_gapminder_2 <- lm(lifeExp ~ log_pop + continent*log_gdpPercap,
                      data = gapminder_2007)

# Using the ggeffect function:

ggeffect(mod_gapminder_2, 
         # Predicted life expectancy at logged GDP per capita of 6 to 11
         # for countries in Europe and Africa:
         terms = c("log_gdpPercap [6:11]", "continent [Europe, Africa]")) %>% 
         plot() +
         labs(title = "Predicted Values of Life Expectancy", 
              x = "Log of Per Capita GDP",
              y = "Life Expectancy (Years)",
              fill = "",
              colour = "") +
          theme_ggeffects(base_family = "IBM Plex Sans") +
          scale_color_economist() +
          scale_fill_economist() +
          theme(legend.position = "bottom") +
          # Overriding default alpha of confidence intervals:
          guides(fill = guide_legend(override.aes =
                                       list(alpha = 0.35)))

# The longer route:

mod_gapminder_2 %>% ggeffect(terms = c("log_gdpPercap [6:11]",
                                       "continent [Europe, Africa]")) %>% 
                    as_tibble() %>% 
                    ggplot(mapping = aes(x = x, 
                                         y = predicted, 
                                         ymin = conf.low,
                                         ymax = conf.high,
                                         # group corresponds to discrete term:
                                         colour = group,
                                         fill = group)) +
                    geom_line() +
                    # For confidence intervals:
                    geom_ribbon(alpha = 0.5) 

# Can you adjust this plot so that it (more or less) matches the previous one?

# via {marginaleffects} --------------------------------------------------------

plot_predictions(mod_gapminder_2, 
                 condition = list("log_gdpPercap" = 6:11,
                                  "continent" = c("Africa", "Europe"))) +
  theme_ggeffects(base_family = "IBM Plex Sans") +
  labs(fill = "", colour = "",
       x = "Log of GDP per Capita",
       y = "Life Expectancy")
                   
# The longer route ...
                 
avg_predictions(mod_gapminder_2,
                variables = list(log_gdpPercap = c(6:11),
                                 continent = c("Africa", "Europe"))) %>% 
ggplot(mapping = aes(x = log_gdpPercap, 
                     y = estimate, 
                     ymin = conf.low,
                     ymax = conf.high,
                     colour = continent,
                     fill = continent)) +
geom_line() +
geom_ribbon(alpha = 0.5)  


# AVERAGE MARGINAL EFFECTS -----------------------------------------------------

mod_wages_2 %>% tbl_regression()

avg_slopes(mod_gapminder_2,
           # Focal variable:
           variables = "log_gdpPercap",
           # Across levels of ...
           by = "continent")

plot_slopes(mod_gapminder_2,
            variables = "log_gdpPercap",
            condition = "continent") + 
theme_ggeffects(base_family = "IBM Plex Sans")


  
# EXPLORE GSS_2010 -------------------------------------------------------------

gss_2010

# Other years:

# gss_get_yr(****)