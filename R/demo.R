## @knitr loadps

library(ggplot2)
library(readr)
library(dplyr)
library(here)
library(haven)

## @knitr readdata

toth <- here::here('data/Toth2008GJT.sav') %>%
  read_spss()


## @knitr knowdata



## @knitr boxplots

# name the object box
# x = respondent_wall_type, y = rooms
# add jitter, width = .20, alpha = .60

box <- ggplot(data = int_plot, aes(x = respondent_wall_type,
                                   y = rooms)) +
  geom_boxplot() +
  geom_jitter(width = .20, alpha = .60)


## @knitr scatterplots

# name the plot scatter
# x = no_membrs, y = number_items
# start with geom_point()
# iteratively add geom_jitter(alpha = 0.5, aes(color = village))

scatter <- ggplot(int_plot, aes(x = no_membrs, 
                                y = number_items,
                                color = village)) +
  geom_point() +
  geom_jitter(alpha = .5) +
  ylab("Number of Items") +
  xlab("Number of People in Family") +
  ggtitle("Number of Items by People") +
  scale_color_viridis_d(option = 'inferno')

## @knitr barplots

# name the object bar
# use var respondent_wall_type for x, fill, and color; add scale_fill_v & scale_color_v
# add theme(legend.position = 'none')

bar <- ggplot(int_plot, aes(x = respondent_wall_type, 
                            fill = respondent_wall_type,
                            color = respondent_wall_type)) +
  geom_bar() +
  scale_color_viridis_d() +
  scale_fill_viridis_d()
bar
## @knitr faceting

# facet by village
# x = respondent_wall_type, fill = respondent_wall_type
# add dodge to second plot, proportions to third, use viridis for all

bar_vil <- ggplot(int_plot, aes(x = respondent_wall_type, 
                                fill = respondent_wall_type,
                                color = respondent_wall_type)) +
  geom_bar() +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  facet_wrap(~ village)

# show proportions
# filter(respondent_wall_type != "cement") %>%
#   count(village, respondent_wall_type) %>%
#   group_by(village) %>%
#   mutate(percent = n / sum(n)) %>%
#   ungroup()

# facet by items

percent_items <- int_plot %>%
  # select variable needed and turn dataframe into long format
  gather(items, items_owned_logical, bicycle:no_listed_items)%>%
  # keep observations in which the items are owned
  filter(items_owned_logical) %>%
  # count the items by village; this creates the n variable
  count(items, village) %>%
  # add a column with the number of people in each village
  mutate(people_in_village = case_when(village == "Chirodzo" ~ 39,
                                       village == "God" ~ 43,
                                       village == "Ruaca" ~ 49)) %>%
  # make a percentage column
  mutate(percent = n / people_in_village)

# add themes

# show theme_minimal first
theme_evergreen_s <- theme_minimal() + theme(axis.ticks = element_blank(),
                                             panel.grid = element_blank())

theme_set(theme_evergreen_s)

## @knitr timeseries

# air <- economics %>%
#   ggplot(., aes(x = date, y = pop)) +
#   geom_line()
# 
# air_two <- economics %>%
#   select(date, psavert, uempmed) %>%
#   gather(key, value, -date) %>%
#   ggplot(., aes(x = date, y = value, color = key)) +
#   geom_line()
