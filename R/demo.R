## @knitr loadps

library(tidyverse)
library(here)
library(haven)

## @knitr readdata

toth <- here::here('data/Toth2008GJT.sav') %>%
  read_spss() %>%
  mutate(Group = factor(Group))


## @knitr boxplots

# name the object box
# x = respondent_wall_type, y = rooms
# add jitter, width = .20, alpha = .60

box <- ggplot(toth, aes(x = Group, y = GJTpretest, fill = Group)) +
  geom_boxplot() +
  geom_jitter(width = .20, alpha = .60) +
  scale_fill_viridis_d(alpha = .30) 



## @knitr scatterplots

# name the plot scatter
# x = no_membrs, y = number_items
# start with geom_point()
# iteratively add geom_jitter(alpha = 0.5, aes(color = village))

scatter <- ggplot(toth, aes(x = GJTpretest, y = GJTposttest, color = Group)) +
  geom_point() +
  scale_color_viridis_d(option = 'inferno')

## @knitr tables

toth_long <- toth %>%
  gather(key = Test, value = Score, -subject, -Group)

toth_sum <- toth_long %>%
  group_by(Group, Test) %>%
  summarise(N = n(),
            Mean = mean(Score),
            SD = sd(Score))

toth_pre <- filter(toth_sum, Test %in% 'GJTpretest')
toth_pst <- filter(toth_sum, Test %in% 'GJTposttest')
toth_del <- filter(toth_sum, Test %in% 'GJTdelayed')

