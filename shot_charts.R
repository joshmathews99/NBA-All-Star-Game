library(tidyverse)
library(ggimage)
library(jpeg)
library(grid)
library(gganimate)

load('shiny_app/data_complete.Rdata')

# Load basketball court image for use as scatterplot background. Saved to shiny
# app.
img <- jpeg::readJPEG('shiny_app/NBA_Court.jpg')

# Created fun animation of all shots from 1998 onwards. Won't use in shiny app
# due to long load time.
all_shots <- data_complete %>% 
  filter(yearSeason > 1997) %>%
  filter(!is.na(typeShot)) %>% 
  ggplot(aes(locationX, locationY, colour = typeShot, group = yearSeason, shape = isShotMade)) + 
  annotation_custom(rasterGrob(img, 
                               width = unit(.935,"npc"), 
                               height = unit(.935,"npc")), 
                    -Inf, Inf, -Inf, Inf) + 
  geom_point(alpha = 0.35, na.rm = TRUE) + 
  scale_y_continuous(limits = c(-52, 418)) + 
  scale_x_continuous(limits = c(-250, 250)) + 
  theme_classic() + 
  scale_shape_manual(values=c(1, 16), labels = c('Missed', 'Made')) + 
  theme(axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title = element_blank(), axis.line = element_blank(),
        legend.title = element_blank()) + 
  transition_states(yearSeason, transition_length = 2, state_length = 3) + 
  labs(title = 'Year: {closest_state}')

all_shots_animation <- animate(all_shots, end_pause = 30)

all_shots_animation

# Trial for settings needed to produce pleasing graphic of shots. Will use
# similar settings in shiny app server.

data_complete %>% 
  filter(yearSeason > 1997) %>%
  filter(!is.na(typeShot)) %>% 
  ggplot(aes(locationX, locationY, colour = typeShot, group = yearSeason, shape = isShotMade)) + 
  annotation_custom(rasterGrob(img, 
                               width = unit(.935, "npc"), 
                               height = unit(.935, "npc")), 
                    -Inf, Inf, -Inf, Inf) + 
  geom_point(alpha = 1, size = 0.5, na.rm = TRUE) + 
  scale_y_continuous(limits = c(-52, 418)) + 
  scale_x_continuous(limits = c(-250, 250)) + 
  theme_classic() + 
  scale_shape_manual(values=c(1, 16), labels = c('Missed', 'Made')) + 
  theme(axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title = element_blank(), axis.line = element_blank(),
        legend.title = element_blank())

# Within the shiny app, this may be used to allow for comparison of multiple
# players' shot profiles over the years.
