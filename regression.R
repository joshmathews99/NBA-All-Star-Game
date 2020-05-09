
library(tidyverse)
library(broom)

load('shiny_app/data_complete.Rdata')

# Create popularity variable where higher value means more votes and 1 represents least votes.

pop <- data_complete %>% 
  select(yearSeason, namePlayer, minutes, votes) %>% 
  filter(yearSeason > 1974) %>% 
  distinct() %>% 
  mutate(votes = replace_na(votes, 0)) %>% 
  filter(minutes != 0) %>% 
  group_by(yearSeason) %>% 
  mutate(popularity = rank((votes)))

reg <- pop %>% 
  group_by(yearSeason) %>% 
  nest() %>% 
  mutate(mod = map(data, ~ lm(minutes ~ popularity, data = .))) %>% 
  mutate(reg_results = map(mod, ~ tidy(.))) %>% 
  mutate(coef = map_dbl(reg_results, ~ filter(., term == 'popularity') %>% pull(estimate))) %>% 
  mutate(se = map_dbl(reg_results, ~ filter(., term == 'popularity') %>% pull(std.error))) %>% 
  mutate(lower = coef - 1.96 * se,
         upper = coef + 1.96 * se)

ggplot(reg, aes(yearSeason, coef)) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), alpha = 0.8, colour = 'light blue') + 
  geom_point(alpha = 0.8) + 
  theme_classic() + 
  labs(x = 'All Star Game', y = 'Change in Minutes') + 
  geom_hline(yintercept = 0, linetype = 'dotted', colour = 'red')

# Attempt to re run this by team as minutes depend on this factor.

pop_team <- data_complete %>% 
  mutate(isWin = ifelse(isWin == FALSE, 'Team Lost', 'Team Won')) %>% 
  select(yearSeason, namePlayer, isWin, minutes, votes) %>% 
  filter(yearSeason > 1974) %>% 
  distinct() %>% 
  mutate(votes = replace_na(votes, 0)) %>% 
  filter(minutes != 0) %>% 
  group_by(yearSeason, isWin) %>% 
  mutate(popularity = rank((votes)))

reg_team <- pop_team %>% 
  group_by(yearSeason, isWin) %>% 
  nest() %>% 
  mutate(mod = map(data, ~ lm(minutes ~ popularity, data = .))) %>% 
  mutate(reg_results = map(mod, ~ tidy(.))) %>% 
  mutate(coef = map_dbl(reg_results, ~ filter(., term == 'popularity') %>% pull(estimate))) %>% 
  mutate(se = map_dbl(reg_results, ~ filter(., term == 'popularity') %>% pull(std.error))) %>% 
  mutate(lower = coef - 1.96 * se,
         upper = coef + 1.96 * se)

plot_regressions_mins <- ggplot(reg_team, aes(yearSeason, coef, colour = isWin)) + 
  facet_wrap(~ isWin) + 
  geom_errorbar(aes(ymin = lower, ymax = upper)) + 
  geom_point() + 
  theme_classic() + 
  labs(x = 'All Star Game', y = 'Change in Minutes', 
       title = 'Effect of Player Popularity on Minutes Played',
       subtitle = 'Change in minutes due to an increase in fan vote ranking by one.') + 
  geom_hline(yintercept = 0, linetype = 'dotted') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.subtitle = element_text(hjust = 0.5)) + 
  theme(legend.position = 'none') + 
  theme(strip.background = element_rect(colour = 'white')) + 
  scale_x_continuous(breaks = seq(1975, 2020, 5)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1 )) + 
  scale_y_continuous(breaks = seq(-2, 4, 0.5))

save(plot_regressions_mins, file = '~/Desktop/GOV 1005/projects/final_project/shiny_app/regressions_mins_plot.Rdata')

# Regression of voting popularity on points scored

pts_fga <- data_complete %>% 
  mutate(isWin = ifelse(isWin == FALSE, 'Team Lost', 'Team Won')) %>% 
  select(yearSeason, namePlayer, isWin, pts, fga, votes) %>% 
  filter(yearSeason > 1974) %>% 
  distinct() %>% 
  mutate(votes = replace_na(votes, 0)) %>% 
  group_by(yearSeason, isWin) %>% 
  mutate(popularity = rank(votes))

reg_pts <- pts_fga %>% 
  group_by(yearSeason, isWin) %>% 
  nest() %>% 
  mutate(mod = map(data, ~ lm(pts ~ popularity, data = .))) %>% 
  mutate(reg_results = map(mod, ~ tidy(.))) %>% 
  mutate(coef = map_dbl(reg_results, ~ filter(., term == 'popularity') %>% pull(estimate))) %>% 
  mutate(se = map_dbl(reg_results, ~ filter(., term == 'popularity') %>% pull(std.error))) %>% 
  mutate(lower = coef - 1.96 * se,
         upper = coef + 1.96 * se)

plot_regressions_pts <- ggplot(reg_pts, aes(yearSeason, coef, colour = isWin)) + 
  facet_wrap(~ isWin) + 
  geom_errorbar(aes(ymin = lower, ymax = upper)) + 
  geom_point() + 
  theme_classic() + 
  labs(x = 'All Star Game', y = 'Change in Points Scored', 
       title = 'Effect of Player Popularity on Points Scored',
       subtitle = 'Change in points scored due to an increase in fan vote ranking by one.') + 
  geom_hline(yintercept = 0, linetype = 'dotted') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.subtitle = element_text(hjust = 0.5)) + 
  theme(legend.position = 'none') + 
  theme(strip.background = element_rect(colour = 'white')) + 
  scale_x_continuous(breaks = seq(1975, 2020, 5)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1 )) + 
  scale_y_continuous(breaks = seq(-2, 4, 0.5))

save(plot_regressions_pts, file = '~/Desktop/GOV 1005/projects/final_project/shiny_app/regressions_pts_plot.Rdata')

# Regression of voting popularity on shots attempted

reg_fga <- pts_fga %>% 
  group_by(yearSeason, isWin) %>% 
  nest() %>% 
  mutate(mod = map(data, ~ lm(fga ~ popularity, data = .))) %>% 
  mutate(reg_results = map(mod, ~ tidy(.))) %>% 
  mutate(coef = map_dbl(reg_results, ~ filter(., term == 'popularity') %>% pull(estimate))) %>% 
  mutate(se = map_dbl(reg_results, ~ filter(., term == 'popularity') %>% pull(std.error))) %>% 
  mutate(lower = coef - 1.96 * se,
         upper = coef + 1.96 * se)

plot_regressions_fga <- ggplot(reg_fga, aes(yearSeason, coef, colour = isWin)) + 
  facet_wrap(~ isWin) + 
  geom_errorbar(aes(ymin = lower, ymax = upper)) + 
  geom_point() + 
  theme_classic() + 
  labs(x = 'All Star Game', y = 'Change in Field Goals Attempted', 
       title = 'Effect of Player Popularity on Field Goals Attempted',
       subtitle = 'Change in field goals attempted due to an increase in fan vote ranking by one.') + 
  geom_hline(yintercept = 0, linetype = 'dotted') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.subtitle = element_text(hjust = 0.5)) + 
  theme(legend.position = 'none') + 
  theme(strip.background = element_rect(colour = 'white')) + 
  scale_x_continuous(breaks = seq(1975, 2020, 5)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1 )) + 
  scale_y_continuous(breaks = seq(-2, 4, 0.5))

save(plot_regressions_fga, file = '~/Desktop/GOV 1005/projects/final_project/shiny_app/regressions_fga_plot.Rdata')



