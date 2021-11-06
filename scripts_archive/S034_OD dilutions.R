
# collect data ----

# gather plate reader data
pld <- 
  processed.data %>% 
  mutate(Dilution_fold = 2^(`Replicate #` - 1), .keep = 'unused', .before = 1) %>%  # add fold dilutions
  select(!contains("_mean")) %>% # remove unnecessary cols having means
  rename(Strain = Samples) %>% 
  mutate(Measurement = 'Plate reader')
  
# gather spectrophotometer data
specd <- readODS::read_ods(path = str_c('plate reader data/', 'OD_spectrophotometer','.ods')) %>% 
  pivot_longer(cols = c('Cary', 'Denovix'), names_to = 'Measurement', values_to = 'OD') %>% 
  arrange(Measurement, Strain)

# pile em up
alldata <- bind_rows(pld, specd) %>% 
  mutate(concentration = 1/Dilution_fold) %>% 
  group_by(Measurement, Strain) 
  
  


# linear regressions
# fitdata <- 
#   expan (alldata, lm(OD ~ concentration)
# broom::tidy(fit)

# plotting ----

plt_full <-  
  ggplot(alldata, 
         aes(x = concentration, y = OD, colour = Measurement)) +
  geom_point() +
  # geom_line() +
  geom_smooth(method = 'lm', show.legend = F) +
  
  facet_grid(Strain ~ . , scales = 'free_y') + 
  ggtitle('Full range: 1 - 2048 fold')

plt_subset <-  
  ggplot(alldata %>% filter(concentration < 0.5), 
         aes(x = concentration, y = OD, colour = Measurement)) +
  geom_point() +
  # geom_line() +
  geom_smooth(method = 'lm', show.legend = F) +

  facet_grid(Strain ~ . , scales = 'free_y') +
  ggtitle('Dilutions : 4 - 2048 fold', subtitle = '4 fold dilution is ideal')

plt_subset1 <-  
  ggplot(alldata %>% filter(concentration <= 0.5), 
         aes(x = concentration, y = OD, colour = Measurement)) +
  geom_point() +
  # geom_line() +
  geom_smooth(method = 'lm', show.legend = F) +
  
  facet_grid(Strain ~ . , scales = 'free_y') +
  ggtitle('Dilutions : 2 - 2048 fold')


plt_subset_logx <-  
  ggplot(alldata %>% filter(concentration < 0.5), 
         aes(x = concentration, y = OD, colour = Measurement)) +
  geom_point() +
  # geom_line() +
  # geom_line(stat = 'smooth', method = 'lm', alpha = 0.4, show.legend = F) +
  geom_smooth(method = 'lm', show.legend = F) + 
  
  facet_grid(Strain ~ . , scales = 'free_y') +
  coord_trans(x = 'log10') + 
  ggtitle('Logscale : dilutions 4 - 2048 fold')

library(patchwork)

(plt_full | plt_subset) / 
  (plt_subset1 | plt_subset_logx) + 
  plot_layout(guide = 'collect')

ggsave('plate reader analysis/plots and data/S034_OD dilutions.png', width = 8, height = 6)

plt_subset_logx
# format_logscale_x(plt_subset_logx) %>% print()

ggplotly(plt1, dynamicTicks = TRUE)

# logscale on X
plt1 %>% 
  format_logscale_x() %>% 
  print()

