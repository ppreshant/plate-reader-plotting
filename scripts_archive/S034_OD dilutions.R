# Adhoc script for S034 analysis of the linearity range of OD measrements


# collect data ----

# gather plate reader data
pld <- 
  # input file from processed data
  read_csv(file = 'plate reader data/processed/S034_OD_plate reader_serial dils-processed.csv') %>% 
  
  mutate(Dilution_fold = 2^(`Replicate #` - 1), .keep = 'unused', .before = 1) %>%  # add fold dilutions
  select(!contains("_mean"), -X1) %>% # remove unnecessary cols having means
  rename(Strain = Samples) %>% 
  mutate(Measurement = 'Plate reader')
  
# gather spectrophotometer data from .ods file
specd <- readODS::read_ods(path = str_c('plate reader data/', 'S034_OD_spectrophotometer','.ods')) %>% 
  pivot_longer(cols = c('Cary', 'Denovix'), names_to = 'Measurement', values_to = 'OD') %>% 
  arrange(Measurement, Strain)

# pile em up
alldata <- bind_rows(pld, specd) %>% 
  mutate(concentration = 1/Dilution_fold) %>% 
  group_by(Measurement, Strain) 
  
  

# plotting ----

plt_full <-  # plot full range of dilutions
  ggplot(alldata, 
         aes(x = concentration, y = OD, colour = Measurement)) +
  geom_point() +
  # geom_line() +
  geom_smooth(method = 'lm', show.legend = F) +
  
  facet_grid(Strain ~ . , scales = 'free_y') + 
  ggtitle('Full range: 1 - 2048 fold')

plt_subset_4x <-  # plot dilutions 4 fold onwards
  ggplot(alldata %>% filter(concentration < 0.5), 
         aes(x = concentration, y = OD, colour = Measurement)) +
  geom_point() +
  # geom_line() +
  geom_smooth(method = 'lm', show.legend = F) +

  facet_grid(Strain ~ . , scales = 'free_y') +
  ggtitle('Dilutions : 4 - 2048 fold', subtitle = '4 fold dilution is ideal')

plt_subset_2x <-  # plot dilutions 2 fold onwards
  ggplot(alldata %>% filter(concentration <= 0.5), 
         aes(x = concentration, y = OD, colour = Measurement)) +
  geom_point() +
  # geom_line() +
  geom_smooth(method = 'lm', show.legend = F) +
  
  facet_grid(Strain ~ . , scales = 'free_y') +
  ggtitle('Dilutions : 2 - 2048 fold')


plt_subset_logx_4x <-  
  ggplot(alldata %>% filter(concentration < 0.5), 
         aes(x = concentration, y = OD, colour = Measurement)) +
  geom_point() +
  # geom_line() +
  # geom_line(stat = 'smooth', method = 'lm', alpha = 0.4, show.legend = F) +
  geom_smooth(method = 'lm', show.legend = F) + 
  
  facet_grid(Strain ~ . , scales = 'free_y') +
  coord_trans(x = 'log10') + 
  ggtitle('Logscale : dilutions 4 - 2048 fold')


# log log plot full
plt_logxy <- 
  
  {plt_full + 
      ggtitle('Full range : 1 - 2048 fold : Log-log scale')} %>% 
  format_logscale_x() %>% # this fits data after the log-transform
  format_logscale_y() %>%  # unable to use coord_trans on y
  print


# truncated dilutions from manual eyeballing
plt_subset_arbit <-  # plot dilutions 2 fold - 32  (2:6)
  ggplot(alldata %>% filter(Dilution_fold >= 2 & Dilution_fold <= 32), 
         aes(x = concentration, y = OD, colour = Measurement)) +
  geom_point() +
  # geom_line() +
  geom_smooth(method = 'lm', show.legend = F) +
  
  facet_grid(Strain ~ . , scales = 'free_y') +
  ggtitle('Dilutions : 2 - 32 fold')

plt_subset_arbit_logxy <- 
  
  {plt_subset_arbit + 
      ggtitle('Dilutions : 2 - 32 fold : Log-log scale')} %>% 
  format_logscale_x() %>% # this fits data after the log-transform
  format_logscale_y() %>%  # unable to use coord_trans on y
  print


library(patchwork)

# put together multiple plots for easy visualization
(plt_full | plt_subset_4x) / 
  (plt_subset_2x | plt_subset_logx_4x) + 
  plot_layout(guide = 'collect')

ggsave('plate reader analysis/plots and data/S034_OD dilutions.png', width = 8, height = 6)

# log log patchwork
plt_loglogs <- 
  plt_logxy + plt_subset_arbit_logxy + 
  plot_layout(guide = 'collect')


# save log log plot
ggsave('plate reader analysis/plots and data/S034_OD dilutions_loglog.png', 
       plot = plt_loglogs, 
       width = 7, height = 5)




# interactive plots ----


# interactive log log -- most useful
ggplotly(plt_logxy, dynamicTicks = TRUE)

# all data
ggplotly(plt_full, dynamicTicks = TRUE)


# linear regressions ----


fit_full <-
  
  alldata %>% 
  nest() %>% 
  
  mutate(
    fit = map(data, # map linear regression across each group
              ~ lm(formula = OD ~ concentration,
                   data = .x,
                   subset = 2:6
                   )),
    
    # get model fit information
    params = map(fit,
                 ~ broom::tidy(.x)),  
    
    # make equation
    equation = 
      map_chr(params, 
              
              # y = mx + c
              ~ glue::glue('y = {format(.x[[2,2]], digits = 3)} x + {format(.x[[1,2]], digits = 3)}')),
    
    # get R square values
    Rsquare = 
      map_dbl(fit,
              ~ broom::glance(.x)$r.squared)
  )

# see fit summary
fit_full


# plot + linear regression ----



# testing 10-plot_scatter.R : make sure it is sourced

# does not work.. problem with grouping by two or more variables

# plt_sctr <- 
#   {plot_scatter(.data = alldata,
#                x_var = Dilution_fold, y_var = OD,
#                colour_var = Measurement,
#                already_pivoted_data = 'yes',
#                grouping_var = c(Measurement, Strain) ) +
#   
#   facet_grid(Strain ~ . , scales = 'free_y')} %>% 
#   
#   print()

