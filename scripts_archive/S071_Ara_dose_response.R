# S071_Ara.R

dose_data <- 
  separate(processed.data, Samples, into = c('Samples', 'day'), sep = '_') %>% 
  mutate(sample_type = if_else(str_detect(Samples, 'glu|OFF|ON'), 'Controls', 'Induction'), # mark controls
         Arabinose = as.numeric(Samples), .after = Samples) # convert to numeric for plotting properly

induction_samples <- filter(dose_data, sample_type == 'Induction')

# plotting ----

ara_plt <- 
  {ggplot(induction_samples,
          aes(Arabinose, `GFP/OD`, colour = day, label = `Replicate #`)) + 
      
      geom_point() + 
      
      geom_line(aes(group = interaction(day, `Replicate #`)), alpha = 0.2) +
      # geom_line(aes(y = `GFP/OD_mean`),
      #           data = select(induction_samples, Arabinose, day, sample_type, `GFP/OD_mean`, `Replicate #`) %>%
      #             unique) +
      
      theme(legend.position = 'top') + 
      ggtitle(title_name)
    
  } %>% 
  
  format_logscale_x() %>% # format_logscale_y() %>%
  print()


plotly::ggplotly(ara_plt)

ggsave(plot_as(title_name, '-linear'), width = 5, height = 5)


control_plt <- 
  {ggplot(filter(dose_data, sample_type == 'Controls'),
          aes(Samples, `GFP/OD`, colour = day, label = `Replicate #`)) +
  geom_point(position = position_jitter(width = 0.2, height = 0)) + 
      theme(legend.position = 'top') + 
      ylab(NULL) + xlab('Controls')} %>% print

# ggsave(plot_as(title_name, '-controls'), width = 2, height = 4)


library(patchwork)

ara_plt + control_plt + 
  plot_layout(widths = c(4, 1))

ggsave(plot_as(title_name), width = 6, height = 4)
