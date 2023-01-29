# S061_autofluor.R : for scatter plots when there's too many points to show fluorescence as bars

# Clean sample names
plot_data <- 
  processed.data %>% 
  separate(Samples, into = 'Samples') %>% 
  filter(day == 2)
  # filter(str_detect(Samples, 'a'))

library(patchwork)
title_name <- 'S061_WW isolates autofluor'

pos = position_jitter(width = 0.25, height = 0, seed = 1) # give position jitter

pred <- 
  ggplot(plot_data, 
       aes(x = day, y = `RFP`, label = Samples)) + 
  geom_point(position = pos, colour = 'red') + 
  
  ggrepel::geom_text_repel(position = pos)


pgr <- 
  ggplot(plot_data, 
         aes(x = day, y = `GFP`, label = Samples)) + 
  geom_point(position = pos, colour = 'dark green') + 
  
  ggrepel::geom_text_repel(position = pos)

pod <- 
  ggplot(plot_data, 
         aes(x = day, y = OD, label = Samples)) + 
  geom_point(position = pos, colour = 'burlywood') + 
  
  ggrepel::geom_text_repel(position = pos)

pred + pgr + pod + 
  plot_annotation(title = title_name, subtitle = 'natural / autofluorescence')

ggsave(plot_as(title_name, '-d2'), width = 8, height = 4)
