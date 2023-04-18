# S066-67_analysis.R

# Prelims ----

source('0.5-user_inputs.R') # get user inputs
source('general_functions_plate_reading.R') # source the file that contains all the functions

flnm <- 'S066-67_Ara-d0_memory WW-d0_18-3-23'

# Load data ----

processed.data <- read_csv(str_c('plate reader data/processed/', flnm, '-processed.csv'))
  

# S066 analysis ----
# updated analysis for dose response at `dose_response_custom_analysis.R`

# plan : make controls as a sample type, facet by it, and add text labels below the points for controls!

S66_dat <- filter(processed.data, str_detect(Samples, 'S066')) %>% 
  mutate(across(Samples, 
                ~ str_remove(., '_S06.')),
         
         Arabinose = as.numeric(Samples), # make a new column for [inducer]
         
         sample_type = if_else(is.na(Arabinose), 'Control', 'Induction'),
         across(Arabinose, ~ replace_na(., 1)) )  
  
title_name <- 'S066_Ara_d0' # change title_name for the dataset

# plot 

ara_plt <- 
  {ggplot(S66_dat,
          aes(Arabinose, `GFP/OD`)) + 
      
      geom_point() + 
      
      geom_line(aes(y = `GFP/OD_mean`),
                data = select(S66_dat, Samples, Arabinose, sample_type, `GFP/OD_mean`) %>% 
                  unique %>% filter(sample_type == 'Induction')) + 
      
      # Facets
      facet_grid(cols = vars(sample_type), 
                 scales = 'free_x', space = 'free_x') +
      
      
      
      # theme(panel.background = element_blank()) + # hide background to show text better
      
      # Labels
      geom_text(aes(label = Samples, y = minval * 0.8),
                data = filter(S66_dat, sample_type == 'Control') %>% 
                  summarise(.by = c(Samples, Arabinose, sample_type), minval = min(`GFP/OD`) )) + 
      
      ggtitle('Arabinose : d0', subtitle = title_name)
      
      } %>% 
  
  format_logscale_x() %>% # format_logscale_y() %>%
  print()


ggsave(plot_as(title_name, '-linear'), width = 6, height = 5)
# ggsave(str_c('plate reader analysis/plots and data/archive/', title_name, '.pdf'))


# S067 analysis ----

S67_dat <- filter(processed.data, str_detect(Samples, 'S067')) %>% 
  mutate(across(Samples, 
                ~ str_remove(., '_S06.'))) %>% 
         
         separate(Samples, into = c('Organism', 'Inducer'), sep = '\\.') %>% 
  
  # Create distinction for memory vs direct
  mutate(category = if_else(str_detect(Organism, '143'),
                            'Direct', 'Memory'))
  
         
        
title_name <- 'S067_WW_memory_d0' # change title_name for the dataset


# plot
ggplot(S67_dat,
       aes(`RFP/OD`, Organism, colour = Inducer)) + 
  
  geom_point(size = 3) + 
  
  # line like a dumbell plot
  geom_line(aes(group = Organism), colour = 'black', alpha = 0.3) + 
  
  facet_grid(vars(category), scale = 'free_y', space = 'free_y') +
  theme(legend.position = 'top') +
  
  # labels
  ggtitle('Memory in wastewater', subtitle = title_name)

ggsave(plot_as(title_name), width = 5, height = 4)


# S067b analysis ----
# This is from different datasets

# Prelims ----
source('general_functions_plate_reading.R') # source the file that contains all the functions

# Load data b1, b2 ----

flnms <- c('S067b1_143 memory ww d-1-d2_24-3-23', 'S067b2_79 memory ww d-1-d2_24-3-23')

processed.data <- 

  map_dfr(flnms, 
          ~ read_csv(str_c('plate reader data/processed/', .x, '-processed.csv'))) %>% 
  # Create distinction for memory vs direct
  mutate(category = if_else(str_detect(Samples, '143'),
                            'Direct', 'Memory')) %>% 
  
  mutate(across(Samples, ~ str_remove(., '^79/|^143/'))) %>%  # remove plasmid names
  
  mutate(across(c(`AHL (uM)`, Day), as_factor)) # make inducer and days factor for ease of plotting colours

sample_specific_variables <- c('Samples', 'Day', 'AHL (uM)')


# Analysis 79/b2 ----

title_name <- 'S067b2_79 memory ww d-1-d2'

# processing ----

S67b2 <- filter(processed.data, category == 'Memory') # select only 79 ones

# arrangement in ascending order
sample_order <-  
  select(S67b2,
         Samples, `RFP/OD`) %>% # select only required columns
  group_by(Samples) %>% # group
  
  mutate(maxsignal = max(`RFP/OD`)) %>% # make a column for max in each Sample (across days, inducers)
  
  # filter(S67b2, `AHL (uM)` == 10) %>% # select required samples
  
  arrange(desc(maxsignal)) %>% # arrange in descending order
  ungroup() %>% 
  mutate(across(Samples, fct_inorder)) %>% # remove groups to make a factor
  
  pull(Samples) %>% levels() %>% rev # grab unique samples and make ascending order

S67b2_order <- mutate(S67b2, across(Samples, ~ fct_relevel(., sample_order)))


# plot b ----

# Samples vs signal: plot of S067b2
ggplot(S67b2_order,
       aes(`RFP/OD`, Samples, colour = Day, shape = `AHL (uM)`)) + 
  
  geom_point(size = 2) +
  # scale_colour_brewer(palette = 'Dark2', direction = -1) + # change the values - orange for uninduced/0
  scale_shape_manual(values = c(1, 16)) + # shape : open and closed circles
  
  # line like a dumbell plot
  geom_line(aes(group = Samples), colour = 'black', alpha = 0.3) + 
  
  # facet_grid(vars(category), scale = 'free_y', space = 'free_y') +
  theme(legend.position = 'top') +
  
  # labels
  ggtitle('Memory in wastewater', subtitle = title_name)

ggsave(plot_as(title_name), width = 4, height = 6)


# timeseries plot ----


# Small multiples-timeseries plot
processed.data %<>% mutate(across(Samples, ~ fct_relevel(., rev(sample_order)))) # order in desc of max

title_name <- 'S067b wastewater memory II'

filter(processed.data, category == 'Memory') %>% 
  
  ggplot(aes(Day, `RFP/OD`, colour = category, shape = `AHL (uM)`)) + 
  
  geom_point(size = 2) +
  # scale_colour_brewer(palette = 'Dark2', direction = -1) + # change the values - orange for uninduced/0
  scale_shape_manual(values = c(1, 16)) + # shape : open and closed circles
  
  # line like a dumbell plot
  geom_line(aes(group = interaction(`AHL (uM)`, category)), colour = 'black', alpha = 0.3) +  
  
  facet_wrap(vars(Samples), scale = 'free_y') +
  theme(legend.position = 'top') +
  
  # labels
  ggtitle('Memory in wastewater', subtitle = title_name)

ggsave(plot_as(title_name, '-timeseries-memory'), width = 5, height = 5)
