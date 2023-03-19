# S066-67_analysis.R

# Prelims ----

source('0.5-user_inputs.R') # get user inputs
source('general_functions_plate_reading.R') # source the file that contains all the functions


# Load data ----

processed.data <- read_csv(str_c('plate reader data/processed/', flnm, '-processed.csv'))
  

# S066 analysis ----

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
      
      # Custom changing scale per facet : https://teunbrand.github.io/ggh4x/reference/facetted_pos_scales.html 
      # ggh4x::facetted_pos_scales(
      #   x = list(sample_type == 'Control' ~ 
      #              scale_x_continuous(limits = c(1e-3, 1e3), breaks = 1))
      #   ) +
      
      # theme(panel.background = element_blank()) + # hide background to show text better
      
      # Labels
      geom_text(aes(label = Samples, y = minval * 0.8),
                data = filter(S66_dat, sample_type == 'Control') %>% 
                  summarise(.by = c(Samples, Arabinose, sample_type), minval = min(`GFP/OD`) )) + 
      
      ggtitle('Arabinose : d0', subtitle = title_name)
      
      } %>% 
  
  format_logscale_x() %>% format_logscale_y() %>%
  print()

ggsave(plot_as(title_name), width = 6, height = 5)
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
