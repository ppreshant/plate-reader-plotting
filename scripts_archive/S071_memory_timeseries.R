# S071_memory_timeseries.R

# Prelims ----

source('0.5-user_inputs.R') # get user inputs
source('general_functions_plate_reading.R') # source the file that contains all the functions

# override filename and title name from user inputs 
title_name <- 'S071_ww timeseries'
flnms <- c('S070_S071_d0_13-April-23', 
           'S071_d1,2_24-4-24',
           'S071_d3, 8_24-4-24',
           'S071_d5_79_19-4-24',
           'S071_d5_143_24-4-24')

# User inputs ----
# translate plasmid names (assay_variable) into meaningful names
plasmid_translation <- c('143' = 'direct reporter',
                         '79' = 'Memory')


# Load data ----

get_data <- get_processed_datasets(flnms) # load pre-processed data

# processing ----

processed_data <- 
  
  get_data %>% 
  
  # cleanup/subset data : d0 dataset
  filter(!str_detect(Samples, "_")) %>% # remove S070 samples (have "_" in them)
  replace_na(list(Day = 0)) %>% # add missing Day
  
  # split columns and cleanup names
  separate(Samples, into = c('plasmid', 'organism'), sep = '/', remove = FALSE) %>% 
  rename('AHL (uM)' = Inducer) %>% # rename Inducer column
  
  # translate plasmid name to be descriptive 
  mutate(across(plasmid, ~ str_replace_all(.x, plasmid_translation))) %>% 
  
  mutate(across(`AHL (uM)`, as_factor)) # make inducer and days factor for ease of plotting colours


# timeseries plot ----

plot_timeseries <- function(.data = processed_data)
{
  # filter(processed.data, plasmid == 'Memory') %>% 
  ggplot(.data,
         aes(Day, `RFP/OD`, colour = plasmid, shape = `AHL (uM)`)) + 
    
    geom_point(size = 2) +
    # scale_colour_brewer(palette = 'Dark2', direction = -1) + # change the values - orange for uninduced/0
    scale_shape_manual(values = c(1, 16)) + # shape : open and closed circles
    
    # lines connect each replicate
    geom_line(aes(group = interaction(`AHL (uM)`, plasmid, replicate)), colour = 'black', alpha = 0.3) +  
    
    
    
    facet_wrap(vars(organism), scale = 'free_y') + # ncol = 4
    theme(legend.position = 'top') +
    
    # labels
    ggtitle('Memory in wastewater', subtitle = title_name)
}

# plot all data 
plot_timeseries()
ggsave(plot_as(title_name), width = 5, height = 5)

# plot only uninduced : check for leak
processed_data %>% filter(`AHL (uM)` == 0) %>% plot_timeseries()
ggsave(plot_as(title_name, '-no induction'), width = 5, height = 5)
