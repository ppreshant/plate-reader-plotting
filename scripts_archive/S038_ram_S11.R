# S038_ram_S11.R

# Data : S038_47+67_23-2-22
# Prelims ----

source('general_functions_plate_reading.R') # source the file that contains all the functions
MEFL_normalization <- F # needed for plotting

flnm <- 'S038_47+67_23-2-22'

output_path <- '../../Writing/RAM paper outputs/Archive'


# Load data ----

processed.data <- read_processed_data(flnm)
# or 
# run_single_plate.R until line 26


# Processing ----

# select subset of data
selected_data <- filter(processed.data, str_detect(Samples, '47')) %>% 
  droplevels() %>%  # clean up unused levels in the facor
  
  # normalize to mean of max
  mutate(maxmean_redod = max(`RFP/OD_mean`), # store the max mean
         `RFP/OD normalized` = `RFP/OD` / maxmean_redod) %>% # make a normalized column
  
  group_by(Samples) %>% mutate(`RFP/OD normalized_mean` = mean(`RFP/OD normalized`)) %>% ungroup()
         
  

# plot ----
pltredd <- plot_static_fluorescence(`RFP/OD`, .data = selected_data, interactive_plot = F)
# plot_raw_static_fluorescence(GFP)

pltred_norm <- plot_static_fluorescence(`RFP/OD normalized`, .data = selected_data, interactive_plot = F)
  
reddnorm_log <- pltred_norm %>% format_logscale_x() %>% print()

# save plot (in writing folder)
ggsave('data_S10.pdf', reddnorm_log,
       path = output_path, 
       height = 3, width = 4)

# data export
write_csv(selected_data, file = str_c(output_path, '/S10.csv'))
