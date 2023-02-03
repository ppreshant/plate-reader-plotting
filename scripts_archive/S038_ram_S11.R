# S038_ram_S11.R

# run_single_plate.R until line 26
# Data : S038_47+67_23-2-22

output_path <- '../../Writing/RAM paper outputs/Archive'

# select subset of data
selected_data <- filter(processed.data, str_detect(Samples, '47')) %>% 
  droplevels() # clean up unused levels in the facor

# plot 
plot_static_fluorescence(`RFP/OD`, .data = selected_data, interactive_plot = F)
# plot_raw_static_fluorescence(GFP)

# save plot (in writing folder)
ggsave('data_S11.pdf', 
       path = output_path, 
       height = 2, width = 4)

# data export
write_csv(selected_data, file = str_c(output_path, '/S11.csv'))
