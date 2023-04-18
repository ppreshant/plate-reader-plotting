# S070_Ara_dose_response.R

# Other Ara dose response datasets : S066, S069, S070, S070x

# Prelims ----

source('0.5-user_inputs.R') # get user inputs
source('general_functions_plate_reading.R') # source the file that contains all the functions

# flnm <- 'S070_S071_d0_13-April-23' # works for this file

# Load data ----

processed.data <- read_csv(str_c('plate reader data/processed/', flnm, '-processed.csv'))


# processing ----

# plan select subset, cleanup names, call plotting function
relevant.data <- filter(processed.data, str_detect(Samples, 'd0')) %>% # select S070 samples (with d0 key) // not general
  # mutate(across(Samples, ~ str_replace(.x, '_d0'))) %>% # remove d0
  separate(Samples, into = c('Samples', 'day'), sep = '_') %>% # push the day into another column
  
  # processing for dose_response type of data
  mutate(sample_type = if_else(str_detect(Samples, 'glu|OFF|ON'), 'Controls', 'Induction'), # mark controls
         Arabinose = as.numeric(Samples), .after = Samples) # convert to numeric for plotting properly


# plotting ----

source('scripts_general_functions/11-plot_dose_response_and_controls.R') # source the script
plt_dosec <- plot_dose_response_and_controls() %>% print

ggsave(plot_as(title_name), width = 6, height = 4)
# ggplotly(plt_dosec) # interactive : controls panel only :(

# calculations ----

# take mean gfp/od into smaller set
summary.data <- 
  reframe(relevant.data, .by = Samples,
        signal_mean = unique(`GFP/OD_mean`))

# dynamic range : max Ara to glu
summary.data$signal_mean %>% {.[7] / .[10]}

# dynamic range max : ON to OFF (max and min)
summary.data$signal_mean %>% {max(.) / min(.)}

# TODO : add hill fitting to the plotting function..