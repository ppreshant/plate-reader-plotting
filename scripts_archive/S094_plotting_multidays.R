# S094_plotting_multidays.R

# Prelims ----

source('0.5-user_inputs.R') # get user inputs
source('general_functions_plate_reading.R') # source the file that contains all the functions


# Load data ----

# load the processed data from file
# flnm <- 'S094_testing pPK070'

processed.data <- read_processed_data(flnm)


# Processing ----

# arrange samples in descending order, to aid the eye in seeing patterns
fluor_order_data <- processed.data %>% 
  arrange(`RFP/OD`) %>% 
  
  ungroup() %>% 
  mutate(across(Samples, fct_inorder)) %>% 
  
  arrange(sheet_ID)


# Plotting ----

# imperfect plot
# plt_red <- 
#   plot_static_fluorescence(.fluor_variable = `RFP/OD`)


# plot RFP/OD, showing values as points, and arrow from d1 to d2 mean values
ggplot(fluor_order_data,
       aes(y = Samples, x = `RFP/OD`, colour = sheet_ID)) +
  
  geom_jitter(height = 0.3, width = 0) + 
  
  # show mean
  geom_point(aes(x = `RFP/OD_mean`), shape = '|', size = 5) +
  ggarrow::geom_arrow(aes(x = `RFP/OD_mean`, group = Samples), colour = 'gray') + 
  # arrow from https://teunbrand.github.io/ggarrow/
  
  # add a text label for quick reference of the mean
  geom_text(data = ~ filter(.x, sheet_ID == 'd2'),
            mapping = aes(x = `RFP/OD_mean`, label = `RFP/OD_mean` %>% round, 
                          vjust = if_else(`RFP/OD_mean` > max(`RFP/OD_mean`)/2, 1, 0.5),
                          hjust = if_else(`RFP/OD_mean` > max(`RFP/OD_mean`)/2, 1.5, -0.3)),
            show.legend = FALSE,
  ) + 
  
  # labels and formatting
  ggtitle(flnm) + 
  theme(legend.position = 'top')


ggsave(plot_as(flnm), width = 5, height = 4)
