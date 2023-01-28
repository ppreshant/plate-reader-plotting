# Pulls a file output from plate readers () and plots GFP, GFP/OD, RFP/OD etc. in both linear and logscales
# Note: The script only works for SPARK files where OD, metadata next to it, and optionally any fluorescence below it are read
# Look at example file inside the 'plate reader data/' folder for inspiration

# Prelims ----

source('0.5-user_inputs.R') # get user inputs
source('general_functions_plate_reading.R') # source the file that contains all the functions


# Input data ----

flpath <- str_c('plate reader data/',flnm,'.xlsx')
fl <- read_plateReader_file(flpath) # load all non empty sheets of the excel file into fl - as a list 

# Get all measurements and metadata (sample names, ..) into a data frame
# 1. Map locations of all relevant grids with data '<>', along with labels
# 2. Retrieve and merge the grids to associate metadata for respective samples
# 3. Calculate ratios of fluorescence to OD, calculate mean
# 4. Create a number for replicates (same name, and metadata)
# 5. also returns the baseline data - to show in the markdown

processed_and_baseline_list <- read_multiple_grids_in_sheet(sheet_name)

# unpack the processed data and baseline
processed.data <- processed_and_baseline_list[[1]]
empty_cells_baseline <- processed_and_baseline_list[[2]]

rm(processed_and_baseline_list) # remove list after unpacking

# Processing ----
# location for optional processing - custom written code

# collect all measurements in 1 column : Long format data
long_fluor_processed <- processed.data %>% 
  select(!matches('Replicate|units')) %>%  # choose OD, x/OD and _bs and _mean additions
  mutate(index = row_number(), .after = 2) %>% # add a dummy index -- to keep replicates apart
  
  rename_with(.cols = !matches('_mean|Samples|Inducer|index'), .fn = ~ str_c(.x, '_value') ) %>%  # suffix 'value' for non mean columns
  
  pivot_longer(cols = -all_of(c(sample_specific_variables, 'index')), # pull measurement types and summary type (mean vs 'raw' value) into two cols
               names_pattern = '(.*)_(....*)', names_to = c('Measurement', 'type_of_summary')) %>% 
  
  pivot_wider(names_from = type_of_summary, values_from = value) # bring values and means into two separate cols

# Long formatted for only normalized data
long_fluor.normalized_processed <- 
  long_fluor_processed %>% 
  filter(str_detect(Measurement,'/OD')) # choose OD, x/OD and _bs and _mean additions

# retain only the mean values -- for plotting the line and bars
long_unique.normalized_processed <- select(long_fluor.normalized_processed, -c(value, index)) %>% unique()


# processed.data %<>% mutate(Samples = as_factor(Samples), Inducer = as_factor(Inducer)) # freeze order of samples as in the plate - columnwise - for easy plotting
# processed.data$Inducer %<>% str_c(.,' uM') %>% as_factor() # This will make inducer a text, remove this line to retain inducer as numeric


# Check if inducer is present and dose response needs to be plotted (> 3 inducer values?)
inducer.present <- 'Inducer' %in% colnames(processed.data) &&
  (processed.data$Inducer %>% unique() %>% length()) > 3

# plotting ----

# calling r markdown file for plotting in a nice format html file
rmarkdown::render('static_plate_reader_plotting_and_html.Rmd', output_file = str_c('plate reader analysis/html files/', title_name, '.html'))


# Save data ----

# save data in the 'processed' folder
write.csv(processed.data, 
          str_c('plate reader data/processed/', flnm, '-processed.csv', 
                na = ''))