# Pulls a file output from plate readers () and plots GFP, GFP/OD, RFP/OD etc. in both linear and logscales

# User inputs ----

# User inputs: 1. Enter name of the excel file, 2. Name of the data sheet(S) 3. number of rows and columns in plate reader data 4. Title for plots #comment (file name starts in the previous directory of this Rproject)
flnm <- 'S033_ww-isolts_AHL-gfp_23-7-21'
# Note: The script only works for SPARK files where OD, GFP and RFP are read, 
  # if you leave out anything b_gap needs to be changed accordingly
sheet_name <- 'default'

title_name <- flnm # appears on the html file name and header of selected plots, change as required

n_Rows <- 2; 
n_Cols <- 9;
partial_plate = T; # enter FALSE (F) here when the plate is 8 x 12 but non rectangular

baseline_sample_to_subtract <- 'MG1655|DH10B|NEB10b|PBS' # Add baseline cell name(s) here
# Fluorescence from samples matching this name will be subtracted from all values (baseline)

# Prelims ----

source('./general_functions_plate_reading.R') # source the file that contains all the functions


# Input data ----

flpath <- str_c('../plate reader data/',flnm,'.xlsx')
fl <- read_plateReader_file(flpath) # load all non empty sheets of the excel file into fl - as a list 

all.grids.merged_raw.data <- extract_from_given_sheet(sheet_name, n_Rows, n_Cols, partial_plate)


# Processing ----

# Purpose : Data crunching of plate reader after loading data set
processed.data <- all.grids.merged_raw.data %>% 
  filter(!str_detect(Samples, "NA|MG")) %>%   # remove NA samples (empty wells)
  group_by(Samples, Inducer) %>%  # mean will be calculated in these groups 
  mutate(across(where(is.numeric), list( mean = ~ mean(.x, na.rm = T)) )) # calculate mean of all numbers

# processed.data %<>% mutate(Samples = as_factor(Samples), Inducer = as_factor(Inducer)) # freeze order of samples as in the plate - columnwise - for easy plotting
processed.data$Inducer %<>% str_c(.,' uM') %>% as_factor() # This will make inducer a text, remove this line to retain inducer as numeric



# merged3 <- processed.data %>% group_by(Samples, Inducer) %>%  summarize_at('GFP/RFP', funs(mean, sd)) # calculate mean and SD of the GFP/RFP for each Sample and inducer value
# # merged3 <- processed.data %>% gather(Reading, Value, OD, GFP, RFP) # gather all the reading into 1 column - to plot multiple
# merged4 <- merged3 %>% arrange(mean) %>% ungroup() %>% separate(Samples, c('Samples', NA), sep ='\\+') %>% mutate(Samples = fct_inorder(Samples)) # freeze samples in ascending order of uninduced  # remove the common reporter plasmid name after the + sign

# plotting ----

# calling r markdown file for plotting in a nice format html file
rmarkdown::render('plate reader plotting.Rmd', output_file = str_c('./html files/', title_name, '.html'))
