# Pulls a file output from plate readers () and plots GFP, GFP/OD, RFP/OD etc. in both linear and logscales

# User inputs ----

# User inputs: 1. Enter name of the excel file, 2. Name of the data sheet(S) 3. number of rows and columns in plate reader data 4. Title for plots #comment (file name starts in the previous directory of this Rproject)
flnm <- 'example-S032_Mfdpir_pPK66,sPK14,15_mScarlet_leak_check'
# Note: The script only works for SPARK files where OD, GFP and RFP are read, 
  # if you leave out anything b_gap needs to be changed accordingly
sheet_name <- 'default'

title_name <- flnm # appears on the html file name and header of selected plots, change as required

n_Rows <- 2; 
n_Cols <- 9;
partial_plate = T; # enter FALSE (F) here when the plate is 8 x 12 but non rectangular

baseline_sample_to_subtract <- 'MG1655|DH10B|MFDpir|NEB10b|PBS' # Add baseline cell name(s) here
# Fluorescence from samples matching this name will be subtracted from all values (baseline)

# Prelims ----

source('./general_functions_plate_reading.R') # source the file that contains all the functions


# Input data ----

flpath <- str_c('../plate reader data/',flnm,'.xlsx')
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

# Processing ----
# location for optional processing - custom written code

# processed.data %<>% mutate(Samples = as_factor(Samples), Inducer = as_factor(Inducer)) # freeze order of samples as in the plate - columnwise - for easy plotting
# processed.data$Inducer %<>% str_c(.,' uM') %>% as_factor() # This will make inducer a text, remove this line to retain inducer as numeric

# plotting ----

# calling r markdown file for plotting in a nice format html file
rmarkdown::render('plate_reader_plotting_and_html.Rmd', output_file = str_c('./html files/', title_name, '.html'))


# Save data ----

# save data in the 'processed' folder
write.csv(processed.data, 
          str_c('../plate reader data/processed/', flnm, '-processed.csv', 
                na = ''))