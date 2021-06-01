# Pulls a file output from plate readers and reads GFP, GFP/OD, RFP/OD etc. in all designated sheets
# The data is then processed and plotted (by days, inducer concentrations, and other data categories)

# User inputs ----

# User inputs: 1. Enter name of the excel file, 2. Name of the data sheet(S) to exclude 3. number of rows and columns in plate reader data 4. Title for plots 
# Comment (make sure excel file is in the parent directory of the Rproject file or code)

flnm <- 'S022_optimal 25-11-19'
sheets_to_exclude <- 'auto' # regular expression for sheets to be excluded

title_name <- flnm

n_Rows <- 8; # default full 96 well plate = 8 rows x 12 columns
n_Cols <- 12; 
# check the number of rows and columns for first sheet listed below

plotting_order <- c("pRV01", "pInt8", "pPK7/8", "pPK7/17","Controls") # order of plotting for samples
# can generalize using regex??


# Prelims ----

source('./general_functions_plate_reading.R') # source the file that contains all the functions


## Import data ----

flpath <- str_c('../plate reader data/',flnm,'.xlsx') # path for the plate reader file 
fl <- read_plateReader_file(flpath) # load all non empty sheets of the excel file into fl - as a list 

sheet_names <- names(fl) %>% .[!str_detect(., sheets_to_exclude)] # get the list of sheets, other than excluded ones 
# sheet_names <- c("0 h","6 h","24 h", "48 h", "72 h", "96 h") # list the sheets to be loaded manually

sheet_times <- sheet_names %>% 
  str_split(' ') %>% # sheet names are '0 h' '6 h' etc, they will get split and hte alternate elements will be h
  unlist() %>% 
  .[rep(c(T,F))] %>%  # exclude the alternate elements whihc are just 'h'
  as.numeric() # extracting the hour time from sheet name string
# sheet_times <- c(0,6,24,48,72,96) # manual entry of sheet times


# Processing ----

all.sheets_data.merged <- map2_dfr(sheet_names, sheet_times, 
                                   ~ extract_from_given_sheet(.x, n_Rows, n_Cols) %>% 
                                     mutate(Time = .y)) # extracing OD, GFP and RFP from each sheet (vectorized) by preset number of dummy rows between the grids, and merge into one table by joining rows <dfr>

all_days_table <- all.sheets_data.merged %>% 
  separate(Samples, into = c('Samples', 'Reporter'), sep = '\\_| \\+ ') %>% 
  mutate(Reporter = if_else(is.na(Reporter)|str_detect(Reporter,"rM"), Samples, Reporter), 
         Samples = if_else(Samples == Reporter, "Controls", Samples), 
         Samples = if_else(str_detect(Reporter,'^[:digit:]*$'), str_c(Samples,Reporter, sep = '_'), Samples) ,
         Reporter  = str_replace(Reporter, 'rG$|^[:digit:]*$', 'rGFP')) # Samples has the recombinase plasmid or controls and reporter has the reporter plasmid
# details: split into 2 columns by _ and +; make singleton entries as controls and put name in Reporter; make mutants ex: pPK7_17 into 1 sample name and reporter as the default rGFP (also replace shorthand notation rG with rGFP) 

all_days_table %<>% filter(!str_detect(Reporter, 'LB'))
all_days_table %<>% mutate(Reporter = fct_inorder(Reporter), Reporter = fct_relevel(Reporter, 'rGFP', "fGFP")) # arranging samples again to make up for factor mismatch
all_days_table$Samples %<>% fct_relevel(plotting_order) # manually reorder sample order for plotting
all_days_table$Inducer %<>% as.numeric() %>% round(3) %>% as.character() 

all_days_gathered <- all_days_table %>% gather(key = 'Measurement', value = 'Reading', OD, GFP, RFP, 'GFP/RFP', 'GFP/OD', 'RFP/OD') # gather different measurements and ratios into 1 column


# plotting ----

# calling r markdown file for plotting in a nice format html file
rmarkdown::render('plate reader plotting.Rmd', output_file = str_c('./html files/', title_name, '.html'))