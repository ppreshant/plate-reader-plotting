# Pulls a file output from plate readers and reads GFP, GFP/OD, RFP/OD etc. in all designated sheets
# The data is then processed and plotted (by days, inducer concentrations, and other data categories)

# User inputs ----

# User inputs: 1. Enter name of the excel file, 2. Name of the data sheet(S) to exclude 3. number of rows and columns in plate reader data 4. Title for plots 
# Comment (make sure excel file is in the parent directory of the Rproject file or code)

flnm <- 'S012b_Ara vs AHL_1-6-19'
sheets_to_exclude <- 'none' # regular expression for sheets to be excluded

title_name <- flnm

n_Rows <- 8; # default full 96 well plate = 8 rows x 12 columns
n_Cols <- 12; 
# check the number of rows and columns for first sheet listed below

partial_plate = F; # enter FALSE (F) here when the plate is 8 x 12 but non rectangular

baseline_sample_to_subtract <- 'MG1655|DH10B|NEB10b' # Add baseline cell name(s) here
# Fluorescence from samples matching this name will be subtracted from all values (baseline)

grouping_vars_for_mean = c('Samples', 'Reporter', 'Inducer', 'Time') # all replicates within these groups will be averaged

plotting_order <- c("pRV01", "pInt8", "Controls") # order of plotting for samples
# can generalize using regex??


# Prelims ----

source('./general_functions_plate_reading.R') # source the file that contains all the functions


# Labelling translators ----

# Subtitle labeller (for y axis variables)
yaxis_translation <- c('GFP/OD' = 'GFP/OD (a.u.)',
                       'GFP/OD_bs' = 'GFP/OD (a.u.)')

# # Label x axis (assay_variable) in easy to interpret form 
# lst_assay.vars_translation <- list('gfp' = '89',
#                                    'Ribo' = c('295', '297', '298', '299', '300', '186'),
#                                    'empty' = '103') # new_name -> c('assay_variables' ..)
# 
# tbl_assay.vars_translation <- lst_assay.vars_translation %>% # convert the list into tibble
#   map2_dfr(., names(.), ~ tibble('assay_variable' = .x, 'assay_var.identifier' = .y))
# # Add another column in this tibble for specific conversion of 295, etc. into descriptive names?
# # make a lookup named vector; use str_replace() to make this new column
# 
# plot_assay_variable <- 'Template name' # printed on the x axis of the graph



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
                                   ~ extract_from_given_sheet(.x, n_Rows, n_Cols, partial_plate) %>% 
                                     mutate(Time = .y)) # extracing OD, GFP and RFP from each sheet (vectorized) by preset number of dummy rows between the grids, and merge into one table by joining rows <dfr>

processed_all.sheets <- all.sheets_data.merged %>% 
  separate(Samples, into = c('Samples', 'Reporter'), sep = '\\_| \\+ ') %>%  # parsing the names for multiple plasmids separated by + or _

  # details: split into 2 columns by _ and +; 
  # make singleton entries as controls and put name in Reporter; 
  # make mutants ex: pPK7_17 into 1 sample name and reporter as the default rGFP (also replace shorthand notation rG with rGFP) 
    mutate(Reporter = if_else(is.na(Reporter)|str_detect(Reporter,"rM"), Samples, Reporter), 
         Samples = if_else(Samples == Reporter, "Controls", Samples), 
         Samples = if_else(str_detect(Reporter,'^[:digit:]*$'), str_c(Samples,Reporter, sep = '_'), Samples) ,
         Reporter  = str_replace(Reporter, 'rG$|^[:digit:]*$', 'rGFP')) %>%  # Samples has the recombinase plasmid or controls and reporter has the reporter plasmid
  
  filter(!str_detect(Reporter, 'LB')) %>%  # remove unnecessary elements from results 
  mutate(Reporter = fct_inorder(Reporter),  # arranging samples again to make up for factor mismatch
         Reporter = fct_relevel(Reporter, 'rGFP', "fGFP"),
         across(Samples, ~ fct_relevel(., plotting_order)),  # manually reorder sample order for plotting 
         across(Inducer, ~ as.numeric(.) %>% round(3)) ) # round off the inducer value
                  # as.character()) )  # why convert to character??
  

processed.data_w.mean <- processed_all.sheets %>% 
  group_by(across(any_of(grouping_vars_for_mean))) %>%  # mean will be calculated in these groups 
  mutate(across(where(is.numeric), list( mean = ~ mean(.x, na.rm = T)) )) # calculate mean of all numeric columns, into new columns

# all_days_gathered <- processed_all.sheets %>% 
#   gather(key = 'Measurement', value = 'Reading', OD, GFP, RFP, 'GFP/RFP', 'GFP/OD', 'RFP/OD') # gather different measurements and ratios into 1 column


# plotting ----

# calling r markdown file for plotting in a nice format html file
rmarkdown::render('plate reader plotting.Rmd', output_file = str_c('./html files/', title_name, '.html'))