# 6-test_automatic grid reader

# experimental ----

# Identify the data grid using <> and Identify the table of data and type (OD, GFP, RFP etc. by the Label field)

if(str_detect(device_name, 'infinite')) # infinite M1000 plate reader
{
  measurement_identifier.text = 'Label' # Tecan uses 'Label' --> needs an if loop?

} else if(str_detect(device_name, 'SPARK')) 

  {measurement_identifier.text = 'Name' # for SPARK plate reader ;

  } else stop(paste('Device not recognised. it is ', device_name)) # stop and throw error for unrecognized plate reader device


.df <- fl$`Result sheet` # extract the first sheet (everything after this should be vectorized for all sheets, with sheet name auto found)

# map the locations of data grids with '<>' along with their labels (ex: OD, GFP, RFP etc.)
measurement.grid_info <- map_in_sheet(.df, str_c('^', measurement_identifier.text, '|<>'), 1) %>% # find occurrences of labels and '<>'
  
  # clean up into two columns
  separate(identifier, c('identifier','regex_part'), sep = ': ') %>%  # find the occurrence and index of "Label" word in the sheet; retain the text after 'label: '
  mutate(label = coalesce(neighbour, regex_part)) %>%  # if match is NA, takes the value from label (accounts for Spark and Infinite M1000)
  select(-neighbour, -regex_part) %>%  # remove the individual columns since coalesced column contains this data
  filter(!is.na(label)) %>%  # remove the dummy first occurrence of 'Name' in Spark data, where 2nd column is empty

  # making groups for each label, <> pair
  mutate(grp_index = (row_number()/2) %>% ceiling()) %>%   # divide into groups of 2 rows each
  group_by(grp_index) %>% # each group has one label and one dataset, marked by <>
  nest() %>% # get all columns for each group into a smaller data-frame for easy manipulation
  
  # iterating functions on each group,  
  mutate(label = map_chr(data, # bring out the label of the data, with minimal standardized names
                             ~ str_replace_all(.$label[1], 
                                               regex(measurement.labels_translation, ignore_case = TRUE) )), 
         row_index = map_int(data, ~ .$index[2]), # bring out the index of '<>' 
         col_index = 1) %>%  # all measurement grid '<>' are in the first column
  
  ungroup() %>%  # group index will be removed and re-created after joining with metadata info
  select(-data, -grp_index) # drop the list column 'data' and the group index


# get user written metadata to the right of OD data (or the first set of data)

# user written metadata (sample names, inducer conc etc.) is next to the first grid (usually OD)         
user_metadata.row_index <- measurement.grid_info$row_index[1]  

user_metadata.grid_info <- slice(.df, user_metadata.row_index) %>% # first grid row, 
  as.vector() %>%  # as vector
  {which(. == '<>')[-1]} %>% # find the col_index where '<>' occurs; remove first occurence (OD)
  map_dfr( ~ tibble(label = .df[[user_metadata.row_index-1, .]] %>% # get label from the row above
                      str_replace_all( regex(measurement.labels_translation, ignore_case = TRUE)), # translate to minimal standardized names
                    row_index = user_metadata.row_index, 
                    col_index = .)) # get the column index of each '<>' match
rm(user_metadata.row_index) # remove temporary variable

# join measurement and user_metadata grid identifiers and get grids
measured_and_metadata.grids <- bind_rows(measurement.grid_info, 
                                         user_metadata.grid_info) %>% 
  # mutate(grp_index = 1:n()) %>% # add a new group index for each grid
  nest(grid_info = -label) %>% # create a nested data frame, indexed by grp_index
  
  # testing in progress
  mutate(grid_data = map(grid_info,  # identify the non empty grid size and read it
                         ~ find_plate_read_grid(.df, .$row_index, .$col_index)) )
  

  # iterate over each group, and retrieve the data of the grid corresponding to <> 


# Now we need to identify automatically, how many rows and columns of data appear below the <> // or count the number of empty cells somehow
# plan : make each <> and the element before it into a group, walk across groups
# in each group, go to the index of <>, look for ^A-H$ and ^1-12$ in that row and 1st column and capture the grid between these
 # or look for is.na() in an 9x13 grid starting from the <> index -> pick the index of the closest na's in rows and columns where the grid ends
 # or map all the empty cells and search in this grid below ->
# empty_cells <- .df %>% is.na(.) %>% which(arr.ind = T) %>% as_tibble() # maps empty cells with row and column index


# Suggestion
# Check the way this is done in the FlopR script - for timeseries == 'FALSE'
# https://github.com/ucl-cssb/flopr/blob/master/R/tecanSparkFunctions.R
# uses 'Start Time' and 'End time' keys to identify the data grid
# This only works for the Spark which puts the full grid irrespective of how many rows/columns are read
 # need to adapt this for the weird Infinite M1000 pro output..


read_all_plates_in_sheet2 <- function(device_name, .df, n_Rows, n_Cols, partial_plate, sheet_name)
{
  # Reads OD, GFP and RFP tables from 1 sheet of plate reader output.
  # And calculates GFP/RFP, GFP/OD and RFP/OD
  # 1. Sample names should be provided in a table next to OD (the first table) in every sheet 
  # 2. Inducer values should be provided in a table next to the Sample names in every sheet
  # -> See example excel file S010 for reference
  
  # Context: b_gap = row number of OD data header (<>) , a_gap = 2 + # of rows between OD and fluorescence value header(<>), The same between GFP and RFP as well; 
  # Example - Infinite M1000 with full plate has: 23 rows of lines before data is seen, 26 rows between data and fluorescence values, 26 more rows till RFP values (including the row with <>); If partial plate is read there is 1 extra line in all 3 places
  
  # If partial plate is read there is 1 extra line in all 3 places for infinite; makes no difference for Spark
  
  if(str_detect(device_name, 'infinite')) # infinite M1000 plate reader
    
  { 
    measurement_identifier.text = 'Label' # this is how the label appears Label:OD600 (in the same cell)
    # if(n_Rows == 8 & n_Cols == 12 & partial_plate == F) {b_gap = 23; a_gap <- 26; # full plate
    # }  else {b_gap = 24; a_gap <- 27} # partial plate (has extra lines for plate region) - also includes non rectangular selection
    # 
  } else if(str_detect(device_name, 'Spark')) # Spark plate reader
    
  { measurement_identifier.text = 'Name' # this is how the label appears Name OD600 (in the adjacent cell)
  #   b_gap = 35; a_gap <- 27;
  # n_Rows = 8; n_Cols = 12
  }  # override rows and columns numbering since empty cells are also printed  
  
  else stop(paste('Device not recognised. it is ', device_name)) # stop and throw error for unrecognized plate reader device
  
  inducer_flag <- 0
  
  table_OD <- .df[b_gap + 0:n_Rows, 1 + 0:n_Cols] # exract the OD table
  table_Samples <- .df[b_gap + 0:n_Rows, 3 + n_Cols + 0:n_Cols] # exract the Sample names table
  if(!str_detect(table_Samples[1,1], '<>')) {stop(str_c('Sample names in the wrong place or are improperly formatted, for sheet: ', sheet_name))}
  
  if (ncol(.df) >= 5+3*n_Cols) # extracting the inducer table 
  {
    table_Inducer <- .df[b_gap + 0:n_Rows, 5+2*n_Cols + 0:n_Cols] # exract the Inducer table if it exists
    if (is_empty(table_Inducer)) inducer_flag <- 1 # flag that there are no inducer values
  }  else inducer_flag <- 1 # flag that there are no inducer values
  
  if(inducer_flag) {table_Inducer <- table_Samples; table_Inducer[-1,-1] <- '0'; warning(str_c('No inducer values provided, assumed to be zero. in sheet: ', sheet_name))} # if inducer table is empty or absent, make it zero (same size as samples) and throw a warning
  
  table_GFP <- .df[(b_gap + a_gap - 1 +n_Rows) + 0:n_Rows, 1 + 0:n_Cols] # exract the GFP values
  table_RFP <- .df[(b_gap + 2*a_gap - 2 +2*n_Rows) + 0:n_Rows,  1 + 0:n_Cols] # exract the RFP values
  
  tables_list <- list(table_Samples,table_OD,table_GFP,table_RFP,table_Inducer)
  names_vector <- c('Samples','OD','GFP','RFP','Inducer')
  
  merged1 <- map2_dfc(tables_list, names_vector, read_plate_to_column) # convert plate tables into columns and merge all four data types into 1 table
  merged1 %<>% mutate_at(c('OD','GFP','RFP'),as.numeric) # convert to numeric (they are loaded as characters by default)
  empty_cells_baseline <- merged1 %>% 
    filter(str_detect(Samples, 'MG1655|DH10B|NEB10b')) %>% 
    group_by(Samples) %>% 
    summarize(across(where(is.numeric), ~ mean(.x, na.rm = T ))) # avg of MG1655/other controls fluor values in plate
  
  merged2 <- merged1 %>% mutate(GFP_bs = pmax(GFP - empty_cells_baseline$GFP,0), RFP_bs = pmax(RFP - empty_cells_baseline$RFP,0)) %>% mutate('GFP/RFP_bs' = GFP_bs/RFP_bs) %>% mutate('GFP/OD_bs' = GFP_bs/OD) %>% mutate('RFP/OD_bs' = RFP_bs/OD) # Subtract baseline fluor and calculate ratios
}