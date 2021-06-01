# 6-test_automatic grid reader

# experimental 

# Identify the data grid using <> and Identify the table of data and type (OD, GFP, RFP etc. by the Label field)
label.text = 'Name' # only for SPARK plate reader


data_sheet1 <- fl$`Result sheet` # extract the first sheet (everything after this should be vectorized for all sheets, with sheet name auto found)

label_list <- map_in_sheet(data_sheet1, str_c('^', label.text, '|<>'), 1) %>% 
  separate(identifier, c('identifier','label'), sep = ': ') # find the occurance and index of "Label" word in the sheet; retain the text after 'label: '

# data_starting <- map_in_sheet(data_sheet1,'<>', 1) # maps the index of where plate read values start by '<>'


# Now we need to identify automatically, how many rows and columns of data appear below the <> // or count the number of empty cells somehow
# empty_cells <- data_sheet1 %>% is.na(.) %>% which(arr.ind = T) %>% as.tibble() # maps empty cells with row and column index




read_all_plates_in_sheet2 <- function(device_name, data_sheet1, n_Rows, n_Cols, partial_plate, sheet_name)
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
    label.text = 'Label' # this is how the label appears Label:OD600 (in the same cell)
    # if(n_Rows == 8 & n_Cols == 12 & partial_plate == F) {b_gap = 23; a_gap <- 26; # full plate
    # }  else {b_gap = 24; a_gap <- 27} # partial plate (has extra lines for plate region) - also includes non rectangular selection
    # 
  } else if(str_detect(device_name, 'Spark')) # Spark plate reader
    
  { label.text = 'Name' # this is how the label appears Name OD600 (in the adjacent cell)
  #   b_gap = 35; a_gap <- 27;
  # n_Rows = 8; n_Cols = 12
  }  # override rows and columns numbering since empty cells are also printed  
  
  else stop(paste('Device not recognised. it is ', device_name)) # stop and throw error for unrecognized plate reader device
  
  inducer_flag <- 0
  
  table_OD <- data_sheet1[b_gap + 0:n_Rows, 1 + 0:n_Cols] # exract the OD table
  table_Samples <- data_sheet1[b_gap + 0:n_Rows, 3 + n_Cols + 0:n_Cols] # exract the Sample names table
  if(!str_detect(table_Samples[1,1], '<>')) {stop(str_c('Sample names in the wrong place or are improperly formatted, for sheet: ', sheet_name))}
  
  if (ncol(data_sheet1) >= 5+3*n_Cols) # extracting the inducer table 
  {
    table_Inducer <- data_sheet1[b_gap + 0:n_Rows, 5+2*n_Cols + 0:n_Cols] # exract the Inducer table if it exists
    if (is_empty(table_Inducer)) inducer_flag <- 1 # flag that there are no inducer values
  }  else inducer_flag <- 1 # flag that there are no inducer values
  
  if(inducer_flag) {table_Inducer <- table_Samples; table_Inducer[-1,-1] <- '0'; warning(str_c('No inducer values provided, assumed to be zero. in sheet: ', sheet_name))} # if inducer table is empty or absent, make it zero (same size as samples) and throw a warning
  
  table_GFP <- data_sheet1[(b_gap + a_gap - 1 +n_Rows) + 0:n_Rows, 1 + 0:n_Cols] # exract the GFP values
  table_RFP <- data_sheet1[(b_gap + 2*a_gap - 2 +2*n_Rows) + 0:n_Rows,  1 + 0:n_Cols] # exract the RFP values
  
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