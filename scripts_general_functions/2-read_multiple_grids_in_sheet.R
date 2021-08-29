# 2-read_multiple_grids_in_sheet.R

# Reading all plate related data from a sheet; vectorizable ----
# uses above functions and makes the working RMD file clean

read_all_plates_in_sheet <- function(device_name, data_sheet1, n_Rows, n_Cols, partial_plate, sheet_name)
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
    if(n_Rows == 8 & n_Cols == 12 & partial_plate == F) {b_gap = 23; a_gap <- 26; # full plate
    }  else {b_gap = 24; a_gap <- 27} # partial plate (has extra lines for plate region) - also includes non rectangular selection
    
  } else if(str_detect(device_name, 'Spark')) # Spark plate reader
    
  {
    # locate the first data set and store the row number of the <> in b_gap
    b_gap <- (data_sheet1$...1 %>% str_which('Start Time') %>% min()) + 3
    a_gap <- 27;
    n_Rows = 8; n_Cols = 12 # override rows and columns numbering since empty cells are also printed
  }    
  
  else stop(paste('Device not recognised. it is ', device_name)) # stop and throw error for unrecognized plate reader device
  
  inducer_flag <- 0
  
  table_OD <- data_sheet1[b_gap + 0:n_Rows, 1 + 0:n_Cols] # exract the OD table
  table_Samples <- data_sheet1[b_gap + 0:n_Rows, 3 + n_Cols + 0:n_Cols] # exract the Sample names table
  if(!str_detect(table_Samples[1,1], '<>')) {stop(str_c('Sample names in the wrong place or improperly formatted; check if n_Rows and n_Cols is accurate. \n for sheet: ', sheet_name))}
  
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
  
  merged_all.grids.in.sheet <- map2_dfc(tables_list, names_vector, read_plate_to_column) %>%  # convert plate tables into columns and merge all four data types into 1 table
    mutate(across(any_of(c('OD','GFP','RFP')), as.numeric)) # convert to numeric (they are loaded as characters by default)
  
  # set baseline for empty cells or empty vector
  empty_cells_baseline <- merged_all.grids.in.sheet %>% 
    filter(str_detect(Samples, baseline_sample_to_subtract)) %>% # select samples that are the baseline cells
    group_by(Samples) %>% 
    summarize(across(where(is.numeric), ~ mean(.x, na.rm = T ))) %>%  # avg of MG1655/other controls fluor values in plate
  
  # if there is no data for baseline, make it zero
    {if(plyr::empty(.)) 
      (add_row(., Samples = 'none') %>%  # add a dummy row
         mutate(across(where(is.numeric), ~0 )) # make the number entries 0 
  ) else .}
  
  # baseline subtraction
  # Bug: this is not generalized to work when GFP or RFP is missing .. do we make them 0??
  baseline.subtracted_all.grids <- merged_all.grids.in.sheet %>% 
    mutate(GFP_bs = pmax(GFP - empty_cells_baseline$GFP,0), 
           RFP_bs = pmax(RFP - empty_cells_baseline$RFP,0), # Subtract baseline fluor
           
           # and calculate ratios 
           'GFP/RFP_bs' = GFP_bs/RFP_bs,
           'GFP/OD_bs' = GFP_bs/OD,
           'RFP/OD_bs' = RFP_bs/OD)
}



extract_from_given_sheet <- function(sheet_name, n_Rows, n_Cols, partial_plate)
{ # extracts sheet from file, data from sheet and gives clean output - mean and var of GFP/RFP : Vectorizable over multiple sheets
  if(str_detect(sheet_name, 'default')) sheet_name <- names(fl)[1] # if default, read the first sheet
  
  data_sheet1 <- fl[[sheet_name]] # extract the sheet of interest (sheet2 is by default the first non-empty sheet unless it was renamed)
  
  device_name <- data_sheet1[1:3, 1] %>% str_match('Device: (.*)') %>% pluck(2) # exract the plate reader device name  
  
  merged_processed_all.grids <- read_all_plates_in_sheet(device_name, data_sheet1, n_Rows, n_Cols, partial_plate, sheet_name)
  cleaned.data_all.grids <- clean_and_arrange(merged_processed_all.grids) # removes empty wells or unlabelled cells; arranges by sample alphabetical order and inducer, makes replicates
  
}