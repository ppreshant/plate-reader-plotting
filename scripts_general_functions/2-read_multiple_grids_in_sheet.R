# 2-read_multiple_grids_in_sheet.R


# Extracts sheet from file, data from all plate like grids, combines all data and processing : Vectorizable over multiple sheets

# Reads OD, GFP and RFP, Sample name, Inducer etc. tables from 1 sheet of plate reader output.
# And calculates GFP/RFP, GFP/OD and RFP/OD
# 1. Sample names should be provided in a table next to OD (the first table) in every sheet 
# 2. Inducer values should be provided in a table next to the Sample names in every sheet
# -> See example excel file S010 for reference


read_multiple_grids_in_sheet <- function(sheet_name)
{   
  # Prelims ----
  
  if(str_detect(sheet_name, 'default')) sheet_name <- names(fl)[1] # if default, read the first sheet
  
  # load data form the given sheet
  .df <- fl[[sheet_name]] # extract the sheet of interest (sheet2 is by default the first non-empty sheet unless it was renamed)
  
  # extract the plate reader device name  
  device_name <- .df[1:3, 1] %>% as.character %>% str_match('Device: (.*)') %>% pluck(2)
  
  
  # Device check ----
  measurement_identifier.text <- # Find the label text that defines the data identity in each grid
    
    if(str_detect(device_name, 'infinite')) # infinite M1000 plate reader
    {
      'Label' # InfiniteM1000 uses 'Label:... ' 
      
    } else if(str_detect(device_name, 'Spark')) 
      
    { 'Name' # for Spark plate reader 
    
    } else { # ask user input for the measurement demarcator
      paste('Device not recognised. it is :', device_name, 
            'Your device is not Tecan Spark or Infinite M1000, we need some help from you \n') 
      
      readline('Enter the text that demarcates the type of measurement above the data grid here :,
                                           ex: `Label` from "Label:OD600" ')
    }
      
  # Map the grids ----
  
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
                                             regex(measurement.labels_translation, ignore_case = TRUE) )), # replace labels of fluorophores with standardized : GFP and RFP
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
                        str_replace_all( regex(measurement.labels_translation, ignore_case = TRUE)), # translate to minimal standardized names for fluorophores: like GFP and RFP
                      row_index = user_metadata.row_index, 
                      col_index = .)) # get the column index of each '<>' match
  rm(user_metadata.row_index) # remove temporary variable
  
  
  # Retrieve grid data ----
  
  # join measurement and user_metadata grid identifiers and get grids
  measured_and_metadata.grids <- bind_rows(measurement.grid_info, 
                                           user_metadata.grid_info) %>% 
    # mutate(grp_index = 1:n()) %>% # add a new group index for each grid
    nest(grid_info = -label) %>% # create a nested data frame, indexed by grp_index
    
    # read the data of the grids starting with the row and column index
    mutate(grid_data = map(grid_info,  # identify the non empty grid size and read it
                           ~ find_plate_read_grid(.df, .$row_index, .$col_index)) )
  
  
  
  # Merge grids ----
  
  # convert plate tables into columns and merge all measurements and metadata into 1 table
  merged_all.grids <- map2_dfc(measured_and_metadata.grids$grid_data, 
                               measured_and_metadata.grids$label, 
                               read_plate_to_column) %>%  
    
    # convert to numeric (they are loaded as characters by default)
    mutate(across(any_of(c('OD', 'GFP', 'RFP', 'Inducer')), as.numeric)) 
  
  
  # Baseline subtraction ----
  
  # set baseline for empty cells or empty vector
  empty_cells_baseline <- merged_all.grids %>% 
    filter(str_detect(Samples, baseline_sample_to_subtract)) %>% # select samples that are the baseline cells
    group_by(Samples) %>% 
    summarize(across(where(is.numeric), ~ mean(.x, na.rm = T ))) %>%  # avg of MG1655/other controls fluor values in plate
    
    
    # if there are more than one unique baseline samples detected, ask user to choose one or none
    { if(length(.$Samples) > 1) { 
      print(.) # show the baseline data
      which_baseline_sample <- menu(c(.$Samples, 'none of the samples'), # ask user to choose of them them
                                    title = 'Multiple potential baseline values detected, please indicate which one should be used?')
      
      slice(., which_baseline_sample) # depending on the user selection
      # empty_cells_baseline %<>% .[which_baseline_sample,] # select the chosen baseline
    } else .} %>% 

    
    # if there is no data for baseline, make it zero
    {if(plyr::empty(.)) 
      (add_row(., Samples = 'none') %>%  # add a dummy row
         mutate(across(where(is.numeric), ~0 )) # make the number entries 0 
      ) else .}
  
  # baseline subtraction
  baseline.subtracted_all.grids <- merged_all.grids %>% 
    
    mutate(across(matches('.FP'), # for GFP and RFP data, subtract the baseline fluorescence
                  ~ pmax(. - empty_cells_baseline[[1, cur_column()]], 0) )) %>%  # replace 0 for negative values
# Thoughts: retaining negative values causes hill fitting to fail, and does not have physical meaning
  # but large negative values imply anomalies in the sample and need to be looked at
  # can throw a warning -- how large is large? depends on the gain stuff and LB vs PBS measurements?
        
  # and calculate ratios 
  mutate(across(matches('.FP'),
                ~ ./OD,
                .names = "{.col}/OD"))
  
  # Clean up ----

  # 1. removes NA and undesirable samples
  # 2. Arranges the values in order of plate columns
  # 3. Adds a column for Replicate # useful for collecting samples, before calculating mean
  
  sample_specific_variables <<- user_metadata.grid_info$label # use c('Samples', 'Inducer') to hardcode
  
  processed.data <- baseline.subtracted_all.grids %>% 
    filter(!str_detect(Samples, "NA")) %>%  # remove NA samples (empty wells)
    
    arrange(across(any_of(sample_specific_variables)) ) %>% # re-arrange values
    mutate(Samples = fct_inorder(Samples)) %>% # freeze the order of Samples
    
    group_by(across(any_of(sample_specific_variables))) %>%  
    
    # Add replicate numbering
    mutate('Replicate #' = row_number()) %>%  
    
    # calculate means
    mutate(across(where(is.numeric), # calculate mean of all number columns
                  list( mean = ~ mean(.x, na.rm = T)) )) %>%  
  
    ungroup()
  
  # return the processed data and baseline data
  return(list(processed.data, empty_cells_baseline))
  
}