# 7-read_merge_metadata_grids.R

# return a merged well- metadata table read from tables one below another in the .xlsx file
read_merge_metadata_grids <- function(.dataframe)
  
{
  metadata.grids <- 
    
    select(.dataframe, 1) %>% # first grid column, 
    as.vector() %>%  # as vector
    {which(. == '<>')} %>% # find the row_index where '<>' occurs
    
    # map the row col index and names of metadata type
    map_dfr( ~ tibble(label = .dataframe[[.x - 1, 1]],  # get label from the row above
                      # str_replace_all( regex(measurement.labels_translation, ignore_case = TRUE)), # translate to minimal standardized names for fluorophores: like GFP and RFP
                      row_index = .x, 
                      col_index = 1)) %>% # get the column index of each '<>' match
    
    
    nest(grid_info = -label) %>% # create a nested data frame, indexed by grp_index
    
    # read the data of the grids starting with the row and column index
    mutate(grid_data = map(grid_info,  # identify the non empty grid size and read it
                           ~ find_plate_read_grid(.dataframe, .$row_index, .$col_index)) )
  
  
  # convert all metadata plate tables into columns and merge them along with the well index
  merged_grids <- map2_dfc(metadata.grids$grid_data, 
                           metadata.grids$label, 
                           read_plate_to_column,
                           retain_well_id = TRUE) %>%  
    
    # convert to numeric (they are loaded as characters by default)
    mutate(across(any_of(c('OD', 'GFP', 'RFP', 'Inducer')), as.numeric)) 
  
}

