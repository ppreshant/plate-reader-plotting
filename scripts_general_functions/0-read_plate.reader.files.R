# 0-read_plate.reader.files_manipulate.columns.R
# reading files and manipulating columns ----

#' read all sheets in the excel file into a list

read_plateReader_file <- function(flnm)
{ # reads excel file output by plate reader; outputs a list of non-empty sheets
  fl <- flnm %>%  
    readxl::excel_sheets() %>% # get the names of all the sub-sheets
    set_names(.,.) %>% 
    map(readxl::read_excel, path = flnm, col_names = F, col_types = 'text') %>% # read each sub-sheet
    rlist::list.clean(fun = is_empty) # removes data from empty sheets
}



# Browse the plate reader files ----

map_in_sheet <- function(data_list, match.key, 
                         match.col.number, neighbour.col.number = match.col.number + 1)
{ # finds the occurrence of "match.key" in the match.col.number (generally 1st column) of data list and ... 
  # ... gives the row of occurrence along with the index
  
  data_list %<>% mutate(index = 1:n())  # add a column for row index
  
  matched_rows_and_neighbour <- data_list %>%
    pull(match.col.number) %>% # select the column where matching is desired
    str_detect(match.key) %>%    # temporary variable to locate where 'Label' matches
    which() %>% # find the index of the matching row
    data_list[.,] %>%  # select this row and all columns from the data
    
    # return the labelled columns of the matching column and it's next column
    select(identifier = all_of(match.col.number), neighbour = all_of(neighbour.col.number), index) 
    
}


# finds non empty cells below and to the right of the starting cell with '<>' 
find_plate_read_grid <- function(.df, start_row_index, start_col_index = 1)
{ 
  # add an error check if first cell is not <>
  
  # Check if the excel file holds a full 9 x 13 grid (8 x 12 + row and col labels) at this location
  # relevant for Inifinite M1000 grids that are smaller than 8 x 12
  grid_end_indices <- pmin( c(start_row_index + 8, start_col_index + 12), # store the minimum of a ..
                           dim(.df))   # .. full grid or the excel dimensions
  
  # find indices relative to grid : so 8 x 12 grid will become 10 x 14 (including the headers and 1 border row/col)
  relative_grid_border_indices <- grid_end_indices - c(start_row_index, start_col_index) + c(2,2)
  
  
  # Read 9 columns x 13 rows including labels = theoretical maximum 96 well plate 
  full_grid <- .df[start_row_index : grid_end_indices[1],  # extract a full grid or smaller until end of sheet
                   start_col_index : grid_end_indices[2] ]
  
  
  # find the empty cells around the edges of the grid using the headers for rows and cols
  col_edge <- full_grid[1,] %>% # select first row (<>, 1, 2, 3..)
    {which(is.na(.))} %>%  # find the empty cells (NA) in the grid header
    min(., relative_grid_border_indices[2]) - 2 # find the minimum NA occurrence or the cell beyond the full grid
  
  row_edge <- full_grid[,1] %>% # select first col (<>, A, B, C..)
    {which(is.na(.))} %>%  # find the empty cells (NA) in the row headers
    min(., relative_grid_border_indices[1]) - 2 # find the minimum NA occurrence or the cell beyond the full grid
   # should the 1 and 2 be interchanged? 
  
  # read the actual grid (< = full grid)
  if(col_edge == 10 & row_edge == 14) {data_grid <- full_grid # if it's a full grid
  }  else data_grid <- .df[start_row_index + 0:(row_edge), start_col_index + 0:(col_edge)]
  
}



