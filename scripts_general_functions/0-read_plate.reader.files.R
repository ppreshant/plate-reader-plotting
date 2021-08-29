# 0-read_plate.reader.files_manipulate.columns.R
# reading files and manipulating columns ----

# read in the excel file (from row 14 onwards)
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
{ # finds the occurrence of "match.key" in the match.col.number (generally 1st column) of data list and gives the row of occurrence along with the index
  
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
  
  
  # 9 columns x 13 rows including labels = theoretical maximum 96 well plate 
  full_grid <- .df[start_row_index + 0:8, start_col_index + 0:12]
  
  # find the empty cells around the edges of the grid using the headers for rows and cols
  col_edge <- full_grid[1,] %>% # select first row (<>, 1, 2, 3..)
    {which(is.na(.))} %>%  # find the empty cells (NA) in the grid header
    min(10) # find the minimum NA occurrence or the cell beyond the full grid
  
  row_edge <- full_grid[,1] %>% # select first col (<>, A, B, C..)
    {which(is.na(.))} %>%  # find the empty cells (NA) in the row headers
    min(14) # find the minimum NA occurrence or the cell beyond the full grid
  
  # read the actual grid (< = full grid)
  if(col_edge == 10 & row_edge == 14) {data_grid <- full_grid # if it's a full grid
  }  else data_grid <- .df[start_row_index + 0:(row_edge-2), start_col_index + 0:(col_edge-2)]
  
}



