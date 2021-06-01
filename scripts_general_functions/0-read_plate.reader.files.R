# 0-read_plate.reader.files_manipulate.columns.R
# reading files and manipulating columns ----

# read in the excel file (from row 14 onwards)
read_plateReader_file <- function(flnm)
{ # reads excel file output by plate reader; outputs a list of non-empty sheets
  fl <- flnm %>%  
    excel_sheets() %>% # get the names of all the sub-sheets
    set_names(.,.) %>% 
    map(read_excel, path = flnm, col_names = F) %>% # read each sub-sheet
    list.clean(fun = is_empty) # removes data from empty sheets
}



# Browse the plate reader files ----

map_in_sheet <- function(data_list, key, column)
{ # finds the occurrence of "key" in the column (generally 1st column) of data list and gives the row of occurrence along with the index
  
  data_list %<>% mutate(index = 1:n())  # add a column for row index
  # label_list_match <- data_list %>% 
  #   pull(column) %>% 
  #   str_subset(key) %>% 
  #   tibble(label = .)   # identify cells with 'Label' in n'th column, where n is user input : typically 1st column

  label_index_boolean <- data_list %>%
    pull(column) %>% 
    str_detect(key)   # temporary variable to locate where 'Label' matches
  
  label_and_index.match <- data_list %>% 
    filter(label_index_boolean) %>% 
    select(identifier = all_of(column), neighbour = all_of(column + 1), index)
  
  # label_list <- data_list %>% select('index') %>% filter(label_index_boolean) %>% bind_cols(label_list_match,.)   # get the index of the matching rows, merge label and index into 1 data frame
  # label_list
}

find_plate_read_cells <- function(data_starting_index, empty_cells)
{ # finds non empty cells below and to the right of the starting cell with '<>' 
  
  plate_full_index <- tibble(row = rep(data_starting_index + 0:8,13), col = rep(1:13, each = 9)) # 9 columns x 13 rows including labels = theortical maximum 96 well plate # 9 columns x 13 rows including labels = theortical maximum 96 well plate
  
  # setdiff(plate_full_index - empty_cells)
  # anti_join(plate_full_index,empty_cells)
}



