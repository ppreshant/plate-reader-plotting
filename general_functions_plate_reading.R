# FUnctions to load plate reader data and analysis. The functions can be called from another R file

# read in excel file (.xls or .xlsx) exported from tecan plate reader (Silberg Lab)

# calling libraries ; make sure they are installed (install.packages)
library(readxl); library(magrittr); library(tidyverse); library(ggrepel); library(rlist)  

# reading files and manipulating columns ----

# read in the excel file (from row 14 onwards)
read_plateReader_file <- function(flnm)
{ # reads excel file output by plate reader; outputs a list of non-empty sheets
  fl <- flnm %>%  
    excel_sheets() %>% 
    set_names(.,.) %>% 
    map(read_excel, path = flnm, col_names = F) %>% 
    list.clean(fun = is_empty)
}

map_in_sheet <- function(data_list, key, column)
{ # finds the occurance of "key" in the column (generally 1st column) of data list and gives the row of occurance along with the index
  
  data_list %<>% mutate(index = 1:n())  # add a column for row index
  label_list <- data_list %>% pull(column) %>% str_subset(key) %>% tibble(label = .)   # identify cells with 'Label' in first column
  label_index_boolean <- data_list %>% pull(column) %>% str_detect(key)   # temporary variable to locate where 'Label' matches 
  label_list <- data_list %>% select('index') %>% filter(label_index_boolean) %>% bind_cols(label_list,.)   # get the index of the matching rows, merge label and index into 1 data frame
  label_list
}

find_plate_read_cells <- function(data_starting_index, empty_cells)
{ # finds non empty cells below and to the right of the starting cell with '<>' 

  plate_full_index <- tibble(row = rep(data_starting_index + 0:8,13), col = rep(1:13, each = 9)) # 9 columns x 13 rows including labels = theortical maximum 96 well plate # 9 columns x 13 rows including labels = theortical maximum 96 well plate
  
  # setdiff(plate_full_index - empty_cells)
  # anti_join(plate_full_index,empty_cells)
}

paste_plate_to_column <- function(val_name = '0')
{ # extracts table from clipboard and transforms it into a column (named after the top left cell, unless mentioned)
  # eliminates plate row,column numbering ; Select 1 row above the plate (even if it doesn't contain a label)
  
  data_tibble <- read_tsv(clipboard(), col_names = F) # read table from clipboard
  colnames(data_tibble) <- data_tibble[2,] # set column names as the second row
  if(val_name == '0') val_name <- data_tibble[[1,1]] # returns the first column name (which is the sample type etc.) 
  data_tibble[-(1:2),] %>% gather(key = 'col_num', value = !!val_name, -`<>`) %>% rename(row_num = `<>`) %>% select(!!val_name)
}

read_plate_to_column <- function(data_tibble, val_name)
{ # transforms a plate reader table into a column (named after the top left cell, unless mentioned)
  # eliminates plate row,column numbering ; Select 1 row above the plate (even if it doesn't contain a label)
  
  val_name <- enquo(val_name)
  colnames(data_tibble) <- data_tibble[1,] # set column names as the first row
  data_tibble[-(1),] %>% gather(key = 'col_num', value = !!val_name, -`<>`) %>% rename(row_num = `<>`) %>% select(!!val_name)
}

# formatting plots ----

# plot formatting function : format as classic, colours = Set1
format_classic <- function(plt)
{ # formats plot as classic, with colour palette Set1, centred title, angled x axis labels
  plt <- plt +
    theme_classic() + scale_color_brewer(palette="Set1") + scale_fill_brewer(palette="Set1") + 
    theme(plot.title = element_text(hjust = 0.5)) #,axis.text.x = element_text(angle = 90, hjust = 1, vjust = .3))
}

# plot formatting function : format as logscale
format_logscale <- function(plt)
{ # extra comments
  plt <- plt +
    scale_y_log10(  # logscale for y axis with tick marks
      labels = scales::trans_format("log10", scales::math_format(10^.x) )
    )
}