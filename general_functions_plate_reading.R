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

# Reading all plate related data from a sheet; vectorizable ----
# uses above functions and makes the working RMD file clean

read_all_plates_in_sheet <- function(data_sheet1, n_Rows, n_Cols)
{
  # Context: 23 rows of lines before data is seen, 26 rows between data and fluorescence values, 26 more rows till RFP values (including the row with <>); If partial plate is read there is 1 extra line in all 3 places
  
  if(n_Rows == 8 & n_Cols == 12) {b_gap = 23; a_gap <- 26;
  }  else {b_gap = 24; a_gap <- 27}
  
  table_OD <- data_sheet1[b_gap + 0:n_Rows, 1 + 0:n_Cols] # exract the OD table
  table_Samples <- data_sheet1[b_gap + 0:n_Rows, 3 + n_Cols + 0:n_Cols] # exract the Sample names table
  table_Inducer <- data_sheet1[b_gap + 0:n_Rows, 5+2*n_Cols + 0:n_Cols] # exract the Inducer table
  table_GFP <- data_sheet1[(b_gap + a_gap - 1 +n_Rows) + 0:n_Rows, 1 + 0:n_Cols] # exract the GFP values
  table_RFP <- data_sheet1[(b_gap + 2*a_gap - 2 +2*n_Rows) + 0:n_Rows,  1 + 0:n_Cols] # exract the RFP values
  
  tables_list <- list(table_Samples,table_OD,table_GFP,table_RFP,table_Inducer)
  names_vector <- c('Samples','OD','GFP','RFP','Inducer')
  
  merged1 <- map2_dfc(tables_list, names_vector, read_plate_to_column) # convert plate tables into columns and merge all four data types into 1 table
  merged1 %<>% mutate_at(c('OD','GFP','RFP'),as.numeric) %>% mutate('GFP/RFP' = GFP/RFP) # convert the OD, GFP and RFP into numbers (they are loaded as characters) and calculate GFP/RFP ratio
  merged1
}

clean_summarize_and_arrange <- function(merged1)
{ # Purpose : Data crunching of plate reader after loading data set
  # 1. removes NA and undesirable samples
  # 2. adds units to inducer concentration value
  # 4. Calculates mean and variance of GFP/RFP within replicates (treating different Inducer values differently)
  # 5. Aranges the values in ascending order of mean for convenient plotting
  
  merged2 <- merged1 %>% filter(!str_detect(Samples, "NA|MG"))  # remove NA samples (empty wells)
  # merged2 %<>% mutate(Samples = as_factor(Samples), Inducer = as_factor(Inducer)) # freeze order of samples as in the plate - columnwise - for easy plotting
  merged2$Inducer %<>% str_c(.,' uM') %>% as_factor()
  
  merged3 <- merged2 %>% group_by(Samples, Inducer) %>%  summarize_at('GFP/RFP', funs(mean, sd)) # calculate mean and SD of the GFP/RFP for each Sample and inducer value
  # merged3 <- merged2 %>% gather(Reading, Value, OD, GFP, RFP) # gather all the reading into 1 column - to plot multiple
  merged4 <- merged3 %>% arrange(mean) %>% ungroup() %>% separate(Samples, c('Samples', NA), sep ='\\+') %>% mutate(Samples = fct_inorder(Samples)) # freeze samples in ascending order of uninduced  # remove the common reporter plasmid name after the + sign
  merged4
}

extract_from_given_sheet <- function(sheet_name, n_Rows, n_Cols)
{ # extracts sheet from file, data from sheet and gives clean output - mean and var of GFP/RFP : Vectorizable over multiple sheets
  data_sheet1 <- fl[[sheet_name]] # extract the sheet of interest (sheet2 is by default the first non-empty sheet unless it was renamed)
  
  merged1 <- read_all_plates_in_sheet(data_sheet1, n_Rows, n_Cols)
  table_sheet1 <- clean_summarize_and_arrange(merged1) # gives mean and var of GFP/RFP ratio (arranged in ascending order of mean)
  
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