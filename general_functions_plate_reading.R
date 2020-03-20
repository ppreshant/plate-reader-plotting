# FUnctions to load plate reader data and analysis. The functions can be called from another R file

# read in excel file (.xls or .xlsx) exported from tecan plate reader (Silberg Lab)

# calling libraries ; make sure they are installed (install.packages)
library(readxl); library(magrittr); library(tidyverse); library(ggrepel); library(rlist); library(plotly)  

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

read_all_plates_in_sheet <- function(data_sheet1, n_Rows, n_Cols, device_name, sheet_name)
{
  # Context: b_gap = row number of OD data header (<>) , a_gap = 2 + # of rows between OD and fluorescence value header(<>), The same between GFP and RFP as well; 
  # If partial plate is read there is 1 extra line in all 3 places for infinite; makes no difference for Spark
  
  if(str_detect(device_name, 'infinite')) # infinite M1000 plate reader
    
  {
    if(n_Rows == 8 & n_Cols == 12) {b_gap = 23; a_gap <- 26; # full plate
    }  else {b_gap = 24; a_gap <- 27} # partial plate (has extra lines for plate region)
    
  } else if(str_detect(device_name, 'Spark')) # Spark plate reader
    
  {b_gap = 35; a_gap <- 27;}  
  
  else stop(paste('Device not recognised. it is ', device_name)) # stop and throw error for unrecognized plate reader device
  
  inducer_flag <- 0
  
  table_OD <- data_sheet1[b_gap + 0:n_Rows, 1 + 0:n_Cols] # exract the OD table
  
  if(ncol(data_sheet1) < 3 + 2* n_Cols) {stop(str_c('Sample names do not exist for sheet: ',sheet_name))}
  table_Samples <- data_sheet1[b_gap + 0:n_Rows, 3 + n_Cols + 0:n_Cols] # exract the Sample names table
  if(!str_detect(table_Samples[[1,1]], '<>') | is.na(table_Samples[[1,1]])) {stop(str_c('Sample names in the wrong place or are improperly formatted, for sheet: ',sheet_name))}
  
    if (ncol(data_sheet1) >= 5+3*n_Cols) # extracting the inducer table 
  {
    table_Inducer <- data_sheet1[b_gap + 0:n_Rows, 5+2*n_Cols + 0:n_Cols] # exract the Inducer table if it exists
    if (is_empty(table_Inducer)) inducer_flag <- 1 # flag that there are no inducer values
  }  else inducer_flag <- 1 # flag that there are no inducer values
  
  if(inducer_flag) {table_Inducer <- table_Samples; table_Inducer[-1,-1] <- 0} # if inducer table is empty or absent, make it zero (same size as samples)
  
  table_GFP <- data_sheet1[(b_gap + a_gap - 1 +n_Rows) + 0:n_Rows, 1 + 0:n_Cols] # exract the GFP values
  table_RFP <- data_sheet1[(b_gap + 2*a_gap - 2 +2*n_Rows) + 0:n_Rows,  1 + 0:n_Cols] # exract the RFP values
  
  tables_list <- list(table_Samples,table_OD,table_GFP,table_RFP,table_Inducer)
  names_vector <- c('Samples','OD','GFP','RFP','Inducer')
  
  merged1 <- map2_dfc(tables_list, names_vector, read_plate_to_column) # convert plate tables into columns and merge all four data types into 1 table
  merged1 %<>% mutate_at(c('OD','GFP','RFP'),as.numeric) %>% mutate('GFP/RFP' = GFP/RFP) %>% mutate('GFP/OD' = GFP/OD) %>% mutate('RFP/OD' = RFP/OD) # convert the OD, GFP and RFP into numbers (they are loaded as characters) and calculate GFP/RFP ratio
  
  # MG1655_baseline <- merged1 %>% filter(str_detect(Samples, 'MG1655')) %>% summarize_all(funs(mean)) # avg of MG1655 fluor values in plate

  merged2 <- merged1 %>% mutate('GFP/OD' = GFP/OD) %>% mutate('RFP/OD' = RFP/OD) # Subtract baseline fluor and calculate ratios
  
}

extract_from_given_sheet <- function(sheet_name, n_Rows, n_Cols)
{ # extracts sheet from file, data from sheet and gives clean output - mean and var of GFP/RFP : Vectorizable over multiple sheets
  data_sheet1 <- fl[[sheet_name]] # extract the sheet of interest (sheet2 is by default the first non-empty sheet unless it was renamed)
  
  device_name <- data_sheet1[1:3, 1] %>% str_match('Device: (.*)') %>% pluck(2) # exract the plate reader device name
  merged1 <- read_all_plates_in_sheet(data_sheet1, n_Rows, n_Cols, device_name, sheet_name)
  table_sheet1 <- clean_and_arrange(merged1) # gives mean and var of GFP/RFP ratio (arranged in ascending order of mean)
  
}

clean_and_arrange <- function(merged1)
{ # Purpose : Data crunching of plate reader after loading data set
  # 1. removes NA and undesirable samples
  # 2. adds units to inducer concentration value
  # 4. Calculates mean and variance of GFP/RFP within replicates (treating different Inducer values differently)
  # 5. Aranges the values in ascending order of mean for convenient plotting
  
  merged2 <- merged1 %>% filter(!str_detect(Samples, "NA"))  # remove NA samples (empty wells)
  merged2$Inducer %<>% str_c(.,' uM') %>% as_factor()
  merged3 <- merged2 %>% arrange(Inducer, Samples) %>% mutate(Samples = fct_inorder(Samples)) %>% group_by(Samples, Inducer) %>%  mutate('Replicate #' = row_number()) # freeze samples in order of plate columns and replicates # group by variables and map out replicates  
  ungroup(merged3)
}


# Post processing data ----

group_and_summarize_at <- function(merged2, feature_name = 'GFP/OD')
{ # calculates mean and SD of a given column / feature  ex: GFP/RFP
  merged3 <- merged2 %>% group_by(Samples, Reporter, Inducer, Time, Media) %>%  summarize_at(vars(feature_name), funs(mean, sd)) # calculate mean and SD of the GFP/RFP for each Sample and inducer value
  # merged3 <- merged2 %>% gather(Reading, Value, OD, GFP, RFP) # gather all the reading into 1 column - to plot multiple
  # merged4 <- merged3 %>% arrange(mean) %>% ungroup() %>% mutate(Samples = fct_inorder(Samples)) # freeze samples in ascending order of uninduced
  # merged4
}

mutate_cond <- function(.data, condition, ..., envir = parent.frame()) 
{ # mutates a subset of data and returns a new array (without modifying the parent) - from https://stackoverflow.com/a/34096575/9049673
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

baseline_subtraction <- function(data1, baseline_name) # work in progress
{
  # Finds the baseline (negative control for fluorescence values) and does subtraction
  # If baseline culture is not present in every day/sheet, then the program extrapolates from the average of the data present

  # value_baseline <- data1 %>% filter(str_detect(Samples, baseline_name)) %>% summarize_all(funs(mean)) # avg of MG1655 fluor values in plate
  # 
  # merged2 <- data1 %>% mutate(GFP = pmax(GFP - value_baseline$GFP,0), RFP = pmax(RFP - value_baseline$RFP,0)) %>% mutate('GFP/RFP' = GFP/RFP) %>% mutate('GFP/OD' = GFP/OD) %>% mutate('RFP/OD' = RFP/OD) # Subtract baseline fluor and calculate ratios
  
  value_baseline <- data1 %>% group_by(Samples, Reporter, Inducer, Time) %>% filter(str_detect(Reporter, baseline_name)) # avg of MG1655 fluor values in plate
  
  baseline_subtracted_data <- data1 %>% mutate(GFP = pmax(GFP - value_baseline$GFP,0), RFP = pmax(RFP - value_baseline$RFP,0)) %>% mutate('GFP/RFP' = GFP/RFP) %>% mutate('GFP/OD' = GFP/OD) %>% mutate('RFP/OD' = RFP/OD) # Subtract baseline fluor and calculate ratios
  
  
}

# formatting plots ----

# plotting timeseries (mean in points, stdev in errorbars; Coloured by reporter plasmid, facetted by integrase plasmid and shape as inducer)
plot_time_series <- function(data_table, induction_duration = c(0,6/24), x_breaks = c(0,6,24,48), stroke_width = 1, x_axis_label = 'Time (days)', y_axis_label = 'GFP/OD (a.u.)', plot_title = 'AHL flipping with time', colour_by_var = Reporter, facet_by_var = Samples )
{
  # colour_by_var <- enquo(colour_by_var) # not being used
  facet_by_var = enquo(facet_by_var)
  plt <- ggplot(data_table, aes(Time, mean, colour = Reporter, shape = Inducer)) 
  
  plt_layers <- add_layers_time_series(plt, x_breaks, facet_by_var = !! facet_by_var)
 
   format_classic(plt_layers) # output a classic formatted plot
}

# Adding layers to timeseries (mean in points, stdev in errorbars; Coloured by reporter plasmid, facetted by integrase plasmid and shape as inducer)
add_layers_time_series <- function(plt_object, induction_duration = c(0,6/24), x_breaks = c(0,6,24,48), stroke_width = 1, errorbar_width = .25, x_axis_label = 'Time (days)', y_axis_label = 'GFP/OD (a.u.)', plot_title = 'AHL flipping with time', colour_by_var = Reporter, to_colour = 1, facet_by_var = Samples)
{
  facet_by_var <- enquos(facet_by_var)
  plt <- plt_object + 
    annotate('rect', xmin = induction_duration[1], ymin = 0, xmax = induction_duration[2], ymax = Inf, alpha = .2) +  # grey rectangle for induction duration
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = errorbar_width) + facet_wrap(vars(!!! facet_by_var)) + geom_line() + geom_point(size = 1, fill = 'white', stroke = stroke_width) + 
    scale_shape_manual(values = c(21,19)) +  scale_x_continuous(breaks = x_breaks) + 
    ylab(y_axis_label) + xlab(x_axis_label) + ggtitle(plot_title)
  
  format_classic(plt) # output a classic formatted plot
}

# Set theme for plots : format as classic, colours = Set1
format_classic <- function(plt)
{ # formats plot as classic, with colour palette Set1, centred title, angled x axis labels
  plt <- plt +
    theme_classic() + scale_color_brewer(palette="Dark2") + scale_fill_brewer(palette="Set1") #+ 
    #theme(plot.title = element_text(hjust = 0.5)) #,axis.text.x = element_text(angle = 90, hjust = 1, vjust = .3))
}

# plot formatting function : format as logscale
format_logscale <- function(plt)
{ # extra comments
  plt <- plt +
    scale_y_log10(  # logscale for y axis with tick marks
      labels = scales::trans_format("log10", scales::math_format(10^.x) )
    )
}