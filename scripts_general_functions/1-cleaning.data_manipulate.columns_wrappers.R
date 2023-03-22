# 1-cleaning.data_manipulate.columns.R

# Clean up and summary ----

group_and_summarize_at <- function(merged2, feature_name = 'GFP/OD',
                                   
                                   # variables to group by, excluding Reporter
                                   grouping_vars = c('Samples', 'Inducer', 'category', 'Time') )
                                   

{ # calculates mean and SD of a given column / feature  ex: GFP/OD; 
  # makes output into wide format for multiple features; with mean and stdev in separate columns
  
  merged3 <- merged2 %>% 
    group_by(across(any_of(c(grouping_vars, 'Reporter')))) %>%  # any_of accounts for missing variables among grouping_vars
    summarize(across(any_of(feature_name), lst(mean, sd))) # calculate mean and SD of the feature_name for each group : Sample, inducer value etc.
  
  # when summarizing multiple columns, the output is recast into long format
  if(length(feature_name) > 1) merged3_g <- merged3 %>% # if multiple columns need to be summarized
      pivot_longer(cols = -any_of(grouping_vars), names_to = 'Measurement', values_to = 'Reading')  %>% 
      separate(Measurement, into = c('Measurement','type'),"_") %>% 
      pivot_wider(names_from = type, values_from = Reading) # Cleaning: Separate mean and variance and group by variable of measurement
  
  else merged3 %>% mutate(Measurement = feature_name) # if there is only 1 feature, it's name will be saved in measurement
}



# Plate reader grid to single column ----

# transforms a plate reader table into a column (named after the top left cell, unless mentioned)
# eliminates plate row,column numbering ; Select 1 row above the plate (even if it doesn't contain a label)

read_plate_to_column <- function(data_tibble, val_name, retain_well_id = FALSE)
{ 
  # Check if the tibble is empty
  if(drop_na(data_tibble) %>% plyr::empty()) return(NULL)  # return NULL if tibble is empty
  
  # check for '<>' in the top left (or if NA)
  if(data_tibble[[1,1]] %>% 
     {is.na(.) | !str_detect(.,'<>')})
  {stop(str_c('Could not locate "<>" in : ', val_name, 
                '\n Could be improperly formatted; check if n_Rows and n_Cols is accurate.'))}
  
  colnames(data_tibble) <- data_tibble[1,] # set column names as the first row
  
  # Table extrapolation -- if 'all' or regular expressions are used to concise entry of metadata
  if( c(as_vector(data_tibble[1, ]), data_tibble[ , 1]) %>% unlist %>% # check if first row or column (containing indices)
      str_detect('all|\\[|\\]') %>% any ) # contains the 'all' or other regular expressions '[ ]'
    {extrapolate_table = TRUE} # then enable the table extrapolation function
  else extrapolate_table = FALSE
  
  
  # pivot the table into wider (columns
  data_tibble[-(1),] %>% 
    pivot_longer(names_to = 'col_num', values_to = val_name, cols = -`<>`) %>% 
    rename(row_num = `<>`) %>% # letters go into row_num
    
    {if(extrapolate_table) extrapolate_96_well_plate(.) else .} %>% # table extrapolation
    
    unite(col = 'well', c(row_num, col_num), sep = '') %>% # merge letters with numbers
    
    {if(!retain_well_id) select(., all_of(val_name)) else .}
}


# obsolete function

# extracts table from clipboard and transforms it into a column (named after the top left cell, unless mentioned)
# eliminates plate row,column numbering ; Select 1 row above the plate (even if it doesn't contain a label)

paste_plate_to_column <- function(val_name = '0')
{   
  data_tibble <- read_tsv(clipboard(), col_names = F) # read table from clipboard
  colnames(data_tibble) <- data_tibble[2,] # set column names as the second row
  if(val_name == '0') val_name <- data_tibble[[1,1]] # returns the first column name (which is the sample type etc.) 
  data_tibble[-(1:2),] %>% pivot_longer(names_to = 'col_num', values_to = val_name, cols = -`<>`) %>% rename(row_num = `<>`) %>% select(all_of(val_name))
}


# Table extrapolation ----

#' Regex plate filler/extrapolation function ; called from "read_plate_to_column()"
#' extrapolate rows or columns in metadata tables using regular expressions
#' enables concise entry of inducer concentrations etc for full rows or columns

extrapolate_96_well_plate <- function(.data_tibble)
{
  
  # regular expression cleanup ----
  # Converts user input to exact regular expressions
  translate_regex <- c('all' = '.*', # regex for all
                       '\\[' = '^[', # regex for ranges - constrain begin
                       '\\]' = ']$') # and end of string
  
  
  # pick a small subset for testing
  processed_tibble <- .data_tibble %>% 
    
    # replace "all" with the regular expression '.*' ; clean up the [..] regex to ^[..]$
    mutate(across(col_num, ~ str_replace_all(.x, translate_regex)))  
  
  
  # Make tibbles of full row and columns, for extrapolation
  # one dimension at a time
  
  full_cols <- tibble(col_num = 1:12)
  full_rows <- tibble(row_num = LETTERS[1:8])
  
  
  # Extrapolation ----
  
  fuzzyjoin::regex_right_join(full_cols, processed_tibble, by = c(col_num = 'col_num')) %>% # extrapolate columns
    {fuzzyjoin::regex_right_join(full_rows, ., by = c(row_num = 'row_num'))} %>% # extrapolate rows
    
    # clean up
    select(!ends_with('.y')) %>%  # remove matched columns '//.x
    rename_with(.cols = ends_with('.x'), 
                .fn = ~ str_replace(.x, '\\.x', '')) # clean up ".x" from the column names
  
  # select(row_num, col_num, everything()) %>% # arrange column order
  
}


# Naming interchanges ----

measurement.labels_translation <- c('OD600|^od$' = 'OD', # labels in file = new labels for rest of code
                       'sfgfp|mgl|.*greenlantern|.*gfp.*' = 'GFP',
                       'mcherry.*|mscarlet.*|.*rfp.*' = 'RFP',
                       '^Sample.*' = 'Samples',
                       'inducer' = 'Inducer',
                       'time' = 'Time')


# Special function to implement a switch() using regex matches
# Source : https://stackoverflow.com/a/66519022/9049673
# Usage : 
# map_chr(
# c("aaaa","bb","aabbaa"),
# regex_switch,
# "^aa"="Apple","^bbbb"="Grape","^*$"="Unknown"

regex_switch<-function(.v,...)
{
  e <- enexprs(...)
  i <- min(which(str_detect(.v,names(e))))
  eval(e[[i]],parent.frame())
}

# Tidyeval helpers ----

# Add a prefix and suffix to an unevaluated expression -- used for plotting mean of stuff with mean_xx
add_prefix.suffix_expr <- function(prefix, .expr, suffix, separator = '_')
{
  deparse(enexpr(.expr)) %>% 
    {rlang::parse_expr(str_c(prefix, ., suffix, sep = separator))}
}



# Other column manipulation ----

# Extra function : convert a column into a plate layout
column_to_plate <- function(.data, column_of_interest)
{
  grid.outpyt <- .data %>% 
    mutate(colid = 0:(n()-1)%/% 8 + 1, 
           rowid = 0:(n()-1) %% 8 + 1) %>% # create column and row IDs; 8 wells per column
    select(rowid, colid, {{column_of_interest}}) %>% # select only relevant column
    pivot_wider(names_from = colid, values_from = {{column_of_interest}}) # creates the grid
}


# Wrappers ----


#' wrapper for reading processed csv data
#' @param flnm : string, name of file to read / omit the '-processed' suffix

read_processed_data <- function(.flnm)
{
  read_csv(str_c('plate reader data/processed/', .flnm, '-processed.csv'))
}
