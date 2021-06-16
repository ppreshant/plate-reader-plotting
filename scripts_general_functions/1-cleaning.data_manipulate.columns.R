# 1-cleaning.data_manipulate.columns.R

# Clean up and summary ----

clean_and_arrange <- function(merged1)
{ # Purpose : Data crunching of plate reader after loading data set
  # 1. removes NA and undesirable samples
  # 2. adds units to inducer concentration value if desired
  # 3. Aranges the values in order of plate columns
  # 4. Adds a column for Replicate #, ignore if not neccesary
  
  merged2 <- merged1 %>% filter(!str_detect(Samples, "NA"))  # remove NA samples (empty wells)
  # merged2$Inducer %<>% str_c(.,' uM') %>% as_factor() # convert to a string with units -- plots are not representative
  # merged2$Inducer %>% as.numeric () %>% scales::scientific(digits = 0) # convert to scientific format number; digits = 0 removes information?
  merged3 <- merged2 %>% arrange(Inducer, Samples) %>% mutate(Samples = fct_inorder(Samples)) %>% group_by(Samples, Inducer) %>%  mutate('Replicate #' = row_number()) # freeze samples in order of plate columns and replicates; group by variables and figure out replicates
  ungroup(merged3)
}

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

paste_plate_to_column <- function(val_name = '0')
{ # extracts table from clipboard and transforms it into a column (named after the top left cell, unless mentioned)
  # eliminates plate row,column numbering ; Select 1 row above the plate (even if it doesn't contain a label)
  
  data_tibble <- read_tsv(clipboard(), col_names = F) # read table from clipboard
  colnames(data_tibble) <- data_tibble[2,] # set column names as the second row
  if(val_name == '0') val_name <- data_tibble[[1,1]] # returns the first column name (which is the sample type etc.) 
  data_tibble[-(1:2),] %>% pivot_longer(names_to = 'col_num', values_to = val_name, cols = -`<>`) %>% rename(row_num = `<>`) %>% select(all_of(val_name))
}

read_plate_to_column <- function(data_tibble, val_name)
{ # transforms a plate reader table into a column (named after the top left cell, unless mentioned)
  # eliminates plate row,column numbering ; Select 1 row above the plate (even if it doesn't contain a label)
  
  colnames(data_tibble) <- data_tibble[1,] # set column names as the first row
  data_tibble[-(1),] %>% pivot_longer(names_to = 'col_num', values_to = val_name, cols = -`<>`) %>% rename(row_num = `<>`) %>% select(all_of(val_name))
}

# Extra function : convert a column into a plate layout
column_to_plate <- function(.data, column_of_interest)
{
  grid.outpyt <- .data %>% 
    mutate(colid = 0:(n()-1)%/% 8 + 1, 
           rowid = 0:(n()-1) %% 8 + 1) %>% # create column and row IDs; 8 wells per column
    select(rowid, colid, {{column_of_interest}}) %>% # select only relevant column
    pivot_wider(names_from = colid, values_from = {{column_of_interest}}) # creates the grid
}
