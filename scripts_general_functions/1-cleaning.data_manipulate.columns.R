# 1-cleaning.data_manipulate.columns.R

# Clean up and summary ----

clean_and_arrange <- function(merged1)
{ # Purpose : Data crunching of plate reader after loading data set
  # 1. removes NA and undesirable samples
  # 2. adds units to inducer concentration value if desired
  # 3. Aranges the values in order of plate columns
  # 4. Adds a column for Replicate #, ignore if not neccesary
  
  merged2 <- merged1 %>% filter(!str_detect(Samples, "NA"))  # remove NA samples (empty wells)
  # merged2$Inducer %<>% str_c(.,' uM') %>% as_factor()
  merged3 <- merged2 %>% arrange(Inducer, Samples) %>% mutate(Samples = fct_inorder(Samples)) %>% group_by(Samples, Inducer) %>%  mutate('Replicate #' = row_number()) # freeze samples in order of plate columns and replicates # remove the common reporter plasmid name after the + sign  
  ungroup(merged3)
}

group_and_summarize_at <- function(merged2, feature_name = 'GFP/RFP')
{ # calculates mean and SD of a given column / feature  ex: GFP/RFP
  merged3 <- merged2 %>% group_by(Samples, Inducer, category, Time, Reporter) %>%  summarize_at(vars(feature_name), funs(mean, sd)) # calculate mean and SD of the GFP/RFP for each Sample and inducer value
  
  if(length(feature_name) > 1) merged3_g <- merged3 %>% gather(key = 'Measurement', value = 'Reading', -Samples, -Inducer, -category, -Time) %>% separate(Measurement, into = c('Measurement','val'),"_") %>% spread(val,Reading) # Cleaning: Seperate mean and variance and group by variable of measurement
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