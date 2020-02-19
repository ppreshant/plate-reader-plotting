# FUnctions to load plate reader data and analysis. The functions can be called from another R file

# read in excel file (.xls or .xlsx) exported from tecan plate reader (Silberg Lab)

# calling libraries ; make sure they are installed (install.packages)
library(readxl); library(magrittr); library(tidyverse); library(ggrepel); library(rlist); library(plotly); library(minpack.lm)   

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

read_all_plates_in_sheet <- function(data_sheet1, n_Rows, n_Cols, device_name)
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
  table_Samples <- data_sheet1[b_gap + 0:n_Rows, 3 + n_Cols + 0:n_Cols] # exract the Sample names table
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
  
  MG1655_baseline <- merged1 %>% filter(str_detect(Samples, 'MG1655')) %>% summarize_all(funs(mean)) # avg of MG1655 fluor values in plate
  
  merged2 <- merged1 %>% mutate(GFP = pmax(GFP - MG1655_baseline$GFP,0), RFP = pmax(RFP - MG1655_baseline$RFP,0)) %>% mutate('GFP/RFP' = GFP/RFP) %>% mutate('GFP/OD' = GFP/OD) %>% mutate('RFP/OD' = RFP/OD) # Subtract baseline fluor and calculate ratios
  
}

clean_and_arrange <- function(merged1)
{ # Purpose : Data crunching of plate reader after loading data set
  # 1. removes NA and undesirable samples
  # 2. adds units to inducer concentration value
  # 4. Calculates mean and variance of GFP/RFP within replicates (treating different Inducer values differently)
  # 5. Aranges the values in ascending order of mean for convenient plotting
  
  merged2 <- merged1 %>% filter(!str_detect(Samples, "NA"))  # remove NA samples (empty wells)
  merged2$Inducer %>% as.numeric () %>% scales::scientific(digits = 0)
  merged3 <- merged2 %>% arrange(Inducer, Samples) %>% mutate(Samples = fct_inorder(Samples)) %>% group_by(Samples, Inducer) %>%  mutate('Replicate #' = row_number()) # freeze samples in order of plate columns and replicates # group by variables and map out replicates  
  ungroup(merged3)
}

group_and_summarize_at <- function(merged2, feature_name = 'GFP/OD')
{ # calculates mean and SD of a given column / feature  ex: GFP/RFP
  merged3 <- merged2 %>% group_by(Samples, Reporter, Inducer, Time) %>%  summarize_at(vars(feature_name), funs(mean, sd)) # calculate mean and SD of the GFP/RFP for each Sample and inducer value
  # merged3 <- merged2 %>% gather(Reading, Value, OD, GFP, RFP) # gather all the reading into 1 column - to plot multiple
  # merged4 <- merged3 %>% arrange(mean) %>% ungroup() %>% mutate(Samples = fct_inorder(Samples)) # freeze samples in ascending order of uninduced
  # merged4
}

extract_from_given_sheet <- function(sheet_name, n_Rows, n_Cols)
{ # extracts sheet from file, data from sheet and gives clean output - mean and var of GFP/RFP : Vectorizable over multiple sheets
  data_sheet1 <- fl[[sheet_name]] # extract the sheet of interest (sheet2 is by default the first non-empty sheet unless it was renamed)
  
  device_name <- data_sheet1[1:3, 1] %>% str_match('Device: (.*)') %>% pluck(2) # exract the plate reader device name
  merged1 <- read_all_plates_in_sheet(data_sheet1, n_Rows, n_Cols, device_name)
  table_sheet1 <- clean_and_arrange(merged1) # gives mean and var of GFP/RFP ratio (arranged in ascending order of mean)
  
}

hill_fit <- function(results_array)
{ # Fittiing Hill equation (typically useful for dose reponse curves)
  
  # source: https://github.com/dritoshi/Fitting-Hill-equation/blob/master/bin/hill.r
  # Itoshi NIKAIDO <dritoshi@gmail.com>
  
  # make demo data
  L  <- results_array$L
  y  <- results_array$y
  
  # # conf
  # output <- "results/hill.pdf"
  
  # initial
  y0 <- min(y)
  ymax.init <- 1e10
  n.init  <- 1
  Kd.init <- .005
  
  # fitting Hill equation
  y.nls <- nlsLM(y ~ y0 + (ymax - y0) * L^n / (Kd^n + L^n), start = c(ymax = ymax.init, n = n.init, Kd = Kd.init))
  
  # # extract fitting data
  # y.nls.summary <- summary(y.nls)
  # y.nls.n       <- y.nls.summary$param[1]
  # y.nls.Kd      <- y.nls.summary$param[2]
  # y.nls.predict <- predict(y.nls)
  # results <- cbind(y, y.nls.predict)
}

# formatting plots ----

# plotting timeseries (mean in points, stdev in errorbars; Coloured by reporter plasmid, facetted by integrase plasmid and shape as inducer)
plot_time_series <- function(data_table, induction_duration = c(0,1), x_breaks = c(0,6,24,48), stroke_width = 1, x_axis_label = 'Time (days)', y_axis_label = 'GFP/OD (a.u.)', plot_title = 'AHL flipping with time' )
{
  plt <- ggplot(data_table, aes(Time, mean, colour = Inducer, shape = Reporter)) + 
    annotate('rect', xmin = induction_duration[1], ymin = 0, xmax = induction_duration[2], ymax = Inf, alpha = .2) +  # grey rectangle for induction duration
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.05) + facet_wrap(~ Samples) + geom_line() + geom_point(size = 2, fill = 'white', stroke = stroke_width) + 
    scale_x_continuous(breaks = x_breaks) + 
    ylab(y_axis_label) + xlab(x_axis_label) + ggtitle(plot_title)
 
   format_classic(plt) # output a classic formatted plot
}

# plotting dose response (mean vs Inducer)
plot_dose_response <- function(sel_tablex, y_axis_label = 'GFP/OD (a.u.)', plot_title = 'Flipping vs inducer dose' )
{ # Input the filtered summary table and plot the mean vs Inducer points and errorbars. facet by Samples, colour by sample and title
  plt1 <- ggplot(sel_tablex, aes(Inducer, mean)) + 
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) + 
    geom_point(size = 2, fill = 'white') + 
    facet_grid(~ Samples, scales = 'free_x', space = 'free_x') +
    ylab(y_axis_label) + ggtitle(plot_title) +
    scale_x_log10(  # logscale for y axis with tick marks
      labels = scales::trans_format("log10", scales::math_format(10^.x) ) )
  
  format_classic(plt1) # output a classic formatted plot
}

# formatting labels in logscale cleanly : a x 10^b
# use as ggplot(df,aes(x,y)) + geom_point() + scale_y_log10(labels = fancy_scientific)
fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # remove + after exponent, if exists. E.g.: (3x10^+2 -> 3x10^2) 
  l <- gsub("e\\+","e",l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # convert 1x10^ or 1.000x10^ -> 10^ 
  l <- gsub("\\'1[\\.0]*\\'\\%\\*\\%", "", l)
  # return this as an expression
  parse(text=l)
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