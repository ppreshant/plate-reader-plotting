# Functions to load plate reader data and analysis. The functions can be called from another R file

# read in excel file (.xls or .xlsx) exported from Tecan Spark plate reader (Silberg Lab)

# Libraries ----
# calling libraries ; make sure they are installed (install.packages)
library(tidyverse); # general 
library(minpack.lm); # for hill fitting
library(plotly) # for plotting



# dummy data  ---- 
# or test data for testing simple functions 

# dummy test tibble
a <- tibble(a1 = 1:6, a2 = 6:1, a3 = rep(c('a', 'b'),3), a4 = a2 ^2)

# expression to test on plotting
y_namr_test <- list( 'a2' = expression(paste('this is a ', mu, 'L')),
                     'a4' = expression(paste('super large ', sigma, 'L')))

# test ggplot
a_plt <- ggplot(a, aes(a1, a2, colour = a3)) + 
  geom_point() + 
  geom_line() + 
  ylab(y_namr_test[['a4']])



# calling more funs ----

list_of_general_functions <- c("0-read_plate.reader.files.R",
                               "1-cleaning.data_manipulate.columns.R",
                               "2-read_multiple_grids_in_sheet.R",
                               "3-mathematical.functions.R",
                               "4-plotting_fns.R",
                               "5-formatting_plots.R")

# bash command to make this file list
# ls -Q | sed 's/$/,/g' > flnm.txt

# Source all the functions listed above
map(str_c('./scripts_general_functions/', list_of_general_functions),
    source)



