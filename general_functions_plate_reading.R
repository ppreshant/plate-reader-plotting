# Functions to load plate reader data and analysis. The functions can be called from another R file

# read in excel file (.xls or .xlsx) exported from Tecan Spark plate reader (Silberg Lab)

# Libraries ----
# calling libraries ; make sure they are installed (install.packages)
library(tidyverse); # general 
library(minpack.lm); # for hill fitting
library(plotly) # for plotting
# extra libraries
library(magrittr) # for complex pipes not included in tidyverse (ex: %<>% )


# dummy data  ---- 
# or test data for testing simple functions 

# dummy test tibble
a <- tibble(a1 = 1:6, a2 = 6:1, a3 = rep(c('a', 'b'),3), a4 = a2 ^2, 
            mean_a1 = mean(a1), mean_a4 = mean(a4))

# expression to test on plotting
y_namr_test <- list( 'a2' = expression(paste('this is a ', mu, 'L')),
                     'a4' = expression(paste('super large ', sigma, 'L')))

# test ggplot in function
check_plot <- function(.dataframe = a, variable = a1, prefix = 'mean')
{
  y_expr <- add_prefix.suffix_expr('mean', !!enexpr(variable), NULL)
  
  
  ggplot(.dataframe,
         aes(x = a1, y = {{y_expr}})) +
    geom_point() + geom_line()
  
}


compound_expr <- function(a, b)
{ 
  ytp <- deparse(enexpr(a))
  
}


a_plt <- ggplot(a, aes(a1, a2, colour = a3)) + 
  geom_point() + 
  geom_line() + 
  ylab(y_namr_test[['a4']])


# dummy function to test quosure/meta-programming stuff
metafn <- function(.var)
{
  # make a string to use mean_{{y_var}} in regular functions
  mean_y_var <- rlang::parse_expr(str_c('mean_', rlang::expr_text(enexpr(.var)) ) )
  # .vexpr <- enexpr(.var) 
  
  rlang::parse_expr(.var)
  
  # select(a, {{mean_y_var}})
  
}


# Make a full plate layout. Not useful but just in case..
# full_96_plate <- tibble(row_num = rep(LETTERS[1:8], each = 12), 
#                         col_num = rep(1:12, 8)) %>% 
#   
#   mutate(across(everything(), .fns = ~ .x, .names = "final_{.col}")) # duplicate vectors for final row and col names


# calling more funs ----

list_of_general_functions <- c("0-read_plate.reader.files.R",
                               "1-cleaning.data_manipulate.columns_wrappers.R",
                               "2-read_multiple_grids_in_sheet.R",
                               "3-mathematical.functions.R",
                               "4-plotting_fns.R",
                               "5-formatting_plots.R",
                               '6-parse_continuous_growth.R',
                               '7-read_merge_metadata_grids.R',
                               '8-MEFL_normalization.R',
                               '9-plot_static_fluorescence.R')

# bash command to make this file list
# ls -Q | sed 's/$/,/g' > flnm.txt

# Source all the functions listed above
map(str_c('./scripts_general_functions/', list_of_general_functions),
    source)


# Convenience wrappers ----

# generates a path with the default plot saving directory and .png suffix
plot_as <- function(plt_name, ...)  
{
  str_c('plate reader analysis/plots and data/archive/', 
       plt_name, ...,
       '.png')
}


#' Read a bunch of processed data sheets and join them, including the Sxx as run_ID
#' @param .flnms vector of strings with name of file, without the '-processed.csv' 
get_processed_datasets <- function(.flnms)
{
  .df <- map_dfr(.flnms, 
                 ~ read_csv(str_c('plate reader data/processed/', .x, '-processed.csv')) %>%  # read processed csv file from R
                   mutate(run_ID = str_extract(.x, 'S[:alnum:]*')) # add the run_ID from the filename
  )
  
  
}