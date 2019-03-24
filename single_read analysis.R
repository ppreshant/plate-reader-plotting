# Read in the plate reader excel file - auto locate the plate rows and columns for each variable and do some plotting and normalization calculations
# Author: Prashant Kalvapalle;  Started : March 24 2019

# Source the general_functions file before running this
# The last part of the script contains important codes from the console that were used for specific situations : These will be commented

# User inputs: choose file name, title for plots and experiment mode (file name starts in the previous directory of this Rproject) ----

flnm <- '../AHL-GFP test_pSH001_23-3-19.xlsx'

# File input and processing ----

fl <- read_plateReader_file(flnm)