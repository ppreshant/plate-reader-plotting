# 0.5-user_inputs.R

# Moving all the userinputs to run_single_plate.R and run_kinetic_reads.R here so those scripts remain static across expts

# User inputs ----

# User inputs: 1. Enter name of the excel file, 2. Name of the data sheet(S) 
# 3. number of rows and columns in plate reader data 4. Title for plots 
# Note:  (file name starts in the previous directory of this Rproject)
# Note: The script only works for SPARK files where OD, metadata next to it, and optionally any fluorescence below it are read

flnm <- 'S067b2_79 memory ww d-1-d2_24-3-23'

sheet_name <- 'default' # 'default' reads the first sheet (for single plate runs only)


# for single plate runs only (I think, need to check)
baseline_sample_to_subtract <- 'MG1655|DH10B|MFDpir|NEB10b|PBS' # Add baseline cell name(s) here
# Fluorescence from samples matching this name will be subtracted from all values (baseline)

# should I divide the signal by molecular equivalent fluorophores? 
do_MEFL_normalization <- F # TRUE by default, unless you want to turn off


# Calculation ----

date_regex <- '[:digit:]*-[:digit:]*-[:digit:]*' # Date regex

# title_name : appears on the html file name and header of selected plots, change as required
title_name <- stringr::str_remove(flnm, stringr::str_c("_", date_regex)) 
